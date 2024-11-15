"""
***************************************************************
    Copyright 2023 Hewlett Packard Enterprise Development LP.
***************************************************************
"""

import re
import argparse
import subprocess
from fortran_hls.llvm_ir_processor import LLVMIRProcessor
from fortran_hls.fortran_processor import FortranProcessor
from functools import reduce
import os
import pathlib
import shutil

WRONG_NUMBER_OPTIMISATIONS=1


translate_builtin_types = {'integer':'int', 'real':'float'}
translate_builtin_tollvm = {'integer':'i32', 'real':'i32'}

push_declarations = []
pop_declarations = []
full_declarations = []
empty_declarations = []
depth_declarations = []

initiation_intervals = []


class FXX():
    @staticmethod
    def generate_oracle(filename, code):
        f = open("oracles/" + filename.replace(".f90", ".ll"), "w")
        f.write(code)
        f.close()

    # IR
    def extract_subroutines_names(self, fortran_code):
        subroutine_names = set()
        for regex in ["subroutine\s+(\w+)\s*\(", "function\s+(\w+)\s*\("]:
            for match_obj in re.finditer(regex, fortran_code, flags=re.I):
                subroutine_names.add(match_obj.group(1))

        f = open("kernel_names.txt", "w")

        f.write("\n".join(subroutine_names))

        f.close()

        return subroutine_names



    # IR
    def replace_llvm_calls(self, ir_code, xilinx_generated=False):
        stream_ssa_dict = {}
        code = ir_code

        attribute_idx = 2
        match_obj = re.search("call void @_Z7\w*set_depth_\w+\([\[\]\w\s]+\* %(\w+), \w+\*? %?(\w+)\)", code)

        # Trace back where the stream is allocated and add xilinx_attr
        if match_obj:
            stream_var = match_obj.group(1)

            match_obj = re.search(f"%{stream_var} = getelementptr %(\w+), %\w+\* %(\w+), i32 0, i32 0", code)
            stream_type = match_obj.group(1)
            allocated_stream_var = match_obj.group(2)
            if re.search("%{allocated_stream_var} = alloca", code):
                code = re.sub(f"%{allocated_stream_var} = alloca %(\w+), i64 1, align 8", f"%{allocated_stream_var} = alloca %\g<1>, i64 1, align 8, !xilinx.attributes !{attribute_idx}", code)
                attribute_idx += 4
            #else:
            #    #%3 = getelementptr [2 x %_QMhls_streamThlsstream_integer], [2 x %_QMhls_streamThlsstream_integer]* %2, i32 0, i64 0
            #    #match_obj = re.search(f"%{allocated_stream_var} = getelementptr (%)?([^,]+), %\w+\* (%)?([^,]+), i32 0, i64 0", code)
            #    match_obj = re.search(f"%{allocated_stream_var} = getelementptr (%)?([^,]+), (%)?[^,]+\* %(\w+), i32 0, i64 0", code)
            #    allocated_stream_var = match_obj.group(4)
                                                    #%2 = alloca [2 x %_QMhls_streamThlsstream_integer], i64 1, align 8
                code = re.sub(f"%{allocated_stream_var} = alloca (%)?([^\,]+), i64 1, align 8", f"%{allocated_stream_var} = alloca \g<1>\g<2>, i64 1, align 8, !xilinx.attributes !{attribute_idx}", code)
                attribute_idx += 4

            stream_ssa_dict[stream_var] = [stream_type]
        code = re.sub("call void @_Z7\w*set_depth_\w+\((\w+)\* %(\w+), (\w+)(\*)? (%)?(\w+)\)", "call void (...) @llvm.fpga.set.stream.depth(\g<1>* %\g<2>, \g<3>\g<4> \g<5>\g<6>)", code)

        def get_push_sub(match_obj):
            push_type = match_obj.group(2)
            g = match_obj.group

            def k(string):
                return string if string is not None else ""

            if push_type == "i32":
                txt = f"call void @llvm.fpga.fifo.push.i32.p0i32({g(2)} {k(g(3))}{g(4)}{g(5)}, {k(g(6))}* %{g(7)}) #6"
            elif push_type == "double":
                txt = f"call void @llvm.fpga.fifo.push.f64.p0f64({g(2)} {k(g(3))}{g(4)}{g(5)}, {k(g(6))}* %{g(7)}) #6"
            else:
                txt = f"call void @llvm.fpga.fifo.push.derived.p0derived({g(2)} {g(3)}, {g(4)} {g(5)}) #6"

            return txt


        code = re.sub("call void @_Z7\w*hls_lowered_write_(\w+)\((\w+)(\*)? (%)?(\w+), (\w+)\* %(\w+)\)", get_push_sub, code)
        code = re.sub("call void @_Z7\w*hls_lowered_write_(\w+)\(([^\s]+) ([^,]+), ([^\s]+) ([^\)]+)\)", get_push_sub, code)


        def get_pop_sub(match_obj):
            pop_type = match_obj.group(2)
            g = match_obj.group

            def k(string):
                return string if string is not None else ""


            if pop_type == "i32":
                txt = f"%{g(1)} = call {g(2)} @llvm.fpga.fifo.pop.i32.p0i32({g(4)}* %{g(5)})"
            elif pop_type == "double":
                txt = f"%{g(1)} = call {g(2)} @llvm.fpga.fifo.pop.f64.p0f64({g(4)}* %{g(5)})"
            else:
                txt = f"{g(3)} = call {g(2)} @llvm.fpga.fifo.pop.f64.p0f64({g(4)} {g(5)})"

            return txt
        code = re.sub("%(\w+) = call (\w+) @_Z7_QMhls_streamPhls_lowered_read_(\w+)\((\w+)\* %(\w+)\)", get_pop_sub, code)
        code = re.sub("call void @_Z7_QMhls_streamPhls_lowered_read_(\w+)\(([^\s]+) ([^,]+), ([^\s]+) ([^\)]+)\)", get_pop_sub, code)

        # The result of the empty function has to be extended to 32 bits. We output a 8 bits result in the Fortran code to generate the sext instruction with the F18 
        # compilation
        # Empty method working for integer and double for now
        for match_obj in re.finditer("%(\w+) = call i8 @_Z7_QMhls_streamPhls_lowered_empty_\w+\(\w+\* %(\w+)\)", code):
            if match_obj:
                empty_result = match_obj.group(1)
                #code = re.sub(f"%(\w+) = sext i8 %{empty_result} to i32", f"%\g<1> = sext i1 %{empty_result} to i32", code)
                code = re.sub(f"%(\w+) = sext i8 %{empty_result} to i32", f"%lnot.\g<1> = xor i1 %{empty_result}, true\n  " + \
                                                                      f"%\g<1> = zext i1 %lnot.\g<1> to i32", code)

        def get_empty_sub(match_obj):
            empty_type = match_obj.group(2)

            g = match_obj.group

            def k(string):
                return string if string is not None else ""
        
            if empty_type == "i32":
                txt = f'%{g(1)} = call i1 @llvm.fpga.fifo.not.empty.p0i32({g(2)}* %{g(3)}) #6'
            elif empty_type == "double":
                txt = f'%{g(1)} = call i1 @llvm.fpga.fifo.not.empty.p0f64({g(2)}* %{g(3)}) #6'

            return txt
        

        code = re.sub("%(\w+) = call i8 @_Z7_QMhls_streamPhls_lowered_empty_\w+\((\w+)\* %(\w+)\)", get_empty_sub, code)

        # The result of the full function has to be extended to 32 bits. We output a 8 bits result in the Fortran code to generate the sext instruction with the F18 
        # compilation
        match_obj = re.search("%(\w+) = call i8 @_Z7_QMhls_streamPhls_lowered_full_integer\(i32\* %(\w+)\)", code)
        if match_obj:
            empty_result = match_obj.group(1)
            code = re.sub(f"%(\w+) = sext i8 %{empty_result} to i32", f"%lnot.\g<1> = xor i1 %{empty_result}, true\n  " + \
                                                                      f"%\g<1> = zext i1 %lnot.\g<1> to i32", code)
        def get_full_sub(match_obj):
            full_type = match_obj.group(2)

            g = match_obj.group

            def k(string):
                return string if string is not None else ""
        
            if full_type == "i32":
                txt = f'%{g(1)} = call i1 @llvm.fpga.fifo.not.full.p0i32({g(2)}* %{g(3)}) #2'
            elif full_type == "double":
                txt = f'%{g(1)} = call i1 @llvm.fpga.fifo.not.full.p0f64({g(2)}* %{g(3)}) #2'

            return txt

        code = re.sub("%(\w+) = call i8 @_Z7_QMhls_streamPhls_lowered_full_\w+\((\w+)\* %(\w+)\)", get_full_sub,code) #2", code)

        # Add declarations for stream functions
                                      #void @llvm.fpga.fifo.push.i32.p0i32(i32 *%1, i32* %6)
        #for match_obj in re.finditer("void @llvm.fpga.fifo.push.([^\(]+)\((\w+)(\*)?, (\w+)(\*)?[^\)]+\)", code):
        for match_obj in re.finditer("void @llvm.fpga.fifo.push.([^\(]+)\((\w+)\s*(\*)?[^,]+, (\w+)(\*)?", code):
            pop_type = match_obj.group(1)
            argtype1 =  match_obj.group(2)
            point1 = match_obj.group(3)
            if not point1:
                point1 = ""
            argtype2 = match_obj.group(4)
            point2 = match_obj.group(5)
            if not point2:
                point2 = ""

            push_declarations.append(f'declare void @llvm.fpga.fifo.push.{pop_type}({argtype1}{point1}, {argtype2}{point2} nocapture) #2')


        if not xilinx_generated:
            depth_declarations.append(f'call void (...) @llvm.fpga.set.stream.depth(...) #1')
            pop_declarations.append(f'declare i32 @llvm.fpga.fifo.pop.i32.p0i32(i32* %V) #2')
            pop_declarations.append(f'declare double @llvm.fpga.fifo.pop.f64.p0f64(double* %V) #2')
            #push_declarations.add(f'declare void @llvm.fpga.fifo.push.i32.p0i32(i32, i32* nocapture) #2')
            empty_declarations.append(f'declare i1 @llvm.fpga.fifo.not.empty.p0i32(i32* nocapture)  #1\n  ')
            empty_declarations.append(f'declare i1 @llvm.fpga.fifo.not.empty.p0f64(double* nocapture)  #1\n  ')
            full_declarations.append(f'declare i1 @llvm.fpga.fifo.not.full.p0i32(i32* nocapture) #1\n  ')

        return code, stream_ssa_dict


    # IR
    def add_attributes(self, code, code_lines):
        n_attributes = code.count("attributes #")

        f_interfaces = open("interfaces.txt")
        iface_ir_lines = f_interfaces.readlines()
        f_interfaces.close()

        iface_nline = 0
        attr_code = code
        for interface_match in re.finditer("call void @_Z7_interface_.+", code):
            iface_line = iface_ir_lines[iface_nline]
            iface_line = re.sub("#\d+", f"#{n_attributes}", iface_line)
            attr_code = attr_code.replace(interface_match.group(0), iface_line)
            iface_nline += 1

        attr_code += f'\nattributes #{n_attributes} = {{ inaccessiblememonly nounwind "xlx.port.bitwidth"="320000" }}\n'
        attr_code += f'\nattributes #{n_attributes+1} = {{ inaccessiblememonly nounwind }}\n'

        attr_code += f'\nattributes #{n_attributes+2} = {{ noduplicate nounwind }}\n'
        attr_code += f'\nattributes #{n_attributes+3} = {{ nounwind }}\n' # For HLS Streams
        attr_code += f'\nattributes #{n_attributes+4} = {{ argmemonly nounwind }}\n' # For HLS Streams

        return attr_code, n_attributes

    # IR
    def add_declarations_ir(self, code, n_attributes, xilinx_generated=False):
        # We need the lists to always follow the same order for the IR to be testable
        def get_unique_lst(lst):
            lookup = set()
            unique_lst = [x for x in lst if x not in lookup and lookup.add(x) is None]

            return unique_lst

        pop_declarations_str = reduce(lambda x, y: x + '\n' + y, [''] + get_unique_lst(pop_declarations))
        push_declarations_str = reduce(lambda x, y: x + '\n' + y, [''] + get_unique_lst(push_declarations))
        full_declarations_str = reduce(lambda x, y: x + '\n' + y, [''] + get_unique_lst(full_declarations))
        empty_declarations_str = reduce(lambda x, y: x + '\n' + y, [''] + get_unique_lst(empty_declarations))
        if not xilinx_generated:
            if re.search("declare", code):
                code = re.sub("declare", f'declare void @llvm.sideeffect() #{n_attributes+1}\ndeclare token @llvm.directive.scope.entry() #{n_attributes+2}\ndeclare void @llvm.directive.scope.exit(token) #{n_attributes+2}\ndeclare void @llvm.fpga.set.stream.depth(...) #{n_attributes+3}\n{pop_declarations_str}\n{push_declarations_str}\n{full_declarations_str}\n{empty_declarations_str}\ndeclare', code, count=1)
            else:
                code = re.sub("attributes", f'declare void @llvm.sideeffect() #{n_attributes+1}\ndeclare token @llvm.directive.scope.entry() #{n_attributes+2}\ndeclare void @llvm.directive.scope.exit(token) #{n_attributes+2}\ndeclare void @llvm.fpga.set.stream.depth(...) #{n_attributes+3}\n{pop_declarations_str}\n{push_declarations_str}\n{full_declarations_str}\n{empty_declarations_str}\nattributes', code, count=1)
        else:
            if re.search("declare", code):
                #code = re.sub("declare", f'declare token @llvm.directive.scope.entry() #{n_attributes+2}\ndeclare void @llvm.directive.scope.exit(token) #{n_attributes+2}\n{pop_declarations_str}\n{push_declarations_str}\n{full_declarations_str}\n{empty_declarations_str}\ndeclare', code, count=1)
                code = re.sub("declare", f'declare token @llvm.directive.scope.entry() #{n_attributes+2}\ndeclare void @llvm.directive.scope.exit(token) #{n_attributes+2}\n{pop_declarations_str}\n{full_declarations_str}\n{empty_declarations_str}\ndeclare', code, count=1)

        return code

    # IR
    def add_dataflow_primitives_ir(self, code, is_dataflow):
        ssa_counter = 0 # TODO: replace by the lowest SSA counter before the dataflow region
        n_df_functions = code.count("call i32 @_start_df_call()")
        for _ in range(n_df_functions):
            match_obj = re.search("%(\w+) = call i32 @_start_df_call\(\)", code) 
            ssa_var = match_obj.group(1)
            code = re.sub("call i32 @_start_df_call\(\)", f'call token @llvm.directive.scope.entry() [ "xcl_outline"() ]', code, count=1)
            code = re.sub("call void @_end_df_call\(\)", f'call void @llvm.directive.scope.exit(token %{ssa_var})', code, count=1)
            ssa_counter += 1

        code = re.sub("call void @_Z7_dataflow_\(\)", "", code)
        if is_dataflow:
            code = re.sub("attributes #0 = {(.+)}", 'attributes #0 = { \g<1> "fpga.dataflow.func"="0" }', code)

        return code 

    # IR
    def add_array_partition_primitives_ir(self, code):
        def get_partition_repl(match_obj):
            partition_type = match_obj.group(1)
            factor = match_obj.group(2)
            dim = match_obj.group(3)
            array = match_obj.group(4)

            if partition_type == "cyclic":
                partition_field = 0
            if partition_type == "block":
                partition_field = 1
            if partition_type == "complete":
                partition_field = 2

            return f'call void @llvm.sideeffect() #2 [ "xlx_array_partition"({array}, i32 {partition_field}, i32 {factor}, i32 1, i1 false) ]'


        code = re.sub("call void @_Z7partition_(\w+)_(\d+)_(\d+)_\(([^\)]+)\)", get_partition_repl, code) #2 [ "xlx_array_partition"(\g<4>, i32 2, i32 0, i32 1, i1 false) ]'code)
        return code

    def add_stream_sizes(self, code):
        new_code = re.sub("call void @_Z7stream_size_qualifier_stencil\((.*)\)", "call void @llvm.sideeffect() [ \"xlx_reqd_pipe_depth\"(\g<1>) ]", code)
        new_code = re.sub("call void @_Z7stream_size_qualifier_double\((.*)\)", "call void @llvm.sideeffect() [ \"xlx_reqd_pipe_depth\"(\g<1>) ]", new_code)
        return new_code

    def add_interface_qualifiers(self, code):
        new_code = re.sub("call void @_maxi_(\w+)\(([^%]+) %(\w+)\)", "call void @llvm.sideeffect() [ \"xlx_m_axi\"(\g<2> %\g<3>, [5 x i8] c\"\g<1>\", i64 2000, [5 x i8] c\"slave\", [0 x i8] zeroinitializer, i64 -1, i64 -1, i64 -1, i64 -1, i64 -1, i64 -1) ]", code)
        new_code = re.sub("call void \(\.\.\.\) @_maxi_(\w+)\(([^%]+) %(\w+)\)", "call void @llvm.sideeffect() [ \"xlx_m_axi\"(\g<2> %\g<3>, [5 x i8] c\"\g<1>\", i64 2000, [5 x i8] c\"slave\", [0 x i8] zeroinitializer, i64 -1, i64 -1, i64 -1, i64 -1, i64 -1, i64 -1) ]", new_code)

        return new_code


    # IR
    @staticmethod
    def fix_stream_signatures(code):
        fixed_code = code
        match_obj = re.search("define (\w+) @(\w+)\(([^\)]*)\)", code)
        args = match_obj.group(3)
        original_args = args
        if args:
            args = args.split(',')
            for _ in range(len(args)):
                args[_] = args[_].replace("_QMhls_streamThlsstream", '"class.hls::stream<int, 0>"') # TDOO: replace int by the corresponding type

        args = ', '.join(args)
        fixed_code = code.replace(original_args, args)

        #f_type = match_obj.group(1)
        #f_name = match_obj.group(2)


        for call_match_obj in re.finditer(f"call (\w+) @(\w+)\(([^\)]*)\)", fixed_code):
            f_type = call_match_obj.group(1)
            f_name = call_match_obj.group(2)
            call_args = call_match_obj.group(3)
            call_args = call_args.replace("_QMhls_streamThlsstream", '"class.hls::stream<int, 0>"')

            fixed_code = re.sub(f"call {f_type} @{f_name}\([^\)]*\)", f"call {f_type} @{f_name}({call_args}) ", fixed_code)



        # Fix allocas. TODO: this should be done by backtracking from the function call argumets to get the template types right
        fixed_code = re.sub("alloca %_QMhls_streamThlsstream", 'alloca %"class.hls::stream<int, 0>"', fixed_code)

        return fixed_code



    # TXT after LLVM pass
    def get_pipelines_ii(self):
        ii_f = open("ii.txt")

        ii_lst = list(map(lambda x: x.strip(), ii_f.readlines()))

        ii_f.close()

        return list(map(lambda x: int(x) if x != '' else -1, ii_lst))

    # TXT after LLVM pass
    def get_unroll_factors(self):
        unroll_factor_f = open("unroll_factor.txt")

        unroll_factor_lst = list(map(lambda x: x.strip(), unroll_factor_f.readlines()))

        unroll_factor_f.close()

        return list(map(lambda x: int(x) if x != '' else -1, unroll_factor_lst))

    # IR
    def set_metadata(self, iface_ir_code, function_stream_ssa_dict):
        code = iface_ir_code


        initiation_intervals = self.get_pipelines_ii()
        unroll_factors = self.get_unroll_factors()
            
        def get_ii(match_obj):
            ii = initiation_intervals[get_ii.counter]
            get_ii.counter += 1

            return f'!{match_obj.group(1)} = !{{!"llvm.loop.pipeline.enable", i32 {ii}, i1 false, i8 -1}}'
        get_ii.counter = 0

        code = re.sub('!(\d+) = !{!"llvm.loop.pipeline.enable"}', get_ii, code)

        def get_unroll_factor(match_obj):
            unroll_factor = unroll_factors[get_unroll_factor.counter]
            get_unroll_factor.counter += 1

            return f'!{match_obj.group(1)} = !{{!"llvm.loop.unroll.count", i32 {unroll_factor}}}'
        get_unroll_factor.counter = 0


        code = re.sub('!(\d+) = !{!"llvm.loop.unroll.count"}', get_unroll_factor, code)

        # Get last metadata index in the IR
        all_occurrences_lst = re.findall("!\d+ =", iface_ir_code)
        if all_occurrences_lst:
            match_obj = re.search("!(\d+)", all_occurrences_lst[-1])
            last_metadata_idx = match_obj.group(1)
        else:
            last_metadata_idx = -1

        curr_meta_idx = int(last_metadata_idx) + 1
        tail = f'!{curr_meta_idx} = !{{i32 1, !"wchar_size", i32 4}}\n' + \
                f'!{curr_meta_idx+1} = !{{!"clang version 7.0.0 "}}\n'
        curr_meta_idx += 2

        already_declared = set()

        n_type = 0
        for stream_ssa_dict in function_stream_ssa_dict.values():
            if stream_ssa_dict: # TODO: generate metadata for all the types declaraed at the high-level
                for var in stream_ssa_dict.keys():
                    stream_type = stream_ssa_dict[var][0]
                #for stream_type in stream_ssa_dict:
                    if stream_type in translate_builtin_types.keys():
                        llvm_type = translate_builtin_types[stream_type]
                    else:
                        llvm_type = stream_type

                    tail += f'!{curr_meta_idx + n_type*4} = !{{' + f'!{curr_meta_idx + 1+n_type*4}' + '}\n' + \
                            f'!{curr_meta_idx + 1+n_type*4} = !{{!"discrete.components", ' + f'!{curr_meta_idx + 2+n_type*4}' + '}\n' + \
                            f'!{curr_meta_idx + 2+n_type*4} = !{{' + f'!{curr_meta_idx + 3+n_type*4}' + '}\n' + \
                            f'!{curr_meta_idx + 3+n_type*4} = !{{!"stream", %{llvm_type}* null}}\n'
                    #curr_meta_idx += 4
                    n_type += 1

        code += tail

        return code

        f_iface = open("tmp/iface.ll", "w")
        f_iface.write(code)
        f_iface.close()
        f.close()



    def launch_vitis(self, output_file, kernel_name, mode, compile_stage, opt_level, platform):
        # compile_stage:
        # 0 - Skip v++
        # 1 - Just compile
        # 2 - Compile and link
        if opt_level is not None:
            vitis_opt_level = "-" + opt_level
        else:
            vitis_opt_level = ""


        if compile_stage == "0":
            print("OUTPUT FILE: ", output_file)
            shutil.copy("tmp/iface.ll", f"{output_file}.ll")
            return
        elif compile_stage == "1" or compile_stage == "2": 
            if self.temp_dir == None:
                temp_dir = "_x_" + output_file
            else:
                temp_dir = self.temp_dir
            subprocess.call(f"{self.xilinx_llvm_path}/llvm-as tmp/iface.ll -o {output_file}.xpirbc", shell=True)
            subprocess.call(f"v++ -c -t {mode} --platform {platform} {output_file}.xpirbc -o {output_file}.xo -k {kernel_name} {vitis_opt_level} --vivado.impl.jobs 24 --temp_dir {temp_dir}", shell=True)

            if compile_stage == "2":
                subprocess.call(f"v++ -l -t {mode} --platform {platform} {output_file}.xo -o {output_file}.xclbin {vitis_opt_level} --vivado.impl.jobs 24 --temp_dir {temp_dir}", shell=True)

    @staticmethod
    def fix_stream_names():
        f = open("preprocessor.tmp")
        code = f.read()
        f.close()
    #
        code = re.sub("integer :: _stream_type_(\w+)\((\w+)\)_(\w+)", "integer :: _stream_type_\g<1>\g<2>_\g<3>", code)

        f = open("preprocessor.tmp", "w")
        f.write(code)
        f.close()

    def __init__(self):
        parser = argparse.ArgumentParser()
        parser.add_argument("filename", type=str, help="File to build")
        parser.add_argument("output_file", type=str, help="Output name: <output_file>.xo, <output_file>.xclbin")
        parser.add_argument("gen_llvm_ir", type=str, help="Generate LLVM IR: <0/1>") # TODO: this is just for debugging purposes. Remove for the artifact release
        parser.add_argument("kernel_name", type=str, help="Name of the top function")
        parser.add_argument("mode", type=str, help="Compilation mode. Supported: hw_emu, hw")
        parser.add_argument("compile_stage", type=str, help="Compilation stage: 0 - skip v++; 1 - just compile; 2 - compile and link")
        parser.add_argument("oracle", type=str, help="Generate oracle IR for unit tests")
        parser.add_argument("-I", action='append', help="Include directory in compilation")
        parser.add_argument("-k", action='append', help="List of kernel names. This has to be provided when the input is a .ll file")
        parser.add_argument("-temp_dir", help="Name of the temporary directory")
        parser.add_argument("-O0", action="store_const", const="O0", dest="O0", help="Optimization level 0")
        parser.add_argument("-O1", action="store_const", const="O1", dest="O1", help="Optimization level 1")
        parser.add_argument("-O2", action="store_const", const="O2", dest="O2", help="Optimization level 2")
        parser.add_argument("-O3", action="store_const", const="O3", dest="O3", help="Optimization level 3")
        parser.add_argument("-xilinx_generated", action="store_true")
        parser.add_argument("-platform")
        args = parser.parse_args()

        self.filename = args.filename
        self.output_file = args.output_file
        self.gen_llvm_ir = args.gen_llvm_ir
        self.kernel_name = args.kernel_name
        self.mode = args.mode
        self.compile_stage = args.compile_stage
        self.oracle = args.oracle
        self.included_directories = args.I
        self.arg_kernel_names = args.k
        self.temp_dir = args.temp_dir
        self.opt_level = None

        self.xilinx_generated = args.xilinx_generated

        n_optimisation_levels_enabled = 0
        if args.O0:
            self.opt_level = args.O0
            n_optimisation_levels_enabled += 1
        if args.O1:
            self.opt_level = args.O1
            n_optimisation_levels_enabled += 1
        if args.O2:
            self.opt_level = args.O2
            n_optimisation_levels_enabled += 1
        if args.O3:
            self.opt_level = args.O3
            n_optimisation_levels_enabled += 1

        if args.platform:
            self.platform = args.platform
        else:
            self.platform = "xilinx_u280_xdma_201920_3"

        if n_optimisation_levels_enabled > 1:
           print("Choose just one optimisation level!")
           sys.exit(WRONG_NUMBER_OPTIMISATIONS)

        self.fxx_dir = pathlib.Path(__file__).parent.resolve()

        self.llvm_path = os.environ.get("FXX_LLVM_PATH")
        self.xilinx_llvm_path = os.environ.get("FXX_XILINX_LLVM_PATH")
        self.flang_path = os.environ.get("FXX_FLANG_PATH")
        self.llvm_passes_path = os.path.join(pathlib.Path(__file__).parent.resolve(), "../llvm-passes-f18/build-v16")
        self.xilinx_passes_path = os.path.join(pathlib.Path(__file__).parent.resolve(), "../llvm-passes-f18/build-v7")
        self.flang_bin = f"{self.flang_path}/flang-new"

    def run(self):
        try:
            os.mkdir("tmp")
        except:
            pass


        filetype = os.path.basename(self.filename).split('.')[1]

        if self.gen_llvm_ir == "1":
            if filetype == "f90":
                fp = FortranProcessor(self.flang_path)

                # Process HLS pragmas
                f = open(self.filename)
                code = f.read()
                f.close()

                kernels = self.extract_subroutines_names(code)
                print("---KERNELS: ", kernels)
                
                # TODO: fix this. It breaks the synthetic streams benchmark
                #code = fp.replace_intrinsics(code)
                #print(code)

                code = fp.process_hls_unroll(code)

                code = fp.process_hls_pipeline(code)

                code = fp.process_hls_interface(code)

                code = fp.process_hls_array_partition(code)

                code, is_dataflow = fp.process_hls_dataflow(code)

                f = open("tmp/pragma_code.f90", "w")
                f.write(code)
                f.close()


                # Get stream types by using the preprocessor
                #subprocess.call(f'{self.flang_bin} -E -o tmp/preprocessor.tmp tmp/pragma_code.f90 -I {self.fxx_dir}/../fortran_testcases', shell=True)
                if self.included_directories:
                    included_dirs = f"-I {' '.join(self.included_directories)}"
                else:
                    included_dirs = ""

                subprocess.call(f'{self.flang_bin} -E -o tmp/preprocessor.tmp tmp/pragma_code.f90 {included_dirs}', shell=True)

                #fix_stream_names()

                decl_stream_types_dict = fp.get_stream_types()



                f = open("tmp/preprocessor.tmp")
                code = f.read()
                # We remove kind=8 for easier procesing and assume kind=8 for reals. TODO: this should be processed in the future
                #code = re.sub("\(kind=\d+\)", "", code)
                f.close()
                f = open("tmp/preprocessor.tmp", "w")
                f.write(code)
                f.close()
                code = fp.remove_local_depths(code)

                fp.lower_polymorphic(decl_stream_types_dict, code)


                f = open("tmp/poly.f90")
                code = f.read()

                code = fp.process_hls_streams(code, decl_stream_types_dict)

                out_f = open("tmp/_pragmacalls.f90", "w")
                out_f.write(code)
                out_f.close()
                f.close()


                subprocess.call(f'{self.flang_bin} -mmlir "--opaque-pointers=false" -c -emit-llvm tmp/_pragmacalls.f90 -I ../../dataflow -I tests/ -o tmp/_pragmacalls.bc', shell=True)
                subprocess.call(f'{self.flang_path}/llvm-dis tmp/_pragmacalls.bc', shell=True)


            llvmp = LLVMIRProcessor(self.llvm_path, self.xilinx_llvm_path, self.llvm_passes_path)

            
            if filetype == 'll':
                subprocess.call(f'{self.flang_path}/llvm-as {self.filename} -o tmp/_pragmacalls.bc', shell=True)
                kernels = self.arg_kernel_names
                is_dataflow = False

                f = open("kernel_names.txt", "w")
                f.write('\n'.join(kernels))
                f.close()

            llvmp.set_input_file("tmp/_pragmacalls.bc")
            llvmp.set_output_file("tmp/downgraded_pragmacalls.ll")
            llvmp.downgrade()

            llvmp.qualify_kernels(kernels, "tmp/downgraded_pragmacalls.ll", "tmp/downgraded_pragmacalls_qualified.ll")

            f_downgraded = open("tmp/downgraded_pragmacalls_qualified.ll")
            downgraded_ir = f_downgraded.read()
            f_downgraded.close()





            if not self.xilinx_generated:
                downgraded_ir = re.sub("declare", f'declare i32 @_start_df_call()\ndeclare void @_end_df_call()\ndeclare', downgraded_ir, count=1)

            f_downgraded = open("tmp/downgraded_pragmacalls_qualified.ll", "w")
            f_downgraded.write(downgraded_ir)
            f_downgraded.close()


            subprocess.call(f"{self.xilinx_llvm_path}/opt  -load {self.xilinx_passes_path}/set_pragma_metadata/libSetPragmaMetadata.so --set_pragma_metadata --strip-dead-prototypes tmp/downgraded_pragmacalls_qualified.ll -o tmp/dataflow.bc", shell=True)
            subprocess.call(f"{self.xilinx_llvm_path}/llvm-dis tmp/dataflow.bc -o tmp/dataflow.ll", shell=True)


            # TODO: remove. This is just for debuggind purposes
            f = open("tmp/dataflow.ll")
            f_post = open("tmp/post_pass.ll", "w")
            f_post.write(f.read())
            f_post.close()
            f.close()


            #f = open("downgraded_pragmacalls_qualified.ll")
            f = open("tmp/dataflow.ll")
            ir_code = f.read()
            f.close()

            ir_code = self.add_array_partition_primitives_ir(ir_code)
            ir_code = self.add_dataflow_primitives_ir(ir_code, is_dataflow)
            ir_code = self.add_stream_sizes(ir_code)
            ir_code = self.add_interface_qualifiers(ir_code) # Relevant when we get a .ll file


            function_stream_ssa_dict = {}
            functions_def = llvmp.extract_functions_def(ir_code)
            for fdef in functions_def:
                fdef_ir = fdef
                original_fdef_ir = fdef_ir
                fdef_ir, stream_ssa_dict = self.replace_llvm_calls(fdef_ir, self.xilinx_generated)

                match_obj = re.search("define \w+ @(\w+)\([^\(]*\)", fdef_ir)
                function_name = match_obj.group(1)

                function_stream_ssa_dict[function_name] = stream_ssa_dict

                ir_code = ir_code.replace(original_fdef_ir, fdef_ir)


            # Count how many arguments there are
            ir_lines = ir_code.split('\n')
            ir_code, n_attributes = self.add_attributes(ir_code, ir_lines)
            iface_ir_code = self.add_declarations_ir(ir_code, n_attributes, self.xilinx_generated)

            iface_ir_code = self.set_metadata(iface_ir_code, function_stream_ssa_dict)

            f = open("tmp/iface.ll", "w")
            f.write(iface_ir_code)
            f.close()


            f = open("tmp/iface.ll", "r")
            code = f.read()
            f.close()
            f = open("tmp/iface.ll", "w")
            code = re.sub('target triple = "fpga32-xilinx-none"', 'target triple = "fpga64-xilinx-none"', code)
            f.write(code)
            f.close()

            if self.oracle == "1":
                self.generate_oracle(self.filename, code)



        self.launch_vitis(self.output_file, self.kernel_name, self.mode, self.compile_stage, self.opt_level, self.platform)

    if __name__ == "__main__":
        main()
