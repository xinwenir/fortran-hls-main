"""
***************************************************************
    Copyright 2023 Hewlett Packard Enterprise Development LP.
***************************************************************
"""

import os
import subprocess
import argparse
import re
from functools import reduce
 
class FortranProcessor:
    def __init__(self, flang_path):
        """
        This class processes Fortran source code and applies the necessary transformations at the source code level to generate LLVM IR compatible with v7 in the 
        next step with the LLVM IR processor.

        :param flang_path: path of the build directory of F18
        :type flang_path: str
        """
        self.flang = flang_path + "flang-new"

    def extract_subroutine_names(self, in_filename, out_filename):
        """
        Extracts the names of all the subroutines in the input Fortran file and writes the to a file, one per line.

        :param in_filename: path of the Fortran source code
        :type in_filename: str
        :param out_filename: path of the file containing the subroutine names
        :type out_filename: str
        """
        f_in = open(in_filename, "r")
        f_out = open(out_filename, "w")
        input_ir = f_in.read()

        for subroutine_matchobj in re.finditer("subroutine\s*(\w+)\s*\(", input_ir, re.I):
            subroutine_name = subroutine_matchobj.group(1)
            f_out.write(subroutine_name + "\n")

        f_in.close()
        f_out.close()

    def limit_dimensions(self, in_filename, out_filename):
        """
        Limits the dimensions of asssumed shaped arrays (dimension(:)) setting the dimension to 1000 (arbitrary number).

        :param in_filename: path of the Fortran source code
        :type in_filename: str
        :param out_filename: path of the output Fortran source code with all the arrays with concrete shape.
        :type out_filename: str
        """

        f_in = open(in_filename, "r")
        f_out = open(out_filename, "w")

        code = f_in.read()

        for subroutine_match in re.finditer("subroutine[ ]+\w+(.|\n)*?end subroutine", code, re.I):
            subroutine_str = subroutine_match.group(0)

            args_match = re.search("\((.|\n)*?\)", subroutine_match.group(0))
            # Extract arguments
            subroutine_args = set(re.findall("\w+", args_match.group(0)))

            mod_subroutine_str = subroutine_str
            #for def_arg_matchobj in re.finditer("(\w+)[ ]*(\([ ]*kind[^\)]+\))?[ ]*::([\w|,| ]+)", subroutine_match.group(0), re.IGNORECASE):
            for def_arg_matchobj in re.finditer("[\w|,|\(|\)|\s|=]*(dimension\(\s*:\s*\))[\w|,|\(|\)\s]*::[\w|,|\s|&]+\n", subroutine_match.group(0), re.IGNORECASE):
                def_arg_limited_dimension = re.sub("dimension\([^\)]*\)", "dimension(1000)", def_arg_matchobj.group(0), flags=re.I)

                mod_subroutine_str = mod_subroutine_str.replace(def_arg_matchobj.group(0), def_arg_limited_dimension)

            code = code.replace(subroutine_str, mod_subroutine_str)

        f_out.write(code)
        f_in.close()
        f_out.close()


    def qualify_with_value(self, in_filename, out_filename):
        """
        Qualifies scalar arguments in Fortran subroutines with the value attribute.

        :param in_filename: path of the Fortran source code.
        :type in_filename: str
        :param out_filename: path of the Fortran source code qualified with value attribute
        :type out_filename: str
        """
        f_in = open(in_filename, "r")
        f_out = open(out_filename, "w")

        code = f_in.read()

        #builtin_types = ["integer", "real", "complex", "logical", "character"]
        for subroutine_match in re.finditer("subroutine[ ]+\w+(.|\n)*?end subroutine", code, re.IGNORECASE):
            subroutine_str = subroutine_match.group(0)

            args_match = re.search("\((.|\n)*?\)", subroutine_match.group(0))
            # Extract arguments
            subroutine_args = set(re.findall("\w+", args_match.group(0)))

            mod_subroutine_str = subroutine_str
            for def_arg_matchobj in re.finditer("(\w+)\s*(\(\s*kind[^\)]+\))?(\s*,\s*\w+)*\s*::([\w|,| ]+)", subroutine_match.group(0), re.IGNORECASE):
                def_args=re.findall("\w+", def_arg_matchobj.group(4))
                def_dummy = list(filter(lambda x: x in subroutine_args, def_args))
                def_dummy= ','.join(def_dummy)
                def_nodummy = list(filter(lambda x: x not in subroutine_args, def_args))
                def_nodummy= ','.join(def_nodummy)

                def_type = def_arg_matchobj.group(1)
                def_kind = def_arg_matchobj.group(2)
                def_remainder = def_arg_matchobj.group(3)
                if def_remainder is None: 
                    def_remainder = ''
                def_kind = '' if def_kind is None else def_kind
                def_string = ""
                if def_dummy != "":
                    def_string += f"{def_type} {def_kind} {def_remainder}, VALUE :: {def_dummy}" 
                if def_nodummy != "":
                    def_string += f"\n{def_type} {def_kind} {def_remainder} :: {def_nodummy}"

                mod_subroutine_str = mod_subroutine_str.replace(def_arg_matchobj.group(0), def_string)

            code = code.replace(subroutine_str, mod_subroutine_str)

        f_out.write(code)
        f_in.close()
        f_out.close()

    def emit_llvm(self, in_filename, out_filename, include_path):
        """
        Emits LLVM IR from F18 without opaque pointers, as v++ needs explicit types for function arguments.

        :param in_filename: name of the input Fortran source code
        :type in_filename: str
        :param out_filename: name of the output LLVM IR bytecode file
        :type out_filename: str
        :param include_path: name of the path to include in the compilation
        :type include_path: str
        """
        subprocess.call(f'{self.flang} -mmlir "--opaque-pointers=false" -c -emit-llvm {in_filename} -I {include_path} -o {out_filename}', shell=True)

    def qualify_with_value_and_emit_llvm(self, in_filename, out_filename, include_path):
        """
        Qualifies scalar arguments in each subroutine in the input Fortran source code file.

        :param in_filename: name of the input Fortran source code
        :type in_filename: str
        :param out_filename: name of the LLVM IR bytecode output file with scalar arguments qualified with the value attribute
        :type out_filename: str
        :param include_path: name of the path to include in the compilation
        :type include_path: str
        """
        temp_qualified_with_value = "tmp/temp_qualified_with_value.f90"
        self.qualify_with_value(in_filename, temp_qualified_with_value)
        self.emit_llvm(temp_qualified_with_value, out_filename, include_path)


    # HLS PRAGMAS
    def process_hls_unroll(self, code):
        code = re.sub("!\$\s*hls unroll factor=(\d+)", "call _unroll_\g<1>()", code)
        code = re.sub("!\$\s*hls unroll", "call _unroll()", code)
        code = re.sub("!\$\s*hls end unroll", "", code)

        return code 

    def process_hls_array_partition(self, code):
        # Find number of dimensions of the array
        code = re.sub("!\$\s*hls\s*array_partition variable=(\w+) type=(\w+) factor=(\d+) dim=(\d+)", "call partition_\g<2>_\g<3>_\g<4>(\g<1>)", code)
        code = re.sub("!\$\s*hls\s*array_partition variable=(\w+) type=(\w+) factor=(\d+)", "call partition_\g<2>_\g<3>_1(\g<1>)", code)
        code = re.sub("!\$\s*hls\s*array_partition variable=(\w+) type=(\w+)", "call partition_\g<2>_0_1(\g<1>)", code)

        return code

    def process_hls_pipeline(self, code):
        # II provided
        code = re.sub("!\$\s*hls pipeline ii=(\d+)", "call_pipeline_\g<1>()", code)

        # No II
        code = re.sub("!\$\s*hls pipeline", "call_pipeline()", code)
        code = re.sub("!\$\s*hls end pipeline", "", code)

        return code

    def process_hls_interface(self, code):
        code = re.sub("!\$ hls interface port=(\w+) mode=m_axi bundle=(\w+)", "call _interface_maxi_\g<2>(\g<1>)", code)
        code = re.sub("!\$ hls interface port=(\w+) mode=(\w+) bundle=(\w+)", "call _interface_\g<2>_\g<3>(\g<1>)", code)
        code = re.sub("!\$ hls interface port=(\w+) mode=(\w+)", "call _interface_\g<2>_none(\g<1>)", code)
        code = re.sub("!\$ hls end interface", "", code)

        return code

    def process_hls_dataflow(self, code):
        is_dataflow = False
        if re.search("!\$ hls dataflow", code):
            is_dataflow = True
        code = re.sub("!\$ hls dataflow", "call _dataflow()", code)
        code = re.sub("!\$ hls end dataflow", "", code) 

        return code, is_dataflow

    def process_hls_streams(self, code, decl_stream_types):
        code = re.sub("integer :: _stream_type_([a-zA-Z0-9]+)_(\w+)", "call _stream_\g<2>_NAME_\g<1>(\g<1>)", code)

        return code

    # Preprocessing before polymorphic lowering
    # Fortran
    def remove_local_depths(self, code_preproc):
        code_no_depths = code_preproc
        code_no_depths = re.sub("&\n\s*&", "", code_no_depths)
        code_no_depths = re.sub(";", "\n", code_no_depths)

        code_original = code_no_depths
        output_code = code_original

        regex = ["subroutine \w*\s*\(?([^\)]*)\)?.*?end subroutine",
                 "function \w*\s*\(?([^\)]*)\)?.*?end function"]
                

        for r in regex:
            for match_obj in re.finditer(r, code_no_depths, flags=re.DOTALL):
                subr_code = match_obj.group(0)
                original_subr_code = subr_code
                subr_args = match_obj.group(1)

                if subr_args:
                    subr_args = subr_args.split(',')

                for arg in subr_args:
                    arg = arg.strip(' ')
                    subr_code = re.sub(f"call set_depth_\w+\({arg}(\(\w+\))?%\w+, \w+\)", "", subr_code)

                output_code = output_code.replace(original_subr_code, subr_code)

        return output_code


    # Fortran
    def get_stream_types(self):
        f_preproc = open('tmp/preprocessor.tmp')
        code_preproc = f_preproc.read()
        code_preproc = re.sub("&\n\s*&", "", code_preproc)
        code_preproc = re.sub(";", "\n", code_preproc)
        #code_preproc = code_preproc.replace("&\n", "")
        f_preproc.close()

        decl_stream_types_dict = {}

        # Check if the stream is created in the function or is an argument. If passed as argument the declaration is already somewhere else
        orig_code_preproc = code_preproc

        regex = ["subroutine (\w+)*\s*\(?([^\)]*)\)?.*?end subroutine",
                 "function (\w+)*\s*\(?([^\)]*)\)?.*?end function"]

        # Remove interface block, as it interferes with the regex in the following step
        while True:
            match_obj=re.search("interface.*?end interface", orig_code_preproc, flags=re.DOTALL)
            if match_obj:
                orig_code_preproc = re.sub("interface.*?end interface", "", orig_code_preproc, flags=re.DOTALL, count=1)
            else:
                break

        print(orig_code_preproc)

        #for match_obj in re.finditer("integer :: _stream_type_([a-zA-Z0-9]+)_(\w+)", orig_code_preproc):
        for r in regex:
            for match_obj in re.finditer(r, orig_code_preproc, flags=re.DOTALL):
                subr_code = match_obj.group(0)
                original_subr_code = subr_code
                subr_name = match_obj.group(1)
                decl_stream_types_dict[subr_name] = {}

                #for submatch_obj in re.finditer("call set_depth_\w+\(([^\%]+)%data_(\w+), \d+\)", original_subr_code):
                #for submatch_obj in re.finditer("call set_depth_\w+(\(kind=\d+\))?\(([^\%]+)%data_(\w+)(\(kind=\d+\))?, \d+\)", original_subr_code):
                for submatch_obj in re.finditer("call set_depth_\w+(\(kind=\d+\))?\(([^%]+)%data_(\w+)(\(kind=\d+\))?, \d+\)", original_subr_code):
                    stream_name = submatch_obj.group(2)		
                    stream_name = stream_name.split("(")[0]
                    stream_type = submatch_obj.group(3)

                    decl_stream_types_dict[subr_name][stream_name] = stream_type

        return decl_stream_types_dict

    def hls_read_to_register(self, code_preproc, decl_stream_types_dict):


        reg_counter = 0


        regex = ["subroutine (\w+)*\s*\(?([^\)]*)\)?.*?end subroutine",
                 "function (\w+)*\s*\(?([^\)]*)\)?.*?end function"]

        orig_code_preproc = code_preproc

        for r in regex:
            for func_match_obj in re.finditer(r, orig_code_preproc, flags=re.DOTALL):
                register_declarations = ""
                subr_code = func_match_obj.group(0)
                print("*********** SUBR_CODE: ", subr_code)
                subr_name = func_match_obj.group(1)
                orig_subr_code = subr_code

                code_lines = subr_code.split('\n')
                read_as_reg_code = []

                for line in code_lines:
                    reg_line = ""
                    line_regs = []

                    n_reads_line = len(re.findall("hls_read\([^\)]+\)", line))

                    for match_obj in re.finditer("hls_read\(([^\)]+)\)", line):
                        stream_name = match_obj.group(1)

                        reg_line += f"tmp{reg_counter} = hls_read({match_obj.group(1)})\n"
                        line_regs.append(reg_counter)

                        line = line.replace(match_obj.group(0), f"tmp{reg_counter}")


                        stream_type = decl_stream_types_dict[subr_name][stream_name]
                        register_declarations += f"{stream_type} :: tmp{reg_counter}\n"
                        reg_counter += 1

                    read_as_reg_code.append(reg_line + line)

                final_subr_code = "\n".join(read_as_reg_code)


                # Declare the registers explicitly, add before the first declaration (if we add it just after the signature we will declare the registers
                # before the use statement
                first_declaration = ""

                for line in code_lines:
                    if "::" in line:
                        first_declaration = line
                        break

                # Escape parentheses in case we are dealing with an array declaration
                first_declaration_escaped = first_declaration.replace("(", "\(")
                first_declaration_escaped = first_declaration_escaped.replace(")", "\)")
                #first_declaration = "integer, dimension(100) :: a, b"

                print("----->>>>>> FIRST DECLARATION: ", first_declaration)
                print("----->>>>>> REGISTER DECLARATIONS: ", register_declarations)


                final_subr_code = re.sub(first_declaration_escaped, f"{register_declarations}\n{first_declaration}", final_subr_code)

                print("FINAL SUBR CODE: ", final_subr_code)

                code_preproc = code_preproc.replace(orig_subr_code, final_subr_code) 

        return code_preproc

    # Fortran
    def lower_polymorphic(self, filetype, decl_stream_types_dict, code_preproc):
        # TODO: genralise this. Just done this to get the experimentation
        code_preproc = re.sub("packed_data, value", "type(packed_data), value", code_preproc)
        #f_preproc = open('fixed_preprocessor.tmp')
        #f_preproc = open('preprocessor.tmp')
        #code_preproc = f_preproc.read()
        #f_preproc.close()

        code_preproc = self.set_derived_types(code_preproc)
        orig_code_preproc = code_preproc

        code_poly = ''
        regex = ["subroutine (\w+)*\s*\(?([^\)]*)\)?.*?end subroutine",
                 "function (\w+)*\s*\(?([^\)]*)\)?.*?end function"]


        #for match_obj in re.finditer("integer :: _stream_type_([a-zA-Z0-9]+)_(\w+)", orig_code_preproc):
        for r in regex:
            for match_obj in re.finditer(r, orig_code_preproc, flags=re.DOTALL):
                subr_code = match_obj.group(0)
                original_subr_code = subr_code
                subr_name = match_obj.group(1)
                original_subr_code = subr_code

                # Replace polymorphic hls_read to hls_read_<type> calls
                for regex in ["(\w+)\s*=\s*hls_read\((\w+\(\w+\))\)", "(\w+)\s*=\s*hls_read\(([^\)]+)\)",
                              "hls_read\((\w+\(\w+\))\)", "hls_read\(([^\)]+)\)"]:
                #for regex in ["hls_read\((\w+\(\w+\))\)", "hls_read\(([^\)]+)\)"]:
                    for match_obj in re.finditer(regex, subr_code): 
                        if len(match_obj.groups()) == 2: # hls_read returns a result
                            result_var = match_obj.group(1)
                            stream_name = match_obj.group(2)
                            stream_name = stream_name.split('%')[0]
                            stream_name = stream_name.split("(")[0]
                            stream_type = decl_stream_types_dict[subr_name][stream_name]

                            subr_code = re.sub(regex, f"\g<1> = hls_lowered_read_{stream_type}(\g<2>%data_{stream_type})", subr_code)
                        elif len(match_obj.groups()) == 1:
                            stream_name = match_obj.group(1)
                            stream_name = stream_name.split('%')[0]
                            stream_name = stream_name.split("(")[0]
                            stream_type = decl_stream_types_dict[subr_name][stream_name]

                            subr_code = re.sub(regex, f"hls_lowered_read_{stream_type}(\g<1>%data_{stream_type})", subr_code)

                        #subr_code = re.sub(regex, f"hls_lowered_read_{stream_type}(\g<2>%data_{stream_type})", subr_code)
                        #print("----------------> SUBR CODE: ", subr_code)

                # Replace polymorphic hls_write to hls_write_<type> calls
                                             # hls_write(s1%data_integer, input)

                #for regex in ["hls_write\((\w+)\s*,\s*(\w+\(\w+\))\)", "hls_write\((\w+)\s*,\s*([^\)"]:
                for regex in ["hls_write\((\w+)\s*,\s*(\w+\(\w+\))\)", "hls_write\((\w+)\s*,\s*([^\)]+)\)", "hls_write\(([^,]+),\s*([^\)]+)\)"]:
                    for match_obj in re.finditer(regex, subr_code): 
                        stream_name = match_obj.group(2)
                        stream_name = stream_name.split('%')[0]
                        stream_name = stream_name.split('(')[0]
                        stream_type = decl_stream_types_dict[subr_name][stream_name]
                        write_data = match_obj.group(1)

                        subr_code = re.sub(regex, f"hls_lowered_write_{stream_type}({write_data}, \g<2>%data_{stream_type})", subr_code, count=1)


                # Replace polymorphic hls_full to hls_full_<type> calls
                #for match_obj in re.finditer("hls_full\((\w+)%data_(\w+)\)\s*\)", subr_code): 
                ##%9 = call float @_Z7hls_full_(%_QMhls_streamThlsstream_integer* %5)
                for regex in ["hls_full\((\w+\(\w+\))\)", "hls_full\((\w+)\)"]:
                    for match_obj in re.finditer(regex, original_subr_code): 
                        stream_name = match_obj.group(1)
                        stream_type = decl_stream_types_dict[subr_name][stream_name]

                        subr_code = re.sub(regex, f"hls_lowered_full_{stream_type}(\g<1>%data_{stream_type})", subr_code, count=1)

                # Replace polymorphic hls_empty to hls_empty_<type> calls
                for regex in ["hls_empty\((\w+\(\w+\))\s*\)", "hls_empty\((\w+)\s*\)"]:
                    for match_obj in re.finditer(regex, original_subr_code): 
                        stream_name = match_obj.group(1)
                        stream_name = stream_name.split("(")[0]
                        #stream_type = match_obj.group(2)
                        stream_type = decl_stream_types_dict[subr_name][stream_name]

                        subr_code = re.sub(regex, f"hls_lowered_empty_{stream_type}(\g<1>%data_{stream_type})", subr_code, count=1)

                code_preproc = code_preproc.replace(original_subr_code, subr_code)

        #code_preproc = re.sub("real\s*,([\w:,\(\)]+)::", "real(kind=8),\g<1>::", code_preproc)
        code_preproc = re.sub("real\s*,\s*dimension", "real(kind=8),dimension", code_preproc)
        code_preproc = re.sub("real\s*(,\s*value\s*)?::", "real(kind=8)\g<1> ::", code_preproc)
        code_preproc = re.sub("data_real\(kind=8\)", "data_real", code_preproc)
        code_preproc = re.sub("real\s*function", "real(kind=8) function", code_preproc)


        if filetype == "f90":
            f_poly = open("tmp/poly.f90", "w")
        else:
            f_poly = open("tmp/poly.f", "w")
        f_poly.write(code_preproc)
        f_poly.close()

    # Fortran
    def set_derived_types(self, code):
        # Fixes derived datatypes used from macros. E.g. proto_hls_stream(packed_data) will generate code such as packed_data :: x, we want 
        # typed(packed_data) :: x instead

        # Get all the derived datatypes first
        derived_datatypes = set()
        for match_obj in re.finditer("type\s*::\s*(\w+)", code):
            der_type = match_obj.group(1)
            derived_datatypes.add(der_type)


        ## Apply only to the module hls_streams
        match_obj = re.search("module hls_stream.*end module", code, flags=re.DOTALL)
        if match_obj:
            hls_stream_mod = match_obj.group(0)
            original_hls_stream_mod = hls_stream_mod

            hls_stream_mod = re.sub("&\n\s*&", "", hls_stream_mod)
            hls_stream_mod = re.sub(";", "\n", hls_stream_mod)



            # Replace wrong uses of the derived type <type> by type(<type>)
            for der_type in derived_datatypes:
                # Declarations
                hls_stream_mod = re.sub(f"{der_type}\s*::\s+(\w+)", f"type({der_type}) :: \g<1>", hls_stream_mod)
                # Return values
                hls_stream_mod = re.sub(f"{der_type}\s+function\s+(\w+)\(([^\)]+)\)", f"type({der_type}) function \g<1>(\g<2>)", hls_stream_mod)

            code = code.replace(original_hls_stream_mod, hls_stream_mod)

        return code

    # some Fortran intrinsics such as abs are not supported by the Xilinx backend. Some of them
    # will be replaced here at the source code level by equivalent subroutines that will be lowered
    # to cokmpatible LLVM IR. Others are replaced directly over the IR on the LLVM IR processor
    def replace_intrinsics(self, code):
        abs_subroutine = " \
        function _fxx_abs(x)\n \
            if x >= 0 then \n\
                return x \n\
            else \n\
                return -x \n\
            end if\n \
        end function \
        "

        abs_re = "abs\(([^\)]+)\)"

        if re.search(abs_re, code, flags=re.I):
            code = re.sub(abs_re, "_fxx_abs(\g<1>)", code, flags=re.I)

        code = re.sub("subroutine", abs_subroutine + "\nsubroutine", code, flags=re.I, count=1)

        print(code)

        return code

