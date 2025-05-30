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
from fortran_hls.exceptions import NoneFilename
import itertools

class LLVMIRProcessor:
    def __init__(self, llvm_path, xilinx_llvm_path, llvm_passes_path):
        """
        This class processes LLVM IR and adapts it to be compliant with LLVM IR v7 and the particularities of the Xilinx backend. For the basic translation
        to v7 the LLVM for SYCL developed by the Khronos Group has been adapted to run standalone here. Further modifications are needed for the IR 
        generated by F18 to taken by v++.

        :param llvm_path: path of the LLVM build directory
        :type llvm_path: str
        :param xilinx_llvm_path: path of the Xilinx specific LLVM build directory
        :type xilinx_llvm_path: str
        :param llvm_passes_path: path of the LLVM passes for F18
        :type llvm_passes_path: str
        """
        self.llvm_dis = llvm_path + '/llvm-dis'
        self.llvm_as = llvm_path + '/llvm-as'
        self.opt = llvm_path + '/opt'
        self.xilinx_opt = xilinx_llvm_path + '/opt'
        self.in_filename = None
        self.out_filename = None
        self.llvm_passes_path = llvm_passes_path
        self.libdowngrader = llvm_passes_path + "/vxxdowngrader/libVXXDowngrader.so"
        self.libextractfunctions = llvm_passes_path + "/extract_subroutines/libExtractFunctions.so"

        try:
            os.mkdir("tmp")
        except:
            pass

    def set_input_file(self, in_filename):
        """
        Sets the input file for the processors's method by default. Otherwise, the files must be specified in a every call to the methods. If not provided by any 
        means, then the NoneFilename exceptin is thrown.

        :param in_filename: default input file for the processor's methods.
        :type in_filename: str
        """
        self.in_filename = in_filename

    def set_output_file(self, out_filename):
        """
        Sets the output file for the processors's method by default. Otherwise, the files must be specified in a every call to the methods. If not provided by any 
        means, then the NoneFilename exception is thrown.

        :param out_filename: default input file for the processor's methods.
        :type out_filename: str
        """
        self.out_filename = out_filename

    def disassemble(self, in_filename=None, out_filename=None):
        """
        Disassembles the input LLVM IR bytecode into human-readable format.

        :param in_filename: name of the input LLVM IR bytecode. Typically .bc extension
        :type in_filename: str
        :param out_filename: name of the output LLVM IR human-readable format. Typically .ll extension
        :type out_filename: str
        """
        if self.none_files(in_filename, out_filename):
            raise NoneFilename
        else:
            subprocess.call(f'{self.llvm_dis} {in_filename} -opaque-pointers=0 -o {out_filename}', shell=True)

    def assemble(self, in_filename=None, out_filename=None):
        """
        Assembles the input LLVM IR human-readable format into LLVM IR bytecode.

        :param in_filename: name of the input LLVM IR human-readable format. Typically .ll extension
        :type in_filename: str
        :param out_filename: name of the output LLVM IR bytecode. Typically .bc. extension
        :type out_filename: str
        """
        if self.none_files(in_filename, out_filename):
            raise NoneFilename
        else:
            subprocess.call(f'{self.llvm_as} {in_filename} -opaque-pointers=0 -o {out_filename}', shell=True)

    def strip_debug(self, in_filename=None, out_filename=None):
        """
        Removes debug metadata from the input LLVM IR.

        :param in_filename: name of the input LLVM IR, either bytecode or human-readable format.
        :type in_filename: str
        :param out_filename: name of the output LLVM IR, either bytecode or human-readable format.
        :type out_filename: str
        """
        if self.none_files(in_filename, out_filename):
            raise NoneFilename
        else:
            subprocess.call(f'{self.opt} -opaque-pointers=0 -strip-debug {in_filename} -o {out_filename}', shell=True)
            print("llvm-opt: strip_debug")

    def _downgrade(self, in_filename=None, out_filename=None):
        """
        Downgrades the input LLVM IR to v7 using the Khronos Group's LLVM pass (--downgrader)

        :param in_filename: name of the input LLVM IR, either bytecode or human-readable format.
        :type in_filename: str
        :param out_filename: name of the output LLVM IR
        :type out_filename: str
        """
        # For internal use

        if self.none_files(in_filename, out_filename):
            raise NoneFilename
        else:
            subprocess.call(f'{self.opt} -opaque-pointers=0 --load={self.libdowngrader} --downgrader -enable-new-pm=0 {in_filename} -o {out_filename}', shell=True)
            print("llvm-opt: _downgrade")

    def none_files(self, in_filename, out_filename):
        return (in_filename is None or out_filename is None) and (self.in_filename is None or self.out_filename is None)

    def none_file(self, in_filename):
        return (in_filename is None and self.in_filename is None)

    @staticmethod
    def get_attribute(func_name, attr_index):
        """
        Returns the attribute that qualifies a kernel to be identified as such by the Xilinx backend.

        :param func_name: name of the function to be qualified. This must be the name as it appears in the Fortran source code and not a mangled version.
        :type func_name: str
        :attr_index: index of the attribute (#{attr_index}, e.g. #0)
        :type attr_index: int
        :return: attribute that qualifies a kernel to be identified as such by v++. 
        :rtype: str
        """
        attribute = f'attributes #{attr_index} = {{ nounwind uwtable "correctly-rounded-divide-sqrt-fp-math"="false" "disable-tail-calls"="false" "fpga.demangled.name"="{func_name}" "fpga.top.func"="{func_name}" "less-precise-fpmad"="false" "no-frame-pointer-elim"="true" "no-frame-pointer-elim-non-leaf" "no-infs-fp-math"="false" "no-jump-tables"="false" "no-nans-fp-math"="false" "no-signed-zeros-fp-math"="false" "no-trapping-math"="false" "stack-protector-buffer-size"="8" "target-cpu"="x86-64" "target-features"="+fxsr,+mmx,+sse,+sse2,+x87" "unsafe-fp-math"="false" "use-soft-float"="false" }}'

        return attribute

    def qualify_kernels(self, func_names_lst, in_filename=None, out_filename=None):
        """
        Qualifies kernel functions with the kernel attribute to be identified as such by v++. It also removes attributes that are not part of the v7 standard.

        :param func_names_lst: list of functions that are going to be qualified as kernels in the LLVM IR
        :type func_names_lst: list[str]
        :param in_filename: LLVM IR in human-readable format
        :type in_filename: str
        :param out_filename: LLVM IR in human-readable format with qualified kernels.
        :type out_filename: str
        """

        # Function attributes of the LLVM IR 7 specification.
        # URL: https://releases.llvm.org/7.0.0/docs/LangRef.html#attribute-groups
        func_attr_7 = {"alingstack", "allocsize", "alwaysinline", "builtin", "cold", "convergent", "inaccessiblememonly", "inaccessiblemem_or_argmemonly", "inlinehint", "jumptable", "minsize", "naked", "no-jump-tables", "nobuiltin", "noduplicate", "noimplicitfloat", "noinline", "nonlazybind", "noredzone", "noreturn", "norecurse", "nounwind", "null-pointer-is-valid", "optforfuzzing", "optone", "optsize", "patchable-function", "probe-stack", "readnone", "readonly", "stack-probe-size", "no-stack-arg-probe", "writeonly", "argmemonly", "returns_twice", "safestack", "sanitize_address", "sanitize_memory", "sanitize_thread", "sanitize_hwaddress", "speculatable", "ssp", "sspreq", "sspstrong", "sstrictfp", "thunk", "uwtable", "nofc_check", "shadowcallstack"}

        if self.none_files(in_filename, out_filename):
            raise NoneFilename
        else:
            f_in = open(in_filename)
            f_out = open(out_filename, "w")
            input_ir = f_in.read()

            # Remove non compatible attributes
            for match_obj in re.finditer("attributes #([0-9]+) = {([^}]+)}", input_ir):
                n_attrib = match_obj.group(1)
                attribs = match_obj.group(2).split()
                attribs_7_lst = list(filter(lambda at: at in func_attr_7, attribs))
                attribs_7 = ' '.join(attribs_7_lst)
                input_ir = input_ir.replace(match_obj.group(0), f"attributes #{n_attrib} = {{ {attribs_7} }}")

            n_attribs = input_ir.count('attributes')

            for func_name in func_names_lst:
                # Insert reference to attribute for indicating this is a kernel to the backend
                #insert_attr = re.search(f'define \w+ @_Z7QM\w+{func_name}\([^\)]+\)', input_ir, re.I).end()
                # TEMPORARY PATCH FOR TEST FILE FOR PRAGMA HLS UNROLL; function name doesn't start with _Z7QM; also it ends with _
                # TODO: Hack to work around attributes with parentheses inside function signatures. Instead, we should extract the signature with a FSM
                input_ir = re.sub("dereferenceable\((\d+)\)", "dereferenceable_\g<1>", input_ir)
                insert_attr = re.search(f'define \w+ @\w*{func_name}\w*\([^\)]*\)', input_ir, re.I).end()
                input_ir = input_ir[:insert_attr] + f' #{n_attribs} ' + input_ir[insert_attr:]
                input_ir = re.sub("dereferenceable_(\d+)", "dereferenceable(\g<1>)", input_ir)

                # Appending the attribute for indicating this is a kernel
                input_ir += "\n" + self.get_attribute(func_name, n_attribs)
                n_attribs += 1

            f_out.write(input_ir)

            f_in.close() 
            f_out.close()

    # Convert the declarations of incompatible malloc and free to the LLVM-V7 compatible format.
    def convert_llvm_declaration(self, input_ir):
        """
        This method is used to convert the declarations of incompatible malloc and free in the input IR (Intermediate Representation) to the format that is compatible with LLVM-V7.

        Args:
            input_ir (str): The input Intermediate Representation string which may contain incompatible declarations.

        Returns:
            str: The modified Intermediate Representation string with converted declarations.
        """
        llvm_ir_cleaned = re.sub(r', i64 1', '', input_ir)
        llvm_ir_cleaned = re.sub(r'\scontract\s', ' ', llvm_ir_cleaned)
        llvm_ir_cleaned = re.sub(r'getelementptr inbounds \(([^)]+)\)(\w+)\)', 
                                 r'getelementptr inbounds (\1), i64 \2)', llvm_ir_cleaned)
        llvm_ir_cleaned = re.sub(r'attributes\s+#0\s+=\s+\{\s*\}', 'attributes #0 = { nounwind }', llvm_ir_cleaned)
        llvm_ir_cleaned = re.sub(r'(\{[^}]*?)\b(speculatable)\b([^}]*\})', r'\1\3',llvm_ir_cleaned)
        # llvm_ir_cleaned = re.sub( r'getelementptr inbounds \(([^)]+)\),\s+i64\s+(\w+)\)',
        #                           r'getelementptr inbounds (\1), i32 \2)',llvm_ir_cleaned)
        # # Replace the incompatible malloc declaration
        # # Replace the declaration of 'declare ptr @_Z7malloc(i64)' with 'declare i8* @malloc(i64)'
        # modified_ir = re.sub(r"declare\s+ptr\s+@_Z7malloc\(i64\)", "declare i8* @malloc(i64)", input_ir)

        # # Modify the free declaration
        # # Replace the declaration of 'declare void @_Z7free(ptr)' with 'declare void @free(i8*)'
        # modified_ir = re.sub(r'declare\s+void\s+@_Z7free\(ptr\)', "declare void @free(i8*)", modified_ir)

        # # Modify the 'ptr' type in the 'define' statements
        # # Replace the pattern 'define void @\\w+(ptr %(\w+))' with 'define void @\\w+(i8* %\1)'
        # modified_ir = re.sub(r'define\s+void\s+@([a-zA-Z_][a-zA-Z0-9_]*)\(ptr\s+%([a-zA-Z_][a-zA-Z0-9_]*)\)', r'define void @\1(i8* %\2)', modified_ir)
        # # Modify all the places where the 'ptr' type is used and unify it to the 'i8*' type
        # modified_ir = re.sub(r'\bptr\b', 'i32*', modified_ir)
        return llvm_ir_cleaned

    # Read the IR file and process it
    def process_ir_file(self, input_filename, output_filename):
        """
        This method reads an IR (Intermediate Representation) file specified by the input_filename,
        processes it by converting the incompatible declarations using the convert_llvm_declaration method,
        and then writes the processed output to the file specified by the output_filename.

        Args:
            input_filename (str): The name of the input IR file to be read.
            output_filename (str): The name of the output file where the processed IR will be written.
        """
        with open(input_filename, 'r') as infile:
            input_ir = infile.read()
        # Convert the malloc declaration
        output_ir = self.convert_llvm_declaration(input_ir)
        with open(output_filename, 'w') as outfile:
            outfile.write(output_ir)


    def clean_ir(self, in_filename=None, out_filename=None):
        """
        Removes useless metadata that cannot be fed to v++.
        
        :param in_filename: Path of the LLVM IR in human-readable format
        :type in_filename: str
        :param out_filename: Path of the output LLVM IR in human-readable format without useless metadata
        :type out_filename: str
        """
        # Remove useless metadata
        if self.none_files(in_filename, out_filename):
            raise NoneFilename
        else:
            f_in = open(in_filename)
            f_out = open(out_filename, "w")
            input_ir = f_in.read()

            input_ir = re.sub("!llvm.module.flags = !{[^}]+}", "", input_ir)

            f_out.write(input_ir)

            f_in.close() 
            f_out.close()


    def mangle_functions(self, in_filename=None, out_filename=None):
        """
        Mangles function names in the format expected by v++. It seems the backend is content as long as all the mangled names begin with @_Z7. Note that
        F18 mangles names beginning with @_QM.

        :param in_filename: Path of the input LLVM IR in human-readable format
        :type in_filename: str
        :param out_filename: Path of the output LLVM IR in human-readable format with mangled functions in the v++ format.
        :type out_filename: str
        """
        # For the backend to accept the functions it is enough for them to have the prefix @_Z7. Mangle both definitions and calls
        if self.none_files(in_filename, out_filename):
            raise NoneFilename
        else:
            f_in = open(in_filename)
            f_out = open(out_filename, "w")

            input_ir = f_in.read()
            #input_ir = re.sub("@_QM", "@_Z7QM", input_ir)
            input_ir = re.sub("define (\w+) @", "define \g<1> @_Z7", input_ir)
            input_ir = re.sub("define internal (\w+) @", "define internal \g<1> @_Z7", input_ir)
            input_ir = re.sub("call (\w+) @", "call \g<1> @_Z7", input_ir)
            input_ir = re.sub("declare (\w+) @", "declare \g<1> @_Z7", input_ir)
            
            #added by zxw 20241223-----
            input_ir = re.sub("call (\w+) (\w+) @", "call \g<1> \g<2> @_Z7", input_ir)
            #input_ir = re.sub("call contract double @acos", "call contract double @_7Zacos", input_ir)
            # input_ir = re.sub("declare double @acos", "declare double @llvm.acos.f64", input_ir)
            # input_ir = re.sub("call contract double @acos", "call contract double @llvm.acos.f64", input_ir)
            # input_ir = re.sub("call double @atan", "call double @llvm.atan.f64", input_ir)
            # input_ir = re.sub("declare double @atan", "declare double @llvm.atan.f64", input_ir)
            #--------------------

            input_ir = re.sub("define i32 @_Z7_start_df_call\(", "define i32 @_start_df_call(", input_ir)
            input_ir = re.sub("call i32 @_Z7_start_df_call\(", "call i32 @_start_df_call(", input_ir)
            input_ir = re.sub("declare i32 @_Z7_start_df_call\(", "declare i32 @_start_df_call(", input_ir)

            input_ir = re.sub("define void @_Z7_end_df_call\(", "define void @_end_df_call(", input_ir)
            input_ir = re.sub("call void @_Z7_end_df_call\(", "call void @_end_df_call(", input_ir)
            input_ir = re.sub("declare void @_Z7_end_df_call\(", "declare void @_end_df_call(", input_ir)

            input_ir = re.sub("@_Z7llvm.directive.scope.entry\(\)", "@llvm.directive.scope.entry()", input_ir)
            input_ir = re.sub("@_Z7llvm.directive.scope.exit\(([^\)]+)\)", "@llvm.directive.scope.exit(\g<1>)", input_ir)


            # Leave Xilinx intrinsics as is
            input_ir = re.sub("define double @_Z7pow\(", "define double @pow(", input_ir)
            input_ir = re.sub("call double @_Z7pow\(", "call double @pow(", input_ir)
            input_ir = re.sub("declare double @_Z7pow\(", "declare double @pow(", input_ir)

            input_ir = re.sub("define double @_Z7llvm.maxnum.f64\(", "define double @llvm.maxnum.f64(", input_ir)
            input_ir = re.sub("call double @_Z7llvm.maxnum.f64\(", "call double @llvm.maxnum.f64(", input_ir)
            input_ir = re.sub("declare double @_Z7llvm.maxnum.f64\(", "declare double @llvm.maxnum.f64(", input_ir)

            input_ir = re.sub("define double @_Z7llvm.copysign.f64\(", "define double @llvm.copysign.f64(", input_ir)
            input_ir = re.sub("call double @_Z7llvm.copysign.f64\(", "call double @llvm.copysign.f64(", input_ir)
            input_ir = re.sub("declare double @_Z7llvm.copysign.f64\(", "declare double @llvm.copysign.f64(", input_ir)

            input_ir = re.sub("define double @_Z7llvm.fabs.f64\(", "define double @llvm.fabs.f64(", input_ir)
            input_ir = re.sub("call double @_Z7llvm.fabs.f64\(", "call double @llvm.fabs.f64(", input_ir)
            input_ir = re.sub("declare double @_Z7llvm.fabs.f64\(", "declare double @llvm.fabs.f64(", input_ir)


            input_ir = re.sub("@_Z7llvm.fpga.fifo.push", "@llvm.fpga.fifo.push", input_ir)
            input_ir = re.sub("@_Z7llvm.fpga.fifo.pop", "@llvm.fpga.fifo.pop", input_ir)
            input_ir = re.sub("@_Z7llvm.sideeffect\(", "@llvm.sideeffect(", input_ir)
            input_ir = re.sub("@_Z7llvm.fpga.set.stream.depth\(", "@llvm.fpga.set.stream.depth(", input_ir)

            input_ir = re.sub("@_Z7_maxi_(\w+)", "@_maxi_\g<1>", input_ir)


            f_out.write(input_ir)

            f_in.close()
            f_out.close()

    def extract_functions(self, in_filename=None):
        """
        Split the input LLVM IR into the different functions to be used later when linking to solve dependencies. Generates one file per function with the following naming convention:
        prepared_{mangled_function_name}.ll

        :param in_filename: path of the input LLVM IR in human-readable format
        :type in_filename: str
        """
        if self.none_file(in_filename):
            raise NoneFilename
        else:
            f_in = open(in_filename)
            # File with architecture understood by official LLVM
            f_temp = open("tmp/_arch_temp.ll", "w")
            input_ir = f_in.read()
            arch_swap_ir = re.sub('target triple = "fpga32-xilinx-none"', 'target triple = "x86_64-unknown-linux-gnu"', input_ir)
            f_temp.write(arch_swap_ir)
            f_temp.close()
            #subprocess.call(f'{self.opt} --help', shell=True)
            print(self.libextractfunctions)
            subprocess.call(f'{self.opt} -load={self.libextractfunctions} --extract_functions \
                            -enable-new-pm=0 \
                            -opaque-pointers=0 \
                            tmp/_arch_temp.ll', shell=True)
            print("llvm-opt: extract_functions--zaizai-359")
            f_in.close()

            # Prepare separate function files for compilation
            arch = 'source_filename = "FIRModule"\ntarget triple = "fpga32-xilinx-none"\n'
            decls_ir = open("tmp/decls.ll").read() # declarations of functions in other files
            globals_ir = open("tmp/globals.ll").read()
            for function in open("tmp/_functions_names.tmp").readlines():
                func_file = open("tmp/" + function.strip() + '.ll')
                inner_decls_ir = self._gen_declarations(input_ir, function) # declarations of functions in file
                function_ir = func_file.read()
                function_ir = arch + decls_ir + "\n" + inner_decls_ir +"\n"+ globals_ir + "\n" + function_ir

                f_prepared = open("tmp/prepared_" + function.strip() + ".ll", "w")
                f_prepared.write(function_ir)
                f_prepared.close()

            f_in.close()
            f_temp.close()

    def rename_ir(self, in_filename=None, out_filename=None):
        """
        v++ expects functions' arguments to be named with alphabet characters, whereas F18 generates variables named with numbers. Similarly, local function SSA variable names must 
        start running from %0. This method renames the arguments (tracking every reference in the function and renaming it as well) and shifts the local variable names to start in
        %0.

        :param in_filename: path of the input LLVM IR in human-readable format
        :type in_filename: str
        :param out_filename: path of the output LLVM IR in human-readable format with renamed args/variables
        :type out_filename: str
        """
        if self.none_files(in_filename, out_filename):
            raise NoneFilename
        else:
            f_in = open(in_filename)
            f_out = open(out_filename, "w")
            input_ir = f_in.read()

            # just the alphabet might be too short
            alphabet = 'abcdefghijklmnopqrstuvwxyz'
            alphabet += alphabet.upper()

            for function_def_str in self.extract_functions_def(input_ir):
                if re.search("define (\w+) @(\w+)\(\)", function_def_str): # function with no arguments
                    continue
                    
                signature_def_matchobj = re.search("define \w+ @\w+\([^\)]+\)", function_def_str, re.IGNORECASE)
                n_args = signature_def_matchobj.group(0).count(",") + 1

                function_def_new_str = re.sub("%[0-9]+", lambda p: "%" + (str(int(p.group(0)[1:]) - n_args) if int(p.group(0)[1:]) >= n_args else alphabet[int(p.group(0)[1:])]), function_def_str)
                input_ir = input_ir.replace(function_def_str, function_def_new_str)

            f_out.write(input_ir)

            f_in.close()
            f_out.close()

    def extract_functions_def(self, code):
        """
        Extracts the definition of all the functions in the input LLVM IR. This cannot be implemented with regex, as the terminating character for functions 
        (}) can also appear in the body of the function for structure definitions.

        :param code: input LLVM IR code in human-readable format
        :type code: str
        :return: list of function definitions
        :rtype: list[str]
        """
        search_f = re.search if all else re.finditer
        function_def_str_lst = []

        #for function_def_matchobj in re.finditer("define \w+ @\w+\([^\)]*\)\s{", code, re.I):
        for function_def_matchobj in re.finditer("define \w+ @\w+\([^\)]*\)\s(?:#\d+)?\s*{", code, re.I):
            s = function_def_matchobj.start()
            e = function_def_matchobj.end()
            bracket_count = 1

            function_def_str = function_def_matchobj.group(0)

            cursor = e
            while bracket_count > 0:
                char = code[cursor]

                if char == '{':
                    bracket_count += 1
                elif char == '}':
                    bracket_count -= 1

                function_def_str += char
                cursor += 1

            function_def_str_lst.append(function_def_str)

        return function_def_str_lst

    def _gen_declarations(self, input_ir, kernel=None):
        """
        Generates declarations for functions all the function except kernel defined in the LLVM IR file that have not been declared yet.

        :param input_ir: path of the LLVM IR file in human-readable format
        :type input_ir: str
        :param kernel: name of the kernel for which the declaration will not be generated
        :type kernel: str
        :return: LLVM IR with added declarations
        :rtype: str
        """
        declarations = []
        for func_match_obj in re.finditer("define (\w+) (@[^\(]+)", input_ir):
            if not re.search(f"declare \w+ {func_match_obj.group(2)}", input_ir):
                ret_type = func_match_obj.group(1)
                f_name = func_match_obj.group(2)
                signature_line = re.search(f"define {ret_type} {f_name}[^\)]+\)", input_ir).group(0)
                signature_line = signature_line.replace("define", "declare")
                #Leave just argument types
                signature_line = re.sub(" %\w+", "", signature_line)
                declarations.append(signature_line)

        all_declarations_str = str(reduce(lambda x, y: x + "\n" + y, declarations))
        if kernel:
            all_declarations_str = re.sub(f"declare \w+ @{kernel.strip()}\([^\)]+\)", "", all_declarations_str)
        output_ir = all_declarations_str

        return output_ir

    def gen_declarations(self, in_filename, out_filename):
        """
        Generates declarations for the all the functions in the LLVM IR file.

        :param in_filename: path of the LLVM IR file in human-readable format
        :type in_filename: str
        :param out_filename: path of the LLVM IR file in human-readable format with declarations
        :type out_filename: str
        """
        f_in = open(in_filename)
        f_out = open(out_filename, "w")
        input_ir = f_in.read()
    
        output_ir = self._gen_declarations(input_ir)

        f_out.write(output_ir)
        f_in.close()

    def downgrade(self, in_filename=None, out_filename=None):
        """
        Downgrades the input LLVM IR to be usable by v++.

        :param in_filename: path of the input LLVM IR file in human-readable format
        :type in_filename: str
        :param out_filename: path of the output LLVM IR file in human readable-format in v7.
        :type out_filename: str
        """
        if self.none_files(in_filename, out_filename):
            raise NoneFilename
        else:
            if in_filename is not None: 
                self.strip_debug(in_filename, "tmp/nodebug_temp.bc") 
            else:
                self.strip_debug(self.in_filename, "tmp/nodebug_temp.bc") 
            self._downgrade("tmp/nodebug_temp.bc", "tmp/downgraded_temp.bc")
            self.disassemble("tmp/downgraded_temp.bc", "tmp/downgraded_temp.ll")
            self.mangle_functions("tmp/downgraded_temp.ll", "tmp/mangled_temp.ll")
            self.rename_ir("tmp/mangled_temp.ll", "tmp/renamed_temp.ll")
            if out_filename is not None:
                self.clean_ir("tmp/renamed_temp.ll", out_filename)
                self.extract_functions(out_filename)
            else:
                self.clean_ir("tmp/renamed_temp.ll", self.out_filename)
                self.extract_functions(self.out_filename)

    def replace_intrinsics(self, in_filename, out_filename):
        f_in = open(in_filename)
        f_out = open(out_filename, "w")
        input_ir = f_in.read()

        output_ir = re.sub("@_Z7llvm.sqrt.f64", "@sqrt", input_ir)

        f_out.write(output_ir)
        f_in.close()
        f_out.close()
