"""
***************************************************************
    Copyright 2023 Hewlett Packard Enterprise Development LP.
***************************************************************
"""

import argparse
import re
import os
from fortran_processor import FortranProcessor
from llvm_ir_processor import LLVMIRProcessor
import subprocess

if __name__ == "__main__":
    parser = argparse.ArgumentParser()
    parser.add_argument("filename", type=str, help="File to build")
    parser.add_argument("--kernel", type=str, default=None, required=False)
    parser.add_argument("--qualify", type=int, default=0)

    args = parser.parse_args()
    filename = args.filename
    kernel = args.kernel
    qualify = args.qualify
    
    flang_path = "/home/nx08/nx08/s2081362-2/HPE/llvm-project/build/bin/"
    llvm_path = "/home/nx08/nx08/s2081362-2/HPE/llvm-project/build/bin/"
    xilinx_llvm_path = "/home/nx08/nx08/s2081362-2/HPE/HLS/hls-build/bin/"
    llvm_passes_path = "/home/nx08/nx08/s2081362-2/HPE/fortran_hls/llvm-passes-f18/build/"

    fp = FortranProcessor(flang_path)
    llvmp = LLVMIRProcessor(llvm_path, xilinx_llvm_path, llvm_passes_path)

    # Get definitions IR - this file is necessary in TeaLeaf to compile kernels
    try:
        os.mkdir("tmp")
    except:
        pass
    fp.emit_llvm("../TeaLeaf_ref/definitions.f90", "tmp/definitions.bc", "../TeaLeaf_ref")
    llvmp.strip_debug("tmp/definitions.bc", "tmp/definitions_strip.bc")
    llvmp._downgrade("tmp/definitions_strip.bc", "tmp/definitions_downgraded.bc")
    llvmp.disassemble("tmp/definitions_downgraded.bc", "tmp/definitions_downgraded.ll")
    llvmp.mangle_functions("tmp/definitions_downgraded.ll", "tmp/definitions_mangled.ll")
    llvmp.clean_ir("tmp/definitions_mangled.ll", "tmp/definitions_clean.ll")

   ################################################################################ 

    lim_dim_filename = "tmp/" + re.sub("\.(\w+)", "_limited_dim.\g<1>", os.path.basename(filename))
    fp.limit_dimensions(filename, lim_dim_filename)
    #fp.qualify_with_value_and_emit_llvm(lim_dim_filename, "tmp/output_file.bc", "../TeaLeaf_ref")
    fp.emit_llvm(lim_dim_filename, "tmp/output_file.bc", "../TeaLeaf_ref")
    # Get names of the subroutines in the Fortran code
    fp.extract_subroutine_names(filename, "tmp/subroutine_names.out")
    f_in = open("tmp/subroutine_names.out")
    kernels = list(map(lambda x: x.strip(), f_in.readlines()))
    f_in.close()


    llvmp.disassemble("tmp/output_file.bc", "tmp/output_file.ll")
    llvmp.set_input_file("tmp/output_file.bc")
    llvmp.set_output_file("tmp/downgraded.ll")
    llvmp.downgrade()
    llvmp.qualify_kernels(kernels, "tmp/downgraded.ll", "tmp/downgraded_qualified.ll")

    subprocess.call("mv *.mod tmp/", shell=True)
