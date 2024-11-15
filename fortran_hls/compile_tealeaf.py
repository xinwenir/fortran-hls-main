"""
***************************************************************
    Copyright 2023 Hewlett Packard Enterprise Development LP.
***************************************************************
"""

import subprocess
from functools import reduce
import os
import sys


def compile(kernels, module, dependencies, xilinx_llvm_path):
    llvm_link = xilinx_llvm_path + "/llvm-link"
    llvm_dis = xilinx_llvm_path + "/llvm-dis"
    llvm_as = xilinx_llvm_path + "/llvm-as"
    dependencies_str = reduce(lambda x,y: x + " " + f"tmp/prepared_{y}.ll", [""] + dependencies)

    print(dependencies_str)

    for kernel in kernels:
        err = subprocess.call(f"{llvm_link} --internalize tmp/downgraded_qualified_nointr.ll tmp/definitions_clean.ll {dependencies_str} -o tmp/prepared_{kernel}.bc", shell=True)
        if err != 0:
            print(f"{llvm_link} --internalize downgraded_qualified_nointr.ll tmp/definitions_clean.ll {dependencies_str} -o tmp/prepared_{kernel}.bc")
            print(f"LLVM LINK error: {err}")
            sys.exit(err)
        err = subprocess.call(f"{llvm_dis} tmp/prepared_{kernel}.bc", shell=True)
        if err != 0:
            print(f"{llvm_dis} prepared_{kernel}.bc")
            print(f"LLVM DIS error: {err}")
            sys.exit(err)
        err = subprocess.call(f"{llvm_as} tmp/prepared_{kernel}.ll -o tmp/prepared_{kernel}.xpirbc", shell=True)
        if err != 0:
            print(f"{llvm_as} tmp/prepared_{kernel}.ll -o tmp/prepared_{kernel}.xpirbc")
            print(f"LLVM AS error: {err}")
            sys.exit(err)
        print(f"v++ -t hw --platform xilinx_u280_xdma_201920_3 -c -k {kernel} tmp/prepared_{kernel}.xpirbc -o tmp/prepared_{kernel}.xo")
        try:
            os.mkdir("fpga_obj")
        except:
            pass
        #err = subprocess.call(f"v++ -t hw_emu --platform xilinx_u280_xdma_201920_3 -c -k {kernel} tmp/prepared_{kernel}.xpirbc -o fpga_obj/prepared_{kernel}.xo", shell=True)
        err = subprocess.call(f"v++ -t hw --platform xilinx_u280_xdma_201920_3 -c -k {kernel} tmp/prepared_{kernel}.xpirbc -o fpga_obj/prepared_{kernel}.xo", shell=True)
        if err != 0:
            print(f"v++ ERROR: {err}")
            sys.exit(err)


def synthesise(kernels):
    kernels_str = reduce(lambda x,y: x + " " + f"fpga_obj/prepared_{y}.xo", [""] + kernels)
    print(f"v++ -t hw_emu --platform xilinx_u280_xdma_201920_3 -l {kernels_str} -o fpga_bin/tea_leaf.xclbin")
    try:
        os.mkdir("fpga_bin")
    except:
        pass
    #err = subprocess.call(f"v++ -t hw_emu --platform xilinx_u280_xdma_201920_3 -l {kernels_str} -o fpga_bin/tea_leaf.xclbin", shell=True)
    #err = subprocess.call(f"v++ -t hw_emu --platform xilinx_u280_xdma_201920_3 -l {kernels_str} -o fpga_bin/field_summary_kernel.xclbin", shell=True)
    err = subprocess.call(f"v++ -t hw --platform xilinx_u280_xdma_201920_3 -l {kernels_str} -o fpga_bin/hw_tea_leaf.xclbin", shell=True)
    if err != 0:
        print(f"v++ ERROR: {err}")
        sys.exit(err)


if __name__ == "__main__":
    
    ######################################################################################################################################################
    #subprocess.call(f"python3 fortran_hls/driver.py ../TeaLeaf_ref/kernels/field_summary_kernel.f90", shell=True)
    #kernels = ["field_summary_kernel"]
    #module = "field_summary_kernel_module"
    #dependencies = []
    #compile(kernels, module, dependencies, "/home/nx08/nx08/s2081362-2/HPE/HLS/hls-build/bin/")

    ####################################################################################################################################################
    subprocess.call(f"python3 fortran_hls/driver.py ../TeaLeaf_ref/kernels/generate_chunk_kernel.f90", shell=True)
    kernels = ["generate_chunk_kernel"]
    module = "generate_chunk_kernel_module"
    dependencies = []
    compile(kernels, module, dependencies, "/home/nx08/nx08/s2081362-2/HPE/HLS/hls-build/bin/")

    #####################################################################################################################################################
    #subprocess.call(f"python3 fortran_hls/driver.py ../TeaLeaf_ref/kernels/initialise_chunk_kernel.f90", shell=True)
    #kernels = ["initialise_chunk_kernel"]
    #module = "initialise_chunk_kernel_module"
    #dependencies = []
    #compile(kernels, module, dependencies, "/home/nx08/nx08/s2081362-2/HPE/HLS/hls-build/bin/")

    #####################################################################################################################################################
    #subprocess.call(f"python3 fortran_hls/driver.py ../TeaLeaf_ref/kernels/set_field_kernel.f90", shell=True)
    #kernels = ["set_field_kernel"]
    #module = "set_field_kernel_module"
    #dependencies = []
    #compile(kernels, module, dependencies, "/home/nx08/nx08/s2081362-2/HPE/HLS/hls-build/bin/")

    #######################################################################################################################################################
    #subprocess.call(f"python3 fortran_hls/driver.py ../TeaLeaf_ref/kernels/tea_leaf_common_kernel.f90", shell=True)
    #kernels = ["tea_diag_solve", "tea_block_solve", "tea_leaf_calc_2norm_kernel", "tea_leaf_calc_residual_kernel", "tea_leaf_common_init_kernel",
    #        "tea_leaf_kernel_finalise", "tea_block_init", "tea_diag_init"]
    #module = "tea_leaf_common_kernel_module"
    #dependencies = []
    #
    #compile(kernels, module, dependencies, "/home/nx08/nx08/s2081362-2/HPE/HLS/hls-build/bin/")

    
    ####################################################################################################################################################
    #subprocess.call(f"python3 fortran_hls/driver.py ../TeaLeaf_ref/kernels/tea_leaf_cg_kernel.f90", shell=True)
    #kernels = ["tea_leaf_cg_init_kernel"]
    #module = "tea_leaf_cg_kernel_module"
    #dependencies = ["tea_block_solve", "tea_diag_solve"]
    ##dependencies = ["tea_block_solve"] # Same file
    #compile(kernels, module, dependencies, "/home/nx08/nx08/s2081362-2/HPE/HLS/hls-build/bin/")


    #kernels = ["cg_calc_p_kernel", "cg_calc_w_kernel", "cg_calc_w_kernel_norxy", "cg_calc_ur_kernel"]
    #module = "tea_leaf_cg_kernel_module"
    #dependencies = ["tea_block_solve"]
    #compile(kernels, module, dependencies, "/home/nx08/nx08/s2081362-2/HPE/HLS/hls-build/bin/")

    ##################################################################################################################################################
    #subprocess.call(f"python3 fortran_hls/driver.py ../TeaLeaf_ref/kernels/tea_leaf_cheby_kernel.f90", shell=True)
    #kernels = ["tea_leaf_kernel_cheby_init", "tea_leaf_kernel_cheby_iterate"]
    #module = "tea_leaf_cheby_kernel_module"
    #dependencies = ["tea_block_solve", "tea_diag_solve"]
    #compile(kernels, module, dependencies, "/home/nx08/nx08/s2081362-2/HPE/HLS/hls-build/bin/")

    ###################################################################################################################################################
    #subprocess.call(f"python3 fortran_hls/driver.py ../TeaLeaf_ref/kernels/tea_leaf_jacobi_kernel.f90", shell=True)
    #kernels = ["tea_leaf_jacobi_solve_kernel"]
    #module = "tea_leaf_jacobi_kernel_module"
    #dependencies = []
    #compile(kernels, module, dependencies, "/home/nx08/nx08/s2081362-2/HPE/HLS/hls-build/bin/")

    #####################################################################################################################################################
    #subprocess.call(f"python3 fortran_hls/driver.py ../TeaLeaf_ref/kernels/tea_leaf_ppcg_kernel.f90", shell=True)
    ##kernels = ["kernel_ppcg_init_sd", "ppcg_calc_zrnorm_kernel", "ppcg_update_z_kernel", "ppcg_pupdate_kernel", "ppcg_store_r_kernel", "ppcg_calc_rrn_kernel"]
    #kernels = ["ppcg_inner_norxy"]
    #module = "tea_leaf_ppcg_kernel_module"
    #dependencies = ["tea_block_solve"]
    #compile(kernels, module, dependencies, "/home/nx08/nx08/s2081362-2/HPE/HLS/hls-build/bin/")

    #kernels = ["tea_leaf_ppcg_init_kernel"]
    #module = "tea_leaf_ppcg_kernel_module"
    #dependencies = ["tea_block_solve", "tea_diag_solve"]
    #compile(kernels, module, dependencies, "/home/nx08/nx08/s2081362-2/HPE/HLS/hls-build/bin/")

    # write(*,*) had to be commented out
    #kernels = ["tea_leaf_kernel_ppcg_inner"]
    #module = "tea_leaf_ppcg_kernel_module"
    #dependencies = ["tea_block_solve"]
    #compile(kernels, module, dependencies, "/home/nx08/nx08/s2081362-2/HPE/HLS/hls-build/bin/")

    #kernels = ["ppcg_inner_norxy"]
    #module = "tea_leaf_ppcg_kernel_module"
    #dependencies = ["tea_block_solve"]
    #compile(kernels, module, dependencies, "/home/nx08/nx08/s2081362-2/HPE/HLS/hls-build/bin/")

    #####################################################################################################################################################
    #subprocess.call(f"python3 fortran_hls/driver.py ../TeaLeaf_ref/kernels/update_halo_kernel.f90", shell=True)
    #kernels = ["update_halo_cell", "update_halo_kernel"]
    #module = "update_halo_kernel_module"
    #dependencies = []
    #compile(kernels, module, dependencies, "/home/nx08/nx08/s2081362-2/HPE/HLS/hls-build/bin/")

    ##################################################################################################################################################
    #subprocess.call(f"python3 fortran_hls/driver.py ../TeaLeaf_ref/kernels/update_internal_halo_kernel.f90", shell=True)
    #kernels = ["update_in_halo_lr_kernel", "update_in_halo_cell_lr", "update_in_halo_bt_kernel", "update_in_halo_cell_bt"]
    #module = "update_internal_halo_kernel_module"
    #dependencies = []
    #compile(kernels, module, dependencies, "/home/nx08/nx08/s2081362-2/HPE/HLS/hls-build/bin/")

    ##################################################################################################################################################
    
    kernels = ["field_summary_kernel"]
    kernels += ["generate_chunk_kernel"]
    kernels += ["initialise_chunk_kernel"]
    kernels += ["set_field_kernel"]
    kernels += ["tea_diag_solve", "tea_block_solve"]
    kernels += ["tea_leaf_cg_init_kernel"]
    kernels += ["cg_calc_p_kernel", "cg_calc_w_kernel", "cg_calc_w_kernel_norxy", "cg_calc_ur_kernel"] #TODO: excessive length
    kernels += ["tea_leaf_kernel_cheby_init", "tea_leaf_kernel_cheby_iterate"]
    kernels += ["tea_leaf_jacobi_solve_kernel"]
    kernels += ["kernel_ppcg_init_sd", "ppcg_calc_zrnorm_kernel", "ppcg_update_z_kernel", "ppcg_pupdate_kernel", "ppcg_store_r_kernel", "ppcg_calc_rrn_kernel"] #TODO: excessive lenght
    kernels += ["tea_leaf_ppcg_init_kernel"]
    kernels += ["tea_leaf_kernel_ppcg_inner"]
    kernels += ["ppcg_inner_norxy"]
    kernels += ["update_halo_cell"]
    kernels += ["update_halo_kernel"]
    kernels += ["update_in_halo_lr_kernel", "update_in_halo_cell_lr", "update_in_halo_bt_kernel", "update_in_halo_cell_bt"] #TODO: excessive length

    #synthesise(kernels)
