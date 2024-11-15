#***************************************************************
#   Copyright 2023 Hewlett Packard Enterprise Development LP.
#***************************************************************

$vbuild/bin/clang baseline_cg_c_w_kernel_norxy.cpp -o c_baseline_cg_c_w_kernel_norxy -fopenmp -I/usr/lib/gcc/x86_64-redhat-linux/4.8.5/include/ -L .
$newfbuild/bin/flang-new baseline_cg_c_w_kernel_norxy.f90 -o f_baseline_cg_c_w_kernel_norxy -L $newfbuild/lib -fopenmp
$vbuild/bin/clang shift_register_calc_w_kernel_norxy.cpp -o c_shift_register_calc_w_kernel_norxy -fopenmp -I/usr/lib/gcc/x86_64-redhat-linux/4.8.5/include/ -L .
$newfbuild/bin/flang-new shift_register_calc_w_kernel_norxy.f90 -o f_shift_register_calc_w_kernel_norxy -L $newfbuild/lib -fopenmp
