#***************************************************************
#   Copyright 2023 Hewlett Packard Enterprise Development LP.
#***************************************************************

$vbuild/bin/clang cpu_ppcg_calc_rrn_kernel.cpp -o c_cpu_ppcg_calc_rrn_kernel -fopenmp -I/usr/lib/gcc/x86_64-redhat-linux/4.8.5/include/ -L .
$newfbuild/bin/flang-new cpu_ppcg_calc_rrn_kernel.f90 -o f_cpu_ppcg_calc_rrn_kernel -L $newfbuild/lib -fopenmp
