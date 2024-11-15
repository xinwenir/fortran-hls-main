/***************************************************************
    Copyright 2023 Hewlett Packard Enterprise Development LP.
****************************************************************/

#ifndef _FIELD_HEADER
#define _FIELD_HEADER

#define LATENCY 8

extern "C" {
void tea_leaf_cg_calc_w_kernel_norxy(int x_min,
                                     int x_max,
                                     int y_min,
                                     int y_max,
                                     int halo_exchange_depth,             
                                     double * p,
                                     double * w,
                                     double * Kx,
                                     double * Ky,
                                     double * Di);
}

#endif
