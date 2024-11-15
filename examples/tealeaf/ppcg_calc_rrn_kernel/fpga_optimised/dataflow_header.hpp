/***************************************************************
    Copyright 2023 Hewlett Packard Enterprise Development LP.
****************************************************************/

#define VEC 4
#define LATENCY 4 * VEC

typedef struct
{
    double array[VEC];
} VECTOR;

extern "C" {
void ppcg_calc_rrn_kernel(int x_min, int x_max, int y_min, int y_max, int halo_exchange_depth,
                          VECTOR * r, VECTOR * r_store, VECTOR * z, double * rrn);
}
