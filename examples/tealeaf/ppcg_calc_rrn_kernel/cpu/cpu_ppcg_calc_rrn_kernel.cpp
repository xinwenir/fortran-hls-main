/***************************************************************
    Copyright 2023 Hewlett Packard Enterprise Development LP.
****************************************************************/

#include <stdio.h>
#include <stdlib.h>
#include <omp.h>
#include <string.h>

void ppcg_calc_rrn_kernel(int x_min, int x_max, int y_min, int y_max, int halo_exchange_depth,
                          double * r, double * r_store, double * z, double * rrn) {

    int x_dim = x_max - x_min;
    int y_dim = y_max - y_min;

    int r_x  = 2 * halo_exchange_depth + x_dim + 1;

    for(int k = 0; k <= y_dim; k++) {
        for(int j = 0; j <= x_dim; j++) {
            *rrn = *rrn + (r[(k + halo_exchange_depth) * r_x + (halo_exchange_depth + j)] - r_store[(k + halo_exchange_depth) * r_x + (halo_exchange_depth + j)]) * z[(k + halo_exchange_depth) * r_x + (halo_exchange_depth + j)];

        }
    }
}

int main(int argc, char * argv[]) {
    int size = atoi(argv[1]);

    double * r = (double*)malloc((size+1) * (size+1) * sizeof(double));
    double * r_store = (double*)malloc((size+1) * (size+1) * sizeof(double));
    double * z = (double*)malloc((size+1) * (size+1) * sizeof(double));

    double rrn;

    double start_time, total_time;


    for(int i = 0; i < (size+1) * (size+1); i++) {
        z[i] = i;
        r[i] = 2*i;
        r_store[i] = i;
    }   

    start_time = omp_get_wtime();
    ppcg_calc_rrn_kernel(0, size, 0, size, 0, r, r_store, z, &rrn);
    total_time = omp_get_wtime() - start_time;

    printf("VERSION: c_baseline\n");
    printf("IMPLEMENTATION: C\n");
    printf("rrn: %lf\n", rrn);
    printf("Execution time: %lf\n", total_time);

    return 0;
}
