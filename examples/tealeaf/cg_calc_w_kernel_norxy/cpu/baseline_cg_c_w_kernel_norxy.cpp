/***************************************************************
    Copyright 2023 Hewlett Packard Enterprise Development LP.
****************************************************************/

#include <stdio.h>
#include <string.h>
#include <stdlib.h>
#include <omp.h>

void cg_calc_w_kernel_norxy(int x_min,
                                     int x_max,
                                     int y_min,
                                     int y_max,
                                     int halo_exchange_depth,
                                     double * p,
                                     double * w,
                                     double * Kx,
                                     double * Ky,
                                     double * Di,
                                     double * pw) {
    *pw = 0;

    int x_dim = x_max - x_min;
    int y_dim = y_max - y_min;

    int w_x = x_max - x_min + 2 * halo_exchange_depth + 1;

    for(int k = 0; k <= y_dim; k++) {
        for(int j = 0; j <= x_dim; j++) {
            w[(k + halo_exchange_depth) * w_x + (j + halo_exchange_depth)] = 
                Di[(k + halo_exchange_depth) * w_x + (j + halo_exchange_depth)] * p[(k + halo_exchange_depth) * w_x + (j + halo_exchange_depth)]
                    - (Ky[(k + halo_exchange_depth + 1) * w_x + (j + halo_exchange_depth)] * p[(k + halo_exchange_depth + 1) * w_x + (j + halo_exchange_depth)] + 
                            Ky[(k + halo_exchange_depth) * w_x + (j + halo_exchange_depth)] * p[(k + halo_exchange_depth - 1) * w_x + (j + halo_exchange_depth)])
                    - (Kx[(k + halo_exchange_depth) * w_x + (j + halo_exchange_depth + 1)] * p[(k + halo_exchange_depth) * w_x + (j + halo_exchange_depth + 1)] + 
                            Kx[(k + halo_exchange_depth) * w_x + (j + halo_exchange_depth)] * p[(k + halo_exchange_depth) * w_x + (j + halo_exchange_depth - 1)]);
        }
        for(int j = 0; j <= x_dim; j++) {
            *pw = *pw + w[(k + halo_exchange_depth) * w_x + (j + halo_exchange_depth)] * p[(k + halo_exchange_depth) * w_x + (j + halo_exchange_depth)];
        }
    }
}

int main(int argc, char * argv[]) {
    int size = atoi(argv[1]);

    double * w = (double*)malloc(15 * size * sizeof(double));
    double * Kx = (double*)malloc(15 * size * sizeof(double));
    double * Ky = (double*)malloc(15 * size * sizeof(double));
    double * p = (double*)malloc(15 * size * sizeof(double));
    double * Di = (double*)malloc(15 * size * sizeof(double));

    double pw;
    double start_time, total_time;


    for(int i = 0; i < 15 * size; i++) {
        w[i] = 0;
        Kx[i] = i;
        Ky[i] = i;
        p[i] = i;
        Di[i] = i;
    }

    start_time = omp_get_wtime();
    cg_calc_w_kernel_norxy(0, 6, 0, size, 1, p, w, Kx, Ky, Di, &pw);
    total_time = omp_get_wtime() - start_time;


    printf("VERSION: c_baseline\n");
    printf("IMPLEMENTATION: C\n");
    printf("pw: %lf\n", pw);
    printf("Execution time: %lf\n", total_time);

    return 0;
}
