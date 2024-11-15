/***************************************************************
    Copyright 2023 Hewlett Packard Enterprise Development LP.
****************************************************************/

#define MAX_SIZE 40
#define MAX_SR_SIZE MAX_SIZE
#define LATENCY 7

#include <stdlib.h>
#include <string.h>
#include <omp.h>
#include <stdio.h>

extern "C" {
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
#pragma hls interface mode=m_axi port=p bundle=gmem1// depth=300
#pragma hls interface mode=m_axi port=w bundle=gmem2// depth=300
#pragma hls interface mode=m_axi port=Kx bundle=gmem3// depth=300
#pragma hls interface mode=m_axi port=Ky bundle=gmem4// depth=300
#pragma hls interface mode=m_axi port=Di bundle=gmem5// depth=300


        int x_dim = x_max - x_min;
        int y_dim = y_max - y_min;

        int w_x = x_max - x_min + 2 * halo_exchange_depth + 1;

        int shift_len = (halo_exchange_depth+2) * w_x;

        double shift_p[MAX_SR_SIZE];
        double shift_Kx[MAX_SR_SIZE];
        double shift_Ky[MAX_SR_SIZE];
        double shift_Di[MAX_SR_SIZE];

        int last_elem = 0;

        for(int i = 0; i < MAX_SR_SIZE; i++) {
            #pragma hls pipeline
            shift_p[i] = p[i];
            shift_Kx[i] = Kx[i];
            shift_Ky[i] = Ky[i];
            shift_Di[i] = Di[i];
            last_elem++;
        }


        int p_center = halo_exchange_depth * w_x + halo_exchange_depth;
        int p_up = (halo_exchange_depth - 1) * w_x + halo_exchange_depth;
        int p_down = (halo_exchange_depth + 1) * w_x + halo_exchange_depth;
        int p_left = halo_exchange_depth * w_x + halo_exchange_depth-1;
        int p_right = halo_exchange_depth * w_x + halo_exchange_depth + 1;

        double wreg = 0;

        for(int k = 0; k <= y_dim; k++) {
            for(int j = 0; j <= x_dim; j++) {
                #pragma hls pipeline
                w[(k + halo_exchange_depth) * w_x + j + halo_exchange_depth] = shift_Di[p_center] * shift_p[p_center]
                    - (shift_Ky[p_down] * shift_p[p_down] + shift_Ky[p_center] * shift_p[p_up])
                    - (shift_Kx[p_right] * shift_p[p_right] + shift_Kx[p_center] * shift_p[p_left]);


                for(int i = 0; i < MAX_SR_SIZE-1; i++) {
                    shift_p[i] = shift_p[i+1];
                    shift_Kx[i] = shift_Kx[i+1];
                    shift_Ky[i] = shift_Ky[i+1];
                    shift_Di[i] = shift_Di[i+1];
                }
                shift_p[MAX_SIZE-1] = p[last_elem];
                shift_Kx[MAX_SIZE-1] = Kx[last_elem];
                shift_Ky[MAX_SIZE-1] = Ky[last_elem];
                shift_Di[MAX_SIZE-1] = Di[last_elem];

                last_elem++;
            }

            for(int z = 0; z < 2 * halo_exchange_depth; z++) {
#pragma hls pipeline
                for(int i = 0; i < MAX_SR_SIZE-1; i++) {
                    shift_p[i] = shift_p[i+1];
                    shift_Kx[i] = shift_Kx[i+1];
                    shift_Ky[i] = shift_Ky[i+1];
                    shift_Di[i] = shift_Di[i+1];
                }

                shift_p[MAX_SR_SIZE-1] = p[last_elem];
                shift_Kx[MAX_SR_SIZE-1] = Kx[last_elem];
                shift_Ky[MAX_SR_SIZE-1] = Ky[last_elem];
                shift_Di[MAX_SR_SIZE-1] = Di[last_elem];

                last_elem++;
            }
        }

        
        double partial_sum[LATENCY];

        for(int i = 0; i < LATENCY; i++)
            partial_sum[i] = 0;

        for(int k = 0; k <= y_dim; k++) {
            #pragma hls pipeline
            for(int j = 0; j <= x_dim; j+=LATENCY) {
                for(int i = 0; i < LATENCY; i++) {
                    partial_sum[i] = partial_sum[i] + w[(k + halo_exchange_depth) * w_x + j + halo_exchange_depth + i] * p[(k + halo_exchange_depth) * w_x + j + halo_exchange_depth + i];
                }
            }
        }

        for(int i = 1; i < LATENCY; i++) {
            #pragma hls unroll
            partial_sum[0] += partial_sum[i];
        }

        *pw = partial_sum[0];
    }
}

int main(int argc, char * argv[]) {
    int size = atoi(argv[1]);

    double * w = (double*)malloc(10 * size * sizeof(double));
    double * Kx = (double*)malloc(10 * size * sizeof(double));
    double * Ky = (double*)malloc(10 * size * sizeof(double));
    double * p = (double*)malloc(10 * size * sizeof(double));
    double * Di = (double*)malloc(10 * size * sizeof(double));

    double pw;
    double start_time, total_time;


    for(int i = 0; i < 10 * size; i++) {
        w[i] = 0;
        Kx[i] = i;
        Ky[i] = i;
        p[i] = i;
        Di[i] = i;
    }

    start_time = omp_get_wtime();
    cg_calc_w_kernel_norxy(0, 6, 0, size, 1, p, w, Kx, Ky, Di, &pw);
    total_time = omp_get_wtime() - start_time;


    printf("VERSION: c_shift\n");
    printf("IMPLEMENTATION: C\n");
    printf("pw: %lf\n", pw);
    printf("Execution time: %lf\n", total_time);

    return 0;
}
