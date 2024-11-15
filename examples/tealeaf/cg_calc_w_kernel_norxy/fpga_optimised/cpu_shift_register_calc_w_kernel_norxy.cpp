/***************************************************************
    Copyright 2023 Hewlett Packard Enterprise Development LP.
****************************************************************/

#include <stdio.h>
#include "header.hpp"

#define MAX_SIZE 30
#define MAX_SR_SIZE MAX_SIZE

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


        *pw = 0;

        printf("x_min: %d, x_max: %d, y_min: %d, y_max: %d, halo_exchange_depth: %d\n", x_min, x_max, y_min, y_max, halo_exchange_depth);

        int x_dim = x_max - x_min;
        int y_dim = y_max - y_min;

        int w_x = x_max - x_min + 2 * halo_exchange_depth + 1;
        printf("w_x: %d\n", w_x);

        int shift_len = (halo_exchange_depth+2) * w_x;
        printf("shift_len: %d\n", shift_len);

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

        printf("HELLO\n");

        int p_center = halo_exchange_depth * w_x + halo_exchange_depth;
        int p_up = (halo_exchange_depth - 1) * w_x + halo_exchange_depth;
        int p_down = (halo_exchange_depth + 1) * w_x + halo_exchange_depth;
        int p_left = halo_exchange_depth * w_x + halo_exchange_depth-1;
        int p_right = halo_exchange_depth * w_x + halo_exchange_depth + 1;

        printf("p_center: %d\n", p_center);
        printf("p_up: %d\n", p_up);
        printf("p_down: %d\n", p_down);
        printf("p_left: %d\n", p_left);
        printf("p_right: %d\n", p_right);

        double wreg = 0;

        int iters = 0;

        for(int k = 0; k <= y_dim; k++) {
            for(int j = 0; j <= x_dim; j++) {
                //printf("k: %d, j: %d\n", k, j);
                //printf("index: %d\n", (k + halo_exchange_depth) * w_x + j + halo_exchange_depth);
                //printf("w_x: %d\n", w_x);
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

                iters++;
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

        printf("iters: %d\n", iters);

        
        double partial_sum[LATENCY];

        for(int i = 0; i < LATENCY; i++)
            partial_sum[i] = 0;

        for(int k = 0; k <= y_dim; k++) {
            #pragma hls pipeline
            for(int j = 0; j <= x_dim; j+=LATENCY) {
                for(int i = 0; i < LATENCY; i++) {
                    printf("w: %lf\n", w[(k + halo_exchange_depth) * w_x + j + halo_exchange_depth + i]);
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


int main() {

    int x_min = 0;
    int x_max = 6;
    int y_min = 0;
    int y_max = 20;
    int halo_exchange_depth = 1;
    int array_size = 99999;
    double p[array_size];
    double w[array_size];
    double Kx[array_size];
    double Ky[array_size];
    double Di[array_size];
    double pw;

    for(int i = 0; i < array_size; i++) {
        p[i] = i;
        Kx[i] = i;
        Ky[i] = i;
        Di[i] = i;
    }   
    for(int i = 0; i < array_size; i++)
        w[i] = 0;
    
    cg_calc_w_kernel_norxy(x_min, x_max, y_min, y_max, halo_exchange_depth,
                                    p, w, Kx, Ky, Di, &pw);

    for(int i = 0; i < 100; i++) {
        printf("w[%d] = %lf\n", i, w[i]);
    }

    printf("pw: %lf\n", pw);
}
