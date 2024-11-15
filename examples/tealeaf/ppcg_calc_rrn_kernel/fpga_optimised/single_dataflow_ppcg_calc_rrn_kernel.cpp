/***************************************************************
    Copyright 2023 Hewlett Packard Enterprise Development LP.
****************************************************************/

#include "single_dataflow_header.hpp"
#include <hls_stream.h>
#include <stdio.h>

void load(int x_min, int x_max, int y_min, int y_max, int halo_exchange_depth, double * r, double * r_store, double * z, hls::stream<double> & out_r, hls::stream<double> & out_r_store, hls::stream<double> & out_z);
void reduce(int x_min, int x_max, int y_min, int y_max, int halo_exchange_depth, hls::stream<double> & in_r, hls::stream<double> & in_r_store, hls::stream<double> & in_z, double * rrn);

extern "C" {
void ppcg_calc_rrn_kernel(int x_min, int x_max, int y_min, int y_max, int halo_exchange_depth,
                          double * r, double * r_store, double * z, double * rrn) {
#pragma hls interface port=r mode=m_axi depth=300 bundle=gmem1
#pragma hls interface port=r_store mode=m_axi depth=300 bundle=gmem2
#pragma hls interface port=z mode=m_axi depth=300 bundle=gmem3

    hls::stream<double> fifo_r;
    hls::stream<double> fifo_r_store;
    hls::stream<double> fifo_z;


#pragma hls dataflow
    load(x_min, x_max, y_min, y_max, halo_exchange_depth, r, r_store, z, fifo_r, fifo_r_store, fifo_z);
    reduce(x_min, x_max, y_min, y_max, halo_exchange_depth, fifo_r, fifo_r_store, fifo_z, rrn);
}
}

void load(int x_min, int x_max, int y_min, int y_max, int halo_exchange_depth, double * r, double * r_store, double * z, hls::stream<double> & out_r, hls::stream<double> & out_r_store, hls::stream<double> & out_z) {
    double r_cell;
    double r_store_cell;
    double z_cell;

    int x_dim = x_max - x_min;
    int y_dim = y_max - y_min;
    int r_x = x_dim + (2*halo_exchange_depth) + 1;


    for(int k = 0; k <= y_dim; k++) {
#pragma hls pipeline
        for(int j = 0; j <= x_dim; j++) {
            r_cell = r[(k + halo_exchange_depth) * r_x + (halo_exchange_depth + j)];
            r_store_cell = r_store[(k + halo_exchange_depth) * r_x + (halo_exchange_depth + j)];
            z_cell = z[(k + halo_exchange_depth) * r_x + (halo_exchange_depth + j)];

            out_r.write(r_cell);
            out_r_store.write(r_store_cell);
            out_z.write(z_cell);
        }
    }
}

void reduce(int x_min, int x_max, int y_min, int y_max, int halo_exchange_depth, hls::stream<double> & in_r, hls::stream<double> & in_r_store, hls::stream<double> & in_z, double * rrn) {
    double partial_rrn[LATENCY];

    for(int i = 0; i < LATENCY; i++) {
#pragma hls unroll
        partial_rrn[i] = 0;
    }

    double r_cell;
    double r_store_cell;
    double z_cell;

    int x_dim = x_max - x_min;
    int y_dim = y_max - y_min;


    for(int k = 0; k <= y_dim; k++) {
#pragma hls pipeline
        for(int j = 0; j <= x_dim; j+=LATENCY) {
            for(int i = 0; i < LATENCY; i++) {
                r_cell = in_r.read();
                r_store_cell = in_r_store.read();
                z_cell = in_z.read();

                partial_rrn[i] += (r_cell - r_store_cell) * z_cell;
            }

        }
    }


    for(int i = 1; i < LATENCY; i++) {
#pragma hls unroll
        partial_rrn[0] += partial_rrn[i];
    }
    

    *rrn = partial_rrn[0];
}
