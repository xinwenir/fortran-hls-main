/***************************************************************
    Copyright 2023 Hewlett Packard Enterprise Development LP.
****************************************************************/

#define LATENCY 7

void ppcg_calc_rrn_kernel(int x_min, int x_max, int y_min, int y_max, int halo_exchange_depth,
                          double * r, double * r_store, double * z, double * rrn) {
#pragma hls interface port=r mode=m_axi bundle=gmem1
#pragma hls interface port=r_store mode=m_axi bundle=gmem2
#pragma hls interface port=z mode=m_axi bundle=gmem3

    int x_dim = x_max - x_min;
    int y_dim = y_max - y_min;

    int r_x  = 2 * halo_exchange_depth + x_dim + 1;

    double partial_rrn[LATENCY];

    for(int i = 0; i < LATENCY; i++) {
#pragma hls unroll
        partial_rrn[i] = 0;
    }


    for(int k = 0; k <= y_dim; k++) {
#pragma hls pipeline
        for(int j = 0; j <= x_dim; j+=LATENCY) {
            for(int i = 0; i < LATENCY; i++) {
                partial_rrn[i] += (r[(k + halo_exchange_depth) * r_x + (halo_exchange_depth + j + i)] - r_store[(k + halo_exchange_depth) * r_x + (halo_exchange_depth + j + i)]) * z[(k + halo_exchange_depth) * r_x + (halo_exchange_depth + j + i)];
            }

        }
    }

    for(int i = 1; i < LATENCY; i++) {
#pragma hls unroll
        partial_rrn[0] += partial_rrn[i];
    }

    *rrn = partial_rrn[0];
}
