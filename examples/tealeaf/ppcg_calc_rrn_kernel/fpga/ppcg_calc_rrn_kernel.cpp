/***************************************************************
    Copyright 2023 Hewlett Packard Enterprise Development LP.
****************************************************************/

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
