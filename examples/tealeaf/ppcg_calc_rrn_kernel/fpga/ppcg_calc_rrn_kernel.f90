!************************************************************
! Copyright 2023 Hewlett Packard Enterprise Development LP.
!************************************************************

SUBROUTINE ppcg_calc_rrn_kernel(x_min, &
                                         x_max, &
                                         y_min, &
                                         y_max, &
                                         halo_exchange_depth, &
                                         r, &
                                         r_store, &
                                         z, &
                                         rrn )

  IMPLICIT NONE
  INTEGER(KIND=4) :: j,k
  INTEGER(KIND=4) :: x_min, x_max, y_min, y_max, halo_exchange_depth
  REAL(KIND=8) :: rrn
  REAL(KIND=8), DIMENSION(x_min-halo_exchange_depth:x_max+halo_exchange_depth,&
                          y_min-halo_exchange_depth:y_max+halo_exchange_depth) :: r, r_store, z

  rrn = 0.0_8

  DO k=y_min,y_max
    DO j=x_min,x_max
      rrn = rrn + (r(j, k) - r_store(j, k))*z(j, k)
    ENDDO
  ENDDO

END SUBROUTINE ppcg_calc_rrn_kernel
