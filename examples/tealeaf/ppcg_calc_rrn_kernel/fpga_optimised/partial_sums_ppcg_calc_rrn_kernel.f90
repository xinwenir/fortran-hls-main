!************************************************************
! Copyright 2023 Hewlett Packard Enterprise Development LP.
!************************************************************

#define LATENCY 7

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
  INTEGER(KIND=4) :: j,k,i
  INTEGER(KIND=4), value :: x_min, x_max, y_min, y_max, halo_exchange_depth
  REAL(KIND=8) :: rrn
  REAL(KIND=8), DIMENSION(x_min-halo_exchange_depth:x_max+halo_exchange_depth,&
                          y_min-halo_exchange_depth:y_max+halo_exchange_depth) :: r, r_store, z
  real(kind=8), dimension(LATENCY) :: partial_rrn

  !$ hls interface port=r mode=m_axi bundle=gmem1
  !$ hls interface port=r_store mode=m_axi bundle=gmem2
  !$ hls interface port=z mode=m_axi bundle=gmem3

  !rrn = 0.0_8


  do i=1,LATENCY
    !$ hls unroll
    partial_rrn(i) = 0
    !$ hls end unroll
  end do

  DO k=y_min,y_max
    !$ hls pipeline
    DO j=x_min,x_max,LATENCY
      do i=1,LATENCY
          partial_rrn(i) = partial_rrn(i) + (r(j+i, k) - r_store(j+i, k))*z(j+i, k)
      end do
    ENDDO
    !$ hls end pipeline
  ENDDO

  do i=2,LATENCY
    !$ hls unroll
    partial_rrn(1) = partial_rrn(1) + partial_rrn(i)
    !$ hls end unroll
  end do

  rrn = partial_rrn(1)

END SUBROUTINE ppcg_calc_rrn_kernel
