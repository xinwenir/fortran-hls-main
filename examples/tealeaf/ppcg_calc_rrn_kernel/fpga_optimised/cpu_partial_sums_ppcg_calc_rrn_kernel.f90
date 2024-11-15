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
  INTEGER(KIND=4) :: x_min, x_max, y_min, y_max, halo_exchange_depth
  REAL(KIND=8) :: rrn
  REAL(KIND=8), DIMENSION(x_min-halo_exchange_depth:x_max+halo_exchange_depth,&
                          y_min-halo_exchange_depth:y_max+halo_exchange_depth) :: r, r_store, z
  real(kind=8), dimension(LATENCY) :: partial_rrn

  rrn = 0.0_8

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

program my_program
    real(kind=8), dimension(999999) :: z, r, r_store
    integer :: x_min, x_max, y_min, y_max, halo_exchange_depth
    real(kind=8) :: rrn
    integer :: r_size
    integer :: array_size

    array_size = 999999

    r_size = 699

    x_min = 0
    x_max = r_size
    y_min = 0
    y_max = r_size
    halo_exchange_depth = 0


    do i=1,r_size
        z(i) = i/array_size
        r(i) = 2*i/array_size
        r_store(i) = i/array_size
    end do

    call ppcg_calc_rrn_kernel(x_min, x_max, y_min, y_max, halo_exchange_depth, r, r_store, z, rrn)

    write (*,*) "rrn: ", rrn
end program
