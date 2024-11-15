!************************************************************
! Copyright 2023 Hewlett Packard Enterprise Development LP.
!************************************************************

include 'hls_stream.h'

#define LATENCY 7

proto_hls_stream(real)

SUBROUTINE ppcg_calc_rrn_kernel(x_min, &
                                         x_max, &
                                         y_min, &
                                         y_max, &
                                         halo_exchange_depth, &
                                         r, &
                                         r_store, &
                                         z, &
                                         rrn )

  use hls_stream
  IMPLICIT NONE

  interface
    subroutine load(x_min, x_max, y_min, y_max, halo_exchange_depth, r, r_store, z, out_r, out_r_store, out_z)
        use hls_stream
      IMPLICIT NONE
      INTEGER(KIND=4) :: j,k,i
      INTEGER(KIND=4), value :: x_min, x_max, y_min, y_max, halo_exchange_depth
      REAL(KIND=8), DIMENSION(x_min-halo_exchange_depth:x_max+halo_exchange_depth,&
                              y_min-halo_exchange_depth:y_max+halo_exchange_depth) :: r, r_store, z
      type(HLSStream_real) :: out_r, out_r_store, out_z
    end subroutine

    subroutine reduce(x_min, x_max, y_min, y_max, halo_exchange_depth, in_r, in_r_store, in_z, rrn)
        use hls_stream

        implicit none
      INTEGER(KIND=4), value :: x_min, x_max, y_min, y_max, halo_exchange_depth
      type(HLSStream_real) :: in_r, in_r_store, in_z

      real(kind=8) :: r_cell, r_store_cell, z_cell
      real(kind=8), dimension(LATENCY) :: partial_rrn
      real(kind=8) :: rrn
    end subroutine

  end interface

  INTEGER(KIND=4) :: j,k,i
  INTEGER(KIND=4), value :: x_min, x_max, y_min, y_max, halo_exchange_depth
  REAL(KIND=8) :: rrn
  REAL(KIND=8), DIMENSION(x_min-halo_exchange_depth:x_max+halo_exchange_depth,&
                          y_min-halo_exchange_depth:y_max+halo_exchange_depth) :: r, r_store, z
  type(HLSStream_real) :: fifo_r, fifo_r_store, fifo_z

  !$ hls interface port=r mode=m_axi bundle=gmem1
  !$ hls interface port=r_store mode=m_axi bundle=gmem2
  !$ hls interface port=z mode=m_axi bundle=gmem3

  call set_depth_real(fifo_r%data_real, 0)
  call set_depth_real(fifo_r_store%data_real, 0)
  call set_depth_real(fifo_z%data_real, 0)

  !$ hls dataflow
  call load(x_min, x_max, y_min, y_max, halo_exchange_depth, r, r_store, z, fifo_r, fifo_r_store, fifo_z)
  call reduce(x_min, x_max, y_min, y_max, halo_exchange_depth, fifo_r, fifo_r_store, fifo_z, rrn)
  !$ hls end dataflow

END SUBROUTINE ppcg_calc_rrn_kernel


subroutine load(x_min, x_max, y_min, y_max, halo_exchange_depth, r, r_store, z, out_r, out_r_store, out_z)
    use hls_stream
  IMPLICIT NONE
  INTEGER(KIND=4) :: j,k,i
  INTEGER(KIND=4), value :: x_min, x_max, y_min, y_max, halo_exchange_depth
  REAL(KIND=8), DIMENSION(x_min-halo_exchange_depth:x_max+halo_exchange_depth,&
                          y_min-halo_exchange_depth:y_max+halo_exchange_depth) :: r, r_store, z
  type(HLSStream_real) :: out_r, out_r_store, out_z

  real(kind=8) :: r_cell, r_store_cell, z_cell

  set_hls_stream_type(out_r, real)
  set_hls_stream_type(out_r_store, real)
  set_hls_stream_type(out_z, real)

    do k=y_min,y_max
    !$ hls pipeline
        do j=x_min,x_max
            r_cell = r(j,k)
            r_store_cell = r_store(j,k)
            z_cell = z(j,k)

            call hls_write(r_cell, out_r)
            call hls_write(r_store_cell, out_r_store)
            call hls_write(z_cell, out_z)
        end do
    !$ hls end pipeline
    end do
end subroutine


subroutine reduce(x_min, x_max, y_min, y_max, halo_exchange_depth, in_r, in_r_store, in_z, rrn)
    use hls_stream

    implicit none
  INTEGER(KIND=4), value :: x_min, x_max, y_min, y_max, halo_exchange_depth
  type(HLSStream_real) :: in_r, in_r_store, in_z

  real(kind=8) :: r_cell, r_store_cell, z_cell
  real(kind=8), dimension(LATENCY) :: partial_rrn
  real(kind=8) :: rrn
  integer :: i,j,k

  set_hls_stream_type(in_r, real)
  set_hls_stream_type(in_r_store, real)
  set_hls_stream_type(in_z, real)

    do i=1,LATENCY
    !$ hls unroll
        partial_rrn(i) = 0
    !$ hls end unroll
    end do

    do k=y_min,y_max
    !$ hls pipeline
        do j=x_min,x_max,LATENCY
            do i=1,LATENCY
                r_cell = hls_read(in_r)
                r_store_cell = hls_read(in_r_store)
                z_cell = hls_read(in_z)

                partial_rrn(i) = partial_rrn(i) + (r_cell - r_store_cell) * z_cell
            end do
        end do
    !$ hls end pipeline
    end do

    do i=2,LATENCY
    !$ hls unroll
        partial_rrn(1) = partial_rrn(1) + partial_rrn(i)
    !$ hls end unroll
    end do

    rrn = partial_rrn(1)
end subroutine
