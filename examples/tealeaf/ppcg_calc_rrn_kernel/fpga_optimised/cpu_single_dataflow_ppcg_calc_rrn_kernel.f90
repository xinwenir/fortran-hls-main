!************************************************************
! Copyright 2023 Hewlett Packard Enterprise Development LP.
!************************************************************

#define LATENCY 7

module hls_stream
    type HLSStream_real
        real(kind=8), dimension(3000) :: data_real
        integer :: current_write = 1 
        integer :: current_read = 1 
        integer :: n_elems = 0 
        integer :: empty
    end type
    contains
    subroutine hls_write(input, s)
        real(kind=8) :: input
        type(HLSStream_real) :: s
    
        integer :: i
        i = s%current_write

        s%data_real(i) = input
        s%empty = 0 
        !write (*,*) "writing: ", input, ", idx: ", s%current_write

        s%current_write = s%current_write + 1 
        s%n_elems = s%n_elems + 1 
    end subroutine

    real(kind=8) function hls_read(s)
        type(HLSStream_real) :: s

        integer :: i
        real(kind=8) :: data_read

        i = s%n_elems
        if (i == 1) then
            s%empty = 1 
        end if

        data_read = s%data_real(i)

        !write(*,*) "data read: ", data_read

        s%current_read = s%current_read + 1 
        !write(*,*) "current_data: ", data_read

        hls_read = data_read
        s%n_elems = s%n_elems - 1 
    end function

    integer function hls_empty(s)
        type(HLSStream_real) :: s

        hls_empty = s%empty
    end function
end module

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
  INTEGER(KIND=4) :: j,k,i
  INTEGER(KIND=4), value :: x_min, x_max, y_min, y_max, halo_exchange_depth
  REAL(KIND=8) :: rrn
  REAL(KIND=8), DIMENSION(x_min-halo_exchange_depth:x_max+halo_exchange_depth,&
                          y_min-halo_exchange_depth:y_max+halo_exchange_depth) :: r, r_store, z
  type(HLSStream_real) :: fifo_r, fifo_r_store, fifo_z

  !$ hls interface port=r mode=m_axi bundle=gmem1
  !$ hls interface port=r_store mode=m_axi bundle=gmem2
  !$ hls interface port=z mode=m_axi bundle=gmem3

  !call set_depth_real(fifo_r%data_real, 0)
  !call set_depth_real(fifo_r_store%data_real, 0)
  !call set_depth_real(fifo_z%data_real, 0)

  !$ hls dataflow
  call load(x_min, x_max, y_min, y_max, halo_exchange_depth, r, r_store, z, fifo_r, fifo_r_store, fifo_z)
  call reduce(x_min, x_max, y_min, y_max, halo_exchange_depth, fifo_r, fifo_r_store, fifo_z, rrn)

END SUBROUTINE ppcg_calc_rrn_kernel


subroutine load(x_min, x_max, y_min, y_max, halo_exchange_depth, r, r_store, z, out_r, out_r_store, out_z)
    use hls_stream
  IMPLICIT NONE
  INTEGER(KIND=4) :: j,k,i
  INTEGER(KIND=4) :: x_min, x_max, y_min, y_max, halo_exchange_depth
  REAL(KIND=8), DIMENSION(x_min-halo_exchange_depth:x_max+halo_exchange_depth,&
                          y_min-halo_exchange_depth:y_max+halo_exchange_depth) :: r, r_store, z
  type(HLSStream_real) :: out_r, out_r_store, out_z

  real(kind=8) :: r_cell, r_store_cell, z_cell

  !set_hls_stream_type(out_r, real)
  !set_hls_stream_type(out_r_store, real)
  !set_hls_stream_type(out_z, real)

    do k=y_min,y_max
        do j=x_min,x_max
            r_cell = r(j,k)
            write(*,*) "r_cell: ", r_cell
            r_store_cell = r_store(j,k)
            z_cell = z(j,k)

            call hls_write(r_cell, out_r)
            call hls_write(r_store_cell, out_r_store)
            call hls_write(z_cell, out_z)
        end do
    end do
end subroutine


subroutine reduce(x_min, x_max, y_min, y_max, halo_exchange_depth, in_r, in_r_store, in_z, rrn)
    use hls_stream

    implicit none
  INTEGER(KIND=4) :: x_min, x_max, y_min, y_max, halo_exchange_depth
  type(HLSStream_real) :: in_r, in_r_store, in_z

  real(kind=8) :: r_cell, r_store_cell, z_cell
  real(kind=8), dimension(LATENCY) :: partial_rrn
  real(kind=8) :: rrn
  integer :: i,j,k

  !set_hls_stream_type(in_r, real)
  !set_hls_stream_type(in_r_store, real)
  !set_hls_stream_type(in_z, real)

    do i=1,LATENCY
        partial_rrn(i) = 0
    end do

    do k=y_min,y_max
        do j=x_min,x_max,LATENCY
            do i=1,LATENCY
                r_cell = hls_read(in_r)
                r_store_cell = hls_read(in_r_store)
                z_cell = hls_read(in_z)

                partial_rrn(i) = partial_rrn(i) + (r_cell - r_store_cell) * z_cell
            end do
        end do
    end do

    do i=2,LATENCY
        partial_rrn(1) = partial_rrn(1) + partial_rrn(i)
    end do

    rrn = partial_rrn(1)
end subroutine

program my_program
    interface
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
         end subroutine

    end interface
    real(kind=8), dimension(999999) :: z, r, r_store
    integer :: x_min, x_max, y_min, y_max, halo_exchange_depth
    real(kind=8) :: rrn
    integer :: r_size
    integer :: array_size

    array_size = 999999

    r_size = 69

    x_min = 0
    x_max = r_size
    y_min = 0
    y_max = 1
    halo_exchange_depth = 0


    do i=0,999999
        z(i+1) = i
        r(i+1) = 2*i
        r_store(i+1) = i
    end do

    call ppcg_calc_rrn_kernel(x_min, x_max, y_min, y_max, halo_exchange_depth, r, r_store, z, rrn)

    write (*,*) "rrn: ", rrn
end program
