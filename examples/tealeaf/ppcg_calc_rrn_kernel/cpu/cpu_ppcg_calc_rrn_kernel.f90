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
        !write (*,*) "r: ", r(j,k)
        !write (*,*) "r_store: ", r_store(j,k)
        !write (*,*) "z: ", z(j,k)
      rrn = rrn + (r(j, k) - r_store(j, k))*z(j, k)
    ENDDO
  ENDDO

END SUBROUTINE ppcg_calc_rrn_kernel

program my_program
    use omp_lib

    implicit none

    real(kind=8), allocatable :: z(:), r(:), r_store(:)
    real(kind=8) :: rrn
    integer :: i_size, i
    character(len=32) :: size_str
    real(kind=8) :: start_time, total_time

    

    call get_command_argument(1, size_str)
    read (size_str,*) i_size

    allocate(z((i_size+1) * (i_size+1)))
    allocate(r((i_size+1) * (i_size+1)))
    allocate(r_store((i_size+1) * (i_size+1)))


    do i = 1,(i_size+1) * (i_size+1)
        z(i) = i-1
        r(i) = 2*(i-1)
        r_store(i) = i-1
    end do

    start_time = omp_get_wtime()
    
    call ppcg_calc_rrn_kernel(0, i_size, 0, i_size, 0, r, r_store, z, rrn)

    total_time = omp_get_wtime() - start_time

        
    !do i=1,100
    !    write (*,*) "w(", i, ") = ", w(i)
    !end do
    write (*,*) "VERSION: fortran_baseline"
    write (*,*) "IMPLEMENTATION: FORTRAN"
    write (*,*) "rrn: ", rrn
    write (*,*) "Execution time: ", total_time

end program
