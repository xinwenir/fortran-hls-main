!************************************************************
! Copyright 2023 Hewlett Packard Enterprise Development LP.
!************************************************************

SUBROUTINE tea_leaf_cg_calc_w_kernel_norxy(x_min,             &
                                                   x_max,             &
                                                   y_min,             &
                                                   y_max,             &
                                                   halo_exchange_depth,             &
                                                   p,                 &
                                                   w,                 &
                                                   Kx,                &
                                                   Ky,                &
                                                   Di,                &
                                                   pw                 )

  IMPLICIT NONE

  INTEGER(KIND=4):: x_min,x_max,y_min,y_max,halo_exchange_depth
  REAL(KIND=8), DIMENSION(x_min-halo_exchange_depth:x_max+halo_exchange_depth,y_min-halo_exchange_depth:y_max+halo_exchange_depth)&
                          :: w, Kx, Ky, p, Di

    INTEGER(KIND=4) :: j,k
    REAL(kind=8) :: pw

  pw = 0.0_8

!!$OMP PARALLEL REDUCTION(+:pw)
!!$OMP DO
    DO k=y_min,y_max
        DO j=x_min,x_max
            w(j, k) = Di(j,k)*p(j, k)                             &
                - (Ky(j, k+1)*p(j, k+1) + Ky(j, k)*p(j, k-1))  &
                - (Kx(j+1, k)*p(j+1, k) + Kx(j, k)*p(j-1, k))
        ENDDO
        DO j=x_min,x_max
            pw = pw + w(j, k)*p(j, k)
        ENDDO
    ENDDO
!!$OMP END DO NOWAIT
!!$OMP END PARALLEL

END SUBROUTINE tea_leaf_cg_calc_w_kernel_norxy


program my_program
    use omp_lib

    implicit none

    real(kind=8), allocatable :: w(:), Kx(:), Ky(:), p(:), Di(:)
    real(kind=8) :: pw
    integer :: i_size, i
    character(len=32) :: size_str
    real(kind=8) :: start_time, total_time

    

    call get_command_argument(1, size_str)
    read (size_str,*) i_size

    allocate(w(15 * i_size))
    allocate(Kx(15 * i_size)) 
    allocate(Ky(15 * i_size)) 
    allocate(p(15 * i_size)) 
    allocate(Di(15 * i_size)) 


    do i = 1,15 * i_size
        w(i) = 0
        Kx(i) = i-1
        Ky(i) = i-1
        p(i) = i-1
        Di(i) = i-1
    end do

    start_time = omp_get_wtime()
    
    call tea_leaf_cg_calc_w_kernel_norxy(0, 6, 0, i_size, 1, p, w, Kx, Ky, Di, pw)

    total_time = omp_get_wtime() - start_time

        
    !do i=1,100
    !    write (*,*) "w(", i, ") = ", w(i)
    !end do
    write (*,*) "VERSION: fortran_baseline"
    write (*,*) "IMPLEMENTATION: FORTRAN"
    write (*,*) "pw: ", pw
    write (*,*) "Execution time: ", total_time

end program
