!************************************************************
! Copyright 2023 Hewlett Packard Enterprise Development LP.
!************************************************************


#define MAX_SIZE 5000
#define MAX_SR_SIZE (3+2) * (MAX_SIZE + 2*3)
#define LATENCY 8

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

    INTEGER(KIND=4) :: j,k,i,z,t
    integer :: sr_k
    REAL(kind=8) :: pw

    real(kind=8), dimension(x_min-halo_exchange_depth:x_max+halo_exchange_depth, y_min-halo_exchange_depth:3) :: shift_p, shift_Kx, shift_Ky, shift_Di

    integer :: shift_len, w_x, w_y, last_elem
    integer :: p_center, p_up, p_down, p_left, p_right
    real(kind = 8), dimension(LATENCY) :: partial_sum
    integer :: x_dim, y_dim

    x_dim = x_max - x_min + 1
    y_dim = y_max - y_min + 1
    w_x = x_dim + 2 * halo_exchange_depth
    w_y = y_dim + 2 * halo_exchange_depth

    shift_len = (halo_exchange_depth+2) * w_x

    pw = 0.0_8

    last_elem = 1

    do k=y_min-halo_exchange_depth,y_min + 2
        write(*,*) "--------->last_k sr: ", k
        do j=x_min-halo_exchange_depth,x_max+halo_exchange_depth
            shift_p(j,k) = p(j,k)
            shift_Kx(j,k) = Kx(j,k)
            shift_Ky(j,k) = Ky(j,k)
            shift_Di(j,k) = Di(j,k)
            
            write(*,*) "p(j,k): ", p(0,0)
            !write(*,*) "shift_p(last_elem): ", shift_p(last_elem), ", last_elem: ", last_elem
            last_elem = last_elem + 1
        end do
    end do
    sr_k = k
    write(*,*) "-------->sr_k: ", sr_k

    DO k=y_min,y_max
        DO j=x_min,x_max
            write (*,*) "j: ", j, ", k: ", k
            p_center = halo_exchange_depth * w_x + halo_exchange_depth + 1
            p_up = (halo_exchange_depth - 1) * w_x + halo_exchange_depth + 1
            p_down = (halo_exchange_depth + 1) * w_x + halo_exchange_depth + 1
            p_left = halo_exchange_depth * w_x + halo_exchange_depth-1 + 1
            p_right = halo_exchange_depth * w_x + halo_exchange_depth + 1 + 1

            !write(*,*) "shift_p(p_center):", shift_p(p_center), ", p_center: ", p_center
            !write(*,*) "shift_Di(p_center):", shift_Di(p_center)

            w(j, k) = shift_Di(halo_exchange_depth+1,halo_exchange_depth+1)*shift_p(halo_exchange_depth+1,halo_exchange_depth+1)                             &
                - (shift_Ky(halo_exchange_depth+1,halo_exchange_depth+2)*shift_p(halo_exchange_depth+1,halo_exchange_depth+2) + shift_Ky(halo_exchange_depth+1,halo_exchange_depth+1)*shift_p(halo_exchange_depth+1,halo_exchange_depth))  &
                - (shift_Kx(halo_exchange_depth+2,halo_exchange_depth+1)*shift_p(halo_exchange_depth+2,halo_exchange_depth+1) + shift_Kx(halo_exchange_depth+1,halo_exchange_depth+1)*shift_p(halo_exchange_depth+1,halo_exchange_depth))
            
            do i=y_min-halo_exchange_depth,y_min + 2
                do z=x_min-halo_exchange_depth,x_max+halo_exchange_depth-1
                    shift_p(z,i) = shift_p(z,i+1)
                    shift_Kx(z,i) = shift_Kx(z,i+1)
                    shift_Ky(z,i) = shift_Ky(z,i+1)
                    shift_Di(z,i) = shift_Di(z,i+1)
                end do
                shift_p(z,x_max+halo_exchange_depth) = shift_p(z+1,x_min-halo_exchange_depth)
                shift_Kx(z,x_max+halo_exchange_depth) = shift_Kx(z+1,x_min-halo_exchange_depth)
                shift_Ky(z,x_max+halo_exchange_depth) = shift_Ky(z+1,x_min-halo_exchange_depth)
                shift_Di(z,x_max+halo_exchange_depth) = shift_Di(z+1,x_min-halo_exchange_depth)
            end do

            shift_p(y_min+2,x_max+halo_exchange_depth) = p(sr_k,j)
            shift_Kx(y_min+2,x_max+halo_exchange_depth) = Kx(sr_k,j)
            shift_Ky(y_min+2,x_max+halo_exchange_depth) = Ky(sr_k,j)
            shift_Di(y_min+2,x_max+halo_exchange_depth) = Kx(sr_k,j)
        ENDDO


        do t=1,2*halo_exchange_depth
            do i=y_min-halo_exchange_depth,y_min + 2
                do z=x_min-halo_exchange_depth,x_max+halo_exchange_depth-1
                    shift_p(z,i) = shift_p(z,i+1)
                    shift_Kx(z,i) = shift_Kx(z,i+1)
                    shift_Ky(z,i) = shift_Ky(z,i+1)
                    shift_Di(z,i) = shift_Di(z,i+1)
                end do
                shift_p(z,x_max+halo_exchange_depth) = shift_p(z+1,x_min-halo_exchange_depth)
                shift_Kx(z,x_max+halo_exchange_depth) = shift_Kx(z+1,x_min-halo_exchange_depth)
                shift_Ky(z,x_max+halo_exchange_depth) = shift_Ky(z+1,x_min-halo_exchange_depth)
                shift_Di(z,x_max+halo_exchange_depth) = shift_Di(z+1,x_min-halo_exchange_depth)
            end do

            shift_p(y_min+2,x_max+halo_exchange_depth) = 0
            shift_Kx(y_min+2,x_max+halo_exchange_depth) = 0
            shift_Ky(y_min+2,x_max+halo_exchange_depth) = 0
            shift_Di(y_min+2,x_max+halo_exchange_depth) = 0
        end do


        !sr_k = sr_k + 1

        !do z=x_min-halo_exchange_depth,x_min-1
        !    write(*,*) "z: ", z
        !    do i=1,shift_len-1
        !        shift_p(i) = shift_p(i+1)
        !        shift_Kx(i) = shift_Kx(i+1)
        !        shift_Ky(i) = shift_Ky(i+1)
        !        shift_Di(i) = shift_Di(i+1)
        !    end do
        !    shift_p(shift_len) = p(sr_k,z)
        !    write(*,*) "--shift_p(shift_len): ", shift_p(shift_len)
        !    shift_Kx(shift_len) = Kx(sr_k,z)
        !    shift_Ky(shift_len) = Ky(sr_k,z)
        !    shift_Di(shift_len) = Di(sr_k,z)
        !end do
    ENDDO

    do i = 1,LATENCY
        partial_sum(i) = 0
    end do

    do k=y_min,y_max
        do j=x_min,x_max,LATENCY
            do i=1,LATENCY 
                partial_sum(i) = partial_sum(i) + w(j,k) * p(j,k)
            end do 
        end do    
    end do

    do i=2,LATENCY
        partial_sum(0) = partial_sum(0) + partial_sum(i)
    end do

    pw = partial_sum(0)
END SUBROUTINE tea_leaf_cg_calc_w_kernel_norxy

program my_program
    real(kind=8), dimension(300) :: w, Kx, Ky, p, Di
    real(kind=8) :: pw


    do i = 0,300
        w(i+1) = 0
        Kx(i+1) = i
        Ky(i+1) = i
        p(i+1) = i
        Di(i+1) = i
    end do


    call tea_leaf_cg_calc_w_kernel_norxy(0, 15, 0, 2, 2, p, w, Kx, Ky, Di, pw)
        
    do i=1,100
        write (*,*) "w(", i, ") = ", w(i)
    end do
    write (*,*) "pw: ", pw

end program
