!************************************************************
! Copyright 2023 Hewlett Packard Enterprise Development LP.
!************************************************************


#define MAX_SIZE 5000
#define MAX_SR_SIZE (3+2) * (MAX_SIZE + 2*3)

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

    INTEGER(KIND=4) :: j,k,i
    REAL(kind=8) :: pw

    real(kind=8), dimension(MAX_SR_SIZE) :: shift_p, shift_Kx, shift_Ky

    integer :: shift_len, w_x, last_elem
    integer :: p_center, p_up, p_down, p_left, p_right
    real(kind = 8), dimension(LATENCY) :: partial_sum

    w_x = x_max - x_min + 2 * halo_exchange_depth + 1

    shift_len = (halo_exchange_depth+2) * w_x

    pw = 0.0_8

    do i=1,shift_len
        shift_p(i) = p(i)
        shift_Kx(i) = Kx(i)
        shift_Ky(i) = Ky(i)
        last_elem = last_elem + 1
    end do

    DO k=y_min,y_max
        DO j=x_min,x_max
            p_center = halo_exchange_depth * w_x + halo_exchange_depth;
            p_up = (halo_exchange_depth - 1) * w_x + halo_exchange_depth;
            p_down = (halo_exchange_depth + 1) * w_x + halo_exchange_depth;
            p_left = halo_exchange_depth * w_x + halo_exchange_depth-1;
            p_right = halo_exchange_depth * w_x + halo_exchange_depth + 1;

            w(j, k) = shift_Di(j,k)*shift_p(p_center)                             &
                - (shift_Ky(j, k+1)*shift_p(j, k+1) + shift_Ky(j, k)*shift_p(j, k-1))  &
                - (shift_Kx(j+1, k)*shift_p(j+1, k) + shift_Kx(j, k)*shift_p(j-1, k))
        ENDDO


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
