!************************************************************
! Copyright 2023 Hewlett Packard Enterprise Development LP.
!************************************************************

#define MAX_SIZE 30
#define MAX_SR_SIZE MAX_SIZE
#define LATENCY 7

SUBROUTINE cg_calc_w_kernel_norxy(x_min,             &
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
  !REAL(KIND=8), DIMENSION((x_max-x_min+1+2*halo_exchange_depth)*(y_max-y_min+1*2halo_exchange_depth)) :: w, Kx, Ky, p, Di
  REAL(KIND=8), DIMENSION((x_max-x_min+1+2*halo_exchange_depth)*(y_max-y_min+1+2*halo_exchange_depth)) :: w, Kx, Ky, p, Di

    INTEGER(KIND=4) :: j,k,i,z
    REAL(kind=8) :: pw

    real(kind=8), dimension(MAX_SR_SIZE) :: shift_p, shift_Kx, shift_Ky, shift_Di

    integer :: shift_len, w_x, w_y, last_elem
    integer :: p_center, p_up, p_down, p_left, p_right
    real(kind = 8), dimension(LATENCY) :: partial_sum
    integer :: x_dim, y_dim
    integer :: iters
!$ hls interface port=p mode=m_axi bundle=gmem1
!$ hls interface port=w mode=m_axi bundle=gmem2
!$ hls interface port=Kx mode=m_axi bundle=gmem3
!$ hls interface port=Ky mode=m_axi bundle=gmem4
!$ hls interface port=Di mode=m_axi bundle=gmem5

    x_dim = x_max - x_min + 1
    y_dim = y_max - y_min + 1
    w_x = x_dim + 2 * halo_exchange_depth
    w_y = y_dim + 2 * halo_exchange_depth


    pw = 0.0_8

    last_elem = 1

    do i=1,MAX_SR_SIZE
        shift_p(last_elem) = p(i)
        shift_Kx(last_elem) = Kx(i)
        shift_Ky(last_elem) = Ky(i)
        shift_Di(last_elem) = Di(i)
        
        last_elem = last_elem + 1
    end do

    iters = 0

    DO k=1,y_dim
        DO j=1,x_dim
            p_center = halo_exchange_depth * w_x + halo_exchange_depth + 1
            p_up = (halo_exchange_depth - 1) * w_x + halo_exchange_depth + 1
            p_down = (halo_exchange_depth + 1) * w_x + halo_exchange_depth + 1
            p_left = halo_exchange_depth * w_x + halo_exchange_depth-1 + 1
            p_right = halo_exchange_depth * w_x + halo_exchange_depth + 1 + 1

            w((k-1+halo_exchange_depth) * w_x + (j+halo_exchange_depth)) = shift_Di(p_center)*shift_p(p_center)                             &
                - (shift_Ky(p_down)*shift_p(p_down) + shift_Ky(p_center)*shift_p(p_up))  &
                - (shift_Kx(p_right)*shift_p(p_right) + shift_Kx(p_center)*shift_p(p_left))
            
            do i=1,MAX_SR_SIZE-1
                shift_p(i) = shift_p(i+1)
                shift_Kx(i) = shift_Kx(i+1)
                shift_Ky(i) = shift_Ky(i+1)
                shift_Di(i) = shift_Di(i+1)
            end do

            shift_p(MAX_SR_SIZE) = p(last_elem)
            shift_Kx(MAX_SR_SIZE) = Kx(last_elem)
            shift_Ky(MAX_SR_SIZE) = Ky(last_elem)
            shift_Di(MAX_SR_SIZE) = Di(last_elem)

            last_elem = last_elem + 1

            iters = iters + 1
        ENDDO

        do z=1,2*halo_exchange_depth
            do i=1,MAX_SR_SIZE-1
                shift_p(i) = shift_p(i+1)
                shift_Kx(i) = shift_Kx(i+1)
                shift_Ky(i) = shift_Ky(i+1)
                shift_Di(i) = shift_Di(i+1)
            end do

            shift_p(MAX_SR_SIZE) = p(last_elem)
            shift_Kx(MAX_SR_SIZE) = Kx(last_elem)
            shift_Ky(MAX_SR_SIZE) = Ky(last_elem)
            shift_Di(MAX_SR_SIZE) = Di(last_elem)
            last_elem = last_elem + 1
        end do
    ENDDO

    do i = 1,LATENCY
        partial_sum(i) = 0
    end do

    do k=1,y_dim
        !$ hls pipeline
        do j=1,x_dim,LATENCY
            do i=0,LATENCY-1 
                write (*,*) "w: ", w((k-1+halo_exchange_depth) * w_x + (j+halo_exchange_depth+i))
                partial_sum(i+1) = partial_sum(i+1) + w((k-1+halo_exchange_depth) * w_x + (j+halo_exchange_depth+i)) * p((k-1+halo_exchange_depth) * w_x + (j+halo_exchange_depth+i))
            end do 
        end do    
        !$ end hls pipeline
    end do

    do i=2,LATENCY
        !$ hls unroll
        partial_sum(1) = partial_sum(1) + partial_sum(i)
        !$ hls end unroll
    end do

    write (*,*) "iters: ", iters

    pw = partial_sum(1)
END SUBROUTINE cg_calc_w_kernel_norxy

program my_program
    real(kind=8), dimension(99999) :: w, Kx, Ky, p, Di
    real(kind=8) :: pw


    do i = 0,99999
        w(i+1) = 0
        Kx(i+1) = i
        Ky(i+1) = i
        p(i+1) = i
        Di(i+1) = i
    end do


    call cg_calc_w_kernel_norxy(0, 6, 0, 20, 1, p, w, Kx, Ky, Di, pw)
        
    do i=1,100
        write (*,*) "w(", i, ") = ", w(i)
    end do
    write (*,*) "pw: ", pw

end program
