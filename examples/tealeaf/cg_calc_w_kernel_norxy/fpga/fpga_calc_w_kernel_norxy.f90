!************************************************************
! Copyright 2023 Hewlett Packard Enterprise Development LP.
!************************************************************

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
  REAL(KIND=8), DIMENSION(x_min-halo_exchange_depth:x_max+halo_exchange_depth,y_min-halo_exchange_depth:y_max+halo_exchange_depth)&
                          :: w, Kx, Ky, p, Di

    INTEGER(KIND=4) :: j,k
    REAL(kind=8) :: pw

  pw = 0.0_8

!$OMP PARALLEL REDUCTION(+:pw)
!$OMP DO
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
!$OMP END DO NOWAIT
!$OMP END PARALLEL

END SUBROUTINE cg_calc_w_kernel_norxy
