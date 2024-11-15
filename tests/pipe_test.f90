!***************************************************************
!   Copyright 2023 Hewlett Packard Enterprise Development LP.
!***************************************************************

! RUN: fxx %s %S/pipe_test 1 pipe_test hw_emu 0 0
! RUN: diff %S/tmp/iface.ll %S/oracles/pipe_test.ll

subroutine pipe_test(a)
    integer, dimension(100) :: a
    integer :: k

    k = 1

    do i=1,100
    !$ hls pipeline ii=76
    a(k) = k + 2 
    k = k + 1

    !$ end pipeline
    end do
end subroutine
