!***************************************************************
!   Copyright 2023 Hewlett Packard Enterprise Development LP.
!***************************************************************

! RUN: fxx %s %S/unrll_test 1 unrll_test hw_emu 0 0
! RUN: diff %S/tmp/iface.ll %S/oracles/unrll_test.ll

subroutine unrll_test(a)
    integer, dimension(100) :: a
    integer :: k

    k = 1

    do i=1,100
    !$ hls unroll
    a(k) = k + 2 
    k = k + 1

    !$ hls end unroll
    end do
end subroutine
