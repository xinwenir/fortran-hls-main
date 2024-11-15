!***************************************************************
!   Copyright 2023 Hewlett Packard Enterprise Development LP.
!***************************************************************

! RUN: fxx %s %S/array_partition 1 array_partition hw_emu 0 0
! RUN: diff %S/tmp/iface.ll %S/array_partition.ll

subroutine array_partition(c)
    integer, dimension(123) :: c
    integer, dimension(123) :: x
    integer, dimension(123) :: y
    integer, dimension(123) :: z
    integer, dimension(123) :: k
    integer, dimension(123, 123) :: d

    !$ hls array_partition variable=x
    !$ hls array_partition variable=y type=cyclic factor=67
    !$ hls array_partition variable=z type=block factor=12
    !$ hls array_partition variable=k type=complete
    !$ hls array_partition variable=d type=block factor=22 dim=2

    do i = 1,123
        x(i) = i
        y(i) = i
        z(i) = i
        k(i) = i
        d(i,0) = i
    end do

    do i=1,123
        c(i) = x(i) + y(i) + z(i) + k(i) + d(i,0)
    end do

end subroutine
