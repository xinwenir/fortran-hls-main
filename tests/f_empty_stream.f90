!***************************************************************
!   Copyright 2023 Hewlett Packard Enterprise Development LP.
!***************************************************************

! RUN: fxx -I ../include %s %S/f_empty_stream 1 entry hw_emu 0 0
! RUN: diff %S/tmp/iface.ll %S/oracles/f_empty_stream.ll

include 'hls_stream.h'

proto_hls_stream(integer, real)

subroutine entry(a, b)
    use hls_stream

    integer, dimension(100) :: a, b
    type(HLSStream_integer) :: s1
    integer :: input
    integer :: is_full
    integer :: n_elems
    integer :: output

    set_hls_stream_type(s1, integer)

    input = 1234
    is_empty = 0 

    counter = 1
    n_elems = 0

    do i=1,2
        call hls_write(input, s1)
    end do

    do while (is_empty == 0)
        output = hls_read(s1)
        is_empty = hls_empty(s1)
    end do
    
    if (is_empty == 1) then
        b(1) = 412341
    else 
        b(1) = 666
    end if
    b(2) = is_empty
end subroutine
