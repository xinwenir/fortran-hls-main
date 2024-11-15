!***************************************************************
!   Copyright 2023 Hewlett Packard Enterprise Development LP.
!***************************************************************

! RUN: fxx -I ../include %s %S/f_full_stream 1 entry hw_emu 0 0
! RUN: diff %S/tmp/iface.ll %S/oracles/f_full_stream.ll

include 'hls_stream.h'

proto_hls_stream(integer, real)

subroutine entry(a, b)
    use hls_stream

    integer, dimension(100) :: a, b
    type(HLSStream_integer) :: s1
    integer :: input
    integer :: is_full
    integer :: n_elems

    set_hls_stream_type(s1, integer)

    input = 1234
    is_full = 0 

    counter = 1
    n_elems = 0
    do while (is_full == 0)
        call hls_write(input, s1)
        !input = hls_read(s1)
        is_full = hls_full(s1)

        n_elems = n_elems + 1
    end do

    b(3) = n_elems
    b(4) = is_full

    do while (n_elems > 0)
        call consumer(s1)
        n_elems = n_elems - 1
    end do
    
    if (is_full == 1) then
        b(1) = 412341
    else 
        b(1) = 666
    end if
    b(2) = is_full
end subroutine

               
subroutine consumer(s)
    use hls_stream
    type(HLSStream_integer) :: s
    integer :: output

    set_hls_stream_type(s, integer)

    output = hls_read(s)
end subroutine
