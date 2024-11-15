!***************************************************************
!   Copyright 2023 Hewlett Packard Enterprise Development LP.
!***************************************************************

! RUN: fxx -I ../include %s %S/f_dataflow 1 entry hw_emu 0 0
! RUN: diff %S/tmp/iface.ll %S/oracles/f_dataflow.ll

subroutine entry(a, b, c)
    integer,dimension(10000) :: a,b,c
    !!$ hls interface port=a mode=m_axi bundle=gmem0
    !$ hls interface port=a mode=axis 
    !$ hls interface port=b mode=m_axi bundle=gmem1
    !$ hls interface port=c mode=m_axi bundle=gmem2

    !$ hls dataflow
    call compute0(a,b)
    call compute1(b,c)
end subroutine

subroutine compute0(a,b)
    integer,dimension(10000) :: a,b

    do i=1,10000
        b(i) = 13413 * a(i) * a(i) * a(i)
    end do    
end subroutine

subroutine compute1(b,c)
    integer,dimension(10000) :: b,c

    do i=1,10000
        c(i) = 1234123 * b(i) * b(i)
    end do
end subroutine
