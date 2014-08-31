    integer*4 function parse_line(inline,prm,val,i1,i2,i3,i4)

    ! parse character string inline to get parameter name, value, and 4 optional integer flags

    implicit none

    character*(*), intent(in) :: inline             ! line to parse
    character*6, intent(out) :: prm                 ! parameter name
    real*8, intent(out) :: val                      ! value
    integer*4, intent(out) :: i1,i2,i3,i4           ! modifiers

    integer*4 i,m,n,ios,is(5),ie(5)

    ios = 0
    prm = ' '
    val = 0.D0
    i1 = 0
    i2 = 0
    i3 = 0
    i4 = 0

    ! get the length of the line, stripped of any comments

    i = 1
    m = len_trim(inline)
    do while(i <= m)
        if(inline(i:i) == '!') exit
        i = i + 1
    end do
    m = len_trim(inline(1:i-1))
    if(m == 0) goto 100

    ! now get parameter name. length max = 6 chars

    i = 1
    n = min(6,m)
    do while(i <= n)
        if(inline(i:i) == ' ') exit
        i = i + 1
    end do
    n = i - 1

    prm = inline(1:n)

    ! if prm = 'GO' or rest of line blank, don't parse

    if(prm == 'GO    ') goto 100
    if(n >= m)          goto 100

    call parse_string
    read(inline(is(1):ie(1)),*,iostat=ios,err=100,end=100) val
    if(is(2) <= m) read(inline(is(2):ie(2)),*,iostat=ios,err=100,end=100) i1
    if(is(3) <= m) read(inline(is(3):ie(3)),*,iostat=ios,err=100,end=100) i2
    if(is(4) <= m) read(inline(is(4):ie(4)),*,iostat=ios,err=100,end=100) i3
    if(is(5) <= m) read(inline(is(5):ie(5)),*,iostat=ios) i4

100 parse_line = ios

    contains

    subroutine parse_string

    implicit none

    character*1, parameter :: sep = ' '             ! parameter separator

    integer*4 i,ix

    ! strip any leading seps

    ix = n + 1
    do while(ix <= m)
        if(inline(ix:ix) /= sep) exit
        ix = ix + 1
    end do

    do i = 1,5
        do while(ix <= m)
            if(inline(ix:ix) /= sep) exit
            ix = ix + 1
        end do
        is(i) = ix
        do while(ix <= m)
            if(inline(ix:ix) == sep) exit
            ix = ix + 1
        end do
        ie(i) = ix - 1
    end do

    return
    end subroutine parse_string

    end function parse_line
