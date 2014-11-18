    integer*4 function parse_line(inline,prm,val,i1,i2,i3,i4)

    ! parse character string inline to get parameter name, value, and 4 optional integer flags

    implicit none

    character*(*), intent(in) :: inline             ! line to parse
    character*6, intent(out) :: prm                 ! parameter name
    real*8, intent(out) :: val                      ! value
    integer*4, intent(out) :: i1,i2,i3,i4           ! modifiers

    integer*4 i,npm,nch,ios,is(5),ie(5)

    parse_line = 0
    prm = ' '
    val = 0.D0
    i1 = 0
    i2 = 0
    i3 = 0
    i4 = 0

    ! get the length of the line, stripped of any comments

    i = 1
    nch = len_trim(inline)
    do while(i <= nch)
        if ((inline(i:i) == '!') .or. (inline(i:i) == '*')) exit
        if ((inline(i:i) == '/') .or. (inline(i:i) == ';')) exit
        i = i + 1
    end do
    nch = len_trim(inline(1:i-1))
    if(nch == 0) return

    ! now get parameter name. length max = 6 chars

    i = 1
    npm = min(6,nch)
    do while(i <= npm)
        if(inline(i:i) == ' ') exit
        i = i + 1
    end do
    npm = i - 1

    prm = inline(1:npm)

    ! if prm = 'GO' or rest of line blank, don't parse

    if(prm == 'GO    ') return
    if(npm >= nch)      return

    ! parse string for parameters

    call parse_string

    read(inline(is(1):ie(1)),*,iostat=ios) val
    i1 = intl(2)
    i2 = intl(3)
    i3 = intl(4)
    i4 = intl(5)

    parse_line = ios   ! return final status

    return

    contains

    !---------------------------------------------------------------------

    subroutine parse_string

    implicit none

    character*1, parameter :: sep = ' '             ! parameter separator

    integer*4 i,ix

    ! parse string for location of value & optional integer parameters

    ! first strip off any leading seps

    ix = npm + 1
    do while(ix <= nch)
        if(inline(ix:ix) /= sep) exit
        ix = ix + 1
    end do

    ! look for sub-strings containing value & optional integer parameters

    do i = 1,5
        do while(ix <= nch)
            if(inline(ix:ix) /= sep) exit
            ix = ix + 1
        end do
        is(i) = ix
        do while(ix <= nch)
            if(inline(ix:ix) == sep) exit
            ix = ix + 1
        end do
        ie(i) = ix - 1
    end do

    return
    end subroutine parse_string

    !---------------------------------------------------------------------

    integer*4 function intl(nx)

    implicit none

    integer*4, intent(in) :: nx     ! substring index

    ! try to convert sub-string nx to integer

    integer*4 k,l,i1,i2
    character*4 :: fmt = '(Ix)'

    intl = 0
    if(ios /= 0) return     ! don't continue after errors

    ! try to read the sub-string

    i1 = is(nx)
    i2 = ie(nx)
    k = i2 - i1 + 1     ! length of sub-string

    select case(k)
    case(:0)
        return
    case(1:9)
        write(fmt(3:3),'(I1)') k
        read(inline(i1:i2),fmt,iostat=ios) l
    case default
        read(inline(i1:i2),*,iostat=ios) l
    end select

    if(ios /= 0) then
        ! on error, warn user of the problem
        write(*,'(a)')' Error parsing following line from input:'
        write(*,'(a)')  inline(1:nch)
        write(8,'(a,a)')' ERROR: Parsing following line from input:', inline(1:nch)
        l = 0
    endif

    intl = l

    return
    end function intl

    end function parse_line
