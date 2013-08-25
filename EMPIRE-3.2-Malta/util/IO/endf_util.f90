module endf_util

    use endf_io

    implicit none

    private

    integer*4 :: ip = 2                  ! current data index used by terpa
    integer*4 :: ir = 1                  ! current interpolation scheme index
    real*8 :: tbax                       ! little beyond end of TAB1 to let in
    type (tab1), pointer :: tb           ! TAB1 to be interpolated in terpa

    interface intrp
        module procedure terpa
        module procedure intab
    end interface

    interface operator (+)
        module procedure add_tab1
    end interface

    public operator (+)
    public intrp, genintr, set_interp, unigrd, add_tab1

    contains

    !------------------------------------------------------------------------------

    subroutine set_interp(tb1,ip1,ir1)

    implicit none

    type (tab1), intent(in), target :: tb1        ! TAB1 to be used for interpolation
    integer*4, intent(in), optional :: ip1,ir1    ! data and interpolation indices

    real*8, parameter :: shade = 1.00001D0

    tb => tb1
    tbax = shade*tb%dat(tb%np)%x

    if(present(ip1)) then
        ip = min(max(ip1,2),tb%np)
    else
        ip = 2
    endif

    if(present(ir1)) then
        ir = min(max(ir1,1),tb%nr)
    else
        ir = 1
        do while(ir < tb%nr)
            if(tb%itp(ir)%x >= ip) exit
            ir = ir + 1
        end do
    endif

    return
    end subroutine set_interp

    !------------------------------------------------------------------------------

    real*8 function terpa(x)

    implicit none

    ! Interpolate the TAB1 record pointer to by tb. Return zero if the
    ! x is outside the range of the table (within shade). Before calling this
    ! routine set_interp must be called to define the TAB1 being interpolated,
    ! and optionally set initial values for the indices ip and ir.

    real*8, intent(in) :: x               ! x value to interpolate

10  if(x < tb%dat(ip)%x) then

        if(x >= tb%dat(ip-1)%x) then
            terpa = genintr(x,tb%itp(ir)%y,tb%dat(ip-1),tb%dat(ip))
            return
        else
            if (ip > 2) then
                ! move down
                ip = ip - 1
                if(ir == 1) goto 10
                if (ip <= tb%itp(ir-1)%x) ir = ir - 1
                goto 10
            else
                terpa = 0.D0
                return
            endif
        endif

    else

        if(ip < tb%np) then
            ! move up
            ip = ip + 1
            if(ip > tb%itp(ir)%x) ir = min(ir+1, tb%nr)
            goto 10
        else
            if(x < tbax) then
                terpa = tb%dat(tb%np)%y
            else
                terpa = 0.D0
            endif
            return
        endif

    endif

    end function terpa

    !------------------------------------------------------------------------------

    real*8 function intab(e,tbl)

    implicit none

    ! interpolate cross section at value e from tab1 structure

    real*8, intent(in) :: e                   ! energy to interpolate tab1
    type (tab1), intent(in) :: tbl            ! tab1 to be interpolated

    integer*4 ie,ix

    if(e < tbl%dat(1)%x) then
        intab = 0.D0
        return
    endif

    if(e > tbl%dat(tbl%np)%x) then
        intab = 0.D0
        return
    endif

    ie = 1
    do while(ie < tbl%np)
        ie = ie + 1
        if(tbl%dat(ie)%x >= e) exit
    end do

    ix = 1
    do while(ix < tbl%nr)
        if(tbl%itp(ix)%x >= ie) exit
        ix = ix + 1
    end do

    intab = genintr(e,tbl%itp(ix)%y, tbl%dat(ie-1), tbl%dat(ie))

    return
    end function intab

    !------------------------------------------------------------------------------

    real*8 function genintr(e, ip, p1, p2)

    implicit none

    ! interpolate cross section at value e from real pairs p1 & p2
    ! using ENDF interpolation scheme ip (1-5).

    real*8, intent(in) :: e                   ! energy to interpolate at
    integer*4, intent(in) :: ip               ! interpolation flag
    type (real_pair), intent(in) :: p1, p2    ! points which are interpolated

   if((p1%x == p2%x) .or. (e == p1%x)) then
        genintr = p1%y  ! just set to y1
        return
    endif

    ! don't allow them to be inverted

    if(p1%x > p2%x) stop ' Interpolation energy E2 < E1'

    select case(ip)
    case(1)
        ! histogram
        genintr = p1%y
    case(2)
        ! linear - linear
        genintr = p1%y + (p2%y-p1%y)*(e-p1%x)/(p2%x-p1%x)
    case(3)
        ! log(x) linear
        if(p1%x <= 0.D0) stop ' log interp of negative energy'
        if(p2%x <= 0.D0) stop ' log interp of negative energy'
        if(e <= 0.D0) stop ' log interp to negative energy'
        genintr = p1%y + (p2%y-p1%y)*log(e/p1%x)/log(p2%x/p1%x)
    case(4)
        ! linear(x) log(y)
        if(p1%y <= 0.D0) stop ' log interp of negative value'
        if(p2%y <= 0.D0) stop ' log interp of negative value'
        genintr = p1%y*(p2%y/p1%y)**((e-p1%x)/(p2%x-p1%x))
    case(5)
        ! log(x) log(y)
        if(p1%x <= 0.D0) stop ' log interp of negative energy'
        if(p2%x <= 0.D0) stop ' log interp of negative energy'
        if(p1%y <= 0.D0) stop ' log interp of negative value'
        if(p2%y <= 0.D0) stop ' log interp of negative value'
        if(e <= 0.D0) stop ' log interp to negative energy'
        genintr = p1%y*(p2%y/p1%y)**(log(e/p1%x)/log(p2%x/p1%x))
    case default
        write(6,*) ' Undefined interpolation flag:',ip
        stop ' Undefined interpolation flag'
    end select

    return
    end function genintr

    !------------------------------------------------------------------------------

    subroutine unigrd(l1,en1,l2,en2,lu,eun,neu)

    implicit none

    ! Title  : Subroutine UNIGRID
    !          based on original by Andrej Trkov
    !          
    ! Purpose: Generate union grid from two sets
    ! Description:
    !   Two grids EN1 and EN2 containing L1 and L2 points respectively
    !   are merged into EUN with LU points. Special care is taken to
    !   retain double points within a grid that allow for function
    !   discontinuities.

    integer*4, intent(in) :: l1, l2, lu
    real*8, intent(in), target :: EN1(l1), EN2(l2)

    integer*4, intent(out) :: neu       ! # points of union grid
    real*8, intent(out) :: EUN(lu)       ! union grid

    logical*4 qdb,qsm,qst,qd
    integer*4 j,na,nb

    ! form pointers to the 2 1-D arrays:
    ! the 'a' array points to array with smallest value.
    ! the 'b' array points to the other array.
    ! as we walk up the arrays, this pointer will switch 
    ! back & forth between the 2 arrays en1 & en2.

    integer*4, target  :: j1, j2
    integer*4, pointer, volatile :: ja, jb
    real*8, pointer    :: ea(:), eb(:)

    ! This routine assume both arrays are monotonically
    ! increasing in energy. Check this to make sure.

    do j = 2,l1
        if(en1(j) < en1(j-1)) STOP 'UNIGRD ERROR - Array 1 not monotonically increasing'
    end do

    do j = 2,l2
        if(en2(j) < en2(j-1)) STOP 'UNIGRD ERROR - Array 2 not monotonically increasing'
    end do

    NEU = 0
    J1  = 1
    J2  = 1

    ! point array "a" to one with lowest energy

    if(en1(1) <= en2(1)) then
        qst = .true.	! point to 1
        na = l1
        nb = l2
        ja => j1
        jb => j2
        ea => en1
        eb => en2
    else
        qst = .false.	! point to 2
        na = l2
        nb = l1
        ja => j2
        jb => j1
        ea => en2
        eb => en1
    endif

    qdb = .false.     ! true if written double values 

    do

        do while(ja <= na)

            ! keep going or swap?

            if(jb <= nb) then
                if(ea(ja) > eb(jb)) then
                    if(swap()) cycle
                endif
            endif

            ! never write more than double energies

            qsm = ea(ja) == eun(neu)

            if(.not.(qdb .and. qsm)) then
                ! insert point in list
                qdb = qsm
                neu = neu + 1
                if(neu > lu) STOP 'UNIGRD ERROR - output array size exceeded'
                eun(neu) = ea(ja)
            endif

            ja = ja + 1

        end do

        qd = swap()
        if(ja > na) return

    end do

    contains

    logical*4 function swap()

    logical*4 qt

    if(qst) then
        na = l2
        nb = l1
        ja => j2
        jb => j1
        ea => en2
        eb => en1
    else
        na = l1
        nb = l2
        ja => j1
        jb => j2
        ea => en1
        eb => en2
    endif

    qst = .not.qst
    qt = ea(ja) == eun(neu)
    if(qt) ja = ja + 1   ! skip
    swap = qt

    return
    end function swap

    end subroutine unigrd

    !------------------------------------------------------------------------------

    function add_tab1(tb1,tb2)

    implicit none

    ! "add" 2 TAB1 types to produce a new TAB1

    type (tab1) :: add_tab1, tx
    type (tab1), intent(in) :: tb1,tb2

    real*8, allocatable :: e1(:), e2(:), ex(:)
    type (int_pair), allocatable :: ip(:), jp(:)

    logical*4 q2
    integer*4 i,j,k,ie,ix,iy1,iy2,lj
    real*8 y1,y2

    ! first form the new data by taking the union grid of the
    ! 2 tab1's x-values (usually energies) and then adding
    ! the interpolated values from both at each point.

    allocate(e1(tb1%np),e2(tb2%np),ex(tb1%np+tb2%np))

    do i = 1,tb1%np
        e1(i) = tb1%dat(i)%x
    end do

    do i = 1,tb2%np
        e2(i) = tb2%dat(i)%x
    end do

    call unigrd(tb1%np,e1,tb2%np,e2,tb1%np+tb2%np,ex,tx%np)
    allocate(tx%dat(tx%np))

    do i = 1,tx%np
        tx%dat(i)%x = ex(i)
        tx%dat(i)%y = intrp(ex(i),tb1) + intrp(ex(i),tb2)
    end do

    ! this almost works, except for the case of double-energy points.
    ! intrp will always return the value of the first double point at the energy
    ! of the point - it has to pick one, but the table is double-valued
    ! and discontinuous at the double-point. So we need to scan through
    ! the input TAB1 data for both sets and look for double points and then fix
    ! the y values. It would be better to make a special version of unigrd for the
    ! case of real pairs, but for now this fix will take care of it.

    do i = 1,tb1%np-1
        if(e1(i) /= e1(i+1)) cycle
        ! 2nd array too?
        j = 1
        do while (j <= tb2%np)
            if(e2(j) == e1(i)) exit
            j = j + 1
        end do
        q2 = .false.
        if(j < tb2%np) then
            q2 = e2(j) == e2(j+1)
        endif
        if(q2) then
            ! they both have double points!
            y1 = tb1%dat(i)%y + tb2%dat(j)%y
            y2 = tb1%dat(i+1)%y + tb2%dat(j+1)%y
        else
            y1 = tb1%dat(i)%y + intrp(e1(i),tb2)
            y2 = tb1%dat(i+1)%y + intrp(e1(i),tb2)
        endif
        ! put values in tx
        j = 1
        do while (j < tx%np)
            if(ex(j) == e1(i)) exit
            j = j + 1
        end do
        if(j >= tx%np)       stop 'ERROR adding TAB1 - double values not in sum'
        if(ex(j) /= ex(j+1)) stop 'ERROR adding TAB1 - double values not in sum'
        tx%dat(j)%y = y1
        tx%dat(j+1)%y = y2
    end do

    do i = 1,tb2%np-1
        if(e2(i) /= e2(i+1)) cycle
        ! 1st array too?
        j = 1
        do while (j <= tb1%np)
            if(e1(j) == e2(i)) exit
            j = j + 1
        end do
        q2 = .false.
        if(j < tb1%np) then
            q2 = e1(j) == e1(j+1)
        endif
        if(q2) then
            ! they both have double points!
            y1 = tb2%dat(i)%y + tb1%dat(j)%y
            y2 = tb2%dat(i+1)%y + tb1%dat(j+1)%y
        else
            y1 = tb2%dat(i)%y + intrp(e1(i),tb1)
            y2 = tb2%dat(i+1)%y + intrp(e1(i),tb1)
        endif
        ! put values in tx
        j = 1
        do while (j < tx%np)
            if(ex(j) == e2(i)) exit
            j = j + 1
        end do
        if(j >= tx%np)       stop 'ERROR adding TAB1 - double values not in sum'
        if(ex(j) /= ex(j+1)) stop 'ERROR adding TAB1 - double values not in sum'
        tx%dat(j)%y = y1
        tx%dat(j+1)%y = y2
    end do

    deallocate(e1,e2,ex)

    ! now we need to form the union of the interpolation regions.
    ! start by converting the indices of the region boudaries
    ! to energies, then we can convert the old indicies into
    ! indicies of the new union grid.

    allocate(e1(tb1%nr),e2(tb2%nr),ex(tb1%nr+tb2%nr))

    do i = 1,tb1%nr
        e1(i) = tb1%dat(tb1%itp(i)%x)%x
    end do

    do i = 1,tb2%nr
        e2(i) = tb2%dat(tb2%itp(i)%x)%x
    end do

    call unigrd(tb1%nr,e1,tb2%nr,e2,tb1%nr+tb2%nr,ex,k)

    allocate(ip(k),jp(k))

    ! now convert these energies to indicies as first step to forming
    ! an interpolation table. see which interpolation scheme (1-5)
    ! was used for each of the original TAB1s.
    ! WARNING: It's difficult to correctly add regions if the interpolation
    ! scheme used is different in the 2 tables. What should be done is
    ! the addition of points to better preserve the sum correctly when different
    ! interpolation schemes were used. Here, just take the highest scheme
    ! number and use that. An alternate would be to require that the overlap
    ! of the schemes must be the same, and leave it to the user to do the
    ! conversion from log to linear, or whatever is needed.

    do i = 1,k

        ie = 1
        do while(ie < tx%np)
            ie = ie + 1
            if(tx%dat(ie)%x >= ex(i)) exit
        end do
        ip(i)%x = ie

        if((ex(i) < tb1%dat(1)%x) .or. (ex(i) > tb1%dat(tb1%np)%x)) then
            iy1 = 0
        else
            ie = 1
            do while(ie < tb1%np)
                ie = ie + 1
                if(tb1%dat(ie)%x >= ex(i)) exit
            end do
            ix = 1
            do while(ix < tb1%nr)
                if(tb1%itp(ix)%x >= ie) exit
                ix = ix + 1
            end do
        endif
        iy1 = tb1%itp(ix)%y

        if((ex(i) < tb2%dat(1)%x) .or. (ex(i) > tb2%dat(tb2%np)%x)) then
            iy2 = 0
        else
            ie = 1
            do while(ie < tb2%np)
                ie = ie + 1
                if(tb2%dat(ie)%x >= ex(i)) exit
            end do
            ix = 1
            do while(ix < tb2%nr)
                if(tb2%itp(ix)%x >= ie) exit
                ix = ix + 1
            end do
        endif
        iy2 = tb2%itp(ix)%y

        ip(i)%y = max(iy1,iy2)     ! simple assumption

        if((ip(i)%y < 1) .or. (ip(i)%y > 5)) stop ' ERROR adding TAB1 types. Uncoded interpolation scheme encountered'
        if(iy1 /= iy2) write(6,*) ' Warning: Adding TAB1s with differing interpolation schemes'

    end do

    ! now we have an interpolation table ip for the new tab1 with k points,
    ! but there may be adjacent regions with the same interpolation scheme.
    ! scan through points and merge schemes into a final interpolation table.

    lj = 1
    jp(1) = ip(1)
    do i = 2,k
        if(ip(i)%y /= jp(lj)%y) lj = lj + 1
        jp(lj) = ip(i)
    end do

    tx%nr = lj
    allocate(tx%itp(lj))
    tx%itp = jp(1:lj)

    deallocate(e1,e2,ex)
    deallocate(ip,jp)

    add_tab1 = tx

    return
    end function add_tab1

end module endf_util
