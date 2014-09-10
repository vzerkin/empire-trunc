module ENDF_MF5_IO

    use endf_iolib

    ! author: Sam Hoblit, NNDC, BNL
    ! provide I/O functions for MF5

    implicit none

    ! ~~~~~~~~~~~~~~~~ Energy distributions of secondary particles  ~~~~~~~~~~~~~~~~~

    public

    type mf5_tb2
        real e                                  ! incident E
        type (tab1) tb1                         ! outgoing dist table (E', g dist). See manual
    end type

    type mf5_subsect
        integer lf                              ! flag for E dist law
        real u                                  ! upper E limit for 2nd particle
        real efl                                ! constant in E-dep fission spec, LF=12
        real efh                                ! constant in E-dep fission spec, LF=12
        type (tab1) p                           ! frac part of crs in kth bin
        integer nr                              ! # interp ranges, LF=1
        integer ne                              ! # E ranges, LF=1
        type (int_pair), pointer :: itp(:)      ! interpolation tables (nr), LF=1
        type (mf5_tb2), pointer :: tb2(:)       ! secondary dist tables for each incident E (ne), LF=1
        type (tab1), pointer :: tht             ! eff temp of 2nd E dist, LF=5,7,9
        type (tab1), pointer :: g               ! partial E dist, LF=5
        type (tab1), pointer :: b               ! param for E-dep Watt spec, LF=11
        type (tab1), pointer :: a               ! param for E-dep Watt spec, LF=11
        type (tab1), pointer :: tm              ! max temp of E-dep fission spec, LF=12
    end type

    type MF_5
        type (mf_5), pointer :: next            ! next section
        integer mt                              ! MT
        real za                                 ! ZA for material
        real awr                                ! AWR for material
        integer nk                              ! # part E dists
        type (mf5_subsect), pointer :: sct(:)   ! sub-sections
    end type

!------------------------------------------------------------------------------
    contains
!------------------------------------------------------------------------------

    subroutine read_mf5(mf5)

    implicit none

    type (mf_5), intent(out), target :: mf5

    integer i,j,n
    real xx

    type (mf5_subsect), pointer :: sc
    type (mf5_tb2), pointer :: tb

    call get_endf(mf5%za, mf5%awr, n, n, mf5%nk, n)
    allocate(mf5%sct(mf5%nk),stat=n)
    if(n .ne. 0) call endf_badal

    do i = 1,mf5%nk

        sc => mf5%sct(i)

        call read_endf(sc%u, xx, n, sc%lf, sc%p%nr, sc%p%np)
        call read_endf(sc%p)

        sc%nr = 0
        sc%ne = 0
        sc%efl = 0
        sc%efh = 0
        nullify(sc%itp, sc%tb2, sc%tht, sc%g, sc%a, sc%b, sc%tm)

        select case(sc%lf)
        case(1)
            call read_endf(n, n, sc%nr, sc%ne)
            allocate(sc%itp(sc%nr), sc%tb2(sc%ne),stat=n)
            if(n .ne. 0) call endf_badal
            call read_endf(sc%itp,sc%nr)
            do j = 1,sc%ne
                tb => sc%tb2(j)
                call read_endf(xx, tb%e, n, n, tb%tb1%nr, tb%tb1%np)
                call read_endf(tb%tb1)
            end do
        case(5)
            allocate(sc%tht, sc%g)
            call read_endf(n, n, sc%tht%nr, sc%tht%np)
            call read_endf(sc%tht)
            call read_endf(n, n, sc%g%nr, sc%g%np)
            call read_endf(sc%g)
        case(7,9)
            allocate(sc%tht)
            call read_endf(n, n, sc%tht%nr, sc%tht%np)
            call read_endf(sc%tht)
        case(11)
            allocate(sc%a, sc%b)
            call read_endf(n, n, sc%a%nr, sc%a%np)
            call read_endf(sc%a)
            call read_endf(n, n, sc%b%nr, sc%b%np)
            call read_endf(sc%b)
        case(12)
            allocate(sc%tm)
            call read_endf(sc%efl, sc%efh, n, n, sc%tm%nr, sc%tm%np)
            call read_endf(sc%tm)
        case default
            write(erlin,*) 'Undefined LF found in MF5:',sc%lf
            call endf_error(erlin)
        end select

    end do

    return
    end subroutine read_mf5

!------------------------------------------------------------------------------

    subroutine write_mf5(mf5)

    implicit none

    type (mf_5), intent(in), target :: mf5

    integer i,j

    type (mf5_subsect), pointer :: sc
    type (mf5_tb2), pointer :: tb

    call set_mt(mf5%mt)
    call write_endf(mf5%za, mf5%awr, 0, 0, mf5%nk, 0)

    do i = 1,mf5%nk

        sc => mf5%sct(i)

        call write_endf(sc%u, zero, 0, sc%lf, sc%p%nr, sc%p%np)
        call write_endf(sc%p)

        select case(sc%lf)
        case(1)
            call write_endf(0, 0, sc%nr, sc%ne)
            call write_endf(sc%itp,sc%nr)
            do j = 1,sc%ne
                tb => sc%tb2(j)
                call write_endf(zero, tb%e, 0, 0, tb%tb1%nr, tb%tb1%np)
                call write_endf(tb%tb1)
            end do
        case(5)
            call write_endf(0, 0, sc%tht%nr, sc%tht%np)
            call write_endf(sc%tht)
            call write_endf(0, 0, sc%g%nr, sc%g%np)
            call write_endf(sc%g)
        case(7,9)
            call write_endf(0, 0, sc%tht%nr, sc%tht%np)
            call write_endf(sc%tht)
        case(11)
            call write_endf(0, 0, sc%a%nr, sc%a%np)
            call write_endf(sc%a)
            call write_endf(0, 0, sc%b%nr, sc%b%np)
            call write_endf(sc%b)
        case(12)
            call write_endf(sc%efl, sc%efh, 0, 0, sc%tm%nr, sc%tm%np)
            call write_endf(sc%tm)
        case default
            write(erlin,*) 'Undefined LF found in MF5:',sc%lf
            call endf_error(erlin)
        end select

    end do

    call write_send

    return
    end subroutine write_mf5

!------------------------------------------------------------------------------

    subroutine del_mf5(mf5)

    implicit none

    type (mf_5) :: mf5

    integer i,j,n

    type (mf5_subsect), pointer :: sc

    do i = 1,mf5%nk
        sc => mf5%sct(i)
        call del_tab1(sc%p)
        select case(sc%lf)
        case(1)
            do j = 1,sc%ne
                call del_tab1(sc%tb2(j)%tb1)
            end do
            deallocate(sc%itp, sc%tb2,stat=n)
        case(5)
            call remove_tab1(sc%tht)
            call remove_tab1(sc%g)
        case(7,9)
            call remove_tab1(sc%tht)
        case(11)
            call remove_tab1(sc%a)
            call remove_tab1(sc%b)
        case(12)
            call remove_tab1(sc%tm)
        end select
    end do

    if(associated(mf5%sct)) deallocate(mf5%sct,stat=n)

    return
    end subroutine del_mf5

!------------------------------------------------------------------------------

    integer function lc_mf5(mf5)

    implicit none

    type (mf_5), intent(in) :: mf5

    integer i,j,l

    l = 1

    do i = 1,mf5%nk
        l = l + lc_tab1(mf5%sct(i)%p) + 1
        select case(mf5%sct(i)%lf)
        case(1)
            l = l + (2*mf5%sct(i)%nr+5)/6 + 1
            do j = 1,mf5%sct(i)%ne
                l = l + lc_tab1(mf5%sct(i)%tb2(j)%tb1) + 1
            end do
        case(5)
            l = l + lc_tab1(mf5%sct(i)%tht) + lc_tab1(mf5%sct(i)%g) + 2
        case(7,9)
            l = l + lc_tab1(mf5%sct(i)%tht) + 1
        case(11)
            l = l + lc_tab1(mf5%sct(i)%a) + lc_tab1(mf5%sct(i)%b) + 2
        case(12)
            l = l + lc_tab1(mf5%sct(i)%tm) + 1
        case default
            write(erlin,*) 'Undefined LF found in MF5:',mf5%sct(i)%lf
            call endf_error(erlin)
        end select

    end do

    lc_mf5 = l

    return
    end function lc_mf5

end module ENDF_MF5_IO
