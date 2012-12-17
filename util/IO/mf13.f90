module ENDF_MF13_IO

    use base_endf_io

    ! author: Sam Hoblit, NNDC, BNL
    ! provide I/O functions for MF13

    implicit none

    !------------------------ Photon production cross sections -------------------------------

    public

    type mf13_photon
        real eg                            ! photon energy (eV)
        real es                            ! energy of level from which photon originates
        integer lp                         ! primary flag
        integer lf                         ! dist law # (1 or 2)
        type (tab1) crs                    ! cross sections
    end type

    type MF_13
        type (mf_13), pointer :: next      ! next section
        integer mt                         ! MT
        real za                            ! ZA for material
        real awr                           ! AWR for material
        integer nk                         ! # discreet photons incl continuum
        type (tab1), pointer :: tyld             ! total cross section
        type (mf13_photon), pointer :: gam(:)    ! discrete photon cross sections (nk)
    end type

!---------------------------------------------------------------------------------------------
    contains
!---------------------------------------------------------------------------------------------

    subroutine read_mf13(mf13)

    implicit none

    type (mf_13), intent(out), target :: mf13

    integer i,n

    type (mf13_photon), pointer :: gm

    call get_endf(mf13%za, mf13%awr, n, n, mf13%nk, n)

    if(mf13%nk .gt. 1) then
        allocate(mf13%tyld)
        call read_endf(n, n, mf13%tyld%nr, mf13%tyld%np)
        call read_endf(mf13%tyld)
    else
        nullify(mf13%tyld)
    endif

    allocate(mf13%gam(mf13%nk),stat=n)
    if(n .ne. 0) call endf_badal
    do i = 1,mf13%nk
        gm => mf13%gam(i)
        call read_endf(gm%eg, gm%es, gm%lp, gm%lf, gm%crs%nr, gm%crs%np)
        call read_endf(gm%crs)
    end do

    return
    end subroutine read_mf13

!---------------------------------------------------------------------------------------------

    subroutine write_mf13(mf13)

    implicit none

    type (mf_13), intent(in), target :: mf13

    integer i
    type (mf13_photon), pointer :: gm

    call set_mt(mf13%mt)
    call write_endf(mf13%za, mf13%awr, 0, 0, mf13%nk, 0)
    if(mf13%nk .gt. 1) then
        call write_endf(0, 0, mf13%tyld%nr, mf13%tyld%np)
        call write_endf(mf13%tyld)
    endif
    do i = 1,mf13%nk
        gm => mf13%gam(i)
        call write_endf(gm%eg, gm%es, gm%lp, gm%lf, gm%crs%nr, gm%crs%np)
        call write_endf(gm%crs)
    end do
    call write_send

    return
    end subroutine write_mf13

!---------------------------------------------------------------------------------------------

    subroutine del_mf13(mf13)

    implicit none

    type (mf_13), target :: mf13

    integer i,n

    if(associated(mf13%tyld)) call remove_tab1(mf13%tyld)

    if(associated(mf13%gam)) then
        do i = 1,mf13%nk
            call del_tab1(mf13%gam(i)%crs)
        end do
        deallocate(mf13%gam,stat=n)
    endif

    return
    end subroutine del_mf13

!---------------------------------------------------------------------------------------------

    integer function lc_mf13(mf13)

    implicit none

    type (mf_13), target :: mf13

    integer i,l

    l = 1
    if(mf13%nk .gt. 1) then
        l = l + lc_tab1(mf13%tyld) + 1
    endif
    do i = 1,mf13%nk
        l = l + lc_tab1(mf13%gam(i)%crs) + 1
    end do

    lc_mf13 = l

    return
    end function lc_mf13

end module ENDF_MF13_IO
