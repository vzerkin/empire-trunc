module ENDF_MF15_IO

    use base_endf_io

    ! author: Sam Hoblit, NNDC, BNL
    ! provide I/O functions for MF15

    implicit none

    !--------------------- Photon production cross sections -------------------------------

    public

    type mf15_tabular_dist
        real e                         ! neutron energy
        type (tab1) gtb                ! prob table
    end type

    type mf15_dist
        integer lf                     ! dist law (only 15?)
        type (tab1) ptb                ! prob table
        integer nr                     ! # interp ranges
        integer ne                     ! # incident E 
        type (int_pair), pointer :: inb(:)             ! incident E interpolation table
        type (mf15_tabular_dist), pointer :: gtb(:)    ! g distributions (ne)
    end type

    type MF_15
        type (mf_15), pointer :: next  ! next section
        integer mt                     ! MT
        real za                        ! ZA for material
        real awr                       ! AWR for material
        integer nc                     ! # partial distributions
        type (mf15_dist), pointer :: dst(:)       ! distribution
    end type

!---------------------------------------------------------------------------------------------
    contains
!---------------------------------------------------------------------------------------------

    subroutine read_mf15(mf15)

    implicit none

    type (mf_15), intent(out), target :: mf15

    integer i,j,n
    real xx
    type (mf15_dist), pointer :: ds
    type (mf15_tabular_dist), pointer :: tb

    call get_endf(mf15%za, mf15%awr, n, n, mf15%nc, n)
    allocate(mf15%dst(mf15%nc),stat=n)
    if(n /= 0) call endf_badal

    do i = 1,mf15%nc

        ds => mf15%dst(i)
        call read_endf(xx, xx, n, ds%lf, ds%ptb%nr, ds%ptb%np)
        call read_endf(ds%ptb)

        call read_endf(xx, xx, n, n, ds%nr, ds%ne)
        allocate(ds%inb(ds%nr), ds%gtb(ds%ne),stat=n)
        if(n /= 0) call endf_badal
        call read_endf(ds%inb,ds%nr)

        do j = 1, ds%ne
            tb => ds%gtb(j)
            call read_endf(xx, tb%e, n, n, tb%gtb%nr, tb%gtb%np)
            call read_endf(tb%gtb)
        end do

    end do

    return
    end subroutine read_mf15

!---------------------------------------------------------------------------------------------

    subroutine write_mf15(mf15)

    implicit none

    type (mf_15), intent(in), target :: mf15

    integer i,j

    type (mf15_dist), pointer :: ds
    type (mf15_tabular_dist), pointer :: tb

    call set_mt(mf15%mt)
    call write_endf(mf15%za, mf15%awr, 0, 0, mf15%nc, 0)

    do i = 1,mf15%nc
        ds => mf15%dst(i)
        call write_endf(zero, zero, 0, ds%lf, ds%ptb%nr, ds%ptb%np)
        call write_endf(ds%ptb)
        call write_endf(zero, zero, 0, 0, ds%nr, ds%ne)
        call write_endf(ds%inb,ds%nr)
        do j = 1, ds%ne
            tb => ds%gtb(j)
            call write_endf(zero, tb%e, 0, 0, tb%gtb%nr, tb%gtb%np)
            call write_endf(tb%gtb)
        end do
    end do

    call write_send

    return
    end subroutine write_mf15

!---------------------------------------------------------------------------------------------

    subroutine del_mf15(mf15)

    implicit none

    type (mf_15) mf15

    integer i,j,n

    do i = 1,mf15%nc
        call del_tab1(mf15%dst(i)%ptb)
        do j = 1, mf15%dst(i)%ne
            call del_tab1(mf15%dst(i)%gtb(j)%gtb)
        end do
        deallocate(mf15%dst(i)%inb, mf15%dst(i)%gtb,stat=n)
    end do

    end subroutine del_mf15

!---------------------------------------------------------------------------------------------

    integer function lc_mf15(mf15)

    implicit none

    type (mf_15), intent(in), target :: mf15

    integer i,j,l

    type (mf15_dist), pointer :: ds

    l = 1
    do i = 1,mf15%nc
        ds => mf15%dst(i)
        l = l + lc_tab1(ds%ptb) + (2*ds%nr+5)/6 + 2
        do j = 1, ds%ne
            l = l + lc_tab1(ds%gtb(j)%gtb) + 1
        end do
    end do

    lc_mf15 = l

    return
    end function lc_mf15

end module ENDF_MF15_IO
