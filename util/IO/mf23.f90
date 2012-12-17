module ENDF_MF23_IO

    use base_endf_io

    ! author: Sam Hoblit, NNDC, BNL
    ! provide I/O functions for MF23

    implicit none

    !------------------ Smooth photon interaction cross sections -------------------------------

    public

    type MF_23
        type (mf_23), pointer :: next      ! next section
        integer mt                         ! MT
        real za                            ! ZA for material
        real awr                           ! AWR for material
        real epe                           ! subshell binding energy (eV)
        real efl                           ! fluorescence yield
        type (tab1) ctab                   ! Cross section table x=E, y=crs (b)
    end type

!---------------------------------------------------------------------------------------------
    contains
!---------------------------------------------------------------------------------------------

    subroutine read_mf23(mf23)

    implicit none

    type (mf_23), intent(out) :: mf23

    integer n

    call get_endf(mf23%za, mf23%awr, n, n, n, n)
    call read_endf(mf23%epe, mf23%efl, n, n, mf23%ctab%nr, mf23%ctab%np)
    call read_endf(mf23%ctab)

    return
    end subroutine read_mf23

!---------------------------------------------------------------------------------------------

    subroutine write_mf23(mf23)

    implicit none

    type (mf_23), intent(in) :: mf23

    call set_mt(mf23%mt)
    call write_endf(mf23%za, mf23%awr, 0, 0, 0, 0)
    call write_endf(mf23%epe, mf23%efl, 0, 0, mf23%ctab%nr, mf23%ctab%np)
    call write_endf(mf23%ctab)
    call write_send

    return
    end subroutine write_mf23

!---------------------------------------------------------------------------------------------

    subroutine del_mf23(mf23)

    implicit none

    type (mf_23) :: mf23

    call del_tab1(mf23%ctab)

    return
    end subroutine del_mf23

!---------------------------------------------------------------------------------------------

    integer function lc_mf23(mf23)

    implicit none

    type (mf_23), intent(in) :: mf23

    lc_mf23 = lc_tab1(mf23%ctab) + 2

    return
    end function lc_mf23

end module ENDF_MF23_IO
