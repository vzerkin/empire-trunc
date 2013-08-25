module ENDF_MF27_IO

    use base_endf_io

    ! author: Sam Hoblit, NNDC, BNL
    ! provide I/O functions for MF27

    implicit none

    !------------------ Atomic form factors or scattering functions -------------------------------

    public

    type MF_27
        type (mf_27), pointer :: next      ! next section
        integer mt                         ! MT
        real za                            ! ZA for material
        real awr                           ! AWR for material
        real z                             ! Z in form factor
        type (tab1) ftab                   ! form factor table x=E, y=crs (b)
    end type

!---------------------------------------------------------------------------------------------
    contains
!---------------------------------------------------------------------------------------------

    subroutine read_mf27(mf27)

    implicit none

    type (mf_27), intent(out) :: mf27

    integer n
    real xx

    call get_endf(mf27%za, mf27%awr, n, n, n, n)
    call read_endf(xx, mf27%z, n, n, mf27%ftab%nr, mf27%ftab%np)
    call read_endf(mf27%ftab)

    return
    end subroutine read_mf27

!---------------------------------------------------------------------------------------------

    subroutine write_mf27(mf27)

    implicit none

    type (mf_27), intent(in) :: mf27

    call set_mt(mf27%mt)
    call write_endf(mf27%za, mf27%awr, 0, 0, 0, 0)
    call write_endf(zero, mf27%z, 0, 0, mf27%ftab%nr, mf27%ftab%np)
    call write_endf(mf27%ftab)
    call write_send

    return
    end subroutine write_mf27

!---------------------------------------------------------------------------------------------

    subroutine del_mf27(mf27)

    implicit none

    type (mf_27) mf27

    call del_tab1(mf27%ftab)

    return
    end subroutine del_mf27

!---------------------------------------------------------------------------------------------

    integer function lc_mf27(mf27)

    implicit none

    type (mf_27), intent(in) :: mf27

    lc_mf27 = lc_tab1(mf27%ftab) + 2

    return
    end function lc_mf27

end module ENDF_MF27_IO
