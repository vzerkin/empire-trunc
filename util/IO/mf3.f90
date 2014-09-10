module ENDF_MF3_IO

    use endf_iolib

    ! author: Sam Hoblit, NNDC, BNL
    ! provide I/O functions for MF3

    implicit none

    ! ~~~~~~~~~~~~~~~~~~~~ Reaction Cross Sections ~~~~~~~~~~~~~~~~~~~~~~~~~~~~

    public

    type MF_3
        type (MF_3), pointer :: next
        integer mt                   ! MT reaction
        real za                      ! ZA
        real awr                     ! AWR
        real qm                      ! mass-diff Q-value
        real qi                      ! Q-value for lowest E state in MT = QM - E_excited_state
        integer lr                   ! "complex" breakup flag
        type (tab1) tb               ! table of values
    end type

    contains

!------------------------------------------------------------------------------

    subroutine read_mf3(mf3)

    implicit none

    type (mf_3), intent(out) :: mf3

    integer n

    call get_endf(mf3%za, mf3%awr, n, n, n, n)
    call read_endf(mf3%qm, mf3%qi, n, mf3%lr, mf3%tb%nr, mf3%tb%np)
    call read_endf(mf3%tb)

    return
    end subroutine read_mf3

!------------------------------------------------------------------------------

    subroutine write_mf3(mf3)

    implicit none

    type (mf_3), intent(in) :: mf3

    call set_mt(mf3%mt)
    call write_endf(mf3%za, mf3%awr, 0, 0, 0, 0)
    call write_endf(mf3%qm, mf3%qi, 0, mf3%lr, mf3%tb%nr, mf3%tb%np)
    call write_endf(mf3%tb)
    call write_send

    return
    end subroutine write_mf3

!------------------------------------------------------------------------------

    subroutine del_mf3(mf3)

    implicit none

    type (mf_3) mf3

    call del_tab1(mf3%tb)

    return
    end subroutine del_mf3

!------------------------------------------------------------------------------

    integer function lc_mf3(mf3)

    implicit none

    type (mf_3), intent(in) :: mf3

    lc_mf3 = lc_tab1(mf3%tb) + 2

    return
    end function lc_mf3

end module ENDF_MF3_IO
