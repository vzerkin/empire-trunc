module ENDF_MF9_IO

    use base_endf_io

    ! author: Sam Hoblit, NNDC, BNL
    ! provide I/O functions for MF9

    implicit none

    !-------------- Multiplicities for production of radioactive nuclides -------------

    public

    type mf9_fnl_st
        real qm                        ! mass-difference Q value (eV)
        real qi                        ! reaction Q value for state specified in subsection (eV)
        integer izap                   ! ZA of product nucleus
        integer lfs                    ! level number of nuclide produced in reaction
        type (tab1) mlt                ! multiplicities for FS. E=x, Y=y
    end type

    type MF_9
        type (mf_9), pointer :: next   ! next section
        integer mt                     ! MT
        real za                        ! ZA for material
        real awr                       ! AWR for material
        integer lis                    ! level # of target
        integer ns                     ! # of final states specified
        type(mf9_fnl_st), pointer :: fst(:)    ! final states
    end type

!------------------------------------------------------------------------------
    contains
!------------------------------------------------------------------------------

    subroutine read_mf9(mf9)

    implicit none

    type (mf_9), intent(out), target :: mf9

    integer i,n

    type (mf9_fnl_st), pointer :: fs

    call get_endf(mf9%za, mf9%awr, mf9%lis, n, mf9%ns, n)

    allocate(mf9%fst(mf9%ns),stat=n)
    if(n /= 0) call endf_badal
    do i = 1,mf9%ns
        fs => mf9%fst(i)
        call read_endf(fs%qm, fs%qi, fs%izap, fs%lfs, fs%mlt%nr, fs%mlt%np)
        call read_endf(fs%mlt)
    end do

    return
    end subroutine read_mf9

!------------------------------------------------------------------------------

    subroutine write_mf9(mf9)

    implicit none

    type (mf_9), intent(in), target :: mf9

    integer i
    type (mf9_fnl_st), pointer :: fs

    call set_mt(mf9%mt)
    call write_endf(mf9%za, mf9%awr, mf9%lis, 0, mf9%ns, 0)

    do i = 1,mf9%ns
        fs => mf9%fst(i)
        call write_endf(fs%qm, fs%qi, fs%izap, fs%lfs, fs%mlt%nr, fs%mlt%np)
        call write_endf(fs%mlt)
    end do

    call write_send

    return
    end subroutine write_mf9

!------------------------------------------------------------------------------

    subroutine del_mf9(mf9)

    implicit none

    type (mf_9) mf9

    integer i,n

    do i = 1,mf9%ns
        call del_tab1(mf9%fst(i)%mlt)
    end do
    deallocate(mf9%fst,stat=n)

    return
    end subroutine del_mf9

!------------------------------------------------------------------------------

    integer function lc_mf9(mf9)

    implicit none

    type (mf_9), intent(in) :: mf9

    integer i,l

    l = 1
    do i = 1,mf9%ns
        l = l + lc_tab1(mf9%fst(i)%mlt) + 1
    end do

    lc_mf9 = l

    return
    end function lc_mf9

end module ENDF_MF9_IO
