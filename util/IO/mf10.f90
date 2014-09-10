module ENDF_MF10_IO

    use endf_iolib

    ! author: Sam Hoblit, NNDC, BNL
    ! provide I/O functions for MF10

    implicit none

    !---------------- Cross sections for production of radioactive nuclides -------------

    public

    type mf10_fnl_st
        real qm                         ! mass-difference Q value (eV)
        real qi                         ! reaction Q value for state specified in subsection (eV)
        integer izap                    ! ZA of product nucleus
        integer lfs                     ! level number of nuclide produced in reaction
        type (tab1) crs                 ! cross sections for FS. E=x, sigma=y
    end type

    type MF_10
        type (mf_10), pointer :: next   ! next section
        integer mt                      ! MT
        real za                         ! ZA for material
        real awr                        ! AWR for material
        integer lis                     ! level # of target
        integer ns                      ! # of final states specified
        type(mf10_fnl_st), pointer :: fst(:)    ! final states
    end type

!---------------------------------------------------------------------------------------------
    contains
!---------------------------------------------------------------------------------------------

    subroutine read_mf10(mf10)

    implicit none

    type (mf_10), intent(out), target :: mf10

    integer i,n

    type (mf10_fnl_st), pointer :: fs

    call get_endf(mf10%za, mf10%awr, mf10%lis, n, mf10%ns, n)
    allocate(mf10%fst(mf10%ns),stat=n)
    if(n .ne. 0) call endf_badal

    do i = 1,mf10%ns
        fs => mf10%fst(i)
        call read_endf(fs%qm, fs%qi, fs%izap, fs%lfs, fs%crs%nr, fs%crs%np)
        call read_endf(fs%crs)
    end do

    return
    end subroutine read_mf10

!---------------------------------------------------------------------------------------------

    subroutine write_mf10(mf10)

    implicit none

    type (mf_10), intent(in), target :: mf10

    integer i
    type (mf10_fnl_st), pointer :: fs

    call set_mt(mf10%mt)
    call write_endf(mf10%za, mf10%awr, mf10%lis, 0, mf10%ns, 0)
    do i = 1,mf10%ns
        fs => mf10%fst(i)
        call write_endf(fs%qm, fs%qi, fs%izap, fs%lfs, fs%crs%nr, fs%crs%np)
        call write_endf(fs%crs)
    end do
    call write_send

    return
    end subroutine write_mf10

!---------------------------------------------------------------------------------------------

    subroutine del_mf10(mf10)

    implicit none

    type (mf_10), target :: mf10

    integer i,n

    do i = 1,mf10%ns
        call del_tab1(mf10%fst(i)%crs)
    end do
    deallocate(mf10%fst,stat=n)

    return
    end subroutine del_mf10

!---------------------------------------------------------------------------------------------

    integer function lc_mf10(mf10)

    implicit none

    type (mf_10), intent(in) :: mf10

    integer i,l

    l = 1
    do i = 1,mf10%ns
        l = l + lc_tab1(mf10%fst(i)%crs) + 1
    end do

    lc_mf10 = l

    return
    end function lc_mf10

end module ENDF_MF10_IO
