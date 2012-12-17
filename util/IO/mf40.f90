module ENDF_MF40_IO

    use base_endf_io
    use endf_cov_io

    ! author: Sam Hoblit, NNDC, BNL
    ! provide I/O functions for MF40

    implicit none

    ! -------------  Covariances for production of radioactive nuclei  ----------------

    public

    type MF40_subsect
        real xmf1                          ! mf of 2nd E-dep crs
        real xlfs1                         ! final excited state of 2nd E-dep crs
        integer mat1                       ! MAT for 2nd E-dep crs
        integer mt1                        ! MT for 2nd E-dep crs
        integer nc                         ! # of NC-type sub-sections
        integer ni                         ! # of NI-type sub-sections
        type (nc_cov_sect), pointer :: ncs(:)    ! NC sections
        type (ni_cov_sect), pointer :: nis(:)    ! NI sections
    end type

    type MF40_sect
        real qm                            ! mass-diff Q value based on GS of residual
        real qi                            ! reaction Q-value.
        integer lfs                        ! level number of nuclide produced
        integer nl                         ! # subsections
        type (mf40_subsect), pointer :: sub(:)     ! subsections
    end type

    type MF_40
        type (MF_40), pointer :: next
        integer mt
        real za
        real awr
        integer lis                        ! level # of target
        integer ns                         ! number of subsections
        type (MF40_sect), pointer :: sct(:)        ! sections (ns)
    end type

!------------------------------------------------------------------------------
    contains
!------------------------------------------------------------------------------

    subroutine read_mf40(mf40)

    implicit none

    type (mf_40), intent(out), target :: mf40

    integer i,j,k,n
    type (mf40_sect), pointer :: sc
    type (mf40_subsect), pointer :: ss

    call get_endf(mf40%za, mf40%awr, mf40%lis, n, mf40%ns, n)
    allocate(mf40%sct(mf40%ns),stat=n)
    if(n /= 0) call endf_badal

    do i = 1, mf40%ns

        sc => mf40%sct(i)
        call read_endf(sc%qm, sc%qi, n, sc%lfs, n, sc%nl)
        allocate(sc%sub(sc%nl),stat=n)
        if(n /= 0) call endf_badal

        do j = 1, sc%nl
            ss => sc%sub(j)
            call read_endf(ss%xmf1, ss%xlfs1, ss%mat1, ss%mt1, ss%nc, ss%ni)
            allocate(ss%ncs(ss%nc),stat=n)
            if(n /= 0) call endf_badal
            do k = 1,ss%nc
                call read_nc(ss%ncs(k))
            end do
            allocate(ss%nis(ss%ni),stat=n)
            if(n /= 0) call endf_badal
            do k = 1,ss%ni
                call read_ni(ss%nis(k),40)
            end do
        end do

    end do

    return
    end subroutine read_mf40

!------------------------------------------------------------------------------

    subroutine write_mf40(mf40)

    implicit none

    type (mf_40), intent(in), target :: mf40

    integer i,j,k
    type (mf40_sect), pointer :: sc
    type (mf40_subsect), pointer :: ss

    call set_mt(mf40%mt)
    call write_endf(mf40%za, mf40%awr, mf40%lis, 0, mf40%ns, 0)
    do i = 1, mf40%ns
        sc => mf40%sct(i)
        call write_endf(sc%qm, sc%qi, 0, sc%lfs, 0, sc%nl)
        do j = 1, sc%nl
            ss => sc%sub(j)
            call write_endf(ss%xmf1, ss%xlfs1, ss%mat1, ss%mt1, ss%nc, ss%ni)
            do k = 1,ss%nc
                call write_nc(ss%ncs(k))
            end do
            do k = 1,ss%ni
                call write_ni(ss%nis(k),40)
            end do
        end do
    end do
    call write_send

    return
    end subroutine write_mf40

!------------------------------------------------------------------------------

    subroutine del_mf40(mf40)

    implicit none

    type (mf_40), target :: mf40

    integer i,j,k,n
    type (mf40_subsect), pointer :: ss

    do i = 1, mf40%ns
        do j = 1, mf40%sct(i)%nl
            ss => mf40%sct(i)%sub(j)
            do k = 1,ss%nc
                call del_nc(ss%ncs(k))
            end do
            deallocate(ss%ncs,stat=n)
            do k = 1,ss%ni
                call del_ni(ss%nis(k))
            end do
            deallocate(ss%nis,stat=n)
        end do
        deallocate(mf40%sct(i)%sub,stat=n)
    end do
    deallocate(mf40%sct,stat=n)

    return
    end subroutine del_mf40

!------------------------------------------------------------------------------

    integer function lc_mf40(mf40)

    implicit none

    type (mf_40), intent(in) :: mf40

    integer i,j,k,l
    type (mf40_sect), pointer :: sc
    type (mf40_subsect), pointer :: ss

    l = 1
    do i = 1, mf40%ns
        sc => mf40%sct(i)
        l = l + 1
        do j = 1, sc%nl
            ss => sc%sub(j)
            l = l + 1
            do k = 1,ss%nc
                l = l + lc_nc(ss%ncs(k))
            end do
            do k = 1,ss%ni
                l = l + lc_ni(ss%nis(k),40)
            end do
        end do
    end do

    lc_mf40 = l

    return
    end function lc_mf40

end module ENDF_MF40_IO
