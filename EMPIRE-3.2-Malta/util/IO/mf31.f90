module ENDF_MF31_IO

    use base_endf_io
    use endf_cov_io

    ! author: Sam Hoblit, NNDC, BNL
    ! provide I/O functions for MF31

    implicit none

    ! ~~~~~~~~~ Covariances for nubar (ave # neutrons/fission) ~~~~~~~~~~~~~~~~~~

    public

    type MF31_sect
        integer mf1                   ! mf of 2nd E-dep crs
        integer lfs1                  ! final excited state of 2nd E-dep crs
        integer mat1                  ! MAT for 2nd E-dep crs
        integer mt1                   ! MT for 2nd E-dep crs
        integer nc                    ! # of NC-type sub-sections
        integer ni                    ! # of NI-type sub-sections
        type (nc_cov_sect), pointer :: ncs(:)    ! NC sections
        type (ni_cov_sect), pointer :: nis(:)    ! NI sections
    end type

    type MF_31
        type (MF_31), pointer :: next
        integer mt
        real za
        real awr
        integer mtl                   ! MT for lumped covar (851-870)
        integer nl                    ! number of sections (with same MT)
        type (MF31_sect), pointer :: sct(:)    ! sections (nl)
    end type

!------------------------------------------------------------------------------
    contains
!------------------------------------------------------------------------------

    subroutine read_mf31(mf31)

    implicit none

    type (mf_31), intent(out), target :: mf31

    integer i,j,n
    real x1,x2
    type (MF31_sect), pointer :: sc

    call get_endf(mf31%za, mf31%awr, n, mf31%mtl, n, mf31%nl)

    if((mf31%mtl .ne. 0) .and. (mf31%nl .ne. 0)) then
        ! MTL .ne. 0 indicates lumped covars and NL == 0
        write(erlin,*) 'Non-zero NL found with non-zero MTL in MF31:',mf31%nl
        call endf_error(erlin)
    end if

    allocate(mf31%sct(mf31%nl),stat=n)
    if(n .ne. 0) call endf_badal

    do i = 1,mf31%nl

        sc => mf31%sct(i)
        call read_endf(x1, x2, sc%mat1, sc%mt1, sc%nc, sc%ni)
        sc%mf1 = nint(x1)
        sc%lfs1 = nint(x2)

        allocate(sc%ncs(sc%nc),stat=n)
        if(n /= 0) call endf_badal
        do j = 1,sc%nc
            call read_nc(sc%ncs(j))
        end do

        allocate(sc%nis(sc%ni),stat=n)
        if(n /= 0) call endf_badal
        do j = 1,sc%ni
            call read_ni(sc%nis(j),31)
            if((sc%nis(j)%ne .eq. 0) .and. ((sc%nis(j)%lb .ne. 0) .and. (sc%nis(j)%lb .ne. 1))) then
                write(erlin,*) 'Undefined LB encountered for spontaneous fission cov in MF31:',sc%nis(j)%lb
                call endf_error(erlin)
            endif
        end do

    end do

    return
    end subroutine read_mf31

!------------------------------------------------------------------------------

    subroutine write_mf31(mf31)

    implicit none

    type (mf_31), intent(in), target :: mf31

    integer i,j
    real x1,x2
    type (MF31_sect), pointer :: sc

    call set_mt(mf31%mt)
    call write_endf(mf31%za, mf31%awr, 0, mf31%mtl, 0, mf31%nl)

    if((mf31%mtl .ne. 0) .and. (mf31%nl .ne. 0)) then
        ! MTL .ne. 0 indicates lumped covars and NL == 0
        write(erlin,*) 'Non-zero NL found with non-zero MTL in MF31:',mf31%nl
        call endf_error(erlin)
    end if

    do i = 1,mf31%nl
        sc => mf31%sct(i)
        x1 = sc%mf1
        x2 = sc%lfs1
        call write_endf(x1, x2, sc%mat1, sc%mt1, sc%nc, sc%ni)
        do j = 1,sc%nc
            call write_nc(sc%ncs(j))
        end do
        do j = 1,sc%ni
            if((sc%nis(j)%ne .eq. 0) .and. ((sc%nis(j)%lb .ne. 0) .and. (sc%nis(j)%lb .ne. 1))) then
                write(erlin,*) 'Undefined LB encountered for spontaneous fission cov in MF31:',sc%nis(j)%lb
                call endf_error(erlin)
            endif
            call write_ni(sc%nis(j),31)
        end do
    end do

    call write_send

    return
    end subroutine write_mf31

!------------------------------------------------------------------------------

    subroutine del_mf31(mf31)

    implicit none

    type (mf_31), target :: mf31

    integer i,j,n

    do i = 1,mf31%nl
        do j = 1,mf31%sct(i)%nc
            call del_nc(mf31%sct(i)%ncs(j))
        end do
        deallocate(mf31%sct(i)%ncs,stat=n)
        do j = 1,mf31%sct(i)%ni
            call del_ni(mf31%sct(i)%nis(j))
        end do
        deallocate(mf31%sct(i)%nis,stat=n)
    end do

    return
    end subroutine del_mf31

!------------------------------------------------------------------------------

    integer function lc_mf31(mf31)

    implicit none

    type (mf_31), intent(in), target :: mf31

    integer i,j,l
    type (MF31_sect), pointer :: sc

    l = 1

    if((mf31%mtl .ne. 0) .and. (mf31%nl .ne. 0)) then
        ! MTL .ne. 0 indicates lumped covars and NL == 0
        write(erlin,*) 'Non-zero NL found with non-zero MTL in MF31:',mf31%nl
        call endf_error(erlin)
    end if

    do i = 1,mf31%nl
        sc => mf31%sct(i)
        l = l + 1
        do j = 1,sc%nc
            l = l + lc_nc(sc%ncs(j))
        end do
        do j = 1,sc%ni
            if((sc%nis(j)%ne .eq. 0) .and. ((sc%nis(j)%lb .ne. 0) .and. (sc%nis(j)%lb .ne. 1))) then
                write(erlin,*) 'Undefined LB encountered for spontaneous fission cov in MF31:',sc%nis(j)%lb
                call endf_error(erlin)
            endif
            l = l + lc_ni(sc%nis(j),31)
        end do
    end do

    lc_mf31 = l

    return
    end function lc_mf31

end module ENDF_MF31_IO
