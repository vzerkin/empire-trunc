module ENDF_MF33_IO

    use base_endf_io
    use endf_cov_io

    ! author: Sam Hoblit, NNDC, BNL
    ! provide I/O functions for MF33

    implicit none

    ! ~~~~~~~~~~~~  Covariances of neutron cross sections  ~~~~~~~~~~~~~~~~~~~~~~~~

    public

    type MF33_sect
        real xmf1                      ! mf of 2nd E-dep crs
        real xlfs1                     ! final excited state of 2nd E-dep crs
        integer mat1                   ! MAT for 2nd E-dep crs
        integer mt1                    ! MT for 2nd E-dep crs
        integer nc                     ! # of NC-type sub-sections
        integer ni                     ! # of NI-type sub-sections
        type (nc_cov_sect), pointer :: ncs(:)    ! NC sections
        type (ni_cov_sect), pointer :: nis(:)    ! NI sections
    end type

    type MF_33
        type (MF_33), pointer :: next
        integer mt
        real za
        real awr
        integer mtl                    ! MT for lumped covar (851-870)
        integer nl                     ! number of sections (with same MT)
        type (MF33_sect), pointer :: sct(:)    ! sections (nl)
    end type

!------------------------------------------------------------------------------
    contains
!------------------------------------------------------------------------------

    subroutine read_mf33(mf33)

    implicit none

    type (mf_33), intent(out), target :: mf33

    integer i,j,n
    type (MF33_sect), pointer :: sc

    call get_endf(mf33%za, mf33%awr, n, mf33%mtl, n, mf33%nl)

    if((mf33%mtl .ne. 0) .and. (mf33%nl .ne. 0)) then
        ! MTL .ne. 0 indicates lumped covars and NL == 0
        write(erlin,*) 'Non-zero NL found with non-zero MTL in MF33:',mf33%nl
        call endf_error(erlin)
    end if

    allocate(mf33%sct(mf33%nl),stat=n)
    if(n .ne. 0) call endf_badal

    do i = 1,mf33%nl

        sc => mf33%sct(i)
        call read_endf(sc%xmf1, sc%xlfs1, sc%mat1, sc%mt1, sc%nc, sc%ni)

        allocate(sc%ncs(sc%nc),stat=n)
        if(n .ne. 0) call endf_badal
        do j = 1,sc%nc
            call read_nc(sc%ncs(j))
        end do

        allocate(sc%nis(sc%ni),stat=n)
        if(n .ne. 0) call endf_badal
        do j = 1,sc%ni
            call read_ni(sc%nis(j),33)
        end do

    end do

    return
    end subroutine read_mf33

!------------------------------------------------------------------------------

    subroutine write_mf33(mf33)

    implicit none

    type (mf_33), intent(in), target :: mf33

    integer i,j
    type (MF33_sect), pointer :: sc

    call set_mt(mf33%mt)
    call write_endf(mf33%za, mf33%awr, 0, mf33%mtl, 0, mf33%nl)

    if((mf33%mtl .ne. 0) .and. (mf33%nl .ne. 0)) then
        ! MTL .ne. 0 indicates lumped covars and NL == 0
        write(erlin,*) 'Non-zero NL found with non-zero MTL in MF33:',mf33%nl
        call endf_error(erlin)
    end if

    do i = 1,mf33%nl
        sc => mf33%sct(i)
        call write_endf(sc%xmf1, sc%xlfs1, sc%mat1, sc%mt1, sc%nc, sc%ni)
        do j = 1,sc%nc
            call write_nc(sc%ncs(j))
        end do
        do j = 1,sc%ni
            call write_ni(sc%nis(j),33)
        end do
    end do

    call write_send

    return
    end subroutine write_mf33

!------------------------------------------------------------------------------

    subroutine del_mf33(mf33)

    implicit none

    type (mf_33), target :: mf33

    integer i,j,n

    do i = 1,mf33%nl
        do j = 1,mf33%sct(i)%nc
            call del_nc(mf33%sct(i)%ncs(j))
        end do
        deallocate(mf33%sct(i)%ncs,stat=n)
        do j = 1,mf33%sct(i)%ni
            call del_ni(mf33%sct(i)%nis(j))
        end do
        deallocate(mf33%sct(i)%nis,stat=n)
    end do

    return
    end subroutine del_mf33

!------------------------------------------------------------------------------

    integer function lc_mf33(mf33)

    implicit none

    type (mf_33), target :: mf33

    integer i,j,l
    type (MF33_sect), pointer :: sc

    l = 1

    if((mf33%mtl .ne. 0) .and. (mf33%nl .ne. 0)) then
        ! MTL .ne. 0 indicates lumped covars and NL == 0
        write(erlin,*) 'Non-zero NL found with non-zero MTL in MF33:',mf33%nl
        call endf_error(erlin)
    end if

    do i = 1,mf33%nl
        sc => mf33%sct(i)
        l = l + 1
        do j = 1,sc%nc
            l = l + lc_nc(sc%ncs(j))
        end do
        do j = 1,sc%ni
            l = l + lc_ni(sc%nis(j),33)
        end do
    end do

    lc_mf33 = l

    return
    end function lc_mf33

end module ENDF_MF33_IO
