module ENDF_MF35_IO

    use base_endf_io

    ! author: Sam Hoblit, NNDC, BNL
    ! provide I/O functions for MF35

    implicit none

    ! ------  Covariances for energy distributions of secondary particles -----------

    public

    ! note that we can't use the general NI covariance type here.
    ! MF35 includes E1 & E2 in the NI section, while the others do not.

    type MF35_list
        real e1                            ! lowest  incident neutron energy (eV)
        real e2                            ! highest incident neutron energy (eV)
        integer ls                         ! symm matrix == 1 always
        integer lb                         ! LB type == 7 always
        integer nt                         ! number of data items in list
        integer ne                         ! number of energies 
        real, pointer :: ek(:)             ! outgoing energy bins
        real, pointer :: cov(:,:)          ! covariance array 
    end type

    type MF_35
        type (MF_35), pointer :: next
        integer mt
        real za
        real awr
        integer nk                         ! number of sub-sections
        type (MF35_list), pointer :: sct(:)   ! sections (nk)
    end type

    private read_mf35_list, write_mf35_list

!------------------------------------------------------------------------------
    contains
!------------------------------------------------------------------------------

    subroutine read_mf35(mf35)

    implicit none

    type (mf_35), intent(out), target :: mf35

    integer i,n

    call get_endf(mf35%za, mf35%awr, n, n, mf35%nk, n)
    allocate(mf35%sct(mf35%nk),stat=n)
    if(n /= 0) call endf_badal
    do i = 1,mf35%nk
        call read_mf35_list(mf35%sct(i))
    end do

    return
    end subroutine read_mf35

!------------------------------------------------------------------------------

    subroutine read_mf35_list(lst)

    implicit none

    integer n

    type (MF35_list), intent(out) :: lst

    call read_endf(lst%e1, lst%e2, lst%ls, lst%lb, lst%nt, lst%ne)

    if(lst%ls .ne. 1) then
        write(erlin,*) 'Undefined value for LS found in MF35:',lst%ls
        call endf_error(erlin)
    endif

    if(lst%lb .ne. 7) then
        write(erlin,*) 'Undefined value for LB found in MF35:',lst%lb
        call endf_error(erlin)
    endif

    if(lst%nt .ne. (lst%ne*(lst%ne+1)/2)) then
        write(erlin,*) 'Inconsistent NT, NE in MF35 matrix:',lst%nt,lst%ne
        call endf_error(erlin)
    endif

    allocate(lst%ek(lst%ne), lst%cov(lst%ne-1,lst%ne-1),stat=n)
    if(n .ne. 0) call endf_badal
    call read_endf(lst%ek,lst%ne)
    call get_endf(lst%cov,lst%ne-1)

    return
    end subroutine read_mf35_list

!******************************************************************************

    subroutine write_mf35(mf35)

    implicit none

    type (mf_35), intent(in), target :: mf35

    integer i

    call set_mt(mf35%mt)
    call write_endf(mf35%za, mf35%awr, 0, 0, mf35%nk, 0)
    do i = 1,mf35%nk
        call write_mf35_list(mf35%sct(i))
    end do
    call write_send

    return
    end subroutine write_mf35

!------------------------------------------------------------------------------

    subroutine write_mf35_list(lst)

    implicit none

    type (MF35_list), intent(in) :: lst

    if(lst%ls .ne. 1) then
        write(erlin,*) 'Undefined value for LS found in MF35:',lst%ls
        call endf_error(erlin)
    endif

    if(lst%lb .ne. 7) then
        write(erlin,*) 'Undefined value for LB found in MF35:',lst%lb
        call endf_error(erlin)
    endif

    call write_endf(lst%e1, lst%e2, lst%ls, lst%lb, (lst%ne*(lst%ne+1))/2, lst%ne)
    call write_endf(lst%ek,lst%ne)
    call put_endf(lst%cov,lst%ne-1)

    return
    end subroutine write_mf35_list

!******************************************************************************

    subroutine del_mf35(mf35)

    implicit none

    type (mf_35) mf35

    integer i,n

    do i = 1,mf35%nk
        deallocate(mf35%sct(i)%ek, mf35%sct(i)%cov,stat=n)
    end do
    deallocate(mf35%sct,stat=n)

    return
    end subroutine del_mf35

!******************************************************************************

    integer function lc_mf35(mf35)

    implicit none

    type (mf_35), intent(in) :: mf35

    integer i,l

    l = 1
    do i = 1,mf35%nk
        if(mf35%sct(i)%ls .ne. 1) then
            write(erlin,*) 'Undefined value for LS found in MF35:',mf35%sct(i)%ls
            call endf_error(erlin)
        endif
        if(mf35%sct(i)%lb .ne. 7) then
            write(erlin,*) 'Undefined value for LB found in MF35:',mf35%sct(i)%lb
            call endf_error(erlin)
        endif
        l = l + ((mf35%sct(i)%ne*(mf35%sct(i)%ne+1))/2+5)/6 + 1
    end do

    lc_mf35 = l

    return
    end function lc_mf35

end module ENDF_MF35_IO
