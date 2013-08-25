module ENDF_MF34_IO

    use base_endf_io
    use endf_cov_io

    ! author: Sam Hoblit, NNDC, BNL
    ! provide I/O functions for MF34

    implicit none

    ! ~~~~~~~ Covariances for angular distributions of secondary particles ~~~~~~~~~~

    public

    type MF34_subsect
        integer l                       ! legendre coef for this MT
        integer l1                      ! legendre coef for MT1
        integer lct                     ! ref frame flag
        integer ni                      ! # list records
        type (ni_cov_sect), pointer :: lst(:)      ! covariance list
    end type

    type MF34_sect
        integer mat1                    ! MAT for 2nd E-dep crs
        integer mt1                     ! MT for 2nd E-dep crs
        integer nl                      ! # Legendre coefs given for MT
        integer nl1                     ! # Legendre coefs for MT1
        type (MF34_subsect), pointer :: ssc(:,:)   ! sub-sections (nl,nl1)
    end type

    type MF_34
        type (MF_34), pointer :: next
        integer mt
        real za
        real awr
        integer ltt                     ! representation flag (1,2 or 3)
        integer nmt1                    ! number of sub-sections with MT1 .ge. MT
        type (MF34_sect), pointer :: sct(:)        ! sections (nmt1)
    end type

    private read_mf34_ni, write_mf34_ni

!------------------------------------------------------------------------------
    contains
!------------------------------------------------------------------------------

    subroutine read_mf34(mf34)

    implicit none

    type (mf_34), intent(out), target :: mf34

    integer i,k,k1,l,n
    real xx
    type (MF34_sect), pointer :: sc

    call get_endf(mf34%za, mf34%awr, n, mf34%ltt, n, mf34%nmt1)
    allocate(mf34%sct(mf34%nmt1),stat=n)
    if(n .ne. 0) call endf_badal

    do i = 1,mf34%nmt1

        sc => mf34%sct(i)
        call read_endf(xx, xx, sc%mat1, sc%mt1, sc%nl, sc%nl1)
        allocate(sc%ssc(sc%nl,sc%nl1),stat=n)
        if(n .ne. 0) call endf_badal

        do l = 1,sc%nl
            if(sc%mt1 .eq. mf34%mt) then
                k1 = l        ! symmetric
            else
                k1 = 1        ! asymmetrix
            endif
            do k = k1, sc%nl1
                call read_mf34_ni(sc%ssc(l,k))
            end do
        end do

    end do

    return
    end subroutine read_mf34

!------------------------------------------------------------------------------

    subroutine read_mf34_ni(nis)

    implicit none

    type (MF34_subsect), intent(out) :: nis

    integer i,n

    call read_endf(nis%l, nis%l1, nis%lct, nis%ni)
    allocate(nis%lst(nis%ni),stat=n)
    if(n .ne. 0) call endf_badal

    do i = 1, nis%ni
        call read_ni(nis%lst(i),34)
        select case(nis%lst(i)%lb)
        case(0:2,5,6)
            ! allowed
        case default
            write(erlin,*) 'Undefined LB encountered in MF34 : ',nis%lst(i)%lb
            call endf_error(erlin)
        end select
    end do

    return
    end subroutine read_mf34_ni

!***********************************************************************************

    subroutine write_mf34(mf34)

    implicit none

    type (mf_34), intent(in), target :: mf34

    integer i,k,k1,l
    type (MF34_sect), pointer :: sc

    call set_mt(mf34%mt)
    call write_endf(mf34%za, mf34%awr, 0, mf34%ltt, 0, mf34%nmt1)
    do i = 1,mf34%nmt1
        sc => mf34%sct(i)
        call write_endf(sc%mat1, sc%mt1, sc%nl, sc%nl1)
        do l = 1,sc%nl
            if(sc%mt1 .eq. mf34%mt) then
                k1 = l        ! symmetric
            else
                k1 = 1        ! asymmetrix
            endif
            do k = k1, sc%nl1
                call write_mf34_ni(sc%ssc(l,k))
            end do
        end do
    end do
    call write_send

    return
    end subroutine write_mf34

!------------------------------------------------------------------------------

    subroutine write_mf34_ni(nis)

    implicit none

    type (MF34_subsect), intent(in) :: nis

    integer i

    call write_endf(nis%l, nis%l1, nis%lct, nis%ni)
    do i = 1, nis%ni
        select case(nis%lst(i)%lb)
        case(0:2,5,6)    ! allowed
            call write_ni(nis%lst(i),34)
        case default
            write(erlin,*) 'Undefined LB encountered in MF34 : ',nis%lst(i)%lb
            call endf_error(erlin)
        end select
    end do

    return
    end subroutine write_mf34_ni

!***********************************************************************************

    subroutine del_mf34(mf34)

    implicit none

    type (mf_34), target :: mf34

    integer i,k,k1,l,n,m

    do i = 1,mf34%nmt1
        do l = 1,mf34%sct(i)%nl
            if(mf34%sct(i)%mt1 .eq. mf34%mt) then
                k1 = l        ! symmetric
            else
                k1 = 1        ! asymmetrix
            endif
            do k = k1, mf34%sct(i)%nl1
                do n = 1, mf34%sct(i)%ssc(l,k)%ni
                    call del_ni(mf34%sct(i)%ssc(l,k)%lst(n))
                end do
                deallocate(mf34%sct(i)%ssc(l,k)%lst,stat=m)
            end do
        end do
        deallocate(mf34%sct(i)%ssc,stat=m)
    end do
    deallocate(mf34%sct,stat=m)

    return
    end subroutine del_mf34

!***********************************************************************************

    integer function lc_mf34(mf34)

    implicit none

    type (mf_34), intent(in), target :: mf34

    integer i,j,k,k1,l,m
    type (MF34_sect), pointer :: sc

    l = 1
    do i = 1,mf34%nmt1
        sc => mf34%sct(i)
        l = l + 1
        do j = 1,sc%nl
            if(sc%mt1 .eq. mf34%mt) then
                k1 = j        ! symmetric
            else
                k1 = 1        ! asymmetrix
            endif
            do k = k1, sc%nl1
                l = l + 1
                do m = 1, sc%ssc(j,k)%ni
                    select case(sc%ssc(j,k)%lst(m)%lb)
                    case(0:2,5,6)    ! allowed
                        l = l + lc_ni(sc%ssc(j,k)%lst(m),34)
                    case default
                        write(erlin,*) 'Undefined LB encountered in MF34 : ',sc%ssc(j,k)%lst(m)%lb
                        call endf_error(erlin)
                    end select
                end do
            end do
        end do
    end do

    lc_mf34 = l

    return
    end function lc_mf34

end module ENDF_MF34_IO
