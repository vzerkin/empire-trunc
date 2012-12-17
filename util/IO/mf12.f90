module ENDF_MF12_IO

    use base_endf_io

    ! author: Sam Hoblit, NNDC, BNL
    ! provide I/O functions for MF12

    implicit none

    !---------- Photon production multiplicities and transition probability arrays ---------

    public

    type mf12_photon
        real eg                        ! photon (LP=0,1) or binding (LP=2) energy (eV)
        real es                        ! energy of level from which photon originates
        integer lp                     ! primary flag
        integer lf                     ! dist law # (1 or 2)
        type (tab1) yld                ! yields (multiplicities) for photon. E=x, Y=y
    end type

    type mf12_tran_prob
        sequence
        real es                        ! energy of level
        real tp                        ! prob of direct transition
        real gp                        ! prob of photon trans (only if lg=2)
    end type

    type mf12_prob_array
        integer lg                     ! case flag. 1=simple, 2=complex
        integer lp                     ! 
        integer nt                     ! # transitions
        real esp                       ! energy of present state (es(ns))
        type (mf12_tran_prob), pointer :: tpb(:)    ! trans probabilities
    end type

    type MF_12
        type (mf_12), pointer :: next  ! next section
        integer mt                     ! MT
        real za                        ! ZA for material
        real awr                       ! AWR for material
        integer lo                     ! type flag. 1=multiplicities, 2=trans prob
        integer nk                     ! # discrete photons incl continuum
        integer ns                     ! # discrete levels below current one
        type (tab1), pointer :: tyld   ! total yield
        type (mf12_photon), pointer :: gam(:)     ! discrete photons
        type (mf12_prob_array), pointer :: par    ! transition probability
    end type

!---------------------------------------------------------------------------------------------
    contains
!---------------------------------------------------------------------------------------------

    subroutine read_mf12(mf12)

    implicit none

    type (mf_12), intent(out), target :: mf12

    integer i,n,lg,np,m
    real xx

    type (mf12_photon), pointer :: gm
    type (mf12_tran_prob), pointer :: tp

    call get_endf(mf12%za, mf12%awr, mf12%lo, lg, m, n)

    mf12%nk = 0
    mf12%ns = 0
    nullify(mf12%tyld, mf12%gam, mf12%par)

    select case(mf12%lo)
    case(1)
        mf12%nk = m
        if(mf12%nk .gt. 1) then
            allocate(mf12%tyld)
            call read_endf(n, n, mf12%tyld%nr, mf12%tyld%np)
            call read_endf(mf12%tyld)
        else
            nullify(mf12%tyld)
        endif
        allocate(mf12%gam(mf12%nk),stat=n)
        if(n .ne. 0) call endf_badal
        do i = 1,mf12%nk
            gm => mf12%gam(i)
            call read_endf(gm%eg, gm%es, gm%lp, gm%lf, gm%yld%nr, gm%yld%np)
            call read_endf(gm%yld)
        end do
    case(2)
        mf12%ns = m
        allocate(mf12%par)
        mf12%par%lg = lg
        if((lg .lt. 1) .or. (lg .gt. 2)) then
            write(erlin,*) 'Undefined value for LG in MF12:',lg
            call endf_error(erlin)
        endif
        call read_endf(mf12%par%esp, xx, mf12%par%lp, n, np, mf12%par%nt)
        allocate(mf12%par%tpb(mf12%par%nt),stat=n)
        if(n .ne. 0) call endf_badal
        do i = 1,mf12%par%nt
            tp => mf12%par%tpb(i)
            call get_endf(tp%es)
            call get_endf(tp%tp)
            if(lg .gt. 1) then
                call get_endf(tp%gp)
            else
                tp%gp = zero
            endif
        end do
    case default
        write(erlin,*) 'Undefined value of LO found in MF12:',mf12%lo
        call endf_error(erlin)
    end select

    return
    end subroutine read_mf12

!---------------------------------------------------------------------------------------------

    subroutine write_mf12(mf12)

    implicit none

    type (mf_12), intent(in), target :: mf12

    integer i
    type (mf12_photon), pointer :: gm
    type (mf12_tran_prob), pointer :: tp

    call set_mt(mf12%mt)

    select case(mf12%lo)
    case(1)
        call write_endf(mf12%za, mf12%awr, mf12%lo, 0, mf12%nk, 0)
        if(mf12%nk .gt. 1) then
            call write_endf(0, 0, mf12%tyld%nr, mf12%tyld%np)
            call write_endf(mf12%tyld)
        endif
        do i = 1,mf12%nk
            gm => mf12%gam(i)
            call write_endf(gm%eg, gm%es, gm%lp, gm%lf, gm%yld%nr, gm%yld%np)
            call write_endf(gm%yld)
        end do
    case(2)
        if((mf12%par%lg .lt. 1) .or. (mf12%par%lg .gt. 2)) then
            write(erlin,*)  'Undefined value for LG in MF12:',mf12%par%lg
            call endf_error(erlin)
        endif
        call write_endf(mf12%za, mf12%awr, mf12%lo, mf12%par%lg, mf12%ns, 0)
        call write_endf(mf12%par%esp, zero, mf12%par%lp, 0, (mf12%par%lg+1)*mf12%par%nt, mf12%par%nt)
        do i = 1,mf12%par%nt
            tp => mf12%par%tpb(i)
            call put_endf(tp%es)
            call put_endf(tp%tp)
            if(mf12%par%lg .gt. 1) call put_endf(tp%gp)
        end do
    case default
        write(erlin,*) 'Undefined value of LO found in MF12:',mf12%lo
        call endf_error(erlin)
    end select

    call write_send

    return
    end subroutine write_mf12

!---------------------------------------------------------------------------------------------

    subroutine del_mf12(mf12)

    implicit none

    type (mf_12), target :: mf12

    integer i,n

    if(associated(mf12%tyld)) call remove_tab1(mf12%tyld)

    if(associated(mf12%gam)) then
        do i = 1,mf12%nk
            call del_tab1(mf12%gam(i)%yld)
        end do
        deallocate(mf12%gam,stat=n)
    endif

    if(associated(mf12%par)) then
        deallocate(mf12%par%tpb,stat=n)
        deallocate(mf12%par,stat=n)
    endif

    return
    end subroutine del_mf12

!---------------------------------------------------------------------------------------------

    integer function lc_mf12(mf12)

    implicit none

    type (mf_12), intent(in) :: mf12

    integer i,l

    select case(mf12%lo)
    case(1)
        l = 1
        if(mf12%nk .gt. 1) then
            l = l + lc_tab1(mf12%tyld) + 1
        endif
        do i = 1,mf12%nk
            l = l + lc_tab1(mf12%gam(i)%yld) + 1
        end do
    case(2)
        l = ((mf12%par%lg+1)*mf12%par%nt+5)/6 + 2
    case default
        write(erlin,*) 'Undefined value of LO found in MF12:',mf12%lo
        call endf_error(erlin)
    end select

    lc_mf12 = l

    return
    end function lc_mf12

end module ENDF_MF12_IO
