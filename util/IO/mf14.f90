module ENDF_MF14_IO

    use endf_iolib

    ! author: Sam Hoblit, NNDC, BNL
    ! provide I/O functions for MF14

    implicit none

    !--------------------- Photon production cross sections ----------------------------

    public

    type mf14_isotropic_dist
        real eg                        !! photon energy (eV)
        real es                        !! energy of level from which photon originates
    end type

    type mf14_legendre_dist
        real e                         !! neutron energy
        integer nl                     !! # Legendre coefs
        real, pointer :: a(:)          !! Legendre coefs (a(0)==1)
    end type!

    type mf14_tabular_dist
        real e                         !! neutron energy
        type (tab1) mut                !! x=cos(theta)=mu, y=prob at mu
    end type

    type mf14_anisotropic_dist
        real eg                        !! photon energy (eV)
        real es                        !! energy of level from which photon originates
        integer nr                     !! # interpolation regions
        type (int_pair), pointer :: inb(:)   !! indident E interpolation table
        integer ne                     !! # incident neutron energies
        type (mf14_legendre_dist), pointer :: leg(:)   !! Legendre coefs
        type (mf14_tabular_dist), pointer :: tab(:)    !! tables
    end type

    type MF_14
        type (mf_14), pointer :: next  !! next section
        integer mt                     !! MT
        real za                        !! ZA for material
        real awr                       !! AWR for material
        integer li                     !! isotropic flag
        integer ltt                    !! rep flag: 1=Legendre coef, 2=tabular
        integer nk                     !! # discreet photons incl continuum
        integer ni                     !! # isotropic ang dists given
        type (mf14_isotropic_dist), pointer :: isg(:)      !! iostropic photons (ni)
        type (mf14_anisotropic_dist), pointer :: aig(:)    !! aniostropic photons (nk-ni)
    end type

!---------------------------------------------------------------------------------------------
    contains
!---------------------------------------------------------------------------------------------

    subroutine read_mf14(mf14)

    implicit none

    type (mf_14), intent(out), target :: mf14

    integer i,j,n,nl
    real xx
    type (mf14_anisotropic_dist), pointer :: ag
    type (mf14_legendre_dist), pointer :: lg
    type (mf14_tabular_dist), pointer :: tb

    call get_endf(mf14%za, mf14%awr, mf14%li, mf14%ltt, mf14%nk, mf14%ni)

    select case(mf14%li)
    case(0)

        ! ang dist info to read in

        nl = mf14%nk - mf14%ni

        ! nl = # of anisotropic photons. Make sure it makes sense

        if(nl == 0) then
            erlin = ' No ang dist data specified with LTT=0 in MF14'
            call endf_error(erlin,10)
        else if(nl < 0) then
            write(erlin,*) 'NK greater than NI in MF14:',mf14%nk,mf14%ni
            call endf_error(erlin)
        endif

        allocate(mf14%isg(mf14%ni),mf14%aig(nl),stat=n)
        if(n /= 0) call endf_badal
        do i = 1,mf14%ni
            call read_endf(mf14%isg(i)%eg,mf14%isg(i)%es, n, n, n, n)
        end do

        do i = 1,nl

            ag => mf14%aig(i)
            call read_endf(ag%eg, ag%es, n, n, ag%nr, ag%ne)
            allocate(ag%inb(ag%nr),stat=n)
            if(n /= 0) call endf_badal
            call read_endf(ag%inb,ag%nr)

            if(mf14%ltt == 1) then        ! Legendre
                nullify(ag%tab)
                allocate(ag%leg(ag%ne),stat=n)
                if(n /= 0) call endf_badal
                do j = 1, ag%ne
                    lg => ag%leg(j)
                    call read_endf(xx, lg%e, n, n, lg%nl, n)
                    allocate(lg%a(lg%nl),stat=n)
                    if(n /= 0) call endf_badal
                    call read_endf(lg%a,lg%nl)
                end do
            else if(mf14%ltt == 2) then    ! tables
                nullify(ag%leg)
                allocate(ag%tab(ag%ne),stat=n)
                if(n /= 0) call endf_badal
                do j = 1, ag%ne
                    tb => ag%tab(j)
                    call read_endf(xx, tb%e, n, n, tb%mut%nr, tb%mut%np)
                    call read_endf(tb%mut)
                end do
            else
                write(erlin,*) 'Undefined value of LTT encountered in MF14:',mf14%ltt
                call endf_error(erlin)
            endif

        end do

    case(1)

        ! all isotropic - nothing to read in

    case default

        write(erlin,*) 'Unrecognized value of LI encountered in MF14:',mf14%li
        call endf_error(erlin)

    end select

    return
    end subroutine read_mf14

!---------------------------------------------------------------------------------------------

    subroutine write_mf14(mf14)

    implicit none

    type (mf_14), intent(in), target :: mf14

    integer i,j,nl
    type (mf14_anisotropic_dist), pointer :: ag
    type (mf14_legendre_dist), pointer :: lg
    type (mf14_tabular_dist), pointer :: tb

    call set_mt(mf14%mt)
    call write_endf(mf14%za, mf14%awr, mf14%li, mf14%ltt, mf14%nk, mf14%ni)

    select case(mf14%li)
    case(0)

        ! ang dist info to write in

        nl = mf14%nk - mf14%ni

        ! nl = # of anisotropic photons. Make sure it makes sense

        if(nl == 0) then
            erlin = ' No ang dist data specified with LTT=0 in MF14'
            call endf_error(erlin,10)
        else if(nl < 0) then
            write(erlin,*) 'NK greater than NI in MF14:',mf14%nk,mf14%ni
            call endf_error(erlin)
        endif

        do i = 1,mf14%ni
            call write_endf(mf14%isg(i)%eg,mf14%isg(i)%es, 0, 0, 0, 0)
        end do

        do i = 1,nl

            ag => mf14%aig(i)
            call write_endf(ag%eg, ag%es, 0, 0, ag%nr, ag%ne)
            call write_endf(ag%inb,ag%nr)

            if(mf14%ltt == 1) then        ! Legendre
                do j = 1, ag%ne
                    lg => ag%leg(j)
                    call write_endf(zero, lg%e, 0, 0, lg%nl, 0)
                    call write_endf(lg%a,lg%nl)
                end do
            else if(mf14%ltt == 2) then    ! tables
                do j = 1, ag%ne
                    tb => ag%tab(j)
                    call write_endf(zero, tb%e, 0, 0, tb%mut%nr, tb%mut%np)
                    call write_endf(tb%mut)
                end do
            else
                write(erlin,*) 'Unrecognized value of LTT encountered in MF14:',mf14%ltt
                call endf_error(erlin)
            endif

        end do

    case(1)

        ! isotropic - nothing to write

    case default

        write(erlin,*) 'Unrecognized value of LI encountered in MF14:',mf14%li
        call endf_error(erlin)

    end select

    call write_send

    return
    end subroutine write_mf14

!---------------------------------------------------------------------------------------------

    subroutine del_mf14(mf14)

    implicit none

    type (mf_14) mf14

    integer i,j,nl,n

    if(mf14%li == 0) then
        ! ang dist info 
        nl = mf14%nk - mf14%ni
        do i = 1,nl
            deallocate(mf14%aig(i)%inb,stat=n)
            if(associated(mf14%aig(i)%leg)) then
                do j = 1,mf14%aig(i)%ne
                    deallocate(mf14%aig(i)%leg(j)%a,stat=n)
                end do
                deallocate(mf14%aig(i)%leg,stat=n)
            else if(associated(mf14%aig(i)%tab)) then
                do j = 1,mf14%aig(i)%ne
                     call del_tab1(mf14%aig(i)%tab(j)%mut)
                end do
                deallocate(mf14%aig(i)%tab,stat=n)
            endif
        end do
        deallocate(mf14%isg, mf14%aig,stat=n)
    endif

    return
    end subroutine del_mf14

!---------------------------------------------------------------------------------------------

    integer function lc_mf14(mf14)

    implicit none

    type (mf_14), intent(in), target :: mf14

    integer i,j,nl,l
    type (mf14_anisotropic_dist), pointer :: ag

    l = 1
    select case(mf14%li)
    case(0)
        ! ang dist info to write in
        nl = mf14%nk - mf14%ni
        ! nl = # of anisotropic photons. Make sure it makes sense
        if(nl == 0) then
            erlin = ' No ang dist data specified with LTT=0 in MF14'
            call endf_error(erlin,10)
        else if(nl < 0) then
            write(erlin,*) 'NK greater than NI in MF14:',mf14%nk,mf14%ni
            call endf_error(erlin)
        endif

        l = l + mf14%ni

        do i = 1,nl
            ag => mf14%aig(i)
            l = l +(2*ag%nr+5)/6 + 1
            if(mf14%ltt == 1) then        ! Legendre
                do j = 1, ag%ne
                    l = l + (ag%leg(j)%nl+5)/6 + 1
                end do
            else if(mf14%ltt == 2) then    ! tables
                do j = 1, ag%ne
                    l = l + lc_tab1(ag%tab(j)%mut) + 1
                end do
            else
                write(erlin,*) 'Unrecognized value of LTT encountered in MF14:',mf14%ltt
                call endf_error(erlin)
            endif
        end do
    case(1)
        ! isotropic - nothing to write
    case default
        write(erlin,*) 'Unrecognized value of LI encountered in MF14:',mf14%li
        call endf_error(erlin)
    end select

    lc_mf14 = l

    return
    end function lc_mf14

end module ENDF_MF14_IO
