module ENDF_MF7_IO

    use endf_iolib

    ! author: Sam Hoblit, NNDC, BNL
    ! provide I/O functions for MF7

    implicit none

    ! ~~~~~~~~~~~~~~ Thermal neutron scattering law data ~~~~~~~~~~~~~~~~~~~~~~~~~

    public

    type mf7_coh_elas
        integer nr                              !! # NR E-interpolation ranges
        type (int_pair),  pointer :: ine(:)     !! E-interp tables (nr)
        integer np                              !! # Bragg-edge energies
        real, pointer :: e(:)                   !! Bragg energies (np)
        integer lt                              !! # temps
        integer, pointer :: li(:)               !! temp interpolation (lt) btw i & (i-1) bins
        real, pointer :: t(:)                   !! temps (K) (0:lt)
        real, pointer :: s(:,:)                 !! Bragg edges (np,0:lt)
    end type

    type mf7_incoh_elas
        real sb                                 !! bound cross section (b)
        type (tab1) W                           !! Debye-Waller integrals
    end type

    type mf7_incoh_inelas
        integer lat                             !! temp flag:0=actual temp, 1=zero253 eV
        integer lasym                           !! symmetry flag:0=symm, 1=asymm
        integer lln                             !! flag of form of S:0=direct,1=ln(s)
        integer ni                              !! # items in b list
        integer ns                              !! # non-principal scat atom types
        real, pointer :: b(:)                   !! list of constants (ni)
        integer nrb                             !! # beta interpolation ranges
        type (int_pair), pointer :: inb(:)      !! beta interpolation table (nrb)
        integer nb                              !! # beta values
        real, pointer :: beta(:)                !! beta values (nb)
        integer lt                              !! # temp values
        integer, pointer :: li(:)               !! temp interpolation (lt) btw i & (i-1) bins
        real, pointer :: temp(:)                !! temperature values (lt)
        integer nra                             !! # alpha interpolation ranges
        type (int_pair), pointer :: ina(:)      !! alpha interpolation table (nra)
        integer np                              !! # alpha values
        real, pointer :: alpha(:)               !! alpha values (np)
        real, pointer :: s(:,:,:)               !! neutron scat law, (np,nb,lt)
        type (tab1), pointer :: tef(:)          !! table for Teff0
    end type

    type MF_7
        type (mf_7), pointer :: next            !! next section
        integer mt                              !! MT
        real za                                 !! ZA for material
        real awr                                !! AWR for material
        integer lthr                            !! coherent flag for MT=2; 0 for MT=4
        type(mf7_coh_elas),     pointer :: cel  !! coherent elastic scattering, MT=2
        type(mf7_incoh_elas),   pointer :: iel  !! incoherent elastic scattering, MT=2
        type(mf7_incoh_inelas), pointer :: iin  !! incoherent inelastic scattering, MT=4
    end type

    !---------------------  private ---------------------------------------------

    private read_coh_elas, read_incoh_elas, read_incoh_inelas
    private write_coh_elas, write_incoh_elas, write_incoh_inelas

!------------------------------------------------------------------------------
    contains
!------------------------------------------------------------------------------

    subroutine read_mf7(mf7)

    implicit none

    type (mf_7), intent(out) :: mf7

    integer n,n1,n2

    call get_endf(mf7%za, mf7%awr, mf7%lthr, n1, n2, n)

    nullify(mf7%cel, mf7%iel, mf7%iin)

    if(mf7%mt == 2) then
        if(mf7%lthr == 1) then
            allocate(mf7%cel)
            call read_coh_elas(mf7%cel)
        else if(mf7%lthr == 2) then
            allocate(mf7%iel)
            call read_incoh_elas(mf7%iel)
        else
            write(erlin,*) 'Unrecognized MF7/MT2 LTHR:',mf7%lthr
            call endf_error(erlin)
        end if
    else if(mf7%mt == 4) then
        allocate(mf7%iin)
        mf7%iin%lat = n1
        mf7%iin%lasym = n2
        call read_incoh_inelas(mf7%iin)
    else
        write(erlin,*) 'Undefined MT found in MF7 :',mf7%mt
        call endf_error(erlin)
    end if

    return
    end subroutine read_mf7

!------------------------------------------------------------------------------

    subroutine read_coh_elas(r7)

    implicit none

    type (mf7_coh_elas), intent(out) :: r7

    integer i,np,n
    real xx,xt
    real, allocatable :: xp(:)

    call read_endf(xt, xx, r7%lt, n, r7%nr, r7%np)
    allocate(r7%ine(r7%nr), r7%e(r7%np), r7%li(r7%lt), r7%t(0:r7%lt), r7%s(r7%np,0:r7%lt), stat=n)
    if(n /= 0) call endf_badal
    call read_endf(r7%ine, r7%nr)
    r7%t(0) = xt

    ! first t is a tab1, which contains the E & 0th S

    allocate(xp(2*r7%np),stat=n)
    if(n /= 0) call endf_badal
    call read_endf(xp,2*r7%np)
    do i = 1,r7%np
        r7%e(i)   = xp(2*i-1)
        r7%s(i,0) = xp(2*i)
    end do
    deallocate(xp)

    ! read in the rest of the S as lists

    do i = 1,r7%lt
        call read_endf(r7%t(i), xx, r7%li(i), n, np, n)
        if(np /= r7%np) call endf_error('Inconsistent values for NP encounted in coherent MF7/MT2')
        call read_endf(r7%s(1:r7%np,i),r7%np)
    end do

    return
    end subroutine read_coh_elas

!------------------------------------------------------------------------------

    subroutine read_incoh_elas(ie)

    implicit none

    type (mf7_incoh_elas), intent(out) :: ie

    integer n
    real xx

    call read_endf(ie%sb, xx, n, n, ie%w%nr, ie%w%np)
    call read_endf(ie%w)

    return
    end subroutine read_incoh_elas

!------------------------------------------------------------------------------

    subroutine read_incoh_inelas(ic)

    implicit none

    type (mf7_incoh_inelas), intent(out) :: ic

    integer i,j,n,nra,np,lt
    real xt,beta
    real, allocatable :: xp(:)

    call read_endf(ic%lln, n, ic%ni, ic%ns)
    if(ic%ni /= 6*(ic%ns+1)) then
        write(erlin,*) 'Inconsistent values for NI, NS encountered in MF7/MT4:',ic%ni,ic%ns
        call endf_error(erlin)
    endif

    allocate(ic%b(ic%ni),stat=n)
    if(n /= 0) call endf_badal
    call read_endf(ic%b,ic%ni)

    call read_endf(n, n, ic%nrb, ic%nb)
    allocate(ic%inb(ic%nrb),ic%beta(ic%nb),stat=n)
    if(n /= 0) call endf_badal
    call read_endf(ic%inb, ic%nrb)

    do i = 1,ic%nb

        call read_endf(xt, ic%beta(i), lt, n, nra, np)

        if(i == 1) then
            allocate(ic%temp(0:lt),ic%li(lt),ic%ina(nra),xp(2*np),ic%alpha(np),ic%s(np,ic%nb,0:lt),stat=n)
            if(n /= 0) call endf_badal
            ic%lt = lt
            ic%np = np
            ic%nra = nra
            ic%temp(0) = xt
        else
            if(lt /= ic%lt) call endf_error('Inconsistent values for LT encountered in MF7/MT4')
            if(np /= ic%np) call endf_error('Inconsistent values for NP encountered in MF7/MT4')
            if(nra /= ic%nra) call endf_error('Inconsistent values for alpha-NR encountered in MF7/MT4')
            if(xt /= ic%temp(0)) then
                write(erlin,*) 'Inconsistent temperatures encountered in MF7/MT4:',xt,ic%temp(0)
                call endf_error(erlin)
            endif
        end if

        ! we will assume all alpha tables & values are
        ! the same -> just re-read them for each beta loop

        call read_endf(ic%ina, ic%nra)
        call read_endf(xp,2*ic%np)
        do j = 1,ic%np
            ic%alpha(j) = xp(2*j-1)
            ic%s(j,i,0) = xp(2*j)
        end do

        ! now read in the temps 1 - lt

        do j = 1,ic%lt
            call read_endf(xt,beta,ic%li(j), n, np, n)
            if(np /= ic%np) call endf_error('Inconsistent values for NP encountered in MF7/MT4')
            if(beta /= ic%beta(i)) call endf_error('Inconsistent values for beta encountered in MF7/MT4')
            if(i == 1) then
                ic%temp(j) = xt
            else
                if(xt /= ic%temp(j)) then
                    write(erlin,*) 'Inconsistent temperatures encountered in MF7/MT4:',xt,ic%temp(0)
                    call endf_error(erlin)
                endif
            end if
            call read_endf(ic%s(1:ic%np,i,j),ic%np)
        end do

    end do

    deallocate(xp)

    ! read in Teff tables

    allocate(ic%tef(0:ic%ns),stat=n)
    if(n /= 0) call endf_badal
    call read_endf(n, n, ic%tef(0)%nr, ic%tef(0)%np)
    call read_endf(ic%tef(0))

    j = 1
    do i = 1,ic%ns
        j = j + 6
        if(ic%ni < j)     exit
        if(ic%b(j) /= zero) cycle
        call read_endf(n, n, ic%tef(i)%nr, ic%tef(i)%np)
        call read_endf(ic%tef(i))
    end do

    return
    end subroutine read_incoh_inelas

!******************************************************************************************

    subroutine write_mf7(mf7)

    implicit none

    type (mf_7), intent(in) :: mf7

    call set_mt(mf7%mt)

    if(mf7%mt == 2) then
        call write_endf(mf7%za, mf7%awr, mf7%lthr, 0, 0, 0)
        if(mf7%lthr == 1) then
            call write_coh_elas(mf7%cel)
        else if(mf7%lthr == 2) then
            call write_incoh_elas(mf7%iel)
        else
            write(erlin,*) 'Undefined LTHR MF7/MT2:',mf7%lthr
            call endf_error(erlin)
        end if
    else if(mf7%mt == 4) then
        call write_endf(mf7%za, mf7%awr, mf7%lthr, mf7%iin%lat, mf7%iin%lasym, 0)
        call write_incoh_inelas(mf7%iin)
    else
        write(erlin,*) 'Undefined MT found in MF7 :',mf7%mt
        call endf_error(erlin)
    end if

    call write_send

    return
    end subroutine write_mf7

!------------------------------------------------------------------------------

    subroutine write_coh_elas(r7)

    implicit none

    type (mf7_coh_elas), intent(in) :: r7

    integer i,n
    real, allocatable :: xp(:)

    call write_endf(r7%t(0), zero, r7%lt, 0, r7%nr, r7%np)
    call write_endf(r7%ine, r7%nr)

    ! first t is a tab1, which contains the E & 0th S

    allocate(xp(2*r7%np),stat=n)
    if(n /= 0) call endf_badal
    do i = 1,r7%np
        xp(2*i-1) = r7%e(i)
        xp(2*i) = r7%s(i,0)
    end do
    call write_endf(xp,2*r7%np)
    deallocate(xp)

    ! write in the rest of the S as lists

    do i = 1,r7%lt
        call write_endf(r7%t(i), zero, r7%li(i), 0, r7%np, 0)
        call write_endf(r7%s(1:r7%np,i),r7%np)
    end do

    return
    end subroutine write_coh_elas

!------------------------------------------------------------------------------

    subroutine write_incoh_elas(ie)

    implicit none

    type (mf7_incoh_elas), intent(in) :: ie

    call write_endf(ie%sb, zero, 0, 0, ie%w%nr, ie%w%np)
    call write_endf(ie%w)

    return
    end subroutine write_incoh_elas

!------------------------------------------------------------------------------

    subroutine write_incoh_inelas(ic)

    implicit none

    type (mf7_incoh_inelas), intent(in) :: ic

    integer i,j,n
    real, allocatable :: xp(:)

    if(ic%ni /= 6*(ic%ns+1)) then
        write(erlin,*) 'Inconsistent values for NI, NS encountered in MF7/MT4:',ic%ni,ic%ns
        call endf_error(erlin)
    endif

    call write_endf(ic%lln, 0, ic%ni, ic%ns)
    call write_endf(ic%b,ic%ni)
    call write_endf(0, 0, ic%nrb, ic%nb)
    call write_endf(ic%inb, ic%nrb)

    allocate(xp(2*ic%np),stat=n)
    if(n /= 0) call endf_badal

    do i = 1,ic%nb

        ! the first record has temp(0) and the alpha values as well
        call write_endf(ic%temp(0), ic%beta(i), ic%lt, 0, ic%nra, ic%np)
        call write_endf(ic%ina, ic%nra)

        do j = 1,ic%np
            xp(2*j-1) = ic%alpha(j)
            xp(2*j) = ic%s(j,i,0)
        end do
        call write_endf(xp,2*ic%np)

        ! write temps 1-lt as simple lists
        do j = 1,ic%lt
            call write_endf(ic%temp(j),ic%beta(i),ic%li(j), 0, ic%np, 0)
            call write_endf(ic%s(1:ic%np,i,j),ic%np)
        end do

    end do

    deallocate(xp)

    ! now write in Teff tables

    call write_endf(0, 0, ic%tef(0)%nr, ic%tef(0)%np)
    call write_endf(ic%tef(0))

    j = 1
    do i = 1,ic%ns
        j = j + 6
        if(ic%ni < j)     exit
        if(ic%b(j) /= zero) cycle
        call write_endf(0, 0, ic%tef(i)%nr, ic%tef(i)%np)
        call write_endf(ic%tef(i))
    end do

    return
    end subroutine write_incoh_inelas

!******************************************************************************************

    subroutine del_mf7(mf7)

    implicit none

    type (mf_7) :: mf7

    integer i,n

    if(associated(mf7%cel)) then
        deallocate(mf7%cel%ine, mf7%cel%e, mf7%cel%li, mf7%cel%t, mf7%cel%s,stat=n)
        deallocate(mf7%cel,stat=n)
    else if(associated(mf7%iel)) then
        call del_tab1(mf7%iel%w)
        deallocate(mf7%iel,stat=n)
    else if(associated(mf7%iin)) then
        deallocate(mf7%iin%temp, mf7%iin%li, mf7%iin%ina, mf7%iin%b,stat=n)
        deallocate(mf7%iin%alpha, mf7%iin%s, mf7%iin%inb, mf7%iin%beta,stat=n)
        do i = 0,mf7%iin%ns
            if(mf7%iin%tef(i)%nr > 0) call del_tab1(mf7%iin%tef(i))
        end do
        deallocate(mf7%iin%tef,stat=n)
        deallocate(mf7%iin,stat=n)
    end if

    return
    end subroutine del_mf7

!******************************************************************************************

    integer function lc_mf7(mf7)

    implicit none

    type (mf_7), intent(in) :: mf7

    integer i,j,l

    if(mf7%mt == 2) then
        l = 1
        if(mf7%lthr == 1) then
            l = l + (2*mf7%cel%nr+5)/6 + (2*mf7%cel%np+5)/6 + 1
            l = l + mf7%cel%lt*((mf7%cel%np+5)/6 + 1)
        else if(mf7%lthr == 2) then
            l = l + lc_tab1(mf7%iel%w) + 1
        else
            write(erlin,*) 'Undefined LTHR MF7/MT2:',mf7%lthr
            call endf_error(erlin)
        end if
    else if(mf7%mt == 4) then
        if(mf7%iin%ni /= 6*(mf7%iin%ns+1)) then
            write(erlin,*) 'Inconsistent values for NI, NS encountered in MF7/MT4:',mf7%iin%ni,mf7%iin%ns
            call endf_error(erlin)
        endif
        l = (mf7%iin%ni+5)/6 + (2*mf7%iin%nrb+5)/6 + 3
        do i = 1,mf7%iin%nb
            l = l + (2*mf7%iin%nra+5)/6 + (2*mf7%iin%np+5)/6 + 1
            l = l + mf7%iin%lt*((mf7%iin%np+5)/6 + 1)
        end do

        l = l + lc_tab1(mf7%iin%tef(0)) + 1
        j = 1
        do i = 1,mf7%iin%ns
            j = j + 6
            if(mf7%iin%ni < j)     exit
            if(mf7%iin%b(j) /= zero) cycle
            l = l + lc_tab1(mf7%iin%tef(i)) + 1
        end do
    else
        write(erlin,*) 'Undefined MT found in MF7 :',mf7%mt
        call endf_error(erlin)
    end if

    lc_mf7 = l

    return
    end function lc_mf7

end module ENDF_MF7_IO
