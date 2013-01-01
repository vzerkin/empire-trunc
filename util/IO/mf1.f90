module ENDF_MF1_IO

    use base_endf_io

    ! author: Sam Hoblit, NNDC, BNL
    ! provide I/O functions for MF1

    implicit none

    !---------------------------  MF1 -- General Information ----------------------------

    public

    !~~~~~~~~~~~~~~~~~~~~~~~  MT451  Descriptive Data & Directory ~~~~~~~~~~~~~~~~~~~~~~

    type MF1_sect_list
        integer mf                                ! section MF
        integer mt                                ! section MT
        integer nc                                ! # lines in section
        integer mod                               ! modification number for section.
    end type

    type MF1_451
        integer lrp                               ! flag indicating file2. 0=no,1=yes,2=yes, but don't use
        integer lfi                               ! flag for fission: 0=no, 1=yes.
        integer nlib                              ! library identifier
        integer nmod                              ! modification number
        real elis                                 ! energy of target relative to zero for GS.
        real sta                                  ! flag for stable. zero=stable, 1.0=unstable.
        integer lis                               ! state number of target, lis=0 -> GS.
        integer liso                              ! isomeric state number
        integer nfor                              ! format number (6).
        real awi                                  ! projectile mass in neutron masses
        real emax                                 ! max E in library
        integer lrel                              ! library release number
        integer nsub                              ! sub-library number
        integer nver                              ! library version number
        real temp                                 ! temp for Doppler broadened evals.
        integer ldrv                              ! special derived material flag. 0=primary
	integer mat                               ! MAT number from comment record 3, chars 32:35. should = MAT
        integer irev                              ! revision number from comment record 3, chars 54:56
        integer mfor                              ! format number from comment record 5, char 12. should = nfor
        integer nwd                               ! number of records of descriptive text
        integer nxc                               ! number of records in directory
        character*11 zsymam                       ! char rep of material
        character*11 alab                         ! lab name
        character*10 edate                        ! eval date
        character*33 auth                         ! authors
        character*20 ref                          ! primary reference for eval
        character*10 ddate                        ! orig distribution date
        character*10 rdate                        ! date & number of last revision.
        character*8 endate                        ! NNDC master file entry date.
        character*17 libtyp                       ! library type & version (eg, 'ENDF/B-VII.1')
        character*61 sublib                       ! sub-library identifier, (eg, 'INCIDENT NEUTRON DATA')
        character*66, pointer :: cmnt(:)          ! lines of ascii comments (nwd)
        type (MF1_sect_list), pointer :: dir(:)   ! "directory" of sections (nxc)
    end type

    !~~~~~~~~~~~~~~~~~~~~~~~~~ MT452  nubar = # neutrons/fission ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

    type MF1_452
        integer lnu                               ! flag for poly or table
        integer nc                                ! number of terms in poly
        real, pointer :: c(:)                     ! coefs of polynomial
        type (tab1), pointer :: tb                ! table of values
    end type

    !~~~~~~~~~~~~~~~~~~~~~~~~  MT455 Delayed neutron data ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

    type tab2_455_lamb                            ! E-dep lamda family. Families are not interpolated.
        integer nr                                ! # NR interpolation ranges in E
        integer ne                                ! # energy points
        type (int_pair),  pointer :: itp(:)       ! E interpolation tables (NR)
        real, pointer :: e(:)                     ! interpolation energies dim (NE)
        type (real_pair), pointer :: dgc(:,:)     ! delay group const. x=lamba, y=alpha dim (NE,NFF)
    end type

    type MF1_455
        integer lnu                               ! flag for represenation: 1=poly, 2=table
        integer ldg                               ! flag for E-dep:0=indep, 1=dep
        integer nc                                ! number of terms in poly for nubar
        real, pointer :: c(:)                     ! coefs of polynomial for nubar
        type (tab1), pointer :: tb                ! table of values for nubar
        integer nff                               ! number of precursor families for lambda
        real, pointer :: lambda(:)                ! E-independent lambda for nff families
        type (tab2_455_lamb), pointer :: lb       ! E-dependent families 
    end type

    !~~~~~~~~~~~~~~~~~~~~~~ MT458  Components of energy release from fission ~~~~~~~~~~~~~~~~~~~~~~

    type MF1_458_terms
        sequence
        real efr                                  ! KE of fission products
        real defr                                 ! unc in efr
        real enp                                  ! KE of prompt fission neutrons
        real denp                                 ! unc in enp
        real end                                  ! KE of delayed fission neutrons
        real dend                                 ! unc in end
        real egp                                  ! total E released in prompt gammas
        real degp                                 ! unc in egp
        real egd                                  ! total E released in delayed gammas
        real degd                                 ! inc in egd
        real eb                                   ! energy released by betas
        real deb                                  ! unc in eb
        real enu                                  ! energy released by neutrinos
        real denu                                 ! unc in enu
        real er                                   ! total E less neutrinos = ET - ENU. =pseudo Q-value in MF3/MT18
        real der                                  ! unc in der
        real et                                   ! sum of all partial energies
        real det                                  ! unc in et
    end type

    type MF1_458
        integer nply                             ! max poly order. starts at 0
        type (MF1_458_terms), pointer :: cmp(:)
    end type

    !~~~~~~~~~~~~~~~~~~~~~~~~~~~~  MT460  Delayed Photon Data  ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

    type MF1_460
        integer lo                               ! rep flag: 1=discreet, 2=continuous
        integer ng                               ! # of photons (gammas)
        real, pointer :: e(:)                    ! energy of ith photon (ng)
        type (tab1), pointer :: phot(:)          ! time dep of ith photon multiplicity (ng)
        integer nnf                              ! number of precursor families
        real, pointer :: lambda(:)               ! decay constants (nnf)
    end type

    !~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ MF1 data type ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

    type MF_1
        type (MF_1), pointer :: next
        integer mt
        real za
        real awr
        type (MF1_451), pointer :: mt451
        type (MF1_452), pointer :: mt452
        type (MF1_455), pointer :: mt455
        type (MF1_452), pointer :: mt456
        type (MF1_458), pointer :: mt458
        type (MF1_460), pointer :: mt460
    end type

    !~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ private ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

    private read_451,read_4526,read_455,read_458,read_460
    private write_451,write_4526,write_455,write_458,write_460

!------------------------------------------------------------------------------
    contains
!------------------------------------------------------------------------------

    subroutine read_mf1(mf1)

    implicit none

    type (mf_1), intent(out) :: mf1

    integer n

    call get_endf(mf1%za, mf1%awr, n, n, n, n)
    nullify(mf1%mt451, mf1%mt452, mf1%mt455, mf1%mt456, mf1%mt458, mf1%mt460)

    select case(mf1%mt)
    case(451)
        allocate(mf1%mt451)
        call read_451(mf1%mt451)
    case(452)
        allocate(mf1%mt452)
        call read_4526(mf1%mt452)
    case(455)
        allocate(mf1%mt455)
        call read_455(mf1%mt455)
    case(456)
        allocate(mf1%mt456)
        call read_4526(mf1%mt456)
    case(458)
        allocate(mf1%mt458)
        call read_458(mf1%mt458)
    case(460)
        allocate(mf1%mt460)
        call read_460(mf1%mt460)
    case default
        write(erlin,*) 'Undefined MT encountered in MF1: ', mf1%mt
        call endf_error(erlin)
    end select

    return
    end subroutine read_mf1

!------------------------------------------------------------------------------

    subroutine read_451(r1)

    implicit none

    type (mf1_451), intent(out), target :: r1

    integer i,n,ios
    real xx
    character*66, pointer :: cmt

    call get_endf(r1%lrp, r1%lfi, r1%nlib, r1%nmod)
    call read_endf(r1%elis, r1%sta, r1%lis, r1%liso, n, r1%nfor)
    call read_endf(r1%awi, r1%emax, r1%lrel, n, r1%nsub, r1%nver)
    call read_endf(r1%temp, xx, r1%ldrv, n, r1%nwd, r1%nxc)

    if(r1%nfor < 6) then
        write(erlin,'(a,i1)') ' Unsupported format number in MF1/451 NFOR = ',r1%nfor
        call endf_error(erlin,-125)
    endif

    call chk_siz(r1%nwd,'MF1 comments','NWD',0)
    call chk_siz(r1%nxc,'MF1 directory','NXC',0)
    allocate(r1%cmnt(r1%nwd),r1%dir(r1%nxc),stat=n)
    if(n /= 0) call endf_badal

    do i = 1,r1%nwd
        call get_endline
        r1%cmnt(i) = endline(1:66)
    end do

    do i = 1,r1%nxc
        call read_endf(r1%dir(i)%mf, r1%dir(i)%mt, r1%dir(i)%nc, r1%dir(i)%mod)
    end do

    ! the first 5 lines of comments usually have fixed fields containing
    ! the symbolic name of the material, lab name, date, author, etc.
    ! but these fields are NOT required and they may not be there. Look
    ! at each line and try to extract these fields, if possible.

    r1%zsymam = ' '
    r1%alab   = ' '
    r1%edate  = ' '
    r1%auth   = ' '
    if(r1%nwd > 0) then
        cmt => r1%cmnt(1)
        if((cmt(4:4) == '-') .and. (cmt(7:7) == '-')) r1%zsymam = cmt(1:11)
        r1%alab   = cmt(12:22)
        r1%edate  = cmt(23:32)
        r1%auth   = cmt(34:66)
    endif

    r1%ref    = ' '
    r1%ddate  = ' '
    r1%rdate  = ' '
    r1%endate = ' '
    if(r1%nwd > 1) then
        cmt => r1%cmnt(2)
        r1%ref = cmt(2:22)
        if(cmt(23:26) == 'DIST') r1%ddate = cmt(23:32)
        if(cmt(34:36) == 'REV')  r1%rdate = cmt(34:43)
        r1%endate = cmt(56:63)
    endif

    r1%libtyp = ' '
    r1%mat = 0
    r1%irev = 0
    if(r1%nwd > 2) then
        cmt => r1%cmnt(3)
        if(cmt(1:4) == '----') then
            r1%libtyp = cmt(5:21)
            if(cmt(23:31) == 'MATERIAL ') then
                read(cmt(32:35),'(i4)',iostat=ios) r1%mat
                if(ios /= 0) r1%mat = 0
            endif
            if(cmt(45:52) == 'REVISION') then
                read(cmt(54:56),*,iostat=ios) r1%irev
                if(ios /= 0) r1%irev = 0
            endif
        endif
    endif

    r1%sublib = ' '
    if(r1%nwd > 3) then
        cmt => r1%cmnt(4)
        if(cmt(1:5) == '-----') r1%sublib = cmt(6:66)
    endif

    r1%mfor = 0
    if(r1%nwd > 4) then
        cmt => r1%cmnt(5)
        if(cmt(1:6) == '------') then
            read(cmt(12:12),'(i1)',iostat=ios) r1%mfor
            if(ios /= 0) r1%mfor = 0
        endif
    endif

    return
    end subroutine read_451

!------------------------------------------------------------------------------

    subroutine read_4526(r2)

    implicit none

    type (mf1_452), intent(out) :: r2

    integer n

    call get_endf(n, r2%lnu, n, n)

    if(r2%lnu == 1) then
        nullify(r2%tb)
        call read_endf(n, n, r2%nc, n)
        call chk_siz(r2%nc,'polynomial expansion','NC')
        allocate(r2%c(r2%nc),stat=n)
        if(n /= 0) call endf_badal
        call read_endf(r2%c,r2%nc)
    else if(r2%lnu == 2) then
        r2%nc = 0
        nullify(r2%c)
        allocate(r2%tb,stat=n)
        if(n /= 0) call endf_badal
        call read_endf(n, n, r2%tb%nr, r2%tb%np)
        call read_endf(r2%tb)
    else
        write(erlin,*) 'Undefined LNU specified in MF1,MT452:',r2%lnu
        call endf_error(erlin)
    end if

    return
    end subroutine read_4526

!------------------------------------------------------------------------------

    subroutine read_455(r5)

    implicit none

    type (mf1_455), intent(out) :: r5

    integer i,j,n
    real xx

    call get_endf(r5%ldg, r5%lnu, n, n)

    if(r5%ldg == 0) then

        ! here we have just one E-indep lambda/family
        ! the families are not interpolated.

        nullify(r5%lb)
        call read_endf(n, n, r5%nff, n)
        call chk_siz(r5%nff,'precursor families','NFF')
        allocate(r5%lambda(r5%nff),stat=n)
        if(n /= 0) call endf_badal
        call read_endf(r5%lambda,r5%nff)

    else if(r5%ldg == 1) then

        ! here the families are E-dependent. We need to read in the E-inter table
        ! and the values for decay const lambda(x) and group abundancies alpha(y)
        ! for each energy & family. The size of the real pair is (NE,NFF).

        nullify(r5%lambda)
        allocate(r5%lb,stat=n)
        if(n /= 0) call endf_badal
        call read_endf(n, n, r5%lb%nr, r5%lb%ne)
        call chk_siz(r5%lb%nr,'Interpolation table','NR')
        call chk_siz(r5%lb%ne,'Delayed group energies','NE')
        allocate(r5%lb%itp(r5%lb%nr),r5%lb%e(r5%lb%ne),stat=n)
        if(n /= 0) call endf_badal
        call read_endf(r5%lb%itp,r5%lb%nr)

        ! read first family to get 2*NFF in first (& every) record

        call read_endf(xx, r5%lb%e(1), n, n, i, n)
        r5%nff = i/2
        call chk_siz(r5%nff,'precursor families','NFF')
        allocate(r5%lb%dgc(r5%lb%ne, r5%nff),stat=n)
        if(n /= 0) call endf_badal
        do j = 1,r5%nff
            call get_endf(r5%lb%dgc(1,j)%x)
            call get_endf(r5%lb%dgc(1,j)%y)
        end do

        ! read the rest

        do i = 2,r5%lb%ne
            call read_endf(xx, r5%lb%e(i), n, n, n, n)
            do j = 1,r5%nff
                call get_endf(r5%lb%dgc(i,j)%x)
                call get_endf(r5%lb%dgc(i,j)%y)
            end do
        end do

    else

        write(erlin,*) 'Undefined LDF specified in MF1,MT455:',r5%ldg
        call endf_error(erlin)

    end if

    ! now read in the nubars

    if(r5%lnu == 1) then
        nullify(r5%tb)
        call read_endf(n, n, r5%nc, n)
        call chk_siz(r5%nc,'Polynomial expansion','NC')
        allocate(r5%c(r5%nc),stat=n)
        if(n /= 0) call endf_badal
        call read_endf(r5%c,r5%nc)
    else if(r5%lnu == 2) then
        r5%nc = 0
        nullify(r5%c)
        allocate(r5%tb,stat=n)
        if(n /= 0) call endf_badal
        call read_endf(n, n, r5%tb%nr, r5%tb%np)
        call read_endf(r5%tb)
    else
        write(erlin,*) 'Undefined LNU specified in MF1,MT455:',r5%lnu
        call endf_error(erlin)
    end if

    return
    end subroutine read_455

!------------------------------------------------------------------------------

    subroutine read_458(r8)

    implicit none

    type (mf1_458), intent(out) :: r8

    integer i,j,k,n

    call get_endf(n, n, n, n)
    call read_endf(n, r8%nply, i, j)
    if((i /= (18*(r8%nply+1))) .or. (j /= (9*(r8%nply+1)))) then
        write(erlin,*)  'Inconsistent values for NT, NPLY in MF1/458 found: ',i,r8%nply
        call endf_error(erlin)
    endif

    call chk_siz(r8%nply,'Polynomial expansion','NPLY',0)
    allocate(r8%cmp(0:r8%nply),stat=n)
    if(n /= 0) call endf_badal

    do k = 0,r8%nply
        call read_reals(r8%cmp(k)%efr,18)
    end do

    return
    end subroutine read_458

!------------------------------------------------------------------------------

    subroutine read_460(r6)

    implicit none

    type (mf1_460), intent(out) :: r6

    integer i,n
    real xx

    call get_endf(r6%lo, n, i, n)

    if(r6%lo == 1) then
        r6%ng = i
        call chk_siz(r6%ng,'Number of discrete photons','NG')
        allocate(r6%e(r6%ng),r6%phot(r6%ng),stat=n)
        if(n /= 0) call endf_badal
        do i = 1,r6%ng
            call read_endf(r6%e(i), xx, n, n, r6%phot(i)%nr, r6%phot(i)%np)
            call read_endf(r6%phot(i))
        end do
        r6%nnf = 0
        nullify(r6%lambda)
    else if(r6%lo == 2) then
        call read_endf(n, n, r6%nnf, n)
        call chk_siz(r6%nnf,'Precursor families','NNF')
        allocate(r6%lambda(r6%nnf),stat=n)
        if(n /= 0) call endf_badal
        call read_endf(r6%lambda,r6%nnf)
        r6%ng = 0
        nullify(r6%e,r6%phot)
    else
        write(erlin,*) 'Undefined LO specified in MF1,MT460:',r6%lo
        call endf_error(erlin)
    end if

    return
    end subroutine read_460

!***********************************************************************************

    subroutine write_mf1(mf1)

    implicit none

    type (mf_1), intent(in) :: mf1

    call set_mt(mf1%mt)

    select case(mf1%mt)
    case(451)
        call write_451(mf1%mt451,mf1%za,mf1%awr)
    case(452)
        call write_4526(mf1%mt452,mf1%za,mf1%awr)
    case(455)
        call write_455(mf1%mt455,mf1%za,mf1%awr)
    case(456)
        call write_4526(mf1%mt456,mf1%za,mf1%awr)
    case(458)
        call write_458(mf1%mt458,mf1%za,mf1%awr)
    case(460)
        call write_460(mf1%mt460,mf1%za,mf1%awr)
    case default
        write(erlin,*) 'Undefined MT encountered in MF1: ', mf1%mt
        call endf_error(erlin)
    end select

    call write_send

    return
    end subroutine write_mf1

!------------------------------------------------------------------------------

    subroutine write_451(r1,za,awr)

    implicit none

    type (mf1_451), intent(in) :: r1
    real, intent(in) :: za,awr

    integer i,nmod,ios

    ! reset nmod using directory modifcation numbers

    nmod = 0
    do i = 1,r1%nxc
        nmod = max(nmod,r1%dir(i)%mod)
    end do

    call write_endf(za, awr, r1%lrp, r1%lfi, r1%nlib, nmod)
    call write_endf(r1%elis, r1%sta, r1%lis, r1%liso, 0, r1%nfor)
    call write_endf(r1%awi, r1%emax, r1%lrel, 0, r1%nsub, r1%nver)
    call write_endf(r1%temp, zero, r1%ldrv, 0, r1%nwd, r1%nxc)

    ! the first 5 lines of comments usually have fixed fields containing
    ! the symbolic name of the material, lab name, date, author, etc.
    ! but these fields are NOT required and they may not be there. 
    ! check these fields and rebuild lines from sub-fields when specified.

    if(r1%nwd > 0) then
        endline = r1%cmnt(1)
        if((r1%zsymam(4:4) == '-') .and. (r1%zsymam(7:7) == '-')) endline(1:11) = r1%zsymam
        endline(12:22) = r1%alab
        endline(23:32) = r1%edate
        endline(34:66) = r1%auth
        call put_endline
    endif

    if(r1%nwd > 1) then
        endline = r1%cmnt(2)
        endline(1:1) = ' '
        endline(2:22) = r1%ref
        if(r1%ddate(1:4) == 'DIST') endline(23:32) = r1%ddate
        if(r1%rdate(1:3) == 'REV')  endline(34:43) = r1%rdate
        endline(56:63) = r1%endate
        call put_endline
    endif

    if(r1%nwd > 2) then
        endline = r1%cmnt(3)
        if((r1%mat > 0) .and. (r1%mat < 10000)) then
            ! assume legal material number & write it
            endline(1:4) = '----'
            endline(5:21) = r1%libtyp
            endline(22:30) = ' MATERIAL'
            write(endline(32:35),'(i4)',iostat=ios) r1%mat
            if(ios /= 0) then
                write(erlin,*) 'Error writing MAT in MF1/451 comment line 3'
                call endf_error(erlin)
            endif
        endif
        if(r1%irev > 0) then
            endline(45:52) = 'REVISION'
            if(r1%irev < 10) then
                write(endline(54:54),'(i1)',iostat=ios) r1%irev
            else if(r1%irev < 100) then
                write(endline(54:55),'(i2)',iostat=ios) r1%irev
            else if(r1%irev < 1000) then
                write(endline(54:56),'(i3)',iostat=ios) r1%irev
            else
                write(erlin,*) 'Revision number too large in MF1/451, line 3:',r1%irev
                call endf_error(erlin)
            endif
            if(ios /= 0) then
                write(erlin,*) 'Error writing revision number in MF1/451 comment line 3'
                call endf_error(erlin)
            endif
        endif
        call put_endline
    endif

    if(r1%nwd > 3) then
        endline = r1%cmnt(4)
        if(len_trim(r1%sublib) > 0) then
            endline(1:5) = '-----'
            endline(6:66) = r1%sublib
        endif
        call put_endline
    endif

    if(r1%nwd > 4) then
        endline = r1%cmnt(5)
        if((r1%mfor > 0) .and. (r1%mfor < 10)) then
            endline(1:66) = '------ENDF-X FORMAT'
            write(endline(12:12),'(i1)',iostat=ios) r1%mfor
            if(ios /= 0) then
                write(erlin,*) 'Error writing format number in MF1/451 comment line 5'
                call endf_error(erlin)
            endif
        endif
        call put_endline
    endif

    ! the rest of the comment lines are free-format strings

    do i = 6,r1%nwd
        endline(1:66) = r1%cmnt(i)
        call put_endline
    end do

    ! write directory at end

    do i = 1,r1%nxc
        call write_endf(0,r1%dir(i)%mf, r1%dir(i)%mt, r1%dir(i)%nc, r1%dir(i)%mod)
    end do

    return
    end subroutine write_451

!------------------------------------------------------------------------------

    subroutine write_4526(r2,za,awr)

    implicit none

    type (mf1_452), intent(in)  :: r2
    real, intent(in) :: za,awr

    call write_endf(za, awr, 0, r2%lnu, 0, 0)

    if(r2%lnu == 1) then
        call write_endf(0, 0, r2%nc, 0)
        call write_endf(r2%c,r2%nc)
    else if(r2%lnu == 2) then
        call write_endf(0, 0, r2%tb%nr, r2%tb%np)
        call write_endf(r2%tb)
    else
        write(erlin,*) 'Undefined LNU specified in MF1,MT452:',r2%lnu
        call endf_error(erlin)
    end if

    return
    end subroutine write_4526

!------------------------------------------------------------------------------

    subroutine write_455(r5,za,awr)

    implicit none

    type (mf1_455), intent(in) :: r5
    real, intent(in) :: za,awr

    integer i,j

    call write_endf(za, awr, r5%ldg, r5%lnu, 0, 0)

    if(r5%ldg == 0) then

        ! here we have just one E-indep lambda/family
        ! the families are not interpolated.

        call write_endf(0, 0, r5%nff, 0)
        call write_endf(r5%lambda,r5%nff)

    else if(r5%ldg == 1) then

        ! here the families are E-dependent. We need to write in the E-inter table
        ! and the values for decay const lambda(x) and group abundancies alpha(y)
        ! for each energy & family. The size of the real pair is (NE,NFF).

        call write_endf(0, 0, r5%lb%nr, r5%lb%ne)
        call write_endf(r5%lb%itp,r5%lb%nr)

        ! write families

        do i = 1,r5%lb%ne
            call write_endf(zero, r5%lb%e(i), 0, 0, 2*r5%nff, 0)
            do j = 1,r5%nff
                call put_endf(r5%lb%dgc(i,j)%x)
                call put_endf(r5%lb%dgc(i,j)%y)
            end do
        end do

    else

        write(erlin,*) 'Undefined LDF specified in MF1,MT455:',r5%ldg
        call endf_error(erlin)

    end if

    ! now write the nubars

    if(r5%lnu == 1) then
        call write_endf(0, 0, r5%nc, 0)
        call write_endf(r5%c,r5%nc)
    else if(r5%lnu == 2) then
        call write_endf(0, 0, r5%tb%nr, r5%tb%np)
        call write_endf(r5%tb)
    else
        write(erlin,*) 'Undefined LNU specified in MF1,MT455:',r5%lnu
        call endf_error(erlin)
    end if

    return
    end subroutine write_455

!------------------------------------------------------------------------------

    subroutine write_458(r8,za,awr)

    implicit none

    type (mf1_458), intent(in) :: r8
    real, intent(in) :: za,awr

    integer i

    call write_endf(za, awr, 0, 0, 0, 0)
    call write_endf(zero, zero, 0, r8%nply, 18*(r8%nply+1), 9*(r8%nply+1))
    do i = 0,r8%nply
        call write_reals(r8%cmp(i)%efr,18)
    end do

    return
    end subroutine write_458

!------------------------------------------------------------------------------

    subroutine write_460(r6,za,awr)

    implicit none

    type (mf1_460), intent(in) :: r6
    real, intent(in) :: za,awr

    integer i

    if(r6%lo == 1) then
        call write_endf(za, awr, r6%lo, 0, r6%ng, 0)
        do i = 1,r6%ng
            call write_endf(r6%e(i), zero, i, 0, r6%phot(i)%nr, r6%phot(i)%np)
            call write_endf(r6%phot(i))
        end do
    else if(r6%lo == 2) then
        call write_endf(za, awr, r6%lo, 0, 0, 0)
        call write_endf(0, 0, r6%nnf, 0)
        call write_endf(r6%lambda,r6%nnf)
    else
        write(erlin,*) 'Undefined LO specified in MF1,MT460:',r6%lo
        call endf_error(erlin)
    end if

    return
    end subroutine write_460

!***********************************************************************************

    subroutine del_mf1(mf1)

    implicit none

    type (mf_1) mf1

    integer i,n

    if(associated(mf1%mt451)) then
        if(associated(mf1%mt451%cmnt)) deallocate(mf1%mt451%cmnt,mf1%mt451%dir,stat=n)
        deallocate(mf1%mt451,stat=n)
    else if(associated(mf1%mt452)) then
        if(associated(mf1%mt452%c)) deallocate(mf1%mt452%c,stat=n)
        if(associated(mf1%mt452%tb)) call remove_tab1(mf1%mt452%tb)
        deallocate(mf1%mt452,stat=n)
    else if(associated(mf1%mt455)) then
        if(associated(mf1%mt455%lambda)) deallocate(mf1%mt455%lambda,stat=n)
        if(associated(mf1%mt455%lb)) then
            if(associated(mf1%mt455%lb%itp)) deallocate(mf1%mt455%lb%itp, mf1%mt455%lb%e,stat=n)
            if(associated(mf1%mt455%lb%dgc)) deallocate(mf1%mt455%lb%dgc,stat=n)
            deallocate(mf1%mt455%lb,stat=n)
        endif
        if(associated(mf1%mt455%c)) deallocate(mf1%mt455%c,stat=n)
        if(associated(mf1%mt455%tb)) call remove_tab1(mf1%mt455%tb)
        deallocate(mf1%mt455,stat=n)
    else if(associated(mf1%mt456)) then
        if(associated(mf1%mt456%c)) deallocate(mf1%mt456%c,stat=n)
        if(associated(mf1%mt456%tb)) call remove_tab1(mf1%mt456%tb)
        deallocate(mf1%mt456,stat=n)
    else if(associated(mf1%mt458)) then
        if(associated(mf1%mt458%cmp)) deallocate(mf1%mt458%cmp,stat=n)
        deallocate(mf1%mt458,stat=n)
    else if(associated(mf1%mt460)) then
        if(associated(mf1%mt460%e)) then
            do i = 1,mf1%mt460%ng
                call del_tab1(mf1%mt460%phot(i))
            end do
            deallocate(mf1%mt460%e, mf1%mt460%phot,stat=n)
        endif
        if(associated(mf1%mt460%lambda)) deallocate(mf1%mt460%lambda,stat=n)
        deallocate(mf1%mt460,stat=n)
    endif

    return
    end subroutine del_mf1

!***********************************************************************************

    integer function lc_mf1(mf1)

    implicit none

    type (mf_1), intent(in) :: mf1

    integer i,l

    l = 0

    select case(mf1%mt)
    case(451)
        l = mf1%mt451%nwd + 4    ! don't include directory count at this point
    case(452)
        l = 1
        if(mf1%mt452%lnu == 1) then
            l = l + (mf1%mt452%nc + 5)/6 + 1
        else if(mf1%mt452%lnu == 2) then
            l = l + lc_tab1(mf1%mt452%tb) + 1
        else
            write(erlin,*) 'Undefined LNU specified in MF1,MT452:',mf1%mt452%lnu
            call endf_error(erlin)
        end if
    case(455)
            l = 1
        if(mf1%mt455%ldg == 0) then
            l = l + (mf1%mt455%nff+5)/6 + 1
        else if(mf1%mt455%ldg == 1) then
            l = l + (2*mf1%mt455%lb%nr+5)/6 + 1
            do i = 1,mf1%mt455%lb%ne
                l = l + (2*mf1%mt455%nff+5)/6 + 1
        end do
        else
            write(erlin,*) 'Undefined LDG specified in MF1,MT455:',mf1%mt455%ldg
            call endf_error(erlin)
        end if
        if(mf1%mt455%lnu == 1) then
            l = l + (mf1%mt455%nc+5)/6 + 1
        else if(mf1%mt455%lnu == 2) then
            l = l + lc_tab1(mf1%mt455%tb) + 1
        else
            write(erlin,*) 'Undefined LNU specified in MF1,MT455:',mf1%mt455%lnu
            call endf_error(erlin)
        end if
    case(456)
        l = 1
        if(mf1%mt456%lnu == 1) then
            l = l + (mf1%mt456%nc+5)/6 + 1
        else if(mf1%mt456%lnu == 2) then
            l = l + lc_tab1(mf1%mt456%tb) + 1
        else
            write(erlin,*) 'Undefined LNU specified in MF1,MT456:',mf1%mt456%lnu
            call endf_error(erlin)
        end if
    case(458)
        l = 3*(mf1%mt458%nply+1) + 2
    case(460)
        if(mf1%mt460%lo == 1) then
            l = 1
            do i = 1,mf1%mt460%ng
                l = l + lc_tab1(mf1%mt460%phot(i)) + 1
            end do
        else if(mf1%mt460%lo == 2) then
            l = (mf1%mt460%nnf+5)/6 + 2
        else
            write(erlin,*) 'Undefined LO specified in MF1,MT460:',mf1%mt460%lo
            call endf_error(erlin)
        end if
    case default
        write(erlin,*) 'Undefined MT encountered in MF1: ', mf1%mt
        call endf_error(erlin)
    end select

    lc_mf1 = l

    return
    end function lc_mf1

end module ENDF_MF1_IO
