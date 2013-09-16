    program kalman

    !*********************************************************************
    !*     program kalman    :    kyushu university 1992                 *
    !*                                                                   *
    !*                             original version 1983 y.uenohara      *
    !*                              revised version 1989 t.kawano        *
    !*                                 open version 1992 t.kawano        *
    !*                              BNL F90 version 2013 s.hoblit        *
    !*                                                                   *
    !*    unit(1)  : KALMAN.INP  read nreac,nparm,iexp,iparm,ew...       *
    !*    unit(2)  : KALMAN.OUT                                          *
    !*    unit(10) : read x,y                                            *
    !*    unit(11) : read x,z                                            *
    !*    unit(12) : read v                                              *
    !*    unit(13) : write spline function                               *
    !*    unit(14) : write covariance of spline function                 *
    !*    unit(15) : write variation of parameters                       *
    !*    unit(50) : read ene,crs                                        *
    !*    unit(52) : read p0,x0,a                                        *
    !*********************************************************************

    implicit none

    ! define global variables

    integer*4 :: kctl1 = 0                       ! if non-zero, read initial posterior values & covariances from this unit
    integer*4 :: kctl2 = 0                       ! if non-zero, write posterior values & errors to this unit
    integer*4 :: kcovex = 0                      ! if == 1, read data covariances from unit 12
    integer*4 :: ntotal = 0                      ! total # data points processed
    integer*4 :: iseqno = 0                      ! total # measurements processed
    real*8 :: chitot = 0.D0                      ! total accumulated chi2
    real*8 emin0,emax0                           ! global energy limits on data
    character*80 title                           ! fit title

    ! parameters

    type parameter
        character*12 nam                         ! parameter name
        real*8 x                                 ! value
        real*8 dx                                ! uncertainty
    end type
    type (parameter), allocatable :: pri(:)      ! (nptot) all prior parameters
    type (parameter), allocatable :: pr0(:)      ! (nparm) fitted prior params
    type (parameter), allocatable :: pr1(:)      ! (nparm) fitted posterior params
    real*8, allocatable :: pric(:,:)             ! all prior parameter covariances.
    real*8, allocatable :: prc(:,:)              ! fitted parameter covariances.
    integer*4 :: nptot = 0                       ! # of parameters in prior file
    integer*4 :: nparm = 0                       ! # of parameters to be fit
    integer*4, allocatable :: iparm(:)           ! index of parameters being fit
    real*8 :: scale = 1.D0                       ! scale factor to multiply all parameter covariances. if == 0.0, set to 1.D0.
    integer*4 :: nplog = 0                       ! # parameter log entries
    integer*4, parameter :: max_steps = 500      ! max # parameters steps to save
    real*8, allocatable :: par_steps(:,:)        ! saved parameters from each fit

    ! data

    integer*4 :: nreac = 0                       ! # of data reactions to fit
    integer*4 :: ndata = 0                       ! number of experimental data points in measurement
    real*8, allocatable :: xdat(:)               ! experimental energies
    real*8, allocatable :: ydat(:)               ! experimental cross sections
    real*8, allocatable :: vdat(:,:)             ! experimental data covariances

    ! reactions

    type reaction
        integer*4 nen                            ! # energies
        character*43 nam                         ! reaction name
        real*8, allocatable :: ene(:)            ! (nen) energies
        real*8, allocatable :: crs(:)            ! (nen) cross sections
        real*8, allocatable :: err(:)            ! (nen) cross sections error (from fit)
        real*8, allocatable :: sen(:,:)          ! (nptot,nen) sensitivity at each energy to each parameter
    end type
    type (reaction), allocatable, target :: rxn(:)  ! all reactions
    type (reaction) crx                             ! reaction being fit
    integer*4 :: nrtot = 0                          ! total # of calculated reactions in file

    integer*4 i

    ! open KALMAN.INP file on 1, KALMAN.OUT file on 2

    open(1,file='KALMAN.INP',status='old',action='read')
    open(2,file='KALMAN.OUT',status='replace',action='write')

    ! read various parameters & flags from input file. Also the
    ! indicies (iparm) of which parameters are to be fit.

    read(1,10) title
    read(1,20) nreac,nparm,kctl1,kctl2,kcovex,scale,emin0,emax0
    allocate(iparm(nparm))
    read(1,'(14I5)')(iparm(i),i=1,nparm)
    if(scale == 0.D0) scale = 1.D0

    call read_reactions              ! get calculated cross sections
    call read_parameters             ! read initial parameters

    write(2,10) title
    write(2,30) nptot
    write(2,40) nparm
    call fit_reactions               ! fit the reactions
    close(1)

    call write_posterior             ! write posterior parameters
    call outspl                      ! write spline functions
    call cvrspl                      ! write covariance of splines
    call cvrprm(0)                   ! write covariance of parameters
    call pwrite(kctl2)               ! write posterior params
    call write_psteps                ! write parameter variations
    close(2)

10  format(a80)
20  format(5i5,5x,3e10.3)
30  format('Number of total parameters ',i5 )
40  format('      estimated parameters ',i5/)

    contains

    !******************************************************************************************

    subroutine fit_reactions

    ! main fitting loop

    implicit none

    integer*4 i,ireac,imsur,iexp,nmsur
    real*8 emin,emax,emn,chisq
    real*8, allocatable :: ew(:)

    emin = emin0
    emax = emax0
    call log_params

    open(10,status='old',action='read')
    open(11,status='old',action='read')
    if(kcovex == 1) open(12,status='old',action='read')

    ! reaction loop

    do ireac = 1,nreac

        ! read reaction index & number of measurments
        ! allocate & read weighting for each meas

        read(1,'(2I5)') iexp,nmsur
        allocate(ew(nmsur))
        read(1,220) (ew(i),i=1,nmsur)

        if((iexp < 1) .or. (iexp > nrtot)) then
            write(6,'(a,i0)') ' Undefined reaction index specified : ',iexp
            stop 1
        endif

        write(6,*)
        write(6,'(a)') ' Fitting reaction: '//trim(rxn(iexp)%nam)

        emn = set_reaction(iexp)

        if(emin0 == 0.D0) emin = emn
        if(emax0 == 0.D0) emax = crx%ene(1)

        do imsur = 1,nmsur

            call get_data(emin,emax,ew(imsur))
            write(6,'(a,i0,a,i0)') '   measurement ',imsur,'   # data pts = ',ndata
            chisq = prcalc()
            write(6,'(a,1PE11.4)') '     partial chi2 = ',chisq

            chitot = chitot + chisq
            ntotal = ntotal + ndata
            iseqno = iseqno + 1

            write(2,280) crx%nam
            write(2,290) iseqno,imsur,ndata,ntotal
            write(2,300) chisq,chitot
            call log_params

        end do

        deallocate(ew)

    end do

    close(10)
    close(11)
    if(kcovex == 1) close(12)

    return

220 format(7e10.3)
280 format('Cross Section :',a43)
290 format('Sq.no.:',i5,3x,'React.:',i5,3x,'Points:',i5,3x,'Sum up:',i5)
300 format('Partial Chi sq:',1pe12.5,3x,'Cumulative :',1pe12.5/)

    end subroutine fit_reactions

    !******************************************************************************************

    real*8 function prcalc()

    ! main calculation

    implicit none

    integer*4, parameter :: nord = 1         ! order of polynomial interpolation

    integer*4 i,j,k,l
    real*8 x1,x2,x3,x4
    real*8, allocatable :: d(:,:), w(:,:), f(:,:), dp(:)

    if(ndata <= 0) then
        ! nothing to do
        prcalc = 0.D0
        return
    endif

    allocate(d(crx%nen,ndata),w(ndata,nparm),f(nparm,ndata))

    ! make interpolation matrix d

    do i = 1,ndata
        do j = 1,crx%nen
            d(j,i) = polynm(xdat(i),j,nord,crx%nen)
        end do
    end do

    ! y <= y - f*crs

    do i = 1,ndata
        x1 = 0.D0
        do k = 1,crx%nen
            x1 = x1 + d(k,i)*crx%crs(k)
        end do
        ydat(i) = ydat(i) - x1
    end do

    ! f <= fi * sen

    do i = 1,ndata
        do j = 1,nparm
            x1 = 0.D0
            do k = 1,crx%nen
                x1 = x1 + d(k,i)*crx%sen(iparm(j),k)
            end do
            f(j,i) = x1
        end do
    end do

    deallocate(d)

    ! w <= f * prc

    do i = 1,nparm
        do j = 1,ndata
            x1 = 0.D0
            do k = 1,nparm
                x1 = x1 + prc(k,i)*f(k,j)
            end do
            w(j,i) = x1
        end do
    end do

    ! vdat <= ( f * prc * f**t + vdat )**(-1)

    do i = 1,ndata
         do j = 1,ndata
            x1 = 0.D0
            do k = 1,nparm
               x1 = x1 + w(j,k)*f(k,i)
            end do
            vdat(j,i) = vdat(j,i) + x1
         end do
    end do

    deallocate(f)

    call invmtx(ndata,vdat)

    ! calc p1,x1,dp

    allocate(dp(nparm))      ! change in each fitted parameter

    do i = 1,nparm

        do j = 1,i-1
            x1 = 0.D0
            do k = 1,ndata
                x3 = 0.D0
                do l = 1,ndata
                    x3 = x3 + vdat(l,k)*w(l,j)
                end do
                x1 = x1 + w(k,i)*x3
            end do
            prc(i,j) = prc(i,j) - x1
            prc(j,i) = prc(i,j)
        end do

        x1 = 0.D0
        x2 = 0.D0
        do k = 1,ndata
            x3 = 0.D0
            x4 = 0.D0
            do l = 1,ndata
                x3 = x3 + vdat(l,k)*w(l,i)
                x4 = x4 + vdat(l,k)*ydat(l)
            end do
            x1 = x1 + w(k,i)*x3
            x2 = x2 + w(k,i)*x4
        end do
        prc(i,i) = prc(i,i) - x1
        dp(i) = x2
        pr1(i)%x = pr1(i)%x + x2

    end do

    deallocate(w)

    do i = 1,crx%nen
        x1 = 0.D0
        do j = 1,nparm
            x1 = x1 + crx%sen(iparm(j),i)*dp(j)
        end do
        crx%crs(i) = crx%crs(i) + x1
    end do

    deallocate(dp)

    ! checking for parameters and cross sections

    do i = 1,nparm
        if (pr1(i)%x < 0.D0) then
            write(6,'(a,i0,a)') ' ** parameter ',i,' is negative!'
        endif 
    enddo

    do i = 1,crx%nen
        if (crx%crs(i) < 0.D0) then
            write(6,'(a,i0,a)') '  ** calc cross section reaction ',i,' < 0!!!!'
        endif
    enddo

    ! chi square test

    x1 = 0.D0
    do i = 1,ndata
        do j = 1,ndata
            x1 = x1 + vdat(j,i)*ydat(j)*ydat(i)
        end do
    end do

    prcalc = x1

    return
    end function prcalc

    !******************************************************************************************

    subroutine read_reactions

    ! read calculated values using prior parameters

    implicit none

    integer*4 i,k
    type (reaction), pointer :: rx

    open(50,status='old',action='read')

    read(50,'(i5)') nrtot
    allocate(rxn(nrtot))

    do i = 1,nrtot
        rx => rxn(i)
        read(50,'(a43,i5)') rx%nam, rx%nen
        allocate(rx%ene(rx%nen),rx%crs(rx%nen),rx%err(rx%nen))
        read(50,'(6e11.4)') (rx%ene(k),rx%crs(k),k=1,rx%nen)
        rx%err = 0.D0
    end do

    close(50)

    return
    end subroutine read_reactions

    !******************************************************************************************

    subroutine read_parameters

    ! read parameters & their covariances from parameter file
    ! here we read get the number of parameters being fit & read them in
    ! fill the prior & post arrays of parameters being fit.

    implicit none

    integer*4 i

    open(52,status='old',action='read')

    read(52,'(25x,i5)') nptot
    allocate(pri(nptot),pr0(nparm),pr1(nparm))
    allocate(pric(nptot,nptot),prc(nparm,nparm))
    read(52,'(11a12)')  (pri(i)%nam,i=1,nptot)

    call pread(52,pr0)
    call read_sensitivities
    close(52)

    if(kctl1 == 0) then
        pr1 = pr0
    else
        call pread(kctl1,pr1)
    endif

    allocate(par_steps(nparm,max_steps))

    return
    end subroutine read_parameters

    !******************************************************************************************

    subroutine pread(kunit, par)

    ! read prior parameters

    implicit none

    integer*4, intent(in) :: kunit             ! logical unit of file containing priors
    type (parameter), intent(out) :: par(*)    ! parameters

    logical*4 qop
    integer*4 i,j,ki,kj

    inquire(kunit,opened=qop)
    if(.not.qop) open(kunit,status='old',action='read')

    ! read parameters from file into full array
    ! if we read in starting values for posteriors,
    ! we must overright the full array

    read(kunit,100)(pri(i)%x,i=1,nptot)        ! prior parameter values
    read(kunit,100)(pri(i)%dx,i=1,nptot)       ! prior relative errors

    do i = 1,nptot
        pri(i)%dx = pri(i)%x*pri(i)%dx         ! convert to absolute errors
        read(kunit,200)(pric(j,i),j=1,nptot)   ! parameter correlation matrix
    end do

    if(.not.qop) close(kunit)

    ! convert pc from correlation to covariance matrix

    do i = 1,nptot
        do j = 1,nptot
            pric(j,i) = pric(j,i)*pri(i)%dx*pri(j)%dx
        end do
    end do

    ! extract the parameters being fitted

    do i = 1,nparm
        ki = iparm(i)
        par(i) = pri(ki)                      ! value of fitted parameters
        do j = 1,i-1
            kj = iparm(j)
            prc(i,j) = pric(ki,kj)          ! covariance of fitted parameters
            prc(j,i) = pric(kj,ki)
        end do
        prc(i,i) = pric(ki,ki)
    end do

    return

100 format(11e12.5)
200 format(20f6.3)

    end subroutine pread

    !******************************************************************************************

    subroutine pwrite(kunit)

    ! write posterior parameters 

    implicit none

    integer*4, intent(in) :: kunit           ! logical unit to write parameters

    integer*4 i,j

    if(kunit == 0) return
    open(kunit,action='write')

    write(kunit,100)(pri(i)%x,i=1,nptot)
    write(kunit,100)(pri(i)%dx/pri(i)%x,i=1,nptot)
    do i=1,nptot
        write(kunit,200)(pric(j,i)/(pri(i)%dx*pri(j)%dx),j=1,nptot)
    end do
    close(kunit)

    return

100 format(11(1pe12.5))
200 format(20f6.3)

    end subroutine pwrite

    !******************************************************************************************

    subroutine read_sensitivities

    ! read sensitivity matrix

    implicit none

    integer*4 i,k,n,l
    character*25 targ
    type (reaction), pointer :: rx

    do i = 1,nrtot
        rx => rxn(i)
        read(52,'(a25,i5)') targ,n
        if(n /= rx%nen) then
            write(6,'(a25,i5)') targ,n
            write(6,*) 'sensitivity, spline, energy points different'
            stop 1
        endif
        allocate(rx%sen(nptot,n))
        do k = 1,n
            read(52,'(11e12.5)')(rxn(i)%sen(l,k),l=1,nptot)
        end do
    end do

    return
    end subroutine read_sensitivities

    !******************************************************************************************

    real*8 function set_reaction(kn)

    ! set current reaction & prepare sensitivity and spline

    implicit none

    integer*4, intent(in) :: kn         ! reaction index

    integer*4 i,j,l
    real*8 xx
    real*8, allocatable :: wk(:)

    if(allocated(crx%ene)) deallocate(crx%ene)
    if(allocated(crx%crs)) deallocate(crx%crs)
    if(allocated(crx%err)) deallocate(crx%err)
    if(allocated(crx%sen)) deallocate(crx%sen)

    allocate(crx%ene(rxn(kn)%nen))
    allocate(crx%crs(rxn(kn)%nen))
    allocate(crx%err(rxn(kn)%nen))
    allocate(crx%sen(nptot,rxn(kn)%nen),wk(nptot))

    crx%nen = rxn(kn)%nen
    crx%nam = rxn(kn)%nam
    crx%ene = rxn(kn)%ene
    crx%crs = rxn(kn)%crs
    crx%sen = rxn(kn)%sen
    crx%err = 0.D0

    ! quick sort

    do j = 1,crx%nen
        l = j
        do i = j,crx%nen
            if(crx%ene(i) > crx%ene(l)) l = i
        end do
        if(l == j) cycle
        call swap(crx%ene(j),crx%ene(l))
        call swap(crx%crs(j),crx%crs(l))
        wk = crx%sen(1:nptot,l)
        crx%sen(1:nptot,l) = crx%sen(1:nptot,j)
        crx%sen(1:nptot,j) = wk
    end do

    deallocate(wk)

    ! update calculated cross sections to current parameter values p1

    do i = 1,crx%nen
        xx = 0.D0
        do j = 1,nparm
            xx = xx + crx%sen(iparm(j),i)*(pr1(j)%x - pr0(j)%x)
        end do
        crx%crs(i) = crx%crs(i) + xx
    end do

    ! find energy with zero or smallest cross section

    do i = 1,crx%nen
        if(crx%crs(i) > 0.D0) cycle
        set_reaction = crx%ene(i-1)
        return
    end do

    set_reaction = crx%ene(crx%nen)

    return
    end function set_reaction

    !******************************************************************************************

    subroutine get_data(emin,emax,ewt)

    ! read experimental data from input file

    implicit none

    real*8, intent(in) :: emin         ! lower energy limit
    real*8, intent(in) :: emax         ! upper energy limit
    real*8, intent(in) :: ewt          ! weight of data set

    logical*4 qfill
    integer*4 i,j,l,nc,ne,nd
    real*8 xmin,xmax,xx
    real*8, allocatable :: z(:),xd(:),yd(:),vd(:,:)
    character*43 dum

    ! read data from 10,11,12

    read(10,10) dum,nd
    allocate(xd(nd),yd(nd),z(nd),vd(nd,nd))
    read(10,20)(xd(i),yd(i),i=1,nd)
    write(2,10) dum,nd
    read(11,10) dum,ne
    if(ne /= nd) then
        write(6,'(a,i0,2x,i0)') '*** No. of error points not equal to data points, ND,NE = ',nd,ne
        stop 1
    endif
    read(11,20)(xx,z(i),i=1,nd)

    xx = 0.D0
    qfill = .true.
    if(kcovex == 1) then
        read(12,10) dum,nc
        if(nc /= 0) then
            if(abs(nc) /= nd) then
                write(6,'(a,i0,2x,i0)') '*** No. of correlation points not equal to data points, ND,NC = ',nd,nc
                stop 1
            endif
            if(nc > 0) then
                do i=1,nc
                    read(12,30)(vd(i,j),j=1,i)
                    do j = 1,i-1
                        vd(j,i) = vd(i,j)
                    end do
                end do
                qfill = .false.
            else
                read(12,30) xx
                if(abs(xx) > 1.D0) then
                    write(6,'(a,1PE12.5)') ' Off diagonal data correlation > 1.0 : ',xx
                    stop 1
                endif
            endif
        endif
    endif

    if(qfill) then
        vd = xx
        do i = 1,nd
            vd(i,i) = 1.D0
        end do
    end if

    if(ewt == 0.D0) then
        ndata = 0
        deallocate(xd,yd,vd,z)
        write(6,'(a)') ' *** exp. data set with weight = 0.0 (ignored) ********'
        write(6,'(a)') '   data set skipped in fit'
        return
    endif

    ! check range

    xmin = xd(1)
    xmax = xd(1)
    do i = 2,nd
        if(xmin > xd(i)) xmin = xd(i)
        if(xmax < xd(i)) xmax = xd(i)
    end do

    if((xmin > emax) .or. (xmax < emin)) then
        ndata = 0
        deallocate(xd,yd,vd,z)
        write(6,'(a)') ' *** all exp. data out of range ***'
        write(6,'(a)') '   data set skipped in fit'
        return
    endif

    ! convert data correlation to covariance

    do i = 1,nd

        if(z(i) < 0.D0) then
            z(i) = ewt*dabs(z(i))
        else if(z(i) > 0.D0) then
            z(i) = ewt*yd(i)*z(i)
        else
            ndata = 0
            deallocate(xd,yd,vd,z)
            write(6,'(a,i0)') '*** datum with err = 0 encountered, index=',i
            write(6,'(a)') '   data set skipped in fit'
            return
        endif

    end do

    do i = 1,nd
        do j = 1,nd
            vd(j,i) = vd(j,i)*z(i)*z(j)
        end do
    end do

    ! eliminate out-of-range data

    do i = nd,1,-1

        if(xd(i) < emin) then
            nd = nd - i
            exit
        endif

        if(xd(i) <= emax) cycle

        do l = i+1,nd
            xd(l-1) = xd(l)
            yd(l-1) = yd(l)
            vd(l-1,1:nd) = vd(l,1:nd)
            vd(1:nd,l-1) = vd(1:nd,l)
        end do

        nd = nd - 1

    end do

    ! is anything left?

    if(nd < 1) then
        ndata = 0
        deallocate(xd,yd,vd,z)
        return
    endif

    ! store data into final arrays

    if(allocated(xdat)) deallocate(xdat)
    if(allocated(ydat)) deallocate(ydat)
    if(allocated(vdat)) deallocate(vdat)

    allocate(xdat(nd),ydat(nd),vdat(nd,nd))

    xdat = xd(1:nd)
    ydat = yd(1:nd)
    vdat = vd(1:nd,1:nd)

    deallocate(xd,yd,vd)

    ! sort to monotonically decreasing energies

    do j = 1,nd
        l = j
        do i = j,nd
            if(xdat(i) > xdat(l)) l = i
        end do
        if(l == j) cycle
        call swap(xdat(j),xdat(l))
        call swap(ydat(j),ydat(l))
        z(1:nd) = vdat(l,1:nd)
        vdat(l,1:nd) = vdat(j,1:nd)
        vdat(j,1:nd) = z(1:nd)
        z(1:nd) = vdat(1:nd,l)
        vdat(1:nd,l) = vdat(1:nd,j)
        vdat(1:nd,j) = z(1:nd)
    end do

    deallocate(z)

    ndata = nd

    return

10  format(a43,i5)
20  format(6e11.4)
30  format(12f6.3)

    end subroutine get_data

    !******************************************************************************************

    subroutine swap(a,b)

    implicit none

    ! swap data a <-> b

    real*8, intent(inout) :: a,b

    real*8 c

    c = a
    a = b
    b = c

    return
    end subroutine swap

    !******************************************************************************************

    real*8 function polynm(e,j,n,nen)

    implicit none

    real*8, intent(in) :: e           ! energy to interpolate
    integer*4, intent(in) :: j        ! reaction index
    integer*4, intent(in) :: n        ! order of poly to interpolate
    integer*4, intent(in) :: nen      ! # point in energy grid

    integer*4 l,k,id1,id2
    real*8 y1,y2,de1,de2

    polynm = 0.D0

    do l = 1,nen-1
         de1 = e - crx%ene(l)
         de2 = e - crx%ene(l+1)
         if((de1 <= 0.0) .and. (de2 > 0.D0)) exit
    end do

    id2 = max(l,1)
    id1 = min(l+n,nen)
    if(id1 < j) return
    if(id2 > j) return

    y1 = 1.D0
    y2 = 1.D0
    do k = id2,id1
        if(k == j) cycle
        y1 = y1*(e - crx%ene(k))
        y2 = y2*(crx%ene(j) - crx%ene(k))
    end do

    polynm = y1/y2

    return
    end function polynm

    !******************************************************************************************

    subroutine write_posterior

    ! write out posterior parameters

    implicit none

    integer*4 i,j,k,ki,kj
    real*8 ratio,xx,yy

    if(ntotal == 0) then
        ratio = 0.D0
    else
        ratio = chitot/dble(ntotal)
    endif
    write(2,310) chitot,ntotal,ratio

    ratio = dsqrt(max(ratio,1.D0))

    write(21,'(25x,i5)') nparm
    if(kctl1 == 0) then
        write(2,320)
        write(21,'(11a12)')(pr1(i)%nam,i=1,nparm)
        do i = 1,nparm
            pr1(i)%dx = dsqrt(prc(i,i))
            xx = pr1(i)%x
            if(xx == 0.D0) then
                write(2,330) i,pr1(i)%nam,pr0(i)%x,pr1(i)%x,pr1(i)%dx
            else
                write(2,340) i,pr1(i)%nam,pr0(i)%x,pr1(i)%x,100.D0*pr1(i)%dx/xx
            endif
        end do
        write(21,'(11(1pe12.5))') (pr1(i)%x,i=1,nparm)
        write(21,'(11(1pe12.5))') (pr1(i)%dx/pr1(i)%x,i=1,nparm)
    else
        write(2,350)
        write(21,'(11a12)')(pr1(i)%nam,i=1,nparm)
        do i = 1,nparm
            pr1(i)%dx = dsqrt(prc(i,i))
            xx = pr1(i)%x
            k=iparm(i)
            if(xx == 0.D0) then
                write(2,360) i,pr1(i)%nam,pr0(i)%x,pri(k)%x,pr1(i)%x,pr1(i)%dx
                write(6,360) i,pr1(i)%nam,pr0(i)%x,pri(k)%x,pr1(i)%x,pr1(i)%dx
            else
                yy = 100.D0*pr1(i)%dx/xx
                write(2,370) i,pr1(i)%nam,pr0(i)%x,pri(k)%x,pr1(i)%x,yy
                write(6,370) i,pr1(i)%nam,pr0(i)%x,pri(k)%x,pr1(i)%x,yy
            endif
        end do
        write(21,'(11(1pe12.5))') (pr1(i)%dx/pr1(i)%x,i=1,nparm)
    endif

    write(2,*) 'Scale = ', scale

    ! replace original params with final fit

    do i = 1,nparm
        ki = iparm(i)
        pri(ki) = pr1(i)
        do j = 1,i-1
            kj = iparm(j)
            pric(ki,kj) = prc(i,j)*scale
            pric(kj,ki) = prc(j,i)*scale
        end do
        pric(ki,ki) = prc(i,i)*scale
    end do

    return

310 format('Chi-square test !   ',   15x,'Chi - s = ',e12.5,/'Degree of Freedom = ',i5,10x,'Ratio   = ',e12.5)
320 format(/5x,' Parameter  ','  Initial  ','  Final    ','  Error    '/)
330 format(i5,a12,3(1pe11.4),1x,'(abs)')
340 format(i5,a12,3(1pe11.4),1x,'( % )')
350 format(/5x,' Parameter  ','  Initial  ','  Final 1  ','  Final 2  ','  Error    '/)
360 format(i5,a12,4(1pe11.4),1x,'(abs)')
370 format(i5,a12,4(1pe11.4),1x,'( % )')

    end subroutine write_posterior

    !******************************************************************************************

    subroutine outspl

    ! write splines on unit 13

    implicit none

    integer*4 ix,i,j
    real*8 xx
    type (reaction), pointer :: rx

    ! crs <= sen * (p1 - p0)

    open(13,action='write')

    do ix = 1,nrtot
        rx => rxn(ix)
        do i = 1,rx%nen
            xx = 0.D0
            do j = 1,nparm
                xx = xx + rx%sen(iparm(j),i)*(pr1(j)%x - pr0(j)%x)
            end do
            rx%crs(i) = rx%crs(i) + xx
        end do
        write(13,100) rx%nam,rx%nen
        write(13,200)(rx%ene(i),rx%crs(i),i=1,rx%nen)
    end do

    close(13)

100 format(a43,i5)
200 format(6(1pe11.4))

    return
    end subroutine outspl

    !******************************************************************************************

    subroutine cvrspl

    ! write covariance matrix of splines on unit 14

    implicit none

    integer*4 index,jndex,i,j,jmax,ir,jr
    integer*4, allocatable :: iv(:)
    real*8, allocatable :: vg(:,:)
    type (reaction), pointer :: rx1,rx2

    open(14,action='write')
    open(16,action='write')
    open(32,action='write')

    ! same reactions

    index = 0
    do ir = 1,nrtot

        rx1 => rxn(ir)
        allocate(vg(rx1%nen,rx1%nen),iv(rx1%nen))
        call mulmtx(ir,ir,vg)

        do i = 1,rx1%nen
            rx1%err(i) = dsqrt(vg(i,i))
        end do

        write(14,300) rx1%nam
        write(14,200)(index+j,j=1,rx1%nen)

        ! generate endf-like numbers

        write(16,102) rx1%nen,rx1%nam,rx1%nam
        write(16,500) (rx1%ene(i),i=1,rx1%nen)
        write(16,500) (rx1%crs(i),i=1,rx1%nen)
        do i=1,rx1%nen
            write(16,500) (vg(i,j),j=1,rx1%nen)
        end do

        do i = 1,rx1%nen
            do j = 1,rx1%nen
                if(i == j) then
                    iv(j) = 1000
                else if((rx1%err(i) == 0.D0) .or. (rx1%err(j) == 0.D0)) then
                    iv(j) = 0
                else
                    iv(j) = nint(1000.D0*vg(i,j)/(rx1%err(i)*rx1%err(j)))
                endif
            end do
            jmax = min(i,50)
            if(rx1%crs(i) == 0.D0) then
                write(14,100) index+i,rx1%ene(i),rx1%err(i),(iv(j),j=1,jmax)
            else
                write(14,100) index+i,rx1%ene(i),rx1%err(i)/rx1%crs(i)*100.D0,(iv(j),j=1,jmax)
            endif
            if(jmax < i) write(14,200)(iv(j),j=jmax+1,i)
        end do

        deallocate(vg,iv)
        index = index + rx1%nen

    end do

    ! different reactions

    index=0
    do ir = 1,nrtot

        rx1 => rxn(ir)
        jndex=0

        do jr = 1,ir-1

            rx2 => rxn(jr)
            allocate(vg(rx1%nen,rx2%nen),iv(rx2%nen))
            call mulmtx(ir,jr,vg)
            write(14,400) rx1%nam,rx2%nam
            write(14,200)(jndex+j,j=1,rx2%nen)

            ! generate endf-like numbers

            write(32,102) rx1%nen,rx1%nam,rx2%nam
            write(32,500) (rx1%ene(i),i=1,rx1%nen)
            write(32,500) (rx1%crs(i),i=1,rx1%nen)
            do i=1,rx1%nen
                write(32,500) (vg(i,j),j=1,rx2%nen)
            end do

            do i = 1,rx1%nen
                do j=1,rx2%nen
                    if((rx1%err(i) == 0.D0) .or. (rx2%err(j) == 0.D0)) then
                        iv(j) = 0
                    else
                        iv(j) = nint(1000.D0*vg(i,j)/(rx1%err(i)*rx2%err(j)))
                    endif
                end do
                jmax = min(rx2%nen,50)
                if(rx1%crs(i) == 0.D0) then
                    write(14,100) index+i,rx1%ene(i),rx1%err(i),(iv(j),j=1,jmax)
                else
                    write(14,100) index+i,rx1%ene(i),rx1%err(i)/rx1%crs(i)*100.0,(iv(j),j=1,jmax)
                endif
                if(jmax < rx2%nen) write(14,200) (iv(j),j=jmax+1,rx2%nen)
            end do

            jndex = jndex + rx2%nen
            deallocate(vg,iv)

        end do

        index = index + rx1%nen

    end do

    close(14)
    close(16)
    close(32)

100 format(i5,2(1pe10.2),50i5)
101 format(i5,43x,a12)
102 format(i5,43x,a12,1x,a12)
150 format(i5,20f6.2)
200 format(25x,50i5)
300 format('Diagonal    ',a43)
400 format('Off-Diagonal',a43,/,'            ',a43)
500 format(6(1pe12.5))

    return
    end subroutine cvrspl

    !******************************************************************************************

    subroutine mulmtx(ir,jr,vg)

    ! get reaction covariances

    implicit none

    integer*4, intent(in) :: ir         ! 1st reaction index
    integer*4, intent(in) :: jr         ! 2nd reaction index
    real*8, intent(out) :: vg(:,:)      ! reaction covariance

    ! vg <= sen * pric * sen**t

    integer*4 i,j,k
    real*8 xx
    real*8, allocatable :: w(:)

    if(.not.allocated(w)) allocate(w(nptot))

    do i = 1,rxn(ir)%nen

        do j = 1,nptot
            xx = 0.D0
            do k = 1,nptot
                xx = xx + pric(k,j)*rxn(ir)%sen(k,i)
            end do
            w(j) = xx
        end do

        do j = 1,rxn(jr)%nen
            xx = 0.D0
            do k = 1,nptot
                xx = xx + w(k)*rxn(jr)%sen(k,j)
            end do
            vg(i,j) = xx
        end do

    end do

    return
    end subroutine mulmtx

    !******************************************************************************************

    subroutine log_params

    ! log current parameters

    implicit none

    integer*4 i

    if(nplog >= max_steps) return

    nplog = nplog + 1
    do i = 1,nparm
        par_steps(i,nplog) = pr1(i)%x
    end do

    return
    end subroutine log_params

    !******************************************************************************************

    subroutine write_psteps

    ! write_psteps : write parameter steps on unit 15

    implicit none

    integer*4 i,j

    open(15,action='write')

    do i = 1,nparm
        write(15,10) i,pr1(i)%nam,nplog
        write(15,20)(dble(j-1),par_steps(i,j),j=1,nplog)
    end do

    close(15)

    return

20  format(6(1pe11.4))
10  format(i4,a12,27x,i5)

    end subroutine write_psteps

    !******************************************************************************************

    subroutine cvrprm(k)

    ! WRITE COVARIANCE MATRIX

    implicit none

    integer*4, intent(in) :: k

    integer*4 i,j,ix(40),iblk,nblock,lo,j1,j2,j12,jm

    if(k /= 0) then
        do i = 1,nparm
            write(2,200) (prc(i,j),j=1,i)
        end do
        return
    endif

    nblock = nparm/10 + 1
    lo = mod(nparm,10)
    if(lo == 0) nblock = nblock - 1

    do iblk = 1,nblock
        j1 = 10*iblk - 9
        j2 = 10*iblk
        if((iblk == nblock) .and. (lo /= 0)) j2 = j1+lo-1
        jm = j2 - j1 + 1 
        do j = 1,jm
            ix(j) = j1 + j - 1
        end do
        write(2,300) (ix(j),j=1,jm)
        do i = j1,nparm
            j12 = min(i,j2)
            do j = j1,j12
                if(i == j) then
                    ix(j-j1+1) = 1000
                else
                    ix(j-j1+1) = nint(1000.D0*prc(i,j)/(pr1(i)%dx*pr1(j)%dx))
                end if
            end do
            write(2,100) i,pr1(i)%nam,pr1(i)%x,(ix(j),j=1,j12-j1+1)
        end do
    end do

    ! c for covariance matrix of parameter (input format)

    do i = 1,nparm
        write(21,'(20(f6.3))') (prc(i,j)/(pr1(i)%dx*pr1(j)%dx),j=1,nparm)
    end do

    return

100 format(i5,a12,1pe9.2,10i5)
200 format(12e10.3)
300 format(/,26x,20i5)

    end subroutine cvrprm

    end program kalman

    !##########################################################################################

    subroutine invmtx(n,v)

    ! calculate inverse of 1-D packed symmetric matrix

    implicit none

    integer*4, intent(in) :: n          ! # elements in 1-D matrix
    real*8, intent(inout) :: v(n,n)     ! symmetric matrix to invert

    real*8, allocatable :: vv(:)         ! storage for packed 1-D matrix

    integer*4 i,j,k,icond

    icond = 0

    if(n < 1) then
        write(6,'(a,i0)') ' inverse of matrix with dimension < 1 :',n
        stop 1
    else if(n == 1) then 
        if(v(1,1) /= 0.D0) v(1,1) = 1.D0/v(1,1)
        return
    endif

    allocate(vv(n*(n+1)/2))

    k = 1
    do i = 1,n
        do j = 1,i
            vv(k) = v(j,i)
            k = k + 1
        end do
    end do

    call cholsk(n,vv,icond)
    if(icond == -1) then
        write(6,*) ' matrix not positive'
        stop 1
    else if(icond == -2) then 
        write(6,*) ' pivot zero'
        stop 1
    endif

    call invers(n,vv,icond)
    if(icond.eq.-1) then
        write(6,*) ' matrix singular'
        stop 1
    endif

    k = 1
    do i = 1,n
        do j = 1,i
            v(i,j) = vv(k)
            v(j,i) = vv(k)
            k = k + 1
        end do
    end do

    deallocate(vv)

    return
    end subroutine invmtx

    !******************************************************************************************

    subroutine cholsk(n,v,icond)

    ! cholsk : ldu decomposition by modified choleski method *
    !          l triangular matrix                           *
    !          d inverse of diagonal matrix                  *
    !          u transpose of l                              *

    implicit none

    integer*4, intent(in)  :: n          ! # elements in 1-D matrix
    integer*4, intent(out) :: icond      ! condition flag
    real*8, intent(inout)  :: v(*)       ! 1-D packed symmetric matrix 

    real*8, parameter :: eps = 1.0D-30

    integer*4 i,j,ij,jk,ik,jj,iv
    real*8 x1,xx

    icond = 0

    if(v(1) == 0.D0) then
        icond = -2
        return
    else if(v(1) < 0.D0) then
        icond = -1
        return
    endif

    v(1) = 1.D0/v(1)
    x1   = v(3) - v(1)*v(2)*v(2)
    v(2) = v(1)*v(2)

    if(dabs(x1) < dabs(v(3)*eps)) then
        icond=-2
        return
    endif

    if(x1 < 0.D0) then
        icond=-1
        return
    endif

    v(3) = 1.D0/x1

    !***************************************
    !  for n=2                             *
    !                                      *
    !  v1  v2       1 /v1  v2/ v1          *
    !  v2  v3  -->  v2/v1  v1/(v1*v3-v2*v2)*
    !                                      *
    !***************************************

    if (n == 2) return

    iv=4

    do i = 1,n-2
        jk = 2
        ik = iv
        do j = 1,i
            xx = 0.D0
            do ij = iv,iv+j-1
                xx = xx+v(ij)*v(jk)
                jk = jk+1
            end do
            jk = jk + 1
            ik = ik + 1
            v(ik) = v(ik) - xx
        end do
        jj = 1
        ij = iv
        xx = 0.D0
        do j = 2,i+2
             x1 = v(ij)*v(jj)
             xx = xx + v(ij)*x1
             v(ij) = x1
             jj = jj + j
             ij = ij + 1
        end do
        x1 = v(jj) - xx
        if( dabs(x1) < dabs(v(jj)*eps) ) then
            icond = -2
            return
        endif
        if(x1 < 0.D0) then
            icond = -1
            return
        endif
        v(jj) = 1.D0/x1
        iv = jj + 1
    end do

    return
    end subroutine cholsk

    !******************************************************************************************

    subroutine invers(n,v,icond)

    ! invers : inversion of ldu decomposed matrix
    !          v=ldu
    !          v**(-1)= u**(-1) d l**(-1)

    implicit none

    integer*4, intent(in)  :: n          ! # elements in 1-D matrix
    integer*4, intent(out) :: icond      ! condition flag
    real*8, intent(inout)  :: v(*)       ! 1-D packed symmetric matrix to invert

    integer*4 i,j,k,ij,ij1,ij2,kj1,kj2,kj,ii,ki,ijsp,ik
    real*8 x1,xx

    icond = 0

    v(2) = -v(2)
    if(n /= 2) then
        ij2= 2
        do i = 2,n-1
            ij1 = ij2 + 2
            ij2 = ij2 + i + 1
            kj1 = 0
            do j = 2,i
                xx = v(ij1)
                ij1 = ij1+1
                kj2 = j
                kj1 = kj1+kj2
                kj  = kj1
                do ik = ij1,ij2
                    xx = xx + v(ik)*v(kj)
                    kj = kj + kj2
                    kj2 = kj2 + 1
                end do
                v(kj) = -xx
            end do
            v(ij2) = -v(ij2)
        end do
    endif

    ii = 1
    ki = 2
    xx = v(1)
    do k = 2,n
        ii = ii+k
        if(v(ii) < 0.D0) then
            icond=-1
            return
        endif
        x1 = v(ki)*v(ii)
        xx = xx + v(ki)*x1
        v(ki) = x1
        ki = ki + k
    end do
    v(1) = xx
    if(n == 2) return
    ij = 1
    do i = 2,n-1
        do j = 2,i
            ij = ij+1
            kj = ij
            xx = v(kj)
            ijsp = i + 1 - j
            do k = i,n-1
                kj = kj + k
                ki = kj + ijsp
                xx = xx + v(kj)*v(ki)
            end do
            v(ij) = xx
        end do
        ij = ij+1
        ki = ij+i
        ii = ij
        xx = v(ii)
        do k = i+1,n
            ii = ii + k
            x1 = v(ki)*v(ii)
            xx = xx + v(ki)*x1
            v(ki) = x1
            ki = ki + k
        end do
        v(ij) = xx
    end do

    return
    end subroutine invers
