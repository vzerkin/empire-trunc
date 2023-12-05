program c4tokal
    !! convert c4 file into format understood by Kalman
    use rctn, only: retReactionMT

    use c4_io
    implicit none

    integer*4, parameter :: ngmt = 15      !! list of allowed MTs for Kalman fitting
    integer*4, parameter :: goodmt(ngmt) = (/1,2,3,4,16,17,18,102,103,107,207,251,456, 5, 851/)

    integer*4, parameter :: kctl1 = 0      !! set nonzero to read priors
    integer*4, parameter :: kctl2 = 0      !! set nonzero to write posteriors
    integer*4, parameter :: kcovex = 1     !! set nonzero to read experimental covariances
    real*4, parameter    :: scale = 1.0    !! scale factor for parameter unc**2
    real*4, parameter    :: emin = 0.0     !! lower energy limit on exp data, 0 == no limit
    real*4, parameter    :: emax = 0.0     !! upper energy limit on exp data, 0 == no limit

    character*4, parameter :: xsc = '.xsc'
    character*8, parameter :: inpsen = '-inp.sen'

    integer*4 nsec     !! # of sections in C4 file
    integer*4 mt1      !! MT to plot. If MT1=0, then plot all MTs. If NEX=1, only fit this MT. 
    integer*4 nex      !! fitting flag: 1=>fit only MT1, 2=>fit all MTs.
    integer*4 nrx      !! # of reactions in EMPIRE XSC file
    integer*4 mat      !! MAT of material. not used here
    integer*4 nprm     !! # EMPIRE parameters in sensitivity input file

    logical*4 qex,qmt,hmt(999)
    integer*4 i,j,k,m,ix,l1,l2,ios,status
    character pname*6,line*130,file*25

    type empire_reaction
        integer*4 mt
        character*12 name
    end type
    type (empire_reaction), allocatable :: rcx(:)

    type fitted_reaction
        integer*4 mt                               !! MT value
        integer*4 ix                               !! index in empire reaction
        integer*4 num                              !! # of C4 data sets for this MT
        integer*4, allocatable :: ic(:)            !! C4 index of each data set
        real*8, allocatable :: wt(:)               !! weights for each data set
    end type
    integer*4 :: nmt                               !! # MTs that will be fit
    integer*4, allocatable :: kx(:)                !! section indices
    type (fitted_reaction), target  :: fmt(ngmt)   !! the MTs being fit
    type (fitted_reaction), pointer :: fx

    type (c4_file) c4
    type (c4_section), pointer :: sc
    type (c4_data_point), pointer :: pt


    ! get project name, plotting MT, MAT and fitting flag nex

    read(5,*) file,mt1,mat,nex
    call strlen(file,l1,l2)

    i = 1
    do while (i <= ngmt)
        if(mt1 == goodmt(i)) exit
        i = i + 1
    end do

    ! make sure flags are consistent

    select case(nex)
    case(1)
        qmt = .true.
        if(i > ngmt) then
            write(0,'(a,i0)') ' Plotted & fitted MT unknown: ',mt1
            stop 1
        endif
    case(2)
        qmt = .false.
        if((mt1 /= 0) .and. (i > ngmt)) then
            write(0,'(a,i0)') ' Plotted MT unknown: ',mt1
            stop 1
        endif
    case default
        write(0,'(a,i0)') ' Undefined value for NEX specified : ',nex
        stop 1
    end select

    ! get number of empire parameters from empire sensitivity input file

    inquire(file=file(l1:l2)//inpsen,exist=qex)
    if(.not.qex) then
        write(0,'(a)') 'EMPIRE sensitivity input file not found: '//file(l1:l2)//inpsen
        stop 1
    endif

    open(13,file=file(l1:l2)//inpsen,status='old')
    nprm = 0
    do
        read(13,*,iostat=ios) pname
        if(ios > 0) then
            write(0,*) 'ERROR READING: ',file(l1:l2)//inpsen
            stop 1
        else if(ios < 0) then
            exit
        endif
        if(pname(1:1) == '!') cycle
        nprm = nprm + 1
    end do
    close(13)

    ! get list of reactions from empire cross section XSC file

    inquire(file=file(l1:l2)//xsc,exist=qex)
    if(.not.qex) then
        write(0,'(a)') 'EMPIRE CROSS SECTION FILE NOT FOUND: '//file(l1:l2)//xsc
        stop 1
    endif

    open(13,file=file(l1:l2)//xsc,status='old',action='read')
    read(13,'(1X,I3)') nrx
    allocate(rcx(nrx))
    read(13,'(12X,90A12)') (rcx(i)%name,i=1,nrx)
    close(13)

    ! convert reaction name to MT value

    do i = 1,nrx
       rcx(i)%mt = retReactionMT(rcx(i)%name)
    end do

    ! open C4 data file and get data
    ! look first for kalman specified -kal.c4.
    ! if not found, try just the .c4 file.

    inquire(file=file(l1:l2)//'-kal.c4',exist=qex)
    if(qex) then
        status = read_c4_file(file(l1:l2)//'-kal.c4',c4)
    else
       inquire(file=file(l1:l2)//'.c4',exist=qex)
       if(qex) then
          status = read_c4_file(file(l1:l2)//'.c4',c4)
       else
          write(0,*) 'No C4 file found!'
          stop 1
       endif
    endif

    if(status /= 0) then
       write(0,*) ' Error reading in C4 file. status = ',status
       stop 1
    endif

    ! scan C4 sections looking for good MTs
    ! count the number of sections found for each

    nmt = 0
    do k = 1,c4%nsec
        sc => c4%sec(k)
        hmt = .false.
        do j = 1,sc%ndat
            pt => sc%pt(j)

            if(pt%mf /= 3)               cycle    ! we only fit MF3
            if(qmt .and. (pt%mt /= mt1)) cycle    ! not fitting this MT
            if(hmt(pt%mt)) cycle                  ! already found this MT

            i = 1
            do while (i <= ngmt)
               if(pt%mt == goodmt(i)) exit
               i = i + 1
            end do
            if(i > ngmt) cycle                    ! only fit allowed MTs

            ix = 1
            do while(ix <= nrx)
               if(rcx(ix)%mt == pt%mt) exit
               ix = ix + 1
            end do
            if(ix > nrx) cycle                    ! reaction not in XSC file

            m = 1
            do while(m <= nmt)
               if(fmt(m)%mt == pt%mt) exit
               m = m + 1
            end do

            if(m > nmt) then
               nmt = m                            ! new reaction
               fmt(m)%mt = pt%mt
               fmt(m)%ix = ix
               fmt(m)%num = 1
            else
               fmt(m)%num = fmt(m)%num + 1
            endif

            hmt(pt%mt) = .true.

        end do
    end do

   if(nmt == 0) then
        write(0,'(a)') ' No data from C4 found in EMPIRE XSC file for reaction MT=',mt1,'!'
        stop 1
    endif

    ! allocate space for each MT

    do m = 1,nmt
        fx => fmt(m)
        allocate(fx%ic(fx%num),fx%wt(fx%num))
    end do

    ! allocate & initialize counters

    allocate(kx(nmt))
    kx = 0

    ! re-scan C4 sections, setting index of C4 section

    do k = 1,c4%nsec
        sc => c4%sec(k)
        hmt = .false.
        do j = 1,sc%ndat
            pt => sc%pt(j)

            if(pt%mf /= 3)               cycle    ! we only fit MF3
            if(qmt .and. (pt%mt /= mt1)) cycle    ! not fitting this MT
            if(hmt(pt%mt)) cycle                  ! already found this MT

            m = 1
            do while(m <= nmt)
               if(fmt(m)%mt == pt%mt) exit
               m = m + 1
            end do
            if(m > nmt) cycle                     ! not fitting MT

            hmt(pt%mt) = .true.

            fx => fmt(m)
            kx(m) = kx(m) + 1
            fx%ic(kx(m)) = k
            fx%wt(kx(m)) = 1.D0            ! Default weights = 1.0
            ! fx%wt(kx(m)) = fmt(m)%num**0.5  ! Normalizing by the number of experiments for each reaction
            ! fx%wt(kx(m)) = 45.05D0/(sc%ndat)**0.5     ! Normalizing each experiment by the number of its points
            ! fx%wt(kx(m)) = (sc%ndat)**0.5     ! Normalizing each experiment by the number of its points
            ! fx%wt(kx(m)) = sc%ndat     ! Normalizing each experiment by the number of its points

        end do
    end do

    ! make final consistency check

    do m = 1,nmt
        if(fmt(m)%num /= kx(m)) then
             write(0,'(a)') ' Internal inconsistency counting reactions'
             stop 1
        endif
    end do

    ! ok, now write input & data files

    open(15,file='KALMAN.INP',status='NEW',recl=120,action='WRITE')
    write(15,*) 'INPUT'
    write(15,'(5I5,5X,3E10.3)') nmt,nprm,kctl1,kctl2,kcovex,scale,emin,emax
    write(15,'(14I5)') (i,i=1,nprm)   ! fitting all parameters in sens file

    do m = 1,nmt
        fx => fmt(m)
        write(15,'(2I5)') fx%ix,fx%num
        write(15,'(7E10.3)') (fx%wt(i),i=1,fx%num)
        do i = 1,fx%num
            call dataout(c4%sec(fx%ic(i)),fx%mt)
        end do
    end do

    close(15)

    contains

    !------------------------------------------------------------------------

    subroutine dataout(sc,mt)

        implicit none

        ! echo data items back out to fortran units 10,11,12.
        ! write energies in MeV, cross sections in mb.
        ! also to unit 75 in MeV, barns for plotting.

        type (c4_section), intent(in) :: sc       ! C4 section to scan
        integer*4, intent(in) :: mt               ! MT to extract

        real*8, parameter :: cor = 0.2D0

        logical*4 qwt
        integer*4 i,l,m,npt
        real*8 xf
        character chr3*3

        type mtpt
            real*8 e
            real*8 x
            real*8 z
        end type
        type (mtpt), allocatable, target :: gpts(:)
        type (mtpt), pointer :: gp

        type (c4_data_point), pointer :: pt

        ! data written to unit 75 is for plots

        qwt = .false.
        if((mt1 == 0) .or. (mt == mt1)) then
           write(chr3,'(I3)') mt
           call strlen(chr3,l,m)
           open(75,file=file(l1:l2)//'-'//chr3(l:m)//'-c4.gpd',status='UNKNOWN',action='WRITE',access='APPEND')
           qwt = .true.
        endif

        if(mt < 200) then
            xf = 1000.D0         ! convert regular cross sections to mb
        else
            xf = 1.D0            ! don't convert mubar, nubar
        endif

        allocate(gpts(sc%ndat))

        npt = 0
        do i = 1,sc%ndat
           pt => sc%pt(i)
           if(pt%mt /= mt) cycle
           npt = npt + 1
           gp => gpts(npt)
           gp%e = 1.0D-06*pt%e
           gp%x = xf*pt%x
           if(pt%x == 0.D0) then
               gp%z = 0.D0
           else
               gp%z = pt%dx/pt%x
           endif
           if(.not.qwt) cycle
           if(pt%dx > 0.D0) write(75,999) gp%e,pt%x,pt%dx,mt
        end do

        if(qwt) close(75)

        if(npt < 1) then
            ! no points for MT should not happen
            ! print error message and abort
            write(0,'(a,i0)') ' Internal inconsistency processing MT = ',mt
            stop 1
        endif

        ! data to 10,11,12 are used for fitting

        write(10,100) sc%ref, sc%ent, sc%sub, npt
        write(10,200) (gpts(i)%e,gpts(i)%x, i=1,npt)
        write(11,100) sc%ref, sc%ent, sc%sub, npt
        write(11,200) (gpts(i)%e,gpts(i)%z,i=1,npt)
        if(kcovex /= 0) then
            write(12,100) sc%ref, sc%ent, sc%sub,-npt
            write(12,300) cor
        endif

        deallocate(gpts)

        return

        100 FORMAT(A25,5X,A5,A3,5X,I6)
        200 FORMAT(6(1PE11.4))
        300 FORMAT(F6.3)
        999 FORMAT(3(1X,E12.5),I4)

    end subroutine dataout

end program c4tokal
