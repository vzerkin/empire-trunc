      module empire_materials

      use c4_io
      use endf_io
        use endf_util
      use empire_parameters
      use endf_distributions

      implicit none

      public

      integer*4, parameter :: nen = 500               ! # points in fitted curve

      integer*4, parameter :: nmt = 20                ! # MTs for plots & covariances (MF=3)
      integer*4, parameter :: smt(nmt) = (/1,2,4,16,18,51,52,53,54,55,56,57,58,59,60,102,103,104,105,107/)  ! MTs for plots & covariances
      integer*4, parameter :: mlev = 40               ! # exicted energy levels to parse

      real*8, parameter :: edfm = 0.01                ! difference in incoming energy limit for same group
      real*8, parameter :: adfm = 1.0D-5              ! difference in cos(theta) for same group

      type data_point
            real*8 x                            ! independent variable (energy, cos(tht), etc)
            real*8 y0                           ! central Empire value
            real*8 yd                           ! exp value
            real*8 ud                           ! exp unc
            real*8 er2                          ! exp unc**2
            type (emp_sens), allocatable :: ep(:)           ! Empire parameter sensitivies
      end type

      type data_group
            integer*4 mf                              ! MF number
            integer*4 mt                              ! MT number
            logical*4 inc                             ! group included in fit
            logical*4 fit                             ! true when scale factor xcl is fit
            logical*4 wfit                            ! true when width sig is fit
            integer*4 id                              ! group index
            integer*4 ix                              ! Minuit scale parameter index
            integer*4 ix2                             ! Minuit width parameter index (for ene dists)
            integer*4 ndat                            ! # data points
            real*8 e                            ! incident energy for ang dist, PFNS, etc
            real*8 cth                          ! cos(theta) for outgoing energy distributions
            real*8 sig                          ! relative width to sqrt(E) of ene dists
            real*8 dsig                         ! unc relative width to sqrt(E) of ene dists
            real*8 xcl                          ! systematic scale factor
            real*8 dxcl                         ! systematic scale factor unc
            real*8 unc                          ! systematic relative uncertainty to weight fit
            real*8 chi2                         ! group total chi2
            character*8 flag                    ! pmeta,x4stat,cm,fid C4 flags
            character*1 pmeta                   ! product meta-stable flag (M=metastable)
            character*1 x4stat                        ! EXFOR status flag
            character*1 cm                            ! center-of-mass flag (C=cm,blank=lab)
            character*3 fid                           ! identification flag for dat7 & dat8.
            character*2 dum                           ! for alignment
            type (data_point), pointer :: pt(:)       ! data points
            type (data_point), pointer :: econ(:)           ! continuum energy dist from MF6
            type (data_point), pointer :: edsr(:)           ! discreet energies, cross sect from MF4
      end type

      type data_set
            type (data_set), pointer :: next          ! next data set
            real*8 uns                          ! sysetmatic unc to subtract from C4 total err
            integer*4 id                              ! dataset index
            integer*4 ngrp                            ! # of group (unique MF,MT)
            character*1 tmeta                   ! target meta-stable flag (M=metastable)
            character*25 ref                    ! reference (first author, year)
            character*5 ent                           ! EXFOR entry number
            character*3 sub                           ! EXFOR sub-entry number
            character*1 mdf                           ! multi-dimensional data flag
            type (data_group), pointer :: gp(:)       ! data groups
      end type

      type ext_curve
            type (real_pair) pt(0:nen)                ! curve
            character*120 endf                        ! ENDF file
            integer*4 col                             ! gnuplot color
            type (ext_curve), pointer :: next         ! next curve
      end type

      type data_plot
            integer*4 mf                              ! MF number
            integer*4 mt                              ! MT number
            type (ext_curve), pointer :: ex                 ! external curves
            type (data_point) pt(0:nen)               ! curve & sensitivities
      end type

      type rxn_scl
            real*8 xl                           ! scale factor for reaction (MT)
            real*8 unc                          ! uncertainty in scale factor
            logical*4 fit                             ! true if fitted
            integer*4 mt                              ! MT
            integer*4 ix                              ! Minuit prm index
            integer*4 id                              ! relative index
            integer*4 num                             ! # datasets included in fit
      end type

      type material
            character*80 dir                    ! material directory (1:ndr)
            character*32 proj                   ! project name (1:npr)
            integer*4 atgt                            ! material A
            integer*4 ztgt                            ! material Z
            integer*4 ndr                             ! # chars in directory
            integer*4 npr                             ! # chars in proj name
            integer*4 nparm                           ! # Empire parameters
            integer*4 ndset                           ! # data sets
            integer*4 ndgrp                           ! # data groups
            integer*4 nsmtg                           ! # MT scaling groups
            integer*4 npts                            ! # data points in material
            integer*4 nes                             ! # energies in Empire calc
            integer*4 iof                             ! Minuit parameter offset
            logical*4 fit                             ! material included in fit
            real*8 chi2                         ! material total chi2
            real*8 leve(mlev)                   ! excited state energies
            real*8, allocatable :: ene(:)             ! Empire calc energies (nes)
            type (rxn_scl) rxl(200)                   ! scale params for each reaction (MT)
            type (data_point), allocatable :: sen(:,:)      ! sensitivies for reaction cov (nes,nmt)
            type (emp_param), allocatable :: prm(:)         ! Empire parameters (nparm)
            type (data_plot), allocatable :: plt(:)         ! data plots (nmt)
            type (data_set), pointer :: ds                  ! data sets
      end type

      type C4_sort
            integer*4 mf                              ! MF number
            integer*4 mt                              ! MT number
            integer*4 ndat                            ! # data points
            real*8 e                            ! incident energy for ang dist, PFNS, etc
            real*8 cth                          ! cos(theta) for ene dists
            character*8 flag                    ! pmeta,x4stat,cm,fid C4 flags
            type (mf_3), pointer :: mf3
            type (mf_4), pointer :: mf4
      end type

      integer*4, private :: iof = 0                   ! Minuit parameter offset
      logical*4, private :: rmf(40)                   ! which MTs to read
      type (endf_file), private :: endf               ! endf file
      real*8, private :: elo6,ehi6                    ! low, high energies for incident neutrons in MF6

      integer*4 :: nptot = 0                          ! Total # of Minuit parameters
      integer*4 :: nmat = 0                           ! # materials
      type (material), allocatable, target :: mat(:)        ! materials
      type (material), pointer :: mtr                       ! "current" material

      logical*4 :: abort = .false.                    ! user abort flag

      logical*4 :: kalman = .false.                   ! true when taking covariance from KALMAN output file
      character*200 kalfil

      integer*4 nin                                   ! length of file name stem on command line
      character*200 infile                            ! input file name stem on command line

      private tablim,get_data,create_params,c4_sort

      contains

      !--------------------------------------------------------------------------------------

      subroutine get_materials

      implicit none

      integer*4 m,status,nfil

      integer*4, external :: get_input_file

      ! we need to get the project name for the fit
      ! at the moment, this is just the name of the
      ! Empire input file without the ".inp" extension.
      ! look first on the command line. If not supplied,
      ! look for a single *.inp in local directory and
      ! use that if only 1 found.

      nmat = 1    ! for now
      allocate(mat(1))

        if(nin < 1) then
            nfil = get_input_file(infile)
            if(nfil < 1) then
                  write(6,*) ' No Empire input file found'
                  stop 1
            else if(nfil == 1) then
                  nin = len_trim(infile)
                  mat(1)%proj = infile(1:nin-4)
                  mat(1)%npr = nin - 4
            else
                  write(6,*) ' Too many input files found'
                  stop 1
            endif
      else
            if(infile(max(nin-3,1):nin) == '.inp') nin = nin - 4
            if(nin < 0) then
                  write(6,*) ' Error parsing input filename'
                  stop 1
            endif
            mat(1)%proj = infile(1:nin)
            mat(1)%npr = nin
        endif

      iof = 0
      nptot = 0

      do m = 1,nmat

            mtr => mat(m)
            mtr%iof = iof
            mtr%fit = .true.

            call get_varied_parameters(iof,mtr%proj(1:mtr%npr),mtr%nparm,mtr%prm)   ! get Empire params from -sen.inp

            mtr%atgt = atgt                                       ! store A
            mtr%ztgt = ztgt                                       ! store Z

            call get_empire_energies(mtr%nes,mtr%ene)             ! Empire calc energies
            call read_lev_file(mtr%proj(1:mtr%npr))                     ! get energy levels

            status = read_endf_file(mtr%proj(1:mtr%npr)//trim(cen_dir)//mtr%proj(1:mtr%npr)//'.endf',endf)
            if(status /= 0) then
                  write(6,*)
                  write(6,*) ' Error reading central ENDF file'
                  stop 1
            endif

            call get_data                                         ! read diff exp data
            call setup_sens                                       ! setup sens & plts

            status = del_endf(endf)                               ! let go of central ENDF

            if(qfit) call get_sens                                ! calculate sensitivies
            call create_params                                    ! create Minuit params

            write(6,*) ' Material : ',mtr%proj
            write(6,*) ' Nparm = ',mtr%nparm
            write(6,*) ' Ndset = ',mtr%ndset

      end do

      nptot = iof       ! update total number of minuit parameters

      mtr => mat(1)
      write(6,*) ' Material fit : ',mtr%proj(1:mtr%npr)

      return
      end subroutine get_materials

      !--------------------------------------------------------------------------------------

      subroutine get_data

      implicit none

      integer*4, parameter :: maxr = 50         ! max # reactions in any single C4 experiment

      logical*4 qlab,qf
      integer*4 i,j,k,m,n,status,ios,ns,kid,npts
      real*8 x,xx,us,rlm,uang,e1,e2,lel,dif,esl
      character line*200

      type (c4_file) c4
      type (C4_sort), allocatable, target :: c4t(:)

      type (c4_section),    pointer :: sec
      type (c4_data_point), pointer :: cp
      type (c4_sort),       pointer :: tm
      type (data_set),      pointer :: ds
      type (data_group),    pointer :: dg
      type (data_point),    pointer :: pt
      type (mf_3),          pointer :: mf3
      type (mf_4),          pointer :: mf4
      type (mf_6),          pointer :: mf6

      ! read C4 file

      status = read_c4_file(mtr%proj(1:mtr%npr)//'.c4',c4)
      if(status /= 0) then
            write(6,'(a)') ' Error reading C4 file '//mtr%proj(1:mtr%npr)//'.c4'
            stop 3
      endif

      call mtmap(c4)          ! remap MT51

      ! open fit file to see what C4 datasets will be included in fit

      open(22,file=mtr%proj(1:mtr%npr)//'.fit',status='old',readonly,iostat=ios)
      if(ios /= 0) then
            write(6,*) ' Error opening ',mtr%proj(1:mtr%npr)//'.fit'
            stop 1
      endif

      ! get cutoff energies in MF6 in this endf file

      elo6 = 2.0D+07
      ehi6 = 0.D0
      mf6 => endf%mat%mf6
      do while(associated(mf6))
            call mf6_elim(mf6,0,1,0,e1,e2)
            elo6 = min(elo6,e1)
            ehi6 = max(ehi6,e2)
            mf6 => mf6%next
      end do

      npts = 0
      kid = 0
      rmf = .false.
      mtr%ndset = 0
      allocate(c4t(maxr))

      ! now go through list & locate data in C4 file.

      do

            read(22,'(a)',iostat=ios) line
            if(ios < 0) exit
            if(ios > 0) then
                  write(6,'(a)') ' Error reading fit file '//mtr%proj(1:mtr%npr)//'.fit'
                  stop 2
            endif
            if(len_trim(line) < 30) cycle
            if(line(1:1) == '#') cycle
            if(line(1:1) == '!') cycle

            read(line(35:),*) us,xx
            if(xx < 1.D-6) then
                  write(6,*) ' Data set found with no systematic uncertainty:'
                  write(6,*) trim(line)
                  stop 1
            endif

            ns = 0

            qf = .false.
            do j = 1,c4%nsec

                  sec => c4%sec(j)

                  if(line(26:30) /= sec%ent) cycle
                  if((line(31:31) /= '*') .and. (line(32:32) /= '*') .and. (line(33:33) /= '*')) then
                        if(line(31:33) /= sec%sub) cycle
                        if(line(34:34) /= sec%mdf) cycle
                  endif

                  qf = .true.

                  ! found a matching dataset in C4 file (sec)
                  ! go through and separate into different reaction groups

                  dscn: do i = 1,sec%ndat
                        cp => sec%pt(i)
                        do k = 1,ns
                              tm => c4t(k)
                              if(tm%mf /= cp%mf) cycle
                              if(tm%mt /= cp%mt) cycle
                              if(tm%flag(1:1) /= cp%pmeta)  cycle
                              if(tm%flag(2:2) /= cp%x4stat) cycle
                              if(tm%flag(3:3) /= cp%cm)     cycle
                              if(tm%flag(4:6) /= cp%fid)    cycle
                              select case(tm%mf)
                              case(4)
                                    if(abs(tm%e - cp%e) >  edfm) cycle
                              case(6)
                                    if(abs(tm%e - cp%e) >  edfm) cycle
                                    if(abs(tm%cth - cp%cos) >  adfm) cycle
                              end select
                              if(goodpt(tm,cp)) tm%ndat= tm%ndat + 1
                              cycle dscn
                        end do
                        ns = ns + 1
                        if(ns > maxr) then
                              write(6,*) ' Experiment contains too many reactions'
                              write(6,*) line
                              write(6,*) ' Increase maxr in emp_mat.f90'
                              stop 10
                        endif
                        tm => c4t(ns)
                        tm%mf = cp%mf
                        tm%mt = cp%mt
                        tm%flag = cp%pmeta//cp%x4stat//cp%cm//cp%fid
                        tm%ndat = 1
                          tm%mf3 => find(endf%mat%mf3,cp%mt)
                        select case(tm%mf)
                        case(4)
                              tm%e = cp%e
                              tm%mf4 => find(endf%mat%mf4,cp%mt)
                              tm%cth = 0.D0
                        case(6)
                              tm%e = cp%e
                              tm%cth = cp%cos
                        case default
                              tm%e = 0.D0
                              tm%mf4 => null()
                              tm%cth = 0.D0
                        end select
                        if(.not.goodpt(tm,cp)) ns = ns - 1
                  end do dscn

            end do

            if(.not.qf) then
                  write(6,*) ' Data set not found in C4 file'
                  write(6,*) ' ENT,SUB: ',line(26:34)
                  cycle
            endif

            ! get total number of good data points

            j = 0
            m = 0
            do i = 1,ns
                  j = j + c4t(i)%ndat
                  if(c4t(i)%ndat > 0) m = m + 1
            end do

            if(j == 0) cycle   ! no data

            ! at this point we have a good dataset

            ds => nxt_ds()

            ds%ngrp = m
            ! ds%tmeta = sec%tmeta
            ds%ref = line(1:25)
            ds%ent = line(26:30)
            ds%sub = line(31:33)
            ds%mdf = line(34:34)
            ds%uns = us/100.D0
            ds%next => null()
            allocate(ds%gp(m))

            mtr%ndset = mtr%ndset + 1
            ds%id = mtr%ndset
            uang = 0.D0

            write(6,*) ds%ref

            ! sort data from C4 file

            m = 0
            do i = 1,ns

                  tm => c4t(i)
                  if(tm%ndat < 1) cycle

                  iof = iof + 1
                  kid = kid + 1
                  m = m + 1
                  dg => ds%gp(m)
                  dg%mf = tm%mf
                  dg%mt = tm%mt
                  dg%inc = .true.
                  dg%fit = .true.
                  dg%id = kid
                  dg%ix = iof
                  dg%ix2 = 0
                  dg%flag = tm%flag
                  dg%ndat = tm%ndat
                  dg%e = tm%e
                  dg%cth = tm%cth
                  dg%sig = 0.D0
                  dg%dsig = 0.D0
                  dg%xcl = 1.D0
                  dg%dxcl = 0.D0
                  dg%unc = xx/100.D0
                  allocate(dg%pt(dg%ndat))
                  rmf(dg%mf) = .true.
                  e1 = 2.0D+7
                  e2 = 0.D0
                  qlab = dg%flag(3:3) == ' '

                  n = 0

                  do k = 1,c4%nsec

                        sec => c4%sec(k)

                        if(line(26:30) /= sec%ent) cycle
                        if((line(31:31) /= '*') .and. (line(32:32) /= '*') .and. (line(33:33) /= '*')) then
                              if(line(31:33) /= sec%sub) cycle
                              if(line(34:34) /= sec%mdf) cycle
                        endif

                        select case(dg%mf)
                        case(3)

                              ! regular cross sections

                              call set_interp(tm%mf3%tb)

                              do j = 1,sec%ndat
                                    cp => sec%pt(j)
                                    if(tm%mf /= cp%mf) cycle
                                    if(tm%mt /= cp%mt) cycle
                                    if(tm%flag(1:1) /= cp%pmeta)  cycle
                                    if(tm%flag(2:2) /= cp%x4stat) cycle
                                    if(tm%flag(3:3) /= cp%cm)     cycle
                                    if(tm%flag(4:6) /= cp%fid)    cycle
                                    if(.not.goodpt(tm,cp)) cycle
                                    n = n + 1
                                    pt => dg%pt(n)
                                    if(cp%dx <= 0.0) then
                                          if(uang == 0.D0) then
                                                write(6,*) ' Zero unc found in ', ds%ref
                                                write(6,'(a,$)'), ' Enter default %unc for dataset: '
                                                read(5,*) uang
                                                if(uang <= 0.D0) stop 1
                                                uang = min(uang,100.D0)/100.D0
                                          endif
                                          cp%dx = uang*cp%x
                                    endif
                                    pt%x  = cp%e      ! energy (eV)
                                    pt%yd = cp%x      ! cross section
                                    if(pt%yd <= 0.D0) then
                                          write(6,'(a,i0)') ' Non-positive cross section for datum ',j
                                          write(6,*) ' Data set: ',ds%ref
                                          stop 1
                                    endif
                                    pt%y0 = intrp(pt%x)
                                    pt%er2 = (cp%dx)**2 - (ds%uns*cp%x)**2
                                    if(pt%er2 <= 0.D0) then
                                          write(6,'(a,i0)') ' Non-positive unc for datum ',j
                                          write(6,*) ' Data set: ',ds%ref
                                          stop 1
                                    endif
                                    pt%ud = sqrt(pt%er2)
                                    rlm = pt%ud/pt%yd
                                    if(rlm < 1.D-4) then
                                          write(6,'(a,i0)') ' Uncertainty too small for datum ',j
                                          write(6,*) ' Data set: ',ds%ref
                                          stop 1
                                    endif
                                    allocate(pt%ep(mtr%nparm))
                              end do

                        case(4)

                              ! MF4 elastic ang dists

                              call set_interp(tm%mf3%tb)

                              do j = 1,sec%ndat
                                    cp => sec%pt(j)
                                    if(tm%mf /= cp%mf) cycle
                                    if(tm%mt /= cp%mt) cycle
                                    if(abs(tm%e - cp%e) >  edfm) cycle
                                    if(tm%flag(1:1) /= cp%pmeta)  cycle
                                    if(tm%flag(2:2) /= cp%x4stat) cycle
                                    if(tm%flag(3:3) /= cp%cm)     cycle
                                    if(tm%flag(4:6) /= cp%fid)    cycle
                                    if(.not.goodpt(tm,cp)) cycle
                                    n = n + 1
                                    pt => dg%pt(n)

                                    if(cp%dx <= 0.0) then
                                          if(uang == 0.D0) then
                                                write(6,*) ' Zero unc in ', ds%ref, j
                                                write(6,'(a,$)'), ' Enter default %unc for dataset: '
                                                read(5,*) uang
                                                if(uang <= 0.D0) stop 1
                                                uang = min(uang,100.D0)/100.D0
                                          endif
                                          cp%dx = uang*cp%x
                                    endif
                                    pt%x  = cp%cos    ! cos theta
                                    pt%yd = cp%x      ! cross section
                                    pt%y0 = intrp(cp%e)*ang_dist(tm%mf4,cp%e,pt%x,qlab)
                                    pt%er2 = (cp%dx)**2 - (ds%uns*cp%x)**2
                                    if(pt%yd <= 0.D0) then
                                          write(6,'(a,i0)') ' Non-positive cross section for datum ',j
                                          write(6,*) ' Data set: ',ds%ref
                                          stop 1
                                    endif
                                    if(pt%er2 <= 0.D0) then
                                          write(6,'(a,i0)') ' Non-positive unc for datum ',j
                                          write(6,*) ' Data set: ',ds%ref
                                          stop 1
                                    endif
                                    pt%ud = sqrt(pt%er2)
                                    rlm = pt%ud/pt%yd
                                    if(rlm < 1.D-4) then
                                          write(6,'(a,i0)') ' Uncertainty too small for datum ',j
                                          write(6,*) ' Data set: ',ds%ref
                                          stop 1
                                    endif
                                    allocate(pt%ep(mtr%nparm))
                              end do

                        case(6)

                              ! energy dists

                              do j = 1,sec%ndat
                                    cp => sec%pt(j)
                                    if(tm%mf /= cp%mf) cycle
                                    if(tm%mt /= cp%mt) cycle
                                    if(abs(tm%e - cp%e) >  edfm) cycle
                                    if(abs(tm%cth - cp%cos) >  adfm) cycle
                                    if(tm%flag(1:1) /= cp%pmeta)  cycle
                                    if(tm%flag(2:2) /= cp%x4stat) cycle
                                    if(tm%flag(3:3) /= cp%cm)     cycle
                                    if(tm%flag(4:6) /= cp%fid)    cycle
                                    if(.not.goodpt(tm,cp)) cycle
                                    n = n + 1
                                    pt => dg%pt(n)

                                    if(cp%dx <= 0.0) then
                                          if(uang == 0.D0) then
                                                write(6,*) ' Zero unc in ', ds%ref, j
                                                write(6,'(a,$)'), ' Enter default %unc for dataset: '
                                                read(5,*) uang
                                                if(uang <= 0.D0) stop 1
                                                uang = min(uang,100.D0)/100.D0
                                                endif
                                          cp%dx = uang*cp%x
                                    endif
                                    pt%x  = cp%dat7   ! outgoing energy
                                    pt%yd = cp%x      ! cross section
                                    pt%y0 = 0.D0    ! unused - filled later with fitted value
                                    pt%er2 = (cp%dx)**2 - (ds%uns*cp%x)**2
                                    e1 = min(e1,pt%x)
                                    e2 = max(e2,pt%x)
                                    if(pt%yd <= 0.D0) then
                                          write(6,'(a,i0)') ' Non-positive cross section for datum ',j
                                          write(6,*) ' Data set: ',ds%ref
                                          stop 1
                                    endif
                                    if(pt%er2 <= 0.D0) then
                                          write(6,'(a,i0)') ' Non-positive unc for datum ',j
                                          write(6,*) ' Data set: ',ds%ref
                                          stop 1
                                    endif
                                    pt%ud = sqrt(pt%er2)
                                    rlm = pt%ud/pt%yd
                                    if(rlm < 1.D-4) then
                                          write(6,'(a,i0)') ' Uncertainty too small for datum ',j
                                          write(6,*) ' Data set: ',ds%ref
                                          stop 1
                                    endif
                              end do

                        end select

                  end do

                  if(n /= dg%ndat) then
                        write(6,*) ' Internal inconsistency parsing set ',ds%ref
                        stop 1
                  endif

                  npts = npts + n

                  if(dg%mf /= 6) cycle

                  ! make width parameter for spectra

                  iof = iof + 1
                  dg%ix2 = iof
                  dg%sig = 75.0
                  dg%wfit = .false.

                  ! fill continuous histogram from MF6

                  e1 = max(0.8*e1,elo6)
                  e2 = min(1.1*e2,ehi6)
                  lel = log(e1)
                  dif = (log(e2) - lel)/dble(nen)
                  allocate(dg%econ(0:nen))

                  do j = 0,nen
                        pt => dg%econ(j)
                        pt%x = exp(lel + dble(j)*dif) ! outgoing energy
                        pt%y0 = 0.D0
                        allocate(pt%ep(mtr%nparm))
                  end do

                  mf6 => endf%mat%mf6
                  do while(associated(mf6))
                        mf3 => find(endf%mat%mf3,mf6%mt)
                        if(associated(mf3)) then
                              call set_interp(mf3%tb)
                              do j = 0,nen
                                    pt => dg%econ(j)
                                    pt%y0 = pt%y0 + intrp(dg%e)*mf6_ene_dist(mf6,dg%e,0,1,0,pt%x,dg%cth,qlab)
                              end do
                        endif
                        mf6 => mf6%next
                  end do

                  ! fill discreet cross sections from MF4

                  allocate(dg%edsr(0:40))
                  do j = 0,40
                        pt => dg%edsr(j)
                        pt%x = -1.D0
                        pt%y0 = 0.D0
                        allocate(pt%ep(mtr%nparm))
                  end do

                  mf4 => endf%mat%mf4
                  do while(associated(mf4))
                        select case(mf4%mt)
                        case(2)
                              esl = 0.D0
                              pt => dg%edsr(0)
                        case(51:90)
                              esl = mtr%leve(mf4%mt-50)
                              pt => dg%edsr(mf4%mt-50)
                        case(91:)
                              exit
                        case default
                              cycle
                        end select
                        if(esl < 0.D0) exit
                        mf3 => find(endf%mat%mf3,mf4%mt)
                        if(associated(mf3)) then
                              call set_interp(mf3%tb)
                              pt%y0 = intrp(dg%e)*ang_dist(mf4,dg%e,dg%cth,esl,x,qlab)
                              pt%x = x
                        endif
                        mf4 => mf4%next
                  end do

            end do

            if(m /= ds%ngrp) then
                  write(6,*) ' Internal inconsistency parse C4 file'
                  stop 1
            endif

      end do

      mtr%ndgrp = kid
      mtr%npts = npts

      deallocate(c4t)
      call delete_c4(c4)

      write(6,*) ' Total # data points = ',npts

      end subroutine get_data

      !--------------------------------------------------------------------------------------

      logical*4 function goodpt(cs,pt)

      implicit none

      type (C4_sort), intent(in)    :: cs
      type (c4_data_point), intent(in) :: pt

      real*8 el,eh

      goodpt = .false.

      ! first check X4 status

      select case(ichar(pt%x4stat))
      case(ichar(' '))
      case(ichar('D'))
            ! write(6,*) ' Dependent data for ',trim(sec%ref)
      case(ichar('A'))
            ! write(6,*) ' Author approved data for ',trim(sec%ref)
      case(ichar('U'))
            ! write(6,*) ' Unnormalized data for ',trim(sec%ref)
      case(ichar('C'))
            ! write(6,*) ' Correlated data for ',trim(sec%ref)
      case(ichar('O'))
            ! write(6,*) ' Outdated data for ',trim(sec%ref)
            ! write(6,*) ' Data point will be skipped'
            return
      case(ichar('P'))
            ! write(6,*) ' Preliminary data for ',trim(sec%ref)
      case(ichar('R'))
            ! write(6,*) ' Renormalized data for ',trim(sec%ref)
      case(ichar('S'))
            ! write(6,*) ' Superceded data for ',trim(sec%ref)
            ! write(6,*) ' Data point  will be skipped'
            return
      case default
            ! write(6,*) ' X4 status = ',pt%x4stat
            ! write(6,*) ' Data point will be skipped'
            return
      end select

      ! now make sure we have Empire predictions for this point

      select case(pt%mf)
      case(3,4)
            if(.not.associated(cs%mf3)) return
            call tablim(cs%mf3%tb,el,eh)
            if(pt%e < el) return
            if(pt%e > eh) return
            if(pt%mf == 4) then
                  if(.not.associated(cs%mf4)) return
                  call mf4_elim(cs%mf4,el,eh)
                  if(pt%e < el) return
                  if(pt%e > eh) return
            endif
      case(6)
            ! only check in the incoming energy.
            ! assume that the outgoing energy & angle will be covered
            if(pt%e < elo6) return
            if(pt%e > ehi6) return
      case default
            return
      end select

      goodpt = .true.   ! data point OK
      return

      end function goodpt

      !--------------------------------------------------------------------------------------

      function nxt_ds

      type (data_set), pointer :: nxt_ds,ds

      if(.not.associated(mtr%ds)) then
            allocate(mtr%ds)
            nxt_ds => mtr%ds
            return
      endif

      ds => mtr%ds
      do while(associated(ds%next))
            ds => ds%next
      end do
      allocate(ds%next)
      nxt_ds => ds%next

      return
      end function nxt_ds

      !--------------------------------------------------------------------------------------

      subroutine mtmap(c4)

      implicit none

      type (c4_file), intent(inout) :: c4
      type (c4_section),    pointer :: sec
      type (c4_data_point), pointer :: pt

      integer*4 i,j,m

      ! for now, we only remap MF3 & 4, MT=51

      do i = 1,c4%nsec
            sec => c4%sec(i)
            do j = 1,sec%ndat
                  pt => sec%pt(j)

                  if(pt%mf < 3)   cycle
                  if(pt%mf > 4)   cycle
                  if(pt%mt /= 51) cycle

                  pt%mt = 0

                  if((pt%fid /= 'LVL') .and. (pt%fid /= 'EXC')) cycle
                  if(pt%dat8 /= 0.D0) cycle

                  do m = 1,mlev
                        if(abs(pt%dat7 - mtr%leve(m)) > 50.D0) cycle
                        pt%mt = 50 + m
                        exit
                  end do
            end do
      end do

      return
      end subroutine mtmap

      !--------------------------------------------------------------------------------------

      subroutine read_lev_file(proj)

      implicit none

      character*(*), intent(in) :: proj   ! file stem (no ext)

      integer*4, parameter :: levun = 10  ! fortran logical unit for reads

      logical*4 qex
      integer*4 i,k,ios
      real*8 ex
      character line*100, fin*10

      mtr%leve = -1.D0

      inquire(file=proj//'.lev',exist=qex)
      if(.not.qex) then
            write(6,*)' File ',proj,'.lev not found.'
            goto 10
      endif

      open(levun,file=proj//'.lev',status='old',iostat=ios,action='READ')

      write(fin,'(2I5)') mtr%atgt,mtr%ztgt

      line = ' '
      do while(line(6:15) /= fin)
            read(levun,'(a)',iostat=ios) line
            if(ios < 0) then
                  write(6,*)' Material not found in ',proj,'.lev'
                  goto 10
            else if(ios > 0) then
                  write(6,'(a)') ' Error reading level file ',proj,'.lev'
                  goto 10
            endif
      end do

      k = 0
      do while(k <= mlev)

            read(levun,'(a)',iostat=ios) line
            if(ios < 0) exit
            if(ios > 0) then
                  write(6,'(a)') ' Error reading level file ',proj,'.lev'
                  exit
            endif

            if(line(1:3) == '   ') cycle

            read(line(1:3),'(I3)',iostat=ios) i
            if(ios /= 0) then
                  write(6,'(a)') ' Error reading level file ',proj,'.lev'
                  exit
            endif

            if(i /= (k+1)) then
                  write(6,'(a,i0,3a)') ' Excited state ',k,' not found in ',proj,'.lev'
                  exit
            endif

            if(k == 0) then
                  k = 1
                  cycle
            endif

            read(line(6:14),*,iostat=ios) ex
            if(ios /= 0) then
                  write(6,'(a,i0,3a)') ' Error reading excited state ',k,' in ',proj,'.lev'
                  exit
            endif

            mtr%leve(k) = ex*1.0D+6

            k = k + 1

      end do

      close(levun)

      k = max(k-1,0)

      write(6,'(3a,i0)') ' Levels parsed from ',proj,'.lev = ',k
      return

10    write(6,*)' No inelastic states will be parsed from C4 file'
      return

      end subroutine read_lev_file

      !--------------------------------------------------------------------------------------

      subroutine tablim(tb,el,eh)

      implicit none

      type (tab1), intent(in) :: tb       ! TAB1 to search
      real*8, intent(out) :: el,eh        ! lowest & highest x with y > 0

      integer*4 il,ih

      il = 1
      do while(il < tb%np)
            if(tb%dat(il)%y > 0.0) exit
            il = il + 1
      end do
      el = tb%dat(il)%x

      ih = tb%np
      do while(ih > 1)
            if(tb%dat(ih)%y > 0.0) exit
            ih = ih - 1
      end do
      eh = tb%dat(ih)%x

      return
      end subroutine tablim

      !--------------------------------------------------------------------------------------

      subroutine setup_sens

      implicit none

      integer*4 i,j
      real*8 el,eh,lel,dif

      type (mf_3),          pointer :: mf3
      type (data_point),    pointer :: pt
      type (data_plot),     pointer :: plt

      ! set central values & allocate the sensitivites for reaction covariances & plots

      allocate(mtr%sen(mtr%nes,nmt),mtr%plt(nmt))

      do i = 1,nmt
            plt => mtr%plt(i)
            plt%mf = 3
            plt%mt = smt(i)
            plt%ex => null()
              mf3 => find(endf%mat%mf3,smt(i))
            if(associated(mf3)) then
                  call set_interp(mf3%tb)
                  do j = 1,mtr%nes
                        pt => mtr%sen(j,i)
                        pt%x = mtr%ene(j)
                        pt%y0 = intrp(pt%x)
                        pt%yd = 0.D0
                        allocate(pt%ep(mtr%nparm))
                  end do
                  call tablim(mf3%tb,el,eh)
                  lel = log(el)
                  dif = (log(eh) - lel)/dble(nen)
                  do j = 0,nen
                        pt => plt%pt(j)
                        pt%x = exp(lel + dble(j)*dif) ! energy
                        pt%y0 = intrp(pt%x)           ! central Empire value
                        pt%yd = pt%y0                 ! fitted value
                        allocate(pt%ep(mtr%nparm))
                  end do
            else
                  do j = 1,mtr%nes
                        pt => mtr%sen(j,i)
                        pt%x = mtr%ene(j)
                        pt%y0 = -1.0D0
                        pt%yd = -1.0D0
                  end do
                  lel = log(mtr%ene(1))
                  dif = (log(mtr%ene(mtr%nes)) - lel)/dble(nen)
                  do j = 0,nen
                        pt => plt%pt(j)
                        pt%x = exp(lel + dble(j)*dif) ! energy
                        pt%y0 = -1.0D0                ! central Empire value
                        pt%yd = -1.0D0                ! fitted value
                  end do
            endif
      end do

      return
      end subroutine setup_sens

      !--------------------------------------------------------------------------------------

      subroutine get_sens

      implicit none

      real*8, parameter :: mdif = 0.001         ! max allowed rel diff between calc & actual Empire prms
      real*8, parameter :: dlim = 0.00001       ! max allowed diff on test of + & - values

      logical*4 qlab
      integer*4 i,j,n,status
      real*8 x,x13,x21,x22,x32,x23,y21,y22,ym,yp,esl
      character line*200

        type (emp_param),     pointer :: pm
      type (mf_3),          pointer :: mf3
      type (mf_4),          pointer :: mf4
      type (mf_6),          pointer :: mf6
      type (data_set),      pointer :: ds
      type (data_group),    pointer :: dg
      type (data_point),    pointer :: pt
      type (data_plot),     pointer :: plt
      type (emp_sens),      pointer :: ep

        ! step through all the varied Empire parameters & calculate sensitivities

      do n = 1,mtr%nparm

            pm => mtr%prm(n)

            write(6,'(1x,a18,2x,F7.4)') pm%dir,real(pm%rel)

            line = mtr%proj(1:mtr%npr)//'_'//trim(pm%dir)

            call read_input_file(trim(line)//'plus/'//mtr%proj(1:mtr%npr)//'.inp')
            status = get_empire_prm(pm,pm%xp)
                if(status /= 0) then
                        write(6,*) ' Error reading Empire input file'
                        stop 5
                endif

            if(abs(((1.D0+pm%rel)*pm%x0 - pm%xp)) > abs(mdif*pm%x0)) then
                  write(6,*)' Large difference seen calc parameter & value in input file'
                  type '(a,3(2x,F10.6))',pm%dir,pm%rel,(1.D0+pm%rel)*pm%x0, pm%xp
                  stop 10
            endif

            status = read_endf_file(trim(line)//'plus/'//mtr%proj(1:mtr%npr)//'.endf',endf,0,rmf)
            if(status /= 0) then
                  write(6,*)
                  write(6,*) ' Error reading PLUS ENDF file'
                  stop 5
            endif

            ds => mtr%ds
            do while(associated(ds))
                  do i = 1,ds%ngrp
                        dg => ds%gp(i)
                        qlab = dg%flag(3:3) == ' '
                        select case(dg%mf)
                        case(3)
                                mf3 => find(endf%mat%mf3,dg%mt)
                              call set_interp(mf3%tb)
                              do j = 1,dg%ndat
                                    pt => dg%pt(j)
                                    pt%ep(n)%yp = intrp(pt%x)
                              end do
                        case(4)
                                mf3 => find(endf%mat%mf3,dg%mt)
                                mf4 => find(endf%mat%mf4,dg%mt)
                              call set_interp(mf3%tb)
                              do j = 1,dg%ndat
                                    pt => dg%pt(j)
                                    pt%ep(n)%yp = intrp(dg%e)*ang_dist(mf4,dg%e,pt%x,qlab)
                              end do
                        case(6)
                              ! continuous histogram from MF6
                              do j = 0,nen
                                    dg%econ(j)%ep(n)%yp = 0.D0
                              end do
                              mf6 => endf%mat%mf6
                              do while(associated(mf6))
                                    mf3 => find(endf%mat%mf3,mf6%mt)
                                    if(associated(mf3)) then
                                          call set_interp(mf3%tb)
                                          do j = 0,nen
                                                pt => dg%econ(j)
                                                pt%ep(n)%yp = pt%ep(n)%yp + intrp(dg%e)*mf6_ene_dist(mf6,dg%e,0,1,0,pt%x,dg%cth,qlab)
                                          end do
                                    endif
                                    mf6 => mf6%next
                              end do

                              ! discreet cross sections from MF4
                              mf4 => endf%mat%mf4
                              do while(associated(mf4))
                                    select case(mf4%mt)
                                    case(2)
                                          esl = 0.D0
                                          pt => dg%edsr(0)
                                    case(51:90)
                                          esl = mtr%leve(mf4%mt-50)
                                          pt => dg%edsr(mf4%mt-50)
                                    case(91:)
                                          exit
                                    case default
                                          cycle
                                    end select
                                    mf3 => find(endf%mat%mf3,mf4%mt)
                                    if(associated(mf3)) then
                                          call set_interp(mf3%tb)
                                          pt%ep(n)%yp = intrp(dg%e)*ang_dist(mf4,dg%e,dg%cth,esl,x,qlab)
                                          pt%x = x
                                    endif
                                    mf4 => mf4%next
                              end do

                        end select
                  end do
                  ds => ds%next
            end do

            do i = 1,nmt
                    mf3 => find(endf%mat%mf3,smt(i))
                  if(.not.associated(mf3)) cycle
                  call set_interp(mf3%tb)
                  do j = 1,mtr%nes
                        pt => mtr%sen(j,i)
                        pt%ep(n)%yp = intrp(pt%x)
                  end do
                  plt => mtr%plt(i)
                  do j = 0,nen
                        pt => plt%pt(j)
                        pt%ep(n)%yp = intrp(pt%x)
                  end do
            end do

            status = del_endf(endf)

            call read_input_file(trim(line)//'minus/'//mtr%proj(1:mtr%npr)//'.inp')
            status = get_empire_prm(pm,pm%xm)
                if(status /= 0) then
                        write(6,*) ' Error reading Empire input file'
                        stop 5
                endif

            if(abs(((1.D0-pm%rel)*pm%x0 - pm%xm)) > abs(mdif*pm%x0)) then
                  write(6,*)' Large difference seen calc parameter & value in input file'
                  type '(a,3(2x,F10.6))',pm%dir,pm%rel,(1.D0-pm%rel)*pm%x0, pm%xm
                  stop 10
            endif

            status = read_endf_file(trim(line)//'minus/'//mtr%proj(1:mtr%npr)//'.endf',endf,0,rmf)
            if(status /= 0) then
                  write(6,*)
                  write(6,*) ' Error reading ',trim(line)//'minus/'//mtr%proj(1:mtr%npr)//'.endf'
                  stop 5
            endif

            ds => mtr%ds
            do while(associated(ds))
                  do i = 1,ds%ngrp
                        dg => ds%gp(i)
                        qlab = dg%flag(3:3) == ' '
                        select case(dg%mf)
                        case(3)
                                mf3 => find(endf%mat%mf3,dg%mt)
                              call set_interp(mf3%tb)
                              do j = 1,dg%ndat
                                    pt => dg%pt(j)
                                    pt%ep(n)%ym = intrp(pt%x)
                              end do
                        case(4)
                                mf3 => find(endf%mat%mf3,dg%mt)
                                mf4 => find(endf%mat%mf4,dg%mt)
                              call set_interp(mf3%tb)
                              do j = 1,dg%ndat
                                    pt => dg%pt(j)
                                    pt%ep(n)%ym = intrp(dg%e)*ang_dist(mf4,dg%e,pt%x,qlab)
                              end do
                        case(6)
                              ! continuous histogram from MF6
                              do j = 0,nen
                                    dg%econ(j)%ep(n)%ym = 0.D0
                              end do
                              mf6 => endf%mat%mf6
                              do while(associated(mf6))
                                    mf3 => find(endf%mat%mf3,mf6%mt)
                                    if(associated(mf3)) then
                                          call set_interp(mf3%tb)
                                          do j = 0,nen
                                                pt => dg%econ(j)
                                                pt%ep(n)%ym = pt%ep(n)%ym + intrp(dg%e)*mf6_ene_dist(mf6,dg%e,0,1,0,pt%x,dg%cth,qlab)
                                          end do
                                    endif
                                    mf6 => mf6%next
                              end do

                              ! discreet cross sections from MF4
                              mf4 => endf%mat%mf4
                              do while(associated(mf4))
                                    select case(mf4%mt)
                                    case(2)
                                          esl = 0.D0
                                          pt => dg%edsr(0)
                                    case(51:90)
                                          esl = mtr%leve(mf4%mt-50)
                                          pt => dg%edsr(mf4%mt-50)
                                    case(91:)
                                          exit
                                    case default
                                          cycle
                                    end select
                                    mf3 => find(endf%mat%mf3,mf4%mt)
                                    if(associated(mf3)) then
                                          call set_interp(mf3%tb)
                                          pt%ep(n)%ym = intrp(dg%e)*ang_dist(mf4,dg%e,dg%cth,esl,x,qlab)
                                          pt%x = x
                                    endif
                                    mf4 => mf4%next
                              end do
                        end select
                  end do
                  ds => ds%next
            end do

            do i = 1,nmt
                    mf3 => find(endf%mat%mf3,smt(i))
                  if(.not.associated(mf3)) cycle
                  call set_interp(mf3%tb)
                  do j = 1,mtr%nes
                        pt => mtr%sen(j,i)
                        pt%ep(n)%ym = intrp(pt%x)
                  end do
                  plt => mtr%plt(i)
                  do j = 0,nen
                        pt => plt%pt(j)
                        pt%ep(n)%ym = intrp(pt%x)
                        pt%ep(n)%ym = intrp(pt%x)
                  end do
            end do

            status = del_endf(endf)

            x13 = pm%xm - pm%xp
            x21 = pm%x0 - pm%xm
            x23 = pm%xp - pm%x0
            x22 = pm%x0*pm%x0 - pm%xm*pm%xm
            x32 = pm%xp*pm%xp - pm%xm*pm%xm

            if(x13 == 0.D0) write(6,*) ' NO DIFF', pm%dir

            ds => mtr%ds
            do while(associated(ds))
                  do i = 1,ds%ngrp
                        dg => ds%gp(i)
                        select case(dg%mf)
                        case(3,4)
                              do j = 1,dg%ndat
                                    ep => dg%pt(j)%ep(n)
                                    y21 = dg%pt(j)%y0 - ep%ym
                                    y22 = ep%yp - ep%ym
                                    ep%s2 = (y21*x13 + y22*x21)/(x13*x22 + x21*x32)
                                    ep%s1 = y21/x21 + x21*ep%s2
                                    ! ep%s1 = -y22/x13
                                    pm%ms1 = max(pm%ms1,abs(ep%s1))
                                    pm%ms2 = max(pm%ms2,abs(ep%s2))
                                    !type *, ep%ym,dg%pt(j)%y0,ep%yp
                                    ym = dg%pt(j)%y0 - x21*(ep%s1 - x21*ep%s2)
                                    yp = dg%pt(j)%y0 + x23*(ep%s1 + x23*ep%s2)
                                    !type *, ym,dg%pt(j)%y0,yp
                                    if((abs(ep%ym-ym) > dlim) .or. (abs(ep%yp-yp) > dlim)) then
                                          type *,' Data set',i,j
                                          type *, ep%ym,dg%pt(j)%y0,ep%yp
                                          type *, ym,dg%pt(j)%y0,yp
                                          type *, ep%s1 - (y21/x21 + x21*ep%s2)
                                    endif
                                    ep => null()
                              end do
                        case(6)
                              do j = 1,nen
                                    ep => dg%econ(j)%ep(n)
                                    y21 = dg%econ(j)%y0 - ep%ym
                                    y22 = ep%yp - ep%ym
                                    ep%s2 = (y21*x13 + y22*x21)/(x13*x22 + x21*x32)
                                    ep%s1 = y21/x21 + x21*ep%s2
                                    pm%ms1 = max(pm%ms1,abs(ep%s1))
                                    pm%ms2 = max(pm%ms2,abs(ep%s2))
                                    ym = dg%econ(j)%y0 - x21*(ep%s1 - x21*ep%s2)
                                    yp = dg%econ(j)%y0 + x23*(ep%s1 + x23*ep%s2)
                                    if((abs(ep%ym-ym) > dlim) .or. (abs(ep%yp-yp) > dlim)) then
                                          type *,' Data set, MF6 spectra ',i,j
                                          type *, ep%ym,dg%econ(j)%y0,ep%yp
                                          type *, ym,dg%econ(j)%y0,yp
                                    endif
                                    ep => null()
                              end do
                              do j = 0,40
                                    ep => dg%edsr(j)%ep(n)
                                    y21 = dg%edsr(j)%y0 - ep%ym
                                    y22 = ep%yp - ep%ym
                                    ep%s2 = (y21*x13 + y22*x21)/(x13*x22 + x21*x32)
                                    ep%s1 = y21/x21 + x21*ep%s2
                                    pm%ms1 = max(pm%ms1,abs(ep%s1))
                                    pm%ms2 = max(pm%ms2,abs(ep%s2))
                                    ym = dg%edsr(j)%y0 - x21*(ep%s1 - x21*ep%s2)
                                    yp = dg%edsr(j)%y0 + x23*(ep%s1 + x23*ep%s2)
                                    if((abs(ep%ym-ym) > dlim) .or. (abs(ep%yp-yp) > dlim)) then
                                          type *,' Data set, discreet ',i,j
                                          type *, ep%ym,dg%econ(j)%y0,ep%yp
                                          type *, ym,dg%econ(j)%y0,yp
                                    endif
                                    ep => null()
                              end do
                        end select
                  end do
                  ds => ds%next
            end do

            do i = 1,nmt

                  do j = 1,mtr%nes
                        if(.not.allocated(mtr%sen(j,i)%ep)) cycle
                        ep => mtr%sen(j,i)%ep(n)
                        y21 = mtr%sen(j,i)%y0 - ep%ym
                        y22 = ep%yp - ep%ym
                        ep%s2 = (y21*x13 + y22*x21)/(x13*x22 + x21*x32)
                        ep%s1 = y21/x21 + x21*ep%s2
                        ! ep%s1 = -y22/x13
                        ym = mtr%sen(j,i)%y0 - x21*(ep%s1 - x21*ep%s2)
                        yp = mtr%sen(j,i)%y0 + x23*(ep%s1 + x23*ep%s2)
                        ! type *, j,real(ym),real(mtr%sen(j,i)%y0),real(yp)
                        ! type *, j,real(ep%s1),real((ep%ym-ep%yp)/x13)
                        ! type *, j,real(pm%xm), real(pm%x0), real(pm%xp)
                        if((abs(ep%ym-ym) > dlim) .or. (abs(ep%yp-yp) > dlim)) then
                              type *,' SENS set',i,j
                              type *, ep%ym,mtr%sen(j,i)%y0,ep%yp
                              type *, ym,mtr%sen(j,i)%y0,yp
                              type *, ep%s1 - (y21/x21 + x21*ep%s2)
                        endif
                        ep => null()
                  end do

                  plt => mtr%plt(i)
                  do j = 0,nen
                        if(.not.allocated(plt%pt(j)%ep)) cycle
                        ep => plt%pt(j)%ep(n)
                        y21 = plt%pt(j)%y0 - ep%ym
                        y22 = ep%yp - ep%ym
                        ep%s2 = (y21*x13 + y22*x21)/(x13*x22 + x21*x32)
                        ep%s1 = y21/x21 + x21*ep%s2
                        ym = plt%pt(j)%y0 - x21*(ep%s1 - x21*ep%s2)
                        yp = plt%pt(j)%y0 + x23*(ep%s1 + x23*ep%s2)
                        if((abs(ep%ym-ym) > dlim) .or. (abs(ep%yp-yp) > dlim)) then
                              type *,' PLT set',i,j
                              type *, ep%ym,plt%pt(j)%y0,ep%yp
                              type *, ym,plt%pt(j)%y0,yp
                              type *, ep%s1 - (y21/x21 + x21*ep%s2)
                        endif
                        ep => null()
                  end do

            end do

      end do

      return
      end subroutine get_sens

      !--------------------------------------------------------------------------------------

      subroutine show_sens(imt,ip)

      implicit none

      integer*4, intent(in) :: imt  ! MT (MF=3) to show
      integer*4, intent(in) :: ip   ! Empire parameter index to show

      integer*4 j
      type (data_point), pointer :: pt
      type (emp_sens), pointer :: ep

      ! show the lin & quad sensitivites of reaction MF=3,MT to Empire parameter ip
      ! only show sensitivies at energies actually calculated by Empire

      if(.not.qfit)      return
      if(imt < 1)        return
      if(imt > nmt)      return
      if(ip < 1)         return
      if(ip > mtr%nparm) return

      write(6,'(a,i0,a,a)') 'Sensitivity of MT = ',smt(imt),' to Empire parameter ',mtr%prm(ip)%dir
      write(6,*) '    E             lin            quad'
      do j = 1,mtr%nes
            pt => mtr%sen(j,imt)
            if(.not.allocated(pt%ep)) cycle
            ep => pt%ep(ip)
            write(6,'(2x,1PE9.3,3(2x,1PE13.5),2x,2(3x,1PE13.5))') pt%x,ep%ym,pt%y0,ep%yp,ep%s1,ep%s2
      end do

      return
      end subroutine show_sens

      !--------------------------------------------------------------------------------------

      subroutine excl_mt(mt)

      implicit none

      integer*4, intent(in) :: mt

      ! exclude data groups with specified MT from fit
      ! if this removed sets with fitted scales, fix them as well.
      ! also fix the reaction scale for this MT

      integer*4 i,stat

      type (data_set),   pointer :: ds
      type (data_group), pointer :: dg

      ds => mtr%ds
      do while(associated(ds))
            do i = 1,ds%ngrp
                  dg => ds%gp(i)
                  if(dg%mt /= mt) cycle
                  stat = inc_grp(dg,.false.)
            end do
            ds => ds%next
      end do

      return
      end subroutine excl_mt

      !--------------------------------------------------------------------------------------

      subroutine incl_mt(mt)

      implicit none

      integer*4, intent(in) :: mt

      integer*4 i,stat

      ! include data groups with specifed MT into fit
      ! do not release the scales - let user decided 

      type (data_set),   pointer :: ds
      type (data_group), pointer :: dg

      ds => mtr%ds
      do while(associated(ds))
            do i = 1,ds%ngrp
                  dg => ds%gp(i)
                  if(dg%mt /= mt) cycle
                  stat = inc_grp(dg,.true.)
            end do
            ds => ds%next
      end do

      return
      end subroutine incl_mt

      !--------------------------------------------------------------------------------------

      subroutine show_dataset(ds)

      implicit none

      type (data_set), intent(in), target :: ds ! data set

      integer*4 i,n

      type (data_group), pointer :: dg
      type (data_point), pointer :: pt

      write(6,'(2a)')   ' Data set : ',ds%ref
      write(6,'(a,i0)') ' # groups : ',ds%ngrp

      do n = 1,ds%ngrp

            dg => ds%gp(n)

            write(6,*)
            write(6,'(a,i2,a,i4)') ' MF:',dg%mf,'   MT:',dg%mt

            if(dg%inc) then
                  write(6,*)'Set included in fit'
                  if(dg%fit) then
                        write(6,*)'Scale factor free'
                  else
                        write(6,*)'Scale factor fixed'
                  endif
            else
                  write(6,*)'Set excluded from fit'
            endif

            write(6,'(a,f7.3)') ' Systematic unc = ',dg%unc
            write(6,'(a,f7.3,a,f7.3)') ' Scale factor = ',dg%xcl,' +/- ',dg%dxcl
            write(6,*)

            select case(dg%mf)
            case(3)
                  write(6,'(a)') '      E           Data        Empire'
                  do i = 1,dg%ndat
                        pt => dg%pt(i)
                        write(6,'(3x,f10.1,2(2x,E10.4E1))') pt%x,pt%yd,pt%y0
                  end do
            case(4)
                  write(6,'(a,f10.1)') ' Incident energy = ',dg%e
                  write(6,'(a)') '      cos       Data      Emp'
                  do i = 1,dg%ndat
                        pt => dg%pt(i)
                        write(6,'(3x,3(2x,F8.5))') pt%x,pt%yd,pt%y0
                  end do
            end select

      end do

      return
      end subroutine show_dataset

      !--------------------------------------------------------------------------------------

      subroutine ave_group(dg,nav)

      implicit none

      type (data_group), intent(inout), target :: dg  ! data groups to average
      integer*4, intent(in) :: nav              ! # points to average over

      integer*4 i,j,k,m,mpt

      real*8 y1,y2,e1,de

      type (data_point), pointer :: pt,t1,t2,ap(:)

      mpt = ceiling(real(dg%ndat)/real(nav))
      allocate(ap(mpt))

      i = 0
      m = 0
      do while(i < dg%ndat)
            y1 = 0.D0
            y2 = 0.D0
            e1 = 0.D0
            k = min(i+nav,dg%ndat)
            do j = i+1,k
                  pt => dg%pt(j)
                  y1 = y1 + pt%yd/pt%er2
                  y2 = y2 + 1.D0/pt%er2
                  e1 = e1 + pt%x
            end do
            m = m + 1
            pt => ap(m)
            pt%x = e1/dble(k-i)
            ! type *,' ave',pt%x
            pt%yd = y1/y2
            pt%er2 = 1/y2
            pt%ud = dsqrt(pt%er2)
            j = i + 1
            do while(j < k)
                  ! data can be increasing or decreasing
                  if(dg%pt(j)%x < dg%pt(j+1)%x) then
                        if((pt%x > dg%pt(j)%x) .and. (pt%x < dg%pt(j+1)%x)) exit
                  else
                        if((pt%x < dg%pt(j)%x) .and. (pt%x > dg%pt(j+1)%x)) exit
                  endif
                  j = j + 1
            end do
            if(j < k) then
                  t1 => dg%pt(j)
                  t2 => dg%pt(j+1)
            else
                  t1 => dg%pt(i+1)
                  t2 => dg%pt(k)
            endif
            de = (pt%x - t1%x)/(t2%x - t1%x)
            pt%y0 = t1%y0 + (t2%y0 - t1%y0)*de
            ! type *,' Ave = ', j,pt%x,pt%yd,pt%ud
            allocate(pt%ep(mtr%nparm))
            do j = 1,mtr%nparm
                  pt%ep(j)%yp = t1%ep(j)%yp + (t2%ep(j)%yp - t1%ep(j)%yp)*de
                  pt%ep(j)%ym = t1%ep(j)%ym + (t2%ep(j)%ym - t1%ep(j)%ym)*de
                  pt%ep(j)%s1 = t1%ep(j)%s1 + (t2%ep(j)%s1 - t1%ep(j)%s1)*de
                  pt%ep(j)%s2 = t1%ep(j)%s2 + (t2%ep(j)%s2 - t1%ep(j)%s2)*de
            end do
            do j = i+1,k
                  pt => dg%pt(j)
                  write(6,*) '       ',pt%x,pt%yd,pt%ud
            end do
            i = k
      end do

      if(m /= mpt) then
            write(6,*) ' Inconsistency when averaging data set '
            write(6,*) m,mpt
      endif

      do i = 1,dg%ndat
            pt => dg%pt(i)
            deallocate(pt%ep)
      end do
      deallocate(dg%pt)

      dg%ndat = mpt
      dg%pt => ap

      return
      end subroutine ave_group

      !--------------------------------------------------------------------------------------

      subroutine add_curve(endfile,icol,nmt)

      implicit none

      character*(*), intent(in) :: endfile            ! ENDF file
      integer*4, intent(in) :: icol             ! gnuplot color
      integer*4, optional, intent(in) :: nmt          ! MT to add, if not present or 0 -> add all

      logical*4 rf(40)
      integer*4 i,j,mt,status,iz,ia
      real*8 el,eh,lel,dif
      type (ext_curve), pointer :: cv,dg

      type (endf_file) endf
      type (mf_3), pointer :: mf3
      type (data_plot), pointer :: plt

      if(.not.present(nmt)) then
            mt = 0
      else
            mt = nmt
      endif

      rf = .false.
      rf(3) = .true.          ! only read in file 3

      status = read_endf_file(endfile,endf,0,rf)
      if(status /= 0) then
            write(6,*)
            write(6,*) ' Error reading ENDF file ',endfile
            return
      endif

      iz = int(endf%mat%mf1%za/1000.D0)
      ia = mod(int(endf%mat%mf1%za),1000)

      if(iz /= mtr.ztgt) then
            write(6,*) ' Z of ENDF file does not match current material'
            write(6,*) ' No curves added to plots'
            goto 10
      endif

      if(ia /= mtr.atgt) then
            write(6,*) ' A of ENDF file does not match current material'
            write(6,*) ' No curves added to plots'
            goto 10
      endif

      do i = 1,nmt
            plt => mtr%plt(i)
            if((mt /= 0) .and. (mt /= plt%mt)) cycle
              mf3 => find(endf%mat%mf3,smt(i))
            if(.not.associated(mf3)) cycle
            call set_interp(mf3%tb)
            allocate(dg)
            if(.not.associated(plt%ex)) then
                  plt%ex => dg
                  cv => plt%ex
            else
                  cv => plt%ex
                  do while(associated(cv%next))
                        cv => cv%next
                  end do
                  cv%next => dg
                  cv => cv%next
            endif
            cv%next => null()
            cv%col = icol
            cv%endf = endfile
            call tablim(mf3%tb,el,eh)
            lel = log(el)
            dif = (log(eh) - lel)/dble(nen)
            do j = 0,nen
                  cv%pt(j)%x = exp(lel + dble(j)*dif)
                  cv%pt(j)%y = intrp(cv%pt(j)%x)
            end do
      end do

10    status = del_endf(endf)

      return
      end subroutine add_curve

      !--------------------------------------------------------------------------------------

      subroutine update_plt(plt)

      implicit none

      type (data_plot), intent(inout), target :: plt

      integer*4 i,n
      real*8 x,y

      type (data_point), pointer :: pt

      do i = 0,nen
            pt => plt%pt(i)
            if(.not.allocated(pt%ep)) then
                  pt%yd = pt%y0
            else
                  y = pt%y0
                  do n = 1,mtr%nparm
                        x = mtr%prm(n)%x
                        y = y + x*(pt%ep(n)%s1 + x*pt%ep(n)%s2)
                  end do
                  pt%yd = y
            endif
      end do

      return
      end subroutine update_plt

      !--------------------------------------------------------------------------------------

      subroutine show_data_group(dg)

      implicit none

      integer*4 i
      real*8 xn
      character set*4

      type (data_group), intent(in) :: dg
      type (data_point), pointer :: pt

      write(6,'(a,i0,a,i0)') ' Data group MF = ',dg%mf,'   MT = ',dg%mt
      write(6,'(a,i0)') ' Number of data points = ',dg%ndat
      write(6,'(a,1PE12.5)') ' Systematic unc  = ',dg%unc
      write(6,'(a,1PE12.5)') ' Group chi2 = ',dg%chi2
      if(dg%mf == 4) then
            write(6,'(a,1PE12.5)') ' Incident neutron energy = ',dg%e
            write(6,*) '    cos tht       Empire         Data           Unc'
      else if(dg%mf == 6) then
            write(6,'(a,1PE12.5)') ' Incident neutron energy = ',dg%e
            write(6,'(a,1PE12.5)') ' Outgoing neutron angle  = ',acosd(dg%cth)
            write(6,*) '    out E         Empire         Data           Unc'
      endif

      do i = 1,dg%ndat
            pt => dg%pt(i)
            write(6,'(4(2x,1PE12.5))') pt%x,pt%y0,pt%yd,pt%ud
      end do

      return
      end subroutine show_data_group

      !--------------------------------------------------------------------------------------

      subroutine show_rxscl

      implicit none

      integer*4 i

      type (rxn_scl), pointer :: xl

      write(6,*)
      write(6,'(3a,f8.5)') ' Empire material: ',mtr%proj(1:mtr%npr),'   Chi2 = ',mtr%chi2/dble(mtr%npts)
      write(6,'(a)')' Scl  MF MT  Fit   Value      Unc'

      do i = 1,mtr%nsmtg
            xl => rsx(i)
            write(6,10) xl%id,3,xl%mt,xl%fit,xl%xl,xl%unc,xl%num
      end do

      return

10    format(1x,i3,1x,i2,1x,i3,3x,L1,2(2x,F8.5),i6)

      end subroutine show_rxscl

      !--------------------------------------------------------------------------------------

      subroutine show_data

      implicit none

      integer*4 i
      real*8 xn
      character set*4

      type (data_set),   pointer :: ds
      type (data_group), pointer :: dg

      write(6,*)
      write(6,'(3a,f8.5)') ' Empire material: ',mtr%proj(1:mtr%npr),'   Chi2 = ',mtr%chi2/dble(mtr%npts)
      write(6,'(a)')' Set Grp MF  MT       Name                  Inc Fit   Value      Unc      Chi2'

      ds => mtr%ds
      do while(associated(ds))
            write(set,'(I4)') ds%id
            do i = 1,ds%ngrp
                  dg => ds%gp(i)
                  xn = dble(min(1,dg%ndat))
                  write(6,10) set,dg%id,dg%mf,dg%mt,ds%ref,dg%inc,dg%fit,dg%xcl,dg%dxcl,dg%chi2/xn
                  if(dg%mf == 6) write(6,20) dg%inc,dg%wfit,dg%sig,dg%dsig
                  set = '    '
            end do
            ds => ds%next
      end do

      return

10    format(a4,i4,1x,i2,i4,2x,a25,3x,L1,3x,L1,2(2x,F8.5),2x,F8.2)
20    format(45x,L1,3x,L1,2(2x,F8.5))

      end subroutine show_data

      !--------------------------------------------------------------------------------------

      subroutine show_params

      implicit none

      integer*4 i
      real*8 xrel,xunc,y

        type (emp_param), pointer :: prm

      write(6,*)
      write(6,'(2a)') ' Empire material: ',mtr%proj(1:mtr%npr)
      write(6,'(a)') ' Prm         Name           Fit   Value      Unc      % Dif    %Unc'
      do i = 1,mtr%nparm
            prm => mtr%prm(i)
            y = prm%x0 + prm%x
            if(prm%x0 /= 0.D0) then
                  xrel = 100.D0*prm%x/prm%x0
            else
                  xrel = 1.0
            endif
            if(y /= 0.D0) then
                  ! xunc = 100.D0*abs(prm%dx/prm%x)
                  xunc = 100.D0*abs(prm%dx/y)
            else
                  xunc = 0.D0
            endif
            write(6,10) prm%id,prm%dir,prm%fit,y,prm%dx,xrel,xunc
      end do

      return

10    format(1x,i3,2x,a18,5x,L1,2(2x,F8.5),2(1x,F8.3))

      end subroutine show_params

      !--------------------------------------------------------------------------------------

      subroutine save_params(sfil)

      implicit none

      ! save all parameters & errors to savefile

      character*(*), intent(in) :: sfil

      integer*4, parameter :: iun = 20

      logical*4 qex
      integer*4 i,j,m,ios
      real*8 xrel,xunc
      character*20 dum

      type (material),   pointer :: mt
      type (data_group), pointer :: dg
      type (data_set),   pointer :: ds
        type (emp_param),  pointer :: prm
      type (rxn_scl),    pointer :: xl

      inquire(file=sfil,exist=qex)
      if(qex) then
            write(6,'(3a,$)') ' Replace existing ',sfil,'? : '
            read(5,*) dum
            call lowercase(dum)
            if((dum(1:1) == 'y') .or. (dum(1:1) == 't')) then
                  open(iun,file=sfil,status='replace',recl=1000,iostat=ios)
            else
                  write(6,*) 'State not saved to ',sfil
                  return
            endif
      else
            open(iun,file=sfil,status='new',recl=1000,iostat=ios)
      endif
      if(ios /= 0) then
            write(5,*)' Error opening ',sfil
            return
      endif

      write(iun,*) nmat

      do m = 1,nmat

            mt => mat(m)

            write(iun,*)
            write(iun,'(2a)') ' Empire material: ',mt%proj
            write(iun,'(i0,2x,i3,2x,i3)') mt%nparm,mt%ndset,mt%nsmtg

            write(iun,*)
            write(iun,'(a)') ' Num         Name                Fit   Value      Unc       Rel     Rel-unc'
            do i = 1,mt%nparm
                  prm => mt%prm(i)
                  if(prm%x0 /= 0.D0) then
                        xrel = 100.D0*prm%x/prm%x0
                  else
                        xrel = 1.0
                  endif
                  if(prm%x /= 0.D0) then
                        xunc = 100.D0*abs(prm%dx/prm%x)
                  else
                        xunc = 0.D0
                  endif
                  write(iun,20) prm%id,prm%dir,prm%fit,prm%x0+prm%x,prm%dx,xrel,xunc
            end do

            write(iun,*)
            write(iun,'(a)') ' MF   MT  INC FIT  Value      Unc'
            ds => mt%ds
            do while(associated(ds))
                  write(iun,'(1x,i3,2x,a25,a5,a3,a1)') ds%ngrp, ds%ref, ds%ent, ds%sub, ds%mdf
                  do j = 1,ds%ngrp
                        dg => ds%gp(j)
                        select case(dg%mf)
                        case(3)
                              write(iun,10) dg%mf,dg%mt,dg%inc,dg%fit,dg%xcl,dg%dxcl,0.D0,0.D0
                        case(4)
                              write(iun,10) dg%mf,dg%mt,dg%inc,dg%fit,dg%xcl,dg%dxcl,dg%e,0.D0
                        case(6)
                              write(iun,10) dg%mf,dg%mt,dg%inc,dg%fit,dg%xcl,dg%dxcl,dg%e,dg%cth
                              write(iun,11) dg%wfit,dg%sig,dg%dsig
                        end select
                  end do
                  ds => ds%next
            end do

            write(iun,*)
            write(iun,'(a)')' Scl  MF MT  Fit   Value      Unc'
            do i = 1,mtr%nsmtg
                  xl => rsx(i)
                  write(iun,30) xl%id,3,xl%mt,xl%fit,xl%xl,xl%unc
            end do

      end do

      close(iun)

      write(6,*)' Fitting parameters saved to ',sfil

      return

10    format(1x,i2,1x,i4,3x,L1,2x,L1,2(2x,F8.5),2(2x,1PE12.5))
11      format(16x,L1,2(2x,F8.5))
20    format(1x,i3,2x,a18,10x,L1,2(2x,F8.5),2(1x,F8.3))
30    format(1x,i3,1x,i2,1x,i3,3x,L1,2(2x,F8.5))

      end subroutine save_params

      !--------------------------------------------------------------------------------------

      subroutine load_params(sfil)

      implicit none

      ! read all parameters from file specified

      character*(*), intent(in) :: sfil

      integer*4, parameter :: iun = 20

      logical*4 qf,qi,qw
      integer*4 i,j,m,k,nm,ndset,nparm,ngrp,ios,stat,nsmtg,mf,mt
      real*8 y,xcl,dxcl,sig,dsig,e,cth
      character name*32,ref*34,dir*18

      type (material),   pointer :: mts
      type (data_set),   pointer :: ds
      type (data_group), pointer :: dg
        type (emp_param),  pointer :: prm
      type (rxn_scl),    pointer :: xl

      open(iun,file=sfil,status='old',readonly,iostat=ios)
      if(ios /= 0) then
            write(6,*) ' Error opening ',sfil
            write(6,'(a,i0)') '  IOSTAT error: ',ios
            return
      endif

      mts => mtr  ! save current material

      read(iun,*,err=50,iostat=ios) nm

      do m = 1,nm

            read(iun,*,err=50,iostat=ios)
            read(iun,'(18x,a)',err=50,iostat=ios) name
            do i = 1,nmat
                  mtr => mat(i)
                  if(mtr%proj == name) exit
                  mtr => null()
            end do

            if(.not.associated(mtr)) write(6,*) ' Material not found: ',name
            write(6,*) ' Loading material ',name

            read(iun,*,err=50,iostat=ios) nparm,ndset,nsmtg
            read(iun,*,err=50,iostat=ios)
            read(iun,*,err=50,iostat=ios)

            write(6,*) ' Loading parameters'

            do i = 1,nparm
                  read(iun,'(6x,a18,10x,L1,2x,F8.5)',err=50,iostat=ios) dir,qf,y
                  if(.not.associated(mtr)) cycle
                  do j = 1,mtr%nparm
                        prm => mtr%prm(j)
                        if(prm%dir == dir) exit
                        prm => null()
                  end do
                  if(.not.associated(prm)) then
                        write(6,*) ' Parameter not found: ',dir
                        cycle
                  endif
                  ! write(6,'(3a,F10.5)') '  ',dir,' set to ',y
                  prm%x = y - prm%x0
                  prm%dx = 0.D0
                  stat = set_prm(prm%ix,prm%x)
                  if(qf == prm%fit) cycle
                  prm%fit = qf
                  if(.not.mtr%fit) cycle
                  if(qf) then
                        stat = rel_prm(prm%ix)
                  else
                        stat = fix_prm(prm%ix)
                  endif
            end do
            prm => null()

            read(iun,*,err=50,iostat=ios)
            read(iun,*,err=50,iostat=ios)

            write(6,*) ' Loading data scaling & width parameters'

            do i = 1,ndset

                  read(iun,'(1x,i3,2x,a34)',err=50,iostat=ios) ngrp, ref
                  if(associated(mtr)) then
                        ds => mtr%ds
                        do while(associated(ds))
                              if(ds%ref//ds%ent//ds%sub//ds%mdf == ref) exit
                              ds => ds%next
                        end do
                  else
                        ds => null()
                  endif
                  if(.not.associated(ds)) write(6,*) ' Data set not found: ',ref

                  grp: do j = 1,ngrp
                        read(iun,10,err=50,iostat=ios) mf,mt,qi,qf,xcl,dxcl,e,cth
                        if(mf == 6) read(iun,11,err=50,iostat=ios) qw,sig,dsig
                        ! type *,ds%ref,mt,qi,qf,qw,xcl,sig
                        if(.not.associated(ds)) cycle
                        do k = 1,ds%ngrp
                              dg => ds%gp(k)
                              if(dg%mf /= mf) cycle
                              if(dg%mt /= mt) cycle
                              select case(mf)
                              case(4)
                                    if(abs(dg%e - e) >  edfm) cycle
                              case(6)
                                    if(abs(dg%e - e) >  edfm) cycle
                                    if(abs(dg%cth - cth) >  adfm) cycle
                              end select
                              dg%xcl = xcl
                              dg%dxcl = 0.D0
                              stat = set_prm(dg%ix,dg%xcl)
                              if(dg%ix2 > 0) then
                                    dg%sig = sig
                                    dg%dsig = 0.D0
                                    stat = set_prm(dg%ix2,dg%sig)
                              endif
                              stat = inc_grp(dg,qi)
                              stat = fit_grp(dg,qf,qw)
                              cycle grp
                        end do
                        write(6,'(a,i0,a,i0,a)') ' Data group MF=',mf,' MT=',mt,' not found.'
                  end do grp

            end do

            read(iun,*,err=50,iostat=ios)
            read(iun,*,err=50,iostat=ios)

            write(6,*) ' Loading MT-group scaling factors'

            do j = 1,nsmtg

                  read(iun,30,err=50,iostat=ios) k,mf,mt,qf,xcl,dxcl
                  if(.not.associated(mtr)) cycle
                  ! type *,mf,mt,qf

                  do i = 1,mtr%nsmtg
                        xl => rsx(i)
                        if(xl%mt == mt) exit
                        xl => null()
                  end do
                  if(.not.associated(xl)) then
                        write(6,'(a,i0)') ' Scale group not found for MT = ',mt
                        cycle
                  endif
                  xl%xl = xcl
                  xl%unc = 0.D0
                  stat = set_prm(xl%ix,xl%xl)
                  if(qf == xl%fit) cycle
                  xl%fit = qf
                  if(.not.mtr%fit) cycle
                  if(qf) then
                        stat = rel_prm(xl%ix)
                  else
                        stat = fix_prm(xl%ix)
                  endif

            end do

      end do

      close(iun)
      mtr => mts  ! restore current material

      return

10    format(1x,i2,1x,i4,3x,L1,2x,L1,2(2x,F8.5),2(2x,1PE12.5))
11      format(16x,L1,2(2x,F8.5))
30    format(1x,i3,1x,i2,1x,i3,3x,L1,2(2x,F8.5))

50    close(iun)
      write(6,*) ' Error occured while reading ',sfil
      write(6,'(a,i0)') '  IOSTAT error: ',ios
      mtr => mts  ! restore current material
      return

      end subroutine load_params

      !--------------------------------------------------------------------------------------

      subroutine create_params

      implicit none

      ! create Minuit parameters for Material scale & Empire parameters

      integer*4 i,ierr,k
      character pnam*10

      type (data_set),   pointer :: ds
      type (data_group), pointer :: dg
      type (emp_param),  pointer :: pm
      type (rxn_scl),    pointer :: xl

      ! initialize scaling parameters

      do i = 1,200
            mtr%rxl(i)%ix = 0
            mtr%rxl(i)%id = 0
            mtr%rxl(i)%mt = 0
            mtr%rxl(i)%num = 0
            mtr%rxl(i)%fit = .false.
            mtr%rxl(i)%xl = 1.D0
            mtr%rxl(i)%unc = 0.D0
      end do

      if(.not.qfit) return

      ! create Empire parameters

      do i = 1,mtr%nparm
            pm => mtr%prm(i)
            call mnparm(pm%ix,pm%nam,pm%x,pm%rel/2.D0,0.D0,0.D0,ierr)
            if(ierr /= 0) then
                  write(6,*) ' Error creating Empire parameter ',pm%nam
                  stop 5
            endif
      end do

      ! next create scaling parameter for each data group

      ds => mtr%ds
      do while(associated(ds))
            do i = 1,ds%ngrp
                  dg => ds%gp(i)
                  pnam = 'Scl-'
                  write(pnam(5:7),'(i3.3)') dg%id
                  call mnparm(dg%ix,pnam,dg%xcl,0.01D0,0.D0,0.D0,ierr)
                  if(ierr /= 0) then
                        write(6,*) ' Error creating scale parameter for ',ds%ref
                        stop 5
                  endif
                  if(.not.dg%fit) then
                        ierr = fix_prm(dg%ix)
                        if(ierr /= 0) write(6,'(2a)') ' Error fixing scale parameter for ',ds%ref
                  endif
                  if(dg%ix2 == 0) cycle
                  pnam = 'Sig-'
                  write(pnam(5:7),'(i3.3)') dg%id
                  call mnparm(dg%ix2,pnam,dg%sig,0.01D0,0.D0,0.D0,ierr)
                  if(ierr /= 0) then
                        write(6,*) ' Error creating width parameter for ',ds%ref
                        stop 5
                  endif
                  if(.not.dg%wfit) then
                        ierr = fix_prm(dg%ix2)
                        if(ierr /= 0) write(6,'(2a)') ' Error fixing width for ',ds%ref
                  endif
            end do
            ds => ds%next
      end do

      ! create scaling parameters for each MT

      k = iof
      ds => mtr%ds
      do while(associated(ds))
            do i = 1,ds%ngrp
                  dg => ds%gp(i)
                  if(dg%mf /= 3) cycle
                  if(dg%mt > 200) cycle
                  xl => mtr%rxl(dg%mt)
                  xl%num = xl%num + 1
                  if(xl%num > 1) cycle
                  iof = iof + 1
                  xl%ix = iof
                  xl%id = iof - k
                  xl%mt = dg%mt
                  xl%fit = .true.
                  pnam = 'Rx-scl-'
                  write(pnam(8:10),'(i3.3)') dg%mt
                  call mnparm(xl%ix,pnam,xl%xl,0.01D0,0.D0,0.D0,ierr)
                  if(ierr /= 0) then
                        write(6,'(a,i0)') ' Error creating reaction scale parameter for MT = ',dg%mt
                        stop 5
                  endif
            end do
            ds => ds%next
      end do

      mtr%nsmtg = iof - k     ! # scaled MT groups

      ! go through and fix scale on any single data group

      ds => mtr%ds
      do while(associated(ds))
            do i = 1,ds%ngrp
                  dg => ds%gp(i)
                  if(dg%mf /= 3) cycle
                  if(dg%mt > 200) cycle
                  xl => mtr%rxl(dg%mt)
                  if(xl%num /= 1) cycle
                  dg%fit = .false.
                  dg%dxcl = 0.D0
                  ierr = fix_prm(dg%ix)
                  if(ierr /= 0) write(6,'(a,i0)') ' Error fixing group',dg%ix
            end do
            ds => ds%next
      end do

      return
      end subroutine create_params

      !--------------------------------------------------------------------------------------

      integer*4 function set_prm(ipm,xval)

      implicit none

      ! set a parameter to a given value

      integer*4, intent(in) :: ipm        ! absolute Minuit parameter
      real*8, intent(in) :: xval          ! value to set

      integer*4 ierr
      real*8 xrg(4)

      xrg(1) = dble(ipm)
      xrg(2) = xval
      xrg(3) = 0.D0

      call mnexcm(,'SET PAR',xrg,2,ierr,)
      if(ierr /= 0) write(6,*) ' Error setting parameter',ipm

      set_prm = ierr

      return
      end function set_prm

      !--------------------------------------------------------------------------------------

      subroutine get_params

      implicit none

      ! refresh the local values of all parameters
      ! and their errors with values from Minuit

      integer*4 i,m,ip
      real*8 bnd1,bnd2
      character parnam*10

        type (material),   pointer :: mr
      type (data_set),   pointer :: ds
      type (data_group), pointer :: dg
      type (emp_param),  pointer :: pm
      type (rxn_scl),    pointer :: xl

      do m = 1,nmat

            mr => mat(m)

            do i = 1,mr%nparm
                  pm => mr%prm(i)
                  call mnpout(pm%ix,parnam,pm%x,pm%dx,bnd1,bnd2,ip)
            end do

            ds => mr%ds
            do while(associated(ds))
                  do i = 1,ds%ngrp
                        dg => ds%gp(i)
                        call mnpout(dg%ix,parnam,dg%xcl,dg%dxcl,bnd1,bnd2,ip)
                        if(dg%mf /= 6) cycle
                        call mnpout(dg%ix2,parnam,dg%sig,dg%dsig,bnd1,bnd2,ip)
                  end do
                  ds => ds%next
            end do

            do i = 1,mr%nsmtg
                  xl => rsx(i)
                  call mnpout(xl%ix,parnam,xl%xl,xl%unc,bnd1,bnd2,ip)
            end do

      end do

      return
      end subroutine get_params

      !--------------------------------------------------------------------------------------

      integer*4 function fix_prm(ipm)

      implicit none

      ! tell Minuit to not vary (fit) this parameter
      ! the fit flag is cleared and unc set to 0.0

      integer*4, intent(in) :: ipm        ! absolute Minuit parameter

      integer*4 ierr
      real*8 xrg(2)

      if(ipm < 1) then
            write(6,'(a,i0,a)') ' Parameter ',ipm, ' undefined!'
            fix_prm = 1
            return
      endif

      xrg(1) = dble(ipm)
      xrg(2) = 0.D0

      call mnexcm(,'FIX',xrg,1,ierr,)
      if(ierr /= 0) write(6,*) ' Error fixing parameter',ipm

      fix_prm = ierr

      return
      end function fix_prm

      !--------------------------------------------------------------------------------------

      integer*4 function rel_prm(ipm)

      implicit none

      integer*4, intent(in) :: ipm        ! absolute Minuit parameter

      integer*4 ierr
      real*8 xrg(2)

      if(ipm < 1) then
            write(6,'(a,i3,a)') ' Parameter ',ipm, ' undefined!'
            rel_prm = 1
            return
      endif

      xrg(1) = dble(ipm)
      xrg(2) = 0.D0

      call mnexcm(,'REL',xrg,1,ierr,)
      if(ierr /= 0) write(6,*) ' Error releasing parameter',ipm

      rel_prm = ierr

      return
      end function rel_prm

      !--------------------------------------------------------------------------------------

      integer*4 function fit_grp(dg,qf,qw)

      implicit none

      ! set fitting of scale factor for data group gp in material mtr to qf

      type (data_group) dg                ! data group
      logical*4, intent(in) :: qf         ! true to fit scale, false to fix
      logical*4, intent(in), optional :: qw     ! true to fit width, false to fix

      integer*4 stat
      type (rxn_scl), pointer :: xl

      stat = 0

      if(qf /= dg%fit) then

            if(qf) then
                  if(.not.dg%inc) then
                        write(6,'(a,i0,a)') ' Group ',dg%id,' not included in fit.'
                        fit_grp = -1
                        return
                  endif
                  if(mtr%fit) then
                        stat = rel_prm(dg%ix)
                        if(stat /= 0) then
                              write(6,'(a,i0)') ' Error releasing scale for group', dg%id
                              fit_grp = stat
                              return
                        endif
                  endif
            else
                  dg%dxcl = 0.D0
                  if(mtr%fit) then
                        stat = fix_prm(dg%ix)
                        if(stat /= 0) then
                              write(6,'(a,i0)') ' Error fixing scale for group', dg%id
                              fit_grp = stat
                              return
                        endif
                  endif
            endif

      endif

      dg%fit = qf

      fit_grp = stat
      if(dg%mf /=6) return
      if(.not.present(qw)) return
      if(qw == dg%wfit) return

      if(qw) then
            if(.not.dg%inc) then
                  write(6,'(a,i0,a)') ' Group ',dg%id,' not included in fit.'
                  fit_grp = -1
                  return
            endif
            if(mtr%fit) then
                  stat = rel_prm(dg%ix2)
                  if(stat /= 0) then
                        write(6,'(a,i0)') ' Error releasing width for group ', dg%id
                        fit_grp = stat
                        return
                  endif
                  dg%wfit = .true.
            endif
      else
            dg%dsig = 0.D0
            if(mtr%fit) then
                  stat = fix_prm(dg%ix2)
                  if(stat /= 0) then
                        write(6,'(a,i0)') ' Error fixing width for group ', dg%id
                        fit_grp = stat
                        return
                  endif
            endif
      endif

      dg%wfit = qw

      fit_grp = stat

      return
      end function fit_grp

      !--------------------------------------------------------------------------------------

      integer*4 function inc_grp(dg,qf)

      implicit none

      ! set include status of group dg to qf

      type (data_group) dg
      logical*4, intent(in) :: qf   ! true to include, false to exclude

      integer*4 i,stat

      type (data_set),   pointer :: ds
      type (data_group), pointer :: dd
      type (rxn_scl),    pointer :: xl

      inc_grp = 0
      if(qf == dg%inc) return

      dg%inc = qf
      xl => mtr%rxl(dg%mt)

      if(qf) then
            ! don't enable fitting -- let user decide
            if(dg%mf == 3)    xl%num = xl%num + 1
            return
      endif

      stat = fit_grp(dg,.false.,.false.)
      if(dg%mf /= 3) return
      xl%num = xl%num - 1

      if(xl%num == 1) then
            if(xl%fit) then
                  ! a single set is left.
                  ! find it & fix it's scale
                  ds => mtr%ds
                  do while(associated(ds))
                        do i = 1,ds%ngrp
                              dd => ds%gp(i)
                              if(dd%mf /= 3)  cycle
                              if(dd%mt /= dg%mt) cycle
                              stat = fit_grp(dd,.false.)
                        end do
                        ds => ds%next
                  end do
            endif
      else if(xl%num == 0) then
            ! no groups left
            ! turn off MT scale fitting
            if(xl%fit) then
                  xl%fit = .false.
                  if(mtr%fit) then
                        stat = fix_prm(xl%ix)
                        if(stat /= 0) write(6,'(a,i0)') ' Error fixing scale',xl%id
                  endif
            endif
      endif

      inc_grp = stat

      return
      end function inc_grp

      !--------------------------------------------------------------------------------------

      subroutine fix_badprms(unc_lim)

      implicit none

      ! look through all parameters and if the relative
      ! uncertainty is above the specified user limit
      ! then fix it & set to original value.

      real*8, intent(in) :: unc_lim       ! uncertainty limit

      integer*4 i,stat
      real*8 xl

      type (data_set),   pointer  :: ds
      type (data_group), pointer :: dg
      type (emp_param),  pointer :: pm

      if(unc_lim > 0.D0) then
            xl = 100.D0/unc_lim
      else
            xl = 0.2
      endif

      do i = 1,mtr%nparm
            pm => mtr%prm(i)
            if(.not.pm%fit) cycle
            if(abs(pm%x0+pm%x) > xl*pm%dx) cycle
            write(6,*) ' Fixing Empire parameter ',pm%nam,real(pm%x0+pm%x),real(pm%dx)
            pm%fit = .false.
            pm%x = 0.D0
            pm%dx = 0.D0
            stat = set_prm(pm%ix,0.D0)
            stat = fix_prm(pm%ix)
      end do

      ds => mtr%ds
      do while(associated(ds))
            do i = 1,ds%ngrp
                  dg => ds%gp(i)
                  if(.not.dg%fit) cycle
                  if(abs(dg%xcl) > xl*dg%dxcl) cycle
                  write(6,*) ' Fixing scale parameter for ',ds%ref,real(dg%xcl),real(dg%dxcl)
                  dg%xcl = 1.D0
                  dg%dxcl = 1.D0
                  stat = set_prm(dg%ix,1.D0)
                  stat = fit_grp(dg,.false.)
            end do
            ds => ds%next
      end do

      return
      end subroutine fix_badprms

      !--------------------------------------------------------------------------------------

      subroutine fix_insen_params

      implicit none

      real*8, parameter :: min_s1 = 1.0D-04                 ! minimum allowed sens1
      real*8, parameter :: min_s2 = 1.0D-02                 ! minimum allowed sens2

      integer*4 i,m,stat

      type (emp_param), pointer :: pm
        type (material),  pointer :: mr

      ! look for insensitive Empire parameters & fix them

      do m = 1,nmat
            mr => mat(m)
            do i = 1,mr%nparm
                  pm => mr%prm(i)
                  if(.not.pm%fit) cycle
                  if(pm%ms1 > min_s1) cycle
                  if(pm%ms2 > min_s2) cycle
                  write(6,*) ' Fixing insensitive Empire parameter ',pm%dir
                  write(6,*) ' Maximum s1, s2 = ', real(pm%ms1), real(pm%ms2)
                  if(mr%fit .and. pm%fit) stat = fix_prm(pm%ix)
                  pm%fit = .false.
            end do
      end do

      return
      end subroutine fix_insen_params

      !--------------------------------------------------------------------------------------

      subroutine dump_minuit

      implicit none

      integer*4 i,j,k,np
      real*8 xx,dx,bd1,bd2
      character pnam*10

      write(6,*)'  Minuit parameters'
      do i = 1,nptot
            call mnpout(i,pnam,xx,dx,bd1,bd2,k)
            write(6,'(i4,2x,2(E12.4,2x),i3)') i,pnam,xx,dx,k
      end do

      return
      end subroutine dump_minuit

      !--------------------------------------------------------------------------------------

      subroutine get_prm_cov(cov)

      implicit none

      ! fetch parameter covariance matrix from Minuit

      real*8, intent(out), allocatable :: cov(:,:)    ! Minuit parameter covariance matrix

      integer*4 i,j,k,np
      real*8 xx,dx,bd1,bd2
      character pnam*10

      integer*4, allocatable :: map(:)
      real*8, allocatable :: cvi(:,:)

      if(allocated(cov)) deallocate(cov)
      allocate(cov(nptot,nptot),map(nptot))

      np = 0
      map = 0
      do i = 1,nptot
            call mnpout(i,pnam,xx,dx,bd1,bd2,k)
            if(k <= 0) cycle
            np = np + 1
            map(k) = i
      end do

      allocate(cvi(np,np))
      call mnemat(cvi,np)

      ! convert covariance from internal Minuit to external parameters

      cov = 0.D0
      do i = 1,np
            do j = 1,np
                  cov(map(i),map(j)) = cvi(i,j)
            end do
      end do

      deallocate(cvi,map)

      return
      end subroutine get_prm_cov

      !--------------------------------------------------------------------------------------

      subroutine get_reaction_cov(mr1,mt1,mr2,mt2,cov,m1,m2)

      implicit none

      ! from Empire parameter covariances calculate reaction energy covariances
      ! between material 1, MT1 and material 2, MT2.

      type (material), target, intent(in) :: mr1,mr2        ! materials 1 & 2
      integer*4, intent(in) :: mt1,mt2                ! MTs 1 & 2
      real*8, intent(out), allocatable :: cov(:,:)          ! covariance between them
      integer*4, intent(out) :: m1,m2                       ! MT1 & MT2 indicies

      integer*4 i,j,k,kpm
      real*8 xx
      real*8, allocatable :: w(:),cve(:,:),cvk(:,:),kun(:)

      type (emp_param), pointer :: pm1,pm2

      ! first check that we have sensitivities to these MTs

      m1 = 0
      do i = 1,nmt
            if(smt(i) == mt1) then
                  m1 = i
                  exit
            endif
      end do
      if(m1 == 0) then
            write(6,'(a,i0)') ' No sensitivies calculated for MT = ',mt1
            if(allocated(cov)) deallocate(cov)
            return
      endif

      m2 = 0
      do i = 1,nmt
            if(smt(i) == mt2) then
                  m2 = i
                  exit
            endif
      end do
      if(m2 == 0) then
            write(6,'(a,i0)') ' No sensitivies calculated for MT = ',mt2
            if(allocated(cov)) deallocate(cov)
            return
      endif

      if(allocated(cov)) deallocate(cov)

      ! fetch parameter covariances from Minuit

      call get_prm_cov(cve)

      ! add in systematic uncertainties where we have data

!     plt => mr1%plt
!     do while(associated(plt))
!           xx = system_scl(mr1,plt%mt,i)
!           if(i /= 0) then
!                 j = mr1%ieof + i
!                 cve(j,j) = cve(j,j) + xx
!           endif
!           plt => plt%next
!     end do

      if(kalman) then         ! replace with external covariances from KALMAN

            kpm = mr1%nparm
            allocate(cvk(kpm,kpm),kun(kpm))

            i = read_kalman_cov(kalfil,kpm,cvk,kun,.true.)
            if(i /= 0) then
                  deallocate(cvk,kun,cve)
                  m1 = 0
                  m2 = 0
                  return
            endif

            cve = 0.D0

            do i = 1,kpm
                  do j = 1,kpm
                        cve(i,j) = cvk(i,j)*kun(i)*kun(j)
                  end do
            end do

            deallocate(cvk,kun)

      endif

      ! cov <= sen * parcov * sen**t

      allocate(w(mr2%nparm),cov(mr1%nes,mr2%nes))

      do i = 1,mr1%nes

            do j = 1,mr2%nparm
                  pm2 => mr2%prm(j)
                  xx = 0.D0
                  do k = 1,mr1%nparm
                        pm1 => mr1%prm(k)
                        xx = xx + cve(pm1%ix,pm2%ix)*mr1%sen(i,m1)%ep(k)%s1
                  end do
                  w(j) = xx
            end do

            do j = 1,mr2%nes
                  xx = 0.D0
                  do k = 1,mr2%nparm
                        xx = xx + w(k)*mr2%sen(j,m2)%ep(k)%s1
                  end do
                  cov(i,j) = xx
            end do

      end do

      deallocate(cve,w)

      return
      end subroutine get_reaction_cov

      !--------------------------------------------------------------------------------------

      real*8 function system_scl(mtr,mt,ipm)

      ! DON'T USE THIS

      implicit none

      type (material), target, intent(in) :: mtr            ! material to get systematic errors
      integer*4, intent(in) :: mt                     ! MT
      integer*4, intent(out) :: ipm                   ! Empire parameter corresponding to this MT

      ! find an amount to add to unc in Empire parameter
      ! due to spead in the data sets. Take this from
      ! the fitted data sets for this material.

      integer*4, parameter :: nmt = 4
      integer*4, parameter :: smt(nmt) = (/1,2,18,102/)     ! only total, elastic, fission & capture supported (now)
      character*18, parameter :: sdir(nmt) = (/'TOTRED_00_00_00_00','CELRED_00_00_00_00','TUNEFI_00_00_00_00','TUNE_00_00_00_00  '/)

      integer*4 i,m
!     real*8 z,dz
!     type (data_set), pointer  :: ds

      ! first, see that we have an Empire parameter to apply data spreads to

      m = 1
      do while(m <= nmt)
            if(mt == smt(m)) exit
            m = m + 1
      end do
      if(m > nmt) then
            ! MT not supported
            system_scl = 0.D0
            ipm = 0
            return
      endif

      i = 1
      do while(i <= mtr%nparm)
            if(mtr%prm(i)%dir == sdir(m)) exit
            i = i + 1
      end do

      if(i > mtr%nparm) then
            ! parameter not found
            write(6,*) ' Sensitive parameter ',sdir(m),' not found in varied parameters'
            system_scl = 0.D0
            ipm = 0
            return
      endif

      ipm = i

      ! ok, now calculate the additional systematic variance

!     z = 0.D0
!     dz = 0.D0
!     do i = 1,mtr%ndset
!           ds => mtr%ds(i)
!           if(ds%mt /= mt) cycle
!           if(.not.ds%sfit) cycle
!           if(ds%dxcl <= 0.D0) cycle
!           y = max(abs(ds%xcl - 1.D0),ds%unm)  ! never let unc go below report sys unc
!           z = z + (y/ds%dxcl)**2
!           dz = dz + (1.D0/ds%dxcl)**2
!           ! z = z + (ds%unc/ds%dxcl)**2 + ((ds%xcl - 1.D0)/ds%dxcl)**2
!           ! dz = dz + 2.D0*(1.D0/ds%dxcl)**2
!     end do
!
!     if(dz > 0.D0) then
!           z = z/dz          ! average relative uncert**2 in param
!           z = z*(mtr%prm(ipm)%x0 + mtr%prm(ipm)%x)**2     ! abs uncert**2
!           system_scl = z          ! additional systematic error**2
!     else
!           system_scl = 0.D0
!     endif

      write(6,*) ' systematic err increased ',sdir(m),system_scl

      return
      end function system_scl

      !--------------------------------------------------------------------------------------

      real*8 function fitpt(pt)

      implicit none

      type (data_point), intent(in) :: pt

      integer*4 n
      real*8 x,y

      y = pt%y0
      do n = 1,mtr%nparm
            x = mtr%prm(n)%x
            y = y + x*(pt%ep(n)%s1 + x*pt%ep(n)%s2)
      end do

      fitpt = y

      return
      end function fitpt

      !--------------------------------------------------------------------------------------

      function rsx(id)

      implicit none

      ! return pointer to scale group id

      integer*4, intent(in) :: id
      type (rxn_scl), pointer  :: rsx

      integer*4 i

      rsx => null()

      if(id < 1)   return
      if(id > 200) return

      do i = 1,200
            if(mtr%rxl(i)%id /= id) cycle
            rsx => mtr%rxl(i)
            return
      end do

      return
      end function rsx

      !--------------------------------------------------------------------------------------

      function psx(id)

      implicit none

      ! return pointer to parameter id

      integer*4, intent(in) :: id

      type (emp_param), pointer  :: psx

      if((id < 1) .or. (id > mtr%nparm)) then
            psx => null()
      else
            psx => mtr%prm(id)
      endif

      return
      end function psx

      !--------------------------------------------------------------------------------------

      function dsx(id)

      implicit none

      ! return pointer to dataset id

      integer*4, intent(in) :: id

      type (data_set), pointer  :: ds,dsx

      if((id < 1) .or. (id > mtr%ndset)) then
            dsx => null()
            return
      endif

      ds => mtr%ds
      do while(associated(ds))
            if(ds%id == id) then
                  dsx => ds
                  return
            endif
            ds => ds%next
      end do

      dsx => null()

      return
      end function dsx

      !--------------------------------------------------------------------------------------

      function gsx(id)

      implicit none

      ! return pointer to data group id

      integer*4, intent(in) :: id
      type (data_group), pointer :: gsx

      integer*4 i
      type (data_set), pointer  :: ds

      if((id < 1) .or. (id > mtr%ndgrp)) then
            gsx => null()
            return
      endif

      ds => mtr%ds
      do while(associated(ds))
            do i = 1,ds%ngrp
                  if(ds%gp(i)%id /= id) cycle
                  gsx => ds%gp(i)
                  return
            end do
            ds => ds%next
      end do

      gsx => null()

      return
      end function gsx

      !----------------------------------------------------------------------------

      integer*4 function read_kalman_cov(file,npar,cov,unc,qabs)

      implicit none

      integer*4, intent(in) :: npar                   ! # parameters in kalman output file
      character*(*), intent(in) :: file               ! kalman output filename
      logical*4, intent(in) :: qabs                   ! true for absolute, false for % errors
      real*8, intent(out) :: cov(npar,npar), unc(npar)      ! # correlation matrix & rel unc of parameters

      integer*4 i,j,k,n,nchr,kof,nparm,npare,ios
      integer*4, allocatable :: icov(:,:)
      real*4 xinit,finl
      character line*300,parnam*12

      allocate(icov(npar,npar))
      icov = 0

      write(6,'(a)') 'Reading parameter covariances from '//file

      open(12,file=file,status='OLD',action='READ',iostat=ios)
      if(ios /= 0) then
            write(6,*) ' Error opening ',file
            read_kalman_cov = ios
            return
      endif

      read(12,*)
      read(12,'(30x,i2)') nparm
      read(12,'(30x,i2)') npare

      if(npare /= nparm) then
            write(6,'(a)') ' Total and estimated parameters not equal'
            close(12)
            read_kalman_cov = -1
            return
      endif

      if(nparm > npar) then
            write(6,'(a)') ' Kalman output file contains more parameters than requested'
            close(12)
            read_kalman_cov = -2
            return
      else if(nparm < npar) then
            write(6,'(a)') ' Kalman output file contains fewer parameters than requested'
            close(12)
            read_kalman_cov = -3
            return
      endif

      read(12,100) line
      nchr = len_trim(line)
      do while(line(1:17) /= 'Chi-square test !')
            read(12,100) line
            nchr = len_trim(line)
            ! write(6,'(a)') line(1:nchr)
      end do

      read(12,*)
      read(12,*)
      read(12,*)
      read(12,*)

      do i = 1,npar
            read(12,'(i5,a12,3(E11.4))') k,parnam,xinit,finl,unc(i)
            ! write(6,'(a)') parnam
            if(qabs) unc(i) = finl*unc(i)/100.D0      ! return absolute errors
      end do
      read(12,*)

      kof = 0

      do while (kof < npar)

            read(12,*)
            read(12,*)
            k = min(npar-kof,10)
            do i = kof+1,npar
                  read(12,100) line
                  nchr = len_trim(line)
                  ! write(6,'(a)') line(1:nchr)
                  n = min(i,k)
                  read(line(27:nchr),'(10i5)') (icov(i,j),j=kof+1,kof+n)
            end do

            kof = kof + k

      end do

      close(12)

      do i = 1,npar-1
            do j = i+1,npar
                  icov(i,j) = icov(j,i)
            end do
      end do

      cov = dble(icov)/1000.D0

      deallocate(icov)

      read_kalman_cov = 0

      return

100   FORMAT(A300)

      end function read_kalman_cov

      end module empire_materials
