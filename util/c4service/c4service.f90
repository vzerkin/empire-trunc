program c4service

   use c4_io

   implicit none

   integer*4, parameter :: ngmt = 13      ! list of allowed MTs for Kalman fitting
   integer*4, parameter :: goodmt(ngmt) = (/1,2,3,4,16,17,18,102,103,107,207,251,456/)

   integer*4, parameter :: kctl1 = 0      ! set nonzero to read priors
   integer*4, parameter :: kctl2 = 0      ! set nonzero to write posteriors
   integer*4, parameter :: kcovex = 1     ! set nonzero to read experimental covariances
   real*4, parameter    :: scale = 1.0    ! scale factor for parameter unc**2
   real*4, parameter    :: emin = 0.0     ! lower energy limit on exp data, 0 == no limit
   real*4, parameter    :: emax = 0.0     ! upper energy limit on exp data, 0 == no limit

   character*4, parameter :: xsc = '.xsc'
   character*8, parameter :: inpsen = '-inp.sen'
   character*8, parameter :: inpc4 = '-c4.inp'
   character*8, parameter :: outc4 = '-mod.c4'
   character*1            :: action = 'c' ! action to be taken on a section c-copy, d-delete, m-modify, s-smooth

   real*4 :: y1 = 0.0, y2 = 1.0, dy1 = 0.0, dy2 = 1.0 ! section modifying parameters
   integer*4 nsec     ! # of sections in C4 file
   integer*4 mt1      ! MT to plot. If MT1=0, then plot all MTs. If NEX=1, only fit this MT.
   integer*4 nex      ! fitting flag: 1=>fit only MT1, 2=>fit all MTs.
   integer*4 nrx      ! # of reactions in EMPIRE XSC file
   integer*4 mat      ! MAT of material. not used here
   integer*4 nprm     ! # EMPIRE parameters in sensitivity input file
   logical*4 qex,qmt,hmt(999)
   logical*4 :: inp_read = .FALSE.
   integer*4 i,j,k,m,ix,l1,l2,ios,status,ndat,sec_num
   character pname*6,line*130,file*25, command*100

   integer*4 rk, rpza, rtza,  rmf, rmt
   character rref*25, rent*5, rsub*3, raction*1
   real*4  ry1,ry2, rdy1, rdy2


   type empire_reaction
      integer*4 mt
      character*12 name
   end type
   type (empire_reaction), allocatable :: rcx(:)

   type fitted_reaction
      integer*4 mt                               ! MT value
      integer*4 ix                               ! index in empire reaction
      integer*4 num                              ! # of C4 data sets for this MT
      integer*4, allocatable :: ic(:)            ! C4 index of each data set
      real*8, allocatable :: wt(:)               ! weights for each data set
   end type
   integer*4 :: nmt                               ! # MTs that will be fit
   integer*4, allocatable :: kx(:)                ! section incidices
   type (fitted_reaction), target  :: fmt(ngmt)   ! the MTs being fit
   type (fitted_reaction), pointer :: fx

   type (c4_file) c4
   type (c4_section), pointer :: sc
   type (c4_data_point), pointer :: pt

   integer*4, external :: rctn

   ! get project name, plotting MT, MAT and fitting flag nex

   !    read(5,*) FILE,MT1,MAT,NEX
   file = 'fe56-full'
   MT1 = 0
   MAT = 1111
   NEX = 2
   call strlen(file,l1,l2)

   !  read C4 file into the memory structure c4
   write(6,*) 'Reading C4 file into the c4-structure'
   status = read_c4_file(file(l1:l2)//'.c4',c4)


   inquire(file=file(l1:l2)//inpc4,exist=qex)
   if(qex) then
      open(13,file=file(l1:l2)//inpc4,status='unknown',iostat=ios)
      inp_read = .TRUE.
   else
      write(0,*) 'No c4service input found!'
      write(0,*) 'C4 file will be scanned and c4service input file generated'
      open(13,file=file(l1:l2)//inpc4,status='new',iostat=ios)
   endif

   if(inp_read) then       ! perform operations on the section if any
      write(6,*) 'Performing operations on the subsections'
      !   perform operations on the sections copy, delete, modify, smooth
      do k=1,c4%nsec
         sc => c4%sec(k)
         pt => c4%sec(k)%pt(1)  ! (sc%ndat) for the last point
         read(13,12) rk, rpza, rtza,  rmf, rmt, rref, rent, rsub, action, y1, y2, dy1, dy2
         if(rent /= sc%ent .or. rsub /= sc%sub) then
            write(6,*) 'FATAL: Subsection k=',k,' should have entry ',rent, &
               'and subentry ',rsub
            write(6,*) 'internal c4-structure has for this k entry',sc%ent, &
               'and subentry ', sc%sub
            write(6,*) 'Rerun with the input file containing only name of the c4 file'
            stop 'EXFOR entry mismatch'
         end if
         if(action == 'd') call delete_section(c4, k)
         if(action == 'm') call modify_section(c4, k, y1, y2, dy1, dy2)
         if(action == 's') call smooth_section(c4, k, y1)
      enddo

     !   recreate c4 file from the internal c4-structure (includes modifications made above)
      write(6,*) 'Writting c4 file from the internal c4-structure'
      status = write_c4_file(file(l1:l2)//outc4, c4)
      write(6,*) ' '
      write(6,*) 'Modified c4 file stored as ',file(l1:l2)//outc4
      write(6,*) 'Moving it over the old one ',file(l1:l2)//'.c4'
      command = 'mv '//file(l1:l2)//outc4//' '//file(l1:l2)//'.c4'
      CALL EXECUTE_COMMAND_LINE(command)

!     read and rescan the new C4 file, produce new input for c4service
      write(6,*) 'Reading new C4 file into the c4-structure'
      status = read_c4_file(file(l1:l2)//'.c4',c4)
      call scan(c4)
      call make_input(c4)
      write(6,*) 'List of sections and c4service input file ',file(l1:l2)//inpc4,' have been generated'

      stop
   end if
!   write(12,11) k, sc%pza, sc%tza,  pt%mf, pt%mt,  &      !WE NEED THIS
!      sc%ref, sc%ent, sc%sub, action, y1, y2, dy1, dy2
11 FORMAT(i4, ')', i4, i6, i4, i5, a26, 1x, a6, a4, 1x, a1, 4(1x,f7.3) )
12 FORMAT(i4, 1x, i4, i6, i4, i5, a26, 1x, a6, a4, 1x, a1, 4(1x,f7.3) )


   !   print list of sections and create input file for the next service run
   call scan(c4)
   call make_input(c4)
   write(6,*) 'List of sections and c4service input file ',file(l1:l2)//inpc4,' have been generated'

!   sec_num = 1123
!   if(sec_num > c4%nsec) then
!      write(6,*) 'Requested EXFOR-subentry position out of range'
!      stop 'Requested EXFOR-subentry position out of range'
!   end if
!   sc => c4%sec(sec_num)
!
!   do k=1,sc%ndat
!      pt => c4%sec(sec_num)%pt(k)
!      write(6,*) pt%e/10**6, pt%de/10**6, pt%x*10**3, pt%dx*10**3,  sc%ref, sc%ent, sc%sub
!   end do
   stop


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
      write(0,'(a)') ' No data from C4 found in EMPRIRE XSC file!'
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
         fx%wt(kx(m)) = 1.D0

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

   subroutine modify_section(c4, k, y1, y2, dy1, dy2)
      implicit none
      integer*4 i,k
      real*4 :: y1, y2, dy1, dy2 ! section modifying parameters

      type (c4_file) c4
      type (c4_section), pointer :: sc
      type (c4_data_point), pointer :: pt

      sc => c4%sec(k)

      do i = 1, sc%ndat
         pt => c4%sec(k)%pt(i)
         pt%x  = pt%x  + y1  + pt%x*y2
         pt%dx = pt%dx + dy1 + pt%dx*dy2
      end do
      write(6,*)' Modified subsection: ',k, sc%ref, sc%ent, sc%sub
      return
   end subroutine modify_section

   subroutine delete_section(c4,k)
      implicit none
      integer*4 k

      type (c4_file) c4
      type (c4_section), pointer :: sc
      type (c4_data_point), pointer :: pt

      sc => c4%sec(k)
      sc%ndat = 0

      write(6,*)' Deleted  subsection: ',k, sc%ref, sc%ent, sc%sub
      return
   end subroutine delete_section

   subroutine smooth_section(c4, k, y1)

      ! Volume Conserving Smoothing for Piecewise Linear Curves ...
      ! after Andrew Kuprat, Ahmed Khamayseh, Denise George, and Levi Larkey
      ! Journal of Computational Physics 172, 99–118 (2001)
      ! doi:10.1006/jcph.2001.6816
      ! Algorithm simplified to consider integral between four-point-defined
      ! piecewise linear curve and the x axis.

      implicit none
      integer*4 j,i,k
      real*4 l      ! distance between the points 1 and 4
      real*4 h      ! new distance of points 2 and 3 from the 1 to 4 line
      real*4 s      ! area of the quadrilateral defined by the four points
      real*4 y1     ! number of smoothing iterations

      type (c4_file) c4
      type (c4_section), pointer :: sc
      type (c4_data_point), pointer :: p0, p1, p2, p3

      sc => c4%sec(k)
      if(sc%ndat < 4) return      ! smoothing needs at least 4 points
      y1 = min(int(y1), sc%ndat-3)
      do i = 1, int(y1)
         do j = 1, sc%ndat-3
            p0 => c4%sec(k)%pt(j)
            p1 => c4%sec(k)%pt(j+1)
            p2 => c4%sec(k)%pt(j+2)
            p3 => c4%sec(k)%pt(j+3)
            s = 0.5*(p1%e - p0%e)*(p1%x + p0%x) + 0.5*(p2%e - p1%e)*(p2%x + p1%x) + &
               0.5*(p3%e - p2%e)*(p3%x + p2%x)
            l = p3%e - p0%e
            h = 0.75*s/l - 0.25*(p0%x + p3%x)
            p1%e = p0%e + l/3.0
            p2%e = p3%e - l/3.0
            p1%x = h
            p2%x = h
         end do
      end do
      write(6,*)' Smoothed subsection: ',k, sc%ref, sc%ent, sc%sub
      return
   end subroutine smooth_section


   subroutine scan(c4)

      ! prints list of sections (EXFOR subentries)

      implicit none

      type (c4_file) c4
      type (c4_section), pointer :: sc
      type (c4_data_point), pointer :: pt

      write(6,*)''
      write(6,*)'--------------------------------------------------------------------------------------------'
      write(6,*)' num  pro targ.   MF    MT Emin        Emax     points   author             year  entry sub'
      write(6,*)'--------------------------------------------------------------------------------------------'

      do k=1,c4%nsec
         sc => c4%sec(k)
         pt => c4%sec(k)%pt(1)
         write(6,10) k, sc%pza, sc%tza, sc%tmeta, pt%mf, pt%mt, sc%pt(1)%e/10**6, sc%pt(sc%ndat)%e/10**6, &
            sc%ndat, sc%ref, sc%ent, sc%sub
      enddo
      write(6,*)'--------------------------------------------------------------------------------------------'
      write(6,*)''
      return
10    FORMAT(i4, ')', i4, i6, a2, i4, i5, 1x, 1Pg9.3,' - ',1Pg9.3, i7, a26, 1x, a6, a4)
   end subroutine scan

   subroutine make_input(c4)

      !  creates c4service input file for the next run

      implicit none

      type (c4_file) c4
      type (c4_section), pointer :: sc
      type (c4_data_point), pointer :: pt

      do k=1,c4%nsec
         sc => c4%sec(k)
         pt => c4%sec(k)%pt(1)
         write(13,11) k, sc%pza, sc%tza,  pt%mf, pt%mt,  &
            sc%ref, sc%ent, sc%sub, action, y1, y2, dy1, dy2
      enddo
      write(6,*) 'c4service input file created '
      return
11    FORMAT(i4, ')', i4, i6, i4, i5, a26, 1x, a6, a4, 1x, a1, 4(1x,f7.3) )

   end subroutine make_input


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

100   FORMAT(A25,5X,A5,A3,5X,I5)
200   FORMAT(6(1PE11.4))
300   FORMAT(F6.3)
999   FORMAT(3(1X,E12.5),I4)

   end subroutine dataout

   SUBROUTINE sort(file_name)
      INTEGER*4 :: ioflag
      CHARACTER*200 :: file_name, temp_file = 'temporale', err_msg
      CHARACTER(LEN=100) :: command
      command='sort -n -t''?'' --key=1.16 --key=1.123 --key=1.128 '// &
         trim(file_name)//' > '//trim(temp_file)
      CALL EXECUTE_COMMAND_LINE(command,exitstat=ioflag,cmdmsg=err_msg)
      IF(ioflag /= 0) THEN
         WRITE(*,*) 'ERROR sorting input file! '//trim(err_msg)
         STOP
      ENDIF
      command='mv '//trim(temp_file)//' '//trim(file_name)
      CALL EXECUTE_COMMAND_LINE(command)
   END SUBROUTINE sort


end program c4service
