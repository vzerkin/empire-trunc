PROGRAM c4service

   USE c4_io

   IMPLICIT NONE

   ! INTEGER*4, PARAMETER :: ngmt = 13      ! list of allowed MTs for Kalman fitting
   ! INTEGER*4, PARAMETER :: goodmt(ngmt) = (/1,2,3,4,16,17,18,102,103,107,207,251,456/)

   INTEGER*4, PARAMETER   :: kcovex = 1     ! set nonzero to read experimental covariances
   ! REAL*4, PARAMETER    :: scale = 1.0    ! scale factor for parameter unc**2


   CHARACTER*4, PARAMETER :: xsc = '.xsc'
   CHARACTER*8, PARAMETER :: inpsen = '-inp.sen'
   CHARACTER*7, PARAMETER :: inpc4 = '-c4.inp'
   CHARACTER*8, PARAMETER :: outc4 = '-mod.c4'
   CHARACTER*7, PARAMETER :: logc4 = '-log.c4'
   CHARACTER*1            :: action = 'r'   ! action to be taken on a section r-retain, d-delete, m-modify, s-smooth, c-crop energy range, t-thin
   CHARACTER*10            :: arg2, arg3, arg4        ! arguments on the command line
   REAL*4 :: y1 = 0.0, y2 = 0.0, dy1 = 0.0, dy2 = 0.0 ! section modifying parameters
   INTEGER*4 mt1      ! MT to plot. If MT1=0, then plot all MTs. If NEX=1, only fit this MT.
   INTEGER*4 nex      ! fitting flag: 1=>fit only MT1, 2=>fit all MTs.
   !   INTEGER*4 nrx      ! # of reactions in EMPIRE XSC file
   LOGICAL :: qex   
   LOGICAL :: inp_read = .FALSE.
   INTEGER*4 i,j,k,m,ix,l1,l2,ios,status,ndat,sec_num
   CHARACTER file*25, command*100   !pname*6,line*130

   INTEGER*4 rk, rpza, rtza,  rmf, rmt
   INTEGER*4 nsec     ! # of sections in C4 file
   CHARACTER rref*25, rent*5, rsub*3, raction*1
   REAL*4 :: ry1,ry2, rdy1, rdy2  ! input parameters for c4service
   REAL*4 :: Emin = 0.0     ! lower energy limit on exp data
   REAL*4 :: Emax = 1000.0     ! upper energy limit on exp data
   REAL*4 :: Xmin = 0.0     ! lower cross section limit on exp data

   TYPE (c4_file) c4
   TYPE (c4_section), POINTER :: sc
   TYPE (c4_data_point), POINTER :: pt


   ! get root file name of the c4 file (i.e., without .c4 extension)
   CALL getarg(1,file)
   CALL strlen(file,l1,l2)

   CALL getarg(2,arg2)
   READ(arg2,'(f12.5)') Emin

   CALL getarg(3,arg3)
   READ(arg3,'(f12.5)') Emax

   CALL getarg(4,arg4)
   READ(arg4,'(f12.5)') Xmin


   !  read C4 file into the memory structure c4
   ! WRITE(6,*) 'Reading C4 file into the c4-structure'
   status = read_c4_file(file(l1:l2)//'.c4',c4, Emin, Emax, Xmin)
   if ( status == -1) stop

   !   open c4service input file
   INQUIRE(FILE=file(l1:l2)//inpc4,EXIST=qex)  
   IF(qex) THEN
      OPEN(13,FILE=file(l1:l2)//inpc4,STATUS='unknown',IOSTAT=ios)
      inp_read = .TRUE.
   ELSE
   !   WRITE(0,*) 'No c4service input found!'
   !   WRITE(0,*) 'c4service input file will be generated'
      OPEN(13,FILE=file(l1:l2)//inpc4,STATUS='new',IOSTAT=ios)
   ENDIF

  !   open c4service log file
   INQUIRE(FILE=file(l1:l2)//logc4,EXIST=qex)
   IF(qex) THEN
      OPEN(9,FILE=file(l1:l2)//logc4,STATUS='unknown',IOSTAT=ios, ACCESS='APPEND')
      WRITE(0,*) 'Found previous c4service log file; it will be updated'
   ELSE
      WRITE(0,*) 'c4service log file not found and will be created'
      OPEN(9,FILE=file(l1:l2)//logc4,STATUS='new',IOSTAT=ios)
   ENDIF

   IF(inp_read) THEN       ! perform operations on the subsections if any
      WRITE(6,*) ' '
      WRITE(6,*) 'Performed operations on the subsections'
      WRITE(9,*) ' '
      WRITE(9,*) 'Performed operations on the subsections'
      WRITE(9,*) 'Performing operations on the subsections'
      DO k=1,c4%nsec            !do loop over subsections
         sc => c4%sec(k)
         pt => c4%sec(k)%pt(1)  ! (sc%ndat) for the last point
         READ(13,12) rk, rpza, rtza,  rmf, rmt, rref, rent, rsub, action, y1, y2, dy1, dy2
12       FORMAT(i4, 1x, i4, i6, i4, i5, a26, 1x, a6, a4, 1x, a1, 4(1x,G12.6) )
         IF(rent /= sc%ent .OR. rsub /= sc%sub) THEN
            WRITE(6,*) 'FATAL: Subentry k=',k,' should have entry ',rent, &
               'and subentry ',rsub
            WRITE(6,*) 'internal c4-structure has for this k entry',sc%ent, &
               'and subentry ', sc%sub
            WRITE(6,*) 'Delete the ',file(l1:l2)//inpc4,' file and rerun'
            STOP 'EXFOR entry mismatch'
         END IF
         !  perform operations on the subsections: delete, print, modify, smooth, thin, crop
         ! IF(action == 'p') CALL print_section(c4, k)
         IF(action == 'd') CALL delete_section(c4, k)
         IF(action == 'm') CALL modify_section(sc, k, y1, y2, dy1, dy2)
         IF(action == 's') CALL smooth_section(sc, k, int(y1), y2, dy1)
         IF(action == 't') CALL thin_section(sc, k, y1)
         IF(action == 'c') CALL crop_section(sc, k, y1, y2)  !limit incident energies between y1 and y2
         IF(action == 'h') CALL hole_section(sc, k, y1, y2)  !remove incident energies between y1 and y2

      ENDDO
      action = 'r'
      ! Recreate c4 file from the internal c4-structure with modifications made above
      ! WRITE(6,*) 'Writting c4 file from the internal c4-structure'
      status = write_c4_file(file(l1:l2)//outc4, c4)
      WRITE(6,*) ' '
      WRITE(9,*) ' '
      ! Remove old c4 file
      command = "rm "//file(l1:l2)//".c4"
      CALL EXECUTE_COMMAND_LINE(command)      
      ! Modified c4 file stored as 'file(l1:l2)//outc4' contains zeros inside empty fields
      ! which makes it almost human unreadable.
      ! Command for moving modified c4 file over the old one leaving zeros
      ! command = 'mv '//file(l1:l2)//outc4//' '//file(l1:l2)//'.c4'     
      ! Command to copy modified c4 file over the old one replacing zeros with blanks
      command = "sed 's/ 0.00E+00/         /g' "//file(l1:l2)//outc4//" >> "//file(l1:l2)//".c4"
      CALL EXECUTE_COMMAND_LINE(command)

      ! Read and rescan the new C4 file, produce new input for c4service
      ! WRITE(6,*) 'Reading new C4 file into the c4-structure'
      status = read_c4_file(file(l1:l2)//'.c4',c4)
      CALL scan(c4)

      !  close, reopen new file to write c4service input file
      CLOSE(13)
!      WRITE(6,*) 'make_input: inp will be replaced',file(l1:l2)//inpc4,'!period'
      OPEN(13,FILE=file(l1:l2)//inpc4,STATUS='REPLACE')
      CALL make_input(c4)
      WRITE(6,*) 'New c4service input file ',file(l1:l2)//inpc4,' has been set as neutral'

      IF(sec_num /= 0) CALL print_section(c4, sec_num)  ! printing subentry #sub_num e.g., for gnu plotting

      STOP
   END IF

   !   print list of sections and create input file for the next service run
   CALL scan(c4)
   CALL make_input(c4)
   WRITE(6,*) 'c4service input file ',file(l1:l2)//inpc4,' has been reset to neutral'



   IF(sec_num /= 0) CALL print_section(c4, sec_num)  ! printing subentry #sub_num e.g., for gnu plotting


   STOP


CONTAINS

   !------------------------------------------------------------------------

   SUBROUTINE crop_section(sc, k, y1, y2)

!  Crops X4 section dropping points laying outside the y1 - y2 energy interval

      IMPLICIT NONE
      INTEGER*4 i,k, n
      REAL*4 :: y1, y2 ! range of incident energies to preserve (in keV)
      TYPE (c4_section), INTENT(INOUT) :: sc       ! C4 section to modify
!      TYPE (c4_data_point), POINTER :: pt1, pt2

      WRITE(6,*)' Cropping subentry: ',k,')', sc%ref, sc%ent, sc%sub
      WRITE(9,*)' Cropping subentry: ',k,')', sc%ref, sc%ent, sc%sub
      WRITE(9,*)' Cropping subsection: ',k, sc%ref, sc%ent, sc%sub

      IF(y1+y2 == 0.0) RETURN   ! no limits, nothing to do
      y1 = y1*10**6.   ! converting from MeV to eV
      y2 = y2*10**6.
      if(y2 == 0.0) y2 = sc%pt(sc%ndat)%e
      n = 0
      DO i = 1, sc%ndat
         IF(sc%pt(i)%e < y1 .or. sc%pt(i)%e > y2) cycle
         n = n + 1
         sc%pt(n) = sc%pt(i)
      END DO
      sc%ndat = n
      WRITE(6,*)'  - Subentry limitted to incident energies between', y1/10**6,' and ',y2/10**6,' MeV'
      WRITE(9,*)'  - Subentry limitted to incident energies between', y1/10**6,' and ',y2/10**6,' MeV'
      WRITE(9,*)' Subsection limitted to incident energies between', y1,' and ',y2,' keV'
      RETURN
   END SUBROUTINE crop_section

!------------------------------------------------------------------------

   SUBROUTINE hole_section(sc, k, y1, y2)

!  Drills hole in X4 section dropping points laying inside the y1 - y2 energy interval
!  (inverse of crop_section subroutine)

      IMPLICIT NONE
      INTEGER*4 i,k, n
      REAL*4 :: y1, y2 ! range of incident energies to remove (in keV)
      TYPE (c4_section), INTENT(INOUT) :: sc       ! C4 section to modify
!      TYPE (c4_data_point), POINTER :: pt1, pt2

      WRITE(6,*)' Drilling hole in subentry: ',k,')', sc%ref, sc%ent, sc%sub
      WRITE(9,*)' Drilling hole in subentry: ',k,')', sc%ref, sc%ent, sc%sub

      IF(y1 == 0.0 .or. y2 == 0.0) THEN
         WRITE(6,*)'  - NOTHING DONE! both energy limits needed for dropping energy range'
         WRITE(9,*)'  - NOTHING DONE! both energy limits needed for dropping energy range'
         RETURN   ! no limits, nothing to do
      ENDIF

      y1 = y1*10**6.   ! converting from MeV to eV
      y2 = y2*10**6.
      if(y2 == 0.0) y2 = sc%pt(sc%ndat)%e
      n = 0
      DO i = 1, sc%ndat
         IF(sc%pt(i)%e > y1 .and. sc%pt(i)%e < y2) cycle
         n = n + 1
         sc%pt(n) = sc%pt(i)
      END DO
      sc%ndat = n
      WRITE(6,*)'  - Removed points between incident energies ', y1/10**6,' and ',y2/10**6,' MeV'
      WRITE(9,*)'  - Removed points between incident energies ', y1/10**6,' and ',y2/10**6,' MeV'
      RETURN
   END SUBROUTINE hole_section




   SUBROUTINE thin_section(sc, k, y1)

!  Reduces number of points in the experimental data set by picking up only each y1-th
!  point from the full data set. There is no checking whether this procedure
!  conserves the area or that the omitted points would be restored by linear interpolation
!  between the selected points. Therefore, this thining should be applied only to experimental
!  data sets that are reach in points and were subjected to heavy smoothing, which makes the two
!  above mentioned conditions likely to be fulfilled.

      IMPLICIT NONE
      INTEGER*4 i,k, istep, n
      REAL*4 :: y1 ! section thinning parameter (take every y1 point only)
      TYPE (c4_section), INTENT(INOUT) :: sc       ! C4 section to modify
!      TYPE (c4_data_point), POINTER :: pt1, pt2

      WRITE(6,*)' Thinning subentry: ',k,')', sc%ref, sc%ent, sc%sub
      WRITE(9,*)' Thinning subentry: ',k,')', sc%ref, sc%ent, sc%sub
      WRITE(9,*)' Thinning subsection: ',k, sc%ref, sc%ent, sc%sub

      istep = y1
      IF(sc%ndat <= 5*istep) RETURN  !set too small for the istep
      n = 0
      DO i = 1, sc%ndat, istep
         n = n + 1
         sc%pt(n) = sc%pt(i)
      END DO
      IF(n*istep <= sc%ndat) THEN   ! including the last point if missing
         n = n + 1
         sc%pt(n) = sc%pt(sc%ndat)
      ENDIF
      sc%ndat = n
      WRITE(6,*)'  - Subentry thinned, every ',istep,'-th value preserved'
      WRITE(9,*)'  - Subentry thinned, every ',istep,'-th value preserved'
      RETURN
   END SUBROUTINE thin_section

   SUBROUTINE modify_section(sc, k, y1, y2, dy1, dy2)
      IMPLICIT NONE
      INTEGER*4 i,k
      REAL*4 :: y1  ! add y1 to each cross section
      REAL*4 :: y2  ! scale each cross section by a factor of y2
      REAL*4 :: dy1 ! add, in square, dy1 % uncertainty to the original uncertainty
      REAL*4 :: dy2 ! scale each uncertainty by a factor of dy2

      TYPE (c4_section), INTENT(IN) :: sc       ! C4 section to modify
      TYPE (c4_data_point), POINTER :: pt

      ! Check default input
      IF (y2 > 0.0 .and. dy2 == 0.0) dy2 = y2  ! Set the same factor for uncertainties as for x-sec.
      IF (dy2 == 0.0)  dy2 = 1.0 ! Don't zero uncertainties!
      WRITE(9,*)' Modifying subsection: ',k, sc%ref, sc%ent, sc%sub

      WRITE(6,*)' Modifying subentry: ',k,')', sc%ref, sc%ent, sc%sub
      WRITE(9,*)' Modifying subentry: ',k,')', sc%ref, sc%ent, sc%sub


      ! This DO LOOP can be modified if another form of modification is required
      DO i = 1, sc%ndat
         pt => sc%pt(i)
         pt%x  = pt%x  + y1  + pt%x*y2                         !modifying cross sections
         ! write(*,*)'original:',pt%dx
         pt%dx = dy2*SQRT(pt%dx**2 + (pt%x*dy1/100.)**2)       ! adding additional dy1 uncertainty (in %) in squares
                                                               ! and multiplying uncertainty by a factor
      END DO
      IF(y1 /= 0.0)   WRITE(6,*)'  - cross sections changed by adding ',y1, '[b]'
      IF(y2 /= 0.0)   WRITE(6,*)'  - cross sections multiplied by ',y2
      IF(dy1 /= 0.0)  WRITE(6,*)'  - absolute uncertainties increased by square-adding ', dy1, '[%]' 
      IF(dy2 /= 1.0)  WRITE(6,*)'  - absolute uncertainties multiplied by ', dy2      
      IF(y1 /= 0.0)   WRITE(9,*)'  - cross sections changed by adding ',y1, '[b]'
      IF(y2 /= 0.0)   WRITE(9,*)'  - cross sections multiplied by ',y2
      IF(dy1 /= 0.0)  WRITE(9,*)'  - absolute uncertainties increased by square-adding ', dy1, '[%]' 
      IF(dy2 /= 1.0)  WRITE(9,*)'  - absolute uncertainties multiplied by ', dy2

      RETURN
   END SUBROUTINE modify_section


   SUBROUTINE delete_section(c4,k)
      IMPLICIT NONE
      INTEGER*4 k

      TYPE (c4_file) c4
      TYPE (c4_section), POINTER :: sc
      TYPE (c4_data_point), POINTER :: pt

      sc => c4%sec(k)
      sc%ndat = 0

      WRITE(6,*)' Deleted  subentry: ',k, sc%ref, sc%ent, sc%sub
      WRITE(9,*)' Deleted  subentry: ',k, sc%ref, sc%ent, sc%sub
      WRITE(9,*)' Deleted  subsection: ',k, sc%ref, sc%ent, sc%sub
      RETURN
   END SUBROUTINE delete_section


   SUBROUTINE smooth_section(sc, k, iter, emin, emax)

      ! Volume Conserving Smoothing for Piecewise Linear Curves ...
      ! after Andrew Kuprat, Ahmed Khamayseh, Denise George, and Levi Larkey
      ! Journal of Computational Physics 172, 99-118 (2001)
      ! doi:10.1006/jcph.2001.6816
      ! Algorithm simplified to consider integral between four-point-defined
      ! piecewise linear curve and the x axis.

      IMPLICIT NONE
      INTEGER*4 j,i,k,jmin,jmax
      INTEGER*4 iter! number of smoothing iterations
      REAL*4 l      ! distance between the points 1 and 4
      REAL*4 h      ! new distance of points 2 and 3 from the 1 to 4 line
      REAL*4 s,sp   ! area of the quadrilateral defined by the four points
      REAL*4 emin   ! lower energy limit on exp data, 0 == no limit
      REAL*4 emax   ! upper energy limit on exp data, 0 == no limit

      TYPE (c4_section), INTENT(IN) :: sc              ! C4 section to smooth
      TYPE (c4_data_point), POINTER :: p0, p1, p2, p3  ! 4 points involved in smoothing

      WRITE(6,*)' Smoothing subentry: ',k,')', sc%ref, sc%ent, sc%sub
      WRITE(9,*)' Smoothing subentry: ',k,')', sc%ref, sc%ent, sc%sub

      IF(iter == 0.0) iter = sqrt(Real(sc%ndat))
      emin = emin*10**6   ! Convert to eV since C4 file is in eV
      emax = emax*10**6 

      ! Set full EXFOR energy range for smoothing
      jmin = 1
      jmax = sc%ndat-3
      ! Set requested smoothing energy range 
      IF(emin > 0.0) THEN
         DO j = 1, sc%ndat 
            jmin = 1
            IF(sc%pt(j)%e <= emin) jmin = j
         ENDDO 
      ENDIF
      IF(emax > 0.0) THEN
         DO j = 1, sc%ndat 
            IF(sc%pt(j)%e >= emax) THEN
               jmax = j-3   ! Because smoothing needs 4 points j must stop 3 points from the last
               EXIT
            ENDIF
         ENDDO 
      ENDIF

      IF(emin == 0.0) emin = sc%pt(1)%e
      IF(emax == 0.0) emax = sc%pt(sc%ndat)%e
      IF(emin > 0.0 .or. emax > 0.0) THEN
         WRITE(6,*) '  - smoothing energy range restricted to ',emin*1.0E-6,' - ',emax*1.0E-6,' MeV'  
         WRITE(9,*) '  - smoothing energy range restricted to ',emin*1.0E-6,' - ',emax*1.0E-6,' MeV'  
      ENDIF
      IF(jmax-jmin < 10) THEN ! smoothing needs at least 4 points but we set limit higher
         WRITE(6,*) '   NO smoothing - too few points! jmin=',jmin,' jmax=',jmax
         WRITE(9,*) '   NO smoothing - too few points! jmin=',jmin,' jmax=',jmax
         RETURN 
      ENDIF     

      ! Actual smoothing starts here
      DO i = 1, iter
         DO j = 1, jmax
            p0 => sc%pt(j)
            p1 => sc%pt(j+1)
            p2 => sc%pt(j+2)
            p3 => sc%pt(j+3)
            IF(p1%e<p0%e .OR. p2%e<p1%e .OR. p3%e<p2%e) THEN
               WRITE(6,*) 'Energies not monotonically increasing for quadruplet starting with j=',j
               STOP 'Energies not monotonic'
            END IF

            ! cross sections
            s = 0.5*abs(p1%e - p0%e)*(p1%x + p0%x) + 0.5*abs(p2%e - p1%e)*(p2%x + p1%x) + &
               0.5*abs(p3%e - p2%e)*(p3%x + p2%x)
            l = abs(p3%e - p0%e)
            IF(l == 0) THEN
               WRITE(6,*)'   Fatal error in smoothig: points at the same energy ', p3%e, p0%e
               WRITE(6,*)'   The first od four at position ', j,'-th in the dataset'  
               WRITE(9,*)'   Fatal error in smoothig: points at the same energy ', p3%e, p0%e
               WRITE(9,*)'   The first od four at position ', j,'-th in the dataset  STOP'  
               STOP ' Smoothing failed due to 4 points with the same energy'
            END IF
            h = (3*s)/(2*l) - (p0%x + p3%x)/4.
            p1%e = p0%e + (p3%e - p0%e)/3.0
            p2%e = p3%e - (p3%e - p0%e)/3.0
            p1%x = h
            p2%x = h
!            sp = l*(p0%x+2*p1%x+2*p2%x+p3%x)/6.0
!            if (s /= sp) write(6,*) 'Inegral mismatch', s, sp

            !  x-sec uncertainties
            s = 0.5*abs(p1%e - p0%e)*(p1%dx + p0%dx) + 0.5*abs(p2%e - p1%e)*(p2%dx + p1%dx) + &
               0.5*abs(p3%e - p2%e)*(p3%dx + p2%dx)
            h = (3*s)/(2*l) - (p0%dx + p3%dx)/4.
            p1%dx = h
            p2%dx = h
            sp = l*(p0%dx+2*p1%dx+2*p2%dx+p3%dx)/6.0
            if (abs(s-sp)/s > 0.00001) write(6,*) '   integral mismatch', s, sp
         END DO
      END DO
      
      WRITE(6,*)'  - Subentry smoothed ',int(iter),' times '
      WRITE(9,*)'  - Subentry smoothed ',int(iter),' times ' 
      
      RETURN
   END SUBROUTINE smooth_section


   SUBROUTINE print_section(c4,sec_num)

      INTEGER*4, INTENT(IN) :: sec_num   ! list number of the section to print
      INTEGER*4 k
      TYPE (c4_file) c4
      TYPE (c4_section), POINTER :: sc
      TYPE (c4_data_point), POINTER :: pt
      CHARACTER author*9, year*2

      IF(sec_num > c4%nsec) THEN
         WRITE(6,*) 'Requested EXFOR-subentry position out of range'
         STOP 'Requested EXFOR-subentry position out of range'
      END IF
      sc => c4%sec(sec_num)

!      READ(sc%ref,'(a9)') author
      author = sc%ref(1:9)
      year   = sc%ref(23:24)
      file   = author//'-'//year//'.c4'

      WRITE(6,*) ' Print subentry:',file
      OPEN(20,FILE=file,STATUS='REPLACE')

      DO k=1,sc%ndat
         pt => c4%sec(sec_num)%pt(k)
         WRITE(20,*) pt%e/10**6, pt%de/10**6, pt%x*10**3, pt%dx*10**3,  sc%ref, sc%ent, sc%sub
      END DO

      RETURN

   END SUBROUTINE print_section


   SUBROUTINE scan(c4)

      ! prints list of sections (EXFOR subentries)

      IMPLICIT NONE

      TYPE (c4_file) c4
      TYPE (c4_section), POINTER :: sc
      TYPE (c4_data_point), POINTER :: pt

      WRITE(6,*)''
      WRITE(6,*)'--------------------------------------------------------------------------------------------'
      WRITE(6,*)' num  pro targ.   MF    MT Emin        Emax     points   author             year  entry sub'
      WRITE(6,*)'--------------------------------------------------------------------------------------------'
      ! WRITE(9,*)''
      ! WRITE(9,*)'--------------------------------------------------------------------------------------------'
      ! WRITE(9,*)' num  pro targ.   MF    MT Emin        Emax     points   author             year  entry sub'
      ! WRITE(9,*)'--------------------------------------------------------------------------------------------'

      DO k=1,c4%nsec
         sc => c4%sec(k)
         pt => c4%sec(k)%pt(1)
         WRITE(6,10) k, sc%pza, sc%tza, sc%tmeta, pt%mf, pt%mt, sc%pt(1)%e/10**6, sc%pt(sc%ndat)%e/10**6, &
            sc%ndat, sc%ref, sc%ent, sc%sub
         ! WRITE(9,10) k, sc%pza, sc%tza, sc%tmeta, pt%mf, pt%mt, sc%pt(1)%e/10**6, sc%pt(sc%ndat)%e/10**6, &
         !    sc%ndat, sc%ref, sc%ent, sc%sub
      ENDDO
      WRITE(6,*)'--------------------------------------------------------------------------------------------'
      WRITE(6,*)''      
      ! WRITE(9,*)'--------------------------------------------------------------------------------------------'
      ! WRITE(9,*)''
      RETURN

10    FORMAT(i4, ')', i4, i6, a2, i4, i5, 1x, 1Pg9.3,' - ',1Pg9.3, i7, a26, 1x, a6, a4)

   END SUBROUTINE scan


   SUBROUTINE make_input(c4)

      !  creates c4service input file for the next run

      IMPLICIT NONE

      TYPE (c4_file) c4
      TYPE (c4_section), POINTER :: sc
      TYPE (c4_data_point), POINTER :: pt
      INTEGER*4 k

      y1  = 0.0
      y2  = 0.0
      dy1 = 0.0
      dy2 = 0.0
      DO k=1,c4%nsec
         sc => c4%sec(k)
         pt => c4%sec(k)%pt(1)
         WRITE(13,11) k, sc%pza, sc%tza,  pt%mf, pt%mt,  &
            sc%ref, sc%ent, sc%sub, action, y1, y2, dy1, dy2
      ENDDO
!      WRITE(6,*) ' c4service input file created '
      RETURN
11    FORMAT(i4, ')', i4, i6, i4, i5, a26, 1x, a6, a4, 1x, a1, 4(1x,G12.6) )

   END SUBROUTINE make_input


   SUBROUTINE dataout(sc,mt)
      IMPLICIT NONE

      ! echo data items back out to fortran units 10,11,12.
      ! write energies in MeV, cross sections in mb.
      ! also to unit 75 in MeV, barns for plotting.

      TYPE (c4_section), INTENT(IN) :: sc       ! C4 section to scan
      INTEGER*4, INTENT(IN) :: mt               ! MT to extract

      REAL*8, PARAMETER :: cor = 0.2D0

      LOGICAL*4 qwt
      INTEGER*4 i,l,m,npt
      REAL*8 xf
      CHARACTER chr3*3

      TYPE mtpt
         REAL*8 e
         REAL*8 x
         REAL*8 z
      END TYPE
      TYPE (mtpt), ALLOCATABLE, TARGET :: gpts(:)
      TYPE (mtpt), POINTER :: gp

      TYPE (c4_data_point), POINTER :: pt

      ! data written to unit 75 is for plots

      qwt = .FALSE.
      IF((mt1 == 0) .OR. (mt == mt1)) THEN
         WRITE(chr3,'(I3)') mt
         CALL strlen(chr3,l,m)
         OPEN(75,FILE=file(l1:l2)//'-'//chr3(l:m)//'-c4.gpd',STATUS='UNKNOWN',ACTION='WRITE',ACCESS='APPEND')
         qwt = .TRUE.
      ENDIF

      IF(mt < 200) THEN
         xf = 1000.D0         ! convert regular cross sections to mb
      ELSE
         xf = 1.D0            ! don't convert mubar, nubar
      ENDIF

      ALLOCATE(gpts(sc%ndat))

      npt = 0
      DO i = 1,sc%ndat
         pt => sc%pt(i)
         IF(pt%mt /= mt) CYCLE
         npt = npt + 1
         gp => gpts(npt)
         gp%e = 1.0D-06*pt%e
         gp%x = xf*pt%x
         IF(pt%x == 0.D0) THEN
            gp%z = 0.D0
         ELSE
            gp%z = pt%dx/pt%x
         ENDIF
         IF(.NOT.qwt) CYCLE
         IF(pt%dx > 0.D0) WRITE(75,999) gp%e,pt%x,pt%dx,mt
      END DO

      IF(qwt) CLOSE(75)

      IF(npt < 1) THEN
         ! no points for MT should not happen
         ! print error message and abort
         WRITE(0,'(a,i0)') ' Internal inconsistency processing MT = ',mt
         STOP 1
      ENDIF

      ! data to 10,11,12 are used for fitting

      WRITE(10,100) sc%ref, sc%ent, sc%sub, npt
      WRITE(10,200) (gpts(i)%e,gpts(i)%x, i=1,npt)
      WRITE(11,100) sc%ref, sc%ent, sc%sub, npt
      WRITE(11,200) (gpts(i)%e,gpts(i)%z,i=1,npt)
      IF(kcovex /= 0) THEN
         WRITE(12,100) sc%ref, sc%ent, sc%sub,-npt
         WRITE(12,300) cor
      ENDIF

      DEALLOCATE(gpts)

      RETURN

100   FORMAT(A25,5X,A5,A3,5X,I5)
200   FORMAT(6(1PE11.4))
300   FORMAT(F6.3)
999   FORMAT(3(1X,E12.5),I4)

   END SUBROUTINE dataout


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


END PROGRAM c4service
