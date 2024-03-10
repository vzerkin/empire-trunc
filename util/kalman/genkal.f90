program genkal
   use rctn, only: retReactionMT, strLength
   implicit none

   !! Convert empire cross sections and the sensitivities these reactions 
   !! have to each varied empire model parameter into Kalamn code format.
   !! The cross sections are written to unit 50.
   !! The sensitivies are written to unit 52. 
   !! The model parameters are assumed to be independent and their initial
   !! (prior) correlations are represented by an identity matrix.

   integer*4, parameter :: nr = 60                ! maximum # reactions allowed
   integer*4, parameter :: ne = 1500              ! maximum # energies allowed for each reaction
   integer*4, parameter :: np = 200               ! maximum # of varied empire parameters

   integer*4 i, j, k, mt, mt1, ios, nreac, ie, mat, nex, npfns, l1, l2
   integer*4 ln1, ln2, la1, la2, lb1, lb2, lc1, lc2, nparm, lt1, lt2
   CHARACTER*4 IA, IB, IC
   CHARACTER*6 PN, TMP
   CHARACTER*8 INPSEN
   CHARACTER*15 MATSEN, XSC
   CHARACTER*12 REACTION(NR), PNAME(NP)
   CHARACTER*25 FILE
   real*8 E(NE), CSEC(NE, NR), PERTB(NP), SENS(NP, NE, NR), SW(NP), W(NP, NP)

   INPSEN = '-inp.sen'

   READ (5, *) FILE, MT1, MAT, NEX, NPFNS

   IF (NPFNS .EQ. 0) THEN
      MATSEN = '-mat.sen'
      XSC = '.xsc'
   ELSE
      MATSEN = '-pfns-mat.sen'
      XSC = '-pfns.kal'
   END IF

   CALL strLength(FILE, L1, L2)

   ! GENERATE CROSS SECTION FILE - UNIT 50

   OPEN (10, FILE=FILE(L1:L2)//trim(XSC), STATUS='OLD', iostat=ios)
   if (ios /= 0) then
      WRITE (0, *) 'CROSS SECTION DATA FILE NOT FOUND: ', FILE(L1:L2)//trim(XSC)
      stop 1
   end if

   READ (10, '(1X,I3)') nreac
   IF (nreac .GT. NR) then
      write (0, *) 'Too many reactions specified in file ', FILE(L1:L2)//trim(XSC)
      stop 1
   end if

   READ (10, '(12X,(94A12))') (REACTION(j), j=1, nreac)
   i = 1
   do
      READ (10, *, END=10) E(I), (CSEC(i, j), j=1, nreac)
      i = i + 1
      IF (i > ne) then
         write (0, *) 'Too many energies in file ', FILE(L1:L2)//trim(XSC)
         stop 1
      end if
   end do
   10 CLOSE (10)
   ie = i - 1

   ! CHECKING IF THE REQUESTED MT1 IS PRESENT IN THE FILE.XSC FILE
   ! This check is only necessary when fitting cross sections, not pfns

   ! IF (NPFNS .EQ. 0) THEN
   !    j = 0
   !    DO i = 1, nreac
   !       mt = retReactionMT(REACTION(i))
   !       IF (MT .EQ. MT1) THEN
   !          j = 1
   !          exit
   !       END IF
   !    end do
   !    IF (j .EQ. 0) THEN
   !       WRITE (0, *) 'REACTION( MT=', MT1, ') NOT FOUND IN ', FILE(L1:L2)//trim(XSC)
   !       write (0, *)
   !       STOP 1
   !    END IF
   ! END IF

   ! write the cross section file for kalman

   open (50, action='WRITE')
   write (50, '(i5)') nreac
   DO I = 1, nreac
      WRITE (50, '(A12,31X,I5)') REACTION(I), ie
      WRITE (50, '(6(1PE11.4))') (E(J), CSEC(J, I), J=1, ie)
   end do
   CLOSE (50)

   ! GENERATE SENSITIVITY FILE - UNIT 52

   OPEN (10, FILE=FILE(L1:L2)//INPSEN, STATUS='OLD', iostat=ios)
   if (ios /= 0) then
      WRITE (0, *) 'PARAMETER FILE NOT FOUND: ', FILE(L1:L2)//INPSEN
      STOP 1
   end if
   i = 1
   do
      READ (10, *, END=20) PN, PERTB(I), IA, IB, IC
      IF ((PN(1:1) .EQ. '!') .OR. (PN(1:1) .EQ. '*')) cycle
      CALL strLength(PN, LN1, LN2)

      IF ((IA .NE. '!') .AND. (IA .NE. '*')) THEN
         CALL strLength(IA, LA1, LA2)
         CALL strLength(IB, LB1, LB2)
         PNAME(I) = PN(LN1:LN2)//IA(LA1:LA2)//IB(LB1:LB2)
         IF (IC .NE. '!') THEN
            CALL strLength(IC, LC1, LC2)
            PNAME(I) = PN(LN1:LN2)//IA(LA1:LA2)//IB(LB1:LB2)//IC(LC1:LC2)
         END IF
      ELSE
         PNAME(I) = PN
      END IF

      i = i + 1
      IF (i > np) then
         write (0, *) 'TOO MANY PARAMETERS in ', FILE(L1:L2)//INPSEN
         stop 1
      end if
   end do
   20 CLOSE (10)
   NPARM = i - 1

   !  Parameter correlations (identity matrix)
   w = 0.D0
   DO i = 1, NPARM
      W(i, i) = 1.D0
      SW(i) = 1.D0
   end do

   ! Write parameter number, names, prior values (1.0), and prioir correlations
   WRITE (52, '(25X,I5)') NPARM
   WRITE (52, '(11A12)') (PNAME(I), I=1, NPARM)
   WRITE (52, 100) (SW(I), I=1, NPARM)
   WRITE (52, 100) (PERTB(I), I=1, NPARM)
   DO I = 1, NPARM
      WRITE (52, '(20(F6.3))') (W(I, J), J=1, NPARM)
   end do
   close (10)

   ! Sensitivities
   OPEN (10, FILE=FILE(L1:L2)//trim(MATSEN), STATUS='OLD', iostat=ios)
   if (ios /= 0) then
      WRITE (0, *) 'SENSITIVITY FILE NOT FOUND: ', FILE(L1:L2)//trim(MATSEN)
      STOP 1
   end if
   print *, "sensitivities for nparm, ie, nreac ", nparm, ie, nreac
   DO i = 1, NPARM
      READ (10, *) ! FIRST LINE
      READ (10, '(13X,A6)') PN
      CALL strLength(PN, LT1, LT2)
      TMP = PNAME(I)
      IF (TMP(LT1:LT2) .NE. PN(LT1:LT2)) then
         write (0, *) 'WRONG PARAMETER found in ', FILE(L1:L2)//trim(MATSEN)
         write (0, *) TMP(LT1:LT2), ' vs ', PN(LT1:LT2)
         write (0, *) 'Order in -inp.sen file might differ from -mat.sen '
         write (0, *) 'Check also for double blank lines if sensitivities were added'
         stop 1
      end if
      READ (10, *) ! THIRD LINE
      ! print *,"Sensitivities for parameter # ", i 
      DO j = 1, ie
         READ (10, *) E(j), (SENS(i, j, k), k=1, nreac)
         ! print *,  E(j), (SENS(i, j, k), k=1, 5)
      end do
      READ (10, *)!EMPTY LINE
   end do
   CLOSE (10)
   
   print *, "sensitivities for nparm, ie, nreac ", nparm, ie, nreac
   ! i=> 1-nparm (57)    PARAMETERS
   ! k => 1-nreac (39)   reactions
   ! j +> 1-ie (56)      energies

   do k = 1, nreac
      WRITE (52, '(A12,13X,I5)') REACTION(k), ie
      do j = 1, ie
         WRITE (52, 100) (SENS(i, j, K)*CSEC(J, K)/(2.D0*PERTB(i)), i=1, NPARM)
      end do
   end do 
   close(52)
   100 FORMAT(11(1PE12.5))
   stop
END
