! $Rev: 4656 $
! $Author: rcapote $
! $Date: 2016-03-24 05:09:10 +0100 (Do, 24 Mär 2016) $
!
MODULE TLJs
   IMPLICIT NONE

   include 'dimension.h'
   include 'global.h'

   INTEGER, PUBLIC :: MAX_cc_mod,MAX_pmatr,MAX_umatr

   TYPE, PUBLIC :: cc_channel
      INTEGER Pcn        ! CN parity to which cc-channel couples (+1 or -1)
      REAL*8 Jcn           ! CN spin   to which cc-channel couples
      INTEGER nceq       ! Number of diagonal TLj in the P-matrix for a given CN J-pi
      INTEGER lev        ! number of the collective level (in the collective level file)
      INTEGER l          ! orbital angular momentum l
      REAL*8 j             ! channel spin (j in T_lj)
      REAL*8 tlj           ! CN transmission coefficient
      REAL*8 pchan         ! Pchan value
   END TYPE cc_channel

   TYPE, PUBLIC :: cc_pdiag
      INTEGER Pcn        ! CN parity to which cc-channel couples (+1 or -1)
      REAL*8 Jcn           ! CN spin   to which cc-channel couples
      INTEGER nceq       ! Number of diagonal TLj in the P-matrix for a given CN J-pi
      REAL*8 pdiag         ! Pdiag(irow) - Diagonal matrix elements of the diagonal P-diag matrix
   END TYPE cc_pdiag

   TYPE, PUBLIC :: cc_umatrix
      INTEGER Pcn        ! CN parity to which cc-channel couples (+1 or -1)
      REAL*8 Jcn           ! CN spin   to which cc-channel couples
      INTEGER nceq       ! Number of diagonal TLj in the P-matrix for a given CN J-pi
      INTEGER irow       ! Row number of the matrix in the transformed space
      INTEGER icol       ! Column number of the matrix in the transformed space
      COMPLEX*16 umatrix   ! U(irow,icol) - Matrix element of the unitary transformation matrix U
   END TYPE cc_umatrix

   TYPE(cc_channel), PUBLIC, ALLOCATABLE, TARGET :: STLcc(:)
   TYPE(cc_umatrix), PUBLIC, ALLOCATABLE, TARGET :: CCpmatrix(:)
   TYPE(cc_pdiag), PUBLIC, ALLOCATABLE, TARGET :: CCpdiag(:)
   TYPE(cc_umatrix), PUBLIC, ALLOCATABLE, TARGET :: CCumatrix(:)

   PUBLIC AllocTLJmatr, AllocEWmatr, DelTLJs
   PUBLIC AllocCCmatr, DelCCmatr, Prepare_CCmatr
   PUBLIC Open_CC_Files, Read_CC_Matrices, Close_CC_Files
   REAL*8, PUBLIC, ALLOCATABLE :: Pdiag(:)

   COMPLEX*16, PUBLIC, ALLOCATABLE :: Pmatr(:,:),Umatr(:,:) ! EW matrices Smatr(:,:)

   PRIVATE
  
   REAL*8, ALLOCATABLE :: Pchan(:)

   

CONTAINS

   !----------------------------------------------------------------------------------------------------
   SUBROUTINE AllocCCmatr(nch)
      IMPLICIT NONE
      INTEGER, INTENT(IN) :: nch
      INTEGER my

      IF(allocated(Pchan)) DEALLOCATE(Pchan)
      ALLOCATE(Pchan(nch),STAT=my)
      IF(my /= 0) GOTO 20
      Pchan = 0.0d0

      IF(allocated(Pdiag)) DEALLOCATE(Pdiag)
      ALLOCATE(Pdiag(nch),STAT=my)
      IF(my /= 0) GOTO 30
      Pdiag = 0.0d0

      IF(INTERF==0) RETURN

      !  EW matrices

      !  IF(allocated(Smatr)) DEALLOCATE(Smatr)
      !  ALLOCATE(Smatr(nch,nch),STAT=my)
      !  IF(my /= 0) GOTO 20
      !  Smatr = (0.d0,0.d0)

      IF(allocated(Pmatr)) DEALLOCATE(Pmatr)
      ALLOCATE(Pmatr(nch,nch),STAT=my)
      IF(my /= 0) GOTO 30
      Pmatr = (0.d0,0.d0)

      IF(allocated(Umatr)) DEALLOCATE(Umatr)
      ALLOCATE(Umatr(nch,nch),STAT=my)
      IF(my /= 0) GOTO 30
      Umatr = (0.d0,0.d0)

      RETURN
20    WRITE(8,*)  'ERROR: Insufficient memory for CC matrices in HRTW'
      WRITE(12,*) 'ERROR: Insufficient memory for CC matrices in HRTW'
      STOP 'ERROR: Insufficient memory for CC matrices in HRTW'
30    WRITE(8,*)  'ERROR: Insufficient memory for EW matrices in HRTW'
      WRITE(12,*) 'ERROR: Insufficient memory for EW matrices in HRTW'
      STOP 'ERROR: Insufficient memory for EW matrices in HRTW'
   END SUBROUTINE AllocCCmatr



   SUBROUTINE AllocTLJmatr(nch)
      IMPLICIT NONE
      INTEGER nch
      INTEGER my

      MAX_cc_mod = nch
      ! write(*,*) 'Inside AllocTLJs:',nch,MAX_cc_mod,MAX_cc

      ! TYPE(cc_channel), PUBLIC, ALLOCATABLE, TARGET :: STLcc(:)
      IF(allocated(STLcc)) DEALLOCATE(STLcc)
      ALLOCATE(STLcc(nch),STAT=my)
      IF(my /= 0) THEN
         WRITE(8,*)  'ERROR: Insufficient memory for TLJs'
         WRITE(12,*) 'ERROR: Insufficient memory for TLJs'
         STOP 'ERROR: Insufficient memory for TLJs'
         RETURN
      ENDIF

      STLcc%Pcn   = 1
      STLcc%Jcn   = 0.d0
      STLcc%nceq  = 1
      STLcc%lev   = 0
      STLcc%l     = 0
      STLcc%j     = 0.d0
      STLcc%tlj   = 0.d0
      STLcc%pchan = 0.d0

      RETURN
   END SUBROUTINE AllocTLJmatr

   !----------------------------------------------------------------------------------------------------

   SUBROUTINE AllocEWmatr(nch,npmat,numat)
      IMPLICIT NONE
      INTEGER nch,npmat,numat
      INTEGER my

      MAX_cc_mod = nch
      MAX_pmatr  = npmat
      MAX_umatr  = numat

      IF(INTerf==0) RETURN

      !TYPE(cc_pdiag), PUBLIC, ALLOCATABLE, TARGET :: CC_pdiag(:)
      IF(allocated(CCpdiag)) DEALLOCATE(CCpdiag)
      ALLOCATE(CCpdiag(nch),STAT=my)
      IF(my /= 0) THEN
         WRITE(8,*)  'ERROR: Insufficient memory for CCpdiag (in TLJs)'
         WRITE(12,*) 'ERROR: Insufficient memory for CCpdiag (in TLJs)'
         STOP 'ERROR: Insufficient memory for CCpdiag (in TLJs)'
         RETURN
      ENDIF

      CCpdiag%Pcn   = 1
      CCpdiag%Jcn   = 0.d0
      CCpdiag%nceq  = 0
      CCpdiag%pdiag = 0.d0

      !TYPE(cc_umatrix), PUBLIC, ALLOCATABLE, TARGET :: CC_umatrix(:)
      IF(allocated(CCumatrix)) DEALLOCATE(CCumatrix)
      ALLOCATE(CCumatrix(numat),STAT=my)
      IF(my /= 0) THEN
         WRITE(8,*)  'ERROR: Insufficient memory for CCumatrix (in TLJs)'
         WRITE(12,*) 'ERROR: Insufficient memory for CCumatrix (in TLJs)'
         STOP 'ERROR: Insufficient memory for CCumatrix (in TLJs)'
         RETURN
      ENDIF

      CCumatrix%Pcn     = 1
      CCumatrix%Jcn     = 0.d0
      CCumatrix%nceq    = 0
      CCumatrix%irow    = 0
      CCumatrix%icol    = 0
      CCumatrix%umatrix = (0.d0,0.d0)

      IF(allocated(CCpmatrix)) DEALLOCATE(CCpmatrix)
      ALLOCATE(CCpmatrix(npmat),STAT=my)
      IF(my /= 0) THEN
         WRITE(8,*)  'ERROR: Insufficient memory for CCpmatrix (in TLJs)'
         WRITE(12,*) 'ERROR: Insufficient memory for CCpmatrix (in TLJs)'
         STOP 'ERROR: Insufficient memory for CCpmatrix (in TLJs)'
         RETURN
      ENDIF

      CCpmatrix%Pcn     = 1
      CCpmatrix%Jcn     = 0.d0
      CCpmatrix%nceq    = 0
      CCpmatrix%irow    = 0
      CCpmatrix%icol    = 0
      CCpmatrix%umatrix = (0.d0,0.d0)

      RETURN
   END SUBROUTINE AllocEWmatr

   !----------------------------------------------------------------------------------------------------

   SUBROUTINE DelTLJs()
      IMPLICIT NONE
      IF(allocated(STLcc))     DEALLOCATE(STLcc)
      IF(INTerf==0) RETURN
      IF(allocated(CCpdiag))   DEALLOCATE(CCpdiag)
      IF(allocated(CCumatrix)) DEALLOCATE(CCumatrix)
      IF(allocated(CCpmatrix)) DEALLOCATE(CCpmatrix)
      RETURN
   END SUBROUTINE DelTLJs

   !----------------------------------------------------------------------------------------------------

   SUBROUTINE DelCCmatr()

      IMPLICIT NONE

      IF(DIRECT==0) RETURN

      IF(allocated(Pchan)) DEALLOCATE(Pchan)
      IF(allocated(Pdiag)) DEALLOCATE(Pdiag)

      IF(INTERF==0) RETURN
      !  IF(allocated(Smatr)) DEALLOCATE(Smatr)
      !  EW matrices
      IF(allocated(Pmatr)) DEALLOCATE(Pmatr)
      IF(allocated(Umatr)) DEALLOCATE(Umatr)

      RETURN
   END SUBROUTINE DelCCmatr

   !----------------------------------------------------------------------------------------------------

   LOGICAL FUNCTION Open_CC_files()

      IMPLICIT NONE
      LOGICAL fexist
      !CHARACTER*3 ctldir
      !CHARACTER*23 ctmp23
      !DATA ctldir/'TL/'/

      Open_CC_files = .FALSE.

      !WRITE (ctmp23,'(i3.3,i3.3,1h_,i3.3,i3.3,1h_,i9.9)') INT(ZEJc(0)),INT(AEJc(0)),INT(Z(0)),INT(A(0)),INT(EINl*1000000)

      !--The INQUIRE statement determines whether or not the file exists.
      !--If it does not, the program calculates new transmission coeff.
      !INQUIRE (FILE = (ctldir//ctmp23//'_Pchan.LST'),EXIST = fexist)
      INQUIRE (FILE = ('INCIDENT_Pchan.LST'),EXIST = fexist)
      IF (.not. fexist) RETURN ! *_Pchan.LST not found
      !  INQUIRE (FILE = (ctldir//ctmp23//'_Smatr.LST'),EXIST = fexist)
      !  INQUIRE (FILE = ('INCIDENT_Smatr.LST'),EXIST = fexist)
      !  IF (.not. fexist) RETURN ! *_Smatr.LST not found

      !  OPEN (126, FILE = (ctldir//ctmp23//'_Pchan.LST'), STATUS = 'old')
      OPEN (126, FILE = ('INCIDENT_Pchan.LST'), STATUS = 'old')
      !  OPEN (58 , FILE = (ctldir//ctmp23//'_Smatr.LST'), STATUS = 'old')
      !  OPEN (58 , FILE = ('INCIDENT_Smatr.LST'), STATUS = 'old')

      Open_CC_files = .TRUE. ! normal return

      IF(INTERF==0) RETURN

      Open_CC_files = .FALSE.
      !--------------
      !--EW matrices
      !--------------

      !--The INQUIRE statement determines whether or not the file exists.
      !--If it does not, the program calculates new transmission coeff.
      !INQUIRE (FILE = (ctldir//ctmp23//'_Pmatr.LST'),EXIST = fexist)
      INQUIRE (FILE = ('INCIDENT_Pmatr.LST'),EXIST = fexist)
      IF (.not. fexist) RETURN ! *_Pmatr.LST not found
      !INQUIRE (FILE = (ctldir//ctmp23//'_Pdiag.LST'),EXIST = fexist)
      INQUIRE (FILE = ('INCIDENT_Pdiag.LST'),EXIST = fexist)
      IF (.not. fexist) RETURN ! *_Pdiag.LST not found
      !INQUIRE (FILE = (ctldir//ctmp23//'_Umatr.LST'),EXIST = fexist)
      INQUIRE (FILE = ('INCIDENT_Umatr.LST'),EXIST = fexist)
      IF (.not. fexist) RETURN ! *_Umatr.LST not found

      !OPEN (125, FILE = (ctldir//ctmp23//'_Pmatr.LST'), STATUS = 'old')
      !OPEN (60 , FILE = (ctldir//ctmp23//'_Umatr.LST'), STATUS = 'old')
      !OPEN (61 , FILE = (ctldir//ctmp23//'_Pdiag.LST'), STATUS = 'old')
      OPEN (125, FILE = ('INCIDENT_Pmatr.LST'), STATUS = 'old')
      OPEN (60 , FILE = ('INCIDENT_Umatr.LST'), STATUS = 'old')
      OPEN (61 , FILE = ('INCIDENT_Pdiag.LST'), STATUS = 'old')

      Open_CC_files = .TRUE. ! normal return

      RETURN
   END FUNCTION Open_CC_files

   !----------------------------------------------------------------------------------------------------

   Subroutine Close_CC_files()
      IF(DIRECT==0) RETURN
      Close(126)
      !  Close(58)
      IF(INTERF==0) RETURN
      Close(125)
      Close(60)
      Close(61)
   END Subroutine Close_CC_files

   !----------------------------------------------------------------------------------------------------

   LOGICAL FUNCTION Read_CC_matrices()
   IMPLICIT NONE
   DOUBLE PRECISION jc, jj, sreal, simag, jcn
   INTEGER nceq, nc1, nc2, i1, i2, nelem, my, ncc, nch, npmat, numat
   INTEGER nlev, nl, nmax, pcn
   CHARACTER*1 parc
   COMPLEX*16, ALLOCATABLE :: cres(:,:),ctmp(:,:), Umatr_T(:,:) ! temporal matrices
   TYPE (cc_channel), POINTER :: ps_tlj
   TYPE (cc_umatrix), POINTER :: ps_pmatrix
   TYPE (cc_umatrix), POINTER :: ps_umatrix
   TYPE (cc_pdiag), POINTER :: ps_pdiag
   logical debug
   DATA debug/.FALSE./

      Read_CC_matrices = .FALSE.

      nch = 0
      npmat = 0
      numat = 0

!==Reading Pchan
   DO ncc = 1, MAX_cc_mod
     !--jc,parc are the channel spin and parity
     !--nceq is the number of coupled equations
     READ (126,'(1x,f9.1,4x,a1,1x,i4)',END=10,ERR=10) jc, parc, nceq
     ! write(*,*) jc,parc,nceq
     npmat = npmat + (nceq*(nceq+1))/2
     numat = numat + nceq**2
     DO i1 = 1, nceq
       nch = nch + 1
       ps_tlj => STLcc(nch)
       READ (126,*,END = 4,ERR = 4) nc1, sreal, nlev, nl, jj
      ! write(*,'(1x,I3,1x,I3,1x,F5.1,d12.6,1x,F5.1,1x,I2)') nlev,nl,sngl(jj),sngl(sreal),sngl(jc),ps_tlj%Pcn
       ps_tlj%Jcn = jc
       ps_tlj%Pcn = 1
       if(parc == '-') ps_tlj%Pcn = -1
       ps_tlj%nceq = nceq
       ps_tlj%lev = nlev
       ps_tlj%l   = nl
       ps_tlj%j   = jj
       ps_tlj%pchan = sreal
       ps_tlj%tlj = sreal
     ENDDO
   ENDDO
10 if (debug) WRITE(*,*) 'Pchan channels read:',nch,' expected',MAX_cc_mod

      Read_CC_matrices = .TRUE.

      IF(INTerf==0) RETURN

      write(*,*) nch,npmat,numat

      CALL AllocEWmatr(nch,npmat,numat)
      Read_CC_matrices = .FALSE.

   ! TYPE(cc_umatrix), PUBLIC, ALLOCATABLE, TARGET :: CCpmatrix(:)
   !==Reading Pmatr
   nch = 0
   DO ncc = 1, MAX_cc_mod
     !--jc,parc are the channel spin and parity
     !--nceq is the number of coupled equations
     READ (125,'(1x,f9.1,4x,a1,1x,i4)',END=12,ERR=12) jc, parc, nceq
     ! write(*,*) jc,parc,nceq
     !--Loop over the number of coupled equations (squared)
     nelem = nceq*(nceq+1)/2
     DO i1 = 1, nelem
       nch = nch + 1
       ps_pmatrix => CCpmatrix(nch)
       READ (125,'(1x,2(I4,1x),2(D15.9,1x))',END = 6,ERR = 6) nc1, nc2, sreal, simag
      !write(*,'(1x,I4,1x,2(I3,1x),2(1x,d12.6))') nch,nc1,nc2,sreal,simag
       ps_pmatrix%Jcn = jc
       ps_pmatrix%Pcn = 1
       if(parc == '-') ps_pmatrix%Pcn = -1
       ps_pmatrix%nceq = nceq
       ps_pmatrix%irow = nc1
       ps_pmatrix%icol = nc2
       ps_pmatrix%umatrix = CMPLX(sreal,simag,8)
     ENDDO
   ENDDO

12 MAX_pmatr = nch
   if (debug) WRITE(*,*) 'Pmatrix channels read:',nch
   if (debug) WRITE(*,*) 'Pmatrix channels calc:',npmat

   !==Reading Smatr
   !---Here the calculated files are read
   !--jc,parc are the channel spin and parity
   !--nceq is the number of coupled equations
   !  READ (58,'(1x,f9.1,4x,a1,1x,i4)',END=6,ERR=6) jc, parc, nceq
   !  write(*,*) jc,parc,nceq
   !
   !--Loop over the number of coupled equations
   !  DO i1 = 1, nceq
   !     DO i2 = 1, i1
   !      READ (58,*,END = 6,ERR = 6) nc1, nc2, sreal, simag
   !      Smatr(nc1,nc2) = CMPLX(sreal,simag,8)
   !       if(nc1 /= nc2) Smatr(nc2,nc1) = Smatr(nc1,nc2)
   !    ENDDO
   !  ENDDO

   !TYPE(cc_umatrix), PUBLIC, ALLOCATABLE, TARGET :: CCumatrix(:)
   !==Reading Umatr
   nch = 0
   DO ncc = 1, MAX_cc_mod
      !--jc,parc are the channel spin and parity
      !--nceq is the number of coupled equations
      READ (60,'(1x,f9.1,4x,a1,1x,i4)',END=14,ERR=14) jc, parc, nceq
      !  write(*,*) jc,parc,nceq
      !
      !--Loop over the number of coupled equations
      DO i1 = 1, nceq
         READ (60,*,END = 7,ERR = 7) ! nc1
         DO i2 = 1, nceq
            nch = nch + 1
            ps_umatrix => CCumatrix(nch)
            READ (60,'(1x,D15.9,1x,D15.9)',END = 7,ERR = 7) sreal,simag
            ! write(*,'(1x,I3,1x,I3,2(1x,d12.6))') i1,i2,sngl(sreal),sngl(simag)
            ps_umatrix%Jcn = jc
            ps_umatrix%Pcn = 1
            if(parc == '-') ps_umatrix%Pcn = -1
            ps_umatrix%nceq = nceq
            ps_umatrix%irow = i1
            ps_umatrix%icol = i2
            ps_umatrix%umatrix = CMPLX(sreal,simag,8)
         ENDDO
      ENDDO
   ENDDO

14 MAX_umatr = nch
   if (debug) WRITE(*,*) 'Umatrix channels read:',nch
   if (debug) WRITE(*,*) 'Umatrix channels calc:',numat

   !TYPE(cc_pdiag), PUBLIC, ALLOCATABLE, TARGET :: CCpdiag(:)
   !==Reading Pdiag
   nch = 0
   DO ncc = 1, MAX_cc_mod
     !--jc,parc are the channel spin and parity
     !--nceq is the number of coupled equations
     READ (61,'(1x,f9.1,4x,a1,1x,i4)',END=16,ERR=16) jc, parc, nceq
     ! write(*,*) jc,parc,nceq
     !
     !--Loop over the number of coupled equations
     DO i1 = 1, nceq
       nch = nch + 1
       ps_pdiag => CCpdiag(nch)
       READ (61,'(1x,I4,1x,D15.9)',END = 8,ERR = 8) nc1, sreal
       ! write(*,*) nc1,sreal
       ps_pdiag%Jcn = jc
       ps_pdiag%Pcn = 1
       if(parc == '-') ps_pdiag%Pcn = -1
       ps_pdiag%nceq = nceq
       ps_pdiag%pdiag = sreal
     ENDDO
   ENDDO

16 if (debug) WRITE(*,*) 'Pdiag channels read:',nch
   if (debug) WRITE(*,*) 'Pdiag channels expected:',MAX_cc_mod

Read_CC_matrices = .TRUE.

RETURN

write(*,*) 'Start checking ...'

Jcn = 1.5
Pcn = +1

   write(*,*) 'Pdiag'
   DO ncc = 1, MAX_cc_mod
     if(NINT(2*CCpdiag(ncc)%Jcn) == NINT(2*Jcn) .and. CCpdiag(ncc)%Pcn == Pcn) THEN
       write(*,*) 'J,Pi,nceq=',CCpdiag(ncc)%Jcn, CCpdiag(ncc)%Pcn, CCpdiag(ncc)%nceq
       nch = CCpdiag(ncc)%nceq

      CALL AllocCCmatr(nch)

      ! reading diagonal elements p_{alpha}
      nmax = ncc + nch - 1
      do i1 = ncc, nmax
         WRITE (*,*) i1, i1 - ncc +1, CCpdiag(i1)%pdiag
         Pdiag(i1 - ncc +1) = CCpdiag(i1)%pdiag
      enddo
      ! if(debug) pause
      EXIT
   endif
ENDDO

DO ncc = 1, MAX_umatr

     if(NINT(2*CCumatrix(ncc)%Jcn) /= NINT(2*Jcn) .or. CCumatrix(ncc)%Pcn /= Pcn) cycle ! Jpi = 3/2+
     write(*,*) 'J,Pi,nceq=',CCumatrix(ncc)%Jcn, CCumatrix(ncc)%Pcn, CCumatrix(ncc)%nceq

   nch = CCumatrix(ncc)%nceq

   if (debug) write(*,*) 'Umatr'
   nmax = ncc + nch*nch - 1
   do i1 = ncc, nmax
      if (debug) WRITE (*,*) i1, CCumatrix(i1)%irow, CCumatrix(i1)%icol, CCumatrix(i1)%umatrix
      Umatr(CCumatrix(i1)%irow,CCumatrix(i1)%icol) = CCumatrix(i1)%umatrix
   enddo

   EXIT

ENDDO

IF(allocated(Umatr_T)) DEALLOCATE(Umatr_T)
ALLOCATE(Umatr_T(nch,nch),STAT=my)
IF(my /= 0) then
   WRITE(8,*)  'ERROR: Insufficient memory for Umatr_T matrix in TLJs'
   WRITE(12,*) 'ERROR: Insufficient memory for Umatr_Ts matrix in TLJs'
   STOP
ENDIF
Umatr_T = 0.0d0

! Calculating the transposed matrix
Umatr_T = TRANSPOSE(Umatr)

!  TRANSPOSE WORKS !!!! March 2016
write (*,'(1x,A7,2(1x,i3),1x,d12.6,1x,d12.6)') 'Umatr  =',2,1,DREAL(Umatr(2,1)),DIMAG(Umatr(2,1))
write (*,'(1x,A7,2(1x,i3),1x,d12.6,1x,d12.6)') 'Umatr_T=',1,2,DREAL(Umatr_T(1,2)),DIMAG(Umatr_T(1,2))
write (*,'(1x,A7,2(1x,i3),1x,d12.6,1x,d12.6)') 'Umatr  =',3,1,DREAL(Umatr(3,1)),DIMAG(Umatr(3,1))
write (*,'(1x,A7,2(1x,i3),1x,d12.6,1x,d12.6)') 'Umatr_T=',1,3,DREAL(Umatr_T(1,3)),DIMAG(Umatr_T(1,3))
write (*,*)

IF(allocated(ctmp)) DEALLOCATE(ctmp)
ALLOCATE(ctmp(nch,nch),STAT=my)
IF(my /= 0) then
   WRITE(8,*)  'ERROR: Insufficient memory for ctmp matrix in TLJs'
   WRITE(12,*) 'ERROR: Insufficient memory for ctmp matrix in TLJs'
   STOP
ENDIF
ctmp = 0.0d0

ctmp = MATMUL(Umatr,Pmatr)

do i1=1,nceq
   do i2=1,nceq
      sreal =  REAL(Umatr_T(i1,i2))
      simag = -DIMAG(UMatr_T(i1,i2))
      Umatr_T(i1,i2) = CMPLX(sreal,simag,8)
   enddo
enddo

cres = MATMUL(ctmp,Umatr_T)

write (*,'(1x,A,1x,6(d12.6,1x))') 'Calc  =',(cres(i1,i1),i1=1,2)

IF(allocated(cres)) DEALLOCATE(cres)
IF(allocated(ctmp)) DEALLOCATE(ctmp)
IF(allocated(Umatr_T)) DEALLOCATE(Umatr_T)

pause

CALL DelCCmatr()

RETURN

4 WRITE(8,*)' WARNING: Problem reading Pchan matrix'
STOP ' WARNING: Problem reading Pchan matrix'
5 WRITE(8,*)' WARNING: Problem reading EW Pmatrix'
STOP ' WARNING: Problem reading EW Pmatrix'
6 WRITE(8,*)' WARNING: Problem reading EW Smatr matrix'
STOP ' WARNING: Problem reading EW Pmatrix'
7 WRITE(8,*)' WARNING: Problem reading EW Umatr matrix'
STOP ' WARNING: Problem reading EW Pmatrix'
8 WRITE(8,*)' WARNING: Problem reading EW Pdiag matrix'
STOP ' WARNING: Problem reading EW Pmatrix'

END FUNCTION Read_CC_matrices

SUBROUTINE PREPARE_CCmatr(Jcn, pcn, ncc, nmaxp, nmaxu, ndim)

   REAL*8 Jcn
   INTEGER pcn, ndim, ncc, nmaxp, nmaxu, i1
   LOGICAL debug
   DATA debug/.FALSE./
   ndim = 0
   nmaxp = 0
   nmaxu = 0
   if (debug) write(*,*) 
   if(debug) write(*,*) 'Pdiag, MAX_cc_mod:',MAX_cc_mod
   DO ncc = 1, MAX_cc_mod
      ndim  = 0
      nmaxp = 0
      if(NINT(2*STLcc(ncc)%Jcn) == NINT(2*Jcn) .and. STLcc(ncc)%Pcn == Pcn) THEN

         ndim = STLcc(ncc)%nceq

         CALL AllocCCmatr(ndim)

         ! reading diagonal elements p_{alpha}
         nmaxp = ncc + ndim - 1
         if (debug) write(*,*) 'J,Pi,nceq=',sngl(STLcc(ncc)%Jcn), STLcc(ncc)%Pcn, STLcc(ncc)%nceq, nmaxp
         IF (INTerf==0) THEN
            do i1 = ncc, nmaxp
               if (debug) WRITE (*,*) i1, i1 - ncc +1, STLcc(i1)%tlj
               Pdiag(i1 - ncc +1) = STLcc(i1)%tlj
            enddo
         ELSE
            do i1 = ncc, nmaxp
               if (debug) WRITE (*,*) i1, i1 - ncc +1, CCpdiag(i1)%pdiag
               Pdiag(i1 - ncc +1) = CCpdiag(i1)%pdiag
            enddo
         END IF

         EXIT
      endif
   ENDDO

   RETURN !!! DISABLE Umatrix reading
   IF(INTerf==0) RETURN

   ! if (debug) write(*,*) 'Umatr, MAX_umatr: ',MAX_umatr
                write(*,*) 'Umatr, MAX_umatr: ',MAX_umatr
   DO ncc = 1, MAX_umatr
      ndim =  0
      nmaxu = 0
      if(NINT(2*CCumatrix(ncc)%Jcn) /= NINT(2*Jcn) .or. CCumatrix(ncc)%Pcn /= Pcn) cycle ! Jpi = 3/2+

      ndim = CCumatrix(ncc)%nceq
      nmaxu = ncc + ndim*ndim - 1                                                                           

      if (debug) write(*,*) 'J,Pi,nceq=',sngl(CCumatrix(ncc)%Jcn), CCumatrix(ncc)%Pcn, CCumatrix(ncc)%nceq
      if (debug) write(*,*) 'ndim,ndim^2,maxu=',ndim,ndim*ndim,nmaxu

      do i1 = ncc, nmaxu
         if (debug) WRITE (*,*) i1, i1-ncc+1, CCumatrix(i1)%irow, CCumatrix(i1)%icol, CCumatrix(i1)%umatrix
         Umatr(CCumatrix(i1)%irow,CCumatrix(i1)%icol) = CCumatrix(i1)%umatrix
      enddo
      EXIT

   ENDDO

END SUBROUTINE PREPARE_CCmatr

END MODULE TLJs


