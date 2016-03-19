! $Rev: 4624 $
! $Author: rcapote $
! $Date: 2016-03-19 23:21:18 +0100 (Sa, 19 Mär 2016) $
!
   MODULE TLJs
   IMPLICIT NONE

   include 'dimension.h'
   include 'global.h'

   TYPE, PUBLIC :: cc_channel
     INTEGER*4 NJcn       ! CN parity to which cc-channel couples (+1 or -1)
     REAL*8 Jcn           ! CN spin   to which cc-channel couples
     INTEGER*4 nceq       ! Number of diagonal TLj in the P-matrix for a given CN J-pi
     INTEGER*4 lev        ! number of the collective level (in the collective level file)
     INTEGER*4 l          ! orbital angular momentum l
     REAL*8 j             ! channel spin (j in T_lj)
     REAL*8 tlj           ! Tlj value
   END TYPE cc_channel

   INTEGER*4, PUBLIC :: MAX_CCch
   TYPE(cc_channel), PUBLIC, ALLOCATABLE, TARGET :: STLcc(:) ! coupled channels for inelastic calculations (including E-W transformation)
   PUBLIC AllocTLJs, DelTLJs 

   COMPLEX*16, PUBLIC, ALLOCATABLE :: Umatr(:,:),Umatr_T(:,:),Pmatr(:,:) ! EW matrices Smatr(:,:) 

   REAL*8, PUBLIC, ALLOCATABLE :: Pdiag(:),Pchan(:) 

   CONTAINS

   !----------------------------------------------------------------------------------------------------

   SUBROUTINE AllocTLJs(nch)
   IMPLICIT NONE
   INTEGER*4 nch
   INTEGER my

   IF(allocated(STLcc)) DEALLOCATE(STLcc)
   ALLOCATE(STLcc(nch),STAT=my)
   IF(my /= 0) THEN
     WRITE(8,*)  'ERROR: Insufficient memory for TLJs'
     WRITE(12,*) 'ERROR: Insufficient memory for TLJs'
     STOP 'ERROR: Insufficient memory for TLJs'
     RETURN
   ENDIF

   STLcc%NJcn = 1
   STLcc%Jcn  = 0.d0
   STLcc%nceq = 0
   STLcc%lev  = 0
   STLcc%l    = 0
   STLcc%j    = 0.d0
   STLcc%tlj  = 0.d0

   RETURN
   END SUBROUTINE AllocTLJs

   !----------------------------------------------------------------------------------------------------

   SUBROUTINE DelTLJs()
   IMPLICIT NONE
   IF(allocated(STLcc))    DEALLOCATE(STLcc)
   RETURN
   END SUBROUTINE DelTLJs

   !----------------------------------------------------------------------------------------------------

   SUBROUTINE AllocCCmatr(nch)

   IMPLICIT NONE

   INTEGER*4, INTENT(IN) :: nch

   INTEGER my

   IF(DIRECT==0) RETURN

   IF(allocated(Pmatr)) DEALLOCATE(Pmatr)
   ALLOCATE(Pmatr(nch,nch),STAT=my)
   IF(my /= 0) GOTO 20
   Pmatr = 0.0d0

   IF(allocated(Pchan)) DEALLOCATE(Pchan)
   ALLOCATE(Pchan(nch),STAT=my)
   IF(my /= 0) GOTO 20
   Pchan = 0.0d0

!  IF(allocated(Smatr)) DEALLOCATE(Smatr)
!  ALLOCATE(Smatr(nch,nch),STAT=my)
!  IF(my /= 0) GOTO 20
!  Smatr = 0.0d0

   IF(INTERF==0) RETURN
!  EW matrices
   IF(allocated(Pdiag)) DEALLOCATE(Pdiag)
   ALLOCATE(Pdiag(nch),STAT=my)
   IF(my /= 0) GOTO 30
   Pdiag = 0.0d0

   IF(allocated(Umatr)) DEALLOCATE(Umatr)
   ALLOCATE(Umatr(nch,nch),STAT=my)
   IF(my /= 0) GOTO 30
   Umatr = 0.0d0

   IF(allocated(Umatr_T)) DEALLOCATE(Umatr_T)
   ALLOCATE(Umatr_T(nch,nch),STAT=my)
   IF(my /= 0) GOTO 30
   Umatr_T = 0.0d0

   RETURN
20 WRITE(8,*)  'ERROR: Insufficient memory for CC matrices in HRTW'
   WRITE(12,*) 'ERROR: Insufficient memory for CC matrices in HRTW'
   STOP 'ERROR: Insufficient memory for CC matrices in HRTW'
30 WRITE(8,*)  'ERROR: Insufficient memory for EW matrices in HRTW'
   WRITE(12,*) 'ERROR: Insufficient memory for EW matrices in HRTW'
   STOP 'ERROR: Insufficient memory for EW matrices in HRTW'

   END SUBROUTINE AllocCCmatr

   !----------------------------------------------------------------------------------------------------

   SUBROUTINE DelCCmatr()

   IMPLICIT NONE

   IF(DIRECT==0) RETURN

   IF(allocated(Pmatr)) DEALLOCATE(Pmatr)
   IF(allocated(Pchan)) DEALLOCATE(Pchan)
!  IF(allocated(Smatr)) DEALLOCATE(Smatr)

   IF(INTERF==0) RETURN

!  EW matrices
   IF(allocated(Pdiag)) DEALLOCATE(Pdiag)
   IF(allocated(Umatr)) DEALLOCATE(Umatr)
   IF(allocated(Umatr_T)) DEALLOCATE(Umatr_T)

   RETURN
   END SUBROUTINE DelCCmatr


   LOGICAL FUNCTION Open_EW_files()

   IMPLICIT NONE
   LOGICAL fexist
   CHARACTER*3 ctldir
   CHARACTER*23 ctmp23
   DATA ctldir/'TL/'/

   Open_EW_files = .TRUE.

   IF(DIRECT==0) RETURN

   Open_EW_files = .FALSE.

   WRITE (ctmp23,'(i3.3,i3.3,1h_,i3.3,i3.3,1h_,i9.9)') INT(ZEJc(0)),INT(AEJc(0)),INT(Z(0)),INT(A(0)),INT(EINl*1000000)

!--The INQUIRE statement determines whether or not the file exists.
!--If it does not, the program calculates new transmission coeff.
   INQUIRE (FILE = (ctldir//ctmp23//'_Pmatr.txt'),EXIST = fexist)
   IF (.not. fexist) RETURN ! *_Pmatr.txt not found
   INQUIRE (FILE = (ctldir//ctmp23//'_Pchan.txt'),EXIST = fexist)
   IF (.not. fexist) RETURN ! *_Pchan.txt not found
!  INQUIRE (FILE = (ctldir//ctmp23//'_Smatr.txt'),EXIST = fexist)
!  IF (.not. fexist) RETURN ! *_Smatr.txt not found

   OPEN (125, FILE = (ctldir//ctmp23//'_Pmatr.txt'), STATUS = 'old',ERR=10)
   OPEN (126, FILE = (ctldir//ctmp23//'_Pchan.txt'), STATUS = 'old',ERR=10)
!  OPEN (58 , FILE = (ctldir//ctmp23//'_Smatr.txt'), STATUS = 'old',ERR=10)

   Open_EW_files = .TRUE. ! normal return

   IF(INTERF==0) RETURN

   Open_EW_files = .FALSE. 
!--------------
!--EW matrices 
!--------------

!--The INQUIRE statement determines whether or not the file exists.
!--If it does not, the program calculates new transmission coeff.
   INQUIRE (FILE = (ctldir//ctmp23//'_Pdiag.txt'),EXIST = fexist)
   IF (.not. fexist) RETURN ! *_Pdiag.txt not found
   INQUIRE (FILE = (ctldir//ctmp23//'_Umatr.txt'),EXIST = fexist)
   IF (.not. fexist) RETURN ! *_Umatr.txt not found

   OPEN (60 ,FILE = (ctldir//ctmp23//'_Umatr.txt'), STATUS = 'old',ERR=10)
   OPEN (61 ,FILE = (ctldir//ctmp23//'_Pdiag.txt'), STATUS = 'old',ERR=10)

   Open_EW_files = .TRUE. ! normal return

10 RETURN
   END FUNCTION Open_EW_files

   !----------------------------------------------------------------------------------------------------

   Subroutine Close_EW_files()
  !  Close(58)
     IF(DIRECT==0) RETURN
     Close(125)
     Close(126)
     IF(INTERF==0) RETURN
     Close(60)
     Close(61)
   END Subroutine Close_EW_files

   !----------------------------------------------------------------------------------------------------

   LOGICAL FUNCTION Read_EW_matrices()
   IMPLICIT NONE
   DOUBLE PRECISION jc, sreal, simag
   INTEGER nceq, nc1, nc2, i1, i2, nelem
   CHARACTER*1 parc
   COMPLEX*16 cres(2*NDLW,2*NDLW),ctmp(2*NDLW,2*NDLW)

   Read_EW_matrices = .TRUE.

   IF(DIRECT==0) RETURN

   Read_EW_matrices = .FALSE.

   Pmatr   = 0.d0   
   Pchan   = 0.d0   
!  Smatr   = 0.d0   

!==Reading Pmatr
!--jc,parc are the channel spin and parity
!--nceq is the number of coupled equations
   READ (125,'(1x,f9.1,4x,a1,1x,i4)',END=5,ERR=5) jc, parc, nceq  
!
!--Loop over the number of coupled equations (squared)
   nelem = nceq*(nceq+1)/2
   DO i1 = 1, nelem
     READ (125,*,END = 5,ERR = 5) nc1, nc2, sreal, simag
     Pmatr(nc1,nc2) = CMPLX(sreal,simag,8)
     if(nc1 /= nc2) Pmatr(nc2,nc1) = Pmatr(nc1,nc2) 
   ENDDO



!  TO READ Pchan.txt
!  (126)


!==Reading Smatr
!---Here the calculated files are read
!--jc,parc are the channel spin and parity
!--nceq is the number of coupled equations
!  READ (59,'(1x,f9.1,4x,a1,1x,i4)',END=6,ERR=6) jc, parc, nceq  
!  write(*,*) jc,parc,nceq  
!
!--Loop over the number of coupled equations
!  DO i1 = 1, nceq
!     DO i2 = 1, i1
!      READ (59,*,END = 6,ERR = 6) nc1, nc2, sreal, simag
!      Smatr(nc1,nc2) = CMPLX(sreal,simag,8)
!       if(nc1 /= nc2) Smatr(nc2,nc1) = Smatr(nc1,nc2) 
!    ENDDO
!  ENDDO

   Read_EW_matrices = .TRUE.

   IF(INTERF==0) RETURN

   Read_EW_matrices = .FALSE.
   Pdiag   = 0.d0   
   Umatr   = 0.d0   
   Umatr_T = 0.d0   

!==Reading Umatr
!--jc,parc are the channel spin and parity
!--nceq is the number of coupled equations
   READ (60,'(1x,f9.1,4x,a1,1x,i4)',END=7,ERR=7) jc, parc, nceq  
!  write(*,*) jc,parc,nceq  
!
!--Loop over the number of coupled equations
   DO i1 = 1, nceq
     READ (60,*,END = 7,ERR = 7) ! nc1
     DO i2 = 1, nceq
       READ (60,*,END = 7,ERR = 7)  sreal, simag
       Umatr(i1,i2) = CMPLX(sreal,simag,8)
     ENDDO
   ENDDO

!  Calculating the transposed matrix
   Umatr_T = TRANSPOSE(Umatr)

!  TRANSPOSE WORKS !!!! March 2016
!  write (*,'(1x,A7,2(1x,i3),1x,d12.6,1x,d12.6)') 'Umatr  =',2,1,DREAL(Umatr(2,1)),DIMAG(Umatr(2,1))
!  write (*,'(1x,A7,2(1x,i3),1x,d12.6,1x,d12.6)') 'Umatr_T=',1,2,DREAL(Umatr_T(1,2)),DIMAG(Umatr_T(1,2))
!  write (*,'(1x,A7,2(1x,i3),1x,d12.6,1x,d12.6)') 'Umatr  =',3,1,DREAL(Umatr(3,1)),DIMAG(Umatr(3,1))
!  write (*,'(1x,A7,2(1x,i3),1x,d12.6,1x,d12.6)') 'Umatr_T=',1,3,DREAL(Umatr_T(1,3)),DIMAG(Umatr_T(1,3))
!  write (*,*)

!==Reading Pdiag
!--jc,parc are the channel spin and parity
!--nceq is the number of coupled equations
   READ (61,'(1x,f9.1,4x,a1,1x,i4)',END=8,ERR=8) jc, parc, nceq  
!  write(*,*) jc,parc,nceq  
!
!--Loop over the number of coupled equations
   DO i1 = 1, nceq
     READ (61,*,END = 8,ERR = 8) nc1, nc2, sreal, simag
     Pdiag(nc1) = sreal
   ENDDO

   Read_EW_matrices = .TRUE.

   RETURN

   ! Checking part to be done
   write (*,'(1x,A,1x,6(d12.6,1x))') 'Pdiag =',(Pdiag(i1),i1=1,2)

   write(*,*) '1,1=',Pmatr(1,1)
   write(*,*) '2,2=',Pmatr(2,2)
   write(*,*) '1,2=',Pmatr(1,2)
   write(*,*) '2,1=',Pmatr(2,1)

   ctmp = 0.d0
   cres = 0.d0

   ctmp = MATMUL(Umatr,Pmatr)

   do i1=1,nceq
   do i2=1,nceq
   sreal = REAL(Umatr_T(i1,i2))
   simag = -DIMAG(UMatr_T(i1,i2))
   Umatr_T(i1,i2) = CMPLX(sreal,simag,8)
   enddo
   enddo

   cres = MATMUL(ctmp,Umatr_T)

   write (*,'(1x,A,1x,6(d12.6,1x))') 'Calc  =',(cres(i1,i1),i1=1,2)

   pause

   RETURN
 5 WRITE(8,*)' WARNING: Problem reading EW Pmatr matrix'
   STOP ' WARNING: Problem reading EW Pmatrix'
 6 WRITE(8,*)' WARNING: Problem reading EW Smatr matrix'
   STOP ' WARNING: Problem reading EW Pmatrix'
 7 WRITE(8,*)' WARNING: Problem reading EW Umatr matrix'
   STOP ' WARNING: Problem reading EW Pmatrix'
 8 WRITE(8,*)' WARNING: Problem reading EW Pdiag matrix'
   STOP ' WARNING: Problem reading EW Pmatrix'

   END FUNCTION Read_EW_matrices

END MODULE TLJs


