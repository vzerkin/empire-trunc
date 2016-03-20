! $Rev: 4628 $
! $Author: rcapote $
! $Date: 2016-03-20 23:33:11 +0100 (So, 20 Mär 2016) $
!
MODULE TLJs
   IMPLICIT NONE

   include 'dimension.h'
   include 'global.h'

   INTEGER, PUBLIC :: MAX_cc_mod 

   TYPE, PUBLIC :: cc_channel
     INTEGER*4 Pcn        ! CN parity to which cc-channel couples (+1 or -1)
     REAL*8 Jcn           ! CN spin   to which cc-channel couples
     INTEGER*4 nceq       ! Number of diagonal TLj in the P-matrix for a given CN J-pi
     INTEGER*4 lev        ! number of the collective level (in the collective level file)
     INTEGER*4 l          ! orbital angular momentum l
     REAL*8 j             ! channel spin (j in T_lj)
     REAL*8 tlj           ! CN transmission coefficient
     REAL*8 pchan         ! Pchan value
   END TYPE cc_channel

   TYPE, PUBLIC :: cc_pdiag    
     INTEGER*4 Pcn        ! CN parity to which cc-channel couples (+1 or -1)
     REAL*8 Jcn           ! CN spin   to which cc-channel couples
     INTEGER*4 nceq       ! Number of diagonal TLj in the P-matrix for a given CN J-pi
     REAL*8 pdiag         ! Pdiag(irow) - Diagonal matrix elements of the diagonal P-diag matrix
   END TYPE cc_pdiag

   TYPE, PUBLIC :: cc_umatrix    
     INTEGER*4 Pcn        ! CN parity to which cc-channel couples (+1 or -1)
     REAL*8 Jcn           ! CN spin   to which cc-channel couples
     INTEGER*4 nceq       ! Number of diagonal TLj in the P-matrix for a given CN J-pi
     INTEGER*4 irow       ! Row number of the matrix in the transformed space
     INTEGER*4 icol       ! Column number of the matrix in the transformed space
     COMPLEX*16 umatrix   ! U(irow,icol) - Matrix element of the unitary transformation matrix U 
   END TYPE cc_umatrix

   TYPE(cc_channel), PUBLIC, ALLOCATABLE, TARGET :: STLcc(:)      
   TYPE(cc_umatrix), PUBLIC, ALLOCATABLE, TARGET :: CCpmatrix(:) 
   TYPE(cc_pdiag), PUBLIC, ALLOCATABLE, TARGET :: CCpdiag(:)     
   TYPE(cc_umatrix), PUBLIC, ALLOCATABLE, TARGET :: CCumatrix(:) 

   PUBLIC AllocTLJs, DelTLJs, AllocCCmatr, DelCCmatr, Open_CC_Files, Read_CC_Matrices, Close_CC_Files  

   PRIVATE

   COMPLEX*16, ALLOCATABLE :: Pmatr(:,:),Umatr(:,:) ! EW matrices Smatr(:,:) 
   REAL*8, ALLOCATABLE :: Pdiag(:),Pchan(:) 

   CONTAINS

   !----------------------------------------------------------------------------------------------------

   SUBROUTINE AllocTLJs(nch)
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
   CCpdiag%nceq  = 1
   CCpdiag%pdiag = 0.d0

   !TYPE(cc_umatrix), PUBLIC, ALLOCATABLE, TARGET :: CC_umatrix(:) 
   IF(allocated(CCumatrix)) DEALLOCATE(CCumatrix)
   ALLOCATE(CCumatrix(nch),STAT=my)
   IF(my /= 0) THEN
     WRITE(8,*)  'ERROR: Insufficient memory for CCumatrix (in TLJs)'
     WRITE(12,*) 'ERROR: Insufficient memory for CCumatrix (in TLJs)'
     STOP 'ERROR: Insufficient memory for CCumatrix (in TLJs)'
     RETURN
   ENDIF

   CCumatrix%Pcn     = 1
   CCumatrix%Jcn     = 0.d0
   CCumatrix%nceq    = 1
   CCumatrix%irow    = 1
   CCumatrix%icol    = 1
   CCumatrix%umatrix = (0.d0,0.d0)

   IF(allocated(CCpmatrix)) DEALLOCATE(CCpmatrix)
   ALLOCATE(CCpmatrix(nch),STAT=my)
   IF(my /= 0) THEN
     WRITE(8,*)  'ERROR: Insufficient memory for CCpmatrix (in TLJs)'
     WRITE(12,*) 'ERROR: Insufficient memory for CCpmatrix (in TLJs)'
     STOP 'ERROR: Insufficient memory for CCpmatrix (in TLJs)'
     RETURN
   ENDIF

   CCpmatrix%Pcn     = 1
   CCpmatrix%Jcn     = 0.d0
   CCpmatrix%nceq    = 1
   CCpmatrix%irow    = 1
   CCpmatrix%icol    = 1
   CCpmatrix%umatrix = (0.d0,0.d0)

   RETURN
   END SUBROUTINE AllocTLJs

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

   SUBROUTINE AllocCCmatr(nch)

   IMPLICIT NONE
   INTEGER*4, INTENT(IN) :: nch
   INTEGER my

   IF(allocated(Pchan)) DEALLOCATE(Pchan)
   ALLOCATE(Pchan(nch),STAT=my)
   IF(my /= 0) GOTO 20
   Pchan = 0.0d0

!  IF(allocated(Smatr)) DEALLOCATE(Smatr)
!  ALLOCATE(Smatr(nch,nch),STAT=my)
!  IF(my /= 0) GOTO 20
!  Smatr = (0.d0,0.d0)

   IF(INTERF==0) RETURN

   IF(allocated(Pmatr)) DEALLOCATE(Pmatr)
   ALLOCATE(Pmatr(nch,nch),STAT=my)
   IF(my /= 0) GOTO 30
   Pmatr = (0.d0,0.d0)

!  EW matrices
   IF(allocated(Pdiag)) DEALLOCATE(Pdiag)
   ALLOCATE(Pdiag(nch),STAT=my)
   IF(my /= 0) GOTO 30
   Pdiag = 0.0d0

   IF(allocated(Umatr)) DEALLOCATE(Umatr)
   ALLOCATE(Umatr(nch,nch),STAT=my)
   IF(my /= 0) GOTO 30
   Umatr = (0.d0,0.d0)

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

   IF(allocated(Pchan)) DEALLOCATE(Pchan)
!  IF(allocated(Smatr)) DEALLOCATE(Smatr)

   IF(INTERF==0) RETURN

!  EW matrices
   IF(allocated(Pmatr)) DEALLOCATE(Pmatr)
   IF(allocated(Pdiag)) DEALLOCATE(Pdiag)
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
   !INQUIRE (FILE = (ctldir//ctmp23//'_Pchan.txt'),EXIST = fexist)
   INQUIRE (FILE = ('INCIDENT_Pchan.txt'),EXIST = fexist)
   IF (.not. fexist) RETURN ! *_Pchan.txt not found
!  INQUIRE (FILE = (ctldir//ctmp23//'_Smatr.txt'),EXIST = fexist)
!  INQUIRE (FILE = ('INCIDENT_Smatr.txt'),EXIST = fexist)
!  IF (.not. fexist) RETURN ! *_Smatr.txt not found

!  OPEN (126, FILE = (ctldir//ctmp23//'_Pchan.txt'), STATUS = 'old')
   OPEN (126, FILE = ('INCIDENT_Pchan.txt'), STATUS = 'old')
!  OPEN (58 , FILE = (ctldir//ctmp23//'_Smatr.txt'), STATUS = 'old')
!  OPEN (58 , FILE = ('INCIDENT_Smatr.txt'), STATUS = 'old')

   Open_CC_files = .TRUE. ! normal return

   IF(INTERF==0) RETURN

   Open_CC_files = .FALSE. 
!--------------
!--EW matrices 
!--------------

!--The INQUIRE statement determines whether or not the file exists.
!--If it does not, the program calculates new transmission coeff.
   !INQUIRE (FILE = (ctldir//ctmp23//'_Pmatr.txt'),EXIST = fexist)
   INQUIRE (FILE = ('INCIDENT_Pmatr.txt'),EXIST = fexist)
   IF (.not. fexist) RETURN ! *_Pmatr.txt not found
   !INQUIRE (FILE = (ctldir//ctmp23//'_Pdiag.txt'),EXIST = fexist)
   INQUIRE (FILE = ('INCIDENT_Pdiag.txt'),EXIST = fexist)
   IF (.not. fexist) RETURN ! *_Pdiag.txt not found
   !INQUIRE (FILE = (ctldir//ctmp23//'_Umatr.txt'),EXIST = fexist)
   INQUIRE (FILE = ('INCIDENT_Umatr.txt'),EXIST = fexist)
   IF (.not. fexist) RETURN ! *_Umatr.txt not found

   !OPEN (125, FILE = (ctldir//ctmp23//'_Pmatr.txt'), STATUS = 'old')
   !OPEN (60 , FILE = (ctldir//ctmp23//'_Umatr.txt'), STATUS = 'old')
   !OPEN (61 , FILE = (ctldir//ctmp23//'_Pdiag.txt'), STATUS = 'old')
   OPEN (125, FILE = ('INCIDENT_Pmatr.txt'), STATUS = 'old')
   OPEN (60 , FILE = ('INCIDENT_Umatr.txt'), STATUS = 'old')
   OPEN (61 , FILE = ('INCIDENT_Pdiag.txt'), STATUS = 'old')

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
   DOUBLE PRECISION jc, jj, sreal, simag
   INTEGER nceq, nc1, nc2, i1, i2, nelem, my, ncc, nch
   INTEGER nlev, nl
   CHARACTER*1 parc
   COMPLEX*16, ALLOCATABLE :: cres(:,:),ctmp(:,:), Umatr_T(:,:) ! temporal matrices 
   TYPE (cc_channel), POINTER :: ps_tlj
   TYPE (cc_umatrix), POINTER :: ps_pmatrix
   TYPE (cc_umatrix), POINTER :: ps_umatrix
   TYPE (cc_pdiag), POINTER :: ps_pdiag

   Read_CC_matrices = .FALSE.

   nch = 0
!==Reading Pchan
   DO ncc = 1, MAX_CC
     !--jc,parc are the channel spin and parity
     !--nceq is the number of coupled equations
     READ (126,'(1x,f9.1,4x,a1,1x,i4)',END=10,ERR=10) jc, parc, nceq  
     ! write(*,*) jc,parc,nceq  
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
10 Read_CC_matrices = .TRUE.

   IF(INTerf==0) RETURN

   Read_CC_matrices = .FALSE.

   ! TYPE(cc_umatrix), PUBLIC, ALLOCATABLE, TARGET :: CCpmatrix(:) 
   !==Reading Pmatr
   nch = 0
   DO ncc = 1, MAX_CC
     ps_pmatrix => CCpmatrix(ncc)
     !--jc,parc are the channel spin and parity
     !--nceq is the number of coupled equations
     READ (125,'(1x,f9.1,4x,a1,1x,i4)',END=12,ERR=12) jc, parc, nceq  
     write(*,*) jc,parc,nceq  
     ps_pmatrix%Jcn = jc
     ps_pmatrix%Pcn = 1
     if(parc == '-') ps_pmatrix%Pcn = -1
     ps_pmatrix%nceq = nceq                        
     !--Loop over the number of coupled equations (squared)
     nelem = nceq*(nceq+1)/2
     DO i1 = 1, nelem
       READ (125,*,END = 6,ERR = 6) nc1, nc2, sreal, simag
	   write(*,'(1x,I3,1x,I3,2(1x,d12.6))') nc1,nc2,sngl(sreal),sngl(simag)
       ps_pmatrix%irow = nc1
       ps_pmatrix%icol = nc2
       ps_pmatrix%umatrix = CMPLX(sreal,simag,8)
     ENDDO
   ENDDO

12 CONTINUE
   PAUSE 
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
   DO ncc = 1, MAX_CC
     ps_umatrix => CCumatrix(ncc)
     !--jc,parc are the channel spin and parity
     !--nceq is the number of coupled equations
     READ (60,'(1x,f9.1,4x,a1,1x,i4)',END=14,ERR=14) jc, parc, nceq  
     !  write(*,*) jc,parc,nceq  
     ps_umatrix%Jcn = jc
     ps_umatrix%Pcn = 1                                           
     if(parc == '-') ps_umatrix%Pcn = -1
     ps_umatrix%nceq = nceq                        
     !
     !--Loop over the number of coupled equations
     DO i1 = 1, nceq
       READ (60,*,END = 7,ERR = 7) ! nc1
       DO i2 = 1, nceq
         READ (60,*,END = 7,ERR = 7)  sreal, simag
  	     ! write(*,'(1x,I3,1x,I3,2(1x,d12.6))') i1,i2,sngl(sreal),sngl(simag)
         ps_umatrix%irow = i1
         ps_umatrix%icol = i2
         ps_umatrix%umatrix = CMPLX(sreal,simag,8)
       ENDDO
     ENDDO
   ENDDO

14 CONTINUE
   ! PAUSE 

   !TYPE(cc_pdiag), PUBLIC, ALLOCATABLE, TARGET :: CCpdiag(:)     
   !==Reading Pdiag
   DO ncc = 1, MAX_CC
     ps_pdiag => CCpdiag(ncc)
     !--jc,parc are the channel spin and parity
     !--nceq is the number of coupled equations
     READ (60,'(1x,f9.1,4x,a1,1x,i4)',END=16,ERR=16) jc, parc, nceq  
     ps_pdiag%Jcn = jc
     ps_pdiag%Pcn = 1
     if(parc == '-') ps_pdiag%Pcn = -1
     ps_pdiag%nceq = nceq                        
     !  write(*,*) jc,parc,nceq  
     !
     !--Loop over the number of coupled equations
     DO i1 = 1, nceq
       READ (61,*,END = 8,ERR = 8) nc1, sreal
       ps_pdiag%pdiag = sreal
     ENDDO
   ENDDO

16 CONTINUE
   ! PAUSE 

   Read_CC_matrices = .TRUE.

   RETURN

   DO ncc = 1, MAX_CC

     ps_umatrix => CCumatrix(ncc)

     write(*,*) 'J,Pi=',ps_umatrix%Jcn, ps_umatrix%Pcn
     nch = ps_pdiag%nceq
     CALL AllocCCmatr(nch)

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
     !  write (*,'(1x,A7,2(1x,i3),1x,d12.6,1x,d12.6)') 'Umatr  =',2,1,DREAL(Umatr(2,1)),DIMAG(Umatr(2,1))
     !  write (*,'(1x,A7,2(1x,i3),1x,d12.6,1x,d12.6)') 'Umatr_T=',1,2,DREAL(Umatr_T(1,2)),DIMAG(Umatr_T(1,2))
     !  write (*,'(1x,A7,2(1x,i3),1x,d12.6,1x,d12.6)') 'Umatr  =',3,1,DREAL(Umatr(3,1)),DIMAG(Umatr(3,1))
     !  write (*,'(1x,A7,2(1x,i3),1x,d12.6,1x,d12.6)') 'Umatr_T=',1,3,DREAL(Umatr_T(1,3)),DIMAG(Umatr_T(1,3))
     !  write (*,*)

     write (*,'(1x,A,1x,6(d12.6,1x))') 'Pdiag =',(Pdiag(i1),i1=1,2)

     write(*,*) '1,1=',Pmatr(1,1)
     write(*,*) '2,2=',Pmatr(2,2)
     write(*,*) '1,2=',Pmatr(1,2)
     write(*,*) '2,1=',Pmatr(2,1)

     IF(allocated(cres)) DEALLOCATE(cres)
     ALLOCATE(cres(nch,nch),STAT=my)
     IF(my /= 0) then
       WRITE(8,*)  'ERROR: Insufficient memory for cres matrix in TLJs'
       WRITE(12,*) 'ERROR: Insufficient memory for cres matrix in TLJs'
       STOP
     ENDIF
     cres = 0.0d0

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
     CALL DelCCmatr() 

     pause

   ENDDO

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

END MODULE TLJs


