! $Rev: 5090 $
! $Author: capote $
! $Date: 2018-04-28 08:09:29 +0200 (Sa, 28 Apr 2018) $
!
MODULE TLJs
   IMPLICIT NONE

   include 'dimension.h'
   include 'global.h'

   INTEGER, PUBLIC :: MAX_cc_mod,MAX_pmatr,MAX_umatr, NDIm_cc_matrix

   TYPE, PUBLIC :: cc_channel
      INTEGER Pcn        ! CN parity to which cc-channel couples (+1 or -1)
      REAL*8 Jcn         ! CN spin   to which cc-channel couples
      INTEGER nceq       ! Number of diagonal TLj in the P-matrix for a given CN J-pi
      INTEGER lev        ! number of the collective level (in the collective level file)
      INTEGER l          ! orbital angular momentum l
      REAL*8 j           ! channel spin (j in T_lj)
      REAL*8 tlj         ! CN transmission coefficient
      REAL*8 pchan       ! Pchan value
   END TYPE cc_channel

   TYPE, PUBLIC :: cc_pdiag
      INTEGER Pcn        ! CN parity to which cc-channel couples (+1 or -1)
      REAL*8 Jcn         ! CN spin   to which cc-channel couples
      INTEGER nceq       ! Number of diagonal TLj in the P-matrix for a given CN J-pi
      REAL*8 pdiag       ! Pdiag(irow) - Diagonal matrix elements of the diagonal P-diag matrix
   END TYPE cc_pdiag

   TYPE, PUBLIC :: cc_umatrix
      INTEGER Pcn        ! CN parity to which cc-channel couples (+1 or -1)
      REAL*8 Jcn         ! CN spin   to which cc-channel couples
      INTEGER nceq       ! Number of diagonal TLj in the P-matrix for a given CN J-pi
      INTEGER irow       ! Row number of the matrix in the transformed space
      INTEGER icol       ! Column number of the matrix in the transformed space
      COMPLEX*16 umatrix ! U(irow,icol) - Matrix element of the unitary transformation matrix U
   END TYPE cc_umatrix

   TYPE(cc_channel), PUBLIC, ALLOCATABLE, TARGET :: STLcc(:)
   TYPE(cc_umatrix), PUBLIC, ALLOCATABLE, TARGET :: CCsmatrix(:)
   TYPE(cc_pdiag)  , PUBLIC, ALLOCATABLE, TARGET :: CCpdiag(:)
   TYPE(cc_umatrix), PUBLIC, ALLOCATABLE, TARGET :: CCumatrix(:)
   TYPE(cc_umatrix), PUBLIC, ALLOCATABLE, TARGET :: CCpmatrix(:)

   PUBLIC AllocTLJmatr, AllocEWmatr, DelTLJs
   PUBLIC AllocCCmatr, DelCCmatr, Prepare_CCmatr
   PUBLIC Open_CC_Files, Read_CC_Matrices, Close_CC_Files
   REAL*8, PUBLIC, ALLOCATABLE :: Pdiag(:), Pchan(:), Sphase(:), sigma_alph_beta(:,:), & 
                                  PPdiag(:,:), Sdiag(:,:), &
                                  ZItmp(:,:), ZRtmp1(:,:), ZItmp1(:,:)

   COMPLEX*16, PUBLIC, ALLOCATABLE :: Pmatr(:,:),Umatr(:,:),Smatr(:,:),Tmatr(:,:) ! EW matrices 

   PRIVATE
   
CONTAINS

   !----------------------------------------------------------------------------------------------------
   SUBROUTINE AllocCCmatr(nch)
      IMPLICIT NONE
      INTEGER, INTENT(IN) :: nch
      INTEGER my

      NDIm_cc_matrix = nch

      IF(allocated(Pchan)) DEALLOCATE(Pchan)
      ALLOCATE(Pchan(nch),STAT=my)
      IF(my /= 0) GOTO 20
      Pchan = 0.0d0

      IF(allocated(Pdiag)) DEALLOCATE(Pdiag)
      ALLOCATE(Pdiag(nch),STAT=my)
      IF(my /= 0) GOTO 20
      Pdiag = 0.0d0

      IF(INTERF==0) RETURN
      !  EW matrices

      IF(allocated(Sphase)) DEALLOCATE(Sphase)
      ALLOCATE(Sphase(nch),STAT=my)
      IF(my /= 0) GOTO 20
      Sphase = 0.0d0

      IF(allocated(sigma_alph_beta)) DEALLOCATE(sigma_alph_beta)
      ALLOCATE(sigma_alph_beta(nch,nch),STAT=my)
      IF(my /= 0) GOTO 30
      sigma_alph_beta = 0.0d0

      IF(allocated(Pmatr)) DEALLOCATE(Pmatr)
      ALLOCATE(Pmatr(nch,nch),STAT=my)
      IF(my /= 0) GOTO 30
      Pmatr = (0.d0,0.d0)

      IF(allocated(Tmatr)) DEALLOCATE(Tmatr)
      ALLOCATE(Tmatr(nch,nch),STAT=my)
      IF(my /= 0) GOTO 30
      Tmatr = (0.d0,0.d0)

      IF(allocated(Smatr)) DEALLOCATE(Smatr)
      ALLOCATE(Smatr(nch,nch),STAT=my)
      IF(my /= 0) GOTO 30
      Smatr = (0.d0,0.d0)

      IF(allocated(Umatr)) DEALLOCATE(Umatr)
      ALLOCATE(Umatr(nch,nch),STAT=my)
      IF(my /= 0) GOTO 30
      Umatr = (0.d0,0.d0)

      IF(allocated(Sdiag)) DEALLOCATE(Sdiag)
      ALLOCATE(Sdiag(nch,nch),STAT=my)
      IF(my /= 0) GOTO 30
      Sdiag = 0.d0

      IF(allocated(PPdiag)) DEALLOCATE(PPdiag)
      ALLOCATE(PPdiag(nch,nch),STAT=my)
      IF(my /= 0) GOTO 30
      PPdiag = 0.d0

      IF(allocated(ZItmp)) DEALLOCATE(ZItmp)
      ALLOCATE(ZItmp(nch,nch),STAT=my)
      IF(my /= 0) GOTO 30
      ZItmp = 0.d0

      IF(allocated(ZRtmp1)) DEALLOCATE(ZRtmp1)
      ALLOCATE(ZRtmp1(nch,nch),STAT=my)
      IF(my /= 0) GOTO 30
      ZRtmp1 = 0.d0

      IF(allocated(ZItmp1)) DEALLOCATE(ZItmp1)
      ALLOCATE(ZItmp1(nch,nch),STAT=my)
      IF(my /= 0) GOTO 30
      ZItmp1 = 0.d0

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

      ! MAX_cc_mod = nch
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

      ! write(*,*) 'Inside AllocEWmatr:',nch,npmat,numat
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

      IF(allocated(CCsmatrix)) DEALLOCATE(CCsmatrix)
      ALLOCATE(CCsmatrix(npmat),STAT=my)
      IF(my /= 0) THEN
         WRITE(8,*)  'ERROR: Insufficient memory for CCsmatrix (in TLJs)'
         WRITE(12,*) 'ERROR: Insufficient memory for CCsmatrix (in TLJs)'
         STOP 'ERROR: Insufficient memory for CCsmatrix (in TLJs)'
         RETURN
      ENDIF

      CCsmatrix%Pcn     = 1
      CCsmatrix%Jcn     = 0.d0
      CCsmatrix%nceq    = 0
      CCsmatrix%irow    = 0
      CCsmatrix%icol    = 0
      CCsmatrix%umatrix = (0.d0,0.d0)

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
      IF(allocated(CCsmatrix)) DEALLOCATE(CCsmatrix)
      RETURN
   END SUBROUTINE DelTLJs

   !----------------------------------------------------------------------------------------------------

   SUBROUTINE DelCCmatr()

      IMPLICIT NONE

      IF(allocated(Pchan)) DEALLOCATE(Pchan)
      IF(allocated(Pdiag)) DEALLOCATE(Pdiag)

      IF(INTERF==0) RETURN

      !  EW matrices
      IF(allocated(Sphase)) DEALLOCATE(Sphase)
      IF(allocated(sigma_alph_beta)) DEALLOCATE(sigma_alph_beta)
      IF(allocated(Pmatr)) DEALLOCATE(Pmatr)
      IF(allocated(Tmatr)) DEALLOCATE(Tmatr)
      IF(allocated(Umatr)) DEALLOCATE(Umatr)
      IF(allocated(Smatr)) DEALLOCATE(Smatr)
      !  Temporal matrices 
      IF(allocated(Sdiag)) DEALLOCATE(Sdiag)
      IF(allocated(PPdiag)) DEALLOCATE(PPdiag)
      IF(allocated(ZItmp)) DEALLOCATE(ZItmp)
      IF(allocated(ZRtmp1)) DEALLOCATE(ZRtmp1)
      IF(allocated(ZItmp1)) DEALLOCATE(ZItmp1)

      RETURN
   END SUBROUTINE DelCCmatr

   !----------------------------------------------------------------------------------------------------

   LOGICAL FUNCTION Open_CC_files()

      IMPLICIT NONE
      LOGICAL fexist

      Open_CC_files = .FALSE.

      !--The INQUIRE statement determines whether or not the file exists.
      !--If it does not, the program calculates new transmission coeff.
      INQUIRE (FILE = ('INCIDENT_Pchan.bin'),EXIST = fexist)
      IF (.not. fexist) RETURN ! *_Pchan.bin not found
      OPEN (126, FILE = ('INCIDENT_Pchan.bin'), STATUS = 'old',form='unformatted')

      Open_CC_files = .TRUE. ! normal return

      IF(INTERF==0) RETURN

      Open_CC_files = .FALSE.
      !--------------
      !--EW matrices
      !--------------

      !--The INQUIRE statement determines whether or not the file exists.
      !--If it does not, the program calculates new transmission coeff.
      INQUIRE (FILE = ('INCIDENT_Pmatr.bin'),EXIST = fexist)
      IF (.not. fexist) RETURN ! *_Pmatr.bin not found
      INQUIRE (FILE = ('INCIDENT_Pdiag.bin'),EXIST = fexist)
      IF (.not. fexist) RETURN ! *_Pdiag.bin not found
      INQUIRE (FILE = ('INCIDENT_Umatr.bin'),EXIST = fexist)
      IF (.not. fexist) RETURN ! *_Umatr.bin not found
      INQUIRE (FILE = ('INCIDENT_Smatr.bin'),EXIST = fexist)
      IF (.not. fexist) RETURN ! *_Smatr.bin not found
      open(125,FILE = ('INCIDENT_Pmatr.bin'), STATUS = 'old',form='unformatted')   
      open( 58,FILE = ('INCIDENT_Smatr.bin'), STATUS = 'old',form='unformatted')   
      open( 60,FILE = ('INCIDENT_Umatr.bin'), STATUS = 'old',form='unformatted')   
      open( 61,FILE = ('INCIDENT_Pdiag.bin'), STATUS = 'old',form='unformatted')   

      Open_CC_files = .TRUE. ! normal return

      RETURN
   END FUNCTION Open_CC_files

   !----------------------------------------------------------------------------------------------------

   Subroutine Close_CC_files()
      IF(DIRECT==0) RETURN
      Close(126)
      IF(INTERF==0) RETURN
      Close(125)
      Close(58)
      Close(60)
      Close(61)
   END Subroutine Close_CC_files

   !----------------------------------------------------------------------------------------------------

   LOGICAL FUNCTION Read_CC_matrices()
   IMPLICIT NONE
   DOUBLE PRECISION jc, jj, sreal, simag
   INTEGER nceq, nc1, nc2, i1, i2, nelem, ncc, nch, npmat, numat
   INTEGER nlev, nl
   CHARACTER*1 parc
   TYPE (cc_channel), POINTER :: ps_tlj
   TYPE (cc_umatrix), POINTER :: ps_pmatrix
   TYPE (cc_umatrix), POINTER :: ps_smatrix
   TYPE (cc_umatrix), POINTER :: ps_umatrix
   TYPE (cc_pdiag), POINTER :: ps_pdiag
   logical debug
   DATA debug/.false./

   Read_CC_matrices = .FALSE.

   nch = 0
   npmat = 0
   numat = 0

!==Reading Pchan
   DO ncc = 1, MAX_cc_mod
     !--jc,parc are the channel spin and parity
     !--nceq is the number of coupled equations

     !READ (126,END=10,ERR=10) jc, parc, nceq
     READ (126,END=10,ERR=10) jc, parc, nceq
     !READ (126,'(1x,f9.1,4x,a1,1x,i4)',END=10,ERR=10) jc, parc, nceq
     !write(*,*) jc,parc,nceq
     npmat = npmat + (nceq*(nceq+1))/2
     numat = numat + nceq**2
     DO i1 = 1, nceq
       nch = nch + 1
       ps_tlj => STLcc(nch)
       READ (126,END = 4,ERR = 4) nc1, sreal, nlev, nl, jj
       !READ (126,*,END = 4,ERR = 4) nc1, sreal, nlev, nl, jj
       !write(*,'(1x,I3,1x,I3,1x,F5.1,d12.6,1x,F5.1,1x,I2)') nlev,nl,sngl(jj),sngl(sreal),sngl(jc),ps_tlj%Pcn
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

   if (debug) write(*,*) 'nch,npmat,numat=',nch,npmat,numat

   CALL AllocEWmatr(nch,npmat,numat)
   Read_CC_matrices = .FALSE.

   ! TYPE(cc_umatrix), PUBLIC, ALLOCATABLE, TARGET :: CCpmatrix(:)
   !==Reading Pmatr
   nch = 0
   DO ncc = 1, MAX_cc_mod
     !--jc,parc are the channel spin and parity
     !--nceq is the number of coupled equations
     READ (125,END=12,ERR=12) jc, parc, nceq
     !READ (125,'(1x,f9.1,4x,a1,1x,i4)',END=12,ERR=12) jc, parc, nceq
     !write(*,*) jc,parc,nceq
     !--Loop over the number of coupled equations (squared)
     nelem = (nceq*(nceq+1))/2
     DO i1 = 1, nelem
       nch = nch + 1
       ps_pmatrix => CCpmatrix(nch)
       READ (125,END = 5,ERR = 5) nc1, nc2, sreal, simag
       !READ (125,'(1x,2(I4,1x),2(D15.9,1x))',END = 6,ERR = 6) nc1, nc2, sreal, simag
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
   if (debug) pause

   ! TYPE(cc_umatrix), PUBLIC, ALLOCATABLE, TARGET :: CCsmatrix(:)
   !==Reading Smatr
   nch = 0
   DO ncc = 1, MAX_cc_mod
     !--jc,parc are the channel spin and parity
     !--nceq is the number of coupled equations
     READ (58,END=13,ERR=13) jc, parc, nceq
     !READ (58,'(1x,f9.1,4x,a1,1x,i4)',END=13,ERR=13) jc, parc, nceq
     if (debug) write(*,*) jc,parc,nceq
     !--Loop over the number of coupled equations (squared)
     nelem = (nceq*(nceq+1))/2
     DO i1 = 1, nelem
       nch = nch + 1
       ps_smatrix => CCsmatrix(nch)
       READ (58,END = 6,ERR = 6) nc1, nc2, sreal, simag
       !READ (58,'(1x,2(I4,1x),2(D15.9,1x))',END = 6,ERR = 6) nc1, nc2, sreal, simag
       if (debug) write(*,'(1x,I4,1x,2(I3,1x),2(1x,d12.6))') nch,nc1,nc2,sreal,simag
       ps_smatrix%Jcn = jc
       ps_smatrix%Pcn = 1
       if(parc == '-') ps_smatrix%Pcn = -1
       ps_smatrix%nceq = nceq
       ps_smatrix%irow = nc1
       ps_smatrix%icol = nc2
       ps_smatrix%umatrix = CMPLX(sreal,simag,8)
     ENDDO
   ENDDO

13 if (debug) WRITE(*,*) 'Smatrix channels read:',nch
   if (debug) WRITE(*,*) 'Smatrix channels calc:',npmat

   !TYPE(cc_umatrix), PUBLIC, ALLOCATABLE, TARGET :: CCumatrix(:)
   !==Reading Umatr
   nch = 0
   DO ncc = 1, MAX_cc_mod
      !--jc,parc are the channel spin and parity
      !--nceq is the number of coupled equations
      READ (60,END=14,ERR=14) jc, parc, nceq
      !READ (60,'(1x,f9.1,4x,a1,1x,i4)',END=14,ERR=14) jc, parc, nceq
      if (debug) write(*,*) jc,parc,nceq
      !
      !--Loop over the number of coupled equations
      DO i1 = 1, nceq
         ! READ (60,END = 7,ERR = 7) !nc1
         DO i2 = 1, nceq
            nch = nch + 1
            ps_umatrix => CCumatrix(nch)
            READ (60,END = 7,ERR = 7) sreal,simag
            !READ (60,'(1x,D15.9,1x,D15.9)',END = 7,ERR = 7) sreal,simag
            if (debug) write(*,'(1x,I3,1x,I3,2(1x,d12.6))') i1,i2,sreal,simag
            ps_umatrix%Jcn = jc
            ps_umatrix%Pcn = 1
            if(parc == '-') ps_umatrix%Pcn = -1
            ps_umatrix%nceq = nceq
            ps_umatrix%irow = i2
            ps_umatrix%icol = i1
            ps_umatrix%umatrix = CMPLX(sreal,simag,8)
         ENDDO
      ENDDO
   ENDDO

14 MAX_umatr = nch
   if (debug) WRITE(*,*) 'Umatrix channels read:',nch
   if (debug) WRITE(*,*) 'Umatrix channels calc:',numat
   if (debug) pause 'End of Umatrix read, press any key'

   !TYPE(cc_pdiag), PUBLIC, ALLOCATABLE, TARGET :: CCpdiag(:)
   !==Reading Pdiag
   nch = 0
   DO ncc = 1, MAX_cc_mod
     !--jc,parc are the channel spin and parity
     !--nceq is the number of coupled equations
     READ (61,END=16,ERR=16) jc, parc, nceq
     !READ (61,'(1x,f9.1,4x,a1,1x,i4)',END=16,ERR=16) jc, parc, nceq
     ! write(*,*) jc,parc,nceq
     !
     !--Loop over the number of coupled equations
     DO i1 = 1, nceq
       nch = nch + 1
       ps_pdiag => CCpdiag(nch)
       READ (61,END = 8,ERR = 8) sreal
       !READ (61,'(1x,I4,1x,D15.9)',END = 8,ERR = 8) nc1, sreal
       if (debug) write(*,*) nc1,sreal
       ps_pdiag%Jcn = jc
       ps_pdiag%Pcn = 1
       if(parc == '-') ps_pdiag%Pcn = -1
       ps_pdiag%nceq = nceq
       ps_pdiag%pdiag = sreal
     ENDDO
   ENDDO

16 if (debug) WRITE(*,*) 'Pdiag channels read:',nch
   if (debug) WRITE(*,*) 'Pdiag channels expected:',MAX_cc_mod
   if (debug) pause

Read_CC_matrices = .TRUE.

RETURN

4 WRITE(8,*)' WARNING: Problem reading Pchan matrix'
STOP ' WARNING: Problem reading Pchan matrix'
5 WRITE(8,*)' WARNING: Problem reading EW Pmatrix'
STOP ' WARNING: Problem reading EW Pmatrix'
6 WRITE(8,*)' WARNING: Problem reading EW Smatr matrix'
STOP ' WARNING: Problem reading EW Smatrix'
7 WRITE(8,*)' WARNING: Problem reading EW Umatr matrix'
STOP ' WARNING: Problem reading EW Umatrix'
8 WRITE(8,*)' WARNING: Problem reading EW Pdiag matrix'
STOP ' WARNING: Problem reading EW Pdiag'

END FUNCTION Read_CC_matrices

SUBROUTINE PREPARE_CCmatr(Jcn, pcn, ncc, nmaxp, nmaxu, ndim)
   IMPLICIT NONE
   REAL*8 Jcn
   INTEGER pcn, ndim, ncc, nccu, nmaxp, nmaxu, i1, i2, irow, icol
   LOGICAL debug
   DATA debug/.false./
   ncc  = 0
   ndim = 0
   nmaxp = 0
   nmaxu = 0
   
   if (debug) write(*,*) 'Pdiag, MAX_cc_mod:',MAX_cc_mod

   DO i2 = 1, MAX_cc_mod
      ndim  = 0
      nmaxp = 0
      if(NINT(2*STLcc(i2)%Jcn) == NINT(2*Jcn) .and. STLcc(i2)%Pcn == Pcn) THEN
         if(ncc.eq.0) ncc=i2

         ndim = STLcc(i2)%nceq

         CALL AllocCCmatr(ndim)

         ! reading diagonal elements p_{alpha}
         nmaxp = i2 + ndim - 1
         if (debug) write(*,*) 'J,Pi,nceq=',sngl(STLcc(i2)%Jcn), STLcc(i2)%Pcn, STLcc(i2)%nceq, nmaxp
         IF (INTerf==0) THEN
            do i1 = i2, nmaxp
               if (debug) WRITE (*,*) i1, i1 - i2 +1, STLcc(i1)%tlj
               Pdiag(i1 - i2 +1) = STLcc(i1)%tlj
            enddo
         ELSE
            do i1 = i2, nmaxp
               if (debug) WRITE (*,*) i1, i1 - i2 +1, CCpdiag(i1)%pdiag
               Pdiag(i1 - i2 +1) = CCpdiag(i1)%pdiag
            enddo
         END IF

         EXIT
      endif
   ENDDO
   if (debug) write(*,*) ' Prepare_CC: Pdiag', sngl(Jcn), pcn, ncc, nmaxp
   
   IF(INTerf==0) RETURN
       
   ! if(NINT(2*CCpmatrix(nccu)%Jcn) == 1 .and. CCpmatrix(nccu)%Pcn == +1) debug = .TRUE.
   if (debug) write(*,*) 'Umatr, MAX_umatr: ',MAX_umatr

   DO nccu = 1, MAX_umatr
      ndim =  0
      nmaxu = 0
      if(NINT(2*CCumatrix(nccu)%Jcn) /= NINT(2*Jcn) .or. CCumatrix(nccu)%Pcn /= Pcn) cycle ! Jpi = 3/2+

      ndim = CCumatrix(nccu)%nceq
      nmaxu = nccu + ndim*ndim - 1                                                                           

      if (debug) write(*,*) 'J,Pi,nceq=',sngl(CCumatrix(nccu)%Jcn), CCumatrix(nccu)%Pcn, CCumatrix(nccu)%nceq
      if (debug) write(*,*) 'ndim,ndim^2,maxu=',ndim,ndim*ndim,nmaxu

      do i1 = nccu, nmaxu
         if (debug) WRITE (*,*) CCumatrix(i1)%irow, CCumatrix(i1)%icol, CCumatrix(i1)%umatrix
         Umatr(CCumatrix(i1)%irow,CCumatrix(i1)%icol) = CCumatrix(i1)%umatrix
      enddo
      if (debug) write(*,*) ' Prepare_CC: Umatr', sngl(Jcn), pcn, nccu, nmaxu

      if (debug) pause
      EXIT

   ENDDO

   ! debug = .false.

   if (debug) write(*,*) 'Pmatr, MAX_pumatr: ',MAX_umatr
   DO nccu = 1, MAX_pmatr
      ndim =  0
      nmaxu = 0
      if(NINT(2*CCpmatrix(nccu)%Jcn) /= NINT(2*Jcn) .or. CCpmatrix(nccu)%Pcn /= Pcn) cycle ! Jpi = 3/2+

      ndim = CCpmatrix(nccu)%nceq
      nmaxu = nccu + (ndim*(ndim+1))/2 - 1                                                                           

      if (debug) write(*,*) 'J,Pi,nceq=',sngl(CCpmatrix(nccu)%Jcn), CCpmatrix(nccu)%Pcn, CCpmatrix(nccu)%nceq
      if (debug) write(*,*) 'ndim,nmaxu=',ndim,nmaxu

      do i1 = nccu, nmaxu
	     icol = CCpmatrix(i1)%irow
	 	 irow = CCpmatrix(i1)%icol
         if(irow == icol) then
           Pmatr(irow,icol) =  CCpmatrix(i1)%umatrix
         else
           Pmatr(irow,icol) =  CCpmatrix(i1)%umatrix 
           Pmatr(icol,irow) =  CONJG(Pmatr(irow,icol))  ! Pmatrix is Hermitian
         endif
      enddo
            
	  if (debug) then
        WRITE (*,*) 'preparing Pmatrix...'
	    do irow = 1, ndim
	      do icol = 1, ndim
            WRITE (*,*) irow, icol, Pmatr(irow,icol) 
	      enddo
        enddo
        pause
      endif

	  EXIT

	ENDDO
    if (debug) write(*,*) ' Prepare_CC: Pmatr', sngl(Jcn), pcn, nccu, nmaxu
	if (debug) pause

    if (debug) write(*,*) 'Smatr, MAX_smatr: ',MAX_pmatr

    DO nccu = 1, MAX_pmatr
      ndim =  0
      nmaxu = 0
      if(NINT(2*CCsmatrix(nccu)%Jcn) /= NINT(2*Jcn) .or. CCsmatrix(nccu)%Pcn /= Pcn) cycle ! Jpi = 3/2+

      ndim = CCsmatrix(nccu)%nceq
      nmaxu = nccu + (ndim*(ndim+1))/2 - 1                                                                           

      if (debug) write(*,*) 'J,Pi,nceq=',sngl(CCsmatrix(nccu)%Jcn), CCsmatrix(nccu)%Pcn, CCsmatrix(nccu)%nceq
      if (debug) write(*,*) 'ndim,nmaxu=',ndim,nmaxu

      do i1 = nccu, nmaxu
	     icol = CCsmatrix(i1)%irow
		 irow = CCsmatrix(i1)%icol
         if(irow == icol) then
           Smatr(irow,icol) =  CCsmatrix(i1)%umatrix
         else
           Smatr(irow,icol) =  CCSmatrix(i1)%umatrix 
           Smatr(icol,irow) =  Smatr(irow,icol)      ! Smatrix is symmetric !!!
         endif
      enddo
      
	  if (debug) then
        WRITE (*,*) 'preparing Smatrix...'
	    do irow = 1, ndim
	      do icol = 1, ndim
            WRITE (*,*) irow, icol, Smatr(irow,icol) 
	      enddo
        enddo
        pause
      endif

	  EXIT

    ENDDO
    if (debug) write(*,*) ' Prepare_CC: Smatr', sngl(Jcn), pcn, nccu, nmaxu
    if (debug) pause


END SUBROUTINE PREPARE_CCmatr

END MODULE TLJs


