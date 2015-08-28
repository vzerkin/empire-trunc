   MODULE width_fluct

   USE angular_momentum

   IMPLICIT NONE

   include 'dimension.h'
   include 'global.h'

   PRIVATE

! $Rev: 4456 $
! $Author: rcapote $
! $Date: 2015-08-28 16:58:23 +0200 (Fr, 28 Aug 2015) $
!

   TYPE channel
     INTEGER*4 l         ! ejectile l
     REAL*8 j            ! ejectile j
     REAL*8 t            ! ejectile Tlj
     REAL*8 ti1          ! temporary Tlj used for iteration
     REAL*8 ti2          ! temporary Tlj used for iteration
     REAL*8 rho          ! final level density for this channel
     REAL*8 eef          ! elastic enhancement factor
     INTEGER*4 nejc      ! ejectile index (nejc)
     INTEGER*4 kres      ! populated energy bin (negative for discrete levels, g.s. -1,...)
     REAL*8 xjrs         ! spin of the populated state
     INTEGER*4 jres      ! spin index of the populated state
     INTEGER*4 pres      ! parity index of the populated state
   END TYPE channel

   TYPE cc_channel
     INTEGER*4 lev       ! number of the collective level (in the collective level file)
     INTEGER*4 l         ! orbital angular momentum l
     REAL*8 j            ! channel spin (j in T_lj)
     REAL*8 tlj          ! Tlj value
     REAL*8 Jcn          ! CN spin to which cc-channel couples
   END TYPE cc_channel

   TYPE numchnl
      INTEGER*4 neut     ! number of neutron channels, i.e., number of neutron entries in the 'channel' type
      INTEGER*4 part     ! number of particle channels, i.e., number of particle entries in the 'channel' type
      INTEGER*4 elal     ! position of the first (low) elastic channel
      INTEGER*4 elah     ! position of the last elastic channel; elastics are embedded in particle channels elal<=elah<=part
      INTEGER*4 fiss     ! effective number of fission channels
      INTEGER*4 gamm     ! effective number of gamma channels
   END TYPE numchnl

   TYPE fusion
     INTEGER*4 nout      ! position of the corresponding outgoing channel in outchnl
     INTEGER*4 l         ! projectile l
     REAL*8 j            ! projectile j
     REAL*8 t            ! projectile Tlj
     REAL*8 sig          ! absorption x-section for this channel
   END TYPE fusion


   INTEGER*4, PARAMETER :: ndhrtw1 = 10000
   INTEGER*4, PARAMETER :: ndhrtw2 = 30

   REAL*8 :: H_Sumtl      ! Sum of strong Tlj
   REAL*8 :: H_Sumtls     ! Sum of strong Tlj**2
   REAL*8 :: H_Sweak      ! Sum of weak Tlj
   REAL*8 :: H_Sweaks     ! Sum of weak Tlj**2
   REAL*8 :: sumg         ! Sum of gamma channels
   REAL*8 :: H_Tav        ! Avarage strong Tlj
   REAL*8 :: H_Tthr       ! Thershold for Tlj to be considered strong
   REAL*8 :: TFIs         ! Sum of fission transmission coefficients
   REAL*8 :: TGam         ! Sum of gamma transmission coefficients
   INTEGER*4 :: NCH       ! Number of strong channels (Tlj's)
   INTEGER*4 :: NSCh      ! Number of strong  Tlj processed by VT routine, i.e. poistion in H_Tl matrix

   INTEGER*4, ALLOCATABLE :: MEMel(:,:)
   REAL*8, ALLOCATABLE :: H_Tl(:,:)                      ! strong transmission coefficients LIKELY TO GET RID OFF!!!
   REAL*8, ALLOCATABLE :: H_Abs(:,:)
   TYPE(channel), ALLOCATABLE, TARGET :: outchnl(:)      ! outgoing channels
   TYPE(fusion),  ALLOCATABLE, TARGET :: inchnl(:)       ! fusion channels
   TYPE(cc_channel), ALLOCATABLE, TARGET :: STLj(:)      ! coupled channels for inelastic calculations (including E-W transformation)
   TYPE(numchnl) :: num                                  ! number of particular channels
   REAL*8 :: save_WFC1(20)                               ! stores central part of the Moldauer integral

   COMPLEX*16, ALLOCATABLE :: Umatr(:,:),Umatr_T(:,:),Smatr(:,:),Pmatr(:,:),Pdiag(:,:) ! EW matrices 

   PUBLIC HRTW, Moldauer

   CONTAINS

   !----------------------------------------------------------------------------------------------------

   SUBROUTINE AllocHRTW(nd1,nd2)

   IMPLICIT NONE

   INTEGER*4, INTENT(IN), OPTIONAL :: nd1, nd2

   INTEGER my,ndch,ndfus, ndcc

   ndch = ndhrtw1
   ndcc = ndhrtw1/10
   ndfus = ndhrtw2

   IF(present(nd1)) ndch = nd1
   IF(present(nd1)) ndcc = nd1/10
   IF(present(nd2)) ndfus = nd2

   IF(allocated(MEMel)) DEALLOCATE(MEMel)
   ALLOCATE(MEMel(ndfus,3),STAT=my)
   IF(my /= 0) GOTO 10
   MEMel = 0

   IF(allocated(H_Tl)) DEALLOCATE(H_Tl)
   ALLOCATE(H_Tl(ndch,2),STAT=my)
   IF(my /= 0) GOTO 10
   H_Tl = 0.0d0

   IF(allocated(H_Abs)) DEALLOCATE(H_Abs)
   ALLOCATE(H_Abs(ndfus,3),STAT=my)
   IF(my /= 0) GOTO 10
   H_Abs = 0.0d0

   IF(allocated(outchnl)) DEALLOCATE(outchnl)
   ALLOCATE(outchnl(ndch),STAT=my)
   IF(my /= 0) GOTO 10
   outchnl%l = 0
   outchnl%j = 0.d0
   outchnl%t = 0.d0
   outchnl%ti1 = 0.d0
   outchnl%ti2 = 0.d0
   outchnl%rho = 0.d0
   outchnl%eef = 1.d0
   outchnl%nejc = 0
   outchnl%kres = 0
   outchnl%xjrs = 0.d0
   outchnl%jres = 0
   outchnl%pres = 0

   IF(allocated(STLj)) DEALLOCATE(STLj)
   ALLOCATE(STLj(ndcc),STAT=my)
   IF(my /= 0) GOTO 10
   STLj%lev = 1
   STLj%l   = 0
   STLj%j   = 0.d0
   STLj%tlj = 0.d0
   STLj%Jcn = 0.d0

   IF(allocated(inchnl)) DEALLOCATE(inchnl)
   ALLOCATE(inchnl(ndfus),STAT=my)
   IF(my /= 0) GOTO 10
   inchnl%nout = 0
   inchnl%l    = 0
   inchnl%j    = 0.d0
   inchnl%t    = 0.d0
   inchnl%sig  = 0.d0

!  EW matrices
!  COMPLEX*16, ALLOCATABLE :: Umatr(:,:),Umatr_T(:,:),Smatr(:,:),Pmatr(:,:),Pdiag(:,:) ! EW matrices 
   IF(INTERF==0) RETURN

   IF(allocated(Pmatr)) DEALLOCATE(Pmatr)
   ALLOCATE(Pmatr(2*NDLW,2*NDLW),STAT=my)
   IF(my /= 0) GOTO 20
   Pmatr = 0.0d0

   IF(allocated(Pdiag)) DEALLOCATE(Pdiag)
   ALLOCATE(Pdiag(2*NDLW,2*NDLW),STAT=my)
   IF(my /= 0) GOTO 20
   Pdiag = 0.0d0

   IF(allocated(Smatr)) DEALLOCATE(Smatr)
   ALLOCATE(Smatr(2*NDLW,2*NDLW),STAT=my)
   IF(my /= 0) GOTO 20
   Smatr = 0.0d0

   IF(allocated(Umatr)) DEALLOCATE(Umatr)
   ALLOCATE(Umatr(2*NDLW,2*NDLW),STAT=my)
   IF(my /= 0) GOTO 20
   Umatr = 0.0d0

   IF(allocated(Umatr_T)) DEALLOCATE(Umatr_T)
   ALLOCATE(Umatr_T(2*NDLW,2*NDLW),STAT=my)
   IF(my /= 0) GOTO 20
   Umatr_T = 0.0d0

   RETURN

10 WRITE(8,*)  'ERROR: Insufficient memory for HRTW'
   WRITE(12,*) 'ERROR: Insufficient memory for HRTW'
   STOP 'ERROR: Insufficient memory for HRTW'
   RETURN
20 WRITE(8,*)  'ERROR: Insufficient memory for EW matrices in HRTW'
   WRITE(12,*) 'ERROR: Insufficient memory for EW matrices in HRTW'
   STOP 'ERROR: Insufficient memory for EW matrices in HRTW'

   END SUBROUTINE AllocHRTW

   !----------------------------------------------------------------------------------------------------

   SUBROUTINE DelHRTW()

   IMPLICIT NONE

   IF(allocated(MEMel))   DEALLOCATE(MEMel)
   IF(allocated(H_Tl))    DEALLOCATE(H_Tl)
   IF(allocated(H_Abs))   DEALLOCATE(H_Abs)
   IF(allocated(outchnl)) DEALLOCATE(outchnl)
   IF(allocated(inchnl))  DEALLOCATE(inchnl)
   IF(allocated(STLj))    DEALLOCATE(STLj)

!  EW matrices
   IF(INTERF==0) RETURN

   IF(allocated(Pmatr)) DEALLOCATE(Pmatr)
   IF(allocated(Pdiag)) DEALLOCATE(Pdiag)
   IF(allocated(Smatr)) DEALLOCATE(Smatr)
   IF(allocated(Umatr)) DEALLOCATE(Umatr)
   IF(allocated(Umatr_T)) DEALLOCATE(Umatr_T)

   RETURN
   END SUBROUTINE DelHRTW

   LOGICAL FUNCTION Open_EW_files()

   IMPLICIT NONE
   LOGICAL fexist
   CHARACTER*3 ctldir
   CHARACTER*23 ctmp23
   DATA ctldir/'TL/'/

   Open_EW_files = .FALSE.

   IF(INTERF==0) RETURN
!--------------
!--EW matrices 
!--------------
   WRITE (ctmp23,'(i3.3,i3.3,1h_,i3.3,i3.3,1h_,i9.9)') INT(ZEJc(0)),INT(AEJc(0)),INT(Z(0)),INT(A(0)),INT(EINl*1000000)

!--The INQUIRE statement determines whether or not the file exists.
!--If it does not, the program calculates new transmission coeff.
   INQUIRE (FILE = (ctldir//ctmp23//'_Pmatr.txt'),EXIST = fexist)
   IF (.not. fexist) RETURN ! *_Pmatr.txt not found
   INQUIRE (FILE = (ctldir//ctmp23//'_Pdiag.txt'),EXIST = fexist)
   IF (.not. fexist) RETURN ! *_Pdiag.txt not found
   INQUIRE (FILE = (ctldir//ctmp23//'_Smatr.txt'),EXIST = fexist)
   IF (.not. fexist) RETURN ! *_Smatr.txt not found
   INQUIRE (FILE = (ctldir//ctmp23//'_Umatr.txt'),EXIST = fexist)
   IF (.not. fexist) RETURN ! *_Umatr.txt not found

   OPEN (58 ,FILE = (ctldir//ctmp23//'_Pmatr.txt'), STATUS = 'old',ERR=10)
   OPEN (59 ,FILE = (ctldir//ctmp23//'_Smatr.txt'), STATUS = 'old',ERR=10)
   OPEN (60 ,FILE = (ctldir//ctmp23//'_Umatr.txt'), STATUS = 'old',ERR=10)
   OPEN (61 ,FILE = (ctldir//ctmp23//'_Pdiag.txt'), STATUS = 'old',ERR=10)

   Open_EW_files = .TRUE. ! normal return

10 RETURN
   END FUNCTION Open_EW_files

   Subroutine Close_EW_files()
   Close(58)
   Close(59)
   Close(60)
   Close(61)
   END Subroutine Close_EW_files

   LOGICAL FUNCTION Read_EW_matrices()
   IMPLICIT NONE
   DOUBLE PRECISION jc, sreal, simag
   INTEGER nceq, nc1, nc2, i1, i2
   CHARACTER*1 parc

   Read_EW_matrices = .FALSE.

   Pmatr   = 0.d0   
   Smatr   = 0.d0   
   Pdiag   = 0.d0   
   Umatr   = 0.d0   
   Umatr_T = 0.d0   
!==Reading Pmatr
!--jc,parc are the channel spin and parity
!--nceq is the number of coupled equations
   READ (58,'(1x,f9.1,4x,a1,1x,i4)',END=5,ERR=5) jc, parc, nceq  
   write(*,'(1x,A6,1x,f9.1,4x,a1,1x,i4)') 'Pmatr:', jc,parc,nceq  
!
!--Loop over the number of coupled equations (squared)
   DO i1 = 1, nceq
     DO i2 = 1, i1
       READ (58,*,END = 5,ERR = 5) nc1, nc2, sreal, simag
       Pmatr(nc1,nc2) = CMPLX(sreal,simag,8)
       if(nc1 /= nc2) Pmatr(nc2,nc1) = Pmatr(nc1,nc2) 
     ENDDO
   ENDDO

!==Reading Smatr
!---Here the calculated files are read
!--jc,parc are the channel spin and parity
!--nceq is the number of coupled equations
   READ (59,'(1x,f9.1,4x,a1,1x,i4)',END=6,ERR=6) jc, parc, nceq  
!  write(*,*) jc,parc,nceq  
!
!--Loop over the number of coupled equations
   DO i1 = 1, nceq
     DO i2 = 1, i1
       READ (59,*,END = 6,ERR = 6) nc1, nc2, sreal, simag
       Smatr(nc1,nc2) = CMPLX(sreal,simag,8)
       if(nc1 /= nc2) Smatr(nc2,nc1) = Smatr(nc1,nc2) 
     ENDDO
   ENDDO

!==Reading Umatr
!--jc,parc are the channel spin and parity
!--nceq is the number of coupled equations
   READ (60,'(1x,f9.1,4x,a1,1x,i4)',END=7,ERR=7) jc, parc, nceq  
!  write(*,*) jc,parc,nceq  
!
!--Loop over the number of coupled equations
   DO i1 = 1, nceq
     READ (60,*,END = 7,ERR = 7) nc1
     DO i2 = 1, nceq
       READ (60,*,END = 7,ERR = 7)  sreal, simag
       Umatr(nc1,i2) = CMPLX(sreal,simag,8)
     ENDDO
   ENDDO

!  Calculating the transposed matrix
   Umatr_T = TRANSPOSE(Umatr)

!==Reading Pdiag
!--jc,parc are the channel spin and parity
!--nceq is the number of coupled equations
   READ (61,'(1x,f9.1,4x,a1,1x,i4)',END=8,ERR=8) jc, parc, nceq  
!  write(*,*) jc,parc,nceq  
!
!--Loop over the number of coupled equations
   DO i1 = 1, nceq
     READ (61,*,END = 8,ERR = 8) nc1, nc2, sreal, simag
     Pdiag(nc1,nc2) = CMPLX(sreal,simag,8)
   ENDDO

   Read_EW_matrices = .TRUE.

   RETURN
 5 WRITE(8,*)' WARNING: Problem reading EW Pmatr matrix'
   STOP' WARNING: Problem reading EW Pmatrix'
 6 WRITE(8,*)' WARNING: Problem reading EW Smatr matrix'
   STOP' WARNING: Problem reading EW Pmatrix'
 7 WRITE(8,*)' WARNING: Problem reading EW Umatr matrix'
   STOP' WARNING: Problem reading EW Pmatrix'
 8 WRITE(8,*)' WARNING: Problem reading EW Pdiag matrix'
   STOP' WARNING: Problem reading EW Pmatrix'

   END FUNCTION Read_EW_matrices

   !----------------------------------------------------------------------------------------------------

   SUBROUTINE HRTW

   !cc
   !cc   ********************************************************************
   !cc   *                                                         class:ppu*
   !cc   *                         H R T W                                  *
   !cc   *                                                                  *
   !cc   *  Calculate decay of the Compound Nucleus capture states in       *
   !cc   *  terms of the HRTW theory (width fluctuation correction).        *
   !cc   *  Uses modified routines of standard Hauser-Feshbach (DECAY,      *
   !cc   *  DECAYG, FISSION) and decomposes fusion cross section into       *
   !cc   *  partial wave components.                                        *
   !cc   *                                                                  *
   !cc   *                                                                  *
   !cc   ********************************************************************
   !cc

   IMPLICIT NONE

   REAL*8 :: elada(NDAngecis), elleg(NDAngecis)
   INTEGER neles
   COMMON /angula/elada,elleg,neles
   REAL*8 :: sumin_w, sumtt_w
   COMMON /EWcorr/ sumin_w, sumtt_w

   CHARACTER(1), PARAMETER :: cpar(2) = (/'+', '-'/)

   ! Local variables

   LOGICAL*4 relcal
   INTEGER*4 i, ip, ipar, jcn, ke, m, ndivf, nejc, nhrtw, nnuc, nnur, itmp, lleg, numch_el
   REAL*8 cnspin, fisxse, summa, sumfis, sumg, sumtg, tgexper, xnor, elcor, xjc
   REAL*8 j, Ia, xjr, ja, jb, la, lb, xleg, tmp
   REAL*8 xmas_npro, xmas_ntrg, el, ecms, ak2
   REAL*8 d0c
   REAL*8 sumfism(nfmod) ! , cel_da(NDAngecis), GET_DDXS
   REAL*8 :: sumin_s, sumtt_s, stmp, ewcor

   TYPE (channel), POINTER :: out
   TYPE (fusion),  POINTER :: in

   COMPLEX*16 cres(2*NDLW,2*NDLW)

   CALL AllocHRTW()    !allocate HRTW matrices

   IF(INTERF.GT.0) then
     IF(.NOT.Open_EW_files()   ) WRITE(*,*) 'ERROR opening EW matrices' 
     IF(.NOT.Read_EW_matrices()) WRITE(*,*) 'ERROR reading EW matrices'  
 
     write (*,'(1x,A7,2(1x,i3),1x,d12.6,1x,d12.6)') 'Umatr  =',2,1,DREAL(Umatr(2,1)),DIMAG(Umatr(2,1))
     write (*,'(1x,A7,2(1x,i3),1x,d12.6,1x,d12.6)') 'Umatr_T=',1,2,DREAL(Umatr_T(1,2)),DIMAG(Umatr_T(1,2))
     write (*,'(1x,A7,2(1x,i3),1x,d12.6,1x,d12.6)') 'Pmatr  =',2,1,DREAL(Pmatr(2,1)),DIMAG(Pmatr(2,1))
     write (*,'(1x,A7,2(1x,i3),1x,d12.6,1x,d12.6)') 'Pmatr  =',1,2,DREAL(Pmatr(1,2)),DIMAG(Pmatr(1,2))
     write (*,'(1x,A7,2(1x,i3),1x,d12.6,1x,d12.6)') 'Pdiag  =',2,2,DREAL(Pdiag(2,2)),DIMAG(Pdiag(2,2))

	  cres = Umatr*Pmatr*Umatr_T
     write (*,'(1x,A7,2(1x,i3),1x,d12.6,1x,d12.6)') 'Sdiag  =',2,1,DREAL(cres(2,1)),DIMAG(cres(2,1))
     write (*,'(1x,A7,2(1x,i3),1x,d12.6,1x,d12.6)') 'Sdiag  =',2,2,DREAL(cres(2,2)),DIMAG(cres(2,2))

     CALL Close_EW_files()
   ENDIF

   H_Tthr = 1.0D-6     !threshold for considering channel to be a 'strong' one
   nnuc = 1            !set CN nucleus
   csfis = 0.D0
   SUMfis = 0.D0
   gcasc = 1.0         !ensure full gamma cascade when HRTW
   ke = NEX(nnuc)


   ! Initialize variables and print heading for normalizing g-strength function

   d0c = 0.D0
   sumtg = 0.D0
   tgexper = 0.D0
   IF(.NOT. benchm) THEN
       WRITE(8,'(1x,''Renormalization of gamma-ray strength function'')')
       WRITE(8,'(1x,''-------------------------------------------------------------'')')
   ENDIF
   IF(first_ein .AND. (einl>1.D0) ) THEN
       WRITE(8,'(1x,'' WARNING: First incident energy Einc must be < 1MeV for Do and Gg calculations and'')')
       WRITE(8,'(1x,'' WARNING: for the renormalization of gamma-ray strength function'')')
   ENDIF

   ! xmas_npro = EJMass(NPRoject)
   xmas_npro = EJMass(0)
   xmas_ntrg = AMAss(0)

   el = EINl
   relcal = .FALSE.
   !IF(IRElat(NPRoject,0)>0 .OR. RELkin) relcal = .TRUE.
   IF(IRElat(0,0)>0 .OR. RELkin) relcal = .TRUE.

   IF (AEJc(0).EQ.0.0D0) THEN  

     xmas_npro = 0.d0

     relcal = .TRUE.

   ENDIF



   CALL kinema(el,ecms,xmas_npro,xmas_ntrg,ak2,1,relcal)


   ! write(*,*) 'HRTW=',10.D0*PI/ak2,el,IRElat(0,0),RELKIN,relcal

   coef = 10.D0*PI/ak2/(2.D0*XJLv(LEVtarg,0) + 1.d0)/(2.D0*SEJc(0) + 1.d0)

   ! start CN nucleus decay
   DO ipar = 1, 2                                       ! do loop over decaying nucleus parity
       ip = 1 - 2*abs(mod(ipar+1,2))                    ! actual parity of the state (+1 or -1)
       DO jcn = 1, nlw                                  ! do loop over decaying nucleus spin
          xjc = float(jcn) + HIS(nnuc)
          IF(POP(ke,jcn,ipar,nnuc)<=1.0d-15) CYCLE      ! skip if absorption cross section negligible
          ! write(8,*) ' '
          ! write(8,*) 'CN Jpi=',xjc*ip
          nhrtw = 0
          DENhf = 0.D0
          NSCh = 0
          NCH = 0
          outchnl%l = 0
          outchnl%j = 0.d0
          outchnl%t = 0.d0
          outchnl%ti1 = 0.d0
          outchnl%ti2 = 0.d0
          outchnl%rho = 0.d0
          outchnl%eef = 1.d0
          outchnl%nejc = 0
          outchnl%kres = 0
          outchnl%xjrs = 0.d0
          outchnl%jres = 0
          outchnl%pres = 0
          inchnl%nout = 0
          inchnl%l = 0
          inchnl%j = 0.d0
          inchnl%t = 0.d0
          inchnl%sig = 0.d0
          num%neut = 0
          num%part = 0
          num%elal = 0
          num%elah = 0
          num%fiss = 0
          num%gamm = 0
          H_Sumtls = 0.D0
          H_Sumtl = 0.D0
          H_Tav = 0.D0
          H_Sweak = 0.D0
          H_Tl = 0.D0
          IF(gdrdyn==1.0D0) CALL ULMDYN(nnuc,jcn,EX(ke,nnuc)) ! prepare GDR parameters (if spin dependent GDR selected)

          ! particle decay

          DO nejc = 1, nejcm                            !do loop over ejectiles
             IF(NREs(nejc)<0) CYCLE
             nnur = NREs(nejc)
             summa = HRTW_DECAY(nnuc,ke,jcn,ip,nnur,nejc)
          ENDDO                                         !do loop over ejectiles  ***done***

          ! write(*,*) sumin_w,sumtt_w

          num%part = nch                                !store number of particle channel entries

          ! gamma emission is always a weak channel (one iteration)

          sumg = HRTW_DECAYG(nnuc,ke,jcn,ip)
          ! H_Sumtl = H_Sumtl + sumg
          ! H_Sweak = H_Sweak + sumg

          ! fission (may be a weak or strong channel)

          sumfis = 0.D0
          tfis = 0.D0
          ndivf = 1
          num%fiss = 0
          IF(FISsil(nnuc)) THEN
             IF(nint(FISshi(nnuc))==1) THEN
                CALL FISSION(nnuc,ke,jcn,sumfis)
             ELSE
                CALL FISCROSS(nnuc,ke,ip,jcn,sumfis,sumfism)
             ENDIF
             H_Sumtl = H_Sumtl + sumfis
             DENhf = DENhf + sumfis

             ! dividing sumfis into channels with TFIs < 0.25 each

             ndivf = int(sumfis/0.25) + 1
             tfis = sumfis/dfloat(ndivf)
             H_Sumtls = H_Sumtls + tfis**2*dfloat(ndivf)
             IF(tfis>=H_Tthr) THEN                      ! fission treated as a strong channel
                nch = nch + 1                           ! otherwise 'sumfis' it is left untouched
                num%fiss = nch                          ! store position of fission (only one entry with 'ndivf')
                outchnl(nch)%t = tfis
                outchnl(nch)%rho = dfloat(ndivf)
                outchnl(nch)%nejc = 100                 ! nejc=100 convention identifies fission
             ENDIF
          ENDIF

          IF(H_Sumtl.GT.0.0D0) H_Tav = H_Sumtls/H_Sumtl  ! average transmission coefficient (Sum(T**2)/Sum(T))

          ! gamma decay

          ! sumg = HRTW_DECAYG(nnuc,ke,jcn,ip)           ! gamma emission is always a weak channel (V=T)
          !  write(*,*)' '
          !  write(*,*)'SUMMARY OF DECAY FOR J=',xjc
          !  write(*,*)'total sum of  Tls ', H_Sumtl
          !  write(*,*)'sum of strong Tls ', H_Sumtl-H_Sweak
          !  write(*,*)'sum of weak   Tls ', H_Sweak
          !  write(*,*)'sum of weak part. ', H_Sweak-sumg
          !  write(*,*)'sum of gammas     ', sumg
          !  write(*,*)'sum fission       ', sumfis
          !  write(*,*)'sum Tl**2         ', H_Sumtls
          !  write(*,*)'# of strong Tls   ', nch
          !  write(*,*)'average Tl        ', H_Tav
          !  write(*,*)'first entry DENhf=', DENhf

          ! collecting outgoing channels completed

          ! write(*,*)'pre  AUSTER DENhf=', DENhf
          IF(LHRtw==1 .OR. LHRtw==2) CALL AUSTER(LHRtw)                  !calculate V's for the strong channels (iteration)
          DENhf = H_Sumtl                     !reset DENhf using V's instead of T's

          ! construct scratch matrix for decay of the Jcn state

          sumin_s = 0.d0
          sumtt_s = 0.d0
          ! write(*,*) ' NProject=',NPRoject,' LEVtarg=',LEVtarg
          
          DO i = 1, num%part                          !scan strong particle channels (note: weak channels are already in SCRt)
             out => outchnl(i)
             IF(out%kres>0) THEN                ! continuum channels
                SCRt(out%kres,out%jres,out%pres,out%nejc) = SCRt(out%kres,out%jres,out%pres,out%nejc) + out%t*out%rho/de
             ELSE IF(out%kres<0) THEN           ! discrete level channels
                IF( (out%nejc.EQ.NPRoject) .AND. (-out%kres.NE.LEVtarg) ) THEN
                   stmp = out%t*out%rho 
                   sumin_s = sumin_s + stmp * CINRED(-out%kres) 
                   sumtt_s = sumtt_s + stmp 
                   SCRtl(-out%kres,out%nejc) = SCRtl(-out%kres,out%nejc) +  stmp * CINRED(-out%kres)
                   ! write(*,*) 'ilev=',-out%kres,' nejc=',out%nejc,' CINRED(ilev)=',sngl(CINRED(-out%kres))
                   ! write(*,*) sumin_s,sumtt_s,' strong'
                ELSE
                   SCRtl(-out%kres,out%nejc) = SCRtl(-out%kres,out%nejc) + out%t*out%rho
                ENDIF
             ENDIF
          ENDDO
          ewcor = (sumtt_s - sumin_s +  sumtt_w - sumin_w)  
          !write(*,*) 'EWCORR=', ewcor


          ! Correcting the elastic cross section for inelastic enhancement CINRED

          numch_el = max(num%elah - num%elal + 1 , 1 ) 

          DO i = num%elal, num%elah  ! do loop over elastic channels

             out => outchnl(i)

             SCRtl(-out%kres,out%nejc) = SCRtl(-out%kres,out%nejc) + ewcor/numch_el

             !write(*,*) 'ilev=',-out%kres,' nejc=',out%nejc,' ewcor=',ewcor

          ENDDO


          IF(num%fiss>0) sumfis = outchnl(num%fiss)%t*outchnl(num%fiss)%rho  !redefining sumfis to account for the HRTW T=>V transition

          ! DENhf = 0.0d0                             !test that SCRt+SCRtl sum to the same DENhf
          ! DENhf = SUM(SCRt)*de + SUM(SCRtl) + sumfis
          ! DENhf = DENhf - 0.5*SUM(SCRt(1,:,:,:))*de   !correct for the edge effect in trapezoidal integration
          ! write(*,*)'DENhf calculated as integral of SCRt & SCRtl', DENhf    !should be the same as after AUSTER

          DO i = num%elal, num%elah                   ! do loop over elastic channels

             in => inchnl(i - num%elal + 1)           ! elastic channels for each Jcn are numbered 1,2,3,...
             out => outchnl(i)

             in%t = out%t
             in%sig = coef*in%t*(2.D0*xjc + 1.D0)*FUSred*REDmsc(jcn,ipar)  ! absorption for incoming channel
             xnor = in%sig/DENhf                      ! normalization factor
             ! write(*,*) 'Jcn, Tlj_in, Tlj_out, coef, sig ', xjc, in%t, out%t, coef, in%sig
             IF(LHRtw==1 .OR. LHRtw==2) THEN
                elcor = out%t*(out%eef - 1.D0)     ! elastic channel correction to SCRtl  (elcor=0 for HF)
                ! write(*,*) 'Elcor =', elcor, '  EEF =', out%eef
                SCRtl(-out%kres,out%nejc) = SCRtl(-out%kres,out%nejc) + elcor
             ENDIF
             ! write(*,*)'post AUSTER DENhf=', DENhf + elcor

             ! CN angular distributions (neutron (in)elastic scattering ONLY!)

             ewcor = 0.d0
             IF(.NOT.CN_isotropic) THEN
                ! accumulate Legendre coefficients
                nejc = 1
                nnur = 2
                Ia = XJLv(LEVtarg,0)                  !target spin
                la = in%l                             !incident neutron l
                ja = in%j                    !incident neutron j
                ! write(8,*) 'Incident chnl',i, 'J_pi ',xjc*ip,' number of outgoing n channels', num%neut
                DO j = 1, num%neut                    !do loop over neutron channels only
                   ! write(8,*) 'Elastic channel #',j, ' abs = ',in%sig, ' xnor = ', xnor
                   ! write(8,*) '                                  leg      BB               Jcn   &
                   ! &               l_inc           j_inc                 J_res              l_out              J_out'
                   out => outchnl(j)
                   xjr = out%xjrs              !residual nucleus J
                   lb = out%l                  !outgoing neutron l
                   jb = out%j                  !outgoing neutron j
                   IF(out%kres > 0) THEN
                     PLcont_lmax(out%kres) = 2*in%l
                   ELSEIF(out%kres < 0) THEN
                     PL_lmax(-out%kres) = 2*in%l
                   ENDIF

                   IF(i/=j.AND.out%kres<=0) THEN               ! Inelastic to discrete level
                     stmp = out%t*out%rho * CINRED(-out%kres) 
                   ELSE                                        ! Elastic and continuum
                     stmp = out%t*out%rho 
                   ENDIF

                   DO lleg = 0, 2*in%l, 2    !do loop over Legendre L
                     xleg = dble(lleg)
                     tmp = Blatt(xjc,Ia,la,ja,SEJc(nejc),xjr,lb,jb,SEJc(nejc),xleg)/(2*xleg + 1.0d0)
                     ! write(8,*) ' Leg => tmp,xjc,la,ja,xjr,lb,jb,',lleg,tmp,xjc,la,ja,xjr,lb,jb,outchnl(j)%kres
                     ! if(tmp==0.D0) cycle
                     IF(dabs(tmp) < 1.d-14) CYCLE
                     tmp = tmp*xnor*stmp  !*out%t*out%rho
                     IF(i==j) tmp = tmp*out%eef

                     IF(out%kres > 0) THEN
                       PL_CNcont(lleg,out%kres) = PL_CNcont(lleg,out%kres) + tmp
                     ELSEIF(out%kres < 0) THEN
                       PL_CN(lleg,-out%kres) = PL_CN(lleg,-out%kres) + tmp
                     ENDIF

                   ENDDO

                   !IF(out%kres > 0) THEN
                   !  write (8,'(2x,A10, 5Hchan= ,I4,2x,5Hlmax= ,I4,5H cont  )') 'HRTW-comp ',out%kres, PLcont_lmax(out%kres)
                   !ELSEIF(outchnl(j)%kres < 0) THEN
                   !  write (8,'(2x,A10, 5Hchan= ,I4,2x,5Hlmax= ,I4,5H disc  )') 'HRTW-comp ',out%kres, PL_lmax(-out%kres)
                   !ENDIF
                   !write(8,*) 'PL_CNcont(lleg,1) = ', PL_CNcont(0,1), PL_CNcont(2,1), PL_CNcont(4,1), PL_CNcont(6,1), PL_CNcont(8,1)

                ENDDO
             ENDIF    !end of Legendre coeficients accumulation for Anisotropic CN

          
             CALL XSECT(nnuc,m,xnor,sumfis,sumfism,ke,ipar,jcn,fisxse)  !normalize SCRt matrices and store x-sec

             IF(LHRtw==1 .OR. LHRtw==2) THEN
                out => outchnl(i)
                SCRtl(-out%kres,out%nejc) = SCRtl(-out%kres,out%nejc) - elcor    !restore SCRtl before new elastic is calculated
             ENDIF

             IF(ewcor.NE.0.d0) THEN
                out => outchnl(i)
                !restore SCRtl before new elastic is calculated
                SCRtl(-out%kres,out%nejc) = SCRtl(-out%kres,out%nejc) - ewcor    
             ENDIF

             !write(*,*) 'ewcor=',ewcor   

          ENDDO    !end do loop over incident channels

          ! Gamma width calculation

          IF((first_ein .OR. benchm) .AND. einl<=1.D0) THEN
            cnspin = jcn - 0.5
            IF(mod(XJLv(levtarg,0)*2.,2.D+0)==1)cnspin = jcn - 1
            IF(ip==LVP(levtarg,0) .AND. ((cnspin==XJLv(levtarg,0)+0.5) .OR. (cnspin==XJLv(levtarg,0)-0.5))) THEN
              d0c = d0c + RO(ke,jcn,ipar,nnuc)
              ! write(8,*)'ke,jcn,ipar,ro',ke,jcn,ipar,RO(ke,jcn,ipar,nnuc)
              WRITE(8,'(A12,f4.1,A6,A1,A6,F7.3,A4,/,A37,d12.6)')'CN state J=', cnspin, ', Par:', cpar(ipar), ' at U=',&
                 EX(ke,nnuc), ' MeV', 'Int[Rho(U)*Tlg(U)] + Sum[Tlg(Ui)] = ', sumg
              sumtg = sumtg + sumg
            ENDIF
          ENDIF

       ENDDO       !loop over decaying nucleus spin
   ENDDO          !loop over decaying nucleus parity

   IF(d0c>0.D0) d0c = 1000.0/d0c
   IF(d0_obs==0.0D0) d0_obs = d0c    !use calculated D0 (in keV) if not measured

   IF(benchm) THEN
      WRITE(8,*)
      WRITE(8,'(1x,'' WARNING: Gamma emission width not normalized in benchmark calculations'')')
      WRITE(8,*)
   ENDIF

   itmp = iabs( NINT(1000*TUNe(0,nnuc)) - 999 )
   IF(itmp.EQ.1 .AND. (.NOT.benchm)) THEN
      WRITE(8,'(1x,'' WARNING: Gamma emission width not normalized (TUNE set to 1.000 in input)'')')
      WRITE(8,*)
   ENDIF

   IF(itmp.GT.1 .AND. (.NOT.benchm)) THEN
      WRITE (8 ,'('' WARNING: Gamma emission width from '',I3,A2,'' normalized by '',F7.3)') NINT(A(nnuc)), SYMb(nnuc), TUNe(0,nnuc)
      WRITE (12,'('' Gamma emission width from '',I3,A2,'' normalized by '',F7.3)') NINT(A(nnuc)), SYMb(nnuc), TUNe(0,nnuc)
      WRITE(8,*)
   ENDIF

   ! IF(EINl<=1.D0 .AND. (FIRst_ein .or. BENchm)) THEN
   IF(einl<=1.D0 .AND. first_ein) THEN

     IF(d0_obs>0.D0) THEN
        tgexper = 2*pi*gg_obs/d0_obs/1.E6
        WRITE(8,'(1x,''Experimental information from capture channel'')')
        WRITE(8,'(1x,A13,D12.6)')'2*pi*Gg/D0 = ', tgexper
     ENDIF

     IF(gg_unc>0.0D0) THEN
        WRITE(8,'(1x,A5,F9.3,A5,F8.3,A4)')'Gg = ', gg_obs, ' +/- ', gg_unc, ' meV'
     ELSE
        WRITE(8,'(1x,A5,F9.3,A18)')'Gg = ', gg_obs, ' meV (systematics)'
     ENDIF

     WRITE(12,'(/1x,A5,F9.3,A5,F8.3,A4)')'Gg = ', gg_obs, ' +/- ', gg_unc, ' meV'

     IF(d0_obs>0.0D0) THEN
        WRITE(8,'(1x,A5,F11.6,A5,F11.6,A4)')'D0 = ', d0_obs, ' +/- ', d0_unc, ' keV'
        WRITE(8,'(1x,A5,F11.6,A17)')'D0 = ', d0c, ' keV (calculated)'
        WRITE(12,'(1x,''D0 = '',F8.3,'' keV'')')d0_obs
     ELSE
        WRITE(8,'(1x,A5,F11.6,A17)')'D0 = ', d0c, ' keV (calculated)'
        WRITE(12,'(1x,''D0 = '',F8.3,'' keV, CALC'')')d0c
     ENDIF
     WRITE(8,*)
     WRITE(12,*)

     IF(itmp==0 .AND. (.NOT.benchm)) THEN
        IF(sumtg>0.D0 .AND. tgexper>0.D0) THEN
           tune(0,nnuc) = tgexper/sumtg
           WRITE(8,'(1x,'' WARNING: Gamma emission normalization factor is set to '',F7.3)') TUNe(0,nnuc)
           IF (first_ein) WRITE(8,'(1x,'' WARNING: The normalization is not applied to this incident energy'')')
        ELSE
           WRITE(8,'(1x,'' WARNING: Gamma emission width is not normalized to Do'')')
        ENDIF
        WRITE(8,*)
     ENDIF

     IF(itmp==1 .AND. (.NOT.benchm) .AND. (sumtg>0.D0 .AND. tgexper>0.D0) ) THEN
        WRITE(8,'(1x,'' WARNING: Gamma emission could be normalized by setting TUNE to '',F7.3,'' in input'')')  tgexper/sumtg
        WRITE(8,*)
     ENDIF

   ENDIF

   CALL DelHRTW()    !deallocate HRTW arrays

   RETURN
   END SUBROUTINE HRTW

   !----------------------------------------------------------------------------------------------------

   REAL*8 FUNCTION HRTW_DECAY(nnuc,iec,jc,ipc,nnur,nejc)
   !cc
   !cc   ********************************************************************
   !cc   *                                                         class:PPu*
   !cc   *                      H R T W  _  D E C A Y                       *
   !cc   *                (function to function version)                    *
   !cc   *                                                                  *
   !cc   * Calculates decay of a continuum state in nucleus NNUC into       *
   !cc   * continuum and discrete states of the residual nucleus NNUR       *
   !cc   * through the emission of the ejectile NEJC including width        *
   !cc   * fluctuation correction according to HRTW theory.                 *
   !cc   *                                                                  *
   !cc   *                                                                  *
   !cc   * input:NNUC - decaying nucleus index                              *
   !cc   *       IEC  - energy index of the decaying state                  *
   !cc   *       JC   - spin index of the decaying state                    *
   !cc   *       IPC  - parity of the decaying state                        *
   !cc   *       NNUR - residual nucleus index                              *
   !cc   *       NEJC - ejectile index                                      *
   !cc   *                                                                  *
   !cc   * output:      SUM of transmission coefficients over all outgoing  *
   !cc   *              channels for the requested decay                    *
   !cc   *              Apart of this standard feature it comunicates       *
   !cc   *              quantities needed for the HRTW theory through       *
   !cc   *              the HRTW common:                                    *
   !cc   *                                                                  *
   !cc   *             H_Tl(.,.) - list of strong Tl's and their number(rho)*
   !cc   *             H_Tloc(.,.) - l and ejectile for each strong Tl      *
   !cc   *             H_sumtls - sum of squared Tl's (all)                 *
   !cc   *             H_sweak - sum of weak Tl's (not squared)             *
   !cc   *             H_lch - number of strong Tl's                        *
   !cc   *             H_abs(.,.) - absorption x-sec decomposed in l's      *
   !cc   *             H_Tav - average transmission coefficient             *
   !cc   *             H_Tthr - threshold to consider Tl as strong          *
   !cc   *             MEMel  - records l and position of elastic channel   *
   !cc   *                                                                  *
   !cc   *                                                                  *
   !cc   *                                                                  *
   !cc   * calls:TLLOC                                                      *
   !cc   *                                                                  *
   !cc   *                                                                  *
   !cc   *                                                                  *
   !cc   ********************************************************************
   !cc

   IMPLICIT NONE
   ! COMMON variables
   REAL*8, DIMENSION(ndlw,3) :: ELTLJ
   REAL*8, DIMENSION(ndlw) :: ELTL
   COMMON /ELASTIC/ ELTl,ELTlj
   REAL*8 :: sumin_w, sumtt_w
   COMMON /EWcorr/ sumin_w, sumtt_w

   ! Dummy arguments

   INTEGER*4, INTENT(IN) :: ipc, jc, nejc, nnuc, nnur, iec

   ! Local variables

   REAL*8 :: eout, eoutc, frde, rho1, jmax, jmin, sumdl, tld, xjc, xj, xjr, summa
   INTEGER*4 :: i, ier, iermax, ietl, iexc, il, ip1, ipar, itlc, jr, k, kmax, kmin, nel, jndex

   TYPE (channel), POINTER :: out
   TYPE (fusion),  POINTER :: in

   summa = 0.D0

   ! clear scratch matrices

   scrtem(nejc) = H_Sumtl        !temporarily store here entry value of H_Sumtl
   SCRt(:,:,:,nejc) = 0.D0
   iexc = NEX(nnuc) - NEXr(nejc,nnuc)
   itlc = iexc - 5
   iermax = iec - iexc
   xjc = dble(jc) + HIS(nnuc)  !actual spin  of the CN state

   IF(iermax>=1) THEN

     ! decay to the continuum

     ! Relations between  TLJ(k,j) and physical transmission coefficients labeled l, s, and J
     ! l = k-1 (TLJ matrix starts with index 1 for l=0)
     ! Structure of the TLJ matrix
     ! for s=0 (the first raw only, i.e., m=1 (in the coding below m:=jndex))
     ! J = k-1 (m=1) i.e., J = k - 1 + (m-1) - s    thus J = k+m-(2+s)
     ! for s=1/2 (two rows)
     ! J = k-1-0.5 (m=1) i.e., J = k - 1 - 0.5 + (m-1) = k-2-0.5+m
     ! J = k-1+0.5 (m=2) i.e., J = k - 1 - 0.5 + (m-1) = k-2-0.5+m   thus J = k+m-2.5 = k+m-(2+s)
     ! for s=1 (three rows)
     ! J = k-1-1 (m=1) i.e., J = k - 1 - 1 + (m-1)
     ! J = k-1   (m=2) i.e., J = k - 1 - 1 + (m-1)
     ! J = k-1+1 (m=3) i.e., J = k - 1 - 1 + (m-1)   thus J = k+m-3 = k+m-(2+s)
     ! given spin first occures for the bottme row m=2s+1 and last time in the first row m=1.

     DO jr = 1, nlw                                      ! do loop over r.n. spins
        xjr = dble(jr) + HIS(nnur)                       ! actual spin of the residual nucleus state
        jmin = ABS(xjr - xjc)
        jmax = xjr + xjc
        kmin = jmin - MAXj(nejc) + (2.0 + SEJc(nejc))    ! minimum k=l+1
        kmax = jmax - 1 + (2.0 + SEJc(nejc))             ! maximum k=l+1
        kmax = MIN(ndlw, kmax)                           ! ensure we are within dimensions
        DO k = kmin, kmax                                ! do loop over l in Tlj (note that k=l+1)
           ip1 = 2 - (1 + ipc*( - 1)**(k - 1))/2         ! parity index of r.n. state populated by emission with l=k-1
           DO jndex = 1, MAXj(nejc)                      ! do loop over j-index in Tlj
              xj = k + jndex - (2.0 + SEJc(nejc))
              IF(xj<jmin .OR. xj>jmax) CYCLE
              DO ier = iermax, 1, -1
                 IF(INT(ZEJc(nejc))==0 .AND. ier==iermax) CYCLE   ! omit top bin transition for neutrons
                 ietl = iec - ier - itlc
                 tld = TLJ(ietl,k,jndex,nejc)
                 rho1 = RO(ier,jr,ip1,nnur)*de*TUNe(nejc,nnuc)
                 IF(ier==1 .AND. nint(Z(1))==nint(Z(nnur))) rho1 = rho1*DEPart(nnur)  !correct for gap above Ecut
                 H_Sumtl = H_Sumtl + tld*rho1
                 H_Sumtls = H_Sumtls + tld**2*rho1
                 IF(ier==1) THEN                         !correct for the edge bin in trapeizoidal integration
                    H_Sumtl = H_Sumtl - 0.5*tld*rho1
                    H_Sumtls = H_Sumtls - 0.5*tld**2*rho1
                 ENDIF
                 IF(tld>H_Tthr) THEN                     !store strong channels
                    nch = nch + 1
                    IF(nch>ndhrtw1) CALL HRTW_error()    !STOP - insufficent space allocation
                    out => outchnl(nch)
                    out%l = k-1
                    out%j = xj
                    out%t = tld
                    out%rho = rho1
                    out%nejc = nejc
                    out%kres = ier
                    out%xjrs = xjr
                    out%jres = jr
                    out%pres = ip1
                 ELSE                                    !weak channel (will not be iterated so can be added to SCRt)
                    SCRt(ier,jr,ip1,nejc) = SCRt(ier,jr,ip1,nejc) + tld*rho1/de
                    H_Sweak = H_Sweak + tld*rho1
                    H_Sweaks = H_Sweaks + tld**2*rho1
                    IF(ier==1) THEN                      !correct for the edge bin in trapeizoidal integration
                       H_Sweak = H_Sweak - 0.5*tld*rho1
                       H_Sweaks = H_Sweaks - 0.5*tld**2*rho1
                    ENDIF
                 ENDIF
              ENDDO      !over residual nucleus energies
           ENDDO      ! over j in Tlj
        ENDDO      ! over l in Tlj
     ENDDO      ! over residual nucleus spins
   ENDIF

   ! decay to discrete levels

   SCRtl(:,nejc) = 0.D0
   IF(IZA(nnur)==IZA(0)) memel = 0    !clear memorized elastic channels when entering new J-pi CN state
   eoutc = EX(iec,nnuc) - Q(nejc,nnuc)

   sumin_w = 0.d0
   sumtt_w = 0.d0
   DO i = 1, NLV(nnur)             ! do loop over inelastic levels, elastic done after the loop
     IF(IZA(nnur)==IZA(0) .AND. i==levtarg) CYCLE     !skip if elastic
     IF(IZA(nnur)==IZA(0) .AND. (ICOllev(i)>0 .AND. ICOllev(i)<=LEVcc)) CYCLE   !skip coupled levels
     eout = eoutc - ELV(i,nnur)
     IF(eout<0.0D0) EXIT
     sumdl = 0.D0
     CALL TLLOC(nnur,nejc,eout,il,frde)               !find 'il' postion of the Tlj in the ETL matrix and relative mismatch 'frde'
     jmin = abs(XJLv(i,nnur) - xjc)
     jmax = XJLv(i,nnur) + xjc
     kmin = jmin - MAXj(nejc) + (2.0 + SEJc(nejc))    !minimum k=l+1
     kmax = jmax - 1  + (2.0 + SEJc(nejc))            !maximum k=l+1
     kmax = MIN(ndlw, kmax)                           !ensure we are within dimensions
     DO k = kmin, kmax                                !do loop over l in Tlj (note that real l is k-1)
        ipar = 1 + LVP(i,nnur)*ipc*( - 1)**(k - 1)    !check parity (1 if conserved, 0 if violated)
        IF(ipar==0) CYCLE
        DO jndex = 1, MAXj(nejc)                      !do loop over j-index in Tlj
           xj = k + jndex - (2.0 + SEJc(nejc))
           IF(xj<jmin .OR. xj>jmax) CYCLE
           rho1 = 1.d0                                !reuse level density variable
           ! IF(IZA(nnur)==IZA(0)) rho1 = CINred(i)   !if inelastic - apply respective scaling
           tld = TLJ(il,k,jndex,nejc) + frde*(TLJ(il + 1,k,jndex,nejc) - TLJ(il,k,jndex,nejc))   !interpolate Tlj
           IF(tld<1.0d-15) CYCLE                      !ignore very small channels
           H_Sumtl = H_Sumtl + tld*rho1
           H_Sumtls = H_Sumtls + tld**2*rho1
           sumdl = sumdl + tld*rho1  !think there is no need for it
           IF(tld>H_Tthr) THEN
              nch = nch + 1                              !we've got non-zero channel
              IF(nch>ndhrtw1) CALL HRTW_error()          !STOP - insiufficent space allocation
              out => outchnl(nch)
              out%l = k-1
              out%j = xj
              out%t = tld
              out%rho = rho1
              ! out%sig = 0.d0
              out%nejc = nejc
              out%kres = -i                         !minus indicates channel leading to a discrete level 'i'
              out%xjrs = XJLv(i,nnur)
              out%pres = LVP(i,nnur)
           ELSEIF(tld>1.0d-15) THEN                       !weak channel (will not be iterated so can be stored in SCRtl)
              SCRtl(i,nejc) = SCRtl(i,nejc) + tld*rho1 * CINred(i) 
              sumin_w = sumin_w + tld*rho1 * CINRED(i) 
              sumtt_w = sumtt_w + tld*rho1 
              H_Sweak = H_Sweak + tld*rho1
              H_Sweaks = H_Sweaks + tld**2*rho1
           ENDIF
        ENDDO     !do loop over 'jndex'    --------- done --------------------
     ENDDO    !do loop over 'l'            --------- done --------------------
   ENDDO    ! do loop over inelastic levels --------- done --------------------

!  decay to coupled levels
   IF(IZA(nnur)==IZA(0)) THEN
   DO i = 1, NLV(nnur)             ! do loop over inelastic levels, elastic done after the loop
     IF(i==levtarg) CYCLE     !skip if elastic
     IF(ICOllev(i)==0 .OR. ICOllev(i)>LEVcc) CYCLE   !skip DWBA coupled levels
     eout = eoutc - ELV(i,nnur)
     IF(eout<0.0D0) EXIT
     sumdl = 0.D0
     CALL TLLOC(nnur,nejc,eout,il,frde)               !find 'il' postion of the Tlj in the ETL matrix and relative mismatch 'frde'
     jmin = abs(XJLv(i,nnur) - xjc)
     jmax = XJLv(i,nnur) + xjc
     kmin = jmin - MAXj(nejc) + (2.0 + SEJc(nejc))    !minimum k=l+1
     kmax = jmax - 1  + (2.0 + SEJc(nejc))            !maximum k=l+1
     kmax = MIN(ndlw, kmax)                           !ensure we are within dimensions
     DO k = kmin, kmax                                !do loop over l in Tlj (note that real l is k-1)
        ipar = 1 + LVP(i,nnur)*ipc*( - 1)**(k - 1)    !check parity (1 if conserved, 0 if violated)
        IF(ipar==0) CYCLE
        DO jndex = 1, MAXj(nejc)                      !do loop over j-index in Tlj
           xj = k + jndex - (2.0 + SEJc(nejc))
           IF(xj<jmin .OR. xj>jmax) CYCLE
           rho1 = 1.d0                                !reuse level density variable
           ! IF(IZA(nnur)==IZA(0)) rho1 = CINred(i)   !if inelastic - apply respective scaling
           tld = TLJ(il,k,jndex,nejc) + frde*(TLJ(il + 1,k,jndex,nejc) - TLJ(il,k,jndex,nejc))   !interpolate Tlj
           IF(tld<1.0d-15) CYCLE                      !ignore very small channels
           H_Sumtl = H_Sumtl + tld*rho1
           H_Sumtls = H_Sumtls + tld**2*rho1
           sumdl = sumdl + tld*rho1  !think there is no need for it
              nch = nch + 1                              !we've got non-zero channel
              IF(nch>ndhrtw1) CALL HRTW_error()          !STOP - insiufficent space allocation
              out => outchnl(nch)
              out%l = k-1
              out%j = xj
              out%t = tld
              out%rho = rho1
              out%nejc = nejc
              out%kres = -i                         !minus indicates channel leading to a discrete level 'i'
              out%xjrs = XJLv(i,nnur)
              out%pres = LVP(i,nnur)
           WRITE(8,'(3x,A,2x,2(f5.1,1x),i2,1x,f7.3,1x,i6)')'xjc, xj, ipar, ELV(i,nnur), nch', xjc, xj, ipar, ELV(i,nnur), nch
        ENDDO     !do loop over 'jndex'    --------- done --------------------
     ENDDO    !do loop over 'l'            --------- done --------------------
   ENDDO    ! do loop over inelastic levels --------- done --------------------


   ! elastic channel

!   IF(IZA(nnur)==IZA(0)) THEN
     sumdl = 0.D0
     i = levtarg
     eout = eoutc - ELV(i,nnur)
     IF(eout<0.0D0) GOTO 10
     jmin = abs(XJLv(i,nnur) - xjc)
     jmax = XJLv(i,nnur) + xjc
     kmin = jmin - MAXj(nejc) + (2.0 + SEJc(nejc))    !minimum k=l+1
     kmax = jmax - 1 + (2.0 + SEJc(nejc))             !maximum k=l+1
     kmax = MIN(ndlw, kmax)                           !ensure we are within dimensions
     DO k = kmin, kmax                                !do loop over k in Tlj (note that real l is k-1)
        !         ipar = 1 + LVP(i,nnur)*ipc*( - 1)**(k - 1)    !check parity
        !         ipar = PAR(ipc,LVP(LEVtarg,nnur),k - 1)
        ipar = (1.d0 - (-1.d0)*ipc*LVP(i,nnur)*(-1)**(k-1))/2.d0
        IF(ipar==0) CYCLE
        DO jndex = 1, MAXj(nejc)                      !do loop over j-index in Tlj
           xj = k + jndex - (2.0 + SEJc(nejc))
           IF(xj<jmin .OR. xj>jmax) CYCLE
           tld = ELTLJ(k,jndex)                       !no IF - all elastic channels treated as 'strong'
           nch = nch + 1
           IF(nch>ndhrtw1) CALL HRTW_error()          !STOP - insufficient space allocation
           IF(num%elal == 0) THEN
              num%elal = nch                          !memorize position of the first elastic in the 'outchnl' matrix
              num%elah = nch                          !set it also as the last one in case there are no more
           ENDIF
           IF(nch > num%elah) num%elah = nch          !if another elastic augment position of last elastic channel
           rho1 = CELred
           out => outchnl(nch)
           out%l = k-1
           out%j = xj
           out%t = tld
           out%rho = rho1
           ! out%sig = 0.d0
           out%nejc = nejc
           out%kres = -i    !minus indicates that this is a channel leading to a discrete level 'i'
           out%xjrs = XJLv(i,nnur)
           out%pres = LVP(i,nnur)

           nel = nch - num%elal + 1         !setting correspondence between 'nch' and elastic numbering 'nel'
           in => inchnl(nel)
           in%nout = nch                    !setting incident channel
           in%l = k-1                       !setting incident channel
           in%j = xj                        !          "
           in%t = tld                       !          "
           h_sumtl = h_sumtl + tld*rho1
           h_sumtls = h_sumtls + tld**2*rho1
           sumdl = sumdl + tld*rho1  !think there is no need for it
        ENDDO                 ! do loop over jndex --- done -------
     ENDDO                    ! loop over 'l' ------ done ---------
     summa = summa + sumdl
   ENDIF !end of elastic
   ! decay to discrete levels --------- done --------------------

10 summa = H_Sumtl - scrtem(nejc)     !we may NOT NEED IT
   scrtem(nejc) = summa               !we may NOT NEED IT
   DENhf = DENhf + summa
   IF(nejc==1) num%neut = nch         !store number of neutron-out channels
   ! decay to the continuum and discrete levels ------ done -----------------------------

   hrtw_decay = summa

   RETURN
   END FUNCTION hrtw_decay

   !----------------------------------------------------------------------------------------------------

   REAL*8 FUNCTION HRTW_DECAYG(nnuc,iec,jc,ipc)
   !
   !********************************************************************
   !*                                                         class:PPu*
   !*                         D E C A Y G                              *
   !*                (function to function version)                    *
   !*                                                                  *
   !* Calculates gamma decay of a continuum state in nucleus NNUC into *
   !* continuum and discrete states in the same nucleus NNUC           *
   !*                                                                  *
   !*                                                                  *
   !* input:NNUC - decaying nucleus index                              *
   !*       IEC  - energy index of the decaying state                  *
   !*       JC   - spin index of the decaying state                    *
   !*       IPC  - parity of the decaying state (+1 or -1)             *
   !*                                                                  *
   !*                                                                  *
   !* output:      Sum of transmission coefficients over all           *
   !*              gamma channels.                                     *
   !*              Apart of this standard feature it comunicates       *
   !*              quantities needed for the HRTW model through        *
   !*              the HRTW_mod module.  Note that gamma channels      *
   !*              contribute to the sum of weak channels but their    *
   !*              transmission coefficients are assumed to remain     *
   !*              constant when HRTW is applied.                      *
   !*                                                                  *
   !* calls:none                                                       *
   !*                                                                  *
   !*                                                                  *
   !*                                                                  *
   !********************************************************************

   IMPLICIT NONE

   ! Dummy arguments

   INTEGER*4, INTENT(IN)  :: iec, ipc, jc, nnuc

   ! Local variables

   REAL*8 :: cee, cme, eg, ha, hscrtl, hsumtls, scrtneg
   REAL*8 :: e1, e2, xm1, scrtpos, xjc, xjr, summa
   INTEGER*4 i, ier, ineg, iodd, ipar, ipos, j, jmax, jmin, jr, lamb, lambmax, lambmin, kmax, kmin
   REAL*8, DIMENSION(10) :: xle, xlm

   ! MAXmult - maximal gamma-ray multipolarity
   ! maximal value (.LT.10) of gamma-ray multipolarity (L) in
   ! calculations of gamma-transitions both between states in
   ! continuum and from continuum states to discrete levels.
   ! A default value of 'MAXmult' is set to 2 in 'input.f'
   ! but can be adjusted in the input.
   !
   ! The radiative strength functions of higher multipole orders
   ! (f_EL, f_ML) are calculated using the relationships between
   ! single-particle radiative strength functions in the Weisskopf form.
   !
   ! Electric transitions:
   ! f_E(L+1)/f_EL = eg^2*cee*[(3+L)/(5+L)]^2,
   ! cee=[R/(\hbar*c)]^2, R=r_0*A^(2/3), r_0=1.2 fm => cee=3.7D-5*A^(2/3)
   ! xle(i) = f_Ei
   !
   ! Magnetic transitions:
   ! f_M(L+1)/f_E(L+1) = cme,
   ! cme= 10[\hbar/(m*c*R]^2 => cme = 0.307/A^(2/3)
   ! xlm(i) = f_Mi

   xle = 0.0D0
   xlm = 0.0D0
   ha = A(nnuc)**0.666666666666D0
   cee = 3.7D-5*ha
   cme = 0.307D0/ha
   xjc = dble(Jc) + HIS(Nnuc)
   jmin = 1
   jmax = MIN(nlw,jc + maxmult)

   summa = 0.D0
   scrtem(0) = 0.D0
   hrtw_decayg = 0.D0

   ! clear scratch matrix (continuum)

   DO j = 1, ndlw    ! NLW
      DO i = 1, ndex !NEX(Nnuc)
         scrt(i,j,1,0) = 0.D0
         scrt(i,j,2,0) = 0.D0
      ENDDO
   ENDDO

   ! clear scratch matrix (discrete levels)

   DO i = 1, ndlv ! NLV(Nnuc)
      scrtl(i,0) = 0.D0
   ENDDO

   ! IPOS is a parity-index of final states reached by gamma
   ! transitions which do not change parity (E2 and M1)
   ! INEG is a parity-index of final states reached by gamma
   ! transitions which do change parity (E1)

   IF(iec<1)RETURN

   IF(ipc>0) THEN
     ipos = 1
     ineg = 2
   ELSE
     ipos = 2
     ineg = 1
   ENDIF

   ! decay to the continuum

   DO ier = iec - 1, 1, -1            ! do loop over c.n. energies (loops over spins and parities expanded)
     eg = EX(iec,nnuc) - EX(ier,nnuc)
     xle(1) = e1(nnuc,eg,TNUc(ier,nnuc),UEXcit(ier,nnuc))*TUNe(0,nnuc)
     xlm(1) = xm1(eg)*TUNe(0,nnuc)
     xle(2) = e2(eg)*TUNe(0,nnuc)
     xlm(2) = xle(2)*cme
     IF(maxmult>2) THEN
        DO i = 3, maxmult
           xle(i) = xle(i - 1)*eg**2*cee*(dble(i+3)/dble(i+5))**2
           xlm(i) = xle(i)*cme
        ENDDO
     ENDIF
     DO jr = 1, jmax     !do loop over populated (residual) spins
        xjr = dble(jr) + HIS(nnuc)
        lambmin = MAX(1,abs(jc - jr))
        lambmax = xjc + xjr + 0.001
        lambmax = MIN(lambmax,maxmult)
        IF(lambmin<=lambmax) THEN
           scrtpos = 0.0D0
           scrtneg = 0.0D0
           hsumtls = 0.0D0
           DO lamb = lambmin, lambmax
              IF(lamb/2*2==lamb) THEN
                 scrtpos = scrtpos + xle(lamb)
                 scrtneg = scrtneg + xlm(lamb)
                 hsumtls = hsumtls + xle(lamb)**2*RO(ier,jr,ipos,nnuc) + xlm(lamb)**2*RO(ier,jr,ineg,nnuc)
              ELSE
                 scrtpos = scrtpos + xlm(lamb)
                 scrtneg = scrtneg + xle(lamb)
                 hsumtls = hsumtls + xlm(lamb)**2*RO(ier,jr,ipos,nnuc) + xle(lamb)**2*RO(ier,jr,ineg,nnuc)
                 ! first HRTW entry done
              ENDIF
           ENDDO
           scrt(ier,jr,ipos,0) = scrtpos*RO(ier,jr,ipos,nnuc)
           scrt(ier,jr,ineg,0) = scrtneg*RO(ier,jr,ineg,nnuc)
           IF(ier==1 .AND. nint(Z(1))==nint(Z(nnuc))) THEN
              scrt(ier,jr,ipos,0) = SCRt(ier,jr,ipos,0)*DEPart(nnuc)
              scrt(ier,jr,ineg,0) = SCRt(ier,jr,ineg,0)*DEPart(nnuc)
           ENDIF
           H_Sumtls = H_Sumtls + hsumtls
           IF(ier==1) H_Sumtls = H_Sumtls - 0.5*hsumtls   !correct for edge effect in trapeizoidal integration
        ENDIF
     ENDDO     !over populated spins
   ENDDO      !over populated energy bins

   ! decay to the continuum ----** done***---------------------------
   ! integration of ro*gtl in continuum for ejectile 0 (TRAPEZOID)

   DO j = jmin, jmax
     DO i = 1, iec - 1
        summa = summa + SCRt(i,j,1,0) + SCRt(i,j,2,0)
     ENDDO
     summa = summa - 0.5*(SCRt(1,j,1,0) + SCRt(1,j,2,0))
   ENDDO

   summa = summa*de

   ! integration of ro*gtl in continuum for ejectile 0 -- done ----
   !
   ! DECAY TO DISCRETE LEVELS
   !
   ! do loop over discrete levels -----------------------------------
   DO i = 1, NLV(nnuc)
     kmin = abs(xjc - XJLv(i,nnuc)) + 0.001
     kmax = xjc + XJLv(i,nnuc) + 0.001
     lambmin = max0(1,kmin)
     lambmax = MIN(kmax,maxmult)
     IF(lambmin<=lambmax) THEN
        eg = EX(iec,nnuc) - ELV(i,nnuc)
        ipar = (1 + LVP(i,nnuc)*ipc)/2
        iodd = 1 - ipar
        xle(1) = e1(nnuc,eg,TNUc(1,nnuc),UEXcit(1,nnuc))*TUNe(0,nnuc)
        xlm(1) = xm1(eg)*TUNe(0,nnuc)
        xle(2) = e2(eg)*TUNe(0,nnuc)
        IF(lambmax>2) THEN
           xlm(2) = xle(2)*cme
           DO j = 3, lambmax
              xle(j) = xle(j - 1)*eg**2*cee*(dble(j+3)/dble(j+5))**2
              xlm(j) = xle(j)*cme
           ENDDO
        ENDIF
        hscrtl = 0.0D0
        hsumtls = 0.0D0
        DO lamb = lambmin, lambmax
           IF(lamb/2*2==lamb) THEN
              hscrtl = hscrtl + xle(lamb)*ipar + xlm(lamb)*iodd
              hsumtls = hsumtls + xle(lamb)**2*ipar + xlm(lamb)**2*iodd
           ELSE
              hscrtl = hscrtl + xlm(lamb)*ipar + xle(lamb)*iodd
              hsumtls = hsumtls + xlm(lamb)**2*ipar + xle(lamb)**2*iodd
           ENDIF
        ENDDO
        H_Sumtls = H_Sumtls + hsumtls
        H_Sweaks = H_Sweaks + hsumtls
        SCRtl(i,0) = hscrtl
        summa = summa + hscrtl
     ENDIF
   ENDDO

   ! do loop over discrete levels --------- done --------------------

   scrtem(0) = summa
   DENhf = DENhf + summa
   H_Sumtl = H_Sumtl + summa
   H_Sweak = H_Sweak + summa

   hrtw_decayg = summa

   RETURN
   END FUNCTION hrtw_decayg

   !----------------------------------------------------------------------------------------------------

   SUBROUTINE HRTW_error()

   WRITE(8,*) 'Insufficient space allocated for HRTW'
   WRITE(8,*) 'NDHrtw1 in HRTW-mod.f90 needs to be increased'
   WRITE(*,*) 'Insufficient space allocated for HRTW'
   WRITE(*,*) 'NDHrtw1 in HRTW-mod.f90 needs to be increased'
   STOP 'Insufficient space allocated for HRTW'

   END SUBROUTINE HRTW_error

   !----------------------------------------------------------------------------------------------------

   REAL*8 FUNCTION EEF(tl,tav,sumtl,LHRtw)

   !cc   *****************************************************************
   !cc   *                                                      Class:PPu*
   !cc   *                          E E F                                *
   !cc   *                                                               *
   !cc   * Calculates elastic enhancement factor for HRTW theory         *
   !cc   *                                                               *
   !cc   * input: Tl     - transmission coefficient                      *
   !cc   * Tav    - average of all transmission coefficients             *
   !cc   * Sumtl  - sum of transmission coefficients                     *
   !cc   * LHRtw  - selects origin of elastic enhancement factor         *
   !cc   *                1 - Hofmann, Mertelmeier, Herman, Tepel        *
   !cc   *                2 - Kawano, Talou                              *
   !cc   *                                                               *
   !cc   * output: Eef - elastic enhancement factor                      *
   !cc   *                                                               *
   !cc   * Dummy arguments                                               *
   !cc   *                                                               *
   !cc   *                                                               *
   !cc   *****************************************************************C

   IMPLICIT NONE

   ! Dummy arguments

   REAL*8, INTENT(IN) :: sumtl, tav, tl
   INTEGER*4, INTENT(IN) :: LHRtw

   ! Local variables

   REAL*8 :: a, al

   IF(tl<1.0D-15) THEN
     eef = 3.D0
     RETURN
   ENDIF
   SELECT CASE(LHRtw)
   CASE(1)
   al = 4.D0*tav/sumtl*(1.D0 + tl/sumtl)/(1.D0 + 3.D0*tav/sumtl)
   a = 87.D0*(tl - tav)**2*tl**5/sumtl**7
   eef = 1.D0 + 2.D0/(1.D0 + tl**al) + a
   CASE(2)
   eef = 1.D0 + 2.D0/NU(tl,Sumtl,2)
   END SELECT
   ! eef = 1.D0   !uncomment to eliminate elastic enhancement factor, i.e., no HRTW
   eef = min(eef,3.D0)

   RETURN
   END FUNCTION EEF

   !----------------------------------------------------------------------------------------------------

   SUBROUTINE AUSTER(LHRtw)

   !cc   *****************************************************************
   !cc   *                                                      Class:PPu*
   !cc   *                      A U S T E R                              *
   !cc   *                                                               *
   !cc   * Iterates for V quantities in HRTW theory assuming that weak   *
   !cc   * transmission coefficients remain constant and therefore are   *
   !cc   * not iterated. All communication occures through the outchnl   *
   !cc   * structure in the HRTW_mod module.                             *
   !cc   *                                                               *
   !cc   * Input: LHRtw - selects origin of elastic enhancement factor   *
   !cc   *                1 - Hofmann, Mertelmeier, Herman, Tepel        *
   !cc   *                2 - Kawano, Talou                              *
   !cc   *                                                               *
   !cc   *****************************************************************

   IMPLICIT NONE

   INTEGER*4, INTENT(IN) :: Lhrtw
   INTEGER*4 i, icount

   icount = 0
   outchnl(1:NCH)%ti1 = outchnl(1:NCH)%t                     !copy Tlj on the temporary 'ti1' location for iteration
   DO i = 1, NCH
     outchnl(i)%eef = EEF(outchnl(i)%t,H_Tav,H_Sumtl,LHRtw)  !calculate elastic enhancement for all channels
   ENDDO

   iter: DO
     icount = icount + 1
     outchnl(1:NCH)%ti2 = outchnl(1:NCH)%t/(1.D0 + outchnl(1:NCH)%ti1*(outchnl(1:NCH)%eef-1.D0)/H_Sumtl) !actual iteration
     H_Sumtl = SUM(outchnl(1:NCH)%ti2*outchnl(1:NCH)%rho)                                          !new integral
     DO i = 1, NCH
        IF(outchnl(i)%kres==1) H_Sumtl = H_Sumtl - 0.5*(outchnl(i)%ti2*outchnl(i)%rho)                   !correct integral for edge bin
     ENDDO
     H_Sumtl = H_Sumtl + H_Sweak                                                             !add weak channels that are not iterated
     IF(icount>200) THEN
        WRITE(8,*)' WARNING: Maximum iteration number (200) reached in AUSTER'
        EXIT iter
     ENDIF
     DO i = 1, NCH
        IF(abs(outchnl(i)%ti1 - outchnl(i)%ti2)<=1.D-5*outchnl(i)%ti1) CYCLE  !check convergence
        outchnl(1:NCH)%ti1 = outchnl(1:NCH)%ti2                               !make last result an input for a new iteration
        CYCLE iter
     ENDDO
     EXIT
   ENDDO iter

   outchnl(1:NCH)%ti1 = outchnl(1:NCH)%t          !store orgininal Tlj on ti1
   outchnl(1:NCH)%t = outchnl(1:NCH)%ti2          !replace Tlj by Vlj on t
   ! write(*,*) icount, ' HRTW iterations in AUSTER, H_Sumtl=',H_Sumtl

   RETURN
   END SUBROUTINE AUSTER

   !----------------------------------------------------------------------------------------------------

   SUBROUTINE Moldauer

      !cc
      !cc   ********************************************************************
      !cc   *                                                         class:ppu*
      !cc   *               M o l d a u e r                                    *
      !cc   *                                                                  *
      !cc   *  Calculate decay of the Compound Nucleus capture states in       *
      !cc   *  terms of the  Moldauer approach to account for the width        *
      !cci  *  fluctuation correction.                                         *
      !cc   *                                                                  *
      !cc   *                                                                  *
      !cc   ********************************************************************
      !cc

      IMPLICIT NONE

      REAL*8 :: elada(NDAngecis), elleg(NDAngecis)
      INTEGER neles
      COMMON /angula/elada,elleg,neles
      REAL*8 :: sumin_w, sumtt_w
      COMMON /EWcorr/ sumin_w, sumtt_w

      CHARACTER(1), PARAMETER :: cpar(2) = (/'+', '-'/)

      ! Local variables

      LOGICAL*4 relcal
      INTEGER*4 i, ip, ipar, jcn, ke, m, ndivf, nejc, nhrtw, nnuc, nnur, itmp, lleg, numch_el
      REAL*8 cnspin, fisxse, summa, sumfis, sumtg, tgexper, xnor, xjc
      REAL*8 Ia, xjr, ja, jb, la, lb, xleg, tmp
      REAL*8 xmas_npro, xmas_ntrg, el, ecms, ak2
      REAL*8 d0c
      REAL*8 sumfism(nfmod) ! , cel_da(NDAngecis), GET_DDXS
      REAL*8 :: sumin_s, sumtt_s, stmp, ewcor, w
      !
      TYPE (channel), POINTER :: out
      TYPE (fusion),  POINTER :: in

      CALL AllocHRTW()    !allocate HRTW matrices
      H_Tthr = 1.0D-6     !threshold for considering channel to be a 'strong' one
      nnuc = 1            !set CN nucleus
      csfis = 0.D0
      SUMfis = 0.D0
      gcasc = 1.0         !ensure full gamma cascade with WFC
      ke = NEX(nnuc)


      IF (AEJc(0).EQ.0.0D0) THEN
         WRITE(8,*) 'WARNING:  Width fluctuation correction for neutron reactions only.'
         RETURN
      END IF

      ! Initialize variables and print heading for normalizing g-strength function

      d0c = 0.D0
      sumtg = 0.D0
      tgexper = 0.D0
      IF(.NOT. benchm) THEN
         WRITE(8,'(1x,''Renormalization of gamma-ray strength function'')')
         WRITE(8,'(1x,''-------------------------------------------------------------'')')
      ENDIF
      IF(first_ein .AND. (einl>1.D0) ) THEN
         WRITE(8,'(1x,'' WARNING: First incident energy Einc must be < 1MeV for Do and Gg calculations and'')')
         WRITE(8,'(1x,'' WARNING: for the renormalization of gamma-ray strength function'')')
      ENDIF

      ! xmas_npro = EJMass(NPRoject)
      xmas_npro = EJMass(0)
      xmas_ntrg = AMAss(0)


      el = EINl
      relcal = .FALSE.
      !IF(IRElat(NPRoject,0)>0 .OR. RELkin) relcal = .TRUE.
      IF(IRElat(0,0)>0 .OR. RELkin) relcal = .TRUE.

      IF (AEJc(0).EQ.0.0D0) THEN
         xmas_npro = 0.d0
         relcal = .TRUE.
      ENDIF

      CALL kinema(el,ecms,xmas_npro,xmas_ntrg,ak2,1,relcal)

      ! write(*,*) 'HRTW=',10.D0*PI/ak2,el,IRElat(0,0),RELKIN,relcal
      coef = 10.D0*PI/ak2/(2.D0*XJLv(LEVtarg,0) + 1.d0)/(2.D0*SEJc(0) + 1.d0)
      ! start CN nucleus decay
      DO ipar = 1, 2                                      ! do loop over decaying nucleus parity
         ip = 1 - 2*abs(mod(ipar+1,2))                    ! actual parity of the state (+1 or -1)
         DO jcn = 1, nlw                                  ! do loop over decaying nucleus spin
            xjc = float(jcn) + HIS(nnuc)
            IF(POP(ke,jcn,ipar,nnuc)<=1.0d-15) CYCLE      ! skip if absorption cross section negligible
            ! write(8,*) ' '
            ! write(8,*) 'CN Jpi=',xjc*ip
            nhrtw = 0
            DENhf = 0.D0
            NSCh = 0
            NCH = 0
            outchnl%l = 0
            outchnl%j = 0.d0
            outchnl%t = 0.d0
            outchnl%ti1 = 0.d0
            outchnl%ti2 = 0.d0
            outchnl%rho = 0.d0
            outchnl%eef = 1.d0
            outchnl%nejc = 0
            outchnl%kres = 0
            outchnl%xjrs = 0.d0
            outchnl%jres = 0
            outchnl%pres = 0
            inchnl%nout = 0
            inchnl%l = 0
            inchnl%j = 0.d0
            inchnl%t = 0.d0
            inchnl%sig = 0.d0
            num%neut = 0
            num%part = 0
            num%elal = 0
            num%elah = 0
            num%fiss = 0
            num%gamm = 0
            H_Sumtls = 0.D0
            H_Sumtl = 0.D0
            H_Tav = 0.D0
            H_Sweak = 0.D0
            H_Tl = 0.D0
            IF(gdrdyn==1.0D0) CALL ULMDYN(nnuc,jcn,EX(ke,nnuc)) ! prepare GDR parameters (if spin dependent GDR selected)

            ! Collecting outgoing channels **********************************************************************************************

            ! particle decay

            DO nejc = 1, nejcm                            !do loop over ejectiles
               IF(NREs(nejc)<0) CYCLE
               nnur = NREs(nejc)
               summa = HRTW_DECAY(nnuc,ke,jcn,ip,nnur,nejc)
            ENDDO                                         !do loop over ejectiles  ***done***

            ! write(*,*) sumin_w,sumtt_w

            num%part = nch                                !store number of particle channel entries

            ! gamma emission is always a weak channel (one iteration)

            sumg = HRTW_DECAYG(nnuc,ke,jcn,ip)
            ! H_Sumtl = H_Sumtl + sumg
            ! H_Sweak = H_Sweak + sumg

            ! Trying to include gamma in Moldauer the same way we do with continuum bin
            !               nch = nch + 1
            !               if(nch>ndhrtw1) call HRTW_error()    !STOP - insufficent space allocation
            !               num%gamm = nch
            !               out => outchnl(nch)
            !               out%t = sumg/out%rho
            !               out%rho = sumg/H_Tthr + 1.D0
            !               out%nejc = 0

            ! fission (may be a weak or strong channel)

            sumfis = 0.D0
            tfis = 0.D0
            ndivf = 1
            num%fiss = 0
            IF(FISsil(nnuc)) THEN
               IF(nint(FISshi(nnuc))==1) THEN
                  CALL FISSION(nnuc,ke,jcn,sumfis)
               ELSE
                  CALL FISCROSS(nnuc,ke,ip,jcn,sumfis,sumfism)
               ENDIF
               DENhf = DENhf + sumfis
               H_Sumtl = H_Sumtl + sumfis
               ! dividing sumfis into channels with TFIs < 0.25 each

               ndivf = int(sumfis/0.25) + 1
               tfis = sumfis/dfloat(ndivf)
               H_Sumtls = H_Sumtls + tfis**2*dfloat(ndivf)
               IF(tfis>=H_Tthr) THEN                      ! fission treated as a strong channel
                  nch = nch + 1                           ! otherwise 'sumfis' it is left untouched
                  num%fiss = nch                          ! store position of fission (only one entry with 'ndivf')
                  outchnl(nch)%t = tfis
                  outchnl(nch)%rho = dfloat(ndivf)
                  outchnl(nch)%nejc = 100                 ! nejc=100 convention identifies fission
               ENDIF
            ENDIF
            IF(H_Sumtl.LE.0.0D0) CYCLE                    ! no transitions from the current state


            !  write(*,*)' '
            !  write(*,*)'SUMMARY OF DECAY FOR J=',xjc
            !  write(*,*)'total sum of  Tls ', H_Sumtl
            !  write(*,*)'sum of strong Tls ', H_Sumtl-H_Sweak
            !  write(*,*)'sum of weak   Tls ', H_Sweak
            !  write(*,*)'sum of weak part. ', H_Sweak-sumg
            !  write(*,*)'sum of gammas     ', sumg
            !  write(*,*)'sum fission       ', sumfis
            !  write(*,*)'sum Tl**2         ', H_Sumtls
            !  write(*,*)'# of strong Tls   ', nch
            !  write(*,*)'average Tl        ', H_Tav
            !  write(*,*)'first entry DENhf=', DENhf

            ! Collecting outgoing channels completed  ************************************************************************************

!            write(*,*) 'Decay state ',jcn*ip, ' DENhf calculated before HRTW or Moldauer', DENhf

            !Special treatment of the gamma channels in Moldauer -
            !replace all gammas by a fixed number of equal gammas (outchnl(num%gamm) and apply W to all SCRt and SCRtl
            !before strong channels are added.
            !               w = WFC(num%elal,num%gamm)
            !               write(*,*) 'W for gammas', w
            !               SCRt = SCRt*w
            !               SCRtl = SCRtl*w
!            CALL WFC1()     ! Calculate all nu's and the part of Moldauer integral that doesn't depend on incoming and outgoing channles

            ! Loop over incoming (fusion) channels *****************************************************************************************

            DO i = num%elal, num%elah                   ! do loop over elastic channels
               in => inchnl(i - num%elal + 1)           ! elastic channels for each Jcn are numbered 1,2,3,...
               out => outchnl(i)
               in%t = out%t
               in%sig = coef*in%t*(2.D0*xjc + 1.D0)*FUSred*REDmsc(jcn,ipar)  ! absorption for incoming channel
               xnor=0.D0
               IF(DENhf==0.D0) CYCLE
               xnor = in%sig/DENhf                                 ! normalization factor
               ! write(*,*) 'Jcn, Tlj_in, Tlj_out, coef, sig ', xjc, in%t, out%t, coef, in%sig
               sumin_s = 0.d0
               sumtt_s = 0.d0
               DO iout = 1, num%part                       !Scan strong particle channels (note: weak channels are already in SCRt)
                  out => outchnl(iout)                     !ATTENTION: redefining outgoing channel!!!
!                  w = WFC2(i,iout)                         !Moldauer width fluctuation factor (ECIS style)
                  w = WFC(i,iout)                          !Moldauer width fluctuation factor (paper formula)
                  ! WRITE(8,*) 'continuum WFC', iout, w
                  IF(out%kres>0) THEN                      !continuum channels
                     SCRt(out%kres,out%jres,out%pres,out%nejc) = SCRt(out%kres,out%jres,out%pres,out%nejc) &
                        + out%t*out%rho*w/de
                  ELSE IF(out%kres<0) THEN
                     IF( (out%nejc.EQ.NPRoject) .AND. (-out%kres.NE.LEVtarg) ) THEN ! apply CINRED to discrete inelastic channels
                        stmp = out%t*out%rho*w
                        sumin_s = sumin_s + stmp * CINRED(-out%kres)
                        sumtt_s = sumtt_s + stmp
                        SCRtl(-out%kres,out%nejc) = SCRtl(-out%kres,out%nejc) +  stmp * CINRED(-out%kres)
                     ELSE
                        SCRtl(-out%kres,out%nejc) = SCRtl(-out%kres,out%nejc) + out%t*out%rho*w
                     ENDIF
                  ENDIF
               ENDDO
               ewcor = (sumtt_s - sumin_s +  sumtt_w - sumin_w)
               ! Correcting the elastic cross section for inelastic enhancement CINRED
               numch_el = max(num%elah - num%elal + 1 , 1 )
               DO iout = num%elal, num%elah  ! do loop over elastic channels
                  out => outchnl(iout)
                  SCRtl(-out%kres,out%nejc) = SCRtl(-out%kres,out%nejc) + ewcor/numch_el
                  !write(*,*) 'ilev=',-out%kres,' nejc=',out%nejc,' ewcor=',ewcor
               ENDDO
               IF(num%fiss>0) sumfis = outchnl(num%fiss)%t*outchnl(num%fiss)%rho*WFC2(i,num%fiss)  !redefining sumfis to account for the HRTW T=>V transition
               !Renormalizing scratch matrices to recover unitarity
               DENhf = 0.0d0                             !test that SCRt+SCRtl sum to the same DENhf
               DENhf = SUM(SCRt)*de + SUM(SCRtl) + sumfis
               DENhf = DENhf - 0.5*SUM(SCRt(1,:,:,:))*de   !correct for the edge effect in trapezoidal integration
!               write(*,*)'DENhf calculated as integral of SCRt & SCRtl', DENhf
               !IF(DENhf>0.D0) xnor = in%sig/DENhf                                 ! normalization factor

               ! CN angular distributions (neutron (in)elastic scattering ONLY!)  *******************************************************
               ewcor = 0.d0
               IF(.NOT.CN_isotropic) THEN
                  ! accumulate Legendre coefficients
                  nejc = 1
                  nnur = 2
                  Ia = XJLv(LEVtarg,0)                  !target spin
                  la = in%l                             !incident neutron l
                  ja = in%j                             !incident neutron j
                  ! write(8,*) 'Incident chnl',i, 'J_pi ',xjc*ip,' number of outgoing n channels', num%neut
                  DO iout = 1, num%neut                    !do loop over neutron channels only
                     ! write(8,*) 'Elastic channel #',j, ' abs = ',in%sig, ' xnor = ', xnor
                     ! write(8,*) '                                  leg      BB               Jcn   &
                     ! &               l_inc           j_inc                 J_res              l_out              J_out'
                     ! w = 1.D0
                     w = WFC2(i,iout)             !Moldauer width fluctuation factor
                     out => outchnl(iout)
                     xjr = out%xjrs              !residual nucleus J
                     lb = out%l                  !outgoing neutron l
                     jb = out%j                  !outgoing neutron j
                     IF(out%kres > 0) THEN
                        PLcont_lmax(out%kres) = 2*in%l
                     ELSEIF(out%kres < 0) THEN
                        PL_lmax(-out%kres) = 2*in%l
                     ENDIF

                     IF(i/=iout.AND.out%kres<=0) THEN               ! Inelastic to discrete level
                        stmp = out%t*out%rho*w*CINRED(-out%kres)
                     ELSE                                        ! Elastic and continuum
                        stmp = out%t*out%rho*w
                     ENDIF

                     DO lleg = 0, 2*in%l, 2    !do loop over Legendre L
                        xleg = dble(lleg)
                        tmp = Blatt(xjc,Ia,la,ja,SEJc(nejc),xjr,lb,jb,SEJc(nejc),xleg)/(2*xleg + 1.0d0)
                        ! write(8,*) ' Leg => tmp,xjc,la,ja,xjr,lb,jb,',lleg,tmp,xjc,la,ja,xjr,lb,jb,outchnl(iout)%kres
                        ! if(tmp==0.D0) cycle
                        IF(dabs(tmp) < 1.d-14) CYCLE
                        tmp = tmp*xnor*stmp  !*out%t*out%rho
                        IF(out%kres > 0) THEN
                           PL_CNcont(lleg,out%kres) = PL_CNcont(lleg,out%kres) + tmp
                        ELSEIF(out%kres < 0) THEN
                           PL_CN(lleg,-out%kres) = PL_CN(lleg,-out%kres) + tmp
                        ENDIF
                     ENDDO

                     !IF(out%kres > 0) THEN
                     !  write (8,'(2x,A10, 5Hchan= ,I4,2x,5Hlmax= ,I4,5H cont  )') 'HRTW-comp ',out%kres, PLcont_lmax(out%kres)
                     !ELSEIF(outchnl(iout)%kres < 0) THEN
                     !  write (8,'(2x,A10, 5Hchan= ,I4,2x,5Hlmax= ,I4,5H disc  )') 'HRTW-comp ',out%kres, PL_lmax(-out%kres)
                     !ENDIF
                     !write(8,*) 'PL_CNcont(lleg,1) = ', PL_CNcont(0,1), PL_CNcont(2,1), PL_CNcont(4,1), PL_CNcont(6,1), PL_CNcont(8,1)

                  ENDDO
               ENDIF    !end of Legendre coeficients accumulation for Anisotropic CN

               CALL XSECT(nnuc,m,xnor,sumfis,sumfism,ke,ipar,jcn,fisxse)  !normalize SCRt matrices and store x-sec

               IF(ewcor.NE.0.d0) THEN
                  out => outchnl(i)
                  !restore SCRtl before new elastic is calculated
                  SCRtl(-out%kres,out%nejc) = SCRtl(-out%kres,out%nejc) - ewcor
               ENDIF

               !write(*,*) 'ewcor=',ewcor

            ENDDO    !end do loop over incident channels

            ! Gamma width calculation *************************************************************************************

            IF((first_ein .OR. benchm) .AND. einl<=1.D0) THEN
               cnspin = jcn - 0.5
               IF(mod(XJLv(levtarg,0)*2.,2.D+0)==1)cnspin = jcn - 1
               IF(ip==LVP(levtarg,0) .AND. ((cnspin==XJLv(levtarg,0)+0.5) .OR. (cnspin==XJLv(levtarg,0)-0.5))) THEN
                  d0c = d0c + RO(ke,jcn,ipar,nnuc)
                  ! write(8,*)'ke,jcn,ipar,ro',ke,jcn,ipar,RO(ke,jcn,ipar,nnuc)
                  WRITE(8,'(A12,f4.1,A6,A1,A6,F7.3,A4,/,A37,d12.6)')'CN state J=', cnspin, ', Par:', cpar(ipar), ' at U=',&
                     EX(ke,nnuc), ' MeV', 'Int[Rho(U)*Tlg(U)] + Sum[Tlg(Ui)] = ', sumg
                  sumtg = sumtg + sumg
               ENDIF
            ENDIF
         ENDDO       !loop over decaying nucleus spin
      ENDDO          !loop over decaying nucleus parity
      IF(d0c>0.D0) d0c = 1000.0/d0c
      IF(d0_obs==0.0D0) d0_obs = d0c    !use calculated D0 (in keV) if not measured
      IF(benchm) THEN
         WRITE(8,*)
         WRITE(8,'(1x,'' WARNING: Gamma emission width not normalized in benchmark calculations'')')
         WRITE(8,*)
      ENDIF
      itmp = iabs( NINT(1000*TUNe(0,nnuc)) - 999 )
      IF(itmp.EQ.1 .AND. (.NOT.benchm)) THEN
         WRITE(8,'(1x,'' WARNING: Gamma emission width not normalized (TUNE set to 1.000 in input)'')')
         WRITE(8,*)
      ENDIF
      IF(itmp.GT.1 .AND. (.NOT.benchm)) THEN
         WRITE (8 ,'('' WARNING: Gamma emission width from '',I3,A2,'' normalized by '',F7.3)') NINT(A(nnuc)),&
            SYMb(nnuc), TUNe(0,nnuc)
         WRITE (12,'('' Gamma emission width from '',I3,A2,'' normalized by '',F7.3)') NINT(A(nnuc)), SYMb(nnuc), TUNe(0,nnuc)
         WRITE(8,*)
      ENDIF
      ! IF(EINl<=1.D0 .AND. (FIRst_ein .or. BENchm)) THEN
      IF(einl<=1.D0 .AND. first_ein) THEN
         IF(d0_obs>0.D0) THEN
            tgexper = 2*pi*gg_obs/d0_obs/1.E6
            WRITE(8,'(1x,''Experimental information from capture channel'')')
            WRITE(8,'(1x,A13,D12.6)')'2*pi*Gg/D0 = ', tgexper
         ENDIF
         IF(gg_unc>0.0D0) THEN
            WRITE(8,'(1x,A5,F9.3,A5,F8.3,A4)')'Gg = ', gg_obs, ' +/- ', gg_unc, ' meV'
         ELSE
            WRITE(8,'(1x,A5,F9.3,A18)')'Gg = ', gg_obs, ' meV (systematics)'
         ENDIF
         WRITE(12,'(/1x,A5,F9.3,A5,F8.3,A4)')'Gg = ', gg_obs, ' +/- ', gg_unc, ' meV'
         IF(d0_obs>0.0D0) THEN
            WRITE(8,'(1x,A5,F11.6,A5,F11.6,A4)')'D0 = ', d0_obs, ' +/- ', d0_unc, ' keV'
            WRITE(8,'(1x,A5,F11.6,A17)')'D0 = ', d0c, ' keV (calculated)'
            WRITE(12,'(1x,''D0 = '',F8.3,'' keV'')')d0_obs
         ELSE
            WRITE(8,'(1x,A5,F11.6,A17)')'D0 = ', d0c, ' keV (calculated)'
            WRITE(12,'(1x,''D0 = '',F8.3,'' keV, CALC'')')d0c
         ENDIF
         WRITE(8,*)
         WRITE(12,*)
         IF(itmp==0 .AND. (.NOT.benchm)) THEN
            IF(sumtg>0.D0 .AND. tgexper>0.D0) THEN
               tune(0,nnuc) = tgexper/sumtg
               WRITE(8,'(1x,'' WARNING: Gamma emission normalization factor is set to '',F7.3)') TUNe(0,nnuc)
               IF (first_ein) WRITE(8,'(1x,'' WARNING: The normalization is not applied to this incident energy'')')
            ELSE
               WRITE(8,'(1x,'' WARNING: Gamma emission width is not normalized to Do'')')
            ENDIF
            WRITE(8,*)
         ENDIF
         IF(itmp==1 .AND. (.NOT.benchm) .AND. (sumtg>0.D0 .AND. tgexper>0.D0) ) THEN
            WRITE(8,'(1x,'' WARNING: Gamma emission could be normalized by setting TUNE to '',F7.3,'' in input'')')  tgexper/sumtg
            WRITE(8,*)
         ENDIF
      ENDIF

      CALL DelHRTW()    !deallocate HRTW arrays

      RETURN
   END SUBROUTINE Moldauer



   real*8 function NU(Tl,Sumtl,Itype)

      !cc   *****************************************************************
      !cc   *                                                      Class:PPu*
      !cc   *                          N U                                  *
      !cc   *                                                               *
      !cc   * Calculates channel degree  of freedom for Width Fluctuation   *
      !cc   * Correction used in statistiacl model using original Moldauer  *
      !cc   * and Kawano formulations                                       *
      !cc   *                                                               *
      !cc   * input:                                                        *
      !cc   * Tl     - transmission coefficient                             *
      !cc   * Sumtl  - sum of transmission coefficients                     *
      !cc   * Itype  - 1 Moldauer                                           *
      !cc   *        - 2 Kawano and Talou                                   *
      !cc   *                                                               *
      !cc   * output: NU- channel degree of freedom                         *
      !cc   *                                                               *
      !cc   *                                                               *
      !cc   *                                                               *
      !cc   *****************************************************************

      IMPLICIT NONE

      ! Dummy arguments

      REAL*8, intent(in) :: Sumtl, Tl
      INTEGER*4, intent(in) :: Itype

      ! Local variables

      REAL*8 :: al, be, ce

      SELECT CASE (Itype)
         CASE (1)
            NU = 1.78D0+(Tl**1.212D0-0.78D0)*exp(-0.228D0*Sumtl)
         CASE (2)  !Kawano
            al = 0.0287892D0*Tl + 0.245856D0
            ce = Tl**2 - (Sumtl - 2*Tl)**2
            be = 1.D0 + 2.5D0*Tl*(1.D0 - Tl)*exp(-2.D0*Sumtl)
            IF(Sumtl<2.0D0*Tl .AND. ce>0.d0) THEN
               ! f = al*be1*(Tl + Sumtl)/(1.D0 - Tl)*sqrt(c)/Tl
               NU = 2 -  (1.D0-Tl)*Tl / ( (1.D0-Tl)*Tl + al*be*(Sumtl + Tl)*sqrt(ce) )
            ELSE
               NU = 2 -  (1.D0-Tl)    / ( (1.D0-Tl)    + al*be*(Sumtl + Tl) )
            ENDIF
         CASE DEFAULT
            write(8,'(''Stop: unsupported nu model '',i3)') Itype
            write(*,'(''Stop: unsupported nu model '',i3)') Itype
            STOP
      END SELECT
      NU = Max(1.D0, NU)
      NU = Min(2.D0, NU)
      RETURN
   END FUNCTION NU

   !----------------------------------------------------------------------------------------------------

   real*8 function WFC(in,out)
      !
      !  Calculates width fluctuation correction (WFC) using Moldauer approach
      !
      IMPLICIT NONE
      integer*4, intent(in):: in, out         ! index number of incoming (in) and outgoing (out) channels
      integer*4:: del_ic=0, del_co=0          ! delta function for (in,channel c), and (channle c,out)
      integer*4, parameter:: ndGL=20
      integer*4:: i,ic
      real*8:: nu_c, nu_in, tmp
      !  Data (x and weights) for Gauss-Laguerre quadrature
      REAL*8, DIMENSION(1:ndGL), PARAMETER:: x = (/ &
         7.0539889691988753D-02,&
         3.7212681800161144D-01,&
         9.1658210248327356D-01,&
         1.7073065310283439D+00,&
         2.7491992553094321D+00,&
         4.0489253138508869D+00,&
         5.6151749708616165D+00,&
         7.4590174536710633D+00,&
         9.5943928695810968D+00,&
         1.2038802546964316D+01,&
         1.4814293442630740D+01,&
         1.7948895520519376D+01,&
         2.1478788240285011D+01,&
         2.5451702793186906D+01,&
         2.9932554631700612D+01,&
         3.5013434240479000D+01,&
         4.0833057056728571D+01,&
         4.7619994047346502D+01,&
         5.5810795750063899D+01,&
         6.6524416525615754D+01/)
      REAL*8, DIMENSION(1:ndGL), PARAMETER:: w = (/ &
         1.8108006241898926D-01,&
         4.2255676787856397D-01,&
         6.6690954670184815D-01,&
         9.1535237278307367D-01,&
         1.1695397071955460D+00,&
         1.4313549859282060D+00,&
         1.7029811379850227D+00,&
         1.9870158907927472D+00,&
         2.2866357812534308D+00,&
         2.6058347275538333D+00,&
         2.9497837342139509D+00,&
         3.3253957820093196D+00,&
         3.7422554705898109D+00,&
         4.2142367102518804D+00,&
         4.7625184614902093D+00,&
         5.4217260442455743D+00,&
         6.2540123569324213D+00,&
         7.3873143890544346D+00,&
         9.1513287309874796D+00,&
         1.2893388645939997D+01/)
      !      REAL*8, DIMENSION(1:ndGL), PARAMETER:: x = (/ &
      !         0.035700394308888385122084471D0,&
      !         0.188162283158698516003589346D0,&
      !         0.462694281314576453564937525D0,&
      !         0.859772963972934922257272225D0,&
      !         1.380010820527337186498000330D0,&
      !         2.024209135922826733442066003D0,&
      !         2.793369353506816457653514486D0,&
      !         3.688702677908270209591526352D0,&
      !         4.711641146554972693618722836D0,&
      !         5.863850878343718114273164238D0,&
      !         7.147247908102288250685691952D0,&
      !         8.564017017586163762718522042D0,&
      !         10.11663404845193940684962966D0,&
      !         11.80789229400458484284158670D0,&
      !         13.64093371253708722837167636D0,&
      !         15.61928589333907383720196365D0,&
      !         17.74690595009566304257387749D0,&
      !         20.02823283457489052961261481D0,&
      !         22.46824998349841835137178623D0,&
      !         25.07256077242620379439608621D0,&
      !         27.84748000916886272075170414D0,&
      !         30.80014573944546270075438520D0,&
      !         33.93865708491371960909885859D0,&
      !         37.27224588047600432832076099D0,&
      !         40.81149282388692046615567558D0,&
      !         44.56860317533446270712302063D0,&
      !         48.55776353305999228096204881D0,&
      !         52.79561118721693296935202114D0,&
      !         57.30186332339362749503374700D0,&
      !         62.10017907277511161216819906D0,&
      !         67.21937092712699879908027755D0,&
      !         72.69515884761246211752192772D0,&
      !         78.57280291157130928054389683D0,&
      !         84.91123113570498454270156471D0,&
      !         91.78987467123637699233719348D0,&
      !         99.32080871744680825010905416D0,&
      !         107.6724406393882725207967676D0,&
      !         117.1223095126906888076506441D0,&
      !         128.2018419882556511925411044D0,&
      !         142.2800444691599978883488354D0/)
      !      REAL*8, DIMENSION(1:ndGL), PARAMETER:: w = (/ &
      !         0.091625471157459897311511698080137483D0,&
      !         0.213420584905012080007193367121512341D0,&
      !         0.335718116680284673880510701616292191D0,&
      !         0.458540935033497560385432380376452497D0,&
      !         0.582068165779105168990996365401543284D0,&
      !         0.706495216367219392989830015673016682D0,&
      !         0.832026903003485238099112947978349524D0,&
      !         0.958878198794443111448122679676028907D0,&
      !         1.087276162030549715753869333172026616D0,&
      !         1.217462327977780978954277850665609481D0,&
      !         1.349695491356765307923938594423945197D0,&
      !         1.484254929776846711205611786129787199D0,&
      !         1.621444162811821978023168843164545274D0,&
      !         1.761595374676769611184242204209815984D0,&
      !         1.905074665894799676682993205972793711D0,&
      !         2.052288347261716717601995822729474548D0,&
      !         2.203690553245095889098283443281405708D0,&
      !         2.359792538523203323540373753789014974D0,&
      !         2.521174140376432991653136902874228210D0,&
      !         2.688498055408842264159505447063746595D0,&
      !         2.862527813210448812034763959831043113D0,&
      !         3.044150665311517100410439679543336704D0,&
      !         3.234407097263531941774902394288671117D0,&
      !         3.434529398427748092203984818916024650D0,&
      !         3.645992824994089072389656466994904344D0,&
      !         3.870584597216516568084753202134443384D0,&
      !         4.110498680432822655835822472639515779D0,&
      !         4.368468723254063474508083382729450258D0,&
      !         4.64795898407446688299303399883883991D0, &
      !         4.95344611240989326218696150785562721D0, &
      !         5.29084840590073657468737365718858969D0, &
      !         5.66820460903297677000730529023263795D0, &
      !         6.09679641474342030593376010859198806D0, &
      !         6.59310886103999953794429664206294899D0, &
      !         7.18249599553689315064429801626699575D0, &
      !         7.90666631138422877369310742310586595D0, &
      !         8.84089249281034652079125595063026792D0, &
      !         10.1408992656211694839094600306940469D0, &
      !         12.2100212992046038985226485875881110D0, &
      !         16.7055206420242974052468774398573550D0/)

      nu_in =  outchnl(in)%eef/2.D0      ! half of the degree of freedom for the incoming channel
      WFC = 0.D0
      do i = 1, ndGL        ! do loop over Gauss Laguerre integration steps
         tmp = 0.D0
         !         tmp = 1.D0
         do ic = 1, NCH    ! do loop over channels
            !            if(ic==num%gamm) cycle
            if(in==ic) then
               del_ic = 1
            else
               del_ic = 0
            endif
            if(out==ic) then
               del_co = 1
            else
               del_co = 0
            endif
            nu_c  =  outchnl(ic)%eef/2.D0      ! half of the degree of freedom for the running channel c
            !        Integral in Moldauer formula for WFC using log to turn product into a sum
            tmp = tmp - (outchnl(ic)%rho*nu_c + del_ic + del_co)*dlog(1.D0 + x(i)*outchnl(ic)%t/nu_c/H_Sumtl)
         !            tmp = tmp*(1.D0 + x(i)*outchnl(ic)%t/nu_c/H_Sumtl)**(-nu_c - del_ic - del_co)
         end do
         tmp = tmp - x(i)*H_Sweak/H_Sumtl

         WFC = WFC + w(i)*dexp(tmp)
      !         WFC = WFC + w(i)*tmp
      end do
      if(in==out) then                       ! case of elastic
         WFC = (1.D0 + 1.D0/nu_in)*WFC
      end if

      return
   end function WFC

  SUBROUTINE WFC1()

      !****************************************************************************************
      !                            W F C 1
      !
      !   Moldauer width fluctuation formula - part 1. Calculates global component of the
      !   integral that is independent of the incident and outgoing channels. Uses 20-point
      !   Gauss-Laguerre quadratuire and includes calculation of the degrees of freedom.
      !
      !****************************************************************************************


      integer*4, parameter:: ndGL=20
      integer*4:: i,ic
      real*8:: nu_c, a1
      TYPE (channel), POINTER :: out

      !  Data (x and weights) for Gauss-Laguerre quadrature
      REAL*8, DIMENSION(1:ndGL), PARAMETER:: x = (/ &
         7.0539889691988753D-02,&
         3.7212681800161144D-01,&
         9.1658210248327356D-01,&
         1.7073065310283439D+00,&
         2.7491992553094321D+00,&
         4.0489253138508869D+00,&
         5.6151749708616165D+00,&
         7.4590174536710633D+00,&
         9.5943928695810968D+00,&
         1.2038802546964316D+01,&
         1.4814293442630740D+01,&
         1.7948895520519376D+01,&
         2.1478788240285011D+01,&
         2.5451702793186906D+01,&
         2.9932554631700612D+01,&
         3.5013434240479000D+01,&
         4.0833057056728571D+01,&
         4.7619994047346502D+01,&
         5.5810795750063899D+01,&
         6.6524416525615754D+01/)

        save_WFC1 = 1.D0
        do ic = 1, NCH                           ! do loop over all channels
           out => outchnl(ic)
           out%eef = NU(out%t,H_Sumtl,LHRtw-2)   ! Calculate degrees of freedom for all  channels
           nu_c = out%eef/2.D0
           a1 = out%t/nu_c/H_Sumtl
           IF(a1==0) CYCLE
           IF(a1>1.D-9) THEN
           do i = 1, ndGL                                                     ! do loop over Gauss Laguerre integration steps
              save_WFC1(i) = save_WFC1(i)*((H_Sumtl + x(i)*out%t/nu_c)/H_Sumtl)**outchnl(ic)%rho*nu_c
           end do
           ELSE
           do i = 1, ndGL                                                     ! do loop over Gauss Laguerre integration steps
              save_WFC1(i) = save_WFC1(i)*DEXP(x(i)*out%t/H_Sumtl*out%rho*(1.D0 - 0.5D0*x(i)*a1))
           end do
           ENDIF
        end do
        do i = 1, ndGL                                                        ! do loop over Gauss Laguerre integration steps
           save_WFC1(i) = save_WFC1(i)*(1.D0+x(i)*sumg/20.D0/H_Sumtl)**20     !adding gammas
        end do
      return
   end subroutine WFC1




   !----------------------------------------------------------------------------------------------------

     real*8 function WFC2(in,ou)
      !
      !  Calculates width fluctuation correction (WFC) using Moldauer approach
      !
      IMPLICIT NONE
      integer*4, intent(in):: in, ou         ! index number of incoming (in) and outgoing (out) channels
      integer*4, parameter:: ndGL=20
      integer*4:: i
      real*8:: nu_ou, nu_in, tmp
      !  Data (x and weights) for Gauss-Laguerre quadrature
      REAL*8, DIMENSION(1:ndGL), PARAMETER:: x = (/ &
         7.0539889691988753D-02,&
         3.7212681800161144D-01,&
         9.1658210248327356D-01,&
         1.7073065310283439D+00,&
         2.7491992553094321D+00,&
         4.0489253138508869D+00,&
         5.6151749708616165D+00,&
         7.4590174536710633D+00,&
         9.5943928695810968D+00,&
         1.2038802546964316D+01,&
         1.4814293442630740D+01,&
         1.7948895520519376D+01,&
         2.1478788240285011D+01,&
         2.5451702793186906D+01,&
         2.9932554631700612D+01,&
         3.5013434240479000D+01,&
         4.0833057056728571D+01,&
         4.7619994047346502D+01,&
         5.5810795750063899D+01,&
         6.6524416525615754D+01/)
      REAL*8, DIMENSION(1:ndGL), PARAMETER:: w = (/ &
         1.8108006241898926D-01,&
         4.2255676787856397D-01,&
         6.6690954670184815D-01,&
         9.1535237278307367D-01,&
         1.1695397071955460D+00,&
         1.4313549859282060D+00,&
         1.7029811379850227D+00,&
         1.9870158907927472D+00,&
         2.2866357812534308D+00,&
         2.6058347275538333D+00,&
         2.9497837342139509D+00,&
         3.3253957820093196D+00,&
         3.7422554705898109D+00,&
         4.2142367102518804D+00,&
         4.7625184614902093D+00,&
         5.4217260442455743D+00,&
         6.2540123569324213D+00,&
         7.3873143890544346D+00,&
         9.1513287309874796D+00,&
         1.2893388645939997D+01/)
      !      REAL*8, DIMENSION(1:ndGL), PARAMETER:: x = (/ &
      !         0.035700394308888385122084471D0,&
      !         0.188162283158698516003589346D0,&
      !         0.462694281314576453564937525D0,&
      !         0.859772963972934922257272225D0,&
      !         1.380010820527337186498000330D0,&
      !         2.024209135922826733442066003D0,&
      !         2.793369353506816457653514486D0,&
      !         3.688702677908270209591526352D0,&
      !         4.711641146554972693618722836D0,&
      !         5.863850878343718114273164238D0,&
      !         7.147247908102288250685691952D0,&
      !         8.564017017586163762718522042D0,&
      !         10.11663404845193940684962966D0,&
      !         11.80789229400458484284158670D0,&
      !         13.64093371253708722837167636D0,&
      !         15.61928589333907383720196365D0,&
      !         17.74690595009566304257387749D0,&
      !         20.02823283457489052961261481D0,&
      !         22.46824998349841835137178623D0,&
      !         25.07256077242620379439608621D0,&
      !         27.84748000916886272075170414D0,&
      !         30.80014573944546270075438520D0,&
      !         33.93865708491371960909885859D0,&
      !         37.27224588047600432832076099D0,&
      !         40.81149282388692046615567558D0,&
      !         44.56860317533446270712302063D0,&
      !         48.55776353305999228096204881D0,&
      !         52.79561118721693296935202114D0,&
      !         57.30186332339362749503374700D0,&
      !         62.10017907277511161216819906D0,&
      !         67.21937092712699879908027755D0,&
      !         72.69515884761246211752192772D0,&
      !         78.57280291157130928054389683D0,&
      !         84.91123113570498454270156471D0,&
      !         91.78987467123637699233719348D0,&
      !         99.32080871744680825010905416D0,&
      !         107.6724406393882725207967676D0,&
      !         117.1223095126906888076506441D0,&
      !         128.2018419882556511925411044D0,&
      !         142.2800444691599978883488354D0/)
      !      REAL*8, DIMENSION(1:ndGL), PARAMETER:: w = (/ &
      !         0.091625471157459897311511698080137483D0,&
      !         0.213420584905012080007193367121512341D0,&
      !         0.335718116680284673880510701616292191D0,&
      !         0.458540935033497560385432380376452497D0,&
      !         0.582068165779105168990996365401543284D0,&
      !         0.706495216367219392989830015673016682D0,&
      !         0.832026903003485238099112947978349524D0,&
      !         0.958878198794443111448122679676028907D0,&
      !         1.087276162030549715753869333172026616D0,&
      !         1.217462327977780978954277850665609481D0,&
      !         1.349695491356765307923938594423945197D0,&
      !         1.484254929776846711205611786129787199D0,&
      !         1.621444162811821978023168843164545274D0,&
      !         1.761595374676769611184242204209815984D0,&
      !         1.905074665894799676682993205972793711D0,&
      !         2.052288347261716717601995822729474548D0,&
      !         2.203690553245095889098283443281405708D0,&
      !         2.359792538523203323540373753789014974D0,&
      !         2.521174140376432991653136902874228210D0,&
      !         2.688498055408842264159505447063746595D0,&
      !         2.862527813210448812034763959831043113D0,&
      !         3.044150665311517100410439679543336704D0,&
      !         3.234407097263531941774902394288671117D0,&
      !         3.434529398427748092203984818916024650D0,&
      !         3.645992824994089072389656466994904344D0,&
      !         3.870584597216516568084753202134443384D0,&
      !         4.110498680432822655835822472639515779D0,&
      !         4.368468723254063474508083382729450258D0,&
      !         4.64795898407446688299303399883883991D0, &
      !         4.95344611240989326218696150785562721D0, &
      !         5.29084840590073657468737365718858969D0, &
      !         5.66820460903297677000730529023263795D0, &
      !         6.09679641474342030593376010859198806D0, &
      !         6.59310886103999953794429664206294899D0, &
      !         7.18249599553689315064429801626699575D0, &
      !         7.90666631138422877369310742310586595D0, &
      !         8.84089249281034652079125595063026792D0, &
      !         10.1408992656211694839094600306940469D0, &
      !         12.2100212992046038985226485875881110D0, &
      !         16.7055206420242974052468774398573550D0/)

      nu_in =  outchnl(in)%eef/2.D0      ! half of the degree of freedom for the incoming channel
      nu_ou =  outchnl(ou)%eef/2.D0      ! half of the degree of freedom for the incoming channel
      WFC2 = 0.D0
      do i = 1, ndGL        ! do loop over Gauss Laguerre integration steps
            tmp = (1.D0 + x(i)*outchnl(ou)%t/nu_ou/H_Sumtl)*(1.D0 + x(i)*outchnl(in)%t/nu_in/H_Sumtl)
            WFC2 = WFC2 + w(i)/(save_WFC1(i)*tmp)
      end do
      if(in==ou) then                       ! case of elastic
         WFC2 = (1.D0 + 1.D0/nu_in)*WFC2
      end if

      return
      end function WFC2



   END MODULE width_fluct
