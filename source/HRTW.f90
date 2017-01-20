MODULE width_fluct

   !
   !   ********************************************************************
   !   *                  W I D T H _ F L U C T                           *
   !   *                                                                  *
   !   * Calculates Hauser-Fesbach with width fluctuation correction      *
   !   * using HRTW and Moldauer approaches including angular distr.      *
   !   * In presence of direct reactions proper transmission coefficients *
   !   * are used for the incoming as well as outging channels.           *
   !   * Engelbrecht-Weidenmueller tranformation is used to account for   *
   !   * direct reactions' effect on compound nucleus emission. (in work) *
   !   *                                                                  *
   !   *                                                                  *
   !   ********************************************************************
   !

   USE angular_momentum
   USE TLJs

   IMPLICIT NONE

   INCLUDE 'dimension.h'
   INCLUDE 'global.h'

   PRIVATE

   ! $Rev: 4825 $
   ! $Author: gnobre $
   ! $Date: 2017-01-20 21:36:27 +0100 (Fr, 20 Jän 2017) $
   !

   TYPE channel
      INTEGER l         ! ejectile l
      REAL*8 j            ! ejectile j
      REAL*8 t            ! ejectile Tlj
      REAL*8 ti1          ! temporary Tlj used for iteration
      REAL*8 ti2          ! temporary Tlj used for iteration
      REAL*8 rho          ! final level density for this channel
      REAL*8 eef          ! elastic enhancement factor
      INTEGER nejc      ! ejectile index (nejc)
      INTEGER kres      ! populated energy bin (negative for discrete levels, g.s. -1,...)
      REAL*8 xjrs         ! spin of the populated state
      INTEGER jres      ! spin index of the populated state
      INTEGER pres      ! parity index of the populated state
   END TYPE channel

   TYPE numchnl
      INTEGER neut     ! number of neutron channels, i.e., number of neutron entries in the 'channel' type
      INTEGER part     ! number of particle channels, i.e., number of particle entries in the 'channel' type
      INTEGER coll     ! position of the first (low) coupled level channel
      INTEGER colh     ! position of the last coupled channel;
                          ! coupled channels are embedded in particle channels coll <= colh <=part
      INTEGER elal     ! position of the first (low) elastic channel
      INTEGER elah     ! position of the last elastic channel; elastics are embedded in particle channels elal<=elah<=part
      INTEGER fiss     ! effective number of fission channels
      INTEGER gamm     ! effective number of gamma channels
   END TYPE numchnl

   TYPE fusion
      INTEGER nout      ! position of the corresponding outgoing channel in outchnl
      INTEGER l         ! projectile l
      REAL*8 j          ! projectile j
      REAL*8 t          ! projectile Tlj
      REAL*8 sig        ! absorption x-section for this channel
   END TYPE fusion

   INTEGER, PARAMETER :: ndhrtw1 = 20000        ! max. number of channels in the HRTW decay for a given CN J-pi
   INTEGER, PARAMETER :: ndhrtw2 = 500          ! max. number of absorption channels for a given CN J-pi

   REAL*8 :: H_Sumtl      ! Sum of strong Tlj
   REAL*8 :: H_Sumtls     ! Sum of strong Tlj**2
   REAL*8 :: H_Sweak      ! Sum of weak Tlj
   REAL*8 :: H_Sweaks     ! Sum of weak Tlj**2
   REAL*8 :: sumg         ! Sum of gamma channels
   REAL*8 :: H_Tav        ! Avarage strong Tlj
   REAL*8 :: H_Tthr       ! Thershold for Tlj to be considered strong
   REAL*8 :: TFIs         ! Sum of fission transmission coefficients
   REAL*8 :: TGam         ! Sum of gamma transmission coefficients
   INTEGER :: NCH         ! Number of strong channels (Tlj's)
   ! INTEGER :: NSCh        ! Number of strong  Tlj processed by VT routine, i.e. position in H_Tl matrix

   REAL*8, ALLOCATABLE :: H_Tl(:,:)                      ! strong transmission coefficients LIKELY TO GET RID OFF!!!
   REAL*8, ALLOCATABLE :: H_Abs(:,:)
   TYPE(channel), ALLOCATABLE, TARGET :: outchnl(:)      ! outgoing channels
   TYPE(fusion),  ALLOCATABLE, TARGET :: inchnl(:)       ! fusion channels
   TYPE(numchnl) :: num                                  ! number of particular channels

   REAL*8 :: save_WFC1(41)                               ! stores central part of the Moldauer integral

   REAL*8, ALLOCATABLE :: WFC(:,:)                       ! for Moldauer integral
   REAL*8, ALLOCATABLE :: SCRt_mem(:,:,:,:), SCRtl_mem(:,:)      ! preserve weak transitions in Moldauer looping over elastic channels for traget spin >0

   !  Data (x) for Gauss-Legendre quadrature from 0 to 1
   REAL*8, DIMENSION(1:41), PARAMETER:: xgk = (/ &
      5.7048420586114368D-004,&
      3.4357004074525577D-003,&
      9.2460612748748727D-003,&
      1.8014036361043095D-002,&
      2.9588683084122602D-002,&
      4.3882785874337027D-002,&
      6.0861594373859018D-002,&
      8.0441514088890609D-002,&
      1.0247928558122438D-001,&
      1.2683404676992460D-001,&
      1.5338117183262429D-001,&
      1.8197315963674249D-001,&
      2.1242977659014484D-001,&
      2.4456649902458644D-001,&
      2.7820341238063745D-001,&
      3.1314695564229023D-001,&
      3.4918606594254353D-001,&
      3.8610707442917747D-001,&
      4.2369726737953867D-001,&
      4.6173673943325133D-001,&
      5.0000000000000000D-001,&
      5.3826326056674867D-001,&
      5.7630273262046128D-001,&
      6.1389292557082253D-001,&
      6.5081393405745647D-001,&
      6.8685304435770977D-001,&
      7.2179658761936261D-001,&
      7.5543350097541362D-001,&
      7.8757022340985516D-001,&
      8.1802684036325757D-001,&
      8.4661882816737566D-001,&
      8.7316595323007540D-001,&
      8.9752071441877557D-001,&
      9.1955848591110945D-001,&
      9.3913840562614093D-001,&
      9.5611721412566297D-001,&
      9.7041131691587745D-001,&
      9.8198596363895696D-001,&
      9.9075393872512518D-001,&
      9.9656429959254744D-001,&
      9.9942951579413886D-001 /)
   !
   REAL*8, DIMENSION(1:41), PARAMETER:: wgk = (/ &
      0.003073583718520531501218293246031D0,&
      0.008600269855642942198661787950102D0,&
      0.014626169256971252983787960308868D0,&
      0.020388373461266523598010231432755D0,&
      0.025882133604951158834505067096153D0,&
      0.031287306777032798958543119323801D0,&
      0.036600169758200798030557240707211D0,&
      0.041668873327973686263788305936895D0,&
      0.046434821867497674720231880926108D0,&
      0.050944573923728691932707670050345D0,&
      0.055195105348285994744832372419777D0,&
      0.059111400880639572374967220648594D0,&
      0.062653237554781168025870122174255D0,&
      0.065834597133618422111563556969398D0,&
      0.068648672928521619345623411885368D0,&
      0.071054423553444068305790361723210D0,&
      0.073030690332786667495189417658913D0,&
      0.074582875400499188986581418362488D0,&
      0.075704497684556674659542775376617D0,&
      0.076377867672080736705502835038061D0,&
      0.076600711917999656445049901530102D0,&
      0.076377867672080736705502835038061D0,&
      0.075704497684556674659542775376617D0,&
      0.074582875400499188986581418362488D0,&
      0.073030690332786667495189417658913D0,&
      0.071054423553444068305790361723210D0,&
      0.068648672928521619345623411885368D0,&
      0.065834597133618422111563556969398D0,&
      0.062653237554781168025870122174255D0,&
      0.059111400880639572374967220648594D0,&
      0.055195105348285994744832372419777D0,&
      0.050944573923728691932707670050345D0,&
      0.046434821867497674720231880926108D0,&
      0.041668873327973686263788305936895D0,&
      0.036600169758200798030557240707211D0,&
      0.031287306777032798958543119323801D0,&
      0.025882133604951158834505067096153D0,&
      0.020388373461266523598010231432755D0,&
      0.014626169256971252983787960308868D0,&
      0.008600269855642942198661787950102D0,&
      0.003073583718520531501218293246031D0/)

   PUBLIC HRTW, Moldauer

   PRIVATE QDIAG

CONTAINS

   !----------------------------------------------------------------------------------------------------

   SUBROUTINE AllocHRTW(nd1,nd2)
      IMPLICIT NONE
      INTEGER, INTENT(IN), OPTIONAL :: nd1, nd2
      INTEGER my,ndch,ndfus, ndcc

      ndch = ndhrtw1
      ndcc = ndhrtw1/10
      ndfus = ndhrtw2
      IF(present(nd1)) ndch = nd1
      IF(present(nd1)) ndcc = nd1/10
      IF(present(nd2)) ndfus = nd2

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

      IF(allocated(inchnl)) DEALLOCATE(inchnl)
      ALLOCATE(inchnl(ndfus),STAT=my)
      IF(my /= 0) GOTO 10
      inchnl%nout = 0
      inchnl%l    = 0
      inchnl%j    = 0.d0
      inchnl%t    = 0.d0
      inchnl%sig  = 0.d0

      IF(allocated(WFC)) DEALLOCATE(WFC)
      ALLOCATE(WFC(ndfus,ndch),STAT=my)
      IF(my /= 0) CALL WFC_error()

      !  clones of scratch matrices to preserve weak channels in Moldauer WFC
      IF(allocated(SCRt_mem)) DEALLOCATE(SCRt_mem)
      ALLOCATE(SCRt_mem(NDEX,NDLW,2,0:NDEJC),STAT=my)
      IF(my /= 0) CALL WFC_error()

      IF(allocated(SCRtl_mem)) DEALLOCATE(SCRtl_mem)
      ALLOCATE(SCRtl_mem(NDLV,0:NDEJC),STAT=my)
      IF(my /= 0) CALL WFC_error()

      RETURN

10    WRITE(8,*)  'ERROR: Insufficient memory for HRTW'
      WRITE(12,*) 'ERROR: Insufficient memory for HRTW'
      STOP 'ERROR: Insufficient memory for HRTW'
      RETURN

   END SUBROUTINE AllocHRTW

   !----------------------------------------------------------------------------------------------------

   SUBROUTINE DelHRTW()

      IMPLICIT NONE

      IF(allocated(H_Tl))    DEALLOCATE(H_Tl)
      IF(allocated(H_Abs))   DEALLOCATE(H_Abs)
      IF(allocated(outchnl)) DEALLOCATE(outchnl)
      IF(allocated(inchnl))  DEALLOCATE(inchnl)
      IF(allocated(WFC))     DEALLOCATE(WFC)
      IF(allocated(SCRt_mem))     DEALLOCATE(SCRt_mem)
      IF(allocated(SCRtl_mem))    DEALLOCATE(SCRtl_mem)

      RETURN
   END SUBROUTINE DelHRTW

   !----------------------------------------------------------------------------------------------------

   SUBROUTINE HRTW

      !cc
      !cc   ********************************************************************
      !cc   *                                                         class:ppu*
      !cc   *                         H R T W                                  *
      !cc   *                                                                  *
      !cc   *  Calculate decay of the Compound Nucleus capture states in       *
      !cc   *  terms of the HRTW theory (width fluctuation correction).        *
      !cc   *                                                                  *
      !cc   *                                                                  *
      !cc   ********************************************************************
      !cc

      IMPLICIT NONE
      INCLUDE 'dimension.h'
      INCLUDE 'global.h'


      REAL*8 :: elada(NDAngecis), elleg(NDAngecis)
      INTEGER neles
      COMMON /angula/elada,elleg,neles
      REAL*8 :: sumin_w, sumtt_w
      COMMON /EWcorr/ sumin_w, sumtt_w

      CHARACTER(1), PARAMETER :: cpar(2) = (/'+', '-'/)

      ! Local variables

      LOGICAL*4 relcal
      INTEGER i, ip, ipar, jcn, ke, m, nejc, nhrtw, nnuc, nnur, itmp
      REAL*8 cnspin, fisxse, summa, sumfis, sumg, sumtg, tgexper, xnor, elcor, xjc
	  REAL*8 sumfism(nfmod)
      REAL*8 Ia, sxj
      REAL*8 xmas_npro, xmas_ntrg, el, ecms, ak2
      REAL*8 d0c, sigma_
      REAL*8 sumin_s, sumtt_s

      TYPE (channel), POINTER :: out
      TYPE (fusion),  POINTER :: in

      CALL AllocHRTW()    !allocate HRTW matrices

      H_Tthr = 1.0D-6     !threshold for considering channel to be a 'strong' one
      nnuc = 1            !set CN nucleus
      csfis = 0.D0
      sumfis = 0.D0
      gcasc = 1.0         !ensure full gamma cascade when HRTW
      ke = NEX(nnuc)

      sxj = SEJc(0)
      Ia  = XJLv(LEVtarg,0)

      ! Initialize variables and print heading for normalizing g-strength function
      d0c = 0.D0
      sumtg = 0.D0
      tgexper = 0.D0
      IF(.NOT. benchm) THEN
         WRITE(8,'(1x,''Renormalization of gamma-ray strength function'')')
         WRITE(8,'(1x,''-------------------------------------------------------------'')')
      ENDIF
      IF(first_ein .AND. (einl>1.D0) ) THEN
         WRITE(8,'(1x,'' WARNING: First incident energy Einc must be < 1 MeV for Do and Gg calculations and'')')
         WRITE(8,'(1x,'' WARNING: for the renormalization of gamma-ray strength function'')')
      ENDIF

      xmas_npro = EJMass(0)
      xmas_ntrg = AMAss(0)
      el = EINl

      relcal = .FALSE.
      IF(IRElat(0,0)>0 .OR. RELkin) relcal = .TRUE.
      IF (AEJc(0).EQ.0.0D0) THEN
         xmas_npro = 0.d0
         relcal = .TRUE.
      ENDIF
      CALL kinema(el,ecms,xmas_npro,xmas_ntrg,ak2,1,relcal)
      ! write(*,*) 'HRTW=',10.D0*PI/ak2,el,IRElat(0,0),RELKIN,relcal

      coef = 1.d0
      IF (AEJc(0)>0) coef = 10.D0*PI/ak2/(2.D0*Ia + 1.d0)/(2.D0*sxj + 1.d0)

      !----------------------------------------------------------
      ! start CN nucleus decay
      !----------------------------------------------------------
      DO ipar = 1, 2                                      ! do loop over decaying nucleus parity
         ip = 1 - 2*abs(mod(ipar+1,2))                    ! actual parity of the state (+1 or -1)
         DO jcn = 1, nlw                                  ! do loop over decaying nucleus spin
            xjc = float(jcn) + HIS(nnuc)
            IF(POP(ke,jcn,ipar,nnuc)<=1.0d-15) CYCLE      ! skip if absorption cross section negligible
            ! write(8,*) ' '
            ! write(8,*) 'CN Jpi=',xjc*ip
            nhrtw = 0
            DENhf = 0.D0

			CALL zeroing_module_vars()
            IF(gdrdyn==1.0D0) CALL ULMDYN(nnuc,jcn,EX(ke,nnuc)) ! prepare GDR parameters (if spin dependent GDR selected)

            !----------------------------------------------------------
            ! Collecting outgoing channels
            !----------------------------------------------------------

            !----------------------------------------------------------
            ! particle decay
            !----------------------------------------------------------
            DO nejc = 1, nejcm                            !do loop over ejectiles
               IF(NREs(nejc)<0) CYCLE
               nnur = NREs(nejc)
               !write(8,*) 'HRTW-decay ejectile:',nejc
               summa = WFC_DECAY(nnuc,ke,jcn,ip,nnur,nejc)
            ENDDO                                         !do loop over ejectiles  ***done***
            ! write(*,*) sumin_w,sumtt_w

            if(num%elal == 0) EXIT ! if there are no elastic channels, we can exit the inner "jcn" loop  
            num%part = NCH                                !store number of particle channel entries

            !----------------------------------------------------------
            ! gamma decay (weak channels) (one iteration)
            !----------------------------------------------------------
            sumg = WFC_DECAYG(nnuc,ke,jcn,ip)

            !----------------------------------------------------------
            ! Fission (may be a weak or strong channel)
            !----------------------------------------------------------
            sumfis = WFC_DECAYF(nnuc,ke,jcn,ip)
            
            IF(H_Sumtl.LE.0.0D0) CYCLE

            H_Tav = H_Sumtls/H_Sumtl ! average transmission coefficient (Sum(T**2)/Sum(T))

            !  write(*,*)' '
            !  write(*,*)'SUMMARY OF DECAY FOR J=',xjc
            !  write(*,*)'total sum of  Tls ', H_Sumtl
            !  write(*,*)'sum of strong Tls ', H_Sumtl-H_Sweak
            !  write(*,*)'sum of weak   Tls ', H_Sweak
            !  write(*,*)'sum of weak part. ', H_Sweak-sumg
            !  write(*,*)'sum of gammas     ', sumg
            !  write(*,*)'sum fission       ', sumfis
            !  write(*,*)'sum Tl**2         ', H_Sumtls
            !  write(*,*)'# of strong Tls   ', NCH
            !  write(*,*)'average Tl        ', H_Tav
            !  write(*,*)'pre  AUSTER DENhf=', DENhf

            !----------------------------------------------------------
            ! Collecting outgoing channels completed
            !----------------------------------------------------------
            IF(LHRtw==1 .OR. LHRtw==2) THEN
			  CALL AUSTER(LHRtw)  ! calculate V's for the strong channels (iteration)
              DENhf = H_Sumtl     ! reset DENhf using V's instead of T's
            ENDIF
			IF(DENhf .LE. 0.d0) CYCLE

            !----------------------------------------------------------------------------------
            ! construct scratch matrix for decay of the Jcn state
            !----------------------------------------------------------------------------------
            sumin_s = 0.d0
            sumtt_s = 0.d0
            ! write(*,*) ' NProject=',NPRoject,' LEVtarg=',LEVtarg
          
            DO i = 1, num%part  !scan strong particle channels (note: weak channels are already in SCRt)
               out => outchnl(i)
			   sigma_  = out%t
			   CALL update_SCRt(out, sigma_, sumin_s, sumtt_s)
            ENDDO

			CALL elastic_corr(sumin_s, sumtt_s, sumtt_w, sumin_w)

            !----------------------------------------------------------
            ! Fission
            !----------------------------------------------------------
            IF(num%fiss>0) sumfis = outchnl(num%fiss)%t*outchnl(num%fiss)%rho  !redefining sumfis to account for the HRTW T=>V transition

            !----------------------------------------------------------
            ! Renormalizing scratch matrices to recover unitarity
            !----------------------------------------------------------
            DENhf = SUM(SCRt)*de + SUM(SCRtl) + sumfis
            DENhf = DENhf - 0.5*SUM(SCRt(1,:,:,:))*de   !correct for the edge effect in trapezoidal integration
            ! write(*,*)'DENhf calculated as integral of SCRt & SCRtl', DENhf

            IF(DENhf.LE.0.0D0) CYCLE ! no transitions from the current state
            !----------------------------------------------------------------------------------
            !Loop over incident channels to calculate cross sections and angular distributions
            !----------------------------------------------------------------------------------
            DO i = num%elal, num%elah                   ! do loop over elastic channels
               in => inchnl(i - num%elal + 1)           ! elastic channels for each Jcn are numbered 1,2,3,...
               out => outchnl(i)
               in%t = out%t

			   IF (INTerf==0) THEN
                 ! absorption ~ sigma_a
                 in%sig = coef*in%t*(2.D0*xjc + 1.D0)*FUSred*REDmsc(jcn,ipar)  ! absorption for incoming channel
                 xnor = in%sig/DENhf ! normalization factor
                 ! write(*,*) 'Jcn, Tlj_in, Tlj_out, coef, sig ', xjc, in%t, out%t, coef, in%sig
               ELSE
                 !xnor = 1.d0/DENhf ! normalization factor
                 xnor = 1.d0
			   ENDIF
			   
               ! write(*,*) 'Jcn, Tlj_in, Tlj_out, coef, sig ', xjc, in%t, out%t, coef, in%sig
               elcor = out%t*(out%eef - 1.D0)     ! elastic channel correction to SCRtl  (elcor=0 for HF)
               ! write(*,*) 'Elcor =', elcor, '  EEF =', out%eef
               SCRtl(-out%kres,out%nejc) = SCRtl(-out%kres,out%nejc) + elcor
               ! write(*,*)'post AUSTER DENhf=', DENhf + elcor

               !xnor = in%sig/DENhf                          ! normalization factor
               !SCRt = SCRt*xnor                             ! normalizing scratch matrices instead of passing xnor to XSECT,
               !SCRtl = SCRtl*xnor                           !   the above helps implementation of the EW transformation that provides
               !SCRtem = SCRtem*xnor                         !   unfactorized cross sections.
               !sumfis = sumfis*xnor                         !                                  "
               !sumfism = sumfism*xnor                         !                                  "

               !----------------------------------------------------------------------------------
               ! CN angular distributions (neutron (in)elastic scattering ONLY!)
               !----------------------------------------------------------------------------------
			   CALL CN_DA_anis(i, in, Ia, sxj, xjc, xnor)

               !CALL XSECT(nnuc,m,1.0D0,sumfis,sumfism,ke,ipar,jcn,fisxse)  !normalize SCRt matrices and store x-sec
               CALL XSECT(nnuc,m,xnor,sumfis,sumfism,ke,ipar,jcn,fisxse)  !normalize SCRt matrices and store x-sec

               out => outchnl(i)
               SCRtl(-out%kres,out%nejc) = SCRtl(-out%kres,out%nejc) - elcor    !restore SCRtl before new elastic is calculated

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

      CALL Gamma_renormalization(d0c, sumtg, tgexper, itmp, nnuc)

      CALL DelHRTW()    !deallocate HRTW arrays
      IF(DIRECT>0 .and. MAX_cc_mod>0) CALL DelTLJs() ! deallocate incident channel TLJs for CC
      !IF(INTerf==0) CALL DelCCmatr() ! deallocate EW matrices

      RETURN
   END SUBROUTINE HRTW

   !----------------------------------------------------------------------------------------------------

   REAL*8 FUNCTION WFC_decay(nnuc,iec,jc,ipc,nnur,nejc)
      !cc
      !cc   ********************************************************************
      !cc   *                                                         class:PPu*
      !cc   *                     W F C  _  D E C A Y                          *
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
      !cc   *                                                                  *
      !cc   *                                                                  *
      !cc   *                                                                  *
      !cc   * calls:TLLOC                                                      *
      !cc   *                                                                  *
      !cc   *                                                                  *
      !cc   *                                                                  *
      !cc   ********************************************************************
      !cc
      use TLJs

      IMPLICIT NONE
      ! COMMON variables
      REAL*8, DIMENSION(ndlw,3) :: ELTLJ
      REAL*8, DIMENSION(ndlw) :: ELTL
      COMMON /ELASTIC/ ELTl,ELTlj
      REAL*8 :: sumin_w, sumtt_w
      COMMON /EWcorr/ sumin_w, sumtt_w
      ! Dummy arguments
      INTEGER, INTENT(IN) :: ipc, jc, nejc, nnuc, nnur, iec
      ! Local variables
      REAL*8 :: eout, eoutc, frde, rho1, jmax, jmin, tld, xjc, xj, xjr, summa, ssxj
      INTEGER :: i, ier, iermax, ietl, iexc, il, ip1, ipar, itlc, jr, k, kmax, kmin, nel, jndex
      TYPE (channel), POINTER :: out
      TYPE (fusion),  POINTER :: in
      summa = 0.D0
      ! clear scratch matrices
      SCRtem(nejc) = H_Sumtl        !temporarily store here entry value of H_Sumtl
      SCRt(:,:,:,nejc) = 0.D0
      iexc = NEX(nnuc) - NEXr(nejc,nnuc)
      itlc = iexc - 5
      iermax = iec - iexc
      xjc = dble(jc) + HIS(nnuc)  !actual spin  of the CN state
      ssxj = 2.d0 + SEJc(nejc)

      IF(iermax>=1) THEN

         !----------------------------------------------------------------------------------------------------
         ! Decay to the continuum
         !----------------------------------------------------------------------------------------------------

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
            kmin = jmin - MAXj(nejc) + ssxj                  ! minimum k=l+1
            kmax = jmax - 1 + ssxj                           ! maximum k=l+1
            kmax = MIN(ndlw, kmax)                           ! ensure we are within dimensions
            DO k = kmin, kmax                                ! do loop over l in Tlj (note that k=l+1)
               ip1 = 2 - (1 + ipc*( - 1)**(k - 1))/2         ! parity index of r.n. state populated by emission with l=k-1
               DO jndex = 1, MAXj(nejc)                      ! do loop over j-index in Tlj
                  xj = k + jndex - ssxj
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
                        NCH = NCH +1
                        IF(NCH>ndhrtw1) CALL WFC_error()    !STOP - insufficient space allocation
                        out => outchnl(NCH)
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
      ENDIF    ! whether continuum exists

      !----------------------------------------------------------------------------------------------------
      ! Decay to discrete levels
      !----------------------------------------------------------------------------------------------------

      SCRtl(:,nejc) = 0.D0
      eoutc = EX(iec,nnuc) - Q(nejc,nnuc)
      sumin_w = 0.d0
      sumtt_w = 0.d0

      DO i = 1, NLV(nnur)             ! do loop over inelastic levels, elastic done after the loop
         IF(IZA(nnur)==IZA(0) .AND. i==levtarg) CYCLE     !skip if elastic
         IF(IZA(nnur)==IZA(0) .AND. (ICOllev(i)>0 .AND. ICOllev(i)<=LEVcc)) CYCLE   !skip coupled levels
         eout = eoutc - ELV(i,nnur)
         IF(eout<0.0D0) EXIT
         CALL TLLOC(nnur,nejc,eout,il,frde)               !find 'il' postion of the Tlj in the ETL matrix and relative mismatch 'frde'
         jmin = abs(XJLv(i,nnur) - xjc)
         jmax = XJLv(i,nnur) + xjc
         kmin = jmin - MAXj(nejc) + ssxj                  !minimum k=l+1
         kmax = jmax - 1  + ssxj                          !maximum k=l+1
         kmax = MIN(ndlw, kmax)                           !ensure we are within dimensions
         DO k = kmin, kmax                                !do loop over l in Tlj (note that real l is k-1)
            ipar = 1 + LVP(i,nnur)*ipc*( - 1)**(k - 1)    !check parity (1 if conserved, 0 if violated)
            IF(ipar==0) CYCLE
            DO jndex = 1, MAXj(nejc)                      !do loop over j-index in Tlj
               xj = k + jndex - ssxj
               IF(xj<jmin .OR. xj>jmax) CYCLE
               rho1 = 1.d0 * TUNe(nejc,nnuc)              !reuse level density variable
               ! IF(IZA(nnur)==IZA(0)) rho1 = CINred(i)   !if inelastic - apply respective scaling
               tld = TLJ(il,k,jndex,nejc) + frde*(TLJ(il + 1,k,jndex,nejc) - TLJ(il,k,jndex,nejc))   !interpolate Tlj
               IF(tld<1.0d-15) CYCLE                      !ignore very small channels
               H_Sumtl = H_Sumtl + tld*rho1
               H_Sumtls = H_Sumtls + tld**2*rho1
               IF(tld>H_Tthr) THEN
                  NCH = NCH + 1                             !we've got non-zero channel
                  IF(NCH>ndhrtw1) CALL WFC_error()          !STOP - insufficient space allocation
                  out => outchnl(NCH)
                  out%l = k-1
                  out%j = xj
                  out%t = tld
                  out%rho = rho1
                  ! out%sig = 0.d0
                  out%nejc = nejc
                  out%kres = -i                               !minus indicates channel leading to a discrete level 'i'
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


      !----------------------------------------------------------------------------------------------------
      !  Decay to coupled levels including the elastic channels
      !----------------------------------------------------------------------------------------------------
      IF(DIRect>0 .and. MAX_cc_mod>0) THEN !TEMPORARY to allow for the old treatment of the direct outgoing channels

         CALL DECAY2CC(xjc, ipc, nejc, nnur)                   ! Decay to collective levels including elastic

      ELSE    ! TEMPORARY to allow for the old treatment of the direct outgoing channels
         !  Old version for collective levels
         IF(IZA(nnur)==IZA(0)) THEN
            DO i = 1, NLV(nnur)                                !do loop over inelastic levels, elastic done after the loop
               IF(i==levtarg) CYCLE                            !skip if elastic
               IF(ICOllev(i)==0 .OR. ICOllev(i)>LEVcc) CYCLE   !skip DWBA coupled levels
               eout = eoutc - ELV(i,nnur)
               IF(eout<0.0D0) EXIT
               CALL TLLOC(nnur,nejc,eout,il,frde)               !find 'il' postion of the Tlj in the ETL matrix and relative mismatch 'frde'
               jmin = abs(XJLv(i,nnur) - xjc)
               jmax = XJLv(i,nnur) + xjc
               kmin = jmin - MAXj(nejc) + ssxj                  !minimum k=l+1
               kmax = jmax - 1  + ssxj                          !maximum k=l+1
               kmax = MIN(ndlw, kmax)                           !ensure we are within dimensions
               DO k = kmin, kmax                                !do loop over l in Tlj (note that real l is k-1)
                  ipar = 1 + LVP(i,nnur)*ipc*( - 1)**(k - 1)    !check parity (1 if conserved, 0 if violated)
                  IF(ipar==0) CYCLE
                  DO jndex = 1, MAXj(nejc)                      !do loop over j-index in Tlj
                     xj = k + jndex - ssxj
                     IF(xj<jmin .OR. xj>jmax) CYCLE
                     rho1 = 1.d0                                !reuse level density variable
                     ! IF(IZA(nnur)==IZA(0)) rho1 = CINred(i)   !if inelastic - apply respective scaling
                     tld = TLJ(il,k,jndex,nejc) + frde*(TLJ(il + 1,k,jndex,nejc) - TLJ(il,k,jndex,nejc))   !interpolate Tlj
                     tld = min(1.d0,tld * TUNe(nejc,nnuc)) 
                     IF(tld<1.0d-15) CYCLE                      !ignore very small channels
                     H_Sumtl = H_Sumtl + tld*rho1
                     H_Sumtls = H_Sumtls + tld**2*rho1
                     NCH = NCH + 1                              !we've got non-zero channel
                     IF(NCH>ndhrtw1) CALL WFC_error()           !STOP - insiufficent space allocation

                     IF(num%coll == 0) THEN
                        num%coll = NCH                          !memorize position of the first coupled level in the 'outchnl' matrix
                        num%colh = NCH                          !set it also as the last one in case there are no more
                     ENDIF
                     IF(NCH > num%colh) num%colh = NCH          !in case of another coupled level augment position of last coupled channel

                     out => outchnl(NCH)
                     out%l = k-1
                     out%j = xj
                     out%t = tld
                     out%rho = rho1
                     out%nejc = nejc
                     out%kres = -i                              !minus indicates channel leading to a discrete level 'i'
                     out%xjrs = XJLv(i,nnur)
                     out%pres = LVP(i,nnur)
                     ! WRITE(8,'(3x,A,2x,2(f5.1,1x),i2,1x,f7.3,1x,i6)')'xjc, xj, ipar, ELV(i,nnur), NCH', xjc, xj, ipar, ELV(i,nnur), NCH
                  ENDDO     !do loop over 'jndex'    --------- done --------------------
               ENDDO    !do loop over 'l'            --------- done --------------------
            ENDDO    ! do loop over inelastic levels --------- done --------------------


            ! elastic channel (old version)

            i = levtarg
            eout = eoutc - ELV(i,nnur)
            IF(eout<0.0D0) GOTO 10
            jmin = abs(XJLv(i,nnur) - xjc)
            jmax = XJLv(i,nnur) + xjc
            kmin = jmin - MAXj(nejc) + ssxj                  !minimum k=l+1
            kmax = jmax - 1 + ssxj                           !maximum k=l+1
            kmax = MIN(ndlw, kmax)                           !ensure we are within dimensions
            DO k = kmin, kmax                                !do loop over k in Tlj (note that real l is k-1)
               !         ipar = 1 + LVP(i,nnur)*ipc*( - 1)**(k - 1)    !check parity
               !         ipar = PAR(ipc,LVP(LEVtarg,nnur),k - 1)
               ipar = (1.d0 - (-1.d0)*ipc*LVP(i,nnur)*(-1)**(k-1))/2.d0
               IF(ipar==0) CYCLE
               DO jndex = 1, MAXj(nejc)                      !do loop over j-index in Tlj
                  xj = k + jndex - ssxj
                  IF(xj<jmin .OR. xj>jmax) CYCLE
                  tld = ELTLJ(k,jndex)                       !no IF - all elastic channels treated as 'strong'
                  NCH = NCH + 1
                  IF(NCH>ndhrtw1) CALL WFC_error()          !STOP - insufficient space allocation
                  IF(num%elal == 0) THEN
                     num%elal = NCH                          !memorize position of the first elastic in the 'outchnl' matrix
                     num%elah = NCH                          !set it also as the last one in case there are no more
                  ENDIF
                  IF(NCH > num%elah) num%elah = NCH          !if another elastic augment position of last elastic channel
                  rho1 = CELred
                  out => outchnl(NCH)
                  out%l = k-1
                  out%j = xj
                  out%t = tld
                  out%rho = rho1
                  out%nejc = nejc
                  out%kres = -i    !minus indicates that this is a channel leading to a discrete level 'i'
                  out%xjrs = XJLv(i,nnur)
                  out%pres = LVP(i,nnur)

                  nel = NCH - num%elal + 1         !setting correspondence between 'nch' and elastic numbering 'nel'
                  in => inchnl(nel)
                  in%nout = NCH                    !setting incident channel
                  in%l = k-1                       !setting incident channel
                  in%j = xj                        !          "
                  in%t = tld                       !          "
                  h_sumtl = h_sumtl + tld*rho1
                  h_sumtls = h_sumtls + tld**2*rho1
               ENDDO                 ! do loop over jndex --- done -------
            ENDDO                    ! loop over 'l' ------ done ---------
         ENDIF !end of elastic
      ENDIF !TEMPORARY to allow for the old treatment of the direct outgoing channels
      ! decay to discrete levels --------- done --------------------

10    summa = H_Sumtl - SCRtem(nejc)     !we may NOT NEED IT
      SCRtem(nejc) = summa               !we may NOT NEED IT
      DENhf = DENhf + summa
      IF(nejc==1) num%neut = NCH         !store number of neutron-out channels
      ! decay to the continuum and discrete levels ------ done -----------------------------

      WFC_decay = summa
      RETURN
   END FUNCTION WFC_decay

   !----------------------------------------------------------------------------------------------------

   SUBROUTINE DECAY2CC(xjc, ipc, nejc, nnur)
      !
      !********************************************************************
      !*                                                         class:PPu*
      !*                    D E C A Y 2 C C                               *
      !*                                                                  *
      !* Fills up outchnl and inchnl structures for particle decay        *
      !* to the coupled levels. It also allocates and prepares            *
      !* matrices for the Engelbrecht-Weidenmueller (EW) transformation.  *
      !*                                                                  *
      !* input:xjc  - spin index of the decaying CN state                 *
      !*       IPC  - parity of the decaying CN state (+1 or -1)          *
      !*                                                                  *
      !*                                                                  *
      !* output:      outchnl(:) outgoing channel structure (list)        *
      !*               inchnl(:) incident channel structure (list)        *
      !*                                                                  *
      !* calls:Prepare_CCmatr                                             *
      !*                                                                  *
      !********************************************************************
      INTEGER i, ipc, nel, nnur, nejc, ncc, nccp, nccu, ndim
      REAL*8 tld, xjc
      TYPE (channel), POINTER :: out
      TYPE (fusion),  POINTER :: in

      IF(IZA(nnur)/=IZA(0)) RETURN

      CALL Prepare_CCmatr(xjc, ipc, ncc, nccp, nccu, ndim)    ! open  CC P-diagonal and U-matrix for EW transformation

      IF(ndim==0) RETURN                                      ! no collective channels found
	  
      ! write(*,*) 'After Prepare_CCmatrix: xjc, ipc, ncc, nccp, nccu, ndim',sngl(xjc), ipc, ncc, nccp, nccu, ndim
      DO i = ncc, nccp
         !        write(*,*) i,ncc,nccp,sngl(xjc)
         tld = Pdiag(i-ncc+1)                        ! use Tlj in diagonalized space Pdiag if EW transformation is requested
         H_Sumtl = H_Sumtl + tld
         H_Sumtls = H_Sumtls + tld**2
         NCH = NCH + 1                                !we've got non-zero channel
         IF(nch>ndhrtw1) CALL WFC_error()             !STOP - insufficient space allocation
         IF(num%coll == 0) THEN
            num%coll = NCH                            !memorize position of the first coupled level in the 'outchnl' matrix
            num%colh = NCH                            !set it also as the last one in case there are no more
         ENDIF
         IF(NCH > num%colh) num%colh = NCH            !in case of another coupled level augment position of last coupled channel
         out => outchnl(NCH)
         out%l = STLcc(i)%l
         out%j = STLcc(i)%j
         out%t = tld
         out%rho = 1.0D0
         out%nejc = nejc
         out%kres = -STLcc(i)%lev                      !minus indicates channel leading to a discrete level 'i'
         out%xjrs = XJLv(STLcc(i)%lev,nnur)
         out%pres = LVP(STLcc(i)%lev,nnur)

         IF(INTerf==0) THEN  

           IF(STLcc(i)%lev==levtarg) THEN                ! we've got elastic!
             IF(num%elal == 0) THEN
               num%elal = NCH                          !memorize position of the first coupled level in the 'outchnl' matrix
               num%elah = NCH                          !set it also as the last one in case there are no more
             ENDIF
             IF(NCH > num%elah) num%elah = NCH          !in case of another coupled level augment position of last coupled channel
             nel = NCH - num%elal + 1         !setting correspondence between 'nch' and elastic numbering 'nel'
             in => inchnl(nel)
             in%nout = NCH                    !setting incident channel
             in%l = out%l                     !setting incident channel
             in%j = out%j                     !setting incident channel
             in%t = tld                       !setting incident channel
           ENDIF

		 ELSE
		   ! EW transformed space               
		   ! in the transformed space (INTerf>0) all coupled channels are considered elastic
           nel = NCH - num%coll + 1         !setting correspondence between 'nch' and elastic numbering 'nel'
           in => inchnl(nel)
           in%nout = NCH                    !setting incident channel
           in%l = out%l                     !setting incident channel
           in%j = out%j                     !setting incident channel
           in%t = tld                       !setting incident channel

         ENDIF

      ENDDO
      IF (INTerf>0) THEN
        num%elal = num%coll
        num%elah = num%colh
      ENDIF
	   
   END SUBROUTINE DECAY2CC

   !----------------------------------------------------------------------------------------------------

   REAL*8 FUNCTION WFC_decayg(nnuc,iec,jc,ipc)
      !
      !********************************************************************
      !*                                                         class:PPu*
      !*                    W F C _ D E C A Y G                           *
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

      INTEGER, INTENT(IN)  :: iec, ipc, jc, nnuc

      ! Local variables

      REAL*8 :: cee, cme, eg, ha, hscrtl, hsumtls, scrtneg
      REAL*8 :: e1, e2, xm1, scrtpos, xjc, xjr, summa
      INTEGER i, ier, ineg, iodd, ipar, ipos, j, jmax, jmin, jr, lamb, lambmax, lambmin, kmax, kmin
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
      WFC_decayg = 0.D0

      ! clear scratch matrix (continuum)

      !DO j = 1, ndlw    ! NLW
      !   DO i = 1, ndex !NEX(Nnuc)
      !      scrt(i,j,1,0) = 0.D0
      !      scrt(i,j,2,0) = 0.D0
      !   ENDDO
      !ENDDO

      scrt(1:ndex,1:ndlw,1:2,0) = 0.D0

      ! clear scratch matrix (discrete levels)
      !DO i = 1, ndlv ! NLV(Nnuc)
      !   scrtl(i,0) = 0.D0
      !ENDDO
      scrtl(1:ndlv,0) = 0.D0

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

      SCRtem(0) = summa
      DENhf = DENhf + summa
      H_Sumtl = H_Sumtl + summa
      H_Sweak = H_Sweak + summa

      WFC_decayg = summa

      RETURN
   END FUNCTION WFC_decayg

   !----------------------------------------------------------------------------------------------------


   REAL*8 FUNCTION WFC_decayf(nnuc,iec,jcn,ip)
      !
      !********************************************************************
      !*                                                         class:PPu*
      !*                    W F C _ D E C A Y F                           *
      !*                (function to function version)                    *
      !*                                                                  *
      !* Calculates fission decay in nucleus NNUC                         *
      !*                                                                  *
      !* input:NNUC - decaying nucleus index                              *
      !*       NCH  - outgoing channel index                              *
      !*       IEC  - energy index of the decaying state                  *
      !*       JC   - spin index of the decaying state                    *
      !*       IPC  - parity of the decaying state (+1 or -1)             *
      !*                                                                  *
      !*                                                                  *
      !* output:      Sum of fission transmission coefficients over all   *
      !*              fission channels.                                   *
      !*              Apart of this standard feature it comunicates       *
      !*              quantities needed for the HRTW model through        *
      !*              the HRTW_mod module.                                *    
      !*                                                                  *
      !* calls:none                                                       *
      !*                                                                  *
      !********************************************************************

      IMPLICIT NONE

      ! Dummy arguments

      INTEGER, INTENT(IN)  :: iec, ip, jcn, nnuc

      ! Local variables
      REAL*8 :: sumfis
      REAL*8 sumfism(nfmod) 
      REAL*8 :: tfis
      INTEGER ndivf

      WFC_decayf = 0.d0

      IF(.not.FISsil(nnuc)) RETURN

      sumfis = 0.D0
      tfis = 0.D0
      ndivf = 1
      num%fiss = 0
 
      IF(nint(FISshi(nnuc))==1) THEN
        CALL FISSION(nnuc,iec,jcn,sumfis)
      ELSE
        CALL FISCROSS(nnuc,iec,ip,jcn,sumfis,sumfism)
      ENDIF

	  IF(sumfis<=0.d0) RETURN

      H_Sumtl = H_Sumtl + sumfis

      ! dividing sumfis into channels with TFIs < 0.25 each
      ndivf = int(sumfis/0.25) + 1
      tfis = sumfis/dfloat(ndivf)
      H_Sumtls = H_Sumtls + tfis**2*dfloat(ndivf)
      IF(tfis>=H_Tthr) THEN                      ! fission treated as a strong channel
        NCH = NCH + 1                           ! otherwise 'sumfis' it is left untouched
        num%fiss = NCH                          ! store position of fission (only one entry with 'ndivf')
        outchnl(NCH)%t = tfis
        outchnl(NCH)%rho = dfloat(ndivf)
        outchnl(NCH)%nejc = 100                 ! nejc=100 convention identifies fission
      ENDIF

      WFC_decayf = sumfis

      RETURN
   END FUNCTION WFC_decayf



   SUBROUTINE WFC_error()

      WRITE(8,*) 'Insufficient space allocated for HRTW'
      WRITE(8,*) 'ndhrtw1 in HRTW.f90 needs to be increased'
      WRITE(*,*) 'Insufficient space allocated for HRTW'
      WRITE(*,*) 'ndhrtw2 in HRTW.f90 needs to be increased'
      STOP 'Insufficient space allocated for HRTW'

   END SUBROUTINE WFC_error

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
      INTEGER, INTENT(IN) :: LHRtw

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
            eef = 1.D0 + 2.D0/NU(tl,sumtl,2)
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

      INTEGER, INTENT(IN) :: Lhrtw
      INTEGER i, icount

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
      !cc  *  fluctuation correction.                                         *
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
      INTEGER i, ip, ipar, jcn, ke, m, nejc, nhrtw, nnuc, nnur, itmp
      REAL*8 cnspin, fisxse, summa, sumfis, sumtg, tgexper, xnor, xjc, coef, sxj
      REAL*8 Ia
      REAL*8 xmas_npro, xmas_ntrg, el, ecms, ak2
      REAL*8 d0c, sumfis_mem
      REAL*8 sumfism(nfmod) 
      REAL*8 sumin_s, sumtt_s, w, dtmp
      REAL*8 nu_ialph, nu_ibeta, sigma_ , sigma_EW, epsil
      INTEGER ialph,ibeta,iaa,ibb,ialph_ch,ibeta_ch,IER
	  REAL*8 deg_alph,deg_beta
      COMPLEX*16 ctmp1, phas
	  DATA epsil/1.d-12/
	  
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

      sxj = SEJc(0)
      Ia  = XJLv(LEVtarg,0)

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

      xmas_npro = EJMass(0)
      xmas_ntrg = AMAss(0)
      el = EINl

      relcal = .FALSE.
      IF(IRElat(0,0)>0 .OR. RELkin) relcal = .TRUE.
      IF (AEJc(0).EQ.0.0D0) THEN
         xmas_npro = 0.d0
         relcal = .TRUE.
      ENDIF

      CALL kinema(el,ecms,xmas_npro,xmas_ntrg,ak2,1,relcal)

      ! write(*,*) 'HRTW=',10.D0*PI/ak2,el,IRElat(0,0),RELKIN,relcal
      coef = 1.d0
      IF (AEJc(0)>0) coef = 10.D0*PI/ak2/(2.D0*Ia + 1.d0)/(2.D0*sxj + 1.d0)

      !----------------------------------------------------------
      ! start CN nucleus decay
      !----------------------------------------------------------
      DO ipar = 1, 2                                      ! do loop over decaying nucleus parity
         ip = 1 - 2*abs(mod(ipar+1,2))                    ! actual parity of the state (+1 or -1)
         DO jcn = 1, nlw                                  ! do loop over decaying nucleus spin
            xjc = float(jcn) + HIS(nnuc)
            IF(POP(ke,jcn,ipar,nnuc)<=1.0d-15) CYCLE      ! skip if absorption cross section negligible
            ! write(8,*) ' '
            ! write(8,*) 'CN Jpi=',xjc*ip
            nhrtw = 0
            ! NSCh = 0

			CALL zeroing_module_vars()
            IF(gdrdyn==1.0D0) CALL ULMDYN(nnuc,jcn,EX(ke,nnuc)) ! prepare GDR parameters (if spin dependent GDR selected)

         !----------------------------------------------------------
         ! Collecting outgoing channels
         !----------------------------------------------------------

            !----------------------------------------------------------
            ! particle decay
            !----------------------------------------------------------
            DO nejc = 1, nejcm                            !do loop over ejectiles
               IF(NREs(nejc)<0) CYCLE
               nnur = NREs(nejc)
               summa = WFC_DECAY(nnuc,ke,jcn,ip,nnur,nejc)
            ENDDO                                         !do loop over ejectiles  ***done***
            ! write(*,*) sumin_w,sumtt_w
			! write(*,*) num%elal,num%elah,num%coll,num%colh
            
			if(num%elal == 0) EXIT ! if there are no elastic channels, we can exit the inner "jcn" loop
            num%part = NCH         !store number of particle channel entries

            !----------------------------------------------------------
            ! gammas (weak channels)
            !----------------------------------------------------------
            sumg = WFC_DECAYG(nnuc,ke,jcn,ip)

            !----------------------------------------------------------
            ! Fission (may be a weak or strong channel)
            !----------------------------------------------------------
            sumfis = WFC_DECAYF(nnuc,ke,jcn,ip)

         !----------------------------------------------------------
         ! Collecting outgoing channels completed
         !----------------------------------------------------------
             IF(DENhf==0.D0) CYCLE

            !  write(*,*)' '
            !  write(*,*)'SUMMARY OF DECAY FOR J=',xjc
            !  write(*,*)'total sum of  Tls ', H_Sumtl
            !  write(*,*)'sum of strong Tls ', H_Sumtl-H_Sweak
            !  write(*,*)'sum of weak   Tls ', H_Sweak
            !  write(*,*)'sum of weak part. ', H_Sweak-sumg
            !  write(*,*)'sum of gammas     ', sumg
            !  write(*,*)'sum fission       ', sumfis
            !  write(*,*)'sum Tl**2         ', H_Sumtls
            !  write(*,*)'# of strong Tls   ', NCH
            !  write(*,*)'average Tl        ', H_Tav
            !  write(*,*)'Decay state ',jcn*ip
			!  write(*,*)'DENhf calculated before Moldauer', DENhf

            !----------------------------------------------------------
            ! Calculate WF term common for all channels
            !----------------------------------------------------------
            CALL WFC1()     ! Calculate all nu's and the part of Moldauer integral
                            ! that doesn't depend on incoming and outgoing channles

            !----------------------------------------------------------
            ! Loop over incoming (fusion) channels
            !----------------------------------------------------------
            SCRt_mem  = SCRt    ! store initial values
            SCRtl_mem = SCRtl
            sumfis_mem = sumfis	

    		! write(*,*) 'Elastic channels:',num%elal, num%elah 
    		! write(*,*) 'Collective channels:',num%coll, num%colh 
            DO i = num%elal, num%elah

               SCRt   = SCRt_mem
               SCRtl  = SCRtl_mem
               sumfis = sumfis_mem

               in => inchnl(i - num%elal + 1) ! elastic channels for each Jcn are numbered 1,2,3,...
               out => outchnl(i)
               in%t = out%t

			   IF (INTerf==0) THEN
                 ! absorption ~ sigma_a
                 in%sig = coef*in%t*(2.D0*xjc + 1.D0)*FUSred*REDmsc(jcn,ipar)  ! absorption for incoming channel
                 xnor = in%sig/DENhf ! normalization factor
                 ! write(*,*) 'Jcn, Tlj_in, Tlj_out, coef, sig ', xjc, in%t, out%t, coef, in%sig
               ELSE
                 !xnor = 1.d0/DENhf ! normalization factor
                 xnor = 1.d0
			   ENDIF
               !----------------------------------------------------------
               ! Fission
               !----------------------------------------------------------
               IF(num%fiss>0) sumfis = outchnl(num%fiss)%t*outchnl(num%fiss)%rho*WFC2(i,num%fiss)  !redefining sumfis to account for the HRTW T=>V transition

               sumin_s = 0.d0
               sumtt_s = 0.d0
               DO iout = 1, num%part                       !Scan strong particle channels (note: weak channels are already in SCRt)
                  out => outchnl(iout)                     !ATTENTION: redefining outgoing channel!!!
                  w = WFC2(i,iout)                         !Moldauer width fluctuation factor (ECIS style)
                  ! WRITE(8,*) 'continuum WFC', iout, w

                  WFC(i,iout) = w                          ! saving the calculated sigma corrected by WF
                  sigma_ = out%t*w

                  IF(INTerf>0 .and. (iout>=num%coll .and. iout<=num%colh) ) THEN
                     Sab(i-num%coll+1, iout-num%coll+1) = xnor*sigma_
				     ! write(*,*) iaa, ibb, sngl(Sab(i-num%coll + 1,iout-num%coll + 1))
				     CYCLE	! Skipping coupled channels if INTerf>0
                  ENDIF

                  CALL update_SCRt(out, sigma_, sumin_s, sumtt_s)

               ENDDO ! end of outgoing channel loop over iout

               ! If EW active, all elastic channels assumed collective !!
               IF(INTerf>0 .and. (iout>=num%coll .and. iout<=num%colh) ) CYCLE

      		   CALL elastic_corr(sumin_s, sumtt_s, sumtt_w, sumin_w)

               !----------------------------------------------------------
               ! Renormalizing scratch matrices to recover unitarity
               !----------------------------------------------------------
               DENhf = SUM(SCRt)*de + SUM(SCRtl) + sumfis
               DENhf = DENhf - 0.5*SUM(SCRt(1,:,:,:))*de   !correct for the edge effect in trapezoidal integration
               !               write(*,*)'DENhf calculated as integral of SCRt & SCRtl', DENhf
               IF(DENhf.LE.0.0D0) CYCLE                    ! no transitions from the current state

               xnor = in%sig/DENhf                          ! normalization factor
               SCRt = SCRt*xnor                             ! normalizing scratch matrices instead of passing xnor to XSECT,
               SCRtl = SCRtl*xnor                           !   the above helps implementation of the EW transformation that provides
               SCRtem = SCRtem*xnor                         !   unfactorized cross sections.
               sumfis = sumfis*xnor                         !                                  "
               sumfism = sumfism*xnor                         !                                  "
               !---------------------------------------------------------------
               ! CN angular distributions (neutron (in)elastic scattering ONLY!)
               !---------------------------------------------------------------
               CALL CN_DA_anis(i, in, Ia, sxj, xjc, xnor)

               CALL XSECT(nnuc,m,1.0D0,sumfis,sumfism,ke,ipar,jcn,fisxse)  !normalize SCRt matrices and store x-sec
               !               CALL XSECT(nnuc,m,xnor,sumfis,sumfism,ke,ipar,jcn,fisxse)  !normalize SCRt matrices and store x-sec
                
               IF (INTerf==0) CALL DelCCmatr() ! deallocate EW matrices

            ENDDO    !end do loop over incident channels

            IF(INTerf>0) THEN

       		   ! write (*,*) 'Dimension of Umatr:',NDIm_cc_matrix
			   ! write (*,*) 'Dimension of Umatr (real):',sqrt(DBLE(size(Umatr)))

			   ! setting the complex identity matrix to call DIAG() 
			   ZRtmp1 = 0.d0
               do i=1,NDIm_cc_matrix
                 ZRtmp1(i,i) = 1.d0			     
			   enddo
			   ZItmp1 = 0.d0

			   !Diagonalizing the Smatrix in the transformed space
               Sdiag =   REAL(Smatr) 
               ZItmp =   IMAG(Smatr) 
               !write(*,*) 'Smatrix HRTW'
               !DO i = 1,NDIm_cc_matrix
               !  DO iout = 1,NDIm_cc_matrix
   	           !    write(*,'(1x,2(I3,1x),9(d12.6,1x,d12.6))') i,iout,Sdiag(i,iout),ZItmp(i,iout)
               !  ENDDO
	           !ENDDO

			   CALL QDIAG(Sdiag,ZItmp,ZRtmp1,ZItmp1,NDIm_cc_matrix,epsil,dtmp,IER)
               IF(IER/=0) WRITE (8,*) 'WARNING: EW Smatrix DIAGONALIZATION PROBLEMS for CN Jpi=',sngl(xjc*ip)   
			   ! On exit Sdiag contains the diagonal Smatrix S_{alpha,alpha) in the transformed space 
        	   ! Sphase(i) represents the arctan(S_{alpha,alpha}) given in eq.(20)
			   do i=1,NDIm_cc_matrix
      	         ! write (*,'(1x,A20,i3,2(1x,d12.6),3x,A12,d12.6)') 'Eigenvalues (Smatr)=',i, Sdiag(i,i),ZItmp(i,i),' phi(alpha)=',datan(Sdiag(i,i)) 
			     Sphase(i) = datan(Sdiag(i,i))
			   enddo

			   ! setting the complex identity matrix to call DIAG() 
			   !ZRtmp1 = 0.d0
               !do i=1,NDIm_cc_matrix
               !  ZRtmp1(i,i) = 1.d0			     
			   !enddo
			   !ZItmp1 = 0.d0
			   !Diagonalizing the Pmatrix in the transformed space for cross checking 
               !PPdiag =   REAL(Pmatr) 
               !ZItmp  =   IMAG(Pmatr) 
               !write(*,*) 'Pmatrix HRTW'
               !DO i = 1,NDIm_cc_matrix
               !  DO iout = 1,NDIm_cc_matrix
   	           !    write(*,'(1x,2(I3,1x),9(d12.6,1x,d12.6))') i,iout,PPdiag(i,iout),ZItmp(i,iout)
               !  ENDDO
	           !ENDDO
			   !CALL QDIAG(PPdiag,ZItmp,ZRtmp1,ZItmp1,NDIm_cc_matrix,epsil,dtmp,IER)
               !IF(IER/=0) WRITE (8,*) 'WARNING: EW DIAGONALIZATION PROBLEMS FOR Pmatrix in CN Jpi=',sngl(xjc*ip)   
			   ! On exit PPdiag contains the diagonalized Pmatrix = P{alpha,alpha) in the transformed space 
               ! ZRtmp1,ZItmp1 contains the real and imaginary part of the eigenvectors = Umatrix            
		     
			   !WRITE(*,*) 'HRTW diag: eigenvector = Umatr(,), eigenvalues = PPdiag()'
               !DO iout = 1,NDIm_cc_matrix
      	       !  write (*,'(1x,A20,i3,2(1x,d12.6),3x,A12,d12.6)') 'Eigenvalues (Pmatr)=',iout, PPdiag(iout,iout),ZItmp(iout,iout) 
			   !  DO I = 1,NDIm_cc_matrix
			   !    write(*,'(1x,2(i3,1x),9(d12.6,1x,d12.6))') i,iout, ZRtmp1(I,iout),ZItmp1(I,iout)
               !  ENDDO
			   !ENDDO

			   ! Engelbrecht- Weidenmueller transformaion
               ! loop over iaa=i (coupled channels in the normal space)
               write(*,*) 'CN decay state Jpi',xjc,ip
			   write(*,*) 'num%elal, num%elah=',num%elal, num%elah 
			   write(*,*) 'num%coll, num%colh=',num%coll, num%colh
			   write(*,*) '1-num%part=',1,num%part

               sumin_s = 0.d0
               sumtt_s = 0.d0
               DO i = num%elal, num%elah
                  
				  if(STLcc(i)%lev /= levtarg) CYCLE ! Skipping non-elastic collective channels in the normal space

				  iaa = i - num%elal + 1
                  in => inchnl(iaa) ! elastic channels for each Jcn are numbered 1,2,3,...
                  out => outchnl(i)
	     	      write(*,*) 'i,iaa, -out%kres, in%t=', i, iaa, -out%kres  
                  ! absorption ~ sigma_a
                  ! in%sig = coef*in%t*(2.D0*xjc + 1.D0)*FUSred*REDmsc(jcn,ipar)  ! absorption for incoming channel
                  in%sig = coef*STLcc(i)%tlj*(2.D0*xjc + 1.D0)*FUSred*REDmsc(jcn,ipar)  ! absorption for incoming channel
                  !xnor = in%sig/DENhf                     ! normalization factor
                  !xnor = 1.d0/DENhf                        ! normalization factor
                  xnor = 1.d0
				  ! write(*,*) 'Jcn, Tlj_in, Tlj_out, coef, sig ', xjc, in%t, out%t, coef, in%sig

                  ! loop over ibb=iout (coupled channels in the normal space)
                  DO iout = num%coll, num%colh
                     ibb = iout - num%coll + 1
                     !write(*,*) 'Matrix dimension a & b:',STLcc(i)%nceq,STLcc(iout)%nceq   
					 !write(*,*) 'num%elal,num%coll:',num%elal,num%coll
					 
                     ! Engelbrecht- Weidenmueller backward transformation Eq.(16),(17),(18) TK paper
                     !------------------------------------------------------------------------------
                     ! loops over collective levels in the transformed space (ialph & ibeta)
                     !write(*,*) 'EW transformation, space dimension',NDIm_cc_matrix
                     ctmp1 = (0.d0,0.d0) 
                     sigma_EW = 0.d0      ! This is the cross section \sigma_{ab} in the normal space
                     do ialph = 1, NDIm_cc_matrix    
						ialph_ch = num%coll + ialph -1
                        nu_ialph =  outchnl(ialph_ch)%eef/2.D0   ! half of the degree of freedom for alpha channel
						deg_alph = 	DSQRT(1.d0/nu_ialph - 1.d0)

                        do ibeta = 1, NDIm_cc_matrix 
						  ! phas represents the arctan(S_{alpha,alpha}) given in eq.(20)
						  dtmp = Sphase(ialph)-Sphase(ibeta)
						  phas = CMPLX(cos(dtmp),sin(dtmp))
						  !phas = (1.d0,0.d0) ! assumed one for the time being
   					      ibeta_ch = num%coll + ibeta -1
                          nu_ibeta =  outchnl(ibeta_ch)%eef/2.D0 ! half of the degree of freedom for the outgoing channel
                          deg_beta =  DSQRT(1.d0/nu_ibeta - 1.d0)

        			      ! write(*,*) 'EW loops',ialph, ibeta, sngl(Sab(ialph,ibeta)),sngl(WFC(ialph_ch,ibeta_ch))
						   
                          if(ialph == ibeta) then
                            ctmp1 = ctmp1 + ABS(Umatr(ialph,iaa))**2 & !  Umatr(ialph,iaa)*CONJG(Umatr(iaa,ialph))   &
							              * ABS(Umatr(ialph,ibb))**2 & !* Umatr(ialph,ibb)*CONJG(Umatr(ibb,ialph)) &
										  * Sab(ialph,ialph)
                          else
                            ctmp1 = ctmp1 + Sab(ialph,ibeta)*                          &
							       ( CONJG(Umatr(ialph,iaa))*CONJG(Umatr(ibeta,ibb))*  &
                                      ( Umatr(ialph,iaa)*Umatr(ibeta,ibb) +            &
                                        Umatr(ibeta,iaa)*Umatr(ialph,ibb) ) +          &
                                     CONJG(Umatr(ialph,iaa))*CONJG(Umatr(ialph,ibb))*  &
                                           Umatr(ibeta,iaa) *      Umatr(ibeta,ibb)*   &
								                phas*deg_alph*deg_beta )
                          endif
                          sigma_EW = sigma_EW + REAL(ctmp1)   
                          
                          write(*,*) 'Sigma(a=',iaa,', b=',ibb,')=', REAL (ctmp1),' REAL' ! the cross section \sigma_{ab} in the normal space
                          write(*,*) 'Sigma(a=',iaa,', b=',ibb,')=', DIMAG(ctmp1),' IMAG' ! the imaginary part expected to be zero

                        enddo ! end of the loop over ibeta (transformed space)					 
                     enddo   ! end of the loop over ialph (transformed space)

   				     write(*,*) 'Sigma_abs=',in%sig
                     sigma_ = sigma_EW*in%sig
   				     write(*,*) 'Sigma_ab =',sigma_ 
                     ! END of Engelbrecht- Weidenmueller backward transformation Eq.(16),(17),(18) TK paper
                     !------------------------------------------------------------------------------

					 out => outchnl(iout) ! reassigning output channel to the outgoing CC in the normal space
	     	         write(*,*) 'iout,ibb, -out%kres=', iout, ibb, -out%kres  

					 PAUSE

					 CALL update_SCRt(out, sigma_, sumin_s, sumtt_s)

                  ENDDO ! end of the loop over iout=ibb (outgoing coupled channels in the normal space)

                  CALL DelCCmatr() ! deallocate EW matrices
				  CYCLE

				  CALL elastic_corr(sumin_s, sumtt_s, sumtt_w, sumin_w)

                  !----------------------------------------------------------
                  ! Renormalizing scratch matrices to recover unitarity
                  !----------------------------------------------------------
                  DENhf = SUM(SCRt)*de + SUM(SCRtl) + sumfis
                  DENhf = DENhf - 0.5*SUM(SCRt(1,:,:,:))*de   !correct for the edge effect in trapezoidal integration
                  !               write(*,*)'DENhf calculated as integral of SCRt & SCRtl', DENhf
                  IF(DENhf.LE.0.0D0) CYCLE                    ! no transitions from the current state

                  xnor = in%sig/DENhf                          ! normalization factor
                  SCRt = SCRt*xnor                             ! normalizing scratch matrices instead of passing xnor to XSECT,
                  SCRtl = SCRtl*xnor                           !   the above helps implementation of the EW transformation that provides
                  SCRtem = SCRtem*xnor                         !   unfactorized cross sections.
                  sumfis = sumfis*xnor                         !                                  "
                  sumfism = sumfism*xnor                       !                                  "

                  !---------------------------------------------------------------
                  ! CN angular distributions (neutron (in)elastic scattering ONLY!)
                  !---------------------------------------------------------------
				  CALL CN_DA_anis(i, in, Ia, sxj, xjc, xnor)

                  CALL XSECT(nnuc,m,1.0D0,sumfis,sumfism,ke,ipar,jcn,fisxse)  !normalize SCRt matrices and store x-sec
                  ! CALL XSECT(nnuc,m,xnor,sumfis,sumfism,ke,ipar,jcn,fisxse)  !normalize SCRt matrices and store x-sec

               ENDDO ! end of do loop over i=iaa (coupled elastic channels in the normal space)

            Endif ! ENDIF over INTerf>0


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

      CALL Gamma_renormalization(d0c, sumtg, tgexper, itmp, nnuc)

      CALL DelHRTW()    !deallocate HRTW arrays
      IF(DIRECT>0 .and. MAX_cc_mod>0) CALL DelTLJs() ! deallocate incident channel TLJs for CC
      IF(INTerf==0) CALL DelCCmatr() ! deallocate EW matrices
      RETURN

   END SUBROUTINE Moldauer

   !----------------------------------------------------------------------------------------------------

   REAL*8 FUNCTION NU(Tl,Sumtl,Itype)

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
      INTEGER, intent(in) :: Itype

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

   SUBROUTINE WFC1()

      !****************************************************************************************
      !                            W F C 1
      !
      !   Moldauer width fluctuation formula - part 1. Calculates global component of the
      !   integral that is independent of the incident and outgoing channels. Uses Kawano
      !   transformation t = z/(1-z) that redefines integration limits  from 0-inf to 0-1.
      !   For all the channels we calculate:
      !   - number of degree of freedom nu (divided by 2)
      !   - alpha = 2*T/nu*SUM(T)
      !   - g1 = (1-(1-alpha)z)/(1-z)
      !   - product of all G^nu for each of the values of z = xgk(.)
      !   - product of all G^nu for each of the values of z = 0.5*(1-xgk(1)) ... 0.5*(1+xgk(21))
      !
      !****************************************************************************************


      INTEGER:: i,ic
      real*8:: nu_c, a1, g1, z, zm, dsum
      TYPE (channel), POINTER :: out

      do ic = 1, NCH                             ! do loop over all channels
         out => outchnl(ic)
         out%eef = NU(out%t,H_Sumtl,LHRtw-2)     ! Calculate degrees of freedom for all  channels
      end do
      save_WFC1 = 0.0D0                          !logarithmic version
      do i = 1, 41
         !        save_WFC1(i) = 1.0D0                    !multiplicative version
         z = xgk(i)
         zm = 1.d0 - z
         dsum = 0.d0
         do ic = 1, NCH                          ! do loop over all channels
            out => outchnl(ic)                   ! set outgoing channel shortcut
            nu_c = out%eef/2.D0                  ! calculated number of degrees of freedom nu
            a1 = out%t/nu_c/H_Sumtl              ! calculate alpha
            IF(a1==0) CYCLE
            g1 = (1.0D0-(1.0D0-a1)*z)/zm
            !            save_WFC1(i) = save_WFC1(i)*g1**(outchnl(ic)%rho*nu_c)      !multiplicative version
            !            save_WFC1(i) = MIN(save_WFC1(i),10.0D0**80)                 !multiplicative limit
            dsum = dsum + outchnl(ic)%rho*nu_c*DLOG(g1)                  !logarithmic version
         end do !over channels
         a1 = sumg/20.0D0/H_Sumtl                !calculate alpha for a cumulative gamma channel (nu=20 assumed)
         g1 = (1.0D0-(1.0D0-a1)*z)/zm            !calculating G for a cumulative gamma channel
         !         save_WFC1(i) = save_WFC1(i)*g1**20     !adding gammas multiplicative
         save_WFC1(i) = dsum + 20.0D0*DLOG(g1)   !adding gammas logarithmic
      end do !over integration steps
      return
   end subroutine WFC1

   !----------------------------------------------------------------------------------------------------

   REAL*8 FUNCTION WFC2(in,ou)
      !
      !  Calculates width fluctuation correction (WFC) for nchannels in and out using Moldauer approach.
      !  The in & out channel independent part has been calculated upfront with the WFC1 subroutine.
      !
      IMPLICIT NONE
      INTEGER, intent(in):: in, ou         ! index number of incoming (in) and outgoing (out) channels
      real*8:: nu_ou, nu_in, a_in, a_ou, gl_in, gl_ou
      REAL*8 RESK1,z,zm
      INTEGER j
      nu_in =  outchnl(in)%eef/2.D0                           ! half of the degree of freedom for the incoming channel
      nu_ou =  outchnl(ou)%eef/2.D0                           ! half of the degree of freedom for the outgoing channel
      a_in = outchnl(in)%t/nu_in/H_Sumtl                      ! calculate alpha
      a_ou = outchnl(ou)%t/nu_ou/H_Sumtl                      ! calculate alpha

      IF(a_in*a_ou==0) THEN
         WFC2 = 1.0D0
         RETURN                                         !return if any of the transmission coefficcients is 0
      ENDIF
      resk1 = 0.0D0
      DO j = 41,1,-1
         z=xgk(j)
         gl_in = DLOG(1.0D0-(1.0D0-a_in)*z)
         gl_ou = DLOG(1.0D0-(1.0D0-a_ou)*z)
         zm = save_WFC1(j)+gl_in+gl_ou
         if(zm.gt.40.d0) cycle
         resk1 = resk1 + wgk(j)*dexp(-zm)  !wgk(j)/DEXP(save_WFC1(j)+gl_in+gl_ou)
      ENDDO
      WFC2 = resk1*0.5d0
      if(in==ou) WFC2 = (1.D0 + 1.D0/nu_in)*WFC2 ! case of elastic
      !     write(8,'(''in channel '',2I5,4E12.5,2i5)') in, outchnl(in)%l, outchnl(in)%j, outchnl(in)%t, outchnl(in)%rho, &
      !                 outchnl(in)%eef, outchnl(in)%nejc, outchnl(in)%kres
      !     write(8,'(''ou channel '',2I5,4E12.5,2i5,'' WFC = '',F10.5)') ou, outchnl(ou)%l, outchnl(ou)%j, outchnl(ou)%t, &
      !                outchnl(ou)%rho, outchnl(ou)%eef, outchnl(ou)%nejc, outchnl(ou)%kres, WFC2
      !      WFC2 = 1.0D0
      return
   end function WFC2

   !----------------------------------------------------------------------------------------------------

   SUBROUTINE Gamma_renormalization(d0c, sumtg, tgexper, itmp, nnuc)

      REAL*8 :: d0c, sumtg, tgexper
      INTEGER :: itmp, nnuc

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
         WRITE (8 ,'('' WARNING: Gamma emission width from '',I3,A2,'' normalized by '',F7.3)') &
            NINT(A(nnuc)), SYMb(nnuc), TUNe(0,nnuc)
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

      RETURN

   END SUBROUTINE Gamma_renormalization

   SUBROUTINE QDIAG(ZR,ZI,XR,XI,NC,EPS,AX,IER)
!
! TAKEN FROM ECIS2006 by J. RAYNAL
! DIAGONALISATION OF A HERMITIAN COMPLEX MATRIX BY AN EXTENSION OF THE  
! JACOBI'S METHOD.                                                      
! INPUT:     ZR,ZI:  REAL AND IMAGINARY PARTS OF THE MATRIX.            
!            XR,XI:  REAL AND IMAGINARY PARTS OF THE UNIT MATRIX.       
!            NC:     DIMENSION OF SQUARE MATRICES ZR,ZI,XR AND XI.                
!            EPS:    VALUE BELOW WHICH MATRIX ELEMENTS ARE SET TO 0.    
! OUTPUT:    ZR,ZI:  THE EIGENVALUES ARE ON THE DIAGONAL OF ZR.         
!                    ALL THE OTHER ELEMENTS ARE 0, IF PROCESS SUCCEEDED.
!            XR,XI:  EIGENVECTORS.                                      
!            AX:     SQUARE OF NORM OF THE LARGEST NON DIAGONAL ELEMENT.
!            IER:    RETURNS 0 OR -1 AFTER 4*NC**2 ROTATIONS.           
!***********************************************************************
      IMPLICIT NONE
	  INTEGER NC
	  REAL*8 ZR,ZI,XR,XI
	  DIMENSION ZR(NC,NC),ZI(NC,NC),XR(NC,NC),XI(NC,NC)                     
	  REAL*8 EPS,AX
	  INTEGER IER
      ! local variables	  
      INTEGER NT,I,J,L,M
	  REAL*8 AR,AI,AY,BI,BR,U,V,UC,US,TC,TS,UCC,UCS,USC,USS

      IER=0                                                             
      NT=0                                                              
    1 NT=NT+1                                                           
      IF (NT.GT.4*NC*NC) GO TO 6                                        
      AX=0.D0                                                           
      L=1                                                               
      M=2                                                               
      
!     if(NT<=1) then 
!       write(*,*) 'Inside QDIAG to diag, iter',NT,NC,sngl(EPS)
!       DO I = 1,NC
!           write(*,'(1x,9(d12.6,1x,d12.6/)') (ZR(I,J),ZI(I,J),J=1,NC)
!         ENDDO
!        write(*,*) 'Inside QDIAG unitary, iter'
!        DO I = 1,NC
!            write(*,'(1x,9(d12.6,1x,d12.6/)') (XR(I,J),XI(I,J),J=1,NC)
!          ENDDO
!     endif

! SYMMETRISATION AND SEARCH FOR THE LARGEST NON DIAGONAL ELEMENT.       
      DO I=1,NC                                                       
		DO J=I,NC                                                       
		  IF (ZR(J,I).EQ.0.D0) ZR(I,J)=0.D0                                 
		  IF (ZI(J,I).EQ.0.D0) ZI(I,J)=0.D0                                 
		  IF (ZR(I,J).EQ.0.D0) ZR(J,I)=0.D0                                 
		  IF (ZI(I,J).EQ.0.D0) ZI(J,I)=0.D0                                 
		  AR=(ZR(I,J)+ZR(J,I))/2.D0                                         
		  AI=(ZI(I,J)-ZI(J,I))/2.D0                                         
		  ZR(J,I)=AR                                                        
		  ZR(I,J)=AR                                                        
		  ZI(I,J)=AI                                                        
		  ZI(J,I)=-AI                                                       
		  IF (I.EQ.J) CYCLE
		  AY=ZR(I,J)**2+ZI(I,J)**2                                          
		  IF (AX.GT.AY) CYCLE
		  AX=AY                                                             
		  L=I                                                               
		  M=J
		ENDDO
	  ENDDO                                                               
      IF (AX.EQ.0.D0) RETURN                                            
! ELEMENTARY TRANSFORMATION.                                            
      U=DATAN2(-ZI(L,M),ZR(L,M))/2.D0                                   
      V=DATAN2(2.D0*DSQRT(ZR(L,M)**2+ZI(L,M)**2),ZR(M,M)-ZR(L,L))/2.D0  
      UC=DCOS(U)                                                        
      US=DSIN(U)                                                        
      TC=DCOS(V)                                                        
      TS=-DSIN(V)                                                       
      UCC=UC*TC                                                         
      UCS=UC*TS                                                         
      USC=US*TC                                                         
      USS=US*TS                                                         
   ! TRANSFORMATION OF ROWS.                                               
      DO I=1,NC                                                       
		  AR=XR(I,L)*UCC+XI(I,L)*USC+XR(I,M)*UCS-XI(I,M)*USS                
		  BR=-XR(I,L)*UCS-XI(I,L)*USS+XR(I,M)*UCC-XI(I,M)*USC               
		  AI=XI(I,L)*UCC-XR(I,L)*USC+XI(I,M)*UCS+XR(I,M)*USS                
		  BI=-XI(I,L)*UCS+XR(I,L)*USS+XI(I,M)*UCC+XR(I,M)*USC               
		  XR(I,L)=AR                                                        
		  XR(I,M)=BR                                                        
		  XI(I,L)=AI                                                        
		  XI(I,M)=BI                                                        
		  AR=ZR(I,L)*UCC+ZI(I,L)*USC+ZR(I,M)*UCS-ZI(I,M)*USS                
		  BR=-ZR(I,L)*UCS-ZI(I,L)*USS+ZR(I,M)*UCC-ZI(I,M)*USC               
		  AI=ZI(I,L)*UCC-ZR(I,L)*USC+ZI(I,M)*UCS+ZR(I,M)*USS                
		  BI=-ZI(I,L)*UCS+ZR(I,L)*USS+ZI(I,M)*UCC+ZR(I,M)*USC               
		  ZR(I,L)=AR                                                        
		  ZR(I,M)=BR                                                        
		  ZI(I,L)=AI                                                        
		  ZI(I,M)=BI                                                        
	  ENDDO
   ! TRANSFORMATION OF COLUMNS.                                            
      DO I=1,NC                                                       
      AR=ZR(L,I)*UCC-ZI(L,I)*USC+ZR(M,I)*UCS+ZI(M,I)*USS                
      BR=-ZR(L,I)*UCS+ZI(L,I)*USS+ZR(M,I)*UCC+ZI(M,I)*USC               
      AI=ZI(L,I)*UCC+ZR(L,I)*USC+ZI(M,I)*UCS-ZR(M,I)*USS                
      BI=-ZI(L,I)*UCS-ZR(L,I)*USS+ZI(M,I)*UCC-ZR(M,I)*USC               
      IF (DABS(AR).LT.EPS) AR=0.D0                                      
      IF (DABS(BR).LT.EPS) BR=0.D0                                      
      IF (DABS(AI).LT.EPS) AI=0.D0                                      
      IF (DABS(BI).LT.EPS) BI=0.D0                                      
      ZR(L,I)=AR                                                        
      ZR(M,I)=BR                                                        
      ZI(L,I)=AI                                                        
      ZI(M,I)=BI                                                        
	  ENDDO
      GO TO 1                                                           
    6 IER=-1                                                            
      RETURN                                                            
   END SUBROUTINE QDIAG

!**************************************************************

   SUBROUTINE CN_DA_anis(i, in, Ia, sxj, xjc, xnor)
      INTEGER i
      TYPE (fusion),  POINTER :: in
      REAL*8 Ia, sxj, xjc, xnor 

      REAL*8 la,ja,lb,jb,xjr, w, stmp, xleg, tmp
      INTEGER iout, lleg
      TYPE (channel), POINTER :: out

      IF(CN_isotropic) RETURN
      !---------------------------------------------------------------
      ! CN angular distributions (neutron (in)elastic scattering ONLY!)
      !---------------------------------------------------------------

                  ! accumulate Legendre coefficients
                  la = in%l                             !incident neutron l
                  ja = in%j                             !incident neutron j
                  ! write(8,*) 'Incident chnl',i, 'J_pi ',xjc*ip,' number of outgoing n channels', num%neut
                  DO iout = 1, num%neut !do loop over neutron channels only
                     ! write(8,*) 'Elastic channel #',j, ' abs = ',in%sig, ' xnor = ', xnor
                     ! write(8,*) '                                  leg      BB               Jcn   &
                     ! &               l_inc           j_inc                 J_res              l_out              J_out'
                     ! w = 1.D0
                     out => outchnl(iout)
                     IF(out%kres > 0) cycle      ! skipping continuum channels

                     xjr = out%xjrs              !residual nucleus J
                     lb = out%l                  !outgoing neutron l
                     jb = out%j                  !outgoing neutron j
                     if(LHRtw==3.or.LHRtw==4) then
                        w = WFC(i,iout)             !sigma corrected by Moldauer width fluctuation factor
                     else
                        w=1.0d0
                     endif
                     PL_lmax(-out%kres) = 2*la
                     !                     IF(out%kres > 0) THEN
                     !                        PLcont_lmax(out%kres) = la
                     !                     ELSEIF(out%kres < 0) THEN
                     !                        PL_lmax(-out%kres) = la
                     !                     ENDIF
                     IF(i/=iout.AND.out%kres<=0) THEN               ! Inelastic to discrete level
                        stmp = xnor*out%t*out%rho*w*CINRED(-out%kres)
                     ELSE                                        ! Elastic and continuum
                        stmp = xnor*out%t*out%rho*w
                     ENDIF
                     DO lleg = 0, 2*in%l, 2    !do loop over Legendre L
                        xleg = dble(lleg)
                        tmp = Blatt(xjc,Ia,la,ja,sxj,xjr,lb,jb,sxj,xleg)/(2*xleg + 1.0d0)
                        ! write(8,*) ' Leg => tmp,xjc,la,ja,xjr,lb,jb,',lleg,tmp,xjc,la,ja,xjr,lb,jb,outchnl(iout)%kres
                        ! if(tmp==0.D0) cycle
                        IF(dabs(tmp) < 1.d-14) CYCLE
                        PL_CN(lleg,-out%kres) = PL_CN(lleg,-out%kres) + tmp*stmp
                        ! CN ang. distr. for continuum assumed isotropic
                        !IF(out%kres > 0) THEN
                        !   PL_CNcont(lleg,out%kres) = PL_CNcont(lleg,out%kres) + stmp*tmp/de
                        !ELSEIF(out%kres < 0) THEN
                        !   PL_CN(lleg,-out%kres) = PL_CN(lleg,-out%kres) + stmp*tmp
                        !ENDIF
                     ENDDO
                     !IF(out%kres > 0) THEN
                     !  write (8,'(2x,A10, 5Hchan= ,I4,2x,5Hlmax= ,I4,5H cont  )') 'HRTW-comp ',out%kres, PLcont_lmax(out%kres)
                     !ELSEIF(outchnl(iout)%kres < 0) THEN
                     !  write (8,'(2x,A10, 5Hchan= ,I4,2x,5Hlmax= ,I4,5H disc  )') 'HRTW-comp ',out%kres, PL_lmax(-out%kres)
                     !ENDIF
                     !write(8,*) 'PL_CNcont(lleg,1) = ', PL_CNcont(0,1), PL_CNcont(2,1), PL_CNcont(4,1), PL_CNcont(6,1), PL_CNcont(8,1)
                  ENDDO
      RETURN
   END SUBROUTINE CN_DA_anis


   SUBROUTINE update_SCRt(out,sigma_, sumin_s, sumtt_s)
   IMPLICIT none
   TYPE (channel), POINTER :: out
   REAL*8 sigma_, sumin_s, sumtt_s
   REAL*8 stmp

   IF(out%kres>0) THEN                      !continuum channels
     SCRt(out%kres,out%jres,out%pres,out%nejc) = SCRt(out%kres,out%jres,out%pres,out%nejc) &
	                                           + sigma_ *out%rho/de
                                             ! + out%t*out%rho*w/de
   ELSE IF(out%kres<0) THEN
     IF( (out%nejc.EQ.NPRoject) .AND. (-out%kres.NE.LEVtarg) ) THEN ! apply CINRED to discrete inelastic channels
                                                ! stmp = out%t*out%rho*w
      stmp = sigma_ *out%rho
      sumin_s = sumin_s + stmp * CINRED(-out%kres)
      sumtt_s = sumtt_s + stmp
      SCRtl(-out%kres,out%nejc) = SCRtl(-out%kres,out%nejc) +  stmp * CINRED(-out%kres)
    ELSE
    ! SCRtl(-out%kres,out%nejc) = SCRtl(-out%kres,out%nejc) + out%t*out%rho*w
      SCRtl(-out%kres,out%nejc) = SCRtl(-out%kres,out%nejc) + sigma_ *out%rho
    ENDIF
  ENDIF
  RETURN
  END SUBROUTINE update_SCRt

  SUBROUTINE elastic_corr(sumin_s, sumtt_s, sumtt_w, sumin_w)
  IMPLICIT none
  REAL*8 sumin_s, sumtt_s, sumtt_w, sumin_w
  REAL*8 ewcor
  INTEGER iout, numch_el
  TYPE (channel), POINTER :: out  
  !----------------------------------------------------------------------------------------
  ! Correcting the elastic cross section for inelastic enhancement CINRED if any (ewcor/=0)
  !----------------------------------------------------------------------------------------
  ewcor = (sumtt_s - sumin_s +  sumtt_w - sumin_w)
  ! Correcting the elastic cross section for inelastic enhancement CINRED
  if(ewcor .NE. 0.d0) then
     numch_el = max(num%elah - num%elal + 1 , 1 )
     DO iout = num%elal, num%elah  ! do loop over elastic channels
       out => outchnl(iout)
       SCRtl(-out%kres,out%nejc) = SCRtl(-out%kres,out%nejc) + ewcor/numch_el
       !write(*,*) 'ilev=',-out%kres,' nejc=',out%nejc,' ewcor=',ewcor
     ENDDO
  endif
  RETURN
  END SUBROUTINE elastic_corr

  SUBROUTINE zeroing_module_vars()
     DENhf = 0.D0
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
     num%coll = 0
     num%colh = 0
     num%fiss = 0
     num%gamm = 0
 
     H_Sumtls = 0.D0
     H_Sumtl = 0.D0
     H_Tav = 0.D0
     H_Sweak = 0.D0
     H_Tl = 0.D0

     RETURN
  END SUBROUTINE zeroing_module_vars

END MODULE width_fluct
