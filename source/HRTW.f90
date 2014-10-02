   module HRTW

   use angular_momentum

   implicit none

   private

   include 'dimension.h'
   include 'global.h'

   type channel
     integer*4 l         ! ejectile l
     real*8 j            ! ejectile j
     real*8 t            ! ejectile Tlj
     real*8 ti1          ! temporary Tlj used for iteration
     real*8 ti2          ! temporary Tlj used for iteration
     real*8 rho          ! final level density for this channel
     real*8 eef          ! elastic enhancement factor
     integer*4 nejc      ! ejectile index (nejc)
     integer*4 kres      ! populated energy bin (negative for discrete levels, g.s. -1,...)
     real*8 xjrs         ! spin of the populated state
     integer*4 jres      ! spin index of the populated state
     integer*4 pres      ! parity index of the populated state
   end type channel

   type numchnl
      integer*4 neut     ! number of neutron channels, i.e., number of neutron entries in the 'channel' type
      integer*4 part     ! number of particle channels, i.e., number of particle entries in the 'channel' type
      integer*4 elal     ! position of the first (low) elastic channel
      integer*4 elah     ! position of the last elastic channel; elastics are embedded in particle channels elal<=elah<=part
      integer*4 fiss     ! effective number of fission channels
      integer*4 gamm     ! effective number of gamma channels
   end type numchnl

   type fusion
     integer*4 nout      ! position of the corresponding outgoing channel in outchnl
     integer*4 l         ! projectile l
     real*8 j            ! projectile j
     real*8 t            ! projectile Tlj
     real*8 sig          ! absorption x-section for this channel
   end type fusion

   integer*4, parameter :: ndhrtw1 = 4000
   integer*4, parameter :: ndhrtw2 = 30

   real*8 :: H_Sumtl      ! Sum of strong Tlj
   real*8 :: H_Sumtls     ! Sum of strong Tlj**2
   real*8 :: H_Sweak      ! Sum of weak Tlj
   real*8 :: H_Sweaks     ! Sum of weak Tlj**2
   real*8 :: H_Tav        ! Avarage strong Tlj
   real*8 :: H_Tthr       ! Thershold for Tlj to be considered strong
   real*8 :: TFIs         ! Sum of fission transmission coefficients
   real*8 :: TGam         ! Sum of gamma transmission coefficients
   integer*4 :: NCH       ! Number of strong channels (Tlj's)
   integer*4 :: NSCh      ! Number of strong  Tlj processed by VT routine, i.e. poistion in H_Tl matrix

   integer*4, allocatable :: MEMel(:,:)
   real*8, allocatable :: H_Tl(:,:)                      ! strong transmission coefficients LIKELY TO GET RID OFF!!!
   real*8, allocatable :: H_Abs(:,:)
   type(channel), allocatable, target :: outchnl(:)      ! outgoing channels
   type(fusion),  allocatable, target :: inchnl(:)       ! fusion channels
   type(numchnl) :: num                                  ! number of particular channels

   integer*4, public :: lhrtw = 0                        ! flag for HRTW. 0=no, 1=yes, up to ehrtw MeV incident
   real*8, public :: ehrtw = 0.D0                        ! energy limit for width corrections

   public calc_hrtw

   contains

   !----------------------------------------------------------------------------------------------------

   subroutine AllocHRTW(nd1,nd2)

   implicit none

   integer*4, intent(in), optional :: nd1, nd2

   integer my,ndch,ndfus

   ndch = ndhrtw1
   ndfus = ndhrtw2

   if(present(nd1)) ndch = nd1
   if(present(nd2)) ndfus = nd2

   if(allocated(MEMel)) deallocate(MEMel)
   allocate(MEMel(ndfus,3),stat=my)
   if(my /= 0) goto 10
   MEMel = 0

   if(allocated(H_Tl)) deallocate(H_Tl)
   allocate(H_Tl(ndch,2),stat=my)
   if(my /= 0) goto 10
   H_Tl = 0.0d0

   if(allocated(H_Abs)) deallocate(H_Abs)
   allocate(H_Abs(ndfus,3),stat=my)
   if(my /= 0) goto 10
   H_Abs = 0.0d0

   if(allocated(outchnl)) deallocate(outchnl)
   allocate(outchnl(ndch),stat=my)
   if(my /= 0) goto 10
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

   if(allocated(inchnl)) deallocate(inchnl)
   allocate(inchnl(ndfus),stat=my)
   if(my /= 0) goto 10
   inchnl%nout = 0
   inchnl%l = 0
   inchnl%j = 0.d0
   inchnl%t = 0.d0
   inchnl%sig = 0.d0

   return

10 write(8,*)  'ERROR: Insufficient memory for HRTW'
   write(12,*) 'ERROR: Insufficient memory for HRTW'
   stop 'ERROR: Insufficient memory for HRTW'

   end subroutine AllocHRTW

   !----------------------------------------------------------------------------------------------------

   subroutine DelHRTW()

   implicit none

   if(allocated(MEMel))   deallocate(MEMel)
   if(allocated(H_Tl))    deallocate(H_Tl)
   if(allocated(H_Abs))   deallocate(H_Abs)
   if(allocated(outchnl)) deallocate(outchnl)
   if(allocated(inchnl))  deallocate(inchnl)

   return
   end subroutine DelHRTW

   !----------------------------------------------------------------------------------------------------

   SUBROUTINE calc_hrtw

   !cc
   !cc   ********************************************************************
   !cc   *                                                         class:ppu*
   !cc   *                         H R T W                                  *
   !cc   *                                                                  *
   !cc   *  Calculate decay of the Compound Nucleus capture states in       *
   !cc   *  terms of the HRTW theory (width fluctuation correction).        *
   !cc   *  Uses modified routines of standard Hauser-Feshbach (DECAY,      *
   !cc   *  DECAYG, FISSION) and HRTW_MARENG to decompose capture           *
   !cc   *  cross sections into partial wave components.                    *
   !cc   *                                                                  *
   !cc   *                                                                  *
   !cc   ********************************************************************
   !cc

   IMPLICIT NONE

   REAL*8 :: elada(NDAngecis), elleg(NDAngecis)
   INTEGER neles
   COMMON /angula/elada,elleg,neles

   CHARACTER(1), parameter :: cpar(2) = (/'+', '-'/)

   ! Local variables

   LOGICAL*4 relcal
   INTEGER*4 i, ip, ipar, jcn, ke, m, ndivf, nejc, nhrtw, nnuc, nnur, itmp, lleg
   REAL*8 cnspin, fisxse, summa, sumfis, sumg, sumtg, tgexper, xnor, elcor, xjc
   REAL*8 j, Ia, xjr, ja, jb, la, lb, xleg, tmp
   REAL*8 xmas_npro, xmas_ntrg, el, ecms, ak2
   REAL*8 d0c
   REAL*8 sumfism(nfmod) ! , cel_da(NDAngecis), GET_DDXS

   type (channel), pointer :: out
   type (fusion),  pointer :: in

   call AllocHRTW()    !allocate HRTW matrices
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
   IF(.not. benchm) THEN
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
          IF(LHRtw==1) CALL AUSTER            !calculate V's for the strong channels (iteration)
          DENhf = H_Sumtl                     !reset DENhf using V's instead of T's

          ! construct scratch matrix for decay of the Jcn state

          DO i = 1, num%part                          !scan strong particle channels (note: weak channels are already in SCRt)
             out => outchnl(i)
             IF(out%kres>0) THEN                ! continuum channels
                SCRt(out%kres,out%jres,out%pres,out%nejc) = SCRt(out%kres,out%jres,out%pres,out%nejc) + out%t*out%rho/de
             ELSE IF(out%kres<0) THEN           ! discrete level channels
                SCRtl(-out%kres,out%nejc) = SCRtl(-out%kres,out%nejc) + out%t*out%rho
             ENDIF
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
             IF(LHRtw==1) THEN
                elcor = out%t*(out%eef - 1.D0)     ! elastic channel correction to SCRtl  (elcor=0 for HF)
                ! write(*,*) 'Elcor =', elcor, '  EEF =', out%eef
                SCRtl(-out%kres,out%nejc) = SCRtl(-out%kres,out%nejc) + elcor
             ENDIF
             ! write(*,*)'post AUSTER DENhf=', DENhf + elcor

             ! CN angular distributions (neutron (in)elastic scattering ONLY!)

             IF(.not.CN_isotropic) THEN
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
                   DO lleg = 0, 2*in%l, 2    !do loop over Legendre L
                     xleg = dble(lleg)
                     tmp = Blatt(xjc,Ia,la,ja,SEJc(nejc),xjr,lb,jb,SEJc(nejc),xleg)/(2*xleg + 1.0d0)
                     ! write(8,*) ' Leg => tmp,xjc,la,ja,xjr,lb,jb,',lleg,tmp,xjc,la,ja,xjr,lb,jb,outchnl(j)%kres
                     ! if(tmp==0.D0) cycle
                     if(dabs(tmp) < 1.d-14) cycle
                     tmp = tmp*xnor*out%t*out%rho
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
             ENDIF    !end of Legendre coeficients accumualtion

             CALL XSECT(nnuc,m,xnor,sumfis,sumfism,ke,ipar,jcn,fisxse)  !normalize SCRt matrices and store x-sec

             IF(LHRtw==1) THEN
                out => outchnl(i)
                SCRtl(-out%kres,out%nejc) = SCRtl(-out%kres,out%nejc) - elcor    !restore SCRtl before new elastic is calculated
             ENDIF

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

   if(benchm) THEN
      WRITE(8,*)
      WRITE(8,'(1x,'' WARNING: Gamma emission width not normalized in benchmark calculations'')')
      WRITE(8,*)
   endif

   itmp = iabs( NINT(1000*TUNe(0,nnuc)) - 999 )
   if(itmp.eq.1 .and. (.not.benchm)) THEN
      WRITE(8,'(1x,'' WARNING: Gamma emission width not normalized (TUNE set to 1.000 in input)'')')
      WRITE(8,*)
   endif

   if(itmp.gt.1 .and. (.not.benchm)) THEN
      WRITE (8 ,'('' WARNING: Gamma emission width from '',I3,A2,'' normalized by '',F7.3)') NINT(A(nnuc)), SYMb(nnuc), TUNe(0,nnuc)
      WRITE (12,'('' Gamma emission width from '',I3,A2,'' normalized by '',F7.3)') NINT(A(nnuc)), SYMb(nnuc), TUNe(0,nnuc)
      WRITE(8,*)
   endif

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

     WRITE(12,'(1x,A5,F9.3,A5,F8.3,A4)')'Gg = ', gg_obs, ' +/- ', gg_unc, ' meV'

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

     if(itmp==0 .and. (.not.benchm)) then
        IF(sumtg>0.D0 .AND. tgexper>0.D0) THEN
           tune(0,nnuc) = tgexper/sumtg
           WRITE(8,'(1x,'' WARNING: Gamma emission normalization factor is set to '',F7.3)') TUNe(0,nnuc)
           IF (first_ein) WRITE(8,'(1x,'' WARNING: The normalization is not applied to this incident energy'')')
        ELSE
           WRITE(8,'(1x,'' WARNING: Gamma emission width is not normalized to Do'')')
        ENDIF
        WRITE(8,*)
     endif

     if(itmp==1 .and. (.not.benchm) .and. (sumtg>0.D0 .and. tgexper>0.D0) ) then
        WRITE(8,'(1x,'' WARNING: Gamma emission could be normalized by setting TUNE to '',F7.3,'' in input'')')  tgexper/sumtg
        WRITE(8,*)
     endif

   ENDIF

   CALL DelHRTW()    !deallocate HRTW arrays

   RETURN
   END SUBROUTINE calc_hrtw

   !----------------------------------------------------------------------------------------------------

   real*8 function HRTW_DECAY(nnuc,iec,jc,ipc,nnur,nejc)
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

   ! Dummy arguments

   INTEGER*4, intent(in) :: ipc, jc, nejc, nnuc, nnur, iec

   ! Local variables

   REAL*8 :: eout, eoutc, frde, rho1, jmax, jmin, sumdl, tld, xjc, xj, xjr, summa
   INTEGER*4 :: i, ier, iermax, ietl, iexc, il, ip1, ipar, itlc, jr, k, kmax, kmin, nel, jndex

   type (channel), pointer :: out
   type (fusion),  pointer :: in

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
              IF(xj<jmin .or. xj>jmax) CYCLE
              DO ier = iermax, 1, -1
                 IF(INT(ZEJc(nejc))==0 .and. ier==iermax) CYCLE   ! omit top bin transition for neutrons
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
                    if(nch>ndhrtw1) call HRTW_error()    !STOP - insufficent space allocation
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

   DO i = 1, NLV(nnur)             ! do loop over inelastic levels, elastic done after the loop
     IF(IZA(nnur)==IZA(0) .AND. i==levtarg) CYCLE     !skip if elastic
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
           IF(xj<jmin .or. xj>jmax) CYCLE
           rho1 = 1.d0                                !reuse level density variable
           IF(IZA(nnur)==IZA(0)) rho1 = CINred(i)     !if inelastic - apply respective scaling
           tld = TLJ(il,k,jndex,nejc) + frde*(TLJ(il + 1,k,jndex,nejc) - TLJ(il,k,jndex,nejc))   !interpolate Tlj
           IF(tld<1.0d-15) CYCLE                      !ignore very small channels
           H_Sumtl = H_Sumtl + tld*rho1
           H_Sumtls = H_Sumtls + tld**2*rho1
           sumdl = sumdl + tld*rho1  !think there is no need for it
           IF(tld>H_Tthr) THEN
              nch = nch + 1                              !we've got non-zero channel
              if(nch>ndhrtw1) call HRTW_error()          !STOP - insiufficent space allocation
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
              SCRtl(i,nejc) = SCRtl(i,nejc) + tld*rho1
              H_Sweak = H_Sweak + tld*rho1
              H_Sweaks = H_Sweaks + tld**2*rho1
           ENDIF
        ENDDO     !do loop over 'jndex'    --------- done --------------------
     ENDDO    !do loop over 'l'            --------- done --------------------
   ENDDO    ! do loop over inelastic levels --------- done --------------------

   ! elastic channel

   IF(IZA(nnur)==IZA(0)) THEN
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
           IF(xj<jmin .or. xj>jmax) CYCLE
           tld = ELTLJ(k,jndex)                       !no IF - all elastic channels treated as 'strong'
           nch = nch + 1
           if(nch>ndhrtw1) call HRTW_error()          !STOP - insufficient space allocation
           IF(num%elal == 0) THEN
              num%elal = nch                          !memorize position of the first elastic in the 'outchnl' matrix
              num%elah = nch                          !set it also as the last one in case there are no more
           ENDIF
           IF(nch > num%elah) num%elah = nch          !if another elastic augment position of last elastic channel
           rho1 = celred
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

   return
   end function hrtw_decay

   !----------------------------------------------------------------------------------------------------

   real*8 function HRTW_DECAYG(nnuc,iec,jc,ipc)
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

   INTEGER*4, intent(in)  :: iec, ipc, jc, nnuc

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

   return
   end function hrtw_decayg

   !----------------------------------------------------------------------------------------------------

   SUBROUTINE HRTW_error()

   write(8,*) 'Insufficient space allocated for HRTW'
   write(8,*) 'NDHrtw1 in HRTW-mod.f90 needs to be increased'
   write(*,*) 'Insufficient space allocated for HRTW'
   write(*,*) 'NDHrtw1 in HRTW-mod.f90 needs to be increased'
   STOP 'Insufficient space allocated for HRTW'

   END SUBROUTINE HRTW_error

   !----------------------------------------------------------------------------------------------------

   real*8 function EEF(tl,tav,sumtl)

   !cc   *****************************************************************
   !cc   *                                                      Class:PPu*
   !cc   *                          E E F                                *
   !cc   *                                                               *
   !cc   * Calculates elastic enhancement factor for HRTW theory         *
   !cc   *                                                               *
   !cc   * input: Tl     - transmission coefficient                      *
   !cc   * Tav    - average of all transmission coefficients             *
   !cc   * Sumtl  - sum of transmission coefficients                     *
   !cc   *                                                               *
   !cc   * output: Eef - elastic enhancement factor                      *
   !cc   *                                                               *
   !cc   * Dummy arguments                                               *
   !cc   *                                                               *
   !cc   *                                                               *
   !cc   *****************************************************************C

   IMPLICIT NONE

   ! Dummy arguments

   REAL*8, intent(in) :: sumtl, tav, tl

   ! Local variables

   REAL*8 :: a, al

   IF(tl<1.0D-15) THEN
     eef = 3.D0
     RETURN
   ENDIF

   al = 4.D0*tav/sumtl*(1.D0 + tl/sumtl)/(1.D0 + 3.D0*tav/sumtl)
   a = 87.D0*(tl - tav)**2*tl**5/sumtl**7
   eef = 1.D0 + 2.D0/(1.D0 + tl**al) + a
   ! eef = 1.D0   !uncomment to eliminate elastic enhancement factor, i.e., no HRTW
   eef = min(eef,3.D0)

   RETURN
   END FUNCTION EEF

   !----------------------------------------------------------------------------------------------------

   SUBROUTINE AUSTER

   !cc   *****************************************************************
   !cc   *                                                      Class:PPu*
   !cc   *                      A U S T E R                              *
   !cc   *                                                               *
   !cc   * Iterates for V quantities in HRTW theory assuming that weak   *
   !cc   * transmission coefficients remain constant and therefore are   *
   !cc   * not iterated. All communication occures through the outchnl   *
   !cc   * structure in the HRTW_mod module.                             *
   !cc   *                                                               *
   !cc   *                                                               *
   !cc   *****************************************************************

   IMPLICIT NONE

   INTEGER*4 i, icount

   icount = 0
   outchnl(1:NCH)%ti1 = outchnl(1:NCH)%t                     !copy Tlj on the temporary 'ti1' location for iteration
   DO i = 1, NCH
     outchnl(i)%eef = EEF(outchnl(i)%t,H_Tav,H_Sumtl)        !calculate elastic enhancement for all channels
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

   end module HRTW