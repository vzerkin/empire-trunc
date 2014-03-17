! $Rev: 3900 $
! $Author: rcapote $
! $Date: 2014-03-02 $
!
!
SUBROUTINE HRTW
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
   INCLUDE 'dimension.h'
   INCLUDE 'global.h'
   !
   ! COMMON variables
   !
   REAL*8, DIMENSION(ndhrtw2,3) :: H_Abs
   REAL*8 :: H_Sumtl, H_Sumtls, H_Sweak, H_Tav, H_Tthr, TFIs
   REAL*8, DIMENSION(ndhrtw1,2) :: H_Tl
   INTEGER, DIMENSION(ndhrtw2,3) :: MEMel
   INTEGER :: NH_lch, NSCh
   COMMON /ihrtw / NSCh, MEMel, NH_lch
   COMMON /rhrtw / H_Tl, H_Sumtl, H_Sumtls, H_Sweak, H_Abs, H_Tav, H_Tthr, TFIs
   !
   ! Local variables
   !
   REAL*8 :: cnspin, fisxse, Summa, sumfis, sumg, sumtg, tgexper, xnor
   CHARACTER(1), DIMENSION(2) :: cpar
   REAL :: d0c
   INTEGER :: i, ich, ip, ipar, jcn, ke, m, ndivf, nejc, nhrtw, nnuc, nnur, itmp
   REAL*8, DIMENSION(nfmod) :: sumfism
   REAL*8 :: vt
   DATA cpar/'+', '-'/
   !
   ! threshold for considering channel as a 'strong' one
   H_Tthr = 0.0001D0
   ! set CN nucleus
   nnuc = 1
   ! reset variables
   !     sgamc = 0.d0
   CSFis = 0.D0
   sumfis = 0.D0
   !
   ! assure that full gamma cascade in the first CN is
   ! accounted for when width fluctuation (HRTW) is selected
   GCAsc = 1.0
   ke = NEX(nnuc)
   ! Initialize variables and print heading for normalizing g-strength function
   d0c = 0.D0
   sumtg = 0.D0
   tgexper = 0.D0


   IF(.not. BENchm) THEN
     WRITE(8,'(1x,''Renormalization of gamma-ray strength function'')')


     WRITE(8,'(1x,''-------------------------------------------------------------'')')


   ENDIF


   IF(FIRst_ein .AND. (EINl>1.D0) ) THEN
     WRITE(8,'(1x,'' WARNING: First incident energy Einc must be < 1MeV for Do and Gg calculations and'')')
     WRITE(8,'(1x,'' WARNING: for the renormalization of gamma-ray strength function'')')
   ENDIF
   ! 
   ! start CN nucleus decay
   ! 
   ! do loop over decaying nucleus parity
   DO ipar = 1, 2
      ip = (-1)**(ipar + 1)
      ! do loop over decaying nucleus spin
      DO jcn = 1, NLW
         ! write(8,*)'  '
         ! write(8,*)'  '
         ! write(8,*) 'DECAY STATE ke=',ke,' J=',jcn,' Pi=',ipar
         ! WRITE(8,*)'  '
         ! initialize variables
         nhrtw = 0
         DENhf = 0.D0
         NSCh = 0
         NH_lch = 0
         H_Sumtls = 0.D0
         H_Sumtl = 0.D0
         H_Tav = 0.D0
         H_Sweak = 0.D0
         H_Tl = 0.d0   

         ! prepare gamma-strength (GMR) parameters (if spin
         ! dependent GDR selected)
         IF(NINT(GDRdyn)==1) CALL ULMDYN(nnuc,jcn,EX(ke,nnuc))
         ! 
         ! start the first HRTW run
         ! 
         ! do loop over ejectiles
         !           write(8,*) 'START THE FIRST HRTW RUN'
         DO nejc = 1, NEJcm
            !              emitted nuclei must be heavier than alpha
            IF(NREs(nejc)<0)CYCLE
            nnur = NREs(nejc)
            Summa = 0.D0
            CALL HRTW_DECAY(nnuc,ke,jcn,ip,nnur,nejc,Summa,nhrtw)
            !              write(8,*)'sum for ejectile=' , nejc, sum
            H_Sumtl = H_Sumtl + Summa
         ENDDO
         ! if (nnuc.eq.1 .and. ke.eq.nex(1)) then
         ! write(8,*) 'sum for particles at J=',jcn, H_Sumtl
         ! write(*,*) 'sum for particles at J=',jcn, H_Sumtl
         ! endif
         ! do loop over ejectiles       ***done***
         !
         ! fission (may be a weak or strong channel)
         sumfis = 0.D0
         TFIs = 0.D0
         ndivf = 1
         IF(FISsil(nnuc)) THEN
            IF(nint(FISshi(nnuc))==1) THEN
               CALL FISSION(nnuc,ke,jcn,sumfis)
            ELSE
               CALL FISCROSS(nnuc,ke,ip,jcn,sumfis,sumfism)
            ! write(8,*) 'sum for fission at J= ',jcn, sumfis
            ENDIF
            H_Sumtl = H_Sumtl + sumfis
            ! Dividing sumfis into channels with TFIs < 0.25 each
            ndivf = int(sumfis/0.25) + 1
            TFIs = sumfis/dfloat(ndivf)
            H_Sumtls = H_Sumtls + ndivf*TFIs**2
            CALL TL2VL(TFIs,dfloat(ndivf))
         ENDIF
         ! gamma emission is always a weak channel (one iteration)
         sumg = 0.D0
         CALL HRTW_DECAYG(nnuc,ke,jcn,ip,sumg,nhrtw)
         H_Sumtl = H_Sumtl + sumg
         H_Sweak = H_Sweak + sumg
		 H_Tav = 0.d0
         IF(H_Sumtl.GT.0.0D0) H_Tav = H_Sumtls/H_Sumtl
         ! write(8,*)' '
         ! write(8,*)'SUMMARY OF THE FIRST HRTW RUN J=',jcn
         ! write(8,*)'total sum of  Tls ', H_Sumtl
         ! write(8,*)'sum of strong Tls ', H_Sumtl-H_Sweak
         ! write(8,*)'sum of weak   Tls ', H_Sweak
         ! write(8,*)'sum of gammas     ', sumg
         ! write(8,*)'sum fission       ', sumfis
         ! write(8,*)'sum Tl**2         ', H_Sumtls
         ! write(8,*)'# of strong Tls   ', NH_lch
         ! write(8,*)'average Tl        ', H_Tav
         ! write(8,*)'first entry DENhf=', denhf
         ! write(8,*)'all strong Tls    ', H_Tl
         ! 
         ! the first HRTW run completed
         ! 
         ! calculate V's for the strong channels (iteration)
         IF(NH_lch<=ndhrtw1)CALL AUSTER(H_Tl,H_Tav,H_Sumtl,H_Sweak,NH_lch,ndhrtw1)
         ! write(8,*)'all strong Vs     ', H_Tl
         ! write(8,*)'  '
         ! 
         ! start the second HRTW run
         ! 
         ! write(8,*) 'START SECOND HRTW RUN'
         ! calculate reaction cross section and its spin distribution
         ! split into contributions from individual partial waves
         CALL HRTW_MARENG(0,0,jcn,ipar,ich)
         !           write(8,*) 'called HRTW_MARENG ',ich,' channels'
         DO i = 1, ich
            NSCh = 0
            ! do loop over ejectiles
            nhrtw = i
            DENhf = 0.D0
            DO nejc = 1, NEJcm
               ! emitted nuclei must be heavier than alpha
               IF(NREs(nejc)<0)CYCLE
               nnur = NREs(nejc)
               ! WRITE(8,*)'  '
               ! write(8,*)'   secondary entry with ejec ' , nejc
               CALL HRTW_DECAY(nnuc,ke,jcn,ip,nnur,nejc,Summa,nhrtw)
            ! write(8,*)'DENhf after ejectile = ' ,nejc, Summa
            ENDDO
            ! do loop over ejectiles       ***done***
            ! fission channel - the second HRTW entry
            IF(sumfis>0.D0)sumfis = ndivf*vt(TFIs)
            ! write(8,*)'iteration sumfis = ',sumfis
            ! gamma emission - the second HRTW entry
            ! sumg = 0.d0
            ! CALL HRTW_DECAYG(nnuc,ke,jcn,ip,sumg,nhrtw)
            DENhf = DENhf + sumg + sumfis
            ! write(8,*)'iteration sumg=',sumg
            ! write(8,*)'channel ',i,' iteration DENhf=',denhf
            !
            ! correct scratch matrix for enhancement of the elastic channels
            ! 
            DO nejc = 1, NEJcm
               ! emitted nuclei must be heavier than alpha
               IF(NREs(nejc)<0)CYCLE
               nnur = NREs(nejc)
               IF(IZA(nnur)==IZA(0))CALL ELCORR(nnuc,ke,jcn,ip,nnur,nejc,nhrtw)
            ENDDO
            ! 
            ! normalization and accumulation
            ! 
            xnor = H_Abs(i,1)/DENhf
            ! IF (RO(ke,jcn,ipar,nnuc).NE.0.0D0) sgamc = sgamc + DENhf*H_Abs(i,1)/RO(ke,jcn,ipar,nnuc)
            CALL XSECT(nnuc,m,xnor,sumfis,sumfism,ke,ipar,jcn,fisxse)
         ENDDO    !loop over partial wave contributions to CN state
         !
         ! Gamma width calculation
         !
         IF((FIRst_ein .OR. BENchm) .AND. EINl<=1.D0) THEN
            cnspin = jcn - 0.5
            IF(mod(XJLv(LEVtarg,0)*2.,2.D+0)==1)cnspin = jcn - 1
            IF(ip==LVP(LEVtarg,0) .AND. ((cnspin==XJLv(LEVtarg,0)+0.5) .OR. (cnspin==XJLv(LEVtarg,0)-0.5))) THEN
               d0c = d0c + RO(ke,jcn,ipar,nnuc)
               ! write(8,*)'ke,jcn,ipar,ro',ke,jcn,ipar,RO(ke,jcn,ipar,nnuc)
               WRITE(8,'(A12,f4.1,A6,A1,A6,F7.3,A4,/,A37,d12.6)')'CN state J=', cnspin, ', Par:', cpar(ipar), ' at U=',&
                  EX(ke,nnuc), ' MeV', 'Int[Rho(U)*Tlg(U)] + Sum[Tlg(Ui)] = ', sumg
               sumtg = sumtg + sumg
            ENDIF
         ENDIF
      ENDDO       !loop over decaying nucleus spin
   ENDDO          !loop over decaying nucleus parity
   IF(d0c>0.D0)d0c = 1000.0/d0c
   IF(D0_obs==0.0D0)D0_obs = d0c    !use calculated D0 (in keV) if not measured

   if(BENchm) THEN
     WRITE(8,*)
     WRITE(8,'(1x,'' WARNING: Gamma emission width not normalized in benchmark calculations'')')
     WRITE(8,*)
   endif

   itmp = iabs( NINT(1000*TUNe(0,nnuc)) - 999 ) 
   if(itmp.eq.1 .and. (.not.BENCHM)) THEN
     WRITE(8,'(1x,'' WARNING: Gamma emission width not normalized (TUNE set to 1.000 in input)'')')
     WRITE(8,*)
   endif 

   if(itmp.gt.1 .and. (.not.BENCHM)) THEN
     WRITE (8 ,'('' WARNING: Gamma emission width from '',I3,A2,'' normalized by '',F7.3)') NINT(A(nnuc)), SYMb(nnuc), TUNe(0,nnuc)
     WRITE (12,'('' Gamma emission width from '',I3,A2,'' normalized by '',F7.3)') NINT(A(nnuc)), SYMb(nnuc), TUNe(0,nnuc)
     WRITE(8,*)
   endif

!  IF(EINl<=1.D0 .AND. (FIRst_ein .or. BENchm)) THEN
   IF(EINl<=1.D0 .AND. FIRst_ein) THEN
      IF(D0_obs>0.D0) THEN
         tgexper = 2*PI*GG_obs/D0_obs/1.E6
         WRITE(8,'(1x,''Experimental information from capture channel'')')
         WRITE(8,'(1x,A13,D12.6)')'2*pi*Gg/D0 = ', tgexper
      ENDIF
      IF(GG_unc>0.0D0) THEN
         WRITE(8,'(1x,A5,F9.3,A5,F8.3,A4)')'Gg = ', GG_obs, ' +/- ', GG_unc, ' meV'
      ELSE
         WRITE(8,'(1x,A5,F9.3,A18)')'Gg = ', GG_obs, ' meV (systematics)'
      ENDIF
      WRITE(12,'(1x,A5,F9.3,A5,F8.3,A4)')'Gg = ', GG_obs, ' +/- ', GG_unc, ' meV'
 
      IF(D0_obs>0.0D0) THEN
         WRITE(8,'(1x,A5,F11.6,A5,F11.6,A4)')'D0 = ', D0_obs, ' +/- ', D0_unc, ' keV'
         WRITE(8,'(1x,A5,F11.6,A17)')'D0 = ', d0c, ' keV (calculated)'
         WRITE(12,'(1x,''D0 = '',F8.3,'' keV'')')D0_obs
      ELSE
         WRITE(8,'(1x,A5,F11.6,A17)')'D0 = ', d0c, ' keV (calculated)'
         WRITE(12,'(1x,''D0 = '',F8.3,'' keV, CALC'')')d0c
      ENDIF


      WRITE(8,*)
      WRITE(12,*)
      if(itmp.eq.0 .and. (.not.BENCHM)) then 
        IF(sumtg>0.D0 .AND. tgexper>0.D0) THEN
          !
          ! renormalization of the gamma-ray strength function only
          ! undertaken for the first energy
          !
          TUNe(0,nnuc) = tgexper/sumtg
          WRITE(8,'(1x,'' WARNING: Gamma emission normalization factor is set to '',F7.3)') TUNe(0,nnuc)
          IF (FIRst_ein) WRITE(8,'(1x,'' WARNING: The normalization is not applied to this incident energy'')') 
        ELSE 
          WRITE(8,'(1x,'' WARNING: Gamma emission width is not normalized to Do'')')
        ENDIF
        WRITE(8,*)
      endif

   ENDIF
   RETURN
END SUBROUTINE HRTW

SUBROUTINE HRTW_DECAY(Nnuc,Iec,Jc,Ipc,Nnur,Nejc,Summa,Nhrtw)
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
   !cc   *       Nhrtw- 0 first HRTW entry (calculate Tl's summs)           *
   !cc   *             >0 final HRTW entry (calculate emission widths)      *
   !cc   *                                                                  *
   !cc   * output:Summa - SUM of transmission coefficients over all outgoing  *
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
   INCLUDE 'dimension.h'
   INCLUDE 'global.h'
   !
   ! COMMON variables
   !
   REAL*8, DIMENSION(ndhrtw2,3) :: H_Abs
   REAL*8 :: H_Sumtl, H_Sumtls, H_Sweak, H_Tav, H_Tthr, TFIs
   REAL*8, DIMENSION(ndhrtw1,2) :: H_Tl
   INTEGER, DIMENSION(ndhrtw2,3) :: MEMel
   INTEGER :: NH_lch, NSCh
   REAL*8, DIMENSION(ndlw) :: ELTl


   REAL*8, DIMENSION(ndlw,3) :: ELTlj


   COMMON /ELASTIC/ ELTl,ELTlj


   COMMON /ihrtw / NSCh, MEMel, NH_lch
   COMMON /rhrtw / H_Tl, H_Sumtl, H_Sumtls, H_Sweak, H_Abs, H_Tav, H_Tthr, TFIs
   !
   ! Dummy arguments
   !
   INTEGER :: Iec, Ipc, Jc, Nejc, Nhrtw, Nnuc, Nnur
   REAL*8 :: Summa
   !
   ! Local variables
   !
   REAL*8 :: corr, eout, eoutc, frde, hisr, rho1, rho2, s, smax, smin, sumdl, sumtl1, sumtl2, tld, xjc, xjr
   INTEGER :: i, ichsp, iel, ier, iermax, ietl, iexc, il, ip1, ip2, ipar, itlc, j, jr, l, lmax, lmaxf, lmin, mul
   REAL*8 :: vt
   ! write(8,*)'HRTW-DECAY entry: ejectile ,nhrtw ',nejc,nhrtw
   ! write(8,*)'CN bin, spin, parity',Iec,Jc,Ipc
   Summa = 0.D0                                               ! corrd,
   hisr = HIS(Nnur)
   xjc = float(Jc) + HIS(Nnuc)
   ! clear scratch matrices
   ! NSCh = 0
   SCRtem(Nejc) = 0.D0
   DO j = 1, ndlw    !NLW
      DO i = 1, ndex !NEX(Nnur) + 1
         SCRt(i,j,1,Nejc) = 0.D0
         SCRt(i,j,2,Nejc) = 0.D0
      ENDDO
   ENDDO
   iexc = NEX(Nnuc) - NEXr(Nejc,Nnuc)
   itlc = iexc - 5
   iermax = Iec - iexc
   IF(iermax>=1) THEN
      ! 
      ! decay to the continuum
      ! 
      ! 
      ! decay to the continuum
      ! 
      DO jr = 1, NLW            ! do loop over r.n. spins
         xjr = float(jr) + hisr
         smin = abs(xjr - SEJc(Nejc))
         smax = xjr + SEJc(Nejc)
         mul = int(smax - smin + 1.0001)
         ! write(8,*) 'mul', mul
         DO ichsp = 1, mul              ! do loop over channel spin
            s = smin + float(ichsp - 1)
            lmin = int(abs(xjc - s) + 1.01)
            lmaxf = int(xjc + s + 1.01)
            lmaxf = min0(NLW,lmaxf)
            ! write(8,*) 'lmaxf, xjc, s ',lmaxf, xjc, s
            ipar = (1 + Ipc*( - 1)**(lmin - 1))/2
            ! parity index of r.n. state populated by emission with LMIN
            ip1 = 2 - ipar
            ! parity index of r.n. state populated by emission with LMIN+1
            ip2 = 1
            IF(ip1==1)ip2 = 2
            !
            ! decay to the highest possible bin (non neutron only)
            !
            IF(nint(ZEJc(Nejc))/=0) THEN
               ! lmax = MIN0(LMAxtl(6,Nejc,Nnur),lmaxf)
               lmax = min0(LMAxtl(5,Nejc,Nnur),lmaxf)
               ! odd and even L-values treated separately
               ! ip1 and ip2 decide to which parity each SUMTL  goes
               rho1 = RO(iermax,jr,ip1,Nnur)*DE
               sumtl1 = 0.D0
               DO l = lmin, lmax, 2      ! do loop over L
                  IF(Nhrtw>0) THEN
                     ! replace Tl with V in the second HRTW entry
                     sumtl1 = sumtl1 + vt(TL(5,l,Nejc,Nnur))
                  ELSE
                     ! first entry with HRTW
                     ! WRITE(8,*)'A Tl= ' , TL(5,L,Nejc,Nnur)
                     CALL TL2VL(TL(5,l,Nejc,Nnur),rho1)
                     sumtl1 = sumtl1 + TL(5,l,Nejc,Nnur)
                  ENDIF
               ENDDO
               rho2 = RO(iermax,jr,ip2,Nnur)*DE
               sumtl2 = 0.D0
               DO l = lmin + 1, lmax, 2
                  IF(Nhrtw>0) THEN
                     ! replace Tl with V in the second HRTW entry
                     sumtl2 = sumtl2 + vt(TL(5,l,Nejc,Nnur))
                  ELSE
                     ! first entry with HRTW
                     ! WRITE(8,*)'B Tl= ' , TL(5,L,Nejc,Nnur)
                     CALL TL2VL(TL(5,l,Nejc,Nnur),rho2)
                     sumtl2 = sumtl2 + TL(5,l,Nejc,Nnur)
                  ENDIF
               ENDDO                     ! over L
               ! do loop over l   ***done***
               SCRt(iermax,jr,ip1,Nejc) = SCRt(iermax,jr,ip1,Nejc) + sumtl1*rho1/DE*TUNe(Nejc,Nnuc)
               SCRt(iermax,jr,ip2,Nejc) = SCRt(iermax,jr,ip2,Nejc) + sumtl2*rho2/DE*TUNe(Nejc,Nnuc)
               IF(iermax==1 .AND. nint(Z(1))==nint(Z(Nnur))) THEN
                  SCRt(iermax,jr,ip1,Nejc) = SCRt(iermax,jr,ip1,Nejc)*DEPart(Nnur)
                  SCRt(iermax,jr,ip2,Nejc) = SCRt(iermax,jr,ip2,Nejc)*DEPart(Nnur)
               ENDIF
            ENDIF
            !
            ! decay to the highest but one bin (conditional see the next IF)
            !
            IF(nint(ZEJc(Nejc))==0 .AND. Iec==NEX(Nnuc) - 1) THEN
               lmax = min0(LMAxtl(6,Nejc,Nnur),lmaxf)
               ! write(8,*) 'lmaxf top bin, xjc, s ',lmaxf, xjc, s
               ! do loop over L (odd and even l-values treated separately)
               ! IP1 and IP2 decide which parity each SUMTL  goes to
               rho1 = RO(iermax,jr,ip1,Nnur)*DE
               sumtl1 = 0.D0
               DO l = lmin, lmax, 2       ! do loop over L
                  IF(Nhrtw>0) THEN
                     ! replace Tl with V in the second HRTW entry
                     sumtl1 = sumtl1 + vt(TL(6,l,Nejc,Nnur))
                  ELSE
                     ! first entry with HRTW
                     ! WRITE(8,*)'C Tl= ' , TL(6,L,Nejc,Nnur)
                     CALL TL2VL(TL(6,l,Nejc,Nnur),rho1)
                     sumtl1 = sumtl1 + TL(6,l,Nejc,Nnur)
                  ENDIF
               ENDDO
               rho2 = RO(iermax,jr,ip2,Nnur)*DE
               sumtl2 = 0.D0
               DO l = lmin + 1, lmax, 2
                  IF(Nhrtw>0) THEN
                     ! replace Tl with V in the second HRTW entry
                     sumtl2 = sumtl2 + vt(TL(6,l,Nejc,Nnur))
                  ELSE
                     ! first entry with HRTW
                     ! WRITE(8,*)'D Tl= ' , TL(6,L,Nejc,Nnur)
                     CALL TL2VL(TL(6,l,Nejc,Nnur),rho2)
                     sumtl2 = sumtl2 + TL(6,l,Nejc,Nnur)
                  ENDIF
               ENDDO                      ! over L
               ! do loop over l   ***done***
               !
               ! corr in the next lines accounts for the Tl interpolation
               ! and integration over overlaping bins (2/3), it turned out it must
               ! be energy step and also emission step dependent
               corr = 0.4444D0/(DE - XN(Nnur) + XN(1))
               SCRt(iermax,jr,ip1,Nejc) = SCRt(iermax,jr,ip1,Nejc) + sumtl1*rho1/DE*TUNe(Nejc,Nnuc)*corr
               SCRt(iermax,jr,ip2,Nejc) = SCRt(iermax,jr,ip2,Nejc) + sumtl2*rho2/DE*TUNe(Nejc,Nnuc)*corr
               IF(iermax==1 .AND. nint(Z(1))==nint(Z(Nnur))) THEN
                  SCRt(iermax,jr,ip1,Nejc) = SCRt(iermax,jr,ip1,Nejc)*DEPart(Nnur)
                  SCRt(iermax,jr,ip2,Nejc) = SCRt(iermax,jr,ip2,Nejc)*DEPart(Nnur)
               ENDIF
            ! write(8,*) 'Last but one bin', iermax
            ! write(8,*) 'jr, corr, sumtl1,2', jr, corr, sumtl1, sumtl2
            ! write(8,*) 'SCRt top',SCRt(iermax,jr,ip1,Nejc), SCRt(iermax,jr,ip2,Nejc)
            ENDIF
            !
            ! do loop over r.n. energies (highest bin and eventually the second
            ! bin from the top excluded as already done)
            !
            ! write(8,*) 'Remaining bins'
            DO ier = iermax - 1, 1, -1
               ietl = Iec - ier - itlc
               lmax = min0(LMAxtl(ietl,Nejc,Nnur),lmaxf)
               ! IF (ier.EQ.1) THEN
               ! corr = 0.5d0
               ! ELSE
               ! corr = 1.d0
               ! ENDIF
               ! do loop over L (odd and even L-values treated separately)
               ! IP1 and IP2 decide which parity each SUMTL  goes to
               sumtl1 = 0.D0
               rho1 = RO(ier,jr,ip1,Nnur)*DE
                                           !*corr
               DO l = lmin, lmax, 2
                  IF(Nhrtw>0) THEN
                     ! replace Tl with V in the second HRTW entry
                     sumtl1 = sumtl1 + vt(TL(ietl,l,Nejc,Nnur))
                  ELSE
                     ! first entry with HRTW
                     ! WRITE(8,*)'E Tl= ' , TL(ietl,L,Nejc,Nnur)
                     CALL TL2VL(TL(ietl,l,Nejc,Nnur),rho1)
                     sumtl1 = sumtl1 + TL(ietl,l,Nejc,Nnur)
                  ENDIF
               ENDDO
               sumtl2 = 0.D0
               rho2 = RO(ier,jr,ip2,Nnur)*DE
               !*corr
               DO l = lmin + 1, lmax, 2
                  IF(Nhrtw>0) THEN
                     ! replace Tl with V in the second HRTW entry
                     sumtl2 = sumtl2 + vt(TL(ietl,l,Nejc,Nnur))
                  ELSE
                     ! first entry with HRTW
                     ! WRITE(8,*)'F Tl= ' , TL(ietl,L,Nejc,Nnur)
                     CALL TL2VL(TL(ietl,l,Nejc,Nnur),rho2)
                     sumtl2 = sumtl2 + TL(ietl,l,Nejc,Nnur)
                  ENDIF
               ENDDO
               ! do loop over L   ***done***
               !
               SCRt(ier,jr,ip1,Nejc) = SCRt(ier,jr,ip1,Nejc) + sumtl1*rho1/DE*TUNe(Nejc,Nnuc)
               !                 &               + sumtl1*rho1/DE/corr*TUNe(Nejc,Nnuc)
               SCRt(ier,jr,ip2,Nejc) = SCRt(ier,jr,ip2,Nejc) + sumtl2*rho2/DE*TUNe(Nejc,Nnuc)
               !                 &               + sumtl2*rho2/DE/corr*TUNe(Nejc,Nnuc)
               IF(ier==1 .AND. nint(Z(1))==nint(Z(Nnur))) THEN
                  SCRt(ier,jr,ip1,Nejc) = SCRt(ier,jr,ip1,Nejc)*DEPart(Nnur)
                  SCRt(ier,jr,ip2,Nejc) = SCRt(ier,jr,ip2,Nejc)*DEPart(Nnur)
               ENDIF
            ! write(8,*) 'ietl, lmin, lmax', ietl, lmin, lmax
            ! write(8,*) 'ier, sumtl1,2', ier, sumtl1, sumtl2
            ! write(8,*) 'SCRt ',SCRt(ier,jr,ip1,Nejc), SCRt(ier,jr,ip2,Nejc)
            ENDDO               ! over r.n. energies
         ENDDO           ! over channel spins
      ENDDO        ! over and r.n. spins
      ! trapezoidal integration of ro*tl in continuum for ejectile nejc
      DO j = 1, NLW
         DO i = 1, iermax
            Summa = Summa + SCRt(i,j,1,Nejc) + SCRt(i,j,2,Nejc)
         ENDDO
         Summa = Summa - 0.5*(SCRt(1,j,1,Nejc) + SCRt(1,j,2,Nejc))
      ENDDO
      Summa = Summa*DE
   ! write(8,*)'Summa to continuum for ejectile ', Nejc, Summa
   ! integration of ro*tl in continuum for ejectile nejc -- done ----
   !
   ! if(nnuc.eq.1 .and. Nnur.eq.2 .and. Summa.gt.0.d0) then
   ! write(*,*) 'HRTW=',Iec,Jc,Ipc
   ! write(*,*) '     ',Nhrtw,sngl(Summa)
   ! endif
   !
   ! 
   ! decay to discrete levels
   ! 
   ENDIF
 
   DO i = 1, ndlv !NLV(Nnur)
      SCRtl(i,Nejc) = 0.D0
   ENDDO
   IF(Nhrtw==0 .AND. IZA(Nnur)==IZA(0)) THEN
      ! clear memorized elastic channels when entering new J-pi CN state
      iel = 1


        MEMel = 0
   ENDIF
 
   eoutc = EX(Iec,Nnuc) - Q(Nejc,Nnuc)
   ! do loop over inelastic levels ------------------------------------
   DO i = 1, NLV(Nnur)
      ! Elastic channels excluded, done after the loop
      IF(IZA(Nnur)==IZA(0) .AND. i==LEVtarg)CYCLE
 
      eout = eoutc - ELV(i,Nnur)
      sumdl = 0.D0
      IF(eout<0.0D0)EXIT
 
      CALL TLLOC(Nnur,Nejc,eout,il,frde)
      smin = abs(XJLv(i,Nnur) - SEJc(Nejc))
      smax = XJLv(i,Nnur) + SEJc(Nejc) + 0.01
      s = smin
      DO
         ! loop over channel spin ----------------------------------------
         lmin = int(abs(xjc - s) + 1.01)
         lmax = int(xjc + s + 1.01)
         lmax = min0(NLW,lmax)
         ! do loop over L ------------------------------------------------
         DO l = lmin, lmax
            ipar = 1 + LVP(i,Nnur)*Ipc*( - 1)**(l - 1)
            IF(ipar==0)CYCLE
            tld = TL(il,l,Nejc,Nnur) + frde*(TL(il + 1,l,Nejc,Nnur) - TL(il,l,Nejc,Nnur))
            IF(tld<=0.0D0)CYCLE
            IF(Nhrtw>0) THEN
               ! entry with nhrtw>0
               IF(IZA(Nnur)==IZA(0)) THEN
                  sumdl = sumdl + vt(tld)*CINred(i)
               ELSE
                  sumdl = sumdl + vt(tld)
               ENDIF
            ! entry with nhrtw=0
            ELSEIF(IZA(Nnur)==IZA(0)) THEN
               CALL TL2VL(tld,CINred(i))
               sumdl = sumdl + tld*CINred(i)
            ELSE
               CALL TL2VL(tld,1.D0)
               sumdl = sumdl + tld
            ENDIF
         ! WRITE(8,*)'sumdl,tld ',sumdl,tld
         ENDDO
         ! do loop over L --- done ----------------------------------------
         s = s + 1.
         IF(s<=smax)CYCLE
         ! loop over channel spin ------ done ----------------------------
         SCRtl(i,Nejc) = sumdl
         ! *CINRED(i)
         Summa = Summa + sumdl  ! *CINRED(i)
         EXIT
      ENDDO
   ! if(nhrtw.eq.0) write(8,*) 'Sum to level i=', i,sumdl
   ! write(8,*)'sum to discrete for ejectile ', Nejc, Sumdl
   ! write(8,*)'sum for ejectile ', Nejc, Summa, 'iteration ',nhrtw
   ENDDO
   ! do loop over inelastic levels --------- done --------------------
   ! 
   ! elastic channel
   IF(IZA(Nnur)==IZA(0)) THEN
      i = LEVtarg
      eout = eoutc - ELV(i,Nnur)
      sumdl = 0.D0
      IF(eout<0.0D0)GOTO 10
 
      ! CALL TLLOC(Nnur,Nejc,eout,il,frde)
      smin = abs(XJLv(i,Nnur) - SEJc(Nejc))
      smax = XJLv(i,Nnur) + SEJc(Nejc) + 0.01
      s = smin
      DO
         ! loop over channel spin ----------------------------------------
         lmin = int(abs(xjc - s) + 1.01)
         lmax = int(xjc + s + 1.01)
         lmax = min0(NLW,lmax)
         ! do loop over L ------------------------------------------------
         DO l = lmin, lmax
            ipar = 1 + LVP(i,Nnur)*Ipc*( - 1)**(l - 1)
            IF(ipar==0)CYCLE
            ! tld = TL(il,L,Nejc,Nnur) + frde*(TL(il + 1,L,Nejc,Nnur) - TL(il,L,Nejc,Nnur))
            tld = ELTl(l)
            IF(tld<=0.0D0)CYCLE
 
            ! write(8,*) 'Elastic L=',L,' Tl=',tld
            IF(Nhrtw>0) THEN
               ! entry with nhrtw>0
               sumdl = sumdl + vt(tld)*CELred
            ! sumdl = sumdl + VT(tld)
            ELSE
               ! entry with nhrtw=0
               ! CALL TL2VL(tld,1.d0)
               ! sumdl = sumdl + tld
               CALL TL2VL(tld,CELred)
               sumdl = sumdl + tld*CELred
               ! WRITE(8,*)'sumdl,tld,cor ',sumdl,tld,cor
               IF(tld>H_Tthr) THEN
                  ! case of a strong elastic channel
                  ! record position of Tl, l and channel spin
                  MEMel(iel,1) = NH_lch
                  MEMel(iel,2) = l
                  MEMel(iel,3) = int(2.0*s)
                  ! WRITE(8,*)'got elastic iel ', iel, '  MEM# ',MEMel(iel,1), '  MEMk ',MEMel(iel,2), '  MEM2s ',MEMel(iel,3)
                  iel = iel + 1
               ENDIF
            ENDIF
         ENDDO
         ! do loop over L --- done ----------------------------------------
         s = s + 1.
         IF(s<=smax)CYCLE
         ! loop over channel spin ------ done ----------------------------
         SCRtl(i,Nejc) = sumdl
         !*CELred
         Summa = Summa + sumdl  !*CELred
         EXIT
      ENDDO
   ! if(nhrtw.eq.0) then
   ! write(8,*) 'Sum to elastic', sumdl !*CELRED
   ! write(8,*) 'Sum to levels', Summa    !*CELRED
   ! endif
   ! WRITE(8,*)'i,sumdl,nejc,nhrtw ', i,sumdl,nejc,nhrtw
   ENDIF !end of elastic
 
   ! do loop over discrete levels --------- done --------------------
10 DENhf = DENhf + Summa
   SCRtem(Nejc) = Summa
   ! write(8,*) 'DENhf after particle ',Nejc, Summa
   ! decay to the continuum and discrete levels ------ done -----------------------------
   RETURN
END SUBROUTINE HRTW_DECAY

SUBROUTINE HRTW_DECAYG(Nnuc,Iec,Jc,Ipc,Summa,Nhrtw)
   !cc
   !cc   ********************************************************************
   !cc   *                                                         class:PPu*
   !cc   *                         D E C A Y G                              *
   !cc   *                (function to function version)                    *
   !cc   *                                                                  *
   !cc   * Calculates gamma decay of a continuum state in nucleus NNUC into *
   !cc   * continuum and discrete states in the same nucleus NNUC           *
   !cc   *                                                                  *
   !cc   *                                                                  *
   !cc   * input:NNUC - decaying nucleus index                              *
   !cc   *       IEC  - energy index of the decaying state                  *
   !cc   *       JC   - spin index of the decaying state                    *
   !cc   *       IPC  - parity of the decaying state                        *
   !cc   *       nhrtw- 0 first HRTW entry (calculate Tl's summs)           *
   !cc   *             >0 final HRTW entries (actually not used)            *
   !cc   *                                                                  *
   !cc   *                                                                  *
   !cc   * output:Summa - Sum of transmission coefficients over all gamma     *
   !cc   *              channels.                                           *
   !cc   *              Apart of this standard feature it comunicates       *
   !cc   *              quantities needed for the HRTW theory through       *
   !cc   *              the HRTW common.  Note that gamma channels          *
   !cc   *              contribute to the sum of weak channels but their    *
   !cc   *              transmission coefficients are assumed to remain     *
   !cc   *              constant when HRTW is applied.                      *
   !cc   *                                                                  *
   !cc   * calls:none                                                       *
   !cc   *                                                                  *
   !cc   *                                                                  *
   !cc   *                                                                  *
   !cc   ********************************************************************
   IMPLICIT NONE
   INCLUDE 'dimension.h'
   INCLUDE 'global.h'
   !
   ! COMMON variables
   !
   REAL*8, DIMENSION(ndhrtw2,3) :: H_Abs
   REAL*8 :: H_Sumtl, H_Sumtls, H_Sweak, H_Tav, H_Tthr, TFIs
   REAL*8, DIMENSION(ndhrtw1,2) :: H_Tl
   COMMON /rhrtw / H_Tl, H_Sumtl, H_Sumtls, H_Sweak, H_Abs, H_Tav, H_Tthr, TFIs
   !
   ! Dummy arguments
   !
   INTEGER :: Iec, Ipc, Jc, Nhrtw, Nnuc
   REAL*8 :: Summa
   !
   ! Local variables
   !
   REAL*8 :: cee, cme, eg, ha, hscrtl, hsumtls, scrtneg, scrtpos, xjc, xjr
   REAL*8 :: e1, e2, xm1
   INTEGER :: i, ier, ineg, iodd, ipar, ipos, j, jmax, jmin, jr, lamb, lambmax, lambmin, lmax, lmin
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
   !
   !
   DO i = 1, MAXmult
      xle(i) = 0.0D0
      xlm(i) = 0.0D0
   ENDDO
   ha = A(Nnuc)**0.666666666666D0
   cee = 3.7D-5*ha
   cme = 0.307D0/ha
   ! IF (MAXmult.GT.2) THEN
   ! ha = A(Nnuc)**0.666666666666D0
   ! cee = 3.7D-5*ha
   ! cme = 0.307D0/ha
   ! ENDIF
   !
   jmin = 1
   jmax = min0(NLW,Jc + MAXmult)
 
   Summa = 0.D0
   SCRtem(0) = 0.D0
   xjc = float(Jc) + HIS(Nnuc)
 
   ! clear scratch matrix (continuum)
   DO j = 1, ndlw    ! NLW
      DO i = 1, ndex !NEX(Nnuc)
         SCRt(i,j,1,0) = 0.D0
         SCRt(i,j,2,0) = 0.D0
      ENDDO
   ENDDO
   ! clear scratch matrix (discrete levels)
   DO i = 1, ndlv ! NLV(Nnuc)
      SCRtl(i,0) = 0.D0
   ENDDO
   ! IPOS is a parity-index of final states reached by gamma
   ! transitions which do not change parity (E2 and M1)
   ! INEG is a parity-index of final states reached by gamma
   ! transitions which do change parity (E1)
   IF(Iec<1)RETURN
   IF(Ipc>0) THEN
      ipos = 1
      ineg = 2
   ELSE
      ipos = 2
      ineg = 1
   ENDIF
   ! 
   ! decay to the continuum
   ! 
   ! do loop over c.n. energies (loops over spins and parities expanded)
   DO ier = Iec - 1, 1, -1
      eg = EX(Iec,Nnuc) - EX(ier,Nnuc)
      xle(1) = e1(Nnuc,eg,TNUc(ier,Nnuc),UEXcit(ier,Nnuc))*TUNe(0,Nnuc)
      xlm(1) = xm1(eg)*TUNe(0,Nnuc)
      xle(2) = e2(eg)*TUNe(0,Nnuc)
      xlm(2) = xle(2)*cme
      IF(MAXmult>2) THEN
         DO i = 3, MAXmult
            xle(i) = xle(i - 1)*eg**2*cee*((3.0D0 + float(i))/(5.0D0 + float(i)))**2
            xlm(i) = xle(i)*cme
         ENDDO
      ENDIF
      DO jr = 1, jmax
         xjr = float(jr) + HIS(Nnuc)
         lambmin = max0(1,abs(Jc - jr))
         lambmax = xjc + xjr + 0.001
         lambmax = min0(lambmax,MAXmult)
         IF(lambmin<=lambmax) THEN
            scrtpos = 0.0D0
            scrtneg = 0.0D0
            hsumtls = 0.0D0
            DO lamb = lambmin, lambmax
               IF(lamb/2*2==lamb) THEN
                  scrtpos = scrtpos + xle(lamb)
                  scrtneg = scrtneg + xlm(lamb)
                  IF(Nhrtw==0)hsumtls = hsumtls + xle(lamb)**2*RO(ier,jr,ipos,Nnuc) + xlm(lamb)**2*RO(ier,jr,ineg,Nnuc)
               ELSE
                  scrtpos = scrtpos + xlm(lamb)
                  scrtneg = scrtneg + xle(lamb)
                  IF(Nhrtw==0)hsumtls = hsumtls + xlm(lamb)**2*RO(ier,jr,ipos,Nnuc) + xle(lamb)**2*RO(ier,jr,ineg,Nnuc)
               !                    !first HRTW entry done
               ENDIF
            ENDDO
            SCRt(ier,jr,ipos,0) = scrtpos*RO(ier,jr,ipos,Nnuc)
            SCRt(ier,jr,ineg,0) = scrtneg*RO(ier,jr,ineg,Nnuc)
            IF(ier==1 .AND. nint(Z(1))==nint(Z(Nnuc))) THEN
               SCRt(ier,jr,ipos,0) = SCRt(ier,jr,ipos,0)*DEPart(Nnuc)
               SCRt(ier,jr,ineg,0) = SCRt(ier,jr,ineg,0)*DEPart(Nnuc)
            ENDIF
            !
            ! Check, it could be we need to split hsumtls depending on parity !!!!
            !
            H_Sumtls = H_Sumtls + hsumtls
         ENDIF
      ENDDO
   ENDDO
   ! do loop over c.n. energies ***done***
   ! decay to the continuum ----** done***---------------------------
   ! integration of ro*gtl in continuum for ejectile 0 (TRAPEZOID
   DO j = jmin, jmax
      DO i = 1, Iec - 1
         Summa = Summa + SCRt(i,j,1,0) + SCRt(i,j,2,0)
      ENDDO
      Summa = Summa - 0.5*(SCRt(1,j,1,0) + SCRt(1,j,2,0))
   ENDDO
   Summa = Summa*DE
   ! integration of ro*gtl in continuum for ejectile 0 -- done ----
   ! 
   ! DECAY TO DISCRETE LEVELS
   ! 
   ! do loop over discrete levels -----------------------------------
   DO i = 1, NLV(Nnuc)
      lmin = abs(xjc - XJLv(i,Nnuc)) + 0.001
      lmax = xjc + XJLv(i,Nnuc) + 0.001
      lambmin = max0(1,lmin)
      lambmax = min0(lmax,MAXmult)
      IF(lambmin<=lambmax) THEN
         eg = EX(Iec,Nnuc) - ELV(i,Nnuc)
         ipar = (1 + LVP(i,Nnuc)*Ipc)/2
         iodd = 1 - ipar
         xle(1) = e1(Nnuc,eg,TNUc(1,Nnuc),UEXcit(1,Nnuc))*TUNe(0,Nnuc)
         xlm(1) = xm1(eg)*TUNe(0,Nnuc)
         xle(2) = e2(eg)*TUNe(0,Nnuc)
         IF(lambmax>2) THEN
            xlm(2) = xle(2)*cme
            DO j = 3, lambmax
               xle(j) = xle(j - 1)*eg**2*cee*((3.0D0 + float(j))/(5.0D0 + float(j)))**2
               xlm(j) = xle(j)*cme
            ENDDO
         ENDIF
         hscrtl = 0.0D0
         hsumtls = 0.0D0
         DO lamb = lambmin, lambmax
            IF(lamb/2*2==lamb) THEN
               hscrtl = hscrtl + xle(lamb)*ipar + xlm(lamb)*iodd
               IF(Nhrtw==0)hsumtls = hsumtls + xle(lamb)**2*ipar + xlm(lamb)**2*iodd
            ELSE
               hscrtl = hscrtl + xlm(lamb)*ipar + xle(lamb)*iodd
               IF(Nhrtw==0)hsumtls = hsumtls + xlm(lamb)**2*ipar + xle(lamb)**2*iodd
            ENDIF
         ENDDO
         IF(Nhrtw==0)H_Sumtls = H_Sumtls + hsumtls
         SCRtl(i,0) = hscrtl
         Summa = Summa + hscrtl
      ENDIF
   ENDDO
   ! do loop over discrete levels --------- done --------------------
   SCRtem(0) = Summa
   DENhf = DENhf + Summa
END SUBROUTINE HRTW_DECAYG

SUBROUTINE TL2VL(T,Rho)
   !cc
   !cc   ********************************************************************
   !cc   *                                                         class:PPu*
   !cc   *                      T L 2 V L                                   *
   !cc   *                                                                  *
   !cc   * Service routine for HRTW. It stores strong transmission          *
   !cc   * coefficients, calculates sum of Tl**2, and sum of weak Tl's.     *
   !cc   * On entry with nhrtw>0 returns V that substitute Tl in the HF     *
   !cc   * formula.                                                         *
   !cc   *                                                                  *
   !cc   *                                                                  *
   !cc   * input:T    - transmission coefficient                            *
   !cc   *       Rho  - number of channels with this transmission coeff.    *
   !cc   *                                                                  *
   !cc   * output:      quantities needed for the HRTW theory (through      *
   !cc   *              the HRTW common):                                   *
   !cc   *                                                                  *
   !cc   *             H_Tl(.,.) - list of strong Tl's and their num. (rho) *
   !cc   *             H_Tloc(.,.) - l and ejectile for each strong Tl      *
   !cc   *             H_sumtls - sum of squared Tl's (all)                 *
   !cc   *             H_sweak - sum of weak Tl's (not squared)             *
   !cc   *             H_lch - number of strong Tl's                        *
   !cc   *             H_abs(.,.) - absorption x-sec decomposed in l's      *
   !cc   *             H_Tav - average transmission coefficient             *
   !cc   *             H_Tthr - threshold to consider Tl as strong          *
   !cc   *                                                                  *
   !cc   *                                                                  *
   !cc   * calls:                                                           *
   !cc   *                                                                  *
   !cc   *                                                                  *
   !cc   ********************************************************************
   !cc
   IMPLICIT NONE
   INCLUDE 'dimension.h'
   !
   ! COMMON variables
   !
   REAL*8, DIMENSION(ndhrtw2,3) :: H_Abs
   REAL*8 :: H_Sumtl, H_Sumtls, H_Sweak, H_Tav, H_Tthr, TFIs
   REAL*8, DIMENSION(ndhrtw1,2) :: H_Tl
   INTEGER, DIMENSION(ndhrtw2,3) :: MEMel
   INTEGER :: NH_lch, NSCh
   COMMON /ihrtw / NSCh, MEMel, NH_lch
   COMMON /rhrtw / H_Tl, H_Sumtl, H_Sumtls, H_Sweak, H_Abs, H_Tav, H_Tthr, TFIs
   !
   ! Dummy arguments
   !
   REAL*8 :: Rho, T
   H_Sumtls = H_Sumtls + T**2*Rho
   IF(T>H_Tthr) THEN
      !        !strong or weak
      NH_lch = NH_lch + 1
      IF(NH_lch<=ndhrtw1) THEN
         H_Tl(NH_lch,1) = T      !record strong Tl's
         H_Tl(NH_lch,2) = Rho
      ENDIF
   ELSE
      H_Sweak = H_Sweak + T*Rho
   ENDIF
   RETURN
END SUBROUTINE TL2VL
 
FUNCTION VT(Tl)
   !cc   *********************************************************************
   !Cc   *                                                          Class:PPu*
   !Cc   *                            V T                                    *
   !cc   *                                                                   *
   !cc   * Returns value of the HRTW quantity V corresponding to the         *
   !cc   * provided transmission coefficient (if the                         *
   !cc   * latter is below the threshold value H_Tthr or total number        *
   !cc   * of strong channels is bigger that NDHRTW1). If number of          *
   !cc   * strong channels is lower than NDHRTW1 and Tl is above the         *
   !cc   * threshold value H_Tthr then precalculated H_Tl(nsch) is returned. *
   !cc   * Note that VT keeps track of the number of processed strong        *
   !cc   * channels in the variable NSCh.                                    *
   !cc   *                                                                   *
   !cc   * input: Tl    - transmission coefficient                           *
   !cc   *                                                                   *
   !cc   * output: Tl                                                        *
   !cc   *                                                                   *
   !cc   *                                                                   *
   !cc   *********************************************************************
   IMPLICIT NONE
   INCLUDE 'dimension.h'
   !
   ! COMMON variables
   !
   REAL*8, DIMENSION(ndhrtw2,3) :: H_Abs
   REAL*8 :: H_Sumtl, H_Sumtls, H_Sweak, H_Tav, H_Tthr, TFIs
   REAL*8, DIMENSION(ndhrtw1,2) :: H_Tl
   INTEGER, DIMENSION(ndhrtw2,3) :: MEMel
   INTEGER :: NH_lch, NSCh
   COMMON /ihrtw / NSCh, MEMel, NH_lch
   COMMON /rhrtw / H_Tl, H_Sumtl, H_Sumtls, H_Sweak, H_Abs, H_Tav, H_Tthr, TFIs
   !
   ! Dummy arguments
   !
   REAL*8 :: Tl
   REAL*8 :: vt
   !
   ! Local variables
   !
   REAL*8 :: vt1
   IF(NH_lch>ndhrtw1) THEN
      vt = vt1(Tl,H_Tav,H_Sumtl)
   ELSEIF(Tl<H_Tthr) THEN
      vt = vt1(Tl,H_Tav,H_Sumtl)
   ELSE
      NSCh = NSCh + 1
      vt = H_Tl(NSCh,1)
   ENDIF
   RETURN
END FUNCTION VT
 
FUNCTION VT1(Tl,Tav,Sumtl)
   !cc   *****************************************************************
   !cc   *                                                      Class:PPu*
   !cc   *                          V T 1                                *
   !cc   *                                                               *
   !cc   * calculates V quantities replacing Tl's in the HRTW theory.    *
   !cc   * Performs just one iteration. To be used when number of        *
   !cc   * strong channels is bigger than NDRTW1.                        *
   !cc   *                                                               *
   !cc   * input: Tl    - transmission coefficient                       *
   !cc   * Tav   - average transmission coefficeint                      *
   !cc   * Sumtl - sum of transmission coefficients                      *
   !cc   *                                                               *
   !cc   * output: VT1                                                   *
   !cc   *                                                               *
   !cc   * Dummy arguments                                               *
   !cc   *                                                               *
   !cc   *                                                               *
   !cc   *****************************************************************
   !C
   IMPLICIT NONE
   !
   ! Dummy arguments
   !
   REAL*8 :: Sumtl, Tav, Tl
   REAL*8 :: vt1
   !
   ! Local variables
   !
   REAL*8 :: eef
   vt1 = 0.D0
   IF(Sumtl==0.0D0)RETURN
   vt1 = Tl/Sumtl
   vt1 = 1.D0 + vt1*(eef(Tl,Tav,Sumtl) - 1.D0)
   vt1 = Tl/vt1
   RETURN
END FUNCTION VT1
 
FUNCTION EEF(Tl,Tav,Sumtl)
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
   !
   ! Dummy arguments
   !
   REAL*8 :: Sumtl, Tav, Tl
   REAL*8 :: eef
   !
   ! Local variables
   !
   REAL*8 :: a, al
   IF(Tl<1.0D-15) THEN
      eef = 3.D0
      RETURN
   ENDIF
   al = 4*Tav/Sumtl*(1.D0 + Tl/Sumtl)/(1.D0 + 3*Tav/Sumtl)
   a = 87.D0*(Tl - Tav)**2*Tl**5/Sumtl**7
   eef = 1.D0 + 2.D0/(1.D0 + Tl**al) + a
   IF(eef>3.D0)eef = 3.D0
   RETURN
END FUNCTION EEF
 
SUBROUTINE AUSTER(V,Tav,Sumtl,Sweak,Lch,Ndhrtw1)
   !cc   *****************************************************************
   !cc   *                                                      Class:PPu*
   !cc   *                      A U S T E R                              *
   !cc   *                                                               *
   !cc   * Iterates for V quantities in HRTW theory assuming that weak   *
   !cc   * transmission coefficients remain constant and therefore are   *
   !cc   * not iterated.                                                 *
   !cc   *                                                               *
   !cc   * input: Tav   - average transmission coefficient               *
   !cc   * Sumtl - sum of all transmission coefficients                  *
   !cc   * Sweak - sum of weak (Tl<H_Tthr) transmission coefficients     *
   !cc   * Lch   - number of strong (Tl>H_Tthr) channels                 *
   !cc   *                                                               *
   !cc   * input/output: V    - matrix of Tl's (input) or V's (output)   *
   !cc   *                                                               *
   !cc   *                                                               *
   !cc   *                                                               *
   !cc   * Dummy arguments                                               *
   !cc   *                                                               *
   !cc   *                                                               *
   !cc   *****************************************************************
   IMPLICIT NONE
   !
   ! Dummy arguments
   !
   INTEGER :: Lch, Ndhrtw1
   REAL*8 :: Sumtl, Sweak, Tav
   REAL*8, DIMENSION(Ndhrtw1,2) :: V
   !
   ! Local variables
   !
   REAL*8, DIMENSION(Ndhrtw1) :: e, vd, vp
   REAL*8 :: eef
   INTEGER :: i, icount
   REAL*8 :: Summa, sv
   IF(Lch>Ndhrtw1) THEN
      WRITE(8,*)'ERROR in AUSTER: Lch bigger than allowed by NDHRTW1'
      WRITE(8,*)'If you see this printed it means a BUG!'
      STOP
   ENDIF
   icount = 0
   sv = Sweak
   Summa = Sweak
   DO i = 1, Lch
      e(i) = eef(V(i,1),Tav,Sumtl) - 1.D0
      Summa = Summa + V(i,1)*V(i,2)
   ENDDO
   DO i = 1, Lch
      vd(i) = V(i,1)
      vp(i) = V(i,1)
   ENDDO
   10 DO i = 1, Lch
      vp(i) = V(i,1)/(1.D0 + vp(i)*e(i)/Summa)
      sv = sv + vp(i)*V(i,2)
   ENDDO
   icount = icount + 1
   IF(icount>200) THEN
      WRITE(8,*)' Maximum iteration number (200) reached in AUSTER'
      WRITE(8,*)
      RETURN
   ENDIF
   Summa = sv
   sv = Sweak
   DO i = 1, Lch
      !
      ! relative accuracy of V is set below and may be altered
      ! to any resonable value.
      !
      ! IF (ABS(vd(i)-vp(i)).GT.1.D-7*vp(i)) GOTO 200
      IF(abs(vd(i) - vp(i))>1.D-5*vp(i))GOTO 20
 
   ENDDO
   DO i = 1, Lch
      V(i,1) = vp(i)
   ENDDO
 
   RETURN
   20 DO i = 1, Lch
      vd(i) = vp(i)
   ENDDO
   GOTO 10
END SUBROUTINE AUSTER
 
SUBROUTINE ELCORR(Nnuc,Iec,Jc,Ipc,Nnur,Nejc,Nhrtw)
   !cc
   !cc   ********************************************************************
   !cc   *                                                         class:PPu*
   !cc   *                      E L C O R R                                 *
   !cc   *                                                                  *
   !cc   * Corrects elastic widths for the elastic enhancement by adding    *
   !cc   * appropriate addditional part to scratch matrices.                *
   !cc   *                                                                  *
   !cc   *                                                                  *
   !cc   * input:NNUC - decaying nucleus index                              *
   !cc   *       IEC  - energy index of the decaying state                  *
   !cc   *       JC   - spin index of the decaying state                    *
   !cc   *       IPC  - parity of the decaying state                        *
   !cc   *       NNUR - residual nucleus index                              *
   !cc   *       NEJC - ejectile index                                      *
   !cc   *       nhrtw- 0 MUST NOT be used in this routine                  *
   !cc   *             >0 final HRTW entry (calculate emission widths)      *
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
   INCLUDE 'dimension.h'
   INCLUDE 'global.h'
   !
   ! COMMON variables
   !
   REAL*8, DIMENSION(ndhrtw2,3) :: H_Abs
   REAL*8 :: H_Sumtl, H_Sumtls, H_Sweak, H_Tav, H_Tthr, TFIs
   REAL*8, DIMENSION(ndhrtw1,2) :: H_Tl
   INTEGER, DIMENSION(ndhrtw2,3) :: MEMel
   INTEGER :: NH_lch, NSCh
   REAL*8, DIMENSION(ndlw) :: ELTl


   REAL*8, DIMENSION(ndlw,3) :: ELTlj


   COMMON /ELASTIC/ ELTl,ELTlj


   COMMON /ihrtw / NSCh, MEMel, NH_lch
   COMMON /rhrtw / H_Tl, H_Sumtl, H_Sumtls, H_Sweak, H_Abs, H_Tav, H_Tthr, TFIs
   !
   ! Dummy arguments
   !
   INTEGER :: Iec, Ipc, Jc, Nejc, Nhrtw, Nnuc, Nnur
   !
   ! Local variables
   !
   REAL*8 :: eef, vt1
   REAL*8 :: eout, eoutc, popadd, s, smax, smin, tld, v, xjc
   INTEGER :: i, iel, ipar, kel, l, lmax, lmin
   xjc = float(Jc) + HIS(Nnuc)
   ! 
   ! decay to discrete levels
   ! 
   eoutc = EX(Iec,Nnuc) - Q(Nejc,Nnuc)
   ! only target state considered
   i = LEVtarg
   eout = eoutc - ELV(i,Nnur)
   IF(eout<DE) THEN
      ! level above the bin
      IF(eout<0.0D0)RETURN
   ENDIF
   ! CALL TLLOC(Nnur,Nejc,eout,il,frde)
   smin = abs(XJLv(i,Nnur) - SEJc(Nejc))
   smax = XJLv(i,Nnur) + SEJc(Nejc) + 0.01
   s = smin
   DO ! loop over channel spin ----------------------------------------
      lmin = int(abs(xjc - s) + 1.01)
      lmax = int(xjc + s + 1.01)
      lmax = min0(NLW,lmax)
      DO l = lmin, lmax ! do loop over l ------------------------------------------------
         ipar = 1 + LVP(i,Nnur)*Ipc*( - 1)**(l - 1)
         IF(ipar==0)CYCLE
         tld = ELTl(l)
         IF(tld<=0.D0)CYCLE
         IF(l==int(H_Abs(Nhrtw,2)) .AND. (2.0*s)==H_Abs(Nhrtw,3)) THEN
            ! got a true elastic channel
            IF(tld>H_Tthr .AND. NH_lch<=ndhrtw1) THEN
               DO iel = 1, ndhrtw2
                  IF(MEMel(iel,3)==int(2.0*s + 0.001) .AND. MEMel(iel,2)==l) THEN
                     kel = MEMel(iel,1)
                     v = H_Tl(kel,1)
                  ENDIF
               ENDDO
            ELSE
               v = vt1(tld,H_Tav,H_Sumtl)
            ENDIF
            popadd = v*(eef(tld,H_Tav,H_Sumtl) - 1.D0)
            SCRtl(i,Nejc) = SCRtl(i,Nejc) + popadd
            SCRtem(Nejc) = SCRtem(Nejc) + popadd
         ! write(8,*) '    elastic increased by ',popadd
         ENDIF
      ENDDO ! do loop over l --- done ----------------------------------------
      s = s + 1.
      IF(s<=smax)CYCLE
      RETURN
   ENDDO ! loop over channel spin ------ done ----------------------------
END SUBROUTINE ELCORR
 
SUBROUTINE HRTW_MARENG(Npro,Ntrg,Jcn,Ip,Ich)
   !cc
   !cc   ********************************************************************
   !cc   *                                                         class:ppu*
   !cc   *                H R T W _ M A R E N G                             *
   !cc   *                                                                  *
   !cc   * Calculates initial compound nucleus population after projectile  *
   !cc   * absorption  using transmission coefficients calculated in MARENG *
   !cc   * for elastic channel.                                             *
   !cc   *                                                                  *
   !cc   * input:Npro - projectile index (normally 0)                       *
   !cc   *       Ntrg - target index (normally 0)                           *
   !cc   *       Jcn  - Compound Nucleus spin                               *
   !cc   *       Ip   - Compound Nucleus parity                             *
   !cc   *                                                                  *
   !cc   * output: Ich  - number of physical channels contributing to JCN Ip*
   !cc   *                                                                  *
   !cc   *                                                                  *
   !cc   ********************************************************************
   !cc
   IMPLICIT NONE
   INCLUDE 'dimension.h'
   INCLUDE 'global.h'
   !
   ! COMMON variables
   !
   REAL*8, DIMENSION(ndhrtw2,3) :: H_Abs
   REAL*8 :: H_Sumtl, H_Sumtls, H_Sweak, H_Tav, H_Tthr, TFIs
   REAL*8, DIMENSION(ndhrtw1,2) :: H_Tl
   INTEGER, DIMENSION(ndhrtw2,3) :: MEMel
   INTEGER :: NH_lch, NSCh
   REAL*8, DIMENSION(ndlw) :: ELTl


   REAL*8, DIMENSION(ndlw,3) :: ELTlj


   COMMON /ELASTIC/ ELTl,ELTlj


   COMMON /ihrtw / NSCh, MEMel, NH_lch
   COMMON /rhrtw / H_Tl, H_Sumtl, H_Sumtls, H_Sweak, H_Abs, H_Tav, H_Tthr, TFIs
   !
   ! Dummy arguments
   !
   INTEGER :: Ich, Ip, Jcn, Npro, Ntrg
   !
   ! Local variables
   !
   REAL*8 :: ak2, chsp, coef, ecms, el, s1, smax, smin, vl, xmas_npro, xmas_ntrg
   INTEGER :: i, ichsp, iel, ipa, k, kel, l, lmax, lmin, mul, par
   LOGICAL :: relcal
   REAL*8 :: vt1

   par(i,ipa,l) = (1 - ( - 1)**i*ipa*(-1)**l)/2
 
   xmas_npro = EJMass(Npro)
   xmas_ntrg = AMAss(Ntrg)
 
   el = EINl
   relcal = .FALSE.
   IF(IRElat(Npro,Ntrg)>0 .OR. RELkin)relcal = .TRUE.
   CALL kinema(el,ecms,xmas_npro,xmas_ntrg,ak2,1,relcal)
   coef = 10.D0*PI/ak2/(2*XJLv(LEVtarg,Ntrg) + 1.0)/(2*SEJc(Npro) + 1.0)
   s1 = 0.5
   IF(aint(XJLv(LEVtarg,Ntrg) + SEJc(Npro)) - XJLv(LEVtarg,Ntrg) - SEJc(Npro)==0.0D0)s1 = 1.0
 
   ! channel spin min and max
   smin = abs(SEJc(Npro) - XJLv(LEVtarg,Ntrg))
   smax = SEJc(Npro) + XJLv(LEVtarg,Ntrg)
   mul = smax - smin + 1.0001
 
   Ich = 1
   DO ichsp = 1, mul ! do loop over channel spin
      chsp = smin + float(ichsp - 1)
      lmin = abs(Jcn - chsp - s1) + 0.0001
      lmax = Jcn + chsp - s1 + 0.0001
      lmin = lmin + 1
      lmax = lmax + 1
      lmax = min0(ndlw,lmax)
      DO k = lmin, lmax ! do loop over l
         IF(par(Ip,LVP(LEVtarg,Ntrg),k - 1)/=0) THEN
            IF(Ich>ndhrtw2) THEN
               WRITE(8,*)' '
               WRITE(8,*)'E R R O R !'
               WRITE(8,*)'INSUFFICIENT DIMENSION FOR HRTW CALCULATIONS'
               WRITE(8,*)'INCREASE NDHRTW2 IN THE dimension.h', ' AND RECOMPILE.'
               STOP 'INSUFFICIENT DIMENSION: NDHRTW2'
            ENDIF
            IF(NH_lch>ndhrtw1) THEN
               vl = vt1(ELTl(k),H_Tav,H_Sumtl)
            ELSEIF(ELTl(k)<H_Tthr) THEN
               vl = vt1(ELTl(k),H_Tav,H_Sumtl)
            ELSE
               kel = 0
               DO iel = 1, ndhrtw2
                  IF(MEMel(iel,3)==int(2.0*chsp + 0.001) .AND. MEMel(iel,2)==k)kel = MEMel(iel,1)
               ENDDO
               IF(kel==0) THEN
                  WRITE(8,*)' '
                  WRITE(8,*)' MISMATCH OF ELASTIC CHANNEL IN HRTW'
                  WRITE(8,*)' REPORT THIS ERROR ALONG WITH RELATED'
                  WRITE(8,*)' INPUT FILE TO: mwherman@bnl.gov'
                  STOP ' MISMATCH OF ELASTIC CHANNEL IN HRTW'
               ENDIF
               vl = H_Tl(kel,1)
            ENDIF
            IF(vl/=0.0D0) THEN
               H_Abs(Ich,1) = vl*coef*(float(2*Jcn + 1) - 2*s1)*FUSred*REDmsc(Jcn,Ip)
               ! & *FUSred*REDmsc(Jcn,Ip)*DRTl(k)
               H_Abs(Ich,2) = k
               H_Abs(Ich,3) = 2*chsp
               Ich = Ich + 1
            ENDIF
         ENDIF
      ENDDO  ! do loop over l
   ENDDO  ! do loop over channel spin
   Ich = Ich - 1
   RETURN
END SUBROUTINE HRTW_MARENG