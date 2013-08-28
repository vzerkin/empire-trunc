Ccc   * $Rev: 3479 $
Ccc   * $Author: rcapote $
Ccc   * $Date: 2013-08-25 23:11:40 +0200 (So, 25 Aug 2013) $
C
C
      SUBROUTINE HRTW
Ccc
Ccc   ********************************************************************
Ccc   *                                                         class:ppu*
Ccc   *                         H R T W                                  *
Ccc   *                                                                  *
Ccc   *  Calculate decay of the Compound Nucleus capture states in       *
Ccc   *  terms of the HRTW theory (width fluctuation correction).        *
Ccc   *  Uses modified routines of standard Hauser-Feshbach (DECAY,      *
Ccc   *  DECAYG, FISSION) and HRTW_MARENG to decompose capture           *
Ccc   *  cross sections into partial wave components.                    *
Ccc   *                                                                  *
Ccc   *                                                                  *
Ccc   ********************************************************************
Ccc
      INCLUDE 'dimension.h'
      INCLUDE 'global.h'
C
C
C COMMON variables
C
      DOUBLE PRECISION H_Abs(NDHRTW2,3), H_Sumtl, H_Sumtls, H_Sweak,
     &                 H_Tav,H_Tl(NDHRTW1,2), H_Tthr, TFIs, sumGg
      INTEGER MEMel(NDHRTW2,3), NH_lch, NSCh
      COMMON /IHRTW / NSCh, MEMel, NH_lch
      COMMON /RHRTW / H_Tl, H_Sumtl, H_Sumtls, H_Sweak, H_Abs, H_Tav,
     &                H_Tthr, TFIs
C
C Local variables
C
      DOUBLE PRECISION cnspin, sgamc, tgexper,
     &              sum, sumfis, sumfism(NFMOD), sumg, tlump, xnor, 
     &              fisxse, sumtg, divf
C     DOUBLE PRECISION VT1
      INTEGER i, ich, ip, ipar, jcn, ke, m, nejc, nhrtw, nnuc, nnur
      INTEGER INT
      CHARACTER*1 cpar(2)
      DATA cpar/'+','-'/ 
C
C
C
C-----threshold for considering channel as a 'strong' one
      H_Tthr = 0.0001
C-----set CN nucleus
      nnuc = 1
C-----reset variables
      sgamc = 0.d0
      CSFis = 0.d0
      sumfis = 0.d0
C
      ke = NEX(nnuc)
C-----
C-----start CN nucleus decay
C-----
C-----do loop over decaying nucleus parity
      d0c   = 0.d0
      sumGg = 0.d0
      sumtg = 0.d0
      tgexper=0.d0
      IF(FIRst_ein .or. BENchm) THEN          
        IF(EINl.LE.0.005d0) THEN
          WRITE(8,
     &    '(1x,''Renormalization of gamma-ray strength function'')')
      ELSE
          WRITE(8,'(1x,
     &  '' WARNING: First incident energy Einc must be < 5keV for Do an
     &d Gg calculations'')')
          WRITE(8,'(1x,
     &  '' WARNING: for the renormalization of gamma-ray strength funct
     &ion'')')
      ENDIF
        WRITE(8,'(1x,
     &    ''------------------------------------------------------------
     &-'')')
      ENDIF

      DO ipar = 1, 2
         ip = INT(( - 1.0)**(ipar + 1))
C--------do loop over decaying nucleus spin
         DO jcn = 1, NLW
C           WRITE(8,*)'  '
C           WRITE(8,*)'DECAY STATE J=',jcn,' PI=',ipar
C           WRITE(8,*)'  '
            nhrtw = 0
C-----------initialize variables
            DENhf = 0.d0
            NSCh = 0
            NH_lch = 0
            H_Sumtls = 0.d0
            H_Sumtl = 0.d0
            H_Tav = 0.d0
            H_Sweak = 0.d0
            DO i = 1, NDHRTW1
               H_Tl(i,1) = 0.d0
               H_Tl(i,2) = 0.d0
            ENDDO
C-----------prepare gamma-strength (GMR) parameters (if spin
C-----------dependent GDR selected)
            IF (GDRdyn.EQ.1.0D0) CALL ULMDYN(nnuc,jcn,EX(ke,nnuc))
C-----------
C-----------start the first HRTW run
C-----------
C-----------do loop over ejectiles
            DO nejc = 1, NEJcm
C              emitted nuclei must be heavier than alpha
               if(NREs(nejc).lt.0) cycle
               nnur = NREs(nejc)
               sum = 0.d0
C              WRITE(8,*)'emitting ejectile=', nejc
               CALL HRTW_DECAY(nnuc,ke,jcn,ip,nnur,nejc,sum,nhrtw)
C              WRITE(8,*)'sum for ejectile=' , nejc, sum
               H_Sumtl = H_Sumtl + sum
            ENDDO
C-----------do loop over ejectiles       ***done***

C-----------gamma emission is always a weak channel (one iteration)
            sumg = 0.d0
            CALL HRTW_DECAYG(nnuc,ke,jcn,ip,sumg,nhrtw)
            H_Sumtl = H_Sumtl + sumg
            H_Sweak = H_Sweak + sumg
C
C-----------fission is always a weak channel (one iteration)
            sumfis = 0.d0
            IF (FISsil(nnuc)) THEN
              IF (NINT(FISshi(nnuc)).EQ.1) THEN
                CALL FISSION(nnuc,ke,jcn,sumfis)
              ELSE
                CALL FISCROSS(nnuc,ke,ip,jcn,sumfis,sumfism)
              ENDIF
              H_Sumtl = H_Sumtl + sumfis
              H_Sweak = H_Sweak + sumfis
            ENDIF

            IF (H_Sumtl.GT.0.0D0 .AND. H_Sumtl.GT.H_Sweak) THEN
               tlump = (H_Sumtl - H_Sweak)
     &                 /(10.d0*(1.d0 + (H_Sweak)/(H_Sumtl-H_Sweak)))
C              !define a good Tlump
            ELSE
               tlump = H_Sweak
            ENDIF
C
            IF (H_Sumtl.GT.0.0D0) THEN
C--------------check whether tfis is not too big compared to a good Tlump
               divf = sumfis/tlump + 1.0
               TFIs = sumfis/divf
               H_Sumtls = H_Sumtls + divf*TFIs**2
               H_Tav = H_Sumtls/H_Sumtl
               IF (H_Tav.LT.TFIs) THEN
                  H_Sumtls = H_Sumtls - divf*TFIs**2
                  divf = divf*(TFIs/H_Tav + 1)
                  TFIs = sumfis/divf
                  H_Sumtls = H_Sumtls + divf*TFIs**2
                  H_Tav = H_Sumtls/H_Sumtl
               ENDIF
C--------------redefine fission transmission coef. using single iteration
C              RCN & MS 03-2010
C              redefinition avoided to keep the Cross section difference equal zero.
C                (small difference observed if fission channel is open)
C              TFIs = VT1(TFIs,H_Tav,H_Sumtl)
               sumfis = divf*TFIs
            ELSE
               H_Tav = 0.0
            ENDIF
C           WRITE(8,*)'sum gamma ' , sumg
C           WRITE(8,*)'sum fission redefined ' , sumfis
C           WRITE(8,*)'total sum for this state ' , H_Sumtl
C           WRITE(8,*)'sum Tl**2 for this state ' , H_Sumtls
C           WRITE(8,*)'sum of weak for this state ' , H_Sweak
C           WRITE(8,*)'number of strong Tls for this state ' , NH_lch
C           WRITE(8,*)'average Tl for this state ' , H_Tav
C           WRITE(8,*)'strong Tls from this state ' , H_Tl
C           WRITE(8,*)'first entry DENhf=',denhf
C-----------
C-----------the first HRTW run completed
C-----------
C-----------calculate V's for the strong channels (iteration)
C           WRITE(8,*)'  '
            IF (NH_lch.LE.NDHRTW1)
     &          CALL AUSTER(H_Tl,H_Tav,H_Sumtl,H_Sweak,NH_lch,NDHRTW1)
C           WRITE(8,*)'strong Vs from this state ' , H_Tl
C           WRITE(8,*)'  '
C           WRITE(8,*)'  '
C           WRITE(8,*)'  '
C           WRITE(8,*)'  '
C-----------
C-----------start the second HRTW run
C-----------
C-----------calculate reaction cross section and its spin distribution
C-----------split into contributions from individual partial waves
            CALL HRTW_MARENG(0,0,jcn,ipar,ich)
            DO i = 1, ich
C              WRITE(8,*)' '
C              WRITE(8,*)'HRTW entry=',i
C              WRITE(8,*)' '
               NSCh = 0
C--------------do loop over ejectiles (fission is not repeated)
               nhrtw = i
               DENhf = 0.d0
               DO nejc = 1, NEJcm
C                 emitted nuclei must be heavier than alpha
                  if(NREs(nejc).lt.0) cycle
                  nnur = NREs(nejc)
C                 WRITE(8,*)'  '
C                 WRITE(8,*)'second entry with ejec ' , nejc
                  CALL HRTW_DECAY(nnuc,ke,jcn,ip,nnur,nejc,sum,nhrtw)
C                 WRITE(8,*)'sum for ejec=' ,nejc, sum
               ENDDO
C--------------do loop over ejectiles       ***done***
C              sumg = 0.d0
C              CALL HRTW_DECAYG(nnuc,ke,jcn,ip,sumg,nhrtw)
               DENhf = DENhf + sumg + sumfis
C              WRITE(8,*)'second entry DENhf=',denhf
C              WRITE(8,*)'second entry sumg=',sumg
C--------------
C--------------correct scratch matrix for enhancement of the elastic channels
C--------------
               DO nejc = 1, NEJcm
C                 emitted nuclei must be heavier than alpha
                  if(NREs(nejc).lt.0) cycle
                  nnur = NREs(nejc)
                  IF (IZA(nnur).EQ.IZA(0))
     &              CALL ELCORR(nnuc,ke,jcn,ip,nnur,nejc,nhrtw)
               ENDDO
C--------------
C--------------normalization and accumulation
C--------------
               xnor = H_Abs(i,1)/DENhf
               IF (RO(ke,jcn,ipar,nnuc).NE.0.0D0) sgamc = sgamc +
     &             DENhf*H_Abs(i,1)/RO(ke,jcn,ipar,nnuc)
               CALL XSECT(nnuc,m,xnor,sumfis,sumfism,ke,ipar,jcn,fisxse)
            ENDDO    !loop over partial wave contributions to CN state
C
C           Gamma width calculation
C
            IF((FIRst_ein  .or. BENchm) .AND. EINl.LE.0.002d0) THEN
              cnspin = jcn - 0.5
              if(mod(XJLv(LEVtarg,0)*2.,2.D+0).eq.1) cnspin = jcn-1
              if( ip.eq.LVP(LEVtarg,0) .AND.
     &            ( (cnspin.eq.XJLv(LEVtarg,0)+0.5) .OR.
     &              (cnspin.eq.XJLv(LEVtarg,0)-0.5) ) ) THEN
                d0c = d0c + RO(ke,jcn,ipar,nnuc)
C        write(8,*)'ke,jcn,ipar,ro',ke,jcn,ipar,RO(ke,jcn,ipar,nnuc)
                WRITE(8,'(A12,f4.1,A6,A1,A6,F7.3,A4,/,A37,d12.6)')
     &           'CN state J=',cnspin,', Par:',cpar(ipar),
     &           ' at U=',EX(ke,nnuc),' MeV',
     &           'Int[Rho(U)*Tlg(U)] + Sum[Tlg(Ui)] = ',sumg
                sumtg = sumtg + sumg
              ENDIF
            ENDIF
         ENDDO       !loop over decaying nucleus spin
      ENDDO          !loop over decaying nucleus parity
      IF(d0c.gt.0.d0) d0c = 1000.0/d0c
      IF(D0_obs.EQ.0.0D0) D0_obs = d0c !use calculated D0 (in keV) if not measured
      IF(EINl.LE.0.002d0 .AND. (FIRst_ein .or. BENchm)) THEN
         IF(D0_obs.GT.0.d0) THEN
            tgexper = 2*pi*Gg_obs/D0_obs/1.E6
            WRITE(8,'(1x,
     &      ''Experimental information from capture channel'')')
            WRITE(8,'(1x,A13,D12.6)') '2*pi*Gg/D0 = ',tgexper
         ENDIF
         IF(GG_unc.GT.0.0D0) THEN
            WRITE(8,'(1x,A5,F9.3,A5,F8.3,A4)')
     &        'Gg = ', GG_obs,' +/- ',GG_unc,' meV'
         ELSE
            WRITE(8,'(1x,A5,F9.3,A18)')
     &        'Gg = ', GG_obs,' meV (systematics)'
         ENDIF
         WRITE(12,'(1x,A5,F9.3,A5,F8.3,A4)')
     &          'Gg = ', GG_obs,' +/- ',GG_unc,' meV'

         IF(D0_obs.GT.0.0D0) THEN
            WRITE(8,'(1x,A5,F11.6,A5,F11.6,A4)')
     &          'D0 = ', D0_obs,' +/- ',D0_unc,' keV'
            WRITE(8,'(1x,A5,F11.6,A17)')
     &          'D0 = ', d0c,' keV (calculated)'
            WRITE(12,'(1x,''D0 = '',F8.3,'' keV'')') D0_obs
         ELSE
            WRITE(8,'(1x,A5,F11.6,A17)')
     &          'D0 = ', d0c,' keV (calculated)'
            WRITE(12,'(1x,''D0 = '',F8.3,'' keV, CALC'')') d0c
         ENDIF
         WRITE(8 ,*)
         WRITE(12,*)
         if(sumtg.gt.0.d0 .and. tgexper.gt.0.d0) then
            WRITE(8,'(1x,''Normalization factor = '',F7.3)')
     &          tgexper/sumtg
         else
            WRITE(8,'(1x,
     &        ''Calculated Tgamma = 0'',
     &        '' or D_obs not available, no Normalization'')')
         endif
         WRITE(8 ,*)
C
C        renormalization of the gamma-ray strength function only undertaken for the fir
C
         IF(ABS(TUNe(0, Nnuc)-0.999D+0).LT.0.0001D+0) THEN
            IF(FIRst_ein .or. BENchm) then
              if(sumtg.gt.0.d0 .and. tgexper.gt.0.d0) then
                TUNe(0, Nnuc) = tgexper/sumtg
                WRITE(8 ,
     &         '(1x,''Gamma emission width normalized ''/
     &           1x,''internally by a factor '',F7.3)')
     &         TUNe(0, Nnuc)
                WRITE(8,*)
              ELSE
                WRITE(8 ,
     &       '(1x,''Gamma emission width is not normalized to Do'')')
              ENDIF
            ELSE
                WRITE(8 ,
     &         '(1x,''Gamma emission width normalized ''/
     &           1x,''internally by a factor '',F7.3)')
     &         TUNe(0, Nnuc)
            ENDIF
         ELSE
            IF(ABS(TUNe(0, Nnuc)-1.d0).LT.0.00001D+0) THEN
              WRITE(8 ,
     &       '(1x,''Gamma emission width is not normalized to Do'')')
            ELSE
              WRITE(8,
     &         '(1x,''Gamma emission width normalized ''/
     &           1x,''by setting TUNE in input to '',F7.3)')
     &         TUNe(0, Nnuc)
            ENDIF
         ENDIF
         WRITE(8,*)
      ENDIF
      RETURN
      END
      SUBROUTINE HRTW_DECAY(Nnuc,Iec,Jc,Ipc,Nnur,Nejc,Sum,Nhrtw)
Ccc
Ccc   ********************************************************************
Ccc   *                                                         class:PPu*
Ccc   *                      H R T W  _  D E C A Y                       *
Ccc   *                (function to function version)                    *
Ccc   *                                                                  *
Ccc   * Calculates decay of a continuum state in nucleus NNUC into       *
Ccc   * continuum and discrete states of the residual nucleus NNUR       *
Ccc   * through the emission of the ejectile NEJC including width        *
Ccc   * fluctuation correction according to HRTW theory.                 *
Ccc   *                                                                  *
Ccc   *                                                                  *
Ccc   * input:NNUC - decaying nucleus index                              *
Ccc   *       IEC  - energy index of the decaying state                  *
Ccc   *       JC   - spin index of the decaying state                    *
Ccc   *       IPC  - parity of the decaying state                        *
Ccc   *       NNUR - residual nucleus index                              *
Ccc   *       NEJC - ejectile index                                      *
Ccc   *       Nhrtw- 0 first HRTW entry (calculate Tl's summs)           *
Ccc   *             >0 final HRTW entry (calculate emission widths)      *
Ccc   *                                                                  *
Ccc   * output:SUM - SUM of transmission coefficients over all outgoing  *
Ccc   *              channels for the requested decay                    *
Ccc   *              Apart of this standard feature it comunicates       *
Ccc   *              quantities needed for the HRTW theory through       *
Ccc   *              the HRTW common:                                    *
Ccc   *                                                                  *
Ccc   *             H_Tl(.,.) - list of strong Tl's and their number(rho)*
Ccc   *             H_Tloc(.,.) - l and ejectile for each strong Tl      *
Ccc   *             H_sumtls - sum of squared Tl's (all)                 *
Ccc   *             H_sweak - sum of weak Tl's (not squared)             *
Ccc   *             H_lch - number of strong Tl's                        *
Ccc   *             H_abs(.,.) - absorption x-sec decomposed in l's      *
Ccc   *             H_Tav - average transmission coefficient             *
Ccc   *             H_Tthr - threshold to consider Tl as strong          *
Ccc   *             MEMel  - records l and position of elastic channel   *
Ccc   *                                                                  *
Ccc   *                                                                  *
Ccc   *                                                                  *
Ccc   * calls:TLLOC                                                      *
Ccc   *                                                                  *
Ccc   *                                                                  *
Ccc   *                                                                  *
Ccc   ********************************************************************
Ccc
      INCLUDE 'dimension.h'
      INCLUDE 'global.h'
C
C
C COMMON variables
C
      DOUBLE PRECISION ELTl(NDLW), H_Abs(NDHRTW2,3), H_Sumtl, H_Sumtls,
     &                 H_Sweak, H_Tav, H_Tl(NDHRTW1,2), H_Tthr, TFIs
      INTEGER MEMel(NDHRTW2,3), NH_lch, NSCh
      COMMON /ELASTIC/ ELTl
      COMMON /IHRTW / NSCh, MEMel, NH_lch
      COMMON /RHRTW / H_Tl, H_Sumtl, H_Sumtls, H_Sweak, H_Abs, H_Tav,
     &                H_Tthr, TFIs
C
C Dummy arguments
C
      INTEGER Iec, Ipc, Jc, Nejc, Nhrtw, Nnuc, Nnur
      DOUBLE PRECISION Sum
C
C Local variables
C
      DOUBLE PRECISION cor, corr, eout, eoutc, frde, hisr, rho, s, smax,
     &   smin, sumdl, sumtl1, sumtl2, tld, xjc, xjr
      INTEGER i, ichsp, iel, ier, iermax, ietl, iexc, il, ip1, ip2,
     &        ipar, itlc, j, jr, l, lmax, lmaxf, lmin, mul
      DOUBLE PRECISION VT
C
C
C     WRITE(8,*)' '
C     WRITE(8,*)'ejectile ,nhrtw ',nejc,nhrtw
C     WRITE(8,*)'CN bin, spin, parity',Iec,Jc,Ipc
C     WRITE(8,*)' '
      hisr = HIS(Nnur)
      xjc = FLOAT(Jc) + HIS(Nnuc)
C-----clear scratch matrices
C     NSCh = 0
      Sum = 0.D0
      SCRtem(Nejc) = 0.D0
      DO j = 1, NLW
         DO i = 1, NEX(Nnur) + 1
            SCRt(i,j,1,Nejc) = 0.D0
            SCRt(i,j,2,Nejc) = 0.D0
         ENDDO
      ENDDO
      iexc = NEX(Nnuc) - NEXr(Nejc,Nnuc)
      itlc = iexc - 5
      iermax = Iec - iexc
      IF (iermax.GE.1) THEN
C--------
C--------decay to the continuum
C--------
         DO jr = 1, NLW            ! do loop over r.n. spins
            xjr = FLOAT(jr) + hisr
            smin = ABS(xjr - SEJc(Nejc))
            smax = xjr + SEJc(Nejc)
            mul = INT(smax - smin + 1.0001)
            DO ichsp = 1, mul              ! do loop over channel spin
               s = smin + FLOAT(ichsp - 1)
               lmin = INT(ABS(xjc - s) + 1.01)
               lmaxf = INT(xjc + s + 1.01)
               lmaxf = MIN0(NLW,lmaxf)
               ipar = (1 + Ipc*( - 1)**(lmin - 1))/2
C--------------parity index of r.n. state populated by emission with LMIN
               ip1 = 2 - ipar
C--------------parity index of r.n. state populated by emission with LMIN+1
               ip2 = 1
               IF (ip1.EQ.1) ip2 = 2
C--------------decay to the highest possible bin (non neutron only)
               IF (ZEJc(Nejc).NE.0.0D0) THEN
                  lmax = lmaxf
                  lmax = MIN0(LMAxtl(6,Nejc,Nnur),lmax)
C-----------------odd and even L-values treated separately
C-----------------ip1 and ip2 decide to which parity each SUMTL  goes
                  rho = RO(iermax,jr,ip1,Nnur)*DE*TUNe(Nejc,Nnuc)
                  sumtl1 = 0.d0
                  DO L = lmin, lmax, 2      ! do loop over L
                     IF (Nhrtw.GT.0) THEN
C-----------------------replace Tl with V in the second HRTW entry
                        sumtl1 = sumtl1 + VT(TL(5,L,Nejc,Nnur))
                     ELSE
C-----------------------first entry with HRTW
C                       WRITE(8,*)'A Tl= ' , TL(5,L,Nejc,Nnur)
                        CALL TL2VL(TL(5,L,Nejc,Nnur),rho)
                        sumtl1 = sumtl1 + TL(5,L,Nejc,Nnur)
                     ENDIF
                  ENDDO
                  rho = RO(iermax,jr,ip2,Nnur)*DE*TUNe(Nejc,Nnuc)
                  sumtl2 = 0.d0
                  DO L = lmin + 1, lmax, 2
                     IF (Nhrtw.GT.0) THEN
C-----------------------replace Tl with V in the second HRTW entry
                        sumtl2 = sumtl2 + VT(TL(5,L,Nejc,Nnur))
                     ELSE
C-----------------------first entry with HRTW
C                       WRITE(8,*)'B Tl= ' , TL(5,L,Nejc,Nnur)
                        CALL TL2VL(TL(5,L,Nejc,Nnur),rho)
                        sumtl2 = sumtl2 + TL(5,L,Nejc,Nnur)
                     ENDIF
                  ENDDO                     ! over L
                  SCRt(iermax,jr,ip1,Nejc) = SCRt(iermax,jr,ip1,Nejc)
     &               + sumtl1*RO(iermax,jr,ip1,Nnur)
                  SCRt(iermax,jr,ip2,Nejc) = SCRt(iermax,jr,ip2,Nejc)
     &               + sumtl2*RO(iermax,jr,ip2,Nnur)
               ENDIF
C--------------decay to the highest but one bin (conditional see the next IF)
               IF (ZEJc(Nejc).EQ.0.0D0 .AND. Iec.EQ.NEX(Nnuc) - 1) THEN
                  lmax = lmaxf
                  lmax = MIN0(LMAxtl(6,Nejc,Nnur),lmax)
C-----------------CORR in the next lines accounts for the Tl interpolation
C-----------------and integration over overlaping bins (2/3), it turned out it must
C-----------------be energy step and also emission step dependent
                  corr = 0.4444/(DE - XN(Nnur) + XN(1))
C-----------------do loop over L (odd and even l-values treated separately)
C-----------------IP1 and IP2 decide which parity each SUMTL  goes to
                  rho = RO(iermax,jr,ip1,Nnur)*DE*corr*TUNe(Nejc,Nnuc)
                  sumtl1 = 0.d0
                  DO L = lmin, lmax, 2       ! do loop over L
                     IF (Nhrtw.GT.0) THEN
C-----------------------replace Tl with V in the second HRTW entry
                        sumtl1 = sumtl1 + VT(TL(6,L,Nejc,Nnur))
                     ELSE
C-----------------------first entry with HRTW
C                       WRITE(8,*)'C Tl= ' , TL(6,L,Nejc,Nnur)
                        CALL TL2VL(TL(6,L,Nejc,Nnur),rho)
                        sumtl1 = sumtl1 + TL(6,L,Nejc,Nnur)
                     ENDIF
                  ENDDO
                  rho = RO(iermax,jr,ip2,Nnur)*DE*corr*TUNe(Nejc,Nnuc)
                  sumtl2 = 0.d0
                  DO L = lmin + 1, lmax, 2
                     IF (Nhrtw.GT.0) THEN
C-----------------------replace Tl with V in the second HRTW entry
                        sumtl2 = sumtl2 + VT(TL(6,L,Nejc,Nnur))
                     ELSE
C-----------------------first entry with HRTW
C                       WRITE(8,*)'D Tl= ' , TL(6,L,Nejc,Nnur)
                        CALL TL2VL(TL(6,L,Nejc,Nnur),rho)
                        sumtl2 = sumtl2 + TL(6,L,Nejc,Nnur)
                     ENDIF
                  ENDDO                      ! over L
                  SCRt(iermax,jr,ip1,Nejc) = SCRt(iermax,jr,ip1,Nejc)
     &               + sumtl1*RO(iermax,jr,ip1,Nnur)*corr
                  SCRt(iermax,jr,ip2,Nejc) = SCRt(iermax,jr,ip2,Nejc)
     &               + sumtl2*RO(iermax,jr,ip2,Nnur)*corr
               ENDIF
C--------------do loop over r.n. energies (highest bin and eventually the second
C--------------bin from the top excluded as already done)
               DO ier = iermax - 1, 1, -1
                  ietl = Iec - ier - itlc
                  lmax = lmaxf
                  lmax = MIN0(LMAxtl(ietl,Nejc,Nnur),lmax)
                  IF (ier.EQ.1) THEN
                     corr = 0.5d0
                  ELSE
                     corr = 1.d0
                  ENDIF
C-----------------do loop over L (odd and even L-values treated separately)
C-----------------IP1 and IP2 decide which parity each SUMTL  goes to
                  rho = RO(ier,jr,ip1,Nnur)*DE*corr*TUNe(Nejc,Nnuc)
                  sumtl1 = 0.d0
                  DO L = lmin, lmax, 2
                     IF (Nhrtw.GT.0) THEN
C-----------------------replace Tl with V in the second HRTW entry
                        sumtl1 = sumtl1 + VT(TL(ietl,L,Nejc,Nnur))
                     ELSE
C-----------------------first entry with HRTW
C                       WRITE(8,*)'E Tl= ' , TL(ietl,L,Nejc,Nnur)
                        CALL TL2VL(TL(ietl,L,Nejc,Nnur),rho)
                        sumtl1 = sumtl1 + TL(ietl,L,Nejc,Nnur)
                     ENDIF
                  ENDDO
                  rho = RO(ier,jr,ip2,Nnur)*DE*corr*TUNe(Nejc,Nnuc)
                  sumtl2 = 0.d0
                  DO L = lmin + 1, lmax, 2
                     IF (Nhrtw.GT.0) THEN
C-----------------------replace Tl with V in the second HRTW entry
                        sumtl2 = sumtl2 + VT(TL(ietl,L,Nejc,Nnur))
                     ELSE
C-----------------------first entry with HRTW
C                       WRITE(8,*)'F Tl= ' , TL(ietl,L,Nejc,Nnur)
                        CALL TL2VL(TL(ietl,L,Nejc,Nnur),rho)
                        sumtl2 = sumtl2 + TL(ietl,L,Nejc,Nnur)
                     ENDIF
                  ENDDO
C-----------------do loop over L   ***done***
C
                  SCRt(ier,jr,ip1,Nejc) = SCRt(ier,jr,ip1,Nejc)
     &               + sumtl1*RO(ier,jr,ip1,Nnur)
                  SCRt(ier,jr,ip2,Nejc) = SCRt(ier,jr,ip2,Nejc)
     &               + sumtl2*RO(ier,jr,ip2,Nnur)
               ENDDO               ! over r.n. energies
            ENDDO           ! over channel spins
         ENDDO        ! over and r.n. spins
C--------trapezoidal integration of ro*tl in continuum for ejectile nejc
         DO j = 1, NLW
            DO i = 1, iermax
               Sum = Sum + SCRt(i,j,1,Nejc) + SCRt(i,j,2,Nejc)
            ENDDO
            Sum = Sum - 0.5*(SCRt(1,j,1,Nejc) + SCRt(1,j,2,Nejc))
         ENDDO
         Sum = Sum*DE
C--------integration of ro*tl in continuum for ejectile nejc -- done ----
C--------
C--------decay to discrete levels
C--------
      ENDIF

      DO i = 1, NLV(Nejc)
         SCRtl(i,Nejc) = 0.D0
      ENDDO
      IF (Nhrtw.EQ.0 .AND. IZA(Nnur).EQ.IZA(0)) THEN
C-----clear memorized elastic channels when entering new J-pi CN state
         iel = 1
         DO i = 1, NDHRTW2
            MEMel(i,1) = 0
            MEMel(i,2) = 0
            MEMel(i,3) = 0
         ENDDO
      ENDIF

      eoutc = EX(Iec,Nnuc) - Q(Nejc,Nnuc)
C-----
C-----do loop over discrete levels -----------------------------------
C-----
      DO i = 1, NLV(Nnur) 
C        Elastic channels excluded, done after the loop
         if(IZA(Nnur).EQ.IZA(0) .and. i.eq.LEVtarg) cycle  

         eout = eoutc - ELV(i,Nnur)
         cor  = CINRED(i)
       
         IF (eout.LT.0.0D0) EXIT

         sumdl = 0.d0
         CALL TLLOC(Nnur,Nejc,eout,il,frde)
         smin = ABS(XJLv(i,Nnur) - SEJc(Nejc))
         smax = XJLv(i,Nnur) + SEJc(Nejc) + 0.01
         s = smin
C--------loop over channel spin ----------------------------------------
   20    lmin = INT(ABS(xjc - s) + 1.01)
         lmax = INT(xjc + s + 1.01)
         lmax = MIN0(NLW,lmax)
C--------do loop over L ------------------------------------------------
         DO L = lmin, lmax
            ipar = 1 + LVP(i,Nnur)*Ipc*( - 1)**(L - 1)
            tld = TL(il,L,Nejc,Nnur)
     &            + frde*(TL(il + 1,L,Nejc,Nnur)
     &            - TL(il,L,Nejc,Nnur))
            IF (ipar.NE.0 .AND. tld.GT.0.0D0) THEN
              IF (Nhrtw.GT.0) THEN
C----------------entry with nhrtw>0
                 sumdl = sumdl + VT(tld)*cor
              ELSE
C----------------entry with nhrtw=0
                 CALL TL2VL(tld,cor)
                 sumdl = sumdl + tld*cor
C                WRITE(8,*)'sumdl,tld,cor ',sumdl,tld,cor
               ENDIF
            ENDIF
         ENDDO
C--------do loop over L --- done ----------------------------------------
         s = s + 1.
         IF (s.LE.smax) GOTO 20
C--------loop over channel spin ------ done ----------------------------
         SCRtl(i,Nejc) = sumdl
         Sum = Sum + sumdl
C        WRITE(8,*)'i,sumdl,nejc,nhrtw ', i,sumdl,nejc,nhrtw
      ENDDO

      if(IZA(Nnur).EQ.IZA(0)) THEN
	
         i=LEVtarg  

         eout = eoutc - ELV(i,Nnur)
         cor = CELred
         IF (eout.LT.0.0D0) GOTO 70

         sumdl = 0.d0
         CALL TLLOC(Nnur,Nejc,eout,il,frde)
         smin = ABS(XJLv(i,Nnur) - SEJc(Nejc))
         smax = XJLv(i,Nnur) + SEJc(Nejc) + 0.01
         s = smin
C--------loop over channel spin ----------------------------------------
   30    lmin = INT(ABS(xjc - s) + 1.01)
         lmax = INT(xjc + s + 1.01)
         lmax = MIN0(NLW,lmax)
C--------do loop over L ------------------------------------------------
         DO L = lmin, lmax
            ipar = 1 + LVP(i,Nnur)*Ipc*( - 1)**(L - 1)
            tld = ELTl(L)
            IF (ipar.NE.0 .AND. tld.GT.0.0D0) THEN
              IF (Nhrtw.GT.0) THEN
C----------------entry with nhrtw>0
                 sumdl = sumdl + VT(tld)*cor
              ELSE
C----------------entry with nhrtw=0
                 CALL TL2VL(tld,cor)
                 sumdl = sumdl + tld*cor
C                WRITE(8,*)'sumdl,tld,cor ',sumdl,tld,cor
                 IF (tld.GT.H_Tthr) THEN
C-------------------case of a strong elastic channel
C-------------------record position of Tl, l and channel spin
                    MEMel(iel,1) = NH_lch
                    MEMel(iel,2) = l
                    MEMel(iel,3) = INT(2.0*s)
C                   WRITE(8,*)'got elastic iel ', iel,
C    &              '  MEM# ',MEMel(iel,1),
C    &              '  MEMk ',MEMel(iel,2),
C    &              '  MEM2s ',MEMel(iel,3)
                    iel = iel + 1
                 ENDIF
               ENDIF
            ENDIF
         ENDDO
C--------do loop over L --- done ----------------------------------------
         s = s + 1.
         IF (s.LE.smax) GOTO 30
C--------loop over channel spin ------ done ----------------------------
         SCRtl(i,Nejc) = sumdl
         Sum = Sum + sumdl
C        WRITE(8,*)'i,sumdl,nejc,nhrtw ', i,sumdl,nejc,nhrtw
      ENDIF

C-----do loop over discrete levels --------- done --------------------
   70 DENhf = DENhf + Sum
      SCRtem(Nejc) = Sum
Cpr   WRITE(8,*) 'TOTAL SUM=',SUM
C-----decay to the continuum and discrete levels ------ done -----------------------------
	RETURN
      END
C
C
      SUBROUTINE HRTW_DECAYG(Nnuc,Iec,Jc,Ipc,Sum,Nhrtw)
Ccc
Ccc   ********************************************************************
Ccc   *                                                         class:PPu*
Ccc   *                         D E C A Y G                              *
Ccc   *                (function to function version)                    *
Ccc   *                                                                  *
Ccc   * Calculates gamma decay of a continuum state in nucleus NNUC into *
Ccc   * continuum and discrete states in the same nucleus NNUC           *
Ccc   *                                                                  *
Ccc   *                                                                  *
Ccc   * input:NNUC - decaying nucleus index                              *
Ccc   *       IEC  - energy index of the decaying state                  *
Ccc   *       JC   - spin index of the decaying state                    *
Ccc   *       IPC  - parity of the decaying state                        *
Ccc   *       nhrtw- 0 first HRTW entry (calculate Tl's summs)           *
Ccc   *             >0 final HRTW entries (actually not used)            *
Ccc   *                                                                  *
Ccc   *                                                                  *
Ccc   * output:SUM - SUM of transmission coefficients over all gamma     *
Ccc   *              channels.                                           *
Ccc   *              Apart of this standard feature it comunicates       *
Ccc   *              quantities needed for the HRTW theory through       *
Ccc   *              the HRTW common.  Note that gamma channels          *
Ccc   *              contribute to the sum of weak channels but their    *
Ccc   *              transmission coefficients are assumed to remain     *
Ccc   *              constant when HRTW is applied.                      *
Ccc   *                                                                  *
Ccc   * calls:none                                                       *
Ccc   *                                                                  *
Ccc   *                                                                  *
Ccc   *                                                                  *
Ccc   ********************************************************************
      INCLUDE 'dimension.h'
      INCLUDE 'global.h'
C
C
C COMMON variables
C
      DOUBLE PRECISION H_Abs(NDHRTW2,3), H_Sumtl, H_Sumtls, H_Sweak,
     &                 H_Tav, H_Tl(NDHRTW1,2), H_Tthr, TFIs
      COMMON /RHRTW / H_Tl, H_Sumtl, H_Sumtls, H_Sweak, H_Abs, H_Tav,
     &                H_Tthr, TFIs
C
C Dummy arguments
C
      INTEGER Iec, Ipc, Jc, Nhrtw, Nnuc
      DOUBLE PRECISION Sum
C
C Local variables
C
      DOUBLE PRECISION corr, eg, xjc
      DOUBLE PRECISION E1, E2, XM1
C     DOUBLE PRECISION VT1
      INTEGER i, ier, ineg, iodd, ipar, ipos, j, jmax, jmin, lmax, lmin
      INTEGER Jr, lamb, lambmin, lambmax
      DOUBLE PRECISION ha, cee, cme, xle, xlm, xjr,
     &                 scrtpos, scrtneg, hsumtls,  hscrtl
      DIMENSION xle(10),xlm(10)

C----MAXmult - maximal gamma-ray multipolarity
C    maximal value (.LT.10) of gamma-ray multipolarity (L) in
C    calculations of gamma-transitions both between states in
C    continuum and from continuum states to discrete levels.
C    A default value of 'MAXmult' is set to 2 in 'input.f'
C    but can be adjusted in the input.
C
C   The radiative strength functions of higher multipole orders
C   (f_EL, f_ML) are calculated using the relationships between
C   single-particle radiative strength functions in the Weisskopf form.
C
C     Electric transitions:
C     f_E(L+1)/f_EL = eg^2*cee*[(3+L)/(5+L)]^2,
C     cee=[R/(\hbar*c)]^2, R=r_0*A^(2/3), r_0=1.2 fm => cee=3.7D-5*A^(2/3)
C     xle(i) = f_Ei
C
C     Magnetic transitions:
C     f_M(L+1)/f_E(L+1) = cme,
C     cme= 10[\hbar/(m*c*R]^2 => cme = 0.307/A^(2/3)
C     xlm(i) = f_Mi
C
C
      DO i = 1, MAXmult
         xle(i) = 0.0D0
         xlm(i) = 0.0D0
      ENDDO
      IF (MAXmult.GT.2) THEN
         ha = A(Nnuc)**0.666666666666D0
         cee = 3.7D-5*ha
         cme = 0.307D0/ha
      ENDIF
C
      jmin = 1
Cp    jmin = MAX0(1, Jc - MAXmult)
      jmax = MIN0(NLW, Jc + MAXmult)

      Sum = 0.D0
      SCRtem(0) = 0.D0
      xjc = FLOAT(Jc) + HIS(Nnuc)

C-----clear scratch matrix (continuum)
      DO j = 1, NLW
         DO i = 1, NEX(Nnuc)
            SCRt(i,j,1,0) = 0.D0
            SCRt(i,j,2,0) = 0.D0
         ENDDO
      ENDDO
C-----clear scratch matrix (dNH_lchrete levels)
      DO i = 1, NLV(Nnuc)
         SCRtl(i,0) = 0.D0
      ENDDO
C-----IPOS is a parity-index of final states reached by gamma
C-----transitions which do not change parity (E2 and M1)
C-----INEG is a parity-index of final states reached by gamma
C-----transitions which do change parity (E1)
      IF (Iec.LT.1) RETURN
      IF (Ipc.GT.0) THEN
         ipos = 1
         ineg = 2
      ELSE
         ipos = 2
         ineg = 1
      ENDIF
C-----
C-----decay to the continuum
C-----
C-----do loop over c.n. energies (loops over spins and parities expanded)
      DO ier = Iec - 1, 1, -1
         IF (ier.EQ.1) THEN
            corr = 0.5D0
         ELSE
            corr = 1.0D0
         ENDIF
         eg = EX(Iec,Nnuc) - EX(ier,Nnuc)
         xle(1) = E1(Nnuc,eg, TNUc(ier, Nnuc),Uexcit(ier,Nnuc))
     &      *TUNe(0, Nnuc)
         xlm(1) = XM1(eg)*TUNe(0, Nnuc)
         xle(2) = E2(eg)*TUNe(0, Nnuc)
         IF(MAXmult.GT.2) THEN
            xlm(2) = xle(2)*cme
            DO i = 3, MAXmult
             xle(i) = xle(i-1)*eg**2*cee
     &                *((3.0D0 + FLOAT(i))/(5.0D0 + FLOAT(i)))**2
             xlm(i)= xle(i)*cme
            ENDDO
         ENDIF
         DO Jr = 1, jmax
            xjr = FLOAT(Jr) + HIS(Nnuc)
            lambmin = MAX0(1,ABS(Jc-Jr))
            lambmax = xjc + xjr + 0.001
            lambmax = MIN0(lambmax,MAXmult)
            IF(lambmin.LE.lambmax)THEN
               scrtpos = 0.0D0
               scrtneg = 0.0D0
               hsumtls =0.0D0
               DO lamb = lambmin, lambmax
                 IF(lamb/2*2.EQ.lamb)THEN
                   scrtpos = scrtpos + xle(lamb)
                   scrtneg = scrtneg + xlm(lamb)
                   IF(Nhrtw.EQ.0) hsumtls = hsumtls +
     &                         xle(lamb)**2 * RO(ier, Jr, ipos, Nnuc) +
     &                       xlm(lamb)**2 * RO(ier, Jr, ineg, Nnuc)
                  ELSE
                   scrtpos = scrtpos + xlm(lamb)
                   scrtneg = scrtneg + xle(lamb)
                   IF(Nhrtw.EQ.0) hsumtls = hsumtls +
     &                         xlm(lamb)**2 * RO(ier, Jr, ipos, Nnuc) +
     &                       xle(lamb)**2 * RO(ier, Jr, ineg, Nnuc)
C                  !first HRTW entry done
                 ENDIF
               ENDDO
               SCRt(ier, Jr, ipos, 0) = scrtpos*RO(ier, Jr, ipos, Nnuc)
               SCRt(ier, Jr, ineg, 0) = scrtneg*RO(ier, Jr, ineg,Nnuc)
C
C              Check, it could be we need to split hsumtls depending on parity !!!!
C
               H_Sumtls = H_Sumtls + hsumtls*corr
            ENDIF
         ENDDO
      ENDDO
C-----do loop over c.n. energies ***done***
C-----decay to the continuum ----** done***---------------------------
C-----integration of ro*gtl in continuum for ejectile 0 (TRAPEZOID
      DO j = jmin, jmax
         DO i = 1, Iec - 1
            Sum = Sum + SCRt(i,j,1,0) + SCRt(i,j,2,0)
         ENDDO
         Sum = Sum - 0.5*(SCRt(1,j,1,0) + SCRt(1,j,2,0))
      ENDDO
      Sum = Sum*DE
C-----integration of ro*gtl in continuum for ejectile 0 -- done ----
C-----
C-----DECAY TO DISCRETE LEVELS
C-----
C--------do loop over discrete levels -----------------------------------
         DO i = 1, NLV(Nnuc)
          lmin = ABS(xjc - XJLv(i,Nnuc)) + 0.001
          lmax = xjc + XJLv(i,Nnuc) + 0.001
          lambmin = MAX0(1,lmin)
          lambmax = MIN0(lmax,MAXmult)
          IF(lambmin.LE.lambmax)THEN
             eg = EX(Iec, Nnuc) - ELV(i, Nnuc)
             ipar = (1 + LVP(i, Nnuc)*Ipc)/2
             iodd = 1 - ipar
             xle(1) = E1(Nnuc,eg, TNUc(1, Nnuc),Uexcit(1,Nnuc))
     &           *TUNe(0, Nnuc)
             xlm(1) = XM1(eg)*TUNe(0, Nnuc)
             xle(2) = E2(eg)*TUNe(0, Nnuc)
             IF(lambmax.GT.2) THEN
              xlm(2) = xle(2)*cme
              DO j = 3, lambmax
               xle(j) = xle(j-1)*eg**2*cee
     &                  *((3.0D0 + FLOAT(j))/(5.0D0 + FLOAT(j)))**2
               xlm(j) = xle(j)*cme
              ENDDO
             ENDIF
             hscrtl = 0.0D0
             hsumtls = 0.0D0
             DO lamb = lambmin, lambmax
              IF(lamb/2*2.EQ.lamb)THEN
                 hscrtl = hscrtl +
     &                    xle(lamb)*ipar + xlm(lamb)*iodd
                 IF(Nhrtw.EQ.0)hsumtls = hsumtls +
     &                         xle(lamb)**2*ipar + xlm(lamb)**2*iodd
              ELSE
                 hscrtl = hscrtl +
     &                    xlm(lamb)*ipar + xle(lamb)*iodd
                 IF(Nhrtw.EQ.0)hsumtls = hsumtls +
     &                         xlm(lamb)**2*ipar + xle(lamb)**2*iodd
              ENDIF
             ENDDO
             IF(Nhrtw.EQ.0)H_Sumtls = H_Sumtls + hsumtls
             SCRtl(i, 0) = hscrtl
             Sum = Sum + SCRtl(i, 0)
          ENDIF
         ENDDO
C-----do loop over discrete levels --------- done --------------------
      SCRtem(0) = Sum
      DENhf = DENhf + Sum
      END
C
C
      SUBROUTINE TL2VL(T,Rho)
Ccc
Ccc   ********************************************************************
Ccc   *                                                         class:PPu*
Ccc   *                      T L 2 V L                                   *
Ccc   *                                                                  *
Ccc   * Service routine for HRTW. It stores strong transmission          *
Ccc   * coefficients, calculates sum of Tl**2, and sum of weak Tl's.     *
Ccc   * On entry with nhrtw>0 returns V that substitute Tl in the HF     *
Ccc   * formula.                                                         *
Ccc   *                                                                  *
Ccc   *                                                                  *
Ccc   * input:T    - transmission coefficient                            *
Ccc   *       Rho  - number of channels with this transmission coeff.    *
Ccc   *                                                                  *
Ccc   * output:      quantities needed for the HRTW theory (through      *
Ccc   *              the HRTW common):                                   *
Ccc   *                                                                  *
Ccc   *             H_Tl(.,.) - list of strong Tl's and their num. (rho) *
Ccc   *             H_Tloc(.,.) - l and ejectile for each strong Tl      *
Ccc   *             H_sumtls - sum of squared Tl's (all)                 *
Ccc   *             H_sweak - sum of weak Tl's (not squared)             *
Ccc   *             H_lch - number of strong Tl's                        *
Ccc   *             H_abs(.,.) - absorption x-sec decomposed in l's      *
Ccc   *             H_Tav - average transmission coefficient             *
Ccc   *             H_Tthr - threshold to consider Tl as strong          *
Ccc   *                                                                  *
Ccc   *                                                                  *
Ccc   * calls:                                                           *
Ccc   *                                                                  *
Ccc   *                                                                  *
Ccc   ********************************************************************
Ccc
      INCLUDE 'dimension.h'
C
C
C COMMON variables
C
      DOUBLE PRECISION H_Abs(NDHRTW2,3), H_Sumtl, H_Sumtls, H_Sweak,
     &                 H_Tav, H_Tl(NDHRTW1,2), H_Tthr, TFIs
      INTEGER MEMel(NDHRTW2,3), NH_lch, NSCh
      COMMON /IHRTW / NSCh, MEMel, NH_lch
      COMMON /RHRTW / H_Tl, H_Sumtl, H_Sumtls, H_Sweak, H_Abs, H_Tav,
     &                H_Tthr, TFIs
C
C Dummy arguments
C
      DOUBLE PRECISION Rho, T
C
C
      H_Sumtls = H_Sumtls + T**2*Rho
      IF (T.GT.H_Tthr) THEN
C        !strong or weak
         NH_lch = NH_lch + 1
         IF (NH_lch.LE.NDHRTW1) THEN
            H_Tl(NH_lch,1) = T      !record strong Tl's
            H_Tl(NH_lch,2) = Rho
         ENDIF
      ELSE
         H_Sweak = H_Sweak + T*Rho
      ENDIF
	  RETURN
      END

      DOUBLE PRECISION FUNCTION VT(Tl)
      INCLUDE 'dimension.h'
C
C
C COMMON variables
C
      DOUBLE PRECISION H_Abs(NDHRTW2,3), H_Sumtl, H_Sumtls, H_Sweak,
     &                 H_Tav, H_Tl(NDHRTW1,2), H_Tthr, TFIs
      INTEGER MEMel(NDHRTW2,3), NH_lch, NSCh
      COMMON /IHRTW / NSCh, MEMel, NH_lch
      COMMON /RHRTW / H_Tl, H_Sumtl, H_Sumtls, H_Sweak, H_Abs, H_Tav,
     &                H_Tthr, TFIs
C
C Dummy arguments
C
      DOUBLE PRECISION Tl
C
C Local variables
C
      DOUBLE PRECISION VT1
C
C
C
Ccc   *********************************************************************
CCc   *                                                          Class:PPu*
CCc   *                            V T                                    *
Ccc   *                                                                   *
Ccc   * Returns value of the HRTW quantity V corresponding to the         *
Ccc   * provided transmission coefficient (if the                         *
Ccc   * latter is below the threshold value H_Tthr or total number        *
Ccc   * of strong channels is bigger that NDHRTW1). If number of          *
Ccc   * strong channels is lower than NDHRTW1 and Tl is above the         *
Ccc   * threshold value H_Tthr then precalculated H_Tl(nsch) is returned. *
Ccc   * Note that VT keeps track of the number of processed strong        *
Ccc   * channels in the variable NSCh.                                    *
Ccc   *                                                                   *
Ccc   * input: Tl    - transmission coefficient                           *
Ccc   *                                                                   *
Ccc   * output: Tl                                                        *
Ccc   *                                                                   *
Ccc   *                                                                   *
Ccc   *********************************************************************
C
C
C COMMON variables
C
C
C Dummy arguments
C
C
C Local variables
C
      IF (NH_lch.GT.NDHRTW1) THEN
         VT = VT1(Tl,H_Tav,H_Sumtl)
      ELSEIF (Tl.LT.H_Tthr) THEN
         VT = VT1(Tl,H_Tav,H_Sumtl)
      ELSE
         NSCh = NSCh + 1
         VT = H_Tl(NSCh,1)
      ENDIF
	  RETURN
      END

      DOUBLE PRECISION FUNCTION VT1(Tl,Tav,Sumtl)
C
C
C Dummy arguments
C
      DOUBLE PRECISION Sumtl, Tav, Tl
C
C Local variables
C
      DOUBLE PRECISION EEF
C
C
C
Ccc   *****************************************************************
Ccc   *                                                      Class:PPu*
Ccc   *                          V T 1                                *
Ccc   *                                                               *
Ccc   * calculates V quantities replacing Tl's in the HRTW theory.    *
Ccc   * Performs just one iteration. To be used when number of        *
Ccc   * strong channels is bigger than NDRTW1.                        *
Ccc   *                                                               *
Ccc   * input: Tl    - transmission coefficient                       *
Ccc   * Tav   - average transmission coefficeint                      *
Ccc   * Sumtl - sum of transmission coefficients                      *
Ccc   *                                                               *
Ccc   * output: VT1                                                   *
Ccc   *                                                               *
Ccc   * Dummy arguments                                               *
Ccc   *                                                               *
Ccc   *                                                               *
Ccc   *****************************************************************
C
C
C Local variables
C
      VT1 = 0.D0
      IF (Sumtl.EQ.0.0D0) RETURN
      VT1 = Tl/Sumtl
      VT1 = 1.D0 + VT1*(EEF(Tl,Tav,Sumtl) - 1.D0)
      VT1 = Tl/VT1
	  RETURN
      END

      DOUBLE PRECISION FUNCTION EEF(Tl,Tav,Sumtl)
C
C
C Dummy arguments
C
      DOUBLE PRECISION Sumtl, Tav, Tl
C
C Local variables
C
      DOUBLE PRECISION a, al
C
C
C
Ccc   *****************************************************************
Ccc   *                                                      Class:PPu*
Ccc   *                          E E F                                *
Ccc   *                                                               *
Ccc   * Calculates elastic enhancement factor for HRTW theory         *
Ccc   *                                                               *
Ccc   * input: Tl     - transmission coefficient                      *
Ccc   * Tav    - average of all transmission coefficients             *
Ccc   * Sumtl  - sum of transmission coefficients                     *
Ccc   *                                                               *
Ccc   * output: Eef - elastic enhancement factor                      *
Ccc   *                                                               *
Ccc   * Dummy arguments                                               *
Ccc   *                                                               *
Ccc   *                                                               *
Ccc   *****************************************************************
      IF (Tl.LT.1.0D-15) THEN
         EEF = 3.D0
         RETURN
      ENDIF
      al = 4*Tav/Sumtl*(1.D0 + Tl/Sumtl)/(1.D0 + 3*Tav/Sumtl)
      a = 87.D0*(Tl - Tav)**2*Tl**5/Sumtl**7
      EEF = 1.D0 + 2.D0/(1.D0 + Tl**al) + a
      IF(EEF.GT.3.D0) EEF = 3.D0
	  RETURN
      END

      SUBROUTINE AUSTER(V,Tav,Sumtl,Sweak,Lch,Ndhrtw1)
Ccc   *****************************************************************
Ccc   *                                                      Class:PPu*
Ccc   *                      A U S T E R                              *
Ccc   *                                                               *
Ccc   * Iterates for V quantities in HRTW theory assuming that weak   *
Ccc   * transmission coefficients remain constant and therefore are   *
Ccc   * not iterated.                                                 *
Ccc   *                                                               *
Ccc   * input: Tav   - average transmission coefficient               *
Ccc   * Sumtl - sum of all transmission coefficients                  *
Ccc   * Sweak - sum of weak (Tl<H_Tthr) transmission coefficients     *
Ccc   * Lch   - number of strong (Tl>H_Tthr) channels                 *
Ccc   *                                                               *
Ccc   * input/output: V    - matrix of Tl's (input) or V's (output)   *
Ccc   *                                                               *
Ccc   *                                                               *
Ccc   *                                                               *
Ccc   * Dummy arguments                                               *
Ccc   *                                                               *
Ccc   *                                                               *
Ccc   *****************************************************************
C
C
C Dummy arguments
C
      INTEGER Lch, Ndhrtw1
      DOUBLE PRECISION Sumtl, Sweak, Tav
      DOUBLE PRECISION V(Ndhrtw1,2)
C
C Local variables
C
      DOUBLE PRECISION e(Ndhrtw1), sum, sv, vd(Ndhrtw1), vp(Ndhrtw1)
      DOUBLE PRECISION EEF
      INTEGER i, icount
C
C
C
      IF (Lch.GT.Ndhrtw1) THEN
         WRITE (8,*)
     &             'ERROR in AUSTER: Lch bigger than allowed by NDHRTW1'
         WRITE (8,*) 'If you see this printed it means a BUG!'
         STOP
      ENDIF
      icount = 0
      sv = Sweak
      sum = Sweak
      DO i = 1, Lch
         e(i) = EEF(V(i,1),Tav,Sumtl) - 1.d0
         sum = sum + V(i,1)*V(i,2)
      ENDDO
      DO i = 1, Lch
         vd(i) = V(i,1)
         vp(i) = V(i,1)
      ENDDO
  100 DO i = 1, Lch
         vp(i) = V(i,1)/(1.d0 + vp(i)*e(i)/sum)
         sv = sv + vp(i)*V(i,2)
      ENDDO
      icount = icount + 1
      IF (icount.GT.200) THEN
         WRITE (8,*) ' Maximum iteration number (200) reached in AUSTER'
         WRITE (8,*) 
         RETURN
      ENDIF
      sum = sv
      sv = Sweak
      DO i = 1, Lch
C
C--------relative accuracy of V is set below and may be altered
C--------to any resonable value.  
C
         IF (ABS(vd(i)-vp(i)).GT.1.D-7*vp(i)) GOTO 200
      ENDDO
      DO i = 1, Lch
         V(i,1) = vp(i)
      ENDDO
      RETURN
  200 DO i = 1, Lch
         vd(i) = vp(i)
      ENDDO
      GOTO 100
      END

      SUBROUTINE ELCORR(Nnuc,Iec,Jc,Ipc,Nnur,Nejc,Nhrtw)
Ccc
Ccc   ********************************************************************
Ccc   *                                                         class:PPu*
Ccc   *                      E L C O R R                                 *
Ccc   *                                                                  *
Ccc   * Corrects elastic widths for the elastic enhancement by adding    *
Ccc   * appropriate addditional part to scratch matrices.                *
Ccc   *                                                                  *
Ccc   *                                                                  *
Ccc   * input:NNUC - decaying nucleus index                              *
Ccc   *       IEC  - energy index of the decaying state                  *
Ccc   *       JC   - spin index of the decaying state                    *
Ccc   *       IPC  - parity of the decaying state                        *
Ccc   *       NNUR - residual nucleus index                              *
Ccc   *       NEJC - ejectile index                                      *
Ccc   *       nhrtw- 0 MUST NOT be used in this routine                  *
Ccc   *             >0 final HRTW entry (calculate emission widths)      *
Ccc   *                                                                  *
Ccc   *                                                                  *
Ccc   *                                                                  *
Ccc   * calls:TLLOC                                                      *
Ccc   *                                                                  *
Ccc   *                                                                  *
Ccc   *                                                                  *
Ccc   ********************************************************************
Ccc
      INCLUDE 'dimension.h'
      INCLUDE 'global.h'
C
C
C COMMON variables
C
      DOUBLE PRECISION ELTl(NDLW), H_Abs(NDHRTW2,3), H_Sumtl, H_Sumtls,
     &                 H_Sweak, H_Tav, H_Tl(NDHRTW1,2), H_Tthr, TFIs
      INTEGER MEMel(NDHRTW2,3), NH_lch, NSCh
      COMMON /ELASTIC/ ELTl
      COMMON /IHRTW / NSCh, MEMel, NH_lch
      COMMON /RHRTW / H_Tl, H_Sumtl, H_Sumtls, H_Sweak, H_Abs, H_Tav,
     &                H_Tthr, TFIs
C
C Dummy arguments
C
      INTEGER Iec, Ipc, Jc, Nejc, Nhrtw, Nnuc, Nnur
C
C Local variables
C
      DOUBLE PRECISION EEF, VT1
      DOUBLE PRECISION eout, eoutc, frde, popadd, s, smax, smin, tld, v,
     &                 xjc
      INTEGER i, iel, il, ipar, kel, l, lmax, lmin
C
      xjc = FLOAT(Jc) + HIS(Nnuc)
C-----
C-----decay to discrete levels
C-----
         eoutc = EX(Iec,Nnuc) - Q(Nejc,Nnuc)
C--------only target state considered
         i = LEVtarg
         eout = eoutc - ELV(i,Nnur)
         IF (eout.LT.DE) THEN
C--------level above the bin
            IF (eout.LT.0.0D0) RETURN
         ENDIF
         CALL TLLOC(Nnur,Nejc,eout,il,frde)
         smin = ABS(XJLv(i,Nnur) - SEJc(Nejc))
         smax = XJLv(i,Nnur) + SEJc(Nejc) + 0.01
         s = smin
C--------loop over channel spin ----------------------------------------
   50    lmin = INT(ABS(xjc - s) + 1.01)
         lmax = INT(xjc + s + 1.01)
         lmax = MIN0(NLW,lmax)
C--------do loop over l ------------------------------------------------
         DO l = lmin, lmax
            ipar = 1 + LVP(i,Nnur)*Ipc*( - 1)**(l - 1)
            tld = ELTl(l)
            IF (l.EQ.INT(H_Abs(Nhrtw,2)) .AND. (2.0*s).EQ.H_Abs(Nhrtw,3)
     &          .AND. ipar.NE.0) THEN
C--------------got a true elastic channel
               IF (tld.GT.H_Tthr .AND. NH_lch.LE.NDHRTW1) THEN
                  DO iel = 1, NDHRTW2
                     IF (MEMel(iel,3).EQ.INT(2.0*s + 0.001) .AND.
     &                   MEMel(iel,2).EQ.l) THEN
                        kel = MEMel(iel,1)
                        v = H_Tl(kel,1) 
                     ENDIF
                  ENDDO
               ELSE
                  v = VT1(tld,H_Tav,H_Sumtl)
               ENDIF
               popadd = v*(EEF(tld,H_Tav,H_Sumtl) - 1.0) 
               SCRtl(i,Nejc) = SCRtl(i,Nejc) + popadd
               SCRtem(Nejc) = SCRtem(Nejc) + popadd
            ENDIF
         ENDDO
C--------do loop over l --- done ----------------------------------------
         s = s + 1.
         IF (s.LE.smax) GOTO 50
C--------loop over channel spin ------ done ----------------------------
      RETURN
      END

      SUBROUTINE HRTW_MARENG(Npro,Ntrg,Jcn,Ip,Ich)
Ccc
Ccc   ********************************************************************
Ccc   *                                                         class:ppu*
Ccc   *                H R T W _ M A R E N G                             *
Ccc   *                                                                  *
Ccc   * Calculates initial compound nucleus population after projectile  *
Ccc   * absorption  using transmission coefficients calculated in MARENG *
Ccc   * for elastic channel.                                             *
Ccc   *                                                                  *
Ccc   * input:Npro - projectile index (normally 0)                       *
Ccc   *       Ntrg - target index (normally 0)                           *
Ccc   *       Jcn  - Compound Nucleus spin                               *
Ccc   *       Ip   - Compound Nucleus parity                             *
Ccc   *                                                                  *
Ccc   * output: Ich  - number of physical channels contributing to JCN Ip*
Ccc   *                                                                  *
Ccc   *                                                                  *
Ccc   *                                                                  *
Ccc   ********************************************************************
Ccc
      INCLUDE 'dimension.h'
      INCLUDE 'global.h'
C
C
C COMMON variables
C
      DOUBLE PRECISION ELTl(NDLW), H_Abs(NDHRTW2,3), H_Sumtl, H_Sumtls,
     &                 H_Sweak, H_Tav, H_Tl(NDHRTW1,2), H_Tthr, TFIs
      INTEGER MEMel(NDHRTW2,3), NH_lch, NSCh
      COMMON /ELASTIC/ ELTl
      COMMON /IHRTW / NSCh, MEMel, NH_lch
      COMMON /RHRTW / H_Tl, H_Sumtl, H_Sumtls, H_Sweak, H_Abs, H_Tav,
     &                H_Tthr, TFIs
C
C Dummy arguments
C
      INTEGER Ich, Ip, Jcn, Npro, Ntrg
C
C Local variables
C
      DOUBLE PRECISION ak2, chsp, coef, ecms, el, s1, smax, smin,
     &                 vl, xmas_npro, xmas_ntrg
      REAL FLOAT
      LOGICAL relcal
      INTEGER ichsp, iel, k, kel, lmax, lmin, mul
      INTEGER INT, MIN0
      DOUBLE PRECISION PAR
      DOUBLE PRECISION VT1
C
C
      PAR(i,ipa,l) = 0.5*(1.0 - ( - 1.0)**i*ipa*( - 1.0)**l)

      xmas_npro = EJMass(Npro) 
      xmas_ntrg = AMAss(Ntrg)

      el = EINl
      relcal = .FALSE.
      IF (IRElat(Npro,Ntrg).GT.0 .OR. RELkin) relcal = .TRUE.
      CALL KINEMA(el,ecms,xmas_npro,xmas_ntrg,ak2,1,relcal)
      coef = 10.d0*PI/ak2/
     &    (2*XJLv(LEVtarg,Ntrg) + 1.0)/(2*SEJc(Npro) + 1.0)
      s1 = 0.5
      IF (AINT(XJLv(LEVtarg,Ntrg) + SEJc(Npro)) - XJLv(LEVtarg,Ntrg)
     &    - SEJc(Npro).EQ.0.0D0) s1 = 1.0

C-----channel spin min and max
      smin = ABS(SEJc(Npro) - XJLv(LEVtarg,Ntrg))
      smax = SEJc(Npro) + XJLv(LEVtarg,Ntrg)
      mul = smax - smin + 1.0001

      Ich = 1
      DO ichsp = 1, mul
         chsp = smin + FLOAT(ichsp - 1)
         lmin = ABS(Jcn - chsp - s1) + 0.0001
         lmax = Jcn + chsp - s1 + 0.0001
         lmin = lmin + 1
         lmax = lmax + 1
         lmax = MIN0(NDLW,lmax)
         DO k = lmin, lmax
            IF (PAR(Ip,LVP(LEVtarg,Ntrg),k - 1).NE.0.0D0) THEN
               IF (Ich.GT.NDHRTW2) THEN
                  WRITE (8,*) ' '
                  WRITE (8,*) 'E R R O R !'
                  WRITE (8,*)
     &                    'INSUFFICIENT DIMENSION FOR HRTW CALCULATIONS'
                  WRITE (8,*) 'INCREASE NDHRTW2 IN THE dimension.h',
     &                        ' AND RECOMPILE.'
                  STOP 'INSUFFICIENT DIMENSION: NDHRTW2'
               ENDIF
               IF (NH_lch.GT.NDHRTW1) THEN
                  vl = VT1(ELTl(k),H_Tav,H_Sumtl)
               ELSEIF (ELTl(k).LT.H_Tthr) THEN
                  vl = VT1(ELTl(k),H_Tav,H_Sumtl)
               ELSE
                  kel = 0
                  DO iel = 1, NDHRTW2
                     IF (MEMel(iel,3).EQ.INT(2.0*chsp + 0.001) .AND.
     &                   MEMel(iel,2).EQ.k) kel = MEMel(iel,1)
                  ENDDO
                  IF (kel.EQ.0) THEN
                     WRITE (8,*) ' '
                     WRITE (8,*) ' MISMATCH OF ELASTIC CHANNEL IN HRTW'
                     WRITE (8,*) ' REPORT THIS ERROR ALONG WITH RELATED'
                     WRITE (8,*) ' INPUT FILE TO: mwherman@bnl.gov'
                     STOP ' MISMATCH OF ELASTIC CHANNEL IN HRTW'
                  ENDIF
                  vl = H_Tl(kel,1)
               ENDIF
               IF (vl.NE.0.0D0) THEN
                  H_Abs(Ich,1) = vl*coef*(FLOAT(2*Jcn + 1) - 2*s1)
     &                           *FUSred*REDmsc(Jcn,Ip)*DRTl(k)
                  H_Abs(Ich,2) = k
                  H_Abs(Ich,3) = 2*chsp
                  Ich = Ich + 1
               ENDIF
            ENDIF
         ENDDO
      ENDDO
      Ich = Ich - 1
	  RETURN
      END