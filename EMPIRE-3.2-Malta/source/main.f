cc   * $Rev: 3454 $
Ccc   * $Author: gnobre $
Ccc   * $Date: 2013-07-19 18:11:23 +0200 (Fr, 19 Jul 2013) $

      SUBROUTINE EMPIRE
Ccc
Ccc   ********************************************************************
Ccc   *                                                         class:ppu        
Ccc   *                         E M P I R E                                    
Ccc   *                                                                        
Ccc   *               Used to be main of the EMPIRE code 
Ccc   *                                                 
Ccc   *                                                 
Ccc   ********************************************************************

      use nubar_reader

      INCLUDE "dimension.h"
      INCLUDE "global.h"
C
C
C COMMON variables
C
      DOUBLE PRECISION vdef_1d(NFISBARPNT),eps_1d(NFISBARPNT)            ! NUMBAR
      INTEGER npoints, iiextr(0:2*NFPARAB), nextr                        ! NUMBAR

      DOUBLE PRECISION TFIso, TGIso, TISo, RFIso, PFIso                  ! FIS_ISO

      DOUBLE PRECISION ELAcs, TOTcs, ABScs, SINl, SINlcc, SINlcont       ! ECISXS

      INTEGER*4 INDexf, INDexb, BUFfer(250)                              ! R250COM

      DOUBLE PRECISION XCOs(NDAngecis)                                   ! KALB
      DOUBLE PRECISION ELTl(NDLW)
      
      DOUBLE PRECISION GET_DDXS
      DOUBLE PRECISION xs_cn, xs_norm 

      COMMON /NUMBAR/  eps_1d, vdef_1d, npoints, iiextr, nextr
      COMMON /FIS_ISO/ TFIso, TGIso, TISo, RFIso, PFIso
      COMMON /ECISXS/ ELAcs, TOTcs, ABScs, SINl, SINlcc, SINlcont
      COMMON /ELASTIC/ ELTl
      COMMON /R250COM/ INDexf,INDexb,BUFfer
      COMMON /KALB/ XCOs

      DOUBLE PRECISION eenc(500),signcf(500)
      INTEGER nrnc
      COMMON /inp_sp5/ eenc,signcf,nrnc

      LOGICAL lbreakup, ltransfer, lheader
      COMMON /LPEXS/lbreakup, ltransfer 

      DOUBLE PRECISION crossNT(0:NDEJC),crossNTt,
     &                 crossPE(0:NDEJC),crossPEt
      COMMON /PEXS/ crossNT,crossNTt,crossPE,crossPEt

      DOUBLE PRECISION specBU(0:NDEJC,NDEX),crossBU(0:NDEJC),crossBUt
      COMMON /CBREAKUP/specBU,crossBU,crossBUt
C
C Local variables
C
      DOUBLE PRECISION ares, atotsp, coef, ! controln, controlp,
     &                 corrmsd, csemax, csemist, csmsdl, csum, espec,
     &                 dang, ded, delang, echannel, xscclow, csinel,
     &                 ecm, elada(NDAngecis), elleg(NDAngecis), erecoil,
     &                 epre, ftmp, gang, grand, ! spechk(4),
     &                 gtotsp, htotsp, pope, poph, popl, popleft,
     &                 poplev, popread, poptot, ptotsp, q2, q3, qmax,
     &                 qstep, sgamc, spdif, spdiff, stauc, totcorr,
     &                 step, sum, sumfis, sumfism(NFMOD), xnub,
     &                 totemis, weight, xcse, xizat, xnl, xnor, tothms,
     &                 xtotsp, xsinlcont, xsinl, zres, angstep, checkXS,
     &                 cseaprnt(ndecse,ndangecis),check_DE(ndecse),
     &                 check_DL(NDLV),disc_int(NDLV),pop_disc(0:ndejc),
     &                 cel_da(NDAngecis), totener_out, totener_in,
     &                 emedg, emedn, emedp, emeda, emedd, emedt, emedh, 
     &                 cmulg, cmuln, cmulp, cmula, cmuld, cmult, cmulh, 
C                      -----------------------------------------------
C                      PFNS quantities  
C                      Total prompt fission spectra only for neutrons
C                             and assumed isotropic 
     &                 emiss_en(NDEPFN),
     &                 tequiv, fmaxw, fnorm, eincid, eneutr, ftmp1, 
     &                 post_fisn(NDEPFN),csepfns(NDEPFN), deltae_pfns,
     &                 ratio2maxw(NDEPFN),enepfns(NDEPFN),fmed, 
C                      -----------------------------------------------
     &                 csetmp(ndecse), ftmpA, ftmpB, csmax, val, dtmp,
     &                 fisxse, eps, checkprd,ftmp_gs, recorp, htmp,
     &                 xcross(0:NDEJC+3,0:15,0:20), cspg, dcor,
     &                 xnorm(0:NDEJC,NDExclus),xsdirect, xspreequ, xsmsc

C     For lifetime calculation, now commented (RCN/MH Jan 2011)
C     DOUBLE PRECISION taut,tauf,gamt,gamfis

      DOUBLE PRECISION SFACTOR
      CHARACTER*9 cejectile
      CHARACTER*3 ctldir
      CHARACTER*6 keyname
      CHARACTER*23 ctmp23
      CHARACTER*36 nextenergy
      CHARACTER*72 rtitle
      CHARACTER*72 inprecord,ctmp

      INTEGER i, ia, iad, iam, iang, iang1, ib, icalled, nfission,
     &        icsh, icsl, ie, iizaejc, il, iloc, ilv, imaxt,
     &        imint, ip, ipar, irec, ispec, itimes, its, iz, izares, j,
     &        jcn, ke, kemax, kemin, ltrmax, m, jz, jn, jzmx, jnmx, 
     &        nang, nbr, ncoll, nejc, nejcec, nnuc, jfiss, 
     &        nnur, nnurec, nspec, neles, ncon,
     &        ikey1, ikey2, ikey3, ikey4, nuc_print, ilevcol,
C             -----------------------------------------------
C             PFNS quantities  
C             Total prompt fission spectra only for neutrons
     &        nepfns
C             -----------------------------------------------
      INTEGER max_prn
      LOGICAL nvwful, fexist, nonzero
      CHARACTER*21 reactionx, preaction(ndnuc)
C
      PARAMETER (max_prn = 30) ! maximum number of reactions to print by cs2zvd
C
      real*8, external :: mu_bar

      DATA ctldir/'TL/'/

      call open_files
      lbreakup  = .false.
      ltransfer = .false.

      icalled = 0
      CALL THORA(8)
      EIN = 0.0d0
      epre=EIN
      ICAlangs = 0

      DO jn = 0, 20
        DO jz = 0, 15
          DO nejc = 0, NDEJC + 3
            xcross(nejc,jz,jn) = 0.0d0
          ENDDO
        ENDDO
      ENDDO

C-----
C-----Skip mandatory part of the standard input
C-----
      REWIND 5
      DO i=1,10
         READ(5,*)
      ENDDO
C-----Read line of optional input
  150 READ (5,'(A72)') inprecord
      IF (inprecord(1:1).EQ.'*' .OR. inprecord(1:1).EQ.'#' .OR.
     &    inprecord(1:1).EQ.'!') GOTO 150  ! comments 

      IF(inprecord(1:1).EQ.'@') THEN ! title
        do j = 1,72
          EMPtitle(j:j) = inprecord(j:j) ! title of the run
        enddo
        EMPtitle(1:1)= ' '
      ENDIF
      REWIND 5
C-----
C-----Read and prepare input data
C-----
 1300 CALL INPUT
C
      IF (FITomp.LT.0) OPEN (40,FILE = 'OPTFIT.CAL',STATUS='UNKNOWN')
C-----
C-----Print input data
C-----
      IF (IOUt.GT.0) CALL PRINPUT
      WRITE (*,'(/''   C.M. incident energy '',G12.5,'' MeV'')') EIN
      WRITE (8,'(/''   C.M. incident energy '',G12.5,'' MeV'')') EIN
C-----
C-----Print results of the systematics
C-----
      IF (FIRst_ein .AND. AEJc(0).EQ.1 .AND. ZEJc(0).EQ.0) 
     >    CALL SYSTEMATICS(SNGL(A(0)),SNGL(Z(0)),1)

C-----Clear CN elastic cross section (1/4*pi)
      ELCncs = 0.0D+0 
      cel_da = 0.0D+0           
C-----
C-----Open file 41 with tabulated cross sections
C-----
      IF (FIRst_ein) THEN
        OPEN (53,FILE='LOW_ENERGY.OUT', STATUS = 'UNKNOWN')
        OPEN (41, FILE='XSECTIONS.OUT' , STATUS='unknown')

        IF (SFACT.gt.0) then
          OPEN (unit = 781, file = "S-FACTOR.DAT")
          WRITE(781,*) '#Ecm(MeV)  Cross Section(b)  S-factor(MeV b)'
          WRITE(781,*) '#                                           '
        ENDIF

        IF(NNG_xs.gt.0) 
     &    OPEN (104, FILE='GAMMA_INT.DAT', STATUS='unknown')

        OPEN (107, FILE='EL_INEL.DAT'  , STATUS='unknown')
        OPEN (108, FILE='TOTCOR.DAT'   , STATUS='unknown')
        OPEN (110, FILE='CN-LEV-XS.DAT', STATUS='unknown')


        DO nejc = 1, NEJcm
          IF(NTReac(nejc).GT.0.01d0) ltransfer = .true.
          IF(BUReac(nejc).GT.0.01d0) lbreakup  = .true.
        ENDDO

        IF(ltransfer) OPEN (112, FILE='TRANSFER-XS.DAT')
        IF(lbreakup ) OPEN (113, FILE='BREAK-UP-XS.DAT')

        IF(ltransfer .or. lbreakup) OPEN (114, FILE='REAC-MECH.DAT')
        
        i = 0
        DO nnuc=1,NNUcd
          if( REAction(nnuc)(1:2).eq.'(z' ) then
            i = i + 1
              preaction(i) = REAction(nnuc)
          endif
        ENDDO
        nuc_print = i

        IF (A(0).gt.220 .AND. 
     &   ZEJc(NPRoject).EQ.0 .AND. AEJc(NPRoject).GT.0) then 
C
C        elastic and nonelastic modified for actinides
C        to include/exclude low-lying coupled states
         WRITE(41, '(''#'',I3,6X,A1,'' + '',i3,''-'',A2,''-'',I3,5x,
     &A118)') 
     &      nuc_print+6,SYMbe(0), int(Z(0)), SYMb(0), int(A(0)),
     &   ' Elastic* and Nonelast* modified for A>220 (Cross sections of 
     &2 CC added/substracted to Elastic/Nonelast respectively)'
         WRITE(107, '(''#'',I3,6X,A1,'' + '',i3,''-'',A2,''-'',I3,5x,
     &A118)') 
     &      15         ,SYMbe(0), int(Z(0)), SYMb(0), int(A(0)),
     &   ' (Elastic cross section = Shape Elastic + Compound Elastic; fo  
     &r Elastic* and Nonelastic* see output *.xsc file)       '
C    &      15         ,SYMbe(0), int(Z(0)), SYMb(0), int(A(0)),
C    &   ' Elastic* and Nonelast* modified for A>220 (Cross sections of 
C    &2 CC added/substracted to Elastic/Nonelast respectively)'

         WRITE(41,'(''#'',A10,1X,1P,95A12)') '  Einc    ',
     &      '  Total     ','  Elastic*  ','  Nonelast* ',
     &      '  Fission   ','  Mu-bar    ','  Nu-bar    ',
     &         (preaction(nnuc),nnuc=1,min(nuc_print,max_prn))

C        WRITE(107,'(''#'',A10,1X,1P,20A12)')'   Einc   ',
C    &      '  Total     ','  Elastic*  ','   CN-el    ',
C    &      ' Nonelast*  ','  CN-form   ','  Direct    ',
C    &      'Pre-equil   ','Coup-Chan   ',' DWBA-disc  ',
C    &      'DWBA-cont   ','   MSD      ','    MSC     ',
C    &      '  PCROSS    ','   HMS      ','  CC(2 lev) '

         WRITE(107,'(''#'',A10,1X,1P,20A12)')'   Einc   ',
     &      '  Total     ','  Elastic   ','   CN-el    ',
     &      ' Shape-el   ',
     &      ' Nonelast   ','  CN-form   ','  Direct    ',
     &      'Pre-equil   ','Coup-Chan   ',' DWBA-disc  ',
     &      'DWBA-cont   ','   MSD      ','    MSC     ',
     &      '  PCROSS    ','   HMS      ','  CC(2 lev) '

        ELSE

         WRITE(41, '(''#'',I3,6X,A1,'' + '',i3,''-'',A2,''-'',I3)') 
     &      nuc_print+6,SYMbe(0), int(Z(0)), SYMb(0), int(A(0))
         WRITE(107,'(''#'',I3,6X,A1,'' + '',i3,''-'',A2,''-'',I3)') 
     &      15         ,SYMbe(0), int(Z(0)), SYMb(0), int(A(0))

         WRITE(41,'(''#'',A10,1X,1P,95A12)') '  Einc    ',
     &      '  Total     ','  Elastic   ','  Nonelast  ',
     &      '  Fission   ','  Mu-bar    ','  Nu-bar    ',
     &         (preaction(nnuc),nnuc=1,min(nuc_print,max_prn))

         WRITE(107,'(''#'',A10,1X,1P,20A12)')'   Einc   ',
     &      '  Total     ','  Elastic   ','   CN-el    ',
     &      ' Shape-el   ',
     &      ' Nonelast   ','  CN-form   ','  Direct    ',
     &      'Pre-equil   ','Coup-Chan   ',' DWBA-disc  ',
     &      'DWBA-cont   ','   MSD      ','    MSC     ',
     &      '  PCROSS    ','   HMS      ','  CC(2 lev) '
        ENDIF

        OPEN (98, FILE='FISS_XS.OUT', STATUS='unknown')
        WRITE(98,'(''#'',I3,6X,A1,'' + '',i3,''-'',A2,''-'',I3)') 
     &     10, SYMbe(0), int(Z(0)), SYMb(0), int(A(0))
        WRITE(98,'(''#'',A10,1X,1P,20A12)')'   Einc   ',
     &      '  Fiss-tot  ','  Fiss-1st  ','  Fiss-2nd  ',
     &      '  Fiss-3rd  ','  Fiss-4rd  ','  Fiss-5th  ',
     &      '  Fiss-6th  ','  Fiss-7th  ','  Fiss-8th  ',
     &      '  Fiss-9th  ','  Fiss-10th '
        IF (FISspe.GT.0) THEN
          OPEN (73, FILE='PFNS.OUT', STATUS='unknown')
          OPEN (74, FILE='PFNM.OUT', STATUS='unknown')
          if(IOPsys.eq.0) then  !Linux, Mac
            WRITE(74,
     &   '(''   Elab     <Epfns>  nubar(EVAL)  Tmaxw(equiv) '')')
          else                  !Windows
            WRITE(74,
     &   '(''   Elab     <Epfns>  nubar(TEST)  Tmaxw(equiv) '')')
          endif
        ENDIF
        IF (ltransfer) then                     
          WRITE(112,'(10X,i3,1x,A2,1X,I3)')int(Z(0)), SYMb(0), int(A(0))
          WRITE(112,'(2a12,a10,'',n'',a10,'',p'',a10,'',a'',a10,'',d'',
     &      a10,'',t'',a10,'',He'')')'   Einc   ',' Total NT ',
     &      symbe(0),symbe(0),symbe(0),symbe(0),symbe(0),symbe(0)   
        ENDIF
        IF (lbreakup) then                     
          WRITE(113,'(10X,i3,1x,A2,1X,I3)')int(Z(0)), SYMb(0), int(A(0))
          WRITE(113,'(2a12,a10,'',n'',a10,'',p'',a10,'',a'',a10,'',d'',
     &      a10,'',t'',a10,'',He'')')'   Einc   ',' Total BU ',
     &      symbe(0),symbe(0),symbe(0),symbe(0),symbe(0),symbe(0) 
        ENDIF
        IF (lbreakup .or. ltransfer) then                     
          WRITE(114,'(10X,i3,1x,A2,1X,I3)')int(Z(0)), SYMb(0), int(A(0))
          WRITE(114,*)'  Einc         Reaction     Breakup      Transfer
     &     Preequil.    CN-form '
        ENDIF
      ENDIF
C-----
C-----Prepare Giant Resonance parameters - systematics
C-----
      CALL WHERE(IZA(0),nnuc,iloc)
      if(iloc.eq.0) then
        GDRpar(1,0) = GDRpar(1,nnuc) 
        GDRpar(2,0) = GDRpar(2,nnuc) 
        GDRpar(3,0) = GDRpar(3,nnuc) 
        GDRpar(4,0) = GDRpar(4,nnuc) 
        GDRpar(5,0) = GDRpar(5,nnuc) 
        GDRpar(6,0) = GDRpar(6,nnuc) 
        GDRpar(7,0) = GDRpar(7,nnuc) 
        GDRpar(8,0) = GDRpar(8,nnuc) 
        GDRpar(9,0) = GDRpar(9,nnuc) 
        GDRpar(10,0) = GDRpar(10,nnuc) 
      endif

      CALL ULM(0) ! target
C
      CALL ULM(1) ! CN

      ngamm_tr = 0
      nfiss_tr = 0
	gamm_tr  = 0.d0
	fiss_tr  = 0.d0
C
C	RCN, FEB 2013
C     For a proper consideration of fission and capture composition in the 
C        ECIS CN calculation, further changes needed in tl.f (to be done later)
C
C     IF(.NOT.CN_isotropic) THEN
      IF(.FALSE.) THEN

C-------DO loop over c.n. excitation energy for the highest bin
C              we neglect gamma cascade for the time being
        nnuc = 1
        ke   = NEX(nnuc)
        OPEN (80,FILE = 'FISTMP.OUT')
        IF (FISsil(nnuc) .AND. NINT(FISshi(Nnuc)).NE.1)
     &    CALL READ_INPFIS(nnuc)

        DO ipar = 1, 2 !over decaying CN parity
          ip = INT(( - 1.0)**(ipar + 1))
C         The first parity should be the corresponding to the GS !! (+ for even-even)
          DO jcn = 1, NLW !over decaying CN spin
            IF (GDRdyn.EQ.1.0D0) CALL ULMDYN(nnuc,jcn,EX(ke,nnuc))
C-----------gamma emision
            CALL TL_GAMMA(nnuc,ke,jcn,ip)
C-----------fission ()
            IF (FISsil(nnuc) .AND. NINT(FISshi(nnuc)).NE.1 )        
     &        CALL FISCROSS(nnuc,ke,ip,jcn,sumfis,sumfism)
            if(sumfis.lt.1.d-6) CYCLE
            fiss_tr(jcn,ipar) =  sumfis
            nfiss_tr = jcn
          ENDDO !loop over decaying nucleus spin
          write(*,*) '+++'
          write(*,*) ' Nfis=',nfiss_tr,' Parity=',ip
          do jcn = 1, nfiss_tr
            write(*,*) 'J=',jcn,' fiss_tr=',fiss_tr(jcn,ipar)
          enddo
        ENDDO  !loop over decaying nucleus parity
        write(*,*) ' Jmax =',nfiss_tr
        write(*,*) ' Lmax =',ngamm_tr
        do lamb=1,ngamm_tr
          write(*,*) 'L',lamb,' tr=',gamm_tr(lamb)
        enddo
        write(*,*) '+++'
        CLOSE (80,STATUS='DELETE')

        DENhf  = 0.d0
	  sumfis = 0.d0

	ENDIF
C-----
C-----Calculate reaction cross section and its spin distribution
C-----
      CALL MARENG(0,0)
C     Total cross section is set to absorption cross section
C     for photon induced reactions (to process them in EMPEND)
C
      IF (INT(AEJc(0)).EQ.0) then
        TOTcs = CSFus
        ELAcs = 0.d0
      ENDIF
C
C-----Locate position of the projectile among ejectiles
      CALL WHEREJC(IZAejc(0),nejcec,iloc)
C
C-----Prepare Giant Resonance parameters - systematics
C-----
C-----Locate position of the target among residues
      CALL WHERE(IZA(1) - IZAejc(0),nnurec,iloc)

      if(iloc.eq.0 .and. nnurec.ne.0) then
        GDRpar(1,nnurec) = GDRpar(1,0) 
        GDRpar(2,nnurec) = GDRpar(2,0) 
        GDRpar(3,nnurec) = GDRpar(3,0) 
        GDRpar(4,nnurec) = GDRpar(4,0) 
        GDRpar(5,nnurec) = GDRpar(5,0) 
        GDRpar(6,nnurec) = GDRpar(6,0) 
        GDRpar(7,nnurec) = GDRpar(7,0) 
        GDRpar(8,nnurec) = GDRpar(8,0) 
        GDRpar(9,nnurec) = GDRpar(9,0) 
        GDRpar(10,nnurec)= GDRpar(10,0) 
      endif

      CALL WHERE(IZA(1),nnuc,iloc)
      if(iloc.eq.0 .and. nnuc.ne.1) then
        GDRpar(1,1) = GDRpar(1,nnuc) 
        GDRpar(2,1) = GDRpar(2,nnuc) 
        GDRpar(3,1) = GDRpar(3,nnuc) 
        GDRpar(4,1) = GDRpar(4,nnuc) 
        GDRpar(5,1) = GDRpar(5,nnuc) 
        GDRpar(6,1) = GDRpar(6,nnuc) 
        GDRpar(7,1) = GDRpar(7,nnuc) 
        GDRpar(8,1) = GDRpar(8,nnuc) 
        GDRpar(9,1) = GDRpar(9,nnuc) 
        GDRpar(10,1)= GDRpar(10,nnuc) 
      endif
C
C-----check whether NLW is not larger than 
C-----max spin at which nucleus is still stable 

      IF (NLW.GT.JSTab(1) .and. JSTab(1).GT.0) THEN
          WRITE (8,
     & '(''  WARNING: Maximum spin to preserve stability is'',I4)')
     &             JSTab(1)
          WRITE (8,
     & '(''  WARNING: Calculations will be truncated at this limit'')')
          WRITE (8,
     & '(''  WARNING: Maximum stable spin (rot. limit) Jstab < '',I3)') 
     & Jstab(1) + 1
          IF(Jstab(1).LE.NDLW) then
            ftmp1 = 0.d0
            DO j = Jstab(1), min(NDLW,NLW)
              ftmp1 = ftmp1 + POP(NEX(1),j,1,1) + POP(NEX(1),j,2,1)
              POP(NEX(1),j,1,1) = 0.d0
              POP(NEX(1),j,2,1) = 0.d0
            ENDDO
            CSFus = CSFus - ftmp1
            WRITE (8,'(''  WARNING: Some fusion cross section lost : '',
     & F9.3,'' mb, due to the stability limit'')') ftmp1  
          ELSE
            WRITE (8,
     &'(''  WARNING: Increase NDLW in dimension.h and recompile EMPIRE''
     & )')
          ENDIF
          NLW = min(JSTab(1),NDLW)
      ENDIF

      csmax = 0.d0
      DO ip = 1, 2
        DO j = 1, NDLW
          csmax = DMAX1(POP(NEX(1),j,ip,1),csmax)
        ENDDO
      ENDDO

      IF ((POP(NEX(1),NLW,1,1)*20.D0.GT.csmax .OR. POP(NEX(1),NLW,2,1)
     &    *20.D0.GT.csmax) .AND. NLW.EQ.NDLW) THEN
         WRITE (8,*) 'POP1=', POP(NEX(1),NLW,1,1), 'POP2=',
     &               POP(NEX(1),NLW,2,1), 'NLW=', NLW
         WRITE (8,
     &'('' NUMBER OF PARTIAL WAVES FOR WHICH CODE IS DIMENSIONE'',
     &''D IS INSUFFICIENT'',/,'' INCREASE NDLW IN THE dimensio'',
     &''n.h FILE AND RECOMPILE  '',/,'' EXECUTION  S T O P P E '',
     &''D '')')
         STOP 'ERROR: Insufficient dimension NDLW for partial waves'
      ENDIF
C
      WRITE (ctmp23,'(i3.3,i3.3,1h_,i3.3,i3.3,1h_,i9.9)') INT(ZEJc(0)),
     &       INT(AEJc(0)), INT(Z(0)), INT(A(0)), INT(EINl*1000000)
C     TOTcs, ABScs, ELAcs are initialized within MARENG()
      xsinlcont = 0.d0
      xscclow   = 0.d0
      xsinl     = 0.d0
      csinel    = 0.d0
      checkXS   = 0.d0
      disc_int  = 0.d0
      pop_disc  = 0.d0

C     For resolution function (Spreading levels in the continuum)
      isigma0 = 0
      IF(WIDcoll.GT.0.d0)
     &   isigma0 = INT((0.02d0  + sqrt(EINl)*WIDcoll)/DE + 1.0001)
      ncoll = 0
      ecm = EINl - EIN
      dang = PI/FLOAT(NANgela - 1)
      angstep = 180.d0/(NANgela-1)
      gang = 180.d0/(NDAng-1)
C-----
C-----Get ECIS results for the ground state
C-----
      IF (ICAlangs.GT.0) THEN
        OPEN (45,FILE = (ctldir//ctmp23//'.EXP'),STATUS = 'OLD',
     &      ERR = 1400)
        READ (45,*,END = 1400)   ! To skip first line <ANG.DIS.> ..
        READ (45,*,END = 1400)   ! To skip level identifier line
        DO iang = 1, NANgela
         READ (45,'(24x,D12.5)',END = 1400) elada(iang)
        ENDDO

      ELSE

        OPEN (45,FILE = (ctldir//ctmp23//'.LEG'),STATUS = 'OLD',
     &      ERR = 1400)
        READ (45,*,END = 1400,ERR = 1400) ctmp ! To skip first line <LEGENDRE> ..
        READ (45,'(5x,i5)',END = 1400,ERR = 1400) neles
        DO iang = 1, min(NDAng,neles)
           READ (45,'(10x,D20.10)',END = 1400,ERR = 1400) elleg(iang)
        ENDDO
        CLOSE(45)

        OPEN (45,FILE = (ctldir//ctmp23//'.ANG'),STATUS = 'OLD',
     &      ERR = 1400)
        READ (45,*,END = 1400) ctmp  ! To skip first line <ANG.DIS.> ..
        READ (45,*,END = 1400) ctmp  ! To skip level identifier line
        iang = 0
        DO iang1 = 1, NANgela
C----------To use only those values corresponding to EMPIRE grid for elastic XS
           READ (45,'(3x,12x,D12.5)',END = 1400) ftmp    ! ecis06
           if(mod(DBLE(iang1-1)*angstep+gang,gang).NE.0) cycle
           iang = iang +1
           elada(iang) = ftmp
        ENDDO
      ENDIF
C
C-----
C-----Get ECIS results for excited levels
C-----
      IF (DIRect.NE.0) THEN
          ggmr = 3.d0
          ggqr =85.d0*A(0)**(-2./3.)
          ggor =5.d0
          OPEN (46,FILE = (ctldir//ctmp23//'.ICS'),STATUS = 'OLD',
     &         ERR = 1400)
          READ (46,*,END = 1400) ctmp ! To skip first line <INE.C.S.> ..
C---------Get and add inelastic cross sections (including double-differential)
          DO i = 2, ND_nlv
 
           ilv = ICOller(i)

           IF(ilv.le.0) then
             WRITE(8,*) ' WARNING: Collective level #',ICOllev(i),
     &                  ' has wrong number, skipped'
             CYCLE                
           ENDIF

           IF(ICOllev(i).le.LEVcc .and. SINlcc.le.0.d0) exit
           IF(ICOllev(i).gt.LEVcc .and. SINl+SINlcont.le.0.d0) cycle

           IF(ilv.LE.NLV(nnurec)) then

             IF(ICOller(i).GT.40) then
               WRITE(8,*) ' WARNING: Collective level #',ICOller(i),
     &                  ' has wrong number (bigger than 40)'
               ilv = 40                 
             ENDIF
C
C            D_Elv(i)        Collective level energy (in collective level file)
C            ELV(ilv,nnurec) Discrete level energy
             IF(ABS(D_Elv(i) - ELV(ilv,nnurec)).gt.0.0001d0) THEN
               itmp = 0
               DO iang = 2, NLV(nnurec)
                 IF(D_Elv(i).LT.ELV(iang-1,NTArget)) then
                   itmp = iang-1
                       IF(abs(D_Elv(i)-ELV(iang-1,NTArget)).gt. 
     &                abs(D_Elv(i)-ELV(iang,NTArget)) ) itmp = iang
                   exit
                 ENDIF               
               ENDDO
               IF(itmp.gt.0) then
                 WRITE(8,*)' WARNING: Energy of the collective level #',
     &             ICOllev(i)
                 WRITE(8,*)
     &      ' WARNING: not equal to the energy of the discrete level #', 
     &             ilv
                 WRITE(8,*) 
     &          ' WARNING: Cross section reassigned to discrete level #'
     &           , itmp
                 ilv        = itmp
                 ICOller(i) = itmp
                 ICOllev(i)   = itmp + LEVcc
                 WRITE(8,*) 
               ELSE
                 WRITE(8,*) 
     &            ' ERROR: Delete the collective level #',ICOller(i)
                 STOP ' ERROR: see the long output'
               ENDIF
             ENDIF
C
C------------Adding inelastic to discrete levels
             echannel = EX(NEX(1),1) - Q(nejcec,1) - ELV(ilv,nnurec)
C------------Avoid reading closed channels
             IF (echannel.GE.0.0001) THEN
               xcse = echannel/DE + 1.0001
               icsl = INT(xcse)
               icsh = icsl + 1
               READ (46,*,END = 1400) popread
               popread = popread*FCCred
C
C              Storing the particle inelastic to the first two low-lying CC levels 
C              to compare with the experimental "quasi-elastic" scattering
C              for actinides. The experimental data are pure elastic below
C              3.5 MeV, but at those energies Sel >> Sinl
C
C              The change is avoided in sensitivity calculations as the 
C              ENDF-6 formatted output ALWAYS contains the unmodified elastic.
C                
               IF( INT(ZEJc(0)).EQ.0 .and. A(0).gt.220 .and. ilv.LT.3
     &         .and. KALman.eq.0  .and. INT(AEJc(0)).GT.0) 
     &            xscclow = xscclow + popread
C              To consider only open channels
               ncoll = i
C 
               POPlv(ilv,nnurec) = POPlv(ilv,nnurec) + popread
               CSDirlev(ilv,nejcec) = CSDirlev(ilv,nejcec) + popread
               CSEmis(nejcec,1) = CSEmis(nejcec,1) + popread
C--------------Add direct transition to the spectrum
               popl = popread*(FLOAT(icsh) - xcse)/DE
               IF (icsl.EQ.1) popl = 2.0*popl
               poph = popread*(xcse - FLOAT(icsl))/DE
               CSE(icsl,nejcec,1) = CSE(icsl,nejcec,1) + popl
               CSE(icsh,nejcec,1) = CSE(icsh,nejcec,1) + poph

               CSEt(icsl,nejcec) = CSEt(icsl,nejcec) + popl
               CSEt(icsh,nejcec) = CSEt(icsh,nejcec) + poph

               IF (ICAlangs.GT.0) THEN
                IF (i.LE.ICAlangs) THEN
                  READ (45,'(A)',END = 1400) ctmp  ! Skipping level identifier line
                  DO iang = 1, NANgela
                    READ (45,'(24x,D12.5)',END = 1400) ftmp
                    CSAlev(iang,ilv,nejcec) = 
     >              CSAlev(iang,ilv,nejcec) + ftmp
                  ENDDO
                ENDIF
               ELSE
                READ (45,'(A)',END = 1400) ctmp    ! Skipping level identifier line
                iang = 0
                DO iang1 = 1, NANgela
                  READ (45,'(3x,12x,D12.5)',END = 1400) ftmp    ! ecis06
C-----------------To use only those values corresponding to EMPIRE grid for inelastic XS
                  if(mod(DBLE(iang1-1)*angstep+gang,gang).NE.0) cycle
                  iang = iang +1
                  CSAlev(iang,ilv,nejcec) = CSAlev(iang,ilv,nejcec)
     &                                    + ftmp
                ENDDO
               ENDIF
C--------------Check whether integral over angles agrees with x-sec. read from ECIS
               dang = PI/FLOAT(NANgela - 1)
               coef = 2*PI*dang
C              csum = 0.d0
C              DO iang = 1, NANgela
C                csum= csum + CSAlev(iang,ilv,nejcec)*SANgler(iang)*coef
C              ENDDO
               csum = 0.d0
               DO iang = 2, NANgela  ! over angles
                 csum = csum + (CSAlev(iang  ,ilv,nejcec) 
     &                       +  CSAlev(iang-1,ilv,nejcec))
     &                       * 0.5d0 * (CAngler(iang)-CANgler(iang-1))
               ENDDO
               csum = 2.0d0*PI*csum 
	         disc_int(ilv) = csum
	         pop_disc(nejcec) = pop_disc(nejcec) + POPlv(ilv,nnurec)
C	         write(*,*) 'Lev=',ilv,' Int=',sngl(csum), 
C    &         sngl(POPlv(ilv,nnurec))
C
               if (csum.gt.0.d0) then
C----------------Correct CSAlev() for eventual imprecision
                 ftmp = POPlv(ilv,nnurec)/csum
                 DO iang = 1, NANgela
                   CSAlev(iang,ilv,nejcec)=CSAlev(iang,ilv,nejcec)*ftmp
                 ENDDO
	         endif
C--------------Construct recoil spectra due to direct transitions
               IF (ENDf(nnurec).GT.0 .AND. RECoil.GT.0) THEN
C-----------------Correct 'coef' for eventual imprecision and include recoil DE
C                 coef = coef*POPlv(ilv,nnurec)/csum/DERec
                  coef = 2*PI*PI/FLOAT(NANgela - 1)/DERec
                  echannel = echannel*EJMass(0)/AMAss(1)
                  DO iang = 1, NDANG
                     erecoil = ecm + echannel + 2*SQRT(ecm*echannel)
     &                         *CANgler(iang)
                     irec = erecoil/DERec + 1.001
                     weight = (erecoil - (irec - 1)*DERec)/DERec
C--------------------Escape if we go beyond recoil spectrum dimension
                     IF (irec + 1.GT.NDEREC) GOTO 1350
                     csmsdl = CSAlev(iang,ilv,nejcec)*SANgler(iang)*coef
                     RECcse(irec,0,nnurec) = RECcse(irec,0,nnurec)
     &                  + csmsdl*(1.d0 - weight)
                     RECcse(irec + 1,0,nnurec)
     &                  = RECcse(irec + 1,0,nnurec) + csmsdl*weight
                  ENDDO
               ENDIF
             ELSE
               READ (46,*,END = 1400) popread ! reading zero for a closed channel
             ENDIF
C          
C          Allowing states in the continuum even for MSD>0
C          ELSEIF(MSD.eq.0)then
           ELSE
C------------Adding inelastic to continuum  (D_Elv(ND_nlv) = elvr)
             echannel = EX(NEX(1),1) - Q(nejcec,1) - D_Elv(i)
             icsl = INT(echannel/DE + 1.0001)
             ncon = min(
     &        NINT((EXCn-Q(nejcec,1)-ECUt(nnurec))/DE + 1.0001),NDEcse)
C            WRITE(8,*) 'nejcec, nnurec',IZAejc(nejcec), IZA(nnurec)
C            WRITE(8,*) 'Level in continuum',D_Elv(i)
C            WRITE(8,*) 'Its bin number',icsl
C            WRITE(8,*) 'E calc',EX(NEX(1),1)-Q(nejcec,1)-(icsl-1)*DE
C            WRITE(8,*) 'Last discr. level',ELV(NLV(nnurec),nnurec)
C            WRITE(8,*) 'Ecut',ECUt(nnurec)
C            WRITE(8,*) 'Ex',EX(NEX(1),1)-Q(nejcec,1)-(ncon-1)*DE
C            WRITE(8,*) 'Continuum starts at bin number',ncon
C------------Avoid reading closed channels
             IF (echannel.GE.0.0001) THEN
               READ (46,*,END = 1400) popread
               popread = popread*FCOred
C--------------This level is not counted as a discrete one
C--------------but it is embedded in the continuum
               CSMsd(nejcec) = CSMsd(nejcec) + popread
               xsinlcont = xsinlcont + popread
C
C              Special treatment for Giant Multipole Resonances
C              Any level with D_Def(i,2)<0 treated as GR
C
C              Giant multipole resonances following TALYS
C
C              For each L multipolarity Energy Weighted Sum Rule (EWSR) applies:
C              SUM_i(E_i*beta_i)=57.5*A**(-5/3)*L*(L+1)
C
               if(D_Def(i,2).LT.0  .and.
     >             INT(Aejc(0)).eq.1 .and. INT(Zejc(0)).eq.0   ) then
                 write(8,
     >             '(/''  Giant Multipole Resonance with J ='',F4.1)') 
     >           D_Xjlv(i)
                 IF(int(D_Xjlv(i)).eq.3) then
                   write(8,'( ''  GOR cross section (cont) ='',
     >               F7.1,'' mb'')') popread
                   write(8,'( ''  GOR energy ='',F6.2,'' MeV'')')
     >               D_Elv(i)
                   write(8,'( ''  GOR width  ='',F6.2,'' MeV'')') ggor           
                   write(8,'( ''  GOR deformation ='',F7.4)') 
     >               -D_Def(i,2)                         
                   ENDIF           
                 IF(int(D_Xjlv(i)).eq.2) then
                   write(8,'( ''  GQR cross section (cont) ='',
     >               F7.1,'' mb'')') popread
                   write(8,'( ''  GQR energy ='',F6.2,'' MeV'')')      
     >               D_Elv(i)
                   write(8,'( ''  GQR width  ='',F6.2,'' MeV'')') ggqr     
                   write(8,'( ''  GQR deformation ='',F7.4)') 
     >               -D_Def(i,2)                         
                   ENDIF           
                 IF(int(D_Xjlv(i)).eq.0) then
                   write(8,'( ''  GMR cross section (cont) ='',
     >               F7.1,'' mb'')') popread
                   write(8,'( ''  GMR energy ='',F6.2,'' MeV'')') 
     >               D_Elv(i)
                   write(8,'( ''  GMR width  ='',F6.2,'' MeV'')') ggmr                 
                   write(8,'( ''  GMR deformation ='',F7.4)')
     >               -D_Def(i,2)
                   ENDIF    
                 write(8,*)                                        
               endif  
C
C--------------Spreading discrete levels in the continuum using a resolution function
C-
               isigma  = isigma0
               if(D_Def(i,2).LT.0  .and.
     &               INT(Aejc(0)).eq.1 .and. INT(Zejc(0)).eq.0   ) then
C
C                ggmr = 3.d0
C                ggqr =85.d0*A(0)**(-2./3.)
C                ggor =5.d0
C
                 if(int(D_Xjlv(i)).eq.0) isigma  = nint(ggmr/DE+0.5)
                 if(int(D_Xjlv(i)).eq.2) isigma  = nint(ggqr/DE+0.5)
                 if(int(D_Xjlv(i)).eq.3) isigma  = nint(ggor/DE+0.5)
               endif
               isigma2 = 2*isigma*isigma
C
               if(isigma.gt.0) then
                 dcor  = 0.d0
                 do ie = max(icsl - 3*isigma,1) ,
     &                   min(icsl + 3*isigma,ncon)
                   dcor = dexp(-dble(ie-icsl)**2/isigma2) + dcor
                 enddo
                 if(dcor.gt.0.d0) then
                   do ie = max(icsl - 3*isigma,1) ,
     &                     min(icsl + 3*isigma,ncon)
                       CSEmsd(ie,nejcec) = CSEmsd(ie,nejcec) +
     &                 popread/DE  *  
     &                 dexp(-dble(ie-icsl)**2/isigma2)/dcor
                   enddo
                 else
                   CSEmsd(icsl  ,nejcec) = CSEmsd(icsl,nejcec)
     &              + popread/DE 
                 endif
               else
                 CSEmsd(icsl,nejcec) = CSEmsd(icsl,nejcec)
     &              + popread/DE
               endif
C            
               IF (ICAlangs.EQ.0) THEN
                 READ (45,*,END = 1400)     ! Skipping level identifier line
                 iang = 0
                 DO iang1 = 1, NANgela
                    READ (45,'(3x,12x,D12.5)',END = 1400) ftmp    ! ecis06
C-------------------Use only those values that correspond to EMPIRE grid for inelastic XS
                    if(mod(DBLE(iang1-1)*angstep+gang,gang).NE.0) cycle
                    iang = iang + 1
                    if(isigma.gt.0 .and. dcor.gt.0.d0) then
                      do ie = max(icsl - 3*isigma,1) ,
     &                        min(icsl + 3*isigma,ncon)
                        CSEa(ie,iang,nejcec,1) =  CSEa(ie,iang,nejcec,1)
     &                  + ftmp * dexp(-dble(ie-icsl)**2/isigma2)/dcor  
     &                  * 2 * pi                               ! added 2pi, BVC 
                      enddo
                    else
                      CSEa(icsl,iang,nejcec,1) =
     &                CSEa(icsl,iang,nejcec,1) + ftmp * 2 * pi ! added 2pi, BVC 
                    endif
                 ENDDO
               ENDIF
             ELSE
               READ (46,*,END = 1400) popread ! reading zero for a closed channel
             ENDIF
C------------End of adding inelastic to continuum
           ENDIF
 1350    ENDDO
      ENDIF
 1400 CLOSE (45)
      IF (DIRect.NE.0) CLOSE (46)

      IF (KTRlom(0,0).GT.0 .AND. FIRst_ein .AND.
     &  DIRect.NE.0 .AND. xsinlcont.NE.0 ) 
     &  WRITE (8,*) ' Some discrete levels are embedded into continuum'

      totcorr = 1.d0
      IF(INT(ZEJc(0)).EQ.0 .AND. TOTcs.GT.0.d0) totcorr = 
     & (ELAcs*ELAred +CSFus+ (SINl + SINlcc)*FCCred + SINlcont*FCOred) /
     &                 (TOTcs*TOTred)

      IF (KTRlom(0,0).GT.0) THEN
      IF (INT(ZEJc(0)).EQ.0 .AND. AEJc(0).GT.0) THEN
         WRITE (8,99785) TOTcs,TOTred*totcorr,TOTred*TOTcs*totcorr,
     &                   CSFus/FUSred,FUSRED,CSFus,
     &                   ELAcs, ELAred, ELAred*ELAcs 
99785    FORMAT (/,2x,'Total cross section         :',e14.7,' mb',
     &                '  ( Scaled by ',f6.3,' to ',e14.7,' mb )',/,2x,
     &           'Absorption cross section    :',e14.7,' mb',
     &                '  ( Scaled by ',f6.3,' to ',e14.7,' mb )',/,2x,
     &           'Shape elastic cross section :',e14.7,' mb',
     &                '  ( Scaled by ',f6.3,' to ',e14.7,' mb )')
         IF(((SINlcc + SINl)*FCCred + SINlcont*FCOred).GT.0) 
     &    WRITE (8,99006) SINlcc + SINl + SINlcont,
     &                  ((SINlcc + SINl)*FCCred + SINlcont*FCOred)/
     &                   (SINlcc + SINl + SINlcont),
     &                   (SINlcc + SINl)*FCCred + SINlcont*FCOred
99006    FORMAT (/,2x,
     &           'Direct cross section        :',e14.7,' mb',
     &                '  ( Scaled by ',f6.3,' to ',e14.7,' mb )')
         WRITE(8,'(/)')
      ENDIF
      IF (ZEJc(0).NE.0 .OR. AEJc(0).EQ.0) THEN
         WRITE (8,99010) CSFus/FUSred,FUSRED,CSFus
99010    FORMAT (/,2x,
     &           'Absorption cross section    :',e14.7,' mb',
     &                '  ( Scaled by ',f6.3,' to ',e14.7,' mb )')
         IF(((SINlcc + SINl)*FCCred + SINlcont*FCOred).GT.0) 
     &    WRITE (8,99006) SINlcc + SINl + SINlcont,
     &                  ((SINlcc + SINl)*FCCred + SINlcont*FCOred)/
     &                   (SINlcc + SINl + SINlcont),
     &                   (SINlcc + SINl)*FCCred + SINlcont*FCOred
         WRITE(8,'(/)')
      ENDIF

      gang = 180.0/(NDAng - 1)
      angstep = 180.0/(NANgela - 1)
      IF (AEJc(0).GT.0) THEN
        WRITE (8,99015)
        WRITE (8,99020)
        DO iang = 1, NANgela/4 + 1
          imint = 4*(iang - 1) + 1
          imaxt = MIN0(4*iang,NANgela)
          WRITE (8,99025) 
     &     ((j - 1)*angstep,ELAred*elada(j),j = imint,imaxt)
        ENDDO
      ENDIF

99015 FORMAT (/' ',46x,'SHAPE ELASTIC DIFFERENTIAL CROSS-SECTION',/,' ',
     &        46x,40('*'),/,' ',50x,'CENTER-OF-MASS SYSTEM',/)
99016 FORMAT (/' ',46x,'COMP. ELASTIC DIFFERENTIAL CROSS-SECTION',/,' ',
     &        46x,40('*'),/,' ',50x,'CENTER-OF-MASS SYSTEM',/)
99020 FORMAT (' ',5x,4('    TETA ',2x,'D.SIGMA/D.OMEGA',6x),/)
99025 FORMAT (' ',5x,4(1p,e12.5,2x,e12.5,6x))
99028 FORMAT ( ' ',46x,'DIRECT INEL. DIFFERENTIAL CROSS-SECTION',/,
     &              ' ',46x,' (only discrete levels are listed)',/,' '
     &                 ,46x,36('*'),/,' ',50x,'CENTER-OF-MASS SYSTEM',
     &      /)
99029 FORMAT (/' ',46x,'INELASTIC DIFFERENTIAL CROSS-SECTION',/,
     &              ' ',46x,'    (including compound + direct)',/,' '
     &              ' ',46x,' (only discrete levels are listed)',/,' '
     &                 ,46x,36('*'),/,' ',50x,'CENTER-OF-MASS SYSTEM',
     &      /)
99030 FORMAT ('  Angle ',10(6x,i2,'-level'))
99031 FORMAT ('        ',9(5x,'E=',f7.4))
99032 FORMAT ('        ',10(5x,'E=',f7.4))
99033 FORMAT ('        ',9(4x,f4.1,'/',f5.4))
99034 FORMAT ('        ',10(4x,f4.1,'/',f5.4))
99035 FORMAT (1x,f5.1,3x,11(2x,E12.6))
99040 FORMAT (' DIR.INEL',I1,1x,11(E12.6,2x))
99041 FORMAT (' TOT.INEL',I1,1x,11(E12.6,2x))
      WRITE (8,'(//)')
      IF (ncoll.GT.0) THEN
C--------Locate position of the projectile among ejectiles
         CALL WHEREJC(IZAejc(0),nejcec,iloc)
C
         WRITE (8,*) ' '
         gang = 180.d0/(NDANG - 1)
         its = ncoll
         IF (CSAlev(1,ICOller(2),nejcec).GT.0) THEN
           WRITE(8,99028)
           WRITE(8,99030) (ICOller(ilv),ilv = 2,MIN(its,10))
           WRITE(8,99031) (ELV(ICOller(ilv),nnurec),ilv = 2,MIN(its,10))
           WRITE(8,99033) (XJLv(ICOller(ilv),nnurec)*
     &        LVP(ICOller(ilv),nnurec),D_DEF(ilv,2),ilv = 2,MIN(its,10))
           WRITE(8,*) ' '
           DO iang = 1, NDANG
             WRITE (8,99035) (iang - 1)*gang,
     &            (CSAlev(iang,ICOller(ilv),nejcec),ilv = 2,MIN(its,10))
           ENDDO
           WRITE(8,*) ' '
           WRITE(8,99040)1,(POPlv(ICOller(ilv),nnurec),
     &                            ilv= 2,MIN(its,10))
C
           IF(its.gt.10) THEN
             WRITE(8,*) ' '
             WRITE(8,*) ' '
             WRITE(8,99030)(ICOller(ilv),ilv = 11,MIN(its,20))
             WRITE(8,99032)(ELV(ICOller(ilv),nnurec),ilv=11,MIN(its,20))
             WRITE(8,99034)(XJLv(ICOller(ilv),nnurec)*
     &         LVP(ICOller(ilv),nnurec),D_DEF(ilv,2),ilv=11,MIN(its,20))
             WRITE (8,*) ' '
             DO iang = 1, NDANG
               WRITE (8,99035) (iang - 1)*gang,
     &           (CSAlev(iang,ICOller(ilv),nejcec),ilv = 11,MIN(its,20))
             ENDDO
             WRITE (8,*) ' '
             WRITE (8,99040) 2,(POPlv(ICOller(ilv),nnurec),ilv = 11,
     &                        MIN(its,20))
           ENDIF

           IF(its.gt.20) THEN
             WRITE(8,*) ' '
             WRITE(8,*) ' '
             WRITE(8,99030) (ICOller(ilv),ilv = 21,MIN(its,30))
             WRITE(8,99032)(ELV(ICOller(ilv),nnurec),ilv=21,MIN(its,30))
             WRITE(8,99034)(XJLv(ICOller(ilv),nnurec)*
     &         LVP(ICOller(ilv),nnurec),D_DEF(ilv,2),ilv=21,MIN(its,30))
             WRITE (8,*) ' '
             DO iang = 1, NDANG
               WRITE (8,99035) (iang - 1)*gang,
     &           (CSAlev(iang,ICOller(ilv),nejcec),ilv = 21,MIN(its,30))
             ENDDO
             WRITE (8,*) ' '
             WRITE (8,99040) 3,(POPlv(ICOller(ilv),nnurec),ilv = 21,
     &                          MIN(its,30))
           ENDIF
C
C----------Because of the ENDF format restrictions the maximum
C----------number of discrete levels is limited to 40
C
           IF(its.gt.30) THEN
             WRITE(8,*) ' '
             WRITE(8,*) ' '
             WRITE(8,99030)(ICOller(ilv),ilv = 31,MIN(its,40))
             WRITE(8,99032)(ELV(ICOller(ilv),nnurec),ilv=31,MIN(its,40))
             WRITE(8,99034)(XJLv(ICOller(ilv),nnurec)*
     &         LVP(ICOller(ilv),nnurec),D_DEF(ilv,2),ilv=31,MIN(its,40))
             WRITE (8,*) ' '
             DO iang = 1, NDANG
               WRITE (8,99035) (iang - 1)*gang,
     &           (CSAlev(iang,ICOller(ilv),nejcec),ilv = 31,MIN(its,40))
             ENDDO
             WRITE (8,*) ' '
             WRITE (8,99040) 4,(POPlv(ICOller(ilv),nnurec),ilv = 31,
     &                      MIN(its,40))
           ENDIF
           WRITE (8,*) '++++++'
         ENDIF
      ENDIF

      ENDIF
C
C-----calculate transmission coefficients in outgoing channels
C
      DO nnuc = 1, NNUcd
         DO nejc = 1, NEJcm
            ares = A(nnuc) - AEJc(nejc)
            zres = Z(nnuc) - ZEJc(nejc)
C           residual nuclei must be heavier than alpha
            if(ares.le.4 . or. zres.le.2) cycle

            izares = INT(1000*zres + ares)
            CALL WHERE(izares,nnur,iloc)
            IF (iloc.EQ.1) cycle

            netl = 6
            IF (NEX(nnuc).GT.0) netl =
     &         INT((EX(NEX(nnuc),nnuc) - Q(nejc,nnuc))/DE) + 6
          
            IF (netl.GT.NDETL) cycle
            
            ICAlangs = ICAlangs-10
            itmp = NANgela
            NANgela = 2
            CALL TLEVAL(nejc,nnur,nonzero)
            ICAlangs = ICAlangs+10
            NANgela = itmp
C-----------print transmission coefficients
            IF (nonzero .AND. IOUt.EQ.5) THEN
              WRITE (8,*)
              WRITE (8,*) ' Transmission coefficients for '
              WRITE (8,'(1x,A15,I3,A3,I3,A3,F4.1)')
     &                    ' Projectile: A=', INT(AEJc(nejc)), ' Z=',
     &                   INT(ZEJc(nejc)), ' S=', SEJc(nejc)
              WRITE (8,'(1x,A11,I3,A3,I3,A3,F4.1,A3,I2)')
     &                    ' TARGET: A=', INT(A(nnur)), ' Z=',
     &                   INT(Z(nnur)), ' S=', SNGL(XJLv(1,nnur)),
     &                   ' P=', INT(LVP(1,nnur))
              DO i = 1, netl
                IF (TL(i,1,nejc,nnur).GT.0.0) WRITE (8,99011)
     &             ETL(i,nejc,nnur), (TL(i,j,nejc,nnur),j = 1,12)
              ENDDO
              WRITE (8,'(1X,/)')
            ENDIF
         ENDDO     !over ejectiles (nejc)
      ENDDO     !over nuclei (nnuc)
C
C-----determination of transmission coeff.--done
99011 FORMAT (1X,14(1P,E10.4,1x))
C
C     Skipping all emission calculations
C     GOTO 99999
C
C-----Locate positions of ENDF MT-numbers 2, 91, 649, and 849
      CALL WHERE(IZA(1) - IZAejc(0),mt2,iloc)
      CALL WHERE(IZA(1) - IZAejc(1),mt91,iloc)
      CALL WHERE(IZA(1) - IZAejc(2),mt649,iloc)
      CALL WHERE(IZA(1) - IZAejc(3),mt849,iloc)
C-----Locate residual nuclei after CN decay
      NREs(0) = 1
      DO nejc = 1, NEJcm
         NREs(nejc) = -1
         ares = A(1) - AEJc(nejc)
         zres = Z(1) - ZEJc(nejc)
C        residual nuclei must be heavier than alpha
         if(ares.le.4. and. zres.le.2.) cycle
         izares = INT(1000.0*zres + ares)
         CALL WHERE(izares,nnur,iloc)
         NREs(nejc) = nnur
      ENDDO
C-----
C-----Calculate MSD contribution
C-----
      corrmsd = 1.0
      xsinl   = 0.d0
      IF (MSD.NE.0 .AND. EINl.GE.EMInmsd) THEN
C
C--------call ORION
C
C--------The INQUIRE statement determines if stored file exists.
C--------If it does not, the program start new calculations
         WRITE (ctmp23,'(i3.3,i3.3,1h_,i3.3,i3.3,1h_,i9.9)')
     &       INT(ZEJc(0)), INT(AEJc(0)), INT(Z(0)),
     &       INT(A(0)), INT(EINl*1000000)
         INQUIRE (FILE = (ctldir//ctmp23//'.MSD'),EXIST = fexist)
         IF (.NOT.fexist) THEN
           OPEN (15,FILE = (ctldir//ctmp23//'.MSD'),STATUS='NEW')
         ELSE
           OPEN (15,FILE = (ctldir//ctmp23//'.MSD'),STATUS='OLD')
           WRITE (8,*) ' '
           WRITE (8,*)
     &       ' Using precalculated ORION results for E=',EINl,' MeV'
           WRITE (8,*) ' '
           GOTO 1450
         ENDIF
         WRITE (8,*) ' '
         qmax = 0.99*EIN
         qstep = qmax/3.0
C        Proposed by H. Wienke
         ltrmax = 6
         IF (NLW.LE.15) ltrmax = 5
         IF (NLW.LE.13) ltrmax = 4
         IF (NLW.LE.10) ltrmax = 3
         IF (NLW.LE.8)  ltrmax = 2
         IF (NLW.LE.6)  ltrmax = 1
         WRITE(15,*) qmax,qstep,ltrmax
         q2 = qmax
         q3 = qmax
 1420    CALL ORION(q2,q3,1,EIN,NLW,1,ltrmax,A(0),Z(0),AEJc(0),
     &              ZEJc(0),IOUt,ANGles,NDANG,ICOmpff)
         WRITE (8,
     &'('' ORION calculated for Q2='', F7.3, '' and Q3='',F7.3)') q2, q3
         q2 = q2 - qstep
         IF (q2.LT.( - 0.0001D0)) THEN
            q3 = q3 - qstep
            IF (q3.LT.( - 0.0001D0)) GOTO 1450
            q2 = q3
         ENDIF
C--------Set to Q's to 0 if negative due to rounding error
         IF (q2.LT.0.0D0) q2 = 0.0
         IF (q3.LT.0.0D0) q3 = 0.0
         GOTO 1420
 1450    REWIND (15)
         READ(15,*) qmax,qstep,ltrmax
         WRITE (8,*) ' '
         WRITE (8,*) ' '
         CALL TRISTAN(0,0,ltrmax,qmax,qstep,xsinl)
         CLOSE(15)
      ENDIF

C-----PCROSS exciton model calculations of preequilibrium contribution
C-----including cluster emission by Iwamoto-Harada model and angular
C-----distributions according to Kalbach systematics
C-----
C-----Kalbach parameterizations of direct reactions
C-----   for complex projectiles also used
C-----
      crossBU = 0.d0
      crossBUt= 0.d0
      crossNT = 0.d0
      crossNTt= 0.d0
      crossPE = 0.d0
      crossPEt= 0.d0
      totemis = 0.d0
      IF (EINl.GT.0.1D0 .AND. PEQc.GT.0) THEN
         ftmp = CSFus
         CALL PCROSS(ftmp,totemis)
         IF(ltransfer)WRITE(112,'(1P,E11.4,1x,1P,7E13.5)')EINl,crossNTt,
     &     (crossNT(i),i=1,NDEJC)
         IF(lbreakup) WRITE(113,'(1P,E11.4,1x,1P,7E13.5)')EINl,crossBUt,
     &     (crossBU(i),i=1,NDEJC)
      ENDIF ! PCRoss done
                                                                     ! To include inel for (g,x)
      IF ((xsinl+totemis+(SINl+SINlcc)*FCCRED+SINlcont*FCOred).gt.0. !.AND. NPRoject.gt.0 
     &    .AND. NREs(NPRoject).GE.0 ) THEN
C--------Print inelastic PE double differential cross sections
         nejc = NPRoject
         nnur = NREs(nejc)
         IF (CSMsd(nejc).GT.0.D0 .AND. IOUt.GE.5) THEN
            itimes = FLOAT(NDANG)/11.0 + 0.95
            DO its = 1, itimes
              iad = 1 + (its - 1)*11
              iam = 11 + (its - 1)*11
              iam = MIN0(NDANG,iam)
              IF(nejc.eq.1) WRITE (8,
     &                '(//30X,''     N  E  U  T  R  O  N  S ''/)')
              IF(nejc.eq.2) WRITE (8,
     &                '(//30X,''     P  R  O  T  O  N  S ''/)')
              IF(nejc.eq.3) WRITE (8,
     &                '(//30X,''     A  L  P  H  A  S ''/)')
              IF(nejc.gt.3) cycle 
              WRITE (8,
     &                '(30X,''A      n      g      l      e      s '')')
              WRITE (8,*) ' '
              WRITE (8,'('' Energy  '',11(4X,F5.1,2X))')
     &                (ANGles(ia),ia = iad,iam)
              WRITE (8,*) ' '
C-------------Maximum and minimum energy bin
              echannel = EX(NEX(1),1) - Q(nejc,1)
C
C             Following changes in PCROSS to cover discrete levels , Jan 2011
              DO i = 1, MAX(INT(echannel/DE + 1.0001),1)
                WRITE (8,'(1X,F7.3,1X,11E11.4)') FLOAT(i - 1)*DE,
     &           (max(CSEa(i,iang,nejc,1),0.d0),iang = iad,iam)
              ENDDO
              WRITE (8,*) ' '
            ENDDO
         ENDIF
C        if(xsinlcont.gt.0 .and. FCOred.GT.0) then
C           write(8,*)
C    &     ' DWBA to continuum XS for inelastic channel ',xsinlcont
C           SINlcont =  xsinlcont/FCOred
C        else
C           SINlcont =  0.d0
C        endif
C        WRITE (8,*)
C
         corrmsd = (CSFus - (xsinl + totemis))/CSFus
         IF (corrmsd.LT.0.0D0) THEN
            write(8,*) ' CSFus=',sngl(CSFus),
     &        ' xsinl   (MSD)   =',sngl(xsinl),
     &        ' totemis (PCROSS)=',sngl(totemis)
C    &        ' xsinlcont (ECIS)=',sngl(SINlcont*FCOred)
C             SINLcont = 0.d0
            totemis = CSFus - xsinl
            corrmsd = 0.d0
            if(xsinl.lt.0.0001d0) then
              xsinl = 0.d0
              totemis =  CSFus
              corrmsd = 0.d0
              write(8,*) ' Changed to : xsinl = ', xsinl,
     &                   ' PCROSS=',totemis
            endif

            WRITE (8,*) ' '
            WRITE (8,*) ' ERROR: PE emission larger than fusion xsc'
            WRITE (8,*) ' ERROR: see bottom of the .lst for details'
            IF (MSD+MSC.GT.0 .AND. ICOmpff.GT.0) THEN
              WRITE (8,*) 'TRY TO TURN OFF COMPRESSIONAL FORM FACTOR '
              WRITE (8,*) 'SETTING COMPFF TO 0 IN THE OPTIONAL INPUT.'
              STOP 'ERROR: PE EMISSION LARGER THAN FUSION CROSS SECTION'
            ENDIF
            IF (MSD+MSC.GT.0 .AND. ICOmpff.EQ.0) THEN
              WRITE (8,*) 'THIS MAY HAPPEN IF RESPONSE FUNCTIONS ARE '
              WRITE (8,*) 'RENORMALIZED IN INPUT OR FITTED TO WRONG '
              WRITE (8,*) 'DISCRETE LEVELS. CHECK `EFIT` AND `RESNOR` '
              WRITE (8,*) 'IN OPTIONAL INPUT.    '
              WRITE (8,*) 'IF COLLECTIVE LEVELS ARE CHOSEN INTERNALLY '
              WRITE (8,*) 'IT MAY HAPPEN THAT THE FIRST 2+ LEVEL IS   '
              WRITE (8,*) 'NOT THE COLLECTIVE ONE. IN SUCH A CASE THE '
              WRITE (8,*) 'PROPER ENERGY OF THE COLLECTIVE LEVEL      '
              WRITE (8,*) 'SHOULD BE ENTERED IN OPTIONAL INPUT THROUGH'
              WRITE (8,*) 'THE EFIT KEYWORD.                          '
              WRITE (8,*)
     &                    'IF THESE ARE FINE TRY ANOTHER OPTICAL MODEL.'
              STOP 'ERROR: PE EMISSION LARGER THAN FUSION CROSS SECTION'
            ENDIF
            IF (MSD+MSC.EQ.0) THEN
               WRITE (8,*) 'THIS MAY HAPPEN IF TOO MANY DISCRETE LEVELS'
               WRITE (8,*) 'ARE EMBEDDED INTO CONTINUUM OR HAVE TOO BIG'
               WRITE (8,*) 'DYNAMICAL DEFORMATIONS SPECIFIED IN THE    '
               WRITE (8,*) 'COLLECTIVE LEVEL FILE.'
               WRITE (8,*)
     &                  'TRY TO REDUCE THE NUMBER OF COLLECTIVE LEVELS.'
              STOP 'ERROR: PE EMISSION LARGER THAN FUSION CROSS SECTION'
            ENDIF
         ENDIF
         ftmp = 0.d0
         DO i = 1, NLW
            POP(NEX(1),i,1,1) = POP(NEX(1),i,1,1)*corrmsd
            POP(NEX(1),i,2,1) = POP(NEX(1),i,2,1)*corrmsd
            ftmp = ftmp + POP(NEX(1),i,1,1) + POP(NEX(1),i,2,1)
         ENDDO
         WRITE (8,*) ' '
C--------TRISTAN *** done ***
C--------Add MSD contribution to the residual nucleus population
C--------Locate residual nucleus after MSD emission
         DO i = 0, NDEjc
           nnur = NREs(i)
           IF(nnur.LT.0) CYCLE
           IF (CSMsd(i).LE.0.0D0) cycle

           CALL ACCUMSD(1,nnur,i)
C----------Add PE contribution to energy spectra (angle int.)
C          ftmp = 0.d0
           DO ie = 1, NDEcse
              CSE (ie,i,1) = CSE (ie,i,1) + CSEmsd(ie,i)
              CSEt(ie,i  ) = CSEt(ie,i  ) + CSEmsd(ie,i)      
C             ftmp = ftmp + DE*CSEmsd(ie,i)
           ENDDO
C----------Add PE contribution to the total NEJC emission
           CSEmis(i,1) = CSEmis(i,1) + CSMsd(i)
         ENDDO
C
      ENDIF

      IF (lbreakup .and. crossBUt.gt.0) then 
C--------Add breakup spectra and XS (stil to add DDXS)
         DO i = 1, NDEjc
           IF (crossBU(i).LE.0.0D0) cycle
           DO ie = 1, NDEX
              CSE (ie,i,1) = CSE (ie,i,1) + specBU(i,ie)
              CSEt(ie,i  ) = CSEt(ie,i  ) + specBU(i,ie)      
           ENDDO
C----------Add PE contribution to the total NEJC emission
           CSEmis(i,1) = CSEmis(i,1) + crossBU(i)
         ENDDO
      ENDIF
C
      tothms = 0.d0
C-----
C-----HMS Monte Carlo preequilibrium emission
C-----        
      IF ( EINl.GT.0.1D0 .AND. LHMs.NE.0 .AND. MSD+MSC.EQ.0) THEN
         xizat = IZA(0)
         CALL DDHMS(IZAejc(0),xizat,XJLv(LEVtarg,0),EINl,
     &             CSFus*corrmsd,CHMs,DE,DERec,FHMs,NHMs,QDFrac,
     &                 0,1,0,icalled)
         icalled = 1
c        CSEmis(1,1) = CSEmis(1,1) + CSHms(1,0)
c        CSEmis(2,1) = CSEmis(2,1) + CSHms(2,0)
         WRITE (8,
     &        '('' HMS inclusive neut. emission ='',G12.5,
     &        ''mb'')') CSHms(1,0)
         WRITE (8,
     &        '('' HMS inclusive prot. emission ='',G12.5,
     &          ''mb'')') CSHms(2,0)
         tothms = CSHms(1,1) + CSHms(2,1) 
      ENDIF
C-----
C-----PE + DWBA cont. *** done ***
C-----
      ia = INT(A(1))

C--------
C--------Heidelberg Multistep Compound calculations
C--------
      xsmsc = 0.d0 
      IF (MSC.NE.0) THEN
         CALL HMSC(nvwful)
         CSEmis(0,1) = CSEmis(0,1) + CSMsc(0)                  
         CSEmis(1,1) = CSEmis(1,1) + CSMsc(1)
         CSEmis(2,1) = CSEmis(2,1) + CSMsc(2)
         xsmsc = xsmsc + CSMsc(0) + CSMsc(1) + CSMsc(2)
C        if(nvwful) goto 1500
C        WRITE(8,*) 'MSC: ',CSMsc(0),CSMsc(1),CSMsc(2)
      ENDIF

      IF (IOUt.GT.0) THEN
         WRITE (8,*) ' '
         WRITE (8,*) '*** Summary of PE and direct emission  '
         ftmp = CSFus + (SINl + SINlcc)*FCCred + SINlcont*FCOred
     &        + crossBUt + crossNTt
         IF (DIRect.EQ.0) THEN
            WRITE (8,'(2x,A32,F9.2,A3,'' including'')') 
     &      'Non-elastic cross section      ',
     &      sngl(CSFus),' mb'
            WRITE (8,*) ' '
         ELSEIF (DIRect.EQ.1 .OR. DIRect.EQ.2) THEN
            WRITE (8,'(2x,A32,F9.2,A3,'' including'')') 
     &       'Non-elastic cross section      ', ftmp,' mb'
            WRITE (8,*) ' '
            WRITE (8,'(2x,A32,F9.2,A3,1x,1h(,F7.2,A2,1h))') 
     &       'CC inelastic to discrete levels',
     &        SINlcc*FCCred,' mb', SINlcc*FCCred/ftmp*100,' %'

            WRITE (8,'(2x,A32,F9.2,A3,1x,1h(,F7.2,A2,1h))') 
     &       'DWBA inel to discrete levels   ',
     &        SINl*FCCred,' mb', SINl*FCCred/ftmp*100,' %'
            WRITE (8,'(2x,A32,F9.2,A3,1x,1h(,F7.2,A2,1h))') 
     &       'DWBA to continuum              ',
     &        SINlcont*FCOred,' mb', SINlcont*FCOred/ftmp*100,' %'
            if(lbreakup) WRITE (8,'(2x,A32,F9.2,A3,1x,1h(,F7.2,A2,1h))') 
     &       'Break-up                       ',
     &        crossBUt,' mb', crossBUt/ftmp*100,' %'
         ELSEIF (DIRect.EQ.3) THEN
            WRITE (8,'(2x,A32,F9.2,A3,'' including'')') 
     &       'Non-elastic cross section      ', ftmp,' mb'
            WRITE (8,*) ' '
            WRITE (8,'(2x,A32,F9.2,A3,1x,1h(,F7.2,A2,1h))') 
     &       'DWBA inel to discrete levels   ',
     &        SINl*FCCred,' mb', SINl*FCCred/ftmp*100,' %'
            WRITE (8,'(2x,A32,F9.2,A3,1x,1h(,F7.2,A2,1h))') 
     &       'DWBA to continuum              ',
     &        SINlcont*FCOred,' mb', SINlcont*FCOred/ftmp*100,' %'
            if(lbreakup) WRITE (8,'(2x,A32,F9.2,A3,1x,1h(,F7.2,A2,1h))') 
     &       'Break-up                       ',
     &       crossBUt,' mb', crossBUt/ftmp*100,' %'
         ENDIF
         dtmp = (SINl + SINlcc)*FCCred + SINlcont*FCOred 
     >        + crossBUt + crossNTt
         WRITE (8,'(2x,A32,F9.2,A3,1x,1h(,F7.2,A2,1h))') 
     &     '(Total direct)                 ',
     &     dtmp,' mb',dtmp/ftmp*100,' %'
         WRITE (8,'(2x,A32,F9.2,A3,1x,1h(,F7.2,A2,1h))') 
     &     'MSD contribution               ',
     &      xsinl,' mb', xsinl/ftmp*100,' %'
         WRITE (8,'(2x,A32,F9.2,A3,1x,1h(,F7.2,A2,1h))') 
     &     'MSC contribution               ',
     &      xsmsc,' mb', xsmsc/ftmp*100,' %'
         WRITE (8,'(2x,A32,F9.2,A3,1x,1h(,F7.2,A2,1h))') 
     &     'PCROSS contribution            ',
     &      totemis,' mb', totemis/ftmp*100,' %'
         WRITE (8,'(2x,A32,F9.2,A3,1x,1h(,F7.2,A2,1h))') 
     &     'HMS contribution               ',
     &      tothms,' mb',tothms/ftmp*100,' %'
         if(lbreakup) 
     &     WRITE (8,'(2x,A32,F9.2,A3,1x,1h(,F7.2,A2,1h))') 
     &      'Break-up contribution          ',
     &       crossBUt,' mb', crossBUt/ftmp*100,' %'
         if(ltransfer) 
     &     WRITE (8,'(2x,A32,F9.2,A3,1x,1h(,F7.2,A2,1h))') 
     &      'Transfer contribution          ',
     &       crossNTt,' mb', crossNTt/ftmp*100,' %'

         dtmp = tothms + totemis + xsmsc + xsinl + crossBUt + crossNTt

         WRITE (8,'(2x,A32,F9.2,A3,1x,1h(,F7.2,A2,1h))') 
     &     '(Total pre-equilibrium)        ',
     &     dtmp,' mb', dtmp/ftmp*100,' %'

         WRITE (8,'(2x,A44)')
     &     '----------------------------------------------'
         WRITE (8,'(2x,A32,F9.2,A3)') 
     &     'CN formation cross section     ',
     &      CSFus*corrmsd - tothms - xsmsc,' mb'
         WRITE (8,*) ' '
      ENDIF

      IF (IOUt.GE.3 
     &    .AND. (CSEmis(0,1) + CSEmis(1,1) + CSEmis(2,1)
     &                       + CSEmis(3,1) + CSEmis(4,1)
     &                       + CSEmis(5,1) + CSEmis(6,1)) .NE. 0
     &    ) THEN
          WRITE (8,*) ' *******************************************'
          WRITE (8,*) ' *******************************************'
          WRITE (8,*)
     &                ' Preequilibrium + Direct spectra (tot)'
          IF(CSEmis(0,1).GT.0) CALL AUERST(1,0,0)
          IF(CSEmis(1,1).GT.0) CALL AUERST(1,1,2)
          IF(CSEmis(2,1).GT.0) CALL AUERST(1,2,0)
          IF(CSEmis(3,1).GT.0) CALL AUERST(1,3,0)
          IF(CSEmis(4,1).GT.0) CALL AUERST(1,4,0)
          IF(CSEmis(5,1).GT.0) CALL AUERST(1,5,0)
          IF(CSEmis(6,1).GT.0) CALL AUERST(1,6,0)
          WRITE (8,*)
     &                ' End of Preequilibrium + Direct spectra (tot)'
          WRITE (8,*) ' ********************************************'
          WRITE (8,*) ' ********************************************'
          WRITE (8,*) 
      ENDIF

      IF (IOUt.GT.1) WRITE (8,*)
     &   '*** Summary of Hauser-Feshbach equilibrium decay'

      IF (nnuc.eq.1) THEN
          WRITE (8,*) 
          WRITE (8,*) ' -------------------------------------'
          WRITE (8,'(I3,2X,''Decaying nucleus '',I3,''-'',A2)') nnuc,
     &             ia, SYMb(nnuc)
          WRITE (8,*) ' -------------------------------------'
          WRITE (8,*) 
      ENDIF
      IF (IOUt.GT.1) THEN
         WRITE (8,*)
         WRITE (8,'(''  Compound nucleus '',I3,''-'',A2,
     &  '' spin distribution'')') ia, SYMb(1)
         WRITE (8,*) ' -----------------------------------------'
         WRITE (8,*) ' '
         DO i = 1, NLW
            IF (MOD(ia,2).EQ.0) THEN
               WRITE (8,'(1X,I5,G12.5,5X,I5,G12.5)') i - 1,
     &                POP(NEX(1),i,1,1), ( - (i - 1)), POP(NEX(1),i,2,1)
            ELSE
               WRITE (8,'(1X,I4,''/2'',G12.5,5X,I4,''/2'',G12.5)')
     &                2*i - 1, POP(NEX(1),i,1,1), ( - (2*i - 1)),
     &                POP(NEX(1),i,2,1)
            ENDIF
         ENDDO
         WRITE (8,*) ' '
      ENDIF
C
      WRITE (12,*) ' '
      WRITE (12,'('' FUSION CROSS SECTION = '',1P,E12.5,'' mb'')')
     &     CSFus + (SINl + SINlcc)*FCCred + SINlcont*FCOred
      WRITE (12,'('' TOTAL  CROSS SECTION = '',1P,E12.5,'' mb'')')
     &     TOTcs*TOTred*totcorr
      WRITE (12,*) ' '
C
      POPmax(1) = CSFus*1.0E-25
      nubart=0
      OPEN (80,FILE = 'FISSION.OUT',STATUS = 'UNKNOWN')
C
C-----Start DO loop over decaying nuclei
      DO nnuc = 1, NNUcd

         IF(QPRod(nnuc).LT.-999.d0) CYCLE
         ROFisp = 0.d0  ! setting saddle point LD to zero (again as protection)

C        if(nnuc.le.NNUcd)
         if(nnuc.le.NDEJC)  ! limiting screen printout 
     &     WRITE (*,'(''  Decaying nucleus # '',I3,4H of  ,I3,
     &      2H ( ,I3,1H-,A2,1H-,I3,1H) )') 
     &       nnuc,  NNUcd, INT(Z(nnuc)), SYMb(nnuc), INT(A(nnuc))
C
         IF (FISsil(nnuc) .AND. NINT(FISshi(nnuc)).NE.1 ) THEN 
            CALL READ_INPFIS(nnuc)
            IF (NINT(FISmod(nnuc)).EQ.0)THEN   ! Single mode fission 
               DO i = 1, NRHump
                  IF(FISden(Nnuc).EQ.0)then
                     CALL DAMI_ROFIS(nnuc,i,0,AFIs(i))
                  ELSEIF(FISden(Nnuc).EQ.3) then
                     CALL DAMI_RO_HFB_FIS(nnuc,i,AFIs(i))
                  ELSE
                    WRITE(8,'(''  ERROR: CHECK FISDEN (not 0 or 3)!'')')
                    STOP ' FATAL: CHECK FISDEN (not 0 or 3)!'
                  ENDIF
               ENDDO
            ELSE                             ! Multimodal (FISmod(nnuc)>0)
               IF(NINT(FISden(Nnuc)).EQ.0) then
                 CALL DAMI_ROFIS(nnuc,1,0,AFIs(1))
                 DO m=1,INT(FISmod(Nnuc))+1
                   CALL DAMI_ROFIS(nnuc,2,m,AFIsm(m))
                 ENDDO
               ELSE 
                 WRITE(8,'(''  ERROR: FISmod>0 and FISDEN not 0 ! '')')
                 STOP ' FATAL: FISmod>0 and FISDEN not 0 ! '
C                WRITE(8,'('' WARNING: FISmod>0 and FISDEN not 0 !'')')
C                WRITE(8,'('' WARNING: Resetting FISDEN to 0 '')')
C                WRITE(8,
C    &    '('' WARNING: Only EGSM is allowed for multimodal fission'')')
C                 FISden(Nnuc)=0
               ENDIF
            ENDIF
            IF (NRBar.EQ.3.AND.NRWel.EQ.1.AND.NINT(FISmod(Nnuc)).EQ.0)
     &          THEN
               TFIso = 2.86896*EXP(2.*PI*(EFB(2) - (EFB(3)+H(1,3)/2.))
     &                 /H(1,2))/(H(1,3)*10.**21)
               TGIso = EXP(2.*PI*(EFB(1) - (EFB(3)+H(1,3)/2.))/H(1,1))
     &                 /10.**14
               TISo = TFIso*TGIso/(TFIso + TGIso)
               RFIso = TGIso/(TFIso + TGIso)
            ENDIF
            CALL WRITE_OUTFIS(nnuc)
         ENDIF
         ia = INT(A(nnuc))
C--------Reset variables for life-time calculations
         stauc = 0.d0                         
         sgamc = 0.d0
         csemist = 0.d0
         CSFis = 0.d0
         IF (NINT(FISmod(Nnuc)).GT.0) THEN
            DO m = 1, INT(FISmod(nnuc)) + 1
               CSFism(m) = 0.d0
            ENDDO
         ENDIF
         sumfis = 0.d0
         IF (nnuc.eq.1) THEN
            WRITE (8,*)
            WRITE (8,*)
     &' ---------------------------------------------------------------'
            IF(abs(QPRod(nnuc) + ELV(LEVtarg,0)).gt.99.99) THEN
              WRITE (8,
     &'(''  Decaying nucleus '',I3,''-'',A2,''-'',I3,     ''  mass='',F1
     &0.6,'' Q-value='',F10.5)') INT(Z(nnuc)), SYMb(nnuc), ia,
     &         AMAss(nnuc), QPRod(nnuc) + ELV(LEVtarg,0)
            ELSE
              WRITE (8,
     &'(''  Decaying nucleus '',I3,''-'',A2,''-'',I3,     ''  mass='',F1
     &0.6,'' Q-value='',F10.6)') INT(Z(nnuc)), SYMb(nnuc), ia,
     &         AMAss(nnuc), QPRod(nnuc) + ELV(LEVtarg,0)
            ENDIF
            WRITE (8,*)
     &' ---------------------------------------------------------------'
            WRITE (8,*)

            WRITE (12,*)
     &' ---------------------------------------------------------------'
            IF(abs(QPRod(nnuc) + ELV(LEVtarg,0)).gt.99.99) THEN
              WRITE (12,
     &'(''  Decaying nucleus '',I3,''-'',A2,''-'',I3,     ''  mass='',F1
     &0.6,'' Q-value='',F10.5)') INT(Z(nnuc)), SYMb(nnuc), ia,
     &         AMAss(nnuc), QPRod(nnuc) + ELV(LEVtarg,0)
            ELSE
              WRITE (12,
     &'(''  Decaying nucleus '',I3,''-'',A2,''-'',I3,     ''  mass='',F1
     &0.6,'' Q-value='',F10.6)') INT(Z(nnuc)), SYMb(nnuc), ia,
     &         AMAss(nnuc), QPRod(nnuc) + ELV(LEVtarg,0)
            ENDIF
            WRITE (12,*)
     &' ---------------------------------------------------------------'
            WRITE (12,
     &'(1X,/,10X,''Discrete level population before gamma cascade'')')
            WRITE (12,'(1X,/,10X,40(1H-),/)')
         ENDIF

         IF (FITomp.LE.0) THEN
            IF (nnuc.NE.1) THEN
               IF (nnuc.EQ.mt91) THEN
                  nejc = 1
               ELSEIF (nnuc.EQ.mt649) THEN
                  nejc = 2
               ELSEIF (nnuc.EQ.mt849) THEN
                  nejc = 3
               ELSE
                  GOTO 1460
               ENDIF
               dtmp = 0.d0
               DO il = 1, NLV(nnuc)
                 dtmp = dtmp + CSDirlev(il,nejc)
               ENDDO
               IF(dtmp.LE.0.0 .and. POPlv(1,nnuc).eq.0.d0) GOTO 1460
               WRITE (12,*)
               WRITE (12,*)
     &' ---------------------------------------------------------------'
               WRITE (8,*)
               WRITE (8,*)
     &' ---------------------------------------------------------------'
               IF(abs(QPRod(nnuc) + ELV(LEVtarg,0)).gt.99.99) THEN
                 WRITE (8,
     &'(''  Decaying nucleus '',I3,''-'',A2,''-'',I3,     ''  mass='',F1
     &0.6,'' Q-value='',F10.5)') INT(Z(nnuc)), SYMb(nnuc), ia,
     &         AMAss(nnuc), QPRod(nnuc) + ELV(LEVtarg,0)
                 WRITE (12,
     &'(''  Decaying nucleus '',I3,''-'',A2,''-'',I3,     ''  mass='',F1
     &0.6,'' Q-value='',F10.5)') INT(Z(nnuc)), SYMb(nnuc), ia,
     &         AMAss(nnuc), QPRod(nnuc) + ELV(LEVtarg,0)
               ELSE
                 WRITE (8,
     &'(''  Decaying nucleus '',I3,''-'',A2,''-'',I3,     ''  mass='',F1
     &0.6,'' Q-value='',F10.6)') INT(Z(nnuc)), SYMb(nnuc), ia,
     &         AMAss(nnuc), QPRod(nnuc) + ELV(LEVtarg,0)
                 WRITE (12,
     &'(''  Decaying nucleus '',I3,''-'',A2,''-'',I3,     ''  mass='',F1
     &0.6,'' Q-value='',F10.6)') INT(Z(nnuc)), SYMb(nnuc), ia,
     &         AMAss(nnuc), QPRod(nnuc) + ELV(LEVtarg,0)
               ENDIF
               WRITE (8,*)
     &' ---------------------------------------------------------------'
               WRITE (12,*)
     &' ---------------------------------------------------------------'
               WRITE (12,
     &'(1X,/,10X,''Discrete level population '',      ''before gamma cas
     &cade'')')
               WRITE (12,'(1X,/,10X,40(1H-),/)')
C
               DO il = 1, NLV(nnuc)
C-----------------Check for the number of branching ratios
                  nbr = 0
                  DO ib = 1, NDBR
                     IF (BR(il,ib,2,nnuc).EQ.0.) EXIT
                     nbr = ib
                  ENDDO
                  IF (nbr.EQ.0 .AND. il.NE.1 .AND. FIRst_ein .AND.
     &                (nnuc.EQ.mt91 .OR. nnuc.EQ.mt649 .OR.
     &                nnuc.EQ.mt849) .AND. ENDf(nnuc).NE.0)
     &                WRITE (8,*)
     &                 ' WARNING: Branching ratios for level ', il,
     &                ' IN ', INT(A(nnuc)), '-', SYMb(nnuc),
     &                ' are missing'
C    &                            XJLv(il, nnuc), POPlv(il, nnuc), nbr,
                  WRITE (12,99070) il, ELV(il,nnuc), LVP(il,nnuc),
     &                             XJLv(il,nnuc), CSDirlev(il,nejc),
     &                             nbr,
     &                             (NINT(BR(il,ib,1,nnuc)),BR(il,ib,2,
     &                             nnuc),ib = 1,nbr)
C-----------------Next IF moves levels population to the ground state
C-----------------to avoid gamma cascade between discrete levels
C-----------------originating from the direct population
C-----------------of discrete levels by a neutron, proton or alpha.
C-----------------These gammas should not go into MT=91, 649, or 849.
                  IF ((nnuc.EQ.mt91 .OR. nnuc.EQ.mt649 .OR. nnuc.EQ.
     &                mt849) .AND. il.NE.1 .AND. ENDf(nnuc).NE.0 ) THEN
                     POPlv(1,nnuc) = POPlv(1,nnuc) + CSDirlev(il,nejc)
                     POPlv(il,nnuc) = POPlv(il,nnuc) - CSDirlev(il,nejc)
                  ENDIF
               ENDDO
               WRITE (12,'(1X,/,10X,40(1H-),/)')
C
C--------------Decay direct population of discrete levels by a neutron,
C--------------proton or alpha without storing emitted gammas in the spectra.
               IF ((nnuc.EQ.mt91 .OR. nnuc.EQ.mt649 .OR. nnuc.EQ.
     &              mt849) .AND. ENDf(nnuc).NE.0 .AND. il.NE.1) 
     &              CALL DECAYD_DIR(nnuc, nejc)
C
C--------------Write elastic to tape 12 and to tape 68
 1460          IF (nnuc.EQ.mt2) THEN

                  WRITE (12,*) ' '
                  WRITE (12,
     &             '('' ELASTIC CROSS SECTION= '',1P,E12.5,'' mb'')')
     &              ELAcs*ELAred + 4.d0*PI*ELCncs
                  WRITE (12,*) ' '
                  WRITE (12,
     &             '('' SHAPE ELASTIC CROSS SECTION= '',1P,E12.5,
     &              '' mb'')') ELAcs*ELAred 
                  WRITE (12,*) ' '
                  WRITE (12,
     &             '('' COMP. ELASTIC CROSS SECTION= '',1P,E12.5,
     &               '' mb'')') 4.d0*PI*ELCncs
                  WRITE (12,*) ' '
                  WRITE (12,*) ' Elastic angular distribution '
                  WRITE (12,*) ' '
                  IF (ICAlangs.GT.0) THEN
                    WRITE (12,'(10X,8G15.5)') 
     &                (ANGles(iang),iang = 1, NANgela)
                  ELSE
                    delang = 180./FLOAT(NANgela - 1)
                    WRITE (12,'(10X,8G15.5)') 
     &                (FLOAT(iang - 1)*delang,iang = 1,NANgela)
                  ENDIF
                  
                  DO na = 1, NDANG
                    cel_da(na) = ELCncs ! isotropic
                  ENDDO

                  IF(.not.CN_isotropic) then

                    xs_norm = PL_CN(0,1)
                    IF(xs_norm.gt.0.d0) then
                      DO na = 1, NDANG
                        xs_cn = GET_DDXS(CANGLE(na),1)
                        cel_da(na) = xs_cn*(ELCncs/xs_norm)
C                       write(*,'(1x,A4,F4.0,A15,d13.6,3x,A7,d13.6)') 
C    >                 'ANG=',ANGles(na),' ECIS CN ang. dist.=',xs_cn,
C    >                     '  HF CN ang. distr.=',cel_da(na)
                      ENDDO
                    ENDIF
                    
                    WRITE (12,'(9X,8E15.5)') 
     &                ((ELAred*elada(iang)+cel_da(iang)),iang=1,NANgela)

                    WRITE (12,*)' '
                    WRITE (12,*)' '
                    WRITE (12,*)' DIR Legendre coefficients expansion'
                    WRITE (12,*)' '
                    WRITE (12,'(1x,A7,I5)') ' Lmax =',min(NDAng,neles)
                    WRITE (12,*)' '
                    WRITE (12,'(9X,8D15.8)') ELAred*elleg(1),
     &                (ELAred*elleg(iang),iang = 2,min(NDAng,neles))
                    WRITE (12,*)' '

                    IF(PL_CN(0,1).gt.0.d0) then
                      WRITE (12,*)
     &                          ' CE Legendre coefficients expansion'
                      WRITE (12,*)' '
                      WRITE (12,'(1x,A7,I5)') 
     &                          ' Lmax =',min(NDAng,PL_lmax(1))
                      WRITE (12,*) ' '
                      WRITE (12,'(9X,8D15.8)') ELCncs, 
     &                  (PL_CN(iang-1,1)*ELCncs/PL_CN(0,1),
     &                   iang = 2,min(NDAng,PL_lmax(1)))

                      WRITE (12,*)' '
                      WRITE (12,*)' Legendre coefficients expansion'
                      WRITE (12,*)' '
                      WRITE (12,'(1x,A7,I5)') ' Lmax =',min(NDAng,neles)
                      WRITE (12,*)' '
                      WRITE (12,'(9X,8D15.8)') ELAred*elleg(1)+ELCncs,
     &                  (ELAred*elleg(iang) +
     &                 PL_CN(iang-1,1)*ELCncs/PL_CN(0,1),
     &                 iang = 2,min(NDAng,neles))
                      WRITE (12,*)' '

                    ELSE

                      WRITE (12,*)' '
                      WRITE (12,*)' Legendre coefficients expansion'
                      WRITE (12,*)' '
                      WRITE (12,'(1x,A7,I5)') ' Lmax =',min(NDAng,neles)
                      WRITE (12,*)' '
                      WRITE (12,'(9X,8D15.8)') ELAred*elleg(1)+ELCncs, 
     &                  (ELAred*elleg(iang),iang = 2,min(NDAng,neles))
                      WRITE (12,*)' '

                    ENDIF
                           
                  ELSE
                    
                    WRITE (12,'(9X,8E15.5)') 
     &                ((ELAred*elada(iang)+cel_da(iang)),iang=1,NANgela)

                    WRITE (12,*) ' '
                    WRITE (12,*) ' '
                    WRITE (12,*) ' Legendre coefficients expansion '
                    WRITE (12,*) ' '
                    WRITE (12,'(1x,A7,I5)') ' Lmax =',min(NDAng,neles)
                    WRITE (12,*) ' '
                    WRITE (12,'(9X,8D15.8)')
     &                (ELAred*elleg(1) + ELCncs),
     &                (ELAred*elleg(iang),iang = 2,min(NDAng,neles))
                    WRITE (12,*) ' '

                  ENDIF

                  IF (FITomp.LT.0) THEN
                    WRITE(40,'(F12.4,3D12.5)') 
     &                EINl,TOTcs*TOTred*totcorr,ABScs*FUSred
                    IF (ncoll.GT.0) THEN
C---------------------locate position of the projectile among ejectiles
                      CALL WHEREJC(IZAejc(0),nejcec,iloc)
                      its = ncoll
                      WRITE (40,'(12x,11D12.5)') 
     &                           ELAred*ELAcs + 4.d0*PI*ELCncs,
     &                         (CSDirlev(ICOller(ilv),nejcec),
     &                          ilv = 2,MIN(its,10))
                      IF (ICAlangs.gt.0) THEN
                        DO iang = 1, NDANG
                          WRITE (40,'(f12.4,11D12.5)') ANGles(iang),
     &                           ELAred*elada(iang) + cel_da(iang),
     &                          (CSAlev(iang,ICOller(ilv),nejcec),
     &                           ilv = 2,MIN(its,10))
                        ENDDO
                      ENDIF
                    ELSE
                      WRITE (40,'(12x,11D12.5)') 
     &                           ELAred*ELAcs + 4.d0*PI*ELCncs
                      IF (ICAlangs.gt.0) THEN
                        DO iang = 1, NDANG
                          WRITE (40,'(f12.4,11D12.5)') ANGles(iang),
     &                           ELAred*elada(iang) + cel_da(iang)
                        ENDDO
                      ENDIF
                    ENDIF
                  ENDIF
               ENDIF
            ENDIF
         ENDIF
C--------Jump to end of loop after elastic when fitting
         If(FITomp.LT.0 .AND. nnuc.EQ.mt2) go to 1155

         POPmax(nnuc) = POPmax(nnuc)*0.0001
C        if POPmax(nnuc) = 0 then, continuum of this nucleus has not been populated, skipping
         IF (POPmax(nnuc).EQ.0.0D0) GOTO 1500 
C        IF (POPmax(nnuc).EQ.0.0D0) THEN
C           WRITE (8,*) ' '
C           WRITE (8,*)
C    &                'Continuum of this nucleus has not been populated'
C           GOTO 1500
C        ENDIF
         IF (nnuc.GT.1) THEN
           WRITE (8,*) ' '
           WRITE (8,*) ' -------------------------------------'
           WRITE (8,'(I3,2X,''Decaying nucleus '',I3,''-'',A2)') nnuc,
     &             ia, SYMb(nnuc)
           WRITE (8,*) ' -------------------------------------'
           WRITE (8,*) ' '
         endif
C--------Prepare gamma transition parameters
         CALL ULM(nnuc)
C--------Calculate compound nucleus level density at saddle point
         IF (NINT(FISshi(nnuc)).EQ.1) THEN
            IF (FISsil(nnuc)) THEN
               CALL ROEMP(nnuc,1.D0,0.0D0)
               IF(FIRst_ein) WRITE (8,*)
     &         ' WARNING: For HI reactions (FISSHI =1), LD model at sadd
     &les is EGSM'
               IF (IOUt.EQ.6 .and. FIRst_ein) THEN
                  WRITE (8,'(1X,/,'' Saddle point level density'',/)')
                  WRITE (8,'(1X,13G10.4)') (EX(i,nnuc),(ROF(i,j,nnuc)
     &                         ,j = 1,12), i = 1,NEX(nnuc))
               ENDIF
            ENDIF
         ENDIF
C--------
C--------Start Hauser-Feshbach nnuc nucleus decay
C--------
         popleft = 0.d0
C--------Turn on (KEMIN=1) or off (KEMIN=NEX(NNUC)) gamma cascade
C--------in the first CN, it is preferred to use input parameter GCASC (0=OFF,1=ON) 
         kemin = 1
         IF (nnuc.EQ.1 .and. GCAsc.EQ.0.0D0) kemin = NEX(nnuc)
C--------Turn  off (KEMIN=NEX(NNUC)) gamma cascade in the case of OMP fit
         IF (FITomp.NE.0) kemin = NEX(nnuc)
         kemax = NEX(nnuc)
C--------Account for widths fluctuations (HRTW)
         IF (LHRtw.EQ.1 .AND. EINl.GT.EHRtw) LHRtw = 0
         IF (nnuc.EQ.1 .AND. LHRtw.GT.0) THEN
C
C           Renormalizing transmission coefficients to consider PE emission
C                 before calling HRTW
            DO i = 1, NDLW
              ELTl(i) = ELTl(i) * corrmsd
            ENDDO
            CALL HRTW

            IF (ENDf(1).GT.0 .AND. RECoil.GT.0)
     &        CALL GET_RECOIL(kemax,nnuc) !recoil spectrum
            kemax = max(NEX(nnuc) - 1,1)
            IF (FISsil(nnuc) .and. NINT(FISshi(nnuc)).NE.1 ) THEN
               IF (NINT(FISmod(nnuc)).EQ.0) THEN
                 WRITE (80,*) 'csfis=', CSFis,' mb'
               ELSE
                 WRITE (80,*) '  '
                 DO m = 1, INT(FISmod(nnuc)) + 1
                     WRITE (80,*) '    Mode=', m, '  csfis=', CSFism(m),
     &                            ' mb'
                 ENDDO
               ENDIF
            ENDIF
         ENDIF
C
         cspg = 0.d0 

C--------DO loop over c.n. excitation energy
         DO ke = kemax, kemin, -1
            IF(ke.le.0) cycle
            step = DE
            IF (ke.EQ.NEX(nnuc) .OR. ke.EQ.1) step = 0.5*DE
            IF (ke.EQ.NEX(nnuc) .AND. nnuc.EQ.1) step = 1.0
            IF (ENDf(1).GT.0) THEN
C--------------Clean auxiliary particle spectra for calculation of recoils
               REClev = 0.d0           
               AUSpec = 0.d0
C--------------Calculate population in the energy bin ke
               pope = 0.d0
               DO jcn = 1, NLW
                  pope = pope + POP(ke,jcn,1,nnuc) + POP(ke,jcn,2,nnuc)
               ENDDO
               POPbin(ke,nnuc) = pope*step
            ENDIF

            DO ipar = 1, 2 !over decaying nucleus parity
               ip = INT(( - 1.0)**(ipar + 1))
               DO jcn = 1, NLW !over decaying nucleus spin
                  IF (GDRdyn.EQ.1.0D0) CALL ULMDYN(nnuc,jcn,EX(ke,nnuc))
                  DENhf = 0.d0
                  IF (POP(ke,jcn,ipar,nnuc).LT.POPmax(nnuc)) THEN
                     popleft = popleft + POP(ke,jcn,ipar,nnuc)*DE
                     CYCLE
                  ENDIF
                  DO nejc = 1, NEJcm !over ejectiles
                     ares = A(nnuc) - AEJc(nejc)
                     zres = Z(nnuc) - ZEJc(nejc)
C--------------------Residual nuclei must be heavier than alpha
                     if(ares.le.4. and. zres.le.2.) cycle
                     izares = INT(1000.0*zres + ares)
                     CALL WHERE(izares,nnur,iloc)
                     if(iloc.eq.1) CYCLE
                     CALL DECAY(nnuc,ke,jcn,ip,nnur,nejc,sum)
                  ENDDO
C-----------------DO loop over ejectiles       ***done***
C-----------------gamma emision
                  CALL DECAYG(nnuc,ke,jcn,ip,sum)
C-----------------Distribute yrast population over discrete levels
                  IF (DENhf.EQ.0.0D0) THEN
                     IF (ke.EQ.1) THEN
                        ded = DE*0.5
                     ELSE
                        ded = DE
                     ENDIF
                     IF (IOUt.GT.1) WRITE (8,
     & '('' Yrast state at bin'',I4,'' spin='',F5.1,'' pop='',   G12.5)'
     & ) ke, FLOAT(ip)*(FLOAT(jcn) + HIS(nnur)), POP(ke,jcn,ipar,nnuc)
     &   *ded
C
C                    Corrected on Jan 2011, Previously missed gamma XSs
C
                     CSEmis(0,Nnuc) = CSEmis(0,Nnuc) + 
     &                         POP(ke,jcn,ipar,nnuc)*ded

C--------------------Look for the discrete level with the closest spin
                     xnl = 1.d0
                     spdiff = 100.d0
                     DO il = 1, NLV(nnuc)
                        spdif = ABS(FLOAT(jcn) + HIS(nnur)
     &                          - XJLv(il,nnuc))
                        IF (spdif.LT.spdiff) THEN
                           spdiff = spdif
                           xnl = 1.d0
                        ELSE
                           IF (spdif.EQ.spdiff) xnl = xnl + 1.
                        ENDIF
                     ENDDO
                     DO il = 1, NLV(nnuc)
                        spdif = ABS(FLOAT(jcn) + HIS(nnur)
     &                          - XJLv(il,nnuc))
                        IF (spdif.EQ.spdiff) THEN
                           SCRtl(il,0) = 1.0D0/xnl
                           DENhf = DENhf + SCRtl(il,0)
                           IF (IOUt.GT.1) WRITE (8,
     &'(10X,I3,''% of this was assumed to populate level #'',
     &I3)') INT(100./xnl), il
                        ENDIF
                     ENDDO
                  ENDIF
C-----------------
C-----------------Fission ()
                  IF (FISsil(nnuc) .AND. NINT(FISshi(nnuc)).EQ.1)
     &                CALL FISSION(nnuc,ke,jcn,sumfis)
                  IF (FISsil(nnuc) .AND. NINT(FISshi(nnuc)).NE.1)
     &                CALL FISCROSS(nnuc,ke,ip,jcn,sumfis,sumfism)
C-----------------
C-----------------Normalization and accumulation
C-----------------
                  xnor = POP(ke,jcn,ipar,nnuc)*step/DENhf
                  stauc = stauc + RO(ke,jcn,ipar,nnuc)*xnor
                  IF (RO(ke,jcn,ipar,nnuc).NE.0.0D0) sgamc = sgamc +
     &             DENhf*POP(ke,jcn,ipar,nnuc)*step/RO(ke,jcn,ipar,nnuc)
C
                  CALL XSECT(nnuc,m,xnor,sumfis,
     &                       sumfism,ke,ipar,jcn,fisxse)
C
C-----------------Calculate total emission
C
                  DO nejc = 0, NEJcm
                     csemist = csemist + CSEmis(nejc,nnuc)
                  ENDDO
                  csemist = csemist + CSFis
C-----------------

               ENDDO                !loop over decaying nucleus spin
            ENDDO                   !loop over decaying nucleus parity
C
            IF (nnuc.GT.1 .AND. ENDf(nnuc).GT.0  .AND. RECoil.GT.0)
     &         CALL GET_RECOIL(ke,nnuc) !recoil spectrum for ke bin
            IF (FISsil(nnuc) .and. NINT(FISshi(nnuc)).NE.1
     &         .and. fisxse.gt.0) THEN
               IF (NINT(FISmod(nnuc)).EQ.0) THEN
                  WRITE (80,*) 'csfis=', CSFis,
     &              ' mb', '   fisxse=', fisxse, ' mb'
               ELSE
                  WRITE (80,*) '  '
                  DO m = 1, INT(FISmod(nnuc)) + 1
                     WRITE (80,*) '    Mode=', m, '  csfis=', CSFism(m),
     &                            ' mb'
                  ENDDO
                  WRITE (80,*) 'csfis=', CSFis,
     &            ' mb', '   fisxse=', fisxse, ' mb'
               ENDIF
            ENDIF
         ENDDO                  !loop over c.n. excitation energy
C--------
C--------Hauser-Feshbach decay of nnuc  ***done***
C--------
C--------Printout of results for the decay of NNUC nucleus
         IF (IOUt.GT.0) WRITE (8,
     &          '(1X,/,'' Population neglected because too'',
     &                               '' small '',G12.5,/)') popleft*DE

1500     DO il = 1, NLV(nnuc)
           CSPrd(nnuc) = CSPrd(nnuc) + POPlv(il,nnuc)
         ENDDO

         IF(CSPrd(nnuc).gt.0.d0) THEN

           IF (.not.(nnuc.EQ.1. OR. nnuc.EQ.mt91
     &          .OR. nnuc.EQ.mt649.OR.nnuc.EQ.mt849))  THEN 
                  WRITE (12,*)
     &' ---------------------------------------------------------------'
                  WRITE ( 8,*)
     &' ---------------------------------------------------------------'
                  IF(abs(QPRod(nnuc) + ELV(LEVtarg,0)).gt.99.99) THEN
                    WRITE (12,
     &'(''  Decaying nucleus '',I3,''-'',A2,''-'',I3,     ''  mass='',F1
     &0.6,'' Q-value='',F10.5)') INT(Z(nnuc)), SYMb(nnuc), ia,
     &         AMAss(nnuc), QPRod(nnuc) + ELV(LEVtarg,0)
                    WRITE ( 8,
     &'(''  Decaying nucleus '',I3,''-'',A2,''-'',I3,     ''  mass='',F1
     &0.6,'' Q-value='',F10.5)') INT(Z(nnuc)), SYMb(nnuc), ia,
     &         AMAss(nnuc), QPRod(nnuc) + ELV(LEVtarg,0)
                  ELSE
                    WRITE (12,
     &'(''  Decaying nucleus '',I3,''-'',A2,''-'',I3,     ''  mass='',F1
     &0.6,'' Q-value='',F10.6)') INT(Z(nnuc)), SYMb(nnuc), ia,
     &         AMAss(nnuc), QPRod(nnuc) + ELV(LEVtarg,0)
                    WRITE ( 8,
     &'(''  Decaying nucleus '',I3,''-'',A2,''-'',I3,     ''  mass='',F1
     &0.6,'' Q-value='',F10.6)') INT(Z(nnuc)), SYMb(nnuc), ia,
     &         AMAss(nnuc), QPRod(nnuc) + ELV(LEVtarg,0)
                  ENDIF
                  WRITE (12,*)
     &' ---------------------------------------------------------------'
                  WRITE (8 ,*)
     &' ---------------------------------------------------------------'
                  WRITE (12,*)
                  WRITE (8 ,*)
             ELSE
             IF (ENDF(nnuc).gt.0) WRITE (8,
     &'(3X,''NOTE: Due to ENDF option discrete levels contribution'',/, 
     &  3X,''NOTE:   was not included in emission spectra and direct ''/
     &  3X,''NOTE:   particle contribution was shifted to the g.s.'')')

               IF (kemin.EQ.NEX(nnuc) .AND. nnuc.EQ.1) WRITE (8,
     &'(10X,''(no gamma cascade in the compound nucleus, primary transit
     &ions only)'',/)')

             WRITE (8,
     &'(1X,/,10X,''Discrete level population before gamma cascade'')')
             WRITE (8,'(1X,/,10X,40(1H-),/)')

           ENDIF

           DO il = 1, NLV(nnuc)
             IF(ISIsom(il,Nnuc).EQ.0) THEN
               IF (IOUt.GT.0) WRITE (8,99070) il, ELV(il,nnuc),
     &                              LVP(il,nnuc), XJLv(il,nnuc),
     &                              POPlv(il,nnuc)
             ELSE
99071 FORMAT (I12,F10.5,I5,F8.1,G15.6,A7)
               IF (IOUt.GT.0) WRITE (8,99071) il, ELV(il,nnuc),
     &                              LVP(il,nnuc), XJLv(il,nnuc),
     &                              POPlv(il,nnuc),' ISOMER'
             ENDIF

             IF (nnuc.EQ.1) THEN            
C--------------Check for the number of branching ratios
               nbr = 0
               DO ib = 1, NDBR
                  IF (BR(il,ib,2,nnuc).EQ.0.) EXIT
                  nbr = ib
               ENDDO
               IF (nbr.EQ.0 .AND. il.NE.1 .AND. FIRst_ein) WRITE (8,*)
     &              ' WARNING: Branching ratios for level ', il, ' in ',
     &             INT(A(nnuc)), '-', SYMb(nnuc), ' are missing'
               WRITE (12,99070) il, ELV(il,nnuc), LVP(il,nnuc),
     &                          XJLv(il,nnuc), POPlv(il,nnuc), nbr,
     &                          (NINT(BR(il,ib,1,nnuc)),BR(il,ib,2,nnuc)
     &                          ,ib = 1,nbr)
             ENDIF
           ENDDO

           IF ( (nnuc.EQ.1 .OR. nnuc.EQ.mt91 .OR. nnuc.EQ.mt649 .OR.
     &           nnuc.EQ.mt849)) THEN
             WRITE (8,'(1X,/,10X,40(1H-),/)')
             WRITE (8,*)
           ENDIF
C
C          Primary gamma printout -----------------------
C
           IF (nnuc.EQ.1 .and. NPRIm_g.GT.0) THEN  
             cspg = 0.d0
             DO il = 1, NLV(nnuc)
               cspg = cspg + CSEpg(il) 
             ENDDO
             IF(cspg.gt.0.d0) then
               WRITE (12,*)
               WRITE (12,'(1X,/,10X,40(1H-),/)')
               WRITE (12,'(2x,
     &     '' Primary g  emission cross section'',G12.6,''  mb'')') cspg
               WRITE (12,'(1X,/,10X,40(1H-),/)')
               WRITE (12,'(11x,A1,A12,A6,A5,4x,A12,A7,1x,A6,1x,A10)') 
     &       'i','    Elv(i)  ','Par  ',' Spin',
     &       ' Prim.g CS   ',' Branch','  Egamma  '
               WRITE (12,*) ' '
               DO il = 1, NLV(nnuc)
                 WRITE (12,99910) il, ELV(il,nnuc), LVP(il,nnuc),
     &           XJLv(il,nnuc), CSEpg(il), CSEpg(il)/cspg*100., ENPg(il) 
99910            FORMAT (I12,F10.5,I5,F8.1,G15.6,1x,F6.2,1x,F10.5)
               ENDDO
               WRITE (12,'(1X,/,10X,40(1H-),/)')
               WRITE (12,*)
               WRITE (8,*)
               WRITE (8,'(1X,/,10X,40(1H-),/)')
               WRITE (8,'(2x,
     &     '' Primary g  emission cross section'',G12.6,''  mb'')') cspg
               WRITE (8,'(1X,/,10X,40(1H-),/)')
               WRITE (8,'(11x,A1,A12,A6,A5,4x,A12,A7,1x,A6,1x,A10)') 
     &           'i','    Elv(i)  ','Par  ',' Spin',
     &           ' Prim.g CS   ',' Branch','  Egamma  '
               WRITE (8,*) ' '
               DO il = 1, NLV(nnuc)
                 WRITE (8,99910) il, ELV(il,nnuc), LVP(il,nnuc),
     &           XJLv(il,nnuc), CSEpg(il), CSEpg(il)/cspg*100., ENPg(il) 
               ENDDO
               WRITE (8,'(1X,/,10X,40(1H-),/)')
               WRITE (8,*)
             ENDIF
C            Primary gammas -------- done ---------------
           ENDIF

           IF ( (nnuc.EQ.1 .OR. nnuc.EQ.mt91 .OR. nnuc.EQ.mt649 .OR.
     &           nnuc.EQ.mt849)) THEN
            WRITE (12,'(1X,/,10X,40(1H-),/)')
            WRITE (12,*) ' '
C-----------Write Int. Conv. Coefff. for discrete transitions
            WRITE (12,'(1X,/,10X,
     &             ''Internal conversion coefficients'')')
            WRITE (12,'(1X,/,10X,40(1H-),/)')
            DO il = 1, NLV(nnuc)
C-------------Check for the number of branching ratios
              nbr = 0
              DO ib = 1, NDBR
                IF (BR(il,ib,2,nnuc).EQ.0.) EXIT
                nbr = ib
              ENDDO
              WRITE (12,99065) il, ELV(il,nnuc), LVP(il,nnuc),
     &                          XJLv(il,nnuc), POPlv(il,nnuc), nbr,
     &                          (NINT(BR(il,ib,1,nnuc)),BR(il,ib,3,nnuc)
     &                          ,ib = 1,nbr)
99065         FORMAT (I12,F10.5,I5,F8.1,G15.6,I3,7(I4,E11.4),:/,
     &                 (53X,7(I4,E11.4)))
            ENDDO
            WRITE (12,'(1X,/,10X,40(1H-),/)')
           ENDIF
C
C----------gamma decay of discrete levels (DECAYD)
           CALL DECAYD(nnuc)
C
         ENDIF
C
         ia = INT(A(nnuc))
         iz = INT(Z(nnuc))

         IF (CSPrd(nnuc).GT.0.d0) THEN
           IF (kemin.EQ.NEX(nnuc) .AND. nnuc.EQ.1) WRITE (8,
     &'(1X,''(no gamma cascade in the compound nucleus, primary transiti
     &ons only)'',/)')

C----------Integrating exclusive population spectra (ENDF)
           gtotsp = 0.d0
           xtotsp = 0.d0
           ptotsp = 0.d0
           atotsp = 0.d0
           dtotsp = 0.d0
           ttotsp = 0.d0
           htotsp = 0.d0
           ctotsp = 0.d0
           emedg = 0.d0
           emedn = 0.d0
           emedp = 0.d0
           emeda = 0.d0
           emedd = 0.d0
           emedt = 0.d0
           emedh = 0.d0
           emedc = 0.d0
C          write(*,'(2x,F3.0,1x,F3.0,2x,A3,2x,F3.1)') 
C    &                 A(nnuc),Z(nnuc),' - ',ENDF(nnuc)
           IF (ENDf(nnuc).EQ.1) THEN
             nspec= min(INT(EMAx(nnuc)/DE) + 1,NDECSE-1)
             DO ispec = 1, nspec
               gtotsp = gtotsp + POPcse(0,0,ispec,INExc(nnuc))*DE
C              Write(12,*) nnuc,ispec,'g: ',
C     &           POPcse(0,0,ispec,INExc(nnuc)),CSE(ispec,0,nnuc) 
               xtotsp = xtotsp + POPcse(0,1,ispec,INExc(nnuc))*DE
c              Write(12,*) nnuc,ispec,'n: ',
c     &           POPcse(0,1,ispec,INExc(nnuc)),CSE(ispec,1,nnuc) 
               ptotsp = ptotsp + POPcse(0,2,ispec,INExc(nnuc))*DE
c              Write(12,*) nnuc,ispec,'p: ',
c     &           POPcse(0,2,ispec,INExc(nnuc)),CSE(ispec,2,nnuc) 
               atotsp = atotsp + POPcse(0,3,ispec,INExc(nnuc))*DE
c              Write(12,*) nnuc,ispec,'a: ',
c     &           POPcse(0,3,ispec,INExc(nnuc)),CSE(ispec,3,nnuc) 
               dtotsp = dtotsp + POPcse(0,4,ispec,INExc(nnuc))*DE
c              Write(12,*) nnuc,ispec,'d: ',
c     &          POPcse(0,4,ispec,INExc(nnuc)),CSE(ispec,4,nnuc) 
               ttotsp = ttotsp + POPcse(0,5,ispec,INExc(nnuc))*DE
c              Write(12,*) nnuc,ispec,'t: ',
c     &          POPcse(0,5,ispec,INExc(nnuc)),CSE(ispec,5,nnuc) 
               htotsp = htotsp + POPcse(0,6,ispec,INExc(nnuc))*DE
c              Write(12,*) nnuc,ispec,'h: ',
c     &          POPcse(0,6,ispec,INExc(nnuc)),CSE(ispec,6,nnuc) 
               emedg=emedg+POPcse(0,0,ispec,INExc(nnuc))*DE*(ispec-1)*DE
               emedn=emedn+POPcse(0,1,ispec,INExc(nnuc))*DE*(ispec-1)*DE
               emedp=emedp+POPcse(0,2,ispec,INExc(nnuc))*DE*(ispec-1)*DE
               emeda=emeda+POPcse(0,3,ispec,INExc(nnuc))*DE*(ispec-1)*DE
               emedd=emedd+POPcse(0,4,ispec,INExc(nnuc))*DE*(ispec-1)*DE
               emedt=emedt+POPcse(0,5,ispec,INExc(nnuc))*DE*(ispec-1)*DE
               emedh=emedh+POPcse(0,6,ispec,INExc(nnuc))*DE*(ispec-1)*DE
               IF (NDEJC.EQ.7) THEN
                 ctotsp = ctotsp + POPcse(0,NDEJC,ispec,INExc(nnuc))*DE
                 emedc  = emedc  + POPcse(0,NDEJC,ispec,INExc(nnuc))
     &                    *DE*(ispec - 1)*DE
               ENDIF
             ENDDO
             POPcs(0,INExc(nnuc)) = gtotsp
             POPcs(1,INExc(nnuc)) = xtotsp
             POPcs(2,INExc(nnuc)) = ptotsp
             POPcs(3,INExc(nnuc)) = atotsp
             POPcs(4,INExc(nnuc)) = dtotsp
             POPcs(5,INExc(nnuc)) = ttotsp
             POPcs(6,INExc(nnuc)) = htotsp
             IF (NDEJC.EQ.7) POPcs(NDEJC,INExc(nnuc)) = ctotsp

             WRITE (12,*)
             WRITE (8,*)
             DO nejc = 0, NDEJC         !loop over ejectiles
               IF (POPcs(nejc,INExc(nnuc)).LE.0.d0) CYCLE
               IF (nejc.EQ.0) THEN
                 cejectile = 'gammas   '
               ELSEIF (nejc.EQ.1) THEN
                 cejectile = 'neutrons '
               ELSEIF (nejc.EQ.2) THEN
                 cejectile = 'protons  '
               ELSEIF (nejc.EQ.3) THEN
                 cejectile = 'alphas   '
               ELSEIF (nejc.EQ.4) THEN
                 cejectile = 'deuterons'
               ELSEIF (nejc.EQ.5) THEN
                 cejectile = 'tritons  '
               ELSEIF (nejc.EQ.6) THEN
                 cejectile = 'helium-3 '
               ELSEIF (nejc.EQ.NDEJC) THEN
                 cejectile = 'lt. ions '
               ENDIF
               WRITE (12,9753) iz, SYMb(nnuc), ia, 
     &           POPcs(nejc,INExc(nnuc)),cejectile
9753           FORMAT(1X,I3,'-',A2,'-',I3,
     &           ' population cross section',G12.6,'  mb   : ',A9) 
             ENDDO
             WRITE (8,*)
             IF (gtotsp.NE.0) emedg = emedg/gtotsp
             IF (xtotsp.NE.0) emedn = emedn/xtotsp 
             IF (ptotsp.NE.0) emedp = emedp/ptotsp 
             IF (atotsp.NE.0) emeda = emeda/atotsp 
             IF (dtotsp.NE.0) emedd = emedd/dtotsp 
             IF (ttotsp.NE.0) emedt = emedt/ttotsp 
             IF (htotsp.NE.0) emedh = emedh/htotsp 
             IF (ctotsp.NE.0) emedc = emedc/ctotsp
C--------------Add contributions to discrete levels for MT=91,649,849
C--------------(merely for checking purpose)
             DO ii = 0, NDEjc
               xnorm(ii,INExc(nnuc)) = 1.0d0
             END DO
             IF (nnuc.EQ.mt91) THEN
               nejc = 1
               WRITE (8,'(6X,'' Cont. popul. before g-cascade '',
     &                G12.6,''  mb  '')') xtotsp
               WRITE (8,'(6X,'' Disc. popul. before g-cascade '',
     &                G12.6,''  mb  '')') CSDirlev(1,nejc)
C              WRITE (12,'(5X,'' Cont. popul. before g-cascade '',
C    &                G12.6,''  mb  '')') xtotsp
C              WRITE (12,'(5X,'' Disc. popul. before g-cascade '',
C    &                G12.6,''  mb  '')') CSDirlev(1,nejc)
               xtotsp = xtotsp + CSDirlev(1,nejc)
             ELSEIF (nnuc.EQ.mt649) THEN
               nejc = 2
               WRITE (8,'(6X,'' Cont. popul. before g-cascade '',
     &                G12.6,''  mb  '')') ptotsp
               WRITE (8,'(6X,'' Disc. popul. before g-cascade '',
     &                G12.6,''  mb  '')') CSDirlev(1,nejc)
C              WRITE (12,'(5X,'' Cont. popul. before g-cascade '',
C    &                G12.6,''  mb  '')') ptotsp
C              WRITE (12,'(5X,'' Disc. popul. before g-cascade '',
C    &                G12.6,''  mb  '')') CSDirlev(1,nejc)
               ptotsp = ptotsp + CSDirlev(1,nejc)     
             ELSEIF (nnuc.EQ.mt849) THEN
               nejc = 3
               WRITE (8,'(6X,'' Cont. popul. before g-cascade '',
     &                G12.6,''  mb  '')') atotsp
               WRITE (8,'(6X,'' Disc. popul. before g-cascade '',
     &                G12.6,''  mb  '')') CSDirlev(1,nejc)
C              WRITE (12,'(5X,'' Cont. popul. before g-cascade '',
C    &                G12.6,''  mb  '')') atotsp
C              WRITE (12,'(5X,'' Disc. popul. before g-cascade '',
C    &                G12.6,''  mb  '')') CSDirlev(1,nejc)
               atotsp = atotsp + CSDirlev(1,nejc)
             ELSE
               IF (LHMs.GT.0 .and. atotsp.LT.1.0d-8) THEN
                 totsp = CSprd(nnuc) - dtotsp - htotsp - ttotsp
                 IF(NDEJC.EQ.7) totsp = totsp - ctotsp
                 nxsp = 0
                 IF(xtotsp.GT.0.0d0) THEN
                   IF(ttotsp.GT.0.0d0) THEN
                     nxsp=MAX(INT((xtotsp-dtotsp)/totsp+0.5d0),0)
                     xnorm(1,INExc(nnuc)) =(nxsp*totsp+dtotsp)/xtotsp
                     xtotsp = nxsp*totsp + dtotsp
                   ELSE
                     nxsp=INT(xtotsp/totsp+0.5d0)
                     xnorm(1,INExc(nnuc)) = nxsp*totsp/xtotsp
                     xtotsp = nxsp*totsp
                   ENDIF
                   POPcs(1,INExc(nnuc)) = xtotsp
                   IF(ABS(1.0d0 - xnorm(1,INExc(nnuc))).GT.0.01d0) 
     &               WRITE(8,
     &      '(''  WARNING: Exclusive neutron spectrum renormalized by'',
     &                     f6.3)') xnorm(1,INExc(nnuc))
                 ENDIF
                 npsp = 0
                 IF(ptotsp.GT.0.0d0) THEN
                   IF(htotsp.GT.0.0d0) THEN
                     npsp=MAX(INT((ptotsp-dtotsp)/totsp+0.5d0),0)
                     xnorm(2,INExc(nnuc)) =(npsp*totsp+dtotsp)/ptotsp
                     ptotsp = npsp*totsp + dtotsp
                   ELSE
                     npsp=INT(ptotsp/totsp+0.5d0)
                     xnorm(2,INExc(nnuc)) = npsp*totsp/ptotsp
                     ptotsp = npsp*totsp
                   ENDIF
                   POPcs(2,INExc(nnuc)) = ptotsp
                   IF(ABS(1.0d0 - xnorm(2,INExc(nnuc))).GT.0.01d0) 
     &               WRITE(8,
     &      '(''  WARNING: Exclusive  proton spectrum renormalized by'',
     &                     f6.3)') xnorm(2,INExc(nnuc))
                 ENDIF
               ENDIF
             ENDIF
             WRITE (8,*) 
             WRITE (8,*)
     &           '-------------------------------------------------'
             WRITE (8,*) 
     &        'Population of residual nuclei (exclusive spectra - CMS)'
             WRITE (8,
     &           '('' Energy'',14x,''gamma'',9x,''neutron'',8x,
     &             ''proton'',10x,''alpha'',10x,''deut '',10x,
     &             ''trit '',10x,''He-3 '')')
             WRITE (8,*)
     &           '-------------------------------------------------'
             DO ispec = 1, nspec
               POPcse(0,1,ispec,INExc(nnuc)) = 
     &                xnorm(1,INExc(nnuc))*POPcse(0,1,ispec,INExc(nnuc))
               POPcse(0,2,ispec,INExc(nnuc)) = 
     &                xnorm(2,INExc(nnuc))*POPcse(0,2,ispec,INExc(nnuc))
               IF (NDEJC.EQ.7) THEN
                      WRITE (8,'(9g15.5)') (ispec - 1)*DE,
     &                      POPcse(0,0,ispec,INExc(nnuc)),
     &                      POPcse(0,1,ispec,INExc(nnuc)),
     &                      POPcse(0,2,ispec,INExc(nnuc)),
     &                      POPcse(0,3,ispec,INExc(nnuc)),
     &                      POPcse(0,4,ispec,INExc(nnuc)),
     &                      POPcse(0,5,ispec,INExc(nnuc)),
     &                      POPcse(0,6,ispec,INExc(nnuc)),
     &                      POPcse(0,NDEJC,ispec,INExc(nnuc))
               ELSE
                      WRITE (8,'(8g15.5)') (ispec - 1)*DE,
     &                      POPcse(0,0,ispec,INExc(nnuc)),
     &                      POPcse(0,1,ispec,INExc(nnuc)),
     &                      POPcse(0,2,ispec,INExc(nnuc)),
     &                      POPcse(0,3,ispec,INExc(nnuc)),
     &                      POPcse(0,4,ispec,INExc(nnuc)),
     &                      POPcse(0,5,ispec,INExc(nnuc)),
     &                      POPcse(0,6,ispec,INExc(nnuc))
               ENDIF
             ENDDO
             IF (NDEJC.EQ.7) THEN
               WRITE (8,*) '_________________________________________'
               WRITE (8,'(15X,8g15.6)')gtotsp, xtotsp, ptotsp, atotsp,
     &                   dtotsp,ttotsp,htotsp,ctotsp
               WRITE (8,'(''E-aver.'',8X,8g15.6)')emedg, emedn, emedp,
     &                emeda, emedd, emedt, emedh, emedc
             ELSE  
               WRITE (8,*) '_________________________________________'
               WRITE (8,'(15X,8g15.6)')gtotsp, xtotsp, ptotsp, atotsp,
     &                 dtotsp,ttotsp,htotsp    
               WRITE (8,'(''E-aver.'',8X,8g15.6)')emedg, emedn, emedp,
     &                emeda, emedd, emedt, emedh 
             ENDIF	      
C
C            Calculating Q-balance
C
C            EIN - QPRod(nnuc) + ELV(LEVtarg,0)
C
             cmulg = gtotsp/CSPrd(nnuc)
             cmuln = xtotsp/CSPrd(nnuc)
             cmulp = ptotsp/CSPrd(nnuc)
             cmula = atotsp/CSPrd(nnuc)
             cmuld = dtotsp/CSPrd(nnuc)
             cmult = ttotsp/CSPrd(nnuc)
             cmulh = htotsp/CSPrd(nnuc)
             WRITE (8,'(''Multip.'',8X,8g15.6)')cmulg, cmuln, cmulp,
     &                cmula, cmuld, cmult, cmulh

             totener_in=ABS(EIN+QPRod(nnuc)+ELV(LEVtarg,0))
             totener_out = cmulg*emedg + cmuln*emedn + cmulp*emedp +
     &       cmula*emeda + cmuld*emedd + cmult*emedt + cmulh*emedh
             WRITE (8,*) 
     &         '-------------------------------------------------'
             WRITE (8,'('' Qin ='',F8.3,'' Qout='',F8.3,'' Balance ='',
     &          F8.3,'' MeV  (no disc. levels considered)'')')
     &          totener_in , totener_out, (totener_in - totener_out)
             WRITE (8,*) 
     &         '*************************************************'
             WRITE (8,*) ' '
           ENDIF
         ENDIF
         IF (CSFis.NE.0.0D0) THEN
           WRITE (80,*)
           WRITE (8,*)
           IF (IOUt.GT.0) THEN
          
             DO m = 1, INT(FISmod(nnuc)) + 1
               WFIsm(m) = 0.d0
               IF (CSFis.GT.0.d0) WFIsm(m) = CSFism(m)/CSFis
               IF( NINT(FISmod(nnuc)).GT.0 .and. 
     >             NINT(FISshi(nnuc)).NE.1 )
     >           WRITE (80,*) '    Mode=', m, '   weight=', WFIsm(m)
             ENDDO
             IF(NINT(FISshi(nnuc)).NE.1)
     >         WRITE (80,*) '   Fission cross section=', CSFis, ' mb'
           ENDIF
           CSPfis(nnuc) = CSFis
           WRITE (8,
     &'(1X,I3,''-'',A2,''-'',I3,'' fission cross  section '',G12.5,''
     &mb  ''/)') iz, SYMb(nnuc), ia, CSFis
C
C          GAMT should be defined if lifetime is going to be calculated 
C
C          IF (IOUt.GT.0) THEN
C-------------Calculate average fission life-time and width
C             tauf = stauc*6.589E-22*2.0*PI/CSFis
C             WRITE(8,
C    &         '(''  Average fission life-time'',G12.5,'' s'')') tauf
C              gamfis = gamt*CSFis/csemist
C             WRITE (8,
C    &        '(''  Average fission width    '',G12.5,'' MeV'')') gamfis
C             WRITE (8,*) ' '
C          ENDIF
         ENDIF
         TOTcsfis = TOTcsfis + CSFis
C--------Add compound elastic to shape elastic before everything falls
C--------down on the ground state
9876     IF (nnuc.EQ.1  .AND. INT(AEJc(0)).NE.0
     &                       .AND. POPlv(LEVtarg,mt2).GT.0.) THEN
           WRITE (8,*)
           WRITE (8,*) ' Incident energy (CMS)      ', EIN, ' MeV'
           WRITE (8,*) ' Shape elastic cross section',
     &                     ELAred*ELAcs, ' mb'
C----------CN contribution to elastic ddx
           ELCncs = POPlv(LEVtarg,mt2)/4.d0/PI 
           if(.not.CN_isotropic .and. ELCncs.LT.0.05d0) then    
             CN_isotropic = .TRUE.
             WRITE(8,*)
             WRITE(8,*) 
     &       'CN angular distribution assumed isotropic at Einc = ',
     &       sngl(EINl)
             WRITE(12,*)      
     &       'CN angular distribution assumed isotropic at Einc = ',
     &       sngl(EINl)
             WRITE(8,*)
           endif  

           IF (ELCncs.EQ.0) then
             WRITE (8,*) ' WARNING: CN elastic is 0'
           ELSE
             WRITE (8,*) ' CN elastic cross section   ',
     &                sngl(POPlv(LEVtarg,mt2)),' mb'
             IF(CN_isotropic) then   
               WRITE (8,*)
     &          ' Isotropic Compound Elastic=', sngl(ELCncs), ' mb/str'
             ELSE
               WRITE (8,*) ' CN elastic cross section (ECIS) ',
     &           sngl(4.d0*pi*PL_CN(0,1)),' mb' 
               ftmp = 1.d0
               if(PL_CN(0,1).gt.0.d0) ftmp = ELCncs/PL_CN(0,1)
               IF(INTerf.eq.1) then
                 WRITE (110,'(1x,E12.5,3x,F8.4,3x,11(F9.2,1x),A17)') 
     &           EINl, ftmp, 4.d0*pi*ELCncs,  
     &                      (4.d0*pi*PL_CN(0,ilevcol),ilevcol=1,10),
     &           'ENG-WEID. TRANSF.'  
               ELSE
                 WRITE (110,'(1x,E12.5,3x,F8.4,3x,11(F9.2,1x),A17)') 
     &           EINl, ftmp, 4.d0*pi*ELCncs,  
     &                      (4.d0*pi*PL_CN(0,ilevcol),ilevcol=1,10)
               ENDIF                
               WRITE (8,*) 
               WRITE (8,*) ' Nonisotropic Compound to discrete levels in
     &cluding the Compound Elastic'
               WRITE (8,*) 

               xs_norm = PL_CN(0,1)
               IF(xs_norm.gt.0.d0) then
                 DO na = 1, NDANG
                   xs_cn = GET_DDXS(CANGLE(na),1)
                   cel_da(na) = xs_cn*(ELCncs/xs_norm)
                 ENDDO
               ENDIF

             ENDIF
           ENDIF

           gang = 180.d0/(NDAng - 1)
           angstep = 180.d0/(NANgela - 1)
           WRITE (8,99016)
           WRITE (8,99020)
           DO iang = 1, NANgela/4 + 1
             imint = 4*(iang - 1) + 1
             imaxt = MIN0(4*iang,NANgela)
             WRITE (8,99025) 
     &           ((j - 1)*angstep,cel_da(j),j = imint,imaxt)
           ENDDO

           IF (ncoll.GT.0) THEN
C----------------Locate position of the projectile among ejectiles
                 CALL WHEREJC(IZAejc(0),nejcec,iloc)
C
C                WRITE (8,*) ' '
                 its = ncoll
                 IF (CSAlev(1,ICOller(2),nejcec).GT.0) THEN
                   WRITE(8,99029)
                   WRITE(8,99030) (ICOller(ilv),ilv = 2,MIN(its,10))
                   WRITE(8,99031) (ELV(ICOller(ilv),nnurec),
     &               ilv = 2,MIN(its,10))
                   WRITE(8,99033) (XJLv(ICOller(ilv),nnurec)*
     &               LVP(ICOller(ilv),nnurec),D_DEF(ilv,2),
     &               ilv = 2,MIN(its,10))  
                   WRITE(8,*) ' '
                   DO iang = 1, NDANG
                     WRITE (8,99035) (iang - 1)*gang,
     &               (CSAlev(iang,ICOller(ilv),nejcec),
     &               ilv = 2,MIN(its,10)) 
                   ENDDO
                   WRITE(8,*) ' '
                   WRITE(8,99041) 1,(POPlv(ICOller(ilv),nnurec),
     &               ilv= 2,MIN(its,10))
C
                   IF(its.gt.10) THEN
C                    WRITE(8,*) ' '
                     WRITE(8,*) ' '
                     WRITE(8,99030)(ICOller(ilv),ilv = 11,MIN(its,20))
                     WRITE(8,99032)(ELV(ICOller(ilv),nnurec),
     &                 ilv=11,MIN(its,20))
                     WRITE(8,99034)(XJLv(ICOller(ilv),nnurec)*
     &                 LVP(ICOller(ilv),nnurec),D_DEF(ilv,2),
     &                 ilv=11,MIN(its,20))  
                     WRITE (8,*) ' '
                     DO iang = 1, NDANG
                       WRITE (8,99035) (iang - 1)*gang,
     &                 (CSAlev(iang,ICOller(ilv),nejcec),
     &                 ilv = 11,MIN(its,20))
                     ENDDO
                     WRITE (8,*) ' '
                     WRITE (8,99041) 2,(POPlv(ICOller(ilv),nnurec),
     &                 ilv = 11,MIN(its,20))
                   ENDIF
C
                   IF(its.gt.20) THEN
C                    WRITE(8,*) ' '
                     WRITE(8,*) ' '
                     WRITE(8,99030)(ICOller(ilv),ilv = 21,MIN(its,30))
                     WRITE(8,99032)(ELV(ICOller(ilv),nnurec),
     &                 ilv=21,MIN(its,30))
                     WRITE(8,99034)(XJLv(ICOller(ilv),nnurec)*
     &                 LVP(ICOller(ilv),nnurec),D_DEF(ilv,2),
     &                 ilv=21,MIN(its,30))
                     WRITE (8,*) ' '
                     DO iang = 1, NDANG
                       WRITE (8,99035) (iang - 1)*gang,
     &                 (CSAlev(iang,ICOller(ilv),nejcec),
     &                 ilv=21,MIN(its,30))
                     ENDDO
                     WRITE (8,*) ' '
                     WRITE (8,99041) 3,(POPlv(ICOller(ilv),nnurec),
     &                 ilv=21,MIN(its,30))
                   ENDIF
C                  Because of the ENDF format restrictions the maximum
C                     number of discrete levels is limited to 40
                   IF(its.gt.30) THEN
C                    WRITE(8,*) ' '
                     WRITE(8,*) ' '
                     WRITE(8,99030)(ICOller(ilv),ilv = 31,MIN(its,40))
                     WRITE(8,99032)(ELV(ICOller(ilv),nnurec),
     &                 ilv = 31,MIN(its,40))
                     WRITE(8,99034)(XJLv(ICOller(ilv),nnurec)*
     &                 LVP(ICOller(ilv),nnurec),D_DEF(ilv,2),
     &                 ilv = 31,MIN(its,40))
                     WRITE (8,*) ' '
                     DO iang = 1, NDANG
                       WRITE (8,99035) (iang - 1)*gang,
     &                 (CSAlev(iang,ICOller(ilv),nejcec),
     &                 ilv = 31,MIN(its,40))
                     ENDDO
                     WRITE (8,*) ' '
                     WRITE (8,99041) 4,(POPlv(ICOller(ilv),nnurec),
     &                 ilv = 31,MIN(its,40))
                   ENDIF
                   WRITE (8,*) ' '
                 ENDIF
           ENDIF ! ncoll > 0 ?

C            ENDIF
C          ENDIF

         ENDIF
         IF(CSPrd(nnuc).GT.0.d0) THEN
           checkXS = checkXS + CSPrd(nnuc)
           jz = INT(Z(1))-iz
           jn = INT(A(1))-ia-jz
           checkprd = CSPrd(nnuc)
           xcross(NDEJC+2,jz,jn) = CSPrd(nnuc)
           metas = 0
           ftmp_gs = CSPrd(nnuc)
           DO l= NLV(Nnuc), 2, -1
            IF(ISIsom(l,Nnuc).EQ.1) THEN
              metas = metas + 1
              WRITE(12,'(1X,I3,''-'',A2,''-'',I3,
     &         '' isomer state population  '',G12.6,
     &         '' mb (m'',I1,'' E='',F7.4,''MeV Jp='',F5.1,'')'')')
     &         iz, SYMb(nnuc), ia, POPlv(l,Nnuc),
     &         metas, ELV(l,Nnuc), LVP(l,Nnuc)*XJLv(l,Nnuc)
              ftmp_gs = ftmp_gs - POPlv(l,Nnuc)
C             CSPrd(nnuc) = CSPrd(nnuc) - POPlv(l,Nnuc)
            ENDIF
           ENDDO
           IF(metas.GT.0) WRITE(12,'(1X,I3,''-'',A2,''-'',I3,
     &           '' ground state population  '',G12.6,'' mb'')')
     &           iz, SYMb(nnuc), ia, ftmp_gs
         ENDIF

5753     FORMAT(1X,I3,'-',A2,'-',I3,
     &    '    fission cross section',G12.6,'  mb') 
         IF (CSFis.gt.0.) WRITE (12,5753) iz, SYMb(nnuc), ia, CSFis

         IF(CSPrd(nnuc).gt.0.d0) then 
           WRITE (12,*)
           WRITE (8,*)
           WRITE (8,
     &'(1X,I3,''-'',A2,''-'',I3,'' production cross section '',G12.6,
     &'' mb  '',''      reac: '',A21)') iz, SYMb(nnuc), ia, CSPrd(nnuc),
     &                             REAction(nnuc)
           WRITE (12,
     &'(1X,I3,''-'',A2,''-'',I3,'' production cross section'',G12.6,
     &''  mb '',''      reac: '',A21)') iz, SYMb(nnuc), ia, CSPrd(nnuc),
     &                             REAction(nnuc)
         ENDIF

         checkprd = checkprd + CSFis
         xcross(NDEJC+1,jz,jn) = CSFis
         WRITE (8,*)
C        Integral is calculated by trapezoidal rule being consistent with cross section
         IF(IOUt.GT.0) CALL AUERST(nnuc,0,0)
C        IF(nnuc.eq.NTArget .and. ENDf(nnuc).GT.0) THEN
C        IF(nnuc.eq.NTArget                      ) THEN
C----------Locate position of the projectile among ejectiles
C          CALL WHEREJC(IZAejc(0),nejc,iloc)
C          WRITE (8,'(''  g  disc.lev cross section'',G12.6,''  mb'')')
C    &       CSDirlev(1,nejc)
C          WRITE (12,'(10x,
C    &                 '' g  disc.lev cross section'',G12.6,''  mb'')')
C    &      CSDirlev(1,nejc)
C        ENDIF 
         IF(CSEmis(0,nnuc).gt.0) THEN
           WRITE (12,'(10x,
     &                 '' g  emission cross section'',G12.6,''  mb'')')
     &       CSEmis(0,nnuc)
           if(nnuc.eq.1) WRITE (12,'(2x,
     &     '' Primary g  emission cross section'',G12.6,''  mb'')') cspg
         ENDIF

C        IF(CSPrd(nnuc).gt.0.d0) then 
C          WRITE (8,*)
C          WRITE (8,
C    &'(1X,I3,''-'',A2,''-'',I3,'' production cross section '',G12.6,
C    &'' mb  '',''      reac: '',A21)') iz, SYMb(nnuc), ia, CSPrd(nnuc),
C    &                             REAction(nnuc)
C          WRITE (8,*)
C          WRITE (12,*)
C          WRITE (12,
C    &'(1X,I3,''-'',A2,''-'',I3,'' production cross section'',G12.6,
C    &''  mb '',''      reac: '',A21)') iz, SYMb(nnuc), ia, CSPrd(nnuc),
C    &                             REAction(nnuc)
C        ENDIF

         xcross(0,jz,jn) = CSEmis(0,nnuc)
C----------------------------------------------------------------------
         IF(CSPrd(nnuc).GT.0.d0) THEN
           DO nejc = 1, NEJcm
             ares = A(nnuc) - AEJc(nejc)
             zres = Z(nnuc) - ZEJc(nejc)
C------------Residual nuclei must be heavier than alpha
             if(ares.le.4. and. zres.le.2.) cycle
             izares = INT(1000.0*zres + ares)
             CALL WHERE(izares,nnur,iloc)
             checkprd = checkprd +  CSEmis(nejc,nnuc)
             xcross(nejc,jz,jn) = CSEmis(nejc,nnuc)
             IF(iloc.EQ.1 .AND. CSEmis(nejc,nnuc).GT.0) WRITE (12,
     &       '('' iloc=1! CSEmis('',i1,'','',i2,'')='',G12.5)')
     &                                 nejc,nnuc,CSEmis(nejc,nnuc)
             IF(iloc.EQ.1) CYCLE
C            IF(CSEmis(nejc,nnuc).LE.1.d-8) CYCLE
             IF(CSEmis(nejc,nnuc).LE.0) CYCLE
             WRITE (12,
     &           '(11X,A2,'' emission cross section'',G12.6,''  mb'')')
     &             SYMbe(nejc), CSEmis(nejc,nnuc)
             IF (ENDf(nnuc).EQ.1 .and. FIRst_ein .and. IOUT.GT.5 .and.
     &           AEJc(0).LE.4.)  ! excluding HI reactions
     &           CALL PLOT_EMIS_SPECTRA(nnuc,nejc,0)
C
C            Integral is calculated by trapezoidal rule being consistent with cross section
             IF (IOUt.GT.0) CALL AUERST(nnuc,nejc,0) 
C------------Print residual nucleus population
             poptot = 0.0
             IF (NEX(nnur).GT.0) THEN !avoid summing non-existent continuum
                DO j = 1, NLW
                  DO i = 1, NEX(nnur)
                    poptot = poptot + POP(i,j,1,nnur) + POP(i,j,2,nnur)
                  ENDDO
                  poptot = poptot 
     &                   - 0.5*(POP(1,j,1,nnur) + POP(1,j,2,nnur))
     &                   - 0.5*(POP(NEX(nnur),j,1,nnur) 
     &                   +      POP(NEX(nnur),j,2,nnur))
                ENDDO
             ENDIF 
             poptot = poptot*DE
             poplev = 0.d0
             DO i = 1, NLV(nnur)
               poplev = poplev + POPlv(i,nnur)
             ENDDO
             IF(LHMs.NE.0) THEN
               IF(nejc.GT.2) THEN
                 poptot = poptot - POPcon(nnur)
                 poplev = poplev - POPdis(nnur)
               ELSE
                 poptot = poptot + CSHms(1,nnur) + CSHms(2,nnur)
               ENDIF
             ENDIF

             if(A(nnuc).eq.A(1) .and. Z(nnuc).eq.Z(1) 
     &                          .and. ENDF(nnuc).gt.0) then
               WRITE (12,
     &            '(13x,   '' total population      '',G12.6,''  mb'')')
     &            poplev + poptot
               WRITE (12,
     &            '(13x,   '' total popul.continuum '',G12.6,''  mb'')')
     &            poptot
               WRITE (12,
     &            '(13x,   '' total popul.disc.lev. '',G12.6,''  mb'')')
     &            poplev

C              WRITE (8,*) '    RESIDUAL = TARGET NUCLEUS'
C              WRITE (8,
C    &         '(1x,''    Total population      '',G12.6,''  mb'')')
C    &          poplev + poptot
C              WRITE (8,
C    &         '(1x,''    Total popul.continuum '',G12.6,''  mb'')')
C    &          poptot
C              WRITE (8,
C    &         '(1x,''    Total popul.disc.lev. '',G12.6,''  mb'')')
C    &          poplev
C              WRITE (8,*)
             endif

             IF (IOUt.EQ.4) THEN
               ia = INT(A(nnur))
               WRITE (8,*) ' '
               WRITE (8,*) '**************************** '
               WRITE (8,'('' Residual nucleus '',I3,''-'',A2,/)') ia,
     &                SYMb(nnur)
               WRITE (8,'('' Positive parities population'',/)')
               do i = NEX(nnur),1,-1
                 ftmp = 0.d0
                 do j = 1,12
                   ftmp = ftmp + POP(i,j,1,nnur)
                 enddo
                 if(ftmp.gt.0.d0)
     &             WRITE (8,99075) EX(i,nnur),(POP(i,j,1,nnur),j = 1,12)
               enddo
               WRITE (8,*) ' '
               WRITE (8,'('' Negative parities population'',/)')
               do i = NEX(nnur),1,-1
                 ftmp = 0.d0
                 do j = 1,12
                   ftmp = ftmp + POP(i,j,2,nnur)
                 enddo
                 if(ftmp.gt.0.0)
     &             WRITE (8,99075) EX(i,nnur),(POP(i,j,2,nnur),j = 1,12)
               enddo
               WRITE (8,'('' '')')

             ENDIF

           ENDDO   !over ejectiles
C          WRITE (12,
C    &       '(9x,'' Tot prod+emi cross section'',G12.5,''  mb'')')
C    &               checkprd
C          WRITE ( 8,
C    &       '(9x,'' Tot prod+emi cross section'',G12.5,''  mb'')')
C    &               checkprd
           xcross(NDEJC+3,jz,jn) = checkprd

         ENDIF ! if CSProd > 0
C--------
C--------NNUC nucleus decay    **** done ******
C--------
      ENDDO     !over decaying nuclei
C-----Write a row in the table of cross sections (Note: inelastic has CN elastic subtracted)
cccccccccccccccccccc ccccccccccccccccccccccccccccc
C-----Reaction Cross Sections lower than 1.d-8 are considered zero.

      eps=1.d-8
	csinel=CSPrd(2) ! for charged particles or photons

      IF(INT(AEJc(0)).GT.0 .and. INT(ZEJc(0)).EQ.0) ! for neutrons
     >  csinel=CSPrd(2)-4.d0*PI*ELCncs

      if (csinel.lt.eps) csinel=0.d0
      do nnuc=1,NNUcd
        if (CSPrd(nnuc).lt.eps) CSPrd(nnuc)=0.d0
      enddo

      if(NUBarread) then
          xnub = PFNniu*fniu_nubar_eval(EINl)
      else
          xnub = 0.D0
      endif

      IF(TOTcsfis.gt.0.d0 .and. NINT(FISshi(nnuc)).NE.1) 
     &  WRITE(98,'(1P,E10.4,1x,1P,(30E12.5))') EINl,
     &     TOTcsfis, (CSPfis(nnuc),nnuc=1,min(NNUcd,10,max_prn-1))
      CLOSE (80)
      CLOSE (79)
      WRITE (12,*) 
      WRITE (12,'('' Tot. fission cross section '',G12.4,'' mb'')')
     &       TOTcsfis
      IF(ENDf(1).GT.0) THEN 
        WRITE (12,*) 
        WRITE (12,*) '*******************************************'
        WRITE (12,*) '* EMISSION SPECTRA at Einc =', sngl(EINl) 
        WRITE (12,*) '*******************************************'
        WRITE (12,*) 
      ENDIF 
      WRITE (8,*) 
      WRITE (8,'(''  Tot. fission cross section '',G12.4,'' mb'')')
     &       TOTcsfis
C----
C---- Initialization of PFNS calculations
C----
c fisspec===========
      IF (FISspe.eq.1) CALL INPUT_SPEC
C     Assumed that a maximum of 2 different isotopes contribute to fission XS
C     (i.e those coming from fissioning nuclei after primary neutron-proton
C     emission and first CN.
      nfission = 0
      nepfns   = 0
      enepfns  = 0.d0
      csepfns  = 0.d0
      fniuEVAL = 1.d0 
      fnubar   = 1.d0

c fisspec===============
C     For Kalbach parameterization
      do i=1,NDAng
         theta=DBLE(i-1)/DBLE(NDAng-1)*pi
         xcos(i)=cos(theta)
      enddo
C----
C---- ENDF spectra printout (exclusive representation)
C----
      DO nnuc = 1, NNUcd  ! loop over residues (not decaying nuclei)
         IF (ENDf(nnuc).EQ.1) THEN
           IF (CSPrd(nnuc).GT.0.0D0) THEN

              DO nejc = 0, NDEJC         !loop over ejectiles
                IF (POPcs(nejc,INExc(nnuc)).EQ.0.d0) CYCLE
                IF(A(nnuc).LE.4. AND. Z(nnuc).LE.2.) CYCLE
                IF(nejc.GT.0) THEN
                  CALL WHERE(IZA(nnuc)+IZAejc(nejc),nnur,iloc)
                ELSE
                  nnur = nnuc
                  iloc = 0
                ENDIF
                IF(iloc.NE.0) CYCLE

                IF (nejc.EQ.0) THEN
                  cejectile = 'gammas   '
                  iizaejc = 0
                ELSEIF (nejc.EQ.1) THEN
                  cejectile = 'neutrons '
                  iizaejc = IZAejc(nejc)
                ELSEIF (nejc.EQ.2) THEN
                  cejectile = 'protons  '
                  iizaejc = IZAejc(nejc)
                ELSEIF (nejc.EQ.3) THEN
                  cejectile = 'alphas   '
                  iizaejc = IZAejc(nejc)
                ELSEIF (nejc.EQ.4) THEN
                  cejectile = 'deuterons'
                  iizaejc = IZAejc(nejc)
                ELSEIF (nejc.EQ.5) THEN
                  cejectile = 'tritons  '
                  iizaejc = IZAejc(nejc)
                ELSEIF (nejc.EQ.6) THEN
                  cejectile = 'helium-3 '
                  iizaejc = IZAejc(nejc)
                ELSEIF (nejc.EQ.NDEJC) THEN
                  cejectile = 'lt. ions '
                  iizaejc = IZAejc(NDEJC)
                ENDIF
C---------------Double the first bin x-sec to preserve integral in EMPEND
                POPcse(0, nejc, 1, INExc(nnuc)) =  
     &                  POPcse(0, nejc, 1, INExc(nnuc))*2
                WRITE (12,*) ' '
                WRITE (12,*) ' Spectrum of ', cejectile,
     &                         REAction(nnuc), ' ZAP= ', iizaejc
C
C---------------Exclusive DDX spectra (all particles but gammas)
                recorp = 1.d0
                nspec= min(INT(EMAx(nnuc)/DE) + 1,NDECSE-1)
                dang = PI/FLOAT(NDANG - 1)
C                coef = 2*PI*dang
C               if (nejc.eq.1) then 
C                 csum = 0.d0 
C                 DO nang = 1, NDANG
C                   csum = csum + SANgler(nang)
C                 ENDDO
C                 write (*,*) 'Int(sin)=',csum*dang 
C               endif
                IF (nejc.GT.0) THEN
C---------------recorp is a recoil correction factor defined 1+Ap/Ar that
C---------------multiplies cross sections and divides outgoing energies
                  IF (RECoil.GT.0) 
     &              recorp = 1.d0 + EJMass(nejc)/AMAss(nnuc)
                   WRITE (12,
     &                      '(30X,''A     n     g     l     e     s '')'
     &                      )
                   WRITE (12,*) ' '
                   WRITE (12,'('' Energy   '',8G15.5,/,(10X,8G15.5))')
     &                      (ANGles(nang),nang=1,NDANG)
                   check_DL = 0.d0
C------------------First emission reactions
C------------------(discrete levels part)
                   IF ((nnuc.EQ.mt91  .AND. nejc.EQ.1) .OR.
     &                 (nnuc.EQ.mt649 .AND. nejc.EQ.2) .OR.
     &                 (nnuc.EQ.mt849 .AND. nejc.EQ.3) ) THEN
C                     csum = 0.d0
                     DO il = 1, NLV(nnuc)  ! discrete levels
                        espec = (EMAx(nnuc) - ELV(il,nnuc))/recorp
                        IF (espec.GE.0) WRITE (12,
     &                     '(F10.5,E14.5,7E15.5,/,(9X,8E15.5))') -espec, 
     &                     (max(CSAlev(nang,il,nejc)*recorp/DE,
     &                               0.d0),nang = 1,NDANG)
                        csum = 0.d0
                        DO nang = 2, NDANG  ! over angles
                          csum = csum + (CSAlev(nang,il,nejc) 
     &                                  + CSAlev(nang-1,il,nejc))
     &                         * 0.5d0 * (CAngler(nang)-CANgler(nang-1))
                        ENDDO
                        check_DL(il) = max(2.0d0*PI*csum,1.d-10)
                     ENDDO
                   ENDIF
C
C------------------(continuum part - same for all particles)
                   cseaprnt = 0.d0 ! clean DDX matrix
                   check_DE = 0.d0
                   dtmp = 0.d0
C                  DO ie = 1, nspec     ! reconstruct continuum DDX spectrum
C                  range extended to cover the last energy corresponding to the exact endpoint
                   DO ie = 1, nspec + 1 ! reconstruct continuum DDX spectrum
                     htmp = POPcse(0,nejc,ie,INExc(nnuc))
                     if(htmp.LE.0.d0) cycle
	               ftmp = 1.d0
	               if(ie.eq.1 .or. ie.eq.nspec + 1) ftmp=0.5d0
                     dtmp = dtmp + htmp*DE*ftmp
                     ftmp = (htmp - xnorm(nejc,INExc(nnuc))
     &                        * POPcsed(0,nejc,ie,INExc(nnuc)))/4.0/PI
                     IF(ftmp.LT.0.0d0) ftmp = 0.0d0
                     csum = 0.d0
                     IF(LHMs.GT.0 .AND. (nejc.EQ.1 .OR. nejc.EQ.2)) THEN
C----------------------Check whether integral over angles agrees with DE spectra
                       DO nang = 1, NDANG
                         cseaprnt(ie,nang) = 
     &                     ftmp + xnorm(nejc,INExc(nnuc))*
     &                            POPcsea(nang,0,nejc,ie,INExc(nnuc))
                           IF(nang.GT.1) csum = csum 
     &                         + (cseaprnt(ie,nang)+cseaprnt(ie,nang-1))
     &                         * 0.5d0 * (CAngler(nang)-CANgler(nang-1))
                       ENDDO
                     ELSE
c The following is equivalent the definition of ftmp above, when LHMs=0.
c                      ftmp =(POPcse(0,nejc,ie,INExc(nnuc))-
c     &                  CSEmsd(ie,nejc)*POPcseaf(0,nejc,ie,INExc(nnuc))/4.0/PI
                       DO nang = 1, NDANG
                         cseaprnt(ie,nang) =
     &                     ftmp + CSEa(ie,nang,nejc,1)*
     &                            POPcseaf(0,nejc,ie,INExc(nnuc))
                           IF(nang.GT.1) csum = csum 
     &                         + (cseaprnt(ie,nang)+cseaprnt(ie,nang-1))
     &                         * 0.5d0 * (CAngler(nang)-CANgler(nang-1))
                       ENDDO
                     ENDIF
                     check_DE(ie) = 2.0d0*PI*csum
C--------------------Correct 'cseaprnt()' for eventual imprecision
                     if(check_DE(ie).GT.0.d0) then
  	                 ftmp = POPcse(0,nejc,ie,INExc(nnuc))/check_DE(ie)
                       DO nang = 1, NDANG
                         cseaprnt(ie,nang) = cseaprnt(ie,nang)*ftmp
                       ENDDO
C					 double check
C                      csum = 0.d0
C                      DO nang = 1, NDANG
C                          csum = csum + cseaprnt(ie,nang)*SANgler(nang)
C                      ENDDO
C                      check_DE(ie) = csum*coef
                     endif
                   ENDDO

                   DO ie = 1, nspec 
                                     ! print DDX spectrum
                     if(check_DE(ie).LE.0.d0) cycle ! skipping zeroes

                     WRITE (12,'(F10.5,E14.5,7E15.5,/,(9X,8E15.5))')
     &                     FLOAT(ie - 1)*DE/recorp,
     &                     (cseaprnt(ie,nang)*recorp,nang = 1,NDANG)
                   ENDDO
                                     ! exact DDX spectrum endpoint
                   WRITE (12,'(F10.5,E14.5,7E15.5,/,(9X,8E15.5))')
     &               EMAx(nnuc)/recorp,
     &                (cseaprnt(nspec + 1,nang)*recorp,nang = 1,NDANG)
                   WRITE (12,*) ' '    
C
C                  Integrated spectrum
C
                   IF ((nnuc.EQ.mt91  .AND. nejc.EQ.1) .OR.
     &                 (nnuc.EQ.mt649 .AND. nejc.EQ.2) .OR.
     &                 (nnuc.EQ.mt849 .AND. nejc.EQ.3) ) THEN
                     WRITE (12,
     &              '(4x,''Lev #'',5x,''Integrated Discrete Spectra'')')
                     WRITE (12,
     &              '(10x,''    Energy    Int-DDX[mb]   Elev'')')
                     WRITE (12,*) ' '
	               htmp = 0.d0
                     DO il = 2, NLV(nnuc)  ! discrete levels
                       espec = (EMAx(nnuc) - ELV(il,nnuc))/recorp
                       IF (espec.LT.0) cycle 
                       WRITE (12,'(4x,I3,4x,F10.5,2(E14.5,2x),F6.3)')  
     &                   il, -espec, check_DL(il)*recorp,
     &                           disc_int(il)*recorp,ELV(il,nnuc)
	               	   htmp = htmp + check_DL(il)*recorp
                     ENDDO
	               WRITE (12,*) ' '
                     WRITE (12,'(7X,''Integral of discrete-level DDXS '',
     &                G12.6,'' mb'')') htmp
                     WRITE (12,'(7X,''Population of discrete levels   '',
     &                G12.6,'' mb'')') pop_disc(nejc)
                     WRITE (12,*) ' '
                   ENDIF
                   WRITE (12,'(15x,''Integrated Emission Spectra (printe
     &d DDXS corrected) - consistency check,  Ein ='',F9.5,'' MeV, nejc 
     &= '',i1)') EINl, nejc
                   WRITE (12,'(10x,
     &             ''    Energy      mb/MeV   Int-DDX[mb/MeV]       Diff
     &           Diff[%]    '')')
                   WRITE (12,*) ' '
                   DO ie = 1, nspec 
                      htmp = POPcse(0,nejc,ie,INExc(nnuc))             
                      if(htmp.LE.0.d0) cycle
                      WRITE (12,'(10x,F10.5,4(E14.5,1x))') FLOAT(ie - 1)
     &                *DE/recorp, htmp*recorp, 
     &                check_DE(ie)*recorp,
     &                (htmp - check_DE(ie)) * recorp, 
     &                (htmp - check_DE(ie)) / htmp * 100
                   ENDDO
                                        ! exact endpoint
                   WRITE (12,'(10x,F10.5,4(E14.5,1x))') 
     &               EMAx(nnuc)/recorp,max(0.d0,POPcse(0,nejc,nspec+1,
     &               INExc(nnuc)))*recorp,
     &               check_DE(nspec+1)*recorp,
     &               ( max(0.d0,POPcse(0,nejc,nspec+1,INExc(nnuc))) - 
     &               	check_DE(nspec+1) )*recorp, 0.d0
                   WRITE(12,*) 
                   WRITE(12,'(10x,
     &                ''Integral of spectrum '',G12.6,'' mb'' )') dtmp
                   WRITE(12,'(10x,
     &                ''Emiss. cross section '',G12.6,'' mb'' )') 
     &                  CSEmis(nejc,INExc(nnuc))
                   WRITE(12,'(10x,
     &                ''Popul. cross section '',G12.6,'' mb'' )') 
     &                  POPcs(nejc,INExc(nnuc))

                   WRITE(12,*) 

                ELSE !  then (nejc.GT.0)
C
C------------------Exclusive DE spectra (gammas and light ions)
C------------------double the first bin x-sec to preserve integral in EMPEND
                   POPcse(0,nejc,1,INExc(nnuc)) =
     &                   POPcse(0,nejc,1,INExc(nnuc))*2
                   WRITE (12,*) ' '
                   WRITE (12,'(''    Energy    mb/MeV'')')
                   WRITE (12,*) ' '
                   dtmp =0.d0          
                   DO ie = 1, nspec     
                     htmp = POPcse(0,nejc,ie,INExc(nnuc))          
                     if(htmp.LE.0.d0) cycle
                     if(ie.gt.1) then
                       dtmp = dtmp + htmp*DE
                     else
                       dtmp = dtmp + htmp*DE*0.5d0
                     endif
                     WRITE (12,'(F10.5,E14.5)') FLOAT(ie - 1)*DE/recorp,
     &                  htmp*recorp
                   ENDDO
C                  if(htmp.GT.0.d0) then
                     dtmp = dtmp + 
     &                    POPcse(0,nejc,nspec+1,INExc(nnuc))*DE*0.5d0         
                                              !exact endpoint
                     WRITE (12,'(F10.5,E14.5)') EMAx(nnuc)/recorp,
     &                 max(0.d0,POPcse(0,nejc,nspec+1,
     &                 INExc(nnuc)))*recorp
C                  endif
                   WRITE(12,*) 
                   WRITE(12,'(2x,
     &                  ''Integral of spectrum '',G12.6,'' mb'' )') dtmp
                   WRITE(12,'(2x,
     &                  ''Emiss. cross section '',G12.6,'' mb'' )') 
     &                  CSEmis(nejc,INExc(nnuc))
                   WRITE(12,'(2x,
     &                  ''Popul. cross section '',G12.6,'' mb'' )') 
     &                  POPcs(nejc,INExc(nnuc))

                   WRITE(12,*) 

                ENDIF !  (nejc.GT.0)
 1530         ENDDO   ! over ejectiles

              IF ((A(1)-A(nnuc)).GT.1  .AND. RECoil.GT.0) THEN
                 CALL PRINT_RECOIL(nnuc,REAction(nnuc))
              ELSEIF((A(1)-A(nnuc)).EQ.1  .AND. RECoil.GT.0) THEN
                 CALL PRINT_BIN_RECOIL(nnuc,REAction(nnuc))
              ENDIF
           ENDIF ! IF (CSPrd(nnuc).GT.0.0D0)
         ENDIF ! IF (ENDf(nnuc).EQ.1)

C     ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
C        PFNS calculations for a given decaying nucleus A(Nnuc),Z(Nnuc)
C
         IF (FISSPE.gt.0 .and.
     &      TOTcsfis.gt.0.d0 .and. CSPfis(nnuc).GT.0.0D0 .and.
     &       ( Z(0).eq.Z(nnuc) .OR.   Z(0)-1.eq.Z(nnuc) ) .AND.
C           Prompt fission spectra are not calculated if:
C           Partial fission cross section is lower than 1.d-7*TOTcsfis
     &      CSPfis(Nnuc).GT.1.d-7*TOTcsfis) THEN
C
C           Only PFNS of the neutron chain are considered
C           Checked if Z of the fissioning nucleus
C             is equal to the target, if not PFNS skipped  
            IF (NINT(Z(0)) .NE. NINT(Z(nnuc))) CYCLE

            iaf = A(nnuc)
            izf = Z(nnuc)
C
            nejc = 1 ! neutron emission only
            post_fisn  = 0.d0
            ratio2maxw =  0.d0
            emiss_en   = 0.d0

            IF(nfission.eq.0) THEN
C
C           First fissioning nucleus in the isotope chain
C
C             Calculating unique energy grid 
C
C             Assumed maximum energy of neutrons from fragments will be 25 MeV

              deltae_pfns = 0.1d0 

              nepfns = min( NINT(25.d0/deltae_pfns) + 1, NDEPFN)
              enepfns(1) = 1.d-11
              DO ie = 2, nepfns  
                enepfns(ie) = FLOAT(ie - 1)*deltae_pfns
              ENDDO
C
C             Special grid for printing, ONLY VALID for deltae_pfns = 0.1d0 
C
              csetmp(1) = enepfns(1)
              csetmp(2) = 4.d-11
              csetmp(3) = 7.d-11
              csetmp(4) = 1.d-10
              csetmp(5) = 4.d-10
              csetmp(6) = 7.d-10
              csetmp(7) = 1.d-9
              csetmp(8) = 4.d-9
              csetmp(9) = 7.d-9
              csetmp(10) = 1.d-8
              csetmp(11) = 4.d-8
              csetmp(12) = 7.d-8
              csetmp(13) = 1.d-7
              csetmp(14) = 4.d-7
              csetmp(15) = 7.d-7
              csetmp(16) = 1.d-6
              csetmp(17) = 4.d-6
              csetmp(18) = 7.d-6
              csetmp(19) = 1.d-5
              csetmp(20) = 4.d-5
              csetmp(21) = 7.d-5
              csetmp(22) = 1.d-4
              csetmp(23) = 4.d-4
              csetmp(24) = 7.d-4
              csetmp(25) = 1.d-3
              csetmp(26) = 4.d-3
              csetmp(27) = 7.d-3
              csetmp(28) = 1.d-2
              csetmp(29) = 4.d-2
              csetmp(30) = 7.d-2
              csetmp(31) = enepfns(2)

C             Below first chance, no emissive contributions to fission spectra
C             Only fission neutrons emitted from fully accelerated fragments
C             Initializing the pseudo incident energy
              eincid = EXCn - Q(1,1)  ! emitting from CN, nnuc = 1
C
C             The total nubar is calculated for the incident energy and 
C                used for the normalization of the total PFNS
              if(NUBarread) fniuEVAL = fniu_nubar_eval(EINl)
C
C             Models could be used for NUBAR calculations,
C             however, we prefer to normalize our PFNS to the 
C             evaluated NUBAR
C
C             if(fniuEVAL.eq.1) then
C               iafiss = A(1) 
C               IF(FISspe.eq.1) 
C    &            fnubar = fniuLANL(eincid,NINT(A(1)),NINT(Z(1)))
C               IF(FISspe.eq.2) 
C    &            fnubar = fniu    (eincid,NINT(A(1)),NINT(Z(1)))
C             endif 

              fnubar = fniuEVAL * PFNniu ! scaling of NUBAR

            ELSE

C             For higher emission chances, the corresponding  
C             neutron binding energy is substracted iteratively
C
              eincid = eincid - Q(1,nnuc)               

            ENDIF
C
C           If no more excitation energy available, then PFN emission stopped
            if(eincid.LT.0.d0) CYCLE

            WRITE (12,*)' '
            WRITE (12,*)' *******************************************'
            WRITE (12,*)' PROMPT FISSION NEUTRON SPECTRA calculations'

            WRITE ( 8,*) ' '
            WRITE ( 8,*)' *******************************************'
            WRITE ( 8,*)' PROMPT FISSION NEUTRON SPECTRA calculations'
            
            WRITE
     &        (12,'(''  Fiss. nucleus '',I3,''-'',A2,''-'',I3)')
     &            INT(Z(nnuc)), SYMb(nnuc), INT(A(nnuc))
            WRITE
     &     (12,'(''  Partial fission cross section '',G12.5,'' mb'')')
     &            CSPfis(Nnuc)
            WRITE
     &     (12,'(''  Ratio of partial to total fission '',G12.5)')
     &            CSPfis(Nnuc)/TOTcsfis
            WRITE
     &     (12,'(''  Binding energy of fissioning nucleus '',
     &            G12.5,'' MeV'')') Q(1,nnuc)

            WRITE
     &        (8,'(''  Fiss. nucleus '',I3,''-'',A2,''-'',I3)')
     &            INT(Z(nnuc)), SYMb(nnuc), INT(A(nnuc))
            WRITE(8,'(1x,a11,i2,a7,f8.3,A4)')  
     &      ' Fiss. Nucl ',nfission + 1, ', Uexc=',eincid + Q(1,nnuc), 
     &      ' MeV'
            WRITE
     &     (8,'(''  Partial fission cross section '',G12.5,'' mb'')')
     &            CSPfis(Nnuc)
            WRITE
     &     (8,'(''  Ratio of partial to total fission '',G12.5)')
     &            CSPfis(Nnuc)/TOTcsfis
            WRITE
     &     (8,'(''  Binding energy of fissioning nucleus '',
     &            G12.5,'' MeV'')') Q(1,nnuc)

C-----------Calculating post-fission neutrons in the first chance
C
C           Los Alamos model  
C
            IF (FISspe.eq.1)
     &        CALL get_fragmPFNS_LANL (post_fisn, enepfns, nepfns,
     &         eincid, A(Nnuc), Z(Nnuc), eneutr, tequiv, Q(1,nnuc)
     &          , deltae_pfns, PFNtke, PFNrat, PFNalp, PFNere)
C
C           Kornilov parameterization
C
            IF (FISspe.eq.2)
     &        CALL get_fragmPFNS      (post_fisn, enepfns, nepfns,
     &         eincid, A(Nnuc), Z(Nnuc), eneutr, tequiv, Q(1,nnuc)
     &          , deltae_pfns, PFNtke, PFNrat, PFNalp, PFNere)

            fnorm = CSPfis(nnuc)/TOTcsfis
            if(eneutr.gt.0) then
              DO ie = 1, nepfns
                   ftmp = fmaxw(enepfns(ie),tequiv)
                   if (ftmp.gt.0) ratio2maxw(ie) = post_fisn(ie)/ftmp
C------------------Accumulating total spectrum for neutrons
                   csepfns(ie) = csepfns(ie) + post_fisn(ie)*fnorm
C
C                  CSEfis contains the n,xnf spectra, not being used for now
C                  CSEfis is initialized inside HF-comp.f (EXCLUSIVE..)
C                  csepfns(ie) = csepfns(ie) + CSEfis(ie,nejc,nnuc)
C
              ENDDO

              WRITE ( 8,
     &       '(''  Postfission <En> '', G12.5,'' MeV'')') eneutr
              WRITE ( 8,
     &       '(''  Equivalent Tmaxwell '',G12.5,'' MeV'')') tequiv
              WRITE (8,*) ' '

            else

              WRITE  (8 ,'(''  No fission neutrons emitted'')')

            endif
               
C           WRITE (12,*) ' '
            nfission = nfission + 1

         ENDIF  !  IF ( TOTcsfis > 0 & CSPfis(nnuc)> 0 & Z(0)==Z(nnuc) )

      ENDDO  ! over decaying nuclei

C-----
C-----PRINTING TOTAL PFNS and PFNM quantities
C-----
      IF (FISspe.gt.0 .and. TOTcsfis.gt.0.d0) THEN

        ftmp = 0.D0
        eneutr = 0.d0
        do ie =2, nepfns
          fmed = 
     &    (csepfns(ie)+csepfns(ie-1))*0.5*(enepfns(ie)-enepfns(ie-1))
          eneutr = eneutr + fmed*(enepfns(ie)+enepfns(ie-1))*0.5d0
          ftmp = ftmp + fmed
        enddo
        if(ftmp.GT.0) eneutr = eneutr/ftmp
C
C       tequiv = 2.D0/3.D0*eneutr
C
        tequiv = TMAxw  ! Maxwellian temperature used to scale plots defined in input
C                       ! Default value 1.32 MeV

        WRITE(74,'(1X,g12.5,1x,g12.5,2(4x,f7.3))')
     &        EINl, eneutr, fnubar, tequiv 

        WRITE
     & (73,'(''  Total PFNS from '',I3,''-'',A2,''-'',I3,'': Elab='',
     &  G12.5,'' MeV, <Epfns>='',G12.5,'' MeV, Tmaxw='',f8.4,
     & '' MeV, Norm='',F10.8)') INT(Z(1)), SYMb(1), INT(A(1)), 
     & EINl,eneutr,tequiv,ftmp

        WRITE(8,*)
        WRITE(8,*) ' ***'
        WRITE(8,*)
        WRITE ( 8,'(''  Total PFNS  for  Elab='',
     &     G12.5,'' MeV,   Norm='',F10.8)') EINl, ftmp
        WRITE(8,*)

        WRITE ( 8,'(''  Number of fissioning nuclei '',I3)') nfission
        WRITE ( 8,'(''  Total PFNS average energy  '',G12.5,A5)') eneutr
     &   ,'  MeV'
        WRITE ( 8,'(''  Tmaxwell for plot = '',G12.5,A5)') tequiv
     &   ,'  MeV'
        WRITE ( 8,'(''  Multiplicity (nue) '',F6.3)') fnubar
        if(fnubar.ne.1) 
     &         WRITE ( 8,'(''  Nubar from evaluated library '')')
        if(fnubar.ne.1 .and. PFNniu. ne. 1) 
     &         WRITE ( 8,'(''  Nubar scaled by '',f6.4)') PFNniu
        WRITE(8,*)

        WRITE(12,*)
        WRITE(12,*) ' ***'
        WRITE(12,*)
        WRITE (12,'(''  Total PFNS  for  Elab='',
     &  E10.4,'' MeV, Norm='',F10.8)') EINl, ftmp

        WRITE (12,'(''  Number of fissioning nuclei '',I3)') nfission
        WRITE (12,'(''  Total PFNS average energy  '',G12.5,A5)') eneutr
     &   ,'  MeV'
        WRITE (12,'(''  Tmaxwell for plot = '',G12.5,A5)') tequiv
     &   ,'  MeV'
        WRITE (12,'(''  Normalization '',F12.9)') ftmp
        WRITE (12,'(''  Multiplicity (nue) '',F6.3)') fnubar
        if(fnubar.ne.1) 
     &        WRITE (12,'(''  Nubar from evaluated library '')')
        if(fnubar.ne.1 .and. PFNniu. ne. 1) 
     &        WRITE (12,'(''  Nubar scaled by '',f6.4)') PFNniu
        WRITE(12,*)

        WRITE ( 8,*) ' Spectrum of ','neutrons ', '(z,fission) '
        WRITE ( 8,*) ' '
        WRITE ( 8,'(''    Energy    mb/MeV       Ratio to Maxw'')')
        WRITE ( 8,*) ' '

        WRITE (12,*) ' Spectrum of ','neutrons ', '(z,fission) '
        WRITE (12,*) ' '
        WRITE (12,'(''    Energy    mb/MeV       Ratio to Maxw'')')
        WRITE (12,*) ' '

        WRITE (73,'(/,''    Energy    mb/MeV       Ratio to Maxw'')')

C----------
C       calculating the ratio for the 1st point for interpolation of the outgoing grid
        ftmp  = fmaxw(enepfns(1),tequiv)
        ftmpA = 1.d0
        if (ftmp.gt.0) ftmpA = csepfns(1)/ftmp

C       calculating the ratio for the 2nd point for interpolation of the outgoing grid
        ftmp  = fmaxw(enepfns(2),tequiv)
        ftmpB = 1.d0
        if (ftmp.gt.0) ftmpB = csepfns(2)/ftmp

C       Interpolating and printing between the first and the second point of the 
C             outgoing energy grid enepfns(1) and enepfns(2)
        DO ie = 1, 31
          ftmp1 = ftmpA +  (ftmpB - ftmpA) *
     &     (csetmp(ie) - csetmp(1))/(csetmp(31) - csetmp(1))
         
          ftmp = ftmp1*fmaxw(csetmp(ie),tequiv)

          WRITE (12,'(E10.4,E14.5,2x,E14.5)')
     &      csetmp(ie), ftmp/deltae_pfns, ftmp1
          WRITE (73,'(E11.4,E14.5,4(2x,E14.5))')
     &      csetmp(ie), ftmp/deltae_pfns, ftmp1
          WRITE ( 8,'(E11.4,E14.5,2x,E14.5)')
     &      csetmp(ie), ftmp/deltae_pfns, ftmp1
        ENDDO

        DO ie = 3, nepfns 
          ftmp  = fmaxw(enepfns(ie),tequiv)
          ftmp1 = 1.d0
          if (ftmp.gt.0) ftmp1 = csepfns(ie)/ftmp

          WRITE (12,'(E10.4,E14.5,2x,E14.5)')
     &      enepfns(ie), csepfns(ie)/deltae_pfns, ftmp1
          WRITE (73,'(E11.4,E14.5,4(2x,E14.5))')
     &      enepfns(ie), csepfns(ie)/deltae_pfns, ftmp1
          IF(enepfns(ie).GT.7.d0) cycle
          WRITE ( 8,'(E11.4,E14.5,2x,E14.5)')
     &      enepfns(ie), csepfns(ie)/deltae_pfns, ftmp1
        ENDDO
C       WRITE (12,'(E10.4,E14.5,2x,E14.5)')
C    &     enepfns(nepfns), 0.d0, 0.d0
        WRITE(8,*) '...   PFNS Output supressed for Epfns > 7 MeV '
        WRITE (12,*)
      ENDIF
      
 4536 CONTINUE
C  
C     end of PFNS calculations 
C     ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

C-----
C-----ENDF spectra inclusive representation
C-----
C-----NOTE: transformation into CM
C-----(reduce channel energy to account for recoils)
C-----is not yet done! Should be performed in ACCUM and EXCLUSIVEC/L
C-----NOTE: HMS cumulative spectra (if calculated) are already
C-----stored in CSE(.,x,0) array
C-----
C-----Calculate double-differential spectra
C     DO nejc = 0, NDEJC
C          DO iesp = 1, NDECSE
C            DO nang = 1, NDANG
C               CSEa(iesp,nang,nejc,0)
C    &             = ((CSE(iesp,nejc,0)
C    &             - CSEmsd(iesp,nejc)*POPcseaf(0,nejc,iesp,0))
C    &             /4.0/PI      + CSEa(iesp,nang,nejc,1)
C    &             *POPcseaf(0,nejc,iesp,0))
C            ENDDO
C         ENDDO
C     ENDDO
      WRITE (8,*) ' '
C     WRITE (8,*)

      IF (IOUt.GT.5 .AND. .NOT.EXClusiv ) THEN

         IF (FIRst_ein) THEN 
	      WRITE (8,*)
	      WRITE (8,*)
            WRITE (8,'(11X,''**********************'')')
            WRITE (8,'(11x,'' Total spectra (C.M.)'')')
            WRITE (8,'(11x,''**********************'')')
            DO nejc = 0, NEJcm
              csemax = 0.d0
              ftmp = 0.d0
              DO i = 1, NDEX
                csemax = DMAX1(CSEt(i,nejc),csemax)
                ftmp   = ftmp + CSEt(i,nejc)
              ENDDO
              if(csemax.le.0.01d0 .or. ftmp.le.0.0001d0) cycle 

              CALL Print_Total(nejc)
              CALL PLOT_TOTAL_EMIS_SPECTRA(nejc)

              csum = 0.d0
              DO nnuc = 1, NNUcd
		      csum = csum + CSEmis(nejc,nnuc)
              ENDDO
	        if(nejc.ne.0) then
               WRITE (8,
     &         '(2X,A2,'' emission cross section'',G12.6,''  mb '',A7)')
     &         SYMbe(nejc), csum, '(TOTAL)'  
              else
               WRITE (8,
     &         '(2X,A2,'' emission cross section'',G12.6,''  mb '',A7)')
     &         ' g', csum, '(TOTAL)'  
              endif
            ENDDO

         ENDIF  
      ENDIF
      WRITE (8,*)
      WRITE (8,*)
      checkXS = checkXS + TOTcsfis

      xsdirect = SINlcc*FCCred + SINl*FCCred + SINlcont*FCOred
      xspreequ = xsinl + xsmsc + totemis + tothms

C     for complex projectiles: reaction, BU,NT,PE,CN
      IF(ltransfer .or. lbreakup)
     &   WRITE(114,'(1P,E11.4,1x,1P,5E13.5)') EINl, CSFus,
     &   crossBUt,crossNTt, csprd(4),CSFus-crossBUt-crossNTt-crossPEt

C
C     Elastic and Nonelastic modified for actinides
C     to include/exclude scattering cross section (xscclow) low-lying coupled states

      IF (A(0).gt.220 .AND. ZEJc(0).EQ.0) then 

C        WRITE(41,'(''#'',A10,1X,1P,95A12)') '  Einc    ',
C    &      '  Total     ','  Elastic*  ','  Nonelast* ',
C    &      '  Fission   ','  Mu-bar    ','  Nu-bar    ',
C    &         (preaction(nnuc),nnuc=1,min(nuc_print,max_prn))

        IF(INT(AEJc(0)).GT.0) THEN  ! particles

          WRITE(41,'(1P,E10.4,1x,1P,95E12.5)') 
     &    EINl, TOTcs*TOTred*totcorr,
C                          Low-lying XS   and       CE         added to elastic
     &    ELAcs*ELAred  +   xscclow       +    4.d0*PI*ELCncs, 
     &    TOTcs*TOTred*totcorr - (ELAcs*ELAred+xscclow+4.d0*PI*ELCncs),
     &    TOTcsfis, 
     &    mu_bar(amass(0),NANgela,ELAred,cel_da,elada),xnub,
     &     CSPrd(1), csinel,(CSPrd(nnuc),nnuc=3,min(nuc_print,max_prn))

          WRITE(107,'(1P,E10.4,1x,1P,95E12.5)') EINl, 
     &    TOTcs*TOTred*totcorr,                           !total = reaction + shape-el
     &    ELAcs*ELAred  +  4.d0*PI*ELCncs,                !CE added to elastic 
     &                     4.d0*PI*ELCncs,                !CN_el
     &    ELAcs*ELAred                   ,                !shape elastic 
     &    TOTcs*TOTred*totcorr - (ELAcs*ELAred + 4.d0*PI*ELCncs),
     &    CSFus*corrmsd - tothms - xsmsc,                 !CN-formation 
     &    xsdirect, xspreequ,                             !direct, preequil
     &    SINlcc*FCCred, SINl*FCCred, SINlcont*FCOred,    !CC_inl,DWBA_dis,DWBA_cont  
     &    xsinl,xsmsc,totemis, tothms, xscclow            !MSD,MSC,PCROSS,HMS,xscclow(2 CC levels)
        
        ELSE ! photon-induced

          WRITE(41,'(1P,E10.4,1x,1P,95E12.5)') 
     &    EINl, TOTcs*TOTred*totcorr,
     &    ELAcs*ELAred , TOTcs*TOTred*totcorr - ELAcs*ELAred, TOTcsfis, 
     &    mu_bar(amass(0),NANgela,ELAred,cel_da,elada),xnub,
     &     CSPrd(1), csinel,(CSPrd(nnuc),nnuc=3,min(nuc_print,max_prn))

          WRITE(107,'(1P,E10.4,1x,1P,95E12.5)') EINl, 
     &    TOTcs*TOTred*totcorr,                           !total = reaction + shape-el
     &    ELAcs*ELAred,   !+4.d0*PI*ELCncs                !shape elastic
     &    4.d0*PI*ELCncs,                                 !CN_el
     &    ELAcs*ELAred,                                   !shape elastic 
     &    TOTcs*TOTred*totcorr - ELAcs*ELAred,
     &    CSFus*corrmsd - tothms - xsmsc,                 !CN-formation 
     &    xsdirect, xspreequ,                             !direct, preequil
     &    SINlcc*FCCred, SINl*FCCred, SINlcont*FCOred,    !CC_inl,DWBA_dis,DWBA_cont  
     &    xsinl,xsmsc,totemis, tothms, xscclow            !MSD,MSC,PCROSS,HMS,xscclow(2 CC levels)

        ENDIF

      ELSE

        IF (ZEJc(0).EQ.0) then 
  
          IF(INT(AEJc(0)).GT.0) THEN  ! particles

            WRITE(41,'(1P,E10.4,1x,1P,95E12.5)')
     &      EINl,TOTcs*TOTred*totcorr,
     &      ELAcs*ELAred             + 4.d0*PI*ELCncs,      ! CE added to elastic
     &      TOTcs*TOTred*totcorr - ELAcs*ELAred 
     &                             - 4.d0*PI*ELCncs,      ! CE substracted from nonelastic
     &      TOTcsfis, mu_bar(amass(0),NANgela,ELAred,cel_da,elada),xnub,
     &      CSPrd(1), csinel,(CSPrd(nnuc),nnuc=3,min(nuc_print,max_prn))

            WRITE(107,'(1P,E10.4,1x,1P,95E12.5)') EINl, 
     &      TOTcs*TOTred*totcorr,                           !total = reaction + shape-el
     &      ELAcs*ELAred  +  4.d0*PI*ELCncs,                !CE added to elastic 
     &                     4.d0*PI*ELCncs,                !CN_el
     &      ELAcs*ELAred                   ,                !shape elastic 
     &      TOTcs*TOTred*totcorr - (ELAcs*ELAred + 4.d0*PI*ELCncs),
     &      CSFus*corrmsd - tothms - xsmsc,                 !CN-formation 
     &      xsdirect, xspreequ,                             !direct, preequil
     &      SINlcc*FCCred, SINl*FCCred, SINlcont*FCOred,    !CC_inl,DWBA_dis,DWBA_cont  
     &      xsinl,xsmsc,totemis, tothms, xscclow            !MSD,MSC,PCROSS,HMS,xscclow(2 CC levels)

          ELSE ! photon-induced

            WRITE(41,'(1P,E10.4,1x,1P,95E12.5)')
     &      EINl,TOTcs*TOTred*totcorr,
     &      ELAcs*ELAred,                   
     &      TOTcs*TOTred*totcorr - ELAcs*ELAred,
     &      TOTcsfis, mu_bar(amass(0),NANgela,ELAred,cel_da,elada),xnub,
     &      CSPrd(1), csinel,(CSPrd(nnuc),nnuc=3,min(nuc_print,max_prn))

            WRITE(107,'(1P,E10.4,1x,1P,95E12.5)') EINl, 
     &      TOTcs*TOTred*totcorr,                           !total = reaction + shape-el
     &      ELAcs*ELAred,                                   !elastic
     &      4.d0*PI*ELCncs,                                 !CN_el
     &      ELAcs*ELAred,                                   !shape elastic 
     &      TOTcs*TOTred*totcorr - (ELAcs*ELAred+4.d0*PI*ELCncs),
     &      CSFus*corrmsd - tothms - xsmsc,                 !CN-formation 
     &      xsdirect, xspreequ,                             !direct, preequil
     &      SINlcc*FCCred, SINl*FCCred, SINlcont*FCOred,    !CC_inl,DWBA_dis,DWBA_cont  
     &      xsinl,xsmsc,totemis, tothms, xscclow            !MSD,MSC,PCROSS,HMS,xscclow(2 CC levels)

          ENDIF

        ELSE ! charged-particles

          WRITE(41,'(1P,E10.4,1x,1P,95E12.5)')EINl,TOTcs*TOTred*totcorr,
     &    ELAcs*ELAred             + 4.d0*PI*ELCncs,      !CN_el (CE) added to elastic 
C
C         Total is 0 for charged particles, no correction
C    &    TOTcs*TOTred*totcorr - (ELAcs*ELAred + 4.d0*PI*ELCncs),
C
     &    CSFus + (SINl+SINlcc)*FCCred + SINlcont*FCOred 
     &                             - 4.d0*PI*ELCncs, ! CE substracted from nonelastic
C
     &    TOTcsfis, mu_bar(amass(0),NANgela,ELAred,cel_da,elada),xnub,
     &    CSPrd(1), csinel,(CSPrd(nnuc),nnuc=3,min(nuc_print,max_prn))
C
          WRITE(107,'(1P,E10.4,1x,1P,95E12.5)') EINl, 
     &    TOTcs*TOTred*totcorr,                           !total = reaction + shape-el
     &    ELAcs*ELAred   + 4.d0*PI*ELCncs,                !CN_el (CE) added to elastic 
     &                     4.d0*PI*ELCncs,                !CN_el
     &    ELAcs*ELAred                   ,                !shape elastic 
C
C         Total is 0 for charged particles, no correction
C    &    TOTcs*TOTred*totcorr - (ELAcs*ELAred + 4.d0*PI*ELCncs),
C
     &    CSFus + (SINl+SINlcc)*FCCred + SINlcont*FCOred 
     &                             - 4.d0*PI*ELCncs,      ! CE substracted from nonelastic
C
     &    CSFus*corrmsd - tothms - xsmsc,                 !CN-formation 
     &    xsdirect, xspreequ,                             !direct, preequil
     &    SINlcc*FCCred, SINl*FCCred, SINlcont*FCOred,    !CC_inl,DWBA_dis,DWBA_cont  
     &    xsinl,xsmsc,totemis, tothms, xscclow            !MSD,MSC,PCROSS,HMS,xscclow(2 CC levels)
        ENDIF 

      ENDIF

      IF(ABScs.GT.0.) THEN
        WRITE (8,'('' ********************************************'',
     &           23(1H*))')
        WRITE (8,'('' * Incident energy (LAB): '',G12.5,
     &              '' MeV  '')') EINl
        IF (INT(ZEJc(0)).EQ.0) THEN
          WRITE (8,
     &  '('' * Total cross section                            '',G13.6,
     &              '' mb  '')') TOTcs*TOTred*totcorr
C    &              '' mb  '')') CSFus + (SINl+SINlcc)*FCCred +
C    &    SINlcont*FCOred + ELAred*ELAcs  = TOTcs*TOTred*totcorr

          WRITE (8,
     &  '('' * OM total cross section                         '',G13.6,
     &              '' mb  '')') ELAred*ELAcs + 
     &        (ABScs - (SINl+SINlcc+SINlcont))*FUSred+
     &        (SINl+SINlcc)*FCCred + SINlcont*FCOred 
          WRITE (8,
     &  '('' * Shape Elastic cross section (ELAcs)            '',G13.6,
     &              '' mb  '')') ELAred*ELAcs
        ENDIF

        WRITE (8,
     &  '('' * OM nonelastic cross section (ABScs)            '',G13.6,
     &              '' mb  '')')
     &   (ABScs - (SINl+SINlcc+SINlcont))*FUSred+
     &     (SINl+SINlcc)*FCCred + SINlcont*FCOred 
        WRITE (8,
     &  '('' * Nonelastic cross section                       '',G13.6,
     &              '' mb  '')')
     &   CSFus + (SINl+SINlcc)*FCCred + SINlcont*FCOred
        WRITE (8,
     &  '('' * Production cross section (incl.fission)        '',G13.6,
     &              '' mb'')')  checkXS
        IF(FISsil(1)) WRITE (8,
     &  '('' * Fission cross section                          '',G13.6,
     &              '' mb  '')') TOTcsfis
        WRITE (8,'('' * Difference: '', F7.2, '' mb ('',F6.2,'') %'')')
     &    CSFus + (SINl+SINlcc)*FCCred + SINlcont*FCOred - checkXS,
     &    100.d0*abs(
     &    ( CSFus + (SINl+SINlcc)*FCCred + SINlcont*FCOred - checkXS ))/
     &    ( CSFus + (SINl+SINlcc)*FCCred + SINlcont*FCOred)
        IF (INT(ZEJc(0)).EQ.0 .and. INT(AEJc(0)).GT.0 .and. 
     *      ELCncs.gt.0.1d0) THEN          
           WRITE (8,
     &  '('' * Compound elastic cross section (CE)            '',G13.6,
     &              '' mb  '')') 4.d0*PI*ELCncs
        ELSE
          WRITE (8,'('' * '')') 
        ENDIF
        IF (INT(ZEJc(0)).EQ.0) THEN
          IF(TOTred.ne.1)
     &    WRITE (8,'('' * Total         cross section scaled by '',
     &     G13.6)') TOTred
          IF(totcorr.gt.0) THEN
            IF( abs(1.d0/totcorr - TOTred0).gt.0.001d0) THEN
              WRITE (108,'(2x,G12.5,3x,F10.6)') EINl, 1.d0/totcorr  
              WRITE (8,'('' *   set TOTRED '' , F13.6,
     &         '' to keep unchanged total'')') 1.d0/totcorr
            ENDIF  
          ENDIF
        ENDIF 
        IF(FUSred.ne.1)
     &    WRITE (8,'('' * Reaction      cross section scaled by '',
     &     G13.6)') FUSred
        IF (INT(ZEJc(0)).EQ.0 .AND. ELAred.ne.1)
     &    WRITE (8,'('' * Shape Elastic cross section scaled by '',
     &     G13.6)') ELAred
        IF (INT(ZEJc(0)).EQ.0 .AND. CELred.ne.1)
     &    WRITE (8,'('' * Comp. Elastic cross section scaled by '',
     &     G13.6)') CELred
        if(FCCred.ne.1)
     &    WRITE (8,'('' * Disc.lev. DIR cross section scaled by '',
     &     G13.6)') FCCred
        if(FCOred.ne.1)
     &    WRITE (8,'('' * Cont.lev. DIR cross section scaled by '',
     &      G13.6)') FCOred

        if (INT(ZEJc(0)).EQ.0) then
            DO i=1,NLV(0) ! loop over target discrete levels
            IF(CINred(i).NE.1) WRITE (8,
     >       '('' * Comp. inelastic cross section for target level # '',
     &       i2,'' scaled by '',G13.6)') i, CINred(i)
            ENDDO
        endif
        WRITE (8,'('' ********************************************'',
     &           23(1H*))')

        IF (INT(ZEJc(0)).EQ.0) THEN
          WRITE (*,
     &  '(''   Total cross section                            '',G13.6,
     &              '' mb  '')') TOTcs*TOTred*totcorr
C    &              '' mb  '')') CSFus + (SINl+SINlcc)*FCCred +
C    &    SINlcont*FCOred + ELAred*ELAcs  = TOTcs*TOTred*totcorr

          WRITE (*,
     &  '(''   Shape Elastic cross section (ELAcs)            '',G13.6,
     &              '' mb  '')') ELAred*ELAcs
        ENDIF
        WRITE (*,
     &  '(''   Nonelastic cross section                       '',G13.6,
     &              '' mb  '')')
     &   CSFus + (SINl+SINlcc)*FCCred + SINlcont*FCOred
        WRITE (*,
     &  '(''   Production cross section (incl.fission)        '',G13.6,
     &              '' mb'')')  checkXS
        IF(FISsil(1)) WRITE (*,
     &  '(''   Fission cross section                          '',G13.6,
     &              '' mb  '')') TOTcsfis
        WRITE (*,'(''   Difference: '', F7.2, '' mb ('',F6.2,'') %'')')
     &    CSFus + (SINl+SINlcc)*FCCred + SINlcont*FCOred - checkXS,
     &    100.d0*abs(
     &    ( CSFus + (SINl+SINlcc)*FCCred + SINlcont*FCOred - checkXS ))/
     &    ( CSFus + (SINl+SINlcc)*FCCred + SINlcont*FCOred)
        IF (INT(ZEJc(0)).EQ.0 .and. INT(AEJc(0)).GT.0 .and. 
     &      ELCncs.gt.0.1d0) THEN
          WRITE (*,
     &  '(''   Compound elastic cross section (CE)            '',G13.6,
     &              '' mb  '')') 4.d0*PI*ELCncs
        ELSE
          WRITE (*,*)
        ENDIF
      ENDIF
      IF(abs(CSFus + (SINl+SINlcc)*FCCred + SINlcont*FCOred - checkXS)
     &  .GT.0.01*(CSFus + (SINl+SINlcc)*FCCred + SINlcont*FCOred)) THEN
        WRITE (8,*)
        WRITE (8,'(''  WARNING: Sum of production XS (incl.fission)'')')
        WRITE (8,'(''  WARNING: is not equal reaction cross section'')')
        IF((CSFus + (SINl+SINlcc)*FCCred + SINlcont*FCOred).NE.0.d0)
     &  WRITE (8,'(''  WARNING:     difference: '', F6.2,'' % at E = '',
     &  G12.5,'' MeV'')') 100.d0*
     &   abs(CSFus + (SINl+SINlcc)*FCCred + SINlcont*FCOred - checkXS)/
     &           (CSFus + (SINl+SINlcc)*FCCred + SINlcont*FCOred),EINl
      ENDIF
      IF(TOTred*TOTcs*totcorr.gt.0.d0 .and.
     &     abs(CSFus + (SINl+SINlcc)*FCCred + SINlcont*FCOred + 
     &     ELAred*ELAcs - TOTred*TOTcs*totcorr) .GT.
     &                0.01*TOTred*TOTcs*totcorr) THEN
        WRITE (8,*)
        WRITE (8,'(''  WARNING: Total cross section is NOT equal'')')
        WRITE (8,'(''  WARNING: Elastic + Absorption cross section'')')
        WRITE (8,'(''  WARNING:     difference: '', F6.2,'' % at E = '',
     &  G12.5,'' MeV'')') 100.d0*
     &    abs(ABScs + ELAred*ELAcs - TOTred*TOTcs*totcorr)/
     &                 (TOTred*TOTcs*totcorr),EINl
      ENDIF
      WRITE (8,*)
      WRITE (8,*) '+++ end of one energy +++'
      WRITE (8,*) 
C-----
C-----ENDF spectra printout (inclusive representation)
C-----
      IF (.NOT.EXClusiv) THEN
         WRITE (12,*) 
         WRITE (12,*) '********************************************'
         WRITE (12,*) '* INCLUSIVE SPECTRA at Einc =', sngl(EINl) 
         WRITE (12,*) '********************************************'
         WRITE (12,*)    
 
C--------Print spectra of residues
         reactionx = '(z,x)  '
         DO nnuc = 1, NNUcd    !loop over decaying nuclei
            IF (ENDf(nnuc).EQ.2 .AND. RECoil.GT.0)
     &        CALL PRINT_RECOIL(nnuc,reactionx)
         ENDDO !over decaying nuclei in ENDF spectra printout
C
         WRITE (12,*) ' '    
C--------Print inclusive spectra of gamma and ejectiles
         DO nejc = 0, NEJcm
	     CALL Print_Inclusive(nejc)
	   ENDDO
         WRITE (12,*) ' '    
	ENDIF

C  Summary of exclusive emission cross sections
      jnmx = 0
      jzmx = 0
      jfiss = 0
      DO jn = 0, 20
        DO jz = 0, 15
          IF(xcross(NDEJC+1,jz,jn).GT.0.01d0) jfiss = 1
          IF(xcross(NDEJC+2,jz,jn).GT.0.01d0) THEN
            jnmx = MAX(jnmx,jn)
            jzmx = MAX(jzmx,jz)
          ENDIF
        ENDDO
      ENDDO
      IF(jnmx.gt.1 .AND. jzmx.gt.1) THEN
        iz = INT(Z(1))
        ia = INT(A(1))-iz
        WRITE (12,*) ' '
        WRITE (12,*) ' Gamma emission cross sections (mb) ZAP=    0'
        WRITE (12,*) ' '
        WRITE (12,'(''  Z / N '',20(i6,2x))') (ia-jn, jn = 0,jnmx)
        DO jz = 0, jzmx 
          WRITE(12,'(i5,2x,20f8.2)')iz-jz,(xcross(0,jz,jn), jn = 0,jnmx)
        ENDDO
        WRITE (12,*) ' '
        WRITE (12,*) ' '
        WRITE (12,*) ' Neutron emission cross sections (mb) ZAP=    1'
        WRITE (12,*) ' '
        WRITE (12,'(''  Z / N '',20(i6,2x))') (ia-jn, jn = 0,jnmx)
        DO jz = 0, jzmx 
          WRITE(12,'(i5,2x,20f8.2)')iz-jz,(xcross(1,jz,jn), jn = 0,jnmx)
        ENDDO
        WRITE (12,*) ' '
        WRITE (12,*) ' '
        WRITE (12,*) ' Proton emission cross sections (mb) ZAP= 1001'
        WRITE (12,*) ' '
        WRITE (12,'(''  Z / N '',20(i6,2x))') (ia-jn, jn = 0,jnmx)
        DO jz = 0, jzmx 
          WRITE(12,'(i5,2x,20f8.2)')iz-jz,(xcross(2,jz,jn), jn = 0,jnmx)
        ENDDO
        WRITE (12,*) ' '
        WRITE (12,*) ' '
        WRITE (12,*) ' Alpha emission cross sections (mb) ZAP= 2004'
        WRITE (12,*) ' '
        WRITE (12,'(''  Z / N '',20(i6,2x))') (ia-jn, jn = 0,jnmx)
        DO jz = 0, jzmx 
          WRITE(12,'(i5,2x,20f8.2)')iz-jz,(xcross(3,jz,jn), jn = 0,jnmx)
        ENDDO
        WRITE (12,*) ' '
        WRITE (12,*) ' '
        WRITE (12,*) ' Deuteron emission cross sections (mb) ZAP= 1002'
        WRITE (12,*) ' '
        WRITE (12,'(''  Z / N '',20(i6,2x))') (ia-jn, jn = 0,jnmx)
        DO jz = 0, jzmx 
          WRITE(12,'(i5,2x,20f8.2)')iz-jz,(xcross(4,jz,jn), jn = 0,jnmx)
        ENDDO
        WRITE (12,*) ' '
        WRITE (12,*) ' '
        WRITE (12,*) ' Triton emission cross sections (mb) ZAP= 1003'
        WRITE (12,*) ' '
        WRITE (12,'(''  Z / N '',20(i6,2x))') (ia-jn, jn = 0,jnmx)
        DO jz = 0, jzmx 
          WRITE(12,'(i5,2x,20f8.2)')iz-jz,(xcross(5,jz,jn), jn = 0,jnmx)
        ENDDO
        WRITE (12,*) ' '
        WRITE (12,*) ' '
        WRITE (12,*) ' Helium-3 emission cross sections (mb) ZAP= 2003'
        WRITE (12,*) ' '
        WRITE (12,'(''  Z / N '',20(i6,2x))') (ia-jn, jn = 0,jnmx)

        DO jz = 0, jzmx 
          WRITE(12,'(i5,2x,20f8.2)')iz-jz,(xcross(6,jz,jn), jn = 0,jnmx)
        ENDDO
        WRITE (12,*) ' '

        IF(NDEJC.EQ.7) THEN
          WRITE (12,*) ' '
          WRITE (12,
     &        '(I2,''-'',A2,'' emission cross sections (mb) ZAP='',I5)')
     &                 INT(AEJc(NDEJC)), SYMbe(NDEJC), IZAejc(NDEJC)
          WRITE (12,*) ' Ion emission cross sections (mb)'
          WRITE (12,*) ' '
          WRITE (12,'(''  Z / N '',20(i6,2x))') (ia-jn, jn = 0,jnmx)
          DO jz = 0, jzmx 
            WRITE(12,'(i5,2x,20f8.2)')iz-jz,
     &                                (xcross(NDEJC,jz,jn), jn = 0,jnmx)
           ENDDO
          WRITE (12,*) ' '
        ENDIF
        IF(jfiss.GT.0) THEN
          WRITE (12,*) ' '
          WRITE (12,*) ' Fission cross sections (mb)'
          WRITE (12,*) ' '
          WRITE (12,'(''  Z / N '',20(i6,2x))') (ia-jn, jn = 0,jnmx)
          DO jz = 0, jzmx 
            WRITE(12,'(i5,2x,20f8.2)')iz-jz,
     &                              (xcross(NDEJC+1,jz,jn), jn = 0,jnmx)
           ENDDO
          WRITE (12,*) ' '
        ENDIF
        WRITE (12,*) ' '
        WRITE (12,*) ' Initial populations (mb)'
        WRITE (12,*) ' '
        WRITE (12,'(''  Z / N '',20(i6,2x))') (ia-jn, jn = 0,jnmx)
        DO jz = 0, jzmx 
          WRITE(12,'(i5,2x,20f8.2)')iz-jz,
     &                              (xcross(NDEJC+3,jz,jn), jn = 0,jnmx)
        ENDDO
        WRITE (12,*) ' '
        WRITE (12,*) ' '

        totsum=0.d0
        DO jz = 0, jzmx 
          DO jn = 0, jnmx
            totsum = totsum + xcross(NDEJC+2,jz,jn)
          ENDDO
        ENDDO

        WRITE (12,*) ' Total production cross sections (mb) :', totsum
        WRITE (12,*) ' '
        WRITE (12,'(''  Z / N '',20(i6,2x))') (ia-jn, jn = 0,jnmx)
        DO jz = 0, jzmx 
          WRITE(12,'(i5,2x,20f8.2)')iz-jz,
     &                              (xcross(NDEJC+2,jz,jn), jn = 0,jnmx)
        ENDDO
        WRITE (12,*) ' '
       ENDIF        
C-----End of ENDF spectra (inclusive)
C
C-----S-FACTOR call
      s_factor = 0.d0

      IF(SFAct.GT.0) THEN
        IF(SFAct.EQ.1) s_factor = SFACTOR(CSPrd(1))  
        IF(SFAct.EQ.2) s_factor = SFACTOR(csinel)      
        IF(SFAct.EQ.3) s_factor = SFACTOR(CSPrd(3))    
      ENDIF
C        
 1155 IF( FITomp.GE.0 ) THEN
        lheader = .true.
 1156   READ (5,'(A36)',ERR=11570,END=1200) nextenergy

        IF(nextenergy(1:1).eq.'@') THEN 
          BACKSPACE 5
          READ (5,'(A72)',ERR=11570,END=1200) rtitle
          rtitle(1:1)=' '
          write(*,*) '***',trim(rtitle)
          WRITE( 8,*)
     &      '***************************************************'
          write( 8,*) '***',trim(rtitle)
          WRITE( 8,*)
     &      '***************************************************'
          GOTO 1156  ! next line
        ENDIF

        IF (nextenergy(1:1).EQ.'*' .OR. nextenergy(1:1).EQ.'#' 
     &   .OR. nextenergy(1:1) .EQ.'!') GOTO 1156

        IF(nextenergy(1:1).EQ.'$' .and. lheader) THEN
	    lheader = .false.
          WRITE (8,*) ' '
          WRITE (8,'(1x,61(''=''))')
          WRITE (8,
     &'('' Reaction '',A2,'' + '',I3,A2,
     &    '' :NEXT INCIDENT ENERGY STARTED'')') 
     &     SYMbe(0), INT(A(0)), SYMb(0)      
C         WRITE (8,'(1x,61(''=''))')
C         WRITE (8,*) ' '

          WRITE (12,*) ' '
          WRITE (12,'(1x,61(''=''))')
          WRITE (12,
     &'('' Reaction '',A2,'' + '',I3,A2,
     &    '' :NEXT INCIDENT ENERGY STARTED'')') 
     &     SYMbe(0), INT(A(0)), SYMb(0)      
C         WRITE (12,'(1x,61(''=''))')
C         WRITE (12,*) ' '
        ENDIF

        IF(nextenergy(1:1).EQ.'$') THEN
           READ(nextenergy,'(1x,A6,F10.5,4I5)',ERR=11570,END=1200) 
     &        keyname, val, ikey1, ikey2, ikey3, ikey4
           CALL OPTIONS(keyname, val, ikey1, ikey2, ikey3, ikey4, 1)
           GO TO 1156
        ENDIF

        BACKSPACE 5
        READ(5,*,ERR=11570,END=1200) EIN

        IF (EIN.gt.0.d0) then
          WRITE (8,
     &'('' Incident energy '',1P,D10.3, '' MeV (LAB)'')') EIN
          WRITE (8,'(1x,61(''=''))')
          WRITE (8,*)

          WRITE (12,
     &'('' Incident energy '',1P,D10.3, '' MeV (LAB)'')') EIN
          WRITE (12,'(1x,61(''=''))')
          WRITE (12,*) ' '

        ENDIF
      ELSE
        
 1158   READ (5,'(A36)',ERR=11570,END=1200) nextenergy
        IF (nextenergy(1:1).EQ.'*' .OR. nextenergy(1:1).EQ.'#' .OR. 
     &     nextenergy(1:1).EQ.'!' .OR. nextenergy(1:1).EQ.'$')GOTO 1158

        IF(nextenergy(1:1).eq.'@') THEN 
          BACKSPACE 5
          READ (5,'(A72)',ERR=11570,END=1200) rtitle
          rtitle(1:1)=' '
          write(*,*) '***',trim(rtitle)
          WRITE( 8,*)
     &      '***************************************************'
          write( 8,*)'***',trim(rtitle)
          WRITE( 8,*)
     &      '***************************************************'
          GOTO 1158  ! next line
        ENDIF

        BACKSPACE 5
        READ (5,*,ERR=11570,END=1200) EIN, NDAng, ICAlangs
        IF(NDAng.lt.2) THEN
            NDAng=2
            ANGles(1) = 0.d0
            ANGles(2) = 180.d0
        ELSE
            READ (5,*,ERR=11570,END=1200) (ANGles(na),na=1,NDAng)
        ENDIF
        NANGela=NDAng
        IF(NANgela.GT.NDAngecis) THEN
          WRITE(8,*)
     &        'ERROR: INCREASE NDANGECIS IN dimension.h UP TO ',NANgela
          STOP 'FATAL: INCREASE NDAngecis IN dimension.h'
        ENDIF
      ENDIF
      GOTO 1250
11570 write(8,*) 'ERROR: Wrong input keyword ',trim(nextenergy)
      write(*,*) 'ERROR: Wrong input keyword ',trim(nextenergy)
      STOP
 1200 EIN = -1
 1250 IF (EIN.LT.0.0D0) THEN
C
C        CLOSING FILES
C
         CLOSE (5)
         CLOSE (11)
         CLOSE (13)
         IF (FILevel) CLOSE (14)
         CLOSE (15,STATUS = 'delete')
         CLOSE (16,STATUS = 'delete')
         CLOSE (23)
         CLOSE (24)
         CLOSE (29)
         CLOSE (33)
         CLOSE (40)
         CLOSE (41)
         IF(NNG_xs.gt.0) CLOSE (104)
         CLOSE (107)
         CLOSE (108)
         CLOSE (110)

         IF (ltransfer) CLOSE (112)
         IF (lbreakup)  CLOSE (113)
         IF (ltransfer .or. lbreakup)  CLOSE (114)

         IF(DEGa.GT.0) THEN
           CLOSE (42)
         ELSE
           CLOSE (42,STATUS = 'delete')
         ENDIF

         CLOSE(53)
         CLOSE(58)
         CLOSE (66,STATUS = 'delete')
         IF(FISspe.GT.0) THEN
           CLOSE (73)
           CLOSE (74)
         ENDIF
         CLOSE (98)
         IF (IOPran.NE.0) then
C----------Saving random seeds
           ftmp = grand()
           OPEN(94,file='R250SEED.DAT',status='UNKNOWN')
           write(94,*)  indexf, indexb
           DO i = 1, 250
             write(94,*) buffer(i)
           ENDDO
           CLOSE(94)
           WRITE (8,*)
           WRITE (12,*)

           WRITE (8 ,*) ' Saving RNG status :'
           WRITE (12,*) ' Saving RNG status :'
           WRITE (8 ,*) 'RNG indexes      ', indexf, indexb                         
           WRITE (8 ,*) 'RNG buffer(1)    ', buffer(1)
           WRITE (8 ,*) 'RNG buffer(250)  ', buffer(250)
           WRITE (12,*) 'RNG indexes     ', indexf, indexb                          
           WRITE (12,*) 'RNG buffer(1)   ', buffer(1)
           WRITE (12,*) 'RNG buffer(250) ', buffer(250)

           WRITE (8,*)
           WRITE (12,*)

           WRITE (95,'(A2)') '# '
           WRITE (95,'(A21)') '# Saving RNG status :'
           WRITE (95,'(A21,2(1x,I10))') 
     &     '# RNG indexes        ', indexf, indexb                            
           WRITE (95,'(A21,2(1x,I10))') 
     &     '# RNG buffer(1)      ', buffer(1)                           
           WRITE (95,'(A21,2(1x,I10))') 
     &     '# RNG buffer(250)    ', buffer(250)                         
         ENDIF

         CLOSE(95)  ! FROM INPUT.F
         CLOSE(102)
         IF (SFACT.GT.0) CLOSE(781)

         WRITE (12,*) ' '
         WRITE (12,*) ' CALCULATIONS COMPLETED SUCCESSFULLY'
         WRITE (*,*) '.'
         WRITE (*,*) ' CALCULATIONS COMPLETED SUCCESSFULLY'
         WRITE (8,*) ' '
         WRITE (8,*) ' CALCULATIONS COMPLETED SUCCESSFULLY'

         CALL THORA (8)
         CALL THORA(12)
         CLOSE (8)
         CLOSE (12)

         RETURN
      ENDIF
      IF(EIN.LT.epre .and. .NOT. BENchm) THEN
         WRITE(8,*) EIN,epre
         WRITE(8,*) 'ERROR: INPUT ENERGIES OUT OF ORDER !!'
         WRITE(8,*) 'ERROR: CHECK YOUR INPUT FILE'
         PAUSE 'FATAL: INPUT ENERGIES OUT OF ORDER !!'
         STOP
      ENDIF
      epre = EIN
      IF (IOPran.ne.0) 
     &     WRITE(95,'(A19,2x,D12.6)') '# Incident Energy :',EINl
C-----
C-----
      IF(FITomp.GE.0) THEN
        IF(LHMs.EQ.0) THEN
          NANgela = 91
          NDAng   = 91
        ELSE
          NANgela = NDAnghmx
          NDAng   = NDAnghmx
        ENDIF
C-------Set angles for inelastic calculations
        da = 180.0/(NDANG - 1)
        DO na = 1, NDAng
          ANGles(na)  = (na - 1)*da
          CANgle(na)  = DCOS(ANGles(na)*PI/180.d0)
        ENDDO
        DO na = 1, NDAng
          CANgler(na) = DCOS(ANGles(NDAng - na + 1)*PI/180.d0)
          SANgler(na) = DSIN(ANGles(NDAng - na + 1)*PI/180.d0)
C         SANgler(na) = DSQRT(1.D0 - CANgler(na)**2)
        ENDDO
      ENDIF
      IF(.not.BENchm) FIRst_ein = .FALSE.
C     
C     Changing CN-isotropic to false if energy is too high as CN
C     decay to discrete levels is negligible         
C     ELV(NLV(0),0) is the energy of the last discrete level of the target nucleus
      if(.not.CN_isotropic) then    
C       Only for actinides, more conditions need to be added        
C       depending on odd/even properties and mass number
        if(A(0).gt.220 .and. EIN.gt.8.d0) then
          CN_isotropic = .TRUE.
          WRITE(8,*)
          WRITE(8,*) 
     &    'CN angular distribution assumed isotropic above Einc = 8 MeV'
          WRITE(*,*)      
          WRITE(*,*) 
     &    'CN angular distribution assumed isotropic above Einc = 8 MeV'
          WRITE(8,*)
        endif  
      endif
C
      GOTO 1300
99070 FORMAT (I12,F10.5,I5,F8.1,G15.6,I3,7(I4,F7.4),:/,(53X,7(I4,F7.4)))
99075 FORMAT (1X,F5.2,12E10.3)
      END
C
C
      SUBROUTINE GET_RECOIL(Ke,Nnuc)
Ccc
Ccc   ********************************************************************
Ccc   *                                                         class:ppu*
Ccc   *                         RECOIL                                   *
Ccc   *                                                                  *
Ccc   *                                                                  *
Ccc   *  Constructs recoil spectra:                                      *
Ccc   *  Each excitation bin is given recoil spectrum, when a particle   *
Ccc   *  is emitted its recoil velocity is vector added to the parent    *
Ccc   *  recoil velocity and a resulting spectrum is summed upon daughter*
Ccc   *  recoil spectrum corresponding to the populated energy bin in the*
Ccc   *  daughter (kinematical normalization taken into account).        *
Ccc   *  Daughter recoil spectra will be distributed between             *
Ccc   *  adjacent bins (inversly proportional to the                     *
Ccc   *  distance of the actual energy to the bin energy                 *
Ccc   *  in order to conserve energy).                                   *
Ccc   *  Requires that continuum spectra from each bin are stored on the *
Ccc   *  AUSpec array and those to discrete levels on the REClev for each*
Ccc   *  reaction mechanism considered in the calculations.              *
Ccc   *                                                                  *
Ccc   ********************************************************************
Ccc
      INCLUDE 'dimension.h'
      INCLUDE 'global.h'
C
C Dummy arguments
C
      INTEGER Ke, Nnuc
C
C Local variables
C
      DOUBLE PRECISION coef, dang, erecejc, erecod, erecoil, erecpar,
     &                 exqcut, recorr, sumnor, weight, ares, zres
      REAL FLOAT
      INTEGER icse, ie, il, ire, irec, na, nejc, nnur, izares, iloc
C
C
C-----Normalize recoil spectrum of the parent
      sumnor = 0.0
      DO ire = 1, NDEREC
         sumnor = sumnor + RECcse(ire,Ke,Nnuc)
      ENDDO
      IF (sumnor.NE.0.0D0) THEN
         DO ire = 1, NDEREC
            RECcse(ire,Ke,Nnuc) = RECcse(ire,Ke,Nnuc)/sumnor
         ENDDO
      ENDIF
      dang = PI/FLOAT(NDANG - 1)
      coef = dang/DERec/2.0
      DO nejc = 1, NEJcm   !over ejectiles
         ares = A(nnuc) - AEJc(nejc)
         zres = Z(nnuc) - ZEJc(nejc)
C--------Residual nuclei must be heavier than alpha
         if(ares.le.4. and. zres.le.2.) cycle
         izares = INT(1000.0*zres + ares)
         CALL WHERE(izares,nnur,iloc)
         if(iloc.eq.1) CYCLE
C--------Decay to continuum
C--------recorr is a recoil correction factor that
C--------divides outgoing energies
         recorr = AMAss(Nnuc)/EJMass(nejc)
         exqcut = EX(Ke,Nnuc) - Q(nejc,Nnuc) - ECUt(nnur)
         DO ie = 1, NDECSE !over ejec. energy (daughter excitation)
            icse = (exqcut - (ie - 1)*DE)/DE + 1.001
C-----------Daughter bin
            IF (icse.LE.0) GOTO 50
            erecejc = (ie - 1)*DE/recorr
            DO ire = 1, NDEREC          !over recoil spectrum
               erecpar = (ire - 1)*DERec
               DO na = 1, NDANG
                  erecoil = erecejc + erecpar
                  erecoil = erecoil + 2.0*SQRT(erecejc*erecpar)
     &                      *CANgler(na)
                  irec = erecoil/DERec + 1.001
                  weight = (erecoil - (irec - 1)*DERec)/DERec
                  IF (irec + 1.GT.NDEREC) GOTO 20
                  RECcse(irec,icse,nnur) = RECcse(irec,icse,nnur)
     &               + RECcse(ire,Ke,Nnuc)*AUSpec(ie,nejc)
     &               *(1.0 - weight)*SANgler(na)*coef
                  RECcse(irec + 1,icse,nnur)
     &               = RECcse(irec + 1,icse,nnur) + RECcse(ire,Ke,Nnuc)
     &               *AUSpec(ie,nejc)*weight*SANgler(na)*coef
               ENDDO                  !over angles
   20       ENDDO                  !over recoil spectrum
         ENDDO                  !over  daugther excitation
C--------Decay to discrete levels (stored with icse=0)
   50    exqcut = exqcut + ECUt(nnur)
         DO il = 1, NLV(nnur)
            erecod = exqcut - ELV(il,nnur)   !emission energy
            erecod = erecod/recorr
            IF (erecod.LT.0) GOTO 100
            DO ire = 1, NDEREC      !over recoil spectrum
               DO na = 1, NDANG !over angles
                  erecoil = (ire - 1)*DERec + erecod +
     &                       2.0*SQRT((ire - 1)*DERec*erecod)
     &                      *CANgler(na)
                  irec = erecoil/DERec + 1.001
                  weight = (erecoil - (irec - 1)*DERec)/DERec
                  IF (irec.GT.NDEREC) GOTO 60
                  RECcse(irec,0,nnur) = RECcse(irec,0,nnur)
     &                                  + RECcse(ire,Ke,Nnuc)
     &                                  *REClev(il,nejc)*(1.0 - weight)
     &                                  *SANgler(na)*coef
c------------------------
!                 IF(irec.EQ.5 .AND. RECcse(irec,0,nnur).GT.0
!     &               .AND.na.EQ.10) THEN
!                  WRITE(8,*) '       Parent bin', Ke, 'Nnuc', Nnuc
!                  WRITE(8,*) 'Recoil bin', ire
!                  WRITE(8,*) 'Erecoil ', erecoil, erecod, nnuc
!                  WRITE(8,*) 'RECcse, RECcse par, REClev',
!     &            RECcse(irec,0,nnur),RECcse(ire,Ke,Nnuc),
!     &            REClev(il,nejc)
!                  ENDIF
c------------------------
                  IF (irec + 1.GT.NDEREC) GOTO 60
                  RECcse(irec + 1,0,nnur) = RECcse(irec + 1,0,nnur)
     &               + RECcse(ire,Ke,Nnuc)*REClev(il,nejc)
     &               *weight*SANgler(na)*coef
               ENDDO                  !over angles
   60       ENDDO                  !over recoil spectrum
         ENDDO                  !over levels
  100 ENDDO                  !over ejectiles
C-----
C-----Parent recoil spectrum after gamma decay
C-----
      nnur = Nnuc
      nejc = 0
C-----gamma decay to continuum
      DO ie = 1, NDECSE !over ejec. energy (daughter excitation)
         icse = (EX(Ke,Nnuc) - (ie - 1)*DE - ECUt(nnur))/DE + 1.001
C--------!daughter bin
         DO irec = 1, NDEREC         !over recoil spectrum
            IF (icse.GT.0) RECcse(irec,icse,nnur)
     &          = RECcse(irec,icse,nnur) + RECcse(irec,Ke,Nnuc)
     &          *AUSpec(ie,0)/DERec
         ENDDO                  !over recoil spectrum
      ENDDO                  !over  daugther excitation
C-----gamma decay to discrete levels (stored with icse=0)
      DO il = 1, NLV(nnur)
         DO ire = 1, NDEREC             !over recoil spectrum
            RECcse(ire,0,nnur) = RECcse(ire,0,nnur)
     &                           + RECcse(ire,Ke,Nnuc)*REClev(il,nejc)
     &                           /DERec
         ENDDO                  !over recoil spectrum
      ENDDO                  !over levels
      END

      SUBROUTINE PRINT_RECOIL(Nnuc,React)
C-----
C-----Prints recoil spectrum of nnuc residue
C-----
      INCLUDE 'dimension.h'
      INCLUDE 'global.h'
C
C Dummy arguments
C
      INTEGER Nnuc
      CHARACTER*21 React
C
C Local variables
C
      DOUBLE PRECISION csum, ftmp, corr, xsdisc
      INTEGER ie, ilast

      IF (CSPrd(Nnuc).LE.0.0D0) RETURN
C-----Normalize recoil spectra to remove eventual inaccuracy
C-----due to numerical integration of angular distributions
C-----and find last non-zero cross section for printing
      csum  = 0.0
      ilast = 0
      DO ie = 1, NDEREC
        ftmp = RECcse(ie,0,Nnuc)
        IF (ftmp.GT.0) then
          csum = csum + ftmp
          ilast = ie
        ENDIF
      ENDDO
      IF (csum.EQ.0) RETURN
      ilast = MIN(ilast + 1,NDEX)
C
C        corr = CSPrd(Nnuc)/(csum*DERec)
C        WRITE(8,*) 'ie, RECcse ,ilast', ie, RECcse(ie,0,Nnuc), ilast
C        WRITE(8,*) 'nnuc, rec, cs',nnuc,corr*DERec,CSPrd(nnuc)
C        DO ie = 1, ilast
C           RECcse(ie,0,Nnuc) = RECcse(ie,0,Nnuc)*corr
C        ENDDO
         WRITE (12,*) ' '
         WRITE (12,'(A23,A7,A4,I6,A6,F12.5)') '  Spectrum of recoils  ',
     &          React, 'ZAP=', IZA(Nnuc), ' mass=', AMAss(Nnuc)
         WRITE (12,*) ' '
         WRITE (12,'(''    Energy    mb/MeV'')')
         WRITE (12,*) ' '
         DO ie = 1, ilast
            WRITE (12,'(F10.5,E14.5)') FLOAT(ie - 1)*DERec,
     &                                 RECcse(ie,0,Nnuc)
         ENDDO
         WRITE(12,
     &     '(/2x,''Integral of recoil spectrum   '',G12.6,'' mb'' )') 
     &       csum*DERec
         
		 corr = CSPrd(Nnuc)/(csum*DERec)
		 xsdisc = 0.d0
         IF (nnuc.EQ.mt849) THEN
		xsdisc = CSDirlev(1,3) 
            WRITE (12,'(2X,''Cont. popul. before g-cascade '',
     &         G12.6,'' mb'')') CSPrd(nnuc) - CSDirlev(1,3)
            WRITE (12,'(2X,''Disc. popul. before g-cascade '',
     &                G12.6,'' mb'')') CSDirlev(1,3)
            corr = (CSPrd(Nnuc)- CSDirlev(1,3))/(csum*DERec)
         ENDIF
         WRITE(12,
     &     '( 2x,''Production cross section      '',G12.6,'' mb'' )') 
     &      CSPrd(Nnuc) 

         IF (ABS(1.d0-corr).GT.0.02D0 .AND. CSPrd(Nnuc).GT.0.001D0) 
     &      THEN
            WRITE (8,*) 
            WRITE (8,*) ' ******' 
            WRITE (8,*) ' WARNING:  Ein = ', sngl(EIN), ' MeV'
            WRITE (8,*) ' WARNING:  ZAP = ', IZA(Nnuc), ' from ', React
            WRITE (8,*) 
     &         ' WARNING: x-section balance in recoils wrong'
            WRITE (8,*) ' WARNING: recoil integral     = ',
     &                  sngl(csum*DERec),' mb'
            WRITE (8,*) ' WARNING: Cont. cross section = ',
     &                  sngl(CSPrd(Nnuc)-xsdisc),' mb'
            WRITE (8,*) ' WARNING: Prod. cross section = ',
     &                  sngl(CSPrd(Nnuc)),' mb'
            WRITE (8,*) ' WARNING: Discr.cross section = ',
     &                  sngl(xsdisc),' mb'
         ENDIF
      RETURN
      END

      SUBROUTINE PRINT_BIN_RECOIL(Nnuc,React)
C-----
C-----Prints recoil spectrum of (n,n) or (n,p) residue
C-----
      INCLUDE 'dimension.h'
      INCLUDE 'global.h'
C
C Dummy arguments
C
      INTEGER Nnuc
      CHARACTER*21 React
C
C Local variables
C
      INTEGER ie, ilast, ipart
      DOUBLE PRECISION csum, corr, xsdisc

      ipart = 1  !neutron
      IF(IZA(1)-IZA(Nnuc) .EQ. 1001) ipart = 2    !proton

C-----Find last non-zero cross section for printing
      ilast = 0
      DO ie = NDEX,1,-1
         IF (POPcse(0,ipart,ie,INExc(Nnuc)). GT. 0) THEN
           ilast = ie
           exit
         ENDIF
      ENDDO
      IF (ilast .EQ. 0) RETURN

      ilast = MIN(ilast + 1,NDEX)
C-----correction factor multiplying cross sections and dividing DE is
C-----simply A(1) since ejectile mass is here always 1 (neutron or proton) 
         WRITE (12,*) ' '
         WRITE (12,'(A23,A7,A4,I6,A6,F12.5)') '  Spectrum of recoils  ',
     &          React, 'ZAP=', IZA(Nnuc), ' mass=', AMAss(Nnuc)
         WRITE (12,*) ' '
         WRITE (12,'(''    Energy    mb/MeV'')')
         WRITE (12,*) ' '
		 csum = 0.d0
         DO ie = 1, ilast
		    csum = csum + POPcse(0,ipart,ie,INExc(Nnuc)) 
            WRITE (12,'(F10.5,E14.5)') FLOAT(ie - 1)*DE/A(1),
     &      POPcse(0,ipart,ie,INExc(Nnuc))*A(1)     
         ENDDO
         WRITE(12,
     &     '(/2x,''Integral of recoil spectrum   '',G12.6,'' mb'' )') 
     &     csum*DE

		 corr = CSPrd(Nnuc)/(csum*DE)
		 xsdisc = 0.d0
         IF (nnuc.EQ.mt91) THEN
			xsdisc = CSDirlev(1,1) 
            WRITE (12,'(2X,''Cont. popul. before g-cascade '',
     &         G12.6,'' mb'')') CSPrd(nnuc) - CSDirlev(1,1)
            WRITE (12,'(2X,''Disc. popul. before g-cascade '',
     &                G12.6,'' mb'')') CSDirlev(1,1)
            corr = (CSPrd(Nnuc)- CSDirlev(1,1))/(csum*DE)
         ENDIF
         IF (nnuc.EQ.mt649) THEN
			xsdisc = CSDirlev(1,2) 
            WRITE (12,'(2X,''Cont. popul. before g-cascade '',
     &         G12.6,'' mb'')') CSPrd(nnuc) - CSDirlev(1,2)
            WRITE (12,'(2X,''Disc. popul. before g-cascade '',
     &                G12.6,'' mb'')') CSDirlev(1,2)
            corr = (CSPrd(Nnuc)- CSDirlev(1,2))/(csum*DE)
         ENDIF
         IF (nnuc.EQ.mt849) THEN
			xsdisc = CSDirlev(1,3) 
            WRITE (12,'(2X,''Cont. popul. before g-cascade '',
     &         G12.6,'' mb'')') CSPrd(nnuc) - CSDirlev(1,3)
            WRITE (12,'(2X,''Disc. popul. before g-cascade '',
     &                G12.6,'' mb'')') CSDirlev(1,3)
            corr = (CSPrd(Nnuc)- CSDirlev(1,3))/(csum*DE)
         ENDIF
         WRITE(12,
     &      '(2x,''Production cross section      '',G12.6,'' mb'' )') 
     &      CSPrd(Nnuc) 

         IF (ABS(1.d0-corr).GT.0.02D0 .AND. CSPrd(Nnuc).GT.0.001D0) 
     &      THEN
            WRITE (8,*) 
            WRITE (8,*) ' ******' 
            WRITE (8,*) ' WARNING:  Ein = ', sngl(EIN), ' MeV'
            WRITE (8,*) ' WARNING:  ZAP = ', IZA(Nnuc), ' from ', React
            WRITE (8,*) 
     &         ' WARNING: x-section balance in recoils wrong'
            WRITE (8,*) ' WARNING: recoil integral     = ',
     &                  sngl(csum*DE),' mb'
            WRITE (8,*) ' WARNING: Cont. cross section = ',
     &                  sngl(CSPrd(Nnuc)-xsdisc),' mb'
            WRITE (8,*) ' WARNING: Prod. cross section = ',
     &                  sngl(CSPrd(Nnuc)),' mb'
            WRITE (8,*) ' WARNING: Discr.cross section = ',
     &                  sngl(xsdisc),' mb'
         ENDIF

         RETURN
      END

      SUBROUTINE FISCROSS(Nnuc,Ke,Ip,Jcn,Sumfis,Sumfism)
      INCLUDE 'dimension.h'
      INCLUDE 'global.h'
C
C
C COMMON variables
C
      DOUBLE PRECISION TFIso, TGIso, TISo, RFIso, PFIso                   ! FIS_ISO
      DOUBLE PRECISION TF(NFPARAB), TDIr, TABs, TG2                       ! IMAG
      COMMON /FIS_ISO/ TFIso, TGIso, TISo, RFIso, PFIso
      COMMON /IMAG  / TF, TDIr, TABs, TG2
C
C Dummy arguments
C
      DOUBLE PRECISION Sumfis
      INTEGER Ip, Jcn, Ke, Nnuc
      DOUBLE PRECISION Sumfism(NFMOD)
C
C Local variables
C
      INTEGER INT
      INTEGER k, kk, m

      IF (NINT(FISmod(Nnuc)).EQ.0) THEN

         CALL FISFIS(Nnuc,Ke,Ip,Jcn,Sumfis,0)

      ELSE ! NINT(FISmod(Nnuc)).GT.0 = Multimodal 

         TFB = 0.d0
         DO m = 1, INT(FISmod(Nnuc)) + 1
            EFB(2) = EFBm(m)
            Hcont(2) = HM(1,m)
            DO k = 1, NRFdis(2)
               H(k,2) = HM(k,m)
               EFDis(k,2) = EFDism(k,m)
            ENDDO
            XMInn(2) = XMInnm(m)
            NRBinfis(2) = NRBinfism(m)
            DEStepp(2) = DEStepm(m)
            DO kk = 1, NRBinfis(2)
               UGRid(kk,2) = UGRidf(kk,m)
               ROFisp(kk,Jcn,1,2) = ROFism(kk,Jcn,m)
               ROFisp(kk,Jcn,2,2) = ROFism(kk,Jcn,m)
            ENDDO
            CALL FISFIS(Nnuc,Ke,Ip,Jcn,Sumfis,m)
            TFBm(m) = TF(2)
            TFB = TFB + TFBm(m)
         ENDDO

         Sumfism = 0.d0
         IF ((TF(1) + TFB).GT.0.) then
           DO m = 1, INT(FISmod(Nnuc)) + 1
             Sumfism(m) = TF(1)*TFBm(m)/(TF(1) + TFB)
           ENDDO
         ENDIF

         Sumfis = TF(1)*TFB/(TF(1) + TFB)
         
         DENhf = DENhf + Sumfis
 
      ENDIF
C
      RETURN
      END




      SUBROUTINE XSECT(Nnuc,M,Xnor,Sumfis,Sumfism,Ke,Ipar,Jcn,Fisxse)
      INCLUDE 'dimension.h'
      INCLUDE 'global.h'
C
C
C COMMON variables
C
      DOUBLE PRECISION TFIso, TGIso, TISo, RFIso, PFIso                   ! FIS_ISO
      DOUBLE PRECISION TF(NFPARAB), TDIr, TABs, TG2                       ! IMAG
      COMMON /FIS_ISO/ TFIso, TGIso, TISo, RFIso, PFIso
      COMMON /IMAG  / TF, TDIr, TABs, TG2
C
C Dummy arguments
C
      DOUBLE PRECISION Sumfis, Xnor, Fisxse
      INTEGER Ipar, Jcn, Ke, M, Nnuc
      DOUBLE PRECISION Sumfism(NFMOD)
C
C Local variables
C
      INTEGER INT
      INTEGER nejc, nnur, izares, iloc
      DOUBLE PRECISION ares, zres
C
C-----particles
      DO nejc = 1, NEJcm
         ares = A(nnuc) - AEJc(nejc)
         zres = Z(nnuc) - ZEJc(nejc)
C        residual nuclei must be heavier than alpha
         if(ares.le.4. and. zres.le.2.) cycle
         izares = INT(1000.0*zres + ares)
         CALL WHERE(izares,nnur,iloc)
         if(iloc.eq.1) CYCLE
         CALL ACCUM(Ke,Nnuc,nnur,nejc,Xnor)
         CSEmis(nejc,Nnuc) = CSEmis(nejc,Nnuc) + Xnor*SCRtem(nejc)
      ENDDO
C-----gammas
      CALL ACCUM(Ke,Nnuc,Nnuc,0,Xnor)
      CSEmis(0,Nnuc) = CSEmis(0,Nnuc) + Xnor*SCRtem(0)
      POP(Ke,Jcn,Ipar,Nnuc) = 0.d0

C-----fission
      IF (NINT(FISmod(Nnuc)).EQ.0) THEN
         Fisxse = Sumfis*Xnor
         CSFis  = CSFis + Fisxse
         IF (ENDf(Nnuc).EQ.1 .AND. Fisxse.GT.0.d0 .AND.
     &       POPbin(Ke,Nnuc).GT.0.d0)
     &       CALL EXCLUSIVEC(Ke,0, -1,Nnuc,0,Fisxse)

      ELSE ! Multimodal

         Fisxse = 0.d0
         DO M = 1, INT(FISmod(Nnuc)) + 1
            Fisxse = Fisxse + Sumfism(M)*Xnor
            CSFism(M) = CSFism(M) + Sumfism(M)*Xnor
         ENDDO
         CSFis  = CSFis + Fisxse
         IF (ENDf(Nnuc).EQ.1 .AND. Fisxse.GT.0.d0 .AND.
     &       POPbin(Ke,Nnuc).GT.0.d0)
     &       CALL EXCLUSIVEC(Ke,0, -1,Nnuc,0,Fisxse)
      ENDIF
      RETURN
      END