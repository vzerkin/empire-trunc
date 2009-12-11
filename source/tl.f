Ccc   * $Author: pigni $
Ccc   * $Date: 2009-12-11 21:44:36 $
Ccc   * $Id: tl.f,v 1.106 2009-12-11 21:44:36 pigni Exp $

      SUBROUTINE HITL(Stl)
Ccc
Ccc   ************************************************************
Ccc   *                                                 class:ppu*
Ccc   *                      H I T L                             *
Ccc   *                                                          *
Ccc   * Calculates transmission coefficients for Heavy-Ion abs-  *
Ccc   * orption using either                                     *
Ccc   * input fusion x-section or input value of critical l CRL  *
Ccc   * or using distributed fusion barrier (if CSFUS.EQ.-1.)    *
Ccc   *                                                          *
Ccc   * input:none                                               *
Ccc   *                                                          *
Ccc   * output:STL-matrix of transmission coefficients (1s=1)    *
Ccc   *                                                          *
Ccc   ************************************************************
Ccc
      INCLUDE 'dimension.h'
      INCLUDE 'global.h'
C
C COMMON variables
C
      DOUBLE PRECISION A0R, AU, ETAk, HC, RAB, REDm, V0R
      COMMON /ONE   / V0R, A0R, RAB, REDm, AU, HC, ETAk
C
C Dummy arguments
C
      DOUBLE PRECISION Stl(NDLW)
C
C Local variables
C
      DOUBLE PRECISION arg, clf, homega, ra, rb, rbar, rred, xfu, xfum
      LOGICAL distrb
      INTEGER i
      DOUBLE PRECISION XFUS
      distrb = .FALSE.
C-----if critical l given in input jump directly to Tl calculations
      IF (CRL.LE.0.D0) THEN
C-----CCFUS calculations
         IF (CSRead.EQ.( - 2.D0)) THEN
            CALL CCFUS(Stl)
            RETURN
         ENDIF
C--------CCFUS calculations  *** done ****
C--------check for distribution barrier
         IF (CSRead.EQ.( - 1.D0)) distrb = .TRUE.
C--------calculate projectile+target binding energy if not done already
         IF (Q(0,1).EQ.0.D0) CALL BNDG(0,1,Q(0,1))
         IF (distrb) THEN
            IF (BFUs.EQ.0.0D0) THEN
C--------------calculate fusion barrier using CCFUS routine BAR
               ra = 1.233*AEJc(0)**(1./3.) - 0.978/AEJc(0)**(1./3.)
               rb = 1.233*A(0)**(1./3.) - 0.978/A(0)**(1./3.)
               RAB = ra + rb + 0.29
               rred = ra*rb/(ra + rb)
               REDm = AEJc(0)*A(0)/(AEJc(0) + A(0))
               AU = AMUmev
               HC = HHBarc
               V0R = 30.08*(1. - 1.8*(1. - 2.*ZEJc(0)/AEJc(0))
     &               *(1. - 2.*Z(0)/A(0)))*rred + DV - 20.
               A0R = 0.63
               ETAk = 1.43997*ZEJc(0)*Z(0)
               CALL BAR(rbar,BFUs,homega)
               WRITE (8,*) 'Fusion barrier is ', BFUs, ' MeV'
            ENDIF
            IF (SIG.EQ.0.0D0) SIG = 0.05*BFUs
            IF (IOUt.GT.0) THEN
               WRITE (8,*) 'Distributed fusion barrier with extra push='
     &                     , EXPush
               WRITE (8,*) 'SIG=', SIG, ' and TRUNC=', TRUnc,
     &                     ' has been used'
            ENDIF
            CALL PUSH(EIN,A(1),AEJc(0),A(0),BFUs,EXPush,SIG,TRUnc,Stl,
     &                NLW,NDLW)
            RETURN
         ENDIF
C--------calculation of fusion Tl's with distributed barrier model
C        *** done ***
C--------prepare starting values for searching critical l
         IF (CSRead.LE.0.0D0) THEN
            clf = CRL - 2.0*DFUs
            IF (CRL - clf.LT.3.D0) clf = CRL - 3.
            IF (clf.LT.0.D0) clf = 10.
         ELSE
            CSFus = CSRead
            clf = 1.0
         ENDIF
         xfum = 0.0
   50    xfu = XFUS(EIN,AEJc(0),A(0),DFUs,clf)
         IF (xfu.LT.CSFus) THEN
            xfum = xfu
            clf = clf + 1.
            GOTO 50
         ELSE
            CRL = clf - 1 + (CSFus - xfum)/(xfu - xfum)
         ENDIF
         NLW = max(CRL + MAX(5.D0,5.0D0*DFUs),DBLE(NDLW-2))
C-----setting transmission coefficients for fusion if not distr. barr.
      ENDIF
      DO i = 1, NDLW
         arg = (CRL - i + 1)/DFUs
         arg = MIN(174.0D0,arg)
         Stl(i) = 1.0/(1.0 + EXP((-arg)))
      ENDDO
      END


      SUBROUTINE RIPL2EMPIRE(Nejc,Nnuc,E)
      INCLUDE 'dimension.h'
      INCLUDE 'global.h'
C-----For dispersive optical model potentials
C-----It is ripl2empireS.h because it must be compatible
C-----with global.h declarations so some variables must be renamed
      INCLUDE 'ripl2empireS.h'
C
C COMMON variables
C
      INTEGER MODelcc
      COMMON /LOCAL / MODelcc
C
C Dummy arguments
C
      DOUBLE PRECISION E
      INTEGER Nejc, Nnuc
C
C Local variables
C
      DOUBLE PRECISION alib(6), rlib(6), vlib(6), xmas_nejc, xmas_nnuc
      DOUBLE PRECISION EcollTarget, RCCC
      CHARACTER*80 ch_iuf
      LOGICAL coll_defined
      CHARACTER*132 ctmp
      REAL FLOAT
      INTEGER iainp, izinp, k, n, ncalc, nld_cc
      INTEGER*4 iwin
      INTEGER*4 PIPE
C
C-----Sets CC optical model parameters according to RIPL
C
C-----E must be in lab system !!!
C
      DATA coll_defined/.FALSE./
      iainp = A(Nnuc)
      izinp = Z(Nnuc)
C
C-----SETTING COLLECTIVE LEVELS for DIRECT CALCULATIONS
C
      MODelcc = 0
      IF (DIRect.EQ.0) IMOdel = 0
C     IF (IMOdel.EQ.0) model = 'spherical nucleus model'
C     IF (IMOdel.EQ.1) model = 'coupled-channels rigid rotor model'
C     IF (IMOdel.EQ.2) model = 'coupled-channels vibrational model'
C     IF (IMOdel.EQ.3) model = 'coupled-channels soft rotor model'

      IF (IMOdel.EQ.3 .AND. FIRst_ein) THEN
         WRITE (8,*) 'WARNING: coupled-channels soft rotor model'
         WRITE (8,*) 'WARNING: OPTMAN code is needed            '
         IWArn = 5
         GOTO 300
      ENDIF
      IF (IMOdel.EQ.1 .OR. IMOdel.EQ.2 .OR. IMOdel.EQ.3) THEN
C--------Imodel not used for non-inelastic channels
         IF (iainp.NE.A(0) .OR. izinp.NE.Z(0) .OR. AEJc(Nejc).NE.AEJc(0)
     &       .OR. ZEJc(Nejc).NE.ZEJc(0)) GOTO 300
      ENDIF
      MODelcc = IMOdel
      IF (ND_nlv.GT.0 .AND. NISotop.EQ.0) coll_defined = .TRUE.
      IF (IMOdel.EQ.1 .AND. (.NOT.coll_defined)) THEN
C
C--------model = 'coupled-channels rigid rotor model'
C
         coll_defined = .TRUE.
         IF (NISotop.EQ.0 .AND. FIRst_ein) THEN
            WRITE (8,*) 'WARNING: None of the requested isotopes is '
            WRITE (8,*)
     &               'WARNING: included in the selected RIPL potential.'
            WRITE (8,*)
     &                 'WARNING: Default collective levels will be used'
            GOTO 300
         ENDIF
         ncalc = 0
         DO n = 1, NISotop
            IF (iainp.EQ.IA(n) .AND. izinp.EQ.IZ(n)) THEN
               ncalc = n
               IF (IDEf(n).GT.2*LMAx(n) .OR. NCOll(n).GT.NDCOLLEV) THEN
                  WRITE (8,*) 'WARNING: OMP collective levels are wrong'
                  WRITE (8,*) 'WARNING: Too many levels or deformations'
                  WRITE (8,*)
     &                 'WARNING: Default collective levels will be used'
                  IWArn = 6
                  GOTO 300
               ENDIF
            ENDIF
         ENDDO
         IF (ncalc.EQ.0) THEN
            WRITE (8,*)
     &                'WARNING: RIPL OMP collective levels are not used'
            WRITE (8,*)
     &                 'WARNING: Default collective levels will be used'
            GOTO 300
         ENDIF
         IF (NCOll(ncalc).EQ.0) THEN
            WRITE (8,*)
     &                'WARNING: RIPL OMP collective levels are not used'
            WRITE (8,*)
     &                 'WARNING: Default collective levels will be used'
            GOTO 300
         ENDIF
C--------Setting EMPIRE global variables
         nld_cc = 0
         DO k = 1, ND_nlv
            IF (ICOllev(k).LT.LEVcc) THEN
                nld_cc = nld_cc + 1
            ELSE
              IF (k.le.NCOll(ncalc)) then
                ICOllev(k) = ICOllev(k) - LEVCC
                nld_cc = nld_cc + 1
                WRITE (8,*) 'WARNING: level ',k,' COUPLED'
              ENDIF
            ENDIF
         ENDDO
C
         WRITE (8,*)
         WRITE (8,*)
         WRITE (8,*) 'Deformation of the gsb adopted from CC RIPL OMP'
         LMAxcc = LMAx(ncalc)
         IDEfcc = IDEf(ncalc)
         DO k = 2, IDEfcc, 2
            D_Def(1,k) = DDEf(ncalc,k)
         ENDDO
         IF (nld_cc.NE.NCOll(ncalc)) THEN
            WRITE (8,*) 'WARNING: Default number of coupled levels: ',
     &                  nld_cc
            WRITE (8,*) 'WARNING: is not equal ', NCOll(ncalc),
     &                  ' (used in CC RIPL OMP)'
            WRITE (8,*) 'WARNING: Using RIPL CC levels'
         ENDIF
C
C--------Joining TARGET_COLL.DAT and TARGET_COLL_RIPL.DAT files
C
         OPEN (32,FILE = 'TARGET_COLL_RIPL.DAT')
         OPEN (97,FILE = 'TARGET_COLL.DAT')
         OPEN (96,FILE = 'COLL.DAT')
         WRITE (8,*)
     &       'Collective levels from RIPL CC OMP, symm.rotational model'
         WRITE (32,*)
     &       'Collective levels from RIPL CC OMP, symm.rotational model'
         READ (97,'(A80)') ch_iuf   ! FIRST LINE
         WRITE (96,'(A80)') ch_iuf
         WRITE (8,*) 'Dyn.deformations are not used in symm.rot.model'
         WRITE (32,*) 'Dyn.deformations are not used in symm.rot.model'
         READ (97,'(A80)') ch_iuf       ! 2ND LINE
         WRITE (96,'(A80)') ch_iuf
         WRITE (8,'(1x,i3,1x,i3,a35)') izinp, iainp,
     &                                 ' nucleus is treated as deformed'
         WRITE (32,'(1x,i3,1x,i3,a35)') izinp, iainp,
     &                                 ' nucleus is treated as deformed'
         READ (97,'(A80)') ch_iuf       ! 3ER LINE
         WRITE (96,'(A80)') ch_iuf
         WRITE (8,*)
         WRITE (32,*)
         READ (97,'(A80)') ch_iuf       ! EMPTY LINE
         WRITE (96,'(A80)') ch_iuf
         DEFormed = .TRUE.
         DO n = 1, NISotop
            IF (iainp.EQ.IA(n) .AND. izinp.EQ.IZ(n)) THEN
               WRITE (8,*)
     &                 '   Ncoll  Lmax IDef  Kgs  (Def(1,j),j=2,IDef,2)'
               WRITE (32,*)
     &                 '   Ncoll  Lmax IDef  Kgs  (Def(1,j),j=2,IDef,2)'
               READ (97,'(A80)') ch_iuf
               WRITE (96,*)
     &                 '   Ncoll  Lmax IDef  Kgs  (Def(1,j),j=2,IDef,2)'
               WRITE (8,'(3x,3I5,1x,F5.1,1x,6(e10.3,1x))') NCOll(n),
     &                LMAx(n), IDEf(n), BANdk(n),
     &                (DDEf(n,k),k = 2,IDEf(n),2)
               WRITE (32,'(3x,3I5,1x,F5.1,1x,6(e10.3,1x))') NCOll(n),
     &                LMAx(n), IDEf(n), BANdk(n),
     &                (DDEf(n,k),k = 2,IDEf(n),2)
               READ (97,'(A80)') ch_iuf
               WRITE (96,'(3x,3I5,1x,F5.1,1x,6(e10.3,1x))') ND_nlv,
     &                LMAx(n), IDEf(n), BANdk(n),
     &                (DDEf(n,k),k = 2,IDEf(n),2)
               WRITE (8,*)
               WRITE (32,*)
               READ (97,'(A80)') ch_iuf
               WRITE (96,*)
               WRITE (8,*) ' N   E[MeV]  J   pi Nph L  K  Dyn.Def.'
               WRITE (32,*) ' N   E[MeV]  J   pi Nph L  K  Dyn.Def.'
               READ (97,'(A80)') ch_iuf
               WRITE (96,*) ' N   E[MeV]  J   pi Nph L  K  Dyn.Def.'
            ENDIF
         ENDDO
         k=1
   50    READ (97,'(A80)',END = 100) ch_iuf                                      
         WRITE (96,'(1x,I2,A77)') ICOllev(k),ch_iuf(4:80)
         k = k + 1
         GOTO 50
  100    CLOSE (96)
         CLOSE (97,STATUS = 'DELETE')
         IF (IOPsys.EQ.0) THEN
            ctmp = 'mv COLL.DAT TARGET_COLL.DAT'
            iwin = PIPE(ctmp)
         ELSE
            iwin = PIPE('move COLL.DAT TARGET_COLL.DAT')
         ENDIF
C
C--------JOIN finished
C
         DO k = 1, NCOll(ncalc)
C-----------The deformation for excited levels is not used in the pure
C-----------symm.rotational model but could be used for vibrational
C-----------rotational model so we are setting it to 0.01
            WRITE (32,
     &             '(1x,I2,1x,F7.4,1x,F4.1,1x,F3.0,1x,3(I2,1x),e10.3)')
     &             k, EEX(k,ncalc), SPIn(k,ncalc), FLOAT(IPAr(k,ncalc)),
     &             0, 0, 0, 0.01
            WRITE (8,'(1x,I2,1x,F7.4,1x,F4.1,1x,F3.0,1x,3(I2,1x),e10.3)'
     &             ) k, EEX(k,ncalc), SPIn(k,ncalc),
     &               FLOAT(IPAr(k,ncalc)), 0, 0, 0, 0.01
         ENDDO
         CLOSE (32)
         WRITE (8,*)
         WRITE (8,*)
      ENDIF
      IF (IMOdel.EQ.2 .AND. (.NOT.coll_defined)) THEN
C
C--------model = 'coupled-channels vibrational model'
C
         coll_defined = .TRUE.
         IF (NISotop.EQ.0) THEN
            WRITE (8,*) 'WARNING: None of the requested isotopes is '
            WRITE (8,*) 'WARNING: Included in the selected potential.'
            WRITE (8,*)
     &                 'WARNING: File with RIPL discrete levels can not'
            WRITE (8,*) 'WARNING: be created.                    '
            WRITE (8,*)
     &                 'WARNING: Default collective levels will be used'
            GOTO 300
         ENDIF
         ncalc = 0
         DO n = 1, NISotop
            IF (iainp.EQ.IA(n) .AND. izinp.EQ.IZ(n)) THEN
               ncalc = n
               DO k = 2, NVIb(ncalc)
                  IF (NPH(k,ncalc).EQ.3) THEN
                     IWArn = 6
                     WRITE (8,*) 'NPH(k,i)=3 !!! in RIPL OMP'
                     WRITE (8,*)
     &                          'Default collective levels will be used'
                     GOTO 300
                  ENDIF
               ENDDO
               IF (NVIb(n).GT.NDCOLLEV) THEN
                  WRITE (8,*) 'RIPL OMP collective levels are wrong'
                  WRITE (8,*) 'Too many levels'
                  WRITE (8,*) 'Default collective levels will be used'
                  IWArn = 6
                  GOTO 300
               ENDIF
            ENDIF
         ENDDO
         IF (ncalc.EQ.0) THEN
            WRITE (8,*) 'RIPL OMP collective levels are not used'
            WRITE (8,*) 'Default collective levels will be used'
            GOTO 300
         ENDIF
         IF (NCOll(ncalc).EQ.0) THEN
            WRITE (8,*) 'RIPL OMP collective levels are not used'
            WRITE (8,*) 'Default collective levels will be used'
            GOTO 300
         ENDIF
         nld_cc = 0
         DO k = 1, ND_nlv
            IF (ICOllev(k).LT.LEVcc) nld_cc = nld_cc + 1
         ENDDO
         IF (nld_cc.NE.NVIb(ncalc)) THEN
            WRITE (8,*) 'WARNING: Default number of coupled levels: ',
     &                  nld_cc
            WRITE (8,*) 'WARNING: is not equal ', NVIb(ncalc),
     &                  ' (used in CC RIPL OMP)'
         ENDIF
         WRITE (8,*)
         WRITE (8,*)
C
C--------Joining TARGET_COLL.DAT and TARGET_COLL_RIPL.DAT files
C
         OPEN (32,FILE = 'TARGET_COLL_RIPL.DAT')
         OPEN (97,FILE = 'TARGET_COLL.DAT')
         OPEN (96,FILE = 'COLL.DAT')
         WRITE (8,*)
     &           'Collective levels from RIPL CC OMP, vibrational model'
         WRITE (32,*)
     &           'Collective levels from RIPL CC OMP, vibrational model'
         READ (97,'(A80)') ch_iuf   ! FIRST LINE
         WRITE (96,'(A80)') ch_iuf
         WRITE (8,*) 'Dynamical deformations should be adjusted'
         WRITE (32,*) 'Dynamical deformations should be adjusted'
         READ (97,'(A80)') ch_iuf       ! 2ND LINE
         WRITE (96,'(A80)') 'Dynamical deformations should be adjusted'
         WRITE (8,'(1x,i3,1x,i3,a35)') izinp, iainp,
     &                                ' nucleus is treated as spherical'
         WRITE (32,'(1x,i3,1x,i3,a35)') izinp, iainp,
     &                                ' nucleus is treated as spherical'
         READ (97,'(A80)') ch_iuf       ! 3ER LINE
         WRITE (96,'(A80)') ch_iuf
         WRITE (8,*)
         WRITE (32,*)
         READ (97,'(A80)') ch_iuf       ! EMPTY LINE
         WRITE (96,'(A80)') ch_iuf
         DEFormed = .FALSE.
         DO n = 1, NISotop
            IF (iainp.EQ.IA(n) .AND. izinp.EQ.IZ(n)) THEN
               WRITE (8,*) '   Ncoll'
               WRITE (32,*) '   Ncoll'
               READ (97,'(A80)') ch_iuf
               WRITE (96,*) '   Ncoll'
               WRITE (8,'(3x,3I5,1x,F5.1,1x,6(e10.3,1x))') NCOll(n)
               WRITE (32,'(3x,3I5,1x,F5.1,1x,6(e10.3,1x))') NCOll(n)
               READ (97,'(A80)') ch_iuf
               WRITE (96,'(A80)') ND_nlv
               WRITE (8,*)
               WRITE (32,*)
               READ (97,'(A80)') ch_iuf
               WRITE (96,*)
               WRITE (8,*) ' N   E[MeV]  J   pi Nph L  K  Dyn.Def.'
               WRITE (32,*) ' N   E[MeV]  J   pi Nph L  K  Dyn.Def.'
               READ (97,'(A80)') ch_iuf
               WRITE (96,*) ' N   E[MeV]  J   pi Nph L  K  Dyn.Def.'
            ENDIF
         ENDDO
  150    READ (97,'(A80)',END = 200) ch_iuf
         WRITE (96,'(A80)') ch_iuf
         GOTO 150
  200    CLOSE (96)
         CLOSE (97,STATUS = 'DELETE')
         IF (IOPsys.EQ.0) THEN
            ctmp = 'mv COLL.DAT TARGET_COLL.DAT'
            iwin = PIPE(ctmp)
         ELSE
            iwin = PIPE('move COLL.DAT TARGET_COLL.DAT')
         ENDIF
C
C--------JOIN finished 
C
         DO k = 1, NVIb(ncalc)
            WRITE (32,
     &             '(1x,I2,1x,F7.4,1x,F4.1,1x,F3.0,1x,3(I2,1x),e10.3)')
     &             k, EXV(k,ncalc), SPInv(k,ncalc), FLOAT(IPAr(k,ncalc))
     &             , NPH(k,ncalc), 0, 0, DEFv(k,ncalc)
            WRITE (8,'(1x,I2,1x,F7.4,1x,F4.1,1x,F3.0,1x,3(I2,1x),e10.3)'
     &             ) k, EXV(k,ncalc), SPInv(k,ncalc),
     &               FLOAT(IPAr(k,ncalc)), NPH(k,ncalc), 0, 0,
     &               DEFv(k,ncalc)
         ENDDO
         CLOSE (32)
         WRITE (8,*)
         WRITE (8,*)
      ENDIF

      IF (IMOdel.EQ.3 .AND. (.NOT.coll_defined)) THEN
C
C--------model = 'coupled-channels soft rotor model'
C
         coll_defined = .TRUE.
         IF (NISotop.EQ.0 .AND. FIRst_ein) THEN
            WRITE (8,*) 'WARNING: None of the requested isotopes is '
            WRITE (8,*)
     &               'WARNING: included in the selected RIPL potential.'
            WRITE (8,*)
     &                 'WARNING: No default hamiltonian is available'
            WRITE (8,*)'WARNING: for the soft rotor model'
            STOP       'ERROR: EMPIRE stops'
         ENDIF
         ncalc = 0
         DO n = 1, NISotop
            IF (iainp.EQ.IA(n) .AND. izinp.EQ.IZ(n)) THEN
               ncalc = n
               IF (IDEf(n).GT.2*LMAx(n) .OR. NCOll(n).GT.NDCOLLEV) THEN
                  WRITE (8,*) 'WARNING: OMP collective levels are wrong'
                  WRITE (8,*) 'WARNING: Too many levels or deformations'
                  WRITE (8,*)
     &                 'WARNING: Default collective levels will be used'
                  IWArn = 6
                  GOTO 300
               ENDIF
            ENDIF
         ENDDO
         IF (ncalc.EQ.0) THEN
            WRITE (8,*)
     &                'WARNING: RIPL OMP collective levels are not used'
            WRITE (8,*)
     &                 'WARNING: Default collective levels will be used'
            GOTO 300
         ENDIF
         IF (NCOll(ncalc).EQ.0) THEN
            WRITE (8,*)
     &                'WARNING: RIPL OMP collective levels are not used'
            WRITE (8,*)
     &                 'WARNING: Default collective levels will be used'
            GOTO 300
         ENDIF
C--------Setting EMPIRE global variables
         nld_cc = 0
         DO k = 1, ND_nlv
            IF (ICOllev(k).LT.LEVcc) nld_cc = nld_cc + 1
         ENDDO
         WRITE (8,*)
         WRITE (8,*)
         WRITE (8,*) 'Deformation of the gsb adopted from CC RIPL OMP'
         LMAxcc = LMAx(ncalc)
         IDEfcc = IDEf(ncalc)
         DO k = 2, IDEfcc, 2
            D_Def(1,k) = DDEf(ncalc,k)
         ENDDO
         IF (nld_cc.NE.NCOll(ncalc)) THEN
            WRITE (8,*) 'WARNING: Default number of coupled levels: ',
     &                  nld_cc
            WRITE (8,*) 'WARNING: is not equal ', NCOll(ncalc),
     &                  ' (used in CC RIPL OMP)'
         ENDIF
C
C--------Joining TARGET_COLL.DAT and TARGET_COLL_RIPL.DAT files
C
         OPEN (32,FILE = 'TARGET_COLL_RIPL.DAT')
         OPEN (97,FILE = 'TARGET_COLL.DAT')
         OPEN (96,FILE = 'COLL.DAT')
         WRITE (8,*)
     &       'Collective levels from RIPL CC OMP, symm.rotational model'
         WRITE (32,*)
     &       'Collective levels from RIPL CC OMP, symm.rotational model'
         READ (97,'(A80)') ch_iuf   ! FIRST LINE
         WRITE (96,'(A80)') ch_iuf
         WRITE (8,*) 'Dyn.deformations are not used in symm.rot.model'
         WRITE (32,*) 'Dyn.deformations are not used in symm.rot.model'
         READ (97,'(A80)') ch_iuf       ! 2ND LINE
         WRITE (96,'(A80)') ch_iuf
         WRITE (8,'(1x,i3,1x,i3,a35)') izinp, iainp,
     &                                 ' nucleus is treated as deformed'
         WRITE (32,'(1x,i3,1x,i3,a35)') izinp, iainp,
     &                                 ' nucleus is treated as deformed'
         READ (97,'(A80)') ch_iuf       ! 3ER LINE
         WRITE (96,'(A80)') ch_iuf
         WRITE (8,*)
         WRITE (32,*)
         READ (97,'(A80)') ch_iuf       ! EMPTY LINE
         WRITE (96,'(A80)') ch_iuf
         DEFormed = .TRUE.
         DO n = 1, NISotop
            IF (iainp.EQ.IA(n) .AND. izinp.EQ.IZ(n)) THEN
               WRITE (8,*)
     &                 '   Ncoll  Lmax IDef  Kgs  (Def(1,j),j=2,IDef,2)'
               WRITE (32,*)
     &                 '   Ncoll  Lmax IDef  Kgs  (Def(1,j),j=2,IDef,2)'
               READ (97,'(A80)') ch_iuf
               WRITE (96,*)
     &                 '   Ncoll  Lmax IDef  Kgs  (Def(1,j),j=2,IDef,2)'
               WRITE (8,'(3x,3I5,1x,F5.1,1x,6(e10.3,1x))') NCOll(n),
     &                LMAx(n), IDEf(n), BANdk(n),
     &                (DDEf(n,k),k = 2,IDEf(n),2)
               WRITE (32,'(3x,3I5,1x,F5.1,1x,6(e10.3,1x))') NCOll(n),
     &                LMAx(n), IDEf(n), BANdk(n),
     &                (DDEf(n,k),k = 2,IDEf(n),2)
               READ (97,'(A80)') ch_iuf
               WRITE (96,'(3x,3I5,1x,F5.1,1x,6(e10.3,1x))') ND_nlv,
     &                LMAx(n), IDEf(n), BANdk(n),
     &                (DDEf(n,k),k = 2,IDEf(n),2)
               WRITE (8,*)
               WRITE (32,*)
               READ (97,'(A80)') ch_iuf
               WRITE (96,*)
               WRITE (8,*) ' N   E[MeV]  J   pi Nph L  K  Dyn.Def.'
               WRITE (32,*) ' N   E[MeV]  J   pi Nph L  K  Dyn.Def.'
               READ (97,'(A80)') ch_iuf
               WRITE (96,*) ' N   E[MeV]  J   pi Nph L  K  Dyn.Def.'
            ENDIF
         ENDDO
  350    READ (97,'(A80)',END = 400) ch_iuf
         WRITE (96,'(A80)') ch_iuf
         GOTO 350
  400    CLOSE (96)
         CLOSE (97,STATUS = 'DELETE')
         IF (IOPsys.EQ.0) THEN
            ctmp = 'mv COLL.DAT TARGET_COLL.DAT'
            iwin = PIPE(ctmp)
         ELSE
            iwin = PIPE('move COLL.DAT TARGET_COLL.DAT')
         ENDIF
C
C--------JOIN finished: TARGET_COLL.DAT and TARGET_COLL_RIPL.DAT files
C
         DO k = 1, NCOll(ncalc)
C-----------The deformation for excited levels is not used in the pure
C-----------symm.rotational model but could be used for vibrational
C-----------rotational model so we are setting it to 0.01
            WRITE (32,
     &             '(1x,I2,1x,F7.4,1x,F4.1,1x,F3.0,1x,3(I2,1x),e10.3)')
     &             k, EEX(k,ncalc), SPIn(k,ncalc), FLOAT(IPAr(k,ncalc)),
     &             0, 0, 0, 0.01
            WRITE (8,'(1x,I2,1x,F7.4,1x,F4.1,1x,F3.0,1x,3(I2,1x),e10.3)'
     &             ) k, EEX(k,ncalc), SPIn(k,ncalc),
     &               FLOAT(IPAr(k,ncalc)), 0, 0, 0, 0.01
         ENDDO
         CLOSE (32)
         WRITE (8,*)
         WRITE (8,*)
      ENDIF
C
C-----END OF SETTING COLLECTIVE LEVELS for DIRECT CALCULATIONS
C
  300 IF (iainp.LT.IAMin .OR. iainp.GT.IAMax) IWArn = 1
      IF (izinp.LT.IZMin .OR. izinp.GT.IZMax) IWArn = 2
      IF (E.LT.EEMin) THEN
         EEMin = E + 0.1D0
         IWArn = 3
      ENDIF
      IF (E.GT.EEMax) THEN
         EEMax = E + 1000.D0
         IWArn = 4
      ENDIF
      OMEmin(Nejc,Nnuc) = EEMin
      OMEmax(Nejc,Nnuc) = EEMax
C     IRElat(Nejc,Nnuc) = IREl
      xmas_nejc = (AEJc(Nejc)*AMUmev + XMAss_ej(Nejc))/AMUmev
      xmas_nnuc = (A(Nnuc)*AMUmev + XMAss(Nnuc))/AMUmev
C
C-----INITIALIZING /RIPLXX
C
      ETA = (XN(Nnuc) - Z(Nnuc))/A(Nnuc)
      ATAr = iainp
      ZTAr = izinp
      TARmas = xmas_nnuc
      PROjmas = xmas_nejc
      HBArc = HHBarc
      AMU0c2 = AMUmev
      ENCoul = 0.4*ZTAr/ATAr**(1./3.)
      EFErmi = EEFermi(Nejc,Nnuc)

      IF(KTRlom(Nejc,Nnuc).NE.9602) then
        CALL OPTMOD(E,vlib,rlib,alib,Nejc,Nnuc,ndejc,ndnuc
     &  ,FNrvomp,FNavomp,FNvvomp,FNrwvomp,FNrwvomp,FNwvomp
     &  ,FNwsomp,FNrsomp,FNasomp)
      ELSE
        EcollTarget = -QCC(1)  ! energy of the collective state   
        CALL KUMAR_OMP(A(NNUC),Z(NNUC),EcollTarget,
     >                 E,RCCC,vlib,rlib,alib)
        RC = RCCC  
      ENDIF
C
C-----RIPL-II
C-----1. The ordering of potentials has been changed to follow ECIS96:
C
C-----i=1         real volume potential
C-----i=2         imaginary volume potential
C-----i=3         real surface derivative potential
C-----i=4         imaginary surface derivative potential
C-----i=5         real spin-orbit potential
C-----i=6         imaginary spin-orbit potential
C
C-----2. The number of terms possible in the potential strengths has
C-----been increased from 21 to 24.  The special case potentials
C-----are now:
C
C-----pot(i,j,22) - Smith et al. , OMP reference # 118
C-----pot(i,j,23) - Varner et al., OMP reference # 2100, 5100
C-----pot(i,j,24) - Koning potentials, reference # 2404, 2405 ...
C
      RCOul(Nejc,Nnuc) = RC
      ACOul(Nejc,Nnuc) = ACOu
C-----Volume real potential: Woods-Saxon
      VOM(Nejc,Nnuc) = vlib(1)*FNvvomp(Nejc,Nnuc,0)
      RVOm(Nejc,Nnuc) = rlib(1)*FNrvomp(Nejc,Nnuc,0)
      AVOm(Nejc,Nnuc) = alib(1)*FNavomp(Nejc,Nnuc,0)
C-----Volume imaginary potential: Woods-Saxon
      WOMv(Nejc,Nnuc) = vlib(2)*FNwvomp(Nejc,Nnuc,0)
c      write(0,*) WOMv(Nejc,Nnuc),vlib(2)
      RWOmv(Nejc,Nnuc) = rlib(2)*FNrwvomp(Nejc,Nnuc,0)
      AWOmv(Nejc,Nnuc) = alib(2)*FNawvomp(Nejc,Nnuc,0)
C-----Real surface contribution
      VOMs(Nejc,Nnuc) = vlib(3)*FNwsomp(Nejc,Nnuc,0)
C-----Surface imaginary potential:

      WOMs(Nejc,Nnuc) = vlib(4)*FNwsomp(Nejc,Nnuc,0)
      RWOm(Nejc,Nnuc) = rlib(4)*FNrsomp(Nejc,Nnuc,0)
      AWOm(Nejc,Nnuc) = alib(4)*FNasomp(Nejc,Nnuc,0)
      SFIom(Nejc,Nnuc) = 1.D0
C-----if rco(4,1,1) >0.0: Woods-Saxon derivative surface potential
C-----if rco(4,1,1) <0.0: Gaussian surface potential.
      IF (RCO(4,1,1).LT.0.0) SFIom(Nejc,Nnuc) = -1.D0
C-----Real spin-orbit
C-----****
      VSO(Nejc,Nnuc) = vlib(5)
      RVSo(Nejc,Nnuc) = rlib(5)
      AVSo(Nejc,Nnuc) = alib(5)
C-----Imaginary spin-orbit
C-----****
      WSO(Nejc,Nnuc) = vlib(6)
      RWSo(Nejc,Nnuc) = rlib(6)
      AWSo(Nejc,Nnuc) = alib(6)
      END


      SUBROUTINE OMPAR(Nejc,Nnuc,Eilab,Eicms,Mi,Mt,Ak2,Komp,Ikey)
      INCLUDE 'dimension.h'
      INCLUDE 'global.h'
C
C COMMON variables
C
      INTEGER MODelcc
      COMMON /LOCAL / MODelcc
C
C Dummy arguments
C
      DOUBLE PRECISION Ak2, Eicms, Eilab, Mi, Mt
      INTEGER Ikey, Komp, Nejc, Nnuc
C
C Local variables
C
      DOUBLE PRECISION ener
      LOGICAL fexist, relcal
      INTEGER iaejcr, ianucr, ieof, ierr, iowrite, ipoten, irel, ki
      INTEGER INT
      CHARACTER*2 symbejcr, symbnucr
C
C-----IKEY < 0  :   EIlab is given
C-----IKEY > 0  :   EIcms is given
C
C-----Sets optical model parameters
C
C-----komp   = 29 (usually), 33(for the inelastic channel)
C
C-----Erasing parameters of O.M.P.
      VOM(Nejc,Nnuc)  = 0.D0
      VOMs(Nejc,Nnuc) = 0.D0
      WOMv(Nejc,Nnuc) = 0.D0
      WOMs(Nejc,Nnuc) = 0.D0
      WSO(Nejc,Nnuc)  = 0.D0
      VSO(Nejc,Nnuc)  = 0.D0
      RVOm(Nejc,Nnuc) = 0.D0
      RWOm(Nejc,Nnuc) = 0.D0
      RVSo(Nejc,Nnuc) = 0.D0
      RWSo(Nejc,Nnuc) = 0.D0
      AVOm(Nejc,Nnuc) = 0.D0
      AWOm(Nejc,Nnuc) = 0.D0
      AVSo(Nejc,Nnuc) = 0.D0
      AWSo(Nejc,Nnuc) = 0.D0
C-----set nonlocality range to 0. (as usually is the case)
      RNOnl(Nejc,Nnuc) = 0.D0
C-----set validity range to any energy (can be modified later)
      OMEmin(Nejc,Nnuc) = 0.D0
      OMEmax(Nejc,Nnuc) = 1000.D0
C-----set relativistic calculation key to 0
      IRElat(Nejc,Nnuc) = 0
      fexist = OMPar_riplf
      IF (CCCalc) fexist = OMParfcc
C-----OMPAR.RIPL file exists ?
      IF (fexist) THEN
C--------komp = 29 OR 33
         ipoten = KTRlom(Nejc,Nnuc)
         CALL FINDPOT_OMPAR_RIPL(Komp,ieof,ipoten,Nnuc,Nejc)
         IF (ieof.EQ.0) THEN
C-----------Reading potential parameters from OMPAR.RIPL(OMPAR.DIR) file
            CALL READ_OMPAR_RIPL(Komp,ierr,irel)
            IRElat(Nejc,Nnuc) = irel
            IF(Komp.eq.33) IRElat(0,0) = irel
            IF (ierr.EQ.0) GOTO 100
         ENDIF
C--------ieof<>0 or ierr<>0
         IF (IOUT.EQ.5) THEN
           WRITE (8,*) ' '
           IF (CCCalc) THEN
             WRITE (8,
     &'('' OM parameters for '',I3,''-'',A2,'' +'',I2,''-''        ,A2,'
     &' not found in the OMPAR.DIR file'')') INT(A(Nnuc)), SYMb(Nnuc),
     &INT(AEJc(Nejc)), SYMbe(Nejc)
           ELSE
             WRITE (8,
     &'('' OM parameters for '',I3,''-'',A2,'' +'',I2,''-''        ,A2,'
     &' not found in the OMPAR.RIPL file'')') INT(A(Nnuc)), SYMb(Nnuc),
     &INT(AEJc(Nejc)), SYMbe(Nejc)
           ENDIF
           WRITE (8,*) 'I will resort to parameters from RIPL database'
           WRITE (8,*) ' '
         ENDIF
      ENDIF
C
C-----OMPAR.RIPL(OMPAR.DIR) file does not exists
C-----or reading error happened
C-----(reading from the RIPL database)
C
      ipoten = KTRlom(Nejc,Nnuc)
      ki = 26
C-----Searching in the RIPL database for IPOTEN catalog number
      CALL FINDPOT(ki,ieof,ipoten)
C-----Here ieof must be 0 always because we checked in input.f
      IF (ieof.GT.0) THEN
         WRITE (8,*) 'ERROR: PROBLEM with OMPAR.DIR library,RIPL #',
     &               ipoten
         STOP 'ERROR: PROBLEM with OMPAR.DIR library'
      ENDIF
C-----Reading o.m.  potential parameters for IPOTEN catalog number
      CALL OMIN(ki,ieof,irel)
      IRElat(Nejc,Nnuc) = irel
C
C-----Ener must be in LAB system
C
  100 relcal = .FALSE.
      IF (IRElat(Nejc,Nnuc).GT.0  .or. RELkin) relcal = .TRUE.
      IF (Ikey.LT.0) THEN
         ener = Eilab
C        Commented on Aug. 2008, energy Eilab already in Lab frame 
C        CALL KINEMA(Eilab,Eicms,Mi,Mt,Ak2,1,relcal)
      ELSE
         CALL KINEMA(Eilab,Eicms,Mi,Mt,Ak2,2,relcal)
         ener = Eilab
      ENDIF
      CALL RIPL2EMPIRE(Nejc,Nnuc,ener)
      IF (CCCalc) MODelecis = MODelcc
C     Imaginary potentials must be positive, RCN
      WOMs(Nejc,Nnuc) = MAX(WOMs(Nejc,Nnuc),0.D0)
      WOMv(Nejc,Nnuc) = MAX(WOMv(Nejc,Nnuc),0.D0)
C-----Some default protections
C-----set coulomb radius equal to 1.25 if not defined
C     IF (RCOul(Nejc,Nnuc).EQ.0.0D0 .AND. Zejc(0).GT.0) 
      IF (RCOul(Nejc,Nnuc).EQ.0.0D0) RCOul(Nejc,Nnuc) = 1.25
C-----set volume imaginary diff. equal to surface imag. diff. if not defined
      IF (AWOmv(Nejc,Nnuc).EQ.0.0D0) AWOmv(Nejc,Nnuc) = AWOm(Nejc,Nnuc)
C-----set volume imaginary radius equal to surface imag. radius if not defined
      IF (RWOmv(Nejc,Nnuc).EQ.0.0D0) RWOmv(Nejc,Nnuc) = RWOm(Nejc,Nnuc)
C
C-----write O.M. potential parameters to the file OMPAR=>*.omp
C-----only if .omp file did not exist and IOMWRITE is even (to avoid repetion)
C
      iowrite = IOMwrite(Nejc,Nnuc)
      fexist = OMPar_riplf
      IF (CCCalc) THEN
         iowrite = IOMwritecc
         fexist = OMParfcc
      ENDIF
      IF (MOD(iowrite,2).EQ.0 .AND. .NOT.fexist) THEN
         ki = 26
C--------komp = 29 OR 33
         ipoten = KTRlom(Nejc,Nnuc)
         CALL FINDPOT(ki,ieof,ipoten)
         IF (ieof.GT.0) THEN
            WRITE (8,*) 'ERROR: PROBLEM with OMPAR.RIPL library,RIPL #',
     &                  ipoten
            STOP 'ERROR: PROBLEM with OMPAR.RIPL library'
         ENDIF
C--------Reading IPOTEN catalog number potential parameters
         ianucr = INT(A(Nnuc))
         symbnucr = SYMb(Nnuc)
         iaejcr = INT(AEJc(Nejc))
         symbejcr = SYMbe(Nejc)
         CALL CREATE_OMPAR(ki,Komp,ieof,Nnuc,Nejc,ianucr,symbnucr,
     &                     iaejcr,symbejcr)
         IF (CCCalc) THEN
C-----------increase IOMWRITE so that all these is not written again and again
            IOMwritecc = 1
         ELSE
C-----------increase IOMWRITE so that all these is not written again and again
            IOMwrite(Nejc,Nnuc) = IOMwrite(Nejc,Nnuc) + 1
         ENDIF
      ENDIF
C-----O.M. potential parameters written
      END


      SUBROUTINE CREATE_OMPAR(Ki,Komp,Ieof,Nnucr,Nejcr,Ianucr,Symbnucr,
     &                        Iaejcr,Symbejcr)
      INCLUDE 'ripl2empire.h'
C
C
C Dummy arguments
C
      INTEGER Iaejcr, Ianucr, Ieof, Ki, Komp, Nejcr, Nnucr
      CHARACTER*2 Symbejcr, Symbnucr
C
C Local variables
C
      CHARACTER*80 comment
      INTEGER i, j, krange, l
      INTEGER IABS
      CHARACTER*10 potnam(6)
C
C-----Read optical model parameters from the RIPL-II library and create local
C-----omp file
C
      DATA potnam/' Real vol.', ' Imag vol.', ' Real surf',
     &     ' Imag surf', ' Real SO  ', ' Imag SO  '/
      Ieof = 0
      READ (Ki,*) IREf
      WRITE (Komp,'(3I5,4x,I3,''-'',A2,'' +'',I3,''-'',A2)') IREf,
     &       Nnucr, Nejcr, Ianucr, Symbnucr, Iaejcr, Symbejcr
      DO l = 2, 7
         READ (Ki,'(a80)') comment
         WRITE (Komp,'(a80)') comment
      ENDDO
C-----1   read(ki,*,end=999) iref
C-----2   read(ki,1) (author(i),i=1,80)
C-----3   read(ki,1) (refer(i),i=1,80)
C-----4-7 read(ki,1) (summary(i),i=1,320)
C     read(ki,*) emin,emax
      READ (Ki,'(a80)') comment
      WRITE (Komp,'(a80,a10)') comment, ' Emin,Emax'
C     read(ki,*) izmin,izmax
      READ (Ki,'(a80)') comment
      WRITE (Komp,'(a80,a10)') comment, ' Zmin,Zmax'
C     read(ki,*) iamin,iamax
      READ (Ki,'(a80)') comment
      WRITE (Komp,'(a80,a10)') comment, ' Amin,Amax'
C     read(ki,*) imodel,izproj,iaproj,irel,idr
      READ (Ki,'(a80)') comment
      WRITE (Komp,'(a80,a)') comment, ' Mod,Zp,Ap,irel,idr'
      DO i = 1, 6
         READ (Ki,'(a80)') comment
         WRITE (Komp,'(a80,a10,a10)') comment, ' NEranges ', potnam(i)
         BACKSPACE (Ki)
         READ (Ki,*) JRAnge(i)
         IF (JRAnge(i).NE.0) THEN
            krange = IABS(JRAnge(i))
            DO j = 1, krange
C              read(ki,*) epot(i,j)
               READ (Ki,'(a80)') comment
               WRITE (Komp,'(a80,a19)') comment, ' Upper energy limit'
C              read(ki,*) (rco(i,j,n),n=1,ndim2)
               READ (Ki,'(a80)') comment
               WRITE (Komp,'(a80,a10)') comment, ' Radius(E)'
C--------------Assumed ndim2 must be greater than 7 and less than 13
C--------------Otherwise delete the following two lines
               READ (Ki,'(a80)') comment
               WRITE (Komp,'(a80,a10)') comment, ' Radius(E)'
C              read(ki,*) (aco(i,j,n),n=1,ndim2)
               READ (Ki,'(a80)') comment
               WRITE (Komp,'(a80,a10)') comment, ' Diffus(E)'
C--------------Assumed ndim2 must be greater than 7 and less than 13
C--------------Otherwise delete the following two lines
               READ (Ki,'(a80)') comment
               WRITE (Komp,'(a80,a10)') comment, ' Diffus(E)'
C              read(ki,*) (pot(i,j,n),n=1,ndim3)
               READ (Ki,'(a80)') comment
               WRITE (Komp,'(a80,a10)') comment, ' Vdepth(E)'
               READ (Ki,'(a80)') comment
               WRITE (Komp,'(a80,a10)') comment, ' Vdepth(E)'
               READ (Ki,'(a80)') comment
               WRITE (Komp,'(a80,a10)') comment, ' Vdepth(E)'
C--------------Assumed ndim3 must be greater than 19 !!!
C--------------Otherwise delete the following two lines
               READ (Ki,'(a80)') comment
               WRITE (Komp,'(a80,a10)') comment, ' Vdepth(E)'
            ENDDO
         ENDIF
      ENDDO
      READ (Ki,'(a80)') comment
      WRITE (Komp,'(a80,a20)') comment, ' Coul.ranges (jcoul)'
      BACKSPACE (Ki)
      READ (Ki,*) JCOul
      IF (JCOul.GT.0) THEN
         DO j = 1, JCOul
            READ (Ki,'(a80)') comment
            WRITE (Komp,'(a80,a24)') comment,' Ec,R0,R,R1,R2,bet,Ac,R3'
         ENDDO
      ENDIF
      IF (IMOdel.GT.0) THEN
C
C--------To avoid repetition of discrete level information
C--------we are not writing here to OMPAR.*
C--------Instead a TARGET_LEV.COLL file is used
C
         NISotop = 0
         WRITE (Komp,'(1x,i4)') NISotop
      ENDIF
      WRITE (Komp,'(A8)') '++++++++'
      END


      SUBROUTINE READ_OMPAR_RIPL(Ko,Ierr,Irelout)
      INCLUDE 'ripl2empire.h'
C
C
C Dummy arguments
C
      INTEGER Ierr, Irelout, Ko
C
C Local variables
C
      CHARACTER*80 comment
      INTEGER i, j, k, krange, n
      INTEGER IABS
C
C-----Read RIPL optical model parameters from the local OMPAR.RIPL file
C
      Ierr = 0
      READ (Ko,'(I5)',ERR = 200) IREf
      READ (Ko,99050,ERR = 200) AUThor
      READ (Ko,99050,ERR = 200) REFer
      READ (Ko,99050,ERR = 200) SUMmary
      READ (Ko,99025,ERR = 200) EMIn, EMAx
      READ (Ko,99020,ERR = 200) IZMin, IZMax
      READ (Ko,99020,ERR = 200) IAMin, IAMax
      READ (Ko,99020,ERR = 200) IMOdel, IZProj, IAProj, IREl, IDR
      Irelout = IREl
      DO i = 1, 6
         READ (Ko,99020,ERR = 200) JRAnge(i)
         IF (JRAnge(i).NE.0) THEN
            krange = IABS(JRAnge(i))
            DO j = 1, krange
               READ (Ko,99025,ERR = 200) EPOt(i,j)
C--------------Reading radius
               READ (Ko,99030) (RCO(i,j,n),n = 1,7)
               IF (NDIM2.GT.7 .AND. NDIM2.LE.13) THEN
                  READ (Ko,99035,ERR = 200) (RCO(i,j,n),n = 8,NDIM2)
               ELSE
                  READ (Ko,99035,ERR = 200) (RCO(i,j,n),n = 8,13)
               ENDIF
               IF (NDIM2.GT.13) STOP
     &                 '1 IN tl.f, UNCOMMENT READ ABOVE AND REMOVE STOP'
C--------------Reading diffuss
               READ (Ko,99030,ERR = 200) (ACO(i,j,n),n = 1,7)
               IF (NDIM2.GT.7 .AND. NDIM2.LE.13) THEN
                  READ (Ko,99035,ERR = 200) (ACO(i,j,n),n = 8,NDIM2)
               ELSE
                  READ (Ko,99035,ERR = 200) (ACO(i,j,n),n = 8,13)
               ENDIF
               IF (NDIM2.GT.13) STOP
     &                 '2 IN tl.f, UNCOMMENT READ ABOVE AND REMOVE STOP'
C--------------Reading depths
               READ (Ko,99030,ERR = 200) (POT(i,j,n),n = 1,7)
               IF (NDIM3.GT.7 .AND. NDIM3.LE.13) THEN
                  READ (Ko,99035,ERR = 200) (POT(i,j,n),n = 8,NDIM3)
               ELSE
                  READ (Ko,99035,ERR = 200) (POT(i,j,n),n = 8,13)
               ENDIF
               IF (NDIM3.GT.13 .AND. NDIM3.LE.19) THEN
                  READ (Ko,99035,ERR = 200) (POT(i,j,n),n = 14,NDIM3)
               ELSE
                  READ (Ko,99035,ERR = 200) (POT(i,j,n),n = 14,19)
               ENDIF
               IF (NDIM3.GT.19) READ (Ko,99035,ERR = 200)
     &                                (POT(i,j,n),n = 20,NDIM3)
            ENDDO
         ENDIF
      ENDDO
      READ (Ko,99020,ERR = 200) JCOul
      IF (JCOul.GT.0) THEN
         DO j = 1, JCOul
            READ (Ko,99025,ERR = 200) ECOul(j), RCOul0(j), RCOul(j),
     &       RCOul1(j), RCOul2(j), BETa(j), ACOul(j), RCOul3(j)
         ENDDO
      ENDIF
      IF (IMOdel.EQ.1) THEN
C        Reading rigid rotor model parameters    
         READ (Ko,99020,ERR = 200) NISotop
         DO n = 1, NISotop
            IF (IDEf(n).LE.8) THEN
               READ (Ko,99040,ERR = 200) IZ(n), IA(n), NCOll(n), LMAx(n)
     &               , IDEf(n), BANdk(n), (DEF(n,k),k = 2,IDEf(n),2)
            ELSE
               READ (Ko,99040,ERR = 200) IZ(n), IA(n), NCOll(n), LMAx(n)
     &               , IDEf(n), BANdk(n), (DEF(n,k),k = 2,8,2)
            ENDIF
            IF (IDEf(n).GE.8) READ (Ko,99010,ERR = 200)
     &                              (DEF(n,k),k = 2,IDEf(n),2)
            DO k = 1, NCOll(n)
               READ (Ko,99045,ERR = 200) EX(k,n), SPIn(k,n), IPAr(k,n)
            ENDDO
         ENDDO
      ELSEIF (IMOdel.EQ.2) THEN
C        Reading vibrational model parameters    
         READ (Ko,99020,ERR = 200) NISotop
         DO n = 1, NISotop
            READ (Ko,99020,ERR = 200) IZ(n), IA(n), NVIb(n)
            DO k = 1, NVIb(n)
               READ (Ko,99045,ERR = 200) EXV(k,n), SPInv(k,n),
     &               IPArv(k,n), NPH(k,n), DEFv(k,n), THEtm(k,n)
            ENDDO
         ENDDO
      ELSEIF (IMOdel.EQ.3) THEN
C        Reading soft rotor model parameters    
         READ (Ko,99020,ERR = 200) NISotop
         DO n = 1, NISotop
            READ (Ko,99020,ERR = 200) IZ(n), IA(n), NCOll(n)
            read(ko,99047,ERR = 200)  ! Record 3 from OPTMAN (Hamiltonian parameters)
     &                    SR_hw(n),SR_amb0(n),SR_amg0(n),
     &                    SR_gam0(n),SR_bet0(n),SR_bet4(n)
            read(ko,99047,ERR = 200)  ! Record 4 from OPTMAN (Hamiltonian parameters)
     &                    SR_bb42(n),SR_gamg(n),SR_delg(n),
     &                    SR_bet3(n),SR_et0(n),SR_amu0(n)
            read(ko,99047,ERR = 200)  ! Record 5 from OPTMAN (Hamiltonian parameters)
     &                    SR_hw0(n),SR_bb32(n),SR_gamde(n),
     &                    SR_dpar(n),SR_gshape(n)
            do k=1,NCOll(n)
              read(ko,99049) exv(k,n),spinv(k,n),iparv(k,n),
     +               SR_ntu(k,n),SR_nnb(k,n),SR_nng(k,n),SR_nno(k,n)
            enddo
         ENDDO
      ENDIF
      READ (Ko,'(A80)',END = 100) comment
  100 RETURN
  200 Ierr = 1
99010 FORMAT (30x,4(1x,e10.3))
99015 FORMAT (2I5,1p,3(1x,e11.4))
99020 FORMAT (10I5)
99025 FORMAT (8F10.3)
99030 FORMAT (f12.5,1x,6(e11.4))
99035 FORMAT (13x,6(e11.4))
99040 FORMAT (5I5,f5.1,4(1x,e10.3))
99045 FORMAT (f12.8,f7.1,2I4,1p,2(1x,e11.4))
99047 FORMAT (6(e11.5,1x))
99049 FORMAT (f12.8,f7.1,I4,4I2)
99050 FORMAT (80A1)
      END


      SUBROUTINE OMIN(Ki,Ieof,Irelout)
C
C     routine to read optical model parameters
C
      INCLUDE 'ripl2empire.h'
C
C Dummy arguments
C
      INTEGER Ieof, Irelout, Ki
C
C Local variables
C
      INTEGER i, j, k, krange, n
      INTEGER IABS
      CHARACTER*80 idum
      Ieof = 0
      READ (Ki,*,END = 100) IREf
      READ (Ki,99005) (AUThor(i),i = 1,80)
      READ (Ki,99005) (REFer(i),i = 1,80)
      READ (Ki,99005) (SUMmary(i),i = 1,320)
      READ (Ki,*) EMIn, EMAx
      READ (Ki,*) IZMin, IZMax
      READ (Ki,*) IAMin, IAMax
      READ (Ki,*) IMOdel, IZProj, IAProj, IREl, IDR
      Irelout = IREl
      DO i = 1, 6
         READ (Ki,*) JRAnge(i)
         IF (JRAnge(i).NE.0) THEN
            krange = IABS(JRAnge(i))
            DO j = 1, krange
               READ (Ki,*) EPOt(i,j)
               READ (Ki,*) (RCO(i,j,n),n = 1,NDIM2)
               READ (Ki,*) (ACO(i,j,n),n = 1,NDIM2)
               READ (Ki,*) (POT(i,j,n),n = 1,NDIM3)
            ENDDO
         ENDIF
      ENDDO
      READ (Ki,*) JCOul
      IF (JCOul.GT.0) THEN
         DO j = 1, JCOul
            READ (Ki,*) ECOul(j), RCOul0(j), RCOul(j), RCOul1(j),
     &                  RCOul2(j), BETa(j), ACOul(j), RCOul3(j)
         ENDDO
      ENDIF
      IF (IMOdel.EQ.1) THEN
         READ (Ki,*) NISotop
         DO n = 1, NISotop
            READ (Ki,*) IZ(n), IA(n), NCOll(n), LMAx(n), IDEf(n),
     &                  BANdk(n), (DEF(n,k),k = 2,IDEf(n),2)
            DO k = 1, NCOll(n)
               READ (Ki,*) EX(k,n), SPIn(k,n), IPAr(k,n)
            ENDDO
         ENDDO
      ELSEIF (IMOdel.EQ.2) THEN
         READ (Ki,*) NISotop
         DO n = 1, NISotop
            READ (Ki,*) IZ(n), IA(n), NVIb(n)
            DO k = 1, NVIb(n)
               READ (Ki,*) EXV(k,n), SPInv(k,n), IPArv(k,n), NPH(k,n),
     &                     DEFv(k,n), THEtm(k,n)
            ENDDO
         ENDDO
      ELSEIF (IMOdel.EQ.3) THEN
         READ (Ki,*) NISotop
         DO n = 1, NISotop
            READ (Ki,*) IZ(n), IA(n), NCOll(n)
            read(ki,*)  ! Record 3 from OPTMAN (Hamiltonian parameters)
     &                    SR_hw(n),SR_amb0(n),SR_amg0(n),
     &                    SR_gam0(n),SR_bet0(n),SR_bet4(n)
            read(ki,*)  ! Record 4 from OPTMAN (Hamiltonian parameters)
     &                    SR_bb42(n),SR_gamg(n),SR_delg(n),
     &                    SR_bet3(n),SR_et0(n),SR_amu0(n)
            read(ki,*)  ! Record 5 from OPTMAN (Hamiltonian parameters)
     &                    SR_hw0(n),SR_bb32(n),SR_gamde(n),
     &                    SR_dpar(n),SR_gshape(n)
            do k=1,NCOll(n)
              read(ki,*) exv(k,n),spinv(k,n),iparv(k,n),
     +               SR_ntu(k,n),SR_nnb(k,n),SR_nng(k,n),SR_nno(k,n)
            enddo
         ENDDO
      ENDIF
      READ (Ki,99005,END = 100) idum
      RETURN
  100 Ieof = 1
99005 FORMAT (80A1)
      END


      SUBROUTINE SUMPRT(Ko)
C
C     print summary information from RIPL formatted library
C
      INCLUDE 'ripl2empire.h'
C
C
C Dummy arguments
C
      INTEGER Ko
C
C Local variables
C
      CHARACTER*20 area
      INTEGER i, iarea, iemax, iemin, n, nauth, nn, nrefer, nsum
      INTEGER INT, MIN0
      CHARACTER*8 ldum, proj
      CHARACTER*40 model
      DATA ldum/'++++++++'/
      IF (IZProj.EQ.0 .AND. IAProj.EQ.1) proj = ' Neutron'
      IF (IZProj.EQ.1 .AND. IAProj.EQ.1) proj = '  Proton'
      IF (IZProj.EQ.1 .AND. IAProj.EQ.2) proj = 'Deuteron'
      IF (IZProj.EQ.1 .AND. IAProj.EQ.3) proj = '  Triton'
      IF (IZProj.EQ.2 .AND. IAProj.EQ.3) proj = '    He-3'
      IF (IZProj.EQ.2 .AND. IAProj.EQ.4) proj = '   Alpha'
      IF (IMOdel.EQ.0) model = 'spherical nucleus model'
      IF (IMOdel.EQ.1) model = 'coupled-channels rigid rotor model'
      IF (IMOdel.EQ.2) model = 'coupled-channels vibrational model'
      IF (IMOdel.EQ.3) model = 'coupled-channels soft rotor model'
      iarea = MOD(IREf,1000)
      IF (iarea.LE.99) area = 'United States (LANL)'
      IF (iarea.GE.100 .AND. iarea.LE.199) area = 'United States'
      IF (iarea.GE.200 .AND. iarea.LE.299) area = 'Japan'
      IF (iarea.GE.300 .AND. iarea.LE.399) area = 'Russia'
      IF (iarea.GE.400 .AND. iarea.LE.499) area = 'Western Europe'
      IF (iarea.GE.500 .AND. iarea.LE.599) area = 'China'
      IF (iarea.GE.600 .AND. iarea.LE.649) area = 'Eastern Europe'
      IF (iarea.GE.650 .AND. iarea.LE.699) area = 'India, Pakistan'
      IF (iarea.GE.700 .AND. iarea.LE.799) area = 'Others'
      IF (iarea.GE.800 .AND. iarea.LE.999) area = 'Reserved'
      WRITE (Ko,99010) IREf, proj, model
99010 FORMAT (' IREF=',i5,2x,a8,' incident, ',a40)
      IF (IDR.GT.0) WRITE (Ko,'('' Dispersive optical model'')')
      IF (IREl.EQ.0) WRITE (Ko,'('' Non-relativistic kinematics'')')
      IF (IREl.EQ.1) WRITE (Ko,'('' Relativistic kinematics'')')
      IF (IREl.EQ.2) WRITE (Ko,
     &'('' Relativistic kinematics + Dirac equivalent Schroedinger equat
     &ion.'')')
      iemin = INT(EMIn)
      iemax = INT(EMAx)
      WRITE (Ko,99015) IZMin, IZMax, IAMin, IAMax, iemin, iemax
99015 FORMAT (' Z-Range=',i3,'-',i2,'  A-Range=',i4,'-',i3,'  E-Range=',
     &        i4,'-',i3,' MeV')
      DO nn = 1, 80
         n = 80 - nn + 1
         IF (AUThor(n).NE.' ') GOTO 100
      ENDDO
  100 nauth = MIN0(80,n)
      WRITE (Ko,99020) (AUThor(n),n = 1,nauth)
99020 FORMAT (' Author(s)= ',60A1,/12x,20A1)
      DO nn = 1, 80
         n = 80 - nn + 1
         IF (REFer(n).NE.' ') GOTO 200
      ENDDO
  200 nrefer = MIN0(80,n)
      WRITE (Ko,99025) (REFer(n),n = 1,nrefer)
99025 FORMAT (' Reference= ',60A1,/12x,20A1)
      DO nn = 1, 320
         n = 320 - nn + 1
         IF (SUMmary(n).NE.' ') GOTO 300
      ENDDO
  300 nsum = MIN0(320,n)
      WRITE (Ko,99030) (SUMmary(n),n = 1,nsum)
99030 FORMAT ('   Summary= ',60A1,/12x,60A1,/12x,60A1,/12x,60A1,/12x,
     &        60A1,/12x,20A1)
      WRITE (Ko,99035) (ldum,i = 1,9)
99035 FORMAT (10A8)
      END


      SUBROUTINE FINDPOT_OMPAR_RIPL(Ki,Ieof,Ipoten,Nnuc,Nejc)
C
C-----Find IPOTEN entry in the RIPL optical model database
C
C Dummy arguments
C
      INTEGER Ieof, Ipoten, Ki, Nejc, Nnuc
C
C Local variables
C
      CHARACTER*3 ctmp
      INTEGER iref, nejcr, nnucr
      Ieof = 0
      REWIND (Ki)
      READ (Ki,'(3i5)') iref, nnucr, nejcr
      IF (Ipoten.EQ.iref .AND. Nnuc.EQ.nnucr .AND. nejcr.EQ.Nejc) THEN
         BACKSPACE (Ki)
         RETURN
      ENDIF
  100 READ (Ki,99005,END = 200) ctmp
99005 FORMAT (A3)
      IF (ctmp.NE.'+++') GOTO 100
      READ (Ki,'(3i5)',END = 200) iref, nnucr, nejcr
      IF (iref.NE.Ipoten .OR. Nnuc.NE.nnucr .OR. nejcr.NE.Nejc) GOTO 100
      BACKSPACE (Ki)
      RETURN
  200 Ieof = 1
      END


      SUBROUTINE TLEVAL(Nejc,Nnuc,Nonzero)
CCC
CCC   ********************************************************************
CCC   *                                                         CLASS:PPU*
CCC   *                         T L E V A L                              *
CCC   *                                                                  *
CCC   * CALCULATES OPTICAL MODEL TRANSMISSION COEFFICIENTS AND FILLS THEM*
CCC   * INTO TL(.,.,.,.) MATRIX.                                         *
CCC   *                                                                  *
CCC   * INPUT:NEJC - EJECTILE INDEX                                      *
CCC   *       NNUC - NUCEUS INDEX (TL ARE CALCULATED FOR THE SCATTERING  *
CCC   *              OF NEJC ON NNUC)                                    *
CCC   *                                                                  *
CCC   * OUTPUT:Nonzero true if non-zero Tl's are returned                *
CCC   *                                                                  *
CCC   ********************************************************************
CCC
      INCLUDE 'dimension.h'
      INCLUDE 'global.h'
C
C COMMON variables
C
      INTEGER MAXl(NDETL)
      DOUBLE PRECISION TTLl(NDETL,0:NDLW)
      COMMON /TRANSM/ TTLl, MAXl
C
C Dummy arguments
C
      INTEGER Nejc, Nnuc
      LOGICAL Nonzero
C
C Local variables
C
      INTEGER i, l
      INTEGER INT, MIN0
      Nonzero = .FALSE.
      IF (SEJc(Nejc).GT.1.0D0) THEN
         WRITE (8,
     &'('' SPIN OF EJECTILE A='',I3,'' AND Z='',I2,'' IS '',    F4.1,/,'
     &' SUCH A LARGE VALUE IS NOT ALLOWED IN TRANSMISSION CEOFFICIENT CA
     &LCULATIONS'',/,'' EXECUTION S T O P P E D '')') INT(AEJc(Nejc)),
     &INT(ZEJc(Nejc)), SEJc(Nejc)
         STOP '13'
      ENDIF
C
C-----TL trans. coeff. at zero energy must be zero
C
      DO i = 1, NDETL
         LMAxtl(i,Nejc,Nnuc) = 0
         DO l = 1, NDLW
            TL(i,l,Nejc,Nnuc) = 0.D0
         ENDDO
      ENDDO
C-----TARGET NUCLEUS (ELASTIC CHANNEL), incident neutron or proton
C-----Transmission coefficient matrix for elastic channel
C-----ECIS code is used
C-----KTRlom(Nejc,Nnuc)=KTRompCC
      IWArn = 0
C-----Preparing INPUT and RUNNING ECIS
C-----(or reading already calculated file)
      CALL TRANSINP(Nejc,Nnuc,NDETL)
C-----IWARN=0 - 'NO Warnings'
C-----IWARN=1 - 'A out of the recommended range '
C-----IWARN=2 - 'Z out of the recommended range '
C-----IWARN=3 - 'Energy requested lower than recommended for this potential'
C-----IWARN=4 - 'Energy requested higher than recommended for this potential'
      IF ((IWArn.EQ.1 .OR. IWArn.EQ.2) .AND. IOUt.GE.5) WRITE (8,*)
     &  ' WARNING: OMP not recommended for Z,A=', Z(Nnuc),'-',A(Nnuc)
      IF (IWArn.EQ.3 .AND. IOUt.GE.5) WRITE (8,*)
     &     ' WARNING: OMP not recommended for low energies in Tl calc'
      IF (IWArn.EQ.4 .AND. IOUt.GE.5) WRITE (8,*)
     &     ' WARNING: OMP not recommended for high energies in Tl calc'
      IWArn = 0
C--------transfer of the calculated transmission coeff. onto TL matrix
      DO i = 2, NDETL
         LMAxtl(i,Nejc,Nnuc) = MIN0(MAXl(i) + 1,NDLW)
         DO l = 1, LMAxtl(i,Nejc,Nnuc)
            IF (TTLl(i,l - 1).LT.1.D-10) GOTO 100
            TL(i,l,Nejc,Nnuc) = TTLl(i,l - 1)
            Nonzero = .TRUE.
         ENDDO
  100 ENDDO
      END


      SUBROUTINE TLLOC(Nnuc,Nejc,Eout,Il,Frde)
Ccc
Ccc   ********************************************************************
Ccc   *                                                          class:au*
Ccc   *                         T L L O C                                *
Ccc   *                                                                  *
Ccc   * Locates the first energy in the ETL matrix that lies below       *
Ccc   * EOUT energy  and returns relative position of EOUT between       *
Ccc   * IL and IL+1 elements of ETL. To be used for the transmission     *
Ccc   * coefficients interpolation.                                      *
Ccc   *                                                                  *
Ccc   * input: NNUC - nucleus index                                      *
Ccc   *        NEJC - ejectile index                                     *
Ccc   *        EOUT - requested energy                                   *
Ccc   *                                                                  *
Ccc   * output:IL   - index of the first element of ETL below EOUT       *
Ccc   *        FRDE - relative position of eout between two ETL elements *
Ccc   *               FRDE=(EOUT-ETL(IL))/(ETL(IL+1)-ETL(IL))            *
Ccc   *                                                                  *
Ccc   * calls:none                                                       *
Ccc   *                                                                  *
Ccc   *                                                                  *
Ccc   ********************************************************************
Ccc
      INCLUDE 'dimension.h'
      INCLUDE 'global.h'
C
C Dummy arguments
C
      DOUBLE PRECISION Eout, Frde
      INTEGER Il, Nejc, Nnuc
C
C Local variables
C
      DOUBLE PRECISION detl
      INTEGER i
      INTEGER INT
      IF (Eout.GT.ETL(5,Nejc,Nnuc)) THEN
         Il = INT((Eout - ETL(5,Nejc,Nnuc))/DE) + 5
         Frde = (Eout - ETL(Il,Nejc,Nnuc))/DE
      ELSE
         DO i = 4, 1, -1
            IF (Eout.GT.ETL(i,Nejc,Nnuc)) THEN
               Il = i
               detl = ETL(i + 1,Nejc,Nnuc) - ETL(i,Nejc,Nnuc)
               Frde = (Eout - ETL(i,Nejc,Nnuc))/detl
               RETURN
            ENDIF
         ENDDO
      ENDIF
      END
C
C
C
C PRE_ECIS package - Creation of input files for ECIS95 and processing of the ECIS output
C                        (Version: January , 2001)
C
C                         Author: Roberto Capote
C
C                  Based on PREGNASH code by Arjan Koning
C                       (Version: May 17, 1996)
C
C       Please send any comments, corrections or improvements to:
C
C                             Roberto Capote
C
C          INPUT  : NEJC - Ejectile key
C                   NNUC - Residual nucleus key
C                   NEN  - Max.Emiss. energy Index
C
C
C
      SUBROUTINE TRANSINP(Nejc,Nnuc,Nen)
      INCLUDE "dimension.h"
      INCLUDE "global.h"
C
C COMMON variables
C
      INTEGER MAXl(NDETL)
      DOUBLE PRECISION TTLl(NDETL,0:NDLW)
      COMMON /TRANSM/ TTLl, MAXl
C
C Dummy arguments
C
      INTEGER Nejc, Nen, Nnuc
C
C Local variables
C
      CHARACTER*3 ctldir
      CHARACTER*23 ctmp23
      DOUBLE PRECISION culbar, ener
      LOGICAL fexist, ltmp
      INTEGER i, ilv, ien, ien_beg, l, lmax
      INTEGER INT
      REAL SNGL
C     --------------------------------------------------------------------
C     | Calculation of transmission coefficients using ECIS              |
C     |                for EMPIRE energy grid                            |
C     --------------------------------------------------------------------
      DATA ctldir/'TL/'/

C-----data initialization
      CALL INIT(Nejc,Nnuc)
C-----Cleaning transmission coefficient matrix
      DO i = 1, NDETL
         MAXl(i) = 0
         DO l = 0, NDLW
            TTLl(i,l) = 0.D0
         ENDDO
      ENDDO
C
C-----This part prompts for the name of a data file. The INQUIRE
C-----statement then determines whether or not the file exists.
C-----If it does not, the program calculates new transmission coeff.
      WRITE (ctmp23,'(i3.3,i3.3,1h_,i3.3,i3.3,1h_,i9.9)')
     &       INT(ZEJc(Nejc)), INT(AEJc(Nejc)), INT(Z(Nnuc)),
     &       INT(A(Nnuc)), INT(EINl*1000000)
      INQUIRE (FILE = (ctldir//ctmp23//'.BIN'),EXIST = fexist)
      IF (.NOT.fexist) GOTO 400
C-----Here the previously calculated files should be read
      OPEN (45,FILE = (ctldir//ctmp23//'.BIN'),FORM = 'UNFORMATTED')
      IF (IOUt.EQ.5) OPEN (46,FILE = ctldir//ctmp23//'.LST')
  100 READ (45,END = 200) lmax, ien, ener, IRElat(Nejc,Nnuc)
      IF (IOUt.EQ.5) WRITE (46,'(A5,2I6,E12.6)') 'LMAX:', lmax, ien,
     &                      ener
C
C-----If energy read from the file does not coincide
C-----this nucleus should be recalculated (goto 300)
C
      IF (ABS(ener - ETL(ien,Nejc,Nnuc)).GT.0.0001) THEN
         CLOSE (45,STATUS = 'DELETE')
         IF (IOUt.EQ.5) CLOSE (46,STATUS = 'DELETE')
         IF (IOUt.EQ.5) THEN
         WRITE (8,*) 'WARNING: ENERGY MISMATCH: ETL(ien=', ien, '...)=',
     &               ETL(ien,Nejc,Nnuc), ' REQUESTED ENERGY=',
     &               SNGL(ener)
         WRITE (8,*)
     &              'WARNING: FILE WITH TRANSM. COEFF. HAS BEEN DELETED'
          ENDIF
         GOTO 400
      ENDIF
      ETL(ien,Nejc,Nnuc) = ener
      MAXl(ien) = lmax
      DO l = 0, lmax
         READ (45,END = 300) TTLl(ien,l)
         IF (IOUt.EQ.5) WRITE (46,*) l, TTLl(ien,l)
C
C        Newly added 
C
C        TTLl(ien,l) = TTLl(ien,l)*OUTred(Nejc,Nnuc)   
      ENDDO
C     SIGabs(ien,Nejc,Nnuc) = SIGabs(ien,Nejc,Nnuc)*OUTred(Nejc,Nnuc)
      READ (45,END = 300) SIGabs(ien,Nejc,Nnuc)
      GOTO 100
  200 CLOSE (45)
      IF (IOUt.EQ.5) CLOSE (46)
      IF (IOUt.EQ.5) WRITE (8,*)
     &                      'Transmission coefficients read from file: '
     &                      , (ctldir//ctmp23//'.BIN')
      RETURN

  300 CLOSE (45,STATUS = 'DELETE')
      IF (IOUt.EQ.5) CLOSE (46,STATUS = 'DELETE')
      DO i = 1, NDETL
         MAXl(i) = 0
         DO l = 0, NDLW
            TTLl(i,l) = 0.D0
         ENDDO
      ENDDO
      IF (IOUt.GT.0) WRITE (8,*) 'WARNING: ERROR WHEN READING TLs in ',
     &                           ctmp23
      IF (IOUt.EQ.5) THEN
         WRITE (8,*)
     &              'WARNING: FILE WITH TRANSM. COEFF. HAS BEEN DELETED'
         WRITE (8,*)
     &           'WARNING: TRANSM. COEFF. WILL BE CALCULATED AND STORED'
      ENDIF
C-----Coulomb barrier (somewhat decreased) setting lower energy limit
C-----for transsmission coefficient calculations
  400 culbar = 0.8*ZEJc(Nejc)*Z(Nnuc)/(1 + A(Nnuc)**0.6666)
      ien_beg = -1
      DO i = 2, Nen
         IF (ETL(i,Nejc,Nnuc).GT.culbar) THEN
            ien_beg = i
            GOTO 500
         ENDIF
      ENDDO
  500 IF (ien_beg.NE. - 1) THEN
C
C--------Running ECIS
C
         IF (IOUt.EQ.5) THEN
            WRITE (8,*)
            IF (DIRect.EQ.2 .AND. AEJc(Nejc).LE.1) THEN
               WRITE (8,*) ' CC transmission coefficients used for ',
     &                     'outgoing channels'
            ELSE
               WRITE (8,*) ' Spherical OM transmission coefficients',
     &                     ' used for outgoing channels'
            ENDIF
            WRITE (8,*)
         ENDIF
C--------OPEN Unit=46 for Tl output
         OPEN (UNIT = 46,STATUS = 'unknown',
     &         FILE = (ctldir//ctmp23//'.BIN'),FORM = 'UNFORMATTED')
C
C--------do loop over energy
C
         ltmp = A(Nnuc).EQ.A(0) .AND. Z(Nnuc).EQ.Z(0) .AND.
     &          DIRect.EQ.2 .AND. AEJc(Nejc).LE.1

         DO i = ien_beg, Nen
            ener = ETL(i,Nejc,Nnuc)
            IF (ener.LE.0.1D-6) GOTO 550

            IF (.NOT.(ltmp)) THEN
C
C--------------Spherical optical model is assumed, only one level (gs)
C
               CALL ECIS_CCVIB(Nejc,Nnuc,ener,.TRUE.,0)
               CALL ECIS2EMPIRE_TR(Nejc,Nnuc,i,.TRUE.)
C--------------Transmission coefficient matrix for incident channel
C--------------is calculated (DIRECT = 2 (CCM)) using ECIS code.
C--------------Only coupled levels are considered
            ELSEIF (DEFormed) THEN
C--------------In this case we need only CC calculation so INLkey=0
               CALL ECIS_CCVIBROT(Nejc,Nnuc,ener,0)
               CALL ECIS2EMPIRE_TR(Nejc,Nnuc,i,.FALSE.)
            ELSE
C--------------EXACT (no DWBA) calculation
               CALL ECIS_CCVIB(Nejc,Nnuc,ener,.FALSE., - 1)
               CALL ECIS2EMPIRE_TR(Nejc,Nnuc,i,.TRUE.)

            ENDIF

  550    ENDDO

         CLOSE (46)
         IF (IOUt.EQ.5) WRITE (8,*) ' Transm. coeff. written to file:',
     &                              (ctldir//ctmp23//'.BIN')
         IF (IOUt.EQ.5) WRITE (8,*)
     &            ' ==================================================='
      ELSEIF (IOUt.EQ.5) THEN
         WRITE (8,'(1x,A12,I3,A3,I3,A3,F4.1)') 'EJECTILE: A=',
     &          INT(AEJc(Nejc)), ' Z=', INT(ZEJc(Nejc)), ' S=',
     &          SEJc(Nejc)
         ilv = 1
         If(Nnuc.eq.0) ilv = Levtarg
         WRITE (8,'(1x,A12,I3,A3,I3,A3,F4.1,A3,I2)') 'RESIDUAL: A=',
     &          INT(A(Nnuc)), ' Z=', INT(Z(Nnuc)), ' S=',
     &          SNGL(XJLv(ilv,Nnuc)), ' P=', INT(LVP(ilv,Nnuc))
         WRITE (8,*) 'WARNING: For this residual nucleus'
         WRITE (8,*) 'WARNING: available energy is always '
         WRITE (8,*) 'WARNING: below coulomb barrier'
         WRITE (8,*) 'WARNING: Calculations are not needed!'
         WRITE (8,*)
      ENDIF
      END


      SUBROUTINE ECIS2EMPIRE_TL_TRG(Nejc,Nnuc,Maxlw,Stl,Sel,Lvibrat)
C
C     Process ECIS output to obtain Stl matrix for the incident channel
C     Reads from unit 45 and writes to unit 6
C
C     INPUT:  Nejc,Nnuc ejectile and residual nucleus index
C             LVIbrat = .TRUE. for vibrational model(or spherical OMP)
C             The implemented model could be applied to even-even nucleus
C             therefore the ECIS input is always constructed assuming spin 0
C             So we can not use the real spin of the given nucleus XJLv(1, Nnuc)
C
C     OUTPUT: Maxlw and Stl(1-Maxlw) are the maximum angular momentum and Tls
C             SINl is the total inelastic cross section
C             Selast is the shape elastic cross section
C             Sel(L) contains the shape elastic cross section for a given orbital momentum L
C             Stl(L) contains the transmission coefficient Tl(L) for a given orbital momentum L
C
C
      INCLUDE 'dimension.h'
      INCLUDE 'global.h'
C
C COMMON variables
C
      DOUBLE PRECISION ABScs, ELAcs, SINl, TOTcs, SINlcc, SINlcont
      COMMON /ECISXS/ ELAcs, TOTcs, ABScs, SINl, SINlcc, SINlcont
C
C Dummy arguments
C
      LOGICAL Lvibrat
      INTEGER Maxlw, Nejc, Nnuc
      DOUBLE PRECISION Stl(NDLW),Sel(NDLW)
C
C Local variables
C
      DOUBLE PRECISION ak2, dtmp, ecms, elab, jc, jj, sabs, sreac,
     &                 xmas_nejc, xmas_nnuc, selast
      DOUBLE PRECISION DBLE
      INTEGER ilv, l, nc, nceq, ncoll, nlev, mintsp, nc1, nc2
      LOGICAL relcal
      CHARACTER*1 parc
      REAL SNGL
      Maxlw = 0
      ncoll = 0

      DO l = 1, NDLW
       Stl(l) = 0.d0
       Sel(l) = 0.d0
      ENDDO

      ilv = 1
      If(Nnuc.eq.0) ilv = Levtarg
C------------------------------------------
C-----| Input of transmission coefficients|
C------------------------------------------
C-----Opening ecis03 output file containing Smatrix
      OPEN(UNIT = 45, STATUS = 'old', FILE = 'ecis06.smat', ERR=90)
      READ (45,*,END = 90)   ! To skip first line <SMATRIX> ..
C  80 READ (45,'(1x,f4.1,1x,a1,1x,i4,1x,i4)',END=90)
C    &     jc, parc, nceq, nctot
   80 READ (45,'(1x,f9.1,4x,a1,2(1x,i4))',END = 90) 
     &     jc, parc, nceq, nctot ! ecis06
C     JC,ParC is the channel spin and parity
C     nceq is the total number of coupled equations
C     ncin is the number of independent coupled equations
      ncsol=nctot/nceq
C     Loop over the number of coupled equations
      do ncint=1,ncsol
        do nc=1,nceq
C       Reading the coupled level number nlev, the orbital momentum L,
C           angular momentum j and Transmission coefficient Tlj,c(JC)
C       (nlev=1 corresponds to the ground state)
C       read (45,
C    &  '(1x,3(I3,1x),I3,1x,F5.1,1x,2(D15.7,1x),1x,4x,F11.8)',END=90)
C    &  nc1,nc2,nlev,l,jj,sreal,simag,stmp
C       read (45, ! ecis06                
C    &  '(3I3,I4,1x,F6.1,1x,2(D15.7,1x),1x,4x,F12.8)',END=90)
C        (1X,3(I3,1X),I3,1X,F5.1,1X,2(1P,D15.7,0P,1X),'I',4X,F11.8)
C    &  nc1,nc2,nlev,l,jj,sreal,simag,stmp
        read (45, ! ecis06 (Dec 2008)               
C    &  '(3I3,I4,1x,F5.1,2x,2(D14.7,2x),1x,4x,F12.8)',END=90,ERR=90)
     &  '(1X,3(I3,1X),I3,1X,F5.1,1X,2(D15.7,1X),1x,4X,F11.8)'
     &  ,END=90,ERR=90) nc1,nc2,nlev,l,jj,sreal,simag,stmp

        IF (nlev.eq.1 .and. nc1.eq.nc2
     &  .and. stmp.GT.1.D-15 .AND. L.LT.NDLW)
C-----------Averaging over particle and target spin, summing over channel spin jc
C    &      stot = stot + (2*jj + 1)*(1.d0-sreal) ! /DBLE(2*L + 1)
     &      Sel(l+1) = Sel(l+1) + (2*jc + 1)*((1 - sreal)**2 + simag**2)
     &           /DBLE(2*L + 1)
     &           /DBLE(2*SEJc(Nejc) + 1)
     &           /DBLE(2*XJLv(ilv,Nnuc) + 1)
        enddo
      enddo
      GOTO 80
   90 CLOSE(45)

      OPEN (UNIT = 45,STATUS = 'old',FILE = 'INCIDENT.TLJ', ERR=200)
      READ (45,*,END = 200)   ! To skip first line <TLJs.> ..
C-----JC,ParC is the channel spin and parity
C-----nceq is the number of coupled equations
C 100 READ (45,'(1x,f4.1,1x,a1,1x,i4)',END = 200) jc, parc, nceq
  100 READ (45,'(1x,f9.1,4x,a1,1x,i4)',END = 200) jc, parc, nceq  ! ecis06
C-----Loop over the number of coupled equations
      DO nc = 1, nceq
C--------Reading the coupled level number nlev, the orbital momentum L,
C--------angular momentum j and Transmission coefficient Tlj,c(JC)
C--------(nlev=1 corresponds to the ground state)
C        READ (45,'(1x,I2,1x,I3,1x,F5.1,1x,e15.6)',END = 200) nlev, l,
C    &         jj, dtmp
         READ (45,*,END = 200,ERR = 200) nlev, l, jj, dtmp
         ncoll = MAX(nlev,ncoll)
C--------Selecting only ground state
         IF (nlev.EQ.1 .AND. dtmp.GT.1.D-15 .AND. l.LT.NDLW) THEN
C-----------Averaging over particle and target spin, summing over channel spin jc
            Stl(l + 1) = Stl(l + 1) + (2*jc + 1)*dtmp/DBLE(2*l + 1)
     &                   /DBLE(2*SEJc(Nejc) + 1)
     &                   /DBLE(2*XJLv(ilv,Nnuc) + 1)
            Maxlw = MAX(Maxlw,l)
            if(Maxlw.GE.NDLW) Maxlw = NDLW - 1
         ENDIF
      ENDDO
      GOTO 100
  200 CLOSE (45)
C-----For vibrational the Tls must be multiplied by
      IF (Lvibrat) THEN
         DO l = 0, Maxlw
            Stl(l + 1) = Stl(l + 1)*DBLE(2*XJLv(ilv,Nnuc) + 1)
            Sel(l + 1) = Sel(l + 1)*DBLE(2*XJLv(ilv,Nnuc) + 1)
         ENDDO
      ENDIF

      TOTcs = 0.D0
      ABScs = 0.D0
      ELAcs = 0.D0
      SINl = 0.D0
      SINlcc = 0.D0
      SINlcont = 0.D0
      sabs = 0.D0
      selast = 0.D0

      OPEN (UNIT = 45,FILE = 'INCIDENT.CS',STATUS = 'old',ERR = 300)
      READ (45,*,END = 300)  ! Skipping first line
      IF (ZEJc(Nejc).EQ.0) READ (45,*) TOTcs
      READ (45,*) ABScs
      IF (ZEJc(Nejc).EQ.0) READ (45,*) ELAcs
  300 CLOSE (45)

      IF (ABScs.LE.0.D0) RETURN
C
C-----Estimating absorption cross section from obtained TLs
C
      xmas_nnuc = (A(Nnuc)*AMUmev + XMAss(Nnuc))/AMUmev
      xmas_nejc = EJMass(Nejc)
C     xmas_nnuc = AMAss(Nnuc)
      elab = EINl
      relcal = .FALSE.
      IF (IRElat(Nejc,Nnuc).GT.0 .OR. RELkin) relcal = .TRUE.
      CALL KINEMA(elab,ecms,xmas_nejc,xmas_nnuc,ak2,1,relcal)

C-----Absorption and elastic cross sections in mb
      DO l = 0, Maxlw
         sabs   = sabs   + Stl(l + 1)*DBLE(2*l + 1)
         selast = selast + Sel(l + 1)*DBLE(2*l + 1)
      ENDDO
      sabs   = 10.d0*PI/ak2*sabs
      selast = 10.d0*PI/ak2*selast

      IF (sabs.LE.0.D0) RETURN

      CSFus = ABScs

      mintsp = mod(NINT(2*D_Xjlv(1)),2)
      OPEN (UNIT = 45,FILE = 'INCIDENT.ICS',STATUS = 'old',ERR = 400)
      READ (45,*,END = 400)  ! Skipping first line
      DO l = 1, NDCOLLEV     ! number of inelastic level
         READ (45,*,END = 400) dtmp
         IF ( (ICOller(l+1).LE.NLV(nnuc)) .AND.
C            For odd nuclides, collective states in continuum have
C            different spin than the ground state
     &        (mod(NINT(2*D_Xjlv(l+1)),2).eq.mintsp) )THEN
C          Discrete level scattering
           IF (ICOllev(l+1).LT.LEVcc) THEN
C             Coupled levels
              SINlcc = SINlcc + dtmp
           ELSE
C             Uncoupled discrete levels
              SINl = SINl + dtmp
           ENDIF
         ELSE
C          Scattering into continuum
           SINlcont = SINlcont + dtmp
C          Not included into inelastic as it is renormalized in main.f
C          SINl = SINl + dtmp
         ENDIF
      ENDDO
  400 CLOSE (45)
      IF (SINl+SINlcc+SINlcont.EQ.0.D0) RETURN
C--- SINlcc in next IF changed to SINl - BVC
      IF (SINl.GT.ABScs) THEN
         WRITE (8,*)
         WRITE (8,*) ' WARNING: POSSIBLE ECIS NON-CONVERGENCE !!'
         WRITE (8,
     &     '(5x,''**************************************************'')'
     &     )
         WRITE (8,
     &     '(5x,'' Direct cross section calculation do not converge '')'
     &     )
         WRITE (8,
     &'(6x,''Inelastic cross section ='',F8.2,'' mb''/
     &  6x,''Reaction  cross section ='',F8.2,'' mb''/)') SINl, ABScs
         WRITE (8,
     &     '(5x,'' Either change OMP or change calculation method   '')'
     &     )
         WRITE (8,
     &     '(5x,''        (DIRPOT)   or   (DIRECT) parameters       '')'
     &     )
         WRITE (8,
     &     '(5x,'' This problem usually happens using DWBA method   '')'
     &     )
         WRITE (8,
     &     '(5x,'' to treat strong coupled nuclei                   '')'
     &     )
         WRITE (8,
     &     '(5x,''            CALCULATION STOPPED                   '')'
     &     )
         WRITE (8,
     &     '(5x,''**************************************************'')'
     &     )
         STOP ' POSSIBLE ECIS NON-CONVERGENCE !!'
      ENDIF
C
C     Absorption cross section includes inelastic scattering cross section to coupled levels
C
      sreac = sabs + SINlcc
      IF (IOUt.EQ.5 .AND. sabs.GT.0.D0) THEN
         WRITE (8,*)
         WRITE (8,*) ' INCIDENT CHANNEL:'
         WRITE (8,'(A7,I6,A5,F10.3,A10,F10.3,A10)') '  LMAX:', Maxlw,
     &          ' E = ', EIN, ' MeV (CMS)', elab, ' MeV (LAB)'
         WRITE (8,*) ' XS calculated using Smat:   Selast =',
     &               SNGL(selast), ' mb '
         WRITE (8,*) ' Shape elastic XS =', SNGL(ELAcs),
     &               ' mb (read from ECIS)'
         WRITE (8,*) ' XS calculated using averaged Tls:   Sabs =',
     &               SNGL(sabs), ' mb '
         WRITE (8,*) ' Reaction XS =', SNGL(ABScs),
     &               ' mb (read from ECIS)'
         WRITE (8,*) ' ECIS/EMPIRE ratio of reaction cross section =',
     &               (ABScs - SINlcc)/sabs
         WRITE (8,*) ' Inelastic XS to coupled levels (SINlcc) =',
     &               SNGL(SINlcc), ' mb '
         WRITE (8,*)
     &      ' Inelastic XS to uncoupled discrete levels (DWBA) =',
     &               SNGL(SINl), ' mb '
         WRITE (8,*) ' Inelastic XS to the continuum (sinlcont) =',
     &               SNGL(SINlcont), ' mb '
         IF (SINlcc.GT.0.D0) THEN
            WRITE (8,*) ' Sinl =', SNGL(ABScs), ' mb (read from ECIS)'
            WRITE (8,*) ' Sreac=', SNGL(sreac), ' mb (Sabs + SINlcc)'
         ENDIF
         WRITE (8,*) ' Sreac - SINl - sinlcont(renormalized by DWBA) =',
     &         SNGL(ABScs -SINlcc -SINl -SINlcont), 
     &             ' mb (Sabs - SINl - SINlcont)'
         WRITE (8,*) ' Total XS =', SNGL(TOTcs), ' mb (read from ECIS)'
         WRITE (8,*) ' Shape Elastic XS =', SNGL(ELAcs),
     &               ' mb (read from ECIS)'
         WRITE (8,*)

      ENDIF
C
C-----Renormalizing TLs to correct very small difference
C     between calculated and read ECIS XS
C     Discrete level inelastic scattering (not coupled levels) also included
C
      DO l = 0, Maxlw
        Stl(l + 1) = Stl(l + 1)*(ABScs - SINlcc - SINl -SINlcont)/sabs
      ENDDO
      CSFus = ABScs - SINlcc - SINl - SINlcont
C     DO l = 0, Maxlw
C        Stl(l + 1) = Stl(l + 1)*(ABScs - SINlcc - SINl)/sabs
C     ENDDO
C     CSFus = ABScs - SINlcc - SINl 

      RETURN
      END


      SUBROUTINE ECIS2EMPIRE_TR(Nejc,Nnuc,J,Lvibrat)
C
C     Process ECIS output to obtain TTLl,MaxL matrix for EMPIRE energy grid
C     Reads from units 45, 46 and writes to unit 6
C
C     INPUT:  Nejc  ejectile nucleus index
C             Nnuc  residual nucleus index
C             J     energy index
C     OUTPUT: TTLl(NDETL,0:NDLW),MaxL(NDETL) for all energies from 1 to NDETL
C
C==========================================================================
C     ncoll - number of collective coupled levels
C
C     Process file ecis06.tlj
C     Average of Tlj,c(J) coefficients over J for c = gs
C     to obtain usual T(L)
C     Sigma_abs = pi/k**2 * suma [(2*L+1) T(L)]
C     Sigma_reacc = Sigma_abs(c=gs) + Sigma_inl(c = other coupled channels)
C
      INCLUDE 'dimension.h'
      INCLUDE 'global.h'
C
C COMMON variables
C
      INTEGER MAXl(NDETL)
      DOUBLE PRECISION TTLl(NDETL,0:NDLW)
      COMMON /TRANSM/ TTLl, MAXl
C
C Dummy arguments
C
      INTEGER J, Nejc, Nnuc
      LOGICAL Lvibrat
C
C Local variables
C
      DOUBLE PRECISION ak2, dtmp, ecms, elab, jc, jj, sabs,
     &                 selecis, sinlss, sreac, sreacecis, stotecis,
     &                 xmas_nejc, xmas_nnuc
      DOUBLE PRECISION DBLE
      LOGICAL relcal
      INTEGER l, lmax, nc, nceq, ncoll, nlev
      CHARACTER*1 parc
      REAL SNGL
      lmax = 0
      ncoll = 0
      ecms = ETL(J,Nejc,Nnuc)
C-----
C----- Input of transmission coefficients
C-----
      OPEN (UNIT = 45,STATUS = 'old',FILE = 'ecis06.tlj')
      READ (45,*,END = 200)  ! Skipping one line
C-----JC,ParC is the channel spin and parity
C-----nceq is the number of coupled equations
C 100 READ (45,'(1x,f4.1,1x,a1,1x,i4)',END = 200) jc, parc, nceq
  100 READ (45,'(1x,f9.1,4x,a1,1x,i4)',END = 200) jc, parc, nceq  ! ecis06
C-----Loop over the number of coupled equations
      DO nc = 1, nceq
C--------Reading the coupled level number nlev, the orbital momentum L,
C--------angular momentum j and Transmission coefficient Tlj,c(JC)
C--------(nlev=1 corresponds to the ground state)
C        READ (45,'(1x,I2,1x,I3,1x,F5.1,1x,e15.6)',END = 200) nlev, l,
C    &         jj, dtmp
         READ (45,*,END = 200,ERR = 200) nlev, l, jj, dtmp
         ncoll = MAX(nlev,ncoll)
C--------Selecting only ground state
         IF (nlev.EQ.1 .AND. dtmp.GT.1.D-15 .AND. l.LE.NDLW) THEN
C-----------Averaging over particle and target spin, summing over channel spin JC
            TTLl(J,l) = TTLl(J,l) + (2*jc + 1)*dtmp/DBLE(2*l + 1)
     &                  /DBLE(2*SEJc(Nejc) + 1)
     &                  /DBLE(2*XJLv(1,Nnuc) + 1)
            lmax = MAX(lmax,l)
         ENDIF
      ENDDO
      GOTO 100
  200 CLOSE (45)
C-----For vibrational the Tls must be multiplied by
      IF (Lvibrat) THEN
         DO l = 0, lmax
            TTLl(J,l) = TTLl(J,l)*DBLE(2*XJLv(1,Nnuc) + 1)
         ENDDO
      ENDIF
      stotecis = 0.D0
      selecis = 0.D0
      sreacecis = 0.D0
      OPEN (UNIT = 45,FILE = 'ecis06.cs',STATUS = 'old',ERR = 300)
      READ (45,*,END = 300)  ! Skipping one line
      IF (ZEJc(Nejc).EQ.0) READ (45,*,END = 300) stotecis
      READ (45,*,END = 300) sreacecis
      IF (ZEJc(Nejc).EQ.0) READ (45,*,END = 300) selecis
  300 CLOSE (45)
      sinlss = 0.D0
      SIGabs(J,Nejc,Nnuc) = 0.D0
      IF (sreacecis.LE.0.D0) RETURN
      IF (IOUt.EQ.5) THEN
         xmas_nnuc = (A(Nnuc)*AMUmev + XMAss(Nnuc))/AMUmev
         xmas_nejc = EJMass(Nejc)
         relcal = .FALSE.
         IF (IRElat(Nejc,Nnuc).GT.0 .OR. RELkin) relcal = .TRUE.
         CALL KINEMA(elab,ecms,xmas_nejc,xmas_nnuc,ak2,2,relcal)
C--------Reaction cross section in mb
         sabs = 0.D0
         if(elab.lt.0.3d0) write(8,*)
         DO l = 0, lmax
            stmp = TTLl(J,l)*DBLE(2*l + 1)
            if(elab.lt.0.3d0) write(8,303) l,stmp*10.*PI/ak2
  303       format(3x,' L =',I3,' Sabs(L) =',d12.6)
            sabs = sabs + stmp
         ENDDO
         if(elab.lt.0.3d0) write(8,*)
         sabs = 10.*PI/ak2*sabs
         OPEN (UNIT = 45,FILE = 'ecis06.ics',STATUS = 'old',ERR = 350)
         READ (45,*,END = 350) ! Skipping one line
         sinlss = 0.D0
         DO l = 1, ncoll - 1
            READ (45,*,END = 350) dtmp
            sinlss = sinlss + dtmp
         ENDDO
  350    CLOSE (45)
         sreac = sabs + sinlss
         IF (sabs.GT.0.D0) THEN
            WRITE (8,*)
            WRITE (8,'(A7,I3,A3,E12.6,A10,E12.6,A26,1x,I4)') '  LMAX:',
     &             lmax, ' E=', ecms, ' MeV (CMS)', elab,
     &             ' MeV (LAB); Energy index =', J
            WRITE (8,*) ' XS calculated using averaged Tls:   Sabs =',
     &                  SNGL(sabs), ' mb '
            WRITE (8,*) ' Reaction XS =', SNGL(sreacecis),
     &                  ' mb (read from ECIS)'
            WRITE (8,*) ' ECIS/EMPIRE ratio of reaction XS =',
     &                    SNGL(sreacecis/sreac)
            IF (sinlss.GT.0.D0) THEN
             WRITE (8,*) ' Sinl =', SNGL(sinlss), ' mb (read from ECIS)'
             WRITE (8,*) ' Sreac=', SNGL(sreac), ' mb (Sabs + Sinl)'
            ENDIF
         ENDIF
      ENDIF
C-----Storing transmission coefficients for EMPIRE energy grid
      WRITE (46) lmax, J, ecms, IRElat(Nejc,Nnuc)
      DO l = 0, lmax
         WRITE (46) TTLl(J,l)
C        TTLl(J,l) = TTLl(J,l)*OUTred(Nejc,Nnuc)   
      ENDDO
      WRITE (46) sreacecis
      MAXl(J) = lmax
      SIGabs(J,Nejc,Nnuc) = sreacecis
C     SIGabs(J,Nejc,Nnuc) = sreacecis*OUTred(Nejc,Nnuc)
      END
C
C
      BLOCKDATA FORTL
      INCLUDE "pre_ecis.h"
C
C Local variables
C
      INTEGER i
      DATA (PARname(i),i = 1,9)/'neutron ', 'proton  ', 'deuteron',
     &      'triton  ', 'he-3    ', 'alpha   ', 'ION-1   ', 'ION-2   ',
     &      'ION-3   '/
      DATA (NUC(i),i = 1,103)/'H_', 'HE', 'LI', 'BE', 'B_', 'C_', 'N_',
     &      'O_', 'F_', 'NE', 'NA', 'MG', 'AL', 'SI', 'P_', 'S_', 'CL',
     &      'AR', 'K_', 'CA', 'SC', 'TI', 'V ', 'CR', 'MN', 'FE', 'CO',
     &      'NI', 'CU', 'ZN', 'GA', 'GE', 'AS', 'SE', 'BR', 'KR', 'RB',
     &      'SR', 'Y_', 'ZR', 'NB', 'MO', 'TC', 'RU', 'RH', 'PD', 'AG',
     &      'CD', 'IN', 'SN', 'SB', 'TE', 'I_', 'XE', 'CS', 'BA', 'LA',
     &      'CE', 'PR', 'ND', 'PM', 'SM', 'EU', 'GD', 'TB', 'DY', 'HO',
     &      'ER', 'TM', 'YB', 'LU', 'HF', 'TA', 'W_', 'RE', 'OS', 'IR',
     &      'PT', 'AU', 'HG', 'TL', 'PB', 'BI', 'PO', 'AT', 'RN', 'FR',
     &      'RA', 'AC', 'TH', 'PA', 'U_', 'NP', 'PU', 'AM', 'CM', 'BK',
     &      'CF', 'ES', 'FM', 'MD', 'NO', 'LR'/
      END


      SUBROUTINE INIT(Nejc,Nnuc)
      INCLUDE "dimension.h"
      INCLUDE "global.h"
      INCLUDE "pre_ecis.h"
C
C Dummy arguments
C
      INTEGER Nejc, Nnuc
C
C Local variables
C
      CHARACTER*2 ceject
      CHARACTER*5 ctarget
      INTEGER INT, NINT
C
C                         +10       +20       +30       +40
C               123456789 123456789 123456789 123456789 123456789
      BECis1 = 'FFFFFFFFFFFFFFFFFFFFFFFFFFFTFFFFFFFFFFFFFFFFFFFFFF'

C               +50     +60        +70      +80        +90
C               123456789 123456789 123456789 123456789 123456789
c     BECis2 = 'FFFFFFFFTFFFFTFFTTTFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF'
      BECis2 = 'FFFFFFFFFFFFFTFFTTTFFTTFTFFFFFFFFFFFFFFFFFFFFFFFFF'
C
C-----*** OPTICAL POTENTIALS ***
C-----Relativistic kinematics (y/n)
      IF (IRElat(Nejc,Nnuc).GT.0 .or. RELkin) THEN
         FLGrel = 'y'
      ELSE
         FLGrel = 'n'
      ENDIF
C-----Making unique name of the output file: outfile
      ctarget(1:2) = NUC(NINT(Z(Nnuc)))
      IF (A(Nnuc).LT.10) THEN
         WRITE (ctarget(3:5),'(3I1)') 0, 0, INT(A(Nnuc))
      ELSEIF (A(Nnuc).LT.100) THEN
         WRITE (ctarget(3:5),'(I1,I2)') 0, INT(A(Nnuc))
      ELSE
C--------A>99
         WRITE (ctarget(3:5),'(I3)') INT(A(Nnuc))
      ENDIF
      WRITE (ceject(1:2),'(I1,I1)') INT(AEJc(Nejc)), INT(ZEJc(Nejc))
      OUTfile = ctarget//'_'//ceject
CPR---write(8,'(1x,A8)') ' UNIQUE NAME OF THE OUTPUT FILE:',outfile
      END


      SUBROUTINE ECIS_CCVIB(Nejc,Nnuc,El,Ldwba,Inlkey)
C
C     -------------------------------------------------------------
C     |    Create input files for ECIS06 for COUPLED CHANNELS     |
C     |    harmonic vibrational model                             |
C     |               v1.0     S.Masetti 5/99                     |
C     |               v2.0     R.Capote  1/2001                   |
C     |               v3.0     R.Capote  5/2001 (DWBA added)      |
C     |               v4.0     R.Capote  1/2005 (SOMP added)      |
C     |               v5.0     R.Capote  8/2008 ECIS06 version    |
C     -------------------------------------------------------------
C
C     INLkey = 0  DWBA calculation for the ground state = Spher.OMP
C     INLkey > 0  Calculation for coupled and uncoupled states = DWBA or CC
C     INLkey < 0  Calculation for coupled states only = CC
C
C     ****
C     IP = 1 NEUTRON
C     2 PROTON
C     3 DEUTERON
C     4 TRITON
C     5 HELIUM-3
C     6 ALPHA
C     >6 HEAVY ION
C
      INCLUDE "dimension.h"
      INCLUDE "global.h"
      INCLUDE "pre_ecis.h"
C-----For dispersive optical model potentials
C-----It is ripl2empireS.h because it must be compatible
C-----with global.h declarations so some variables must be renamed
      INCLUDE 'ripl2empireS.h'
C
C COMMON variables
C
      REAL*8 AAV, AS, AAvso, BS, BV, Bvso, CS, ETMP, EF, EP, EA
      INTEGER IQ, NNS, NNV, NNL, IDRs
      COMMON /DENERGY/ ETMP, EF, EP, EA
      common /pdatas/As,Bs,Cs,nns,iq
      common /pdatav/AAv,Bv,nnv
      common /pdatao/AAvso,Bvso,nnl

      REAL*8 VHFnum,DWVcor,VSOnum,DSOcor,ALhf,ALso,
     +               DWScor,ALpha,RSTep
      common /disper/VHFnum,DWVcor,VSOnum,DSOcor,ALhf,ALso,
     +               DWScor,ALpha,RSTep,IDRs
C
C     Dummy arguments
C
      DOUBLE PRECISION El
      INTEGER Inlkey, Nejc, Nnuc
      LOGICAL Ldwba,lodd
C
C Local variables
C
      DOUBLE PRECISION ak2, angstep, ecms, eee, elab, elabe, rmatch,
     &                 xmas_nejc, xmas_nnuc, xratio, zerosp, convg

      DOUBLE PRECISION vvref, wvref, wsref, vsoref, wsoref
      DOUBLE PRECISION fv, fs, fvv, fvs, tv, ts, fso, tso

      CHARACTER*1 ch
      DOUBLE PRECISION DABS
      INTEGER ncollx 

      INTEGER ikey, ip, iterm, j, ldwmax, nppaa,
     &        nd_cons, nd_nlvop, njmax, npp, nwrite

      CHARACTER*132 ctmp
      INTEGER*4 PIPE,iwin
      INTEGER INT, NINT

      IF (AEJc(Nejc).EQ.1.D0 .AND. ZEJc(Nejc).EQ.0.D0) ip = 1
      IF (AEJc(Nejc).EQ.1.D0 .AND. ZEJc(Nejc).EQ.1.D0) ip = 2
      IF (AEJc(Nejc).EQ.2.D0 .AND. ZEJc(Nejc).EQ.1.D0) ip = 3
      IF (AEJc(Nejc).EQ.3.D0 .AND. ZEJc(Nejc).EQ.1.D0) ip = 4
      IF (AEJc(Nejc).EQ.3.D0 .AND. ZEJc(Nejc).EQ.2.D0) ip = 5
      IF (AEJc(Nejc).EQ.4.D0 .AND. ZEJc(Nejc).EQ.2.D0) ip = 6
      IF (AEJc(Nejc).EQ.6.D0 .AND. ZEJc(Nejc).EQ.3.D0) ip = 7
      IF (AEJc(Nejc).EQ.7.D0 .AND. ZEJc(Nejc).EQ.3.D0) ip = 8
      IF (AEJc(Nejc).EQ.7.D0 .AND. ZEJc(Nejc).EQ.4.D0) ip = 9
C-----Data initialization
C     angstep = 2.5
      angstep = 180.d0/(NANgela-1)
      CALL INIT(Nejc,Nnuc)

      rmatch = 1.35d0*A(nnuc)**(1./3.) +  10.d0*0.65d0
      if(rmatch.lt.25.d0) rmatch = 25.d0
      if (ZEjc(nejc).gt.0) rmatch = 40.d0      

      ECIs1 = BECis1
C-----Rotational model
      ECIs1(1:1) = 'F'
C-----Deformation read instead of deformation lengths (T)
C-----for Woods Saxon form factors
      ECIs1(6:6) = 'F'
C-----Coulomb potential deformed
      ECIs1(11:11) = 'T'
C-----Real and imaginary central potential deformed
      ECIs1(12:12) = 'T'
C-----Real SO potential deformed
      ECIs1(13:13) = 'F'
C-----Imaginary SO potential deformed
      ECIs1(14:14) = 'F'

      convg=1.0d-8
C-----ECIS iteration scheme is used.
      ECIs1(21:21) = 'F'
C-----Usual coupled equations instead of ECIS scheme is used
C     for non-zero spins or energies below 10 MeV
      if(XJLv(1,Nnuc).gt.0.d0 .OR. DABS( - El).LT.21.d0) THEN
        ECIs1(21:21) = 'T'
        convg=1.0d-10
      endif
C-----Shift to coupled equations if convergence is not achieved
      ECIs1(23:23) = 'T'
C-----Calculations at experimental angles
      IF (ICAlangs.GT.0) ECIs1(31:31) = 'T'

      ECIs2 = BECis2
      ECIs2(1:1) = 'F'
C-----Cmatrix output
      ECIs2(5:5) = 'F'
C-----Smatrix output
      ECIs2(6:6) = 'T'
      ECIs2(10:10) = 'T'

C-----Penetrabilities punched on cards
      ECIs2(9:9) = 'T'
      ECIs2(13:13) = 'T'
C-----Angular distribution is calculated
      ECIs2(14:14) = 'T'
C-----Legendre coefficients output
      ECIs2(15:15) = 'T'

      ECIs2(20:20) = 'T'

C     DWBA option added
      IF (DIRect.EQ.3 .OR. Ldwba) THEN
C-----Iteration scheme used for DWBA
         ECIs1(21:21) = 'F'
C--------DWBA
         ECIs1(30:30) = 'T'
c        ECIs2(42:42) = 'T'
C        ECIs2(40:40) = 'T'
      ENDIF
      xmas_nejc = EJMass(Nejc)
      xmas_nnuc = (A(Nnuc)*AMUmev + XMAss(Nnuc))/AMUmev
      xratio = xmas_nnuc/(xmas_nejc + xmas_nnuc)
      IF (El.LT.0.D0) THEN
         El = DABS( - El)
         elab = El
         ikey = -1
      ELSE
         ecms = El
         ikey = +1
      ENDIF
C
C-----Transformation of energies from laboratory to center-of-mass if needed
C-----is done inside SETPOTS() -> OMPAR()
C
      CALL SETPOTS(Nejc,Nnuc,elab,ecms,xmas_nejc,xmas_nnuc,ak2,ikey)
C-----relativistic kinematics ?
      IF (IRElat(Nejc,Nnuc).GT.0 .OR. FLGrel.EQ.'y') then
        ECIs1(8:8) = 'T'
        ECIs2(45:45)='T'  ! reduced energy is used
      ENDIF

      IF (IDRs.GT.0) THEN
C      Preparing dispersive CC input for ECIS
       ECIs1(10:10)='T'! ENERGY DEPENDENT POTENTIALS BY DISPERSION RELATIONS (GS)
       ECIs1(20:20)='T'! ENERGY DEPENDENT POTENTIALS BY DISPERSION RELATIONS (EXC.LEV.)
      ENDIF
C-----Only for target, find open channels
C-----At least ground state is always open
      nd_nlvop = 1
      nd_cons = 1
      IF (Inlkey.NE.0) THEN
         DO j = 2, ND_nlv
            IF (.NOT.Ldwba .AND. ICOllev(j).GT.LEVcc) CYCLE
            nd_cons = nd_cons + 1
            eee = elab - D_Elv(j)/xratio
            IF (eee.GT.0.0001) nd_nlvop = nd_nlvop + 1
         ENDDO
         IF (.NOT.Ldwba .AND. Inlkey.GT.0 .AND. nd_nlvop.EQ.1)
     &       WRITE (8,*)
     &               ' All inelastic channels are closed at this energy'
      ENDIF
C
C-----Considering only coupled levels if CC calculation
      ncollx = nd_cons
C
      iterm = 20
C-----For DWBA only one iteration is used
      IF (ECIs1(30:30).EQ.'T') iterm = 1
C     IF (ECIs2(42:42).EQ.'T') iterm = 1

C-----Defining one potential for each collective level
C-----If channel is closed ground state potential is used for this level
      npp = nd_nlvop
C
      zerosp = 0.d0
C
      ldwmax = 2.4*1.25*A(Nnuc)**0.33333333*0.22*SQRT(xmas_nejc*elab)
C-----Maximum number of channel spin (increased to 100 for high energy scattering)
      njmax = MAX(2*ldwmax,20)

      lodd = .false.
      IF( (mod(nint(Z(Nnuc)),2).ne.0 .or.
     >     mod(nint(A(Nnuc)-Z(Nnuc)),2).ne.0) .and.
     >     mod(nint(A(Nnuc)),2).ne.0 ) lodd = .true.

      IF (Inlkey.EQ.0) THEN
C-------writing input
         OPEN (UNIT = 1,STATUS = 'unknown',FILE = 'ecSPH.inp')
C-------CARD 1 : Title
         WRITE (1,
     &     '(f10.5,'' MeV '',a8,'' on '',i3,a2,'': SPHERICAL OMP'')'
     &          ) El, PARname(ip), NINT(A(Nnuc)), NUC(NINT(Z(Nnuc)))
      ELSE
C-------writing input
         OPEN (UNIT = 1,STATUS = 'unknown',FILE = 'ecVIB.inp')
C-------CARD 1 : Title
         WRITE (1,
     &     '(f10.5,'' MeV '',a8,'' on '',i3,a2,'': VIBRATIONAL MODEL'')'
     &      ) El, PARname(ip), NINT(A(Nnuc)), NUC(NINT(Z(Nnuc)))
      ENDIF
C-----CARD 2
      WRITE (1,'(a50)') ECIs1
C-----CARD 3
      WRITE (1,'(a50)') ECIs2
C-----CARD 4
C-----make sure that all contributions to s-wave scattering are included
      ilv = 1
      If(Nnuc.eq.0) ilv = Levtarg
C     only one potential for a full dispersive calculation
      nppaa = npp
      if (IDRs.gt.0) nppaa = 1

      WRITE (1,'(4i5)') ncollx, njmax, iterm, nppaa
C-----Matching radius
C-----CARD 5
      WRITE (1,'(2f10.5,10x,1p,3(2x,e8.1))') 
     +    RStep,rmatch,convg,convg,convg
C     To obtain Legendre expansion a blank card calling for default values of the expansion
      WRITE (1,*)
C-----ground state
C     ch = '-'
C     IF ( LVP(LEVtarg,Nnuc).GT.0 ) ch = '+'
C-----Important: Instead of using TARGET SPIN (XJLV(1,NNUC)) and PARITY(ch)
C-----A.Koning always used in PREGNASH SPIN=0, ch='+'
C-----It is justified for vibrational model and DWBA calculations
C-----so we are using zero spin and positive parity herehere
C-----NOT TRUE for rotational model calculations (see ecis_CCrot)
C     WRITE(1, '(f5.2,2i2,a1,5f10.5)')XJLV(1,NNUC),0,1, ch, elab,
      WRITE (1,'(f5.2,2i2,a1,f10.6,4F10.5)') zerosp, 0, 1, '+', elab,
     &                              SEJc(Nejc), xmas_nejc, xmas_nnuc,
     &                              Z(Nnuc)*ZEJc(Nejc)
C-----0 phonon involved
      WRITE (1,'( )')
      IF (Inlkey.NE.0) THEN
C--------discrete levels
         nwrite = 1
         DO j = 2, ND_nlv
C--------All levels with icollev(j)>LEVcc should be calculated by DWBA
            IF (.NOT.Ldwba .AND. ICOllev(j).GT.LEVcc) CYCLE
            ch = '-'
            IF (D_Lvp(j).GT.0) ch = '+'
C-----------If channel is closed ground state potential is used for this level
            eee = elab - D_Elv(j)/xratio
            dtmp = D_Xjlv(j)
            ! making integer spin for odd nuclides CC levels in DWBA calculations
            if(Ldwba .and. lodd) dtmp = INT(D_Xjlv(j))
            IF (eee.GE.0.0001) THEN

               IF(IDRs.eq.0) then
                 nwrite = nwrite + 1
                 WRITE (1,'(f5.2,2i2,a1,5f10.5)') dtmp, 0, nwrite,
     &                ch, D_Elv(j)

               ELSE

                 WRITE (1,'(f5.2,2i2,a1,5f10.5)') dtmp, 0, 1, ch,

     &                D_Elv(j)

               ENDIF
            ELSE
               WRITE (1,'(f5.2,2i2,a1,5f10.5)') dtmp, 0, 1, ch,

     &                D_Elv(j)

            ENDIF
            IF (Ldwba .OR. DIRect.EQ.3) THEN
C--------------If DWBA, all states are assumed to be one phonon
               WRITE (1,'(3i5)') 1, j - 1, 0
            ELSEIF (IPH(j).EQ.1) THEN
               WRITE (1,'(3i5)') IPH(j), j - 1, 0
            ELSE
C-----------two  phonon states if exist are formed from the quadrupole
C-----------quadrupole phonon spin is equal to 2+ phonon
               WRITE (1,'(3i5)') IPH(j), 1, 1
            ENDIF
         ENDDO
C
C--------deformations: phonon description
C
         DO j = 2, ND_nlv
C-----------All levels with icollev(j)>LEVcc should be calculated by DWBA
            IF (.NOT.Ldwba .AND. ICOllev(j).GT.LEVcc) CYCLE
            IF (Ldwba .OR. DIRect.EQ.3) THEN
C--------------If DWBA, all states are assumed to be one phonon
               WRITE (1,'(i5,5x,6f10.5)') INT(D_Xjlv(j)), D_Def(j,2)
            ELSE
C--------------only one phonon states need deformations as input
               IF (IPH(j).EQ.1) WRITE (1,'(i5,5x,6f10.5)')
     &                                 INT(D_Xjlv(j)), D_Def(j,2)
            ENDIF
         ENDDO
      ENDIF
C
C-----potential parameters
C-----1) groundstate
C     write(1,'(3f10.5)') v,rv,av
      IF (VOM(Nejc,Nnuc).NE.0.) THEN
         IF(IDRs.gt.0) then
           WRITE (1,'(3f10.5)') VOM(Nejc,Nnuc)-DWVcor, 
     &                          RVOm(Nejc,Nnuc), AVOm(Nejc,Nnuc)
         ELSE
           WRITE (1,'(3f10.5)') VOM(Nejc,Nnuc), 
     &                          RVOm(Nejc,Nnuc), AVOm(Nejc,Nnuc)
         ENDIF
      ELSE
         WRITE (1,'(3f10.5)') 0., 0., 0.
      ENDIF
C-----write(1,'(3f10.5)') w,rw,aw
      IF (WOMv(Nejc,Nnuc).NE.0.) THEN
         WRITE (1,'(3f10.5)') WOMv(Nejc,Nnuc), RWOmv(Nejc,Nnuc),
     &                        AWOmv(Nejc,Nnuc)
      ELSE
         WRITE (1,'(3f10.5)') 0., 0., 0.
      ENDIF
C-----write(1,'(3f10.5)') vd,rvd,avd
      IF (VOMs(Nejc,Nnuc).NE.0.) THEN
         IF(IDRs.gt.0) then
           WRITE (1,'(3f10.5)') 0.d0, 
     &                          RWOm(Nejc,Nnuc), AWOm(Nejc,Nnuc)
         ELSE
           WRITE (1,'(3f10.5)') VOMs(Nejc,Nnuc), 
     &                          RWOm(Nejc,Nnuc), AWOm(Nejc,Nnuc)
         ENDIF
      ELSE
         WRITE (1,'(3f10.5)') 0., 0., 0.
      ENDIF
C-----write(1,'(3f10.5)') wd,rwd,awd
      IF (WOMs(Nejc,Nnuc).NE.0.) THEN
         WRITE (1,'(3f10.5)') WOMs(Nejc,Nnuc), RWOm(Nejc,Nnuc),
     &                        AWOm(Nejc,Nnuc)
         IF (SFIom(Nejc,Nnuc).LT.0.0D0) THEN
            WRITE (*,*)
            WRITE (*,*) ' ERROR !!!'
            WRITE (*,*)
     &                 ' ECIS can not be used with Gaussian formfactors'
            WRITE (*,*) ' for imaginary surface contribution '
            WRITE (*,*) ' Change OMP potential for elastic channel'
            WRITE (8,*)
            WRITE (8,*) ' ERROR !!!'
            WRITE (8,*)
     &                 ' ECIS can not be used with Gaussian formfactors'
            WRITE (8,*) ' for imaginary surface contribution'
            WRITE (8,*) ' Change OMP potential for elastic channel'
            STOP ' No Gaussian form factors in ECIS!'
         ENDIF
      ELSE
         WRITE (1,'(3f10.5)') 0., 0., 0.
      ENDIF
C-----write(1,'(3f10.5)') vvso,rrvso,aavso
      IF (VSO(Nejc,Nnuc).NE.0.) THEN
         IF(IDRs.gt.0) then
            WRITE (1,'(3f10.5)') VSO(Nejc,Nnuc)-DSOcor, 
     &                           RVSo(Nejc,Nnuc), AVSo(Nejc,Nnuc)
         ELSE
            WRITE (1,'(3f10.5)') VSO(Nejc,Nnuc), 
     &                           RVSo(Nejc,Nnuc), AVSo(Nejc,Nnuc)
         ENDIF 
      ELSE
         WRITE (1,'(3f10.5)') 0., 0., 0.
      ENDIF
C-----write(1,'(3f10.5)') wwso,rwso,awso
      IF (WSO(Nejc,Nnuc).NE.0.) THEN
         WRITE (1,'(3f10.5)') WSO(Nejc,Nnuc), RWSo(Nejc,Nnuc),
     &                        AWSo(Nejc,Nnuc)
      ELSE
         WRITE (1,'(3f10.5)') 0., 0., 0.
      ENDIF
C-----write(1,'(3f10.5)') rc,0.,0.
      WRITE (1,'(3f10.5)') RCOul(Nejc,Nnuc), ACOul(Nejc,Nnuc), 0.
      WRITE (1,'(3f10.5)') 0., 0., 0.

      IF(IDRs.gt.0) then
C       Storing reference depths for the first level
        vvref  = VOM(Nejc,Nnuc) - DWVcor
        wvref  = WOMv(Nejc,Nnuc)
        wsref  = WOMs(Nejc,Nnuc)
        vsoref = VSO(Nejc,Nnuc) - DSOcor
        wsoref = WSO(Nejc,Nnuc)

        IF (ICAlangs.GT.0) THEN
          WRITE (1,'(3f10.5)') 0.D0, 180.0D0, 180.D0
          WRITE (1,'(2i5)') ICAlangs,0
          DO i = 1, ICAlangs
            WRITE(1,'(A1,I1,I3,2I5,5X,3F10.5)') 'F', 0, NANgela, i, 0,
     1                                    1.0D0, 1.0D0, 0.0D0
            DO iang = 1, NANgela
              WRITE(1,'(5F10.5)') ANGles(iang),1.0D3,1.0D2,0.0D0,0.0D0
            END DO
          END DO
        ELSE
          WRITE (1,'(3f10.5)') 0.D0, angstep, 180.D0
        ENDIF

        nn2 = 2
        if(ea.ge.1000.d0) nn2=0 ! there is non non-locality
        write(1,'(14I5)')  +1,  -nn2,  nnv,  nns,  -nnl,  1  

        fv  = 0.d0
        fs  = 0.d0
        fvv = 0.d0
        fvs = 0.d0
        tv  = 0.d0
        ts  = 0.d0
        fso = 0.d0
        tso = 0.d0

        if(vvref.ne.0.d0) 
     >    fvv = (VOM(Nejc,Nnuc) - DWVcor - vvref)/vvref
        if(wvref.ne.0.d0) then
          fv = DWVcor/wvref
          tv = (WOMv(Nejc,Nnuc) - wvref)/wvref
        endif
        if(wsref.ne.0.d0) then
          fs = DWScor/wsref
          ts = (WOMs(Nejc,Nnuc) - wsref)/wsref
        endif
        if(wsoref.ne.0.d0) then
          fso = DSOcor/wsoref
          tso = (WSO(Nejc,Nnuc) - wsoref )/wsoref
        endif

        if(vsoref.ne.0.d0) 
     >    fvs = (VSO(Nejc,Nnuc) - DSOcor - vsoref)/vsoref

        write (1,'(2(G10.4,F10.4,G10.4))')                                      
     >        tv ,  fv, fvv, ts, fs  , 0.d0
        write (1,'(2(G10.4,F10.4,G10.4))') 
     >        tso, fso, fvs
      endif 

      IF (Inlkey.NE.0) THEN
C
C------2) discrete levels
         DO j = 2, ND_nlv
C           All levels with icollev(j)>LEVcc should be calculated by DWBA
            IF (.NOT.Ldwba .AND. ICOllev(j).GT.LEVcc) CYCLE
            eee = elab - D_Elv(j)/xratio
C-----------If channel is closed ground state potential is used for this level
C           IF (eee.GE.0.0001) THEN
              IF(IDRs.gt.0) then
C---------------SETPOTS : subroutine for optical model parameters
C---------------From  cms system to Lab
C               ecms = eee
C               ikey = +1   ! WE NEED TO SET IKEY TO -1 AS ENERGIES ARE IN THE LAB FRAME
                elabe = eee   ! Changed on Aug. 2008
                ikey  = -1   
C
C---------------Transformation of energies from laboratory to center-of-mass
C---------------if needed is done inside SETPOTS() -> OMPAR()
C
                CALL SETPOTS(Nejc,Nnuc,elabe,ecms,xmas_nejc,xmas_nnuc,
     &                      ak2,ikey)
                fv  = 0.d0
                fs  = 0.d0
                fvv = 0.d0
                fvs = 0.d0
                tv  = 0.d0
                ts  = 0.d0
                fso = 0.d0
                tso = 0.d0

                if(vvref.ne.0.d0) 
     >            fvv = (VOM(Nejc,Nnuc) - DWVcor - vvref)/vvref
                if(wvref.ne.0.d0) then
                  fv  = DWVcor/wvref
                  tv  = (WOMv(Nejc,Nnuc) - wvref)/wvref
                endif
                if(wsref.ne.0.d0) then
                  fs  = DWScor/wsref
                  ts  = (WOMs(Nejc,Nnuc) - wsref)/wsref
                endif
                if(wsoref.ne.0.d0) then
                  fso = DSOcor/wsoref
                  tso = (WSO(Nejc,Nnuc)  - wsoref )/wsoref
                endif

                 if(vsoref.ne.0.d0) 
     >            fvs = (VSO(Nejc,Nnuc) - DSOcor - vsoref)/vsoref

                write (1,'(2(G10.4,F10.4,G10.4))')                                       
     >            tv, fv  , fvv, 
     >            ts, fs  , 0.d0
                write (1,'(2(G10.4,F10.4,G10.4))') 
     >            tso, fso, fvs

                CYCLE
              ENDIF
C           ENDIF
            IF (eee.GT.0.0001) THEN
C---------------SETPOTS : subroutine for optical model parameters
C---------------From  cms system to Lab
C               ecms = eee
C               ikey = +1   ! WE NEED TO SET IKEY TO -1 AS ENERGIES ARE IN THE LAB FRAME
                elabe = eee   ! Changed on Aug. 2008
                ikey  = -1   
C
C---------------Transformation of energies from laboratory to center-of-mass
C---------------if needed is done inside SETPOTS() -> OMPAR()
C
                CALL SETPOTS(Nejc,Nnuc,elabe,ecms,xmas_nejc,xmas_nnuc,
     &                      ak2,ikey)
C
C--------------potential parameters
C--------------write(1,'(3f10.5)') v,rv,av
               IF (VOM(Nejc,Nnuc).NE.0.) THEN
                  WRITE (1,'(3f10.5)') VOM(Nejc,Nnuc),
     &                                 RVOm(Nejc,Nnuc),
     &                                 AVOm(Nejc,Nnuc)
               ELSE
                  WRITE (1,'(3f10.5)') 0., 0., 0.
               ENDIF
C--------------write(1,'(3f10.5)') w,rw,aw
               IF (WOMv(Nejc,Nnuc).NE.0.) THEN
                  WRITE (1,'(3f10.5)') WOMv(Nejc,Nnuc),
     &                                 RWOmv(Nejc,Nnuc),
     &                                 AWOmv(Nejc,Nnuc)
               ELSE
                  WRITE (1,'(3f10.5)') 0., 0., 0.
               ENDIF
C--------------write(1,'(3f10.5)') vd,rvd,avd
               IF (VOMs(Nejc,Nnuc).NE.0.) THEN
                  WRITE (1,'(3f10.5)') VOMs(Nejc,Nnuc),
     &                                 RWOm(Nejc,Nnuc),
     &                                 AWOm(Nejc,Nnuc)
               ELSE
                  WRITE (1,'(3f10.5)') 0., 0., 0.
               ENDIF
C--------------write(1,'(3f10.5)') wd,rwd,awd
               IF (WOMs(Nejc,Nnuc).NE.0.) THEN
                  WRITE (1,'(3f10.5)') WOMs(Nejc,Nnuc),
     &                                 RWOm(Nejc,Nnuc),
     &                                 AWOm(Nejc,Nnuc)
               ELSE
                  WRITE (1,'(3f10.5)') 0., 0., 0.
               ENDIF
C--------------write(1,'(3f10.5)') vvso,rrvso,aavso
               IF (VSO(Nejc,Nnuc).NE.0.) THEN
                  WRITE (1,'(3f10.5)') VSO(Nejc,Nnuc),
     &                                 RVSo(Nejc,Nnuc),
     &                                 AVSo(Nejc,Nnuc)
               ELSE
                  WRITE (1,'(3f10.5)') 0., 0., 0.
               ENDIF
C--------------write(1,'(3f10.5)') wwso,rwso,awso
               IF (WSO(Nejc,Nnuc).NE.0.) THEN
                  WRITE (1,'(3f10.5)') WSO(Nejc,Nnuc),
     &                                 RWSo(Nejc,Nnuc),
     &                                 AWSo(Nejc,Nnuc)
               ELSE
                  WRITE (1,'(3f10.5)') 0., 0., 0.
               ENDIF
C--------------write(1,'(3f10.5)') rc,0.,0.
               WRITE (1,'(3f10.5)')
     &                RCOul(Nejc,Nnuc), ACOul(Nejc,Nnuc), 0.
               WRITE (1,'(3f10.5)') 0., 0., 0.
            ENDIF
         ENDDO
      ENDIF

      IF (IDRs.eq.0) then
C
C-------Angular distribution step
C
        IF (ICAlangs.GT.0) THEN
          WRITE (1,'(3f10.5)') 0.D0, 180.0D0, 180.D0
          WRITE (1,'(2i5)') ICAlangs,0
          DO i = 1, ICAlangs
            WRITE(1,'(A1,I1,I3,2I5,5X,3F10.5)') 'F', 0, NANgela, i, 0,
     1                                    1.0D0, 1.0D0, 0.0D0
            DO iang = 1, NANgela
              WRITE(1,'(5F10.5)') ANGles(iang),1.0D3,1.0D2,0.0D0,0.0D0
            END DO
          END DO
        ELSE
          WRITE (1,'(3f10.5)') 0.D0, angstep, 180.D0
        ENDIF
      ENDIF
      WRITE (1,'(4hFIN )')
      CLOSE (UNIT = 1)

      IF (Inlkey.EQ.0) THEN
        IF (IOPsys.EQ.0) THEN
          ctmp = 'cp ecSPH.inp ecis06.inp'
          iwin = PIPE(ctmp)
        ELSE
          iwin = PIPE('copy ecSPH.inp ecis06.inp >NUL')
        ENDIF
      ELSE
        IF (IOPsys.EQ.0) THEN
          ctmp = 'cp ecVIB.inp ecis06.inp'
          iwin = PIPE(ctmp)
        ELSE
          iwin = PIPE('copy ecVIB.inp ecis06.inp >NUL')
        ENDIF
      ENDIF
C
C-----Running ECIS06
C
      CALL ECIS('ecis06 ')

      IF (Inlkey.EQ.0) THEN
C        CALL ECIS('ecSPH.inp','ECIS_SPH.out')
         IF (IOPsys.EQ.0) THEN
            ctmp = 'mv ecis06.out ECIS_SPH.out'
            iwin = PIPE(ctmp)
            ctmp = 'rm ecis06.inp'
            iwin = PIPE(ctmp)
         ELSE
            iwin = PIPE('move ecis06.out ECIS_SPH.out >NUL')
            iwin = PIPE('del ecis06.inp >NUL')
         ENDIF
      ELSE
C        CALL ECIS('ecVIB.inp','ECIS_VIB.out')
C        CALL ECIS('ecis06 ')
         IF (IOPsys.EQ.0) THEN
            ctmp = 'mv ecis06.out ECIS_VIB.out'
            iwin = PIPE(ctmp)
            ctmp = 'rm ecis06.inp'
            iwin = PIPE(ctmp)
         ELSE
            iwin = PIPE('move ecis06.out ECIS_VIB.out >NUL')
            iwin = PIPE('del ecis06.inp >NUL')
         ENDIF
      ENDIF
      END


      SUBROUTINE ECIS_CCVIBROT(Nejc,Nnuc,El,Inlkey)
C
C     -------------------------------------------------------------
C     |    Create input files for ECIS06 for COUPLED CHANNELS     |
C     |    rotational-vibrational model                           |
C     |               v1.0     S.Masetti 5/99                     |
C     |               v2.0     R.Capote  1/2001                   |
C     |               v3.0     R.Capote  5/2001 (DWBA added)      |
C     |               v4.0     R.Capote  8/2008 ECIS06 version    |
C     -------------------------------------------------------------
C
C     ****
C     IP = 1 NEUTRON
C     2 PROTON
C     3 DEUTERON
C     4 TRITON
C     5 HELIUM-3
C     6 ALPHA
C     >6 HEAVY ION
C
C     INTEGER NINT
C
      INCLUDE "dimension.h"
      INCLUDE "global.h"
      INCLUDE "pre_ecis.h"
C-----For dispersive optical model potentials
C-----It is ripl2empireS.h because it must be compatible
C-----with global.h declarations so some variables must be renamed

      INCLUDE 'ripl2empireS.h'
C
C COMMON variables
C
      REAL*8 AAV, AS, AAvso, BS, BV, Bvso, CS, ETMP, EF, EP, EA
      INTEGER IQ, NNS, NNV, NNL, IDRs
      COMMON /DENERGY/ ETMP, EF, EP, EA
      common /pdatas/As,Bs,Cs,nns,iq
      common /pdatav/AAv,Bv,nnv
      common /pdatao/AAvso,Bvso,nnl

      REAL*8 VHFnum,DWVcor,VSOnum,DSOcor,ALhf,ALso,
     +               DWScor,ALpha,RSTep
      common /disper/VHFnum,DWVcor,VSOnum,DSOcor,ALhf,ALso,
     +               DWScor,ALpha,RSTep,IDRs
C
C     Dummy arguments
C
      DOUBLE PRECISION El
      INTEGER Inlkey, Nejc, Nnuc
C
C Local variables
C
      DOUBLE PRECISION ak2, angstep, ecms, eee, elab, elabe, rmatch,
     &                 xmas_nejc, xmas_nnuc, xratio, convg

      DOUBLE PRECISION vvref, wvref, wsref, vsoref, wsoref
      DOUBLE PRECISION fv, fs, fvv, fvs, tv, ts, fso, tso

      CHARACTER*1 ch
      DOUBLE PRECISION DABS
      INTEGER ikey, ip, iterm, j, jdm, k, ldwmax, lev(NDLV), nppaa,
     &        nd_cons, nd_nlvop, ncollm, njmax, npho, npp, nwrite

      CHARACTER*132 ctmp
      INTEGER*4 PIPE,iwin
      INTEGER INT, NINT

      IF (AEJc(Nejc).EQ.1.D0 .AND. ZEJc(Nejc).EQ.0.D0) ip = 1
      IF (AEJc(Nejc).EQ.1.D0 .AND. ZEJc(Nejc).EQ.1.D0) ip = 2
      IF (AEJc(Nejc).EQ.2.D0 .AND. ZEJc(Nejc).EQ.1.D0) ip = 3
      IF (AEJc(Nejc).EQ.3.D0 .AND. ZEJc(Nejc).EQ.1.D0) ip = 4
      IF (AEJc(Nejc).EQ.3.D0 .AND. ZEJc(Nejc).EQ.2.D0) ip = 5
      IF (AEJc(Nejc).EQ.4.D0 .AND. ZEJc(Nejc).EQ.2.D0) ip = 6
      IF (AEJc(Nejc).EQ.6.D0 .AND. ZEJc(Nejc).EQ.3.D0) ip = 7
      IF (AEJc(Nejc).EQ.7.D0 .AND. ZEJc(Nejc).EQ.3.D0) ip = 8
      IF (AEJc(Nejc).EQ.7.D0 .AND. ZEJc(Nejc).EQ.4.D0) ip = 9
C-----Data initialization
C     angstep = 2.5
      angstep = 180.d0/(NANgela-1)
      CALL INIT(Nejc,Nnuc)

      rmatch = 1.35d0*A(nnuc)**(1./3.) +  10.d0*0.65d0
      if(rmatch.lt.25.d0) rmatch = 25.d0
      if (ZEjc(nejc).gt.0) rmatch = 40.d0      

      ECIs1 = BECis1
C-----Rotational model
      ECIs1(1:1) = 'T'
C-----Deformation read instead of deformation lengths
C-----for Woods Saxon form factors
      ECIs1(6:6) = 'F'
C-----Coulomb potential deformed
      ECIs1(11:11) = 'T'
C-----Real and imaginary central potential deformed
      ECIs1(12:12) = 'T'
C-----Real SO potential deformed
      ECIs1(13:13) = 'F'
C-----Imaginary SO potential deformed
      ECIs1(14:14) = 'F'

      convg=1.0d-8
C-----ECIS iteration scheme is used.
      ECIs1(21:21) = 'F'
C-----Usual coupled equations instead of ECIS scheme is used
C     for non-zero spins or energies below 10 MeV
      if(XJLv(1,Nnuc).gt.0.d0 .OR. DABS( - El).LT.21.d0) THEN
        ECIs1(21:21) = 'T'
        convg=1.0d-10
      endif
C-----Shift to coupled equations if convergence is not achieved
      ECIs1(23:23) = 'T'
C-----Calculations at experimental angles
      IF (ICAlangs.GT.0) ECIs1(31:31) = 'T'


      ECIs2 = BECis2
      ECIs2(1:1) = 'F'
C-----Cmatrix output
      ECIs2(5:5) = 'F'
C-----Smatrix output
      ECIs2(6:6) = 'T'
      ECIs2(10:10) = 'T'

C-----Penetrabilities punched on cards
      ECIs2(9:9) = 'T'
      ECIs2(13:13) = 'T'
C-----Angular distribution is calculated
      ECIs2(14:14) = 'T'
C-----Legendre coefficients output
      ECIs2(15:15) = 'T'

      ECIs2(20:20) = 'T'

C-----DWBA option added
      IF (DIRect.EQ.3) THEN
C-----Iteration scheme used for DWBA
         ECIs1(21:21) = 'F'
C--------DWBA
         ECIs1(30:30) = 'F'
c        ECIs2(42:42) = 'T'
C        ECIs2(40:40) = 'T'
      ENDIF
      xmas_nejc = EJMass(Nejc)
      xmas_nnuc = (A(Nnuc)*AMUmev + XMAss(Nnuc))/AMUmev
      xratio = xmas_nnuc/(xmas_nejc + xmas_nnuc)
C-----From cms system to Lab (ECIS do inverse convertion)
      IF (El.LT.0.D0) THEN
         El = DABS( - El)
         elab = El
         ikey = -1
      ELSE
         ecms = El
         ikey = +1
      ENDIF
C
C-----Transformation of energies from laboratory to center-of-mass if needed
C-----is done inside SETPOTS() -> OMPAR()
C
      CALL SETPOTS(Nejc,Nnuc,elab,ecms,xmas_nejc,xmas_nnuc,ak2,ikey)
C-----relativistic kinematics ?
      IF (IRElat(Nejc,Nnuc).GT.0 .OR. FLGrel.EQ.'y') then
        ECIs1(8:8) = 'T'
        ECIs2(45:45)='T'  ! reduced energy is used
      ENDIF

      IF (IDRs.GT.0) THEN
C      Preparing dispersive CC input for ECIS
       ECIs1(10:10)='T'! ENERGY DEPENDENT POTENTIALS BY DISPERSION RELATIONS (GS)
       ECIs1(20:20)='T'! ENERGY DEPENDENT POTENTIALS BY DISPERSION RELATIONS (EXC.LEV.)
      ENDIF
C
C-----Only for target, find open channels
C-----At least ground state is always open 
      nd_nlvop = 1
      nd_cons = 1
      IF (ND_nlv.GT.0) THEN
         DO j = 2, ND_nlv
C-----------All levels with icollev(j)>LEVcc should be calculated by DWBA
            IF (Inlkey.EQ.0 .AND. ICOllev(j).GT.LEVcc) CYCLE
C-----------All levels with icollev(j)<LEVcc should be calculated by CC
            IF (Inlkey.EQ.1 .AND. ICOllev(j).LT.LEVcc) CYCLE
            nd_cons = nd_cons + 1
            eee = elab - D_Elv(j)/xratio
            IF (eee.GT.0.0001) nd_nlvop = nd_nlvop + 1
         ENDDO
      ENDIF
      IF (nd_nlvop.EQ.1) WRITE (8,*)
     &               ' All inelastic channels are closed at this energy'
C-----Considering even closed channels in calculations
      ncollm = nd_cons
C
      iterm = 20
C-----For DWBA only one iteration is used
      IF (ECIs2(42:42).EQ.'T' .OR. Inlkey.EQ.1) iterm = 1
C
C-----Defining one potential for each collective level
C-----If channel is closed ground state potential is used for this level
      npp = nd_nlvop
C
      ldwmax = 2.4*1.25*A(Nnuc)
     &         **0.33333333*0.22*SQRT(xmas_nejc*elab)
C-----Maximum number of channel spin
      njmax = MAX(2*ldwmax,20)
C-----Writing input
      OPEN (UNIT = 1,STATUS = 'unknown',FILE = 'ecVIBROT.inp')
C-----CARD 1 : Title
      WRITE (1,
     &'(f10.5,'' MeV '',a8,'' on '',i3,a2,'': VIBR-ROTATIONAL CC'')'
     &) El, PARname(ip), NINT(A(Nnuc)), NUC(NINT(Z(Nnuc)))
C-----CARD 2
      WRITE (1,'(a50)') ECIs1
C-----CARD 3
      WRITE (1,'(a50)') ECIs2
C-----CARD 4
C-----make sure that all contributions to s-wave scattering are included
      ilv = 1
      If(Nnuc.eq.0) ilv = Levtarg
      jdm = XJLv(ilv,Nnuc) + SEJc(Nejc) + 0.5
C     only one potential for a full dispersive calculation
      nppaa = npp
      if (IDRs.gt.0) nppaa = 1
      WRITE (1,'(4i5,30x,i5)') ncollm, njmax, iterm, nppaa, jdm
C-----CARD 5
      WRITE (1,'(2f10.5,10x,1p,3(2x,e8.1))') 
     +    RStep,rmatch,convg,convg,convg
C     To obtain Legendre expansion a blank card calling for default values of the expansion
      WRITE(1, *)
C-----Matching radius calculated within ECIS
C     WRITE(1, *)
      ch = '-'
      IF (LVP(ilv,Nnuc).GT.0) ch = '+'
      WRITE (1,'(f5.2,2i2,a1,F10.6,4f10.5)') XJLv(ilv,Nnuc), 0, 1, ch,
     &                              elab, SEJc(Nejc), xmas_nejc,
     &                              xmas_nnuc, Z(Nnuc)*ZEJc(Nejc)
C-----Discrete levels
      npho = 0
      nwrite = 1
      DO j = 2, ND_nlv
C--------All levels with icollev(j)>LEVcc should be calculated by DWBA
         IF (Inlkey.EQ.0 .AND. ICOllev(j).GT.LEVcc) CYCLE
C--------All levels with icollev(j)<LEVcc should be calculated by CC
         IF (Inlkey.EQ.1 .AND. ICOllev(j).LT.LEVcc) CYCLE
         ch = '-'
         IF (D_Lvp(j).GT.0) ch = '+'
C--------If channel is closed ground state potential is used for this level
         eee = elab - D_Elv(j)/xratio
C
C        Vibrational-rotational model modified as proposed
C        by Harm Wienke, March 31, 2005 (vibrational bands can be included)
C
C--------In vibrational-rotational model IPH(j) is phonon number
         IF (eee.GE.0.0001) THEN
           if(nppaa.gt.1) nwrite = nwrite + 1
           IF(IPH(j).ge.1) then
               WRITE (1,'(f5.2,2i2,a1,5f10.5)') D_Xjlv(j), 1, nwrite,
     &             ch, D_Elv(j)
            ELSE
               WRITE (1,'(f5.2,2i2,a1,5f10.5)') D_Xjlv(j), 0, nwrite,
     &             ch, D_Elv(j)
            ENDIF
         ELSE
           IF(IPH(j).ge.1) then
               WRITE (1,'(f5.2,2i2,a1,5f10.5)') D_Xjlv(j), 1, 1, ch,
     &             D_Elv(j)
            ELSE
               WRITE (1,'(f5.2,2i2,a1,5f10.5)') D_Xjlv(j), 0, 1, ch,
     &             D_Elv(j)
            ENDIF
         ENDIF
         IF (IPH(j).NE.0) THEN
           if(IPH(j).ne.IPH(j-1)) then
               npho = npho + 1
               lev(npho) = j
           endif
            WRITE (1,'(10i5)') 1, IPH(j)
         ENDIF
      ENDDO
C-----Description of phonons
      IF (npho.GT.0) THEN
C--------In vibrational rotational model the IPH(j) array contains the
C--------l orbital angular momentum of the phonon.
C--------We are assuming that orbital angular momentum of the phonon is
C--------equal INT(J). The k magnetic quantum number of the vibration is
C--------assumed to be zero !!!!
C--------L_PHO(lev(j))  = IPH(j)
C--------L3_pho(lev(j)) = 0
         DO j = 1, npho
            WRITE (1,'(2i5,6f10.5)') INT(D_Llv(lev(j)) + 0.1),
     &      INT(D_Klv(lev(j))+ 0.1),D_Def(lev(j),2)
         ENDDO
      ENDIF
C-----Deformation of rotational band (only ground state band is present)
C-----IdefCC   = maximum degree of deformation
C-----LMaxCC   = maximum L in multipole decomposition
      WRITE (1,'(2i5,f10.5)') IDEfcc, LMAxcc, D_Xjlv(1)
      WRITE (1,'(6f10.5)') (D_Def(1,k),k = 2,IDEfcc,2)
C-----Potential parameters
C-----1) groundstate
C-----write(1,'(3f10.5)') v,rv,av
C     write(1,'(3f10.5)') v,rv,av
      IF (VOM(Nejc,Nnuc).NE.0.) THEN
         IF(IDRs.gt.0) then
           WRITE (1,'(3f10.5)') VOM(Nejc,Nnuc)-DWVcor, 
     &                          RVOm(Nejc,Nnuc), AVOm(Nejc,Nnuc)
         ELSE
           WRITE (1,'(3f10.5)') VOM(Nejc,Nnuc), 
     &                          RVOm(Nejc,Nnuc), AVOm(Nejc,Nnuc)
         ENDIF
      ELSE
         WRITE (1,'(3f10.5)') 0., 0., 0.
      ENDIF
C-----write(1,'(3f10.5)') w,rw,aw
      IF (WOMv(Nejc,Nnuc).NE.0.) THEN
         WRITE (1,'(3f10.5)') WOMv(Nejc,Nnuc), RWOmv(Nejc,Nnuc),
     &                        AWOmv(Nejc,Nnuc)
      ELSE
         WRITE (1,'(3f10.5)') 0., 0., 0.
      ENDIF
C-----write(1,'(3f10.5)') vd,rvd,avd
      IF (VOMs(Nejc,Nnuc).NE.0.) THEN
         IF(IDRs.gt.0) then
           WRITE (1,'(3f10.5)') 0.d0, 
     &                          RWOm(Nejc,Nnuc), AWOm(Nejc,Nnuc)
         ELSE
           WRITE (1,'(3f10.5)') VOMs(Nejc,Nnuc), 
     &                          RWOm(Nejc,Nnuc), AWOm(Nejc,Nnuc)
         ENDIF
      ELSE
         WRITE (1,'(3f10.5)') 0., 0., 0.
      ENDIF
C-----write(1,'(3f10.5)') wd,rwd,awd
      IF (WOMs(Nejc,Nnuc).NE.0.) THEN
         WRITE (1,'(3f10.5)') WOMs(Nejc,Nnuc), RWOm(Nejc,Nnuc),
     &                        AWOm(Nejc,Nnuc)
         IF (SFIom(Nejc,Nnuc).LT.0.0D0) THEN
            WRITE (*,*)
            WRITE (*,*) ' ERROR !!!'
            WRITE (*,*)
     &                 ' ECIS can not be used with Gaussian formfactors'
            WRITE (*,*) ' for imaginary surface contribution '
            WRITE (*,*) ' Change OMP potential for elastic channel'
            WRITE (8,*)
            WRITE (8,*) ' ERROR !!!'
            WRITE (8,*)
     &                 ' ECIS can not be used with Gaussian formfactors'
            WRITE (8,*) ' for imaginary surface contribution'
            WRITE (8,*) ' Change OMP potential for elastic channel'
            STOP ' No Gaussian form factors in ECIS!'
         ENDIF
      ELSE
         WRITE (1,'(3f10.5)') 0., 0., 0.
      ENDIF
C-----write(1,'(3f10.5)') vvso,rrvso,aavso
      IF (VSO(Nejc,Nnuc).NE.0.) THEN
         IF(IDRs.gt.0) then
            WRITE (1,'(3f10.5)') VSO(Nejc,Nnuc)-DSOcor, 
     &                           RVSo(Nejc,Nnuc), AVSo(Nejc,Nnuc)
         ELSE
            WRITE (1,'(3f10.5)') VSO(Nejc,Nnuc), 
     &                           RVSo(Nejc,Nnuc), AVSo(Nejc,Nnuc)
         ENDIF 
      ELSE
         WRITE (1,'(3f10.5)') 0., 0., 0.
      ENDIF
C-----write(1,'(3f10.5)') wwso,rwso,awso
      IF (WSO(Nejc,Nnuc).NE.0.) THEN
         WRITE (1,'(3f10.5)') WSO(Nejc,Nnuc), RWSo(Nejc,Nnuc),
     &                        AWSo(Nejc,Nnuc)
      ELSE
         WRITE (1,'(3f10.5)') 0., 0., 0.
      ENDIF
      WRITE (1,'(3f10.5)') RCOul(Nejc,Nnuc), ACOul(Nejc,Nnuc), 0.

      WRITE (1,'(3f10.5)') 0., 0., 0.

      IF(IDRs.gt.0) then
C       Storing reference depths for the first level
        vvref  = VOM(Nejc,Nnuc) - DWVcor
        wvref  = WOMv(Nejc,Nnuc)
        wsref  = WOMs(Nejc,Nnuc)
        vsoref = VSO(Nejc,Nnuc) - DSOcor
        wsoref = WSO(Nejc,Nnuc)

        IF (ICAlangs.GT.0) THEN
          WRITE (1,'(3f10.5)') 0.D0, 180.0D0, 180.D0
          WRITE (1,'(2i5)') ICAlangs,0
          DO i = 1, ICAlangs
            WRITE(1,'(A1,I1,I3,2I5,5X,3F10.5)') 'F', 0, NANgela, i, 0,
     1                                    1.0D0, 1.0D0, 0.0D0
            DO iang = 1, NANgela
              WRITE(1,'(5F10.5)') ANGles(iang),1.0D3,1.0D2,0.0D0,0.0D0
            END DO
          END DO
        ELSE
          WRITE (1,'(3f10.5)') 0.D0, angstep, 180.D0
        ENDIF

        nn2 = 2
        if(ea.ge.1000.d0) nn2=0 ! there is non non-locality
        write(1,'(14I5)')  +1,  -nn2,  nnv,  nns,  -nnl,  1  

        fv  = 0.d0
        fs  = 0.d0
        fvv = 0.d0
        fvs = 0.d0
        tv  = 0.d0
        ts  = 0.d0
        fso = 0.d0
        tso = 0.d0

        if(vvref.ne.0.d0) 
     >    fvv = (VOM(Nejc,Nnuc) - DWVcor - vvref)/vvref
        if(wvref.ne.0.d0) then
          fv = DWVcor/wvref
          tv = (WOMv(Nejc,Nnuc) - wvref)/wvref
        endif
        if(wsref.ne.0.d0) then
          fs = DWScor/wsref
          ts = (WOMs(Nejc,Nnuc) - wsref)/wsref
        endif
        if(wsoref.ne.0.d0) then
          fso = DSOcor/wsoref
          tso = (WSO(Nejc,Nnuc) - wsoref )/wsoref
        endif

        if(vsoref.ne.0.d0) 
     >    fvs = (VSO(Nejc,Nnuc) - DSOcor - vsoref)/vsoref

        write (1,'(2(G10.4,F10.4,G10.4))')                                      
     >        tv ,  fv, fvv, ts, fs  , 0.d0
        write (1,'(2(G10.4,F10.4,G10.4))') 
     >        tso, fso, fvs
      endif 

C-----2) discrete levels
      DO j = 2, ND_nlv
C--------All levels with icollev(j)>LEVcc should be calculated by DWBA
         IF (Inlkey.EQ.0 .AND. ICOllev(j).GT.LEVcc) CYCLE
C--------All levels with icollev(j)<LEVcc should be calculated by CC
         IF (Inlkey.EQ.1 .AND. ICOllev(j).LT.LEVcc) CYCLE
C--------If channel is closed ground state potential is used for this level
C        eee = El   - D_Elv(j)/xratio
         eee = elab - D_Elv(j)/xratio

         IF(IDRs.gt.0) then
C-------------SETPOTS : subroutine for optical model parameters
C-------------From  cms system to Lab
C             ecms = eee
C             ikey = +1   ! WE NEED TO SET IKEY TO -1 AS ENERGIES ARE IN THE LAB FRAME
              elabe = eee     ! Changed on Aug. 2008
              ikey  = -1   
C
C-------------Transformation of energies from laboratory to center-of-mass if
C-------------needed is done inside SETPOTS() -> OMPAR()
C
              CALL SETPOTS(Nejc,Nnuc,elabe,ecms,xmas_nejc,xmas_nnuc,
     &                   ak2,ikey)

              fv  = 0.d0
              fs  = 0.d0
              fvv = 0.d0
              fvs = 0.d0
              tv  = 0.d0
              ts  = 0.d0
              fso = 0.d0
              tso = 0.d0

              if(vvref.ne.0.d0) 
     >          fvv = (VOM(Nejc,Nnuc) - DWVcor - vvref)/vvref
              if(wvref.ne.0.d0) then
                fv  = DWVcor/wvref
                tv  = (WOMv(Nejc,Nnuc) - wvref)/wvref
              endif
              if(wsref.ne.0.d0) then
                fs  = DWScor/wsref
                ts  = (WOMs(Nejc,Nnuc) - wsref)/wsref
              endif
              if(wsoref.ne.0.d0) then
                fso = DSOcor/wsoref
                tso = (WSO(Nejc,Nnuc)  - wsoref )/wsoref
              endif

              if(vsoref.ne.0.d0) 
     >          fvs = (VSO(Nejc,Nnuc) - DSOcor - vsoref)/vsoref

              write (1,'(2(G10.4,F10.4,G10.4))')                                       
     >          tv, fv  , fvv, 
     >          ts, fs  , 0.d0
              write (1,'(2(G10.4,F10.4,G10.4))') 
     >          tso, fso, fvs

              CYCLE
         ENDIF

         IF (eee.GT.0.0001) THEN
C-----------SETPOTS : subroutine for optical model parameters
C-----------From  cms system to Lab
C           ecms = eee
C           ikey = +1   ! WE NEED TO SET IKEY TO -1 AS ENERGIES ARE IN THE LAB FRAME
            elabe = eee ! Changed on Aug. 2008
            ikey  = -1   
C
C-----------Transformation of energies from laboratory to center-of-mass if
C-----------needed is done inside SETPOTS() -> OMPAR()
C
            CALL SETPOTS(Nejc,Nnuc,elabe,ecms,xmas_nejc,xmas_nnuc,
     &                   ak2,ikey)
C
C-----------potential parameters
C-----------write(1,'(3f10.5)') v,rv,av
            IF (VOM(Nejc,Nnuc).NE.0.) THEN
               WRITE (1,'(3f10.5)') VOM(Nejc,Nnuc),
     &                              RVOm(Nejc,Nnuc),
     &                              AVOm(Nejc,Nnuc)
            ELSE
               WRITE (1,'(3f10.5)') 0., 0., 0.
            ENDIF
C-----------write(1,'(3f10.5)') w,rw,aw
            IF (WOMv(Nejc,Nnuc).NE.0.) THEN
               WRITE (1,'(3f10.5)') WOMv(Nejc,Nnuc),
     &                              RWOmv(Nejc,Nnuc),
     &                              AWOmv(Nejc,Nnuc)
            ELSE
               WRITE (1,'(3f10.5)') 0., 0., 0.
            ENDIF
C-----------write(1,'(3f10.5)') vd,rvd,avd
            IF (VOMs(Nejc,Nnuc).NE.0.) THEN
               WRITE (1,'(3f10.5)') VOMs(Nejc,Nnuc),
     &                              RWOm(Nejc,Nnuc),
     &                              AWOm(Nejc,Nnuc)
            ELSE
               WRITE (1,'(3f10.5)') 0., 0., 0.
            ENDIF
C-----------write(1,'(3f10.5)') wd,rwd,awd
            IF (WOMs(Nejc,Nnuc).NE.0.) THEN
               WRITE (1,'(3f10.5)') WOMs(Nejc,Nnuc),
     &                              RWOm(Nejc,Nnuc),
     &                              AWOm(Nejc,Nnuc)
            ELSE
               WRITE (1,'(3f10.5)') 0., 0., 0.
            ENDIF
C-----------write(1,'(3f10.5)') vvso,rrvso,aavso
            IF (VSO(Nejc,Nnuc).NE.0.) THEN
               WRITE (1,'(3f10.5)') VSO(Nejc,Nnuc),
     &                              RVSo(Nejc,Nnuc),
     &                              AVSo(Nejc,Nnuc)
            ELSE
               WRITE (1,'(3f10.5)') 0., 0., 0.
            ENDIF
C-----------write(1,'(3f10.5)') wwso,rwso,awso
            IF (WSO(Nejc,Nnuc).NE.0.) THEN
               WRITE (1,'(3f10.5)') WSO(Nejc,Nnuc),
     &                              RWSo(Nejc,Nnuc),
     &                              AWSo(Nejc,Nnuc)
            ELSE
               WRITE (1,'(3f10.5)') 0., 0., 0.
            ENDIF
            WRITE (1,'(3f10.5)')
     &                RCOul(Nejc,Nnuc), ACOul(Nejc,Nnuc), 0.
            WRITE (1,'(3f10.5)') 0., 0., 0.
         ENDIF
      ENDDO
      IF (IDRs.eq.0) then
C
C-------Angular distribution step
C
        IF (ICAlangs.GT.0) THEN
          WRITE (1,'(3f10.5)') 0.D0, 180.0D0, 180.D0
          WRITE (1,'(2i5)') ICAlangs,0
          DO i = 1, ICAlangs
            WRITE(1,'(A1,I1,I3,2I5,5X,3F10.5)') 'F', 0, NANgela, i, 0,
     1                                    1.0D0, 1.0D0, 0.0D0
            DO iang = 1, NANgela
              WRITE(1,'(5F10.5)') ANGles(iang),1.0D3,1.0D2,0.0D0,0.0D0
            END DO
          END DO
        ELSE
          WRITE (1,'(3f10.5)') 0.D0, angstep, 180.D0
        ENDIF
      ENDIF
      WRITE (1,'(4hFIN )')
      CLOSE (UNIT = 1)

      IF (IOPsys.EQ.0) THEN
        ctmp = 'cp ecVIBROT.inp ecis06.inp'
        iwin = PIPE(ctmp)
      ELSE
        iwin = PIPE('copy ecVIBROT.inp ecis06.inp >NUL')
      ENDIF
C-----Running ECIS
      CALL ECIS('ecis06 ')
      IF (npho.GT.0) THEN
C        CALL ECIS('ecVIBROT.inp','ECIS_VIBROT.out')
         IF (IOPsys.EQ.0) THEN
            ctmp = 'mv ecis06.out ECIS_VIBROT.out'
            iwin = PIPE(ctmp)
            ctmp = 'rm ecis06.inp'
            iwin = PIPE(ctmp)
         ELSE
            iwin = PIPE('move ecis06.out ECIS_VIBROT.out >NUL')
            iwin = PIPE('del ecis06.inp >NUL')
         ENDIF
      ELSE
C        CALL ECIS('ecVIBROT.inp','ECIS_ROT.out')
         IF (IOPsys.EQ.0) THEN
            ctmp = 'mv ecis06.out ECIS_ROT.out'
            iwin = PIPE(ctmp)
         ELSE
            iwin = PIPE('move ecis06.out ECIS_ROT.out >NUL')
         ENDIF
      ENDIF
      RETURN
      END
c----
c     CCSOFTROTOR
c----

      SUBROUTINE SETPOTS(Nejc,Nnuc,Eilab,Eicms,Mi,Mt,Ak2,Ikey)
C
C     Transfers o.m. parameters to ECIS
C
C     Ener must be in LAB system
C
C     ener = Elab
C
      INCLUDE 'dimension.h'
      INCLUDE 'global.h'
C
C Dummy arguments
C
      DOUBLE PRECISION Ak2, Eicms, Eilab, Mi, Mt
      INTEGER Ikey, Nejc, Nnuc
C
C Local variables
C
      INTEGER komp
      IF (CCCalc) THEN
C-----calculate o.m. parameters for ejectile NEJC on target NNUC at energy ENER
         komp = 33
         CALL OMPAR(Nejc,Nnuc,Eilab,Eicms,Mi,Mt,Ak2,komp,Ikey)
      ELSE
C-----calculate o.m. parameters for ejectile NEJC on target NNUC at energy ENER
         komp = 29
         CALL OMPAR(Nejc,Nnuc,Eilab,Eicms,Mi,Mt,Ak2,komp,Ikey)
      ENDIF
      END

      SUBROUTINE KINEMA(El,E1,Mi,Mt,Ak2,Iopt,Relcal)
C
C     Author: O.Bersillon (SCAT2000)
C
C***********************************************************************
C  Kinematics:   lab  <===>  CM                                        *
C    With relativistic kinematics, the reduced mass is replaced by     *
C    the reduced total energy                                          *
C----------------------------------------------------------------------*
C  EL     = current lab kinetic energy                                 *
C  E1     = current  CM kinetic energy                                 *
C  MI     = incident particle rest mass (in a.m.u.)                    *
C  MT     = target   nucleus  rest mass (in a.m.u.)                    *
C  AK2    = CM wave number                                             *
C  IOPT   = 1   from lab to CM                                         *
C           2   from CM  to lab                                        *
C  Relcal = FALSE classical    kinematics                              *
C           TRUE  relativistic kinematics                              *
C----------------------------------------------------------------------*
C  AMUmev = a.m.u. in MeV                                              *
C----------------------------------------------------------------------*
C***********************************************************************
C
C COMMON variables
C
      DOUBLE PRECISION AMPi, AMUmev, AMUneu, AMUpro, CETa, CSO, ELE2,
     &                 HHBarc, PI, AMUele
      COMMON /CONSTANT/ AMUmev, PI, CETa, CSO, AMPi,
     &                  ELE2, HHBarc, AMUneu, AMUpro, AMUele
C
C Dummy arguments
C
      DOUBLE PRECISION Ak2, E1, El, Mi, Mt
      INTEGER Iopt
      LOGICAL Relcal
C
C Local variables
C
      DOUBLE PRECISION ck2, mtot, one, p2, two
      DOUBLE PRECISION DSQRT
      DATA one/1.0D+00/
      DATA two/2.0D+00/
      ck2 = (two*AMUmev)/(HHBarc**2)
      mtot = Mi + Mt
      IF (Iopt.EQ.1) THEN
C
C--------From lab to CM (the input quantity is Elab)
C
         IF (.NOT.Relcal) THEN
C
C-----------Classical    kinematics
C
            E1 = El*Mt/mtot
            Ak2 = ck2*Mi*Mt/mtot*E1
         ELSE
C
C-----------Relativistic kinematics
C
            E1 = AMUmev*mtot*
     &           (DSQRT(one + two*El/(AMUmev*Mt*((one+Mi/Mt)**2)))
     &           - one)
            p2 = (El*(El + two*AMUmev*Mi))
     &           /((one + Mi/Mt)**2 + two*El/(AMUmev*Mt))
            Ak2 = p2/(HHBarc*HHBarc)
C           etoti = DSQRT((AMUmev*Mi)**2 + p2)
C           etott = DSQRT((AMUmev*Mt)**2 + p2)
C           Amu = etoti*etott/(etoti + etott)
C           Amu = Amu/AMUmev
         ENDIF
C
C--------From  CM to lab (the input quantity is Ecm)
C
      ELSEIF (.NOT.Relcal) THEN
C
C--------Classical    kinematics
C
         El = E1*mtot/Mt
         Ak2 = ck2*Mi*Mt/mtot*E1
      ELSE
C
C--------Relativistic kinematics
C
         El = E1*(E1 + two*AMUmev*mtot)/(two*AMUmev*Mt)
         p2 = E1*(E1 + two*AMUmev*mtot)*(E1 + two*AMUmev*Mi)
     &        *(E1 + two*AMUmev*Mt)/((two*(E1+AMUmev*mtot))**2)
         Ak2 = p2/(HHBarc*HHBarc)
C        etoti = DSQRT((AMUmev*Mi)**2 + p2)
C        etott = DSQRT((AMUmev*Mt)**2 + p2)
C        Amu = etoti*etott/(etoti + etott)
C        Amu = Amu/AMUmev
C
C
      ENDIF
C
      END
C
C     From om-retrieve.f (RIPL-3 interface)
C     RCN,  Feb 28, 2005
C
      SUBROUTINE OPTMOD(El,Vlib,Rlib,Alib,Nejc,Nnuc,ndejc,ndnuc
     &,FNrvomp,FNavomp,FNvvomp,FNrwvomp,FNawvomp,FNwvomp
     &,FNwsomp,FNrsomp,FNasomp)
C
C     Routine to generate input for ECIS from RIPL-3 library
C
C----------------------------------------------------------------
C     Cut and paste place
C----------------------------------------------------------------
C
C     To use dispersive optical model package
C     Nonlocality constants alpha fixed according to Mahaux 1991
C
      INCLUDE "ripl2empire.h"
C
C COMMON variables
C
      REAL*8 EEE,Ef,Ep,Ea
      REAL*8 WDE,WVE
      REAL*8 As,Bs,Cs
      REAL*8 AAv,Bv 
      REAL*8 AAvso,Bvso
      INTEGER nns, iq, nnv, nnl
      REAL*8 VHFnum,DWVcor,VSOnum,DSOcor,ALhf,ALso,
     +               DWScor,ALpha,RSTep
      INTEGER IDRs 

      common /denergy/EEE,Ef,Ep,Ea          
      common /Wenerg/WDE,WVE
      common /pdatas/As,Bs,Cs,nns,iq
      common /pdatav/AAv,Bv,nnv
      common /pdatao/AAvso,Bvso,nnl
      common /disper/VHFnum,DWVcor,VSOnum,DSOcor,ALhf,ALso,
     +               DWScor,ALpha,RSTep,IDRs
C
C Dummy arguments
C
      REAL*8 El
      REAL*8 Alib(6), Rlib(6), Vlib(6)
C
C Local variables
C
      real*8 VCshift
      real*8 alpha_PB,beta_PB,gamma_PB,Vnonl,VVcoul,VScoul,VDcoul
      real*8 DWS,DWV,DWVnonl,DWVso,DerDWV,DerDWS,dtmp,dtmp1
      real*8 AHF,Visov,WVisov,WSisov,AlphaV

      integer n

      real*8 b(6,NDIM1,15), dwmin, dwplus, dwsm, dwsp, elf,
     &     encoul2, pi, t12der, t1der, t1m, t1p, t2der, t2m, t2p, vc

      REAL*8 DELTA_WD, DELTA_WV, DOM_INT, DOM_INT_T1, DOM_INT_T2,
     &       DOM_INT_WS, DOM_INT_WV, VHF, WDF, WVF

      INTEGER i, j, jab, jc, jp, nn
      INTEGER IABS, INT, MIN0, NINT

      integer Nejc, Nnuc,ii
      real*8 
     &FNrvomp(0:ndejc,0:ndnuc,0:ndim2),FNavomp(0:ndejc,0:ndnuc,0:ndim2),
     &FNvvomp(0:ndejc,0:ndnuc,0:ndim3),FNrwvomp(0:ndejc,0:ndnuc,0:ndim2)
     &,FNawvomp(0:ndejc,0:ndnuc,0:ndim2)
     &,FNwvomp(0:ndejc,0:ndnuc,0:ndim3),FNwsomp(0:ndejc,0:ndnuc,0:ndim3)
     &,FNrsomp(0:ndejc,0:ndnuc,0:ndim2),FNasomp(0:ndejc,0:ndnuc,0:ndim2)

      EXTERNAL DELTA_WD, DELTA_WV, WDF, WVF

C Variation of optical potential parameters
      do i=1,6
        jp=1
        jab=iabs(jrange(i))
        do j=1,jab
           if(el.gt.epot(i,j)) jp=j+1
        enddo
        j=min0(jp,jab)         
        if (i.eq.1) then ! Real volume
         do ii=1,ndim2
          rco(i,j,ii)=rco(i,j,ii)*FNrvomp(Nejc,Nnuc,ii)
          aco(i,j,ii)=aco(i,j,ii)*FNavomp(Nejc,Nnuc,ii)
c          write(111,*) FNrvomp(Nejc,Nnuc,ii),Nejc,Nnuc,ii
c          write(111,*) FNavomp(Nejc,Nnuc,ii),Nejc,Nnuc,ii
         enddo
         do ii=1,ndim3
          pot(i,j,ii)=pot(i,j,ii)*FNvvomp(Nejc,Nnuc,ii)
         enddo
        elseif (i.eq.2) then ! Imag. volume
         do ii=1,ndim2
          rco(i,j,ii)=rco(i,j,ii)*FNrwvomp(Nejc,Nnuc,i)
          aco(i,j,ii)=aco(i,j,ii)*FNawvomp(Nejc,Nnuc,i)
         enddo
         do ii=1,ndim3
          pot(i,j,ii)=pot(i,j,ii)*FNwvomp(Nejc,Nnuc,ii)
c          write(111,*) FNwvomp(Nejc,Nnuc,ii),Nejc,Nnuc,pot(i,j,ii),i
         enddo
        elseif (i.eq.4) then ! Imag. surface
         do ii=1,ndim2
          rco(i,j,ii)=rco(i,j,ii)*FNrsomp(Nejc,Nnuc,ii)
          aco(i,j,ii)=aco(i,j,ii)*FNasomp(Nejc,Nnuc,ii)
         enddo
         do ii=1,ndim3
          pot(i,j,ii)=pot(i,j,ii)*FNwsomp(Nejc,Nnuc,ii)
         enddo
        endif
      enddo
C
C-----Generate optical model parameters for ECIS
C
      nnv = 0
      nns = 0
      nnl = 0
      VHFnum = 0.d0
      VSOnum = 0.d0
      DWVcor = 0.d0
      DWScor = 0.d0
      DSOcor = 0.d0
      VDcoul = 0.d0
      AHF  = 0.d0
      ALhf = 0.d0
      ALso = 0.d0
      Alpha =  0.d0
      RSTep = 10.d0
c
c     Generate optical model parameters for ECIS
c
      pi=acos(-1.d0)
      rc=0.d0
      acou=0.d0

      IDRs = 0 ! Imaginary and HF geometry in dispersive potentials are the same

      if(jcoul.lt.1)go to 194
      jc=1
      do 190 j=1,jcoul
      if(el.gt.ecoul(j)) jc=j+1
 190  continue
      jc=min0(jc,jcoul)
      rc=rcoul0(jc)*atar**(-1./3.) + rcoul(jc) +
     +   rcoul1(jc)*atar**(-2./3.) + rcoul2(jc)*atar**(-5./3.) +
c        RCN addition to consider new Morillon-Romain potential 
     +   rcoul3(jc)
      acou=acoul(jc)
 194  encoul2=0.
      if(rc.gt.0.) encoul2=1.73*ztar/(rc*atar**(1./3.))
c
      do i=1,6
c
c     For Lane consistent potentials change the incident energy for proton potentials
c     by the specified Coulomb shift (pot(1,1,25)
c
      VCshift = 0.d0
      IF(izproj.eq.1 .and. pot(i,1,25).gt.0.d0) 
     >   VCshift = pot(i,1,25)*ztar/atar**(1.d0/3.d0)
      vc = 0.d0
      DerDWV = 0.d0
      DerDWS = 0.d0
      DWVnonl = 0.d0
      VVcoul = 0.d0
      VScoul = 0.d0
      AlphaV = 0.d0
      rlib(i)=0.d0
      alib(i)=0.d0
      vlib(i)=0.d0
      jab=iabs(jrange(i))
      if(jrange(i).lt.1)go to 300
      jp=1
      do 204 j=1,jab
      if(el.gt.epot(i,j)) jp=j+1
 204  continue
      j=min0(jp,jab)
      Ef=efermi
      if(pot(i,j,18).ne.0.) Ef=pot(i,j,18) + pot(i,j,19)*atar

      elf = el - Ef - VCshift
c
c     Calculate radius and diffuseness parameters
c$$$      if (i.eq.1) then ! real volume
c$$$       do ii=1,ndim2
c$$$          rco(i,j,ii)=rco(i,j,ii)*FNrvomp(Nejc,Nnuc,ii)
c$$$          aco(i,j,ii)=aco(i,j,ii)*FNavomp(Nejc,Nnuc,ii)
c$$$c          write(111,*) FNrvomp(Nejc,Nnuc,ii),Nejc,Nnuc,ii
c$$$c          write(111,*) FNavomp(Nejc,Nnuc,ii),Nejc,Nnuc,ii
c$$$       enddo
c$$$       do ii=1,ndim3
c$$$          pot(i,j,ii)=pot(i,j,ii)*FNvvomp(Nejc,Nnuc,ii)
c$$$       enddo
c$$$      elseif (i.eq.2) then ! imag. volume
c$$$       do ii=1,ndim2
c$$$          rco(i,j,ii)=rco(i,j,ii)*FNrwvomp(Nejc,Nnuc,ii)
c$$$       enddo
c$$$       do ii=1,ndim3
c$$$          pot(i,j,ii)=pot(i,j,ii)*FNwvomp(Nejc,Nnuc,ii)
c$$$          write(111,*) FNwvomp(Nejc,Nnuc,ii),Nejc,Nnuc,pot(i,j,ii),i
c$$$       enddo
c$$$      elseif (i.eq.4) then ! imag. surface
c$$$       do ii=1,ndim2
c$$$          rco(i,j,ii)=rco(i,j,ii)*FNrsomp(Nejc,Nnuc,ii)
c$$$          aco(i,j,ii)=aco(i,j,ii)*FNasomp(Nejc,Nnuc,ii)
c$$$       enddo
c$$$       do ii=1,ndim3
c$$$          pot(i,j,ii)=pot(i,j,ii)*FNwsomp(Nejc,Nnuc,ii)
c$$$       enddo
c$$$      endif
      if(rco(i,j,13).eq.0.) then
        rlib(i)=abs(rco(i,j,1)) + rco(i,j,3)*eta
     *       + rco(i,j,4)/atar + rco(i,j,5)/sqrt(atar)
     *       + rco(i,j,6)*atar**(2./3.) + rco(i,j,7)*atar
     *       + rco(i,j,8)*atar**2  + rco(i,j,9)*atar**3
     *       + rco(i,j,10)*atar**(1./3.)
     *       + rco(i,j,11)*atar**(-1./3.)
C--------------------------------------------------------------------
C     RCN, 08/2004, to handle new extension to the OMP RIPL-2 format
     *       + rco(i,j,2)*el + rco(i,j,12)*el*el
      else
C     RCN, 09/2004, to handle new extension to the OMP RIPL-2 format
        nn = int(rco(i,j,7))
        rlib(i)= ( abs(rco(i,j,1)) + rco(i,j,2)*atar ) *
     *           ( 1.d0 - ( rco(i,j,3) + rco(i,j,4)*atar ) * elf**nn/
     *           ( elf**nn + ( rco(i,j,5) + rco(i,j,6)*atar )**nn ) )
      endif

      alib(i)=abs(aco(i,j,1)) + aco(i,j,2)*el + aco(i,j,3)*eta
     *        + aco(i,j,4)/atar + aco(i,j,5)/sqrt(atar)
     *        + aco(i,j,6)*atar**(2./3.) + aco(i,j,7)*atar
     *        + aco(i,j,8)*atar**2 + aco(i,j,9)*atar**3
     *        + aco(i,j,10)*atar**(1./3.) + aco(i,j,11)*atar**(-1./3.)
c
      IF (i.eq.2 .and. idr.ge.2) THEN
        IF( ABS(rlib(1)-rlib(2)).gt.0.001 ) IDRs = 1
        IF( ABS(alib(1)-alib(2)).gt.0.001 ) IDRs = 1
      ENDIF

      if(alib(i).gt.0.d0) RStep = min( 0.25d0*alib(i), RStep)
c
      if (pot(i,j,24).eq.0.) go to 210
c
c     Special Koning-type potential formulas
c
      if (pot(i,j,24).eq.1. .or. pot(i,j,24).eq.3.) then
c
c       Koning-type formulas
c
        elf = el - Ef - VCshift
        if(i.eq.1) call bcoget(b,j,Visov,WVisov,WSisov)

        if(i.eq.1 .and. b(1,j,5).ne.0.d0) then
          vc = b(1,j,1)*encoul2*( b(1,j,2) - 2.*b(1,j,3)*elf +
     +    3.*b(1,j,4)*elf**2 + b(i,j,14)*b(i,j,13)*exp(-b(i,j,14)*elf) )
          VDcoul = b(i,j,5)*vc
        endif

        nn = int(pot(i,j,13))
c       Retrieving average energy of the particle states Ep
        Ep=Ef
        if( (i.eq.2) .or. (i.eq.4) ) Ep=pot(i,j,20)
        if(Ep.eq.0.) Ep=Ef
        elf = el - Ep - VCshift

        iq=1
        if(i.eq.4 .and. b(4,j,12).gt.0.) iq=nint(b(4,j,12))
        if(i.eq.1) then
          if(b(i,j,1).ne.0.d0) then
            ALhf = b(i,j,14)
            AHF  = b(i,j,1)*b(i,j,13)
          else
            ALhf = b(i,j,12)
            AHF  = b(i,j,11)
          endif
        endif
        if(i.eq.5) ALso = b(i,j,12)

        vlib(i)=
     +    b(i,j,1)*( b(i,j,15) - b(i,j,2)*elf + b(i,j,3)*elf**2 -
     +    b(i,j,4)*elf**3 + b(i,j,13)*exp(-b(i,j,14)*elf) ) +
     +    b(i,j,5)*vc + b(i,j,6)*(elf**nn/(elf**nn + b(i,j,7)**nn)) +
     +    b(i,j,8)*exp(-b(i,j,9)*elf**iq)*(elf**nn/
     +    (elf**nn + b(i,j,10)**nn)) +
     +    b(i,j,11)*exp(-b(i,j,12)*elf)
      endif

      if (pot(i,j,24).eq.2.) then
c
c       Morillon-Romain formulas
c
        elf = el - Ef - VCshift
        if(i.eq.1) call bcoget(b,j,Visov,WVisov,WSisov)
c       
c       Vhf(E) calculated from nonlocal approximation
c          as suggested by Perey and Buck
        alpha_PB = b(i,j,1)
        beta_PB  = b(i,j,2)
        gamma_PB = b(i,j,3)
        EEE = el  - VCshift  
        iq=1
        if(i.eq.4 .and. b(4,j,12).gt.0.) iq=nint(b(4,j,12))
 
        Vnonl = 0.d0
        if(i.eq.1 .or. i.eq.5) then
          Vnonl = -Vhf(EEE,alpha_PB,beta_PB,gamma_PB)
          vc = 0.d0
          if(i.eq.1 .and. b(1,j,5).ne.0.d0) then
C           MR do not use derivarive of the potential 
C
C           Numerical derivative of the Vhf
C           Vnonlm = -Vhf(EEE-0.05,alpha_PB,beta_PB,gamma_PB)
C           Vnonlp = -Vhf(EEE+0.05,alpha_PB,beta_PB,gamma_PB)
C           Coulomb correction for Hartree-Fock potential
C           vc = encoul2*(Vnonlm-Vnonlp)*10.d0
C
C           MR are using constant Coulomb correction
            vc = encoul2
            VDcoul = b(i,j,5)*vc
          endif
        endif
        nn = int(pot(i,j,13))

        vlib(i)=
     +    Vnonl + b(i,j,5)*vc +
     +    b(i,j,6)*(elf**nn/(elf**nn + b(i,j,7)**nn)) +
     +    b(i,j,8)*exp(-b(i,j,9)*elf**iq)*(elf**nn/
     +    (elf**nn + b(i,j,10)**nn)) +
     +    b(i,j,11)*exp(-b(i,j,12)*elf)
      endif
c
c     Nonlocality consideration
c
c     Retrieving energy above which nonlocality in the volume absorptive
c               potential is considered (Ea)
c
      Ea=pot(i,j,21)
      if(Ea.eq.0.) Ea=1000.1d0
      if(i.eq.2 .and. Ea.lt.1000.d0 ) then
        AlphaV=pot(i,j,22)
        if(AlphaV.eq.0.d0) AlphaV=1.65d0
        if(el.gt.(Ef+Ea))  vlib(i) = vlib(i) +
     +   AlphaV*(sqrt(el)+(Ef+Ea)**1.5d0/(2.d0*el)-1.5d0*sqrt(Ef+Ea))
      endif

      go to 300
c
 210  if (pot(i,j,23).eq.0.) go to 220
c
c     Special Varner-type potential formulas
c
      elf = el - VCshift
c
      vlib(i)= (pot(i,j,1) + pot(i,j,2)*eta)/
     +    (1.+ exp((pot(i,j,3) - elf + pot(i,j,4)*encoul2)/pot(i,j,5)))
      if(pot(i,j,6).eq.0.)go to 300
      vlib(i) = vlib(i)
     +    + pot(i,j,6)*exp((pot(i,j,7)*elf - pot(i,j,8))/pot(i,j,6))
      go to 300
c
 220  if (pot(i,j,22).eq.0.) go to 230
c
c     Special Smith-type potential formulas
c
      elf = el - VCshift
c
      vlib(i)=pot(i,j,1) + pot(i,j,2)*eta
     +    + pot(i,j,6)*exp(pot(i,j,7)*elf + pot(i,j,8)*elf*elf)
     +    + pot(i,j,9)*elf*exp(pot(i,j,10)*elf**pot(i,j,11))
      if(pot(i,j,5).ne.0.)vlib(i)=vlib(i)
     +    + pot(i,j,3)*cos(2.*pi*(atar - pot(i,j,4))/pot(i,j,5))
      go to 300
c
c     Standard potential formulas
c
 230  elf = el - VCshift
      vlib(i)=pot(i,j,1) + pot(i,j,7)*eta + pot(i,j,8)*encoul
     +   + pot(i,j,9)*atar + pot(i,j,10)*atar**(1./3.)
     +   + pot(i,j,11)*atar**(-2./3.) + pot(i,j,12)*encoul2
      if(elf.gt.0.) vlib(i) = vlib(i) + 
     +      (pot(i,j,2) + pot(i,j,13)*eta + pot(i,j,14)*atar)*elf
     +   + pot(i,j,3)*elf**2 + pot(i,j,4)*elf**3 + pot(i,j,6)*sqrt(elf)
     +   + pot(i,j,17)*encoul/elf**2
     +   + (pot(i,j,5) + pot(i,j,15)*eta + pot(i,j,16)*elf)*log(elf)


 300  if(i.eq.1) VHFnum = vlib(1)
      if(i.eq.2) ALPha  = AlphaV
      if(i.eq.5) VSOnum = vlib(5)
      ENDDO
c
c     To calculate dispersion relation contribution
c
 152  if(abs(idr).ge.2) then
c
c       Exact calculation of the dispersive contribution
c
        EEE = el - VCshift
        i=2
c       Only one energy range
        j=1
c       Real volume contribution from Dispersive relation
        DWV=0.d0
c
        if(jrange(i).gt.0 .and. pot(2,1,24).ne.0) then
          AAv=b(i,j,6)
          Bv =b(i,j,7)
          n = nint( pot(i,j,13) )
          nnv = n
          if(n.eq.0 .or. mod(n,2).eq.1)
     +      stop 'Zero or odd exponent in Wv(E) for dispersive OMP'
          Ep=pot(i,j,20)
          if(Ep.eq.0.) Ep=Ef

          Ea=pot(i,j,21)
          if(Ea.eq.0.) Ea=1000.1d0

c         if(modtyp.eq.6) goto 154  ! modtyp = 5 for dispersive potentials

c         Analytical DOM integral
          DWV=DOM_INT_Wv(Ef,Ep,AAv,Bv,EEE,n,DerDWV)

C         Numerical DOM integral (used in RIPL-2 released interface)  
C         WVE=WVf(AAv,Bv,Ep,Ef,EEE,n)
C         ftmp1=2*DOM_int(Delta_WV,WVf,Ef,Ef+5.*Bv,150000.d0,EEE,0.d0)

C         Coulomb correction for real volume potential 
          if (pot(1,1,25).ne.0) DerDWV = 0.d0
          DerDWV = -b(1,1,5)*encoul2*DerDWV

C         numerical DOM derivative (not needed for a time being) 
C         DWVp = DOM_INT_Wv(Ef,Ep,AAv,Bv,EEE+0.1d0,n,dtmp)
C         DWVm = DOM_INT_Wv(Ef,Ep,AAv,Bv,EEE-0.1d0,n,dtmp)
C         DerDWV = (DWVp-DWVm)*5.d0
C         if (pot(1,1,25).ne.0) DerDWV = 0.d0
C         DerDWV = -b(1,1,5)*encoul2*DerDWV
c         if(idr.le.-2) then
c           numerical DOM integral (not needed for a time being)
c           WVE=WVf(AAv,Bv,Ep,Ef,EEE,n)
c           DWV=2*DOM_int(Delta_WV,WVf,Ef,Ef+5.*Bv,150000.d0,EEE,0.d0)
c         endif
c
          T12der = 0.d0  
          if(Ea.lt.1000.d0) THEN
             AlphaV=pot(i,j,22)
             if(AlphaV.eq.0.d0) AlphaV=1.65d0

             Dwplus = AlphaV*DOM_INT_T2(Ef,Ea,EEE)
             dtmp1 = Wvf(AAv,Bv,Ep,Ef,Ef+Ea,n)
             Dwmin = dtmp1*DOM_INT_T1(Ef,Ea,EEE)

             DWV = DWV + Dwplus + Dwmin
             DWVnonl = Dwplus + Dwmin
c            Coulomb correction for nonlocal dispersive contribution  
c                to real volume potential 
             if(b(1,1,5).ne.0.d0 .and. pot(1,1,25).eq.0) then
               if(eee.ne.0.05d0) then
                 T2p = DOM_INT_T2(Ef,Ea,EEE+0.05d0)  
                 T2m = DOM_INT_T2(Ef,Ea,EEE-0.05d0)  
                     T2der = AlphaV*(T2p-T2m)*10.d0 
                 T1p = DOM_INT_T1(Ef,Ea,EEE+0.05d0)
                 T1m = DOM_INT_T1(Ef,Ea,EEE-0.05d0)
                 T1der = dtmp1*(T1p-T1m)*10.d0
                 T12der =  -b(1,1,5)*encoul2* ( T1der + T2der )
               else
                 T2p = DOM_INT_T2(Ef,Ea,EEE+0.1d0)  
                 T2m = DOM_INT_T2(Ef,Ea,EEE-0.1d0)
                     T2der = AlphaV*(T2p-T2m)*5.d0 
                 T1p = DOM_INT_T1(Ef,Ea,EEE+0.1d0)
                 T1m = DOM_INT_T1(Ef,Ea,EEE-0.1d0)
                 T1der = dtmp1*(T1p-T1m)*5.d0
                 T12der =  -b(1,1,5)*encoul2* ( T1der + T2der )
               endif 
             endif
          endif
          VVcoul = DerDWV + T12der

        endif

154     i=4
c       Only one energy range
        j=1
c       Real surface contribution from Dispersive relation
        DWS=0.d0
	  DerDWS=0.d0
        if(jrange(i).gt.0 .and. pot(4,1,24).ne.0) then
          As=b(i,j,8)
          Bs=b(i,j,10)
          Cs=b(i,j,9)
          n = nint( pot(i,j,13) )
          nns = n
          if(n.eq.0 .or. mod(n,2).eq.1)
     +      stop 'Zero or odd exponent in Wd(E) for dispersive OMP'
          iq=1
          if(b(4,j,12).gt.0.) iq=nint(b(4,j,12))

c         Retrieving average energy of the particle states Ep
          Ep=pot(i,j,20)
          if(Ep.eq.0.) Ep=Ef

c         if(modtyp.eq.6) goto 156   ! modtyp = 5 for dispersive potentials

          if(idr.ge.2) then
c           analytical DOM integral
            DWS = DOM_INT_Ws(Ef,Ep,As,Bs,Cs,EEE,n,DerDWS)
c           Coulomb correction for real surface potential 
            if (pot(1,1,25).ne.0) DerDWS = 0.d0
            VScoul = -b(1,1,5)*encoul2*DerDWS
          endif

          if(idr.le.-2) then
c           numerical DOM integral
            nns=n
            WDE=WDf(As,Bs,Cs,Ep,EEE,n,iq)
            DWS = 2*DOM_int(Delta_WD,WDf,Ef,Ef+30.d0,2000.d0,EEE,WDE)
c           Coulomb correction for real surface potential 
            if(b(1,1,5).ne.0.d0) then
              WDE=WDf(As,Bs,Cs,Ep,EEE+0.1d0,n,iq)
              DWSp = 
     >         2*DOM_int(Delta_WD,WDf,Ef,Ef+30.d0,2000.d0,EEE+0.1d0,WDE)
              WDE=WDf(As,Bs,Cs,Ep,EEE-0.1d0,n,iq)
              DWSm =
     >         2*DOM_int(Delta_WD,WDf,Ef,Ef+30.d0,2000.d0,EEE-0.1d0,WDE)
c             Numerical derivative
              DerDWS = (DWSp-DWSm)*5.d0
              if (pot(1,1,25).ne.0) DerDWS = 0.d0
              VScoul = -b(1,1,5)*encoul2*DerDWS
            endif
          endif

        endif

156     i=6
c       Only one energy range
        j=1
c       Real spin orbit contribution from Dispersive relation
        DWVso=0.d0
c
        if(jrange(i).gt.0.and.pot(6,1,24).ne.0.and.abs(idr).eq.3) then
          AAvso = b(i,j,6)
          Bvso  = b(i,j,7)
          nnl = nint( pot(i,j,13) )
          if(nnl.eq.0 .or. mod(nnl,2).eq.1)
     +      stop 'Zero or odd exponent in Wso(E) for dispersive OMP'

c         analytical DOM integral
          DWVso=DOM_INT_Wv(Ef,Ef,AAvso,Bvso,EEE,nnl,dtmp)

        endif

C-------Adding real volume dispersive contribution to the real potential
C-------Geometry parameters are the same as for the volume potential(imag and real).
        if(idr.ne.0) then
          if (IDRs.gt.0) DWVcor = DWV + VVcoul
          DWScor = DWS + VScoul
          DSOcor = DWVso
        endif
C       
        vlib(1)= vlib(1) + DWVcor

c       Including real surface and Coulomb dispersive contribution
c       Geometry parameters are the same as for the imaginary surface potential.
        vlib(3)= DWScor
        alib(3)= alib(4)
        rlib(3)= rlib(4)
c       Adding real spin orbit dispersive contribution to the real spin orbit potential
c       Geometry parameters are the same as for the imaginary spin orbit potential(imag and real)
        vlib(5) = vlib(5) + DSOcor

      endif

      RETURN
      END

      SUBROUTINE SETR(A,B,N)
C
C     ******************************************************************
C     set all elements of array b(n) to real number a.
C     ******************************************************************
C
C Dummy arguments
C
      REAL*8 A
      INTEGER N
      REAL*8 B(N)
C
C Local variables
C
      INTEGER k
      DO k = 1, N
         B(k) = A
      ENDDO
      END

c-----------------------------------------------------------------------------
      subroutine bcoget(b,j,Visov,WVisov,WSisov)
c-----------------------------------------------------------------------------
c
c     Routine to compute b coefficients for Koning global potential
c
c     Modified by R.Capote to extend RIPL format
c
      INCLUDE "ripl2empire.h"
C
C Dummy arguments
C
      INTEGER J

      real*8 b(6,ndim1,15),Visov,WVisov,WSisov
c
       Visov  = 0.d0
       WVisov = 0.d0
       WSisov = 0.d0

       call setr(0.d0,b,90*ndim1)

       if(pot(1,j,24).eq.1) then
c        Original Koning dependence
         b(1,j,1)  =  pot(1,j,1) + pot(1,j,2)*atar + pot(1,j,8)*eta
c        Visov = 2*pot(1,j,8)*sqrt(eta/atar)
         Visov =   pot(1,j,8)

c        Soukhovitski dependence
         if((pot(1,j,20).ne.0.) .and.
     +    (pot(1,j,14) + pot(1,j,15)*atar + pot(1,j,16)).ne.0. ) then
           b(1,j,1)  =  pot(1,j,1) + pot(1,j,2)*atar + pot(1,j,8)*eta +
     +                pot(1,j,20)*eta/
     +               (pot(1,j,14) + pot(1,j,15)*atar + pot(1,j,16))
c          Visov = 2 * ( pot(1,j,8) + pot(1,j,20) ) * sqrt(eta/atar) 
           Visov =     ( pot(1,j,8) + pot(1,j,20) )
         endif
       
         b(1,j,2)  =  pot(1,j,3) + pot(1,j,4)*atar
         b(1,j,3)  =  pot(1,j,5) + pot(1,j,6)*atar
         b(1,j,4)  =  pot(1,j,7)
         b(1,j,5)  =  pot(1,j,9)
         b(1,j,11) =  pot(1,j,10) + pot(1,j,11)*atar
         b(1,j,12) =  pot(1,j,12)

c        b coefficients from 13 to 15 added for Soukhovitski potential
c        V^DISP_R
         b(1,j,13)  =  pot(1,j,16)
c        Lambda_R
         b(1,j,14)  =  pot(1,j,17)
c        V^0_R + V^A_R*(A-232)
         b(1,j,15)  =  pot(1,j,14) + pot(1,j,15)*atar
c        To preserve compatibility with RIPL-2 Koning database
c        b(i,j,15) must be equal to 1. !!! for Koning OMP
         if((pot(1,j,14) + pot(1,j,15)*atar + pot(1,j,16)).eq.0.)
     >          b(1,j,15) = 1.d0
       endif

       if(pot(1,j,24).eq.3) then
c
c        Li & Cai OMP 
c
c                         V0          V2                V1 
         b(1,j,1)  =  pot(1,j,1) + pot(1,j,2)*atar + pot(1,j,8)*eta
c        Visov = 2*pot(1,j,8)*sqrt(eta/atar)
         Visov =   pot(1,j,8)

         b(1,j,15) = 1.d0         

           b(1,j,5)  =  0.d0   ! No Coulomb correction

         if(b(1,j,1).ne.0.d0) 
c                         beta0         beta1          
     +     b(1,j,2)  = -(pot(1,j,3) + pot(1,j,4)*eta)/b(1,j,1)
       endif

c      Wv( Av )
C      b(2,j,6)  =  pot(2,j,1) + pot(2,j,2)*atar
c      added eta dependence for Av parameter (RCN, 12/2006) if pot(2,j,8)<>0
       b(2,j,6)  =  pot(2,j,1) + pot(2,j,2)*atar + pot(2,j,8)*eta 

c      Wv( Bv )
c      added eta dependence for Av parameter (RCN, 12/2006) if pot(2,j,8)<>0
       b(2,j,7)  =  pot(2,j,3) + pot(2,j,4)*atar + pot(2,j,8)*eta 

C      WVisov = 2*pot(2,j,8)*sqrt(eta/atar)    
       WVisov =   pot(2,j,8)

c      Wd
c      b(4,j,8)  =  pot(4,j,1) + pot(4,j,8)*eta
c      added A dependence for As parameter (RCN, 09/2004) if pot(4,j,7)<>0
C      b(4,j,8)  =  pot(4,j,1) + pot(4,j,8)*eta + pot(4,j,7)*atar
c      added A**(-1/3) dependence for As parameter (RCN, 11/2005) if pot(4,j,9)<>0
c      Wd( As )
       b(4,j,8)  =  pot(4,j,1) + pot(4,j,8)*eta + pot(4,j,7)*atar + 
     >                pot(4,j,9)*atar**(-1.d0/3.d0)

C      WSisov = 2*pot(4,j,8)*sqrt(eta/atar) 
       WSisov =   pot(4,j,8)

c      Wd( Cs )
       if(pot(4,j,3).ne.0.) then
c        added A dependence for Cs parameter (RCN, 05/2008) if pot(4,j,10)<>0
         b(4,j,9)  =  pot(4,j,2) + pot(4,j,10)*atar +
     +                pot(4,j,3)/(1.+ exp((atar-pot(4,j,4))/pot(4,j,5)))
       else
         b(4,j,9)  =  pot(4,j,2) + pot(4,j,10)*atar 
       endif
c      Wd( Bs )
c      added A dependence for Bs parameter (RCN, 05/2008) if pot(4,j,10)<>0
       b(4,j,10) =  pot(4,j,6) + pot(4,j,11)*atar 
C      Wd( q )
       b(4,j,12) =  pot(4,j,12)

c      Vso
       b(5,j,11) =  pot(5,j,10) + pot(5,j,11)*atar
       b(5,j,12) =  pot(5,j,12)
c      Wso
       b(6,j,6)  =  pot(6,j,1)
       b(6,j,7)  =  pot(6,j,3)

      return
      end subroutine bcoget

      REAL*8 FUNCTION VHF(Einp,Alpha_pb,Beta_pb,Gamma_pb)
C
C     According to Morillon B, Romain P, PRC70(2004)014601
C
C     Originally coded in c++ by Morillon B. and Romain P.
C
C     Coded in FORTRAN and tested by RCN, August 2004.
C
      INCLUDE "ripl2empire.h"
C
C Dummy arguments
C
      REAL*8 Alpha_pb, Beta_pb, Einp, Gamma_pb
C
C Local variables
C
      REAL*8 amu, coef1, coef2, etmp, miu_sur_hbar2, vtmp
      DOUBLE PRECISION DEXP
      INTEGER niter
      REAL*8 XKINE, xtmp
C-----getting amu
      xtmp=xkine(einp,amu)
      miu_sur_hbar2 = amu/HBArc**2
      coef1 = -0.5D0*Beta_pb**2*miu_sur_hbar2
      coef2 = 4.0D0*(Gamma_pb*miu_sur_hbar2)**2
      niter = 0.
      VHF = -45.D0
  100 niter = niter + 1
      vtmp = VHF
      etmp = Einp - vtmp
      VHF = Alpha_pb*DEXP(coef1*etmp + coef2*etmp**2)
      IF (ABS(VHF - vtmp).GT.0.0001 .AND. niter.LT.10000) GOTO 100
      END

      REAL*8 FUNCTION XKINE(Ei,Amu)
C
C***********************************************************************
C     From lab to CM (the input quantity is el = Elab)
C***********************************************************************
C     RCN 08/2004, xkine calculated by relativistic kinematics when needed
      INCLUDE "ripl2empire.h"
C
C Dummy arguments
C
      REAL*8 Amu, Ei
C
C Local variables
C
      DOUBLE PRECISION DSQRT
      REAL etoti, etott, p2
      INTEGER mtot
      mtot = (TARmas + PROjmas)
      IF (IREl.EQ.0) THEN
C--------Classical    kinematics (energy independent amu and xkine)
         Amu = PROjmas*TARmas/mtot*AMU0c2
         XKINE = TARmas/mtot
      ELSE
C--------Relativistic kinematics
         p2 = (Ei*(Ei + 2.D0*AMU0c2*PROjmas))
     &        /((1.D0 + PROjmas/TARmas)**2 + 2.D0*Ei/(AMU0c2*TARmas))
         etoti = DSQRT((AMU0c2*PROjmas)**2 + p2)
         etott = DSQRT((AMU0c2*TARmas)**2 + p2)
         Amu = etoti*etott/(etoti + etott)
         XKINE = etott/(etoti + etott)
      ENDIF
      END

      REAL*8 FUNCTION DOM_INT_WV(Ef,Ep,Av,Bv,E,N,Derivintwv)
C
C     AUTHOR: Dr. Roberto Capote Noy
C
C     e-mail: r.capotenoy@iaea.org; rcapotenoy@yahoo.com
C
C     DISPERSIVE OPTICAL MODEL POTENTIAL PACKAGE
C
C     Analytical and numerical dispersive integrals are calculated
C     please cite as:
C     Quesada JM et al,, Phys. Rev. C67(2003) 067601
C     Capote R et al, J. Phys. G27(2001) B15
C
C
C      Analytical dispersive integral and its derivative for
C      Wv(E)=Av*(E-Ep)**n/( (E-Ep)**n + Bv**n )  for E>Ep
C      Wv(E)=Wv(2*Ef-E)                          for E<2Ef-Ep
C      Wv(E)=0                                     OTHERWISE
C
      IMPLICIT NONE
C
C Dummy arguments
C
      REAL*8 Av, Bv, Derivintwv, E, Ef, Ep
      INTEGER N
C
C Local variables
C
      DOUBLE PRECISION DABS, DBLE
      REAL*8 deremin, dereplus, e0, emin, eplus, ex, pi, rds, resemin,
     &       reseplus, rs
      COMPLEX*8 ds, fs
      COMPLEX*16 i, pj, zj, ztmp
      INTEGER j
      REAL REAL
      DATA i/(0.D0,1.D0)/
      pi = 4.D0*ATAN(1.D0)
      e0 = Ep - Ef
      ex = E - Ef
      eplus = ex + e0
      emin = ex - e0
      DOM_INT_WV = 0.D0
      Derivintwv = 0.D0
      resemin = emin**N/(emin**N + Bv**N)
      deremin = emin**(N - 1)
     &          *(emin**N + Bv**N*(1.D0 + N*LOG(DABS(emin))))
     &          /(emin**N + Bv**N)**2
      reseplus = -eplus**N/(eplus**N + Bv**N)
      dereplus = -eplus**(N - 1)
     &           *(eplus**N + Bv**N*(1.D0 + N*LOG(eplus)))
     &           /(eplus**N + Bv**N)**2
C----------------------------------
C-----Complex arithmetic follows
      fs = (0.D0,0.D0)
      ds = (0.D0,0.D0)
      DO j = 1, N
         ztmp = i*(2*j - 1)/DBLE(N)*pi
         pj = Bv*EXP(ztmp)
         zj = pj*(2*pj + eplus - emin)*ex
         zj = zj/((pj + e0)*(pj + eplus)*(pj - emin))
         fs = fs + zj*LOG( - pj)
         ds = ds + 2*pj*(ex*ex + (pj + e0)**2)*LOG( - pj)
     &        /((pj + eplus)**2*(pj - emin)**2)
      ENDDO
      IF (ABS(IMAG(fs)).GT.1.E-4) STOP '(F) Too big imag part in Wv'
      rs = REAL(fs)
      IF (ABS(IMAG(ds)).GT.1.E-4) STOP '(D) Too big imag part in Wv'
      rds = REAL(ds)
C----------------------------------
      DOM_INT_WV = -Av/pi*(rs/N + reseplus*LOG(eplus)
     &             + resemin*LOG(DABS(emin)))
      Derivintwv = -Av/pi*(rds/N + dereplus + deremin)
      END

      REAL*8 FUNCTION DOM_INT_WS(Ef,Ep,As,Bs,Cs,E,M,Derivintws)
C
C      Analytical dispersive integral and its derivative for
C      Ws(E)=As*(E-Ep)**m/( (E-Ep)**m + Bs**m ) * exp(-Cs*(E-Ep)) for E>Ep
C      Ws(E)=Ws(2*Ef-E)                                           for E<2Ef-Ep
C      Ws(E)=0                                                    OTHERWISE
C
      IMPLICIT NONE
C
C Dummy arguments
C
      REAL*8 As, Bs, Cs, Derivintws, E, Ef, Ep
      INTEGER M
C
C Local variables
C
      DOUBLE PRECISION DBLE
      REAL*8 deremin, dereplus, e0, emin, eplus, ex, pi, rds, resemin,
     &       reseplus, rs
      COMPLEX*8 ds, fs
      REAL*8 EIN
      COMPLEX*16 i, pj, zj, ztmp
      INTEGER j
      REAL REAL
      COMPLEX*16 ZFI
      DATA i/(0.D0,1.D0)/
      pi = 4.D0*ATAN(1.D0)
      e0 = Ep - Ef
      ex = E - Ef
      eplus = ex + e0
      emin = ex - e0
      DOM_INT_WS = 0.D0
      Derivintws = 0.D0
      resemin = emin**M/(emin**M + Bs**M)
      deremin = -emin**(M - 1)
     &          *(emin**M + Bs**M + ( - Cs*emin**(M+1) + Bs**M*
     &          (-Cs*emin+M))*EXP( - Cs*emin)*EIN(Cs*emin))
     &          /(emin**M + Bs**M)**2
      reseplus = -eplus**M/(eplus**M + Bs**M)
      dereplus = eplus**(M - 1)
     &           *(eplus**M + Bs**M + (Cs*eplus**(M+1) + Bs**M*
     &           (Cs*eplus+M))*EXP(Cs*eplus)*EIN( - Cs*eplus))
     &           /(eplus**M + Bs**M)**2
C----------------------------------
C     Complex arithmetic follows
      fs = (0.D0,0.D0)
      ds = (0.D0,0.D0)
      DO j = 1, M
         ztmp = i*(2*j - 1)/DBLE(M)*pi
         pj = Bs*EXP(ztmp)
         zj = pj*(2*pj + eplus - emin)*ex
         zj = zj/(pj + e0)/(pj + eplus)/(pj - emin)
         fs = fs + zj*ZFI( - pj*Cs)
         ds = ds + 2*pj*(ex*ex + (pj + e0)**2)*ZFI( - pj*Cs)
     &        /((pj + eplus)**2*(pj - emin)**2)
      ENDDO
      IF (ABS(IMAG(fs)).GT.1.E-4) STOP '(F) Too big imag part in Ws'
      rs = REAL(fs)
      IF (ABS(IMAG(ds)).GT.1.E-4) STOP '(D) Too big imag part in Ws'
      rds = REAL(ds)
C----------------------------------
      DOM_INT_WS = As/pi*(rs/M - reseplus*EXP(Cs*eplus)*EIN( - Cs*eplus)
     &             - resemin*EXP( - Cs*emin)*EIN(Cs*emin))
      Derivintws = As/pi*(rds/M + dereplus + deremin)
      END


      REAL*8 FUNCTION WV(A,B,Ep,Ef,E,N)
      IMPLICIT NONE
C
C Dummy arguments
C
      REAL*8 A, B, E, Ef, Ep
      INTEGER N
C
C Local variables
C
      REAL*8 ee
      WV = 0.D0
      IF (E.LE.Ef) E = 2.D0*Ef - E
      IF (E.LT.Ep) RETURN
      ee = (E - Ep)**N
      WV = A*ee/(ee + B**N)
      END


      REAL*8 FUNCTION WDD(A,B,C,Ep,Ef,E,M)
      IMPLICIT NONE
C
C Dummy arguments
C
      REAL*8 A, B, C, E, Ef, Ep
      INTEGER M
C
C Local variables
C
      REAL*8 arg, ee
      WDD = 0.D0
      IF (E.LE.Ef) E = 2.D0*Ef - E
      IF (E.LT.Ep) RETURN
      arg = C*(E - Ep)
      IF (arg.GT.15) RETURN
      ee = (E - Ep)**M
      WDD = A*ee/(ee + B**M)*EXP( - arg)
      END

      REAL*8 FUNCTION DOM_INT_T1(Ef,Ea,E)
C
C     Integral over E' corresponding to nonlocal additions T1(E'<<0)
C
      IMPLICIT NONE
C
C Dummy arguments
C
      REAL*8 E, Ea, Ef
C
C Local variables
C
      REAL*8 ea2, eax, ex, pi, t11, t12, t13
      pi = 4.D0*ATAN(1.D0)
      ex = E - Ef
      ea2 = Ea**2
      eax = ex + Ea
      t11 = 0.5D0*LOG(Ea)/ex
      t12 = ((2*Ea + ex)*LOG(Ea) + 0.5D0*pi*ex)/(2.*(eax**2 + ea2))
      t13 = -eax**2*LOG(eax)/(ex*(eax**2 + ea2))
      DOM_INT_T1 = ex/pi*(t11 + t12 + t13)
      END

      REAL*8 FUNCTION DOM_INT_T2(Ef,Ea,E)
C
C     Integral over E' corresponding to nonlocal additions T2(E'>>0)
C
      IMPLICIT REAL*8(A - H,O - Z)
C
C Dummy arguments
C
      REAL*8 E, Ea, Ef
C
C Local variables
C
      DOUBLE PRECISION DABS, DLOG, DSQRT
      REAL*8 el, pi, r1, r2, r3, r4
      pi = 4.D0*ATAN(1.D0)
      el = Ef + Ea
      r1 = 1.5*DSQRT(el)*DLOG(ABS((el-E)/Ea))
      IF (ABS(E).LT.1.D-6) THEN
         r2 = 0.5*el**1.5D0*(1.D0/el - DLOG(ABS(el/Ea))/Ef)
      ELSE
         r2 = 0.5*el**1.5D0/(E*Ef)
     &        *(Ef*DLOG(DABS(el/(el-E))) - E*DLOG(DABS(el/Ea)))
      ENDIF
      r3 = 2*DSQRT(DABS(Ef))*(0.5D0*pi - ATAN(DSQRT(el/DABS(Ef))))
      IF (E.GE.0.D0) THEN
         r4 = DSQRT(E)
     &        *DLOG(DABS((DSQRT(el)+DSQRT(E))/(DSQRT(el)-DSQRT(E))))
      ELSE
         r4 = -2.D0*DSQRT(DABS(E))*(0.5D0*pi - ATAN(DSQRT(DABS(el/E))))
      ENDIF
      DOM_INT_T2 = 1.D0/pi*(r1 + r2 + r3 + r4)
      END

      COMPLEX*16 FUNCTION ZFI(Za)
C
C-----FUNCTION TO EVALUATE exp(Z)*E1(Z)
C
C Complex exponential integral function multiplied by exponential
C
C AUTHOR: J. Raynal
C
      IMPLICIT NONE
C
C Dummy arguments
C
      COMPLEX*16 Za
C
C Local variables
C
      REAL*8 aj
      DOUBLE PRECISION DABS, DSQRT
      INTEGER i, m
      COMPLEX*16 y
      ZFI = 0.D0
      IF (Za.EQ.0.) RETURN
      IF (DABS(DREAL(Za) + 18.5D0).LT.25.D0) THEN
         IF (DSQRT(625.D0 - (DREAL(Za)+18.5D0)**2)
     &       /1.665D0.GE.DABS(DIMAG(Za))) THEN
            ZFI = -.57721566490153D0 - CDLOG(Za)
            y = 1.D0
            DO m = 1, 2000
               aj = m
               y = -y*Za/aj
               IF (CDABS(y).LT.1.D-15*CDABS(ZFI)) GOTO 20
               ZFI = ZFI - y/aj
            ENDDO
   20       ZFI = CDEXP(Za)*ZFI
            RETURN
         ENDIF
      ENDIF
      DO i = 1, 20
         aj = 21 - i
         ZFI = aj/(Za + ZFI)
         ZFI = aj/(1.D0 + ZFI)
      ENDDO
      ZFI = 1.D0/(ZFI + Za)
      END

      REAL*8 FUNCTION EIN(X)
C
C-----FUNCTION TO EVALUATE Ei(X)
C
      IMPLICIT NONE
C
C Dummy arguments
C
      REAL*8 X
C
C Local variables
C
      REAL*8 fac, h
      REAL FLOAT
      INTEGER n
      EIN = 0.57721566490153D0 + LOG(ABS(X))
      fac = 1.0
      DO n = 1, 100
         h = FLOAT(n)
         fac = fac*h
         EIN = EIN + X**n/(h*fac)
      ENDDO
      END


      REAL*8 FUNCTION DELTA_WV(WVF,Y)
C
C COMMON variables
C
      REAL*8 A, B, E, EF, EP, EA, WDE, WVE
      INTEGER N
      COMMON /DENERGY/ E, EF, EP, EA
      COMMON /PDATAV/ A, B, N
      COMMON /WENERG/ WDE, WVE
C
C Dummy arguments
C
      REAL*8 Y
      REAL*8 WVF
      DELTA_WV = (WVF(A,B,EP,Y,N) - WVE)/((Y - EF)**2 - (E - EF)**2)
      END


      REAL*8 FUNCTION WVF(A,B,Ep,Ef,Er,N)
C
C Dummy arguments
C
      REAL*8 A, B, Er, Ef, Ep
      INTEGER N
C
C Local variables
C
      REAL ee, E
      WVF = 0.D0
      E = Er
      IF (E.LE.Ef) E = 2.D0*Ef - E
      IF (E.LE.Ep) RETURN
      ee = (E - Ep)**N
      WVF = A*ee/(ee + B**N)
      END

      REAL*8 FUNCTION DELTA_WD(WDF,Y)
C
C COMMON variables
C
      REAL*8 A, B, C, E, EF, EP, WDE, WVE, EA
      INTEGER IQ, M
      COMMON /DENERGY/ E, EF, EP, EA
      COMMON /PDATAS/ A, B, C, M, IQ
      COMMON /WENERG/ WDE, WVE
C
C Dummy arguments
C
      REAL*8 Y
      REAL*8 WDF
      DELTA_WD = (WDF(A,B,C,EP,Y,M,IQ) - WDE)
     &           /((Y - EF)**2 - (E - EF)**2)
      END


      REAL*8 FUNCTION WDF(A,B,C,Ep,E,M,Iq)
C
C Dummy arguments
C
      REAL*8 A, B, C, E, Ep
      INTEGER Iq, M
C
C Local variables
C
      REAL*8 arg, ee
      WDF = 0.D0
      IF (E.LT.Ep) RETURN
      arg = C*(E - Ep)**Iq
      IF (arg.GT.15) RETURN
      ee = (E - Ep)**M
      WDF = A*ee/(ee + B**M)*EXP( - arg)
      END


      REAL*8 FUNCTION DOM_INT(DELTAF,F,Ef,Eint,Ecut,E,We_cte)
C
C     DOM integral (20 points Gauss-Legendre)
C
C     Divided in two intervals for higher accuracy
C     The first interval corresponds to peak of the integrand
C
C
C     THE ABSCISSAE AND WEIGHTS ARE GIVEN FOR THE INTERVAL (-1,1).
C     BECAUSE OF SYMMETRY ONLY THE POSITIVE ABSCISSAE AND THEIR
C     CORRESPONDING WEIGHTS ARE GIVEN.
C
C     XG - ABSCISSAE OF THE 41-POINT GAUSS-KRONROD RULE
C     WG - WEIGHTS OF THE 20-POINT GAUSS RULE
C
C GAUSS QUADRATURE WEIGHTS AND KRONROD QUADRATURE ABSCISSAE AND WEIGHTS
C AS EVALUATED WITH 80 DECIMAL DIGIT ARITHMETIC BY L. W. FULLERTON,
C BELL LABS, NOV. 1981.
C
C
C Dummy arguments
C
      DOUBLE PRECISION E, Ecut, Ef, Eint, We_cte
      DOUBLE PRECISION F
      DOUBLE PRECISION DELTAF
C
C Local variables
C
      DOUBLE PRECISION absc1, absc2, centr1, centr2, hlgth1, hlgth2,
     &                 resg1, resg2, wg(10), www, xg(10), xxx
      REAL corr
      DOUBLE PRECISION DLOG
      INTEGER j
      EXTERNAL F
      DATA wg(1)/0.017614007139152118311861962351853D0/
      DATA wg(2)/0.040601429800386941331039952274932D0/
      DATA wg(3)/0.062672048334109063569506535187042D0/
      DATA wg(4)/0.083276741576704748724758143222046D0/
      DATA wg(5)/0.101930119817240435036750135480350D0/
      DATA wg(6)/0.118194531961518417312377377711382D0/
      DATA wg(7)/0.131688638449176626898494499748163D0/
      DATA wg(8)/0.142096109318382051329298325067165D0/
      DATA wg(9)/0.149172986472603746787828737001969D0/
      DATA wg(10)/0.152753387130725850698084331955098D0/
C
      DATA xg(1)/0.993128599185094924786122388471320D0/
      DATA xg(2)/0.963971927277913791267666131197277D0/
      DATA xg(3)/0.912234428251325905867752441203298D0/
      DATA xg(4)/0.839116971822218823394529061701521D0/
      DATA xg(5)/0.746331906460150792614305070355642D0/
      DATA xg(6)/0.636053680726515025452836696226286D0/
      DATA xg(7)/0.510867001950827098004364050955251D0/
      DATA xg(8)/0.373706088715419560672548177024927D0/
      DATA xg(9)/0.227785851141645078080496195368575D0/
      DATA xg(10)/0.076526521133497333754640409398838D0/
C
      centr1 = 0.5D+00*(Ef + Eint)
      hlgth1 = 0.5D+00*(Eint - Ef)
      centr2 = 0.5D+00*(Ecut + Eint)
      hlgth2 = 0.5D+00*(Ecut - Eint)
C
C-----COMPUTE THE 20-POINT GAUSS-KRONROD APPROXIMATION
C-----TO THE INTEGRAL in TWO INTERVALS (Ef - Eint, Eint - Ecut)
C
      resg1 = 0.0D+00
      resg2 = 0.0D+00
      DO j = 1, 10
         xxx = xg(j)
         www = wg(j)
         absc1 = hlgth1*xxx
         resg1 = resg1 +
     &           www*(DELTAF(F,centr1 - absc1) + DELTAF(F,centr1 +
     &           absc1))
         absc2 = hlgth2*xxx
         resg2 = resg2 +
     &           www*(DELTAF(F,centr2 - absc2) + DELTAF(F,centr2 +
     &           absc2))
      ENDDO
      corr = 0.5D0*We_cte/(E - Ef)*DLOG((Ecut - (E-Ef))/(Ecut + (E-Ef)))
      DOM_INT = (resg1*hlgth1 + resg2*hlgth2 + corr)*(E - Ef)
     &          /(ACOS( - 1.D0))
      END
