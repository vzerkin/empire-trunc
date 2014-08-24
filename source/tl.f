Ccc   * $Rev: 4037 $
Ccc   * $Author: rcapote $
Ccc   * $Date: 2014-08-24 17:58:30 +0200 (So, 24 Aug 2014) $

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
      implicit none 
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
      DOUBLE PRECISION bfu, ecrit1, critl
      DOUBLE PRECISION arg, clf, homega, ra, rb, rbar, rred, xfu, xfum
      LOGICAL distrb
      INTEGER i
      DOUBLE PRECISION XFUS
      distrb = .FALSE.
C-----if critical l given in input jump directly to Tl calculations
      IF (CRL.LE.0.D0) THEN
C--------CCFUS calculations (coupled barriers)
         IF (CSRead.EQ.( - 2.D0)) THEN
            CALL CCFUS(Stl,CSRead)
            RETURN
         ENDIF

C--------CCFUS calculations (uncoupled barriers)
         IF (CSRead.EQ.( - 3.D0)) THEN
            CALL CCFUS(Stl,CSRead)
            RETURN
         ENDIF

C--------CCFUS calculations  *** done ****
C--------check for distribution barrier
         IF (CSRead.EQ.( - 1.D0)) distrb = .TRUE.
C--------calculate projectile+target binding energy if not done already
         IF (Q(0,1).EQ.0.D0) CALL BNDG(0,1,Q(0,1))
         IF (distrb) THEN
            CALL BASS(EIN,ZEJc(0),AEJc(0),Z(0),A(0),Bfu,
     &          ecrit1,critl,csfus)

            IF (Bfu.GE.0) THEN
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
               WRITE (8,*) 
     &             ' CCFUS fusion barrier is ', sngl(BFUs), ' MeV'
            ENDIF

C           IF distributed barrier, then BASS barriers are used
            IF (BFUs.LT.0.0D0) BFUs = bfu

            IF (SIG.EQ.0.0D0) SIG = 0.05*BFUs

            WRITE (8,*)
            WRITE (8,*) 
     &         ' Distributed fusion barrier with extra push=', EXPush
            WRITE (8,*) ' SIG=', sngl(SIG), ' and TRUNC=', sngl(TRUnc),
     &                     ' has been used'

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
            clf = 1.d0
         ENDIF
         xfum = 0.d0
   50    xfu = XFUS(EIN,AEJc(0),A(0),DFUs,clf)
         IF (xfu.LT.CSFus) THEN
            xfum = xfu
            clf = clf + 1.d0
            GOTO 50
         ELSE
            CRL = clf - 1 + (CSFus - xfum)/(xfu - xfum)
         ENDIF
         NLW = max(CRL + MAX(5.D0,5.0D0*DFUs),DBLE(NDLW-2))
C-----setting transmission coefficients for fusion if not distr. barr.
      ENDIF
      DO i = 1, NDLW
         arg = (CRL - i + 1)/DFUs
         arg = MIN(70.0D0,arg)
         Stl(i) = 1.d0/(1.d0 + EXP((-arg)))
      ENDDO
	RETURN
      END

      SUBROUTINE RIPL2EMPIRE(Nejc,Nnuc,E)
      implicit none 
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
      DOUBLE PRECISION EcollTarget, RCCC, elevcc
      DOUBLE PRECISION elvr, xjlvr, t12, dtmp
      INTEGER ilv, itmp, lvpr, ndbrlin, nbr	 
      CHARACTER*80 ch_iuf
      CHARACTER*1 dum
      LOGICAL coll_defined, ldynamical
      INTEGER iainp, izinp, i, j, k, n, ncalc, nld_cc 
      INTEGER iwin, ipipe_move, ipipe_copy
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
C     IF (IMOdel.EQ.4) model = 'coupled-channels rigid+soft rotor'

C-----Imodel not used for non-inelastic channels
      IF(IMOdel.GT.0 .and. (iainp.NE.A(0) .OR. izinp.NE.Z(0)
     & .OR. AEJc(Nejc).NE.AEJc(0) .OR. ZEJc(Nejc).NE.ZEJc(0)) ) GOTO 300

      MODelcc = IMOdel

C
C     the following condition means that this is a second run
C     as ND_nlv>0 and NISotop=0 means that the OMPAR.DIR exists
C
      IF (ND_nlv.GT.0 .AND. NISotop.EQ.0) coll_defined = .TRUE.
C
C     the following condition means that the collective level file exists
C     and was read to obtain the number of collective levels
C
      IF (ND_nlv.GT.0 .AND. COLfile) coll_defined = .TRUE.

      IF (IMOdel.EQ.1 .AND. (.NOT.coll_defined)) THEN
C
C--------model = 'coupled-channels rigid rotor model'
C
         coll_defined = .TRUE.
C        WRITE (8,*)

         IF (NISotop.EQ.0 .AND. FIRst_ein) THEN
            WRITE (8,*)'ERROR: Rigid rotor CC potential selected but '
            WRITE (8,*)'ERROR: no isotopes defined for this OMP     '
            WRITE (8,*)'ERROR: Error in RIPL database.                '
            WRITE (8,*)'ERROR: Report RIPL OMP to r.capotenoy@iaea.org'
            WRITE (8,*)'WARNING: Default collective levels will be used'
            GOTO 300
         ENDIF
         ncalc = 0
         DO n = 1, NISotop
            IF (iainp.EQ.IA(n) .AND. izinp.EQ.IZ(n)) ncalc = n
         ENDDO
         IF (ncalc.EQ.0) THEN
            WRITE (8,*)'WARNING: Target nucleus not listed in RIPL OMP'
            WRITE (8,*)'WARNING: Default collective levels will be used'
            GOTO 300
         ENDIF
         IF (NCOll(ncalc).EQ.0) THEN
            WRITE (8,*)'ERROR: RIPL CC rigid rotor OMP NCOll(target)= 0'
            WRITE (8,*)'ERROR: Error in RIPL database.                 '
            WRITE (8,*)'ERROR: Report RIPL OMP to r.capotenoy@iaea.org '
            WRITE (8,*)'WARNING: Default collective levels will be used'
            GOTO 300
         ENDIF

         nld_cc = 0
         DO k = 1, ND_nlv
            IF (ICOllev(k).LT.LEVcc) nld_cc = nld_cc + 1
         ENDDO
         IF (nld_cc.NE.NCOll(ncalc)) THEN
            WRITE (8,*) 
     &  'WARNING: Default number of coupled levels ', nld_cc
            WRITE (8,*) 
     &  'WARNING: is not equal ', NCOll(ncalc),' defined in RIPL OMP'   
            WRITE (8,*) 'WARNING: RIPL number of coupled channels used'
         ENDIF
C
C        rigid rotor model
C
C        Correcting CC energies of the OMP collective levels EEX 
C
         OPEN (32,FILE = 'TARGET.LEV',STATUS = 'UNKNOWN',ERR=1056)
         DO n=2,NCOll(ncalc)
            elevcc = EEX(n,ncalc)
            REWIND(32)
            READ (32,'(A80)',END=1056,ERR=1056) ch_iuf
            DO ilv = 1, NLV(nnuc)
              READ (32,'(I3,1X,F10.6,1X,F5.1,I3,1X,E10.2,I3)'
     &              ,END=1056,ERR=1056) 
     &          itmp, elvr, xjlvr, lvpr, t12, ndbrlin
              DO nbr = 1, ndbrlin
                READ (32,'(A1)') dum
              ENDDO
              if( abs(elvr-elevcc).le.0.001          .and. ! energy
     &            abs(xjlvr-SPInv(n,ncalc)).le.0.005 .and. ! spin
     &           iabs(lvpr-IPArv(n,ncalc)).eq.0 ) then     ! parity
                    EEX(n,ncalc) = elvr
                    EXIT
              endif 
            ENDDO
         ENDDO
 1056    CLOSE(32)  
C
C        Correcting Default Collective Levels using 
C           corrected CC energies from the RIPL OMP
C
         nld_cc = 1
         DO ilv=2,ND_nlv
           DO n=2,NCOll(ncalc)
             if( abs(D_Elv(ilv)-EEX(n,ncalc)).le.0.001    .and. 
     &           abs(D_Xjlv(ilv)-SPInv(n,ncalc)).le.0.005 .and.
     &          iabs(NINT(D_Lvp(ilv))-IPArv(n,ncalc)).eq.0 ) then
                  nld_cc = nld_cc + 1
                  IF (ICOllev(ilv).GT.LEVcc) 
     &              ICOllev(ilv) = ICOllev(ilv) - LEVcc
                  D_Elv(ilv) = EEX(n,ncalc) 
             endif
           ENDDO
         ENDDO

C        DO k = 1, NCOll(n)
C          READ (Ko,99045,ERR = 200) EEX(k,n), SPIn(k,n), IPAr(k,n)
C        ENDDO
C
C--------Putting Coupled levels first
         DO i = 2, ND_nlv
            DO j = i + 1, ND_nlv
               IF (ICOllev(j).LT.ICOllev(i)) THEN
C-----------------swapping
                  itmp = ICOllev(i)
                  ICOllev(i) = ICOllev(j)
                  ICOllev(j) = itmp

                  dtmp = D_Elv(i)
                  D_Elv(i) = D_Elv(j)
                  D_Elv(j) = dtmp

                  dtmp = D_Lvp(i)
                  D_Lvp(i) = D_Lvp(j)
                  D_Lvp(j) = dtmp

                  dtmp = D_Xjlv(i)
                  D_Xjlv(i) = D_Xjlv(j)
                  D_Xjlv(j) = dtmp

                  dtmp = D_Def(i,2)
                  D_Def(i,2) = D_Def(j,2)
                  D_Def(j,2) = dtmp

               ENDIF
            ENDDO
         ENDDO
 
C--------Setting EMPIRE global variables
         WRITE (8,*) 
         WRITE (8,*) 'Deformation of the gsb adopted from CC RIPL OMP'
         WRITE (8,*) 
         LMAxcc = LMAx(ncalc)
         IDEfcc = IDEf(ncalc)
         DO k = 2, IDEfcc, 2
            D_Def(1,k) = DDEf(ncalc,k)
         ENDDO
C
C--------Joining TARGET_COLL.DAT and TARGET_COLL_RIPL.DAT files
C
         OPEN (32,FILE = 'TARGET_COLL_RIPL.DAT')
         OPEN (97,FILE = 'TARGET_COLL.DAT',ERR=300)
         OPEN (96,FILE = 'COLL.DAT')
         READ (97,*)                    ! FIRST LINE

         WRITE (8,*)
         WRITE (8,*)
     &       'Collective levels from RIPL CC OMP, symm.rotational model'
         WRITE (32,*)
     &       'Collective levels from RIPL CC OMP, symm.rotational model'
         WRITE (96,*) 
     &           'Collective levels: RIPL CC OMP + default, rigid model'
C
         READ (97,'(A80)') ch_iuf       ! 2ND LINE
         WRITE (8,'(A80)') ch_iuf
         WRITE (32,'(A80)') ch_iuf
         WRITE (96,'(A80)') ch_iuf
C
         READ (97,*)                    ! model type , ! 3ER LINE
         WRITE (8, '(1x,i3,1x,i3,1x,a35)') izinp, iainp, 
     &                                 ' nucleus is treated as deformed'
         WRITE (32,'(1x,i3,1x,i3,1x,a35)') izinp, iainp,
     &                                 ' nucleus is treated as deformed'
         WRITE (96,'(1x,i3,1x,i3,1x,a35)') izinp, iainp,
     &                                 ' nucleus is treated as deformed'

         READ (97,*)                    ! EMPTY LINE
         WRITE (8,*) 
         WRITE (32,*) 
         WRITE (96,*) 

         SOFt     = .FALSE.
         DYNam    = .FALSE.
         DEFormed = .TRUE.

         READ (97,*) 
         WRITE (8,*) '   Ncoll  Lmax IDef  Kgs  (Def(1,j),j=2,IDef,2)'
         WRITE (32,*)'   Ncoll  Lmax IDef  Kgs  (Def(1,j),j=2,IDef,2)'
         WRITE (96,*)'   Ncoll  Lmax IDef  Kgs  (Def(1,j),j=2,IDef,2)'
         READ (97,*) 
         WRITE (8,'(3x,3I5,1x,F5.1,1x,6(e10.3,1x))') NCOll(ncalc),
     &                LMAx(ncalc), IDEf(ncalc), BANdk(ncalc),
     &                (DDEf(ncalc,k),k = 2,IDEf(ncalc),2)
         WRITE (32,'(3x,3I5,1x,F5.1,1x,6(e10.3,1x))') NCOll(ncalc),
     &                LMAx(ncalc), IDEf(ncalc), BANdk(ncalc),
     &                (DDEf(ncalc,k),k = 2,IDEf(ncalc),2)
         WRITE (96,'(3x,3I5,1x,F5.1,1x,6(e10.3,1x))') ND_nlv,
     &                LMAx(ncalc), IDEf(ncalc), BANdk(ncalc),
     &                (DDEf(ncalc,k),k = 2,IDEf(ncalc),2)
         READ (97,*) 
         WRITE (8,*)
         WRITE (32,*)
         WRITE (96,*)
         READ (97,*) 
         WRITE (8,*) ' N   E[MeV]  J   pi Nph L  K   Dyn.Def.'
         WRITE (32,*)' N   E[MeV]  J   pi Nph L  K   Dyn.Def.'
         WRITE (96,*)' N   E[MeV]  J   pi Nph L  K   Dyn.Def.'

         if(FIRst_ein) then
C
C          First energy with default TARGET_COLL.DAT
C
C----------The deformation for excited levels is not used in the pure
C----------symm.rotational model but could be used for vibrational
C----------rotational model so we are setting it to 0.005

           DO k = 1, ND_nlv

             READ (97,'(A80)',END=100,ERR=100) ch_iuf        

             IF (ICOllev(k).LT.LEVcc .and. k.gt.1) D_Def(k,2) = 0.005

             IF (ICOllev(k).GE.LEVcc .and. D_Def(k,2).le.0.d0) 
     &          D_Def(k,2) = 0.005

             IF (ICOllev(k).LT.LEVcc) WRITE (32,
     &        '(1x,I2,1x,F7.4,1x,F4.1,1x,F3.0,1x,3(I2,1x),e10.3)')
     &             ICOllev(k), D_Elv(k), D_Xjlv(k), D_Lvp(k),
     &             0, 0, 0, D_Def(k,2)

             WRITE (8,
     &        '(1x,I2,1x,F7.4,1x,F4.1,1x,F3.0,1x,3(I2,1x),e10.3)')
     &             ICOllev(k), D_Elv(k), D_Xjlv(k), D_Lvp(k),
     &             0, 0, 0, D_Def(k,2)

             WRITE (96,
     &        '(1x,I2,1x,F7.4,1x,F4.1,1x,F3.0,1x,3(I2,1x),e10.3)')
     &             ICOllev(k), D_Elv(k), D_Xjlv(k), D_Lvp(k),
     &             0, 0, 0, D_Def(k,2)

           ENDDO
         
         ELSE ! second run, coll level file exists !

           DO k = 1, ND_nlv 
             READ (97,'(A80)',END=100,ERR=100) ch_iuf      
             WRITE (96,'(A80)') ch_iuf
             WRITE (8 ,'(A80)') ch_iuf
           ENDDO

         ENDIF

  100    CLOSE (32)
         CLOSE (96)
         CLOSE (97)
         iwin = ipipe_copy('COLL.DAT','TARGET_COLL.DAT')
C        iwin = ipipe_move('COLL.DAT','TARGET_COLL.DAT')
C
C--------JOIN finished
C
      ENDIF

      IF (IMOdel.EQ.2 .AND. (.NOT.coll_defined)) THEN
C
C--------model = 'coupled-channels vibrational model'
C
         coll_defined = .TRUE.
C        WRITE (8,*)
         IF (NISotop.EQ.0 .AND. FIRst_ein) THEN
            WRITE (8,*)'ERROR: Vibrat. CC potential selected but    '
            WRITE (8,*)'ERROR: no isotopes defined for this OMP     '
            WRITE (8,*)'ERROR: Error in RIPL database.                '
            WRITE (8,*)'ERROR: Report RIPL OMP to r.capotenoy@iaea.org'
            WRITE (8,*)'WARNING: Default collective levels will be used'
            GOTO 300
         ENDIF
         ncalc = 0
         DO n = 1, NISotop
            IF (iainp.EQ.IA(n) .AND. izinp.EQ.IZ(n)) ncalc = n
         ENDDO
         IF (ncalc.EQ.0) THEN
            WRITE (8,*)'WARNING: Target nucleus not listed in RIPL OMP'
            WRITE (8,*)'WARNING: Default collective levels will be used'
            GOTO 300
         ENDIF
         IF (NVIb(ncalc).EQ.0) THEN
            WRITE (8,*)'ERROR: RIPL CC vibr. OMP NVIb(target) = 0    '
            WRITE (8,*)'ERROR: Error in RIPL database.                 '
            WRITE (8,*)'ERROR: Report RIPL OMP to r.capotenoy@iaea.org '
            WRITE (8,*)'WARNING: Default collective levels will be used'
            GOTO 300
         ENDIF
         DO k = 2, NVIb(ncalc)
            IF (NPH(k,ncalc).GT.2) THEN
              IWArn = 6
              WRITE (8,*) 'WARNING: NPH(k,i)=3 !!! in RIPL OMP'
              WRITE (8,*)
     &          'WARNING: Default collective levels will be used'
              GOTO 300
            ENDIF
         ENDDO

         nld_cc = 0
         DO k = 1, ND_nlv
            IF (ICOllev(k).LT.LEVcc) nld_cc = nld_cc + 1
         ENDDO
         IF (nld_cc.NE.NVIb(ncalc)) THEN
            WRITE (8,*) 
     &  'WARNING: Default number of coupled levels ', nld_cc
            WRITE (8,*) 
     &  'WARNING: is not equal ', NVIb(ncalc),' defined in RIPL OMP'   
            WRITE (8,*) 'WARNING: RIPL number of coupled channels used'
         ENDIF
C        
C        vibrational model
C
C        Correcting CC energies of the OMP collective levels EXV 
C
         OPEN (32,FILE = 'TARGET.LEV',STATUS = 'UNKNOWN',ERR=1057)
         DO n=2,NVIb(ncalc)
            elevcc = EXV(n,ncalc)
            REWIND(32)
            READ (32,'(A80)',END=1057,ERR=1057) ch_iuf
            DO ilv = 1, NLV(nnuc)
              READ (32,'(I3,1X,F10.6,1X,F5.1,I3,1X,E10.2,I3)'
     &              ,END=1057,ERR=1057) 
     &          itmp, elvr, xjlvr, lvpr, t12, ndbrlin
              DO nbr = 1, ndbrlin
                READ (32,'(A1)') dum
              ENDDO
              if( abs(elvr-elevcc).le.0.001          .and. ! energy
     &            abs(xjlvr-SPInv(n,ncalc)).le.0.005 .and. ! spin
     &           iabs(lvpr-IPArv(n,ncalc)).eq.0 ) then     ! parity
                  EXV(n,ncalc) = elvr
                  EXIT
              endif 
            ENDDO
         ENDDO
 1057    CLOSE(32)  
C
C        Correcting Default Collective Levels using 
C           corrected CC energies from the RIPL OMP
C
         nld_cc = 1
         DO ilv=2,ND_nlv
           DO n=2,NVIb(ncalc)

             if( abs(D_Elv(ilv)-EXV(n,ncalc)).le.0.01      .and. 
     &            abs(D_Xjlv(ilv)-SPInv(n,ncalc)).le.0.005 .and.
     &           iabs(NINT(D_Lvp(ilv))-IPArv(n,ncalc)).eq.0 ) then

                  nld_cc = nld_cc + 1

                  IF (ICOllev(ilv).GT.LEVcc) 
     &              ICOllev(ilv) = ICOllev(ilv) - LEVcc

                  D_Elv(ilv) = EXV(n,ncalc) 
                  IPH(ilv)   = NPH(n,ncalc)
                  D_Def(ilv,2) = DEFv(n,ncalc)

C                 READ (Ko,99045,ERR = 200) EXV(k,n), SPInv(k,n),
C    &                IPArv(k,n), NPH(k,n), DEFv(k,n), THEtm(k,n)
             endif

           ENDDO
         ENDDO
C
C--------Putting Coupled levels first
         DO i = 2, ND_nlv
            DO j = i + 1, ND_nlv
               IF (ICOllev(j).LT.ICOllev(i)) THEN
C-----------------swapping
                  itmp = ICOllev(i)
                  ICOllev(i) = ICOllev(j)
                  ICOllev(j) = itmp

                  dtmp = D_Elv(i)
                  D_Elv(i) = D_Elv(j)
                  D_Elv(j) = dtmp

                  dtmp = D_Lvp(i)
                  D_Lvp(i) = D_Lvp(j)
                  D_Lvp(j) = dtmp

                  dtmp = D_Xjlv(i)
                  D_Xjlv(i) = D_Xjlv(j)
                  D_Xjlv(j) = dtmp

                  dtmp = D_Def(i,2)
                  D_Def(i,2) = D_Def(j,2)
                  D_Def(j,2) = dtmp

                  itmp = IPH(i)
                  IPH(i) = IPH(j)
                  IPH(j) = itmp
               ENDIF
            ENDDO
         ENDDO
C
C--------Putting one phonon coupled states first for spherical
         DO i = 2, ND_nlv
            IF (ICOllev(i).GE.LEVcc) cycle
            DO j = i + 1, ND_nlv
               IF (ICOllev(j).GE.LEVcc) cycle
               IF (IPH(j).LT.IPH(i)) THEN
C-----------------swapping
                  itmp = IPH(i)
                  IPH(i) = IPH(j)
                  IPH(j) = itmp

                  dtmp = D_Def(i,2)
                  D_Def(i,2) = D_Def(j,2)
                  D_Def(j,2) = dtmp

                  dtmp = D_Lvp(i)
                  D_Lvp(i) = D_Lvp(j)
                  D_Lvp(j) = dtmp

                  dtmp = D_Elv(i)
                  D_Elv(i) = D_Elv(j)
                  D_Elv(j) = dtmp

                  dtmp = D_Xjlv(i)
                  D_Xjlv(i) = D_Xjlv(j)
                  D_Xjlv(j) = dtmp

                  itmp = ICOllev(i)
                  ICOllev(i) = ICOllev(j)
                  ICOllev(j) = itmp

               ENDIF
            ENDDO
         ENDDO

         WRITE (8,*)
C
C--------Joining TARGET_COLL.DAT and TARGET_COLL_RIPL.DAT files
C
         OPEN (32,FILE = 'TARGET_COLL_RIPL.DAT')
         OPEN (97,FILE = 'TARGET_COLL.DAT',ERR=300)
         OPEN (96,FILE = 'COLL.DAT')
         READ (97,'(A80)') ch_iuf   ! FIRST LINE
         WRITE (8,*)
         WRITE (8,*)
     &           'Collective levels from RIPL CC OMP, vibrational model'
         WRITE (32,*)
     &           'Collective levels from RIPL CC OMP, vibrational model'
         WRITE (96,*) 
     &           'Collective levels: RIPL CC OMP + default, vibr. model'
C
         READ (97,'(A80)') ch_iuf       ! 2ND LINE
         WRITE (8,'(A80)') ch_iuf
         WRITE (32,'(A80)') ch_iuf
         WRITE (96,'(A80)') ch_iuf
C
         READ (97,'(A80)') ch_iuf       ! 3ER LINE
         WRITE (8, '(1x,i3,1x,i3,1x,a35)') izinp, iainp, ! 3ER LINE
     &                                ' nucleus is treated as spherical'
         WRITE (32,'(1x,i3,1x,i3,1x,a35)') izinp, iainp,
     &                                ' nucleus is treated as spherical'
         WRITE (96,'(1x,i3,1x,i3,1x,a35)') izinp, iainp,
     &                                ' nucleus is treated as spherical'

         READ (97,'(A80)') ch_iuf       ! EMPTY LINE
         WRITE (96,*) 
         WRITE (8,*)
         WRITE (32,*)

         SOFt     = .FALSE.
         DYNam    = .FALSE.
         DEFormed = .FALSE.

         READ (97,'(A80)') ch_iuf
         WRITE (8 ,*) '   Ncoll'
         WRITE (32,*) '   Ncoll'
         WRITE (96,*) '   Ncoll'

         READ (97,'(A80)') ch_iuf
         WRITE (8,'(3x,3I5)') ND_nlv
         WRITE (32,'(3x,3I5)') NVIb(n)
         WRITE (96,'(3x,3I5)') ND_nlv
C
         READ (97,'(A80)') ch_iuf
         WRITE (8,*)
         WRITE (32,*)
         WRITE (96,*)
C
         READ (97,'(A80)') ch_iuf
         WRITE (8,*) ' N   E[MeV]  J   pi Nph L  K   Dyn.Def.'
         WRITE (32,*)' N   E[MeV]  J   pi Nph L  K   Dyn.Def.'
         WRITE (96,*)' N   E[MeV]  J   pi Nph L  K   Dyn.Def.'

         if(FIRst_ein) then
C          first energy with default TARGET_COLL.DAT
           DO k = 1, ND_nlv

             READ (97,'(A80)',END=200,ERR=200) ch_iuf        

             IF (ICOllev(k).GE.LEVcc .and. D_Def(k,2).le.0.d0) 
     &          D_Def(k,2) = 0.005

             IF (ICOllev(k).LT.LEVcc) WRITE (32,
     &        '(1x,I2,1x,F7.4,1x,F4.1,1x,F3.0,1x,3(I2,1x),e10.3)')
     &             ICOllev(k), D_Elv(k), D_Xjlv(k), D_Lvp(k),
     &             IPH(k), 0, 0, D_Def(k,2)

             WRITE (8,
     &        '(1x,I2,1x,F7.4,1x,F4.1,1x,F3.0,1x,3(I2,1x),e10.3)')
     &             ICOllev(k), D_Elv(k), D_Xjlv(k), D_Lvp(k),
     &             IPH(k), 0, 0, D_Def(k,2)

             WRITE (96,
     &        '(1x,I2,1x,F7.4,1x,F4.1,1x,F3.0,1x,3(I2,1x),e10.3)')
     &             ICOllev(k), D_Elv(k), D_Xjlv(k), D_Lvp(k),
     &             IPH(k), 0, 0, D_Def(k,2)

           ENDDO
         
         ELSE ! second run, coll level file exists !

           DO k = 1, ND_nlv 
             READ (97,'(A80)',END=200,ERR=200) ch_iuf      
             WRITE (96,'(A80)') ch_iuf
             WRITE (8 ,'(A80)') ch_iuf
           ENDDO

         ENDIF

  200    CLOSE (32)
         CLOSE (96)
         CLOSE (97)

C        PAUSE 'Joining of vibrational COLL finished'

C        iwin = ipipe_copy('COLL.DAT','TARGET_COLL.DAT')
         iwin = ipipe_move('COLL.DAT','TARGET_COLL.DAT')
C
C--------JOIN finished 
C                                                                   
      ENDIF

      IF (IMOdel.EQ.3 .AND. (.NOT.coll_defined)) THEN
C
C--------model = 'coupled-channels soft rotor model'
C
         coll_defined = .TRUE.
C        WRITE (8,*)
         IF (NISotop.EQ.0 .AND. FIRst_ein) THEN
            WRITE (8,*)'ERROR: Soft rotor potential selected but      '
            WRITE (8,*)'ERROR: no isotopes defined for this OMP.      '
            WRITE (8,*)'ERROR: Error in RIPL database.                '
            WRITE (8,*)'ERROR: Report RIPL OMP to r.capotenoy@iaea.org'
            WRITE (8,*)'ERROR: Change your selected RIPL potential    '
            WRITE (8,*)'ERROR: EMPIRE stops'
            STOP       'ERROR: See long listing *.lst, EMPIRE stops   '
         ENDIF

         ncalc = 0
         DO n = 1, NISotop
            IF (iainp.EQ.IA(n) .AND. izinp.EQ.IZ(n)) ncalc = n
         ENDDO
         IF (ncalc.EQ.0) THEN
            WRITE (8,*)'ERROR: Soft rotor potential selected but    '
            WRITE (8,*)'ERROR: target nucleus not listed in RIPL OMP'
            WRITE (8,*)'ERROR: No default hamiltonian is available  '
            WRITE (8,*)'ERROR: for the soft rotor model !           '
            WRITE (8,*)'ERROR: EMPIRE stops !'
            STOP       'ERROR: See long listing *.lst, EMPIRE stops '
         ENDIF
         IF (NCOll(ncalc).EQ.0) THEN
            WRITE (8,*)'ERROR: Soft rotor potential selected but zero'
            WRITE (8,*)'ERROR: CC levels are defined, error in RIPL'
            WRITE (8,*)'ERROR: Report to r.capotenoy@iaea.org        '
            WRITE (8,*)'ERROR: Change your selected RIPL potential '
            WRITE (8,*)'ERROR: EMPIRE stops !'
            STOP       'ERROR: See long listing *.lst, EMPIRE stops'
         ENDIF

         IF (ND_nlv .LT. NCOll(ncalc)) THEN
            WRITE (8,*)
     &       'ERROR: Number of collective levels < OMP RIPL # CC    '
            WRITE (8,*)
     &       'ERROR: Delete ECDWBA from the input and the collective'
            WRITE (8,*)
     &       'ERROR: level file *-lev.col, and rerun'
            STOP 'ERROR: see the long output (*.lst)'
         ENDIF
C
C--------Setting EMPIRE global variables
C
         nld_cc = 1
         DO k = 2, ND_nlv
            IF (ICOllev(k).LT.LEVcc) nld_cc = nld_cc + 1
         ENDDO
         IF (nld_cc.NE.NCOll(ncalc)) THEN
            WRITE (8,*) 
     &  'WARNING: Default number of coupled levels ', nld_cc
            WRITE (8,*) 
     &  'WARNING: is not equal ', NCOll(ncalc),' defined in RIPL OMP'
            WRITE (8,*) 'WARNING: RIPL number of coupled channels used'
         ENDIF
C
C        soft rotor model
C
C        Correcting CC energies of the OMP collective levels EXV 
C
         OPEN (32, FILE = 'TARGET.LEV',STATUS = 'UNKNOWN',ERR=1058)
         DO n=2,NCOll(ncalc)
            REWIND(32)
            READ (32,'(A80)',END=1058,ERR=1058) ch_iuf
            DO ilv = 1, NLV(nnuc)
              READ (32,'(I3,1X,F10.6,1X,F5.1,I3,1X,E10.2,I3)'
     &              ,END=1058,ERR=1058) 
     &          itmp, elvr, xjlvr, lvpr, t12, ndbrlin
              DO nbr = 1, ndbrlin
                READ (32,'(A1)',END=1058,ERR=1058) dum
              ENDDO
              if( abs(elvr-EXV(n,ncalc)).le.0.001    .and. 
     &            abs(xjlvr-SPInv(n,ncalc)).le.0.005 .and.
     &           iabs(lvpr-IPArv(n,ncalc)).eq.0 ) then
                    EXV(n,ncalc) = elvr
                    exit 
              endif 
            ENDDO
         ENDDO
 1058    CLOSE(32)  
C
C        Correcting Default Collective Levels using 
C           corrected CC energies from the RIPL OMP
C
         IPH(1)   = SR_ntu(1,ncalc)
         D_Klv(1) = SR_nnb(1,ncalc)
         D_Llv(1) = SR_nng(1,ncalc)
         D_nno(1) = SR_nno(1,ncalc)

         nld_cc = 1
         DO n=2,NCOll(ncalc)
           DO ilv=2,ND_nlv
             if( abs(D_Elv(ilv)-EXV(n,ncalc)).le.0.001    .and. 
     &           abs(D_Xjlv(ilv)-SPInv(n,ncalc)).le.0.005 .and.
     &          iabs(NINT(D_Lvp(ilv))-IPArv(n,ncalc)).le.0.005 ) then

                  nld_cc = nld_cc + 1

                  IF (ICOllev(ilv).GT.LEVcc) 
     &              ICOllev(ilv) = ICOllev(ilv) - LEVcc
                  
                  D_Elv(ilv) = EXV(n,ncalc) 

                  IPH(ilv)   = SR_ntu(n,ncalc)
                  D_Klv(ilv) = SR_nnb(n,ncalc)
                  D_Llv(ilv) = SR_nng(n,ncalc)
                  D_nno(ilv) = SR_nno(n,ncalc)

                  exit 
             endif 
           ENDDO
         ENDDO

C        do k=1,NCOll(n)
C          read(ko,99049) EXV(k,n),SPInv(k,n),IPArv(k,n),
C    +               SR_ntu(k,n),SR_nnb(k,n),SR_nng(k,n),SR_nno(k,n)
C        enddo
C
C--------Putting Coupled levels first
         DO i = 2, ND_nlv
            DO j = i + 1, ND_nlv
               IF (ICOllev(j).LT.ICOllev(i)) THEN
C-----------------swapping
                  itmp = ICOllev(i)
                  ICOllev(i) = ICOllev(j)
                  ICOllev(j) = itmp

                  dtmp = D_Elv(i)
                  D_Elv(i) = D_Elv(j)
                  D_Elv(j) = dtmp
                  
                  dtmp = D_Xjlv(i)
                  D_Xjlv(i) = D_Xjlv(j)
                  D_Xjlv(j) = dtmp

                  dtmp = D_Lvp(i)
                  D_Lvp(i) = D_Lvp(j)
                  D_Lvp(j) = dtmp

                  itmp = IPH(i)
                  IPH(i) = IPH(j)
                  IPH(j) = itmp

                  dtmp = D_Klv(i)
                  D_Klv(i) = D_Klv(j)
                  D_Klv(j) = dtmp

                  dtmp = D_Llv(i)
                  D_Llv(i) = D_Llv(j)
                  D_Llv(j) = dtmp

                  dtmp = D_nno(i)
                  D_nno(i) = D_nno(j)
                  D_nno(j) = dtmp
               ENDIF
            ENDDO
         ENDDO
C
C--------Joining TARGET_COLL.DAT and TARGET_COLL_RIPL.DAT files
C
         OPEN (32,FILE = 'TARGET_COLL_RIPL.DAT')
         OPEN (97,FILE = 'TARGET_COLL.DAT',ERR=300)      
         OPEN (96,FILE = 'COLL.DAT')

         WRITE (8,*)
         WRITE (8,*)
     &    'Collective levels from RIPL CC OMP, soft rotator model'
         WRITE (32,*)
     &    'Collective levels from RIPL CC OMP, soft rotator model'

         READ (97,'(A80)') ch_iuf       ! FIRST LINE
         WRITE (96,*)
     &           'Collective levels: RIPL CC OMP + default, soft rotor '
         READ (97,'(A80)') ch_iuf       ! 2ND LINE
         WRITE (96,'(A80)') ch_iuf
         WRITE (32,'(A80)') ch_iuf
         WRITE (8 ,'(A80)') ch_iuf

         READ (97,'(A80)') ch_iuf       ! 3ER LINE
         ldynamical = .FALSE.
         if(ch_iuf(36:39).eq.'soft') ldynamical = .TRUE.

         WRITE ( 8,'(1x,i3,1x,i3,1x,a35)') izinp, iainp,
     &                                ' nucleus is treated as soft     '
         WRITE (32,'(1x,i3,1x,i3,1x,a35)') izinp, iainp,
     &                                ' nucleus is treated as soft     '
         WRITE (96,'(1x,i3,1x,i3,1x,a35)') izinp, iainp,
     &                                ' nucleus is treated as soft     '
         SOFT     = .TRUE.
         DYNam    = .FALSE.
         DEFormed = .FALSE.

         if(.NOT.ldynamical) then
C           first run with default TARGET_COLL.DAT
            WRITE (8,*)
            WRITE (32,*)
            WRITE (96,*) 

            WRITE (8 ,*)'Soft rotator hamiltonian parameters for OPTMAN'        
            WRITE (32,*)'Soft rotator hamiltonian parameters for OPTMAN'
            WRITE (96,*)'Soft rotator hamiltonian parameters for OPTMAN'

            write(8 ,'(6(e11.5,1x))')  ! Record 3 from Hamiltonian parameters)
     &                    SR_hw(ncalc),SR_amb0(ncalc),SR_amg0(ncalc),
     &                    SR_gam0(ncalc),SR_bet0(ncalc),SR_bet4(ncalc)
            write(8 ,'(6(e11.5,1x))')  ! Record 4 from Hamiltonian parameters)
     &                    SR_bb42(ncalc),SR_gamg(ncalc),SR_delg(ncalc),
     &                    SR_bet3(ncalc),SR_et0(ncalc),SR_amu0(ncalc)
            write(8 ,'(6(e11.5,1x))')  ! Record 5 from Hamiltonian parameters)
     &                    SR_hw0(ncalc),SR_bb32(ncalc),SR_gamde(ncalc),
     &                    SR_dpar(ncalc),SR_gshape(ncalc)
 
            write(32,'(6(e11.5,1x))')  ! Record 3 from Hamiltonian parameters)
     &                    SR_hw(ncalc),SR_amb0(ncalc),SR_amg0(ncalc),
     &                    SR_gam0(ncalc),SR_bet0(ncalc),SR_bet4(ncalc)
            write(32,'(6(e11.5,1x))')  ! Record 4 from Hamiltonian parameters)
     &                    SR_bb42(ncalc),SR_gamg(ncalc),SR_delg(ncalc),
     &                    SR_bet3(ncalc),SR_et0(ncalc),SR_amu0(ncalc)
            write(32,'(6(e11.5,1x))')  ! Record 5 from Hamiltonian parameters)
     &                    SR_hw0(ncalc),SR_bb32(ncalc),SR_gamde(ncalc),
     &                    SR_dpar(ncalc),SR_gshape(ncalc)

            write(96,'(6(e11.5,1x))')  ! Record 3 from Hamiltonian parameters)
     &                    SR_hw(ncalc),SR_amb0(ncalc),SR_amg0(ncalc),
     &                    SR_gam0(ncalc),SR_bet0(ncalc),SR_bet4(ncalc)
            write(96,'(6(e11.5,1x))')  ! Record 4 from Hamiltonian parameters)
     &                    SR_bb42(ncalc),SR_gamg(ncalc),SR_delg(ncalc),
     &                    SR_bet3(ncalc),SR_et0(ncalc),SR_amu0(ncalc)
            write(96,'(6(e11.5,1x))')  ! Record 5 from Hamiltonian parameters)
     &                    SR_hw0(ncalc),SR_bb32(ncalc),SR_gamde(ncalc),
     &                    SR_dpar(ncalc),SR_gshape(ncalc)

C           Initializing Soft Rotator Hamiltonian  
            SR_Ham_hw     = SR_hw(ncalc)       
            SR_Ham_amb0   = SR_amb0(ncalc)       
            SR_Ham_amg0   = SR_amg0(ncalc)       
            SR_Ham_gam0   = SR_gam0(ncalc)       
            SR_Ham_bet0   = SR_bet0(ncalc)       
            SR_Ham_bet4   = SR_bet4(ncalc)       
C
            SR_Ham_bb42   = SR_bb42(ncalc)       
            SR_Ham_gamg   = SR_gamg(ncalc)  
            SR_Ham_delg   = SR_delg(ncalc)  
            SR_Ham_bet3   = SR_bet3(ncalc)       
            SR_Ham_et0    = SR_et0(ncalc)   
            SR_Ham_amu0   = SR_amu0(ncalc)  
C
            SR_Ham_hw0    = SR_hw0(ncalc)  
            SR_Ham_bb32   = SR_bb32(ncalc)       
            SR_Ham_gamde  = SR_gamde(ncalc) 
            SR_Ham_dpar   = SR_dpar(ncalc)       
            SR_Ham_gshape = SR_gshape(ncalc)

         else

C           following runs with Hamiltonian lines
C
            READ (97,'(A80)') ch_iuf       ! EMPTY LINE
            WRITE (96,'(A80)') ch_iuf
            READ (97,'(A80)') ch_iuf       ! Soft rotator hamiltonian ...
            WRITE (96,'(A80)') ch_iuf

            read(97,'(6(e11.5,1x))')  ! Record 3 from OPTMAN (Hamiltonian parameters)
     +         SR_Ham_hw,SR_Ham_amb0,SR_Ham_amg0,
     +         SR_Ham_gam0,SR_Ham_bet0,SR_Ham_bet4
            WRITE (96,'(6E12.5)') 
     +         SR_Ham_hw,SR_Ham_amb0,SR_Ham_amg0,
     +         SR_Ham_gam0,SR_Ham_bet0,SR_Ham_bet4

            read(97,'(6(e11.5,1x))')  ! Record 4 from OPTMAN (Hamiltonian parameters)
     +         SR_Ham_bb42,SR_Ham_gamg,SR_Ham_delg,
     +         SR_Ham_bet3,SR_Ham_et0,SR_Ham_amu0
            WRITE (96,'(6E12.5)') 
     +         SR_Ham_bb42,SR_Ham_gamg,SR_Ham_delg,
     +         SR_Ham_bet3,SR_Ham_et0,SR_Ham_amu0

            read(97,'(6(e11.5,1x))')  ! Record 5 from OPTMAN (Hamiltonian parameters)
     +         SR_Ham_hw0 ,SR_Ham_bb32,SR_Ham_gamde,
     +         SR_Ham_dpar,SR_Ham_gshape
            WRITE (96,'(6E12.5)') 
     +         SR_Ham_hw0 ,SR_Ham_bb32,SR_Ham_gamde,
     +         SR_Ham_dpar,SR_Ham_gshape

         endif

         READ (97,'(A80)') ch_iuf       ! EMPTY LINE
         WRITE (96,'(A80)') ch_iuf
         WRITE (32,'(A80)') ch_iuf
         WRITE (8 ,'(A80)') ch_iuf

         READ (97,'(A80)') ch_iuf       ! Ncoll LINE
C        WRITE (96,'(A80)') ch_iuf
         WRITE (8,*)  '   Ncoll'
         WRITE (32,*) '   Ncoll'
         WRITE (96,*) '   Ncoll'

         READ (97,'(A80)') ch_iuf       ! EMPTY LINE
         WRITE (96,'(3x,I5)') ND_nlv
         WRITE (8 ,'(3x,I5)') ND_nlv
         WRITE (32,'(3x,I5)') NCOll(ncalc)

         READ (97,'(A80)') ch_iuf       ! EMPTY LINE
         WRITE (96,'(A80)')
C        WRITE (96,'(A80)') ch_iuf
         WRITE (8,*)
         WRITE (32,*)

         READ (97,'(A80)') ch_iuf       ! EMPTY LINE
         WRITE (8 ,*) ' N   E[MeV]  J   pi Ntu Nb Ng   -----   No'
         WRITE (32,*) ' N   E[MeV]  J   pi Ntu Nb Ng   -----   No'
         WRITE (96,*) ' N   E[MeV]  J   pi Ntu Nb Ng   -----   No'

         if(.NOT.ldynamical) then
C
C          first run with default TARGET_COLL.DAT
C      
           DO k = 1, ND_nlv

             READ (97,'(A80)',END=290,ERR=290) ch_iuf        

             IF (ICOllev(k).LT.LEVcc) D_Def(k,2) = 0.005

             IF (ICOllev(k).LT.LEVcc) WRITE (32,
     &        '(1x,I2,1x,F7.4,1x,F4.1,1x,F3.0,1x,3(I2,1x),e10.3,1x,I2)')
     &             ICOllev(k), D_Elv(k), D_Xjlv(k), D_Lvp(k),
     &             IPH(k),D_Klv(k),D_Llv(k),D_Def(k,2), D_nno(k)

             WRITE (8,
     &        '(1x,I2,1x,F7.4,1x,F4.1,1x,F3.0,1x,3(I2,1x),e10.3,1x,I2)')
     &             ICOllev(k), D_Elv(k), D_Xjlv(k), D_Lvp(k),
     &             IPH(k),D_Klv(k),D_Llv(k),D_Def(k,2), D_nno(k)

             WRITE (96,
     &        '(1x,I2,1x,F7.4,1x,F4.1,1x,F3.0,1x,3(I2,1x),e10.3,1x,I2)')
     &             ICOllev(k), D_Elv(k), D_Xjlv(k), D_Lvp(k),
     &             IPH(k),D_Klv(k),D_Llv(k),D_Def(k,2), D_nno(k)

           ENDDO

         ELSE ! second run, soft rotor coll level file exists !

           DO k = 1, ND_nlv 
             READ (97,'(A80)',END=290,ERR=290) ch_iuf      
             WRITE (96,'(A80)') ch_iuf
             WRITE (8 ,'(A80)') ch_iuf
           ENDDO

         ENDIF
  290    CLOSE(32)
         CLOSE (96)
         CLOSE (97)

C        PAUSE 'Joining of soft-rotor COLL finished'

C        iwin = ipipe_copy('COLL.DAT','TARGET_COLL.DAT')
         iwin = ipipe_move('COLL.DAT','TARGET_COLL.DAT')
C
C--------JOIN finished
C

      ENDIF

      IF (IMOdel.EQ.4 .AND. (.NOT.coll_defined)) THEN
C
C--------model = 'coupled-channels rigid+soft rotor'
C
         coll_defined = .TRUE.
C        WRITE (8,*)

         IF (NISotop.EQ.0 .AND. FIRst_ein) THEN
            WRITE (8,*)'ERROR: Rigid+soft rotor CC potential selected'
            WRITE (8,*)'ERROR: but no isotopes defined for this OMP '
            WRITE (8,*)'ERROR: Error in RIPL database.                '
            WRITE (8,*)'ERROR: Report RIPL OMP to r.capotenoy@iaea.org'
            WRITE (8,*)'WARNING: Default collective levels will be used'
            GOTO 300
         ENDIF
         ncalc = 0
         DO n = 1, NISotop
            IF (iainp.EQ.IA(n) .AND. izinp.EQ.IZ(n)) ncalc = n
         ENDDO
         IF (ncalc.EQ.0) THEN
            WRITE (8,*)'WARNING: Target nucleus not listed in RIPL OMP'
            WRITE (8,*)'WARNING: Default collective levels will be used'
            GOTO 300
         ENDIF
         IF (NCOll(ncalc).EQ.0) THEN
            WRITE (8,*)
     &            'ERROR: RIPL CC rigid+soft rotor NCOll(target) = 0 '
            WRITE (8,*)'ERROR: Error in RIPL database.                '
            WRITE (8,*)'ERROR: Report RIPL OMP to r.capotenoy@iaea.org '
            WRITE (8,*)'WARNING: Default collective levels will be used'
            GOTO 300
         ENDIF

         IF (ND_nlv .LT. NCOll(ncalc)) THEN
             WRITE (8,*)
     &       'ERROR: Number of collective levels < RIPL CC '
            WRITE (8,*)
     &       'ERROR: Delete ECDWBA from input and the collective'
            WRITE (8,*)
     &       'ERROR: level file *-lev.col, and rerun'
            STOP 'ERROR: see the long output (*.lst)'
         ENDIF

C--------Setting EMPIRE global variables
         nld_cc = 1
         DO k = 2, ND_nlv
            IF (ICOllev(k).LT.LEVcc) nld_cc = nld_cc + 1
         ENDDO

         IF (nld_cc.NE.NCOll(ncalc)) THEN
            WRITE (8,*) 
     &  'WARNING: Default number of coupled levels ', nld_cc
            WRITE (8,*) 
     &  'WARNING: is not equal ', NCOll(ncalc),' defined in RIPL OMP'
            WRITE (8,*) 'WARNING: RIPL number of coupled channels used'
         ENDIF
C        WRITE (8,*) 
C
C        Rigid-soft rotor model
C
C        Correcting CC energies of the OMP collective levels EXV 
C
         OPEN (32, FILE = 'TARGET.LEV',STATUS = 'UNKNOWN',ERR=1061)
         DO n=2,NCOll(ncalc)
            REWIND(32)
            READ (32,'(A80)',END=1061,ERR=1061) ch_iuf
            DO ilv = 1, NLV(nnuc)
              READ (32,'(I3,1X,F10.6,1X,F5.1,I3,1X,E10.2,I3)'
     &              ,END=1061,ERR=1061) 
     &          itmp, elvr, xjlvr, lvpr, t12, ndbrlin
              DO nbr = 1, ndbrlin
                READ (32,'(A1)',END=1061,ERR=1061) dum
              ENDDO
              if( abs(elvr-EXV(n,ncalc)).le.0.001    .and. 
     &            abs(xjlvr-SPInv(n,ncalc)).le.0.005 .and.
     &           iabs(lvpr-IPArv(n,ncalc)).eq. 0 ) then
                   EXV(n,ncalc) = elvr
                   exit 
              endif 
            ENDDO
         ENDDO
 1061    CLOSE(32)  
C
C        Correcting Default Collective Levels using 
C           corrected CC energies from the RIPL OMP
C
         IPH(1)   = SR_ntu(1,ncalc)
         D_Klv(1) = SR_nnb(1,ncalc)
         D_Llv(1) = SR_nng(1,ncalc)
         D_nno(1) = SR_nno(1,ncalc)

         nld_cc = 1
         DO n=2,NCOll(ncalc)
           DO ilv=2,ND_nlv
              if( abs(D_Elv(ilv)-EXV(n,ncalc)).le.0.001  .and. 
     &          abs(D_Xjlv(ilv)-SPInv(n,ncalc)).le.0.005 .and.
     &          iabs(NINT(D_Lvp(ilv))-IPArv(n,ncalc)).le.0.005 ) then

                  nld_cc = nld_cc + 1

                  IF (ICOllev(ilv).GT.LEVcc) 
     &              ICOllev(ilv) = ICOllev(ilv) - LEVcc

                  D_Elv(ilv) = EXV(n,ncalc) 
                  
                  IPH(ilv)   = SR_ntu(n,ncalc)
                  D_Klv(ilv) = SR_nnb(n,ncalc)
                  D_Llv(ilv) = SR_nng(n,ncalc)
                  D_nno(ilv) = SR_nno(n,ncalc)

                  D_Def(ilv,2) = DEFv(n,ncalc)

                  exit 
              endif 
            ENDDO
         ENDDO
       
C        do k=1,NCOll(n)
C           read(ko,*) EXV(k,n),SPInv(k,n),IPArv(k,n),
C    +         SR_ntu(k,n),SR_nnb(k,n),SR_nng(k,n),SR_nno(k,n),DEFv(k,n)
C        enddo

C
C--------Putting Coupled levels first
         DO i = 2, ND_nlv
            DO j = i + 1, ND_nlv
               IF (ICOllev(j).LT.ICOllev(i)) THEN
C-----------------swapping
                  itmp = ICOllev(i)
                  ICOllev(i) = ICOllev(j)
                  ICOllev(j) = itmp

                  dtmp = D_Elv(i)
                  D_Elv(i) = D_Elv(j)
                  D_Elv(j) = dtmp

                  dtmp = D_Lvp(i)
                  D_Lvp(i) = D_Lvp(j)
                  D_Lvp(j) = dtmp

                  dtmp = D_Xjlv(i)
                  D_Xjlv(i) = D_Xjlv(j)
                  D_Xjlv(j) = dtmp

                  dtmp = D_Def(i,2)
                  D_Def(i,2) = D_Def(j,2)
                  D_Def(j,2) = dtmp

                  itmp = IPH(i)
                  IPH(i) = IPH(j)
                  IPH(j) = itmp

                  dtmp = D_Klv(i)
                  D_Klv(i) = D_Klv(j)
                  D_Klv(j) = dtmp

                  dtmp = D_Llv(i)
                  D_Llv(i) = D_Llv(j)
                  D_Llv(j) = dtmp

                  dtmp = D_nno(i)
                  D_nno(i) = D_nno(j)
                  D_nno(j) = dtmp
               ENDIF
            ENDDO
         ENDDO

         WRITE (8,*) 
         WRITE (8,*) 'Deformation of the gsb adopted from CC RIPL OMP'
         WRITE (8,*) 
         LMAxcc = LMAx(ncalc)
         IDEfcc = IDEf(ncalc)
         DO k = 2, IDEfcc, 2
            D_Def(1,k) = DDEf(ncalc,k)
         ENDDO
C
C--------Joining TARGET_COLL.DAT and TARGET_COLL_RIPL.DAT files
C
         OPEN (32,FILE = 'TARGET_COLL_RIPL.DAT')
         OPEN (97,FILE = 'TARGET_COLL.DAT')
         OPEN (96,FILE = 'COLL.DAT')
         READ (97,*)                    ! FIRST LINE

         WRITE (8,*)
         WRITE (8,*)
     &      'Collective levels from RIPL CC OMP, rigid+soft rotor model'
         WRITE (32,*)
     &      'Collective levels from RIPL CC OMP, rigid+soft rotor model'
         WRITE (96,*) 
     &'Collective levels: RIPL CC OMP + default, rigid+soft rotor model'
C
         READ (97,'(A80)') ch_iuf       ! 2ND LINE
         WRITE (8,'(A80)') ch_iuf
         WRITE (32,'(A80)') ch_iuf
         WRITE (96,'(A80)') ch_iuf
C
         READ (97,'(A80)') ch_iuf       ! 3ER LINE, model type  
         ldynamical = .FALSE.
         if(ch_iuf(36:39).eq.'dyna') ldynamical = .TRUE.

         SOFT     = .TRUE.
         DYNam    = .TRUE.
         DEFormed = .FALSE.

         WRITE ( 8,'(1x,i3,1x,i3,1x,a46)') izinp, iainp,
     &      ' nucleus is treated as dynamically deformed'
         WRITE (32,'(1x,i3,1x,i3,1x,a46)') izinp, iainp,
     &      ' nucleus is treated as dynamically deformed'
         WRITE (96,'(1x,i3,1x,i3,1x,a46)') izinp, iainp,
     &      ' nucleus is treated as dynamically deformed'

         READ (97,*)                    ! EMPTY LINE
         WRITE (8,*) 
         WRITE (32,*) 
         WRITE (96,*) 
C
         READ (97,*) 
         WRITE (8,*) '   Ncoll  Lmax IDef  Kgs  (Def(1,j),j=2,IDef,2)'
         WRITE (32,*)'   Ncoll  Lmax IDef  Kgs  (Def(1,j),j=2,IDef,2)'
         WRITE (96,*)'   Ncoll  Lmax IDef  Kgs  (Def(1,j),j=2,IDef,2)'
         READ (97,*) 

         if(.NOT.ldynamical) then
C           first run with default TARGET_COLL.DAT

           WRITE (8,'(3x,3I5,1x,F5.1,1x,6(e10.3,1x))') ND_nlv,
     &                LMAx(ncalc), IDEf(ncalc), BANdk(ncalc),
     &                (DDEf(ncalc,k),k = 2,IDEf(ncalc),2)
           WRITE (32,'(3x,3I5,1x,F5.1,1x,6(e10.3,1x))') NCOll(ncalc),
     &                LMAx(ncalc), IDEf(ncalc), BANdk(ncalc),
     &                (DDEf(ncalc,k),k = 2,IDEf(ncalc),2)
           WRITE (96,'(3x,3I5,1x,F5.1,1x,6(e10.3,1x))') ND_nlv,
     &                LMAx(ncalc), IDEf(ncalc), BANdk(ncalc),
     &                (DDEf(ncalc,k),k = 2,IDEf(ncalc),2)
           READ (97,*) 
           WRITE (8,*)
           WRITE (32,*)
           WRITE (96,*)
           READ (97,*) 
           WRITE (8 ,*)' N   E[MeV]  J   pi 2*K Nc Nb  Dyn.Def.'
           WRITE (32,*)' N   E[MeV]  J   pi 2*K Nc Nb  Dyn.Def.'
           WRITE (96,*)' N   E[MeV]  J   pi 2*K Nc Nb  Dyn.Def.'

           DO k = 1, ND_nlv

             READ (97,'(A80)',END=1001,ERR=1001) ch_iuf        

             IF (ICOllev(k).GE.LEVcc .and. D_Def(k,2).le.0.d0) 
     &          D_Def(k,2) = 0.005

             IF (ICOllev(k).LT.LEVcc) WRITE (32,
     &        '(1x,I2,1x,F7.4,1x,F4.1,1x,F3.0,1x,3(I2,1x),e10.3)')
     &             ICOllev(k), D_Elv(k), D_Xjlv(k), D_Lvp(k),
     &             IPH(k),D_Klv(k),D_Llv(k),D_Def(k,2)

             WRITE (8,
     &        '(1x,I2,1x,F7.4,1x,F4.1,1x,F3.0,1x,3(I2,1x),e10.3)')
     &             ICOllev(k), D_Elv(k), D_Xjlv(k), D_Lvp(k),
     &             IPH(k),D_Klv(k),D_Llv(k),D_Def(k,2)

             WRITE (96,
     &        '(1x,I2,1x,F7.4,1x,F4.1,1x,F3.0,1x,3(I2,1x),e10.3)')
     &             ICOllev(k), D_Elv(k), D_Xjlv(k), D_Lvp(k),
     &             IPH(k),D_Klv(k),D_Llv(k),D_Def(k,2)

           ENDDO

         ELSE ! second run, rigid-soft rotor coll level file exists !

           DO k = 1, ND_nlv 
             READ (97,'(A80)',END=1001,ERR=1001) ch_iuf      
             WRITE (96,'(A80)') ch_iuf
             WRITE (8 ,'(A80)') ch_iuf
           ENDDO

         ENDIF
 1001    CLOSE (96)
         CLOSE (32)
         CLOSE (97)
C        PAUSE 'Joining of soft-rigid rotor COLL finished'

C        iwin = ipipe_copy('COLL.DAT','TARGET_COLL.DAT')
         iwin = ipipe_move('COLL.DAT','TARGET_COLL.DAT')
C
C--------JOIN finished
C
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
      xmas_nejc = EJMass(Nejc)
      xmas_nnuc = AMAss(Nnuc)
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
      k=MOdelcc

      IF(KTRlom(Nejc,Nnuc).NE.9602) then
        CALL OPTMOD(E,vlib,rlib,alib)
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
      VOM(Nejc,Nnuc) = vlib(1)*FNvvomp(Nejc,Nnuc)
      RVOm(Nejc,Nnuc) = rlib(1)*FNrvomp(Nejc,Nnuc)
      AVOm(Nejc,Nnuc) = alib(1)*FNavomp(Nejc,Nnuc)
C-----Volume imaginary potential: Woods-Saxon
      WOMv(Nejc,Nnuc) = vlib(2)*FNwvomp(Nejc,Nnuc)
      RWOmv(Nejc,Nnuc) = rlib(2)*FNrwvomp(Nejc,Nnuc)
      AWOmv(Nejc,Nnuc) = alib(2)*FNavomp(Nejc,Nnuc)
C-----Real surface contribution
      VOMs(Nejc,Nnuc) = vlib(3)*FNwsomp(Nejc,Nnuc)
C-----Surface imaginary potential:
      WOMs(Nejc,Nnuc) = vlib(4)*FNwsomp(Nejc,Nnuc)
      RWOm(Nejc,Nnuc) = rlib(4)*FNrsomp(Nejc,Nnuc)
      AWOm(Nejc,Nnuc) = alib(4)*FNasomp(Nejc,Nnuc)
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
      implicit none 
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
      implicit none 
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
      implicit none 
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
C
C        Reading rigid rotor model parameters    
C
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
C
C        Reading vibrational model parameters    
C
         READ (Ko,99020,ERR = 200) NISotop
         DO n = 1, NISotop
            READ (Ko,99020,ERR = 200) IZ(n), IA(n), NVIb(n)
            DO k = 1, NVIb(n)
               READ (Ko,99045,ERR = 200) EXV(k,n), SPInv(k,n),
     &               IPArv(k,n), NPH(k,n), DEFv(k,n), THEtm(k,n)
            ENDDO
         ENDDO
      ELSEIF (IMOdel.EQ.3) THEN
C
C        Reading soft rotor model parameters    
C
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
              read(ko,99049) EXV(k,n),SPInv(k,n),IPArv(k,n),
     +               SR_ntu(k,n),SR_nnb(k,n),SR_nng(k,n),SR_nno(k,n)
            enddo

         ENDDO

      ELSEIF (IMOdel.EQ.4) THEN
C
C        Reading rigid-soft rotor model parameters    
C
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

            do k=1,NCOll(n)
              read(ko,*) EXV(k,n),SPInv(k,n),IPArv(k,n),
     +         SR_ntu(k,n),SR_nnb(k,n),SR_nng(k,n),SR_nno(k,n),DEFv(k,n)
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
99052 FORMAT (f12.8,f7.1,I4,4I2,1x,e11.4)
99050 FORMAT (80A1)
99051 FORMAT (f12.8,1x,f12.8,1x,f7.1,I4,4I2)
99053 FORMAT (f12.8,1x,f12.8,1x,f7.1,I4,4I2,1x,e11.4)
      END


      SUBROUTINE OMIN(Ki,Ieof,Irelout)
      implicit none 
C
C     routine to read optical model parameters
C
      INCLUDE 'ripl2empire.h'
C
C Dummy arguments
C
      INTEGER Ki,Ieof,Irelout
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
              read(ki,*) EXV(k,n), SPInv(k,n), IPArv(k,n),
     +               SR_ntu(k,n),SR_nnb(k,n),SR_nng(k,n),SR_nno(k,n)
            enddo
         ENDDO
      ELSEIF (IMOdel.EQ.4) THEN
         READ (Ki,*) NISotop
         DO n = 1, NISotop
            READ (Ki,*) IZ(n), IA(n), NCOll(n), LMAx(n), IDEf(n),
     &                  BANdk(n), (DEF(n,k),k = 2,IDEf(n),2)

            do k=1,NCOll(n)
              read(ki,*) EXV(k,n), SPInv(k,n), IPArv(k,n),
     +         SR_ntu(k,n),SR_nnb(k,n),SR_nng(k,n),SR_nno(k,n),DEFv(k,n)
            enddo
         ENDDO
      ENDIF

  90  READ (Ki,99005,END = 100) idum
      RETURN
  100 Ieof = 1
99005 FORMAT (80A1)
99049 FORMAT (f12.8,f7.1,I4,4I2)
99051 FORMAT (f12.8,1x,f12.8,1x,f7.1,I4,4I2)
      END

      SUBROUTINE SUMPRT(Ko)
C
C     print summary information from RIPL formatted library
C
      implicit none 
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
      IF (IMOdel.EQ.4) model = 'coupled-channels rigid+soft rotor'
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
      implicit none 
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
      implicit none 
      INCLUDE 'dimension.h'
      INCLUDE 'global.h'
C
C Dummy arguments
C
      INTEGER Nejc, Nnuc
      LOGICAL Nonzero
C
C Local variables
C
      INTEGER i, l, k, myalloc
      DOUBLE PRECISION, ALLOCATABLE :: ttll(:,:),ttllj(:,:,:)
      INTEGER, ALLOCATABLE :: maxl(:)
      DOUBLE PRECISION ftmp

      Nonzero = .FALSE.
      IF (SEJc(Nejc).GT.1.0D0) THEN
         WRITE (8,
     &'('' SPIN OF EJECTILE A='',I3,'' AND Z='',I2,'' IS '',    F4.1,/,'
     &' SUCH A LARGE VALUE IS NOT ALLOWED IN TRANSMISSION COEFFICIENT CA
     &LCULATIONS'',/,'' EXECUTION S T O P P E D '')') INT(AEJc(Nejc)),
     &INT(ZEJc(Nejc)), SEJc(Nejc)
         WRITE (8,*) 'ERROR: TOO LARGE SPIN OF EJECTILE'
         STOP        'ERROR: TOO LARGE SPIN OF EJECTILE'
      ENDIF

C     allocate MAXl(), TTLl(), TTLlj() 
      ALLOCATE(maxl(NDETL),ttll(NDETL,NDLW),ttllj(NDETL,NDLW,3),
     &   STAT=myalloc)
      IF(myalloc.NE.0) THEN
        WRITE(8,* ) ' ERROR: Insufficient memory for TLEVAL (tl.f)'
        WRITE(12,*) ' ERROR: Insufficient memory for TLEVAL (tl.f)'
        STOP        ' ERROR: Insufficient memory for TLEVAL (tl.f)'
      ENDIF
C     maxl  = 0
C     ttll  = 0.d0
C     ttllj = 0.d0
C
C-----TL trans. coeff. at zero energy must be zero
C
      DO i = 1, NDETL
         LMAxtl(i,Nejc,Nnuc) = 0
         DO l = 1, NDLW
            TL(i,l,Nejc,Nnuc) = 0.D0
            IF(Nnuc.eq.NREs(Nejc)) then      
              DO k=1,MAXj(Nejc)
                TLJ(i,l,k,Nejc) = 0.D0
              ENDDO 
            ENDIF
         ENDDO
      ENDDO
C-----TARGET NUCLEUS (ELASTIC CHANNEL), incident neutron or proton
C-----Transmission coefficient matrix for elastic channel
C-----ECIS code is used
C-----KTRlom(Nejc,Nnuc)=KTRompCC
      IWArn = 0
C-----Preparing INPUT and RUNNING ECIS
C-----(or reading already calculated file)
      CALL TRANSINP(Nejc,Nnuc,NDETL,maxl,ttll,ttllj)
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

C-----transfer of the calculated transmission coeff. onto TL & TLJ matrices
      DO i = 2, NDETL
         LMAxtl(i,Nejc,Nnuc) = MIN(maxl(i),NDLW-1)
         DO l = 0, LMAxtl(i,Nejc,Nnuc)
            ftmp = ttll(i,l+1)
            IF (ftmp.LT.1.D-16) cycle
            TL(i,l+1,Nejc,Nnuc) = ftmp
            Nonzero = .TRUE.
         ENDDO
         IF(Nnuc.eq.NREs(nejc)) then      
           DO l = 0, LMAxtl(i,Nejc,Nnuc)
              DO k=1,MAXj(Nejc)
                ftmp = ttllj(i,l+1,k)
                IF (ftmp.LT.1.D-16) cycle
                TLJ(i,l+1,k,Nejc) = ftmp
              ENDDO 
           ENDDO
         ENDIF
      ENDDO
C     deallocate ttll,ttllj,maxl
      if(allocated(maxl))  deallocate(maxl)
      if(allocated(ttll))  deallocate(ttll)
      if(allocated(ttllj)) deallocate(ttllj)
      RETURN
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
      implicit none
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
      SUBROUTINE TRANSINP(Nejc,Nnuc,Nen,Maxl,Ttll,Ttllj)
      implicit none
      INCLUDE "dimension.h"
      INCLUDE "global.h"
C
C Dummy arguments
C
      INTEGER Nejc, Nen, Nnuc
      INTEGER Maxl(NDETL)
      DOUBLE PRECISION Ttll(NDETL,NDLW),Ttllj(NDETL,NDLW,3)
C
C Local variables
C
      CHARACTER*3 ctldir
      CHARACTER*23 ctmp23
      DOUBLE PRECISION culbar, ener
      LOGICAL fexist, ltmp, logtmp, fexistj
      INTEGER i, ilv, ien, ien_beg, l, lmax, jindex
C     --------------------------------------------------------------------
C     | Calculation of transmission coefficients using ECIS              |
C     |                for EMPIRE energy grid                            |
C     --------------------------------------------------------------------
      DATA ctldir/'TL/'/

C-----data initialization
      CALL INIT(Nejc,Nnuc)
C-----Cleaning transmission coefficient matrices
      Maxl  = 0
      Ttll  = 0.d0
      Ttllj = 0.d0

      culbar = 0.8*ZEJc(Nejc)*Z(Nnuc)/(1 + A(Nnuc)**0.6666)
      DO i = 2, Nen
         IF (ETL(i,Nejc,Nnuc).GT.culbar) GOTO 250
      ENDDO
C     For this residual nucleus available energy is always 
C     below Coulomb barrier; Calculations are not needed   
      RETURN 
C
C-----This part prompts for the name of a data file. The INQUIRE
C-----statement then determines whether or not the file exists.
C-----If it does not, the program calculates new transmission coeff.
 250  WRITE (ctmp23,'(i3.3,i3.3,1h_,i3.3,i3.3,1h_,i9.9)')
     &       INT(ZEJc(Nejc)), INT(AEJc(Nejc)), INT(Z(Nnuc)),
     &       INT(A(Nnuc)), INT(EINl*1000000)
      INQUIRE (FILE = (ctldir//ctmp23//'.BIN'),EXIST = fexist)
      INQUIRE (FILE = (ctldir//ctmp23//'J.BIN'),EXIST = fexistj)
      IF (.NOT.fexist) GOTO 400
C-----Here the previously calculated files should be read
      OPEN (45 ,FILE = (ctldir//ctmp23//'.BIN'),FORM = 'UNFORMATTED')
      if(fexistj) 
     &  OPEN (451,FILE = (ctldir//ctmp23//'J.BIN'),FORM = 'UNFORMATTED')
      IF (IOUt.EQ.5) OPEN (46,FILE = ctldir//ctmp23//'.LST')
  100 READ (45 ,END = 200) lmax, ien, ener, IRElat(Nejc,Nnuc)
      if(fexistj)READ (451,END = 200) lmax, ien, ener, IRElat(Nejc,Nnuc)
      IF (IOUt.EQ.5) WRITE (46,'(A5,2I6,E12.6)') 'LMAX:',lmax,ien,ener
C
C-----If energy read from the file does not coincide
C-----this nucleus should be recalculated (goto 300)
C
      IF (ABS(ener - ETL(ien,Nejc,Nnuc)).GT.0.0001) THEN
         CLOSE (45 ,STATUS = 'DELETE')
         if(fexistj) CLOSE (451,STATUS = 'DELETE')
         IF (IOUt.EQ.5) CLOSE (46,STATUS = 'DELETE')
         IF (IOUt.EQ.5) THEN
           WRITE (8,*)
     &       ' WARNING: ENERGY MISMATCH: ETL(ien=', ien, '...)=',
     &       ETL(ien,Nejc,Nnuc), ' REQUESTED ENERGY=', SNGL(ener)
           WRITE (8,*)
     &       ' WARNING: FILES WITH TRANSM. COEFF. HAVE BEEN DELETED'
         ENDIF
         GOTO 400
      ENDIF
      ETL(ien,Nejc,Nnuc) = ener
      Maxl(ien) = lmax					   
      DO l = 0, lmax
         READ (45 ,END= 300,ERR=300) Ttll(ien,l+1)
         if(fexistj) READ (451,END= 300,ERR=300)
     &        (Ttllj(ien,l+1,jindex), jindex=1,MAXj(Nejc))
         IF (IOUt.EQ.5) then
            WRITE (46,'(2x,I3,3(3x,D15.8))') l, Ttll(ien,l+1)
            if(fexistj) WRITE (46,'(2x,3x,3(3x,D15.8))') 
     &        (Ttllj(ien,l+1,jindex), jindex=1,MAXj(Nejc))
         ENDIF
      ENDDO
      READ (45,END = 300) SIGabs(ien,Nejc,Nnuc)
      GOTO 100
  200 CLOSE (45 )
      CLOSE (451) 
      IF (IOUt.EQ.5) CLOSE (46)
      IF (IOUt.GT.4) THEN
        WRITE (8,*)
     & 'Transmission coefficients Tl and Tljs for outgoing channel read 
     &from files: '
        WRITE (8,*) ctldir//ctmp23//'.BIN'
        if(fexistj) WRITE (8,*) ctldir//ctmp23//'J.BIN'
      ENDIF
      RETURN

  300 CLOSE (45 ,STATUS = 'DELETE')
      if(fexistj) CLOSE (451,STATUS = 'DELETE')
      IF (IOUt.EQ.5) CLOSE (46,STATUS = 'DELETE')
      Maxl  = 0
      Ttll  = 0.d0
      Ttllj = 0.d0
      WRITE (8,*) ' WARNING: ERROR WHEN READING TLs in ', ctmp23
      IF (IOUt.GT.1) THEN
         WRITE (8,*)
     &      ' WARNING: FILES WITH TRANSM. COEFFS. HAVE BEEN DELETED'
         WRITE (8,*)
     &      ' WARNING: TRANSM. COEFFS. WILL BE CALCULATED AND STORED'
      ENDIF
C-----Coulomb barrier (somewhat decreased) setting lower energy limit
C-----for transmission coefficient calculations
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
C--------Running ECIS or OPTMAN
C
         IF (IOUt.GT.1 .AND. FIRST_ein) THEN
            IF (A(Nnuc).EQ.A(0) .AND. Z(Nnuc).EQ.Z(0)) THEN
              WRITE (8,*)
              IF (DIRect.EQ.2 .AND. AEJc(Nejc).LE.1) THEN 
               WRITE (8,*) ' CC transmission coefficients used for the',
     &                     ' inelastic outgoing channel'
              ELSE
               WRITE (8,*) ' Spherical OM transmission coefficients',
     &                     ' used for the inelastic outgoing channel'
              ENDIF 
            ENDIF
         ENDIF
C--------OPEN Unit=46 for Tl output
         OPEN (UNIT = 46 ,STATUS = 'unknown',
     &         FILE = (ctldir//ctmp23//'.BIN'),FORM = 'UNFORMATTED')
         OPEN (UNIT = 461,STATUS = 'unknown',
     &         FILE = (ctldir//ctmp23//'J.BIN'),FORM = 'UNFORMATTED')
C
C--------do loop over energy
C
         ltmp = A(Nnuc).EQ.A(0) .AND. Z(Nnuc).EQ.Z(0) .AND.
     &          DIRect.EQ.2 .AND. AEJc(Nejc).LE.1

C        saving the input value of the key CN_isotropic
         logtmp = CN_isotropic  
C        all OMP calculations calculate only the direct component (no CN)
         CN_isotropic = .TRUE.     
     
         DO i = Nen ,ien_beg, -1

            ener = ETL(i,Nejc,Nnuc)
            IF (ener.LE.0.1D-6) CYCLE

            IF (.NOT.(ltmp)) THEN
C
C--------------Spherical optical model is assumed, only one level (gs)
C
               CALL ECIS_CCVIB(Nejc,Nnuc,ener,.TRUE.,0,.TRUE.)
               CALL ECIS2EMPIRE_TR(Nejc,Nnuc,i,.TRUE.,Maxl,Ttll,Ttllj)
C--------------Transmission coefficient matrix for incident channel
C--------------is calculated (DIRECT = 2 (CCM)) using ECIS code.
C--------------Only coupled levels are considered
            ELSEIF (DEFormed) THEN
C--------------CC rotational calculation (ECIS)
               CALL ECIS_CCVIBROT(Nejc,Nnuc,ener,.TRUE.)
               CALL ECIS2EMPIRE_TR(Nejc,Nnuc,i,.FALSE.,Maxl,Ttll,Ttllj)
            ELSE
C--------------CC vibrational calculation (OPTMAN or ECIS) 
               IF (SOFt) THEN
C----------------OPTMAN CC calc. (only coupled levels)
                 CALL OPTMAN_CCSOFTROT(Nejc,Nnuc,ener,.TRUE.) 
               ELSE
C----------------ECIS   CC calc. (only coupled levels)
                 CALL ECIS_CCVIB(Nejc,Nnuc,ener,.FALSE., -1,.TRUE.)
               ENDIF
               CALL ECIS2EMPIRE_TR(Nejc,Nnuc,i,.TRUE.,Maxl,Ttll,Ttllj)
            ENDIF

         ENDDO

C        restoring the input value of the key CN_isotropic
         CN_isotropic = logtmp

         CLOSE (46 )
         CLOSE (461)
         IF (IOUT.GT.4) THEN
	     WRITE (8,*) ' Transm. coeff. Tl  written to file:',
     &                              (ctldir//ctmp23//'.BIN')
	     WRITE (8,*) ' Transm. coeff. Tlj written to file:',
     &                              (ctldir//ctmp23//'J.BIN')
         ENDIF   
      ELSEIF (IOUt.EQ.5) THEN
         WRITE (8,'(1x,A12,I3,A3,I3,A3,F4.1)') 'EJECTILE: A=',
     &          INT(AEJc(Nejc)), ' Z=', INT(ZEJc(Nejc)), ' S=',
     &          SEJc(Nejc)
         ilv = 1
         If(Nnuc.eq.0) ilv = Levtarg
         WRITE (8,'(1x,A12,I3,A3,I3,A3,F4.1,A3,I2)') 'RESIDUAL: A=',
     &          INT(A(Nnuc)), ' Z=', INT(Z(Nnuc)), ' S=',
     &          SNGL(XJLv(ilv,Nnuc)), ' P=', INT(LVP(ilv,Nnuc))
         WRITE (8,*) ' WARNING: For this residual nucleus'
         WRITE (8,*) ' WARNING: available energy is always '
         WRITE (8,*) ' WARNING: below Coulomb barrier'
         WRITE (8,*) ' WARNING: Calculations are not needed!'
         WRITE (8,*)
      ENDIF
      RETURN
      END

      SUBROUTINE ECIS2EMPIRE_TL_TRG(
     &  Nejc,Nnuc,Maxlw,Stl,Stlj,Sel,Lvibrat)
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
C     OUTPUT: Maxlw and Stl(1:Maxlw+1) are the maximum angular momentum and Tls
C             SINl is the total inelastic cross section
C             Selast is the shape elastic cross section
C             Sel(L) contains the shape elastic cross section for a given orbital momentum L
C             Stl(L) contains the transmission coefficient Tl(L) for a given orbital momentum L
C             Stlj(L,j) contains the transmission coefficient Tlj(L,s) for a given orbital momentum L and spin S
C             J = L + s (vectorial sum) 
C
C
      implicit none
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
      DOUBLE PRECISION Stl(NDLW),Stlj(NDLW,3),Sel(NDLW)
C
C Local variables
C
      DOUBLE PRECISION ak2,dtmp,ecms,elab,jj,sabs,sabsj,sreac,jc,
     & xmas_nejc,xmas_nnuc,selast,sreal,simag,stmp,snorm,ftmp
      DOUBLE PRECISION xsabs,xsabsj,jsp, coeff
      INTEGER ilv, l, nc, nceq, ncoll, nlev, nc1, nc2
      INTEGER nctot, ncsol, ncint, jindex
      LOGICAL relcal, unformat
      CHARACTER*1 parc
      DOUBLE PRECISION sjf
	sjf(l,jindex,stmp)= l - 1 + jindex - stmp

      data unformat/.TRUE./ 

      Maxlw  = 0
      ncoll = 0
      Stl  = 0.d0
      Sel  = 0.d0
      Stlj = 0.d0
      CSFus   = 0.D0
C
C-----Estimating needed coefficient for calculation of the 
C     absorption cross section from obtained TLs
C
      xmas_nejc = EJMass(Nejc)
      xmas_nnuc = AMAss(Nnuc)
      elab = EINl
      relcal = .FALSE.
      IF (IRElat(Nejc,Nnuc).GT.0 .OR. RELkin) relcal = .TRUE.
      CALL KINEMA(elab,ecms,xmas_nejc,xmas_nnuc,ak2,1,relcal)
      coeff = 10.d0*PI/ak2

      ilv = 1
      If(Nnuc.eq.0) ilv = Levtarg
C------------------------------------------
C-----| Input of transmission coefficients|
C------------------------------------------
C-----Opening ecis03 output file containing Smatrix
      OPEN(UNIT = 45, STATUS = 'old', FILE = 'ecis06.smat', ERR=90)
      READ (45,*,END = 90)   ! To skip first line <SMATRIX> ..
   80 READ (45,'(1x,f9.1,4x,a1,2(1x,i4))',END = 90) 
     &     jc, parc, nceq, nctot ! ecis06
C     jc,parc are the channel spin and parity
C     nceq is the total number of coupled equations
C     ncin is the number of independent coupled equations
      ncsol=nctot/nceq
C     Loop over the number of coupled equations
      do ncint=1,ncsol
        do nc=1,nceq
C       Reading the coupled level number nlev, the orbital momentum L,
C           angular momentum jj and Transmission coefficient Tlj,c
C       (nlev=LEVtarg corresponds to the target state)
        read (45, ! ecis06 (Dec 2008)               
     &  '(1X,3(I3,1X),I3,1X,F5.1,1X,2(D15.7,1X),1x,4X,F11.8)'
     &  ,END=90,ERR=90) nc1,nc2,nlev,l,jj,sreal,simag,stmp

        IF (nlev.eq.ilv .and. nc1.eq.nc2
     &    .and. stmp.GT.1.D-15 .AND. L.LT.NDLW) then
C-----------Averaging over particle and target spin, summing over channel spin jc
C    &      stot = stot + (2*jj + 1)*(1.d0-sreal) ! /DBLE(2*L + 1)
            Sel(l+1) = Sel(l+1) +(2*jc + 1)*((1 - sreal)**2 + simag**2)
     &           /DBLE(2*L + 1)
     &           /DBLE(2*XJLv(1,Nnuc) + 1)
     &           /DBLE(2*SEJc(Nejc) + 1)
        ENDIF
        enddo
      enddo
      GOTO 80
   90 CLOSE(45)

      if(unformat) then
        OPEN (UNIT = 45,STATUS = 'old',FILE = 'INCIDENT.TLJ', ERR=200,
     1                                 form='unformatted') !-zvv-2013
      else
        OPEN (UNIT = 45,STATUS = 'old',FILE = 'INCIDENT.TLJ', ERR=200)
        READ (45,*,END = 200)   ! To skip first line <TLJs.> ..
      endif
C-----jc,parc are the channel spin and parity
C-----nceq is the number of coupled equations
  100 continue
      if(unformat) then
        READ (45,                        END = 200) jc, parc, nceq  ! ecis06
      else
        READ (45,'(1x,f9.1,4x,a1,1x,i4)',END = 200) jc, parc, nceq  ! ecis06
      endif    
C-----Loop over the number of coupled equations
      DO nc = 1, nceq
C--------Reading the coupled level number nlev, the orbital momentum L,
C--------angular momentum jj and Transmission coefficient Tlj,c
C--------(nlev=ilv corresponds to the target state)
         if(unformat) then
           READ (45,  END = 200,ERR = 200) nlev, l, jj, dtmp
         else
           READ (45,*,END = 200,ERR = 200) nlev, l, jj, dtmp
         endif
         if(dtmp.lt.1.d-20) cycle

         ncoll = MAX(nlev,ncoll)
C--------Selecting only target state LEVtarg
         IF (nlev.eq.ilv .AND. l.gt.NDLW) then
           WRITE (8,*)
     &     ' ERROR: ECIS angular momentum bigger than NDLW '
           WRITE (8,*)
     &     ' ERROR: Set NDLW in dimension.h to ',l
           WRITE (*,*)
     &     ' ERROR: Set NDLW in dimension.h to ',l
           STOP ' ERROR: ECIS angular momentum bigger than NDLW '
         ENDIF
         IF (nlev.eq.ilv) then
C-----------Averaging over target and particle spin, summing over channel spin jc
            Stl(l + 1) = Stl(l + 1) + 
     &                           (2*jc + 1)*dtmp/DBLE(2*l + 1)  
     &                           /DBLE(2*SEJc(Nejc) + 1)
     &                           /DBLE(2*XJLv(ilv,Nnuc) + 1)

C           It always contain the TRUE maximum L to be used in loops over L from 0 to Maxlw
            Maxlw = l
C
            jindex = 1 ! default, good for alphas
            if    (MAXj(Nejc) .eq. 2) then ! n,p,h,t
              if(jj.gt.l) jindex = 2
            elseif(MAXj(Nejc) .eq. 3) then ! d
              if(jj.eq.l) jindex = 2
              if(jj.gt.l) jindex = 3
            endif
C
C-----------Averaging over target and particle spin, summing over channel spin jc
            Stlj(l + 1,jindex) = Stlj(l + 1,jindex) + 
     &                           (2*jc + 1)*dtmp/DBLE(2*jj+1)   
     &                           /DBLE(2*XJLv(ilv,Nnuc) + 1)
C
C           Sreac = Sum_{L} Sum_{j} (2j+1)/(2s+1) Stlj(L,j)   
C           j goes from |L-s| to L+s
C           Note: the calculation with Tlj() implies dividing by (2s+1) externally
C                 this division is not contained in the definition of Tlj()
C
C           Sreac = Sum_{L} (2L+1) Stl(L)   
C
         ENDIF
      ENDDO
      GOTO 100
  200 CLOSE (45)
C-----For vibrational the Tls must be multiplied by (2*XJLv(ilv,Nnuc) + 1)
C     as the spin of the target nucleus is neglected for spherical and DWBA calcs
      IF (Lvibrat) THEN
        DO l = 0, Maxlw
           Stl(l + 1) = Stl(l + 1)*DBLE(2*XJLv(ilv,Nnuc) + 1)
           Sel(l + 1) = Sel(l + 1)*DBLE(2*XJLv(ilv,Nnuc) + 1)
           DO jindex = 1,MAXj(Nejc)
             Stlj(l + 1,jindex) = Stlj(l + 1,jindex)
     &                          *DBLE(2*XJLv(ilv,Nnuc) + 1)
           ENDDO
        ENDDO
      ENDIF

      TOTcs = 0.D0
      ABScs = 0.D0
      ELAcs = 0.D0
      SINl  = 0.D0
      SINlcc = 0.D0
      SINlcont = 0.D0

      OPEN (UNIT = 45,FILE = 'INCIDENT.CS',STATUS = 'old',ERR = 300)
      READ (45,*,END = 300)  ! Skipping first line
      IF (ZEJc(Nejc).EQ.0) READ (45,*,END=300) TOTcs
      READ (45,*,END=300) ABScs 
      IF (ZEJc(Nejc).EQ.0) READ (45,*,END=300) ELAcs
  300 CLOSE (45)

      IF (ABScs.LE.0.D0) RETURN

      sabs  = 0.D0
      sabsj = 0.D0
      selast= 0.D0
C-----Absorption and elastic cross sections in mb
      DO l = 0, Maxlw
        sabs   = sabs   + Stl(l + 1)*DBLE(2*l + 1)
        selast = selast + Sel(l + 1)*DBLE(2*l + 1)
        DO jindex = 1, MAXj(Nejc) 
          jsp = sjf(l,jindex,SEJc(Nejc)) 
          sabsj = sabsj +  DBLE(2*jsp+1)*Stlj(l + 1,jindex)
        ENDDO 
      ENDDO
      xsabs  = coeff*sabs
      xsabsj = coeff*sabsj/DBLE(2*SEJc(Nejc) + 1)
      selast = coeff*selast

      IF (xsabs.le.0.d0) RETURN

      CSFus = ABScs

      OPEN (UNIT = 45,FILE = 'INCIDENT.ICS',STATUS = 'old',ERR = 400)
      READ (45,*,END = 400)  ! Skipping first line
      DO l = 1, NDCOLLEV     ! number of inelastic level
         READ (45,*,END = 400) dtmp
         IF ( ICOller(l+1).LE.NLV(nnuc) ) THEN 
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
         ENDIF
      ENDDO
  400 CLOSE (45)

      IF (abs(xsabs + SINlcc - ABScs).gt.0.05*ABScs) THEN ! 5% difference check
         WRITE (8,*)
         WRITE (8,*)
     &     ' WARNING: ECIS ABScs absorption cross section ',ABScs
         WRITE (8,*)
     &     ' WARNING: Calc. sabs absorption cross section ',xsabs
         WRITE (8,*) ' WARNING: sabs + SINlcc < ECIS ABS'
         WRITE (8,*) ' WARNING: You may need to increase NDLW !!!'
         write (8,*) ' Lmax',Maxlw
         write (8,*) ' Calc. Sabs                  =',sngl(xsabs)
         write (8,*) ' ECIS  fusion (ABScs-SINlcc) = ',
     &                   sngl(ABScs-SINlcc)
         write (8,*) ' Calc. SINlcc =',sngl(SINlcc)
         write (8,*) ' ECIS  ABScs  =',sngl(ABScs)
         WRITE (8,*)
      ENDIF

      IF (SINl+SINlcc+SINlcont.EQ.0.D0) RETURN
C
C     ECIS convergence check
C
      IF (SINlcc.GT.ABScs) THEN
         WRITE (8,*)
         WRITE (8,*)
     &     ' ERROR: Coupled channel calculation does not converge '
         WRITE (8,'(
     &1x, '' ERROR: Coupled-channels cross section ='',F8.2,'' mb''/
     &1x, '' ERROR: ECIS absorption cross section  ='',F8.2,'' mb''/)') 
     &   SINlcc, ABScs
         WRITE (8,*)
     &    ' ERROR: Change the selected OMP or coupled-level scheme !! '
         WRITE (8,*)
     &    ' ERROR: Please send your case to r.capotenoy@iaea.org      '
         STOP 
     &    ' ERROR: Change the selected OMP or coupled-level scheme !! '
      ENDIF

      IF (SINl+SINlcc.GT.ABScs) THEN
         WRITE (8,*)
         WRITE (8,*) 
     &' ERROR: Too big dynam. deformations of uncoupled discrete levels'
         WRITE (8,*)
     &' ERROR: DWBA calculation produces too big cross sections '
         WRITE (8,'(
     &1x,'' ERROR: Coupled-channel cross sections                  ='',
     &  F8.2,'' mb''/
     &1x,'' ERROR: DWBA cross section to uncoupled discrete levels ='',
     &  F8.2,'' mb''/
     &1x,'' ERROR: CN formation cross section ='',F8.2,'' mb''/)') 
     &   SINlcc, SINl, ABScs-SINlcc-SINl
C        WRITE (8,*)
C    &' ERROR: EDIT the collective level file to decrease dynamical defo
C    &rmations of uncoupled DWBA levels'
C        WRITE (8,*)
C        STOP 
C    &' ERROR: EDIT the collective level file to decrease dynamical defo
C    &rmations of uncoupled DWBA levels'
      ENDIF

      IF (SINlcont+SINlcc.GT.ABScs) THEN
         WRITE (8,
     &'(1x,'' WARNING: reaction cross section renormalized to account fo
     &r DWBA calculated cross sections '', F8.5/
     &1x,'' WARNING: CC cross section to coupled discrete levels     =''
     &,F8.2,'' mb''/
     &1x,'' WARNING: DWBA cross section to uncoupled discrete levels =''
     &,F8.2,'' mb''/)')
     &  ABScs, SINlcc, SINl

         WRITE (8,*)
         WRITE (8,*) 
     &' ERROR: Too big dynam. deformations of DWBA levels in the continu
     &um.'
         WRITE (8,'(
     &1x,'' ERROR: Coupled-channel cross sections                  ='',
     &  F8.2,'' mb''/
     &1x,'' ERROR: DWBA cross section to the continuum             ='',
     &  F8.2,'' mb''/
     &1x,'' ERROR: CN formation cross section ='',F8.2,'' mb''/)') 
     &   SINlcc, SINlcont, ABScs-SINlcc
         WRITE (8,*)
     &' ERROR: EDIT the collective level file to decrease dynamical defo
     &rmations of DWBA levels in the continuum'
         WRITE (8,*)
         STOP 
     &' ERROR: EDIT the collective level file to decrease dynamical defo
     &rmations of DWBA levels in the continuum'
      ENDIF
C
C     Absorption cross section includes inelastic scattering cross section
C     to coupled levels, but not DWBA to uncoupled levels (This is an ECIS double check)
C
      sreac = xsabs  + SINlcc   

      IF (IOUt.EQ.5 .AND. sreac.GT.0.D0) THEN
         WRITE (8,*)
         WRITE (8,*) ' INCIDENT CHANNEL:'
         WRITE (8,'(A7,I6,A5,F10.3,A10,F10.3,A10)') '  LMAX:', Maxlw,
     &          ' E = ', EIN, ' MeV (CMS)', elab, ' MeV (LAB)'
C        WRITE (8,*) ' XS calculated using Smat:   Selast =',
C    &               SNGL(selast), ' mb '
         WRITE (8,*) ' Shape elastic XS =', SNGL(ELAcs),
     &               ' mb (read from ECIS)'
         WRITE (8,*) ' Reaction XS =', SNGL(sreac),
     &               ' mb (calculated)'
         WRITE (8,*) ' Reaction XS =', SNGL(ABScs),
     &               ' mb (read from ECIS)'
         WRITE (8,*) ' ECIS/EMPIRE ratio of reaction cross section =',
     &               ABScs/sreac
         WRITE (8,*) ' XS calculated using averaged Tls :  Sabs =',
     &               SNGL(xsabs), ' mb '
         WRITE (8,*) ' XS calculated using averaged Tljs:  SabsJ=',
     &               SNGL(xsabsj), ' mb '
         WRITE (8,*)
     &      ' Inelastic XS to uncoupled discrete levels (DWBA) =',
     &               SNGL(SINl), ' mb '
         WRITE (8,*) ' Inelastic XS to the continuum (sinlcont) =',
     &               SNGL(SINlcont), ' mb '
         IF (SINlcc.GT.0.D0) THEN
            WRITE (8,*)
     &               ' Inelastic XS to coupled discr. lev. (CC) =',
     &               SNGL(SINlcc), ' mb '
            WRITE (8,*) ' Sinl =', SNGL(ABScs), ' mb (read from ECIS)'
            WRITE (8,*) ' Sreac=', SNGL(sreac), ' mb (Sabs + SINlcc )'
         ENDIF
         WRITE (8,*) ' Total XS =', SNGL(TOTcs), 
     &               ' mb (read from ECIS)'
         WRITE (8,*) ' Shape Elastic XS =', SNGL(ELAcs),
     &               ' mb (read from ECIS)'
         WRITE (8,*)

      ENDIF
C
C-----Renormalizing TLs 
C     Discrete level inelastic scattering (not coupled levels) and DWBA to the
C     continuum also included
C
158   CONTINUE

      IF( xsabs.gt.0.d0 .and. xsabsj.gt.0.d0 .and. 
     >  ABScs.GT.(SINlcc+SINl+SINlcont) .and. 
     >  dabs(ABScs - sreac) .GT. 1.d-7) THEN 
        dtmp = 0.d0
        ftmp = 0.d0
        DO l = 0, Maxlw
	    snorm = (ABScs - SINlcc - SINl -SINlcont)
          Stl(l + 1) = Stl(l + 1)*snorm/xsabs
          dtmp   = dtmp + DBLE(2*l + 1)*Stl(l + 1)
          DO jindex = 1, MAXj(Nejc) 
            Stlj(l + 1,jindex) = Stlj(l + 1,jindex)*snorm/xsabsj
            jsp = sjf(l,jindex,SEJc(Nejc)) 
            ftmp   = ftmp + DBLE(2*jsp+1)*Stlj(l + 1,jindex)
          ENDDO 
        ENDDO
        CSFus = coeff*dtmp  ! = ABScs - SINlcc - SINl - SINlcont       
        WRITE (8,
     &'(1x,'' WARNING: Transmission coefficients renormalized to account
     & for DWBA calculated cross sections '', F8.5/
     &1x,'' WARNING: CC cross section to coupled discrete levels     =''
     &,F8.2,'' mb''/
     &1x,'' WARNING: DWBA cross section to uncoupled discrete levels =''
     &,F8.2,'' mb''/
     &1x,'' WARNING: DWBA cross section to levels in the continuum   =''
     &,F8.2,'' mb''/
     &1x,'' WARNING: CN formation cross section (Sum over Stlj)      =''
     &,F8.2,'' mb''/ 
     &1x,'' WARNING: CN formation cross section (Sum over Stl )      =''
     &,F8.2,'' mb''/ 
     &1x,'' WARNING: Reaction  cross section                         =''
     &,F8.2,'' mb''/)') 
     &  (ABScs-SINlcc-SINl-SINlcont)/xsabs, SINlcc, SINl, SINlcont, 
     &   CSFus, coeff*ftmp/DBLE(2*SEJc(Nejc) + 1), sreac
      ENDIF 

      RETURN
      END

      SUBROUTINE ECIS2EMPIRE_TR(Nejc,Nnuc,Ien,Lvibrat,Maxl,Ttll,Ttllj)
C
C     Process ECIS output to obtain TTLl,Maxl matrix for EMPIRE energy grid
C     Reads from units 45, 46 and writes to unit 6
C
C     INPUT:  Nejc  ejectile nucleus index
C             Nnuc  residual nucleus index
C             Ien   energy index
C
C     OUTPUT: TTLl(NDETL,NDLW),MaxL(NDETL) for all energies from 1 to NDETL
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
      implicit none
      INCLUDE 'dimension.h'
      INCLUDE 'global.h'
C
C Dummy arguments
C
      INTEGER Ien, Nejc, Nnuc
      LOGICAL Lvibrat
      INTEGER Maxl(NDETL)
      DOUBLE PRECISION Ttll(NDETL,NDLW),Ttllj(NDETL,NDLW,3)
C
C Local variables
C
      DOUBLE PRECISION ak2, dtmp, ecms, elab, jc, jj, sabs, sabsj,
     &                 selecis, sinlss, sreac, sreacecis, stotecis,
     &                 xmas_nejc, xmas_nnuc, stmp, xsabsj, xsabs, jsp
      LOGICAL relcal
      INTEGER l, lmax, nc, nceq, ncoll, nlev, jindex, ilv
      CHARACTER*1 parc
      DOUBLE PRECISION sjf
	sjf(l,jindex,stmp)= l - 1 + jindex - stmp
      LOGICAL unformat
      data unformat/.TRUE./

      lmax = 0
      ncoll = 0
      ecms = ETL(Ien,Nejc,Nnuc)
C-----
C----- Input of transmission coefficients
C-----
      IF(unformat) then
        OPEN (UNIT=45,STATUS='old',FILE='ecis06.tlj',form='unformatted',
     &        ERR=200) !-zvv-2013
      ELSE
        OPEN (UNIT = 45,STATUS = 'old',FILE = 'ecis06.tlj')
        READ (45,*,END = 200)  ! Skipping one line
      ENDIF     
C-----JC,ParC is the channel spin and parity
C-----nceq is the number of coupled equations
  100 continue
      IF(unformat) then
        READ (45,                        END = 200) jc, parc, nceq  ! ecis06
      ELSE
        READ (45,'(1x,f9.1,4x,a1,1x,i4)',END = 200) jc, parc, nceq  ! ecis06
      ENDIF  

C-----Selecting only the ground state ilv=1, note that inverse reaction XSs are 
C      calculated assuming that the target is always in the GS 
	ilv = 1
C-----Loop over the number of coupled equations
      DO nc = 1, nceq
C--------Reading the coupled level number nlev, the orbital momentum L,
C--------angular momentum j and Transmission coefficient Tlj,c(JC)
C--------(nlev=1 corresponds to the ground state)
         IF(unformat) then
           READ (45,  END = 200,ERR = 200) nlev, l, jj, dtmp
         ELSE
           READ (45,*,END = 200,ERR = 200) nlev, l, jj, dtmp     
         ENDIF  
         IF (dtmp.LT.1.D-20) CYCLE

         ncoll = MAX(nlev,ncoll)
         IF (nlev.EQ.ilv .and. l.lt.NDLW) THEN
C-----------Averaging over particle and target spin, summing over channel spin JC
            TTLl(Ien,l+1) = TTLl(Ien,l+1) + 
     &                           (2*jc + 1)*dtmp/DBLE(2*l + 1)  
     &                           /DBLE(2*SEJc(Nejc) + 1)
     &                           /DBLE(2*XJLv(ilv,Nnuc) + 1)

C           It always contain the TRUE maximum L to be used in loops over L from 0 to Maxlw
            lmax = l
C
            jindex = 1 ! default, good for alphas
            if    (MAXj(Nejc) .eq. 2) then ! n,p,h,t
              if(jj.gt.l) jindex = 2
            elseif(MAXj(Nejc) .eq. 3) then ! d
              if(jj.eq.l) jindex = 2
              if(jj.gt.l) jindex = 3
            endif

            Ttllj(Ien,l+1,jindex) = Ttllj(Ien,l+1,jindex) +
     &                           (2*jc + 1)*dtmp/DBLE(2*jj+1)   
     &                           /DBLE(2*XJLv(ilv,Nnuc) + 1)
C
C           Sreac = Sum_{L} Sum_{j} (2j+1)/(2s+1) Ttllj(L,j)   
C           j goes from |L-s| to L+s
C           Note: the calculation with Tlj() implies dividing by (2s+1) externally
C                 this division is not contained in the definition of Tlj()
C
C           Sreac = Sum_{L} (2L+1) Ttll(L)   
C
         ENDIF
      ENDDO
      GOTO 100
  200 CLOSE (45)
C-----For vibrational the Tls must be multiplied by 
      IF (Lvibrat) THEN
         DO l = 0, lmax
            Ttll(Ien,l+1) = Ttll(Ien,l+1)*DBLE(2*XJLv(1,Nnuc) + 1)
            DO jindex = 1,MAXj(Nejc)
               Ttllj(Ien,l+1,jindex) = Ttllj(Ien,l+1,jindex)
     &                               *DBLE(2*XJLv(1,Nnuc) + 1)
            ENDDO
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
      SIGabs(Ien,Nejc,Nnuc) = 0.D0
      IF (sreacecis.LE.0.D0) RETURN
      IF (IOUt.EQ.5) THEN
         xmas_nnuc = AMAss(Nnuc)
         xmas_nejc = EJMass(Nejc)
         relcal = .FALSE.
         IF (IRElat(Nejc,Nnuc).GT.0 .OR. RELkin) relcal = .TRUE.
         CALL KINEMA(elab,ecms,xmas_nejc,xmas_nnuc,ak2,2,relcal)
C--------Reaction cross section in mb
         sabs  = 0.D0
         sabsj = 0.D0
C        if(elab.lt.0.3d0) write(8,*)
         DO l = 0, lmax
            stmp = Ttll(Ien,l+1)*DBLE(2*l + 1)
            DO jindex = 1, MAXj(Nejc) 
             jsp = sjf(l,jindex,SEJc(Nejc)) 
             sabsj = sabsj + DBLE(2*jsp+1)*Ttllj(Ien,l+1,jindex)
            ENDDO 
C           if(elab.lt.0.3d0) write(8,303) l,stmp*10.d0*PI/ak2
C 303       format(3x,' L =',I3,' Sabs(L) =',d12.6)
            sabs = sabs + stmp
         ENDDO
C        if(elab.lt.0.3d0) write(8,*)
         xsabs  = 10.d0*PI/ak2*sabs
         xsabsj = 10.d0*PI/ak2*sabsj/DBLE(2*SEJc(Nejc)+1) 
         OPEN (UNIT = 45,FILE = 'ecis06.ics',STATUS = 'old',ERR = 350)
         READ (45,*,END = 350) ! Skipping one line
         sinlss = 0.D0
         DO l = 1, ncoll - 1
            READ (45,*,END = 350) dtmp
            sinlss = sinlss + dtmp
         ENDDO
  350    CLOSE (45)
         sreac = xsabs + sinlss
         IF (xsabs.GT.0.D0) THEN
            WRITE (8,*)
            WRITE (8,'(A7,I3,A3,E12.6,A10,E12.6,A26,1x,I4)') '  LMAX:',
     &             lmax, ' E=', ecms, ' MeV (CMS)', elab,
     &             ' MeV (LAB); Energy index =', Ien
            WRITE (8,*) ' XS calculated using averaged Tls :   Sabs =',
     &               SNGL(xsabs) , ' mb '
            WRITE (8,*) ' XS calculated using averaged Tljs:   SabsJ=',
     &               SNGL(xsabsj), ' mb '
            WRITE (8,*) ' Reaction XS =', SNGL(sreacecis),
     &                  ' mb (read from ECIS)'
            WRITE (8,*) ' ECIS/EMPIRE ratio of reaction XS =',
     &                    SNGL(sreacecis/sreac)
            IF (sinlss.GT.0.D0) THEN
             WRITE (8,*) ' Sinl =', SNGL(sinlss), ' mb (read from ECIS)'
             WRITE (8,*) ' Sreac=', 
     &                       SNGL(sreac),         ' mb (Sabs  + Sinl)'
             WRITE (8,*) ' Sreac=', 
     &                       SNGL(xsabsj+sinlss), ' mb (Sabsj + Sinl)'
            ENDIF
         ENDIF
      ENDIF
C-----Storing transmission coefficients for EMPIRE energy grid
      WRITE (46 ) lmax, Ien, ecms, IRElat(Nejc,Nnuc)
      WRITE (461) lmax, Ien, ecms, IRElat(Nejc,Nnuc)
      DO l = 0, lmax 
         WRITE (46 )  Ttll(Ien,l+1)
         WRITE (461) (Ttllj(Ien,l+1,jindex), jindex=1,MAXj(Nejc))
      ENDDO
      WRITE (46) sreacecis
      Maxl(Ien) = lmax
      SIGabs(Ien,Nejc,Nnuc) = sreacecis
      RETURN
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

      SUBROUTINE ECIS_CCVIB(Nejc,Nnuc,El,Ldwba,Inlkey,TL_calc)
C
C     -------------------------------------------------------------
C     |    Create input files for ECIS06 for COUPLED CHANNELS     |
C     |    harmonic vibrational model                             |
C     |               v1.0     S.Masetti 5/99                     |
C     |               v2.0     R.Capote  1/2001                   |
C     |               v3.0     R.Capote  5/2001 (DWBA added)      |
C     |               v4.0     R.Capote  1/2005 (SOMP added)      |
C     |               v5.0     R.Capote  8/2008 ECIS06 version    |
C     |               v6.0     R.Capote 10/2012 ECIS06 version    |
C     |               CN + Engelbretch-Weidenmuller transformation|
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
      LOGICAL Ldwba,TL_calc
C
C Local variables
C
      DOUBLE PRECISION ak2, angstep, ecms, eee, elab, elabe, rmatch,
     &                 xmas_nejc, xmas_nnuc, xratio, zerosp, convg,ecoul

      DOUBLE PRECISION vvref, wvref, wsref, vsoref, wsoref
      DOUBLE PRECISION fv, fs, fvv, fvs, tv, ts, fso, tso

      CHARACTER*1 ch

      INTEGER ncollx 
      LOGICAL lodd

      INTEGER ikey, ip, iterm, j, ldwmax, nppaa,
     &        nd_cons, nd_nlvop, njmax, npp, nwrite,
     &        nuncoupled, ncontinua, nphonon 

      INTEGER iwin, ipipe_move
      LOGICAL inc_channel, logtmp

      inc_channel = .false.

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

      ecoul  = max(1.444*Z(Nnuc)/A(Nnuc)**0.33333333-1.13,0.d0)

      convg=1.0d-10
      IF(Ldwba .and. DABS(-El).GT.max(ecoul,5.d0)) convg=1.0d-5
C-----ECIS iteration scheme is used.
      ECIs1(21:21) = 'F'
C-----Usual coupled equations instead of ECIS scheme is used
C     for nucleon energies below 10 MeV and CC calculations
      if( DABS(-El)-ecoul.lt.10.d0 .and. (.not.TL_calc) 
     &                             .and. (.not.Ldwba) ) THEN
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
      xmas_nnuc = AMAss(Nnuc)
      xratio = xmas_nnuc/(xmas_nejc + xmas_nnuc)
C
C     saving the input value of the key CN_isotropic
      logtmp = CN_isotropic

      IF (El.LT.0.D0) THEN
         inc_channel = .true.
         El = DABS( - El)
         elab = El
         ikey = -1
      ELSE
C        CN_isotropic = .TRUE.
         ecms = El
         ikey = +1
      ENDIF

      IF (TL_calc .or. (.not.inc_channel)) CN_isotropic = .TRUE.
      if (INLkey.EQ.0 .or. DIRect.EQ.3) CN_isotropic = .TRUE. 
C
C     INLkey = 0  DWBA calculation for the ground state = Spher.OMP
C     INLkey > 0  Calculation for coupled and uncoupled states = DWBA or CC
C     INLkey < 0  Calculation for coupled states only = CC
C     if(.not.TL_calc) then
C       write(*,*)'Vibrational Nnuc=',Nnuc,' Isotr ? ',CN_isotropic
C       write(*,*)'INLkey=',INLkey,' DWBA ? ',lDWBA,' TL calc ? ',TL_calc
C       write(*,*)  
C     endif

      IF(.not.CN_isotropic) then
C
C       COMPOUND NUCLEUS                                                  
C
        ECIs2(31:31) = 'T'  ! HF corrections (CN, etc)
C       33- LO(83) NO ENGELBRETCH-WEIDENMULLER TRANSFORMATION IN CN       
        IF(INTerf.eq.1) ECIs2(33:33) = 'F'  ! E-W transformation used
        IF(INTerf.eq.0) ECIs2(33:33) = 'T'  ! E-W transformation NOT used
C
C       34- LO(84) UNCOUPLED LEVELS FOR COMPOUND NUCLEUS. IT IS SET       
C                .FALSE. IF NONE ARE READ.                              
        ECIs2(34:34) = 'T'  ! THERE ARE ALWAYS UNCOUPLED LEVELS
C
C       35- LO(85) FISSION TRANSMISSION COEFFICIENTS (TO BE READ FROM   
C                CARDS) FOR COMPOUND NUCLEUS. IT IS SET .FALSE. IF NONE ARE READ.                                              
C
        ECIs2(35:35) = 'F'    ! default: no fission in HF
                              ! Gf(J,pi) fission widths should be properly trasferred 
        IF(nfiss_tr.gt.0)
     &    ECIs2(35:35) = 'T'  ! .TRUE. must be used for fissiles !!!!
C
C       36- LO(86) GAMMA EMISSION IN COMPOUND NUCLEUS.                    

        ECIs2(36:36) = 'F'    ! default: no gamma emission in HF
                              ! Gg(L) gamma width should be properly trasferred 
        IF(ngamm_tr.gt.0)     
     &    ECIs2(36:36) = 'T'  ! .TRUE. to consider gamma emission in HF
C
        ECIs2(37:37) = 'F'    ! Moldauer's width fluctuation correction

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
C-----At least ground state is always open and considered
      nd_nlvop = 1
      nd_cons  = 1
      IF (Inlkey.NE.0) THEN
         IF (ND_nlv.GT.1) THEN
           DO j = 2, ND_nlv
             eee = elab - D_Elv(j)/xratio
             IF (.not.Ldwba) THEN
               IF (ICOllev(j).GT.LEVcc) CYCLE
               nd_cons = nd_cons + 1
               IF (eee.GT.0.0001) nd_nlvop = nd_nlvop + 1
             ELSE
               IF (eee.GT.0.0001) nd_nlvop = nd_nlvop + 1
               ! skipping DWBA closed channels
               IF (eee.LT.0.0001 .and. ICOllev(j).GT.LEVcc) CYCLE
               nd_cons = nd_cons + 1   
             ENDIF
           ENDDO
         ENDIF
C        DO j = 2, ND_nlv
C           eee = elab - D_Elv(j)/xratio
C           IF (eee.GT.0.0001) nd_nlvop = nd_nlvop + 1
C           IF (.not.Ldwba) THEN
C             IF (ICOllev(j).GT.LEVcc) CYCLE
C             nd_cons = nd_cons + 1
C           ELSE
C             ! skipping DWBA closed channels
C              IF (eee.LT.0.0001 .and. ICOllev(j).GT.LEVcc) CYCLE
C             nd_cons = nd_cons + 1   
C           ENDIF
C        ENDDO
         IF (.NOT.Ldwba .AND. Inlkey.GT.0 .AND. nd_nlvop.EQ.1)
     &       WRITE (8,*)
     &               ' All inelastic channels are closed at this energy'

C        IF (Ldwba) nd_cons = nd_nlvop ! Only open channels considered for DWBA
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
      IF (.not.Ldwba) npp = min(nd_cons,nd_nlvop)

C     WRITE (*,*) 'Nopen=',nd_nlvop,' nd_cons=',nd_cons,' Npp=',npp
C     WRITE (*,*) 'Nejc=',nejc,' Elab=',elab,'  Ldwba=',Ldwba
C
      zerosp = 0.d0
C
      ecoul  = max(1.444*Z(Nnuc)/A(Nnuc)**0.33333333-1.13,0.d0)
      ldwmax = 2.4*1.25*A(Nnuc)**0.33333333*0.22*
     &         SQRT(xmas_nejc*max(elab-ecoul,0.d0))

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

C     WRITE (*,*) 'Ncoll=',ncollx,' Npot=',nppaa

      WRITE (1,'(4i5)') ncollx, njmax, iterm, nppaa
C-----Matching radius
C-----CARD 5
      WRITE (1,'(2f10.5,10x,1p,3(2x,d8.1))') 
     +    RStep,rmatch,convg,convg,convg
C     To obtain Legendre expansion a blank card calling for default values of the expansion
      WRITE (1,*)

      nuncoupled = 0
      ncontinua  = 0

      IF(.not.CN_isotropic) then
        DO j = 2, ND_nlv
C         skipping coupled levels
          IF (.NOT.Ldwba .AND. ICOllev(j).LE.LEVcc) CYCLE
C---------If channel is closed then eee < 0
          eee = elab - D_Elv(j)/xratio
          ! only open channels considered for DWBA (and closed CC channels)
          IF (eee.LT.0.0001 .AND. ICOllev(j).GT.LEVcc) EXIT 
          nuncoupled = nuncoupled + 1
        ENDDO
C       For DWBA calculations, no additional levels are considered
        IF (Ldwba) nuncoupled = 0
C
C       For CN calculation a card stating a total number of coupled and uncoupled levels + 1 continua
C        1- 5   NSP(1) NUMBER OF UNCOUPLED STATES AND CONTINUA. IF IT IS  ECIS-413
C                      ZERO, LO(84)=.FALSE.                               ECIS-414
C        6-10   NSP(2) NUMBER OF UNCOUPLED STATES WITH ANGULAR            ECIS-415
C                      DISTRIBUTION. THEY MUST BE THE FIRST GIVEN.        ECIS-416
C                      IT IS REPLACED BY MIN(NSP(1),NSP(2)).              ECIS-417
C       11-15   NFISS  NUMBER OF FISSION DATA. IF NFISS=0, LO(85)=.FALSE. ECIS-418
C       16-20   NRD    NUMBER OF GAMMA TRANSMISSION FACTORS. IF IT IS 0,  ECIS-419
C                      THESE COEFFICIENTS ARE COMPUTED.                   ECIS-420
C       21-25   NCONT  NUMBER OF CONTINUA. THEY MUST BE THE LAST GIVEN,   ECIS-421
C                      NO ANGULAR DISTRIBUTION CAN BE REQUESTED FOR THEM. ECIS-422
C-------CARD 7
C
        WRITE (1,'(5i5)') 
     >      nuncoupled, nuncoupled, 2*nfiss_tr, ngamm_tr, ncontinua 
C           Fission transmission multiplied by 2 to account for both parities
C
      ENDIF

C-----ground state
C     ch = '-'
C     IF ( LVP(LEVtarg,Nnuc).GT.0 ) ch = '+'
C-----Important: Instead of using TARGET SPIN (XJLV(1,NNUC)) and PARITY(ch)
C-----A.Koning always used in PREGNASH SPIN=0, ch='+'
C-----It is justified for vibrational model and DWBA calculations
C-----so we are using zero spin and positive parity here
C-----NOT TRUE for rotational model calculations (see ecis_CCvibrot)
C     WRITE(1, '(f5.2,2i2,a1,5f10.5)')XJLV(1,NNUC),0,1, ch, elab,
      WRITE (1,'(f5.2,2i2,a1,f10.6,4F10.5)') zerosp, 0, 1, '+', elab,
     &                              SEJc(Nejc), xmas_nejc, xmas_nnuc,
     &                              Z(Nnuc)*ZEJc(Nejc)
C-----0 phonon involved
      WRITE (1,'( )')
      IF (Inlkey.NE.0) THEN
C--------discrete levels
         nwrite  = 1
         nphonon = 0
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

            ! only open channels considered for DWBA (and closed CC channels)
            IF (eee.LT.0.0001 .and. ICOllev(j).GT.LEVcc .and. Ldwba) 
     >         CYCLE

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
C              This channel is closed, so there is no calculation, 
C              but the change of parity for negative parity level is misleading. 
C              ch = '+'
               WRITE (1,'(f5.2,2i2,a1,5f10.5)') dtmp, 0, 1, ch,
     &                D_Elv(j)
            ENDIF
            IF (Ldwba .OR. DIRect.EQ.3) THEN
C--------------If DWBA, all states are assumed to be one phonon
               nphonon = nphonon + 1
C              WRITE (1,'(3i5)') 1, j - 1, 0
               WRITE (1,'(3i5)') 1, nphonon, 0
            ELSEIF (IPH(j).EQ.1) THEN   ! one-phonon states
               nphonon = nphonon + 1
C              WRITE (1,'(3i5)') IPH(j), j - 1, 0
               WRITE (1,'(3i5)') IPH(j), nphonon, 0
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

C-----------channel energy
            eee = elab - D_Elv(j)/xratio
            ! only open channels considered for DWBA (and closed CC channels)
            IF (eee.LT.0.0001 .and. ICOllev(j).GT.LEVcc .and. Ldwba) 
     >         CYCLE

            IF (Ldwba .OR. DIRect.EQ.3) THEN
C--------------If DWBA, all states are assumed to be one phonon
               WRITE (1,'(i5,5x,6f10.5)') INT(D_Xjlv(j)),ABS(D_Def(j,2))
            ELSE
C--------------only one phonon states need deformations as input
               IF (IPH(j).EQ.1) WRITE (1,'(i5,5x,6f10.5)')
     &                                 INT(D_Xjlv(j)), ABS(D_Def(j,2))
            ENDIF
         ENDDO
      ENDIF

      IF(.not.CN_isotropic .and. nuncoupled.GT.0) then
C        Uncoupled levels
         DO j = 2, ND_nlv
C           skipping coupled levels for CC calculation
            IF (.NOT.Ldwba .AND. ICOllev(j).LT.LEVcc) CYCLE
            ch = '-'
            IF (D_Lvp(j).GT.0) ch = '+'
C-----------skipping closed uncoupled levels
            eee = elab - D_Elv(j)/xratio
C           IF (eee.LT.0.0001) CYCLE
            ! only open channels considered for DWBA (and closed CC channels)
            IF (eee.LT.0.0001 .and. ICOllev(j).GT.LEVcc) CYCLE

            if(nppaa.gt.1) nwrite = nwrite + 1
            dtmp = D_Xjlv(j)
            ! making integer spin for odd nuclides CC levels in DWBA calculations
            if(Ldwba .and. lodd) dtmp = INT(D_Xjlv(j))
             WRITE (1,'(f5.2,i2,a1,5f10.5)')  
     &         dtmp, nwrite, ch, D_Elv(j)
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

        IF(.not.CN_isotropic) then
C
C         HAUSER-FESHBACH CORRECTIONS             FORMAT (7F10.5)                 
C         ***************************                                             
C          1-10   BZ1.   SQUARE ROOT OF ELASTIC ENHANCEMENT.  (DEFAULT VALUE 1.4142)
C         11-20   BZ2.   PARTICLE DEGREES OF FREEDOM (DEFAULT = MOLDAUER prescription)
C         21-30   BZ3.   PARAMETER BZ3 IN MOLDAUER'S FORMULA  (DEFAULT VALUE 1.212 )
C         31-40   BZ4.   PARAMETER BZ4 IN MOLDAUER'S FORMULA  (DEFAULT VALUE 0.78  )
C         41-50   BZ5.   PARAMETER BZ5 IN MOLDAUER'S FORMULA  (DEFAULT VALUE 0.228 )
C
C           IF BZ2=0., THE CHANNEL DEGREE OF FREEDOM PARAMETER, FORMULA (1) 
C           IN P.A.MOLDAUER, NUCLEAR PHYSICS A344 (1980), PAGE 185-195, 
C           WHICH IS:  1.78D0+(TL**1.212D0-0.78D0)*DEXP(-0.228D0*SUM ON TL)
C
          WRITE (1,*)   ! Blank line to get all Moldauer's defaults
C
C
C         FISSION DATA                            FORMAT (7F10.5) IF LO(85)=.TRUE. AND nfiss_tr IS NOT 0. 
C
C          1-10   FISS(1,*)  TRANSMISSION COEFFICIENT                       
C         11-20   FISS(2,*)  DEGREE OF FREEDOM. IF <.5, IT IS REPLACED BY 0.
C         THERE ARE NFISS SUCH CARDS. THE FIRST COEFFICIENT IS FOR THE SMALLEST 
C         TOTAL J VALUE OF THE SYSTEM AND THE SAME PARITY OF THE GROUND STATE. THE
C         SECOND ONE IS FOR THE OPPOSITE PARITY. THE FOLLOWING ONES ARE FOR HIGHER
C         J VALUES, WITH THE SAME ORDER FOR PARITIES.                             
          IF(nfiss_tr.gt.0) THEN
C           write(*,*) 
C    >        'TARGET PARITY =',LVP(LEVtarg,Nnuc),' LEVtarg =',LEVtarg
            IF(LVP(LEVtarg,Nnuc).GT.0) THEN
              do j = 1, nfiss_tr
                WRITE (1,'(2f10.5)') fiss_tr(j,1),1.0  ! first parity  , fiss_tr(j,1),fiss_dof(j) should be defined
                WRITE (1,'(2f10.5)') fiss_tr(j,2),1.0  ! oposite parity, fiss_tr(j,2),fiss_dof(j) should be defined
              enddo
            ELSE
              do j = 1, nfiss_tr
                WRITE (1,'(2f10.5)') fiss_tr(j,2),1.0  ! first parity  , fiss_tr(j,1),fiss_dof(j) should be defined
                WRITE (1,'(2f10.5)') fiss_tr(j,1),1.0  ! oposite parity, fiss_tr(j,2),fiss_dof(j) should be defined
              enddo
            ENDIF 
          ENDIF 
C
C         GAMMA TRANSMISSION FACTORS              FORMAT (7F10.5) IF LO(86)=.TRUE. AND ngamm_tr IS NOT 0. 
C          1-10   GAM(1) FOR L=0.                               
C         11-20   GAM(2) FOR L=1.                               
C         .......................                               
C         61-70   GAM(7) FOR L=6.                               
C         UP TO GAM(NRD), EVENTUALLY ON OTHERS CARDS.           
C         WRITE (1,'(6f10.5)') 2.24*Gg_obs/D0_obs/1.E6, Q(1,1)
C         WRITE (1,*)  ! Gilbert & Cameron target LD (as described in ECIS)
          IF(ngamm_tr.gt.0) THEN
            WRITE (1,'(6f10.5)') (gamm_tr(j),j=1,ngamm_tr) !  gamm_tr(j) should be defined
C
C            
C           LEVEL DENSITY OF COMPOUND NUCLEUS       FORMAT (7F10.5)               
C
C EXAMPLES OF LD:
C 92.       21.79      3.40100   0.48000   6.14200  -0.40000   4.26100  U-239 LD
C 92.       20.69      4.62000   0.53000   5.30000  -0.15000   6.06000  U-238 LD
C
C           IF ncontinua > 0
C             FOR THE TOTAL RESIDUAL NUCLEUS NEEDED FOR THE GAMMA GIANT RESONANCE,  
C             FOLLOWED BY THE RESIDUAL NUCLEUS OF EACH CONTINUUM:                   
C             1-10   SCN(7,I) Z:   CHARGE OF THE COMPOUND NUCLEUS             
C            11-20   SCN(1,I) SA:  LEVEL DENSITY PARAMETER FOR S-WAVE 
C            21-30   SCN(2,I) UX:  MATCHING ENERGY FOR THE TWO DENSITY FORMULA 
C                             SHIFTED BY PAIRING ENERGY. (DEFAULT VALUE 2.5+150/NA).                             
C            31-40   SCN(3,I) TAU: NUCLEAR TEMPERATURE. (DEFAULT VALUE 1/TAU=SQRT(SA/UX)-1.5/UX).                  
C            41-50   SCN(4,I) SG:  SPIN CUT OFF PARAMETER. (DEFAULT VALUE FORMULA (11)  G&C
C            51-60   SCN(5,I) E0:  ENERGY SHIFT. (DEFAULT VALUE FORMULA (28) G&C
C            61-70   SCN(6,I) EX:  MATCHING ENERGY BETWEEN THE TWO DENSITY    51
C                           FORMULAE. (DEFAULT VALUE UX+PAIRING WITH    ECIS-952
C                           PAIRING GIVEN BY COOK)                      ECIS-953
C           WRITE(1,'(7f10.5)') Z(1), ...
C           WRITE(1,'(7f10.5)') Z(0),     ...
          ENDIF
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

      ENDIF ! (IDRs.gt.0)


      IF (Inlkey.NE.0) THEN
C
C------2) discrete levels
         DO j = 2, ND_nlv
C           All levels with icollev(j)>LEVcc should be calculated by DWBA
            IF (.NOT.Ldwba .AND. ICOllev(j).GT.LEVcc) CYCLE

C-----------channel energy
            eee = elab - D_Elv(j)/xratio
C           ! only open channels considered for DWBA
C           IF (eee.LT.0.0001 .and. Ldwba) CYCLE
            ! only open channels considered for DWBA (and closed CC channels)
            IF (eee.LT.0.0001 .and. ICOllev(j).GT.LEVcc .and. Ldwba) 
     >         CYCLE

C-----------If channel is closed ground state potential is used for this level
            IF(IDRs.gt.0) then
C-------------SETPOTS : subroutine for optical model parameters
C-------------From  cms system to Lab
C             ikey = +1   ! WE NEED TO SET IKEY TO -1 AS ENERGIES ARE IN THE LAB FRAME
              elabe = eee 
              ikey  = -1   
C
C-------------Transformation of energies from laboratory to center-of-mass
C-------------if needed is done inside SETPOTS() -> OMPAR()
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

            IF (eee.GT.0.0001) THEN
C---------------SETPOTS : subroutine for optical model parameters
C---------------From  cms system to Lab
                elabe = eee   ! Changed on Aug. 2008
                ikey  = -1    ! Level energies ARE IN THE LAB FRAME 
C
C---------------Transformation of energies from laboratory to center-of-mass
C---------------if needed is done inside SETPOTS() -> OMPAR()
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

        IF(.not.CN_isotropic) then
C
C         HAUSER-FESHBACH CORRECTIONS             FORMAT (7F10.5)                 
C         ***************************                                             
C          1-10   BZ1.   SQUARE ROOT OF ELASTIC ENHANCEMENT.  (DEFAULT VALUE 1.4142)
C         11-20   BZ2.   PARTICLE DEGREES OF FREEDOM (DEFAULT = MOLDAUER prescription)
C         21-30   BZ3.   PARAMETER BZ3 IN MOLDAUER'S FORMULA  (DEFAULT VALUE 1.212 )
C         31-40   BZ4.   PARAMETER BZ4 IN MOLDAUER'S FORMULA  (DEFAULT VALUE 0.78  )
C         41-50   BZ5.   PARAMETER BZ5 IN MOLDAUER'S FORMULA  (DEFAULT VALUE 0.228 )
C
C           IF BZ2=0., THE CHANNEL DEGREE OF FREEDOM PARAMETER, FORMULA (1) 
C           IN P.A.MOLDAUER, NUCLEAR PHYSICS A344 (1980), PAGE 185-195, 
C           WHICH IS:  1.78D0+(TL**1.212D0-0.78D0)*DEXP(-0.228D0*SUM ON TL)
C
          WRITE (1,*)   ! Blank line to get all Moldauer's defaults
C
C
C         FISSION DATA                            FORMAT (7F10.5) IF LO(85)=.TRUE. AND nfiss_tr IS NOT 0. 
C
C          1-10   FISS(1,*)  TRANSMISSION COEFFICIENT                       
C         11-20   FISS(2,*)  DEGREE OF FREEDOM. IF <.5, IT IS REPLACED BY 0.
C         THERE ARE NFISS SUCH CARDS. THE FIRST COEFFICIENT IS FOR THE SMALLEST 
C         TOTAL J VALUE OF THE SYSTEM AND THE SAME PARITY OF THE GROUND STATE. THE
C         SECOND ONE IS FOR THE OPPOSITE PARITY. THE FOLLOWING ONES ARE FOR HIGHER
C         J VALUES, WITH THE SAME ORDER FOR PARITIES.                             
          IF(nfiss_tr.gt.0) THEN
            IF(LVP(LEVtarg,Nnuc).GT.0) THEN
C             write(*,*) 
C    >          'TARGET PARITY =',LVP(LEVtarg,Nnuc),' LEVtarg =',LEVtarg
              do j = 1, nfiss_tr
                WRITE (1,'(2f10.5)') fiss_tr(j,1),1.0  ! first parity  , fiss_tr(j,1),fiss_dof(j) should be defined
                WRITE (1,'(2f10.5)') fiss_tr(j,2),1.0  ! oposite parity, fiss_tr(j,2),fiss_dof(j) should be defined
              enddo
            ELSE
              do j = 1, nfiss_tr
                WRITE (1,'(2f10.5)') fiss_tr(j,2),1.0  ! first parity  , fiss_tr(j,1),fiss_dof(j) should be defined
                WRITE (1,'(2f10.5)') fiss_tr(j,1),1.0  ! oposite parity, fiss_tr(j,2),fiss_dof(j) should be defined
              enddo
            ENDIF 
          ENDIF 
C
C         GAMMA TRANSMISSION FACTORS              FORMAT (7F10.5) IF LO(86)=.TRUE. AND ngamm_tr IS NOT 0. 
C          1-10   GAM(1) FOR L=0.                               
C         11-20   GAM(2) FOR L=1.                               
C         .......................                               
C         61-70   GAM(7) FOR L=6.                               
C         UP TO GAM(NRD), EVENTUALLY ON OTHERS CARDS.           
C         WRITE (1,'(6f10.5)') 2.24*Gg_obs/D0_obs/1.E6, Q(1,1)
C         WRITE (1,*)  ! Gilbert & Cameron target LD (as described in ECIS)
          IF(ngamm_tr.gt.0) THEN
            WRITE (1,'(6f10.5)') (gamm_tr(j),j=1,ngamm_tr) !  gamm_tr(j) should be defined
C
C            
C           LEVEL DENSITY OF COMPOUND NUCLEUS       FORMAT (7F10.5)               
C
C EXAMPLES OF LD:
C 92.       21.79      3.40100   0.48000   6.14200  -0.40000   4.26100  U-239 LD
C 92.       20.69      4.62000   0.53000   5.30000  -0.15000   6.06000  U-238 LD
C
C           IF ncontinua > 0
C             FOR THE TOTAL RESIDUAL NUCLEUS NEEDED FOR THE GAMMA GIANT RESONANCE,  
C             FOLLOWED BY THE RESIDUAL NUCLEUS OF EACH CONTINUUM:                   
C             1-10   SCN(7,I) Z:   CHARGE OF THE COMPOUND NUCLEUS             
C            11-20   SCN(1,I) SA:  LEVEL DENSITY PARAMETER FOR S-WAVE 
C            21-30   SCN(2,I) UX:  MATCHING ENERGY FOR THE TWO DENSITY FORMULA 
C                             SHIFTED BY PAIRING ENERGY. (DEFAULT VALUE 2.5+150/NA).                             
C            31-40   SCN(3,I) TAU: NUCLEAR TEMPERATURE. (DEFAULT VALUE 1/TAU=SQRT(SA/UX)-1.5/UX).                  
C            41-50   SCN(4,I) SG:  SPIN CUT OFF PARAMETER. (DEFAULT VALUE FORMULA (11)  G&C
C            51-60   SCN(5,I) E0:  ENERGY SHIFT. (DEFAULT VALUE FORMULA (28) G&C
C            61-70   SCN(6,I) EX:  MATCHING ENERGY BETWEEN THE TWO DENSITY    51
C                           FORMULAE. (DEFAULT VALUE UX+PAIRING WITH    ECIS-952
C                           PAIRING GIVEN BY COOK)                      ECIS-953
C           WRITE(1,'(7f10.5)') Z(1), ...
C           WRITE(1,'(7f10.5)') Z(0),     ...
          ENDIF
        ENDIF

      ENDIF

      WRITE (1,'(4hFIN )')
      CLOSE (UNIT = 1)
      IF (Inlkey.EQ.0) THEN
         iwin = ipipe_move('ecSPH.inp','ecis06.inp')
      ELSE
         iwin = ipipe_move('ecVIB.inp','ecis06.inp')
      ENDIF
C
C-----Running ECIS06
C
      IF(inc_channel .and. Inlkey.NE.0) 
     >  write (*,*) '  Running ECIS (vibr) ...'

      IF(inc_channel .and. Inlkey.EQ.0) 
     >  write (*,*) '  Running ECIS (sphe) ...'

      CALL ECIS('ecis06 ')

C     restoring the input value of the key CN_isotropic
      CN_isotropic = logtmp

      IF(TL_calc) RETURN

      IF (Inlkey.EQ.0) THEN
         iwin = ipipe_move('ecis06.out','ECIS_SPH.out')
         iwin = ipipe_move('ecis06.inp','ECIS_SPH.inp')
      ELSE
         iwin = ipipe_move('ecis06.out','ECIS_VIB.out')
         iwin = ipipe_move('ecis06.inp','ECIS_VIB.inp')
      ENDIF

      RETURN 
      END

      SUBROUTINE ECIS_CCVIBROT(Nejc,Nnuc,El,TL_calc)
C
C     -------------------------------------------------------------
C     |    Create input files for ECIS06 for COUPLED CHANNELS     |
C     |    rotational-vibrational model                           |
C     |               v1.0     S.Masetti 5/99                     |
C     |               v2.0     R.Capote  1/2001                   |
C     |               v3.0     R.Capote  5/2001 (DWBA added)      |
C     |               v4.0     R.Capote  8/2008 ECIS06 version    |
C     |               v5.0     R.Capote  8/2012 ECIS06 version    |
C     |               CN + Engelbretch-Weidenmuller transformation|
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
      INTEGER Nejc, Nnuc
      LOGICAL TL_calc
C
C Local variables
C
      DOUBLE PRECISION ak2, angstep, ecms, eee, elab, elabe, rmatch,
     &                 xmas_nejc, xmas_nnuc, xratio, convg

      DOUBLE PRECISION vvref, wvref, wsref, vsoref, wsoref
      DOUBLE PRECISION fv, fs, fvv, fvs, tv, ts, fso, tso

      CHARACTER*1 ch

      INTEGER ikey, ip, iterm, j, jdm, k, ldwmax, lev(NDLV), nppaa,
     &        nd_cons, nd_nlvop, ncollm, njmax, npho, npp, nwrite,
     &        nuncoupled, ncontinua

      INTEGER iwin, ipipe_move

      LOGICAL inc_channel, logtmp

      inc_channel = .false.

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

C     T - COMPLETE CALCULATION UP TO THE END-(INVERSE: ONE       
C     ITERATION ONLY AS SOON AS TWO ITERATIONS ARE ENOUGH).  
C     ECIs1(25:25) = 'T'

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
      xmas_nnuc = AMAss(Nnuc)
      xratio = xmas_nnuc/(xmas_nejc + xmas_nnuc)
C
C     saving the input value of the key CN_isotropic
      logtmp = CN_isotropic
C
C-----From cms system to Lab (ECIS do inverse convertion)
      IF (El.LT.0.D0) THEN
         inc_channel = .true.
         El = DABS( - El)
         elab = El
         ikey = -1
      ELSE
C        CN_isotropic = .TRUE.
         ecms = El
         ikey = +1
      ENDIF

      IF (TL_calc .or. (.not.inc_channel)) CN_isotropic = .TRUE.
C     write(*,*)  'Rotational Nnuc=',Nnuc,' Isotr? ',CN_isotropic
C     write(*,*)  ' TL calc?',TL_calc
      
      IF(.not.CN_isotropic) then
C
C       COMPOUND NUCLEUS                                                  
C
        ECIs2(31:31) = 'T'  ! HF corrections (CN, etc)
C       33- LO(83) NO ENGELBRETCH-WEIDENMULLER TRANSFORMATION IN CN       
        IF(INTerf.eq.1) ECIs2(33:33) = 'F'  ! E-W transformation used
        IF(INTerf.eq.0) ECIs2(33:33) = 'T'  ! E-W transformation NOT used
C       34- LO(84) UNCOUPLED LEVELS FOR COMPOUND NUCLEUS. IT IS SET       
C                .FALSE. IF NONE ARE READ.                              
        ECIs2(34:34) = 'T'  ! THERE ARE ALWAYS UNCOUPLED LEVELS

C
C       35- LO(85) FISSION TRANSMISSION COEFFICIENTS (TO BE READ FROM   
C                CARDS) FOR COMPOUND NUCLEUS. IT IS SET .FALSE. IF NONE ARE READ.                                              
C
        ECIs2(35:35) = 'F'    ! default: no fission in HF
                              ! Gf(J,pi) fission widths should be properly trasferred 
        IF(nfiss_tr.gt.0)
     &    ECIs2(35:35) = 'T'  ! .TRUE. must be used for fissiles !!!!
C
C       36- LO(86) GAMMA EMISSION IN COMPOUND NUCLEUS.                    

        ECIs2(36:36) = 'F'    ! default: no gamma emission in HF
                              ! Gg(L) gamma width should be properly trasferred 
        IF(ngamm_tr.gt.0)     
     &    ECIs2(36:36) = 'T'  ! .TRUE. to consider gamma emission in HF

        ECIs2(37:37) = 'F'  ! Moldauer's width fluctuation correction
C
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
            IF (ICOllev(j).GT.LEVcc) CYCLE
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
      IF (ECIs2(42:42).EQ.'T') iterm = 1
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
     &) elab, PARname(ip), NINT(A(Nnuc)), NUC(NINT(Z(Nnuc)))
C    &) El, PARname(ip), NINT(A(Nnuc)), NUC(NINT(Z(Nnuc)))
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
      WRITE (1,'(2f10.5,10x,1p,3(2x,d8.1))') 
     +    RStep,rmatch,convg,convg,convg
C     To obtain Legendre expansion a blank card calling for default values of the expansion
C-----CARD 6
      WRITE(1, *)

      nuncoupled = 0
      ncontinua  = 0

      IF(.not.CN_isotropic) then

C--------
C       Uncoupled levels considered only in DWBA calculations
C
C       IF (ND_nlv.GT.0) THEN
C         DO j = 2, ND_nlv
C           skipping coupled levels
C           if(ICOllev(j).LT.LEVcc) CYCLE
C           eee = elab - D_Elv(j)/xratio
C           if (eee.LT.0.0001) EXIT
C           nuncoupled = nuncoupled + 1
C         ENDDO
C       ENDIF
C--------
C
C       For CN calculation a card stating a total number of coupled and uncoupled levels + 1 continua
C        1- 5   NSP(1) NUMBER OF UNCOUPLED STATES AND CONTINUA. IF IT IS  ECIS-413
C                      ZERO, LO(84)=.FALSE.                               ECIS-414
C        6-10   NSP(2) NUMBER OF UNCOUPLED STATES WITH ANGULAR            ECIS-415
C                      DISTRIBUTION. THEY MUST BE THE FIRST GIVEN.        ECIS-416
C                      IT IS REPLACED BY MIN(NSP(1),NSP(2)).              ECIS-417
C       11-15   NFISS  NUMBER OF FISSION DATA. IF NFISS=0, LO(85)=.FALSE. ECIS-418
C       16-20   NRD    NUMBER OF GAMMA TRANSMISSION FACTORS. IF IT IS 0,  ECIS-419
C                      THESE COEFFICIENTS ARE COMPUTED.                   ECIS-420
C       21-25   NCONT  NUMBER OF CONTINUA. THEY MUST BE THE LAST GIVEN,   ECIS-421
C                      NO ANGULAR DISTRIBUTION CAN BE REQUESTED FOR THEM. ECIS-422
C-------CARD 7
C     
        WRITE (1,'(5i5)') 
     >      nuncoupled, nuncoupled, 2*nfiss_tr, ngamm_tr, ncontinua 
C           Fission transmission multiplied by 2 to account for both parities
C
      ENDIF
C
C-----Matching radius calculated within ECIS
C     WRITE(1, *)
      ch = '-'
      IF (LVP(ilv,Nnuc).GT.0) ch = '+'
      WRITE (1,'(f5.2,2i2,a1,F10.6,4f10.5)') XJLv(ilv,Nnuc), 0, 1, ch,
     &                              elab, SEJc(Nejc), xmas_nejc,
     &                              xmas_nnuc, Z(Nnuc)*ZEJc(Nejc)
C-----Discrete coupled levels
      npho = 0
      nwrite = 1
      DO j = 2, ND_nlv
C--------All levels with icollev(j)>LEVcc skipped
         IF (ICOllev(j).GT.LEVcc) CYCLE
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
               WRITE (1,'(f5.2,2i2,a1,5f10.5)') D_Xjlv(j), 1, 1, 
     &             ch, D_Elv(j)
            ELSE
               WRITE (1,'(f5.2,2i2,a1,5f10.5)') D_Xjlv(j), 0, 1, 
     &             ch, D_Elv(j)
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
C 
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
     &      INT(D_Klv(lev(j))+ 0.1),ABS(D_Def(lev(j),2))
         ENDDO
      ENDIF
C

C--------
C       Uncoupled levels considered only in DWBA calculations
C
C     IF(.not.CN_isotropic) then
C
C        Uncoupled levels
C
C        DO j = 2, ND_nlv
C          skipping coupled levels
C          if(ICOllev(j).LE.LEVcc) CYCLE
C          ch = '-'
C          IF (D_Lvp(j).GT.0) ch = '+'
C          skipping closed channels
C          eee = elab - D_Elv(j)/xratio
C          IF (eee.LT.0.0001) CYCLE
C          if(nppaa.gt.1) nwrite = nwrite + 1
C          WRITE (1,'(f5.2,2i2,a1,5f10.5)') 
C    &       D_Xjlv(j),0, nwrite, ch, D_Elv(j)
C        ENDDO
C     ENDIF
C--------

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

        IF(.not.CN_isotropic) then
C
C         HAUSER-FESHBACH CORRECTIONS             FORMAT (7F10.5)                 
C         ***************************                                             
C          1-10   BZ1.   SQUARE ROOT OF ELASTIC ENHANCEMENT.  (DEFAULT VALUE 1.4142)
C         11-20   BZ2.   PARTICLE DEGREES OF FREEDOM (DEFAULT = MOLDAUER prescription)
C         21-30   BZ3.   PARAMETER BZ3 IN MOLDAUER'S FORMULA  (DEFAULT VALUE 1.212 )
C         31-40   BZ4.   PARAMETER BZ4 IN MOLDAUER'S FORMULA  (DEFAULT VALUE 0.78  )
C         41-50   BZ5.   PARAMETER BZ5 IN MOLDAUER'S FORMULA  (DEFAULT VALUE 0.228 )
C
C           IF BZ2=0., THE CHANNEL DEGREE OF FREEDOM PARAMETER, FORMULA (1) 
C           IN P.A.MOLDAUER, NUCLEAR PHYSICS A344 (1980), PAGE 185-195, 
C           WHICH IS:  1.78D0+(TL**1.212D0-0.78D0)*DEXP(-0.228D0*SUM ON TL)
C
          WRITE (1,*)   ! Blank line to get all Moldauer's defaults
C
C
C         FISSION DATA                            FORMAT (7F10.5) IF LO(85)=.TRUE. AND nfiss_tr IS NOT 0. 
C
C          1-10   FISS(1,*)  TRANSMISSION COEFFICIENT                       
C         11-20   FISS(2,*)  DEGREE OF FREEDOM. IF <.5, IT IS REPLACED BY 0.
C         THERE ARE NFISS SUCH CARDS. THE FIRST COEFFICIENT IS FOR THE SMALLEST 
C         TOTAL J VALUE OF THE SYSTEM AND THE SAME PARITY OF THE GROUND STATE. THE
C         SECOND ONE IS FOR THE OPPOSITE PARITY. THE FOLLOWING ONES ARE FOR HIGHER
C         J VALUES, WITH THE SAME ORDER FOR PARITIES.                             
          IF(nfiss_tr.gt.0) THEN
C           write(*,*) 
C    >        'TARGET PARITY =',LVP(LEVtarg,Nnuc),' LEVtarg =',LEVtarg
            IF(LVP(LEVtarg,Nnuc).GT.0) THEN
              do j = 1, nfiss_tr
                WRITE (1,'(2f10.5)') fiss_tr(j,1),1.0  ! first parity  , fiss_tr(j,1),fiss_dof(j) should be defined
                WRITE (1,'(2f10.5)') fiss_tr(j,2),1.0  ! oposite parity, fiss_tr(j,2),fiss_dof(j) should be defined
              enddo
            ELSE
              do j = 1, nfiss_tr
                WRITE (1,'(2f10.5)') fiss_tr(j,2),1.0  ! first parity  , fiss_tr(j,1),fiss_dof(j) should be defined
                WRITE (1,'(2f10.5)') fiss_tr(j,1),1.0  ! oposite parity, fiss_tr(j,2),fiss_dof(j) should be defined
              enddo
            ENDIF 
          ENDIF 
C
C         GAMMA TRANSMISSION FACTORS              FORMAT (7F10.5) IF LO(86)=.TRUE. AND ngamm_tr IS NOT 0. 
C          1-10   GAM(1) FOR L=0.                               
C         11-20   GAM(2) FOR L=1.                               
C         .......................                               
C         61-70   GAM(7) FOR L=6.                               
C         UP TO GAM(NRD), EVENTUALLY ON OTHERS CARDS.           
C         WRITE (1,'(6f10.5)') 2.24*Gg_obs/D0_obs/1.E6, Q(1,1)
C         WRITE (1,*)  ! Gilbert & Cameron target LD (as described in ECIS)
          IF(ngamm_tr.gt.0) THEN
            WRITE (1,'(6f10.5)') (gamm_tr(j),j=1,ngamm_tr) !  gamm_tr(j) should be defined
C
C            
C           LEVEL DENSITY OF COMPOUND NUCLEUS       FORMAT (7F10.5)               
C
C EXAMPLES OF LD:
C 92.       21.79      3.40100   0.48000   6.14200  -0.40000   4.26100  U-239 LD
C 92.       20.69      4.62000   0.53000   5.30000  -0.15000   6.06000  U-238 LD
C
C           IF ncontinua > 0
C             FOR THE TOTAL RESIDUAL NUCLEUS NEEDED FOR THE GAMMA GIANT RESONANCE,  
C             FOLLOWED BY THE RESIDUAL NUCLEUS OF EACH CONTINUUM:                   
C             1-10   SCN(7,I) Z:   CHARGE OF THE COMPOUND NUCLEUS             
C            11-20   SCN(1,I) SA:  LEVEL DENSITY PARAMETER FOR S-WAVE 
C            21-30   SCN(2,I) UX:  MATCHING ENERGY FOR THE TWO DENSITY FORMULA 
C                             SHIFTED BY PAIRING ENERGY. (DEFAULT VALUE 2.5+150/NA).                             
C            31-40   SCN(3,I) TAU: NUCLEAR TEMPERATURE. (DEFAULT VALUE 1/TAU=SQRT(SA/UX)-1.5/UX).                  
C            41-50   SCN(4,I) SG:  SPIN CUT OFF PARAMETER. (DEFAULT VALUE FORMULA (11)  G&C
C            51-60   SCN(5,I) E0:  ENERGY SHIFT. (DEFAULT VALUE FORMULA (28) G&C
C            61-70   SCN(6,I) EX:  MATCHING ENERGY BETWEEN THE TWO DENSITY    51
C                           FORMULAE. (DEFAULT VALUE UX+PAIRING WITH    ECIS-952
C                           PAIRING GIVEN BY COOK)                      ECIS-953
C           WRITE(1,'(7f10.5)') Z(1), ...
C           WRITE(1,'(7f10.5)') Z(0),     ...
          ENDIF
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

      ENDIF ! (IDRs.gt.0)


      IF (.not.CN_isotropic .and. IDRs.gt.0) then

        DO j = 2, ND_nlv
C          skipping uncoupled levels
           IF (ICOllev(j).GT.LEVcc) CYCLE

           eee = elab - D_Elv(j)/xratio
C          IF ((eee.LT.0.0001) .and. (ICOllev(j).GT.LEVcc)) CYCLE

C----------SETPOTS : subroutine for optical model parameters
C----------From  cms system to Lab
C          ecms = eee
C          ikey = +1   ! WE NEED TO SET IKEY TO -1 AS ENERGIES ARE IN THE LAB FRAME
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
     >       fvv = (VOM(Nejc,Nnuc) - DWVcor - vvref)/vvref
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
     >       fvs = (VSO(Nejc,Nnuc) - DSOcor - vsoref)/vsoref

           write (1,'(2(G10.4,F10.4,G10.4))')                                       
     >          tv, fv  , fvv, 
     >          ts, fs  , 0.d0
           write (1,'(2(G10.4,F10.4,G10.4))') 
     >          tso, fso, fvs

        ENDDO

      ELSE

C-------2) discrete levels
        DO j = 2, ND_nlv
C--------All levels with icollev(j)>LEVcc skipped
         IF (ICOllev(j).GT.LEVcc) CYCLE

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

      ENDIF ! (.not.CN_isotropic)

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

        IF(.not.CN_isotropic) then
C
C         HAUSER-FESHBACH CORRECTIONS             FORMAT (7F10.5)                 
C         ***************************                                             
C          1-10   BZ1.   SQUARE ROOT OF ELASTIC ENHANCEMENT.  (DEFAULT VALUE 1.4142)
C         11-20   BZ2.   PARTICLE DEGREES OF FREEDOM (DEFAULT = MOLDAUER prescription)
C         21-30   BZ3.   PARAMETER BZ3 IN MOLDAUER'S FORMULA  (DEFAULT VALUE 1.212 )
C         31-40   BZ4.   PARAMETER BZ4 IN MOLDAUER'S FORMULA  (DEFAULT VALUE 0.78  )
C         41-50   BZ5.   PARAMETER BZ5 IN MOLDAUER'S FORMULA  (DEFAULT VALUE 0.228 )
C
C           IF BZ2=0., THE CHANNEL DEGREE OF FREEDOM PARAMETER, FORMULA (1) 
C           IN P.A.MOLDAUER, NUCLEAR PHYSICS A344 (1980), PAGE 185-195, 
C           WHICH IS:  1.78D0+(TL**1.212D0-0.78D0)*DEXP(-0.228D0*SUM ON TL)
C
          WRITE (1,*)   ! Blank line to get all Moldauer's defaults
C
C
C         FISSION DATA                            FORMAT (7F10.5) IF LO(85)=.TRUE. AND nfiss_tr IS NOT 0. 
C
C          1-10   FISS(1,*)  TRANSMISSION COEFFICIENT                       
C         11-20   FISS(2,*)  DEGREE OF FREEDOM. IF <.5, IT IS REPLACED BY 0.
C         THERE ARE NFISS SUCH CARDS. THE FIRST COEFFICIENT IS FOR THE SMALLEST 
C         TOTAL J VALUE OF THE SYSTEM AND THE SAME PARITY OF THE GROUND STATE. THE
C         SECOND ONE IS FOR THE OPPOSITE PARITY. THE FOLLOWING ONES ARE FOR HIGHER
C         J VALUES, WITH THE SAME ORDER FOR PARITIES.                             
          IF(nfiss_tr.gt.0) THEN
C           write(*,*) 
C    >        'TARGET PARITY =',LVP(LEVtarg,Nnuc),' LEVtarg =',LEVtarg
            IF(LVP(LEVtarg,Nnuc).GT.0) THEN
              do j = 1, nfiss_tr
                WRITE (1,'(2f10.5)') fiss_tr(j,1),1.0  ! first parity  , fiss_tr(j,1),fiss_dof(j) should be defined
                WRITE (1,'(2f10.5)') fiss_tr(j,2),1.0  ! oposite parity, fiss_tr(j,2),fiss_dof(j) should be defined
              enddo
            ELSE
              do j = 1, nfiss_tr
                WRITE (1,'(2f10.5)') fiss_tr(j,2),1.0  ! first parity  , fiss_tr(j,1),fiss_dof(j) should be defined
                WRITE (1,'(2f10.5)') fiss_tr(j,1),1.0  ! oposite parity, fiss_tr(j,2),fiss_dof(j) should be defined
              enddo
            ENDIF 
          ENDIF 
C
C         GAMMA TRANSMISSION FACTORS              FORMAT (7F10.5) IF LO(86)=.TRUE. AND ngamm_tr IS NOT 0. 
C          1-10   GAM(1) FOR L=0.                               
C         11-20   GAM(2) FOR L=1.                               
C         .......................                               
C         61-70   GAM(7) FOR L=6.                               
C         UP TO GAM(NRD), EVENTUALLY ON OTHERS CARDS.           
C         WRITE (1,'(6f10.5)') 2.24*Gg_obs/D0_obs/1.E6, Q(1,1)
C         WRITE (1,*)  ! Gilbert & Cameron target LD (as described in ECIS)
          IF(ngamm_tr.gt.0) THEN
            WRITE (1,'(6f10.5)') (gamm_tr(j),j=1,ngamm_tr) !  gamm_tr(j) should be defined
C
C            
C           LEVEL DENSITY OF COMPOUND NUCLEUS       FORMAT (7F10.5)               
C
C EXAMPLES OF LD:
C 92.       21.79      3.40100   0.48000   6.14200  -0.40000   4.26100  U-239 LD
C 92.       20.69      4.62000   0.53000   5.30000  -0.15000   6.06000  U-238 LD
C
C           IF ncontinua > 0
C             FOR THE TOTAL RESIDUAL NUCLEUS NEEDED FOR THE GAMMA GIANT RESONANCE,  
C             FOLLOWED BY THE RESIDUAL NUCLEUS OF EACH CONTINUUM:                   
C             1-10   SCN(7,I) Z:   CHARGE OF THE COMPOUND NUCLEUS             
C            11-20   SCN(1,I) SA:  LEVEL DENSITY PARAMETER FOR S-WAVE 
C            21-30   SCN(2,I) UX:  MATCHING ENERGY FOR THE TWO DENSITY FORMULA 
C                             SHIFTED BY PAIRING ENERGY. (DEFAULT VALUE 2.5+150/NA).                             
C            31-40   SCN(3,I) TAU: NUCLEAR TEMPERATURE. (DEFAULT VALUE 1/TAU=SQRT(SA/UX)-1.5/UX).                  
C            41-50   SCN(4,I) SG:  SPIN CUT OFF PARAMETER. (DEFAULT VALUE FORMULA (11)  G&C
C            51-60   SCN(5,I) E0:  ENERGY SHIFT. (DEFAULT VALUE FORMULA (28) G&C
C            61-70   SCN(6,I) EX:  MATCHING ENERGY BETWEEN THE TWO DENSITY    51
C                           FORMULAE. (DEFAULT VALUE UX+PAIRING WITH    ECIS-952
C                           PAIRING GIVEN BY COOK)                      ECIS-953
C           WRITE(1,'(7f10.5)') Z(1), ...
C           WRITE(1,'(7f10.5)') Z(0),     ...
          ENDIF
        ENDIF

      ENDIF

      WRITE (1,'(4hFIN )')
      CLOSE (UNIT = 1)
      iwin = ipipe_move('ecVIBROT.inp','ecis06.inp')

C-----Running ECIS

      IF(inc_channel) write (*,*) '  Running ECIS (rot) ...'
      CALL ECIS('ecis06 ')

C     restoring the input value of the key CN_isotropic
      CN_isotropic = logtmp

      IF(TL_calc) RETURN

      IF (npho.GT.0) THEN
         iwin = ipipe_move('ecis06.out','ECIS_VIBROT.out')
         iwin = ipipe_move('ecis06.inp','ECIS_VIBROT.inp')
      ELSE
         iwin = ipipe_move('ecis06.out','ECIS_ROT.out')
         iwin = ipipe_move('ecis06.inp','ECIS_ROT.inp')
      ENDIF

      RETURN
      END
c----
c     CCSOFTROTOR
c----
      SUBROUTINE OPTMAN_CCSOFTROT(Nejc,Nnuc,El,TL_calc)
C
C     -------------------------------------------------------------
C     |    Create input files for OPTMAN for COUPLED CHANNELS     |
C     |        calculation of transmission coefficients in        |
C     |        the soft/rigid rotor model                         |
C     |    R.Capote  05/2011 OPTMAN v10,v11                       |
C     |    R.Capote  02/2012 OPTMAN v12                           |                                     |
C     -------------------------------------------------------------
C
C     ****
C     IP = 1 NEUTRON
C          2 PROTON
C
C Dummy arguments
C
      DOUBLE PRECISION El
      INTEGER Nejc, Nnuc
      LOGICAL TL_calc
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
C
C Local variables
C
      DOUBLE PRECISION ak2, angstep, ecms, eee, elab,
     &                 xmas_nejc, xmas_nnuc, xratio

      INTEGER ikey, ip,  j, nd_cons, nd_nlvop

      CHARACTER*132 ctmp
      INTEGER iwin, ipipe_move, ipipe

      LOGICAL inc_channel

      inc_channel = .false.

      IF (AEJc(Nejc).EQ.1.D0 .AND. ZEJc(Nejc).EQ.0.D0) ip = 1
      IF (AEJc(Nejc).EQ.1.D0 .AND. ZEJc(Nejc).EQ.1.D0) ip = 2
      IF (AEJc(Nejc).EQ.2.D0 .AND. ZEJc(Nejc).EQ.1.D0) ip = 3
      IF (AEJc(Nejc).EQ.3.D0 .AND. ZEJc(Nejc).EQ.1.D0) ip = 4
      IF (AEJc(Nejc).EQ.3.D0 .AND. ZEJc(Nejc).EQ.2.D0) ip = 5
      IF (AEJc(Nejc).EQ.4.D0 .AND. ZEJc(Nejc).EQ.2.D0) ip = 6
      IF (AEJc(Nejc).EQ.6.D0 .AND. ZEJc(Nejc).EQ.3.D0) ip = 7
      IF (AEJc(Nejc).EQ.7.D0 .AND. ZEJc(Nejc).EQ.3.D0) ip = 8
      IF (AEJc(Nejc).EQ.7.D0 .AND. ZEJc(Nejc).EQ.4.D0) ip = 9

      if(ip.gt.2)  stop 
     &    'ERROR: OPTMAN can not be used for cluster induced reactions'

C-----Data initialization
      angstep = 180.d0/(NANgela-1)

      xmas_nejc = EJMass(Nejc)
      xmas_nnuc = AMAss(Nnuc)
      xratio = xmas_nnuc/(xmas_nejc + xmas_nnuc)
      IF (El.LT.0.D0) THEN
         inc_channel = .true.
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
C
C-----Only for target, find open channels
C-----At least ground state is always open 
      nd_nlvop = 1
      nd_cons = 1
      IF (ND_nlv.GT.0) THEN
         DO j = 2, ND_nlv
C-----------All levels with icollev(j)>LEVcc should be calculated by DWBA
            IF (ICOllev(j).GT.LEVcc) CYCLE
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
      iatar = A(0) 
      iztar = Z(0)
C==============================================================================
C     OPTMAN input parameters
C     OPTMAN 1st line
      mejob = 1 ! OMP calculation, no fitting
      mepot = 2 ! potential expanded by derivatives
      meham = 5 ! Davidov-Chaban(3), Davidov-Filippov(4), Soft rotor(5) 
      mesho = 2
      mehao = 2
      mesha = 4
C mesol > 3 - solution using iterations with exact coupled channels 
C solution with the number of coupled states equal MESOL as zero
C approximation, MESOL must be less than or equal to 20;
      mesol = 5 ! automatic selection

      if( (SOFT .and. DYNam) .and. imodel.ne.4) imodel=4
      if( (.not.SOFT)  .and. imodel.gt.2) imodel=1

      if(imodel.eq.4) then ! rigid + soft rotor (4)
        mepot = 1 
        meham = 1
C mesol > 3 - solution using iterations with exact coupled channels 
C solution with the number of coupled states equal MESOL as zero
C approximation, MESOL must be less than or equal to 20;
        mesol =20 
        mesha = 1
        mesho = 0
        mehao = 0
C       exact calculation for odd and odd-odd targets
C       if ( mod(iatar-iztar,2).ne.0 .or. mod(iztar,2).ne.0 ) mesol = 2
      endif

      if(imodel.le.1) then ! rigid rotor (1) or spherical (0)
        mepot = 1 
        meham = 1
        mesha = 1
        mesho = 0
        mehao = 0
C       exact calculation for odd and odd-odd targets
        if ( mod(iatar-iztar,2).ne.0 .or. mod(iztar,2).ne.0 ) mesol = 2
      endif
      mepri = 0 ! short output
      meapp = 0 ! solution with the potential dependency on level energy losses in the channel
                ! =1 (No energy losses considered)
      mevol = 0
      merel = 0
C-----Relativistic kinematics (y/n)
      IF (IRElat(Nejc,Nnuc).GT.0 .OR. FLGrel.EQ.'y') then
         merel = 2  ! relativistic kinematic only
C        merel = 1  ! relativistic kinematic + potential dependence
      ELSE
         merel = 0  ! non-relativistic kinematic only
      ENDIF

C     Coulomb correction for proton potentials 
      mecul = 0 ! Energy dependent   : DVc = - Ccoul*Z/A**(1/3) * dV/dE
C     mecul = 1 ! Energy independent : DVc = Ccoul*Z/A**(1/3)
C     mecul = 2 ! E = Ep - Ccoul*Z/A**(1/3) for real and imag potentials
C     mecul = 3 ! E = Ep - Ccoul*Z/A**(1/3) for real potential
      if(pot(2,1,25).ne.0.d0 .or. pot(4,1,25).ne.0.d0) mecul =2
      if(pot(1,1,25).ne.0.d0) mecul =3

      merzz = 1 ! Constant charge radius(0) Energy dependent(1)
      merrr = 1 ! Real potential radius is constant(0) Energy dependent(1)  

C     Dispersive potentials
      medis = 0 ! No dispersion
      if (abs(idr).eq.3) medis = 1 
      if (abs(idr).eq.2) medis = 2
C     If merip = 1 then potentials defined at every energy (loop over energies)
      merip = 0 
      if(idr.eq.0 .and. imodel.lt.4) merip = 1
c------------------------------------------------------------------------------

C   Defining potential depths (needs changes in OPTMAN)
C
C       OMPs: 1 - real vol
C       OMPs: 2 - imag vol
C       OMPs: 3 - real surf
C       OMPs: 4 - imag surf
C       OMPs: 5 - real SO
C       OMPs: 6 - imag SO
C
C     call optmod
C
C     Checking if OPTMAN can deal with them. 
C
      if((RVSo(Nejc,Nnuc).ne.0.d0 .and. 
     &  RWSo(Nejc,Nnuc).ne.0.d0) .and.
     +     RVSo(Nejc,Nnuc).ne.RWSo(Nejc,Nnuc) ) then
        write(8,*) 'ERROR: OPTMAN can not deal with different radii for 
     &real and imaginary SO potentials'  
        STOP       'ERROR: OPTMAN can not deal with different radii for 
     &real and imaginary SO potentials'  
      endif

      if((AVSo(Nejc,Nnuc).ne.0.d0 .and. 
     &  AWSo(Nejc,Nnuc).ne.0.d0) .and.
     +     AVSo(Nejc,Nnuc).ne.AWSo(Nejc,Nnuc) ) then
        write(8,*) 'ERROR: OPTMAN can not deal with different diffusenes
     &s for real and imaginary SO potentials'  
        STOP       'ERROR: OPTMAN can not deal with different diffusenes
     &s for real and imaginary SO potentials'  
      endif

C     ldwmax = 2.4*1.25*A(Nnuc)
C    &         **0.33333333*0.22*SQRT(xmas_nejc*elab)
C-----Maximum number of channel spin
C     njmax = MAX(2*ldwmax,20)
C
C-----Writing OPTMAN input
      OPEN (UNIT = 1,STATUS = 'unknown',FILE = 'OPTMAN.INP')
C-----CARD 1 : Title
      IF(imodel.ne.4) then
        WRITE (1,'(f10.5,'' MeV '',
     &   a8,'' on '',i3,a2,'': soft rotor, RIPL OMP # '',i5)')
     &   elab, PARname(ip), NINT(A(Nnuc)), Symb(Nnuc), iref
      ELSE
        WRITE (1,'(f10.5,'' MeV '',
     &   a8,'' on '',i3,a2,'': rigid+soft rotor, RIPL OMP # '',i5)')
     &   elab, PARname(ip), NINT(A(Nnuc)), Symb(Nnuc), iref
      ENDIF

      write(1,'(20i2.2)') 
     +mejob,mepot,meham,mepri,mesol,mesha,mesho,mehao,
     +meapp,mevol,merel,mecul,merzz,merrr,medis,merip,0,0,0,0
C
C     Soft rotor hamiltonian
C
      if(meham.gt.1) then
        write(1,'(6E12.5)') ! Record 3 from OPTMAN (Hamiltonian parameters)
     +    SR_Ham_hw,SR_Ham_amb0,SR_Ham_amg0,
     +    SR_Ham_gam0,SR_Ham_bet0,SR_Ham_bet4
        write(1,'(6E12.5)') ! Record 3 from OPTMAN (Hamiltonian parameters)
     +    SR_Ham_bb42,SR_Ham_gamg,SR_Ham_delg,
     +    SR_Ham_bet3,SR_Ham_et0,SR_Ham_amu0
        write(1,'(6E12.5)') ! Record 3 from OPTMAN (Hamiltonian parameters)
     +    SR_Ham_hw0 ,SR_Ham_bb32,SR_Ham_gamde,
     +    SR_Ham_dpar,SR_Ham_gshape
      endif

C     Default values
      npd = 8     ! maximum multipole in use 
      las = 8
      if(imodel.eq.1 .or. imodel.eq.4) then
C-------Deformation of rotational band (only ground state band is present)
C-------IdefCC   = maximum degree of deformation
C-------LMaxCC   = maximum L in multipole decomposition
        npd = IdefCC
        las = LMaxCC
      endif

      if(imodel.eq.3) then
        npd = 4   ! maximum multipole in use 
        las = 4
      endif

C     write(1,'(9I3)') ncol  ,ne,npd,las,mtet   ,90,200,180,0
C     write(1,'(9I3)') ncollm, 1,npd,las,NANgela,90,200,180,1  ! KODMA = 1
C
C     KODMA 0 needed as coupled states are not ordered
C
      write(1,'(9I3)') ncollm, 1,npd,las,NANgela,90,200,180,0  ! KODMA = 0

      write(1,'(6e12.5)') elab
      if(ZEJc(Nejc).gt.0) then
        write(1,'(36I2.2)') 1
        ANGles(1) = 0.5d0
      else
        write(1,'(36I2.2)') 0
      endif
      write(1,'(6e12.5)') (ANGles(i),i=1,NANgela) 

C     WRITE(1, '(f5.2,2i2,a1,5f10.5)')XJLV(1,NNUC),0,1, ch, elab,
C     WRITE (1,'(f5.2,2i2,a1,f10.6,4F10.5)') zerosp, 0, 1, '+', elab,
C    &                              SEJc(Nejc), xmas_nejc, xmas_nnuc,
C    &                              Z(Nnuc)*ZEJc(Nejc)

      if(imodel.ge.1) then
        if(meham.eq.1 .and. imodel.ne.4) then ! rigid rotor
          do k=1,ND_nlv
            IF(ICOllev(k).GT.LEVcc) CYCLE
            iparit = -1
            if(D_Lvp(k).gt.0.d0) iparit = +1
C           Normal excited states (for n,n or p,p scattering) 
            write(1,'(E12.5,1x,4I2)') 
     +        D_Elv(k),nint(2*D_Xjlv(k)),iparit,nint(2*D_Xjlv(1)),0
C
C           if(ex(k,ntar).GT.100.d0) then
C             Isobar analogue states (for p,n scattering) 
C             write(1,'(E12.5,1x,4I2)') 
C    +        ex(k,ntar)-100.d0,nint(2*spin(k,ntar)),ipar(k,ntar),
C    +                            nint(2*spin(1,ntar)), 1
C           endif
          enddo
        else                    
          if(imodel.ne.4) then ! soft rotor
C     soft-rotor      
C     READ(20,43)(EL(I),JO(I),NPO(I), NTU(I),NNB(I),NNG(I),
C    *                 NNO(I),NCA(I),I=1,NUR)
C  43 FORMAT(E12.7,7I2)
            do k=1,ND_nlv
              IF(ICOllev(k).GT.LEVcc) CYCLE
              iparit = -1
              if(D_Lvp(k).gt.0.d0) iparit = +1
              write(1,'(E12.5,6I2)') 
     +          D_Elv(k), nint(2*D_Xjlv(k)), iparit,
     +          IPH(k), D_Klv(k), D_Llv(k), D_nno(k) 
            enddo
          else                 ! rigid (or rigid-soft) rotor
C     READ(20,3)(EL(I),JO(I),NPO(I),KO(I),NCA(I),
C    *     NUMB(I),BETB(I),I=1,NUR)
C   3 FORMAT(E12.7,5I2,E12.7)
            do k=1,ND_nlv
              IF(ICOllev(k).GT.LEVcc) CYCLE
              iparit = -1
              if(D_Lvp(k).gt.0.d0) iparit = +1
              write(1,'(E12.5,5I2,E12.5)') 
     +          D_Elv(k), nint(2*D_Xjlv(k)), iparit,
     +          IPH(k), D_Klv(k), D_Llv(k), D_Def(k,2)
         ! D_nno(k) is always zero in the rigid rotor model      
            enddo
          endif
        endif
      endif

C     This line is new for OPTMAN v.12 (from November 2011)
C     Flags for inclusion of resonances
      write(1,'(I3)') 0   ! no resonances are considered

C     Ef=dble(int(100000*efermi))/100000
      Ef=EEFermi(Nejc,Nnuc)
      if(pot(1,1,18).ne.0.) Ef=pot(1,1,18) + pot(1,1,19)*atar
C     Ef=dble(int(100000*(pot(1,1,18) + pot(1,1,19)*atar)))/100000

      write(1,'(6g12.5)') xmas_nejc,SEJc(Nejc),xmas_nnuc,Z(0),Ef,Ef

      rc = RCOul(Nejc,Nnuc)
      ac = ACOul(Nejc,Nnuc)
      if(ZEJc(Nejc).eq.0) then
        rc = 1.d0
        ac = 1.d0
      endif
C
C     Dispersive deformed potentials are treated exactly.
C     All others potentials are calculated at a fixed energy, 
C     therefore loop over incident energies is needed. 
C
C     if(imodel.eq.3 .or. (idr.ne.0 .and. imodel.eq.1) .or.
C    +   imodel.eq.4 ) then 

      write(1,'(6g12.5)')
C               Vr0                         Vr1       
     +    pot(1,1,14)+pot(1,1,15)*iatar, -pot(1,1,3)-pot(1,1,4)*iatar, 
C               Vr2                         Vr3      
     +    pot(1,1,5) +pot(1,1,6) *iatar, -pot(1,1,7), 
C          VRLA         ALAVR  
     +    pot(1,1,16), pot(1,1,17)

      write(1,'(6g12.5)')  0.d0, 0.d0, 0.d0, 
C                        W0       
     +    pot(4,1,1) +pot(4,1,7)*iatar +pot(4,1,9)*iatar**(-1.d0/3.d0), 
C            Bs          Cs
     +    pot(4,1,6), pot(4,1,2)    

      write(1,'(6g12.5)')  0.d0, 0.d0, 0.d0, 
C             Av=b(2,j,6)                  Bv=b(2,j,7)
     +    pot(2,1,1)+pot(2,1,2)*iatar, pot(2,1,3)+pot(2,1,4)*iatar, 0.d0    

      write(1,'(6g12.5)')
C               Vso                        Lso                   
     +     pot(5,1,10)+pot(5,1,11)*iatar, pot(5,1,12), 0.d0, 0.d0, 
C                 Wso        Bso
     +     pot(6,1,1), pot(6,1,3)
         
C                                                   nv
      write(1,'(6g12.5)')  RVOm(Nejc,Nnuc), 0.d0, 0.d0, 
     &                           pot(2,1,13), AVOm (Nejc,Nnuc), 0.d0    
      write(1,'(6g12.5)') RWOm(Nejc,Nnuc) ,   AWOm (Nejc,Nnuc), 0.d0,
     &                    RWOmv(Nejc,Nnuc),   AWOmv(Nejc,Nnuc), 0.d0          
      write(1,'(6g12.5)')1.d0, 1.d0, 0.d0 , 
     &                    RVSo(Nejc,Nnuc) ,   AVSo (Nejc,Nnuc), 0.d0                

        Ccoul = 0.d0
        if(izproj.gt.0 .and. rc.gt.0.d0 .and. mecul.eq.0) 
     +        Ccoul =  pot(1,1,9)*1.73/rc
        if(izproj.gt.0 .and. mecul.eq.3) Ccoul =  pot(1,1,25)

      write(1,'(6g12.5)')  rc, 0.d0, 0.d0, ac,Ccoul, 1.d0                      
      write(1,'(6g12.5)')
C                 Cviso          Cwiso                      Ea
     +    abs(pot(1,1,20)),abs(pot(4,1,8)), 0.d0, pot(2,1,21)

      AlphaV = pot(2,1,22)
      if(pot(2,1,21).ne.0.d0 .and. AlphaV.eq.0.d0) AlphaV=1.65d0

C     This line is new for OPTMAN v.10 (from March 2008)

C     Modified for OPTMAN v.12 
      write(1,'(6g12.5)') AlphaV, 0.d0, 0.d0, 0.d0, 0.d0, 0.d0


C     This line is new for OPTMAN v.12 (from November 2011)
      write(1,'(6g12.5)') 0.d0, A(Nnuc)

      if(meham.eq.1 .or. imodel.eq.4 ) ! rigid rotor
     +  write(1,'(6g12.5)') (D_Def(1,k),k = 2,IDEfcc,2) 
      close(1) 
C
C     OPTMAN input done !
C
C-----Running OPTMAN
C
      IF(inc_channel) write (*,*) '  Running OPTMAN ..zz..'

      ctmp = trim(empiredir)//'/source/optmand'
      iwin = ipipe(ctmp)
 
      IF(TL_calc) RETURN

      iwin = ipipe_move('OPTMAN.INP','OPTMAN-INC.inp')
      iwin = ipipe_move('OPTMAN.OUT','OPTMAN-INC.out')

      RETURN
      END


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
      SUBROUTINE OPTMOD(El,Vlib,Rlib,Alib)
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

      EXTERNAL DELTA_WD, DELTA_WV, WDF, WVF
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
      if(rco(i,j,13).eq.0.) then
        rlib(i)=abs(rco(i,j,1)) + rco(i,j,3)*eta
     *       + rco(i,j,4)/atar + rco(i,j,5)/sqrt(atar)
     *       + rco(i,j,6)*atar**(2./3.) + rco(i,j,7)*atar
     *       + rco(i,j,8)*atar**2  + rco(i,j,9)*atar**3
     *       + rco(i,j,10)*atar**(1./3.)
     *       + rco(i,j,11)*atar**(-1./3.)
C--------------------------------------------------------------------
C     RCN, 08/2004, to handle new extension to the OMP RIPL format
     *       + rco(i,j,2)*el + rco(i,j,12)*el*el
      else
C     RCN, 09/2004, to handle new extension to the OMP RIPL format
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

C         Numerical DOM integral (used in RIPL released interface)  
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
c        To preserve compatibility with RIPL Koning database
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
C     LOG(eplus) -> LOG(DABS(eplus)) as E+ could be a negative quantity
C     Jan 2011, M Pigni and R Capote
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
     &           *(eplus**N + Bv**N*(1.D0 + N*LOG(DABS(eplus))))
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
      DOM_INT_WV = -Av/pi*(rs/N + reseplus*LOG(DABS(eplus))
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
