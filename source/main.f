Ccc   * $Author: mike $
Ccc   * $Date: 2001-07-09 17:33:39 $
Ccc   * $Id: main.f,v 1.1.1.1 2001-07-09 17:33:39 mike Exp $
C
      PROGRAM EMPIRE
Ccc
Ccc   ********************************************************************
Ccc   *                                                         class:ppu*
Ccc   *                         E M P I R E                              *
Ccc   *                                                                  *
Ccc   *     Main of the EMPIRE code. A full call structure tree is       *
Ccc   *     given below in alphabetical order:                           *
Ccc   *                                                                  *
Ccc   *    ACCUM                                                         *
Ccc   *        BELLAC                                                    *
Ccc   *    ACCUMSD                                                       *
Ccc   *    AUERST                                                        *
Ccc   *    DECAY                                                         *
Ccc   *        TLLOC                                                     *
Ccc   *    DECAYD                                                        *
Ccc   *    DECAYG                                                        *
Ccc   *        E1                                                        *
Ccc   *        E2                                                        *
Ccc   *        XM1                                                       *
Ccc   *    DECAYT                                                        *
Ccc   *        E1                                                        *
Ccc   *        E2                                                        *
Ccc   *        XM1                                                       *
Ccc   *    FISSION                                                       *
Ccc   *        TLF                                                       *
Ccc   *    HMSC                                                          *
Ccc   *        AUERST                                                    *
Ccc   *        DECHMS                                                    *
Ccc   *        E1                                                        *
Ccc   *        GDOWN                                                     *
Ccc   *            W                                                     *
Ccc   *                W1                                                *
Ccc   *                W2                                                *
Ccc   *                W3                                                *
Ccc   *        MATIN                                                     *
Ccc   *        MATIN1                                                    *
Ccc   *        OMJ                                                       *
Ccc   *        ROPHM                                                     *
Ccc   *        TRATES                                                    *
Ccc   *        VQ                                                        *
Ccc   *            WOBL                                                  *
Ccc   *        W (see above)                                             *
Ccc   *        WHERE                                                     *
Ccc   *        WILLI                                                     *
Ccc   *        WOBL                                                      *
Ccc   *        WT                                                        *
Ccc   *            W (see above)                                         *
Ccc   *        ZERO                                                      *
Ccc   *            W (see above)                                         *
Ccc   *    INPUT                                                         *
Ccc   *        BNDG                                                      *
Ccc   *            WHERE                                                 *
Ccc   *        CLEAR                                                     *
Ccc   *        LEVREAD                                                   *
Ccc   *            BCDNUM                                                *
Ccc   *        PTLEVSET                                                  *
Ccc   *            PTLEVRE                                               *
Ccc   *        READIN                                                    *
Ccc   *            WHERE                                                 *
Ccc   *        READLDP                                                   *
Ccc   *            FSHELL                                                *
Ccc   *            WHERE                                                 *
Ccc   *        READNIX                                                   *
Ccc   *            SHELLC                                                *
Ccc   *                LYMASM                                            *
Ccc   *                    XI                                            *
Ccc   *                    XIMOD                                         *
Ccc   *            WHERE                                                 *
Ccc   *        ROCOL                                                     *
Ccc   *            ALIT                                                  *
Ccc   *            BARFIT                                                *
Ccc   *                LPOLY                                             *
Ccc   *            MOMFIT                                                *
Ccc   *                LPOLY                                             *
Ccc   *            RODEF                                                 *
Ccc   *                DAMPV                                             *
Ccc   *                VIBR                                              *
Ccc   *            SHCFADE                                               *
Ccc   *            SIGMAK                                                *
Ccc   *        ROEMP                                                     *
Ccc   *            BNDG (see above)                                      *
Ccc   *            DAMIRO                                                *
Ccc   *                FSHELL                                            *
Ccc   *                MOMFIT (see above)                                *
Ccc   *                ROBCS                                             *
Ccc   *                    DAMPKS                                        *
Ccc   *                    VIBR                                          *
Ccc   *                RODEF (see above)                                 *
Ccc   *                SIGMAK                                            *
Ccc   *            FSHELL                                                *
Ccc   *            PIPE                                                  *
Ccc   *            PRERO                                                 *
Ccc   *                BARFIT (see above)                                *
Ccc   *                SHCFADE                                           *
Ccc   *                SIGMAK                                            *
Ccc   *        ROGC                                                      *
Ccc   *            BARFIT (see above)                                    *
Ccc   *            PIPE                                                  *
Ccc   *            RIVOLI                                                *
Ccc   *            SHCFADE                                               *
Ccc   *            SIGMAK                                                *
Ccc   *        SMAT                                                      *
Ccc   *        TLEVAL                                                    *
Ccc   *            OMTL                                                  *
Ccc   *                FACT                                              *
Ccc   *                PREANG                                            *
Ccc   *                PRIPOT                                            *
Ccc   *                PRITC                                             *
Ccc   *                PRITD                                             *
Ccc   *                SCAT                                              *
Ccc   *                    INTEG                                         *
Ccc   *                    RCWFN                                         *
Ccc   *                SETPOTS                                           *
Ccc   *                    OMPAR                                         *
Ccc   *                SHAPEC                                            *
Ccc   *                    CGAMMA                                        *
Ccc   *                SHAPEL                                            *
Ccc   *                    CLEB                                          *
Ccc   *                    RACAH                                         *
Ccc   *                SPIN0                                             *
Ccc   *                SPIN05                                            *
Ccc   *                SPIN1                                             *
Ccc   *        WHERE                                                     *
Ccc   *    MARENG                                                        *
Ccc   *        HITL                                                      *
Ccc   *            BNDG (see above)                                      *
Ccc   *            CCFUS                                                 *
Ccc   *                BAR                                               *
Ccc   *                    POTENT                                        *
Ccc   *                        POT                                       *
Ccc   *                POT                                               *
Ccc   *            PUSH                                                  *
Ccc   *                F                                                 *
Ccc   *                G                                                 *
Ccc   *                    F                                             *
Ccc   *                INTGRS                                            *
Ccc   *            XFUS                                                  *
Ccc   *        OMTL (see above)                                          *
Ccc   *    ORION                                                         *
Ccc   *        CCCTRL                                                    *
Ccc   *            FLGLCH                                                *
Ccc   *            OMPOTEN                                               *
Ccc   *        FFCAL                                                     *
Ccc   *        HIBORN                                                    *
Ccc   *            MSTEP                                                 *
Ccc   *        OPMPARN                                                   *
Ccc   *            OMPAR                                                 *
Ccc   *            WHERE                                                 *
Ccc   *        XSEC                                                      *
Ccc   *            LEGNDR                                                *
Ccc   *            XSC12                                                 *
Ccc   *                CLEBSCH                                           *
Ccc   *                XSCABC                                            *
Ccc   *                    CLEBHF                                        *
Ccc   *                    CLEBRD                                        *
Ccc   *                    CLEBZ                                         *
Ccc   *                    RACHLF                                        *
Ccc   *                    RACSIM                                        *
Ccc   *    PRINPUT                                                       *
Ccc   *    ROCOL (see above)                                             *
Ccc   *    ROEMP (see above)                                             *
Ccc   *    TRISTAN                                                       *
Ccc   *        RESPNS                                                    *
Ccc   *            INELAS                                                *
Ccc   *                CLEBTRI                                           *
Ccc   *                DWIDTH                                            *
Ccc   *                RADIAL                                            *
Ccc   *            SPLVL                                                 *
Ccc   *                BCS                                               *
Ccc   *                    ESORT                                         *
Ccc   *                    NUMBER                                        *
Ccc   *                EHO                                               *
Ccc   *                ESORT                                             *
Ccc   *                INDF                                              *
Ccc   *        SPECTR                                                    *
Ccc   *            INVERT                                                *
Ccc   *            LSQLEG                                                *
Ccc   *                MTXGUP                                            *
Ccc   *                PLNLEG                                            *
Ccc   *            PNORM                                                 *
Ccc   *                POLYNM                                            *
Ccc   *            POLYNM                                                *
Ccc   *    ULM                                                           *
Ccc   *    ULMDYN                                                        *
Ccc   *    WHERE                                                         *
Ccc   *                                                                  *
Ccc   *                                                                  *
Ccc   * author: M.Herman                                                 *
Ccc   * date:   June 1994                                                *
Ccc   *                                                                  *
Ccc   *                                                                  *
Ccc   ********************************************************************
Ccc
      INCLUDE 'dimension.h'
      INCLUDE 'global.h'
C
C     COMMON variables
C
      DOUBLE PRECISION ELAcs, ELAda(101), TOTcs
      INTEGER NELang
C-----next COMMON is to transfer elastic ddx from Scat-2
      COMMON /ELASCAT/ ELAda, TOTcs, ELAcs, NELang
C
C     Local variables
C
      DOUBLE PRECISION aorg, ares, corr, corrmsd, csemist, 
     &                 csfis, cturbo, ded, delang, elcncs, 
     &                 gamfis, gamt, pope, popleft, poplev, 
     &                 poptot, 
     &                 popread, dang, coef, ecm, echannel, erecoil, 
     &                 csmsdl, xcse, gang, 
     &                 q2, q3, qmax, qstep, recorp, recorr, 
     &                 sgamc, spdif, spdiff, stauc, step, sum, sumfis, 
     &                 tauf, taut, weight, xccm, xizat, xnhms, xnl, 
     &                 xnor, zorg, zres
      REAL FLOAT
      INTEGER i, ia, iad, iam, iang, ib, iccmh, iccml, icse, ie, il, 
     &        ilast, iloc, imt, iorg, ip, ipar, itimes, its, iz, 
     &        izaorg, izares, j, jcn, ke, kemax, kemin, 
     &        ltrmax, mt649, mt849, mt91, nang, nbr, nejc, ngspec, nnuc, 
     &        nnur, nnurn, nnurp, nspec, irec, icsl, icsh
      INTEGER INT, MIN0
      LOGICAL nvwful
      INCLUDE 'io.h'
C
C-----
C-----read and prepare input data
C-----
 1400 CALL INPUT
C-----
C-----print input data
C-----
      IF(IOUt.GT.0)CALL PRINPUT
      WRITE(*, '('' C.M. incident energy '',G10.5,'' MeV'')')EIN
C-----
C-----calculate reaction cross section and its spin distribution
C-----
      CALL MARENG(0, 0)
C-----
C-----get ECIS results
C-----
      IF(DIRect.GT.0)THEN
C--------locate position of the target among residues
         CALL WHERE(IZA(1) - IZAejc(0), nnurec, iloc)
C--------locate position of the projectile among ejectiles
         CALL WHEREJC(IZAejc(0), nejcec, iloc)
         IF((MODelecis.GT.0 .AND. DIRect.NE.3) .OR. DIRect.EQ.2)THEN
            OPEN(45, FILE = 'ecis95.cs', STATUS = 'OLD')
            READ(45, *, END = 1500)TOTcs
            READ(45, *, END = 1500)ecisabs
            CLOSE(45)
         ENDIF
         NELang = 19
         ecm = EINl - EIN
         dang = 3.14159/FLOAT(NELang - 1)
         OPEN(45, FILE = 'ecis95.ang', STATUS = 'OLD')
         IF((MODelecis.GT.0 .AND. DIRect.NE.3) .OR. DIRect.EQ.2)THEN
            READ(45, *, END = 1500)ELAcs
         ELSE
            READ(45, *, END = 1500)
         ENDIF
C--------Checking if CC OMP was used
         DO iang = 1, NELang
            IF((MODelecis.GT.0 .AND. DIRect.NE.3) .OR. DIRect.EQ.2)THEN
               READ(45, '(15x,E12.5)', END = 1500)ELAda(iang)
            ELSE
C--------------dummy read
               READ(45, '(15x,E12.5)', END = 1500)
            ENDIF
         ENDDO
C--------get and add inelastic cross sections (including double-differential)
         ncoll = 0
         DO i = 2, ND_nlv
            ilv = ICOllev(i)
            IF(ilv.GT.NLV(nnurec))GOTO 1500
            echannel = EX(NEX(1), 1) - Q(nejcec, 1) - ELV(ilv, nnurec)
C-----------avoid reading closed channels
            IF(echannel.GE.0.05)THEN
               xcse = echannel/DE + 1.0001
               icsl = INT(xcse)
               icsh = icsl + 1
               READ(45, *, END = 1500)popread
               ncoll = i
               POPlv(ilv, nnurec) = POPlv(ilv, nnurec) + popread
               CSEmis(nejcec, 1) = CSEmis(nejcec, 1) + popread
C--------------add direct transition to the spectrum
               popl = popread*(FLOAT(icsh) - xcse)/DE
               IF(icsl.EQ.1)popl = 2.0*popl
               poph = popread*(xcse - FLOAT(icsl))/DE
               CSE(icsl, nejcec, 1) = CSE(icsl, nejcec, 1) + popl
               CSE(icsh, nejcec, 1) = CSE(icsh, nejcec, 1) + poph
               DO iang = 1, NELang
                  READ(45, '(15x,E12.5)', END = 1500)
     &                 CSAlev(iang, ilv, nejcec)
               ENDDO
C--------------construct recoil spectra due to direct transitions
               IF(ENDf.EQ.2)THEN
                  coef = 2*PI*dang/DERec
                  echannel = echannel*AEJc(0)/A(1)
                  DO iang = 1, NELang
                     erecoil = ecm + echannel + 2*SQRT(ecm*echannel)
     &                         *CANgler(iang)
                     irec = erecoil/DERec + 1.001
                     weight = (erecoil - (irec - 1)*DERec)/DERec
                     IF(irec + 1.GT.NDEREC)GOTO 1450
                     csmsdl = CSAlev(iang, ilv, nejcec)*SANgler(iang)
     &                        *coef
                     RECcse(irec, 0, nnurec) = RECcse(irec, 0, nnurec)
     &                  + csmsdl*(1.0 - weight)
                     RECcse(irec + 1, 0, nnurec)
     &                  = RECcse(irec + 1, 0, nnurec) + csmsdl*weight
                  ENDDO
               ENDIF
            ENDIF
 1450    ENDDO
C--------print elastic and direct cross sections from ECIS
 1500    WRITE(6, *)' '
         WRITE(6, *)' '
         IF((MODelecis.EQ.0 .AND. DIRect.EQ.1) .OR. DIRect.EQ.3)THEN
            WRITE(6, *)' '
            IF(DIRect.EQ.1)THEN
               WRITE(6, *)' Inelastic scattering results provided by'
               WRITE(6, *)' Coupled Channel calculations with ECIS:'
            ENDIF
            IF(DIRect.EQ.3)THEN
               WRITE(6, *)' Inelastic scattering results provided by'
               WRITE(6, *)' DWBA calculations with ECIS:'
            ENDIF
            WRITE(6, *)' '
            gang = 180.0/(NELang - 1)
            WRITE(6, 99001)(ICOllev(ilv), ilv = 2, ncoll)
99001       FORMAT('  Angle ', 10(6x, i2, '-level'))
            WRITE(6, *)' '
            DO iang = 1, NELang
               WRITE(6, 99002)(iang - 1)*gang, 
     &                        (CSAlev(iang, ICOllev(ilv), nejcec), 
     &                        ilv = 2, ncoll)
99002          FORMAT(1x, f5.0, 3x, 11(2x, E12.6))
            ENDDO
            WRITE(6, *)' '
            WRITE(6, 99003)(POPlv(ICOllev(ilv), nnurec), ilv = 2, ncoll)
99003       FORMAT(6x, 3x, 11(2x, E12.6))
            WRITE(6, *)' '
            WRITE(6, *)' '
            WRITE(6, *)' '
         ENDIF
         IF((MODelecis.GT.0 .AND. DIRect.NE.3) .OR. DIRect.EQ.2)THEN
            WRITE(6, *)' '
            WRITE(6, *)
     &               ' Results provided by Coupled Channel calculations'
     &               , ' with ECIS code:'
            WRITE(6, 99004)TOTcs, ecisabs, ELAcs
99004       FORMAT(/, 2x, 'Total cross section         :', e14.7, ' mb',
     &             /, 2x, 'Absorption cross section    :', e14.7, ' mb',
     &             /, 2x, 'Shape elastic cross section :', e14.7, ' mb',
     &             //)
            gang = 180.0/(NELang - 1)
            WRITE(6, 99005)(ICOllev(ilv), ilv = 2, ncoll)
99005       FORMAT('  Angle    Elastic', 10(6x, i2, '-level'))
            WRITE(6, *)' '
            DO iang = 1, NELang
               WRITE(6, 99006)(iang - 1)*gang, ELAda(iang), 
     &                        (CSAlev(iang, ICOllev(ilv), nejcec), 
     &                        ilv = 2, ncoll)
99006          FORMAT(1x, f5.0, 11(2x, E12.6))
            ENDDO
            WRITE(6, *)' '
            WRITE(6, 99007)ELAcs, 
     &                     (POPlv(ICOllev(ilv), nnurec), ilv = 2, ncoll)
99007       FORMAT(6x, 11(2x, E12.6))
            WRITE(6, *)' '
            WRITE(6, *)' '
            WRITE(6, *)' '
         ENDIF
      ENDIF
C-----locate postions of ENDF MT-numbers 91, 649, and 849
      CALL WHERE(IZA(1) - IZAejc(1), mt91, iloc)
      CALL WHERE(IZA(1) - IZAejc(2), mt649, iloc)
      CALL WHERE(IZA(1) - IZAejc(3), mt849, iloc)
C-----
C-----calculate MSD contribution
C-----
      IF(MSD.NE.0 .AND. EIN.GT.5.D0)THEN
C--------call ORION
         REWIND 15
         WRITE(6,*)' ' 
         qmax = 0.99*EIN
         qstep = qmax/3.0
         ltrmax = 4
         IF(NLW.LE.10)ltrmax = 3
         IF(NLW.LE.8)ltrmax = 2
         IF(NLW.LE.6)ltrmax = 1
         IF(MSD.NE.2)THEN
            q2 = qmax
            q3 = qmax
 1520       CALL ORION(q2, q3, 1, EIN, NLW, 1, ltrmax, A(0), Z(0), 
     &                 AEJc(0), ZEJc(0), IOUt, ANGles, NDANG, ICOmpff)
            WRITE(6,'('' ORION calculated for Q2='', F7.3, '' and Q3='',
     &            F7.3)') q2, q3
            q2 = q2 - qstep
            IF(q2.LT.( - 0.0001D0))THEN
               q3 = q3 - qstep
               IF(q3.LT.( - 0.0001D0))GOTO 1550
               q2 = q3
            ENDIF
C-----------set to Q's to 0 if negative due to rounding error
            IF(q2.LT.0.0D0)q2 = 0.0
            IF(q3.LT.0.0D0)q3 = 0.0
            GOTO 1520
         ENDIF
 1550    REWIND 15
         WRITE(6,*)' ' 
         WRITE(6,*)' ' 
         CALL ULM(1)
         CALL TRISTAN(0, 0, ltrmax, qmax, qstep)
C        CLOSE (15)
C--------print MSD double differential cross sections
         nejc = 1
         IF(ZEJc(0).EQ.1.0D0)nejc = 2
         IF(CSMsd(1).GT.0.D0 .AND. IOUt.GE.3)THEN
            itimes = FLOAT(NDANG)/11.0 + 0.95
            DO its = 1, itimes
               iad = 1 + (its - 1)*11
               iam = 11 + (its - 1)*11
               iam = MIN0(NDANG, iam)
               WRITE(6, *)' '
               WRITE(6, 
     &               '(30X,''A      n      g      l      e      s '')')
               WRITE(6, *)' '
               WRITE(6, '('' Energy  '',11(4X,F5.1,2X))')
     &               (ANGles(ia), ia = iad, iam)
               WRITE(6, *)' '
               DO i = 1, NEX(1)
                  WRITE(6, '(1X,F7.3,1X,11E11.4)')FLOAT(i - 1)*DE, 
     &                  (CSEa(i, iang, nejc, 1), iang = iad, iam)
               ENDDO
               WRITE(6, *)' '
            ENDDO
         ENDIF
         WRITE(6, *)' Neutron MSD cross section = ', CSMsd(1),' mb'
         WRITE(6, *)' Proton  MSD cross section = ', CSMsd(2),' mb'
         WRITE(6, *)' '
C--------correct CN population for the MSD emission
         corrmsd = (CSFus - CSMsd(1) - CSMsd(2))/CSFus
         IF(corrmsd.LT.0.0D0)THEN
            WRITE(6, *)' '
            WRITE(6, *)'MSD EMISSION LARGER THEN FUSION CROSS SECTION'
            IF(ICOmpff.GT.0) THEN 
               WRITE(6, *)'TRY TO TURN OFF COMPRESSIONAL FORM FACTOR '
               WRITE(6, *)'SETTING COMPFF TO 0 IN THE OPTIONAL INPUT.'
               STOP
            ELSE 
               WRITE(6, *)'THIS MAY HAPPEN IF RESPONSE FUNCTIONS ARE '
               WRITE(6, *)'RENORMALIZED IN INPUT OR FITTED TO WRONG '
               WRITE(6, *)'DISCRET LEVELS. CHECK `EFIT` AND `RESNOR` '
               WRITE(6, *)'IN OPTIONAL INPUT.    '
               WRITE(6, *)'IF THESE ARE FINE TRY ANOTHER OPTICAL MODEL.'
               STOP
            ENDIF 
         ENDIF
         DO i = 1, NLW
            POP(NEX(1), i, 1, 1) = POP(NEX(1), i, 1, 1)*corrmsd
            POP(NEX(1), i, 2, 1) = POP(NEX(1), i, 2, 1)*corrmsd
         ENDDO
C--------TRISTAN *** done ***
C--------add MSD contribution to the residual nucleus population
C--------locate residual nucleus after MSD emission
         ares = A(1) - AEJc(nejc)
         zres = Z(1) - ZEJc(nejc)
         izares = INT(1000.0*zres + ares)
         CALL WHERE(izares, nnur, iloc)
         IF(iloc.EQ.1)THEN
            WRITE(6, *)' RESIDUAL NUCLEUS WITH A=', ares, ' AND Z=', 
     &                 zres, ' HAS NOT BEEN INITIALIZED'
            WRITE(6, *)' EXECUTION STOPPED'
            STOP
         ENDIF
         IF(CSMsd(nejc).NE.0.0D0)CALL ACCUMSD(1, nnur, nejc)
C--------second chance preequilibrium emission after MSD emission
C--------neutron emission
         izares = INT(1000.0*Z(nnur) + A(nnur) - 1)
         CALL WHERE(izares, nnurn, iloc)
         IF(iloc.EQ.0)CALL SCNDPREEQ(nnur, nnurn, 1, 0)
         IF(iloc.EQ.0 .AND. IOUt.GT.2)CALL AUERST(nnur, 1)
C--------proton emission
         izares = izares - 1000
         CALL WHERE(izares, nnurp, iloc)
         IF(iloc.EQ.0)THEN
            CALL SCNDPREEQ(nnur, nnurp, 2, 1)
            IF(IOUt.GT.2)CALL AUERST(nnur, 2)
         ELSE
            CALL SCNDPREEQ(nnur, nnurp, 2, 2)
         ENDIF
C--------second chance preequilibrium *** done ***
      ENDIF
C-----
C-----MSD *** done ***
C-----
      ia = INT(A(1))
      IF(IOUt.GT.1)THEN
         WRITE(6, *)' '
         WRITE(6, *)' '
         WRITE(6, 
     &'(''  Compound nucleus '',I3,''-'',A2,                            
     &  '' spin distribution'')')ia, SYMb(1)
         WRITE(6, *)' -----------------------------------------'
         WRITE(6, *)' '
         DO i = 1, NLW
            IF(MOD(ia, 2).EQ.0)THEN
               WRITE(6, '(1X,I5,G12.5,5X,I5,G12.5)')i, 
     &               POP(NEX(1), i, 1, 1), ( - i), POP(NEX(1), i, 2, 1)
            ELSE
               WRITE(6, '(1X,I4,''/2'',G12.5,5X,I4,''/2'',G12.5)')
     &               2*i - 1, POP(NEX(1), i, 1, 1), ( - (2*i - 1)), 
     &               POP(NEX(1), i, 2, 1)
            ENDIF
         ENDDO
         WRITE(6, *)' '
      ENDIF
      IF(IOUt.GT.0)THEN
         IF(DIRect.EQ.0)THEN
            WRITE(6, '(''   Fusion cross section = '',G13.6,
     &                 '' mb '')')CSFus
         ELSEIF(DIRect.EQ.1)THEN
            WRITE(6, '(''   Fusion cross section = '',G13.6,
     &                 '' mb including'')')CSFus
            WRITE(6, '(''   CC inelastic to discrete levels = '',
     &                 G13.6,'' mb'')')SINl
            WRITE(6, '(''   Spin distribution does NOT contain'',
     &                 '' CC inelastic contribution '')')
         ELSEIF(DIRect.EQ.2)THEN
            CSFus = CSFus + SINl
            WRITE(6, '(''   Fusion cross section = '',G13.6,
     &                 '' mb including'')')CSFus
            WRITE(6, '(''   CC inelastic to discrete levels = '',
     &                 G13.6,'' mb'')')SINl
            WRITE(6, '(''   Spin distribution calculated using '',
     &                 ''CC transmission coefficients'')')
         ELSEIF(DIRect.EQ.3)THEN
            WRITE(6, '(''   Fusion cross section = '',G13.6,
     &                 '' mb including'')')CSFus
            WRITE(6, '(''   DWBA inelastic to discrete levels = '',
     &                 G13.6,'' mb'')')SINl
            WRITE(6, '(''   Spin distribution does NOT contain'',
     &                 '' DWBA inelastic contribution '')')
         ENDIF
      ENDIF
      IF(ENDf.EQ.0.0D0)THEN
         WRITE(12, '('' FUSION CROSS SECTION = '',G13.6, '' mb'')')CSFus
      ELSE
         WRITE(12, *)' '
         WRITE(12, '('' FUSION CROSS SECTION = '',G12.5,'' mb'')')CSFus
         WRITE(12, '('' TOTAL  CROSS SECTION = '',G13.6,'' mb'')')TOTcs
         WRITE(12, *)' '
      ENDIF
      POPmax(1) = CSFus*1.0E-25
C-----renormalization of CN spin distribution if TURBO mode invoked
      IF(LTUrbo.NE.1)THEN
         IF(IOUt.GT.0)WRITE(6, 
     &         '('' TURBO mode with LTURBO='',I1,'' has been invoked'')'
     &         )LTUrbo
         cturbo = 0.0
         DO i = 1, NLW, LTUrbo
            cturbo = cturbo + POP(NEX(1), i, 1, 1)
     &               + POP(NEX(1), i, 2, 1)
         ENDDO
         cturbo = CSFus/cturbo
         DO i = 1, NLW, LTUrbo
            POP(NEX(1), i, 1, 1) = POP(NEX(1), i, 1, 1)*cturbo
            POP(NEX(1), i, 2, 1) = POP(NEX(1), i, 2, 1)*cturbo
         ENDDO
      ENDIF
C-----start DO loop over decaying nuclei
      DO nnuc = 1, NNUcd
         ia = INT(A(nnuc))
C--------reset variables for life-time calculations
         stauc = 0.0
         sgamc = 0.0
         csemist = 0.0
         csfis = 0.0
         sumfis = 0.0
         IF(IOUt.GT.0)THEN
            WRITE(6, *)' '
            WRITE(6, *)' '
            WRITE(6, *)' -------------------------------------'
            WRITE(6, '(I3,2X,''Decaying nucleus '',I3,''-'',A2)')nnuc, 
     &            ia, SYMb(nnuc)
            WRITE(6, *)' -------------------------------------'
            WRITE(6, *)' '
         ENDIF
         IF(ENDf.NE.0.0D0)THEN
            WRITE(12, *)' '
            WRITE(12, *)' --------------------------------------------'
            WRITE(12, 
     &'(''  Decaying nucleus '',I3,''-'',A2,''-'',I3,                   
     &  ''  mass='',F10.6)')INT(Z(nnuc)), SYMb(nnuc), ia, AMAss(nnuc)
            WRITE(12, *)' --------------------------------------------'
            IF(nnuc.NE.1)THEN
               WRITE(12, 
     &'(1X,/,10X,''Discrete level population '',                        
     &''before gamma cascade'')')
               WRITE(12, '(1X,/,10X,40(1H-),/)')
               DO il = 1, NLV(nnuc)
C--------------check for the number of branching ratios
                  nbr = 0
                  DO ib = 1, NDBR
                     IF(IBR(il, ib, 2, nnuc).EQ.0)GOTO 1555
                     nbr = ib
                  ENDDO
 1555             IF(nbr.EQ.0 .AND. il.NE.1 .AND. 
     &               (nnuc.EQ.mt91 .OR. nnuc.EQ.mt649 .OR. 
     &               nnuc.EQ.mt849))WRITE(6, *)
     &               ' WARNING!!!: BRANCHING RATIOS FOR LEVEL ', il, 
     &               ' IN ', INT(A(nnuc)), '-', SYMb(nnuc), 
     &               ' ARE MISSING'
                  WRITE(12, 99013)il, ELV(il, nnuc), LVP(il, nnuc), 
     &                            XJLv(il, nnuc), POPlv(il, nnuc), nbr, 
     &                            (IBR(il, ib, 1, nnuc), 
     &                            IBR(il, ib, 2, nnuc), ib = 1, nbr)
C-----------------next IF moves levels population to the ground state
C-----------------to avoid gamma cascade between discrete levels
C-----------------originating from the direct population
C-----------------of discrete levels by a neutron, proton or alpha.
C-----------------These gammas should not go into MT=91, 649, or 849.
                  IF((nnuc.EQ.mt91 .OR. nnuc.EQ.mt649 .OR. nnuc.EQ.mt849
     &               ) .AND. il.NE.1)THEN
                     POPlv(1, nnuc) = POPlv(1, nnuc) + POPlv(il, nnuc)
                     POPlv(il, nnuc) = 0.0
                  ENDIF
               ENDDO
C--------------temporary output to check dir contribution to disc levels         
C              gang=10.
C              IF(nnuc.EQ.mt91) THEN 
C              nejcec=1
C              ELSEIF(nnuc.EQ.mt649) THEN  
C              nejcec=2
C              ELSEIF(nnuc.EQ.mt849) THEN  
C              nejcec=3
C              ELSE
C                 WRITE(6,*)'SALTO Nnuc= ',Nnuc 
C                 GO TO 3392
C              ENDIF 
C              WRITE(6, 9995)(ilv, ilv = 1, 11)
C9995          FORMAT('  Angle  ', 11(6x, i2, '-level'))
C              WRITE(6, *)' '
C              DO iang = 1, NDANG   
C                 WRITE(6, 99006)(iang - 1)*gang,
C    &               (CSAlev(iang, il, nejcec),il=1,11)
C              ENDDO
C              WRITE(6, *)' '
C              WRITE(6, 9995)(ilv, ilv = 12, 22)
C              WRITE(6, *)' '
C              DO iang = 1, NDANG   
C                 WRITE(6, 99006)(iang - 1)*gang,
C    &               (CSAlev(iang, il, nejcec),il=12,22)
C              ENDDO
C              WRITE(6, *)' '
C              WRITE(6, 9995)(ilv, ilv = 23,NLV(nnuc) )
C              WRITE(6, *)' '
C              DO iang = 1, NDANG   
C                 WRITE(6, 99006)(iang - 1)*gang,
C    &               (CSAlev(iang, il, nejcec),il=23, NLV(nnuc))
C              ENDDO
C3392          CONTINUE
C--------------temporary output *** done ***
               WRITE(12, '(1X,/,10X,40(1H-),/)')
C--------------write elastic to tape 12
               IF(nnuc.EQ.mt91)THEN
                  WRITE(12, *)' '
                  WRITE(12, 
     &                  '('' ELASTIC CROSS SECTION ='',G12.5,'' mb'')')
     &                  ELAcs
                  WRITE(12, *)' '
                  WRITE(12, *)' Elastic angular distribution '
                  WRITE(12, *)' '
                  delang = 180./FLOAT(NELang - 1)
                  WRITE(12, 99008)(FLOAT(iang - 1)*delang, iang = 1, 
     &                            NELang)
99008             FORMAT(10X, 8G15.5)
                  WRITE(12, 99009)(ELAda(iang) + elcncs, iang = 1, 
     &                            NELang)
99009             FORMAT(9X, 8E15.5)
                  WRITE(12, *)' '
               ENDIF
            ENDIF
         ENDIF
         POPmax(nnuc) = POPmax(nnuc)*0.0001
         IF(POPmax(nnuc).EQ.0.0D0)THEN
            WRITE(6, *)' '
            WRITE(6, *)
     &                'Continuum of this nucleus has not been populated'
            GOTO 1600
         ENDIF
C--------prepare gamma transition parameters
         CALL ULM(nnuc)
C--------calculate compound nucleus level density at saddle point
         IF(FISsil(nnuc))THEN
            IF(ADIv.EQ.0.0D0)CALL ROEMP(nnuc, 1.D0, 0.0D0)
            IF(ADIv.EQ.1.0D0)CALL ROCOL(nnuc, 1.D0, 2.D0)
            IF(ADIv.GT.3.0D0)CALL ROCOL(nnuc, 1.D0, 1.D0)
            IF(ADIv.EQ.2.0D0)WRITE(6, *)
     &  ' MUST NOT USE GILBERT-CAMERON LEVEL DENSITIES FOR SADDLE POINT'
            IF(IOUt.EQ.6)THEN
               WRITE(6, '(1X,/,'' Saddle point level density'',/)')
               WRITE(6, 99010)(EX(i, nnuc), (ROF(i,j,nnuc), j = 1, 12), 
     &                        i = 1, NEX(nnuc))
Cpr            WRITE(6,20) (EX(I,NNUC),(ROF(I,J,NNUC),J=13,24),I=1,NEX(NNUC))
Cpr            WRITE(6,20) (EX(I,NNUC),(ROF(I,J,NNUC),J=25,36),I=1,NEX(NNUC))
Cpr            WRITE(6,20) (EX(I,NNUC),(ROF(I,J,NNUC),J=37,48),I=1,NEX(NNUC))
Cpr            WRITE(6,20) (EX(I,NNUC),(ROF(I,J,NNUC),J=49,60),I=1,NEX(NNUC))
99010          FORMAT(1X, 13G10.4)
            ENDIF
         ENDIF
C--------locate residual nuclei
         DO nejc = 1, NEJcm
            ares = A(nnuc) - AEJc(nejc)
            zres = Z(nnuc) - ZEJc(nejc)
            izares = INT(1000.0*zres + ares)
            CALL WHERE(izares, nnur, iloc)
            IF(iloc.EQ.1)THEN
               WRITE(6, *)' RESIDUAL NUCLEUS WITH A=', ares, ' AND Z=', 
     &                    zres, ' HAS NOT BEEN INITIALIZED'
               WRITE(6, *)' EXECUTION STOPPED'
            ENDIF
            NREs(nejc) = nnur
         ENDDO
C--------
C-------- DEGAS exciton model calculations of preequilibrium contribution
C--------
         IF(nnuc.EQ.1 .AND. EIN.GT.5.D0 .AND. DEGa.GT.0)THEN
            CALL EMPIREDEGAS
            WRITE(6, *)' '
            WRITE(6, *)' gamma spectrum from DEGAS:'
            CALL AUERST(1, 0)
            WRITE(6, *)' '
            WRITE(6, *)' neutron spectrum from DEGAS:'
            CALL AUERST(1, 1)
            WRITE(6, *)' '
            WRITE(6, *)' proton spectrum from DEGAS:'
            CALL AUERST(1, 2)
            WRITE(6, *)' '
            WRITE(6, *)' Start of summary from DEGAS'
            WRITE(6, *)' ---------------------------'
            IF(GDIv.GT.1.0)WRITE(6, *)' g = A/gdiv, gdiv =', GDIv
            WRITE(6, *)' '
            WRITE(6, *)' DEGAS gamma emission (CN) =',CSEmis(0, 1), 'mb'
            WRITE(6, *)' DEGAS neut. emission (CN) =',CSEmis(1, 1), 'mb'
            WRITE(6, *)' DEGAS prot. emission (CN) =',CSEmis(2, 1), 'mb'
            WRITE(6, *)' '
         ENDIF          ! Degas done
C--------
C--------HMS Monte Carlo preequilibrium emission
C--------
         IF(nnuc.EQ.1 .AND. LHMs.NE.0)THEN
            CLOSE(8)
            xizat = IZA(0)
            xnhms = NHMs
            CALL DDHMS(IZAejc(0), xizat, XJLv(1, 0), EINl, CSFus, CHMs, 
     &                 1.0D0, xnhms, 0, 0, 0)
C           CSEmis(0,1) = CSEmis(0,1) + ?
C           CSEmis(1,1) = CSEmis(1,1) + ?
C           CSEmis(2,1) = CSEmis(2,1) + ?
            CLOSE(8)
         ENDIF
C--------
C--------Heidelberg Multistep Compound calculations
C--------
         IF(nnuc.EQ.1 .AND. MSC.NE.0)THEN
            CALL HMSC(nvwful)
            CSEmis(0, 1) = CSEmis(0, 1) + CSMsc(0)
            CSEmis(1, 1) = CSEmis(1, 1) + CSMsc(1)
            CSEmis(2, 1) = CSEmis(2, 1) + CSMsc(2)
            IF(nvwful)GOTO 1600
         ENDIF
C--------
C--------start nnuc nucleus decay
C--------
         popleft = 0.0
C--------assure that full gamma cascade in the first CN is
C--------accounted for in the case of ENDF calculations
         IF(ENDf.GT.0.0D0)GCAsc = 1.0
C--------turn on (KEMIN=1) or off (KEMIN=NEX(NNUC)) gamma cascade
C--------in the first CN
         kemin = 1
         IF(nnuc.EQ.1)THEN
            IF(GCAsc.EQ.0.0D0)kemin = NEX(nnuc)
            IF(GCAsc.EQ. - 1.0D0 .AND. EXCn.GT.20.0D0)kemin = NEX(nnuc)
         ENDIF
         kemax = NEX(nnuc)
C--------account for widths fluctuations (HRTW)
         IF(LHRtw.EQ.1 .AND. EIN.GT.5.0D+0)THEN
            LHRtw = 0
            WRITE(6, *)' '
            WRITE(6, *)'Widths fluctuation has been turned off'
            WRITE(6, *)'due to the high incident energy. Set HRTW to 2'
            WRITE(6, *)'in the optional input to keep it active.'
            WRITE(6, *)' '
         ENDIF
         IF(nnuc.EQ.1 .AND. LHRtw.GT.0)THEN
            CALL HRTW
            IF(ENDf.EQ.2)CALL RECOIL(kemax, nnuc) !recoil spectrum
            kemax = NEX(nnuc) - 1
            GCAsc = 1.0
         ENDIF
C--------do loop over c.n. excitation energy
         DO ke = kemax, kemin, -1
            step = DE
            IF(ke.EQ.NEX(nnuc) .OR. ke.EQ.1)step = 0.5*DE
            IF(ke.EQ.NEX(nnuc) .AND. nnuc.EQ.1)step = 1.0
            IF(ENDf.EQ.2)THEN
C--------------clean auxiliary particle spectra for calculation of recoils
               IF(DEGa.NE.1 .OR. ke.NE.kemax .OR. nnuc.NE.1)THEN
                  DO nejc = 0, NEJcm
                     DO il = 1, NDLV
                        REClev(il, nejc) = 0.0
                     ENDDO
                     DO ie = 1, NDECSE
                        AUSpec(ie, nejc) = 0.0
                     ENDDO
                  ENDDO
               ENDIF
C--------------calculate population in the energy bin ke
               pope = 0.0
               DO jcn = 1, NLW, LTUrbo
                  pope = pope + POP(ke, jcn, 1, nnuc)
     &                   + POP(ke, jcn, 2, nnuc)
               ENDDO
            ENDIF
C-----------do loop over decaying nucleus parity
            DO ipar = 1, 2
               ip = INT(( - 1.0)**(ipar + 1))
C--------------do loop over decaying nucleus spin
               DO jcn = 1, NLW, LTUrbo
                  IF(GDRdyn.EQ.1.0D0)
     &               CALL ULMDYN(nnuc, jcn, EX(ke, nnuc))
99011             FORMAT(1X, 'J,DEF ', I3, F9.4)
                  DENhf = 0.0
                  IF(POP(ke, jcn, ipar, nnuc).LT.POPmax(nnuc))THEN
                     popleft = popleft + POP(ke, jcn, ipar, nnuc)*DE
                     GOTO 1560
                  ENDIF
C-----------------do loop over ejectiles
                  DO nejc = 1, NEJcm
                     nnur = NREs(nejc)
                     CALL DECAY(nnuc, ke, jcn, ip, nnur, nejc, sum)
                  ENDDO
C-----------------do loop over ejectiles       ***done***
C-----------------gamma emision
                  IF(LTUrbo.EQ.1)THEN
                     CALL DECAYG(nnuc, ke, jcn, ip, sum)
                  ELSE
                     CALL DECAYT(nnuc, ke, jcn, ip, sum)
                  ENDIF
C-----------------fission
                  IF(FISsil(nnuc))CALL FISSION(nnuc, ke, jcn, sumfis)
C-----------------fission                       ***done***
C-----------------distribute yrast population over discrete levels
C
                  IF(DENhf.EQ.0.0D0)THEN
                     IF(ke.EQ.1)THEN
                        ded = DE*0.5
                     ELSE
                        ded = DE
                     ENDIF
                     IF(IOUt.GT.1)WRITE(6, 
     & '('' Yrast state at bin'',I4,'' spin='',F5.1,'' pop='',   G12.5)'
     & )ke, FLOAT(ip)*(FLOAT(jcn) + HIS(nnur)), POP(ke, jcn, ipar, nnuc)
     &  *ded
C--------------------look for the discrete level with the closest spin
                     xnl = 1.0
                     spdiff = 100.
                     DO il = 1, NLV(nnuc)
                        spdif = ABS(FLOAT(jcn) + HIS(nnur)
     &                          - XJLv(il, nnuc))
                        IF(spdif.LT.spdiff)THEN
                           spdiff = spdif
                           xnl = 1.
                        ELSE
                           IF(spdif.EQ.spdiff)xnl = xnl + 1.
                        ENDIF
                     ENDDO
                     DO il = 1, NLV(nnuc)
                        spdif = ABS(FLOAT(jcn) + HIS(nnur)
     &                          - XJLv(il, nnuc))
                        IF(spdif.EQ.spdiff)THEN
                           POPlv(il, nnuc) = POPlv(il, nnuc)
     &                        + POP(ke, jcn, ipar, nnuc)*ded/xnl
                           REClev(il, 0) = REClev(il, 0)
     &                        + POP(ke, jcn, ipar, nnuc)*ded/xnl
                           IF(IOUt.GT.1)WRITE(6, 
     &           '(I3,''% of this was assumed to populate level #'',I3)'
     &           )INT(100./xnl), il
                        ENDIF
                     ENDDO
                     GOTO 1560
                  ENDIF
C-----------------
C-----------------normalization and accumulation
C-----------------
                  xnor = POP(ke, jcn, ipar, nnuc)*step/DENhf
                  stauc = stauc + RO(ke, jcn, nnuc)*xnor
                  IF(RO(ke, jcn, nnuc).NE.0.0D0)sgamc = sgamc + 
     &               DENhf*POP(ke, jcn, ipar, nnuc)
     &               *step/RO(ke, jcn, nnuc)
C-----------------particles
                  DO nejc = 1, NEJcm
                     nnur = NREs(nejc)
                     CALL ACCUM(ke, nnuc, nnur, nejc, xnor)
                     CSEmis(nejc, nnuc) = CSEmis(nejc, nnuc)
     &                  + xnor*SCRtem(nejc)
                  ENDDO
C-----------------gammas
                  CALL ACCUM(ke, nnuc, nnuc, 0, xnor)
                  CSEmis(0, nnuc) = CSEmis(0, nnuc) + xnor*SCRtem(0)
                  POP(ke, jcn, ipar, nnuc) = 0.0
C-----------------fission
                  csfis = csfis + sumfis*xnor
C-----------------calculate total emission
                  DO nejc = 0, NEJcm
                     csemist = csemist + CSEmis(nejc, nnuc)
                  ENDDO
                  csemist = csemist + csfis
 1560          ENDDO                   !loop over decaying nucleus spin
            ENDDO                   !loop over decaying nucleus parity
            IF(ENDf.EQ.2)CALL RECOIL(ke, nnuc)  !recoil spectrum for ke bin
         ENDDO                  !loop over c.n. excitation energy
C--------
C--------Fauser-Feshbach decay of nnuc  ***done***
C--------
C--------printout of results for the decay of NNUC nucleus
         IF(IOUt.GT.0)WRITE(6, 
     &          '(1X,/,'' Population left because too small '',G12.5,/)'
     &          )popleft*DE
 1600    IF(IOUt.GT.0)WRITE(6, 
     &                      '(1X,/,10X,''Discrete level population'')')
         IF(IOUt.GT.0 .AND. kemin.EQ.NEX(nnuc))WRITE(6, 
     &'(10X,''(no gamma cascade in the compound nucleus, primary transit
     &ions only)'',/)')
         IF(IOUt.GT.0 .AND. ENDf.NE.0.0D0 .AND. 
     &      (nnuc.EQ.mt91 .OR. nnuc.EQ.mt649 .OR. nnuc.EQ.mt849))
     &      WRITE(6, 
     &'(10X,''NOTE: due to ENDF option direct particle contribution was 
     &shifted to the g.s.'')')
         IF(IOUt.GT.0)WRITE(6, '(1X,/,10X,40(1H-),/)')
         DO il = 1, NLV(nnuc)
            CSPrd(nnuc) = CSPrd(nnuc) + POPlv(il, nnuc)
            IF(IOUt.GT.0)WRITE(6, 99013)il, ELV(il, nnuc), LVP(il, nnuc)
     &                                  , XJLv(il, nnuc), 
     &                                  POPlv(il, nnuc)
         ENDDO
C--------gamma decay of discrete levels (DECAYD)
         CALL DECAYD(nnuc)
         ia = INT(A(nnuc))
         iz = INT(Z(nnuc))
         IF(IOUt.GT.0)THEN
            WRITE(6, '(1X,/,10X,40(1H-),/)')
            WRITE(6, 
     &'(1X,I3,''-'',A2,''-'',I3,'' production cross section '',G12.5,'' 
     &mb'')')iz, SYMb(nnuc), ia, CSPrd(nnuc)
            IF(kemin.EQ.NEX(nnuc))WRITE(6, 
     &'(1X,''(no gamma cascade in the compound nucleus, primary transiti
     &ons only)'',/)')
            WRITE(6, *)' '
C-----------calculate life-times and widths
            IF(csemist.NE.0.0D0)THEN
               taut = stauc*6.589E-22*2.0*PI/csemist
               WRITE(6, '('' Average total   life-time'',G12.5,'' s'')')
     &               taut
               gamt = sgamc/2.0/PI/csemist
               WRITE(6, 
     &               '('' Average total   width    '',G12.5,'' MeV'')')
     &               gamt
C              TAUT=6.589E-22/GAMT
C              WRITE(6,'('' Average total life-time 1/width'',g12.5,
C              1       '' s'')') TAUT
            ENDIF
            IF(csfis.NE.0.0D0)THEN
               tauf = stauc*6.589E-22*2.0*PI/csfis
               WRITE(6, '('' Average fission life-time'',G12.5,'' s'')')
     &               tauf
               gamfis = gamt*csfis/csemist
               WRITE(6, '('' Average fission width    '',G12.5,'' MeV'')
     &               ')gamfis
            ENDIF
C-----------life-times and widths  *** done ***
            WRITE(6, *)' '
            WRITE(6, '('' Fission    cross section    '',G12.5,'' mb'')'
     &            )csfis
         ENDIF
C--------add compound elastic to shape elastic before everything falls
C--------down on the ground state
         IF(nnuc.EQ.1)THEN
            ELAcs = ELAcs + POPlv(1, mt91)
C-----------CN contribution to elastic ddx            
            elcncs = POPlv(1, mt91)/4.0/PI
         ENDIF
         WRITE(12, 
     &'(1X,I3,''-'',A2,''-'',I3,'' production cross section '',G12.6,'' 
     &mb'')')iz, SYMb(nnuc), ia, CSPrd(nnuc)
         WRITE(12, '(''    fission  cross section'',G12.5,'' mb'')')
     &         csfis
         IF(IOUt.GT.2)CALL AUERST(nnuc, 0)
         WRITE(6, '(''  g  emission cross section'',G12.5,'' mb'')')
     &         CSEmis(0, nnuc)
         WRITE(12, '('' g  emission cross section'',G12.5,'' mb'')')
     &         CSEmis(0, nnuc)
         DO nejc = 1, NEJcm
            nnur = NREs(nejc)
            IF(ENDf.NE.0D0 .OR. MSD.GT.0 .AND. nnuc.EQ.1)THEN
               DO ie = 1, NEX(nnuc)
C-----------------add H-F and MSC contribution to the ddx table
                  DO iang = 1, NDANG
                     CSEa(ie, iang, nejc, nnuc)
     &                  = CSEa(ie, iang, nejc, nnuc)
     &                  + CSE(ie, nejc, nnuc)/4.0/PI
                  ENDDO
                  IF(nnuc.EQ.1)THEN
C-----------------add MSD contribution to the energy spectra (angle int.)
                     CSE(ie, nejc, nnuc) = CSE(ie, nejc, nnuc)
     &                  + CSEmsd(ie, nejc)
                     DO iang = 1, NDANG
                        IF(CSE(ie, nejc, nnuc).NE.0.0D0)
     &                     CSEan(ie, iang, nejc)
     &                     = CSEa(ie, iang, nejc, nnuc)
     &                     /CSE(ie, nejc, nnuc)
                     ENDDO
                  ENDIF
               ENDDO
C--------------add MSD contribution to the total NEJC emission
               IF(nnuc.EQ.1)CSEmis(nejc, nnuc) = CSEmis(nejc, nnuc)
     &            + CSMsd(nejc)
            ENDIF
            IF(IOUt.GT.2)CALL AUERST(nnuc, nejc)
            IF(IOUt.GT.0)WRITE(6, 
     &               '(2X,A2,'' emission cross section'',G12.5,'' mb'')'
     &               )SYMbe(nejc), CSEmis(nejc, nnuc)
            WRITE(12, 
     &            '(1X,A2,'' emission cross section'',G12.5,'' mb'')')
     &            SYMbe(nejc), CSEmis(nejc, nnuc)
C-----------print residual nucleus population
            IF(IOUt.EQ.4)THEN
               nnur = NREs(nejc)
               ia = INT(A(nnur))
               WRITE(6, '('' Residual nucleus '',I3,''-'',A2,/)')ia, 
     &               SYMb(nnur)
               WRITE(6, '('' Positive parities population'',/)')
               WRITE(6, 99012)(EX(i, nnur), (POP(i,j,1,nnur), j = 1, 12)
     &                        , i = NEX(nnur), 1, ( - 1))
               WRITE(6, '('' Negative parities population'',/)')
               WRITE(6, 99012)(EX(i, nnur), (POP(i,j,2,nnur), j = 1, 12)
     &                        , i = NEX(nnur), 1, ( - 1))
               WRITE(6, '('' '')')
               poptot = 0.0
               DO j = 1, NLW
                  DO i = 1, NEX(nnur)
                     poptot = poptot + POP(i, j, 1, nnur)
     &                        + POP(i, j, 2, nnur)
                  ENDDO
                  poptot = poptot - 
     &                     0.5*(POP(1, j, 1, nnur) + POP(1, j, 2, nnur))
               ENDDO
               poptot = poptot*DE
               WRITE(6, *)'Total population of continuum    ', poptot, 
     &                    ' mb'
               poplev = 0.0
               DO i = 1, NLV(nnur)
                  poplev = poplev + POPlv(i, nnur)
               ENDDO
               WRITE(6, *)'Total population of disc. levels ', poplev, 
     &                    ' mb'
               WRITE(6, *)' '
            ENDIF
C-----------print double differential cross sections
            IF(CSMsd(nejc).GT.0.D0 .AND. IOUt.GE.3 .AND. nnuc.EQ.1)THEN
               itimes = FLOAT(NDANG)/11.0 + 0.95
               DO its = 1, itimes
                  iad = 1 + (its - 1)*11
                  iam = 11 + (its - 1)*11
                  iam = MIN0(NDANG, iam)
                  WRITE(6, *)' '
                  WRITE(6, 
     &                 '(30X,''A      n      g      l      e      s '')'
     &                 )
                  WRITE(6, *)' '
                  WRITE(6, '('' Energy  '',11(4X,F5.1,2X))')
     &                  (ANGles(ia), ia = iad, iam)
                  WRITE(6, *)' '
                  DO i = 1, NEX(1)
                     WRITE(6, '(1X,F7.3,1X,11E11.4)')FLOAT(i - 1)*DE, 
     &                     (CSEa(i, iang, nejc, nnuc), iang = iad, iam)
                  ENDDO
                  WRITE(6, *)' '
               ENDDO
            ENDIF
         ENDDO   !over ejectiles
C--------
C--------NNUC nucleus decay    **** done ******
C--------
      ENDDO     !over decaying nuclei
      IF(ENDf.EQ.1.0 .AND. AEJc(0).EQ.1.0D0 .AND. ZEJc(0).EQ.0.0D0)THEN
C-----
C-----ENDF spectra printout (exclusive representation MF=3&6, to be used
C-----only below the threshold for 3 particle emission, the alternative
C-----inclusive representation is recommended)
C-----
C-----   MT=16
C-----
         ares = A(1) - 2.0D0
         zres = Z(1)
         izares = INT(1000.0*zres + ares)
         CALL WHERE(izares, imt, iloc)
         IF(iloc.NE.1)THEN
            aorg = A(1) - 1.0D0
            zorg = Z(1)
            izaorg = INT(1000.0*zorg + aorg)
            CALL WHERE(izaorg, iorg, iloc)
            IF(iloc.NE.1)THEN
               IF(CSPrd(imt).GT.0.0D0)THEN
                  nspec = INT(EMAx(imt)/DE) + 2
                  WRITE(12, *)' '
                  WRITE(12, *)' Spectrum of neutrons (n,2n) '
                  WRITE(12, 
     &                 '(30X,''A      n      g      l      e      s '')'
     &                 )
                  WRITE(12, *)' '
                  WRITE(12, '('' Energy   '',8G15.5,/,(10X,8G15.5))')
     &                  ANGles
C-----------------RECORP recoil correction factor defined 1+Ap/Ar that
C-----------------multiplies cross sections and divides outgoing energies
C-----------------ATTENTION: TEMPORARY - this treatment is crude
                  recorp = 1. + 1./A(imt)
                  DO ie = 1, nspec
                     WRITE(12, '(F9.4,8E15.5,/,(9X,8E15.5))')
     &                     FLOAT(ie - 1)*DE/recorp, 
     &                     (CSEa(ie, nang, 1, iorg)*recorp, nang = 1, 
     &                     NDANG)
                  ENDDO
                  WRITE(12, *)' '
                  WRITE(12, '(A34,I6,A6,F12.5)')
     &                  '  Spectrum of recoils  (n,2n) ZAP=', IZA(imt), 
     &                  ' mass=', AMAss(imt)
                  WRITE(12, 
     &                 '(30X,''A      n      g      l      e      s '')'
     &                 )
                  WRITE(12, *)' '
                  WRITE(12, '('' Energy   '',8G15.5,/,(10X,8G15.5))')
     &                  ANGles
C-----------------RECORR recoil correction factor defined 1+Ar/Ap which multiplies
C-----------------cross sections and divides outgoing energies
                  recorr = 1. + A(imt)
                  DO ie = 1, nspec
                     WRITE(12, '(F9.4,8E15.5,/,(9X,8E15.5))')
     &                     FLOAT(ie - 1)*DE/recorr, 
     &                     (CSEa(ie, nang, 1, iorg)*recorr, 
     &                     nang = NDANG, 1, ( - 1))
                  ENDDO
C-----------------recoils *** done ***
                  WRITE(12, *)' '
                  WRITE(12, *)' Spectrum of gammas   (n,2n) '
                  WRITE(12, *)' '
                  WRITE(12, '('' Energy    mb/MeV'')')
                  WRITE(12, *)' '
                  DO ie = 1, nspec
                     WRITE(12, '(F9.4,E15.5)')FLOAT(ie - 1)*DE, 
     &                     CSE(ie, 0, imt)
                  ENDDO
               ENDIF
            ENDIF
         ENDIF
C-----
C-----   MT=17
C-----
         ares = A(1) - 3.0D0
         zres = Z(1)
         izares = INT(1000.0*zres + ares)
         CALL WHERE(izares, imt, iloc)
         IF(iloc.NE.1)THEN
            aorg = A(1) - 2.0D0
            zorg = Z(1)
            izaorg = INT(1000.0*zorg + aorg)
            CALL WHERE(izaorg, iorg, iloc)
            IF(iloc.NE.1)THEN
               IF(CSPrd(imt).GT.0.0D0)THEN
                  nspec = INT(EMAx(imt)/DE) + 2
                  WRITE(12, *)' '
                  WRITE(12, *)' Spectrum of neutrons (n,3n)'
                  WRITE(12, 
     &                 '(30X,''A      n      g      l      e      s '')'
     &                 )
                  WRITE(12, *)' '
                  WRITE(12, '('' Energy   '',8G15.5,/,(10X,8G15.5))')
     &                  ANGles
C-----------------RECORP recoil correction factor defined 1+Ap/Ar which
C-----------------multiplies cross sections and divides outgoing energies
C-----------------ATTENTION: TEMPORARY - this treatment is very crude
                  recorp = 1. + 1./A(imt)
                  DO ie = 1, nspec
                     WRITE(12, '(F9.4,8E15.5,/,(9X,8E15.5))')
     &                     FLOAT(ie - 1)*DE/recorp, 
     &                     (CSEa(ie, nang, 1, iorg)*recorp, nang = 1, 
     &                     NDANG)
                  ENDDO
                  WRITE(12, *)' '
                  WRITE(12, '(A34,I6,A6,F12.5)')
     &                  '  Spectrum of recoils  (n,3n) ZAP=', IZA(imt), 
     &                  ' mass=', AMAss(imt)
                  WRITE(12, 
     &                 '(30X,''A      n      g      l      e      s '')'
     &                 )
                  WRITE(12, *)' '
                  WRITE(12, '('' Energy   '',8G15.5,/,(10X,8G15.5))')
     &                  ANGles
C-----------------RECORR recoil correction factor defined 1+Ar/Ap which multiplies
C-----------------cross sections and divides outgoing energies
                  recorr = 1. + A(imt)
                  DO ie = 1, nspec
                     WRITE(12, '(F9.4,8E15.5,/,(9X,8E15.5))')
     &                     FLOAT(ie - 1)*DE/recorr, 
     &                     (CSEa(ie, nang, 1, iorg)*recorr, 
     &                     nang = NDANG, 1, ( - 1))
                  ENDDO
C-----------------recoils *** done ***
                  WRITE(12, *)' '
                  WRITE(12, *)' Spectrum of gammas   (n,3n)'
                  WRITE(12, *)' '
                  WRITE(12, '('' Energy    mb/MeV'')')
                  WRITE(12, *)' '
                  ngspec = INT(EMAx(imt)/DE) + 1
                  DO ie = 1, ngspec
                     WRITE(12, '(F9.4,E15.5)')FLOAT(ie - 1)*DE, 
     &                     CSE(ie, 0, imt)
                  ENDDO
               ENDIF
            ENDIF
         ENDIF
C-----
C-----   MT=22
C-----
         ares = A(1) - 2.0D0
         zres = Z(1) - 1.0D0
         izares = INT(1000.0*zres + ares)
         CALL WHERE(izares, imt, iloc)
         IF(iloc.NE.1)THEN
            IF(CSPrd(imt).GT.0.0D0)THEN
               nspec = INT(EMAx(imt)/DE) + 2
               WRITE(12, *)' '
               WRITE(12, *)' Spectrum of neutrons (n,np) '
               WRITE(12, 
     &               '(30X,''A      n      g      l      e      s '')')
               WRITE(12, *)' '
               WRITE(12, '('' Energy   '',8G15.5,/,(10X,8G15.5))')ANGles
               DO ie = 1, nspec
                  WRITE(12, '(F9.4,8E15.5,/,(9X,8E15.5))')FLOAT(ie - 1)
     &                  *DE, (CSEa(ie, nang, 1, IPRes), nang = 1, NDANG)
               ENDDO
               WRITE(12, *)' '
               WRITE(12, *)' Spectrum of protons  (n,np) '
               WRITE(12, 
     &               '(30X,''A      n      g      l      e      s '')')
               WRITE(12, *)' '
               WRITE(12, '('' Energy   '',8G15.5,/,(10X,8G15.5))')ANGles
C--------------RECORP recoil correction factor defined 1+Ap/Ar which multiplies
C--------------cross sections and divides outgoing energies
C--------------ATTENTION: approximate treatment
C--------------recoil due to proton only considered
               recorp = 1. + 1./A(imt)
               DO ie = 1, nspec
                  WRITE(12, '(F9.4,8E15.5,/,(9X,8E15.5))')FLOAT(ie - 1)
     &                  *DE/recorp, 
     &                  (CSEa(ie, nang, 2, INRes)*recorp, nang = 1, 
     &                  NDANG)
               ENDDO
               WRITE(12, *)' '
               WRITE(12, '(A34,I6,A6,F12.5)')
     &               '  Spectrum of recoils  (n,np) ZAP=', IZA(imt), 
     &               ' mass=', AMAss(imt)
               WRITE(12, 
     &               '(30X,''A      n      g      l      e      s '')')
               WRITE(12, *)' '
               WRITE(12, '('' Energy   '',8G15.5,/,(10X,8G15.5))')ANGles
C--------------RECORR recoil correction factor defined 1+Ar/Ap which multiplies
C--------------cross sections and divides outgoing energies
               recorr = 1. + A(imt)
               DO ie = 1, nspec
                  WRITE(12, '(F9.4,8E15.5,/,(9X,8E15.5))')FLOAT(ie - 1)
     &                  *DE/recorr, 
     &                  (CSEa(ie, nang, 2, INRes)*recorr, nang = NDANG, 
     &                  1, ( - 1))
               ENDDO
C--------------recoils *** done ***
               WRITE(12, *)' '
               WRITE(12, *)' Spectrum of gammas   (n,np) '
               WRITE(12, *)' '
               WRITE(12, '('' Energy    mb/MeV'')')
               WRITE(12, *)' '
               DO ie = 1, nspec
                  WRITE(12, '(F9.4,E15.5)')FLOAT(ie - 1)*DE, 
     &                  CSE(ie, 0, imt)
               ENDDO
            ENDIF
         ENDIF
C-----
C-----   MT=28
C-----
         ares = A(1) - 5.0D0
         zres = Z(1) - 2.0D0
         izares = INT(1000.0*zres + ares)
         CALL WHERE(izares, imt, iloc)
         IF(iloc.NE.1)THEN
            IF(CSPrd(imt).GT.0.0D0)THEN
               nspec = INT(EMAx(imt)/DE) + 2
               WRITE(12, *)' '
               WRITE(12, *)' Spectrum of neutrons (n,na) '
               WRITE(12, 
     &               '(30X,''A      n      g      l      e      s '')')
               WRITE(12, *)' '
               WRITE(12, '('' Energy   '',8G15.5,/,(10X,8G15.5))')ANGles
               DO ie = 1, nspec
                  WRITE(12, '(F9.4,8E15.5,/,(9X,8E15.5))')FLOAT(ie - 1)
     &                  *DE, 
     &                  (ANCsea(ie, nang, 1) + CSEa(ie, nang, 1, IARes), 
     &                  nang = 1, NDANG)
               ENDDO
               WRITE(12, *)' '
               WRITE(12, *)' Spectrum of alphas   (n,na) '
               WRITE(12, 
     &               '(30X,''A      n      g      l      e      s '')')
               WRITE(12, *)' '
               WRITE(12, '('' Energy   '',8G15.5,/,(10X,8G15.5))')ANGles
C--------------RECORP recoil correction factor defined 1+Ap/Ar which multiplies
C--------------cross sections and divides outgoing energies
C--------------ATTENTION: approximate treatment
C--------------recoil due to alpha only considered
               recorp = 1. + 4./A(IARes)
               DO ie = 1, nspec
                  WRITE(12, '(F9.4,8E15.5,/,(9X,8E15.5))')FLOAT(ie - 1)
     &                  *DE/recorp, 
     &                  ((ANCsea(ie,nang,2) + CSEa(ie,nang,3,INRes))
     &                  *recorp, nang = 1, NDANG)
               ENDDO
               WRITE(12, *)' '
               WRITE(12, '(A34,I6,A6,F12.5)')
     &               '  Spectrum of recoils  (n,na) ZAP=', IZA(imt), 
     &               ' mass=', AMAss(imt)
               WRITE(12, 
     &               '(30X,''A      n      g      l      e      s '')')
               WRITE(12, *)' '
               WRITE(12, '('' Energy   '',8G15.5,/,(10X,8G15.5))')ANGles
C--------------RECORR recoil correction factor defined 1+Ar/Ap which multiplies
C--------------cross sections and divides outgoing energies
               recorr = 1. + A(IARes)/4.
               DO ie = 1, nspec
                  WRITE(12, '(F9.4,8E15.5,/,(9X,8E15.5))')FLOAT(ie - 1)
     &                  *DE/recorr, 
     &                  ((ANCsea(ie,nang,2) + CSEa(ie,nang,3,INRes))
     &                  *recorp, nang = NDANG, 1, ( - 1))
               ENDDO
C--------------recoils *** done ***
               WRITE(12, *)' '
               WRITE(12, *)' Spectrum of gammas   (n,na) '
               WRITE(12, *)' '
               WRITE(12, '('' Energy    mb/MeV'')')
               WRITE(12, *)' '
               DO ie = 1, nspec
                  WRITE(12, '(F9.4,E15.5)')FLOAT(ie - 1)*DE, 
     &                  CSE(ie, 0, imt)
               ENDDO
            ENDIF
         ENDIF
C-----
C-----   MT=91
C-----
         ares = A(1) - 1.0D0
         zres = Z(1)
         izares = INT(1000.0*zres + ares)
         CALL WHERE(izares, imt, iloc)
         IF(iloc.NE.1)THEN
            nspec = INT(EMAx(imt)/DE) + 2
            WRITE(12, *)' '
            WRITE(12, *)' Spectrum of neutrons (n,n)  '
            WRITE(12, '(30X,''A      n      g      l      e      s '')')
            WRITE(12, *)' '
            WRITE(12, '('' Energy   '',8G15.5,/,(10X,8G15.5))')ANGles
C-----------RECORP recoil correction factor defined 1+Ap/Ar which multiplies
C-----------cross sections and divides outgoing energies
            recorp = 1. + 1./A(imt)
C-----------neutrons to continuum            
            DO ie = 1, NEXr(1,1)
               WRITE(12, '(F9.4,8E15.5,/,(9X,8E15.5))')FLOAT(ie - 1)
     &               *DE/recorp, 
     &               (CSEa(ie, nang, 1, 1)*recorp, nang = 1, NDANG)
            ENDDO
C-----------neutrons to discrete levels (direct transitions)
C-----------NOTE: printed at exact energies corresponding to disc. levels,
C-----------integral not conserved(!) but does not matter as only ang. distr.
C-----------are used for the ENDF formatting (x-sec being taken from MT=51-90)
            DO il = NLV(imt), 2, -1
               WRITE(12, '(F9.4,8E15.5,/,(9X,8E15.5))')
     &               (EMAx(imt)-ELV(il,imt))/recorp, 
     &               (CSAlev(nang, il, 1)*recorp/DE, nang = 1, NDANG)
            ENDDO
            WRITE(12, *)' '
            WRITE(12, '(A34,I6,A6,F12.5)')
     &            '  Spectrum of recoils  (n,n)  ZAP=', IZA(imt), 
     &            ' mass=', AMAss(imt)
            WRITE(12, '(30X,''A      n      g      l      e      s '')')
            WRITE(12, *)' '
            WRITE(12, '('' Energy   '',8G15.5,/,(10X,8G15.5))')ANGles
C-----------RECORR recoil correction factor defined 1+Ar/Ap which multiplies
C-----------cross sections and divides outgoing energies
            recorr = 1. + A(imt)
            DO ie = 1, nspec
               WRITE(12, '(F9.4,8E15.5,/,(9X,8E15.5))')FLOAT(ie - 1)
     &               *DE/recorr, 
     &               (CSEa(ie, nang, 1, 1)*recorr, nang = NDANG, 1, 
     &               ( - 1))
            ENDDO
            WRITE(12, *)' '
            WRITE(12, *)' Spectrum of gammas   (n,n)  '
            WRITE(12, *)' '
            WRITE(12, '('' Energy    mb/MeV'')')
            WRITE(12, *)' '
            DO ie = 1, nspec
               WRITE(12, '(F9.4,E15.5)')FLOAT(ie - 1)*DE, 
     &                                  CSE(ie, 0, imt)
            ENDDO
         ENDIF
C-----
C-----   MT=102
C-----
         ares = A(1)
         zres = Z(1)
         izares = INT(1000.0*zres + ares)
         CALL WHERE(izares, imt, iloc)
         IF(iloc.NE.1)THEN
            WRITE(12, *)' '
            WRITE(12, *)' Spectrum of gammas   (n,gamma)  '
            WRITE(12, *)' '
            WRITE(12, '('' Energy       mb/MeV'')')
            WRITE(12, *)' '
            nspec = INT(EMAx(imt)/DE) + 1
            DO ie = 1, nspec
               WRITE(12, '(F9.4,E15.5)')FLOAT(ie - 1)*DE, 
     &                                  CSE(ie, 0, imt)
            ENDDO
         ENDIF
C-----
C-----   MT=649
C-----
         ares = A(1) - 1.0D0
         zres = Z(1) - 1.0D0
         izares = INT(1000.0*zres + ares)
         CALL WHERE(izares, imt, iloc)
         IF(iloc.NE.1)THEN
            IF(CSPrd(imt).GT.0.0D0)THEN
               nspec = INT(EMAx(imt)/DE) + 2
               WRITE(12, *)' '
               WRITE(12, *)' Spectrum of protons  (n,p) '
               WRITE(12, 
     &               '(30X,''A      n      g      l      e      s '')')
               WRITE(12, *)' '
               WRITE(12, '('' Energy   '',8G15.5,/,(10X,8G15.5))')ANGles
               recorp = 1. + 1./A(imt)
C--------------protons to continuum            
               DO ie = 1,  NEXr(2,1)
                  WRITE(12, '(F9.4,8E15.5,/,(9X,8E15.5))')FLOAT(ie - 1)
     &                  *DE/recorp, 
     &                  (CSEa(ie, nang, 2, 1)*recorp, nang = 1, NDANG)
               ENDDO
C--------------protons to discrete levels (direct transitions)
C--------------NOTE: printed at exact energies corresponding to disc. levels,
C--------------integral not conserved(!) but does not matter as only ang. distr.
C--------------are used for the ENDF formatting (x-sec being taken from MT=601-648)
               DO il = NLV(imt), 2, -1
                  WRITE(12, '(F9.4,8E15.5,/,(9X,8E15.5))')
     &                  (EMAx(imt)-ELV(il,imt))/recorp, 
     &                  (CSAlev(nang, il, 2)*recorp/DE, nang = 1, NDANG)
               ENDDO
               WRITE(12, *)' '
               WRITE(12, '(A34,I6,A6,F12.5)')
     &               '  Spectrum of recoils  (n,p)  ZAP=', IZA(imt), 
     &               ' mass=', AMAss(imt)
               WRITE(12, 
     &               '(30X,''A      n      g      l      e      s '')')
               WRITE(12, *)' '
               WRITE(12, '('' Energy   '',8G15.5,/,(10X,8G15.5))')ANGles
               recorr = 1. + A(imt)
               DO ie = 1, nspec
                  WRITE(12, '(F9.4,8E15.5,/,(9X,8E15.5))')FLOAT(ie - 1)
     &                  *DE/recorr, 
     &                  (CSEa(ie, nang, 2, 1)*recorr, nang = NDANG, 1, 
     &                  ( - 1))
               ENDDO
               WRITE(12, *)' '
               WRITE(12, *)' Spectrum of gammas   (n,p) '
               WRITE(12, *)' '
               WRITE(12, '('' Energy    mb/MeV'')')
               WRITE(12, *)' '
               DO ie = 1, nspec
                  WRITE(12, '(F9.4,E15.5)')FLOAT(ie - 1)*DE, 
     &                  CSE(ie, 0, imt)
               ENDDO
            ENDIF
         ENDIF
C-----
C-----   MT=105, 105 or 106 light ion emission
C-----
         IF(NDEJC.EQ.4 .AND. NEMc.GT.0)THEN
            ares = A(1) - AEJc(NDEJC)
            zres = Z(1) - ZEJc(NDEJC)
            izares = INT(1000.0*zres + ares)
            CALL WHERE(izares, imt, iloc)
            IF(iloc.NE.1)THEN
               IF(CSPrd(imt).GT.0.0D0)THEN
                  nspec = INT(EMAx(imt)/DE) + 2
                  WRITE(12, *)' '
                  WRITE(12, 
     &'(''  Spectrum of  '',I1,''-'',A2,4X,                          ''(
     &n,'',I1,''-'',A2,'')'')')INT(AEJc(NDEJC)), SYMbe(NDEJC), 
     &                         INT(AEJc(NDEJC)), SYMbe(NDEJC)
                  WRITE(12, 
     &                 '(30X,''A      n      g      l      e      s '')'
     &                 )
                  WRITE(12, *)' '
                  WRITE(12, '('' Energy   '',8G15.5,/,(10X,8G15.5))')
     &                  ANGles
                  recorp = 1. + AEJc(NDEJC)/A(imt)
                  DO ie = 1, nspec
                     WRITE(12, '(F9.4,8E15.5,/,(9X,8E15.5))')
     &                     FLOAT(ie - 1)*DE/recorp, 
     &                     (CSEa(ie, nang, NDEJC, 1)*recorp, nang = 1, 
     &                     NDANG)
                  ENDDO
                  WRITE(12, *)' '
                  IF(SYMbe(NDEJC).EQ.'d ' .OR. SYMbe(NDEJC).EQ.'t ')THEN
                     WRITE(12, '(A26,A2,A6,I6,A6,F12.5)')
     &                     '  Spectrum of recoils  (n,', SYMbe(NDEJC), 
     &                     ') ZAP=', IZA(imt), ' mass=', AMAss(imt)
                  ELSE
                     WRITE(12, '(A26,A2,I1,A5,I6,A6,F12.5)')
     &                     '  Spectrum of recoils  (n,', SYMbe(NDEJC), 
     &                     INT(AEJc(NDEJC)), ')ZAP=', IZA(imt), 
     &                     ' mass=', AMAss(imt)
                  ENDIF
                  WRITE(12, 
     &                 '(30X,''A      n      g      l      e      s '')'
     &                 )
                  WRITE(12, *)' '
                  WRITE(12, '('' Energy   '',8G15.5,/,(10X,8G15.5))')
     &                  ANGles
                  recorr = 1. + A(imt)/AEJc(NDEJC)
                  DO ie = 1, nspec
                     WRITE(12, '(F9.4,8E15.5,/,(9X,8E15.5))')
     &                     FLOAT(ie - 1)*DE/recorr, 
     &                     (CSEa(ie, nang, NDEJC, 1)*recorr, 
     &                     nang = NDANG, 1, ( - 1))
                  ENDDO
                  WRITE(12, *)' '
                  WRITE(12, '(A26,I1,''-'',A2,A6)')
     &                  '  Spectrum of gammas   (n,', INT(AEJc(NDEJC)), 
     &                  SYMbe(NDEJC), ')     '
                  WRITE(12, *)' '
                  WRITE(12, '('' Energy    mb/MeV'')')
                  WRITE(12, *)' '
                  DO ie = 1, nspec
                     WRITE(12, '(F9.4,E15.5)')FLOAT(ie - 1)*DE, 
     &                     CSE(ie, 0, imt)
                  ENDDO
               ENDIF
            ENDIF
         ENDIF
C-----
C-----   MT=849+801...848
C-----
         ares = A(1) - 4.0D0
         zres = Z(1) - 2.0D0
         izares = INT(1000.0*zres + ares)
         CALL WHERE(izares, imt, iloc)
         IF(iloc.NE.1)THEN
            IF(CSPrd(imt).GT.0.0D0)THEN
               nspec = INT(EMAx(imt)/DE) + 2
               WRITE(12, *)' '
               WRITE(12, *)' Spectrum of alphas   (n,a)'
               WRITE(12, 
     &               '(30X,''A      n      g      l      e      s '')')
               WRITE(12, *)' '
               WRITE(12, '('' Energy   '',8G15.5,/,(10X,8G15.5))')ANGles
               recorp = 1. + 4./A(imt)
C--------------alphas to continuum            
               DO ie = 1, NEXr(3,1)
                  WRITE(12, '(F9.4,8E15.5,/,(9X,8E15.5))')FLOAT(ie - 1)
     &                  *DE/recorp, 
     &                  (CSEa(ie, nang, 3, 1)*recorp, nang = 1, NDANG)
               ENDDO
C--------------alphas to discrete levels (direct transitions)
C--------------NOTE: printed at exact energies corresponding to disc. levels,
C--------------integral not conserved(!) but does not matter as only ang. distr.
C--------------are used for the ENDF formatting (x-sec being taken from MT=801-848)
               DO il = NLV(imt), 2, -1
                  WRITE(12, '(F9.4,8E15.5,/,(9X,8E15.5))')
     &                  (EMAx(imt)-ELV(il,imt))/recorp, 
     &                  (CSAlev(nang, il, 3)*recorp/DE, nang = 1, NDANG)
               ENDDO
               WRITE(12, *)' '
               WRITE(12, '(A34,I6,A6,F12.5)')
     &               '  Spectrum of recoils  (n,a)  ZAP=', IZA(imt), 
     &               ' mass=', AMAss(imt)
               WRITE(12, 
     &               '(30X,''A      n      g      l      e      s '')')
               WRITE(12, *)' '
               WRITE(12, '('' Energy   '',8G15.5,/,(10X,8G15.5))')ANGles
               recorr = 1. + A(imt)/4.
               DO ie = 1, nspec
                  WRITE(12, '(F9.4,8E15.5,/,(9X,8E15.5))')FLOAT(ie - 1)
     &                  *DE/recorr, 
     &                  (CSEa(ie, nang, 3, 1)*recorr, nang = NDANG, 1, 
     &                  ( - 1))
               ENDDO
               WRITE(12, *)' '
               WRITE(12, *)' Spectrum of gammas   (n,a) '
               WRITE(12, *)' '
               WRITE(12, '('' Energy    mb/MeV'')')
               WRITE(12, *)' '
               DO ie = 1, nspec
                  WRITE(12, '(F9.4,E15.5)')FLOAT(ie - 1)*DE, 
     &                  CSE(ie, 0, imt)
               ENDDO
            ENDIF
         ENDIF
C-----
C-----   MT=???
C-----
         ares = A(1) - 5.0D0
         zres = Z(1) - 3.0D0
         izares = INT(1000.0*zres + ares)
         CALL WHERE(izares, imt, iloc)
         IF(iloc.NE.1)THEN
            IF(CSPrd(imt).GT.0.0D0)THEN
               nspec = INT(EMAx(imt)/DE) + 2
               WRITE(12, *)' '
               WRITE(12, *)' Spectrum of protons  (n,pa) '
               WRITE(12, 
     &               '(30X,''A      n      g      l      e      s '')')
               WRITE(12, *)' '
               WRITE(12, '('' Energy   '',8G15.5,/,(10X,8G15.5))')ANGles
               DO ie = 1, nspec
                  WRITE(12, '(F9.4,8E15.5,/,(9X,8E15.5))')FLOAT(ie - 1)
     &                  *DE, (APCsea(ie, nang, 1), nang = 1, NDANG)
               ENDDO
               WRITE(12, *)' '
               WRITE(12, *)' Spectrum of alphas   (n,pa) '
               WRITE(12, 
     &               '(30X,''A      n      g      l      e      s '')')
               WRITE(12, *)' '
               WRITE(12, '('' Energy   '',8G15.5,/,(10X,8G15.5))')ANGles
C--------------RECORP recoil correction factor defined 1+Ap/Ar which multiplies
C--------------cross sections and divides outgoing energies
C--------------ATTENTION: approximate treatment
C--------------recoil due to alpha only considered
               recorp = 1. + 4./A(IARes)
               DO ie = 1, nspec
                  WRITE(12, '(F9.4,8E15.5,/,(9X,8E15.5))')FLOAT(ie - 1)
     &                  *DE/recorp, 
     &                  (APCsea(ie, nang, 2)*recorp, nang = 1, NDANG)
               ENDDO
               WRITE(12, *)' '
               WRITE(12, '(A34,I6,A6,F12.5)')
     &               '  Spectrum of recoils  (n,pa) ZAP=', IZA(imt), 
     &               ' mass=', AMAss(imt)
               WRITE(12, 
     &               '(30X,''A      n      g      l      e      s '')')
               WRITE(12, *)' '
               WRITE(12, '('' Energy   '',8G15.5,/,(10X,8G15.5))')ANGles
C--------------RECORR recoil correction factor defined 1+Ar/Ap which multiplies
C--------------cross sections and divides outgoing energies
               recorr = 1. + A(IARes)/4.
               DO ie = 1, nspec
                  WRITE(12, '(F9.4,8E15.5,/,(9X,8E15.5))')FLOAT(ie - 1)
     &                  *DE/recorr, 
     &                  (APCsea(ie, nang, 2)*recorp, nang = NDANG, 1, 
     &                  ( - 1))
               ENDDO
C--------------recoils *** done ***
               WRITE(12, *)' '
               WRITE(12, *)' Spectrum of gammas   (n,pa) '
               WRITE(12, *)' '
               WRITE(12, '('' Energy    mb/MeV'')')
               WRITE(12, *)' '
               DO ie = 1, nspec
                  WRITE(12, '(F9.4,E15.5)')FLOAT(ie - 1)*DE, 
     &                  CSE(ie, 0, imt)
               ENDDO
            ENDIF
         ENDIF
      ENDIF
C-----
C-----end ENDF spectra printout (exclusive)
C-----
C-----
C-----sum exclusive energy spectra and double-differential cross
C-----sections into inclusive ones transforming them into CM
C-----(reduce channel energy to account for recoils)
C-----and store them on the 0 nucleus (target)
C-----
      IF(IOUt.GT.1 .OR. ENDf.EQ.2.0D0)THEN
         DO nnuc = 1, NNUcd
            DO nejc = 0, NEJcm
               IF(nejc.GT.0)THEN
                  recorr = (A(nnuc) - AEJc(nejc))/A(nnuc)
               ELSE
                  recorr = 1.0
               ENDIF
               DO icse = 1, NDEX
                  xccm = (icse - 1)*recorr + 1.0000001
                  iccml = xccm
                  iccmh = MIN(NDEX, iccml + 1)
                  weight = xccm - iccml
                  CSE(iccml, nejc, 0) = CSE(iccml, nejc, 0)
     &                                  + CSE(icse, nejc, nnuc)
     &                                  *(1.0 - weight)
C-----------------double contribution to the first energy bin to
C-----------------to conserve the integral
                  IF(iccml.EQ.1 .AND. icse.NE.1)CSE(iccml, nejc, 0)
     &               = CSE(iccml, nejc, 0) + CSE(icse, nejc, nnuc)
     &               *(1.0 - weight)
                  CSE(iccmh, nejc, 0) = CSE(iccmh, nejc, 0)
     &                                  + CSE(icse, nejc, nnuc)*weight
                  DO nang = 1, NDANG
                     CSEa(iccml, nang, nejc, 0)
     &                  = CSEa(iccml, nang, nejc, 0)
     &                  + CSEa(icse, nang, nejc, nnuc)*(1.0 - weight)
C--------------------double contribution to the first energy bin to
C--------------------to conserve the integral
                     IF(iccml.EQ.1 .AND. icse.NE.1)
     &                  CSEa(iccml, nang, nejc, 0)
     &                  = CSEa(iccml, nang, nejc, 0)
     &                  + CSEa(icse, nang, nejc, nnuc)*(1.0 - weight)
                     CSEa(iccmh, nang, nejc, 0)
     &                  = CSEa(iccmh, nang, nejc, 0)
     &                  + CSEa(icse, nang, nejc, nnuc)*weight
                  ENDDO
               ENDDO
            ENDDO
         ENDDO
      ENDIF
      IF(IOUt.GT.1)THEN
         WRITE(6, '(1X,//,''Inclusive spectra (C.M.)'',//)')
         DO nejc = 0, NEJcm
            CALL AUERST(0, nejc)
         ENDDO
      ENDIF
C-----
C-----ENDF spectra printout (inclusive representation MT = 10 or 5
C-----this is a recommended option)
C-----
      IF(ENDf.EQ.2.0D0)THEN
C--------
C--------spectra of residues
C--------
         DO nnuc = 1, NNUcd    !loop over decaying nuclei
            IF(CSPrd(nnuc).GT.0.0D0)THEN
C-------------normalize recoil spectra to remove eventual inaccuracy
C-------------due to numerical integration of angular distributions
C-------------and find last non-zero cross section for printing
               corr = 0.0
               ilast = 0
               DO ie = 1, NDEREC
                  corr = corr + RECcse(ie, 0, nnuc)
                  IF(RECcse(ie, 0, nnuc).NE.0)ilast = ie
               ENDDO
C              WRITE(6,*)'nnuc, rec, cs',nnuc,corr*DERec,CSPrd(nnuc)
               corr = CSPrd(nnuc)/corr/DERec
               ilast = MIN(ilast + 1, NDEREC)
               DO ie = 1, ilast
                  RECcse(ie, 0, nnuc) = RECcse(ie, 0, nnuc)*corr
               ENDDO
               WRITE(12, *)' '
               WRITE(12, '(A34,I6,A6,F12.5)')
     &               '  Spectrum of recoils  (n,x)  ZAP=', IZA(nnuc), 
     &               ' mass=', AMAss(nnuc)
               WRITE(12, *)' '
               WRITE(12, '('' Energy    mb/MeV'')')
               WRITE(12, *)' '
               DO ie = 1, ilast
                  WRITE(12, '(F9.4,E15.5)')FLOAT(ie - 1)*DERec, 
     &                  RECcse(ie, 0, nnuc)
               ENDDO
               IF(ABS(1.0 - corr).GT.0.01D0)THEN
                  WRITE(6, *)' '
                  WRITE(6, *)'WARNING !!! x-section balance in recoils '
                  WRITE(6, *)'difference = ', ABS(1.0 - corr)*100.0, '%'
                  WRITE(6, *)' '
               ENDIF
            ENDIF
         ENDDO !over decaying nuclei in ENDF spectra printout
C--------recoils *** done ***
C--------print inclusive gamma spectrum
         WRITE(12, *)' '
         WRITE(12, *)' Spectrum of gammas   (n,x) '
         WRITE(12, *)' '
         WRITE(12, '('' Energy    mb/MeV'')')
         WRITE(12, *)' '
         DO ie = 1, NDECSE
            WRITE(12, '(F9.4,E15.5)')FLOAT(ie - 1)*DE, CSE(ie, 0, 1)
         ENDDO
C--------print inclusive spectra of ejectiles
C--------neutrons
         nspec = INT((EMAx(1) - Q(1,1))/DE) + 2
         IF(nspec.GT.NDECSE)nspec = NDECSE
         WRITE(12, *)' '
         WRITE(12, *)' Spectrum of neutrons (n,x) '
         WRITE(12, '(30X,''A      n      g      l      e      s '')')
         WRITE(12, *)' '
         WRITE(12, '('' Energy   '',8G15.5,/,(10X,8G15.5))')ANGles
         DO ie = 1, nspec
            WRITE(12, '(F9.4,8E15.5,/,(9X,8E15.5))')FLOAT(ie - 1)*DE, 
     &            (CSEa(ie, nang, 1, 1), nang = 1, NDANG)
         ENDDO
C--------protons
         nspec = INT((EMAx(1) - Q(2,1))/DE) + 2
         IF(nspec.GT.NDECSE)nspec = NDECSE
         WRITE(12, *)' '
         WRITE(12, *)' Spectrum of protons  (n,x) '
         WRITE(12, '(30X,''A      n      g      l      e      s '')')
         WRITE(12, *)' '
         WRITE(12, '('' Energy   '',8G15.5,/,(10X,8G15.5))')ANGles
         DO ie = 1, nspec
            WRITE(12, '(F9.4,8E15.5,/,(9X,8E15.5))')FLOAT(ie - 1)*DE, 
     &            (CSEa(ie, nang, 2, 1), nang = 1, NDANG)
         ENDDO
C--------alphas
         nspec = INT((EMAx(1) - Q(3,1))/DE) + 2
         IF(nspec.GT.NDECSE)nspec = NDECSE
         WRITE(12, *)' '
         WRITE(12, *)' Spectrum of alphas   (n,x) '
         WRITE(12, '(30X,''A      n      g      l      e      s '')')
         WRITE(12, *)' '
         WRITE(12, '('' Energy   '',8G15.5,/,(10X,8G15.5))')ANGles
         DO ie = 1, nspec
            WRITE(12, '(F9.4,8E15.5,/,(9X,8E15.5))')FLOAT(ie - 1)*DE, 
     &            (CSEa(ie, nang, 3, 1), nang = 1, NDANG)
         ENDDO
C--------light ions
         IF(NDEJC.EQ.4 .AND. NEMc.GT.0)THEN
            nspec = INT((EMAx(1) - Q(NDEJC,1))/DE) + 2
            IF(nspec.GT.NDECSE)nspec = NDECSE
            WRITE(12, *)' '
            WRITE(12, 
     &'(''  Spectrum of  '',I1,''-'',A2,4X,                          ''(
     &n,x)'')')INT(AEJc(NDEJC)), SYMbe(NDEJC)
            WRITE(12, '(30X,''A      n      g      l      e      s '')')
            WRITE(12, *)' '
            WRITE(12, '('' Energy   '',8G15.5,/,(10X,8G15.5))')ANGles
            DO ie = 1, nspec
               WRITE(12, '(F9.4,8E15.5,/,(9X,8E15.5))')FLOAT(ie - 1)*DE, 
     &               (CSEa(ie, nang, NDEJC, 1), nang = 1, NDANG)
            ENDDO
         ENDIF
      ENDIF
C-----end of ENDF spectra (inclusive)
      READ(5, *)EIN
      IF(EIN.LT.0.0D0)THEN
         WRITE(12, *)' '
         STOP 'REGULAR '
      ENDIF
      GOTO 1400
99012 FORMAT(1X, F5.2, 12G10.3)
99013 FORMAT(10X, I2, F10.4, I5, F8.1, G15.6, I3, 20I3, (/, 53X, 20I3))
      END
C
C
C
      SUBROUTINE RECOIL(Ke, Nnuc)
C
C----
C----Construct recoil spectra (ENDF=2):
C----Each excitation bin is given its recoil spectrum, when a particle
C----is emitted its recoil velocity is vector added to the parent recoil
C----velocity and a resulting spectrum is summed upon daughter recoil
C----spectrum corresponding to the populated energy bin in the
C----daughter (kinematical normalization taken into account).
C----Daughter recoil spectra will be distributed between
C----adjacent bins (inversly proportional to the
C----distance of the actual energy to the bin energy
C----in order to conserve energy).
C----Requires that continuum spectra from each bin are stored on the
C----AUSpec array and those to discrete levels on the REClev for each
C----reaction mechanism considered in the calculations.
C----
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
     &                 exqcut, recorr, sumnor, weight
      REAL FLOAT
      INTEGER icse, ie, il, ire, irec, na, nejc, nnur
C
C-----normalize recoil spectrum of the parent
      sumnor = 0.0
      DO ire = 1, NDEREC
         sumnor = sumnor + RECcse(ire, Ke, Nnuc)
      ENDDO
      IF(sumnor.NE.0.0D0)THEN
         DO ire = 1, NDEREC
            RECcse(ire, Ke, Nnuc) = RECcse(ire, Ke, Nnuc)/sumnor
         ENDDO
      ENDIF
      dang = 3.14159/FLOAT(NDANG - 1)
      coef = dang/DERec/2.0
      DO nejc = 1, NEJcm   !over ejectiles
C--------decay to continuum
         nnur = NREs(nejc)
C--------recorr is a recoil correction factor that
C--------divides outgoing energies
         recorr = A(Nnuc)/AEJc(nejc)
         exqcut = EX(Ke, Nnuc) - Q(nejc, Nnuc) - ECUt(nnur)
         DO ie = 1, NDECSE !over ejec. energy (daughter excitation)
            icse = (exqcut - (ie - 1)*DE)/DE + 1.001
C           !daughter bin
            IF(icse.LE.0)GOTO 50
            erecejc = (ie - 1)*DE/recorr
            DO ire = 1, NDEREC          !over recoil spectrum
               erecpar = (ire - 1)*DERec
               DO na = 1, NDANG
                  erecoil = erecejc + erecpar
                  erecoil = erecoil + 2.0*SQRT(erecejc*erecpar)
     &                      *CANgler(na)
                  irec = erecoil/DERec + 1.001
                  weight = (erecoil - (irec - 1)*DERec)/DERec
                  IF(irec + 1.GT.NDEREC)GOTO 20
                  RECcse(irec, icse, nnur) = RECcse(irec, icse, nnur)
     &               + RECcse(ire, Ke, Nnuc)*AUSpec(ie, nejc)
     &               *(1.0 - weight)*SANgler(na)*coef
                  RECcse(irec + 1, icse, nnur)
     &               = RECcse(irec + 1, icse, nnur)
     &               + RECcse(ire, Ke, Nnuc)*AUSpec(ie, nejc)
     &               *weight*SANgler(na)*coef
               ENDDO                  !over angles
 20         ENDDO                  !over recoil spectrum
         ENDDO                  !over  daugther excitation
C--------decay to discrete levels (stored with icse=0)
 50      exqcut = exqcut + ECUt(nnur)
         DO il = 1, NLV(nnur)
            erecod = exqcut - ELV(il, nnur)  !emission energy
            erecod = erecod/recorr
            IF(erecod.LT.0)GOTO 100
            DO ire = 1, NDEREC      !over recoil spectrum
               DO na = 1, NDANG !over angles
                  erecoil = (ire - 1)*DERec + erecod
                  erecoil = erecoil + 2.0*SQRT((ire - 1)*DERec*erecod)
     &                      *CANgler(na)
                  irec = erecoil/DERec + 1.001
                  weight = (erecoil - (irec - 1)*DERec)/DERec
                  IF(irec.GT.NDEREC)GOTO 60
                  RECcse(irec, 0, nnur) = RECcse(irec, 0, nnur)
     &               + RECcse(ire, Ke, Nnuc)*REClev(il, nejc)
     &               *(1.0 - weight)*SANgler(na)*coef
                  IF(irec + 1.GT.NDEREC)GOTO 60
                  RECcse(irec + 1, 0, nnur) = RECcse(irec + 1, 0, nnur)
     &               + RECcse(ire, Ke, Nnuc)*REClev(il, nejc)
     &               *weight*SANgler(na)*coef
               ENDDO                  !over angles
 60         ENDDO                  !over recoil spectrum
         ENDDO                  !over levels
 100  ENDDO                  !over ejectiles
C-----
C-----parent recoil spectrum after gamma decay
C-----
      nnur = Nnuc
      nejc = 0
C-----gamma decay to continuum
      DO ie = 1, NDECSE !over ejec. energy (daughter excitation)
         icse = (EX(Ke, Nnuc) - (ie - 1)*DE - ECUt(nnur))/DE + 1.001
C        !daughter bin
         DO irec = 1, NDEREC         !over recoil spectrum
            IF(icse.GT.0)RECcse(irec, icse, nnur)
     &         = RECcse(irec, icse, nnur) + RECcse(irec, Ke, Nnuc)
     &         *AUSpec(ie, 0)/DERec
         ENDDO                  !over recoil spectrum
      ENDDO                  !over  daugther excitation
C-----gamma decay to discrete levels (stored with icse=0)
      DO il = 1, NLV(nnur)
         DO ire = 1, NDEREC             !over recoil spectrum
            RECcse(ire, 0, nnur) = RECcse(ire, 0, nnur)
     &                             + RECcse(ire, Ke, Nnuc)
     &                             *REClev(il, nejc)/DERec
         ENDDO                  !over recoil spectrum
      ENDDO                  !over levels
      END
