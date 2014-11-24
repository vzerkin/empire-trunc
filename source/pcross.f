Ccc   * $Rev: 4242 $
Ccc   * $Author: rcapote $
Ccc   * $Date: 2014-11-24 23:31:10 +0100 (Mo, 24 Nov 2014) $

C
      SUBROUTINE PCROSS(Sigr,Totemis)

      IMPLICIT none
      INCLUDE 'dimension.h' 
      INCLUDE 'global.h'
C
C     FORMAL PARAMETERS:
C     Sigr = REACTION CROSS SECTION
C                ---- OUTPUT -------
C     CROss(NDEjc+1) = CHANNEL EMISSION CROSS SECTION
C     FR = DEPLETION FACTOR FOR PREEQUILIBRIUM
C     SPEc(0:NDEjc,NDEx) = Channel emission spectra, 0 means gamma channel
C
C     Some variables:
C     AC,ZC = MASS AND ATOMIC NUMBER OF THE COMPOUND NUCLEUS
C     EC CN EXCITATION ENERGY, LEVEL DENSITY & PAIRING FOR COMP.NUC.
C     GC,PC LEVEL DENSITY & PAIRING FOR COMP.NUC.
C     G(NEJc),PAIR(NEJc) THE SAME FOR RESIDUAL NUCLEI AFTER EMISSION
C     MFPp = MEAN FREE PATH PARAMETER
C
C PARAMETER definitions
C
      REAL*8 C1, EPS
      PARAMETER (C1 = 3.6824121D17,EPS = 1.D-4)
      INTEGER*4 PMAX
      PARAMETER (PMAX = 50)
C
C COMMON variables
C
      REAL*8 L(0:NDEJC,PMAX), VV
      INTEGER*4 NHEq
      COMMON /CALC5 / L, NHEq
      COMMON /VWELL / VV

      DOUBLE PRECISION cross(0:NDEJC), spec(0:NDEJC,NDEX)

      DOUBLE PRECISION crossNT(0:NDEJC),crossNTt,
     &                 crossPE(0:NDEJC),crossPEt
      COMMON /PEXS/ crossNT,crossNTt,crossPE,crossPEt

      DOUBLE PRECISION specBU(0:NDEJC,NDECSE),crossBU(0:NDEJC),crossBUt
      COMMON /CBREAKUP/specBU,crossBU,crossBUt

      LOGICAL lbreakup, ltransfer
      COMMON /LPEXS/lbreakup, ltransfer 

      DOUBLE PRECISION specNT(0:NDEJC,NDECSE),te_e
      DOUBLE PRECISION culbar

C
C Dummy arguments
C
      REAL*8 Sigr, Totemis
C
C Local variables
C
      DOUBLE PRECISION aat, azt, cme, ec, eee, eint(NDEX), em(PMAX),
     &       ebind, emaxi, emini, emis, er, excnq, ff, ff1, ff2, ff3,
     &       fint(NDEX), flm(4,4), fanisot, fr, ftmp, gc, hlp1, pc,
     &       r(4,PMAX,NDEJC), sg, vvf, vsurf, wb, wda, step, ggg

      DOUBLE PRECISION g(0:NDEJC), pair(0:NDEJC), scompn, 
     &                 we(0:NDEJC,PMAX,NDEX) !, ddxs(NDAngecis)

      DOUBLE PRECISION, ALLOCATABLE :: ddxs(:)

      DOUBLE PRECISION crossaftertrans, crossafterbreak,
     &     crossBUn,crossNTn

      INTEGER*4 ac, ao, ap, ar, h1, hh, icon, icon3, ien, ienerg,
     &          ihmax, j, p, zc, zp, zr, zo, iang, nnn

      LOGICAL callpcross
      DOUBLE PRECISION DENSW
      INTEGER ie, nejc, nexrt, nnur, myalloc
      INTEGER*2 iemax(0:NDEJC), iemin(0:NDEJC)
      DOUBLE PRECISION SGAM
      CHARACTER*12 status
C     To correct bug found by M Pigni and C Mattoon, a variable "callpcross" value is saved between calls   
      SAVE r, callpcross
C
C
      DATA callpcross/.FALSE./
C
C     INITIALIZATION

C     Kalbach systematic for PCROSS DDX calculations
C     fanisot is assumed 1, i.e. pure PE emission
C
      fanisot = 1.d0
C     fanisot set to 0.d0 means isotropic distribution

      crossBUt= 0.d0
      crossNTt= 0.d0
      crossBUn= 0.d0
      crossNTn= 0.d0
      specNT  = 0.d0
      specBU  = 0.d0
      
      if(allocated(ddxs)) deallocate(ddxs)

      ALLOCATE(ddxs(NDAngecis),STAT=myalloc)
      IF(myalloc.NE.0) THEN
        WRITE(8,*)  'ERROR: Insufficient memory for ddxs: PCROSS!'
        WRITE(12,*) 'ERROR: Insufficient memory for ddxs: PCROSS!'
        STOP 'ERROR: Insufficient memory for ddxs: PCROSS!'
      ENDIF     

C     Projectile mass and charge number
      ap = AEJc(0)
      zp = ZEJc(0)     

      cme = MFPp/1.4D21
C-----Excitation energy (MeV)
      ec = EXCn
C-----Compound nucleus
      ac = A(1)
      zc = Z(1)
C
C     Parametrization of the well depth for p-h LD calculations
C     According to TALYS manual,v.0.64 (2004) A.Koning et al,
C
      vvf   = 38.d0
      vsurf = vvf
      IF(ap.eq.1 .and. zp.eq.1) vsurf = 22.d0 +
     &   16.d0*EINl**4/(EINL**4+(450.d0/FLOAT(ac)**0.333333d0)**4)
      IF(ap.eq.1 .and. zp.eq.0) vsurf = 12.d0 +
     &   26.d0*EINl**4/(EINL**4+(245.d0/FLOAT(ac)**0.333333d0)**4)

      XNAver = 0.d0

      fr = 0.D0
      totemis = 0.D0
      cross   = 0.D0
      DO hh = 1, PMAX
         em(hh) = 0.D0
      ENDDO
      em(1) = 1.D0 - QDFrac
      em(2) = QDFrac
C
      ggg = GDIV
      if(GDIV.eq.0) ggg=13.d0
      gc = FLOAT(ac)/ggg*GTIlnor(1)
      pc = 0.d0
      IF(pc.eq.0.d0) then
        ftmp = 0.
        IF (ac.GT.0.D0) ftmp = 12./SQRT(DBLE(FLOAT(ac)))
        pc = ftmp                                             ! odd
        IF (MOD(ac,2).EQ.0 .AND. MOD(zc,2).EQ.0) pc = 2*ftmp  ! e-e
        IF (MOD(ac,2).EQ.0 .AND. MOD(zc,2).EQ.1) pc = 0.d0    ! o-o
      ENDIF
C     We suppress pairing if requested in input
C     Pairing is implicitly considered in the distribution of discrete levels
      if(NPAirpe.eq.0) pc = 0.d0
C-----Compound gamma emitting nucleus
      g(0) = gc
      pair(0) = pc
C
C-----MAXIMUM EXCITON NUMBER NHEq
C
C     CHMax determines the maximum allowable HOLE number
C     for exciton model emission spectra calculations
C
C     It is much better aproximation than using fixed
C     exciton number as the emission loops increase
C     with energy as it is expected on physical basis.
C
C     Nmax = Aproj + NHEq -1 (is equal to NHEq for nucleon induced reactions)
C
C     Chinese group uses CHMax = 0.88, but it gives too hard PE spectrum in Th232
C     so it was reduced to 0.2 for the IAEA Th-232 evaluation.
C     IF(CHMax . EQ. 0.d0) CHMax = 0.2d0 ! default value
Cig   However it was increased by Ignatyuk to the standard value =ln2gt=.540Sqrt(gEc).
      IF(CHMax . EQ. 0.d0) CHMax = 0.540d0 ! default value
C
      NHEq = MIN(PMAX - 1,NINT(CHMax*SQRT(gc*ec))) + 1
C
      IF (.NOT.callpcross) CALL RQFACT(NHEq,r)
C     callpcross = .TRUE.  ! To avoid r factor recalculation at each call

C-----ZERO ARRAY INITIALIZATION
      DO nejc = 0, NDEJC
         iemin(nejc) = 2
         iemax(nejc) = NDEX
         DO ienerg = 1, NDEX
            spec(nejc,ienerg) = 0.D0
            DO hh = 1, PMAX
               we(nejc,hh,ienerg) = 0.D0
            ENDDO
         ENDDO
         DO hh = 1, PMAX
             L(nejc,hh) = 0.D0
         ENDDO
      ENDDO
      pair = 0
C
C-----NEJcm is the maximum number of particles emitted
C
      DO nejc = 1, NEJcm
         ar = ac - AEJc(nejc)
         zr = zc - ZEJc(nejc)
         nnur = NREs(nejc)
         if (nnur.lt.0) cycle
         g(nejc) = FLOAT(ar)/ggg*GTIlnor(nnur)
         ftmp = 0.
         IF (ar.GT.0.D0) ftmp = 12./SQRT(DBLE(FLOAT(ar)))
         pair(nejc) = ftmp
         IF (MOD(ar,2).EQ.0 .AND. MOD(zr,2).EQ.0) pair(nejc) = 2*ftmp
         IF (MOD(ar,2).EQ.0 .AND. MOD(zr,2).EQ.1) pair(nejc) = 0.d0
C        We suppress pairing if requested in input
C        Pairing is implicitly considered in the distribution of discrete levels
         if(NPAirpe.eq.0) pair(nejc) = 0.d0
C--------Maximum and minimum energy bin
         excnq = EXCn - Q(nejc,1)
C
C        Assuming PE calculates over discrete levels' region as well
         nexrt = MAX(INT(excnq/DE + 1.0001),1)

C        IDNa(1,6) = 0   ! discrete N is included even with ECIS active
C        IDNa(3,6) = 0   ! discrete P is included even with ECIS active
C        IDNa(11,6) = 0  ! discrete A is included even with ECIS active
C        IDNa(12,6) = 0  ! discrete D is included even with ECIS active
C        IDNa(13,6) = 0  ! discrete T is included even with ECIS active
C        IDNa(14,6) = 0  ! discrete H is included even with ECIS active
         if(nejc.eq.1 .and. IDNa(1,6).eq.0) 
     &     nexrt = MAX(INT((excnq-ECUt(nnur))/DE + 1.0001),1)
         if(nejc.eq.2 .and. IDNa(3,6).eq.0) 
     &     nexrt = MAX(INT((excnq-ECUt(nnur))/DE + 1.0001),1)
         if(nejc.eq.3 .and. IDNa(11,6).eq.0) 
     &     nexrt = MAX(INT((excnq-ECUt(nnur))/DE + 1.0001),1)
         if(nejc.eq.4 .and. IDNa(12,6).eq.0) 
     &     nexrt = MAX(INT((excnq-ECUt(nnur))/DE + 1.0001),1)
         if(nejc.eq.5 .and. IDNa(13,6).eq.0) 
     &     nexrt = MAX(INT((excnq-ECUt(nnur))/DE + 1.0001),1)
         if(nejc.eq.6 .and. IDNa(14,6).eq.0) 
     &     nexrt = MAX(INT((excnq-ECUt(nnur))/DE + 1.0001),1)

         iemax(nejc) = nexrt
         DO ienerg = 2, nexrt
            eee = DE*(ienerg - 1)
            IF (EMAx(nnur).GT.eee) iemax(nejc) = ienerg
            IF (ETL(5,nejc,nnur).EQ.0) THEN
               sg = SIGabs(ienerg + 5,nejc,nnur)
            ELSE
               sg = SIGabs(ienerg,nejc,nnur)
            ENDIF
            IF ( (sg.LT.1.D-15) .and. nejc.gt.1 .and.
     &         ienerg.LE.iemax(nejc) ) iemin(nejc) = ienerg
         ENDDO
      ENDDO

      WRITE (8,99020)
C
C-----Direct reaction spectra and cross sections 
C
      scompn  = Sigr   

      crossaftertrans = 0.d0
      crossafterbreak = 0.d0

      IF(AEJc(0).LT.2 .or. AEJc(0).GT.4 .or. 
     & ((.not.ltransfer) .and. (.not.lbreakup))) GOTO 9  
C
      culbar = 0.8*ZEJc(0)*Z(0)*ELE2
     &         /(1.3d0*(AEJc(0)**0.3333334 + A(0)**0.3333334))
      te_e = 1.d0/(1.d0+exp((culbar - EINl)/COMega))

C     *************************************************
C     ** break-up
      BUNorm = 0.d0
      DO nejc = 1, NEJcm
         IF(BUReac(nejc).GT.0.01d0) 
     >     BUNorm = max(BUReac(nejc),BUNorm)
      ENDDO
      IF(.not.lbreakup) THEN
        write(8,'(/1X,'' Break-up reactions not considered'',/)')        
        GOTO 8
      ENDIF
      write(8,49002)
49002 FORMAT (/3X,
     &' *** Break-up Parameterization (C. Kalbach) ***',/)
      WRITE(8,'(''  Reaction cross section before break-up'',F12.5,
     &          '' mb'')') scompn

C
C     This routine calculates the total break-up cross section
C      which is substracted from the reaction cross section.
C     No secondary PE emission is allowed after breakup reactions.
C
C     Total breakup includes both elastic and inelastic breakup.
C
C      1)   Elastic breakup implies that the target nucleus is an spectator
C           Breakup particles go directly to outgoing channels without
C           populating residuals. Cross sections and spectra shoudl be added
C         as production cross section of the residual equal to the target nucleus
C
C      2)   Inelastic breakup implies a difference treatment for detected and
C         interacting breakup particles (resulting from breakup).
C           - One particle goes directly to outgoing channels without
C             populating residuals, only the production cross sections are updated
C           (the same as elastic breakup)
C         - Interacting particle is absorbed into the CN* (which coincides with one of residual nuclei)
C           Residual nuclei are populated from the emitted particle spectra. 
C         TO BE DONE LATER.
C
      CALL BREAKUP(iemin,iemax,crossBUt,crossBU,specBU,te_e)

C     write(*,*) (ienerg,specBU(1,ienerg),ienerg=1,10)
C     write(*,*)
c           
      crossafterbreak = Sigr - crossBUt
      IF(crossafterbreak.gt.(1.D0-BUNorm) * Sigr )THEN
         DO nejc = 1, NEJcm
            IF(crossBU(nejc).LE.0.d0)  CYCLE
            cross(nejc) = cross(nejc) + crossBU(nejc)
            write(8,'(4x,''Break-up cross section for ('',
     &          a2,'','',a2,'')  '',f12.5,'' mb'')') 
     &          SYMbe(NPROject),symbe(nejc),crossBU(nejc)
            DO ienerg = iemin(nejc),iemax(nejc)
               spec(nejc,ienerg)   = spec(nejc,ienerg) +
     &                               specBU(nejc,ienerg)
            ENDDO
         ENDDO
      ELSE
         WRITE(8,'(10x,''Total break-up cross section'',2x,F12.5,
     &          '' mb'')')crossBUt
         WRITE(8,49010)
C        WRITE(*,49010)
49010    FORMAT (4X,
     & 'WARNING: Break-up exhausted reaction cross section!!!')
         WRITE(8,'(''    Break-up cross sections normalized to '',F5.3,
     &             ''*reaction cross section'')')BUNorm
         DO nejc = 1, NEJcm
            IF(crossBU(nejc).LE.0.d0) CYCLE 
            crossBU(nejc) = scompn * BUNorm * crossBU(nejc)/
     &                      crossBUt
            cross(nejc) = cross(nejc) + crossBU(nejc)
            write(8,'(4x,''Break-up cross section for ('',
     &        a2,'','',a2,'')  '',f12.5,'' mb'')')
     &        SYMbe(NPROject),symbe(nejc),crossBU(nejc)  
            DO ienerg = iemin(nejc),iemax(nejc)
              specBU(nejc, ienerg) = specBU(nejc, ienerg) *
     &                               scompn * BUNorm/crossBUt  
              spec(nejc,ienerg)   = spec(nejc,ienerg) +
     &                              specBU(nejc,ienerg)
            ENDDO
            crossBUn = crossBUn + crossBU(nejc)
         ENDDO
         crossBUt = crossBUn
         WRITE(8,*)
      ENDIF
      crossafterbreak =  scompn - crossBUt
      WRITE(8,'(10x,''Total break-up cross section  '',F12.5,
     &             '' mb'')')crossBUt
      WRITE(8,'(''  Reaction cross section after break-up '',F12.5,
     &          '' mb'')')crossafterbreak
      WRITE(8,*)
      scompn = crossafterbreak
      scompn = max(crossafterbreak,0.d0)
      IF(scompn.le.0.d0) goto 70
C
c     ****************************************************** 
C     ** transfer reactions: stripping and pick-up 
      NTNorm = 0.d0
 8    DO nejc = 1, NEJcm
         IF(NTReac(nejc).GT.0.01d0) 
     >     NTNorm = max(NTReac(nejc),NTNorm)
      ENDDO
      IF(.not.ltransfer) THEN
       write(8,'(/5X,'' Nucleon transfer reactions not considered'',/)')        
       GOTO 9
      ENDIF

      CALL DTRANS(iemin,iemax,crossNTt,crossNT,specNT,te_e)
      WRITE(8,89002)
89002 FORMAT (/4X,
     &'*** Stripping and Pick-up Parameterization (C. Kalbach) ***',
     & /)
      WRITE(8,'(''  Reaction cross section before transfer'',
     &          F12.5, '' mb'')')scompn

      crossaftertrans = scompn - crossNTt
      IF(crossaftertrans.gt.(1.d0 - NTNorm) * scompn)THEN
         DO nejc = 1, NEJcm
            if(crossNT(nejc).le.0.d0) CYCLE
            cross(nejc) = cross(nejc) + crossNT(nejc)
            IF(AEJc(0).LT.AEJc(nejc))
     &        WRITE(8,'(4x,''Pick-up   cross section for ('',
     &        a2,'','',a2,'') '',f12.5,'' mb'')')
     &        SYMbe(NPROject),symbe(nejc),crossNT(nejc)     
            IF(AEJc(0).GT.AEJc(nejc))
     &        WRITE(8,'(4x,''Stripping cross section for ('',
     &        a2,'','',a2,'') '',f12.5,'' mb'')')
     &        SYMbe(NPROject),symbe(nejc),crossNT(nejc)           
            DO ienerg = iemin(nejc), iemax(nejc) 
               spec(nejc, ienerg) = spec(nejc,ienerg) + 
     &                              specNT(nejc,ienerg)
            ENDDO
         ENDDO
      ELSE
         WRITE(8,'(10x,''Total transfer cross section'',2x,F12.5,
     &          '' mb'')')crossNTt
         WRITE (8,49011)
C        WRITE (*,49011)
49011    FORMAT (4X,
     &'WARNING: Transfer reactions exhausted reaction cross section!!')
         WRITE(8,'(''    Transfer cross sections normalized to '',F5.3,
     & ''*reaction cross section before NT'')') NTNorm
         DO nejc = 1, NEJcm
            if(crossNT(nejc).le.0.d0) CYCLE
            crossNT(nejc) = NTNorm * scompn * crossNT(nejc)/
     &                      crossNTt
            cross(nejc) = cross(nejc) + crossNT(nejc)
            crossNTn = crossNTn + crossNT(nejc)
c
            IF(AEJc(0).LT.AEJc(nejc))
     &           WRITE(8,'(4x,''Pick-up   cross section for ('',
     &           a2,'','',a2,'') '',f12.5,'' mb'')')
     &           SYMbe(NPROject),symbe(nejc),crossNT(nejc)     
            IF(AEJc(0).GT.AEJc(nejc))
     &           WRITE(8,'(4x,''Stripping cross section for ('',
     &           a2,'','',a2,'') '',f12.5,'' mb'')')
     &           SYMbe(NPROject),symbe(nejc),crossNT(nejc)    
            DO ienerg = iemin(nejc), iemax(nejc)       
               specNT(nejc, ienerg) =  NTNorm * scompn * 
     &                            specNT(nejc,ienerg)/crossNTt        
               spec(nejc, ienerg) = spec(nejc,ienerg) + 
     &                            specNT(nejc,ienerg)
            ENDDO     
         ENDDO
         crossNTt = crossNTn
      ENDIF
      crossaftertrans = scompn - crossNTt
      scompn = max(crossaftertrans,0.d0)
      WRITE(8,'(10x,''Total transfer cross section  '',
     &          F12.5, '' mb'')')crossNTt
      WRITE(8,'(''  Reaction cross section after transfer '',
     &          F12.5, '' mb'')')crossaftertrans
      WRITE(8,*)
C
C9    IF(scompn.eq.0.d0) goto 60
 9    IF(scompn.le.0.d0) goto 70

      IF(PEQc.eq.0) goto 70  ! skipping PE calculation
C
C     **Pre-equilibrium decay
C
      IF((.not.ltransfer) .and. (.not.lbreakup)) THEN
         WRITE (8,99005)
99005 FORMAT (/,5X,' Preequilibrium decay (PCROSS)')      
      ELSE         
         WRITE (8,89003)
89003 FORMAT (/4X,'***** Preequilibrium decay (PCROSS) *****', /)
      ENDIF

      WRITE (8,99010) MFPp, NHeq
99010 FORMAT (/,3X,'Mean free path parameter   = ',F4.2,/
     &          3X,'Equilibrium exciton number = ',I4/)  
      write(8,*)
      write(8,*) 'Input reaction for exciton model:',scompn
C
     
C-----Maximum and minimum energy bin for gamma emission
      nnur = NREs(0)
C     No PE contribution to discrete for gammas
      nexrt = MAX(INT((EXCn -ECUt(nnur))/DE + 1.0001),1)
C     Assuming PE calculates over discrete levels' region as well
C     nexrt = MAX(INT(EXCn/DE + 1.0001),1)
      iemax(0) = nexrt
C
C-----EMISSION RATES CALCULATIONS FOLLOWS           
C
C-----PRIMARY PARTICLE LOOP
C
      DO nejc = 0, NEJcm
         nnur = NREs(nejc)
         IF (nnur.lt.0) cycle
         IF (nejc.NE.0) THEN
            ao = AEJc(nejc)
            zo = ZEJc(nejc)
            ar = ac - ao
            zr = zc - zo

C              mn    neutron mass  1.008 665 amu 
C              mp    proton mass   1.007 276 amu 
C              ma    4He mass      4.001 506 amu 
C              md    deuteron mass 2.013 553 amu 
C              mt    triton mass   3.015 501 amu 
C              m3He  3He mass      3.014 932 amu 

C              me    electron mass 5.485 79910-4 amu 

C           ff1 = (2*Sin+1)*redmass
            IF (ao.EQ.1 .AND. zo.EQ.0) ff1 = 2.0173*ar/(ar + 1.0087) ! n
            IF (ao.EQ.1 .AND. zo.EQ.1) ff1 = 2.0145*ar/(ar + 1.0073) ! p
            IF (ao.EQ.4 .AND. zo.EQ.2) ff1 = 4.0015*ar/(ar + 4.0015) ! alpha
            IF (ao.EQ.2 .AND. zo.EQ.1) ff1 = 6.0408*ar/(ar + 2.0136) ! d
            IF (ao.EQ.3 .AND. zo.EQ.1) ff1 = 6.0312*ar/(ar + 3.0156) ! t
         ELSE
            ar = ac
            zr = zc
            ao = 0
            zo = 0
            ff1 = 1.D0/AMUmev
         ENDIF
C        EMPIRE tuning factor is used (important to describe capture) RCN, june 2005
         ff1 = ff1*TUNE(nejc,1)*TUNEPE(nejc)
C
C--------PARTICLE-HOLE LOOP
C
         DO h1 = 1, NHEq
            icon = 0
            hh = h1 - 1
C           Found bug that decreased PE contribution, thanks to A Voinov 
C         Oct 1, 2010 MH & RC, next line was commented to correct the bug
C           IF (hh.EQ.0 .AND. nejc.NE.0) GOTO 50
            p = hh + ap
C
C           Defining well depth for p-h LD calculations
C
            VV = vvf
C           hh .eq. 1 is recommended in the TALYS manual
C           I use hh .LE. 1 to enhance PE photon emission
C           if (hh.eq.1) VV = vsurf
            if (hh.eq.1 .and. nejc.le.2) VV = vsurf
C
C
            ff2 = DENSW(gc,pc,p,hh,ec)
            IF (ff2.EQ.0.) cycle
C
C-----------PRIMARY ENERGY CYCLE
C

            DO ienerg = iemin(nejc), iemax(nejc)
               eee = DE*(ienerg - 1)
               er = EMAx(nnur) - eee
               IF (er.LE.EPS) cycle
C--------------Inverse cross section
               IF (nejc.EQ.0) THEN
                  aat = ac
                  azt = zc
                  sg = SGAM(aat,azt,eee,er,TNUc(ienerg,1))
               ELSEIF (ETL(5,nejc,nnur).EQ.0) THEN
                  sg = SIGabs(ienerg + 5,nejc,nnur)
               ELSE
                  sg = SIGabs(ienerg,nejc,nnur)
               ENDIF
               IF (sg.le.0) cycle
               hlp1 = 0.D0
C--------------PREEQ n & p EMISSION
               IF (nejc.gt.0 .and. nejc.LE.2) THEN
                  wda = DENSW(g(nejc),pair(nejc),p - ao,hh,er)
                  IF (wda.GT.0.) hlp1 = wda*r(1,h1,nejc)
C--------------PREEQ CLUSTER EMISSION
               ELSEIF (nejc.GT.2 .AND. nejc.LE.NEJcm) THEN
                  CALL PREFORMATION(flm,eee)
                  DO j = 1, ao
                     wda = DENSW(g(nejc),pair(nejc),p - j,hh,er)
                     IF (wda.GT.0.) hlp1 = hlp1 + flm(ao,j)
     &                   *wda*r(j,h1,nejc)
                  ENDDO
C--------------PREEQ GAMMA EMISSION
               ELSEIF (nejc.EQ.0) THEN
                  wda = DENSW(gc,pc,p,hh,er)
                  hlp1 = wda*DBLE(p + hh)/(DBLE(p + hh) + eee*gc)
                  if(ap.le.1 .and. hh.GE.1) hlp1 = hlp1 +
     &                     gc*eee*DENSW(gc,pc,p - 1,hh - 1,er)/
     &                            (DBLE(p + hh - 2) + eee*gc)
               ENDIF
               IF (hlp1.EQ.0.) cycle
               ff3 = hlp1*eee*sg
               IF (nejc.EQ.0) ff3 = ff3*eee
               ff = ff1*ff3/ff2
               we(nejc,h1,ienerg) = ff*C1
               icon = icon + 1
               icon3 = ienerg
               eint(icon) = eee
               fint(icon) = ff
C              write(8,'(1x,3i3,1x,7(d12.6,1x))')
C    &             nejc,h1,ienerg,hlp1,sg,eee,wda,ff1,ff2,ff3
            ENDDO

C-----------END OF EMISSION ENERGY LOOP
C
C-----------INTEGRATION PROCEDURE #1 (EMISSION RATES)
C
            IF (icon.GT.0) THEN
               IF (nejc.NE.0) THEN
                  emaxi = EMAx(nnur)
                  IF (DE*icon3.LT.emaxi) emaxi = DE*icon3
                  emini = 0.D0
                  IF (iemin(nejc).GT.2) emini = DE*(iemin(nejc) - 2)
                  hlp1 = fint(1)*(eint(1) - emini) + fint(icon)
     &                   *(emaxi - eint(icon))
               ELSE
                  hlp1 = fint(1)*eint(1) + fint(icon)
     &                   *(EMAx(nnur) - eint(icon))
               ENDIF
               DO ien = 2, icon
                  hlp1 = hlp1 + (eint(ien) - eint(ien - 1))
     &                   *(fint(ien - 1) + fint(ien))
               ENDDO
               L(nejc,h1) = 0.5D0*hlp1*C1
            ENDIF
         ENDDO
C--------END OF PARTICLE-HOLE LOOP

      ENDDO

C-----END OF EMITTED PARTICLE LOOP
C
C-----TRANSITION RATES CALCULATIONS FOLLOWS
C
      CALL TRANSIT(ec,gc,ap,NHEq)
C
C-----MASTER EQUATION SOLUTION
C
      CALL RESOL(em,ihmax,ap,cme)

C--------setup model matrix (IDNa) defining which model is used where
C                        ECIS   MSD   MSC   ????    HMS   PCROSS
C                        1     2     3      4      5      6
C        1 neut. disc.   x     x     0      0      x      x
C        2 neut. cont.   0     x     x      x      x      x
C        3 prot. disc.   x     x     0      0      x      x
C        4 prot. cont.   0     x     x      x      x      x
C        5 gamma         0     0     x      x      0      x
C        6 alpha. cont.  0     0     0      0      0      x
C        7 deut . cont.  0     0     0      0      0      x
C        8 trit . cont.  0     0     0      0      0      x
C        9 He-3 . cont.  0     0     0      0      0      x
C       10 LI   . cont.  0     0     0      0      0      x
C       11 alpha. cont.  0     0     0      0      0      x
C       12 deut . cont.  0     0     0      0      0      x
C       13 trit . cont.  0     0     0      0      0      x
C       14 He-3 . cont.  0     0     0      0      0      x
C
C-----PARTICLE LOOP FOR EMISSION SPECTRA CALCULATIONS
C
60    DO nejc = 0, NEJcm
C        Skipping cross sections if MSD and MSC active
         IF(nejc.eq.0 .and. IDNa(5,6).EQ.0) CYCLE
         IF(nejc.eq.1 .and. IDNa(2,6).EQ.0) CYCLE
         IF(nejc.eq.2 .and. IDNa(4,6).EQ.0) CYCLE
         IF(nejc.eq.3 .and. IDNa(6,6).EQ.0) CYCLE
         IF(nejc.eq.4 .and. IDNa(7,6).EQ.0) CYCLE
         IF(nejc.eq.5 .and. IDNa(8,6).EQ.0) CYCLE
         IF(nejc.eq.6 .and. IDNa(9,6).EQ.0) CYCLE

         hlp1 = 0.D0

         DO ienerg = iemin(nejc), iemax(nejc)
            emis = 0.D0
            DO h1 = 1, ihmax
               wb = we(nejc,h1,ienerg)
               IF (wb.le.0 .or. em(h1).le.0) cycle
               hh = h1 - 1
               p  = h1 - 1 + ap
               emis = emis + wb*em(h1)
C
C              Exciton distribution weighted by emission rates
C              h = h1 - 1; p = h - 1 + ap; n = p + h =  2*h1 - 3  + ap
C
C              We want to calculate the <average by emission> exciton number N
C              for each ejectile and emission energy. Later we will use this average
C              exciton number N to obtain the preequilibrium spin-cut off as proposed
C              by Chadwick and Blann SIG ~ N*A^(2/3) being SIG the spin cut-off squared
               nnn = 2*h1 - 3  + ap
               if(nnn.gt.0) XNAver(nejc,ienerg) =
     >           XNAver(nejc,ienerg) + nnn*wb*em(h1)
            ENDDO          
            if(emis.gt.0.d0) XNAver(nejc,ienerg) =
     >                  NINT(XNAver(nejc,ienerg)/emis) 
C           if(nejc.eq.1) then 
C             write(*,*) 
C    >       'nejc,IE,naver=',nejc,ienerg, NINT(xnaver(nejc,ienerg))
C           endif
            step = 1.d0
C           if(ienerg.eq.iemin(nejc) .OR.
C    >         ienerg.eq.iemax(nejc) ) step=0.5d0
            hlp1 = hlp1 + scompn*emis*DE*step
            spec(nejc,ienerg) = spec(nejc,ienerg) + scompn*emis*step

         ENDDO

         crossPE(nejc) = hlp1
         crossPEt = crossPEt + crossPE(nejc) 

         cross(nejc) = hlp1 + cross(nejc)  
C        totemis = totemis + crossBU(nejc)
         IF(ltransfer .or. lbreakup)THEN
            if(nejc.eq.0) then
              write(8,'(4x,''PE emission cross section  ('',
     &        a2,'','',a2,'')  '',f12.5,'' mb'')')
     &        SYMbe(NPROject),'g ',crossPE(nejc) 
            else
              write(8,'(4x,''PE emission cross section  ('',
     &        a2,'','',a2,'')  '',f12.5,'' mb'')')
     &        SYMbe(NPROject),symbe(nejc),crossPE(nejc) 
            endif
         ENDIF
      ENDDO
 70   IF(ltransfer .or. lbreakup)THEN
         WRITE(8,'(10x,''Total PE cross section        '',
     &             F12.5, '' mb'')')crossPEt
         WRITE(8,89004)
      ENDIF
89004 FORMAT (/4X,
     &'*** BU + NT + PE cross sections calculated in PCROSS ***', /)

      IF (IOUt.GE.1) THEN
         DO nejc = 0, NEJcm
            IF (nejc.gt.0 .and. nejc.LE.3) THEN !nucleons and alpha
               IF (IDNa(2*nejc,6).EQ.0) THEN
                  status = "  (ignored) "
               ELSE
                  status = "  (accepted)"
               ENDIF
               WRITE (8,
     &'(1X,A2,'' PCROSS emission cross section'',G12.5,
     &'' mb'',A12)') SYMbe(nejc), cross(nejc), status
            ELSEIF (nejc.ge.4) THEN !complex particle
               IF (IDNa(3 + nejc,6).EQ.0) THEN
                  status = "  (ignored) "
               ELSE
                  status = "  (accepted)"
               ENDIF
               WRITE (8,
     &'(1X,A2,'' PCROSS emission cross section'',G12.5,
     &'' mb'',A12)') SYMbe(nejc), cross(nejc), status
            ELSE !gamma
               IF (IDNa(5,6).EQ.0) THEN
                  status = "  (ignored) "
               ELSE
                  status = "  (accepted)"
               ENDIF
               WRITE (8,
     &'(1X,A2,'' PCROSS emission cross section'',G12.5,
     &'' mb'',A12)') 'g ', cross(nejc), status
            ENDIF
C---------We don't need this spectrum printout any more but I leave it
C---------commented for checking in case anything goes wrong.
C         DO ienerg = iemin(nejc), iemax(nejc)
C                  WRITE(8,'(1x, F7.2, 2x, 7(D12.6, 1x))')
C    &             DE*(ienerg - 1), spec(nejc, ienerg)
C         ENDDO
C         WRITE(8, *)'==========================='
         ENDDO
      ENDIF
C
C-----Transfer PCROSS results into EMPIRE. Call to ACCUMSD is needed later
C     totemis = 0.D0
      DO nejc = 0, NEJcm  ! over ejectiles
         nnur = NREs(nejc)
         IF(nnur.LT.0 .or. cross(nejc).le.0.d0) cycle
         IF (nejc.gt.0) THEN !particle
            IF (nejc.EQ.1 .AND. IDNa(2,6).EQ.0) cycle
            IF (nejc.EQ.2 .AND. IDNa(4,6).EQ.0) cycle
            IF (nejc.EQ.3 .AND. IDNa(6,6).EQ.0) cycle
            IF (nejc.EQ.4 .AND. IDNa(7,6).EQ.0) cycle
            IF (nejc.EQ.5 .AND. IDNa(8,6).EQ.0) cycle
            IF (nejc.EQ.6 .AND. IDNa(9,6).EQ.0) cycle
            ao = AEJc(nejc)
            zo = ZEJc(nejc)
            excnq = EXCn - Q(nejc,1)
            ebind = Q(nejc,1)
         ELSE !gamma
            IF (IDNa(5,6).EQ.0) cycle
            ao = 0
            zo = 0
            nnur = 1
            excnq = EXCn
            ebind = 0.d0
         ENDIF

         CSMsd(nejc) = CSMsd(nejc) + cross(nejc)
         totemis = totemis + cross(nejc)
         DO ie = iemin(nejc), iemax(nejc)
            eee = DE*(ie - 1)
            ftmp = spec(nejc,ie)
            if(ftmp.le.0.d0) cycle
            CSEmsd(ie,nejc) = CSEmsd(ie,nejc) + ftmp

            ddxs = 0.d0

            Call Kalbach( ac, zp, ap-zp, zo, ao-zo, EINl, EXCn,
     &              ebind, eee, ftmp, fanisot, ddxs, CANgle, NDAng, PI)
            DO iang = 1, NDANG
              CSEa(ie,iang,nejc,1) = CSEa(ie,iang,nejc,1) + ddxs(iang)
            ENDDO
         ENDDO
      ENDDO

      if(allocated(ddxs)) deallocate(ddxs)
C
C      write(8,*) 'End of PCROSS    :',sngl(totemis),sngl(Xsinl)
C      write(*,*) 'End of PCROSS    :',sngl(totemis),sngl(Xsinl)
C
      write(8,*) 
      WRITE (8,99020)
99020 FORMAT (/' ',125('*')/)
      RETURN
      END

      SUBROUTINE KALBACH(Jcom,Jpin,Jninp,Jpout,Jnout,Elab,Esys,
     &Bin, Eps, Total, Fmsd, Sigma, Xcos, NDang, PI)
C
C     Converted to subroutine for EMPIRE by Roberto Capote (May 2005)
C
C     Taken from RIPL-1 files
C
C     Compound nucleus mass and charge (ac,zc) -> jcom,jzcom
C     Projectile charge and neutron numbers    -> jpin,jnin
C     Ejectile charge and neutron numbers      -> jpout,jnout
C     Incident energy in MeV [lab system]      -> elab
C     Excitation energy in MeV [cms]           -> esys
C     Binding energy of the ejectile in MeV    -> bin
C     Emission energy in MeV                   -> eps
C     Emitted cross section [mb/MeV]           -> total
C     Prequilibrium fraction of total          -> fmsd (assumed pure PE here)
C     sigma(NDAng)                             -> DDXS(angle) [mb/MeV/str]
C
C     MB Chadwick modified angel92 to include photuclear reactions,
C     Oct 95, according to the paper to be published in J. Nucl.
C     Sci Eng. (Japan), Nov 1995.  Changes labeled with "cmbc"
C     ANGEL92
C     continuum angular distributions
C     from empirical systematics based on exponentials of cos theta
C          see Phys. Rev. C 37 (1988) 2350
C
C     Written by C. Kalbach
C          April 1987
C     Revised February 1992 to give a smooth transitions
C     Single formulae for E1 and E3.
C          see Nucl. Sci. Eng. 115 (1993) 43
C
C     Third term in 'a' needed for incident N and d but not alpha.
C          Tentatively used for incident t and He-3
C     Transition energies tentatively set at
C          Et1 = 130 MeV (a=n,p,d,t,He3) or 260 MeV (a=alpha)
C          Et3 = 35 MeV (a=n,p,d,t,He3)
C     (only nucleon values of Et1; N and d values of Et3 known)
C
C     *The angel of the Lord encampeth round about them that fear him,
C     *and delivereth them.  (Psalm 34.7)
C
      implicit real*8 (A-H,O-Z)
      implicit integer (I-N)
      INCLUDE 'dimension.h'
      REAL*8 arg, a, xnorm, eps,total,fmsd, bin, elab, esys
      REAL*8 Sigma(NDAngecis), XCOs(NDAngecis)
Cmbc  MB Chadwick, added coding Oct 95, for photonuclear reactions
      jflagph=0
      jcomt = Jcom
      jnin = Jninp
      if(jpin.eq.0.and.jnin.eq.0)then
C        the following is a modification for photonuclear reactions
         jflagph=1
         jcomt = Jcom+1
         jnin=1
      endif
Cmbc  I follow the prescription in the paper "Photonuclear angular
Cmbc  distribution systematics in the quasideuteron regime", MB Chadwick,
Cmbc  PG Young, and S Chiba, to be published in the Nov 1995 issue of
Cmbc  the Journal of Nucl. Sci and Tech. (Japan). The basic idea is
Cmbc  to first calculate the Kalbach MSD a-parameter assuming a neutron
Cmbc  projectile instead of a photon, and the modify a to account for
Cmbc  (a) reduced forward peaking since a photon carries less momentum;
Cmbc  (b) less diffraction/refraction effects since the photon is undistorted.
Cmbc  end of mbc code (more later)
C%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
C
C     Calculate energy independent quantities
C     and print headings
C
      jin=jpin+jnin
      jout=jpout+jnout
C
C     Start of parametrization
C
      er=1.
      if (jin.gt.3) er=2.
C     er = Et1/130.
      esysr = esys / er
      e1 = esys
      if (esysr.gt.190.) then
           e1 = 130.*er
      else if (esysr.gt.80.) then
           x = (esysr-130.)/12.
           x = 1. + exp(x)
           e1 = esys / x
           x = (136.-esysr)/12.
           x = 1. + exp(x)
           e1 = e1 + 130.*er/x
      end if
      if (jin. le.3) then
        e3 = esys
        if (esysr.gt.51.) then
           e3 = 35.*er
        else if (esysr.gt.21.) then
           x = (esysr-35.)/3.2
           x = 1. + exp(x)
           e3 = esys / x
           x = (36.6-esysr)/3.2
           x = 1. + exp(x)
           e3 = e3 + 35.*er/x
        end if
        xmb=1
        if(jout.eq.4) xmb=2.
        if(jpout.eq.0)xmb=0.5
      endif
C
C     energy dependent input
C     calculate and print angular distributions
C
      if(fmsd.le.0.)fmsd=1.d0
      epscm=eps+bin
      y = epscm*e1 / (esys*130.)
      a = 5.2*y + 4.*y*y*y
      if (jin.le.3) then
        y = epscm*e3 / (esys*35.)
        a = a + 1.9*y*y*y*y*xmb
      endif

      xnorm = a*total / (4*PI*dsinh(a))

      do i=1,NDAng
        arg=a*xcos(i)
        sig=fmsd*dsinh(arg)+dcosh(arg)
Cmbc%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
Cmbc    added code for photonuclear reactions. Modify
Cmbc    a which was calculated for a neutron, to be valid for a
Cmbc    photon projectile:
        if(jflagph.eq.1) then
          facmom=dsqrt(elab/(2.d0*939))
          facrefr=9.3/dsqrt(eps)
          if(facrefr.lt.1.)facrefr=1.
          if(facrefr.gt.4.)facrefr=4.
          aphnuc=a*facmom*facrefr
          gth=((2.*aphnuc)/(dexp(aphnuc)-dexp(-aphnuc)))
          gth=gth*(1./(4*PI))*dexp(aphnuc*xcos(i))
Cmbc      Now put in MSC as isotropic, giving:
          sigma(i)=((1.-fmsd)/(4*PI))+(fmsd*gth)
          sigma(i)=sigma(i)*total
          cycle
        endif
        sigma(i)=sig*xnorm
      enddo
      return
      end

      SUBROUTINE RQFACT(Heq,R)

      use angular_momentum

      implicit none
      INCLUDE 'dimension.h'
      INCLUDE 'global.h'
C
C PARAMETER definitions
C
      INTEGER*4 PMAX
      PARAMETER (PMAX = 50)
C
C Dummy arguments
C
      INTEGER*4 Heq
      REAL*8 R(4,PMAX,NDEJC)
C
C Local variables
C
      INTEGER*4 ab, ac, ap, at, h1, hhh, i, j, j1, l, m, maxjx, minjx,
     &          nb, np, nt, p, zb, zc, zp, zt
      REAL*8 f1, f2, f21, f22, f23, f24, s1, s2, ta, tn, tz, uuu, zzz
      INTEGER nejc
C
C     INDC(CPR)-014 and references there in

      R = 1.d0 
      ac = A(1)
      zc = Z(1)
      ap = AEJc(0)
      at = ac - ap
      zp = ZEJc(0)
      zt = zc - zp
      nt = at - zt
      np = ap - zp
      tn = nt
      ta = at
      tz = zt
      zzz = tz/ta
      uuu = 1.D0 - zzz
C
C-----Q-FACTOR CALCULATION FOR PARTICLE EMISSION
C
      DO nejc = 1, NEJcm
         ab = AEJc(nejc)
         zb = ZEJc(nejc)
         nb = ab - zb
         DO l = 1, ab
            DO h1 = 1, Heq
               hhh = h1 - 1
               p = hhh + ap
               IF (p.GE.l) THEN
                  m = ab - l
                  minjx = MAX(0,l - nb)
                  maxjx = MIN(l,zb)
                  s1 = 0.D0
                  DO i = 0, hhh
                     f1 = zzz**i*uuu**(hhh - i)
     &                    *DEXP(lfct(hhh) - lfct(hhh - i) - lfct(i))
                     s2 = 0.D0
                     DO j = minjx, maxjx
                        f21 = 1.D0
                        IF (j.GE.1) THEN
                           DO j1 = 0, j - 1
                              f21 = f21*FLOAT(zp + i - j1)
                           ENDDO
                           f21 = f21/fact(j)
                        ENDIF
                        f22 = 1.D0
                        IF (l - j.GE.1) THEN
                           DO j1 = 0, l - j - 1
                              f22 = f22*FLOAT(np + hhh - i - j1)
                           ENDDO
                           f22 = f22/fact(l - j)
                        ENDIF
                        f23 = 1.D0
                        IF (zb - j.GE.1) THEN
                           DO j1 = 0, zb - j - 1
                              f23 = f23*FLOAT(zt - i - j1)
                           ENDDO
                           f23 = f23/fact(zb - j)
                        ENDIF
                        f24 = 1.D0
                        IF (nb - l + j.GE.1) THEN
                           DO j1 = 0, 2 - l + j - 1
                              f24 = f24*FLOAT(nt - hhh + i - j1)
                           ENDDO
                           f24 = f24/fact(nb - l + j)
                        ENDIF
                        s2 = f21*f22*f23*f24 + s2
                     ENDDO
                     s1 = s1 + f1*s2
                  ENDDO
                  f2 = 1.D0
                  DO j = 0, m - 1
                     f2 = f2*(at - hhh - j)
                  ENDDO
                  f2 = DEXP(lfct(l) + lfct(m) + lfct(p - l)
     &                 - lfct(p) + lfct(ab) - lfct(zb)
     &                 - lfct(nb))/f2
                  R(l,h1,nejc) = s1*f2*(ta/tz)**zb*(ta/tn)**nb
               ENDIF
            ENDDO
         ENDDO
      ENDDO
      DO nejc = 1, NEJcm
         ab = AEJc(nejc)
         DO h1 = 1, Heq
            DO l = 1, ab
               R(l,h1,nejc) = R(l,h1,nejc)/R(l,Heq,nejc)
            ENDDO
         ENDDO
      ENDDO
      END

      SUBROUTINE TRANSIT(E,Gc,Ap,Nheq)
      IMPLICIT LOGICAL*1(A - Z)
C
C PARAMETER definitions
C
      INTEGER*4 PMAX
      PARAMETER (PMAX = 50)
C
C COMMON variables
C
      REAL*8 LM(PMAX), LP(PMAX)
      INTEGER*4 NMAx
      COMMON /CALC3 / LP, LM, NMAx
C
C Dummy arguments
C
      INTEGER*4 Ap, Nheq
      REAL*8 E, Gc
C
C Local variables
C
      REAL*8 a0p, uc0
      REAL FLOAT
      INTEGER*4 h, h1, i, n, p
C
C-----TRANSITION RATES CALCULATIONS FOLLOWS
C
      DO i = 1, PMAX
         LP(i) = 0.
         LM(i) = 0.
      ENDDO
      NMAx = Nheq
      DO h1 = 1, Nheq
         h = h1 - 1
         p = h + Ap
         n = p + h
         a0p = FLOAT(p*(p - 1) + h*(h - 1))*.25
         uc0 = E - a0p/Gc
         IF (uc0.LE.0.D0) THEN
            NMAx = max(h1 - 1,1)
            RETURN
         ENDIF
C
C--------3/8 is coming from the Gupta paper
C
         LM(h1) = FLOAT(p*h)*FLOAT((n - 2)*(n - 1))/(Gc*Gc*uc0)*(3./8.)
         LP(h1) = uc0*(3./8.)
      ENDDO
C
C     END OF TRANSITION RATES CALCULATIONS [ NORMALIZED TO CME ]
C
      END


      SUBROUTINE RESOL(Em,Ih2,Ap,Cme)
      INCLUDE 'dimension.h'
      INCLUDE 'global.h'
C
C PARAMETER definitions
C
      INTEGER*4 PMAX
      PARAMETER (PMAX = 50)
C
C COMMON variables
C
      REAL*8 L(0:NDEJC,PMAX), LM(PMAX), LP(PMAX)
      INTEGER*4 NHEq, NMAx
      COMMON /CALC3 / LP, LM, NMAx
      COMMON /CALC5 / L, NHEq
C
C Dummy arguments
C
      INTEGER*4 Ap, Ih2
      REAL*8 Cme
      REAL*8 Em(PMAX)
C
C Local variables
C
      REAL*8 b(PMAX), c(PMAX), e(PMAX), hlp1, hlp2, hlp4, ln(PMAX),
     &       ls(PMAX)
      INTEGER*4 h1, hhh, i, ij, n
      DO i = 1, PMAX
         ln(i) = 0.
      ENDDO
      n = NMAx
      IF (n.GT.PMAX) n = PMAX
      DO h1 = 1, n
         DO i = 0, NEJcm
            ln(h1) = ln(h1) + L(i,h1)
         ENDDO
C-----------------------------------------
C--------NEVER COME BACK ASUMPTION
         LM(h1) = LM(h1)*1.D-7
C-----------------------------------------
         ls(h1) = Cme*ln(h1) + LP(h1) + LM(h1)
      ENDDO
      IF (IOUt.GE.3 .AND. NEJcm.LE.7) WRITE (8,99005)
99005 FORMAT (/2X,'N',5X,'T r a n s i t i o n   r a t e s   ',
     &        '        E  m  i  s  s  i  o  n     r  a  t  e  s',//,5X,
     &'   plus     minus     TOTAL              TOTAL     gammas   neutr
     &ons   protons   alphas   deuteron   triton     He-3'/)
      IF (IOUt.GE.3 .AND. NEJcm.EQ.7) WRITE (8,99010)
99010 FORMAT (/2X,'N',5X,'T r a n s i t i o n   r a t e s   ',
     &        '        E  m  i  s  s  i  o  n     r  a  t  e  s',//,5X,
     &'   plus     minus     TOTAL              TOTAL     gammas   neutr
     &ons   protons   alphas   deuteron   triton     He-3    light ions'
     &/)
      DO h1 = 1, n
         hhh = h1 - 1
         ij = 2*hhh + Ap
         hlp1 = LP(h1)/Cme
         hlp2 = LM(h1)/Cme
         hlp4 = hlp1 + hlp2
         IF (IOUt.GE.3) WRITE (8,99015) ij, hlp1, hlp2, hlp4, ln(h1),
     &                                            (L(i,h1),i = 0,NEJcm)
99015    FORMAT (I3,3E10.3,10X,9E10.3)
      ENDDO
C
C-----INITIAL CONDITIONS
C
      DO i = 1, PMAX
         b(i) = Em(i)
         Em(i) = 0.
      ENDDO
      c(1) = 0.D0
      DO h1 = 1, n - 1
         c(h1 + 1) = -LP(h1)
         e(h1) = -LM(h1 + 1)
      ENDDO
      CALL MASTER(c,ls,e,b,n,PMAX)
C--------------------------------------------------------------------
C-----NEVER COME BACK ASSUMPTION(ONLY PE-CONTRIBUTION)
      DO hhh = NHEq, n
         b(hhh) = 0.d0
      ENDDO
C--------------------------------------------------------------------
      ih2 = NHEq 
      DO h1 = 1, n
         Em(h1) = b(h1)*Cme
         IF (Em(h1).NE.0.d0) Ih2 = h1
      ENDDO
      Ih2 = MIN(Ih2,NHEq)
      IF (IOUt.GE.3) WRITE (8,99018)  2*(Ih2-1) + Ap, Ih2, CHMax
99018 FORMAT (/3X,'Nmax =',I3,' Hmax =',I3,' Coeff. CHMax =',F5.2/)
      IF (IOUt.GE.3) WRITE (8,99020)
99020 FORMAT (/3X,'TIME INTEGRALS OF TAU(N)'/3X,
     &        ' N   UP TO Nmax       ')
      IF (IOUt.GE.3) THEN
         DO h1 = 1, Ih2
            hhh = 2*(h1 - 1) + Ap
            WRITE (8,'(I5,2E14.3)') hhh, Em(h1)
         ENDDO
         WRITE (8,*)
      ENDIF
      END


      SUBROUTINE MASTER(Subd,Diag,Superd,B,N,Ndim)
      IMPLICIT LOGICAL(A - Z)
C
C Dummy arguments
C
      INTEGER*4 N, Ndim
      REAL*8 B(Ndim), Diag(Ndim), Subd(Ndim), Superd(Ndim)
C
C Local variables
C
      DOUBLE PRECISION DABS
      INTEGER*4 k, kb, kp1, nm1, nm2
      REAL*8 t
      Subd(1) = Diag(1)
      nm1 = N - 1
      IF (nm1.GE.1) THEN
         Diag(1) = Superd(1)
         Superd(1) = 0.0D0
         Superd(N) = 0.0D0
         DO k = 1, nm1
            kp1 = k + 1
            IF (DABS(Subd(kp1)).GE.DABS(Subd(k))) THEN
               t = Subd(kp1)
               Subd(kp1) = Subd(k)
               Subd(k) = t
               t = Diag(kp1)
               Diag(kp1) = Diag(k)
               Diag(k) = t
               t = Superd(kp1)
               Superd(kp1) = Superd(k)
               Superd(k) = t
               t = B(kp1)
               B(kp1) = B(k)
               B(k) = t
            ENDIF
            IF (Subd(k).EQ.0.0D0) STOP
     &            ' DIAGONAL ELEMENT = 0 in SOLUTION of MASTER equation'
            t = -Subd(kp1)/Subd(k)
            Subd(kp1) = Diag(kp1) + t*Diag(k)
            Diag(kp1) = Superd(kp1) + t*Superd(k)
            Superd(kp1) = 0.0D0
            B(kp1) = B(kp1) + t*B(k)
         ENDDO
      ENDIF
      IF (Subd(N).NE.0.0D0) THEN
C
C--------BACK ITERATION
C
         nm2 = N - 2
         B(N) = B(N)/Subd(N)
         IF (N.GT.1) THEN
            B(nm1) = (B(nm1) - Diag(nm1)*B(N))/Subd(nm1)
            IF (nm2.GE.1) THEN
               DO kb = 1, nm2
                  k = nm2 - kb + 1
                  B(k) = (B(k) - Diag(k)*B(k + 1) - Superd(k)*B(k + 2))
     &                   /Subd(k)
               ENDDO
            ENDIF
         ENDIF
      ENDIF
      END


      SUBROUTINE PREFORMATION(Flm,E)
C
C IWAMOTO,HARADA COALESCENCE PICK-UP MODEL, PHYS.REV.1982,V.26,P.1821
C
C Fitted and coded by R. Capote Noy, November 2002
C
C     Input : Energy E in cms
C     Output: FLM(Ejectile Mass,L)
C             L=1,Aejectile
C
C     i.e. FLM(4,L) means alpha particle preformation factor (L=1,4)
C
C
C Dummy arguments
C
      REAL*8 E
      REAL*8 Flm(4,4)
C
C Local variables
C
      INTEGER l
      REAL*8 x(5)
C-----FOR FAST POLYNOMIAL EVALUATION
      x(1) = E
      DO l = 2, 5
         x(l) = E*x(l - 1)
      ENDDO
C-----DEUTERON (MASS = 2)
      IF (E.LT.80.D0) THEN
         Flm(2,2) = MAX(0.00821D0 + 0.02038D0*x(1) + 5.95941D-4*x(2)
     &              - 2.24726D-5*x(3) + 2.38917D-7*x(4)
     &              - 8.34053D-10*x(5),0.0D0)
      ELSE
         Flm(2,2) = 1.
      ENDIF
      Flm(2,1) = MAX(1.D0 - Flm(2,2),0.0D0)
C-----TRITIUM or HELIUM-3 (MASS = 3)
      IF (E.LT.70.D0) THEN
         Flm(3,1) = MAX(0.57315D0 - 0.02083D0*x(1) + 3.19204D-4*x(2)
     &              - 2.85876D-6*x(3) + 1.26332D-8*x(4),0.D0)
      ELSE
         Flm(3,1) = 0.D0
      ENDIF
      IF (E.LT.170.) THEN
         Flm(3,3) = MAX(0.00705D0 - 0.00164D0*x(1) + 2.16549D-4*x(2)
     &              - 1.73867D-6*x(3) + 5.24069D-9*x(4)
     &              - 5.79848D-12*x(5),0.D0)
      ELSE
         Flm(3,3) = 1.D0
      ENDIF
      Flm(3,2) = MAX(1.D0 - Flm(3,1) - Flm(3,3),0.D0)
C-----ALPHA PARTICLES (M=4)
      IF (E.LT.70.D0) THEN
         Flm(4,1) = MAX(0.29119 - 0.01434*x(1) + 3.34045E-4*x(2)
     &              - 4.10957E-6*x(3) + 2.02375E-8*x(4),0.D0)
      ELSE
         Flm(4,1) = 0.D0
      ENDIF
      IF (E.LT.130.D0) THEN
         Flm(4,2) = 0.60522 + 1.30071E-4*x(1) - 1.77653E-4*x(2)
     &              + 1.64048E-6*x(3) - 4.55562E-9*x(4)
     &              + 2.09521E-12*x(5)
      ELSE
         Flm(4,2) = 0.D0
      ENDIF
      IF (E.LT.30.D0) THEN
         Flm(4,4) = 0.D0
      ELSEIF (E.LT.300.D0) THEN
         Flm(4,4) = 0.01917 - 0.00307*x(1) + 9.57966E-5*x(2)
     &              - 4.07085E-7*x(3) + 5.28037E-10*x(4)
     &              + 7.92029E-16*x(5)
      ELSE
         Flm(4,4) = 1.D0
      ENDIF
      IF (E.LT.300.D0) THEN
         Flm(4,3) = MAX(1.D0 - Flm(4,1) - Flm(4,2) - Flm(4,4),0.D0)
      ELSE
         Flm(4,3) = 0.D0
      ENDIF
      END


      REAL*8 FUNCTION DENSW(G,D,P,H,E)

      use angular_momentum

C  RIPL FORMULATION
C
C  G - LEVEL DENSITY PARAMETER
C  D - PAIRING CORRECTION
C  P(H) - PARTICLE(HOLE) NUMBER
C  E - EXCITATION ENERGY
C
C COMMON variables
C
      REAL*8 VV
      COMMON /VWELL / VV
C
C Dummy arguments
C
      REAL*8 D, E, G
      INTEGER*4 H, P
C
C Local variables
C
      REAL*8 a, fac, u, sum
      INTEGER*4 n, j, jmax

      DENSW = 0.D0
      n = P + H - 1
      IF (n < 0) RETURN

      jmax = H
      IF(VV.LE.0.d0) jmax=0
C
C     Following RIPL TECDOC (LD chapter)
C
      a = .5D0*(P*P + H*H)
      sum = 0.d0
      DO j = 0,H
        fac = lfct(P) + lfct(n) + lfct(j) + lfct(H - j) !RCN 0914
        u = G*(E - D - j*VV) - a
C       Changed Sept. 2010  
        IF (u.LE.0.) cycle
        sum = sum + (-1)**j * G*(DEXP(dble(n)*DLOG(u) - fac))
      ENDDO
      if(sum.lt.0.d0) return
      DENSW = sum
      RETURN
      END

      REAL*8 FUNCTION DENSW1(G,D,P,H,E)

      use angular_momentum

C  RIPL FORMULATION
C
C  G - LEVEL DENSITY PARAMETER
C  D - PAIRING CORRECTION
C  P(H) - PARTICLE(HOLE) NUMBER
C  E - EXCITATION ENERGY
C
C Dummy arguments
C
      REAL*8 D, E, G
      INTEGER*4 H, P
C
C Local variables
C
      REAL*8 a, fac, u
      DOUBLE PRECISION DEXP, DLOG
      INTEGER*4 n

      DENSW1 = 0.D0
      n = P + H - 1
      IF (n < 0) RETURN

      a = .5D0*(P*P + H*H)

      fac = lfct(P) + lfct(n) + lfct(H)
      u = G*(E - D) - a
      IF (u.LE.0.D0) return
      DENSW1 =  G*(DEXP(dble(n)*DLOG(u) - fac))

      RETURN
      END

      FUNCTION SGAM(A,Z,Eg,Uex,Tt)
C
C     Photoabsorption cross-section in "mb"
C     If "Key_shape = 0" old expression for photoabsorption
C     cross-section  is used, namely:
C     [1] S.A.FAYANS, "Lectures,DUBNA,feb.1982"
C     SGAM = GAMMA ABSORPTION CROSS SECTION in mb
C
C COMMON variables
C
      INTEGER KEY_gdrgfl, KEY_shape
      COMMON /GSA   / KEY_shape, KEY_gdrgfl
C
C Dummy arguments
C
      REAL*8 A, Eg, Z, Tt, Uex
      REAL*8 SGAM
C
C Local variables
C
      REAL*8 a3, egr, gam, sgm
      REAL*8 GAMMA_STRENGTH, GAMMA_STRENGTH_micro
      INTEGER nnucc
      nnucc = 1
      IF(KEY_shape.EQ.8)THEN
         SGAM = (10.D0/8.674D-7)*
     >           Eg*GAMMA_STRENGTH_micro(nnucc,Eg) ! nnnuc = CN
      ELSEIF (KEY_shape.NE.0) THEN
         SGAM = (10.D0/8.674D-7)*
     >           Eg*GAMMA_STRENGTH(Z,A,Uex,Tt,Eg,KEY_shape,nnucc)
      ELSE
C        If "Key_shape = 0" old expression for photoabsorption
C        cross-section  is used, namely:
C        SGAM_NEW = SGAM
C        [1] S.A.FAYANS
C        "Lectures,DUBNA,feb.1982"
C        SGAM = GAMMA ABSORPTION CROSS SECTION in mb
         a3 = A**(.3333333333333333D0)
         egr = 29.*SQRT((1.D0 + 2.D0/a3)/a3)
         gam = 5.D0
         sgm = 53.2D0*(A - Z)*Z/A
         SGAM = sgm*gam*Eg*Eg/((Eg*Eg - egr*egr)**2 + (gam*Eg)**2)
      ENDIF
      RETURN
      END
