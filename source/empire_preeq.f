Ccc   * $Rev: 3705 $
Ccc   * $Author: rcapote $
Ccc   * $Date: 2014-01-04 22:01:02 +0100 (Sat, 04 Jan 2014) $

      SUBROUTINE EMPIRE_PREEQ
     &     (xsinl,xsmsc,tothms,totemis,corrmsd,totcorr)

      use empcess, ONLY: CSHms

      implicit none
      INCLUDE "dimension.h"
      INCLUDE "global.h"
      
	DOUBLE PRECISION xsinl,xsmsc,tothms,totemis,corrmsd,totcorr

      LOGICAL nvwful, fexist
      CHARACTER*23 ctmp23

      DOUBLE PRECISION qmax,qstep,q2,q3,ftmp,echannel,dtmp 
      INTEGER	ltrmax, i, nejc, nnur, itimes, its, iad, iam, ia, iang, ie 
      INTEGER icalled

C     COMMON variables
      DOUBLE PRECISION ELAcs, TOTcs, ABScs, SINl, SINlcc, SINlcont       
      COMMON /ECISXS/ ELAcs, TOTcs, ABScs, SINl, SINlcc, SINlcont

      LOGICAL lbreakup, ltransfer
      COMMON /LPEXS/lbreakup, ltransfer 

      DOUBLE PRECISION crossNT(0:NDEJC),crossNTt,
     &                 crossPE(0:NDEJC),crossPEt
      COMMON /PEXS/ crossNT,crossNTt,crossPE,crossPEt

      DOUBLE PRECISION specBU(0:NDEJC,ndecse),crossBU(0:NDEJC),crossBUt
      COMMON /CBREAKUP/specBU,crossBU,crossBUt

      DOUBLE PRECISION ELTl(NDLW)    
      COMMON /ELASTIC/ ELTl

      CHARACTER*3 ctldir
      DATA ctldir/'TL/'/

C     initialization
      xsinl   = 0.d0
      xsmsc   = 0.d0
     	tothms  = 0.d0
	totemis = 0.d0  
      corrmsd = 1.d0

      WRITE (ctmp23,'(i3.3,i3.3,1h_,i3.3,i3.3,1h_,i9.9)') INT(ZEJc(0)),
     &       INT(AEJc(0)), INT(Z(0)), INT(A(0)), INT(EINl*1000000)

C-----
C-----Calculate MSD contribution
C-----
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

         corrmsd = (CSFus - (xsinl + totemis))/CSFus
         IF (corrmsd.LT.0.0D0) THEN
            write(8,*) ' CSFus=',sngl(CSFus),
     &        ' xsinl   (MSD)   =',sngl(xsinl),
     &        ' totemis (PCROSS)=',sngl(totemis)
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
C        ftmp = 0.d0
         DO i = 1, NLW
            POP(NEX(1),i,1,1) = POP(NEX(1),i,1,1)*corrmsd
            POP(NEX(1),i,2,1) = POP(NEX(1),i,2,1)*corrmsd
C           ftmp = ftmp + POP(NEX(1),i,1,1) + POP(NEX(1),i,2,1)
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
           DO ie = 1, NDEcse
              CSE (ie,i,1) = CSE (ie,i,1) + CSEmsd(ie,i)
              CSEt(ie,i  ) = CSEt(ie,i  ) + CSEmsd(ie,i)      
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
C-----
C-----HMS Monte Carlo preequilibrium emission
C-----        
      IF ( EINl.GT.0.1D0 .AND. LHMs.NE.0 .AND. MSD+MSC.EQ.0) THEN
         ftmp = IZA(0)
         CALL DDHMS(IZAejc(0),ftmp,XJLv(LEVtarg,0),EINl,
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
          IF(CSEmis(0,1).GT.0) CALL AUERST(1,0)
          IF(CSEmis(1,1).GT.0) CALL AUERST(1,1)
          IF(CSEmis(2,1).GT.0) CALL AUERST(1,2)
          IF(CSEmis(3,1).GT.0) CALL AUERST(1,3)
          IF(CSEmis(4,1).GT.0) CALL AUERST(1,4)
          IF(CSEmis(5,1).GT.0) CALL AUERST(1,5)
          IF(CSEmis(6,1).GT.0) CALL AUERST(1,6)
          WRITE (8,*)
     &                ' End of Preequilibrium + Direct spectra (tot)'
          WRITE (8,*) ' ********************************************'
          WRITE (8,*) ' ********************************************'
          WRITE (8,*) 
      ENDIF

C     Renormalizing elastic transmission coefficients to consider PE emission
      DO i = 1, NDLW
          ELTl(i) = ELTl(i) * corrmsd 
      ENDDO

      WRITE (12,*) ' '
      WRITE (12,'('' FUSION CROSS SECTION = '',1P,E12.5,'' mb'')')
     &     CSFus + (SINl + SINlcc)*FCCred + SINlcont*FCOred
      WRITE (12,'('' TOTAL  CROSS SECTION = '',1P,E12.5,'' mb'')')
     &     TOTcs*TOTred*totcorr
      WRITE (12,*) ' '
C
      IF (IOUt.GT.1) WRITE (8,*)
     &   '*** Summary of Hauser-Feshbach equilibrium decay'
C
      RETURN
      END