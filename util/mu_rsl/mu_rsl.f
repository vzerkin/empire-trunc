      PROGRAM MU_RSL
C-Title  : MU_RSL Program
C-Purpose: Resolution-broaden cross sections
C-Author : A.Trkov, Jozef Stefan Institute, Ljubljana, Slovenia
C-Version: 2013 Original code
C-V  2014/11 Major upgrade to resolution-broaden MF4 data
C-V  2015/11 Extend MAT0=0 definition to process all materials on file.
C-V  2016/03 Increase MXN from 300k to 800k.
C-V  2016/04 Relax the No.of zero x.s. condition for scatt.mom. to 1%
C-M  
C-M  Manual for Program MU_RSL
C-M  =========================
C-M  Sometimes it is convenient to compare cross sections and/or
C-M  angular distributions in low resolution to identify the general
C-M  trends or make comparisons between evaluated data sets
C-M  without the details of the resonance structure. The MU_RSL
C-M  code was designed for this purpose. It reads an ENDF file
C-M  and writes an ENDF file with the specified cross sections or
C-M  angular distributions in resolution-broadened form.
C-M
C-M  When processing angular distributions it really makes sense
C-M  to resolution-broaden the product of the cross section and the
C-M  distribution. This is achieved by specifying an appropriate
C-M  value of the MF input parameter as described below.
C-M
C-M  Resolution-broadened cross sections or Legendre coefficients 
C-M  of angular distributions are written to the output ENDF file.
C-M  If Legendre coefficient are processed, an additional file
C-M  MU_RSL.CUR is written containing the input and the resolution-
C-M  broadened scattering moments (or Legendre coefficients)
C-M  for all Legendre orders.
C-M  
C-M  Instructions:
C-M  The user is prompted for the following input parameters:
C-M   FLIN Filename of the ENDF file to be edited
C-M        (default ENDFold.dat).
C-M   FLOU Filename of the new updated ENDF file
C-M        (default ENDFnew.dat).
C-M   MAT0 Material MAT to be processed.
C-M        If zero, all materials are processed.
C-M   MT   Reaction MT to be processed
C-M          MT>1000 implies MF=MT/1000 and MT=MT-1000*MF
C-M          MF=   4 implies that angular distributions are
C-M                  resolution-broadened
C-M          MF= 304 implies that scattering moments are resolution
C-M                  broadened (i.e. the cross section times the
C-M                  distribution).
C-M   FRC  Fractional half-width at half-maximum (tipically 0.3)
C-M        or the absolute width (eV), if FRC>1 (~10000 for strong
C-M        smoothing).
C-M        WARNING2- this option needs checking!!!
C-M   NRS  Number of points to represent the resolution function
C-M        (tipically 20 - 50)
C-M
C-M  NOTE : At present it is assumed that the angular distributions
C-M         are given in Legendre polynomial representation.
C-M
C-M  WARNING: The resolution-broadening algorithm was not optimised
C-M           for efficiency. When the number of points is large,
C-M           execution times may become very long and the number of
C-M           additional points very large; no thinning was
C-M           implemented.
C-
C* MXW is the real work field array dimension (MXW >> 2*MXN)
C* MXN is the maximum number of points in the new array
C* MXR is the maximum number of points in the resolution function array
      PARAMETER   (MXW=12 000 000,MXN=4 000 000,MXR=2000)
      CHARACTER*80 REC,FLNM,FLIN, FLOU, FLTM
      CHARACTER*40 BLNK
      CHARACTER*66 CH66
      DIMENSION    RWO(MXW)
      DIMENSION    ENX(MXN),XSX(MXN), ENR(MXR),RSL(MXR)
     &            ,NBT(20),INR(20)
C* Filenames and logical file units
      DATA LIN,LOU,LTM,LKB,LTT / 1, 3, 4, 5, 6 /
      DATA BLNK/'                                        '/
     1    ,FLIN/'ENDFold.dat'/
     3    ,FLOU/'ENDFnew.dat'/
     4    ,FLTM/'MU_RSL.CUR'/
C*
C* Initialise criterion for Gaussian tail cutoff
      EPS=1E-3
      ZRO=0
      IZR=0
C* Default number of points for the resolution function
      FRC=0
      NRS=50
      MF0= 3
      MOM= 0
C* Define input parameters - Write banner to terminal
      WRITE(LTT,991)
      WRITE(LTT,991) ' MU_RSL - Resolution-broaden x-sections '
      WRITE(LTT,991) ' ====================================== '
      WRITE(LTT,991)
C* Define the source ENDF file to be edited
   13 WRITE(LTT,991) ' Default ENDF file to be edited       : ',FLIN
      WRITE(LTT,991) '$          Enter new name to redefine : '
      READ (LKB,986) FLNM
      IF(FLNM(1:40).NE.BLNK) FLIN=FLNM
      OPEN(UNIT=LIN,FILE=FLIN,STATUS='OLD',ERR=13)
C* Define the output file
   14 WRITE(LTT,991) ' Default output filename              : ',FLOU
      WRITE(LTT,991) '$          Enter new name to redefine : '
      READ (LKB,986) FLNM
      IF(FLNM(1:40).NE.BLNK) FLOU=FLNM
      OPEN (UNIT=LOU,FILE=FLOU,STATUS='UNKNOWN')
C* Define the material to be processed
      WRITE(LTT,991) '$Material MAT to be processed           '
      WRITE(LTT,991) '$(if MAT=0 all will be processed)     : '
      READ (LKB,995) MAT0
C* Define the MT to be processed
      WRITE(LTT,991) '$Reaction MT to be resolution-broaden   '
      WRITE(LTT,991) '  MT>1000 implies MF=MT/1000 and MT=MT-1000*MF'
      WRITE(LTT,991) '  MF=   4 for Leg. coefficients smoothing'
      WRITE(LTT,991) '  MF= 304 for scattering moments smoothing:'
      READ (LKB,995) MTCMB
      MT0=MTCMB
      IF(MT0.GT.1000) THEN
        MF0=MT0/1000
        MT0=MT0-1000*MF0
C*      -- Check if angular distribution moments are to be calculated
        MOM=MF0/100
        MF0=MF0-MOM*100
      ELSE
        MOM=0
        MF0=3
      END IF
C* Define the Fractional or absolute half-width at half-maximum
      WRITE(LTT,991) '$Fract./Abs(eV). width at half-maximum '
      WRITE(LTT,991) '   typically 0.3 for fractional '
      WRITE(LTT,991) '   if >1.0 absolute in eV (strong ~10000) :'
      READ (LKB,996) FRC
C*    -- Detect if relative (IRR=0) or absolute (IRR=1)
      IF(FRC.GT.1) THEN
        IRR=1
      ELSE
        IRR=0
        IF(NINT(FRC*10000).EQ.0) IRR=-1
      END IF
C* Define the maximum number of points for the resolution function
      WRITE(LTT,991) ' No. of points for resolution function  '
      WRITE(LTT,991) '        (typically 20-50, default 50) : '
      READ (LKB,986,END=20) FLNM
      IF(FLNM(1:40).NE.BLNK) READ (FLNM,995) NRS
      IF(NRS.GT.MXR) STOP 'MU_RSL ERROR - MXR limit exceeded'
C*
C* Summarize input options on screen
   20 WRITE(LTT,991) BLNK  
      WRITE(LTT,991) ' ENDF file to be edited               : ',FLIN
      WRITE(LTT,991) ' Output ENDF file                     : ',FLOU
      WRITE(LTT,997) ' Material MAT to be processed         : ',MAT0
      WRITE(LTT,997) ' Input (combined) MT code             : ',MTCMB
      WRITE(LTT,997) ' Reaction MF to be processed          : ',MF0
      WRITE(LTT,997) ' Reaction MT to be processed          : ',MT0
      IF(IRR.EQ.1) THEN
        WRITE(LTT,998) ' Absolute width at half-maximum (eV)  : ',FRC
        WRITE(LTT,997) ' No. of points for resolution function: ',NRS
      ELSE IF(IRR.EQ.0) THEN
        WRITE(LTT,998) ' Fractional width at half-maximum     : ',FRC
        WRITE(LTT,997) ' No. of points for resolution function: ',NRS
      ELSE
        WRITE(LTT,998) ' WARNING - No resol.broadening requested'
        WRITE(LTT,998) '           Copy to output as is         '
      END IF
      IF     (MF0.EQ.3) THEN
C*      -- Valid enrty - no action needed
      ELSE IF(MF0.EQ.4) THEN
        IF     (MOM.EQ.0) THEN
          WRITE(LTT,991) '     Legendre coefficients are broadened'
        ELSE IF(MOM.EQ.3) THEN
          WRITE(LTT,991) '        Scattering moments are broadened'
        END IF
      ELSE
        WRITE(LTT,997) ' MU_RSL ERROR - Invalid option for MF ',MF0
        GO TO 902
      END IF
      WRITE(LTT,991) BLNK
C*
C* Open the scratch file
      OPEN(UNIT=LTM,FILE=FLTM,STATUS='UNKNOWN')
C*
C* Tabulate a Gaussian resolution-broadening function into RSL at ENR
      IF (IRR.GE.0) CALL FNGAUS(NRS,ENR,RSL,FRC,EPS)
c...
c...  ss=0
c...  do i=1,nrs
c...    if(i.gt.1) ss=ss+(enr(i)-enr(i-1))*(rsl(i)+rsl(i-1))/2
c...    print *,enr(i),rsl(i),ss
c...  end do
c...  stop
c...
C*
C* Copy the ENDF file header
      NS  =-1
      NMAT=0
      MAT1=MAT0
      CALL RDTEXT(LIN,MAT,MF,MT,CH66,IER)
      CALL WRTEXT(LOU,MAT,MF,MT,NS,CH66)
   40 IF(IRR.LT.0) GO TO 80
C* Findt the appropriate MT section of MF3 in the ENDF file
      CALL RDTEXT(LIN,MAT,MF,MT,CH66,IER)
      CALL WRTEXT(LOU,MAT,MF,MT,NS,CH66)
      IF(MAT.LT.0. OR. IER.NE.0) THEN
C*      -- Normal termination if at least one material was processed
        IF(NMAT.GT.0) GO TO 900
        PRINT *,' MU_RSL ERROR - IER',IER,' on record:'
        PRINT *,' "'//CH66,'"',MAT,MF,MT
        GO TO 901
      END IF
      IF(MAT.GT.0 .AND. MAT1.EQ.0) MAT1=MAT
      IF(MAT.NE.MAT1) GO TO 40
C* Even for MF4 data it is necessary to retrieve the MF 3 x.s. first
      IF(MF.NE.3 .OR. MT.NE.MT0) GO TO 40
      CALL RDTAB1(LIN,C1,C2,L1,L2,NR,NP,NBT,INR,ENX,XSX,MXN,IER)
      IF(IER.NE.0) THEN
        WRITE(LTT,*) ' ERROR',IER,' reading MF3/MT',MT
        IF(IER.EQ.9) THEN
          WRITE(LTT,*) '         Exceeded array limit',MXN
        END IF
        GO TO 902
      END IF
      INR3=INR(1)
C*    -- Perform resolution-broadening of the cross section
C*    -- Pack the broadened function after the original
      LP1=NP+1
      MX1=MXN-NP
      WRITE(LTT,*) 'MAT',MAT1,' MT',MT0,', Points to process',NP
c---  CALL RSLBR0(NP,ENX,XSX,NPX1,MX1,ENX(LP1),XSX(LP1)
      CALL RSLBR1(NP,ENX,XSX,NPX1,MX1,ENX(LP1),XSX(LP1)
     &           ,NRS,ENR,RSL,MXW,RWO,IRR)
C...
C...      -- Print the cross sections to the CUR file
c...      WRITE(LTM,*) 'Input cross sections for MT',MT0
c...      DO I=1,NP
c...        WRITE(LTM,'(1P,E11.5E1,E11.4)') ENX(I),XSX(I)
c...      END DO
c...      WRITE(LTM,*) ' '
c...      WRITE(LTM,*) 'Broadened cross sections for MT',MT0
c...      DO I=1,NPX1
c...        WRITE(LTM,'(1P,E11.5E1,E11.4)') ENX(LP1-1+I),XSX(LP1-1+I)
c...      END DO
c...      WRITE(LTM,*) ' '
C...
C*    -- Write the cross sections
      IF(MF0.EQ.3) THEN
C*      -- Write the broadened cross sections back into the ENDF file
        NBT(1)=NPX1
        CALL WRTAB1(LOU,MAT,MF,MT,NS,C1,C2,L1,L2,NR,NPX1,NBT,INR
     &             ,ENX(LP1),XSX(LP1))
      ELSE IF(MF0.EQ.4) THEN
C*      -- Write the original cross sections back into the ENDF file
        CALL WRTAB1(LOU,MAT,MF,MT,NS,C1,C2,L1,L2,NR,NP,NBT,INR,ENX,XSX)
C*      -- Continue search for angular distributions
   50   CALL RDTEXT(LIN,MAT,MF,MT,CH66,IER)
        CALL WRTEXT(LOU,MAT,MF,MT,NS,CH66)
        IF(MAT.LT.0. OR. IER.NE.0) THEN
          PRINT *,' MU_RSL ERROR - IER',IER,' on record:'
          PRINT *,' "'//CH66,'"',MAT,MF,MT
          GO TO 901
        END IF
        IF(MAT.NE.MAT1) GO TO 50
        IF(MF.NE.MF0 .OR. MT.NE.MT0) GO TO 50
        PRINT *,' Found MAT/MF/MT',MAT,MF,MT
        READ(CH66(34:44),*) LTTFLG
        IF(LTTFLG.NE.1 .AND. LTTFLG.NE.3) THEN
          WRITE(LTT,*) 'WARNING - Resolution broadening of angular'
          WRITE(LTT,*) '          distrib. not possible for LTT',LTTFLG
          GO TO 80
        END IF
C*      -- Read the header information for angular distributions
        CALL RDHEAD(LIN,MAT,MF,MT,C1,AWR,L1,LCT4,N1,NM4,IER)
        IF(IER.NE.0) THEN
          WRITE(LTT,*) ' ERROR',IER,' reading CONT OF MF',MF0,'/MT',MT
          GO TO 902
        END IF
C...
C...    print *,'C1,AWR,L1,LCT4,N1,NM4',C1,AWR,L1,LCT4,N1,NM4
C...
        IF(NM4.LE.0) THEN
          NM4=64
          PRINT *,' WARNING - Undefined max.No. of Legendre coefficints'
          PRINT *,'           Assumed NM=',NM4
        END IF
        CALL RDTAB2(LIN,C1,C2,L1,L2,NR4,NE4,NBT,INR,IER)
        IF(IER.NE.0) THEN
          WRITE(LTT,*) ' ERROR',IER,' reading TAB2 OF MF',MF0,'/MT',MT
          GO TO 902
        END IF
        INR4=INR(1)
C*      -- Pack coefficients into P(i,l) equivalent to
C*         linearised array RWO(L1)
C*         index i   for energies,
C*               l=1 for energy,
C*               l>1 for coefficients
C*         where:
C*           LP4 is the index of the energy grid in RWO
C*           LX4 is the index of P in RWO
C*           NE4 is the number of energy points
        LP4=NM4+1
        MX1=MXW-LP4
        NN =(NM4+1)*NE4
        NL4=0
        LBL=LP4+NN
        IF(LBL.GT.MXW) THEN
          WRITE(LTT,*) 'ERROR - MXW Array capacity exceeded'
          GO TO 902
        END IF
C...
C...    print *,'nm4,nr4,ne4,lp4,nn',nm4,nr4,ne4,lp4,nn
C...
        DO I=1,NN
          RWO(LP4-1+I)=0
        END DO
        NLHI=0
        DO I=1,NE4
          CALL RDLIST(LIN,C1,EE,L1,L2,NL,N2,RWO,MX1,IER)
          IF(IER.NE.0) THEN
            WRITE(LTT,*) ' ERROR',IER,' reading CONT OF MF'
     &                  ,MF0,'/MT',MT,' at E',EE,' eV'
            GO TO 902
          END IF
          IF(NL.GT.NM4) THEN
            PRINT *,'ERROR - No. of Legendre coefficients',NL
            PRINT *,' exceeds max. declared NM=',NM4
            STOP 'MU_RSL ERROR - ENDF consistency error'
          END IF
          NLHI=MAX(NLHI,NL)
C*        -- Pack into RWO with energy index first
          J=LP4-1+I
          RWO(J)=EE
          XS=1
          IF(MOM.EQ.3) XS=FINTXS(EE,ENX,XSX,NP,INR3,IER)
          DO L=1,NL
            PL=RWO(L)
C*          -- Calculate moments of the angular distributions
            RWO(J+L*NE4)=PL*XS
C...
C...        PRINT *,J,L,J+L*NE4,RWO(J),RWO(J+L*NE4)
C...
          END DO
        END DO
C*      -- Redefine the Max. moment to the actual one on the file
        NL4=NLHI
C*      -- Resolution-broaden each of the coefficients 
C*         and save them on scratch unit LTM (CUR format)
C*           LPE - Address for energy (=energy grid of the first moment)
C*           LPX - Address of the broadened moments
        NN =(NM4+1)*NE4
        LPE=LP4+NN+1
        MX1=(MXW-LPE)/3
        LPX=LPE+MX1
        LBL=LPX+MX1
        IF(MX1.LE.NE4) THEN
          WRITE(LTT,*) 'ERROR - MXW Array capacity exceeded'
          GO TO 902
        END IF
        DO L=1,NL4
          WRITE(LTT,*) 'Moment',L,', Points to process',NE4
          LX4=LP4+L*NE4
c---      CALL RSLBR0(NE4,RWO(LP4),RWO(LX4),NP1,MX1,RWO(LPE),RWO(LPX)
          CALL RSLBR1(NE4,RWO(LP4),RWO(LX4),NP1,MX1,RWO(LPE),RWO(LPX)
     &               ,NRS,ENR,RSL,MX1,RWO(LBL),IRR)
C...
C...      DO K=1,NE4
C...        IF(K.LE.5 .OR.K.GT.NE4-2)PRINT *,L,RWO(LP4-1+K),RWO(LX4-1+K)
C...      END DO
C...      STOP
C*
C*        -- Print the input and broadened moments to the CUR file
          WRITE(LTM,*) 'Input Moment',L
          DO I=1,NE4
            WRITE(LTM,'(1P,E11.5E1,E11.4)') RWO(LP4-1+I),RWO(LX4-1+I)
          END DO
          WRITE(LTM,*) ' '
          WRITE(LTM,*) 'Broadened Moment',L
          DO I=1,NP1
            WRITE(LTM,'(1P,E11.5E1,E11.4)') RWO(LPE-1+I),RWO(LPX-1+I)
          END DO
          WRITE(LTM,*) ' '
C*        -- Energy grid of 1-st moment is adopted for output
          IF(L.EQ.1) NEO=NP1
        END DO
C*
C*      -- Read in from scratch into RWO with moments index first
C*         Redefine indices according to the reqirement
        REWIND LTM
        NN =(NL4+1)*NEO
c...
c...    print *,'NN,NL4,NEO',NN,NL4,NEO
c...
        LPE=LP4+NN+1
        LPX=(LPE+MXW)/2
        MX1=LPX-LPE
        IF(MX1.LT.NEO) THEN
          WRITE(LTT,*) 'ERROR - MXW Array capacity exceeded'
          GO TO 902
        END IF
C*      -- Read in the first moment
  60    READ (LTM,'(A)') FLNM
        IF(FLNM(1:17).NE.' Broadened Moment') GO TO 60
        DO I=1,NEO
          READ(LTM,'(2F11.0)') EE,PL
          IF(MOM.EQ.3) THEN
            XS=FINTXS(EE,ENX(LP1),XSX(LP1),NPX1,INR3,IER)
            IF(XS.GT.0)PL=PL/XS
          END IF
          RWO(LP4+(I-1)*(NL4+1)  )=EE
          RWO(LP4+(I-1)*(NL4+1)+1)=PL
        END DO
        WRITE(LTT,*) 'Read',NEO,' points for Broadened Moment',1
C*      -- Read in the remaining moments, interpolating to the
C*         chosen grid, if needed
        DO L=2,NL4
          NPL=0
   62     READ (LTM,'(A)') FLNM
          IF(FLNM(1:17).NE.' Broadened Moment') GO TO 62
c...
c...      PRINT *,'Reading ',FLNM(1:40)
c...
   64     READ (LTM,'(A)') FLNM
          IF(FLNM(1:40).NE.BLNK) THEN
            NPL=NPL+1
            READ(FLNM,'(2F11.0)') RWO(LPE-1+NPL),RWO(LPX-1+NPL)
            GO TO 64
          END IF
          WRITE(LTT,*) 'Read',NPL,' points for Broadened Moment',L
          NXSZ=0
          PL0 =0
          DO I=1,NEO
            EE=RWO(LP4+(I-1)*(NL4+1)  )
            IF(EE.GE.RWO(LPE) .AND. EE.LE.RWO(LPE+NPL-1)) THEN
              PL=FINTXS(EE,RWO(LPE),RWO(LPX),NPL,INR4,IER)
            ELSE
              PL=0
            END IF
            IF(MOM.EQ.3) THEN
              XS=FINTXS(EE,ENX(LP1),XSX(LP1),NPX1,INR3,IER)
              IF(XS.GT.0)THEN
                PL =PL/XS
                PL0=PL
              ELSE
C*              -- If x.s. equals zero, extrapolate flat
                NXSZ=NXSZ+1
                PL  =PL0
C...
                	IF(L.EQ.2) print *,'        Zero x.s. at E',EE,' eV'
C...
              END IF
            END IF
            RWO(LP4+(I-1)*(NL4+1)+L)=PL
          END DO
          IF(NXSZ.GT.0 .AND. L.EQ.2) THEN
            WRITE(LTT,*) ' WARNING - Zero x.s. for moments broadening'
            WRITE(LTT,*) '           at',NXSZ,' points'
C*          -- fatal error if zero-points exceed 1%
            IF((100*NXSZ)/NEO.GE.1)
     &        STOP 'MU_RSL ERROR - Zero x.s. for scatering moments'
          END IF
        END DO
C*      -- All moments read in
C*         Begin writing the ENDF file
        CALL WRCONT(LOU,MAT1,MF0,MT0,NS,ZRO,AWR,IZR,LCT4,IZR,NL4)
        NPDIF=NEO-NBT(1)
        DO I=1,NR4
          NBT(I)=NBT(I)+NPDIF
        END DO
        CALL WRTAB2(LOU,MAT1,MF0,MT0,NS,ZRO,ZRO,IZR,IZR
     1                 ,NR4,NEO,NBT,INR)
        DO I=1,NEO
          NLO=0
          EE=RWO(LP4+(I-1)*(NL4+1))
          DO L=1,NL4
            PL=RWO(LP4+(I-1)*(NL4+1)+L)
            RWO(L)=PL
            IF(PL.NE.0) NLO=L
          END DO
          CALL WRLIST(LOU,MAT1,MF0,MT0,NS,ZRO,EE,IZR,IZR,NLO,IZR,RWO)
        END DO
      ELSE
        PRINT *,' MU_RSL ERROR - Broadenin not supported for MF',MF0
      END IF
C*
C* Copy the rest of the file for this material
   80 CALL RDTEXT(LIN,MAT,MF,MT,CH66,IER)
      IF(IER.NE.0) THEN
        PRINT *,' MU_RSL ERROR - IER',IER,' on record:'
        PRINT *,' "'//CH66,'"',MAT,MF,MT
        GO TO 901
      END IF
      CALL WRTEXT(LOU,MAT,MF,MT,NS,CH66)
      IF(MAT.GT.0) GO TO 80
      IF(MAT.LT.0) GO TO 900
      REWIND(UNIT=LTM)
      NMAT=NMAT+1
      MAT1=MAT0
      IF(MAT0.EQ.0) GO TO 40
C*
  900 STOP 'MU_RSL Completed'
C*
  901 STOP 'MU_RSL ERROR - EOF before material/reaction found'
C*
  902 STOP 'MU_RSL ERROR - Processing terminated'
C*
  986 FORMAT(A80)
  991 FORMAT(2A40)
  992 FORMAT(4X,I4,3X,I2,3X,I3)
  993 FORMAT(2X,I6,3X,I2,3X,I3)
  994 FORMAT(6F11.0)
  995 FORMAT(BN,I10)
  996 FORMAT(BN,F10.0)
  997 FORMAT(A40,I7)
  998 FORMAT(A40,F12.4)
      END
      SUBROUTINE FNGAUS(NEN,ENR,DXS,FRC,EPS)
C-Title  : Subroutine FNGAUS
C-Purpose: Tabulate normalised Gaussian function at NEN points
C-Description:
C-D  The Gaussian function is defined as:
C-D                 1
C-D  f(x) = ------------------ exp ( -x^2/( 2 Sig^2) )
C-D         Sig * sqrt( 2 Pi )
C-D
C-D  The half-width at half-maximum FRC = +/- Sig * sqrt( 2* ln 2 )
C-D
C-D  Sig = FRC / sqrt( 2* ln 2 )
C-D
C-D  The tolerance EPS defines the range of tabulation. It is defined
C-D  as the fraction of the peak value. By analogy with the derivation
C-D  of the HWHM, the limiting values w are
C-D
C-D  w = +/- Sig * 2 * sqrt (-2 ln EPS )
C-D
C-D  Translation to an arbitrary coordinate system with the peak at u0
C-D  can be done by the transformation:
C-D
C-D  x = ( u - u0 )/u0 --> u = u0 ( 1 + x )
C-D
C-D  and FRC is the fractional distance from u0.
C-D  The Jacobian of the transformation is 1/u0
C-D
C-Ref: http://mathworld.wolfram.com/GaussianFunction.html
C-
      DIMENSION ENR(NEN),DXS(NEN)
C*
      DATA PI/ 3.141592654 /
C*
      TWO = 2
      SIG = FRC / SQRT( 2*LOG(TWO) )
      WDT = SIG * SQRT(-2*LOG(EPS))
      DX  = WDT*2/(NEN-1)
      X   =-WDT
      C   = 1/( SIG * SQRT(2*PI) )
      DO I=1,NEN
        ENR(I)=X
        DXS(I)=C * EXP ( -0.5 * (X/SIG)**2 )
        X     =X + DX
      END DO
      RETURN
      END
      SUBROUTINE RSLBR1(NP,ENX,XSX,NX1,MX1,EN1,XS1,NRS,ENR,RSL
     &                 ,MXW,RWO,IRR)
C-Title  : Subroutine RSLBR1
C-Purpose: Perform resolution broadening of tabulated data
C-Description:
C-D  A function tabulated at NP points is given in XSX(i) with arguments
C-D  ENX(i). The resolution-broadening function is tabulated at NRS
C-D  points in RSL(i) with arguments ENR(i). The resolution function
C-D  is given in relative units, normalised to one and centred at zero.
C-D  A linear transformation is made at every point that is resolution-
C-D  broadened. A scratch array RWO of size MXW is required. The size
C-D  may vary, but it must be several times the sum of (NP+NRS).
C-D
C-D  If IRR=1, the resolution function is given in absolute scale,
C-D  otherwise it is assumed to be given relative to the pivot argument
C-D  value
C-D
C-D  The resolution-broadened function is output on NX1 points in XS1(i)
C-D  with arguments EN1(i). The maximum number of points for the output
C-D  tabulated function is MX1.
C-D
C-D  Procedure
C-D  - The energy grid for the output function is generated such that it
C-D    is dense enough to adequately describe the resolution function.
C-D  - The original function is expanded to the fine mesh by linear
C-D    interpolation.
C-D  - Integrals of the original function are calculated for each fine
C-D    mesh interval.
C-D  - The range of indices on the fine mesh is determined that span
C-D    the range of the resolution function.
C-D  - The resolution function is interpolated to the fine grid within
C-D    the range of the indices.
C-D  - Integrals of the resolution function are calculated, as well as
C-D    the total integral.
C-D  - Loop is set over each fine-mesh interval; the integral of the
C-D    original function is distributed according to the distribution
C-D    of the resolution function integrals in the neighbouring
C-D    intervals within the range of the resolution function.
C-D  - Function values are reconstructed as the average values over
C-D    two two consecutive intervals (calculated from the integrals).
C-D  - A correction is made to every second point to preserve integrals
C-D    over double intervals.
C-D
C-D  No thinning of the data points is performed.
C-
      DIMENSION ENX(NP),XSX(NP),ENR(NRS),RSL(NRS)
     &         ,EN1(MX1),XS1(MX1),RWO(MXW)
C* Tolerance for interpolated cross section values
      EPS=1E-3
      INR=2
C...
      PRINT *,'Points to process',NP
C...
C*
      EXLO=ENX(1)
      EXHI=ENX(NP)
C*
C* Generate the energy grid dense enough for resolution broadening
      IE =0
      ID =1
      EN =ENX(1)
      XN =XSX(1)
      EE =EN
      EN1(ID)=EE
c...
c...  print *,'enr',(enr(j),j=1,10)
C...  print *,'enx',(enx(j),j=1,10)
c...
C*
C* Loop over original function mesh points
  100 IE =IE+1
C*    -- Ee last point in the fine grid
C*    -- Ex current point in the cross section grid
C*    -- En next point in the cross section grid
C*    -- Er next point of the resolution function
      EX =ENX(IE)
      I1 =MIN(IE+1,NP)
      EN0=EN
      XN0=XN
      EN =ENX(I1)
      XN =XSX(I1)
c...
c...  if(ee.lt.500.) then
c...    PRINT *,'ie,ex,ee',ie,ex,ee
c...  else
c...    stop
c...  end if
c...
      DO IR=1,NRS
        IF(IRR.EQ.1) THEN
C*        -- Linearly translated resolution function mesh point
          ER=EX   +ENR(IR)
        ELSE
C*        -- Linearly transformed resolution function mesh point
          ER=EX*(1+ENR(IR))
        END IF
c...
c...    print *,'    ee,ex,er',ee,ex,er
c...
C*      -- Next original function mesh point
c...
C...    print *,'   ir,ex,er,en1',ir,ex,er,en1(id),id
c...
        IF(EE.LT.EX .AND. ER.GT.EX) THEN
C*        -- Insert current point if not present
c...
C...      print *,'    Insert id,ex',id+1,ex
c...
          ID=ID+1
          IF(ID.GT.MX1) STOP 'RSLBR1 ERROR - MX1 limit exceeded'
          EN1(ID)=EX
          EE=EX
        END IF
        IF(ER.GE.EN) THEN
C*        -- Next resol.funct. point exceeds next data point
          EXIT
        END IF
C*      -- Ignore resolution function points below previous point
        IF(ER.LE.EN1(ID)) CYCLE
C*      -- Insert resol.funct. point if less than next point
        ID=ID+1
        IF(ID.GT.MX1) STOP 'RSLBR1 ERROR - MX1 limit exceeded'
        EN1(ID)=ER
        EE=ER
c...
c...    print *,'      Entered Er',id,er,' Interval',en1(id-1),en
c...
      END DO
C*    -- If original points are widely spaced add extra points
C*       stepping in the last interval of the resolution function
  110 IF(IRR.EQ.1) THEN
        DE=ENR(NRS)-ENR(NRS-1)
      ELSE
        DE=EE*(1+ENR(NRS))/(1+ENR(NRS-1))
      END IF
      EF=EE+DE
      IF(EF+DE.LT.EN) THEN
        ID=ID+1
        IF(ID.GT.MX1) THEN
          PRINT *,'Added points exceed limit',MX1,' near E',EN,' eV'
c...      PRINT *,'LO'
c...      PRINT *,(EN1(J),J=1,10)
c...      PRINT *,'HI',MX1
c...      PRINT *,(EN1(J),J=MX1-10,MX1)
          STOP 'RSLBR1 ERROR - MX1 limit exceeded'
        END IF
        EN1(ID)=EF
        EE=EF
C...
c...    Print '(a,2i6,1p,6e11.4)',' Inserting point'
c... &                           ,id,ie,ee,de,en,xn0,xn
C...
        GO TO 110
      END IF
      IF(IE.LT.NP) GO TO 100
c...
c...  if(np.lt.500) stop
c...
C*    -- Last point
      IF(EN1(ID).LT.EXHI) THEN
        ID=ID+1
        IF(ID.GT.MX1) STOP 'RSLBR1 ERROR - MX1 limit exceeded'
        EN1(ID)=EXHI
      END IF
C-F Count total number of points
C...
      print *,'Total No. of fine grid points',ID
C...
      NX1=ID
C-F Interpolate the original function to the fine grid EN1, XS1
      CALL FITGRD(NP,ENX,XSX,NX1,EN1,XS1)
c...
c...  do i=1,20
c...    write(*,'(1p,4e10.3)') enx(i),xsx(i),en1(i),xs1(i)
c...  end do
c...  return
c...
C* Initialise the integrals over the intervals
      LD1=1
      DO I=2,NX1
        RWO(LD1-2+I)=0
      END DO
C-F Distribute the interval integrals according to the resol.funct.
      LRE=LD1+NX1
      LBL=LRE+NRS
C*    -- Loop over all intervals
      JX1=1
      SUMYTGI=0
      DO I=2,NX1
C*      -- Starting energy of the intervals
        EA=EN1(I-1)
        XA=XS1(I-1)
C*      -- Define subinterval step width ED
        IF(IRR.EQ.1) THEN
C*        -- Linearly translated resolution function mesh point
          ED=   ENR(NRS) / NRS
        ELSE
C*        -- Linearly transformed resolution function mesh point
          ED=EA*ENR(NRS) / NRS
        END IF
  120   EB=MIN(EN1(I),EA+ED)
C...    EB=EN1(I)
C*      -- Resolution function pivot point Ec
C...    -- Should be the centroid!!!
C...    EC =...
        EC =(EA+EB)/2
C...
C...    Print *,'Interval',ea,eb,ec
C...
C*      -- Integral of the original function in the subinterval
        XB  =FINTXS(EB,EN1,XS1,NX1,INR,IER)
        YTGI=(EB-EA)*(XB+XA)/2
        SUMYTGI=SUMYTGI+YTGI
C*      -- Define the resolution function energy grid
        DO J=1,NRS
          IF(IRR.EQ.1) THEN
C*          -- Linearly translated resolution function mesh point
            RWO(LRE-1+J)=EC   +ENR(J)
          ELSE
C*          -- Linearly transformed resolution function mesh point
            RWO(LRE-1+J)=EC*(1+ENR(J))
          END IF
        END DO
C*      -- Bounds of the resolution function in energy terms
        ERLO=RWO(LRE)
        ERHI=RWO(LRE-1+NRS)
C*      -- Find bounding indices on the fine-mesh
        ILO=I
        IHI=I
        DO WHILE (ILO.GT.1)
          IF(EN1(ILO).LT.ERLO) EXIT
          ILO=ILO-1
C...
C...      PRINT *,'ILO,E_ILO,ERLO',ILO,EN1(ILO),ERLO
C...
        END DO
        DO WHILE (IHI.LT.NX1)
          IF(EN1(IHI).GT.ERHI) EXIT
          IHI=IHI+1
C...
C...      PRINT *,'IHI,E_IHI,ERHI',IHI,EN1(IHI),ERHI
C...
        END DO
        LRF=LBL
        SS=0
C*      -- Integral of the resolution function on intervals ILO:IHI
        DO J=ILO,IHI-1
          E1A =EN1(J)
          E1B =EN1(J+1)
          CALL YTGEOU(YTGJ,E1A,E1B,NRS,RWO(LRE),RSL,INR)
c...
C...      print *,j,en1(j),en1(j+1),RWO(LRF+J-ILO),RWO(LRF+J+1-ILO),ytgj
c...
C*        -- Save the resol.funct. integral for the interval
          RWO(LRF-ILO+J)=YTGJ
          SS=SS+YTGJ
        END DO
c...
C...    if(nint(EN1(I)).eq.2250000) then
C...      print *,'ss,ytgi,sumytgi',ss,ytgi,sumytgi
C...    end if
c...    stop
c...
C*      -- Distribute the integral
        DO J=ILO,IHI-1
          YTGJ=RWO(LRF-ILO+J)
          RWO(LD1-1+J)=RWO(LD1-1+J) + YTGI*YTGJ/SS
        END DO
        JX1=JX1+1
        IF(JX1.GE.5000) THEN
C*        -- Print progress message every 5000 points
          WRITE(*,'(A,I7,A,I7)')'             Done',I,' of',NX1
          JX1=0
        END IF
C*      -- Done one subinterval, do next step
        IF(EB.LT.EN1(I)) THEN
          EA=EB
          XA=XB
          GO TO 120
        END IF
      END DO
      SUMYTGJ=0
      DO I=2,NX1
        SUMYTGJ =SUMYTGJ+RWO(LD1-2+I)
      END DO
C...
      WRITE(*,'(A,1P,2E10.3,A,0P,F10.4)') ' Integral check'
     &     ,SUMYTGI,SUMYTGJ,' Diff[%]',100*(SUMYTGJ/SUMYTGI-1)
C...
C*
C-F Reconstruct the function from the integrals
c...
      print *,'Reconstruct the cross sections'
c...
      DO I=3,NX1
        XS1(I-1)= RWO(LD1+I-3)/(EN1(I-1)-EN1(I-2))
      END DO
C*    -- Correct the first and last point
      FAV=RWO(LD1)/(EN1(2)-EN1(1))
      XS1(1)=2*FAV - XS1(2)
      FAV=RWO(LD1+NX1-2)/(EN1(NX1)-EN1(NX1-1))
      XS1(NX1)=2*FAV - XS1(NX1-1)
C*    -- Correct every other point to preserve integrals
c...  N1=NX1-1
c...  DO I=2,N1,2
c...    YTGI=RWO(LD1-2+I)+RWO(LD1-1+I)
c...    Y1  =XS1(I-1)
c...    Y3  =XS1(I+1)
c...    X1  =EN1(I-1)
c...    X2  =EN1(I  )
c...    X3  =EN1(I+1)
c...    Y2  =(YTGI*2 -Y1*(X2-X1) - Y3*(X3-X2))/(X3-X1)
c...    XS1(I)=Y2
c...  END DO
      RETURN
      END
      SUBROUTINE RSLBR0(NP,ENX,XSX,NX1,MX1,EN1,XS1,NRS,ENR,RSL,MXW,RWO)
C-Title  : Subroutine RSLBR0
C-Purpose: Perform resolution broadening of tabulated data
C-Description:
C-D  A function tabulated at NP points is given in XSX(i) with arguments
C-D  ENX(i). The resolution-broadening function is tabulated at NRS
C-D  points in RSL(i) with arguments ENR(i). The resolution function
C-D  is given in relative units, normalised to one and centred at zero.
C-D  A linear transformation is made at every point that is resolution-
C-D  broadened. A scratch array RWO of size MXW is required. The size
C-D  may vary, but it must be several times the sum of (NP+NRS).
C-D
C-D  The resolution-broadened function is output on NX1 points in XS1(i)
C-D  with arguments EN1(i). The maximum number of points for the output
C-D  tabulated function is MX1.
C-D
C-D  The energy grid of the resolution-broadened function is the union
C-D  of the input grid and the resolution function grid. No thinning
C-D  of the data points is performed.
C-
      DIMENSION ENX(NP),XSX(NP),ENR(NRS),RSL(NRS)
     &         ,EN1(MX1),XS1(MX1),RWO(MXW)
C* Tolerance for interpolated cross section values
      EPS=1E-3
C...
C...  PRINT *,'Points to process',NP
C...
C*
C* Consider each cross section point in turn
      JX1=0
      NX1=0
      INR=2
      EEX0=ENX(1)
      DER0=(ENR(NRS)-ENR(1))*4/(NRS-1)
C*
C* Loop over all data points of the original function
      DO I=1,NP
        JX1=JX1+1
        IF(JX1.GE.5000) THEN
C*        -- Print progress message every 5000 points
          PRINT *,'             Done',I
          JX1=0
        END IF
   20   EEX =EEX0

C* Interpolate the resolution function to a union grid
C*      -- Find the range of actual energies of the resolution function
        ELO=EEX*(1 + ENR(  1) )
        EHI=EEX*(1 + ENR(NRS) )
        LE1=0
   22   LE1=LE1+1
        IF(ENX(LE1).LT.ELO .AND. LE1.LT.NP) GO TO 22
        NE1=-1
   24   NE1=NE1+1
        IF(ENX(LE1+NE1).LT.EHI .AND. LE1+NE1.LT.NP) GO TO 24
C*      -- Convert resolution function argument to energy
        LE2=1
        DO J=1,NRS
          RWO(LE2-1+J)=EEX*(1 + ENR(J) )
        END DO
C*      -- Make a union grid with the cross sections
        LE3=LE2+NRS
        KX=MXW-LE3
        CALL UNIGRD(NE1,ENX(LE1),NRS,RWO(LE2),NEU,RWO(LE3),KX)
C*      -- Interpolate the resolution function to the union grid
        LX2=LE3+NEU
        CALL FITGRD(NRS,RWO(LE2),RSL,NEU,RWO(LE3),RWO(LX2))
C*      -- Interpolate the cross sections to the union grid
        LX3=LX2+NEU
        CALL FITGRD(NP,ENX,XSX,NEU,RWO(LE3),RWO(LX3))
C*
C*      -- Apply resolution-broadening to this point
        SXS=0
        SRS=0
        EER2=RWO(LE3)
C*      -- Gauss finction with the Jacobian transformation
        RSL2=RWO(LX2)/EER2
C*      -- Interpolate cross section to EER2
        XSX2=RWO(LX3)
        RRX2=RSL2*XSX2
        DO J=1,NEU
          RSL1=RSL2
          EER1=EER2
          RRX1=RRX2
C*        -- Energy, resolution function * Jacobian and cross section
          EER2=RWO(LE3-1+J)
          RSL2=RWO(LX2-1+J)/EER2
          XSX2=RWO(LX3-1+J)
C*        -- Integrand
          RRX2=RSL2*XSX2
          IF(EER1.LT.ENX(1)) THEN
C...
c...        print *,'Skipping point',eer1
C...
            CYCLE
          END IF
          IF(EER2.GT.ENX(NP)) THEN
C...
c...        print *,'Terminate at point',eer2,SRS, ENX(NP)
C...
            EXIT
          END IF
c...
c...      IF(IPR.EQ.1) WRITE(LCU,'(1p,4E11.4)') EER2,RSL2,XSX2,RRX2
c...
C*        -- Integral of the cross sect.*resolution function
          SXS=SXS + (EER2-EER1)*(RRX2+RRX1)/2
C*        -- Integral of the resolution function for normalisation
          SRS=SRS + (EER2-EER1)*(RSL2+RSL1)/2
        END DO
c...
c...    PRINT *,'EEX,SXS,SRS',EEX,SXS,SRS,SXS/SRS ,der0*eex
c...    if(eex.gt.22.68e3) stop
c...    if(eex.gt.22.70e3) stop
c...
        XSX0=SXS/SRS
C* Add point if it differs from interpolated value
        IF(ABS(XSX0-XSX2).GT.ABS(EPS*XSX2)) THEN
          NX1=NX1+1
          IF(NX1.GT.MX1) THEN
            PRINT *,'NX1,MX1',NX1,MX1
            STOP 'RSLBR0 ERROR - MX1 limit exceeded'
          END IF
          EN1(NX1)=EEX
          XS1(NX1)=XSX0
        END IF
C* Check if mesh refinement is needed
        IF(I.LT.NP) THEN
          DER=EEX*DER0
          IF(EEX+2*DER.LT.ENX(I+1)) THEN
            EEX0=EEX+DER
c...
c...        PRINT *,'Insert point',eex0
c...
            GO TO 20
          ELSE
            EEX0=ENX(I+1)
          END IF
        END IF
      END DO
      RETURN
      END
      SUBROUTINE UNIGRD(NEP1,EN1,NEP2,EN2,NEU,EUN,KX)
C-Title  : Subroutine UNIGRID
C-Purpose: Generate union grid from two sets
C-Description:
C-D  Two grids EN1 and EN2 containing NEP1 and NEP2 points respectively
C-D  are merged into EUN with NEU points. Special care is taken to
C-D  retain double points within a grid that allow for function
C-D  discontinuities.
C-
      DIMENSION EN1(NEP1),EN2(NEP2),EUN(KX)
      IF(NEP2.LE.0) GO TO 30
      IF(NEP1.LE.0) GO TO 34
      NEU=0
      J1 =1
      J2 =1
C*
C* Select the grid set for the next point
    8 IF(EN1(J1).GT.EN2(J2)) GO TO 20
C*
C* Add a point from first set
   10 NEU=NEU+1
      IF(NEU.GT.KX) THEN
        PRINT *,'UNIGRD ERROR in grid-1:',NEU,KX
        STOP 'UNIGRD ERROR - KX limit exceeded'
      END IF
      EUN(NEU)=EN1(J1)
C* Test for coincident points in the second grid
      IF(J2.LE.NEP2) THEN
        IF(EN1(J1).EQ.EN2(J2)) J2=J2+1
      END IF
      J1 =J1+1
C* Test for the end of the grid set
      IF(J1.GT.NEP1 .AND. J2.GT.NEP2) GO TO 40
      IF(J1.GT.NEP1) GO TO 20
C* Test for double points in a grid
      IF(EN1(J1).EQ.EUN(NEU)) GO TO 10
      IF(J2.GT.NEP2) GO TO 10
      GO TO 8
C*
C* Add a point from second set
   20 NEU=NEU+1
      IF(NEU.GT.KX) THEN
         PRINT *,'NEU,KX',NEU,KX
         STOP 'UNIGRD ERROR - KX limit exceeded'
      END IF
      EUN(NEU)=EN2(J2)
C* Test for coincident points in the first grid
      IF(J1.LE.NEP1) THEN
        IF(EN1(J1).EQ.EN2(J2)) J1=J1+1
      END IF
      J2 =J2+1
C* Test for the end of the grid set
      IF(J1.GT.NEP1 .AND. J2.GT.NEP2) GO TO 40
      IF(J2.GT.NEP2) GO TO 10
C* Test for double points in a grid
      IF(EN2(J2).EQ.EUN(NEU)) GO TO 20
      IF(J1.GT.NEP1) GO TO 20
      GO TO 8
C*
C* Add all points from the first set
   30 NEU=NEP1
      DO 32 J=1,NEU
      EUN(J)=EN1(J)
   32 CONTINUE
      GO TO 40
C*
C* Add all points from the second set
   34 NEU=NEP2
      DO 36 J=1,NEU
      EUN(J)=EN2(J)
   36 CONTINUE
C*
C* All points processed
   40 CONTINUE
      RETURN
      END
      SUBROUTINE FITGRD(NEP1,EN1,XS1,NEP2,EN2,XS2)
C-Title  : Subroutine FITGRD
C-Purpose: Interpolate a tabulated function to a given grid
C-Description:
C-D Function XS1 at NEP1 argument values in EN1 is interpolated to
C-D NEP2 values XS2 corresponding to argument values in EN2
      DIMENSION EN1(NEP1),XS1(NEP1),EN2(NEP2),XS2(NEP2)
C*
      IF(NEP1.LE.0) THEN
        DO J=1,NEP2
          XS2(J)=0
        END DO
        RETURN
      END IF
      J2=1
      J1=1
C* Test terminal condition - last point in the given grid
   10 IF(J2.GT.NEP2) RETURN
C*
C* Select case for the next point
      IF     (EN1(J1).EQ.EN2(J2)) THEN
C* Case: copy coincident points
        XS2(J2)=XS1(J1)
        IF(J1.LT.NEP1) J1=J1+1
        J2=J2+1
        GO TO 10
      ELSE IF(EN1(J1).GT.EN2(J2)) THEN
C* Case: insert point by interpolation or zero below threshold
        JJ=J1-1
        XX=0
        IF(JJ.GT.0) XX= XS1(JJ)
     1    + (XS1(J1)-XS1(JJ))*(EN2(J2)-EN1(JJ))/(EN1(J1)-EN1(JJ))
        XS2(J2)= XX
        J2=J2+1
        GO TO 10
      ELSE
        IF(J1.LT.NEP1) THEN
C* Case: skip points in original grid
          J1=J1+1
        ELSE
C* Case: ZERO beyond last point in original grid
          XS2(J2)=0
          J2=J2+1
        END IF
        GO TO 10
      END IF
C*
      END
      FUNCTION FINTXS(EIN,EN,XS,NP,INR,IER)
C-Title  : Function FINTXS
C-Purpose: Interpolate the cross section table to EIN
C-Description:
C-D  EIN  Incident energy
C-D  EN   Array of energies
C-D  XS   Array of corresponding cross sections
C-D  NP   Number of points in the cross section array
C-D  INR  Interpolation law (INR=1,2 allowed)
C-D  IER  = 0 - normal termination
C-D        11 - requested point outside interpolation range
      DIMENSION EN(NP),XS(NP)
      IER=0
      IF     (EIN.LT.EN(1) .OR. NP.LT.2) THEN
        FINTXS=XS(1)
        RETURN
      ELSE IF(EIN.GT.EN(NP)) THEN
        FINTXS=XS(NP)
        RETURN
      END IF
      DO I=2,NP
        I2=I
        IF(EN(I).GE.EIN) GO TO 22
      END DO
      IER=11
   22 I1=I2-1
      IF(INR.EQ.2) THEN
        FF=XS(I1)+(XS(I2)-XS(I1))*(EIN-EN(I1))/(EN(I2)-EN(I1))
      ELSE
        FF=XS(I1)
      END IF
      FINTXS=FF
      RETURN
      END
      SUBROUTINE YTGEOU(PMU,EA,EB,NEP,EOU,DXS,INR)
C-Title  : Subroutine YTGEOU
C-Purpose: Integrate a pointwise tabulated function over EA - EB
C-Author : A.Trkov, IAEA, Vienna, Austria
C-Version:
C-V  05/01 Fix interpolation at energy boundaries
C* WARNING: Only lin-lin or histogram interpolation allowed
      DIMENSION  EOU(NEP),DXS(NEP)
C*
      PMU=0
      E2=EOU(1)
      F2=DXS(1)
      DO 20 I=2,NEP
      E1=E2
      F1=F2
      E2=EOU(I)
      F2=DXS(I)
C* Treat lower energy bound
      IF(E2.LT.EA) GO TO 20
      IF(E1.LT.EA) THEN
        IF(INR.EQ.2) F1=F1+(F2-F1)*(EA-E1)/(E2-E1)
        E1=EA
      END IF
      IF(E1.GE.EB) RETURN
C* Treat upper energy bound
      IF(E2.GT.EB) THEN
        IF(INR.LT.2) THEN
          F2=F1
        ELSE
          F2=F1+(F2-F1)*(EB-E1)/(E2-E1)
        END IF
        E2=EB
      END IF
C* Define average function value over the interval
      FF=0.5*(F2+F1)
C* Add interval contribution to the integral
      PMU=PMU+FF*(E2-E1)
   20 CONTINUE
C*
      RETURN
      END
      SUBROUTINE RDTEXT(LEF,MAT,MF,MT,REC,IER)
C-Title  : RDTEXT Subroutine
C-Purpose: Read a text record to an ENDF file
      CHARACTER*66  REC
      READ (LEF,40,END=81,ERR=82) REC,MAT,MF,MT
      IER=0
      RETURN
   81 IER=1
      RETURN
   82 IER=2
      RETURN
   40 FORMAT(A66,I4,I2,I3,I5)
      END
      SUBROUTINE RDHEAD(LEF,MAT,MF,MT,C1,C2,L1,L2,N1,N2,IER)
C-Title  : Subroutine RDHEAD
C-Purpose: Read an ENDF HEAD record
C-Description:
C-D  The HEAD record of an ENDF file is read. The following error
C-D  conditions are trapped by setting the IER flag:
C-D    IER = 0  Normal termination
C-D          1  End-of-file
C-D          2  Read error
C-
      READ (LEF,92) C1,C2,L1,L2,N1,N2,MAT,MF,MT
      RETURN
   92 FORMAT(2F11.0,4I11.0,I4,I2,I3,I5)
      END
      SUBROUTINE RDTAB1(LEF,C1,C2,L1,L2,N1,N2,NBT,INR,EN,XS,NMX,IER)
C-Title  : Subroutine RDTAB1
C-Purpose: Read an ENDF TAB1 record
C-Description:
C-D  The TAB1 record of an ENDF-formatted file is read.
C-D  Error condition:
C-D    IER=1  End-of-file
C-D        2  Read error
C-D       -8  WARNING - Numerical underflow (<E-36)
C-D        8  WARNING - Numerical overflow  (>E+36)
C-D        9  WARNING - Available field length exceeded, NMX entries read.
C-
      DOUBLE PRECISION EE(3),XX(3)
      DIMENSION    NBT(*),INR(*)
      DIMENSION    EN(NMX), XS(NMX)
C*
      IER=0
      READ (LEF,902,END=100,ERR=200) C1,C2,L1,L2,N1,N2
      READ (LEF,903,END=100,ERR=200) (NBT(J),INR(J),J=1,N1)
      JP=N2
      IF(N2.GT.NMX) THEN
        JP=NMX
        IER=9
      END IF
      JR=(JP+2)/3
      J=0
      DO K=1,JR
        READ(LEF,904,END=100,ERR=200) (EE(M),XX(M),M=1,3)
        DO M=1,3
          J=J+1
          IF(J.LE.JP) THEN
            IF(ABS(XX(M)).LT.1E-36) THEN
              XX(M)=0
C...          IER=-8
            ELSE IF(ABS(XX(M)).GT.1.E36) THEN
              XX(M)=1E36
              IER=8
            END IF
            EN(J)=EE(M)
            XS(J)=XX(M)
          END IF
        END DO
      END DO
      RETURN
  100 IER=1
      RETURN
  200 IER=2
      RETURN
C*
  902 FORMAT(2F11.0,4I11)
  903 FORMAT(6I11)
  904 FORMAT(6F11.0)
      END
      SUBROUTINE RDTAB2(LEF,C1,C2,L1,L2,N1,N2,NBT,INR,IER)
C-Title  : Subroutine RDTAB2
C-Purpose: Read an ENDF TAB2 record
C-D  Error condition:
C-D    IER=1  End-of-file
C-D        2  Read error
      DIMENSION    NBT(*),INR(*)
C*
      READ (LEF,902,END=100,ERR=200) C1,C2,L1,L2,N1,N2
      READ (LEF,903,END=100,ERR=200) (NBT(J),INR(J),J=1,N1)
      RETURN
  100 IER=1
      RETURN
  200 IER=2
      RETURN
C*
  902 FORMAT(2F11.0,4I11)
  903 FORMAT(6I11)
      END
      SUBROUTINE RDLIST(LEF,C1,C2,L1,L2,N1,N2,VK,MVK,IER)
C-Title  : Subroutine RDLIST
C-Purpose: Read an ENDF LIST record
      DOUBLE PRECISION RUFL,RR(6)
      DIMENSION    VK(1)
C*
      READ (LEF,902) C1,C2,L1,L2,N1,N2
      IF(N1+5.GT.MVK) THEN
        IER=-1
        RETURN
      END IF
      IF(N1.EQ.0) RETURN
C* Read the LIST2 entries, watch for underflow
      NUFL=0
      RUFL=1
      DO J=1,N1,6
        READ (LEF,903) (RR(K),K=1,6)
        DO K=1,6
          IF(RR(K).NE.0 .AND. ABS(RR(K)).LT.1.E-30) THEN
            NUFL=NUFL+1
            IF(ABS(RR(K)).LT.ABS(RUFL)) RUFL=RR(K)
          END IF
          VK(J-1+K)=RR(K)
        END DO
      END DO
      IF(NUFL.GT.0) THEN
        PRINT *,' RDLIST WARNING - Underflow conditions',NUFL
        PRINT *,'                        Minimum number',RUFL
      END IF
      RETURN
C*
  902 FORMAT(2F11.0,4I11)
  903 FORMAT(6F11.0)
      END
      SUBROUTINE WRTEXT(LIB,MAT,MF,MT,NS,REC)
C-Title  : WRTEXT Subroutine
C-Purpose: Write a text record to an ENDF file
      CHARACTER*66  REC
   12 NS=NS+1
      IF(NS.GT.99999) NS=0
      IF(MT.EQ.0)     NS=99999
      IF(MF.EQ.0)     NS=0
      WRITE(LIB,40) REC,MAT,MF,MT,NS
      IF(MT.EQ.0)     NS=0
      RETURN
   40 FORMAT(A66,I4,I2,I3,I5)
      END
      SUBROUTINE WRCONT(LIB,MAT,MF,MT,NS,C1,C2,L1,L2,N1,N2)
C-Title  : WRCONT Subroutine
C-Purpose: Write a CONT record to an ENDF file
      CHARACTER*11  BLN,REC(6)
      DATA BLN/'           '/
      DO 10 I=1,6
      REC(I)=BLN
   10 CONTINUE
      IF( (C1.EQ.0. .AND. C2.EQ.0.) .AND.
     1    (L1.EQ.0  .AND. L2.EQ.0 ) .AND.
     2    (N1.EQ.0  .AND. N2.EQ.0 ) ) GO TO 12
      CALL CHENDF(C1,REC(1))
      CALL CHENDF(C2,REC(2))
      WRITE(REC(3),20) L1
      WRITE(REC(4),20) L2
      WRITE(REC(5),20) N1
      WRITE(REC(6),20) N2
   12 NS=NS+1
      IF(NS.GT.99999) NS=0
      IF(MT.EQ.0)     NS=99999
      IF(MF.EQ.0)     NS=0
      WRITE(LIB,40) (REC(J),J=1,6),MAT,MF,MT,NS
      IF(MT.EQ.0)     NS=0
      RETURN
   20 FORMAT(I11)
   40 FORMAT(6A11,I4,I2,I3,I5)
      END
      SUBROUTINE WRTAB1(LIB,MAT,MF,MT,NS,C1,C2,L1,L2
     1                 ,NR,NP,NBT,INR,X,Y)
C-Title  : WRTAB1 Subroutine
C-Purpose: Write a TAB1 record to an ENDF file
      CHARACTER*11  BLN,REC(6)
      DIMENSION     NBT(NR),INR(NR),X(NP),Y(NP)
      DATA BLN/'           '/
C* First line of the TAB1 record
      CALL CHENDF(C1,REC(1))
      CALL CHENDF(C2,REC(2))
      WRITE(REC(3),42) L1
      WRITE(REC(4),42) L2
      WRITE(REC(5),42) NR
      WRITE(REC(6),42) NP
      NS=NS+1
      IF(NS.GT.99999) NS=0
      WRITE(LIB,40) (REC(J),J=1,6),MAT,MF,MT,NS
C* Write interpolation data
      N =0
   20 I =0
   22 REC(I+1)=BLN
      REC(I+2)=BLN
      IF(N.GE.NR) GO TO 24
      N =N+1
      WRITE(REC(I+1),42) NBT(N)
      WRITE(REC(I+2),42) INR(N)
   24 I =I +2
      IF(I.LT.6) GO TO 22
      NS=NS+1
      IF(NS.GT.99999) NS=0
      WRITE(LIB,40) (REC(J),J=1,6),MAT,MF,MT,NS
      IF(N.LT.NR) GO TO 20
C* Loop for all argument&function pairs
      N =0
   30 I =0
   32 REC(I+1)=BLN
      REC(I+2)=BLN
      IF(N.GE.NP) GO TO 34
      N =N+1
      CALL CHENDF(X(N),REC(I+1))
      CALL CHENDF(Y(N),REC(I+2))
   34 I =I+2
      IF(I.LT.6) GO TO 32
      NS=NS+1
      IF(NS.GT.99999) NS=0
      WRITE(LIB,40) (REC(J),J=1,6),MAT,MF,MT,NS
      IF(N.LT.NP) GO TO 30
      RETURN
   40 FORMAT(6A11,I4,I2,I3,I5)
   42 FORMAT(I11)
      END
      SUBROUTINE WRTAB2(LIB,MAT,MF,MT,NS,C1,C2,L1,L2
     1                 ,NR,NZ,NBT,INR)
C-Title  : WRTAB2 Subroutine
C-Purpose: Write a TAB2 record to an ENDF file
      CHARACTER*11  BLN,REC(6)
      DIMENSION     NBT(NR),INR(NR)
      DATA BLN/'           '/
C* First line of the TAB2 record
      CALL CHENDF(C1,REC(1))
      CALL CHENDF(C2,REC(2))
      WRITE(REC(3),42) L1
      WRITE(REC(4),42) L2
      WRITE(REC(5),42) NR
      WRITE(REC(6),42) NZ
      NS=NS+1
      IF(NS.GT.99999) NS=0
      WRITE(LIB,40) (REC(J),J=1,6),MAT,MF,MT,NS
C* Write interpolation data
      N =0
   20 I =0
   22 REC(I+1)=BLN
      REC(I+2)=BLN
      IF(N.GE.NR) GO TO 24
      N =N+1
      WRITE(REC(I+1),42) NBT(N)
      WRITE(REC(I+2),42) INR(N)
   24 I =I +2
      IF(I.LT.6) GO TO 22
      NS=NS+1
      IF(NS.GT.99999) NS=0
      WRITE(LIB,40) (REC(J),J=1,6),MAT,MF,MT,NS
      IF(N.LT.NR) GO TO 20
      RETURN
   40 FORMAT(6A11,I4,I2,I3,I5)
   42 FORMAT(I11)
      END
      SUBROUTINE WRLIST(LIB,MAT,MF,MT,NS,C1,C2,L1,L2,NPL,N2,BN)
C-Title  : WRLIST Subroutine
C-Purpose: Write a LIST record to an ENDF file
      CHARACTER*11  BLN,REC(6)
      DIMENSION     BN(*)
      DATA BLN/'           '/
C* First line of the TAB2 record
      CALL CHENDF(C1,REC(1))
      CALL CHENDF(C2,REC(2))
      WRITE(REC(3),42) L1
      WRITE(REC(4),42) L2
      WRITE(REC(5),42) NPL
      WRITE(REC(6),42) N2
      NS=NS+1
      WRITE(LIB,40) (REC(J),J=1,6),MAT,MF,MT,NS
      IF(NPL.EQ.0) RETURN
C* Write data
      N =0
   20 I =0
   22 REC(I+1)=BLN
      IF(N.GE.NPL) GO TO 24
      N =N+1
      CALL CHENDF(BN(N),REC(I+1))
   24 I =I +1
      IF(I.LT.6) GO TO 22
      NS=NS+1
      WRITE(LIB,40) (REC(J),J=1,6),MAT,MF,MT,NS
      IF(N.LT.NPL) GO TO 20
      RETURN
   40 FORMAT(6A11,I4,I2,I3,I5)
   42 FORMAT(I11)
      END
      SUBROUTINE CHENDF(FF,CH)
C-Title  : CHENDF Subroutine
C-Purpose: Pack value into 11-character string
      CHARACTER*1  SN
      CHARACTER*11 CH
      CH=' 0.00000+00'
      FA=ABS(FF)
      IA=0
C* Trap unreasonably large values, print as "9.99999+99"
      IF(FA.GT.1E30) THEN
        CH=' 9.99999+99'
        RETURN
      END IF
C* Check for small values, print as zero
   20 IF(FA.LT.1.0E-30 ) RETURN
C* Condition mantissa of small numnbers
      IF(FA.LT.9.999950) GO TO 40
      FA=FA/10
      IA=IA+1
      GO TO 20
C* Condition mantissa of large numnbers
   40 IF(FA.GE.0.999995) GO TO 50
      FA=FA*10
      IA=IA-1
      GO TO 40
C* Sign of the exponent
   50 SN='+'
      IF(IA.LT.0) THEN
        SN='-'
        IA=-IA
      END IF
C* Sign of the mantissa
      IF(FF.LT.0) FA=-FA
C* Write character fiels
      IF(IA.GE.10) THEN
        WRITE(CH,80) FA,SN,IA
      ELSE
        WRITE(CH,81) FA,SN,IA
      END IF
      RETURN
   80 FORMAT(F8.5,A1,I2.2)
   81 FORMAT(F9.6,A1,I1)
      END
