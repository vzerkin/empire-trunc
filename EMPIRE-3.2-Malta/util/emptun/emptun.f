      PROGRAM EMPTUN
C-Title  : EMPTUN Program
C-Purpose: Set energy grid and tuning parameters in EMPIRE input
C-Author : A. Trkov, Jozef Stefan Institute, Ljubljana, Slovenia
C-Version: April 2013
C-M  
C-M  Manual for Program EMPTUN
C-M  =========================
C-M  In case that EMPIRE is to follow a detailed structure in the
C-M  cross sections based on experimental data, the need may arise
C-M  to introduce tuning parameters on a relatively dense energy
C-M  mesh. It is assumed that the tuning factors FUSRED, ELARED
C-M  and CELRED are given in an external data file on a fixed
C-M  energy grid. The EMPIRE input file is scanned and the tuning
C-M  factors are introduced on the given energy mesh, leaving the
C-M  rest of the file unchanged.
C-M  
C-M  Instructions
C-M  The following input parameters are entered from input:
C-M   - FLIN  Source tuning factor filename. The tuning factors
C-M           are tabulated in PLOTTAB "curves" format. The
C-M           header is the tuning factor name, followed by
C-M           pairs of numbers:
C-M             * Energy (in eV !!! because they are usually
C-M               calculated from cross sections tabulated in eV)
C-M             * Tuning factor value.
C-M           The values are given in 11-column format.
C-M           The tuning factors FUSRED, ELARED and CELRED are
C-M           given one after the other, separated by a blank
C-M           record. They MUST be given on the same energy grid.
C-M           These are followed by the fusion cross section
C-M           (identified by the header "Sig_f") and the 
C-M           compound-elastic cross section (identified by 
C-M           the header "Sig_CE") from the EMPIRE reference
C-M           calculation. Definitions:
C-M             FUSRED factors account for the change from the
C-M                    reference EMPIRE calculation to the desired
C-M                    fusion cross section value (name FUSRED2).
C-M             ELARED factors definition is as for FUSRED,
C-M                    referring to the shape-elastic cross sections.
C-M                    (name ELARED2)
C-M             CELRED factors determine the fraction of compound-
C-M                    elastic cross setion in the desired value
C-M                    of the fusion cross section (name CELDSH2).
C-M             Sig_f  cross sections represent the EMPIRE nominal
C-M                    calculation of the fusion cross section
C-M                   (name SIGFUS1).
C-M             Sig_CE cross sections represent the EMPIRE nominal
C-M                    calculation of the compound-elastic cross
C-M                    section (name SIGCEL1)
C-M   - FLEI  Source EMPIRE input filename (to be modified)
C-M   - FLOU  Updated EMPIRE input.
C-M
C-
      PARAMETER    (MXEN=200,MXCOM=10)
      CHARACTER*6   LABEL(5)
      CHARACTER*40  BLNK
      CHARACTER*80  FLNM,FLIN,FLEI,FLOU
      CHARACTER*120 REC,RECM(MXCOM)
     &             ,HTOTRED,HFUSRED,HFCCRED,HFCORED,HELARED,HCELRED
C*
      DIMENSION    ENC(MXEN),PRC(MXEN,5)
C* Data set labels and flags
      DIMENSION    KFLG(5)
      DATA KFLG/ 0, 0, 0, 0, 0 /
      DATA LABEL/'FUSRED','ELARED','CELRED','Sig_f ','Sig_CE' /
C* Filenames and logical file units
      DATA LIN,LEI,LOU,LKB,LTT
     &    /  1,  2,  3,  5,  6 /
      DATA BLNK/'                                        '/
     1    ,FLIN/'inp.dat'/
     2    ,FLEI/'emp.inp'/
     3    ,FLOU/'emp.out'/
C*
      EPS =1.E-5
C* Define input parameters - Write banner to terminal
      WRITE(LTT,901) ' '
      WRITE(LTT,901) ' EMPTUN - Set EMPIRE tuning parameters  '
      WRITE(LTT,901) ' =====================================  '
      WRITE(LTT,901) ' '
C* Define the source files
   11 WRITE(LTT,901) ' Default source tuning fact. filename : ',FLIN
      WRITE(LTT,901) '$          Enter new name to redefine : '
      READ (LKB,900) FLNM
      IF(FLNM(1:40).NE.BLNK) FLIN=FLNM
      OPEN(UNIT=LIN,FILE=FLIN,STATUS='OLD',ERR=11)
   13 WRITE(LTT,901) ' Default source EMPIRE input filename : ',FLEI
      WRITE(LTT,901) '$          Enter new name to redefine : '
      READ (LKB,900) FLNM
      IF(FLNM(1:40).NE.BLNK) FLEI=FLNM
      OPEN(UNIT=LEI,FILE=FLEI,STATUS='OLD',ERR=13)
C* Define the output file
   14 WRITE(LTT,901) ' Default updated EMPIRE input         : ',FLOU
      WRITE(LTT,901) '$          Enter new name to redefine : '
      READ (LKB,900) FLNM
      IF(FLNM(1:40).NE.BLNK) FLOU=FLNM
      OPEN (UNIT=LOU,FILE=FLOU,STATUS='UNKNOWN')
C*
      WRITE(LTT,901) ' '
      WRITE(LTT,901) ' Source tuning factor filename        : ',FLIN
      WRITE(LTT,901) ' Source EMPIRE input filename         : ',FLEI
      WRITE(LTT,901) ' Updated EMPIRE input                 : ',FLOU
      WRITE(LTT,901) ' '
C*
C* Start reading the desired tuning factors
      KC =0
      MC =0
      NEN=0
      HTOTRED=BLNK//BLNK//BLNK
      HFUSRED=BLNK//BLNK//BLNK
      HFCCRED=BLNK//BLNK//BLNK
      HFCORED=BLNK//BLNK//BLNK
      HELARED=BLNK//BLNK//BLNK
      HCELRED=BLNK//BLNK//BLNK
  100 READ (LIN,901,END=140) FLNM
C* Check if all data types are present
      LC=0
      DO K=1,5
        IF(FLNM(1:6).EQ.LABEL(K)) THEN
          LC=K
          KC=KC+1
          KFLG(K)=1
        END IF
      END DO
      IF(LC.EQ.0) THEN
        WRITE(LTT,*) 'ERROR - Invalid tuning parameter header ',FLNM
        GO TO 802
      END IF
C*
      MC =MC+1
      JEN=0
  120 READ (LIN,901) FLNM
      IF(FLNM(1:40).NE.BLNK) THEN
        JEN=JEN+1
        READ (FLNM,*) EE,FF
        EE=EE/1000000
C*      Check if the energy mesh is consistent
        IF(MC.GT.1) THEN
          IF(JEN.GT.NEN) THEN
            WRITE(LTT,*) 'WARNING - Excess E-mesh for ',LABEL(LC)
     &                  ,' :  ',FLNM(1:40)
            GO TO 120
          END IF
          IF(ABS(EE-ENC(JEN)).GT.EPS*ENC(JEN)) THEN
            WRITE(LTT,*) 'ERROR - Iconsistent E-mesh for ',LABEL(LC)
     &                  ,' Expected',ENC(JEN),' Found',EE
            GO TO 802
          END IF
        END IF
        ENC(JEN)=EE
        PRC(JEN,LC)=FF
        GO TO 120
      END IF
      IF(MC.EQ.1) NEN=JEN
      GO TO 100
C*
C* Tuning parameters read
  140 DO K=1,5
        IF(KFLG(K).NE.1) THEN
          WRITE(LTT,'(A)') ' EMPTUN ERROR - Missing tuning parameter '
     &                    ,LABEL(K)
          GO TO 802
        END IF
      END DO
      WRITE(LTT,'(A,2F8.4,A,I4,A)')
     &           ' Tuning parameters between energies (MeV)'
     &            ,ENC(1),ENC(NEN),' at',NEN,' points'
      WRITE(LTT,901) ' '
C*
C* Tuning parametersfile processed - read EMPIRE input
      EN0    =0
      TOTRED1=1
      FUSRED1=1
      ELARED1=1
      CELRED1=1
      EN1    =0
      TOTREDX=1
      FUSREDX=1
      ELAREDX=1
      CELREDX=1
      JEN    =1
      ICOM   =0
C* Start reading the main EMPIRE input block
  200 READ (LEI,902) REC
C*    -- Check for tuning factor definitions in the main block
      IF     (REC(1:6).EQ.'TOTRED') THEN
        TOTRED1=TOTREDX
        READ (REC(7:120),*) TOTREDX
        HTOTRED(1:20)='$TOTRED'//REC(7:19)
      ELSE IF(REC(1:6).EQ.'FUSRED') THEN
        FUSRED1=FUSREDX
        READ (REC(7:120),*) FUSREDX
        HFUSRED(1:20)='$FUSRED'//REC(7:19)
      ELSE IF(REC(1:6).EQ.'FCCRED') THEN
        HFCCRED(1:20)='$FCCRED'//REC(7:19)
      ELSE IF(REC(1:6).EQ.'FCORED') THEN
        HFCORED(1:20)='$FCORED'//REC(7:19)
      ELSE IF(REC(1:6).EQ.'ELARED') THEN
        ELARED1=ELAREDX
        READ (REC(7:120),*) ELAREDX
        HELARED(1:20)='$ELARED'//REC(7:19)
      ELSE IF(REC(1:6).EQ.'CELRED') THEN
        CELRED1=CELREDX
        READ (REC(7:120),*) CELREDX
        HCELRED(1:20)='$CELRED'//REC(7:19)
C*    -- Check for the start of the energy-dependent block
      ELSE IF(REC(1:6).EQ.'GO    ') THEN
        WRITE(LOU,902) REC
        GO TO 220
      END IF
C*    -- Copy all records in the main block
      WRITE(LOU,902) REC
      GO TO 200
C*
C* Begin processing the energy-dependent block
  220 READ (LEI,902) REC
      IF(REC(1:1).EQ.'*') THEN
C*      --Copy comments
        WRITE(LOU,902) REC
        GO TO 220
      END IF
      IF(REC(1:1).NE.'$') THEN
C*      -- Process the energy
        READ (REC,*) EE
C*        -- Update the tuning factors from main input to current value
        TOTRED1=TOTREDX
        FUSRED1=FUSREDX
        ELARED1=ELAREDX
        CELRED1=CELREDX
C...
C...    print *,'read energy',ee,enc(1),enc(nen),fusRED1
C...
        IF(EE.LT.0) THEN
C*        -- Last energy flag - Copy the rest of the file to output
  240     WRITE(LOU,902) REC
          READ (LEI,902,END=800) REC
          GO TO 240
        ELSE
C*        -- Check tuning factors for the current energy
C...      IF(EE.LT.ENC(1) .OR. EE.GE.ENC(NEN)) THEN
C...      IF(EE.LT.ENC(1) .OR. JEN.GT.NEN) THEN
  242     KEN=MIN(JEN,NEN)
c...
c...      if(jen.lt.nen) print *,'jen,icom,ee,enc',jen,icom,ee,enc(jen)
c...
          IF((EE.LT.ENC(1) .OR. JEN.GT.NEN) .OR.
     &       (EE.LT.ENC(KEN)                ) ) THEN
C... &       (EE.LT.ENC(KEN) .AND. ICOM.EQ.1) ) THEN
C*          -- Copy any remaining command and energies outside
C*             the adjustment range
            DO I=1,ICOM
              WRITE(LOU,902) RECM(I)
            END DO
            WRITE(LOU,902) REC
          ELSE
C*          -- Insert energies and tuning factors from the file
            DO WHILE(ENC(JEN).LE.EE)
              REC=BLNK//BLNK//BLNK
C...
C...          PRINT *,ENC(JEN),EE,FUSRED1,PRC(JEN,1),PRC(JEN,1)*FUSRED1
C...
C*
C*            -- FUSRED=FCCRED=FCORED for consistency with TOTRED
              REC(1:7)='$FUSRED'
              FUSRED2=PRC(JEN,1)
              FUSRED =FUSRED2*FUSRED1*TOTRED1
              WRITE(REC(8:17),910) FUSRED
              WRITE(LOU,902) REC
              REC(1:7)='$FCCRED'
              WRITE(LOU,902) REC
              REC(1:7)='$FCORED'
              WRITE(LOU,902) REC
C*
C*            -- ELARED
              REC(1:7)='$ELARED'
              ELARED2=PRC(JEN,2)
              ELARED =ELARED2*ELARED1*TOTRED1
              WRITE(REC(8:17),910) ELARED
              WRITE(LOU,902) REC
C*
C*            -- CELRED
              REC(1:7)='$CELRED'
C... This is wrong!!!
C...          CELRED=PRC(JEN,3)*CELRED1
C...
              CELDSH2=PRC(JEN,3)
              SIGFUS1=PRC(JEN,4)
              SIGCEL1=PRC(JEN,5)
              CELDSH1=SIGCEL1/SIGFUS1
C*
              SIGFUS0=SIGFUS1/(FUSRED1*TOTRED1)
              SIGCEL0=CELDSH1*SIGFUS0/(CELRED1+CELDSH1-CELRED1*CELDSH1)
              CELRED =(SIGFUS0/SIGCEL0-1)*CELDSH2/(1-CELDSH2)
              WRITE(REC(8:17),910) CELRED
              WRITE(LOU,902) REC
C*
C*            -- Copy any remaining commands
              IF(ABS(ENC(JEN)-EE).LT.EPS*EE) THEN
                DO I=1,ICOM
                  WRITE(LOU,902) RECM(I)
                END DO
                ICOM=0
              END IF
C*
C*            -- Incident energy
              REC=BLNK//BLNK//BLNK
              WRITE(REC(1:8),908) ENC(JEN)
              WRITE(LOU,902) REC
              JEN=JEN+1
              IF(JEN.GT.NEN) EXIT
              IF(ENC(JEN).GT.EE .AND. ENC(JEN-1).LT.EE) THEN
                WRITE(REC(1:8),908) EE
                GO TO 242
              END IF
            END DO
C*          -- All additional tuning factors and energies entered
C*          -- Check for the last-defined tuning factor in originl input
            IF(EE.GT.ENC(NEN)) THEN
              IF(HFUSRED(1:40).EQ.BLNK) HFUSRED(1:12)='$FUSRED  1.0'
              IF(HFCCRED(1:40).EQ.BLNK) HFCCRED(1:12)='$FCCRED  1.0'
              IF(HFCORED(1:40).EQ.BLNK) HFCORED(1:12)='$FCORED  1.0'
              IF(HELARED(1:40).EQ.BLNK) HELARED(1:12)='$ELARED  1.0'
              IF(HCELRED(1:40).EQ.BLNK) HCELRED(1:12)='$CELRED  1.0'
              IF(HTOTRED(1:40).NE.BLNK) WRITE(LOU,902) HTOTRED
              WRITE(LOU,902) HFUSRED
              WRITE(LOU,902) HFCCRED
              WRITE(LOU,902) HFCORED
              WRITE(LOU,902) HELARED
              WRITE(LOU,902) HCELRED
C*            -- Incident energy
              REC=BLNK//BLNK//BLNK
              WRITE(REC(1:8),908) EE
              WRITE(LOU,902) REC
            END IF
          END IF
          ICOM=0
          GO TO 220
        END IF
      ELSE
C*
C* Process an energy-dependent command
        IF     (REC(1:7).EQ.'$TOTRED') THEN
          HTOTRED=REC
          TOTRED1=TOTREDX
          READ (REC(8:120),*) TOTREDX
        ELSE IF(REC(1:7).EQ.'$FUSRED') THEN
          HFUSRED=REC
          FUSRED1=FUSREDX
          READ (REC(8:120),*) FUSREDX
C...
C...      PRINT *,'Redefine FUSRED',fusREDX
C...
        ELSE IF(REC(1:7).EQ.'$ELARED') THEN
          HELARED=REC
          ELARED1=ELAREDX
          READ (REC(8:120),*) ELAREDX
        ELSE IF(REC(1:7).EQ.'$CELRED') THEN
          HCELRED=REC
          CELRED1=CELREDX
          READ (REC(8:120),*) CELREDX
        END IF
C*      -- Copy all commands outside the adjustment range
C*         and commands other than the tuning parameters
        IF((EE.LT.ENC(1) .OR. EE.GT.ENC(NEN)) .OR.
     &     (REC(1:7).NE.'$TOTRED' .AND.
     &      REC(1:7).NE.'$FUSRED' .AND.
     &      REC(1:7).NE.'$ELARED' .AND.
     &      REC(1:7).NE.'$CELRED' .AND.
     &      REC(1:7).NE.'$FCCRED' .AND.
     &      REC(1:7).NE.'$FCORED')           ) THEN
C*        -- Save the additional command to print before energy
          ICOM=ICOM+1
          IF(ICOM.GT.MXCOM) STOP 'EMPTUN ERROR - MXCOM limit exceeded'
          RECM(ICOM)=REC
          GO TO 220
        END IF
      END IF
      GO TO 220
C* End of file processing
  800 STOP 'EMPTUN Completed'
C*
  802 STOP 'EMPTUN ERROR - Processing terminated'
C*
  900 FORMAT( A80)
  901 FORMAT(2A40)
  902 FORMAT(A120)
  908 FORMAT( F8.4)
  910 FORMAT(F10.4)
      END
