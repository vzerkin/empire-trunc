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
C-M  and CELCOR are given in an external data file on a fixed
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
C-M             * Tuning factor value
C-M           The values are given in 11-column format.
C-M           The tuning factors FUSRED, ELARED and CELCOR are
C-M           given one after the other, separated by a blank
C-M           record. They MUST be given on the same energy grid.
C-M   - FLEI  Source EMPIRE input filename (to be modified)
C-M   - FLOU  Updated EMPIRE input.
C-M
C-
      PARAMETER    (MXEN=200)
      CHARACTER*40  BLNK
      CHARACTER*80  FLNM,FLIN,FLEI,FLOU
      CHARACTER*120 REC
     &             ,HTOTRED,HFUSRED,HFCCRED,HFCORED,HELARED,HCELCOR
C*
      DIMENSION    ENC(MXEN),PRC(MXEN,3)
C* Filenames and logical file units
      DATA LIN,LEI,LOU,LKB,LTT / 1, 2, 3, 5, 6 /
      DATA BLNK/'                                        '/
     1    ,FLIN/'inp.dat'/
     2    ,FLEI/'emp.inp'/
     3    ,FLOU/'emp.out'/
C*
C* Define input parameters - Write banner to terminal
      WRITE(LTT,901) ' '
      WRITE(LTT,901) ' EMPTUN - Set EMPIRE tuning parameters  '
      WRITE(LTT,901) ' =====================================  '
      WRITE(LTT,901) ' '
C* Define the source file
   12 WRITE(LTT,901) ' Default source tuning fact. filename : ',FLIN
      WRITE(LTT,901) '$          Enter new name to redefine : '
      READ (LKB,900) FLNM
      IF(FLNM(1:40).NE.BLNK) FLIN=FLNM
      OPEN(UNIT=LIN,FILE=FLIN,STATUS='OLD',ERR=12)
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
      KC=0
      HTOTRED=BLNK//BLNK//BLNK
      HFUSRED=BLNK//BLNK//BLNK
      HFCCRED=BLNK//BLNK//BLNK
      HFCORED=BLNK//BLNK//BLNK
      HELARED=BLNK//BLNK//BLNK
      HCELCOR=BLNK//BLNK//BLNK
  100 READ (LIN,901) FLNM
      IF     (FLNM(1:6).EQ.'FUSRED') THEN
        LC=1
      ELSE IF(FLNM(1:6).EQ.'ELARED') THEN
        LC=2
      ELSE IF(FLNM(1:6).EQ.'CELCOR') THEN
        LC=3
      ELSE
        WRITE(LTT,*) 'ERROR - Invalid tuning parameter header ',FLNM
        STOP 'EMPTUN ERROR - Invalid tuning parameter file'
      END IF
      KC=KC+1
      NEN=0
  120 READ (LIN,901) FLNM
      IF(FLNM(1:40).NE.BLNK) THEN
        NEN=NEN+1
        READ (FLNM,*) EE,FF
        EE=EE/1000000
C*      Check if the energy mesh is consistent
        IF(KC.GT.1 .AND. EE.NE.ENC(NEN)) THEN
          WRITE(LTT,*) 'ERROR - Iconsistent E-mesh for LC=',LC
     &                ,' Expected',ENC(NEN),' Found',EE
          STOP 'EMPTUN ERROR - Invalid tuning parameter E-mesh'
        END IF
        ENC(NEN)=EE
        PRC(NEN,LC)=FF
        GO TO 120
      END IF
      IF(KC.LT.3) GO TO 100
      WRITE(LTT,'(A,2F8.4,A,I4,A)')
     &           ' Tuning parameters between energies (MeV)'
     &            ,ENC(1),ENC(NEN),' at',NEN,' points'
      WRITE(LTT,901) ' '
C*
C* Tuning parametersfile processed - read EMPIRE input
      EN0    =0
      TOTRED0=1
      FUSRED0=1
      ELARED0=1
      CELCOR0=1
      EN1    =0
      TOTRED1=1
      FUSRED1=1
      ELARED1=1
      CELCOR1=1
      JEN    =1
C* Start reading the main EMPIRE input block
  200 READ (LEI,902) REC
C*    -- Check for tuning factor definitions in the main block
      IF     (REC(1:6).EQ.'TOTRED') THEN
        TOTRED0=TOTRED1
        READ (REC(7:120),*) TOTRED1
        HTOTRED(1:20)='$TOTRED'//REC(7:19)
      ELSE IF(REC(1:6).EQ.'FUSRED') THEN
        FUSRED0=FUSRED1
        READ (REC(7:120),*) FUSRED1
        HFUSRED(1:20)='$FUSRED'//REC(7:19)
      ELSE IF(REC(1:6).EQ.'FCCRED') THEN
        HFCCRED(1:20)='$FCCRED'//REC(7:19)
      ELSE IF(REC(1:6).EQ.'FCORED') THEN
        HFCORED(1:20)='$FCORED'//REC(7:19)
      ELSE IF(REC(1:6).EQ.'ELARED') THEN
        ELARED0=ELARED1
        READ (REC(7:120),*) ELARED1
        HELARED(1:20)='$ELARED'//REC(7:19)
      ELSE IF(REC(1:6).EQ.'CELCOR') THEN
        CELCOR0=CELCOR1
        READ (REC(7:120),*) CELCOR1
        HCELCOR(1:20)='$CELCOR'//REC(7:19)
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
        TOTRED0=TOTRED1
        FUSRED0=FUSRED1
        ELARED0=ELARED1
        CELCOR0=CELCOR1
C...
C...    print *,'read energy',ee,enc(1),enc(nen),fusred0
C...
        IF(EE.LT.0) THEN
C*        -- Last energy flag - Copy the rest of the file to output
  240     WRITE(LOU,902) REC
          READ (LEI,902,END=800) REC
          GO TO 240
        ELSE
C*        -- Check tuning factors for the current energy
C...      IF(EE.LT.ENC(1) .OR. EE.GE.ENC(NEN)) THEN
          IF(EE.LT.ENC(1) .OR. JEN.GT.NEN) THEN
C*          -- Copy all energies outside the adjustment range
            WRITE(LOU,902) REC
          ELSE
C*          -- Insert energies and tuning factors from the file
            DO WHILE(ENC(JEN).LE.EE)
              REC=BLNK//BLNK//BLNK
C...
C...          PRINT *,ENC(JEN),EE,FUSRED0,PRC(JEN,1),PRC(JEN,1)*FUSRED0
C...
C*            -- FUSRED=FCCRED=FCORED for consistency with TOTRED
              REC(1:7)='$FUSRED'
              WRITE(REC(8:17),910) PRC(JEN,1)*FUSRED0*TOTRED0
              WRITE(LOU,902) REC
              REC(1:7)='$FCCRED'
              WRITE(LOU,902) REC
              REC(1:7)='$FCORED'
              WRITE(LOU,902) REC
C*            -- ELARED
              REC(1:7)='$ELARED'
              WRITE(REC(8:17),910) PRC(JEN,2)*ELARED0*TOTRED0
              WRITE(LOU,902) REC
C*            -- CELCOR
              REC(1:7)='$CELCOR'
              WRITE(REC(8:17),910) PRC(JEN,3)*CELCOR0
              WRITE(LOU,902) REC
C*            -- Incident energy
              REC=BLNK//BLNK//BLNK
              WRITE(REC(1:8),908) ENC(JEN)
              WRITE(LOU,902) REC
              JEN=JEN+1
              IF(JEN.GT.NEN) EXIT
            END DO
C*          -- All additional tuning factors and energies entered
C*          -- Check for the last-defined tuning factor in originl input
            IF(EE.GT.ENC(NEN)) THEN
              IF(HFUSRED(1:40).EQ.BLNK) HFUSRED(1:12)='$FUSRED  1.0'
              IF(HFCCRED(1:40).EQ.BLNK) HFCCRED(1:12)='$FCCRED  1.0'
              IF(HFCORED(1:40).EQ.BLNK) HFCORED(1:12)='$FCORED  1.0'
              IF(HELARED(1:40).EQ.BLNK) HELARED(1:12)='$ELARED  1.0'
              IF(HCELCOR(1:40).EQ.BLNK) HCELCOR(1:12)='$CELCOR  1.0'
              IF(HTOTRED(1:40).NE.BLNK) WRITE(LOU,902) HTOTRED
              WRITE(LOU,902) HFUSRED
              WRITE(LOU,902) HFCCRED
              WRITE(LOU,902) HFCORED
              WRITE(LOU,902) HELARED
              WRITE(LOU,902) HCELCOR
C*            -- Incident energy
              REC=BLNK//BLNK//BLNK
              WRITE(REC(1:8),908) EE
              WRITE(LOU,902) REC
            END IF
          END IF
          GO TO 220
        END IF
      ELSE
C*
C* Process an energy-dependent command
        IF     (REC(1:7).EQ.'$TOTRED') THEN
          HTOTRED=REC
          TOTRED0=TOTRED1
          READ (REC(8:120),*) TOTRED1
        ELSE IF(REC(1:7).EQ.'$FUSRED') THEN
          HFUSRED=REC
          FUSRED0=FUSRED1
          READ (REC(8:120),*) FUSRED1
C...
C...      PRINT *,'Redefine FUSRED',fusred1
C...
        ELSE IF(REC(1:7).EQ.'$ELARED') THEN
          HELARED=REC
          ELARED0=ELARED1
          READ (REC(8:120),*) ELARED1
        ELSE IF(REC(1:7).EQ.'$CELCOR') THEN
          HCELCOR=REC
          CELCOR0=CELCOR1
          READ (REC(8:120),*) CELCOR1
        END IF
C*      -- Copy all commands outside the adjustment range
C*         and commands other than the tuning parameters
        IF((EE.LT.ENC(1) .OR. EE.GT.ENC(NEN)) .OR.
     &     (REC(1:7).NE.'$TOTRED' .AND.
     &      REC(1:7).NE.'$FUSRED' .AND.
     &      REC(1:7).NE.'$ELARED' .AND.
     &      REC(1:7).NE.'$CELCOR' .AND.
     &      REC(1:7).NE.'$FCCRED' .AND.
     &      REC(1:7).NE.'$FCORED')           ) THEN
          WRITE(LOU,902) REC
          GO TO 220
        END IF
      END IF
      GO TO 220
C* End of file processing
  800 STOP 'EMPTUN Completed'
C*
  900 FORMAT( A80)
  901 FORMAT(2A40)
  902 FORMAT(A120)
  908 FORMAT( F8.4)
  910 FORMAT(F10.4)
      END
