      PROGRAM LSTTAB
C-Title  : Program LSTTAB
C-Purpose: Tabulate ENDF and EXFOR data in PLOTTAB format
C-Author : A.Trkov, IAEA-NDS.
C-Version: 00/12 Original code
C-M  
C-M  Manual for Program LSTTAB
C-M  =========================
C-M
C-M  The program uses the list file generated by PLOTC4 to extract
C-M  data from an ENDF file and from the C4 file containing EXFOR
C-M  data in computational format (generated by X4TOC4) and writes
C-M  them in PLOTTAB "curves" and "points" format, respectively.
C-M
C-M  Instructions:
C-M  Input parameters can be entered interactively in response to
C-M  the prompts on the screen:
C-M  1  PLOTC4 list file.
C-M  2  EXFOR source file in computational C4 format.
C-M  3a ENDF source file.
C-M  3b Label to appear on the plot for this source file.
C-M     NOTE: Up to MXEN files can be specified.
C-M  4  Resolution broadening fraction (0<Ep6<0.1)
C-M  5a Index number from the PLOTC4 list file to be processed.
C-M  5b Scaling factor for this set (default scaling factor=1,
C-M     an arbitrary factor may be assigned, for example to
C-M     differentiate between data sets on the same plot).
C-M     NOTE: Several pairs of entries may be chosen, terminated
C-M     by blank.
C-M
C-M  Files used:
C-M   Name Unit   Description
C-M   FLLS LLS=1  PLOTC4 list file.
C-M   FLEF LEF=2  ENDF source file.
C-M   FLC4 LC4=3  EXFOR source file in computational format.
C-M   FLPN LPN=7  Selected experimental points (PLOTTAB points fmt).
C-M   FLCU LCU=8  Selected curve from ENDF file (PLOTTAB points fmt).
C-M
C-Extern.: DXSEND,DXSEXF,COMCUR
C-
      PARAMETER   (MPT=1000,MXP=100000,MXR=400000,MXEN=5)
      CHARACTER*1  CM
      CHARACTER*40 BLNK,FLNM,FLLS,FLC4,FLPN,FLCU
     1            ,FLEF(MXEN),COM(MXEN)
      CHARACTER*80 COM1,COM2
      CHARACTER*80 C80,RFX(MPT)
      DIMENSION    ES(MXP),SG(MXP),RWO(MXR)
C* Default logical file units
      DATA LLS,LEF,LC4,LKB,LTT,LCU,LPN
     1    /  1,  2,  3,  5,  6, 7, 8 /
      DATA BLNK/'                                        '/
     1     FLLS/'PLOTC4.LST'/
     3     FLC4/'C4.DAT'/
     7     FLPN/'LSTTAB.PNT'/
     8     FLCU/'LSTTAB.CUR'/
     2     FLEF(1)/'ENDF.DAT'/
C*
      DATA PI/3.14159265/
      DATA EP6/0.03/
C*
C* Write banner
      WRITE(LTT,91) ' LSTTAB - Extract Data from ENDF / C4   '
      WRITE(LTT,91) ' ------------------------------------   '
      WRITE(LTT,91)
C* Define the PLOTC4 list file
   12 WRITE(LTT,91) ' Default PLOTC4 list filename         : ',FLLS
      WRITE(LTT,91) '$          Enter new name to redefine : '
      READ (LKB,91) FLNM
      IF(FLNM.NE.BLNK) FLLS=FLNM
      OPEN (UNIT=LLS,FILE=FLLS,STATUS='OLD',ERR=12)
C* Read the PLOTC4 list file
      CALL RDC4LS(LLS,NID,RFX)
      CLOSE(UNIT=LLS)
C* Define the EXFOR file in computational format
   22 WRITE(LTT,91) ' Default C4 file of EXFOR data        : ',FLC4
      WRITE(LTT,91) '$          Enter new name to redefine : '
      READ (LKB,91) FLNM
      IF(FLNM.NE.BLNK) FLC4=FLNM
      OPEN (UNIT=LC4,FILE=FLC4,STATUS='OLD',ERR=22)
C* Define the ENDF files
      NEN=0
      WRITE(LTT,91) ' Enter ENDF files terminated by "-"     '
      GO TO 25
   24 WRITE(LTT,91) '         ERROR - Redo: Invalid filename ',FLNM
   25 WRITE(LTT,91) '$            Enter the filename       : '
      READ (LKB,91) FLNM
      IF(FLNM.EQ.BLNK .AND. NEN.EQ.0) FLNM=FLEF(1)
      IF(FLNM.EQ.BLNK .OR.  FLNM(1:1).EQ.'-') GO TO 30
      OPEN (UNIT=LEF,FILE=FLNM,STATUS='OLD',ERR=24)
      CLOSE(UNIT=LEF)
      NEN=NEN+1
      COM(NEN)=BLNK
      GO TO 25
   30 CONTINUE
C...  IF(NEN.LE.1) GO TO 40
      WRITE(LTT,95) ' Total number of ENDF files entered     ',NEN
      DO 32 I=1,NEN
      WRITE(LTT,91) ' ENDF file                            : ',FLEF(I)
      WRITE(LTT,91) '$  Enter new label to redefine default: '
      READ (LKB,91) COM(I)
   32 CONTINUE
C* Redefine labels if necessary
   40 CONTINUE
C*
C* Open the output files
      OPEN (UNIT=LPN,FILE=FLPN,STATUS='UNKNOWN')
      OPEN (UNIT=LCU,FILE=FLCU,STATUS='UNKNOWN')
C*
   42 WRITE(LTT,91) '$Enter the resol.broadening fraction  : '
      READ (LKB,98) DMY
      IF(DMY.GT.0.1) THEN
        WRITE(LTT,91) ' ERROR: Range 0<ep6<0.1 - Redo          '
        GO TO 42
      ELSE IF(DMY.GT.0) THEN
        EP6=DMY
      END IF
C*
C* Select the PLOTC4 list entry index
   50 WRITE(LTT,91) '$Enter entry index number             : '
      READ (LKB,97,ERR=50) IDX
      IF(IDX.LE.0) GO TO 90
      IF(IDX.GT.NID) THEN
        WRITE(LTT,91) '           ERROR - Invalid entry - redo '
        GO TO 50
      END IF
   54 WRITE(LTT,91) '$Enter the x-sect. scaling factor     : '
      READ (LKB,98,ERR=54) SCL
      IF(SCL.EQ.0) SCL=1
C*
C* Process the index entry
      C80=RFX(IDX)
      READ (C80,92) IZ,IA,CM,MAT,MF,MT,JEP,JXP,JFX,EIN,DEG,EOU
C*
      COM2=C80(1:11)//C80(19:22)//C80(23:27)
      IF(MF.NE.3) COM2=COM2(1:20)//'Ei'//C80(46:49)//C80(52:55)
      IF(C80(56:59).NE.'    ')
     1 COM2=COM2(1:30)//'An'//C80(56:59)
      IF(C80(63:67).NE.'    ')
     1 COM2=COM2(1:30)//'Eo'//C80(64:67)//C80(70:72)
C*
      IF(C80(55:62).EQ.'        '  ) DEG=-2
      IF(C80(63:72).EQ.'          ') EOU=-2
      ZA0=IZ*1000+IA
      IF(CM.NE.' ') ZA0=ZA0+0.1
      ZAP=1
      PAR=-2
      MTE=MT
      IF     (MF.EQ.3              ) THEN
        KEA=0
      ELSE IF(MF.GE.4 .AND. MF.LE.6) THEN
        IF(MF.EQ.4 .OR.
     1    (MF.EQ.6 .AND. DEG.LT.0) ) THEN
C* Request angular distributions
          KEA=1
          PAR=EOU
          IF(MT.EQ.9000) MTE=5
        END IF
        IF(MF.EQ.5 .OR.
     1    (MF.EQ.6 .AND. EOU.LT.0) ) THEN
C* Request energy spectra
          KEA=2
          IF(DEG.GE.0) PAR=COS(DEG*PI/180)
          IF(MT.EQ.9000) MTE=5
        END IF
      END IF
C*
C* Extract the data from the ENDF file
      ZA=ZA0
      DO 86 M=1,NEN
      OPEN (UNIT=LEF,FILE=FLEF(M),STATUS='OLD')
      CALL DXSEND(LEF,ZA,ZAP,MTE,KEA,EIN,PAR,EP6,ES,SG
     1           ,RWO,NP,MXP,MXR,LTT)
      IF(NP.LE.0) PRINT *,'DXSEND CUR:mt,kea,ein,par',mt,kea,ein,par
      CLOSE(UNIT=LEF)
C* Prepare the ENDF comment header for the PLOTTAB curves file
      COM1=COM(M)
      IF(COM(M).EQ.BLNK) CALL COMCUR(MAT,MF,MTE,KEA,EIN,PAR,COM1)
C* Write the data to the PLOTTAB curves file
      WRITE(LCU,91) COM1,COM2
      DO 82 I=1,NP
C* Suppress printing negative or zero points
      EE=ES(I)
      IF(KEA.EQ.1) EE=ACOS(EE)*180/PI
      FF=SG(I)*SCL
      IF(SG(I).GT.0) WRITE(LCU,94) EE,FF
   82 CONTINUE
      WRITE(LCU,94)
   86 CONTINUE
C*
C* Extract the data from the C4 file
      CALL DXSEXF(LC4,LPN,ZA0,ZAP,MF,MT,KEA,EIN,PAR,NP,NS,SCL,COM2)
      IF(NP.LE.0) PRINT *,'DXSEXF PNT:mt,kea,ein,par',mt,kea,ein,par
C*
C* Try another set of points
      REWIND LC4
      GO TO 50
C* All processing completed
   90 STOP 'LSTTAB Completed'
C*
   91 FORMAT(2A40)
   92 FORMAT(I3,4X,I3,A1,I6,I4,I5,3I6,1P,E10.3,0P,F8.2,1P,E10.3,I4)
   94 FORMAT(1P,3(E11.5E1,E11.4))
   95 FORMAT(A40,I6)
   96 FORMAT(A80)
   97 FORMAT(BN,I10)
   98 FORMAT(BN,F10.0)
      END
      SUBROUTINE RDC4LS(LLS,NID,RFX)
C-Title  : Subroutine RDC4LS
C-Purpose: Read the PLOTC4 list file
      CHARACTER*80 C80,RFX(1)
      CHARACTER*1  CM
C*
      NID=0
      READ (LLS,96,END=20) C80
C* Test for partial list file beginning with " MATERIAL "
      IF(C80(1:10).EQ.' MATERIAL ') GO TO 15
      IF(C80(1:10).NE.' PLOT ENDF') THEN
C* Test for partial list file beginning with the items list directly
C* (dummy read)
        READ (C80,92,ERR=14) IZ,IA,CM,MAT,MF,MT,JEP,JXP,JFX,EIN,DEG,EOU
        GO TO 17
      END IF
C* Search for the " MATERIAL " string
   14 READ (LLS,96,END=20) C80
      IF(C80(1:10).NE.' MATERIAL ') GO TO 14
   15 READ (LLS,96) C80
      READ (LLS,96) C80
C* Read the items into RFX array
   16 READ (LLS,96,END=20) C80
   17 IF(C80(1:10).EQ.' =========') GO TO 20
C* Move any "metastable" nuclide flag for easier reading
      IF(C80(9:9).EQ.'M') THEN
        C80( 9: 9)='M'
        C80(11:11)='M'
      END IF
      IF(C80(10:10).EQ.'M') THEN
        C80(10:10)='M'
        C80(11:11)='M'
      END IF
C* Save the record
      NID=NID+1
      RFX(NID)=C80
      GO TO 16
C* File processed
   20 CLOSE(UNIT=LLS)
      IF(NID.LT.1) STOP ' LSTTAB ERROR - Invalid PLOTC4 list'
      RETURN
   92 FORMAT(I3,4X,I1,2X,A1,2X,I4,I4,I5,3I6,F10.0,F8.2,F10.0,I4)
   96 FORMAT(A80)
      END
      SUBROUTINE COMCUR(MAT,MF,MT,KEA,EIN,PAR,COM)
C-Title  : CURCOM Subroutine
C-Purpose: Construct comment headed for the "curves" file
      CHARACTER*80  COM
      CHARACTER*1   UN,UO
      DATA PI/3.14159265/
C*
      IF(MF.EQ.3) THEN
        WRITE(COM,925) MAT,MF,MT
      ELSE
        IF     (EIN.GT.1000000) THEN
          UN='M'
          EE=EIN/1000000
        ELSE IF(EIN.GT.1000   ) THEN
          UN='K'
          EE=EIN/1000
        ELSE
          EE=EIN
        UN=' '
        END IF
        IF(KEA.EQ.2) THEN
          IF(PAR.GE.0) THEN
            DEG=ACOS(PAR)*180/PI
            WRITE(COM,924) MT,UN,EE,NINT(DEG)
          ELSE
            WRITE(COM,924) MT,UN,EE
          END IF
        ELSE
          IF     (PAR.GT.999999) THEN
            UO='M'
            EO=PAR/1000000
          ELSE IF(PAR.GT.999   ) THEN
            UO='K'
            EO=PAR/1000
          ELSE
            EO=PAR
            UO=' '
          END IF
          WRITE(COM,922) MT,UN,EE,UO,EO
          IF(EO.LT.0) COM(16:23)='        '
        END IF
      END IF
      RETURN
  922 FORMAT('MT',I3,1X,A1,'eV',F5.1,1X,A1,'eV',F5.1)
  924 FORMAT('MT',I3,1X,A1,'eV',F5.1,:,'  Deg',I3)
  925 FORMAT(' MAT',I4,' MF',I2,' MT',I3,:,20X,' EI',1P,E10.3)
      END
      SUBROUTINE DXSEXF(LC4,LPN,ZA0,ZAP0,MF0,MT0,KEA,EI0,PR0
     1                 ,NPP,NS,SCL,COM2)
C-Title  : Subroutine DXSEXF
C-Purpose: Extract data from EXFOR computational format file
      CHARACTER*1  MM(3),CM,C1,C2,C3
      CHARACTER*11 REC(6)
      CHARACTER*25 REF,RF0
      CHARACTER*40 COM2
      DATA MM/' ','M','N'/
C* Fractional tolerance to identify "equal" argiments
      DATA EPS/0.005/
      DATA PI/3.14159265/
C*
      RF0='                         '
      IZA0=ZA0
      IZAP0=ZAP0
      IM   =10*(ZAP0-IZAP0)+1.1
      NS   =0
      NPP  =0
      NP   =0
      DO 12 J=1,6
      REC(J)='           '
   12 CONTINUE
C*
   20 READ (LC4,901,END=80) IZAP,IZA,CM,MF,MT,C1,C2,C3
     1                     ,F1,F2,F3,F4,F5,F6,F7,F8,LBL,REF
C* Test for matching data request
      IF(IZAP.NE.IZAP0 ) GO TO 20
      IF(IZA0.GT.0 .AND. IZA .NE.IZA0  ) GO TO 20
      IF(CM  .NE.MM(IM)) GO TO 20
      IF(MF  .NE.MF0   ) GO TO 20
      IF(MT  .NE.MT0   ) GO TO 20
      IF(MF.GE.4 .AND. MF.LE.6) THEN
C* Test incident energy
        IF(ABS(EI0/F1-1).GT.1.E-3) GO TO 20
C* Test outgoing particle energy for correalted angular distributions
        IF((MF.EQ.6 .AND. KEA.EQ.1) .AND.
     1     ABS(PR0/F7-1).GT.EPS) GO TO 20
C* Test outgoing particle scattering angle cos for correal.ang.distrib.
        IF((MF.EQ.6 .AND. KEA.EQ.2) .AND.
     1     ABS(PR0/F5-1).GT.EPS) GO TO 20
      END IF
C* Identify next set of points if author changes
      IF(REF.NE.RF0) THEN
        IF(NS.GT.0) WRITE(LPN,920)
        NP=0
        NS=NS+1
        WRITE(LPN,902) REF,COM2
        RF0=REF
      END IF
C*
      IF(MF.EQ.3) THEN
C* Simple cross sections
        WRITE(REC(1),911) F1
        IF(F2.NE.0) THEN
          WRITE(REC(2),912) F2
          WRITE(REC(3),912) F2
        ELSE
          REC(2)='           '
          REC(3)='           '
        END IF
        WRITE(REC(4),911) F3
        IF(F4.NE.0) THEN
          G4=MIN(0.9*F3, F4)
          WRITE(REC(5),912) G4
          WRITE(REC(6),912) F4
        ELSE
          REC(5)='           '
          REC(6)='           '
        END IF
C* Angular distributions
      ELSE IF(MF.EQ.4 .OR.(MF.EQ.6 .AND. KEA.EQ.1) ) THEN
C* Convert elastic ang.distrib. from CM to Lab if necessary
        IF((MF.EQ.4 .AND. MT.EQ.2) .AND. C3.NE.' ') THEN
          EIN=F1
          AIN=F5
          QQ =0
          AWR=IZA-1000*(IZA/1000)
          GAM=SQRT( EIN/ (EIN*AWR*AWR+QQ*AWR*(AWR+1) ) )
          DMC=2*GAM*AIN + SQRT(1-GAM*GAM*(1-AIN*AIN))
     1       +(GAM*AIN)**2 / SQRT(1-GAM*GAM*(1-AIN*AIN))
          F3=F3*DMC
          F5=(AIN+GAM)/SQRT(1+GAM*(GAM+2*AIN))
        END IF
C* Convert cosine to degrees
        A6=MIN(1.,F5+F6)
        F5=ACOS(F5)*180/PI
        WRITE(REC(1),911) F5
        IF(F6.NE.0) THEN
          A6=ACOS(A6)*180/PI-F5
          WRITE(REC(2),912) A6
          WRITE(REC(3),912) A6
        ELSE
          REC(2)='           '
          REC(3)='           '
        END IF
C* Energy spectra
      ELSE IF(MF.EQ.5 .OR.(MF.EQ.6 .AND. KEA.EQ.2) ) THEN
        WRITE(REC(1),911) F7
        IF(F8.NE.0) THEN
          WRITE(REC(2),912) F8
          WRITE(REC(3),912) F8
        ELSE
          REC(2)='           '
          REC(3)='           '
        END IF
      END IF
C* Cross section
        WRITE(REC(4),912) F3*SCL
        IF(F4.NE.0) THEN
          G4=MIN(0.9*F3, F4)
          WRITE(REC(5),912) F4*SCL
          WRITE(REC(6),912) G4*SCL
        ELSE
          REC(5)='           '
          REC(6)='           '
        END IF
C* Suppress printing negative or zero points
      IF(F3.GT.0) THEN
        NP=NP+1
        NPP=NPP+1
        WRITE(LPN,920) (REC(J),J=1,6)
      END IF
      GO TO 20
C* End of data set
   80 WRITE(LPN,920)
      RETURN
C*
  901 FORMAT(I5,I6,A1,I3,I4,3A1,8F9.0,A3,A25)
  902 FORMAT(A25,15X,A40)
  911 FORMAT(1P,E11.4E1)
  912 FORMAT(1P,E11.3)
  920 FORMAT(6A11)
      END
