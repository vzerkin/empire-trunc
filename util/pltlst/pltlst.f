      PROGRAM PLTLST
C-Title  : Program PLTLST
C-Purpose: Prepare a list of EXFOR data that can be compared to ENDF
C-Author : A. Trkov, International Atomic Energy Agency, Vienna, Austria
C-Version: 2004/01 original code
C-V  2004/07 Fix format for metastable targets (A.Trkov)
C-V  2004/09 Include inelastic discrete level x-sect. (A.Trkov)
C-M
C-M  Manual for Program PLTLST
C-M  -------------------------
C-M
C-M  The EXFOR file in computational C4 format is scanned. Entries that
C-M  can be plotted are collected and a summary listing of such data
C-M  sets in generated. The output format is equivalent in format to
C-M  the one produced by the PLOTC4 code and may serve as the index
C-M  to the LSTTAB code for comparing EXFOR data to the contents of
C-M  evaluated nuclear data files in ENDF format.
C-M
C-M  Instructions:
C-M  Two input records may be specified:
C-M  - Name of the file with EXFOR data in C4 format (default C4.DAT)
C-M  - Name of the output list file (default PLOTC4.LST)
C-M
C-M  The default input filename is PLTLST.INP. If the file does not
C-M  exist, the program tries to read the entries from the default
C-M  input. If an end-of-file is encountered on the input file, the
C-M  remaining entries assume their default values.
C-
      LOGICAL      EXST
      CHARACTER*1  CHA,CHB,MST
      CHARACTER*2  CH(100)
      CHARACTER*3  CHC
      CHARACTER*10 CH10(3)
      CHARACTER*26 REF
      CHARACTER*40 BLNK,FLNM,FLEX,FLLS,FLIN
      CHARACTER*80 RC1,RC2
      CHARACTER*120     REC
      DATA BLNK/'                                        '/
     &     FLEX/'C4.DAT'/
     &     FLLS/'PLOTC4.LST'/
     &     FLIN/'PLTLST.INP'/
      DATA LEX,LLS,LIN,LTT/ 1, 2, 5, 6 /
      DATA PI/ 3.1415926 /
      DATA CH
     1 /'H ','He','Li','Be','B ','C ','N ','O ','F ','Ne'
     2 ,'Na','Mg','Al','Si','P ','S ','Cl','Ar','K ','Ca'
     3 ,'Sc','Ti','V ','Cr','Mn','Fe','Co','Ni','Cu','Zn'
     4 ,'Ga','Ge','As','Se','Br','Kr','Rb','Sr','Y ','Zr'
     5 ,'Nb','Mo','Tc','Ru','Rh','Pd','Ag','Cd','In','Sn'
     6 ,'Sb','Te','I ','Xe','Cs','Ba','La','Ce','Pr','Nd'
     7 ,'Pm','Sm','Eu','Gd','Tb','Dy','Ho','Er','Tm','Yb'
     8 ,'Lu','Hf','Ta','W ','Re','Os','Ir','Pt','Au','Hg'
     9 ,'Tl','Pb','Bi','Po','At','Rn','Fr','Ra','Ac','Th'
     * ,'Pa','U ','Np','Pu','Am','Cm','Bk','Cf','Es','Fm'/
C* Fractional tolerance for differentiating incident energies of
C* differential and double differential data
      DATA ETOL/ 0.015 /
C* Write the banner
      WRITE(LTT,903)
      WRITE(LTT,903) ' PLTLST - Generate listing of EXFOR data'
      WRITE(LTT,903) ' ---------------------------------------'
C*
C* Check for the existence of the input file
      INQUIRE(FILE=FLIN,EXIST=EXST)
      IF(EXST) OPEN(UNIT=LIN,FILE=FLIN,STATUS='OLD')
C* If input does not exist, try reading the default input
      IF(.NOT. EXST)
     &WRITE(LTT,903) ' Enter the C4 filename                  '
      READ (LIN,903,END=10) FLNM
      IF(FLNM.NE.BLNK) FLEX=FLNM
      IF(.NOT. EXST)
     &WRITE(LTT,903) ' Enter the output list filename         '
      READ (LIN,903,END=10) FLNM
      IF(FLNM.NE.BLNK) FLLS=FLNM
C* Open the files
   10 OPEN (UNIT=LEX,FILE=FLEX,STATUS='OLD')
      OPEN (UNIT=LLS,FILE=FLLS,STATUS='UNKNOWN')
C* Write the banner
      WRITE(LLS,910)
      WRITE(LLS,912)
      WRITE(LLS,910)
C*
C* Read the first C4 record
      IEX=1
      IDX=0
      READ (LEX,901,END=80) REC
      IF(REC(1:40).EQ.BLNK) GO TO 80
      READ (REC,902) IZI0,IZA0,MST,MF0,MT0,CHA0,CHB0,ENR0,DEN0,XSR0,DXS0
     &              ,PRA0,PRB0,PRC0,PRD0,CHC0,REF0,NEN0,NSU0
      IZP0=1
      IF(MT0.EQ.9000) IZP0=PRB0
C*
C* Process all C4 records and check for changes
   20 CONTINUE
      IEF =1
      READ (LEX,901,END=40) REC
      IF(REC(1:40).EQ.BLNK) GO TO 40
      READ (REC,902) IZI1,IZA1,MST,MF1,MT1,CHA1,CHB1,ENR1,DEN1,XSR1,DXS1
     &              ,PRA1,PRB1,PRC1,PRD1,CHC1,REF1,NEN1,NSU1
      IEF =0
      IZP1=1
      IF(MT1.EQ.9000) IZP1=PRB1
C* Mark for printout if any of the parameters change
      IF(IZI1.NE.IZI0) GO TO 40
      IF(IZA1.NE.IZA0) GO TO 40
      IF(MF1 .NE.MF0 ) GO TO 40
      IF(MT1 .NE.MT0 ) GO TO 40
C* Mark partial inelastic MF 3 where level energy changes
      IF(MF1 .EQ. 3 .AND. PRC1.NE.PRC0) GO TO 40
C* Mark ang.distr. MF 4 where level energy changes
      IF(MF1 .EQ. 4 .AND. PRC1.NE.PRC0) GO TO 40
C* Mark spectra MF 5 where outgoing particle angle changes
      IF(MF1 .EQ. 5 .AND. PRA1.NE.PRA0) GO TO 40
C* Mark spectra MF 6 where outgoing particle angle changes
      IF(MF1 .EQ. 6 .AND. PRA1.NE.PRA0) GO TO 40
C* Mark spectra where outgoing particle changes
      IF(IZP1.NE.IZP0) GO TO 40
C* Mark entries for MF>3 where incident particle energy changes
      DE=ABS(ENR1-ENR0)
      IF((MF1 .GT. 3 .AND. MF1.NE.10) .AND. DE.GT.ENR0*ETOL) GO TO 40
C* None of the print conditions is satisfied - add to the data count
      IEX=IEX+1
      GO TO 20
C*
C* Reaction/Energy/Particle change - print record for previous set
   40 CONTINUE
C* Exclude printout for the following conditions
C* - MF out of range
      IF(MF0.LT.3 .OR. (MF0.GT.6 .AND. MF0.NE.10)) GO TO 60
C* - MT out of range
      IF(MT0.GT.999 .AND. MT0.NE.9000) GO TO 60
C* - Insufficient number of points (this also excludes distributions
C*   which are not suitably sorted and would result in excessive output)
      IF(IEX.LE.2) GO TO 60
C*
C* Printout conditions satisfied - prepare output record
      IZ=IZA1/1000
      IA=IZA1-1000*IZ
      DO I=1,3
        CH10(I)='          '
      END DO
C* Consider different cases
      IF     (MF0.EQ.3) THEN
C* - MF3 discrete level energy (if present)
        IF(PRC0.NE.0) WRITE(CH10(3),'(1P,E10.4E1)') PRC0
      ELSE IF(MF0.EQ.4) THEN
C* - MF4 incident particle energy, angle and level
        WRITE(CH10(1),'(1P,E10.3E1)') ENR0
        IF(PRC0.NE.0) WRITE(CH10(3),'(1P,E10.4E1)') PRC0
      ELSE IF(MF0.EQ.5) THEN
C* - MF 5 incident particle energy and outgoing particle angle
        WRITE(CH10(1),'(1P,E10.3E1)') ENR0
C... Print 2 decimal places for consistency with PLOTC4 output
C...    WRITE(CH10(2),'(F8.1)') ACOS(PRA0)*180/PI
        WRITE(CH10(2),'(F8.2)') ACOS(PRA0)*180/PI
      ELSE IF(MF0.EQ.6) THEN
        WRITE(CH10(1),'(1P,E10.3E1)') ENR0
C... Print 2 decimal places for consistency with PLOTC4 output
C...    WRITE(CH10(2),'(F8.1)') ACOS(PRA0)*180/PI
        WRITE(CH10(2),'(F8.2)') ACOS(PRA0)*180/PI
      END IF
C* Check for close-lying discrete levels
      RC2=RC1
      WRITE(RC1,914) IZ,CH(IZ),IA,MST,IZP0,MF0,MT0,IEX,CH10,IDX
      IF(MF0.EQ.3 .AND. IDX.GT.0) THEN
        IF(RC1(1:26).EQ.RC2(1:26)) THEN
          READ (RC1(63:72),904) FLV
          READ (RC2(63:72),904) FL1
          IFLV=NINT(FLV)
          IFL1=NINT(FL1)
          IF(ABS(IFLV-IFL1).LE.150) THEN
            RC1=RC2
            GO TO 60
          END IF
        END IF
      END IF
C* Write a record to output list file
      IDX=IDX+1
      WRITE(LLS,914) IZ,CH(IZ),IA,MST,IZP0,MF0,MT0,IEX,CH10,IDX
c*
C* Reset parameters
   60 IEX=1
      IZI0=IZI1
      IZA0=IZA1
      IZP0=IZP1
      MF0 =MF1
      MT0 =MT1
      CHA0=CHA1
      CHB0=CHB1
      CHC0=CHC1
      ENR0=ENR1
      DEN0=DEN1
      XSR0=XSR1
      DXS0=DXS1
      PRA0=PRA1
      PRB0=PRB1
      PRC0=PRC1
      PRD0=PRD1
      REF0=REF1
      NEN0=NEN1
      NSU0=NSU1
      IF(IEF.NE.1) GO TO 20
C* All processing completed
   80 WRITE(LLS,910)
      STOP 'PLTLST Completed'
C*
  901 FORMAT(A120)
  902 FORMAT(I5,I6,A1,I3,I4,2A1,1X,8F9.0,A3,A26,I4,I3)
  903 FORMAT(2A40)
  904 FORMAT(BN,F10.0)
  910 FORMAT(' ======================================='
     &      ,'====================================')
  912 FORMAT(' MATERIAL     ZAP  MF   MT  EVAL. EXPR. '
     &      ,'EXPR.    E-INC ANG-OUT ELV/E-OUT IDX'/
     &       '                            PNTS. PNTS. '
     &      ,' REF.       EV     DEG        EV    ')
  914 FORMAT(I3,'-',A2,'-',I3,A1,I6,I4,I5,6X,I6,6X,A10,A8,A10,I4)
      END
