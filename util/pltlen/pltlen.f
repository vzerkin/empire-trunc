      PROGRAM PLTLEN
C-Title  : Program PLTLEN
C-Purpose: Prepare a list of ENDF data
C-Author : A. Trkov, International Atomic Energy Agency, Vienna, Austria
C-Version: 2004/11 original code
C-V  05/02 Add projectile ZA to the list file
C-V  05/04 Minor fix
C-V  06/03 Add MF 6 detailed reaction list
C-V  06/04 Add MF 4 detailed reaction list
C-V  07/04 - Trivial fix for initialisation (V. Zerkin)
C-V        - Add MF 4 MT=MT+40000 for cross sections at fixed angle
C-V  08/02 Add to list from MF10 (A. Trkov)
C-V  16/01 Add to list the nu-bar (MT 452, 456)
C-M
C-M  Manual for Program PLTLEN
C-M  -------------------------
C-M
C-M  A pointwise ENDF file is scanned. Entries are collected and a
C-M  summary listing is generated. The output format is equivalent
C-M  to the one produced by the PLOTC4 code and may serve as the index
C-M  to the LSTTAB code for plotting ENDF data.
C-M
C-M  Instructions:
C-M  Two input records may be specified:
C-M  - Name of the ENDF file (default ENDF.PEN)
C-M  - Name of the output list file
C-M    default: same as PEN filename with extension LST.
C-M
C-M  The default input filename is PLTLEN.INP. If the file does not
C-M  exist, the program tries to read the entries from the default
C-M  input. If an end-of-file is encountered on the input file, the
C-M  remaining entries assume their default values.
C-M
C-M  WARNING: Current version for MF3, MF4(law 7), MF10 only!
C-
      PARAMETER   (MXLV=400,MXAN=3,MXEN=1000)
      LOGICAL      EXST
      CHARACTER*1  MST,CST(3)
!      CHARACTER*2  CH(100)
      CHARACTER*2  Nuc(113)
      CHARACTER*10 CH10(3)
      CHARACTER*40 BLNK,FLNM,FLEN,FLLS,FLIN
      CHARACTER*66 C66
      DIMENSION    MTL(MXLV),ILV(MXLV),ANG6(MXAN),ENIN(MXEN)
      DATA BLNK/'                                        '/
     &     FLEN/'ENDF.PEN'/
     &     FLLS/'                                        '/
     &     FLIN/'PLTLEN.INP'/
      DATA LEN,LLS,LIN,LTT/ 1, 2, 5, 6 /
!      DATA CH
      DATA Nuc/'nn',
     1  'H ','He','Li','Be','B ','C ','N ','O ','F ','Ne'
     2 ,'Na','Mg','Al','Si','P ','S ','Cl','Ar','K ','Ca'
     3 ,'Sc','Ti','V ','Cr','Mn','Fe','Co','Ni','Cu','Zn'
     4 ,'Ga','Ge','As','Se','Br','Kr','Rb','Sr','Y ','Zr'
     5 ,'Nb','Mo','Tc','Ru','Rh','Pd','Ag','Cd','In','Sn'
     6 ,'Sb','Te','I ','Xe','Cs','Ba','La','Ce','Pr','Nd'
     7 ,'Pm','Sm','Eu','Gd','Tb','Dy','Ho','Er','Tm','Yb'
     8 ,'Lu','Hf','Ta','W ','Re','Os','Ir','Pt','Au','Hg'
     9 ,'Tl','Pb','Bi','Po','At','Rn','Fr','Ra','Ac','Th'
     A ,'Pa','U ','Np','Pu','Am','Cm','Bk','Cf','Es','Fm'
     B ,'Md','No','Lr','Rf','Db','Sg','Bh','Hs','Mt','Ds'
     C ,'Rg','Cn'/
C* States of the target nucleus
      DATA CST/' ','m','n'/
      DATA PI/ 3.1415926 /
C* Default angles for double-fifferential data
      DATA ANG6/ 10.0, 60.0, 150.0 /
C* Write the banner
      WRITE(LTT,903)
      WRITE(LTT,903) ' PLTLEN - Generate listing of ENDF data '
      WRITE(LTT,903) ' -------------------------------------- '
C*
C* Check for the existence of the input file
      INQUIRE(FILE=FLIN,EXIST=EXST)
      IF(EXST) OPEN(UNIT=LIN,FILE=FLIN,STATUS='OLD')
C* If input does not exist, try reading the default input
      IF(.NOT. EXST)
     &WRITE(LTT,903) ' Enter the ENDF filename                  '
      READ (LIN,903,END=10) FLNM
      IF(FLNM.NE.BLNK) FLEN=FLNM
      IF(.NOT. EXST)
     &WRITE(LTT,903) ' Enter the output list filename         '
      READ (LIN,903,END=10) FLNM
      IF(FLNM.NE.BLNK) FLLS=FLNM
C* Define default LST filename
   10 IF(FLLS.NE.BLNK) GO TO 12
      I1=1
      DO WHILE (FLEN(I1:I1).EQ.' ')
        I1=I1+1
        IF(I1.GT.40) THEN
          print *,'PLTLEN ERROR - Invalid output LST filename ',FLEN
          STOP 'PLTLEN ERROR - Invalid output LST filename'
        END IF
      END DO
      I2=I1
      DO WHILE (FLEN(I2:I2).NE.' ' .AND. FLEN(I2:I2).NE.'.')
        I2=I2+1
      END DO
      IP=I2-I1
      I2=I2-1
      DO J=1,IP
       FLLS(J:J)=FLEN(I1-1+J:I1-1+J)
      END DO
      FLLS(IP+1:IP+4)='.lst'
C* Open the files
   12 OPEN (UNIT=LEN,FILE=FLEN,STATUS='OLD')
      OPEN (UNIT=LLS,FILE=FLLS,STATUS='UNKNOWN')
C* Write the banner
      WRITE(LTT,903) ' Contents of source ENDF file         : ',FLEN
      WRITE(LLS,903) ' Contents of source ENDF file         : ',FLEN
      WRITE(LLS,910)
      WRITE(LLS,912)
      WRITE(LLS,910)
C*
C* Read the ENDF header record
      READ (LEN,901) C66
      MAT0=0
      MF0 =0
      MT0 =0
      IDX =0
      ENR =0
      ANG =0
      IMT2=0
C*
C* Loop reading sections
   20 READ (LEN,901) C66,MAT,MF,MT
      IF(MAT.LT.0) GO TO 80
      IF(MT .EQ.0) GO TO 20
      IF(MAT.NE.MAT0) THEN
C*      -- New material - extract ZA
        READ (C66(1:11),*) ZA
        IZA=NINT(ZA)
        IZ=IZA/1000
        IA=IZA-IZ*1000
        MAT0=MAT
        NLV =0
        MT0 =0
C*      -- Extract incident particle, target state, if possible
        IF(MF.EQ.1 .AND. MT.EQ.451) THEN
C*        -- Define MST from LIS0
          READ (LEN,901) C66,MAT,MF,MT
          READ (C66(34:44),*) LIS0
          IF(LIS0.LE.2) THEN
            MST=CST(LIS0+1)
          ELSE
            PRINT *,'Unknown state',LIS0,' for nuclide',IZA
          END IF
C*        -- Define incident particla ZA from NLIB
          READ (LEN,901) C66,MAT,MF,MT
          READ (C66(45:55),*) NLIB
          IZP=NLIB/10
        END IF
      END IF
      IF(MF.NE.MF0 .OR. MT.NE.MT0) THEN
C* New section - print entry
        DO I=1,3
          CH10(I)='          '
        END DO
        IEN=0
        IF     (MF.EQ.1) THEN
          IF(MT.EQ.452 .OR. MT.EQ.456) THEN
          READ (LEN,901) C66,MAT,MF0,MT0
C*        -- Read number of points
          READ (C66(56:66),*) IEN
C*        -- Write the entry to list file
          IDX=IDX+1
          CALL MTTOZA(MT,IZP,IZO)
          WRITE(LLS,914)IZ,Nuc(IZ+1),IA,MST,IZO,MF,MT,IEN,CH10,IDX,IZP
          END IF
        ELSE IF(MF.EQ.3) THEN
C*
C*        -- MF3 Save level energy, if non-zero
          READ (LEN,901) C66,MAT,MF,MT
          READ (C66( 1:11),*) QM
          READ (C66(12:22),*) QI
          JLV=NINT(QM)-NINT(QI)
          IF(JLV.NE.0) THEN
            NLV=NLV+1
            MTL(NLV)=MT
            ILV(NLV)=JLV
          END IF
C*        -- Read number of points
          READ (C66(56:66),*) IEN
C*        -- Write the entry to list file
          IDX=IDX+1
          CALL MTTOZA(MT,IZP,IZO)
          WRITE(LLS,914)IZ,Nuc(IZ+1),IA,MST,IZO,MF,MT,IEN,CH10,IDX,IZP
          MT0=MT
          MF0=MF
        ELSE IF(MF.EQ.10) THEN
C*
C* MF10 
          READ (C66(45:55),*) NS
          DO I=1,NS
C*          -- Read number of points
            READ (LEN,901) C66,MAT,MF,MT
            READ (C66( 1:11),*) QM
            READ (C66(12:22),*) QI
            READ (C66(34:44),*) LFS
c...        JLV=NINT(QM)-NINT(QI)
c...        ELV=QM-QI
c...        IF(JLV.NE.0) WRITE(CH10(3),'(1P,E10.4E1)') ELV
            WRITE(CH10(3),'(F10.1)') FLOAT(LFS)
            READ (C66(23:33),*) IZO
            READ (C66(34:44),*) LFS
            READ (C66(56:66),*) IEN
C*          -- Write the entry to list file
            IF(MT.EQ.5) THEN
              MT=9000+LFS
c...          MF=3
            ELSE
              CALL MTTOZA(MT,IZP,IZO)
C...          MT=MT+1000*(10+LFS)
c...          MF=3
            END IF
            IDX=IDX+1
            WRITE(LLS,914)IZ,Nuc(IZ+1),IA,MST,IZO,MF,MT,IEN,CH10,IDX,IZP
            MT0=MT
            MF0=MF
            NN =1+(IEN+2)/3
            DO J=1,NN
              READ (LEN,901) C66
            END DO
          END DO
        ELSE IF(MF.EQ.4) THEN
C*
C* MF4
          MT0=MT
          MF0=MF
C*        -- Check for the presence of the transformation matrix
          READ (C66(23:33),*) LVT
          READ (C66(34:44),*) LTT1
          IF(LTT1.EQ.0) GO TO 20
          IF(LTT1.NE.2) THEN
            PRINT *,'WARNING - Angular distributions not pointwise'
            GO TO 20
          END IF
          READ (LEN,901) C66
          IF(LVT.EQ.1) THEN
C*        -- Skip the transformation matrix, if given
            READ (C66(45:55),*) NK
            JK=(NK+5)/6
            DO J=1,JK
              READ (LEN,901) C66
            END DO
          END IF
C*        -- Incident particle energy and level
          JLV=0
          DO I=1,NLV
            IF(MT.EQ.MTL(I)) JLV=ILV(I)
          END DO
          ELV=JLV
          READ (LEN,901) C66,MAT,MF,MT
C*        -- Read number of points
          READ (C66(45:55),*) NR
          READ (C66(56:66),*) IEN
C*            Skip interpolation law
            READ (LEN,901) C66,MAT,MF,MT
          DO I=1,IEN
C*            Read the energy
            READ (LEN,901) C66,MAT,MF,MT
            READ (C66(12:22),*) ENR
            READ (C66(56:66),*) NP
C*            Skip the TAB1 interpolation law
            READ (LEN,901) C66,MAT,MF,MT
C*            Skip the TAB1 data
            JS=(NP*2+5)/6
            DO J=1,JS
              READ (LEN,901) C66,MAT,MF,MT
            END DO
            WRITE(CH10(1),'(1P,E10.3E1)') ENR
            IF(JLV.NE.0) WRITE(CH10(3),'(1P,E10.4E1)') ELV
            IDX=IDX+1
            CALL MTTOZA(MT,IZP,IZO)
            WRITE(LLS,914)IZ,Nuc(IZ+1),IA,MST,IZO,MF,MT,IEN,CH10,IDX,IZP
          END DO
        ELSE IF(MF.EQ.5) THEN
C*
C* - MF 5 incident particle energy and outgoing particle angle
          MT0=MT
          MF0=MF
C*        -- Number of partal energy distributions
          READ (C66(45:55),*) NK
C*        -- Pick incident energies from first distribution only
C*        -- Read data representation flag
          READ (LEN,901) C66
          READ (C66(34:44),*) LF
C*        -- Skip weights interpolation law
          READ (C66(45:55),*) NR
          READ (C66(56:66),*) NE
          NN=(NR+2)/3 + (NE+2)/3
          DO I=1,NN
            READ (LEN,901) C66
          END DO
          IF(LF.EQ.1) THEN
            READ (LEN,901) C66
            READ (C66(45:55),*) NR
            READ (C66(56:66),*) NE
            NN=(NR+2)/3
            DO I=1,NN
              READ (LEN,901) C66
            END DO
            IEN=NE
            DO I=1,NE
              READ (LEN,901) C66
              READ (C66(12:22),*) ENR
              READ (C66(45:55),*) NR
              READ (C66(56:66),*) NF
              NN=(NR+2)/3 + (NF+2)/3
              DO J=1,NN
                READ (LEN,901) C66
              END DO
              WRITE(CH10(1),'(1P,E10.3E1)') ENR
              WRITE(LLS,914)IZ,Nuc(IZ+1),IA,MST,IZO,MF,MT,IEN
     &                      ,CH10,IDX,IZP
            END DO
          ELSE IF(LF.EQ. 5 .OR. LF.EQ. 7 .OR. LF.EQ.9 .OR.
     &            LF.EQ.11 .OR. LF.EQ.12) THEN
            IF(NE.GT.MXEN) STOP 'PLTLEN ERROR - MXEN limit exceeded'
            READ (LEN,905) (ENIN(J),DMY, J=1,NE)
            IEN=NE
            DO I=1,NE
              ENR=ENIN(I)
              WRITE(CH10(1),'(1P,E10.3E1)') ENR
              WRITE(LLS,914)IZ,Nuc(IZ+1),IA,MST,IZO,MF,MT,IEN
     &                      ,CH10,IDX,IZP
            END DO
          ELSE
            PRINT *,'PLTLEN WARNNG - MF 5 Illegal LF',LF
          END IF
        ELSE IF(MF.EQ.6) THEN
C*
C* - MF 6 Double-differential cross sections
          MT0=MT
          MF0=MF
C*        -- Number of particles
          READ (C66(45:55),*) NK
C*        -- Data representation flag, particle
          READ (LEN,901) C66
          READ (C66(34:44),*) LAW
          READ (C66( 1:11),*) ZAO
          IZO=NINT(ZAO)
          IF(LAW.EQ.5) THEN
C*        -- Process charged particle distribution LAW=5
C*           List cross sections at fixed angles
             MF=4
             MT=MT+40000
             IEN=1
             DO J=1,MXAN
               IDX=IDX+1
               CH10(1)='          '
               WRITE(CH10(2),'(F8.2)') ANG6(J)
               WRITE(LLS,914)IZ,Nuc(IZ+1),IA,MST,IZO,MF,MT,IEN
     &                       ,CH10,IDX,IZP
              END DO
          ELSE IF(LAW.EQ.7) THEN
C*          -- Skip multiplicities
            READ (C66(45:55),*) NR
            READ (C66(56:66),*) NP
            NN=(NR+2)/3 + (NP+2)/3
            DO I=1,NN
              READ (LEN,901) C66
            END DO
C*          -- Read number of incident energies
            READ (LEN,901) C66
            READ (C66(45:55),*) NR
            READ (C66(56:66),*) NE
C*          -- Skip energy interpolation law
            NN=(NR+2)/3
            DO I=1,NN
              READ (LEN,901) C66
            END DO
C*          -- Loop over energies
            DO I=1,NE
C*            -- Read energy
              READ (LEN,901) C66
              READ (C66(12:22),*) ENR
              READ (C66(45:55),*) NR
              READ (C66(56:66),*) NC
              NN=(NR+2)/3
              DO J=1,NN
                READ (LEN,901) C66
              END DO
              DO J=1,NC
C*              -- Skip the distribution for each cosine
                READ (LEN,901) C66
                READ (C66(45:55),*) NR
                READ (C66(56:66),*) NP
                NN=(NR+2)/3 + (NP+2)/3
                DO K=1,NN
                  READ (LEN,901) C66
                END DO
              END DO
C*            -- Make an entry to LST file
              DO J=1,MXAN
                IDX=IDX+1
                WRITE(CH10(1),'(1P,E10.3E1)') ENR
                WRITE(CH10(2),'(F8.2)') ANG6(J)
                WRITE(LLS,914)IZ,Nuc(IZ+1),IA,MST,IZO,MF,MT,IEN
     &                        ,CH10,IDX,IZP
              END DO
            END DO
          END IF
        END IF
      END IF
      GO TO 20
C* All processing completed
   80 WRITE(LLS,910)
      STOP 'PLTLEN Completed'
C*
  901 FORMAT(A66,I4,I2,I3,I5)
  903 FORMAT(2A40)
  905 FORMAT(6F11.0)
  910 FORMAT(' ======================================='
     &      ,'====================================  ======')
  912 FORMAT(' MATERIAL   ZAOUT  MF   MT  EVAL. EXPR. '
     &      ,'EXPR.    E-INC ANG-OUT ELV/E-OUT IDX    PROJ'/
     &       '                            PNTS. PNTS. '
     &      ,' REF.       EV     DEG        EV    ')
  914 FORMAT(I3,'-',A2,'-',I3,A1,I6,I4,I5,I6,6X,6X,A10,A8,A10,I4,I8)
      END
      SUBROUTINE MTTOZA(MT,IZI,IZO)
C-Title  : Subroutine MTTOIZO
C-Purpose: Define ZA of outgoing particle from MT
      IF(MT.EQ.2) THEN
        IZO=IZI
      ELSE IF(MT.EQ.  4) THEN
        IZO=1
      ELSE IF(MT.EQ.  5) THEN
        IZO=1
      ELSE IF(MT.EQ. 16) THEN
        IZO=1
      ELSE IF(MT.EQ. 17) THEN
        IZO=1
      ELSE IF(MT.GT. 50 .AND. MT.LE.91) THEN
        IZO=1
      ELSE IF(MT.EQ.103) THEN
        IZO=1001
      ELSE IF(MT.EQ.104) THEN
        IZO=1002
      ELSE IF(MT.EQ.105) THEN
        IZO=1003
      ELSE IF(MT.EQ.106) THEN
        IZO=2003
      ELSE IF(MT.EQ.107) THEN
        IZO=2004
      ELSE IF(MT.EQ.201) THEN
        IZO=1
      ELSE IF(MT.EQ.203) THEN
        IZO=1001
      ELSE IF(MT.EQ.204) THEN
        IZO=1002
      ELSE IF(MT.EQ.205) THEN
        IZO=1003
      ELSE IF(MT.EQ.206) THEN
        IZO=2003
      ELSE IF(MT.EQ.207) THEN
        IZO=2004
      ELSE IF(MT.EQ.452) THEN
        IZO=   1
      ELSE IF(MT.EQ.456) THEN
        IZO=   1
      ELSE IF(MT.GE.600 .AND. MT.LE.649) THEN
        IZO=1001
      ELSE IF(MT.GE.800 .AND. MT.LE.849) THEN
        IZO=2004
      ELSE
        IZO=0
      END IF
      RETURN
      END
