      PROGRAM EMPTUN
C-Title  : EMPTUN Program
C-Purpose: Set energy grid and tuning parameters in EMPIRE input
C-Author : A. Trkov, Jozef Stefan Institute, Ljubljana, Slovenia
C-Version: April 2013
C-V 2014/01  Major revision to include the calculation of tuning 
C-V          factors directly from the data (previously done with
C-V          the FLUCTU code).
C-M  
C-M  Manual for Program EMPTUN
C-M  =========================
C-M  In case that EMPIRE is to follow a detailed structure in the
C-M  cross sections based on experimental data, the need may arise
C-M  to introduce tuning parameters on a relatively dense energy
C-M  mesh. One of the options allows the tuning factors FUSRED, ELARED
C-M  and CELRED to be given in an external data file on a fixed
C-M  energy grid. Alternatively, the coarse-mesh cross sections and
C-M  the desired energy grid are read in and the tuning factors are
C-M  calculated according to three different options:
C-M
C-M  1. All cross sections are scaled proportionally so that the
C-M     total cross section matches the measured total cross section.
C-M
C-M  2. The non-elastic cross sections remain fixed by the model,
C-M     but the elastic is tuned so that the total cross section
C-M     matches the smoothed measured one. The ratio of the shape-
C-M     elastic and compound elastic is adjusted so that the average
C-M     cosine of scattering remains as predicted by the model.
C-M
C-M  3. When coarse-mesh elastic cross sections and the average
C-M     cosine of scattering are available, the shape-elastic and
C-M     the compound-elastic cross sections are adjusted to match
C-M     the measured elastic cross section and the average cosine of
C-M     scattering. The remainder if the difference from the
C-M     smoothed total cross section is absorbed by the non-elastic 
C-M     cross section.
C-M
C-M  The tuning factors are calculated as described in the paper
C-M  presented at the ND2013 conference in New York.
C-M
C-M  The EMPIRE input file is scanned and the tuning
C-M  factors are introduced on the given energy mesh, leaving the
C-M  rest of the file unchanged.
C-M  
C-M  Instructions (SORRY, THE INSTRUCTIONS NEED UPDATING!!!)
C-M  The following input parameters are entered from input:
C-M   - FLIN  The tuning factor filename; this can be an input
C-M           file in which the tuning factors are given in the
C-M           format described below. Alternatively, the tuning
C-M           factors are calculated from the cross sections
C-M           internally, in which case FLIN is the output file
C-M           containing the tuning factors inserted into the
C-M           EMPIRE input file.
C-M             If the tuning factors are given on input, they
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
C-M           These are followed by the reaction cross section
C-M           (identified by the header "Sig_r") and the 
C-M           compound-elastic cross section (identified by 
C-M           the header "Sig_CE") from the EMPIRE reference
C-M           calculation. Definitions:
C-M             FUSRED factors account for the change from the
C-M                    reference EMPIRE calculation to the desired
C-M                    reaction cross section value (name FUSRED2).
C-M             ELARED factors definition is as for FUSRED,
C-M                    referring to the shape-elastic cross sections.
C-M                    (name ELARED2)
C-M             CELRED factors determine the fraction of compound-
C-M                    elastic cross setion in the desired value
C-M                    of the reaction cross section (name CELDSH2).
C-M             Sig_r  cross sections represent the EMPIRE nominal
C-M                    calculation of the reaction cross section
C-M                   (name SIGFUS1).
C-M             Sig_CE cross sections represent the EMPIRE nominal
C-M                    calculation of the compound-elastic cross
C-M                    section (name SIGCEL1)
C-M   - FLEI  Source EMPIRE input filename (to be modified).
C-M   - FLOU  Updated EMPIRE input.
C-M   - ... cross section files...
C-M   - ITUN  Tuning option
C-M   - ELO   Lower energy bound for tuning [MeV]
C-M   - EHI   Upper energy bound for tuning [MeV]
C-M
C-M  NOTE:
C-M  The present code is an extension of the previous version, which
C-M  did not support the calculation of the tuning factors from the 
C-M  cross sections. The present version is backward compatible.
C-
      PARAMETER    (MXEN=2000,MXCOM=10,MXSTR=4,MXTUN=3)
      CHARACTER*6   LABEL(5)
      CHARACTER*10  TSTR,CALSTR(MXSTR)
      CHARACTER*40  BLNK,HEDR,HOPT(MXTUN)
      CHARACTER*80  FLNM,FLIN,FLEI,FLOU,FLLS
     &             ,FLXT,FLXE,FLXM,FLEG,FLCX,FLCU
      CHARACTER*120 REC,RECM(MXCOM)
     &             ,HTOTRED,HFUSRED,HFCCRED,HFCORED,HELARED,HCELRED
C*
      DIMENSION    XCL(MXEN,4)
      DIMENSION    ENC(MXEN),PRC(MXEN,5)
     &            ,EXT(MXEN),XXT(MXEN),EXE(MXEN),XXE(MXEN)
     &            ,EXM(MXEN),XXM(MXEN),EEG(MXEN),XEG(MXEN)
     &            ,ECX(MXEN),XCX(MXEN)
     &            ,XTT(MXEN),XEL(MXEN),XCE(MXEN),XSE(MXEN)
     &            ,XRE(MXEN),XMU(MXEN)
     &            ,TOTR(MXEN),FUSR(MXEN),ELAR(MXEN),CELR(MXEN)
      EQUIVALENCE (XCL(1,1),XTT(1)),(XCL(1,2),XEL(1)),(XCL(1,3),XCE(1))
     &           ,(XCL(1,4),XMU(1))
C* Data set labels and flags
      DIMENSION    KFLG(5)
      DATA KFLG/ 0, 0, 0, 0, 0 /
      DATA LABEL/'FUSRED','ELARED','CELRED','Sig_r ','Sig_CE' /
C* Filenames and logical file units
      DATA LIN,LEI,LOU,LLS,LKB,LTT,LCU,LEG,LXT,LXE,LXM,LCX
     &     / 1,  2,  3,  4,  5,  6,  7, 11, 12, 22, 32, 52 /
      DATA BLNK/'                                        '/
     1    ,FLIN/'inp.dat'/
     2    ,FLEI/'emp.inp'/
     3    ,FLOU/'emp.out'/
     4    ,FLLS/'emptun.lst'/
     7    ,FLCU/'emptun.cur'/
     1    ,FLXT/'total_lo_mod.cur'/
     2    ,FLXE/'smolin_el_mod.cur'/
     3    ,FLXM/'smolin_mu_mod.cur'/
     4    ,FLEG/'egrid.cur'/
     6    ,FLCX/'empire.cur'/
C*
      DATA CALSTR/'MF 3 MT  1'
     &           ,'MF 3 MT  2'
     &           ,'MF 3 MT 50'
     &           ,'MF 3 MT251'/
C*
      DATA HOPT/' Scale all x.s. to match Sig_tot(m)   '
     1         ,' Match Sig_tot(m), keep mu(c),Sig_x(c)'
     2         ,' Match Sig_tot(m), Sig_el(m), mu(m)   '/
C*
      EPS =1.E-5
C*
C* Write banner to terminal and log file
      OPEN(UNIT=LLS,FILE=FLLS,STATUS='UNKNOWN')
      OPEN(UNIT=LCU,FILE=FLCU,STATUS='UNKNOWN')
      WRITE(LTT,901) ' '
      WRITE(LTT,901) ' EMPTUN - Set EMPIRE tuning parameters  '
      WRITE(LTT,901) ' =====================================  '
      WRITE(LTT,901) ' '
      WRITE(LLS,901) ' EMPTUN - Set EMPIRE tuning parameters  '
      WRITE(LLS,901) ' =====================================  '
      WRITE(LLS,901) ' '
C* Initialise
      NXT =0
      NXE =0
      NXM =0
      NEG =0
      NCX =0
      INR =2
C*
      KTUN=-1
      ITUN= 1
      ELO = 1.0E-5
      EHI =20.0E+6
C*
C* Define the tuning factor file
      WRITE(LTT,901) ' Default source tuning fact. filename : ',FLIN
      WRITE(LTT,901) '$          Enter new name to redefine : '
      READ (LKB,900) FLNM
      IF(FLNM(1:40).NE.BLNK) FLIN=FLNM
      OPEN(UNIT=LIN,FILE=FLIN,STATUS='UNKNOWN')
C* Define the source and tuned EMPIRE inputs
   20 WRITE(LTT,901) ' Default source EMPIRE input filename : ',FLEI
      WRITE(LTT,901) '$          Enter new name to redefine : '
      READ (LKB,900) FLNM
      IF(FLNM(1:40).NE.BLNK) FLEI=FLNM
      OPEN(UNIT=LEI,FILE=FLEI,STATUS='OLD',ERR=20)
C* Define the output file
      WRITE(LTT,901) ' Default updated EMPIRE input         : ',FLOU
      WRITE(LTT,901) '$          Enter new name to redefine : '
      READ (LKB,900) FLNM
      IF(FLNM(1:40).NE.BLNK) FLOU=FLNM
      OPEN (UNIT=LOU,FILE=FLOU,STATUS='UNKNOWN')
C*
C* Define the source files of measured data
   30 WRITE(LTT,901) ' Measured total x.s. filename         : ',FLXT
      WRITE(LTT,901) '$          Enter new name to redefine : '
      READ (LKB,900,END=88) FLNM
      IF(FLNM(1:40).NE.BLNK) FLXT=FLNM
      IF(FLNM(1: 1).EQ. '-') GO TO 40
      OPEN(UNIT=LXT,FILE=FLXT,STATUS='OLD',ERR=30)
   32 READ (LXT,901,ERR=30) FLNM
      HEDR=FLNM(1:40)
      WRITE(LTT,901) ' Total x.s.  : '//FLXT
      WRITE(LTT,901) ' Header      : '//FLNM
      WRITE(LLS,901) ' Total x.s.  : '//FLXT
      WRITE(LLS,901) ' Header      : '//FLNM
   34 READ (LXT,901,END=36) FLNM
      IF(FLNM(1:40).EQ.BLNK) GO TO 36
      READ (FLNM, * ) EE,XX
      NXT=NXT+1
      IF(NXT.GT.MXEN) THEN
        WRITE(LTT, * ) ' EMPTUN ERROR - Reading ',FLXT
        WRITE(LLS, * ) ' EMPTUN ERROR - Reading ',FLXT
        STOP 'EMPTUN ERROR - MXEN limit exceeded'
      END IF
      EXT(NXT)=EE
      XXT(NXT)=XX
      GO TO 34
   36 CLOSE(UNIT=LXT)
      WRITE(LTT,922) ' Energy range',EXT(1),EXT(NXT)
     &              ,' [eV]  Points',NXT
      WRITE(LTT,901) ' '
      WRITE(LLS,922) ' Energy range',EXT(1),EXT(NXT)
     &              ,' [eV]  Points',NXT
      WRITE(LLS,901) ' '
C*    --Save the data to the logging curves for QA
      WRITE(LCU,901) HEDR
      DO I=1,NXT
        WRITE(LCU,911) EXT(I),XXT(I)
      END DO
      WRITE(LCU,901) ' '
C*
   40 WRITE(LTT,901) ' Measured elastic x.s. filename       : ',FLXE
      WRITE(LTT,901) '$          Enter new name to redefine : '
      READ (LKB,900) FLNM
      IF(FLNM(1:40).NE.BLNK) FLXE=FLNM
      IF(FLNM(1: 1).EQ. '-') GO TO 50
      OPEN(UNIT=LXE,FILE=FLXE,STATUS='OLD',ERR=40)
      READ (LXE,901,ERR=40) FLNM
      WRITE(LTT,901) ' Elastic x.s.: '//FLXE
      WRITE(LTT,901) ' Header      : '//FLNM
      WRITE(LLS,901) ' Elastic x.s.: '//FLXE
      WRITE(LLS,901) ' Header      : '//FLNM
   44 READ (LXE,901,END=48) FLNM
      IF(FLNM(1:40).EQ.BLNK) GO TO 48
      READ (FLNM, * ) EE,XX
      NXE=NXE+1
      IF(NXE.GT.MXEN) THEN
        WRITE(LTT, * ) ' EMPTUN ERROR - Reading ',FLXE
        WRITE(LLS, * ) ' EMPTUN ERROR - Reading ',FLXE
        STOP 'EMPTUN ERROR - MXEN limit exceeded'
      END IF
      EXE(NXE)=EE
      XXE(NXE)=XX
      GO TO 44
   48 WRITE(LTT,922) ' Energy range',EXE(1),EXE(NXE)
     &              ,' [eV]  Points',NXE
      WRITE(LTT,901) ' '
      WRITE(LLS,922) ' Energy range',EXE(1),EXE(NXE)
     &              ,' [eV]  Points',NXE
      WRITE(LLS,901) ' '
C*
   50 WRITE(LTT,901) ' Measured mu-bar filename             : ',FLXM
      WRITE(LTT,901) '$          Enter new name to redefine : '
      READ (LKB,900) FLNM
      IF(FLNM(1:40).NE.BLNK) FLXM=FLNM
      IF(FLNM(1: 1).EQ. '-') GO TO 60
      OPEN(UNIT=LXM,FILE=FLXM,STATUS='OLD',ERR=50)
      READ (LXM,901,ERR=50) FLNM
      WRITE(LTT,901) ' mu-bar      : '//FLXM
      WRITE(LTT,901) ' Header      : '//FLNM
      WRITE(LLS,901) ' mu-bar      : '//FLXM
      WRITE(LLS,901) ' Header      : '//FLNM
   54 READ (LXM,901,END=58) FLNM
      IF(FLNM(1:40).EQ.BLNK) GO TO 58
      READ (FLNM, * ) EE,XX
      NXM=NXM+1
      IF(NXM.GT.MXEN) THEN
        WRITE(LTT, * ) ' EMPTUN ERROR - Reading ',FLXM
        WRITE(LLS, * ) ' EMPTUN ERROR - Reading ',FLXM
        STOP 'EMPTUN ERROR - MXEN limit exceeded'
      END IF
      EXM(NXM)=EE
      XXM(NXM)=XX
      GO TO 54
   58 WRITE(LTT,922) ' Energy range',EXE(1),EXE(NXE)
     &              ,' [eV]  Points',NXE
      WRITE(LTT,901) ' '
      WRITE(LLS,922) ' Energy range',EXE(1),EXE(NXE)
     &              ,' [eV]  Points',NXE
      WRITE(LLS,901) ' '
C*
C* Read the energy grid
   60 WRITE(LTT,901) ' Desired energy grid filename         : ',FLEG
      WRITE(LTT,901) '$          Enter new name to redefine : '
      READ (LKB,900) FLNM
      IF(FLNM(1:40).NE.BLNK) FLEG=FLNM
      IF(FLNM(1: 1).EQ. '-') GO TO 70
      OPEN(UNIT=LEG,FILE=FLEG,STATUS='OLD',ERR=60)
      READ (LEG,901,ERR=60) FLNM
      WRITE(LTT,901) ' Energy grid : '//FLEG
      WRITE(LTT,901) ' Header      : '//FLNM
      WRITE(LLS,901) ' Energy grid : '//FLEG
      WRITE(LLS,901) ' Header      : '//FLNM
   64 READ (LEG,901,END=68) FLNM
      IF(FLNM(1:40).EQ.BLNK) GO TO 68
      READ (FLNM, * ) EE
      NEG=NEG+1
      IF(NEG.GT.MXEN) THEN
        WRITE(LTT, * ) ' EMPTUN ERROR - Reading ',FLEG
        WRITE(LLS, * ) ' EMPTUN ERROR - Reading ',FLEG
        STOP 'EMPTUN ERROR - MXEN limit exceeded'
      END IF
      EEG(NEG)=EE
      GO TO 64
   68 IF(NEG.LE.0) THEN
        WRITE(LTT, * ) ' EMPTUN ERROR - Reading ',FLEG
        WRITE(LLS, * ) ' EMPTUN ERROR - Reading ',FLEG
        STOP 'EMPTUN ERROR - NEG=0'
      END IF
      WRITE(LTT,922) ' Energy range',EEG(1),EEG(NEG)
     &              ,' [eV]  Points',NEG
      WRITE(LTT,901) ' '
      WRITE(LLS,922) ' Energy range',EEG(1),EEG(NEG)
     &              ,' [eV]  Points',NEG
      WRITE(LLS,901) ' '
C*
C* Read the calculated cross sections
   70 WRITE(LTT,901) ' Calculated cross sections filename   : ',FLCX
      WRITE(LTT,901) '$          Enter new name to redefine : '
      READ (LKB,900) FLNM
      IF(FLNM(1:40).NE.BLNK) FLCX=FLNM
      IF(FLNM(1: 1).EQ. '-') GO TO 80
      OPEN(UNIT=LCX,FILE=FLCX,STATUS='OLD',ERR=70)
      WRITE(LTT,901) ' Calc. x.s.  : '//FLCX
      WRITE(LLS,901) ' Calc. x.s.  : '//FLCX
      NSTR=0
   73 IF(NSTR.GE.MXSTR) GO TO 80
      READ (LCX,901,ERR=70) FLNM
      TSTR=FLNM(10:19)
      NCX=0
   74 READ (LCX,901,END=60) FLNM
      IF(FLNM(1:40).EQ.BLNK) GO TO 76
      READ (FLNM, * ) EE,XX
      NCX=NCX+1
      IF(NCX.GT.MXEN) THEN
        WRITE(LTT, * ) ' EMPTUN ERROR - Reading ',FLCX
        WRITE(LLS, * ) ' EMPTUN ERROR - Reading ',FLCX
        STOP 'EMPTUN ERROR - MXEN limit exceeded'
      END IF
      ECX(NCX)=EE
      XCX(NCX)=XX
      GO TO 74
   76 DO I=1,MXSTR
        ISTR=I
        IF(TSTR.EQ.CALSTR(I)) GO TO 77
      END DO
      GO TO 73
   77 NSTR=NSTR+1
      WRITE(LTT,901) ' Header      : '//TSTR//BLNK
      WRITE(LTT,922) ' Energy range',ECX(1),ECX(NCX)
     &              ,' [eV]  Points',NCX
      WRITE(LTT,901) ' '
      WRITE(LLS,901) ' Header      : '//TSTR//BLNK
      WRITE(LLS,922) ' Energy range',ECX(1),ECX(NEG)
     &              ,' [eV]  Points',NCX
      WRITE(LLS,901) ' '
C*    -- Interpolate the calculated cross sections to the specified grid
C*       Skip the first energy if duplicated
      I1=1
      IF(ECX(I1).GE.ECX(I1+1)) THEN
        I1=I1+1
        NCX=NCX-1
      END IF
      CALL FITGRD(NCX,ECX(I1),XCX(I1),NEG,EEG,XCL(1,ISTR))
      GO TO 73
C*
C* Select the tuning options for different energy ranges
   80 WRITE(LTT,901) ' Select the tuning parameter option :   '
      DO I=1,MXTUN
        WRITE(LTT,914) I-1,HOPT(I)
      END DO
      READ (LKB,900) FLNM
      IF(FLNM(1:40) .NE. BLNK) READ (FLNM,*,ERR=80) KTUN
      ITUN=KTUN
   82 WRITE(LTT,901) '  Enter lower energy for tuning [MeV] : '
      READ (LKB,900) FLNM
      IF(FLNM(1:40) .NE. BLNK) THEN
        READ (FLNM,*,ERR=82) ELO
        ELO=ELO*1000000
      END IF
   84 WRITE(LTT,901) '  Enter upper energy for tuning [MeV] : '
      READ (LKB,900) FLNM
      IF(FLNM(1:40) .NE. BLNK) THEN
        READ (FLNM,*,ERR=84) EHI
        EHI=EHI*1000000
      END IF
      IF(ITUN.EQ.-1) ITUN=1
      GO TO 90
C*
C* Backward compatibility option - tuning parameters given on input
   88 FLXT=BLNK//BLNK
      FLXT(1:1)='-'
      FLXE=BLNK//BLNK
      FLXE(1:1)='-'
      FLXM=BLNK//BLNK
      FLXM(1:1)='-'
      FLEG=BLNK//BLNK
      FLEG(1:1)='-'
      FLCX=BLNK//BLNK
      FLCX(1:1)='-'
C*
   90 WRITE(LTT,901) ' '
      WRITE(LTT,901) ' SUMMARY of selected input options:     '
      WRITE(LTT,901) ' '
      WRITE(LTT,901) ' Measured total x.s. filename         : ',FLXT
      WRITE(LTT,901) ' Measured elastic x.s. filename       : ',FLXE
      WRITE(LTT,901) ' Measured mu-bar filename             : ',FLXM
      WRITE(LTT,901) ' Desired energy grid filename         : ',FLEG
      WRITE(LTT,901) ' Calculated cross sections filename   : ',FLCX
      WRITE(LTT,901) ' Source EMPIRE input filename         : ',FLEI
      WRITE(LTT,901) ' Updated EMPIRE input                 : ',FLOU
      WRITE(LTT,901) ' '
      WRITE(LTT,901) ' Tuning factors filename              : ',FLIN
      WRITE(LTT,901) ' '
      WRITE(LTT,901) ' Selected tuning option:                '
      WRITE(LTT,914) ITUN,HOPT(ITUN+1)
      WRITE(LTT,903) '                    Tuning range [eV] : ',ELO
      WRITE(LTT,903) '                                        ',EHI
C*
C* Reconstruct the derived (calculated) cross sections
      DO i=1,NEG
        XSE(I)=XEL(I)-XCE(I)
        XRE(I)=XTT(I)-XSE(I)
      END DO
C*
C* BEGIN TUNING FACTOR CALCULATION
C*
      IF     (KTUN.LT.0) THEN
C* CASE: Tuning factors defined externally on FLIN
        GO TO 98
      ELSE IF(KTUN.EQ.0) THEN
C* CASE: All cross sections tuned proportionally
C*      -- Check that the measured total cross section is available
        IF(NXT.LE.0) THEN
          WRITE(LTT,*) 'ERROR - Measured total cross section not given'
          WRITE(LLS,*) 'ERROR - Measured total cross section not given'
          STOP 'EMPTUN ERROR - Insufficient input data'
        END IF
        IF(EXT(1).GT.ELO .OR. EXT(NXT).LT.EHI) THEN
          WRITE(LTT,*) 'WARNING - Requested tuning range is outside '
     &                ,'tabulated data',EXT(1),EXT(NXT)
        END IF
        WRITE(LIN,*) 'TOTRED'
        DO I=1,NEG
          EE=EEG(I)
          IF(EE.GE.ELO .AND. EE.LE.EHI) THEN
            FF=FINTXS(EE,EXT,XXT,NXT,INR,IER)/XTT(I)
            TOTR(I)=FF
            WRITE(LIN,911) EE,FF
          END IF
        END DO
        WRITE(LIN,*) ' '
        REWIND LIN
      ELSE IF(KTUN.EQ.1) THEN
C* CASE: Measured total cross section given,
C*      Preserve calculated mu-bar and non-elastic
C*      -- Check that the measured total cross section is available
        IF(NXT.LE.0) THEN
          WRITE(LTT,*) 'ERROR - Measured total cross section not given'
          WRITE(LLS,*) 'ERROR - Measured total cross section not given'
          STOP 'EMPTUN ERROR - Insufficient input data'
        END IF
        IF(EXT(1).GT.ELO .OR. EXT(NXT).LT.EHI) THEN
          WRITE(LTT,903) ' WARNING - Requested tuning range [eV] :'
     &                  ,ELO,EHI
          WRITE(LTT,903) '           is outside tabulated data    '
     &                  ,EXT(1),EXT(NXT)
          ELO=MAX(ELO,EXT(1))
          EHI=MIN(EHI,EXT(NXT))
        END IF
        I1=0
        I2=0
        DO I=1,NEG
          EE=EEG(I)
          IF(EE.GE.ELO .AND. EE.LE.EHI) THEN
            XTOTM=FINTXS(EE,EXT,XXT,NXT,INR,IER)
C*
            XNONC  = XTT(I)- XEL(I)
            XSELC  = XEL(I)- XCE(I)
            XFUSC  = XTT(I)- XSELC
C*
            XCELM  =(XTOTM - XNONC)*XCE(I)/XEL(I)
            XSELM  = XCELM * XSELC /XCE(I)
            XFUSM  = XTOTM - XSELM
C*
            FUSR(I)= XFUSM/XFUSC
            ELAR(I)= XSELM/XSELC
            FCE    = XCELM/XCE(I)
C*          -- Correction for FUSRRED
            FCE    = FCE/FUSR(I)
C*          -- Take into account the internal EMPIRE definition
C...
C... What I think it should be (definition is unclear)
C...        FCE    = FCE*(XFUSC-XCE(I))/(XFUSC-FCE*XCE(I))
C... What is programmed in FLUCTU ???
            FCE    = XCELM/XFUSM
C...
            CELR(I)= FCE
            IF(I1.EQ.0) I1=I
            I2=I
          END IF
        END DO
        IF(I1.EQ.0) THEN
          WRITE(LTT,903) ' ERROR - No data in tuning range [eV]  :'
     &                  ,ELO,EHI
          STOP 'EMPTUN ERROR - No data in range'
        END IF
C*
        WRITE(LIN,901) 'FUSRED'//BLNK
        DO I=I1,I2
          WRITE(LIN,912) EEG(I),FUSR(I)
        END DO
        WRITE(LIN,901) ' '
        WRITE(LIN,901) 'ELARED'//BLNK
        DO I=I1,I2
          WRITE(LIN,912) EEG(I),ELAR(I)
        END DO
        WRITE(LIN,901) ' '
        WRITE(LIN,901) 'CELRED'//BLNK
        DO I=I1,I2
          WRITE(LIN,912) EEG(I),CELR(I)
        END DO
        WRITE(LIN,901) ' '
        WRITE(LIN,901) 'Sig_r '//BLNK
        DO I=I1,I2
          WRITE(LIN,912) EEG(I),XRE(I)
        END DO
        WRITE(LIN,901) ' '
        WRITE(LIN,901) 'Sig_CE'//BLNK
        DO I=I1,I2
          WRITE(LIN,912) EEG(i),XSE(I)
        END DO
        WRITE(LIN,901) ' '
        REWIND LIN
      ELSE IF(KTUN.EQ.2) THEN
C* CASE: Measured total & elastic cross sections and mu-bar given,
C*      Preserve calculated mu-bar and non-elastic
C*      -- Check that the measured total cross section is available
        IF(NXT.LE.0 .OR. NXE.LE.0 .OR. NXM.LE.0) THEN
          WRITE(LTT,*) 'ERROR - NO measured total, elastic or mu-bar'
          WRITE(LLS,*) 'ERROR - NO measured total, elastic or mu-bar'
          STOP 'EMPTUN ERROR - Insufficient input data'
        END IF
        ULO=MAX(EXT(  1),EXE(  1),EXM( 1 ))
        UHI=MIN(EXT(NXT),EXE(NXE),EXM(NXM))
        IF(ULO.GT.ELO .OR. UHI.LT.EHI) THEN
          WRITE(LTT,903) ' WARNING - Requested tuning range [eV] :'
     &                  ,ELO,EHI
          WRITE(LTT,903) '      is outside tabulated data total   '
     &                  ,EXT(1),EXT(NXT)
          WRITE(LTT,903) '                              elastic   '
     &                  ,EXE(1),EXE(NXE)
          WRITE(LTT,903) '                            or mu-bar   '
     &                  ,EXM(1),EXM(NXM)
          ELO=MAX(ELO,ULO)
          EHI=MIN(EHI,UHI)
        END IF
        IF(EHI.LE.ELO) THEN
          WRITE(LTT,903) ' ERROR - in requested tuning range      '
          STOP 'ENDTUN ERROR - Insufficient data or wrong energy range'
        END IF
        I1=0
        I2=0
        DO I=1,NEG
          EE=EEG(I)
          IF(EE.GE.ELO .AND. EE.LE.EHI) THEN
            XTOTM=FINTXS(EE,EXT,XXT,NXT,INR,IER)
            XELSM=FINTXS(EE,EXE,XXE,NXE,INR,IER)
            XMUBM=FINTXS(EE,EXM,XXM,NXM,INR,IER)
C*
            XSELC  = XEL(I)- XCE(I)
            XFUSC  = XTT(I)- XSELC
            XMSEC  = XMU(I)* XEL(I)/XSELC
C*
            XSELM  = XELSM * XMUBM /XMSEC
            XCELM  = XELSM - XSELM
            XFUSM  = XTOTM - XSELM
C...
C...        print *,'xtotm,xelsm,xmubm,xselm,xcelm,xfusm'
C...        print *, xtotm,xelsm,xmubm,xselm,xcelm,xfusm
C...        print *,'xtt,xel,xmu,xce,xselc,xfusc'
C...        print *, xtt(i),xel(i),xmu(i),xce(i),xselc,xfusc
C...        stop
C...
C*
            FUSR(I)= XFUSM/XFUSC
            ELAR(I)= XSELM/XSELC
            FCE    = XCELM/XCE(I)
C*          -- Correction for FUSRRED
            FCE    = FCE/FUSR(I)
C*          -- Take into account the internal EMPIRE definition
C...
C... What I think it should be (definition is unclear)
C...        FCE    = FCE*(XFUSC-XCE(I))/(XFUSC-FCE*XCE(I))
C... What is programmed in FLUCTU ???
            FCE    = XCELM/XFUSM
C...
            CELR(I)= FCE
C*          -- Check for unreasonable values
            IF(FUSR(I).LT.0 .OR. ELAR(I).LT.0 .OR. CELR(I).LT.0) THEN
              WRITE(LTT, * ) ' WARNING - Unreasonable tuning factors at'
     &                      ,' Energy [eV]',EE
              WRITE(LTT,924) ' FUSRED',FUSR(I),' ELARED',ELAR(I)
     &                      ,' CELRED',CELR(I)
              FUSR(I)=MAX(FUSR(I),0.)
              ELAR(I)=MAX(ELAR(I),0.)
              CELR(I)=MAX(CELR(I),0.)
            END IF
            IF(I1.EQ.0) I1=I
            I2=I
          END IF
        END DO
        IF(I1.EQ.0) THEN
          WRITE(LTT,903) ' ERROR - No data in tuning range [eV]  :'
     &                  ,ELO,EHI
          STOP 'EMPTUN ERROR - No data in range'
        END IF
C*
        WRITE(LIN,901) 'FUSRED'//BLNK
        DO I=I1,I2
          WRITE(LIN,912) EEG(I),FUSR(I)
        END DO
        WRITE(LIN,901) ' '
        WRITE(LIN,901) 'ELARED'//BLNK
        DO I=I1,I2
          WRITE(LIN,912) EEG(I),ELAR(I)
        END DO
        WRITE(LIN,901) ' '
        WRITE(LIN,901) 'CELRED'//BLNK
        DO I=I1,I2
          WRITE(LIN,912) EEG(I),CELR(I)
        END DO
        WRITE(LIN,901) ' '
        WRITE(LIN,901) 'Sig_r '//BLNK
        DO I=I1,I2
          WRITE(LIN,912) EEG(I),XRE(I)
        END DO
        WRITE(LIN,901) ' '
        WRITE(LIN,901) 'Sig_CE'//BLNK
        DO I=I1,I2
          WRITE(LIN,912) EEG(i),XSE(I)
        END DO
        WRITE(LIN,901) ' '
        REWIND LIN
      ELSE
C* CASE: Illegal entry
        WRITE(LTT,*) 'ERROR - No coding for tuning factor',KTUN
        WRITE(LTT,*) 'EMPTUN ERROR - Invalid tuning factor'
      END IF
C*
C* START READING THE DESIRED TUNING FACTORS
C*
   98 KC =0
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
  903 FORMAT(A40,1P,2E10.3)
  908 FORMAT( F8.4)
  910 FORMAT(F10.4)
  911 FORMAT(1P,E11.4,0P,F11.4)
  912 FORMAT(1P,2E11.5E1)
  914 FORMAT(I6,A40)
  922 FORMAT(A,1P,2E10.3,A,I6)
  924 FORMAT(6X,3(A,1P,E9.2E1))
      END
      SUBROUTINE SMODAT(NXC,XSC,NXF,XSF,SCL,BVL,RWO,MXR)
C-Title  : SMODAT Subroutine
C-Purpose: Smooth a function so that the integral is preserved
C-Description:
C-D  An array XSF(i,1) contains NXF fine mesh argument values and XSF(i,2)
C-D  contains the corresponding function values. Averaging is done to a
C-D  coarse grid of NXC values contained in XSC(j,1) and the function
C-D  values are stored in XSC(j,2). The averages over double coarse mesh
C-D  intervals are preserved. The values at the double-mesh boundaries
C-D  are linear averages of the neighbouring coarse mesh averages. The
C-D  mesh points internal to the double intervals are adjusted to preserve
C-D  the double-mesh integrals.
C-D    The treatment of the endpoints of the total argument interval
C-D  are by default equal to the interpolated fine mesh function
C-D  values. If BVL flag is set to -1, the function boundary values
C-D  are adopted
C-D
C-D  NOTE: The number of coarse mesh points must be odd (i.e. even number
C-D        of coarse mesh intervals).
C-
      DIMENSION XSC(NXC,2),XSF(NXF,2),RWO(*)
C* Linear interpolation function
      FINT(X,XA,YA,XB,YB)=YA+(YB-YA)*(X-XA)/(XB-XA)
C     IF((NXC+1)/2.EQ.NXC/2) STOP 'SMODAT ERROR - NXC must be odd'
C* Check scratch field size
      IF(2*NXC.GT.MXR) STOP 'ERROR in SMODAT - MXR limit exceeded'
C* Initialize
      ST =0.
      JF =1
      FX1=XSF(JF,1)
      FY1=XSF(JF,2)
      FX2=FX1
      FY2=FY1
      CX1=XSC( 1,1)
C* Find the interval for the first point
   20 IF(CX1.LE.FX2) GO TO 22
        FX1=FX2
        FY1=FY2
        JF =JF+1
        IF(JF.GT.NXF) RETURN
        FX2=XSF(JF,1)
        FY2=XSF(JF,2)
        GO TO 20
C* First point on the coarse mesh by interpolation
   22 IF(CX1.GT.FX1) FY1=FINT(CX1,FX1,FY1,FX2,FY2)
      FYA=FY1
      FX1=CX1
      SY =0.
C* Loop over all intervals - pack average from RWO(2)
      DO 60 IC=2,NXC
      RWO(IC)=0.
      CX2=XSC(IC,1)
      IF(CX2.LT.FX1)                 GO TO 60
      IF(CX2.GT.FX2 .AND. JF.GT.NXF) GO TO 60
   30 IF(FX2.GT.CX2) GO TO 40
C* Add the fine mesh contribution to the integral
      SY =SY + 0.5*(FY2+FY1)*(FX2-FX1)
      FX1=FX2
      FY1=FY2
      JF =JF+1
      IF(JF.GT.NXF) GO TO 42
      FX2=XSF(JF,1)
      FY2=XSF(JF,2)
      FYB=FY2
      GO TO 30
C* Complete the integral and store in work array
   40 FY =FINT(CX2,FX1,FY1,FX2,FY2)
      SY =SY + 0.5*(FY +FY1)*(CX2-FX1)
      FX1=CX2
      FY1=FY
      FYB=FY
   42 RWO(IC)=SY/(CX2-CX1)
      CX1=CX2
      ST =ST +SY
      SY =0.
   60 CONTINUE
C* From average values calculate the boundary values, from RWO(NXC+1)
      HB=XSC(2   ,1)-XSC(1   ,1)
      YB=RWO(2)
      DO 80 IC=3,NXC
      HA=HB
      YA=YB
      HB=XSC(IC  ,1)-XSC(IC-1,1)
      YB=RWO(IC)
      RWO(NXC+IC-1)=(YB*HA+YA*HB)/(HA+HB)
   80 CONTINUE
C* End points - extrapolate OR set to end values
      IF(BVL.EQ.0)  FYA=2.*RWO(  2)-RWO(NXC  +2)
      RWO(NXC+1   )=FYA
      IF(BVL.EQ.0)  FYB=2.*RWO(NXC)-RWO(NXC+NXC-1)
      RWO(NXC+NXC )=FYB
C* Adjust every other point value to preserve the integrals
C* at least over double intervals
      NX1=NXC-1
      DO 82 IC=2,NX1,2
      H1=XSC(IC  ,1)-XSC(IC-1,1)
      H2=XSC(IC+1,1)-XSC(IC  ,1)
      S2=RWO(IC)*H1 +RWO(IC+1)*H2
      RWO(NXC+IC)=(2.*S2-H1*RWO(NXC+IC-1)-H2*RWO(NXC+IC+1))/(H1+H2)
   82 CONTINUE
      IF(NX1/2 .NE. NXC/2) RWO(NXC+NXC)=2.*RWO(NXC)-RWO(NXC+NXC-1)
C* Add the scaled contribution to the output array
      DO 88 IC=1,NXC
      XSC(IC,2)=XSC(IC,2)+SCL*RWO(NXC+IC)
   88 CONTINUE
      RETURN
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
      IF     (EIN.LT.EN(1)) THEN
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
