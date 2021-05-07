      PROGRAM RR_UNC
C-Title  : Program RR_UNC
C-Program: Calculate uncertainties in reaction rates and x.sect.
C-Author : A. Trkov, Jozef Stefan Institute, Ljubljana, Slovenia
C-Version: 2011 Original code
C-V  12/01 Refine axes selection in PLOTTAB input file
C-V  12/04 - Search also by IZAP and LFS (MF10/40).
C-V        - Increase MXM from 1000 to 20000.
C-V        - Allow metastable targets.
C-V  13/07 Skip normalisation if a single reaction is processed.
C-V  13/08 - Refine PLOTTAB input when selected reactions are plotted.
C-V        - Clarify input, print version to screen
C-V  13/10 Fix trivial bug (NBT, INR dimension declaration, print
C-V        control, etc.).
C-V  13/11 Allow multiple reactions for selective printout.
C-V  14/02 - Fix printout of selected cross sections for plotting.
C-V        - Add option to print lethargy spectrum
C-V        - Allow scaling factors to selected reactions
C-V        - Parse input records explicitly to define ID1,ID2,MAT,MT,SCL
C-V  14/04 - Correct typos in the input instructions.
C-V        - Improve the logic defining ranges in PLOTTAB input.
C-V  14/08 Allow percent-integral threshold definition from input.
C-V        WARNING - In case of processing of multiple cross section
C-V                  files the input is not backward compatible.
C-V  14/09 Protect against anomalous covariances.
C-V  15/02 More on negative variance and invalid covariance.
C-V  17/04 Allow processing all materials for a given MT by MAT=0.
C-V  17/10 Make IER=4 a non-fatal error (skip reaction)
C-V  17/11 Increase MXR from 1.6M to 1.8M.
C-V  18/08 Allow MT=0 in selecting reactions to print all.
C-V  19/06 Fix processing of reaction rate ratios for Topsy-1
C-V  19/07 Guard against case when spectrum and x.s. do not overlap.
C-V  21/01 Add input example (no change to coding).
C-  (check VERS data statement below)
C-M
C-M  Manual for Program RR_UNC
C-M  =========================
C-M
C-M  The uncertainties in reaction rates and cross sections are
C-M  calculated, given the cross sections and the neutron spectrum
C-M  files in ENDF group format. The group structure for the
C-M  cross sections and the spectra must be the same.
C-M
C-M  The basis for uncertainty propagation is the expression for the
C-M  relative covariance of a product of two uncorrelated parameters
C-M  (usually the spectrum and the cross section in our case) [1]:
C-M
C-M    Rcov(xy) = Cov(xy)/(E[x]E[y])^2
C-M             = Cov(x)/E(x)^2 + Cov(y)/E(y)^2 +
C-M               Cov(x) Cov(y)/(E[x]E[y])^2
C-M             = Rcov(x) + Rcov(y) + Rcov(x) Rcov(y)        (1)
C-M
C-M  If the relative uncertainties are small, the last(quadratic)
C-M  term can be neglected. This is the approximation used in RR_UNC.
C-M
C-M  If the parameters are correlated the equation is more complex:
C-M
C-M    Rcov(xy) = Rcov(x) + Rcov(y) + Rcov(x) Rcov(y) +
C-M               Rcov(x^2,y^2) - Rcov(x,y)^2 - 2 Rcov(x,y)  (2)
C-M
C-M  Dosimetry reaction cross sections are usually uncorrelated
C-M  with the spectra, hence equation (2) is not used.
C-M
C-M  The neutron spectrum and the cumulative reaction rate integrals
C-M  are printed to the RR_UNC.CUR file. By default, reaction curves
C-M  are normalised to the total reaction rate. If specific MAT/MT are
C-M  defined, only the cumulative integral for the selected reaction
C-M  rate is printed without normalisation.
C-M
C-M  The code allows reaction rate spectra to be specified as the
C-M  weighting function, in which case the (self-shielded) spectrum
C-M  is obtained by dividing the reaction rate by the cross sections
C-M  at infinite dilution. It is assumed that the relative uncertainty
C-M  on the reaction rate is valid for the self-shielded spectrum.
C-M  NOTE: the above assumption is valid for reaction rate
C-M        spectra calculated by a Monte Carlo code where no
C-M        uncertainties in the cross sections are taken into
C-M        account and the uncertainty in the reaction rate is
C-M        purely the statistical uncertainty.
C-M
C-M  WARNING: The code supports processing of entire libraries,
C-M        containing up to 20 000 reactions. However, with large
C-M        files the running time begins to increase rapidly for
C-M        libraries with more than 1000 reactions.
C-M
C-M  Instructions
C-M  ------------
C-M  The filenames and the spectrum MAT are requested from input:
C-M    FLEN  source cross sections filename (fine-group ENDF, INT=1)
C-M    FLSP  source spectrum filename (same fine-group spectra, MT261)
C-M    MATS  spectrum MAT identifier.
C-M    IPRT  up to five-digit spectrum printout flag "ijklm"
C-M          (variables i=LETH,j=INEN,k=INRR,l=ICUML,m=INORM
C-M          relevant to PLOTTAB input file RR-UNC.p92 only)
C-M           i=1  Print the spectrum "per unit lethargy"
C-M             0  Print the spectrum "per unit energy"
C-M           j=0  Spectrum scale automatic (let the code decide)
C-M             1  Force Lin-scale Reaction rate intg./spect.
C-M             2  Force Log-scale
C-M           k=0  Energy scale automatic (let the code decide)
C-M             1  Force Lin-scale for the Energy
C-M             2  Force Log-scale
C-M           l=1  Print cumulative reaction rate integral
C-M             0  Print reaction rate spectrum (per unit energy)
C-M           m=1  Normalise to the total reaction rate integral
C-M             0  Print unnormalised quantities.
C-M    MRR,IRR,MATPRT,MTPRT,SCLRR for selected reactions:
C-M          MRR    MAT number in the cross section file, if the
C-M                 selected spectrum is a reaction rate spectrum.
C-M          IRR    MT  reaction identifier, if the selected
C-M                 spectrum is a reaction rate spectrum.
C-M          MATPRT ZA number of the selected cumulative reaction rate
C-M                 integral (or spectrum) to be printed.
C-M                 If MATPRT=0, all materials for the specifies MT
C-M                 (see below) are processed.
C-M          MTPRT  MT number of the selected cumulative reaction rate
C-M                 integral (or spectrum) to be printed.
C-M                 If MTPRT=0, all reactions for the specified 
C-M                 material (all materials if MATPRT=0) are processed.
C-M          SCLRR  Scaling factor for the selected reaction rate on
C-M                 printout; for example, if the scaling factor
C-M                 is the number density, the absolute contribution
C-M                 to the reaction rate of this reaction can be
C-M                 calculated.
C-M          Notes:
C-M          - MRR and IRR define the reaction rate spectrum on FLSP,
C-M            The spectrum is divided by the cross sections defined
C-M            by MATPRT and MTPRT (corresponding to the reaction
C-M            MAT and MT numbers on the FLEN file.
C-M          - If MRR and IRR are zero, the spectrum on the FLSP file
C-M            is actually the neutron spectrum, but MATPRT and MTPRT
C-M            define the selected reaction to be tabulated in the
C-M            RR_UNC.OUT file and the RR_UNC_CUR file.
C-M          - More than one input records can be specified to
C-M            define the list of MATPRT/MTPRT reaction to be printed.
C-M            The condition MRR=IRR=0 must be observed.
C-M          - If MRR and IRR are defined they should appear on the
C-M            first input record to define the reaction for the
C-M            calculation of the shielded spectrum.
C-M          - Once the spectrum is defined, it can be re-used to
C-M            calculate reaction rates given on a different FLEN.
C-M          Blank record terminates the MRR,IRR,MATPRT,MTPRT list.
C-M    FR_RI Fraction of the reaction rate integral defining the
C-M          median energy (default 0.5) (NEW).
C-M    FLEN  Filename of the next library to be processed. More than
C-M          one record can appear for different libraries. The list
C-M          of selected reactions defined for the first library is
C-M          applicable.
C-M          Blank record terminates the list of libraries.
C-M
C-M
C-M  Output files:
C-M  RR_UNC.LST file contains the calculated reaction rate
C-M             uncertainties.
C-M  RR_UNC.CUR file gives the spectrum and the normalised cumulative
C-M             reaction rate integral as a function of energy
C-M  RR_UNC.P92 file is the PLOTTAB input file for displaying the
C-M             data in RR_UNC.CUR with the PLOTTAB code, if desired.
C-M
C-M  Special features:
C-M
C-M  - The code produces tabular output of neutron spectra and reaction
C-M    rate spectra or cumulative reaction rate integrals in two-column
C-M    format that is easily ported into graphics applications.
C-M    Specifically, the code also prepares input instructions for the
C-M    graphics package PLOTTAB [2] so that interactive of PostScript
C-M    can be prepared instantly.
C-M
C-M  - When specific reactions are selected for printout by
C-M    explicitly specifying "MRR,IRR,MATPRT,MTPRT,SCLRR", the
C-M    reactions are sorted in ascending order of the median
C-M    energy
C-M
C-M  - By entering MATPRT and/or MTPRT as zero, all materials
C-M    and/or reactions will be printed, sorted by the median
C-M    energy.
C-M
C-M  - If the whole library is processed (entering blank for
C-M    the record defining MRR,IRR,MATPRT,MTPRT,SCLRR), the
C-M    reaction rates are printed in the order as they appear
C-M    in the library.
C-M
C-M  Input example (Fission SACS in Cf-252 spectrum)
C-M  IRDFF-II.g725
C-M  IRDFF-II_sp.g
C-M  9861
C-M  11211
C-M  0 0 92235 18
C-M  0 0 92238 18
C-M  0 0 93237 18
C-M  0 0 94239 18
C-M  0 0 95241 18
C-M
C-References:
C-R  [1] Wikipedia, "https://en.wikipedia.org/wiki/Variance"
C-R      Chapters "Product of independent variables" and
C-R      "Product of statistically dependent variables".
C-R  [2] D.E. Cullen: PLOTTAB - Plot continuous and/or Discrete Data
C-R      International Atomic Energy Agency, Vienna, Austria
C-R      IAEA-NDS-82, Rev 1, November 22, 2013
C-R      "https://www-nds.iaea.org/plottab/".
C-
      PARAMETER (MX1=800,MX2=800,MXR=1 800 000,MXM=80000,MXMT=800)
C* Maximum number of reactions per material
      PARAMETER (MXRR=600)
      CHARACTER*20   VERS
      CHARACTER*40   BLNK
      CHARACTER*80   FLNM,FLEN,FLSP,FLCU,FLOU,FLPI,FLE1
      CHARACTER*80   HDR
      CHARACTER*66   C66
      CHARACTER*8    CH8
C* Storage arrays:
C* EN1  - Energy boundaries of the cross section vector
C* CV1  - Cross-section covariance matrix
C* ESP  - Energy boundaries of the spectrum vector
C* CSP  - Spectrum covariance matrix
C* XSG  - Cross section vector
C* XSD  - Standard deviation of the cross sections
C* FSP  - Spectrum vector (as read)
C* FSD  - Spectrum vector standard deviation
C* FLX  - Group flux vector (from spectrum vector)
C* FLD  - Group flux standard deviation
C*
C* MATL - MAT*10 +LIS
C* MTL  - MF*1000+MT
C* IZFS - IZAP*10+LFS
      DIMENSION EN1(MX1),CV1(MX1,MX1),ESP(MX1),CSP(MX1,MX1)
     &         ,XSG(MX1),XSD(MX1),RIG(MX1),RID(MX1)
     &         ,FSP(MX1),FSD(MX1),FLX(MX1),FLD(MX1)
     &         ,RWO(MXR),MATPRTX(MXMT),MTPRTX(MXMT),SCLRR(MXMT)
     &         ,MATL(MXM),MTL(MXM),IZFS(MXM),NBT(20),INR(20)
C*
      DIMENSION MATZ(MXRR),MTZ(MXRR),IZAPZ(MXRR),LFSZ(MXRR),MSORT(MXRR)
     &     ,ZAZ(MXRR),RRIZ(MXRR),RR1Z(MXRR),RR2Z(MXRR),RR3Z(MXRR)
     &     ,E_HALFZ(MXRR)
C*
      DATA BLNK/'                                        '/ 
     &     FLEN/'mn55g-doso.g'/
     &     FLSP/'Maxwell-th.g'/
     &     FLOU/'RR_UNC.lst'/
     &     FLCU/'RR_UNC.cur'/
     &     FLPI/'RR_UNC.P92'/
     &     FLE1/'RR_UNC.TMP'/
      DATA LEN,LSP,LCU,LOU,LKB,LTT,LPI,LE1 /
     &       1,  2,  3,  4,  5,  6, 7,  8  /
C* Version label (20 characters)
      DATA VERS/'Version Jul. 2019   '/
C* Plot control flags
      INORM=1
      ICUML=1
      LETH =0
      INEN =0
      INRR =0
      RMAX =0
      RMIN =0
C* Maximum number of curves on any one plot
      MXPLT=30
C* Fraction of the integral to define the median energy
      FR_RI=0.5
C*
      MATS=9901
      IRR =0
      EMAX  =2.E+7
      E50LO =EMAX
      E50HI =1.E-5
      XLO=1.E20
      XHI=0
      NMAT  =0
      MATPRT=0
      MTPRT =0
      IRCP  =0
      DO I=1,MXMT
        MATPRTX(I)=0
        MTPRTX(I) =0
        SCLRR(I)  =1
      END DO
C* Character scaling in PLOTTAB input
      SCLCH =1.0
C*
      WRITE(LTT,*) ' RR_UNC - Calculate uncertainties in reaction rates'
      WRITE(LTT,*) ' --------------------------------------------------'
      WRITE(LTT,*) 
     & ' Andrej Trkov, Jozef Stefan Institute, Ljubljana, Slovenia'
      WRITE(LTT,*) '                  '//VERS
      WRITE(LTT,*) ' '
   12 WRITE(LTT,*) '$Enter reference cross sect. filename : '
      READ (LKB,'(A80)',END=20) FLNM
      IF(FLNM(1:40).NE.BLNK) FLEN=FLNM
      OPEN (UNIT=LEN,FILE=FLEN,STATUS='OLD',ERR=12)
C*
   14 WRITE(LTT,*) '$Enter source spectrum filename       : '
      READ (LKB,'(A80)',END=20) FLNM
      IF(FLNM(1:40).NE.BLNK) FLSP=FLNM
      OPEN (UNIT=LSP,FILE=FLSP,STATUS='OLD',ERR=14)
C*
   16 WRITE(LTT,*) '$Enter source spectrum MAT number     : '
      READ (LKB,'(A80)',END=20) FLNM
      IF(FLNM(1:40).NE.BLNK) READ (FLNM,*,ERR=16) MATS
      IF(MATS.GT.9999) GO TO 16
C*
      WRITE(LTT,*) '$Enter the printout flag "ijklm"       : '
      WRITE(LTT,*) '  i=1  Print the spectrum "per unit lethargy"   '
      WRITE(LTT,*) '    0  Print the spectrum "per unit energy"     '
      WRITE(LTT,*) '  j=1  Lin-scale Reaction rate intg./spect.     '
      WRITE(LTT,*) '    2  Log-scale                                '
      WRITE(LTT,*) '  k=1  Lin-scale Energy                         '
      WRITE(LTT,*) '    2  Log-scale                                '
      WRITE(LTT,*) '  l=1  Print cumulative reaction rate integral  '
      WRITE(LTT,*) '    0  Print reaction rate spectrum (per unit E)'
      WRITE(LTT,*) '  m=1  Normalise to total reaction rate integral'
      WRITE(LTT,*) '    0  Print unnormalised quantities.           '
      READ (LKB,'(A80)',END=20) FLNM
      IF(FLNM(1:40).NE.BLNK) THEN
        READ (FLNM,*,ERR=20) IPRT
        LETH =IPRT/10000
        ID1  =IPRT-10000*LETH
        ID2  =ID1/10
        INORM=ID1-10*ID2
        ID1  =ID2
        ID2  =ID1/10
        ICUML=ID1-10*ID2
        ID1  =ID2
        ID2  =ID1/10
        INEN =ID1-10*ID2
        ID1  =ID2
        ID2  =ID1/10
        INRR =ID1-10*ID2
      END IF
C*
      WRITE(LTT,*) ' Select RR spectrum, or reaction (dflt=blank)'
      WRITE(LTT,*) '        MRR    Spectrum MAT number     '
      WRITE(LTT,*) '        IRR    Spectrum MT number      '
      WRITE(LTT,*) '        MATPRT Material ZA designation '
      WRITE(LTT,*) '        MTPRT  Reaction MT number      '
C*
C* Parse the input records for specific reaction selection
   18 READ (LKB,'(A80)',END=20) FLNM
      IF(FLNM(1:40).EQ.BLNK) GO TO 19
      L2=0
      CALL PRSREC(L1,L2,80,FLNM)
      IF(L1.GT.L2) GO TO 20
      READ (FLNM(L1:L2),*) ID1
      CALL PRSREC(L1,L2,80,FLNM)
      IF(L1.GT.L2) GO TO 20
      READ (FLNM(L1:L2),*) ID2
      IF(ID1.NE.0 .AND. ID2.NE.0) THEN
        MRR=ID1
        IRR=ID2
      END IF
      CALL PRSREC(L1,L2,80,FLNM)
      IF(L1.GT.L2) GO TO 20
      READ (FLNM(L1:L2),*) MAT
      CALL PRSREC(L1,L2,80,FLNM)
      IF(L1.GT.L2) THEN
        WRITE(LTT,*) ' RR_UNC WARNING - MT missing for material',MAT
        WRITE(LTT,*) '                  Remaining input ignored'
        GO TO 20
      END IF
      READ (FLNM(L1:L2),*) MT
      IF(MT.LT.0) THEN
        IRCP=1
        MT  =-MT
      END IF
      CALL PRSREC(L1,L2,80,FLNM)
      SCL=1
      IF(L2.GE.L1) READ (FLNM(L1:L2),*) SCL
      IF(NMAT.GE.MXMT) THEN
        WRITE(LTT,*) ' RR_UNC WARNING - MXMT limit exceeded',MXMT
        WRITE(LTT,*) '                  Remaining input ignored'
        GO TO 20
      END IF
      NMAT=NMAT+1
      MATPRTX(NMAT)=MAT
      MTPRTX(NMAT) =MT
      SCLRR(NMAT)  =SCL
C... Old coding that requires more care with free-form input
C...  READ (FLNM,*,ERR=18) ID1,ID2,MATPRTX(NMAT+1),MTPRTX(NMAT+1)
C... &                    ,SCLRR(NMAT+1)
C...  NMAT=NMAT+1
C...  IF(ID1.NE.0 .AND. ID2.NE.0) THEN
C...    MRR=ID1
C...    IRR=ID2
C...  END IF
C...
      WRITE(LTT,*) ' Next selection or blank to finish'
      GO TO 18
C*
C* Enter the fraction of the reaction rate to define the median energy
   19 WRITE(LTT,*) ' Enter fraction of integral defining median energy:'
      READ (LKB,'(A80)',END=20) FLNM
      IF(FLNM(1:40) .NE. BLNK) READ(FLNM,*) FR_RI
      WRITE(LTT,'(A,I3)') ' Percent of integral defining median energy:'
     &     ,NINT(100*FR_RI)
C*
C* All input defined - proceed with calculations
   20 CONTINUE
C     OPEN (UNIT=90,FILE='file90.tmp',STATUS='UNKNOWN')
C     OPEN (UNIT=22,FILE='file22.tmp',STATUS='UNKNOWN')
      OPEN (UNIT=LOU,FILE=FLOU,STATUS='UNKNOWN')
      OPEN (UNIT=LCU,FILE=FLCU,STATUS='UNKNOWN')
      OPEN (UNIT=LPI,FILE=FLPI,STATUS='UNKNOWN')
      OPEN (UNIT=LE1,FILE=FLE1,STATUS='UNKNOWN')
C*
      WRITE(LOU,901) 
     & ' RR_UNC - Calculate uncertainties in reaction rates       '
      WRITE(LOU,901) 
     & ' ----------------------------------------------------------'
      WRITE(LOU,901) 
     & ' Andrej Trkov, Jozef Stefan Institute, Ljubljana, Slovenia'
      WRITE(LOU,901) 
     & '                  '//VERS
      WRITE(LOU,901) 
     & ' ----------------------------------------------------------'
      WRITE(LOU,903) ' '
      WRITE(LOU,903) ' Reference x.s. file      : ',FLEN
      WRITE(LOU,903) ' Source spectrum file     : ',FLSP
      WRITE(LOU,902) ' Reaction rate integ.flag : ',ICUML
      WRITE(LOU,902) ' Reaction rate norm. flag : ',INORM
      WRITE(LOU,903) ' '
      WRITE(LOU,902) ' Spectrum MAT No.         : ',MATS
      IF(IRR.NE.0)
     &WRITE(LOU,902) ' from reaction rate MAT/MT: ',MRR,IRR
C...  IF(NMAT.GT.0) THEN
C...    DO I=1,NMAT
C...      WRITE(LOU,902) ' Selected reaction rates  : '
C... &                  ,MATPRTX(I),MTPRTX(I),SCLRR(I)
C...    END DO
C...    WRITE(LOU,903) ' '
C...  END IF
C*
      WRITE(LTT,903) ' '
      WRITE(LTT,903) ' Reference x.s. file      : ',FLEN
      WRITE(LTT,903) ' Source spectrum file     : ',FLSP
      WRITE(LTT,902) ' Reaction rate print flag : ',ICUML
      WRITE(LTT,902) ' Reaction rate norm. flag : ',INORM
      WRITE(LTT,903) ' '
      WRITE(LTT,902) ' Spectrum MAT No.         : ',MATS
      IF(IRR.NE.0)
     &WRITE(LTT,902) ' from reaction rate MAT/MT: ',MRR,IRR
      WRITE(LTT,903) ' '
      IF(NMAT.GT.0) THEN
        DO I=1,NMAT
          WRITE(LTT,902) ' Selected reaction rates  : '
     &                  ,MATPRTX(I),MTPRTX(I),SCLRR(I)
        END DO
      END IF
C*
C* Retrieve the spectrum
      WRITE(LTT,*) ' Searching the spectrum file'
      MTS =0
      ICV =1
      REWIND LSP
      MAT =MATS
      MF  =3
      MT  =MTS
      IZAP=0
      LFS =0
      CALL GETXSG(LSP,MAT,MF,MT,IZAP,LFS,ICV,ZA,AWR,QM,QI
     &           ,NGS,MX1,ESP,FSP,FSD,MX1,CSP,MXR,RWO,IER)
      IF(IER.NE.0) THEN
        WRITE(*,*) 'RR_UNC ERROR - Searching MAT',MATS,' IER',IER
        STOP 'RR_UNC ERROR - Spectrum not found'
      END IF
      NGS0=NGS
      MTS =MT
C*    -- Calculate the spectrum integral
      SPYTG=0
      EAVRG=0
      EPEAK=0
      FPEAK=0
      E2   =ESP(1)
      DO I=1,NGS
        E1 = E2
        E2 = ESP(I+1)
        IF(FSP(I).GE.FPEAK) THEN
          FPEAK=FSP(I)
          EPEAK=SQRT(E1*E2)
        END IF
        FF = (E2-E1)*FSP(I)
        SPYTG=SPYTG+FF
        EAVRG=EAVRG+FF*SQRT(E1*E2)
      END DO
      EAVRG=EAVRG/SPYTG
      IF(ABS(EAVRG/0.0253-2).LT.0.01) EPEAK=EAVRG/2
      WRITE(LOU,901) ' Spectrum Integral            : ',SPYTG
      WRITE(LOU,901) ' Spectrum average energy [eV] : ',EAVRG
      WRITE(LOU,901) ' Spectrum peak energy    [eV] : ',EPEAK
      WRITE(LTT,901) ' Spectrum Integral            : ',SPYTG
      WRITE(LTT,901) ' Spectrum average energy [eV] : ',EAVRG
      WRITE(LTT,901) ' Spectrum peak energy    [eV] : ',EPEAK
      IF(ABS(SPYTG-1).LT.1E-3) THEN
        WRITE(LOU,901) ' Reaction rate RR = average cross-section'
        WRITE(LTT,901) ' Reaction rate RR = average cross-section'
      ELSE
        WRITE(LOU,901) ' Reaction rate RR = spectrum integral'
        WRITE(LTT,901) ' Reaction rate RR = spectrum integral'
      END IF
        WRITE(LTT,901) ' '
C*
C* Write the spectrum (as read) to CURves file
      IF(IRR.EQ.0) THEN
        WRITE(HDR,*) 'Mat',MATS,' MT',261
        ETH= 0
        IFL= 1
        FMX= 0
        DO I=1,NGS
          E1=ESP(I)
          E2=ESP(I+1)
          FF=FSP(I)
          FD=FSD(I)
C*        -- Convert to lethargy spectrum, if requested
          IF(LETH.EQ.1) THEN
            FF=FF*(E2-E1)/ALOG(E2/E1)
            FD=FD*(E2-E1)/ALOG(E2/E1)
          END IF
          FLX(I)=FF
          FLD(I)=FD
          FMX=MAX(FF,FMX)
        END DO
C*      -- Find reduced limits for plotting the spectrum
        ELO=ESP(2)/2
        FMN=FMX/1.E8
        II=NGS
   22   EHI=ESP(II+1)
        IF(FLX(II).LT.FMN .AND. II.GT.NGS/10) THEN
          EHI=ESP(II)
          II=II-1
          GO TO 22
        END IF
C*      -- Print the spectrum to the curves file
        CALL PRTCUR(LCU,HDR,ETH,NGS,ESP,FLX,FLD,IFL)
C*      -- Make the corresponding PLOTTAB input entry
        WRITE(LPI,921) 0.0, 14.0, 0.0, 10.6, 1, 1, SCLCH*1.2
        WRITE(LPI,922) 1, 0, 1, 0, 0, 0, 0
        WRITE(LPI,923) 'Energy'//BLNK,'eV'//BLNK
        IF(LETH.EQ.1) THEN
          WRITE(LPI,923) 'Spectrum'//BLNK,'n/lethargy'//BLNK
        ELSE
          WRITE(LPI,923) 'Spectrum'//BLNK,'n/eV'//BLNK
        END IF
        WRITE(LPI,923) 'RR_UNC: '//FLSP
        WRITE(LPI,923) 'Spectrum'//BLNK
        WRITE(LPI,925) ELO, EHI,  0,    0, 0, 0
        WRITE(LPI,924) BLNK,BLNK, 0,    0, 0
        WRITE(LPI,923) BLNK
      END IF
C*
C* Retrieve the cross sections
      WRITE(LTT,*) ' '
      WRITE(LTT,*) ' Searching the cross section file'
C*
      KXS=0
   30 MAT=0
      MT =0
      ZA =0
      NXS=0
C*    -- Skip file header
      CALL RDTEXT(LEN,MAT,MF,MT,C66,IER)
C*    -- Looping over reactions
   40 MAT0=MAT
      MT0 =MT
      CALL RDTEXT(LEN,MAT,MF,MT,C66,IER)
      IF(IER.NE.0) GO TO 90
C*    --Check for metastable targets
      IF(MF .EQ. 1 .AND. MT.EQ.451 .AND. MAT.NE.MAT0) THEN
        READ(C66(1:11),921) ZA
        CALL RDTEXT(LEN,MAT,MF,MT,C66,IER)
        READ (C66(34:44),922) LIS
        ZA=ZA+0.1*LIS
c...
c...    print *,'                     za.lis',za,lis
c...
      END IF
      IF(MF .EQ. 8 .OR. MF .EQ. 9) GO TO 40  ! RCN
      IF(MT .EQ. 0 .OR. MT.EQ.MT0) GO TO 40
      IF(MF .EQ. 3 .OR. MF.EQ.10 ) THEN
C*      -- Convert reference reaction rate ZA to MAT
        IF(NMAT.GT.0) THEN
          JMAT=NMAT
          DO I=1,NMAT
C...
c...        PRINT *,I,nmat,NINT(ZA),MT
c... &             ,MATPRTX(I),MTPRTX(I),MTPRTX(NMAT)
C...
            IF(NINT(ZA).EQ.MATPRTX(I) .AND. MTPRTX(I).EQ.MT) THEN
              MATPRTX(I)=MAT
              EXIT
            END IF
C*          -- If requested material is not specified (MAT=0)
C*             add current material to the list
c...        IF(MATPRTX(I).EQ.0 .AND. MTPRTX(I).EQ.MT) THEN
            IF(MATPRTX(I).EQ.0 .AND.
     &         (MTPRTX(I).EQ.0 .OR. MTPRTX(I).EQ.MT) ) THEN
              IF(I.GE.NMAT .AND. 
     &         (MTPRTX(I).EQ.0 .OR. MTPRTX(I).EQ.MT) ) THEN
                JMAT=NMAT+1
                IF(JMAT.GT.MXMT) THEN
                  DO J=1,NMAT
                    PRINT *,MATPRTX(J),MTPRTX(J),SCLRR(J)
                  END DO
                  STOP 'RR_UNC ERROR - MXMT limit exceeded'
                END IF
                MATPRTX(JMAT)=MATPRTX(I)
                MTPRTX (JMAT)=MTPRTX(I)
C...            MATPRTX(JMAT)=MAT
C...            MTPRTX (JMAT)=MT
                SCLRR  (JMAT)=SCLRR(I)
              END IF
              MATPRTX(I)=MAT
              MTPRTX (I)=MT
              EXIT
            END IF
C...
          END DO
          NMAT=JMAT
        END IF
        IF(MF.EQ.10) THEN
          READ(C66(45:55),*) NFS
        ELSE
          NFS=1
        END IF
C*
        IFS=0
   42   IF(NXS.LT.MXM) THEN
          IFS=IFS+1
          NXS=NXS+1
          MATL(NXS)=MAT*10+LIS
          MTL (NXS)=MT+1000*MF
          IZFS(NXS)=0
C...
c...      WRITE(*,'(A,4I5)') ' Material',NXS,MAT,MF,MT
C...
          IF(MF.EQ.10) THEN
            CALL RDTAB1(LEN,QM,QI,IZAP,LFS,NR,NEX
     &                 ,NBT,INR,EN1,XSG,MX1,IER)
            IZFS(NXS)=10*IZAP+LFS
C...
C...        print *,'izap,lfs',izap,lfs,QI,NEX,IER
C...        print *,'nxs,mat,mft,izfs',nxs,mat,mtl(nxs),izfs(nxs)
C...
            IF(IFS.LT.NFS) GO TO 42
          END IF
C...
C...      print *,'nxs,mat,mft,izfs',nxs,mat,mtl(nxs),izfs(nxs)
C...      if(mat.gt.1325) stop
C...
        ELSE
          WRITE(LTT,*) 'RR_UNC WARNING - MXM limit exceeded'
          GO TO 90
        END IF
      END IF
      GO TO 40
C*
C* The list of all reactions is complete
C*
   90 WRITE(LTT,*) ' Number of reactions: ',NXS
C*    -- Check if reaction rate spectrum is given
      MXS=NXS
      IF(IRR.LE.0) GO TO 92
      DO I=1,NXS
        MTI=MTL(I)
        MTI=MTI-1000*(MTI/1000)
        IF(MATL(I)/10.EQ.MRR .AND. MTI.EQ.IRR) THEN
C*        -- If spectrum is given, only the corresponding
C*           reaction will be processed
          IRR=I
          IXS=I-1
          NXS=I
          GO TO 100
        END IF
      END DO
      WRITE(LTT,*) 'RR_UNC ERRORR - Requested RR MT not on file',IRR
      STOP 'RR_UNC ERRORR - Requested RR MT not on file'
C*
C* Process each reaction in turn
   92 NXS=MXS
      IXS=0
      JXS=0
      IHR=0
      MAT0=-1
      MFT0=-1
      EHI=0
      REWIND LEN
  100 IXS=IXS+1
      IF(IXS.GT.NXS) GO TO 800
      NGS=NGS0
      MAT=MATL(IXS)/10
      LIS=MATL(IXS)-10*MAT
      MFT=MTL (IXS)
      MF =MFT/1000
      MT =MFT-1000*MF
      IZAP=IZFS(IXS)/10
      LFS=IZFS(IXS)-10*IZAP
      E_HALF=0
      ICV=1
      SCL=1
C*
c...
C...  print *,'mat,mft,izfs,lfs,nmat',mat,mft,izfs(ixs),lfs,nmat
c...  if(mat.gt.1325) stop
c...
C* If MAT number changes, copy this material to scratch
      IF(MAT.NE.MAT0 .OR. MFT.LT.MFT0) THEN
        CALL RDTEXT(LEN,MATX,MFX,MTX,C66,IER)
        CALL CPYMAT(LEN,LE1,MAT,IER)
        MAT0=MAT
        MFT0=MFT
      END IF
      REWIND LE1
      MTX=MT
C* Check if this material/reaction is to be processed
      IF(NMAT.GT.0) THEN
        DO I=1,NMAT
c...
c...      print *,i,MATPRTX(I),MTPRTX(I),mat,mt
c...
          IF(MAT.EQ.MATPRTX(I) .AND. MT.EQ.MTPRTX(I)) THEN
            MATPRT=MATPRTX(I)
            MTPRT =MTPRTX(I)
            SCL   =SCLRR(I)
            GO TO 102
          END IF
        END DO
        GO TO 100
      END IF
C* Retrieve the cross sections
  102 CALL GETXSG(LE1,MAT,MF,MT,IZAP,LFS,ICV,ZA,AWR,QM,QI
     &           ,NG,MX1,EN1,XSG,XSD,MX1,CV1,MXR,RWO,IER)
      WRITE(*,'(A,5I5)') ' Processing reaction',IXS,MAT,MF,MT
      IF(IER.NE.0) THEN
        WRITE(*,*) ' ERROR encountered',IER
        IF     (IER.EQ.1) THEN
          WRITE(*,*) ' Specified material not found'
        ELSE IF(IER.EQ.2) THEN
          WRITE(*,*) ' End-of-file before material found'
        ELSE IF(IER.EQ.3) THEN
          WRITE(*,*) ' Read error'
        ELSE IF(IER.EQ.4) THEN
          WRITE(*,*) ' Covariance energy grid denser than x.s. grid'
          GO TO 100
        ELSE IF(IER.EQ.11) THEN
          WRITE(*,*) ' Multiple ranges for the cross section table'
        ELSE IF(IER.EQ.12) THEN
          WRITE(*,*) ' Cross sections not in histogram representation'
        ELSE IF(IER.EQ.21) THEN
          WRITE(*,*) ' Covariance E-grid exceeds cross section grid'
        END IF
        GO TO 850
      END IF
      ZA=ZA+0.1*LIS
      IF(IRCP.EQ.1) THEN
        DO I=1,NG
C*        -- Averaged cross section is the inverse
          XX=1/XSG(I)
C*        -- Convert uncertainty to relative
          DD=XSD(I)*XX
C*        -- Save inverse cross section and absolute uncertainty
          XSG(I)=XX
          XSD(I)=DD*XX
        END DO
      END IF
C...
C...  WRITE(LTT,*) 'IXS,MAT,MF,MT,IZAP,LFS,NG,ICV,IER'
C... &             ,IXS,MAT,MF,MT,IZAP,LFS,NG,ICV,IER
C...  WRITE(LTT,*) 'ZA,AWR,QM,QI',ZA,AWR,QM,QI
C...
C* Truncate cross section vector if it extends above spectrum range
      EPS= 1.E-3
      LGS = NGS
      DO WHILE(EN1(NG+1)-ESP(LGS+1).GT.EPS*ESP(LGS+1) .AND. NG.GT.1)
        NG=NG-1
      END DO
C* Truncate spectrum vector if it extends above cross section range
      DO WHILE(ESP(LGS+1)-EN1(NG+1).GT.EPS*ESP(LGS+1) .AND. LGS.GT.1)
        LGS=LGS-1
      END DO
C* Check if the cross section vector extends below spectrum range
      LX1=1
      DO WHILE(ESP(2)-EN1(LX1+1).GT.EPS*ESP(2) .AND. LX1.LT.NG)
        LX1=LX1+1
      END DO
C* Check if the spectrum vector extends below cross section range
      LS1=1
      DO WHILE(EN1(2)-ESP(LS1+1).GT.EPS*EN1(2) .AND. LS1.LT.LGS)
        LS1=LS1+1
      END DO
C* Check the number of groups (NGG is the union number of groups)
      IF(NG-LX1.NE.LGS-LS1) THEN
        IER=1
        JGG=0
        EX=NG -LX1
        ES=LGS-LS1
        GO TO 120
      END IF
      NGG=NG-LX1+1
C* Define the maximum energy
      EMAX=EN1(NG+1)
C*
C* If reaction rate spectrum is given, divide by cross section
C* on first pass, then restart processing all cross sections
      IF(IRR.GT.0) THEN
        FMX=0
        ETH= 0
        IFL= 1
        WRITE(HDR,*) 'Mat',MATS,' MT',261
        DO I=1,NGG
          FF=FSP(LS1-1+I)
          XX=XSG(LX1-1+I)
          DD=FSD(LS1-1+I)/FF
          IF(XX.GT.0) THEN
            FF=FF/XX
          ELSE
            FF=0
          END IF
          FD=DD*FF
          FSP(LS1-1+I)=FF
          FSD(LS1-1+I)=FD
          IF(LETH.EQ.1) THEN
            FF=FF*(E2-E1)/ALOG(E2/E1)
            FD=FD*(E2-E1)/ALOG(E2/E1)
          END IF
          EN1(I)=ESP(LS1-1+I)
          FLX(I)=FF
          FLD(I)=FD
          FMX=MAX(FMX,FF)
        END DO
C*      -- Find reduced limits for plotting the spectrum
        ELO=ESP(2)/2
        FMN=FMX/1.E8
        II=NGS
  104   EHI=ESP(II+1)
        IF(FLX(II).LT.FMN .AND. II.GT.NGS/10) THEN
          EHI=ESP(II)
          II=II-1
          GO TO 104
        END IF
C*      -- Print the self-shielded spectrum to the curves file
        CALL PRTCUR(LCU,HDR,ETH,NGG,EN1,FLX,FLD,IFL)
C*      -- Make the corresponding PLOTTAB input entry
        WRITE(LPI,921) 0.0, 14.0, 0.0, 10.6, 1, 1, SCLCH*1.2
        WRITE(LPI,922) 1, 0, 1, 0, 0, 0, 0
        WRITE(LPI,923) 'Energy'//BLNK,'eV'//BLNK
        WRITE(LPI,923) 'Spectrum'//BLNK,'n/eV'//BLNK
        WRITE(LPI,923) 'RR_UNC: '//FLSP
        WRITE(LPI,923) 'Spectrum'//BLNK
        WRITE(LPI,925) ELO, EHI,  0,    0, 0, 0
        WRITE(LPI,924) BLNK,BLNK, 0,    0, 0
        WRITE(LPI,923) BLNK
        IRR=-IRR
        GO TO 92
      END IF
C*
C* Reserve scratch array
C* LRR array of length NGG storing reaction rates
C* LRC scratch array of length NGG
C* LM1 packed cross section relative covariance matrix NGG*NGG
C* LM2 packed spectrum relative covariance matrix NGG*NGG
C* LRO reaction rate uncertainty (formally a 1*1 array)
      LRR=1
      LRC=LRR+NGG
      LM1=LRC+NGG
      LM2=LM1+NGG*NGG
      LRO=LM2+NGG*NGG
      IF(LRO.GT.MXR) THEN
        PRINT *,'Workspace needed',LRO
        STOP 'RR_UNC ERROR - MXR limit exceeded'
      END IF
C* Shift the cross sections, spectrum and their covariance matrices
      JJ =0
c...
c...  print *,'ngg,i,lx1,ls1',ngg,i,lx1,ls1
c...
      DO I=1,NGG
        IF(ABS(EN1(I+LX1)-ESP(I+LS1)).GT.EPS*ESP(I+LS1)) THEN
          JGG=I
          IER=1
          EX=EN1(I+LX1)
          ES=ESP(I+LS1)
          IF(NGG.EQ.1) THEN
            WRITE(*,*) ' WARNING - No overlap between spectrum'
     &                ,' and flux groups for reaction MT',MTX
            GO TO 100
          ELSE
            GO TO 120
          END IF
        END IF
        EN1(I)=ESP(I+LS1-1)
        FLX(I)=FSP(I+LS1-1)
        FLD(I)=FSD(I+LS1-1)
        XSG(I)=XSG(I+LX1-1)
        XSD(I)=XSD(I+LX1-1)
        DO J=1,NGG
          RWO(LM1+JJ)=CV1(LX1-1+J,LX1-1+I)
          RWO(LM2+JJ)=CSP(LS1-1+J,LS1-1+I)
          JJ=JJ+1
        END DO
      END DO
      EN1(NGG+1)=ESP(NGG+LS1)
C...
C...      NPR=NGG
          NPR=MIN(7,NGG)
C...      PRINT *,'Original spectrum'
C...      CALL MTXPRT(21,LDG,  1,NPR,MX1,CSP)
C...      PRINT *,'Truncated spectrum for MT',mtx,lx1,npr,ngg
C...      CALL MTXPRT(22,LDG,LS1,NPR,NGG,RWO(LM2))
C         PRINT *,'Original cross section Cov for MT',mtx,lx1,npr,ngg
C         write(22,*) 'Original cross section Cov MT',mtx,lx1,npr,ngg
C         write(22,*) (en1(j),j=1,npr)
C         CALL MTXPRT(22,LDG,  1,NPR,MX2,CV1)
C         write(22,*) 'Reduced cross section Cov'
C         CALL MTXPRT(22,LDG,LX1,NPR,NGG,RWO(LM1))
C...
C...      STOP
C...
  120 IF(IER.NE.0) THEN
        WRITE(*,*) ' ERROR - Inconsistent group',JGG
     &            ,' Exs=',EX,' Esp=',ES
        STOP 'RR_UNC ERROR - Inconsistent group boundaries'
      END IF
C* Calculate the flux FLX (spectrum integral per group)
      RRI=0
      RRU=0
      DO I=1,NGG
        FF =FLX(I)
        IF(FF.GT.0) THEN
          DD =FLD(I)/FF
        ELSE
          DD=0
        END IF
        FF =FF*(EN1(I+1)-EN1(I))
        XX =XSG(I)
        RR =FF*XX
        RRI=RRI+ RR
        RRU=RRU+(RR*DD)**2
        FLX(I)=FF
        RWO(LRR-1+I)=RR
      END DO
c...
c...  PRINT *,'Diagonal uncertainty',100*SQRT(RRU)/RRI
c...
C* Total reaction rate RRX = RWO(NGG,1) = XSG(NGG,1)*FLX(1,NGG)
      CALL MTXPRD(XSG,FLX,RWO(LRO),NGG,1,1)
      RRX=RWO(LRO)
      IF(RRX.LE.0) THEN
        WRITE(LTT,*) 'RR_UNC WARNING - zero reaction rate for MT',MTX
        GOTO 100
      END IF
C*
C* Reaction rate uncertainty from cross sections
      CALL MTXPRD(RWO(LM1),RWO(LRR),RWO(LRC),NGG,NGG,1)
      CALL MTXPRD(RWO(LRC),RWO(LRR),RWO(LRO),NGG,  1,1)
      IF(RWO(LRO).LT.0) THEN
        IF(RWO(LRO).LT.-1.E-6)
     &    PRINT *,'WARNING - Negative X.S. variance',RWO(LRO)
        RWO(LRO)=0
      END IF
      RRD=SQRT(RWO(LRO))
      RR1=100*RRD/RRX
C* Reaction rate uncertainty from spectrum
      CALL MTXPRD(RWO(LM2),RWO(LRR),RWO(LRC),NGG,NGG,1)
      CALL MTXPRD(RWO(LRC),RWO(LRR),RWO(LRO),NGG,  1,1)
      IF(RWO(LRO).LT.0) THEN
        IF(RWO(LRO).LT.-1.E-6)
     &    PRINT *,'WARNING - Negative spect. variance',RWO(LRO)
        RWO(LRO)=0
      END IF
      RRD=SQRT(RWO(LRO))
      RR2=100*RRD/RRX
C* Total reaction rate uncertainty
      CALL MTXSUM(RWO(LM1),RWO(LM2),RWO(LM1),NGG,NGG)
      CALL MTXPRD(RWO(LM1),RWO(LRR),RWO(LRC),NGG,NGG,1)
      CALL MTXPRD(RWO(LRC),RWO(LRR),RWO(LRO),NGG,  1,1)
      IF(RWO(LRO).LT.0) THEN
        IF(RWO(LRO).LT.-1.E-6)
     &    PRINT *,'WARNING - Negative total variance',RWO(LRO)
        RWO(LRO)=0
      END IF
      RRD=SQRT(RWO(LRO))
      RR3=100*RRD/RRX
C*
C* Prepare the cross section reaction rates for printing
      ETH=-QI*(AWR+1)/AWR
      MAZ= NINT(ZA)
      RRI= 0
      DDI= 0
      E2 =-1.E6
      DO I=1,NGG
        E1=E2
        E2=EN1(I)
        XX=XSG(I)
        FF=FLX(I)
        IF(FF.GT.0 .AND. XX.GT.0) THEN
          DD=XSD(I)/XX + FLD(I)/FF
        ELSE
          DD=0
        END IF
        RR =XX*FF*SCL
        RRI=RRI+RR
        XSG(I)=RR/(E2-E1)
        XSD(i)=RR*DD/(E2-E1)
        DDI=DDI+RR*DD
        RIG(I)=RRI
        RID(I)=DDI
      END DO
C----------------------------------------------------
C	Normalise the response (if more than 1 RR) and get the E(50%)
C
      E2=EN1(1)
      X2=0
      XMX=0
      RMX=0
      XMN=1.E32
      RMN=1.E32
      DO I=1,NGG
        X1=X2
        X2=RIG(I)/RRI
        E1=E2
        E2=EN1(I+1)
        IF(E2.GE.ETH .AND. (X1.LT.FR_RI .AND. X2.GE.FR_RI)) THEN
          E_HALF=E1+(FR_RI-X1)*(E2-E1)/(X2-X1)
        END IF
C*      -- Normalise if more than one reaction rate
        IF(INORM.EQ.1) THEN
          XSG(I)=XSG(I)/RRI
          RIG(I)=X2
        END IF
        IF(XSG(I).GT.XMX) XMX=XSG(I)
        IF(RIG(I).GT.RMX) RMX=RIG(I)
        IF(XSG(I).LT.XMN) XMN=XSG(I)
        IF(RIG(I).LT.RMN) RMN=RIG(I)
      END DO
C----------------------------------------------------
C*
C* Print the uncertainties
      IHR=IHR+1
      IF(NMAT.EQ.0) THEN
C*      -- Immediate printout if no specific reactions selected
        CALL PRTUNC(MATS,LOU,IHR,ZA,MT,IZAP,LFS,RRI,RR1,RR2,RR3
     &             ,E_HALF,FR_RI)
      ELSE
C*      -- Collect printout quantities in case of selected reactions
        IF(IHR.GT.MXRR) STOP 'RR_UNC ERROR - MXRR limit exceeded'
        MATZ(IHR) =MAT
        ZAZ(IHR)  =ZA
        MTZ (IHR) =MT
        IZAPZ(IHR)=IZAP
        LFSZ(IHR) =LFS
        RRIZ(IHR) =RRI
        RR1Z(IHR) =RR1
        RR2Z(IHR) =RR2
        RR3Z(IHR) =RR3
        E_HALFZ(IHR)=E_HALF
      END IF
C*
      IFL=0
      IF(MTPRT.EQ.0) THEN
C*      -- General printout
        WRITE(HDR,*) 'Mat',MAZ,' MT',MT
        IF(ICUML.EQ.1) THEN
          CALL PRTCUR(LCU,HDR,ETH,NGG,EN1,RIG,RID,IFL)
          XLO =MIN(XLO,RMN)
          XHI =MAX(XHI,RMX)
          XTOL=1E-3
          RLST=RIG(NGG)
          EHIJ=EN1(2)
          DO J=2,NGG
            IF(1-RIG(J-1)/RLST .GT.XTOL) EHIJ=EN1(J)
          END DO
        ELSE
          CALL PRTCUR(LCU,HDR,ETH,NGG,EN1,XSG,XSD,IFL)
          XLO =MIN(XLO,XMN)
          XHI =MAX(XHI,XMX)
          EHIJ=MIN(E50HI*1000,EMAX)
        END IF
        EHI=MAX(EHI,EHIJ)
      ELSE IF(MATPRT.EQ.MAT .AND. MTPRT.EQ.MT) THEN
C*      -- Printout of selected reaction rates
C...    HDR=FLEN
        WRITE(HDR,*) 'Mat',MAZ,' MT',MT
        IF(ICUML.EQ.1) THEN
          CALL PRTCUR(LCU,HDR,ETH,NGG,EN1,RIG,RID,IFL)
          XLO =MIN(XLO,RMN)
          XHI =MAX(XHI,RMX)
          XTOL=1E-3
          RLST=RIG(NGG)
          EHIJ=EN1(2)
          DO J=2,NGG
            IF(1-RIG(J-1)/RLST .GT.XTOL) EHIJ=EN1(J)
          END DO
        ELSE
          CALL PRTCUR(LCU,HDR,ETH,NGG,EN1,XSG,XSD,IFL)
          XLO =MIN(XLO,XMN)
          XHI =MAX(XHI,XMX)
          EHIJ=MIN(E50HI*1000,EMAX)
        END IF
        EHI=MAX(EHI,EHIJ)
        KXS=KXS+1
      END IF
      IF(IHR.EQ.1) THEN
        MAP  =MAZ
        E50LO=E_HALF
        E50HI=E_HALF
      END IF
      IF(MTPRT.EQ.0 .AND. (MAZ.NE.MAP .OR. IXS.EQ.NXS) ) THEN
C*      -- Make a new PLOTTAB input entry if material changes
        IF(IXS.EQ.NXS) JXS=JXS+1
        ELO=E50LO/100
        SCLCHI=SCLCH
        IF(JXS.LE.24) SCLCHI=SCLCH*1.1
        IF(JXS.LE.12) SCLCHI=SCLCH*1.2
        IF(JXS.LE. 6) SCLCHI=SCLCH*1.5
        WRITE(LPI,921) 0.0, 14.0, 0.0, 10.6, 1, 1, SCLCHI
        JXSP=MIN(JXS,MXPLT)
        NXSP=JXS
        WRITE(LPI,922) JXSP, 0, 1, 0, 0, 0, 0
        WRITE(LPI,923) 'Energy'//BLNK,'eV'//BLNK
        IF(NXS.GT.1) THEN
          WRITE(LPI,923) 'Reaction Rate'//BLNK,'Relative'//BLNK
        ELSE
          WRITE(LPI,923) 'Reaction Rate'//BLNK,'1/s'//BLNK
        END IF
  122   WRITE(LPI,923) 'RR_UNC: '//FLEN
        WRITE(LPI,923) 'Cumulative Reaction Rate'//BLNK
        INENJ=INEN
        IF(E50LO*10.GT.EHI) THEN
C*        -- Force linear if median E less than a decade from Ehi
          IF(INEN.EQ.0) INENJ=1
        END IF
        WRITE(LPI,925) ELO, EHI,  0, INENJ, 0, 0
        IF(INRR.EQ.2) XLO=MAX(XLO,XHI/1.E4)
        WRITE(LPI,925) XLO, XHI,  0, INRR, 0, 0
C...    WRITE(LPI,924) BLNK,BLNK, 0, INRR, 0, 0
        NXSP=NXSP-JXSP
        IF(NXSP.GT.0) GO TO 122
        WRITE(LPI,923) BLNK
        JXS=0
        E50LO=1.E20
        E50HI=0
        XLO=1.E20
        XHI=0
      END IF
      E50LO=MIN(E50LO,E_HALF)
      E50HI=MAX(E50HI,E_HALF)
      JXS=JXS+1
c...
c...      print *,'maz,jxs,e50lo,e50hi,elo,ehi,emax,e_half'
c... &           , maz,jxs,e50lo,e50hi,elo,ehi,emax,e_half
c...
      MAP=MAZ
C*
C* One reaction processed - loop to the next one
      GO TO 100
C* Make the Plottab input entry if specific reaction is selected
  800 IF(MTPRT.NE.0) THEN
        IF(KXS.LE.0) GO TO 830
        ELO=E50LO/100
        NXSP=KXS
  824   JXSP=MIN(NXSP,MXPLT)
C...
C...    PRINT *,'kxs,nxsp,jxsp,mxplt',kxs,nxsp,jxsp,mxplt
C...
        SCLCHI=SCLCH
        IF(KXS.LE.24) SCLCHI=SCLCH*1.1
        IF(KXS.LE.12) SCLCHI=SCLCH*1.2
        IF(KXS.LE. 6) SCLCHI=SCLCH*1.5
        WRITE(LPI,921) 0.0, 14.0, 0.0, 10.6, 1, 1, SCLCHI
        WRITE(LPI,922) JXSP, 0, 1, 0, 0, 0, 0
        WRITE(LPI,923) 'Energy'//BLNK,'eV'//BLNK
        WRITE(LPI,923) 'Reaction Rate'//BLNK,'1/s'//BLNK
        WRITE(LPI,923) 'RR_UNC: '//FLSP
        WRITE(LPI,926) 'Cumulative Reaction Rates: '//FLEN
        IF(E50LO*10.GT.EHI) THEN
C*        -- Force linear if threshold starts high
          INENJ=INEN
          IF(INEN.EQ.0) INENJ=1
          WRITE(LPI,924) BLNK,BLNK, 0, INENJ, 0, 0
        ELSE
          WRITE(LPI,925) ELO, EHI,  0, INEN , 0, 0
        END IF
        IF(INRR.EQ.2) XLO=MAX(XLO,XHI/1.E4)
        WRITE(LPI,925) XLO, XHI,  0, INRR, 0, 0
C...    WRITE(LPI,924) BLNK,BLNK, 0, INRR, 0, 0
        WRITE(LPI,923) BLNK
        NXSP=NXSP-JXSP
        IF(NXSP.GT.0) GO TO 824
        KXS=0
C*      -- Sort the reaction rates by median energy
        CALL SRTINM(IHR,MSORT,E_HALFZ)
        DO I=1,IHR
          JHR  =MSORT(I)
          MAT  =MATZ(JHR)
          ZA   =ZAZ(JHR)
          MT   =MTZ (JHR)
          IZAP =IZAPZ(JHR)
          LFS  =LFSZ(JHR)
          RRI  =RRIZ(JHR)
          RR1  =RR1Z(JHR)
          RR2  =RR2Z(JHR)
          RR3  =RR3Z(JHR)
          E_HF =E_HALFZ(JHR)
          CALL PRTUNC(MATS,LOU,I,ZA,MT,IZAP,LFS,RRI,RR1,RR2,RR3
     &               ,E_HF,FR_RI)
        END DO
      END IF
C* Process another cross section file
  830 WRITE(LTT,*) ' '
      WRITE(LTT,*) '$Enter next cross section filename    : '
      READ (LKB,'(A80)',END=900) FLNM
      IF(FLNM(1:40).EQ.BLNK) GO TO 850
      FLEN=FLNM
      CLOSE(UNIT=LEN)
      WRITE(LTT,*) ' '
      WRITE(LTT,*) ' Next cross section file  : ',FLEN
      WRITE(LTT,*) ' '
      WRITE(LOU,*) ' '
      WRITE(LOU,*) ' Next cross section file  : ',FLEN
      WRITE(LOU,*) ' '
      OPEN (UNIT=LEN,FILE=FLEN,STATUS='OLD',ERR=850)
      GO TO 30
C*
C* Close the output and curves file
  850 WRITE(LOU,*) ' '
      CLOSE(LCU)
      CLOSE(LOU)
      CLOSE(LPI)
  900 STOP 'RR_UNC Completed'
C*
  901 FORMAT(A,1P,E10.3)
  902 FORMAT(A,2I6,1P,E12.3)
  903 FORMAT(2A)
  911 FORMAT(A,1P,E12.4,0P,F10.4)
C*
  921 FORMAT(4F11.4,2I11,F4.2)
  922 FORMAT(6I11,I4)
  923 FORMAT(2A40)
  924 FORMAT(2A11,4I11)
  925 FORMAT(1P,2E11.4,4I11)
C.925 FORMAT(1P, E11.4,A11,4I11)
  926 FORMAT(A)
      END
C
      SUBROUTINE PRSREC(L1,L2,MXL,REC)
C-Title  : Subroutine PRSREC
C-Purpose: Parse a record for a non-blank string of characters
C-Description:
C-D  The record string REC of length MXL is parsed. On input, L2 is
C-D  the last character that has been scanned in a previous call,
C-D  therefore the search starts at character L2+1. On exit:
C-D    L1  is the starting index of a non-blank string on the record
C-D    L2  is the index of the last non-blank character on the record.
C-D  For compatibility with free-form input, the "/" character
C-D  implies the end of input and the remainder is not scanned
C-D  On first call set L2=0 to search the record from the beginning.
C-D  The search is successful if L2>L1 on exit.
C-D  
      CHARACTER*(MXL) REC
C*
C* Find the next non-blank character
      L1=L2
   10 L1=L1+1
      IF(L1.GT.MXL) RETURN
      IF(REC(L1:L1).EQ.'/') RETURN
      IF(REC(L1:L1).EQ.' ') GO TO 10
C* Find the last character of the string
      L2=L1
   20 IF(L2.GE.MXL) RETURN
      IF(REC(L2+1:L2+1).EQ.' ' .OR.
     &   REC(L2+1:L2+1).EQ.'/' .OR.
     &   REC(L2+1:L2+1).EQ.'$') RETURN
      L2=L2+1
      GO TO 20
      END
C
      SUBROUTINE CPYMAT(LEN,LE1,MAT1,IER)
C-Title  : Subroutine CPYMAT
C-Purpose: Copy next material from unit LEN to LE1
C-Description:
C-D  LEN  - Source ENDF file unit
C-D  LE1  - Output ENDF file unit
C-D  MAT1 - Material to be copied
C-D  IER  - Error flag
C-D           1  - TEND record found before material found
C-
      CHARACTER*66 C66,H66
C* Write header to scratch file
      REWIND LE1
      H66='Scratch file                     '//
     &    '                                 '
      IZR=0
      IHD=7777
      NS =-1
      CALL WRTEXT(LE1,IHD,IZR,IZR,NS,H66)
C* Start reading the source file
   10 CALL RDTEXT(LEN,MAT,MF,MT,C66,IER)
      IF(MAT.EQ.MAT1) GO TO 20
C*    --Skip to the next material
      IF(MAT.LT.0) THEN
        IER=1
        GO TO 40
      END IF
      GO TO 10
C* Copy next material including MEND record
   20 CALL WRTEXT(LE1,MAT,MF,MT,NS,C66)
   30 CALL RDTEXT(LEN,MAT,MF,MT,C66,IER)
      CALL WRTEXT(LE1,MAT,MF,MT,NS,C66)
      IF(MAT.NE.0) GO TO 30
C* Write the TEND record
   40 H66='                                 '//
     &    '                                 '
      IHD=-1
      NS =-1
      CALL WRTEXT(LE1,IHD,IZR,IZR,NS,H66)
      RETURN
      END
C
      SUBROUTINE PRTUNC(MATS,LOU,IXS,ZA,MT,IZAP,LFS
     &                 ,RRI,RR1,RR2,RR3,E_HALF,FR_RI)
C-Title  : Subroutine PRTUNC
C-Purpose: Print the calculated uncertainties
C     REAL*8 dtmp
      CHARACTER*1  HH,CH,HFS(5)
      CHARACTER*8  CH8
      DATA HFS/ 'g','m','n','o','p'/
C*
      IF(IXS.EQ.1) THEN
        IPC=MIN(99,NINT(FR_RI*100))
        WRITE(LOU,900) IPC
        WRITE( * ,900) IPC
      END IF
C*
  850 EHEV=E_HALF/1000000
      CALL CH8PCK(CH8,EHEV)
      MM=MT
      CH=' '
      IF(IZAP.GT.0) THEN
C...    IF(MT.EQ.5) MM=IZAP
        MM=IZAP
        CH='*'
        IF(LFS.LT.5) CH=HFS(LFS+1)
      END IF
      IZA=ZA+0.01
      LIS=NINT(ZA*10)-IZA*10
      IF(LIS.GT.0) THEN
        HH=HFS(LIS+1)
      ELSE
        HH=' '
      END IF
      WRITE(*,901) IXS,IZA,HH,MM,CH,
     & CH8,RRI*1000.d0,RR3/100.d0*RRI*1000.d0,RR1,RR2,RR3
      WRITE(LOU,901) IXS,NINT(ZA),HH,MM,CH,
     & CH8,RRI*1000.d0,RR3/100.d0*RRI*1000.d0,RR1,RR2,RR3
  880 RETURN
  900 FORMAT(
     &/'                                                       '
     &,'  Unc.   Unc.   Unc.  '
     &/'   No.  Mat.     MT   E(',I2.2,'%)       <RR> +/-  Unc       '
     &,'  x.s.   Sp.    Total '
     &/'                      [MeV]             [mb]           '
     &,'  [%]    [%]    [%]   '					 
     &/' ----- ------ ------ ------- ------------------------  '
     &,' ------ ------ ------')
  901 FORMAT(I6,I6,A1,I6,A1,A8,1P,E11.4,' +/-',E10.3,2X,0P,3F7.2)
      END
      SUBROUTINE GETXSG(LEN,MAT,MFI,MTI,IZAP,LFS,ICV,ZA,AWR,QM,QI
     &                 ,NG,MXG,ENG,XSG,XSD,MXC,COV,MXR,RWO,IER)
C-Title  : Subroutine GETXSG
C-Purpose: Retrieve group cross sections and covariances from ENDF
C-Description:
C-D  Parameters:
C-D  LEN   Unit for the ENDF file
C-D  MAT   Requested material
C-D      0 Retrieve the first material from the current position
C-D     >0 On input, MAT=10*ZA+LIS0 where
C-D        ZA = 1000*Z+A
C-D        Z    atomic number
C-D        A    mass number
C-D        LIS0 metastable state designation
C-D          0  ground state
C-D          1  first metastable state, etc.
C-D     <0 On input, MAT=|MAT| (ENDF material number)
C-D        On output, MAT is always negative, the absolute value
C-D        being the ENDF material number.
C-D  MFI   Requested ENDF file designation (MF=3 or 10)
C-D  MTI   Requested ENDF reaction designation
C-D  IZAP  Requested residual ZA designation (MF 10/40 only)
C-D  LFS   Requested residual final state    (MF 10/40 only)
C-D  ICV   Flag to retrieve covariance data
C-D      0 No covariance data are needed
C-D      1 Retrieve covariance data, if present
C-D  ZA    Material ZA designation
C-D  AWR   Atomic weight ratio (nuclide to neutron mass)
C-D  QM    Reaction Q-value
C-D  QI    Discrete state Q-value
C-D  NG    Number of groups (=number of energy boundaries - 1)
C-D  MXG   Maximum size of the cross section vector
C-D  ENG   Energy boundaries of the cross sections
C-D  XSG   Group cross sections [barns]
C-D  XSD   Cross section absolute uncertainties [barns]
C-D  MXC   Maximum order of the covariance matrix
C-D  COV   Correlation matrix
C-D  MXR   Maximum length of the scratch array
C-D  RWO   Scratch array
C-D  IER   Error flag:
C-D      0 Normal trermination
C-D      1 Specified material not found.
C-D      2 End-of-file before material found.
C-D      3 Read error.
C-D      4 Covariance energy grid much denser than x.s. grid (fatal)
C-D     11 Multiple ranges for the cross section table.
C-D     12 Cross sections not in histogram representation.
C-D     21 Covariance energy grid exceeds cross section grid.
C-D
C-D     WARNING - The covariance mesh cannot be condensed
C-D               It can only be expanded. If the covariance
C-D               mesh exceeds cross section mesh IER=22 is set.
C-D               If the mesh > 2*cross section mesh, the error
C-D               is fatal, IER=4 and processing is terminated.
C-
      DIMENSION ENG(MXG),XSG(MXG),XSD(MXG),COV(MXC,MXC),RWO(MXR)
      DIMENSION NBT(20),INR(20)
c...
c...  dimension et(9)
c...  data et/ 1.e-4, 2.9e6, 4.5e6, 8.e6, 11.e6
c... &       , 12.e6, 14.e6, 16.e6, 20.e6/
c...
C*
C* Initialise
      IER= 0
      ZA0=-MAT
      MF =MFI
      MT =MTI
C* Clear the covariance matrix
      DO I=1,MXC
        DO J=1,MXC
          COV(J,I)=0.d0
        END DO
      END DO
C*
C-F Search the ENDF file for the required cross section
c...
C...  print *,'Search mat,mf,mt,za0',mat,mf,mt,za0
c...
      IF(MAT.EQ.0 .AND. MT.EQ.0) THEN
        CALL FINDMT(LEN,ZA0,ZA,AWR,L1,L2,NS,N2,MAT,MF,MT,IER)
        IF(IER.NE.0) RETURN
        MT =0
      ELSE
        CALL FINDMT(LEN,ZA0,ZA,AWR,L1,L2,NS,N2,MAT,MF,MT,IER)
        IF(IER.NE.0) RETURN
      END IF
c...
C...  print *,'mat,mf,mt,ns,za,awr',mat,mf,mt,ns,za,awr
c...
C-F Read the cross section
      IS=0
   20 IS=IS+1
      CALL RDTAB1(LEN,QM,QI,JZAP,JFS,NR,NEX,NBT,INR,ENG,XSG,MXG,IER)
c...
c...  print *,'     is,jzap,jfs,izap,nex',is,jzap,jfs,izap,nex
c...
      IF(IS.LT.NS .AND. (JZAP.NE.IZAP .OR. JFS.NE.LFS)) GO TO 20
      ETH=ENG(1)
      DO I=1,NEX
        XSD(I)=0
      END DO
      NG=NEX-1
      IF(NR.NE.1) THEN
        IER=11
        IF(INR(1).NE.1) IER=12
        RETURN
      END IF
c...
C...  Force test group structure
C...  nex=9
C...  ng=nex-1
C...  do i=1,nex
C...    eng(i)=et(i)
C...  end do
C...  print *,'nex',nex
C...  print *,(et(i),i=1,nex)
c...
      IF(ICV.LT.1) RETURN
C*
C-F Search for the covariance data
      IF(MF.EQ.10) THEN
        MF=40
      ELSE
        MF=33
      END IF
      MMA= MAT
      MMF= MF
      MMT= MT
      CALL FINDMT(LEN,ZA0,ZZ,AA,L1,L2,N1,NL,MMA,MMF,MMT,IER)
      IF(IER.NE.0) THEN
c...    WRITE(*,*) 'GETXSG WARNING - No covariance data'
c... &           ,' for MAT',NINT(ZA0),' MT',MT
        IER=0
        RETURN
      END IF
C* Reserve LB8 contribution and clear the covariance matrix
      LB8=1
      DO I=1,NG
        RWO(LB8-1+I)=0
      END DO
      LBX= LB8+NG
      LB8=-LB8
C*
      NS=1
      IF(MF.EQ.40) NS=N1
C*    -- Loop over product nuclides and final states
      IS=0
   40 IS=IS+1
        IF(MF.EQ.40) THEN
          CALL RDHEAD(LEN,MAT,MF,MT,QM,QI,JZAP,JFS,N1,NL,IER)
        END IF
        IF(NL.GT.1) THEN
          PRINT *,'GETXSG WARNING - No. of cov. subsect. is',NL
     &           ,' for MAT',NINT(ZA0),' MT',MT
          PRINT *,'                 Only the first will be taken'
        END IF
        CALL RDHEAD(LEN,MAT,MF,MT,XMF1,XLFS1,MAT1,MT1,NC,NI,IER)
        IF(NC.GT.0) THEN
          PRINT *,'WARNING - NC sections found',NC
     &           ,' for MAT',NINT(ZA0),' MT',MT
          PRINT *,'          They are ignored'
        END IF
C*
C* Read the covariance matrix and store the results at addresses
C* LC1 covariance energy grid
C* LC2 unpacked covariance matrix on original energy grid LC1
C* LC3 unpacked covariance matrix on cross section energy grid ENG
        LC3=LBX
        MEX=MAX(NG,NEX-1)
c...    LBL=LC3+MEX*MEX
        LBL=LC3+MEX*(MEX*2)
c...
c...    print *,'ng,nex,lbx',ng,nex,lbx,lbl
c...
        DO LI=1,NI
          KXR=MXR-LBL
          CALL RDLIST(LEN,C1,C2,LS,LB,NT,NE,RWO(LBL),KXR,IER)
          IF(IER.NE.0) THEN
            IF(IER.EQ.22) THEN
              PRINT *,'GETXSG WARNING - Covariance mesh'
     &               ,' exceeds cross section mesh'
              IER=0
            ELSE
              PRINT *,'GETXSG ERROR - reading RDLIST',IER
     &               ,' for MAT',NINT(ZA0),' MT',MT
              STOP 'GETXSG ERROR - reading RDLIST'
            END IF
          END IF
          IF(NE.GT.NG) THEN
            IER=0
            IF(NE.GT.NG*2) THEN
              IER=4
              RETURN
            END IF
          END IF
          LC1=LBL
          LC2=LC1+NT
          LL =LC2+(NE -1)*(NE -1)
c...
c...      print *,'lc1,lc2,lc3',lc1,lc2,lc3
c...      print *,'egb',(rwo(lc1-1+j),j=1,5)
c...
          IF(LL.GT.MXR) THEN
            PRINT *,'GETXSG ERROR - MXR limit exceeded',LL
     &             ,' for MAT',NINT(ZA0),' MT',MT
            STOP 'GETXSG ERROR - MXR limit exceeded'
          END IF
C*      
          IF     (LB.EQ.1) THEN
            DO I=LC2,LL
              RWO(I)=0
            END DO
            RWO(LC1)=RWO(LBL)
            DO I=2,NE
              RWO(LC2+(I-2)*(NE-1)+(I-2))=RWO(LBL+2*(I-2)+1)
              RWO(LC1-1+I)=RWO(LBL+2*(I-2)+2)
            END DO
C*      
          ELSE IF(LB.EQ.5) THEN
            JJ=LBL+NE-1
            DO I=2,NE
              J0=I
              IF(LS.EQ.0) J0=2
              DO J=J0,NE
                JJ=JJ+1
                RR=RWO(JJ)
                J2=LC2+(J-2)+(I-2)*(NE-1)
                RWO(J2)=RR
                IF(LS.EQ.1) THEN
                  J2=LC2+(I-2)+(J-2)*(NE-1)
                  RWO(J2)=RR
                END IF
              END DO
            END DO
          ELSE IF(LB.EQ.8) THEN
            LB8=ABS(LB8)
            K=1
            EK2=RWO(LBL)
            FK =RWO(LBL+1)
            EI2=ENG(1)
            DO I=1,NG
              EI1=EI2
              EI2=ENG(I+1)
              DI =EI2-EI1
              FF =0
              IF(EK2.GT.EI1 .AND. K.GT.1) GO TO 82
C*              -- Increment K until [Ek1:Ek2] overlaps with [Ei1,Ei2]
   80           IF(K.GE.NE) EXIT
                K =K+1
                EK1=EK2
                EK2=RWO(LBL+2*(K-1))
                DK =EK2-EK1
                FL =RWO(LBL+2*(K-1)-1)
                IF(EK2.LE.EI1) GO TO 80
   82         CONTINUE
              EF1=MAX(EK1,EI1)
              EF2=MIN(EK2,EI2)
              FF =FF + FL*(DK/DI)*(EF2-EF1)/DI
              IF(EK2.LT.EI2) GO TO 80
              RWO(LB8-1+I)=RWO(LB8-1+I)+FF
            END DO
            EXIT
          ELSE
            PRINT *,'GETXSG WARNING - Unsupported LB',LB
     &             ,' for MAT',NINT(ZA0),' MT',MT
          END IF
c...
c...      print *,'cov',(rwo(lc2-1+j),j=1,5)
c...
C-F Expand the covariance matrix, if necessary
          NGX=NEX-1
          NGC=NE -1
c...
c...      print *,'ne,nex,ng',ne,nex,ng
c...      print *,(rwo(lc1-1+i),i=1,5)
c...
          CALL COVEXP(ETH,NE,RWO(LC1),NEX,ENG,NGC,RWO(LC2),NG,RWO(LC3))
c...
c...      print *,'ne,nex,ng',ne,nex,ng
c...      print *,(rwo(lc1-1+i),i=1,5)
c...
          JJ=LC3
          DO I=1,NG
            DO J=1,NG
              COV(J,I)=COV(J,I)+RWO(JJ)
              JJ=JJ+1
            END DO
          END DO
c...
c...      print *,'Row 1',(cov(j,1),j=1,5)
c...
        END DO
      IF(IS.LT.NS .AND. (JZAP.NE.IZAP .OR. JFS.NE.LFS)) THEN
C*      -- Clear the covariance matrix and try next set
        DO I=1,MXC
          DO J=1,MXC
            COV(J,I)=0.d0
          END DO
        END DO
        GO TO 40
      END IF
C*
C-F Add LB8 contribution and convert to absolute uncertainty
      DO I=1,NG
        IF(LB8.GT.0) THEN
          COV(I,I)=COV(I,I)+RWO(LB8-1+I)
        END IF
        IF(COV(I,I).LT.0) THEN
          PRINT *,'GETXSG WARNING - Negative variance',COV(I,I)
     &           ,' for MAT',NINT(ZA0),' MT',MT,' Group',I
          COV(I,I)=0
        END IF
        XSD(I)=SQRT(COV(I,I))*XSG(I)
      END DO
      XSD(NG+1)=0
      RETURN
      END
      SUBROUTINE COVEXP(ETH,NE1,EN1,NE2,EN2,MC1,CV1,MC2,CV2)
C-Title  : Subroutine COVEXP
C-Purpose: Expand covariance matrix to output energy grid
C-Description:
C-D  ETH  Threshold energy
C-D  NE1  Number of energy points in input grid
C-D  EN1  Energies of the input grid
C-D  NE2  Number of energies in the output grid
C-D  EN2  Energies of the output grid
C-D  MC1  Maximum order of covariance matrix CV1
C-D  CV1  Input covariance matrix CV1(MC1,MC1)
C-D  MC2  Maximum order of covariance matrix CV2
C-D  CV2  Output covariance matrix CV2(MC2,MC2)
C-
      PARAMETER (MXW=2000)
      DIMENSION EN1(NE1),EN2(NE2),CV1(MC1,MC1),CV2(MC2,*)
      DIMENSION RWO(MXW)
      LX=1
      LC=LX+NE2
      IF(LC+NE2.GT.MXW) THEN
        PRINT *,'COVEXP ERROR - MXR limit exceeded'
        PRINT *,'       available',MXW
        PRINT *,'       requested',LC+NE2
        STOP 'COVEXP ERROR - MXW limit exceeded'
      END IF
C...
C...      PRINT *,'NE2,MC1',NE2,MC1
C...      DO I=1,MC1
C...        DO J=1,MC1
C...          IF(CV1(J,I).GT.0) THEN
C...            PRINT *,J,I,CV1(J,I)
C...            STOP
C...          END IF
C...        END DO
C...      END DO
C...      STOP
C...      DO I=160,170
C...        PRINT *,I,EN1(I),(CV1(J,I),J=1,5)
C...      END DO
C...
C*
C-F Process each row in turn
C...
C...  print *,'ne1,ne2',ne1,ne2
C...
      DO J=2,NE1
C*      -- Expand the covariances of one row to the output grid
        CALL ROWEXP(ETH,NE1,EN1,MC1,CV1(1,J-1),NE2,EN2,MC2,RWO(LX))
c...
c...    print *,'Row',J-1,(rwo(lx-2+k),k=2,ne2)
c...    print *,' '
C...
C*      -- Save the row into the output array
        DO K=2,NE2
          CV2(K-1,J-1)=RWO(LX-2+K)
        END DO
      END DO
C*
C-F Process each column in turn

c     NPR=MIN(9,NE2-1)
c     print *,'Npr',Npr
c     CALL MTXPRT(6,LDG,1,NPR,mc2,cv2)

      DO J=2,NE2
        DO K=2,NE1
          RWO(LC-2+K)=CV2(J-1,K-1)
        END DO

C       print *,'Col',J-1,(rwo(lc-2+k),k=2,ne1)
C       print *,' '

C*      -- Expand the covariances of one column to the output grid
        CALL ROWEXP(ETH,NE1,EN1,MC1,RWO(LC),NE2,EN2,MC2,RWO(LX))
C*      -- Save the column
        DO K=2,NE2
          CV2(J-1,K-1)=RWO(LX-2+K)
        END DO
      END DO
      RETURN
      END
      SUBROUTINE ROWEXP(ETH,NE1,EN1,NC1,CV1,NE2,EN2,NC2,CV2)
C-Title  : Subroutine ROWEXP
C-Purpose: Expand covariance matrix row to union grid
      DIMENSION EN1(NE1),CV1(NC1),EN2(NE2),CV2(NC2)
C* Clear the covariance vector
      DO J=2,NE2
        CV2(J-1)=0
      END DO
      IF(NE1.LT.2) RETURN
C*
C* Initial energy grid points
      I  =2
      EI1=EN1(I-1)
      EI2=EN1(I  )
C*
C* Loop over union grid points
      J  =1
   20 J  =J+1
      EJ1=EN2(J-1)
      EJ2=EN2(J)

c     print *,'Element',j-1,ej1,ej2,EI1,EI2,CV1(I-1)

C*    -- Check union grid intervals below input grid
      IF(EJ2.LE.EI1) THEN

c       print *,'  Zero ej2=<ei1',ej2,ei1

        CV2(J-1)=0
        IF(J.LT.NE2) GO TO 20
        GO TO 80
      END IF
C*    -- Check union grid interval above current interval
   22 IF(EJ1.GE.EI2) THEN
C*      -- Increment interval if not the last
        IF(I.LT.NE1) THEN
          I  =I+1
          EI1=EN1(I-1)
          EI2=EN1(I  )

c         print *,'      Increment_2 I',i-1,ei1,ei2

          GO TO 22
        END IF
C*      -- If last, set remaining union grid intervals to zero

c       print *,'  Zero ej1>=ei2',ej1,ei2

        CV2(J-1)=0
        IF(J.LT.NE2) GO TO 20
        GO TO 80
      END IF
      GO TO 32
C*
C* Add contribution from EI1-EI2 to EJ1-EJ2
   30 I  =I+1
      EI1=EN1(I-1)
      EI2=EN1(I  )

c     print *,'      Increment_1 I',i-1,ei1,ei2

   32 CC=CV1(I-1)
      IF(EI1.GE.EJ1) THEN
C*      -- Lower bound of input interval within output interval
        IF(EJ2.GT.ETH) THEN
          EK1=MAX(EJ1,EI1,ETH)
          EK2=MIN(EJ2,EI2)
          EKK=MAX(EJ1,ETH)
          IF(I.EQ.2) EK1=EKK
          CC=CC*(EK2-EK1)/(EJ2-EKK)
          CV2(J-1)=CV2(J-1)+CC

c         print *,'Add2',i-1,j-1,CV2(j-1),cc,ek1,ek2

        ELSE
          CV2(J-1)=0
        END IF
        IF(EI2.LT.EJ2 .AND. I.LT.NE1) GO TO 30
        IF(J.LT.NE2) GO TO 20
        GO TO 80
      END IF
      IF(EI2.LT.EJ2) THEN
C*      -- Upper bound of input interval within output interval
        IF(EJ2.GT.ETH) THEN
          EK1=MAX(EJ1,EI1,ETH)
          EK2=MIN(EJ2,EI2)
          CC =CC*(EK2-EK1)/(EJ2-EJ1)
          CV2(J-1)=CV2(J-1)+CC

c         print *,'Add1',i-1,j-1,CV2(j-1),cc

        ELSE
          CV2(J-1)=0
        END IF
        IF(I.LT.NE1) GO TO 30
        IF(J.LT.NE2) GO TO 20
        GO TO 80
      END IF
C*    -- Interval EJ1-EJ2 wholy contained in EI1-EI2
      CV2(J-1)=CC

c       print *,'Add3',i-1,j-1,CV2(j-1),cc

      IF(J.LT.NE2) GO TO 20
   80 RETURN
      END
      SUBROUTINE PRTCUR(LCU,HDR,ETH,NG,EN1,XSG,XSD,IFL)
C-Title  : Subroutine PRTCUR
C-Purpose: Write a tabulated function to the CURves file
C-Description
C-D  LCU  Logical output unit
C-D  HDR  Header label
C-D  ETH  Threshold from which to plot the data
C-D  NG   Number of points
C-D  EN1  Argument array
C-D  XSG  Function array
C-D  XSD  Uncertainty array
C-D  IFL  Control flag, which can take the following values
C-D        0  Normal tabulated function (zero-function values are
C-D           suppressed
C-D        1  Histogram representation; the i-th function value
C-D           is assumed between the i-th and i+1-th argument value.
C-
      CHARACTER*(*) HDR
      DIMENSION EN1(NG),XSG(NG),XSD(NG)
      WRITE(LCU,*) HDR
      IF(ETH.GT.0) THEN
        WRITE(LCU,901) ETH, 0., 0.
      END IF
      IP =0
      DO I=1,NG-1
        EE=EN1(I+1)
        IF(EE.GE.ETH) THEN
          IF(IP.EQ.0 .AND. I.GT.1)
     &    WRITE(LCU,901) EN1(I),0.,0.
          IF(IFL.EQ.1)
     &    WRITE(LCU,901) EN1(I  ),XSG(I),XSD(I)
          WRITE(LCU,901) EN1(I+1),XSG(I),XSD(I)
          IP=IP+1
        END IF
      END DO
      WRITE(LCU,*) ' '
      RETURN
  901 FORMAT(1P,3E11.4)
      END
      SUBROUTINE SRTINM(N,M,X)
C-Title  : SRTINM subroutine
C-Purpose: Perform a sort (on array indices) by Insertion method
C-Description:
C-D Sort the vector of N real numbers in X in ascending order
C-D The actual entries in X remain unaffected, but on exit,
C-D M (integer array) contains the addresses of the consecutive 
C-D array entries to produce a sorted sequence,
C-Author : A.Trkov, Institute J.Stefan, Ljubljana, Slovenia (1990)
      DIMENSION M(*),X(*)
      M(1)=1
      DO 20 J=2,N
      M(J)=J
      K = J
      T = X(J)
   10 L = M(K-1)
      IF(X(L).LE.T) GO TO 12
      M(K)=L
      K   = K-1
      IF(K.GT.1) GO TO 10
   12 M(K)=J
   20 CONTINUE
      RETURN
      END
      SUBROUTINE MTXPRT(LOU,LDG,NP0,NPR,NGC,RMT)
C-Title  : MTXPRT subroutine
C-Purpose: Print a matrix to unit LOU
C-Author : A.Trkov, J.Stefan Institute, Ljubljana, Slovenia
      DIMENSION RMT(NGC,NGC)
      DO I=1,NPR
        WRITE(LOU,901) NP0-1+I,(RMT(NP0-1+J,NP0-1+I),J=1,npr)
      END DO
      RETURN
  901 FORMAT(I4,1P,8E9.3E1:/1(4X,8E9.3E1))
      END
      SUBROUTINE MTXPRD(A,B,C,N,M,L)
C-Title  : MTXPRD subroutine
C-Purpose: Calculate a product of two matrices
C-Author : A.Trkov, J.Stefan Institute, Ljubljana, Slovenia
C-Version: 1988 original code
C-V  10/01 Generalise for rectangular matrices
C-Description:
C-D Matrix multiplication A(N,M).B(L,N) = C(L,M)
C-D If two rectangular matrices are multiplied A(N,M).B(L,N), then L=M
C-D If matrix A(N,N) is multiplied by a vector B(N), then N=M, L=1
C-
      DOUBLE PRECISION S
      DIMENSION A(N,M),B(L,N),C(L,M)
      DO I=1,L
        DO J=1,M
          S = 0
          DO K=1,N
            S = S + DBLE(A(K,J))*DBLE(B(I,K))
          END DO
          C(I,J) = S
        END DO
      END DO
      RETURN
      END
      SUBROUTINE MTXSUM(A,B,C,N,M)
C-Title  : MTXSUM subroutine
C-Purpose: Calculate a sum of two matrices
C-Author : A.Trkov, J.Stefan Institute, Ljubljana, Slovenia
C-Version: 2010 original code
C-Description:
C-D Matrix summation A(N,M)+B(N,M) = C(N,M)
C-
      DIMENSION A(N,M),B(N,M),C(N,M)
      DO I=1,M
        DO J=1,N
          C(J,I) = A(J,I)+B(J,I)
        END DO
      END DO
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
      DIMENSION    NBT(N1),INR(N1)
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
      SUBROUTINE FINDMT(LEF,ZA0,ZA,AW,L1,L2,N1,N2,MAT,MF,MT,IER)
C-Title  : Subroutine FINDMT
C-Purpose: Find specified reaction in an ENDF file
C-Description:
C-D  The routine finds the specified material, file or section
C-D  in an ENDF file. The material may be characterised by the
C-D  ZA0 number defined as Z*1000+A+LIS0/10 (the decimal point
C-D  allows for isomeric states). Alternately, if ZA0<0, the
C-D  absolute integer value is interpreted as the required MAT
C-D  number. If MF and MT are non-zero, the file is scanned
C-D  until the value on the file matches the input value.
C-D
C-D  Notes:
C-D  - The search for metastable states by ZA0 is only possible
C-D    if MF1 MT451 data are on the file.
C-D  - In this case the actual file position is on the second
C-D    record of this section.
C-D  - Once the required ZA is identified, the ZA0 value is
C-D    redefined to -MAT to search by the MAT number in
C-D    consecutive searches, if required.
C-D
C-D  Error flags:
C-D  IER = 0  Normal termination.
C-D        1  Specified material not found.
C-D        2  End-of-file before material found.
C-D        3  Read error.
C-
      CHARACTER*66 C66
C* Initialise
      IER= 0
      MF0=MF
      MT0=MT
      MF =-1
      MT =-1
      MMM=-1
      ZA = 0
      IF     (ZA0.LT.0) THEN
        MAT0=-ZA0+0.1
      ELSE IF(ZA0.GT.0) THEN
        IZA0=ZA0*10
      ELSE
        CALL RDTEXT(LEF,MAT,MF,MT,C66,IER)
        IF(IER.GT.0) GO TO 80
        IF(MF.EQ.0 .AND. MT.EQ.0) CALL RDTEXT(LEF,MAT,MF,MT,C66,IER)
        MAT0=-1
        GO TO 21
      END IF
C*
C* Loop to find the specified material
   20 CALL RDTEXT(LEF,MAT,MF,MT,C66,IER)
      IF(IER.GT.0 .OR. MAT.LT.0) GO TO 80
      IF(ZA0.LT.0) THEN
C* Case: Search by MAT number
        IF(MAT.NE.MAT0) GO TO 20
      ELSE
C* Case: Search by ZA number (including decimal LIS0)
        IF(MT.EQ.  0) GO TO 20
        IF(MT.EQ.MMM) GO TO 20
        MMM=MT
        READ (C66,92) ZA
        IZA=ZA*10
        IF(MF.EQ.1. AND. MT.EQ.451) THEN
          READ (LEF,92) DD,DD,LIS,LIS0
          IZA=IZA+LIS0
        END IF
        IF(IZA.NE.IZA0) GO TO 20
        ZA=IZA*0.1
        ZA0=-MAT
      END IF
C* Loop to find the file number
   21 IF(MF0.EQ. 0) GO TO 30
   22 IF(MF0.EQ.MF) GO TO 30
      CALL RDTEXT(LEF,MAT,MF,MT,C66,IER)
      IF(IER.GT.0 .OR. MAT.LE.0) GO TO 80
      GO TO 22
C* Loop to find the reaction type number
   30 IF(MT0.EQ. 0) GO TO 40
   32 IF(MT0.EQ.MT) GO TO 40
      CALL RDTEXT(LEF,MAT,MF,MT,C66,IER)
      IF(IER.GT.0 .OR. MAT.LE.0) GO TO 80
      IF(MF0.GT.0 .AND. MF.GT.MF0) GO TO 20
      GO TO 32
C* Normal termination
   40 READ (C66,92) ZA,AW,L1,L2,N1,N2
      RETURN
C*
C* Error traps
   80 IER=IER+1
      RETURN
C*
   92 FORMAT(2F11.0,4I11.0,I4,I2,I3,I5)
      END
      SUBROUTINE SKIPSC(LEF)
C-Title  : Subroutine SKIPSC
C-Purpose: Skip current section in an ENDF file
C-
      CHARACTER*66 C66
   20 CALL RDTEXT(LEF,MAT,MF,MT,C66,IER)
      IF(MT.NE.0) GO TO 20
      RETURN
      END
      SUBROUTINE CH8PCK(CH,R)
C-Title  : CH8PCK subroutine
C-Purpose: Write a real number R into CH string, 8 characters long
C-Author : A.Trkov, Institute Jozef Stefan, Ljubljana, Slovenia (1990)
      CHARACTER*8 CH
      IF( R .NE. 0) GO TO 10
      CH ='       0'
      RETURN
   10 AR =ABS(R)
      IF( AR .GE. 1.E-2 .AND. AR .LT. 1.E6) GO TO 20
      F=R
      M=0
   12 IF(ABS(F).LT.10) GO TO 14
      F=F/10
      M=M+1
      GO TO 12
   14 IF(ABS(F).GE. 1) GO TO 16
      F=F*10
      M=M-1
      GO TO 14
   16 WRITE(CH,21) F
      IF(IABS(M).LT.10) WRITE(CH(7:8),22) M
      IF(IABS(M).GE.10) WRITE(CH(6:8),23) M
      RETURN
   20 IF( AR .GT. 9.99999) GO TO 30
      WRITE(CH,21) R
      IF( CH(8:8) .NE. '0' ) RETURN
   30 IF( AR .GT. 99.9999) GO TO 40
      WRITE(CH,31) R
      IF( CH(8:8) .NE. '0' ) RETURN
   40 IF( AR .GT. 999.999) GO TO 50
      WRITE(CH,41) R
      IF( CH(8:8) .NE. '0' ) RETURN
   50 IF( AR .GT. 9999.99) GO TO 60
      WRITE(CH,51) R
      IF( CH(8:8) .NE. '0' ) RETURN
   60 IF( AR .GT. 99999.9) GO TO 70
      WRITE(CH,61) R
      IF( CH(8:8) .NE. '0' ) RETURN
   70 IR=R+0.5
      WRITE(CH,71) IR
      RETURN
   21 FORMAT(F8.5)
   22 FORMAT(SP,I2)
   23 FORMAT(SP,I3)
   31 FORMAT(F8.4)
   41 FORMAT(F8.3)
   51 FORMAT(F8.2)
   61 FORMAT(F8.1)
   71 FORMAT(I8)
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
