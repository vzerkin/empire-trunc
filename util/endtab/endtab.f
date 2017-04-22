      PROGRAM ENDTAB
C-Title  : Program ENDTAB
C-Purpose: Convert ENDF tabulated data to PLOTTAB format
C-Version: Original code October 2000 (Major revision of ENTOPT code).
C-V  02/07 Changes for compatibility with revised DXSEND routine.
C-V  05/09 Fix undefined variables (problems on Linux).
C-V  06/02 Update for consistency with DXSEND upgrade to provide
C-V        uncertainties, if available.
C-V  06/11 Upgrade to retrieve cross sections at fixed angle.
C-V  07/12 Increase MXR from 400000 to 1200000 (A. Trkov).
C-V  08/01 Call DXSELM instead of DXSEND to allow elemental
C-V        cross section reconstruction.
C-V  08/04 Increase MXR from 4 000 000 to 6 000 000 (A.Trkov)
C-V  08/11 Allow filenames of length up to 80 characters (A. Trkov)
C-V  11/07 - Allow comment to be redefined from input (A. Trkov)
C-V        - Fix definition of metastable states.
C-V        - Increase MPT from 200000 to 1200000.
C-V  11/11 Print more digits for energy to curves file.
C-V  12/02 Fix upper energy cutoff
C-V  12/03 Fix processing of nu-bar
C-V  12/05 Increase precision of argument printout for 1E6<E<1e8
C-V  12/11 Fix setting the error flag IER
C-V  12/02 Implement retrieval of cross sections at fixed angle
C-V        for incident neutrons
C-V  13/06 Trivial fix to retrieve MF3/MT5 as is
C-V  14/02 Re-design to accurately print values at interval boundaries.
C-V  14/03 Guard against upper boundary outside data range.
C-V  14/05 Fix typo (applicable when Ehi > last point on file).
C-V  14/06 Implement ratios of fission spectra to Maxwellian.
C-V  15/03 Fix bug printing NaN for PFNS ratio to Maxwellian at 0 eV.
C-V  15/11 Print input instructions to screen.
C-V  16/03 Allow MF203 for alpha (SigC/SigF).
C-V  16/07 Fix missing "REWIND" before calling DXSEND for MF5.
C-V  16/12 Fix printout of the backward angle in the distributions.
C-V  17/04 Reset upper energy for plotting in multiple plots.
C-Author : Andrej Trkov,  International Atomic Energy Agency
C-A                email: Andrej.Trkov@ijs.si
C-A      Current address: Jozef Stefan Institute
C-A                       Jamova 39
C-A                       1000 Ljubljana
C-A                       Slovenia
C-M  
C-M  Program ENDTAB Users' Guide
C-M  ===========================
C-M
C-M  The program is a descendant of the ENTOPT code and is designed
C-M  to convert pointwise data in an ENDF file into PLOTTAB format.
C-M  In addition to the capability of ENTOPT to process simple cross
C-M  section data, ENDTAB can also retrieve the double differential
C-M  data from Law 7 representation in an ENDF file.
C-M
C-M  The main program is the driver for the DXSEND routine package,
C-M  which does the actual data retrieval from an ENDF file. Since
C-M  the file needs to be scanned several times, particularly when
C-M  assembling differential particle emission data, it is advisable
C-M  to operate ENDTAB on single material files. Codes like GETMAT
C-M  can be used to extract single materials from a master library.
C-M
C-M  Instructions:
C-M  Input instructions are entered interactively in response to the
C-M  prompts as follows:
C-M  - Source ENDF filenames.
C-M  - Tabulated output in PLOTTAB format.
C-M  - MAT Required material identification: this can be specified in
C-M    several forms:
C-M     0  - Processing depends on the selected MF number:
C-M           MF=3 All materials on the file corresponding to
C-M                the selected MT number are processed.
C-M           MF=6 The first material on the file is processed.
C-M    >0  - The input entry designates the ZA number of the
C-M          required nuclide where ZA=1000*Z+A+0.1*LIS0 where
C-M          Z is the atomic number, A the mass number and LIS0
C-M          the isomeric state identifier (=0 for ground state).
C-M          Note that the ZA search for isomeric states is only
C-M          possible on files with ENDF file MF1 setion MT 451
C-M          present.
C-M          Note: On first pass the ZA search identifier is
C-M                converted to the equivalent MAT search identifier.
C-M    <0  - The input entry is the MAT number of the required
C-M          nuclide.
C-M  - MF Required ENDF file MF number:
C-M     3  - Request cross section data.
C-M     4  - Request angular distribution data.
C-M     5  - Request outgoing particle energy distribution data.
C-M     6  - Request double differential data.
C-M   203  - Request cross section ratios.
C-M          (Currently only MT102=SigC/SigF is supported).
C-M    Note: The requested MF number implies the type of output and
C-M          need not coincide with the data actually on the file.
C-M          For example, requesting MF 4 will give outgoing particle
C-M          energy integrated angular distributions when MF 6 data
C-M          are present. Conversely, requesting MF 6 the double
C-M          differential data will be reconstruncted from MF 4/5
C-M          data.
C-M
C-M  Input requests that follow depend on the selected MF number.
C-M
C-M  Case MF=3:
C-M  - ELO Lower energy bound for the retrieved cross sections [eV],
C-M    default is the lower bound on the file.
C-M  - EHI Upper energy bound for the retrieved cross sections [eV],
C-M    default is the upper bound on the file.
C-M  - EPS Fractional relative difference between consecutive energy
C-M    points for thinning the data on the output list, By
C-M    default no thinning is performed.
C-M      EPS=0, double points are suppressed.
C-M      EPS<0, all points are retained.
C-M  - MT/COM Required ENDF reaction MT number and data header COM.
C-M    COM is defined in columns 11-50. If blank, the default comment
C-M    structure is used. Special features:
C-M      MT>40000  Cross section at fixed outgoing angle is requested.
C-M                The outgoind particla ZA designation and the angle
C-M                (in degrees) are requested from input.
C-M      MT=261    The temperature of the Maxwellian fission spectrum
C-M                is requested if ratio to Maxwellian is to be
C-M                printed to output.
C-M    The program then cycles back to request additional MT numbers
C-M    to be processed and added to the same output file. The list is
C-M    terminated by zero. Valid entries are those that actually
C-M    appear on the ENDF file. (Use FIXUP [1] of the PrePro codes to
C-M    generate redundant cross sections and other quantities).
C-M
C-M  Case MF=4:
C-M  - ZA Outgoing particle ZA designation.
C-M    Note: at present the program is tested only for neutrons,
C-M          ZA=1.
C-M  - EIN Incident particle energy [eV].
C-M  - MT/COM Required ENDF reaction MT number. The program cycles
C-M    back to request additional MT numbers to be processed and
C-M    added to the same output file. The list is terminated by
C-M    zero. Valid entries are those that actually appear on
C-M    the ENDF file under MF=4 or 6. A special meaning is assigned
C-M    to MT=5: cross sections for all partial reactions producing
C-M    the selected outgoing particles are summed.
C-M    COM - the same convention as described for MF=3 applies.
C-M
C-M  Case MF=5:
C-M  - ZA Outgoing particle ZA designation.
C-M    Note: at present the program is tested only for neutrons,
C-M          ZA=1.
C-M  - EIN Incident particle energy [eV].
C-M  - MT/COM Required ENDF reaction MT number. The program cycles
C-M    back to request additional MT numbers to be processed and
C-M    added to the same output file. The list is terminated by
C-M    zero. Valid entries are those that actually appear on
C-M    the ENDF file under MF=4 or 6. A special meaning is assigned
C-M    to MT=5: cross sections for all partial reactions producing
C-M    the selected outgoing particles are summed.
C-M    COM - the same convention as described for MF=3 applies.
C-M    Special case:
C-M      MT=18     The temperature of the Maxwellian fission spectrum
C-M                is requested if ratio to Maxwellian is to be
C-M                printed to output.
C-M  Case MF=6:
C-M  - ZA Outgoing particle ZA designation.
C-M    Note: at present the program is tested only for neutrons,
C-M          ZA=1.
C-M  - EIN Incident particle energy [eV].
C-M  - ANG Scattered particle angle [Degrees].
C-M  - RES Resolution broadening fraction. This parameter is needed 
C-M    to define final width of the energy distribution of the
C-M    scattered particle in elastic or discrete inelastic
C-M    scattering reaction. The default value 0.02 is used.
C-M  - MT/COM Required ENDF reaction MT number. The program cycles
C-M    back to request additional MT numbers to be processed and
C-M    added to the same output file. The list is terminated by
C-M    zero. Valid entries are those that actually appear on
C-M    the ENDF file under MF=4 or 6. A special meaning is assigned
C-M    to MT=5: cross sections for all partial reactions producing
C-M    the selected outgoing particles are summed.
C-M    COM - the same convention as described for MF=3 applies.
C-M
C-M    Note: - Contrary to the MF=3 case, the MT numbers for
C-M            MF 4-6 can be entered in any order.
C-M          - Option MT=5 is only applicable to neutrons for
C-M            MF 4-6 at present.
C-M
C-M  Output format:
C-M  The data are written to output in the PLOTTAB "curves" format:
C-M  the header comment is followed by any number of records
C-M  containing data pairs (argument, function). Each entry of the
C-M  pair is 11 columns wide. A data set is terminated by the
C-M  end-of-file mark or by a blank record. The header comment
C-M  of the next set may follow. There may be any number of data
C-M  sets in a file.
C-M
C-M  The default comment header structure differs for MF 3 and MF 6.
C-M  For MF 3 it is a string of the form:
C-M  "MATnnn MFnn MTnnn" where "n" stands for the appropriate
C-M  numerical digits representing the MAT, MF and MT numbers
C-M  for the selected reactions.
C-M  For MF 6 the string has the form:
C-M  "MTnnn MeVnnn.n Degnnn" where "n" stands for the digits
C-M  representing the reaction MT number, the incident particle
C-M  energy and the scattered particle angle.
C-M
C-
      PARAMETER   (MPT=1200000,MXR=6000000,MXIS=24)
      CHARACTER*1  UN,UO
      CHARACTER*11 E11
      CHARACTER*40 BLNK,COM,COM1
      CHARACTER*80 FLNM,FLIN,FLOU
      CHARACTER*66 REC
      DOUBLE PRECISION ED
      DIMENSION    ES(MPT),SG(MPT),UG(MPT),RWO(MXR),ZEL(MXIS),FRC(MXIS)
     1            ,NBT(20),INR(20)
C* Default logical file units
      DATA LIN,LOU,LKB,LTT
     1    /  1,  2,  5,  6 /
C* Default ENDF file and reaction type numbers
      DATA MF,MT/ 3,  102 /
      DATA EPS,MT0/-.1,0/
      DATA EA0,EB0/1.E-5,200.E6/
      DATA BLNK/'                                        '/
C*
      DATA PI/3.1415926/
C* Write banner
      WRITE(LTT,91)
      WRITE(LTT,91) ' ENDTAB - Extract Data from ENDF files  '
      WRITE(LTT,91) ' -------------------------------------  '
      WRITE(LTT,91)
C* Define the files
   12 WRITE(LTT,91) '$Enter the ENDF source filename       : '
      READ (LKB,99) FLNM
      IF(FLNM(1:40).NE.BLNK) FLIN=FLNM
      OPEN (UNIT=LIN,FILE=FLIN,STATUS='OLD',ERR=12)
      WRITE(LTT,91) '$Enter the PLOTTAB output filename    : '
      READ (LKB,99) FLNM
      IF(FLNM(1:40).NE.BLNK) FLOU=FLNM
      OPEN (UNIT=LOU,FILE=FLOU,STATUS='UNKNOWN')
      WRITE(LTT,91) ' ENDF Source file                       '
      WRITE(LTT,99) '   '//FLIN
      WRITE(LTT,91) ' Output curves file                     '
      WRITE(LTT,99) '   '//FLOU
      WRITE(LTT,99) ' '
C* Select the material
   13 WRITE(LTT,91) '$Enter requested ZA(>0) or MAT(<0) No.: '
      READ (LKB,98,ERR=13) ZA0
      WRITE(LTT,991) ' Selected ZA                          : ',ZA0
C* Select the ENDF file (3, 6, 10 or 15)
   14 WRITE(LTT,91) '$Enter the requested ENDF file MF No. : '
      READ (LKB,97,ERR=14) IDMY
      IF(IDMY.GT.0) MF0=IDMY
      WRITE(LTT,91) ' Selected MF                          : ',MF0
      DEG=-1
      EOU=-1
      ZAP=-1
C* Case: MF3/10 Energy range and grid thinning criterion
      IF(MF0. EQ. 1 .OR. MF0.EQ.3 .OR. MF0.EQ.10 .OR.
     &   MF0.EQ.23 .OR. MF0.EQ.203) THEN
        KEA=0
C* Select the energy interval
        WRITE(LTT,91) '$Enter the lower energy bound [eV]    : '
        READ (LKB,91) COM
        IF(COM.NE.BLNK) READ (COM,98) EA0
        WRITE(LTT,91) '$Enter the upper energy bound [eV]    : '
        READ (LKB,91) COM
        IF(COM.NE.BLNK) READ (COM,98) EB0
   15   WRITE(LTT,91) '$Thinning min.rel.diff.in energy pts. : '
        READ (LKB,91) COM
        IF(COM.NE.BLNK) READ (COM,98,ERR=15) EPS
        WRITE(LTT,99) ' '
        WRITE(LTT,992) ' Selected lower energy bound [eV]     : ',EA0
        WRITE(LTT,992) ' Selected upper energy bound [eV]     : ',EB0
        WRITE(LTT,992) ' Selected thinning tolerance [fract.] : ',EPS
      ELSE IF(MF0.EQ.4) THEN
        KEA=1
        EA0=   0
        EB0= 180
   16   WRITE(LTT,91) '$       Incident particle energy [eV] : '
        READ (LKB,98,ERR=16) EIN
        WRITE(LTT,99) ' '
        WRITE(LTT,992) ' Selected incid. particle energy [eV]: ',EIN
        EOU=0.
        PAR=EOU
        EP6=0.02
      ELSE IF(MF0.EQ.5 .OR. MF0.EQ.26) THEN
        KEA= 2
        EA0=0
        EB0=0
   17   WRITE(LTT,91) '$       Incident particle energy [eV] : '
        READ (LKB,98,ERR=17) EIN
        DEG=-1
        PAR=DEG
        EP6=0.02
        GO TO 19
   18   WRITE(LTT,91) ' Reasonable value <0.1 - please redo    '
   19   WRITE(LTT,91) '$      Resolution broadening fraction : '
        READ (LKB,98,ERR=18) EP6
        IF(EP6.LT.0 .OR. EP6.GT.0.1) GO TO 18
        IF(EP6.EQ.0) EP6=0.02
        WRITE(LTT,99) ' '
        WRITE(LTT,992) ' Selected incident particle energy [eV]:',EIN
        WRITE(LTT,992) ' Resolution-broadening fraction        :',EP6
      ELSE IF(MF0.EQ.6) THEN
        KEA=2
        EA0=0
        EB0=0
   21   WRITE(LTT,91) '$       Incident particle energy [eV] : '
        READ (LKB,98,ERR=21) EIN
   22   WRITE(LTT,91) '$  Scattered particle angle [Degrees] : '
        READ (LKB,98,ERR=22) DEG
        PAR=DEG
        GO TO 24
   23   WRITE(LTT,91) ' Reasonable value <0.1 - please redo    '
   24   WRITE(LTT,91) '$      Resolution broadening fraction : '
        READ (LKB,98,ERR=23) EP6
        IF(EP6.LT.0 .OR. EP6.GT.0.1) GO TO 23
        IF(EP6.EQ.0) EP6=0.02
        WRITE(LTT,99) ' '
        WRITE(LTT,992) ' Selected incident particle energy [eV]:',EIN
        WRITE(LTT,991) ' Selected scattering angle [degrees]   :',DEG
        WRITE(LTT,992) ' Resolution-broadening fraction        :',EP6
      ELSE IF(MF0.EQ.15) THEN
   29   WRITE(LTT,91) '$       Incident particle energy [eV] : '
        READ (LKB,98,ERR=29) EIN
        WRITE(LTT,99) ' '
        WRITE(LTT,992) ' Selected incident particle energy [eV]:',EIN
      ELSE
        WRITE(LTT,91) ' ERROR - Please redo - Illegal MF number',MF0
        GO TO 14
      END IF
C* Select the reaction type number
   30 WRITE(LTT,91) ' Enter the ENDF reaction type  MT No.   '
      WRITE(LTT,91) '$      (MT>40000 for angle-dep. x.s.) : '
      READ (LKB,99) FLNM
      READ (FLNM(1:10),97,ERR=30) IDMY
      WRITE(LTT,91) ' Selected MT                          : ',IDMY
      IF(FLNM(11:50).NE.BLNK) THEN
        COM1=FLNM(11:50)
        ICOM=1
      ELSE
        ICOM=0
      END IF
      IER=0
      EA =EA0
      EB =EB0
C* If zero and not first loop, finish
      IF(IDMY.EQ.0 .AND. MT0.NE.0) GO TO 90
C* Check if ratio to Maxwellian is to be printed
      TMXW=0
      IF((IDMY.EQ.261 .AND. MF0.EQ.3) .OR.
     &   (IDMY.EQ. 18 .AND. MF0.EQ.5)) THEN
        WRITE(LTT,91) '$Enter the Temperature of Maxwellian    '
        WRITE(LTT,91) ' (zero or blank to print absolute)    : '
        READ (LKB,99) FLNM
        IF(FLNM(1:40).NE.BLNK) READ(FLNM,98) TMXW
      END IF
C... Special case for backward compatibility to force MT 5 explicitly
C...  IF(IDMY.EQ.5) IDMY=-5
C...
      IF(IDMY.NE.0) MT0=IDMY
      ELV=0
      FST=0
C* Particle emission spectra
      IF( MT0.EQ.5 .OR. MT0.EQ. 9000 .OR.
     &   (ZAP.LT.0 .AND. (MF0.EQ.5 .OR. MF0.EQ.6)) ) THEN
        WRITE(LTT,91) '$    Outgoing particla ZA designation : '
        READ (LKB,98) ZAP
      END IF
C* Cross section at particular particle emission angle
      IF(MT0.GT.40000) THEN
        WRITE(LTT,91) '$    Outgoing particla ZA designation : '
        READ (LKB,98) ZAP
        WRITE(LTT,91) '$  Scattered particle angle [Degrees] : '
        READ (LKB,98) DEG
        FST=DEG
      END IF
C* Angular distributions
      IF(MF0.EQ.4) THEN
        IF(MT0.EQ.51 .OR. MT0.EQ.601 .OR. MT0.EQ.801) THEN
   32     WRITE(LTT,91) '$          Discrete level energy [eV] : '
          READ (LKB,98,ERR=32) ELV
        END IF
        IF(MT0.GE. 50 .AND. MT0.LE. 91) ZAP=   1
        IF(MT0.GE.600 .AND. MT0.LE.649) ZAP=1001
        IF(MT0.GE.650 .AND. MT0.LE.691) ZAP=1002
        IF(MT0.GE.700 .AND. MT0.LE.749) ZAP=1003
        IF(MT0.GE.750 .AND. MT0.LE.791) ZAP=2003
        IF(MT0.GE.800 .AND. MT0.LE.849) ZAP=2004
      END IF
C... Check the test: yields in MF6 can be given for metastable states.
C...  IF(MF0.EQ.10 .OR. (MT0.EQ.5 .OR. MT0.EQ. 9000)) THEN
      IF(MF0.EQ.10) THEN
   33   WRITE(LTT,91) '$       Discrete final state No.(0-9) : '
        READ (LKB,97,ERR=33) IDMY
        FST=IDMY
      END IF
C*
C* Search for the material, the file and the reaction 
   40 MF =MF0
      MT =MT0
      WRITE(LTT,961) MT,MF,ZA0
C* Appropriate section in the ENDF file found
      IF(MF. EQ. 1 .OR. MF.EQ.3 .OR. MF.EQ.10 .OR.
     &   MF.EQ.23 .OR. MF.EQ.203) THEN
C*
C* Read the cross sections from file MF 1, 3,10,23
        MTE=MT
        KEA=0
        EIN=0
        PAR=FST
        EP6=0
c...
C...    izp=nint(zap)
C...    print *,'zap,mf,mt,kea,ein,par,elv',izp,mf,mt,kea,ein,par,elv
c...
        NUC=0
        ZEL(1)=ZA0
        FRC(1)=1
        REWIND LIN
        CALL DXSELM(LIN,NUC,ZEL,FRC,ZAP,MF,MTE,KEA,EIN,PAR,EP6
     1             ,ES,SG,UG,RWO,NP,MPT,MXR,LTT,ELV)
c...
C...    print *,'                   mte,np',mte,np
C...    do k=1,np
C...      print *,k,es(k),sg(k)
C...    end do
c...
        MAT=NINT(ZA0)
        MFX=MF
        IF(MT.GT.0 .AND. MT.LE.999) THEN
          MTX=MT
        ELSE IF(MT.EQ.9000) THEN
          MTX= 5
          MFX=10
        ELSE IF(MT.EQ.-5) THEN
          MTX=5
        ELSE IF(MT/10000.EQ.4) THEN
          MTX=MT-10000*(MT/10000)
        END IF
        IF(ICOM.EQ.0) THEN
          IF(MAT.LT.0) THEN
            WRITE(COM,925) -MAT,MFX,MTX
          ELSE
            WRITE(COM,926)  MAT,MFX,MTX
          END IF
          IF(MT.EQ.9000 .OR. FST.GT.0)
     &      WRITE(COM(20:40),927) NINT(ZAP),NINT(FST)
          IF(MT/10000.EQ.4)
     &      WRITE(COM(20:40),928) NINT(DEG)
        ELSE
          COM=COM1
        END IF
        IF(NP.LT.1) THEN
          IER=1
          GO TO 80
        END IF
C* Eliminate the near-coincident energy points
        E0=ES(1)
        KP=1
        DO I=2,NP
          E1=ES(I)
          IF(E1-E0 .LT. EPS*E1 .AND. I.LT.NP) CYCLE
          KP=KP+1
          E0=E1
          ES(KP)=E0
          SG(KP)=SG(I)
          UG(KP)=UG(I)
        END DO
        IN2=2
        NP =KP
c...
C...    print *,'                   mte,np',mte,np
C...    do k=1,np
C...      print *,k,es(k),sg(k)
C...    end do
c...
      ELSE IF((MF.GE.4 .AND. MF.LE.6) .OR. MF.EQ.26) THEN
C*
C*      -- Read the outgoing neutron energy distributions from 
C*         file MF3/4/5/6
        MTE=MT
        IF(MF.EQ.4 .OR. MF.EQ.5) PAR=-2
c...
c...    izp=nint(zap)
c...    print *,'zap,mf,mt,kea,ein,par,elv',izp,mf,mt,kea,ein,par,elv
c...
        NUC=0
        ZEL(1)=ZA0
        FRC(1)=1
        REWIND LIN
        CALL DXSELM(LIN,NUC,ZEL,FRC,ZAP,MF,MTE,KEA,EIN,PAR,EP6
     1             ,ES,SG,UG,RWO,NP,MPT,MXR,LTT,ELV)
c...
        print *,'Number of points for MT',MTE,' is',NP
c...
c...
c...    CALL DXSEND(LIN,ZA0,ZAP,MF,MTE,KEA,EIN,PAR,EP6,ES,SG,UG
c... 1             ,RWO,NP,MPT,MXR,LTT,ELV)
c...
c...    print *,'                   mte,np',mte,np
c...    do k=1,np
c...      print *,k,es(k),sg(k)
c...    end do
c...
c...
C*      -- Prepare the comment header record for the PLOTTAB file
        IF     (EIN.GT.999999) THEN
          UN='M'
          EE=EIN/1000000
        ELSE IF(EIN.GT.   999) THEN
          UN='K'
          EE=EIN/1000
        ELSE
          EE=EIN
          UN=' '
        END IF
        IF(KEA.EQ.2) THEN
          IF(ICOM.EQ.0) THEN
            WRITE(COM,924) MT,UN,EE,NINT(DEG)
            IF(DEG.LT.0) COM(15:22)='        '
          ELSE
            COM=COM1
          END IF
        ELSE
          IF     (EOU.GT.999999) THEN
            UO='M'
            EO=EOU/1000000
          ELSE IF(EOU.GT.   999) THEN
            UO='K'
            EO=EOU/1000
          ELSE
            EO=EOU
            UO=' '
          END IF
          IF(ICOM.EQ.0) THEN
            WRITE(COM,922) MT,UN,EE,UO,EO
          ELSE
            COM=COM1
          END IF
        END IF
        IN2=2
        IF(NP.LT.1) IER=1
C*
C* Process secondary neutron spectra
      ELSE IF(MF.EQ.15) THEN
C*      -- Process photon spectra
        CALL FINDMT(LIN,ZA0,ZA,MAT,MF,MT,REC,IER)
        IF     (IER.EQ.1) THEN
          GO TO 90
        ELSE IF(IER.GT.1) THEN
          WRITE(LTT,91) ' ERROR - Processing terminated IER cond.',IER
          STOP 'ENDTAB ERROR - Processing terminated'
        END IF
        READ (REC,96) C1,C2,L1,L2,NC,N2
        IF(NC.NE.1)
     1    STOP ' ENDTAB ERROR - More than one distribution given'
C*      -- Read but ignore weights for distributions
        CALL RDTAB1(LIN,C1,C2,L1,LF,NR,NP,NBT,INR
     1             ,ES,SG,MPT,IER)
        IF(LF.NE.1)
     1    STOP ' ENDTAB ERROR - not a tabulated distribution'
C*      -- Read the number of energies at which distributions are 
C*         tabulated
        CALL RDTAB2(LIN,C1,C2,L1,L2,NR,NE,NBT,INR,IER)
        IF(NR.NE.1)
     1  STOP ' ENDTAB ERROR - More than one interpolation range'
C*      -- Read the distribution to be tabulated
   53   CALL RDTAB1(LIN,C1,EI,L1,L2,NR,NP,NBT,INR
     1             ,ES,SG,MPT,IER)
        IE=IE+1
        IF(EI.LT.EIN .AND. IE.LT.NE) GO TO 53
        IF(ICOM.EQ.0) THEN
          WRITE(COM,925) IABS(MAT),MF,MT,EI
        ELSE
          COM=COM1
        END IF
        IN2=INR(1)
      END IF
C*
C* Skip to the end of section if necessary
      CALL SKIPSC(LIN)
C* Section processed
   80 IF(IER.NE.0) THEN
        WRITE(LTT,91) ' WARNING - No data written for MT     : ',MT
        GO TO 30
      END IF
c...
c...    print *,'                   mte,np',mte,np,EA,EB
c...    do k=1,np
c...      print *,k,es(k),sg(k)
c...    end do
c...
C* Write the data to the PLOTTAB file
      WRITE(LOU,91) COM
      IF(NP.LT.1) GO TO 84
      KK =1
      JK =1
      JD =1
      IF(KEA.EQ.1) THEN
        JK=NP
        JD=-1
      ELSE
        IF(EB.LE.0) EB =ES(NP)
      END IF
      EE2=ES(JK)
      IF(KEA.EQ.1) EE2=ACOS(EE2)*180/PI
      SG2=SG(JK)
      UG2=UG(JK)
C...
C...  PRINT *,KK,EE2,SG2,NP
C...
      IF(TMXW.GT.0) THEN
        EEA=ES(1)
        EEB=ES(NP)
        SC=YTGPNT(NP,ES,SG,EEA,EEB)
        PWR=0.5
        FF=THRMXW(EE2,TMXW,PWR)
        IF(FF .GT.0) THEN
          SG2=SG2/FF/SC
          UG2=UG2/FF/SC
        END IF
      END IF
   81 IF(KK.GT.MPT) THEN
        PRINT *,' ENDTAB ERROR - MPT limit',MPT,KK
        STOP 'ENDTAB ERROR - MPT Limit exceeded'
      END IF
      KK =KK+1
      JK =JK+JD
      EE1=EE2
      SG1=SG2
      UG1=UG2
      EE2=ES(JK)
      IF(KEA.EQ.1) EE2=ACOS(EE2)*180/PI
      SG2=SG(JK)
      UG2=UG(JK)
C...
C...  PRINT *,JK,EE2,SG2
C...
      IF(TMXW.GT.0) THEN
        PWR=0.5
        FF=0
        IF(EE2.GT.0) FF=THRMXW(EE2,TMXW,PWR)
        SG2=SG2/FF/SC
        UG2=UG2/FF/SC
      END IF
      IF(EE2.LE.EA .AND. KK.LT.NP) GO TO 81
C*    -- First point
      IF(EE1.LE.EA .OR. KK.LE.2) THEN
        ED=DBLE(EE1)
        IF(IN2.EQ.1) THEN
          ED=DBLE(EA)
        ELSE IF(IN2.EQ.2) THEN
          IF(EE1.LE.EA) THEN
            IF(EE1.LT.EE2) SG1=SG1+(EA-EE1)*(SG2-SG1)/(EE2-EE1)
            ED=DBLE(EA)
          END IF
        ELSE
          STOP 'ENDTAB ERROR - Invalid interpolation law'
        END IF
        IF(ABS(ED).LE.1.D-9) ED=0
        CALL CH11PK(E11,ED)
        WRITE(LOU,194) E11,SG1,UG1
C...
C...    PRINT *,'WRITTEN FIRST',KK,E11,SG1
C...
      END IF
      IF(EE2.GE.EB .OR. KK.GE.NP) THEN
C*      -- Last point
        IF(IN2.EQ.1) THEN
          ED=DBLE(EB)
        ELSE IF(IN2.EQ.2) THEN
          IF(EE1.LT.EE2 .AND. EE2.GE.EB) THEN
            SG2=SG1+(EB-EE1)*(SG2-SG1)/(EE2-EE1)
            ED=DBLE(EB)
          ELSE
            ED=DBLE(EE2)
          END IF
        END IF
        CALL CH11PK(E11,ED)
        IF(IN2.EQ.1) WRITE(LOU,194) E11,SG1,UG1
        WRITE(LOU,194) E11,SG2,UG2
C...
C...    PRINT *,'WRITTEN LAST',KK,E11,SG2,EE2,EB,NP
C...
      ELSE
C*      -- Intermediate points
        ED=DBLE(EE2)
        CALL CH11PK(E11,ED)
        IF(IN2.EQ.1) WRITE(LOU,194) E11,SG1,UG1
        WRITE(LOU,194) E11,SG2,UG2
C...
C...    PRINT *,'WRITTEN INTERM',KK,E11,SG2,NP
C...
        IF(KK.LT.NP) GO TO 81
      END IF
C*    -- End of data set - write blank delimiter
   84 WRITE(LOU,94)
c*
      IF((ZA0.EQ. 0 .OR. MT0.EQ.0) .AND.
     1  (( MF.LT.4 .AND.  MF.GT.6) .OR. MF.EQ.26)) GO TO 40
C*
C* Try for the next MT number
      GO TO 30
C*
C* Format error trap
   88 WRITE(LTT,91) ' ENDTAB ERROR - Invalid record          '
      WRITE(LTT,93) REC
      STOP 'ENDTAB ERROR - Reading source ENDF file'
C*
C* All processing completed
   90 WRITE(LTT,91) ' More curves to plot (blank=No) ?       '
      READ (LKB,99) FLNM
      IF(FLNM(1:40).NE.BLNK .AND.
     1   FLNM(1:1).NE.'N' .AND. FLNM(1:1).NE.'n') THEN
        REWIND LIN
        GO TO 13
      END IF
C* All processing completed
      STOP 'ENDTAB Completed'
C*
   91 FORMAT(A40,I6)
   92 FORMAT(6F11.0)
   93 FORMAT(A66,I4,I2,I3)
   94 FORMAT(1P,E11.5E1,5E11.4)
   95 FORMAT(   6I11)
   96 FORMAT(1P,2E11.4,4I11)
   97 FORMAT(BN,I10)
   98 FORMAT(BN,F10.0)
   99 FORMAT(A80)
  194 FORMAT(A11,1P,5E11.4)
  922 FORMAT(' MT',I4,1X,A1,'eV',F5.1,1X,A1,'eV',F5.1)
  924 FORMAT(' MT',I4,1X,A1,'eV',F5.1,'  Deg',I3)
  925 FORMAT(' MAT',I4,' MF',I2,' MT',I3,:,20X,' EI',1P,E10.3)
  926 FORMAT(' M'  ,I6,' MF',I2,' MT',I3,:,20X,' EI',1P,E10.3)
  927 FORMAT(' ZA',I6,' LFS',I2)
  928 FORMAT(' Ang',I5)
  961 FORMAT(40X,'Searching for MT',I5,'  MF',I2,'  Mat',F8.1)
  991 FORMAT(A40,F10.1)
  992 FORMAT(A40,1P,E10.3)
      END
      FUNCTION YTGPNT(NP,XX,YY,XA,XB)
C-Title  : Function YTGPNT
C-Purpose: Integrate a tabulated function
C-Description:
C-D  Given a function tabulated at NP points XX(i) with values YY(i),
C-D  integrate the function in the interval [XA:XB] using trapezoidal
C-D  rule (linear interpolation). The argument of the function in XX
C-D  must be monotonic ascending.
C-
      DIMENSION XX(NP),YY(NP)
C
      YTGPNT=0
      X2=XX(1)
      Y2=YY(1)
      DO I=2,NP
        X1=X2
        Y1=Y2
        X2=XX(I)
        Y2=YY(I)
        IF(X2.LE.XA) CYCLE
        IF(X1.LT.XA) THEN
          IF(X2.NE.X1) Y1=Y1+(XA-X1)*(Y2-Y1)/(X2-X1)
          X1=XA
        END IF
        IF(X2.LE.XB) THEN
          YTGPNT=YTGPNT+(X2-X1)*(Y2+Y1)/2
        ELSE
          IF(X2.NE.X1) Y2=Y1+(XB-X1)*(Y2-Y1)/(X2-X1)
          X2=XB
          YTGPNT=YTGPNT+(X2-X1)*(Y2+Y1)/2
          EXIT
        END IF
      END DO
      RETURN
      END
      FUNCTION THRMXW(EE,TMXW,PWR)
C-Title  : Function THRMXW
C-Purpose: Thermal Maxwellian spectrum distribution
      P1 =PWR+1
      THRMXW=(TMXW**(-P1)/GAMMA(P1))*(EE**PWR)*EXP(-EE/TMXW)
      RETURN
      END
      SUBROUTINE CH11PK(CH,R)
C-Title  : CH11PCK subroutine
C-Purpose: Write a real number R into CH string, 11 characters long
C-Author : A.Trkov, Institute Jozef Stefan, Ljubljana, Slovenia
C-Version 2011
C-V  12/05 Increase precision of argument printout for 1E6<E<1e8
      CHARACTER*11 CH
      DOUBLE PRECISION R,F,AR
      IF( R .NE. 0) GO TO 10
      CH ='         0'
      RETURN
   10 AR =ABS(R)
      IF( AR .GE. 1.E-2 .AND. AR .LT. 1.E8) GO TO 20
      F=R
      M=0
   12 IF(ABS(F).LT.10) GO TO 14
      F=F/10
      M=M+1
      GO TO 12
   14 IF(ABS(F).GE.1.) GO TO 16
      F=F*10
      M=M-1
      GO TO 14
   16 WRITE(CH,21) F
      IF(IABS(M).LT.10) WRITE(CH(10:11),22) M
      IF(IABS(M).GE.10) WRITE(CH( 9:11),23) M
      RETURN
   20 IF( AR .GT.  9.99999999D0) GO TO 30
      WRITE(CH,21) R
C...  IF( CH(11:11) .NE. '0' ) RETURN
      RETURN
   30 IF( AR .GT.  99.9999999D0) GO TO 40
      WRITE(CH,31) R
C...  IF( CH(11:11) .NE. '0' ) RETURN
      RETURN
   40 IF( AR .GT.  999.999999D0) GO TO 50
      WRITE(CH,41) R
C...  IF( CH(11:11) .NE. '0' ) RETURN
      RETURN
   50 IF( AR .GT.  9999.99999D0) GO TO 60
      WRITE(CH,51) R
C...  IF( CH(11:11) .NE. '0' ) RETURN
      RETURN
   60 IF( AR .GT.  99999.9999D0) GO TO 70
      WRITE(CH,61) R
C...  IF( CH(11:11) .NE. '0' ) RETURN
      RETURN
   70 IF( AR .GT.  999999.999D0) GO TO 80
      WRITE(CH,71) R
C...  IF( CH(11:11) .NE. '0' ) RETURN
      RETURN
   80 IF( AR .GT.  9999999.99D0) GO TO 90
      WRITE(CH,81) R
C...  IF( CH(11:11) .NE. '0' ) RETURN
      RETURN
   90 IF( AR .GT.  99999999.9D0) GO TO 100
      WRITE(CH,91) R
C...  IF( CH(11:11) .NE. '0' ) RETURN
      RETURN
  100 IR=R+0.5
      WRITE(CH,101) IR
      RETURN
   21 FORMAT(F11.8)
   22 FORMAT(SP,I2)
   23 FORMAT(SP,I3)
   31 FORMAT(F11.7)
   41 FORMAT(F11.6)
   51 FORMAT(F11.5)
   61 FORMAT(F11.4)
   71 FORMAT(F11.3)
   81 FORMAT(F11.2)
   91 FORMAT(F11.1)
  101 FORMAT(I11)
      END
