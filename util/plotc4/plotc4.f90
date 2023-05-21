   program PLOTC4
   implicit none
!*--PLOTC44
!
!
! PARAMETER definitions
!
   integer,parameter :: MXPNT = 90000,MXPGP = 10000,MXRW = 200000,MXIS = 10
!
! COMMON variables
!
   real :: ADVanc,AWR,EINc,EP6,SIZplt,THIck
   real,dimension(MXPGP) :: DXEx,DYEx,E2,XEX,YEX
   real,dimension(48) :: E2T,EXHigh,EXLow
   real,dimension(100) :: EHGet,ELGet,EPGet
   real,dimension(4) :: FIEld4
   integer :: IBAse,IBMpc,ICHr,ICNtrl,IDOub,IEX,IGEt,IGRoup,ILIb,INP,IPLotz,IREf,ISCr,ISYm,ITApe1,ITApe2,ITHick,ITOp,IZA,IZAbcd,  &
            & IZArat,KGRoup,MAT,MAXie,MAXref,MF,MFIn,MINnie,MODiza,MPT,MREf,MT,MTRat,MYGrid,MYMode,N2,NGEt,NTApe1,NTApe2,NTApe3,  &
            & NUMplt,OUTp
   integer,dimension(5000) :: ICHpen
   integer,dimension(2,256) :: INDchr
   integer,dimension(100) :: INTrn,IZAhi,IZAlow,MFHi,MFLow,MTHi,MTLow
   integer,dimension(2) :: IWAy
   character(1) :: LABcm,MSTar1,MSTar2,STAtus
   character(4),dimension(4) :: LIBnam,ZABcd
   integer,dimension(48) :: LREf
   character(4) :: MSTat1,MSTat2
   integer,dimension(MXPGP) :: NREf
   integer,dimension(4) :: NVErse
   character(4),dimension(9) :: REF1
   character(4),dimension(9,48) :: REFs
   character(4),dimension(5,4) :: VERses
   real,dimension(5000) :: XCHar,YCHar
   real,dimension(2) :: XLIm,XREal,YLIm,YREal
   real,dimension(MXPNT) :: XPAge,YPAge
   common /DOUBL / IDOub,FIEld4
   common /EPSMF6/ EP6
   common /EXFOR / XEX,DXEx,YEX,DYEx,NREf,E2,IEX
   common /GETEM / IZAlow,MFLow,MTLow,ELGet,IZAhi,MFHi,MTHi,EHGet,EPGet,INTrn,NGEt,IGEt
   common /GRIDDY/ MYGrid
   common /INPARM/ MINnie,MAXie
   common /LIBC  / LIBnam
   common /LIBI  / ILIb
   common /MODEMY/ MYMode
   common /PAGEXY/ XPAge,YPAge,N2,IBAse,ITOp,ISCr
   common /PLOTN / NUMplt,ADVanc
   common /PLTSIZ/ SIZplt,IPLotz,IBMpc
   common /RATZA / IZArat,MTRat,MFIn
   common /RATZAC/ MSTar1,MSTar2
   common /REFERC/ REFs,REF1
   common /REFERI/ LREf,EXLow,EXHigh,E2T,IREf,MREf,MAXref,IGRoup,KGRoup
   common /SYMTB2/ XCHar,YCHar,ICHpen,INDchr,ICHr,ICNtrl,ISYm
   common /SYSSTA/ LABcm,STAtus
   common /THICKY/ ITHick,THIck
   common /UNITS / INP,OUTp,ITApe1,ITApe2
   common /UNITT / NTApe1,NTApe2,NTApe3
   common /VERSC / VERses
   common /VERSI / NVErse
   common /WAYS  / IWAy
   common /WHERE2/ IZAbcd
   common /WHEREC/ ZABcd,MSTat1,MSTat2
   common /WHEREI/ IZA,MAT,MF,MT,MODiza,EINc,AWR
   common /XLIMIT/ MPT
   common /XYLIM / XLIm,YLIm
   common /XYREAL/ XREal,YREal
!
! Local variables
!
   character(4) :: blank
   character(1) :: blank1
   character(40) :: c4dat,chtab,edat,mfdef,mtdef,p4inp,p4lst,zadef
   real :: deg,elv,par,zaa,zap
   real,dimension(MXIS) :: frc,zel
   integer :: i,idx,ievend,iexend,izap,j,kea,ltt,mtx,nuc
   integer :: IFIX,NINT
   character(4),dimension(9) :: refx
   real,dimension(MXRW) :: rwo
   character(4),dimension(5,4) :: versez
   real,dimension(MXPNT) :: zpage
!
!
!-Title  : Program PLOTC4
!-Version: 86-1 (August 1986)
!-V        87-1 (June 1987)   *Software upper and lower case
!-V                            characters.
!-V      2001-3 (March 2001)  *Upgrade to compare measured
!-V                            differential data with curves from
!-V                            evaluated data files.
!-V                           *Modifications for compatibility with
!-V                            character plotting package of the
!-V                            Pre-Pro codes.
!-V      2002-5 (Jul 2002)    *Enable plotting of emitted charged
!-V                            particle spectra and gamma.
!-V                           *Enhance plotting of discrete level
!-V                            inelastic angular distributions.
!-V                           *Differentiate between isomeric states.
!-V                           *Fix bugs for compilers with dynamic
!-V                            memory allocation
!-V      2004-1               *Fix bug plotting EXFOR only.
!-V      2006-2               *Update for consistency with DXSEND
!-V                            upgrade to provide uncertainties,
!-V                            if available.
!-Purpose: Plot experimental and evaluated nuclear data
!-Author:
!-A  OWNED, MAINTAINED AND DISTRIBUTED BY:
!-A  -------------------------------------
!-A  The Nuclear Data Section
!-A  International Atomic Energy Agency
!-A  P.O. Box 100
!-A  A-1400, VIENNA, AUSTRIA
!-A  EUROPE
!-A
!-A  ORIGINALLY WRITTEN BY:
!-A  ----------------------
!-A  Dermott E. Cullen
!-A  University of California
!-A  Lawrence Livermore National Laboratory
!-A  L-86
!-A  P.O. Box 808
!-A  Livermore, CA 94550
!-A  U.S.A.
!-A  Telephone  925-423-7359
!-A  e-mail     CULLEN1@LLNL.GOV
!-A  Website    HTTP://REDDOG1.LLNL.GOV
!-A
!-A  VERSION 2001-3 IMPLEMENTED BY:
!-A  ------------------------------
!-A  Andrej Trkov
!-A  The Nuclear Data Section
!-A  International Atomic Energy Agency
!-A  P.O. Box 100
!-A  A-1400, VIENNA, AUSTRIA
!-A  EUROPE
!-
!-M
!-M  USERS' GUIDE FOR PROGRAM PLOTC4
!-M  ===============================
!-M
!-M  PURPOSE
!-M  -------
!-M  The function of the PLOTC4 program is to plot and compare
!-M  experimental data, which are in a computation format with
!-M  evaluated data in ENDF format.
!-M
!-M  This program has been designed to use the computation format
!-M  and conventions which are used by program X4TOC4 to translate
!-M  experimentally measured data from the EXFOR to a computation
!-M  format.
!-M
!-M  The evaluated data in ENDF format need some pre-processing to
!-M  reduce the complexity of format representations, which are
!-M  allowed in the general ENDF format. The codes of the Pre-Pro
!-M  sequence are used for this purpose. It is recommended that
!-M  the following are executed:
!-M    LINEAR   to convert all cross sections to linearly interpolable
!-M             form.
!-M    RECENT   to reconstruct the resonances from resonance parameters.
!-M    SIGMA1   to Doppler broaden the cross sections to room
!-M             temperature (optional but recommended when comparing
!-M             to experimental data since measurements are usually
!-M             done at room temperature).
!-M    ACTIVATE to generate cross sections for excitation into discrete
!-M             excited state (file MF10) from the data in files MF3
!-M             and MF9.
!-M    LEGEND   to convert angular distributions in arbitrary ENDF
!-M             representation into tabular ENDF representation.
!-M    SIXTAB   to convert double differential data in arbitrary ENDF
!-M             representation into tabular (law-7) ENDF representation.
!-M
!-M  GRAPHICS INTERFACE
!-M  ------------------
!-M  This program uses a simple CALCOMP like graphics interface which
!-M  requires only 3 subroutines...PLOTS, PLOT and PEN (described in
!-M  detail below). All characters and symbols are drawn using tables
!-M  of pen strokes (supplied with this program). Using this method
!-M  the program should be simple to interface to virtually any plotter
!-M  or graphics terminal and the appearance and layout of the plots
!-M  should be independent of which plotter is used.
!-M
!-M  WHAT CAN BE PLOTTED
!-M  -------------------
!-M  At the present time the following types of data can be plotted:
!-M
!-M  (1) MF =   3 - Cross sections. ENDF and EXFOR data can be
!-M                 compared (only MF = 3 ENDF data can be compared).
!-M  (2) MF =   4 - Angular distributions. ENDF and EXFOR data can be
!-M                 compared.
!-M  (3) MF =   5 - Energy distributions. ENDF and EXFOR data can be
!-M                 compared.
!-M  (4) MF =   6 - Double differential cross sections. ENDF and EXFOR
!-M                 data can be compared.
!-M  (5) MF = 154 - Legendre coefficients (only EXFOR).
!-M  (6) MF = 203 - Cross section ratios (only EXFOR).
!-M  (7) MF = 402 - Resonance parameters (only EXFOR).
!-M  (8) MF = 801 - Fission yield data (only EXFOR).
!-M
!-M  All other data will be skipped.
!-M
!-M  WHAT DATA WILL BE PLOTTED
!-M  -------------------------
!-M  Based on input options the user may specify whether the program
!-M  should plot only EXFOR data or EXFOR and ENDF data. In addition
!-M  the user may specify up to 100 ZA/MF/MT ranges to select data.
!-M
!-M  HOW MUCH DATA CAN BE PLOTTED
!-M  ----------------------------
!-M  ENDF DATA
!-M  Each section of ENDF cross sections (MF =3, any MT) may contain
!-M  any number of data points. If a section contains MXPNT or fewer
!-M  points all of the data will be in core. If the section contains
!-M  more than MXPNT points the data will be written to a scratch file
!-M  and read as needed.
!-M  Parameter MXPNT is currently set at 90000.
!-M
!-M  EXFOR DATA
!-M  Based on input parameters the user may control how much EXFOR data
!-M  will appear on each plot. This program allows the user to specify
!-M  that up to MXPGP data points may appear on each plot. If there are
!-M  more physically comparable points (e.g., same ZA, MF, MT) than
!-M  specified by the user the program will create a series of plots
!-M  each containing not more than the maximum number of points per
!-M  plot specified by the user.
!-M  Parameter MXPGP is currently set at 10000.
!-M
!-M  WHAT COMPUTERS WILL THE PROGRAM RUN ON
!-M  --------------------------------------
!-M  The program has been implemented on a variety of computers from
!-M  Cray and IBM mainframe to Sun workstations to a PC. The
!-M  program is small enough to run on virtually any computer.
!-M
!-M  The program uses a simple Calcomp-like graphics interface
!-M  (described below) and allows the user specify the physical size
!-M  of the plotter being used, by input parameters. Using these
!-M  conventions this program can be easily interfaced to virtually
!-M  any plotter.
!-M
!-M  For special considerations see the sections below on,
!-M  (1) Computer dependent coding.
!-M  (2) Plotter/graphics terminal interface.
!-M
!-M  COMPUTATION FORMAT
!-M  ------------------
!-M  The computation format uses a classification system and units
!-M  which are compatible with ENDF. Data is classified by (1) ZA
!-M  of projectile, (2) ZA of target, (3) metastable state of target,
!-M  (4) MF - type of data, (5) MT - reaction, (6) metastable state
!-M  of residual nucleus. To identify the source of the data the first
!-M  author and year and the EXFOR accession and sub-accession number
!-M  are included in the format. In addition, fields are assigned to
!-M  define the status of the EXFOR data (e.g., S = superceded),
!-M  whether data is in the laboratory or center-of-mass frame of
!-M  reference and the physical significance of the last 2 output
!-M  fields (LVL = level energy, HL = half-life). Finally the format
!-M  includes 8 fields in which the output data are contained (e.g.,
!-M  incident energy, data, cosine, uncertainties, etc.)
!-M
!-M  Columns   Description
!-M  -------   -----------
!-M    1-  5   Projectile ZA (e.g. neutron =1, proton =1001)
!-M    6- 11   Target ZA (e.g. 26-Fe-56 =  26056)
!-M       12   Target metastable state (e.g. 26-FE-56m = M)
!-M   13- 15   MF (ENDF conventions, plus additions).
!-M   16- 19   MT (ENDF conventions, plus additions).
!-M       20   Product metastable state (e.g. 26-FE-56M = M)
!-M       21   EXFOR status
!-M       22   Center-of-mass flag (C=center-of-mass, blank=lab)
!-M   23- 94   8 data fields (each in E9.3 format). defined below.
!-M   95- 97   Identification of data fields 7 and 8
!-M            (e.g., LVL=level, HL=half-life, etc.)
!-M   98-122   Reference (first author and year)
!-M  123-127   EXFOR accession number
!-M  128-130   EXFOR sub-accession number
!-M      131   Multi-dimension table flag
!-M
!-M  DEFINITION OF 8 COMPUTATION FORMAT DATA FIELDS
!-M  In order to plot data this program assumes that the following
!-M  conventions have been used for data in the computation format.
!-M
!-M  Data Field   Definition
!-M  ----------   ----------
!-M    1          Projectile incident energy
!-M    2          Projectile incident energy uncertainty
!-M    3          Data, e.g., cross section, angular distribution, etc.
!-M    4          Data uncertainty
!-M    5          Cosine or legendre order
!-M    6          Cosine uncertainty
!-M    7          Identified by columns 95-97 (e.g.,level E, half-life)
!-M    8          Identified by columns 95-97 (e.g.,level E, uncertainty)
!-M
!-M  The physical significance of the first 6 data fields is defined by
!-M  the MF (data type). The physical significance of fields 7 and 8
!-M  are defined by columns 95 through 97 (e.g. LVL = level energy and
!-M  its uncertainty).
!-M
!-M  SPECIAL CONVENTIONS
!-M  The above conventions are appropriate for most types of data
!-M  in the ENDF system. In order to allow this program to plot
!-M  additional types of data the following special conventions have
!-M  been adopted:
!-M
!-M  Cross section ratios  - Field 5 = MT of denominator.
!-M  (MF = 203)              Field 6 = ZA of denominator.
!-M  Fission yield data    - Field 5 = ZA of fission fragment.
!-M  (MF = 801)              Field 6 = mass of fission fragment.
!-M  Production            - Field 6 = ZA of product.
!-M  (MT = 9000-9999)
!-M
!-M  See, remarks below on metastable state flags.
!-M
!-M  BLANK VS. ZERO DATA FIELDS
!-M  The 8 data fields on each computation format line are read as
!-M  characters and internally converted to floating point numbers
!-M  (see, subroutine FLOAT9). By testing before converting this
!-M  program can determine whether any given field is blank (no data
!-M  given) as opposed to zero. It is often important to make this
!-M  distinction, particularly for fields 7 and 8, e.g. the difference
!-M  between 0.0 indicating ground state as opposed to no data given.
!-M  The EXFOR to computation format conversion program X4TOC4 also
!-M  makes this distinction and leaves undefined fields blank (not
!-M  zero). Therefore, any data converted to the computation format
!-M  format using program X4TOC4 will follow the correct conventions.
!-M
!-M  However, if the user of this program directly codes data in the
!-M  computation format it is important to maintain this convention.
!-M  Remember---any undefined fields should be left blank and not set
!-M  to zero.
!-M
!-M  COMPUTATION FORMAT UNITS
!-M  In order to plot data this program assumes that the following
!-M  units have been used for data in the computation format.
!-M
!-M  eV         = energy
!-M  barns      = cross section
!-M  steradians = solid angle
!-M  seconds    = time
!-M  kelvin     = temperature
!-M
!-M  For example double differential data (MF=6) will be in,
!-M
!-M  barns/eV/steradian
!-M
!-M  METASTABLE STATE
!-M  The computation format allows the metastable state of the target
!-M  and residual nucleus to be identified. For ratio data metastable
!-M  state of both numerator and denominator of the ratio should be
!-M  defined.
!-M
!-M  The metastable state of the target is identified in column 12 and
!-M  the metastable state of the residual nucleus in column 20. For
!-M  ratio data the metastable state of the denominator target and
!-M  residual nucleus are identified by having the denominator ZA and
!-M  MT in the form ZA.m AND MT.m (e.g., 26056.9 and 102.1). Columns
!-M  12 and 20 may contain characters such as M, but to maintain the
!-M  eight output fields in strictly numerical form the denominator
!-M  ZA.m and MT.m will be in numerical form. The possible characters
!-M  that may appear in columns 12 or 20 and their numerical
!-M  equivalents used with ratio denominator za and mt include:
!-M
!-M  Definition    Column 12 or 20     Equivalent   Plotted as
!-M  ----------    ---------------     ----------   ----------
!-M  ground              G                0           -G
!-M  m1                  1                1           -M1
!-M  m2                  2                2           -M2
!-M  m3                  3                3           -M3
!-M  m4                  4                4           -M4
!-M  m5                  5                5           -M5
!-M  unknown             ?                6           -M?
!-M  m                   M                7           -M
!-M  more than 1         +                8           -M+
!-M  all or total        T                9           blank
!-M  all or total      blank              9           blank
!-M
!-M  By convention if an EXFOR reaction does not specify a metastable
!-M  state the state is defined in the computation format to be..ALL..
!-M  (i.e., blank in column 12 or 20, 9 in ratio ZA or MT).
!-M
!-M  For example, for a ratio if the ZA.m and MT.m are output as
!-M  26056.9 and 102.1, respectively the ratio denominator target is
!-M  26-Fe-56 (all) and the reaction is capture (MT=102) leaving the
!-M  residual nucleus in the m1 state.
!-M
!-M  EXFOR STATUS
!-M  Column 21 of each computation format record may contain blank
!-M  (status not specified) or one to the following characters:
!-M
!-M  Column 21   Definition
!-M  ---------   ----------
!-M     U        Unnormalized (has priority over EXFOR status and is
!-M              used to indicate that the data is not in standard
!-M              output units. y axis label will say..UNNORMALIZED..).
!-M     A        Approved by author
!-M     C        Correlated
!-M     D        Dependent
!-M     O        Outdated
!-M     P        Preliminary
!-M     R        Renormalized
!-M     S        Superceded
!-M
!-M  If data has any other EXFOR status (e.g., translated from SCISRS)
!-M  the status field will be blank.
!-M
!-M  CONTROL OF PLOTTING
!-M  -------------------
!-M  The user has control over how data in the computation format
!-M  is interpreted by this program.
!-M
!-M  Data on each plot is identified by plotting a character equivalent
!-M  of target za and metastable state (ZA), data type (MF), reaction
!-M  (MT) and residual metastable state. The ZA, MF and MT may be
!-M  interpreted in any manner that the user chooses.
!-M
!-M  This is accomplished by using three dictionaries which control
!-M  the plotting. All three of these dictionaries are distributed
!-M  with this program. Each dictionary is a simple card image file
!-M  which may be modified by the user at any time to meet specific
!-M  needs. The three dictionaries are:
!-M
!-M  (1) INTERPRETATION OF SPECIAL ZA
!-M      For all target or residual nuclei this program will use the
!-M      ENDF convention of assuming ZA = 1000*Z + A. For special
!-M      materials which do not easily fit into this scheme (e.g.,
!-M      water) the ENDF convention is to define Z =0 and to assign
!-M      a numerical equivalent for each special material. For normal
!-M      materials this program will use ZA to define the material or
!-M      isotope. For special material (Z=0) this program will use this
!-M      dictionary to define the material. As distributed this
!-M      dictionary contains all of the special materials defined in
!-M      the ENDF system. The user may code data for any special
!-M      material in the computation format and assign it a special
!-M      ZA. By adding the definition to this dictionary the user may
!-M      obtain plots on which the special material is properly
!-M      identified.
!-M
!-M  (2) INTERPRETATION OF MF
!-M      This dictionary defines the titles that will appear for each
!-M      MF read from the computation format. In addition this
!-M      dictionary allows the user to specify different titles for
!-M      the same MF and different MT ranges, e.g.:
!-M
!-M      MF =3, MT =  251 - 253 = parameters (used for MU, XI, GAMMA)
!-M      MF =3, MT = 9000       = neutron induced (used for production)
!-M      MF =3, MT = other      = cross section
!-M
!-M      If the user does not like the titles normally output by this
!-M      program it is merely necessary to modify this dictionary.
!-M
!-M  (3) INTERPRETATION OF MT
!-M      This dictionary defines the titles that will appear for each
!-M      MT read from the computation format, e.g.:
!-M
!-M      MT  =    1 = total
!-M          = 9000 = production
!-M
!-M      If the user does not like the titles normally output by this
!-M      program it is merely necessary to modify this dictionary.
!-M
!-M  Used in combination with the translation of the ZA, MF and MT serve
!-M  to identify the data being plotted. By using the dictionaries
!-M  described above the user has complete control over how ZA, MF and
!-M  MT are interpreted and as such may select any form to identify
!-M  data.
!-M
!-M  PROGRAM OPERATION
!-M  -----------------
!-M  EXFOR DATA INDEX TABLE
!-M  The entire computation format file will first be read and compared
!-M  to the requested ZA/MF/MT ranges specified by the user. If no
!-M  comparable data is found the program will terminate execution. If
!-M  comparable data is found the program will create an index table
!-M  specifying (1) ZA, (2) MF, (3) MT, (4) starting record number,
!-M  (5) ending record number, (6) number of data points with this ZA,
!-M  MF and MT. During execution this index table will be used to,
!-M  (1) select the next ZA, MF, MT to be plotted if only plotting
!-M  EXFOR data, or (2) to determine whether or not there is comparable
!-M  EXFOR data (without again searching the EXFOR data file) when
!-M  comparing EXFOR and ENDF data. Once it has been decided to plot
!-M  EXFOR data which has a given ZA, MF and MT the starting record
!-M  index is used to quickly position to the first record to read and
!-M  the ending record index is used to define when to stop reading
!-M  (instead of reading the entire computation format data file).
!-M
!-M  ONLY PLOTTING EXFOR DATA
!-M  The program will use the index table to define the ZA, MF and MT
!-M  of the next set of data to plot. Based on user input the program
!-M  will then plot either one reference (reference = author, year,
!-M  EXFOR accession and sub-accession number) per plot or all
!-M  comparable references on the same plot. The cycle of reading data
!-M  and producing plots will be continued until all data defined in
!-M  the index table have been plotted.
!-M
!-M  COMPARING ENDF/B AND EXFOR DATA
!-M  In the comparison mode the program will only plot data if there
!-M  is comparable data (same ZA, MF, MT) on both the ENDF formatted
!-M  and computation formatted files.
!-M
!-M  Based on the plotting requests (see below) the program will first
!-M  search the ENDF data file to find an acceptable section of cross
!-M  sections (MF=3,4,5,6). The program will then use the EXFOR index
!-M  to determine if there is comparable EXFOR data (same ZA, MF, MT).
!-M  If there is no comparable data the program will ignore the current
!-M  section of ENDF data and search for the next requested section
!-M  of ENDF data. The cycle of reading ENDF data and comparing to
!-M  the EXFOR index table will be continued until comparable ENDF
!-M  and EXFOR data are found. Only after the EXFOR index table shows
!-M  that the computation format file contains comparable data will
!-M  the file be read. As described above while reading EXFOR data the
!-M  program will use the starting and ending record number to quickly
!-M  position to the data to read and to stop reading when all required
!-M  data has been read.
!-M
!-M  Experimentally measured differential cross sections are seldom
!-M  measured for a particular reaction as defined in ENDF. Usually
!-M  they are "particle production cross sections", therefore
!-M  summation over several ENDF reactions is necessary. Differential
!-M  data in an ENDF file are marked available for comparison if file
!-M  MF4, 5 or 6 are found, but no action is taken until the
!-M  differential data are also found in the EXFOR file. At this
!-M  point the emitted particle, the incident particle energy, the
!-M  outgoing particle anergy and/or angle are known. At this point
!-M  the differential data retrieval routine is activated, extracting
!-M  the normalised distributions, multiplying by the appropriate
!-M  cross sections from file MF3 and summing contributions from all
!-M  reactions that produce that outgoing particle.
!-M
!-M  To simplify coding a restriction on the formats of the
!-M  differential data exists. The retrieval routine only accepts
!-M  tabular representation. ENDF files in other format representations
!-M  can be converted using codes LEGEND for the angular distributions
!-M  and SIXTAB for the double differential data.
!-M
!-M  At present the assembly of the differential data is limited to
!-M  the neutron production reactions.
!-M
!-M  ONE REFERENCE PER PLOT
!-M  When plotting one reference per plot the program will use the
!-M  EXFOR index table to determine where to start reading. After one
!-M  data point has been read the program will continue to read data
!-M  points until (1) a point is found with a different ZA, MF, MT or
!-M  reference, (2) the index table last record number indicates that
!-M  the last point has been read, or (3) the maximum number of points
!-M  per plot have been read.
!-M
!-M  WARNING...When plotting one reference per plot in order to produce
!-M  a plot the program must find at least the minimum number of points
!-M  required (see, input description below) on suceessive records.
!-M  therefore the computation format should be sorted to insure that
!-M  all data with the same ZA, MF, MT, reference appear on successive
!-M  records.
!-M
!-M  ALL COMPARABLE EXFOR DATA ON SAME PLOT
!-M  When plotting all comparable data on the same plot the program
!-M  will use the EXFOR index table to define where to start reading.
!-M  The program will continue to read data until (1) the index table
!-M  last record number indicates that the last point has been read, or
!-M  (2) the maximum number of points per plot have been read.
!-M
!-M  In this mode the EXFOR data need not be sorted by ZA, MF, MT,
!-M  reference since the EXFOR index table will define where all
!-M  comparable data are located on the computation format data file.
!-M  However, to minimize the time required to search the computation
!-M  format file the user should sort the data by ZA, MF, MT.
!-M
!-M  OPTIMIZING PROGRAM OPERATION
!-M  ----------------------------
!-M  Program operation can be optimized by minimizing the size of the
!-M  ENDF and computation formatted files. If you wish to compare
!-M  a limited number of reactions it is suggested that you first
!-M  prepare ENDF and computation formatted data files that only
!-M  contain the data which will be plotted, i.e., never use this
!-M  program to try to compare two enormous files of ENDF and EXFOR
!-M  data unless you are willing to spend a correspendingly enormous
!-M  amount of computer time. In addition the EXFOR data file
!-M  should be sorted by ZA, MF, MT, reference.
!-M
!-M  SCALING OF PLOTS
!-M  ----------------
!-M  ENDF/B AND/OR EXFOR
!-M  If only plotting EXFOR data this program will scale the X and Y
!-M  range of each plot to include only EXFOR data. If plotting EXFOR
!-M  and ENDF data the user may specify by input (input described
!-M  below) to scale plots to include all ENDF and EXFOR data or
!-M  only all ENDF data or only all EXFOR data. Although this option
!-M  may be used for special purposes to obtain special scaling it is
!-M  recommended that the user always scale plots to include all ENDF
!-M  and EXFOR data.
!-M
!-M  ENERGY RANGE
!-M  Regardless of the energy range specified by plotting requests
!-M  (see description of requests below) this program will never extend
!-M  the energy range of plots beyond where there are data. For example
!-M  to plot (n,2n) over the entire energy range where there are data
!-M  the user can specify 0.0 to 100 MeV. This program will produce
!-M  plots from threshold up to the highest energy below 100 MeV where
!-M  data are given.
!-M
!-M  COSINE RANGE
!-M  For angular (MF=4) and double differential (MF=6) distributions
!-M  where the X variable is cosine, plots will always be produced over
!-M  the cosine range -1.0 to 1.0.
!-M
!-M  INPUT LOGICAL FILE UNITS
!-M  ------------------------
!-M  Note that input instructions for PLOTC4 are on unit 4, rather than
!-M  the usual Fortran unit 5. This is so for historic reasons to allow
!-M  the program to be used on an IBM-PC where unit 5 was reserved for
!-M  keyboard interaction.
!-M
!-M  Unit   Description
!-M  ----   -----------
!-M    4    Input options              (BCD - 80 columns/record)
!-M   10    Computation formatted data (BCD - 131 columns/record)
!-M   11    ENDF formatted data        (BCD - 80 columns/record)
!-M   12    Special ZA definitions     (BCD - 80 columns/record)
!-M   14    MF definitions             (BCD - 80 columns/record)
!-M   15    MT definitions             (BCD - 80 columns/record)
!-M   17    Software characters        (BCD - 80 columns/record)
!-M
!-M  OUTPUT UNITS
!-M  ------------
!-M  Unit   Description
!-M  ----   -----------
!-M    6    Output report              (BCD - 120 columns/record)
!-M
!-M  SCRATCH UNITS
!-M  -------------
!-M  UNIT   DESCRIPTION
!-M  ----   -----------
!-M   18    ENDF data paging unit      (Binary - 6000 words/record)
!-M
!-M  INPUT CARDS
!-M  -----------
!-M  The user must input at least teo cards to specify plotting options.
!-M  In the simplest case this first card can be completely blank (see
!-M  Example input No. 1 below). To select data by ZA/MF/MT/Incident
!-M  energy range the user may input up to 100 additional cards.
!-M
!-M  Card  Columns  Format  Description
!-M  ----  -------  ------  -----------
!-M    1     1-10     F10   Paper offset along paper height [inch].
!-M         11-20     F10   Total paper height (default 12.5) [inch]
!-M         21-30     F10   Paper offset along paper width.
!-M         31-40     F10   Total paper width (default 10) [inch]
!-M    2     1- 5     I5    Compare EXFOR data to ENDF
!-M                         0 = No
!-M                         1 = Yes
!-M                         2 = Yes (identify ENDF points by plotting
!-M                                  a small diamond round each point).
!-M                         NOTE: If comparing data plots will only
!-M                         be produced if comparable data is found
!-M                         on both the ENDF and computation format
!-M                         files.
!-M          6-10     I5    All comparable exfor data on same plot
!-M                          0 = No
!-M                         <0 = Yes, each reference on a seperate plot
!-M                         >O = Yes, the value entered is the maximum
!-M                              number of references per plot (current
!-M                              upper limit=48).
!-M         11-15     I5    Plot scaling
!-M                         0 = ENDF and EXFOR (recommended)
!-M                         1 = ENDF
!-M                         2 = EXFOR
!-M                         (automatically set to 2 if not comparing)
!-M         16-20     I5    Plot X error bars (energy, cosine, etc.)
!-M                         0 = No, 1 = Yes
!-M         21-25     I5    Plot Y error bars (cross section, etc.)
!-M                         0 = No, 1 = Yes
!-M         26-30     I5    Identify all references by symbol
!-M                         0 = No, 1 = Yes
!-M                         (0 = If only one reference on plot do not
!-M                         plot box and reference symbol around each
!-M                         data point...recommended).
!-M         31-35     I5    Allow variable E2 on same plot
!-M                         0 = No, 1 = Yes
!-M                         (Normally only data with same ZA/MF/MT/E2
!-M                         will appear on same plot. 1 = collect data
!-M                         from 1 reference for same ZA/MF/MT and
!-M                         a number of values of E2. Identify data on
!-M                         plot by each value of E2).
!-M         36-40     I5    Minimum EXFOR points per plot
!-M                         (If there are fewer comparable EXFOR points
!-M                         they will be skipped...default 8, minimum
!-M                         valid entry 2).
!-M         41-45     I5    Maximum number of EXFOR points per plot
!-M                         (Minimum defined by columns 36-40 up to
!-M                         MXPGP...DEFAULT MXPGP1000).
!-M                         Currently parameter MXPGP is set to 10000.
!-M                         HINT: The limit applies to the total number
!-M                               of points extracted from the EXFOR
!-M                               file. If plots are defined by
!-M                               explicitly requested ranges (see the
!-M                               next input line) and all comparable
!-M                               points are to be displayed on the
!-M                               same plot, use the default value.
!-M         46-50     I5    Grid type
!-M                         = 0 - Tick marks on each axis..recommended.
!-M                         = 1 - Full grid.
!-M         51-55     I5    Plot size
!-M                         = 0 - Full size plots.
!-M                         = 1 - Half size (4 sub-plots per plot).
!-M         56-70   3A4,A3  ENDF library identification.
!-M                         e.g., ENDF/B-V (only used if comparing).
!-M  3-N     1- 7     I7    Lower ZA limit
!-M          8-11     I4    Lower MF limit
!-M         12-15     I4    Lower MT limit
!-M         16-26   E11.4   Lower incident energy limit (eV)
!-M         27-33     I7    Upper ZA limit
!-M         34-37     I4    Upper MF limit
!-M         38-41     I4    Upper MT limit
!-M         42-52   E11.4   Upper incident energy limit (eV)
!-M         53-55     I3    Plot scales (ENDF convention):
!-M                          2  Linear abscisa and ordinate
!-M                          3  Logarithmic abscisa and linear ordinate
!-M                          4  Linear abscisa and logarithmic ordinate
!-M                          5  Logarithmic abscisa and ordinate
!-M         56-66   E11.4   Smearing parameter for elastic and
!-M                         discrete level inelastic scattering energy
!-M                         distributions given in terms of a
!-M                         fractional energy increment at which a
!-M                         gaussian curve is half maximum. This is
!-M                         only used to simulate resolution broadening
!-M                         of the distributions, which are ideally
!-M                         delta-functions (e.g. elastic and discrete
!-M                         inelastic scattering reactions when
!-M                         assembling total neutron emission
!-M                         energy distribution).
!-M
!-M * The request list is terminated by a blank line. The remainder
!-M   of the input file will be ignored.
!-M
!-M * There may be up to 100 ZA/MF/MT/Energy range requests. If there
!-M   are more than 100 requests only the first 100 will be used.
!-M * Each request independently specifies a range of ZA/MF/MT/Energy
!-M   to be plotted.
!-M * For each set of data, ZA must be between the lower and upper ZA
!-M   limit, MF must be between the lower and upper MF limit, MT must
!-M   be between the lower and upper mt limit and the incident energy
!-M   must be between the lower and upper energy limit.
!-M * e.g., Z=1 to 90000, MF=3 to 3, MT=1 TO 1, E=0.0 to 2.0E+7 eV
!-M   will select all ZA between 1 to 90000 which have MF=3 and MT=1
!-M   and data points with incident energy between 0 and 20 MeV.
!-M * If there are no request cards all data will be plotted.
!-M
!-M  EXAMPLE INPUT NO. 1
!-M  ...................
!-M  To plot all EXFOR data without comparison to ENDF and without
!-M  error bars the user need only enter a single blank card, or,
!-M
!-M      0   13.9947    0      10.3
!-M      0    0    0    0    0    0    0    0    0    0    0
!-M
!-M  EXAMPLE INPUT NO. 2
!-M  ...................
!-M  Plot all EXFOR data 1 reference per plot with X and Y error bars.
!-M  Do not plot data unless there are 8 or more points. Plot a full
!-M  grid. Input the following 1 card,
!-M
!-M      0    0    0    1    1    0    1    8    0    1    0
!-M
!-M  NOTE: This is a good set of input parameters to use in order to
!-M  produce all possible plots of all EXFOR data translated from a
!-M  given EXFOR tape. It is recommended to specify 8 as the minimum
!-M  number of points per plot in order to avoid obtaining a large
!-M  number of plots each containing only 1 or 2 data points.
!-M
!-M  EXAMPLE INPUT NO. 3
!-M  ...................
!-M  Plot Co-59 (n,2n) ENDF cross sections and all comparable
!-M  EXFOR data on the same plot with cross section error bars, one
!-M  plot from 0.0 eV (scaled to threshold) to 20.0 MeV and a second
!-M  plot from 12.0 to 15.0 MeV. Do not plot data unless there are at
!-M  least 8 experimental data points. Only tick marks on axis. ENDL84
!-M  is the identification for the ENDF library. Input the following
!-M  3 cards.
!-M
!-M      1    1    0    0    1    0    0    8    0    0    0  ENDL84
!-M    27059   3  16 0.00000+ 0  27059   3  16 2.00000+ 7
!-M    27059   3  16 1.20000+ 7  27059   3  16 1.50000+ 7
!-M
!-M  REPORTING ERRORS
!-M  ----------------
!-M  In order to improve this code and make future versions more
!-M  compatible for use on as many different types of computers as
!-M  possible please report all compiler diagnostics and/or operating
!-M  problems to the author at the above address.
!-M
!-M  Please remember if you simply report "I'VE GOT A PROBLEM" and do
!-M  not adequately describe exactly how you were using the program
!-M  it will be impossible for the author to help you. When a problem
!-M  arises please write to the author, describe the problem in as much
!-M  detail as possible, identify the version of the program that you
!-M  are using (e.g. Version 2001-3) and send the following information
!-M  in computer-readable form (e-mail, floppy disc, etc.) to the author:
!-M
!-M  (1) A copy of the program you are using
!-M  (2) A copy of compiler diagnostics (if any)
!-M  (3) A copy of the jcl deck you used to execute the program
!-M  (4) A copy of the 3 translation dictionaries you are using
!-M  (5) A copy of the computation format data you using
!-M  (6) A copy of the output report from the program.
!-M
!-M  Also send copies of any plots which you have obtained as output
!-M  from this program, if possible and/or applicable.
!-M
!-M  Without all of this information it is impossible to exactly
!-M  simulate the problem that you ran and to determine the source
!-M  of your problem.
!-M
!***** RETRIEVAL OF DIFFERENTIAL AND DOUBLE DIFFERENTIAL DATA **********
!-M
!-M  RETRIEVAL OF DIFFERENTIAL AND DOUBLE DIFFERENTIAL DATA
!-M  ------------------------------------------------------
!-M
!-M  Retrieval of differential and double differential data is done
!-M  through the DXSEND package, which is provided as a separate
!-M  module. It is called by:
!-M
!-M     CALL DXSELM(LEF,NUC,ZEL,FRC,ZAP,MF0,MT0,KEA,EINC,PAR,EPS
!-M    1           ,ENR,DXS,RWO,NEN,MEN,MRW,LTT,ELV)
!-M
!-M  The DXSELM module reads an ENDF file and extract cross sections
!-M  (KEA=0), differential cross section (angular distributions KEA=1
!-M  or energy spectra KEA=2, parameter PAR < -2) and double
!-M  differential cross sections (correlated energy/angle distributions
!-M  with the same conventions for KEA. Parameter PAR is the requested
!-M  outgoing particle energy when the correlated angular distribution
!-M  are requested. Similarly, PAR is the cosine of the scattering
!-M  angle when the spectrum at a particular scattering angle is
!-M  requested. Differential cross sections are output in the Lab
!-M  co-ordinate system.
!-M    If a special MT number is requested (for example, MT=5 for
!-M  particle emission where particle is defined by its ZA designation
!-M  in ZAP), the retrieval is done recursively for all neutron emission
!-M  reactions and all contributions are summed.
!-M    Although the DXSELM routine allows reconstruction of elemental
!-M  data from the isotopic components, this feature cannot be
!-M  implemented in PLOTC4.
!-M
!-M  Formal parameters are:
!-M  LEF  - File unit number from which the ENDF file is read.
!-M  ZA0  - Requested nuclide identification number. If ZA>0 it is
!-M         given in terms of Z*1000+A+LIS0/10 where Z is the atomic
!-M         number, A the mass number and LIS0 the metastable state
!-M         number. When ZA<0 it implies the ENDF material MAT number.
!-M  ZAP0 - Outgoing particle ZA designation (ZAP0=1 for neutrons).
!-M  MF0  - Requested MF number according to the ENDF conventions.
!-M  MT0  - Requested reaction MT number. Broadly this follows the
!-M         ENDF conventions.
!-M  KEA  - Control flag to select retrieval of cross section (KEA=0)
!-M         angular distributions (KEA=1) of energy spectra (KEA=2).
!-M  EINC - Incident particle energy (eV).
!-M  PAR  - Fixed parameter when requesting differential data:
!-M         KEA=1, PAR is the requested outgoing particle energy.
!-M                A value PAR <= -2 implies integrated distribution
!-M                over all angles.
!-M         KEA=2, PAR is the requested scattering angle (cosine).
!-M                A value PAR <= -2 implies angle integrated energy
!-M                distribution.
!-M  EPS  - Resolution broadening parameter is used for the two-body
!-M         scattering reactions like the elastic and discrete inelastic
!-M         cross sections  where in principle the energy distribution
!-M         is a delta function. For such reactions the energy
!-M         distribution is displayed as a Gaussian distribution where
!-M         EPS the fractional half-width at half-maximum. Such
!-M         representation is convenient for comparison with measured
!-M         data.
!-M  ENR  - Argument vector of the assembled output cross section.
!-M  DXS  - Function vector of the assembled output cross section.
!-M  RWO  - Work array of length MRW.
!-M  NEN  - Number of points in the assembled output cross section
!-M         vector.
!-M  MEN  - Available size of ENR and DXS arrays.
!-M  MRW  - Available size of the RWO work array.
!-M  ELV  - Energy of the requested discrete inelastic level when
!-M         requesting angular distributions (if applicable).
!-M         completed successfully.
!-M
!-M  External routines called:
!-M   DXSELM,DXSEND,DXSEN1,SKIPSC,FINDMT,RDTAB1,RDTAB2,RDLIST,
!-M   FINT2D,YTGEOU,FNGAUS,FYTG2D,UNIGRD,FITGRD
!-M
!***** RETRIEVAL OF DIFFERENTIAL AND DOUBLE DIFFERENTIAL DATA **********
!***** COMPUTER DEPENDENT CODING ***************************************
!-M
!-M COMPUTER DEPENDENT CODING
!-M -------------------------
!-M
!-M * This program is designed to be used with a Fortran-77 or
!-M  Fortran-90 compiler.
!-M
!-M * The only compiler dependent format statements involve,
!-M  (1) CHARACTER*1 and CHARACTER*4
!-M  (2) Testing for errors and end of file during reads.
!-M
!-M * It is assumed that characters are stored in successive storage
!-M   locations and that characters may be treated as continuous strings
!-M   of characters in either CHARACTER*4 or CHARACTER*1 format.
!-M
!-M * For example, if one subroutine contains,
!-M
!-M   CHARACTER*4 BCD
!-M   DIMENSION BCD(10)
!-M
!-M  the array BCD is assumed to be an array of 40 characters in
!-M  successive byte locations.
!-M
!-M  It is assumed that this array can be passed as an argument to
!-M  another subroutine and used as CHARACTER*1, e.g.,
!-M
!-M   CALL DUMMY(BCD)
!-M
!-M   SUBROUTINE DUMMY(BCD)
!-M   CHARACTER*1 BCD
!-M   DIMENSION BCD(40)
!-M
!-M * This convention will work on all 32 bit per word computers (e.g.,
!-M   IBM or IBM compatible computers).
!-M
!-M * For longer word length computers (e.g., CDC or CRAY) it is
!-M   suggested that before implementing and using this program the
!-M   user first verify that character strings can be treated as
!-M   described above, e.g., write a simple program to read a character
!-M   string of 40 characters in CHARACTER*4 format, pass it to a
!-M   subroutine which uses the character string in CHARACTER*1 format
!-M   and print the character string in the subroutine. If the character
!-M   string is printed as a continuous string you will be able to use
!-M   this program. If the character string is not printed as a
!-M   continuous string it is not recommended that you use this program.
!-M
!-M * This program using the Fortran-77 convention for testing for
!-M   reading errors and end of file during reads, e.g.,
!-M
!-M   READ(10,1000,END=100,ERR=200) A,B,C,D
!-M
!***** COMPUTER DEPENDENT CODING ***************************************
!***** PLOTTER/GRAPHICS TERMINAL INTERFACE *****************************
!-M
!-M   PLOTTER/GRAPHICS TERMINAL INTERFACE
!-M   -----------------------------------
!-M
!-M   This program uses a simple Calcomp-like interface involving
!-M   only 3 subroutines,
!-M
!-M   PLOTS(BUF,IBUF,IPLOT) - Initialize plotter
!-M         BUF    - Plotter buffer
!-M         IBUF   - Size of plotting buffer (5000 words used)
!-M         IPLOT  - Plotter unit (16)...usually a dummy unit
!-M
!-M   PLOT(X,Y,IPEN)        - Draw or move from last location to (X,Y),
!-M                           end of current plot or end of plotting.
!-M         IPEN =   2 - Draw
!-M              =   3 - Move
!-M              =  -1 - End of current plot...advance by X,Y
!-M              = 999 - End of plotting.
!-M
!-M   PEN(ICOL)             - Select color.
!-M        ICOL  - Color = 1 to N (N = Any positive integer)
!-M
!-M  In order to interface this program for use on any plotter which
!-M  does not use the above conventions it is merely necessary for the
!-M  the user to write 3 subroutines with the names PLOTS, PLOT and PEN
!-M  with the subroutine arguments described above and to then call the
!-M  local equivalent routines.
!-M
!-M  AVAILABLE PLOTTER INTERFACES
!-M  ----------------------------
!-M  This program has available plotter interfaces to operate as
!-M  follows,
!-M  (1) Mainframe - hardcopy plots in black and white.
!-M  (2) Mainframe - screen plots in 7 colors on IBM graphics terminal.
!-M  (3) PC        - hardcopy plots in 6 colors on a Hewlett-Packard
!-M                  7475a plotter.
!-M
!-M  Contact the author to obtain copies of any of the above plotter
!-M  interfaces.
!-M
!-M  COLOR PLOTS
!-M  -----------
!-M  To select plotting colors subroutine PEN (described above) is used
!-M  to select one of the available colors. When running on a mainframe
!-M  using an IBM graphics terminal or on a PC using a Hewlett-
!-M  Packard plotter the graphics interface (described above) will
!-M  produce color plots.
!-M
!-M  BLACK AND WHITE PLOTS
!-M  ---------------------
!-M  When producing black and white hardcopy on a mainframe the user
!-M  should add a dummy subroutine PEN to the end of the program to
!-M  ignore attempts to change color. Add the following subroutine:
!-M
!-M  SUBROUTINE PEN(IPEN)
!-M  RETURN
!-M  END
!-M
!-M  CHARACTER SET
!-M  -------------
!-M  This program uses computer and plotter device independent software
!-M  characters. This program comes with a file that defines the pen
!-M  strokes required to draw all characters on the keyboard (upper
!-M  and lower case characters, numbers, etc.) plus an alternate set of
!-M  all upper and lower case greek characters and additional special
!-M  symbols.
!-M
!-M  The software character table contains X and Y and pen positions to
!-M  draw each character. If you wish to draw any additional characters
!-M  or to modify the font of the existing characters you need only
!-M  modify this table.
!-M
!-M  CONTROL CHARACTERS
!-M  In the software character table all characters to be plotted will
!-M  have pen position = 2 (draw) or = 3 (move). In addition the table
!-M  currently contains 4 control characters:
!-M
!-M  PEN POSITION = 0
!-M  Shift the next printed character by X and Y. 3 control characters
!-M  are presently included in the software character table to allow
!-M  shifting.
!-M
!-M  {   = Shift up (for superscripts..............X= 0.0, Y= 0.5)
!-M  }   = Shift down (for subscripts..............X= 0.0, Y=-0.5)
!-M  \   = Shift left 1 character (for backspace...X=-1.0, Y= 0.0)
!-M
!-M  PEN POSITION =-1
!-M  Select the next printed character from the alternate character
!-M  set. At present this control character is,
!-M
!-M  ]   = Switch to alternate character set
!-M
!-M  These 4 control characters are only defined by the value of the
!-M  pen position in the software character table (i.e., they are not
!-M  hard wired into this program). As such by modifying the software
!-M  character table the user has the option of defining any control
!-M  characters to meet specific needs.
!-M
!-M  These characters may be used in character strings to produce
!-M  special effects. For example, to plot subscript 5, B, superscript
!-M  10 use the string,
!-M
!-M  }5B{1{0
!-M
!-M  To plot B, subscript 5 and superscript 10 with the 5 directly
!-M  below the 1 of the 10 use the string,
!-M
!-M  B}5\{1{0
!-M
!-M  To plot upper case greek gamma followed by the words "Total Width"
!-M  use the string,
!-M
!-M  ]G Total Width
!-M
!-M  NOTE: When these control characters are used they only effect the
!-M  next 1 printed character (see, above example of plotting super-
!-M  script 10 where the shift up control character was used before the
!-M  1 and then again before the 0).
!-M
!-M  If these 4 control characters are not available on your computer
!-M  you can modify the software character table to use any other 4
!-M  characters that you do not normally use in character strings (for
!-M  details see the software character table).
!-M
!-M  STANDARD/ALTERNATE CHARACTER SETS
!-M  The software character table contains 2 sets of characters, which
!-M  are a standard set (all characters on an IBM keyboard) and an
!-M  alternate set (upper and lower case greek characters and special
!-M  characters). To draw a character from the alternate character set
!-M  put a vertical stroke character (]) before a character (see the
!-M  above example and the software character table for details). This
!-M  control character will only effect the next 1 plotted character.
!-M
!-M  SUB AND SUPER SCRIPTS
!-M  To draw subscript preceed a character by }. To draw superscript
!-M  preceed a character by { (see the above example and the software
!-M  character table for details). These control character will only
!-M  effect the next 1 plotted character.
!-M
!-M  BACKSPACING
!-M  To backspace one character preceed a character by \ (see, the
!-M  above example and the software character table for details). This
!-M  control character will perform a true backspace and will effect
!-M  all following characters in the same character string.
!-M
!***** PLOTTER/GRAPHICS TERMINAL INTERFACE *****************************
   data refx/'OTHE','RS  ',' ',' ',' ',' ',' ',' ',' '/
   data blank/'    '/
   data blank1/' '/
!  ----DEFINE ALL I/O UNITS.
   data p4inp/'PLOTC4.INP'/p4lst/'PLOTC4.LST'/c4dat/'C4.DAT'/edat/'ENDF.DAT'/zadef/'ENDFZA.DAT'/mfdef/'ENDFMF.DAT'/mtdef/         &
       &'ENDFMT.DAT'/chtab/'PLOT.CHR'/
!  ----CHANGE THIS DATA STATEMENT TO IDENTIFY UPDATED VERSIONS
   data versez/'   P','ROGR','AM P','LOTC','4   ','   (','VERS','ION ','2001','-3) ','NUCL','EAR ','DATA',' SEC','TION','    ',   &
       &'IAEA','  VI','ENNA','    '/
   do i = 1,4
      NVErse(i) = 20
      do j = 1,5
         VERses(j,i) = versez(j,i)
      enddo
   enddo
   idx = 0
   INP = 4
   OUTp = 7
   ITApe1 = 10
   ITApe2 = 11
   NTApe1 = 12
   NTApe2 = 14
   NTApe3 = 15
   ISCr = 18
   ISYm = 17
   open(unit=INP,file=p4inp,status='OLD')
   open(unit=ITApe1,file=c4dat,status='OLD')
   open(unit=ITApe2,file=edat,status='UNKNOWN')
   open(unit=NTApe1,file=zadef,status='OLD')
   open(unit=NTApe2,file=mfdef,status='OLD')
   open(unit=NTApe3,file=mtdef,status='OLD')
   open(unit=ISYm,file=chtab,status='OLD')
   open(unit=ISCr,file='SCR.',form='UNFORMATTED',status='UNKNOWN')
   open(unit=OUTp,file=p4lst,status='UNKNOWN')
!  ----LOAD SOFTWARE CHARACTERS.
   call SYMIN
!  ----DEFINE LINE THICKNESS PARAMETERS.
   ITHick = 0
   THIck = 0.002
!  ----INITIALIZE FLAG TO SHOW PROGRAM IS RUNNING ON MAINFRAME (WILL BE
   IBMpc = 0
!  ----INITIALIZE NUMBER OF PLOTS GENERATED.
   NUMplt = 0
!  ----IDENTIFY PROGRAM.
   write(OUTp,6000)(VERses(i,2),i=2,5)
!  ----READ ZA/MF/MT TRANSLATION TABLES.
   call ZAMFMT
!  ----READ ALL INPUT PARAMETERS AND INITIALIZE PLOTTER.
   call READIN
!  ----INITIALIZE ENDF/B AND EXFOR POINT COUNTS AND ENDF/B MAT NUMBER.
   N2 = 0
   IEX = 0
   MAT = 0
!  ----INITIALISE LAST REFERENCE AS "OTHERS".
   do j = 1,9
      REFs(j,MAXref+1) = refx(j)
   enddo
!  ----PRINT TITLE FOR OUTPUT REPORT.
   write(OUTp,6040)
!
!  READ ENDF/B AND/OR EXFOR DATA AND MAKE PLOTS.
!
!  ----IF COMPARISON MODE READ NEXT SECTION OF ENDF/B DATA.
!...
!...      print *,'mymode',mymode
!...
   20 continue
   if(MYMode<2)then
   else
      ievend = 0
      call GETEV(ievend)
!...
!...  PRINT *,'MF,MT,IEVEND',MF,MT,IEVEND
!...
!     ----END OF RUN IF END OF ENDF/B DATA.
      if(ievend/=0)goto 80
   endif
!  ----READ COMPARABLE EXFOR DATA.
   30 continue
   iexend = 0
   call GETEX(iexend)
!...
!...      PRINT *,'MF,MT,IEXEND,IEX',MF,MT,IEXEND,IEX
!...
!  ----NO PLOT IF LESS THAN MINIMUM NUMBER OF EXFOR POINTS.
   if(IEX<MINnie)then
   else
!     ----IF COMPARING TO ENDF/B SET ALL METASTABLE STATE FLAGS TO BLANK.
      if(MYMode<2)then
      else
!...     MSTAT1=BLANK
!...     MSTAT2=BLANK
         MSTar1 = blank1
         MSTar2 = blank1
!        ----PREPARE ENDF DOUBLE DIFFERENTIAL DATA FOR COMPARISON
         if(MF<4.or.MF>6)then
         else
            N2 = 0
            par = -2
!*          Outgoing particle ZA designation
            if(MFIn==4)then
               if(MT==2)IZArat = 1
               if(MT>50.and.MT<91)IZArat = 1
               if(MT>=600.and.MT<649)IZArat = 1001
               if(MT>=800.and.MT<849)IZArat = 2004
            endif
            zap = IZArat
!*          Select plot as a function of outgoing particle angle or energy
            if(MFIn==4)then
               kea = 1
            elseif(MFIn==5)then
               kea = 2
            elseif(MFIn==6)then
               if(IDOub==2)then
                  kea = 2
                  deg = ACOS(FIEld4(1))*180/3.1415926
                  par = deg
               else
                  kea = 1
                  par = FIEld4(3)
               endif
            endif
!*          Energy level (if applicable)
            elv = E2T(1)
!*          Unit LTT is for messages from DXSEND (set zero to suppress)
            ltt = 6
            zaa = IZA
            MPT = MXPNT
            mtx = MT
            if(MT==9000)mtx = 5
!...        Temporary diagnostic printout
            print *,'MFIN,MF,MT,ZAP,KEA',MFIn,MF,mtx,NINT(zap),kea
            nuc = 0
            zel(1) = zaa
            call DXSELM(ITApe2,nuc,zel,frc,zap,MFIn,mtx,kea,EINc,par,EP6,XPAge,YPAge,zpage,rwo,N2,MPT,MXRW,ltt,elv)
!...        Temporary diagnostic printout
            print *,'ZAA,ZAP,MTX,EIN,PAR,KEA,NP',IFIX(zaa),NINT(zap),mtx,EINc,par,IDOub,N2
!...        IF(IDOUB.EQ.1) THEN
!...        PRINT *,(XPAGE(J),J=1,N2)
!...        PRINT *,(YPAGE(J),J=1,N2)
!...        END IF
!...
!*          Skip the plot if no comparable data to plot
            if(N2==0)goto 60
            IBAse = 0
            ITOp = N2
         endif
      endif
!     ----DEFINE HOLLERITH EQUIVALENT OF ZA.
      call ZAHOL(IZA,MSTat1,ZABcd,IZAbcd)
!     ----SAVE INPUT MF AND DEFINE INTERNAL MF. NO PLOT IF MF NOT IN LEGAL
!     ----RANGE.
      if(MF==10)MF = 3
      if(MF==203)MF = 3
      if(MF==402)MF = 3
      if(MF==154)MF = 7
      if(MF==801)MF = 8
      if(MF<1.or.MF>10)then
      else
!        ----PRINT DESCRIPTION OF DATA TO BE PLOTTED.
         idx = idx + 1
         izap = NINT(zap)
         if(MF==3.or.MF==10)then
            write(OUTp,6050)(ZABcd(i),i=1,3),izap,MFIn,MT,N2,IEX,IREf,idx
         elseif(MFIn==4)then
            if((MT>50.and.MT<91).or.(MT>=600.and.MT<649).or.(MT>=800.and.MT<849))then
               write(OUTp,6053)(ZABcd(i),i=1,3),izap,MFIn,MT,N2,IEX,IREf,EINc,elv,idx
            else
               write(OUTp,6051)(ZABcd(i),i=1,3),izap,MFIn,MT,N2,IEX,IREf,EINc,idx
            endif
         elseif(MFIn==6)then
            if(IDOub==2)then
               write(OUTp,6052)(ZABcd(i),i=1,3),izap,MFIn,MT,N2,IEX,IREf,EINc,deg,idx
            else
               write(OUTp,6053)(ZABcd(i),i=1,3),izap,MFIn,MT,N2,IEX,IREf,EINc,par,idx
            endif
         else
            write(OUTp,6051)(ZABcd(i),i=1,3),izap,MFIn,MT,N2,IEX,IREf,EINc,idx
         endif
!
!        SET UP LOOP OVER REQUESTS. IF NO REQUESTS SET UP ONCE THROUGH LOOP
!
         do IGEt = 1,NGEt
!           ----SELECT POINTS TO APPEAR ON NEXT PLOT.
            call SCALER
!           ----NO PLOT IF LESS THAN MINIMUM NUMBER OF POINTS.
            if(MPT<MINnie)then
            else
!              ----SELECT AXIS UNITS.
               call UNITED
!              ----PLOT BORDER AND ALL AXIS LABELS.
               call BORDER
!              ----PLOT X AND Y AXIS GRID
               if(MYGrid==0)call GRID0
               if(MYGrid/=0)call GRID1
!              ----PLOT ENDF/B DATA (IF ANY).
               if(MYMode>=2)call EVALP
!              ----PLOT EXPERIMENTAL DATA.
               call EXFORP
!              ----END OF PLOT. ADVANCE TO NEXT PLOTTING AREA.
               call NXTPLT
            endif
         enddo
      endif
   endif
!  ----END OF EXFOR DATA = END OF PLOTTING.
   60 continue
   if(iexend==2)then
   else
!     ----IF ALL EXFOR DATA HAS NOT YET BEEN READ CONTINUE READING EXFOR
!     ----(IF NO COMPARISON...NO NEED TO READ ENDF/B)
!     ----(IF COMPARISON......THERE MAY BE MORE EXFOR DATA TO COMPARE TO
!     ----CURRENT SECTION OF ENDF/B DATA).
      if(iexend<=0)goto 30
!     ----IF COMPARISON MODE CONTINUE READING ENDF/B DATA.
      if(MYMode>=2)goto 20
!     ----NO COMPARISON. IF PLOTTING ALL REFERENCES TOGETHER CONTINUE
!     ----READING UNTIL NO MORE DATA. OTHERWISE END OF RUN.
      if(MYMode==1.and.iexend<2)goto 30
   endif
!  ----END OF RUN.
   80 continue
   call ENDPLOTS
   write(OUTp,6060)NUMplt
   stop
 6000 format(' PLOT ENDF/B AND/OR EXFOR DATA (PLOTC4 ',4A4/1x,72('='))
 6040 format(1x,72('=')/' PROCESSING'/1x,75('=')/' MATERIAL     ZAP  MF   MT  EVAL. EXPR. ',                                      &
           & 'EXPR.    E-INC ANG-OUT ELV/E-OUT IDX'/'                            PNTS. PNTS. ',                                   &
            &' REF.       EV     DEG        EV    '/1x,75('='))
 6050 format(1x,3A4,i4,i4,i5,3I6,28x,i4)
 6051 format(1x,3A4,i4,i4,i5,3I6,1pe10.3,18x,i4)
 6052 format(1x,3A4,i4,i4,i5,3I6,1pe10.3,0p,f8.2,10x,i4)
 6053 format(1x,3A4,i4,i4,i5,3I6,1pe10.3,8x,e10.3,i4)
 6060 format(1x,72('=')/' END OF RUN',20x,i6,' PLOTS GENERATED')
   end program PLOTC4
   subroutine SETUP
   implicit none
!*--SETUP1323
!
!
! COMMON variables
!
   real :: ADVanc,BOX,BOX2,BOX4,BOXwt2,HT,HT2,HT34,HTH,PAPerx0,PAPerx1,PAPery0,PAPery1,SIZplt,WT,WT38,WTH
   integer :: IBMpc,IPLotz,NUMplt
   integer,dimension(4) :: NVErse
   real,dimension(10) :: TABlog
   character(4),dimension(5,4) :: VERses
   real,dimension(2) :: XHP,XINch,YHP,YINch
   real,dimension(2,4) :: XPInch,YPInch
   common /HPINCH/ XHP,YHP
   common /INCHES/ XINch,YINch
   common /INCHTB/ XPInch,YPInch
   common /LOGTAB/ TABlog
   common /PAINT / BOX,BOX2,BOX4,BOXwt2,HT,HT2,HTH,HT34,WT,WTH,WT38
   common /PAPER / PAPerx0,PAPerx1,PAPery0,PAPery1
   common /PLOTN / NUMplt,ADVanc
   common /PLTSIZ/ SIZplt,IPLotz,IBMpc
   common /VERSC / VERses
   common /VERSI / NVErse
!
! Local variables
!
   real :: ALOG10,FLOAT
   real :: htv,wtv,x,xl,xmid,y
   integer :: i,ii,j,l
!
!
!
!  DEFINE ALL PLOTTER PARAMETERS AND INITIALIZE PLOTTER.
!
!  ----DEFINE HEIGHT AND WIDTH OF CHARACTERS FOR PROGRAM I.D.
   data htv/0.28/
   data wtv/0.28/
!  ----SELECT FULL OR HALF SIZE PLOTS.
   if(IPLotz==0)then
!
!     FULL SIZE PLOTS.
!
      SIZplt = 1.0
      XINch(1) = PAPerx0
      XINch(2) = PAPerx1*0.65
      YINch(1) = PAPery0
      YINch(2) = PAPery1
      BOX = 0.008*(PAPerx1-PAPerx0)
      HT = 0.012*(PAPerx1-PAPerx0)
   else
!
!     HALF SIZE PLOTS.
!
      SIZplt = 0.5
      ii = 0
      do i = 1,2
         do j = 1,2
            ii = ii + 1
            XPInch(1,ii) = PAPerx0 + 0.5*PAPerx1*FLOAT(j-1)
            XPInch(2,ii) = XPInch(1,ii) + 0.45*(PAPerx1-PAPerx0)*0.65
            YPInch(1,ii) = PAPery0 + 0.5*PAPery1*FLOAT(2-i)
            YPInch(2,ii) = YPInch(1,ii) + 0.45*(PAPery1-PAPery0)
         enddo
      enddo
      XINch(1) = XPInch(1,1)
      XINch(2) = XPInch(2,1)
      YINch(1) = YPInch(1,1)
      YINch(2) = YPInch(2,1)
      BOX = 0.008*(PAPerx1-PAPerx0)*SIZplt
      HT = 0.012*(PAPerx1-PAPerx0)*SIZplt
   endif
!  ----DEFINE X AND Y SIZE OF PLOTS (ONLY TO DEFINE PAPER PLOTTER
!  ----ADVANCE AND MAXIMUM SIZE FOR HEWLETT-PACKARD PLOTTER).
   WT = HT
   XHP(1) = PAPerx0
   XHP(2) = PAPerx1*0.6
   YHP(1) = PAPery0
   YHP(2) = PAPery1
!  ----DEFINE DISTANCE TO ADVANCE IN X DIRECTION BETWEEN PLOTS.
   ADVanc = 1.75*XHP(2)
!
!  INITIALIZE PLOTTER
!
!  IBMPC IS USED TO INDICATE WHETHER THE PROGRAM IS RUNNING ON THE
!  MAINFRAME OR ON AN IBM-PC.
!
!  IBMPC IS INITIALIZED TO -1 IN THE MAIN AND IF RUNNING ON THE
!  MAINFRAME IT WILL NOT CHANGE.
!
!  IF RUNNING ON AN IBM-PC THE PLOTTER INTERFACE ROUTINE PLOTS WILL
!  CHANGE THE VALUE OF IBM AS FOLLOWS,
!
!  IBMPC = 0 - PC FULL SIZE PLOTS.
!  = 1 - PC HALF SIZE PLOTS.
!
   call STARPLOT
!  ----DEFINE ALL BOX AND CHARACTER HEIGHTS AND WIDTHS (ALL SCALED TO
!  ----BOX AND HT DEFINED ABOVE).
   BOX2 = BOX/2.0
   BOX4 = BOX/4.0
   BOXwt2 = 4.0*BOX/7.0
   HT2 = 2.0*HT
   HTH = HT/2.0
   HT34 = 3.0*HT/4.0
   WTH = WT/2.0
   WT38 = 3.0*WT/8.0
!  ----DEFINE POSITION OF PLOT LABELS.
   call SPOTER
!  ----CONSTRUCT ONE DECADE OF TABLE OF LOGS (FOR AXIS LABELS).
   do i = 1,10
      xl = i
      TABlog(i) = ALOG10(xl)
   enddo
!
!  IF RUNNING ON MAINFRAME IDENTIFY PROGRAM AND INSTALLATION.
!
   if(IBMpc>=0)then
   else
      call PEN(2)
      y = 0.5*(YHP(1)+YHP(2)) + 2.0*htv
      xmid = 0.5*(XHP(1)+XHP(2))
      do l = 1,4
         x = xmid - 0.5*FLOAT(NVErse(l))*wtv
         call SYMBLH(x,y,htv,VERses(1,l),0.0,NVErse(l))
         y = y - 1.75*htv
      enddo
!     ----ADVANCE TO NEXT PLOTTING AREA.
      call NEXTPLOT
   endif
   return
   end subroutine SETUP
   subroutine NXTPLT
   implicit none
!*--NXTPLT1458
!
!
! COMMON variables
!
   real :: ADVanc,BOX,BOX2,BOX4,BOXwt2,HT,HT2,HT34,HTH,SIZplt,WT,WT38,WTH
   integer :: IBMpc,IPLotz,NUMplt
   real,dimension(2) :: XHP,XINch,YHP,YINch
   real,dimension(2,4) :: XPInch,YPInch
   common /HPINCH/ XHP,YHP
   common /INCHES/ XINch,YINch
   common /INCHTB/ XPInch,YPInch
   common /PAINT / BOX,BOX2,BOX4,BOXwt2,HT,HT2,HTH,HT34,WT,WTH,WT38
   common /PLOTN / NUMplt,ADVanc
   common /PLTSIZ/ SIZplt,IPLotz,IBMpc
!
! Local variables
!
   integer :: ii
!
!
!
!  ADVANCE TO NEXT PLOTTER AREA.
!
!  ----MOVE TO ORIGIN WITH PEN UP.
   call PLOT(0.0,0.0,3)
!  ----IF FULL PLOT SIZE ADVANCE TO NEXT PLOT.
   if(IPLotz==0)then
   else
!     ----DEFINE COORDINATES OF NEXT SUB-PLOT.
      IPLotz = IPLotz + 1
      ii = IPLotz
      if(IPLotz>4)IPLotz = 1
      XINch(1) = XPInch(1,IPLotz)
      XINch(2) = XPInch(2,IPLotz)
      YINch(1) = YPInch(1,IPLotz)
      YINch(2) = YPInch(2,IPLotz)
!     ----DEFINE POSITION OF PLOT LABELS.
      call SPOTER
!     ----IF LAST PLOT IS FULL, ADVANCE TO NEXT PLOTTING AREA.
      if(ii<=4)goto 20
   endif
!  ----END OF PLOT. ADVANCE TO NEXT PLOTTING AREA.
   call NEXTPLOT
   20 continue
   return
   end subroutine NXTPLT
   subroutine SPOTER
   implicit none
!*--SPOTER1510
!
!
! COMMON variables
!
   real :: BOT1,BOT2,BOT3,BOX,BOX2,BOX4,BOXwt2,HT,HT2,HT34,HTH,RIGht1,RIGht2,RIGht3,RIGht4,RIGht5,RIGht6,RIGht7,RIGht8,TOP1,TOP2, &
         & TOP3,WT,WT38,WTH
   real,dimension(2) :: XINch,YINch
   common /INCHES/ XINch,YINch
   common /PAINT / BOX,BOX2,BOX4,BOXwt2,HT,HT2,HTH,HT34,WT,WTH,WT38
   common /SPOTS / TOP1,TOP2,TOP3,BOT1,BOT2,BOT3,RIGht1,RIGht2,RIGht3,RIGht4,RIGht5,RIGht6,RIGht7,RIGht8
!
!
!
!  DEFINE POISITION OF AXIS LABELS AND LEGEND BOX COORDINATES.
!
!  TOP1   = Y POSITION OF TOP TITLE LINE
!  TOP2   = Y POSITION OF SECOND TITLE LINE
!  TOP3   = Y POSITION OF THIRD TITLE LINE
!  BOT1   = Y POSITION OF X AXIS NUMBERS (LINEAR)
!  = Y POSITION OF X AXIS LOG EXPONENTS (LOG)
!  BOT2   = Y POSITION OF X AXIS NUMBERS (LOG)
!  BOT3   = Y POSITION OF X AXIS LABEL
!  RIGHT1 = X POSITION OF Y AXIS NUMBERS
!  RIGHT2 = X POSITION OF Y AXIS LABEL
!  RIGHT3 = X POSITION OF START OF LEGEND BOX
!  RIGHT4 = X POSITION OF END OF LEGEND BOX
!  RIGHT5 = X POSITION OF REFERENCE/ENERGY RANGE TITLE
!  RIGHT6 = X POSITION OF POINTS TITLE
!  RIGHT7 = X POSITION OF START OF BOX TO IDENTIFY REFERENCE
!  RIGHT8 = X POSITION OF END OF BOX TO IDENTIFY REFERENCE
!
   TOP3 = YINch(2) + HT
   TOP2 = TOP3 + 1.75*HT
   TOP1 = TOP2 + 1.75*HT
   BOT1 = YINch(1) - 1.75*HT
   BOT2 = YINch(1) - 2.75*HT
   BOT3 = YINch(1) - 4.5*HT
   RIGht1 = XINch(2) + WT
   RIGht2 = XINch(2) + 7.0*HT
   RIGht3 = XINch(2) + 8.0*WT
   RIGht4 = RIGht3 + 4.0*BOX + 30.0*WT
   RIGht5 = RIGht3 + 4.0*BOX + WT
   RIGht6 = RIGht5 + 22.0*WT
   RIGht7 = RIGht3 + 2.0*BOX
   RIGht8 = RIGht7 + 2.0*BOX
   return
   end subroutine SPOTER
   subroutine READIN
   implicit none
!*--READIN1563
!
!
! PARAMETER definitions
!
   integer,parameter :: MXPGP = 10000
!
! COMMON variables
!
   real :: BOX,BOX2,BOX4,BOXwt2,HT,HT2,HT34,HTH,PAPerx0,PAPerx1,PAPery0,PAPery1,SIZplt,WT,WT38,WTH
   real,dimension(48) :: E2T,EXHigh,EXLow
   real,dimension(100) :: EHGet,ELGet,EPGet
   integer :: IBMpc,IGEt,IGRoup,ILIb,IMEndf,INP,IPLotz,IREf,ISCale,ITApe1,ITApe2,IXErr,IYErr,KGRoup,MAXie,MAXref,MINnie,MREf,     &
            & MSYmbl,MYGrid,MYMode,NGEt,OUTp
   integer,dimension(100) :: INTrn,IZAhi,IZAlow,MFHi,MFLow,MTHi,MTLow
   character(4),dimension(4) :: LIBnam
   integer,dimension(48) :: LREf
   real,dimension(2) :: XINch,YINch
   common /ENDFIM/ IMEndf
   common /EXERRS/ IXErr,IYErr
   common /GETEM / IZAlow,MFLow,MTLow,ELGet,IZAhi,MFHi,MTHi,EHGet,EPGet,INTrn,NGEt,IGEt
   common /GRIDDY/ MYGrid
   common /INCHES/ XINch,YINch
   common /INPARM/ MINnie,MAXie
   common /LIBC  / LIBnam
   common /LIBI  / ILIb
   common /MODEMY/ MYMode
   common /PAINT / BOX,BOX2,BOX4,BOXwt2,HT,HT2,HTH,HT34,WT,WTH,WT38
   common /PAPER / PAPerx0,PAPerx1,PAPery0,PAPery1
   common /PLTSIZ/ SIZplt,IPLotz,IBMpc
   common /REFERI/ LREf,EXLow,EXHigh,E2T,IREf,MREf,MAXref,IGRoup,KGRoup
   common /SCALEI/ ISCale
   common /SYMBLM/ MSYmbl
   common /UNITS / INP,OUTp,ITApe1,ITApe2
!
! Local variables
!
   character(4),dimension(2) :: answer,exfor,hsize
   character(4) :: blank,endf
   real :: emax,papx0,papx1,papy0,papy1
   character(4),dimension(4,3) :: endfan
   character(4),dimension(3,2) :: gridty
   integer :: i,imx4,jget,mxr
   character(80) :: line
   character(4),dimension(3,3) :: scalem
!
!
!
!  READ ALL INPUT PARAMETERS AND INITIALIZE PLOTTER.
!
   data endfan/'  NO','    ','    ','    ',' YES','    ','    ','    ',' YES',' (ID','ENTI','FY) '/
   data answer/' NO','YES'/
   data endf/'ENDF'/
   data exfor/'  EX','FOR '/
   data blank/'    '/
   data scalem/'ENDF',' + E','XFOR','    ','    ','ENDF','    ','   E','XFOR'/
   data gridty/'TICK',' MAR','KS  ',' FUL','L GR','ID  '/
   data hsize/'FULL','HALF'/
!  ----DEFINE UPPER DEFAULT ENERGY TO BE 100 MEV.
   data emax/1.0E+10/
!  ----DEFAULT PAPER SIZE
   PAPerx0 = 0
   PAPerx1 = 12.5
   PAPery0 = 0.5
   PAPery1 = 10
!  ----READ INPUT PARAMTERS.
   read(INP,4030)papx0,papx1,papy0,papy1
   if(papx0/=0)PAPerx0 = papx0
   if(papx1/=0)PAPerx1 = papx1
   if(papy0/=0)PAPery0 = papy0
   if(papy1/=0)PAPery1 = papy1
   read(INP,4000)IMEndf,MAXref,ISCale,IXErr,IYErr,MSYmbl,IGRoup,MINnie,MAXie,MYGrid,IPLotz,LIBnam
   imx4 = 0
   if(MAXref<1)MAXref = 1
   if(MAXref>1)imx4 = 1
   if(IMEndf>2)IMEndf = 2
   if(IMEndf<=0)IMEndf = 0
   if(ISCale<0.or.ISCale>2)ISCale = 0
   if(IMEndf==0)ISCale = 2
   if(IXErr/=0)IXErr = 1
   if(IYErr/=0)IYErr = 1
   if(MSYmbl/=0)MSYmbl = 1
   if(IGRoup/=0)IGRoup = 1
   if(MINnie<=0)MINnie = 8
   if(MAXie<=MINnie)MAXie = MXPGP
   if(MAXie>MXPGP)MAXie = MXPGP
   if(MYGrid/=0)MYGrid = 1
   if(IPLotz/=0)IPLotz = 1
   write(OUTp,6000)(endfan(i,IMEndf+1),i=1,4),answer(imx4+1),(scalem(i,ISCale+1),i=1,3),answer(IXErr+1),answer(IYErr+1),          &
                 & answer(MSYmbl+1),answer(IGRoup+1),MINnie,MAXie,(gridty(i,MYGrid+1),i=1,3),hsize(IPLotz+1)
!  ----DEFINE INTERNAL RUN MODE.
   MYMode = imx4 + 2*IMEndf
   if(IMEndf==2)MYMode = imx4 + 2
   if(MYMode<2)then
!     ----INITIALIZE LIBRARY I.D. TO EXFOR (ADD AN/SAN LATER).
      LIBnam(1) = exfor(1)
      LIBnam(2) = exfor(2)
   else
!     ----LEFT ADJUST ENDF/B LIBRARY I.D....IF COMPLETELY BLANK DEFINE
!     ----LIBRARY I.D. TO BE ENDF.
      call SHIFTY(LIBnam,ILIb)
      if(ILIb>0)then
      else
         ILIb = 4
         LIBnam(1) = endf
         do i = 2,4
            LIBnam(i) = blank
         enddo
      endif
      write(OUTp,6010)LIBnam
   endif
!  ----DEFINE ALL PLOTTER PARAMETERS AND INITIALIZE PLOTTER.
   call SETUP
!  ----LIMIT THE NUMBER OF DISPLAYED REFERENCES BY AVAILABLE SPACE
   mxr = (YINch(2)-YINch(1))/(1.75*HT) - 2
   MAXref = MIN(mxr,MAXref,48)
!  ----IDENTIFY COMPUTER THAT PROGRAM IS RUNNING ON.
   if(IBMpc<0)write(OUTp,6012)
   if(IBMpc>=0)write(OUTp,6014)
!  ----PRINT TITLE BEFORE FIRST REQUEST.
   write(OUTp,6020)
   NGEt = 0
   do jget = 1,100
      NGEt = NGEt + 1
      do while (.true.)
         read(INP,4020,end=80)line
         if(line(1:26)=='                          '.and.line(27:52)=='                          ')goto 80
         read(line,4010,err=80)IZAlow(NGEt),MFLow(NGEt),MTLow(NGEt),ELGet(NGEt),IZAhi(NGEt),MFHi(NGEt),MTHi(NGEt),EHGet(NGEt),    &
                             & INTrn(NGEt),EPGet(NGEt)
!        ----IF COMPARING TO ENDF/B SET MF = 3 (ONLY CROSS SECTIONS) AND
!        ---- MT = ONLY UP TO 999.
         if(MYMode<2)then
         else
            if(MFHi(NGEt)<3.or.MFLow(NGEt)>6)cycle
            if(MFLow(NGEt)<3)MFLow(NGEt) = 3
            if(MFHi(NGEt)>10)MFHi(NGEt) = 10
         endif
!        ----IF REQUIRED SET UPPER ZA/MF/MT LIMIT TO LOWER LIMIT.
         if(IZAhi(NGEt)<IZAlow(NGEt))IZAhi(NGEt) = IZAlow(NGEt)
         if(MTHi(NGEt)<MTLow(NGEt))MTHi(NGEt) = MTLow(NGEt)
         if(MFHi(NGEt)<MFLow(NGEt))MFHi(NGEt) = MFLow(NGEt)
!        ----IF REQUIRED SET UPPER ENERGY LIMIT TO 100 MEV.
         if(EHGet(NGEt)<=0.0)EHGet(NGEt) = emax
!        ----PRINT REQUEST.
         write(OUTp,6030)IZAlow(NGEt),MFLow(NGEt),MTLow(NGEt),ELGet(NGEt),IZAhi(NGEt),MFHi(NGEt),MTHi(NGEt),EHGet(NGEt)
         if(EHGet(NGEt)>ELGet(NGEt))exit
         write(OUTp,6040)
      enddo
   enddo
   NGEt = 101
   80 continue
   NGEt = NGEt - 1
   if(NGEt>0)then
   else
!     ----NO REQUESTS. DEFINE DEFAULT REQUEST TO PLOT AS MUCH DATA AS
!     ----POSSIBLE.
      IZAlow(1) = 1
      IZAhi(1) = 999999
      MFLow(1) = 1
      MFHi(1) = 999
      MTLow(1) = 1
      MTHi(1) = 9999
      ELGet(1) = 0.0
      EHGet(1) = emax
      NGEt = 1
      write(OUTp,6030)IZAlow(NGEt),MFLow(NGEt),MTLow(NGEt),ELGet(NGEt),IZAhi(NGEt),MFHi(NGEt),MTHi(NGEt),EHGet(NGEt)
   endif
   return
 4000 format(11I5,4A4)
 4010 format(i7,2I4,e11.4,i7,2I4,e11.4,i3,e11.4)
 4020 format(a80)
 4030 format(4F10.0)
 6000 format(1x,72('=')/' READING INPUT PARAMETERS'/1x,72('=')/' COMPARE EXFOR DATA TO ENDF/B DATA----------',12x,                &
            &4A4/' ALL COMPARABLE EXFOR DATA ON SAME PLOT-----',13x,a3/' SCALE PLOTS ACCORDING TO-------------------',4x,         &
            &3A4/' PLOT X ERROR BARS--------------------------',13x,a3/' PLOT Y ERROR BARS--------------------------',13x,        &
            &a3/' IDENTIFY ALL REFERENCES BY SYMBOL----------',13x,a3/' ALLOW VARIABLE E2 ON SAME PLOT-------------',13x,         &
            &a3/' MINIMUM EXFOR POINTS PER PLOT--------------',i16/' MAXIMUM EXFOR POINTS PER PLOT--------------',                &
            &i16/' GRID TYPE----------------------------------',6x,2A4,a2/' PLOT SIZE----------------------------------',12x,a4)
 6010 format(' EVALUATED DATA I.D.------------------------',4A4)
 6012 format(' COMPUTER TYPE------------------------------','       MAINFRAME')
 6014 format(' COMPUTER TYPE------------------------------','          IBM-PC')
 6020 format(1x,72('=')/' PLOT THE FOLLOWING DATA'/1x,72('=')/'           LOWER LIMIT               UPPER LIMIT'/                 &
            &'      ZA  MF  MT  ENERGY-EV     ZA  MF  MT  ENERGY-EV'/1x,72('='))
 6030 format(1x,i7,2I4,1pe11.4,i7,2I4,1pe11.4)
 6040 format(' UPPER ENERGY LESS THAN LOWER ENERGY...REQUEST IGNORED')
   end subroutine READIN
   subroutine SHIFTY(Libnam,Ilib)
   implicit none
!*--SHIFTY1754
!
!
! Dummy arguments
!
   integer :: Ilib
   character(1),dimension(16) :: Libnam
   intent (inout) Ilib,Libnam
!
! Local variables
!
   character(1) :: blank
   integer :: i,ii,j
!
!
!
!  LEFT ADJUST LIBRARY I.D.
!
   data blank/' '/
!  ----SKIP LEADING BLANKS.
   do i = 1,16
      if(Libnam(i)/=blank)goto 20
   enddo
!  ----NAME IS COMPLETELY BLANK
   Ilib = 0
   return
!  ----LEFT ADJUST NAME.
   20 continue
   ii = 0
   Ilib = 0
   do j = i,16
      ii = ii + 1
      Libnam(ii) = Libnam(j)
      if(Libnam(ii)/=blank)Ilib = ii
   enddo
!  ----BLANK OUT REMAINDER (IF ANY).
   if(Ilib>=16)then
   else
      ii = Ilib + 1
      do i = ii,16
         Libnam(i) = blank
      enddo
   endif
   return
   end subroutine SHIFTY
   subroutine GETEV(Iend)
   implicit none
!*--GETEV1804
!
!
! PARAMETER definitions
!
   integer,parameter :: MXPNT = 90000
!
! COMMON variables
!
   real :: AWR,EINc
   real,dimension(100) :: EHGet,ELGet,EPGet
   integer :: IBAse,IGEt,INP,ISCr,ITApe1,ITApe2,ITOp,IZA,MAT,MF,MODiza,MT,N2,NGEt,OUTp
   integer,dimension(100) :: INTrn,IZAhi,IZAlow,MFHi,MFLow,MTHi,MTLow
   character(4) :: MSTat1,MSTat2
   real,dimension(MXPNT) :: XPAge,YPAge
   real,dimension(4) :: ZABcd
   common /GETEM / IZAlow,MFLow,MTLow,ELGet,IZAhi,MFHi,MTHi,EHGet,EPGet,INTrn,NGEt,IGEt
   common /PAGEXY/ XPAge,YPAge,N2,IBAse,ITOp,ISCr
   common /UNITS / INP,OUTp,ITApe1,ITApe2
   common /WHEREC/ ZABcd,MSTat1,MSTat2
   common /WHEREI/ IZA,MAT,MF,MT,MODiza,EINc,AWR
!
! Dummy arguments
!
   integer :: Iend
   intent (out) Iend
!
! Local variables
!
   real,save :: c1,c2,header
   character(4),dimension(9) :: chst
   integer,save :: i,ii,inti,ipass,j,k,l1,l2,lis,lis0,matlst,mf10mt,mf10st,mfs,mts,n1,nbt,ns
!
!
!
!  FIND NEXT SECTION OF REQUESTED ENDF DATA
!
!...
!...
!...
!...
!...
!...
!...
   data ipass/0/
   data matlst/ - 99999/
!...
   data mf10mt,mf10st/0,0/
   data chst/'G   ','M   ','N   ','4   ','5   ','6   ','7   ','8   ','*   '/
!...
!  ----DURING FIRST PASS SKIP ENDF/B TAPE LABEL.
   if(ipass==0)read(ITApe2,1000,err=100,end=100)header
   ipass = 1
   Iend = 0
   do while (.true.)
!     ----FIND BEGINNING OF NEXT SECTION.
      read(ITApe2,1010)c1,c2,l1,l2,ns,N2,MAT,MF,MT
      if(MT<=0)then
         if(MAT<0)then
!           ----END OF ENDF/B DATA (BASED ON MAT).
            Iend = 1
            return
 1000       format(17A4,a4)
 1020       format(2I11)
 1030       format(6E11.4)
 6000       format(' ERROR READING ENDF/B DATA...EXECUTION TERMINATED')
 6010       format(' ERROR - ENDF DATA NOT LIN.INTERPOLABLE','...EXECUTION TERMINATED')
         endif
      else
!        ----DEFINE EVALUATION MOD. FROM FIRST CARD OF HOLLERITH SECTION.
         if(MAT==matlst)then
         else
            matlst = MAT
            MODiza = 0
            if(MF/=1.or.MT/=451)then
            else
               MODiza = N2
               read(ITApe2,1010)c1,c2,lis,lis0,n1,N2,MAT,MF,MT
               MSTat1 = '    '
               if(lis0>0)MSTat1 = chst(lis0+1)
            endif
         endif
!        ----SEARCH FOR SECTION OF FILE 3 OR 10.
         AWR = c2
         if(MF<3.or.MF>10)then
         else
!
!           CHECK ZA/MF/MT AGAINST REQUESTS.
!
            IZA = c1
            call RQUEST(IGEt,IZA,MF,MT)
            if(IGEt<=0)then
!
!              DATA IS REQUESTED. READ DATA UP TO MXPNT POINTS AT A TIME. IF
!              OVER MXPNT POINTS STORE ALL IN PAGING SYSTEM.
!
!              ----CHECK FOR MF3/4/5/6 FILE DATA
!              ----IF MF4/5/6, LOAD THE DATA AFTER EXFOR
            elseif(MF==3.or.MF==10)then
!              ----PROCESS FILE MF3 AND MF10 DATA
!...
               ns = MAX(ns,1)
               MSTat2 = '    '
               if(MF/=10)exit
               if(MT/=mf10mt)mf10st = 0
               mf10st = mf10st + 1
               if(mf10st>ns)then
               else
                  j = MIN(mf10st,9)
                  MSTat2 = chst(j)
                  exit
               endif
            elseif(MF<4.or.(MF>6.and.MF<12).or.MF>12)then
            else
               if(MF/=4)MF = 6
               if(MF/=4)MT = 9000
               Iend = 0
               do while (.true.)
                  read(ITApe2,1040)MAT,mfs,mts
                  if(mts/=0)then
                  else
                     return
                  endif
               enddo
            endif
         endif
         do while (.true.)
!
!           SKIP SECTION.
!
            read(ITApe2,1040)MAT,MF,MT
            if(MT<=0)exit
         enddo
      endif
   enddo
   do k = 1,ns
!...
      read(ITApe2,1010)c1,c2,l1,l2,n1,N2,MAT,MF,MT
      read(ITApe2,1020)(nbt,inti,i=1,n1)
      if(n1>1.or.inti/=2)goto 102
      if(N2>MXPNT)rewind ISCr
      do i = 1,N2,MXPNT
         ii = i + MXPNT - 1
         if(ii>N2)ii = N2
         read(ITApe2,1030)(XPAge(j-i+1),YPAge(j-i+1),j=i,ii)
         if(N2>MXPNT)write(ISCr)XPAge,YPAge
      enddo
!...
   enddo
!...
   IBAse = 0
   if(N2>MXPNT)then
!     ----LOAD FIRST PAGE FROM SCRATCH AND DEFINE IN CORE PAGE LIMITS.
      endfile ISCr
      rewind ISCr
      read(ISCr)XPAge,YPAge
      ITOp = MXPNT
      return
   else
      ITOp = N2
      return
   endif
!  ----ERROR OR END OF FILE WHILE READING ENDF/B DATA.
  100 continue
   write(OUTp,6000)
   stop
  102 continue
   write(OUTp,6010)
   stop
 1010 format(2E11.4,4I11,i4,i2,i3)
 1040 format(66x,i4,i2,i3)
   end subroutine GETEV
   function X(I)
   implicit none
!*--X1981
!
!
! PARAMETER definitions
!
   integer,parameter :: MXPNT = 90000
!
! COMMON variables
!
   integer :: IBAse,INP,ISCr,ITApe1,ITApe2,ITOp,N2,OUTp
   real,dimension(MXPNT) :: XPAge,YPAge
   common /PAGEXY/ XPAge,YPAge,N2,IBAse,ITOp,ISCr
   common /UNITS / INP,OUTp,ITApe1,ITApe2
!
! Dummy arguments
!
   integer :: I
   real :: X
   intent (in) I
!
! Local variables
!
   integer :: ierr,incore
!
!
!
!  RETRIEVE ENDF/B ENERGY FROM PAGING SYSTEM.
!
   data ierr/0/
!  ----INSURE INDEX IS IN LEGAL RANGE.
   if(I>0.and.I<=N2)then
!     ----SEE IF REQUESTED POINT PRECEEDS POINTS IN CORE.
      if(I<=IBAse)then
!        ----REWIND AND INITIALIZE UPPER PAGE INDEX.
         rewind ISCr
         ITOp = 0
      else
         goto 40
      endif
   else
      write(OUTp,6000)I,N2
 6000 format(' FUNCTION X...I=',i6,' (MUST BE 1 TO',i6,') DEFINED X=0')
      ierr = ierr + 1
      if(ierr<50)return
      stop
   endif
!  ----LOAD NEXT PAGE INTO CORE.
   30 continue
   read(ISCr)XPAge,YPAge
   IBAse = ITOp
   ITOp = ITOp + MXPNT
!  ----SEE IF REQUESTED POINT FOLLOWS POINTS IN CORE.
   40 continue
   if(I<=ITOp)then
!     ----POINT IS IN CORE. DEFINE IT.
      incore = I - IBAse
      X = XPAge(incore)
      return
   else
      goto 30
   endif
   end function X
   function Y(I)
   implicit none
!*--Y2048
!
!
! PARAMETER definitions
!
   integer,parameter :: MXPNT = 90000
!
! COMMON variables
!
   integer :: IBAse,INP,ISCr,ITApe1,ITApe2,ITOp,N2,OUTp
   real,dimension(MXPNT) :: XPAge,YPAge
   common /PAGEXY/ XPAge,YPAge,N2,IBAse,ITOp,ISCr
   common /UNITS / INP,OUTp,ITApe1,ITApe2
!
! Dummy arguments
!
   integer :: I
   real :: Y
   intent (in) I
!
! Local variables
!
   integer :: ierr,incore
!
!
!
!  RETRIEVE ENDF/B CROSS SECTION FROM PAGING SYSTEM.
!
   data ierr/0/
!  ----INSURE INDEX IS IN LEGAL RANGE.
   if(I>0.and.I<=N2)then
!     ----SEE IF REQUESTED POINT PRECEEDS POINTS IN CORE.
      if(I<=IBAse)then
!        ----REWIND AND INITIALIZE UPPER PAGE INDEX.
         rewind ISCr
         ITOp = 0
      else
         goto 40
      endif
   else
      write(OUTp,6000)I,N2
 6000 format(' FUNCTION Y...I=',i6,' (MUST BE 1 TO',i6,') DEFINED X=0')
      ierr = ierr + 1
      if(ierr<50)return
      stop
   endif
!  ----LOAD NEXT PAGE INTO CORE.
   30 continue
   read(ISCr)XPAge,YPAge
   IBAse = ITOp
   ITOp = ITOp + MXPNT
!  ----SEE IF REQUESTED POINT FOLLOWS POINTS IN CORE.
   40 continue
   if(I<=ITOp)then
!     ----POINT IS IN CORE. DEFINE IT.
      incore = I - IBAse
      Y = YPAge(incore)
      return
   else
      goto 30
   endif
   end function Y
   subroutine RQUEST(Kget,Iza,Mf,Mt)
   implicit none
!*--RQUEST2115
!
!
! COMMON variables
!
   real,dimension(100) :: EHGet,ELGet,EPGet
   integer :: IGEt,NGEt
   integer,dimension(100) :: INTrn,IZAhi,IZAlow,MFHi,MFLow,MTHi,MTLow
   common /GETEM / IZAlow,MFLow,MTLow,ELGet,IZAhi,MFHi,MTHi,EHGet,EPGet,INTrn,NGEt,IGEt
!
! Dummy arguments
!
   integer :: Iza,Kget,Mf,Mt
   intent (in) Iza,Mf,Mt
   intent (inout) Kget
!
!
!
!  COMPARE CURRENT ENDF/B ZA/MF/MT TO REQUESTS.
!
   do Kget = 1,NGEt
      if(Iza<IZAlow(Kget).or.Iza>IZAhi(Kget))then
      elseif(Mf<MFLow(Kget).or.Mf>MFHi(Kget))then
      elseif(Mt>=MTLow(Kget).and.Mt<=MTHi(Kget))then
         goto 20
      endif
   enddo
   Kget = 0
   20 continue
   return
   end subroutine RQUEST
   subroutine SCALER
   implicit none
!*--SCALER2151
!
!
! PARAMETER definitions
!
   integer,parameter :: MXPNT = 90000,MXPGP = 10000
!
! COMMON variables
!
   real :: AWR,EINc
   real,dimension(MXPGP) :: DXEx,DYEx,E2,XEX,YEX
   real,dimension(48) :: E2T,EXHigh,EXLow
   real,dimension(100) :: EHGet,ELGet,EPGet
   real,dimension(4) :: FIEld4
   integer :: IBAse,IDOub,IEX,IGEt,IGRoup,IREf,ISCale,ISCr,ITOp,IZA,IZArat,KGRoup,MAT,MAXref,MF,MFIn,MODiza,MPT,MREf,MT,MTRat,N2, &
            & NGEt
   integer,dimension(100) :: INTrn,IZAhi,IZAlow,MFHi,MFLow,MTHi,MTLow
   integer,dimension(2) :: IWAy
   integer,dimension(48) :: LREf
   integer,dimension(MXPGP) :: NREf
   real,dimension(2) :: X4Limx,X4Limy,XLIm,YLIm
   real,dimension(MXPNT) :: XPAge,YPAge
   common /DOUBL / IDOub,FIEld4
   common /EXFOR / XEX,DXEx,YEX,DYEx,NREf,E2,IEX
   common /GETEM / IZAlow,MFLow,MTLow,ELGet,IZAhi,MFHi,MTHi,EHGet,EPGet,INTrn,NGEt,IGEt
   common /PAGEXY/ XPAge,YPAge,N2,IBAse,ITOp,ISCr
   common /RATZA / IZArat,MTRat,MFIn
   common /REFERI/ LREf,EXLow,EXHigh,E2T,IREf,MREf,MAXref,IGRoup,KGRoup
   common /SCALEI/ ISCale
   common /WAYS  / IWAy
   common /WHEREI/ IZA,MAT,MF,MT,MODiza,EINc,AWR
   common /X4LIMS/ X4Limx,X4Limy
   common /XLIMIT/ MPT
   common /XYLIM / XLIm,YLIm
!
! Local variables
!
   real :: ALOG10
   integer :: i,imhi,imhigh,imlow,ipass,jntr,kref
   real :: X,Y
   real :: xexm,xexp,xlast,xmid,xnow,xrange,yexm,yexp,ylast,ymid,ynow,yrange
!
!
!
!  DEFINE X AND Y LIMITS. SELECT LINEAR OR LOG SCALING.
!
!
!  COMPARE CURRENT ZA/MF/MT TO CURRENT REQUEST
!  IF NOT CROSS SECTIONS ALSO COMPARE INCIDENT ENERGY.
!
   MPT = 0
   if(IZA<IZAlow(IGEt).or.IZA>IZAhi(IGEt))then
   elseif(MFIn<MFLow(IGEt).or.MFIn>MFHi(IGEt))then
   elseif(MT<MTLow(IGEt).or.MT>MTHi(IGEt))then
   else
      if(MF==3)goto 20
      if(EINc<ELGet(IGEt).or.EINc>EHGet(IGEt))then
      else
         goto 20
      endif
   endif
!  ----REQUEST IS NOT FOR THIS ZA/MF/MT.
   return
!
!  DEFINE MAXIMUM AND MINIMUM X AND Y VALUES.
!
   20 continue
   ipass = 0
!  ----IF NO ENDF/B DATA OR ONLY SCALING EXFOR DATA SKIP THIS SECTION.
   if(N2<=0.or.ISCale==2)then
   else
!
!     SCALE ENDF/B DATA.
!
      do i = 1,N2
!        ----IF CROSS SECTIONS ONLY SELECT POINTS IN ENERGY RANGE OF NEXT PLOT.
         xnow = X(i)
         ynow = Y(i)
         if(MF/=3)then
         else
!           ----IGNOR ALL POINTS BELOW LOWER ENERGY LIMIT OF PLOT.
            if(xnow<ELGet(IGEt))goto 100
            if(xnow==ELGet(IGEt))then
!              ----LOWER LIMIT OF PLOT REACHED. CHECK UPPER LIMIT.
            elseif(xnow<EHGet(IGEt))then
!              ----POINT IS WITHIN ENERGY RANGE OF PLOT. IF LIMITS ARE ALREADY
!              ----INITIALIZED USE CURRENT POINT TO UPDATE X AND Y LIMITS.
               if(ipass>0)goto 90
!              ----LIMITS ARE NOT YET INITIALIZED. IF THIS IS FIRST POINT USE
!              ----IT TO INITIALIZE X AND Y LIMITS. OTHERWISE INTERPOLATE TO
!              LOWER ----ENERGY LIMIT OF PLOT.
               if(i<=1)then
               else
                  ynow = ((xnow-ELGet(IGEt))*ylast+(ELGet(IGEt)-xlast)*ynow)/(xnow-xlast)
                  xnow = ELGet(IGEt)
               endif
               goto 80
            else
!              ----UPPER ENERGY OF PLOT REACHED. NOTHING TO DO IF ALL POINTS ARE
!              ----ABOVE THE ENERGY RANGE OF THE PLOT.
               if(i<=1)exit
!              ----IF LAST POINT WAS BELOW LOWER LIMIT OF PLOT INTERPOLATE TO
!              LOWER ----LIMIT AND INITIALIZE X AND Y LIMITS.
               if(ipass>0)then
               else
                  ynow = ((xnow-ELGet(IGEt))*ylast+(ELGet(IGEt)-xlast)*ynow)/(xnow-xlast)
                  xnow = ELGet(IGEt)
                  XLIm(1) = xnow
                  XLIm(2) = xnow
                  YLIm(1) = ynow
                  YLIm(2) = ynow
               endif
!              ----INTERPOLATE TO UPPER ENERGY LIMIT (ABOVE TESTS INSURE AT
!              LEAST ----ONE PRECEDING POINT).
               ynow = ((xnow-EHGet(IGEt))*ylast+(EHGet(IGEt)-xlast)*ynow)/(xnow-xlast)
               xnow = EHGet(IGEt)
!              ----SET FLAG TO INDICATE END OF PLOT.
               ipass = 2
               goto 90
            endif
         endif
!        ----INITIALIZE LIMITS ON FIRST ACCEPTABLE POINT.
         if(ipass>0)goto 90
   80    continue
         ipass = 1
         XLIm(1) = xnow
         XLIm(2) = xnow
         YLIm(1) = ynow
         YLIm(2) = ynow
         goto 100
!        ----UPDATE LIMIT.
   90    continue
         if(xnow<XLIm(1))XLIm(1) = xnow
         if(xnow>XLIm(2))XLIm(2) = xnow
         if(ynow<YLIm(1))YLIm(1) = ynow
         if(ynow>YLIm(2))YLIm(2) = ynow
!        ----END OF SCALING IF FLAG INDICATES UPPER ENERGY LIMIT REACHED.
  100    continue
         if(ipass>=2)exit
!        ----SAVE X AND Y VALUES FOR INTERPOLATION.
         xlast = xnow
         ylast = ynow
      enddo
   endif
!
!  DETERMINE WHICH REFERENCES WILL APEEAR ON NEXT PLOT, NUMBER OF
!  POINTS FROM EACH REFERENCE AND ENERGY RANGE OF EACH REFERENCE.
!  IF REQUESTED USE EXFOR DATA FOR SCALING.
!
   MREf = 0
   imlow = 0
   imhi = 0
!  ----SCALE EACH REFERENCE SEPARATELY.
   do kref = 1,IREf
      LREf(kref) = 0
!     ----SELECT POINTS FROM CURRENT REFERENCE.
      do i = 1,IEX
         if(NREf(i)/=kref)then
         else
!           ----IF CROSS SECTIONS ONLY SELECT POINTS IN ENERGY RANGE OF NEXT
!           PLOT. ----ENERGY RANGE OF NEXT PLOT.
            if(MF/=3)then
            elseif(XEX(i)<ELGet(IGEt))then
!              ----COUNT POINTS BELOW ENERGY RANGE OF PLOT.
               imlow = imlow + 1
               cycle
            elseif(XEX(i)>EHGet(IGEt))then
!              ----COUNT POINT ABOVE ENERGY RANGE OF PLOT.
               imhigh = imhigh + 1
               cycle
            endif
!           ----IF ONLY SCALING FOR ENDF/B IGNOR POINTS OUTSIDE THE RANGE OF THE
!           ----PLOT.
            if(ISCale/=1)then
            else
               if(XEX(i)<XLIm(1).or.XEX(i)>XLIm(2))cycle
               if(YEX(i)<YLIm(1).or.YEX(i)>YLIm(2))cycle
            endif
!           ----COUNT TOTAL POINTS, REFERENCES, POINTS FOR CURRENT REFERENCE AND
!           ----SAVE ENERGY RANGE OF CURRENT REFERENCE.
            MPT = MPT + 1
            if(LREf(kref)>0)then
            else
               MREf = MREf + 1
               EXLow(kref) = XEX(i)
               EXHigh(kref) = XEX(i)
            endif
            if(XEX(i)<EXLow(kref))EXLow(kref) = XEX(i)
            if(XEX(i)>EXHigh(kref))EXHigh(kref) = XEX(i)
            LREf(kref) = LREf(kref) + 1
!           ----IF REQUESTED DO NOT SCALE FOR EXFOR DATA.
            if(ISCale==1)then
            else
!
!              SCALE DATA ALLOWING FOR X AND Y UNCERTAINTIES.
!
!              ----DEFINE X LIMITS TRUNCATED TO REQUESTED ENERGY RANGE AND DO
!              NOT ----LET LOWER X LIMIT CROSS ZERO.
               xexm = XEX(i) - DXEx(i)
               xexp = XEX(i) + DXEx(i)
               if(MF/=3)then
               else
                  if(xexm<ELGet(IGEt))xexm = ELGet(IGEt)
                  if(xexp>EHGet(IGEt))xexp = EHGet(IGEt)
                  if(MF==4)then
!                    ----ALLOW COSINE ERROR TO CROSS ZERO.
                  elseif(MF==6.and.IDOub==1)then
                  else
                     if(XEX(i)>0.0.and.xexm<=0.0)xexm = XEX(i)
                  endif
               endif
!              ----DEFINE Y LIMITS TRUNCATED NOT TO LET LOWER Y LIMIT CROSS
!              ZERO.
               yexm = YEX(i) - DYEx(i)
               yexp = YEX(i) + DYEx(i)
               if(YEX(i)>0.0.and.yexm<=0.0)yexm = YEX(i)
               if(ipass>0)then
!                 ----UPDATE X AND Y LIMITS.
                  if(xexm<XLIm(1))XLIm(1) = xexm
                  if(xexp>XLIm(2))XLIm(2) = xexp
                  if(yexm<YLIm(1))YLIm(1) = yexm
                  if(yexp>YLIm(2))YLIm(2) = yexp
               else
!                 ----SAVE LIMITS BASED ON FIRST POINT.
                  ipass = 1
                  XLIm(1) = xexm
                  XLIm(2) = xexp
                  YLIm(1) = yexm
                  YLIm(2) = yexp
               endif
            endif
         endif
      enddo
   enddo
!  ----IF THESE ARE CROSS SECTIONS IF THERE ARE POINTS BELOW OR ABOVE
!  ----THE REQUESTED ENERGY LIMITS SET ENERGY LIMITS TO EXACTLY REQUESTED
!  ----REQUESTED LIMITS.
   if(MF/=3.or.ISCale/=1)then
   else
      if(imlow>0)XLIm(1) = ELGet(IGEt)
      if(imhigh>0)XLIm(2) = EHGet(IGEt)
   endif
!  ----SAVE CURRENT LIMITS TO PREVENT ADDITIONAL EXFOR POINTS BEING
!  ----PLOTTED AFTER ROUNDING PLOT LIMITS OUTWARD.
   X4Limx(1) = XLIm(1)
   X4Limx(2) = XLIm(2)
   X4Limy(1) = YLIm(1)
   X4Limy(2) = YLIm(2)
!
!  DEFINE LINEAR OR LOG SCALING
!
   IWAy(1) = 2
   if(MF==7)XLIm(1) = 0.0
   jntr = INTrn(IGEt)
   if(MF==6.and.IDOub==1)IWAy(1) = 1
   if(MF==4.or.MF==8)IWAy(1) = 1
   if(jntr==0.and.XLIm(1)<=0.0)IWAy(1) = 1
   if(jntr==0.and.XLIm(2)<10.0*XLIm(1))IWAy(1) = 1
   if(jntr==2.or.jntr==4)IWAy(1) = 1
   if(IWAy(1)==2.and.XLIm(1)<=0)XLIm(1) = 1.E-5
   IWAy(2) = 2
   if(jntr==0.and.YLIm(1)<=0.0)IWAy(2) = 1
   if(jntr==0.and.YLIm(2)<10.0*YLIm(1))IWAy(2) = 1
   if(jntr==2.or.jntr==3)IWAy(2) = 1
   if(IWAy(2)==2.and.YLIm(1)<=0)YLIm(1) = 1.E-5*YLIm(2)
!
!  IF REQUIRED, CONVERT LIMITS TO LOG.
!
   if(IWAy(1)==1)then
   else
      XLIm(1) = ALOG10(XLIm(1))
      XLIm(2) = ALOG10(XLIm(2))
   endif
   if(IWAy(2)==1)then
   else
      YLIm(1) = ALOG10(YLIm(1))
      YLIm(2) = ALOG10(YLIm(2))
   endif
!
!  PREVENT ZERO RANGES AND ROUND LIMITS OUTWARD.
!
   if(MF==4)then
   elseif(MF==6.and.IDOub==1)then
   elseif(XLIm(2)>XLIm(1))then
      if(MF==7.or.IWAy(1)==2.or.XLIm(1)>0.0)then
         xmid = 0.5*(XLIm(2)+XLIm(1))
         xrange = 0.5*(XLIm(2)-XLIm(1))
         XLIm(1) = xmid - 1.04*xrange
         XLIm(2) = xmid + 1.04*xrange
      else
         XLIm(2) = 1.02*XLIm(2)
      endif
      goto 280
   elseif(XLIm(1)==0.0)then
   else
      XLIm(1) = 0.5*XLIm(1)
      XLIm(2) = 1.5*XLIm(2)
      goto 280
   endif
   XLIm(1) = -1.04
   XLIm(2) = 1.04
  280 continue
   if(YLIm(2)>YLIm(1))then
      if(IWAy(2)==2.or.YLIm(1)>0.0)then
         ymid = 0.5*(YLIm(2)+YLIm(1))
         yrange = 0.5*(YLIm(2)-YLIm(1))
         YLIm(1) = ymid - 1.04*yrange
         YLIm(2) = ymid + 1.04*yrange
      else
         YLIm(2) = 1.04*YLIm(2)
      endif
   elseif(YLIm(1)==0.0)then
      YLIm(1) = -1.0
      YLIm(2) = 1.0
   else
      YLIm(1) = 0.5*YLIm(1)
      YLIm(2) = 1.5*YLIm(2)
   endif
   return
   end subroutine SCALER
   subroutine UNITED
   implicit none
!*--UNITED2476
!
!
! COMMON variables
!
   real :: AWR,EINc,SIZplt,XBAse,XMUlt,XSTep1,XSTep2,YBAse,YMUlt,YSTep1,YSTep2
   real,dimension(4) :: FIEld4
   integer :: IBMpc,IDOub,IMNorm,IPLotz,ISCale,IXLab,IXStep,IYLab,IYStep,IZA,IZArat,MAT,MF,MFIn,MODiza,MT,MTRat
   character(4) :: IM78
   integer,dimension(2) :: IWAy
   character(1) :: LABcm,STAtus
   real,dimension(2) :: X4Limx,X4Limy,XLIm,XREal,YLIm,YREal
   character(4),dimension(10) :: XLAbel,YLAbel
   common /DOUBL / IDOub,FIEld4
   common /PLTSIZ/ SIZplt,IPLotz,IBMpc
   common /RATZA / IZArat,MTRat,MFIn
   common /SCALEI/ ISCale
   common /SYSSTA/ LABcm,STAtus
   common /UNNORM/ IMNorm
   common /WAYS  / IWAy
   common /WHEREI/ IZA,MAT,MF,MT,MODiza,EINc,AWR
   common /WHO78C/ IM78
   common /X4LIMS/ X4Limx,X4Limy
   common /XYLABC/ XLAbel,YLAbel
   common /XYLABI/ IXLab,IYLab,XMUlt,YMUlt,XBAse,XSTep1,XSTep2,YBAse,YSTep1,YSTep2,IXStep,IYStep
   common /XYLIM / XLIm,YLIm
   common /XYREAL/ XREal,YREal
!
! Local variables
!
   character(4),dimension(4) :: atwt,coefcm,lorder,yield
   character(1) :: cmsys
   character(4),dimension(3) :: coeff,coscm,def78,noknow
   character(4),dimension(2) :: cosine,ratio
   integer :: i,idef78,iptz,j
!
!
!
!  SELECT X AND Y UNITS. DEFINE X AND Y SCALE FACTORS.
!  CONVERT X AND Y LIMITS TO AXIS UNITS.
!
   data cosine/'COSI','NE  '/
   data coscm/'COSI','NE-C','M   '/
   data lorder/'LEGE','NDRE',' ORD','ER'/
   data atwt/'ATOM','IC W','EIGH','T   '/
   data yield/'FISS','ION ','YIEL','D'/
   data cmsys/'C'/
   data coeff/'COEF','FICE','NTS '/
   data coefcm/'COEF','FICE','NTS-','CM  '/
   data ratio/'RATI','O'/
   data noknow/'UNNO','RMAL','IZED'/
!
!  SELECT X AND Y UNITS. DEFINE AXIS LABELS AND MULTIPLIERS.
!
   if(MF==3.or.MF==5)then
   elseif(MF==6.and.IDOub==2)then
   else
!     ----USE X STANDARD UNITS.
      XMUlt = 1.0
!     ----DEFINE ANGULAR DISTRIBUTION X LABEL.
      if(MF/=4.and.MF/=6)then
!        ----USE STANDARD Y UNITS.
         YMUlt = 1.0
!        ----DEFINE LEGENDRE ORDER X AND Y LABELS.
         if(MF/=7)then
!           ----DEFINE FISSION YIELD X AND Y LABELS.
            if(MF/=8)then
            else
!
!              FISSION YIELD.
!
!              ----DEFINE ATOMIC WEIGHT
               IXLab = 13
               do i = 1,4
                  XLAbel(i) = atwt(i)
               enddo
!              ----DEFINE FISSION YIELD.
               IYLab = 13
               do i = 1,4
                  YLAbel(i) = yield(i)
               enddo
!              ----DEFINE X AXIS LABEL INCREMENTS.
               call SPACER(XLIm,XBAse,XSTep1,XSTep2,IXStep,IWAy(1))
!              ----DEFINE Y AXIS LABEL INCREMENTS.
               call SPACER(YLIm,YBAse,YSTep1,YSTep2,IYStep,IWAy(2))
            endif
         else
!
!           LEGENDRE COEFFICIENTS.
!
!           ----DEFINE LEGENDRE ORDER.
            IXLab = 14
            do i = 1,4
               XLAbel(i) = lorder(i)
            enddo
!           ----DEFINE EITHER COEFFICIENTS OR COEFFICIENTS-CM
            if(LABcm==cmsys)then
               IYLab = 14
               do j = 1,4
                  YLAbel(j) = coefcm(j)
               enddo
            else
               IYLab = 11
               do j = 1,3
                  YLAbel(j) = coeff(j)
               enddo
            endif
!           ----ONLY LABEL X AXIS AND DRAW LINES AT LEGENDRE ORDERS.
            IXStep = -1
            XBAse = -1.0
            XSTep1 = 1.0
            XSTep2 = 1.0
!           ----DEFINE Y AXIS LABEL INCREMENTS.
            call SPACER(YLIm,YBAse,YSTep1,YSTep2,IYStep,IWAy(2))
         endif
      else
!
!        ANGULAR DISTRIBUTION (SIMPLE OR DOUBLE DIFFERENTIAL).
!
!        ----DEFINE EITHER COSINE OR COSINE-CM
         if(LABcm==cmsys)then
            IXLab = 9
            XLAbel(1) = coscm(1)
            XLAbel(2) = coscm(2)
            XLAbel(3) = coscm(3)
         else
            IXLab = 6
            XLAbel(1) = cosine(1)
            XLAbel(2) = cosine(2)
         endif
!        ----DEFINE Y UNITS AND MULTIPLIER.
         call CSUNIT(YLIm,YMUlt,YLAbel,IYLab,IWAy(2))
!        ----ADD TO UNITS IF REQUIRED.
         call PERUN(YLAbel,IYLab,MF,IDOub)
!        ----IF RUNNING ON IBM-PC LABEL X AXIS EVERY 0.5 AND DRAW LINE EVERY
!        ----0.25
         IXStep = 1
         if(IBMpc<=0)then
!           ----LABEL X AXIS EVERY 0.2 AND DRAW LINE EVERY 0.1.
            XBAse = -1.2
            XSTep1 = 0.2
            XSTep2 = 0.1
         else
            XBAse = -1.5
            XSTep1 = 0.5
            XSTep2 = 0.25
         endif
!        ----DEFINE Y AXIS LABEL INCREMENTS.
         call SPACER(YLIm,YBAse,YSTep1,YSTep2,IYStep,IWAy(2))
      endif
      goto 200
   endif
!
!  CROSS SECTION VS. ENERGY.
!
!  ----DEFINE ENERGY UNITS AND MULTIPLIER.
   call EUNIT(XLIm(2),XMUlt,XLAbel,IXLab,4,iptz,IWAy(1))
!  ----IF SECONDARY ENERGY PRECEED X AXIS TITLE BY DEFINITION OF FIELD 7.
   if(MF==5)then
   else
      if(MF/=3.and.MF/=6)goto 160
      if(IDOub/=2)goto 160
   endif
!  ----DEFINE FIELD 7.
   call WHAT78(IM78,def78,idef78)
!  ----COMBINE FIELD 7 DEFINITION AND X AXIS LABEL.
   call PAKZAP(def78,idef78,XLAbel,IXLab)
  160 continue
   XLIm(1) = XLIm(1)*XMUlt
   XLIm(2) = XLIm(2)*XMUlt
!  ----FOR RATIO OF RESONANCE PARAMETERS DEFINE Y LABEL AND USE STANDARD
!  ----MULTIPLIER 1.0
   if(MFIn==402.and.MT>=6050)then
!     ----FOR RATIO DEFINE Y LABEL AND USE STANDARD MULTIPLIER 1.0
   elseif(MFIn/=203)then
!     ----DEFINE Y UNITS AND MULTIPLIER.
      call CSUNIT(YLIm,YMUlt,YLAbel,IYLab,IWAy(2))
!     ----ADD TO UNITS IF REQUIRED.
      call PERUN(YLAbel,IYLab,MF,IDOub)
      goto 190
   endif
   IYLab = 5
   YLAbel(1) = ratio(1)
   YLAbel(2) = ratio(2)
   YMUlt = 1.0
!  ----DEFINE X AXIS LABEL INCREMENTS.
  190 continue
   call SPACER(XLIm,XBAse,XSTep1,XSTep2,IXStep,IWAy(1))
!  ----DEFINE Y AXIS LABEL INCREMENTS.
   call SPACER(YLIm,YBAse,YSTep1,YSTep2,IYStep,IWAy(2))
!
!  SAVE LIMITS IN THE PLANE OF THE DATA.
!
  200 continue
   if(IWAy(1)==1)then
      XREal(1) = XLIm(1)/XMUlt
      XREal(2) = XLIm(2)/XMUlt
   else
      XREal(1) = 10.0**XLIm(1)
      XREal(2) = 10.0**XLIm(2)
   endif
   if(IWAy(2)==1)then
      YREal(1) = YLIm(1)/YMUlt
      YREal(2) = YLIm(2)/YMUlt
   else
      YREal(1) = 10.0**YLIm(1)
      YREal(2) = 10.0**YLIm(2)
   endif
!  ----IF USING EXFOR TO SCALE SAVE ROUNDED PLOT LIMITS.
   if(ISCale==1)then
   else
      X4Limx(1) = XREal(1)
      X4Limx(2) = XREal(2)
      X4Limy(1) = YREal(1)
      X4Limy(2) = YREal(2)
   endif
!  ----IF STATUS IS UNNORMALIZED CHANGE Y AXIS LABEL.
   if(IMNorm/=1)then
   else
      do i = 1,3
         YLAbel(i) = noknow(i)
      enddo
      IYLab = 12
   endif
   return
   end subroutine UNITED
   subroutine WHAT78(Im78,Def78,Idef78)
   implicit none
!*--WHAT782707
!
!
! Dummy arguments
!
   integer :: Idef78
   character(4) :: Im78
   character(4),dimension(3) :: Def78
   intent (in) Im78
   intent (out) Def78,Idef78
!
! Local variables
!
   integer :: i,k
   integer,dimension(8) :: itab78
   character(4),dimension(8) :: taba78
   character(4),dimension(3,8) :: tabb78
!
!
!
!  DEFINE FIELDS 7-8.
!
   data taba78/' E2','LVL','EXC',' HL','DE2','DLV','MIN','MAX'/
   data itab78/2,5,10,9,2,5,6,6/
   data tabb78/'E2  ','    ','    ','LEVE','L   ','    ','EXCI','TATI','ON  ','HALF','-LIF','E   ','E2  ','    ','    ','LEVE',   &
       &'L   ','    ','E2-M','IN  ','    ','E2-M','AX  ','    '/
   do i = 1,8
      if(Im78==taba78(i))goto 20
   enddo
   i = 1
   20 continue
   Idef78 = itab78(i)
   do k = 1,3
      Def78(k) = tabb78(k,i)
   enddo
   return
   end subroutine WHAT78
   subroutine SPACER(Zlim,Zbase,Zstep1,Zstep2,Izstep,Izway)
   implicit none
!*--SPACER2749
!
!
! COMMON variables
!
   integer :: IBMpc,IPLotz
   real :: SIZplt
   common /PLTSIZ/ SIZplt,IPLotz,IBMpc
!
! Dummy arguments
!
   integer :: Izstep,Izway
   real :: Zbase,Zstep1,Zstep2
   real,dimension(2) :: Zlim
   intent (in) Izway,Zlim
   intent (out) Izstep,Zbase,Zstep2
   intent (inout) Zstep1
!
! Local variables
!
   real :: dzlim,twice
   real :: FLOAT
   integer :: i,ineed,izbase,j,k
   integer,dimension(13) :: isteps
   real,dimension(13) :: steps
!
!
!
!  DEFINE LINEAR X OR Y AXIS LABEL INCREMENTS.
!  USE FEWER AXIS INCREMENTS IF RUNNING ON IBM-PC.
!
   data steps/100.,50.,20.,10.,5.,2.,1.,.5,.2,.1,.05,.02,.01/
   data isteps/ - 1, - 1, - 1, - 1, - 1, - 1, - 1,1,1,1,2,2,2/
   if(Izway==2)return
   dzlim = Zlim(2) - Zlim(1)
   ineed = 10
   if(Zlim(2)>=100.0)ineed = 5
!  ----FEWER GRID LINES IF HALF SIZE PLOTS ON IBM-PC.
   if(IBMpc>0)ineed = 5
   if(IBMpc>0.and.Zlim(2)>=100.0)ineed = 4
   do i = 1,13
      if(i==7)ineed = 5
      if(i==7.and.IBMpc>0)ineed = 4
      if(i==13)goto 20
      j = dzlim/steps(i)
      k = dzlim/steps(i+1)
      if(k>=2*ineed)goto 20
      if(j>=ineed)goto 20
   enddo
   i = 13
   20 continue
   Zstep1 = steps(i)
   izbase = Zlim(1)/Zstep1
   Zbase = FLOAT(izbase)*Zstep1
   Izstep = isteps(i)
   twice = 1.0
   if(j<10)twice = 2.0
   Zstep2 = Zstep1/twice
   return
   end subroutine SPACER
   subroutine BORDER
   implicit none
!*--BORDER2814
!
!
! COMMON variables
!
   real :: ADVanc,AWR,BOT1,BOT2,BOT3,BOX,BOX2,BOX4,BOXwt2,EINc,HT,HT2,HT34,HTH,RIGht1,RIGht2,RIGht3,RIGht4,RIGht5,RIGht6,RIGht7,  &
         & RIGht8,SIZplt,TOP1,TOP2,TOP3,WT,WT38,WTH,XBAse,XMUlt,XSTep1,XSTep2,YBAse,YMUlt,YSTep1,YSTep2
   real,dimension(48) :: E2T,EXHigh,EXLow
   real,dimension(4) :: FIEld4
   integer :: IBMpc,IDOub,IGRoup,ILIb,IMAm78,IPLotz,IREf,IXLab,IXStep,IYLab,IYStep,IZA,IZAbcd,IZArat,KGRoup,MAT,MAXref,MF,MFIn,   &
            & MODiza,MREf,MT,MTRat,MYMode,NUMplt
   character(4) :: IM78,MSTat1,MSTat2
   integer,dimension(2) :: IWAy
   character(1) :: LABcm,MSTar1,MSTar2,STAtus
   character(4),dimension(4) :: LIBnam,ZABcd
   integer,dimension(48) :: LREf
   character(4),dimension(9) :: REF1
   character(4),dimension(9,48) :: REFs
   real,dimension(10) :: TABlog
   real,dimension(2) :: XINch,XLIm,YINch,YLIm
   character(4),dimension(10) :: XLAbel,YLAbel
   common /DOUBL / IDOub,FIEld4
   common /INCHES/ XINch,YINch
   common /LIBC  / LIBnam
   common /LIBI  / ILIb
   common /LOGTAB/ TABlog
   common /MODEMY/ MYMode
   common /PAINT / BOX,BOX2,BOX4,BOXwt2,HT,HT2,HTH,HT34,WT,WTH,WT38
   common /PLOTN / NUMplt,ADVanc
   common /PLTSIZ/ SIZplt,IPLotz,IBMpc
   common /RATZA / IZArat,MTRat,MFIn
   common /RATZAC/ MSTar1,MSTar2
   common /REFERC/ REFs,REF1
   common /REFERI/ LREf,EXLow,EXHigh,E2T,IREf,MREf,MAXref,IGRoup,KGRoup
   common /SPOTS / TOP1,TOP2,TOP3,BOT1,BOT2,BOT3,RIGht1,RIGht2,RIGht3,RIGht4,RIGht5,RIGht6,RIGht7,RIGht8
   common /SYSSTA/ LABcm,STAtus
   common /WAYS  / IWAy
   common /WHERE2/ IZAbcd
   common /WHEREC/ ZABcd,MSTat1,MSTat2
   common /WHEREI/ IZA,MAT,MF,MT,MODiza,EINc,AWR
   common /WHO78C/ IM78
   common /WHO78I/ IMAm78
   common /XYLABC/ XLAbel,YLAbel
   common /XYLABI/ IXLab,IYLab,XMUlt,YMUlt,XBAse,XSTep1,XSTep2,YBAse,YSTep1,YSTep2,IXStep,IYStep
   common /XYLIM / XLIm,YLIm
!
! Local variables
!
   character(4) :: blank,datao,en1,en2,hl
   real :: botx3,botx4,deg,enout,xdec,xi,xi1,ximax,ximin,xmat,xmid,xr,xz,xz1,ydec,yi,yi3,ymid,yr,yr1,yr2,yr3,ytop,yz,zmult
   character(4),dimension(2) :: cos1,ratio,zunit
   character(4),dimension(4) :: cosa
   character(4),dimension(3) :: def78,undefi
   character(1),dimension(9) :: digits
   real :: FLOAT
   integer :: i,idef78,ilog1,imfbcd,imtbcd,iptz,irr,ixmax,ixmin,iymax,iymin,izunit,j,jmfbcd,jmtbcd,jxlab,jylab,kd,klog1,kmfbcd,   &
            & kmtbcd,kn,krr,kt,kxlab,kylab,kzap
   character(4),dimension(8) :: mfbcd
   character(4),dimension(20) :: mtbcd,zapbcd
   character(1),dimension(7) :: stat1
   character(4),dimension(3,7) :: statab
!
!
!
!  PLOT BORDER AND ALL AXIS LABELS.
!
   data hl/' HL'/
   data blank/'    '/
   data cos1/'DEGR','=   '/
   data cosa/'ANGL','E IN','TEGR','ATED'/
   data en2/'E2 ='/
   data en1/'E ='/
   data stat1/'P','S','D','C','A','O','R'/
   data statab/' PRE','LIMI','NARY','  SU','PERC','EDED','   D','EPEN','DENT','  CO','RREL','ATED','    ','APPR','OVED','    ',   &
       &'OUTD','ATED','RENO','RMAL','IZED'/
   data datao/'DATA'/
   data undefi/'UNDE','FINE','D'/
   data ratio/'RATI','O'/
   data digits/'1','2','3','4','5','6','7','8','9'/
!  ----SELECT BLACK PEN FOR ALL BORDER INFORMATON.
   call PEN(2)
!  ----INCREMENT PLOT COUNT.
   NUMplt = NUMplt + 1
!  ----DEFINE TRUE CENTER OF PLOT.
   xmid = 0.5*(XINch(1)+XINch(2))
   ymid = 0.5*(YINch(1)+YINch(2))
!  ----DEFINE LOCATION OF X AXIS LABEL (DIFFERENT FOR LINEAR OR LOG).
   botx3 = BOT3
   if(IWAy(1)==1)botx3 = botx3 + HT
   botx4 = botx3 - 1.75*HT
!
!  IF NOT IDENTIFYING E2 IN OUTSIDE LEGEND BOX, IDENTIFY E2 AT TOP
!  OF PLOT (IF CROSS SECTIONS, ANGULAR DISTRIBUTIONS OR LEGENDRE
!  COEFFICIENTS).
!
   if(KGRoup>0)then
   else
      if(MF/=3.and.MF/=4.and.MF/=7)goto 40
!     ----PRINT INCIDENT OR SECONDARY ENERGY IN NORMAL FORM.
      xz = XINch(1)
      if(MF==3.and.IDOub==2)then
!        ----FOR CROSS SECTIONS IDENTIFY E.
         call SYMBLH(xz,botx4,HT,en1,0.0,4)
         xz = xz + 4.0*WT
      else
!        ----IDENTIFY E2 (UNLESS E2 FIELD IS BLANK).
         if(IMAm78<=0)goto 40
         call WHAT78(IM78,def78,idef78)
         call SYMBLH(xz,botx4,HT,def78,0.0,idef78)
         xz = xz + FLOAT(idef78+1)*WT
         call SYMBLH(xz,botx4,HT,'=',0.0,1)
         xz = xz + 2.0*WT
         if(IM78/=hl)then
         else
!           ----SELECT HALF-LIFE UNITS.
            call HLUNIT(E2T(1),zmult,zunit,izunit,4,iptz)
            goto 30
         endif
      endif
!     ----SELECT ENERGY UNITS.
      call EUNIT(E2T(1),zmult,zunit,izunit,4,iptz,1)
!     ----PRINT ENERGY OR HALF-LIFE IN NORMALIZED FORM.
   30 continue
      enout = E2T(1)*zmult
      call NUMBRH(xz,botx4,HT,enout,0.0,iptz)
      xz = xz + 7.0*WT
      call SYMBLH(xz,botx4,HT,zunit,0.0,izunit)
   endif
!
!  SPECIAL TITLE FOR RATIO.
!
   40 continue
   if(MFIn/=203)then
!     ----IDENTIFY MATERIAL.
      xz = XINch(1)
      call SYMBLH(xz,TOP1,HT,ZABcd,0.0,IZAbcd)
!     ----IF DOUBLE DIFFERENTAIL PRINT SECONDARY ENERGY OR COSINE.
      if(MF/=6)then
      else
         xz = XINch(1)
         if(IDOub==2)then
!           ----PRINT COSINE.
            if(MFIn==5)then
               call SYMBLH(xz,botx4,HT,cosa,0.0,15)
            else
               call SYMBLH(xz,botx4,HT,cos1,0.0,5)
               xz = xz + 6.0*WT
               deg = 180.*ACOS(FIEld4(1))/3.14159654
               call NUMBRH(xz,botx4,HT,deg,0.0,0)
            endif
         else
!           ----PRINT SECONDARY ENERGY IN NORMAL FORM.
            call SYMBLH(xz,botx4,HT,en2,0.0,4)
            xz = xz + 5.0*WT
            call EUNIT(FIEld4(3),zmult,zunit,izunit,4,iptz,1)
            enout = FIEld4(3)*zmult
            call NUMBRH(xz,botx4,HT,enout,0.0,iptz)
            xz = xz + 7.0*WT
            call SYMBLH(xz,botx4,HT,zunit,0.0,izunit)
         endif
      endif
!
!     IDENTIFY DATA TYPE AND REACTION.
!
      call MTHOL(MT,mtbcd,imtbcd,MSTat2)
      call MFHOL(MFIn,MT,mfbcd,imfbcd)
!     ----IDENTIFY REACTION.
      jmtbcd = imtbcd
      call LEFTY(mtbcd,imtbcd,kmtbcd,jmtbcd)
      xi = xmid - WTH*FLOAT(kmtbcd)
      call SYMBLH(xi,TOP2,HT,mtbcd,0.0,imtbcd)
!     ----IDENTIFY DATA TYPE.
      if(MF/=3)then
!        ----IF NOT CROSS SECTION PRECEED DATA TYPE BY ENERGY.
         call EUNIT(EINc,zmult,zunit,izunit,4,iptz,1)
         jmfbcd = imfbcd
         call LEFTY(mfbcd,imfbcd,kmfbcd,jmfbcd)
         xi1 = xmid - WTH*FLOAT(8+izunit+kmfbcd)
         enout = EINc*zmult
         call NUMBRH(xi1,TOP3,HT,enout,0.0,iptz)
         xi1 = xi1 + 7.0*WT
         call SYMBLH(xi1,TOP3,HT,zunit,0.0,izunit)
         xi1 = xi1 + WT*FLOAT(1+izunit)
         call SYMBLH(xi1,TOP3,HT,mfbcd,0.0,imfbcd)
      else
         jmfbcd = imfbcd
         call LEFTY(mfbcd,imfbcd,kmfbcd,jmfbcd)
         xi1 = xmid - WTH*FLOAT(kmfbcd)
         call SYMBLH(xi1,TOP3,HT,mfbcd,0.0,imfbcd)
      endif
   else
!     ----IDENTIFY AS RATIO.
      call SYMBLH(XINch(1),TOP1,HT,ratio,0.0,5)
!     ----DEFINE NUMERATOR ZA AND MT. PACK TOGETHER AND PLOT.
      call MTHOL(MT,mtbcd,imtbcd,MSTat2)
      call ZAHOL(IZA,MSTat1,zapbcd,kzap)
      call PAKZAP(zapbcd,kzap,mtbcd,imtbcd)
      jmtbcd = imtbcd
      call LEFTY(mtbcd,imtbcd,kmtbcd,jmtbcd)
      xi1 = xmid - WTH*FLOAT(kmtbcd)
      ximin = xi1
      call SYMBLH(xi1,TOP2,HT,mtbcd,0.0,imtbcd)
!     ----DEFINE DENOMINATOR ZA AND MT. PACK TOGETHER AND PLOT.
      if(IZArat<=0.or.MTRat<=0)then
!        ----DENOMINATOR ZA AND/OR MT IF NOT DEFINED.
         xi = xmid - WTH*9.0
         if(xi<ximin)ximin = xi
         call SYMBLH(xi,TOP3,HT,undefi,0.0,9)
      else
         call MTHOL(MTRat,mtbcd,imtbcd,MSTar2)
         call ZAHOL(IZArat,MSTar1,zapbcd,kzap)
         call PAKZAP(zapbcd,kzap,mtbcd,imtbcd)
         jmtbcd = imtbcd
         call LEFTY(mtbcd,imtbcd,kmtbcd,jmtbcd)
         xi = xmid - WTH*FLOAT(kmtbcd)
         if(xi<ximin)ximin = xi
         call SYMBLH(xi,TOP3,HT,mtbcd,0.0,imtbcd)
      endif
!     ----PLOT LINE BETWEEN NUMERATOR AND DENOMINATOR.
      ximin = ximin - WTH
      ximax = xmid + (xmid-ximin)
      yi3 = TOP2 - 0.375*HT
      call PLOT(ximin,yi3,3)
      call PLOT(ximax,yi3,2)
   endif
!  ----IDENTIFY ENDF/B OR EXFOR DATA.
   if(MYMode<2)then
!     ----IF ONLY ONE REFERENCE PRINT EXFOR ACCESSION/SUB-ACCESSION NUMBER.
      if(MREf>1.and.KGRoup==0)then
         xz = XINch(2) - 10.0*WT
         call SYMBLH(xz,TOP1,HT,'EXFOR DATA',0.0,10)
      else
         xz = XINch(2) - WT*FLOAT(ILIb)
         call SYMBLH(xz,TOP1,HT,LIBnam,0.0,ILIb)
!        ----PRINT STATUS.
         do i = 1,7
            if(STAtus==stat1(i))goto 140
         enddo
      endif
   else
      xz = XINch(2) - FLOAT(11+ILIb)*WT
      call SYMBLH(xz,TOP1,HT,LIBnam,0.0,ILIb)
      xz = xz + FLOAT(1+ILIb)*WT
      xmat = MAT
      call NUMBRH(xz,TOP1,HT,xmat,0.0,-1)
      xz = xz + 5.0*WT
      call SYMBLH(xz,TOP1,HT,'MOD',0.0,3)
      xz = xz + 4.0*WT
      xmat = MODiza
      call NUMBRH(xz,TOP1,HT,xmat,0.0,-1)
   endif
   goto 160
  140 continue
   xz1 = XINch(2) - 12.0*WT
   call SYMBLH(xz1,botx4,HT,statab(1,i),0.0,12)
!
!  PLOT BORDER FOR FIGURE
!
  160 continue
   call PLOTP(XLIm(2),YLIm(1),3)
   call PLOTP(XLIm(1),YLIm(1),2)
   call PLOTP(XLIm(1),YLIm(2),2)
   call PLOTP(XLIm(2),YLIm(2),2)
   call PLOTP(XLIm(2),YLIm(1),2)
!
!  PLOT X AXIS LABEL AND UNITS.
!
   jxlab = IXLab
   call LEFTY(XLAbel,IXLab,kxlab,jxlab)
   xi = xmid - WTH*FLOAT(kxlab)
   call SYMBLH(xi,botx3,HT,XLAbel,0.0,IXLab)
   if(IWAy(1)==1)then
!
!     PLOT X SCALE LINEAR UNITS.
!
      xr = XBAse
   else
!
!     PLOT X SCALE LOG10 DECADES.
!
      ixmin = XLIm(1)
      if(XLIm(1)<0.0)ixmin = ixmin - 1
      ixmax = XLIm(2) + 1.0
      xdec = XLIm(2) - XLIm(1)
      if(xdec<=10.0)then
         if(xdec<=6.0)then
            if(xdec<=3.0)then
               ilog1 = 2
               klog1 = 1
            else
               ilog1 = 2
               klog1 = 2
            endif
         else
            ilog1 = 5
            klog1 = 5
         endif
      else
         ilog1 = 10
         klog1 = 10
      endif
      do i = ixmin,ixmax
         xr = i
         if(xr<XLIm(1).or.xr>XLIm(2))then
         else
            xi = ((xr-XLIm(1))*XINch(2)+(XLIm(2)-xr)*XINch(1))/(XLIm(2)-XLIm(1))
            call SYMBLH(xi-WT,BOT2,HT,'10',0.0,2)
            call NUMBRH(xi+WT,BOT1,HT,xr,0.0,-1)
         endif
         if(ilog1>9)then
         else
            do j = ilog1,9,klog1
               xz = xr + TABlog(j)
               if(xz<XLIm(1).or.xz>XLIm(2))then
               else
                  xi = ((xz-XLIm(1))*XINch(2)+(XLIm(2)-xz)*XINch(1))/(XLIm(2)-XLIm(1))
                  call SYMBLH(xi-WT38,BOT1,HT34,digits(j),0.0,1)
               endif
            enddo
         endif
      enddo
      goto 270
   endif
  250 continue
   if(xr<XLIm(1))then
   else
      if(xr>XLIm(2))goto 270
      xi = ((xr-XLIm(1))*XINch(2)+(XLIm(2)-xr)*XINch(1))/(XLIm(2)-XLIm(1))
      xi = xi - WTH
      if(MF==4.and.ABS(xr)<0.01)xr = 0.0
      if(MF==6.and.IDOub==1.and.ABS(xr)<0.01)xr = 0.0
      if(xr<0.0)xi = xi - WTH
      if(xr>=10.0)xi = xi - WTH
      if(xr>=100.0)xi = xi - WTH
      if(IXStep==1)xi = xi - WT
      if(IXStep==2)xi = xi - WTH
      if(IXStep==3)xi = xi - WTH
      call NUMBRH(xi,BOT1,HT,xr,0.0,IXStep)
   endif
   xr = xr + XSTep1
   goto 250
!
!  PLOT Y AXIS LABEL AND UNITS.
!
  270 continue
   jylab = IYLab
   call LEFTY(YLAbel,IYLab,kylab,jylab)
   yi = ymid - WTH*FLOAT(IYLab)
   call SYMBLH(RIGht2,yi,HT,YLAbel,90.0,IYLab)
   if(IWAy(2)==1)then
!
!     PLOT Y SCALE LINEAR UNITS.
!
      yr = YBAse
      ytop = yr
      do while (yr<=YLIm(2))
         ytop = yr
         yr = yr + YSTep1
      enddo
      kt = 1
      if(ytop>=10.0)kt = 2
      if(ytop>=100.0)kt = 3
      yr = YBAse
   else
!
!     PLOT Y SCALE LOG10 DECADES.
!
      iymin = YLIm(1)
      if(YLIm(1)<0.0)iymin = iymin - 1
      iymax = YLIm(2) + 1.0
      ydec = YLIm(2) - YLIm(1)
      if(ydec<=10.0)then
         if(ydec<=6.0)then
            if(ydec<=3.0)then
               ilog1 = 2
               klog1 = 1
            else
               ilog1 = 2
               klog1 = 2
            endif
         else
            ilog1 = 5
            klog1 = 5
         endif
      else
         ilog1 = 10
         klog1 = 10
      endif
      do i = iymin,iymax
         yr = i
         if(yr<YLIm(1).or.yr>YLIm(2))then
         else
            yi = ((yr-YLIm(1))*YINch(2)+(YLIm(2)-yr)*YINch(1))/(YLIm(2)-YLIm(1))
            call SYMBLH(RIGht1,yi,HT,'10',0.0,2)
            call NUMBRH(RIGht1+2.0*WT,yi+HT,HT,yr,0.0,-1)
         endif
         if(ilog1>9)then
         else
            do j = ilog1,9,klog1
               yz = yr + TABlog(j)
               if(yz<YLIm(1).or.yz>YLIm(2))then
               else
                  yi = ((yz-YLIm(1))*YINch(2)+(YLIm(2)-yz)*YINch(1))/(YLIm(2)-YLIm(1))
                  call SYMBLH(RIGht1,yi,HT34,digits(j),0.0,1)
               endif
            enddo
         endif
      enddo
      goto 400
   endif
  380 continue
   if(yr<YLIm(1))then
   else
      if(yr>YLIm(2))goto 400
      yi = ((yr-YLIm(1))*YINch(2)+(YLIm(2)-yr)*YINch(1))/(YLIm(2)-YLIm(1))
      kn = 1
      if(yr>=10.0)kn = 2
      if(yr>=100.0)kn = 3
      kd = kt - kn + 1
      xi = XINch(2) + kd*WT
      call NUMBRH(xi,yi+HTH,HT,yr,0.0,IYStep)
   endif
   yr = yr + YSTep1
   goto 380
!
!  PLOT BORDERS FOR REFERENCES
!
  400 continue
   yr1 = YINch(2)
   yr2 = YINch(2) - 1.75*(MREf+2)*HT
   if(KGRoup/=0)yr2 = yr2 - 1.75*HT
   call SYMBLH(RIGht5,yr1-1.75*HT,HT,'REFERENCE',0.0,9)
   if(KGRoup==0)then
   else
      call LEFTY(REFs(1,1),irr,krr,25)
      call SYMBLH(RIGht5,yr1-3.5*HT,HT,REFs(1,1),0.0,irr)
   endif
   call PLOT(RIGht3,yr1,3)
   call PLOT(RIGht4,yr1,2)
   call PLOT(RIGht4,yr2,2)
   call PLOT(RIGht3,yr2,2)
   call PLOT(RIGht3,yr1,2)
!  ----SUPPRESS RANGE BOX PLOT IF TOO MANY REFERENCES
   if(YINch(2)-YINch(1)<1.75*(MREf+2)*HT*2)return
   yr1 = YINch(1)
   yr2 = YINch(1) + 1.75*(MREf+2)*HT
   yr3 = yr2 - 1.75*HT
   if(MF==7.or.MF==8)then
      call SYMBLH(RIGht5,yr3,HT,'RANGE',0.0,5)
   else
      if(MF==4)then
      elseif(MF==6.and.IDOub==1)then
      else
         call SYMBLH(RIGht5,yr3,HT,'ENERGY RANGE',0.0,12)
         goto 440
      endif
      call SYMBLH(RIGht5,yr3,HT,'COSINE RANGE',0.0,12)
   endif
  440 continue
   call SYMBLH(RIGht6,yr3,HT,'POINTS',0.0,6)
   call PLOT(RIGht3,yr1,3)
   call PLOT(RIGht4,yr1,2)
   call PLOT(RIGht4,yr2,2)
   call PLOT(RIGht3,yr2,2)
   call PLOT(RIGht3,yr1,2)
   return
   end subroutine BORDER
   subroutine EUNIT(Z,Zmult,Zunit,Izunit,Lptz,Iptz,Izway)
   implicit none
!*--EUNIT3286
!
!
! Dummy arguments
!
   integer :: Iptz,Izunit,Izway,Lptz
   real :: Z,Zmult
   character(4),dimension(2) :: Zunit
   intent (in) Izway,Lptz,Z
   intent (out) Izunit,Zunit
   intent (inout) Iptz,Zmult
!
! Local variables
!
   integer :: i
   integer,dimension(6) :: iunit
   real,dimension(6) :: ranger
   character(4),dimension(2,6) :: untab
   real :: zx
!
!
!
!  DEFINE MULTIPLIER AND UNITS TO PUT ENERGY IN NORMAL FORM AND
!  NUMBER OF DECIMAL PLACES TO OUTPUT.
!
!  ----DEFINE MULTIPLIERS, LENGTH OF UNIT TITLE AND UNIT TITLES.
   data ranger/1.0E+9,1.0E+6,1.0E+3,1.0,1.0E-3,1.0E-6/
   data iunit/3,3,3,2,8,8/
   data untab/'GEV ','    ','MEV ','    ','KEV ','    ','EV  ','    ','MILL','I-EV','MICR','O-EV'/
!  ----USE EV FOR LOG SCALING OR ZERO VALUES.
   if(Izway==1.and.ABS(Z)/=0.0)then
!     ----SELECT MULTIPLIER.
      do i = 1,6
         if(Z>=ranger(i))goto 30
      enddo
      i = 6
   else
      i = 4
   endif
!  ----DEFINE UNITS AND MULTIPLIER.
   30 continue
   Zunit(1) = untab(1,i)
   Zunit(2) = untab(2,i)
   Izunit = iunit(i)
   Zmult = 1.0/ranger(i)
!  ----DEFINE NUMBER OF DECIMAL PLACES TO OUTPUT.
   zx = Z*Zmult
   Iptz = Lptz
   if(zx>=9.9999)Iptz = Iptz - 1
   if(zx>=99.999)Iptz = Iptz - 1
   if(zx>=999.99)Iptz = Iptz - 1
   if(Iptz<-1)Iptz = -1
   return
   end subroutine EUNIT
   subroutine HLUNIT(Hl,Hlmult,Hlout,Ihlout,Lptz,Iptz)
   implicit none
!*--HLUNIT3345
!
!
! Dummy arguments
!
   real :: Hl,Hlmult
   integer :: Ihlout,Iptz,Lptz
   character(4),dimension(2) :: Hlout
   intent (in) Lptz
   intent (out) Hlout,Ihlout
   intent (inout) Hl,Hlmult,Iptz
!
! Local variables
!
   character(4),dimension(2,9) :: hltab
   real,dimension(9) :: hltime
   integer :: i,j
   integer,dimension(9) :: ihltab
   real :: zhl,zmax
!
!
!
!  DEFINE CONVERSION FACTORS FROM SECONDS TO VARIABLE UNITS.
!
   data ihltab/4,4,5,4,3,3,4,3,4/
   data hltab/'PSEC','    ','NSEC','    ','MUSE','C   ','MSEC','    ','SEC ','    ','MIN ','    ','HOUR','    ','DAY ','    ',    &
       &'YEAR','    '/
   data hltime/1.00000E-12,1.00000E-9,1.00000E-6,1.00000E-3,1.00000E+0,6.00000E+1,3.60000E+2,8.64000E+3,3.15576E+6/
   Hl = ABS(Hl)
!  ----IF HALF-LIFE IS ZERO USE SECONDS.
   if(Hl>0.0)then
!     ----SELECT APPROPRIATE UNITS (WITH NON-ZERO LEADING DIGIT).
      i = 9
      do j = 1,9
         if(Hl>hltime(i))goto 30
         i = i - 1
      enddo
      i = 1
   else
      i = 5
   endif
!  ----DEFINE UNITS AND MULTIPLIER.
   30 continue
   Hlout(1) = hltab(1,i)
   Hlout(2) = hltab(2,i)
   Ihlout = ihltab(i)
   Hlmult = 1.0/hltime(i)
!  ----DEFINE NUMBER OF DIGITS AFTER DECIMAL POINT.
   zhl = Hl*Hlmult
   Iptz = Lptz
   zmax = 9.9999
   do while (Iptz>=0)
      if(zhl<zmax)exit
      Iptz = Iptz - 1
      zmax = 10.0*zmax
   enddo
   return
   end subroutine HLUNIT
   subroutine CSUNIT(Ylim,Ymult,Ylabel,Iylab,Iyway)
   implicit none
!*--CSUNIT3408
!
!
! Dummy arguments
!
   integer :: Iylab,Iyway
   real :: Ymult
   character(4),dimension(10) :: Ylabel
   real,dimension(2) :: Ylim
   intent (in) Iyway
   intent (out) Iylab,Ylabel
   intent (inout) Ylim,Ymult
!
! Local variables
!
   integer :: i,j
   integer,dimension(6) :: iyunit
   real,dimension(6) :: ranger
   character(4),dimension(3,6) :: yunit
!
!
!
!  SELECT UNITS Y. DEFINE Y AXIS LABEL AND SCALE FACTOR.
!
   data ranger/1.0E+9,1.0E+6,1.0E+3,1.0,1.0E-3,1.0E-6/
   data iyunit/10,10,10,5,11,11/
   data yunit/'GEGA','-BAR','NS  ','MEGA','-BAR','NS  ','KILO','-BAR','NS  ','BARN','S   ','    ','MILL','I-BA','RNS ','MICR',    &
       &'O-BA','RNS '/
!  ----USE BARNS FOR LOG SCALING.
   if(Iyway==1)then
!     ----LINEAR SCALING...USE VARIABLE UNITS.
      do i = 1,6
         if(Ylim(2)>=ranger(i))goto 30
      enddo
      i = 6
   else
      i = 4
   endif
   30 continue
   Ymult = 1.0/ranger(i)
   Ylim(1) = Ylim(1)*Ymult
   Ylim(2) = Ylim(2)*Ymult
   Iylab = iyunit(i)
   do j = 1,3
      Ylabel(j) = yunit(j,i)
   enddo
   return
   end subroutine CSUNIT
   subroutine PERUN(Ylabel,Iylab,Mf,Idoub)
   implicit none
!*--PERUN3461
!
!
! Dummy arguments
!
   integer :: Idoub,Iylab,Mf
   character(1),dimension(40) :: Ylabel
   intent (in) Mf
   intent (out) Ylabel
   intent (inout) Iylab
!
! Local variables
!
   integer :: i
   character(1),dimension(13) :: perest
   character(1),dimension(3) :: perev
   character(1),dimension(10) :: perst
!
!
!
!  IF NOT MF=3 ADD APPROPRIATE UNITS TO Y AXIS UNITS.
!
   data perev/'/','E','V'/
   data perst/'/','S','T','E','R','A','D','I','A','N'/
   data perest/'/','E','V','/','S','T','E','R','A','D','I','A','N'/
   if(Mf==4)then
!     ----ADD /STERADIAN.
      do i = 1,10
         Iylab = Iylab + 1
         Ylabel(Iylab) = perst(i)
      enddo
      return
   elseif(Mf==5)then
!     ----ADD /EV.
      do i = 1,3
         Iylab = Iylab + 1
         Ylabel(Iylab) = perev(i)
      enddo
      return
   elseif(Mf==6)then
!     ----ADD /EV/STERADIAN.
      do i = 1,13
         Iylab = Iylab + 1
         Ylabel(Iylab) = perest(i)
      enddo
      return
   else
      return
   endif
   end subroutine PERUN
   subroutine GRID0
   implicit none
!*--GRID03516
!
!
! COMMON variables
!
   real :: BOX,BOX2,BOX4,BOXwt2,HT,HT2,HT34,HTH,WT,WT38,WTH,XBAse,XMUlt,XSTep1,XSTep2,YBAse,YMUlt,YSTep1,YSTep2
   integer,dimension(2) :: IWAy
   integer :: IXLab,IXStep,IYLab,IYStep
   real,dimension(10) :: TABlog
   real,dimension(2) :: XINch,XLIm,YINch,YLIm
   common /INCHES/ XINch,YINch
   common /LOGTAB/ TABlog
   common /PAINT / BOX,BOX2,BOX4,BOXwt2,HT,HT2,HTH,HT34,WT,WTH,WT38
   common /WAYS  / IWAy
   common /XYLABI/ IXLab,IYLab,XMUlt,YMUlt,XBAse,XSTep1,XSTep2,YBAse,YSTep1,YSTep2,IXStep,IYStep
   common /XYLIM / XLIm,YLIm
!
! Local variables
!
   real :: box2p,xdec,xr,xr1,xr2,xz,ydec,yr,yr1,yr2,yz
   integer :: i,ilog1,ixmax,ixmin,iymax,iymin,j,klog1,loop
!
!
!
!  PLOT X AND Y AXIS TICK MARKS.
!
   call PEN(2)
   yr1 = YLIm(1)
   box2p = BOX2*(YLIm(2)-YLIm(1))/(YINch(2)-YINch(1))
   yr2 = yr1 + box2p
   if(IWAy(1)==1)then
!
!     PLOT X SCALE LINEAR UNITS.
!
      do loop = 1,2
         xr = XBAse
  100    continue
         if(xr<XLIm(1))then
         elseif(xr>XLIm(2))then
            yr1 = YLIm(2)
            yr2 = yr1 - box2p
            cycle
         else
            call PLOTP(xr,yr1,3)
            call PLOTP(xr,yr2,2)
         endif
         xr = xr + XSTep2
         goto 100
      enddo
   else
!
!     PLOT X SCALE LOG10 DECADES.
!
      do loop = 1,2
         ixmin = XLIm(1)
         if(XLIm(1)<0.0)ixmin = ixmin - 1
         ixmax = XLIm(2) + 1.0
         xdec = XLIm(2) - XLIm(1)
         if(xdec<=10.0)then
            if(xdec<=6.0)then
               if(xdec<=3.0)then
                  ilog1 = 2
                  klog1 = 1
               else
                  ilog1 = 2
                  klog1 = 2
               endif
            else
               ilog1 = 5
               klog1 = 5
            endif
         else
            ilog1 = 10
            klog1 = 10
         endif
         do i = ixmin,ixmax
            xr = i
            if(xr<XLIm(1).or.xr>XLIm(2))then
            else
               call PLOTP(xr,yr1,3)
               call PLOTP(xr,yr2,2)
               if(ilog1>9)cycle
            endif
            do j = ilog1,9,klog1
               xz = xr + TABlog(j)
               if(xz<XLIm(1).or.xz>XLIm(2))then
               else
                  call PLOTP(xz,yr1,3)
                  call PLOTP(xz,yr2,2)
               endif
            enddo
         enddo
         yr1 = YLIm(2)
         yr2 = yr1 - box2p
      enddo
   endif
   xr1 = XLIm(1)
   box2p = BOX2*(XLIm(2)-XLIm(1))/(XINch(2)-XINch(1))
   xr2 = xr1 + box2p
   if(IWAy(2)==1)then
!
!     PLOT Y SCALE LINEAR UNITS.
!
      do loop = 1,2
         yr = YBAse
  240    continue
         if(yr<YLIm(1))then
         elseif(yr>YLIm(2))then
            xr1 = XLIm(2)
            xr2 = xr1 - box2p
            cycle
         else
            call PLOTP(xr1,yr,3)
            call PLOTP(xr2,yr,2)
         endif
         yr = yr + YSTep2
         goto 240
      enddo
   else
!
!     PLOT Y SCALE LOG10 DECADES.
!
      do loop = 1,2
         iymin = YLIm(1)
         if(YLIm(1)<0.0)iymin = iymin - 1
         iymax = YLIm(2) + 1.0
         ydec = YLIm(2) - YLIm(1)
         if(ydec<=10.0)then
            if(ydec<=6.0)then
               if(ydec<=3.0)then
                  ilog1 = 2
                  klog1 = 1
               else
                  ilog1 = 2
                  klog1 = 2
               endif
            else
               ilog1 = 5
               klog1 = 5
            endif
         else
            ilog1 = 10
            klog1 = 10
         endif
         do i = iymin,iymax
            yr = i
            if(yr<YLIm(1).or.yr>YLIm(2))then
            else
               call PLOTP(xr1,yr,3)
               call PLOTP(xr2,yr,2)
            endif
            if(ilog1>9)then
            else
               do j = ilog1,9,klog1
                  yz = yr + TABlog(j)
                  if(yz<YLIm(1).or.yz>YLIm(2))then
                  else
                     call PLOTP(xr1,yz,3)
                     call PLOTP(xr2,yz,2)
                  endif
               enddo
            endif
         enddo
         xr1 = XLIm(2)
         xr2 = xr1 - box2p
      enddo
   endif
   return
   end subroutine GRID0
   subroutine GRID1
   implicit none
!*--GRID13690
!
!
! COMMON variables
!
   integer,dimension(2) :: IWAy
   integer :: IXLab,IXStep,IYLab,IYStep
   real,dimension(10) :: TABlog
   real :: XBAse,XMUlt,XSTep1,XSTep2,YBAse,YMUlt,YSTep1,YSTep2
   real,dimension(2) :: XLIm,YLIm
   common /LOGTAB/ TABlog
   common /WAYS  / IWAy
   common /XYLABI/ IXLab,IYLab,XMUlt,YMUlt,XBAse,XSTep1,XSTep2,YBAse,YSTep1,YSTep2,IXStep,IYStep
   common /XYLIM / XLIm,YLIm
!
! Local variables
!
   integer :: i,idir,ilog1,ixmax,ixmin,iymax,iymin,j,klog1
   real :: xdec,xr,xz,ydec,yr,yz
!
!
!
!  PLOT X AND Y AXIS FULL GRID
!
   call PEN(1)
   idir = 1
   if(IWAy(1)==1)then
!
!     PLOT X SCALE LINEAR UNITS.
!
      xr = XBAse
   else
!
!     PLOT X SCALE LOG10 DECADES.
!
      ixmin = XLIm(1)
      if(XLIm(1)<0.0)ixmin = ixmin - 1
      ixmax = XLIm(2) + 1.0
      xdec = XLIm(2) - XLIm(1)
      if(xdec<=10.0)then
         if(xdec<=6.0)then
            if(xdec<=3.0)then
               ilog1 = 2
               klog1 = 1
            else
               ilog1 = 2
               klog1 = 2
            endif
         else
            ilog1 = 5
            klog1 = 5
         endif
      else
         ilog1 = 10
         klog1 = 10
      endif
      do i = ixmin,ixmax
         xr = i
         if(xr<XLIm(1).or.xr>XLIm(2))then
         else
            if(idir==1)then
               call PLOTP(xr,YLIm(1),3)
               call PLOTP(xr,YLIm(2),2)
            else
               call PLOTP(xr,YLIm(2),3)
               call PLOTP(xr,YLIm(1),2)
            endif
            idir = 3 - idir
         endif
         if(ilog1>9)then
         else
            do j = ilog1,9,klog1
               xz = xr + TABlog(j)
               if(xz<XLIm(1).or.xz>XLIm(2))then
               else
                  if(idir==1)then
                     call PLOTP(xz,YLIm(1),3)
                     call PLOTP(xz,YLIm(2),2)
                  else
                     call PLOTP(xz,YLIm(2),3)
                     call PLOTP(xz,YLIm(1),2)
                  endif
                  idir = 3 - idir
               endif
            enddo
         endif
      enddo
      goto 160
   endif
  130 continue
   if(xr<XLIm(1))then
   else
      if(xr>XLIm(2))goto 160
      if(idir==1)then
         call PLOTP(xr,YLIm(1),3)
         call PLOTP(xr,YLIm(2),2)
      else
         call PLOTP(xr,YLIm(2),3)
         call PLOTP(xr,YLIm(1),2)
      endif
   endif
   idir = 3 - idir
   xr = xr + XSTep2
   goto 130
  160 continue
   idir = 1
   if(IWAy(2)==1)then
!
!     PLOT Y SCALE LINEAR UNITS.
!
      yr = YBAse
   else
!
!     PLOT Y SCALE LOG10 DECADES.
!
      iymin = YLIm(1)
      if(YLIm(1)<0.0)iymin = iymin - 1
      iymax = YLIm(2) + 1.0
      ydec = YLIm(2) - YLIm(1)
      if(ydec<=10.0)then
         if(ydec<=6.0)then
            if(ydec<=3.0)then
               ilog1 = 2
               klog1 = 1
            else
               ilog1 = 2
               klog1 = 2
            endif
         else
            ilog1 = 5
            klog1 = 5
         endif
      else
         ilog1 = 10
         klog1 = 10
      endif
      do i = iymin,iymax
         yr = i
         if(yr<YLIm(1).or.yr>YLIm(2))then
         else
            if(idir==1)then
               call PLOTP(XLIm(1),yr,3)
               call PLOTP(XLIm(2),yr,2)
            else
               call PLOTP(XLIm(2),yr,3)
               call PLOTP(XLIm(1),yr,2)
            endif
            idir = 3 - idir
         endif
         if(ilog1>9)then
         else
            do j = ilog1,9,klog1
               yz = yr + TABlog(j)
               if(yz<YLIm(1).or.yz>YLIm(2))then
               else
                  if(idir==1)then
                     call PLOTP(XLIm(1),yz,3)
                     call PLOTP(XLIm(2),yz,2)
                  else
                     call PLOTP(XLIm(2),yz,3)
                     call PLOTP(XLIm(1),yz,2)
                  endif
                  idir = 3 - idir
               endif
            enddo
         endif
      enddo
      goto 320
   endif
  290 continue
   if(yr<YLIm(1))then
   else
      if(yr>YLIm(2))goto 320
      if(idir==1)then
         call PLOTP(XLIm(1),yr,3)
         call PLOTP(XLIm(2),yr,2)
      else
         call PLOTP(XLIm(2),yr,3)
         call PLOTP(XLIm(1),yr,2)
      endif
   endif
   idir = 3 - idir
   yr = yr + YSTep2
   goto 290
  320 continue
   return
   end subroutine GRID1
   subroutine EVALP
   implicit none
!*--EVALP3882
!
!
! PARAMETER definitions
!
   integer,parameter :: MXPNT = 90000
!
! COMMON variables
!
   integer :: IBAse,ISCr,ITOp,IXLab,IXStep,IYLab,IYStep,N2
   integer,dimension(2) :: IWAy
   real :: XBAse,XMUlt,XSTep1,XSTep2,YBAse,YMUlt,YSTep1,YSTep2
   real,dimension(2) :: XLIm,XREal,YLIm,YREal
   real,dimension(MXPNT) :: XPAge,YPAge
   common /PAGEXY/ XPAge,YPAge,N2,IBAse,ITOp,ISCr
   common /WAYS  / IWAy
   common /XYLABI/ IXLab,IYLab,XMUlt,YMUlt,XBAse,XSTep1,XSTep2,YBAse,YSTep1,YSTep2,IXStep,IYStep
   common /XYLIM / XLIm,YLIm
   common /XYREAL/ XREal,YREal
!
! Local variables
!
   integer :: i,imon,ipass,ktop
   real :: X,Y
   real :: xl,xlast,xn,xnow,yl,ylast,ynow
!
!
!
!  PLOT EVALUATED DATA.
!
!
   if(N2<=0)return
   call PEN(2)
   ipass = 0
   ktop = 0
!
!  SET UP LOOP OVER POINTS.
!
   i = 0
   10 continue
   i = i + 1
   do while (.true.)
!
!     SELECT NEXT POINT AND INITIALIZE FLAG TO INDICATE POINT IS NO PLOT
!
      xnow = X(i)
      ynow = Y(i)
      imon = 1
!
!     SELECT POINTS IN ENERGY RANGE OF PLOT.
!
      if(xnow<XREal(1))exit
      if(ynow<YREal(1).and.ipass==0)exit
      if(xnow<XREal(2))then
!
!        INTERPOLATE TO LOWER ENERGY LIMIT AND TOP/BOTTOM OF PLOT
!        UNLESS FIRST POINT OR AT LEAST ONE POINT HAS ALREADY BEEN PLOTTED.
!
         if(i<=1.or.ipass>0)goto 110
      else
!
!        END OF PLOTTING RANGE REACHED. IF NO PRECEDING POINTS (ALL DATA
!        ABOVE RANGE OF PLOT) RETURN
!
         if(i<=1)goto 150
!
!        IF NO POINTS YET PLOTTED (I.E., LAST POINT BELOW PLOT RANGE,
!        CURRENT POINT ABOVE RANGE) RESET POINT INDEX TO DO UPPER LIMIT
!        NEXT TIME THROUGH LOOP. THIS TIME INTERPOLATE TO LOWER ENERGY
!        LIMIT AND TOP/BOTTOM OF PLOT.
!
         if(ipass>0)then
!
!           SET FLAG TO INDICATE END OF PLOTTING RANGE.
!           INTERPOLATE TO UPPER ENERGY LIMIT AND TOP/BOTTOM OF PLOT.
!
            if(xnow>XREal(2))imon = 0
            ktop = 1
            if(xnow>xlast)ynow = ((xnow-XREal(2))*ylast+(XREal(2)-xlast)*ynow)/(xnow-xlast)
            xnow = XREal(2)
            if(ynow<=YREal(2))then
               if(ynow>=YREal(1))then
               else
                  imon = 0
                  if(ynow>ylast)xnow = ((ynow-YREal(1))*xlast+(YREal(1)-ylast)*xnow)/(ynow-ylast)
                  ynow = YREal(1)
               endif
            else
               imon = 0
               if(ynow<ylast)xnow = ((ynow-YREal(2))*xlast+(YREal(2)-ylast)*xnow)/(ynow-ylast)
               ynow = YREal(2)
            endif
            goto 110
         else
            i = i - 1
         endif
      endif
      imon = 0
      if(xnow>xlast)ynow = ((xnow-XREal(1))*ylast+(XREal(1)-xlast)*ynow)/(xnow-xlast)
      xnow = XREal(1)
      if(ynow<=YREal(2))then
         if(ynow>=YREal(1))then
         else
            imon = 0
            if(ynow<ylast)xn = ((ynow-YREal(1))*xlast+(YREal(1)-ylast)*xnow)/(ynow-ylast)
            xnow = MIN(xn,xnow)
            ynow = YREal(1)
         endif
      else
         imon = 0
         if(ynow<ylast)xn = ((ynow-YREal(2))*xlast+(YREal(2)-ylast)*xnow)/(ynow-ylast)
         xnow = MAX(xn,xnow)
         ynow = YREal(2)
      endif
!
!     LIMIT X AND Y TO RANGE OF THE PLOT. CONVERT TO PLOT UNITS.
!
  110 continue
      if(xnow<XREal(1))xnow = XREal(1)
      if(xnow>XREal(2))xnow = XREal(2)
      if(ynow<YREal(1))ynow = YREal(1)
      if(ynow>YREal(2))ynow = YREal(2)
      xl = xnow*XMUlt
      yl = ynow*YMUlt
!
!     MOVE OR PLOT TO POINT.
!
      if(ipass>0)then
!        IF(I-1) 35,35,3
         call PLOTI(xl,yl,2,imon)
      else
         call PLOTI(xl,yl,3,imon)
         ipass = 1
!        ----IF FIRST POINT USED FOR INTERPOLATION BRANCH BACK TO NOW TREAT
!        ----FIRST POINT.
         if(xnow<X(i))cycle
      endif
      if(ktop>0)goto 150
      exit
   enddo
!
!  SAVE COORDINATES FOR INTERPOLATION.
!
   xlast = xnow
   ylast = ynow
   if(i<N2)goto 10
  150 continue
   return
   end subroutine EVALP
   subroutine EXFORP
   implicit none
!*--EXFORP4036
!
!
! PARAMETER definitions
!
   integer,parameter :: MXPGP = 10000
!
! COMMON variables
!
   real :: AWR,BOT1,BOT2,BOT3,BOX,BOX2,BOX4,BOXwt2,EINc,HT,HT2,HT34,HTH,RIGht1,RIGht2,RIGht3,RIGht4,RIGht5,RIGht6,RIGht7,RIGht8,  &
         & TOP1,TOP2,TOP3,WT,WT38,WTH,XBAse,XMUlt,XSTep1,XSTep2,YBAse,YMUlt,YSTep1,YSTep2
   real,dimension(MXPGP) :: DXEx,DYEx,E2,XEX,YEX
   real,dimension(48) :: E2T,EXHigh,EXLow
   real,dimension(100) :: EHGet,ELGet,EPGet
   real,dimension(4) :: FIEld4
   integer :: IDOub,IEX,IGEt,IGRoup,IREf,IXLab,IXStep,IYLab,IYStep,IZA,KGRoup,MAT,MAXref,MF,MODiza,MREf,MSYmbl,MT,NGEt
   character(4) :: IM78
   integer,dimension(100) :: INTrn,IZAhi,IZAlow,MFHi,MFLow,MTHi,MTLow
   integer,dimension(2) :: IWAy
   integer,dimension(48) :: LREf
   integer,dimension(MXPGP) :: NREf
   character(4),dimension(9) :: REF1
   character(4),dimension(9,48) :: REFs
   real,dimension(2) :: X4Limx,X4Limy,XINch,XLIm,XREal,YINch,YLIm,YREal
   character(4),dimension(10) :: XLAbel,YLAbel
   common /DOUBL / IDOub,FIEld4
   common /EXFOR / XEX,DXEx,YEX,DYEx,NREf,E2,IEX
   common /GETEM / IZAlow,MFLow,MTLow,ELGet,IZAhi,MFHi,MTHi,EHGet,EPGet,INTrn,NGEt,IGEt
   common /INCHES/ XINch,YINch
   common /PAINT / BOX,BOX2,BOX4,BOXwt2,HT,HT2,HTH,HT34,WT,WTH,WT38
   common /REFERC/ REFs,REF1
   common /REFERI/ LREf,EXLow,EXHigh,E2T,IREf,MREf,MAXref,IGRoup,KGRoup
   common /SPOTS / TOP1,TOP2,TOP3,BOT1,BOT2,BOT3,RIGht1,RIGht2,RIGht3,RIGht4,RIGht5,RIGht6,RIGht7,RIGht8
   common /SYMBLM/ MSYmbl
   common /WAYS  / IWAy
   common /WHEREI/ IZA,MAT,MF,MT,MODiza,EINc,AWR
   common /WHO78C/ IM78
   common /X4LIMS/ X4Limx,X4Limy
   common /XYLABC/ XLAbel,YLAbel
   common /XYLABI/ IXLab,IYLab,XMUlt,YMUlt,XBAse,XSTep1,XSTep2,YBAse,YSTep1,YSTep2,IXStep,IYStep
   common /XYLIM / XLIm,YLIm
   common /XYREAL/ XREal,YREal
!
! Local variables
!
   real :: ALOG10,FLOAT
   character(4),dimension(3) :: def78
   real :: dxl,dyl,e2out,exh,exl,xbotx,xl,xlbm,xlbp,xm,xnow2,xp,xref,ybot,yl,ylbm,ylbp,ym,ynow,yp,zmult
   character(4) :: hl
   integer :: i,idef78,ipen,iptz,irr,izunit,jptx,kref,krr
   character(4),dimension(26) :: refnum
   character(4),dimension(2) :: zunit
!
!
!
!  PLOT EXPERIMENTAL DATA...EACH REF SEPARATELY.
!
   data hl/' HL'/
   data refnum/'1','2','3','4','5','6','7','8','9','A','B','C','D','E','F','G','H','J','K','L','M','N','P','R','S','X'/
!  ----NOTHING TO DO IF NO EXFOR DATA.
   if(IEX<=0)return
!  ----INITIALIZE LEGEND BOX COORDINATES.
   ynow = YINch(2) - 3.5*HT
   if(KGRoup/=0)ynow = ynow - 1.75*HT
   ybot = YINch(1) + 1.75*MREf*HT
!
!  SELECT REFERENCES IN ENERGY RANGE.
!
   do kref = 1,IREf
      if(LREf(kref)<=0)cycle
!
!     IDENTIFY REFERENCE.
!
      ipen = kref + 2
      call PEN(ipen)
      if(MSYmbl==0.and.MREf==1.and.KGRoup==0)then
      else
!        ----IDENTIFY SYMBOL USED FOR EACH REFERENCE.
         call PLOT(RIGht7,ynow-BOX,3)
         call PLOT(RIGht8,ynow-BOX,2)
         call PLOT(RIGht8,ynow+BOX,2)
         call PLOT(RIGht7,ynow+BOX,2)
         call PLOT(RIGht7,ynow-BOX,2)
         call SYMBLH(RIGht7+BOX-BOXwt2,ynow-BOX2,BOX,refnum(kref),0.0,1)
         if(KGRoup==0)goto 50
         if(MF/=3)then
         elseif(IDOub==2)then
!           ----IDENTIFY INCIDENT ENERGY.
            call EUNIT(E2T(kref),zmult,zunit,izunit,4,iptz,1)
            call SYMBLH(RIGht5,ynow-HTH,HT,'E = ',0.0,4)
            xnow2 = RIGht5 + 4.0*WT
            goto 40
         endif
!        ----IDENTIFY SECOND ENERGY.
         call WHAT78(IM78,def78,idef78)
         call SYMBLH(RIGht5,ynow-HTH,HT,def78,0.0,idef78)
         xnow2 = RIGht5 + FLOAT(idef78+1)*WT
         call SYMBLH(xnow2,ynow-HTH,HT,'=',0.0,1)
         xnow2 = xnow2 + 2.0*WT
!        ----SELECT HALF-LIFE OR ENERGY UNITS.
         if(IM78/=hl)then
            call EUNIT(E2T(kref),zmult,zunit,izunit,4,iptz,1)
         else
            call HLUNIT(E2T(kref),zmult,zunit,izunit,4,iptz)
         endif
!        ----PLOT E2 FIELD AND UNITS.
   40    continue
         e2out = E2T(kref)*zmult
         call NUMBRH(xnow2,ynow-HTH,HT,e2out,0.0,iptz)
         call SYMBLH(xnow2+7.0*WT,ynow-HTH,HT,zunit,0.0,izunit)
         goto 60
      endif
!     ----IDENTIFY REFERENCE.
   50 continue
      call LEFTY(REFs(1,kref),irr,krr,25)
      call SYMBLH(RIGht5,ynow-HTH,HT,REFs(1,kref),0.0,irr)
!...
!...  print *,iex,refs(1,kref),refs(2,kref)
!...
   60 continue
      if(MSYmbl==0.and.MREf==1.and.KGRoup==0)then
      else
!        ----SUPPRESS RANGE BOX PLOT IF TOO MANY REFERENCES
         if(YINch(2)-YINch(1)<1.75*(MREf+2)*HT*2)goto 92
!        ----IDENTIFY REFERENCE IN RANGE BOX.
         call PLOT(RIGht7,ybot-BOX,3)
         call PLOT(RIGht8,ybot-BOX,2)
         call PLOT(RIGht8,ybot+BOX,2)
         call PLOT(RIGht7,ybot+BOX,2)
         call PLOT(RIGht7,ybot-BOX,2)
         call SYMBLH(RIGht7+BOX-BOXwt2,ybot-BOX2,BOX,refnum(kref),0.0,1)
      endif
!     ----PLOT RANGE.
      if(MF==3.or.MF==5)then
      elseif(MF==6.and.IDOub==2)then
      else
!        ----PRINT COSINE, LEGENDRE ORDER OR ATOMIC WEIGHT RANGE.
         iptz = -1
         if(MF==4)iptz = 4
         if(MF==6.and.IDOub==1)iptz = 4
         call NUMBRH(RIGht5,ybot-HTH,HT,EXLow(kref),0.0,iptz)
         xnow2 = RIGht5 + 3.0*WT
         if(iptz==4)xnow2 = xnow2 + 5.0*WT
         if(EXLow(kref)<=0.0)xnow2 = xnow2 + WT
         if(EXLow(kref)>10.0)xnow2 = xnow2 + WT
         if(EXLow(kref)>100.0)xnow2 = xnow2 + WT
         call NUMBRH(xnow2,ybot-HTH,HT,EXHigh(kref),0.0,iptz)
         goto 90
      endif
!     ----PRINT ENERGY IN NORMAL FORM.
      call EUNIT(EXHigh(kref),zmult,zunit,izunit,4,iptz,1)
      exl = EXLow(kref)*zmult
      jptx = 4
      if(exl>=10.0)jptx = 3
      if(exl>=100.0)jptx = 2
      call NUMBRH(RIGht5,ybot-HTH,HT,exl,0.0,jptx)
      exh = EXHigh(kref)*zmult
      call NUMBRH(RIGht5+8.0*WT,ybot-HTH,HT,exh,0.0,iptz)
      call SYMBLH(RIGht5+15.0*WT,ybot-HTH,HT,zunit,0.0,izunit)
!     ----PRINT POINT COUNT RIGHT ADJUSTED.
   90 continue
      xbotx = RIGht6
      xref = LREf(kref)
      if(xref<100000.0)xbotx = xbotx + WT
      if(xref<10000.0)xbotx = xbotx + WT
      if(xref<1000.0)xbotx = xbotx + WT
      if(xref<100.0)xbotx = xbotx + WT
      if(xref<10.0)xbotx = xbotx + WT
      call NUMBRH(xbotx,ybot-HTH,HT,xref,0.0,-1)
   92 continue
      ynow = ynow - 1.75*HT
      ybot = ybot - 1.75*HT
!
!     SELECT POINTS IN ENERGY RANGE AND PLOT THEM.
!
      do i = 1,IEX
!...
!...     print *,i,xex(i),yex(i)
!...
!        ----SELECT POINTS FROM CURRENT REFERENCE WHICH ARE WITHIN THE PLOTTING
!        ----AREA.
         if(NREf(i)/=kref)then
         elseif(XEX(i)<X4Limx(1).or.XEX(i)>X4Limx(2))then
         elseif(YEX(i)<X4Limy(1).or.YEX(i)>X4Limy(2))then
         else
!           ----DEFINE X COORDINATES AND TRUNCATE TO LIMITS OF PLOT.
            xl = XEX(i)
            dxl = DXEx(i)
            xm = xl - dxl
            xp = xl + dxl
            if(xm<XREal(1))xm = XREal(1)
            if(xp>XREal(2))xp = XREal(2)
            xm = xm*XMUlt
            xl = xl*XMUlt
            xp = xp*XMUlt
!           ----IF REQUIRED CONVERT TO LOGS.
            if(IWAy(1)==1)then
            else
               xl = ALOG10(xl)
               xm = ALOG10(xm)
               xp = ALOG10(xp)
            endif
!           ----TRANSFORM TO COORDINATES OF THE PLOT.
            xl = ((xl-XLIm(1))*XINch(2)+(XLIm(2)-xl)*XINch(1))/(XLIm(2)-XLIm(1))
            xm = ((xm-XLIm(1))*XINch(2)+(XLIm(2)-xm)*XINch(1))/(XLIm(2)-XLIm(1))
            xp = ((xp-XLIm(1))*XINch(2)+(XLIm(2)-xp)*XINch(1))/(XLIm(2)-XLIm(1))
!           ----DEFINE Y COORDINATES AND TRUNCATE TO LIMITS OF PLOT.
            yl = YEX(i)
            dyl = DYEx(i)
            ym = yl - dyl
            yp = yl + dyl
            if(ym<YREal(1))ym = YREal(1)
            if(yp>YREal(2))yp = YREal(2)
            ym = ym*YMUlt
            yl = yl*YMUlt
            yp = yp*YMUlt
!           ----IF REQUIRED CONVERT TO LOGS.
            if(IWAy(2)==1)then
            else
               yl = ALOG10(yl)
               ym = ALOG10(ym)
               yp = ALOG10(yp)
            endif
!           ----TRANSFORM TO COORDINATES OF THE PLOT.
            yl = ((yl-YLIm(1))*YINch(2)+(YLIm(2)-yl)*YINch(1))/(YLIm(2)-YLIm(1))
            ym = ((ym-YLIm(1))*YINch(2)+(YLIm(2)-ym)*YINch(1))/(YLIm(2)-YLIm(1))
            yp = ((yp-YLIm(1))*YINch(2)+(YLIm(2)-yp)*YINch(1))/(YLIm(2)-YLIm(1))
!           ----IF ONLY ONE REFERENCE AND NOT IDENTIFYING E2 DO NOT PLOT SYMBOL.
            if(MSYmbl/=0.or.MREf>1.or.KGRoup/=0)then
!
!              PLOT SYMBOL AND ERROR BARS.
!
               xlbm = xl - BOX
               xlbp = xl + BOX
               ylbm = yl - BOX
               ylbp = yl + BOX
!              ----PLOT BOX AND SYMBOL.
               call PLOT(xlbm,ylbm,3)
               call PLOT(xlbp,ylbm,2)
               call PLOT(xlbp,ylbp,2)
               call PLOT(xlbm,ylbp,2)
               call PLOT(xlbm,ylbm,2)
               call SYMBLH(xl-BOXwt2,yl-BOX2,BOX,refnum(kref),0.0,1)
!              ----PLOT X ERROR BARS IF THEY EXTEND BEYOND BOX.
               if(xm>=xlbm)then
               else
                  call PLOT(xm,ylbm,3)
                  call PLOT(xm,ylbp,2)
                  call PLOT(xm,yl,3)
                  call PLOT(xlbm,yl,2)
               endif
               if(xp<=xlbp)then
               else
                  call PLOT(xp,ylbm,3)
                  call PLOT(xp,ylbp,2)
                  call PLOT(xp,yl,3)
                  call PLOT(xlbp,yl,2)
               endif
!              ----PLOT Y ERROR BARS IF THEY EXTEND BEYOND BOX.
               if(ym>=ylbm)then
               else
                  call PLOT(xlbm,ym,3)
                  call PLOT(xlbp,ym,2)
                  call PLOT(xl,ym,3)
                  call PLOT(xl,ylbm,2)
               endif
               if(yp<=ylbp)then
               else
                  call PLOT(xlbm,yp,3)
                  call PLOT(xlbp,yp,2)
                  call PLOT(xl,yp,3)
                  call PLOT(xl,ylbp,2)
               endif
            else
!
!              ONLY PLOT ERROR BARS AND X AT MIDDLE.
!
               xlbm = xl - BOX4
               xlbp = xl + BOX4
               ylbm = yl - BOX4
               ylbp = yl + BOX4
!              ----PLOT X ERROR BARS.
               if(dxl<=0)then
               else
                  call PLOT(xm,ylbm,3)
                  call PLOT(xm,ylbp,2)
                  call PLOT(xm,yl,3)
                  call PLOT(xp,yl,2)
                  call PLOT(xp,ylbm,3)
                  call PLOT(xp,ylbp,2)
               endif
!              ----PLOT Y ERROR BARS.
               if(dyl<=0.0)then
               else
                  call PLOT(xlbm,ym,3)
                  call PLOT(xlbp,ym,2)
                  call PLOT(xl,ym,3)
                  call PLOT(xl,yp,2)
                  call PLOT(xlbm,yp,3)
                  call PLOT(xlbp,yp,2)
               endif
!              ----PLOT X AT DATA POINT.
               call PLOT(xlbm,ylbp,3)
               call PLOT(xlbp,ylbm,2)
               call PLOT(xlbm,ylbm,3)
               call PLOT(xlbp,ylbp,2)
            endif
         endif
      enddo
   enddo
   return
   end subroutine EXFORP
   subroutine PLOTP(X,Y,Ipen)
   implicit none
!*--PLOTP4353
!
!
! COMMON variables
!
   real,dimension(2) :: XINch,XLIm,YINch,YLIm
   common /INCHES/ XINch,YINch
   common /XYLIM / XLIm,YLIm
!
! Dummy arguments
!
   integer :: Ipen
   real :: X,Y
   intent (in) X,Y
!
! Local variables
!
   real :: xi,yi
!
!
!
!  TRANSLATE DATA FROM THE PLANE OF THE DATA TO THE PLANE OF THE
!  PLOT.
!
   xi = ((X-XLIm(1))*XINch(2)+(XLIm(2)-X)*XINch(1))/(XLIm(2)-XLIm(1))
   yi = ((Y-YLIm(1))*YINch(2)+(YLIm(2)-Y)*YINch(1))/(YLIm(2)-YLIm(1))
   call PLOT(xi,yi,Ipen)
   return
   end subroutine PLOTP
   subroutine PLOTI(Xp,Yp,Ipen,Imon)
   implicit none
!*--PLOTI4387
!
!
! COMMON variables
!
   real :: BOX,BOX2,BOX4,BOXwt2,HT,HT2,HT34,HTH,WT,WT38,WTH
   integer :: IMEndf
   integer,dimension(2) :: IWAy
   real,dimension(2) :: XINch,XLIm,YINch,YLIm
   common /ENDFIM/ IMEndf
   common /INCHES/ XINch,YINch
   common /PAINT / BOX,BOX2,BOX4,BOXwt2,HT,HT2,HTH,HT34,WT,WTH,WT38
   common /WAYS  / IWAy
   common /XYLIM / XLIm,YLIm
!
! Dummy arguments
!
   integer :: Imon,Ipen
   real :: Xp,Yp
   intent (in) Imon,Xp,Yp
!
! Local variables
!
   real :: absdx,absdy,dx,dxinch,dy,dyinch,x,xi,xilast,xplast,xstep,xt,xz,y,yi,yilast,yplast,ystep,yt,yz
   real :: ALOG10
   integer :: i,istep
!
!
!
!  TRANSLATE DATA FROM THE PLANE OF THE DATA TO THE PLANE OF THE
!  PLOT. IF NECESSARY INTERPOLATE DATA TO NON-LINEAR PLOT.
!
!...
!...
   data xplast/0.0/
   data yplast/0.0/
   data xilast/0.0/
   data yilast/0.0/
!  ----DEFINE COORDINATES FOR INTERNAL USE.
   x = Xp
   y = Yp
!  ----IF NECESSARY CONVERT TO LOG.
   if(IWAy(1)==2)x = ALOG10(x)
   if(IWAy(2)==2)y = ALOG10(y)
!  ----CONVERT TO THE PLANE OF THE PLOT.
   xi = ((x-XLIm(1))*XINch(2)+(XLIm(2)-x)*XINch(1))/(XLIm(2)-XLIm(1))
   yi = ((y-YLIm(1))*YINch(2)+(YLIm(2)-y)*YINch(1))/(YLIm(2)-YLIm(1))
!  ----IF MOVING, NOT PLOTTING MOVE (NO INTERPOLATION NECESSARY).
   if(Ipen==3)then
!     ----IF PLOT IS LINEAR X VS. LINEAR Y DRAW STRAIGHT LINE.
   elseif(IWAy(1)==1.and.IWAy(2)==1)then
   else
!     ----CHECK FOR INTERPOLATION.
      dxinch = xi - xilast
      dyinch = yi - yilast
      absdx = ABS(dxinch)
      absdy = ABS(dyinch)
!     ----IF NO CHANGE IN ONE COORDINATE DRAW STRIAGHT LINE.
      if(absdx<=0.0.or.absdy<=0.0)then
      else
!        ----CHECK FOR LARGEST CHANGE IN INCHES. IF LESS THAN 0.1 DO NOT
!        ----INTERPOLATE.
         if(absdx<absdy)then
         else
            if(absdx<=0.1)goto 120
!           ----INTERPOLATE IN X DIRECTION. DEFINE LINEAR OR LOG STEP CHANGE IN
!           X.
            istep = absdx/0.1 + 1.0
            xstep = istep
            xz = xplast
            if(IWAy(1)/=1)then
               dx = LOG(Xp/xplast)/xstep
               dx = EXP(dx)
            else
               dx = (Xp-xplast)/xstep
            endif
!           ----SET UP LOOP OVER INTERPOLATION STEPS.
            do i = 1,istep
               if(i==istep)goto 120
               if(IWAy(1)/=1)then
                  xz = xz*dx
               else
                  xz = xz + dx
               endif
               x = xz
               y = ((x-xplast)*Yp+(Xp-x)*yplast)/(Xp-xplast)
               if(IWAy(1)==2)x = ALOG10(x)
               if(IWAy(2)==2)y = ALOG10(y)
               xt = ((x-XLIm(1))*XINch(2)+(XLIm(2)-x)*XINch(1))/(XLIm(2)-XLIm(1))
               yt = ((y-YLIm(1))*YINch(2)+(YLIm(2)-y)*YINch(1))/(YLIm(2)-YLIm(1))
               call PLOT(xt,yt,2)
            enddo
         endif
         if(absdy<=0.1)then
         else
!           ----INTERPOLATE IN Y DIRECTION. DEFINE LINEAR OR LOG STEP CHANGE IN
!           Y.
            istep = absdy/0.1 + 1.0
            ystep = istep
            yz = yplast
            if(IWAy(2)/=1)then
               dy = LOG(Yp/yplast)/ystep
               dy = EXP(dy)
            else
               dy = (Yp-yplast)/ystep
            endif
!           ----SET UP LOOP OVER INTERPOLATION STEPS.
            do i = 1,istep
               if(i==istep)exit
               if(IWAy(2)/=1)then
                  yz = yz*dy
               else
                  yz = yz + dy
               endif
               y = yz
               x = ((y-yplast)*Xp+(Yp-y)*xplast)/(Yp-yplast)
               if(IWAy(1)==2)x = ALOG10(x)
               if(IWAy(2)==2)y = ALOG10(y)
               xt = ((x-XLIm(1))*XINch(2)+(XLIm(2)-x)*XINch(1))/(XLIm(2)-XLIm(1))
               yt = ((y-YLIm(1))*YINch(2)+(YLIm(2)-y)*YINch(1))/(YLIm(2)-YLIm(1))
               call PLOT(xt,yt,2)
            enddo
         endif
      endif
   endif
!  ----PLOT OR MOVE TO FINAL LOCATION.
  120 continue
   call PLOT(xi,yi,Ipen)
!  ----IF REQUESTED IDENTIFY ENDF/B DATA POINT.
   if(Imon<=0.or.IMEndf/=2)then
   else
      call PLOT(xi-BOX4,yi,3)
      call PLOT(xi,yi+BOX4,2)
      call PLOT(xi+BOX4,yi,2)
      call PLOT(xi,yi-BOX4,2)
      call PLOT(xi-BOX4,yi,2)
      call PLOT(xi,yi,3)
   endif
!  ----SAVE COORDINATES.
   xplast = Xp
   yplast = Yp
   xilast = xi
   yilast = yi
   return
   end subroutine PLOTI
   subroutine GETEX(Iend)
   implicit none
!*--GETEX4537
!
!
! PARAMETER definitions
!
   integer,parameter :: MXPGP = 10000,MXZAT = 1800
!
! COMMON variables
!
   real :: AWR,EINc
   real,dimension(MXPGP) :: DXEx,DYEx,E2,XEX,YEX
   real,dimension(48) :: E2T,EXHigh,EXLow
   real,dimension(4) :: FIEld4
   integer :: IDOub,IEX,IGRoup,ILIb,IMAm78,IMNorm,INP,IREf,ITApe1,ITApe2,IXErr,IYErr,IZA,IZArat,KGRoup,MAT,MAXie,MAXref,MF,MFIn,  &
            & MINnie,MODiza,MREf,MT,MTRat,MYMode,OUTp
   character(4) :: IM78,MSTat1,MSTat2
   character(1) :: LABcm,MSTar1,MSTar2,STAtus
   character(4),dimension(4) :: LIBnam,ZABcd
   integer,dimension(48) :: LREf
   integer,dimension(MXPGP) :: NREf
   character(4),dimension(9) :: REF1
   character(4),dimension(9,48) :: REFs
   common /DOUBL / IDOub,FIEld4
   common /EXERRS/ IXErr,IYErr
   common /EXFOR / XEX,DXEx,YEX,DYEx,NREf,E2,IEX
   common /INPARM/ MINnie,MAXie
   common /LIBC  / LIBnam
   common /LIBI  / ILIb
   common /MODEMY/ MYMode
   common /RATZA / IZArat,MTRat,MFIn
   common /RATZAC/ MSTar1,MSTar2
   common /REFERC/ REFs,REF1
   common /REFERI/ LREf,EXLow,EXHigh,E2T,IREf,MREf,MAXref,IGRoup,KGRoup
   common /SYSSTA/ LABcm,STAtus
   common /UNITS / INP,OUTp,ITApe1,ITApe2
   common /UNNORM/ IMNorm
   common /WHEREC/ ZABcd,MSTat1,MSTat2
   common /WHEREI/ IZA,MAT,MF,MT,MODiza,EINc,AWR
   common /WHO78C/ IM78
   common /WHO78I/ IMAm78
!
! Dummy arguments
!
   integer :: Iend
   intent (out) Iend
!
! Local variables
!
   real,save :: ain,de1,de2,dmc,e2tol,eext,ein,etol,gam,qq,small,ts1,ts2
   character(4) :: blank,hl,im78x,msta1x,msta2x
   character(1) :: blank1,dummy,labx,msta1r,msta2r,statx,ustat
   real,dimension(MXZAT),save :: enext,eninc
   character(4),dimension(3,8) :: fieldc
   real,dimension(8),save :: fieldi
   integer,save :: i,iatwt,iexpas,ii,iline,imsame,ipass,iprojt,iprojx,izatry,izax,j,jex,k,kex,kget,kref,kzatab,m,mex1,mex2,mfj,   &
                 & mfm2,mfsave,mfsavx,mfx,mor10,mttry,mtx,nzatab
   integer,dimension(9,MXZAT),save :: mzatab
!
!
!
!  READ COMPARABLE EXFOR DATA BASED ON EITHER,
!  (1) COMPARISON TO ENDF/B IZA/MF/MT, OR,
!  (2) COMPARISON TO REQUESTS.
!
!...
!...
   data fieldc/'    ','    ','    ','    ','    ','    ','    ','    ','    ','    ','    ','    ','    ','    ','    ','    ',   &
       &'    ','    ','    ','    ','    ','    ','    ','    '/
   data ipass/0/
   data blank/'    '/
   data blank1/' '/
   data hl/' HL'/
   data izatry/0/
   data mttry/0/
   data msta1r/' '/
   data msta2r/' '/
   data ustat/'U'/
!* Fraction tolerance for incident and secondary energies, respectively
   data etol,e2tol/0.015,0.003/
!* Tolerance fraction for numbers to be considered the same
   data small/1.E-5/
!
!  WHEN THIS ROUTINE IS CALLED THE FIRST TIME READ ENTIRE EXFOR
!  FILE AND SAVE INDICES TO REQUESTED ZA/MF/MT/POINT COUNTS.
!
   mor10 = 0
   if(ipass>0)goto 100
!...
   open(unit=29,file='EXFOR.LST',status='UNKNOWN')
!...
   nzatab = 0
   iline = 0
   10 continue
   do while (.true.)
      iline = iline + 1
      !
      read(ITApe1,2000,end=70,err=10)iprojx,izax,msta1x,mfx,mtx,msta2x,statx,labx,fieldc,im78x,REF1
      !
!     SKIP ALL POINTS WHICH CANNOT BE PLOTTED OR HAVE NOT BEEN REQUESTED
!
      if(MYMode>=2.and.(mfx<3.or.mfx>10))then
      else
         mfsavx = mfx
         if(mfx==10)mfsavx = 3
         if(mfx==203)mfsavx = 3
         if(mfx==402)mfsavx = 3
         if(mfx==154)mfsavx = 7
         if(mfx==801)mfsavx = 8
         if(mfsavx<3.or.mfsavx>10)cycle
         call FLOAT9(fieldc(1,1),fieldi(1))
         call FLOAT9(fieldc(1,5),fieldi(5))
         call FLOAT9(fieldc(1,7),fieldi(7))
         call RQEX(kget,izax,mfx,mtx,fieldi(1))
         if(kget<=0)cycle
!
!        IF ONLY PLOTTING EXFOR DATA, IF NECESSARY TRANSLATE ADDITIONAL
!        ZA AND MT (PRODUCTION OR RATIO).
!
         mttry = 0
         izatry = 0
         if(MYMode>=2)then
!           ----FOR CROSS SECTION RATIOS (MF=203) DEFINE DENOMINATOR ZA AND MT.
         elseif(mfx/=203)then
         else
            call FLOAT9(fieldc(1,5),fieldi(5))
            call FLOAT9(fieldc(1,6),fieldi(6))
            mttry = fieldi(5)
            izatry = fieldi(6)
            goto 30
         endif
!        ----FOR PARTICLE/ISOTOPE PRODUCTION (MT=9000-9999) CROSS SECTIONS OR
!        ----ANGULAR DISTRIBUTIONS (MF=3 OR 4) DEFINE PRODUCT ZA.
         if(mtx<9000)then
         else
            call FLOAT9(fieldc(1,6),fieldi(6))
!           BY DEFAULT OUTGOING PARTICLE IS THE SAME AS THE PROJECTILE
            if(mfx==6.and.fieldi(6)==0)fieldi(6) = iprojx + 0.9
            izatry = fieldi(6)
         endif
!
!        ACCEPTABLE POINT. SAVE ALL ZA/MF/MT COMBINATIONS.
!
   30    continue
         if(nzatab<=0)then
         else
            do i = 1,nzatab
               if(iprojx/=mzatab(1,i).or.izax/=mzatab(2,i).or.mfx/=mzatab(3,i).or.mtx/=mzatab(4,i).or.mttry/=mzatab(8,i).or.      &
                & izatry/=mzatab(9,i))then
               else
                  if(mfsavx==3)goto 60
!                 DIFFERENTIAL DATA ARE DIFFERENT IF INCIDENT OR SECONDARY
!                 ENERGIES DIFFER BY MORE THAN A FRACTION ETOL
!...
                  EINc = fieldi(1)
                  de1 = ABS(EINc-eninc(i))
                  ts1 = etol*EINc
                  if(mfx==4)then
                     eext = fieldi(7)
                     de2 = ABS(eext-enext(i))
                  elseif(mfx==5)then
                     eext = fieldi(5)
                     de2 = ABS(eext-enext(i))
                  else
                     eext = 0
                     de2 = 0
                  endif
                  ts2 = e2tol*eext
!...              IF((ABS(EINC-ENINC(I)).LE. ETOL*EINC) .AND.
!...              &   (ABS(EEXT-ENEXT(I)).LE.E2TOL*EEXT)) GO TO 60
                  if(de1<=ts1.and.de2<=ts2)goto 60
               endif
!...           IF( ABS(FIELDI(1)-ENINC(I)).LT.ETOL*ENINC(I)) GO TO 60
!...           IF(FIELDI(1).EQ.ENINC(I)) GO TO 60
!...
!...           print *,EINC,DE1,TS1,EEXT,DE2,TS2
!...
            enddo
!           ----NEW ZA/MF/MT. IF POSSIBLE SAVE.
!           ----SUPPRESS PROCESSING OF ANGLE-DEPENDENT ELASTIC CROSS SECTION
            if(mfsavx/=4)then
            elseif(iline-mzatab(5,nzatab)>1)then
            else
               goto 52
            endif
            if(nzatab<MXZAT)then
            else
               write(OUTp,6000)
               goto 70
            endif
         endif
         nzatab = nzatab + 1
   52    continue
         mzatab(1,nzatab) = iprojx
         mzatab(2,nzatab) = izax
         mzatab(3,nzatab) = mfx
         mzatab(4,nzatab) = mtx
         mzatab(5,nzatab) = iline
         mzatab(7,nzatab) = 0
         mzatab(8,nzatab) = mttry
         mzatab(9,nzatab) = izatry
         eninc(nzatab) = EINc
         enext(nzatab) = eext
         i = nzatab
         exit
      endif
   enddo
!...
!...  print *,' NEW mf,mt,Ei,Ex',mfx,mtx,einc,eext
!... 1       ,ref1(1),ref1(2)
!...
!  ----OLD ZA/MF/MT. UPDATE LAST RECORD COUNT AND POINT COUNT.
   60 continue
   mzatab(6,i) = iline
   mzatab(7,i) = mzatab(7,i) + 1
!...
!...      print *,'same mf,mt,Ei,Ex',mfx,mtx,einc,eext
!...     1       ,ref1(1),ref1(2)
!...
   goto 10
!
!  ENTIRE EXFOR FILE READ AND INDEXED. ELIMINATE SETS WITH LESS THAN
!  MINIMUM NUMBER OF POINTS. INDICATE END OF PLOTTING IF NO EXFOR TO
!  PLOT.
!
   70 continue
   if(nzatab<=0)goto 630
   kzatab = 0
   do i = 1,nzatab
!...
      write(29,'(I4,I6,I4,I6,2I8,3I6,1P,2E10.3)')(mzatab(j,i),j=1,9),eninc(i),enext(i)
!...
      if(mzatab(7,i)<MINnie)then
      else
         kzatab = kzatab + 1
         do k = 1,9
            mzatab(k,kzatab) = mzatab(k,i)
         enddo
         eninc(kzatab) = eninc(i)
         enext(kzatab) = enext(i)
      endif
   enddo
!...
   close(unit=29)
!...
   nzatab = kzatab
   if(nzatab<=0)goto 630
!
!  POSITION EXFOR FILE TO READ.
!
   kzatab = 0
   ipass = 1
   iline = 0
   rewind ITApe1
!
!  IF NO MORE EXFOR DATA TO PLOT INDICATE END OF PLOTTING.
!  DEFINE NEXT ZA/MF/MT BASED ON ENDF/B (ALREADY DEFINED) OR NEXT
!  ZA/MF/MT IN EXFOR INDEX. DETERMINE INDEX TO FIRST EXFOR RECORD
!  TO READ.
!
  100 continue
   if(nzatab<=0)goto 630
   if(MYMode>=2)then
!
!     COMPARE ENDF/B ZA/MF/MT TO EXFOR INDEX.
!
      iprojt = 1
      MTRat = 0
      IZArat = 0
      EINc = 0.0
      mfsave = MF
      if(MF==10)mfsave = 3
      KGRoup = IGRoup
      if(MF<5.or.MF>6)KGRoup = 0
      IMNorm = 0
      do k = 1,nzatab
         kzatab = k
         mfj = MF
         if(mfj==6.and.mzatab(3,kzatab)==5)mfj = 5
!...
!        IF(MFJ.EQ.10) MFJ=3
!...
         if(iprojt==mzatab(1,kzatab).and.IZA==mzatab(2,kzatab).and.mfj==mzatab(3,kzatab).and.MT==mzatab(4,kzatab))goto 130
      enddo
!     ----NO COMPARABLE EXFOR DATA.
      IEX = 0
      Iend = 1
      return
   else
!
!     SELECT NEXT ZA/MF/MT FROM EXFOR INDEX. END IF NO MORE EXFOR DATA
!     TO PLOT.
!
      MSTat1 = blank
      MSTat2 = blank
      kzatab = kzatab + 1
      if(kzatab>nzatab)goto 630
      iprojt = mzatab(1,kzatab)
      IZA = mzatab(2,kzatab)
      MF = mzatab(3,kzatab)
      MT = mzatab(4,kzatab)
      MTRat = mzatab(8,kzatab)
      IZArat = mzatab(9,kzatab)
      EINc = eninc(kzatab)
      eext = enext(kzatab)
      mfsave = MF
      if(MF==10)mfsavx = 3
      if(MF==203)mfsave = 3
      if(MF==402)mfsave = 3
      if(MF==154)mfsave = 7
      if(MF==801)mfsave = 8
      KGRoup = IGRoup
      if(MF/=3.and.MF/=10)KGRoup = 0
   endif
!
!  REQUIRED DATA IS PRESENT. INITIALIZE PARAMETERS.
!
!  ----INITIALIZE DATA POINT, REFERENCE COUNT, GROUP BY E2 FLAG, FIELDS
!  ----7 - 8 BLANK AND INTERNAL MF NUMBER.
  130 continue
   IEX = 0
   iexpas = 0
   IREf = 0
!  ----SET THE INCIDENT ENERGY AND OUTGOING PARTICLE FOR DIFFERENTIAL DATA
   IZArat = mzatab(9,kzatab)
   if(MF>=4.and.MF<=6)then
      EINc = eninc(kzatab)
      eext = enext(kzatab)
!...
!...  DO K1=1,NZATAB
!...  PRINT *,(MZATAB(K2,K1),K2=1,4),ENINC(K1),ENEXT(K1)
!...  END DO
!...
!...  PRINT *,'MF,MT,KZATAB,EINC,EEXT',MF,MT,KZATAB,EINC,EEXT
!...
   endif
!  ----INITIALIZE ALL METASTABLE STATE FLAGS.
   MSTar1 = blank1
   MSTar2 = blank1
   msta1x = MSTat1
   msta2x = MSTat2
   msta1r = blank1
   msta2r = blank1
!  ----INITIALIZE STATUS AND CENTER-OF-MASS FLAG.
   STAtus = blank1
   IMNorm = 0
   LABcm = blank1
!
!  DECIDE WHETHER OR NOT TO REWIND COMPUTATION FORMAT FILE BASED
!  ON CURRENT POSITION OF EXFOR DATA FILE VS. INDEX TO FIRST DATA
!  POINT OF REQUIRED ZA/MF/MT.
!
   if(iline<mzatab(5,kzatab))then
   elseif(iline==mzatab(5,kzatab))then
!
!     SET UP LOOP TO READ ALL COMPARABLE DATA POINTS.
!
!     ----SET FLAG TO SHOW THAT NEXT POINT IS IN CORE.
      iexpas = 1
      goto 180
   else
!     ----RE-INITIALIZE LINE COUNT AND REWIND.
      iline = 0
      rewind ITApe1
   endif
!  ----SKIP FORWARD TO REQUESTED DATA.
   ii = mzatab(5,kzatab) - 1
   if(ii==iline)then
   else
      iline = iline + 1
      do i = iline,ii
         read(ITApe1,2010,err=160)dummy
  160 enddo
      iline = ii
   endif
  180 continue
   do jex = 1,MAXie
      IEX = jex
!...
!...  print *,'JEX,IEX,IEXPAS',JEX,IEX,IEXPAS
!...
!     ----SKIP READ IF NEXT POINT IS ALREADY IN CORE.
      if(iexpas==1)goto 200
!     ----INCREMENT LINE COUNT AND READ NEXT POINT.
  190 continue
      iline = iline + 1
      read(ITApe1,2000,end=520,err=190)iprojx,izax,msta1x,mfx,mtx,msta2x,statx,labx,fieldc,im78x,REF1
!
!     USE INDEX TO LAST POINT OF ZA/MF/MT TO DETERMINE WHEN TO STOP
!     READING.
!
  200 continue
      iexpas = 0
      if(iline>mzatab(6,kzatab))goto 520
!
!     IMMEDIATELY SKIP DATA WHICH CANNOT BE PLOTTED.
!
!     NON-MATCHING DISCRETE STATES (GROUND AND METASTABLE)
!...
!...  print *,'MSTA2X,MSTAT2"',MSTA2X(1:1),'"',MSTAT2(1:1),'"',IEX
!...
!
      if(msta2x(1:1)==MSTat2(1:1))then
!        SPECIAL DATA TYPES AND RATIOS
         mfsavx = mfx
         if(mfx==10)mfsavx = 3
         if(mfx==203)mfsavx = 3
         if(mfx==402)mfsavx = 3
         if(mfx==154)mfsavx = 7
         if(mfx==801)mfsavx = 8
         if(mfsavx<1.or.mfsavx>10)goto 190
!
!        TRANSLATE 8 FIELDS FROM CHARACTER TO FLOATING POINT.
!
         do i = 1,8
            call FLOAT9(fieldc(1,i),fieldi(i))
         enddo
!        BY DEFAULT OUTGOING PARTICLE IS THE SAME AS THE PROJECTILE
         if(mfx==6.and.fieldi(6)==0)fieldi(6) = iprojx + 0.9
!
!        SKIP POINT IF IT IS NOT REQUESTED (EVEN THOUGH INDEX TO EXFOR HAS
!        ALREADY BEEN MADE THIS TEST IS NECESSARY TO COLLECT TOGETHER
!        REQUESTED POINTS WHICH ARE MIXED IN WITH POINTS WHICH HAVE NOT
!        BEEN REQUESTED, E.G. MULTI-DIMENSIONAL TABLE WHERE SUCCESSIVE
!        POINTS CAN BE FOR DIFFERENT REACTIONS).
!
         call RQEX(kget,izax,mfx,mtx,fieldi(1))
         if(kget<=0)goto 190
!
!        IF ONLY PLOTTING EXFOR DATA, IF NECESSARY TRANSLATE ADDITIONAL
!        ZA AND MT (PRODUCTION OR RATIO).
!
         mttry = 0
         izatry = 0
!        ----FOR CROSS SECTION RATIOS (MF=203) DEFINE DENOMINATOR ZA AND MT.
         if(mfx/=203)then
!           ----FOR PARTICLE/ISOTOPE PRODUCTION (MT=9000-9999) CROSS SECTIONS OR
!           ----ANGULAR DISTRIBUTIONS (MF=3 OR 4) DEFINE PRODUCT ZA.
            if(mtx<9000)then
            else
               izatry = fieldi(6)
!...           IZARAT=IZATRY
!              ----SET DATA FIELD TO ZERO TO AVOID ITS BEING INTERPRETED AS
!              DATA.
               fieldi(6) = 0.0
            endif
         else
            mttry = fieldi(5)
            izatry = fieldi(6)
!           ----DEFINE DEMONINATOR METASTABLE STATE FLAG (CONVERT 9-TH CHARACTER
!           ----OF 6-TH FIELD FROM INTEGER TO METASTABLE FLAG).
            call META10(fieldc(3,6),msta1r)
!           ----DEFINE DEMONINATOR PRODUCT METASTABLE STATE FLAG (CONVERT 9-TH
!           ----CHARACTER OF 5-TH FIELD FROM INTEGER TO METASTABLE FLAG).
            call META10(fieldc(3,5),msta2r)
!           ----SET DATA FIELDS TO ZERO TO AVOID THEIR BEING INTERPRETED AS
!           DATA.
            fieldi(5) = 0.0
            fieldi(6) = 0.0
         endif
!
!        COMPARE,
!        (1) CURRENT ZA/MF/MT TO REQUIRED ZA/MF/MT.
!        (2) IF NOT CROSS SECTIONS INCIDENT ENERGY
!
         MFIn = MF
         if(MFIn==6.and.mfx==5)MFIn = mfx
!...
!...     PRINT *,'IPROJT,IPROJX',IPROJT,IPROJX
!...     PRINT *,'IZA,IZAX',IZA,IZAX
!...     PRINT *,'MFIN,MFX,MFSAVE',MFIN,MFX,MFSAVE
!...     PRINT *,'MTRAT,MTTRY',MTRAT,MTTRY
!...     PRINT *,'IZARAT,IZATRY',IZARAT,IZATRY,' iex',IEX
!...
         if(iprojt/=iprojx.or.IZA/=izax.or.MFIn/=mfx.or.MT/=mtx.or.MTRat/=mttry.or.IZArat/=izatry)then
         else
!           ----SAME ZA/MF/MT. IF NOT MF=3 CHECK INCIDENT ENERGY.
            if(mfsave==3)goto 250
!...
            de1 = ABS(fieldi(1)-EINc)
            ts1 = etol*EINc
!
!...        PRINT *,'        DE1,TS1',FIELDI(1),EINC,DE1,TS1
!
            if(de1<=ts1)goto 250
         endif
!...     IF(ABS(FIELDI(1)-EINC).LT.ETOL*EINC) GO TO 250
!...     IF(FIELDI(1).EQ.EINC) GO TO 250
!
!        CURRENT POINT CANNOT BE INCLUDED ON CURRENT PLOT (E.G., NEW ZA/MF/
!        MT OR OTHER PARAMETER CHANGED).
!
!        ----IF PLOTTING ALL REFERENCES TOGETHER CONTINUE READING
         if(MYMode==1.or.MYMode==3)goto 190
!        ----IF NO POINTS FOUND YET CONTINUE READING.
         if(IEX==1)goto 190
         goto 530
!        ----RETURN CURRENT POINTS READ.
!
!        IF ONLY PLOTTING EXFOR DATA SAVE PARAMETERS WHEN FIRST POINT IS
!        READ.
!
  250    continue
         if(MYMode>=2.or.IEX>1)then
         else
            ILIb = 16
            LIBnam(3) = REF1(8)
            LIBnam(4) = REF1(9)
            STAtus = statx
            IMNorm = 0
            if(STAtus/=ustat)then
            else
               STAtus = blank1
               IMNorm = 1
            endif
            LABcm = labx
            MSTat1 = msta1x
            MSTat2 = msta2x
            MSTar1 = msta1r
            MSTar2 = msta2r
            IM78 = im78x
         endif
!
!        TEST FOR NON-BLANK E2 FIELD. DEFINE E2.
!
         IMAm78 = 0
         do i = 1,3
            if(fieldc(i,7)/=blank)IMAm78 = 1
         enddo
         E2(IEX) = fieldi(7)
!        ----CONVERT ELASTIC CM DISTRIBUTION TO LAB
         if(MYMode<2.or.(mfx/=4.and.mtx/=2))then
         elseif(LABcm==blank1)then
         else
            ein = fieldi(1)
            ain = fieldi(5)
            qq = 0
            gam = SQRT(ein/(ein*AWR*AWR+qq*AWR*(AWR+1)))
            dmc = 2*gam*ain + SQRT(1-gam*gam*(1-ain*ain)) + (gam*ain)**2/SQRT(1-gam*gam*(1-ain*ain))
            fieldi(3) = fieldi(3)*dmc
            fieldi(5) = (ain+gam)/SQRT(1+gam*(gam+2*ain))
            LABcm = blank1
         endif
!
!        SAVE ALL REFS AND DEFINE POINT INDEX TO REF.
!
         if(IREf<=0)then
         else
            do i = 1,IREf
               kref = i
               imsame = 0
               do j = 1,9
                  if(REFs(j,kref)/=REF1(j))goto 300
               enddo
               imsame = 1
!              ----OLD REFERENCE. FOR CROSS SECTIONS AND RATIO TEST FOR CHANGE
!...           IN E2.
!...           PRINT *,'MF,MT,MSAME,E2,E2T',MF,MT,IMSAME,E2(IEX),E2T(KREF)
!...           1       ,KGROUP,MFSAVE
!...
               if(mfsave/=3)goto 340
!              ----TEST STATE DESIGNATION IF PLOTTING SIMPLE TABULATED DATA
!...
               de2 = ABS(E2(IEX)-E2T(kref))
               ts2 = e2tol*E2T(kref)
               if(de2<=ts2)goto 340
!...           IF(ABS(E2(IEX)-E2T(KREF)).LE.E2TOL*E2T(KREF)) GO TO 340
!...           IF(E2T(KREF)-E2(IEX)) 300,340,300
  300       enddo
!...
!...        print *,'New ref, kg,ms,mod',kgroup,imsame,mymode
!...
!           ----NEW REFERENCE AND/OR E2. IF IDENTIFYING E2 CONTINUE TO READ AS
!           ----LONG AS REFERENCE HAS NOT CHANGED. OTHERWISE RETURN.
            if(KGRoup==0)then
            elseif(imsame==1)then
               goto 320
            endif
!           ----NOT IDENTIFYING E2. RETURN IF PLOTTING EACH REFERENCE
!           SEPERATELY.
            if(MYMode==0.or.MYMode==2)goto 530
!           ----COLLECTING REFERENCES. ONLY ALLOW ONE E2 VALUE FOR MF 3.
            if(mfsave/=3)then
            else
!...
!...           print *,'e2,e2t,dif,crit',E2(IEX),E2T(1)
!...           1       ,(E2(IEX)-E2T(1)),E2TOL*E2T(1)
!
               de2 = ABS(E2(IEX)-E2T(1))
               ts2 = e2tol*E2T(1)
               if(de2>ts2)goto 530
            endif
         endif
!...     IF(ABS(E2(IEX)-E2T(1)).GT.E2TOL*E2T(1)) GO TO 530
!...     IF(E2T(1).NE.E2(IEX)) GO TO 530
  320    continue
         if(IREf>=MAXref)goto 530
         IREf = IREf + 1
!        ----IF MORE THAN MAXIMUM NUMBER OF REFERENCES IDENTIFY ALL OTHER
!        ----REFERENCES ONLY AS OTHERS.
!        ----SAVE E2 AND REFERENCE.
         E2T(IREf) = E2(IEX)
         do j = 1,9
            REFs(j,IREf) = REF1(j)
         enddo
         kref = IREf
!        ----DEFINE DATA POINT INDEX TO REFERENCE.
  340    continue
         NREf(IEX) = kref
!
!        SAVE DATA POINT ACCORDING TO TYPE OF DATA.
!
         mfm2 = mfsave - 2
         YEX(IEX) = fieldi(3)
         DYEx(IEX) = ABS(fieldi(4))
         select case(mfm2)
         case(2)
!           ----ANGULAR DISTRIBUTION - SEPARATE OUT DIFFERENT LEVELS
!...
!...        410 print *,'e2,e2t,dif,test'
!...        1       ,E2(IEX),E2T(1),(E2(IEX)-E2T(1)),E2TOL*E2T(1)
!...
!...        IF(ABS(E2(IEX)-E2T(1)).GT.E2TOL*E2T(1)) GO TO 530
            de2 = ABS(E2(IEX)-E2T(1))
            ts2 = e2tol*E2T(1)
            if(de2>ts2)goto 530
!...        410 IF(ABS(E2(IEX)-E2T(1)).GT.E2TOL*E2T(1)) GO TO 530
!...
            XEX(IEX) = fieldi(5)
            DXEx(IEX) = fieldi(6)
         case(3)
!           ----ENERGY DISTRIBUTION
            XEX(IEX) = fieldi(7)
            DXEx(IEX) = fieldi(8)
         case(4)
!           ----DOUBLE DIFFERENTIAL. ALLOW FOR EITHER CONSTANT COSINE OR E.
            if(IEX>1)then
!              ----FOR DOUBLE DIFFERENTAL DATA COLLECT EITHER CROSS SECTION VS.
!              ----ANGLE (CONSTANT SECONDARY ENERGY) OR CROSS SECTION VS.
!              SECONDARY ----ENERGY (COSTANT ANGLE) BASED ON WHICHEVER CHANGES
!              BEWTEEEN FIRST ----TWO POINTS READ.
               if(IEX>2)then
               else
                  if(FIEld4(1)/=fieldi(5).and.FIEld4(3)/=fieldi(7))goto 530
                  if(FIEld4(1)==fieldi(5))IDOub = 2
                  if(IDOub==1)then
                  else
                     XEX(1) = FIEld4(3)
                     DXEx(1) = FIEld4(4)
                  endif
               endif
               if(IDOub==2)then
                  if(FIEld4(1)/=fieldi(5))goto 530
                  XEX(IEX) = fieldi(7)
!
!...              print *,'   iex,e,x',iex,xex(iex),dxex(iex),' going to 510'
!
                  DXEx(IEX) = fieldi(8)
               else
                  if(FIEld4(3)/=fieldi(7))goto 530
                  XEX(IEX) = fieldi(5)
                  DXEx(IEX) = fieldi(6)
               endif
            else
               IDOub = 1
               FIEld4(1) = fieldi(5)
               FIEld4(2) = fieldi(6)
               FIEld4(3) = fieldi(7)
               FIEld4(4) = fieldi(8)
               XEX(IEX) = FIEld4(1)
               DXEx(IEX) = FIEld4(2)
            endif
         case(5)
!           ----LEGENDRE COEFFICIENTS.
            XEX(IEX) = fieldi(5)
            DXEx(IEX) = 0.0
         case(6)
!           ----FISSION YIELD. WHEN FIRST DATA POINT IS READ INITIALIZE
!           ----CUMULATIVE YIELD TABLE AND STORE FIRST VALUE.
            if(IEX>1)then
!              ----FISSION YIELD. ADD ATOMIC WEIGHT YIELD TO TABLE.
               iatwt = fieldi(6)
               YEX(iatwt) = YEX(iatwt) + fieldi(3)
               DYEx(iatwt) = DYEx(iatwt) + fieldi(4)**2
               if(iatwt<mex1)mex1 = iatwt
               if(iatwt>mex2)mex2 = iatwt
            else
               do m = 1,400
                  YEX(m) = 0.0
                  DYEx(m) = 0.0
               enddo
               iatwt = fieldi(6)
               YEX(iatwt) = fieldi(3)
               DYEx(iatwt) = fieldi(4)**2
               mex1 = iatwt
               mex2 = iatwt
            endif
         case default
!           ----BASED ON FIRST TWO POINT SELECT E VS. CROSS SECTION (CONSTANT
!           E2) ----OR E2 VS. CROSS SECTION (CONSTANT E).
            if(IEX>1)then
               if(IEX>2)then
!                 ----CANNOT USE E2 IF FIELD IS BLANK.
               elseif(IMAm78<=0)then
!                 ----CANNOT USE E2 IF FIELD IS HALF-LIFE
               elseif(IM78==hl)then
!                 ----TEST FOR CONSTANT E FIELD.
               elseif(XEX(1)/=fieldi(1))then
!                 ----CONSTANT E. TEST FOR CONSTANT E2 FIELD.
               elseif(FIEld4(1)/=fieldi(7))then
!                 ----CONSTANT E. CHANGE TO E2 VS. CROSS SECTION.
                  IDOub = 2
                  E2(1) = XEX(1)
                  E2T(1) = XEX(1)
                  XEX(1) = FIEld4(1)
                  DXEx(1) = FIEld4(2)
               endif
               if(IDOub==2)then
!                 ----CROSS SECTION...E2 VS. CROSS SECTION.
                  XEX(IEX) = fieldi(7)
                  DXEx(IEX) = fieldi(8)
                  E2(IEX) = fieldi(1)
               else
!                 ----CROSS SECTION. E VS. CROSS SECTION.
                  XEX(IEX) = fieldi(1)
                  DXEx(IEX) = fieldi(2)
               endif
            else
!              ----INITIALIZE ASSUMING E VS. CROSS SECTION.
               IDOub = 1
               FIEld4(1) = fieldi(7)
               FIEld4(2) = fieldi(8)
               XEX(IEX) = fieldi(1)
               DXEx(IEX) = fieldi(2)
            endif
         end select
      else
         if(msta2x(1:1)>MSTat2(1:1))mor10 = 1
         goto 190
      endif
!
!     READ UP TO MAXIMUM POINTS.
!
   enddo
   goto 540
  520 continue
   ipass = ipass + 1
!
!  ALL EXFOR DATA READ.
!
  530 continue
   IEX = IEX - 1
!  ----SAVE COUNT OF ACTUAL POINTS READ (TO DECREMENT INDEX).
  540 continue
   kex = IEX
   if(IEX<=0)then
   elseif(MF/=801)then
!
!     INSURE ERRORS ARE NON-NEGATIVE. SET TO ZERO IF ERROR WILL NOT BE
!     PLOTTED.
!
      do i = 1,IEX
         DXEx(i) = ABS(DXEx(i))
         if(IXErr<=0)DXEx(i) = 0.0
         DYEx(i) = ABS(DYEx(i))
         if(IYErr<=0)DYEx(i) = 0.0
      enddo
   else
!     ----DEFINE ATOMIC WEIGHT RANGE FOR FISSION YIELD.
      IEX = 0
      do m = mex1,mex2
         if(YEX(m)<=0.0)then
         else
            IEX = IEX + 1
            XEX(IEX) = m
            DXEx(IEX) = 0.0
            YEX(IEX) = YEX(m)
            DYEx(IEX) = SQRT(DYEx(m))
         endif
      enddo
   endif
!...
!  ----DO NOTHING IF MF=10 AND MORE STATES EXIST
!...  IF(MF.EQ.10 .AND. MOR10.NE.0) GO TO 582
!...
!  ----IF LESS THAN TWO REFERENCES TURN OFF IDENTIFY E2 FLAG.
   if(IREf<2)KGRoup = 0
!
!  UPDATE EXFOR INDEX TABLE TO PREVENT POINTS BEING PLOTTED TWICE
!
!  ----RESET LOWER LINE INDEX AND REMAINING POINT COUNT.
   mzatab(5,kzatab) = iline
   mzatab(7,kzatab) = mzatab(7,kzatab) - kex
!  ----IF MORE POINTS WITH SAME ZA/MF/MT TO PLOT RESET INDEX TO TABLE.
   if(mzatab(7,kzatab)<MINnie.or.mzatab(5,kzatab)>mzatab(6,kzatab))then
!     ----ELIMINATE INDEX FROM TABLE BY SHIFTING ALL FOLLOWING ENTRIES ONE
!     ----LOCATION FORWARD IN TABLE.
      if(nzatab<=1.or.kzatab==nzatab)then
      else
         ii = kzatab + 1
         do i = ii,nzatab
            do k = 1,9
               mzatab(k,i-1) = mzatab(k,i)
            enddo
            eninc(i-1) = eninc(i)
            enext(i-1) = enext(i)
         enddo
         kzatab = kzatab - 1
      endif
      nzatab = nzatab - 1
!     ----SET END OF FILE FLAG IF NO EXFOR DATA LEFT TO PLOT.
      if(nzatab<=0)Iend = 2
      return
   else
      kzatab = kzatab - 1
      return
   endif
!
!  END OF EXFOR PLOTTING.
!
  630 continue
   Iend = 2
   return
 2000 format(i5,i6,a1,i3,i4,3A1,8(2A4,a1),a3,6A4,a1,2A4)
 2010 format(a1)
 6000 format(' MAX.NO. OF REACTIONS REACHED...REMAINING SKIPPED')
   end subroutine GETEX
   subroutine FLOAT9(Field,X)
   implicit none
!*--FLOAT95368
!
!
! COMMON variables
!
   integer :: INP,ITApe1,ITApe2,OUTp
   common /UNITS / INP,OUTp,ITApe1,ITApe2
!
! Dummy arguments
!
   real :: X
   character(1),dimension(9) :: Field
   intent (in) Field
   intent (inout) X
!
! Local variables
!
   character(1) :: blank,dot,expd,expe,ifield,minus,plus,star
   character(1),dimension(10) :: digit
   integer :: i,in,ipt,j,jc,k,kexp,ksign
   character(1),dimension(9) :: mess
   real,dimension(35) :: ten
   real :: xsign,zero
!
!
!
!  CONVERT FROM 9 HOLLERITH CHARACTERS TO FLOATING POINT.
!  MUST BE BETWEEN 1.0E-40 AND 1.0E+40.
!
   data blank/' '/
   data dot/'.'/
   data expd/'D'/
   data expe/'E'/
   data plus/'+'/
   data minus/'-'/
   data star/'*'/
   data mess/' ',' ',' ',' ',' ',' ',' ',' ',' '/
   data digit/'0','1','2','3','4','5','6','7','8','9'/
   data zero/0.0E+00/
   data ten/1.0E+01,1.0E+02,1.0E+03,1.0E+04,1.0E+05,1.0E+06,1.0E+07,1.0E+08,1.0E+09,1.0E+10,1.0E+11,1.0E+12,1.0E+13,1.0E+14,      &
      & 1.0E+15,1.0E+16,1.0E+17,1.0E+18,1.0E+19,1.0E+20,1.0E+21,1.0E+22,1.0E+23,1.0E+24,1.0E+25,1.0E+26,1.0E+27,1.0E+28,1.0E+29,  &
      & 1.0E+30,1.0E+31,1.0E+32,1.0E+33,1.0E+34,1.0E+35/
!
!  TRANSLATE MANTISSA.
!
!  ----SKIP LEADING BLANK CHARACTERS.
   do i = 1,9
      if(Field(i)/=blank)goto 20
   enddo
!  ----FIELD IS COMPLETELY BLANK. RETURN ZERO.
   X = zero
   goto 240
!  ----INITIALIZE FIXED POINT INPUT FIELD AND POSITION OF DECIMAL POINT.
   20 continue
   in = 0
   ipt = -20
!  ----ALLOW LEADING SIGN.
   if(Field(i)==minus)then
      i = i + 1
      xsign = -1.0
   else
      if(Field(i)/=plus)then
      else
         i = i + 1
      endif
      xsign = 1.0
   endif
!  ----SCAN REMAINDER OF MANTISSA.
   do j = i,9
      jc = j
      ifield = Field(j)
!     ----SCAN FOR DIGIT OR DECIMAL POINT (WHICH ARE PART OF MANTISSA).
      do k = 1,10
         if(ifield==digit(k))goto 80
      enddo
      if(ifield/=dot)then
!        ----SCAN FOR BLANK (WHICH ENDS MANTISSA).
         if(ifield==blank)goto 100
!        ----SCAN FOR E,D,- OR + (WHICH BEGINS EXPONENT).
         if(ifield==expe.or.ifield==expd)goto 130
         if(ifield==minus)goto 160
         if(ifield==plus)goto 140
         goto 250
!        ----ERROR. CANNOT IDENTIFY CHARACTER.
      else
         ipt = 0
         cycle
      endif
!     ----DIGIT FOUND. INCREMENT FIXED POINT EQUIVALENT AND DECIMAL POINT
!     ----OFFSET.
   80 continue
      in = 10*in + (k-1)
      ipt = ipt + 1
   enddo
!  ----ENTIRE FIELD TRANSLATED (NO EXPONENT). CONVERT TO FLOATING POINT.
   goto 120
!  ----BLANK FOUND (END OF MANTISSA). SCAN REMAINDER OF FIELD FOR
!  ----EXPONENT.
  100 continue
   i = jc + 1
   if(i>9)then
   else
      do j = i,9
         jc = j
         ifield = Field(j)
         if(ifield==blank)then
         else
            if(ifield==expe.or.ifield==expd)goto 130
            if(ifield==minus)goto 160
            if(ifield==plus)goto 140
            goto 250
!           ----ERROR. CANNOT IDENTIFY CHARACTER.
         endif
      enddo
   endif
!  ----ENTIRE FIELD TRANSLATED (NO EXPONENT). CONVERT TO FLOATING POINT.
  120 continue
   X = in
   if(ipt>0)X = X/ten(ipt)
   goto 230
!
!  TRANSLATE EXPONENT.
!
!  ----BEGINNING OF EXPONENT FOUND (X OR D). CHECK FOR FOLLOWING - OR +.
  130 continue
   j = j + 1
   ifield = Field(j)
   if(ifield==minus)goto 160
   if(ifield/=plus)goto 150
!  ---- + FOUND. INITIALIZE EXPONENT SIGN.
  140 continue
   j = j + 1
  150 continue
   ksign = 1
   goto 170
!  ---- - FOUND. INITIALIZE EXPONENT SIGN.
  160 continue
   j = j + 1
   ksign = -1
!  ----INITIALIZE EXPONENT AND SCAN REMAINING CHARACTERS FOR EXPONENT.
  170 continue
   kexp = 0
   do i = j,9
      jc = i
      ifield = Field(i)
      if(ifield==blank)cycle
      do k = 1,10
         if(ifield==digit(k))goto 190
      enddo
!     ----ERROR. CANNOT IDENTIFY CHARACTER.
      goto 250
!     ----DIGIT FOUND. INCREMENT EXPONENT.
!     ----OFFSET.
  190 continue
      kexp = 10*kexp + (k-1)
   enddo
!  ----ENTIRE FIELD TRANSLATED (WITH EXPONENT). CONVERT TO FLOATING
!  ----POINT.
   X = in
   kexp = ksign*kexp
   if(ipt>0)kexp = kexp - ipt
   if(kexp<0)then
      kexp = -kexp
      X = X/ten(kexp)
   elseif(kexp==0)then
   else
      X = X*ten(kexp)
   endif
  230 continue
   X = xsign*X
  240 continue
   return
  250 continue
   mess(jc) = star
   write(OUTp,6000)Field,mess
   X = zero
   mess(jc) = blank
   return
 6000 format(/1x,' STRING "',9A1,'"'/1x,' COLUMN  ',9A1/' SUBROUTINE FLOAT9...ERROR IN INPUT DATA...TRANSLATED AS 0.0')
   end subroutine FLOAT9
   subroutine RQEX(Kget,Iza,Mf,Mt,En)
   implicit none
!*--RQEX5553
!
!
! COMMON variables
!
   real,dimension(100) :: EHGet,ELGet,EPGet
   real :: EP6
   integer :: IGEt,NGEt
   integer,dimension(100) :: INTrn,IZAhi,IZAlow,MFHi,MFLow,MTHi,MTLow
   common /EPSMF6/ EP6
   common /GETEM / IZAlow,MFLow,MTLow,ELGet,IZAhi,MFHi,MTHi,EHGet,EPGet,INTrn,NGEt,IGEt
!
! Dummy arguments
!
   real :: En
   integer :: Iza,Kget,Mf,Mt
   intent (in) En,Iza,Mf,Mt
   intent (inout) Kget
!
!
!
!  COMPARE CURRENT EXFOR ZA/MF/MT/INCIDENT ENERGY TO REQUESTS.
!
   do Kget = 1,NGEt
      if(Iza<IZAlow(Kget).or.Iza>IZAhi(Kget))then
      elseif(Mf<MFLow(Kget).or.Mf>MFHi(Kget))then
      elseif(Mt<MTLow(Kget).or.Mt>MTHi(Kget))then
      elseif(En>=ELGet(Kget).and.En<=EHGet(Kget))then
         goto 20
      endif
   enddo
   Kget = 0
!* RESOLUTION BROADENING PARAMETER
   20 continue
   if(EPGet(Kget)>0.and.EPGet(Kget)<0.1)then
      EP6 = EPGet(Kget)
   else
      EP6 = 0.02
   endif
   return
   end subroutine RQEX
   subroutine ZAMFMT
   implicit none
!*--ZAMFMT5599
!
!
! COMMON variables
!
   integer,dimension(4,100) :: IMFtab
   integer,dimension(3,300) :: IMTtab
   integer :: INP,ITApe1,ITApe2,MFLong,MTLong,MZLong,NTApe1,NTApe2,NTApe3,OUTp
   integer,dimension(200) :: IZAtab
   character(4),dimension(8,100) :: MFTab
   character(4),dimension(10,300) :: MTTab
   character(4),dimension(3,200) :: ZATab
   common /TABMFC/ MFTab
   common /TABMFI/ MFLong,IMFtab
   common /TABMTC/ MTTab
   common /TABMTI/ MTLong,IMTtab
   common /TABZAC/ ZATab
   common /TABZAI/ MZLong,IZAtab
   common /UNITS / INP,OUTp,ITApe1,ITApe2
   common /UNITT / NTApe1,NTApe2,NTApe3
!
! Local variables
!
   integer :: k,mfmax,mtmax,mzmax
!
!
!
!  READ ALL,
!  (1) SPECIAL ZA TITLES (IZA LESS THAN 1000)
!  (2) MF TITLES
!  (3) MT TITLES
!
!  WARNING....THIS ROUTINE MUST BE CALLED BEFORE TRYING TO DEFINE
!  ANY ZA/MF/MT EQUIVALENCES.
!
!  ----DEFINE TABLE SIZES.
   data mzmax/200/
   data mfmax/100/
   data mtmax/300/
!  ----INDICATE PROGRAM IS READING TRANSLATION TABLES.
   write(OUTp,6000)
 6000 format(' READING TRANSLATION TABLES'/1x,72('='))
!
!  READ SPECIAL ZA TITLES.
!
   do MZLong = 1,mzmax
      read(NTApe1,1200,end=20,err=30)IZAtab(MZLong),(ZATab(k,MZLong),k=1,3)
 1200 format(i5,1x,3A4)
   enddo
   MZLong = mzmax
   goto 40
!  ----END OF REACTIONS.
   20 continue
   MZLong = MZLong - 1
   if(MZLong>0)goto 40
!  ----ERROR READING.
   30 continue
   write(OUTp,6040)
 6040 format(' ERROR READING SPECIAL ZA TITLES...EXECUTION TERMINATED')
   goto 140
   40 continue
   write(OUTp,6010)MZLong,mzmax
 6010 format(' SPECIAL ZA TITLES----------',i5,'(',i5,' ALLOWED)')
!
!  READ MF TITLES
!
   do MFLong = 1,mfmax
      do while (.true.)
         read(NTApe2,1400,end=70,err=80)IMFtab(2,MFLong),IMFtab(3,MFLong),IMFtab(4,MFLong),(MFTab(k,MFLong),k=1,8)
 1400    format(3I5,1x,8A4)
         if(IMFtab(2,MFLong)<=0)then
         else
            call SIZER(MFTab(1,MFLong),IMFtab(1,MFLong),32)
            exit
         endif
      enddo
   enddo
   MFLong = mfmax
   goto 90
!  ----END OF REACTIONS.
   70 continue
   MFLong = MFLong - 1
   if(MFLong>0)goto 90
!  ----ERROR READING.
   80 continue
   write(OUTp,6050)
   goto 140
   90 continue
   write(OUTp,6020)MFLong,mfmax
 6020 format(' MF TITLES------------------',i5,'(',i5,' ALLOWED)')
!
!  READ MT TITLES
!
   do MTLong = 1,mtmax
      read(NTApe3,1500,end=110,err=120)IMTtab(2,MTLong),IMTtab(3,MTLong),(MTTab(k,MTLong),k=1,10)
 1500 format(2I5,1x,10A4)
      call SIZER(MTTab(1,MTLong),IMTtab(1,MTLong),40)
   enddo
   MTLong = mtmax
   goto 130
!  ----END OF REACTIONS.
  110 continue
   MTLong = MTLong - 1
   if(MTLong>0)goto 130
!  ----ERROR READING.
  120 continue
   write(OUTp,6050)
   goto 140
  130 continue
   write(OUTp,6030)MTLong,mtmax
 6030 format(' MT TITLES------------------',i5,'(',i5,' ALLOWED)')
   return
 6060 format(' ERROR READING MT TITLES...EXECUTION TERMINATED')
  140 continue
   stop
 6050 format(' ERROR READING MF TITLES...EXECUTION TERMINATED')
   end subroutine ZAMFMT
   subroutine ZAHOL(Za,Mstat1,Zabcd,Izabcd)
   implicit none
!*--ZAHOL5721
!
!
! Dummy arguments
!
   integer :: Izabcd,Za
   character(1) :: Mstat1
   character(4),dimension(4) :: Zabcd
!
! Local variables
!
   integer :: a,i,z
   character(4) :: blank4
   character(4),dimension(54) :: dum1
   character(4),dimension(56) :: dum2
   character(4),dimension(110) :: zatab
!
!
!
!  GIVEN ANY ZA = (1000*Z+A) THIS ROUTINE WILL DEFINE A THREE WORD
!  EQUIVALENT IN ONE OF THE TWO FOLLOWING FORMS.
!
!  IF Z IS GREATER THAN ZERO....ZZZ-SS-AAAM
!  E.G., 26056-M = 26-FE-56M
!
!  IF Z IS EQUAL TO ZERO........3A4 CHARACTER EQUIVALENT
!  E.G., ZA=302 = ZIRCALLOY-2
!
   equivalence(zatab(1),dum1(1))
   equivalence(zatab(55),dum2(1))
   data dum1/'-H -','-HE-','-LI-','-BE-','-B -','-C -','-N -','-O -','-F -','-NE-','-NA-','-MG-','-AL-','-SI-','-P -','-S -',     &
       &'-CL-','-AR-','-K -','-CA-','-SC-','-TI-','-V -','-CR-','-MM-','-FE-','-CO-','-NI-','-CU-','-ZN-','-GA-','-GE-','-AS-',   &
       &'-SE-','-BR-','-KR-','-RB-','-SR-','-Y -','-ZR-','-NB-','-MO-','-TC-','-RU-','-RH-','-PD-','-AG-','-CD-','-IN-','-SN-',   &
       &'-SB-','-TE-','-I -','-XE-'/
   data dum2/'-CS-','-BA-','-LA-','-CE-','-PR-','-ND-','-PM-','-SM-','-EU-','-GD-','-TB-','-DY-','-HO-','-ER-','-TM-','-YB-',     &
       &'-LU-','-HF-','-TA-','-W -','-RE-','-OS-','-IR-','-PT-','-AU-','-HG-','-TL-','-PB-','-BI-','-PO-','-AT-','-RN-','-FR-',   &
       &'-RA-','-AC-','-TH-','-PA-','-U -','-NP-','-PU-','-AM-','-CM-','-BK-','-CF-','-ES-','-FM-','-MD-','-NO-','-LR-',' ',' ',  &
       &' ',' ',' ',' ','-ERR'/
   data blank4/'    '/
   do i = 1,4
      Zabcd(i) = blank4
   enddo
!  ----SPECIAL HANDLING FOR SPECIAL ENDF/B MATERIALS.
   if(Za>=1000)then
!     ----INSURE Z IS IN LEGAL RANGE.
      z = Za/1000
      a = Za - 1000*z
      if(z<1.or.z>110)z = 110
!     ----PACK ZZZ-SS-AAA INTO CHARACTER FORM.
      call PACKZA(z,a,Mstat1,zatab(z),Zabcd)
   else
      call COMPND(Za,Zabcd)
   endif
!  ----DEFINE LENGTH OF PACKED ZA.
   call SIZER(Zabcd,Izabcd,16)
   return
   end subroutine ZAHOL
   subroutine PACKZA(Z,A,Mstat1,Zatab,Zabcd)
   implicit none
!*--PACKZA5783
!
!
! Dummy arguments
!
   integer :: A,Z
   character(1) :: Mstat1
   character(1),dimension(16) :: Zabcd
   character(1),dimension(4) :: Zatab
   intent (in) Zatab
   intent (inout) A,Z
!
! Local variables
!
   character(1) :: blank,dash
   character(1),dimension(10) :: digits
   integer :: i,ia,iz,j,k,multa,multz
   integer,dimension(3) :: mult
!
!
!
!  PACK ZZZ-SS-AAAM INTO CHARACTER FORM.
!
   data digits/'0','1','2','3','4','5','6','7','8','9'/
   data blank/' '/
   data dash/'-'/
   data mult/1,10,100/
!  ----INITIALIZE CHARACTERS AND INDEX.
   do i = 1,16
      Zabcd(i) = blank
   enddo
   i = 0
!  ----PACK Z.
   j = 3
   if(Z<100)j = 2
   if(Z<10)j = 1
   multz = mult(j)
   do k = 1,j
      iz = Z/multz
      i = i + 1
      Zabcd(i) = digits(iz+1)
      Z = Z - multz*iz
      multz = multz/10
   enddo
!  ----PACK CHEMICAL SYMBOL.
   do k = 1,4
      i = i + 1
      Zabcd(i) = Zatab(k)
   enddo
!  ----PACK A.
   j = 3
   if(A<100)j = 2
   if(A<10)j = 1
   multa = mult(j)
   do k = 1,j
      ia = A/multa
      i = i + 1
      Zabcd(i) = digits(ia+1)
      A = A - multa*ia
      multa = multa/10
   enddo
!  ----IF METASTABLE STATE FLAG IS NOT BLANK ADD IT TO STRING.
   if(Mstat1/=blank)call METAST(Zabcd,Mstat1,i)
   return
   end subroutine PACKZA
   subroutine MTHOL(Mt,Mtbcd,Imtbcd,Mstat2)
   implicit none
!*--MTHOL5853
!
!
! COMMON variables
!
   integer,dimension(3,300) :: IMTtab
   integer :: IZArat,MFIn,MTLong,MTRat
   character(4),dimension(10,300) :: MTTab
   common /RATZA / IZArat,MTRat,MFIn
   common /TABMTC/ MTTab
   common /TABMTI/ MTLong,IMTtab
!
! Dummy arguments
!
   integer :: Imtbcd,Mt
   character(4) :: Mstat2
   character(4),dimension(20) :: Mtbcd
   intent (in) Mt
!
! Local variables
!
   character(4) :: blank
   integer :: kzap,l,m
   character(4),dimension(10) :: unknow
   character(4),dimension(20) :: zapbcd
!
!
!
!  DEFINE HOLLERITH EQUIVALENT OF REACTION (MT).
!
!  DEFINITION OF TABLES
!
!  MTTAB = CHARACTER EQUIVALENT OF EACH MT NUMBER
!  UP TO 40 CHARACTERS PER MT - LEFT ADJUSTED
!  IMTTAB = (1) NUMBER OF CHRACTERS IN EACH CHARACTER EQUIVALENT
!  (2) LOWER MT LIMIT
!  (3) UPPER MT LIMIT
!
!  THE CHARACTER EQUIVALENT IS RETURNED IN MTBCD
!
!  MTBCD  = CHARACTER EQUIVALENT OF MT
!  UP TO 40 CHARACTERS - LEFT ADJUSTED
!  IMTBCD = NUMBER OF CHARACTERS IN CHARACTER EQUIVALENT OF MT
!
   data blank/'    '/
   data unknow/'***U','NDEF','INED','***',' ',' ',' ',' ',' ',' '/
!  ----LOOK UP CHARACTER EQUIVALENT AND LOAD INTO CORE.
   do m = 1,MTLong
      if(Mt<IMTtab(3,m))then
         if(Mt<IMTtab(2,m))exit
         goto 50
      elseif(Mt==IMTtab(3,m))then
         goto 50
      endif
   enddo
!  ----MT NUMBER NOT DEFINED. LOAD **UNDEFINED**
   do l = 1,10
      Mtbcd(l) = unknow(l)
   enddo
   Imtbcd = 15
   return
!  ----MT NUMBER IS DEFINED.
   50 continue
   do l = 1,10
      Mtbcd(l) = MTTab(l,m)
   enddo
   Imtbcd = IMTtab(1,m)
!  ----IF MT=9000-9999 (PARTICLE/ISOTOPE PRODUCTION) ADD PRODUCT ZA
!  ----BEFORE MT I.D.
   if(Mt<9000)then
!     ----IF FINAL METASTABLE STATE FLAG IS NOT BLANK ADD IT TO STRING.
      if(Mstat2/=blank)call METAST(Mtbcd,Mstat2,Imtbcd)
   else
      call ZAHOL(IZArat,Mstat2,zapbcd,kzap)
      call PAKZAP(zapbcd,kzap,Mtbcd,Imtbcd)
   endif
   return
   end subroutine MTHOL
   subroutine METAST(Bcd,Mstate,Ibcd)
   implicit none
!*--METAST5936
!
!
! Dummy arguments
!
   integer :: Ibcd
   character(1) :: Mstate
   character(1),dimension(40) :: Bcd
   intent (in) Mstate
   intent (out) Bcd
   intent (inout) Ibcd
!
! Local variables
!
   character(1) :: blank
   integer :: i,j
   character(1),dimension(14) :: mstab1
   character(1),dimension(3,15) :: mstab2
!
!
!
!  ADD METASTABLE STATE FLAG TO ZA OR REACTION.
!
!  ON ENTRY INTO THIS ROUTINE IBCD IS THE LENGTH OF THE CHARACTER
!  STRING DESCRIBING THE ZA AND/OR REACTION. UNLESS THE METASTABLE
!  STATE FLAG IS BLANK IT WILL BE ADDED TO THE STRING.
!
   data blank/' '/
   data mstab1/' ','T','G','M','1','2','3','4','5','6','7','8','9','+'/
   data mstab2/' ',' ',' ',' ',' ',' ','-','G',' ','-','M',' ','-','M','1','-','M','2','-','M','3','-','M','4','-','M','5','-',   &
      & 'M','6','-','M','7','-','M','8','-','M','9','-','M','+','-','M','?'/
!  ----LOOK UP ONE CHARACTER STATE FLAG.
   do i = 1,14
      if(Mstate==mstab1(i))goto 20
   enddo
   i = 15
   20 continue
   do j = 1,3
      if(mstab2(j,i)==blank)exit
      Ibcd = Ibcd + 1
      Bcd(Ibcd) = mstab2(j,i)
   enddo
   return
   end subroutine METAST
   subroutine SIZER(Bcd,Ibcd,Maxbcd)
   implicit none
!*--SIZER5985
!
!
! Dummy arguments
!
   integer :: Ibcd,Maxbcd
   character(1),dimension(Maxbcd) :: Bcd
   intent (in) Bcd,Maxbcd
   intent (inout) Ibcd
!
! Local variables
!
   character(1) :: blank
   integer :: i
!
!
!
!  DEFINE NON-BLANK LENGTH OF CHARACTER STRING.
!
   data blank/' '/
   Ibcd = Maxbcd
   do i = 1,Maxbcd
      if(Bcd(Ibcd)/=blank)exit
      Ibcd = Ibcd - 1
   enddo
   return
   end subroutine SIZER
   subroutine LEFTY(Bcd,Ibcd,Nbcd,Kbcd)
   implicit none
!*--LEFTY6017
!
!
! COMMON variables
!
   character(1),dimension(256) :: CHRtab,CHRtrl
   integer,dimension(5000) :: ICHpen
   integer :: ICHr,ICNtrl,ISYm
   integer,dimension(2,256) :: INDchr
   real,dimension(5000) :: XCHar,YCHar
   common /SYMTB1/ CHRtab,CHRtrl
   common /SYMTB2/ XCHar,YCHar,ICHpen,INDchr,ICHr,ICNtrl,ISYm
!
! Dummy arguments
!
   integer :: Ibcd,Kbcd,Nbcd
   character(1),dimension(Kbcd) :: Bcd
   intent (in) Kbcd
   intent (inout) Bcd,Ibcd,Nbcd
!
! Local variables
!
   character(1) :: blank
   integer :: i,ii,j,k,l
!
!
!
!  LEFT ADJUST TITLES AND DEFINE NUMBER OF NON-BLANK CHARACTERS AND
!  NUMBER OF PLOTTED CHARACTERS (NON-BLANK MINUS CONTROL CHARACTERS).
!
   data blank/' '/
!  ----FIND LAST NON-BLANK CHARACTER.
   i = Kbcd
   do ii = 1,Kbcd
      if(Bcd(i)/=blank)goto 20
      i = i - 1
   enddo
   Ibcd = 0
   Nbcd = 0
   goto 70
!  ----FIND FIRST NON-BLANK CHARACTER.
   20 continue
   do j = 1,i
      if(Bcd(j)/=blank)goto 40
   enddo
   Ibcd = 0
   Nbcd = 0
   goto 70
   40 continue
   Ibcd = i
!  ----IF REQUIRED SHIFT CHARACTERS LEFT.
   if(j==1)then
   else
      k = 0
      do l = j,i
         k = k + 1
         Bcd(k) = Bcd(l)
      enddo
      Ibcd = k
      k = k + 1
      do i = k,Kbcd
         Bcd(i) = blank
      enddo
   endif
!  ----COUNT SPECIAL NON-PRINTING CHARACTERS.
   70 continue
   Nbcd = Ibcd
   if(ICNtrl<=0)then
   else
      do i = 1,Ibcd
         do j = 1,ICNtrl
            if(Bcd(i)/=CHRtrl(j))then
            else
               Nbcd = Nbcd - 1
               exit
            endif
         enddo
      enddo
   endif
   return
   end subroutine LEFTY
   subroutine MFHOL(Mf,Mt,Mfbcd,Imfbcd)
   implicit none
!*--MFHOL6103
!
!
! COMMON variables
!
   integer,dimension(4,100) :: IMFtab
   integer :: MFLong
   character(4),dimension(8,100) :: MFTab
   common /TABMFC/ MFTab
   common /TABMFI/ MFLong,IMFtab
!
! Dummy arguments
!
   integer :: Imfbcd,Mf,Mt
   character(4),dimension(8) :: Mfbcd
   intent (in) Mf,Mt
   intent (out) Imfbcd,Mfbcd
!
! Local variables
!
   character(4),dimension(8) :: error
   integer :: i,k
!
!
!
!  DEFINE HOLLERITH EQUIVALENT OF DATA TYPE (MF).
!
   data error/'*** ','ERRO','R **','*',' ',' ',' ',' '/
!  ----LOOK UP MF AND MT RANGE IN TABLE.
   do i = 1,MFLong
      if(Mf/=IMFtab(2,i))then
      elseif(Mt>=IMFtab(3,i).and.Mt<=IMFtab(4,i))then
         goto 30
      endif
   enddo
!  ----MF AND MT RANGE IS NOT IN TABLE.
   Imfbcd = 13
   do k = 1,8
      Mfbcd(k) = error(k)
   enddo
   return
!  ----DEFINE MF AND MT RANGE EQUIVALENT.
   30 continue
   Imfbcd = IMFtab(1,i)
   do k = 1,8
      Mfbcd(k) = MFTab(k,i)
   enddo
   return
   end subroutine MFHOL
   subroutine PAKZAP(Zapbcd,Kzap,Mtbcd,Imtbcd)
   implicit none
!*--PAKZAP6157
!
!
! Dummy arguments
!
   integer :: Imtbcd,Kzap
   character(1),dimension(80) :: Mtbcd
   character(1),dimension(12) :: Zapbcd
   intent (in) Kzap,Zapbcd
   intent (inout) Imtbcd,Mtbcd
!
! Local variables
!
   character(1) :: blank
   integer :: i,j
   character(1),dimension(80) :: mtpbcd
!
!
!
!  COMBINE PRODUCT ZA WITH MT I.D. FOR PARTICLE/ISOTOPE PRODUCTION.
!
   data blank/' '/
!  ----PACK PRODUCT ZA.
   i = 0
   do j = 1,Kzap
      i = i + 1
      mtpbcd(i) = Zapbcd(j)
   enddo
!  ----INSERT BLANK AND THEN MT I.D.
   i = i + 1
   mtpbcd(i) = blank
   do j = 1,Imtbcd
      i = i + 1
      mtpbcd(i) = Mtbcd(j)
   enddo
!  ----DEFINE NEW MT I.D. LENGTH AND COPY BACK TO MT I.D.
   Imtbcd = i
   do j = 1,Imtbcd
      Mtbcd(j) = mtpbcd(j)
   enddo
   return
   end subroutine PAKZAP
   subroutine COMPND(Za,Zabcd)
   implicit none
!*--COMPND6204
!
!
! COMMON variables
!
   integer,dimension(200) :: IZAtab
   integer :: MZLong
   character(4),dimension(3,200) :: ZATab
   common /TABZAC/ ZATab
   common /TABZAI/ MZLong,IZAtab
!
! Dummy arguments
!
   integer :: Za
   character(4),dimension(4) :: Zabcd
   intent (in) Za
   intent (out) Zabcd
!
! Local variables
!
   character(4) :: blank4
   character(4),dimension(3) :: error
   integer :: i,n
!
!
!
!  DEFINE ZA EQUIVALENCE FOR ENDF/B SPECIAL MATERIALS AND COMPOUNDS.
!
!  ----DEFINE NUMBER OF SPECIAL MATERIALS AND COMPOUNDS IN TABLE.
   data blank4/'    '/
   data error/'** E','RROR',' ** '/
   Zabcd(4) = blank4
!  ----ERROR IF ZA IS NOT LESS THAN 1000.
   if(Za>=1000)then
   else
!     ----LOOK UP ZA IN TABLE.
      do n = 1,MZLong
         if(IZAtab(n)<Za)then
         elseif(IZAtab(n)==Za)then
            goto 40
         else
            exit
         endif
      enddo
   endif
!  ----ZA IS NOT IN TABLE.
   do i = 1,3
      Zabcd(i) = error(i)
   enddo
   return
!  ----MATCH FOUND. DEFINE HOLLERITH EQUIVALENCE.
   40 continue
   do i = 1,3
      Zabcd(i) = ZATab(i,n)
   enddo
   return
   end subroutine COMPND
   subroutine META10(Out,Mstate)
   implicit none
!*--META106266
!
!
! Dummy arguments
!
   character(1) :: Mstate,Out
   intent (in) Out
   intent (out) Mstate
!
! Local variables
!
   integer :: i
   character(1),dimension(10) :: mtab1,mtab2
!
!
!
!  DEFINE CHARACTER EQUIVALENT OF METASTABLE STATE FLAG
!
   data mtab1/'G','1','2','3','4','5','?','M','+',' '/
   data mtab2/'0','1','2','3','4','5','6','7','8','9'/
!  ----LOOK UP METASTABLE STATE CHARACTER.
   do i = 1,10
      if(Out==mtab2(i))goto 20
   enddo
!  ----SET INDEX TO UNKNOWN.
   i = 7
   20 continue
   Mstate = mtab1(i)
   return
   end subroutine META10
   subroutine NUMBRH(Xi,Yi,Ht,Z,Ang,Iz)
   implicit none
!*--NUMBRH6301
!
!
! Dummy arguments
!
   real :: Ang,Ht,Xi,Yi,Z
   integer :: Iz
   intent (in) Iz,Z
!
! Local variables
!
   real :: az,z10
   character(1),dimension(10) :: digit
   character(1) :: dot,minus
   character(1),dimension(16) :: field
   integer :: i,idig,idot,ifield,iiz,iz10,kdig,m10,mr,mr1,ndig,nr
!
!
!
!  CONVERT FLOATING POINT NUMBER OF CHARACTER STRING AND PLOT WIDE.
!
   data digit/'1','2','3','4','5','6','7','8','9','0'/
   data minus/'-'/
   data dot/'.'/
!
!  ROUND-OFF NUMBER TO REQUIRED DIGITS, E.G. IF WRITING 54.321 WITH
!  2 DIGITS AFTER DECIMAL POINT DEFINE MR=5432 (ENTIRE STRING) AND
!  MR1=54 (DIGITS PRECEDING DECIMAL POINT).
!
   az = ABS(Z)
   iiz = Iz
   if(iiz<=0)iiz = 0
   z10 = 10.0**iiz
   mr = az*z10 + 0.5
   iz10 = z10
   mr1 = mr/iz10
!
!  DETERMINE NUMBER OF DIGITS PRECEDING DECIMAL POINT.
!
   m10 = 1
   do idig = 1,12
      nr = mr1/m10
      if(nr<=0)goto 20
      m10 = 10*m10
   enddo
!
!  NUMBER IS TOO BIG...NO PLOTTING.
!
   return
   20 continue
   if(idig==1)then
   else
      idig = idig - 1
      m10 = m10/10
   endif
!
!  ADD DIGITS AFTER DECIMAL POINT.
!
   idig = idig + iiz
   m10 = m10*iz10
!
!  IF NUMBER IS ZERO, PLOT ZERO AND RETURN.
!
   if(idig>0)then
!
!     INITIALIZE CHARACTER COUNT.
!
      ifield = 0
!
!     IF NUMBER IS NEGATIVE INSERT MINUS SIGN.
!
      if(Z>=0.0)then
      else
         field(1) = minus
         ifield = 1
      endif
!
!     DEFINE POSITION OF DECIMAL POINT (IF ANY).
!
      idot = idig - Iz
      if(Iz<0)idot = idig + 2
!
!     INSERT DIGITS AND DECIMAL POINT (IF ANY) IN STRING.
!
      do i = 1,idig
         ndig = mr/m10
         kdig = ndig
         if(kdig==0)kdig = 10
         ifield = ifield + 1
         field(ifield) = digit(kdig)
!
!        INSERT DECIMAL POINT AT APPROPRIATE POSITION.
!
         if(i/=idot)then
         else
            ifield = ifield + 1
            field(ifield) = dot
         endif
         mr = mr - m10*ndig
         m10 = m10/10
      enddo
   else
      ifield = 1
      field(1) = digit(10)
   endif
!
!  ENTIRE FIELD FORMATTED. PLOT IT.
!
   call SYMBLH(Xi,Yi,Ht,field,Ang,ifield)
   end subroutine NUMBRH
   subroutine SYMBLH(X,Y,Htin,Mess,Ang,Nmess)
   implicit none
!*--SYMBLH6416
!
!
! COMMON variables
!
   real :: BOX,BOX2,BOX4,BOXwt2,HT,HT2,HT34,HTH,THIck,WT,WT38,WTH
   character(1),dimension(256) :: CHRtab,CHRtrl
   integer,dimension(5000) :: ICHpen
   integer :: ICHr,ICNtrl,ISYm,ITHick
   integer,dimension(2,256) :: INDchr
   real,dimension(5000) :: XCHar,YCHar
   common /PAINT / BOX,BOX2,BOX4,BOXwt2,HT,HT2,HTH,HT34,WT,WTH,WT38
   common /SYMTB1/ CHRtab,CHRtrl
   common /SYMTB2/ XCHar,YCHar,ICHpen,INDchr,ICHr,ICNtrl,ISYm
   common /THICKY/ ITHick,THIck
!
! Dummy arguments
!
   real :: Ang,Htin,X,Y
   integer :: Nmess
   character(1),dimension(Nmess) :: Mess
   intent (in) Ang,Htin,Mess,Nmess,X,Y
!
! Local variables
!
   real :: dx1,dy1,x1,xoff,y1,yoff
   real :: FLOAT
   integer :: i,i1,i2,ialter,iway,n,nalter,nthick
!
!
!
!  DRAW THICK CHARACTERS DEFINED BY STROKES IN SYMBOL TABLE
!
!  ----INITIALIZE FLAG TO USE STANDARD CHARACTER SET.
   data ialter/0/
!  ----INITIALIZE X AND Y OFFSET (USED FOR SUB AND SUPER SCRIPTS).
   data xoff/0.0/
   data yoff/0.0/
!  ----IF NO SOFTWARE CHARACTERS RETURN.
   if(ICHr<=0)return
!  ----SAVE LINE WIDTH AND DEFINE SCALED LINE WIDTH.
   nthick = ITHick
   ITHick = FLOAT(nthick)*Htin/HT
!  ----INITIALIZE POSITION AND DEFINE INCREMENTS.
   x1 = X
   y1 = Y
   iway = 0
   if(ABS(Ang)>1.0)iway = 1
   if(iway/=0)then
      dx1 = 0.0
      dy1 = Htin
   else
      dx1 = Htin
      dy1 = 0.0
   endif
!  ----SET UP LOOP TO PLOT CHARACTERS ONE AT A TIME.
   do n = 1,Nmess
!     ----INITIALIZE COUNT OF THE NUMBER OF TIMES CHARACTER HAS BEEN FOUND
!     ----(TO SELECT STANDARD OR ALTERNATE CHARACTER SET).
      nalter = 0
      do i = 1,ICHr
         if(Mess(n)/=CHRtab(i))then
         else
!           ----ALWAYS USE CONTROL CHARACTERS REGARDLESS OF CHARACTER SET.
            i1 = INDchr(1,i)
            if(ICHpen(i1)<=0)goto 50
!           ----SELECT STANDARD OR ALTERNATE CHARACTER SET.
            if(nalter/=ialter)then
               nalter = nalter + 1
            else
!              ----CHARACTER FOUND.
               i2 = INDchr(2,i)
               goto 70
            endif
         endif
      enddo
!     ----NOT SPECIAL CHARACTER...IGNORE.
      cycle
!     ----CONTROL CHARACTER...CHANGE CHARACTER SET OR RE-DEFINE OFFSET.
   50 continue
      if(ICHpen(i1)==0)then
!        ----DEFINE OFFSET.
         xoff = xoff + XCHar(i1)
         yoff = yoff + YCHar(i1)
      else
!        ----CHANGE CHARACTER SETS.
         ialter = 1 - ialter
      endif
      cycle
   70 continue
      if(iway/=0)then
!        ----VERTICAL.
         do i = i1,i2
            call PLOTH(x1-Htin*(yoff+YCHar(i)),y1+Htin*(xoff+XCHar(i)),ICHpen(i))
         enddo
!        ----ALL FOLLOWING CHARACTERS WILL BE POSITIONED RELATIVE TO THE OFFSET
         y1 = y1 + xoff
      else
!        ----HORIZONTAL.
         do i = i1,i2
            call PLOTH(x1+Htin*(xoff+XCHar(i)),y1+Htin*(yoff+YCHar(i)),ICHpen(i))
         enddo
!        ----ALL FOLLOWING CHARACTERS WILL BE POSITIONED RELATIVE TO THE OFFSET
         x1 = x1 + xoff
      endif
!     ----MOVE TO NEXT CHARACTER POSITION.
      x1 = x1 + dx1
      y1 = y1 + dy1
!     ----TURN OFF ALTERNATE CHARACTER SET FLAG AND SET OFFSET TO ZERO.
      ialter = 0
      xoff = 0.0
      yoff = 0.0
   enddo
!  ----RESTORE LINE WIDTH.
   ITHick = nthick
   return
   end subroutine SYMBLH
   subroutine SYMIN
   implicit none
!*--SYMIN6538
!
!
! COMMON variables
!
   character(1),dimension(256) :: CHRtab,CHRtrl
   integer,dimension(5000) :: ICHpen
   integer :: ICHr,ICNtrl,ISYm
   integer,dimension(2,256) :: INDchr
   real,dimension(5000) :: XCHar,YCHar
   common /SYMTB1/ CHRtab,CHRtrl
   common /SYMTB2/ XCHar,YCHar,ICHpen,INDchr,ICHr,ICNtrl,ISYm
!
! Local variables
!
   integer,save :: i,icore,icount,ihigh,jchr,lowest
!
!
!=======================================================================
!
!  LOAD SPECIAL SYMBOL TABLE.
!
!=======================================================================
!...  IMPLICIT DOUBLE PRECISION (A-H,O-Z)
!  ----DEFINE AVAILABLE CORE SIZE.
   data icore/5000/
!  ----INITIALIZE CHARACTER INDEX AND COUNT OF CONTROL CHARACTERS.
   ihigh = 0
   ICNtrl = 0
!  ----SET UP LOOP TO READ UP TO 256 SPECIAL CHARACTERS.
   do jchr = 1,256
      ICHr = jchr
!     ----READ FIRST LINE TO DEFINE CHARACTER AND NUMBER OF STROKES.
      read(ISYm,60,err=40,end=40)CHRtab(ICHr),icount
   60 format(1x,a1,i5)
      lowest = ihigh + 1
      ihigh = ihigh + icount
!     ----INSURE AVAILABLE CORE WILL NOT BE EXCEEDED.
      if(ihigh>icore)goto 40
!     ----READ STROKES (X, Y, PEN POSITION).
      do i = lowest,ihigh
         read(ISYm,70,err=40,end=40)XCHar(i),YCHar(i),ICHpen(i)
   70    format(2F7.3,i5)
      enddo
!     ----SAVE CONTROL CHARACTERS.
      if(icount/=1)then
      elseif(ICHpen(lowest)>0)then
      else
         ICNtrl = ICNtrl + 1
         CHRtrl(ICNtrl) = CHRtab(ICHr)
      endif
!     ----DEFINE INDICES TO SPECIAL CHARACTER STROKE TABLE.
      INDchr(1,ICHr) = lowest
      INDchr(2,ICHr) = ihigh
   enddo
   ICHr = 256
   goto 50
!  ----END OF DATA OR ERROR.
   40 continue
   ICHr = ICHr - 1
   if(ICHr<1)stop 'SYMIN ERROR - NO SYMBOLS LOADED'
   50 continue
   return
   end subroutine SYMIN
   subroutine PLOTH(X,Y,Ipen)
   implicit none
!*--PLOTH6607
!
!
! COMMON variables
!
   integer :: ITHick
   real :: THIck
   common /THICKY/ ITHick,THIck
!
! Dummy arguments
!
   integer :: Ipen
   real :: X,Y
!
! Local variables
!
   real :: cost,dxt,dxt1,dxy,dyt,dyt1,sint,xl,yl
   integer :: i
!
!
!
!  PLOT FROM LAST (X,Y) TO CURRENT (X,Y) EITHER NORMAL OR THICK LINE.
!
!  ----IF MOVE, NOT DRAW OR NORMAL LINE DO IT WITH 1 STROKE.
   if(Ipen/=2.or.ITHick<=0)then
!     ----DRAW NORMAL WIDTH LINE.
      call PLOT(X,Y,Ipen)
   else
      dxy = SQRT((X-xl)**2+(Y-yl)**2)
      if(dxy<=0.0)then
      else
!        ----DEFINE DIRECTION COSINE AND SINE.
         cost = (Y-yl)/dxy
         sint = (X-xl)/dxy
!        ----DEFINE OFFSET FOR LINE THICKNESS.
         dxt = -THIck*cost
         dyt = THIck*sint
!        ----DRAW THICK LINE.
         call PLOT(X,Y,2)
         dxt1 = dxt
         dyt1 = dyt
         do i = 1,ITHick
            call PLOT(xl+dxt,yl+dyt,3)
            call PLOT(X+dxt,Y+dyt,2)
            dxt = dxt + dxt1
            dyt = dyt + dyt1
         enddo
         dxt = dxt1
         dyt = dyt1
         do i = 1,ITHick
            call PLOT(xl-dxt,yl-dyt,3)
            call PLOT(X-dxt,Y-dyt,2)
            dxt = dxt + dxt1
            dyt = dyt + dyt1
         enddo
         call PLOT(X,Y,3)
      endif
   endif
   xl = X
   yl = Y
   return
   end subroutine PLOTH
!
