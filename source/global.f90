MODULE global_mod
    !! $Rev: 5131 $
    !! $Author: mwherman $
    !! $Date:  $
    !!
    !!   ********************************************************************
    !!   *                                                                  *
    !!   *                  G L O B A L                                     *
    !!   *                                                                  *
    !!   *  This module sets most of the Empire structures and serves as    *
    !!   *  a main channel of communication between different modules of    *
    !!   *  Empire.                                                         *
    !!   *  It has been dervived, and is replacing, the global.h in         *
    !!   *  the previous versions of the  code.                             *
    !!   *                                                                  *
    !!   *                                                                  *
    !!   *                                                                  *
    !!   ********************************************************************

IMPLICIT NONE

PUBLIC

!! Basic dimensions (when defaults are adjusted by the code during execution 
!! the 'parameter' will have to be removed)
   INTEGER*4, parameter  :: NDNUC = 100      !! number of nuclei
   INTEGER*4, parameter  :: NDEXCLUS = 50    !! number of  nuclei with exclusive spectra
   INTEGER*4, parameter  :: NDEX = 151       !! number of energy bins in the continuum
   INTEGER*4, parameter  :: NDLW = 100       !! number of partial waves
   INTEGER*4, parameter  :: LEVCC = 40       !! number of discrete levels (including those in the continuum?)
   INTEGER*4, parameter  :: NDLV = 40        !! number of discrete levels
   INTEGER*4, parameter  :: NDBR = 40        !! number of decay transitions (branching ratios) from single level
   INTEGER*4, parameter  :: NDMSCS = 4       !! number of MSC stages
!!
!! DO NOT CHANGE parameters below unless you REALLY know what you are doing.
!!
   INTEGER*4, parameter :: NDANGecis = 91   !! number of angles in ECIS code                                                  
   INTEGER*4, parameter :: NDCC = 10                                                          
   INTEGER*4, parameter :: NDDEFCC = 6
   INTEGER*4, parameter :: NDCOLLEV = 99    !! number of collective levels including DWBA in the contiuum                     
   INTEGER*4, parameter  :: MAX_PRN = 30                                             
   INTEGER*4, parameter  :: NDEJC = 6        !! number of ejectiles                                                  
   INTEGER*4, parameter  :: NDZMAX = 110     !! number of elements in symbol tables                                                 
   INTEGER*4, parameter  :: NDEPFN = 251     !! number of energies in PFNS spectra                                                  
   INTEGER*4, parameter  :: NDAnghmx = 37    !! must be equal to NDAnghms in ddhms.cmb
   INTEGER*4, parameter  :: NLGRID = 301     !! number of points in the HFB GDR RIPL files      
   INTEGER*4, parameter  :: NDVOM = 7        !! number of parameters in OMP real depth                                                  
   INTEGER*4, parameter  :: NDWOM = 7        !! number of parameters in OMP imaginary depth                                                   
   INTEGER*4, parameter  :: NDVSO = 7        !! number of parameters in OMP spin-orbit depth                                                   
   INTEGER*4, parameter  :: NDRVOM = 3       !! number of parameters in OMP real radius                                                   
   INTEGER*4, parameter  :: NDRWOM = 3       !! number of parameters in OMP imaginary radius                                                  
   INTEGER*4, parameter  :: NDRVSO = 3       !! number of parameters in OMP spin-orbit radius                                                   
   INTEGER*4, parameter  :: NDROPM = 7       !! number of level density parameters                                                  
   INTEGER*4, parameter  :: NDGDRPM = 10     !! number of GDR parameters
   INTEGER*4, parameter  :: NDGQRPM = 8      !! number of GQR parameters
   INTEGER*4, parameter  :: NDGMRPM = 8      !! number of GMR parameters                             
   INTEGER*4, parameter  :: NDKNTR = 3
   INTEGER*4, parameter  :: NMAsse = 9066    !! number of nuclides in the mass table                                        
   INTEGER*4, parameter  :: NDREGIONS = 14   !! number of exit channels in the IDNA matrix defining use of the models (neut. disc., neut. cont., prot. disc., prot. cont.,
   INTEGER*4, parameter  :: NDMODELS = 6     !! number of reaction models
   INTEGER*4, parameter  :: NFtrans = 40
   INTEGER*4, parameter  :: NFMOD = 3        !! number of fission modes
   INTEGER*4, parameter  :: NFisbarpnt = 300                                
   INTEGER*4, parameter  :: NFPARAB = 5      !! number of parabolas to describe fission barries
   INTEGER*4, parameter  :: NFHUMP = 3       !! number of humps in fission barriers 
   INTEGER*4, parameter  :: NFISENMAX = 400

   INTEGER*4 :: NDNUCD         !! number of decaying nuclei
   INTEGER*4 :: NDECSED
   INTEGER*4 :: NDANGD         !! number of angles in angular distributions
   INTEGER*4 :: NDEX_D
   INTEGER*4 :: NDEJCD         !! number of ejectiles used
   INTEGER*4 :: NDETL          !! number of energies in transmission coef. tables
   INTEGER*4 :: NDEREC         !! number of energies in recoil spectra
   INTEGER*4 :: NDERO          !! number of energy points in level density tables


character*200 :: EMPiredir = '../'
character*72  :: EMPtitle = 'Default calculation title - please change it'

!! Physics_constants CONSTANTS taken from the 2010 CODATA SET as given @ 
!! http://physics.nist.gov/constants
!! from SCAT2000:  ampipm = 1.395688D+02; ampi0 = 1.349645D+02
!!  and AMPi = (2.D0*ampipm + ampi0)/3.D0
   real*8, parameter :: AMPi   = 1.3802753333D+02
   real*8, parameter :: AMUmev = 9.31494061D+02      !! atmomic mass unit in MeV CODATA 2010
   real*8, parameter :: AMUneu = 1.00866491600D0     !! neutron  mass in AMU 
   real*8, parameter :: AMUpro = 1.007276466812D0    !! proton   mass in AMU 
   real*8, parameter :: AMUele = 0.00054857990946D0  !! electron mass in AMU  
   real*8, parameter :: ELE2   = 1.43996529D+00      !! e*e in MeV*fm  (CODATA 2010)
   real*8, parameter :: HHBarc = 197.3269718D0       !! h_bar*c MeV fm (CODATA 2010)

!! Numerical constants
   real*8, parameter :: EXPmax  = 700.d0  !! maximum argument of EXP
   real*8, parameter :: EXPdec  = 300.d0  !! maximum exponent of 10
   real*8, parameter :: CSMinim = 1.0d-7  !! minimum cross section to consider
   real*8, parameter :: TISomer = 1.0D0   !! half-life to be considered isomer (in sec.)

!! For Gamma_gamma calculations
   real*8  D0_obs !! observed spacing of S-wave resonances at neutron binding
   real*8  D0_unc !! unertainty of the observed S-wave spacing
   real*8  S0_obs !! measured neutron strength function for S-wave
   real*8  S0_unc !! uncertaity of the neutron strength function
   real*8  Gg_obs !! obsered averaged gamma width at thermal
   real*8  Gg_unc !! uncertainty of the gamma width 

!! ECIS realted
   real*8  SANgler(ndangecis)
   real*8  ANGles(ndangecis)
   real*8  CANGle(ndangecis)
   real*8  CANgler(ndangecis)
   real*8  FCD(ndcc)
   real*8  FLAm(ndcc)

!!OMP Hamiltonian matrix elements
   real*8  SR_Ham_hw
   real*8  SR_Ham_amb0
   real*8  SR_Ham_amg0
   real*8  SR_Ham_gam0
   real*8  SR_Ham_bet0
   real*8  SR_Ham_bet4
   real*8  SR_Ham_bb42
   real*8  SR_Ham_gamg
   real*8  SR_Ham_delg
   real*8  SR_Ham_bet3
   real*8  SR_Ham_et0
   real*8  SR_Ham_amu0
   real*8  SR_Ham_hw0 
   real*8  SR_Ham_bb32
   real*8  SR_Ham_gamde
   real*8  SR_Ham_dpar
   real*8  SR_Ham_gshape

!! PFNS parameters
   real*8  PFNtke
   real*8  PFNalp
   real*8  PFNrat
   real*8  PFNniu
   real*8  PFNere
   real*8  TMAxw

!! Collective levels
   integer*4  D_Klv(ndcollev)
   integer*4  D_Llv(ndcollev)
   integer*4  ICOller(ndcollev)
   integer*4  ICOllev(ndcollev)
   integer*4  D_nno(ndcollev)
   integer*4  IPH(ndcollev)
   real*8     D_Elv(ndcollev)
   real*8     D_Lvp(ndcollev)
   real*8     D_Xjlv(ndcollev)
   real*8     D_Def(ndcollev,nddefcc)
   real*8     BETcc(ndcc)
   real*8     QCC(1:ndcc)

!! ENDF MT numbers
   integer*4  MT2
   integer*4  MT91
   integer*4  MT649
   integer*4  MT699
   integer*4  MT749
   integer*4  MT799
   integer*4  MT849

!! Global correction factors    
   real*8 ::  TOTred  = 1.0D0
   real*8 ::  ELAred  = 1.0D0
   real*8 ::  CELred  = 1.0D0
   real*8 ::  CELcor  = 1.0D0  ! MAY NOT BE NEEDED
   real*8 ::  rTOTRED = 1.0D0
   real*8 ::  rFCCRED = 1.0D0
   real*8 ::  rFUSRED = 1.0D0
   real*8 ::  FUSred0 = 1.0D0
   real*8 ::  FUSred  = 1.0D0
   real*8 ::  rFCOred = 1.0D0
   real*8 ::  rELAred = 1.0D0
   real*8 ::  rCELred = 1.0D0
   real*8 ::  rCELcor = 1.0D0 ! MAY NOT BE NEEDED
   real*8 ::  FCCred0 = 1.0D0
   real*8 ::  ELAred0 = 1.0D0
   real*8 ::  FCOred0 = 1.0D0
   real*8 ::  TOTred0 = 1.0D0
   real*8 ::  rCINred(ndlv) = 1.0D0 ! ALL MATRX TO SET TO 1

!! Input options
!! (to covert eventually to logical)
   integer*4 :: FITomp = 0
   integer*4 :: INTerf = 1
   integer*4 :: KALman = 0
   integer*4 :: LHMs   = 0
   integer*4 :: MSC    = 1
   integer*4 :: MSD    = 1
!! (non-covertable to logical)
   integer*4 :: LHRtw   = 1
   integer*4 :: IOUt    = 3
   integer*4 :: LEVtarg = 1
   integer*4 :: NEMn    = 2
   integer*4 :: NEMp    = 1
   integer*4 :: NEMa    = 1
   integer*4 :: NEMc    = 0
   integer*4 :: NEXreq  = 80
   integer*4 :: NHMs    = 0
   real*8    :: EIN     = 5.0d0
   real*8    :: FITlev  = 0.0d0
   real*8    :: FCCred  = 1.0d0
   real*8    :: GCAsc   = 1.0d0

!! others
   integer*4  ICOmpff
   integer*4  IX4ret 
   integer*4  PESpin
   integer*4  NNG_xs
   integer*4  F_Print
   integer*4  NPRIm_g
   integer*4  NPAirpe
   integer*4  FHMs
   integer*4  IDEfcc
   integer*4  IFLuc
   integer*4  IGE1
   integer*4  IGE2
   integer*4  IGM1
   integer*4  IOMwritecc
   integer*4  IOPsys
   integer*4  IOPran
   integer*4  IDDfig(3)
   integer*4  IWArn
   integer*4  JCUtcoll
   integer*4  KEY_gdrgfl
   integer*4  KEY_shape
   integer*4  KTRompcc
   integer*4  LMAxcc
   integer*4  MODelecis
   integer*4  MAXmult
   integer*4  NACc
   integer*4  ND_nlv
   integer*4  NEJcm
   integer*4  NANgela
   integer*4  IHFnew
   integer*4  NLW
   integer*4  NNUcd
   integer*4  NNUct
   integer*4  NOUt
   integer*4  NPRoject
   integer*4  NRBar
   integer*4  NRWel
   integer*4  NSCc
   integer*4  NTArget
   integer*4  NENdf
   integer*4  NEXclusive
   integer*4  NDAng
   integer*4  ICAlangs
   integer*4  FISspe
   integer*4  SFAct
   integer*4  IPArcov
   integer*4  ngamm_tr
   integer*4  nfiss_tr
   integer*4  PLcont_lmax(NDEX)
   integer*4  IDNa(ndregions,ndmodels)
   
   LOGICAL  :: FIRst_ein = .true.
   LOGICAL  :: FUSread = .false.
   LOGICAL  :: EXClusiv = .false.
   LOGICAL  :: CN_isotropic = .false.
   LOGICAL  CCCalc
   LOGICAL  DEFault_energy_functional
   LOGICAL  DEFormed
   LOGICAL  FILevel
   LOGICAL  OMParfcc
   LOGICAL  OMPar_riplf
   LOGICAL  RELkin
   LOGICAL  SDRead
   LOGICAL  SOFt
   LOGICAL  NUBarread
   LOGICAL  BENchm
   LOGICAL  CALctl
   LOGICAL  DYNam
   LOGICAL  COLfile

   real*8   :: EXCessmass(0:130,0:400)
   real*8   :: RESmas(0:130,0:400)
   real*8   TFB
   real*8   TDIrect
   real*8   ADIv
   real*8   EHRtw
   real*8   BETav
   real*8   BFUs
   real*8   BUNorm
   real*8   NTNorm
   real*8   COMega
   real*8   CETa
   real*8   CHMs
   real*8   CRL
   real*8   CHMax
   real*8   CSFis
   real*8   CSFus
   real*8   CSGdr1
   real*8   CSGdr2
   real*8   COEf
   real*8   ELCncs
   real*8   CSMsc(0:2)
   real*8   CSO
   real*8   CSRead
   real*8   D1Fra
   real*8   DE
   real*8   DEFga
   real*8   DEFgp
   real*8   DEFgw
   real*8   DEFpar
   real*8   DEFprj
   real*8   DEGa
   real*8   DENhf
   real*8   DERec
   real*8   DFUs 
   real*8   DIRect 
   real*8   DIToro
   real*8   DV 
   real*8   DEFdyn
   real*8   DEFsta
   real*8   ECUtcoll
   real*8   EGDr1 
   real*8   EGDr2 
   real*8   EINl         
   real*8   SUMlev_alf
   real*8   EDDfig  
   real*8   EWSr2
   real*8   EX1
   real*8   EX2
   real*8   EXCn
   real*8   EXPush
   real*8   FCC
   real*8   EWSr1
   real*8   EMInmsd
   real*8   GDIv
   real*8   FCOred
   real*8   GDResh
   real*8   GDRspl
   real*8   GDRwa1
   real*8   GDRwa2
   real*8   GDRweis
   real*8   GGDr1
   real*8   GGDr2
   real*8   GDRdyn
   real*8   DXSred
   real*8   GST
   real*8   LQDfac
   real*8   MFPp
   real*8   MOMortcrt
   real*8   PEQc
   real*8   PEQcont
   real*8   PI
   real*8   PIx4
   real*8   WIDcoll
   real*8   QDFrac
   real*8   QFIs
   real*8   REDsef
   real*8   Emax_tlj
   real*8   SHNix
   real*8   SHRd
   real*8   SHRj
   real*8   SHRt
   real*8   SIG
   real*8   STMro
   real*8   TEMp0
   real*8   TORy
   real*8   TOTcsfis
   real*8   TRUnc
   real*8   XNI
   real*8   DBRkup
   real*8   hnorm                         !! ROHFBSADDLE
   real*8   MOMparcrt
   real*8   CSDbrkup(6)
   real*8   CSEat(ndecse,2)               !! total DDXS at two angles and 1 ejectile  
   real*8   gamm_tr(10)
   real*8   CINred(NDLV)
   real*8   CSEpg(NDLV)
   real*8   ENPg(NDLV)
   real*8   TUNetl(NDLW)
   real*8   REDmsc(NDLW,2)
   integer*4  PL_lmax(NDLV)
   real*8   PL_CN(0:(2*NDLW),NDLV)
   real*8   PL_CNcont(0:(2*NDLW), NDEX)
   real*8   fiss_tr(NDLW,2)
   real*8   ATIlnoz(NDZmax)

!! Fission parabolas (make into type)
   integer*4  NRFdis(nfparab)
   real*8     AFIs(nfparab)
   real*8     HCOnt(nfparab)
   real*8     EFB(nfparab)
   real*8     DEFfis(nfparab)
   integer*4  IPFdis(nftrans,nfparab)
   real*8     SFDis(nftrans,nfparab)
   real*8     H(nftrans,nfparab)
   real*8     EFDis(nftrans,nfparab)

!! Fission  humps  (make into type)
   integer*4  NRHump
   integer*4  NRBinfis(NFHump)
   integer*4  BFF(NFHump)
   real*8     AWF(NFHUMP)
   real*8     ECFis(NFHUMP)
   real*8     ECDamp(NFHUMP)
   real*8     DEStepp(NFHump)
   real*8     DELtafis(NFHump)
   real*8     SHCfis(NFHump)
   real*8     GAMmafis(NFHump)
   real*8     vibf12(NFHump)
   real*8     vibfdt(NFHump)
   real*8     vibfnorm(NFHump)
   real*8     XMInn(NFHump)
   real*8     barnorm(NFHump)
   real*8     rohfbp_sd(NFHump)
   real*8     rohfba_sd(NFHump)             !! ROHFBSADDLE
   real*8     rohfb_norm(NFHUMP)
   real*8     WIMag(NFHump-1,3)
   real*8     ENH_ld(3,NFHump)
   real*8     UGRid(0:nfisenmax,NFHump)
   real*8     ROFisp(nfisenmax,ndlw,2,NFHump)

!! Multi-modal fission (make into type)
   integer*4  BFFm(NFMOD)
   integer*4  NRBinfism(NFMOD)
   real*8     ROFism(NFISENMAX,NDLW,NFMOD)
   real*8     HM(NFTRANS,NFMOD)
   real*8     EFDism(NFTRANS,NFMOD)
   real*8     UGRidf(NFISENMAX,NFMOD)
   real*8     EFBm(NFMOD)
   real*8     XMInnm(NFMOD)
   real*8     AFIsm(NFMOD)
   real*8     DEFbm(NFMOD)
   real*8     SHCfism(NFMOD)
   real*8     DELtafism(NFMOD)
   real*8     GAMmafism(NFMOD)
   real*8     WFIsm(NFMOD)
   real*8     DEStepm(NFMOD)
   real*8     TFBm(NFMOD)
   real*8     TDIrm(NFMOD)
   real*8     CSFism(NFMOD)
   real*8     ECFism(NFMOD)
   real*8     VIBf12m(NFMOD)
   real*8     VIBfdtm(NFMOD)
   real*8     VIBfnormm(NFMOD)

TYPE OMP_type !TO BE DIMENSIONED 0(or 1):NDNUC
   integer KTRlom(0:ndejc,0:ndnuc)
   real*8  SIGabs(ndetl,ndejc,ndnuc)
   real*8  VOM(0:ndejc,0:ndnuc)
   real*8  VOMs(0:ndejc,0:ndnuc)
   real*8  VSO(0:ndejc,0:ndnuc)
   real*8  WOMs(0:ndejc,0:ndnuc)
   real*8  WOMv(0:ndejc,0:ndnuc)
   real*8  WSO(0:ndejc,0:ndnuc)
   real*8  RVOm(0:ndejc,0:ndnuc)
   real*8  RVSo(0:ndejc,0:ndnuc)
   real*8  RWOmv(0:ndejc,0:ndnuc)
   real*8  RWOm(0:ndejc,0:ndnuc)
   real*8  RWSo(0:ndejc,0:ndnuc)
   real*8  AWSo(0:ndejc,0:ndnuc)
   real*8  AVOm(0:ndejc,0:ndnuc)
   real*8  AVSo(0:ndejc,0:ndnuc)
   real*8  AWOm(0:ndejc,0:ndnuc)
   real*8  AWOmv(0:ndejc,0:ndnuc)
   real*8  FNvvomp(0:ndejc,0:ndnuc)
   real*8  FNwvomp(0:ndejc,0:ndnuc)
   real*8  FNavomp(0:ndejc,0:ndnuc)
   real*8  FNwsomp(0:ndejc,0:ndnuc)
   real*8  FNasomp(0:ndejc,0:ndnuc)
   real*8  FNrvomp(0:ndejc,0:ndnuc)
   real*8  FNrwvomp(0:ndejc,0:ndnuc)
   real*8  FNrsomp(0:ndejc,0:ndnuc)
   real*8  XNAver(0:ndejc,NDEtl)
END TYPE OMP_type

TYPE Ejectile_type !TO BE DIMENSIONED 0(or 1):NDejc
   character*2 SYMbe(0:ndejc)        !! symbol
   integer IZAejc(0:ndejc)           !! Z*1000 + A
   integer NREs(0:ndejc)             !!
   real*8  SEJc(0:ndejc)             !! spin
   real*8  ZEJc(0:ndejc)             !! Z
   real*8  AEJc(0:ndejc)             !! A
   real*8  XNEjc(0:ndejc)            !! N
   real*8  EJMass(0:ndejc)           !! mass
   real*8  XMAss_ej(0:ndejc)         !! mass ???
   real*8  AUSpec(ndecse,0:ndejc)    !! auxiliary spectrum
   real*8  CSComplev(ndlv,0:ndejc)   !! compound cross section to the discrete level
   real*8  CSAlev(ndangecis,ndlv,0:ndejc)  !! x-sec angular distribution to the level
   real*8  CSDirlev(ndlv,0:ndejc)    !! direct cross section to the level
   real*8  CSEt(ndecse,0:ndejc)      !! total emisssion spectrum
   real*8  CSEa(ndecse,ndangecis,0:ndejc)  !! total energy-angle DDX
   real*8  CSEmsd(ndecse,0:ndejc)    !! MSD (or preequilibrium) energy spectrum
   real*8  CSEdbk(ndecse,0:ndejc)    !! breakup energy spectrum
   real*8  CSMsd(0:ndejc)            !! MSD (or preequilibrium) cross section
   real*8  CSGinc(ndejc)
   real*8  MAXj(0:ndejc)
   real*8  REClev(ndlv,0:ndejc)      !! recoil energy of the residue after emission of ejectile to the level 
   real*8  SCRt(ndex,ndlw,2,0:ndejc) !! scratch matrix  
   real*8  SCRtem(0:ndejc)
   real*8  SCRtl(ndlv,0:ndejc)
   real*8  TLJ(ndetl,ndlw,3,ndejc)   !! T_lj (emission energy, l, j)
   real*8  BUReac(0:ndejc)           !! breakup cross section for incident deuteron 
   real*8  NTReac(0:ndejc)
   !! Ejectile specific correction factors
   real*8  rTUNEPE(0:ndejc)
   real*8  TUNEpe(0:ndejc)
   real*8  TUNEbu(0:ndejc)
   real*8  TUNEnt(0:ndejc)
END TYPE Ejectile_type

TYPE Nucleus_type !TO BE DIMENSIONED 0(or 1):NDNUC
   character*2 SYMb(0:ndnuc)             !! chemical symbol of the nucleus
   character*21 REAction(1:ndnuc)        !! reaction string associated with the nucleus
   logical :: FISsil(1:ndnuc) = .false.  !! is it fissile?
   integer A                   !! A of the nucleus
   integer Z                   !! Z of the nucleus
   real*8  AMAss               !! mass of the nucleus    
   real*8  XZ(0:ndnuc)         !! Z of the nucleus (real) 
   real*8  XN(0:ndnuc)         !! number of neutrons in the nucleus
   real*8  XMAss(0:ndnuc)      !! mass of the nucleus
   real*8  HIS(0:ndnuc)        !! ground state spin
   real*8  SHLlnor(0:ndnuc)    !! shell normalization fcator
   real*8  POPmax(1:ndnuc)     !! maximum continuum cell (E,J,pi) population 
   real*8  POPcon(1:ndnuc)     !! integrated population of the continuum 
   real*8  POPdis(1:ndnuc)     !! summed opulation of all discrete levels
   real*8  CSPfis(0:ndnuc) 
   real*8  CSPopul(1:ndnuc)    !! population of the nucleus ???
   real*8  CSPrd(ndnuc)        !! cumulative production x-sec of the nucleus
   real*8  DEPart(1:ndnuc) 
   real*8  DEFnor(0:NDNUC) 
   real*8  ATIlnor(0:ndnuc)    !! normalization factor on level density parameter 'a'
   real*8  DOBs(0:ndnuc)       !! S-wave D-observed
   real*8  CSInc(1:ndnuc) 
   real*8  :: GTIlnor(0:ndnuc) = 1.0  !! PE g normalization fcator
   real*8  QQInc(0:ndnuc)
   real*8  QPRod(0:ndnuc) 
   integer NLVc(0:ndnuc)  !! total number of levels used in the calculations
   integer NLVt(0:ndnuc)  !! total number of levels
   integer NLV(0:ndnuc)   !! number of the level after which continuum starts
   real*8  ECOnt(0:ndnuc) !! energy the continuum starts  
   real*8  ECUt(0:ndnuc)  !! energy the discrete levels end
   real*8  EMAx(1:ndnuc)  !! maximum excitation energy of the nucleus
   integer ENDf(0:ndnuc)  !! exclusive (1) or inclusive (0) ENDF treatment
   integer JSTab(1:ndnuc) !! max J of stability of the nucleus
   integer PIZA(0:ndnuc)
   integer ncontr(0:ndnuc)
   integer NCOmp(0:ndnuc)
   integer NEX(1:ndnuc)   !! number of bins in the continuum
   integer NSTored(0:ndnuc)
   integer INExc(0:ndnuc)
   integer ISProd(0:ndnuc)
   integer NRSmooth(0:ndnuc)
   integer iugMax(0:ndnuc)

   real*8  DEF(ndlw,0:ndnuc)        !! dynamical l-dependent deformation (for lev. den.) 
   real*8  SHCjf(ndlw,1:ndnuc)
   real*8  YRAst(ndlw,1:ndnuc)      !! max l for a nucleus
   real*8  UEXcit(ndecse,1:ndnuc)   !! nucleus excitation bin energies 
   
   integer IOMwrite(0:ndejc,0:ndnuc)
   integer ENDfp(0:ndejc,0:ndnuc)   !! print (1) or not (0) exclusive spectra for ejectile 
   integer IRElat(0:ndejc,0:ndnuc)
   integer NEXr(0:ndejc,1:ndnuc)    !! number of bin in the continuum for the residual after ejectile emission
   real*8  EEFermi(0:ndejc,0:ndnuc) !! Fermi energy for the residual after ejectile emission
   real*8  Q(0:ndejc,0:ndnuc)       !! Q-value for the ejectile in the current nucleus
   real*8  RCOul(0:ndejc,0:ndnuc)   !! Coulomb barrier for the ejectile from the current nucleus
   real*8  RNOnl(0:ndejc,0:ndnuc) 
   real*8  ACOul(0:ndejc,0:ndnuc)
   real*8  OMEmax(0:ndejc,0:ndnuc) 
   real*8  OMEmin(0:ndejc,0:ndnuc)

   real*8  CSE(ndecse,0:ndejc,0:ndnuc)    !! x-sec energy distribution (spectrum)
   real*8  CSEfis(NDEPFN,0:ndejc,0:ndnuc) !! fission spectrum

   integer LMAxtl(ndetl,ndejc,1:ndnuc)   !! maximum l for T_l's 
   real*8  ETL(ndetl,ndejc,1:ndnuc)      !! energy grid for transmission coeficients
   real*8  TL(ndetl,ndlw,ndejc,1:ndnuc)  !! transmision coeficients T_l


   integer ISIsom(ndlv,0:ndnuc) 
   integer LVP(ndlv,0:ndnuc)        !! discrete level parity
   real*8  POPlv(ndlv,1:ndnuc)      !! population of individual discrete levels
   real*8  ELV(ndlv,0:ndnuc)        !! discrete level energy
   real*8  XJLv(ndlv,0:ndnuc)       !! discrete level spin
   real*8  BR(ndlv,ndbr,3,0:ndnuc)  !! discrete level's decay (branching ratios, conversion, )

   real*8  EX(ndex + 1,1:ndnuc)     !! continuum bins' energy boundaries 

   real*8  POPbin(ndex,1:ndnuc)     !! population of the bin (E)
   real*8  POP(ndex,ndlw,2,1:ndnuc) !! population of every cell (E,J,pi) in the continuum

   real*8  POPcs(0:ndejc,0:ndexclus)  !! population cross section (exclusive nuclei)
   real*8  POPcse(0:ndex_d,0:ndejc,ndecsed,0:ndexclus) !! population energy spectrum (exclusive nuclei)  
   real*8  POPcsed(0:ndex_d,0:ndejc,ndecsed,0:ndexclus)  !! population energy ??? spectrum (exclusive nuclei)
   real*8  POPcseaf(0:ndex_d,0:ndejcd,ndecsed,0:ndexclus) !! fraction of energy_angle spectrum from the CN emission(exclusive nuclei)
 
   real*8  GDRpar(ndgdrpm,0:ndnuc)  !! Giant Dipol Resonance parametrs
   real*8  GMRpar(ndgmrpm,0:ndnuc)  !! Giant Magnetic Resonance parametrs 
   real*8  GQRpar(ndgqrpm,0:ndnuc)  !! Giant Quadrupole Resonance parametrs

   real*8  FIStga(1:ndnuc)
   real*8  FISb(ndlw,1:ndnuc)
   real*8  FISbar(1:ndnuc)
   real*8  FISden(1:ndnuc)
   real*8  FISdis(1:ndnuc)
   real*8  FISmod(1:ndnuc)
   real*8  FISopt(1:ndnuc)
   real*8  FISshi(1:ndnuc)
   real*8  HJ(ndnuc,nfparab)
   real*8  FISv_n(NFHUMP,1:ndnuc)
   real*8  FISh_n(NFHUMP,1:ndnuc)
   real*8  FISa_n(NFHUMP,1:ndnuc)
   real*8  FISd_n(NFHUMP,1:ndnuc)
   real*8  FISn_n(NFHUMP,1:ndnuc)

   real*8  RECcse(nderec,0:nderec,1:ndnuc)  !! recoil energy spectra

   real*8  ROPaa(1:ndnuc)
   real*8  ROHfbp(1:ndnuc)
   real*8  ROHfba(1:ndnuc)
   real*8  ROHfbp_off(1:ndnuc) 
   real*8  ROHfba_off(1:ndnuc)
   real*8  om2_ig(0:NDNUC)
   real*8  delp_ig(0:NDNUC)
   real*8  atil_ig(0:NDNUC)
   real*8  dshift_ig(0:NDNUC)
   real*8  SHC(0:ndnuc)

   real*8  RO(ndex,ndlw,2,1:ndnuc) 
   real*8  ROF(ndex,ndlw,1:ndnuc)
   real*8  ROPar(ndropm,1:ndnuc)

   real*8  SFIom(0:ndejc,0:ndnuc)

   real*8  TNUc(ndecse,1:ndnuc)
   real*8  TNUcf(ndecse,1:ndnuc)
   real*8  TUNe(0:ndejc,0:ndnuc)
   real*8  rTUNe(0:ndejc,0:ndnuc)
   real*8  TUNEfi(0:ndnuc)
   real*8  rTUNEfi(0:ndnuc)
   
   TYPE (OMP_type), pointer :: OMP(:)   !! OM parameters
END TYPE Nucleus_type

! Dimensions derived from other dimensions (DO NOT MODIFY!)
!NDECSE  = NDEX*1.30                                                 
!NDNUCD  = NDNUC                                                     
!NDECSED = NDECSE                                                   
!NDANGD  = NDANGecis                                                 
!NDEX_D  = NDEX                                                      
!NDEJCD  = NDEJC                                                     
!NDETL   = NDECSE                                                     
!NDEREC  = NDECSE                                                    
!NDERO   = NDEX                                                       
! Temporary fo testing mnucleus allocation
 
TYPE(Nucleus_type), ALLOCATABLE, TARGET :: nucleus(:)      !! nuclei
END MODULE global_mod