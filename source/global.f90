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

!! Basic dimensions (when matrices are allocated dynamically during execution 
!! the 'parameter' will have to be removed)
   INTEGER*4, parameter  :: NDNUC = 100      !! max number of nuclei
   INTEGER*4, parameter  :: NDEXCLUS = 50    !! max number of  nuclei with exclusive spectra
   INTEGER*4, parameter  :: NDEX = 151       !! max number of energy bins in the continuum
   INTEGER*4, parameter  :: NDLW = 100       !! max number of partial waves
   INTEGER*4, parameter  :: LEVCC = 40       !! max number of discrete levels (including those in the continuum?)
   INTEGER*4, parameter  :: NDLV = 40        !! max number of discrete levels
   INTEGER*4, parameter  :: NDBR = 40        !! max number of decay transitions (branching ratios) from single level
   INTEGER*4, parameter  :: NDMSCS = 4       !! max number of MSC stages
!!
!! DO NOT CHANGE parameters below unless you REALLY know what you are doing.
!!
!   INTEGER*4, parameter :: NDANGecis = 91    !! max number of angles in ECIS code (defined with collective levels later)
   INTEGER*4, parameter :: NDCC = 10         !! maxx                                             
   INTEGER*4, parameter :: NDDEFCC = 6       !! max
   INTEGER*4, parameter :: NDCOLLEV = 99     !! max number of collective levels including DWBA in the contiuum
   INTEGER*4, parameter  :: MAX_PRN = 30     !! max                                 
   INTEGER*4, parameter  :: NDEJC = 6        !! max number of ejectiles                                                  
   INTEGER*4, parameter  :: NDZMAX = 110     !! max number of elements in symbol tables
   INTEGER*4, parameter  :: NDEPFN = 251     !! max number of energies in PFNS spectra
   INTEGER*4, parameter  :: NDAnghmx = 37    !! max must be equal to NDAnghms in ddhms.cmb
   INTEGER*4, parameter  :: NLGRID = 301     !! max number of points in the HFB GDR RIPL files      
   INTEGER*4, parameter  :: NDVOM = 7        !! max number of parameters in OMP real depth
   INTEGER*4, parameter  :: NDWOM = 7        !! max number of parameters in OMP imaginary depth
   INTEGER*4, parameter  :: NDVSO = 7        !! max number of parameters in OMP spin-orbit depth
   INTEGER*4, parameter  :: NDRVOM = 3       !! max number of parameters in OMP real radius
   INTEGER*4, parameter  :: NDRWOM = 3       !! max number of parameters in OMP imaginary radius
   INTEGER*4, parameter  :: NDRVSO = 3       !! max number of parameters in OMP spin-orbit radius
   INTEGER*4, parameter  :: NDROPM = 7       !! max number of level density parameters
   INTEGER*4, parameter  :: NDGDRPM = 10     !! max number of GDR parameters
   INTEGER*4, parameter  :: NDGQRPM = 8      !! max number of GQR parameters
   INTEGER*4, parameter  :: NDGMRPM = 8      !! max number of GMR parameters                             
   INTEGER*4, parameter  :: NDKNTR = 3       !! max
   INTEGER*4, parameter  :: NMAsse = 9066    !! max number of nuclides in the mass table
   INTEGER*4, parameter  :: NDREGIONS = 14   !! max number of exit channels in the IDNA matrix defining use of the models
   INTEGER*4, parameter  :: NDMODELS = 6     !! max number of reaction models
   INTEGER*4, parameter  :: NFtrans = 40     !! max
   INTEGER*4, parameter  :: NFMOD = 3        !! max number of fission modes
   INTEGER*4, parameter  :: NFisbarpnt = 300 !! max                           
   INTEGER*4, parameter  :: NFPARAB = 5      !! max number of parabolas to describe fission barries
   INTEGER*4, parameter  :: NFHUMP = 3       !! max number of humps in fission barriers 
   INTEGER*4, parameter  :: NFISENMAX = 400  !! max 

   INTEGER*4 :: NDNUCD           !! number of decaying nuclei
   INTEGER*4 :: NDECSED          !!
   INTEGER*4 :: NDANGD           !! number of angles in angular distributions
   INTEGER*4 :: NDEX_D           !!
   INTEGER*4 :: NDEJCD           !! number of ejectiles used
   INTEGER*4 :: NDETL            !! number of energies in transmission coef. tables
   INTEGER*4 :: NDEREC           !! number of energies in recoil spectra
   INTEGER*4 :: NDERO            !! number of energy points in level density tables


character*200 :: EMPiredir = '../'
character*72  :: EMPtitle = 'Default calculation title - please change it'

!! Physics_constants CONSTANTS taken from the 2010 CODATA SET as given @ 
!! http://physics.nist.gov/constants
!! from SCAT2000:  ampipm = 1.395688D+02; ampi0 = 1.349645D+02
!!  and AMPi = (2.D0*ampipm + ampi0)/3.D0
   real*8, parameter :: AMPi   = 1.3802753333D+02     !!
   real*8, parameter :: AMUmev = 9.31494061D+02       !! atmomic mass unit in MeV CODATA 2010
   real*8, parameter :: AMUneu = 1.00866491600D0      !! neutron  mass in AMU 
   real*8, parameter :: AMUpro = 1.007276466812D0     !! proton   mass in AMU 
   real*8, parameter :: AMUele = 0.00054857990946D0   !! electron mass in AMU  
   real*8, parameter :: ELE2   = 1.43996529D+00       !! e*e in MeV*fm  (CODATA 2010)
   real*8, parameter :: HHBarc = 197.3269718D0        !! h_bar*c MeV fm (CODATA 2010)

!! Numerical constants
   real*8, parameter :: EXPmax  = 700.d0  !! maximum argument of EXP
   real*8, parameter :: EXPdec  = 300.d0  !! maximum exponent of 10
   real*8, parameter :: CSMinim = 1.0d-7  !! minimum cross section to consider
   real*8, parameter :: TISomer = 1.0D0   !! half-life to be considered isomer (in sec.)

!! For Gam::ma_gamma calculations
   real*8 :: D0_obs  !! observed spacing of S-wave resonances at neutron binding
   real*8 :: D0_unc  !! unertainty of the observed S-wave spacing
   real*8 :: S0_obs  !! measured neutron strength function for S-wave
   real*8 :: S0_unc  !! uncertaity of the neutron strength function
   real*8 :: Gg_obs  !! obsered averaged gamma width at thermal
   real*8 :: Gg_unc  !! uncertainty of the gamma width at thermal

!! ECIS realted
   real*8 :: SANgler(ndangecis)
   real*8 :: ANGles(ndangecis)
   real*8 :: CANGle(ndangecis)
   real*8 :: CANgler(ndangecis)
   real*8 :: FCD(ndcc)
   real*8 :: FLAm(ndcc)

!!OMP Hamiltonian matrix elements
   real*8 :: SR_Ham_hw       !! Hamiltonian matrix element (OPTMAN)
   real*8 :: SR_Ham_amb0     !! Hamiltonian matrix element (OPTMAN)
   real*8 :: SR_Ham_amg0     !! Hamiltonian matrix element (OPTMAN)
   real*8 :: SR_Ham_gam0     !! Hamiltonian matrix element (OPTMAN)
   real*8 :: SR_Ham_bet0     !! Hamiltonian matrix element (OPTMAN)
   real*8 :: SR_Ham_bet4     !! Hamiltonian matrix element (OPTMAN)
   real*8 :: SR_Ham_bb42     !! Hamiltonian matrix element (OPTMAN)
   real*8 :: SR_Ham_gamg     !! Hamiltonian matrix element (OPTMAN)
   real*8 :: SR_Ham_delg     !! Hamiltonian matrix element (OPTMAN)
   real*8 :: SR_Ham_bet3     !! Hamiltonian matrix element (OPTMAN)
   real*8 :: SR_Ham_et0      !! Hamiltonian matrix element (OPTMAN)
   real*8 :: SR_Ham_amu0     !! Hamiltonian matrix element (OPTMAN)
   real*8 :: SR_Ham_hw0      !! Hamiltonian matrix element (OPTMAN)
   real*8 :: SR_Ham_bb32     !! Hamiltonian matrix element (OPTMAN)
   real*8 :: SR_Ham_gamde    !! Hamiltonian matrix element (OPTMAN)
   real*8 :: SR_Ham_dpar     !! Hamiltonian matrix element (OPTMAN)
   real*8 :: SR_Ham_gshape   !! Hamiltonian matrix element (OPTMAN)

!! PFNS parameters
   real*8 :: PFNtke    !! PFNS kinetic energy
   real*8 :: PFNalp    !! PFNS parameter
   real*8 :: PFNrat    !! PFNS parameter
   real*8 :: PFNniu    !! PFNS parameter
   real*8 :: PFNere    !! PFNS parameter
   real*8 :: TMAxw     !! PFNS Maxwellian temperature

!! Collective levels
   INTEGER*4, parameter :: NDANGecis = 91  !! number of angles in ECIS code
   integer*4 :: D_Klv(ndcollev)            !!              
   integer*4 :: D_Llv(ndcollev)            !!
   integer*4 :: ICOller(ndcollev)          !!
   integer*4 :: ICOllev(ndcollev)          !!  
   integer*4 :: D_nno(ndcollev)            !!
   integer*4 :: IPH(ndcollev)              !!
   real*8    :: D_Elv(ndcollev)            !!
   real*8    :: D_Lvp(ndcollev)            !!
   real*8    :: D_Xjlv(ndcollev)           !!
   real*8    :: D_Def(ndcollev,nddefcc)    !!
   real*8    :: BETcc(ndcc)                !! 
   real*8    :: QCC(1:ndcc)                !!

!! ENDF MT numbers
   integer*4 :: MT2       !! elastic nucleus position
   integer*4 :: MT91      !! inelastic nucleus position
   integer*4 :: MT649     !! (z,p) residue position
   integer*4 :: MT699     !! (z,d) residue position
   integer*4 :: MT749     !! (z,t) residue position
   integer*4 :: MT799     !! (z,3He) residue position
   integer*4 :: MT849     !! (z,a) residue position

!! Global correction factors    
   real*8 ::  TOTred  = 1.0D0    !! total x-sec scale factor
   real*8 ::  FUSred  = 1.0D0    !! fusion x-sec scale factor
   real*8 ::  ELAred  = 1.0D0    !! elastic x-sec scale factor
   real*8 ::  CELred  = 1.0D0    !! compound elastic scale factor
   real*8 ::  FCCred  = 1.0d0    !! dicrete collective levels scaling factor
   real*8 ::  FCOred  = 1.0D0    !! scaling factor on DWBA cross sections
   real*8 ::  rTOTRED = 1.0D0    !! total x-sec scale factor in randomization
   real*8 ::  rFCCRED = 1.0D0    !! CC scale factor in randomizaton
   real*8 ::  rFUSRED = 1.0D0    !! fusion x-sec scale factor in randomization
   real*8 ::  rELAred = 1.0D0    !! elastic scale factor in randomization
   real*8 ::  rCELred = 1.0D0    !! compound elastic x-sec scale factor in randomization
   real*8 ::  rCINred(ndlv) = 1.0D0 !! compound inelastic x-sec scale factor in randomization 

!! Input options
!! (to covert eventually to logical)
   integer*4 :: FITomp = 0    !! fit OM parameters
   integer*4 :: INTerf = 1    !! include Engelbrecht-Weidenmueller transformation
   integer*4 :: KALman = 0    !! run sensitivity calculations for Kalman
   integer*4 :: LHMs   = 0    !! run HMS calculations
   integer*4 :: MSC    = 1    !! run MSC calculations
   integer*4 :: MSD    = 1    !! run MSD calculations

   !! (non-convertable to logical)
   integer*4 :: LHRtw   = 1      !! selects Widths Fluctuation model and its elastic enhacment option
   integer*4 :: IOUt    = 3      !! amouint of output
   integer*4 :: LEVtarg = 1      !! level in which target is
   integer*4 :: NEMn    = 2      !! number of neutrons to follow
   integer*4 :: NEMp    = 1      !! number of protons to follow
   integer*4 :: NEMa    = 1      !! number of alphas to follow
   integer*4 :: NEMc    = 0      !! number of clusters to follow
   integer*4 :: NEXreq  = 80     !! number of requested bins in the continum
   integer*4 :: NHMs    = 0      !! number of Monts Carlo events in HMS
   real*8    :: EIN     = 5.0d0  !! incident projectile energy
   real*8    :: FITlev  = 0.0d0  !! controls fitting of cumulative number of discrete levels
   real*8    :: GCAsc   = 1.0d0  !! controls gamma cascade in a CN

!! others
   integer*4 :: ICOmpff      !! controls use of compressional factor in MSD (0 - off, 1 - on)
   integer*4 :: IX4ret       !! controls EXFOR retrieval
   integer*4 :: PESpin       !! cntrols spin cut-off factor in exciton model (0 - 2, 1 - n)
   integer*4 :: NNG_xs       !! controls gamma production cross sections printout (0 - no , 1 - yes)
   integer*4 :: F_Print      ! likely to be deleted
   integer*4 :: NPRIm_g      !! controls primary gammas (0 - no, 1 - yes)
   integer*4 :: NPAirpe      !! controls pairing correction in PCROSS (0 - no, 1 - yes)
   integer*4 :: FHMs         !! controls level densities in HMS (0 - 3)
   integer*4 :: IDEfcc       !! number of multipoles used to define a collective level deformation
   integer*4 :: IFLuc        ! likely to be deleted
   integer*4 :: IGE1         !! GDR ??
   integer*4 :: IGE2         !! GQR ??
   integer*4 :: IGM1         !! GMR ??
   integer*4 :: IOMwritecc   !! controls writing in tlf
   integer*4 :: IOPsys       !! 0 Linux, 1 Windows
   integer*4 :: IOPran       !! MC sampling off by default, 'RANDOM' turns it on
   integer*4 :: IDDfig(3)    !! position of 30, 150 deg in angular distributions (1 for neutrons)
   integer*4 :: IWArn        !! selects type of warning
   integer*4 :: JCUtcoll     !! maximum spin for DWBA collective levels
   integer*4 :: KEY_gdrgfl   !! paameter to set shape of E1 strength function and GDR parameters
   integer*4 :: KEY_shape    !! paameter to set shape of E1 strength function and GDR parameters
   integer*4 :: KTRompcc     !! o. m. parameters RIPL catalog number 
   integer*4 :: LMAxcc       !! max l for CC collective levels
   integer*4 :: MODelecis    !! transfer IMOdel to local common (IMOdel selects spherical or type of CC calculations)
   integer*4 :: MAXmult      !! max. gamma multiplicity
   integer*4 :: NACc         !! number of additional channels 
   integer*4 :: ND_nlv       !! number of collective states to be considered
   integer*4 :: NEJcm        !! number of ejectiles
   integer*4 :: NANgela      !! number of angles in angular distributions
   integer*4 :: IHFnew       !! modified Hauser-Feshbach to include Tlj(E) (not default)
   integer*4 :: NLW          !! number of partial waves
   integer*4 :: NNUcd        !! number of nuclei being decayed
   integer*4 :: NNUct        !! total number of nuclides involved (includes non-dcaying residues)
   integer*4 :: NOUt         !! controls size of output
   integer*4 :: NPRoject     !! index pointing to projectile type
   integer*4 :: NRBar        !! number of fission barriers
   integer*4 :: NRWel        !! number of wells in the potential surface (fission) 
   integer*4 :: NSCc         !! number of inelastic surface channels in CCFUS
   integer*4 :: NTArget      !! index of target in the list of nuclei
   integer*4 :: NENdf        !! controls ENDF formatting (0 no fmt)
   integer*4 :: NEXclusive   !! number of exclusive nuclei
   integer*4 :: NDAng        !! DIMENSION: for angles in DA
   integer*4 :: ICAlangs     !! related to number of angles in DA?
   integer*4 :: FISspe       !! controls calculation of PFNS (0 - no, 1 - LANL, 2 - Kornilov)
   integer*4 :: SFAct        !! controls output of S-factor
   integer*4 :: IPArcov      !! counter of sampled parameters
   integer*4 :: ngamm_tr     !! max l 
   integer*4 :: nfiss_tr     !! controls consideration of gamma emission in HF in ECIS
   integer*4 :: PLcont_lmax(NDEX)           !! maximum order of Legendre polynomials
   integer*4 :: IDNa(ndregions,ndmodels)    !! array of models usage in different channels

   LOGICAL  :: FIRst_ein = .true.      !! indicates the first incident energy
   LOGICAL  :: FUSread = .false.       !! read fuson ross sections 
   LOGICAL  :: EXClusiv = .false.      !! exclusive nucleus
   LOGICAL  :: CN_isotropic = .false.  !! HF is isotropic
   LOGICAL  :: CCCalc                  !! CC calculations
   LOGICAL  :: DEFault_energy_functional
   LOGICAL  :: DEFormed
   LOGICAL  :: FILevel       !! ??
   LOGICAL  :: OMParfcc      !! is OM parameter file not empty
   LOGICAL  :: OMPar_riplf   !! does OM parameter file exist
   LOGICAL  :: RELkin        !! relativistic kinematics
   LOGICAL  :: SDRead        !! fusion spin distribution read from SDREAD file
   LOGICAL  :: SOFt          !! soft rotor
   LOGICAL  :: NUBarread     !! read nu-bar
   LOGICAL  :: BENchm        !! controls if benchmark calculation is requested
   LOGICAL  :: CALctl        !! controls use of calculated transmission coefficients for both projectile and ejectiles
   LOGICAL  :: DYNam         !! dynamically deformed
   LOGICAL  :: COLfile       !! file with collective levels exists
   LOGICAL  SENsita       !! calculate sensitivities for Kalman

   real*8 :: EXCessmass(0:130,0:400)    !!   
   real*8 :: RESmas(0:130,0:400)        !!
   real*8 :: TFB              !! fission related (barrier) ??
   real*8 :: TDIrect          !! seems not used ??
   real*8 :: ADIv             !! type of level density
   real*8 :: EHRtw            !! incident energy limit for using width fluctuation correction
   real*8 :: BETav            !! viscosity parameter in Eqs. \ref{diss1}, \ref{diss2}
   real*8 :: BFUs             !! fusion barrier height in the distributed barrier model
   real*8 :: BUNorm           !! breakup normalization
   real*8 :: NTNorm           !! nucleon transfer reactions normalization (Kalbach)
   real*8 :: COMega           !! width of Coulomb barrier 
   real*8 :: CETa             !! constant in ECIS ??
   real*8 :: CHMs             !! multiplies default damp rate in HMS
   real*8 :: CRL              !! critical l-value for HI fusion
   real*8 :: CHMax            !! Max hole number in PCROSS set to CHMax*sqrt(g*U)
   real*8 :: CSFis            !! fission cross section
   real*8 :: CSFus            !! fusion cross section
   real*8 :: CSGdr1           !! GDR cross section of first peak
   real*8 :: CSGdr2           !! GDR cross section of second peak
   real*8 :: COEf             !! scales fusion cross section
   real*8 :: ELCncs           !! CN elastic cross section is PIx4*ELCncs
   real*8 :: CSMsc(0:2)       !! MSC emission cross section (n or p)
   real*8 :: CSO              !! ECIS related ??
   real*8 :: CSRead           !! Controls HI fusion cross section determination (see manual)
   real*8 :: D1Fra            !! Ratio of the spreading GDR width to the total GDR width in MSC
   real*8 :: DE               !! contiuum discretization energy bin width 
   real*8 :: DEFga            !! Amplitude of the Gaussian term defined by Eq. \ref{BfJfade};  positive increases fission barrier
   real*8 :: DEFgp            !! $J_{G}$ (spin position) of the Gaussian (term Eq. \ref{BfJfade})
   real*8 :: DEFgw            !! $\Delta J_{G}$ (width in spin) in the Gaussian (term of Eq. \ref{BfJfade})
   real*8 :: DEFpar           !! Coefficient \emph{b} in Eq. \ref{defor}
   real*8 :: DEFprj           !! Deformation of the projectile
   real*8 :: DEGa             !! probably related to DEGAS and NOT used???
   real*8 :: DENhf            !! Hauser-Feshbach denominator
   real*8 :: DERec            !! Bin width in recoil spectra     
   real*8 :: DFUs             !! Diffuseness in the transmission coefficients of HI fusion
   real*8 :: DIRect           !! Controls use of direct models
   real*8 :: DIToro           !! Factor in energy increase of GDR width
   real*8 :: DV               !! Parameter used to adjust the barrier in CCFUS (Default 20) 
   real*8 :: DEFdyn           !! Multiplies DWBA dynamical deformation 
   real*8 :: DEFsta           !! scales CC static deformation 
   real*8 :: ECUtcoll         !! Cut-off energy for collective DWBA levels
   real*8 :: EGDr1            !! GDR energy of first peak
   real*8 :: EGDr2            !! GDR energy of second peak
   real*8 :: EINl             !! Incident energy in LAB   
   real*8 :: SUMlev_alf       !! Total population of levels in prime alpha reside
   real*8 :: EDDfig           !! If >0 DE and DD spectra will be plotted at incident energy EDDfig (MeV) 
   real*8 :: EWSr1            !! Scales gamma strength for the first GDR hump
   real*8 :: EWSr2            !! Scales gamma strength for the second GDR hump
   real*8 :: EX1              !! Initial number of excitons that are neutrons
   real*8 :: EX2              !! Initial number of excitons that are neutrons
   real*8 :: EXCn             !! Excitation energy of compound nucleus
   real*8 :: EXPush           !! Extra-push energy in HI fusion
   real*8 :: FCC              !! FCC parameter in CCFUS
   real*8 :: EMInmsd          !! incident energy at which MSD starts 
   real*8 :: GDIv             !! factor defining p-h level densities in MSC and PCROSS (A/GDIv)
   real*8 :: GDResh           !!
   real*8 :: GDRspl           !!
   real*8 :: GDRwa1           !!
   real*8 :: GDRwa2           !!
   real*8 :: GDRweis          !!
   real*8 :: GGDr1            !!
   real*8 :: GGDr2            !!
   real*8 :: GDRdyn           !!
   real*8 :: DXSred           !!  
   real*8 :: GST              !!
   real*8 :: LQDfac           !!
   real*8 :: MFPp             !!
   real*8 :: MOMortcrt        !!
   real*8 :: PEQc             !!
   real*8 :: PEQcont          !!
   real*8 :: PI               !!
   real*8 :: PIx4             !!
   real*8 :: WIDcoll          !!
   real*8 :: QDFrac           !!
   real*8 :: QFIs             !!
   real*8 :: REDsef           !!
   real*8 :: Emax_tlj         !!
   real*8 :: SHNix            !!
   real*8 :: SHRd             !!
   real*8 :: SHRj             !!
   real*8 :: SHRt             !!
   real*8 :: SIG              !!
   real*8 :: STMro            !!
   real*8 :: TEMp0            !!
   real*8 :: TORy             !!
   real*8 :: TOTcsfis         !!
   real*8 :: TRUnc            !!
   real*8 :: XNI              !!
   real*8 :: DBRkup           !!
   real*8 :: hnorm            !! ROHFBSADDLE
   real*8 :: MOMparcrt        !!
   real*8 :: CSDbrkup(6)      !!
   real*8 :: CSEat(ndecse,2)  !! total DDXS at two angles and 1 ejectile  
   real*8 :: gamm_tr(10)      !!
   real*8 :: CINred(NDLV)     !!
   real*8 :: CSEpg(NDLV)      !!
   real*8 :: ENPg(NDLV)       !!
   real*8 :: TUNetl(NDLW)     !!
   real*8 :: REDmsc(NDLW,2)   !!
   integer*4  PL_lmax(NDLV)   !!
   real*8   PL_CN(0:(2*NDLW),NDLV)           !!
   real*8   PL_CNcont(0:(2*NDLW), NDEX)      !!
   real*8   fiss_tr(NDLW,2)                  !!
   real*8   ATIlnoz(NDZmax)                  !!

!! Fission parabolas (make into type)
   integer*4 :: NRFdis(nfparab)              !!
   real*8    :: AFIs(nfparab)                !!
   real*8    :: HCOnt(nfparab)               !!
   real*8    :: EFB(nfparab)                 !!
   real*8    :: DEFfis(nfparab)              !!
   integer*4 :: IPFdis(nftrans,nfparab)      !!
   real*8    :: SFDis(nftrans,nfparab)       !!
   real*8    :: H(nftrans,nfparab)           !!
   real*8    :: EFDis(nftrans,nfparab)       

!! Fission  humps  (make into type)
   integer*4 :: NRHump                 !!
   integer*4 :: NRBinfis(NFHump)       !!
   integer*4 :: BFF(NFHump)            !!
   real*8    :: AWF(NFHUMP)            !!
   real*8    :: ECFis(NFHUMP)          !!
   real*8    :: ECDamp(NFHUMP)         !!
   real*8    :: DEStepp(NFHump)        !!
   real*8    :: DELtafis(NFHump)       !!
   real*8    :: SHCfis(NFHump)         !!
   real*8    :: GAMmafis(NFHump)       !!
   real*8    :: vibf12(NFHump)         !!
   real*8    :: vibfdt(NFHump)         !!
   real*8    :: vibfnorm(NFHump)       !!
   real*8    :: XMInn(NFHump)          !!
   real*8    :: barnorm(NFHump)        !!
   real*8    :: rohfbp_sd(NFHump)      !!
   real*8    :: rohfba_sd(NFHump)      !! ROHFBSADDLE
   real*8    :: rohfb_norm(NFHUMP)     !!
   real*8    :: WIMag(NFHump-1,3)      !!
   real*8    :: ENH_ld(3,NFHump)       !!
   real*8    :: UGRid(0:nfisenmax,NFHump)       !!
   real*8    :: ROFisp(nfisenmax,ndlw,2,NFHump) !!

!! Multi-modal fission (make into type)
   integer*4 :: BFFm(NFMOD)
   integer*4 :: NRBinfism(NFMOD)
   real*8    :: ROFism(NFISENMAX,NDLW,NFMOD)
   real*8    :: HM(NFTRANS,NFMOD)
   real*8    :: EFDism(NFTRANS,NFMOD)
   real*8    :: UGRidf(NFISENMAX,NFMOD)
   real*8    :: EFBm(NFMOD)
   real*8    :: XMInnm(NFMOD)
   real*8    :: AFIsm(NFMOD)
   real*8    :: DEFbm(NFMOD)
   real*8    :: SHCfism(NFMOD)
   real*8    :: DELtafism(NFMOD)
   real*8    :: GAMmafism(NFMOD)
   real*8    :: WFIsm(NFMOD)
   real*8    :: DEStepm(NFMOD)
   real*8    :: TFBm(NFMOD)
   real*8    :: TDIrm(NFMOD)
   real*8    :: CSFism(NFMOD)
   real*8    :: ECFism(NFMOD)
   real*8    :: VIBf12m(NFMOD)
   real*8    :: VIBfdtm(NFMOD)
   real*8    :: VIBfnormm(NFMOD)

! TYPE OMP_type !TO BE DIMENSIONED 0(or 1):NDNUC
   integer :: KTRlom(0:ndejc,0:ndnuc)
   real*8  :: SIGabs(ndetl,ndejc,ndnuc)
   real*8  :: VOM(0:ndejc,0:ndnuc)
   real*8  :: VOMs(0:ndejc,0:ndnuc)
   real*8  :: VSO(0:ndejc,0:ndnuc)
   real*8  :: WOMs(0:ndejc,0:ndnuc)
   real*8  :: WOMv(0:ndejc,0:ndnuc)
   real*8  :: WSO(0:ndejc,0:ndnuc)
   real*8  :: RVOm(0:ndejc,0:ndnuc)
   real*8  :: RVSo(0:ndejc,0:ndnuc)
   real*8  :: RWOmv(0:ndejc,0:ndnuc)
   real*8  :: RWOm(0:ndejc,0:ndnuc)
   real*8  :: RWSo(0:ndejc,0:ndnuc)
   real*8  :: AWSo(0:ndejc,0:ndnuc)
   real*8  :: AVOm(0:ndejc,0:ndnuc)
   real*8  :: AVSo(0:ndejc,0:ndnuc)
   real*8  :: AWOm(0:ndejc,0:ndnuc)
   real*8  :: AWOmv(0:ndejc,0:ndnuc)
   real*8  :: FNvvomp(0:ndejc,0:ndnuc)
   real*8  :: FNwvomp(0:ndejc,0:ndnuc)
   real*8  :: FNavomp(0:ndejc,0:ndnuc)
   real*8  :: FNwsomp(0:ndejc,0:ndnuc)
   real*8  :: FNasomp(0:ndejc,0:ndnuc)
   real*8  :: FNrvomp(0:ndejc,0:ndnuc)
   real*8  :: FNrwvomp(0:ndejc,0:ndnuc)
   real*8  :: FNrsomp(0:ndejc,0:ndnuc)
   real*8  :: XNAver(0:ndejc,NDEtl)
! END TYPE OMP_type

! TYPE Ejectile_type !TO BE DIMENSIONED 0(or 1):NDejc
!  (ndejc)
   character*2 :: SYMbe(0:ndejc)                !! symbol
   integer :: IZAejc(0:ndejc)                   !! Z*1000 + A
   integer :: NREs(0:ndejc)                     !!
   real*8  :: SEJc(0:ndejc)                     !! spin
   real*8  :: ZEJc(0:ndejc)                     !! Z
   real*8  :: AEJc(0:ndejc)                     !! A
   real*8  :: XNEjc(0:ndejc)                    !! N
   real*8  :: EJMass(0:ndejc)                   !! mass
   real*8  :: XMAss_ej(0:ndejc)                 !! mass ???
   real*8  :: CSMsd(0:ndejc)                    !! MSD (or preequilibrium) cross section
   real*8  :: CSGinc(ndejc)                     !!
   real*8  :: MAXj(0:ndejc)                     !!
   real*8  :: SCRtem(0:ndejc)                   !!
   real*8  :: BUReac(0:ndejc)                   !! breakup cross section for incident deuteron 
   real*8  :: NTReac(0:ndejc)                   !!
   ! Ejectile specific correction factors
   real*8  :: rTUNEPE(0:ndejc)                  !!
   real*8  :: TUNEpe(0:ndejc)                   !!
   real*8  :: TUNEbu(0:ndejc)                   !!
   real*8  :: TUNEnt(0:ndejc)                   !!
   !  (ndejc, ndecse)
   real*8  :: AUSpec(ndecse,0:ndejc)            !! auxiliary spectrum
   real*8  :: CSEmsd(ndecse,0:ndejc)            !! MSD (or preequilibrium) energy spectrum
   real*8  :: CSEt(ndecse,0:ndejc)              !! total emisssion spectrum
   real*8  :: CSEdbk(ndecse,0:ndejc)            !! breakup energy spectrum
   !  (ndejc, ndlv)   
   real*8  :: CSDirlev(ndlv,0:ndejc)            !! direct cross section to the level
   real*8  :: CSComplev(ndlv,0:ndejc)           !! compound cross section to the discrete level
   real*8  :: REClev(ndlv,0:ndejc)              !! recoil energy of the residue after emission of ejectile to the level 
   real*8  :: SCRtl(ndlv,0:ndejc)               !!
   !  (ndejc, other, other )   
   real*8  :: CSEa(ndecse,ndangecis,0:ndejc)    !! total energy-angle DDX
   real*8  :: CSAlev(ndangecis,ndlv,0:ndejc)    !! x-sec angular distribution to the level
   !  (ndejc, other, other, other )   
   real*8  :: SCRt(ndex,ndlw,2,0:ndejc)         !! scratch matrix  
   real*8  :: TLJ(ndetl,ndlw,3,ndejc)           !! T_lj (emission energy, l, j)
! END TYPE Ejectile_type

! TYPE Nucleus_type !TO BE DIMENSIONED 0(or 1):NDNUC
   character*2  :: SYMb(0:ndnuc)          !! chemical symbol of the nucleus
   character*21 :: REAction(1:ndnuc)      !! reaction string associated with the nucleus
   logical :: FISsil(1:ndnuc) = .false.   !! is it fissile?
   integer :: A                           !! A of the nucleus
   integer :: Z                           !! Z of the nucleus
   real*8  :: AMAss                       !! mass of the nucleus    
   real*8  :: XZ(0:ndnuc)                 !! Z of the nucleus (real) 
   real*8  :: XN(0:ndnuc)                 !! number of neutrons in the nucleus
   real*8  :: XMAss(0:ndnuc)              !! mass of the nucleus
   real*8  :: HIS(0:ndnuc)                !! ground state spin
   real*8  :: SHLlnor(0:ndnuc)            !! shell normalization fcator
   real*8  :: POPmax(1:ndnuc)             !! maximum continuum cell (E,J,pi) population 
   real*8  :: POPcon(1:ndnuc)             !! integrated population of the continuum 
   real*8  :: POPdis(1:ndnuc)             !! summed opulation of all discrete levels
   real*8  :: CSPfis(0:ndnuc)             !!
   real*8  :: CSPopul(1:ndnuc)            !! population of the nucleus ???
   real*8  :: CSPrd(ndnuc)                !! cumulative production x-sec of the nucleus
   real*8  :: DEPart(1:ndnuc)             !!
   real*8  :: DEFnor(0:NDNUC)             !!
   real*8  :: ATIlnor(0:ndnuc)            !! normalization factor on level density parameter 'a'
   real*8  :: DOBs(0:ndnuc)               !! S-wave D-observed
   real*8  :: CSInc(1:ndnuc)              !!
   real*8  :: GTIlnor(0:ndnuc) = 1.0      !! PE g normalization fcator
   real*8  :: QQInc(0:ndnuc)              !!
   real*8  :: QPRod(0:ndnuc)              !! 
   integer :: NLVc(0:ndnuc)               !! total number of levels used in the calculations
   integer :: NLVt(0:ndnuc)               !! total number of levels
   integer :: NLV(0:ndnuc)                !! number of the level after which continuum starts
   real*8  :: ECOnt(0:ndnuc)              !! energy the continuum starts  
   real*8  :: ECUt(0:ndnuc)               !! energy the discrete levels end
   real*8  :: EMAx(1:ndnuc)               !! maximum excitation energy of the nucleus
   integer :: ENDf(0:ndnuc)               !! exclusive (1) or inclusive (0) ENDF treatment
   integer :: JSTab(1:ndnuc)              !! J of stability of the nucleus
   integer :: PIZA(0:ndnuc)               !!
   integer :: ncontr(0:ndnuc)             !!
   integer :: NCOmp(0:ndnuc)              !!
   integer :: NEX(1:ndnuc)                !! number of bins in the continuum of excitation energies
   integer :: NSTored(0:ndnuc)            !!
   integer :: INExc(0:ndnuc)              !!
   integer :: ISProd(0:ndnuc)             !!
   integer :: NRSmooth(0:ndnuc)           !!
   integer :: iugMax(0:ndnuc)             !!

   real*8  :: DEF(ndlw,0:ndnuc)           !! dynamical l-dependent deformation (for lev. den.) 
   real*8  :: SHCjf(ndlw,1:ndnuc)         !!
   real*8  :: YRAst(ndlw,1:ndnuc)         !! max l for a nucleus
   real*8  :: UEXcit(ndecse,1:ndnuc)      !! nucleus excitation bin energies 
   
   integer :: IOMwrite(0:ndejc,0:ndnuc)   !!
   integer :: ENDfp(0:ndejc,0:ndnuc)      !! print (1) or not (0) exclusive spectra for ejectile 
   integer :: IRElat(0:ndejc,0:ndnuc)     !!
   integer :: NEXr(0:ndejc,1:ndnuc)       !! number of bin in the continuum for the residual after ejectile emission
   real*8  :: EEFermi(0:ndejc,0:ndnuc)    !! Fermi energy for the residual after ejectile emission
   real*8  :: Q(0:ndejc,0:ndnuc)          !! Q-value for the ejectile in the current nucleus
   real*8  :: RCOul(0:ndejc,0:ndnuc)      !! Coulomb barrier for the ejectile from the current nucleus
   real*8  :: RNOnl(0:ndejc,0:ndnuc)      !!
   real*8  :: ACOul(0:ndejc,0:ndnuc)      !!
   real*8  :: OMEmax(0:ndejc,0:ndnuc)     !! 
   real*8  :: OMEmin(0:ndejc,0:ndnuc)     !!

   real*8  :: CSE(ndecse,0:ndejc,0:ndnuc)    !! x-sec energy distribution (spectrum)
   real*8  :: CSEfis(NDEPFN,0:ndejc,0:ndnuc) !! fission spectrum

   integer :: LMAxtl(ndetl,ndejc,1:ndnuc)    !! maximum l for T_l's 
   real*8  :: ETL(ndetl,ndejc,1:ndnuc)       !! energy grid for transmission coeficients
   real*8  :: TL(ndetl,ndlw,ndejc,1:ndnuc)   !! transmision coeficients T:: _:: l


   integer :: ISIsom(ndlv,0:ndnuc) 
   integer :: LVP(ndlv,0:ndnuc)        !! discrete level parity:: 
   real*8  :: POPlv(ndlv,1:ndnuc)      !! population of individual discrete::  levels
   real*8  :: ELV(ndlv,0:ndnuc)        !! discrete level energy
   real*8  :: XJLv(ndlv,0:ndnuc)       !! discrete level spin
   real*8  :: BR(ndlv,ndbr,3,0:ndnuc)  !! discrete level's decay (branching ratios, conversion, )

   real*8  :: EX(ndex + 1,1:ndnuc)     !! continuum bins' energy boundaries 

   real*8  :: POPbin(ndex,1:ndnuc)     !! population of the bin (E)
   real*8  :: POP(ndex,ndlw,2,1:ndnuc) !! population of every cell (E,J,pi) in the continuum

   real*8  :: POPcs(0:ndejc,0:ndexclus)  !! population cross section (exclusive nuclei)
   real*8  :: POPcse(0:ndex_d,0:ndejc,ndecsed,0:ndexclus) !! population energy spectrum (exclusive nuclei)  
   real*8  :: POPcsed(0:ndex_d,0:ndejc,ndecsed,0:ndexclus)  !! population energy ??? spectrum (exclusive nuclei)
   real*8  :: POPcseaf(0:ndex_d,0:ndejcd,ndecsed,0:ndexclus) !! fraction of energy_angle spectrum from the CN emission
   real*8  :: GDRpar(ndgdrpm,0:ndnuc)  !! Giant Dipol Resonance parametrs
   real*8  :: GMRpar(ndgmrpm,0:ndnuc)  !! Giant Magnetic Resonance parametrs 
   real*8  :: GQRpar(ndgqrpm,0:ndnuc)  !! Giant Quadrupole Resonance parametrs

   real*8  :: FIStga(1:ndnuc)
   real*8  :: FISb(ndlw,1:ndnuc)
   real*8  :: FISbar(1:ndnuc)
   real*8  :: FISden(1:ndnuc)
   real*8  :: FISdis(1:ndnuc)
   real*8  :: FISmod(1:ndnuc)
   real*8  :: FISopt(1:ndnuc)
   real*8  :: FISshi(1:ndnuc)
   real*8  :: HJ(ndnuc,nfparab)
   real*8  :: FISv_n(NFHUMP,1:ndnuc)
   real*8  :: FISh_n(NFHUMP,1:ndnuc)
   real*8  :: FISa_n(NFHUMP,1:ndnuc)
   real*8  :: FISd_n(NFHUMP,1:ndnuc)
   real*8  :: FISn_n(NFHUMP,1:ndnuc)

   real*8  :: RECcse(nderec,0:nderec,1:ndnuc)  !! recoil energy spectra

   real*8  :: ROPaa(1:ndnuc)
   real*8  :: ROHfbp(1:ndnuc)
   real*8  :: ROHfba(1:ndnuc)
   real*8  :: ROHfbp_off(1:ndnuc) 
   real*8  :: ROHfba_off(1:ndnuc)
   real*8  :: om2_ig(0:NDNUC)
   real*8  :: delp_ig(0:NDNUC)
   real*8  :: atil_ig(0:NDNUC)
   real*8  :: dshift_ig(0:NDNUC)
   real*8  :: SHC(0:ndnuc)

   real*8  :: RO(ndex,ndlw,2,1:ndnuc) 
   real*8  :: ROF(ndex,ndlw,1:ndnuc)
   real*8  :: ROPar(ndropm,1:ndnuc)

   real*8  :: SFIom(0:ndejc,0:ndnuc)

   real*8  :: TNUc(ndecse,1:ndnuc)
   real*8  :: TNUcf(ndecse,1:ndnuc)
   real*8  :: TUNe(0:ndejc,0:ndnuc)
   real*8  :: rTUNe(0:ndejc,0:ndnuc)
   real*8  :: TUNEfi(0:ndnuc)
   real*8  :: rTUNEfi(0:ndnuc)
   
!   TYPE (OMP_type), pointer :: OMP(:)   !! OM parameters
!END TYPE Nucleus_type


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
! TYPE(Nucleus_type), ALLOCATABLE, TARGET :: nucleus(:)      !! nuclei

END MODULE global_mod