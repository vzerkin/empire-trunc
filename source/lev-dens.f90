   !cc   * $Author: mwherman $
   !cc   * $Date: 2024-06-05 23:42:55 +0200 (Mi, 05 Jun 2024) $
   !cc   * $Id: lev-dens.f90 5575 2024-06-05 21:42:55Z mwherman $
   
   
   
   !CC
   !CC    Following routines/functions are called from INPUT or other Empire segments
   !CC
   !CC    ROEMP                 - Empire specific level densities
   !CC    |-> PRERORITO
   !CC    |-> PRERO
   !CC    |-> BNDG
   !CC    |-> LEVFIT
   !CC        |-> SIGMAK
   !CC        |-> SHCFADE
   !CC    |-> EGSMsys
   !CC    |-> DAMIRO_CRT
   !CC        |-> FSHELL
   !CC    |-> DAMIRO
   !CC        |-> FSHELL
   !CC        |-> SIGMAK
   !CC        |-> BSQ
   !CC        |-> ROBCS
   !CC            |-> COLL_KQ_EGSM
   !CC                |-> VIB_K_EGSM
   !CC                |-> VIB_Q_EGSM
   !CC                |-> ROT_Q_EGSM
   !CC        |-> RODEF
   !CC            |-> COLL_KQ_EGSM
   !CC                |-> VIB_K_EGSM
   !CC                |-> VIB_Q_EGSM
   !CC                |-> ROT_Q_EGSM
   !CC    |-> DAMIRO_FISHI
   !CC        |-> FSHELL
   !CC        |-> SIGMAK
   !CC        |-> BSQ
   !CC        |-> MOMFIT
   !CC        |-> ROBCS
   !CC        |-> RODEF
   !CC    |-> PLOT_ZVV_NumCumu
   !CC    |-> PLOT_ZVV_GSLD
   !CC
   !CC    ROGC                  - classic Gilbert-Cameron
   !CC    |-> PRERO
   !CC    |-> FSHELL
   !CC        |-> SHCFADE
   !CC    |-> PARITY_FRACTION (commented)
   !CC    |-> PLOT_ZVV_NumCumul
   !CC    |-> PLOT_ZVV_GSLD
   !CC
   !CC
   !CC    ROGSM                 - Ignatyuk's Generalised Superfluid model
   !CC    |-> GSMsys
   !CC    |-> PRERORITO
   !CC    |-> DAMIRO_CRT
   !CC    |-> BCS_FG
   !CC        |-> FSHELL
   !CC        |-> VIB_KQ_GSM
   !CC        |-> ROT_KQ_GSM
   !CC    |-> PLOT_ZVV_GSLD
   !CC    |-> PLOT_ZVV_NumCumul
   !CC
   !CC    DAMI_ROFIS           - Empire-specific saddle point lev. dens.
   !CC    |-> EGSMsys
   !CC    |-> DAMIRO_CRT
   !CC    |-> FSHELL
   !CC    |-> SIGMAK
   !CC    |->RODEFF
   !CC        |-> RODEF
   !CC        |-> COLL_KQ_FIS
   !CC    |-> ROBCSF
   !CC        |-> ROBCS
   !CC        |-> COLL_KQ_FIS
   !CC    |-> PLOT_ZVV_SadLD
   !CC
   !CC
   !CC    READ_SHELL_CORR
   !CC
   !CC    ROHFB
   !CC    |-> PRERO
   !CC        |-> SHCFADE
   !CC    |-> scale_ld
   !CC    |-> PLOT_ZVV_GSLD
   !CC    |-> PLOT_ZVV_NumCumul
   !CC
   !CC    DAMI_RO_HFB_FIS
   !CC    |-> HFB_FIS
   !CC    |-> PLOT_ZVV_SadLD
   
   
   
   subroutine ROEMP(Nnuc, Cf, Asaf)
      !!C
      !!C   *****************************************************************
      !!C   *                                                      CLASS:PPU*
      !!C   *                         R O E M P                             *
      !!C   *                                                               *
      !!C   *                                                               *
      !!C   * Calculates table of energy and spin dependent level densities *
      !!C   * using Empire-specific level density model. The fission lvel   *
      !!C   * densities are for the Heavy Ion reactions. For light particle *
      !!C   * induced fission saddle point level densities within Empire    *
      !!C   * specific model are provided by the DAMI_ROFIS routine.        *
      !!C   *                                                               *
      !!C   *                                                               *
      !!C   * INPUT:                                                        *
      !!C   *  NNUC - index of the nucleus                                  *
      !!C   *  CF   - 1. for the saddle point, 0. otherwise                 *
      !!C   *  ASAF - controls a=parameter at a saddle point                *
      !!C   *       - if ASAF.GE.0 it corresponds to the gamma-param.       *
      !!C   *         in the Ignatyuk formula (ASAF=0 selects               *
      !!C   *         asymptotic value for a)                               *
      !!C   *       - if ASAF.lt.0 asymptotic value of a-parameter          *
      !!C   *         times ABS(ASAF) is taken for at the saddle point      *
      !!C   *  BF controls shape of the nucleus                             *
      !!C   *     BF=0. stands for the saddle point                         *
      !!C   *     BF=1. stands for the oblate   yrast state                 *
      !!C   *     BF=2. stands for the prolate  yrast state                 *
      !!C   *     BF=3. stands for the triaxial yrast state                 *
      !!C   *       SCUTF - SPIN CUT-OFF FACTOR (0.146 IS RECOMMENDED)      *
      !!C   *                                                               *
      !!C   * OUTPUT:RO(.,.,NNUC) - LEVEL DENSITIES                         *
      !CC   *                                                               *
      !CC   *                                                               *
      !CC   *     ROEMP                                                     *
      !CC   *     |-> PRERORITO                                             *
      !CC   *     |-> PRERO                                                 *
      !CC   *     |-> BNDG                                                  *
      !CC   *     |-> LEVFIT                                                *
      !CC   *         |-> SIGMAK                                            *
      !CC   *         |-> SHCFADE                                           *
      !CC   *     |-> EGSMsys                                               *
      !CC   *     |-> DAMIRO_CRT                                            *
      !CC   *         |-> FSHELL                                            *
      !CC   *     |-> DAMIRO                                                *
      !CC   *         |-> FSHELL                                            *
      !CC   *         |-> SIGMAK                                            *
      !CC   *         |-> BSQ                                               *
      !CC   *         |-> ROBCS                                             *
      !CC   *             |-> COLL_KQ_EGSM                                  *
      !CC   *                 |-> VIB_K_EGSM                                *
      !CC   *                 |-> VIB_Q_EGSM                                *
      !CC   *                 |-> ROT_Q_EGSM                                *
      !CC   *         |-> RODEF                                             *
      !CC   *             |-> COLL_KQ_EGSM                                  *
      !CC   *                 |-> VIB_K_EGSM                                *
      !CC   *                 |-> VIB_Q_EGSM                                *
      !CC   *                 |-> ROT_Q_EGSM                                *
      !CC   *     |-> DAMIRO_FISHI                                          *
      !CC   *         |-> FSHELL                                            *
      !CC   *         |-> SIGMAK                                            *
      !CC   *         |-> BSQ                                               *
      !CC   *         |-> MOMFIT                                            *
      !CC   *         |-> ROBCS                                             *
      !CC   *         |-> RODEF                                             *
      !CC   *     |-> PLOT_ZVV_NumCumu                                      *
      !CC   *     |-> PLOT_ZVV_GSLD                                         *
      !CC   *                                                               *
      !CC   *                                                               *
      !CC   *                                                               *
      !CC   *****************************************************************
      !CC
      
      implicit none
      !*--ROEMP168
      include 'dimension.h'
      include 'global.h'
 
      real*8 :: A2, A23, ACR, ACRt, AP1, AP2, ATIl, BF, DEL, DELp, DETcrt, ECOnd, GAMma, SCR, TCRt, UCRt
      integer :: NLWst
      common /CRIT  / TCRt, ECOnd, ACRt, UCRt, DETcrt, SCR, ACR, ATIl
      common /PARAM / AP1, AP2, GAMma, DEL, DELp, BF, A23, A2, NLWst
 
      real*8 :: Asaf, Cf
      integer :: Nnuc
      intent (in) Cf
  
      real*8 :: aj, defit, dshif, dshift, ecrt, ellq, pi2, rotemp
      real :: FLOAT
      integer :: ia, in, iz, kk, nplot
      integer :: INT


      pi2 = PI*PI                                                     ! PARAM
      BF = 1.0
      if(Cf/=0.0D0)BF = 0.0D0
      dshift = 0.0D0
      A23 = A(Nnuc)**0.666667D0
      ia = INT(A(Nnuc))
      iz = INT(Z(Nnuc))
      in = ia - iz
      !
      if(NEX(Nnuc)<=0.0D0 .and. FITlev<=0.1)then
         write(8, &
      &'('' EXCITATION ENERGY TABLE FOR A='',I3,'' Z='',I3,         '' &
      &     HAS NOT BEEN DETERMINED BEFORE CALL OF PRERO'',// &
      &       ,'' LEVEL DENSITIES WILL NOT BE CALCULATED'')')ia, iz
         return
      endif
      
      if(EX(NEX(Nnuc), Nnuc)<=0.0D0 .and. FITlev<=0.1)return
      
      call PRERORITO(Nnuc)
      
      NLWst = NLW
      if((FISshi(Nnuc)>0.99D0.and.FISshi(Nnuc)<=1.01D0) .or. AEJc(0)>4)call PRERO(Nnuc)
      !-----set level density parameter systematics
      !-----EMPIRE-3.0-dependence
      call EGSMsys(AP1, AP2, GAMma, DEL, DELp, Nnuc)
      if(BF==0.0D0 .and. Asaf>=0.0D0)GAMma = Asaf
      ATIl = AP1*FLOAT(ia) + AP2*A23
      ATIl = ATIl*ATIlnor(Nnuc)
      !-----calculate crtical values
      call DAMIRO_CRT(ia, iz, SHC(Nnuc), IOUt, 0)
      if(BF==0.D0 .and. Asaf<0.0D0)ACR = ACRt
      !
      !-----fit of cumulative low-lying discrete levels
      !
      if(BF/=0.D0)call LEVFIT(Nnuc, nplot, dshif, dshift, defit)
      !
      if((FITlev>0.or.IOUt==6) .and. NLV(Nnuc)>3 .and. ENDf(Nnuc)<=1)call PLOT_ZVV_NumCumul(Nnuc)
      !
      !  IF (FITlev.GT.0 .AND. NLV(Nnuc).GT.3)
      !  &   Call PLOT_GNU_NumCumul(Nnuc,Dshift,DEL)
      !
      if(Q(1, Nnuc)==0.0D0)then
         rewind(25)
         call BNDG(1, Nnuc, Q(1, Nnuc))
      endif
      
      !  Ecrt = UCRt - DEL - dshift
      ecrt = UCRt - DEL
      if(ecrt<Q(1, Nnuc))then
         ellq = ecrt - (ELV(NLV(Nnuc), Nnuc) + LDShif(Nnuc))
      else
         ellq = Q(1, Nnuc) - (ELV(NLV(Nnuc), Nnuc) + LDShif(Nnuc))
         if(FIRst_ein .and. Nnuc==1)then
            write(8, *)
            write(8, *)' WARNING: ECRT= UCRT-DEL > BN, CALCULATED D0 MAY BE NOT ACCURATE'
            write(8, *)' WARNING: TUNING OF THE CN ATILNO MAY BE REQUIRED'
            write(8, *)
         endif
      endif
      
      do kk = 1, NEX(Nnuc)
         if(FITlev<=0.1D0 .or. EX(kk, Nnuc)>=ELV(NLV(Nnuc), Nnuc) + LDShif(Nnuc))then
      
            if(ecrt<Q(1, Nnuc))then
               if(EX(kk, Nnuc)<ecrt .and. ellq/=0.0D0)then
                  dshif = dshift*(ecrt - EX(kk, Nnuc))/ellq
               else
                  dshif = 0.D0
               endif
            elseif(EX(kk, Nnuc)<Q(1, Nnuc) .and. ellq/=0.0D0)then
               dshif = dshift*(Q(1, Nnuc) - EX(kk, Nnuc))/ellq
            else
               dshif = 0.D0
            endif
      
            if(BF==0.0D0)then
               call DAMIRO_FISHI(kk, Nnuc, Asaf, rotemp, aj)
            else
               call DAMIRO(kk, Nnuc, dshif, 0.0D0, rotemp, aj)
            endif
         endif
      enddo
      
      !  IF(IOUt.eq.6 .and.NLV(Nnuc).GT.3) Call PLOT_ZVV_GSLD(Nnuc)
      if((FITlev>0.or.IOUt==6) .and. NLV(Nnuc)>3 .and. ENDf(Nnuc)<=1)call PLOT_ZVV_GSLD(Nnuc)
      
      !  Added INITIALIZATION for ROPar(1,Nnuc) and ROPar(3,Nnuc)
      ROPar(1, Nnuc) = ACR
      ROPar(3, Nnuc) = DEL
      
      return
   end subroutine ROEMP

   
   
   subroutine DAMIRO(Kk, Nnuc, Dshif, Destep, Rotemp, Aj)
      !CC   
      !CC   *****************************************************************
      !CC        DAMIRO
      !CC        |-> FSHELL
      !CC        |-> SIGMAK
      !CC        |-> BSQ
      !CC        |-> ROBCS
      !CC            |-> COLL_KQ_EGSM
      !CC                |-> VIB_K_EGSM
      !CC                |-> VIB_Q_EGSM
      !CC                |-> ROT_Q_EGSM
      !CC        |-> RODEF
      !CC            |-> COLL_KQ_EGSM
      !CC                |-> VIB_K_EGSM
      !CC                |-> VIB_Q_EGSM
      !CC                |-> ROT_Q_EGSM
!CC   *****************************************************************
!CC
      
      implicit none
      !*--DAMIRO309
      include 'dimension.h'
      include 'global.h'
    
      real*8 :: A2, A23, ACR, ACRt, AP1, AP2, ATIl, BF, DEL, DELp, DETcrt, ECOnd, GAMma, SCR, TCRt, UCRt
      integer :: NLWst
      common /CRIT  / TCRt, ECOnd, ACRt, UCRt, DETcrt, SCR, ACR, ATIl
      common /PARAM / AP1, AP2, GAMma, DEL, DELp, BF, A23, A2, NLWst

      real*8 :: Aj, Destep, Dshif, Rotemp
      integer :: Kk, Nnuc
      intent (in) Destep, Dshif, Kk, Nnuc
      intent (inout) Rotemp
      
      real*8 :: ac, cigor, momort, mompar, stab, t, u
      logical :: bcs
      real*8 :: BSQ, FSHELL, ROBCS, RODEF
      integer :: egsm, i, ia, iz, lazy
      integer :: INT
      real :: REAL
       
      bcs = .true.                                                    ! PARAM
      !-----EGSM - J>>K (egsm=0) and EGSM (egsm=1)
      egsm = 0
      lazy = 0
      !
      ia = INT(A(Nnuc))
      iz = INT(Z(Nnuc))
      !
      if(Destep/=0.0D0)then
         u = (Kk - 1)*Destep + DEL + Dshif
      else
         u = EX(Kk, Nnuc) + DEL + Dshif
      endif
      if(u<=0.0D0)return
      if(u>UCRt)then
         u = u - ECOnd
         if(u<=0.0D0)return
         bcs = .false.
      else
         bcs = .true.
      endif
      if(lazy==1)then
         Aj = 0.D0
         u = 0.D0
         call SIGMAK(A(Nnuc), Z(Nnuc), DEF(1, Nnuc), 1.0D0, u, ac, Aj, mompar, momort, A2, stab, cigor)
      endif
      !-----do loop over angular momentum
      !  DO i = 1, 1     ! only the first momentum is used, valid only for even-even nuclei
      do i = 1, NLWst
         Aj = REAL(i) + HIS(Nnuc)
         !--------spin  dependent moments of inertia for yrast states by Karwowski
         !--------(spin dependent deformation beta calculated according to B.-Mot.)
         !--------temporary value of 'a' parameter needed for ground state deformation
         !--------damping (no surface correction)
         if(lazy==0)then
            ATIl = AP1*A(Nnuc) + AP2*A23
            ATIl = ATIl*ATIlnor(Nnuc)
            ac = ATIl*FSHELL(u, SHC(Nnuc), GAMma)
            !-----------here above FSHELL can become negative    !   !   !
            if(ac<=0.0D0)return
            call SIGMAK(A(Nnuc), Z(Nnuc), DEF(1, Nnuc), 1.0D0, u, ac, Aj, mompar, momort, A2, stab, cigor)
            !-----------'a' including surface dependent factor
            ATIl = AP1*A(Nnuc) + AP2*A23*BSQ(cigor)
            ATIl = ATIl*ATIlnor(Nnuc)
         endif
         ac = ATIl*FSHELL(u, SHC(Nnuc), GAMma)
         if(ac<=0.0D0)return
      
         if(A2<0.D0)then
            BF = 1
         else
            BF = 2
         endif
      
         if(bcs)then
            Rotemp = ROBCS(A(Nnuc), u, Aj, mompar, momort, A2, t, BF)
         else
            Rotemp = RODEF(A(Nnuc), u, ac, Aj, mompar, momort, t, YRAst(i, Nnuc), HIS(Nnuc), BF, EXPmax, A2, egsm)
         endif
      
         RO(Kk, i, 1, Nnuc) = Rotemp
         RO(Kk, i, 2, Nnuc) = Rotemp
      
         if(i==1)TNUc(Kk, Nnuc) = t
      enddo
      return
   end subroutine DAMIRO



   function ROBCS(A, U, Aj, Mompar, Momort, A2, T, Bf)
      !!C   
      !!C   ********************************************************************
      !!C   *                                                         CLASS:APU*
      !!C   *                        R O B C S                                 *
      !!C   * Calculates level densities in the framework of the BCS model     *
      !CC            ROBCS
      !CC            |-> COLL_KQ_EGSM
      !CC                |-> VIB_K_EGSM
      !CC                |-> VIB_Q_EGSM
      !CC                |-> ROT_Q_EGSM
      !CC   *                                                                  *
      !CC   ********************************************************************
      !CC   
      
      implicit none

      real*8 :: ACR, ACRt, ATIl, DETcrt, ECOnd, MOMo, MOMp, SCR, TCRt, UCRt
      common /CRIT  / TCRt, ECOnd, ACRt, UCRt, DETcrt, SCR, ACR, ATIl
      common /MOM   / MOMo, MOMp
      !
      ! Dummy arguments
      !
      real*8 :: A, A2, Aj, Bf, Momort, Mompar, T, U
      real*8 :: ROBCS
      intent (in) Aj, Bf, Momort, Mompar
      intent (inout) T
      !
      ! Local variables
      !
      real*8 :: const, det, dphi2, exp1, phi, phi2, rot_k, rot_q, ro_j, ro_u, s, seff2, vib_kq
      real*8 :: DEXP, DSQRT
      real*8 :: ro_pi

      !-----CONST=1/(2*SQRT(2 PI))
      data const/0.199471D0/                      !,denopt   !,bet
      
      ROBCS = 0.D0
      dphi2 = U/UCRt
      phi2 = 1.D0 - dphi2
      phi = DSQRT(phi2)
      T = 2.D0*TCRt*phi/LOG((phi + 1.D0)/(1.D0 - phi))
      s = SCR*TCRt*dphi2/T
      det = DETcrt*dphi2*(1.D0 + phi2)**2
      MOMp = Mompar*TCRt*dphi2/T
      if(MOMp<0.0D0)MOMp = 2.D0        ! RETURN
      MOMo = Momort*0.3333D0 + 0.6666D0*Momort*TCRt*dphi2/T
      if(MOMo<0.0D0)return
      seff2 = MOMp*T
      if(ABS(A2)>0.005D0)seff2 = MOMp**0.333D0*MOMo**0.6666D0*T
      if(seff2<=0.0D0)return
      ro_u = dexp(s)/dsqrt(det)
      exp1 = (Aj + 0.5)**2/(2.D0*seff2)
      if(exp1>20D0)return
      ro_j = const*(2.D0*Aj + 1.D0)/seff2**1.5*DEXP( - exp1)
      if(ro_j<1D-15)return
      ro_pi = 0.5D0
      ROBCS = ro_u*ro_j*ro_pi
      if(Bf==0.D0)return
      call COLL_KQ_EGSM(A, T, MOMo, A2, U, vib_kq, rot_k, rot_q)
      ROBCS = ROBCS*rot_k*rot_q*vib_kq
      
      return
   end function ROBCS



   function RODEF(A, E, Ac, Aj, Mompar, Momort, T, Yrast, Ss, Bf, Expmax, A2, Egsm)
      !!c   *********************************************************************
      !!c   *                                                         class:ppu *
      !!c   *                         R O D E F                                 *
      !!c   *                                                                   *
      !!c   *  Calculates spin dependent level densities (for a single parity)  *
      !!c   *  in the dynamical approach.                                       *
      !!c   *  Different deformation at each spin could be considered if        *
      !!c   *  'egsm' is set to 1 in DAMIRO.                                    *
      !!c   *  Collective enhancement effects are taken into account including  *
      !!c   *  their energy fade-out.                                           *
      !!c   *                                                                   *
      !!C   *        RODEF
      !!C            |-> COLL_KQ_EGSM
      !!C                |-> VIB_K_EGSM
      !!C                |-> VIB_Q_EGSM
      !!C                |-> ROT_Q_EGSM
      !!c   *                                                                   *
      !!c   *********************************************************************
      !!c
      
      implicit none

      real*8 :: A, A2, Ac, Aj, Bf, E, Expmax, Momort, Mompar, Ss, T, Yrast
      integer :: Egsm
      real*8 :: RODEF
      intent (in) Ac, Aj, Bf, Egsm, Mompar, Ss
      intent (inout) Expmax, T, Yrast
      !
      ! Local variables
      !
      real*8 :: ak, arg, con, const, det, e1, pi, ro, rot_k, rot_q, ro_j, ro_pi, ro_u, s, seff, seff2, sort2,   &
                  & sum, u, vib_kq
      real*8 :: DSQRT
      integer :: i, k, kmin

      
      data const/0.01473144D0/, pi/3.1415926535897932D0/
      !-----CONST=1.0/(24.0*SQRT(2.0))/2.0
      !-----the last 2.0 takes into account parity (half to half)
      !-----BF controls shape of the nucleus
      !-----BF=0. stands for the saddle point         (rot. perpend. to symm.)
      !-----BF=1. stands for the oblate yrast state   (rot. paralel  to symm.)
      !-----BF=2. stands for the prolate yrast state  (rot. perpend. to symm.)
      !-----BF=3. stands for the triaxial yrast state (rot. perpend. to long )
      
      Expmax = 700.
      if(Ac<=0. .or. E<=0.D0)return
      !----
      RODEF = 0.D0
      T = DSQRT(E/Ac)
      seff2 = Mompar**0.333D0*Momort**0.6666D0*T
      !-----FG
      if(Egsm==0)then
         s = 2.*Ac*T
         det = 45.84*Ac**3*T**5
         ro_u = exp(s)/sqrt(det)
         ro_j = (1.D0/(2.D0*sqrt(2.D0*pi)))*(2.D0*Aj + 1.D0)/seff2**1.5*EXP( - (Aj + 0.5)**2/(2.D0*seff2))
         ro_pi = 0.5
         ro = ro_u*ro_j*ro_pi
         if(Bf==0.D0)then
            RODEF = ro
            return
         endif
         call COLL_KQ_EGSM(A, T, Momort, A2, E, vib_kq, rot_k, rot_q)
         RODEF = ro*rot_k*rot_q*vib_kq
      
         return
      endif
      
      !-----EGSM (classic Heavy Ion version, needs egsm variable set to 1 in DAMIRO)
      sum = 0.0
      if(Mompar<0.0D0 .or. Momort<0.0D0)then
         write(8, *)' WARNING: NEGATIVE MOMENT OF INERTIA FOR SPIN ', Aj
         write(8, *)' WARNING: 0 LEVEL DENSITY RETURNED BY RODEF'
         return
      endif
      
      if(Ac==0.0D0)then
         write(8, '(''ERROR: LEVEL DENS. PARAMETER A=0 IN RODEF'')')
         stop
      endif
      seff = 1.0/Mompar - 1.0/Momort
      !ms---yrast recalculated
      Yrast = Aj*(Aj + 1.)/(2.*Momort)
      e1 = E - Yrast
      if(e1<=0.0D0)return
      T = SQRT(e1/Ac)
      sort2 = Momort*T
      const = (16.*sqrt(6.*pi))**( - 1)
      con = const/Ac**0.25/SQRT(Mompar*T)
      
      if(Ss==( - 1.0D0))then
         arg = 2*SQRT(Ac*e1)
         if(arg<=( - Expmax))then
            sum = 0.0
         elseif(e1>1.0D0)then
            sum = EXP(arg)/e1**1.25
         else
            sum = EXP(arg)
         endif
      
         if(Aj<1.0D0)goto 100
      endif
      i = int(Aj + 1.)
      
      if(Ss==( - 1.0D0))then
         kmin = 2
      else
         kmin = 1
      endif
      
      do k = kmin, i
         ak = k + Ss
         if(e1<=0.0D0)return
         if(Bf/=1.0D0)then
            !-----------rotation perpendicular to the symmetry axis (prolate nucleus)
            u = e1 - 0.5*ak**2*seff
         else
            !-----------rotation parallel to the symmetry axis (oblate nucleus)
            u = e1 - 0.5*(Aj*(Aj + 1.) - ak**2)*ABS(seff)
         endif
         if(u<=0.0D0)exit
         arg = 2.0*SQRT(Ac*u)
         if(arg>( - Expmax))then
            if(u>1.0D0)then
               sum = sum + 2.0*EXP(arg)/u**1.25
            else
               sum = sum + 2.0*EXP(arg)
            endif
         endif
      enddo
      100 continue
      ro = con*sum
      if(Bf==0.D0)then
         RODEF = ro
         return
      endif
      call COLL_KQ_EGSM(A, T, Momort, A2, e1, vib_kq, rot_k, rot_q)
      !-----rot_K=1
      RODEF = ro*rot_q*vib_kq
      
      return
   end function RODEF



   subroutine COLL_KQ_EGSM(A, T, Momo, A2, U, Vib_kq, Rot_k, Rot_q)
      !!C***************************************************************
      !!C   Calculates collective enhancements and damping for EGSM and GSM
      !!C***************************************************************
      !
      
      implicit none
 
      real*8 :: A, A2, Momo, Rot_k, Rot_q, T, U, Vib_kq
      intent (in) A2, Momo
      intent (out) Rot_k, Rot_q, Vib_kq
 
      real*8 :: ftmp, qr, qv, vibrk

      !  To avoid Compiler warning
      ftmp = A2
      !-----vibrational enhancement factor (EMPIRE-2.19)
      call VIB_K_EGSM(A, T, vibrk)
      !-----damping of vibrational effects
      call VIB_Q_EGSM(T, qv)
      if(qv>=0.999D0)vibrk = 1.0
      Vib_kq = qv - vibrk*(qv - 1.)
      !-----rotational enhancement
      !c IF (ABS(A2).LT.0.05D0)THEN
      !    rot_K=1.d0
      !    rot_Q=1.d0
      !    return
      !c ENDIF
      Rot_k = Momo*T
      !-----damping of rotational effects
      call ROT_Q_EGSM(U, qr)
      Rot_q = 1.0 - qr*(1.0 - 1.0/(Momo*T))
      return
   end subroutine COLL_KQ_EGSM



   subroutine VIB_K_EGSM(A, T, Vibrk)
      !CCC  *****************************************************************
      !CCC  *  Liquid drop vibrational enhancement of level densities
      !CCC  *****************************************************************
      !
      
      implicit none
 
      real*8 :: A, T, Vibrk
      intent (in) A, T
      intent (out) Vibrk
 
      real*8 :: cost, ht, m0, pi, r0, sdrop
 
      data m0, pi, r0, ht/1.044D0, 3.1415926535897932D0, 1.26D0, 6.589D0/
      sdrop = 17.D0/(4.D0*pi*r0**2)
      cost = 3.D0*m0*A/(4.D0*pi*ht**2*sdrop)
      Vibrk = EXP(1.7D0*cost**(2.D0/3.D0)*T**(4.D0/3.D0))
   end subroutine VIB_K_EGSM



   subroutine VIB_Q_EGSM(T, Q)
      !!C   *****************************************************************
      !!C   *         DAMPING FOR VIBRATIONAL EFFECTS                       *
      !!C   * Q=0 FOR T=0, Q=1/2 FOR T=THALF   , Q=1 FOR T=INFINITY        *
      !!C   *****************************************************************
      !
      
      implicit none
 
      real*8 :: Q, T
      intent (in) T
      intent (out) Q
      real*8 :: arg, dt, thalf

      thalf = 1.
      dt = 0.1
      arg = (T - thalf)/dt
      Q = 1.0/(EXP((-arg)) + 1.0)

   end subroutine VIB_Q_EGSM



   subroutine ROT_Q_EGSM(E1, Qk)
      !!CC  *****************************************************************
      !!CC  * damping of rotational  effects with Fermi function independent
      !!CC  * of deformation and mass number (consistent with the builtin systematics)
      !!CC  *****************************************************************
      
      implicit none
 
      real*8 :: E1, Qk
      intent (in) E1
      intent (out) Qk
 
      real*8 :: dmpdiff, dmphalf
 
      Qk = 0.
      dmphalf = 40.
      dmpdiff = 10.
      Qk = 1./(1. + EXP((-dmphalf/dmpdiff))) - 1./(1. + EXP((E1-dmphalf)/dmpdiff))

   end subroutine ROT_Q_EGSM



   function BSQ(Cigor)
      
      implicit none
 
      real*8 :: BSQ
      real*8 :: Cigor
      intent (in) Cigor
      real*8 :: qigor
 
      qigor = ( - 0.00246 + 0.3912961*Cigor - 0.00536399*Cigor**2 - 0.051313*Cigor**3 + 0.043075445*Cigor**4) - 0.375
      if(qigor>0.077D0)then
         BSQ = 0.983 + 0.439*qigor
      else
         BSQ = 1.0 + 0.4*(Cigor - 1.0)**2
      endif
      return

   end function BSQ



   subroutine DAMIRO_CRT(Ia, Iz, Shcn, Iout, Ifis)
      !CC   
      !CC****************************************************
      !CC    |-> DAMIRO
      !CC        |-> FSHELL
      !CC        |-> SIGMAK
      !CC        |-> BSQ
      !CC        |-> ROBCS
      !CC            |-> COLL_KQ_EGSM
      !CC                |-> VIB_K_EGSM
      !CC                |-> VIB_Q_EGSM
      !CC                |-> ROT_Q_EGSM
      !CC        |-> RODEF
      !CC            |-> COLL_KQ_EGSM
      !CC                |-> VIB_K_EGSM
      !CC                |-> VIB_Q_EGSM
      !CC                |-> ROT_Q_EGSM
      !CC****************************************************
      !CC   
      
      implicit none
 
      real*8 :: A2, A23, ACR, ACRt, AP1, AP2, ATIl, BF, DEL, DELp, DETcrt, ECOnd, GAMma, SCR, TCRt, UCRt
      integer :: NLWst
      common /CRIT  / TCRt, ECOnd, ACRt, UCRt, DETcrt, SCR, ACR, ATIl
      common /PARAM / AP1, AP2, GAMma, DEL, DELp, BF, A23, A2, NLWst
 
      integer :: Ia, Ifis, Iout, Iz
      real*8 :: Shcn
      intent (in) Ia, Ifis, Iout
 
      real*8 :: ar, pi, pi2, xr
      real*8 :: FSHELL
      integer :: ix
      character(2) :: SMAT
 
      pi = 3.1415926535897932D0                                       ! PARAM
      pi2 = pi*pi
      TCRt = 0.567*DELp
      
      ar = ATIl*(1.0 + Shcn*GAMma)
      do ix = 1, 20
         xr = ar*TCRt**2
         ACRt = ATIl*FSHELL(xr, Shcn, GAMma)
         if(ABS(ACRt - ar)<=0.001D0*ACRt)goto 70
         ar = ACRt
      enddo
      write(8, *)' WARNING: SEARCH FOR CRITICAL A-PARAMETER HAS NOT CONVERGED FOR A=', Ia, ' Z=', Iz
      write(8, *)' WARNING: LAST ITERATION HAS GIVEN ACRT=', ACRt
      write(8, *)' WARNING: SETTING ACRT TO 0.1, EXECUTION CONTINUES'
      ACRt = max(ACRt, 0.1D0)
      
      70 continue
      if(ACRt<0.0D0)ACRt = 0.1D0
      ECOnd = 1.5*ACRt*DELp**2/pi2
      UCRt = ACRt*TCRt**2 + ECOnd
      !-----45.84 stands for (12/SQRT(pi))**2
      DETcrt = 45.84*ACRt**3*TCRt**5
      ACR = ATIl*FSHELL(UCRt, Shcn, GAMma)
      SCR = 2.*ACRt*TCRt
      
      if(Iout==6 .and. Ifis==0)then
         write(8, '(1X,/, ''  TOTAL LEVEL DENSITY (TWICE THE SINGLE PARITY) FOR '' ,I3,''-'',A2)') Ia, SMAT(Iz)
      
         write(8,'(2X,/,''  ATIL='', F6.3,''  ACRT='',F6.3,''  UCRT='', F6.3, ''  ECRT='', F6.3,&
      &  ''  ECOND='', F5.3,''  DET='', G11.3, ''  SCRT='',F6.3)')ATIl, ACRt, UCRt, UCRt - DEL, ECOnd, DETcrt, SCR
      endif
      
      return
   end subroutine DAMIRO_CRT



   subroutine SIGMAK(A, Z, B, Bf, E, Ac, Aj, Mompar, Momort, A2, Stab, Cigor)
      !!cc  ******************************************************************
      !!ccc *                                                                *
      !!ccc *                    S I G M A K                                 *
      !!ccc *                                                                *
      !!cc  *  Paralel and orthogonal spin cut-off paprameters calculated    *
      !!cc  *  following Vigdor and Karwowski (Phys.Rev.C26(1982)1068)       *
      !!cc  *  Calculates also def. parameter alpha2 (leg. pol. expansion)   *
      !!cc  *  in function of spin in terms of the ldm + dampped g.s. defor. *
      !!ccc *                                                                *
      !!ccc *  Input: A - nucleus mass number                                *
      !!ccc *         Z - nucleus atomic number                              *
      !!ccc *         B - ground state deformation (beta2)                   *
      !!ccc *        Bf >  0 yrast states                                    *
      !!ccc *           =  0 outer saddle (    AXIAL SYMMETRY,MASS  SYMMETRY *
      !!ccc *           = -1 outer saddle (    AXIAL SYMMETRY,MASS ASYMMETRY *
      !!ccc *           = -2 inner  point (NON-AXIAL SYMMETRY,MASS  SYMMETRY *
      !!ccc *         E - excitation energy                                  *
      !!ccc *        Ac - level density parameter                            *
      !!ccc *        Aj - spin                                               *
      !!ccc *                                                                *
      !!ccc * Output: Mompar - parallel moment of inertia                    *
      !!ccc *         Momort - orthgonal moment of inertia                   *
      !!ccc *             A2 - nuclear deformation EPS including damped      *
      !!ccc *                  static and dynamical deformation              *
      !!ccc *           Stab - maximum spin ensuring stability against       *
      !!ccc *                  fission                                       *
      !!ccc *          Cigor - ratio of the longest and shortest axis        *
      !!ccc *                  to calculate Igor's factor accounting         *
      !!ccc *                  for increase of the lev. den. parameter       *
      !!ccc *                  due to increased nuclear surface              *
      !!ccc *                                                                *
      !!ccc *                                                                *
      !!ccc *                                                                *
      !!ccc *                                                                *
      !!cc  ******************************************************************
      !!C   
      
      implicit none
 
      real*8 :: A, A2, Ac, Aj, B, Bf, Cigor, E, Momort, Mompar, Stab, Z
      intent (in) A, Ac, Aj, B, Bf, E, Z
      intent (out) Cigor, Momort, Stab
      intent (inout) A2, Mompar
 
      real*8 :: a4, arg, beta, bt, c1, c2, c3, damp, dt, eta, gamma, pi, r1, r2, r3, rbmsph, ry, t, tgscr, x,&
                  & y, ycrit
 
      pi = 3.1415926535897932D0
      if(A<=0.D0)return
      !-----Damping ground state deformation (DT=0.4 Tgscr=1.5 MeV)
      dt = 0.4
      t = SQRT(E/Ac)
      tgscr = 1.5
      damp = 1.0/(1.0 + EXP((t-tgscr)/dt))
      bt = B*damp
      
      !---- YBM : Y di Bohr-Mottelson, vol.2, pag.663
      !  YBM=2.1*AJ**2/A**2.33333
      eta = 1.0 - 1.7826*(A - 2.0*Z)**2/A**2
      x = 0.01965*Z**2/eta/A
      ycrit = 1.4*(1 - x)**2
      Stab = SQRT(ycrit*eta*A**2.33333/1.9249)
      y = 1.9249*Aj*(Aj + 1.0)/eta/A**2.33333
      if(y>ycrit)y = ycrit
      !-----calculation of dynamic deformation in terms of the ldm
      !-----saddle point
      if(Bf==0.0D0)then
         beta = 7./6.*SQRT(4*pi/5.)*(1.0 - x)*SQRT(4. - 15.*y/7./(1 - x)**2)
         arg = 1./SQRT(4. - 15.*y/7./(1 - x)**2)
         if(arg>1.0D0)arg = 1.0
         gamma = pi/3. - ACOS(arg)
      elseif(Bf<0.0D0)then
         beta = bt            ! the actual static saddle deformation is used (must be < 1.5    !   !   !)
         !  default: axial symmetry                     (outer saddle)
      
         gamma = pi/3.
      
         !  arbitrarily fixed nonaxiality of 10 degrees (inner saddle)
         if(Bf< - 1.50D0)gamma = pi/18.
         !-----yrast states
      elseif(Bf>0.0D0)then
         gamma = pi/3.
         beta = 7./6.*SQRT(4*pi/5.)*(1.0 - x)*( - 1. + SQRT(1. + 15.*y/7./(1-x)**2))
         beta = beta + bt         ! adding damped static deformation 'bt'
      endif
      
      r3 = 1. + SQRT(5./4./pi)*beta*COS(gamma - 2.*pi)
      r2 = 1. + SQRT(5./4./pi)*beta*COS(gamma - 4.*pi/3.)
      r1 = 1. + SQRT(5./4./pi)*beta*COS(gamma - 2.*pi/3.)
      
      Cigor = MAX(r1, r2, r3)
      ry = r2/r1
      A2 = (ry - 1.0)/(1.0 + 0.5*ry)
      if(A2>0.9D0)A2 = 0.9
      if(A2<( - 0.9D0))A2 = -0.9
      !---- next line (if uncommented) neglects all deformations
      !  A2=0
      if(A2<0.0D0)then
         c1 = -0.266
         c2 = -0.896
         c3 = -0.571
      else
         c1 = -0.700
         c2 = 0.663
         c3 = 0.286
      endif
      a4 = A2**2*(0.057 + 0.171*x + c2*y) + c3*A2*y
      a4 = a4/(1.0 - 0.37*x - c1*y)
      rbmsph = 0.01448*A**1.66667
      Mompar = (1.0 -   A2 + 0.429*A2**2 + 0.268*A2**3 - 0.212*A2**4 - 1.143*A2*a4 + 0.494*A2**2*a4 + 0.266*a4**2)*rbmsph
      Momort = (1 + 0.5*A2 + 1.286*A2**2 + 0.581*A2**3 - 0.451*A2**4 + 0.571*A2*a4 + 1.897*A2**2*a4 + 0.700*a4**2)*rbmsph
      
      !  Ignatyuk estimates
      !   mompar = rbmsph*(1. - (2./3.)*a2)
      !   momort = rbmsph*(1. + (1./3.)*a2)
      
      if(ABS(A2)<=0.001D0)Momort = Mompar
   end subroutine SIGMAK



   function FSHELL(X, Xs, Xg)
      
      implicit none
      
      real*8 :: FSHELL
      real*8 :: X, Xg, Xs
      intent(in) X, Xg, Xs

      if(X>0.01D0)then
         FSHELL = 1.0 + (1.0 - EXP((-Xg*X)))*Xs/X
      else
         FSHELL = 1 + Xg*Xs
      endif
      return

   end function FSHELL



   subroutine ROGSM(Nnuc)
      !!C   
      !!C*********************************************************
      !!C
      !!C                R O G S M
      !!C
      !!C   Generlaised Superfluid Model following A. Ignatyuk
      !!C   BCS calculations below critical eneergy and Fermi gass
      !!C   including vibration and reotational collective enhancements
      !!C   above critical energy.
      !!C
      !!C    ROGSM
      !!C    |-> GSMsys
      !!C    |-> PRERORITO
      !!C    |-> DAMIRO_CRT
      !!C    |-> BCS_FG
      !!C        |-> FSHELL
      !!C        |-> VIB_KQ_GSM
      !!C        |-> ROT_KQ_GSM
      !!C    |-> PLOT_ZVV_GSLD
      !!C    |-> PLOT_ZVV_NumCumul
      !!C
      !!C*********************************************************
      !!C   

      
      implicit none
      include'dimension.h'
      include 'global.h'
 
      real*8 :: A2, A23, ACR, ACRt, AP1, AP2, ATIl, BF, DEL, DELp, DETcrt, ECOnd, GAMma, SCR, TCRt, UCRt
      integer :: NLWst
      common /CRIT  / TCRt, ECOnd, ACRt, UCRt, DETcrt, SCR, ACR, ATIl
      common /PARAM / AP1, AP2, GAMma, DEL, DELp, BF, A23, A2, NLWst
 
      integer :: Nnuc
 
      real*8 :: ac, cga, def2, mm2, momort, mompar, om2, om3, pi2, rotemp, shcn, u
      real*8 :: FSHELL
      integer :: ia, iz, kk
      integer :: INT
 
      pi2 = PI*PI                                                     ! PARAM
      ia = INT(A(Nnuc))
      iz = INT(Z(Nnuc))
      
      call GSMsys(Nnuc, AP1, AP2, GAMma, DEL, DELp, om2, om3, cga)
      
      shcn = SHC(Nnuc)
      
      ATIl = AP1*A(Nnuc) + AP2*A23
      ATIl = ATIl*ATIlnor(Nnuc)
      
      u = Q(1, Nnuc) + DEL
      
      ac = ATIl*FSHELL(u, shcn, GAMma)
      if(ac<=0.0D0)return
      
      call PRERORITO(Nnuc)
      NLWst = NLW
      call DAMIRO_CRT(ia, iz, shcn, IOUt, 0)
      
      def2 = DEF(1, Nnuc)
      rotemp = 0.D0
      
      mm2 = .24*A(Nnuc)**.66666
      mompar = 0.608*ACRt*mm2*(1. - 0.6667*def2)
      momort = 0.608*ACRt*mm2*(1. + 0.3330*def2)
      
      do kk = 1, NEXreq
         u = EX(kk, Nnuc) + DEL
         call BCS_FG(Nnuc, kk, u, mompar, momort, NLWst, def2, om2, om3, cga, GAMma, shcn)
      enddo
      
      !  IF (IOUt.GE.6 .AND. NLV(Nnuc).GT.3) Call PLOT_ZVV_GSLD(Nnuc)
      if((FITlev>0.or.IOUt==6) .and. NLV(Nnuc)>3 .and. ENDf(Nnuc)<=1)then
         call PLOT_ZVV_GSLD(Nnuc)
      
         !  IF (FITlev.GT.0 .AND. NLV(Nnuc).GT.3) then
      
         !    Cumulative levels must be calculated for FITLEV>0
         !    as Ecut(Nnuc) is set to zero
      
         !    In a normal calculation, Ecut(NNuc)>0 and therefore the
         !    cumulative integral is wrongly calculated    !   !   !
         !    RCN, April 2012
         !    Call PLOT_GNU_NumCumul(Nnuc,0.d0,0.d0)
         call PLOT_ZVV_NumCumul(Nnuc)
      endif
      
      !  Added INITIALIZATION for ROPar(1,Nnuc) and ROPar(3,Nnuc)
      ROPar(1, Nnuc) = ac
      ROPar(3, Nnuc) = DEL
      return
   end subroutine ROGSM



   subroutine BCS_FG(Nnuc, Kk, U, Mompar, Momort, Nlwst, Def2, Om2, Om3, Cga, Gamm, Shcn)
      !CC   
      !CC   
      
      implicit none

      include 'dimension.h'
      include 'global.h'
  
      real*8 :: ACR, ACRt, ATIl, DETcrt, ECOnd, SCR, TCRt, UCRt
      common /CRIT  / TCRt, ECOnd, ACRt, UCRt, DETcrt, SCR, ACR, ATIl
 
      real*8 :: Cga, Def2, Gamm, Momort, Mompar, Om2, Om3, Shcn, U
      integer :: Kk, Nlwst, Nnuc
      intent (in) Kk, Momort, Mompar, Nlwst, Nnuc
      intent (inout) U
 
      real*8 :: ac, aj, arg, const, det, dphi2, mm2, momo, momp, phi, phi2, q2, q3, rho, rotemp, rot_kq, ro_j,  &
                  & ro_pi, ro_u, s, seff2, seff2ort, t, vib_kq
      real*8 :: DSQRT
      real*8 :: FSHELL
      integer :: i
      real :: REAL
 
      !-----CONST=1/(2*SQRT(2 PI))
      data const/0.199471D0/                                       ! CRIT
      !
      mm2 = .24*A(Nnuc)**.66666
      !
      if(U<=UCRt)then
      !-----BCS
         dphi2 = U/UCRt
         phi2 = 1.D0 - dphi2
         phi = DSQRT(phi2)
         t = 2.D0*TCRt*phi/LOG((phi + 1.D0)/(1.D0 - phi))
         s = SCR*TCRt*(1.D0 - phi2)/t
         det = DETcrt*(1.D0 - phi2)*(1.D0 + phi2)**2
         momp = Mompar*TCRt*(1 - phi2)/t
         if(momp<0.0D0)return
         momo = Momort*0.3333D0 + 0.6666D0*Momort*TCRt*(1.D0 - phi2)/t
         if(momo<0.0D0)return
      else
         !-----FG
         U = U - ECOnd
         ac = ATIl*FSHELL(U, Shcn, Gamm)
         if(ac<=0. .or. U<=0.D0)return
         t = DSQRT(U/ac)
         s = 2.*ac*t
         det = 45.84*ac**3*t**5
         momp = 0.608*ac*mm2*(1. - 0.6667*Def2)
         momo = 0.608*ac*mm2*(1. + 0.3330*Def2)
      endif
      
      seff2 = momp*t
      if(ABS(Def2)>0.005D0)seff2 = momp**0.333D0*momo**0.6666D0*t
      if(seff2<=0.0D0)return
      seff2ort = momo*t
      !-----collective enhancements
      if(Om2>0.)then
         call VIB_KQ_GSM(t, Om2, Cga, 5, q2)
      else
         q2 = 1.D0
      endif
      call VIB_KQ_GSM(t, Om3, Cga, 7, q3)
      call ROT_KQ_GSM(A(Nnuc), Def2, seff2ort, U, rot_kq)
      vib_kq = q2*q3
      ro_u = exp(s)/sqrt(det)
      ro_pi = 0.5D0
      
      do i = 1, Nlwst
         aj = REAL(i) + HIS(Nnuc)
         arg = s - (aj + 0.5D0)**2/(2.D0*seff2)
         if(arg<=0.0D0)cycle         !return
         ro_j = const*(2.D0*aj + 1.D0)/seff2**1.5*EXP( - (aj + 0.5)**2/(2.D0*seff2))
         rho = ro_u*ro_j*ro_pi
         rotemp = rho*rot_kq*vib_kq
         RO(Kk, i, 1, Nnuc) = rotemp
         RO(Kk, i, 2, Nnuc) = rotemp
         if(i==1)TNUc(Kk, Nnuc) = t
      enddo
      return
   end subroutine BCS_FG



   subroutine VIB_KQ_GSM(T, Om, Cga, Lam, Q)
      !!C   
      !!c   ********************************************************************
      !!c   *                                                          class:pu*
      !!c   *                      Q V I B R                                   *
      !!c   *                                                                  *
      !!c   * Calculates level density vibrational enhancement factor  using   *
      !!c   * Ignatyuk's formula including damping (see RIPL's)                *
      !!c   *                                                                  *
      !!c   * input: T - nuclear temperature                                   *
      !!c   *       OM - energy of the vibrational level                       *
      !!c   *      LAM - multipolarity (5 for 2+; 7 for 3- states)             *
      !!c   *                                                                  *
      !!c   * output: Q - vibrational enhancement factor due to the OM state   *
      !!c   *                                                                  *
      !!c   * calls:none                                                       *
      !!c   *                                                                  *
      !!c   ********************************************************************
      !!c
      
      implicit none
 
      real*8 :: Cga, Om, Q, T
      integer :: Lam
      intent (in) Cga, Lam, Om, T
      intent (inout) Q
      
      ! Local variables
      
      real*8 :: DEXP, DLOG
      real*8 :: fn, gam, s, u
      
      Q = 1.D0
      if(T<0.01D0)return
      gam = Cga*(Om**2 + (2.D0*3.1415926535897932D0*T)**2)
      fn = DEXP( - gam/Om/2.D0)/(DEXP(Om/T) - 1.D0)
      if(fn<0.D0)return
      u = Lam*Om*fn
      s = Lam*((1.D0 + fn)*DLOG(1.D0 + fn) - fn*DLOG(fn))
      Q = DEXP(s - u/T)
      if(Q<1.D0)Q = 1.D0
      return

   end subroutine VIB_KQ_GSM



   subroutine ROT_KQ_GSM(A, Bet, Sig4, U, Qr)
      !!CC   ********************************************************************
      !!      QROT INCLUDING DAMPING 
      !!CC   ********************************************************************
      
      implicit none
 
      real*8 :: A, Bet, Qr, Sig4, U
      intent (in) A, Bet, Sig4, U
      intent (inout) Qr
 
      real*8 :: dcr1, ucr1

      ucr1 = 120.*Bet*Bet*A**.33333
      dcr1 = 1400.*Bet*Bet/A**.66666
      if(Bet>0.D0)then
         Qr = 1./(1. + EXP((U-ucr1)/dcr1))
         Qr = Qr*(Sig4 - 1.) + 1.
      else
         Qr = 1.D0
      endif
      return

   end subroutine ROT_KQ_GSM



   subroutine PRERORITO(Nnuc)
      !CC   
      !CC   
      
      implicit none
      !*--PRERORITO1453
      include 'dimension.h'
      include 'global.h'
 
      real*8 :: A2, A23, AP1, AP2, BF, DEL, DELp, GAMma
      integer :: NLWst
      common /PARAM / AP1, AP2, GAMma, DEL, DELp, BF, A23, A2, NLWst
 
      integer :: Nnuc
      intent (in) Nnuc
 
      integer :: ia, iz
      integer :: INT
 
      !-----check of the input data ---------------------------------------
      ia = INT(A(Nnuc))                                               ! PARAM
      iz = INT(Z(Nnuc))
      if(NLW<=0)then
         write(8,'('' MAXIMUM NUMBER OF PARTIAL WAVES HAS NOT BEEN'','' DETRMINED BEFORE CALL OF PRERO'',//,&
         &     '' EXECUTION STOPPED'')')
         stop
      endif
      if(ia<=0 .or. iz<=0)then
         write(8,'('' A='',I3,'' AND/OR Z='',I2,''HAS NOT BEEN DETERMINED BEFORE CALL OF PRERO'',//,&
              &'' EXECUTION STOPPED'')')ia, iz
         stop
      endif
      if(Nnuc>NDNUC)then
         write(8,&
         &'('' PRERO  CALLED FOR A NUCLEUS INDEX NNUC='',I3,'' WHICH EXCEEDS DIMENSIONS'',/, &
         &'' CHECK THIS CALL OR INCREASE NDNUC TO'',I4, &
         &'' INsdimendion.h AND RECOMPILE'',//,''EXECUTION STOPPED'')')Nnuc, Nnuc
         stop
      endif
      if(EX(NEX(Nnuc), Nnuc)<=0.0D0 .and. FITlev<=0.1)return
      
      if(BF/=0.0D0)then
         RO(:, :, :, Nnuc) = 0.D0
      else
         ROF(:, :, Nnuc) = 0.D0
      endif
      
      return
   end subroutine PRERORITO



   subroutine PRERO(Nnuc)
      !!C
      !!C   ********************************************************************
      !!C   *                                                         CLASS:APU*
      !!C   *                        P R E R O                                 *
      !!C   *                                                                  *
      !!C   *                                                                  *
      !!C   * Prepares for level density calculations. checks for the          *
      !!C   * energy table determination, sets yrast energies, fission         *
      !!C   * barriers, scaling factor, and cleans up level density tables.    *
      !!C   *                                                                  *
      !!C   *                                                                  *
      !!C   * INPUT:NNUC - index of the nucleus                                *
      !!C   *       CF   - 1 for saddle point, 0 otherwise                     *
      !!C   *                                                                  *
      !!C   * calls: BARFIT                                                    *
      !!C   *           LPOLY                                                  *
      !!C   *        SHCFADE                                                   *
      !!C   *        SIGMAK                                                    *
      !!C   *                                                                  *
      !!C   * AUTHOR: M.HERMAN                                                 *
      !!C   * DATE:   11.NOV.1998                                              *
      !!C   * REVISION:1    BY:M Herman                 ON:08.Feb.2000         *
      !!C   *   Liquid drop stability limit revised. Myers & Swiatecki fission *
      !!C   * barriers for Z>102 introduced.                                   *
      !!C   *                                                                  *
      !!C   *                                                                  *
      !!C   ********************************************************************
      !!C
      
      implicit none

      include 'dimension.h'
      include 'global.h'
 
      real*8 :: A2, A23, AP1, AP2, BF, DEL, DELp, GAMma
      integer :: NLWst
      common /PARAM / AP1, AP2, GAMma, DEL, DELp, BF, A23, A2, NLWst
 
      integer :: Nnuc
      intent (in) Nnuc
 
      real*8 :: aj, cigor, ftmp, fx, momort, mompar, s, sb, sb0, sbnor, segnor, segs, selmax, stab, x, x0, x1,  &
                  & xi, xk
      real :: FLOAT
      integer :: i, ia, iz, j, jstabf, k, kstab, ldstab
      integer :: INT, MIN0, NINT
      real*8 :: SHCFADE
      real*8 :: xfis
 
      !-----check of the input data ---------------------------------------
      ia = INT(A(Nnuc))                                               ! PARAM
      iz = INT(Z(Nnuc))
      
      if(NLW<=0)then
         write(8, '('' MAXIMUM NUMBER OF PARTIAL WAVES HAS NOT BEEN'', '' DETRMINED BEFORE CALL OF PRERO'',//, &
         &'' EXECUTION STOPPED'')')
         stop
      endif
      if(ia<=0 .or. iz<=0)then
         write(8, '('' A='',I3,'' AND/OR Z='',I2, ''   HAS NOT BEEN DETERMINED BEFORE CALL OF PRERO'', //, &
         & '' EXECUTION STOPPED'')')ia, iz
         stop
      endif
      if(Nnuc>NDNUC)then
         write(8, '('' PRERO  CALLED FOR A NUCLEUS INDEX NNUC='' , &
               & I3,'' WHICH EXCEEDS DIMENSIONS'',/, '' CHECK THIS CALL OR INCREASE NDNUC TO'',I4, &
               & '' INsdimendion.h AND RECOMPILE'',//, ''EXECUTION STOPPED'')')Nnuc, Nnuc
         stop
      endif
      
      if(EX(NEX(Nnuc), Nnuc)<=0.0D0 .and. FITlev<=0.1)return
      !-----check of the input data ---- done -----------------------------
      if(NINT(FISshi(Nnuc))==1)then
         !-----check whether the nucleus is fissile
         FISsil(Nnuc) = .true.
         xfis = 0.0205*Z(Nnuc)**2/A(Nnuc)
         if(xfis<0.3D0)FISsil(Nnuc) = .false.
      endif
      !-----determination of the yrast and saddle point energies
      
      !-----determination of the LD rotational stability limit LDSTAB
      call SIGMAK(A(Nnuc), Z(Nnuc), 0.0D0, 1.0D0, 0.0D0, 15.0D0, 0.0D0, mompar, momort, ftmp, stab, cigor)
      kstab = int(stab)
      !-----set fission barrier at sky (just in case it is not calculated)
      sb0 = 1000.
      sb = 1000.
      if(iz>19 .and. iz<102)then
         call BARFIT(iz, ia, 0, sb0, segs, stab)
         ldstab = int(stab)
      else
         ldstab = kstab
      endif
      NLWst = NLW
      if(HIS(Nnuc)== - 0.5D0)then
         ldstab = ldstab - 1
         kstab = kstab - 1
      endif
      
      if(FISb(1, Nnuc)==0.0D0)then
         !-----determination of the fission barrier at J=0 (for Z.GE.102)
         !-----according to Myers&Swiatecki, Phys. Rev. C60(1999)014606
         if(iz>=102)then
            x0 = 48.5428
            x1 = 34.15
            xi = (A(Nnuc) - 2*Z(Nnuc))/A(Nnuc)
            xk = 1.9 + (Z(Nnuc) - 80.0)/75.0
            s = A(Nnuc)**0.666667D0*(1.0 - xk*xi**2)
            x = Z(Nnuc)**2/A(Nnuc)/(1.0 - xk*xi**2)
            fx = 0.0
            if(x<=x0 .and. x>=x1)fx = 0.000199749*(x0 - x)**3
            if(x<=x1 .and. x>=30.0D0)fx = 0.595553 - 0.124136*(x - x1)
            sb0 = s*fx
            write(8,'('' LIQUID DROP FISSION BARRIER FOR '',I3,''-'',A2, '' SET TO '',G10.5)') &
                       INT(A(Nnuc)), SYMb(Nnuc), sb0
         endif
         !
         !--------determination of the yrast, saddle point energies and deformations
         !
         !--------do loop over angular momentum
         segnor = 1.0
         sbnor = 1.0
         jstabf = 0
      
         do j = 1, NLW
            aj = FLOAT(j - 1)
            call SIGMAK(A(Nnuc), Z(Nnuc), DEF(1, Nnuc), 1.0D0, 0.0D0, 15.0D0, aj, mompar, momort, ftmp, stab, cigor)
            !  CALL SIGMAK(A(Nnuc),Z(Nnuc),DEF(1,Nnuc),1.0D0,0.0D0,15.0D0,
            !  &                  aj,mompar,momort,beta,stab,cigor)
            !         IF (Cf.EQ.0.0D0) DEF(j,Nnuc) = beta    ! Commented to avoid using wrong beta out from SIGMAK
      
            if(iz>19 .and. iz<102)then
               sb = 0.0
               if(j - 1<=ldstab)call BARFIT(iz, ia, j - 1, sb, segs, selmax)
               if(j - 1==ldstab)segnor = segs/(aj*(aj + 1)/(2.0*momort))
               if(j - 1>ldstab)segs = aj*(aj + 1)/(2.0*momort)*segnor
               !------------- Yrast states redefined for normal states to avoid discontinuities
               !  as proposed by MS, except for HI induced reactions (AJEc(0)>4)
               if(AEJc(0)<=4.)segs = aj*(aj + 1)/(2.0*momort)            ! Jan 2011
            else
               !--------------out of the BARFIT range of applicability;
               !--------------fission barrier spin dependence is assumed to be  that of
               !--------------A=256 Z=102 and normalized at J=0 to the value of Myers &
               !--------------Swiatecki (SB0)
               call BARFIT(102, 256, j - 1, sb, segs, selmax)
               if(j==1)sbnor = sb0/sb
               sb = sb*sbnor
               segs = aj*(aj + 1)/(2.0*momort)
            endif
            !  segs = aj*(aj + 1)/(2.0*momort)
            YRAst(j, Nnuc) = segs
            SHCjf(j, Nnuc) = SHCFADE(j - 1, SHRj, SHRd)
            FISb(j, Nnuc) = sb*QFIs + segs
            if(JSTab(Nnuc)/=0 .and. j>=JSTab(Nnuc))exit
            !-----------determination of stability limit including shell correction
            if(sb*QFIs - SHCjf(j, Nnuc)*SHC(Nnuc)<=0.001D0)exit
            jstabf = j
         enddo
         if(JSTab(Nnuc)==0)JSTab(Nnuc) = jstabf
      endif
      !  IF (JSTab(Nnuc).EQ.0) NLWst = MIN0(JSTab(Nnuc),NLWst)
      NLWst = MIN0(JSTab(Nnuc), NLWst, NLW)
      
      return
      !-----yrast and saddle point energies ----- done ---------------
      !-----set to 0 level density array
      do i = 1, NDEX
         do k = 1, NDLW
            if(BF/=0.0D0)then
               RO(i, k, 1, Nnuc) = 0.0
               RO(i, k, 2, Nnuc) = 0.0
            else
               ROF(i, k, Nnuc) = 0.0
            endif
         enddo
      enddo
      !-----setting to 0 level density array ------ done ------
   end subroutine PRERO



   function SHCFADE(J, Shrj, Shrd)
      !!C   
      !!c   ********************************************************************
      !!c   *                                                         CLASS:PPU*
      !!c   *                      S H C F A D E                               *
      !!c   *                                                                  *
      !!c   * calculates angular momentum (J) fade-out of the shell            *
      !!c   * correction to the fission barrier                                *
      !!c   *                                                                  *
      !!c   ********************************************************************
      !!C   
      !
      implicit none

      integer :: J
      real*8 :: SHCFADE
      real*8 :: Shrd, Shrj
      intent (in) J, Shrd, Shrj
      real :: FLOAT
 
      SHCFADE = 1.
      if(Shrd/=0.D0)SHCFADE = 1.0/(1.0 + EXP((FLOAT(J)-Shrj)/Shrd))
      return

   end function SHCFADE



   subroutine ROGC(Nnuc, Scutf)
      !!   
      !!   ********************************************************************
      !!   *                                                         CLASS:PPU*
      !!   *                         R O G C                                  *
      !!   * CALCULATES TABLE OF ENERGY AND SPIN DEPENDENT LEVEL DENSITIES    *
      !!   * FOR NUCLEUS NNUC ACCORDING TO GILBERT-CAMERON                    *
      !!   *                                                                  *
      !!   * INPUT:NNUC - INDEX OF THE NUCLEUS                                *
      !!   *       SCUTF - SPIN CUT-OFF FACTOR (0.146 IS RECOMMENDED)         *
      !!   *                                                                  *
      !!   * OUTPUT:RO(.,.,NNUC) - LEVEL DENSITIES                            *
      !!   *                                                                  *
      !!   *   ROGC
      !!   *   |-> PRERO
      !!   *   |-> FSHELL
      !!   *       |-> SHCFADE
      !!   *   |-> PARITY_FRACTION (commented)
      !!   *   |-> PLOT_ZVV_NumCumul
      !!   *   |-> PLOT_ZVV_GSLD
      !!   *
      !!   *                                                                  *
      !!   ********************************************************************
      !CC
      
      implicit none

      include 'dimension.h'
      include 'global.h'
 
      real*8 :: A2, A23, AM, AP1, AP2, BF, DEL, DELp, EO, GAMma, T, UX
      integer :: NLWst
      common /CT    / AM, UX, EO, T
      common /PARAM / AP1, AP2, GAMma, DEL, DELp, BF, A23, A2, NLWst
 
      integer :: Nnuc
      real*8 :: Scutf
      intent (in) Scutf

      real*8 :: a1, amas, arg, atil, b1, c1, e, enorm, eom, eps, exl, rolowint, ro_j, ro_u, sigh, sigl, tm, u, &
                  & xj
      real*8 :: DEXP
      real*8 :: FSHELL
      integer :: i, ig, igna, igs, iter, j
      integer :: INT
      real*8, dimension(1:2) :: ratio
      real :: SNGL

      eom = 0.D0
      !-----next call prepares for lev. dens. calculations
      call PRERO(Nnuc)
      amas = A(Nnuc)
      A23 = A(Nnuc)**0.666667D0
      igna = 0
      igs = 1                              ! set ground state parity index
      ratio = 0.5D0                        ! 50%/50% default for parity distribution
      if(LVP(1, Nnuc)== - 1)igs = 2
      !  Set parameters of the parity energy dependence
      !  Preliminary imlementation  valid only for the major nuclides involved
      !  in the 56Fe evaluation (other calculations not affected)
      if(IZA(Nnuc)==26055)then
         a1 = 0.017139
         b1 = -0.0672441
         c1 = 1.05495
      elseif(IZA(Nnuc)==26056)then
         a1 = 0.0327456
         b1 = -0.082746
         c1 = 1.066
      elseif(IZA(Nnuc)==26057)then
         a1 = 0.0359514
         b1 = -0.086571
         c1 = 1.05589
      elseif(IZA(Nnuc)==25055)then
         a1 = 0.0293767
         b1 = -0.0971741
         c1 = 1.07698
      elseif(IZA(Nnuc)==25056)then
         a1 = 0.0482035
         b1 = -0.0978324
         c1 = 1.04671
      elseif(IZA(Nnuc)==24052)then
         a1 = 0.0164111
         b1 = -0.107355
         c1 = 1.13664
      elseif(IZA(Nnuc)==24053)then
         a1 = 0.0241924
         b1 = -0.0849786
         c1 = 1.0703
      else
         a1 = 0.0D0
         b1 = 0.0D0
         c1 = 0.0D0
      endif
      !-----zero potentially undefined variables
      GAMma = 0.D0
      exl = 0.D0
      sigh = 0.D0
      !-----a-parameter given in input
      if(ROPaa(Nnuc)>0.0D0)ROPar(1, Nnuc) = ROPaa(Nnuc)
      !-----Ignatyuk parametrization
      enorm = 5.D0
      if(ROPaa(Nnuc)==0.0D0)then
         atil = 0.154D0*A(Nnuc) + 6.3D-5*A(Nnuc)**2
         !--------next line assures normalization to experimental data (on average)
         atil = atil*ATIlnor(Nnuc)
         GAMma = -0.054D0
         ROPar(1, Nnuc) = atil*FSHELL(enorm, SHC(Nnuc), -GAMma)
         igna = 1
      endif
      !-----Arthurs' parametrization
      if(ROPaa(Nnuc)==( - 1.0D0))then
         atil = 0.1375*A(Nnuc) - 8.36E-5*A(Nnuc)**2
         !--------next line assures normalization to experimental data (on average)
         atil = atil*ATIlnor(Nnuc)
         GAMma = -0.054D0
         ROPar(1, Nnuc) = atil*FSHELL(enorm, SHC(Nnuc), -GAMma)
         igna = 1
      endif
      !-----Mebel's  parametrization (taken from the INC code for the case
      !-----of no collective enhancements) normalized to existing exp. data
      if(ROPaa(Nnuc)==( - 2.0D0))then
         atil = 0.114*A(Nnuc) + 9.80E-2*A23
         !--------next line assures normalization to experimental data (on average)
         atil = atil*ATIlnor(Nnuc)
         GAMma = -0.051D0
         ROPar(1, Nnuc) = atil*FSHELL(enorm, SHC(Nnuc), -GAMma)
         igna = 1
      endif
      
      !-----If parameters given in input, they are initialized
      
      AM = ROPar(1, Nnuc)
      UX = ROPar(2, Nnuc)
      DEL = ROPar(3, Nnuc)
      EO = ROPar(4, Nnuc)
      T = ROPar(5, Nnuc)
      
      !-----calculation of nuclear temperature if t=0
      
      if(T==0.D0)then
         if(UX==0.0D0)then
            T = 0.9 - 0.0024*amas
            if(amas<100.D0)T = 60./amas + 0.06
         else
            T = SQRT(AM/UX) - 3./2./UX
            T = 1./T
            tm = T
         endif
      endif
      
      !-----calculation of spin cut-off parameter from resolved levels
      
      sigl = 0.
      do i = 2, NLV(Nnuc)
         sigl = sigl + (ABS(XJLv(i,Nnuc)) + 0.5)**2
      enddo
      if(NLV(Nnuc)>1)sigl = sigl/(NLV(Nnuc) - 1)
      sigl = sigl/2.
      if(sigl<0.5D0)sigl = 0.5
      
      !-----calculation of matching point /if UX=0.0/
      
      iter = 0
      
      100 continue
      if(AM*T<=6.D0 .or. iter>300)then
      
         if(iter>300)then
            write(8, *)'WARNING: '
            write(8, *)'WARNING: MAXIMUM NUMBER OF ITERATIONS IN ROGC REACHED FOR'
            write(8, *)'WARNING: Z=', INT(Z(Nnuc)), '  A=', INT(A(Nnuc))
            write(8, *)'WARNING: I WILL USE THE LAST T=', tm, ' FOR CALC.'
            write(8, *)'WARNING: '
         endif
         !--------set nuclear temperature to the value from the systematics
         T = 0.9 - 0.0024*amas
         if(amas<100.D0)T = 60./amas + 0.06
         tm = T
      endif
      
      if(igna/=0D0)then
         do i = 1, 10
            !  write(*,*) '***',a(nnuc),z(nnuc),ux
            !  write(*,*) am, 6.d0/t, atil
            if(AM*T<6.D0)then
               AM = atil
               exit
            endif
            if(UX==0.0D0)UX = T*T*(AM - 3/T + SQRT((AM-6/T)*AM))/2.0
            AM = atil*FSHELL(UX, SHC(Nnuc), -GAMma)
         enddo
      else
         if(UX==0.0D0)UX = T*T*(AM - 3/T + SQRT((AM-6/T)*AM))/2.0
      endif
      
      exl = UX + DEL
      !-----RCN 12/2004
      !-----IF(Scutf.LT.0.0D0)sigh calculated according to Dilg's recommendations
      !-----0.6079 = 6/pi^2   a=6/pi^2*g  sig^2 = <m^2>gt  Scutf = <m^2>
      sigh = Scutf*0.6079*A23*SQRT(UX*AM)
      !
      !-----determination of the index in EX-array such that EX(IG,.).LT.EXL
      !-----(low-energy level density formula is used up to IG)
      !
      do i = 1, NEX(Nnuc)
         if(EX(i, Nnuc)>exl)goto 300
      enddo
      ig = NEX(Nnuc)
      goto 400
      300 continue
      ig = i - 1
      400 continue
      if(EO==0.0D0)then
         ro_u = DEXP(2.*SQRT(AM*UX))/(12.*SQRT(2*sigh))/AM**0.25/UX**1.25
         EO = exl - T*LOG(T*ro_u)
      endif
      eom = EO
      !-----fit nuclear temperature (and Ux) to discrete levels
      !  IF (NLV(Nnuc).GT.5 .AND. ROPar(2,Nnuc).EQ.0.0D0 .AND.
      if(NLV(Nnuc)>2 .and. ROPar(2, Nnuc)==0.0D0 .and. ROPar(5, Nnuc)==0.0D0)then
         eps = MIN(NLV(Nnuc)*0.03, 0.5)
         rolowint = EXP(( - EO/T))*(EXP(ELV(NLV(Nnuc),Nnuc)/T) - 1.)
         if(ABS(rolowint + 1.0 - NLV(Nnuc))>eps)then
            tm = T
            T = T + 0.01*LOG((NLV(Nnuc) - 1)/EXP((-EO/T))/(EXP(ELV(NLV(Nnuc),Nnuc)/T) - 1))
            iter = iter + 1
            if(iter<=300)then
               UX = 0.0
               EO = 0.0
               goto 100
            else
               UX = 0.0
               EO = 0.0
            endif
         endif
      endif
      
      ROPar(1, Nnuc) = AM
      ROPar(2, Nnuc) = UX
      ROPar(3, Nnuc) = DEL
      ROPar(4, Nnuc) = EO
      ROPar(5, Nnuc) = T
      
      if(UX<=0.0D0)write(*, *)INT(Z(Nnuc)), INT(A(Nnuc)), 'UX=', sngl(UX)
      
      if(ig/=0)then
         !-----calculation of level densities below EXL
         !-----(low energy formula)
         do i = 1, ig          !Do loop over excitation energies
            e = EX(i, Nnuc)
            !  CALL PARITY_FRACTION(e,a1,b1,c1,ratio,igs)    ! get parity multipliers 'ratio'
            !  write(8,*) 'E, ratios, igs, nnuc', E, ratio, igs, nnuc
            arg = (e - EO)/T
            if(arg<EXPmax)then
               ro_u = EXP(arg)/T
               !--------------Spin-cutoff is interpolated
               SIG = sigl
               if(e>ECUt(Nnuc))SIG = (sigh - sigl)*(e - ECUt(Nnuc))/(exl - ECUt(Nnuc)) + sigl
               do j = 1, NLW
                  xj = j + HIS(Nnuc)
                  arg = (xj + 0.5)**2/(2.*SIG)
                  if(arg<=EXPmax)then
                     ro_j = (2*xj + 1.)/(2.*SIG)*EXP( - arg)
                     RO(i, j, 1, Nnuc) = ro_u*ro_j*ratio(1)
                     RO(i, j, 2, Nnuc) = ro_u*ro_j*ratio(2)
                  endif
               enddo
               UEXcit(i, Nnuc) = e
               TNUc(i, Nnuc) = SQRT(e/AM)
            endif
         enddo           ! Loop over excitation energies
      endif
      ig = ig + 1
      if(ig<=NEX(Nnuc))then
      
         !--------calculation of level densities for energies surpassing
         !--------EXL /fermi gas formula/
      
         do i = ig, NEX(Nnuc)              !Do loop over excitation energies
            u = EX(i, Nnuc) - DEL
            !  CALL PARITY_FRACTION(EX(i,Nnuc),a1,b1,c1,ratio,igs)
            !  write(8,*) 'E, ratios, Igs, nnuc',EX(i,Nnuc),ratio,igs,nnuc
            if(u<0.D0)cycle
            if(igna==1)AM = atil*FSHELL(u, SHC(Nnuc), -GAMma)
            UEXcit(i, Nnuc) = u
            TNUc(i, Nnuc) = SQRT(u/AM)
            !-----------Dilg's recommendations
            SIG = Scutf*0.6079*A23*SQRT(u*AM)
            arg = 2.*SQRT(AM*u)
            if(arg<=EXPmax)then
               ro_u = DEXP(arg)/(12.*SQRT(2*SIG))/AM**0.25/u**1.25
               do j = 1, NLW
                  xj = j + HIS(Nnuc)
                  arg = (xj + 0.5)**2/(2.*SIG)
                  if(arg<EXPmax)then
                     ro_j = (2*xj + 1.)/(2.*SIG)*EXP( - arg)
                     RO(i, j, 1, Nnuc) = ro_u*ro_j*ratio(1)
                     RO(i, j, 2, Nnuc) = ro_u*ro_j*ratio(2)
                  endif
               enddo
            endif
         enddo           ! Loop over excitation energies
      endif
      
      if((FITlev>0.or.IOUt==6) .and. NLV(Nnuc)>3 .and. ENDf(Nnuc)<=1)then
         !  Call PLOT_GNU_NumCumul(Nnuc,0.d0,0.d0)
         call PLOT_ZVV_NumCumul(Nnuc)
         call PLOT_ZVV_GSLD(Nnuc)
      endif
      
      ROPar(4, Nnuc) = EO
      ROPar(2, Nnuc) = UX
      
      write(8, '(I3,''-'',A2,''-'',I3, 5X,5(2X,1F8.5))')INT(Z(Nnuc)), SYMb(Nnuc), INT(A(Nnuc)), (ROPar(j, Nnuc), j = 1, 5)
      
      return
   end subroutine ROGC



   subroutine PARITY_FRACTION(E, A1, B1, C1, Ratio, Igs)
      !!c
      !!c   ********************************************************************
      !!c   *                                                          class:au*
      !!c   *             P A R I T Y _ F R A C T I O N                        *
      !!c   *                                                                  *
      !!c   * Calculates ratio of parities for level density (non-g.s to g.s)) *
      !!c   *                                                                  *
      !!c   * input: E  - excitation energy                                    *
      !!c   *        a1 - parameter in parity distribution formula             *
      !!c   *        b1 - parameter in parity distribution formula             *
      !!c   *        c1 - parameter in parity distribution formula             *
      !!c   *        Igs - ground-state parity (1 for plus, 2 for negative,    *
      !!c   *             3 to return equal probability )                      *
      !!c   *                                                                  *
      !!c   * output:Ratio(1) - fraction of positive parity                    *
      !!c   *        Ratio(2) - fraction of negative parity                    *
      !!c   *                                                                  *
      !!c   *  Ground state parity fration is assumed to be 0.5                *
      !!c   *  Only non-ground-state parity fraction is changed                *
      !!c   *  If inputed parity factors are all 0 equal parity is returned    *
      !!c   *                                                                  *
      !!c   * calls:none                                                       *
      !!c   *                                                                  *
      !!c   ********************************************************************
      !!c
      
      implicit none

      real*8 :: A1, B1, C1, E
      integer*4 :: Igs
      real*8, dimension(1:2) :: Ratio
      intent (in) A1, B1, C1, E, Igs
      intent (out) Ratio

      integer*4 :: ings
      real*8 :: tmp

      Ratio = 0.5D0
      if(Igs==3)return            ! Return equal probability for both parities if Ip=3
      ings = 2
      if(Igs==2)ings = 1
      tmp = A1*E**2 + B1*E + C1
      tmp = 1.0D0 - 1.0D0/tmp        ! Ratio of lev. den. for non-g.s. to g.s. parity
      if(tmp<=0.0D0)then
         Ratio(ings) = 0.0D0
         return
      endif
      Ratio(ings) = 0.5D0*tmp
      return

   end subroutine PARITY_FRACTION



   subroutine READ_SHELL_CORR
      !!c
      !!c   ********************************************************************
      !!c   *                                                          class:au*
      !!c   *             R E A D _ S H E L L _ C O R R                        *
      !!c   *                                                                  *
      !!c   * Reads MS Shell Corrections from RIPL                           *
      !!c   *                                                                  *
      !!c   * input: none (implicit - all considered nuclei)                   *
      !!c   *                                                                  *
      !!c   * output:none (implicit - shell corrections for considered nuclei) *
      !!c   *                                                                  *
      !!c   * calls:none                                                       *
      !!c   *                                                                  *
      !!c   ********************************************************************
      !!c
      
      implicit none

      include 'dimension.h'
      include 'global.h'
 
      real*8 :: defcorr, shelmsr
      character(2) :: dum
      integer :: iloc, na, nnuc, nz
      real :: TRIM
 
      !-----Reading MS shell corrections and deformation energies
      open(211, file = trim(EMPiredir)//'/RIPL/densities/shellcor-ms.dat', status = 'OLD')
      !-----Skipping header lines
      read(211, *)
      read(211, *)
      read(211, *)
      read(211, *)
      do while (.true.)
         read(211, 98, end = 50, err = 60)nz, na, dum, shelmsr, defcorr
         98 format(2(i4), 1x, a2, 2x, f7.3, 1x, f8.3)
         call WHERE(nz*1000 + na, nnuc, iloc)
         if(iloc==0)then
            SHC(nnuc) = shelmsr*SHLlnor(nnuc)
            if(ADIv==1)SHC(nnuc) = (shelmsr - defcorr)*SHLlnor(nnuc)
         endif
         !-----projectile
         if(nz==Z(0) .and. na==A(0))then
            SHC(0) = shelmsr*SHLlnor(nnuc)
            if(ADIv==1)SHC(0) = (shelmsr - defcorr)*SHLlnor(nnuc)
         endif
      enddo
      60 continue
      write(8, *)' ERROR: ERROR READING SHELL CORRECTION FILE'
      stop ' ERROR: ERROR READING SHELL CORRECTION FILE'
      50 continue
      close(211)
      return

   end subroutine READ_SHELL_CORR



   subroutine ROHFB(Nnuc)
      !!C
      !!C   *********************************************************************
      !!C   *                                                         CLASS:PPU *
      !!C   *                      R O H F B                                    *
      !!C   *                                                                   *
      !!C   *  Reads level densities numerically calculated by a combinatorial  *
      !!C   *  method using Hartree-Fock-Bogoliubov single particle levels      *
      !!C   *            (to be included within RIPL-3)                         *
      !!C   *                                                                   *
      !!C   *     S.Hilaire and S. Goriely, Nucl.Phys.A 779 (2006) 63-81        *
      !!C   *  "Global microscopic nuclear level densities within the HFB plus  *
      !!C   *   combinatorial method for practical applications"                *
      !!C   *                                                                   *
      !!C   *  Interpolates LDs linearily in log to the EMPIRE energy grid.     *
      !!C   *                                                                   *
      !!C   *  INPUT:                                                           *
      !!C   *  NNUC - INDEX OF THE NUCLEUS (POSITION IN THE TABLES)             *
      !!C   *                                                                   *
      !!C   *                                                                   *
      !!C   * OUTPUT:NONE                                                       *
      !!C   *
      !!C   *      ROHFB
      !!C   *      |-> PRERO
      !!C   *          |-> SHCFADE
      !!C   *      |-> scale_ld
      !!C   *      |-> PLOT_ZVV_GSLD
      !!C   *      |-> PLOT_ZVV_NumCumul
      !!C   *                                                                   *
      !!C   *********************************************************************
      !!C
      
      implicit none

      include 'dimension.h'
      include 'global.h'

      integer, parameter :: NLDGRID = 60, JMAX = 50
      integer :: Nnuc
      real*8 :: acorr, c1, c2, corr1, hhh, pcorr, r1, r2, u
      character(2) :: car2
      real*8, dimension(0:NLDGRID, 2) :: cgrid, rhoogrid, rhotgrid
      real*8 :: DEXP, DLOG10, DSQRT
      logical :: fexist
      character(50) :: filename
      integer :: i, ia, iar, ipp, iugrid, iz, izr, j, jmaxl, k, khi, kk, klo, lenst
      integer :: LEN
      real*8, dimension(0:NLDGRID, JMAX, 2) :: rhogrid
      real*8, dimension(0:NLDGRID) :: tgrid, uugrid
      real :: TRIM

      ia = int(A(Nnuc))
      iz = int(Z(Nnuc))
      !-----next call prepares for lev. dens. calculations
      call PRERO(Nnuc)
      
      !-----initialization
      
      jmaxl = MIN(NDLW, JMAX)
      do i = 0, NLDGRID
         uugrid(i) = 0.D0
         tgrid(i) = 0.D0
         do ipp = 1, 2
            cgrid(i, ipp) = 0.D0
            rhoogrid(i, ipp) = 0.D0
            rhotgrid(i, ipp) = 0.D0
            do j = 1, jmaxl
               rhogrid(i, j, ipp) = 0.D0
            enddo
         enddo
      enddo
      write(filename, 99005)iz
      99005 format('/RIPL/densities/total/level-densities-hfb/z', i3.3, '.tab')
      lenst = len(trim(filename))
      open(unit = 34, file = trim(EMPiredir)//filename(1:lenst), err = 300)
      do while (.true.)
         read(34, 99010, err = 300, end = 300)car2
         if(car2/='Z=')then
         else
            backspace(34)
            read(34, 99010, err = 300, end = 300)car2, izr, iar
                                                            !, paritate
            if(iar/=ia .or. izr/=iz)then
            else
            
            !-----reading microscopic lev. dens. from the RIPL-3 file
      
               read(34, *, end = 300)
               read(34, *, end = 300)
               i = 1
               do while (.true.)
                  read(34, 99015, end = 300)uugrid(i), tgrid(i), cgrid(i, 1), rhoogrid(i, 1), rhotgrid(i, 1),                  &
                     & (rhogrid(i, j, 1), j = 1, jmaxl)
                  if(uugrid(i)<=0.001)exit
                  if(i==NLDGRID)then
                     i = 1
                     read(34, *, end = 300)
                     exit
                  else
                     i = i + 1
                  endif
               enddo
               !  SKIPPING 4 TITLE LINES
               read(34, *, end = 300)
               read(34, *, end = 300)
               read(34, *, end = 300)
               read(34, *, end = 300)
               do while (.true.)
                  read(34, 99015, end = 300)uugrid(i), tgrid(i), cgrid(i, 2), rhoogrid(i, 2), rhotgrid(i, 2),                  &
                     & (rhogrid(i, j, 2), j = 1, jmaxl)
                  if(uugrid(i)<=0.001)goto 400
                  if(i==NLDGRID)goto 400
                  i = i + 1
               enddo
            endif
         endif
      enddo
      300 continue
      write(8, *)' ERROR: NO HFB LEV. DENS. FOR Z=', iz, ' A=', ia
      write(8, *)' ERROR: USE OTHER LEVEL DENSITIES. '
      stop ' ERROR: RIPL HFB GROUND STATE LEV DENSITY MISSING'
      400 continue
      close(34)
      
      !  Checking if file with scaling factors for HFB LD is present
      filename = 'LEVDEN_SCALE.DAT'
      lenst = len(trim(filename))
      inquire(file = filename(1:lenst), exist = fexist)
      !  If it exists, call function to re-scale LD's
      if(fexist)call scale_ld(filename(1:lenst), iz, ia, uugrid, rhoogrid, rhotgrid, rhogrid, NLDGRID, JMAX, jmaxl)
      
      
      !  Using correction files given by A. Koning on March 2008.
      !  Corrections are defined exactly as ROHfba() and ROHfbp() parameters
      !  by fitting available discrete levels' and D0s' information
      
      write(filename, 99007)iz
      99007 format('/RIPL/DENSITIES/TOTAL/LEVEL-DENSITIES-HFB/Z', i3.3, '.COR')
      lenst = len(trim(filename))
      open(unit = 34, file = trim(EMPiredir)//filename(1:lenst), err = 440)
      pcorr = 0.D0
      acorr = 0.D0
      do while (.true.)
         read(34, 99008, err = 440, end = 440)izr, iar, acorr, pcorr
         99008 format(1x, i3, 1x, i3, 10x, f11.5, 1x, f11.5)
         if(iar/=ia .or. izr/=iz)then
         else
      
            ROHfbp(Nnuc) = ROHfbp_off(Nnuc) + pcorr
            ROHfba(Nnuc) = ROHfba_off(Nnuc) + acorr
      
            !-----printing microscopic lev. dens. corrections from the RIPL-3 file
      
            if(ROHfba(Nnuc)/=0.D0)then
               write(8, '('' GS HFB L.D. NORM  IN '',I3,A2,'' SET TO '',F8.3)')ia, SYMb(Nnuc), ROHfba(Nnuc)
               write(12, '('' GS HFB L.D. NORM  IN '',I3,A2,'' SET TO '',F8.3)')ia, SYMb(Nnuc), ROHfba(Nnuc)
            endif
            if(ROHfbp(Nnuc)/=0.D0)then
               write(8, '('' GS HFB L.D. SHIFT IN '',I3,A2,'' SET TO '',F8.3)')ia, SYMb(Nnuc), ROHfbp(Nnuc)
               write(12, '('' GS HFB L.D. SHIFT IN '',I3,A2,'' SET TO '',F8.3)')ia, SYMb(Nnuc), ROHfbp(Nnuc)
            endif
            exit
         endif
      enddo
      440 continue
      close(34)
      goto 445
      write(8, *)' ERROR: READING MICROSC. LD CORRECTIONS FOR Z=', iz, ' A=', ia, ' IN HFB'
      445 continue
      close(34)
      iugrid = i - 1
      
      do kk = 1, NEX(Nnuc)
      
         u = EX(kk, Nnuc) - ROHfbp(Nnuc)
      
         !  UEXcit(kk,Nnuc) = EX(kk,Nnuc)
         UEXcit(kk, Nnuc) = MAX(u, 0.D0)
      
         if(u<0.)cycle
      
         if(u>200.0D0)then
            write(8, *)' '
            write(8, *)' ERROR: HFB LEV. DENS. DEFINED UP TO 200 MEV ONLY'
            write(8, *)' ERROR: REQUESTED ENERGY IS ', u, ' MEV'
            write(8, *)' ERROR: YOU HAVE TO USE OTHER LEVEL DENSITIES'
            write(8, *)' ERROR: EXECUTION STOPPED'
            stop ' ERROR: TOO HIGH ENERGY FOR HFB LEV. DENS.'
         endif
         corr1 = 1.D0
         if(ROHfba(Nnuc)/=0.D0)corr1 = dexp(ROHfba(Nnuc)*dsqrt(u))
      
         !--------interpolation in the level density tables
      
         klo = 1
         khi = iugrid
         if(u<=uugrid(klo))then
            klo = 0
            khi = 1
            goto 500
         endif
         if(u>=uugrid(khi))then
            klo = iugrid - 1
            goto 500
         endif
         do while (khi - klo>1)
            k = int((khi + klo)/2.)
            if(uugrid(k)>u)then
               khi = k
            else
               klo = k
            endif
         enddo
         500 continue
         hhh = uugrid(khi) - uugrid(klo)
         c1 = (uugrid(khi) - u)/hhh
         c2 = (u - uugrid(klo))/hhh
         do j = 1, jmaxl
            do ipp = 1, 2
               r1 = rhogrid(klo, j, ipp)
               r2 = rhogrid(khi, j, ipp)
               if(r1>0 .and. r2>0)then
                  RO(kk, j, ipp, Nnuc) = 10.**(c1*DLOG10(r1) + c2*DLOG10(r2))*corr1
               else
                  RO(kk, j, ipp, Nnuc) = (c1*r1 + c2*r2)*corr1
               endif
               if(RO(kk, j, ipp, Nnuc)<0)RO(kk, j, ipp, Nnuc) = 0.D0
            enddo
         enddo
         TNUc(kk, Nnuc) = c1*tgrid(klo) + c2*tgrid(khi)
      enddo
      
      if((FITlev>0.or.IOUt>=6) .and. ENDf(Nnuc)<=1 .and. NLV(Nnuc)>3)then
         call PLOT_ZVV_GSLD(Nnuc)
         !
         !  Cumulative levels must be calculated for FITLEV>0
         !  as Ecut(Nnuc) is set to zero
         !
         !  In a normal calculation, Ecut(NNuc)>0 and therefore the
         !  cumulative integral is wrongly calculated    !   !   !
         !  RCN, April 2012
         !  Call PLOT_GNU_NumCumul(Nnuc,0.d0,0.d0)
         call PLOT_ZVV_NumCumul(Nnuc)
      endif
      
      return
      99010 format(23x, a2, i3, 3x, i3)
                                    !,2x,a8)
      99015 format(1x, f6.2, f7.3, 1x, 53E9.2)
   end subroutine ROHFB


   subroutine scale_ld(Filename, Iz, Ia, Uugrid, Rhoogrid, Rhotgrid, Rhogrid, Nldgrid, Jmax, Jmaxl)
      
      implicit none

      character(*) :: Filename
      integer :: Ia, Iz, Jmax, Jmaxl, Nldgrid
      real*8, dimension(0:Nldgrid, Jmax, 2) :: Rhogrid
      real*8, dimension(0:Nldgrid, 2) :: Rhoogrid, Rhotgrid
      real*8, dimension(0:Nldgrid) :: Uugrid
      intent (in) Ia, Iz, Jmax, Jmaxl, Nldgrid, Uugrid
      intent (inout) Rhogrid, Rhoogrid, Rhotgrid
      character(2) :: char2

      real*8 :: DABS
      integer :: i, iaread, ios, ipar, izread, j, npar
      character(8) :: par
      real :: TRIM
      real*8 :: x, y
      open(34, file = Filename, status = 'OLD', iostat = ios)

      if(ios/=0)then
         write(*, *)'ERROR: FILE ''', Filename, ''' EXPECTED TO BE ', 'PRESENT BUT WAS NOT FOUND!'
         stop
      endif
   
   
      do npar = 1, 2 ! Parity loop, we have to find the scaling for both parities.
         5 continue       ! Nuclide loop, looking for (Z,A) match
         do
            read(34, 10, err = 5, end = 100)char2, izread, iaread, par
            10    format(23x, a2, i3, 3x, i3, 2x, a8)
            if(char2=='Z=' .and. izread==Iz .and. iaread==Ia)exit     ! Nuclide match!
         enddo     ! End of nuclide loop

         !  Assigning interger value for parity from character variable
         if(par=='POSITIVE')then
            ipar = 1
         elseif(par=='NEGATIVE')then
            ipar = 2
         else
            write(*, 15)Filename, par
            15 format('ERROR: WRONG PARITY IN FILE ', a, '!!', /, 'ERROR: ', 'IT SHOULD BE ''POSITIVE'' OR ''NEGATIVE'' BUT READ ',&
               & a, ' INSTEAD!!')
            stop
         endif

         write(*, 17)Iz, Ia, par, trim(Filename)
         write(8, 17)Iz, Ia, par, trim(Filename)
         write(12, 17)Iz, Ia, par, trim(Filename)

         !  Skipping two lines
         read(34, *)
         read(34, *)
         !  Energy loop to read the scaling factors
         do i = 1, Nldgrid
            read(34, *, end = 80, err = 80)x, y
            !  Checking if value of energy just read matches the one stored in uugrid(i)
            if(DABS(x - Uugrid(i))>1.D-6)then
               write(*, 20)Uugrid(i), x, trim(Filename)
               20 format('ERROR: MISMATCH OF ENERGY GRID VALUES FOR HFB ', 'LEVEL-DENSITY SCALING!!', 1/,&
                  & 'ERROR: EXPECTED ', f6.2,  ' FROM ', 'RIPL BUT INSTEAD READ ', f6.2, ' FROM FILE ''', a, '''!!')
               stop
            endif
            !  Rescaling level densities
            Rhoogrid(i, ipar) = Rhoogrid(i, ipar)*y
            Rhotgrid(i, ipar) = Rhotgrid(i, ipar)*y
            do j = 1, Jmaxl
               Rhogrid(i, j, ipar) = Rhogrid(i, j, ipar)*y
            enddo

         enddo   ! End of energy loop
      80 enddo     ! End of parity loop

      100 continue
      close(34)

      return
      17 format(1/, 'HFB LEVEL DENSITIES FOR NUCLEUS (Z,A)=(', i3, ', ', i3, '), ', a, ' PARITY,', 1/, &
               &'RESCALED ACCORDING TO FILE ''', a, '''.', 1/)

   end subroutine SCALE_LD


   subroutine LEVFIT(Nnuc, Nplot, Dshif, Dshift, Defit)
      
      implicit none

      include 'dimension.h'
      include 'global.h'

      real*8 :: A2, A23, ACR, ACRt, AP1, AP2, ATIl, BF, DEL, DELp, DETcrt, ECOnd, GAMma, SCR, TCRt, UCRt
      integer :: NLWst
      common /CRIT  / TCRt, ECOnd, ACRt, UCRt, DETcrt, SCR, ACR, ATIl
      common /PARAM / AP1, AP2, GAMma, DEL, DELp, BF, A23, A2, NLWst

      real*8 :: Defit, Dshif, Dshift
      integer :: Nnuc, Nplot
      intent (out) Nplot
      intent (inout) Defit, Dshift

      real*8 :: aj, dshi, ecrt, ellq, exkk, rocumd, rocumu, rocumul, rolev, rotemp
      integer :: ij, iter, kk, kkl, kku
      integer :: NINT
      real :: REAL, SNGL


      !-----fit level densities to discrete levels applying energy shift
      !-----which will linearly go to 0 at neutron binding energy
      !-----
      !-----get neutron binding energy  if not yet defined
      if(Q(1, Nnuc)==0.0D0)then       !,ldshif(40)
         rewind(25)
         call BNDG(1, Nnuc, Q(1, Nnuc))
      endif
      !-----get distance between Qn and the last level
      ellq = Q(1, Nnuc) - (ELV(NLV(Nnuc), Nnuc) + LDShif(Nnuc))
      Dshift = 0.0
      iter = 0
      !-----we are not going to fit discrete levels if there are not more
      !-----than three
      if(NLV(Nnuc)>3)then
         if(FITlev>0)then
            write(8, *)' '
            write(8, *)' FITTING L.D. TO DISCRETE LEVELS'
            write(8, *)NLV(Nnuc), ' LEVELS AT ', ELV(NLV(Nnuc), Nnuc), ' MEV'
         endif
         Defit = (ELV(NLV(Nnuc), Nnuc) + LDShif(Nnuc) + 2.D0)/(NEXreq - 1)
         Nplot = int((ELV(NLV(Nnuc), Nnuc) + LDShif(Nnuc) + 2.D0)/Defit)
         do while (.true.)

            rocumul = 1.0
            iter = iter + 1
            kkl = 0
            kku = 0

            RO(:, :, :, Nnuc) = 0.D0

            do kk = 1, NEXreq
               !-----------decrease energy shift above the last level to become 0 at Qn
               exkk = (kk - 1)*Defit
               if(exkk<=ELV(NLV(Nnuc), Nnuc) + LDShif(Nnuc))then
                  Dshif = Dshift
               elseif(exkk<Q(1, Nnuc) .and. ellq/=0.0D0)then
                  Dshif = Dshift*(Q(1, Nnuc) - exkk)/ellq
               else
                  Dshif = 0.0
               endif

               call DAMIRO(kk, Nnuc, Dshif, Defit, rotemp, aj)

               do ij = 1, NLWst
                  !--------------Integration over energy. Parity dependence explicitly considered.
                  !--------------There is a factor 1/2 steming from the trapezoid integration
                  if(kk>1)rocumul = rocumul + 0.5D0*Defit*(RO(kk - 1, ij, 1, Nnuc) + RO(kk, ij, 1, Nnuc) &
                                 & + RO(kk - 1, ij, 2, Nnuc) + RO(kk, ij, 2, Nnuc))
               enddo
               if(rocumul<=NLV(Nnuc))then
                  kkl = kk
                  rocumd = rocumul
               elseif(kku==0)then
                  kku = kk
                  rocumu = rocumul
               endif
            enddo

            rocumd = LOG(rocumd)
            rocumu = LOG(rocumu)
            rolev = LOG(REAL(NLV(Nnuc)))
            dshi = (rolev - rocumd)/(rocumu - rocumd)
            dshi = (kkl - 1 + dshi)*Defit
            dshi = dshi - (ELV(NLV(Nnuc), Nnuc) + LDShif(Nnuc))
            Dshift = Dshift + dshi

            if(FITlev>0)then
               ecrt = UCRt - DEL - Dshift
               write(8, *)
               write(8, *)'*****   A=', nint(A(Nnuc)), ' Z=', nint(Z(Nnuc)), ' BN=', sngl(Q(1, Nnuc)), ' LDSHIF=', &
                        & sngl(LDShif(Nnuc))
               write(8, '(A7,G12.5,A6,G12.5,A9,G12.5,A7,G12.5)')'UCRT = ', UCRt, ' ECRT=', ecrt, ' ECOND = ', ECOnd, ' DEL = ',&
                  & DEL
               write(8, '(A5,I3,4X,G12.5,A15,2(G12.5,1X))')'IT # ', iter, dshi, ' FINAL SHIFT = ', Dshift
               write(8, *)
            endif
            if(ABS(dshi)>0.01D0 .and. iter<=20)then
            else
               exit
            endif
         enddo
      endif

      return

   end subroutine LEVFIT

   !CC
   !CC
   !CC   *************     F I S S I O N   **********************************
   !CC
   !CC

   subroutine DAMIRO_FISHI(Kk, Nnuc, Asaf, Rotemp, Aj)
      !CC   *****************************************************************
      !CC   *****************************************************************
      
      implicit none

      include 'dimension.h'
      include 'global.h'

      real*8 :: A2, A23, ACR, ACRt, AP1, AP2, ATIl, BF, DEL, DELp, DETcrt, ECOnd, GAMma, SCR, TCRt, UCRt
      integer :: NLWst
      common /CRIT  / TCRt, ECOnd, ACRt, UCRt, DETcrt, SCR, ACR, ATIl
      common /PARAM / AP1, AP2, GAMma, DEL, DELp, BF, A23, A2, NLWst

      real*8 :: Aj, Asaf, Rotemp
      integer :: Kk, Nnuc
      intent (in) Kk, Nnuc
      intent (inout) Rotemp

      real*8 :: ac, accn, ampl, cigor, momort, mompar, phi, rbmsph, saimid, saimin, saimx, selmax, shredt, stab,   &
                  & t, temp, u
      logical :: bcs
      real*8 :: BSQ, FSHELL, ROBCS, RODEF
      integer :: egsm, i, ia, iz
      integer :: INT
      real :: REAL

      bcs = .true.                                                 ! PARAM
      rbmsph = 0.01448*A(Nnuc)**1.6667
      ia = INT(A(Nnuc))
      iz = INT(Z(Nnuc))

      !-----do loop over angular momentum
      do i = 1, NLWst
         Aj = REAL(i) + HIS(Nnuc)

         !-----a-parameter and U determination for fission channel
         !-----------temperature fade-out of the shell correction
         !-----------ACCN  serves only to calculate temperature fade-out
         if(EX(Kk, Nnuc)>UCRt)then
            accn = ATIl*(1 + SHC(Nnuc)*(1 - EXP((-GAMma*EX(Kk,Nnuc))))/EX(Kk, Nnuc))
         else
            accn = ACRt
         endif
         temp = 0.
         if(EX(Kk, Nnuc)>=YRAst(i, Nnuc))temp = SQRT((EX(Kk,Nnuc) - YRAst(i,Nnuc))/accn)
         ampl = EXP(TEMp0*SHRt)
         shredt = 1.
         if(temp>=TEMp0)shredt = ampl*EXP(( - SHRt*temp))
         !--------temperature fade-out of the shell correction  --- done ----
         u = EX(Kk, Nnuc) + DEL - FISb(i, Nnuc) + SHC(Nnuc)*shredt*SHCjf(i, Nnuc)
         if(u>UCRt)then
            u = u - ECOnd
            bcs = .false.
         else
            bcs = .true.
         endif
         UEXcit(Kk, Nnuc) = MAX(u, 0.D0)
         if(u<=0.0D0)return
         if(Z(Nnuc)<102.0D0 .and. Z(Nnuc)>=19.0D0)then
            !-----------next line is to calculate deformation parameter A2 only
            call SIGMAK(A(Nnuc), Z(Nnuc), DEF(1, Nnuc), 0.0D0, u, accn, Aj, mompar, momort, A2, stab, cigor)
            call MOMFIT(iz, ia, i - 1, saimin, saimid, saimx, selmax)
            mompar = saimin*rbmsph
            momort = saimx*rbmsph
         else
            call SIGMAK(A(Nnuc), Z(Nnuc), DEF(1, Nnuc), 0.0D0, u, accn, Aj, mompar, momort, A2, stab, cigor)
         endif
         !--------calculation of level density parameter 'a' including surface
         !--------dependent factor
         ATIl = AP1*A(Nnuc) + AP2*A23*BSQ(cigor)
         ATIl = ATIl*ATIlnor(Nnuc)
         if(Asaf>=0.D0)ac = ATIl*FSHELL(u, SHC(Nnuc), Asaf)
         if(Asaf<0.D0)ac = -ATIl*Asaf
         if(ac<=0.D0)return

         if(bcs)then
            Rotemp = ROBCS(A(Nnuc), u, Aj, mompar, momort, A2, t, BF)
            if(i==1)then
               phi = SQRT(1.D0 - u/UCRt)
               t = 2.0*TCRt*phi/LOG((phi + 1.D0)/(1.D0 - phi))
            endif
         else
         !
         !-----------EGSM - J>>K (egsm=0) and EGSM (egsm=1)
         !
            egsm = 0
            Rotemp = RODEF(A(Nnuc), u, ac, Aj, mompar, momort, t, YRAst(i, Nnuc), HIS(Nnuc), BF, EXPmax, A2, egsm)
            if(i==1)t = SQRT(u/ac)
         endif

         ROF(Kk, i, Nnuc) = Rotemp
         if(i==1)TNUc(Kk, Nnuc) = t
      enddo
      return

   end subroutine DAMIRO_FISHI


   subroutine DAMI_RO_HFB_FIS(Nnuc, Ib, Rafis)
      !!
      !!CC  ******************************************************************
      !!CC
      !!CC          D A M I _ R O _ H F B _ F I S
      !!CC
      !!CC   Provides tables of saddle-point level densities extracted
      !!CC   from the RIPL-3 Hartree-Fock-Bogolyubov file and interpolated to
      !!CC   the currentenergy grid.
      !!CC
      !!CC    DAMI_RO_HFB_FIS
      !!CC    |-> HFB_FIS
      !!CC    |-> PLOT_ZVV_SadLD
      !!CC
      !!CC  ******************************************************************
      !!
      
      implicit none

      include 'dimension.h'
      include 'global.h'

      real*8 :: A2, A23, ACR, ACRt, AP1, AP2, ATIl, BF, DEL, DELp, DETcrt, ECOnd, GAMma, SCR, TCRt, UCRt
      real*8, dimension(NFHUMP) :: ACRtf, DETcrtf, ECOndf, SCRtf, TCRtf, UCRtf
      real*8, dimension(NFPARAB) :: MORtcrt, MPArcrt
      integer :: NLWst
      common /CRIT  / TCRt, ECOnd, ACRt, UCRt, DETcrt, SCR, ACR, ATIl
      common /CRITFIS/ ACRtf, UCRtf, TCRtf, DETcrtf, SCRtf, MORtcrt, MPArcrt, ECOndf
      common /PARAM / AP1, AP2, GAMma, DEL, DELp, BF, A23, A2, NLWst

      integer :: Ib, Nnuc
      real*8 :: Rafis
      intent (out) Rafis

      real*8 :: aaj, bbb, def2, excn1, ggg, mompar, rbmsph, rotemp, rrr1, rrr2, rrry, temp, xmax
      real :: FLOAT
      integer :: ia, iff, in, ipp, iz, jj, kk, nr
      integer :: INT


      iz = INT(Z(Nnuc))                                            ! PARAM
      ia = INT(A(Nnuc))
      in = ia - iz
      !-----continuum, level densities at saddle points
      excn1 = EMAx(Nnuc)
      !-----where continuum starts,ends,steps in between
      XMInn(Ib) = 0.0001
      do nr = 1, NRFdis(Ib)
         if(EFDis(nr, Ib)>XMInn(Ib))XMInn(Ib) = EFDis(nr, Ib)
      enddo
      !  IF(ECFis(ib).gt.0.) XMInn(Ib) = ECFis(ib)

      if(excn1<=(EFB(Ib) + XMInn(Ib)))then
         xmax = XMInn(Ib) + 3.5D0
      else
         xmax = excn1 - (EFB(Ib) + XMInn(Ib)) + 3.5D0
      endif
      DEStepp(Ib) = (xmax - XMInn(Ib))/100.
      NRBinfis(Ib) = INT((xmax - XMInn(Ib))/DEStepp(Ib))

      if(NRBinfis(Ib)>NFISENMAX)then
         write(8, *)' ERROR: LEVEL DENSITY AT SADDLE EXCEEDS DIMENSIONS', ' INCREASE NFISENMAX INsdimendion.h'
         stop 'ERROR: LEVEL DENSITY AT SADDLE EXCEEDS NFISENMAX'
      endif

      do kk = 1, NRBinfis(Ib)
         UGRid(kk, Ib) = XMInn(Ib) + (kk - 1)*DEStepp(Ib)
      enddo
      call HFB_FIS(Ib, Nnuc)
      Rafis = 1.D0

      iff = BFF(Ib)
      rbmsph = 0.01448*A(Nnuc)**1.66667
      !  See eq.(1.38) of the Ignatyuk Book (Stat.prop....)

      bbb = DEFfis(Ib)    ! the actual static saddle deformation is used
      if(bbb>1.5D0)then
         write(8, *)' WARNING: DEFORMATION RESET TO 1.5 FOR HFB FISS.BARRIER B=', Ib
         write(8, *)' WARNING:  FOR MOMENT OF INERTIA CALCULATION (SIGMAK) '
         bbb = 1.5D0
      endif

      ggg = PI/3.                    ! axial symmetry
      if(iff==2)ggg = PI/18.         ! arbitrarily fixed asymmetry to 10 degrees

      rrr2 = 1. + SQRT(5./4./PI)*bbb*COS(ggg - 4.*PI/3.)
      rrr1 = 1. + SQRT(5./4./PI)*bbb*COS(ggg - 2.*PI/3.)

      rrry = rrr2/rrr1
      def2 = (rrry - 1.0)/(1.0 + 0.5*rrry)
      if(def2>0.9D0)def2 = 0.9
      if(def2<( - 0.9D0))def2 = -0.9

      mompar = rbmsph*(1. - (2./3.)*def2)

      do jj = 1, NLW
         aaj = FLOAT(jj) + HIS(Nnuc)
         do kk = 1, NRBinfis(Ib)
            temp = TNUcf(kk, Nnuc)

            !-----------SYMMETRY ENHANCEMENT
            !  The normal GS is axial and mass symmetric

            do ipp = 1, 2
               rotemp = ROFisp(kk, jj, ipp, Ib)
               !  Nonaxial symmetry with mass symmetry
               if(iff==2)rotemp = rotemp*SQRT(PI/2.D0)*SQRT(mompar*temp)
               !  Mass asymmetry (with axial symmetry) is already considered in HFB calculations
               !  IF (Iff.EQ.3) rotemp = rotemp*2.   ! axial, mass asymmetry
               ROFisp(kk, jj, ipp, Ib) = rotemp*ROHfb_norm(Ib)
            enddo
         enddo
      enddo

      if(IOUt==6)call PLOT_ZVV_SadLD(Nnuc, Ib)

   end subroutine DAMI_RO_HFB_FIS


   subroutine HFB_FIS(Ib, Nnuc)
      
      !!C
      !!C   *********************************************************************
      !!C   *                                                         CLASS:PPU *
      !!C   *                      R O H F B_FIS                                *
      !!C   *                                                                   *
      !!C   *  Reads level densities numerically calculated by a combinatorial  *
      !!C   *  method using Hartree-Fock-Bogoliubov single particle levels      *
      !!C   *            (to be included within RIPL-3)                         *
      !!C   *                                                                   *
      !!C   *     S.Hilaire and S. Goriely, Nucl.Phys.A 779 (2006) 63-81        *
      !!C   *  "Global microscopic nuclear level densities within the HFB plus  *
      !!C   *   combinatorial method for practical applications"                *
      !!C   *                                                                   *
      !!C   *  Interpolates LDs linearily in log to the EMPIRE energy grid.     *
      !!C   *********************************************************************
      
      
      implicit none

      include 'dimension.h'
      include 'global.h'

      integer, parameter :: NLDGRID = 60, JMAX = 50

      integer :: Ib, Nnuc
      intent (in) Ib, Nnuc

      real*8 :: c1, c2, hhh, r1, r2, u
      character(2) :: car2
      real*8, dimension(0:NLDGRID, 2) :: cgrid, rhoogrid, rhotgrid
      real*8 :: DEXP, DSQRT
      real*8 :: DLOG10
      character(38) :: filename
      integer :: i, ia, iar, ipp, iugrid, iz, izr, j, jmaxl, k, khi, kk, klo
      real*8, dimension(0:NLDGRID, JMAX, 2) :: rhogrid
      real*8, dimension(0:NLDGRID) :: tgrid, uugrid
      real :: TRIM


      ia = int(A(Nnuc))
      iz = int(Z(Nnuc))
      !
      !-----initialization
      !
      jmaxl = MIN(NDLW, JMAX)
      do i = 0, NLDGRID
         uugrid(i) = 0.D0
         tgrid(i) = 0.D0
         do ipp = 1, 2
            cgrid(i, ipp) = 0.D0
            rhoogrid(i, ipp) = 0.D0
            rhotgrid(i, ipp) = 0.D0
            do j = 1, jmaxl
               rhogrid(i, j, ipp) = 0.D0
            enddo
         enddo
      enddo

      !-----reading microscopic lev. dens. from the RIPL-3 file
      write(filename, 99005)Ib, iz
      99005 format('/RIPL/FISSION/LEVELDENSITIES/MAX', i1, '/Z', i3.3)
      open(unit = 34, file = trim(EMPiredir)//filename, err = 300)
      do while (.true.)
         !  OPEN (UNIT = 34,FILE = trim(empiredir)//
         !  &'/RIPL/fission/leveldensities/Max/'//ctmp6, ERR = 300)
         read(34, 99010, err = 300, end = 300)car2
         if(car2/='Z=')then
         else
            backspace(34)
            read(34, 99010, err = 300, end = 300)car2, izr, iar
            if(iar/=ia .or. izr/=iz)then
            else
               read(34, *, end = 300)
               read(34, *, end = 300)
               i = 1
               do while (.true.)
                  read(34, 99015, end = 300)uugrid(i), tgrid(i), cgrid(i, 1), rhoogrid(i, 1), rhotgrid(i, 1), &
                     & (rhogrid(i, j, 1), j = 1, jmaxl)
                  if(uugrid(i)<=0.001)exit
                  if(i==NLDGRID)then
                     i = 1
                     read(34, *, end = 300)
                     exit
                  else
                     i = i + 1
                  endif
               enddo
               !  SKIPPING 4 TITLE LINES
               read(34, *, end = 300)
               read(34, *, end = 300)
               read(34, *, end = 300)
               read(34, *, end = 300)
               do while (.true.)
                  read(34, 99015, end = 300)uugrid(i), tgrid(i), cgrid(i, 2), rhoogrid(i, 2), rhotgrid(i, 2), &
                     & (rhogrid(i, j, 2), j = 1, jmaxl)
                  if(uugrid(i)<=0.001)goto 400
                  if(i==NLDGRID)goto 400
                  i = i + 1
               enddo
            endif
         endif
      enddo
      300 continue
      write(8, *)' NO LEV. DENS. FOR Z=', iz, ' A=', ia, ' IN HFB AT SADDLE ', Ib
      write(8, *) ' USE OTHER LEVEL DENSITIES. EXECUTION TERMINATED '
      write(8, *)' ERROR: HFB LEV DENS. AT SADDLE', Ib, '  MISSING'
      stop ' ERROR: HFB LEV DENS. AT SADDLE MISSING'
      400 continue
      close(34)
      iugrid = i - 1
      do kk = 1, NRBinfis(Ib)
         u = XMInn(Ib) + (kk - 1)*DEStepp(Ib) - ROHfbp_sd(Ib)
         if(u<0.)cycle
         if(u>200.0D0)then
            write(8, *)' '
            write(8, *)' HFB LEV. DENS. DEFINED UP TO 200 MEV ONLY'
            write(8, *)' REQUESTED ENERY IS ', u, ' MEV'
            write(8, *)' YOU HAVE TO USE ANOTHER LEVEL DENSITIES'
            write(8, *)' EXECUTION STOPPED'
            stop 'TOO HIGH ENERGY FOR HFB LEV. DENS.'
         endif
         !
         !--------interpolation in the level density tables
         !
         klo = 1
         khi = iugrid
         if(u<=uugrid(klo))then
            klo = 0
            khi = 1
            goto 500
         endif
         if(u>=uugrid(khi))then
            klo = iugrid - 1
            goto 500
         endif
         do while (khi - klo>1)
            k = int((khi + klo)/2.)
            if(uugrid(k)>u)then
               khi = k
            else
               klo = k
            endif
         enddo
         500 continue
         hhh = uugrid(khi) - uugrid(klo)
         c1 = (uugrid(khi) - u)/hhh
         c2 = (u - uugrid(klo))/hhh
         do j = 1, jmaxl
            do ipp = 1, 2
               r1 = rhogrid(klo, j, ipp)
               r2 = rhogrid(khi, j, ipp)
               if(r1>1.D-12 .and. r2>1.D-12)then
                  ROFisp(kk, j, ipp, Ib) = 10.**(c1*DLOG10(r1) + c2*DLOG10(r2))
                  if(ROHfba_sd(Ib)/=0.D0)ROFisp(kk, j, ipp, Ib) = ROFisp(kk, j, ipp, Ib)*dexp(ROHfba_sd(Ib)*dsqrt(u))
               else
                  ROFisp(kk, j, ipp, Ib) = (c1*r1 + c2*r2)
               endif
               if(ROFisp(kk, j, ipp, Ib)<0.D0)ROFisp(kk, j, ipp, Ib) = 0.D0
            enddo
         enddo
         TNUcf(kk, Nnuc) = c1*tgrid(klo) + c2*tgrid(khi)
      enddo
      return
      99010 format(23x, a2, i3, 3x, i3)
      99015 format(1x, f6.2, f7.3, 1x, 53E9.2)
   end subroutine HFB_FIS


   subroutine DAMI_ROFIS(Nnuc, Ib, Mmod, Rafis)
      !!
      !!CC  ************************************************************
      !!CC  *
      !!CC  *            D A M I _ R O F I S
      !!CC  *
      !!CC  *   Provides saddle point level densities using Empire
      !!CC  *   specific level density model (BCS below critical energy
      !!CC  *   and Fermi gas with collectiobe enhacements above)
      !!CC  *
      !!CC  *    DAMI_ROFIS
      !!CC  *    |-> EGSMsys
      !!CC  *    |-> DAMIRO_CRT
      !!CC  *    |-> PLOT_ZVV_SadLD
      !!CC  *    |-> FSHELL
      !!CC  *    |-> SIGMAK
      !!CC  *    |->RODEFF
      !!CC  *        |-> RODEF
      !!CC  *        |-> COLL_KQ_FIS
      !!CC  *    |-> ROBCSF
      !!CC  *        |-> ROBCS
      !!CC  *        |-> COLL_KQ_FIS
      !!CC  *
      !!CC  **************************************************************
      !!
      
      implicit none

      include 'dimension.h'
      include 'global.h'

      real*8 :: A2, A23, ACR, ACRt, AP1, AP2, ATIl, BF, DEL, DELp, DETcrt, ECOnd, GAMma, SCR, TCRt, UCRt
      real*8, dimension(NFHUMP) :: ACRtf, DETcrtf, ECOndf, SCRtf, TCRtf, UCRtf
      real*8, dimension(NFPARAB) :: MORtcrt, MPArcrt
      integer :: NLWst
      common /CRIT  / TCRt, ECOnd, ACRt, UCRt, DETcrt, SCR, ACR, ATIl
      common /CRITFIS/ ACRtf, UCRtf, TCRtf, DETcrtf, SCRtf, MORtcrt, MPArcrt, ECOndf
      common /PARAM / AP1, AP2, GAMma, DEL, DELp, BF, A23, A2, NLWst

      integer :: Ib, Mmod, Nnuc
      real*8 :: Rafis
      intent (in) Mmod, Rafis

      real*8 :: aaj, accn, aj, ar, cigor, def2, desteppp, excn1, mm2, momort, mompar, r0, rotemp, shcf, stab,     &
                  & temp, u, vibbf12, vibbfdt, vn, xmax
      real*8 :: BSQ, FSHELL, ROBCSF, RODEFF
      real*8 :: dshiff
      real :: FLOAT
      integer :: i, ia, iff, in, iz, jj, kk, nr
      integer :: INT


      !-----continuum, level densities at saddle points
      excn1 = EMAx(Nnuc)                                           ! PARAM
      !-----where continuum starts,ends,steps in between
      if(Mmod==0)then
         XMInn(Ib) = 0.01
         do nr = 1, NRFdis(Ib)
            if(EFDis(nr, Ib)>XMInn(Ib))XMInn(Ib) = EFDis(nr, Ib)
         enddo
         if(ECFis(Ib)>0.)XMInn(Ib) = ECFis(Ib)

         if(excn1<=(EFB(Ib) + XMInn(Ib)))then
            xmax = XMInn(Ib) + 3.5D0
         else
            xmax = excn1 - (EFB(Ib) + XMInn(Ib)) + 3.5D0
         endif
         DEStepp(Ib) = (xmax - XMInn(Ib))/100.
         NRBinfis(Ib) = INT((xmax - XMInn(Ib))/DEStepp(Ib))

         if(NRBinfis(Ib)>NFISENMAX)then
            write(8, *)' ERROR: LEVEL DENSITY AT SADDLE EXCEEDS DIMENSIONS', ' INCREASE NFISENMAX INsdimendion.h'
            stop 'ERROR: LEVEL DENSITY AT SADDLE EXCEEDS NFISENMAX'
         endif
         do kk = 1, NRBinfis(Ib)
            UGRid(kk, Ib) = XMInn(Ib) + (kk - 1)*DEStepp(Ib)
         enddo
      else    ! Mmod.GT.0
         XMInnm(Mmod) = 0.0001
         do nr = 1, NRFdis(Ib)
            if(EFDism(nr, Mmod)>XMInnm(Mmod))XMInnm(Mmod) = EFDism(nr, Mmod)
         enddo
         if(ECFism(Mmod)>0.)XMInnm(Mmod) = ECFism(Mmod)

         if(excn1<=(EFBm(Mmod) + XMInnm(Mmod)))then
            xmax = XMInn(Mmod) + 3.D0
         else
            xmax = excn1 - (EFBm(Mmod) + XMInnm(Mmod)) + 3.D0
         endif
         DEStepm(Mmod) = (xmax - XMInnm(Mmod))/100.D0
         NRBinfism(Mmod) = INT((xmax - XMInnm(Mmod))/DEStepm(Mmod))
         if(NRBinfism(Mmod)>NFISENMAX)then
            write(8, *)' ERROR: LEVEL DENSITY AT SADDLE EXCEEDS DIMENSIONS', ' INCREASE NFISENMAX INsdimendion.h'
            stop 'ERROR: LEVEL DENSITY AT SADDLE EXCEEDS NFISENMAX'
         endif

         do kk = 1, NRBinfism(Mmod)
            UGRidf(kk, Mmod) = XMInnm(Mmod) + (kk - 1)*DEStepm(Mmod)
         enddo
      endif

      iz = INT(Z(Nnuc))
      ia = INT(A(Nnuc))
      in = ia - iz
      A23 = A(Nnuc)**0.666667D0
      mm2 = 0.24*A23
      r0 = 1.24
      iff = 1
      temp = 0.D0
      !-----EMPIRE-3.0-dependence
      call EGSMsys(AP1, AP2, GAMma, DEL, DELp, Nnuc)

      if(Mmod==0)then
         GAMma = GAMmafis(Ib)
         dshiff = DELtafis(Ib)
         shcf = SHCfis(Ib)
         iff = BFF(Ib)
         desteppp = DEStepp(Ib)
         vibbf12 = VIBf12(Ib)
         vibbfdt = VIBfdt(Ib)
         vn = VIBfnorm(Ib)
      else    ! Mmod.GT.0
         NRBinfis(Ib) = NRBinfism(Mmod)
         XMInn(Ib) = XMInnm(Mmod)
         GAMma = GAMmafism(Mmod)
         dshiff = DELtafism(Mmod)
         shcf = SHCfism(Mmod)
         iff = BFFm(Mmod)
         desteppp = DEStepm(Mmod)
         vibbf12 = VIBf12m(Mmod)
         vibbfdt = VIBfdtm(Mmod)
         vn = VIBfnormm(Mmod)
      endif

      GAMma = GAMma/A(Nnuc)**0.333333
      ATIl = AP1*A(Nnuc) + AP2*A23
      ATIl = ATIl*Rafis
      ar = ATIl*(1.0 + shcf*GAMma)

      DELp = 14./SQRT(A(Nnuc))
      DEL = 0.
      if(FISden(Nnuc)<=1.)then
         if(MOD(in, 2)/=0)DEL = DELp
         if(MOD(iz, 2)/=0)DEL = DEL + DELp
      endif

      aj = 0.
      u = 0.
      !  ROFisp(nfisenmax,ndlw,2,nfhump),
      do kk = 1, NDEX
         do i = 1, NDLW
            ROFisp(kk, i, 1, Ib) = 0.D0
            ROFisp(kk, i, 2, Ib) = 0.D0
         enddo
      enddo

      def2 = DEFfis(Ib)    ! the actual static saddle deformation is used
      if(def2>1.5D0)then
         write(8, *)' WARNING: DEFORMATION RESET TO 1.5 FOR FISSION BARRIER B=', Ib
         write(8, *)' WARNING:  FOR FISSION LD CALCULATIONS (SIGMAK,RODEFF)   '
         def2 = 1.5D0
      endif

      if(iff==2)then
         !  Non-axial, non-axiality parameter assumed as 10 degrees
         call SIGMAK(A(Nnuc), Z(Nnuc), def2, -2.D0, u, ar, aj, mompar, momort, A2, stab, cigor)
      else
         !  Axial symmetry
         call SIGMAK(A(Nnuc), Z(Nnuc), def2, -1.D0, u, ar, aj, mompar, momort, A2, stab, cigor)
      endif


      !-----calculation of level density parameter 'a' including surface
      !-----dependent factor
      ATIl = AP1*A(Nnuc) + BSQ(cigor)*AP2*A23
      ATIl = ATIl*Rafis

      call DAMIRO_CRT(ia, iz, shcf, IOUt, 1)

      MOMparcrt = mompar
      MOMortcrt = momort

      if(mompar<0.0D0 .or. momort<0.0D0)then
         write(8, *)'WARNING: NEGATIVE MOMENT OF INERTIA FOR SPIN ', aj
         write(8, *)'WARNING: 0 LEVEL DENSITY RETURNED BY RODEF'
         return
      endif

      do jj = 1, NLW
         aaj = FLOAT(jj) + HIS(Nnuc)
         do kk = 1, NRBinfis(Ib)
            rotemp = 0.D0
            u = XMInn(Ib) + (kk - 1)*desteppp + DEL + dshiff
            if(u>UCRt)then
               u = u - ECOnd
               accn = ATIl*FSHELL(u, shcf, GAMma)
               if(accn<=0.0D0)return

               rotemp = RODEFF(A(Nnuc), u, accn, aaj, mompar, momort, HIS(Nnuc), EXPmax, temp, def2, vibbf12, vibbfdt, vn)
            else
               accn = ACRt
               rotemp = ROBCSF(A(Nnuc), u, aaj, MOMparcrt, MOMortcrt, mompar, temp, def2, vibbf12, vibbfdt, vn)
            endif
            !-----------SYMMETRY ENHANCEMENT
            !-----------The normal GS is axial and mass symmetric

            !-----------Nonaxial symmetry with mass symmetry
            if(iff==2)rotemp = rotemp*SQRT(PI/2.)*SQRT(mompar*temp)
            !---------- Axial symmetry with mass asymmetry
            if(iff==3)rotemp = rotemp*2.D0

            ROFisp(kk, jj, 1, Ib) = rotemp
            ROFisp(kk, jj, 2, Ib) = rotemp

            if(Mmod>0)ROFism(kk, jj, Mmod) = rotemp
         enddo
      enddo

      if(Mmod==0)then

         ACRtf(Ib) = ACRt
         UCRtf(Ib) = UCRt
         ECOndf(Ib) = ECOnd
         DETcrtf(Ib) = DETcrt
         TCRtf(Ib) = TCRt
         SCRtf(Ib) = SCR

         VIBf12(Ib) = vibbf12
         VIBfdt(Ib) = vibbfdt
         VIBfnorm(Ib) = vn

      else    ! Mmod.GT.0

         ACRtf(Mmod) = ACRt
         UCRtf(Mmod) = UCRt
         ECOndf(Mmod) = ECOnd
         DETcrtf(Mmod) = DETcrt
         TCRtf(Mmod) = TCRt
         SCRtf(Mmod) = SCR

         VIBf12(Mmod) = vibbf12
         VIBfdt(Mmod) = vibbfdt
         VIBfnorm(Mmod) = vn

      endif

      if(IOUt==6)call PLOT_ZVV_SadLD(Nnuc, Ib)
      return

   end subroutine DAMI_ROFIS



   function ROBCSF(A, U, Aj, Mompar, Momort, Mompr, T, Def2, Vibbf12, Vibbfdt, Vn)
      !==============================================================
      !==============================================================

      implicit none

      real*8 :: ACR, ACRt, ATIl, DETcrt, ECOnd, MOMo, MOMp, SCR, TCRt, UCRt
      common /CRIT  / TCRt, ECOnd, ACRt, UCRt, DETcrt, SCR, ACR, ATIl
      common /MOM   / MOMo, MOMp
      real*8 :: A, Aj, Def2, Momort, Mompar, Mompr, T, U, Vibbf12, Vibbfdt, Vn
      real*8 :: ROBCSF
      intent (out) Mompr
      real*8 :: bf, ro, rot_k, rot_q, vib_kq
      real*8 :: ROBCS

      ROBCSF = 0.D0                                                ! CRIT
      bf = 0.D0
      ro = ROBCS(A, U, Aj, Mompar, Momort, Def2, T, bf)
      Mompr = MOMp
      call COLL_KQ_FIS(0, A, T, MOMo, Def2, U, vib_kq, rot_k, rot_q, Vibbf12, Vibbfdt, Vn)
      ROBCSF = ro*rot_k*rot_q*vib_kq
      return

   end function ROBCSF


   function RODEFF(A, E, Ac, Aj, Mompar, Momort, Ss, Expmax, T, Def2, Vibbf12, Vibbfdt, Vn)
      !==============================================================
      !==============================================================
      
      implicit none

      real*8 :: A, Ac, Aj, E, Expmax, Momort, Mompar, Ss, T, Vibbf12, Vibbfdt, Vn
      real*8 :: Def2
      real*8 :: RODEFF
      real*8 :: bf, e1, ro, rot_k, rot_q, vib_kq, yrast
      integer :: egsm
      real*8 :: RODEF

      Expmax = 700.
      bf = 0.D0
      if(Ac<=0. .or. E<=0.D0)return
      !-----Fission LD: EGSM - J>>K (egsm=0) and EGSM (egsm=1)
      egsm = 0
      RODEFF = 0.D0
      yrast = 0.D0
      ro = RODEF(A, E, Ac, Aj, Mompar, Momort, T, yrast, Ss, bf, Expmax, Def2, egsm)
      e1 = E - yrast
      call COLL_KQ_FIS(egsm, A, T, Momort, Def2, e1, vib_kq, rot_k, rot_q, Vibbf12, Vibbfdt, Vn)
      RODEFF = ro*rot_k*rot_q*vib_kq

      return

   end function RODEFF


   subroutine COLL_KQ_FIS(Egsm, A, T, Momo, Def2, E1, Vib_kq, Rot_k, Rot_q, Thalf, Dt, Vn)
      
      implicit none

      real*8 :: A, Def2, Dt, E1, Momo, Rot_k, Rot_q, T, Thalf, Vib_kq, Vn
      integer :: Egsm
      intent (in) A, Def2, Dt, E1, Egsm, Momo, T, Thalf, Vn
      intent (out) Rot_k, Rot_q, Vib_kq
      real*8 :: arg, cost, dmpdiff, dmphalf, ht, m0, pi, qr, qv, r0, sdrop, vibrk

      !-----vib_K
      data m0, pi, r0, ht/1.044D0, 3.1415926535897932D0, 1.26D0, 6.589D0/
      sdrop = 17.D0/(4*pi*r0**2)
      cost = 3.D0*m0*A/(4*pi*ht**2*sdrop)
      vibrk = Vn*4.D0*EXP(1.7D0*cost**(2.D0/3.D0)*T**(4.D0/3.D0))

      !-----vib_Q
      arg = (T - Thalf)/Dt
      qv = 1.D0/(EXP((-arg)) + 1.D0)
      if(qv>=0.999D0)vibrk = 1.D0
      Vib_kq = qv - vibrk*(qv - 1.D0)

      !-----rot_K
      Rot_k = 1.D0
      if(Egsm==0)Rot_k = Momo*T

      !-----rot_Q
      dmphalf = 120.D0*A**0.333*Def2**2            !according to RIPL
      dmpdiff = 1400.D0*A**( - 0.666)*Def2**2
      qr = 1.D0/(1 + EXP((-dmphalf/dmpdiff))) - 1.D0/(1 + EXP((E1-dmphalf)/dmpdiff))
      Rot_q = 1.D0 - qr*(1.D0 - 1.D0/(Momo*T))

      return

   end subroutine COLL_KQ_FIS


   subroutine GSMsys(Nnuc, Ap1, Ap2, Gamma, Del, Delp, Om2, Om3, Cga)
      !!cc 
      !!cc  ********************************************************************
      !!cc  *                    R O G S M_sys                                 *
      !!cc  *                                                                  *
      !!cc  * GSM level density systematics  from RIPL-2             .         *
      !!cc  *                                                                  *
      !!cc  ********************************************************************
      !!cc 
      
      implicit none

      include 'dimension.h'
      include 'global.h'

      real*8 :: Ap1, Ap2, Cga, Del, Delp, Gamma, Om2, Om3
      integer :: Nnuc
      intent (in) Nnuc
      intent (out) Ap1, Ap2, Cga, Gamma, Om3
      intent (inout) Del, Delp, Om2
      real*8 :: a13, a23, arobn, dshif
      real :: FLOAT
      real :: gamm
      integer :: ia, iz
      integer :: INT

      ia = INT(A(Nnuc))
      iz = INT(Z(Nnuc))
      a23 = float(ia)**0.666667D0
      a13 = float(ia)**0.333333D0


      arobn = ATIl_ig(Nnuc)
      Delp = DELp_ig(Nnuc)
      Om2 = OM2_ig(Nnuc)
      if(Om2<=0.D0)Om2 = 30./float(ia)**.66666

      dshif = DSHift_ig(Nnuc)
      if(arobn<=0.D0)dshif = 0.617 - 0.00164*float(ia)

      gamm = 0.375
      Gamma = gamm/a13

      Om3 = 50./a23
      Cga = .0075*a13

      Ap1 = 0.103
      Ap2 = -0.105

      Delp = 12./SQRT(A(Nnuc))
      Del = 0.D0
      if(MOD((ia-iz), 2)/=0.0D0)Del = Delp
      if(MOD(iz, 2)/=0.0D0)Del = Del + Delp

      Del = Del + dshif
      return

   end subroutine GSMSYS



   subroutine EGSMsys(Ap1, Ap2, Gamma, Del, Delp, Nnuc)
      !!cc 
      !!cc  ********************************************************************
      !!cc  *                                                          class:au*
      !!cc  *                    E G S M s y s                                 *
      !!cc  *                                                                  *
      !!cc  * EGSM level density systematics fitted to Do from RIPL-3.         *
      !!cc  *                                                                  *
      !!cc  * Set coefficients in the level-density-parameter formula          *
      !!cc  * used in the EMPIRE-specific (EGSM) model:                        *
      !!cc  * atil = ap1*A(Nnuc) + ap2*A(nnuc)**2/3                            *
      !!cc  * gamma = gam/A(nnuc)**1/3                                         *
      !!cc  *                                                                  *
      !!cc  * Using liquid drop vibrational enhancement factor (EMPIRE-2.19)   *
      !!cc  *                                                                  *
      !!cc  * MINUIT fit results:                                              *
      !!cc  * EXT PARAMETER                                                    *
      !!cc  * NO.   NAME        VALUE                                          *
      !!cc  *  1     A1        0.74055E-01                                     *
      !!cc  *  2     A2        0.28598E-03                                     *
      !!cc  *  3     gam       0.57248                                         *
      !!cc  *                                                                  *
      !!cc  *  frm=1.70   Chi**2=36 (per degree of freedom)                    *
      !!cc  *                                                                  *
      !!cc  * author: M.Herman                                                 *
      !!cc  * date:   December 2008                                            *
      !!cc  ********************************************************************
      !!      
      implicit none

      include 'dimension.h'
      include 'global.h'

      real*8 :: Ap1, Ap2, Del, Delp, Gamma
      integer :: Nnuc
      intent (in) Nnuc
      intent (out) Gamma
      intent (inout) Ap1, Ap2, Del, Delp
      real*8 :: gam
      integer :: INT

      Del = 0.D0
      Delp = 12./SQRT(A(Nnuc))
      if(MOD(XN(Nnuc), 2.D0)/=0.0D0)Del = Delp
      if(MOD(Z(Nnuc), 2.D0)/=0.0D0)Del = Del + Delp
         !****************************************************************
         !ccc  * MINUIT fit results:
         !
         !-----parameters of Dec 4, 2008
         !  frm=1.70   Chi**2=36 (per degree of freedom)
         !  ap1 = 0.74055E-01
         !  ap2 = 0.28598E-03
         !  gam = 0.57248
         !
         !****************************************************************
         !
         !-----parameters of Jan 26, 2011
         !  Do-fit using RIPL-3 database, 2.19 vibr enhancement (MINUIT)
         !     alpha 0=  .0750000 delta alpha= .500000D-01
         !     gam   0=  .5750000 delta gam  = .500000D-02
         !  ---------------------------------
         !  alpha=   7.488729E-02 gam=   5.697688E-01
         !  frm=       1.687021929004768 Chi^2=      27.301609174895010
         !      ap1= 7.488729D-02
         !      ap2= 0.d0
         !      gam= 5.697688D-01
         !
         !      EGSM model is used
         !****************************************************************
         !
         !-----parameters of Jan 16, 2012 (EMPIRE v.2182 for LD routines)
         !   Do-fit using RIPL-3 database, 2.19 vibr enhancement
         !      alpha 0=  .0750000 delta alpha= .500000D-01
         !      gam   0=  .5750000 delta gam  = .500000D-02
         !---------------------------------
         !  alpha=   7.481815E-02  beta=   0.000000E+00  gam=   5.609533E-01
         !  frm=       1.672033623535850 Chi^2=      27.242032055693340
         !  Ncalc=        255 Nres=        300 N(Ucrt>Bn)=         48
         !  48 nuclei having Ucrt>Bn skipped in the MINUIT fit
         !
         !      EGSM (J>>K approx.)
         !      Closed formula for spin dependence instead of sum_K
         !
      Ap1 = 7.481815D-02
      Ap2 = 0.D0
      gam = 5.609533D-01
      Gamma = gam/A(Nnuc)**(1.D0/3.D0)
      if(ATIlnoz(INT(Z(Nnuc)))==0.D0)return
      Ap1 = Ap1*ATIlnoz(INT(Z(Nnuc)))    !apply elemental normalization factor
      Ap2 = Ap2*ATIlnoz(INT(Z(Nnuc)))    !apply elemental normalization factor
      return
   end subroutine EGSMSYS

