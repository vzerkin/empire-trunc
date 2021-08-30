subroutine ROGCC(Nnuc,Cf,Asaf)
   !CC
   !CC   *****************************************************************
   !CC   *                                                      CLASS:PPU*
   !CC   *                         R O G C C                             *
   !CC   *                                                               *
   !CC   *                                                               *
   !CC   * CALCULATES TABLE OF ENERGY AND SPIN DEPENDENT LEVEL DENSITIES *
   !CC   * FOR NUCLEUS NNUC ACCORDING TO GILBERT-CAMERON WITH COLLECTIVE *
   !CC   * ENHANCENENT FCTORS AND USING EGSM SYSTEMATICS FOR LD PARAMETER*
   !CC   * AT NEUTRON BINDING ENERGY.                                    *
   !CC   *                                                               *
   !CC   * INPUT:                                                        *
   !CC   *  NNUC - index of the nucleus                                  *
   !CC   *  CF   - 1. for the saddle point, 0. otherwise                 *
   !CC   *  Asaf - controls a-parameter at a saddle point                *
   !CC   *       - IF Asaf.GE.0 it corresponds to the gamma-param.       *
   !CC   *         in the Ignatyuk formula (Asaf=0 selects               *
   !CC   *         asymptotic value for a)                               *
   !CC   *       - IF Asaf.lt.0 asymptotic value of a-parameter          *
   !CC   *         times ABS(Asaf) is taken for at the saddle point      *
   !CC   *  BF controls shape of the nucleus                             *
   !CC   *     BF=0. stands for the saddle point                         *
   !CC   *     BF=1. stands for the oblate   yrast state                 *
   !CC   *     BF=2. stands for the prolate  yrast state                 *
   !CC   *     BF=3. stands for the triaxial yrast state                 *
   !CC   *  SCUTF - SPIN CUT-OFF FACTOR (0.146 IS RECOMMENDED)           *
   !CC   *                                                               *
   !CC   * OUTPUT:RO(.,.,NNUC) - LEVEL DENSITIES                         *
   !CC   *                                                               *
   !CC   *****************************************************************
   !CC      

   implicit none

   include 'dimension.h'
   include 'global.h'
   
   !
   ! Dummy arguments
   !
   integer*4, intent(in) :: Nnuc          ! nucleus index
   real*8,intent(in) :: Asaf              ! controls a-parameter at a saddle point 
   real*8,intent(in) :: Cf                ! 1. for the saddle point, 0. otherwise
   !
   ! common variables
   !
   real*8 :: A2                           ! EPS deformation including damped static and dynamical deformation
   real*8 :: A23                          ! A**(2/3)
   real*8 :: AP1                          ! parameter in the Ignatyuk 'a' systematics 
   real*8 :: AP2                          ! parameter in the Ignatyuk 'a' systematics
   real*8 :: GAMma                        ! parameter in the Ignatyuk 'a' systematics 
   real*8 :: BF                           ! controls shape of the nucleus (see above)
   real*8 :: DEL                          ! pairing shift (neutrons plus protons)
   real*8 :: DELp                         ! pairing shift for single gas (systematics)
   real*8 :: Scutf                        ! spin cut-off factor (0.146 is recommended)
   integer*4 :: NLWst                     ! number of partial waves up to stability against fission
   common /PARAM / AP1, AP2, GAMma, DEL, DELp, BF, A23, A2, NLWst

   real*8 :: TCRt                         ! critical temerature
   real*8 :: ECOnd                        ! condenstaion energy
   real*8 :: ACRt                         ! level density parameter at critical energy (temporary value in iteration LIKELY NOT NEEDED)
   real*8 :: UCRt                         ! critical energy
   real*8 :: DETcrt                       ! determinant at critical energy
   real*8 :: SCR                          ! entropy at critical energy 
   real*8 :: ACR                          ! level density parameter at critical energy
   real*8 :: atil                         ! asymptotic value of LD parameter 'a' 
   common /CRIT  / TCRt, ECOnd, ACRt, UCRt, DETcrt, SCR, ACR, ATIl

   real*8 :: am                           ! level density parameter 'a' 
   real*8 :: T                            ! nuclear temperature in Constatnt Temperature (CT) model
   real*8 :: eo                           ! Eo enrgy shift in CT formula 
   real*8 :: ux                           ! matching energy between CT and Fermi-gas level densities
   common /CT/ am,ux,eo,T
   !
   ! Local variables
   !
   integer*4, PARAMETER :: nde = 20       ! number of exc. energies in the Fermi-gas table for matching CT
   real*8 :: FSHELL                       ! energy dependence of the LD 'a' parameter due to shell (function)
   real*8 :: mompar                       ! moment of inertia with respect to the symmetry axis
   real*8 :: momort                       ! moment of inertia with respect to the orthogonal to the symmetry axis
   real*8 :: seff2                        ! effective moment of inertia - mompar**0.333D0*momort**0.6666D0*T
   real*8 :: ratio(1:2) = 0.5             ! parity ratio
   real*8 :: exl = 0.0D0                  ! upper energy boundary for CT  
   real*8 :: ro_u = 0.0D0                 ! level density at the low-energy side of the matching point ??? 
   real*8 :: ro_j = 0.0D0                 ! spin/parity dependent level density at the low-energy side of the matching point ???
   real*8 :: sigh = 0.0D0                 ! spin cut-off calculated from Fermi-gas at Ux
   real*8 :: sigl = 0.0D0                 ! spin cut-off calculated from discrete levels
   real*8 :: e                            ! excitation energy
   real*8 :: u                            ! excitation energy  
   real*8 :: fde = 20.0D0/nde             ! energy step in Fermi-gas LD for matching CT LD
   real*8 :: roFG(0:nde)= 0.0D0           ! total Fermi-gas LD for matching CT LD 
   ! real*8 :: deriv1, deriv2               ! derivatives of Fermi-gas level densities at two excitation energies
   real*8 :: eps                          ! tolerance when comparing two matching quantities
   real*8 :: Tct                          ! temperature in constant temperature model
   real*8 :: El                           ! energy of the lowest discrete level used in a cumulative-number of levels fit
   real*8 :: Eu                           ! energy of the highest discrete level used in a cumulative-number of levels fit
   real*8 :: stab                         ! stability limit versus fission
   real*8 :: cigor                        ! ratio of the longest axis to the shortest one
   real*8 :: xj                           ! running value of spin
   ! real*8 :: a1, b1, c1                   ! auxiliary variables
   ! real*8 :: enorm                        ! probbably USELES 
   real*8 :: roUx                         ! FG total level densities at the Ux matching point (= UCRt + DEL)
   integer*4 :: Nl                        ! number of the lowest discrete level used in a cumulative-number of levels fit
   integer*4 :: Nu                        ! number of the highest discrete level used in a cumulative-number of levels fit
   integer*4 :: ia                        ! nucleus mass number (A)
   integer*4 :: in                        ! number of neutrons
   integer*4 :: iz                        ! number of protons
   integer*4 :: i, ig, j, iux       ! running indices

   real*8 :: ro_fermi                     ! funtion - Fermi Gas total level densities
   real*8 :: ro_ct                        ! funtion - Constant temperature total level densities
   real*8 :: j_fermi                      ! funtion - Fermi Gas level density spin/parity distribution
   real*8 :: BSQ                          ! funtion - calculates Igor factor to increse 'a' due to increased surface

   if (Cf.NE.0.0D0) then
      BF = 0.0D0        ! normal level densities
   else
      BF = 1.0          ! saddle point (fission)
   endif
   
   A23 = A(Nnuc)**0.666667d0
   ia = INT(A(Nnuc))
   iz = INT(Z(Nnuc))
   in = ia - iz

   call PRERO(Nnuc) ! prepares for lev. dens. calculations

   !
   ! Fit parameters of the level densities the first time they are calculated
   !
   if (Ux == 0.0 .or. Tct == 0.0 ) then
      !-----setting the lowest and highest discrete levels to fit constant temperature formula
      Nl = 3
      El = ELV(Nl,Nnuc)
      Nu = NLV(nnuc)
      Eu = ELV(Nu,Nnuc)

      DEL = 0.0D0
      eo = 0.0D0

      !-----EGSM systematics for Fermi-gas model 
      CALL EGSMsys(ap1,ap2,gamma,del,delp,nnuc)
      IF (BF.EQ.0.0D0 .AND. Asaf.GE.0.0D0) gamma = asaf
      atil = (ap1*A(Nnuc) + ap2*a23)*atilnor(Nnuc)

      !-----calculate crtical values
      CALL DAMIRO_CRT(ia,iz,shc(nnuc),IOUt,0)
      IF (BF.EQ.0.D0 .AND. asaf.LT.0.0D0) acr = acrt

      !
      !  FITTING PARAMETERS IF NOT ALREADY DETERMINED
      ! 
      !  Fermi-gas part of the level denisties
      do i = 1, nde         ! over excitation energies (NOT realLY NEEDED HERE AS WE USE ONLY AT CRITICAL POINT)
         u = i*fde + DEL - ECOnd  
         IF(u.lt.0.d0) CYCLE
         am = atil*FSHELL(u,SHC(Nnuc),GAMma)
         IF (am.LE.0.0D0) RETURN  
         ! next call to SIGMAK only to find cigor    
         call SIGMAK(A(Nnuc),Z(Nnuc),DEF(1,Nnuc),1.0D0,u,am,0.0,mompar,momort,A2,stab,cigor)
         if(cigor > 1.001) then ! skip if the change in atil is negligible
            ! 'a' including surface dependent factor
            atil = AP1*A(Nnuc) + AP2*A23*BSQ(cigor)
            atil = atil*ATIlnor(Nnuc)
            am = atil*FSHELL(u,SHC(Nnuc),gamma)
            call SIGMAK(A(Nnuc),Z(Nnuc),DEF(1,Nnuc),1.0D0,u,am,0.0, mompar,momort,a2,stab,cigor)
         endif
         roFG(i) = ro_fermi(A(Nnuc), u, am, Momort, T, Bf, A2)
         seff2 = mompar**0.333D0*momort**0.6666D0*T
         roFG(i) = roFG(i)/sqrt(2.0*pi*seff2)  ! sqrt() converts state into level densities
      enddo     ! over excitation energies

      ! ! Calculate derivatives of ro_fermi (not needed if Ux related to Ucrit)
      ! DO i = 0, nde-2  
      !    deriv1 = (roFG(i+1) - roFG(i  ))/fde
      !    deriv2 = (roFG(i+2) - roFG(i+1))/fde
      ! ENDDO

      Ux = UCRt - DEL   ! matching is imposed to be at Ucrit (NO(!) smoth transition needed)
      print *, "Matching energy =", Ux
      iux = int(Ux/fde)
      roUx = roFG(iux) + (roFG(iux+1) - roFG(iux))*(Ux/fde - float(iux)) ! interplotation in the roFG table 

      !
      !-----Constant Temperature part of the level densities
      !     Set initail value for a temperture (systematics)
      if (A(Nnuc).LT.100.D0) then 
         Tct = 60./A(Nnuc) + 0.06
      else
         Tct = 0.9 - 0.0024*A(Nnuc)
      endif
      
      !-----calculation of T, Ux and Eo (THIS IS ONLY IF THE PARAM WERE NOT DETRMINED; OTHERWISE GO TO 500)
      !     Eq. 56 of Nucl. Phys. A810(2008)13 and iterate...
      eps = Tct*roUx*exp(-Ux/Tct)*(exp(Eu/Tct)-exp(El/Tct)) + Nl - Nu  
      print *, "initial eps, Tct, Ux, Nl, El,  Nu, Eu =", eps, Tct, Ux,  Nl, El,  Nu, Eu
      i = 0    
      do while (abs(eps) > 0.1) 
         eps = Tct*roUx*exp(-Ux/Tct)*(exp(Eu/Tct)-exp(El/Tct)) + Nl - Nu      
         tct = tct - 0.001*eps   ! THIS MIGHT BE TOO SIMPLE TO WORK!!! 
         i = i + 1
         print *, "iter, Tct, eps", i, Tct, eps
         if (i > 100) then
            WRITE (8,*) 'WARNING: ROGCC'
            WRITE (8,*) 'WARNING: Maximum number of iterations in ROGCC reached for'
            WRITE (8,*) 'WARNING: Z=', INT(Z(Nnuc)), '  A=', INT(A(Nnuc))
            WRITE (8,*) 'WARNING: The last T=', Tct,' will be used for calculations'
            WRITE (8,*) 'WARNING: '
            exit
         endif 
      enddo
      ! Calculate Eo   
      Eo = Ux - Tct*log(Tct*roUx)     !  Eq. 53 of Nucl. Phys. A810(2008)13
      print *, " Ucrt, Tct, Eo =", UCRt, Tct, Eo
      !
      !  FITTING PARAMETERS DONE
      !
      ! ROPar(1,Nnuc) = am
      ROPar(2,Nnuc) = ux
      ROPar(3,Nnuc) = DEL
      ROPar(4,Nnuc) = eo
      ROPar(5,Nnuc) = Tct
   endif

   if (ux.lE.0.0D0) print *, INT(Z(Nnuc)),INT(A(Nnuc)),'Ux=',sngl(ux) 

   !-----calculation of spin cut-off parameter from resolved levels  (TO BE UPDATED)
   sigl = 0.0
   do i = 2, NLV(Nnuc)
      sigl = sigl + (ABS(XJLv(i,Nnuc)) + 0.5)**2
   enddo
   if (NLV(Nnuc).GT.1) sigl = sigl/(NLV(Nnuc) - 1)
   sigl = sigl/2.
   if (sigl.LT.0.5D0) sigl = 0.5

   exl = Ux + DEL
   !-----IF(Scutf.LT.0.0D0) sigh calculated according to Dilg's recommendations
   !-----0.6079 = 6/pi^2   a=6/pi^2*g  sig^2 = <m^2>gt  Scutf = <m^2>
   sigh = Scutf*0.6079*A23*SQRT(ux*am)
   
   !-----determination of the index in Ex-array such that Ex(IG,.).LT.EXL
   !-----(low-energy level density formula is used up to IG)
   ig = NEX(Nnuc)
   do i = 1, NEX(Nnuc)
      if (Ex(i,Nnuc).GT.exl) then
         ig = i - 1
         exit
      endif
   enddo
   !
   !-----calculation of level densities below EXL - (T-constant formula)
   !
   do i = 1, ig     ! do loop over T-constant excitation energies
      e = Ex(i,Nnuc)
      ! CALL PARITY_FRACTION(e,a1,b1,c1,ratio,igs) ! get parity multipliers 'ratio'
      ! print *, 'E, ratios, igs, nnuc', E, ratio, igs, nnuc
      ro_u = ro_ct(e, Tct, eo)
      SIG = (sigh - sigl)*(e - ECUt(Nnuc))/(exl - ECUt(Nnuc)) + sigl ! spin-cutoff interpolated
      do j = 1, NLW
         xj = j + HIS(Nnuc)
         ro_j = j_fermi(xj, sig**2)
         RO(i,j,1,Nnuc) = ro_u*ro_j*ratio(1)
         RO(i,j,2,Nnuc) = ro_u*ro_j*ratio(2)
      enddo
      UEXcit(i,Nnuc) = e
      TNUc(i,Nnuc) = SQRT(e/am)
   enddo      ! Loop over T-constant excitation energies

!
!--------calculation of level densities for energies surpassing EXL (Fermi gas formula)
!
   do i = ig+1, NEX(Nnuc) ! do loop over Fermi gas excitation energies
      u = EX(i,Nnuc) + DEL - ECOnd
      if(u.lt.0.d0) cycle
      am = atil*FSHELL(u,SHC(Nnuc),GAMma)
      IF (am.LE.0.0D0) RETURN  
      ! next call to SIGMAK only to find cigor    
      call SIGMAK(A(Nnuc),Z(Nnuc),DEF(1,Nnuc),1.0D0,u,am,0.0,mompar,momort,A2,stab,cigor)
      if(cigor > 1.001) then ! skip if the change in atil is negligible
         atil = AP1*A(Nnuc) + AP2*A23*BSQ(cigor)
         atil = atil*ATIlnor(Nnuc)
         am = atil*FSHELL(u,SHC(Nnuc),gamma)
         call SIGMAK(A(Nnuc),Z(Nnuc),DEF(1,Nnuc),1.0D0,u,am,0.0, mompar,momort,a2,stab,cigor)
      endif
      ro_u = ro_fermi(A(Nnuc), u, am, Momort, T, Bf, A2)
      seff2 = mompar**0.333D0*momort**0.6666D0*T
      do j = 1, NLW
         xj = j + HIS(Nnuc)
         ro_j = j_fermi(xj, seff2)
         RO(i,j,1,Nnuc) = ro_u*ro_j*ratio(1)
         RO(i,j,2,Nnuc) = ro_u*ro_j*ratio(2)
      enddo
      UEXcit(i,Nnuc) = u
      TNUc(i,Nnuc) = SQRT(u/am)      
   enddo
   if((FITlev.gt.0 .or. IOUt.eq.6) .and. NLV(Nnuc).GT.3 .and. ENDf(Nnuc).LE.1) then
      !       CALL PLOT_GNU_NumCumul(Nnuc,0.d0,0.d0)
      call PLOT_ZVV_NumCumul(Nnuc)
      call PLOT_ZVV_GSLD(Nnuc)     
   endif

   write(8,'(I3,''-'',A2,''-'',I3, 5X,5(2x,1F8.5))') &
            INT(Z(nnuc)), SYMb(nnuc), INT(A(nnuc)), (ROPar(j,nnuc),j=1,5)

   return
end subroutine ROGCC




real*8 function ro_fermi(Anuc, E, Ac, Momort, T, Bf, A2)

      implicit none
!cc   *********************************************************************
!cc   *                                                         class:ppu *
!cc   *                     R O _ F E R M I                               *
!cc   *                                                                   *
!cc   *  Calculates total Fermi-gas state(!) densities using Ignatyuk     *
!cc   *  approach. Collective enhancement effects are taken into          *
!cc   *  account including their energy fade-out.                         *
!cc   *                                                                   *
!cc   *  BF=0. saddle point         (rot. perpend. to symm.)              *
!cc   *********************************************************************
!cc

   real*8, intent(in)  :: Anuc
   real*8, intent(in)  :: E
   real*8, intent(in)  :: Ac
   real*8, intent(in)  :: Momort
   real*8, intent(out) :: T
   real*8, intent(in)  :: Bf
   real*8, intent(in)  :: A2
!
! Local variables
!
   real*8  :: s
   real*8  :: det
   real*8  :: rot_K   
   real*8  :: rot_Q   
   real*8  :: vib_KQ   

   ro_fermi = 0.d0
   IF(Ac.LE.0. .or. e.le.0.d0) RETURN
   T = DSQRT(E/Ac)
   s = 2.* Ac * T
   det = 45.84d0 * Ac**3 * T**5
   ro_fermi = exp(s)/sqrt(det) 
   IF(Bf.EQ.0.d0) RETURN
   CALL COLL_KQ_EGSM(Anuc,T,Momort,A2,e,vib_KQ,rot_K,rot_Q)       
   ro_fermi = ro_fermi * rot_K * rot_Q * vib_KQ 

   return
end function ro_fermi



real*8 function j_fermi(J, seff2)
   
   implicit none
   !cc   *********************************************************************
   !cc   *                                                         class:ppu *
   !cc   *                     j _ f e r m i                                 *
   !cc   *                                                                   *
   !cc   *  Spin distribution for Fermi-gas level densities. Sum of both     *
   !cc   *  parities.                                                        *
   !cc   *                                                                   *
   !cc   *********************************************************************
   
   real*8, parameter  :: pi = 3.1415926535897932D0    ! a number close to 3.14 :)
   real*8, intent(in) :: J                            ! spin (might be signed when parity is included) 
   real*8, intent(in) :: seff2                        ! mompar**0.333D0*momort**0.6666D0*t (equivalent to \sigma^2)
   ! real*8, intent(in) :: parityRatio                  ! ratio of + (or -) parity to total level densities

      j_fermi = (1.d0/(2.d0*sqrt(2.d0*pi)))*(2.d0*J + 1.d0)/seff2 **1.5*EXP(-(J+0.5)**2/(2.d0*seff2))
   
   return
end function j_fermi

real*8 function ro_ct(E, Tct, eo)
   
   implicit none
   !cc   *********************************************************************
   !cc   *                                                         class:ppu *
   !cc   *                     r o _ c t                                     *
   !cc   *                                                                   *
   !cc   *  Total level densities unsing constant temperature  model.        *
   !cc   *                                                                   *
   !cc   *                                                                   *
   !cc   *********************************************************************
   
   real*8, intent(in) :: E                ! excitation energy
   real*8 :: Tct                          ! temerature in constant temperature model
   real*8 :: eo                           ! Eo in constant temperature model

   ro_ct = exp((E-eo)/Tct)/Tct

   return
end function ro_ct





