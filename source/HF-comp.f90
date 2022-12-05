!cc   * $Rev: 5404 $
!cc   * $Author: mwherman $
!cc   * $Date: 2022-12-05 03:21:45 +0100 (Mo, 05 Dez 2022) $

!cc   ********************************************************************
!cc   *                                                                  *
!cc   * These routines are called after HF decay of each J-pi state      *
!cc   * at each continuum energy. This differs HF-comp routines from     *
!cc   * from those in the HF-decay module that are targeting whole       *
!cc   * nuclei, rather than individual cells in the continuum.           *
!cc   * Thus HF-decay comes on the level above HF-comp.                  *
!cc   *                                                                  *
!cc   ********************************************************************

subroutine ACCUM(Iec , Nnuc , Nnur , Nejc , Xnor)
   !cc
   !cc   ********************************************************************
   !cc   *                                                         class:mpu*
   !cc   *                         A C C U M                                *
   !cc   *                                                                  *
   !cc   * Normalizes scratch arrays SCRT and SCRTL with the population     *
   !cc   * divided by the H-F denominator and accumulates the result on the *
   !cc   * population array POP for a given nucleus NNUR                    *
   !cc   *                                                                  *
   !cc   * input:Iec  - energy index of the decaying state                  *
   !cc   *       Nnuc - index of the decaying nucleus                       *
   !cc   *       Nnur - index of the residual nucleus                       *
   !cc   *       Nejc - index of the ejectile (0 for gamma)                 *
   !cc   *       Xnor - normalization factor (POP*STEP/DENHF)               *
   !cc   *                                                                  *
   !cc   *                                                                  *
   !cc   * output:none                                                      *
   !cc   *                                                                  *
   !cc   *                                                                  *
   !cc   *                                                                  *
   !cc   * calls:EXCLUSIVEC                                                 *
   !cc   *       EXCLUSIVEL                                                 *
   !cc   *                                                                  *
   !cc   *                                                                  *
   !cc   *                                                                  *
   !cc   ********************************************************************
   !cc

   implicit none
   include 'dimension.h'
   include 'global.h'

   integer :: Iec , Nejc , Nnuc , Nnur
   real*8 :: Xnor
   intent (in) Xnor

   real*8 :: eemi , excnq , pop1 , pop2 , poph , popl , popll , pops , popt , xcse
   real :: FLOAT
   integer :: icse , icsh , icsl , ie , il , j , nexrt , nspec
   integer :: INT , MAX0
   logical :: primaryGamma = .false.


   if(Xnor<=0)return  !, nth
   
   if(Nnuc==Nnur)then
      excnq = EX(Iec , Nnuc)
   else
      excnq = EX(Iec , Nnuc) - Q(Nejc , Nnuc)
   endif
   nexrt = INT((excnq - ECUt(Nnur))/DE + 1.0001)
   !  Extending continuum for capture over the discrete levels, RCN 112019
   if(Nejc==0) nexrt = INT(excnq/DE + 1.0001)  ! LIKELY TO BE REMOVED!
   
   !-----
   !-----Continuum to continuum
   !-----
   if(nexrt>0)then
      do ie = 1 , nexrt !loop over residual energies (continuum)
         popt = 0.D0
         do j = 1 , NLW !loop over residual spins
            pop1 = Xnor*SCRt(ie , j , 1 , Nejc)
            pop2 = Xnor*SCRt(ie , j , 2 , Nejc)
            pops = pop1 + pop2
            if(pops<=0)cycle
            if(ie==1)pops = pops*0.5
            popt = popt + pops !sum over spin/pi at a given energy bin
            POP(ie , j , 1 , Nnur) = POP(ie , j , 1 , Nnur) + pop1
            POP(ie , j , 2 , Nnur) = POP(ie , j , 2 , Nnur) + pop2
            if(Nejc/=0 .and. POPmax(Nnur)<POP(ie , j , 1 , Nnur))POPmax(Nnur) = POP(ie , j , 1 , Nnur)
            if(Nejc/=0 .and. POPmax(Nnur)<POP(ie , j , 2 , Nnur))POPmax(Nnur) = POP(ie , j , 2 , Nnur)
         enddo !over residual spins
         if(popt>0)then
            icse = min(INT((excnq-EX(ie,Nnur))/DE + 1.0001) , NDECSE)
            !  icse = MAX0(2,icse)
            icse = MAX0(1 , icse)
            AUSpec(icse , Nejc) = AUSpec(icse , Nejc) + popt
            CSE(icse , Nejc , Nnuc) = CSE(icse , Nejc , Nnuc) + popt
            CSEt(icse , Nejc) = CSEt(icse , Nejc) + popt
            if(ENDf(Nnuc)==1)then
               call EXCLUSIVEC(Iec , ie , Nejc , Nnuc , Nnur , excnq , popt) !exclusive spectra stored
            else
               CSE(icse , Nejc , 0) = CSE(icse , Nejc , 0) + popt !inclusive spectrum stored on Nnuc=0
            endif
         endif
      enddo  !over residual energies in continuum
   endif
   !-----
   !-----Continuum to discrete levels
   !-----
   nspec = min(INT(EMAx(Nnur)/DE) + 1 , NDECSE - 1)  ! Spectrum length
   do il = 1 , NLV(Nnur)
      if(Nnuc==1 .and. Nejc==0 .and. Iec==NEX(1)) primaryGamma = .true.
      eemi = excnq - ELV(il , Nnur)
      if(eemi<0)return
      pop1 = Xnor*SCRtl(il , Nejc)
      if(pop1>0)then
         POPlv(il , Nnur) = POPlv(il , Nnur) + pop1   ! Add contribution to discrete level population
         REClev(il , Nejc) = REClev(il , Nejc) + pop1 ! Add contribution to recoils auxiliary matrix for discrete levels

         ! Add contribution of discrete levels to emission spectra
         ! Transitions to discrete levels are distributed
         ! between the nearest spectrum bins (inversely proportional to the
         ! distance of the actual energy to the bin energy
         ! Eliminate transitions from the top bin in the 1-st CN (except gammas)

         if(Nnuc/=1 .or. ENDf(Nnuc)/=1 .or. Iec/=NEX(1) .or. Nejc==0)then
            xcse = eemi/DE + 1.0001
            icsl = min(INT(xcse) , NDECSE - 1)        ! Lower energy bin for the gamma transition
            icsh = icsl + 1                           ! Upper energy bin for the gamma transition
            if(NPRim_g>0 .and. primaryGamma)then      ! Primary gamma to be considered
               ENPg(il) = eemi
               CSEpg(il) = CSEpg(il) + pop1
            else                                      ! Typical path (not a primary gamma)
               if(icsl<nspec)then
                  popl = pop1*(FLOAT(icsh) - xcse)/DE
                  poph = pop1*(xcse - FLOAT(icsl))/DE
               else
                  popl = pop1/DE
                  poph = 0.0D0
               endif
               popll = popl                           !we also need popl not multiplied by 2
               if(icsl==1)popl = 2.0*popl
               !
               !  Addition of continuum to discrete gamma to spectra
               !
               if(popll>0)then
                  CSE(icsl , Nejc , Nnuc) = CSE(icsl , Nejc , Nnuc) + popl
                  CSEt(icsl , Nejc) = CSEt(icsl , Nejc) + popl
                  if(ENDf(Nnuc)==1)then
                     CALL EXCLUSIVEL(Iec,icsl,Nejc,Nnuc,Nnur,il,popll)
                  else
                     CSE(icsl , Nejc , 0) = CSE(icsl , Nejc , 0) + popll
                  endif
               endif
   
               if(poph>0)then
                  CSE(icsh , Nejc , Nnuc) = CSE(icsh , Nejc , Nnuc) + poph
                  CSEt(icsh , Nejc) = CSEt(icsh , Nejc) + poph
                  if(ENDf(Nnuc)==1)then
                     CALL EXCLUSIVEL(Iec,icsh,Nejc,Nnuc,Nnur,il,poph)
                  else
                     CSE(icsh , Nejc , 0) = CSE(icsh , Nejc , 0) + poph
                  endif
               endif
            endif  ! over primary gamma or not
         endif
         
         !
         ! Add CN contribution to direct ang. distributions
         !
         if(Nnuc==1 .and. Iec==NEX(1) .and. Nejc/=0)then
            CSDirlev(il , Nejc) = CSDirlev(il , Nejc) + pop1
            !  Compound elastic and inelastic stored for final renormalization
            CSComplev(il , Nejc) = CSComplev(il , Nejc) + pop1
         endif   ! on top CN1 state, particles only
      endif    ! if pop1>0
   enddo      !loop over levels
   primaryGamma = .false.
   return
end subroutine ACCUM



subroutine EXCLUSIVEF(Iec , Nnuc , Popt)
   !cc
   !cc   ********************************************************************
   !cc   *                                                         class:mpu*
   !cc   *                   E X C L U S I V E F                            *
   !cc   *                                                                  *
   !cc   *      DOES NOTHING, CURRENTLY NOT BEING USED !!!                  *
   !cc   *                                                                  *
   !cc   * Deconvolutes inclusive spectra calculated by the statistical     *
   !cc   * model into spectra for individual reactions (exclusive) as       *
   !cc   * requested by the ENDF format. EXCLUSIVEF is for transitions      *
   !cc   * to continuum.                                                    *
   !cc   *                                                                  *
   !cc   * input:Iec  - energy index of the decaying state                  *
   !cc   *       Nnuc - index of the decaying nucleus                       *
   !cc   *       Popt - x-sec/MeV for the transition from the initial       *
   !cc   *              (Iec,Jcn,Ipar) cell to the final bin at energy Ief  *
   !cc   *              This cross section is directly added to the spectrum*
   !cc   *              and Popt*DE is used to define a portion of the      *
   !cc   *              feeding spectrum that has to be moved.              *
   !cc   * output:none                                                      *
   !cc   *                                                                  *
   !cc   * calls:none                                                       *
   !cc   *                                                                  *
   !cc   ********************************************************************
   !cc
   implicit none
   include 'dimension.h'
   include 'global.h'

   integer :: Iec , Nnuc
   real*8 :: Popt
   intent (in) Iec , Nnuc , Popt

   integer :: ie , iejc
   real*8 :: xnor


   !  POPcse(Ief,Nejc,icsp,INExc(Nnuc))  - spectrum for the population of the
   !                                energy bin with index Ief in Nnuc by
   !                                Nejc particles (cumulative over all
   !                                decays leading to this energy bin)
   !-----
   !-----Fission
   !-----
   !-----Contribution due to feeding spectra from Nnuc (no decay contribution)
   !-----DE spectra (DDX are not done for fission although they could be)
   !
   !  fission of n,nx and npx nuclei considered
   !
   !  Should be used to calculate pre-fission spectra. Not used for the time being
   return

   if(POPbin(Iec , Nnuc)<=0)return
   xnor = Popt*DE/POPbin(Iec , Nnuc)
   do ie = 1 , NDECSE
      do iejc = 0 , NDEJC
         if(POPcse(Iec , iejc , ie , INExc(Nnuc))>0)CSEfis(ie , iejc , Nnuc) = CSEfis(ie , iejc , Nnuc)                          &
          & + POPcse(Iec , iejc , ie , INExc(Nnuc))*xnor
      enddo
   enddo
   return
end subroutine EXCLUSIVEF


subroutine EXCLUSIVEC(Iec , Ief , Nejc , Nnuc , Nnur , Excnq , Popt)
   !cc
   !cc   ********************************************************************
   !cc   *                                                         class:mpu*
   !cc   *                   E X C L U S I V E C                            *
   !cc   *                                                                  *
   !cc   * Deconvolutes inclusive spectra calculated by the statistical     *
   !cc   * model into spectra for individual reactions (exclusive) as       *
   !cc   * requested by the ENDF format. EXCLUSIVEC is for transitions      *
   !cc   * to continuum.                                                    *
   !cc   *                                                                  *
   !cc   *                                                                  *
   !cc   * input:Iec  - energy index of the decaying state                  *
   !cc   *       Ief  - energy index of the final state                     *
   !cc   *       Nejc - index of the ejectile                               *
   !cc   *       Nnuc - index of the decaying nucleus                       *
   !cc   *       Nnur - index of the final nucleus                          *
   !cc   *       Popt - x-sec/MeV for the transition from the initial       *
   !cc   *              (Iec,Jcn,Ipar) cell to the final bin at energy Ief  *
   !cc   *              This cross section is directly added to the spectrum*
   !cc   *              and Popt*DE is used to define a portion of the      *
   !cc   *              feeding spectrum that has to be moved.              *
   !cc   *                                                                  *
   !cc   * output:none                                                      *
   !cc   *                                                                  *
   !cc   *                                                                  *
   !cc   *                                                                  *
   !cc   * calls:none                                                       *
   !cc   *                                                                  *
   !cc   *                                                                  *
   !cc   *                                                                  *
   !cc   ********************************************************************
   !cc
   use empcess , ONLY:POPcsea
   implicit none
   include 'dimension.h'
   include 'global.h'

   real*8 :: Excnq , Popt
   integer :: Iec , Ief , Nejc , Nnuc , Nnur
   intent (in) Excnq , Iec , Nejc , Nnuc , Nnur , Popt

   integer :: icsp , ie , iejc , nth
   integer :: INT
   real*8 :: xnor

   !  data jsigma/0/,jsigma2/36/
   !  POPcse(Ief,Nejc,icsp,INExc(Nnuc))  - spectrum for the population of the energy bin with index Ief in Nnuc by
   !                                       Nejc particles (cumulative over all decays leading to this energy bin)

   !-----
   !-----Particle decay
   !
   !-----Contribution coming straight from the current decay
   icsp = INT((Excnq - EX(Ief,Nnur))/DE + 1.0001)
   if(ENDf(Nnur)==1)then
      POPcse(Ief , Nejc , icsp , INExc(Nnur)) = POPcse(Ief , Nejc , icsp , INExc(Nnur)) + Popt ! adding Popt to population spectra in exclusive residue
   else
      CSE(icsp , Nejc , 0) = CSE(icsp , Nejc , 0) + Popt ! inclusive residue - add Popt directly to spectra
   endif
   !-----
   !-----Contribution due to feeding spectra from Nnuc
   !-----
   if(POPbin(Iec , Nnuc)==0)return
   !-----DE spectra
   if(Nnuc/=1 .or. Nejc==0)then          !skip the first CN except gammas !REVERSE LOGIC END MAKE RETURN!
      xnor = Popt*DE/POPbin(Iec , Nnuc)

      !  DO ie = 1, NDEcse
      do ie = 1 , NEX(Nnuc)
         do iejc = 0 , NDEJC
            if(POPcse(Iec , iejc , ie , INExc(Nnuc))>0)then
               if(ENDf(Nnur)==2)then
                  CSE(ie , iejc , 0) = CSE(ie , iejc , 0) + POPcse(Iec , iejc , ie , INExc(Nnuc))*xnor ! DE feeding contribution added to inclusive spectrum
               else
                  POPcse(Ief , iejc , ie , INExc(Nnur)) = POPcse(Ief , iejc , ie , INExc(Nnur))                                  &
                     & + POPcse(Iec , iejc , ie , INExc(Nnuc))*xnor  ! DE feeding contribution added to population spectrum
               endif
            endif

            !-----DDX spectra for HMS
            if(LHMs/=0 .and. iejc>0 .and. iejc<3)then

               ! POPcsed seems to be equivalent to POPcse but for HMS or MSD rather than for CN
               if(POPcsed(Iec , iejc , ie , INExc(Nnuc))>0)then
                  if(ENDf(Nnur)==1)then
                     POPcsed(Ief , iejc , ie , INExc(Nnur)) = POPcsed(Ief , iejc , ie , INExc(Nnur))                             &
                        & + POPcsed(Iec , iejc , ie , INExc(Nnuc))*xnor
                                                               !DE feeding HMS/MSD contribution added to population spectrum
                  else
                     POPcsed(Ief , iejc , ie , 0) = POPcsed(Ief , iejc , ie , 0) + POPcsed(Iec , iejc , ie , INExc(Nnuc))*xnor
                                                               !DE feeding HMS/MSD contribution added to inclusive spectrum POPcsed
                  endif
               endif

               if(POPcsed(Iec , iejc , ie , INExc(Nnuc))>0)then
                  if(ENDf(Nnur)==1)then
                     do nth = 1 , NDAng
                        POPcsea(nth , Ief , iejc , ie , INExc(Nnur)) = POPcsea(nth , Ief , iejc , ie , INExc(Nnur))              &
                           & + POPcsea(nth , Iec , iejc , ie , INExc(Nnuc))*xnor
                     enddo
                  else
                     do nth = 1 , NDAng
                        POPcsea(nth , Ief , iejc , ie , 0) = POPcsea(nth , Ief , iejc , ie , 0)                                  &
                           & + POPcsea(nth , Iec , iejc , ie , INExc(Nnuc))*xnor
                     enddo
                  endif
               endif
            !-----DDX spectra using portions
            elseif(POPcseaf(Iec , iejc , ie , INExc(Nnuc))>0)then
               if(ENDf(Nnur)==1)then
                  POPcseaf(Ief , iejc , ie , INExc(Nnur)) = POPcseaf(Ief , iejc , ie , INExc(Nnur))                              &
                     & + POPcseaf(Iec , iejc , ie , INExc(Nnuc))*xnor
               else
                  POPcseaf(0 , iejc , ie , 0) = POPcseaf(0 , iejc , ie , 0) + POPcseaf(Iec , iejc , ie , INExc(Nnuc))*xnor
               endif
            endif
         enddo
      enddo
   endif
end subroutine EXCLUSIVEC



SUBROUTINE EXCLUSIVEL(Iec,Ie,Nejc,Nnuc,Nnur,Il,Popt) 
   !cc
   !cc   ********************************************************************
   !cc   *                                                         class:mpu*
   !cc   *                   E X C L U S I V E L                            *
   !cc   *                                                                  *
   !cc   * Deconvolutes inclusive spectra calculated by the statistical     *
   !cc   * model into spectra for individual reactions (exclusive) as       *
   !cc   * requested by the ENDF format. EXCLUSIVEL is for transitions to   *
   !cc   * discrete levels (will store population spectra on POPcse(0,.,.,.)*
   !cc   *                                                                  *
   !cc   *                                                                  *
   !cc   * input:Iec  - energy index of the decaying state                  *
   !cc   *       Ie  -  index of the spectrum bin                           *
   !cc   *       Nejc - index of the ejectile                               *
   !cc   *       Nnuc - index of the decaying nucleus                       *
   !cc   *       Nnur - index of the final nucleus                          *
   !cc   *       Il   - index of the populated discrete level               *
   !cc   *       Popt - x-sec/MeV for the transition from the initial       *
   !cc   *              (Iec,Jcn,Ipar) cell to the final bin at energy Ief  *
   !cc   *              This cross section is directly added to the spectrum*
   !cc   *              and Popt*DE is used to define a portion of the      *
   !cc   *              feeding spectrum that has to be moved.              *
   !cc   *                                                                  *
   !cc   * output:none                                                      *
   !cc   *                                                                  *
   !cc   *                                                                  *
   !cc   *                                                                  *
   !cc   * calls:none                                                       *
   !cc   *                                                                  *
   !cc   *                                                                  *
   !cc   *                                                                  *
   !cc   ********************************************************************
   !cc
   use empcess , ONLY:POPcsea

   implicit none
   include 'dimension.h'
   include 'global.h'

   integer :: Ie , Iec , Nejc , Nnuc , Nnur, Il
   real*8 :: Popt
   intent (in) Ie , Iec , Nejc , Nnuc , Nnur , Il, Popt

   integer :: iejc , iesp , nth
   real*8 :: xnor

   !  POPcse(Ief,Nejc,icsp,INExc(Nnuc))  - spectrum for the population of the
   !                                energy bin with index Ief in Nnuc by
   !                                Nejc particles (cumulative over all
   !                                decays leading to this energy bin)
   !
   !-----Contribution coming straight from the current decay
   !-----(ignore if residue is inclusive since summation already done in ACCUM)
   if(ENDf(Nnur)==1)then
      POPcse(0 , Nejc , Ie , INExc(Nnur)) = POPcse(0 , Nejc , Ie , INExc(Nnur)) + Popt
   else
      CSE(Ie , Nejc , 0) = CSE(Ie , Nejc , 0) + Popt
   endif
   !-----Contribution due to feeding spectra from Nnuc
   if(POPbin(Iec , Nnuc)<=0)return
   !-----DE spectra
   if(Nnur/=1 .or. Nejc==0)then          !skip the first CN except gammas
      xnor = Popt*DE/POPbin(Iec , Nnuc)
      !  DO iesp = 1, NDECSE
      do iesp = 1 , NEX(Nnuc)
         do iejc = 0 , NDEJC
            if(POPcse(Iec , iejc , iesp , INExc(Nnuc))>0)then
               if(ENDf(Nnur)==1)then
                  POPcse(0 , iejc , iesp , INExc(Nnur)) = POPcse(0 , iejc , iesp , INExc(Nnur))                                  &
                     & + POPcse(Iec , iejc , iesp , INExc(Nnuc))*xnor
               else
                  CSE(iesp , iejc , 0) = CSE(iesp , iejc , 0) + POPcse(Iec , iejc , iesp , INExc(Nnuc))*xnor
                  !---------------------Store also population spectra for discrete levels
                  !
                  !  ### Index Il as a dummy parameter of the routine. It is needed to uncomment this part
                  !      SUBROUTINE EXCLUSIVEL(Iec,Ie,Nejc,Nnuc,Nnur,Il,Popt)
                  !
                  POPcselv(Il,iejc,iesp,INExc(Nnur)) = POPcselv(Il,iejc,iesp,INExc(Nnur)) &
                     + POPcse(Iec,iejc,iesp,INExc(Nnuc))*xnor
               endif
            endif

            if(LHMs/=0 .and. iejc>0 .and. iejc<3)then             !HMS n & p only

               if(POPcsed(Iec , iejc , iesp , INExc(Nnuc))>0)then
                  if(ENDf(Nnur)==1)then
                     POPcsed(0 , iejc , iesp , INExc(Nnur)) = POPcsed(0 , iejc , iesp , INExc(Nnur))                             &
                      & + POPcsed(Iec , iejc , iesp , INExc(Nnuc))*xnor
                  else
                     POPcsed(0 , iejc , iesp , 0) = POPcsed(0 , iejc , iesp , 0) + POPcsed(Iec , iejc , iesp , INExc(Nnuc))*xnor
                  endif
               endif

               if(POPcsed(Iec , iejc , iesp , INExc(Nnuc))>0)then
                  if(ENDf(Nnur)==1)then
                     do nth = 1 , NDAng
                        POPcsea(nth , 0 , iejc , iesp , INExc(Nnur)) = POPcsea(nth , 0 , iejc , iesp , INExc(Nnur))              &
                         & + POPcsea(nth , Iec , iejc , iesp , INExc(Nnuc))*xnor
                     enddo
                  else
                     do nth = 1 , NDAng
                        POPcsea(nth , 0 , iejc , iesp , 0) = POPcsea(nth , 0 , iejc , iesp , 0) + POPcsea(nth , Iec , iejc , iesp,&
                         & INExc(Nnuc))*xnor
                     enddo
                  endif
               endif
            !-----------DDX spectra using portions
            elseif(POPcseaf(Iec , iejc , iesp , INExc(Nnuc))>0)then
               if(ENDf(Nnur)==1)then
                  POPcseaf(0 , iejc , iesp , INExc(Nnur)) = POPcseaf(0 , iejc , iesp , INExc(Nnur))                              &
                   & + POPcseaf(Iec , iejc , iesp , INExc(Nnuc))*xnor
               else
                  POPcseaf(0 , iejc , iesp , 0) = POPcseaf(0 , iejc , iesp , 0) + POPcseaf(Iec , iejc , iesp , INExc(Nnuc))*xnor
               endif
            endif
         enddo
      enddo
   endif
end subroutine EXCLUSIVEL


subroutine DECAY(Nnuc , Iec , Jc , Ipc , Nnur , Nejc , Sum)
   !cc
   !cc   ********************************************************************
   !cc   *                                                         class:PPu*
   !cc   *                         D E C A Y                                *
   !cc   *                (function to function version)                    *
   !cc   *                                                                  *
   !cc   * Calculates decay of a continuum state in nucleus NNUC into       *
   !cc   * continuum and discrete states of the residual nucleus NNUR       *
   !cc   * through the emission of the ejectile NEJC.                       *
   !cc   *                                                                  *
   !cc   *                                                                  *
   !cc   * input:NNUC - decaying nucleus index                              *
   !cc   *       IEC  - energy index of the decaying state                  *
   !cc   *       JC   - spin index of the decaying state                    *
   !cc   *       IPC  - parity of the decaying state                        *
   !cc   *       NNUR - residual nucleus index                              *
   !cc   *       NEJC - ejectile index                                      *
   !cc   *                                                                  *
   !cc   *                                                                  *
   !cc   * output:SUM - SUM of transmission coefficients over all outgoing  *
   !cc   *              channels for the requested decay (partial sums are  *
   !cc   *              stored in SCRT and SCRTL arrays for continuum and   *
   !cc   *              discrete levels respectively. SUMs for all ejectiles*
   !cc   *              combine to the total Hauser-Feshbach denominator.   *
   !cc   *              Inverse of the latter multiplied by the population  *
   !cc   *              of the (NNUC,IEC,JC,IPC) state is used to normalize *
   !cc   *              SCRT and SCRTL matrices to give residual nucleus    *
   !cc   *              population.                                         *
   !cc   *                                                                  *
   !cc   * calls:TLLOC                                                      *
   !cc   *                                                                  *
   !cc   *                                                                  *
   !cc   *                                                                  *
   !cc   ********************************************************************
   !cc

   implicit none
   include 'dimension.h'
   include 'global.h'

   real*8 , dimension(NDLW) :: ELTl
   real*8 , dimension(NDLW , 3) :: ELTlj
   common /ELASTIC/ ELTl , ELTlj

   integer :: Iec , Ipc , Jc , Nejc , Nnuc , Nnur
   real*8 :: Sum
   intent (in) Iec , Ipc , Jc , Nnuc
   intent (inout) Sum

   real*8 :: corr , eout , eoutc , frde , hisr , s , smax , smin , sumdl , sumin , sumtl1 , sumtl2 , sumtt , xjc , xjr
   real :: FLOAT
   integer :: i , ichsp , ier , iermax , ietl , iexc , il , ip1 , ip2 , ipar , itlc , j , jr , l , lmax , lmaxf , lmin , mul
   integer :: INT , MIN0 , NINT

   Sum = 0.D0
   hisr = HIS(Nnur)
   xjc = FLOAT(Jc) + HIS(Nnuc)
   iexc = NEX(Nnuc) - NEXr(Nejc , Nnuc)
   itlc = iexc - 5
   iermax = Iec - iexc
   !-----clear scratch matrix
   SCRtem(Nejc) = 0.D0

   if(iermax>=1)then
      !-------clear scratch matrices
      SCRt(: , 1:NLW , 1 , Nejc) = 0.D0
      SCRt(: , 1:NLW , 2 , Nejc) = 0.D0
      !  DO j = 1, NDLW    ! NLW
      !    DO i = 1, NDEX ! NEX(Nnur) + 1
      !      SCRt(i,j,1,Nejc) = 0.d0
      !      SCRt(i,j,2,Nejc) = 0.d0
      !    ENDDO
      !  ENDDO
      !
      !-----
      !-----decay to the continuum
      !-----
      do jr = 1 , NLW              ! do loop over r.n. spins
         xjr = FLOAT(jr) + hisr
         smin = ABS(xjr - SEJc(Nejc))
         smax = xjr + SEJc(Nejc)
         mul = INT(smax - smin + 1.0001)
         !  write(8,*) 'mul', mul
         do ichsp = 1 , mul                ! do loop over channel spin
            s = smin + FLOAT(ichsp - 1)
            lmin = INT(ABS(xjc - s) + 1.01)
            lmaxf = INT(xjc + s + 1.01)
            lmaxf = MIN0(NLW , lmaxf)
            !  write(8,*) 'lmaxf, xjc, s ',lmaxf, xjc, s
            ipar = (1 + Ipc*( - 1)**(lmin - 1))/2
            !--------------parity index of r.n. state populated by emission with LMIN
            ip1 = 2 - ipar
            !--------------parity index of r.n. state populated by emission with LMIN+1
            ip2 = 1
            if(ip1==1)ip2 = 2
            !
            !--------------decay to the highest possible bin (non neutron only)
            !
            if(NINT(ZEJc(Nejc))/=0)then
               !  lmax = MIN0(LMAxtl(6,Nejc,Nnur),lmaxf)
               lmax = MIN0(LMAxtl(5 , Nejc , Nnur) , lmaxf)
               !-----------------do loop over l (odd and even l-values treated separately)
               !-----------------IP1 and IP2 decide to which parity each SUMTL  goes
               sumtl1 = 0.D0
               do l = lmin , lmax , 2
                  sumtl1 = sumtl1 + TL(5 , l , Nejc , Nnur)
               enddo
               sumtl2 = 0.D0
               do l = lmin + 1 , lmax , 2
                  sumtl2 = sumtl2 + TL(5 , l , Nejc , Nnur)
               enddo
               !-----------------do loop over l   ***done***
               SCRt(iermax , jr , ip1 , Nejc) = SCRt(iermax , jr , ip1 , Nejc) + sumtl1*RO(iermax , jr , ip1 , Nnur)             &
                & *TUNe(Nejc , Nnuc)
               SCRt(iermax , jr , ip2 , Nejc) = SCRt(iermax , jr , ip2 , Nejc) + sumtl2*RO(iermax , jr , ip2 , Nnur)             &
                & *TUNe(Nejc , Nnuc)
               if(iermax==1 .and. NINT(Z(1))==NINT(Z(Nnur)))then
                  SCRt(iermax , jr , ip1 , Nejc) = SCRt(iermax , jr , ip1 , Nejc)*DEPart(Nnur)
                  SCRt(iermax , jr , ip2 , Nejc) = SCRt(iermax , jr , ip2 , Nejc)*DEPart(Nnur)
               endif
            endif
            !
            !--------------decay to the highest but one bin (conditional see the next IF)
            !
            if(NINT(ZEJc(Nejc))==0 .and. Iec==NEX(Nnuc) - 1)then
               lmax = MIN0(LMAxtl(6 , Nejc , Nnur) , lmaxf)
               !  write(8,*) 'lmaxf top bin, xjc, s ',lmaxf, xjc, s
               !-----------------do loop over l (odd and even l-values treated separately)
               !-----------------IP1 and IP2 decide which parity each SUMTL  goes to
               sumtl1 = 0.D0
               do l = lmin , lmax , 2
                  sumtl1 = sumtl1 + TL(6 , l , Nejc , Nnur)
               enddo
               sumtl2 = 0.D0
               do l = lmin + 1 , lmax , 2
                  sumtl2 = sumtl2 + TL(6 , l , Nejc , Nnur)
               enddo
               !-----------------do loop over l   ***done***
               !
               !-----------------corr in the next lines accounts for the Tl interpolation
               !-----------------and integration over overlaping bins (2/3), it turned out it must
               !-----------------be energy step and also emission step dependent
               corr = 0.4444D0/(DE - XN(Nnur) + XN(1))
               SCRt(iermax , jr , ip1 , Nejc) = SCRt(iermax , jr , ip1 , Nejc) + sumtl1*RO(iermax , jr , ip1 , Nnur)             &
                & *corr*TUNe(Nejc , Nnuc)
               SCRt(iermax , jr , ip2 , Nejc) = SCRt(iermax , jr , ip2 , Nejc) + sumtl2*RO(iermax , jr , ip2 , Nnur)             &
                & *corr*TUNe(Nejc , Nnuc)
               if(iermax==1 .and. NINT(Z(1))==NINT(Z(Nnur)))then
                  SCRt(iermax , jr , ip1 , Nejc) = SCRt(iermax , jr , ip1 , Nejc)*DEPart(Nnur)
                  SCRt(iermax , jr , ip2 , Nejc) = SCRt(iermax , jr , ip2 , Nejc)*DEPart(Nnur)
               endif
               !  write(8,*) 'Last but one bin', iermax
               !  write(8,*) 'jr, corr, sumtl1,2', jr, corr, sumtl1,
               !  &            sumtl2
               !                write(8,*) 'SCRt top',SCRt(iermax,jr,ip1,Nejc),
               !  &              SCRt(iermax,jr,ip2,Nejc)
            endif
            !
            !--------------do loop over r.n. energies (highest bin and eventually the second
            !--------------bin from the top excluded as already done)
            !
            !  write(8,*) 'Remaining bins'
            do ier = iermax - 1 , 1 , -1
               ietl = Iec - ier - itlc
               lmax = MIN0(LMAxtl(ietl , Nejc , Nnur) , lmaxf)
               !  write(8,*) 'lmin, lmax', lmin, lmax
               !-----------------do loop over l (odd and even l-values treated separately)
               !-----------------IP1 and IP2 decide which parity each SUMTL  goes to
               sumtl1 = 0.D0
               do l = lmin , lmax , 2
                  sumtl1 = sumtl1 + TL(ietl , l , Nejc , Nnur)
               enddo
               sumtl2 = 0.D0
               do l = lmin + 1 , lmax , 2
                  sumtl2 = sumtl2 + TL(ietl , l , Nejc , Nnur)
               enddo !-----------------do loop over l   ***done***

               SCRt(ier , jr , ip1 , Nejc) = SCRt(ier , jr , ip1 , Nejc) + sumtl1*RO(ier , jr , ip1 , Nnur)*TUNe(Nejc , Nnuc) !*corr
               SCRt(ier , jr , ip2 , Nejc) = SCRt(ier , jr , ip2 , Nejc) + sumtl2*RO(ier , jr , ip2 , Nnur)*TUNe(Nejc , Nnuc) !*corr
               if(ier==1 .and. NINT(Z(1))==NINT(Z(Nnur)))then
                  SCRt(ier , jr , ip1 , Nejc) = SCRt(ier , jr , ip1 , Nejc)*DEPart(Nnur)
                  SCRt(ier , jr , ip2 , Nejc) = SCRt(ier , jr , ip2 , Nejc)*DEPart(Nnur)
               endif
            enddo !--------------do loop over r.n. energies ***done***
         enddo !-----------do loop over channel spins ***done***
      enddo !--------do loop over and r.n. spins ***done***
      !--------decay to the continuum  ***done***

      !--------trapezoidal integration of ro*tl in continuum for ejectile nejc
      do j = 1 , NLW
         do i = 1 , iermax
            Sum = Sum + SCRt(i , j , 1 , Nejc) + SCRt(i , j , 2 , Nejc)
         enddo
         Sum = Sum - 0.5*(SCRt(1 , j , 1 , Nejc) + SCRt(1 , j , 2 , Nejc))
      enddo
      Sum = Sum*DE
      !  write(8,*)'sum to continuum for ejectile ', Nejc, Sum
      !--------integration of ro*tl in continuum for ejectile nejc -- done ----
   endif
   !-----
   !-----decay to discrete levels
   !-----
   SCRtl(1:NDLV , Nejc) = 0.D0

   eoutc = EX(Iec , Nnuc) - Q(Nejc , Nnuc)

   sumin = 0.D0
   sumtt = 0.D0
   
   do i = 1 , NLV(Nnur) !-----do loop over inelastic levels. Elastic channels excluded, done after the loop
      if(IZA(Nnur)==IZA(0) .and. i==LEVtarg)cycle

      eout = eoutc - ELV(i , Nnur)
      sumdl = 0.D0
      if(eout<0.0D0)exit

      call TLLOC(Nnur , Nejc , eout , il , frde)
      smin = ABS(XJLv(i , Nnur) - SEJc(Nejc))
      smax = XJLv(i , Nnur) + SEJc(Nejc) + 0.01
      s = smin
      do while (.true.) !--------loop over channel spin 
         lmin = INT(ABS(xjc - s) + 1.01)
         lmax = INT(xjc + s + 1.01)
         lmax = MIN0(NLW , lmax)
         do l = lmin , lmax  !--------do loop over l
            ipar = 1 + LVP(i , Nnur)*Ipc*( - 1)**(l - 1)
            if(ipar/=0)sumdl = sumdl + TL(il , l , Nejc , Nnur) + frde*(TL(il + 1 , l , Nejc , Nnur) - TL(il , l , Nejc , Nnur))
         enddo  
         s = s + 1.
         if(s<=smax)then  !HERE
         !--------loop over channel spin ------ done ----------------------------
         else
            sumdl = sumdl*TUNe(Nejc , Nnuc)  ! Allowing for tuning of discrete levels

            if(IZA(Nnur)==IZA(0))then
               SCRtl(i , Nejc) = sumdl*CINred(i)
               Sum = Sum + sumdl
                                !* CINRED(i)
               sumin = sumin + sumdl*CINred(i)
               sumtt = sumtt + sumdl
            else
               SCRtl(i , Nejc) = sumdl
               Sum = Sum + sumdl
            endif
            exit
         endif
      enddo
   !  write(8,*) 'Sum to level i=', i,sumdl*CINRED(i)
   enddo
   !-----do loop over inelastic levels --------- done --------------------
   !----
   !-----elastic channel
   if(IZA(Nnur)==IZA(0))then
      i = LEVtarg
      eout = eoutc - ELV(i , Nnur)
      sumdl = 0.D0
      if(eout<0.0D0)then
      else
         !  CALL TLLOC(Nnur,Nejc,eout,il,frde)
         smin = ABS(XJLv(i , Nnur) - SEJc(Nejc))
         smax = XJLv(i , Nnur) + SEJc(Nejc) + 0.01
         s = smin
         do while (.true.) !--------loop over channel spin 
            lmin = INT(ABS(xjc - s) + 1.01)
            lmax = INT(xjc + s + 1.01)
            lmax = MIN0(NLW , lmax)
            do l = lmin , lmax !--------do loop over l 
               ipar = 1 + LVP(i , Nnur)*Ipc*( - 1)**(l - 1)
               !  IF (ipar.NE.0) sumdl = sumdl + TL(il,l,Nejc,Nnur)
               !  &                          + frde*(TL(il + 1,l,Nejc,Nnur)
               !  &                          - TL(il,l,Nejc,Nnur))
               !        write(8,*) 'Elastic L=',L,' Tl=',TL(il,l,Nejc,Nnur)
               !  &                          + frde*(TL(il + 1,l,Nejc,Nnur)
               !  &                          - TL(il,l,Nejc,Nnur))
               if(ipar/=0)sumdl = sumdl + ELTl(l)
               !  write(8,*) 'Elastic L=',L,' Tl=', ELTl(l)
            enddo  
            s = s + 1.
            if(s<=smax)then
            else
               !  !sumdl = sumdl * TUNe(Nejc,Nnuc)
               sumdl = sumdl*TUNe(Nejc , NTArget)

               !--------loop over channel spin ------ done ----------------------------
               SCRtl(i , Nejc) = (sumdl + sumtt - sumin)*CELred
               Sum = Sum + sumdl*CELred
               exit
            endif
         enddo
      endif
      !  write(8,*) 'Sum to elastic', sumdl*CELred
   endif    !end of elastic
   !  write(8,*) 'Sum to levels', Sum

   DENhf = DENhf + Sum
   SCRtem(Nejc) = Sum
   return
end subroutine DECAY


subroutine DECAYD(Nnuc)
   !cc
   !cc   ********************************************************************
   !cc   *                                                         class:ppu*
   !cc   *                       D E C A Y D                                *
   !cc   *                                                                  *
   !cc   *  Calculates gamma decay of discrete levels according to          *
   !cc   *  the decay scheme contained in the IBR matrix. Prints out        *
   !cc   *  the results and updates gamma spectrum matrix CSE(.,0,NNUC)     *
   !cc   *  Must be called after all particle emission is done.             *
   !cc   *  NOTE: no particle emission from discrete levels is considered   *
   !cc   *                                                                  *
   !cc   * input:NNUC - nucleus index (position) in the table               *
   !cc   *                                                                  *
   !cc   *                                                                  *
   !cc   * output:none                                                      *
   !cc   *                                                                  *
   !cc   * calls:none                                                       *
   !cc   *                                                                  *
   !cc   *                                                                  *
   !cc   ********************************************************************
   

   implicit none
   include 'dimension.h'
   include 'global.h'

   type gamma_type
      real*8 :: Eg
      real*8 :: gXsc
   end type gamma_type
   

   integer, intent(in) :: Nnuc
   real*8 :: egd , gacs , gacs_noicc , popl
   integer :: i , icse , j , j1 , l , nejc, igamma
   integer :: INT , NINT
   type(gamma_type) :: discrGamma(1:250)  ! List of discrete gammas emitted in transitions between levels in Nnuc) 

   discrGamma(:)%Eg = 0.0d0
   discrGamma(:)%gXsc = 0.0d0
   igamma = 0

   nejc = 0
   if(Nnuc==MT91)then
      nejc = 1
   elseif(Nnuc==MT649)then
      nejc = 2
   elseif(Nnuc==MT849)then
      nejc = 3
   elseif(Nnuc==MT699)then
      nejc = 4
   elseif(Nnuc==MT749)then
      nejc = 5
   elseif(Nnuc==MT799)then
      nejc = 6
   endif

   if(IOUt>2)write(8 , 99005)
   99005 format(1x , / , 1x , 27('*') , / , 1x , 'Discrete gamma transitions ' , / , 1x , 27('*'))

   do i = 1 , NLV(Nnuc) - 1
      l = NLV(Nnuc) - i + 1
      popl = POPlv(l , Nnuc)
      if(popl>0)then
         if(BR(l , 1 , 2 , Nnuc)==0. .and. ISIsom(l , Nnuc)==0)then
            !
            !-----------Normal level without branching ratios
            !
            if(IOUt>2)write(8 , 99010)ELV(l , Nnuc) , LVP(l , Nnuc)*XJLv(l , Nnuc) , popl
            99010 format(1x , / , 5x , 'Level of energy  ' , f8.4 , ' MeV' , ' and spin ' , f6.1 , ' with population ' , g13.5 , &
                  &' mb has no brenchings - g.s. transition assumed')
            !-----------Well... let it go down to the ground state
            !  gacs = POPlv(l,Nnuc)
            POPlv(1 , Nnuc) = POPlv(1 , Nnuc) + popl
            POPlv(l , Nnuc) = 0.D0
            egd = ELV(l , Nnuc)
            !  icse = min(INT(2.0001 + egd/DE),ndecse)
            icse = min(INT(1.0001 + egd/DE) , NDECSE)
            CSE(icse , 0 , Nnuc) = CSE(icse , 0 , Nnuc) + popl/DE
            CSEt(icse , 0) = CSEt(icse , 0) + popl/DE
                                                   ! Jan 2011
            CSEmis(0 , Nnuc) = CSEmis(0 , Nnuc) + popl
            if(NPRim_g > 0 .and. ENDf(Nnuc)==1) then  ! Store discrete transitions in discrGamma structure if Prime option active
               igamma = igamma+1
               discrGamma(igamma)%Eg = egd
               discrGamma(igamma)%gXsc = popl
            elseif(ENDf(Nnuc)==1)then           ! Add transition to the exclusive or inclusive gamma spectrum
               POPcse(0 , 0 , icse , INExc(Nnuc)) = POPcse(0 , 0 , icse , INExc(Nnuc)) + popl/DE
            else
               CSE(icse , 0 , 0) = CSE(icse , 0 , 0) + popl/DE
            endif
         elseif(ISIsom(l , Nnuc)==1 .and. nejc>0)then
            !
            !-----------Isomer state in the residue after n,p, or alpha, d, t, or He3 emission
            !-----------No gamma-decay of the isomeric state imposed
            !-----------Add gamma cascade population to the direct population
            !
            POPlv(l , Nnuc) = popl + CSDirlev(l , nejc)
            if(IOUt>2)write(8 , 99012)ELV(l , Nnuc) , LVP(l , Nnuc)*XJLv(l , Nnuc) , POPlv(l , Nnuc)
            !-----------We add it to the ground state to have correct total cross section
            POPlv(1 , Nnuc) = POPlv(1 , Nnuc) + POPlv(l , Nnuc)
         elseif(ISIsom(l , Nnuc)==1)then
            !
            !-----------Isomer state in any other nucleus
            !-----------No gamma-decay of the isomeric state imposed
            !
            if(IOUt>2)write(8 , 99012)ELV(l , Nnuc) , LVP(l , Nnuc)*XJLv(l , Nnuc) , popl
            !-----------We add it to the ground state to have correct total cross section
            POPlv(1 , Nnuc) = POPlv(1 , Nnuc) + popl
         else
            !
            !-----------Normal level with branching ratios
            !
            if(IOUt>2)write(8 , 99015)ELV(l , Nnuc) , LVP(l , Nnuc)*XJLv(l , Nnuc) , popl
            99015 format(1x/ , 5x , 'Decay of  ' , f7.4 , ' MeV  ' , f5.1 , ' level with final population ' , g13.5 , ' mb' , / , &
                  & 5x , 'level populated ' , 5x , 'Egamma ' , 3x , 'intensity (cross sect.)')
            do j = 1 , NDBR
               if(BR(l , j , 2 , Nnuc)<=0.D0)cycle
               j1 = NINT(BR(l , j , 1 , Nnuc))
               if(j1==0)cycle
               if(j1>=l)then
                  write(8 , 99020)
                  99020 format(10x , 'WARNING: error in discrete level decay data' , / , 10x ,                                   &
                        &'WARNING: final level above the initial one' , / , 10x , 'WARNING: further decay not considered ')
                  write(8 , '(10X,''WARNING: nucleus '',I3,''-'',A2,'' level '', I3)') INT(A(Nnuc)) , SYMb(Nnuc) , l
                  return
               endif
               gacs = popl*BR(l , j , 2 , Nnuc)
               POPlv(j1 , Nnuc) = POPlv(j1 , Nnuc) + gacs

               gacs_noicc = gacs                          ! no int. conversion
               gacs = gacs/(1.D0 + BR(l , j , 3 , Nnuc))  !    int. conversion

               egd = ELV(l , Nnuc) - ELV(j1 , Nnuc)
               !  icse = min(INT(2.0001 + egd/DE),ndecse)
               icse = min(INT(1.0001 + egd/DE) , NDECSE)
               if(icse==1)icse = 2  ! TEMPORARY; shift low energy gammas to the second bin
               CSE(icse , 0 , Nnuc) = CSE(icse , 0 , Nnuc) + gacs/DE
               CSEt(icse , 0) = CSEt(icse , 0) + gacs/DE
               CSEmis(0 , Nnuc) = CSEmis(0 , Nnuc) + gacs
               !-------------Add transition to the exclusive gamma spectrum
               !-------------NOTE: internal conversion taken into account
               if(NPRim_g > 0 .and. ENDf(Nnuc)==1) then  ! Store discrete transitions in discrGamma structure if Prime option active
                  igamma = igamma+1
                  discrGamma(igamma)%Eg = egd
                  discrGamma(igamma)%gXsc = gacs               
               elseif(ENDf(Nnuc)==1)then
                  POPcse(0 , 0 , icse , INExc(Nnuc)) = POPcse(0 , 0 , icse , INExc(Nnuc)) + gacs/DE
               else
                  CSE(icse , 0 , 0) = CSE(icse , 0 , 0) + gacs/DE
               endif
               if(IOUt>2)write(8 , 99025)ELV(j1 , Nnuc) , LVP(j1 , Nnuc)*XJLv(j1 , Nnuc) , egd , gacs
               99025 format(5x , f7.4 , 2x , f5.1 , 5x , f7.4 , 5x , g13.5 , ' mb')

               if(NNG_xs>0 .and. ENDf(Nnuc)<=1)then
                  if(Z(Nnuc)==Z(0) .and. NINT(A(Nnuc))==NINT(A(0)))write(104 , '(1X,4I5,1X,5(G12.5,1X))')4 , NINT(A(Nnuc)) , l , &
                   & j1 , egd , EINl , gacs_noicc , gacs , popl
                  if(Z(Nnuc)==Z(0) .and. NINT(A(Nnuc)) + 1==NINT(A(0)))write(104 , '(1X,4I5,1X,5(G12.5,1X))')16 , NINT(A(Nnuc)) , &
                   & l , j1 , egd , EINl , gacs_noicc , gacs , popl
                  if(Z(Nnuc)==Z(0) .and. NINT(A(Nnuc)) + 2==NINT(A(0)))write(104 , '(1X,4I5,1X,5(G12.5,1X))')17 , NINT(A(Nnuc)) , &
                   & l , j1 , egd , EINl , gacs_noicc , gacs , popl
                  if(Z(Nnuc)==Z(0) .and. NINT(A(Nnuc)) + 3==NINT(A(0)))write(104 , '(1X,4I5,1X,5(G12.5,1X))')37 , NINT(A(Nnuc)) , &
                   & l , j1 , egd , EINl , gacs_noicc , gacs , popl
               endif
            enddo  !over branching ratios
         endif   
      endif  !over popl>0
   enddo  !over levels
   if (NPRim_g > 0) call printDiscreteGammas(igamma,discrGamma(:))
   return
   99012 format(1x , / , 5x , 'Level of energy  ' , f8.4 , ' MeV' , ' and spin ' , f6.1 , ' with final population ' , g13.5 ,     &
            &' mb is an isomer')
end subroutine DECAYD



subroutine printDiscreteGammas(imax, discretGamma)

   implicit none
   type gamma_type
      real*8 :: Eg
      real*8 :: gXsc
   end type gamma_type
   
   type(gamma_type), dimension(1:250) :: discretGamma
   type(gamma_type)  tmp
   integer, intent(in) :: imax
   integer :: i
   logical :: sorted = .true.
   real*8 :: sumGamma

   ! Sort discrete gammas according to increasing energy
   do
     sorted = .true. 
     do i = 1, imax
        if(discretGamma(i)%Eg > discretGamma(i+1)%Eg) then
           tmp = discretGamma(i)
           discretGamma(i) = discretGamma(i+1)
           discretGamma(i+1) = tmp
           sorted = .false.
        end if
     end do
     if(sorted) exit
   end do

   sumGamma = SUM(discretGamma(1:imax)%gXsc)

   ! Print discrete gammas to *.out file for ENDF-6 formatting
   
   write(12, '('' '')')
   write(12, '(10x,40(''-''))')
   write(12, '('' '')')
   write(12, '(3x,"Discrete g emission cross section ",G10.5)') sumGamma
   write(12, '('' '')')
   write(12, '(10x,40(''-''))')
   write(12, '('' '')')
   write(12, '(9x,''i'',7x,''Egam         Disc.g CS'')')
   write(12, '('' '')')
   do i = 2, imax+1
      write(12, '(i10,2G15.6)') i-1, discretGamma(i)%Eg, discretGamma(i)%gXsc
   end do 
   write(12, '('' '')')
   write(12, '(10x,40(''-''))')
   write(12, '('' '')')

   return
end subroutine printDiscreteGammas



subroutine DECAYD_DIR(Nnuc , Nejc)
   !cc
   !cc   ********************************************************************
   !cc   *                                                         class:ppu*
   !cc   *                       D E C A Y D _ D I R                        *
   !cc   *                                                                  *
   !cc   *  Calculates gamma decay of discrete levels according to          *
   !cc   *  the decay scheme contained in the IBR matrix. Special version   *
   !cc   *  to process direct population of discrete levels by a neutron,   *
   !cc   *  proton or alpha without adding gamma-transitions to spectra.    *
   !cc   *  Must be called after the decay of the first CN bin is done.     *
   !cc   *                                                                  *
   !cc   * input:Nnuc - nucleus  index (position) in the table              *
   !cc   *       Nejc - ejectile index (position) in the table              *
   !cc   *                                                                  *
   !cc   *                                                                  *
   !cc   * output:none                                                      *
   !cc   *                                                                  *
   !cc   * calls:none                                                       *
   !cc   *                                                                  *
   !cc   *                                                                  *
   !cc   ********************************************************************
   !cc

   implicit none
   include 'dimension.h'
   include 'global.h'

   integer :: Nejc , Nnuc
   intent (in) Nejc , Nnuc
   real*8 :: gacs , popl
   integer :: i , j , j1 , l
   integer :: NINT

   do i = 1 , NLV(Nnuc) - 1 !, gacs_noicc, egd
      l = NLV(Nnuc) - i + 1
      popl = CSDirlev(l , Nejc)
      if(popl<=0.D0)cycle
      if(BR(l , 1 , 2 , Nnuc)>0)then
         do j = 1 , NDBR
            j1 = NINT(BR(l , j , 1 , Nnuc))
            if(j1==0)cycle
            if(j1>=l)return
            gacs = popl*BR(l , j , 2 , Nnuc)
            CSDirlev(j1 , Nejc) = CSDirlev(j1 , Nejc) + gacs
            !  gacs_noicc = gacs                      ! no int. conversion
            gacs = gacs/(1.D0 + BR(l , j , 3 , Nnuc)) ! int. conversion
            CSEmis(0 , Nnuc) = CSEmis(0 , Nnuc) + gacs
         enddo
      else
         !-----------Well... let it go down to the ground state
         !  gacs = CSDirlev(l,nejc)
         !  CSDirlev(1,Nejc) = CSDirlev(1,Nejc) + gacs
         !  CSEmis(0,Nnuc) = CSEmis(0,Nnuc) + gacs
         CSDirlev(1 , Nejc) = CSDirlev(1 , Nejc) + popl
         CSEmis(0 , Nnuc) = CSEmis(0 , Nnuc) + popl
      endif
   enddo
end subroutine DECAYD_DIR


subroutine TL_GAMMA(Nnuc , Iec , Jc , Ipc)
   !cc
   !cc   ********************************************************************
   !cc   *                                                         class:PPu*
   !cc   *                         T L G A M M A                            *
   !cc   *                                                                  *
   !cc   * Calculates gamma decay of a continuum state in nucleus NNUC into *
   !cc   * continuum and discrete states in the same nucleus NNUC for       *
   !cc   * E1 transitions to be used in ECIS CN calculation                 *
   !cc   *                                                                  *
   !cc   * input:NNUC - decaying nucleus index                              *
   !cc   *       IEC  - energy index of the decaying state                  *
   !cc   *       JC   - spin index of the decaying state                    *
   !cc   *       IPC  - parity of the decaying state                        *
   !cc   *                                                                  *
   !cc   * output:                                                          *
   !cc   *       gamm_tr(nfiss_tr), nfiss_tr                                *
   !cc   *                                                                  *
   !cc   *                                                                  *
   !cc   *                                                                  *
   !cc   * calls:none                                                       *
   !cc   *                                                                  *
   !cc   ********************************************************************
   !

   implicit none
   include 'dimension.h'
   include 'global.h'

   integer :: Iec , Ipc , Jc , Nnuc
   intent (in) Iec , Ipc , Jc
   real*8 :: cee , cme , eg , etmp , ha , hscrtl , scrtneg , scrtpos , xjc , xjr
   real*8 :: E1 , E2 , XM1
   real :: FLOAT
   integer :: i , ier , ineg , iodd , ipar , ipos , j , jmax , jmin , jr , lamb , lambmax , lambmin , lhighest , lmax , lmin
   integer :: IABS , MAX0 , MIN0
   real*8 , dimension(10) :: xle , xlm

   !----MAXmult - maximal gamma-ray multipolarity
   !  maximal value (.LT.10) of gamma-ray multipolarity (L) in
   !  calculations of gamma-transitions both between states in
   !  continuum and from continuum states to discrete levels.
   !  A default value of 'MAXmult' is set to 2 in 'input.f'
   !  but can be adjusted in the input.
   !
   !  The radiative strength functions of higher multipole orders
   !  (f_EL, f_ML) are calculated using the relationships between
   !  single-particle radiative strength functions in the Weisskopf form.
   !
   !    Electric transitions:
   !    f_E(L+1)/f_EL = eg^2*cee*[(3+L)/(5+L)]^2,
   !    cee=[R/(\hbar*c)]^2, R=r_0*A^(2/3), r_0=1.2 fm => cee=3.7D-5*A^(2/3)
   !    xle(i) = f_Ei
   !
   !    Magnetic transitions:
   !    f_M(L+1)/f_E(L+1) = cme,
   !    cme= 10[\hbar/(m*c*R]^2 => cme = 0.307/A^(2/3)
   !    xlm(i) = f_Mi
   !
   !
   xle = 0.D0
   xlm = 0.D0
   if(MAXmult>2)then
      ha = A(Nnuc)**0.666666666666D0
      cee = 3.7D-5*ha
      cme = 0.307D0/ha
   endif

   jmin = 1
   !p jmin = MAX0(1, Jc - MAXmult)
   jmax = MIN0(NLW , Jc + MAXmult)

   xjc = FLOAT(Jc) + HIS(Nnuc)
   !-----IPOS is a parity-index of final states reached by gamma
   !-----transitions which do not change parity (E2 and M1)
   !-----INEG is a parity-index of final states reached by gamma
   !-----transitions which do change parity (E1)
   if(Iec<1)return

   if(Ipc>0)then
      ipos = 1
      ineg = 2
   else
      ipos = 2
      ineg = 1
   endif
   !-----
   !-----decay to the continuum
   !-----
   do ier = Iec - 1 , 1 , -1 !-----do loop over c.n. energies (loops over spins and parities expanded
      etmp = EX(ier , Nnuc)
      eg = EX(Iec , Nnuc) - etmp
      xle(1) = E1(Nnuc , eg , TNUc(ier , Nnuc) , etmp)*TUNe(0 , Nnuc)
      xlm(1) = XM1(eg)*TUNe(0 , Nnuc)
      xle(2) = E2(eg)*TUNe(0 , Nnuc)
      xlm(2) = xle(2)*cme
      if(MAXmult>2)then
         do i = 3 , MAXmult
            xle(i) = xle(i - 1)*eg**2*cee*((3.0D0 + FLOAT(i))/(5.0D0 + FLOAT(i)))**2
            xlm(i) = xle(i)*cme
         enddo
      endif

      lhighest = 0
      do jr = 1 , jmax
         xjr = FLOAT(jr) + HIS(Nnuc)
         lambmin = MAX0(1 , IABS(Jc - jr))
         lambmax = int(xjc + xjr + 0.001)
         lambmax = MIN0(lambmax , MAXmult)
         if(lambmin<=lambmax)then
            NGAmm_tr = max(lambmax , NGAmm_tr)
            scrtpos = 0.0
            scrtneg = 0.0
            do lamb = lambmin , lambmax
               if(lamb/2*2==lamb)then
                  scrtpos = scrtpos + xle(lamb)
                  scrtneg = scrtneg + xlm(lamb)
               else
                  scrtpos = scrtpos + xlm(lamb)
                  scrtneg = scrtneg + xle(lamb)
               endif
            enddo
            GAMm_tr(lamb) = GAMm_tr(lamb) + scrtpos*RO(ier , jr , ipos , Nnuc) + scrtneg*RO(ier , jr , ineg , Nnuc)

         endif
      enddo
   enddo
   !-----do loop over c.n. energies ***done***
   !-----decay to the continuum ----** done***-----------------------
   !  write(*,*) ' CONTINUUM Lmax=',ngamm_tr
   !  do lamb=1,MAXMULT
   !    write(*,*) 'L',lamb,' tr=',gamm_tr(lamb)
   !  enddo
   !-----
   !-----DECAY TO DISCRETE LEVELS
   !-----
   !-----do loop over discrete levels -----------------------------------
   do i = 1 , NLV(Nnuc)
      lmin = INT(ABS(xjc - XJLv(i , Nnuc)) + 0.001)
      lmax = INT(xjc + XJLv(i , Nnuc) + 0.001)
      lambmin = MAX0(1 , lmin)
      lambmax = MIN0(lmax , MAXmult)
      if(lambmin<=lambmax)then
         NGAmm_tr = max(lambmax , NGAmm_tr)
         eg = EX(Iec , Nnuc) - ELV(i , Nnuc)
         ipar = (1 + LVP(i , Nnuc)*Ipc)/2
         iodd = 1 - ipar
         xle(1) = E1(Nnuc , eg , TNUc(1 , Nnuc) , UEXcit(1 , Nnuc))*TUNe(0 , Nnuc)
         xlm(1) = XM1(eg)*TUNe(0 , Nnuc)
         xle(2) = E2(eg)*TUNe(0 , Nnuc)
         if(lambmax>2)then
            xlm(2) = xle(2)*cme
            do j = 3 , lambmax
               xle(j) = xle(j - 1)*eg**2*cee*((3.0D0 + FLOAT(j))/(5.0D0 + FLOAT(j)))**2
               xlm(j) = xle(j)*cme
            enddo
         endif
         hscrtl = 0.0D0
         do lamb = lambmin , lambmax
            if(lamb/2*2==lamb)then
               hscrtl = hscrtl + xle(lamb)*ipar + xlm(lamb)*iodd
            else
               hscrtl = hscrtl + xlm(lamb)*ipar + xle(lamb)*iodd
            endif
            GAMm_tr(lamb) = GAMm_tr(lamb) + hscrtl
         enddo
      endif
   enddo
   !  write(*,*) ' DISCRETE  Lmax=',ngamm_tr,' Jcn=', jc,' pi=',ipc
   !  do lamb=1,MAXMULT
   !    write(*,*) 'L',lamb,' tr=',gamm_tr(lamb)
   !  enddo
   !
   !-----do loop over discrete levels --------- done --------------------
   return
end subroutine TL_GAMMA


subroutine DECAYG(Nnuc , Iec , Jc , Ipc , Sum)
   !
   !cc
   !cc   ********************************************************************
   !cc   *                                                         class:PPu*
   !cc   *                         D E C A Y G                              *
   !cc   *                (function to function version)                    *
   !cc   *                                                                  *
   !cc   * Calculates gamma decay of a continuum state in nucleus NNUC into *
   !cc   * continuum and discrete states in the same nucleus NNUC           *
   !cc   *                                                                  *
   !cc   *                                                                  *
   !cc   * input:NNUC - decaying nucleus index                              *
   !cc   *       IEC  - energy index of the decaying state                  *
   !cc   *       JC   - spin index of the decaying state                    *
   !cc   *       IPC  - parity of the decaying state                        *
   !cc   *                                                                  *
   !cc   *                                                                  *
   !cc   * output:SUM - SUM of transmission coefficients over all outgoing  *
   !cc   *              channels for the requested decay (partial sums are  *
   !cc   *              stored in SCRT and SCRTL arrays for continuum and   *
   !cc   *              discrete levels respectively. SUMs for all ejectiles*
   !cc   *              combine to the total Hauser-Feshbach denominator.   *
   !cc   *              Inverse of the latter multiplied by the population  *
   !cc   *              of the (NNUC,IEC,JC,IPC) state is used to normalize *
   !cc   *              SCRT and SCRTL matrices to give residual nucleus    *
   !cc   *              population.                                         *
   !cc   *                                                                  *
   !cc   * calls:none                                                       *
   !cc   *                                                                  *
   !cc   *                                                                  *
   !cc   *                                                                  *
   !cc   ********************************************************************

   implicit none
   include 'dimension.h'
   include 'global.h'

   integer :: Iec , Ipc , Jc , Nnuc
   real*8 :: Sum
   intent (in) Iec , Ipc , Jc
   intent (inout) Sum
   real*8 :: cee , cme , eg , ha , hscrtl , scrtneg , scrtpos , xjc , xjr
   real*8 :: E1 , E2 , XM1
   real :: FLOAT
   integer :: i , ier , ineg , iodd , ipar , ipos , j , jmax , jmin , jr , lamb , lambmax , lambmin , lmax , lmin
   integer :: IABS , MAX0 , MIN0 , NINT
   real*8 , dimension(10) :: xle , xlm

      !----MAXmult - maximal gamma-ray multipolarity
   !  maximal value (.LT.10) of gamma-ray multipolarity (L) in
   !  calculations of gamma-transitions both between states in
   !  continuum and from continuum states to discrete levels.
   !  A default value of 'MAXmult' is set to 2 in 'input.f'
   !  but can be adjusted in the input.
   !
   !  The radiative strength functions of higher multipole orders
   !  (f_EL, f_ML) are calculated using the relationships between
   !  single-particle radiative strength functions in the Weisskopf form.
   !
   !    Electric transitions:
   !    f_E(L+1)/f_EL = eg^2*cee*[(3+L)/(5+L)]^2,
   !    cee=[R/(\hbar*c)]^2, R=r_0*A^(2/3), r_0=1.2 fm => cee=3.7D-5*A^(2/3)
   !    xle(i) = f_Ei
   !
   !    Magnetic transitions:
   !    f_M(L+1)/f_E(L+1) = cme,
   !    cme= 10[\hbar/(m*c*R]^2 => cme = 0.307/A^(2/3)
   !    xlm(i) = f_Mi
   !
   !
   do i = 1 , MAXmult
      xle(i) = 0.0D0
      xlm(i) = 0.0D0
   enddo
   if(MAXmult>2)then
      ha = A(Nnuc)**0.666666666666D0
      cee = 3.7D-5*ha
      cme = 0.307D0/ha
   endif

   jmin = 1
   !p jmin = MAX0(1, Jc - MAXmult)
   jmax = MIN0(NLW , Jc + MAXmult)

   Sum = 0.D0
   SCRtem(0) = 0.D0
   xjc = FLOAT(Jc) + HIS(Nnuc)
   !-----clear scratch matrix (continuum)
   do j = 1 , NLW
      do i = 1 , NEX(Nnuc)
         SCRt(i , j , 1 , 0) = 0.D0
         SCRt(i , j , 2 , 0) = 0.D0
      enddo
   enddo
   !-----clear scratch matrix (discrete levels)
   do i = 1 , NLV(Nnuc)
      SCRtl(i , 0) = 0.D0
   enddo
   !-----IPOS is a parity-index of final states reached by gamma
   !-----transitions which do not change parity (E2 and M1)
   !-----INEG is a parity-index of final states reached by gamma
   !-----transitions which do change parity (E1)
   if(Iec<1)return
   if(Ipc>0)then
      ipos = 1
      ineg = 2
   else
      ipos = 2
      ineg = 1
   endif
   !-----
   !-----decay to the continuum
   !-----
   do ier = Iec - 1 , 1 , -1 !-----do loop over c.n. energies (loops over spins and parities expanded
      eg = EX(Iec , Nnuc) - EX(ier , Nnuc)
      xle(1) = E1(Nnuc , eg , TNUc(ier , Nnuc) , UEXcit(ier , Nnuc))*TUNe(0 , Nnuc)
      xlm(1) = XM1(eg)*TUNe(0 , Nnuc)
      xle(2) = E2(eg)*TUNe(0 , Nnuc)
      if(MAXmult>2)then
         xlm(2) = xle(2)*cme
         do i = 3 , MAXmult
            xle(i) = xle(i - 1)*eg**2*cee*((3.0D0 + FLOAT(i))/(5.0D0 + FLOAT(i)))**2
            xlm(i) = xle(i)*cme
         enddo
      endif

      do jr = 1 , jmax
         xjr = FLOAT(jr) + HIS(Nnuc)
         lambmin = MAX0(1 , IABS(Jc - jr))
         lambmax = int(xjc + xjr + 0.001)
         lambmax = MIN0(lambmax , MAXmult)
         if(lambmin<=lambmax)then
            scrtpos = 0.0
            scrtneg = 0.0
            do lamb = lambmin , lambmax
               if(lamb/2*2==lamb)then
                  scrtpos = scrtpos + xle(lamb)
                  scrtneg = scrtneg + xlm(lamb)
               else
                  scrtpos = scrtpos + xlm(lamb)
                  scrtneg = scrtneg + xle(lamb)
               endif
            enddo
            SCRt(ier , jr , ipos , 0) = scrtpos*RO(ier , jr , ipos , Nnuc)
            SCRt(ier , jr , ineg , 0) = scrtneg*RO(ier , jr , ineg , Nnuc)
            if(ier==1 .and. NINT(Z(1))==NINT(Z(Nnuc)))then
               SCRt(ier , jr , ipos , 0) = SCRt(ier , jr , ipos , 0)*DEPart(Nnuc)
               SCRt(ier , jr , ineg , 0) = SCRt(ier , jr , ineg , 0)*DEPart(Nnuc)
            endif
         endif
      enddo
   enddo !over c.n. energies ***done***
   !-----decay to the continuum ----** done***
   
   !-----integration of ro*gtl in continuum for ejectile 0 (TRAPEZOID)
   do j = jmin , jmax
      do i = 1 , Iec - 1
         Sum = Sum + SCRt(i , j , 1 , 0) + SCRt(i , j , 2 , 0)
      enddo
      Sum = Sum - 0.5*(SCRt(1 , j , 1 , 0) + SCRt(1 , j , 2 , 0))
   enddo
   Sum = Sum*DE
   !-----integration of ro*gtl in continuum for ejectile 0 *** done ***
   !-----
   !-----DECAY TO DISCRETE LEVELS
   !-----
   do i = 1 , NLV(Nnuc) !-----do loop over discrete levels 
      lmin = INT(ABS(xjc - XJLv(i , Nnuc)) + 0.001)
      lmax = INT(xjc + XJLv(i , Nnuc) + 0.001)
      lambmin = MAX0(1 , lmin)
      lambmax = MIN0(lmax , MAXmult)
      if(lambmin<=lambmax)then
         eg = EX(Iec , Nnuc) - ELV(i , Nnuc)
         ipar = (1 + LVP(i , Nnuc)*Ipc)/2
         iodd = 1 - ipar
         xle(1) = E1(Nnuc , eg , TNUc(1 , Nnuc) , UEXcit(1 , Nnuc))*TUNe(0 , Nnuc)
         xlm(1) = XM1(eg)*TUNe(0 , Nnuc)
         xle(2) = E2(eg)*TUNe(0 , Nnuc)
         if(lambmax>2)then
            xlm(2) = xle(2)*cme
            do j = 3 , lambmax
               xle(j) = xle(j - 1)*eg**2*cee*((3.0D0 + FLOAT(j))/(5.0D0 + FLOAT(j)))**2
               xlm(j) = xle(j)*cme
            enddo
         endif
         hscrtl = 0.0D0
         do lamb = lambmin , lambmax
            if(lamb/2*2==lamb)then
               hscrtl = hscrtl + xle(lamb)*ipar + xlm(lamb)*iodd
            else
               hscrtl = hscrtl + xlm(lamb)*ipar + xle(lamb)*iodd
            endif
         enddo
         SCRtl(i , 0) = hscrtl
         Sum = Sum + SCRtl(i , 0)
      endif   !over lambmin<=lambmax
   enddo   !over discrete levels *** done ***
   SCRtem(0) = Sum
   DENhf = DENhf + Sum
end subroutine DECAYG


subroutine FISSION(Nnuc , Iec , Jc , Sumfis)
   !cc
   !cc   ********************************************************************
   !cc   *                                                         class:ppu*
   !cc   *                         F I S S I O N                            *
   !cc   *                                                                  *
   !cc   *    Calculates fission of the nuclear state defined by NNUC,      *
   !cc   *    IEC, and JC including two viscosity effects.                  *
   !cc   *    Corrected trapezoidal integration rule used.                  *
   !cc   *                                                                  *
   !cc   *                                                                  *
   !cc   * input:NNUC-decaying nucleus index                                *
   !cc   *       IEC -decaying state excitation energy index                *
   !cc   *       JC  -decaying state spin index                             *
   !cc   *                                                                  *
   !cc   *                                                                  *
   !cc   *                                                                  *
   !cc   * output:SUMFIS-integral of Tf*ro for the fission channel over     *
   !cc   *            kinetic energy of fission fragments (to be added      *
   !cc   *            to the Hauser-Feshbach denominator DENHF)             *
   !cc   *                                                                  *
   !cc   * calls:TLF                                                        *
   !cc   *                                                                  *
   !cc   *                                                                  *
   !cc   *                                                                  *
   !cc   ********************************************************************
   !cc

   implicit none
   include 'dimension.h'
   include 'global.h'

   integer :: Iec , Jc , Nnuc
   real*8 :: Sumfis
   intent (in) Iec , Jc
   intent (inout) Sumfis
   real*8 :: accn , ampl , ap1 , ap2 , atil , del , delp , ekin , ekinm , erest , fisba , fric , gamma , gpart , htom ,       &
                 & shredt , sum1 , sum2 , sum3 , sumf , sumgs , sumr , tau , temp
   real :: FLOAT
   integer :: INT
   integer :: kn , knm
   real*8 :: TLF


   Sumfis = 0.0
   if(EX(Iec , Nnuc)==0.0D0)return
   !-----set level density parameter systematics
   !-----EMPIRE-3.0-dependence
   call EGSMsys(ap1 , ap2 , gamma , del , delp , Nnuc)
   !-----set Ignatyuk type energy dependence for 'a'
   atil = ap1*A(Nnuc) + ap2*A(Nnuc)**0.666667
   atil = atil*ATIlnor(Nnuc)
   !-----temperature fade-out of the shell correction
   temp = 0.

   accn = atil*(1 + SHC(Nnuc)*(1 - EXP((-gamma*EX(Iec,Nnuc))))/EX(Iec , Nnuc))
   if(EX(Iec , Nnuc)>=YRAst(Jc , Nnuc))temp = SQRT((EX(Iec,Nnuc) - YRAst(Jc,Nnuc))/accn)
   ampl = EXP(TEMp0*SHRt)
   shredt = 1.
   if(temp>=TEMp0)shredt = ampl*EXP(( - SHRt*temp))
   !-----temperature fade-out of the shell correction *** done *****
   fisba = FISb(Jc , Nnuc) - SHC(Nnuc)*SHCjf(Jc , Nnuc)*shredt
   ekinm = EX(Iec , Nnuc) - fisba

   if(ekinm<0.0D0)return
   knm = INT(ekinm/DE + 1.001)
   erest = ekinm - (knm - 1)*DE
   !-----IEC to g.s.
   sumgs = TLF(ekinm)
   !-----IEC to IEC
   sum1 = TLF(0.0D0)*ROF(Iec , Jc , Nnuc)
   if(knm==1)then
      Sumfis = 0.5*(sumgs + sum1)*erest
      goto 100
   endif
   !-----IEC to IEC-1
   sum2 = TLF(DE)*ROF(Iec - 1 , Jc , Nnuc)
   !-----IEC to g.s.+1
   sum3 = TLF(FLOAT(knm - 1)*DE)*ROF(Iec - knm + 1 , Jc , Nnuc)
   Sumfis = 0.5*((sumgs + sum3)*erest + (sum1 + sum2)*DE)
   !-----correction to the trapezoidal integration rule
   Sumfis = Sumfis + ((sum3 - sumgs)*erest - (sum1 - sum2)*DE)/12.
   if(knm/=2)then
      Sumfis = Sumfis + 0.5*(sum3 + sum2)*DE
      if(knm/=3)then
   !-----------
   !-----------do loop over kinetic energy of fission fragments
   !-----------
         sumr = 0.0
         do kn = Iec - 2 , Iec - knm + 2 , -1
            ekin = EX(Iec , Nnuc) - EX(kn , Nnuc)
            sumf = TLF(ekin)*ROF(kn , Jc , Nnuc)
            sumr = sumr + sumf
         enddo
         Sumfis = Sumfis + sumr*DE
      endif
   endif
  100 continue
   if(Sumfis<1.D-25)return

   Sumfis = Sumfis*TUNefi(Nnuc)
   if(BETav/=0.0D0)then
   !--------reduction of the fission width due to possible return from the
   !--------saddle point (nuclear viscosity 1-st effect)
      htom = 1.0
      Sumfis = Sumfis*(SQRT(1.0 + (BETav/2./htom)**2) - BETav/2./htom)
      if(fisba - YRAst(Jc , Nnuc)>0.0D0)then
         !-----------reduction of fission width due to the transient time needed
         !-----------to form a saddle point (nuclear viscosity 2-nd effect)
         !-----------according to Rastopchin et al. Sov. Jour. Nucl. Phys. 53,741(1991)
         !-----------omega1=omega0=1.6*10^21 (1/s) hbar*omega=1MeV
         !-----------BETAV critical =3.2; 0.19531 stands for 1/(2*omega1**2)
         gpart = DENhf/(RO(Iec , Jc , 1 , Nnuc) + RO(Iec , Jc , 2 , Nnuc))/2.D0/PI
         !  GFIS = SUMFIS/RO(IEC,JC,NNUC)/2./PI
         tau = LOG(10.0*(fisba - YRAst(Jc,Nnuc))/temp)
         if(BETav<3.2D0)then
            tau = tau/BETav
         else
            tau = tau*BETav*0.19531
         endif
         fric = gpart*tau/0.6589
         fric = MIN(EXPmax , fric)
         if(fric>0D0)then
            fric = EXP(( - fric))
            Sumfis = Sumfis*fric
         endif
      endif
   endif
   DENhf = DENhf + Sumfis
   return
end subroutine FISSION


function TLF(Ekin)
   !-----energy dependent transmission coefficient (note that htom is
   !-----fixed below and does not depend on angular momentum as it might)
   implicit none

   real*8 :: Ekin
   real*8 :: TLF
   intent (in) Ekin
   real*8 :: atlf , htom , pix2

   data pix2/6.28318530717958647692528676655901D0/
   data htom/1.D0/
   TLF = 1.D0
   atlf = pix2*Ekin/htom
   if(atlf<38.D0) TLF = 1./(1. + EXP((-atlf)))
end function TLF


subroutine FISFIS(Nnuc , Iec , Ip , Jc , Sumfis , Mmod)
   !cc
   !cc   ********************************************************************
   !cc   *                                                         class:ppu*
   !cc   *                         F I S F I S                              *
   !cc   *                                                                  *
   !cc   *    Calculates fission of the nuclear state defined by NNUC,      *
   !cc   *    IEC, IP and JC within optical model for fission               *
   !cc   *    using double or triple humped barrier assumptions             *
   !cc   *                                                                  *
   !cc   * input:NNUC-decaying nucleus index                                *
   !cc   *       IEC -decaying state excitation energy index                *
   !cc   *       IP  -decaying state parity                                 *
   !cc   *       JC  -decaying state spin index                             *
   !cc   *       mmod-number of modes (multimodal fission)                  *
   !cc   *                                                                  *
   !cc   *                                                                  *
   !cc   *                                                                  *
   !cc   * output:SUMFIS - total fission transmission coefficient (to be
   !cc                     added to the Hauser-Feshbach denominator DENHF)  *
   !cc   *                                                                  *
   !cc   *                                                                  *
   !cc   * calls:...                                                        *
   !cc   *                                                                  *
   !cc   *                                                                  *
   !cc   *                                                                  *
   !cc   ********************************************************************
   !ccc
   implicit none
   include 'dimension.h'
   include 'global.h'

   common /FIS_ISO/ TFIso , TGIso , TISo , RFIso , PFIso
   common /IMAG  / TF , TDIr , TABs , TG2
   common /VBAR  / VBArex , HO , FAZa2

   real*8 :: FAZa2 , PFIso , RFIso , TABs , TDIr , TFIso , TG2 , TGIso , TISo
   real*8 , dimension(NFPARAB) :: HO , TF , VBArex
   integer :: Iec , Ip , Jc , Mmod , Nnuc
   real*8 :: Sumfis
   intent (in) Iec , Ip , Jc , Mmod
   intent (inout) Sumfis
   real*8 :: arg1 , barmin , ee , exfis , exfis1 , rap , rap0 , sfmin , snc , tabs1 , tdir23_sub , tindp , vsh
   real*8 :: DEXP
   real*8 , dimension(NFHUMP) :: enh
   real*8 , dimension(NFPARAB) :: enh_asym , sumtp , tfd , vbarmax , wdir
   real :: FLOAT
   integer :: ib , ibar , ih , ih1 , ipa , ist , iw , iw1 , jcc , jnc , k , nr
   integer :: INT , NINT
   real*8 , dimension(NFPARAB , NFPARAB) :: tabsp , tabspp , tdirp , tdirpp
   real*8 , dimension(NFHUMP) :: tdircont , tfcon
   real*8 , dimension(NFPARAB) :: tfdis

   ee = EX(Iec , Nnuc)

   !-----initialization
   Sumfis = 0.D0
   jcc = Jc
   tdirp = 0.D0
   tdirpp = 0.D0
   sumtp = 0.D0
   wdir = 0.D0
   tabsp = 0.D0
   tabspp = 0.D0
   tfdis = 0.D0
   tfcon = 0.D0
   tdircont = 0.D0
   TF = 0.D0
   TABs = 0.D0
   tabs1 = 0.D0
   TDIr = 0.D0

   barmin = 0.D0
   vsh = 0.D0

   if(ee==0.0D0)return

   do ib = 1 , NRBar
      vbarmax(ib) = 0.D0
      VBArex(ib) = EFB(ib)
   enddo

   do k = 1 , NRBar , 2
      HO(k) = H(1 , int(k/2) + 1)
   enddo
   do k = 2 , NRBar , 2
      HO(k) = H(1 , NRHump + int(k/2))
   enddo

   snc = FLOAT(Jc) + HIS(Nnuc)
   do ibar = 1 , NRHump
      enh_asym(ibar) = 1.D0
      if(BFF(ibar)==2)enh_asym(ibar) = 2.D0*snc + 1.D0
      if(BFF(ibar)==3)enh_asym(ibar) = 2.D0
   enddo
   enh = 1.D0
   if(NRHump==2)enh(1) = min(enh_asym(1) , enh_asym(2))
   if(NRHump==3)then
      enh(1) = min(enh_asym(1) , enh_asym(2) , enh_asym(3))
      enh(2) = min(enh_asym(2) , enh_asym(3))
      enh(3) = min(enh_asym(1) , enh_asym(2))
   endif

   !  goto 700  ! jumps over discrete contribution
   !CC Discrete transition states contribution
   do nr = 1 , NRFdis(1)
      sfmin = SFDis(nr , 1)
      ist = 1
      if(SFDis(nr , 1)==0.0 .and. IPFdis(nr , 1)==1)then
         sfmin = 0.
         ist = 2
      endif
      if(SFDis(nr , 1)==0.0 .and. IPFdis(nr , 1)== - 1)then
         sfmin = 1.
         ist = 2
      endif
      sfmin = sfmin - HIS(Nnuc)
      do jnc = INT(sfmin) , Jc , ist
         if(jnc==Jc .and. IPFdis(nr , 1)==Ip)then
            do k = 1 , NRBar , 2
               HO(k) = H(nr , int(k/2) + 1)
            enddo
            do k = 2 , NRBar , 2
               HO(k) = H(nr , NRHump + int(k/2))
            enddo
            snc = FLOAT(jnc) + HIS(Nnuc)
            do ibar = 1 , NRBar

               exfis1 = snc*(snc + 1.D0) - SFDis(nr , ibar)*(SFDis(nr , ibar) + 1.D0)
               exfis = EFDis(nr , ibar) + exfis1*HJ(Nnuc , ibar)

               VBArex(ibar) = EFB(ibar) + exfis

               if(nr==1)vbarmax(ibar) = exfis
               if(exfis>vbarmax(ibar))vbarmax(ibar) = exfis
               if(VBArex(NRHump + 1)>=VBArex(1))VBArex(NRHump + 1) = VBArex(1) - 0.02
               if(VBArex(NRHump + 1)>=VBArex(2))VBArex(NRHump + 1) = VBArex(2) - 0.02
               if(VBArex(NRHump + 2)>=VBArex(2))VBArex(NRHump + 2) = VBArex(2) - 0.02
               if(VBArex(NRHump + 2)>=VBArex(3))VBArex(NRHump + 2) = VBArex(3) - 0.02
            enddo

            if(NINT(FISopt(Nnuc))==0)then
               !-----------------complete damping
               do ibar = 1 , NRHump
                  arg1 = 2*PI*(VBArex(ibar) - ee)/H(nr , ibar)
                  if(arg1>=EXPmax)arg1 = EXPmax
                  tfd(ibar) = 1.D0/(1.D0 + EXP(arg1))
               enddo
            else
               !-----------------partial damping
               !  IF(NINT(FISbar(Nnuc)).EQ.3)CALL NUMBARR(Nnuc,Vbarex,HO)
               call WKBFIS(ee , Nnuc , tfd , tdirp , tabsp)
               !-----------------forward absorption coefficients
               do iw = 1 , NRWel
                  do iw1 = iw + 1 , NRWel + 1
                     tabspp(iw , iw1) = tabspp(iw , iw1) + tabsp(iw , iw1)
                  enddo
               enddo
               !-----------------backward absorption coefficients
               do iw = NRWel + 1 , 3 , -1
                  do iw1 = iw - 1 , 2 , -1
                     tabspp(iw , iw1) = tabspp(iw , iw1) + tabsp(iw , iw1)
                  enddo
               enddo
               !-----------------direct transmission coefficient
               do ih = 1 , NRHump
                  do ih1 = 1 , NRHump
                     if(tdirp(ih , ih1)<1D-29)tdirp(ih , ih1) = 0.D0
                     tdirpp(ih , ih1) = tdirpp(ih , ih1) + tdirp(ih , ih1)
                  enddo
               enddo
            endif    ! PARTIAL OR FULL DAMPING

            do ibar = 1 , NRHump
               tfdis(ibar) = tfdis(ibar) + tfd(ibar)
            enddo
         endif
      enddo
   enddo

   !   Continuum contribution
   if(Ip==1)ipa = 1
   if(Ip== - 1)ipa = 2
   !-----continuum direct and indirect weights for surogate optical model
   do iw = 1 , NRWel
      if(AWF(iw)==0.D0)then
         wdir(iw + 1) = 1.D0
      else

         !  barmin = ECDamp(iw)+ efb(iw)-vsh
         barmin = ECDamp(iw) + EFB(iw)
         !sin=================================================================
         vsh = 0.65
         !  wdir(iw + 1) = 1.d0*(Ee**2 - efb(NRhump + iw)**2-0.5) /
         !  &                   ((barmin**2 - efb(NRhump + iw)**2)* 1.d0)/
         !  &          dexp(- (Ee - barmin) / awf(iw))
         !  &                   (1.d0 + dexp(- (Ee - barmin) / awf(iw))))
         wdir(iw + 1) = 1.D0*(ee**2 - (EFB(NRHump+iw) - vsh)**2)/((barmin**2 - (EFB(NRHump+iw)-vsh)**2)*1.D0)                    &
                      & /dexp( - (ee - barmin)/AWF(iw))

         if(ee>=barmin)wdir(iw + 1) = 1.D0
         if(ee<=(EFB(NRHump+iw) - vsh))wdir(iw + 1) = 0.D0
         !sin==============================================================
      endif
   enddo

   !-----Continuum contribution to each hump transmission coefficient
   call SIMPSFIS(Nnuc , ee , jcc , ipa , tfcon)
   do ih = 1 , NRHump
      TF(ih) = tfdis(ih)*enh_asym(ih) + tfcon(ih)
      if(TF(ih)<=1.D-29)then
         TF(ih) = 0.D0
         tfdis(ih) = 0.D0
         tfcon(ih) = 0.D0
         Sumfis = 0.D0
         goto 890
      endif
   enddo

   if(NRHump==1)then
      Sumfis = TF(1)
      goto 890
   endif

   !  Continuum direct for surrogate optical model
   if(AWF(1)==0. .and. AWF(2)==0.)then
   else
      call SIMPSTDIR(Nnuc , ee , jcc , ipa , tdircont , barmin)
   endif

   !-----adding weighted continuum direct
   do ih = 1 , NRHump
      tdirpp(ih , ih) = TF(ih)
   enddo

   do ih = 1 , NRHump - 1
      tdirpp(ih , NRHump) = tdirpp(ih , NRHump)*enh(ih) + tdircont(ih)*(1.D0 - wdir(ih + 1))
   enddo

   if(NINT(FISopt(Nnuc))==0)then
      !--------COMPLETE DAMPING + surrogate OPT.MOD
      tfd(NRHump) = TF(NRHump)
      do ih = NRHump - 1 , 1 , -1
         if(TF(ih) + tfd(ih + 1)>0)then
            tfd(ih) = tdirpp(ih , NRHump)*(1.D0 - wdir(ih + 1)) + wdir(ih + 1)*TF(ih)*tfd(ih + 1)/(TF(ih) + tfd(ih + 1))
         else
            tfd(ih) = 0.D0
         endif
      enddo
      Sumfis = tfd(1)
   else
      !--------PARTIAL DAMPING
      !--------adding weighted continuum absorption
      do iw = 1 , NRWel
         tabspp(iw , iw + 1) = tabspp(iw , iw + 1)*enh_asym(iw) + tfcon(iw)*wdir(iw + 1)
         tabspp(iw + 1 , iw) = tabspp(iw + 1 , iw)*enh_asym(iw + 1) + tfcon(iw)*wdir(iw)
      enddo
      !--------sum of competting channels in wells
      sumtp(1) = 1.D0
      do iw = 2 , NRWel + 1
         sumtp(iw) = tdirpp(iw - 1 , 1) + tdirpp(iw , NRHump)
         do iw1 = 2 , NRWel + 1
            if(iw1/=iw)sumtp(iw) = sumtp(iw) + tabspp(iw , iw1)
         enddo
      enddo

      !--------normalization factor for the indirect terms
      rap0 = 0.D0
      rap = 1.D0
      do iw = 2 , NRWel + 1
         do iw1 = iw + 1 , NRWel + 1
            rap0 = rap0 + tabspp(iw , iw1)*tabspp(iw1 , iw)/(sumtp(iw)*sumtp(iw1))
            rap = 1.D0/(1.D0 - rap0)
         enddo
      enddo
      !--------Sumfis
      do iw = 1 , NRWel + 1
         tindp = 0.D0
         do iw1 = 2 , NRWel + 1
            if(iw1/=iw)tindp = tindp + tabspp(iw , iw1)*tdirpp(iw1 , NRHump)/(sumtp(iw)*sumtp(iw1))
         enddo
         tindp = tindp + tdirpp(iw , NRHump)/sumtp(iw)
         Sumfis = Sumfis + tindp*tabspp(1 , iw)*rap
      enddo
      Sumfis = Sumfis + tdirpp(1 , NRHump)
      TABs = tabspp(1 , 2)
      TDIr = tdirpp(1 , NRHump)
      if(ee<EFB(5))tdir23_sub = tdirpp(2 , 3)   !!!attention

      if(NINT(FISopt(Nnuc))>0 .and. NRHump==2)then
         !----------gamma transition in isomeric well
         !  only constant for the moment
         TG2 = FIStga(Nnuc)
         if(TG2<0.D0)TG2 = 0.D0
         Sumfis = TDIr + TABs*(TF(2) + RFIso*TG2)/(TF(1) + TF(2) + TG2)
      else
         TG2 = 0.D0
      endif

      !--------FISSION CONTRIBUTION TO THE HAUSER-FESHBACH denominator
      if(Sumfis<1.D-25)Sumfis = 0.D0

   endif    ! END OF COMPLETE OR PARTIAL DAMPING

   !  Now TUNEFI() considered for all discr. & cont. transitions
   890 continue
   Sumfis = Sumfis*TUNefi(Nnuc)

   if(NINT(FISmod(Nnuc))==0)DENhf = DENhf + Sumfis
   !
   !-----WRITING FISSION OUTPUT
   !
   if(Jc==1 .and. Ip==1 .and. Mmod<2)then
      write(80 , *)'  '
      write(80 , '(1X,A19,F9.5,A4)')'EXCITATION ENERGY =' , ee , ' MEV'
      write(80 , *)' '
      !---------single-humped
      if(NRBar==1)write(80 , '(17X,3(A2,9X))')'TD' , 'TC' , 'TF'
      !---------double-humped
      if(NRBar==2 .or. (NRBar==3.and.NRWel==1.and.NINT(FISopt(Nnuc))==0))write(80 , '(22X,5(A3,9X))')'TAD' , 'TBD' , 'TAC', 'TBC',&
        &'TF'
      if(NRBar==3 .and. NRWel==1 .and. NINT(FISopt(Nnuc))>=1)write(80 , '(22X,7(A4,7X))')'TAD' , 'TBD' , 'TAC' , 'TBC' , 'TF' ,   &
        &'TDIR' , 'TABS'
      !---------triple-humped
      if(NRBar==3 .and. NRWel==0)write(80 , '(22X,7(A4,7X))')'TAD' , 'TBD' , 'TCD' , 'TAC' , 'TBC' , 'TCC' , 'TF'

      if(NRBar==5)write(80 , '(16X,7(A4,7X),5(A6,5X))')'TAD' , 'TBD' , 'TCD' , 'TAC' , 'TBC' , 'TCC' , 'TF' , 'TDIR12' , 'TDIR23',&
                       &'TDIR13' , 'TABS12' , 'TABS13'
   endif
   !-----single-humped
   if(NRHump==1)write(80 , '(1X,A2,F4.1,1X,A3,I2,3G11.4)')'J=' , snc , 'PI=' , Ip , tfdis(1) , tfcon(1) , Sumfis

   !-----double-humped
   if(NRHump==2)write(80 , '(1X,A5,I1,1X,A2,F4.1,1X,A3,I2,7G11.4)')'MODE=' , Mmod , 'J=' , snc , 'PI=' , Ip , tfdis(1) , tfdis(2),&
                    & tfcon(1) , tfcon(2) , Sumfis , TDIr , TABs
   !-----triple-humped
   if(NRHump==3)write(80 , '(1X,A2,F4.1,1X,A3,I2,13G11.4)')'J=' , snc , 'PI=' , Ip , tfdis(1) , tfdis(2) , tfdis(3) , tfcon(1) ,  &
                    & tfcon(2) , tfcon(3) , Sumfis , tdirpp(1 , 2) , tdirpp(2 , 3) , tdirpp(1 , 3) , tabspp(1 , 2) , tabspp(1 , 3)

   return
end subroutine FISFIS


subroutine SIMPSFIS(Nnuc , Ee , Jcc , Ipa , Tfcon)
   !====================================================================
   !-----Simpson integration
   !====================================================================

   implicit none
   include 'dimension.h'
   include 'global.h'

   real*8 :: Ee
   integer :: Ipa , Jcc , Nnuc
   real*8 , dimension(NFHUMP) :: Tfcon
   intent (in) Ee , Ipa , Jcc
   intent (out) Tfcon
   real*8 :: arg1 , tfcc , uexcit1 , ux1
   logical :: discrete
   integer :: i , ibar , iphas_opt , nn
   integer :: INT , NINT
   real*8 , dimension(2*NFPARAB) :: phase
   real*8 , dimension(NFPARAB) :: phase_h
   discrete = .false.
   !-----iphas_opt=0 parabolic shape, iphas_opt=1 non-parabolic numerical shape
   iphas_opt = 1
   do ibar = 1 , NRHump
      tfcc = 0.D0
      Tfcon(ibar) = 0.D0
      do i = 1 , NRBinfis(ibar)
         ux1 = XMInn(ibar) + (i - 1)*DEStepp(ibar)
         if(NINT(FISbar(Nnuc))==3)then
            uexcit1 = Ee - ux1
            call PHASES(uexcit1 , phase , phase_h , Nnuc , iphas_opt , discrete)
            arg1 = 2.D0*phase_h(ibar)
         else
            arg1 = 2.D0*PI*(ux1 + EFB(ibar) - Ee)/HCOnt(ibar)
         endif
         if(arg1>=EXPmax)arg1 = EXPmax
         nn = 2
         if((i*0.5)==INT(i/2))nn = 4
         if(i==1 .or. i==(NRBinfis(ibar)))nn = 1
         tfcc = tfcc + nn*ROFisp(i , Jcc , Ipa , ibar)/(1.D0 + EXP(arg1))
      enddo
      tfcc = tfcc*DEStepp(ibar)/3.
      ! Now considered for all discr. & cont. transitions
      Tfcon(ibar) = tfcc    ! * TUNEfi(Nnuc)
   enddo
   return
end subroutine SIMPSFIS


subroutine SIMPSTDIR(Nnuc , Ee , Jcc , Ipa , Tdircont , Barmin)
   !=======================================================================
   !-----Simpson integration for direct transmission
   !=======================================================================

   implicit none
   include 'dimension.h'
   include 'global.h'

   real*8 :: Barmin , Ee
   integer :: Ipa , Jcc , Nnuc
   real*8 , dimension(NFHUMP) :: Tdircont
   intent (in) Barmin , Ee , Ipa , Jcc
   intent (inout) Tdircont
   real*8 :: arg1 , dmom , uexcit1 , ux1
   logical :: discrete
   integer :: i , ib , ibar , ih , ih1 , iphas_opt , nn
   integer :: INT , NINT
   real*8 , dimension(2*NFPARAB) :: phase
   real*8 , dimension(NFPARAB) :: phase_h
   real*8 , dimension(NFHUMP , NFHUMP) :: tdirc

   discrete = .false.                               !, vbarmax(NFHump)
   iphas_opt = 1
   arg1 = 0.D0
   dmom = 0.D0
   do ih = 1 , NRHump
      tdirc(ih , NRHump) = 0.D0
      Tdircont(ih) = 0.D0
   enddo

   ibar = 2    !!! needs better definition

   do i = 1 , NRBinfis(ibar)
      ux1 = XMInn(ibar) + (i - 1)*DEStepp(ibar)
      if(ux1>Barmin)exit
      if(NINT(FISbar(Nnuc))==3)then
         uexcit1 = Ee - ux1
         call PHASES(uexcit1 , phase , phase_h , Nnuc , iphas_opt , discrete)
         !  CALL PHASES_Parab(ee, nnuc, phase, discrete)
      endif

      do ib = 1 , NRHump
         if(NINT(FISbar(Nnuc))==3)then
            arg1 = 2.D0*phase_h(ib)
         else
            arg1 = 2.D0*PI*(ux1 + EFB(ib) - Ee)/HCOnt(ib)
         endif
         if(arg1>=EXPmax)arg1 = EXPmax
         tdirc(ib , ib) = 1.D0/(1.D0 + EXP(arg1))
      enddo

      do ih1 = NRHump - 1 , 1 , -1
         dmom = (1.D0 - tdirc(ih1 , ih1))*(1.D0 - tdirc(ih1 + 1 , NRHump))
         tdirc(ih1 , NRHump) = tdirc(ih1 , ih1)*tdirc(ih1 + 1 , NRHump)/(1.D0 + dmom)
         ! to avoid fluctuations
         !  &             (1.d0 + 2.d0 * dSQRT(dmom)             + dmom)
         !  &             (1.d0 + 2.d0 * dSQRT(dmom) * COS(arg1) + dmom)
      enddo
      nn = 2
      if((i*0.5)==INT(i/2))nn = 4
      if(i==1 .or. i==(NRBinfis(ibar)))nn = 1

      do ih = 1 , NRHump
         tdirc(ih , NRHump) = nn*tdirc(ih , NRHump)*ROFisp(i , Jcc , Ipa , ibar)
         Tdircont(ih) = Tdircont(ih) + tdirc(ih , NRHump)
      enddo
   enddo
   do ih = 1 , NRHump
      Tdircont(ih) = Tdircont(ih)*DEStepp(ibar)/3.
   enddo
   return
end subroutine SIMPSTDIR

