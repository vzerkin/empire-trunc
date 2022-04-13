MODULE HFdecay
    !  * : 5291 $
    !  * : capote $
    !  * : 2021-07-06 04:54:43 -0600 (Tue, 06 Jul 2021) $
    USE empcess, ONLY: CSDirsav, CSHms
    use width_fluct

    implicit none

    INCLUDE "dimension.h"
    INCLUDE "global.h"
    INCLUDE "main_common.h"

    INTEGER :: nejc, nnur, iang, nxsp, npsp, ia, iz, ncollx
    INTEGER :: kemin, kemax, jz, jn

    REAL*8 :: cspg, step, checkprd, ares, zres, popleft, ftmp 
    REAL*8 :: sumfis, sumfism(NFMOD), xs_norm, csum
    
    REAL*8, external :: GET_DDXS
    
    Logical primeResidue

    Public HF_decay, XSECT

contains


    subroutine HF_decay(ncollx, nnuc, nnurec, nejcec, iret, totcorr)

        INTEGER, INTENT(IN) :: ncollx     ! number of collective levels
        INTEGER, INTENT(IN) :: nnuc       ! index of decaying nucleus
        INTEGER, INTENT(IN) :: nnurec     ! index of the residue
        INTEGER, INTENT(IN) :: nejcec     ! index of the ejectile (0 - g, 1 - n, 2 - p, 3 - a, 4 - d, 5 - t, 6 - He3)
        INTEGER, INTENT(OUT) :: iret      ! number of iterations - OM fit only!
        REAL*8, INTENT(IN) :: totcorr     ! multiplicative correction to total

        ! REAL*8, external :: GET_DDXS

        INTEGER :: i, il, j

        iret = 0
        sumfism = 0.d0
        sumfis = 0.d0
        checkprd = 0.d0
        kemin = 1
        kemax = NEX(nnuc)
        ia = INT(A(nnuc))
        iz = INT(Z(nnuc))
        jz = min(INT(Z(1)) - iz, 15)       !  protection for higher energies
        jn = min(INT(A(1)) - ia - jz, 20)  !  protection for higher energies

        primeResidue = .false.
        if (nnuc == mt91 .or. nnuc == mt649 .or. &
            nnuc == mt699 .or. nnuc == mt749 .or. nnuc == mt799 .or. &    ! UNCOMMENT to allow for d, t, He3 exclusive treatment
            nnuc == mt849) primeResidue = .true.

        if (nnuc == 1) call printCompoudHeader(nnuc)
        if (FITomp <= 0 .and. nnuc /= 1) call printPrimeLevelsBeforeCascade (nnuc, totcorr)


        !-----Jump to end of loop after elastic when first OM fitting run
        If (FITomp < 0 .and. nnuc == mt2) then
            iret = 1
            RETURN
        end if

        POPmax(nnuc) = POPmax(nnuc)*0.0001d0 ! why is decreased ?
        if (POPmax(nnuc) == 0.d0) goto 1500 ! nnuc continuum has not been populated, skipping

        !   if (nnuc>1) then
        !     write (8,*) ' '
        !     write (8,*) ' -------------------------------------'
        !     write (8,'(I3,2X,''Decaying nucleus '',I3,''-'',A2)') nnuc,
        !  &             ia, SYMb(nnuc)
        !     write (8,*) ' -------------------------------------'
        !     write (8,*) ' '
        !   endif

        !-----Calculate compound nucleus level density at saddle point
        if (NINT(FISshi(nnuc)) == 1) then
            if (FISsil(nnuc)) then
                CALL ROEMP(nnuc, 1.D0, 0.0D0)
                if (FIRst_ein) write (8, *) ' WARNING: For HI reactions (FISSHI =1), LD model at sadd les is EGSM'
                if (IOUt == 6 .and. FIRst_ein) then
                    write (8, '(1X,/,'' Saddle point level density'',/)')
                    write (8, '(1X,13G10.4)') (EX(i, nnuc), (ROF(i, j, nnuc), j=1, 12), i=1, NEX(nnuc))
                end if
            end if
        end if
        call HFdecayNucleus(nnuc)   
        if (IOUt > 0) write (8, '(1X,/,'' Population neglected because too'', '' small '',G12.5,/)') popleft*DE

        1500 continue  ! no continuum population - HF decay has been skipped, deal with discrete levels 

        do il = 1, NLV(nnuc)
            CSPrd(nnuc) = CSPrd(nnuc) + POPlv(il, nnuc)  
        end do

        if (nnuc == 1) call discLevelAngularDistr() ! Updating discrete level double differential cross sections

        csum = 0.d0
        if (CSPrd(nnuc) > 0) then
            if (.not. primeResidue .and. nnuc /= 1) then
                call printMultipleEmissionHeader(nnuc)
            else
                call printPrimeResiduesAfterCascade(nnuc)
            end if
            ! Primary gammas printout
            if (nnuc == 1 .and. NPRIm_g > 0) call printPrimaryGammas(nnuc)
            CALL DECAYD(nnuc) ! Gamma decay of discrete levels
        end if 

        if ((CSPrd(nnuc) > 0) .or. (csum > 0)) call primeExclusiveReactions(nnuc)

        if (CSFis /= 0.0D0) call printFission(nnuc)
        TOTcsfis = TOTcsfis + CSFis

        if (nnuc == 1 .and. INT(AEJc(0)) /= 0 .and. ncollx > 0) call printElasticAngDistr(nnurec, nejcec)
        if (CSPrd(nnuc) > 0) call printIsomericCrossSections(nnuc)
        if (CSFis > 0.) write (12, '(1X, I3, ''-'', A2, ''-'', I3, ''fission cross section'', G12.6, ''  mb'')') &
                                  iz, SYMb(nnuc), ia, CSFis
        if (CSPrd(nnuc) > 0) call printInclusiveProductionXSec(nnuc)
        xcross(0, jz, jn) = CSEmis(0, nnuc)
        if (CSPrd(nnuc) > 0) call integrateXSections(nnuc)

        RETURN

    end subroutine HF_decay

        
    subroutine HFdecayNucleus(nnuc)
        implicit none
        integer,intent(in) :: nnuc
        integer :: ke, il, m, jcn, ipar, ip, izares, iloc
        real*8 xnl, spdif, spdiff, ded, suma, pope, xnor, fisxse
        !-----
        !-----Hauser-Feshbach nnuc nucleus decay
        !-----      
        popleft = 0.d0      
        !-----Turn on (KEMIN=1) or off (KEMIN=NEX(NNUC)) gamma cascade
        !-----in the first CN, it is preferred to use input parameter GCASC (0=OFF,1=ON)
        kemin = 1
        if (nnuc == 1 .and. GCAsc == 0.0D0) kemin = NEX(nnuc)
        !-----Turn  off (KEMIN=NEX(NNUC)) gamma cascade in the case of OMP fit
        if (FITomp /= 0) kemin = NEX(nnuc)
        kemax = NEX(nnuc)       
        !-----Account for widths fluctuations (only for top bin of the CN)
        call widthFluctuation(nnuc)
        
        cspg = 0.d0
        !-----do loop over c.n. excitation energy
        do ke = kemax, kemin, -1
            if (ke <= 0) cycle
            step = DE
            if (ke == NEX(nnuc) .or. ke == 1) step = 0.5*DE
            if (ke == NEX(nnuc) .and. nnuc == 1) step = 1.0     
            if (ENDf(1) > 0) then
            !---------Clean auxiliary particle spectra for calculation of recoils
                REClev = 0.d0
                AUSpec = 0.d0
                !---------Calculate population in the energy bin ke
                pope = 0.d0
                do jcn = 1, NLW
                    pope = pope + POP(ke, jcn, 1, nnuc) + POP(ke, jcn, 2, nnuc)
                end do
                POPbin(ke, nnuc) = pope*step
            end if      
            ! posible parallelization over parity and spin of Nnuc
            fisxse = 0.d0
            do ipar = 1, 2 ! over decaying nucleus parity
                ip = INT((-1.0)**(ipar + 1))
                do jcn = 1, NLW !over decaying nucleus spin
                    if (GDRdyn == 1.0D0) CALL ULMDYN(nnuc, jcn, EX(ke, nnuc))
                    DENhf = 0.d0
                    if (POP(ke, jcn, ipar, nnuc) < POPmax(nnuc)) then
                        popleft = popleft + POP(ke, jcn, ipar, nnuc)*DE
                        cycle
                    end if      
                    do nejc = 1, NEJcm !over ejectiles
                        ares = A(nnuc) - AEJc(nejc)
                        zres = Z(nnuc) - ZEJc(nejc)
                        !-------------Residual nuclei must be heavier than alpha
                        if (ares <= 4 .and. zres <= 2.) cycle
                        izares = INT(1000.0*zres + ares)
                        CALL WHERE (izares, nnur, iloc)
                        if (iloc == 1) cycle
                        CALL DECAY(nnuc, ke, jcn, ip, nnur, nejc, suma)
                    end do
                    !-----------do loop over ejectiles       ***done***     
                    !-----------gamma emision
                    CALL DECAYG(nnuc, ke, jcn, ip, suma)        
                    !-----------Distribute yrast population over discrete levels
                    if (DENhf == 0.0D0) then
                        if (ke == 1) then
                            ded = DE*0.5
                        else
                            ded = DE
                        end if
                        if (IOUt > 3) write (8, '('' Yrast state at bin'',I4,'' spin='',F5.1,'' pop='', G12.5$)') &
                            ke, FLOAT(ip)*(FLOAT(jcn) + HIS(nnur)), POP(ke, jcn, ipar, nnuc)*ded
                        ! Corrected on Jan 2011, Previously missed gamma XSs
                        CSEmis(0, Nnuc) = CSEmis(0, Nnuc) + POP(ke, jcn, ipar, nnuc)*ded
                        !-------------Look for the discrete level with the closest spin
                        xnl = 1.d0
                        spdiff = 100.d0
                        do il = 1, NLV(nnuc)
                            spdif = ABS(FLOAT(jcn) + HIS(nnur) - XJLv(il, nnuc))
                            if (spdif < spdiff) then
                                spdiff = spdif
                                xnl = 1.d0
                            else
                                if (spdif == spdiff) xnl = xnl + 1.
                            end if
                        end do
                        do il = 1, NLV(nnuc)
                            spdif = ABS(FLOAT(jcn) + HIS(nnur) - XJLv(il, nnuc))
                            if (spdif == spdiff) then
                                SCRtl(il, 0) = 1.0D0/xnl
                                DENhf = DENhf + SCRtl(il, 0)
                                if (IOUt > 3) write (8, '(53x,''- '',I3,''% populating lev # '',I3)') INT(100./xnl), il
                            end if
                        end do
                    end if
                    !-----------Fission ()
                    if (FISsil(nnuc) .and. NINT(FISshi(nnuc)) == 1) CALL FISSION(nnuc, ke, jcn, sumfis)
                    if (FISsil(nnuc) .and. NINT(FISshi(nnuc)) /= 1) CALL FISCROSS(nnuc, ke, ip, jcn, sumfis, sumfism)
                    !-----------
                    !-----------Normalization and accumulation
                    !-----------
                    xnor = POP(ke, jcn, ipar, nnuc)*step/DENhf
                    CALL XSECT(nnuc, xnor, sumfis, sumfism, ke, ipar, jcn, fisxse)
                    !-----------Calculate total emission
                    !
                    !           do nejc = 0, NEJcm
                    !             csemist = csemist + CSEmis(nejc,nnuc)
                    !           ENDDO
                    !           csemist = csemist + CSFis
                    !-----------------      
                end do                 !loop over decaying nucleus spin
            end do                 !loop over decaying nucleus parity       
            if (ENDf(nnuc) > 0 .and. RECoil > 0) CALL getRecoil(ke, nnuc) !recoil spectrum for ke bin
            if (FISsil(nnuc) .and. NINT(FISshi(nnuc)) /= 1 .and. fisxse > 0) then
                if (INT(FISmod(nnuc)) == 0) then
                    write (80, *) 'csfis=', CSFis, ' mb', '   fisxse=', fisxse, ' mb'
                else
                    write (80, *) '  '
                    do m = 1, INT(FISmod(nnuc)) + 1
                        write (80, *) '    Mode=', m, '  csfis=', CSFism(m), ' mb'
                    end do
                    write (80, *) 'csfis=', CSFis, ' mb', '   fisxse=', fisxse, ' mb'
                end if
            end if      
        end do                  !loop over c.n. excitation energy

    end subroutine HFdecayNucleus



    subroutine printPrimeLevelsBeforeCascade (nnuc, totcorr)
        implicit none
        integer, intent(in) :: nnuc
        real*8, intent(in) ::  totcorr

        real*8 :: dtmp, popl
        integer :: il, nbr, ib

        if (nnuc == mt91) then
            nejc = 1
        ELSEIF (nnuc == mt649) then
            nejc = 2
        ELSEIF (nnuc == mt849) then
            nejc = 3
        ELSEIF (nnuc == mt699) then
            nejc = 4
        ELSEIF (nnuc == mt749) then
            nejc = 5
        ELSEIF (nnuc == mt799) then
            nejc = 6
        else
            GOTO 1460
        end if
        dtmp = 0.d0
        do il = 1, NLV(nnuc)
            ! dtmp = dtmp + POPlv(il,nnuc)
            ! CSDirsav(il,nejc) = POPlv(il,nnuc)
            dtmp = dtmp + CSDirlev(il, nejc)
            ! Saving CSDirlev() array before the gamma cascade to CSDirsav()
            CSDirsav(il, nejc) = CSDirlev(il, nejc)
        end do
        !---------CN contribution to elastic ddx
        ELCncs = 0.d0
        ! ELCncs = POPlv(LEVtarg,mt2)/PIx4
        if (nint(ZEJc(0)) == 0 .and. nint(AEJc(0)) > 0) ELCncs = CSDirsav(LEVtarg, NPRoject)/PIx4 ! isotropic
        ! write(*,*) 'ELCncs =', CSDirsav(LEVtarg,NPRoject)
        if (dtmp <= 0.0 .and. POPlv(1, nnuc) <= 0.d0) GOTO 1460
        write (12, *)
        write (12, *) &
            ' ---------------------------------------------------------------'
        write (8, *)
        write (8, *) &
            ' ---------------------------------------------------------------'
        if (abs(QPRod(nnuc) + ELV(LEVtarg, 0)) > 99.99) then
            write (8, '(i3,''  Decaying nucleus '',I3,''-'',A2,''-'',I3,     ''  mass='' &
                  &,F10.6,'' Q-value='',F10.5,''      reac: '',A21)') &
                    nnuc, INT(Z(nnuc)), SYMb(nnuc), ia, AMAss(nnuc), QPRod(nnuc) + ELV(LEVtarg, 0), REAction(nnuc)
            write (12, '(''  Decaying nucleus '',I3,''-'',A2,''-'',I3,     ''  mass='',F1 &
                   &0.6,'' Q-value='',F10.5)') INT(Z(nnuc)), SYMb(nnuc), ia, AMAss(nnuc), QPRod(nnuc) + ELV(LEVtarg, 0)
        else
            write (8, '(i3,''  Decaying nucleus '',I3,''-'',A2,''-'',I3,     ''  mass='' &
                   &,F10.6,'' Q-value='',F10.6,''      reac: '',A21)') nnuc, INT(Z(nnuc)), SYMb(nnuc), ia, AMAss(nnuc), &
                   QPRod(nnuc) + ELV(LEVtarg, 0), REAction(nnuc)
            write (12, &
                  '(''  Decaying nucleus '',I3,''-'',A2,''-'',I3,     ''  mass='',F1 &
                  &0.6,'' Q-value='',F10.6)') INT(Z(nnuc)), SYMb(nnuc), ia, AMAss(nnuc), QPRod(nnuc) + ELV(LEVtarg, 0)
        end if
        write (8, *) ' ---------------------------------------------------------------'
        if (ENDf(nnuc) > 0) then
            write (8, '(1x,/,10X,44(1H-))')
            write (8, '(10X,''Direct level population BEFORE gamma cascade'')')
            write (8, '(10X,44(1H-),/)')
            write (12, '(1x,/,10X,44(1H-))')
            write (12, '(1X,/,10X,''Discrete level population before gamma cascade'')')
            write (12, '(1X,/,10X,40(1H-),/)')
            if (primeResidue) SUMlev_alf = 0.0
            do il = 1, NLV(nnuc)
                popl = CSDirlev(il, nejc)
                !-------------Check for the number of branching ratios
                nbr = 0
                do ib = 1, NDBR
                    if (BR(il, ib, 2, nnuc) == 0.) EXIT
                    nbr = ib
                end do
                if (primeResidue .and. il > 1 .and. nbr == 0) write (8, *) ' WARNING: Branching ratios for level ', il, &
                    ' in ', INT(A(nnuc)), '-', SYMb(nnuc), ' are missing'
                if (primeResidue) SUMlev_alf = SUMlev_alf + popl
                write (8, 99065) il, ELV(il, nnuc), LVP(il, nnuc), XJLv(il, nnuc), popl, nbr, &
                    (NINT(BR(il, ib, 1, nnuc)), BR(il, ib, 2, nnuc), ib=1, nbr)
                write (12, 99065) il, ELV(il, nnuc), LVP(il, nnuc), XJLv(il, nnuc), popl, nbr, &
                    (NINT(BR(il, ib, 1, nnuc)), BR(il, ib, 2, nnuc), ib=1, nbr)
                !----------   Next if moves levels' population to the ground state
                !-----------  to avoid gamma cascade between discrete levels
                !-----------  originating from the direct population
                !-----------  of discrete levels by a neutron, proton, deuteron, triton, He3 or alpha.
                !-----------  These gammas should not go into MT=91, 649, 699, 749, 799 or 849.
                !-----------  HOWEVER until 699, 749 and 799 are included in formatting
                !-----------  we should not eliminate respective gamma transitions since they
                !-----------  should be included in the formatted file.
                if (il > 1 .and. primeResidue) then
                    POPlv(1, nnuc) = POPlv(1, nnuc) + popl
                    POPlv(il, nnuc) = POPlv(il, nnuc) - popl
                end if
            end do ! end of the loop over discrete levels
            write (12, '(1X,/,10X,40(1H-),/)')
            !
            !-----------Decay direct population of discrete levels by a neutron,
            !-----------proton, d, t, 3He, or alpha without storing emitted gammas in the spectra.
            if (primeResidue) CALL DECAYD_DIR(nnuc, nejc)
            !-----------At this point nnuc could be further decayed and printed out
            !-----------to separte exclusive reaction with single emission of
            !-----------d, t, He3 or alpha from other (inclusive) channels involving
            !-----------sequential emission of various particles.           
        
        end if
        !---------Write elastic to tape 12 and to tape 68
        1460 if (nnuc == mt2) call writeElastic(totcorr)
        99065 format(I12, F10.6, I5, F8.1, 1P, E15.6, I3, 7(I4, 1X, 1P, E10.4), :/, (53X, 7(I4, 1X, 1P, E10.4)))

    end subroutine printPrimeLevelsBeforeCascade 


    subroutine printPrimeResiduesAfterCascade(nnuc)
        implicit none
        integer,intent(in) :: nnuc

        integer :: il, nbr, ib, nbr_icc
        real*8 :: esum

        if (ENDF(nnuc) > 0) write (8, &
           '(3X,''NOTE: Due to ENDF option discrete levels contribution'',/, &
           &  3X,''NOTE: was not included in emission spectra and direct ''/ &
           &  3X,''NOTE: particle contribution was shifted to the g.s.'')')
        if (kemin == NEX(nnuc) .and. nnuc == 1) write (8, &
           '(10X,''(no gamma cascade in the compound nucleus, primary transitions only)'',/)')
        do il = 1, NLV(nnuc)
            csum = csum + POPlv(il, nnuc)
        end do
        if (csum == 0) RETURN ! no disc level population
            write (8, '(1X,/,10X,''Discrete level population AFTER gamma cascade from continum'',/, &
                  & 10X,''and potential side-feeding by various reactions'')')
            write (8, '(1X,/,10X,60(1H-),/)')
            write (12, '(1X,/,10X,''**Discrete level population after gamma cascade from continum'')')
            write (12, '(1X,/,10X,40(1H-),/)')
            nbr_icc = 0
            do il = 1, NLV(nnuc)
                if (ISIsom(il, Nnuc) == 0) then
                 write (8, '(I12,F10.6,I5,F8.1,1P,G15.6)') il, ELV(il, nnuc), LVP(il, nnuc), XJLv(il, nnuc), POPlv(il, nnuc)
                else
                    write (8, 99071) il, ELV(il, nnuc), LVP(il, nnuc), XJLv(il, nnuc), POPlv(il, nnuc), ' ISOMER'
                    99071 format(I12, F10.6, I5, F8.1, 1P, G15.6, A7)
                end if
                ! if (nnuc==1) then
                ! Check for the number of internal conversion coefficients
                nbr = 0
                do ib = 1, NDBR
                    if (BR(il, ib, 3, nnuc) == 0.) EXIT
                    nbr = ib
                end do
                nbr_icc = max(nbr, nbr_icc)
                ! Check for the number of branching ratios
                nbr = 0
                do ib = 1, NDBR
                    if (BR(il, ib, 2, nnuc) == 0.) EXIT
                    nbr = ib
                end do
                !              if (nbr==0 .and. il>1 .and. FIRst_ein) write (8,*)
                !  &               ' WARNING: Branching ratios for level ', il,' in ',
                !  &               INT(A(nnuc)), '-', SYMb(nnuc), ' are missing'
                !              write ( 8,99065) il, ELV(il,nnuc), LVP(il,nnuc),
                !  &                          XJLv(il,nnuc), POPlv(il,nnuc), nbr,
                !  &                          (NINT(BR(il,ib,1,nnuc)),BR(il,ib,2,nnuc)
                !  &                          ,ib = 1,nbr)
                write (12, 99065) il, ELV(il, nnuc), LVP(il, nnuc), XJLv(il, nnuc), POPlv(il, nnuc), nbr, &
                    (NINT(BR(il, ib, 1, nnuc)), BR(il, ib, 2, nnuc), ib=1, nbr)
                ! ENDIF
            end do ! over discrete levels
            if (nbr_icc > 0) then
                write (8, '(1X,/,10X,60(1H-),/)')
                write (12, '(1X,/,10X,40(1H-),/)')
                ! Write Int. Conv. Coefff. for discrete transitions
                write (12, '(1X,/,10X,''Internal conversion coefficients'')')
                write (12, '(1X,/,10X,40(1H-),/)')
                esum = 0.d0
                ftmp = 0.d0
                do il = 1, NLV(nnuc)
                    ! Check for the number of branching ratios
                    nbr = 0
                    do ib = 1, NDBR
                        if (BR(il, ib, 3, nnuc) == 0.) EXIT
                        nbr = ib
                    end do
                    if (nbr == 0 .and. il > 1 .and. FIRst_ein) write (8, *) &
                        ' WARNING: Conversion coeff for level ', il, ' in ', INT(A(nnuc)), '-', SYMb(nnuc), ' are missing'
                    esum = esum + POPlv(il, nnuc)*(EMAx(nnuc) - ELV(il, nnuc))
                    ftmp = ftmp + POPlv(il, nnuc)
                    write (12, 99065) il, ELV(il, nnuc), LVP(il, nnuc), XJLv(il, nnuc), POPlv(il, nnuc), nbr, &
                        (NINT(BR(il, ib, 1, nnuc)), BR(il, ib, 3, nnuc), ib=1, nbr)
                    99065 format(I12, F10.6, I5, F8.1, 1P, E15.6, I3, 7(I4, 1X, 1P, E10.4), :/, (53X, 7(I4, 1X, 1P, E10.4)))
                end do
                write (8, '(1X,/,10X,40(1H-),/)')
                write (12, '(1X,/,10X,40(1H-),/)')
            end if ! nbr_icc>0

    end subroutine printPrimeResiduesAfterCascade


    subroutine printMultipleEmissionHeader(nnuc)
        implicit none
        integer, intent(in) :: nnuc

        write (12, *)
        write (12, *) ' ---------------------------------------------------------------'
        write (8, *)
        write (8, *) ' ---------------------------------------------------------------'
        if (abs(QPRod(nnuc) + ELV(LEVtarg, 0)) > 99.99) then
            write (8, '(i3,''  Decaying nucleus '',I3,''-'',A2,''-'',I3,     ''  mass='' &
                   &,F10.6,'' Q-value='',F10.5,''      react: '',A21)') &
                   nnuc, INT(Z(nnuc)), SYMb(nnuc), ia, AMAss(nnuc), &
                   QPRod(nnuc) + ELV(LEVtarg, 0), REAction(nnuc)
            write (12, '(''  Decaying nucleus '',I3,''-'',A2,''-'',I3,     ''  mass='',F1 &
                   &0.6,'' Q-value='',F10.5)') INT(Z(nnuc)), SYMb(nnuc), ia, &
                    AMAss(nnuc), QPRod(nnuc) + ELV(LEVtarg, 0)
        else
            write (8, '(i3,''  Decaying nucleus '',I3,''-'',A2,''-'',I3,     ''  mass='' &
                   &,F10.6,'' Q-value='',F10.6,''      react: '',A21)') &
                   nnuc, INT(Z(nnuc)), SYMb(nnuc), ia, AMAss(nnuc), &
                   QPRod(nnuc) + ELV(LEVtarg, 0), REAction(nnuc)
            write (12, '(''  Decaying nucleus '',I3,''-'',A2,''-'',I3,     ''  mass='',F1 &
                   &0.6,'' Q-value='',F10.6)') INT(Z(nnuc)), SYMb(nnuc), ia, &
                   AMAss(nnuc), QPRod(nnuc) + ELV(LEVtarg, 0)
        end if
        write (12, *) ' ---------------------------------------------------------------'
        write (8, *) ' ---------------------------------------------------------------'
        write (12, *)
        write (8, *)

    end subroutine printMultipleEmissionHeader


    subroutine primeExclusiveReactions(nnuc)
        !---------------------------------------------------------------
        !  Varius opreations and lot of printing related to treatment 
        !  of prime emission channels (first emission of g, n, p, a, 
        !  d, t and 3He) for the ENDF file. 
        !---------------------------------------------------------------
        implicit none
        integer,intent(in) :: nnuc
        character*9 :: cejectile
        real*8 :: dtmp, cmulg
        real*8 :: gtotsp, xtotsp, ptotsp, atotsp, dtotsp, ttotsp, htotsp, ctotsp
        real*8 :: emedg, emedn, emedp, emeda, emedd, emedt, emedh, emedc, totsp, ftmp_disc

        integer :: nspec, ispec, itmp


        if (kemin == NEX(nnuc) .and. nnuc == 1) write &
            (8, '(1X,''(no gamma cascade in the compound nucleus, primary transitions only)'',/)')
        ! Integrating exclusive population spectra (ENDF)
        gtotsp = 0.d0
        xtotsp = 0.d0
        ptotsp = 0.d0
        atotsp = 0.d0
        dtotsp = 0.d0
        ttotsp = 0.d0
        htotsp = 0.d0
        ctotsp = 0.d0
        emedg = 0.d0
        emedn = 0.d0
        emedp = 0.d0
        emeda = 0.d0
        emedd = 0.d0
        emedt = 0.d0
        emedh = 0.d0
        emedc = 0.d0
        if (ENDf(nnuc) /= 1) RETURN
        nspec = min(INT(EMAx(nnuc)/DE) + 1, NDECSE - 1)
        do ispec = 1, nspec
            gtotsp = gtotsp + POPcse(0, 0, ispec, INExc(nnuc))*DE
            xtotsp = xtotsp + POPcse(0, 1, ispec, INExc(nnuc))*DE
            ptotsp = ptotsp + POPcse(0, 2, ispec, INExc(nnuc))*DE
            atotsp = atotsp + POPcse(0, 3, ispec, INExc(nnuc))*DE
            dtotsp = dtotsp + POPcse(0, 4, ispec, INExc(nnuc))*DE
            ttotsp = ttotsp + POPcse(0, 5, ispec, INExc(nnuc))*DE
            htotsp = htotsp + POPcse(0, 6, ispec, INExc(nnuc))*DE
            emedg = emedg + POPcse(0, 0, ispec, INExc(nnuc))*DE*(ispec - 1)*DE
            emedn = emedn + POPcse(0, 1, ispec, INExc(nnuc))*DE*(ispec - 1)*DE
            emedp = emedp + POPcse(0, 2, ispec, INExc(nnuc))*DE*(ispec - 1)*DE
            emeda = emeda + POPcse(0, 3, ispec, INExc(nnuc))*DE*(ispec - 1)*DE
            emedd = emedd + POPcse(0, 4, ispec, INExc(nnuc))*DE*(ispec - 1)*DE
            emedt = emedt + POPcse(0, 5, ispec, INExc(nnuc))*DE*(ispec - 1)*DE
            emedh = emedh + POPcse(0, 6, ispec, INExc(nnuc))*DE*(ispec - 1)*DE
            if (NDEJC == 7) then
                ctotsp = ctotsp + POPcse(0, NDEJC, ispec, INExc(nnuc))*DE
                emedc = emedc + POPcse(0, NDEJC, ispec, INExc(nnuc))*DE*(ispec - 1)*DE
            end if
        end do
        POPcs(0, INExc(nnuc)) = gtotsp
        POPcs(1, INExc(nnuc)) = xtotsp
        POPcs(2, INExc(nnuc)) = ptotsp
        POPcs(3, INExc(nnuc)) = atotsp
        POPcs(4, INExc(nnuc)) = dtotsp
        POPcs(5, INExc(nnuc)) = ttotsp
        POPcs(6, INExc(nnuc)) = htotsp
        if (NDEJC == 7) POPcs(NDEJC, INExc(nnuc)) = ctotsp
        write (12, *)
        write (8, *)
        !
        ! Accumulating population cross sections
        !
        ! RC: this part only calculates the exclusive population CSPopul() to print the calculate inclusive cross section CSInc()
        do nejc = 0, NDEJC         !loop over ejectiles
            if (POPcs(nejc, INExc(nnuc)) <= 0.d0) cycle
            if (ENDfp(nejc, nnuc) == 1) then
                itmp = 1
                ! estimating multiplicity
                if (nejc == 1 .or. nejc == 2) itmp = NINT(A(1)) - NINT(A(nnuc))   ! (n,xn), (n,xp)
                if (.not. (NINT(A(1)) - NINT(A(nnuc)) > 2 .and. nejc == 0)) &
                    CSPopul(nnuc) = CSPopul(nnuc) + POPcs(nejc, INExc(nnuc))/itmp !  Summing exclusive cross section
                ! RCN 12/2015
                ! write (*,*) 'excl:',NINT(A(nnuc)),NINT(Z(nnuc)), &
                !    nejc,sngl(POPcs(nejc,INExc(nnuc))/itmp), ' ', trim(Reaction(nnuc))
            end if
        end do
        do nejc = 0, NDEJC         !loop over ejectiles
            if (POPcs(nejc, INExc(nnuc)) <= 0.d0) cycle
            if (nejc == 0) then
                cejectile = 'gammas   '
            ELSEIF (nejc == 1) then
                cejectile = 'neutrons '
            ELSEIF (nejc == 2) then
                cejectile = 'protons  '
            ELSEIF (nejc == 3) then
                cejectile = 'alphas   '
            ELSEIF (nejc == 4) then
                cejectile = 'deuterons'
            ELSEIF (nejc == 5) then
                cejectile = 'tritons  '
            ELSEIF (nejc == 6) then
                cejectile = 'helium-3 '
            ELSEIF (nejc == NDEJC) then
                cejectile = 'lt. ions '
            end if
            if (nejc == 0) then
                ftmp = -1.d0
                ftmp_disc = 0.d0
                !  if (nnuc==mt91 ) ftmp_disc = CSDirlev(1,1)
                !  if (nnuc==mt649) ftmp_disc = CSDirlev(1,2)
                if (nnuc == mt849) ftmp_disc = CSDirlev(1, 3)
                if (nnuc == mt699) ftmp_disc = CSDirlev(1, 4)
                if (nnuc == mt749) ftmp_disc = CSDirlev(1, 5)
                if (nnuc == mt799) ftmp_disc = CSDirlev(1, 6)
                ftmp = (POPcs(3, INExc(nnuc)) + ftmp_disc)/CSPrd(nnuc)
                CSGinc(3) = POPcs(0, INExc(nnuc))*ftmp
                dtmp = (1.d0 - ftmp)*POPcs(nejc, INExc(nnuc))
                if (dtmp < 1.d-7) then
                    CSGinc(3) = POPcs(0, INExc(nnuc))
                    ftmp = -1.d0
                end if
                if (ftmp > 0) then
                    write (12, 97532) iz, SYMb(nnuc), ia, POPcs(nejc, INExc(nnuc)), cejectile
                    dtmp = (1.d0 - ftmp)*POPcs(nejc, INExc(nnuc))
                    if (dtmp < 1.d-7) dtmp = 0.d0
                    write (12, 97533) iz, SYMb(nnuc), ia, dtmp, cejectile
                    write (12, 97531) iz, SYMb(nnuc), ia, ftmp*POPcs(nejc, INExc(nnuc)), cejectile
                else
                    write (12, 9753) iz, SYMb(nnuc), ia, POPcs(nejc, INExc(nnuc)), cejectile
                end if
            else
                write (12, 9753) iz, SYMb(nnuc), ia, POPcs(nejc, INExc(nnuc)), cejectile
            end if
            9753  format(1X, I3, '-', A2, '-', I3, ' population cross section', G12.6, '  mb   : ', A9)
            97531 format(1X, I3, '-', A2, '-', I3, ' population cross section', G12.6, '  mb   : ', A9, '   (exclus)')
            ! 97534 format(1X, I3, '-', A2, '-', I3, ' deut. pop. cross section', G12.6, '  mb   : ', A9, '   (deut  )')
            ! 97535 format(1X, I3, '-', A2, '-', I3, ' np+pn pop. cross section', G12.6, '  mb   : ', A9, '   (np+pn )')
            97532 format(1X, I3, '-', A2, '-', I3, ' tot. gamma cross section', G12.6, '  mb   : ', A9)
            97533 format(1X, I3, '-', A2, '-', I3, ' incl.gamma cross section', G12.6, '  mb   : ', A9)
        end do
        write (8, *)
        if (gtotsp /= 0) emedg = emedg/gtotsp
        if (xtotsp /= 0) emedn = emedn/xtotsp
        if (ptotsp /= 0) emedp = emedp/ptotsp
        if (atotsp /= 0) emeda = emeda/atotsp
        if (dtotsp /= 0) emedd = emedd/dtotsp
        if (ttotsp /= 0) emedt = emedt/ttotsp
        if (htotsp /= 0) emedh = emedh/htotsp
        if (ctotsp /= 0) emedc = emedc/ctotsp
        !  Add contributions to discrete levels for MT=91,649,...,849
        !  (merely for checking purpose)
        do nejc = 0, NDEjc
            xnorm(nejc, INExc(nnuc)) = 1.0d0
        end do
        if (nnuc == mt91) then
            nejc = 1
            write (8, 97547) xtotsp
            write (12, 97547) xtotsp
            write (8, 97548) CSDirlev(1, nejc)
            write (12, 97548) CSDirlev(1, nejc)
            xtotsp = xtotsp + CSDirlev(1, nejc)
            write (8, 97549) xtotsp
            write (12, 97549) xtotsp
        ELSEIF (nnuc == mt649) then
            nejc = 2
            write (8, 97547) ptotsp
            write (12, 97547) ptotsp
            write (8, 97548) CSDirlev(1, nejc)
            write (12, 97548) CSDirlev(1, nejc)
            ptotsp = ptotsp + CSDirlev(1, nejc)
            write (8, 97549) ptotsp
            write (12, 97549) ptotsp
        ELSEIF (nnuc == mt849) then
            nejc = 3
            write (8, 97547) atotsp
            write (12, 97547) atotsp
            write (8, 97548) CSDirlev(1, nejc)
            write (12, 97548) CSDirlev(1, nejc)
            atotsp = atotsp + CSDirlev(1, nejc)
            write (8, 97549) atotsp
            write (12, 97549) atotsp
        ELSEIF (nnuc == mt699) then
            nejc = 4
            write (8, 97547) dtotsp
            write (12, 97547) dtotsp
            write (8, 97548) CSDirlev(1, nejc)
            write (12, 97548) CSDirlev(1, nejc)
            dtotsp = dtotsp + CSDirlev(1, nejc)
            write (8, 97549) dtotsp
            write (12, 97549) dtotsp
        ELSEIF (nnuc == mt749) then
            nejc = 5
            write (8, 97547) ttotsp
            write (12, 97547) ttotsp
            write (8, 97548) CSDirlev(1, nejc)
            write (12, 97548) CSDirlev(1, nejc)
            ttotsp = ttotsp + CSDirlev(1, nejc)
            write (8, 97549) ttotsp
            write (12, 97549) ttotsp
        ELSEIF (nnuc == mt799) then
            nejc = 6
            write (8, 97547) htotsp
            write (12, 97547) htotsp
            write (8, 97548) CSDirlev(1, nejc)
            write (12, 97548) CSDirlev(1, nejc)
            htotsp = htotsp + CSDirlev(1, nejc)
            write (8, 97549) htotsp
            write (12, 97549) htotsp
        else
            if (LHMs > 0 .and. atotsp < 1.0d-8) then
                totsp = CSprd(nnuc) - dtotsp - htotsp - ttotsp
                if (NDEJC == 7) totsp = totsp - ctotsp
                nxsp = 0
                if (xtotsp > 0.0d0) then
                    if (ttotsp > 0.0d0) then
                        nxsp = MAX(INT((xtotsp - dtotsp)/totsp + 0.5d0), 0)
                        xnorm(1, INExc(nnuc)) = (nxsp*totsp + dtotsp)/xtotsp
                        xtotsp = nxsp*totsp + dtotsp
                    else
                        nxsp = INT(xtotsp/totsp + 0.5d0)
                        xnorm(1, INExc(nnuc)) = nxsp*totsp/xtotsp
                        xtotsp = nxsp*totsp
                    end if
                    POPcs(1, INExc(nnuc)) = xtotsp
                    if (ABS(1.0d0 - xnorm(1, INExc(nnuc))) > 0.01d0) &
                        write (8, '(''  WARNING: Exclusive neutron spectrum renormalized by'', f6.3)') xnorm(1, INExc(nnuc))
                end if
                npsp = 0
                if (ptotsp > 0.0d0) then
                    if (htotsp > 0.0d0) then
                        npsp = MAX(INT((ptotsp - dtotsp)/totsp + 0.5d0), 0)
                        xnorm(2, INExc(nnuc)) = (npsp*totsp + dtotsp)/ptotsp
                        ptotsp = npsp*totsp + dtotsp
                    else
                        npsp = INT(ptotsp/totsp + 0.5d0)
                        xnorm(2, INExc(nnuc)) = npsp*totsp/ptotsp
                        ptotsp = npsp*totsp
                    end if
                    POPcs(2, INExc(nnuc)) = ptotsp
                    if (ABS(1.0d0 - xnorm(2, INExc(nnuc))) > 0.01d0) write (8, &
                        '(''  WARNING: Exclusive  proton spectrum renormalized by'', f6.3)') xnorm(2, INExc(nnuc))
                end if
            end if
        end if
        write (8, *)
        write (8, '(116(1H_))')
        write (8, *) 'Population of residual nuclei (exclusive spectra - CMS)'
        write (8, '('' Energy'',14x,''gamma'',9x,''neutron'',8x,''proton'',10x,''alpha'',10x,''deut '',10x, &
                  & ''trit '',10x,''He-3 '')')
        write (8, '(116(1H-))')
        do ispec = 1, nspec
            POPcse(0, 1, ispec, INExc(nnuc)) = &
                xnorm(1, INExc(nnuc))*POPcse(0, 1, ispec, INExc(nnuc))
            POPcse(0, 2, ispec, INExc(nnuc)) = &
                xnorm(2, INExc(nnuc))*POPcse(0, 2, ispec, INExc(nnuc))
            if (NDEJC == 7) then
                write (8, '(9g15.5)') (ispec - 1)*DE, &
                    POPcse(0, 0, ispec, INExc(nnuc)), &
                    POPcse(0, 1, ispec, INExc(nnuc)), &
                    POPcse(0, 2, ispec, INExc(nnuc)), &
                    POPcse(0, 3, ispec, INExc(nnuc)), &
                    POPcse(0, 4, ispec, INExc(nnuc)), &
                    POPcse(0, 5, ispec, INExc(nnuc)), &
                    POPcse(0, 6, ispec, INExc(nnuc)), &
                    POPcse(0, NDEJC, ispec, INExc(nnuc))
            else
                write (8, '(8g15.5)') (ispec - 1)*DE, &
                    POPcse(0, 0, ispec, INExc(nnuc)), &
                    POPcse(0, 1, ispec, INExc(nnuc)), &
                    POPcse(0, 2, ispec, INExc(nnuc)), &
                    POPcse(0, 3, ispec, INExc(nnuc)), &
                    POPcse(0, 4, ispec, INExc(nnuc)), &
                    POPcse(0, 5, ispec, INExc(nnuc)), &
                    POPcse(0, 6, ispec, INExc(nnuc))
            end if
        end do
        if (NDEJC == 7) then
            write (8, '(116(1H-))')
            write (8, '(''Tot.popul.'',5X,8g15.6)') gtotsp, xtotsp, ptotsp, atotsp, dtotsp, ttotsp, htotsp, ctotsp
            write (8, '(''E-aver.'',8X,8g15.6)') emedg, emedn, emedp, emeda, emedd, emedt, emedh, emedc
        else
            write (8, '(116(1H-))')
            write (8, '(''Tot.popul.'',5X,8g15.6)') gtotsp, xtotsp, ptotsp, atotsp, dtotsp, ttotsp, htotsp
            write (8, '(''E-aver.'',8X,8g15.6)') emedg, emedn, emedp, emeda, emedd, emedt, emedh
        end if
        cmulg = gtotsp/CSPrd(nnuc)
        write (8, '(''Multip.'',8X,8g15.6)') cmulg
        write (8, '(116(1H-)/)')

        97547   format(3X, ' Cont. popul. with cont. g-casc. ', G12.6, ' mb')
        97548   format(3X, ' Disc. popul. with cont. g-casc. ', G12.6, ' mb')
        97549   format(3X, ' Total popul. with cont. g-casc. ', G12.6, ' mb')

    end subroutine primeExclusiveReactions


    subroutine printFission(nnuc)
 
        implicit none
        integer,intent(in) :: nnuc
        integer ::  m
    
        write (80, *)
        write (8, *)
        if (IOUt > 0) then
            do m = 1, INT(FISmod(nnuc)) + 1
                WFIsm(m) = 0.d0
                if (CSFis > 0.d0) WFIsm(m) = CSFism(m)/CSFis
                if (NINT(FISmod(nnuc)) > 0 .and. &
                    NINT(FISshi(nnuc)) /= 1) &
                    write (80, *) '    Mode=', m, '   weight=', WFIsm(m)
            end do
            if (NINT(FISshi(nnuc)) /= 1) write (80, *) '   Fission cross section=', CSFis, ' mb'
        end if
        CSPfis(nnuc) = CSFis
        write (8, '(1X,I3,''-'',A2,''-'',I3,'' fission cross  section '',G12.5, '' mb''/)') iz, SYMb(nnuc), ia, CSFis

    end subroutine printFission

    subroutine printElasticAngDistr(nnurec,  nejcec)
        implicit none
        integer,intent(in) :: nnurec
        integer,intent(in) :: nejcec

        integer :: its, ilv, iloc
        real*8 :: gang
        
        ! Locate position of the projectile among ejectiles
        CALL WHEREJC(IZAejc(0), nejcec, iloc)
        its = ncollx
        if (CSAlev(1, ICOller(2), nejcec) > 0) then
            gang = 180.d0/(NDAng - 1)
            write (8, 99029)
            write (8, 99030) (ICOller(ilv), ilv=1, MIN(its, 10))
            write (8, 99032) (ELV(ICOller(ilv), nnurec), &
                              ilv=1, MIN(its, 10))
            write (8, 99034) (XJLv(ICOller(ilv), nnurec)* &
                              LVP(ICOller(ilv), nnurec), D_DEF(ilv, 2), ilv=1, MIN(its, 10))
            write (8, *) ' '
            do iang = 1, NDANG
                write (8, 99035) (iang - 1)*gang, ELAred*elada(iang) + CSAlev(iang, 1, nejcec), &
                    (CSAlev(iang, ICOller(ilv), nejcec), ilv=2, MIN(its, 10))
            end do
            write (8, *) ' '
            write (8, 99041) 1, ELAred*ELAcs + PIx4*PL_CN(0, LEVtarg), (POPlv(ICOller(ilv), nnurec), ilv=2, MIN(its, 10))
            if (its > 10) then
                write (8, *) ' '
                write (8, 99030) (ICOller(ilv), ilv=11, MIN(its, 20))
                write (8, 99032) (ELV(ICOller(ilv), nnurec), ilv=11, MIN(its, 20))
                write (8, 99034) (XJLv(ICOller(ilv), nnurec)*LVP(ICOller(ilv), nnurec), D_DEF(ilv, 2), ilv=11, MIN(its, 20))
                write (8, *) ' '
                do iang = 1, NDANG
                    write (8, 99035) (iang - 1)*gang, (CSAlev(iang, ICOller(ilv), nejcec), ilv=11, MIN(its, 20))
                end do
                write (8, *) ' '
                write (8, 99041) 2, (POPlv(ICOller(ilv), nnurec), ilv=11, MIN(its, 20))
            end if
            if (its > 20) then
                write (8, *) ' '
                write (8, 99030) (ICOller(ilv), ilv=21, MIN(its, 30))
                write (8, 99032) (ELV(ICOller(ilv), nnurec), ilv=21, MIN(its, 30))
                write (8, 99034) (XJLv(ICOller(ilv), nnurec)*LVP(ICOller(ilv), nnurec), D_DEF(ilv, 2), ilv=21, MIN(its, 30))
                write (8, *) ' '
                do iang = 1, NDANG
                    write (8, 99035) (iang - 1)*gang, (CSAlev(iang, ICOller(ilv), nejcec), ilv=21, MIN(its, 30))
                end do
                write (8, *) ' '
                write (8, 99041) 3, (POPlv(ICOller(ilv), nnurec), ilv=21, MIN(its, 30))
            end if
            ! Because of the ENDF format restrictions the maximum
            ! number of discrete levels is limited to 40
            if (its > 30) then
                write (8, *) ' '
                write (8, 99030) (ICOller(ilv), ilv=31, MIN(its, 40))
                write (8, 99032) (ELV(ICOller(ilv), nnurec), ilv=31, MIN(its, 40))
                write (8, 99034) (XJLv(ICOller(ilv), nnurec)*LVP(ICOller(ilv), nnurec), D_DEF(ilv, 2), ilv=31, MIN(its, 40))
                write (8, *) ' '
                do iang = 1, NDANG
                    write (8, 99035) (iang - 1)*gang, (CSAlev(iang, ICOller(ilv), nejcec), ilv=31, MIN(its, 40))
                end do
                write (8, *) ' '
                write (8, 99041) 4, (POPlv(ICOller(ilv), nnurec), ilv=31, MIN(its, 40))
            end if
            write (8, *) ' '
        end if     ! (CSAlev(1,ICOller(2),nejcec)>0)
        99029   format(/' ', 40x, &
                'ELASTIC AND INELASTIC DIFFERENTIAL CROSS-SECTION', /, &
                ' ', 46x, '    (including compound + direct)', /, ' ' &
                ' ', 46x, ' (only discrete levels are listed)', /, ' ' &
                , 46x, 36('*'), /, ' ', 50x, 'CENTER-OF-MASS SYSTEM', &
                /)        
        99030   format('  Angle ', 10(6x, i2, '-level'))
        99032   format('        ', 10(5x, 'E=', f7.4))
        99034   format('        ', 10(4x, f4.1, '/', f5.4))
        99035   format(1x, f5.1, 3x, 11(2x, E12.6))
        99041   format(' TOT.INEL', I1, 1x, 11(E12.6, 2x))        
        return
    end subroutine printElasticAngDistr


    subroutine printInclusiveProductionXSec(nnuc)

        implicit none
        integer,intent(in) :: nnuc

        real*8 :: ftmp_disc
    
        write (12, *)
        write (8, *)
        write (8, '(1X,I3,''-'',A2,''-'',I3,'' production cross section '',G12.6, &
                    &'' mb  '',''      reac: '',A21)') iz, SYMb(nnuc), ia, CSPrd(nnuc), REAction(nnuc)
        write (12, '(1X,I3,''-'',A2,''-'',I3,'' production cross section'',G12.6, &
                    &''  mb '',''      reac: '',A21)') iz, SYMb(nnuc), ia, CSPrd(nnuc), REAction(nnuc)
        ftmp_disc = 0.d0
        if (nnuc == mt91)  ftmp_disc = CSDirlev(1, 1)
        if (nnuc == mt649) ftmp_disc = CSDirlev(1, 2)
        if (nnuc == mt849) ftmp_disc = CSDirlev(1, 3)
        if (nnuc == mt699) ftmp_disc = CSDirlev(1, 4)
        if (nnuc == mt749) ftmp_disc = CSDirlev(1, 5)
        if (nnuc == mt799) ftmp_disc = CSDirlev(1, 6)
        if ((CSPrd(nnuc) - CSPopul(nnuc) - ftmp_disc) > 0) then
            CSInc(nnuc) = max(CSPrd(nnuc) - CSPopul(nnuc) - ftmp_disc, 0.d0)
            write (12, '(1X,I3,''-'',A2,''-'',I3,'' inclusive  cross section'',G12.6, &
                  &''  mb '',''      reac: '',A21)') iz, SYMb(nnuc), ia, CSPrd(nnuc) - CSPopul(nnuc) - ftmp_disc, REAction(nnuc)
        else
            CSInc(nnuc) = 0.d0
            write (12, '(1X,I3,''-'',A2,''-'',I3,'' inclusive  cross section'',G12.6, &
                  & ''  mb '',''      reac: '',A21)') iz, SYMb(nnuc), ia, 0.d0, REAction(nnuc)
        end if
        checkprd = checkprd + CSFis
        xcross(NDEJC + 1, jz, jn) = CSFis
        return
    end subroutine printInclusiveProductionXSec


    subroutine widthFluctuation(nnuc)

        implicit none
        integer, intent(in) ::  nnuc
        integer :: m
        
        if (LHRtw > 0 .and. EINl > EHRtw) LHRtw = 0
        if (nnuc == 1 .and. EINl < EMAx_tlj) then
            if (LHRtw <= 2)                 CALL HRTW()     ! HF (LHRtw=0) or HRTW width fluctuation (LHRtw=1,2)
            if (LHRtw == 3 .or. LHRtw == 4) CALL Moldauer() ! Moldauer width fluctuation
            kemax = max(NEX(nnuc) - 1, 1)
            if (FISsil(nnuc) .and. NINT(FISshi(nnuc)) /= 1) then
                if (NINT(FISmod(nnuc)) == 0) then
                    write (80, *) 'csfis=', CSFis, ' mb'
                else
                    write (80, *) '  '
                    do m = 1, INT(FISmod(nnuc)) + 1
                        write (80, *) '    Mode=', m, '  csfis=', CSFism(m), ' mb'
                    end do
                end if
            end if
        end if
        return
    end subroutine widthFluctuation



    subroutine printIsomericCrossSections(nnuc)

        implicit none
        integer, intent(in) :: nnuc
        integer its, ilv
        real*8 :: ftmp_gs

        checkXS = checkXS + CSPrd(nnuc)
            checkprd = CSPrd(nnuc)
            xcross(NDEJC + 2, jz, jn) = CSPrd(nnuc)
            ilv = 0  ! count of meta-stable states
            ftmp_gs = CSPrd(nnuc)
            do its = NLV(nnuc), 2, -1
                if (ISIsom(its, Nnuc) == 1) then
                    ilv = ilv + 1
                    write (12, '(1X,I3,''-'',A2,''-'',I3, '' isomer state population  '',G12.6, &
                           & '' mb (m'',I1,'' E='',F10.6,'' MeV Jp='',F5.1,'')'')') &
                            iz, SYMb(nnuc), ia, POPlv(its, Nnuc), ilv, ELV(its, Nnuc), LVP(its, Nnuc)*XJLv(its, Nnuc)
                    ftmp_gs = ftmp_gs - POPlv(its, Nnuc)
                end if
            end do
            if (ilv > 0) write (12, '(1X,I3,''-'',A2,''-'',I3, '' ground state population  '',G12.6,'' mb'')') &
                iz, SYMb(nnuc), ia, ftmp_gs
    end subroutine printIsomericCrossSections

    subroutine integrateXSections(nnuc)
        !---------------------------------------------------------
        !  Calculates integrated cross sections for all ejectiles
        !  emitted from the nucleus 'nnuc'. Also prints spectra 
        !  by calling AUERST and prints energy-spin-parity dependent
        !  population cross sections by calling 
        !  printEnergySpinPoulations subroutine.
        !---------------------------------------------------------
    
        implicit none
        integer, intent(in) :: nnuc
        integer :: i, j, izares, iloc
        real*8 :: poplev, poptot

        ! Integral is calculated by trapezoidal rule being consistent with cross section
        if (IOUt > 0) CALL AUERST(nnuc, 0, 0)
        if (CSEmis(0, nnuc) > 0 .and. nnuc == 1) then
            write (12, '(10x, '' g  emission cross section'',G12.6,''  mb'')') CSEmis(0, nnuc)
            write (12, '(2x, '' Primary g  emission cross section'',G12.6,''  mb'')') cspg
        end if

        do nejc = 1, NEJcm
            ares = A(nnuc) - AEJc(nejc)
            zres = Z(nnuc) - ZEJc(nejc)
            ! Residual nuclei must be heavier than alpha
            if (ares <= 4 .and. zres <= 2.) cycle
            izares = INT(1000.0*zres + ares)
            CALL WHERE (izares, nnur, iloc)
            checkprd = checkprd + CSEmis(nejc, nnuc)
            xcross(nejc, jz, jn) = CSEmis(nejc, nnuc)
            if (iloc == 1 .and. CSEmis(nejc, nnuc) > 0) &
                write (12, '('' iloc=1! CSEmis('',i1,'','',i2,'')='',G12.5)') nejc, nnuc, CSEmis(nejc, nnuc)
            if (iloc == 1) cycle
            ! if(CSEmis(nejc,nnuc)<=1.d-8) cycle
            if (CSEmis(nejc, nnuc) <= 0) cycle
            !            write (12,
            !    &           '(11X,A2,'' emission cross section'',G12.6,''  mb'')')
            !    &             SYMbe(nejc), CSEmis(nejc,nnuc)
            if (ENDf(nnuc) == 1 .and. dabs(EINl - EDDfig) <= 1.d-5 .and. &
                AEJc(0) <= 4. .and. nejc == IDDfig(3)) CALL PLOT_EMIS_SPECTRA(nnuc, nejc)  ! excluding HI reactions
            ! Integral is calculated by trapezoidal rule being consistent with cross section
            if (IOUt > 0) CALL AUERST(nnuc, nejc, 0)
            ! Print residual nucleus population
            poptot = 0.d0
            if (NEX(nnur) > 0) then !avoid summing non-existent continuum
                do j = 1, NLW
                    do i = 1, NEX(nnur)
                        poptot = poptot + POP(i, j, 1, nnur) + POP(i, j, 2, nnur)
                    end do
                    poptot = poptot - 0.5*(POP(1, j, 1, nnur) + POP(1, j, 2, nnur)) - 0.5*(POP(NEX(nnur), j, 1, nnur) &
                                                                                           + POP(NEX(nnur), j, 2, nnur))
                end do
            end if
            poptot = poptot*DE
            poplev = 0.d0
            do i = 1, NLV(nnur)
                poplev = poplev + POPlv(i, nnur)
            end do
            if (LHMs /= 0) then
                if (nejc > 2) then
                    poptot = poptot - POPcon(nnur)
                    poplev = poplev - POPdis(nnur)
                else
                    poptot = poptot + CSHms(1, nnur) + CSHms(2, nnur)
                end if
            end if
            !            if(A(nnuc)==A(1) .and. Z(nnuc)==Z(1)
            !    &         .and. ENDF(nnuc)==1) then
            !              write (12,
            !    &            '(13x,   '' total population      '',G12.6,''  mb'')')
            !    &            poplev + poptot
            !              write (12,
            !    &            '(13x,   '' total popul.continuum '',G12.6,''  mb'')')
            !    &            poptot
            !              write (12,
            !    &            '(13x,   '' total popul.disc.lev. '',G12.6,''  mb'')')
            !    &            poplev
            !
            !              write (8,*) '    RESIDUAL = TARGET NUCLEUS'
            !              write (8,
            !    &         '(1x,''    Total population      '',G12.6,''  mb'')')
            !    &          poplev + poptot
            !              write (8,
            !    &         '(1x,''    Total popul.continuum '',G12.6,''  mb'')')
            !    &          poptot
            !              write (8,
            !    &         '(1x,''    Total popul.disc.lev. '',G12.6,''  mb'')')
            !    &          poplev
            !              write (8,*)
            !            endif
            if (IOUt == 4) call printEnergySpinPoulations(nnur)
        end do   !over ejectiles

        if (DBRkup > 1.0d-2 .and. jz == 0 .and. jn == 0) checkprd = checkprd - CSDbrkup(1)
        xcross(NDEJC + 3, jz, jn) = checkprd
        return
    end subroutine integrateXSections


    subroutine printEnergySpinPoulations(nnur)
        implicit none
        integer, intent(in) :: nnur
        integer :: i, j
        ia = INT(A(nnur))
        write (8, *) ' '
        write (8, *) '**************************** '
        write (8, '('' Residual nucleus '',I3,''-'',A2,/)') ia, SYMb(nnur)
        write (8, '('' Positive parities population'',/)')
        do i = NEX(nnur), 1, -1
            ftmp = 0.d0
            do j = 1, 12
                ftmp = ftmp + POP(i, j, 1, nnur)
            end do
            if (ftmp > 0.d0) &
                write (8, 99075) EX(i, nnur), (POP(i, j, 1, nnur), j=1, 12)
        end do
        write (8, *) ' '
        write (8, '('' Negative parities population'',/)')
        do i = NEX(nnur), 1, -1
            ftmp = 0.d0
            do j = 1, 12
                ftmp = ftmp + POP(i, j, 2, nnur)
            end do
            if (ftmp > 0.0) write (8, 99075) EX(i, nnur), (POP(i, j, 2, nnur), j=1, 12)
        end do
        write (8, '('' '')')
        99075   format(1X, F5.2, 12E10.3)

    end subroutine printEnergySpinPoulations


    subroutine printPrimaryGammas(nnuc)
 
        integer, intent(in) :: nnuc   ! index of the nucleus (actually 1)
        
        integer il
        cspg = 0.d0
        do il = 1, NLV(nnuc)
            cspg = cspg + CSEpg(il)
        end do
        if (cspg > 0) then
            write (12, *)
            write (12, '(1X,/,10X,40(1H-),/)')
            write (12, '(2x, '' Primary g  emission cross section'',G12.6,''  mb'')') cspg
            write (12, '(1X,/,10X,40(1H-),/)')
            write (12, '(11x,A1,A12,A6,A5,4x,A12,A7,1x,A6,1x,A10)') 'i', '    Elv(i)  ', 'Par  ', ' Spin', &
                ' Prim.g CS   ', ' Branch', '  Egamma  '
            write (12, *) ' '
            do il = 1, NLV(nnuc)
                write (12, 99910) il, ELV(il, nnuc), LVP(il, nnuc), XJLv(il, nnuc), CSEpg(il), CSEpg(il)/cspg*100., ENPg(il)
                99910 format(I12, F10.6, I5, F8.1, G15.6, 1x, F6.2, 1x, F10.6)
            end do
            write (12, '(1X,/,10X,40(1H-),/)')
            write (12, *)
            write (8, *)
            !  write (8,'(1X,/,10X,40(1H-),/)')
            write (8, '(2x, '' Primary g  emission cross section'',G12.6,''  mb'')') cspg
            write (8, '(1X,/,10X,40(1H-),/)')
            write (8, '(11x,A1,A12,A6,A5,4x,A12,A7,1x,A6,1x,A10)') 'i', '    Elv(i)  ', 'Par  ', ' Spin', &
                ' Prim.g CS   ', ' Branch', '  Egamma  '
            write (8, *) ' '
            do il = 1, NLV(nnuc)
                write (8, 99910) il, ELV(il, nnuc), LVP(il, nnuc), XJLv(il, nnuc), CSEpg(il), CSEpg(il)/cspg*100., ENPg(il)
            end do
            write (8, '(1X,/,10X,40(1H-),/)')
            write (8, *)
        end if
    end subroutine printPrimaryGammas


    subroutine writeElastic(totcorr)

        real*8, intent(in) :: totcorr 
        integer :: j, ncollx, nejcec, its, ilevcol, imint, imaxt, ilv, iloc
        real*8 :: delang, angstep
        
        ! ELCncs = CSDirsav(LEVtarg,NPRoject)/PIx4 ! isotropic
        ! write(*,*) 'ELCncs =', ELCncs ! CSDirsav(LEVtarg,NPRoject)
        write (8, *)
        write (8, *) ' Incident energy (CMS)      ', sngl(EIN), ' MeV'
        write (8, *) ' Shape elastic cross section', sngl(ELAred*ELAcs), ' mb'
        write (8, *) ' CN elastic cross section   ', sngl(PIx4*ELCncs), ' mb'
        write (12, *) ' '
        write (12, &
               '('' ELASTIC CROSS SECTION= '',1P,E12.5,'' mb'')') ELAcs*ELAred + PIx4*ELCncs
        write (12, *) ' '
        write (12, '('' SHAPE ELASTIC CROSS SECTION= '',1P,E12.5, '' mb'')') ELAcs*ELAred
        write (12, *) ' '
        write (12, &
               '('' COMP. ELASTIC CROSS SECTION= '',1P,E12.5, '' mb'')') PIx4*ELCncs
        write (12, *) ' '
        write (12, *) ' Elastic angular distribution '
        write (12, *) ' '
        if (ICAlangs > 0) then
            write (12, '(10X,8G15.5)') (ANGles(iang), iang=1, NANgela)
        else
            delang = 180.d0/FLOAT(NANgela - 1)
            write (12, '(10X,8G15.5)') (FLOAT(iang - 1)*delang, iang=1, NANgela)
        end if
        if (.not. CN_isotropic .and. ELCncs < 0.05d0) then
            CN_isotropic = .TRUE.
            write (8, *)
            write (8, *) ' CN angular distribution assumed isotropic at Einc = ', sngl(EINl)
            write (8, *)
        end if
        if (CN_isotropic .or. PL_CN(0, LEVtarg) <= 0.d0) then
                write (8, *) &
                    ' Isotropic Elastic=', sngl(ELCncs), ' mb/str'
                do iang = 1, NDANG
                    cel_da(iang) = ELCncs ! isotropic
                end do
                write (12, '(9X,8E15.5)') ((ELAred*elada(iang) + cel_da(iang)), iang=1, NANgela)
                write (12, *) ' '
                write (12, *) ' '
                write (12, *) ' Legendre coefficients expansion '
                write (12, *) ' '
                write (12, '(1x,A7,I5)') ' Lmax =', min(NDAng, neles)
                write (12, *) ' '
                write (12, '(9X,8D15.8)') (ELAred*elleg(1) + ELCncs), (ELAred*elleg(iang), iang=2, min(NDAng, neles))
                write (12, *) ' '
        else
            xs_norm = ELCncs/PL_CN(0, LEVtarg)
                write (8, *) ' CN elas. cross section (BB)', sngl(PIx4*PL_CN(0, LEVtarg)), ' mb'
            if (INTerf == 1) then
                write (110, '(1x,E12.5,3x,11(F9.2,1x),A17)') EINl, PIx4*ELCncs, (PIx4*xs_norm*PL_CN(0, ilevcol) &
                                                            , ilevcol=1, 10), 'ENG-WEID. TRANSF.'
            else
                write (110, '(1x,E12.5,3x,11(F9.2,1x))') EINl, PIx4*ELCncs, (PIx4*xs_norm*PL_CN(0, ilevcol), ilevcol=1, 10)
            end if
            write (8, *)
            write (8, *) ' Nonisotropic Compound to discrete levels including the Compound Elastic'
            write (8, *)
            if (DABS(xs_norm - 1.d0) > 1.d-4) then
                write (8, *) ' EMPIRE CN ELCncs= ', 4*PI*ELCncs
                write (8, *) ' PL_CN(0,LEVtarg)=', 4*PI*PL_CN(0, LEVtarg)
                write (8, *) ' Renormalizing CN Ang.Dist. by ELCncs/PL_CN(0,LEVtarg)=', sngl(xs_norm)
                write (8, *)
            end if
            do iang = 1, NDANG
                cel_da(iang) = xs_norm*GET_DDXS(CANGLE(iang), LEVtarg)
            end do
            !--------------PRINT compound elastic
            angstep = 180.d0/(NANgela - 1)
            write (8, 99016)
            write (8, 99020)
            do iang = 1, NANgela/4 + 1
                imint = 4*(iang - 1) + 1
                imaxt = MIN0(4*iang, NANgela)
                write (8, 99025) ((j - 1)*angstep, cel_da(j), j=imint, imaxt)
            end do
            write (8, *)
            write (12, '(9X,8E15.5)') ((ELAred*elada(iang) + cel_da(iang)), iang=1, NANgela)
            write (12, *) ' '
            write (12, *) ' '
            write (12, *) ' Legendre coefficients expansion'
            write (12, *) ' '
            write (12, '(1x,A7,I5)') ' Lmax =', min(NDAng, neles)
            write (12, *) ' '
            write (12, '(9X,8D15.8)') ELAred*elleg(1) + xs_norm*PL_CN(0, LEVtarg), (ELAred*elleg(iang) &
                                                          + xs_norm*PL_CN(iang - 1, LEVtarg), iang=2, min(NDAng, neles))
            write (12, *) ' '
            write (12, *) ' DIR Legendre coefficients expansion'
            write (12, *) ' '
            write (12, '(1x,A7,I5)') ' Lmax =', min(NDAng, neles)
            write (12, *) ' '
            write (12, '(9X,8D15.8)') ELAred*elleg(1), (ELAred*elleg(iang), iang=2, min(NDAng, neles))
            write (12, *) ' '
            write (12, *) ' CE Legendre coefficients expansion'
            write (12, *) ' '
            write (12, '(1x,A7,I5)') ' Lmax =', min(NDAng, PL_lmax(LEVtarg))
            write (12, *) ' '
            write (12, '(9X,8D15.8)') xs_norm*PL_CN(0, LEVtarg), (xs_norm*PL_CN(iang - 1, 1), iang=2, &
                                                                  min(NDAng, PL_lmax(LEVtarg)))
        end if
        if (FITomp < 0) then
            write (40, '(F12.4,3D12.5)') EINl, TOTcs*TOTred*totcorr, ABScs*FUSred
            if (ncollx > 0) then
                !---------------locate position of the projectile among ejectiles
                CALL WHEREJC(IZAejc(0), nejcec, iloc)
                its = ncollx
                write (40, '(12x,11D12.5)') ELAred*ELAcs + PIx4*ELCncs, (CSDirlev(ICOller(ilv), nejcec), &
                                            ilv=2, MIN(its, 10))
                if (ICAlangs > 0) then
                    do iang = 1, NDANG
                        write (40, '(f12.4,11D12.5)') ANGles(iang), ELAred*elada(iang) + cel_da(iang), &
                                                    (CSAlev(iang, ICOller(ilv), nejcec), ilv=2, MIN(its, 10))
                    end do
                end if
            else
                write (40, '(12x,11D12.5)') &
                    ELAred*ELAcs + PIx4*ELCncs
                if (ICAlangs > 0) then
                    do iang = 1, NDANG
                        write (40, '(f12.4,11D12.5)') ANGles(iang), ELAred*elada(iang) + cel_da(iang)
                    end do
                end if
            end if
        end if ! FITomp < 0
        return
        99016   format(/' ', 46x, 'COMP. ELASTIC DIFFERENTIAL CROSS-SECTION', /, ' ', 46x, 40('*'), /, ' ', 50x, &
                      'CENTER-OF-MASS SYSTEM',/)
        99020   format(' ', 5x, 4('    TETA ', 2x, 'D.SIGMA/D.OMEGA', 6x),/)
        99025   format(' ', 5x, 4(1p, e12.5, 2x, e12.5, 6x))
    end subroutine writeElastic

    subroutine discLevelAngularDistr()
        implicit none
        !
        ! Sum angular distributions for discrete levels 
        !
        integer :: il, na
        real*8 :: xs_cn

        do nejc = 1, NEJcm
            nnur = NREs(nejc)
            if (nnur < 0) cycle
            ares = A(1) - AEJc(nejc)
            zres = Z(1) - ZEJc(nejc)
            ! residual nuclei must be heavier than alpha
            if (ares <= 4 .and. zres <= 2.) cycle
        do il = 1, NLV(nnur)
                ftmp = CSComplev(il, nejc)
                if (ftmp <= 0) cycle
                if ((nejc == NPRoject) .and. (.not. CN_isotropic) .and. (PL_CN(0, il) > 0)) then
                    xs_norm = ftmp/(4*PI*PL_CN(0, il))
                    do na = 1, NDANG
                        xs_cn = GET_DDXS(CANGLE(na), il)
                        CSAlev(na, il, nejc) = CSAlev(na, il, nejc) + xs_cn*xs_norm
                    end do
                else
                ! Not the inelastic channel OR isotropic CN DA
                    xs_cn = ftmp/PIx4  ! default isotropic
                    do na = 1, NDANG
                        CSAlev(na, il, nejc) = CSAlev(na, il, nejc) + xs_cn
                    end do ! loop over angles
                end if
            end do  !loop over levels
        end do  ! loop over emitted particles
        return
    end subroutine discLevelAngularDistr


    subroutine printCompoudHeader(nnuc)
        !
        !!        p r i n t N u c 1 H e a d e r
        !
        !! Prints to 8 and 12 CN header and spin distribution of the capture state
        ! 

        integer, intent(in) :: nnuc   ! index of the nucleus (actually 1) 
        integer :: i

        ! ia = INT(A(nnuc))
        write (8, *)
        write (8, *) &
            ' ---------------------------------------------------------------'
        if (abs(QPRod(nnuc) + ELV(LEVtarg, 0)) > 99.99) then
            write (8, &
                   '(i3,''  Decaying nucleus '',I3,''-'',A2,''-'',I3,     ''  mass='' &
                  &,F10.6,'' Q-value='',F10.5,''      reaction: '',A21)') &
                    nnuc, INT(Z(nnuc)), SYMb(nnuc), ia, AMAss(nnuc), &
                    QPRod(nnuc) + ELV(LEVtarg, 0), REAction(nnuc)
        else
            write (8, '(i3,''  Decaying nucleus '',I3,''-'',A2,''-'',I3,     ''  mass='', &
                  & F10.6,'' Q-value='',F10.5,''      reaction: '',A21)') &
                  nnuc, INT(Z(nnuc)), SYMb(nnuc), ia, AMAss(nnuc), &
                  QPRod(nnuc) + ELV(LEVtarg, 0), REAction(nnuc)
        end if
        write (8, *) &
            ' ---------------------------------------------------------------'
        write (8, *)

        if (IOUt > 1) then
            write (8, *)
            write (8, '(''  Compound nucleus '',I3,''-'',A2, '' spin distribution'')') NINT(A(1)), SYMb(1)
            write (8, *) ' -----------------------------------------'
            write (8, *) ' '
            do i = 1, NLW
                if (MOD(ia, 2) == 0) then
                    write (8, '(1X,SP,I5,SS,G12.5,5X,I5,G12.5)') i - 1, POP(NEX(1), i, 1, 1), -(i - 1), POP(NEX(1), i, 2, 1)
                else
          write (8, '(1X,SP,I4,SS,''/2'',G12.5,5X,I4,''/2'',G12.5)') 2*i - 1, POP(NEX(1), i, 1, 1), -(2*i - 1), POP(NEX(1), i, 2, 1)
                end if
            end do
            write (8, *) ' '
        end if
        write (12, *) &
            ' ---------------------------------------------------------------'
        if (abs(QPRod(nnuc) + ELV(LEVtarg, 0)) > 99.99) then
            write (12, &
                   '(''  Decaying nucleus '',I3,''-'',A2,''-'',I3,     ''  mass='',F10.6,'' Q-value='',F10.5)') &
                    INT(Z(nnuc)), SYMb(nnuc), ia, AMAss(nnuc), QPRod(nnuc) + ELV(LEVtarg, 0)
        else
            write (12, '(''  Decaying nucleus '',I3,''-'',A2,''-'',I3,     ''  mass='',F10.6,'' Q-value='',F10.6)') &
                        INT(Z(nnuc)), SYMb(nnuc), ia, AMAss(nnuc), QPRod(nnuc) + ELV(LEVtarg, 0)
        end if
        write (12, *) &
            ' ---------------------------------------------------------------'
    end SUBROUTINE printCompoudHeader


    subroutine getRecoil(Ke, Nnuc)
        !cc
        !cc   ********************************************************************
        !cc   *                                                         class:ppu*
        !cc   *                         RECOIL                                   *
        !cc   *                                                                  *
        !cc   *                                                                  *
        !cc   *  Constructs recoil spectra:                                      *
        !cc   *  Each excitation bin is given recoil spectrum, when a particle   *
        !cc   *  is emitted its recoil velocity is vector added to the parent    *
        !cc   *  recoil velocity and a resulting spectrum is summed upon daughter*
        !cc   *  recoil spectrum corresponding to the populated energy bin in the*
        !cc   *  daughter (kinematical normalization taken into account).        *
        !cc   *  Daughter recoil spectra will be distributed between             *
        !cc   *  adjacent bins (inversly proportional to the                     *
        !cc   *  distance of the actual energy to the bin energy                 *
        !cc   *  in order to conserve energy).                                   *
        !cc   *  Requires that continuum spectra from each bin are stored on the *
        !cc   *  AUSpec array and those to discrete levels on the REClev for each*
        !cc   *  reaction mechanism considered in the calculations.              *
        !cc   *                                                                  *
        !cc   ********************************************************************
        !cc
        INCLUDE 'dimension.h'
        INCLUDE 'global.h'
 
        ! Dummy arguments
 
        INTEGER Ke, Nnuc

        ! Local variables

        DOUBLE PRECISION coeff, dang, erecejc, erecod, erecoil, erecpar, &
            exqcut, recorr, sumnor, weight, ares, zres, csmsdl, ftmp
        INTEGER icse, ie, il, ire, irec, na, nejc, nnur, izares, iloc
        !
        !
        !-----Normalize recoil spectrum of the parent
        !     sumnor = 0.d0
        !     do ire = 1, NDEREC
        !        sumnor = sumnor + RECcse(ire,Ke,Nnuc)
        !     ENDDO
        !     if (sumnor/=0.0D0) then
        !        do ire = 1, NDEREC
        !           RECcse(ire,Ke,Nnuc) = RECcse(ire,Ke,Nnuc)/sumnor
        !        ENDDO
        !     ENDIF
        sumnor = SUM(RECcse(:, Ke, Nnuc))
        if (sumnor > 0) RECcse(:, Ke, Nnuc) = RECcse(:, Ke, Nnuc)/sumnor

        dang = PI/FLOAT(NDANG - 1)
        coeff = dang/DERec/2.0
        do nejc = 1, NEJcm   !over ejectiles
            ares = A(nnuc) - AEJc(nejc)
            zres = Z(nnuc) - ZEJc(nejc)
            !--------Residual nuclei must be heavier than alpha
            if (ares <= 4 .and. zres <= 2.) cycle
            izares = INT(1000.0*zres + ares)
            CALL WHERE (izares, nnur, iloc)
            if (iloc == 1) cycle
            !--------Decay to continuum
            !--------recorr is a recoil correction factor that
            !--------divides outgoing energies
            !        recorr = DBLE(ares)/AEJc(nejc)
            recorr = AMAss(Nnuc)/EJMass(nejc)

            exqcut = EX(Ke, Nnuc) - Q(nejc, Nnuc) - ECUt(nnur)
            do ie = 1, NDECSE !over ejec. energy (daughter excitation)
                icse = int((exqcut - (ie - 1)*DE)/DE + 1.001)
                !-----------Daughter bin
                if (icse <= 0) EXIT

                erecejc = (ie - 1)*DE/recorr
                do ire = 1, NDEREC          !over recoil spectrum         ! Bug 07/      !21
                    ftmp = RECcse(ire, Ke, Nnuc)*AUSpec(ie, nejc)*coeff !*recorr
                    if (ftmp > 0) then
                        erecpar = (ire - 1)*DERec
                        do na = 1, NDANG
                            erecoil = erecejc + erecpar + 2.0*SQRT(erecejc*erecpar)*CANgler(na)
                            irec = int(erecoil/DERec + 1.001)
                            weight = (erecoil - (irec - 1)*DERec)/DERec
                            csmsdl = ftmp*SANgler(na)
                            if (irec > NDEREC) EXIT
                            RECcse(irec, icse, nnur) = RECcse(irec, icse, nnur) + csmsdl*(1.0 - weight)
                            if (irec + 1 > NDEREC) EXIT
                            RECcse(irec + 1, icse, nnur) = RECcse(irec + 1, icse, nnur) + csmsdl*weight
                        end do                  !over angles
                    end if
                end do                  !over recoil spectrum
            end do                  !over  daugther excitation

            !--------Decay to discrete levels (stored with icse=0)
            exqcut = exqcut + ECUt(nnur)
            do il = 1, NLV(nnur)
                erecod = exqcut - ELV(il, nnur)   !emission energy
                if (erecod < 0) EXIT
                erecod = erecod/recorr
                do ire = 1, NDEREC      !over recoil spectrum
                    ftmp = RECcse(ire, Ke, Nnuc)*REClev(il, nejc)*coeff
                    if (ftmp > 0) then
                        erecpar = (ire - 1)*DERec
                        do na = 1, NDANG !over angles
                            erecoil = erecpar + erecod + 2.0*SQRT(erecpar*erecod)*CANgler(na)
                            irec = int(erecoil/DERec + 1.001)
                            weight = (erecoil - (irec - 1)*DERec)/DERec
                            if (irec > NDEREC) EXIT
                            RECcse(irec, 0, nnur) = RECcse(irec, 0, nnur) + ftmp*(1.d0 - weight)*SANgler(na)
                            !------------------------
                            ! if(irec==5 .and. RECcse(irec,0,nnur)>0
                            ! &               .and.na==10) then
                            ! write(8,*) '       Parent bin', Ke, 'Nnuc', Nnuc
                            ! write(8,*) 'Recoil bin', ire
                            ! write(8,*) 'Erecoil ', erecoil, erecod, nnuc
                            ! write(8,*) 'RECcse, RECcse par, REClev',
                            ! &            RECcse(irec,0,nnur),RECcse(ire,Ke,Nnuc),
                            ! &            REClev(il,nejc)
                            ! ENDIF
                            !------------------------
                            if (irec + 1 > NDEREC) EXIT
                            RECcse(irec + 1, 0, nnur) = RECcse(irec + 1, 0, nnur) + ftmp*weight*SANgler(na)
                        end do                  !over angles
                    end if
                end do                  !over recoil spectrum
            end do                  !over levels

        end do                  !over ejectiles
        !-----
        !-----Parent recoil spectrum after gamma decay
        !-----
        nnur = Nnuc
        nejc = 0
        !-----gamma decay to continuum
        do ie = 1, NDECSE !over ejec. energy (daughter excitation)
            icse = int((EX(Ke, Nnuc) - (ie - 1)*DE - ECUt(nnur))/DE + 1.001)
            if (icse < 0) EXIT
            !-------!daughter bin
            do irec = 1, NDEREC         !over recoil spectrum
                ftmp = RECcse(irec, Ke, Nnuc)*AUSpec(ie, 0)/DERec
                if (ftmp > 0) RECcse(irec, icse, nnur) = RECcse(irec, icse, nnur) + ftmp
            end do                  !over recoil spectrum
        end do                  !over  daugther excitation

        !-----gamma decay to discrete levels (stored with icse=0)
        do il = 1, NLV(nnur)
            do ire = 1, NDEREC             !over recoil spectrum
                ftmp = RECcse(ire, Ke, Nnuc)*REClev(il, nejc)/DERec
                if (ftmp > 0) RECcse(ire, 0, nnur) = RECcse(ire, 0, nnur) + ftmp
            end do                  !over recoil spectrum
        end do                  !over levels
        RETURN
    end subroutine getRecoil

    subroutine printRecoil(Nnuc, React) !,qout)
        !-----
        !-----Prints recoil spectrum of nnuc residue
        !-----
        implicit none
        INCLUDE 'dimension.h'
        INCLUDE 'global.h'
        !
        ! Dummy arguments
        !
        INTEGER Nnuc
        CHARACTER*21 React
        !     DOUBLE PRECISION qout
        !
        ! Local variables
        !
        !     DOUBLE PRECISION csum,ftmp,corr,xsdisc,esum,recorr,cmul,stmp
        DOUBLE PRECISION csum, ftmp, corr, xsdisc, esum, cmul, stmp
        DOUBLE PRECISION dtmp, sstmp
        INTEGER ie, ilast

        !     if (CSPrd(Nnuc)<=CSMinim.or.NINT(A(Nnuc))==NINT(A(1))) RETURN
        if (CSPrd(Nnuc) <= 0.d0 .or. NINT(A(Nnuc)) == NINT(A(1))) RETURN
        !-----Normalize recoil spectra to remove eventual inaccuracy
        !-----due to numerical integration of angular distributions
        !-----and find last non-zero cross section for printing
        !     recorr = A(Nnuc)/(A(1)-A(Nnuc))
        !
        !     To get consistent integral value
        !
        RECcse(1, 0, Nnuc) = RECcse(1, 0, Nnuc)*2.d0

        csum = 0.d0
        esum = 0.d0
        ilast = 0
        do ie = 1, NDEREC
            ftmp = RECcse(ie, 0, Nnuc)
            if (ftmp > 0) then
                csum = csum + ftmp
                esum = esum + ftmp*FLOAT(ie - 1)*DERec !/recorr !bug 07/21
                ilast = ie
            end if
        end do

        if (csum <= CSMinim*0.001d0) RETURN
        !     if(csum<=0.d0) RETURN

        !     ilast = MIN(ilast + 1,NDEX)
        ilast = MIN(ilast + 1, NDEREC)

        if (ilast > 1) then
            csum = csum - &
                   0.5d0*(RECcse(1, 0, Nnuc) + RECcse(ilast, 0, Nnuc))
            esum = esum - RECcse(ilast, 0, Nnuc)*0.5d0*FLOAT(ilast - 1)*DERec !/recorr  !bug 07/21
        end if
        !
        !     recoil correction added by RCN, 01/2014
        !
        write (12, *) ' '
        if (ENDf(nnuc) == 2) then
        write (12, '(A23,A7,A4,I6,A6,F12.5,1x,6H(incl))') '  Spectrum of recoils  ', React, 'ZAP=', IZA(Nnuc), ' mass=', AMAss(Nnuc)
        else
        write (12, '(A23,A7,A4,I6,A6,F12.5            )') '  Spectrum of recoils  ', React, 'ZAP=', IZA(Nnuc), ' mass=', AMAss(Nnuc)
        end if

        write (12, *) ' '
        write (12, '(''    Energy    mb/MeV'')')
        write (12, *) ' '

        dtmp = 1.d0
        if (ENDF(nnuc) == 2) dtmp = CSInc(nnuc)/(csum*DERec)

        sstmp = 0.d0
        do ie = 1, ilast
            stmp = RECcse(ie, 0, Nnuc)*dtmp
            if (stmp <= 0 .and. ie /= ilast) cycle
            write (12, '(F10.6,E14.5)') FLOAT(ie - 1)*DERec, stmp !*recorr  !bug 0      !7/21
            sstmp = sstmp + stmp
        end do

        write (12, '(/2x,''Ave.  E  of recoil spectrum   '',G12.6,'' MeV  for '', I3,''-'',A2,''-'',I3,A21)') esum/csum, &
            INT(Z(nnuc)), SYMb(nnuc), INT(A(nnuc)), REAction(nnuc)

        xsdisc = 0.d0
        if (nnuc == mt91) xsdisc = CSDirlev(1, 1)
        if (nnuc == mt649) xsdisc = CSDirlev(1, 2)
        if (nnuc == mt849) xsdisc = CSDirlev(1, 3)
        if (nnuc == mt699) xsdisc = CSDirlev(1, 4)
        if (nnuc == mt749) xsdisc = CSDirlev(1, 5)
        if (nnuc == mt799) xsdisc = CSDirlev(1, 6)
        if (ENDf(nnuc) <= 1) then
            cmul = csum*DERec/(CSPrd(nnuc) - xsdisc)  ! multiplicity
        !       qout = qout + cmul*esum/csum            ! multiplicity x <E>
        !       write(12,
        !    &  '( 2x,''Ave. <Q> of recoil spectrum   '',G12.6,'' MeV'')')
        !    &     cmul*esum/csum
            write (12, '(2x,''Recoil multiplicity          '',G12.6)') cmul
            write (12, '( 2x,''Integral of recoil spectrum   '',G12.6,'' mb'' )') csum*DERec
        else
            write (12, '( 2x,''Integral of recoil spectrum   '',G12.6, '' mb (incl)'' )') sstmp*DERec
        end if

        if (xsdisc > 0.d0 .and. ENDf(nnuc) == 1) then
            write (12, 97547) CSPrd(nnuc) - xsdisc
            write (12, 97548) xsdisc
            !       write (12,'(2X,''Cont. popul. before g-cascade '',
            !    &         G12.6,'' mb'')')
            !       write (12,'(2X,''Disc. popul. before g-cascade '',
            !    &                G12.6,'' mb'')') xsdisc
        end if

        if (ENDF(nnuc) == 2) then
            corr = 1.d0
            if (sstmp > 0) corr = CSInc(Nnuc)/(sstmp*DERec)
            write (12, &
                   '( 2x,''Prod. cross sect. (continuum) '',G12.6,'' mb (incl)'')') CSInc(Nnuc)
        else
            ! corr = (CSPrd(Nnuc)-xsdisc)/(csum*DERec)
            corr = CSPrd(Nnuc)/(csum*DERec)
            write (12, '( 2x,''Prod. cross sect. (continuum) '',G12.6,'' mb'' )') CSPrd(Nnuc) - xsdisc
            write (12, '( 2x,''Prod. cross sect. (disc+cont) '',G12.6,'' mb'' )') CSPrd(Nnuc)
        end if

        if (ENDF(nnuc) == 2) then
            write (12, &
                   '( 2x,''Ratio continuum  XS/Recoil XS '',G12.6,''    (incl)'')') corr
        else
            write (12, '( 2x,''Ratio continuum  XS/Recoil XS '',G12.6)') corr
            write (12, '( 2x,''Ratio Production XS/Recoil XS '',G12.6)') CSPrd(Nnuc)/(csum*DERec)
        end if
        write (12, *)
        write (12, *)

        if (ABS(1.d0 - corr) > 0.10D0 .and. CSPrd(Nnuc) > 0.001D0) then
            write (8, *)
            write (8, *) ' ******'
            write (8, *) ' WARNING:  Ein = ', sngl(EIN), ' MeV'
            write (8, *) ' WARNING:  ZAP = ', IZA(Nnuc), ' from ', React
            write (8, *) ' WARNING: x-section balance in recoils >10%, ratio=', corr
            write (8, *) ' WARNING: recoil integral     = ', sngl(csum*DERec), ' mb'
            write (8, *) ' WARNING: Prod. cross section = ', sngl(CSPrd(Nnuc)), ' mb'
            if (ENDf(nnuc) == 1) then
                write (8, *) ' WARNING: Cont. cross section = ', sngl(CSPrd(Nnuc) - xsdisc), ' mb'
                write (8, *) ' WARNING: Discr.cross section = ', sngl(xsdisc), ' mb'
            end if
        end if
        97547 format(3X, ' Cont. popul. with cont. g-casc. ', G12.6, ' mb')
        97548 format(3X, ' Disc. popul. with cont. g-casc. ', G12.6, ' mb')
        RETURN
    end subroutine printRecoil

    subroutine printBinaryRecoil(Nnuc, React) !,qout)
        !-----
        !-----Prints recoil spectrum of (n,n) or (n,p) residue
        !-----
        implicit none
        INCLUDE 'dimension.h'
        INCLUDE 'global.h'
        !
        ! Dummy arguments
        !
        INTEGER Nnuc
        CHARACTER*21 React
        !     DOUBLE PRECISION qout
        !
        ! Local variables
        !
        INTEGER ie, ilast, ipart
        DOUBLE PRECISION csum, corr, xsdisc, ftmp, esum, recorr, cmul, stmp

        ipart = 1  !neutron
        if (IZA(1) - IZA(Nnuc) == 1001) ipart = 2    !proton

        !-----Find last non-zero cross section for printing
        csum = 0.d0
        esum = 0.d0
        ilast = 0
        !     recorr = A(Nnuc)/(A(1)-A(Nnuc))
        recorr = AMAss(Nnuc)/EJMass(ipart)
        do ie = 1, NDEX
            ftmp = POPcse(0, ipart, ie, INExc(Nnuc))
            if (ftmp > 0) then
                csum = csum + ftmp
                esum = esum + ftmp*FLOAT(ie - 1)*DE/recorr
                ilast = ie
            end if
        end do
        !     if (A(Nnuc)==A(1)-1 .and. Z(Nnuc)==Z(1))
        !    & write(*,*) '  Spectrum of recoils  ',
        !    &       React, 'ZAP=', IZA(Nnuc), ' csum=', csum

        !     if (csum<=0.d0 .or. ilast==0 .or. A(Nnuc)==A(1)) RETURN
        if (csum <= CSMinim*0.001d0 .or. ilast == 0 .or. A(Nnuc) == A(1)) RETURN

        ilast = MIN(ilast + 1, NDEX)
        !     ilast = MIN(ilast + 1,NDEREC)

        if (ilast > 1) then
            csum = csum - 0.5d0*(POPcse(0, ipart, 1, INExc(Nnuc)) + POPcse(0, ipart, ilast, INExc(Nnuc)))
            esum = esum - POPcse(0, ipart, ilast, INExc(Nnuc))*0.5d0*FLOAT(ilast - 1)*DE/recorr
        end if
        !-----correction factor multiplying cross sections and dividing DE is
        !-----simply A(1) since ejectile mass is here always 1 (neutron or proton)
        write (12, *) ' '

        if (ENDf(nnuc) == 2) then
        write (12, '(A23,A7,A4,I6,A6,F12.5,1x,6H(incl))') '  Spectrum of recoils  ', React, 'ZAP=', IZA(Nnuc), ' mass=', AMAss(Nnuc)
        else
        write (12, '(A23,A7,A4,I6,A6,F12.5            )') '  Spectrum of recoils  ', React, 'ZAP=', IZA(Nnuc), ' mass=', AMAss(Nnuc)
        end if
        write (12, *) ' '
        write (12, '(''    Energy    mb/MeV'')')
        write (12, *) ' '

        do ie = 1, ilast
            stmp = POPcse(0, ipart, ie, INExc(Nnuc))
            if (stmp <= 0 .and. ie /= ilast) cycle
            write (12, '(F10.6,E14.5)') FLOAT(ie - 1)*DE/recorr, stmp*recorr
        end do

        write (12, '(/2x,''Ave.  E  of recoil spectrum   '',G12.6,'' MeV  for '', &
              & I3,''-'',A2,''-'',I3,A21)') esum/csum, INT(Z(nnuc)), SYMb(nnuc), INT(A(nnuc)), REAction(nnuc)

        xsdisc = 0.d0
        if (nnuc == mt91) xsdisc = CSDirlev(1, 1)
        if (nnuc == mt649) xsdisc = CSDirlev(1, 2)

        cmul = csum*DE/(CSPrd(nnuc) - xsdisc)     ! multiplicity
        !     qout = qout + cmul*esum/csum            ! multiplicity x <E>
        !     write(12,
        !    &  '( 2x,''Ave. <Q> of recoil spectrum   '',G12.6,'' MeV'')')
        !    &     cmul*esum/csum
        write (12, '(2x,''Recoil multiplicity (binary) '',G12.6)') cmul
        write (12, *)
        write (12, '( 2x,''Integral of recoil spectrum   '',G12.6,'' mb'' )') csum*DE

        if (xsdisc > 0.d0 .and. ENDf(nnuc) == 1) then
            !       if (CSPrd(nnuc)>xsdisc) write (12,
            !    &            '(2X,''Cont. popul. before g-cascade '',
            !    &         G12.6,'' mb'')') CSPrd(nnuc) - xsdisc
            !       write (12,'(2X,''Disc. popul. before g-cascade '',
            !    &                G12.6,'' mb'')') xsdisc

            if (CSPrd(nnuc) > xsdisc) write (12, 97547) CSPrd(nnuc) - xsdisc
            write (12, 97548) xsdisc
        97547 format(3X, ' Cont. popul. with cont. g-casc. ', G12.6, ' mb')
        97548 format(3X, ' Disc. popul. with cont. g-casc. ', G12.6, ' mb')
        end if

        if (ENDf(nnuc) == 1) then
            corr = (CSPrd(Nnuc) - xsdisc)/(csum*DE)
        else
            corr = CSPrd(Nnuc)/(csum*DE)
        end if

        write (12, '( 2x,''Ratio Production XS/Recoil XS '',G12.6,'' mb''//)') corr

        if (ABS(1.d0 - corr) > 0.1D0 .and. CSPrd(Nnuc) > 0.001D0) then
            write (8, *)
            write (8, *) ' ******'
            write (8, *) ' WARNING:  Ein = ', sngl(EIN), ' MeV'
            write (8, *) ' WARNING:  ZAP = ', IZA(Nnuc), ' from ', React
            write (8, *) ' WARNING: x-section balance in recoils >10%, ratio=', corr
            write (8, *) ' WARNING: recoil integral     = ', sngl(csum*DE), ' mb'
            write (8, *) ' WARNING: Prod. cross section = ', sngl(CSPrd(Nnuc)), ' mb'
            if (ENDf(nnuc) == 1) then
                write (8, *) ' WARNING: Cont. cross section = ', sngl(CSPrd(Nnuc) - xsdisc), ' mb'
                write (8, *) ' WARNING: Discr.cross section = ', sngl(xsdisc), ' mb'
            end if

        end if

        RETURN
    end subroutine printBinaryRecoil

END MODULE HFdecay
