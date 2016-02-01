Ccc   * $Rev: 4578 $
Ccc   * $Author: rcapote $
Ccc   * $Date: 2016-02-01 20:09:15 +0100 (Mo, 01 Feb 2016) $

      SUBROUTINE PFNS_calc(nnuc)

      use nubar_reader

      implicit none
      INCLUDE "dimension.h"
      INCLUDE "global.h"

      INTEGER nnuc

C     PFNS quantities  
C     Total prompt fission spectra only for neutrons and assumed isotropic 
      DOUBLE PRECISION  emiss_en(NDEPFN),
     & tequiv, fmaxw, fnorm, eincid, eneutr, ftmp, 
     & post_fisn(NDEPFN), ratio2maxw(NDEPFN), fniueval

      INTEGER iaf, izf, nejc, ie 

      DOUBLE PRECISION deltae_pfns
      COMMON /pfns_res/deltae_pfns

      INTEGER     nepfns, nfission
      DOUBLE PRECISION fnubar,enepfns(NDEPFN),csepfns(NDEPFN)
      COMMON /pfns_quant/nfission, nepfns, fnubar, enepfns, csepfns

      fniueval = 1.d0
C     Only PFNS of the neutron chain are considered
C     Checked if Z of the fissioning nucleus
C     is equal to the target, if not PFNS skipped  
      IF (NINT(Z(0)) .NE. NINT(Z(nnuc))) RETURN
C     (Z(0).eq.Z(nnuc) .OR. Z(0)-1.eq.Z(nnuc)) ! neutron or proton chain

      IF (FISSPE.EQ.0 .OR. TOTcsfis.LE.0.d0 .OR.
     &    CSPfis(nnuc).LE.0.0D0 .OR.
C         Prompt fission spectra are not calculated if:
C         Partial fission cross section is lower than 1.d-7*TOTcsfis
     &    CSPfis(Nnuc).LE.1.d-7*TOTcsfis) RETURN
C
C     ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
C     PFNS calculations for a given decaying nucleus A(Nnuc),Z(Nnuc)
C
            iaf = A(nnuc)
            izf = Z(nnuc)
C
            nejc = 1 ! neutron emission only
            post_fisn  = 0.d0
            ratio2maxw =  0.d0
            emiss_en   = 0.d0

            IF(nfission.eq.0) THEN
C
C           First fissioning nucleus in the isotope chain
C
C             Calculating unique energy grid 
C
C             Assumed maximum energy of neutrons from fragments will be 25 MeV

              deltae_pfns = 0.1d0 

              nepfns = min( NINT(25.d0/deltae_pfns) + 1, NDEPFN)
              enepfns(1) = 1.d-11
              DO ie = 2, nepfns  
                enepfns(ie) = FLOAT(ie - 1)*deltae_pfns
              ENDDO
C             Below first chance, no emissive contributions to fission spectra
C             Only fission neutrons emitted from fully accelerated fragments
C             Initializing the pseudo incident energy
              eincid = EXCn - Q(1,1)  ! emitting from CN, nnuc = 1
C
C             The total nubar is calculated for the incident energy and 
C                used for the normalization of the total PFNS
              if(NUBarread) fniuEVAL = fniu_nubar_eval(EINl)
C
C             Models could be used for NUBAR calculations,
C             however, we prefer to normalize our PFNS to the 
C             evaluated NUBAR
C
C             if(fniuEVAL.eq.1) then
C               iafiss = A(1) 
C               IF(FISspe.eq.1) 
C    &            fnubar = fniuLANL(eincid,NINT(A(1)),NINT(Z(1)))
C               IF(FISspe.eq.2) 
C    &            fnubar = fniu    (eincid,NINT(A(1)),NINT(Z(1)))
C             endif 

              fnubar = fniuEVAL * PFNniu ! scaling of NUBAR

            ELSE

C             For higher emission chances, the corresponding  
C             neutron binding energy is substracted iteratively
C
              eincid = eincid - Q(1,nnuc)               

            ENDIF
C
C           If no more excitation energy available, then PFN emission stopped
            if(eincid.LT.0.d0) RETURN

            WRITE (12,*)' '
            WRITE (12,*)' *******************************************'
            WRITE (12,*)' PROMPT FISSION NEUTRON SPECTRA calculations'

            WRITE ( 8,*) ' '
            WRITE ( 8,*)' *******************************************'
            WRITE ( 8,*)' PROMPT FISSION NEUTRON SPECTRA calculations'
            
            WRITE
     &        (12,'(''  Fiss. nucleus '',I3,''-'',A2,''-'',I3)')
     &            INT(Z(nnuc)), SYMb(nnuc), INT(A(nnuc))
            WRITE
     &     (12,'(''  Partial fission cross section '',G12.5,'' mb'')')
     &            CSPfis(Nnuc)
            WRITE
     &     (12,'(''  Ratio of partial to total fission '',G12.5)')
     &            CSPfis(Nnuc)/TOTcsfis
            WRITE
     &     (12,'(''  Binding energy of fissioning nucleus '',
     &            G12.5,'' MeV'')') Q(1,nnuc)

            WRITE
     &        (8,'(''  Fiss. nucleus '',I3,''-'',A2,''-'',I3)')
     &            INT(Z(nnuc)), SYMb(nnuc), INT(A(nnuc))
            WRITE(8,'(1x,a11,i2,a7,f8.3,A4)')  
     &      ' Fiss. Nucl ',nfission + 1, ', Uexc=',eincid + Q(1,nnuc), 
     &      ' MeV'
            WRITE
     &     (8,'(''  Partial fission cross section '',G12.5,'' mb'')')
     &            CSPfis(Nnuc)
            WRITE
     &     (8,'(''  Ratio of partial to total fission '',G12.5)')
     &            CSPfis(Nnuc)/TOTcsfis
            WRITE
     &     (8,'(''  Binding energy of fissioning nucleus '',
     &            G12.5,'' MeV'')') Q(1,nnuc)

C-----------Calculating post-fission neutrons in the first chance
C
C           Los Alamos model  
C
            IF (FISspe.eq.1)
     &        CALL get_fragmPFNS_LANL (post_fisn, enepfns, nepfns,
     &         eincid, A(Nnuc), Z(Nnuc), eneutr, tequiv, Q(1,nnuc)
     &          , deltae_pfns, PFNtke, PFNrat, PFNalp, PFNere)
C
C           Kornilov parameterization
C
            IF (FISspe.eq.2)
     &        CALL get_fragmPFNS      (post_fisn, enepfns, nepfns,
     &         eincid, A(Nnuc), Z(Nnuc), eneutr, tequiv, Q(1,nnuc)
     &          , deltae_pfns, PFNtke, PFNrat, PFNalp, PFNere)

            fnorm = CSPfis(nnuc)/TOTcsfis

C           write (*,*) nint(A(nnuc)),nint(Z(nnuc)),
C    *            sngl(CSPfis(nnuc)),sngl(TOTcsfis)

            if(eneutr.gt.0) then
              DO ie = 1, nepfns
                   ftmp = fmaxw(enepfns(ie),tequiv)
                   if (ftmp.gt.0) ratio2maxw(ie) = post_fisn(ie)/ftmp
C------------------Accumulating total spectrum for neutrons
                   csepfns(ie) = csepfns(ie) + post_fisn(ie)*fnorm
C
C                  CSEfis contains the n,xnf spectra, not being used for now
C                  CSEfis is initialized inside HF-comp.f (EXCLUSIVE..)
C                  csepfns(ie) = csepfns(ie) + CSEfis(ie,nejc,nnuc)
C
              ENDDO

              WRITE ( 8,
     &       '(''  Postfission  En  '', G12.5,'' MeV'')') eneutr
              WRITE ( 8,
     &       '(''  Equivalent Tmaxwell '',G12.5,'' MeV'')') tequiv
              WRITE (8,*) ' '

            else

              WRITE  (8 ,'(''  No fission neutrons emitted'')')

            endif
               
C           WRITE (12,*) ' '
            nfission = nfission + 1
C  
C     end of PFNS calculations 
C     ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
      RETURN
      END

      SUBROUTINE print_PFNS()
      implicit none
      INCLUDE "dimension.h"
      INCLUDE "global.h"

      DOUBLE PRECISION csetmp(ndecse)
      DOUBLE PRECISION fmed, ftmp1, ftmpA, ftmpB

C     PFNS quantities  
C     Total prompt fission spectra only for neutrons and assumed isotropic 
      DOUBLE PRECISION tequiv, fmaxw, eneutr, ftmp

      INTEGER ie

      DOUBLE PRECISION deltae_pfns
      COMMON /pfns_res/deltae_pfns

      INTEGER     nepfns, nfission
      DOUBLE PRECISION fnubar,enepfns(NDEPFN),csepfns(NDEPFN)
      COMMON /pfns_quant/nfission, nepfns, fnubar, enepfns, csepfns

      save csetmp
C
C     Special grid for printing, ONLY VALID for deltae_pfns = 0.1d0 
C
      data csetmp(2)/4.d-11/, csetmp(3)/7.d-11/, csetmp(4)/1.d-10/, 
     &     csetmp(5)/4.d-10/, csetmp(6)/7.d-10/, csetmp(7)/1.d-9/ ,
     &     csetmp(8)/4.d-9/ , csetmp(9)/7.d-9/ , csetmp(10)/1.d-8/, 
     &     csetmp(11)/4.d-8/, csetmp(12)/7.d-8/, csetmp(13)/1.d-7/,
     &     csetmp(14)/4.d-7/, csetmp(15)/7.d-7/, csetmp(16)/1.d-6/,
     &     csetmp(17)/4.d-6/, csetmp(18)/7.d-6/, csetmp(19)/1.d-5/,
     &     csetmp(20)/4.d-5/, csetmp(21)/7.d-5/, csetmp(22)/1.d-4/,
     &     csetmp(23)/4.d-4/, csetmp(24)/7.d-4/, csetmp(25)/1.d-3/,
     &     csetmp(26)/4.d-3/, csetmp(27)/7.d-3/, csetmp(28)/1.d-2/,
     &     csetmp(29)/4.d-2/, csetmp(30)/7.d-2/

      csetmp(1) = enepfns(1)
      csetmp(31)= enepfns(2)

C-----
C-----PRINTING TOTAL PFNS and PFNM quantities
C-----
      IF (FISspe.gt.0 .and. TOTcsfis.gt.0.d0) THEN

        ftmp = 0.D0
        eneutr = 0.d0
        do ie =2, nepfns
          fmed = 
     &    (csepfns(ie)+csepfns(ie-1))*0.5*(enepfns(ie)-enepfns(ie-1))
          eneutr = eneutr + fmed*(enepfns(ie)+enepfns(ie-1))*0.5d0
          ftmp = ftmp + fmed
        enddo
        if(ftmp.GT.0) eneutr = eneutr/ftmp
C
C       tequiv = 2.D0/3.D0*eneutr
C
        tequiv = TMAxw  ! Maxwellian temperature used to scale plots defined in input
C                       ! Default value 1.32 MeV

        WRITE(115,'(1X,g12.5,1x,g12.5,2(4x,f7.3))')
     &        EINl, eneutr, fnubar, tequiv 

        WRITE
     & (114,'(/''  Total PFNS from '',I3,''-'',A2,''-'',I3,'': Elab='',
     &  G12.5,'' MeV,  Epfns ='',G12.5,'' MeV, Tmaxw='',f8.4,
     & '' MeV, Norm='',F10.8)') INT(Z(1)), SYMb(1), INT(A(1)), 
     & EINl,eneutr,tequiv,ftmp

        WRITE(8,*)
        WRITE(8,*) ' ***'
        WRITE(8,*)
        WRITE ( 8,'(''  Total PFNS  for  Elab='',
     &     G12.5,'' MeV,   Norm='',F10.8)') EINl, ftmp
        WRITE(8,*)

        WRITE ( 8,'(''  Number of fissioning nuclei '',I3)') nfission
        WRITE ( 8,'(''  Total PFNS average energy  '',G12.5,A5)') eneutr
     &   ,'  MeV'
        WRITE ( 8,'(''  Tmaxwell for plot = '',G12.5,A5)') tequiv
     &   ,'  MeV'
        WRITE (8,'(''  Delta-epsil   '',F12.9)') deltae_pfns
        WRITE 
     &     (8,'(   ''  Total fission cross section '',G12.5,'' mb'')')
     &            TOTcsfis
        WRITE ( 8,'(''  Multiplicity (nue) '',F6.3)') fnubar
        if(fnubar.ne.1) 
     &         WRITE ( 8,'(''  Nubar from evaluated library '')')
        if(fnubar.ne.1 .and. PFNniu. ne. 1) 
     &         WRITE ( 8,'(''  Nubar scaled by '',f6.4)') PFNniu
        WRITE(8,*)

        WRITE(12,*)
        WRITE(12,*) ' ***'
        WRITE(12,*)
        WRITE (12,'(''  Total PFNS  for  Elab='',
     &  E10.4,'' MeV, Norm='',F10.8)') EINl, ftmp
        WRITE (12,'(''  Number of fissioning nuclei '',I3)') nfission
        WRITE (12,'(''  Total PFNS average energy  '',G12.5,A5)') eneutr
     &   ,'  MeV'
        WRITE (12,'(''  Tmaxwell for plot = '',G12.5,A5)') tequiv
     &   ,'  MeV'
        WRITE (12,'(''  Normalization '',F12.9)') ftmp
        WRITE (12,'(''  Delta-epsil   '',F12.9)') deltae_pfns
        WRITE 
     &     (12,'(   ''  Total fission cross section '',G12.5,'' mb'')')
     &            TOTcsfis
        WRITE (12,'(''  Multiplicity (nue) '',F6.3)') fnubar
        if(fnubar.ne.1) 
     &        WRITE (12,'(''  Nubar from evaluated library '')')
        if(fnubar.ne.1 .and. PFNniu. ne. 1) 
     &        WRITE (12,'(''  Nubar scaled by '',f6.4)') PFNniu
        WRITE(12,*)

        WRITE ( 8,*) ' Spectrum of ','neutrons ', '(z,fission) '
        WRITE ( 8,*) ' '
        WRITE ( 8,'(''    Energy    mb/MeV       Ratio to Maxw'')')
        WRITE ( 8,*) ' '

        WRITE (12,*) ' Spectrum of ','neutrons ', '(z,fission) '
        WRITE (12,*) ' '
        WRITE (12,'(''    Energy    mb/MeV       Ratio to Maxw'')')
        WRITE (12,*) ' '

        WRITE (114,'(/,''    Energy    mb/MeV       Ratio to Maxw'')')

C----------
C       calculating the ratio for the 1st point for interpolation of the outgoing grid
        ftmp  = fmaxw(enepfns(1),tequiv)
        ftmpA = 1.d0
        if (ftmp.gt.0) ftmpA = csepfns(1)/ftmp

C       calculating the ratio for the 2nd point for interpolation of the outgoing grid
        ftmp  = fmaxw(enepfns(2),tequiv)
        ftmpB = 1.d0
        if (ftmp.gt.0) ftmpB = csepfns(2)/ftmp

C       Interpolating and printing between the first and the second point of the 
C             outgoing energy grid enepfns(1) and enepfns(2)
        DO ie = 1, 31
          ftmp1 = ftmpA +  (ftmpB - ftmpA) *
     &     (csetmp(ie) - csetmp(1))/(csetmp(31) - csetmp(1))
         
          ftmp = ftmp1*fmaxw(csetmp(ie),tequiv)

          WRITE (12,'(E10.4,E14.5,2x,E14.5)')
     &      csetmp(ie), ftmp/deltae_pfns, ftmp1
          WRITE (114,'(E11.4,E14.5,4(2x,E14.5))')
     &      csetmp(ie), ftmp/deltae_pfns, ftmp1
          WRITE ( 8,'(E11.4,E14.5,2x,E14.5)')
     &      csetmp(ie), ftmp/deltae_pfns, ftmp1
        ENDDO

        DO ie = 3, nepfns 
          ftmp  = fmaxw(enepfns(ie),tequiv)
          ftmp1 = 1.d0
          if (ftmp.gt.0) ftmp1 = csepfns(ie)/ftmp

          WRITE (12,'(E10.4,E14.5,2x,E14.5)')
     &      enepfns(ie), csepfns(ie)/deltae_pfns, ftmp1
          WRITE (114,'(E11.4,E14.5,4(2x,E14.5))')
     &      enepfns(ie), csepfns(ie)/deltae_pfns, ftmp1
          IF(enepfns(ie).GT.7.d0) cycle
          WRITE ( 8,'(E11.4,E14.5,2x,E14.5)')
     &      enepfns(ie), csepfns(ie)/deltae_pfns, ftmp1
        ENDDO
C       WRITE (12,'(E10.4,E14.5,2x,E14.5)')
C    &     enepfns(nepfns), 0.d0, 0.d0
        WRITE(8,*) '...   PFNS Output supressed for Epfns > 7 MeV '
        WRITE (12,*)
      ENDIF
C  
C     end of PFNS calculations 
C     ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
      RETURN
      END