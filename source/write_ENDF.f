Ccc   * $Rev: 3705 $
Ccc   * $Author: rcapote $
Ccc   * $Date: 2014-01-04 22:01:02 +0100 (Sat, 04 Jan 2014) $

      SUBROUTINE write_ENDF_spectra(totcorr,corrmsd,
     & xscclow,xsinl,xsmsc,tothms,totemis)

      use nubar_reader

      implicit none
      INCLUDE "dimension.h"
      INCLUDE "global.h"

      DOUBLE PRECISION totcorr,corrmsd 
      DOUBLE PRECISION xscclow,xsinl,xsmsc,tothms,totemis	
C
C     common variables
C
      DOUBLE PRECISION ELAcs, TOTcs, ABScs, SINl, SINlcc, SINlcont       ! ECISXS
      COMMON /ECISXS/ ELAcs, TOTcs, ABScs, SINl, SINlcc, SINlcont

      LOGICAL lbreakup, ltransfer
      COMMON /LPEXS/lbreakup, ltransfer 

      DOUBLE PRECISION crossNT(0:NDEJC),crossNTt,
     &                 crossPE(0:NDEJC),crossPEt
      COMMON /PEXS/ crossNT,crossNTt,crossPE,crossPEt

      DOUBLE PRECISION specBU(0:NDEJC,ndecse),crossBU(0:NDEJC),crossBUt
      COMMON /CBREAKUP/specBU,crossBU,crossBUt

      DOUBLE PRECISION cel_da(NDAngecis), checkXS
      COMMON /emp_main/cel_da,checkXS   

      DOUBLE PRECISION elada(NDAngecis), elleg(NDAngecis) 
      COMMON /angula/elada,elleg

      integer icalled
      DOUBLE PRECISION xcross(0:NDEJC+3,0:15,0:20)
      COMMON /init_empire/icalled,xcross
C
C     local variables
C
      CHARACTER*21 reactionx
      INTEGER nejc,i,nnuc,jn,jz,iz,ia,nuc_print,jfiss,jnmx,jzmx
      DOUBLE PRECISION csemax,ftmp,csum,xsdirect,xspreequ,totsum
      DOUBLE PRECISION eps,xnub,csinel,s_factor,qout
      DATA eps/1.d-8/

      DOUBLE PRECISION, external :: mu_bar, SFACTOR

      csinel=CSPrd(2) ! for charged particles or photons

      IF(INT(AEJc(0)).GT.0 .and. INT(ZEJc(0)).EQ.0) ! for neutrons
     >  csinel=CSPrd(2)-4.d0*PI*ELCncs

      if (csinel.lt.eps) csinel=0.d0
      do nnuc=1,NNUcd
        if (CSPrd(nnuc).lt.eps) CSPrd(nnuc)=0.d0
      enddo

      if(NUBarread) then
        xnub = PFNniu*fniu_nubar_eval(EINl)
      else
        xnub = 0.D0
      endif

C********************************************
C-----
C-----ENDF spectra inclusive representation
C-----
      WRITE (8,*) ' '

      IF (IOUt.GT.3 .AND. .NOT.EXClusiv ) THEN

         IF (FIRst_ein) THEN 
            WRITE (8,*)
            WRITE (8,*)
            WRITE (8,'(11X,''**********************'')')
            WRITE (8,'(11x,'' Total spectra (~LAB) '')')
            WRITE (8,'(11x,''**********************'')')
            DO nejc = 0, NEJcm
              csemax = 0.d0
              ftmp = 0.d0
              DO i = 1, NDEX
                csemax = DMAX1(CSEt(i,nejc),csemax)
                ftmp   = ftmp + CSEt(i,nejc)
              ENDDO
              if(csemax.le.0.01d0 .or. ftmp.le.0.0001d0) cycle 

              CALL Print_Total(nejc)
              CALL PLOT_TOTAL_EMIS_SPECTRA(nejc)

              csum = 0.d0
              DO nnuc = 1, NNUcd
                  csum = csum + CSEmis(nejc,nnuc)
              ENDDO
              if(nejc.ne.0) then
               WRITE (8,
     &         '(2X,A2,'' emission cross section'',G12.6,''  mb '',A7)')
     &         SYMbe(nejc), csum, '(TOTAL)'  
              else
               WRITE (8,
     &         '(2X,A2,'' emission cross section'',G12.6,''  mb '',A7)')
     &         ' g', csum, '(TOTAL)'  
              endif
            ENDDO

         ENDIF  
      ENDIF
      WRITE (8,*)
      WRITE (8,*)
      checkXS = checkXS + TOTcsfis

      xsdirect = SINlcc*FCCred + SINl*FCCred + SINlcont*FCOred
      xspreequ = xsinl + xsmsc + totemis + tothms

C     for complex projectiles: reaction, BU,NT,PE,CN
      IF(ltransfer .or. lbreakup)
     &   WRITE(114,'(1P,E11.4,1x,1P,5E13.5)') EINl, CSFus,
     &   crossBUt,crossNTt, csprd(4),CSFus-crossBUt-crossNTt-crossPEt

C
C     Elastic and Nonelastic modified for actinides
C     to include/exclude scattering cross section (xscclow) low-lying coupled states

      IF (A(0).gt.220 .AND. ZEJc(0).EQ.0) then 

C        WRITE(41,'(''#'',A10,1X,1P,95A12)') '  Einc    ',
C    &      '  Total     ','  Elastic*  ','  Nonelast* ',
C    &      '  Fission   ','  Mu-bar    ','  Nu-bar    ',
C    &         (preaction(nnuc),nnuc=1,min(nuc_print,max_prn))

        IF(INT(AEJc(0)).GT.0) THEN  ! particles

          WRITE(41,'(1P,E10.4,1x,1P,95E12.5)') 
     &    EINl, TOTcs*TOTred*totcorr,
C                          Low-lying XS   and       CE         added to elastic
     &    ELAcs*ELAred  +   xscclow       +    4.d0*PI*ELCncs, 
     &    TOTcs*TOTred*totcorr - (ELAcs*ELAred+xscclow+4.d0*PI*ELCncs),
     &    TOTcsfis, 
     &    mu_bar(amass(0),NANgela,ELAred,cel_da,elada),xnub,
     &     CSPrd(1), csinel,(CSPrd(nnuc),nnuc=3,min(nuc_print,max_prn))

          WRITE(107,'(1P,E10.4,1x,1P,95E12.5)') EINl, 
     &    TOTcs*TOTred*totcorr,                           !total = reaction + shape-el
     &    ELAcs*ELAred  +  4.d0*PI*ELCncs,                !elastic (SE + CN_el) 
     &                     4.d0*PI*ELCncs,                !CN_el
     &    ELAcs*ELAred                   ,                !SE
C    &    TOTcs*TOTred*totcorr - (ELAcs*ELAred+4.d0*PI*ELCncs),
     &    CSFus + (SINl+SINlcc)*FCCred + SINlcont*FCOred  !Nonelastic
     &                             - 4.d0*PI*ELCncs,      !   CE substracted from nonelastic
     &    CSFus*corrmsd - tothms - xsmsc,                 !CN-formation 
     &    xsdirect, xspreequ,                             !direct, preequil
     &    SINlcc*FCCred, SINl*FCCred, SINlcont*FCOred,    !CC_inl,DWBA_dis,DWBA_cont  
     &    xsinl,xsmsc,totemis, tothms, xscclow            !MSD,MSC,PCROSS,HMS,xscclow(2 CC levels)
        
        ELSE ! photon-induced

          WRITE(41,'(1P,E10.4,1x,1P,95E12.5)') 
     &    EINl, TOTcs*TOTred*totcorr, ELAcs*ELAred , 
     &    max(TOTcs*TOTred*totcorr - ELAcs*ELAred - CSPrd(1),0.d0), 
     &    TOTcsfis, mu_bar(amass(0),NANgela,ELAred,cel_da,elada),xnub,
     &     CSPrd(1), csinel,(CSPrd(nnuc),nnuc=3,min(nuc_print,max_prn))

          WRITE(107,'(1P,E10.4,1x,1P,95E12.5)') EINl, 
     &    TOTcs*TOTred*totcorr,                           !total = reaction + shape-el
     &    ELAcs*ELAred,   
     &    4.d0*PI*ELCncs,                                 !CN_el
     &    ELAcs*ELAred,                                   !shape elastic 
C    &    TOTcs*TOTred*totcorr - ELAcs*ELAred,
     &    max(CSFus + (SINl+SINlcc)*FCCred + SINlcont*    !Nonelastic
     &      FCOred - 4.d0*PI*ELCncs - CSPrd(1),0.d0),     !CE substracted from nonelastic
     &    max(CSFus*corrmsd - tothms - xsmsc,0.d0),       !CN-formation 
     &    xsdirect, xspreequ,                             !direct, preequil
     &    SINlcc*FCCred, SINl*FCCred, SINlcont*FCOred,    !CC_inl,DWBA_dis,DWBA_cont  
     &    xsinl,xsmsc,totemis, tothms, xscclow            !MSD,MSC,PCROSS,HMS,xscclow(2 CC levels)

        ENDIF

      ELSE ! non-actinides

        IF (ZEJc(0).EQ.0) then 
  
          IF(INT(AEJc(0)).GT.0) THEN  ! particles

            WRITE(41,'(1P,E10.4,1x,1P,95E12.5)')
     &      EINl,TOTcs*TOTred*totcorr,
     &      ELAcs*ELAred           + 4.d0*PI*ELCncs,        ! CE added to elastic
     &      max(TOTcs*TOTred*totcorr - ELAcs*ELAred 
     &                           - 4.d0*PI*ELCncs,0.d0),    ! CE substracted from nonelastic
     &      TOTcsfis, mu_bar(amass(0),NANgela,ELAred,cel_da,elada),xnub,
     &      CSPrd(1), csinel,(CSPrd(nnuc),nnuc=3,min(nuc_print,max_prn))

            WRITE(107,'(1P,E10.4,1x,1P,95E12.5)') EINl, 
     &      TOTcs*TOTred*totcorr,                           !total = reaction + shape-el
     &      ELAcs*ELAred  +  4.d0*PI*ELCncs,                !CE added to elastic 
     &                       4.d0*PI*ELCncs,                !CN_el
     &      ELAcs*ELAred                   ,                !shape elastic 
C    &      TOTcs*TOTred*totcorr - (ELAcs*ELAred + 4.d0*PI*ELCncs),
     &      max(CSFus + (SINl+SINlcc)*FCCred + SINlcont*    !Total nonelastic
     &         FCOred - 4.d0*PI*ELCncs,0.d0),               !   CE substracted from nonelastic
     &      CSFus*corrmsd - tothms - xsmsc,                 !CN-formation 
     &      xsdirect, xspreequ,                             !direct, preequil
     &      SINlcc*FCCred, SINl*FCCred, SINlcont*FCOred,    !CC_inl,DWBA_dis,DWBA_cont  
     &      xsinl,xsmsc,totemis, tothms, xscclow            !MSD,MSC,PCROSS,HMS,xscclow(2 CC levels)

          ELSE ! photon-induced

            WRITE(41,'(1P,E10.4,1x,1P,95E12.5)')
     &      EINl,TOTcs*TOTred*totcorr,
     &      ELAcs*ELAred,                   
     &      TOTcs*TOTred*totcorr - ELAcs*ELAred - CSPrd(1),
     &      TOTcsfis, mu_bar(amass(0),NANgela,ELAred,cel_da,elada),xnub,
     &      CSPrd(1), csinel,(CSPrd(nnuc),nnuc=3,min(nuc_print,max_prn))

            WRITE(107,'(1P,E10.4,1x,1P,95E12.5)') EINl, 
     &      TOTcs*TOTred*totcorr,                           !total = reaction + shape-el
     &      ELAcs*ELAred  +  4.d0*PI*ELCncs,                !CE added to elastic 
     &      4.d0*PI*ELCncs,                                 !CN_el
     &      ELAcs*ELAred,                                   !shape elastic 
C    &      TOTcs*TOTred*totcorr - (ELAcs*ELAred + 4.d0*PI*ELCncs),
     &      max(CSFus + (SINl+SINlcc)*FCCred + SINlcont*    !Nonelastic
     &         FCOred - CSPrd(1) - 4.d0*PI*ELCncs,0.d0),    !   CE substracted from nonelastic
     &      max(CSFus*corrmsd - tothms - xsmsc,0.d0),       !CN-formation 
     &      xsdirect, xspreequ,                             !direct, preequil
     &      SINlcc*FCCred, SINl*FCCred, SINlcont*FCOred,    !CC_inl,DWBA_dis,DWBA_cont  
     &      xsinl,xsmsc,totemis, tothms, xscclow            !MSD,MSC,PCROSS,HMS,xscclow(2 CC levels)

          ENDIF

        ELSE ! charged-particles

          WRITE(41,'(1P,E10.4,1x,1P,95E12.5)')EINl,TOTcs*TOTred*totcorr,
     &    ELAcs*ELAred             + 4.d0*PI*ELCncs,      !CN_el (CE) added to elastic 
C
C         Total is 0 for charged particles, no correction
C    &    TOTcs*TOTred*totcorr - (ELAcs*ELAred + 4.d0*PI*ELCncs),
C
     &    max(CSFus + (SINl+SINlcc)*FCCred + SINlcont*FCOred 
     &                             - 4.d0*PI*ELCncs,0.d0),! CE substracted from nonelastic
C
     &    TOTcsfis, mu_bar(amass(0),NANgela,ELAred,cel_da,elada),xnub,
     &    CSPrd(1), csinel,(CSPrd(nnuc),nnuc=3,min(nuc_print,max_prn))
C
          WRITE(107,'(1P,E10.4,1x,1P,95E12.5)') EINl, 
     &    TOTcs*TOTred*totcorr,                           !total = reaction + shape-el
     &    ELAcs*ELAred   + 4.d0*PI*ELCncs,                !CN_el (CE) added to elastic 
     &                     4.d0*PI*ELCncs,                !CN_el
     &    ELAcs*ELAred                   ,                !shape elastic 
C
C         Total is 0 for charged particles, no correction
C    &    TOTcs*TOTred*totcorr - (ELAcs*ELAred + 4.d0*PI*ELCncs),
C
     &    max(CSFus + (SINl+SINlcc)*FCCred + SINlcont*FCOred 
     &                             - 4.d0*PI*ELCncs,0.d0),! CE substracted from nonelastic
C
     &    max(CSFus*corrmsd - tothms - xsmsc,0.d0),       !CN-formation 
     &    xsdirect, xspreequ,                             !direct, preequil
     &    SINlcc*FCCred, SINl*FCCred, SINlcont*FCOred,    !CC_inl,DWBA_dis,DWBA_cont  
     &    xsinl,xsmsc,totemis, tothms, xscclow            !MSD,MSC,PCROSS,HMS,xscclow(2 CC levels)
        ENDIF 

      ENDIF

      IF(ABScs.GT.0.) THEN
        WRITE (8,'('' ********************************************'',
     &           23(1H*))')
        WRITE (8,'('' * SUMMA SUMMARUM '')')
        WRITE (8,'('' *'')')
        WRITE (8,'('' * Incident energy (LAB): '',G12.5,
     &              '' MeV  '')') EINl
        IF (INT(ZEJc(0)).EQ.0) THEN
          WRITE (8,
     &  '('' * Total cross section                            '',G13.6,
     &              '' mb  '')') TOTcs*TOTred*totcorr
C    &              '' mb  '')') CSFus + (SINl+SINlcc)*FCCred +
C    &    SINlcont*FCOred + ELAred*ELAcs  = TOTcs*TOTred*totcorr

          WRITE (8,
     &  '('' * OM total cross section                         '',G13.6,
     &              '' mb  '')') ELAred*ELAcs + 
     &        (ABScs - (SINl+SINlcc+SINlcont))*FUSred+
     &        (SINl+SINlcc)*FCCred + SINlcont*FCOred 
         IF (INT(AEJc(0)).GT.0) WRITE (8,
     &  '('' * Shape Elastic cross section (ELAcs)            '',G13.6,
     &              '' mb  '')') ELAred*ELAcs
        ENDIF

        WRITE (8,
     &  '('' * OM nonelastic cross section (ABScs)            '',G13.6,
     &              '' mb  '')')
     &   (ABScs - (SINl+SINlcc+SINlcont))*FUSred+
     &     (SINl+SINlcc)*FCCred + SINlcont*FCOred 
        WRITE (8,
     &  '('' * Nonelastic cross section                       '',G13.6,
     &              '' mb  '')')
     &   CSFus + (SINl+SINlcc)*FCCred + SINlcont*FCOred
        WRITE (8,
     &  '('' * Production cross section (incl.fission)        '',G13.6,
     &              '' mb'')')  checkXS
        IF(FISsil(1)) WRITE (8,
     &  '('' * Fission cross section                          '',G13.6,
     &              '' mb  '')') TOTcsfis
        WRITE (8,'('' * Difference: '', F7.2, '' mb ('',F6.2,'') %'')')
     &    CSFus + (SINl+SINlcc)*FCCred + SINlcont*FCOred - checkXS,
     &    100.d0*abs(
     &    ( CSFus + (SINl+SINlcc)*FCCred + SINlcont*FCOred - checkXS ))/
     &    ( CSFus + (SINl+SINlcc)*FCCred + SINlcont*FCOred)
        IF (INT(ZEJc(0)).EQ.0 .and. INT(AEJc(0)).GT.0 .and. 
     *      ELCncs.gt.0.1d0) THEN          
           WRITE (8,
     &  '('' * Compound elastic cross section (CE)            '',G13.6,
     &              '' mb  '')') 4.d0*PI*ELCncs
        ELSE
          WRITE (8,'('' * '')') 
        ENDIF
        IF (INT(ZEJc(0)).EQ.0) THEN
          IF(TOTred.ne.1)
     &    WRITE (8,'('' * Total         cross section scaled by '',
     &     G13.6)') TOTred
          IF(totcorr.gt.0) THEN
            IF( abs(1.d0/totcorr - TOTred0).gt.0.001d0) THEN
              WRITE (108,'(2x,G12.5,3x,F10.6)') EINl, 1.d0/totcorr  
              WRITE (8,'('' *   set TOTRED '' , F13.6,
     &         '' to keep unchanged total'')') 1.d0/totcorr
            ENDIF  
          ENDIF
        ENDIF 
        IF(FUSred.ne.1)
     &    WRITE (8,'('' * Reaction      cross section scaled by '',
     &     G13.6)') FUSred
        IF (INT(ZEJc(0)).EQ.0 .AND. ELAred.ne.1)
     &    WRITE (8,'('' * Shape Elastic cross section scaled by '',
     &     G13.6)') ELAred
        IF (INT(ZEJc(0)).EQ.0 .AND. CELred.ne.1)
     &    WRITE (8,'('' * Comp. Elastic cross section scaled by '',
     &     G13.6)') CELred
        if(FCCred.ne.1)
     &    WRITE (8,'('' * Disc.lev. DIR cross section scaled by '',
     &     G13.6)') FCCred
        if(FCOred.ne.1)
     &    WRITE (8,'('' * Cont.lev. DIR cross section scaled by '',
     &      G13.6)') FCOred

        if (INT(ZEJc(0)).EQ.0) then
            DO i=1,NLV(0) ! loop over target discrete levels
            IF(CINred(i).NE.1) WRITE (8,
     >       '('' * Comp. inelastic cross section for target level # '',
     &       i2,'' scaled by '',G13.6)') i, CINred(i)
            ENDDO
        endif
        WRITE (8,'('' ********************************************'',
     &           23(1H*))')

        IF (INT(ZEJc(0)).EQ.0) THEN
          WRITE (*,
     &  '(''   Total cross section                            '',G13.6,
     &              '' mb  '')') TOTcs*TOTred*totcorr
C    &              '' mb  '')') CSFus + (SINl+SINlcc)*FCCred +
C    &    SINlcont*FCOred + ELAred*ELAcs  = TOTcs*TOTred*totcorr

         IF (INT(AEJc(0)).GT.0) WRITE (*,
     &  '(''   Shape Elastic cross section (ELAcs)            '',G13.6,
     &              '' mb  '')') ELAred*ELAcs
        ENDIF
        WRITE (*,
     &  '(''   Nonelastic cross section                       '',G13.6,
     &              '' mb  '')')
     &   CSFus + (SINl+SINlcc)*FCCred + SINlcont*FCOred
        WRITE (*,
     &  '(''   Production cross section (incl.fission)        '',G13.6,
     &              '' mb'')')  checkXS
        IF(FISsil(1)) WRITE (*,
     &  '(''   Fission cross section                          '',G13.6,
     &              '' mb  '')') TOTcsfis
        WRITE (*,'(''   Difference: '', F7.2, '' mb ('',F6.2,'') %'')')
     &    CSFus + (SINl+SINlcc)*FCCred + SINlcont*FCOred - checkXS,
     &    100.d0*abs(
     &    ( CSFus + (SINl+SINlcc)*FCCred + SINlcont*FCOred - checkXS ))/
     &    ( CSFus + (SINl+SINlcc)*FCCred + SINlcont*FCOred)
        IF (INT(ZEJc(0)).EQ.0 .and. INT(AEJc(0)).GT.0 .and. 
     &      ELCncs.gt.0.1d0) THEN
          WRITE (*,
     &  '(''   Compound elastic cross section (CE)            '',G13.6,
     &              '' mb  '')') 4.d0*PI*ELCncs
        ELSE
          WRITE (*,*)
        ENDIF
      ENDIF
      IF(abs(CSFus + (SINl+SINlcc)*FCCred + SINlcont*FCOred - checkXS)
     &  .GT.0.01*(CSFus + (SINl+SINlcc)*FCCred + SINlcont*FCOred)) THEN
        WRITE (8,*)
        WRITE (8,'(''  WARNING: Sum of production XS (incl.fission)'')')
        WRITE (8,'(''  WARNING: is not equal reaction cross section'')')
        IF((CSFus + (SINl+SINlcc)*FCCred + SINlcont*FCOred).NE.0.d0)
     &  WRITE (8,'(''  WARNING:     difference: '', F6.2,'' % at E = '',
     &  G12.5,'' MeV'')') 100.d0*
     &   abs(CSFus + (SINl+SINlcc)*FCCred + SINlcont*FCOred - checkXS)/
     &           (CSFus + (SINl+SINlcc)*FCCred + SINlcont*FCOred),EINl
      ENDIF
      IF(TOTred*TOTcs*totcorr.gt.0.d0 .and.
     &     abs(CSFus + (SINl+SINlcc)*FCCred + SINlcont*FCOred + 
     &     ELAred*ELAcs - TOTred*TOTcs*totcorr) .GT.
     &                0.01*TOTred*TOTcs*totcorr) THEN
        WRITE (8,*)
        WRITE (8,'(''  WARNING: Total cross section is NOT equal'')')
        WRITE (8,'(''  WARNING: Elastic + Absorption cross section'')')
        WRITE (8,'(''  WARNING:     difference: '', F6.2,'' % at E = '',
     &  G12.5,'' MeV'')') 100.d0*
     &    abs(ABScs + ELAred*ELAcs - TOTred*TOTcs*totcorr)/
     &                 (TOTred*TOTcs*totcorr),EINl
      ENDIF
      WRITE (8,*)
      WRITE (8,*) '+++ end of one energy +++'
      WRITE (8,*) 
C-----
C-----ENDF spectra printout (inclusive representation)
C-----
      IF (.NOT.EXClusiv) THEN
         WRITE (12,*) 
         WRITE (12,*) '********************************************'
         WRITE (12,*) '* INCLUSIVE SPECTRA at Einc =', sngl(EINl) 
         WRITE (12,*) '********************************************'
         WRITE (12,*)    
 
C--------Print spectra of residues
         reactionx = '(z,x)  '
	   qout = 0.d0
         DO nnuc = 1, NNUcd    !loop over decaying nuclei
            IF (ENDf(nnuc).EQ.2 .AND. RECoil.GT.0)
     &        CALL PRINT_RECOIL(nnuc,reactionx,qout)
         ENDDO !over decaying nuclei in ENDF spectra printout
C
         WRITE (12,*) ' '    
C--------Print inclusive spectra of gamma and ejectiles
         DO nejc = 0, NEJcm
           CALL Print_Inclusive(nejc,qout)
         ENDDO
         WRITE (12,*) ' '    
      ENDIF

C  Summary of exclusive emission cross sections
      jnmx = 0
      jzmx = 0
      jfiss = 0
      DO jn = 0, 20
        DO jz = 0, 15
          IF(xcross(NDEJC+1,jz,jn).GT.0.01d0) jfiss = 1
          IF(xcross(NDEJC+2,jz,jn).GT.0.01d0) THEN
            jnmx = MAX(jnmx,jn)
            jzmx = MAX(jzmx,jz)
          ENDIF
        ENDDO
      ENDDO
      IF(jnmx.gt.1 .AND. jzmx.gt.1) THEN
        iz = INT(Z(1))
        ia = INT(A(1))-iz
        WRITE (12,*) ' '
        WRITE (12,*) ' Gamma emission cross sections (mb) ZAP=    0'
        WRITE (12,*) ' '
        WRITE (12,'(''  Z / N '',20(i6,2x))') (ia-jn, jn = 0,jnmx)
        DO jz = 0, jzmx 
          WRITE(12,'(i5,2x,20f8.2)')iz-jz,(xcross(0,jz,jn), jn = 0,jnmx)
        ENDDO
        WRITE (12,*) ' '
        WRITE (12,*) ' '
        WRITE (12,*) ' Neutron emission cross sections (mb) ZAP=    1'
        WRITE (12,*) ' '
        WRITE (12,'(''  Z / N '',20(i6,2x))') (ia-jn, jn = 0,jnmx)
        DO jz = 0, jzmx 
          WRITE(12,'(i5,2x,20f8.2)')iz-jz,(xcross(1,jz,jn), jn = 0,jnmx)
        ENDDO
        WRITE (12,*) ' '
        WRITE (12,*) ' '
        WRITE (12,*) ' Proton emission cross sections (mb) ZAP= 1001'
        WRITE (12,*) ' '
        WRITE (12,'(''  Z / N '',20(i6,2x))') (ia-jn, jn = 0,jnmx)
        DO jz = 0, jzmx 
          WRITE(12,'(i5,2x,20f8.2)')iz-jz,(xcross(2,jz,jn), jn = 0,jnmx)
        ENDDO
        WRITE (12,*) ' '
        WRITE (12,*) ' '
        WRITE (12,*) ' Alpha emission cross sections (mb) ZAP= 2004'
        WRITE (12,*) ' '
        WRITE (12,'(''  Z / N '',20(i6,2x))') (ia-jn, jn = 0,jnmx)
        DO jz = 0, jzmx 
          WRITE(12,'(i5,2x,20f8.2)')iz-jz,(xcross(3,jz,jn), jn = 0,jnmx)
        ENDDO
        WRITE (12,*) ' '
        WRITE (12,*) ' '
        WRITE (12,*) ' Deuteron emission cross sections (mb) ZAP= 1002'
        WRITE (12,*) ' '
        WRITE (12,'(''  Z / N '',20(i6,2x))') (ia-jn, jn = 0,jnmx)
        DO jz = 0, jzmx 
          WRITE(12,'(i5,2x,20f8.2)')iz-jz,(xcross(4,jz,jn), jn = 0,jnmx)
        ENDDO
        WRITE (12,*) ' '
        WRITE (12,*) ' '
        WRITE (12,*) ' Triton emission cross sections (mb) ZAP= 1003'
        WRITE (12,*) ' '
        WRITE (12,'(''  Z / N '',20(i6,2x))') (ia-jn, jn = 0,jnmx)
        DO jz = 0, jzmx 
          WRITE(12,'(i5,2x,20f8.2)')iz-jz,(xcross(5,jz,jn), jn = 0,jnmx)
        ENDDO
        WRITE (12,*) ' '
        WRITE (12,*) ' '
        WRITE (12,*) ' Helium-3 emission cross sections (mb) ZAP= 2003'
        WRITE (12,*) ' '
        WRITE (12,'(''  Z / N '',20(i6,2x))') (ia-jn, jn = 0,jnmx)

        DO jz = 0, jzmx 
          WRITE(12,'(i5,2x,20f8.2)')iz-jz,(xcross(6,jz,jn), jn = 0,jnmx)
        ENDDO
        WRITE (12,*) ' '

        IF(NDEJC.EQ.7) THEN
          WRITE (12,*) ' '
          WRITE (12,
     &        '(I2,''-'',A2,'' emission cross sections (mb) ZAP='',I5)')
     &                 INT(AEJc(NDEJC)), SYMbe(NDEJC), IZAejc(NDEJC)
          WRITE (12,*) ' Ion emission cross sections (mb)'
          WRITE (12,*) ' '
          WRITE (12,'(''  Z / N '',20(i6,2x))') (ia-jn, jn = 0,jnmx)
          DO jz = 0, jzmx 
            WRITE(12,'(i5,2x,20f8.2)')iz-jz,
     &                                (xcross(NDEJC,jz,jn), jn = 0,jnmx)
           ENDDO
          WRITE (12,*) ' '
        ENDIF
        IF(jfiss.GT.0) THEN
          WRITE (12,*) ' '
          WRITE (12,*) ' Fission cross sections (mb)'
          WRITE (12,*) ' '
          WRITE (12,'(''  Z / N '',20(i6,2x))') (ia-jn, jn = 0,jnmx)
          DO jz = 0, jzmx 
            WRITE(12,'(i5,2x,20f8.2)')iz-jz,
     &                              (xcross(NDEJC+1,jz,jn), jn = 0,jnmx)
           ENDDO
          WRITE (12,*) ' '
        ENDIF
        WRITE (12,*) ' '
        WRITE (12,*) ' Initial populations (mb)'
        WRITE (12,*) ' '
        WRITE (12,'(''  Z / N '',20(i6,2x))') (ia-jn, jn = 0,jnmx)
        DO jz = 0, jzmx 
          WRITE(12,'(i5,2x,20f8.2)')iz-jz,
     &                              (xcross(NDEJC+3,jz,jn), jn = 0,jnmx)
        ENDDO
        WRITE (12,*) ' '
        WRITE (12,*) ' '

        totsum=0.d0
        DO jz = 0, jzmx 
          DO jn = 0, jnmx
            totsum = totsum + xcross(NDEJC+2,jz,jn)
          ENDDO
        ENDDO

        WRITE (12,*) ' Total production cross sections (mb) :', totsum
        WRITE (12,*) ' '
        WRITE (12,'(''  Z / N '',20(i6,2x))') (ia-jn, jn = 0,jnmx)
        DO jz = 0, jzmx 
          WRITE(12,'(i5,2x,20f8.2)')iz-jz,
     &                              (xcross(NDEJC+2,jz,jn), jn = 0,jnmx)
        ENDDO
        WRITE (12,*) ' '
       ENDIF        
C-----End of ENDF spectra (inclusive)
C
C-----S-FACTOR call
      s_factor = 0.d0
      IF(SFAct.GT.0) THEN
        IF(SFAct.EQ.1) s_factor = SFACTOR(CSPrd(1))  
        IF(SFAct.EQ.2) s_factor = SFACTOR(csinel)      
        IF(SFAct.EQ.3) s_factor = SFACTOR(CSPrd(3))    
      ENDIF
	   
      RETURN
      END
