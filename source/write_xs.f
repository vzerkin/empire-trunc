Ccc   * $Rev: 4535 $
Ccc   * $Author: mherman $
Ccc   * $Date: 2015-12-08 15:48:54 +0100 (Di, 08 Dez 2015) $

      SUBROUTINE write_xs()
      USE empcess, ONLY: POPcsea, CSDirsav, check_DL 

      implicit none
      INCLUDE "dimension.h"
      INCLUDE "global.h"
C
C     common variables
      DOUBLE PRECISION xnorm(0:NDEJC,NDExclus)
      COMMON /HFloop/xnorm
C
C     local variables
C
      CHARACTER*9 cejectile
      INTEGER nnuc,nejc,nnur,iloc,nspec,nang,il,ie,iizaejc,myalloc,itmp
      INTEGER nspec_np,iemm
      DOUBLE PRECISION recorp,espec,csum,esum,qin,qinaver,csnp,emax_np
      DOUBLE PRECISION cmul,xsdisc,dtmp,htmp,ftmp,csum1,ginclus,gexclus
C     DOUBLE PRECISION cseaprnt(ndecse,ndangecis),check_DE(ndecse)
      DOUBLE PRECISION,ALLOCATABLE :: cseaprnt(:,:),check_DE(:),csenp(:)

      if(allocated(cseaprnt)) deallocate(cseaprnt)
 
      ALLOCATE(cseaprnt(ndecse,ndangecis),STAT=myalloc)
      IF(myalloc.NE.0) THEN
        WRITE(8,*)  
     &            'ERROR: Insufficient memory for cseaprnt: write_xs()!'
        WRITE(12,*) 
     &            'ERROR: Insufficient memory for cseaprnt: write_xs()!'
        STOP
     &            'ERROR: Insufficient memory for cseaprnt: write_xs()!'
      ENDIF

      if(allocated(check_DE)) deallocate(check_DE)

      ALLOCATE(check_DE(ndecse),STAT=myalloc)
      IF(myalloc.NE.0) THEN
        WRITE(8,*)  
     &            'ERROR: Insufficient memory for check_DE: write_xs()!'
        WRITE(12,*) 
     &            'ERROR: Insufficient memory for check_DE: write_xs()!'
        STOP
     &            'ERROR: Insufficient memory for check_DE: write_xs()!'
      ENDIF

      if(allocated(csenp)) deallocate(csenp)

      ALLOCATE(csenp(ndecse),STAT=myalloc)
      IF(myalloc.NE.0) THEN
        WRITE(8,*)  
     &            'ERROR: Insufficient memory for csenp: write_xs()!'
        WRITE(12,*) 
     &            'ERROR: Insufficient memory for csenp: write_xs()!'
        STOP
     &            'ERROR: Insufficient memory for csenp: write_xs()!'
      ENDIF
C
C-----Write a row in the table of cross sections (Note: inelastic has CN elastic subtracted)
cccccccccccccccccccc ccccccccccccccccccccccccccccc
C-----Reaction Cross Sections lower than 1.d-8 are considered zero.
      IF(TOTcsfis.gt.0.d0) WRITE(98,'(1P,E10.4,1x,1P,(30E12.5))') 
     &  EINl, TOTcsfis, (CSPfis(nnuc),nnuc=1,min(NNUcd,10,max_prn-1))
      CLOSE (80)
      CLOSE (79)

      IF(FISsil(1)) THEN
        WRITE (12,*) 
        WRITE (12,'('' Tot. fission cross section '',G12.4,'' mb'')')
     &       TOTcsfis
        WRITE (8,*) 
        WRITE (8,'(''  Tot. fission cross section '',G12.4,'' mb'')')
     &       TOTcsfis
      ENDIF

      IF(ENDf(1).GT.0) THEN 
        WRITE (12,*) 
        WRITE (12,*) '*******************************************'
        WRITE (12,*) '* EMISSION SPECTRA at Einc =', sngl(EINl) 
        WRITE (12,*) '*******************************************'
        WRITE (12,*) 
      ENDIF 
C
C---- ENDF spectra printout (exclusive representation)
C----
C     DO nnuc = 1, NNUcd  ! loop over residues (not decaying nuclei)
C        IF (ENDf(nnuc).EQ.1 .and. CSPrd(nnuc).GT.0.0D0) THEN
C           write(*,*) 'Residual nucleus:', NINT(Z(nnuc)),NINT(A(Nnuc))
C           DO nejc = 1, NDEJC         !loop over ejectiles
C             IF (POPcs(nejc,INExc(nnuc)).GT.0.d0) 
C    &           write(*,'(2x,3(I3,1x),A8,2x,I2,2x,d12.6)') 
C    &            nejc, nnuc, INExc(nnuc),
C    &          ' ENDfp= ',ENDfp(nejc,nnuc),POPcs(nejc,INExc(nnuc))
C           ENDDO
C           write(*,*)
C        ENDIF 
C     ENDDO

      csnp = 0.d0
      csenp = 0.d0
      DO nnuc = 1, NNUcd  ! loop over residues (not decaying nuclei)
         IF (ENDf(nnuc).EQ.1) THEN
           IF (CSPrd(nnuc).GT.0.0D0) THEN

             DO nejc = 0, NDEJC         !loop over ejectiles
                IF (POPcs(nejc,INExc(nnuc)).EQ.0.d0) CYCLE
                IF(A(nnuc).LE.4. AND. Z(nnuc).LE.2.) CYCLE
C 
                IF(ENDfp(nejc,nnuc).NE.1) THEN
C                  To add spectra to inclusive
                   nspec= min(INT(EMAx(nnuc)/DE) + 1,NDECSE-1)
C------------------(continuum part - same for all particles)
                   DO ie = 1, nspec + 1 
                     CSE(ie,nejc,0) = CSE(ie,nejc,0) + 
     &                                POPcse(0,nejc,ie,INExc(nnuc))
                   ENDDO 
                   CYCLE
                ENDIF

                ginclus = 0.d0
                gexclus = 1.d0
                CSnp = 0.d0
                IF(ENDfp(nejc,nnuc).EQ.1 .and. nejc.eq.0) THEN
                  IF(NINT(A(1)-A(nnuc)).eq.2 .and. 
     &              NINT(Z(1)-Z(nnuc)).eq.1) THEN ! deuteron
                    ginclus = POPcs(0,INExc(nnuc)) - CSGinc(4)
                    gexclus = CSGinc(4)/POPcs(0,INExc(nnuc))
                  ENDIF  
                  IF(NINT(A(1)-A(nnuc)).eq.3 .and. 
     &              NINT(Z(1)-Z(nnuc)).eq.1) THEN ! triton
                    ginclus = POPcs(0,INExc(nnuc)) - CSGinc(5)
                    gexclus = CSGinc(5)/POPcs(0,INExc(nnuc))
                  ENDIF  
                  IF(NINT(A(1)-A(nnuc)).eq.3 .and. 
     &              NINT(Z(1)-Z(nnuc)).eq.2) THEN ! he-3
                    ginclus = POPcs(0,INExc(nnuc)) - CSGinc(6)
                    gexclus = CSGinc(6)/POPcs(0,INExc(nnuc))
                  ENDIF  
                  IF(NINT(A(1)-A(nnuc)).eq.4 .and. 
     &              NINT(Z(1)-Z(nnuc)).eq.2) THEN ! he-4
                    ginclus = POPcs(0,INExc(nnuc)) - CSGinc(3)
                    gexclus = CSGinc(3)/POPcs(0,INExc(nnuc))
                  ENDIF  

                  IF(ginclus.gt.0) THEN
                    IF(NINT(A(1)-A(nnuc)).eq.2 .and. 
     &                NINT(Z(1)-Z(nnuc)).eq.1) THEN ! deuteron
C                     To split gamma spectra in (z,d) and (z,np+pn)
                      nspec    = min(INT(EMAx(nnuc)/DE) + 1,NDECSE-1)
                      nspec_np = 
     &                 min(INT((EMAx(MT91) -Q(2,MT91))/DE) + 1,NDECSE-1)
C                     write(*,*) 'Emax(nnuc)=',EMAx(nnuc),nnuc
C                     write(*,*) 'Emaxd=',EMAx(1)-Q(4,1)
C                     write(*,*) 'Emaxpn=',EMAx(MT649)-Q(1,MT649)
C                     write(*,*) 'Emaxnp=',EMAx(MT91) -Q(2,MT91)                          

C---------------------(continuum part - same for all particles)
                      ftmp = ginclus/POPcs(0,INExc(nnuc))
C                     write(*,*) nejc,nnuc,ginclus,POPcs(0,INExc(nnuc))
                      DO ie = 1, nspec_np
                        CSEnp(ie) =  POPcse(0,nejc,ie,INExc(nnuc))*ftmp
                      ENDDO 
                    ELSE
C                     To add partial gamma spectra to inclusive
                      nspec= min(INT(EMAx(nnuc)/DE) + 1,NDECSE-1)
C---------------------(continuum part - same for all particles)
                      ftmp = ginclus/POPcs(0,INExc(nnuc))
C                     write(*,*) nejc,nnuc,ginclus,POPcs(0,INExc(nnuc))
                      DO ie = 1, nspec + 1 
                         CSE(ie,nejc,0) = CSE(ie,nejc,0) + 
     &                               POPcse(0,nejc,ie,INExc(nnuc))*ftmp
                      ENDDO 
                    ENDIF
                  ENDIF
                ENDIF
C 
C               nnur is the decaying compound: nnur = nnuc + nejc
C
                IF(nejc.GT.0) THEN
                  CALL WHERE(IZA(nnuc)+IZAejc(nejc),nnur,iloc)
                ELSE
                  nnur = nnuc
                  iloc = 0
                ENDIF
                IF(iloc.NE.0) CYCLE

                ftmp = 1.d0 
                IF (nejc.EQ.0) THEN
                  cejectile = 'gammas   '
                  iizaejc = 0
                ELSEIF (nejc.EQ.1) THEN
                  cejectile = 'neutrons '
                  iizaejc = IZAejc(nejc)
                ELSEIF (nejc.EQ.2) THEN
                  cejectile = 'protons  '
                  iizaejc = IZAejc(nejc)
                ELSEIF (nejc.EQ.3) THEN
                  cejectile = 'alphas   '
                  iizaejc = IZAejc(nejc)
                ELSEIF (nejc.EQ.4) THEN
                  cejectile = 'deuterons'
                  iizaejc = IZAejc(nejc)
                ELSEIF (nejc.EQ.5) THEN
                  cejectile = 'tritons  '
                  iizaejc = IZAejc(nejc)
                ELSEIF (nejc.EQ.6) THEN
                  cejectile = 'helium-3 '
                  iizaejc = IZAejc(nejc)
                ELSEIF (nejc.EQ.NDEJC) THEN
                  cejectile = 'lt. ions '
                  iizaejc = IZAejc(NDEJC)
                ENDIF

C---------------Double the first bin x-sec to preserve integral in EMPEND
                POPcse(0, nejc, 1, INExc(nnuc)) =  
     &                  POPcse(0, nejc, 1, INExc(nnuc))*2

C
C---------------Exclusive DDX spectra (all particles but gammas)
                recorp = 1.d0
                nspec= min(INT(EMAx(nnuc)/DE) + 1,NDECSE-1)
C               nspec= min(INT(EMAx(nnuc)/DE)    ,NDECSE-1)
C-------------------------------------------------------------------
C               A different way of calculating the Q-value using the 
C               residual and ejectile
C               nspec= min(INT((EMAx(nnur)-Q(nejc,nnur))/DE) + 1,
C    &                     NDECSE-1)
C               dang = PI/FLOAT(NDANG - 1)
                IF (nejc.GT.0) THEN
                  WRITE (12,*) ' '
                  WRITE (12,*) ' Spectrum of ', cejectile,
     &                         REAction(nnuc), ' ZAP= ', iizaejc
C-----------------recorp is a recoil correction factor defined 1+Ap/Ar that
C-----------------multiplies cross sections and divides outgoing energies
                  IF (RECoil.GT.0) 
     &              recorp = 1.d0 + EJMass(nejc)/AMAss(nnuc)
                   WRITE (12,
     &                      '(30X,''A     n     g     l     e     s '')'
     &                      )
                   WRITE (12,*) ' '
                   WRITE (12,'('' Energy   '',8G15.5,/,(10X,8G15.5))')
     &                      (ANGles(nang),nang=1,NDANG)
                   check_DL = 0.d0
C------------------First emission reactions
C------------------(discrete levels part)
                   IF ((nnuc.EQ.mt91  .AND. nejc.EQ.1) .OR.
     &                 (nnuc.EQ.mt649 .AND. nejc.EQ.2) .OR.
     &                 (nnuc.EQ.mt849 .AND. nejc.EQ.3) ) THEN
                     DO il = 1, NLV(nnuc)  ! discrete levels
C                       espec is the outgoing energy corresponding to the level "il"
                        espec = (EMAx(nnuc) - ELV(il,nnuc))/recorp
                        IF (espec.GE.0) WRITE (12,
     &                     '(F10.5,E14.5,7E15.5,/,(9X,8E15.5))') -espec, 
c                       Discrete level cros section CSAlev contains angular distribution, NOT DDXS
c                       recoil correction is not needed as we do not integrate over energy 
c                       We only sum over discrete levels ! 
     &                     (max(CSAlev(nang,il,nejc), 
c    &                     (max(CSAlev(nang,il,nejc)*recorp/DE,
     &                               0.d0),nang = 1,NDANG)
                        csum = 0.d0
                        DO nang = 1, NDANG  ! over angles
                          csum = csum+CSAlev(nang,il,nejc)*SANgler(nang)
                        ENDDO
                        check_DL(il)=
     &                    max(2.0d0*PI*csum*PI/(NDAng - 1),1.d-10)  ! PI/90.d0
                     ENDDO                                
                   ENDIF
C
C------------------(continuum part - same for all particles)
                   cseaprnt = 0.d0 ! clean DDX matrix
                   check_DE = 0.d0
                   dtmp = 0.d0
                   esum = 0.d0
                   DO ie = 1, nspec + 1 ! reconstruct continuum DDX spectrum
                     htmp = POPcse(0,nejc,ie,INExc(nnuc))
                     if(htmp.LE.0.d0) cycle
                     itmp = 1
C                    if(ie.eq.1 .or. ie.eq.nspec + 1) itmp=2
                     if(ie.eq.1) itmp=2
                     dtmp = dtmp + htmp*DE/itmp
                     esum = esum + htmp*DE/itmp*FLOAT(ie - 1)*DE/recorp

                     csum = 0.d0
                     IF(LHMs.GT.0 .AND. (nejc.EQ.1 .OR. nejc.EQ.2)) THEN
C----------------------Check whether integral over angles agrees with DE spectra
                       ftmp = max((htmp - xnorm(nejc,INExc(nnuc))
     &                  *POPcsed(0,nejc,ie,INExc(nnuc)))/PIx4,0.d0)
                       DO nang = 1, NDANG
                         cseaprnt(ie,nang) = 
     &                     ftmp + xnorm(nejc,INExc(nnuc))*
     &                            POPcsea(nang,0,nejc,ie,INExc(nnuc))
                           csum = csum + cseaprnt(ie,nang)*SANgler(nang)
                       ENDDO
                     ELSE
c The following is equivalent the definition of ftmp above, when LHMs=0.
                       ftmp = max( (POPcse(0,nejc,ie,INExc(nnuc)) -
     &                  CSEmsd(ie,nejc)*POPcseaf(0,nejc,ie,INExc(nnuc)))  
     &                  /PIx4,0.d0)
                       DO nang = 1, NDANG
                         cseaprnt(ie,nang) =
     &                     ftmp + CSEa(ie,nang,nejc,1)*
     &                            POPcseaf(0,nejc,ie,INExc(nnuc))
                           csum = csum + cseaprnt(ie,nang)*SANgler(nang)
                       ENDDO
                     ENDIF

                     check_DE(ie) = 2.0d0*PI*csum*PI/(NDAng - 1) ! PI/90.d0
C                    if(ie.ne.1) then
C                      check_DE(ie) = 2.0d0*PI*csum*PI/(NDAng - 1) ! PI/90.d0
C                    else
C                      check_DE(ie) =       PI*csum*PI/(NDAng - 1) ! PI/90.d0
C                    endif
                   ENDDO
C
C                  This is a patch to correct the printed DDXS
C                  for DWBA to the continuum
C
C                  if(nejc.eq.7) then
C                   DO ie = 1, nspec + 1
C                    IF(check_DE(ie).le.0) cycle
C                     DO nang = 1, NDANG
C                      cseaprnt(ie,nang) = 
C    >                       POPcse(0,nejc,ie,INExc(nnuc))/check_DE(ie)*
C    >                 cseaprnt(ie,nang)
C                      check_DE(ie) = POPcse(0,nejc,ie,INExc(nnuc)) 
C                    ENDDO
C                   ENDDO
C                  endif

                   DO ie = 1, nspec 
                                     ! print DDX spectrum
                     if(check_DE(ie).LE.0) cycle ! skipping zeroes

                     WRITE (12,'(F10.5,E14.5,7E15.5,/,(9X,8E15.5))')
     &                     FLOAT(ie - 1)*DE/recorp,
     &                     (cseaprnt(ie,nang)*recorp,nang = 1,NDANG)
                   ENDDO
                                     ! exact DDX spectrum endpoint
                   WRITE (12,'(F10.5,E14.5,7E15.5,/,(9X,8E15.5))')
C                    A different way of calculating the Q-value using the 
C                    residual and ejectile
C    &               min((EMAx(nnur)-Q(nejc,nnur))/recorp,
     &               min(EMAx(nnuc)/recorp,
     &               FLOAT(nspec)*DE/recorp),
     &                (cseaprnt(nspec + 1,nang)*recorp,nang = 1,NDANG)
                   WRITE (12,*) ' '    
C
C                  Integrated spectrum
C
                   IF ((nnuc.EQ.mt91  .AND. nejc.EQ.1) .OR.
     &                 (nnuc.EQ.mt649 .AND. nejc.EQ.2) .OR.
     &                 (nnuc.EQ.mt849 .AND. nejc.EQ.3) ) THEN
                     WRITE (12,
     &              '(4x,''Lev #'',5x,''Integrated Discrete Spectra'')')
                     WRITE (12,
     &    '(10x,''    Energy    Int-DDX[mb]      Disc.Lev.XS     Differe
     &nce   Diff[%]     Elev'')')
                     WRITE (12,*) ' '
                     htmp = 0.d0
                     DO il = 1, NLV(nnuc)  ! discrete levels
                       if(check_DL(il).LE.0) cycle ! skipping zeroes
C                      espec is the outgoing energy corresponding to the level "il"
                       espec = (EMAx(nnuc) - ELV(il,nnuc))/recorp
                       IF (espec.LT.0) cycle 
                       WRITE(12, 
     &                   '(4x,I3,4x,F10.5,3(E14.6,2x),F6.2,4x,F7.4)')  
     &                   il,  -espec, check_DL(il),
     &                    CSDirsav(il,nejc),
     &                   (check_DL(il)-CSDirsav(il,nejc)),
     &                   (check_DL(il)-CSDirsav(il,nejc))/
     &                    check_DL(il)*100,  ELV(il,nnuc)        
                         htmp = htmp + check_DL(il)         
                     ENDDO
                     WRITE (12,*) ' '
                     WRITE (12,'(7X,''Integral of discrete-level DDXS '',
     &                G12.6,'' mb ('',A2,'')'')') htmp, SYMbe(nejc)
                     WRITE (12,'(7X,''Population of discrete levels   '',
     &               ,G12.6,'' mb'')') CSDirlev(1,nejc) 
                     WRITE (12,*) ' '
                   ENDIF
                   WRITE (12,'(15x,''Integrated Emission Spectra (printe
     &d DDXS corrected) - consistency check,  Ein ='',F9.5,'' MeV, nejc 
     &= '',i1)') EINl, nejc
                   WRITE (12,'(10x,
     &             ''    Energy      mb/MeV   Int-DDX[mb/MeV]       Diff
     &           Diff[%]    '')')
                   WRITE (12,*) ' '
                   csum  = 0.d0
                   csum1 = 0.d0
                   DO ie = 1, nspec 
                      ftmp = POPcse(0,nejc,ie,INExc(nnuc))             
                      if(ftmp.LE.0.d0) cycle
                      itmp = 1 
                      if (ie.eq.1) itmp = 2
                      WRITE (12,'(10x,F10.5,3(E14.5,1x),4x,F6.2)') 
     &                FLOAT(ie - 1)*DE/recorp, ftmp*recorp, 
     &                check_DE(ie)*recorp,
     &                (ftmp - check_DE(ie)) * recorp, 
     &                (ftmp - check_DE(ie)) / ftmp * 100
                      csum  = csum  + ftmp/itmp
                      csum1 = csum1 + check_DE(ie)/itmp
                   ENDDO
                   ! exact endpoint
                   WRITE (12,'(10x,F10.5,3(E14.5,1x),4x,F6.2)') 
C                    A different way of calculating the Q-value using the 
C                    residual and ejectile
C    &               min((EMAx(nnur)-Q(nejc,nnur))/recorp,
     &               min(EMAx(nnuc)/recorp,
     &                   FLOAT(nspec)*DE/recorp),
     &               max(0.d0,POPcse(0,nejc,nspec+1,
     &               INExc(nnuc)))*recorp,
     &               check_DE(nspec+1)*recorp,
     &               ( max(0.d0,POPcse(0,nejc,nspec+1,INExc(nnuc))) - 
     &                  check_DE(nspec+1) )*recorp, 0.d0

                   WRITE(12,*) 
                   WRITE(12,'(10x,12x,2(A14,1x))') 
     &                ' Integral(DE) ', ' Integ (DDXS) '
                   WRITE(12,'(10x,10x,2(E14.5,1x))') csum*DE,csum1*DE 
                   WRITE(12,*) 
                   if(dtmp.gt.0) then
                     WRITE(12,'(10x,
     &            ''Ave.  E  '',A2,'' cont.spec '',G12.6,'' MeV  for '',
     &            I3,''-'',A2,''-'',I3,A21)') SYMbe(Nejc),esum/dtmp,
     &               INT(Z(nnuc)),SYMb(nnuc),INT(A(nnuc)),REAction(nnuc)     

                     xsdisc = 0.d0
                     IF (nnuc.EQ.mt91 ) xsdisc = CSDirlev(1,1)
                     IF (nnuc.EQ.mt649) xsdisc = CSDirlev(1,2)
                     IF (nnuc.EQ.mt849) xsdisc = CSDirlev(1,3)

                     cmul = dtmp/(CSPrd(nnuc)-xsdisc)

                     WRITE(12,'(10x,
     &                ''Ave. <Q> '',A2,'' cont.spec '',G12.6,'' MeV'')') 
     &                 SYMbe(Nejc),esum/(CSPrd(nnuc)-xsdisc)
                     WRITE(12,'(10x,A2,
     &                '' multiplicity       '',G12.6)') SYMbe(Nejc),cmul
                   endif
                   WRITE(12,*) 
                   WRITE(12,'(10x,
     &              ''Integral '',A2,'' cont.spec '',G12.6,'' mb''  )') 
     &             SYMbe(Nejc),dtmp

                   WRITE(12,'(10x,
     &                ''Popul. cross section  '',G12.6,'' mb'' )') 
     &                  POPcs(nejc,INExc(nnuc))
                   WRITE(12,*) 

                ELSE !  (nejc=0) GAMMAS
C
C------------------Exclusive DE spectra (gammas)
C
C                  write(*,*) 'Emax(nnuc)=',EMAx(nnuc),nnuc
C                  write(*,*) 'Emaxd=',EMAx(1)-Q(4,1)
C                  write(*,*) 'Emaxpn=',EMAx(MT649)-Q(1,MT649)
C                  write(*,*) 'Emaxnp=',EMAx(MT91) -Q(2,MT91)
                              
                   IF(NINT(A(1)-A(nnuc)).eq.2 .and. ! gammas 
     &                NINT(Z(1)-Z(nnuc)).eq.1) THEN ! from deuteron residual 
C
C                     To split gamma spectra in (z,d) and (z,np+pn)
C
C                     First, gammas from (z,np+pn) 
                      WRITE (12,*) ' '
                      WRITE (12,*) ' Spectrum of ', cejectile,
     &                       '(z,np)               ', ' ZAP= ', iizaejc
                      emax_np  = EMAx(MT91) -Q(2,MT91)
                      nspec_np = min(INT(emax_np/DE) + 1,NDECSE-1)
                      WRITE (12,*) ' '
                      WRITE (12,'(''    Energy    mb/MeV'')')
                      WRITE (12,*) ' '
                      dtmp =0.d0          
                      esum =0.d0          
                      DO ie = 1, nspec_np +1 !       
                        htmp = CSEnp(ie)  ! POPcse(0,nejc,ie,INExc(nnuc))*gexclus          
                        if(htmp.LE.0.d0) cycle
                        itmp = 1
                        if(ie.eq.1) itmp = 2
                        dtmp = dtmp + htmp*DE/itmp
                        WRITE (12,'(F10.5,E14.5)') FLOAT(ie - 1)*DE,htmp
                      ENDDO
                      WRITE (12,'(F10.5,E14.5)') 
     &                    min(emax_np,FLOAT(nspec_np+1)*DE), 0.d0
                      if(dtmp.gt.0) then
                        csnp = dtmp
                        WRITE(12,*) 
                        WRITE(12,'(2x,'' g multiplicity (np)  '',
     &                  G12.6)') dtmp/CSPrd(nnuc)
                        WRITE(12,*) 
C                       WRITE(12,'(2x,
C    &                    ''Total Integr.(gamma)np'',G12.6,'' mb'' )') 
C    &                  csnp
                              endif
C                     Second, remaining gammas from (z,d) 
                      WRITE (12,*) ' '
                      WRITE (12,*) ' Spectrum of ', cejectile,
     &                         REAction(nnuc), ' ZAP= ', iizaejc
                      nspec= min(INT(EMAx(nnuc)/DE) + 1,NDECSE-1)
                      WRITE (12,*) ' '
                      WRITE (12,'(''    Energy    mb/MeV             (z,
     &np)       (z,np+pn+d)'')')
                      WRITE (12,*) ' '
                      dtmp =0.d0          
                      esum =0.d0 
                              iemm = 1         
                      DO ie = 1, nspec + 1        
                        htmp = POPcse(0,nejc,ie,INExc(nnuc))-CSEnp(ie)
                        if(htmp.LE.0.d0) cycle
                        iemm = ie - 1
                        itmp = 1
                        if(ie.eq.1) itmp = 2
                        dtmp = dtmp + htmp*DE/itmp
                        esum = esum + htmp*DE/itmp*FLOAT(ie - 1)*DE
                        WRITE (12,'(F10.5,E14.5,2x,E14.5,2x,E14.5)') 
     &                    FLOAT(ie - 1)*DE,htmp,
     &                    CSEnp(ie),POPcse(0,nejc,ie,INExc(nnuc))
                      ENDDO
C                       A different way of calculating the Q-value using the 
C                       residual and ejectile
C    &                  min((EMAx(nnur)-Q(nejc,nnur))/recorp,
                      WRITE (12,'(F10.5,E14.5,2x,E14.5,2x,E14.5)') 
     &                 min(EMAx(nnuc),FLOAT(iemm+1)*DE),0.d0,0.d0,0.d0  
                      WRITE(12,*) 
                      if(dtmp.gt.0) then
                        WRITE(12,'(2x,
     &                  ''Ave.  E   g cont.spec '',G12.6,'' MeV  for '',
     &                    I3,''-'',A2,''-'',I3,A21)') esum/dtmp,
     &                    INT(Z(nnuc)),SYMb(nnuc),
     &                    INT(A(nnuc)),REAction(nnuc) 
                       cmul = dtmp/CSPrd(nnuc)
                       WRITE(12,'(2x,
     &                  ''Ave.  Q   g cont.spec '',G12.6,'' MeV'')') 
     &                    cmul*esum/dtmp  
                       WRITE(12,'(2x,'' g multiplicity (d)   '',G12.6)') 
     &                    cmul
                      endif
                      WRITE(12,*) 
                      WRITE(12,'(2x,
     &                  ''Total Integr.(gamma)np'',G12.6,'' mb'' )') 
     &                  csnp 
                      WRITE(12,'(2x,
     &                  ''Total Integr.(gamma)d '',G12.6,'' mb'' )') 
     &                  dtmp 
                      WRITE(12,'(2x,
     &                  ''Total Integr.(gamma)  '',G12.6,'' mb'' )') 
     &                  csnp+dtmp 
                      WRITE(12,'(2x,
     &                  ''Popul. cross section  '',G12.6,'' mb'' )') 
     &                  POPcs(nejc,INExc(nnuc))

                      GOTO 1515 ! skipping other gammas for this reaction
                   ENDIF

                   WRITE (12,*) ' '
                   WRITE (12,*) ' Spectrum of ', cejectile,
     &                         REAction(nnuc), ' ZAP= ', iizaejc
                   WRITE (12,*) ' '
                   WRITE (12,'(''    Energy    mb/MeV'')')
                   WRITE (12,*) ' '
                   dtmp =0.d0          
                   esum =0.d0          
                   iemm = 1         
                   DO ie = 1, nspec  !       
                     htmp = POPcse(0,nejc,ie,INExc(nnuc))*gexclus          
                     if(htmp.LE.0.d0) cycle
                     iemm = ie -1
                     itmp = 1
                     if(ie.eq.1) itmp = 2
                     dtmp = dtmp + htmp*DE/itmp
                     esum = esum + htmp*DE/itmp*FLOAT(ie - 1)*DE
                     WRITE (12,'(F10.5,E14.5)') FLOAT(ie - 1)*DE, htmp
                   ENDDO
C                  WRITE (12,'(F10.5,E14.5)') EMAx(nnuc), 0.d0
                   WRITE (12,'(F10.5,E14.5)') 
C                    A different way of calculating the Q-value using the 
C                    residual and ejectile
C    &               min((EMAx(nnur)-Q(nejc,nnur))/recorp,
     &               min(EMAx(nnuc),FLOAT(iemm+1)*DE),
     &               POPcse(0,nejc,nspec+1,INExc(nnuc))*gexclus
                   if(dtmp.gt.0) then
                     WRITE(12,*) 
                     WRITE(12,'(2x,
     &                 ''Ave.  E   g cont.spec '',G12.6,'' MeV  for '',
     &                 I3,''-'',A2,''-'',I3,A21)') esum/dtmp,
     &               INT(Z(nnuc)),SYMb(nnuc),INT(A(nnuc)),REAction(nnuc) 

                     xsdisc = 0.d0
                     IF (nnuc.EQ.mt91 ) xsdisc = CSDirlev(1,1)
                     IF (nnuc.EQ.mt649) xsdisc = CSDirlev(1,2)
                     IF (nnuc.EQ.mt849) xsdisc = CSDirlev(1,3)
                     WRITE(12,'(2x,'' g discr.levels       '',G12.6)') 
     &                 xsdisc

                     cmul = dtmp/(CSPrd(nnuc)-xsdisc)
                     WRITE(12,'(2x,
     &                 ''Ave.  Q   g cont.spec '',G12.6,'' MeV'')') 
     &                 cmul*esum/dtmp  
                     WRITE(12,'(2x,'' g multiplicity       '',G12.6)') 
     &                 cmul
 
                   endif
                   WRITE(12,*) 
                   WRITE(12,'(2x,
     &                  ''Total Integr.(gamma)  '',G12.6,'' mb'' )') 
     &                  dtmp 
                   WRITE(12,'(2x,
     &                  ''Popul. cross section  '',G12.6,'' mb'' )') 
     &                  POPcs(nejc,INExc(nnuc))*gexclus
 1515              WRITE(12,*) 

                ENDIF !  (nejc.GT.0)
 1530         ENDDO   ! over ejectiles

              qin     = EIN  + QPRod(nnuc) + ELV(LEVtarg,0) ! CMS
              qinaver = qin
              if(ncontr(nnuc).gt.1) 
     &        qinaver = EIN  + QQInc(nnuc)/ncontr(nnuc) + ELV(LEVtarg,0) ! CMS

              IF (NINT(A(1)-A(Nnuc)).GT.4 )  GOTO 1550

              IF (NINT(A(1)-A(Nnuc)).EQ.4 .AND. 
     &            NINT(Z(1)-Z(Nnuc)).EQ.1) GOTO 1550  ! 3np

              IF (NINT(A(1)-A(Nnuc)).EQ.4 .AND. 
     &            NINT(Z(1)-Z(Nnuc)).EQ.3) GOTO 1550  ! 2pd

              IF (NINT(A(1)-A(Nnuc)).EQ.3 .AND. 
     &            NINT(Z(1)-Z(Nnuc)).EQ.3) GOTO 1550  ! 3p

C             IF (NINT(A(1)-A(Nnuc)).EQ.3 .AND. 
C    &            NINT(Z(1)-Z(Nnuc)).EQ.2) GOTO 1550  ! pd

              IF(RECoil.gt.0) then
                IF (NINT(A(1)-A(Nnuc)).GT.1 .AND. 
     &              NINT(A(1)-A(Nnuc)).LE.4) THEN 
C                  (n,xn),(n,xp) x>1; (n,d),(n,t),(n,h),(n,a)
                   CALL PRINT_RECOIL(nnuc,REAction(nnuc))
C                  write(*,*) 'print_recoil     :',trim(REAction(nnuc)),
C    &                NINT(A(nnuc)),NINT(Z(nnuc))
                ENDIF
                IF (NINT(A(1)-A(Nnuc)).EQ.1) THEN !  n or p emission
                   CALL PRINT_BIN_RECOIL(nnuc,REAction(nnuc))
C                  write(*,*) 'print_bin_recoil :',trim(REAction(nnuc)),
C    &                NINT(A(nnuc)),NINT(Z(nnuc))
                ENDIF
              ENDIF

              WRITE(12,*)
              WRITE(8,*)
              
           ENDIF ! IF (CSPrd(nnuc).GT.0.0D0)
         ENDIF ! IF (ENDf(nnuc).EQ.1)

C********************************************
1550     CALL PFNS_calc(nnuc)
C********************************************

      ENDDO  ! loop over residues (not decaying nuclei)

      if(allocated(cseaprnt)) deallocate(cseaprnt)
      if(allocated(check_DE)) deallocate(check_DE)
      if(allocated(CSEnp)) deallocate(CSEnp)

C********************************************
      CALL print_PFNS()
      RETURN
      END
