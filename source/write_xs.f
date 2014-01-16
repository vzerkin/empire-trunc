Ccc   * $Rev: 3705 $
Ccc   * $Author: rcapote $
Ccc   * $Date: 2014-01-04 22:01:02 +0100 (Sat, 04 Jan 2014) $

      SUBROUTINE write_xs()
      USE empcess, ONLY: POPcsea, disc_int, check_DL 

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
      INTEGER nnuc,nejc,nnur,iloc,nspec,nang,il,ie,iizaejc,myalloc
      DOUBLE PRECISION recorp,espec,csum,esum,qin,qout,cmul,xsdisc
      DOUBLE PRECISION dtmp,htmp,ftmp   
C     DOUBLE PRECISION cseaprnt(ndecse,ndangecis),check_DE(ndecse)
      DOUBLE PRECISION, ALLOCATABLE :: cseaprnt(:,:),check_DE(:)

      if(allocated(cseaprnt)) deallocate(cseaprnt)
 
      ALLOCATE(cseaprnt(ndecse,ndangecis),STAT=myalloc)
      IF(myalloc.NE.0) THEN
        WRITE(8,*)  
     &		'ERROR: Insufficient memory for cseaprnt: write_xs()!'
        WRITE(12,*) 
     &		'ERROR: Insufficient memory for cseaprnt: write_xs()!'
        STOP
     &		'ERROR: Insufficient memory for cseaprnt: write_xs()!'
      ENDIF

      if(allocated(check_DE)) deallocate(check_DE)

      ALLOCATE(check_DE(ndecse),STAT=myalloc)
      IF(myalloc.NE.0) THEN
        WRITE(8,*)  
     &		'ERROR: Insufficient memory for check_DE: write_xs()!'
        WRITE(12,*) 
     &		'ERROR: Insufficient memory for check_DE: write_xs()!'
        STOP
     &		'ERROR: Insufficient memory for check_DE: write_xs()!'
      ENDIF
C
C-----Write a row in the table of cross sections (Note: inelastic has CN elastic subtracted)
cccccccccccccccccccc ccccccccccccccccccccccccccccc
C-----Reaction Cross Sections lower than 1.d-8 are considered zero.
      IF(TOTcsfis.gt.0.d0) WRITE(98,'(1P,E10.4,1x,1P,(30E12.5))') 
     &  EINl, TOTcsfis, (CSPfis(nnuc),nnuc=1,min(NNUcd,10,max_prn-1))
      CLOSE (80)
      CLOSE (79)
      WRITE (12,*) 
      WRITE (12,'('' Tot. fission cross section '',G12.4,'' mb'')')
     &       TOTcsfis

      IF(ENDf(1).GT.0) THEN 
        WRITE (12,*) 
        WRITE (12,*) '*******************************************'
        WRITE (12,*) '* EMISSION SPECTRA at Einc =', sngl(EINl) 
        WRITE (12,*) '*******************************************'
        WRITE (12,*) 
      ENDIF 
      WRITE (8,*) 
      WRITE (8,'(''  Tot. fission cross section '',G12.4,'' mb'')')
     &       TOTcsfis
C
C---- ENDF spectra printout (exclusive representation)
C----
      DO nnuc = 1, NNUcd  ! loop over residues (not decaying nuclei)
         IF (ENDf(nnuc).EQ.1) THEN
           IF (CSPrd(nnuc).GT.0.0D0) THEN
             qout = 0.d0			  
             DO nejc = 0, NDEJC         !loop over ejectiles
                IF (POPcs(nejc,INExc(nnuc)).EQ.0.d0) CYCLE
                IF(A(nnuc).LE.4. AND. Z(nnuc).LE.2.) CYCLE
                IF(nejc.GT.0) THEN
                  CALL WHERE(IZA(nnuc)+IZAejc(nejc),nnur,iloc)
                ELSE
                  nnur = nnuc
                  iloc = 0
                ENDIF
                IF(iloc.NE.0) CYCLE

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
                WRITE (12,*) ' '
                WRITE (12,*) ' Spectrum of ', cejectile,
     &                         REAction(nnuc), ' ZAP= ', iizaejc
C
C---------------Exclusive DDX spectra (all particles but gammas)
                recorp = 1.d0
                nspec= min(INT(EMAx(nnuc)/DE) + 1,NDECSE-1)
C               dang = PI/FLOAT(NDANG - 1)
                IF (nejc.GT.0) THEN
C---------------recorp is a recoil correction factor defined 1+Ap/Ar that
C---------------multiplies cross sections and divides outgoing energies
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
c                     Discrete level cros section CSAlev contains angular distribution, NOT DDXS
c                     recoil correction is not needed as we do not integrate over energy 
c                       We only sum over discrete levels ! 
     &                     (max(CSAlev(nang,il,nejc), 
c    &                     (max(CSAlev(nang,il,nejc)*recorp/DE,
     &                               0.d0),nang = 1,NDANG)
                        csum = 0.d0
                        DO nang = 2, NDANG  ! over angles
                          csum = csum + (CSAlev(nang,il,nejc) 
     &                                  + CSAlev(nang-1,il,nejc))
     &                         * 0.5d0 * (CAngler(nang)-CANgler(nang-1))
                        ENDDO
                        check_DL(il) = max(2.0d0*PI*csum,1.d-10)
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
                     ftmp = 1.d0
C                    if(ie.eq.1 .or. ie.eq.nspec) ftmp=0.5d0
                     if(ie.eq.1 .or. ie.eq.nspec + 1) ftmp=0.5d0
                     dtmp = dtmp + htmp*DE*ftmp
                     esum = esum + htmp*DE*ftmp*FLOAT(ie - 1)*DE/recorp
                     ftmp = max((htmp - xnorm(nejc,INExc(nnuc))
     &                    * POPcsed(0,nejc,ie,INExc(nnuc)))/4.0/PI,0.d0)
                     csum = 0.d0
                     IF(LHMs.GT.0 .AND. (nejc.EQ.1 .OR. nejc.EQ.2)) THEN
C----------------------Check whether integral over angles agrees with DE spectra
                       DO nang = 1, NDANG
                         cseaprnt(ie,nang) = 
     &                     ftmp + xnorm(nejc,INExc(nnuc))*
     &                            POPcsea(nang,0,nejc,ie,INExc(nnuc))
                           IF(nang.GT.1) csum = csum 
     &                         + (cseaprnt(ie,nang)+cseaprnt(ie,nang-1))
     &                         * 0.5d0 * (CAngler(nang)-CANgler(nang-1))
                       ENDDO
                     ELSE
c The following is equivalent the definition of ftmp above, when LHMs=0.
c                      ftmp =(POPcse(0,nejc,ie,INExc(nnuc))-
c     &                  CSEmsd(ie,nejc)*POPcseaf(0,nejc,ie,INExc(nnuc))/4.0/PI
                       DO nang = 1, NDANG
                         cseaprnt(ie,nang) =
     &                     ftmp + CSEa(ie,nang,nejc,1)*
     &                            POPcseaf(0,nejc,ie,INExc(nnuc))
                           IF(nang.GT.1) csum = csum 
     &                         + (cseaprnt(ie,nang)+cseaprnt(ie,nang-1))
     &                         * 0.5d0 * (CAngler(nang)-CANgler(nang-1))
                       ENDDO
                     ENDIF
                     if(ie.ne.1) then
                       check_DE(ie) = 2.0d0*PI*csum
	               else
                       check_DE(ie) =       PI*csum
                     endif
                   ENDDO

                   DO ie = 1, nspec 
                                     ! print DDX spectrum
                     if(check_DE(ie).LE.0.d0) cycle ! skipping zeroes

                     WRITE (12,'(F10.5,E14.5,7E15.5,/,(9X,8E15.5))')
     &                     FLOAT(ie - 1)*DE/recorp,
     &                     (cseaprnt(ie,nang)*recorp,nang = 1,NDANG)
                   ENDDO
                                     ! exact DDX spectrum endpoint
                   WRITE (12,'(F10.5,E14.5,7E15.5,/,(9X,8E15.5))')
     &               EMAx(nnuc)/recorp,
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
     &nce     Elev'')')
                     WRITE (12,*) ' '
                     htmp = 0.d0
                     DO il = 1, NLV(nnuc)  ! discrete levels
C                      espec is the outgoing energy corresponding to the level "il"
                       espec = (EMAx(nnuc) - ELV(il,nnuc))/recorp
                       IF (espec.LT.0) cycle 
                       WRITE (12,'(4x,I3,4x,F10.5,3(E14.5,2x),F7.4)')  
     &                   il, -espec, check_DL(il)             ,! *recorp,
     &                        disc_int(il,nejc)               ,! *recorp,
     &                        (check_DL(il)-disc_int(il,nejc)),!*recorp,
     &                        ELV(il,nnuc)        
                           htmp = htmp + check_DL(il)          !*recorp
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
                   DO ie = 1, nspec 
                      ftmp = POPcse(0,nejc,ie,INExc(nnuc))             
	                if (ie.eq.1) ftmp = ftmp*0.5d0
                      if(ftmp.LE.0.d0) cycle
                      WRITE (12,'(10x,F10.5,3(E14.5,1x),4x,F6.2)') 
     &                FLOAT(ie - 1)*DE/recorp, ftmp*recorp, 
     &                check_DE(ie)*recorp,
     &                (ftmp - check_DE(ie)) * recorp, 
     &                (ftmp - check_DE(ie)) / ftmp * 100
                   ENDDO
                                        ! exact endpoint
                   WRITE (12,'(10x,F10.5,3(E14.5,1x),4x,F6.2)') 
     &               EMAx(nnuc)/recorp,max(0.d0,POPcse(0,nejc,nspec+1,
     &               INExc(nnuc)))*recorp,
     &               check_DE(nspec+1)*recorp,
     &               ( max(0.d0,POPcse(0,nejc,nspec+1,INExc(nnuc))) - 
     &                  check_DE(nspec+1) )*recorp, 0.d0

                   WRITE(12,*) 
                   if(dtmp.gt.0) then
                     WRITE(12,'(10x,
     &            ''Ave. <E> '',A2,'' cont.spec '',G12.6,'' MeV  for '',
     &            I3,''-'',A2,''-'',I3,A21)') SYMbe(Nejc),esum/dtmp,
     &               INT(Z(nnuc)),SYMb(nnuc),INT(A(nnuc)),REAction(nnuc)     

	               xsdisc = 0.d0
                     IF (nnuc.EQ.mt91 ) xsdisc = CSDirlev(1,1)
                     IF (nnuc.EQ.mt649) xsdisc = CSDirlev(1,2)
                     IF (nnuc.EQ.mt849) xsdisc = CSDirlev(1,3)

                     cmul = dtmp/(CSPrd(nnuc)-xsdisc)
                     qout = qout + esum/(CSPrd(nnuc)-xsdisc)

                     WRITE(12,'(10x,
     &                ''Ave. <Q> '',A2,'' cont.spec '',G12.6,'' MeV'')') 
     &                 SYMbe(Nejc),esum/(CSPrd(nnuc)-xsdisc)
                     WRITE(12,'(10x,A1,
     &                '' multiplicity       '',G12.6)') SYMbe(Nejc),cmul
                   endif
                   WRITE(12,*) 
                   WRITE(12,'(10x,
     &              ''Integral '',A1,'' cont.spec '',G12.6,'' mb''  )') 
     &             SYMbe(Nejc),dtmp

                   WRITE(12,'(10x,
     &                ''Popul. cross section '',G12.6,'' mb'' )') 
     &                  POPcs(nejc,INExc(nnuc))
                   WRITE(12,*) 

                ELSE !  (nejc=0) GAMMAS
C
C------------------Exclusive DE spectra (gammas)
                   WRITE (12,*) ' '
                   WRITE (12,'(''    Energy    mb/MeV'')')
                   WRITE (12,*) ' '
                   dtmp =0.d0          
                   esum =0.d0          
                   DO ie = 1, nspec + 1     
                     htmp = POPcse(0,nejc,ie,INExc(nnuc))          
                     if(htmp.LE.0.d0) cycle
                     ftmp = 1.d0
                     if(ie.eq.1 .or. ie.eq.nspec+1) ftmp = 0.5d0
                     dtmp = dtmp + htmp*DE*ftmp
                     esum = esum + htmp*DE*ftmp*FLOAT(ie - 1)*DE
                     WRITE (12,'(F10.5,E14.5)') FLOAT(ie - 1)*DE, htmp
                   ENDDO
                   WRITE (12,'(F10.5,E14.5)') EMAx(nnuc),  0.d0
                   if(dtmp.gt.0) then
                     WRITE(12,*) 
                     WRITE(12,'(2x,
     &                 ''Ave. <E>  g cont.spec '',G12.6,'' MeV  for '',
     &                 I3,''-'',A2,''-'',I3,A21)') esum/dtmp,
     &               INT(Z(nnuc)),SYMb(nnuc),INT(A(nnuc)),REAction(nnuc) 

                     xsdisc = 0.d0
                     IF (nnuc.EQ.mt91 ) xsdisc = CSDirlev(1,1)
                     IF (nnuc.EQ.mt649) xsdisc = CSDirlev(1,2)
                     IF (nnuc.EQ.mt849) xsdisc = CSDirlev(1,3)
                     WRITE(12,'(2x,''g discr.levels       '',G12.6)') 
     &                 xsdisc

                     cmul = dtmp/(CSPrd(nnuc)-xsdisc)
                     qout = qout + cmul*esum/dtmp
                     WRITE(12,'(2x,
     &                 ''Ave. <Q> g cont.spec '',G12.6,'' MeV'')') 
     &                 cmul*esum/dtmp  
                     WRITE(12,'(2x,''g multiplicity       '',G12.6)') 
     &                 cmul
 
                   endif
                   WRITE(12,*) 
                   WRITE(12,'(2x,
     &                  ''Total Integr.(gamma) '',G12.6,'' mb'' )') 
     &                  dtmp 
                   WRITE(12,'(2x,
     &                  ''Popul. cross section '',G12.6,'' mb'' )') 
     &                  POPcs(nejc,INExc(nnuc))
                   WRITE(12,*) 

                ENDIF !  (nejc.GT.0)
 1530         ENDDO   ! over ejectiles

              IF ((A(1)-A(nnuc)).GT.1  .AND. RECoil.GT.0) THEN
                 CALL PRINT_RECOIL(nnuc,REAction(nnuc),qout)
              ELSEIF((A(1)-A(nnuc)).EQ.1  .AND. RECoil.GT.0) THEN
                 CALL PRINT_BIN_RECOIL(nnuc,REAction(nnuc),qout)
              ENDIF

 	        qin = EIN  + QPRod(nnuc) + ELV(LEVtarg,0) ! CMS

              WRITE(12,'( 2x,
     &            '' Total <Q> cont.spec '',G12.6,'' MeV  for '',
     &         I3,''-'',A2,''-'',I3,'' decay'')') qout,
     &         INT(Z(nnuc)), SYMb(nnuc), INT(A(nnuc)) 
              WRITE(12,'( 2x,
     &         '' Qin                 '',G12.6,'' MeV'')') qin
              WRITE(12,'( 2x,
     &         '' Energy balance      '',G12.6,'' MeV ('',F6.2,''%)'')')
     &           qin - qout, (qin - qout)/qin*100
			 
	        WRITE(12,*) 
           ENDIF ! IF (CSPrd(nnuc).GT.0.0D0)
         ENDIF ! IF (ENDf(nnuc).EQ.1)

C********************************************
         CALL PFNS_calc(nnuc)
C********************************************

      ENDDO  ! loop over residues (not decaying nuclei)

      if(allocated(cseaprnt)) deallocate(cseaprnt)
      if(allocated(check_DE)) deallocate(check_DE)

C********************************************
      CALL print_PFNS()
      RETURN
      END
