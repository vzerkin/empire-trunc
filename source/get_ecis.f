Ccc   * $Rev: 4684 $
Ccc   * $Author: rcapote $
Ccc   * $Date: 2016-06-12 13:10:24 +0200 (So, 12 Jun 2016) $

      subroutine get_ecis_inelastic
     >  (nejcec,nnurec,ncollx,xscclow,totcorr)
      implicit none
      INCLUDE "dimension.h"
      INCLUDE "global.h"
      
      INTEGER nejcec,nnurec,ncollx 
      DOUBLE PRECISION xsinlcont,xscclow,totcorr

C     COMMON variables
      DOUBLE PRECISION ELAcs, TOTcs, ABScs, SINl, SINlcc, SINlcont       ! ECISXS
      COMMON /ECISXS/ ELAcs, TOTcs, ABScs, SINl, SINlcc, SINlcont

      DOUBLE PRECISION elada(NDAngecis), elleg(NDAngecis) 
      INTEGER neles 
      COMMON /angula/elada,elleg,neles

C     Local variables
      INTEGER isigma0, irec, iang, iang1, nspec,  
     & i, ilv, icsl, icsh, ncon, isigma, isigma2, ie,
     & imint, imaxt, j, its, iloc      
      DOUBLE PRECISION ecm, angstep, gang, ftmp, ggmr, ggqr, ggor,
     & xcse, popread, popl, poph, csum, echannel,  
     & coeff, erecoil, weight, csmsdl, dcor

      CHARACTER*23 ctmp23
      CHARACTER*3 ctldir
      CHARACTER*72 ctmp

      DATA ctldir/'TL/'/

      xsinlcont = 0.d0
      xscclow   = 0.d0
      elleg     = 0.d0
      elleg(1)  = 1.d0 
C
C     For resolution function (Spreading levels in the continuum)
      isigma0 = 0
      IF(WIDcoll.GT.0.d0)
     &   isigma0 = INT((0.02d0  + sqrt(EINl)*WIDcoll)/DE + 1.0001)
      ncollx = 0
      ecm = EINl - EIN

      angstep = 180.d0/(NANgela-1)
      gang = 180.d0/(NDAng-1)

      WRITE (ctmp23,'(i3.3,i3.3,1h_,i3.3,i3.3,1h_,i9.9)') INT(ZEJc(0)),
     &       INT(AEJc(0)), INT(Z(0)), INT(A(0)), INT(EINl*1000000)
C-----
C-----Get ECIS results for the ground state
C-----
      IF (ICAlangs.GT.0) THEN
        OPEN (45,FILE = (ctldir//ctmp23//'.EXP'),STATUS = 'OLD',
     &      ERR = 1400)
        READ (45,*,END = 1400)   ! To skip first line <ANG.DIS.> ..
        READ (45,*,END = 1400)   ! To skip level identifier line
        DO iang = 1, NANgela
         READ (45,'(24x,D12.5)',END = 1400) elada(iang)
        ENDDO

      ELSE

        OPEN (45,FILE = (ctldir//ctmp23//'.LEG'),STATUS = 'OLD',
     &      ERR = 1400)
        READ (45,*,END = 1400,ERR = 1400) ctmp ! To skip first line <LEGENDRE> ..
        READ (45,'(5x,i5)',END = 1400,ERR = 1400) neles
        DO iang = 1, min(NDAng,neles)
           READ (45,'(10x,D20.10)',END = 1400,ERR = 1400) elleg(iang)
        ENDDO
        CLOSE(45)

        OPEN (45,FILE = (ctldir//ctmp23//'.ANG'),STATUS = 'OLD',
     &      ERR = 1400)
        READ (45,*,END = 1400) ctmp  ! To skip first line <ANG.DIS.> ..
        READ (45,*,END = 1400) ctmp  ! To skip level identifier line
        iang = 0
        DO iang1 = 1, NANgela
C----------To use only those values corresponding to EMPIRE grid for elastic XS
           READ (45,'(3x,12x,D12.5)',END = 1400) ftmp    ! ecis06
           if(mod(DBLE(iang1-1)*angstep+gang,gang).NE.0) cycle
           iang = iang +1
           elada(iang) = ftmp
        ENDDO
      ENDIF
C-----Check whether integral over angles agrees with x-sec. read from ECIS
      csum = 0.d0
      DO iang = 1, NDANG  ! over angles
        csum = csum + elada(iang)*SANgler(iang)*PI/(NDAng-1) !/90.d0
      ENDDO
      csum = 2.0d0*PI*csum 
C     write(*,*) ELAcs,csum,' SIN'
      if (csum.gt.0.d0) then
C-------Correct elada() for eventual imprecision
        ftmp = ELAcs/csum
        DO iang = 1, NANgela
          elada(iang)=elada(iang)*ftmp
        ENDDO
      endif
C
C-----
C-----Get ECIS results for excited levels
C-----
      IF (DIRect.NE.0) THEN
          ggmr = 3.d0
          ggqr =85.d0*A(0)**(-2./3.)
          ggor =5.d0
          nspec= min(INT(EMAx(nnurec)/DE) + 1,NDECSE-1)
          OPEN (46,FILE = (ctldir//ctmp23//'.ICS'),STATUS = 'OLD',
     &         ERR = 1400)
          READ (46,*,END = 1400) ctmp ! To skip first line <INE.C.S.> ..
C---------Get and add inelastic cross sections (including double-differential)
          DO i = 2, ND_nlv
 
           ilv = ICOller(i)

           IF(ilv.le.0) then
             WRITE(8,*) ' WARNING: Collective level #',ICOllev(i),
     &        ' has wrong number, skipped, Ecoll=',sngl(D_Elv(i))
             CYCLE                
           ENDIF

           IF(ICOllev(i).LT.LEVcc .and. SINlcc.le.0.d0) exit
           IF(ICOllev(i).GE.LEVcc .and. SINl+SINlcont.le.0.d0) cycle

           IF(ilv.LE.NLV(nnurec)) then

             IF(ICOllev(i).GE.LEVcc .and. SINl.le.0.d0) cycle

             IF(ICOller(i).GT.40) then
               WRITE(8,*) ' WARNING: Collective level #',ICOller(i),
     &             ' has wrong number (> 40), Ecoll=',sngl(D_Elv(i))
               ilv = 40                 
             ENDIF
C
C            D_Elv(i)        Collective level energy (in collective level file)
C            ELV(ilv,nnurec) Discrete level energy
             IF(ABS(D_Elv(i) - ELV(ilv,nnurec)).gt.0.001d0) THEN
               WRITE(8,*)
     &           ' WARNING: Check the collective level file  !!!'
               WRITE(8,*)
     &           ' WARNING: Energy of the collective level #',
     &             ICOllev(i),sngl(D_Elv(i))
               WRITE(8,*)
     &      ' WARNING: not equal to the energy of the discrete level #', 
     &             ilv,sngl(ELV(ilv,nnurec))
             ENDIF
C
C------------Adding inelastic to discrete levels
             echannel = EX(NEX(1),1) - Q(nejcec,1) - ELV(ilv,nnurec)
C------------Avoid reading closed channels
             IF (echannel.GE.0.0001) THEN
               xcse = echannel/DE + 1.0001
               icsl = INT(xcse)
               icsh = icsl + 1
               READ (46,*,END = 1400) popread
               popread = popread*FCCred
C
C              Storing the particle inelastic to the first two low-lying CC levels 
C              to compare with the experimental "quasi-elastic" scattering
C              for actinides. The experimental data are pure elastic below
C              3.5 MeV, but at those energies Sel >> Sinl
C
C              The change is avoided in sensitivity calculations as the 
C              ENDF-6 formatted output ALWAYS contains the unmodified elastic.
C                
              IF( INT(ZEJc(0)).EQ.0 .and. A(0).gt.220 .and. ilv.LT.3
     &         .and. KALman.eq.0  .and. INT(AEJc(0)).GT.0) 
     &            xscclow = xscclow + popread
C              To consider only open channels
               ncollx = i
C 
               POPlv(ilv,nnurec) = POPlv(ilv,nnurec) + popread
               CSDirlev(ilv,nejcec) = CSDirlev(ilv,nejcec) + popread
               CSEmis(nejcec,1) = CSEmis(nejcec,1) + popread
C--------------Add direct transition to the spectrum
               IF(icsl.LT.nspec) THEN
                 popl = popread*(FLOAT(icsh) - xcse)/DE
                 poph = popread*(xcse - FLOAT(icsl))/DE
               ELSE
                 popl = popread/DE
                 poph = 0.0d0
               ENDIF
C              it is not clear whether this is needed or not
C              IF (icsl.EQ.1) popl = 2.0*popl
               CSE(icsl,nejcec,1) = CSE(icsl,nejcec,1) + popl
               CSE(icsh,nejcec,1) = CSE(icsh,nejcec,1) + poph

               CSEt(icsl,nejcec) = CSEt(icsl,nejcec) + popl
               CSEt(icsh,nejcec) = CSEt(icsh,nejcec) + poph

               IF (ENDf(nnurec).NE.1) THEN
                 CSE(icsl,nejcec,0) = CSE(icsl,nejcec,0) + popl
                 CSE(icsh,nejcec,0) = CSE(icsh,nejcec,0) + poph
               ENDIF
                    
               IF (ICAlangs.GT.0) THEN
                IF (i.LE.ICAlangs) THEN
                  READ (45,'(A)',END = 1400) ctmp  ! Skipping level identifier line
                  DO iang = 1, NANgela
                    READ (45,'(24x,D12.5)',END = 1400) ftmp
                    CSAlev(iang,ilv,nejcec) = 
     >              CSAlev(iang,ilv,nejcec) + ftmp*FCCred
                  ENDDO
                ENDIF
               ELSE
                READ (45,'(A)',END = 1400) ctmp    ! Skipping level identifier line
                iang = 0
                DO iang1 = 1, NANgela
                  READ (45,'(3x,12x,D12.5)',END = 1400) ftmp    ! ecis06
C-----------------To use only those values corresponding to EMPIRE grid for inelastic XS
                  if(mod(DBLE(iang1-1)*angstep+gang,gang).NE.0) cycle
                  iang = iang +1
                  CSAlev(iang,ilv,nejcec) = CSAlev(iang,ilv,nejcec)
     &                                    + ftmp*FCCred
                ENDDO
               ENDIF
C--------------Check whether integral over angles agrees with x-sec. read from ECIS
               csum = 0.d0
               DO iang = 1, NANgela  ! over angles
                 csum = csum + CSAlev(iang,ilv,nejcec)*
     &                  SANgler(iang)*PI/(NDAng-1) !/90.d0
               ENDDO
               csum = 2.0d0*PI*csum 
               if (csum.gt.0.d0) then
C----------------Correct CSAlev() for eventual imprecision
                 ftmp = POPlv(ilv,nnurec)/csum
                 DO iang = 1, NANgela
                   CSAlev(iang,ilv,nejcec)=CSAlev(iang,ilv,nejcec)*ftmp
                 ENDDO
               endif
C--------------Construct recoil spectra due to direct transitions
               IF (ENDf(nnurec).GT.0 .AND. RECoil.GT.0) THEN
C-----------------Correct 'coeff' for eventual imprecision and include recoil DE
C                 coeff = coeff*POPlv(ilv,nnurec)/csum/DERec
                  coeff = 2*PI*PI/FLOAT(NANgela - 1)/DERec
                  echannel = echannel*EJMass(0)/AMAss(1)
                  DO iang = 1, NDANG
                     erecoil = ecm + echannel + 2*SQRT(ecm*echannel)
     &                         *CANgler(iang)
                     irec = erecoil/DERec + 1.001
                     weight = (erecoil - (irec - 1)*DERec)/DERec
C--------------------Escape if we go beyond recoil spectrum dimension
                     IF (irec + 1.GT.NDEREC) GOTO 1350
                     csmsdl =CSAlev(iang,ilv,nejcec)*SANgler(iang)*coeff
                     RECcse(irec,0,nnurec) = RECcse(irec,0,nnurec)
     &                  + csmsdl*(1.d0 - weight)
                     RECcse(irec + 1,0,nnurec)
     &                  = RECcse(irec + 1,0,nnurec) + csmsdl*weight
                  ENDDO
               ENDIF
             ELSE
               READ (46,*,END = 1400) popread ! reading zero for a closed channel
             ENDIF
C          
C          Allowing states in the continuum even for MSD>0
C          ELSEIF(MSD.eq.0)then
           ELSE

             IF(ICOllev(i).GE.LEVcc .and. SINlcont.le.0.d0) cycle

C------------Adding inelastic to continuum  (D_Elv(ND_nlv) = elvr)
             echannel = EX(NEX(1),1) - Q(nejcec,1) - D_Elv(i)
             icsl = INT(echannel/DE + 1.0001)
             ncon = min(
     &        NINT((EXCn-Q(nejcec,1)-ECUt(nnurec))/DE + 1.0001),NDEcse)
C            WRITE(8,*) 'nejcec, nnurec',IZAejc(nejcec), IZA(nnurec)
C            WRITE(8,*) 'Level in continuum',D_Elv(i)
C            WRITE(8,*) 'Its bin number',icsl
C            WRITE(8,*) 'E calc',EX(NEX(1),1)-Q(nejcec,1)-(icsl-1)*DE
C            WRITE(8,*) 'Last discr. level',ELV(NLV(nnurec),nnurec)
C            WRITE(8,*) 'Ecut',ECUt(nnurec)
C            WRITE(8,*) 'Ex',EX(NEX(1),1)-Q(nejcec,1)-(ncon-1)*DE
C            WRITE(8,*) 'Continuum starts at bin number',ncon
C------------Avoid reading closed channels
             IF (echannel.GE.0.0001) THEN
               READ (46,*,END = 1400) popread
               popread = popread*FCOred
C--------------This level is not counted as a discrete one
C--------------but it is embedded in the continuum
               CSMsd(nejcec) = CSMsd(nejcec) + popread
               XSInlcont =  XSInlcont + popread
C
C              Special treatment for Giant Multipole Resonances
C              Any level with D_Def(i,2)<0 treated as GR
C
C              Giant multipole resonances following TALYS
C
C              For each L multipolarity Energy Weighted Sum Rule (EWSR) applies:
C              SUM_i(E_i*beta_i)=57.5*A**(-5/3)*L*(L+1)
C
               if(D_Def(i,2).LT.0  .and.
     >             INT(Aejc(0)).eq.1 .and. INT(Zejc(0)).eq.0   ) then
                 write(8,
     >             '(/''  Giant Multipole Resonance with J ='',F4.1)') 
     >           D_Xjlv(i)
                 IF(int(D_Xjlv(i)).eq.3) then
                   write(8,'( ''  GOR cross section (cont) ='',
     >               F7.1,'' mb'')') popread
                   write(8,'( ''  GOR energy ='',F6.2,'' MeV'')')
     >               D_Elv(i)
                   write(8,'( ''  GOR width  ='',F6.2,'' MeV'')') ggor           
                   write(8,'( ''  GOR deformation ='',F7.4)') 
     >               -D_Def(i,2)                         
                   ENDIF           
                 IF(int(D_Xjlv(i)).eq.2) then
                   write(8,'( ''  GQR cross section (cont) ='',
     >               F7.1,'' mb'')') popread
                   write(8,'( ''  GQR energy ='',F6.2,'' MeV'')')      
     >               D_Elv(i)
                   write(8,'( ''  GQR width  ='',F6.2,'' MeV'')') ggqr     
                   write(8,'( ''  GQR deformation ='',F7.4)') 
     >               -D_Def(i,2)                         
                   ENDIF           
                 IF(int(D_Xjlv(i)).eq.0) then
                   write(8,'( ''  GMR cross section (cont) ='',
     >               F7.1,'' mb'')') popread
                   write(8,'( ''  GMR energy ='',F6.2,'' MeV'')') 
     >               D_Elv(i)
                   write(8,'( ''  GMR width  ='',F6.2,'' MeV'')') ggmr                 
                   write(8,'( ''  GMR deformation ='',F7.4)')
     >               -D_Def(i,2)
                   ENDIF    
                 write(8,*)                                        
               endif  
C
C--------------Spreading discrete levels in the continuum using a resolution function
C-
               isigma  = isigma0
               if(D_Def(i,2).LT.0  .and.
     &               INT(Aejc(0)).eq.1 .and. INT(Zejc(0)).eq.0   ) then
C
C                ggmr = 3.d0
C                ggqr =85.d0*A(0)**(-2./3.)
C                ggor =5.d0
C
                 if(int(D_Xjlv(i)).eq.0) isigma  = nint(ggmr/DE+0.5)
                 if(int(D_Xjlv(i)).eq.2) isigma  = nint(ggqr/DE+0.5)
                 if(int(D_Xjlv(i)).eq.3) isigma  = nint(ggor/DE+0.5)
               endif
               isigma2 = 2*isigma*isigma

               if(isigma.gt.0) then
                 dcor  = 0.d0
                 do ie = max(icsl - 3*isigma,1) ,
     &                   min(icsl + 3*isigma,ncon)
                   dcor = dexp(-dble(ie-icsl)**2/isigma2) + dcor
                 enddo
                 if(dcor.gt.0.d0) then
                   do ie = max(icsl - 3*isigma,1) ,
     &                     min(icsl + 3*isigma,ncon)
                       CSEmsd(ie,nejcec) = CSEmsd(ie,nejcec) +
     &                 popread/DE  *  
     &                 dexp(-dble(ie-icsl)**2/isigma2)/dcor
                   enddo
                 else
                   CSEmsd(icsl  ,nejcec) = CSEmsd(icsl,nejcec)
     &              + popread/DE 
                 endif
               else
                 CSEmsd(icsl,nejcec) = CSEmsd(icsl,nejcec)
     &              + popread/DE
               endif
C            
               IF (ICAlangs.EQ.0) THEN
                 READ (45,*,END = 1400)     ! Skipping level identifier line
                 iang = 0
                 DO iang1 = 1, NANgela
                    READ (45,'(3x,12x,D12.5)',END = 1400) ftmp    ! ecis06
                    ftmp = ftmp * FCOred/DE
C-------------------Use only those values that correspond to EMPIRE grid for inelastic XS
                    if(mod(DBLE(iang1-1)*angstep+gang,gang).NE.0) cycle
                    iang = iang + 1
                    if(isigma.gt.0 .and. dcor.gt.0.d0) then
                      do ie = max(icsl - 3*isigma,1) ,
     &                        min(icsl + 3*isigma,ncon)
                        CSEa(ie,iang,nejcec,1) =  CSEa(ie,iang,nejcec,1)
     &                  + ftmp * dexp(-dble(ie-icsl)**2/isigma2)/dcor  
C     &                  * 2 * pi                               ! 2pi taken out, BVC 
                      enddo
                    else
                      CSEa(icsl,iang,nejcec,1) =
     &                CSEa(icsl,iang,nejcec,1) + ftmp ! 2 * pi taken out BVC 
C     &                CSEa(icsl,iang,nejcec,1) + ftmp * 2 * pi ! added 2pi, BVC 
                    endif
                 ENDDO
               ENDIF
             ELSE
               READ (46,*,END = 1400) popread ! reading zero for a closed channel
             ENDIF
C------------End of adding inelastic to continuum
           ENDIF
 1350    ENDDO
      ENDIF
 1400 CLOSE (45)
      IF (DIRect.NE.0) CLOSE (46)

      IF (KTRlom(0,0).GT.0 .AND. FIRst_ein .AND. DIRect.NE.0 
     &  .AND. SINl.GT.0.0001d0) WRITE (8,*)
     &  ' DWBA contribution added to discrete levels (XS='
     &  , sngl(SINl), ' mb)'
      IF (KTRlom(0,0).GT.0 .AND. FIRst_ein .AND. DIRect.NE.0 
     &  .AND. SINlcont.GT.0.0001d0) WRITE (8,*)
     &  ' DWBA contribution added to the continuum   (XS='
     &  , sngl(SINlcont), ' mb)'

      IF(INT(ZEJc(0)).EQ.0 .AND. TOTcs.GT.0.d0) totcorr = 
     & (ELAcs*ELAred +CSFus+ (SINl + SINlcc)*FCCred + SINlcont*FCOred) /
     &                 (TOTcs*TOTred)

      IF (KTRlom(0,0).GT.0) THEN
       IF (INT(ZEJc(0)).EQ.0 .AND. AEJc(0).GT.0) THEN
         WRITE (8,99785) TOTcs,TOTred*totcorr,TOTred*TOTcs*totcorr,
     &     ((SINlcc + SINl)*FCCred + SINlcont*FCOred) + CSFus,
     &                   CSFus/FUSred,FUSRED,CSFus,
     &                   ELAcs, ELAred, ELAred*ELAcs 
         IF(((SINlcc + SINl)*FCCred + SINlcont*FCOred).GT.0) 
     &     WRITE (8,99006) SINlcc + SINl + SINlcont,
     &                  ((SINlcc + SINl)*FCCred + SINlcont*FCOred)/
     &                   (SINlcc + SINl + SINlcont),
     &                   (SINlcc + SINl)*FCCred + SINlcont*FCOred
         WRITE(8,'(/)')
       ENDIF
       IF (ZEJc(0).NE.0 .OR. AEJc(0).EQ.0) THEN
         IF(DBRkup.GT.1.0d-2) THEN
           WRITE (8,99011)
     &     ((SINlcc + SINl)*FCCred + SINlcont*FCOred) +
     &      CSDbrkup(6) + CSFus,CSDbrkup(6),
     &     CSFus/FUSred,FUSRED,CSFus
          ELSE
           WRITE (8,99010)
     &     ((SINlcc + SINl)*FCCred + SINlcont*FCOred) + CSFus,
     &     CSFus/FUSred,FUSRED,CSFus
          ENDIF

         IF(((SINlcc + SINl)*FCCred + SINlcont*FCOred).GT.0) 
     &     WRITE (8,99006) SINlcc + SINl + SINlcont,
     &                  ((SINlcc + SINl)*FCCred + SINlcont*FCOred)/
     &                   (SINlcc + SINl + SINlcont),
     &                   (SINlcc + SINl)*FCCred + SINlcont*FCOred
         WRITE(8,'(/)')
       ENDIF

       IF (AEJc(0).GT.0) THEN
         WRITE (8,99015)
         WRITE (8,99020)
         DO iang = 1, NANgela/4 + 1
           imint = 4*(iang - 1) + 1
           imaxt = MIN0(4*iang,NANgela)
           WRITE (8,99025) 
     &      ((j - 1)*angstep,ELAred*elada(j),j = imint,imaxt)
         ENDDO
       ENDIF

       WRITE (8,'(//)')
       IF (ncollx.GT.0) THEN
C--------Locate position of the projectile among ejectiles
         CALL WHEREJC(IZAejc(0),nejcec,iloc)
C
         WRITE (8,*) ' '
         gang = 180.d0/(NDANG - 1)
         its = ncollx
         IF (CSAlev(1,ICOller(2),nejcec).GT.0) THEN
           WRITE(8,99028)
           WRITE(8,99030) (ICOller(ilv),ilv = 2,MIN(its,10))
           WRITE(8,99031) (ELV(ICOller(ilv),nnurec),ilv = 2,MIN(its,10))
           WRITE(8,99033) (XJLv(ICOller(ilv),nnurec)*
     &        LVP(ICOller(ilv),nnurec),D_DEF(ilv,2),ilv = 2,MIN(its,10))
           WRITE(8,*) ' '
           DO iang = 1, NDANG
             WRITE (8,99035) (iang - 1)*gang,
     &            (CSAlev(iang,ICOller(ilv),nejcec),ilv = 2,MIN(its,10))
           ENDDO
           WRITE(8,*) ' '
           WRITE(8,99040)1,(POPlv(ICOller(ilv),nnurec),
     &                            ilv= 2,MIN(its,10))
C
           IF(its.gt.10) THEN
             WRITE(8,*) ' '
             WRITE(8,*) ' '
             WRITE(8,99030)(ICOller(ilv),ilv = 11,MIN(its,20))
             WRITE(8,99032)(ELV(ICOller(ilv),nnurec),ilv=11,MIN(its,20))
             WRITE(8,99034)(XJLv(ICOller(ilv),nnurec)*
     &         LVP(ICOller(ilv),nnurec),D_DEF(ilv,2),ilv=11,MIN(its,20))
             WRITE (8,*) ' '
             DO iang = 1, NDANG
               WRITE (8,99035) (iang - 1)*gang,
     &           (CSAlev(iang,ICOller(ilv),nejcec),ilv = 11,MIN(its,20))
             ENDDO
             WRITE (8,*) ' '
             WRITE (8,99040) 2,(POPlv(ICOller(ilv),nnurec),ilv = 11,
     &                        MIN(its,20))
           ENDIF

           IF(its.gt.20) THEN
             WRITE(8,*) ' '
             WRITE(8,*) ' '
             WRITE(8,99030) (ICOller(ilv),ilv = 21,MIN(its,30))
             WRITE(8,99032)(ELV(ICOller(ilv),nnurec),ilv=21,MIN(its,30))
             WRITE(8,99034)(XJLv(ICOller(ilv),nnurec)*
     &         LVP(ICOller(ilv),nnurec),D_DEF(ilv,2),ilv=21,MIN(its,30))
             WRITE (8,*) ' '
             DO iang = 1, NDANG
               WRITE (8,99035) (iang - 1)*gang,
     &           (CSAlev(iang,ICOller(ilv),nejcec),ilv = 21,MIN(its,30))
             ENDDO
             WRITE (8,*) ' '
             WRITE (8,99040) 3,(POPlv(ICOller(ilv),nnurec),ilv = 21,
     &                          MIN(its,30))
           ENDIF
C
C----------Because of the ENDF format restrictions the maximum
C----------number of discrete levels is limited to 40
C
           IF(its.gt.30) THEN
             WRITE(8,*) ' '
             WRITE(8,*) ' '
             WRITE(8,99030)(ICOller(ilv),ilv = 31,MIN(its,40))
             WRITE(8,99032)(ELV(ICOller(ilv),nnurec),ilv=31,MIN(its,40))
             WRITE(8,99034)(XJLv(ICOller(ilv),nnurec)*
     &         LVP(ICOller(ilv),nnurec),D_DEF(ilv,2),ilv=31,MIN(its,40))
             WRITE (8,*) ' '
             DO iang = 1, NDANG
               WRITE (8,99035) (iang - 1)*gang,
     &           (CSAlev(iang,ICOller(ilv),nejcec),ilv = 31,MIN(its,40))
             ENDDO
             WRITE (8,*) ' '
             WRITE (8,99040) 4,(POPlv(ICOller(ilv),nnurec),ilv = 31,
     &                      MIN(its,40))
           ENDIF
           WRITE (8,*) '++++++'
         ENDIF
       ENDIF

      ENDIF
      
      RETURN
99006 FORMAT (/,2x,
     &           'Direct cross section        :',e14.7,' mb',
     &           '  ( Scaled by ',f6.3,' to ',e14.7,' mb )')
99010 FORMAT (/,2x,'Absorption cross section    :',e14.7,' mb',    
     &        /,2x,'CN formation cross section  :',e14.7,' mb',
     &           '  ( Scaled by ',f6.3,' to ',e14.7,' mb )')
99011 FORMAT (/,2x,'Absorption cross section    :',e14.7,' mb',
     &        /,2x,'d breakup cross section     :',e14.7,' mb',
     &        /,2x,'CN formation cross section  :',e14.7,' mb',
     &           '  ( Scaled by ',f6.3,' to ',e14.7,' mb )')
99015 FORMAT (/' ',46x,'SHAPE ELASTIC DIFFERENTIAL CROSS-SECTION',/,' ',
     &        46x,40('*'),/,' ',50x,'CENTER-OF-MASS SYSTEM',/)
99016 FORMAT (/' ',46x,'COMP. ELASTIC DIFFERENTIAL CROSS-SECTION',/,' ',
     &        46x,40('*'),/,' ',50x,'CENTER-OF-MASS SYSTEM',/)
99020 FORMAT (' ',5x,4('    TETA ',2x,'D.SIGMA/D.OMEGA',6x),/)
99025 FORMAT (' ',5x,4(1p,e12.5,2x,e12.5,6x))
99028 FORMAT ( ' ',46x,'DIRECT INEL. DIFFERENTIAL CROSS-SECTION',/,
     &              ' ',46x,' (only discrete levels are listed)',/,' '
     &                 ,46x,36('*'),/,' ',50x,'CENTER-OF-MASS SYSTEM',
     &      /)
99029 FORMAT (/' ',46x,'INELASTIC DIFFERENTIAL CROSS-SECTION',/,
     &              ' ',46x,'    (including compound + direct)',/,' '
     &              ' ',46x,' (only discrete levels are listed)',/,' '
     &                 ,46x,36('*'),/,' ',50x,'CENTER-OF-MASS SYSTEM',
     &      /)
99030 FORMAT ('  Angle ',10(6x,i2,'-level'))
99031 FORMAT ('        ',9(5x,'E=',f7.4))
99032 FORMAT ('        ',10(5x,'E=',f7.4))
99033 FORMAT ('        ',9(4x,f4.1,'/',f5.4))
99034 FORMAT ('        ',10(4x,f4.1,'/',f5.4))
99035 FORMAT (1x,f5.1,3x,11(2x,E12.6))
99040 FORMAT (' DIR.INEL',I1,1x,11(E12.6,2x))
99041 FORMAT (' TOT.INEL',I1,1x,11(E12.6,2x))
99785    FORMAT (/,2x,'Total cross section         :',e14.7,' mb',
     &                '  ( Scaled by ',f6.3,' to ',e14.7,' mb )',
     &           /,2x,'Absorption cross section    :',e14.7,' mb',    
     &           /,2x,'CN formation cross section  :',e14.7,' mb',
     &                '  ( Scaled by ',f6.3,' to ',e14.7,' mb',
     &           /,2x,'Shape elastic cross section :',e14.7,' mb',
     &                '  ( Scaled by ',f6.3,' to ',e14.7,' mb )')
      END
