Ccc   * $Rev: 3705 $
Ccc   * $Author: rcapote $
Ccc   * $Date: 2014-01-04 22:01:02 +0100 (Sat, 04 Jan 2014) $

      SUBROUTINE HF_decay(ncollx,nnuc,nnurec,nejcec,iret,totcorr)

      USE empcess, ONLY: disc_int, CSHms
      use hrtw

      implicit none
      INCLUDE "dimension.h"
      INCLUDE "global.h"

      INCLUDE "main_common.h"
C
      INTEGER ncollx,nnuc,nnurec,nejcec,iret
      DOUBLE PRECISION totcorr
C
C     local variables
C
      CHARACTER*9 cejectile

      INTEGER i,ia,nejc,il,nbr,ib,iang,its,ilv,j,m,ke,iz,nxsp,npsp
      INTEGER kemin,kemax,jcn,ipar,ip,izares,iloc,nnur,nspec,ispec
      INTEGER ilevcol,jz,jn,imint,imaxt

      DOUBLE PRECISION dtmp,delang,cspg,step,xnl,spdif,spdiff,checkprd
      DOUBLE PRECISION ares,zres,ded,sum !,csemist
      DOUBLE PRECISION poplev,poptot,popleft,pope,xnor,fisxse,ftmp
      DOUBLE PRECISION sumfis,sumfism(NFMOD),gang,angstep,xs_norm
                       
      DOUBLE PRECISION gtotsp,xtotsp,ptotsp,atotsp,dtotsp,ttotsp,htotsp
      DOUBLE PRECISION emedg,emedn,emedp,emeda,emedd,emedt,emedh
      DOUBLE PRECISION ctotsp,emedc,totsp,ftmp_gs,esum
      DOUBLE PRECISION cmulg,cmuln,cmulp,cmula,cmuld,cmult,cmulh

      DOUBLE PRECISION, external :: GET_DDXS

      iret = 0

      sumfism = 0.d0
      sumfis  = 0.d0

      ia = INT(A(nnuc))

      IF (nnuc.eq.1) THEN ! CN
        WRITE (8,*)
        WRITE (8,*)
     &' ---------------------------------------------------------------'
        IF(abs(QPRod(nnuc) + ELV(LEVtarg,0)).gt.99.99) THEN
          WRITE (8,
     &'(''  Decaying nucleus '',I3,''-'',A2,''-'',I3,     ''  mass='',F1
     &0.6,'' Q-value='',F10.5)') INT(Z(nnuc)), SYMb(nnuc), ia,
     &         AMAss(nnuc), QPRod(nnuc) + ELV(LEVtarg,0)
        ELSE
          WRITE (8,
     &'(''  Decaying nucleus '',I3,''-'',A2,''-'',I3,     ''  mass='',F1
     &0.6,'' Q-value='',F10.6)') INT(Z(nnuc)), SYMb(nnuc), ia,
     &         AMAss(nnuc), QPRod(nnuc) + ELV(LEVtarg,0)
        ENDIF
        WRITE (8,*)
     &' ---------------------------------------------------------------'
        WRITE (8,*)

        IF (IOUt.GT.1) THEN
          WRITE (8,*) 
          WRITE (8,'(''  Compound nucleus '',I3,''-'',A2,
     &          '' spin distribution'')') NINT(A(1)), SYMb(1)
          WRITE (8,*) ' -----------------------------------------'
          WRITE (8,*) ' '
          DO i = 1, NLW
            IF (MOD(ia,2).EQ.0) THEN
              WRITE (8,'(1X,I5,G12.5,5X,I5,G12.5)') i - 1,
     &           POP(NEX(1),i,1,1), ( - (i - 1)), POP(NEX(1),i,2,1)
            ELSE
              WRITE (8,'(1X,I4,''/2'',G12.5,5X,I4,''/2'',G12.5)')
     &           2*i - 1, POP(NEX(1),i,1,1), ( - (2*i - 1)),
     &           POP(NEX(1),i,2,1)
            ENDIF
          ENDDO
          WRITE (8,*) ' '
        ENDIF

        WRITE (12,*)
     &' ---------------------------------------------------------------'
        IF(abs(QPRod(nnuc) + ELV(LEVtarg,0)).gt.99.99) THEN
          WRITE (12,
     &'(''  Decaying nucleus '',I3,''-'',A2,''-'',I3,     ''  mass='',F1
     &0.6,'' Q-value='',F10.5)') INT(Z(nnuc)), SYMb(nnuc), ia,
     &         AMAss(nnuc), QPRod(nnuc) + ELV(LEVtarg,0)
        ELSE
          WRITE (12,
     &'(''  Decaying nucleus '',I3,''-'',A2,''-'',I3,     ''  mass='',F1
     &0.6,'' Q-value='',F10.6)') INT(Z(nnuc)), SYMb(nnuc), ia,
     &         AMAss(nnuc), QPRod(nnuc) + ELV(LEVtarg,0)
        ENDIF
        WRITE (12,*)
     &' ---------------------------------------------------------------'
        WRITE (12,
     &'(1X,/,10X,''Discrete level population before gamma cascade'')')
            WRITE (12,'(1X,/,10X,40(1H-),/)')
      ENDIF

      IF (FITomp.LE.0) THEN
        IF (nnuc.NE.1) THEN
          IF (nnuc.EQ.mt91) THEN
            nejc = 1
          ELSEIF (nnuc.EQ.mt649) THEN
            nejc = 2
          ELSEIF (nnuc.EQ.mt849) THEN
            nejc = 3
          ELSE
            GOTO 1460
          ENDIF
          dtmp = 0.d0
          DO il = 1, NLV(nnuc)
            dtmp = dtmp + CSDirlev(il,nejc)
            disc_int(il,nejc) = CSDirlev(il,nejc)                  
          ENDDO
          IF(dtmp.LE.0.0 .AND. POPlv(1,nnuc).LE.0.d0) GOTO 1460
          WRITE (12,*)
          WRITE (12,*)
     &' ---------------------------------------------------------------'
          WRITE (8,*)
          WRITE (8,*)
     &' ---------------------------------------------------------------'
          IF(abs(QPRod(nnuc) + ELV(LEVtarg,0)).gt.99.99) THEN
            WRITE (8,
     &'(''  Decaying nucleus '',I3,''-'',A2,''-'',I3,     ''  mass='',F1
     &0.6,'' Q-value='',F10.5)') INT(Z(nnuc)), SYMb(nnuc), ia,
     &        AMAss(nnuc), QPRod(nnuc) + ELV(LEVtarg,0)
            WRITE (12,
     &'(''  Decaying nucleus '',I3,''-'',A2,''-'',I3,     ''  mass='',F1
     &0.6,'' Q-value='',F10.5)') INT(Z(nnuc)), SYMb(nnuc), ia,
     &        AMAss(nnuc), QPRod(nnuc) + ELV(LEVtarg,0)
          ELSE
            WRITE (8,
     &'(''  Decaying nucleus '',I3,''-'',A2,''-'',I3,     ''  mass='',F1
     &0.6,'' Q-value='',F10.6)') INT(Z(nnuc)), SYMb(nnuc), ia,
     &        AMAss(nnuc), QPRod(nnuc) + ELV(LEVtarg,0)
            WRITE (12,
     &'(''  Decaying nucleus '',I3,''-'',A2,''-'',I3,     ''  mass='',F1
     &0.6,'' Q-value='',F10.6)') INT(Z(nnuc)), SYMb(nnuc), ia,
     &         AMAss(nnuc), QPRod(nnuc) + ELV(LEVtarg,0)
          ENDIF
          WRITE (8,*)
     &' ---------------------------------------------------------------'
          WRITE (12,*)
     &' ---------------------------------------------------------------'
          WRITE (12,
     &'(1X,/,10X,''Discrete level population '',      ''before gamma cas
     &cade'')')
          WRITE (12,'(1X,/,10X,40(1H-),/)')
C
          DO il = 1, NLV(nnuc)
C-----------Check for the number of branching ratios
            nbr = 0
            DO ib = 1, NDBR
              IF (BR(il,ib,2,nnuc).EQ.0.) EXIT
              nbr = ib
            ENDDO
            IF (nbr.EQ.0 .AND. il.NE.1 .AND. FIRst_ein .AND.
     &        (nnuc.EQ.mt91 .OR. nnuc.EQ.mt649 .OR.
     &         nnuc.EQ.mt849) .AND. ENDf(nnuc).NE.0)
     &        WRITE (8,*) ' WARNING: Branching ratios for level ', il,
     &                     ' IN ', INT(A(nnuc)), '-', SYMb(nnuc),
     &                     ' are missing'
            WRITE (12,99070) il, ELV(il,nnuc), LVP(il,nnuc),
     &        XJLv(il,nnuc), CSDirlev(il,nejc), nbr,
     &        (NINT(BR(il,ib,1,nnuc)),BR(il,ib,2,nnuc),ib = 1,nbr)
C-----------Next IF moves levels population to the ground state
C-----------to avoid gamma cascade between discrete levels
C-----------originating from the direct population
C-----------of discrete levels by a neutron, proton or alpha.
C-----------These gammas should not go into MT=91, 649, or 849.
            IF ((nnuc.EQ.mt91 .OR. nnuc.EQ.mt649 .OR. nnuc.EQ.
     &                mt849) .AND. il.NE.1 .AND. ENDf(nnuc).NE.0 ) THEN
              POPlv(1,nnuc) = POPlv(1,nnuc) + CSDirlev(il,nejc)
              POPlv(il,nnuc) = POPlv(il,nnuc) - CSDirlev(il,nejc)
            ENDIF
          ENDDO ! end of the loop over discrete levels

          WRITE (12,'(1X,/,10X,40(1H-),/)')
C
C---------Decay direct population of discrete levels by a neutron,
C---------proton or alpha without storing emitted gammas in the spectra.
          IF ((nnuc.EQ.mt91   .OR.  nnuc.EQ.mt649 .OR. 
     &      nnuc.EQ.mt849) .AND. ENDf(nnuc).NE.0 .AND. il.NE.1) 
     &      CALL DECAYD_DIR(nnuc, nejc)
C
C---------Write elastic to tape 12 and to tape 68
 1460     IF (nnuc.EQ.mt2) THEN
            WRITE (12,*) ' '
            WRITE (12,
     &             '('' ELASTIC CROSS SECTION= '',1P,E12.5,'' mb'')')
     &              ELAcs*ELAred + 4.d0*PI*ELCncs
            WRITE (12,*) ' '
            WRITE (12,
     &             '('' SHAPE ELASTIC CROSS SECTION= '',1P,E12.5,
     &              '' mb'')') ELAcs*ELAred 
            WRITE (12,*) ' '
            WRITE (12,
     &             '('' COMP. ELASTIC CROSS SECTION= '',1P,E12.5,
     &               '' mb'')') 4.d0*PI*ELCncs
            WRITE (12,*) ' '
            WRITE (12,*) ' Elastic angular distribution '
            WRITE (12,*) ' '
            IF (ICAlangs.GT.0) THEN
              WRITE (12,'(10X,8G15.5)') (ANGles(iang),iang = 1, NANgela)
            ELSE
              delang = 180.d0/FLOAT(NANgela - 1)
               WRITE (12,'(10X,8G15.5)') 
     &                (FLOAT(iang - 1)*delang,iang = 1,NANgela)
            ENDIF

            if(.not.CN_isotropic .and. ELCncs.LT.0.05d0) then    
              CN_isotropic = .TRUE.
              WRITE(8,*)
              WRITE(8,*) 
     &        'CN angular distribution assumed isotropic at Einc = ',
     &        sngl(EINl)
              WRITE(12,*)      
     &        'CN angular distribution assumed isotropic at Einc = ',
     &        sngl(EINl)
              WRITE(8,*)
            endif  

            IF (ELCncs.EQ.0) then

              WRITE (8,*) ' WARNING: CN elastic is 0'

            ELSE

             IF(CN_isotropic .or. PL_CN(0,LEVtarg).le.0.d0) then   
C
C              WRITE (8,*)
C    &          '    Elastic=', sngl(ELCncs), ' mb/str'

               DO iang = 1, NDANG
                 cel_da(iang) = ELCncs ! isotropic
               ENDDO
   
               WRITE (12,'(9X,8E15.5)') 
     &           ((ELAred*elada(iang)+cel_da(iang)),iang=1,NANgela)

               WRITE (12,*) ' '
               WRITE (12,*) ' '
               WRITE (12,*) ' Legendre coefficients expansion '
               WRITE (12,*) ' '
               WRITE (12,'(1x,A7,I5)') ' Lmax =',min(NDAng,neles)
               WRITE (12,*) ' '
               WRITE (12,'(9X,8D15.8)')
     &          (ELAred*elleg(1) + ELCncs),
     &          (ELAred*elleg(iang),iang = 2,min(NDAng,neles))
               WRITE (12,*) ' '

             ELSE

               xs_norm = ELCncs/PL_CN(0,LEVtarg)
C	         write(*,*) 'NORM=',xs_norm
               DO iang = 1, NDANG
                 cel_da(iang) = xs_norm*GET_DDXS(CANGLE(iang),LEVtarg)
               ENDDO

               WRITE (12,'(9X,8E15.5)') 
     &            ((ELAred*elada(iang)+cel_da(iang)),iang=1,NANgela)
               WRITE (12,*)' '

               WRITE (12,*)' '
               WRITE (12,*)' Legendre coefficients expansion'
               WRITE (12,*)' '
               WRITE (12,'(1x,A7,I5)') ' Lmax =',min(NDAng,neles)
               WRITE (12,*)' '
               WRITE (12,'(9X,8D15.8)') 
     &            ELAred*elleg(1)+xs_norm*PL_CN(0,LEVtarg),
     &           (ELAred*elleg(iang) + xs_norm*PL_CN(iang-1,LEVtarg),
     &                                 iang = 2,min(NDAng,neles))
	    
               WRITE (12,*)' '
               WRITE (12,*)' DIR Legendre coefficients expansion'
               WRITE (12,*)' '
               WRITE (12,'(1x,A7,I5)') ' Lmax =',min(NDAng,neles)
               WRITE (12,*)' '
               WRITE (12,'(9X,8D15.8)') ELAred*elleg(1),
     &            (ELAred*elleg(iang),iang = 2,min(NDAng,neles))

               WRITE (12,*)' '
               WRITE (12,*)' CE Legendre coefficients expansion'
               WRITE (12,*)' '
               WRITE (12,'(1x,A7,I5)') 
     &            ' Lmax =',min(NDAng,PL_lmax(LEVtarg))
               WRITE (12,*) ' '
               WRITE (12,'(9X,8D15.8)') xs_norm*PL_CN(0,LEVtarg), 
     &            (xs_norm*PL_CN(iang-1,1),iang = 2,
     &                       min(NDAng,PL_lmax(LEVtarg)))

             ENDIF

            ENDIF

            IF (FITomp.LT.0) THEN
              WRITE(40,'(F12.4,3D12.5)') 
     &          EINl,TOTcs*TOTred*totcorr,ABScs*FUSred
              IF (ncollx.GT.0) THEN
C---------------locate position of the projectile among ejectiles
                CALL WHEREJC(IZAejc(0),nejcec,iloc)
                its = ncollx
                WRITE (40,'(12x,11D12.5)') 
     &            ELAred*ELAcs + 4.d0*PI*ELCncs,
     &            (CSDirlev(ICOller(ilv),nejcec),ilv = 2,MIN(its,10))
                IF (ICAlangs.gt.0) THEN
                  DO iang = 1, NDANG
                    WRITE (40,'(f12.4,11D12.5)') ANGles(iang),
     &                ELAred*elada(iang) + cel_da(iang),
     &                 (CSAlev(iang,ICOller(ilv),nejcec),
     &                  ilv = 2,MIN(its,10))
                  ENDDO
                ENDIF
              ELSE
                WRITE (40,'(12x,11D12.5)') 
     &                           ELAred*ELAcs + 4.d0*PI*ELCncs
                IF (ICAlangs.gt.0) THEN
                  DO iang = 1, NDANG
                    WRITE (40,'(f12.4,11D12.5)') ANGles(iang),
     &                     ELAred*elada(iang) + cel_da(iang)
                  ENDDO
                ENDIF
              ENDIF
            ENDIF ! FITomp < 0 
          ENDIF ! nnuc.EQ.mt2
        ENDIF ! nnuc.NE.1
      ENDIF ! FITomp <= 0
C-----Jump to end of loop after elastic when fitting
      If(FITomp.LT.0 .AND. nnuc.EQ.mt2) THEN
       iret = 1
      RETURN 
      ENDIF

      kemin = 1
      kemax = NEX(nnuc)
      checkprd = 0.d0

      POPmax(nnuc) = POPmax(nnuc)*0.0001d0 ! why is decreased ?
      IF (POPmax(nnuc).EQ.0.d0) goto 1500 
C
C        Continuum of nnuc nucleus has not been populated, skipping
C
      IF (nnuc.GT.1) THEN
        WRITE (8,*) ' '
        WRITE (8,*) ' -------------------------------------'
        WRITE (8,'(I3,2X,''Decaying nucleus '',I3,''-'',A2)') nnuc,
     &             ia, SYMb(nnuc)
        WRITE (8,*) ' -------------------------------------'
        WRITE (8,*) ' '
      endif

C-----Calculate compound nucleus level density at saddle point
      IF (NINT(FISshi(nnuc)).EQ.1) THEN
        IF (FISsil(nnuc)) THEN
          CALL ROEMP(nnuc,1.D0,0.0D0)
          IF(FIRst_ein) WRITE (8,*)
     &         ' WARNING: For HI reactions (FISSHI =1), LD model at sadd
     &les is EGSM'
          IF (IOUt.EQ.6 .and. FIRst_ein) THEN
            WRITE (8,'(1X,/,'' Saddle point level density'',/)')
            WRITE (8,'(1X,13G10.4)') (EX(i,nnuc),(ROF(i,j,nnuc)
     &                         ,j = 1,12), i = 1,NEX(nnuc))
          ENDIF
        ENDIF
      ENDIF
C-----
C-----Start Hauser-Feshbach nnuc nucleus decay
C-----
      popleft = 0.d0
C-----Turn on (KEMIN=1) or off (KEMIN=NEX(NNUC)) gamma cascade
C-----in the first CN, it is preferred to use input parameter GCASC (0=OFF,1=ON) 
      kemin = 1
      IF (nnuc.EQ.1 .and. GCAsc.EQ.0.0D0) kemin = NEX(nnuc)
C-----Turn  off (KEMIN=NEX(NNUC)) gamma cascade in the case of OMP fit
      IF (FITomp.NE.0) kemin = NEX(nnuc)
      kemax = NEX(nnuc)

C-----Account for widths fluctuations (HRTW)
      IF (LHRtw.EQ.1 .AND. EINl.GT.EHRtw) LHRtw = 0

      IF (nnuc.EQ.1 .AND. EINl.LT.EMAx_tlj) THEN
C
C       only for CN decay 
C
        CALL calc_HRTW   ! width fluctuation

        IF (ENDf(1).GT.0 .AND. RECoil.GT.0)
     &    CALL GET_RECOIL(kemax,nnuc) !recoil spectrum
          kemax = max(NEX(nnuc) - 1,1)
          IF (FISsil(nnuc) .and. NINT(FISshi(nnuc)).NE.1 ) THEN
            IF (NINT(FISmod(nnuc)).EQ.0) THEN
              WRITE (80,*) 'csfis=', CSFis,' mb'
            ELSE
              WRITE (80,*) '  '
              DO m = 1, INT(FISmod(nnuc)) + 1
                WRITE (80,*) '    Mode=', m,'  csfis=', CSFism(m),' mb'
              ENDDO
            ENDIF
          ENDIF
        ENDIF
C
        cspg = 0.d0 
C-------DO loop over c.n. excitation energy
        DO ke = kemax, kemin, -1
          IF(ke.le.0) cycle
          step = DE
          IF (ke.EQ.NEX(nnuc) .OR. ke.EQ.1) step = 0.5*DE
          IF (ke.EQ.NEX(nnuc) .AND. nnuc.EQ.1) step = 1.0

          IF (ENDf(1).GT.0) THEN
C-----------Clean auxiliary particle spectra for calculation of recoils
            REClev = 0.d0           
            AUSpec = 0.d0
C-----------Calculate population in the energy bin ke
            pope = 0.d0
            DO jcn = 1, NLW
              pope = pope + POP(ke,jcn,1,nnuc) + POP(ke,jcn,2,nnuc)
            ENDDO
            POPbin(ke,nnuc) = pope*step
          ENDIF

C
C		posible parallelization over parity and spin of Nnuc
C
          fisxse = 0.d0
          DO ipar = 1, 2 !over decaying nucleus parity
            ip = INT(( - 1.0)**(ipar + 1))
            DO jcn = 1, NLW !over decaying nucleus spin
!             if (nnuc.eq.1 .and. ke.eq.nex(1)) then
!               write(8,*) ' '
!               write(8,*) 'DECAY STATE ke=',ke,' J=',jcn,' Pi=',ipar
!             endif
              IF (GDRdyn.EQ.1.0D0) CALL ULMDYN(nnuc,jcn,EX(ke,nnuc))
              DENhf = 0.d0
!             sumtll = 0.d0
              IF (POP(ke,jcn,ipar,nnuc).LT.POPmax(nnuc)) THEN
                popleft = popleft + POP(ke,jcn,ipar,nnuc)*DE
                CYCLE
              ENDIF
              DO nejc = 1, NEJcm !over ejectiles
                ares = A(nnuc) - AEJc(nejc)
                zres = Z(nnuc) - ZEJc(nejc)
C---------------Residual nuclei must be heavier than alpha
                if(ares.le.4. and. zres.le.2.) cycle
                izares = INT(1000.0*zres + ares)
                CALL WHERE(izares,nnur,iloc)
                if(iloc.eq.1) CYCLE
                CALL DECAY(nnuc,ke,jcn,ip,nnur,nejc,sum)
!               if (nnuc.eq.1 .and. ke.eq.nex(1) .and. nejc.eq.3) then
!                 write(*,*) 'J=',jcn
!                 write(8,*) 'sum for ejectile=', nejc, sum
!                 write(*,*) 'sum for ejectile=', nejc, sum
!                 sumtll = sumtll + sum
!               endif
              ENDDO
C-------------DO loop over ejectiles       ***done***
!             if (nnuc.eq.1 .and. ke.eq.nex(1)) then
!               write(8,*) 'sum for particles at J=',jcn, sumtll
!               write(*,*) 'sum for particles at J=',jcn, sumtll
!             endif
C
C-------------gamma emision
              CALL DECAYG(nnuc,ke,jcn,ip,sum)
C             if (nnuc.eq.1 .and. ke.eq.nex(1)) then
C               write(*,*) 'sum for gammas at J=',jcn, sum
!               write(8,*) 'sum for gammas at J=',jcn, sum
C             endif
C-------------Distribute yrast population over discrete levels
              IF (DENhf.EQ.0.0D0) THEN
                IF (ke.EQ.1) THEN
                  ded = DE*0.5
                ELSE
                  ded = DE
                ENDIF
                IF (IOUt.GT.1) WRITE (8,
     & '('' Yrast state at bin'',I4,'' spin='',F5.1,'' pop='',   G12.5)'
     & ) ke, FLOAT(ip)*(FLOAT(jcn) + HIS(nnur)), POP(ke,jcn,ipar,nnuc)
     &   *ded
C
C               Corrected on Jan 2011, Previously missed gamma XSs
C
                CSEmis(0,Nnuc) = CSEmis(0,Nnuc) + 
     &                         POP(ke,jcn,ipar,nnuc)*ded
C---------------Look for the discrete level with the closest spin
                xnl = 1.d0
                spdiff = 100.d0
                DO il = 1, NLV(nnuc)
                  spdif = ABS(FLOAT(jcn) + HIS(nnur) - XJLv(il,nnuc))
                  IF (spdif.LT.spdiff) THEN
                    spdiff = spdif
                    xnl = 1.d0
                  ELSE
                    IF (spdif.EQ.spdiff) xnl = xnl + 1.
                  ENDIF
                ENDDO
                DO il = 1, NLV(nnuc)
                  spdif = ABS(FLOAT(jcn) + HIS(nnur) - XJLv(il,nnuc))
                  IF (spdif.EQ.spdiff) THEN
                    SCRtl(il,0) = 1.0D0/xnl
                    DENhf = DENhf + SCRtl(il,0)
                    IF (IOUt.GT.1) WRITE (8,
     &'(10X,I3,''% of this was assumed to populate level #'',
     &I3)') INT(100./xnl), il
                  ENDIF
                ENDDO
              ENDIF
C-------------
C-------------Fission ()
              IF (FISsil(nnuc) .AND. NINT(FISshi(nnuc)).EQ.1)
     &                CALL FISSION(nnuc,ke,jcn,sumfis)
              IF (FISsil(nnuc) .AND. NINT(FISshi(nnuc)).NE.1)
     &                CALL FISCROSS(nnuc,ke,ip,jcn,sumfis,sumfism)
!             if (nnuc.eq.1 .and. ke.eq.nex(1)) then
!               write(8,*) 'sum for fission at J= ',jcn, sumfis
!               write(*,*) 'sum for fission at J= ',jcn, sumfis
!             endif 
C-------------
C-------------Normalization and accumulation
C-------------
              xnor = POP(ke,jcn,ipar,nnuc)*step/DENhf
!             if (nnuc.eq.1 .and. ke.eq.nex(1)) then
!               write(8,*) ' '
!               write(8,*) 'SUMMARY HF Jc =', Jc
!               write(8,*)'total sum from this state ' , DENhf
!               write(8,*)'sum gamma ' , sumg
!               write(8,*)'sum fission ' , sumfis
!               write(8,*)'first entry DENhf=',denhf
!               write(8,*) 'HF xnor, DENhf', xnor, DENhf
!             endif
C
              CALL XSECT(nnuc,m,xnor,sumfis,
     &                   sumfism,ke,ipar,jcn,fisxse)
C
C-------------Calculate total emission
C
C             DO nejc = 0, NEJcm
C               csemist = csemist + CSEmis(nejc,nnuc)
C             ENDDO
C             csemist = csemist + CSFis
C-----------------

            ENDDO                 !loop over decaying nucleus spin
          ENDDO                   !loop over decaying nucleus parity
C
          IF (nnuc.GT.1 .AND. ENDf(nnuc).GT.0  .AND. RECoil.GT.0)
     &         CALL GET_RECOIL(ke,nnuc) !recoil spectrum for ke bin
          IF (FISsil(nnuc) .and. NINT(FISshi(nnuc)).NE.1
     &         .and. fisxse.gt.0) THEN
            IF (NINT(FISmod(nnuc)).EQ.0) THEN
              WRITE (80,*) 'csfis=', CSFis,
     &                   ' mb', '   fisxse=', fisxse, ' mb'
            ELSE
              WRITE (80,*) '  '
              DO m = 1, INT(FISmod(nnuc)) + 1
                WRITE (80,*) '    Mode=', m, '  csfis=', CSFism(m),' mb'
              ENDDO
              WRITE (80,*) 'csfis=', CSFis,
     &            ' mb', '   fisxse=', fisxse, ' mb'
            ENDIF
          ENDIF
        ENDDO                  !loop over c.n. excitation energy
C-------
C-------Hauser-Feshbach decay of nnuc  ***done***
C-------
C-------Printout of results for the decay of NNUC nucleus
        IF (IOUt.GT.0) WRITE (8,
     &          '(1X,/,'' Population neglected because too'',
     &                 '' small '',G12.5,/)') popleft*DE

1500    DO il = 1, NLV(nnuc)
          CSPrd(nnuc) = CSPrd(nnuc) + POPlv(il,nnuc)
        ENDDO

        IF(CSPrd(nnuc).gt.0.d0) THEN

           IF (.not.(nnuc.EQ.1. OR. nnuc.EQ.mt91
     &          .OR. nnuc.EQ.mt649 .OR. nnuc.EQ.mt849))  THEN 
             WRITE (12,*)
     &' ---------------------------------------------------------------'
             WRITE ( 8,*)
     &' ---------------------------------------------------------------'
             IF(abs(QPRod(nnuc) + ELV(LEVtarg,0)).gt.99.99) THEN
               WRITE (12,
     &'(''  Decaying nucleus '',I3,''-'',A2,''-'',I3,     ''  mass='',F1
     &0.6,'' Q-value='',F10.5)') INT(Z(nnuc)), SYMb(nnuc), ia,
     &           AMAss(nnuc), QPRod(nnuc) + ELV(LEVtarg,0)
               WRITE ( 8,
     &'(''  Decaying nucleus '',I3,''-'',A2,''-'',I3,     ''  mass='',F1
     &0.6,'' Q-value='',F10.5)') INT(Z(nnuc)), SYMb(nnuc), ia,
     &           AMAss(nnuc), QPRod(nnuc) + ELV(LEVtarg,0)
             ELSE
               WRITE (12,
     &'(''  Decaying nucleus '',I3,''-'',A2,''-'',I3,     ''  mass='',F1
     &0.6,'' Q-value='',F10.6)') INT(Z(nnuc)), SYMb(nnuc), ia,
     &           AMAss(nnuc), QPRod(nnuc) + ELV(LEVtarg,0)
               WRITE ( 8,
     &'(''  Decaying nucleus '',I3,''-'',A2,''-'',I3,     ''  mass='',F1
     &0.6,'' Q-value='',F10.6)') INT(Z(nnuc)), SYMb(nnuc), ia,
     &         AMAss(nnuc), QPRod(nnuc) + ELV(LEVtarg,0)
             ENDIF
             WRITE (12,*)
     &' ---------------------------------------------------------------'
             WRITE (8 ,*)
     &' ---------------------------------------------------------------'
             WRITE (12,*)
             WRITE (8 ,*)

           ELSE

             IF (ENDF(nnuc).gt.0) WRITE (8,
     &'(3X,''NOTE: Due to ENDF option discrete levels contribution'',/, 
     &  3X,''NOTE:   was not included in emission spectra and direct ''/
     &  3X,''NOTE:   particle contribution was shifted to the g.s.'')')

             IF (kemin.EQ.NEX(nnuc) .AND. nnuc.EQ.1) WRITE (8,
     &'(10X,''(no gamma cascade in the compound nucleus, primary transit
     &ions only)'',/)')

             WRITE (8,
     &'(1X,/,10X,''Discrete level population before gamma cascade'')')
             WRITE (8,'(1X,/,10X,40(1H-),/)')

           ENDIF

           DO il = 1, NLV(nnuc)
             IF(ISIsom(il,Nnuc).EQ.0) THEN
               WRITE (8,99070) il, ELV(il,nnuc),
     &           LVP(il,nnuc), XJLv(il,nnuc), POPlv(il,nnuc)
             ELSE
99071          FORMAT (I12,F10.5,I5,F8.1,G15.6,A7)
               WRITE (8,99071) il, ELV(il,nnuc),
     &         LVP(il,nnuc), XJLv(il,nnuc), POPlv(il,nnuc),' ISOMER'
             ENDIF

             IF (nnuc.EQ.1) THEN            
C--------------Check for the number of branching ratios
               nbr = 0
               DO ib = 1, NDBR
                 IF (BR(il,ib,2,nnuc).EQ.0.) EXIT
                 nbr = ib
               ENDDO
               IF (nbr.EQ.0 .AND. il.NE.1 .AND. FIRst_ein) WRITE (8,*)
     &           ' WARNING: Branching ratios for level ', il, ' in ',
     &           INT(A(nnuc)), '-', SYMb(nnuc), ' are missing'
               WRITE (12,99070) il, ELV(il,nnuc), LVP(il,nnuc),
     &                          XJLv(il,nnuc), POPlv(il,nnuc), nbr,
     &                          (NINT(BR(il,ib,1,nnuc)),BR(il,ib,2,nnuc)
     &                          ,ib = 1,nbr)
             ENDIF
           ENDDO ! over discrete levels

           IF ( (nnuc.EQ.1 .OR. nnuc.EQ.mt91 .OR. nnuc.EQ.mt649 .OR.
     &           nnuc.EQ.mt849)) THEN
             WRITE (8,'(1X,/,10X,40(1H-),/)')
             WRITE (8,*)
           ENDIF
C
C          Primary gammas printout 
           IF (nnuc.EQ.1 .and. NPRIm_g.GT.0) THEN  
             cspg = 0.d0
             DO il = 1, NLV(nnuc)
               cspg = cspg + CSEpg(il) 
             ENDDO
             IF(cspg.gt.0.d0) then
               WRITE (12,*)
               WRITE (12,'(1X,/,10X,40(1H-),/)')
               WRITE (12,'(2x,
     &     '' Primary g  emission cross section'',G12.6,''  mb'')') cspg
               WRITE (12,'(1X,/,10X,40(1H-),/)')
               WRITE (12,'(11x,A1,A12,A6,A5,4x,A12,A7,1x,A6,1x,A10)') 
     &       'i','    Elv(i)  ','Par  ',' Spin',
     &       ' Prim.g CS   ',' Branch','  Egamma  '
               WRITE (12,*) ' '
               DO il = 1, NLV(nnuc)
                 WRITE (12,99910) il, ELV(il,nnuc), LVP(il,nnuc),
     &           XJLv(il,nnuc), CSEpg(il), CSEpg(il)/cspg*100., ENPg(il) 
99910            FORMAT (I12,F10.5,I5,F8.1,G15.6,1x,F6.2,1x,F10.5)
               ENDDO
               WRITE (12,'(1X,/,10X,40(1H-),/)')
               WRITE (12,*)
C
               WRITE (8,*)
               WRITE (8,'(1X,/,10X,40(1H-),/)')
               WRITE (8,'(2x,
     &     '' Primary g  emission cross section'',G12.6,''  mb'')') cspg
               WRITE (8,'(1X,/,10X,40(1H-),/)')
               WRITE (8,'(11x,A1,A12,A6,A5,4x,A12,A7,1x,A6,1x,A10)') 
     &           'i','    Elv(i)  ','Par  ',' Spin',
     &           ' Prim.g CS   ',' Branch','  Egamma  '
               WRITE (8,*) ' '
               DO il = 1, NLV(nnuc)
                 WRITE (8,99910) il, ELV(il,nnuc), LVP(il,nnuc),
     &           XJLv(il,nnuc), CSEpg(il), CSEpg(il)/cspg*100., ENPg(il) 
               ENDDO
               WRITE (8,'(1X,/,10X,40(1H-),/)')
               WRITE (8,*)
             ENDIF
           ENDIF
C          Primary gammas done 

           IF ( (nnuc.EQ.1 .OR. nnuc.EQ.mt91 .OR. nnuc.EQ.mt649 .OR.
     &           nnuc.EQ.mt849)) THEN
             WRITE (12,'(1X,/,10X,40(1H-),/)')
             WRITE (12,*) ' '
C------------Write Int. Conv. Coefff. for discrete transitions
             WRITE (12,'(1X,/,10X,
     &                 ''Internal conversion coefficients'')')
             WRITE (12,'(1X,/,10X,40(1H-),/)')
             esum = 0.d0
             ftmp = 0.d0 
             DO il = 1, NLV(nnuc)
C--------------Check for the number of branching ratios
               nbr = 0
               DO ib = 1, NDBR
                 IF (BR(il,ib,2,nnuc).EQ.0.) EXIT
                 nbr = ib
               ENDDO
               esum = esum + POPlv(il,nnuc)*(EMAx(nnuc)-ELV(il,nnuc))
               ftmp = ftmp + POPlv(il,nnuc)
               WRITE (12,99065) il, ELV(il,nnuc), LVP(il,nnuc),
     &               XJLv(il,nnuc), POPlv(il,nnuc), nbr,
     &               (NINT(BR(il,ib,1,nnuc)),BR(il,ib,3,nnuc)
     &                          ,ib = 1,nbr)
99065          FORMAT (I12,F10.5,I5,F8.1,G15.6,I3,7(I4,E11.4),:/,
     &                 (53X,7(I4,E11.4)))
             ENDDO
             WRITE (12,'(1X,/,10X,40(1H-),/)')
           ENDIF
C----------gamma decay of discrete levels (DECAYD)
           CALL DECAYD(nnuc)
C
        ENDIF ! CSPrd(nnuc).gt.0.d0
C
        ia = INT(A(nnuc))
        iz = INT(Z(nnuc))
        jz = min(INT(Z(1))-iz,15)     ! adding protection for higher energies
        jn = min(INT(A(1))-ia-jz,20)  ! adding protection for higher energies

        IF (CSPrd(nnuc).GT.0.d0) THEN
           IF (kemin.EQ.NEX(nnuc) .AND. nnuc.EQ.1) WRITE (8,
     &'(1X,''(no gamma cascade in the compound nucleus, primary transiti
     &ons only)'',/)')

C----------Integrating exclusive population spectra (ENDF)
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
C          write(*,'(2x,F3.0,1x,F3.0,2x,A3,2x,F3.1)') 
C    &                 A(nnuc),Z(nnuc),' - ',ENDF(nnuc)
           IF (ENDf(nnuc).EQ.1) THEN
             nspec= min(INT(EMAx(nnuc)/DE) + 1,NDECSE-1)
             DO ispec = 1, nspec
               gtotsp = gtotsp + POPcse(0,0,ispec,INExc(nnuc))*DE
C              Write(12,*) nnuc,ispec,'g: ',
C     &           POPcse(0,0,ispec,INExc(nnuc)),CSE(ispec,0,nnuc) 
               xtotsp = xtotsp + POPcse(0,1,ispec,INExc(nnuc))*DE
c              Write(12,*) nnuc,ispec,'n: ',
c     &           POPcse(0,1,ispec,INExc(nnuc)),CSE(ispec,1,nnuc) 
               ptotsp = ptotsp + POPcse(0,2,ispec,INExc(nnuc))*DE
c              Write(12,*) nnuc,ispec,'p: ',
c     &           POPcse(0,2,ispec,INExc(nnuc)),CSE(ispec,2,nnuc) 
               atotsp = atotsp + POPcse(0,3,ispec,INExc(nnuc))*DE
c              Write(12,*) nnuc,ispec,'a: ',
c     &           POPcse(0,3,ispec,INExc(nnuc)),CSE(ispec,3,nnuc) 
               dtotsp = dtotsp + POPcse(0,4,ispec,INExc(nnuc))*DE
c              Write(12,*) nnuc,ispec,'d: ',
c     &          POPcse(0,4,ispec,INExc(nnuc)),CSE(ispec,4,nnuc) 
               ttotsp = ttotsp + POPcse(0,5,ispec,INExc(nnuc))*DE
c              Write(12,*) nnuc,ispec,'t: ',
c     &          POPcse(0,5,ispec,INExc(nnuc)),CSE(ispec,5,nnuc) 
               htotsp = htotsp + POPcse(0,6,ispec,INExc(nnuc))*DE
c              Write(12,*) nnuc,ispec,'h: ',
c     &          POPcse(0,6,ispec,INExc(nnuc)),CSE(ispec,6,nnuc) 
               emedg=emedg+POPcse(0,0,ispec,INExc(nnuc))*DE*(ispec-1)*DE
               emedn=emedn+POPcse(0,1,ispec,INExc(nnuc))*DE*(ispec-1)*DE
               emedp=emedp+POPcse(0,2,ispec,INExc(nnuc))*DE*(ispec-1)*DE
               emeda=emeda+POPcse(0,3,ispec,INExc(nnuc))*DE*(ispec-1)*DE
               emedd=emedd+POPcse(0,4,ispec,INExc(nnuc))*DE*(ispec-1)*DE
               emedt=emedt+POPcse(0,5,ispec,INExc(nnuc))*DE*(ispec-1)*DE
               emedh=emedh+POPcse(0,6,ispec,INExc(nnuc))*DE*(ispec-1)*DE
               IF (NDEJC.EQ.7) THEN
                 ctotsp = ctotsp + POPcse(0,NDEJC,ispec,INExc(nnuc))*DE
                 emedc  = emedc  + POPcse(0,NDEJC,ispec,INExc(nnuc))
     &                    *DE*(ispec - 1)*DE
               ENDIF
             ENDDO
C            IF (nspec.gt.1) THEN
C              gtotsp = gtotsp - 0.5d0*DE*
C    &                             (POPcse(0,0,1,INExc(nnuc))+
C    &                              POPcse(0,0,nspec,INExc(nnuc)))
C              xtotsp = xtotsp - 0.5d0*DE*
C    &                             (POPcse(0,1,1,INExc(nnuc))+
C    &                              POPcse(0,1,nspec,INExc(nnuc)))
C              ptotsp = ptotsp - 0.5d0*DE*
C    &                             (POPcse(0,2,1,INExc(nnuc))+
C    &                              POPcse(0,2,nspec,INExc(nnuc)))
C              atotsp = atotsp - 0.5d0*DE*
C    &                             (POPcse(0,3,1,INExc(nnuc))+
C    &                              POPcse(0,3,nspec,INExc(nnuc)))
C              dtotsp = dtotsp - 0.5d0*DE*
C    &                             (POPcse(0,4,1,INExc(nnuc))+
C    &                              POPcse(0,4,nspec,INExc(nnuc)))
C              ttotsp = ttotsp - 0.5d0*DE*
C    &                             (POPcse(0,5,1,INExc(nnuc))+
C    &                              POPcse(0,5,nspec,INExc(nnuc)))
C              htotsp = htotsp - 0.5d0*DE*
C    &                             (POPcse(0,6,1,INExc(nnuc))+
C    &                              POPcse(0,6,nspec,INExc(nnuc)))
C              emedg = emedg - 0.5d0*DE*(nspec-1)*DE*
C    &                       POPcse(0,0,nspec,INExc(nnuc))
C              emedn = emedn - 0.5d0*DE*(nspec-1)*DE*
C    &                       POPcse(0,1,nspec,INExc(nnuc))
C              emedp = emedp - 0.5d0*DE*(nspec-1)*DE*
C    &                       POPcse(0,2,nspec,INExc(nnuc))
C              emeda = emeda - 0.5d0*DE*(nspec-1)*DE*
C    &                       POPcse(0,3,nspec,INExc(nnuc))
C              emedd = emedd - 0.5d0*DE*(nspec-1)*DE*
C    &                       POPcse(0,4,nspec,INExc(nnuc))
C              emedt = emedt - 0.5d0*DE*(nspec-1)*DE*
C    &                       POPcse(0,5,nspec,INExc(nnuc))
C              emedh = emedh - 0.5d0*DE*(nspec-1)*DE*
C    &                       POPcse(0,6,nspec,INExc(nnuc))
C              IF (NDEJC.EQ.7) then
C                ctotsp = ctotsp - 0.5d0*DE*
C    &                          (POPcse(0,NDEJC,1,INExc(nnuc))+
C    &                           POPcse(0,NDEJC,nspec,INExc(nnuc)))
C                emedc = emedc - 0.5d0*DE*(nspec-1)*DE*
C    &                       POPcse(0,NDEJC,nspec,INExc(nnuc))
C              ENDIF
C            ENDIF
             POPcs(0,INExc(nnuc)) = gtotsp
             POPcs(1,INExc(nnuc)) = xtotsp
             POPcs(2,INExc(nnuc)) = ptotsp
             POPcs(3,INExc(nnuc)) = atotsp
             POPcs(4,INExc(nnuc)) = dtotsp
             POPcs(5,INExc(nnuc)) = ttotsp
             POPcs(6,INExc(nnuc)) = htotsp
             IF (NDEJC.EQ.7) POPcs(NDEJC,INExc(nnuc)) = ctotsp

             WRITE (12,*)
             WRITE (8,*)
             DO nejc = 0, NDEJC         !loop over ejectiles
               IF (POPcs(nejc,INExc(nnuc)).LE.0.d0) CYCLE
               IF (nejc.EQ.0) THEN
                 cejectile = 'gammas   '
               ELSEIF (nejc.EQ.1) THEN
                 cejectile = 'neutrons '
               ELSEIF (nejc.EQ.2) THEN
                 cejectile = 'protons  '
               ELSEIF (nejc.EQ.3) THEN
                 cejectile = 'alphas   '
               ELSEIF (nejc.EQ.4) THEN
                 cejectile = 'deuterons'
               ELSEIF (nejc.EQ.5) THEN
                 cejectile = 'tritons  '
               ELSEIF (nejc.EQ.6) THEN
                 cejectile = 'helium-3 '
               ELSEIF (nejc.EQ.NDEJC) THEN
                 cejectile = 'lt. ions '
               ENDIF
               WRITE (12,9753) iz, SYMb(nnuc), ia, 
     &           POPcs(nejc,INExc(nnuc)),cejectile
9753           FORMAT(1X,I3,'-',A2,'-',I3,
     &           ' population cross section',G12.6,'  mb   : ',A9) 
             ENDDO
             WRITE (8,*)
             IF (gtotsp.NE.0) emedg = emedg/gtotsp
             IF (xtotsp.NE.0) emedn = emedn/xtotsp 
             IF (ptotsp.NE.0) emedp = emedp/ptotsp 
             IF (atotsp.NE.0) emeda = emeda/atotsp 
             IF (dtotsp.NE.0) emedd = emedd/dtotsp 
             IF (ttotsp.NE.0) emedt = emedt/ttotsp 
             IF (htotsp.NE.0) emedh = emedh/htotsp 
             IF (ctotsp.NE.0) emedc = emedc/ctotsp
C--------------Add contributions to discrete levels for MT=91,649,849
C--------------(merely for checking purpose)
             DO nejc = 0, NDEjc
               xnorm(nejc,INExc(nnuc)) = 1.0d0
             END DO
             IF (nnuc.EQ.mt91) THEN
               nejc = 1
               WRITE (8,'(6X,'' Cont. popul. before g-cascade '',
     &                G12.6,''  mb  '')') xtotsp
               WRITE (8,'(6X,'' Disc. popul. before g-cascade '',
     &                G12.6,''  mb  '')') CSDirlev(1,nejc)
C              WRITE (12,'(5X,'' Cont. popul. before g-cascade '',
C    &                G12.6,''  mb  '')') xtotsp
C              WRITE (12,'(5X,'' Disc. popul. before g-cascade '',
C    &                G12.6,''  mb  '')') CSDirlev(1,nejc)
               xtotsp = xtotsp + CSDirlev(1,nejc)
               WRITE (8,'(6X,'' Total popul. before g-cascade '',
     &                G12.6,''  mb  '')') xtotsp
             ELSEIF (nnuc.EQ.mt649) THEN
               nejc = 2
               WRITE (8,'(6X,'' Cont. popul. before g-cascade '',
     &                G12.6,''  mb  '')') ptotsp
               WRITE (8,'(6X,'' Disc. popul. before g-cascade '',
     &                G12.6,''  mb  '')') CSDirlev(1,nejc)
C              WRITE (12,'(5X,'' Cont. popul. before g-cascade '',
C    &                G12.6,''  mb  '')') ptotsp
C              WRITE (12,'(5X,'' Disc. popul. before g-cascade '',
C    &                G12.6,''  mb  '')') CSDirlev(1,nejc)
               ptotsp = ptotsp + CSDirlev(1,nejc)     
               WRITE (8,'(6X,'' Total popul. before g-cascade '',
     &                G12.6,''  mb  '')') ptotsp
             ELSEIF (nnuc.EQ.mt849) THEN
               nejc = 3
               WRITE (8,'(6X,'' Cont. popul. before g-cascade '',
     &                G12.6,''  mb  '')') atotsp
               WRITE (8,'(6X,'' Disc. popul. before g-cascade '',
     &                G12.6,''  mb  '')') CSDirlev(1,nejc)
C              WRITE (12,'(5X,'' Cont. popul. before g-cascade '',
C    &                G12.6,''  mb  '')') atotsp
C              WRITE (12,'(5X,'' Disc. popul. before g-cascade '',
C    &                G12.6,''  mb  '')') CSDirlev(1,nejc)
               atotsp = atotsp + CSDirlev(1,nejc)
               WRITE (8,'(6X,'' Total popul. before g-cascade '',
     &                G12.6,''  mb  '')') atotsp
             ELSE
               IF (LHMs.GT.0 .and. atotsp.LT.1.0d-8) THEN
                 totsp = CSprd(nnuc) - dtotsp - htotsp - ttotsp
                 IF(NDEJC.EQ.7) totsp = totsp - ctotsp
                 nxsp = 0
                 IF(xtotsp.GT.0.0d0) THEN
                   IF(ttotsp.GT.0.0d0) THEN
                     nxsp=MAX(INT((xtotsp-dtotsp)/totsp+0.5d0),0)
                     xnorm(1,INExc(nnuc)) =(nxsp*totsp+dtotsp)/xtotsp
                     xtotsp = nxsp*totsp + dtotsp
                   ELSE
                     nxsp=INT(xtotsp/totsp+0.5d0)
                     xnorm(1,INExc(nnuc)) = nxsp*totsp/xtotsp
                     xtotsp = nxsp*totsp
                   ENDIF
                   POPcs(1,INExc(nnuc)) = xtotsp
                   IF(ABS(1.0d0 - xnorm(1,INExc(nnuc))).GT.0.01d0) 
     &               WRITE(8,
     &      '(''  WARNING: Exclusive neutron spectrum renormalized by'',
     &                     f6.3)') xnorm(1,INExc(nnuc))
                 ENDIF
                 npsp = 0
                 IF(ptotsp.GT.0.0d0) THEN
                   IF(htotsp.GT.0.0d0) THEN
                     npsp=MAX(INT((ptotsp-dtotsp)/totsp+0.5d0),0)
                     xnorm(2,INExc(nnuc)) =(npsp*totsp+dtotsp)/ptotsp
                     ptotsp = npsp*totsp + dtotsp
                   ELSE
                     npsp=INT(ptotsp/totsp+0.5d0)
                     xnorm(2,INExc(nnuc)) = npsp*totsp/ptotsp
                     ptotsp = npsp*totsp
                   ENDIF
                   POPcs(2,INExc(nnuc)) = ptotsp
                   IF(ABS(1.0d0 - xnorm(2,INExc(nnuc))).GT.0.01d0) 
     &               WRITE(8,
     &      '(''  WARNING: Exclusive  proton spectrum renormalized by'',
     &                     f6.3)') xnorm(2,INExc(nnuc))
                 ENDIF
               ENDIF
             ENDIF
             WRITE (8,*) 
             WRITE (8,'(116(1H_))') 
             WRITE (8,*) 
     &        'Population of residual nuclei (exclusive spectra - CMS)'
             WRITE (8,
     &           '('' Energy'',14x,''gamma'',9x,''neutron'',8x,
     &             ''proton'',10x,''alpha'',10x,''deut '',10x,
     &             ''trit '',10x,''He-3 '')')
             WRITE (8,'(116(1H-))') 

             DO ispec = 1, nspec
               POPcse(0,1,ispec,INExc(nnuc)) = 
     &                xnorm(1,INExc(nnuc))*POPcse(0,1,ispec,INExc(nnuc))
               POPcse(0,2,ispec,INExc(nnuc)) = 
     &                xnorm(2,INExc(nnuc))*POPcse(0,2,ispec,INExc(nnuc))
               IF (NDEJC.EQ.7) THEN
                      WRITE (8,'(9g15.5)') (ispec - 1)*DE,
     &                      POPcse(0,0,ispec,INExc(nnuc)),
     &                      POPcse(0,1,ispec,INExc(nnuc)),
     &                      POPcse(0,2,ispec,INExc(nnuc)),
     &                      POPcse(0,3,ispec,INExc(nnuc)),
     &                      POPcse(0,4,ispec,INExc(nnuc)),
     &                      POPcse(0,5,ispec,INExc(nnuc)),
     &                      POPcse(0,6,ispec,INExc(nnuc)),
     &                      POPcse(0,NDEJC,ispec,INExc(nnuc))
               ELSE
                      WRITE (8,'(8g15.5)') (ispec - 1)*DE,
     &                      POPcse(0,0,ispec,INExc(nnuc)),
     &                      POPcse(0,1,ispec,INExc(nnuc)),
     &                      POPcse(0,2,ispec,INExc(nnuc)),
     &                      POPcse(0,3,ispec,INExc(nnuc)),
     &                      POPcse(0,4,ispec,INExc(nnuc)),
     &                      POPcse(0,5,ispec,INExc(nnuc)),
     &                      POPcse(0,6,ispec,INExc(nnuc))
               ENDIF
             ENDDO
             IF (NDEJC.EQ.7) THEN
               WRITE (8,'(116(1H-))') 
               WRITE (8,'(''Tot.popul.'',5X,8g15.6)')gtotsp, xtotsp, 
     &                   ptotsp, atotsp,dtotsp,ttotsp,htotsp,ctotsp
               WRITE (8,'(''E-aver.'',8X,8g15.6)')emedg, emedn, emedp,
     &                emeda, emedd, emedt, emedh, emedc
             ELSE  
               WRITE (8,'(116(1H-))') 
               WRITE (8,'(''Tot.popul.'',5X,8g15.6)')gtotsp, xtotsp,
     &                   ptotsp, atotsp,dtotsp,ttotsp,htotsp    
               WRITE (8,'(''E-aver.'',8X,8g15.6)')emedg, emedn, emedp,
     &                emeda, emedd, emedt, emedh 
             ENDIF            

             cmulg = gtotsp/CSPrd(nnuc)
             cmuln = xtotsp/CSPrd(nnuc)
             cmulp = ptotsp/CSPrd(nnuc)
             cmula = atotsp/CSPrd(nnuc)
             cmuld = dtotsp/CSPrd(nnuc)
             cmult = ttotsp/CSPrd(nnuc)
             cmulh = htotsp/CSPrd(nnuc)
             WRITE (8,'(''Multip.'',8X,8g15.6)')cmulg, cmuln, cmulp,
     &                cmula, cmuld, cmult, cmulh

             WRITE (8,'(116(1H-)/)') 
           ENDIF
         ENDIF
         IF (CSFis.NE.0.0D0) THEN
           WRITE (80,*)
           WRITE (8,*)
           IF (IOUt.GT.0) THEN
          
             DO m = 1, INT(FISmod(nnuc)) + 1
               WFIsm(m) = 0.d0
               IF (CSFis.GT.0.d0) WFIsm(m) = CSFism(m)/CSFis
               IF( NINT(FISmod(nnuc)).GT.0 .and. 
     >             NINT(FISshi(nnuc)).NE.1 )
     >           WRITE (80,*) '    Mode=', m, '   weight=', WFIsm(m)
             ENDDO
             IF(NINT(FISshi(nnuc)).NE.1)
     >         WRITE (80,*) '   Fission cross section=', CSFis, ' mb'
           ENDIF
           CSPfis(nnuc) = CSFis
           WRITE (8,
     &'(1X,I3,''-'',A2,''-'',I3,'' fission cross  section '',G12.5,''
     &mb  ''/)') iz, SYMb(nnuc), ia, CSFis
         ENDIF
         TOTcsfis = TOTcsfis + CSFis
C--------Add compound elastic to shape elastic before everything falls
C--------down on the ground state
9876     IF (nnuc.EQ.1  .AND. INT(AEJc(0)).NE.0
     &                       .AND. POPlv(LEVtarg,mt2).GT.0.) THEN
           WRITE (8,*)
           WRITE (8,*) ' Incident energy (CMS)      ', EIN, ' MeV'
           WRITE (8,*) ' Shape elastic cross section',
     &                     ELAred*ELAcs, ' mb'

C----------CN contribution to elastic ddx
           ELCncs = POPlv(LEVtarg,mt2)/4.d0/PI 

           if(.not.CN_isotropic .and. ELCncs.LT.0.05d0) then    
             CN_isotropic = .TRUE.
             WRITE(8,*)
             WRITE(8,*) 
     &       'CN angular distribution assumed isotropic at Einc = ',
     &       sngl(EINl)
             WRITE(12,*)      
     &       'CN angular distribution assumed isotropic at Einc = ',
     &       sngl(EINl)

             WRITE(8,*)
           endif  

           IF (ELCncs.EQ.0) then

             WRITE (8,*) ' WARNING: CN elastic is 0'

           ELSE

             WRITE (8,*) ' CN elastic cross section   ',
     &                sngl(POPlv(LEVtarg,mt2)),' mb'

             IF(CN_isotropic) then   

               WRITE (8,*)
     &          '    Elastic=', sngl(ELCncs), ' mb/str'
               DO iang = 1, NDANG
                 cel_da(iang) = ELCncs ! isotropic
               ENDDO

             ELSE

               WRITE (8,*) ' CN elas. cross section (BB)',
     &           sngl(4.d0*pi*PL_CN(0,LEVtarg)),' mb'

               xs_norm=1.d0
 	         IF(PL_CN(0,LEVtarg).gt.0.d0) 
     &           xs_norm = ELCncs/PL_CN(0,LEVtarg)

               IF(INTerf.eq.1) then
                 WRITE (110,'(1x,E12.5,3x,11(F9.2,1x),A17)') 
     &           EINl, 4.d0*pi*ELCncs,  
     &           (4.d0*pi*xs_norm*PL_CN(0,ilevcol),ilevcol=1,10),
     &           'ENG-WEID. TRANSF.'  
               ELSE
                 WRITE (110,'(1x,E12.5,3x,11(F9.2,1x))') 
     &           EINl, 4.d0*pi*ELCncs,  
     &           (4.d0*pi*xs_norm*PL_CN(0,ilevcol),ilevcol=1,10)
               ENDIF                

               WRITE (8,*) 
               WRITE (8,*) ' Nonisotropic Compound to discrete levels in
     &cluding the Compound Elastic'
               WRITE (8,*) 

 	         IF(PL_CN(0,LEVtarg).gt.0.d0) then
C	          write(*,*) 'NORM=',xs_norm
                DO iang = 1, NDANG
                  cel_da(iang) = xs_norm*GET_DDXS(CANGLE(iang),LEVtarg)
                ENDDO

                IF(DABS(xs_norm-1.d0).gt.1.d-4) then
C                 WRITE(*,*) 'ELCncs = POPlv(LEVtarg,mt2)/4/PI =',ELCncs
C                 WRITE(*,*) 'PL_CN(0,LEVtarg)=',PL_CN(0,LEVtarg)
                  WRITE(8,*)' ELCncs = POPlv(LEVtarg,mt2)/4/PI =',ELCncs
                  WRITE(8,*)' PL_CN(0,LEVtarg)=',PL_CN(0,LEVtarg)
                   WRITE(8,*) 
     &       ' Renormalizing CN Ang.Dist. by ELCncs/PL_CN(0,LEVtarg)=',
     &       sngl(xs_norm)
                   WRITE(8,*) 
                ENDIF

               ELSE

                 DO iang = 1, NDANG
                   cel_da(iang) = ELCncs ! isotropic
                 ENDDO

               ENDIF

             ENDIF

           ENDIF

           gang = 180.d0/(NDAng - 1)
           angstep = 180.d0/(NANgela - 1)

           IF(.NOT.CN_isotropic) then   
             WRITE (8,99016)
             WRITE (8,99020)
             DO iang = 1, NANgela/4 + 1
               imint = 4*(iang - 1) + 1
               imaxt = MIN0(4*iang,NANgela)
               WRITE (8,99025) 
     &           ((j - 1)*angstep,cel_da(j),j = imint,imaxt)
             ENDDO
           ENDIF

           IF (ncollx.GT.0) THEN
C----------------Locate position of the projectile among ejectiles
                 CALL WHEREJC(IZAejc(0),nejcec,iloc)
C
                 its = ncollx
                 IF (CSAlev(1,ICOller(2),nejcec).GT.0) THEN
                   WRITE(8,99029)
                   WRITE(8,99030) (ICOller(ilv),ilv = 2,MIN(its,10))
                   WRITE(8,99031) (ELV(ICOller(ilv),nnurec),
     &               ilv = 2,MIN(its,10))
                   WRITE(8,99033) (XJLv(ICOller(ilv),nnurec)*
     &               LVP(ICOller(ilv),nnurec),D_DEF(ilv,2),
     &               ilv = 2,MIN(its,10))  
                   WRITE(8,*) ' '
                   DO iang = 1, NDANG
                     WRITE (8,99035) (iang - 1)*gang,
     &               (CSAlev(iang,ICOller(ilv),nejcec),
     &               ilv = 2,MIN(its,10)) 
                   ENDDO
                   WRITE(8,*) ' '
                   WRITE(8,99041) 1,(POPlv(ICOller(ilv),nnurec),
     &               ilv= 2,MIN(its,10))
C
                   IF(its.gt.10) THEN
                     WRITE(8,*) ' '
                     WRITE(8,99030)(ICOller(ilv),ilv = 11,MIN(its,20))
                     WRITE(8,99032)(ELV(ICOller(ilv),nnurec),
     &                 ilv=11,MIN(its,20))
                     WRITE(8,99034)(XJLv(ICOller(ilv),nnurec)*
     &                 LVP(ICOller(ilv),nnurec),D_DEF(ilv,2),
     &                 ilv=11,MIN(its,20))  
                     WRITE (8,*) ' '
                     DO iang = 1, NDANG
                       WRITE (8,99035) (iang - 1)*gang,
     &                 (CSAlev(iang,ICOller(ilv),nejcec),
     &                 ilv = 11,MIN(its,20))
                     ENDDO
                     WRITE (8,*) ' '
                     WRITE (8,99041) 2,(POPlv(ICOller(ilv),nnurec),
     &                 ilv = 11,MIN(its,20))
                   ENDIF
C
                   IF(its.gt.20) THEN
                     WRITE(8,*) ' '
                     WRITE(8,99030)(ICOller(ilv),ilv = 21,MIN(its,30))
                     WRITE(8,99032)(ELV(ICOller(ilv),nnurec),
     &                 ilv=21,MIN(its,30))
                     WRITE(8,99034)(XJLv(ICOller(ilv),nnurec)*
     &                 LVP(ICOller(ilv),nnurec),D_DEF(ilv,2),
     &                 ilv=21,MIN(its,30))
                     WRITE (8,*) ' '
                     DO iang = 1, NDANG
                       WRITE (8,99035) (iang - 1)*gang,
     &                 (CSAlev(iang,ICOller(ilv),nejcec),
     &                 ilv=21,MIN(its,30))
                     ENDDO
                     WRITE (8,*) ' '
                     WRITE (8,99041) 3,(POPlv(ICOller(ilv),nnurec),
     &                 ilv=21,MIN(its,30))
                   ENDIF
C                  Because of the ENDF format restrictions the maximum
C                     number of discrete levels is limited to 40
                   IF(its.gt.30) THEN
                     WRITE(8,*) ' '
                     WRITE(8,99030)(ICOller(ilv),ilv = 31,MIN(its,40))
                     WRITE(8,99032)(ELV(ICOller(ilv),nnurec),
     &                 ilv = 31,MIN(its,40))
                     WRITE(8,99034)(XJLv(ICOller(ilv),nnurec)*
     &                 LVP(ICOller(ilv),nnurec),D_DEF(ilv,2),
     &                 ilv = 31,MIN(its,40))
                     WRITE (8,*) ' '
                     DO iang = 1, NDANG
                       WRITE (8,99035) (iang - 1)*gang,
     &                 (CSAlev(iang,ICOller(ilv),nejcec),
     &                 ilv = 31,MIN(its,40))
                     ENDDO
                     WRITE (8,*) ' '
                     WRITE (8,99041) 4,(POPlv(ICOller(ilv),nnurec),
     &                 ilv = 31,MIN(its,40))
                   ENDIF
                   WRITE (8,*) ' '
                 ENDIF
           ENDIF ! ncollx > 0 ?

         ENDIF
         IF(CSPrd(nnuc).GT.0.d0) THEN
           checkXS = checkXS + CSPrd(nnuc)
           checkprd = CSPrd(nnuc)
           xcross(NDEJC+2,jz,jn) = CSPrd(nnuc)
           ilv = 0  ! count of meta-stable states
           ftmp_gs = CSPrd(nnuc)
           DO its= NLV(Nnuc), 2, -1
            IF(ISIsom(its,Nnuc).EQ.1) THEN
              ilv = ilv + 1
              WRITE(12,'(1X,I3,''-'',A2,''-'',I3,
     &         '' isomer state population  '',G12.6,
     &         '' mb (m'',I1,'' E='',F7.4,''MeV Jp='',F5.1,'')'')')
     &         iz, SYMb(nnuc), ia, POPlv(its,Nnuc),
     &         ilv, ELV(its,Nnuc), LVP(its,Nnuc)*XJLv(its,Nnuc)
              ftmp_gs = ftmp_gs - POPlv(its,Nnuc)
C             CSPrd(nnuc) = CSPrd(nnuc) - POPlv(its,Nnuc)
            ENDIF
           ENDDO
           IF(ilv.GT.0) WRITE(12,'(1X,I3,''-'',A2,''-'',I3,
     &           '' ground state population  '',G12.6,'' mb'')')
     &           iz, SYMb(nnuc), ia, ftmp_gs
         ENDIF

5753     FORMAT(1X,I3,'-',A2,'-',I3,
     &    '    fission cross section',G12.6,'  mb') 
         IF (CSFis.gt.0.) WRITE (12,5753) iz, SYMb(nnuc), ia, CSFis

         IF(CSPrd(nnuc).gt.0.d0) then 
           WRITE (12,*)
           WRITE (8,*)
           WRITE (8,
     &'(1X,I3,''-'',A2,''-'',I3,'' production cross section '',G12.6,
     &'' mb  '',''      reac: '',A21)') iz, SYMb(nnuc), ia, CSPrd(nnuc),
     &                             REAction(nnuc)
           WRITE (12,
     &'(1X,I3,''-'',A2,''-'',I3,'' production cross section'',G12.6,
     &''  mb '',''      reac: '',A21)') iz, SYMb(nnuc), ia, CSPrd(nnuc),
     &                             REAction(nnuc)
           checkprd = checkprd + CSFis
           xcross(NDEJC+1,jz,jn) = CSFis
         ENDIF

C        WRITE (8,*)
C        Integral is calculated by trapezoidal rule being consistent with cross section
         IF(IOUt.GT.0) CALL AUERST(nnuc,0,0)
         IF(CSEmis(0,nnuc).gt.0) THEN
           WRITE (12,'(10x,
     &                 '' g  emission cross section'',G12.6,''  mb'')')
     &       CSEmis(0,nnuc)
           if(nnuc.eq.1) WRITE (12,'(2x,
     &     '' Primary g  emission cross section'',G12.6,''  mb'')') cspg
         ENDIF

         xcross(0,jz,jn) = CSEmis(0,nnuc)
C----------------------------------------------------------------------
         IF(CSPrd(nnuc).GT.0.d0) THEN
           DO nejc = 1, NEJcm
             ares = A(nnuc) - AEJc(nejc)
             zres = Z(nnuc) - ZEJc(nejc)
C------------Residual nuclei must be heavier than alpha
             if(ares.le.4. and. zres.le.2.) cycle
             izares = INT(1000.0*zres + ares)
             CALL WHERE(izares,nnur,iloc)
             checkprd = checkprd +  CSEmis(nejc,nnuc)
             xcross(nejc,jz,jn) = CSEmis(nejc,nnuc)
             IF(iloc.EQ.1 .AND. CSEmis(nejc,nnuc).GT.0) WRITE (12,
     &       '('' iloc=1! CSEmis('',i1,'','',i2,'')='',G12.5)')
     &                                 nejc,nnuc,CSEmis(nejc,nnuc)
             IF(iloc.EQ.1) CYCLE
C            IF(CSEmis(nejc,nnuc).LE.1.d-8) CYCLE
             IF(CSEmis(nejc,nnuc).LE.0) CYCLE
             WRITE (12,
     &           '(11X,A2,'' emission cross section'',G12.6,''  mb'')')
     &             SYMbe(nejc), CSEmis(nejc,nnuc)
             IF (ENDf(nnuc).EQ.1 .and. FIRst_ein .and. IOUT.GT.5 .and.
     &           AEJc(0).LE.4.)  ! excluding HI reactions
     &           CALL PLOT_EMIS_SPECTRA(nnuc,nejc)
C
C            Integral is calculated by trapezoidal rule being consistent with cross section
             IF (IOUt.GT.0) CALL AUERST(nnuc,nejc,0) 
C------------Print residual nucleus population
             poptot = 0.d0
             IF (NEX(nnur).GT.0) THEN !avoid summing non-existent continuum
                DO j = 1, NLW
                  DO i = 1, NEX(nnur)
                    poptot = poptot + POP(i,j,1,nnur) + POP(i,j,2,nnur)
                  ENDDO
                  poptot = poptot 
     &                   - 0.5*(POP(1,j,1,nnur) + POP(1,j,2,nnur))
     &                   - 0.5*(POP(NEX(nnur),j,1,nnur) 
     &                   +      POP(NEX(nnur),j,2,nnur))
                ENDDO
             ENDIF 
             poptot = poptot*DE
             poplev = 0.d0
             DO i = 1, NLV(nnur)
               poplev = poplev + POPlv(i,nnur)
             ENDDO
             IF(LHMs.NE.0) THEN
               IF(nejc.GT.2) THEN
                 poptot = poptot - POPcon(nnur)
                 poplev = poplev - POPdis(nnur)
               ELSE
                 poptot = poptot + CSHms(1,nnur) + CSHms(2,nnur)
               ENDIF
             ENDIF

             if(A(nnuc).eq.A(1) .and. Z(nnuc).eq.Z(1) 
     &                          .and. ENDF(nnuc).gt.0) then
               WRITE (12,
     &            '(13x,   '' total population      '',G12.6,''  mb'')')
     &            poplev + poptot
               WRITE (12,
     &            '(13x,   '' total popul.continuum '',G12.6,''  mb'')')
     &            poptot
               WRITE (12,
     &            '(13x,   '' total popul.disc.lev. '',G12.6,''  mb'')')
     &            poplev

C              WRITE (8,*) '    RESIDUAL = TARGET NUCLEUS'
C              WRITE (8,
C    &         '(1x,''    Total population      '',G12.6,''  mb'')')
C    &          poplev + poptot
C              WRITE (8,
C    &         '(1x,''    Total popul.continuum '',G12.6,''  mb'')')
C    &          poptot
C              WRITE (8,
C    &         '(1x,''    Total popul.disc.lev. '',G12.6,''  mb'')')
C    &          poplev
C              WRITE (8,*)
             endif

             IF (IOUt.EQ.4) THEN
               ia = INT(A(nnur))
               WRITE (8,*) ' '
               WRITE (8,*) '**************************** '
               WRITE (8,'('' Residual nucleus '',I3,''-'',A2,/)') ia,
     &                SYMb(nnur)
               WRITE (8,'('' Positive parities population'',/)')
               do i = NEX(nnur),1,-1
                 ftmp = 0.d0
                 do j = 1,12
                   ftmp = ftmp + POP(i,j,1,nnur)
                 enddo
                 if(ftmp.gt.0.d0)
     &             WRITE (8,99075) EX(i,nnur),(POP(i,j,1,nnur),j = 1,12)
               enddo
               WRITE (8,*) ' '
               WRITE (8,'('' Negative parities population'',/)')
               do i = NEX(nnur),1,-1
                 ftmp = 0.d0
                 do j = 1,12
                   ftmp = ftmp + POP(i,j,2,nnur)
                 enddo
                 if(ftmp.gt.0.0)
     &             WRITE (8,99075) EX(i,nnur),(POP(i,j,2,nnur),j = 1,12)
               enddo
               WRITE (8,'('' '')')

             ENDIF

           ENDDO   !over ejectiles
           xcross(NDEJC+3,jz,jn) = checkprd

         ENDIF ! if CSProd > 0

      RETURN         
99016 FORMAT (/' ',46x,'COMP. ELASTIC DIFFERENTIAL CROSS-SECTION',/,' ',
     &        46x,40('*'),/,' ',50x,'CENTER-OF-MASS SYSTEM',/)
99020 FORMAT (' ',5x,4('    TETA ',2x,'D.SIGMA/D.OMEGA',6x),/)
99025 FORMAT (' ',5x,4(1p,e12.5,2x,e12.5,6x))
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
99041 FORMAT (' TOT.INEL',I1,1x,11(E12.6,2x))
C
99070 FORMAT (I12,F10.5,I5,F8.1,G15.6,I3,7(I4,F7.4),:/,(53X,7(I4,F7.4)))
99075 FORMAT (1X,F5.2,12E10.3)
      END
      SUBROUTINE GET_RECOIL(Ke,Nnuc)
Ccc
Ccc   ********************************************************************
Ccc   *                                                         class:ppu*
Ccc   *                         RECOIL                                   *
Ccc   *                                                                  *
Ccc   *                                                                  *
Ccc   *  Constructs recoil spectra:                                      *
Ccc   *  Each excitation bin is given recoil spectrum, when a particle   *
Ccc   *  is emitted its recoil velocity is vector added to the parent    *
Ccc   *  recoil velocity and a resulting spectrum is summed upon daughter*
Ccc   *  recoil spectrum corresponding to the populated energy bin in the*
Ccc   *  daughter (kinematical normalization taken into account).        *
Ccc   *  Daughter recoil spectra will be distributed between             *
Ccc   *  adjacent bins (inversly proportional to the                     *
Ccc   *  distance of the actual energy to the bin energy                 *
Ccc   *  in order to conserve energy).                                   *
Ccc   *  Requires that continuum spectra from each bin are stored on the *
Ccc   *  AUSpec array and those to discrete levels on the REClev for each*
Ccc   *  reaction mechanism considered in the calculations.              *
Ccc   *                                                                  *
Ccc   ********************************************************************
Ccc
      INCLUDE 'dimension.h'
      INCLUDE 'global.h'
C
C Dummy arguments
C
      INTEGER Ke, Nnuc
C
C Local variables
C
      DOUBLE PRECISION coeff, dang, erecejc, erecod, erecoil, erecpar,
     &  exqcut, recorr, sumnor, weight, ares, zres, csmsdl
      INTEGER icse, ie, il, ire, irec, na, nejc, nnur, izares, iloc
C
C
C-----Normalize recoil spectrum of the parent
      sumnor = 0.d0
      DO ire = 1, NDEREC
         sumnor = sumnor + RECcse(ire,Ke,Nnuc)
      ENDDO
      IF (sumnor.NE.0.0D0) THEN
         DO ire = 1, NDEREC
            RECcse(ire,Ke,Nnuc) = RECcse(ire,Ke,Nnuc)/sumnor
         ENDDO
      ENDIF
      dang = PI/FLOAT(NDANG - 1)
      coeff = dang/DERec/2.0
      DO nejc = 1, NEJcm   !over ejectiles
         ares = A(nnuc) - AEJc(nejc)
         zres = Z(nnuc) - ZEJc(nejc)
C--------Residual nuclei must be heavier than alpha
         if(ares.le.4. and. zres.le.2.) cycle
         izares = INT(1000.0*zres + ares)
         CALL WHERE(izares,nnur,iloc)
         if(iloc.eq.1) CYCLE
C--------Decay to continuum
C--------recorr is a recoil correction factor that
C--------divides outgoing energies
         recorr = DBLE(ares)/AEJc(nejc)
         exqcut = EX(Ke,Nnuc) - Q(nejc,Nnuc) - ECUt(nnur)
         DO ie = 1, NDECSE !over ejec. energy (daughter excitation)
            icse = (exqcut - (ie - 1)*DE)/DE + 1.001
C-----------Daughter bin
            IF (icse.LE.0) EXIT
            erecejc = (ie - 1)*DE/recorr
            DO ire = 1, NDEREC          !over recoil spectrum
               erecpar = (ire - 1)*DERec
               DO na = 1, NDANG
                 erecoil = erecejc + erecpar + 2.0*SQRT(erecejc*erecpar)
     &                      *CANgler(na)
                 irec = erecoil/DERec + 1.001
                 weight = (erecoil - (irec - 1)*DERec)/DERec
                 IF (irec + 1.GT.NDEREC) EXIT
                 csmsdl = RECcse(ire,Ke,Nnuc)*AUSpec(ie,nejc)*
     &                    SANgler(na)*coeff*recorr
                 RECcse(irec,icse,nnur) = RECcse(irec,icse,nnur)
     &               + csmsdl*(1.0 - weight)
                 RECcse(irec + 1,icse,nnur) = RECcse(irec + 1,icse,nnur)  
     &               + csmsdl*weight
               ENDDO                  !over angles
            ENDDO                  !over recoil spectrum
         ENDDO                  !over  daugther excitation
C--------Decay to discrete levels (stored with icse=0)
         exqcut = exqcut + ECUt(nnur)
         DO il = 1, NLV(nnur)
            erecod = exqcut - ELV(il,nnur)   !emission energy
            erecod = erecod/recorr
            IF (erecod.LT.0) GOTO 100
            DO ire = 1, NDEREC      !over recoil spectrum
               DO na = 1, NDANG !over angles
                  erecoil = (ire - 1)*DERec + erecod +
     &                       2.0*SQRT((ire - 1)*DERec*erecod)
     &                      *CANgler(na)
                  irec = erecoil/DERec + 1.001
                  weight = (erecoil - (irec - 1)*DERec)/DERec
                  IF (irec.GT.NDEREC) GOTO 60
                  RECcse(irec,0,nnur) = RECcse(irec,0,nnur)
     &                                  + RECcse(ire,Ke,Nnuc)
     &                                  *REClev(il,nejc)*(1.0 - weight)
     &                                  *SANgler(na)*coeff
c------------------------
!                 IF(irec.EQ.5 .AND. RECcse(irec,0,nnur).GT.0
!     &               .AND.na.EQ.10) THEN
!                  WRITE(8,*) '       Parent bin', Ke, 'Nnuc', Nnuc
!                  WRITE(8,*) 'Recoil bin', ire
!                  WRITE(8,*) 'Erecoil ', erecoil, erecod, nnuc
!                  WRITE(8,*) 'RECcse, RECcse par, REClev',
!     &            RECcse(irec,0,nnur),RECcse(ire,Ke,Nnuc),
!     &            REClev(il,nejc)
!                  ENDIF
c------------------------
                  IF (irec + 1.GT.NDEREC) GOTO 60
                  RECcse(irec + 1,0,nnur) = RECcse(irec + 1,0,nnur)
     &               + RECcse(ire,Ke,Nnuc)*REClev(il,nejc)
     &               *weight*SANgler(na)*coeff
               ENDDO                  !over angles
   60       ENDDO                  !over recoil spectrum
         ENDDO                  !over levels
  100 ENDDO                  !over ejectiles
C-----
C-----Parent recoil spectrum after gamma decay
C-----
      nnur = Nnuc
      nejc = 0
C-----gamma decay to continuum
      DO ie = 1, NDECSE !over ejec. energy (daughter excitation)
         icse = (EX(Ke,Nnuc) - (ie - 1)*DE - ECUt(nnur))/DE + 1.001
         IF (icse.LE.0) CYCLE  
C--------!daughter bin
         DO irec = 1, NDEREC         !over recoil spectrum
           RECcse(irec,icse,nnur)
     &          = RECcse(irec,icse,nnur) + RECcse(irec,Ke,Nnuc)
     &          *AUSpec(ie,0)/DERec
         ENDDO                  !over recoil spectrum
      ENDDO                  !over  daugther excitation
C-----gamma decay to discrete levels (stored with icse=0)
      DO il = 1, NLV(nnur)
         DO ire = 1, NDEREC             !over recoil spectrum
            RECcse(ire,0,nnur) = RECcse(ire,0,nnur)
     &                           + RECcse(ire,Ke,Nnuc)*REClev(il,nejc)
     &                           /DERec
         ENDDO                  !over recoil spectrum
      ENDDO                  !over levels
      END

      SUBROUTINE PRINT_RECOIL(Nnuc,React,qout)
C-----
C-----Prints recoil spectrum of nnuc residue
C-----
      implicit none
      INCLUDE 'dimension.h'
      INCLUDE 'global.h'
C
C Dummy arguments
C
      INTEGER Nnuc
      CHARACTER*21 React
      DOUBLE PRECISION qout
C
C Local variables
C
      DOUBLE PRECISION csum, ftmp, corr, xsdisc, esum, recorr, cmul
      INTEGER ie, ilast

      IF (CSPrd(Nnuc).LE.0.0D0 .or. NINT(A(Nnuc)).eq.NINT(A(1))) RETURN
C-----Normalize recoil spectra to remove eventual inaccuracy
C-----due to numerical integration of angular distributions
C-----and find last non-zero cross section for printing
      recorr = A(Nnuc)/(A(1)-A(Nnuc))

C
C     To get consistent integral value
C
      RECcse(1,0,Nnuc) = RECcse(1,0,Nnuc)*2.d0

      csum  = 0.d0
      esum  = 0.d0
      ilast = 0
      DO ie = 1, NDEREC
        ftmp = RECcse(ie,0,Nnuc)
        IF (ftmp.GT.0) then
          csum = csum + ftmp
          esum = esum + ftmp*FLOAT(ie - 1)*DERec/recorr
          ilast = ie
        ENDIF
      ENDDO
      IF (csum.EQ.0) RETURN
      ilast = MIN(ilast + 1,NDEX)

      if (ilast.gt.1)  then
        csum  = csum - 
     &      0.5d0*(RECcse(1,0,Nnuc)+RECcse(ilast,0,Nnuc))
        esum  = esum - RECcse(ilast,0,Nnuc)*
     &          0.5d0*FLOAT(ilast - 1)*DERec/recorr
      endif
C
C     recoil correction added by RCN, 01/2014
C
      WRITE (12,*) ' '
      IF(ENDf(nnuc).EQ.2) then
        WRITE (12,'(A23,A7,A4,I6,A6,F12.5,1x,6H(incl))') 
     &       '  Spectrum of recoils  ',
     &       React, 'ZAP=', IZA(Nnuc), ' mass=', AMAss(Nnuc)
      ELSE
        WRITE (12,'(A23,A7,A4,I6,A6,F12.5            )') 
     &       '  Spectrum of recoils  ',
     &       React, 'ZAP=', IZA(Nnuc), ' mass=', AMAss(Nnuc)
      ENDIF

      WRITE (12,*) ' '
      WRITE (12,'(''    Energy    mb/MeV'')')
      WRITE (12,*) ' '
      DO ie = 1, ilast
        WRITE (12,'(F10.5,E14.5)') FLOAT(ie - 1)*DERec/recorr,
     &                               RECcse(ie,0,Nnuc)*recorr
      ENDDO
      WRITE(12,
     &  '(/2x,''Ave.  E  of recoil spectrum   '',G12.6,'' MeV  for '',
     &  I3,''-'',A2,''-'',I3,A21)') esum/csum,
     &  INT(Z(nnuc)),SYMb(nnuc),INT(A(nnuc)),REAction(nnuc)     

      xsdisc = 0.d0        
      IF (nnuc.EQ.mt849) xsdisc = CSDirlev(1,3)

      IF (ENDf(nnuc).LE.1) THEN
        cmul = csum*DERec/(CSPrd(nnuc)-xsdisc)  ! multiplicity
        qout = qout + cmul*esum/csum            ! multiplicity x <E>
        WRITE(12,
     &  '( 2x,''Ave. <Q> of recoil spectrum   '',G12.6,'' MeV'')') 
     &     cmul*esum/csum
        WRITE(12,'(2x,''Recoil multiplicity          '',G12.6)') cmul
      ENDIF 
      WRITE(12,*)

      WRITE(12,
     &     '( 2x,''Integral of recoil spectrum   '',G12.6,'' mb'' )') 
     &       csum*DERec
        
      if(xsdisc.gt.0.d0  .and. ENDf(nnuc).eq.1) then
        WRITE (12,'(2X,''Cont. popul. before g-cascade '',
     &         G12.6,'' mb'')') CSPrd(nnuc) - xsdisc
        WRITE (12,'(2X,''Disc. popul. before g-cascade '',
     &                G12.6,'' mb'')') xsdisc
      endif 

      IF(ENDf(nnuc).EQ.1) then      
        corr = (CSPrd(Nnuc)-xsdisc)/(csum*DERec)
      ELSE
        corr = CSPrd(Nnuc)/(csum*DERec)
      ENDIF

      WRITE(12,
     &     '( 2x,''Prod. cross sect. (disc+cont) '',G12.6,'' mb'' )') 
     &      CSPrd(Nnuc)
      WRITE(12,
     &     '( 2x,''Ratio Production XS/Recoil XS '',G12.6,'' mb''//)') 
     &      corr 

      IF (ABS(1.d0-corr).GT.0.05D0 .AND. CSPrd(Nnuc).GT.0.001D0) THEN
        WRITE (8,*) 
        WRITE (8,*) ' ******' 
        WRITE (8,*) ' WARNING:  Ein = ', sngl(EIN), ' MeV'
        WRITE (8,*) ' WARNING:  ZAP = ', IZA(Nnuc), ' from ', React
        WRITE (8,*) 
     &       ' WARNING: x-section balance in recoils >5%, ratio=',corr
        WRITE (8,*) ' WARNING: recoil integral     = ',
     &                  sngl(csum*DERec),' mb'
        WRITE (8,*) ' WARNING: Prod. cross section = ',
     &                  sngl(CSPrd(Nnuc)),' mb'
        IF(ENDf(nnuc).EQ.1) then      
          WRITE (8,*) ' WARNING: Cont. cross section = ',
     &                  sngl(CSPrd(Nnuc)-xsdisc),' mb'
          WRITE (8,*) ' WARNING: Discr.cross section = ',
     &                  sngl(xsdisc),' mb'
        ENDIF
      ENDIF
      RETURN
      END

      SUBROUTINE PRINT_BIN_RECOIL(Nnuc,React,qout)
C-----
C-----Prints recoil spectrum of (n,n) or (n,p) residue
C-----
      implicit none 
      INCLUDE 'dimension.h'
      INCLUDE 'global.h'
C
C Dummy arguments
C
      INTEGER Nnuc
      CHARACTER*21 React
      DOUBLE PRECISION qout
C
C Local variables
C
      INTEGER ie, ilast, ipart
      DOUBLE PRECISION csum, corr, xsdisc, ftmp, esum, recorr, cmul

      ipart = 1  !neutron
      IF(IZA(1)-IZA(Nnuc) .EQ. 1001) ipart = 2    !proton

C-----Find last non-zero cross section for printing
      csum  = 0.d0
      esum  = 0.d0
      ilast = 0
      recorr = A(Nnuc)/(A(1)-A(Nnuc))
      DO ie = 1, NDEX
        ftmp = POPcse(0,ipart,ie,INExc(Nnuc))
        IF (ftmp.GT.0) then
          csum = csum + ftmp
          esum = esum + ftmp*FLOAT(ie - 1)*DE/recorr
          ilast = ie
        ENDIF
      ENDDO
      IF (csum.EQ.0 .or. ilast.eq.0 .or. A(Nnuc).eq.A(1)) RETURN
      ilast = MIN(ilast + 1,NDEX)

      if (ilast.gt.1) then
        csum  = csum - 0.5d0*
     &   (POPcse(0,ipart,1,INExc(Nnuc))+
     &    POPcse(0,ipart,ilast,INExc(Nnuc)))
        esum  = esum - POPcse(0,ipart,ilast,INExc(Nnuc))*
     &          0.5d0*FLOAT(ilast - 1)*DE/recorr
      endif
C-----correction factor multiplying cross sections and dividing DE is
C-----simply A(1) since ejectile mass is here always 1 (neutron or proton) 
      WRITE (12,*) ' '

      IF(ENDf(nnuc).EQ.2) then
        WRITE (12,'(A23,A7,A4,I6,A6,F12.5,1x,6H(incl))') 
     &       '  Spectrum of recoils  ',
     &       React, 'ZAP=', IZA(Nnuc), ' mass=', AMAss(Nnuc)
      ELSE
        WRITE (12,'(A23,A7,A4,I6,A6,F12.5            )') 
     &       '  Spectrum of recoils  ',
     &       React, 'ZAP=', IZA(Nnuc), ' mass=', AMAss(Nnuc)
      ENDIF
      WRITE (12,*) ' '
      WRITE (12,'(''    Energy    mb/MeV'')')
      WRITE (12,*) ' '

      DO ie = 1, ilast
        WRITE (12,'(F10.5,E14.5)') FLOAT(ie - 1)*DE/recorr,
     &      POPcse(0,ipart,ie,INExc(Nnuc))*recorr     
      ENDDO

      WRITE(12,
     &  '(/2x,''Ave.  E  of recoil spectrum   '',G12.6,'' MeV  for '',
     &  I3,''-'',A2,''-'',I3,A21)') esum/csum,
     &  INT(Z(nnuc)),SYMb(nnuc),INT(A(nnuc)),REAction(nnuc)     

      xsdisc = 0.d0
      IF (nnuc.EQ.mt91 ) xsdisc = CSDirlev(1,1)
      IF (nnuc.EQ.mt649) xsdisc = CSDirlev(1,2)
 
      cmul = csum*DE/(CSPrd(nnuc)-xsdisc)     ! multiplicity
      qout = qout + cmul*esum/csum            ! multiplicity x <E>
      WRITE(12,
     &  '( 2x,''Ave. <Q> of recoil spectrum   '',G12.6,'' MeV'')') 
     &     cmul*esum/csum
      WRITE(12,'(2x,''Recoil multiplicity (binary) '',G12.6)') cmul
      WRITE(12,*)
      WRITE(12,
     &     '( 2x,''Integral of recoil spectrum   '',G12.6,'' mb'' )') 
     &     csum*DE

      if(xsdisc.gt.0.d0 .and. ENDf(nnuc).eq.1) then
        WRITE (12,'(2X,''Cont. popul. before g-cascade '',
     &         G12.6,'' mb'')') CSPrd(nnuc) - xsdisc
        WRITE (12,'(2X,''Disc. popul. before g-cascade '',
     &                G12.6,'' mb'')') xsdisc
      endif

      IF(ENDf(nnuc).EQ.1) then      
        corr = (CSPrd(Nnuc)-xsdisc)/(csum*DE)
      ELSE
        corr = CSPrd(Nnuc)/(csum*DE)
      ENDIF
      WRITE(12,
     &     '( 2x,''Prod. cross sect. (disc+cont) '',G12.6,'' mb'' )') 
     &      CSPrd(Nnuc) 
      WRITE(12,
     &     '( 2x,''Ratio Production XS/Recoil XS '',G12.6,'' mb''//)') 
     &      corr 

      IF (ABS(1.d0-corr).GT.0.05D0 .AND. CSPrd(Nnuc).GT.0.001D0) THEN
        WRITE (8,*) 
        WRITE (8,*) ' ******' 
        WRITE (8,*) ' WARNING:  Ein = ', sngl(EIN), ' MeV'
        WRITE (8,*) ' WARNING:  ZAP = ', IZA(Nnuc), ' from ', React
        WRITE (8,*) 
     &       ' WARNING: x-section balance in recoils >5%, ratio=',corr
        WRITE (8,*) ' WARNING: recoil integral     = ',
     &                  sngl(csum*DE),' mb'
        WRITE (8,*) ' WARNING: Prod. cross section = ',
     &                  sngl(CSPrd(Nnuc)),' mb'
        IF(ENDf(nnuc).EQ.1) then      
          WRITE (8,*) ' WARNING: Cont. cross section = ',
     &                  sngl(CSPrd(Nnuc)-xsdisc),' mb'
          WRITE (8,*) ' WARNING: Discr.cross section = ',
     &                  sngl(xsdisc),' mb'
        ENDIF

      ENDIF

      RETURN
      END
 
      SUBROUTINE XSECT(Nnuc,M,Xnor,Sumfis,Sumfism,Ke,Ipar,Jcn,Fisxse)
      implicit none
      INCLUDE 'dimension.h'
      INCLUDE 'global.h'
C
C
C COMMON variables
C
      DOUBLE PRECISION TFIso, TGIso, TISo, RFIso, PFIso
      COMMON /FIS_ISO/ TFIso, TGIso, TISo, RFIso, PFIso

      DOUBLE PRECISION TF(NFPARAB), TDIr, TABs, TG2                       
      COMMON /IMAG  / TF, TDIr, TABs, TG2
C
C Dummy arguments
C
      DOUBLE PRECISION Sumfis, Xnor, Fisxse
      INTEGER Ipar, Jcn, Ke, M, Nnuc
      DOUBLE PRECISION Sumfism(NFMOD)
C
C Local variables
C
      INTEGER nejc, nnur, izares, iloc
      DOUBLE PRECISION ares, zres

      Fisxse = 0.d0
C
C-----particles
      DO nejc = 1, NEJcm
         ares = A(nnuc) - AEJc(nejc)
         zres = Z(nnuc) - ZEJc(nejc)
C        residual nuclei must be heavier than alpha
         if(ares.le.4. and. zres.le.2.) cycle
         izares = INT(1000.0*zres + ares)
         CALL WHERE(izares,nnur,iloc)
         if(iloc.eq.1) CYCLE
         CALL ACCUM(Ke,Nnuc,nnur,nejc,Xnor)
         CSEmis(nejc,Nnuc) = CSEmis(nejc,Nnuc) + Xnor*SCRtem(nejc)
      ENDDO
C-----gammas
      CALL ACCUM(Ke,Nnuc,Nnuc,0,Xnor)
      CSEmis(0,Nnuc) = CSEmis(0,Nnuc) + Xnor*SCRtem(0)
      POP(Ke,Jcn,Ipar,Nnuc) = 0.d0

C-----fission
      IF (NINT(FISmod(Nnuc)).EQ.0) THEN
         Fisxse = Sumfis*Xnor
         CSFis  = CSFis + Fisxse
         IF (ENDf(Nnuc).EQ.1 .AND. Fisxse.GT.0.d0 .AND.
     &       POPbin(Ke,Nnuc).GT.0.d0)
     &       CALL EXCLUSIVEC(Ke,0, -1,Nnuc,0,Fisxse)

      ELSE ! Multimodal
         DO M = 1, INT(FISmod(Nnuc)) + 1
            Fisxse = Fisxse + Sumfism(M)*Xnor
            CSFism(M) = CSFism(M) + Sumfism(M)*Xnor
         ENDDO
         CSFis  = CSFis + Fisxse
         IF (ENDf(Nnuc).EQ.1 .AND. Fisxse.GT.0.d0 .AND.
     &       POPbin(Ke,Nnuc).GT.0.d0)
     &       CALL EXCLUSIVEC(Ke,0, -1,Nnuc,0,Fisxse)
      ENDIF
      RETURN
      END
