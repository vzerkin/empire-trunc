Ccc   * $Rev: 4251 $
Ccc   * $Author: bcarlson $
Ccc   * $Date: 2014-11-26 01:43:13 +0100 (Mi, 26 Nov 2014) $

      SUBROUTINE MARENG(Npro,Ntrg,Nnurec,Nejcec)
Ccc
Ccc   ********************************************************************
Ccc   *                                                         class:ppu*
Ccc   *                         M A R E N G                              *
Ccc   *                                                                  *
Ccc   * Calculates initial compound nucleus population after projectile  *
Ccc   * absorption  using transmission coefficients obtained from        *
Ccc   * the optical or the distributed barrier  model.                   *
Ccc   *                                                                  *
Ccc   * input:NPRO    - projectile index (normally 0)                    *
Ccc   *       NTRG    - target index (normally 0)                        *
Ccc   *       Nnurec  - target index among residual nuclei               *
Ccc   *       Nejcec  - projectile index among ejectiles                         *
Ccc   *                                                                  *
Ccc   * output:none                                                      *
Ccc   *                                                                  *
Ccc   ********************************************************************
Ccc
      implicit none
      INCLUDE 'dimension.h'
      INCLUDE 'global.h'
C
C     COMMON variables
C
      DOUBLE PRECISION ELAcs, TOTcs, ABScs, SINl, SINlcc, SINlcont
      COMMON /ECISXS/  ELAcs, TOTcs, ABScs, SINl, SINlcc, SINlcont

      DOUBLE PRECISION ELTl(NDLW),ELTlj(NDLW,3)
      COMMON /ELASTIC/ ELTl,ELTlj
C
C Dummy arguments
C
      INTEGER Npro, Ntrg, Nnurec, Nejcec
C
C Local variables
C
      DOUBLE PRECISION ak2, chsp, cnj, csmax, csvalue, ftmp,
     &    e1tmp, ecms, einlab, el, ener, p1, p2, parcnj, s2a,	jsp,
     &    qdtmp, r2, rp, s0, s1a, smax, smin, selast,	ssabs, ssabsj,
     &    sum, wparg, xmas_npro, xmas_ntrg, S1, stmp,
     &    xssabs, xssabsj
C     fftmp(3) 
C     DOUBLE PRECISION stl(NDLW),stlj(NDLW,3),sel(NDLW)
      DOUBLE PRECISION, ALLOCATABLE :: stl(:),stlj(:,:),sel(:)

      CHARACTER*3 ctldir
      CHARACTER*132 ctmp
      CHARACTER*23 ctmp23
      LOGICAL dodwba, fexist, fexistj, ldbwacalc, ltlj, relcal
      LOGICAL lodd, tljcalc
      DOUBLE PRECISION E1, E2, SIGQD, XM1
      INTEGER i, ichsp, ip, itmp1, j, k, lmax, lmin, maxlw, mul,
     &  nang, itmp2, ncoef1, istat1, istat2, ilev1, mxj,
     &  ipa, il, iloc, l, myalloc, jindex, kmin, kmax
      INTEGER nx,nxx,nxe,lmx,nspec,nspecc,nspecd,ie,nnur,nti,nthi
      DOUBLE PRECISION pops,dex,busigs(5),buspec(NDEX)
      DOUBLE PRECISION sigdep(NDLW),dbfl(NDLW,NDEX),dbf(NDAngecis,NDEX)
      LOGICAL logtmp
      INTEGER iwin, ipipe_move
      CHARACTER*120 rstring
      DATA ctldir/'TL/'/
      DOUBLE PRECISION xj, xjc, jmin, jmax, sxj, trgsp
      DOUBLE PRECISION sjf
      sjf(l,jindex,stmp)= l - 1 + jindex - stmp
      INTEGER PAR
      PAR(i,ipa,l) = (1 - (-1)**i*ipa*(-1)**l)/2

      ltlj = .FALSE.
	tljcalc = .FALSE.
C
C-----Zero qd fraction of photabsorption before it can do any damage
C
      QDFrac = 0.0D0
C
C-----No DWBA by default
C
      ldbwacalc = .FALSE.
C
C-----Locate position of the target among residues
      CALL WHERE(IZA(1) - IZAejc(0),Nnurec,iloc)
C                                                       
C-----Locate position of the projectile among ejectiles
      CALL WHEREJC(IZAejc(0),Nejcec,iloc)
C          
C-----Reduced mass corrected for proper mass values
      xmas_npro = EJMass(Npro) 
      xmas_ntrg = AMAss(Ntrg)

      el = EINl
      ecms = EIN
      mxj   = MAXj(Npro)
      sxj   = SEJc(Npro)
	trgsp = XJLv(LEVtarg,Ntrg)

      S1 = 0.5d0
      IF (AINT(trgsp + sxj) - trgsp - sxj
     &   .EQ.0.0D0) S1 = 1.d0
      ELAcs = 0.D0
      TOTcs = 0.D0
      ABScs = 0.D0
      SINl = 0.D0
      SINlcont = 0.D0
      csmax = 0.d0
      CSFus = 0.d0
      maxlw = 0
C     allocate stl(), stlj(), sel() 
      ALLOCATE(stl(NDLW),sel(NDLW),stlj(NDLW,3),STAT=myalloc)
      IF(myalloc.NE.0) THEN
        WRITE(8,* ) ' ERROR: Insufficient memory for MARENG (fusion.f)'
        WRITE(12,*) ' ERROR: Insufficient memory for MARENG (fusion.f)'
        STOP        ' ERROR: Insufficient memory for MARENG (fusion.f)'
      ENDIF

      stlj = 0.d0
      stl  = 0.d0
      sel  = 0.d0

      WRITE (ctmp23,'(i3.3,i3.3,1h_,i3.3,i3.3,1h_,i9.9)')
     &       INT(ZEJc(Npro)), INT(AEJc(Npro)), INT(Z(Ntrg)),
     &       INT(A(Ntrg)), INT(EINl*1000000)
C
C-----This part prompts for the name of a data file. The INQUIRE
C-----statement then determines whether or not the file exists.
C-----If it does not, the program calculates new transmission coeff.
      INQUIRE (FILE = (ctldir//ctmp23//'.INC'),EXIST = fexist)
      INQUIRE (FILE = (ctldir//ctmp23//'J.INC'),EXIST = fexistj)
      IF (fexist .and. .not.CALctl) THEN
	   if(fexistj) tljcalc = .TRUE. 
C--------Here the old calculated files are read
         OPEN (45 ,FILE = (ctldir//ctmp23//'.INC'),
     &         FORM = 'UNFORMATTED',ERR = 50)
         IF (fexistj) OPEN (451,FILE = (ctldir//ctmp23//'J.INC'),
     &         FORM = 'UNFORMATTED',ERR = 50)
         IF (IOUt.EQ.5) OPEN (46,FILE = ctldir//ctmp23//'_INC.LST')
         IF (fexistj) 
     &     READ (451,END = 50,ERR = 50) lmax, ener, IRElat(Npro,Ntrg)
         READ (45 ,END = 50,ERR = 50) lmax, ener, IRElat(Npro,Ntrg)

         IF (IOUt.EQ.5) WRITE (46,'(A5,I6,E12.6)') 'LMAX:', lmax, ener
          
         IF (ABS(ener - EINl).LT.1.d-6 .AND. FITomp.EQ.0) THEN
            maxlw = lmax
            DO l = 0, maxlw
              READ (45,END = 50,ERR=50) stl(l+1)
              IF(fexistj) READ (451,END = 50,ERR=50) 
     &          (stlj(l+1,jindex), jindex=1,mxj)
              IF (IOUt.EQ.5) THEN
                WRITE (46,'(2x,I3,3(3x,D15.9))') l, stl(l+1)
                IF(fexistj) WRITE (46,'(2x,3x,3(3x,D15.9))') 
     &            (stlj(l+1,jindex), jindex=1,mxj)
              ENDIF
            ENDDO
C           write(*,*) maxlw,stl(maxlw),stl(maxlw+1),' read'

            el = EINl
            relcal = .FALSE.
            IF (IRElat(Npro,Ntrg).GT.0 .OR. RELkin) relcal = .TRUE.
            CALL KINEMA(el,ecms,xmas_npro,xmas_ntrg,ak2,1,relcal)           
C-----------Absorption and elastic cross sections in mb
            ssabs  = 0.d0 
            ssabsj = 0.d0 
            DO l = 0, maxlw
              ssabs   = ssabs   + Stl(l + 1)*DBLE(2*l + 1)
              IF (fexistj) then
                DO jindex = 1, mxj
                  jsp = sjf(l,jindex,sxj) 
                  ssabsj = ssabsj + DBLE(2*jsp+1)*Stlj(l + 1,jindex)
                ENDDO 
			ENDIF   
            ENDDO
            xssabs  = 10.d0*PI/ak2*ssabs
            xssabsj = 10.d0*PI/ak2*ssabsj/DBLE(2*sxj+1)

            IF (fexistj) READ (451,END = 50,ERR=50) 
     &        ELAcs, TOTcs, ABScs, SINl, SINlcc, CSFus
            READ (45 ,END = 50,ERR=50) 
     &        ELAcs, TOTcs, ABScs, SINl, SINlcc, CSFus
             SINlcont = max(ABScs - (SINl + SINlcc + CSFus),0.d0)
            IF (IOUt.EQ.5) THEN
              WRITE (46,*) 'EL,TOT,ABS,INEL,CC,CSFus,SumTl,SumTlj'
              WRITE (46,'(1x,8(D15.9,1x))')
     &          ELAcs, TOTcs, ABScs, SINl, SINlcc, CSFus, xssabs,xssabsj
              IF(FIRST_ein) then
	          WRITE(8,*)
                WRITE (8,*) 
     &            'EL,TOT,ABS,INEL,CC,INELcont,CSFus,SumTl,SumTlj'
                WRITE (8,'(1x,9(D15.9,1x))') ELAcs, TOTcs, ABScs, 
     &            SINl, SINlcc, SINlcont,CSFus, xssabs,xssabsj
	          WRITE(8,*)
              ENDIF 
            ENDIF
            READ (45,END = 40, ERR=40) L
            IF(L.EQ.123456) THEN
              IF (IOUt.EQ.5) WRITE (46,*) L
              DO l = 0, maxlw
                READ (45,END = 40, ERR=40) ftmp
                if(l+1.le.NDLW) THEN 
                  sel(l + 1) = ftmp
                  IF (IOUt.EQ.5) 
     &              WRITE (46,'(2x,I3,3x,D15.9)') l, ftmp
                endif
              ENDDO
            ENDIF
   40       CLOSE (45 )
            IF (fexistj) CLOSE (451)
      
            NLW = min(NDLW,maxlw+1)

            IF (IOUt.EQ.5) CLOSE (46)
            IF (IOUt.GT.1) THEN
              WRITE (8,*)
              WRITE (8,*)
     &' Transmission coefficients Tl for incident channel read from : '
              WRITE (8,*) ' ', ctldir//ctmp23//'.INC'
              IF (fexistj) WRITE (8,*)
     &' Transmission coefficients Tlj for incident channel read from: '
              IF (fexistj) WRITE (8,*) ' ', ctldir//ctmp23//'J.INC'
            ENDIF
            WRITE(8,*)
            WRITE(8,*) ' Maximum CN spin is ', NLW
            WRITE(8,*) ' Spin dimension  is ', NDLW
C           NLW = NDLW
            GOTO 300
         ENDIF
C
C--------If (energy read from file do not coincide
C--------this nucleus should be recalculated (after the ENDIF)
C
   50    CLOSE (45,STATUS = 'DELETE')
         IF (FITomp.EQ.0) THEN
           WRITE (8,*) ' WARNING: ENERGY MISMATCH:  Elab =', 
     &       sngl(EINl),' REQUESTED ENERGY=', SNGL(ener)
           WRITE (8,*) ' WARNING: FILE WITH TRANSM. COEFF.',
     &                 ' FOR INCID.CHANNEL HAS BEEN DELETED'
         ENDIF
         IF (IOUt.EQ.5) CLOSE (46,STATUS = 'DELETE')

      ENDIF
C-----Calculation of fusion cross section for photon induced reactions
      IF (NINT(AEJc(Npro)).EQ.0) THEN
         IF (SDRead) THEN
C-----------Reading of spin distribution from file SDFILE
C
C           If you have file "SDFILE" -> it is possible to use
C           input spin distribution
C           Format of the file:
C           rows with sequentialy  cnJ, ParcnJ, csvalue
C           where  cnJ     - spin of nucleus,
C                  ParcnJ  - parity
C                  csvalue - cross-section
C
C           limits: cnJ=n*0.5, where n=0,1,2...
C                   A of c.n. /2 = integer --> cnJ=n*1, where n=0,1,2...
C                   A of c.n. /2 = integer+1/2 --> cnJ=n*1/2, where n=0,1,2...
C                   ParcnJ=-1 or 1
C                   csvalue = value in mb
C           Reading no more than 2*NDLW rows
            WRITE (8,*)
     &' Spin distribution of fusion cross section read from SDREAD file'
            WRITE (8,*)
     &          ' (all previous instructions concerning fusion ignored)'
            DO i = 1, 2*NDLW
               READ (43,*,END = 60,ERR = 60) cnj, parcnj, csvalue
C--------------Spin of c.n. cnJ=j-S1 => j=cnJ+S1
               IF (2*cnj - DINT(2*cnj).NE.0.d0) THEN
                WRITE(8,*)'ERROR: cnJ!=n*1/2, n=0,+-1... in SDREAD file'
                STOP 'ERROR: cnJ!=n*1/2, n=0,+-1... in SDREAD file'
               ENDIF
               j = IDNINT(cnj + S1)
               IF (NINT(parcnj).EQ.1) ip = 1
               IF (NINT(parcnj).EQ. -1) ip = 2
               IF (NINT(parcnj).NE.1 .AND. NINT(parcnj).NE. -1) THEN
                 WRITE(8,*) 'ERROR: ParcnJ != +-1 in SDREAD file'
                 STOP 'ERROR: ParcnJ != +-1 in SDREAD file'
               ENDIF
               POP(NEX(1),j,ip,1) = csvalue
               CSFus = CSFus + POP(NEX(1),j,ip,1)
               csmax = DMAX1(POP(NEX(1),j,ip,1),csmax)
            ENDDO
   60       CONTINUE
            ABScs=CSFus
            NLW = min(NDLW,j) 
C-----------END of spin distribution from file SDFILE
         ELSE
            JSTab(1) = NDLW !stability limit not a problem for photoreactions
            IF (EIN.LE.ELV(NLV(Ntrg),Ntrg)) THEN
               WRITE (8,*) ' WARNING: '
               WRITE (8,*) ' WARNING: ECN=', EIN, ' Elev=',
     &                     ELV(NLV(Ntrg),Ntrg)
               WRITE (8,*)
     &                 ' WARNING: CN excitation energy below continuum'
               WRITE (8,*)
     &                 ' WARNING: cut-off. zero reaction cross section'
               WRITE (8,*) ' WARNING: will result'
            ENDIF
C-----------E1
            IF (IGE1.NE.0) THEN
C-----------factor 10 near HHBarc from fm**2-->mb
               e1tmp = 10*HHBarc**2*PI*E1(Ntrg,EINl,0.D0,0.D0)
     &                 /(2*EINl**2)
               qdtmp = SIGQD(Z(Ntrg),A(Ntrg),EINl,LQDfac)
               e1tmp = (e1tmp + qdtmp/3.0D0)/(2*trgsp + 1.d0)
C--------------do loop over parity
               DO ip = 1, 2
C-----------------Quasideuteron contribution QDTmp by Carlson
                  wparg = PAR(ip,LVP(LEVtarg,Ntrg),1)*e1tmp
C-----------------do loop over compound nucleus spin
                  DO j = 1, NDLW
C--------------------Spin of c.n. J=j-S1
                     IF (ABS(j - S1 - trgsp).LE.1.d0 .AND.
     &                   (j - S1 + trgsp).GE.1.0)
     &                   POP(NEX(1),j,ip,1) = POP(NEX(1),j,ip,1)
     &                   + (FLOAT(2*j + 1) - 2.0*S1)*wparg
                  ENDDO
               ENDDO
            ENDIF
C-----------end of E1
C
C-----------M1
            IF (IGM1.NE.0) THEN
C--------------factor 10 near HHBarc from fm**2-->mb
               e1tmp = 10*HHBarc**2*PI*XM1(EINl)/(2*EINl**2)
     &                 /(2*trgsp + 1.d0)
C--------------do loop over parity
               DO ip = 1, 2
                  wparg = PAR(ip,LVP(LEVtarg,Ntrg),2)*e1tmp
C-----------------do loop over compound nucleus spin
                  DO j = 1, NDLW
C--------------------Spin of c.n. J=j-S1
                     IF (ABS(j - S1 - trgsp).LE.1.0 .AND.
     &                   (j - S1 + trgsp).GE.1.0)
     &                   POP(NEX(1),j,ip,1) = POP(NEX(1),j,ip,1)
     &                   + (FLOAT(2*j + 1) - 2.0*S1)*wparg
                  ENDDO
               ENDDO
            ENDIF
C-----------end of M1
C
C-----------E2
            IF (IGE2.NE.0) THEN
C-----------factor 10 near HHBarc from fm**2-->mb
               e1tmp = 10*HHBarc**2*PI*E2(EINl)/(2*EINl**2)
     &                 /(2*trgsp + 1.d0)
C--------------do loop over parity
               DO ip = 1, 2
                  wparg = PAR(ip,LVP(LEVtarg,Ntrg),2)*e1tmp
C-----------------do loop over compound nucleus spin
                  DO j = 1, NDLW
C-----------------Spin of c.n. J=j-S1
C-----------------factor 10 near HHBarc from fm**2-->mb
                     IF (ABS(j - S1 - trgsp).LE.2.0 .AND.
     &                   (j - S1 + trgsp).GE.2.0)
     &                   POP(NEX(1),j,ip,1) = POP(NEX(1),j,ip,1)
     &                   + (FLOAT(2*j + 1) - 2.0*S1)*wparg
                  ENDDO
               ENDDO
            ENDIF
C-----------end of E2
            csmax = 0.d0
            maxlw = 0
            DO ip = 1, 2
               DO j = 1, NDLW
                  e1tmp = POP(NEX(1),j,ip,1)
                  if (e1tmp.lt.1.d-15) cycle 
                  CSFus = CSFus + e1tmp
                  csmax = DMAX1(e1tmp,csmax)
                  if(j.gt.maxlw) maxlw = j
               ENDDO
            ENDDO
            IF (IGE1.NE.0 .AND. CSFus.GT.0.D0) QDFrac = qdtmp/CSFus
 
            ABScs = CSFus
            NLW   = maxlw
  
         ENDIF
C--------END of calculation of fusion cross section
C--------for photon induced reactions
C
C        Total cross section is set to absorption cross section
C        for photon induced reactions (to process them in EMPEND)
C
         TOTcs = CSFus
         WRITE(8,*)
         WRITE(8,*) ' Photoabsorption cross section =',CSFus
         WRITE(8,*) ' Maximum CN spin is ', NLW
         WRITE(8,*) ' Spin dimension  is ', NDLW
         WRITE(8,*)
C
         GOTO 400
      ENDIF
      IF (FUSread) THEN
C--------if FUSREAD true read l distribution of fusion cross section
C--------and calculate transmission coefficients
         el = EINl
         CALL KINEMA(el,ecms,xmas_npro,xmas_ntrg,ak2,1,RELkin)
         coef=1.d0
         IF (INT(AEJc(0)).GT.0) coef = 10.*PI/ak2/
     &      (2*trgsp + 1.d0)/(2*sxj + 1.d0)

         CSFus = 0.d0
         DO il = 1, NDLW
            READ (11,*,END = 150) csvalue
            stl(il) = csvalue
            CSFus = CSFus + csvalue*(2*(il-1)+1)*coef
            IF (stl(il).GT.1.0D0) THEN
               WRITE (8,
     &'('' ERROR: INPUT FUSION TRANSMISSION > 1'', '' FOR l='',I3)') 
     & il - 1
               WRITE (8,*) ' FATAL: EXECUTION STOPPED!!!'
               STOP
            ENDIF
         ENDDO

  150    NLW = il - 1
         maxlw = min(NLW,NDLW-1)
         ABScs = CSFus
         WRITE (8,*)
     &  ' Spin distribution of fusion cross section read from the file '
         WRITE (8,*)
     &          ' (all previous instructions concerning fusion ignored)'
         WRITE (8,*) ' Maximum angular momentum :',maxlw
         WRITE (8,*) ' Fusion cross section      :',CSFus
         WRITE (8,*) 
C
C--------calculation of o.m. transmission coefficients for absorption
C
      ELSEIF (KTRlom(Npro,Ntrg).GT.0) THEN
         einlab = -EINl
         IWArn = 0
         ldbwacalc = .FALSE.

         CCCalc = .FALSE.
         lodd = .false.
         IF( (mod(nint(Z(Ntrg)),2).ne.0 .or. 
     >        mod(nint(A(Ntrg)-Z(Ntrg)),2).ne.0) .and.
     >        mod(nint(A(Ntrg)),2).ne.0 ) lodd = .true.  
         dodwba = .FALSE.
         DO l = 1, NDCOLLEV
            IF (ICOllev(l).LT.LEVcc) CYCLE ! Skipping coupled levels
            dodwba = .TRUE.
         ENDDO
         IF (DIRect.EQ.3) dodwba = .TRUE.
         IF (dodwba .AND. DIRect.GT.0 ) THEN
            IF (DIRect.EQ.1 .OR. DIRect.EQ.3) THEN
C--------------Saving KTRlom(0,0)
               itmp1 = KTRlom(0,0)
               KTRlom(0,0) = KTRompcc
               CCCalc = .TRUE.
            ENDIF
            WRITE (8,*) 
            IF (CCCalc .and. DIRECT.NE.3) THEN
               WRITE (8,*) ' CC   calculation  for inelastic scattering'
               WRITE (8,*) '    on   coupled coll. levels'
            ENDIF
C-----------DWBA calculation. All collective levels considered
            IF (DIRect.EQ.3) THEN
               WRITE (8,*) ' DWBA calculations for inelastic scattering'
            ELSE
               WRITE (8,*) ' DWBA calculations for inelastic scattering'
               WRITE (8,*) '    on uncoupled coll. levels and continuum'
            ENDIF
            WRITE (8,*) 
C           
C           saving the input value of the key CN_isotropic
            logtmp = CN_isotropic
C           all OMP calculations calculate only the direct component (no CN)
            CN_isotropic = .TRUE.     
                                                   !  
            CALL ECIS_CCVIB(Npro,Ntrg,einlab,.TRUE.,1,.FALSE.)
C           restoring the input value of the key CN_isotropic
            CN_isotropic = logtmp

            IF (DIRect.NE.3) THEN
               CALL PROCESS_ECIS('dwba',4,4,ICAlangs)
            ELSE
               CALL PROCESS_ECIS('INCIDENT',8,4,ICAlangs)
               CALL ECIS2EMPIRE_TL_TRG(
     &           Npro,Ntrg,maxlw,stl,stlj,sel,.TRUE.)
                            ! TLs are obtained here for DIRECT=3
               ltlj = .TRUE.
               tljcalc = .TRUE. 

               WRITE (8,*) 
               WRITE (8,*) ' SOMP transmission coefficients used for ',
     &                     'fusion determination'
            ENDIF
            IF (DIRect.EQ.1 .OR. DIRect.EQ.3) THEN
C--------------Restoring KTRlom(0,0)
               KTRlom(0,0) = itmp1
               CCCalc = .FALSE.
            ENDIF
            ldbwacalc = .TRUE.
         ENDIF

C
C--------In EMPIRE code the options DIRECT=1 and DIRECT=2 produces exactly the
C--------same array of transmission coefficients for the incident channel
C--------calculated by the CC OMP. The differences between DIRECT 1/2 options
C--------are in the calculations of the outgoing channel (TRANSINP() routine).
C--------DIRECT 2 option uses CC method to produce outgoing TLs
C--------for the inelastic channel. DIRECT 1 option assumes SOMP
C--------with only one level (the GS) to calculate the inelastic TLs.
C
         IF ((DIRect.EQ.1 .OR. DIRect.EQ.2) .AND. AEJc(Npro).LE.1) THEN
            WRITE (8,*) 
            WRITE (8,*) ' CC transmission coefficients used for ',
     &                  'fusion determination'

            einlab = -EINl
            IF (DIRect.EQ.1) THEN
C--------------Saving KTRlom(0,0)
               itmp1 = KTRlom(0,0)
               KTRlom(0,0) = KTRompcc
               CCCalc = .TRUE.
            ENDIF

C-----------Transmission coefficient matrix for incident channel
C-----------is calculated by CC method.
C           saving the input value of the key CN_isotropic
            logtmp = CN_isotropic
C           all OMP calculations calculate only the direct component (no CN)
            CN_isotropic = .TRUE.     

            IF (SOFt) THEN
C-------------EXACT SOFT ROTOR MODEL CC calc. by OPTMAN (only coupled levels)
              CALL OPTMAN_CCSOFTROT(Npro,Ntrg,einlab,.FALSE.) 
              IF (ldbwacalc) THEN
                CALL PROCESS_ECIS('ccm',3,4,ICAlangs)
              ELSE
                CALL PROCESS_ECIS('INCIDENT',8,4,ICAlangs)
                CALL ECIS2EMPIRE_TL_TRG(
     &            Npro,Ntrg,maxlw,stl,stlj,sel,.TRUE.)
                ltlj = .TRUE.
                tljcalc = .TRUE. 
              ENDIF

            ELSE
              IF (DEFormed) THEN
C---------------EXACT ROTATIONAL MODEL CC calc. (only coupled levels)
C               including CN calculation
                CALL ECIS_CCVIBROT(Npro,Ntrg,einlab,.FALSE.)
                IF (ldbwacalc) THEN
                  CALL PROCESS_ECIS('ccm',3,4,ICAlangs)
                ELSE
                  CALL PROCESS_ECIS('INCIDENT',8,4,ICAlangs)
                  CALL ECIS2EMPIRE_TL_TRG(
     >              Npro,Ntrg,maxlw,stl,stlj,sel,.FALSE.)
                  ltlj = .TRUE.
                  tljcalc = .TRUE. 
                ENDIF
              ELSE
C---------------EXACT VIBRATIONAL MODEL CC calc. (only coupled levels)
                CALL ECIS_CCVIB(Npro,Ntrg,einlab,.FALSE., -1,.FALSE.)
                IF (ldbwacalc) THEN
                  CALL PROCESS_ECIS('ccm',3,4,ICAlangs)
                ELSE
                  CALL PROCESS_ECIS('INCIDENT',8,4,ICAlangs)
                  CALL ECIS2EMPIRE_TL_TRG(
     &              Npro,Ntrg,maxlw,stl,stlj,sel,.TRUE.)
                  ltlj = .TRUE.
                  tljcalc = .TRUE. 
                ENDIF
              ENDIF

            ENDIF
C           restoring the input value of the key CN_isotropic
            CN_isotropic = logtmp

            IF (DIRect.EQ.1) THEN
C--------------Restoring KTRlom(0,0)
               KTRlom(0,0) = itmp1
               CCCalc = .FALSE.
            ENDIF
C
C           DYNAM=.TRUE. or SOFT=.TRUE. means OPTMAN is used
C                       
            IF (ldbwacalc) THEN
               ltlj = .TRUE.
C
C--------------Joining DWBA and CCM files
C--------------Total, elastic and reaction cross section is from CCM
C
               iwin=ipipe_move('ccm.CS','INCIDENT.CS')
               iwin=ipipe_move('ccm.TLJ','INCIDENT.TLJ')
C
C              Joining dwba.LEG and ccm.LEG
C
C--------------Legendre expansion (INCIDENT.LEG)
               OPEN (47,FILE = 'INCIDENT.LEG')
               OPEN (45,FILE = 'dwba.LEG',STATUS = 'OLD',ERR = 180)
               READ (45,'(A80)',ERR = 180, END = 180) rstring ! first line is taken from DWBA
               OPEN (46,FILE = 'ccm.LEG' ,STATUS = 'OLD',ERR = 180)
               READ (46,*,ERR = 174, END = 174) 
               WRITE (47,'(A80)') rstring

  174          DO i = 1, ND_nlv ! loop over coupled levels
C---------------------------------------------------------------------
C                Reading dwba.LEG (DWBA calculations)
C 
                 READ (45,'(i5,1x,i4)',END = 180,ERR = 180) 
     &                 ilev1, ncoef1

                 if(ncoef1.GT.0) THEN
                   if(ICOllev(i).GE.LEVcc) 
     &             WRITE (47,'(i5,1x,i4,3x,A24)') 
     &               ilev1, ncoef1, 'DWBA DIR. LEG. EXPANSION'
                 else
                   WRITE (47,'(i5,1x,i4,3x,A24)') 
     &               ilev1, ncoef1, 'CLOSED CHANNEL          '                                          

                   CYCLE  ! skipping all the rest if channel closed

                 endif
C                write(*,*) 'DWBA ilev=',ilev1,' #Ls=', ncoef1
C
C                DWBA DIR PLs
                 DO j= 1 , ncoef1 ! skipping DWBA DIR expansion
                   READ (45,'(A80)',ERR = 180, END = 180) rstring 
                   if(ICOllev(i).GE.LEVcc) WRITE (47,'(A80)') rstring
                 ENDDO

                 IF(ICOllev(i).GE.LEVcc) CYCLE 

C---------------------------------------------------------------------
C                Reading cc.LEG (DWBA calculations)
C 
                 READ  (46,'(i5,1x,i4)',END = 177,ERR = 177) 
     &                ilev1, ncoef1
                 if(ncoef1.GT.0) THEN
                   WRITE (47,'(i5,1x,i4,3x,A24)') 
     &               ilev1, ncoef1, 'C.C. DIR. LEG. EXPANSION'
                 else
                   WRITE (47,'(i5,1x,i4,3x,A24)') 
     &               ilev1, ncoef1, 'CLOSED CHANNEL          '                                          

                   CYCLE  ! skipping all the rest if channel closed
                 endif

C                write(*,*) 'CC ilev=',ilev1,' #Ls=', ncoef1
C
C                CC DIR PLs
                 DO j= 1 , ncoef1 ! skipping CC DIR expansion
                   READ (46,'(A80)',ERR = 177, END = 177) rstring ! first line is taken from DWBA
                   WRITE (47,'(A80)') rstring
                 ENDDO
                    
  177          ENDDO ! end of the loop over collective levels

  180          CLOSE (45, STATUS = 'DELETE')
               CLOSE (46, STATUS = 'DELETE')
               CLOSE (47) 
C
C--------------Inelastic cross section (INCIDENT.ICS)
               OPEN (45,FILE = 'dwba.ICS',STATUS = 'OLD',ERR = 220)
               OPEN (46,FILE = 'ccm.ICS'   ,STATUS = 'OLD',ERR = 220)
   
               OPEN (47,FILE = 'INCIDENT.ICS',STATUS = 'UNKNOWN')
               READ (45,'(A80)',END = 220,ERR = 220) rstring
               READ (46,'(A80)',END = 210,ERR = 210) ! first line is taken from dwba
  210          WRITE (47,'(A80)') rstring
               DO i = 2, ND_nlv
                  READ (45,'(A80)',END = 220,ERR = 220) rstring
                  READ (46,'(A80)',END = 215,ERR = 215) rstring
  215             WRITE (47,'(A80)') rstring
               ENDDO
  220          CLOSE (45,STATUS = 'DELETE')
               CLOSE (46,STATUS = 'DELETE')
               CLOSE (47)
C--------------Angular distribution (INCIDENT.ANG)
               OPEN (45,FILE = 'dwba.ANG',STATUS = 'OLD',ERR = 240)
               READ (45,'(A80)',ERR = 240, END = 240) rstring
               OPEN (46,FILE = 'ccm.ANG' ,STATUS = 'OLD',ERR = 240)
               READ (46,'(A80)',ERR = 230, END = 230) ! first line is taken from dwba
  230          OPEN (47,FILE = 'INCIDENT.ANG',STATUS = 'UNKNOWN')

               WRITE (47,'(A80)') rstring
               DO i = 1, ND_nlv
C-----------------checking the correspondence of the excited states
                  READ (45,'(i5,6x,i4,i5)',END = 240,ERR = 240) 
     &                 istat1, itmp2, nang
                  READ (46,'(i5,6x,i4)',END = 235,ERR = 235) 
     &                 istat2
C-----------------checking the correspondence of the excited states for even-even targets
                  IF ( .not.lodd .AND. istat1.NE.istat2 ) THEN   
                    WRITE (8,*)
     &            ' WARNING: DWBA and CCM state order do not coincide'
                     STOP
     &            ' WARNING: DWBA and CCM state order do not coincide'
                  ENDIF
                  BACKSPACE (46)
                  READ (46,'(A80)',ERR = 235, END = 235) rstring
                  GOTO 2351 
  235             BACKSPACE (45)
                  READ (45,'(A80)',ERR = 240, END = 240) rstring
 2351             WRITE (47,'(A80)') rstring
                  DO j = 1, itmp2*nang ! ecis06
                     READ (45,'(A80)',ERR = 240,END = 240) rstring
                     READ (46,'(A80)',ERR = 236,END = 236) rstring
  236                WRITE (47,'(A80)') rstring
                  ENDDO
               ENDDO
  240          CLOSE (45, STATUS = 'DELETE')
               CLOSE (46, STATUS = 'DELETE')
               CLOSE (47)
C--------------Experimental angular distribution (incident.ang)
               IF( ICAlangs.GT.0) THEN   ! To be updated for ecis06 
                 OPEN (45,FILE = 'dwba.EXP',STATUS = 'OLD',ERR = 260)
                 READ (45,'(A80)',END = 260) rstring
                 OPEN (46,FILE = 'ccm.EXP',STATUS = 'OLD',ERR = 260)
                 READ (46,'(A80)',END = 250) ! first line is taken from dwba
  250            OPEN (47,FILE = 'INCIDENT.EXP',STATUS = 'UNKNOWN')
                 WRITE (47,'(A80)') rstring
                 DO i = 1, ICAlangs
                  READ (45,'(i5,5x,i5)',END = 260) istat1, nang
                  READ (46,'(i5)',END = 255) istat2
C-----------------checking the correspondence of the excited states
                  IF (istat1.NE.istat2) THEN
                     WRITE (8,*)
     &        ' WARNING: Exptl DWBA and CCM state order do not coincide'
                     STOP
     &        ' WARNING: Exptl DWBA and CCM state order do not coincide'
                  ENDIF
  255             BACKSPACE (45)
                  READ (45,'(A80)',END = 260) rstring
                  WRITE (47,'(A80)') rstring
                  DO j = 1, nang
                     READ (45,'(A80)',END = 260) rstring
                     READ (46,'(A80)',END = 256) rstring
  256                WRITE (47,'(A80)') rstring
                  ENDDO
                 ENDDO
                ENDIF
  260          CLOSE (45, STATUS = 'DELETE')
               CLOSE (46, STATUS = 'DELETE')
               CLOSE (47)
               IF (DEFormed) THEN                                                                          
                 CALL ECIS2EMPIRE_TL_TRG(
     &             Npro,Ntrg,maxlw,stl,stlj,sel,.FALSE.)
               ELSE
                 CALL ECIS2EMPIRE_TL_TRG(
     &             Npro,Ntrg,maxlw,stl,stlj,sel,.TRUE.)
               ENDIF
               ltlj = .TRUE.
               tljcalc = .TRUE. 

            ENDIF  ! END of LDWBA (DWBA and CCM joining process)
C           write(*,*) maxlw,stl(maxlw),stl(maxlw+1),' def calc'
  
         ENDIF  ! END of DIRECT=1/2 block

         IF (.NOT.ltlj) THEN
C-----------Transmission coefficient matrix for incident channel
C-----------is calculated like in SOMP i.e.
C-----------SCAT2 like calculation (one state, usually gs, alone)

C           saving the input value of the key CN_isotropic
            logtmp = CN_isotropic
C           all OMP calculations calculate only the direct component (no CN)
            CN_isotropic = .TRUE.     

            CALL ECIS_CCVIB(Npro,Ntrg,einlab,.TRUE.,0,.FALSE.)
C           restoring the input value of the key CN_isotropic
            CN_isotropic = logtmp

            CALL PROCESS_ECIS('INCIDENT',8,3,ICAlangs)
            WRITE (8,*) 
            WRITE (8,*) ' SOMP transmission coefficients used for ',
     &                  'fusion determination'
            CALL ECIS2EMPIRE_TL_TRG(
     &        Npro,Ntrg,maxlw,stl,stlj,sel,.TRUE.)
            ltlj = .TRUE.
            tljcalc = .TRUE. 

C           write(*,*) maxlw,stl(maxlw),stl(maxlw+1),' sph calc'

         ENDIF

         IF (maxlw.GT.NDLW) THEN
            WRITE (8,*)
     &            ' ERROR: INSUFFICIENT NUMBER OF PARTIAL WAVES ALLOWED'
            WRITE (8,*) ' ERROR: INCREASE NDLW IN dimension.h UP TO',
     &                  maxlw + 1
            WRITE (8,*) ' ERROR: AND RECOMPILE THE CODE'
            STOP ' FATAL: INSUFFICIENT NUMBER OF PARTIAL WAVES ALLOWED'
         ENDIF
         WRITE(8,*) 
         WRITE(8,*) ' Maximum CN spin is ', NLW 
         WRITE(8,*) ' Spin dimension  is ', NDLW
C        NLW = NDLW
C--------IWARN=0 - 'NO Warnings'
C--------IWARN=1 - 'A out of the recommended range '
C--------IWARN=2 - 'Z out of the recommended range '
C--------IWARN=3 - 'Energy requested lower than recommended for this potential'
C--------IWARN=4 - 'Energy requested higher than recommended for this potential'
         IF ((IWArn.EQ.1 .or. IWArn.EQ.2) .AND. FIRst_ein
     &       .AND. IOUt.GE.5) WRITE (8,*) ' WARNING: ',KTRlom(0,0),
     &   ' OMP not recommended for target Z,A=', Z(Ntrg),'-',(Ntrg)
         IF ((IWArn.EQ.3 .OR. IWArn.EQ.4) .AND. IOUt.GE.5) WRITE (8,*)
     &      ' WARNING: ',KTRlom(0,0),' OMP not recommended for E=', EINl
         IWArn = 0
      ELSEIF (KTRlom(Npro,Ntrg).EQ.0) THEN
C--------calculation of h.i. transmission coefficients for fusion
         maxlw = NDLW-1
         CALL HITL(stl)
	   
         ltlj    = .TRUE.
         tljcalc = .FALSE.
         if(NLW.GT.0) maxlw = min(NDLW-1,NLW)
         WRITE(8,*) 
         WRITE(8,*) ' Maximum CN spin is ', NLW 
	   WRITE(8,*) ' Spin dimension  is ', NDLW
         NLW = maxlw+1
C--------channel spin min and max
         el = EINl
         CALL KINEMA(el,ecms,xmas_npro,xmas_ntrg,ak2,1,RELkin)
         coef = 1.d0
         IF (INT(AEJc(0)).GT.0) coef = 10.d0*PI/ak2/
     &      (2*trgsp + 1.d0)/(2*sxj + 1.d0)
	                   
         smin = ABS(sxj - trgsp)
         smax = sxj + trgsp
         mul = smax - smin + 1.0001
         CSFus = 0.d0
         csmax = 0.d0
         DO ip = 1, 2     ! over parity
          DO j = 1, NLW !over compound nucleus spin
            sum = 0.d0
            DO ichsp = 1, mul
               chsp = smin + FLOAT(ichsp - 1)
               lmin = ABS(j - chsp - S1) + 0.0001
               lmax = j + chsp - S1 + 0.0001
               lmin = lmin + 1
               lmax = lmax + 1
               lmax = MIN0(NDLW,lmax)
               lmax = MIN0(maxlw,lmax)
               DO k = lmin, lmax
                  sum = sum + PAR(ip,LVP(LEVtarg,Ntrg),k - 1)*stl(k)
               ENDDO
            ENDDO
            POP(NEX(1),j,ip,1) = coef*sum*(FLOAT(2*j + 1) - 2.0*S1)
     &                           *FUSred
            CSFus = CSFus + POP(NEX(1),j,ip,1)
            csmax = DMAX1(POP(NEX(1),j,ip,1),csmax)
          ENDDO
         ENDDO

         ABScs = CSFus
      ENDIF ! END of FUSREAD block
C
C-----Moving incident channel results to TL/ directory
C
      IF (KTRlom(Npro,Ntrg).GT.0) THEN
         ctmp = ctldir//ctmp23//'.CS'
         iwin = ipipe_move('INCIDENT.CS',ctmp)
         IF (DIRect.GT.0) THEN
           ctmp = ctldir//ctmp23//'.ICS'
           iwin = ipipe_move('INCIDENT.ICS',ctmp)
         ENDIF
         IF (ICAlangs.GT.0) THEN
           ctmp = ctldir//ctmp23//'.EXP'
           iwin = ipipe_move('INCIDENT.EXP',ctmp)
         ENDIF
         ctmp = ctldir//ctmp23//'.ANG'
         iwin = ipipe_move('INCIDENT.ANG',ctmp)
         ctmp = ctldir//ctmp23//'.LEG'
         iwin = ipipe_move('INCIDENT.LEG',ctmp)
         ctmp = ctldir//ctmp23//'.TLJ'
         iwin = ipipe_move('INCIDENT.TLJ',ctmp)
      ENDIF
C
C-----Save TLs, SINl
C
C-----Storing transmission coefficients for the incident channel
C     write(*,*) ABScs,CSFus  
      IF (IOUt.EQ.5) THEN
         OPEN (46,FILE = ctldir//ctmp23//'_INC.LST')
         WRITE (46,'(A5,I6,E12.6)') 'LMAX:', maxlw, EINl
	   if(tljcalc) then
          DO l = 0, maxlw
            WRITE (46,'(2x,I3,3(3x,D15.8))') l, stl(l + 1)
            WRITE (46,'(2x,3x,3(3x,D15.8))') 
     &        (stlj(l + 1,jindex), jindex=1,mxj)
          ENDDO
         endif
         el = EINl
         relcal = .FALSE.
         IF (IRElat(Npro,Ntrg).GT.0 .OR. RELkin) relcal = .TRUE.
         CALL KINEMA(el,ecms,xmas_npro,xmas_ntrg,ak2,1,relcal)
C--------Absorption and elastic cross sections in mb
         ssabs  = 0.d0 
         ssabsj = 0.d0 
         DO l = 0, maxlw
           ssabs   = ssabs   + Stl(l + 1)*DBLE(2*l + 1)
           if (tljcalc) then 
             DO jindex = 1, mxj 
               jsp = sjf(l,jindex,sxj) 
               ssabsj = ssabsj + DBLE(2*jsp+1)*Stlj(l + 1,jindex)
             ENDDO 
           endif
         ENDDO
         xssabs  = 10.d0*PI/ak2*ssabs
         xssabsj = 10.d0*PI/ak2*ssabsj/(2*sxj+1.d0)
         WRITE (46,*) 'EL,TOT,ABS,INEL,CC,CSFus,SumTl,SumTlj'
         WRITE (46,'(1x,8(D15.9,1x))') 
     &     ELAcs, TOTcs, ABScs, SINl, SINLcc, CSFus, xssabs, xssabsj
         IF(FIRST_ein) then
           WRITE (8,*)
           WRITE (8,*) 'EL,TOT,ABS,INEL,CC,CSFus,SumTl,SumTlj'
           WRITE (8,'(1x,8(D15.9,1x))') 
     &     ELAcs, TOTcs, ABScs, SINl, SINLcc, CSFus, xssabs, xssabsj
           WRITE (8,*)
         ENDIF 

	   IF (ELAcs.gt.0) THEN 
           WRITE (46,'(1x,I6)') 123456
           DO l = 0, maxlw
             WRITE (46,'(2x,I3,3x,D15.9)') l, sel(l + 1)
           ENDDO
         ENDIF
         CLOSE (46)
      ENDIF

C     write(*,*) maxlw,stl(maxlw),stl(maxlw+1),' stor ',tljcalc

      if (tljcalc) 
     &  OPEN (451,FILE = (ctldir//ctmp23//'J.INC'),FORM = 'UNFORMATTED')
      OPEN (45,FILE = (ctldir//ctmp23//'.INC'),FORM = 'UNFORMATTED')
      if (tljcalc) WRITE (451) maxlw, EINl, IRElat(Npro,Ntrg)
      WRITE (45) maxlw, EINl, IRElat(Npro,Ntrg)
      DO l = 0, maxlw
         WRITE (45 )  stl(l + 1)
         if (tljcalc) WRITE (451) (stlj(l + 1,jindex), jindex=1,mxj)
      ENDDO
      WRITE (45 ) ELAcs, TOTcs, ABScs, SINl, SINlcc, CSFus
      if(tljcalc) WRITE (451) ELAcs, TOTcs, ABScs, SINl, SINlcc, CSFus
C
C     A new flag is introduced to signal storage of the Shape elastic XS (Sel(L))
C
      IF (ELAcs.gt.0) THEN
        L = 123456
        WRITE (45) L 
        DO l = 0, maxlw
          WRITE (45) sel(l + 1)
        ENDDO
	ENDIF
      CLOSE (45 )
      if(tljcalc) CLOSE (451)
  300 CONTINUE

C-----Print elastic and direct cross sections from ECIS
      WRITE (8,*) ' '
      WRITE (8,*) ' '
      IF (KTRlom(0,0).GT.0 .AND. FIRst_ein) THEN
        IF (DIRect.EQ.0) THEN
         WRITE (8,*)
     &       ' Results provided by Spherical Optical Model calculations'
        ELSEIF (DIRect.EQ.1 .OR. DIRect.EQ.2) THEN
         WRITE (8,*) ' Results provided by Coupled Channel calculations'
         WRITE (8,*) ' Inelastic scattering results provided by'
         WRITE (8,*) ' Coupled Channel + DWBA calculations'
        ELSEIF (DIRect.EQ.3) THEN
         WRITE (8,*)
     &       ' Results provided by Spherical Optical Model calculations'
         WRITE (8,*)
     &     ' Inelastic scattering results provided by DWBA calculations'
        ENDIF
      ENDIF
C     write(*,*) 'CSFus=',CSFus,NLW 

      IF(TOTred.NE.1) then
        FUSred = TOTred*FUSred0
        ELAred = TOTred*ELAred0     
        FCCred = TOTred*FCCred0 
        FCOred = TOTred*FCOred0 
        if (TOTred.LT.0.997d0 .or. TOTred.GT.1.003d0) then
          WRITE (8,'(1x,A50,A50,F5.3,A10,G10.5,A4)') 
     >      ' WARNING: FUSRED,ELARED, FCCRED and FCORED changed',
     >      ' to impose requested scaling of total by  TOTRED= ',
     >      TOTred,' for Einc=',EINl,' MeV'
        endif
        WRITE (8,'(1x,A22,F5.3)') ' Renormalized FUSRED :',FUSRED 
        WRITE (8,'(1x,A22,F5.3)') ' Renormalized ELARED :',ELARED 
        WRITE (8,'(1x,A22,F5.3)') ' Renormalized FCCRED :',FCCRED 
        WRITE (8,'(1x,A22,F5.3)') ' Renormalized FCORED :',FCORED 
      ENDIF

      el = EINl
      relcal = .FALSE.
      IF (IRElat(Npro,Ntrg).GT.0  .or. RELkin) relcal = .TRUE.
      CALL KINEMA(el,ecms,xmas_npro,xmas_ntrg,ak2,1,relcal)

      IF (EINl.LT.0.3D0 .AND. ZEJc(Npro).EQ.0) THEN
         s0 = FUSred * stl(1)/(2.0D+00*PI*SQRT(1.0D+06*EINl))
         rp = 1.35*(A(Ntrg)**0.333333333)
         r2 = rp*rp
         p1 = (ak2*r2)/(1.0D+00 + ak2*r2)
         s1a = FUSred * stl(2)/(2.0D+00*PI*p1*SQRT(1.0D+06*EINl))
         p2 = (ak2*r2)**2/(9.0D+00+3.0D+00*ak2*r2+(ak2*r2)**2)
         s2a = FUSred * stl(3)/(2.0D+00*PI*p2*SQRT(1.0D+06*EINl))
C--------Corrected scattering radius
         rp = SQRT((ELAred*ELAcs)/(4.0D+00*PI*10.D+00))
         WRITE (8,*)
         IF(S0_obs.GT.0.)   THEN
           WRITE (8,99004) S0_obs,S0_unc
           WRITE (12,99004) S0_obs,S0_unc
99004      FORMAT (7x,54(1h*)/
     &           6x,' LOW ENERGY NEUTRON SCATTERING:'/
     &           6x,' Exp.  Strength functions S0 =',f6.3,' (',f6.4,')')
         ELSE
           WRITE (8,'(7x,54(1h*)/
     &         7x,''LOW ENERGY NEUTRON SCATTERING:'')')
           WRITE (12,'(7x,54(1h*)/
     &         7x,''LOW ENERGY NEUTRON SCATTERING:'')')
         ENDIF
         WRITE (8,99005)
     &         s0*1D4, FUSRED*stl(1), s1a*1D4, 
     &                 FUSRED*stl(2), s2a*1D4, 
     &                 FUSRED*stl(3),
     &         EINl*1.D3, TOTcs*TOTred, rp, 1.35*(A(Ntrg)**0.333333333) 
         WRITE (12,99005)
     &         s0*1D4, FUSRED*stl(1), s1a*1D4, 
     &                 FUSRED*stl(2), s2a*1D4, 
     &                 FUSRED*stl(3),
     &         EINl*1.D3, TOTcs*TOTred, rp, 1.35*(A(Ntrg)**0.333333333)
99005    FORMAT (6x,' Calc. Strength functions S0 =',f6.3,' T0=',d12.6/
     &           6x,'                          S1 =',f6.3,' T1=',d12.6/
     &           6x,'                          S2 =',f6.3,' T2=',d12.6/
     &           6x,' Elab = ',F6.1,' keV',
     &              '        Total XS = ',F9.2,' mb'/
     &           6x,' Scattering radius =',f7.3,' fm'/
     &           6x,'   1.35 A**(1/3)   =',f7.3,' fm'//7x,54(1h*))
         WRITE (8,*)
         WRITE (12,*)
         selast = 0.d0
         DO l = 0, maxlw
           IF(sel(l+1).LT.1.d-15) EXIT
           selast = selast + (2*l+1)*sel(l + 1)
         ENDDO

         IF(selast.gt.0.d0) then
           selast = selast *  10.d0*PI/ak2
           WRITE(8,'(7x,28HSHAPE ELASTIC CROSS SECTION=,F10.3,1x,
     &              6H(ECIS:,F10.3,1H),1x,2hmb)') 
     &              selast, ELAcs 
           WRITE(12,'(7x,28HSHAPE ELASTIC CROSS SECTION=,F10.3,1x,
     &              6H(ECIS:,F10.3,1H),1x,2hmb)') 
     &              selast, ELAcs
           WRITE(53,'(7x,5HElab=,F7.2,1x,3HkeV,
     &              6x,17HSHAPE ELASTIC XS=,F10.3,1x,2hmb)') 
     &              EINl*1000, selast
           WRITE (8,99006)
           WRITE (12,99006)
           WRITE (53,99006)
           DO l = 0, maxlw
             IF(STL(l+1).LT.1.d-15) EXIT
             WRITE (8,99007) l, stl(l + 1), 
     &              10.d0*PI/ak2*sel(l + 1)
             WRITE (12,99007) l, stl(l + 1),
     &              10.d0*PI/ak2*sel(l + 1)
             WRITE (53,99007) l, stl(l + 1),
     &              10.d0*PI/ak2*sel(l + 1)
           ENDDO
           WRITE (8,99008)
           WRITE (12,99008)
           WRITE (53,99008)
99006      FORMAT (6x,' ****************************************'/
     &             6x,' *  L         Tl(L)    Shape Elastic(L) *')
99007      FORMAT (6x,' *',I3,2(1x,D15.7),'   *')
99008      FORMAT (6x,' ****************************************')
           WRITE (8,*)
           WRITE (8,*)
     &     '      SElast = SUM_over_L {(2*L+1)*Shape Elastic(L)}'
           WRITE (8,*)'      Sfus   = SUM_over_L {(2*L+1)*Tl(L)}'
           WRITE (8,*)
     &     '      Sreact = Sfusion + SUM_over_exc.lev.j {Sinl(j)}'
           WRITE (8,*)
           WRITE (12,*)
          ELSE
           WRITE (8,89006)
           WRITE (12,89006)
           WRITE (53,89006)
           DO l = 0, maxlw
             IF(STL(l+1).LT.1.d-15) EXIT
             WRITE (8 ,89007) l, stl(l + 1) 
             WRITE (12,89007) l, stl(l + 1)
             WRITE (53,89007) l, stl(l + 1)
           ENDDO
           WRITE (8,89008)
           WRITE (12,89008)
           WRITE (53,89008)
89006      FORMAT (/6x,' ************************'/
     &             6x,' *  L         Tl(L)     *')
89007      FORMAT (6x,' *',I3,1x,D15.7,'   *')
89008      FORMAT (6x,' ************************')
           WRITE (8,*)
           WRITE (8,*)'      Sfus   = SUM_over_L {(2*L+1)*Tl(L)}'
           WRITE (8,*)
     &     '      Sreact = Sfusion + SUM_over_exc.lev.j {Sinl(j)}'
           WRITE (8,*)
           WRITE (12,*)
          ENDIF
      ENDIF

      el = EINl
      relcal = .FALSE.
      IF (IRElat(Npro,Ntrg).GT.0  .or. RELkin) relcal = .TRUE.
      CALL KINEMA(el,ecms,xmas_npro,xmas_ntrg,ak2,1,relcal)
	coef = 1.d0
      IF (INT(AEJc(0)).GT.0)
     &        coef = 10.d0*PI/ak2/
     &           (2*trgsp + 1.d0)/(2*sxj + 1.d0)
C     write(*,*) 'FUSI=',10.d0*PI/ak2,el,IRElat(NPRo,Ntrg),
C    &           RELKIN,relcal

C	write (*,*) '*** NLW=',NLW,tljcalc
      CSFus = 0.d0
      csmax = 0.d0
	IF (tljcalc) THEN
C-------absorption spin distribution using Tlj's
        DO ip = 1, 2      ! over parity
          DO j = 1, NLW   !over compound nucleus spin
            sum = 0.d0
            xjc = float(j) + HIS(1)
            jmin = abs(xjc - trgsp)
            jmax = xjc + trgsp
C           ensure we are within dimensions for the min=1, and the max=NDLW 
            kmin = max(1   ,INT(jmin - mxj + (2.d0 + sxj))) !minimum k=l+1
            kmax = min(NDLW,INT(jmax - 1   + (2.d0 + sxj))) !maximum k=l+1
            DO k = kmin, kmax                       !do loop over l in Tlj
               DO jindex = 1, mxj                   !do loop over j-index in Tlj
                  xj = k + jindex - (2.d0 + sxj)
                  IF(xj<jmin .or. xj>jmax) CYCLE
C                 IF(stlj(k,jindex)<=1.0d-15) CYCLE
                  sum = sum + PAR(ip,LVP(LEVtarg,Ntrg),k - 1)*
     &                stlj(k,jindex)
               ENDDO
            ENDDO
            POP(NEX(1),j,ip,1) = coef*sum*(2.D0*xjc + 1.D0)*FUSred
            CSFus = CSFus + POP(NEX(1),j,ip,1)
            csmax = DMAX1(POP(NEX(1),j,ip,1),csmax)
          ENDDO
        ENDDO

	ELSE
C
C-------absorption spin distribution using Tl's
        smin = ABS(sxj - trgsp)
        smax = sxj + trgsp
        mul = smax - smin + 1.0001
        DO ip = 1, 2     ! over parity
          DO j = 1, NLW   !over compound nucleus spin
            sum = 0.d0
            DO ichsp = 1, mul
               chsp = smin + FLOAT(ichsp - 1)
               lmin = ABS(j - chsp - S1) + 0.0001
               lmax = j + chsp - S1 + 0.0001
               lmin = lmin + 1
               lmax = lmax + 1
               lmax = MIN0(NDLW,lmax)
               lmax = MIN0(maxlw,lmax)
               DO k = lmin, lmax
                 sum = sum + PAR(ip,LVP(LEVtarg,Ntrg),k - 1)*stl(k)
               ENDDO
            ENDDO
            POP(NEX(1),j,ip,1) = coef*sum*(FLOAT(2*j + 1) - 2.0*S1)
     &                           *FUSred
            CSFus = CSFus + POP(NEX(1),j,ip,1)
            csmax = DMAX1(POP(NEX(1),j,ip,1),csmax)
          ENDDO
        ENDDO

	ENDIF
C	write (*,*) '*** CSFus =',CSFus

      IF (CSFus.GT.0 .AND. (SINl.GT.0 .or. SINLcont.GT.0) ) THEN
         IF (DIRect.LE.2) THEN
C-----------DIRECT=1 or DIRECT=2
            WRITE (8,*) ' CC OMP TLs normalized to substract DWBA contri
     &tion from collective levels'
         ENDIF
      ENDIF

C Direct deuteron breakup/breakup fusion taken into account here
      IF(DBRkup.gt.1.0d-2) THEN
         ctmp = ctldir//ctmp23//'.DBK'
         INQUIRE (FILE = ctmp, EXIST = fexist)
         IF (.not.fexist .OR. CALctl) CALL dirdbrkup(ctmp)
C            call dirdbrkup(ctmp)
          OPEN(unit=45,file=ctmp,status='OLD')
C Read and check energy grid
          READ(45,'(i6,f12.5)') nxx,dex
          IF(ABS(dex-DE) .GT. 1.0d-3) THEN
            CLOSE(45)
            CALL dirdbrkup
            OPEN(unit=45,file=ctmp,status='OLD')
            READ(45,'(i6,f12.5)') nxx,dex
            WRITE (8,*)
     &  ' Deuteron breakup cross sections and populations written to : '
            WRITE (8,*) ' ', ctmp
            WRITE (8,*)
           ELSE
            WRITE (8,*)
     &   ' Deuteron breakup cross sections and populations read from : '
            WRITE (8,*) ' ', ctmp
            WRITE (8,*)
           ENDIF

C Integrated cross sections - bu, bf,n, bf,p, inclus n, inclus p
          READ(45,'(5e12.5)') (CSDbrkup(i),i=1,5)
          DO i=1,5
            CSDbrkup(i)=DBRkup*CSDbrkup(i)
           ENDDO
          CSEmis(1,1)=CSEmis(1,1)+CSDbrkup(4)
          CSEmis(2,1)=CSEmis(2,1)+CSDbrkup(5)
          CSprd(Nres(4))=CSDbrkup(1)

C Deplete deuteron fusion cross section and initial population in accordance
C with breakup losses. The total absorption cross section is increased if
C breakup losses extend beyond those of the optical model calculation 
C (usually a small effect).
          READ(45,'(i6)') lmx
          READ(45,'(6e12.5)') (sigdep(j),j=1,lmx)
          DO j=1,MIN(NLW,lmx)
            DO ip=1,2
              pops=0.5d0*DBRkup*sigdep(j)
              ftmp=MIN(pops,POP(NEX(1),j,ip,1))
              POP(NEX(1),j,ip,1)=POP(NEX(1),j,ip,1)-ftmp
              CSFus=CSFus-ftmp
              ABScs=ABScs+pops-ftmp
             END DO
           END DO

C Populate neutron emission breakup-fusion residual nucleus
          Nnur=NRes(1)
          nspecc = MIN(MAX(NINT((EX(NEX(1),1) - Q(1,1) - ECUt(Nnur))/DE 
     &                                             + 1.0001),0),ndecsed) 
          READ(45,'(i6)') lmx
          READ(45,'(6e13.5)') ((dbfl(j,nx),j=1,lmx),nx=1,nxx)
          DO nx=1,nxx
            nxe=nxx+2-nx
            ie=MAX(nspecc+1-nxe,0)
            DO j=1,lmx
              pops = 0.5d0*DBRkup*dbfl(j,nx)
              POP(ie,j,1,Nnur) = POP(ie,j,1,Nnur) + pops
              POP(ie,j,2,Nnur) = POP(ie,j,2,Nnur) + pops
             END DO
           END DO

C Neutron DDX
          READ(45,'(2i6)') nthi
          READ(45,'(6e13.5)') ((dbf(nti,nx),nti=1,nthi),nx=1,nxx)

          DO nx=1,nxx
            DO nti=1,nthi
              pops=DBRkup*dbf(nti,nx)
              nxe=nxx+2-nx
              CSEa(nxe,nti,1,1) = CSEa(nxe,nti,1,1) + pops
             END DO
           END DO
        
C Neutron spectrum
          READ(45,'(6e13.5)') (buspec(nx),nx=1,nxx)

          IF(ENDF(1).GT.0) THEN
            DO nx=1,nxx
              pops=DBRkup*buspec(nx)
              nxe=nxx+2-nx
              ie=MAX(nspecc+1-nxe,0)
              POPcse(ie,1,nxe,INExc(Nnur)) =
     &           POPcse(ie,1,nxe,INExc(Nnur)) + pops
              POPcsed(ie,1,nxe,INExc(Nnur)) =
     &            POPcsed(ie,1,nxe,INExc(Nnur)) + pops
              POPcseaf(ie,1,nxe,INExc(Nnur)) = 1.0
              POPbin(ie,Nnur) = pops
             END DO
            ELSE
             DO nx=1,nxx
               pops=DBRkup*buspec(nx)
               nxe=nxx+2-nx
               CSEdbk(nxe,1)=CSEdbk(nxe,1)+pops
               CSE(nxe,1,1)=CSE(nxe,1,1)+pops
              END DO
            ENDIF

C Populate proton emission breakup-fusion residual nucleus
          Nnur=NRes(2)          
          nspecc = MIN(MAX(NINT((EX(NEX(1),1) - Q(2,1) - ECUt(Nnur))/DE 
     &                                             + 1.0001),0),ndecsed) 
          READ(45,'(i6)') lmx
          READ(45,'(6e13.5)') ((dbfl(j,nx),j=1,lmx),nx=1,nxx)
          DO nx=1,nxx
            ie=MAX(nspecc-nx,0)
            DO j=1,lmx
              pops = 0.5d0*DBRkup*dbfl(j,nx)
              POP(ie,j,1,Nnur) = POP(ie,j,1,Nnur) + pops
              POP(ie,j,2,Nnur) = POP(ie,j,2,Nnur) + pops
             END DO
           END DO

C Proton DDX
          READ(45,'(2i6)') nthi
          READ(45,'(6e13.5)') ((dbf(nti,nx),nti=1,nthi),nx=1,nxx)

          DO nx=1,nxx
            DO nti=1,nthi
              pops=DBRkup*dbf(nti,nx)
              CSEa(nx+1,nti,2,1) = CSEa(nx+1,nti,2,1) + pops
             END DO
           END DO
        
C Proton spectrum
          READ(45,'(6e13.5)') (buspec(nx),nx=1,nxx)

          IF(ENDF(1).GT.0) THEN
            DO nx=1,nxx
              pops=DBRkup*buspec(nx)
              ie=MAX(nspecc-nx,0)
              POPcse(ie,2,nx+1,INExc(Nnur)) =
     &            POPcse(ie,2,nx+1,INExc(Nnur)) + pops
              POPcsed(ie,2,ie,INExc(Nnur)) =
     &            POPcsed(ie,2,nx+1,INExc(Nnur)) + pops
              POPcseaf(ie,2,nx+1,INExc(Nnur)) = 1.0
              POPbin(ie,Nnur) = pops
             ENDDO
            ELSE
            DO nx=1,nxx
              pops=DBRkup*buspec(nx)            
              CSEdbk(nx+1,2)=CSEdbk(nx+1,2)+pops
              CSE(nx+1,2,1)=CSE(nx+1,2,1)+pops
             END DO
           ENDIF

        ENDIF  ! End direct dbrkup section

      IF (IOUt.EQ.5) THEN
        WRITE (8,*) 
	  IF(FUSred.ne.1.d0) 
     &    WRITE (8,*) '        FUSred is NOT considered below'
        WRITE (8,*) 
     &'        CSFus(SUM_Tl)      CSFus+SINl+CC+SINlcont     ABScs(OMP)'
        WRITE (8,'(4x,3(4x,D15.8,4x))')
     &   CSFus/Fusred, CSFus/Fusred + SINl + SINlcc + SINlcont, ABScs
        WRITE (8,*) 
     &'           SINl                  SINlcc                SINlcont'
        WRITE (8,'(4x,3(4x,D15.8,4x))') SINl, SINlcc, SINlcont
        WRITE (8,*) 
      ENDIF

C
C     CSFus contains only the reaction cross section to be distributed
C
C-----calculation/reading of transmission coefficients for input channel done ------
C
C-----Passing ELTL() and ELTLJ () to HRTW routine, note that they never contain
C-----direct contribution !!!
C
      DO i = 1, NDLW
         ELTl(i) = stl(i)
         DO j = 1, mxj
           ELTlj(i,j) = stlj(i,j)
         ENDDO
      ENDDO
C
      DO j = NDLW, 1, -1
         NLW = j 
         IF (POP(NEX(1),j,1,1)*10000.D0.GT.csmax) exit
         IF (POP(NEX(1),j,2,1)*10000.D0.GT.csmax) exit
      ENDDO
C
C     NORMAL RETURN
  400 CONTINUE
C-----the next line can be used to increase the number of partial waves
C-----e.g., to account for a high-spin isomer
C-----Plujko_new-2005
      NLW = min(NLW + 1 + MAXmult,NDLW)
C
C-----check whether NLW is not larger than 
C-----max spin at which nucleus is still stable 
C
      IF (NLW.GT.JSTab(1) .and. JSTab(1).GT.0) THEN
          WRITE (8,
     & '(''  WARNING: Maximum spin to preserve stability is'',I4)')
     &             JSTab(1)
          WRITE (8,
     & '(''  WARNING: Calculations will be truncated at this limit'')')
          WRITE (8,
     & '(''  WARNING: Maximum stable spin (rot. limit) Jstab < '',I3)') 
     & Jstab(1) + 1
          IF(Jstab(1).LE.NDLW) then
            ftmp = 0.d0
            DO j = Jstab(1), min(NDLW,NLW)
              ftmp = ftmp + POP(NEX(1),j,1,1) + POP(NEX(1),j,2,1)
              POP(NEX(1),j,1,1) = 0.d0
              POP(NEX(1),j,2,1) = 0.d0
            ENDDO
            CSFus = CSFus - ftmp
            WRITE (8,'(''  WARNING: Some fusion cross section lost : '',
     & F9.3,'' mb, due to the stability limit'')') ftmp  
          ELSE
            WRITE (8,
     &'(''  WARNING: Increase NDLW in dimension.h and recompile EMPIRE''
     & )')
          ENDIF
          NLW = min(JSTab(1),NDLW)
      ENDIF

      IF ((POP(NEX(1),NLW,1,1)*20.D0.GT.csmax .OR. POP(NEX(1),NLW,2,1)
     &    *20.D0.GT.csmax) .AND. NLW.EQ.NDLW) THEN
         WRITE (8,*) 'POP1=', POP(NEX(1),NLW,1,1), 'POP2=',
     &               POP(NEX(1),NLW,2,1), 'NLW=', NLW
         WRITE (8,
     &'('' NUMBER OF PARTIAL WAVES FOR WHICH CODE IS DIMENSIONE'',
     &''D IS INSUFFICIENT'',/,'' INCREASE NDLW IN THE dimensio'',
     &''n.h FILE AND RECOMPILE  '',/,'' EXECUTION  S T O P P E '',
     &''D '')')
         STOP 'ERROR: Insufficient dimension NDLW for partial waves'
      ENDIF

C     deallocate stl(), stlj(), sel() 
      if(allocated(stl )) deallocate(stl)
      if(allocated(sel) ) deallocate(sel)
      if(allocated(stlj)) deallocate(stlj)

      call get_TLs()

      RETURN
      END

      subroutine get_TLs()
      implicit none
      INCLUDE "dimension.h"
      INCLUDE "global.h"

C     local variables
      INTEGER nejc,nnuc,nnur,iloc,netl,izares,i,j    
      DOUBLE PRECISION ares,zres 
      LOGICAL nonzero  
C
C
C-----calculate transmission coefficients in outgoing channels
C
      DO nnuc = 1, NNUcd
         DO nejc = 1, NEJcm
            ares = A(nnuc) - AEJc(nejc)
            zres = Z(nnuc) - ZEJc(nejc)
C           residual nuclei must be heavier than alpha
            if(ares.le.4 . or. zres.le.2) cycle

            izares = INT(1000*zres + ares)
            CALL WHERE(izares,nnur,iloc)
            IF (iloc.EQ.1) cycle

            netl = 6
            IF (NEX(nnuc).GT.0) netl =
     &         INT((EX(NEX(nnuc),nnuc) - Q(nejc,nnuc))/DE) + 6
          
            IF (netl.GT.NDETL) cycle
            
            ICAlangs = ICAlangs-10
            i = NANgela
            NANgela = 2
            CALL TLEVAL(nejc,nnur,nonzero)
            ICAlangs = ICAlangs+10
            NANgela = i
C-----------print transmission coefficients
            IF (nonzero .AND. IOUt.EQ.5) THEN
              WRITE (8,*)
              WRITE (8,*) ' Transmission coefficients for '
              WRITE (8,'(1x,A15,I3,A3,I3,A3,F4.1)')
     &                    ' Projectile: A=', INT(AEJc(nejc)), ' Z=',
     &                   INT(ZEJc(nejc)), ' S=', SEJc(nejc)
              WRITE (8,'(1x,A11,I3,A3,I3,A3,F4.1,A3,I2)')
     &                    ' TARGET: A=', INT(A(nnur)), ' Z=',
     &                   INT(Z(nnur)), ' S=', SNGL(XJLv(1,nnur)),
     &                   ' P=', INT(LVP(1,nnur))
              DO i = 1, netl
                IF (TL(i,1,nejc,nnur).GT.0.0) 
     &             WRITE (8,'(1X,14(1P,E10.4,1x))')
     &             ETL(i,nejc,nnur), (TL(i,j,nejc,nnur),j = 1,12)
              ENDDO
              WRITE (8,'(1X,/)')
            ENDIF
         ENDDO     !over ejectiles (nejc)
      ENDDO     !over nuclei (nnuc)
C
C-----determination of transmission coeff.--done
      RETURN
      END

      SUBROUTINE BASS(Ein,Zp,Ap,Zt,At,Bfus,E1,Crl,Csfus)
Ccc
Ccc   *********************************************************************
Ccc   *                                                         class:ppu *
Ccc   *                         B A S S
Ccc   * Calculates fusion x-section critical l-value for a heavy-ion
Ccc   * induced reaction according to Bass model. E1 is the energy at
Ccc   * which the linear dependence of l critical begins.
Ccc   * ref: formulae  from Bass, Nucl. Phys. A231(1974)45,
Ccc   * and nuclear potential from Phys. Rev. Lett. 39(1977)265
Ccc   *
Ccc   * input:EIN-incident energy (c.m.)
Ccc   *       ZP -Z of a projectile
Ccc   *       AP -A of a projectile
Ccc   *       ZT -Z of a target
Ccc   *       AT -A of a target
Ccc   *
Ccc   * output:BFUS-fusion barrier
Ccc   *        E1   -see above
Ccc   *        CRL  -critical angular momentum
Ccc   *        CSFUS-fusion x-section
Ccc   *
Ccc   * calls:FINDA
Ccc   *
Ccc   *********************************************************************
Ccc
C
C COMMON variables
C
      DOUBLE PRECISION MI, R1, R12, R2
      COMMON /FIND  / R1, R2, R12, MI
C
C Dummy arguments
C
      DOUBLE PRECISION Ap, At, Bfus, Crl, Csfus, E1, Ein, Zp, Zt
C
C Local variables
C
      DOUBLE PRECISION arg, d, dfu, e2, ee2, f, ht, le1, le2, m0, m1,
     &                 m2, p, t, vc, vmax, vmm, vn, x, y
      INTEGER INT
      INTEGER j, jl
      DATA e2, m0, ht, d/1.44, 1.044, 6.589, 1.35/
      m1 = Ap*m0
      m2 = At*m0
      MI = Ap*At/(Ap + At)*m0
      p = Ap**(1./3.)
      t = At**(1./3.)
      R1 = 1.16*p - 1.39/p
      R2 = 1.16*t - 1.39/t
      R12 = R1 + R2
      x = Zp*Zt*e2*(R1 + R2)/(R1*R2*R12**2)*0.07
      y = ht**2*(R1 + R2)/(MI*R1*R2*R12**3)*0.07
      f = 1./(1. + 2./5.*((m1*R1**2.+m2*R2**2.)/(MI*R12**2.)))
      le1 = SQRT((1. - x)/y)
      le2 = le1/f
      vc = Zp*Zt*e2/R12
      vn = R1*R2/R12*(1./(0.03 + 0.0061))
      E1 = vc - vn + (ht**2*le1**2)/(2.*MI*R12**2)
      ee2 = vc - vn + (ht**2*le2**2)/(2.*MI*R12**2)
      dfu = -d*LOG(x)/(1. - 2.*d/R12)
      arg = dfu/d
      IF (arg.GT.74.D0) arg = 74.
      Bfus = Zp*Zt*e2/R12*(R12/(R12 + dfu) - d/(x*R12)*EXP((-arg)))
      IF (Ein.GT.Bfus) THEN
         Crl = SQRT((2.*MI*R12**2/ht**2)*(Ein - vc + vn))
         vmm = 0.0
         vmax = 10000.0
         IF (Ein.LT.E1) THEN
            jl = INT(le1)
            DO j = 1, jl
               Crl = jl - j + 1
               vmm = vmax
               CALL FINDA(Zp,Zt,Crl,vmax)
               IF (vmax.LE.Ein) GOTO 50
            ENDDO
         ENDIF
   50    IF (Ein.GT.ee2) Crl = le2
         IF (Ein.LT.E1) Crl = Crl + (Ein - vmax)/(vmm - vmax)
         Csfus = 657.*(Ap + At)*Crl**2/(Ap*At*Ein)

         WRITE (8,*) ' '
         WRITE (8,*)
     &' Fusion cross section calculated using Bass model as a reference'
         WRITE (8,*) ' see Nucl. Phys. A231(1974)45'
         WRITE (8,*) 
     &     ' Nuclear potential from Phys. Rev. Lett. 39(1977)265'
         WRITE (8,*) ' '
         WRITE (8,*) ' Bass Barr =',sngl(Bfus),  ' Crit.L =',sngl(Crl)
         WRITE (8,*) ' '
         WRITE (8,*) ' Bass XS   =',sngl(Csfus),' mb'
         WRITE (8,*) 

      ELSE

         WRITE (8,'(1X,
     >  ''  WARNING: Incident energy below the Bass fusion barrier'')')
         WRITE (8,'(1X,
     >  ''  WARNING: For the Bass model, fusion cross section = 0'')')
         WRITE (8,*)
         Csfus = 0.d0
      ENDIF
      RETURN
      END

      SUBROUTINE FINDA(Zp,Zt,Crl,Vm)
Ccc
Ccc ********************************************************************
Ccc *                                                         class:mpu*
Ccc *                         F I N D A                                *
Ccc *                                                                  *
Ccc * Solves the equation in Bass model; VM is a solution              *
Ccc *                                                                  *
Ccc * input:ZP -Z of a projectile                                      *
Ccc *       AP -A of a projectile                                      *
Ccc *       ZT -Z of a target                                          *
Ccc *       AT -A of a target                                          *
Ccc *       CRL-l critical                                             *
Ccc *                                                                  *
Ccc * output:VM -solution                                              *
Ccc *                                                                  *
Ccc * calls:none                                                       *
Ccc *                                                                  *
Ccc *                                                                  *
Ccc ********************************************************************
Ccc
C
C COMMON variables
C
      DOUBLE PRECISION MI, R1, R12, R2
      COMMON /FIND  / R1, R2, R12, MI
C
C Dummy arguments
C
      DOUBLE PRECISION Crl, Vm, Zp, Zt
C
C Local variables
C
      DOUBLE PRECISION e2, eps, ht, xm, xn, xp
      INTEGER nr
      DATA e2, ht/1.44, 6.589/
      DATA eps/0.001/
      nr = 0
      xn = R12
      xp = 3*R12
  100 xm = (xn + xp)/2
      nr = nr + 1
      IF (nr.LE.50) THEN
         IF (ABS((-Zp*Zt*e2/xm**2) + ((-ht**2*Crl**2/(MI*xm**3)))
     &       + R1*R2/R12*(0.03/3.3*EXP((xm-R12)/3.3)
     &       +0.0061/0.65*EXP((xm-R12)/0.65))
     &       /(0.03*EXP((xm-R12)/3.3)+0.0061*EXP((xm-R12)/0.65))**2)
     &       .GT.eps) THEN
            IF (((-Zp*Zt*e2/xm**2)) + ((-ht**2*Crl**2/(MI*xm**3)))
     &          + R1*R2/R12*(0.03/3.3*EXP((xm-R12)/3.3)
     &          + 0.0061/0.65*EXP((xm-R12)/0.65))
     &          /(0.03*EXP((xm-R12)/3.3) + 0.0061*EXP((xm-R12)/0.65))
     &          **2.LT.0.D0) THEN
               xp = xm
               GOTO 100
            ELSEIF (((-Zp*Zt*e2/xm**2)) + ((-ht**2*Crl**2/(MI*xm**3)))
     &              + R1*R2/R12*(0.03/3.3*EXP((xm-R12)/3.3)
     &              + 0.0061/0.65*EXP((xm-R12)/0.65))
     &              /(0.03*EXP((xm-R12)/3.3) + 0.0061*EXP((xm-R12)/0.65)
     &              )**2.NE.0.D0) THEN
               xn = xm
               GOTO 100
            ENDIF
         ENDIF
      ENDIF
      Vm = Zp*Zt*e2/xm + ht**2*Crl**2/(2*MI*xm**2)
     &     - R1*R2/R12/(0.03*EXP((xm-R12)/3.3)
     &     + 0.0061*EXP((xm-R12)/0.65))
      IF (nr.GT.50) WRITE (8,'(10X,''MAX NO. OF ITERATIONS IN FINDA'')')
      END


      DOUBLE PRECISION FUNCTION XFUS(Ein,Ap,At,D,Crl)
Ccc
Ccc ********************************************************************
Ccc *                                                         class:ppu*
Ccc *                         X F U S                                  *
Ccc *                                                                  *
Ccc *                                                                  *
Ccc *                                                                  *
Ccc * input:EIN - incident energy (c.m.)                               *
Ccc *       AP  - projectile A                                         *
Ccc *       AT  - target A                                             *
Ccc *       D   - difusness in transmission coefficient formula        *
Ccc *       CRL - l critical for fusion                                *
Ccc *                                                                  *
Ccc * output:XFUS- fusion x-section                                    *
Ccc *                                                                  *
Ccc * calls:none                                                       *
Ccc *                                                                  *
Ccc *                                                                  *
Ccc ********************************************************************
Ccc
      IMPLICIT DOUBLE PRECISION(A - H), DOUBLE PRECISION(O - Z)
C
C
C Dummy arguments
C
      DOUBLE PRECISION Ap, At, Crl, D, Ein
C
C Local variables
C
      DOUBLE PRECISION al, args, sum, tl
      REAL FLOAT
      INTEGER i, icrl
      INTEGER INT
      sum = 0.0
      icrl = INT(Crl + 5.0*D)
      DO i = 1, icrl
         al = FLOAT(i - 1)
         args = (al - Crl)/D
         IF (args.GT.74.D0) args = 74.
         tl = 1./(1. + EXP(args))
         sum = (2.*al + 1.)*tl + sum
      ENDDO
      XFUS = 657.*(Ap + At)/(Ap*At*Ein)*sum
      END


      SUBROUTINE PUSH(Ecm,A,Ap,At,Bas,Expush,Sigi,Trunc,Stl,Nlw,Ndlw)
Ccc ********************************************************************
Ccc *                                                         class:ppu*
Ccc *                      P U S H                                     *
Ccc *                                                                  *
Ccc *  Calculates fusion transmission coefficients in the distributed  *
Ccc *  fusion barrier model.                                           *
Ccc *                                                                  *
Ccc *                                                                  *
Ccc * output:STL - fusion transmission coefficients                    *
Ccc *                                                                  *
Ccc * calls:INTGRS                                                     *
Ccc *                                                                  *
Ccc *                                                                  *
Ccc ********************************************************************
C
C COMMON variables
C
      DOUBLE PRECISION BAVe, E, EROt, SIG
      COMMON /EXTRAP/ BAVe, EROt, E, SIG
C
C Dummy arguments
C
      DOUBLE PRECISION A, Ap, At, Bas, Ecm, Expush, Sigi, Trunc
      INTEGER Ndlw, Nlw
      DOUBLE PRECISION Stl(Ndlw)
C
C Local variables
C
      DOUBLE PRECISION amu, dintf, prob, r0, rf, xlow, xmax
      DOUBLE PRECISION F, G
      INTEGER j
      EXTERNAL F, G
      DATA r0/1.07/
      E = Ecm
      SIG = Sigi
      BAVe = Bas + Expush
      xlow = MAX(BAVe - Trunc*SIG,0.D0)
      xmax = BAVe + Trunc*SIG
      CALL INTGRS(xlow,xmax,F,dintf)
      amu = At*Ap/A
      rf = r0*(At**0.3333 + Ap**0.3333)
      Nlw=0
      DO j = 1, Ndlw
         EROt = (j - 1)*j*20.79259/(amu*rf**2)
         EROt = EROt/2.0
         CALL INTGRS(xlow,xmax,G,prob)
         Stl(j) = prob/dintf
         IF (Stl(j).gt.1.d-10) Nlw = j
      ENDDO
      END

      DOUBLE PRECISION FUNCTION F(X)
      IMPLICIT DOUBLE PRECISION(A - H), DOUBLE PRECISION(O - Z)
C
C COMMON variables
C
      DOUBLE PRECISION BAVe, E, EROt, SIG
      COMMON /EXTRAP/ BAVe, EROt, E, SIG
C
C Dummy arguments
C
      DOUBLE PRECISION X
      F = EXP(( - (BAVe-X)/(2.0*SIG**2))**2)
      END
C
      DOUBLE PRECISION FUNCTION G(X)
      IMPLICIT DOUBLE PRECISION(A - H), DOUBLE PRECISION(O - Z)
C
C COMMON variables
C
      DOUBLE PRECISION BAVe, E, EROt, SIG
      COMMON /EXTRAP/ BAVe, EROt, E, SIG
C
C Dummy arguments
C
      DOUBLE PRECISION X
C
C Local variables
C
      DOUBLE PRECISION arg, htom, pi
      DOUBLE PRECISION F
      EXTERNAL F
      DATA pi, htom/3.14159D0, 4.D0/
      arg = -2.*pi*(E - X - EROt)/htom
      IF (arg.LT.( - 74.D0)) G = F(X)
      IF (arg.GT.74.D0) G = 0.
      IF (ABS(arg).LE.74.D0) G = F(X)
     &                           /(1 + EXP((-2.*pi*(E-X-EROt)/htom)))
      END

      SUBROUTINE PROCESS_ECIS(Outname,Length,Iret,ICAlangs)
C
C Dummy arguments
C
      INTEGER Iret, Length
      CHARACTER*(*) Outname
C
C Local variables
C
      CHARACTER*132 ctmp
      INTEGER iwin, ipipe_move 
      ctmp = Outname(1:Length)//'.CS'
      iwin = ipipe_move('ecis06.cs',ctmp)
      IF(ICAlangs.GT.0) THEN
        ctmp = Outname(1:Length)//'.EXP'
        iwin = ipipe_move('ecis06.exp',ctmp)
      ENDIF
      IF (Iret.EQ.1) RETURN
      ctmp = Outname(1:Length)//'.TLJ'
      iwin = ipipe_move('ecis06.tlj',ctmp)
      IF (Iret.EQ.2) RETURN
      ctmp = Outname(1:Length)//'.ANG'
      iwin = ipipe_move('ecis06.ang',ctmp)
      ctmp = Outname(1:Length)//'.LEG'
      iwin = ipipe_move('ecis06.leg',ctmp)
      IF (Iret.EQ.3) RETURN
      ctmp = Outname(1:Length)//'.ICS'
      iwin = ipipe_move('ecis06.ics',ctmp)

      RETURN
      END


