cc   * $Rev: 4044 $
Ccc   * $Author: rcapote $
Ccc   * $Date: 2014-08-31 19:20:51 +0200 (So, 31 Aug 2014) $

      SUBROUTINE EMPIRE
Ccc
Ccc   ********************************************************************
Ccc   *                                                         class:ppu        
Ccc   *                         E M P I R E                                    
Ccc   *                                                                        
Ccc   *               Used to be main of the EMPIRE code 
Ccc   *                                                 
Ccc   *                                                 
Ccc   ********************************************************************
      USE empcess
      Implicit none

      INCLUDE "dimension.h"
      INCLUDE "global.h"
C
      INCLUDE "main_common.h"

C     to share /factorial/ calculation of log(fact)
      DOUBLE PRECISION FFFact(6*NDLW)
      COMMON /factorial/FFFact
C
C Local variables
C
      DOUBLE PRECISION xscclow,totcorr 
      DOUBLE PRECISION xsinl,xsmsc,tothms,totemis,corrmsd
      DOUBLE PRECISION epre  

      INTEGER nnuc,nnurec,nejcec,ncollx,iret

	LOGICAL nvwful

      EIN  = 0.0d0
      epre = EIN

      call initial()
C-----
C-----Read and prepare input data
C-----
      DO WHILE(.TRUE.)
 
        CALL INPUT()

        CALL EMPAXS(LHMs, NDAng, NDECSE, NNucd, !NNuct ??, 
     1                        NDEX_D, NDEJCD, NDECSED, NEXclusive, NDLV)
        call open_xs_files()
C-----
C-----  Calculate reaction cross section and its spin distribution
C-----
        CALL MARENG(0,0,nnurec,nejcec)
C
        call get_ecis_inelastic(nejcec,nnurec,ncollx,xscclow,totcorr)
C
C       Preequilibrium 
C
        CALL EMPIRE_PREEQ(xsinl,xsmsc,tothms,totemis,corrmsd,
     >    totcorr,nvwful)

        IF (nvwful) CYCLE ! Skipping HF 

        WRITE(151,*) 'Einc=',EINl

        POPmax(1) = CSFus*1.0E-25
C-------Start DO loop over decaying nuclei
        DO nnuc = 1, NNUcd

          IF(QPRod(nnuc).LT.-999.d0) CYCLE
          CALL calc_fission(nnuc)

          CALL HF_decay(ncollx,nnuc,nnurec,nejcec,iret,totcorr)
                              
        IF(iret.gt.0) GOTO 10 ! GOTO 1155 
        
        ENDDO
C-------END of DO loop over decaying nuclei
C
        CALL write_xs()

        CALL write_ENDF_spectra(totcorr,corrmsd,
     &    xscclow,xsinl,xsmsc,tothms,totemis)
C        
C       end_of_calc is called in this if-block
C   
   10   IF( FITomp.GE.0 ) THEN
          CALL normal_read()   
        ELSE
          CALL OMPFIT_read()
        ENDIF
        CALL new_energy_calc(epre)
C
        CALL EMPDAXS
C
      ENDDO

      RETURN
      END

      subroutine initial
      implicit none
      INCLUDE "dimension.h"
      INCLUDE "global.h"
C
      integer icalled
      DOUBLE PRECISION xcross(0:NDEJC+3,0:15,0:20)
      COMMON /init_empire/xcross,icalled

C     to share /factorial/ calculation of log(fact)
      DOUBLE PRECISION FFFact(6*NDLW)
      COMMON /factorial/FFFact
C
C     Local variables
C
      integer i ,j
      CHARACTER*72 inprecord
      
      ICAlangs = 0
      xcross   = 0.d0
      icalled  = 0

c***********************************************************************
c  Calculate factorial logarithms from 0! ( =1.) up to (idim-1)!       *
c                  k! = fact(k+1)                                      *
c***********************************************************************
      call FCT()

      call open_files
C-----
C-----Skip mandatory part of the standard input
C-----
      REWIND 5
      DO i=1,10
         READ(5,*)
      ENDDO
C-----Read line of optional input
  150 READ (5,'(A72)') inprecord
      IF (inprecord(1:1).EQ.'*' .OR. inprecord(1:1).EQ.'#' .OR.
     &    inprecord(1:1).EQ.'!') GOTO 150  ! comments 

      IF(inprecord(1:1).EQ.'@') THEN ! title
        do j = 1,72
          EMPtitle(j:j) = inprecord(j:j) ! title of the run
        enddo
        EMPtitle(1:1)= ' '
      ENDIF
      REWIND 5

      RETURN
      END

      subroutine FCT()
c***********************************************************************
c  Calculate factorial logarithms from 0! ( =1.) up to (idim-1)!       *
c                  k! = fact(k+1)                                      *
c***********************************************************************
c
      implicit none
      include 'dimension.h' 
      integer          k
      double precision zero
      data             zero /0.0d+00/
	double precision FFFact(6*NDLW)
      common /factorial/FFFact
c=======================================================================
      FFFact(1) = zero
      FFFact(2) = zero
      do k=3,6*NDLW
        FFFact(k) = FFFact(k-1) + DLOG(DBLE(k-1))
      end do
      return
      end

      subroutine open_xs_files()
      implicit none
      INCLUDE "dimension.h"
      INCLUDE "global.h"
C
C     Common variables
C
      DOUBLE PRECISION cel_da(NDAngecis), checkXS
      COMMON /emp_main/cel_da,checkXS   

      INTEGER nepfns, nfission
      DOUBLE PRECISION fnubar,enepfns(NDEPFN),csepfns(NDEPFN)
      COMMON /pfns_quant/nfission, nepfns, fnubar, enepfns, csepfns
 
      logical lbreakup,ltransfer
      COMMON /LPEXS/lbreakup, ltransfer 
C
      CHARACTER*21 preaction(ndnuc)
      integer nuc_print
      common /xsfiles0/preaction
      common /xsfiles1/nuc_print
C     
C     local variables
      integer nejc,i,nnuc,iloc,izares,nnur   
      DOUBLE PRECISION ares, zres
C-----
C     Initialization of energy dependent quantities 
C
      ELCncs  = 0.0D+0 ! Clear CN elastic cross section (1/4*pi) 
      cel_da  = 0.0D+0     
      checkXS = 0.0D+0   

C---- Initialization of PFNS calculations
      IF (FISspe.eq.1) CALL INPUT_SPEC
        
      nfission= 0
      nepfns  = 0             
      enepfns = 0.d0
      csepfns = 0.d0
      fnubar  = 1.d0

C-----Locate positions of ENDF MT-numbers 2, 91, 649, and 849
      CALL WHERE(IZA(1) - IZAejc(0),mt2,iloc)
      CALL WHERE(IZA(1) - IZAejc(1),mt91,iloc)
      CALL WHERE(IZA(1) - IZAejc(2),mt649,iloc)
      CALL WHERE(IZA(1) - IZAejc(3),mt849,iloc)
C-----Locate residual nuclei after CN decay
      NREs(0) = 1
      DO nejc = 1, NEJcm
         NREs(nejc) = -1
         ares = A(1) - AEJc(nejc)
         zres = Z(1) - ZEJc(nejc)
C        residual nuclei must be heavier than alpha
         if(ares.le.4. and. zres.le.2.) cycle
         izares = INT(1000.0*zres + ares)
         CALL WHERE(izares,nnur,iloc)
         NREs(nejc) = nnur
      ENDDO

C-----
      IF (FITomp.LT.0) OPEN (40,FILE = 'OPTFIT.CAL',STATUS='UNKNOWN')

      OPEN (80,FILE = 'FISSION.OUT',STATUS = 'UNKNOWN')
C-----
C-----Opening XS files for the first energy
C-----
      IF (FIRst_ein) THEN
        OPEN (53,FILE='LOW_ENERGY.OUT', STATUS = 'UNKNOWN')
        OPEN (41, FILE='XSECTIONS.OUT' , STATUS='unknown')

        IF (SFACT.gt.0) then
          OPEN (unit = 781, file = 'S-FACTOR.DAT')
          WRITE(781,*) '#Ecm(MeV)  Cross Section(b)  S-factor(MeV b)'
          WRITE(781,*) '#                                           '
        ENDIF

        IF(NNG_xs.gt.0) 
     &    OPEN (104, FILE='GAMMA_INT.DAT', STATUS='unknown')

        OPEN (107, FILE='EL_INEL.DAT'  , STATUS='unknown')
        OPEN (108, FILE='TOTCOR.DAT'   , STATUS='unknown')
        OPEN (110, FILE='CN-LEV-XS.DAT', STATUS='unknown')

        lbreakup  = .false.
        ltransfer = .false.
        DO nejc = 1, NEJcm
          IF(NTReac(nejc).GT.0.01d0) ltransfer = .true.
          IF(BUReac(nejc).GT.0.01d0) lbreakup  = .true.
        ENDDO

        IF(ltransfer) OPEN (112, FILE='TRANSFER-XS.DAT')
        IF(lbreakup ) OPEN (113, FILE='BREAK-UP-XS.DAT')

        IF(ltransfer .or. lbreakup) OPEN (114, FILE='REAC-MECH.DAT')
        
        i = 0
        DO nnuc=1,NNUcd
          if( REAction(nnuc)(1:2).eq.'(z' ) then
            i = i + 1
              preaction(i) = REAction(nnuc)
          endif
        ENDDO

        nuc_print = i
	  
C       write(*,*) 'nuc_print=',nuc_print,' First energy?',FIRst_ein

        IF (AEJc(NPRoject).EQ.0) then  ! incident photons
          WRITE(41, '(''#'',I3,6X,A1,'' + '',i3,''-'',A2,''-'',I3,5x,
     &A123)') 
     &      nuc_print+6,SYMbe(NPRoject), int(Z(0)), SYMb(0), int(A(0))
          WRITE(107,'(''#'',I3,6X,A1,'' + '',i3,''-'',A2,''-'',I3)') 
     &      15    ,SYMbe(NPRoject), int(Z(0)), SYMb(0), int(A(0))
          WRITE(41,'(''#'',A10,1X,1P,95A12)') '  Einc    ',
     &      '  Total     ','            ',' Nonel=Abs  ',
     &      '  Fission   ','            ','  Nu-bar    ',
     &         (preaction(nnuc),nnuc=1,min(nuc_print,max_prn))
          WRITE(107,'(''#'',A10,1X,1P,20A12)')'   Einc   ',
     &      '  Total     ','            ','            ',
     &      '            ',
     &      ' Nonel=Abs  ','  CN-form   ','  Direct    ',
     &      'Pre-equil   ','Coup-Chan   ',' DWBA-disc  ',
     &      'DWBA-cont   ','   MSD      ','    MSC     ',
     &      '  PCROSS    ','   HMS      '

        ELSE ! incident particles

          IF (A(0).gt.220 .AND. ZEJc(NPRoject).EQ.0 ) then 
C
C           elastic and nonelastic modified for actinides
C           to include/exclude low-lying coupled states
            WRITE(41, '(''#'',I3,6X,A1,'' + '',i3,''-'',A2,''-'',I3,5x,
     &        A123)') 
     &      nuc_print+6,SYMbe(NPRoject), int(Z(0)), SYMb(0), int(A(0)),
     &   ' Elastic* and Nonelast* modified for A>220 (CE & Cross section
     &s of 2 CC added/substracted to Elastic/Nonelast respectively)'
            WRITE(107, '(''#'',I3,6X,A1,'' + '',i3,''-'',A2,''-'',I3,5x,
     &A123)') 
     &      15    ,SYMbe(NPROject), int(Z(0)), SYMb(0), int(A(0)),
     &   ' (Elastic cross section = Shape Elastic + Compound Elastic; fo  
     &r Elastic* and Nonelastic* see output *.xsc file)       '
            WRITE(41,'(''#'',A10,1X,1P,95A12)') '  Einc    ',
     &      '  Total     ','  Elastic*  ',' Nonel-Cel* ',
     &      '  Fission   ','  Mu-bar    ','  Nu-bar    ',
     &         (preaction(nnuc),nnuc=1,min(nuc_print,max_prn))
            WRITE(107,'(''#'',A10,1X,1P,20A12)')'   Einc   ',
     &      '  Total     ','  Elastic   ','     Cel    ',
     &      '   Sel      ',
     &      ' Nonel-Cel  ','  CN-form   ','  Direct    ',
     &      'Pre-equil   ','Coup-Chan   ',' DWBA-disc  ',
     &      'DWBA-cont   ','   MSD      ','    MSC     ',
     &      '  PCROSS    ','   HMS      ','  CC(2 lev) '
          ELSE
            WRITE(41, '(''#'',I3,6X,A1,'' + '',i3,''-'',A2,''-'',I3,5x,
     &A123)') 
     &      nuc_print+6,SYMbe(NPRoject), int(Z(0)), SYMb(0), int(A(0))
            WRITE(107,'(''#'',I3,6X,A1,'' + '',i3,''-'',A2,''-'',I3)') 
     &      15    ,SYMbe(NPRoject), int(Z(0)), SYMb(0), int(A(0))
            WRITE(41,'(''#'',A10,1X,1P,95A12)') '  Einc    ',
     &      '  Total     ','  Elastic   ',' Nonel-Cel  ',
     &      '  Fission   ','  Mu-bar    ','  Nu-bar    ',
     &         (preaction(nnuc),nnuc=1,min(nuc_print,max_prn))
            WRITE(107,'(''#'',A10,1X,1P,20A12)')'   Einc   ',
     &      '  Total     ','  Elastic   ','     Cel    ',
     &      '   Sel      ',
     &      ' Nonel-Cel  ','  CN-form   ','  Direct    ',
     &      'Pre-equil   ','Coup-Chan   ',' DWBA-disc  ',
     &      'DWBA-cont   ','   MSD      ','    MSC     ',
     &      '  PCROSS    ','   HMS      ','  CC(2 lev) '
          ENDIF

        ENDIF

        OPEN (98, FILE='FISS_XS.OUT', STATUS='unknown')
        WRITE(98,'(''#'',I3,6X,A1,'' + '',i3,''-'',A2,''-'',I3)') 
     &     10, SYMbe(NPRoject), int(Z(0)), SYMb(0), int(A(0))
        WRITE(98,'(''#'',A10,1X,1P,20A12)')'   Einc   ',
     &      '  Fiss-tot  ','  Fiss-1st  ','  Fiss-2nd  ',
     &      '  Fiss-3rd  ','  Fiss-4rd  ','  Fiss-5th  ',
     &      '  Fiss-6th  ','  Fiss-7th  ','  Fiss-8th  ',
     &      '  Fiss-9th  ','  Fiss-10th '
        IF (FISspe.GT.0) THEN
          OPEN (114, FILE='PFNS.OUT', STATUS='unknown')
          OPEN (115, FILE='PFNM.OUT', STATUS='unknown')
          WRITE(115,
     &   '(''   Elab         Epfns       nubar(EVAL)  Tmaxw(equiv) '')')
        ENDIF
        IF (ltransfer) then                     
          WRITE(112,'(10X,i3,1x,A2,1X,I3)')int(Z(0)), SYMb(0), int(A(0))
          WRITE(112,'(2a12,a10,'',n'',a10,'',p'',a10,'',a'',a10,'',d'',
     &      a10,'',t'',a10,'',He'')')'   Einc   ',' Total NT ',
     &      SYMbe(NPRoject),SYMbe(NPRoject),SYMbe(NPRoject),
     &      SYMbe(NPRoject),SYMbe(NPRoject),SYMbe(NPRoject)   
        ENDIF
        IF (lbreakup) then                     
          WRITE(113,'(10X,i3,1x,A2,1X,I3)')int(Z(0)), SYMb(0), int(A(0))
          WRITE(113,'(2a12,a10,'',n'',a10,'',p'',a10,'',a'',a10,'',d'',
     &      a10,'',t'',a10,'',He'')')'   Einc   ',' Total BU ',
     &      SYMbe(NPRoject),SYMbe(NPRoject),SYMbe(NPRoject),
     &      SYMbe(NPRoject),SYMbe(NPRoject),SYMbe(NPRoject)   
        ENDIF
        IF (lbreakup .or. ltransfer) then                     
          WRITE(114,'(10X,i3,1x,A2,1X,I3)')int(Z(0)), SYMb(0), int(A(0))
          WRITE(114,*)'  Einc         Reaction     Breakup      Transfer
     &     Preequil.    CN-form '
        ENDIF
      ENDIF

      RETURN
      END
    
      subroutine end_of_calc()
      USE empcess
      implicit none
      INCLUDE "dimension.h"
      INCLUDE "global.h"
C
C     Common variables
C
      INTEGER*4 INDexf, INDexb, BUFfer(250)                              ! R250COM
      COMMON /R250COM/ INDexf,INDexb,BUFfer

      logical lbreakup,ltransfer
      COMMON /LPEXS/lbreakup, ltransfer 
C     
C     local variables
C
      INTEGER i
      DOUBLE PRECISION ftmp

      DOUBLE PRECISION, external :: grand
C
C     CLOSING FILES
C
      CLOSE (5)  ! INPUT.DAT
      IF(FUSREAD) CLOSE (11)  ! FUSION
C     CLOSE (13)              ! RIPL levels
      IF (FILevel) CLOSE (14) ! LEVELS
C     CLOSE (15,STATUS = 'delete')
C     CLOSE (16,STATUS = 'delete')
C     CLOSE (23)   ! not used 
      CLOSE (24)   ! empire/data

      CLOSE (29)
      CLOSE (33)
      IF (FITomp.LT.0) CLOSE (40) ! OPTFIT.CAL
      CLOSE (41)   ! XSECTIONS.OUT
      IF(NNG_xs.gt.0) CLOSE (104) ! 'GAMMA_INT.DAT'
      CLOSE (107)  ! 'EL_INEL.DAT'
      CLOSE (108)  ! 'TOTCOR.DAT'
      CLOSE (110)  ! 'CN-LEV-XS.DAT'

      IF (ltransfer) CLOSE (112)  ! 'TRANSFER-XS.DAT'
      IF (lbreakup)  CLOSE (113)  ! 'BREAK-UP-XS.DAT'
      IF (ltransfer .or. lbreakup)  CLOSE (114) ! 'REAC-MECH.DAT'

C     IF(DEGa.GT.0) THEN
C       CLOSE (42)
C     ELSE
C       CLOSE (42,STATUS = 'delete')
C     ENDIF

      CLOSE(53)   ! LOW-ENERGY.DAT
C     CLOSE(58)

      CLOSE(151)  ! Cont-ANIS-check.dat

C     CLOSE (66,STATUS = 'delete')  ! MSD-orion

      IF(FISspe.GT.0) THEN
        CLOSE (114) ! PFNS.OUT
        CLOSE (115) ! PFNM.OUT
      ENDIF
      CLOSE (98)    ! FISS_XS.OUT

      IF (IOPran.NE.0) then
C----------Saving random seeds
           ftmp = grand()
           OPEN(94,file='R250SEED.DAT',status='UNKNOWN')
           write(94,*)  indexf, indexb
           DO i = 1, 250
             write(94,*) buffer(i)
           ENDDO
           CLOSE(94)
C
           WRITE (8,*)
           WRITE (12,*)

           WRITE (8 ,*) ' Saving RNG status :'
           WRITE (12,*) ' Saving RNG status :'
           WRITE (8 ,*) 'RNG indexes      ', indexf, indexb                         
           WRITE (8 ,*) 'RNG buffer(1)    ', buffer(1)
           WRITE (8 ,*) 'RNG buffer(250)  ', buffer(250)
           WRITE (12,*) 'RNG indexes     ', indexf, indexb                          
           WRITE (12,*) 'RNG buffer(1)   ', buffer(1)
           WRITE (12,*) 'RNG buffer(250) ', buffer(250)

           WRITE (8,*)
           WRITE (12,*)

           WRITE (95,'(A2)') '# '
           WRITE (95,'(A21)') '# Saving RNG status :'
           WRITE (95,'(A21,2(1x,I10))') 
     &     '# RNG indexes        ', indexf, indexb                            
           WRITE (95,'(A21,2(1x,I10))') 
     &     '# RNG buffer(1)      ', buffer(1)                           
           WRITE (95,'(A21,2(1x,I10))') 
     &     '# RNG buffer(250)    ', buffer(250)                         
      ENDIF

      CLOSE(95)  ! COVAR-PAR.DAT
C     CLOSE(102)

      IF (SFACT.GT.0) CLOSE(781)  ! 'S-FACTOR.DAT'

      CALL EMPDAXS


      WRITE (12,*) ' '
      WRITE (12,*) ' CALCULATIONS COMPLETED SUCCESSFULLY'
      WRITE (*,*) ' '
      WRITE (*,*) ' CALCULATIONS COMPLETED SUCCESSFULLY'
      WRITE (8,*) ' '
      WRITE (8,*) ' CALCULATIONS COMPLETED SUCCESSFULLY'

      CALL THORA (8)
      CALL THORA(12)

      CLOSE (8)
      CLOSE (12)

      STOP ' ' 

      END

      subroutine normal_read()
      implicit none
      INCLUDE "dimension.h"
      INCLUDE "global.h"
C     
C     local variables
C
      LOGICAL lheader
      INTEGER ikey1, ikey2, ikey3, ikey4
      DOUBLE PRECISION val

      CHARACTER*6 keyname
      CHARACTER*36 nextenergy
      CHARACTER*72 rtitle

      lheader = .true.
 1156 READ (5,'(A36)',ERR=11570,END=1200) nextenergy

      IF(nextenergy(1:1).eq.'@') THEN 
        BACKSPACE 5
        READ (5,'(A72)',ERR=11570,END=1200) rtitle
        rtitle(1:1)=' '
        write(*,*) '***',trim(rtitle)
        WRITE( 8,*)
     &      '***************************************************'
        write( 8,*) '***',trim(rtitle)
        WRITE( 8,*)
     &      '***************************************************'
        GOTO 1156  ! next line
      ENDIF

      IF (nextenergy(1:1).EQ.'*' .OR. nextenergy(1:1).EQ.'#' 
     &   .OR. nextenergy(1:1) .EQ.'!') GOTO 1156

      IF(nextenergy(1:1).EQ.'$' .and. lheader) THEN
        lheader = .false.
        WRITE (8,*) ' '
        WRITE (8,'(1x,61(''=''))')
        WRITE (8,
     &   '('' Reaction '',A2,'' + '',I3,A2,
     &     '' :NEXT INCIDENT ENERGY STARTED'')') 
     &     SYMbe(NPROject), INT(A(0)), SYMb(0)      

        WRITE (12,*) ' '
        WRITE (12,'(1x,61(''=''))')
        WRITE (12,
     &   '('' Reaction '',A2,'' + '',I3,A2,
     &     '' :NEXT INCIDENT ENERGY STARTED'')') 
     &     SYMbe(NPROject), INT(A(0)), SYMb(0)      
      ENDIF

      IF(nextenergy(1:1).EQ.'$') THEN
        READ(nextenergy,'(1x,A6,F10.5,4I5)',ERR=11570,END=1200) 
     &        keyname, val, ikey1, ikey2, ikey3, ikey4
        CALL OPTIONS(keyname, val, ikey1, ikey2, ikey3, ikey4, 1)
        GO TO 1156
      ENDIF

      BACKSPACE 5
      READ(5,*,ERR=11570,END=1200) EIN

      IF (EIN.LT.0.0D0) THEN
          CALL end_of_calc()
          ELSE
          WRITE (8,
     &'('' Incident energy '',1P,D10.3, '' MeV (LAB)'')') EIN
          WRITE (8,'(1x,61(''=''))')
          WRITE (8,*)

          WRITE (12,
     &'('' Incident energy '',1P,D10.3, '' MeV (LAB)'')') EIN
          WRITE (12,'(1x,61(''=''))')
          WRITE (12,*) ' '

      ENDIF

      RETURN
 1200 CALL end_of_calc()

      RETURN
11570 write(8,*) 'ERROR: Wrong input keyword ',trim(nextenergy)
      write(*,*) 'ERROR: Wrong input keyword ',trim(nextenergy)
      STOP
      END

      subroutine OMPFIT_read()
      implicit none
      INCLUDE "dimension.h"
      INCLUDE "global.h"
C     
C     local variables
C
      CHARACTER*36 nextenergy
      CHARACTER*72 rtitle
      INTEGER iang

 1158 READ (5,'(A36)',ERR=11570,END=1200) nextenergy
      IF (nextenergy(1:1).EQ.'*' .OR. nextenergy(1:1).EQ.'#' .OR. 
     &     nextenergy(1:1).EQ.'!' .OR. nextenergy(1:1).EQ.'$')GOTO 1158

      IF(nextenergy(1:1).eq.'@') THEN 
        BACKSPACE 5
        READ (5,'(A72)',ERR=11570,END=1200) rtitle
        rtitle(1:1)=' '
        write(*,*) '***',trim(rtitle)
        WRITE( 8,*)
     &      '***************************************************'
        write( 8,*)'***',trim(rtitle)
        WRITE( 8,*)
     &      '***************************************************'
        GOTO 1158  ! next line
      ENDIF

      BACKSPACE 5
      READ (5,*,ERR=11570,END=1200) EIN, NDAng, ICAlangs

      IF (EIN.LT.0.0D0) GOTO 1200

      IF(NDAng.lt.2) THEN
        NDAng=2
        ANGles(1) = 0.d0
        ANGles(2) = 180.d0
      ELSE
        READ (5,*,ERR=11570,END=1200) (ANGles(iang),iang=1,NDAng)
      ENDIF
      NANGela=NDAng
      IF(NANgela.GT.NDAngecis) THEN
        WRITE(8,*)
     &        'ERROR: INCREASE NDANGECIS IN dimension.h UP TO ',NANgela
        STOP 'FATAL: INCREASE NDAngecis IN dimension.h'
      ENDIF

      RETURN
 1200 CALL end_of_calc()

      RETURN
11570 write(8,*) 'ERROR: Wrong input keyword ',trim(nextenergy)
      write(*,*) 'ERROR: Wrong input keyword ',trim(nextenergy)

      STOP
      END

      SUBROUTINE new_energy_calc(epre)
      implicit none
      INCLUDE "dimension.h"
      INCLUDE "global.h"

      DOUBLE PRECISION epre  
C     
C     local variables
C
      DOUBLE PRECISION da
      INTEGER iang    

      IF(EIN.LT.epre .and. .NOT. BENchm) THEN
         WRITE(8,*) EIN,epre
         WRITE(8,*) 'ERROR: INPUT ENERGIES OUT OF ORDER !!'
         WRITE(8,*) 'ERROR: CHECK YOUR INPUT FILE'
         PAUSE 'FATAL: INPUT ENERGIES OUT OF ORDER !!'
         STOP
      ENDIF
      epre = EIN
      IF (IOPran.ne.0) 
     &     WRITE(95,'(A19,2x,D12.6)') '# Incident Energy :',EINl
C-----
C-----
      IF(FITomp.GE.0) THEN
        IF(LHMs.EQ.0) THEN
          NANgela = 91
          NDAng   = 91
        ELSE
          NANgela = NDAnghmx
          NDAng   = NDAnghmx
        ENDIF
C-------Set angles for inelastic calculations
        da = 180.d0/(NDANG - 1)
        DO iang = 1, NDAng
          ANGles(iang)  = (iang - 1)*da
          CANgle(iang)  = DCOS(ANGles(iang)*PI/180.d0)
        ENDDO
        DO iang = 1, NDAng
          CANgler(iang) = DCOS(ANGles(NDAng - iang + 1)*PI/180.d0)
          SANgler(iang) = DSIN(ANGles(NDAng - iang + 1)*PI/180.d0)
        ENDDO
      ENDIF

      IF(.not.BENchm) FIRst_ein = .FALSE.
C     
C     Changing CN-isotropic to false if energy is too high as CN
C     decay to discrete levels is negligible         
C     ELV(NLV(0),0) is the energy of the last discrete level of the target nucleus
      if(.not.CN_isotropic) then    
C       Only for actinides, more conditions need to be added        
C       depending on odd/even properties and mass number
        if(A(0).gt.220 .and. EIN.gt.8.d0) then
          CN_isotropic = .TRUE.
          WRITE(8,*)
          WRITE(8,*) 
     &    'CN angular distribution assumed isotropic above Einc = 8 MeV'
          WRITE(*,*)      
          WRITE(*,*) 
     &    'CN angular distribution assumed isotropic above Einc = 8 MeV'
          WRITE(8,*)
        endif  
      endif

      RETURN
      END
