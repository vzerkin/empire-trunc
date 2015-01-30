Ccc   * $Rev: 4761 $
Ccc   * $Author: rcapote $
Ccc   * $Date: 2015-01-19 $
C
C      VERSION 14 (Dec. 2013, LANE CONSISTENT COULOMB CORRECTION 
C      WITH DISPERSIVE OPTICAL POTENTIAL RELATIONS, NON-AXIAL AND
C      NEGATIVE PARITY BABDS LEVELS COUPLING, EVEN-EVEN and ODD
C      NUCLIDES CASE, ANALYZING POWERS, GLOBAL SEARCH, NATURAL 
C      FOR TOTAL AND ISOTOPES RATIO ADJUSTMENT, LOW ENERGY RESONANCES)
C
C      FULL DOUBLE PRECISION (REAL*8 = DOUBLE PRECISION, COMPLEX*16 = DOUBLE COMPLEX)
C      To allow automatic extension to quadruple precision (REAL*16, COMPLEX*32)
C
C      AUTHORS:
C
C      EFREM SOUKHOVITSKI - NON-AXIAL SOFT-ROTATOR NUCLEAR MODEL THEORY, MAIN
C                           CC COMPUTATIONAL ALGORITHMS AND CODING, LANE 
C                           CONSISTENNCY AND(P,N) CS
C                           E-MAIL: esukhov@sosny.bas-net.by
C
C      SATOSHI CHIBA      - THEORY DEVELOPMENT in COOPERATION WITH
C                           E. SOUKHOVITSKI, E-MAIL: chiba.satoshi@jaea.go.jp
C
C      ROBERTO CAPOTE     - DISPERSIVE OPTICAL MODEL POTENTIAL RELATIONS,
C                           LANE CONSISTENNCY AND (P,N) CS,RIPL INTERFACE,
C                           E-MAIL: R.CapoteNoy@iaea.org
C
C      JOSE M. QUESADA    - DISPERSIVE OPTICAL MODEL POTENTIAL RELATIONS,
C                           LANE CONSISTENCY,E-MAIL: quesada@us.es
C
C      MAIN REFERENCES:   1. E.SH. SOUKHOVITSKII, S. CHIBA, R. CAPOTE, JOSE M.
C                            QUESADA, S. KUNIEDA and G.B. MOROGOVSKII, TECHNICAL 
C                            REPORT,JAEA-DATA/CODE 2008-025, JAPAN ATOMIC ENERGY
C                            AGENCY, 2008.
C                         2. E.SH. SOUKHOVITSKII, S. CHIBA, O.IWAMOTO, K.SHIBATA
C                            T. FUKAHORI and G.B. MOROGOVSKII, TECHNICAL REPORT,
C                            JAERI-DATA/CODE 2005-002, JAPAN ATOMIC ENERGY
C                            INSTITUTE, 2005.
C                         3. E.SH. SOUKHOVITSKII, R. CAPOTE, J.M. QUESADA,
C                            S. CHIBA, PHYS. REV. C72, 024604 (2005).
C                         4. J.M. QUESADA, R. CAPOTE, E.Sh. SOUKHOVITSKI and
C                            S. CHIBA, PHYS. REV. C76, 057602 (2007).
C                         5. MORE DETAILS, MANUAL,CODE'S SOURCE FILES AND INPUTS
C                            WITH VERIOUS CC OPTICAL POTENTIALS CAN BE FOUND ON: 
C                            http://www-nds.iaea.org/RIPL-3/
C
C
C  ****************************************************************
      PROGRAM OPTMAN14
C     SUBROUTINE OPTMAN14(fname)
C  *************************************************************
      CHARACTER*20 fname 
      COMMON/INOUT/fname
      CHARACTER*80 TITLE 

      COMMON/MENU/MEJOB,MEPOT,MEHAM,MECHA,MEPRI,MESOL,MESHA,MESHO,MEHAO
     *,MEAPP,MEVOL,MEREL,MECUL,MERZZ,MERRR,MEDIS,MERIP

      REAL*16 LFA(400),dtmp  
      DOUBLE PRECISION A
      COMMON/LOFAC/A(800)

C----------------------------------------------------------------------------
C----------------------------------------------------------------------------
C     FACTORIAL CALCULATION AVOIDING A LONG DATA STATEMENT (common /LOFAC/A)
C            but keeping the same precision
C   (a long data statement was producing errors/warnings with some compilers)
C
      DO i = 1, 800
        A(i) = 0.d0
      ENDDO
      LFA(1) = 0
      DO i = 2, 400
        dtmp = i 
        LFA(i) = LOG(dtmp) + LFA(i - 1)
      ENDDO
      DO j = 6, 800
        if(mod(j,2).eq.0) A(j)=LFA(j/2 - 1)  
C       if(j.lt.15) write(*,*) j,A(j) 
      ENDDO
C
C     FORMER BLOCK DATA VALUES
C     DATA A/0.d0,0.d0,0.d0,0.d0,
C    *0.,.693147180559945309417232D+00,0.,.179175946922805500081248D+01,
C    *0.,.317805383034794561964694D+01,0.,.478749174278204599424770D+01,
C    *0.,.657925121201010099506018D+01,0.,.852516136106541430016553D+01,
C    *0.,.106046029027452502284172D+02,0.,.128018274800814696112077D+02,

C----------------------------------------------------------------------------
C----------------------------------------------------------------------------


C---- EMPIRE related i/o changes ----------------------
C     CALL getarg(1,fname)
c     if(fname(1:1).eq.'') fname='ecis06'
C     write(*,*) 'Calc OPTMAN with ', trim(fname)//'.inp'
C     open(unit=20,file=TRIM(fname)//'.inp',STATUS='OLD',ERR=100)

      fname='ecis06'
      open(unit=20,file='OPTMAN.INP',STATUS='OLD',ERR=100)
      read(20,*,END=100,ERR=100) cline
      rewind 20
      open(unit=21,file='OPTMAN.OUT')
      open(unit=323,file='OPTMAN.DAT')

      WRITE(21,'(5x,A)')
     *  '***********************************************'
      WRITE(21,'(5x,A)')
     *  '*      CODE OPTMAN VERSION 14 ( Dec 2013)     *'
      WRITE(21,'(5x,A)')
     *  '*  DISPERSIVE RELATIONS AND LANE CONSISTENCY  *'
      WRITE(21,'(5x,A)')
     *  '*      LANE CONSISTENT COULOMB CORRECTION     *'
      WRITE(21,'(5x,A)')
     *  '*            GLOBAL POTENTIAL SEARCH          *'
      WRITE(21,'(5x,A)')
     *  '*  OTHER NON-AXIAL BANDS LEVELS COUPLING      *'
      WRITE(21,'(5x,A)')
     *  '*     OPTION USING AXIAL ROTATIONAL MODEL     *'
      WRITE(21,'(5x,A)')
     *  '*    POTENTIAL MULTIPOLES, ANALYZING POWERS   *'
      WRITE(21,'(5x,A)')
     *  '*---------------------------------------------*'
      WRITE(21,'(5x,A)')
     *  '*    COMPATIBLE WITH THE EMPIRE-3.2 SYSTEM    *'
      WRITE(21,'(5x,A)')
     *  '***********************************************'

      CALL THORA(21)

      READ (20,'(A80)') TITLE
      WRITE(21,*) trim(TITLE)
      READ(20,1)MEJOB,MEPOT,MEHAM,MEPRI,MESOL,MESHA,MESHO,
     *                MEHAO,MEAPP,MEVOL,MEREL,MECUL,MERZZ,MERRR,
     *                MEDIS,MERIP

C     FOR EMPIRE OPERATION MEPRI IS SET TO 99
      MEPRI=99
                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                               
    1 FORMAT(20I2)
    4 FORMAT(20A4)
C#1   MEJOP 1-JOB STANDARD*2-JOB WITH POTENTIAL OPTIMIZATION.
C#2   MEPOT 1-POT-AL OF ROT.MODEL  YL0* 2-POT-AL EXPANDED BY BETTA
C#3   MEHAM 1-RM* 2-VM* 3-DCHM* 6-5PARM* 4-FDM* 5-5PAR0M* 7-COUPL.GB
C#4   MEPRI OUTPUT MANAGEMENT * 0-MINIMUM (99 = EMPIRE I.E. NO SCREEN OUTPUT AND LIMITED FILE OUTPUT
C           OUTPUT MANAGEMENT * 99- FOR EMPIRE I.E. NO SCREEN OUTPUT AND LIMITED FILE OUTPUT
C#5   MESOL 1-OPTIMIZED 2-EXACT >3-ITERATION METHODS
C           3-ZERO APPROX..-SPHERICAL OPTICAL MODEL
C          >3-ZERO APPROX. HAS THIS NUMBER OF COUPLED EQ.
C#6   MESHA 1-QUADR*2-+HEXAD.AXIAL.* 3-+HEXAD.N.AXIAL. DEF BY  GAM.
C     *4-COMMON CASE.
C#7   MESHO 0-NO *1-AXIAL.*2-NON-AXIAL OCTUPOLE DEFORMATION
C#8   MEHAO 0-NO *1-CONSIDER. OF OCT OSC. * 2-SIMMET. OCTUPOLE OSC. ScaLED BY \beta^2
C           3-2-SIMMET. OCTUPOLE OSC. NOT SCALED BY \beta^2
C#9   MEAPP 0-EXECT SOLUTION; *1-QUICK SOLUTION WITHOUT LEVEL'S  POTENTIAL DEPENDANCE
C           2- The most common case
C#10  MEVOL 0-STANDARD SOLUTION; *>1- ACCOUNT OF NUCLEAR VOLUME
C     CONSERVATION
C#11  MEREL 0-STANDARD SOLUTION;
C           1-ACCOUNT OF RELATIVISTIC KINEM AND POTEN. DEPENDENCE
C           2-ACCOUNT OF RELATIVISTIC KINEMATICS
C           3-ACCOUNT OF RELATIVISTIC KINEM AND REAL POTEN. DEPENDENCE
C#12  MECUL 0-COULOMB CORRECTION PROPORTIONAL TO DERIVATIVE OF REAL PORENTIAL
C           1-CONSTANT
C           2-LANE CONSISTANT COULOMB CORRECTION NUCLEAR POTENTIAL ENERGY FOR PROTONS EQUAL TO
C           INCIDENT ENERGY - CDE, APPLIED TO BOTH REAL AND UMAGINARY POTENTIAL
C           3-LANE CONSISTANT COULOMB CORRECTION NUCLEAR POTENTIAL ENERGY FOR PROTONS EQUAL TO
C           INCIDENT ENERGY - CDE, APPLIED TO REAL POTENTIAL ONLY
C#13  MERZZ 0-CHARGE RADIUS -CONSTANT
C           1-ENERGY DEPENDENT
C#14  MERRR 0-REAL RADIUS IS ENERGY INDEPENDENT
C           1-REAL RADIUS IS ENERGY DEPENDENT
C#15  MEDIS 0-WITHOUT ACCOUNT OF DISPERSION RELATIONS BETWEEN REAL AND IMAGINARY POTENTIALS
C           1-ACCOUNT OF DISPERSION RELATIONS BETWEEN REAL AND IMAGINARY POTENTIALS
C           2-ACCOUNT OF DISPERSION RELATIONS BETWEEN REAL AND IMAGINARY POTENTIALS,
C           EXCLUDING SPIN ORBIT POTENTIAL
C#16  MERIP 0-IN ABCT READS POTENTIAL ONE TIME WITH ANALITICAL DEPENDENCIES FOR ALL ENERGIES
C           1-ABCT READS POTENTIAL BLOCKS FOR EACH ENERGY TO USE RIPL COMPILED INPUTS
C
C
C     OMP CALCULATION
C
      CALL ABCT

      CLOSE(323)
      CLOSE(20)
      CLOSE(21)
C
C     RETURN
C
      STOP
100   CLOSE(20,STATUS='DELETE')
      END
C     *******************************************************
      SUBROUTINE ABCT
C     *******************************************************
      IMPLICIT DOUBLE PRECISION(A-H,O-Z) 
      CHARACTER*4 ITEXT,IMOD,IPOT,MPOT,IFOR1,IFOR2,
     *IFOR3,ISMOD,IFOR4,IHMOD,IFOR5           

      CHARACTER*1  cpar
      CHARACTER*20 fname 
      COMMON/INOUT/fname

C     CHARACTER*8 centmp

      LOGICAL*1 unformat/.TRUE./ 
C     LOGICAL*1 unformat/.FALSE./

      DIMENSION ITEXT(3),IMOD(7),IPOT(7),MPOT(2),IFOR1(2),IFOR2(9),
     *IFOR3(3),ISMOD(2),IFOR4(5),IHMOD(3),IFOR5(4)
      COMMON/DISK/TET(150),MTET
      COMMON/DISCAN/DISC(40,150),PL(180,150),COEF(40,180),LKK
     */COUL/CONZ,ETA,COPH(40,90)
     */COULCO/COEFR(90),COEFI(90)
      COMMON/CSB/CST,CSR,NST
     */ENA/EN,EL(40),BET(10),NUR,NMAX,NPD,LAS
     */POT1/VR3,VR2,VR1,VR0,WC1,WC0,WD1,WD0,VS,AC0,AR0,AW0,AD0,AS0
     */POT2/AR1,AC1,AW1,AD1,AS1
     */POT3/BNDC,WDA1,WCA1,CCOUL,CISO,WCISO,WS0,WS1
     */POTD/ALAVR,VRLA,WCBW,WCWID,WDBW,WDWID,ALAWD,EFERMN,EFERMP,ALASO
     *,PDIS,WSBW,WSWID,RRBWC,RRWID,RZBWC,RZWID
      COMMON/RAD/RR,RC,RD,RW,RS,AR,AC,AW,AD,AS,ALF,AT,ANEU,RZ,ZNUC,ASP
     *,AZ
     */QNSB/INC(180),INR(180),JS(180)
      COMMON/JNN/CSS,INCC,NCLL,NSS,NJ,INCR
      COMMON/POTB/WNK(40),WN(40),VR,WC,WD
      COMMON/QNB/JO(40),NPO(40),KO(40),NCA(40)
     */QNBBAND/NUMB(40),BETB(40) 
     */QNBCAL/ELC(40),BETBC(40),JOC(40),NP0C(40),KOC(40),NUMBC(40),
     *NCAC(40),NTUC(40),NNBC(40),NNGC(40),NNOC(40),NPOC(40)
     */ALFA/AIGS(40),AGSIC(40)
      COMMON/MENU/MEJOB,MEPOT,MEHAM,MECHA,MEPRI,MESOL,MESHA,MESHO,MEHAO
     *,MEAPP,MEVOL,MEREL,MECUL,MERZZ,MERRR,MEDIS,MERIP
     */ENB/EE(50),MCHAE(50)
     */NU/NUI,NUF
     */CS1/CSN(40),CM(40)
     */SF12/SF0,SF1,SF2
     */SHEMM/ES(40),JJ(40),NTU(40),NNB(40),NNG(40),NNO(40),
     *NPI(40)
     */SHEM1/HW,AMG0,AMB0,GAM0,BET0,BET4,GAMDE,GSHAPE
     */INRM/AMO,BMO,CMO,BB42,GAMG,DELG
     */SHAMO/BET3,ETO,AMUO,HWO,BB32,DPAR
     */NCLMA/LLMA,NCMA,NSMA,KODMA
     */DISPE/VD,VRDC,EA,WDISO
      COMMON/DISPE2/VRD,WDSHI,WDWID2,ALFNEW
     */RESONI/ERN(10),GNN(10),GREN(10),LON(10),JMN(10),JCON(10),NEL(10)
     *,NRESN
     */AP/KEYAP
      DATA ITEXT,IMOD/4HHAMI,4HLTON,4HIAN ,4H RV ,4H VM ,4HDCHM,
     *4H FDM,4H5PA0,4H 5PM,4HCLGB/,
     *IPOT,MPOT/4HPOTE,4HNTIA,4HL EX,4HPAND,4HED  ,4HBY  ,
     *4H    ,4HYL0 ,4HBET0/,IFOR1,IFOR2,IFOR3/4HWITH,4H AC.,
     *4HAXIA,4HL HE,4HXADE,4HCAPO,4HLE D,4HEFOR,4HMATI,4HONS ,
     *4H    ,4H    ,4H|NON,4H NON/,
     *ISMOD,IFOR4,IHMOD,IFOR5/4H    ,4H NON,4HAXIA,4HL OC,4HTUP0,
     *4HLE  ,4H    ,4HRID.,4HSOFT,4HSOFT,4H    ,4HDEFO,4HRMAT,4HIONS /

      IF(MEHAM.GT.1) READ(20,2)HW,AMB0,AMG0,GAM0,BET0,BET4,
     *BB42,GAMG,DELG,
     *BET3,ETO,AMUO,HWO,BB32,GAMDE,DPAR,GSHAPE
 
C=======================================================================
C     LLMA-MAXIMUM MOMENTUM L
C     NCMA-MAXIMUM NUMBER OF COUPLED EQ.
C     NSMA-NUMBER OF SYSTEMS WITH  J AND PARITY
C     KODMA-SWITCH: 0-COUPLED STATES ARE ORDERED ONE BY ONE, NO MORE
C                   THAN NCMA
C                 1:-COUPLED STATES ARE ORDERED BY GROWING MOMENTUM L
C                    NO MORE THAN NCMA
C=======================================================================

           READ(20,211)NUR,NST,NPD,LAS,MTET,LLMA,NCMA,NSMA,KODMA
           
  211 FORMAT(20I3)
      IF(LLMA.EQ.0.OR.LLMA.GT.89) LLMA=89
      IF(NCMA.EQ.0.OR.NCMA.GT.200) NCMA=200
      IF(NSMA.EQ.0.OR.NSMA.GT.180) NSMA=180
            if(NST.LT.0) THEN
                 OPEN(99,FILE='OMINPUT.INP')
                 READ(99,*) NST
                 READ(99,*,END=212)(EE(I),I=1,NST)
  212            CLOSE(99)
C             WRITE(*,*) NST
              DO I=1,NST
C              WRITE(*,*) EE(I)
              MCHAE(I)=0
              ENDDO
            ELSE
             READ(20,2)(EE(I),I=1,NST)
             READ(20,1)(MCHAE(I),I=1,NST)                          
            ENDIF
      IF(MTET.EQ.0) GO TO 13
           READ(20,2)(TET(I),I=1,MTET)   
   13 IF(MEHAM.GT.1) GO TO 16
           READ(20,3)(ELC(I),JOC(I),NPOC(I),KOC(I),NCAC(I),
     *     NUMBC(I),BETBC(I),AGSIC(I),I=1,NUR)
        GO TO 117
   16      READ(20,43)(ELC(I),JOC(I),NPOC(I), NTUC(I),NNBC(I),NNGC(I),
     *                 NNOC(I),NCAC(I),I=1,NUR)
C====================================================================
C     VR=VR0+VR1*EN+VR2*EN*EN      AR=AR0+AR1*EN
C===================================================================
C                WD=WD0+WD1*EN     AD=AD0+AD1*EN
C     EN<BNDC    WC=WC0+WC1*EN     AC=AC0+AC1*EN
C ====================================================================
C                WD=WD0+WD1*BNDC+(EN-BNDC)*WDA1
C     EN>BNDC    WC=WC0+WC1*BNDC+(EN-BNDC)*WCA1
C                AD=AD0+AD1+BNDC
C====================================================================
        
  117 READ(20,211)NRESN
      IF(NRESN.EQ.0) GO TO 17      
           READ(20,213)(ERN(I),GNN(I),GREN(I),
     *         LON(I),JMN(I),JCON(I),NEL(10),I=1,NRESN)     
  213      FORMAT(3E12.6,4I3)  

   17      READ(20,2)ANEU,ASP,AT,ZNUC,EFERMN,EFERMP
           READ(20,2)VR0,VR1,VR2,VR3,VRLA,ALAVR,
     *               WD0,WD1,WDA1,WDBW,WDWID,ALAWD,
     *               WC0,WC1,WCA1,WCBW,WCWID,BNDC,
     *               VS,ALASO,WS0,WS1,WSBW,WSWID,
     *               RR,RRBWC,RRWID,PDIS,AR0,AR1,
     *               RD,AD0,AD1,RC,AC0,AC1,
     *               RW,AW0,AW1,RS,AS0,AS1,
     *               RZ,RZBWC,RZWID,AZ,CCOUL,ALF,
     *               CISO,WCISO,WDISO,EA,WDSHI,WDWID2,
     *               ALFNEW,VRD,CAVR,CARR,CAAR,CARD,
     *               CAAC,ATI
     
     
      
C      !!!!!!!!COMPARING WITH DATET YOU NEED TO INPUT ATI - REFERENCE NUCLEI NUMBER IN ABCT !!!!      
      IF(MEPRI.NE.99) PRINT 500,ASP,AT
      WRITE(21,500)ASP,AT
  500 FORMAT( 7X,'INTERACTION OF PARTICLE, HAVING SPIN =',F5.2/19X,
     *'WITH NUCLEI',2X,'A=',F12.7/20X,'COUPLED CHANNELS METHOD')
      MESHH=MESHA-1
      IF(MEREL.EQ.0 .AND. MEPRI.NE.99) PRINT 134
      IF(MEREL.EQ.0) WRITE(21,134)
  134 FORMAT(22X,'NEWTON KINEMATICS')
      IF(MEREL.EQ.1 .AND. MEPRI.NE.99) PRINT 135
      IF(MEREL.EQ.1) WRITE(21,135)
  135 FORMAT(5X,'RELATIVISTIC KINEMATICS AND POTENTIAL DEPENDENCE')
      IF(MEREL.EQ.2 .AND. MEPRI.NE.99) PRINT 136
      IF(MEREL.EQ.2) WRITE(21,136)
  136 FORMAT(20X,'RELATIVISTIC KINEMATICS')
      IF(MEREL.EQ.3 .AND. MEPRI.NE.99) PRINT 137
      IF(MEREL.EQ.3) WRITE(21,137)
  137 FORMAT(3X,'RELATIVISTIC KINEMATICS AND REAL POTENTIAL DEPENDENCE')
C
      IF(MEDIS.EQ.0 .AND. MEPRI.NE.99) PRINT 184
      IF(MEDIS.EQ.0) WRITE(21,184)
  184 FORMAT(6X,'OPTICAL POTENTIAL WITHOUT DISPERSIVE RELATIONSHIPS')
      IF(MEDIS.GE.1 .AND. MEPRI.NE.99) PRINT 185
      IF(MEDIS.GE.1) WRITE(21,185)
  185 FORMAT(6X,'OPTICAL POTENTIAL WITH THE DISPERSIVE RELATIONSHIPS')
C
      IF(MECUL.EQ.0 .AND. MEPRI.NE.99) PRINT 154
      IF(MECUL.EQ.0) WRITE(21,154)
  154 FORMAT(5X,'COULOMB CORRECTION PROPORTIONAL REAL POTENTIAL DER-VE')
      IF(MECUL.EQ.1 .AND. MEPRI.NE.99) PRINT 155
      IF(MECUL.EQ.1) WRITE(21,155)
  155 FORMAT(15X,' COULOMB CORRECTION IS CONSTANT')
      IF(MECUL.EQ.2 .AND. MEPRI.NE.99) PRINT 156
      IF(MECUL.EQ.2) WRITE(21,156)
  156 FORMAT(/7X,' LANE CONSISTENT, EFFECTIVE PROTON ENERGY = E-CME,'/
     *13X,'BOTH FOR REAL AND IMAGINARY POTENTIALS'/)
      IF(MECUL.EQ.3 .AND. MEPRI.NE.99) PRINT 157
      IF(MECUL.EQ.3) WRITE(21,157)
  157 FORMAT(/7X,' LANE CONSISTENT, EFFECTIVE PROTON ENERGY = E-CME,'/
     *20X,' FOR REAL POTENTIAL ONLY'/)
C
      IF(MERZZ.EQ.0 .AND. MEPRI.NE.99) PRINT 164
      IF(MERZZ.EQ.0) WRITE(21,164)
  164 FORMAT(22X,'CHARGE RADIUS IS CONSTANT')
      IF(MERZZ.EQ.1 .AND. MEPRI.NE.99) PRINT 165
      IF(MERZZ.EQ.1) WRITE(21,165)
  165 FORMAT(15X,' CHARGE RADIUS IS ENERGY DEPENDENT')
C
      IF(MERRR.EQ.0 .AND. MEPRI.NE.99) PRINT 174
      IF(MERRR.EQ.0) WRITE(21,174)
  174 FORMAT(22X,'REAL RADIUS IS CONSTANT')
      IF(MERRR.EQ.1 .AND. MEPRI.NE.99) PRINT 175
      IF(MERRR.EQ.1) WRITE(21,175)
  175 FORMAT(15X,' REAL RADIUS IS ENERGY DEPENDENT')
C
      IF(MESHA.GT.1 .AND. MEPRI.NE.99) PRINT 51,IFOR1,IFOR3(MESHH),IFOR2
      IF(MESHA.GT.1) WRITE(21,51)IFOR1,IFOR3(MESHH),IFOR2
   51 FORMAT(10X,14A4)
      IF(MEPRI.NE.99) PRINT 50,ITEXT,IMOD(MEHAM),IPOT,MPOT(MEPOT)
      WRITE (21,50)ITEXT,IMOD(MEHAM),IPOT,MPOT(MEPOT)
   50 FORMAT(/10X,4A4,6X,8A4)
      MEHA1=MEHAO+1
      IF(MESHO.GT.0 .AND. MEPRI.NE.99) 
     >  PRINT 45, IFOR1,ISMOD(MESHO),IFOR4,IHMOD(MEHA1),IFOR5
      IF(MESHO.GT.0) WRITE(21,45) IFOR1,ISMOD(MESHO),IFOR4,IHMOD(MEHAO),
     *IFOR5
   45 FORMAT(10X,13A4)
      IF(MEPRI.NE.99) PRINT 100,NUR,NPD,LAS
      WRITE(21,100)NUR,NPD,LAS
      IF(MEHAM.GT.1) GO TO 18
      IF(MEPRI.NE.99) PRINT 501,(I,ELC(I),JOC(I),NPOC(I),KOC(I),NCAC(I),
     *NUMBC(I),BETBC(I),AGSIC(I),I=1,NUR)
      WRITE(21,501)(I,ELC(I),JOC(I),NPOC(I),KOC(I),NCAC(I),NUMBC(I),
     *BETBC(I),AGSIC(I),I=1,NUR)
      GO TO 19
   18 IF(MEPRI.NE.99) PRINT 20,(I,ELC(I),JOC(I),NTUC(I),NNBC(I),NNGC(I),
     *NNOC(I),NPOC(I),NCAC(I),I=1,NUR)
      WRITE(21,20)(I,ELC(I),JOC(I),NTUC(I),NNBC(I),NNGC(I),NNOC(I),
     *NPOC(I),NCAC(I),I=1,NUR)
      IF(MEPRI.NE.99) PRINT 21,HW,AMB0,AMG0,GAM0,BET0,BET4,BB42,GAMG,
     *DELG,BET3,ETO,AMUO,HWO,BB32,GAMDE,DPAR,GSHAPE
      WRITE(21,21)HW,AMB0,AMG0,GAM0,BET0,BET4,BB42,GAMG,DELG
     *,BET3,ETO,AMUO,HWO,BB32,GAMDE,DPAR,GSHAPE
   21 FORMAT(/22X,'PARAMETERS OF HAMILTONIAN '/5X,'HW=',F12.5,3X,
     *'AMB0=',F 8.5,3X,'AMG0=',F 8.5,3X,'GAM0=',F 8.5,3X,
     *'BET0=',F 8.5/
     *5X,'BET4=',F10.5,3X,'BB42=',F8.5,3X,'GAMG=',F8.5,3X,
     *'DELG=',F8.5/
     *5X,'BET3=',F10.5,3X,'ETO=',F9.5,3X,'AMUO=',F8.5,3X,
     *'HWO=',F8.5,4X,'BB32=',F8.5,3X/
     *5X,'GAMDE=',F9.5,3X,'DPAR=',F8.4,3X,'GSHAPE=',F8.5//)

    
      
   20 FORMAT(//16X,'ENERGY ',4X,'LEVEL''S SPIN*2',4X,'NTU  ',
     *6X,'NNB  ', 6X,'NNG', 9X,'NNO',9X,'NPO',9X,'NCA'//
     *(1X,I4,8X,E14.7,7I11))
  501 FORMAT(//16X,'ENERGY',5X,'LEVEL''S SPIN*2',3X,'PARITY',10X,
     *'BAND*2',10X,'NCA',8X,'NUMB',9X,'BETB',11X,'ALFA(I)-> GS'/
     */(1X,I4,6X,E12.5,I11,I14,I15,I15,I11,2E19.5))
 100  FORMAT( /15X,'NUMBER OF COUPLED LEVELS=',I3,5X,'NPD =',I2/14X,
     *'NUMBER OF TERMS IN POTENTIAL EXPANSION= ',2X,I2)
  19  IF(MEPRI.NE.99) PRINT 90
      WRITE(21,90)
  90  FORMAT(/15X,'POTENTIAL   PARAMETERS   V(R)')
      IF(MEPRI.NE.99) PRINT 91,VR0,VR1,VR2,RR,AR0,AR1,WD0,WD1,VR3,RD,
     *AD0,AD1,WC0,WC1,RC,AC0,AC1,RW,AW0,AW1,VS,RS,AS0,AS1,ALF,ANEU,RZ,
     *AZ,BNDC,WDA1,WCA1,CCOUL,CISO,WCISO,WS0,WS1,VRLA,ALAVR,
     *WCBW,WCWID,WDBW,WDWID,ALAWD,EFERMN,EFERMP,ALASO,PDIS,
     *WSBW,WSWID,RRBWC,RRWID,RZBWC,RZWID,EA,WDISO,WDSHI,WDWID2,
     *ALFNEW,VRD,CAVR,CARR,CAAR,CARD,CAAC
      WRITE(21,91)VR0,VR1,VR2,RR,AR0,AR1,WD0,WD1,VR3,RD,AD0,AD1,
     *WC0,WC1,RC,AC0,AC1,RW,AW0,AW1,VS,RS,AS0,AS1,ALF,ANEU,RZ,
     *AZ,BNDC,WDA1,WCA1,CCOUL,CISO,WCISO,WS0,WS1,VRLA,ALAVR,
     *WCBW,WCWID,WDBW,WDWID,ALAWD,EFERMN,EFERMP,ALASO,PDIS,
     *WSBW,WSWID,RRBWC,RRWID,RZBWC,RZWID,EA,WDISO,WDSHI,WDWID2,
     *ALFNEW,VRD,CAVR,CARR,CAAR,CARD,CAAC


   91 FORMAT(/1X,'VR0=',F7.3,5X,'VR1=',F7.4,5X,'VR2=',F10.7,2X,
     *'RR=',F7.4,5X,'AR0=',F7.4,5X,'AR1=',F7.4
     */1X,'WD0=',F7.4,5X,'WD1=',F7.4,5X,'VR3=',F10.7,2X,
     *'RD=',F7.4,5X,'AD0=',F7.4,5X,'AD1=',F7.4
     */1X,'WC0=',F7.4,5X,'WC1=',F7.4,21X,
     *'RC=',F7.4,5X,'AC0=',F7.4,5X,'AC1=',F7.4
     */49X,'RW=',F7.4,5X,'AW0=',F7.4,5X,'AW1=',F7.4
     */1X,'VSO=',F7.4,37X,'RS=',F7.4,5X,'AS0=',F7.4,5X,'AS1=',F7.4
     */1X,'ALF=',F7.4,5X,'ANEU=',F7.4,20X,'RZ=',F7.4,5X,'AZ0=',F7.4,
     */1X,'BNDC=',F7.2,4X,'WDA1=',F7.4,4X,'WCA1=',F7.4,4X,'CCOUL=',F7.4
     *,5X,'CISO=',F7.3,4X,'WCISO=',F7.3
     */1X,'WS0=',F7.4,5X,'WS1=',F7.4,5X,'VRLA=',F7.4
     *,4X,'ALAVR=',F8.5,4X,'WCBW=',F7.4,4X,'WCWID=',F7.4,/1X,'WDBW='
     *,F7.4,4X,'WDWID=',F7.4,3X,'ALAWD=',F7.4
     *,3X,'EFERMN=',F7.3,4X,'EFERMP=',F7.3,2X,'ALASO=',F7.4,
     */1X,'PDIS=',F7.4,4X,'WSBW=',F7.4,4X,'WSWID=',F7.2,3X,'RRBWC=',F7.4
     *,5X,'RRWID=',F6.2,4X,'RZBWC=',F7.4,
     */1X,'RZWID=',F7.4,3X,'EA=',F9.5,4X,'WDISO=',F7.3,
     *3X,'WDSHI=',F7.2,5X,'WDWID2=',F7.2,2X,'ALFNEW=',F6.3,
     */1X,'VRD=',F8.3,4X,'CAVR=',F8.5,3X,'CARR=',F9.6,
     *2X,'CAAR=',F9.6,4X,'CARD=',F9.6,2X,'CAAC=',F9.6/)

      IF(MEPRI.NE.99) PRINT 133,ZNUC,ATI
      WRITE(21,133) ZNUC,ATI
 133  FORMAT(/10X,'NUCLEUS CHARGE = ',F7.4,5x,
     *            'REFFERENCE NUCLEUS   MASS = ',F7.3/)
           IF(MEHAM.GT.1) GO TO 8
           IF(NPD.EQ.0) GO TO 8
           READ(20,2)(BET(I),I=2,NPD,2)
      IF(MEPRI.NE.99) PRINT 96,(I,BET(I),I=2,NPD,2)
      WRITE(21,96)(I,BET(I),I=2,NPD,2)
  96  FORMAT(6X,'NPD',5X,'DEFORMATION PARAMETER VALUES'/
     *(6X,I2,13X,F7.4))
    1 FORMAT(36I2)
    2 FORMAT(6E12.7)
    3 FORMAT(E12.7,5I2,2E12.7)
   43 FORMAT(E12.7,7I2)
    8 ASQ=AT**(1./3.)
    
      itmp = NINT(AT)-NINT(ATI)   
      VRLA=VRLA+CAVR*itmp
      RR=(RR+CARR*itmp)*ASQ
      RC=RC*ASQ
      RD=(RD+CARD*itmp)*ASQ
      RW=RW*ASQ
      RS=RS*ASQ
      RZ=RZ*ASQ       
      AR0=AR0+CAAR*itmp
      AC0=AC0+CAAC*itmp
      KOD=KODMA
      
      CSN = 0.d0
      DO 4 II=1,NST
                     IF(II.EQ.1) GO TO 678
                     IF(MERIP.NE.1) GO TO 678
                     READ(20,*)      
                     READ(20,2)VR0,VR1,VR2,VR3,VRLA,ALAVR,
     *               WD0,WD1,WDA1,WDBW,WDWID,ALAWD,
     *               WC0,WC1,WCA1,WCBW,WCWID,BNDC,
     *               VS,ALASO,WS0,WS1,WSBW,WSWID,
     *               RR,RRBWC,RRWID,PDIS,AR0,AR1,
     *               RD,AD0,AD1,RC,AC0,AC1,
     *               RW,AW0,AW1,RS,AS0,AS1,
     *               RZ,RZBWC,RZWID,AZ,CCOUL,ALF,
     *               CISO,WCISO,WDISO,EA,WDSHI,WDWID2,
     *               ALFNEW,VRD,CAVR,CARR,CAAR,CARD,
     *               CAAC 
      WRITE(21,91)VR0,VR1,VR2,RR,AR0,AR1,WD0,WD1,VR3,RD,AD0,AD1,
     *WC0,WC1,RC,AC0,AC1,RW,AW0,AW1,VS,RS,AS0,AS1,ALF,ANEU,RZ,
     *AZ,BNDC,WDA1,WCA1,CCOUL,CISO,WCISO,WS0,WS1,VRLA,ALAVR,
     *WCBW,WCWID,WDBW,WDWID,ALAWD,EFERMN,EFERMP,ALASO,PDIS,
     *WSBW,WSWID,RRBWC,RRWID,RZBWC,RZWID,EA,WDISO,WDSHI,WDWID2,
     *ALFNEW,VRD,CAVR,CARR,CAAR,CARD,CAAC 
      
      ASQ=AT**(1./3.)
      RR=RR*ASQ
      RC=RC*ASQ
      RD=RD*ASQ
      RW=RW*ASQ
      RS=RS*ASQ
      RZ=RZ*ASQ    

  678 EN=EE(II)

C     write(centmp,'(I8.8)') NINT(EN*1E6)   

C     EMPIRE FILES FOR CROSS SECTIONS, TLs 
C     ecis06 format, printout of TLs,LEG and ANG distributions
C     open(unit=93,file=TRIM(fname)//centmp//'.CS')
C     open(unit=98,file=TRIM(fname)//centmp//'.ICS')
C     open(unit=92,file=TRIM(fname)//centmp//'.TLJ')
C     open(unit=96,file=TRIM(fname)//centmp//'.LEG')
C     open(unit=97,file=TRIM(fname)//centmp//'.ANG')
C     open(unit=327,file=TRIM(fname)//centmp//'.ana')
C     open(unit=328,file=TRIM(fname)//centmp//'.pol')

      open(unit=93,file=TRIM(fname)//'.cs')
      open(unit=98,file=TRIM(fname)//'.ics')
      IF(unformat) then
        open(unit=92,file=TRIM(fname)//'.tlj',form='unformatted')       
      ELSE
        open(unit=92,file=TRIM(fname)//'.tlj')
      ENDIF
      open(unit=96,file=TRIM(fname)//'.leg')
      open(unit=97,file=TRIM(fname)//'.ang')
      open(unit=327,file=TRIM(fname)//'.ana')
      open(unit=328,file=TRIM(fname)//'.pol')
    
  777 MECHA=MCHAE(II)
  
C     CREATING LEVELS FOR (P,N) ANALOG STATES CALCULATIONS
     
      NURRR=NUR
            
      NURC=0
      IF(MEHAM.GT.1) GO TO 638
      DO 601 I=1, NUR
      IF(MECHA.EQ.0.AND. NCAC(I).NE.NCAC(1)) GO TO 601
      NURC=NURC+1
      EL(NURC)=ELC(I)
      JO(NURC)=JOC(I)
      NPO(NURC)=NPOC(I)
      KO(NURC)=KOC(I)
      NCA(NURC)=NCAC(I)
      NUMB(NURC)=NUMBC(I)
      BETB(NURC)=BETBC(I)
      AIGS(NURC)=AGSIC(I)
  601 CONTINUE
      NUR=NURC
      
      GO TO 639
  638 DO 602 I=1, NUR
      IF(MECHA.EQ.0.AND. NCAC(I).NE.NCAC(1)) GO TO 602
      NURC=NURC+1
      EL(NURC)=ELC(I)
      JO(NURC)=JOC(I)
      NTU(NURC)=NTUC(I)
      NNB(NURC)=NNBC(I)
      NNG(NURC)=NNGC(I)
      NNO(NURC)=NNOC(I)      
      NPO(NURC)=NPOC(I)
      NCA(NURC)=NCAC(I)
  602 CONTINUE
      NUR=NURC

  639 CONTINUE    
             

      IF(MEHAM.GT.2) CALL PREQU 
  
  
       ANEU=1.008664924
       IF(MECHA.EQ.1) ANEU=1.007825032
       AMI=939.56536D0
       IF(MECHA.EQ.1) AMI=938.272029D0
       
       
       IF(MECHA.EQ.1.AND.IDINT(ASP+0.1D0).EQ.1) AMI=1875.612859D0 
       IF(MECHA.EQ.1.AND.IDINT(ASP+0.1D0).EQ.1) ANEU=2.013553212712D0 
       
      REL=(EN+AMI)/AMI
      IF(MEREL.EQ.0) REL=1. 
      ENC=EN*AT/(AT+ANEU*REL)
      DO 5 I1=1,NUR
      IF(ENC-EL(I1))6,6,5
    5 CONTINUE
      NMAX=NUR
      GO TO 7
    6 NMAX=I1-1
    7 CONTINUE
      KODMA=KOD
      IF(NMAX.LT.NUR) KODMA=0
      CALL RIPAT
      CALL ASFUT
      IF(MEHAM.GT.1) CALL KNCOE
      CALL QUANT
      IF(MTET.EQ.0) GO TO 15
      NUI=1
      NUF=NMAX
      
      KEYAP=1
      CALL ANPOW
      IF (MEPRI.LT.99) PRINT 300
      IF (MEPRI.LT.99) WRITE(21,300)
  300 FORMAT(/23X,'ANALYZING POWERS FOR SCATTERED PARTICLES'/)
      WRITE(327,'(F10.6,2I3)') EN,MTET,NMAX
      DO 314 M=1,MTET
      IF (MEPRI.LT.99) PRINT 11,TET(M),(DISC(K,M),K=1,NMAX)
      IF (MEPRI.LT.99) WRITE(21,11)TET(M),(DISC(K,M),K=1,NMAX)
      WRITE(327,111)TET(M),(DISC(K,M),K=1,NMAX)
  314 CONTINUE  
    
      KEYAP=2
      CALL ANPOW
      IF(MEPRI.LT.99) PRINT 338
      IF (MEPRI.LT.99) WRITE(21,338)
  338 FORMAT(/29X,'POLARIZATION FOR SCATTERED PARTICLES'/)
      WRITE(328,'(F10.6,2I3)') EN,MTET,NMAX
      DO 315 M=1,MTET
      IF (MEPRI.LT.99) PRINT 11,TET(M),(DISC(K,M),K=1,NMAX)
      IF (MEPRI.LT.99) WRITE(21,11)TET(M),(DISC(K,M),K=1,NMAX)
      WRITE(328,111)TET(M),(DISC(K,M),K=1,NMAX)      
  315 CONTINUE
      CALL DISCA
      IF(MEPRI.NE.99) PRINT 110
      IF (MEPRI.LT.99) WRITE(21,110)
  110 FORMAT(/23X,'ANGULAR DISTRIBUTIONS OF SCATTERED PARTICLES'/
     *'    ANGLES[deg]      gs          1st       ...      '  )
      IF(MEPRI.NE.99) THEN
        WRITE(27,'(F10.6,2I3)') EN,MTET,NMAX
        DO 14 M=1,MTET
          PRINT 11,TET(M),(DISC(K,M),K=1,NMAX)
          WRITE(21,11)TET(M),(DISC(K,M),K=1,NMAX)
          WRITE(27,111)TET(M),(DISC(K,M),K=1,NMAX)
   14   CONTINUE
  111   FORMAT(E13.6,(8E13.5),(12E13.5))
      ENDIF
   11 FORMAT(2X,F10.3,3X,(8E11.3),(12E11.3))
      DO 25 L=1,40
      IF(COEFR(L).EQ.0.AND.COEFI(L).EQ.0.) LCOUL=L-1
      IF(COEFR(L).EQ.0.AND.COEFI(L).EQ.0.) GO TO 26
   25 CONTINUE
   26 IF(MEPRI.NE.99) THEN
        PRINT 101, EN,LKK,LCOUL
        WRITE(26,101)EN,LKK,LCOUL
        WRITE(26,131)EN,CST,CSR,(CSN(K),K=1,NMAX)
  101 FORMAT (/3X,'EN=',F9.3,3X,'LKK=',I3,3X,'LCOUL=',I3)
        DO 9 N=1,NMAX
          PRINT 112,(COEF(N,L),L=1,LKK)
          WRITE(26,112)(COEF(N,L),L=1,LKK)
          IF(ETA.EQ.0.D0.OR.N.GT.1) GO TO 9
          PRINT 113, (COEFR(L),COEFI(L),L=1,LCOUL)
          WRITE(26,113) (COEFR(L),COEFI(L),L=1,LCOUL)
    9     CONTINUE
 112  FORMAT(/14X,'LEGENDRE COEFFICIENTS FOR  SCATTERED NUCLEONS'/
     *16X,'ANGULAR DISTRIBUTIONS - NUCLEAR AMPLITUDE'/(1X,6E15.7))
 113  FORMAT(/14X,'LEGENDRE COEFFICIENTS FOR  SCATTERED PROTONS'/
     *16X,'ANGULAR DISTRIBUTIONS - COULOMB AMPLITUDE'/(1X,6E15.7))
      ENDIF 
   15 IF(MECHA.NE.0) GO TO 102
C RCN
      IF(EN.LT.2.d0) THEN
        IF(MEPRI.NE.99) PRINT 12,EN,CST,CSN(1),CSR,CST-CSN(1),
     *    SQRT((CST-CSR)/0.125663706D0)
        WRITE(21,12)EN,CST,CSN(1),CSR,CST-CSN(1),
     *    SQRT((CST-CSR)/0.125663706D0)
      ELSE
        IF(MEPRI.NE.99) PRINT 120,EN,CST,CSN(1),CSR,CST-CSN(1)
        WRITE(21,120)EN,CST,CSN(1),CSR,CST-CSN(1)
      ENDIF
C RCN
      IF(MEPRI.NE.99) WRITE(25,33)EN
      GO TO 103

  102 IF(MEPRI.NE.99) PRINT 104,EN,CST,CSN(1),CSR,CST-CSN(1)
      WRITE(21,104)EN,CST,CSN(1),CSR,CST-CSN(1)

      IF(MEPRI.NE.99) WRITE(25,34)EN
  103 IF(MEPRI.NE.99) WRITE(23,131)EN,CST,CSN(1),CST-CSN(1),
     *                       CSR,(CSN(K),K=2,NURRR)
      WRITE(323,131)EN,CST,CSN(1),CST-CSN(1),CSR,(CSN(K),K=2,NURRR)
  131 FORMAT(1P24E14.5)
      IF(MEPRI.NE.99) THEN 
      DO 31 M=1,MTET
      PRINT 11,TET(M),(DISC(K,M),K=1,NMAX)
      WRITE(25,11)TET(M),(DISC(K,M),K=1,NMAX)
   31 CONTINUE
      ENDIF
   33 FORMAT(///1X,'NEUTRON ENERGY =',F10.6)
   34 FORMAT(///1X,'PROTON  ENERGY =',F10.6)
C     IF(MEPRI.NE.99) PRINT 130,(K,CSN(K),K=1,NMAX)
C     WRITE(21,130)(K,CSN(K),K=1,NMAX)
      IF(MEPRI.NE.99) PRINT 130,(K,EL(k),0.5*JO(k),
     *                cpar(NPO(k)),CSN(K),K=1,NMAX)
      WRITE(21,130)(K,EL(k),0.5*JO(k),cpar(NPO(k)),CSN(K),K=1,NMAX)
      IF(MEPRI.NE.99) PRINT 129,SF0,SF1,SF2
      WRITE(21,129)SF0,SF1,SF2
C RCN
C
C     CROSS SECTION FILES
C
      IF(ETA.EQ.0.D0) THEN
C       WRITE(93,'(10H<CROSS-S.>,F10.2,F10.5,F10.2,2I5)') 
        WRITE(93,1009) ANEU,EN,AT,NINT(0.5*JO(1)),3
      ELSE
C       WRITE(93,'(10H<CROSS-S.>,F10.2,F10.5,F10.2,2I5)') 
        WRITE(93,1009) ANEU,EN,AT,NINT(0.5*JO(1)),1
      ENDIF
 1009 FORMAT ('<CROSS-S.>',F10.2,1P,D20.8,0P,F10.2,2I5)                 RESU-604
 1010 FORMAT ('<INE.C.S.>',F10.2,1P,D20.8,0P,F10.2,2I5)                 RESU-605
C     WRITE(98,'(10H<INE.C.S.>,F10.2,F10.5,F10.2,2I5)') 
      WRITE(98,1010) ANEU,EN,AT,NINT(0.5*JO(1)),NMAX-1

      IF(ETA.EQ.0.D0) THEN
C
C       TOTAL
C       WRITE(93,'(1X,E14.8)') CST*1000.
        WRITE(93,1012) CST*1000.d0
 1012   FORMAT (1P,D12.5)                                               RESU-607
C
C       INELASTIC TO LEVELS
        DO K=2,NMAX
C         WRITE(98,'(1X,E14.8)') CSN(K)*1000.
          WRITE(98,1012) CSN(K)*1000.d0
        ENDDO
C
C       REACTION + INELASTIC TO LEVELS
C       WRITE(93,'(1X,E14.8)') (CST - CSN(1))*1000.
        WRITE(93,1012) (CST - CSN(1))*1000.d0
C
C       ELASTIC
C         WRITE(93,'(1X,E14.8)') CSN(1)*1000.
          WRITE(93,1012) CSN(1)*1000.d0

      ELSE
C
C       INELASTIC TO LEVELS
        DO K=2,NMAX
C         WRITE(98,'(1X,E14.8)') CSN(K)*1000.
          WRITE(98,1012) CSN(K)*1000.d0
        ENDDO
C
C       REACTION + INELASTIC TO LEVELS
C       WRITE(93,'(1X,E14.8)') (CST - CSN(1))*1000.
        WRITE(93,1012) (CST - CSN(1))*1000.d0

      ENDIF

      IF(MEPRI.EQ.99) THEN  
C
c       IF(ETA.EQ.0.) WRITE(96,'(10H<LEGENDRE>,F10.2,F10.5,F10.2,2I5)') 
        IF(ETA.EQ.0.D0) WRITE(96,1000) ANEU,EN,AT,NINT(0.5*JO(1)),NMAX
 1000   FORMAT ('<LEGENDRE>',F10.2,1P,D20.8,0P,F10.2,2I5)               
c       WRITE(97,'(10H<ANG.DIS.>,F10.2,F10.5,F10.2,2I5)')
        WRITE(97,1008) ANEU,EN,AT,NINT(0.5*JO(1)),NMAX
 1008   FORMAT ('<ANG.DIS.>',F10.2,1P,D20.8,0P,F10.2,2I5)               
        DO K=1,NMAX
          IF(ETA.EQ.0.D0) 
     *     WRITE(96,'(2I5,'' COUPLED LEVEL, NUMBER OF VALUES'')') K, LKK
          IF(NPO(K).eq.+1)
     *      WRITE(97,'(I5,F5.1,A1,I4,I5)') K,0.5*JO(K),'+',1,MTET
C                                                          Cross section printed
C    *      WRITE(97,'(I5,F5.1,A1,I4,I5)') K,0.5*JO(K),'+',0,MTET  
          IF(NPO(K).eq.-1)
     *      WRITE(97,'(I5,F5.1,A1,I4,I5)') K,0.5*JO(K),'-',1,MTET
C                                                          Cross section printed
C    *      WRITE(97,'(I5,F5.1,A1,I4,I5)') K,0.5*JO(K),'-',0,MTET  
          IF(ETA.EQ.0.D0) THEN
            DO L=1,LKK
C             WRITE(96,'(2I5,1P,D20.10)') K,L-1,COEF(K,L)
C                                               OUTPUT IN MB 
              WRITE(96,'(2I5,1P,D20.10)') K,L-1,1000.D0*COEF(K,L) 
            ENDDO
          ENDIF
          DO M=1,MTET
            WRITE(97,1038) 0,TET(M),DISC(K,M)*1000.d0,'CROSS-SECTION   '
C           WRITE(97,1038)   TET(M),DISC(K,M)*1000.d0,'CROSS-SECTION   '
C1038       FORMAT (I3,1P,2D12.5,5X,4A4,A2)                             RESU-648
 1038       FORMAT (I3,1P,2D12.5,5X,A16)     
          ENDDO
        ENDDO
      ENDIF

   12 FORMAT(/1X,'NEUTRON ENERGY =',F10.6/1X,'TOTAL  CR-SECT.=',F10.6/
     *1X,'ELASTIC  CR-SECT. =',F10.6/     
     *1X,'REACTION CR-SECT. =',F10.6/
     *1X,'REACTION CR-SECT. incl. coupled levels =',F10.6/
     *1X,'SCATTERING RADIUS =',F10.6)
  120 FORMAT(/1X,'NEUTRON ENERGY =',F10.6/1X,'TOTAL  CR-SECT.=',F10.6/
     *1X,'ELASTIC  CR-SECT. =',F10.6/     
     *1X,'REACTION CR-SECT. =',F10.6/
     *1X,'REACTION CR-SECT. incl. coupled levels =',F10.6)
C RCN
  104 FORMAT(/1X,'PROTON  ENERGY =',F10.6/1X,'TOTAL  CR-SECT.=',F10.6/
     *1X,'ELASTIC  CR-SECT. =',F10.6/ 
     *1X,'REACTION CR-SECT. =',F10.6/
     *1X,'REACTION CR-SECT. incl. coupled levels =',F10.6)
C 130 FORMAT(/3X,'Nlev',17X,'CR-SECT. OF LEVEL EXCITATION '
C    */(1X,I5,25X,F10.6))
  130 FORMAT(
     */2x,'Nlev',4X,'Elev',3x,'Jpi',9x,'CR-SECT(Nlev)'
     */(2X,I2,3X,F7.4,2x,F4.1,A1,10X,F10.6))
  129 FORMAT(/30X,'STRENGTH  FUNCTIONS'
     */1X,'SF0=',E15.7,8X,'SF1=',E15.7,8X,'SF2=',E15.7/)
        NUR=NURRR
        CALL THORA(21)
        close(93) 
        close(98)
        close(92)
        close(96)
        close(97)
        close(327)    
        close(328)    
    4 CONTINUE

      RETURN
      END

      CHARACTER*1 FUNCTION cpar(integ)
      INTEGER integ
      cpar = '*'
      if(integ.eq.-1) cpar = '-'
      if(integ.eq.+1) cpar = '+'
      RETURN
      END
     
C     *******************************************************
C     END of optmand
C     *******************************************************
C     *******************************************************
C     Start of ccrd
C     *******************************************************
      SUBROUTINE POTET
C     *******************************************************
      IMPLICIT DOUBLE PRECISION(A-H,O-Z) 
      COMMON/POTB/WNK(40),WN(40),VR,WC,WD
      COMMON/RAD/RR,RC,RD,RW,RS,AR,AC,AW,AD,AS,ALF,AT,ANEU,RZ,ZNUC,ASP
     *,AZ
      COMMON/POTEB/R,DE,VP,WP
      COMMON/DISPE/VD,VRDC,EA,WDISO
      X=(R-RR*DE)/AR
      IF(X.GT.23) GO TO 7
      EX1=EXP(X)
      ARC1=-1.D0/(1.D0+EX1)
      GO TO 3
    7 EX1=EXP(-X)
      ARC1=-EX1
    3 IF(WC.EQ.0.D0) GO TO 5
      X=(R-RC*DE)/AC
      XX=((R-RW*DE)/AW)**2
      IF(X.GT.23) GO TO 4
      EX1=EXP(X)
      BRC1=-ALF/(1.D0+EX1)-(1.D0-ALF)*EXP(-XX)
      GO TO 5
    4 EX1=EXP(-X)
      BRC1=-ALF*EX1-(1.D0-ALF)*EXP(-XX)
    5 X=(R-RD*DE)/AD
      IF(X.GT.23) GO TO 6
      EX2=EXP(X)
      BRC2=-4.D0*EX2/(1.D0+EX2)/(1.D0+EX2)
      GO TO 2
    6 EX2=EXP(-X)
      BRC2=-4.D0*EX2
    2 CONTINUE
      VP=ARC1*VR+BRC2*VD+BRC1*VRDC
      WP=BRC1*WC+BRC2*WD
      RETURN
      END
C     *******************************************************
      SUBROUTINE ASFUT
C     *******************************************************
      IMPLICIT DOUBLE PRECISION(A-H,O-Z) 
      COMMON/RAD/RR,RC,RD,RW,RS,AR,AC,AW,AD,AS,ALF,AT,ANEU,RZ,ZNUC,ASP
     *,AZ
      COMMON/BNWI/FNR(90),FNI(90),FBR(90),FBI(90),X,LMA1
      COMMON/POTB/WNK(40),WN(40),VR,WC,WD
     */STR/STEP,RK,NH1
     */ENA/EN,EL(40),BET(10),NUR,NMAX,NPD,LAS
     */FBN/FBR1(40,90),FBI1(40,90),FNR2(40,90),FNI2(40,90),FNR1(40,90),
     *FNI1(40,90),FBR2(40,90),FBI2(40,90)
     */NCLMA/LLMA,NCMA,NSMA,KODMA
      COMMON/MENU/MEJOB,MEPOT,MEHAM,MECHA,MEPRI,MESOL,MESHA,MESHO,MEHAO
     *,MEAPP,MEVOL,MEREL,MECUL,MERZZ,MERRR,MEDIS,MERIP
     */COUL/CONZ,ETA,COPH(40,90)
     */WSTE/WSTEP
      COMMON/QNB/JO(40),NPO(40),KO(40),NCA(40)
      AMI=939.56536D0
       IF(MECHA.EQ.1) AMI=938.272029D0
       
       IF(MECHA.EQ.1.AND.IDINT(ASP+0.1D0).EQ.1)AMI=1875.612859D0 
       IF(MECHA.EQ.1.AND.IDINT(ASP+0.1D0).EQ.1)ANEU=2.013553212712D0 
       
      REL=(EN+AMI)/AMI
      IF(MEREL.EQ.0) REL=1.D+0
      RELPOT=1.d0
      IF(REL.NE.1.) RELPOT=2.D+0*(EN+AMI)/(2.D+0*AMI+EN)

      EIRELM=SQRT(REL**2*AT**2+2.D+0*REL*ANEU*AT+ANEU**2)
      EIRELT=AT*SQRT(REL**2*ANEU**2+2.D+0*REL*ANEU*AT+AT**2)
      DEREL=SQRT(2.D+0*REL*ANEU*AT+AT**2+ANEU**2)
      WW=4.8257984E-2*EIRELM*EIRELT/(ANEU*EIRELM+EIRELT)/DEREL*(ANEU/
     *1.008664924)
      IF(MEREL.EQ.1.OR.MEREL.EQ.3) WW=WW*RELPOT


      LLMA1=LLMA+1
      LMA2=0.5252113*SQRT(VR*WW)*RR+4.D+0
      LMA1=1.65*(WNK(1)*RK+1.83)+4.D+0
      IF(LMA1.GT.LLMA1) LMA1=LLMA1
      DO 1 I=1,NUR
      COPH(I,1)=0.
      X=WNK(I)*(RK-STEP)
      
      ETA=CONZ/WNK(I)
      IF(MECHA.EQ.1.AND.NCA(1).NE.NCA(I)) ETA=0.D0
       

      IF(ETA.GT.400.D0) THEN
        IF (MEPRI.NE.99) PRINT 999, I,ETA
        WRITE(21,999) I,ETA
        pause 'WARNING, SEE OUTPUT FILE !!!'
      ENDIF
     
  999 FORMAT(9X,'WARNING! INCIDENT ENERGY IS TOO CLOSE TO',I3,3X,'LEVEL'
     */10X,'IT MAY CAUSE ERROR IN CALCULATED VALUES!!!!!!'//
     *14X,'YOU CAN MOVE ENERGY BY 0.01MEV,',4X,'ETA=',E12.7,2E12.5)

      IF(ETA.NE.0.D0) CALL COPHA
      COPH(I,1)=COPH(40,1)
      IF(ETA.EQ.0.D0) COPH(I,1)=0.D0
      IF(I.GT.NMAX) GO TO 2
      CALL BENEC
      GO TO 3
    2 LMA1=LMA2+1
      IF(LMA1.GT.LLMA1) LMA1=LLMA1
       WSTEP=0.D0
      CALL BESIM
    3 DO 4 K=1,LMA1
      IF(K.GE.LMA1) GO TO 8
      COPH(I,K+1)=COPH(I,K)+ATAN(ETA/K)
    8 FBR1(I,K)=FBR(K)
      FBI1(I,K)=FBI(K)
      FNR1(I,K)=FNR(K)
    4 FNI1(I,K)=FNI(K)
      X=WNK(I)*(RK+STEP)
      IF(I.GT.NMAX) GO TO 5
      CALL BENEC
      GO TO 6
    5 WSTEP=WNK(I)*2.D0*STEP
      CALL BESIM
    6 DO 7 K=1,LMA1      
      IF(K.GE.LMA1) GO TO 9
      COPH(I,K+1)=COPH(I,K)+ATAN(ETA/K)
    9 FBR2(I,K)=FBR(K)
      FBI2(I,K)=FBI(K)
      FNR2(I,K)=FNR(K)
    7 FNI2(I,K)=FNI(K)
    1 CONTINUE
      COPH0=COPH(1,1)
      DO 10 I=1,20
      DO 10 K=1,90
   10 COPH(I,K)=COPH(I,K)-COPH0
      RETURN
      END
C     *******************************************************
      SUBROUTINE BENEC
C     *******************************************************
C***  FBR, FNR - F AND G ASSYMPTOTIC FUNCTIONS FOR OPENED CHANNELS
      IMPLICIT DOUBLE PRECISION(A-H,O-Z) 
      COMMON/BNWI/FNR(90),FNI(90),FBR(90),FBI(90),X,LMA1
     */COUL/CONZ,ETA,COPH(40,90)
      DIMENSION FC(91),FCP(91),GC(91),GCP(91)
      IF(ETA.EQ.0.D0) GO TO 3
      MINL=0
      MAXL=LMA1-1
      RHS=X
      ETS=ETA
      ACCUR=10.D-14
c      STEPC=100
      STEPC=999.0
      LN=1
      NUMBR=3
c      CALL RCWFN(RHS,ETS,MINL,MAXL,FC,FCP,GC,GCP,ACCUR,STEPC)
      CALL RCWF(RHS,ETS,MINL,MAXL,FC,FCP,GC,GCP,ACCUR,STEPC,NUMBR)
      GO TO 4
    3 LN=3
      FNI(1)=0.D0
      FNI(2)=0.D0
      FBI(2)=0.D0
      FBI(1)=0.D0
      FBR(1)=SIN(X)
      FNR(1)=COS(X)
      FBR(2)=SIN(X)/X-COS(X)
      FNR(2)=COS(X)/X+SIN(X)
      IF(LMA1.LT.3) GO TO 2
    4 DO 1 K=LN,LMA1
      FBI(K)=0.D0
      FNI(K)=0.D0
      IF(ETA.EQ.0.D0) GO TO 5
      FBR(K)=FC(K)
      FNR(K)=GC(K)
      GO TO 1
    5 K1=K-1
      K2=K-2
      XK=(K+K-3)/X
      FNR(K)=XK*FNR(K1)-FNR(K2)
      FBR(K)=XK*FBR(K1)-FBR(K2)
    1 CONTINUE
    2 RETURN
      END
C     *******************************************************
      SUBROUTINE BESIM
C     *******************************************************
C***  FBR, FNR - F AND G ASSYMPTOTIC FUNCTIONS FOR CLOSED CHANNELS,
C***  SUCH, THAT G+iF IS DECREASING SPHERICAL WAVE
      IMPLICIT DOUBLE PRECISION(A-H,O-Z) 
      COMMON/BNWI/FNR(90),FNI(90),FBR(90),FBI(90),X,LMA1
     */COUL/CONZ,ETA,COPH(40,90)
     */STR/STEP,RK,NH1
     */WSTE/WSTEP
      IF(ETA.GT.0.D0) GO TO 4
      EX1=EXP( X)
      EX2=1./EX1
      FBR(1)=0.D0
      FBI(1)=(EX1+EX2)/2.
      FNR(1)=(EX1-EX2)/2.
      FNI(1)=0.D0
      FBR(2)=FBI(1)/X-FNR(1)
      FBI(2)=0.D0
      FNR(2)=0.D0
      FNI(2)=FBI(1)-FNR(1)/X
      IF(LMA1.LT.3) GO TO 2
      DO 1 K=3,LMA1
      K1=K-1
      K2=K-2
      XK=(K+K-3)/X
      FBR(K)=XK*FBI(K1)-FBR(K2)
      FBI(K)=-XK*FBR(K1)-FBI(K2)
      FNR(K)=XK*FNI(K1)-FNR(K2)
    1 FNI(K)=-XK*FNR(K1)-FNI(K2)
    2 RETURN
    4 X=X-WSTEP
      CALL BESIMC
      RETURN
      END
C*******************************************************************
      SUBROUTINE WITTFU
C*******************************************************************
C
C     Calculates Whittaker functions W(-ETA, L+1/2, 2X) for
C     L=0 to L=LMA1-1, and X>0., using integral presentation form
C     See: M. Abramowitz and I. Stegun " Handbook of Mathematical
C      Functions" 1964, formulas 13.1.33, 13.2.5
C
      IMPLICIT DOUBLE PRECISION(A-H,O-Z) 
      COMMON/BNWI/FNR(90),FNI(90),FBR(90),FBI(90),X,LMA1
     */COUL/CONZ,ETA,COPH(40,90)
      COMMON/INTEG/SUMCUR,ALOW,AUP,ALW,EPSIN
      DO 1 I=1,LMA1
      ALW=I-1.
      AUP=0.D+00
      EPS=1.D-07
      ST=2.*I
      IF(I+ETA.GT.ST) ST=I+ETA
      WIT=0.
      EPSIN=EPS
    8 ALOW=AUP
      AUP=ALOW+ST
      CALL SIMPSW
      WIT=WIT+SUMCUR
      IF(SUMCUR/WIT-EPS) 13,13,18
   18 EPSIN=EPS*WIT/SUMCUR
      GO TO 8
   13 FNI(I)=WIT*EXP(-X-ETA*LOG(2.D+00*X))/GAMMA(1.D+00+ALW+ETA)
    1 CONTINUE
      RETURN
      END
C*******************************************************************
      SUBROUTINE SIMPSW
C***********************************************************************
      IMPLICIT DOUBLE PRECISION(A-H,O-Z) 
      COMMON/BNWI/FNR(90),FNI(90),FBR(90),FBI(90),X,LMA1
     */COUL/CONZ,ETA,COPH(40,90)
      COMMON/INTEG/SUMCUR,ALOW,AUP,ALW,EPSIN
      SUMIN=0.
      NN=4
      H=(AUP-ALOW)/NN
      Y=ALOW
C     IF(Y.EQ.0.) T3=0.
      T3=0.D0
      IF(Y.NE.0.) T3=Y**(ALW+ETA)*EXP(-Y)*(1.D+00+Y/2./X)**(ALW-ETA)
      Y=AUP
C     IF(Y.EQ.0.) T3=T3
      IF(Y.NE.0.) T3=T3+Y**(ALW+ETA)*EXP(-Y)*(1.D+00+Y/2./X)**(ALW-ETA)
      Y=(ALOW+AUP)/2.
      S2=0.D0
C     IF(Y.EQ.0.) S2=0.
      IF(Y.NE.0.) S2=Y**(ALW+ETA)*EXP(-Y)*(1.D+00+Y/2./X)**(ALW-ETA)
   1  S4=0.D0
      K1=NN-1
      Y=ALOW+H
      I1=1
   2  IF(Y.EQ.0.) S4=S4
      IF(Y.NE.0.) S4=S4+Y**(ALW+ETA)*EXP(-Y)*(1.D+00+Y/2./X)**(ALW-ETA)
      IF(I1-K1)3,4,4
   3  I1=I1+2
      Y=Y+2.*H
      GO TO 2
   4  SUMCUR=H/3.*(T3+2.*S2+4.*S4)
         C1=ABS(1.-SUMIN/SUMCUR)
       IF(C1-EPSIN)6,6,5
    5 SUMIN=SUMCUR
      S2=S2+S4
      H=H*0.5
      NN=NN*2
      GO TO 1
    6 SUMCUR=SUMIN+(SUMCUR-SUMIN)*1.066666
      RETURN
      END
C     ***********************************************************************
      SUBROUTINE BESIMR
C     *******************************************************
C***  FBR, FNR - F AND G ASSYMPTOTIC FUNCTIONS FOR CLOSED
C***  CHARGED PARTICLES CHANNELS,
C***  SUCH, THAT G+iF IS DECREASING SPHERICAL WAVE
      IMPLICIT DOUBLE PRECISION(A-H,O-Z) 
      COMMON/BNWI/FNR(90),FNI(90),FBR(90),FBI(90),X,LMA1
     */COUL/CONZ,ETA,COPH(40,90)
     */STR/STEP,RK,NH1
     */WSTE/WSTEP
      DIMENSION F(91),FD(91),G(91),GD(91),SIGMA(91)
      RHO=X
       L=LMA1-1
       DO 5 K=1,LMA1
       FNI(K)=0.D0
       FBR(K)=0.D0
       FBI(K)=3.D0
       FNR(K)=1.D0
    5 CONTINUE
       IF(WSTEP.EQ.0.) GO TO 8
       A=1.D0
       B=2.D0
       WS10=WSTEP/3.D+02
       DO 7 I=1,300
       RHO=RHO+WS10*(I-1)
      CALL cocl(g,gd,f,fd,sigma,eta,rho,l)
       DO 4 K=1,LMA1
       A=A+A/F(K)*FD(K)*WS10
       B=B+B/G(K)*GD(K)*WS10
C      PRINT 999, FNI(K),FBR(K),x,eta,WSTEP,K
C 999 FORMAT (5E12.4,I8)
C      PAUSE 1
    4  CONTINUE
    7 CONTINUE
       DO 9 K=1,LMA1
       FBI(K)=A+B
       FNR(K)=B-A
C      PRINT 999, FNI(K),FBR(K),x,eta,WSTEP,K
C 999 FORMAT (5E12.4,I8)
C      PAUSE 1
    9  CONTINUE
    8 RETURN
      END
C     ***********************************************************************
      SUBROUTINE BESIMC
C     ***********************************************************************
C***  TWO LINEAR  INDEPENDENT CLOSED CHANNELS COULOMB FUNCTIONS
C***  EQUAL UNITY AT K*(X-STEP) AND INTEGRATED TO K*(X+STEP) USING
C***  SECOND DERIVATIVE FROM FROM COULOMB EQUATION.
C***  LINEAR INDEPENDENT, AS EQUAL IN FIRST MATCHING POINT K*(X-STEP) AND DIFFERENT
C***  AT THE NEXT INTEGRATION  POINT
      IMPLICIT DOUBLE PRECISION(A-H,O-Z) 
      COMMON/BNWI/FNR(90),FNI(90),FBR(90),FBI(90),X,LMA1
     */COUL/CONZ,ETA,COPH(40,90)
     */STR/STEP,RK,NH1
     */WSTE/WSTEP
      RHO=X
       ETA2=ETA*2.D0
       NSTE=10
       NSTE1=NSTE-1
       DO 5 K=1,LMA1
       FNI(K)=0.D0
       FBR(K)=0.D0
       FBI(K)=1.D0
       FNR(K)=1.D0
    5 CONTINUE
       IF(WSTEP.EQ.0.) RETURN
       DO 6 K=1,LMA1
       ALL1=K*(K-1.D0)
       F0=1.D0
       G0=1.D0
       F1=F0-2.D0/NSTE
       G1=G0+5.D0/NSTE
       WSN=WSTEP/NSTE
       WSN2=WSN*WSN
       X=RHO
       X2=X*X
       DO 7 I=1,NSTE1
       X=X+WSN
       F2=2.D0*F1-F0+WSN2*(ALL1/X2+ETA2/X+1.D0)*F1
       G2=2.D0*G1-G0+WSN2*(ALL1/X2+ETA2/X+1.D0)*G1
       F0=F1
       F1=F2
       G0=G1
       G1=G2
    7  CONTINUE
       FBI(K)=F2
       FNR(K)=G2
C      PRINT 999, FBI(K),FNR(K),x,eta,WSTEP,K
C 999 FORMAT (5E12.4,I8)
C      PAUSE 1
    6  CONTINUE
      RETURN
      END
C     ***********************************************************************
C 25/07/02                                                      
      SUBROUTINE COCL(G,GD,F,FD,SIGMA,ETA,RHO,L)                        
C CLOSED CHANNEL DECREASING COULOMB FUNCTIONS                           
C INPUT VARIABLES: ETA: COULOMB PARAMETER; ETA >= 0                     
C                  RHO: |K|*R VALUE                                     
C                  L:    MAXIMUM L VALUE                                
C OUTPUT VARIABLES: SIGMA(I)=0 FOR I = 1 TO L+1                         
C       F(I):  DECREASING SOLUTION AT (ETA,RHO) FOR I = 1 TO L+1        
C       FD(I): DERIVATIVE OF F(I) FOR I = 1 TO L+1                      
C       G(I):  INCREASING SOLUTION FOR I = 1 TO L+1                     
C       GD(I): DERIVATIVE OF F(I) FOR I = 1 TO L+1                      
C THE FUNCTIONS ARE RENORMALISED TO 1 AND SUCH THAT  F*GD - G*FD = 1    
C***********************************************************************
      IMPLICIT DOUBLE PRECISION(A-H,O-Z)                                
      LOGICAL IFEQAL                                                   
      DIMENSION F(1),FD(1),G(1),GD(1),SIGMA(1),S(7),T(3)                
      DATA S /7*0.D0/                                                  
      IF (ETA.LT.0.D0.OR.RHO.LE.0.D0) GO TO 11                          
      LP1=L+1                                                           
      IF (ETA.LT.1.D-6) GO TO 6                                         
      IS=7                                                             
      M=10.D0*RHO+1.D0                                                  
      H=M                                                               
      H=RHO/H                                                           
      RHOA=10.D0*(ETA+1.D0)                                             
      IFEQAL=RHOA.LT.RHO                                                
      IF (IFEQAL) RHOA=RHO                                             
      M=RHOA/H+0.5D0                                                   
      RHOA=H*M                                                         
      IF (.NOT.IFEQAL.AND.RHOA.LT.RHO+1.5D0*H) RHOA=RHO+2.D0*H          
C EXPANSION IN POWERS OF 1/RHOA                                         
    1 C=1.D0/RHOA                                                      
      A=1.D0                                                           
      B=A                                                               
      D=0.D0                                                           
      DO 2 M=1,26                                                      
      AM=M                                                              
      A=-A*0.5D0*(ETA+AM-1.D0)*(ETA+AM)*C/AM                           
      B=B+A                                                            
    2 D=D-A*AM*C                                                       
      F(1)=1.D0                                                         
      FD(1)=D/B-1.D0-ETA/RHOA                                           
      IF (IFEQAL) GO TO 7                                              
      S(IS)=B                                                          
      IF (IS.NE.7) GO TO 3                                              
      IS=6                                                              
      RHOA=RHOA+H                                                      
      S(7)=S(7)*EXP(H-ETA*LOG(1.D0-H/RHOA))                          
      GO TO 1                                                          
C BACKWARD INTEGRATION                                                  
    3 A=2.D0+1.D0/1.2D0*H*H                                            
      B=1.D0/6.D0*H*ETA                                                
      C=1.D0-1.D0/12.D0*H*H                                            
      M1=RHOA/H-0.5D0                                                 
      M2=RHO/H-1.5D0                                                   
      AM=M1                                                            
      T(2)=B/(AM+1.D0)                                                 
      T(3)=B/AM                                                        
      JS=M1                                                            
      DO 5 IS=M2,M1                                                    
      DO 4 I=1,6                                                       
    4 S(I)=S(I+1)/S(7)                                                 
      T(1)=T(2)                                                         
      T(2)=T(3)                                                        
      AM=JS-1                                                           
      T(3)=B/AM                                                        
      S(7)=((A+10.D0*T(2))*S(6)-(C-T(1))*S(5))/(C-T(3))                
    5 JS=JS-1                                                          
      F(1)=1.D0                                                        
      FD(1)=(1.D0/60.D0*(S(1)-S(7))+0.15D0*(S(6)-S(2))+0.75D0*(S(3)-S(5)
     1))/(H*S(4))                                                       
      GO TO 7                                                           
    6 F(1)=1.D0                                                        
      FD(1)=-1.D0                                                      
C RECURRENCE FOR L > 0
C RECURRENCE FOR L >                                                   
    7 C=1.D0/RHO                                                       
      IF (L.LE.0) GO TO 9                                              
      DO 8 M=1,L                                                       
      AM=M                                                             
      A=ETA/AM                                                         
      B=A+C*AM                                                         
      F(M+1)=1.D0                                                       
    8 FD(M+1)=(A*A-1.D0)/(B-FD(M))-B                                   
    9 DO 10 M=1,LP1                                                    
      G(M)=1.D0                                                        
      GD(M)=1.D0+FD(M)                                                 
   10 SIGMA(M)=0.D0                                                    
      RETURN                                                           
   11 WRITE (6,1000) ETA,RHO                                           
      STOP                                                             
 1000 FORMAT (17H COCL  ***  ETA =,1P,D13.5,8H,   RHO=,D13.5,26H   ARGUM
     1ENT OUT OF RANGE    )                                             
      END                                                               


C     *******************************************************
c 29/05/86  ibm version                                         
      subroutine coclold(g,gd,f,fd,sigma,eta,rho,l)                     
c closed channel decreasing coulomb functions                           
c input variables: eta: coulomb parameter; eta >= 0                     
c                  rho: |k|*r value                                     
c                  l:    maximum l value                                
c output variables: sigma(i)=0 for i = 1 to l+1                         
c       f(i):  decreasing solution at (eta,rho) for i = 1 to l+1        
c       fd(i): derivative of f(i) for i = 1 to l+1                      
c       g(i):  increasing solution for i = 1 to l+1                     
c       gd(i): derivative of f(i) for i = 1 to l+1                      
c the functions are renormalised to 1 and such that  f*gd - g*fd = 1    
c***********************************************************************
      IMPLICIT DOUBLE PRECISION(A-H,O-Z)                                
      logical ifeqal                                                    
      dimension f(1),fd(1),g(1),gd(1),sigma(1),s(7),t(3)                
      data s /7*0.d0/                                                   
      if (eta.lt.0.d0.or.rho.le.0.d0) go to 11                          
      lp1=l+1                                                           
      if (eta.lt.1.d-6) go to 6                                         
      is=7                                                             
      m=10.d0*rho+3.d0                                                
      h=m                                                             
      h=rho/h                                                         
      rhoa=10.d0*(eta+1.d0)                                          
      ifeqal=rhoa.lt.rho                                              
      m=rhoa/h+0.5d0                                                  
      rhoa=h*m                                                       
      if (ifeqal.or.rhoa.lt.rho+1.5d0*h) rhoa=rho+2.d0*h             
c expansion in powers of 1/rhoa                                       
    1 c=1.d0/rhoa                                                     
      a=1.d0                                                          
      b=1.d0-c*eta                                                    
      f(1)=a                                                         
      fd(1)=b                                                        
      do 2 m=1,26                                                    
      am=m                                                           
      d=0.5d0*(eta+am-1.d0)*(eta+am)*c/am                             
      a=-a*d                                                          
      b=-b*d-a*c                                                     
      f(1)=f(1)+a                                                     
    2 fd(1)=fd(1)+b                                                   
      if (ifeqal) go to 7                                            
      s(is)=f(1)                                                      
      if (is.ne.7) go to 3                                             
      is=6                                                           
      rhoa=rhoa+h                                                    
      s(7)=s(7)*EXP(h-eta*LOG(1.d0-h/rhoa))                         
      go to 1                                                         
c backward integration                                                
    3 a=2.d0+1.d0/1.2d0*h*h                                           
      b=1.d0/6.d0*h*eta                                                
      c=1.d0-1.d0/12.d0*h*h                                          
      m1=rhoa/h-0.5d0                                               
      m2=rho/h-1.5d0                                                 
      am=m1                                                         
      t(2)=b/(am+1.d0)                                              
      t(3)=b/am                                                    
      js=m1                                                         
      do 5 is=m2,m1                                                 
      do 4 i=1,6                                                     
    4 s(i)=s(i+1)/s(7)                                              
      t(1)=t(2)                                                    
      t(2)=t(3)                                                    
      am=js-1                                                      
      t(3)=b/am                                                       
      s(7)=((a+10.d0*t(2))*s(6)-(c-t(1))*s(5))/(c-t(3))              
    5 js=js-1                                                        
      f(1)=1.d0                                                     
      fd(1)=(1.d0/60.d0*(s(1)-s(7))+0.15d0*(s(6)-s(2))+0.75d0*(s(3)-s(5)
     1))/(h*s(4))                                                       
      go to 7                                                          
    6 f(1)=1.d0                                                        
      fd(1)=-1.d0                                                      
c recurrence for l > 0                                                 
    7 c=1.d0/rho                                                       
      if (l.le.0) go to 9                                              
      do 8 m=1,l                                                       
      am=m                                                             
      a=eta/am                                                         
      b=a+c*am                                                         
      f(m+1)=1.d0                                                     
    8 fd(m+1)=(a*a-1.d0)/(b-fd(m))-b                                   
    9 do 10 m=1,lp1                                                    
      g(m)=1.d0                                                       
      gd(m)=1.d0+fd(m)                                                
   10 sigma(m)=0.d0                                                   
      return                                                           
   11 write (6,1000) eta,rho                                           
      stop                                                             
 1000 format (17h cocl  ***  eta =,1p,d13.5,8h,  rho= ,d13.5,27h    argu
     1ment out of range         )                                       
      end                                                               
C     *******************************************************
      SUBROUTINE QUANT
C     *******************************************************
      IMPLICIT DOUBLE PRECISION(A-H,O-Z) 

      DIMENSION STL(250)
      DOUBLE PRECISION jc,jj
      INTEGER ltlmax, itmp, LLLMAX
      CHARACTER*1 parc
      LOGICAL*1 unformat/.TRUE./ 
C     LOGICAL*1 unformat/.FALSE./

      real*8, parameter :: haf = 0.5D0

      PARAMETER (LLLMAX=230)

      CHARACTER*20 fname
      COMMON/INOUT/fname

      COMMON/TDB/TD1(180,19)
      COMMON/CSB/CST,CSR,NST
     */QNSB/INC(180),INR(180),JS(180)
      COMMON/QNS1/LNJ1(250),JNJ1(250),NNJ1(250),KPJ1(250)
      COMMON/POTB/WNK(40),WN(40),VR,WC,WD
     */CS1/CSN(40),CM(40)
      COMMON/LNP/JSS,NPIS,NPIO,JSO,NN1,LNO(180),NS1(180),JNO(180),NSPI
     */ENA/EN,EL(40),BET(10),NUR,NMAX,NPD,LAS
      COMMON/MENU/MEJOB,MEPOT,MEHAM,MECHA,MEPRI,MESOL,MESHA,MESHO,MEHAO
     *,MEAPP,MEVOL,MEREL,MECUL,MERZZ,MERRR,MEDIS,MERIP
      COMMON/QNB/JO(40),NPO(40),KO(40),NCA(40)
      COMMON/JNN/CSS,INCC,NCLL,NSS,NJ,INCR
      COMMON/CMAT/CR(200,200),CI(200,200)
     */STR/STEP,RK,NH1
      COMMON/RAD/RR,RC,RD,RW,RS,AR,AC,AW,AD,AS,ALF,AT,ANEU,RZ,ZNUC,ASP
     *,AZ
     */SF12/SF0,SF1,SF2
     */PRON/TL(100),SSR(100),SSI(100)
     */QNSBD/LNJ(180,250),JNJ(180,250),NNJ(180,250)
     */NCLMA/LLMA,NCMA,NSMA,KODMA
     */TROUT/TRL(100),TRLJ(100,2)
     */RESONI/ERN(10),GNN(10),GREN(10),LON(10),JMN(10),JCON(10),NEL(10)
     *,NRESN
      AMI=939.56536
       IF(MECHA.EQ.1) AMI=938.272029D0
       
      IF(MECHA.EQ.1.AND.IDINT(ASP+0.1D0).EQ.1) AMI=1875.612859D0 
      IF(MECHA.EQ.1.AND.IDINT(ASP+0.1D0).EQ.1) ANEU=2.013553212712D0 
       
C     NSPI=IDINT(ASP*2+0.001)
      NSPI=INT(ASP*2+0.001)
      NDEL=1
      IF(NSPI/2*2.EQ.NSPI) NDEL=0
      DO 876 L=1,100
      TL(L)=0.D0
      TRLJ(L,1)=0.D0
      TRLJ(L,2)=0.D0
      SSR(L)=0.D0
      SSI(L)=0.D0
 876  CONTINUE
      LMAX=1.65*(WNK(1)*RK+1.83)+3
      LMAX1=LMAX+1
      IF(LMAX.GT.LLMA) LMAX1=LLMA+1
      REL=(EN+AMI)/AMI
      IF(MEREL.EQ.0) REL=1.D+0
      IF(REL.NE.1.) RELPOT=2.D+0*(EN+AMI)/(2.D+0*AMI+EN)


      EIRELM=SQRT(REL**2*AT**2+2.D+0*REL*ANEU*AT+ANEU**2)
      EIRELT=AT*SQRT(REL**2*ANEU**2+2.D+0*REL*ANEU*AT+AT**2)
      DEREL=SQRT(2.D+0*REL*ANEU*AT+AT**2+ANEU**2)
      WW=4.8257984E-2*EIRELM*EIRELT/(ANEU*EIRELM+EIRELT)/DEREL*(ANEU/
     *1.008664924)
      IF(MEREL.EQ.1.OR.MEREL.EQ.3) WW=WW*RELPOT


      LMA2=0.5252113*SQRT(VR*WW)*RR+4.D+0
      IF(MEPRI.GE.10 .AND. MEPRI.LT.99) 
     *  PRINT 18,RK,WW,WNK(1),LMA2,LMAX
      IF(MEPRI.GE.10 .AND. MEPRI.LT.99) 
     *  WRITE(21,18)RK,WW,WNK(1),LMA2,LMAX
   18 FORMAT (1X,'RK=',D15.7,1X,'WW=',D15.7,1X,'WNK(1)=',D15.7,
     *1X,'LMA2=',I3,1X,'LMAX=',I3/)
      SF0=0.D0
      SF1=0.D0
      SF2=0.D0
      RS1=1.23*AT**(1./3.)+0.8
      WRR=WN(1)*RS1*RS1
      SEN=SQRT(EN)*3141.593
      P0=SEN
      P1=SEN*3.*WRR/(1.+WRR)
      P2=SEN*5.*WRR**2/(9.+3.*WRR+WRR**2)
      NSS=0
      DO 12 N=1,NMAX
      CSN(N)=0.
   12 CONTINUE
      CST=0.D0
      CSTRT=0.D0
      CSRINT=0.D0
      
      
      IF(MEPRI.GE.10 .AND. MEPRI.LT.99) PRINT 19
      IF(MEPRI.GE.10 .AND. MEPRI.LT.99) WRITE(21,19)
   19 FORMAT(20X,'JSS    NPIS    LNJ1(I)     JNJ1(I)      NNJ1(I) ')
      DO 1 NOT=1,300
      NOT1=NOT-1
      NJ=0
      DO 3 NO1=1,2
      JSS=JO(1)+NDEL+2*NOT1*(-1)**NO1
      IF(JSS.LT.0) NJ=NJ+2
      IF(JSS.LT.0) GO TO 3
      NJ1=0
      DO 2 NCH=1,2
      NSS=NSS+1
      JS(NSS)=JSS
      N1=0
      N2=0
      N3=0
      NPIS=(-1)**NCH
      DO 4 I1=1,NUR
      LMAX=1.65*(WNK(1)*RK+1.83)+3
      IF(I1.GT.NMAX) LMAX=LMA2
      IF(LMAX.GT.LLMA) LMAX=LLMA
      JSO=JO(I1)
      NPIO=NPO(I1)
      CALL LOGMO
      IF(NN1.EQ.0) GO TO 4
      DO 5 I2=1,NN1
      IF(LNO(I2).GT.LMAX) GO TO 4
      IF(N1.GE.250) GO TO 4
      N1=N1+1
      IF(I1.EQ.1) N2=N2+1
      LNJ1(N1)=LNO(I2)
      JNJ1(N1)=JNO(I2)
      NNJ1(N1)=I1
      KPJ1(N1)=NPO(I1)
      LNJ(NSS,N1)=LNJ1(N1)
      JNJ(NSS,N1)=JNJ1(N1)
    5 NNJ(NSS,N1)=NNJ1(N1)
    4 CONTINUE
      IF(N2)13,14,13
   14 NSS=NSS-1
      NJ1=NJ1+1
      IF(NJ1.EQ.2) GO TO 7
      GO TO 2
   13 CONTINUE
      IF(KODMA.LE.0) GO TO 22
      N2P=N2+1
      N1M=N1-1
      DO 21 IM=N2P,N1M
      MLL=LNJ1(IM)
      IMP=IM+1
      DO 20 JM=IMP,N1
      IF(MLL.LE.LNJ1(JM)) GO TO 20
      MLL=LNJ1(JM)
      MLNJ=LNJ1(IM)
      MJNJ=JNJ1(IM)
      MNNJ=NNJ1(IM)
      MKPJ=KPJ1(IM)
      LNJ1(IM)=LNJ1(JM)
      JNJ1(IM)=JNJ1(JM)
      NNJ1(IM)=NNJ1(JM)
      KPJ1(IM)=KPJ1(JM)
      LNJ(NSS,IM)=LNJ1(JM)
      JNJ(NSS,IM)=JNJ1(JM)
      NNJ(NSS,IM)=NNJ1(JM)
      LNJ1(JM)=MLNJ
      JNJ1(JM)=MJNJ
      NNJ1(JM)=MNNJ
      KPJ1(JM)=MKPJ
      LNJ(NSS,JM)=MLNJ
      JNJ(NSS,JM)=MJNJ
      NNJ(NSS,JM)=MNNJ
   20 CONTINUE
   21 CONTINUE
   22 INCC=N2
C     NEXT TWO CARDS JUST TO OVERCOME THE ERROR OR  MICROSOFT FPS COMPILER
C     v.1.00, YOU CAN DELETE THEM USING OTHER COMPILERS
      IF (MEPRI.LT.99) PRINT 899,(WNK(II),II=1,NUR)
  899 FORMAT (5E12.5)
      IF(N2.GT.NCMA) INCC=NCMA
      INC(NSS)=INCC
      NCLL=N1
      IF (MEPRI.LT.99) PRINT 898, N1, NCMA
  898 FORMAT(/2X,'GENERATED CC NUMBER=',I3,3X,'ALLOWED CC NUMBER=',I3/)
      IF(N1.GT.NCMA) NCLL=NCMA
      DO 23 MNC=1,NCLL
      IF(NNJ1(MNC).LE.NMAX) N3=N3+1
   23 CONTINUE
      INCR=N3
      INR(NSS)=INCR
      IF(MEPRI.GE.10 .AND. MEPRI.LT.99) 
     *  PRINT 11,   (JSS,NPIS,LNJ1(NL),JNJ1(NL),NNJ1(NL),NL=1,NCLL)
      IF(MEPRI.GE.10 .AND. MEPRI.LT.99)
     *  WRITE(21,11)(JSS,NPIS,LNJ1(NL),JNJ1(NL),NNJ1(NL),NL=1,NCLL)
   11 FORMAT (15(I5,2I3,I3,I2))
      IF(LAS.EQ.0) GO TO 100
      IF(MEHAM.EQ.1.AND.NPD.EQ.0) GO TO 100
C     CALL KNDIT
 100  CALL CMATC
      CONG=(JSS+1)/(JO(1)+1.)
      CON=12.566372*CONG/WN(1)/100./(NSPI+1.D0)
      
C      PRINT 787, JSS, JO(1),NSPI, CONG,CON
C  787 FORMAT(3I5,2E12.5)
C      PAUSE
      
      CSS=CON*CSS
      IF(MEPRI.LT.99) PRINT 999, JSS,CSS
C     IF(CSS.LT.1.E-4) NJ=NJ+1

      IF(NSS.GT.1) GO TO 677
      FIR=WNK(1)*RS1
      CSEL=FIR**2
           
      FIR=FIR-ATAN(FIR)
      CSEL=CSEL+3.D0*FIR**2
      
      FIR=FIR-ATAN(3.D0*FIR/(3.D0-FIR**2))
      CSEL=CSEL+5.D0*FIR**2 
           
      FIR=FIR-ATAN(FIR*(15.D0-FIR)**2/
     *(15.D0-6.D0*FIR**2))
      CSEL=(CSEL+7.D0*FIR**2)*CON/CONG*(NSPI+1.D0) 
      
 677  CSTJPI=0.D0
      DO 8 IC=1,INCC
      CSTJPI=CSTJPI+ABS(CI(IC,IC)*CON)
 
      CSTR=0.D0
      CSNR=0.D0
      CSNRIN=0.D0
      CSRER=0.D0 
      TDR1=0.D0 

      IF(NRESN.EQ.0) GO TO 201     
     
      EBOUND=6.D0
      CRO=EXP(-2.D0*(AT/8.D0*EBOUND)) 
      CROE=EXP(2.D0*(AT/8.D0*(EN-EBOUND))) 
      ANORMM=(1.D0-CRO)/(1.D0-2.D0*CRO+CROE)
      ANORM=1.D0-ANORMM
      ANORM2=ANORM**2
c     IF(MEPRI.LT.99) PRINT 919, EN,ANORM,ANORMM,EBOUND,CRO,CROE,CONG
c     PAUSE 777
      ANORMM=1.D0
      ANORM=1.D0
         
      DO 202 I=1,NRESN
           
      IF(JCON(I).NE.JSS.OR.LON(I).NE.LNJ1(IC)
     *.OR.JMN(I).NE.JNJ1(IC))GO TO 202
      
      ERI=ERN(I)
      REL=(ABS(ERI)+AMI)/AMI
      IF(MEREL.EQ.0.OR.ERI.LE.0.) REL=1.D+0
      IF(MEREL.NE.0.AND.ERI.GT.0.) ERIC=(REL**2-1.D+0)*AMI/2.D+0
      APAN=ANEU/1.00866491600D0 
      WNKRI=0.219677*SQRT(ABS(ERIC))*AT/SQRT(AT**2+2.D+0*ANEU*REL*AT+
     *ANEU**2)*SQRT(APAN)
      WNKR=WNKRI*RS1
      WNKR1=WNK(1)*RS1
      WRRRE=WNKR**2
      RP00=WNKR1/WNKR
      IF(LON(I).EQ.0) PR0=RP00
      IF(LON(I).EQ.1) PR0=RP00*WRR/(1.+WRR)/WRRRE*(1.D0+WRRRE)
      IF(LON(I).EQ.2) PR0=RP00*WRR**2/(9.+3.*WRR+WRR**2)/
     *WRRRE**2*(9.+3.*WRRRE+WRRRE**2)
      IF(LON(I).EQ.3) PR0=RP00*WRR**3/(225.+45.*WRR+6.*WRR**2+WRR**3)/
     *WRRRE**3/(225.+45.*WRRRE+6.*WRRRE**2+WRRRE**3)
     
      IF(LON(I).EQ.0) FIR=WNKR1
      IF(LON(I).EQ.1) FIR=WNKR1-ATAN(WNKR1)
      IF(LON(I).EQ.2) FIR=WNKR1-ATAN(3.D0*WNKR1/(3.D0-WNKR1**2))
      IF(LON(I).EQ.3) FIR=WNKR1-ATAN(WNKR1*(15.D0-WNKR1)**2/
     *(15.D0-6.D0*WNKR1**2))
     
      GNNE=GNN(I)*PR0
      GTOTN=GREN(I)+GNNE
      IF(NEL(I).NE.0)GTOTN=GREN(I)+GNNE+GNN(NEL(I))*PR0
      ADEN=(EN-ERN(I))**2+GTOTN**2/4.D0
      CMRR=GNNE*GTOTN/4.D0/ADEN
      CMRI=-GNNE*(EN-ERN(I))/2.D0/ADEN
      
        

      CR(IC,IC)=ANORM*CR(IC,IC)+CMRR*ANORMM
      CI(IC,IC)=ANORM*CI(IC,IC)+CMRI*ANORMM
      GO TO 979 
      CR(IC,IC)= ANORM*CR(IC,IC)
      CI(IC,IC)= ANORM*CI(IC,IC)
      
      
       
      CSTR=CSTR+GNNE*GTOTN/ADEN*CON/4.D0
      CSNR=CSNR+GNNE*GNNE/ADEN*CON/4.D0
      CSNRIN=CSNRIN-(GNNE*GTOTN*SIN(FIR)**2-(EN-ERN(I))*GNNE*
     *SIN(2.D0*FIR))/ADEN*CON/2.D0
      CSRER=CSRER+GNNE*GREN(I)/ADEN*CON/4.D0
      TDR1=CSRER*4.D0/CON*ANORMM
       
  979 IF(MEPRI.LT.99) THEN
        PRINT 919,CSTR,CSNR,CSRER,CSNRIN
        PRINT 919,CON,GNNE,GTOTN,ADEN
        PRINT 919, CMRI, CMRR,CI(IC,IC),CR(IC,IC)
      ENDIF
  919 FORMAT (5E12.5)
  202 CONTINUE    
  201 CONTINUE           
      
      CSTRT=CSTRT+CSTR
      CSRINT=CSRINT+CSNRIN
c   8 nspi
CST+CI(IC,IC)*CON+CSTR+CSNRIN
    8 CST=CST+CI(IC,IC)*CON 
      

      IF(MECHA.EQ.0) EPC=3.E-4
      IF(MECHA.NE.0) EPC=3.E-9
      IF(CSS.LT.EPC.AND.CSTJPI.LT.EPC) NJ=NJ+1
      IF(MEPRI.LT.99) PRINT 123,CST
  123 FORMAT (6X,F20.10)
      DO 9 I1=1,NMAX
      CM(I1)=0.
      DO 10 IC=1,INCC
      DO 10 IR=1,INCR
      IF(I1.EQ.NNJ1(IR)) CM(I1)=CM(I1)+CR(IC,IR)**2+CI(IC,IR)**2
   10 CONTINUE
    9 CSN(I1)=CSN(I1)+CM(I1)*CON
      DO 15 IC=1,INCC
      JNUM=(3+JNJ1(IC)-2*LNJ1(IC))/2
      CI2=CI(IC,IC)
      T1=CI2
C     TL,SR,SI,SFI -FOR SPIN=1/2, CHANGE 2 by NSPI+1 IN FORMULAE
      IF(LNJ1(IC).EQ.0) SF0=SF0+CI2*CONG*2.D0/(NSPI+1.D0)
      IF(LNJ1(IC).EQ.1) SF1=SF1+CI2*CONG*2.D0/(NSPI+1.D0)
      IF(LNJ1(IC).EQ.2) SF2=SF2+CI2*CONG*2.D0/(NSPI+1.D0)
      DO 16 IR=1,INCR
      ANOIR=1.D0
      IF(IC.NE.IR) ANOIR=ANORM
      ANOIR=1.D0
      CR1=CR(IC,IR)*ANOIR
      CI1=CI(IC,IR)*ANOIR
      T1=T1-CR1*CR1-CI1*CI1
      IF(LNJ1(IC).EQ.0) SF0=SF0-(CR1*CR1+CI1*CI1)*CONG*2.D0/(NSPI+1.D0)
      IF(LNJ1(IC).EQ.1) SF1=SF1-(CR1*CR1+CI1*CI1)*CONG*2.D0/(NSPI+1.D0)
      IF(LNJ1(IC).EQ.2) SF2=SF2-(CR1*CR1+CI1*CI1)*CONG*2.D0/(NSPI+1.D0)
   16 CONTINUE
      TD1(NSS,IC)=T1*4.D0 +TDR1
      IF(MEPRI.LT.99) PRINT 919, T1, TDR1, TD1(NSS,IC)
C     PAUSE 100

      TL(LNJ1(IC)+1)=TL(LNJ1(IC)+1)+(JSS+1)*4.*T1
      TRLJ(LNJ1(IC)+1,JNUM)=TRLJ(LNJ1(IC)+1,JNUM)+(JSS+1)*4.*T1
      SSR(LNJ1(IC)+1)=SSR(LNJ1(IC)+1)+(1.-2.*CI(IC,IC))*(JSS+1)
      SSI(LNJ1(IC)+1)=SSI(LNJ1(IC)+1)+2.*CR(IC,IC)*(JSS+1)
   15 CONTINUE
      IF(NSS.GE.NSMA) GO TO 7
    2 CONTINUE
C     IF(NJ.EQ.2*(NSPI+1)) GO TO 7
      IF(NJ.EQ.4) GO TO 7
      IF(NOT1.EQ.0) GO TO 1
    3 CONTINUE
    1 CONTINUE
    7 NJ=NSS
      IF(NRESN.EQ.0) GO TO 220

      IF(MEPRI.LT.99) THEN
        PRINT *,' RENORMALIZATON FROM RESONANCES'
        PRINT 919, ANORM,CST,CSN(1),CSTRT,CSRINT
      ENDIF
      CST=(CST-CSN(1))*ANORM+CSN(1)+CSTRT+CSRINT
      CSN(1)=CSN(1)+CSRINT
      ltlmax = LLLMAX
      WRITE (21,*) 
      WRITE (21,*) ' RENORMALIZATON FROM RESONANCES'
      WRITE (21,919) ANORM,CST,CSN(1),CSTRT,CSRINT
C     PAUSE 999
 220  IF(MEJOB.EQ.2) GO TO 17
      
c      IF(NSPI.GT.1) GO TO 17

      IF(MEPRI.LT.99) PRINT 987
      WRITE(21,987)
 987  FORMAT (//1X,'ORB. MOMENT',14X,'TRANSITIONS',12X,'SR',18X,'SI'/)
      
      DO 888 L=1,LMAX1
      LL=L-1
      TLL=TL(L)/(NSPI+1.D0)/(JO(1)+1.)/(2.*LL+1.)
C     TRLJ(L,1)=TRLJ(L,1)/2./(JO(1)+1.)/(2.*LL+1)
C     TRLJ(L,2)=TRLJ(L,2)/2./(JO(1)+1.)/(2.*LL+1)
      IF(LL.EQ.0) TRLJ(1,1)=TLL
      if(tll.gt.1.d-15) ltlmax = L
      IF(LL.NE.0) THEN
        TRLJ(L,1)=TRLJ(L,1)/2./(JO(1)+1.)/(LL)
        if(TRLJ(L,1).gt.1.e-15) ltlmax = L
      ENDIF
      TRLJ(L,2)=TRLJ(L,2)/2./(JO(1)+1.)/(LL+1)
      if(TRLJ(L,2).gt.1.e-15) ltlmax = L
      SRL=SSR(L)/(NSPI+1.D0)/(JO(1)+1.)/(2.D0*LL+1.D0)
      SIL=SSI(L)/(NSPI+1.D0)/(JO(1)+1.)/(2.D0*LL+1.D0)
      TRL(L)=TLL
      IF(MEPRI.LT.99) PRINT 999,LL,TLL,SRL,SIL
      WRITE(21,999)LL,TLL,SRL,SIL
 888  CONTINUE

      ltlmax = Min(ltlmax,LLLMAX-1)
C
C     Formatting the output for reaction codes (e.g. EMPIRE,GNASH)
C     RCN
      
C
      IF(MEPRI.EQ.99) THEN
C
C      EMPIRE (ecis06 formatted output)
C
C----------------
C      from ecis06
 1006  FORMAT ('<TLJ     >',F10.2,1P,D20.8,0P,F10.2,2I5)                CAL1-427
 1007  FORMAT (1X,F9.1,4X,A1,1X,I4)                                     CAL1-428
 1008  FORMAT (1X,I2,I6,F9.1,2X,1P,D18.8,0P)                            CAL1-429
C----------------
C      open(unit=92,file=TRIM(fname)//'.TLJ')

C      WRITE(92,'(10H<TLJ     >,F10.2,F10.5,F10.2,2I5)') 
       IF(.NOT.unformat) WRITE(92,1006)ANEU,EN,AT,NINT(0.5*JO(1)),numbtl

       parc = '+'
       numbtl = 0

       DO L=1,ltlmax
        LL = L-1 
        IF(NPO(1)*(-1)**LL.EQ.-1) CYCLE
        IF(L.NE.1 .AND. TRLJ(L,1).GT.0) THEN
          numbtl = numbtl + 1
          xz = dble(ll) - haf
          IF(unformat) then 
            WRITE(92)                  xz, parc,1
            WRITE(92)      1, ll, xz, TRLJ(L,1)
          ELSE
C           WRITE(92,1007)         L-1-0.5,'+',1
C           WRITE(92,1008) 1, L-1, L-1-0.5, TRLJ(L,1)
            WRITE(92,1007)             xz, parc,1
            WRITE(92,1008) 1, ll, xz, TRLJ(L,1)
          ENDIF

        ENDIF
        IF(TRLJ(L,2).GT.0) THEN
          numbtl = numbtl + 1
          xz = dble(ll) + haf
          IF(unformat) then 
            WRITE(92)                  xz, parc,1
            WRITE(92)      1, ll, xz, TRLJ(L,2)
          ELSE
C           WRITE(92,1007)         L-1+0.5,'+',1
C           WRITE(92,1008) 1, L-1, L-1+0.5, TRLJ(L,2)
            WRITE(92,1007)             xz, parc,1
            WRITE(92,1008) 1, ll, xz, TRLJ(L,2)
          ENDIF 
        ENDIF
       ENDDO

       parc='-'

       DO L=1,ltlmax
        LL = L-1 
        IF(NPO(1)*(-1)**LL.EQ.+1) CYCLE
        IF(L.NE.1 .AND. TRLJ(L,1).GT.0.) THEN
          numbtl = numbtl + 1
          xz = dble(ll) - haf
          IF(unformat) then 
            WRITE(92)                  xz, parc,1
            WRITE(92)      1, ll, xz, TRLJ(L,1)
          ELSE 
C           WRITE(92,1007)         L-1-0.5,'-',1
C           WRITE(92,1008) 1, L-1, L-1-0.5, TRLJ(L,1)
            WRITE(92,1007)             xz, parc,1
            WRITE(92,1008) 1, ll, xz, TRLJ(L,1)
          ENDIF
        ENDIF
        IF(TRLJ(L,2).GT.0.) THEN
          numbtl = numbtl + 1
          xz = dble(ll) + haf
          IF(unformat) then 
            WRITE(92)                  xz, parc,1
            WRITE(92)      1, ll, xz, TRLJ(L,2)
          ELSE
C           WRITE(92,1007)         L-1+0.5,'-',1
C           WRITE(92,1008) 1, L-1, L-1+0.5, TRLJ(L,2)
            WRITE(92,1007)             xz, parc,1
            WRITE(92,1008) 1, ll, xz, TRLJ(L,2)
          ENDIF
        ENDIF
       ENDDO
C      CLOSE(92)

       Stl = 0.d0

       IF(unformat) then 
         OPEN(45,STATUS='old',FILE=TRIM(fname)//'.tlj',
     &         ERR=1200,form='unformatted')
       ELSE
         OPEN(45,STATUS='old',FILE=TRIM(fname)//'.tlj', ERR=1200)
         READ(45,*,END=1200)    ! To skip first line <TLJs.> ..
       ENDIF 

C------JC,ParC is the channel spin and parity
C------nceq is the number of coupled equations
1100   IF(unformat) then 
           READ(45,END = 1200)      jc, parc, nceq  ! ecis06
         ELSE
           READ(45,1007,END = 1200) jc, parc, nceq  ! ecis06
         ENDIF      
C------Loop over the number of coupled equations
       DO nc = 1, nceq
C--------Reading the coupled level number nlev, the orbital momentum L,
C--------angular momentum j and Transmission coefficient Tlj,c(JC)
C--------(nlev=1 corresponds to the ground state)

         IF(unformat) then 
           READ(45)   nlev, l, jj, dtmp
         ELSE
           READ(45,*) nlev, l, jj, dtmp
         ENDIF

C--------Selecting only ground state
         IF (dtmp.GT.1.D-15 .AND. l.LT.LLLMAX) THEN
C-----------Averaging over particle and target spin, summing over channel spin jc
            dtmp = dtmp*(2.D0*jc + 1.D0)/(2.D0*haf + 1.D0)
            Stl(l+1) = Stl(l+1) + dtmp/DBLE(2*l + 1)/DBLE(JO(1) + 1)
         ENDIF
       ENDDO
       goto 1100
 1200  CLOSE (45)
C
      ELSE
C
C      GNASH and others
C
       WRITE(22,998)EN,LMAX,(TRL(L),L=1,LMAX1)
       WRITE(24,996)EN,LLMA,TRLJ(1,2),
     *  (TRLJ(L,1),TRLJ(L+1,1),TRLJ(L,2),TRLJ(L+1,2),L=2,LLMA,2)
C
      ENDIF

 996  FORMAT (E12.6,I3/(6E11.4))
 998  FORMAT (E12.6,I3/(6F11.8))
 999  FORMAT(1X,I5,10X,4F20.10)
C     IF(MEPRI.NE.99) PRINT 111,LNJ1(IC),CR(IC,IC),CI(IC,IC)
C 111 FORMAT (3X,I10,2F20.10)

   17 CSR=CST-CSN(1)
      
      SINlcc = 0.d0
      IF(NMAX.LT.2) GO TO 955
      DO 55 N=2,NMAX
   55 SINlcc = SINlcc + CSN(N)

C  55 CSR=CSR-CSN(N)
  955 CONTINUE
      SF0=SF0/P0
      SF1=SF1/P1
      SF2=SF2/P2
      
      IF(MEPRI.EQ.99) THEN
C
C       for EMPIRE
C
        OPEN (46,FILE = trim(fname)//'_INC.LST')
        WRITE (46,'(A5,I6,1X,D12.6)') 'LMAX:', ltlmax, EN
        DO l = 0, ltlmax
          WRITE (46,*) l, stl(l+1)
        ENDDO
        WRITE (46,'(1x,A27,2x,6(D12.6,1x))') 
     &     'EL,TOT,REAC,INEL,CC,CSFus:',
     &   1000.d0*CSN(1), 1000.d0*CST, 1000.d0*CSR, 0.d0, 
     &   1000.d0*SINLcc, 1000.d0*(CSR-SINLcc)
C       WRITE (46,'(1x,I6)') 123456 
C       DO l = 1, ltlmax
C         WRITE (46,*) l-1, SNGL(sel(l))
C       ENDDO
        CLOSE (46)

        OPEN (45,FILE = trim(fname)//'.INC',FORM = 'UNFORMATTED')
        IF (MEREL.EQ.0) then 
          itmp = 0 
          WRITE (45) ltlmax, EN, itmp
        ELSE
          itmp = 1 
          WRITE (45) ltlmax, EN, itmp
        ENDIF
        DO l = 0, ltlmax
           WRITE (45) stl(l+1)
        ENDDO
        WRITE (45) 
     &   1000.d0*CSN(1), 1000.d0*CST, 1000.d0*CSR, 0.d0, 
     &   1000.d0*SINLcc, 1000.d0*(CSR-SINLcc)
C
C       A new flag is introduced to signal storage of the Shape elastic XS (Sel(L))
C
C       l = 123456
C       WRITE (45) l 
C       DO l = 1, ltlmax
C         WRITE (45) sel(l)
C       ENDDO
        CLOSE (45)

C***********************************************************************
C  Kinematics:   lab  ===>  CM                                         *
C    With relativistic kinematics, the reduced mass is replaced by     *
C    the reduced total energy                                          *
C----------------------------------------------------------------------*
C  EN     = current lab kinetic energy                                 *
C  ecms   = current  CM kinetic energy                                 *
C  AI     = incident particle rest mass (in a.m.u.)                    *
C  AT     = target   nucleus  rest mass (in a.m.u.)                    *
C  AK2    = CM wave number                                             *
C----------------------------------------------------------------------*
C  AMUmev = a.m.u. in MeV                                              *
C----------------------------------------------------------------------*
C-------CONSTANTS COMPUTED FROM THE FUNDAMENTAL CONSTANTS, ATOMIC MASS, HBAR*C
C-------AND ALPHA, AS GIVEN IN THE EUROPEAN PHYSICAL JOURNAL, PAGE 73, VOLUME
C-------15 (2000) REFERRING FOR THESE VALUES TO THE 1998 CODATA SET WHICH MAY
C-------BE FOUND AT http://physics.nist.gov/constants
C-------CM=931.494013 +/- 0.000037 MeV
C-------The above value is the one used also in the ENDF-6 manual (April 2001, 2009)
C       AMUmev = 9.31494013D+02
C       CARBUN=931.49378D0  , this is the OPTMAN mass unit
        AMUmev = 9.31494013D+02
C-------CHB=197.3269601 +/- 0.0000078 (*1.0E-9 eV*cm)
        HHBarc = 197.3269601D0

        AI = ANEU
        ck2= (2.d0*AMUmev)/(HHBarc**2)
C
C-------From lab to CM (the input quantity is EN)
C
C
        IF (MEREL.EQ.0) then 
C---------Classical    kinematics
          ecms = EN*AT/(AI + AT)
          Ak2 = ck2*AI*AT/(AI + AT)*ecms
        ELSE
C
C---------Relativistic kinematics
          ecms = AMUmev*(AI + AT)*
     &           (SQRT(1.d0 + 2.d0*EN/(AMUmev*AT*((1.d0+AI/AT)**2)))
     &           - 1.d0)
          p2 = (EN*(EN + 2.d0*AMUmev*AI))
     &           /((1.d0 + AI/AT)**2 + 2.d0*EN/(AMUmev*AT))
          Ak2 = p2/(HHBarc*HHBarc)
        ENDIF
C       10 factor converts to mbarns
        dtmp = 10.D0*4.d0*atan(1.d0)/Ak2   
        sabs  =0.d0
        DO l = 0, ltlmax
          sabs   = sabs   + Stl(l + 1)*DBLE(2*l + 1)
        ENDDO
C       write(*,*) 'Test       :',dtmp*sabs,1000*(CSR-SINLcc)

C       Renormalizing TLs
        if(dtmp.gt.0.d0  .and. sabs.gt.0.d0) then
          DO l = 0, ltlmax
            Stl(l + 1)=Stl(l + 1)/(dtmp*sabs)*1000.d0*(CSR-SINLcc)
          ENDDO
          sabs  =0.d0
          DO l = 0, ltlmax
            sabs   = sabs   + Stl(l + 1)*DBLE(2*l + 1)
          ENDDO
C         write(*,*) 'Test renorm:',dtmp*sabs,1000*(CSR-SINLcc)
        endif

      ENDIF

      CSR=CSR-SINlcc

      RETURN
      END
C     *******************************************************
      SUBROUTINE CHLOG
C     *******************************************************
      IMPLICIT DOUBLE PRECISION(A-H,O-Z) 
      COMMON/LNP/JSS,NPIS,NPIO,JSO,NN1,LNO(180),NS1(180),JNO(180),NSPI
      N=0
      K1=IABS(JSO-JSS)+2
      K2=JSO+JSS+2
      DO 5 L=K1,K2,2
      N=N+1
      IF(NPIO*(-1)**((L-3)/2)*NPIS)2,1,1
    1 LNO(N)=(L-3)/2
      NS1(N)=1
      GO TO 5
    2 LNO(N)=(L-1)/2
      NS1(N)=2
    5 CONTINUE
    6 NN1=N
      RETURN
      END
C     *******************************************************
      SUBROUTINE RACAH
C     *******************************************************
      IMPLICIT DOUBLE PRECISION(A-H,O-Z) 
      COMMON/LOFAC/A(800)
      COMMON/RACB/JA,JB,JC,JD,JE,JF,W
      W=0.
      NB1=JA+JB+JC
      NB2=JC+JD+JE
      NB3=JA+JE+JF
      NB4=JB+JD+JF
      NM1=JA+JB+JD+JE
      NM2=JA+JC+JD+JF
      NM3=JB+JC+JE+JF
      NM=NM1
      NB=NB1
      IF(NM2-NM)1,2,2
    1 NM=NM2
    2 IF(NM3-NM)3,4,4
    3 NM=NM3
    4 IF(NB2-NB)5,5,6
    6 NB=NB2
    5 IF(NB3-NB)7,7,8
    8 NB=NB3
    7 IF(NB4-NB)9,9,10
   10 NB=NB4
    9 IF(NB-NM)12,12,11
   12 NF=JA+JB-JC
      FLN=A(NF+2)
      CL=FLN
      NF=JA-JB+JC
      FLN=A(NF+2)
      CL=CL+FLN
      NF=JB+JC-JA
      FLN=A(NF+2)
      CL=CL+FLN
      NF=NB1+2
      FLN=A(NF+2)
      CL=CL-FLN
      NF=JC+JD-JE
      FLN=A(NF+2)
      CL=CL+FLN
      NF=JC-JD+JE
      FLN=A(NF+2)
      CL=CL+FLN
      NF=JD+JE-JC
      FLN=A(NF+2)
      CL=CL+FLN
      NF=NB2+2
      FLN=A(NF+2)
      CL=CL-FLN
      NF=JA+JE-JF
      FLN=A(NF+2)
      CL=CL+FLN
      NF=JA-JE+JF
      FLN=A(NF+2)
      CL=CL+FLN
      NF=JE+JF-JA
      FLN=A(NF+2)
      CL=CL+FLN
      NF=NB3+2
      FLN=A(NF+2)
      CL=CL-FLN
      NF=JB+JD-JF
      FLN=A(NF+2)
      CL=CL+FLN
      NF=JB-JD+JF
      FLN=A(NF+2)
      CL=CL+FLN
      NF=JD+JF-JB
      FLN=A(NF+2)
      CL=CL+FLN
      NF=NB4+2
      FLN=A(NF+2)
      CL=0.5*(CL-FLN)
      NB=NB+2
      NM=NM+2
      DO 13 K=NB,NM,2
      C1=1.
      NF=K
      FLN=A(NF+2)
      CL1=CL+FLN
      I=K-2
      NF=I-NB1
      FLN=A(NF+2)
      CL1=CL1-FLN
      NF=I-NB2
      FLN=A(NF+2)
      CL1=CL1-FLN
      NF=I-NB3
      FLN=A(NF+2)
      CL1=CL1-FLN
      NF=I-NB4
      FLN=A(NF+2)
      CL1=CL1-FLN
      NF=NM1-I
      FLN=A(NF+2)
      CL1=CL1-FLN
      NF=NM2-I
      FLN=A(NF+2)
      CL1=CL1-FLN
      NF=NM3-I
      FLN=A(NF+2)
      CL1=CL1-FLN
      IF((NM1+I)/4*4.NE.NM1+I) C1=-1.
   13 W=W+C1*EXP(CL1)
   11 RETURN
      END
C     *******************************************************
      SUBROUTINE SOSIT
C     *******************************************************
      IMPLICIT DOUBLE PRECISION(A-H,O-Z) 
C     BELOW CARD IS NECESSARY IF MEMORY IS LESS THAN 32Mb
      DOUBLE PRECISION VL,VSL,PV,PW,WSL,PVC,PRC
      COMMON/QNS1/LNJ1(250),JNJ1(250),NNJ1(250),KPJ1(250)
     */ENA/EN,EL(40),BET(10),NUR,NMAX,NPD,LAS
      COMMON/JNN/CSS,INCC,NCLL,NSS,NJ,INCR
      COMMON/MENU/MEJOB,MEPOT,MEHAM,MECHA,MEPRI,MESOL,MESHA,MESHO,MEHAO
     *,MEAPP,MEVOL,MEREL,MECUL,MERZZ,MERRR,MEDIS,MERIP
     */STR/STEP,RK,NH1
     */FFV/FRF1(200,200),FRF2(200,200),FIF1(200,200),FIF2(200,200)
      COMMON/RAD/RR,RC,RD,RW,RS,AR,AC,AW,AD,AS,ALF,AT,ANEU,RZ,ZNUC,ASP
     *,AZ
      COMMON/POTB/WNK(40),WN(40),VR,WC,WD
     */CV/CVNR(160000)
      COMMON/CVPN/CVNRPN(40000)
     */AB/DEF(300),VL(300),VSL(480000),POL(5,300),
     *PV(2400000),PW(2400000),WSL(480000)
     */ABCOUL/PVC(5400),PRC(5400),CVNC(720000)
     */VRI/X4(40000),X5(40000),X6(40000),Y4(40000),Y5(40000),Y6(40000)
     *,VR1(40000),VR2(40000),VR3(40000),VR4(40000),VI1(40000),VI2(40000)
     *,VI3(40000),VI4(40000)
     */VRII/VR5E(200),VI5E(200)
     */CONT/P(40000),R(40000),W(40000),V(40000),E(40000),F(40000),
     *S(40000),B(40000),Q(40000),T(40000),G(40000),Z(40000)
      COMMON/FUEC/FREM(300,40000),FIEM(300,40000)
      COMMON/POTPN/PVV(120000),PWW(120000),PRR(120000),PII(120000)
      COMMON/QNB/JO(40),NPO(40),KO(40),NCA(40)
     */CVOL/CBET0

C     WRITE(21,'(1X,11I6,1X,6(f8.4,1X))') NCLL

      SPI2=ASP*(ASP+1.D0)
      NCL2=NCLL*NCLL
      IF(MEPOT.EQ.1) GO TO 27
      LAS2=LAS+1
      LAS1=LAS
      GO TO 26
   27 LAS2=(LAS+2)/2
      LAS1=LAS2-1
   26 NCLA=NCLL*LAS1
      LAS8=9
      LASC=1
      IF(LAS2.GE.3) LAS8=18
      IF(LAS2.GE.3) LASC=2
      ICLL=NCLL*LAS8
      MNULA=300*NUR*LAS2
      MLA=300*LAS2
      MNU=300*NUR
      NH=NH1
      ST=1.
      M=8
      M4=4*M
      MF=M4-1
      STEP1=STEP/M

C     K-NUMBER OF INDEPENDENT SOLUTION

      DO 1 K=1,NCLL
      NCLK=(K-1)*NCLL
      KC1=ICLL*(K-1)
      K1=(K-1)*LAS1-1
      ST2=STEP1*STEP1/240.
      LL1=LNJ1(K)
      NPI1=KPJ1(K)
      NUK=NNJ1(K)
      MNULAK=MNULA*(NUK-1)
      MNUK=MNU*(NUK-1)
      C=ST*(STEP1)**(LL1+1)
      C2=ST*(2.*STEP1)**(LL1+1)
      C3=ST*(3.*STEP1)**(LL1+1)

C     L-LINE OF INDEPENDENT SOLUTION

      DO 2 L=1,NCLL
      KL=NCLK+L
      KC2=KC1+LAS8*(L-1)
      K2=K1+(L-1)*NCLA
      LL=LNJ1(L)
      NUL=NNJ1(L)
      NU=NUL
      MNULKL=MNULAK+MLA*(NUL-1)
      MNUKL=MNUK+300*(NUL-1)
      NPI2=KPJ1(L)
      JJ=JNJ1(L)
      CJ1=JJ/2.
      SOP=(CJ1-LL)*(CJ1+LL+1)-SPI2
      X4(KL)=0.
      Y4(KL)=0.
      X5(KL)=0.
      Y5(KL)=0.
      VR1(KL)=0.
      VI1(KL)=0.
      UR=0.
      UI=0.

      
      
      C=CVNRPN(KL)
C     IF(NU.NE.NUN.AND.JO(NU).EQ.JO(NUN).AND. JO(NU).EQ.0) C=1.D0
      IF(NCA(NU).NE.NCA(NUK).AND.JO(NU).EQ.JO(NUK))UR=UR+PV(MNULKL+1)*C
      IF(NCA(NU).NE.NCA(NUK).AND.JO(NU).EQ.JO(NUK))UI=UI+PW(MNULKL+1)*C
      
      
      
      
C     IF(NU.NE.NUK.AND.JO(NU).EQ. JO(NUK)) UR=UR+PVV(MNUKL+1)*CVNRPN(KL)
C     IF(NU.NE.NUK.AND.JO(NU).EQ. JO(NUK)) UI=UI+PWW(MNUKL+1)*CVNRPN(KL)



         ACS=0.
      IF(LAS2.LT.2) GO TO 300
      DO 30 LLL=2,LAS2
      LP1=MNULKL+LLL
      PV1=PV(LP1)
      PW1=PW(LP1)
         IF(LLL.EQ.2) PVS=PV1
         IF(LLL.EQ.2) PWS=PW1
      LL2=K2+LLL
      A=CVNR(LL2)
      IF(NPI1.NE.NPI2) GO TO 36
      UR=UR+A*PV1
      UI=UI+A*PW1
      GO TO 41
   36 UR=UR-A*PW1
      UI=UI+A*PV1
   41 IF(MEPOT.EQ.1) GO TO 30
      IF(LLL.GT.3) GO TO 30
      LAI=3
      LAF=5
      IF(LLL.EQ.3) LAI=1
      IF(LLL.EQ.3) LAF=9
      DO 43 LA=LAI,LAF
      LALAS=LASC*(LA-1)+LLL-1
      LP1=LALAS
      PV1=PVC(LP1)
      LL2=KC2+LALAS
      A=CVNC(LL2)
          IF(LLL.EQ.3.AND.LA.EQ.1) ACS=-A*CBET0
          IF(LA.EQ.1) GO TO 43
      IF(NPI1.NE.NPI2) GO TO 42
      UR=UR+A*PV1
      GO TO 43
   42 UI=UI+A*PV1
43    CONTINUE
30    CONTINUE
      IF(MEVOL.EQ.0) GO TO 300
          UR=UR+ACS*PVS
          UI=UI+ACS*PWS
300   POTR2=PV(MNULKL+1)+VSL(MNUKL+1)*SOP+VL(1)*LL*(LL+1)-WN(NU)
      IF(L-K) 3,4,3
    4 X4(KL)=ST*(3.*STEP1)**(LL+1)
      X5(KL)=ST*(4.*STEP1)**(LL+1)
      UR=UR+POTR2
      UI=UI+PW(MNULKL+1)+WSL(MNUKL+1)*SOP
      IF(LL.EQ.1) VR1(KL)=2.*ST
    3 VR2(KL)=UR*C
      VI2(KL)=UI*C
      UR=0.
      UI=0.
      



      C=CVNRPN(KL)
C     IF(NU.NE.NUN.AND.JO(NU).EQ.JO(NUN).AND. JO(NU).EQ.0) C=1.D0
      IF(NCA(NU).NE.NCA(NUK).AND.JO(NU).EQ.JO(NUK))UR=UR+PV(MNULKL+2)*C
      IF(NCA(NU).NE.NCA(NUK).AND.JO(NU).EQ.JO(NUK))UI=UI+PW(MNULKL+2)*C
      

C     IF(NU.NE.NUK.AND.JO(NU).EQ. JO(NUK)) UR=UR+PVV(MNUKL+2)*CVNRPN(KL)
C     IF(NU.NE.NUK.AND.JO(NU).EQ. JO(NUK)) UI=UI+PWW(MNUKL+2)*CVNRPN(KL)

      
      
          ACS=0.
      IF(LAS2.LT.2) GO TO 310
      DO 31 LLL=2,LAS2
      LL2=LAS2+LLL+MNULKL
      PV1=PV(LL2)
      PW1=PW(LL2)
         IF(LLL.EQ.2) PVS=PV1
         IF(LLL.EQ.2) PWS=PW1
      LL2=K2+LLL
      A=CVNR(LL2)
      IF(NPI1.NE.NPI2) GO TO 37
      UR=UR+A*PV1
      UI=UI+A*PW1
      GO TO 44
   37 UR=UR-A*PW1
      UI=UI+A*PV1
   44 IF(MEPOT.EQ.1) GO TO 31
      IF(LLL.GT.3) GO TO 31
      LAI=3
      LAF=5
      IF(LLL.EQ.3) LAI=1
      IF(LLL.EQ.3) LAF=9
      DO 45 LA=LAI,LAF
      LALAS=LASC*(LA-1)+LLL-1
      LP1=LALAS+LAS8
      PV1=PVC(LP1)
      LL2=KC2+LALAS
      A=CVNC(LL2)
          IF(LLL.EQ.3.AND.LA.EQ.1) ACS=-A*CBET0
          IF(LA.EQ.1) GO TO 45
      IF(NPI1.NE.NPI2) GO TO 46
      UR=UR+A*PV1
      GO TO 45
   46 UI=UI+A*PV1
   45 CONTINUE
   31 CONTINUE
      IF(MEVOL.EQ.0) GO TO 310
          UR=UR+ACS*PVS
          UI=UI+ACS*PWS
310   LL2=LAS2+1+MNULKL
      POTR2=PV(LL2)+VSL(MNUKL+2)*SOP+VL(2)*LL*(LL+1)-WN(NU)
      IF(L.EQ.K) UR=UR+POTR2
      IF(L.EQ.K) UI=UI+PW(LL2)+WSL(MNUKL+2)*SOP
      VR3(KL)=UR*C2
      VI3(KL)=UI*C2
      UR=0.
      UI=0.



      C=CVNRPN(KL)
C     IF(NU.NE.NUN.AND.JO(NU).EQ.JO(NUN).AND. JO(NU).EQ.0) C=1.D0
      IF(NCA(NU).NE.NCA(NUK).AND.JO(NU).EQ.JO(NUK))UR=UR+PV(MNULKL+3)*C
      IF(NCA(NU).NE.NCA(NUK).AND.JO(NU).EQ.JO(NUK))UI=UI+PW(MNULKL+3)*C
      

C      IF(NU.NE.NUK.AND.JO(NU).EQ. JO(NUK)) UR=UR+PVV(MNUKL+3)*CVNRPN(KL)
C     IF(NU.NE.NUK.AND.JO(NU).EQ. JO(NUK)) UI=UI+PWW(MNUKL+3)*CVNRPN(KL)

C      WRITE(21,'(1X,3I6,1X,6(f8.4,1X))') k,l,ll,PV(LL2),VSL(MNUKL+2),
C     *SOP,VL(2)

C     IF(NU.NE.NUK.AND.JO(NU).EQ. JO(NUK))
C     *     WRITE(21,'(1X,11I6,1X,9(f8.4,1X))') NCLL,NU,NUK,KL, MNUKL,
C     *JNJ1(L), JNJ1(K),LNJ1(L),LNJ1(K), JO(NU),
C     *jO(NUK),PVV(MNUKL+3),CVNRPN(KL)




           ACS=0.
      IF(LAS2.LT.2) GO TO 320
      DO 32 LLL=2,LAS2
      LL2=LAS2*2+LLL+MNULKL
      PV1=PV(LL2)
      PW1=PW(LL2)
         IF(LLL.EQ.2) PVS=PV1
         IF(LLL.EQ.2) PWS=PW1
      LL2=K2+LLL
      A=CVNR(LL2)
      IF(NPI1.NE.NPI2) GO TO 38
      UR=UR+A*PV1
      UI=UI+A*PW1
      GO TO 47
   38 UR=UR-A*PW1
      UI=UI+A*PV1
   47 IF(MEPOT.EQ.1) GO TO 32
      IF(LLL.GT.3) GO TO 32
      LAI=3
      LAF=5
      IF(LLL.EQ.3) LAI=1
      IF(LLL.EQ.3) LAF=9
      DO 48 LA=LAI,LAF
      LALAS=LASC*(LA-1)+LLL-1
      LP1=LALAS+LAS8*2
      PV1=PVC(LP1)
      LL2=KC2+LALAS
      A=CVNC(LL2)
          IF(LLL.EQ.3.AND.LA.EQ.1) ACS=-A*CBET0
          IF(LA.EQ.1) GO TO 48
      IF(NPI1.NE.NPI2) GO TO 49
      UR=UR+A*PV1
      GO TO 48
   49 UI=UI+A*PV1
   48 CONTINUE
   32 CONTINUE
      IF(MEVOL.EQ.0) GO TO 320
          UR=UR+ACS*PVS
          UI=UI+ACS*PWS
320   LL2=LAS2*2+1+MNULKL
      POTR2=PV(LL2)+VSL(MNUKL+3)*SOP+VL(3)*LL*(LL+1)-WN(NU)
      IF(L.EQ.K) UR=UR+POTR2
      IF(L.EQ.K) UI=UI+PW(LL2)+WSL(MNUKL+3)*SOP
      VR4(KL)=UR*C3
      VI4(KL)=UI*C3
      P(KL)=VR1(KL)
      R(KL)=VI1(KL)
    2 CONTINUE
    1 CONTINUE
      DO 5 MM=4,MF
      MM1=(MM-1)*LAS2
      MM2=MM1+1
      MMC=LAS8*(MM-1)
C     L-LINE OF INDEPENDENT SOLUTION

      DO 15 L=1,NCLL
      LPN1=(L-1)*NCLL
      KC1=ICLL*(L-1)
      K1=(L-1)*NCLA-1
      DO 6 K=1,NCLL
      VR5E(K)=0.
    6 VI5E(K)=0.
      LL=LNJ1(L)
      JJ=JNJ1(L)
      NU=NNJ1(L)
      MNULAL=MNULA*(NU-1)
      MNUL=MNU*(NU-1)
      NPI1=KPJ1(L)
      CJ1=JJ/2.
      SOP=(CJ1-LL)*(CJ1+LL+1)-SPI2

C     N-NUMBER OF LINE OF SOLUTION COUPLED WITH L-LINE

      DO 7 N=1,NCLL
      LN=LPN1+N
      KC2=KC1+LAS8*(N-1)
      K2=K1+(N-1)*LAS1
      NPI2=KPJ1(N)
      NUN=NNJ1(N)
      MNULLN=MNULAL+MLA*(NUN-1)
      MNULN=MNUL+300*(NUN-1)
      POTR5=PV(MNULLN+MM2)+VSL(MNULN+MM)*SOP+VL(MM)*LL*(LL+1)-WN(NU)
      UR=0.
      UI=0.
      C=CVNRPN(LN)
C     IF(NU.NE.NUN.AND.JO(NU).EQ.JO(NUN).AND. JO(NU).EQ.0) C=1.D0
      IF(NCA(NU).NE.NCA(NUN).AND.JO(NU).EQ.JO(NUN))
     *UR=UR+PV(MNULLN+MM2)*C
      IF(NCA(NU).NE.NCA(NUN).AND.JO(NU).EQ.JO(NUN))
     *UI=UI+PW(MNULLN+MM2)*C

C     write (21,9797) mm,ur, ui,2.2, l,n
C 9797 format(i5, 3e12.5,2i5)

C      WRITE(21,'(1X,4I6,1X,6(f8.4,1X))')mm,l,n

C     WRITE(21,'(1X,4I6,1X,6(f8.4,1X))')mm,l,n,ll,
C     *PV(MNULLN+MM2),pw(MNULLN+MM2),VSL(MNUKL+mm),SOP,VL(mm)

C     IF(NU.NE.NUn.AND.JO(NU).EQ. JO(NUn))
C     *     WRITE(21,'(1X,12I5,1X,6(f8.4,1X))') NCLL,NU,NUn,ln, MNUln,
C     *JNJ1(L), JNJ1(n),LNJ1(L),LNJ1(n), JO(NU),
C     *jO(NUn),mm, PVV(MNUln+mm),Pww(MNUln+mm),CVNRPN(ln),WN(NU)


          ACS=0.
      IF(LAS2.LT.2) GO TO 330
      DO 33 LLL=2,LAS2
      LL2=MM1+LLL+MNULLN
      PV1=PV(LL2)
      PW1=PW(LL2)
         IF(LLL.EQ.2) PVS=PV1
         IF(LLL.EQ.2) PWS=PW1
      LL2=K2+LLL
      A=CVNR(LL2)
      IF(NPI1.NE.NPI2) GO TO 39
      UR=UR+A*PV1
      UI=UI+A*PW1
C     DIAGONAL COUPLING FOR (P,N)




      GO TO 50
   39 UR=UR-A*PW1
      UI=UI+A*PV1
   50 IF(MEPOT.EQ.1) GO TO 33
      IF(LLL.GT.3) GO TO 33
      LAI=3
      LAF=5
      IF(LLL.EQ.3) LAI=1
      IF(LLL.EQ.3) LAF=9
      DO 51 LA=LAI,LAF
      LALAS=LASC*(LA-1)+LLL-1
      LP1=LALAS+MMC
      PV1=PVC(LP1)
      LL2=KC2+LALAS
      A=CVNC(LL2)
          IF(LLL.EQ.3.AND.LA.EQ.1) ACS=-A*CBET0
          IF(LA.EQ.1) GO TO 51
      IF(NPI1.NE.NPI2) GO TO 52
      UR=UR+A*PV1
      GO TO 51
   52 UI=UI+A*PV1
   51 CONTINUE
   33 CONTINUE
      IF(MEVOL.EQ.0) GO TO 330
          UR=UR+ACS*PVS
          UI=UI+ACS*PWS
330   IF(L.EQ.N) UR=UR+POTR5
      IF(L.EQ.N) UI=UI+PW(MM2+MNULLN)+WSL(MNULN+MM)*SOP


C      write (21,9797) mm,ur, ui, potr5,l,n

      DO 7 K=1,NCLL
      KN=(K-1)*NCLL+N
      X5N=X5(KN)
      Y5N=Y5(KN)
      VR5E(K)=VR5E(K)+UR*X5N-UI*Y5N
    7 VI5E(K)=VI5E(K)+UR*Y5N+UI*X5N

C     K-NUMBER OF INDEPENDENT SOLUTION

      DO 19 K=1,NCLL
      NCLK=(K-1)*NCLL
      KL=NCLK+L
      VR5L=VR5E(K)
      VI5L=VI5E(K)
      WR2=VR2(KL)
      WR3=VR3(KL)
      WR4=VR4(KL)
      WI2=VI2(KL)
      WI3=VI3(KL)
      WI4=VI4(KL)
      XW5=X5(KL)
      YW5=Y5(KL)
      X6L=2.*XW5-X4(KL)+ST2*(299.*VR5L-176.*WR4+194.*WR3-
     *96.*WR2+19.*VR1(KL))
      Y6L=2.*YW5-Y4(KL)+ST2*(299.*VI5L-176.*WI4+194.*WI3-
     *96.*WI2+19.*VI1(KL))
      VR1(KL)=WR2
      VR2(KL)=WR3
      VR3(KL)=WR4
      VR4(KL)=VR5L
      VI1(KL)=WI2
      VI2(KL)=WI3
      VI3(KL)=WI4
      VI4(KL)=VI5L
      X4(KL)=XW5
      Y4(KL)=YW5
      X6(KL)=X6L
      Y6(KL)=Y6L
   19 CONTINUE
   15 CONTINUE
      IF(MM/M*M.NE.MM) GO TO 12
      IF(MM-2*M) 8,9,10
    8 DO 901 NN=1,NCL2
      FREM(1,NN)=0.
       FIEM(1,NN)=0.
      FREM(2,NN)=X6L
       FIEM(2,NN)=Y6L
      W(NN)=VR4(NN)
  901 V(NN)=VI4(NN)
      GO TO 302
    9 DO 910 NN=1,NCL2
      FREM(3,NN)=X6L
       FIEM(3,NN)=Y6L
      S(NN)=VR4(NN)
  910 B(NN)=VI4(NN)
      GO TO 302
   10 IF(MM-3*M) 11,11,302
   11 DO 902 NN=1,NCL2
      FREM(4,NN)=X6L
       FIEM(4,NN)=Y6L
      G(NN)=VR4(NN)
      Z(NN)=VI4(NN)
      E(NN)=X4(NN)
  902 F(NN)=Y4(NN)
      GO TO 302
   12 IF(MM.NE.MF) GO TO 302
      DO 911 NN=1,NCL2
       FREM(5,NN)=X6L
       FIEM(5,NN)=Y6L
      Q(NN)=X6(NN)
  911 T(NN)=Y6(NN)
  302 DO 24 L=1,NCL2
      X5(L)=X6(L)
   24 Y5(L)=Y6(L)
    5 CONTINUE
      DO 800 NB=1,NCL2
      VR1(NB)=P(NB)
      VI1(NB)=R(NB)
      VR2(NB)=W(NB)
      VI2(NB)=V(NB)
      VR3(NB)=S(NB)
      VI3(NB)=B(NB)
      VR4(NB)=G(NB)
      VI4(NB)=Z(NB)
      X4(NB)=E(NB)
      Y4(NB)=F(NB)
      X5(NB)=Q(NB)
      Y5(NB)=T(NB)
 800  CONTINUE
      ST2=STEP*STEP/240.
       NHTT=5
      DO 13 MM=M4,NH
       NHTT=NHTT+1
      MM1=(MM-1)*LAS2
      MMC=LAS8*(MM-1)
      MM2=MM1+1
      DO 17 L=1,NCLL
      LPN1=(L-1)*NCLL
      KC1=ICLL*(L-1)
      K1=(L-1)*NCLA-1
      DO 16 K=1,NCLL
      VR5E(K)=0.
   16 VI5E(K)=0.
      LL=LNJ1(L)
      JJ=JNJ1(L)
      NU=NNJ1(L)
      MNULAL=MNULA*(NU-1)
      MNUL=MNU*(NU-1)
      NPI1=KPJ1(L)
      CJ1=JJ/2.
      SOP=(CJ1-LL)*(CJ1+LL+1)-SPI2
      DO 14 N=1,NCLL
      LN=LPN1+N
      NPI2=KPJ1(N)
      NUN=NNJ1(N)
      MNULLN=MNULAL+MLA*(NUN-1)
      MNULN=MNUL+300*(NUN-1)
      POTR5=PV(MNULLN+MM2)+VSL(MNULN+MM)*SOP+VL(MM)*LL*(LL+1)-WN(NU)
      KC2=KC1+LAS8*(N-1)
      K2=K1+(N-1)*LAS1
      UR=0.
      UI=0.

      C=CVNRPN(LN)
c      IF(NU.NE.NUN.AND.JO(NU).EQ.JO(NUN).AND. JO(NU).EQ.0) C=1.D0
      IF(NCA(NU).NE.NCA(NUN).AND.JO(NU).EQ.JO(NUN))
     *UR=UR+PV(MNULLN+MM2)*C
      IF(NCA(NU).NE.NCA(NUN).AND.JO(NU).EQ.JO(NUN))
     *UI=UI+PW(MNULLN+MM2)*C

C           write (21,9797) mm,ur, ui,2.2, l,n
C 
C      WRITE(21,'(1X,4I6,1X,6(f8.4,1X))')mm,l,n

C     WRITE(21,'(1X,4I6,1X,6(f8.4,1X))')mm,l,n,ll,
C     *PV(MNULLN+MM2),pw(MNULLN+MM2),VSL(MNUKL+mm),SOP,VL(mm)

C     IF(NU.NE.NUn.AND.JO(NU).EQ. JO(NUn))
C     *     WRITE(21,'(1X,12I5,1X,6(f8.4,1X))') NCLL,NU,NUn,ln, MNUln,
C     *JNJ1(L), JNJ1(n),LNJ1(L),LNJ1(n), JO(NU),
C     *jO(NUn),mm, PVV(MNUln+mm),Pww(MNUln+mm),CVNRPN(ln)




         ACS=0.
      IF(LAS2.LT.2) GO TO 340
      DO 34 LLL=2,LAS2
      LL2=MM1+LLL+MNULLN
      PV1=PV(LL2)
      PW1=PW(LL2)
         IF(LLL.EQ.2) PVS=PV1
         IF(LLL.EQ.2) PWS=PW1
      LL2=K2+LLL
      A=CVNR(LL2)
      IF(NPI1.NE.NPI2) GO TO 40
      UR=UR+A*PV1
      UI=UI+A*PW1

C     DIAGONAL COUPLING FOR (P,N)

      


      GO TO 53
   40 UR=UR-A*PW1
      UI=UI+A*PV1
   53 IF(MEPOT.EQ.1) GO TO 34
      IF(LLL.GT.3) GO TO 34
      LAI=3
      LAF=5
      IF(LLL.EQ.3) LAI=1
      IF(LLL.EQ.3) LAF=9
      DO 54 LA=LAI,LAF
      LALAS=LASC*(LA-1)+LLL-1
      LP1=LALAS+MMC
      PV1=PVC(LP1)
      LL2=KC2+LALAS
      A=CVNC(LL2)
          IF(LLL.EQ.3.AND.LA.EQ.1) ACS=-A*CBET0
          IF(LA.EQ.1) GO TO 54
      IF(NPI1.NE.NPI2) GO TO 55
      UR=UR+A*PV1
      GO TO 54
   55 UI=UI+A*PV1
   54 CONTINUE
   34 CONTINUE
      IF(MEVOL.EQ.0) GO TO 340
          UR=UR+ACS*PVS
          UI=UI+ACS*PWS
340   IF(L.EQ.N) UR=UR+POTR5
      IF(L.EQ.N) UI=UI+PW(MM2+MNULLN)+WSL(MNULN+MM)*SOP
      DO 14 K=1,NCLL
      KN=(K-1)*NCLL+N
      X5N=X5(KN)
      Y5N=Y5(KN)
      VR5E(K)=VR5E(K)+UR*X5N-UI*Y5N
   14 VI5E(K)=VI5E(K)+UR*Y5N+UI*X5N
      DO 18 K=1,NCLL
      NCLK=(K-1)*NCLL
      KL=NCLK+L
      VR5L=VR5E(K)
      VI5L=VI5E(K)
      WR2=VR2(KL)
      WR3=VR3(KL)
      WR4=VR4(KL)
      WI2=VI2(KL)
      WI3=VI3(KL)
      WI4=VI4(KL)
      XW5=X5(KL)
      YW5=Y5(KL)
      X6L=2.*XW5-X4(KL)+ST2*(299.*VR5L-176.*WR4+194.*WR3-
     *96.*WR2+19.*VR1(KL))
      Y6L=2.*YW5-Y4(KL)+ST2*(299.*VI5L-176.*WI4+194.*WI3-
     *96.*WI2+19.*VI1(KL))
      VR1(KL)=WR2
      VR2(KL)=WR3
      VR3(KL)=WR4
      VR4(KL)=VR5L
      VI1(KL)=WI2
      VI2(KL)=WI3
      VI3(KL)=WI4
      VI4(KL)=VI5L
      X4(KL)=XW5
      Y4(KL)=YW5
      X6(KL)=X6L
      Y6(KL)=Y6L
       FREM(NHTT,KL)=X6L
       FIEM(NHTT,KL)=Y6L
      IF(MM-NH+2) 18,21,22
   21 FRF1(K,L)=X6L
      FIF1(K,L)=Y6L
      GO TO 18
   22 IF(MM-NH) 18,23,18
   23 FRF2(K,L)=X6L
      FIF2(K,L)=Y6L
   18 CONTINUE
   17 CONTINUE
      DO 25 L=1,NCL2
      X5(L)=X6(L)
   25 Y5(L)=Y6(L)
      IF(NCLL.LE.20) GO TO 13
      IF(MM.NE.M4+2) GO TO 13
      DO 20 K=1,NCLL
      NCLK=(K-1)*NCLL
      KK=NCLK+K
      LL1=LNJ1(K)
      AA=(STEP*(MM+1)*2.178281/(2.*LL+1.))**(LL1+1)/X5(KK)
      DO 20 L=1,NCLL
      KL=NCLK+L
      X4(KL)=X4(KL)*AA
      X5(KL)=X5(KL)*AA
      Y4(KL)=Y4(KL)*AA
      Y5(KL)=Y5(KL)*AA
      VR1(KL)=VR1(KL)*AA
      VR2(KL)=VR2(KL)*AA
      VR3(KL)=VR3(KL)*AA
      VR4(KL)=VR4(KL)*AA
      VI1(KL)=VI1(KL)*AA
      VI2(KL)=VI2(KL)*AA
      VI3(KL)=VI3(KL)*AA
      VI4(KL)=VI4(KL)*AA
   20 CONTINUE
   13 CONTINUE
      RETURN
      END
C     *******************************************************
      SUBROUTINE MASCT
C     *******************************************************
      IMPLICIT DOUBLE PRECISION(A-H,O-Z) 
      REAL*16 FF1,FF2,GG1,GG2,RF1,RF2,F1I,F2I,RG1,RG2,G1I,G2I,ZNI,
     *C1,C2,D1,D2,BMR,BMI,ABR,ABI
      COMMON/MENU/MEJOB,MEPOT,MEHAM,MECHA,MEPRI,MESOL,MESHA,MESHO,MEHAO
     *,MEAPP,MEVOL,MEREL,MECUL,MERZZ,MERRR,MEDIS,MERIP
      COMMON/POTB/WNK(40),WN(40),VR,WC,WD
      COMMON/CMAT/CR(200,200),CI(200,200)
     */ENA/EN,EL(40),BET(10),NUR,NMAX,NPD,LAS
     */FFV/FRF1(200,200),FRF2(200,200),FIF1(200,200),FIF2(200,200)
      COMMON/JNN/CSS,INCC,NCLL,NSS,NJ,INCR
      COMMON/QNS1/LNJ1(250),JNJ1(250),NNJ1(250),KPJ1(250)
     */FBN/FBR1(40,90),FBI1(40,90),FNR2(40,90),FNI2(40,90),FNR1(40,90),
     *FNI1(40,90),FBR2(40,90),FBI2(40,90)
     */CCMAT/CRD(180,10,200),CID(180,10,200)
      COMMON/CVINV/BMR(40000),BMI(40000),ABR(40000),ABI(40000)
      COMMON/AUK/AUR(400),AUI(400)
      COMMON/INF/INFOR
      CSS=0.
      DO 1 I=1,NCLL
      NR=NNJ1(I)
      L=LNJ1(I)+1
      IF(NR.GT.NMAX) GO TO 5
      FF1=FBR1(NR,L)
      FF2=FBR2(NR,L)
      GG1=FNR1(NR,L)
      GG2=FNR2(NR,L)
      ZN=FF2*GG1-FF1*GG2
      GO TO 6
    5 RF1=FBR1(NR,L)
      RF2=FBR2(NR,L)
      F2I=FBI2(NR,L)
      F1I=FBI1(NR,L)
      RG1=FNR1(NR,L)
      RG2=FNR2(NR,L)
      G1I=FNI1(NR,L)
      G2I=FNI2(NR,L)
      ZNI=RF2*G1I+F2I*RG1-RF1*G2I-F1I*RG2
    6 DO 2 K=1,NCLL
      K1=(K-1)*NCLL+I
      C1=FRF1(K,I)
      C2=FRF2(K,I)
      D1=FIF1(K,I)
      D2=FIF2(K,I)
      IF(NR.GT.NMAX) GO TO 7
      BMR(K1)=(C2*FF1-C1*FF2)/ZN
      BMI(K1)=(D2*FF1-D1*FF2)/ZN
      ABR(K1)=(C2*GG1-C1*GG2)/ZN-BMI(K1)
      ABI(K1)=(D2*GG1-D1*GG2)/ZN+BMR(K1)
      GO TO 2
    7 BMR(K1)=(D2*RF1+C2*F1I-D1*RF2-C1*F2I)/ZNI
      BMI(K1)=(D2*F1I-C2*RF1-D1*F2I+C1*RF2)/ZNI
      ABR(K1)=(D2*RG1+C2*G1I-D1*RG2-C1*G2I)/ZNI-BMI(K1)
      ABI(K1)=(D2*G1I-C2*RG1-D1*G2I+C1*RG2)/ZNI+BMR(K1)
    2 CONTINUE
    1 CONTINUE
      INFOR=1
      CALL INMAT
      DO 3 I=1,NCLL
      I1=(I-1)*NCLL
      DO 3 K=1,NCLL
      NR=NNJ1(K)
      C=SQRT(WNK(NR)/WNK(1))
      C1=SQRT(ABS(WN(NR)/WN(1)))
      CCR=0.
      CCI=0.
      DO 4 L=1,NCLL
      IL=I1+L
      L1=(L-1)*NCLL
      LK=L1+K
      AABR=ABR(IL)
      BBMR=BMR(LK)
      AABI=ABI(IL)
      BBMI=BMI(LK)
      CCR=CCR-AABR*BBMR+AABI*BBMI
      CCI=CCI-AABR*BBMI-AABI*BBMR
    4 CONTINUE
      IF(I.GT.INCC.OR.K.GT.INCR) GO TO 13
      CSS=CSS+(CCR*CCR+CCI*CCI)*C1
C     IF(MEPRI.LT.99) PRINT 111,CCR,CCI
   13 CR(I,K)=CCR*C
      CI(I,K)=CCI*C
      IF(I.GT.10) GO TO 3
      CRD(NSS,I,K)=CCR*C
      CID(NSS,I,K)=CCI*C
    3 CONTINUE
      IF(MESOL.EQ.2) GO TO 14
      DO 9 I=1,NCLL
      I1=(I-1)*NCLL
      DO 9 K=1,NCLL
      IK=I1+K
      ABR(IK)=BMR(IK)
      ABI(IK)=BMI(IK)
    9 CONTINUE
      INFOR=2
      CALL INMAT
      DO 10 I=1,NCLL
      I1=(I-1)*NCLL
      DO 10 K=1,NCLL
      IK=I1+K
      AUR(IK)=0.
      AUI(IK)=0.
      DO 10 J=1,NCLL
      IJ=I1+J
      J1=(J-1)*NCLL
      JK=J1+K
      AUR(IK)=AUR(IK)+CR(I,J)*ABR(JK)-CI(I,J)*ABI(JK)
      AUI(IK)=AUI(IK)+CI(I,J)*ABR(JK)+CR(I,J)*ABI(JK)
   10 CONTINUE
C      DO 11 I=1,NCLL
C      I1=(I-1)*NCLL
C      DO 11 K=1,NCLL
C      IK=I1+K
C      ABR(IK)=AUR(IK)
C      ABI(IK)=AUI(IK)
C   11 CONTINUE
C      INFOR=3
C      CALL INMAT
C      DO 12 I=1,NCLL
C      I1=(I-1)*NCLL
C      DO 12 K=1,NCLL
C      IK=I1+K
C      AUR(IK)=ABR(IK)
C      AUI(IK)=ABI(IK)
C   12 CONTINUE
C     IF(MEPRI.NE.99) PRINT 967,CR(1,2),CI(1,2)
C 967 FORMAT(1X,'M1',2D20.7)
C     WRITE (16) CR,CI,LNJ1,JNJ1,NNJ1
C     IF(MEPRI.NE.99) PRINT 9998,
C    *   (LNJ1(I8),JNJ1(I8),NNJ1(I8),I8=1,NCLL),NCLL
C9999 FORMAT(10X,'MASCT')
C7999 FORMAT(10X,'MASC1')
C9998 FORMAT(3I5)
   14 RETURN
      END
C     *******************************************************
      SUBROUTINE INMAT
C     *******************************************************
      IMPLICIT DOUBLE PRECISION(A-H,O-Z) 
      REAL*16 BMR,BMI,ABR,ABI
      REAL*16 FR1,FR2,FI1,FI2
      REAL*16 FFR1,AB,FR
      COMMON/JNN/CSS,INCC,NCLL,NSS,NJ,INCR
      COMMON/CVINV/BMR(40000),BMI(40000),ABR(40000),ABI(40000)
      COMMON/IW/LG
     */FV/FR1(40000),FR2(40000),FI1(40000),FI2(40000)
      LG=NCLL
      DO 1 K=1,NCLL
      K1=(K-1)*NCLL
      DO 1 L=1,NCLL
      KL=K1+L
      FI1(KL)=ABI(KL)
    1 FI2(KL)=ABR(KL)
      CALL INVER
      DO 2 K=1,NCLL
      K1=(K-1)*NCLL
      DO 2 L=1,NCLL
      KL=K1+L
      FR2(KL)=FI2(KL)
      FFR1=ABR(KL)
      DO 5 M=1,NCLL
      KM=K1+M
      AB=ABI(KM)
      M1=(M-1)*NCLL
      DO 5 N=1,NCLL
      MN=M1+N
      NL=(N-1)*NCLL+L
    5 FFR1=FFR1+AB*FI2(MN)*FI1(NL)
    2 FR1(KL)=FFR1
      DO 3 K=1,NCLL
      K1=(K-1)*NCLL
      DO 3 L=1,NCLL
      KL=K1+L
    3 FI2(KL)=FR1(KL)
      CALL INVER
      DO 4 K=1,NCLL
      K1=(K-1)*NCLL
      DO 4 L=1,NCLL
      KL=K1+L
      ABR(KL)=FI2(KL)
      AB=0.
      DO 6 M=1,NCLL
      KM=K1+M
      FR=FR2(KM)
      M1=(M-1)*NCLL
      DO 6 N=1,NCLL
      MN=M1+N
      NL=(N-1)*NCLL+L
    6 AB=AB-FR*FI1(MN)*FI2(NL)
    4 ABI(KL)=AB
      RETURN
      END
C     *******************************************************
      SUBROUTINE INVER
C     *******************************************************
      IMPLICIT DOUBLE PRECISION(A-H,O-Z) 
      REAL*16 AMM,AMAI,AIAMAI
      REAL*16 FR1,FR2,FI1,FI2
      REAL*16 PIV,ELEM,SUMD,SUMN,AA,A
      COMMON/MENU/MEJOB,MEPOT,MEHAM,MECHA,MEPRI,MESOL,MESHA,MESHO,MEHAO
     *,MEAPP,MEVOL,MEREL,MECUL,MERZZ,MERRR,MEDIS,MERIP
      COMMON/IW/LG
     */FV/FR1(40000),FR2(40000),FI1(40000),FI2(40000)
      COMMON/INF/INFOR
      DIMENSION AMM(40000),AMAI(40000),AIAMAI(40000)     
C     DET=1.0
      EPS=1.0D-7
      IR=1
      LG2=LG*LG
      DO 1 I=1,LG2
    1 AMM(I)=FI2(I)
      DO 60 K=1,LG
      K1G=(K-1)*LG
      KK=K1G+K
C     DET=DET*FI2(KK)
      PIV=1.0D+00/FI2(KK)
      DO 20 J=1,LG
      KJ=K1G+J
   20 FI2(KJ)=FI2(KJ)*PIV
      FI2(KK)=PIV
      DO 50 I=1,LG
      I1G=(I-1)*LG
      IK=I1G+K
      IF(I-K)30,50,30
   30 ELEM=FI2(IK)
      FI2(IK)=0.0
      DO 40 J=1,LG
      IJ=I1G+J
      KJ=K1G+J
   40 FI2(IJ)=FI2(IJ)-ELEM*FI2(KJ)
   50 CONTINUE
   60 CONTINUE
      SUMD=0.D+00
      SUMN=0.D+00
      DO 2 I=1,LG
      I1=LG*(I-1)
      DO 3 M=1,LG
      AA=0.D+00
      DO 4 K=1,LG
      K1=LG*(K-1)
      IK=I1+K
      KM=K1+M
      A=AMM(IK)*FI2(KM)
      AA=AA+A
    4 CONTINUE
      IF(I.EQ.M) SUMD=SUMD+ABS(AA)
      IF(I.NE.M) SUMN=SUMN+ABS(AA)
    3 CONTINUE
    2 CONTINUE
      SUMD=SUMD/LG
C     IF(MEPRI.NE.99) PRINT 99,SUMD,SUMN
C  99 FORMAT(10X,'M ATRIX ',2D20.8)
      IF(SUMN.LE.EPS.AND.ABS(SUMD-1.D0).LT.EPS) GO TO 19
C
C     ITTERATIONS TO MAKE INVERTED MATRIX ACCURATE
C
      ITER=0
   12 ITER=ITER+1
      DO 5 I=1,LG
      I1=LG*(I-1)
      DO 5 M=1,LG
      AA=0.D+00
      DO 14 K=1,LG
      K1=LG*(K-1)
      IK=I1+K
      KM=K1+M
      A=AMM(IK)*FI2(KM)
      AA=AA+A
   14 CONTINUE
      IM=I1+M
      AMAI(IM)=AA
    5 CONTINUE
      DO 6 I=1,LG
      I1=LG*(I-1)
      DO 6 M=1,LG
      AA=0.D+00
      DO 7 K=1,LG
      K1=LG*(K-1)
      IK=I1+K
      KM=K1+M
      A=FI2(IK)*AMAI(KM)
      AA=AA+A
    7 CONTINUE
      IM=I1+M
      AIAMAI(IM)=AA
    6 CONTINUE
      DO 8 I=1,LG2
    8 FI2(I)=2.D+00*FI2(I)-AIAMAI(I)
      SUMD=0.D+00
      SUMN=0.D+00
      DO 9 I=1,LG
      I1=LG*(I-1)
      DO 9 M=1,LG
      AA=0.D+00
      DO 10 K=1,LG
      K1=LG*(K-1)
      IK=I1+K
      KM=K1+M
      A=AMM(IK)*FI2(KM)
      AA=AA+A
   10 CONTINUE
      IF(I.EQ.M) SUMD=SUMD+ABS(AA)
      IF(I.NE.M) SUMN=SUMN+ABS(AA)
    9 CONTINUE
      SUMD=SUMD/LG
      IF(SUMN.LE.EPS.AND.ABS(SUMD-1.D0).LT.EPS) GO TO 19
      IF(ITER.LE.2) GO TO 12
      IF(MEPRI.NE.99) PRINT 24,SUMN,SUMD,INFOR,ITER
      WRITE(21,24)SUMN,SUMD,INFOR,ITER
   24 FORMAT(10X,'WARNING! MATRIX IS POORLY INVERTED'/
     *5X,'SUM OF NON-DIAG. ELEM-S=',D11.5,
     *', DIAGONAL=',D11.5,',INFOR=',I2,',ITER=',I2)
   19 RETURN
      END
C     *******************************************************
      SUBROUTINE PLEGA
C     *******************************************************
      IMPLICIT DOUBLE PRECISION(A-H,O-Z) 
      COMMON/DISK/TET(150),MTET
      COMMON/DISCAN/DISC(40,150),PL(180,150),COEF(40,180),LKK
      DO 3 M=1,MTET
      TETA=TET(M)*4.d0*ATAN(1.d0)/180.d0
      PL(1,M)=1.D0
      PL(2,M)=COS(TETA)
      DO 3 K=3,180
      AK=K-1.
    3 PL(K,M)=((2.D0*AK-1.D0)*
     * COS(TETA)*PL(K-1,M)-(AK-1.D0)*PL(K-2,M))/AK
      RETURN
      END
C     *******************************************************
      SUBROUTINE DISCA
C     *******************************************************
      IMPLICIT DOUBLE PRECISION(A-H,O-Z) 
      COMMON/DISK/TET(150),MTET
      COMMON/DISCAN/DISC(40,150),PL(180,150),COEF(40,180),LKK
      COMMON/KG/J1,J2,M1,M2,J,M,AKG
     */ENA/EN,EL(40),BET(10),NUR,NMAX,NPD,LAS
     */NU/NUI,NUF
      COMMON/RACB/JA,JB,JC,JD,JE,JF,W
      COMMON/QNB/JO(40),NPO(40),KO(40),NCA(40)
      COMMON/POTB/WNK(40),WN(40),VR,WC,WD
      COMMON/CMAT/CR(200,200),CI(200,200)
     */CCMAT/CRD(180,10,200),CID(180,10,200)
      COMMON/JNN/CSS,INCC,NCLL,NSS,NJ,INCR
     */QNSB/INC(180),INR(180),JS(180)
     */QNSBD/LNJ(180,250),JNJ(180,250),NNJ(180,250)
      COMMON/QNS1/LNJ1(250),JNJ1(250),NNJ1(250),KPJ1(250)
     */NCLMA/LLMA,NCMA,NSMA,KODMA
     */COUL/CONZ,ETA,COPH(40,90)
     */COULCO/COEFR(90),COEFI(90)
      COMMON/RAD/RR,RC,RD,RW,RS,AR,AC,AW,AD,AS,ALF,AT,ANEU,RZ,ZNUC,ASP
     *,AZ
     */AP/KEYAP

      IF(IDINT(ASP+0.1D0).EQ.1) THEN
      CALL DEUTR
      RETURN
      ENDIF
     
      CALL PLEGA
      LKK=1
      ETA=CONZ/WNK(1)
      DO 2 N=1,NMAX
      DO 4 L=1,180
      IF(L.LE.90.AND.N.EQ.1) COEFR(L)=0.
      IF(L.LE.90.AND.N.EQ.1) COEFI(L)=0.
    4 COEF(N,L)=0.
      DO 2 M=1,MTET
    2 DISC(N,M)=0.
      IC=JO(1)
      A1=200.D0*WN(1)*(IC+1.D0)
      DO 1 K1=1,NJ
      JS1=JS(K1)
      N1I=INC(K1)
      N1F=INR(K1)
      DO 1 N1C=1,N1I
      L1I=LNJ(K1,N1C)
      J1I=JNJ(K1,N1C)
      CORR=DCOS(2.D0*COPH(1,L1I+1))
      COII=DSIN(2.D0*COPH(1,L1I+1))
      ACRR=CRD(K1,N1C,N1C)
      ACII=CID(K1,N1C,N1C)
      ACRN=ACRR*CORR-ACII*COII
      ACIN=ACRR*COII+ACII*CORR
      DO 8 N1R=1,N1F
      L1F=LNJ(K1,N1R)
      J1F=JNJ(K1,N1R)
      NU1=NNJ(K1,N1R)
      COR=DCOS(COPH(1,L1I+1)+COPH(NU1,L1F+1))
      COI=DSIN(COPH(1,L1I+1)+COPH(NU1,L1F+1))
      IF(KODMA.GT.0) GO TO 11
      IF(NU1.LT.NUI) GO TO 8
      IF(NU1.GT.NUF) GO TO 17
      GO TO 13
   11 IF(NU1.LT.NUI) GO TO 8
      IF(NU1.GT.NUF) GO TO 8
   13 IR=JO(NU1)
      ACR=CRD(K1,N1C,N1R)
      ACI=CID(K1,N1C,N1R)
      ACR1=ACR*COR-ACI*COI
      ACI1=ACR*COI+ACI*COR
      A=(JS1+1.D0)*DSQRT((J1I+1.D0)*(J1F+1.D0))/A1
     **(-1)**((IR-IC)/2)
      A2=A
      DO 7 K2=K1,NJ
      IF(K2.NE.K1) A2=2.D0*A
      JS2=JS(K2)
      N2I=INC(K2)
      N2F=INR(K2)
      DO 7 N2C=1,N2I
      L2I=LNJ(K2,N2C)
      J2I=JNJ(K2,N2C)
      DO 5 N2R=1,N2F
      NU2=NNJ(K2,N2R)
      IF(KODMA.GT.0) GO TO 12
      IF(NU2.LT.NU1) GO TO 5
      IF(NU2.GT.NU1) GO TO 7
      GO TO 14
   12 IF(NU2.LT.NU1) GO TO 5
      IF(NU2.GT.NU1) GO TO 5
   14 L2F=LNJ(K2,N2R)
      J2F=JNJ(K2,N2R)
      COR=DCOS(COPH(1,L2I+1)+COPH(NU1,L2F+1))
      COI=DSIN(COPH(1,L2I+1)+COPH(NU1,L2F+1))
      LN=IABS(L2F-L1F)+1
      LK=L1F+L2F+1
      LL=L1I+L2I+1
      LLL=LL+LK
      IF(LLL/2*2.NE.LLL) GO TO 5
      LLN=IABS(L1I-L2I)+1
      IF(LLN.GT.LN) LN=LLN
      IF(LL.LT.LK) LK=LL
      LIO=IABS(J1I-J2I)/2+1
      LFO=(J1I+J2I)/2+1
      LLN=IABS(J1F-J2F)/2+1
      LL=(J1F+J2F)/2+1
      IF(LLN.GT.LIO) LIO=LLN
      IF(LL.LT.LFO) LFO=LL
      LLN=IABS(JS1-JS2)/2+1
      LL=(JS1+JS2)/2+1
      IF(LLN.GT.LIO) LIO=LLN
      IF(LL.LT.LFO) LFO=LL
      IF(LIO.GT.LN) LN=LN+(LIO-LN+1)/2*2
      IF(LFO.LT.LK) LK=LK-(LK-LFO+1)/2*2
      IF(LN.GT.LK) GO TO 5
      ACR=CRD(K2,N2C,N2R)
      ACI=CID(K2,N2C,N2R)
      ACR2=ACR*COR-ACI*COI
      ACI2=ACR*COI+ACI*COR
      B=A2*(JS2+1.D0)*DSQRT((J2I+1.D0)*(J2F+1.D0))*(ACR1*ACR2+ACI1*ACI2)
     **(-1)**((J1I+J2I+J1F+J2F)/2)
      IF(LK.GT.LKK) LKK=LK
      DO 6 L=LN,LK,2
      L1=L-1
      JA=JS1
      JB=J1F
      JE=JS2
      JD=J2F
      JC=IR
      JF=L1*2
      CALL RACAH
      AA=B*W
      JB=J1I
      JD=J2I
      JC=IC
      CALL RACAH
      AA=AA*W
      J1=J1I
      J2=J2I
      M1=1
      M2=-1
      J=JF
      M=0
      CALL KLEGO
      AA=AA*AKG
      J1=J1F
      J2=J2F
      CALL KLEGO
      AA=AA*AKG
      DO 3 M=1,MTET
    3 DISC(NU1,M)=DISC(NU1,M)+PL(L,M)*AA
      COEF(NU1,L)=COEF(NU1,L)+AA
    6 CONTINUE
    5 CONTINUE
    7 CONTINUE
    8 CONTINUE
   17 IF(ETA.EQ.0.D0) GO TO 1
      IF(NUI.GT.1) GO TO 1
      COEFR(L1I+1)=COEFR(L1I+1)+ACRN/A1*(JS1+1.D0)/(2.D0*L1I+1.D0)/A1*2
      COEFI(L1I+1)=COEFI(L1I+1)+ACIN/A1*(JS1+1.D0)/(2.D0*L1I+1.D0)/A1*2
       DO 18 M=1,MTET
      TETA=TET(M)*4.D0*DATAN(1.D0)/180.D0
      SIT22=DSIN(TETA/2.D0)**2
      ALST2=DLOG(DSIN(TETA/2.D0))
      ARGC=2.*(COPH(1,1)-ETA*ALST2)
C     COULOMB AMPLITUDE * by 2K
      COULR=-ETA/SIT22*DCOS(ARGC)
      COULI=-ETA/SIT22*DSIN(ARGC)
C     Cmat*Frez*+Frex*Cmat*=2.*ACC
      AAC=(COULR*ACRN+COULI*ACIN)*(JS1+1.D0)
   18 DISC(1,M)=DISC(1,M)+PL(L1I+1,M)/A1*AAC
    1 CONTINUE
      IF(ETA.EQ.0.D0) GO TO 16
      IF(NUI.GT.1) GO TO 16
      DO 15 M=1,MTET
      TETA=TET(M)*4.D0*DATAN(1.D0)/180.D0
   15 DISC(1,M)=DISC(1,M)+ETA**2/A1/2.D0/DSIN(TETA/2.D0)**4*(IC+1.D0)
   16 DO 10 N=NUI,NUF
      COE=COEF(N,1)
      DO 10 L=1,LKK
      COE=1.
   10 COEF(N,L)=COEF(N,L)/COE/(2*L-1)
      RETURN
      END
C     ***********************************************************
      SUBROUTINE ECISS
C     ***********************************************************
      IMPLICIT DOUBLE PRECISION(A-H,O-Z) 
C     BELOW CARD IS NECESSARY IF MEMORY IS LESS THAN 32Mb
      DOUBLE PRECISION WPSL,PL,PSL,PR,PI,PVC,PRC
      COMMON/RAD/RR,RC,RD,RW,RS,AR,AC,AW,AD,AS,ALF,AT,ANEU,RZ,ZNUC,ASP
     *,AZ
      COMMON/QNS1/LNJ1(250),JNJ1(250),NNJ1(250),KPJ1(250)
      COMMON/LNP/JSS,NPIS,NPIO,JSO,NN1,LNO(180),NS1(180),JNO(180),NSPI
      COMMON/QNB/JO(40),NPO(40),KO(40),NCA(40)
     */ENA/EN,EL(40),BET(10),NUR,NMAX,NPD,LAS
     */SHEM1/HW,AMG0,AMB0,GAM0,BET0,BET4,GAMDE,GSHAPE
      COMMON/MENU/MEJOB,MEPOT,MEHAM,MECHA,MEPRI,MESOL,MESHA,MESHO,MEHAO
     *,MEAPP,MEVOL,MEREL,MECUL,MERZZ,MERRR,MEDIS,MERIP
      COMMON/JNN/CSS,INCC,NCLL,NSS,NJ,INCR
     */STR/STEP,RK,NH1
     */ECI/NECI,NHI
      COMMON/POTB/WNK(40),WN(40),VR,WC,WD
     */ABEC/PL(300),PSL(480000),PR(2400000),PI(2400000),WPSL(480000)
      COMMON/POTPN/PVV(120000),PWW(120000),PRR(120000),PII(120000)
     */ABCOUL/PVC(5400),PRC(5400),CVNC(720000)
     */CV/CVNR(160000)
      COMMON/CVPN/CVNRPN(40000)
     */FBN/FBR1(40,90),FBI1(40,90),FNR2(40,90),FNI2(40,90),FNR1(40,90),
     *FNI1(40,90),FBR2(40,90),FBI2(40,90)
      COMMON/CMAT/CR(200,200),CI(200,200)
     */CCMAT/CRD(180,10,200),CID(180,10,200)
      COMMON/MATT/ABR1,ABI1,ANR1,ANI1,ABR2,ABI2,ANR2,ANI2,
     *CFR1,CFI1,CFR2,CFI2,CRC,CIC,ARA,AIA
      COMMON/FUNC/FRH(300,200),FIH(300,200),FRI(300,200),FII(300,200),
     *FRS(300,11),FIS(300,11)
      COMMON/CRIC/CRH(200),CIH(200),CRI(200),CII(200),CRS(200),CIS(200)
      COMMON/CRIT/CRP(200),CIP(200),CRT(40,200),CIT(40,200),NPAD(200)
      COMMON/PAD/CT(40),APA(20),BPA(20),CY(20),MPA,LPA
      COMMON/FUEC/FREM(300,40000),FIEM(300,40000)
      COMMON/AUK/AUR(400),AUI(400)
      COMMON/FIRA/FRA(300,200),FIA(300,200)
     */CVOL/CBET0
C     NEXT TWO CARDS JUST TO OVERCOME THE ERROR OR  MICROSOFT FPS COMPILER
C     v.1.00, YOU CAN DELETE THEM USING OTHER COMPILERS
      IF(MEPRI.NE.99) PRINT 999,(WNK(II),II=1,NUR)
  999 FORMAT (5E12.5)
      SPI2=ASP*(ASP+1.D0)
      CONG=(JSS+1)/(JO(1)+1.D0)
      CON=12.566372*CONG/WN(1)/100.D0/(NSPI+1)
      IF(MEHAM.EQ.1)   AKW=1.+BET(2)*2.5
      IF(MEHAM.NE.1)   AKW=1.+BET0*2.5
      NSLU=1
      IF(MEJOB.GT.3) NSLU=2
      LLNC=INCC
      NECI=1
   24 IF(NSLU.EQ.2) AKW=1.
      IF(NSLU.EQ.2) LLNC=NCLL
      IF(MEPOT.EQ.1) GO TO 30
      LAS2=LAS+1
      LAS1=LAS
      GO TO 36
   30 LAS2=(LAS+2)/2
      LAS1=LAS2-1
   36 NCLA=NCLL*LAS1
      LAS8=9
      LASC=1
      IF(LAS2.GE.3) LAS8=18
      IF(LAS2.GE.3) LASC=2
      ICLL=NCLL*LAS8
      MNULA=300*NUR*LAS2
      MLA=300*LAS2
      MNU=300*NUR
      ST=1.
      ST2=STEP*STEP
      ST12=ST2/12.
      ST6=ST2/6.
      DO 1 L=1,LLNC
      LLL=(L-1)*(NCLA+LAS1)-1
      LC2=(L-1)*(ICLL+LAS8)
      LL=LNJ1(L)
       ST=1./(18.D+00)**LL
      JJ=JNJ1(L)
      NU=NNJ1(L)
      MNULLL=MNULA*(NU-1)+MLA*(NU-1)
      MNULL=MNU*(NU-1)+300*(NU-1)
C       IF((EN-EL(NU)/AT*(AT+ANEU)).LT.0) ST=EXP(-5.*LL)
      CJ1=JJ/2.
      SOP=(CJ1-LL)*(CJ1+LL+1)-SPI2
      FRH(1,L)=0.
      FIH(1,L)=0.
      FRH(2,L)=ST*STEP**(LL+1)
      FIH(2,L)=0.
      QSR1=0.
      QSI1=0.
      IF(LL.EQ.1) QSR1=-ST*ST6
      POTR=PSL(MNULL+2)*SOP+PL(2)*LL*(LL+1)-WN(NU)+PR(MNULLL+LAS2+1)
      POTI=PI(MNULLL+LAS2+1)*AKW+WPSL(MNULL+2)*SOP
      LI=LAS2+2+MNULLL
      LF=2*LAS2+MNULLL
          ACS=0.
      IF(LI.GT.LF) GO TO 32
      DO 2 LA=LI,LF
      A=CVNR(LLL+LA-LAS2-MNULLL)
      POTR=POTR+PR(LA)*A
      POTI=POTI+PI(LA)*A*AKW
      IF(MEPOT.EQ.1) GO TO 2
      LLLL=LA-LI+1
      IF(LLLL.GT.2) GO TO 2
      LAI=3
      LAF=5
      IF(LLLL.EQ.2) LAI=1
      IF(LLLL.EQ.2) LAF=9
          IF(LLLL.EQ.1) PVS=PR(LA)
          IF(LLLL.EQ.1) PWS=PI(LA)*AKW
      DO 62 LAC=LAI,LAF
      LALAS=LASC*(LAC-1)+LLLL
      LP1=LALAS+LAS8
      PV1=PRC(LP1)
      LL2=LC2+LALAS
      A=CVNC(LL2)
          IF(LLLL.EQ.2.AND.LAC.EQ.1) ACS=-A*CBET0
          IF(LAC.EQ.1) GO TO 62
      POTR=POTR+A*PV1
62    CONTINUE
    2 CONTINUE
      IF(MEVOL.EQ.0) GO TO 32
          POTR=POTR+ACS*PVS
          POTI=POTI+ACS*PWS
  32  QSR2=FRH(2,L)*(1.-ST12*POTR)
      QSI2=-FRH(2,L)*ST12*POTI
      POI=POTI+ST6*POTR*POTI
      POR=POTR+ST12*(POTR*POTR-POTI*POTI)
      UR=(QSR2*POR-QSI2*POI)*ST2
      UI=(QSR2*POI+QSI2*POR)*ST2
      DO 3 NS=3,NHI
      QSR3=2.*QSR2-QSR1+UR
      QSI3=2.*QSI2-QSI1+UI
      LIC=(NS-1)*LAS8
      LI=(NS-1)*LAS2+1+MNULLL
      POTR=PSL(MNULL+NS)*SOP+PL(NS)*LL*(LL+1)-WN(NU)+PR(LI)
      POTI=PI(LI)*AKW+WPSL(MNULL+NS)*SOP
      LI=LI+1
      LF=NS*LAS2+MNULLL
      LP=LLL-LAS2*(NS-1)
          ACS=0.
      IF(LI.GT.LF) GO TO 34
      DO 4 LA=LI,LF
      A=CVNR(LP+LA-MNULLL)
      POTR=POTR+PR(LA)*A
      POTI=POTI+PI(LA)*A*AKW
      IF(MEPOT.EQ.1) GO TO 4
      LLLL=LA-LI+1
      IF(LLLL.GT.2) GO TO 4
      LAI=3
      LAF=5
      IF(LLLL.EQ.2) LAI=1
      IF(LLLL.EQ.2) LAF=9
          IF(LLLL.EQ.1) PVS=PR(LA)
          IF(LLLL.EQ.1) PWS=PI(LA)*AKW
      DO 65 LAC=LAI,LAF
      LALAS=LASC*(LAC-1)+LLLL
      LP1=LALAS+LIC
      PV1=PRC(LP1)
      LL2=LC2+LALAS
      A=CVNC(LL2)
          IF(LLLL.EQ.2.AND.LAC.EQ.1) ACS=-A*CBET0
          IF(LAC.EQ.1) GO TO 65
      POTR=POTR+A*PV1
65    CONTINUE
    4 CONTINUE
      IF(MEVOL.EQ.0) GO TO 34
          POTR=POTR+ACS*PVS
          POTI=POTI+ACS*PWS
   34 POR=POTR+ST12*(POTR*POTR-POTI*POTI)
      POI=POTI+ST6*POTR*POTI
      UR=(QSR3*POR-QSI3*POI)*ST2
      UI=(QSR3*POI+QSI3*POR)*ST2
      FRH(NS,L)=QSR3+UR/12.
      FIH(NS,L)=QSI3+UI/12.
C     ANS=(NS-1.)*STEP
C     IF(MEPRI.NE.99) PRINT 111,FRH(NS,L),FIH(NS,L),ANS
      QSR1=QSR2
      QSI1=QSI2
      QSR2=QSR3
      QSI2=QSI3
    3 CONTINUE
      L1=LL+1
      ABR1=FBR1(NU,L1)
      ABI1=FBI1(NU,L1)
      ANR1=FNR1(NU,L1)
      ANI1=FNI1(NU,L1)
      ABR2=FBR2(NU,L1)
      ABI2=FBI2(NU,L1)
      ANR2=FNR2(NU,L1)
      ANI2=FNI2(NU,L1)
      CFR1=FRH(NHI-2,L)
      CFI1=FIH(NHI-2,L)
      CFR2=FRH(NHI,L)
      CFI2=FIH(NHI,L)
C     IF(MEPRI.NE.99) PRINT 222,L,LL,NCLL,NU
C 222 FORMAT (2X,20I5)
C     IF(MEPRI.NE.99) PRINT 111,CFR1,CFI1,CFR2,CFI2
  111 FORMAT (2X,2E13.5,4I10)
C     IF(MEPRI.NE.99) PRINT 111,ABR1,ABI1,ANR1,ANI1
C     IF(MEPRI.NE.99) PRINT 111,ABR2,ABI2,ANR2,ANI2
      CALL MATCH
      ZN=ARA*ARA+AIA*AIA
C        PRINT 1111,ARA,AIA,CRC,CIC,LL,L
C1111 FORMAT(4E10.2,4I3)
      CRH(L)=(ARA*CRC+AIA*CIC)/ZN
      CIH(L)=(ARA*CIC-AIA*CRC)/ZN
      IF(NSLU.EQ.2) GO TO 23
      CRS(L)=CRH(L)
      CIS(L)=CIH(L)
      DO 5 NS=1,NHI
      CIC=FIH(NS,L)
      CRC=FRH(NS,L)
      FRS(NS,L)=(ARA*CRC+AIA*CIC)/ZN
      FIS(NS,L)=(ARA*CIC-AIA*CRC)/ZN
    5 CONTINUE
      GO TO 1
   23 DO 25 NS=1,NHI
      CIC=FIH(NS,L)
      CRC=FRH(NS,L)
      FRH(NS,L)=(ARA*CRC+AIA*CIC)/ZN
      FIH(NS,L)=(ARA*CIC-AIA*CRC)/ZN
   25 CONTINUE
C 444    FORMAT(2X,3E15.7,9I5)
    1 CONTINUE
      NSLU=NSLU+1
      IF(NSLU.EQ.2) GO TO 24
      CSS=0.
C     IF(MEPRI.NE.99) PRINT 333
C333  FORMAT('    SECOND PART')
      MALL=NCLL
      IF(MESOL.LE.NCLL) MALL=MESOL
      DO 6 IC=1,INCC
      IC1=(IC-1)*MALL
      DO 9 L=1,NCLL
      CRI(L)=0.
      CII(L)=0.
      CRP(L)=0.
      CIP(L)=0.
      CRT(1,L)=0.
      CIT(1,L)=0.
      NPAD(L)=1
      DO 9 NS=1,NHI
      FRI(NS,L)=0.
    9 FII(NS,L)=0.
      IF(MESOL.GT.3) GO TO 47
      CRI(IC)=CRS(IC)
      CII(IC)=CIS(IC)
      CRP(IC)=CRS(IC)
      CIP(IC)=CIS(IC)
      CRT(1,IC)=CRS(IC)
      CIT(1,IC)=CIS(IC)
      DO 10 NS=1,NHI
      FRI(NS,IC)=FRS(NS,IC)
   10 FII(NS,IC)=FIS(NS,IC)
 1199 FORMAT (2E15.5)
      GO TO 48
   47 DO 49 MLL=1,MALL
      CRI(MLL)=CR(IC,MLL)
      CII(MLL)=CI(IC,MLL)
      CRP(MLL)=CR(IC,MLL)
      CIP(MLL)=CI(IC,MLL)
      CRT(1,MLL)=CR(IC,MLL)
      CIT(1,MLL)=CI(IC,MLL)
      DO 50 NS=1,NHI
      FRI(NS,MLL)=0.
      FII(NS,MLL)=0.
      DO 50 JLL=1,MALL
      JM=(JLL-1)*MALL+MLL
      MJ=(MLL-1)*MALL+JLL
      IJ=IC1+JLL
      FRI(NS,MLL)=FRI(NS,MLL)-AUR(IJ)*FREM(NS,JM)+AUI(IJ)*FIEM(NS,JM)
   50 FII(NS,MLL)=FII(NS,MLL)-AUR(IJ)*FIEM(NS,JM)-AUI(IJ)*FREM(NS,JM)
   49 CONTINUE
   48 ITER=1
      BEN=1
   21 CSI=0.
      ITER=ITER+1
      IF(ITER.GT.25) GO TO 26
      IF(MEHAM.NE.1) BEN=BEN*BET0
      IF(MEHAM.EQ.1) BEN=BEN*BET(2)
      DO 7 L=1,NCLL
      LLL=(L-1)*(NCLA+LAS1)-1
      
      LISO=(L-1)*NCLL

      LC2=(L-1)*(ICLL+LAS8)
      LL=LNJ1(L)
      ST=1./(18.D+00)**LL
      JJ=JNJ1(L)
      NU=NNJ1(L)
      MNULLL=MNULA*(NU-1)+MLA*(NU-1)
      MNULL=MNU*(NU-1)+300*(NU-1)
      MNLL=MNULA*(NU-1)
      MNL=MNU*(NU-1)
C       IF((EN-EL(NU)/AT*(AT+ANEU)).LT.0) ST=EXP(-5.*LL)
      NPI1=KPJ1(L)
C     NEXT TWO CARDS JUST TO OVERCOME THE ERROR OR  MICROSOFT FPS COMPILER
C     v.1.00, YOU CAN DELETE THEM USING OTHER COMPILERS
             IF(WNK(NU)/WNK(1).LE.0.) PRINT 99,WNK(NU),WNK(1),NU
   99 FORMAT (2E12.3,I3)
      C=SQRT(WNK(NU)/WNK(1))
      CJ1=JJ/2.
      SOP=(CJ1-LL)*(CJ1+LL+1)-SPI2
      FRI(1,L)=0.
      FII(1,L)=0.
      FRI(2,L)=ST*STEP**(LL+1)
      FII(2,L)=0.
      QSR1=0.
      QSI1=0.
      IF(LL.EQ.1) QSR1=-ST*ST6
      POTR=PSL(MNULL+2)*SOP+PL(2)*LL*(LL+1)-WN(NU)+PR(MNULLL+LAS2+1)
      POTI=PI(MNULLL+LAS2+1)+WPSL(MNULL+2)*SOP

     
      LI=LAS2+2+MNULLL
      LF=2*LAS2+MNULLL
          ACS=0.
      IF(LI.GT.LF) GO TO 38
      DO 8 LA=LI,LF
      A=CVNR(LLL+LA-LAS2-MNULLL)
      POTR=POTR+PR(LA)*A
      POTI=POTI+PI(LA)*A
      IF(MEPOT.EQ.1) GO TO 8
      LLLL=LA-LI+1
      IF(LLLL.GT.2) GO TO 8
      LAI=3
      LAF=5
      IF(LLLL.EQ.2) LAI=1
      IF(LLLL.EQ.2) LAF=9
          IF(LLLL.EQ.1) PVS=PR(LA)
          IF(LLLL.EQ.1) PWS=PI(LA)
      DO 68 LAC=LAI,LAF
      LALAS=LASC*(LAC-1)+LLLL
      LP1=LALAS+LAS8
      PV1=PRC(LP1)
      LL2=LC2+LALAS
      A=CVNC(LL2)
          IF(LLLL.EQ.2.AND.LAC.EQ.1) ACS=-A*CBET0
          IF(LAC.EQ.1) GO TO 68
      POTR=POTR+A*PV1
   68 CONTINUE
    8 CONTINUE
      IF(MEVOL.EQ.0) GO TO 38
          POTR=POTR+ACS*PVS
          POTI=POTI+ACS*PWS
   38 QSR2=FRI(2,L)*(1.-ST12*POTR)
      QSI2=-FRI(2,L)*ST12*POTI
      POR=POTR+ST12*(POTR*POTR-POTI*POTI)
      POI=POTI+ST6*POTR*POTI
      UR=(QSR2*POR-QSI2*POI)*ST2
      UI=(QSR2*POI+QSI2*POR)*ST2
      WR1=0.
      WI1=0.
      WR2=0.
      WI2=0.
      K1=(L-1)*NCLA-1
      KC1=ICLL*(L-1)
      DO 11 IR=1,NCLL
C        IF(L.LE.MALL.AND.IR.GT.MALL) GO TO 11
C        IF(L.GT.MALL.AND.IR.GT.1) GO TO 11
C        IF(L.GT.MALL) GO TO 11
      NPI2=KPJ1(IR)
      NUIR=NNJ1(IR)
      LISOR=LISO+IR
      MNLLIR=MNLL+MLA*(NUIR-1)
      MNLIR=MNL+300*(NUIR-1)
      K2=K1+(IR-1)*LAS1
      KC2=KC1+LAS8*(IR-1)
      LI=LAS2+2+MNLLIR
      IF(IR.EQ.L) GO TO 11
C      LI=LAS2+2+MNLLIR
      LF=2*LAS2+MNLLIR
      POTR=0.
      POTI=0.
          ACS=0.
      IF(LI.GT.LF) GO TO 42
      DO 12 LA=LI,LF
      A=CVNR(K2+LA-LAS2-MNLLIR)
      IF(NPI1.NE.NPI2) GO TO 39
      POTR=POTR+PR(LA)*A
      POTI=POTI+PI(LA)*A
      GO TO 70
   39 POTR=POTR-PI(LA)*A
      POTI=POTI+PR(LA)*A
   70 IF(MEPOT.EQ.1) GO TO 12
      LLLL=LA-LI+1
      IF(LLLL.GT.2) GO TO 12
      LAI=3
      LAF=5
      IF(LLLL.EQ.2) LAI=1
      IF(LLLL.EQ.2) LAF=9
          IF(LLLL.EQ.1) PVS=PR(LA)
          IF(LLLL.EQ.1) PWS=PI(LA)
      DO 71 LAC=LAI,LAF
      LALAS=LASC*(LAC-1)+LLLL
      LP1=LALAS+LAS8
      PV1=PRC(LP1)
      LL2=KC2+LALAS
      A=CVNC(LL2)
          IF(LLLL.EQ.2.AND.LAC.EQ.1) ACS=-A*CBET0
          IF(LAC.EQ.1) GO TO 71
      IF(NPI1.NE.NPI2) GO TO 72
      POTR=POTR+A*PV1
      GO TO 71
   72 POTI=POTI+A*PV1
   71 CONTINUE
   12 CONTINUE

      
      
      COUPL=CVNRPN(LISOR)
C      IF(NU.NE.NUIR.AND.JO(NU).EQ. JO(NUIR).AND.JO(NU).EQ.0) COUPL=1.D0
      IF(NCA(NU).NE.NCA(NUIR).AND.JO(NU).EQ. JO(NUIR))
     *POTR=POTR+PR(LI-1)*COUPL
      IF(NCA(NU).NE.NCA(NUIR).AND.JO(NU).EQ. JO(NUIR))
     *POTI=POTI+PI(LI-1)*COUPL
 
      
      
      
 


      IF(MEVOL.EQ.0) GO TO 42
          POTR=POTR+ACS*PVS
          POTI=POTI+ACS*PWS
   42 FR=FRI(2,IR)
      FI=FII(2,IR)
      WR2=WR2+POTR*FR-POTI*FI
      WI2=WI2+POTR*FI+POTI*FR
   11 CONTINUE
      DO 13 NS=3,NHI
      WR3=0.
      WI3=0.
      DO 14 IR=1,NCLL
C        IF(L.LE.MALL.AND.IR.GT.MALL) GO TO 14
C        IF(L.GT.MALL.AND.IR.GT.1) GO TO 14
C        IF(L.GT.MALL) GO TO 14
      LISOR=LISO+IR
      NPI2=KPJ1(IR)
      NUIR=NNJ1(IR)
      MNLLIR=MNLL+MLA*(NUIR-1)
      MNLIR=MNL+300*(NUIR-1)
      K2=K1+(IR-1)*LAS1
      KC2=KC1+LAS8*(IR-1)
C      IF(IR.EQ.L) GO TO 14
      LI=(NS-1)*LAS2+2+MNLLIR
      IF(IR.EQ.L) GO TO 14
      LIC=(NS-1)*LAS8
      LC2=(L-1)*(ICLL+LAS8)
      POTR=0.
      POTI=0.
      LF=NS*LAS2+MNLLIR
      LP=K2-(NS-1)*LAS2
          ACS=0.
      IF(LI.GT.LF) GO TO 45
      DO 15 LA=LI,LF
      A=CVNR(LP+LA-MNLLIR)
      IF(NPI1.NE.NPI2) GO TO 40
      POTR=POTR+PR(LA)*A
      POTI=POTI+PI(LA)*A
      GO TO 73
   40 POTR=POTR-PI(LA)*A
      POTI=POTI+PR(LA)*A
   73 IF(MEPOT.EQ.1) GO TO 15
      LLLL=LA-LI+1
      IF(LLLL.GT.2) GO TO 15
      LAI=3
      LAF=5
      IF(LLLL.EQ.2) LAI=1
      IF(LLLL.EQ.2) LAF=9
          IF(LLLL.EQ.1) PVS=PR(LA)
          IF(LLLL.EQ.1) PWS=PI(LA)
      DO 79 LAC=LAI,LAF
      LALAS=LASC*(LAC-1)+LLLL
      LP1=LALAS+LIC
      PV1=PRC(LP1)
      LL2=KC2+LALAS
      A=CVNC(LL2)
          IF(LLLL.EQ.2.AND.LAC.EQ.1) ACS=-A*CBET0
          IF(LAC.EQ.1) GO TO 79
      IF(NPI1.NE.NPI2) GO TO 82
      POTR=POTR+A*PV1
      GO TO 79
   82 POTI=POTI+A*PV1
   79 CONTINUE
   15 CONTINUE

      
            
      COUPL=CVNRPN(LISOR)
C      IF(NU.NE.NUIR.AND.JO(NU).EQ. JO(NUIR).AND.JO(NU).EQ.0) COUPL=1.D0
      IF(NCA(NU).NE.NCA(NUIR).AND.JO(NU).EQ. JO(NUIR))
     *POTR=POTR+PR(LI-1)*COUPL
      IF(NCA(NU).NE.NCA(NUIR).AND.JO(NU).EQ. JO(NUIR))
     *POTI=POTI+PI(LI-1)*COUPL
 
C        IF(NU.NE.NUIR.AND.JO(NU).EQ. JO(NUIR))
C     *print 567, lisor,nu,nuir,jo(nu),jo(nuir),li,PR(LI-1),Pi(LI-1),
C     *     CVNRPN(LISOR)
C  567 Format (6i4,5e12.3)
C      IF(NU.NE.NUIR.AND.JO(NU).EQ. JO(NUIR))pause

      IF(MEVOL.EQ.0) GO TO 45
          POTR=POTR+ACS*PVS
          POTI=POTI+ACS*PWS
   45 FR=FRI(NS,IR)
      FI=FII(NS,IR)
      WR3=WR3+POTR*FR-POTI*FI
      WI3=WI3+POTR*FI+POTI*FR
   14 CONTINUE
      QSR3=2.*QSR2-QSR1+UR+(WR3+10.*WR2+WR1)*ST12
      QSI3=2.*QSI2-QSI1+UI+(WI3+10.*WI2+WI1)*ST12
      LI=(NS-1)*LAS2+1+MNULLL
      POTR=PSL(MNULL+NS)*SOP+PL(NS)*LL*(LL+1)-WN(NU)+PR(LI)
      POTI=PI(LI)+WPSL(MNULL+NS)*SOP



      LI=LI+1
      LF=NS*LAS2+MNULLL
      LP=LLL-LAS2*(NS-1)
          ACS=0.
      IF(LI.GT.LF) GO TO 46
      DO 16 LA=LI,LF
      A=CVNR(LP+LA-MNULLL)
      POTR=POTR+PR(LA)*A
      POTI=POTI+PI(LA)*A
      IF(MEPOT.EQ.1) GO TO 16
      LLLL=LA-LI+1
      IF(LLLL.GT.2) GO TO 16
      LAI=3
      LAF=5
      IF(LLLL.EQ.2) LAI=1
      IF(LLLL.EQ.2) LAF=9
          IF(LLLL.EQ.1) PVS=PR(LA)
          IF(LLLL.EQ.1) PWS=PI(LA)
      DO 74 LAC=LAI,LAF
      LALAS=LASC*(LAC-1)+LLLL
      LP1=LALAS+LIC
      PV1=PRC(LP1)
      LL2=LC2+LALAS
      A=CVNC(LL2)
          IF(LLLL.EQ.2.AND.LAC.EQ.1) ACS=-A*CBET0
          IF(LAC.EQ.1) GO TO 74
      POTR=POTR+A*PV1
   74 CONTINUE
   16 CONTINUE
      IF(MEVOL.EQ.0) GO TO 46
          POTR=POTR+ACS*PVS
          POTI=POTI+ACS*PWS
   46 POR=POTR+ST12*(POTR*POTR-POTI*POTI)
      POI=POTI+ST6*POTR*POTI
      UR=(QSR3*POR-QSI3*POI)*ST2
      UI=(QSR3*POI+QSI3*POR)*ST2
      FRI(NS,L)=QSR3+UR/12.
      FII(NS,L)=QSI3+UI/12.
      WR1=WR2
      WI1=WI2
      WR2=WR3
      WI2=WI3
      QSR1=QSR2
      QSI1=QSI2
      QSR2=QSR3
      QSI2=QSI3
   13 CONTINUE
      L1=LL+1
      ABR1=FBR1(NU,L1)
      ABI1=FBI1(NU,L1)
      ANR1=FNR1(NU,L1)
      ANI1=FNI1(NU,L1)
      ABR2=FBR2(NU,L1)
      ABI2=FBI2(NU,L1)
      ANR2=FNR2(NU,L1)
      ANI2=FNI2(NU,L1)
      CFR1=FRI(NHI-2,L)
      CFI1=FII(NHI-2,L)
      CFR2=FRI(NHI,L)
      CFI2=FII(NHI,L)
C
C     IF(MEPRI.NE.99) THEN
C       PRINT 222,L,LL,NCLL,NU
C       PRINT 111,CFR1,CFI1,CFR2,CFI2
C       PRINT 111,ABR1,ABI1,ANR1,ANI1
C       PRINT 111,ABR2,ABI2,ANR2,ANI2
C     ENDIF  
C
      CALL MATCH
      IF(IC.EQ.L) ARA=ARA-1.
      CHR=CRH(L)
      CHI=CIH(L)
C     IF(MEPRI.NE.99) THEN
C       PRINT 1111,ARA,AIA,CRC,CIC,LL,L,ITER
C       PRINT 1111,ARA,AIA,CRC,CIC,CHR,CHI,L
C     ENDIF  
C1111 FORMAT(6E10.2,3I2)
      CRC=(CRC-ARA*CHR+AIA*CHI)*C
      CIC=(CIC-ARA*CHI-AIA*CHR)*C
      CRT(ITER,L)=(CRC-CRI(L))/BEN
      CIT(ITER,L)=(CIC-CII(L))/BEN
      IF(CRT(ITER,L).NE.0..AND.CIT(ITER,L).NE.0.) GO TO 28
C     IF(MEPRI.NE.99) PRINT 111,CRT(ITER,L),CIT(ITER,L),L,NSS,ITER,NCLL
      IF(ITER.GT.3) GO TO 31
      CR(IC,L)=CRC
      CI(IC,L)=CIC
  31  NPAD(L)=0
 28   CRI(L)=CRC
      CII(L)=CIC
      DO 17 NS=1,NHI
      CRC=FRI(NS,L)
      CIC=FII(NS,L)
      FR=FRH(NS,L)
      FI=FIH(NS,L)
      FRI(NS,L)=CRC-ARA*FR+AIA*FI
      FII(NS,L)=CIC-ARA*FI-AIA*FR
   17 CONTINUE
    7 CONTINUE
      IF(ITER.GE.3.AND.ITER/2*2.NE.ITER) GO TO 18
      GO TO 21
   18 SUMC=0.
      SUMI=0.
      SUMT=0.
      LPA=ITER/2
      LPA1=LPA+1
      MPA=LPA
C     IF(MEPRI.NE.99) PRINT 1999,ITER,MPA,LPA
C1999 FORMAT(3I6)
      DO 20 L=1,INCR
C         GO TO 27
      IF(NPAD(L).EQ.0) GO TO 27
      DO 19 IT=1,ITER
   19 CT(IT)=CRT(IT,L)
C     IF(MEPRI.NE.99) PRINT 1999,ITER,MPA,LPA
      CALL PADE
      AL=0.
      AM=0.
      DO 22 LM=1,LPA1
      IF(MEHAM.NE.1) BTN=BET0**(LM-1)
      IF(MEHAM.EQ.1) BTN=BET(2)**(LM-1)
      AL=AL+APA(LM)*BTN
   22 AM=AM+BPA(LM)*BTN
      CR(IC,L)=AL/AM
      DO 29 IT=1,ITER
   29 CT(IT)=CIT(IT,L)
      CALL PADE
      AL=0.
      AM=0.
      DO 33 LM=1,LPA1
      IF(MEHAM.NE.1) BTN=BET0**(LM-1)
      IF(MEHAM.EQ.1) BTN=BET(2)**(LM-1)
      AL=AL+APA(LM)*BTN
   33 AM=AM+BPA(LM)*BTN
      CI(IC,L)=AL/AM
   27 CRC=CR(IC,L)
      CIC=CI(IC,L)
      CSI=CSI+CRC*CRC+CIC*CIC
      SUMC=SUMC+ABS(CRP(L)-CRC)+ABS(CIP(L)-CIC)
      IF(IC.EQ.L) SUMI=SUMI+ABS(CIP(L)-CIC)
      IF(IC.EQ.L) SUMT=SUMT+CIC*CON
      IF(ABS(CRC).GT.1.D+3) GO TO 26
      IF(ABS(CIC).GT.1.D+3) GO TO 26
      CRD(NSS,IC,L)=CR(IC,L)
      CID(NSS,IC,L)=CI(IC,L)
      CRP(L)=CRC
   20 CIP(L)=CIC
      CSUR=CSI*CON
      ACON=CON
      IF(MEPRI.NE.99) PRINT 456,SUMC,SUMI,ACON,CSI,CSUR,SUMT
      IF(ITER.EQ.3) GO TO 21

      EPECI=0.0005/CON
      IF(SUMI.GT.EPECI) GO TO 21
C     RCN: CHECK
C     A division by zero occurs if optimization allowed in MSF
C
      EPECI=0.0005D0/CON/2/SQRT(CSI)
      IF(SUMC.GT.EPECI) GO TO 21
C
C     HIGH ANGULAR MOMENTUM CASE : SCATTERING EQUAL TOTAL, TO
C     AVOID NEGATIVE TRANSMISSIONS AS ABSORPTION IS VERY SMALL.
C
      IF(SUMT.GE.0.AND.CSI.LE.CI(IC,IC)) GO TO 77
      CSID=CSI-CI(IC,IC)*CI(IC,IC)
      CI(IC,IC)=0.5D0*(1.-SQRT(1.D0-4.*CSID))
      CID(NSS,IC,IC)=CI(IC,IC)
      CSI=CI(IC,IC)
C
   77 CSS=CSS+CSI
  456       FORMAT(2X,4E8.2,2E14.7)
    6 CONTINUE
      IF(MEPRI.NE.99) PRINT 123,NSS,NCLL,ITER
  123       FORMAT(3I5,E20.7)
      RETURN
   26 NECI=0
      RETURN
      END
C     ***********************************************************
      SUBROUTINE MATCH
C     ***********************************************************
      IMPLICIT DOUBLE PRECISION(A-H,O-Z) 
      COMMON/MATT/ABR1,ABI1,ANR1,ANI1,ABR2,ABI2,ANR2,ANI2,
     *CFR1,CFI1,CFR2,CFI2,CRC,CIC,ARA,AIA
      ZNR=ABR2*ANR1-ABI2*ANI1-ABR1*ANR2+ABI1*ANI2
      ZNI=ABI2*ANR1+ABR2*ANI1-ABR1*ANI2-ABI1*ANR2
      IF(ZNI.NE.0.) GO TO 1
      CRC=(CFR1*ABR2-CFR2*ABR1)/ZNR
      CIC=(CFI1*ABR2-CFI2*ABR1)/ZNR
      ARA=(CFR2*ANR1-CFI2*ABR1-CFR1*ANR2+CFI1*ABR2)/ZNR
      AIA=(CFI2*ANR1+CFR2*ABR1-CFR1*ABR2-CFI1*ANR2)/ZNR
      GO TO 2
    1 CRC=(CFR1*ABI2+CFI1*ABR2-CFR2*ABI1-CFI2*ABR1)/ZNI
      CIC=(CFI1*ABI2+CFR2*ABR1-CFR1*ABR2-CFI2*ABI1)/ZNI
      FWR1=ANR1-ABI1
      FWI1=ANI1+ABR1
      FWR2=ANR2-ABI2
      FWI2=ANI2+ABR2
      ARA=(CFR2*FWI1+CFI2*FWR1-CFR1*FWI2-CFI1*FWR2)/ZNI
      AIA=(CFI2*FWI1-CFR2*FWR1-CFI1*FWI2+CFR1*FWR2)/ZNI
    2 RETURN
      END
C     ***********************************************************
      SUBROUTINE PADE
C     ***********************************************************
      IMPLICIT DOUBLE PRECISION(A-H,O-Z) 
      COMMON/PAD/CT(40),APA(20),BPA(20),CY(20),MPA,LPA
      COMMON/MATI/AMA(400),MAM
      KKK=9
C     PRINT 100,CT
C     PRINT 200,MPA,LPA,KKK
C 200 FORMAT(5I10)
      IF(MPA.LE.0) GO TO 9
      DO 1 I=1,MPA
      I1=(I-1)*MPA
      DO 1 J=1,MPA
      IJ=I1+J
      LMPA=LPA-MPA+I+J
      IF(LMPA.LT.1) GO TO 2
      AMA(IJ)=CT(LMPA)
      GO TO 1
    2 AMA(IJ)=0.
    1 CONTINUE
      MAM=MPA
      CALL MATIN
      DO 3 I=1,MPA
    3 CY(I)=CT(LPA+I+1)
      DO 4 K=1,MPA
      K1=(K-1)*MPA
      MK=MPA+2-K
      BP=0.
      DO 5 I=1,MPA
      KI=K1+I
    5 BP=BP-AMA(KI)*CY(I)
    4 BPA(MK)=BP
    9 BPA(1)=1.
      LPA1=LPA+1
      MPA1=MPA+1
      DO 6 L=1,LPA1
      AP=CT(L)
      LMA=L
      IF(L.GT.MPA1) LMA=MPA1
      IF(LMA.LT.2) GO TO 8
      DO 7 I=2,LMA
    7 AP=AP+BPA(I)*CT(L-I+1)
    8 APA(L)=AP
    6 CONTINUE
C     PRINT 100,APA,BPA
C 100 FORMAT (2X,6E20.7)
      RETURN
      END
C     ***********************************************************
      SUBROUTINE MATIN
C     ***********************************************************
      IMPLICIT DOUBLE PRECISION(A-H,O-Z) 
      COMMON/MATI/AMA(400),MAM
      DO 60 K=1,MAM
      K1=(K-1)*MAM
      KK=K1+K
      PIV=1./AMA(KK)
      DO 20 J=1,MAM
      KJ=K1+J
   20 AMA(KJ)=AMA(KJ)*PIV
      AMA(KK)=PIV
      DO 50 I=1,MAM
      I1=(I-1)*MAM
      IK=I1+K
      IF(I-K) 30,50,30
   30 ELEM=AMA(IK)
      AMA(IK)=0.
      DO 40 J=1,MAM
      IJ=I1+J
      KJ=K1+J
   40 AMA(IJ)=AMA(IJ)-ELEM*AMA(KJ)
   50 CONTINUE
   60 CONTINUE
      RETURN
      END
C     ***********************************************************
      SUBROUTINE RIPAT
C     ***********************************************************
      IMPLICIT DOUBLE PRECISION(A-H,O-Z) 
C     BELOW CARD IS NECESSARY IF MEMORY IS LESS THAN 32Mb
      DOUBLE PRECISION VL,VSL,PV,PW,WSL,WPSL,PL,PSL,PR,PI,PVC,PRC
      COMMON/POTEB/R,DE,VP,WP
     */AB/DEF(300),VL(300),VSL(480000),POL(5,300),
     *PV(2400000),PW(2400000),WSL(480000)
     */ABEC/PL(300),PSL(480000),PR(2400000),PI(2400000),WPSL(480000)
     */ABCOUL/PVC(5400),PRC(5400),CVNC(720000)
      COMMON/POTPN/PVV(120000),PWW(120000),PRR(120000),PII(120000)
      COMMON/POTB/WNK(40),WN(40),VR,WC,WD
     */STR/STEP,RK,NH1
     */ECI/NECI,NHI
     */POT1/VR3,VR2,VR1,VR0,WC1,WC0,WD1,WD0,VS,AC0,AR0,AW0,AD0,AS0
     */POT2/AR1,AC1,AW1,AD1,AS1
     */POT3/BNDC,WDA1,WCA1,CCOUL,CISO,WCISO,WS0,WS1
     */POTD/ALAVR,VRLA,WCBW,WCWID,WDBW,WDWID,ALAWD,EFERMN,EFERMP,ALASO
     *,PDIS,WSBW,WSWID,RRBWC,RRWID,RZBWC,RZWID
      COMMON/QNB/JO(40),NPO(40),KO(40),NCA(40)
      COMMON/RAD/RR,RC,RD,RW,RS,AR,AC,AW,AD,AS,ALF,AT,ANEU,RZ,ZNUC,ASP
     *,AZ
     */ENA/EN,EL(40),BET(10),NUR,NMAX,NPD,LAS
      COMMON/MENU/MEJOB,MEPOT,MEHAM,MECHA,MEPRI,MESOL,MESHA,MESHO,MEHAO
     *,MEAPP,MEVOL,MEREL,MECUL,MERZZ,MERRR,MEDIS,MERIP
     */COUL/CONZ,ETA,COPH(40,90)
     */COULON/PZI(300)
     */DISPE/VD,VRDC,EA,WDISO
      COMMON/VHFS/WW4,VHF,ENCON,VRLANP,EFERM,AMI
      COMMON/RIP/ST,NH,NAN1,NAN2
      COMMON/DISPE2/VRD,WDSHI,WDWID2,ALFNEW

      DO 97 I=1,600000
      IF(I.GT.120000) GO TO 78
      PVV(I)=0.D0
      PWW(I)=0.D0
      PRR(I)=0.D0
      PII(I)=0.D0
      PSL(I)=0.D0
      WPSL(I)=0.D0
      VSL(I)=0.D0
      WSL(I)=0.D0
   78 PV(I)=0.D0
      PW(I)=0.D0
      PR(I)=0.D0
      PI(I)=0.D0
   97 CONTINUE

      CCDE=0.0
      CCCOUL=CCOUL
      IF(MECUL.GE.2) CCDE=CCOUL
      IF(MECUL.GE.2) CCOUL=0.0
      CDE=CCDE*MECHA*ZNUC/AT**(1.D0/3.D0)
      CDE12=CCDE*ZNUC/AT**(1.D0/3.D0)
 
      VRDC=0.0
      VD=0.0
      VDISP=0.0
      CARBUN=931.49378D0
      AMI=939.56536D0
      IF(MECHA.EQ.1) AMI=938.272029D0
     
      IF(MECHA.EQ.1.AND.IDINT(ASP+0.1D0).EQ.1) AMI=1875.612859D0 
      IF(MECHA.EQ.1.AND.IDINT(ASP+0.1D0).EQ.1) ANEU=2.013553212712D0 
      
      DAVERN=0.00
      DAVERP=0.00
      EFERM=EFERMN
      EPAVER=EFERM+DAVERN
CCHA      IF(MECHA.EQ.1) EFERM=EFERMP
      IF(MECHA.EQ.1) EFERM=EFERMP+CDE 
      IF(MECHA.EQ.1) EPAVER=EFERM+DAVERP
      EINT=EN
CCHA      EN=EN-EFERM-CDE
      EN=EN-EFERM
          
      CALL POTVOL
      EN=EINT

             RZIN=RZ
                 IF(MERZZ.EQ.0) GO TO 52
                 IF(MERZZ.EQ.1)   RZ=RZ*(1.D00-RZBWC*(EN-EFERM)**PDIS/
     *((EN-EFERM)**PDIS+RZWID**PDIS))

   52        RRR=RR
      APAN=ANEU/1.00866491600D0 
C
C           RR ENERGY DEPENDENCE
C
      IF(MERRR.EQ.1)   RR=RR*(1.D00-RRBWC*(EN-EFERM)**PDIS/
     *((EN-EFERM)**PDIS+RRWID**PDIS))
      DO 1 I=1,NUR
      REL=(EN+AMI)/AMI
      IF(MEREL.EQ.0) REL=1.D+0
      E=EN-EL(I)/AT*(AT+ANEU*REL)
      REL=(ABS(E)+AMI)/AMI
      IF(MEREL.EQ.0.OR.E.LE.0.) REL=1.D+0
      IF(MEREL.NE.0.AND.E.GT.0.) E=(REL**2-1.D+0)*AMI/2.D+0

      WNK(I)=0.219677*SQRT(ABS(E))*AT/SQRT(AT**2+2.D+0*ANEU*REL*AT+
     *ANEU**2)*SQRT(APAN)
      WN(I)=WNK(I)*WNK(I)
      IF(EN-EL(I)/AT*(AT+ANEU*REL).LE.0.) WN(I)=-WN(I)
    1 CONTINUE

      
      REL=(EN+AMI)/AMI
      IF(MEREL.EQ.0) REL=1.D+0
      IF(REL.NE.1.) RELPOT=2.D+0*(EN+AMI)/(2.D+0*AMI+EN)

      EIRELM=SQRT(REL**2*AT**2+2.D+0*REL*ANEU*AT+ANEU**2)
      EIRELT=AT*SQRT(REL**2*ANEU**2+2.D+0*REL*ANEU*AT+AT**2)
      DEREL=SQRT(2.D+0*REL*ANEU*AT+AT**2+ANEU**2)
      WW=4.8257984E-2*EIRELM*EIRELT/(ANEU*EIRELM+EIRELT)/DEREL*APAN
      IF(MEREL.EQ.1.OR.MEREL.EQ.3) WW=WW*RELPOT
      CONZ=MECHA*ZNUC*EIRELM*EIRELT/(ANEU*EIRELM+EIRELT)/DEREL*
     *3.458814365D-2*APAN
      CONZ12=ZNUC*EIRELM*EIRELT/(ANEU*EIRELM+EIRELT)/DEREL*
     *3.458814365D-2*APAN

      CONZZ=CONZ
         
      
         

CCHA      ENEF=EN-EFERM-CDE
CCHA      EN=EN-CDE
      ENEF=EN-EFERM
      EN=EN
      
      VISO=CISO*(AT-2.*ZNUC)/AT
      WISO=WCISO*(AT-2.*ZNUC)/AT
      WVISO=WDISO*(AT-2.*ZNUC)/AT
      IF(MECHA.EQ.0) VISO=-VISO
      IF(MECHA.EQ.0) WISO=-WISO
      IF(MECHA.EQ.0) WVISO=-WVISO
      VRLANP=VRLA+VISO


C      WCP=Wv(Ef-Ea)=Wv(Ef+Ea)

      WCP=(WCBW+WVISO)*(EA)**PDIS/
     *(EA**PDIS+WCWID**PDIS)

C     TWO OPTIONS FOR COULOMB CORRECTIONS
C     !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!


      VRDIR=((VR1+2.*VR2*ENEF+3.*VR3*ENEF**2
     *-VRLA*ALAVR*EXP(-ALAVR*ENEF)))*(1.D0+VISO/(VR0+VRLA))
     **(-1.D0)

      IF(MEDIS.EQ.0) GO TO 391
C    !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
C     VHF DERIVATIVE AT EN
      EEE=EN
      EN=EN-0.005D+00
      ENCON=EN

      CALL VHFROM
      VHFM=VHF

      EN=EN+0.01D+00
      ENCON=EN

      CALL VHFROM
      VHFP=VHF

      VRDIR=(VHFM-VHFP)*100
      EN=EEE

C    !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!


      VDISP=DOM_INT_WV(Eferm,EPAVER,WCBW+WVISO,WCWID,INT(PDIS),EN,VDISD)

      T12D = 0.D0
      IF(EA.GT.0.d0.AND.WC.NE.0.D0) THEN
       T12P=WCP*DOM_INT_T1(Eferm,EA,EN+0.01)+
     *ALFNEW*DOM_INT_T2(EFERM,EA,EN+0.01)
       T12M=WCP*DOM_INT_T1(Eferm,EA,EN-0.01)+
     *ALFNEW*DOM_INT_T2(EFERM,EA,EN-0.01)
       T12D=(T12M-T12P)*50
      ENDIF
      VRDIRC=VDISD+T12D

      VDP=DOM_INT_WS
     * (Eferm,EPAVER,WDBW+WISO,WDWID,ALAWD,INT(PDIS),EN,VDD)

       VDCUL=VDD*MECHA*ZNUC/AT**0.333333333*CCOUL

      PDIS1=PDIS-1
C
      WDUL=0.
      WCUL=0.

      WCUL=0.
C
      VCULC=MECHA*ZNUC/AT**0.333333333*CCOUL*VRDIRC
  391 VCUL=MECHA*ZNUC/AT**0.333333333*CCOUL*VRDIR

      IF(MECUL.EQ.1) VCUL=MECHA*ZNUC/AT**0.333333333*CCOUL
C
      MUP=-1.91301
      IF(MECHA.NE.0) MUP=2.2928
      MUP=0.
C
      VR=(VR0+VR1*ENEF+VR2*ENEF**2+VR3*ENEF**3
     *+VRLA*EXP(-ALAVR*ENEF))*(1.D00+VISO/(VR0+VRLA))+VCUL

      IF(MEDIS.EQ.0) GO TO 392
C    !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
C     VHF AND VR AT EN

      ENCON=EN

      CALL VHFROM
      VR=VHF+VCUL


C    !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

      VDIS=VDISP

      VD=VDP+VDCUL

      WRITE(21,787) 
     * EN,VRLA,ALAVR,ENEF,VISO,VCUL,VR0, VR, VD, VDP, VDCUL,
     * VDIS, VDISD,VDD,T12D

  787 FORMAT(/2X,' POTENTIAL VALUES (I) FOR INCIDENT ENERGY=',F11.6,1X,
     *'MeV:'//,
     *3X,'VRLA=',F8.3,3X,'ALAVR=',F8.3,3X,
     *                       '   Ef=',F8.3,3X,'VISO=',F8.3/
     *3X,'VCUL=',F8.3,3X,'  VR0=',F8.3,3X,
     *                       '   VR=',F8.3/
     *3X,'  VD=',F8.3,3X,'  VDP=',F8.3,3X,
     *                       'VDCUL=',F8.3/
     *3X,'VDIS=',F8.3,3X,'VDISD=',F8.3,3X,
     *                       '  VDD=',F8.3,3X,'T12D=',F8.3/)

C     VR = VR + VDIS
      VRDC=VDIS+VCULC

  392 CONTINUE


      IF(EN.LT.BNDC) GO TO 800
      WD=WD0+WD1*BNDC+(ENEF-BNDC)*WDA1+(WDBW+WISO)*EXP(-ALAWD*ENEF)
     **ENEF**PDIS/(ENEF**PDIS+WDWID**PDIS)+WDUL
      WC=WC0+WC1*BNDC+(ENEF-BNDC)*WCA1+(WCBW+WVISO)*ENEF**PDIS/
     *(ENEF**PDIS+WCWID**PDIS)+WCUL
      GO TO 801
  800 WD=WD0+WD1*ENEF+(WDBW+WISO)*EXP(-ALAWD*ENEF)
     **ENEF**PDIS/(ENEF**PDIS+WDWID**PDIS)+WDUL
      WC=WC0+WC1*ENEF+(WCBW+WVISO)*ENEF**PDIS/
     *(ENEF**PDIS+WCWID**PDIS)+WCUL




C     !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

  801   REL=(EN+AMI)/AMI
      IF(REL.LT.1.D00) REL=1.D00
      EIRELM=SQRT(REL**2*AT**2+2.D+0*REL*ANEU*AT+ANEU**2)
      EIRELT=AT*SQRT(REL**2*ANEU**2+2.D+0*REL*ANEU*AT+AT**2)
      DEREL=SQRT(2.D+0*REL*ANEU*AT+AT**2+ANEU**2)
C        ENCON=(REL**2-1.D0)*AMI*AT*(ANEU+AT)/DEREL**2/2.D+0
      WW4=1.2064496D-2*EIRELM*EIRELT/(ANEU*EIRELM+EIRELT)/DEREL*APAN

C     !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
       WW4=1.
       VSO=VS*EXP(-WW4*ALASO*(ENEF+CDE))
      IF(MEDIS.EQ.0) GO TO 491
      IF(MEDIS.EQ.2) GO TO 492

CCHA       DVSO=DOM_INT_WV(Eferm,EPAVER,WSBW,WSWID,INT(PDIS),EN+CDE,dtmp)
       DVSO=DOM_INT_WV(Eferm-CDE,EPAVER,WSBW,WSWID,INT(PDIS),EN,dtmp)
       VSO=VSO+DVSO
 
  492 IF(EA.GT.0.D0.AND.WC.NE.0.D0) THEN
      VRDC=WCP*DOM_INT_T1(Eferm,EA,EN)+ALFNEW*DOM_INT_T2(EFERM,EA,EN)
     *+VRDC
      IF(EN.GT.EFERM+EA) 
     *WC=WC+ALFNEW*(SQRT(EN)+(EFERM+EA)**1.5D0/
     *(2.D0*EN)-1.5D0*SQRT(EFERM+EA))
      ENDIF

  491 CONTINUE
      WSO=WS0+WS1*ENEF+WSBW*(ENEF+CDE)**PDIS/
     *((ENEF+CDE)**PDIS+WSWID**PDIS)
      IF(WD.LT.0.) WD=0.D0
      IF(WC.LT.0.) WC=0.D0

      EN=EINT
      WRITE(21,789) EN,VR,VRDC,VD,WC,WD,VSO,DVSO,WSO,VHF,VCUL,VCULC,
     *RELPOT
      IF(MEPRI.NE.99)  
     *PRINT 789,EN,VR,VRDC,VD,WC,WD,VSO,DVSO,WSO,VHF,VCUL,VCULC,RELPOT
  789 FORMAT(/2X,' POTENTIAL VALUES (II) FOR INCIDENT ENERGY=',F11.6,1X,
     *'MeV:'//,
     *3X,' VR=',F8.3,3X,'VRDC(DWv)=',F8.3,3X,
     *                   '  DWs=',F8.3,3X,' Wv=',F8.3/
     *3X,' Ws=',F8.3,3X,'      VSO=',F8.3,3X,
     *                   ' DVSO=',F8.3,3X,'WSO=',F8.3/
     *3X,'VHF=',F8.3,3X,'     VCUL=',F8.3,3X,
     *                   'VCULC=',F8.3,3X,'RELPOT=',F8.3/)

      AR=AR0+AR1*ENEF
      AW=AW0+AW1*ENEF
      AC=AC0+AC1*ENEF
      AD=AD0+AD1*ENEF
      IF (ENEF.GT.BNDC) AD=AD0+AD1*BNDC
      AS=AS0+AS1*ENEF


      RK=RR+(LOG(VR/(EN-EL(NMAX)))+10.)*AR
      
      IF(WD.EQ.0.) RKD=RK
      IF(WD.EQ.0.) GO TO 802
      RKD=RD+(LOG(WD/(EN-EL(NMAX)))+10.)*AD
  802 IF(RKD.GT.RK) RK=RKD
c       IF(MECHA.NE.0) RK=RK*4.D0
C      IF(MECHA.NE.0) RK=RK+ 3.0*RR
c      IF(RK.GT.30.) RK=30.
      RKC=3.*(MECHA*ZNUC*RR**2/EN)**0.3333333



       IF(RKC.GT.RK) RK=RKC

      IF(MECHA.NE.0) RK=RK

      

      NH=RK/AR/0.3+1
      NH1=RK/AD/0.8+1
      NH2=RK/AC/0.8+1
      NH3=RK*WNK(1)/0.5+1
      IF(NH1.GT.NH) NH=NH1
      IF(NH2.GT.NH) NH=NH2
      IF(NH3.GT.NH) NH=NH3
      IF(NH.GT.270) NH=270
      LAS1=LAS+1
      LAS2=(LAS+2)/2
      IF(MEPOT.EQ.1) GO TO 39
      LAS2=LAS+1
   39 IF(MEPOT.EQ.1) GO TO 40
      GO TO 41
   40 IF(NPD.EQ.0) GO TO 41
      AN=0.
      DO 2 L=2,NPD,2
    2 AN=AN+ABS(BET(L))*L*LAS2
      NAN1=AN*25
      NAN1=2*NAN1+2
      IF(NAN1.LT.40) NAN1=40
      IF(NAN1.GT.298) NAN1=298
      NAN2=NAN1+1
      ST=1./(NAN2-1)
      DO 3 N=1,NAN2
      X=ST*(N-1)
      A=0.2820947917739D0
      B=X*A
      POL(1,N)=A
      DEF(N)=1.
      DO 4 K=3,LAS1,2
      KK=K-1
      C=((2.*KK-1.D0)*X*B-(KK-1.D0)*A)/KK
C     POL - SPHERICAL FUNCTIONS L-1,ANGLE
      POL((KK+2)/2,N)=C*SQRT(2.D0*KK+1.D0)
      IF(KK.LE.NPD) DEF(N)=DEF(N)+BET(KK)*C*SQRT(2.D0*KK+1.D0)
      A=B
      B=C
      KK=KK+1
      C=((2.*KK-1.D0)*X*B-(KK-1.D0)*A)/KK
      A=B
      B=C
    4 CONTINUE
    3 CONTINUE
   41 PIM=1.996852
      MNULA=300*NUR*LAS2
      LALAS=9
      LASC=1
      IF(LAS2.GE.3) LALAS=18
      IF(LAS2.GE.3) LASC=2
      MLA=300*LAS2
      MNU=300*NUR
      DO 38 NU1=1,NUR
      MNULA1=MNULA*(NU1-1)
      MNU1=MNU*(NU1-1)
      DO 38 NU2=1,NUR
      MNULA2=MNULA1+MLA*(NU2-1)
      MNU2=MNU1+300*(NU2-1)
      EN=EINT-(EL(NU1)+EL(NU2))/2
C     if(nu1.ne.nu2)EN=EINT-ABS(EL(NU1)-EL(NU2))/2.
       if(nu1.ne.nu2)EN=EINT-(EL(NU1)+EL(NU2))/2.
      IF(MEAPP.EQ.1) EN=EINT



C     SPHERICAL PART OF THE POTENTIAL AND COUPLING IN PARTITIONS  !!!!!!!!
      
      IF(NCA(NU1).NE.NCA(NU2)) GO TO 38


      VISO=CISO*(AT-2.*ZNUC)/AT
      WISO=WCISO*(AT-2.*ZNUC)/AT
      WVISO=WDISO*(AT-2.*ZNUC)/AT
      IF(MECHA.EQ.0) VISO=-CISO*(AT-2.*ZNUC)/AT
      IF(MECHA.EQ.0) WISO=-WCISO*(AT-2.*ZNUC)/AT
      IF(MECHA.EQ.0) WVISO=-WDISO*(AT-2.*ZNUC)/AT


      



      IF(MECHA.EQ.1.AND.NCA(1).NE.NCA(NU1)) VISO=-CISO*(AT-2.*ZNUC-2.D0)
     */AT
      IF(MECHA.EQ.1.AND.NCA(1).NE.NCA(NU1))WISO=-WCISO*(AT-2.*ZNUC-2.D0)
     */AT
      IF(MECHA.EQ.1.AND.NCA(1).NE.NCA(NU1))WVISO=-WDISO*(AT-2.*ZNUC-2.D0
     *)/AT
      

C      WCP=Wv(Ef-Ea)=Wv(Ef+Ea)

      WCP=(WCBW+WVISO)*(EA)**PDIS/
     *(EA**PDIS+WCWID**PDIS)



      

      IF(MECHA.EQ.1.AND.NCA(1).NE.NCA(NU1)) CONZ=0.D0
      IF(MECHA.EQ.0.AND.NCA(1).NE.NCA(NU1)) CONZ=CONZ12
      IF(MECHA.EQ.1.AND.NCA(1).EQ.NCA(NU1)) CONZ=CONZ12
      IF(MECHA.EQ.0.AND.NCA(1).NE.NCA(NU1)) CONZ=0.D0
      
      IF(MECHA.EQ.1.AND.NCA(1).NE.NCA(NU1)) EFENU12=EFERMN
      IF(MECHA.EQ.0.AND.NCA(1).NE.NCA(NU1)) EFENU12=EFERMP
      IF(MECHA.EQ.1.AND.NCA(1).EQ.NCA(NU1)) EFENU12=EFERMP
      IF(MECHA.EQ.0.AND.NCA(1).EQ.NCA(NU1)) EFENU12=EFERMN
      
      


                          
      
      IF(MECHA.EQ.1.AND.NCA(1).NE.NCA(NU1)) ENEF=EN-EFENU12
      IF(MECHA.EQ.1.AND.NCA(1).NE.NCA(NU1)) EN=EN

CCHA      IF(MECHA.EQ.1.AND.NCA(1).EQ.NCA(NU1)) ENEF=EN-EFENU12-CDE12
CCHA      IF(MECHA.EQ.1.AND.NCA(1).EQ.NCA(NU1)) EN=EN-CDE12
      IF(MECHA.EQ.1.AND.NCA(1).EQ.NCA(NU1)) ENEF=EN-EFENU12
      IF(MECHA.EQ.1.AND.NCA(1).EQ.NCA(NU1)) EN=EN
      
CCHA     IF(MECHA.EQ.0.AND.NCA(1).NE.NCA(NU1)) ENEF=EN-EFENU12-CDE12
CCHA      IF(MECHA.EQ.0.AND.NCA(1).NE.NCA(NU1)) EN=EN-CDE12
      IF(MECHA.EQ.0.AND.NCA(1).NE.NCA(NU1)) ENEF=EN-EFENU12
      IF(MECHA.EQ.0.AND.NCA(1).NE.NCA(NU1)) EN=EN

      IF(MECHA.EQ.0.AND.NCA(1).EQ.NCA(NU1)) ENEF=EN-EFENU12
      IF(MECHA.EQ.0.AND.NCA(1).EQ.NCA(NU1)) EN=EN



C     TWO OPTIONS FOR COULOMB CORRECTIONS
C
      
      COENH=1.D0

 
  876 VRLANP=VRLA+COENH*VISO



      VRDIR=((VR1+2.*VR2*ENEF+3.*VR3*ENEF**2
     *-VRLA*ALAVR*EXP(-ALAVR*ENEF)))*(1.D00+VISO/(VR0+VRLA))
     **(-1.D0)

      IF(MEDIS.EQ.0) GO TO 394


C     VHF DERIVATIVE AT EN
      EEE=EN
      EN=EN-0.005D+00
      ENCON=EN

      CALL VHFROM
      VHFM=VHF

      EN=EN+0.01D+00
      ENCON=EN

      CALL VHFROM
      VHFP=VHF

      VRDIR=(VHFM-VHFP)*100
      EN=EEE

C     DISPERSIVE WC CONTRIBUTION TO VR






      VDISP=DOM_INT_WV(Eferm,EPAVER,WCBW+WVISO,WCWID,INT(PDIS),EN,VDISD)
      




      T12D=0.d0
      IF(EA.GT.0.d0.AND.WC.NE.0.D0) THEN
      T12P=WCP*DOM_INT_T1(Eferm,EA,EN+0.01)+
     *ALFNEW*DOM_INT_T2(EFERM,EA,EN+0.01)
      T12M=WCP*DOM_INT_T1(Eferm,EA,EN-0.01)+
     *ALFNEW*DOM_INT_T2(EFERM,EA,EN-0.01)
      T12D=(T12M-T12P)*50.
      ENDIF
      VRDIRC=VDISD+T12D


      VCULC=MECHA*ZNUC/AT**0.333333333*CCOUL*VRDIRC
      WDUL=0.
      WCUL=0.

      
C     DISPERSIVE WD CONTRIBUTION TO VR

  984 CONTINUE
      
      IF(ALAWD.NE.0.D0) THEN    
        VDP=DOM_INT_WS
     * (Eferm,EPAVER,WDBW+WISO,WDWID,ALAWD,INT(PDIS),EN,VDD)
      ELSE
        VDP=DOM_INT_WV(Eferm,EPAVER,WDBW+WISO,WDWID,INT(PDIS),EN,VDD)
      ENDIF

      IF(EN.GT.WDSHI.AND.ALAWD.EQ.0.D+00) THEN 
        VDP1=VDP
        VDD1=VDD
        VDP=DOM_INT_WV(Eferm,WDSHI,WDBW+WISO,WDWID2,INT(PDIS),EN,VDD)
        VDP=VDP1-VDP
        VDD=VDD1-VDD 
      ENDIF      

          IF(EN.GT.EA) VDP=VDP+VRD*(EN-EA)
          IF(EN.GT.EA) VDD=VDD+VRD


      VDCUL=VDD*MECHA*ZNUC/AT**0.333333333*CCOUL

          
C
       PDIS1=PDIS-1

C
      WDUL=0.

      WCUL=0.

  394 VCUL=MECHA*ZNUC/AT**0.333333333*CCOUL*VRDIR
      IF(MECUL.EQ.1) VCUL=MECHA*ZNUC/AT**0.333333333*CCOUL

 

      VR=(VR0+VR1*ENEF+VR2*ENEF**2+VR3*ENEF**3
     *+VRLA*EXP(-ALAVR*ENEF))*(1.D00+VISO/(VR0+VRLA))+VCUL

      IF(MEDIS.EQ.0) GO TO 395

C    !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!




C     VHF AND VR AT EN

      

      ENCON=EN

      CALL VHFROM
      
       VR=VHF+VCUL
       VRDC=VDISP+VCULC


C         ENEF=ENCON-EFERM

C    !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!






       VD=VDP+VDCUL

  395 CONTINUE

  



      ENE=ENEF
      IF(MECUL.EQ.3) ENE=ENE+CDE

      ENESHID=ENE-WDSHI+EFERM                  !!!!!!NEEDS to be CHECKED
      
      IF(ENE.LT.BNDC) GO TO 900
      WD=WD0+WD1*BNDC+(ENE-BNDC)*WDA1+(WDBW+WISO)*EXP(-ALAWD*ENE)
     **ENE**PDIS/(ENE**PDIS+WDWID**PDIS)+WDUL
      IF(ENE+EFERM.GT.WDSHI.AND.ALAWD.EQ.0.D+00)WD=WD-(WDBW+WISO)
     **ENESHID**PDIS/(ENESHID**PDIS+WDWID2**PDIS)
      WC=WC0+WC1*BNDC+(ENE-BNDC)*WCA1+(WCBW+WVISO)*ENE**PDIS/
     *(ENE**PDIS+WCWID**PDIS)+WCUL
      GO TO 901
  900 WD=WD0+WD1*ENE+(WDBW+WISO)*EXP(-ALAWD*ENE)
     **ENE**PDIS/(ENE**PDIS+WDWID**PDIS)+WDUL
      IF(ENE+EFERM.GT.WDSHI.AND.ALAWD.EQ.0.D+00)WD=WD-(WDBW+WISO)
     **ENESHID**PDIS/(ENESHID**PDIS+WDWID2**PDIS)

      WC=WC0+WC1*ENE+(WCBW+WVISO)*ENE**PDIS/
     *(ENE**PDIS+WCWID**PDIS)+WCUL


CCCCC 
      
  901 CONTINUE


C     !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
  972 REL=(EN+AMI)/AMI
      IF(REL.LT.1.D00) REL=1.D00
      EIRELM=SQRT(REL**2*AT**2+2.D+0*REL*ANEU*AT+ANEU**2)
      EIRELT=AT*SQRT(REL**2*ANEU**2+2.D+0*REL*ANEU*AT+AT**2)
      DEREL=SQRT(2.D+0*REL*ANEU*AT+AT**2+ANEU**2)
C        ENCON=(REL**2-1.D0)*AMI*AT*(ANEU+AT)/DEREL**2/2.D+0
      WW4=1.2064496D-2*EIRELM*EIRELT/(ANEU*EIRELM+EIRELT)/DEREL*APAN

C     !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
       WW4=1.
CCHA       VSO=VS*EXP(-WW4*ALASO*(ENEF+CDE))
       VSO=VS*EXP(-WW4*ALASO*(ENEF))

      IF(MEDIS.EQ.0) GO TO 396
      IF(MEDIS.EQ.2) GO TO 397


CCHA      DVSO=DOM_INT_WV(Eferm,EPAVER,WSBW,WSWID,INT(PDIS),EN+CDE,dtmp)
      DVSO=DOM_INT_WV(Eferm,EPAVER,WSBW,WSWID,INT(PDIS),EN,dtmp)                   ???
       VSO=VSO+DVSO

 397  IF(EA.GT.0.D0.AND.WC.NE.0.D0) THEN
      VRDC=WCP*DOM_INT_T1(Eferm,EA,EN)+ALFNEW*DOM_INT_T2(EFERM,EA,EN)
     *+VRDC
      IF(EN.GT.EFERM+EA) 
     *WC=WC+ALFNEW*(SQRT(EN)+(EFERM+EA)**1.5D0/
     *(2.D0*EN)-1.5D0*SQRT(EFERM+EA))
      ENDIF
  396 CONTINUE
 
CCHA      WSO=WS0+WS1*ENEF+WSBW*(ENEF+CDE)**PDIS/
CCHA     *((ENEF+CDE)**PDIS+WSWID**PDIS)
      IF(MECUL.EQ.3) ENESO=ENEF+CDE
       WSO=WS0+WS1*ENESO+WSBW*(ENESO)**PDIS/
     *((ENESO)**PDIS+WSWID**PDIS)    
C     WD=WD+WISO
      IF(WD.LE.0.)WD=0.
      IF(WC.LE.0.)WC=0.
      AR=AR0+AR1*ENEF
      AW=AW0+AW1*ENEF
      AC=AC0+AC1*ENEF
      AD=AD0+AD1*ENEF
      IF (ENEF.GT.BNDC) AD=AD0+AD1*BNDC
      AS=AS0+AS1*ENEF
      IF(MEPOT.EQ.1) GO TO 17
      RAR=RR/AR
      RAC=RC/AC
      RAW=RW/AW
      RAD=RD/AD
   17 M=8
      M4=4*M
      KL=0
      IN=1
      KIT=M
      NIT=1
      IK=M4-1
C
      STEP=RK/NH
      STEP1=STEP/M
C
      DO 5 K=1,2
      DO 6 I=IN,IK
      LALASI=LALAS*(I-1)
      L1=(I-1)*LAS2+MNULA2
      L1AP=(I-1)*LAS2
      IF(K.EQ.2.AND.I.EQ.IK) GO TO 14
      R=STEP1*(I-KL)
      VSLC=-MUP*CONZ/RZ**3*PIM
      IF(R.GT.RZ) VSLC=-MUP*CONZ/R**3*PIM
      X=(R-RS)/AS
      II=MNU2+I
      IF(X.GT.23) GO TO 7
      EX1=EXP(X)
      VSLF=-PIM*EX1/AS/R/(1.D0+EX1)/(1.D0+EX1)
      GO TO 8
    7 EX1=EXP(-X)
      VSLF=-PIM*EX1/AS/R
    8 VL(I)=1.D0/R/R
      VSL(II)=VSLF*VSO*WW
      WSL(II)=VSLF*WSO*WW
      IF(MEREL.EQ.1.OR.MEREL.EQ.3) VSL(II)=VSLF*VSO*WW/RELPOT
      IF(MEREL.EQ.1.OR.MEREL.EQ.3) WSL(II)=VSLF*WSO*WW/RELPOT
      IF(I/KIT*KIT.NE.I) GO TO 15
      IR=I/KIT+NIT
      LALASR=LALAS*(IR-1)
      I1=(IR-1)*LAS2+MNULA2
      IIC=MNU2+IR
      PL(IR)=VL(I)
      PSL(IIC)=VSL(II)
      WPSL(IIC)=WSL(II)
      GO TO 15
   14 R=0.
      IR=1
      I1=MNULA2
      IIC=MNU2+1
   15 IF(AZ.GT.0.) GO TO 47
      PZ=CONZ/RZ*(3.-R*R/RZ/RZ)
      IF(R.GE.RZ) PZ=2.*CONZ/R
      GO TO 46
   47 CALL SPHEPOT
      PZ=2.*DE*CONZ
   46 IF(MEPOT.EQ.1) GO TO 18
      IF(LAS2.EQ.1) GO TO 21
      DO 20 LAC=1,9
      LALALI=LALASI+LASC*(LAC-1)
      LALALR=LALASR+LASC*(LAC-1)
      LAM=LAC-1
      IF(R.EQ.0.) RLAM=0.0
      IF(R.EQ.0.) RLAM1=0.0
      IF(R.NE.0.) RLAM=R**LAM
      IF(R.NE.0.) RLAM1=R**(LAM+1)
      PVC(LALALI+1)=6.*CONZ/(2.*LAM+1)*RLAM/RZ**(LAM+1)
      IF(R.GT.RZ) PVC(LALALI+1)=6.*CONZ/(2.*LAM+1)*RZ**LAM/RLAM1
C     PVC(LALALI+1)=0.0
      IF(LAS2.EQ.2) GO TO 20
      PVC(LALALI+2)=6.*CONZ/(2.*LAM+1)*(1.-LAM)*RLAM/RZ**(LAM+1)
C     PVC(LALALI+2)=6.*CONZ/(2.*LAM+1)*(LAM+2.)*RLAM/RZ**(LAM+1)
      IF(R.GT.RZ) PVC(LALALI+2)=6.*CONZ/(2.*LAM+1)*(LAM+2)*RZ**LAM/
     *RLAM1
C     PVC(LALALI+2)=0.0
   20 CONTINUE
   21 NT0=L1+1
      NT1=L1+2
      NT2=L1+3
      NT3=L1+4
      NT4=L1+5
      X=(R-RR)/AR
      IF(X.GT.23.) GO TO 30
      EX=EXP(X)
      EX1=1.D0+EX
      EXD=1.D0/EX1
      EXDM=1.D0-EXD
      PV(NT0)=-VR*EXD*WW+PZ
      IF(LAS.EQ.0) GO TO 31
      PV(NT1)=(PV(NT0)-PZ)*EXDM*RAR
      IF(LAS.EQ.1) GO TO 31
      Y1=1.D0-2.D0*EXD
      PV(NT2)=PV(NT1)*Y1*RAR/2.D0
      IF(LAS.EQ.2) GO TO 31
      Y2=1.D0-6.D0*EXD*EXDM
      PV(NT3)=PV(NT2)/Y1*Y2*RAR/3.D0
      IF(LAS.EQ.3) GO TO 31
      Y3=1.-EXD*(14.D0-EXD*(36.D0-24.D0*EXD))
      PV(NT4)=PV(NT3)/Y2*Y3*RAR/4.D0
      GO TO 31
   30 EX1=EXP(-X)
      PV(NT0)=-VR*EX1*WW+PZ
      IF(LAS.EQ.0) GO TO 31
C     PV(NT1)=PV(NT0)*RAR
      PV(NT1)=(PV(NT0)-PZ)*RAR
      IF(LAS.EQ.1) GO TO 31
      PV(NT2)=PV(NT1)*RAR/2.D0
      IF(LAS.EQ.2) GO TO 31
      PV(NT3)=PV(NT2)*RAR/3.D0
      IF(LAS.EQ.3) GO TO 31
      PV(NT4)=PV(NT3)*RAR/4.D0
   31 X=(R-RC)/AC
      Y=(R-RW)/AW
      XX=Y*Y
      IF(XX.GT.180.) EXX=0.
      IF(XX.LE.180.) EXX=EXP(-XX)
      IF(X.GT.23.) GO TO 32
      EX=EXP(X)
      EX1=1.D0+EX
      EXD=1.D0/EX1
      EXDM=1.D0-EXD
      WCC=-WC*ALF*EXD*WW
      WWW=-WC*(1.D0-ALF)*EXX*WW
      IF(MEREL.EQ.3) WCC=WCC/RELPOT
      IF(MEREL.EQ.3) WWW=WWW/RELPOT
      PW(NT0)=WCC+WWW
      PV(NT0)=PV(NT0)+WCC*VRDC/WC
      IF(LAS.EQ.0) GO TO 33
      WCC=WCC*EXDM*RAC
      WWW=WWW*2.*RAW*Y
      PW(NT1)=WCC+WWW
       PV(NT1)=PV(NT1)+WCC*VRDC/WC
      IF(LAS.EQ.1) GO TO 33
      YC1=1.-2.*EXD
      WCC=WCC*YC1*RAC/2.
      Y1=2.*Y*Y-1.
      WWW=WWW/Y*Y1*RAW/2.
      PW(NT2)=WCC+WWW
       PV(NT2)=PV(NT2)+WCC*VRDC/WC
      IF(LAS.EQ.2) GO TO 33
      YC2=1.-6.*EXD*EXDM
      WCC=WCC/YC1*YC2*RAC/3.
      Y2=Y**3*2.-Y*3.
      WWW=WWW/Y1*Y2*RAW*2./3.
      PW(NT3)=WCC+WWW
       PV(NT3)=PV(NT3)+WCC*VRDC/WC
      IF(LAS.EQ.3) GO TO 33
      YC3=1.-EXD*(14.-EXD*(36.-24.*EXD))
      WCC=WCC/YC2*YC3*RAC/4.
      WWW=WWW/Y2*(Y**4*4.-Y*Y*12.+3.)*RAW/4.
      PW(NT4)=WCC+WWW
       PV(NT1)=PV(NT1)+WCC*VRDC/WC
      GO TO 33
   32 EX1=EXP(-X)
      WCC=-WC*ALF*EX1*WW
      WWW=-WC*(1.-ALF)*EXX*WW
      IF(MEREL.EQ.3) WCC=WCC/RELPOT
      IF(MEREL.EQ.3) WWW=WWW/RELPOT
      PW(NT0)=WCC+WWW
      PV(NT0)=PV(NT0)+WCC*VRDC/WC
      IF(LAS.EQ.0) GO TO 33
      WCC=WCC*RAC
      WWW=WWW*2.*RAW*Y
      PW(NT1)=WCC+WWW
       PV(NT1)=PV(NT1)+WCC*VRDC/WC
      IF(LAS.EQ.1) GO TO 33
      WCC=WCC*RAC/2.
      Y1=2.*Y*Y-1.
      WWW=WWW/Y*Y1*RAW/2.
      PW(NT2)=WCC+WWW
       PV(NT2)=PV(NT2)+WCC*VRDC/WC
      IF(LAS.EQ.2) GO TO 33
      WCC=WCC*RAC/3.
      Y2=Y**3*2.-Y*3.
      WWW=WWW/Y1*Y2*RAW*2./3.
      PW(NT3)=WCC+WWW
       PV(NT3)=PV(NT3)+WCC*VRDC/WC
      IF(LAS.EQ.3) GO TO 33
      WCC=WCC*RAC/4.
      WWW=WWW/Y2*(Y**4*4.-Y*Y*12.+3.)*RAW/4.
      PW(NT4)=WCC+WWW
       PV(NT4)=PV(NT4)+WCC*VRDC/WC
   33 X=(R-RD)/AD
      IF(X.GT.23.) GO TO 34
      EX=EXP(X)
      EX1=1.D0+EX
      EXD=1.D0/EX1
      EXDM=1.D0-EXD
      WDD=-4.D0*WD*EXDM*EXD*WW
      IF(MEREL.EQ.3) WDD=WDD/RELPOT
      PW(NT0)=PW(NT0)+WDD
      PV(NT0)=PV(NT0)+WDD/WD*VD
      IF(LAS.EQ.0) GO TO 35
      Y1=1.D0-2.D0*EXD
      WDD=WDD*Y1*RAD
      PW(NT1)=PW(NT1)+WDD
       PV(NT1)=PV(NT1)+WDD/WD*VD
      IF(LAS.EQ.1) GO TO 35
      Y2=1.D0-6.D0*EXD*EXDM
      WDD=WDD/Y1*Y2*RAD/2.D0
      PW(NT2)=PW(NT2)+WDD
       PV(NT2)=PV(NT2)+WDD/WD*VD
      IF(LAS.EQ.2) GO TO 35
      Y3=1.D0-EXD*(14.D0-EXD*(36.D0-24.D0*EXD))
      WDD=WDD/Y2*Y3*RAD/3.D0
      PW(NT3)=PW(NT3)+WDD
       PV(NT3)=PV(NT3)+WDD/WD*VD
      IF(LAS.EQ.3) GO TO 35
      Y4=1.D0-EXD*(30.D0-EXD*(150.D0-EXD*(240.D0-120.D0*EXD)))
      WDD=WDD/Y3*Y4*RAD/4.D0
      PW(NT4)=PW(NT4)+WDD
       PV(NT4)=PV(NT4)+WDD/WD*VD
      GO TO 35
   34 EX1=EXP(-X)
      WDD=-4.D0*WD*EX1*WW
      IF(MEREL.EQ.3) WDD=WDD/RELPOT
      PW(NT0)=PW(NT0)+WDD
       PV(NT0)=PV(NT0)+WDD/WD*VD
      IF(LAS.EQ.0) GO TO 35
      WDD=WDD*RAD
      PW(NT1)=PW(NT1)+WDD
       PV(NT1)=PV(NT1)+WDD/WD*VD
      IF(LAS.EQ.1) GO TO 35
      WDD=WDD*RAD/2.D0
      PW(NT2)=PW(NT2)+WDD
       PV(NT2)=PV(NT2)+WDD/WD*VD
      IF(LAS.EQ.2) GO TO 35
      WDD=WDD*RAD/3.D0
      PW(NT3)=PW(NT3)+WDD
       PV(NT3)=PV(NT3)+WDD/WD*VD
      IF(LAS.EQ.3) GO TO 35
      WDD=WDD*RAD/4.
      PW(NT4)=PW(NT4)+WDD
       PV(NT4)=PV(NT4)+WDD/WD*VD
   35 IF(I/KIT*KIT.NE.I) GO TO 6
      PR(I1+1)=PV(NT0)
      PI(I1+1)=PW(NT0)
      IF(LAS.EQ.0) GO TO 6
      PR(I1+2)=PV(NT1)
      PI(I1+2)=PW(NT1)
      DO 23 LAC=1,8
      LALALI=LALASI+LASC*(LAC-1)
      LALALR=LALASR+LASC*(LAC-1)
      LAM=LAC-1
      PRC(LALALR+1)=PVC(LALALI+1)
      IF(LAS2.EQ.2) GO TO 23
      PRC(LALALR+2)=PVC(LALALI+2)
   23 CONTINUE
      IF(LAS.EQ.1) GO TO 6
      PR(I1+3)=PV(NT2)
      PI(I1+3)=PW(NT2)
      IF(LAS.EQ.2) GO TO 6
      PR(I1+4)=PV(NT3)
      PI(I1+4)=PW(NT3)
      IF(LAS.EQ.3) GO TO 6
      PR(I1+5)=PV(NT4)
      PI(I1+5)=PW(NT4)
      GO TO 6
   18 DO 9 L=1,LAS2
      
C      IF(NU1.NE.NU2.AND.L.EQ.1) GO TO 9
C      IF(NCA(NU1).NE.NCA(NU2)) GO TO 9

  109 PDC=0.
      LL2=L1+L
      LL2AP=L1AP+L
      IF(MEAPP.EQ.0) GO TO 44
      IF(NU1.EQ.NU2) GO TO 45
      GO TO 42
   45 IF(NU1.NE.1) GO TO 42
   44 DE=1.
      IF(L.EQ.1) GO TO 19
      LAC=2*(L-1)
      IF(LAC.GT.NPD) GO TO 19
      IF(R.LE.RZ) PDC=6.*CONZ/(2.*LAC+1.)*R**LAC/RZ**(LAC+1)*BET(LAC)
      IF(R.GT.RZ) PDC=6.*CONZ/(2.*LAC+1.)*RZ**LAC/R**(LAC+1)*BET(LAC)

C     HIGHER COULOMB MULTIPOLES AS PROPOSES BY SATCHLER

      IF(LAS2.EQ.1) GO TO 19
      IF(R.LE.RZ) FPVC=6.*CONZ/(2.*LAC+1)*(1.-LAC)*R**LAC/RZ**(LAC+1)
      IF(R.GT.RZ) FPVC=6.*CONZ/(2.*LAC+1)*(LAC+2)*RZ**LAC/R**(LAC+1)
      IF(LAC.EQ.2) PDC=PDC+FPVC*(0.180223752*BET(2)+0.241795536*BET(4))
     **BET(2)
      IF(LAC.EQ.4) PDC=PDC+FPVC*(0.241795536*BET(2)+0.163839774*BET(4))
     **BET(2)
      IF(LAC.EQ.6) PDC=PDC+FPVC*0.238565132*BET(2)*BET(4)
C
   19 IF(NPD.NE.0) DE=DEF(1)
      CALL POTET
      IF(NPD.EQ.0) GO TO 13
      VV=VP*POL(L,1)
      WV=WP*POL(L,1)
      N=NAN2
      DE=DEF(N)
      CALL POTET
      VV=VV+VP*POL(L,N)
      WV=WV+WP*POL(L,N)
      V4=0.
      W4=0.
      DO 10 N=2,NAN2,2
      DE=DEF(N)
      CALL POTET
      V4=V4+VP*POL(L,N)
   10 W4=W4+WP*POL(L,N)
      V2=0.
      W2=0.
      DO 11 N=3,NAN1,2
      DE=DEF(N)
      CALL POTET
      V2=V2+VP*POL(L,N)
   11 W2=W2+WP*POL(L,N)

c      write(21,1799) vr,vd,vrdc,wc,wd
c1799 format(6e12.7)
c      pause 1



        

      PV(LL2)=(VV+4.*V4+2.*V2)*ST*WW*4.1887902047864D0
      PW(LL2)=(WV+4.*W4+2.*W2)*ST*WW*4.1887903047864D0
      IF(MEREL.EQ.3) PW(LL2)=PW(LL2)/RELPOT
      IF(L.NE.1) PV(LL2)=PV(LL2)+PDC
      IF(L.NE.1) GO TO 16
      PV(LL2)=PV(LL2)*0.2820947917739D0+PZ
      PW(LL2)=PW(LL2)*0.2820947917739D0
      GO TO 16
   13 PV(LL2)=VP*WW
      IF(L.EQ.1) PV(LL2)=PV(LL2)+PZ
      PW(LL2)=WP*WW
      IF(MEREL.EQ.3) PW(LL2)=PW(LL2)/RELPOT
   16 CONTINUE
      GO TO 43
   42 PV(LL2)=PV(LL2AP)
      PW(LL2)=PW(LL2AP)
   43 IF(I/KIT*KIT.NE.I) GO TO 9
      II2=I1+L
      PR(II2)=PV(LL2)
      PI(II2)=PW(LL2)
    9 CONTINUE
    6 CONTINUE
      KL=M4-4
      IN=M4
      KIT=1
      NIT=5-M4
      IK=NH+KL+2
      STEP1=STEP
    5 CONTINUE
      NH1=NH+KL
      NHI=NH+2

C      write(21,1799) vr,vd,vrdc,wc,wd,enef,WDBW,WISO
C 1799 format(8e12.5)
C      pause 1
      
   38 CONTINUE
      EN=EINT
      RR=RRR
      RZ=RZIN
      
C      write(21,1799) vr,vd,vrdc,wc,wd
C 1799 format(6e12.5)
C      pause 1
      CCOUL=CCCOUL
      CONZ=CONZZ

C           WRITE(21,9997) RK, STEP,CONZ ,3.302222, NH1
C9997  FORMAT(4E12.5,I3)
      CALL RIPAT1
      RETURN
      END
C     ***********************************************************
      SUBROUTINE RIPAT1
C     ***********************************************************
      IMPLICIT DOUBLE PRECISION(A-H,O-Z) 
C     BELOW CARD IS NECESSARY IF MEMORY IS LESS THAN 32Mb
      DOUBLE PRECISION VL,VSL,PV,PW,WSL,WPSL,PL,PSL,PR,PI,PVC,PRC
      COMMON/POTEB/R,DE,VP,WP
     */AB/DEF(300),VL(300),VSL(480000),POL(5,300),
     *PV(2400000),PW(2400000),WSL(480000)
     */ABEC/PL(300),PSL(480000),PR(2400000),PI(2400000),WPSL(480000)
     */ABCOUL/PVC(5400),PRC(5400),CVNC(720000)
      COMMON/POTPN/PVV(120000),PWW(120000),PRR(120000),PII(120000)
      COMMON/POTB/WNK(40),WN(40),VR,WC,WD
     */SHEMM/ES(40),JJ(40),NTU(40),NNB(40),NNG(40),NNO(40),
     *NPI(40)
     */STR/STEP,RK,NH1
     */ECI/NECI,NHI
     */POT1/VR3,VR2,VR1,VR0,WC1,WC0,WD1,WD0,VS,AC0,AR0,AW0,AD0,AS0
     */POT2/AR1,AC1,AW1,AD1,AS1
     */POT3/BNDC,WDA1,WCA1,CCOUL,CISO,WCISO,WS0,WS1
     */POTD/ALAVR,VRLA,WCBW,WCWID,WDBW,WDWID,ALAWD,EFERMN,EFERMP,ALASO
     *,PDIS,WSBW,WSWID,RRBWC,RRWID,RZBWC,RZWID
      COMMON/QNB/JO(40),NPO(40),KO(40),NCA(40)
      COMMON/RAD/RR,RC,RD,RW,RS,AR,AC,AW,AD,AS,ALF,AT,ANEU,RZ,ZNUC,ASP
     *,AZ
     */ENA/EN,EL(40),BET(10),NUR,NMAX,NPD,LAS
      COMMON/MENU/MEJOB,MEPOT,MEHAM,MECHA,MEPRI,MESOL,MESHA,MESHO,MEHAO
     *,MEAPP,MEVOL,MEREL,MECUL,MERZZ,MERRR,MEDIS,MERIP
     */COUL/CONZ,ETA,COPH(40,90)
     */COULON/PZI(300)
     */DISPE/VD,VRDC,EA,WDISO
      COMMON/VHFS/WW4,VHF,ENCON,VRLANP,EFERM,AMI
      COMMON/RIP/ST,NH,NAN1,NAN2
      COMMON/DISPE2/VRD,WDSHI,WDWID2,ALFNEW


      CONZZ=CONZ


         

      

      APAN=ANEU/1.00866491600D0 

      CCDE=0.0
      CCCOUL=CCOUL
      CCOUL=CCOUL/2.0
      IF(MECUL.GE.2) CCDE=CCOUL
      IF(MECUL.GE.2) CCOUL=0.0
      CDE=CCDE*MECHA*ZNUC/AT**(1.D0/3.D0)
      CDE12=CCDE*ZNUC/AT**(1.D0/3.D0)


      VRDC=0.0
      VD=0.0
      VDISP=0.0
      CARBUN=931.49378D0
      AMI=939.56536D0
      IF(MECHA.EQ.1) AMI=938.272029D0
      
       IF(MECHA.EQ.1.AND.IDINT(ASP+0.1D0).EQ.1) AMI=1875.612859D0 
       IF(MECHA.EQ.1.AND.IDINT(ASP+0.1D0).EQ.1) ANEU=2.013553212712D0 
      
      DAVERN=0.00
      DAVERP=0.00
      EFERM=EFERMN
      EPAVER=EFERM+DAVERN
      IF(MECHA.EQ.1) EFERM=EFERMP 

      
C           THREE CARDS FOR U238 CHARGE, DELETE THEN FOR USUAL CALCUL.
C           TWO CARDS AT THE END OF SUBROUTINE
             RZIN=RZ
                 IF(MERZZ.EQ.0) GO TO 52
                 IF(MERZZ.EQ.1)   RZ=RZ*(1.D00-RZBWC*(EN-EFERM)**PDIS/
     *((EN-EFERM)**PDIS+RZWID**PDIS))
C             RZ=9.100
C             IF(EN.GT.26.) RZ=9.100-0.31324222*(EN-26.)
C             IF(EN.GT.26.) RZ=RZ-(RZ-6.28082)/9.*(EN-26.)
C             IF(EN.GT.35.) RZ=6.28082
   52        RRR=RR
      APAN=ANEU/1.00866491600D0 
C
C           RR ENERGY DEPENDENCE
C
C     IF(MERRR.EQ.1)   RR=RR*(1.D00-RRBWC*EN**2/(EN**2+RRWID**2))
      IF(MERRR.EQ.1)   RR=RR*(1.D00-RRBWC*(EN-EFERM)**PDIS/
     *((EN-EFERM)**PDIS+RRWID**PDIS))
      
     

      

      REL=(EN+AMI)/AMI
      IF(MEREL.EQ.0) REL=1.D+0
      IF(REL.NE.1.) RELPOT=2.D+0*(EN+AMI)/(2.D+0*AMI+EN)

      
      EIRELM=SQRT(REL**2*AT**2+2.D+0*REL*ANEU*AT+ANEU**2)
      EIRELT=AT*SQRT(REL**2*ANEU**2+2.D+0*REL*ANEU*AT+AT**2)
      DEREL=SQRT(2.D+0*REL*ANEU*AT+AT**2+ANEU**2)
      WW=4.8257984E-2*EIRELM*EIRELT/(ANEU*EIRELM+EIRELT)/DEREL*APAN
      IF(MEREL.EQ.1.OR.MEREL.EQ.3) WW=WW*RELPOT

            
      





      LAS1=LAS+1
      LAS2=(LAS+2)/2
      IF(MEPOT.EQ.1) GO TO 39
      LAS2=LAS+1
   39 CONTINUE

      
   41 PIM=1.996852
      
      MNULA=300*NUR*LAS2
      LALAS=9
      LASC=1
      IF(LAS2.GE.3) LALAS=18
      IF(LAS2.GE.3) LASC=2
      MLA=300*LAS2
      MNU=300*NUR



   
      EINT=EN
      
      DO 38 NU1=1,NUR
      MNULA1=MNULA*(NU1-1)
      MNU1=MNU*(NU1-1)
      DO 38 NU2=1,NUR
      MNULA2=MNULA1+MLA*(NU2-1)
      MNU2=MNU1+300*(NU2-1)
      EN=EINT-(EL(NU1)+EL(NU2))/2
C       if(nu1.ne.nu2)EN=EINT-ABS(EL(NU1)-EL(NU2))/2.
        if(nu1.ne.nu2)EN=EINT-(EL(NU1)+EL(NU2))/2.
      IF(MEAPP.EQ.1) EN=EINT
      


C     COUPLINGIN BETWEEN LEVELS OF THE DIFFERENT PARTITIONS    !!!!!!!!
      
      
      IF(NCA(NU1).EQ.NCA(NU2)) GO TO 38
      IF(MEHAM.EQ.1) GO TO 40
      IF(NTU(NU1).NE.NTU(NU2).OR.NNB(NU1).NE.NNB(NU2)
     *.OR.NNG(NU1).NE.NNG(NU2).OR.NNO(NU1).NE.NNO(NU2))GO TO 38

      
   40 EFENU12=(EFERMN+EFERMP)/2.D0
      
CCHA      ENEF=EN-EFENU12-CDE
      
CCHA      EN=EN-CDE

      ENEF=EN-EFENU12-1./2.*CDE             !!!!!!     ALD CASE  withOTHER RIGHT     !!!!!!!
      ENEF=EN-EFENU12 
      
      EN=EN     
      
      COENH=1.D0
C     
C     IF(MECUL.GE.2)    COENH=1.D0+ALAVR*VRLA*EXP(-ALAVR*ENEF)
      

  
  
      VISO=CISO*2.D0*SQRT(AT-2.D0*ZNUC)/AT
      WISO=WCISO*2.D0*SQRT(AT-2.D0*ZNUC)/AT
      WVISO=WDISO*2.D0*SQRT(AT-2.D0*ZNUC)/AT
      VRLANP=VISO*COENH
      WCBWNP=WVISO
      WDBWNP=WISO


C      WCP=Wv(Ef-Ea)=Wv(Ef+Ea)

      WCP=(WCBWNP)*(EA)**PDIS/
     *(EA**PDIS+WCWID**PDIS)

C      write(21,1799) vcp,wcbwnp

C      pause 89

      
C     TWO OPTIONS FOR COULOMB CORRECTIONS
C


      VRDIR=(VR1+2.*VR2*ENEF+3.*VR3*ENEF**2
     *-VRLA*ALAVR*EXP(-ALAVR*ENEF))*VISO/(VR0+VRLA)   
     **(-1.D0) 
      

      IF(MEDIS.EQ.0) GO TO 394
C    !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
C     VHF DERIVATIVE AT EN
      EEE=EN
      EN=EN-0.005D+00
      ENCON=EN

      CALL VHFROM
      VHFM=VHF

      EN=EN+0.01D+00
      ENCON=EN

      CALL VHFROM
      VHFP=VHF

      VRDIR=(VHFM-VHFP)*100
      EN=EEE



C     DISPERSIVE CONTRIBUTION TO VR

  983 VDISP=DOM_INT_WV(Eferm,EPAVER,WCBWNP,WCWID,INT(PDIS),EN,VDISD)





C     NO ISOSCALOR PART OF Wv
        
      IF(EA.GT.0.d0) THEN 
       T12P=WCP*DOM_INT_T1(Eferm,EA,EN+0.01)
       T12M=WCP*DOM_INT_T1(Eferm,EA,EN-0.01)
       T12D=(T12M-T12P)*50.
      ENDIF




      
      VRDIRC=VDISD+T12D



      
      VCULC=MECHA*ZNUC/AT**0.333333333*CCOUL*VRDIRC
      WDUL=0.
      WCUL=0.




C     DISPERSIVE WD CONTRIBUTION TO VR

      

  984        IF(ALAWD.NE.0.D0) THEN    
        VDP=DOM_INT_WS
     * (Eferm,EPAVER,WDBWNP,WDWID,ALAWD,INT(PDIS),EN,VDD)
      ELSE
        VDP=DOM_INT_WV(Eferm,EPAVER,WDBWNP,WDWID,INT(PDIS),EN,VDD)
      ENDIF

      IF(EN.GT.WDSHI.AND.ALAWD.EQ.0.D+00) THEN 
        VDP1=VDP
        VDD1=VDD
        VDP=DOM_INT_WV(Eferm,WDSHI,WDBWNP,WDWID2,INT(PDIS),EN,VDD)
        VDP=VDP1-VDP
        VDD=VDD1-VDD 
      ENDIF      


       


       VDCUL=VDD*MECHA*ZNUC/AT**0.333333333*CCOUL
C
       PDIS1=PDIS-1
      
C
      WDUL=0.
C
      WCUL=0.

  394 VCUL=MECHA*ZNUC/AT**0.333333333*CCOUL*VRDIR
      IF(MECUL.EQ.1) VCUL=MECHA*ZNUC/AT**0.333333333*CCOUL
      
C   

      VR=(VR0+VR1*ENEF+VR2*ENEF**2+VR3*ENEF**3
     *+VRLA*EXP(-ALAVR*ENEF))*VISO/(VR0+VRLA)+VCUL

      IF(MEDIS.EQ.0) GO TO 395
      
C    !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
C     VHF AND VR AT EN
      
      ENCON=EN

      CALL VHFROM
      
      
      VR=VHF+VCUL               !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
      
      VRDC=VDISP+VCULC
      
      
C         ENEF=ENCON-EFERM

C    !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!






       VD=VDP+VDCUL

  395 CONTINUE

      ENE=ENEF                       !!!!!!!!!!!!!!!!!!!!!!!!

CCHA      IF(MECUL.EQ.3) ENE=ENE-CDE   
      IF(MECUL.EQ.3) ENE=ENE+CDE/2.D0

     
      ENESHID=ENE-WDSHI+EFERM
      
      IF(ENE.LT.BNDC) GO TO 900
      WD=WD0+WD1*BNDC+(ENE-BNDC)*WDA1+(WDBWNP)*EXP(-ALAWD*ENE)
     **ENE**PDIS/(ENE**PDIS+WDWID**PDIS)+WDUL
      IF(ENE+EFERM.GT.WDSHI.AND.ALAWD.EQ.0.D+00)WD=WD-(WDBWNP)
     **ENESHID**PDIS/(ENESHID**PDIS+WDWID2**PDIS)
      WC=WC0+WC1*BNDC+(ENE-BNDC)*WCA1+(WCBWNP)*ENE**PDIS/
     *(ENE**PDIS+WCWID**PDIS)+WCUL
      GO TO 901
  900 WD=WD0+WD1*ENE+(WDBWNP)*EXP(-ALAWD*ENE)
     **ENE**PDIS/(ENE**PDIS+WDWID**PDIS)+WDUL
      IF(ENE+EFERM.GT.WDSHI.AND.ALAWD.EQ.0.D+00)WD=WD-(WDBWNP)
     **ENESHID**PDIS/(ENESHID**PDIS+WDWID2**PDIS)

      WC=WC0+WC1*ENE+(WCBWNP)*ENE**PDIS/
     *(ENE**PDIS+WCWID**PDIS)+WCUL





  901 CONTINUE



C     !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
  972 REL=(EN+AMI)/AMI
      IF(REL.LT.1.D00) REL=1.D00
      EIRELM=SQRT(REL**2*AT**2+2.D+0*REL*ANEU*AT+ANEU**2)
      EIRELT=AT*SQRT(REL**2*ANEU**2+2.D+0*REL*ANEU*AT+AT**2)
      DEREL=SQRT(2.D+0*REL*ANEU*AT+AT**2+ANEU**2)
C        ENCON=(REL**2-1.D0)*AMI*AT*(ANEU+AT)/DEREL**2/2.D+0
      WW4=1.2064496D-2*EIRELM*EIRELT/(ANEU*EIRELM+EIRELT)/DEREL*APAN

C     !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
       WW4=1.
c       VSO=VS*EXP(-WW4*ALASO*(ENEF+CDE))

      IF(MEDIS.EQ.0) GO TO 396
      IF(MEDIS.EQ.2) GO TO 397


      

C
C     NO ISOSCALAR CONTRIBUTION FROM NON SYMMETRIC IMAGINARY POSITIVE POTENTIAL Wv 
C
 397  IF(EA.GT.0.D0) VRDC=WCP*DOM_INT_T1(Eferm,EA,EN)+VRDC

  396 CONTINUE
      
      
C     WD=WD+WISO
      IF(WD.LE.0.)WD=0.
      IF(WC.LE.0.)WC=0.
      AR=AR0+AR1*ENEF
      AW=AW0+AW1*ENEF
      AC=AC0+AC1*ENEF
      AD=AD0+AD1*ENEF
      IF (ENEF.GT.BNDC) AD=AD0+AD1*BNDC
      AS=AS0+AS1*ENEF
      RAR=RR/AR
      RAC=RC/AC
      RAW=RW/AW
      RAD=RD/AD
      M=8
      M4=4*M
      KL=0
      IN=1
      KIT=M
      NIT=1
      IK=M4-1

      
C
      STEP=RK/NH
      STEP1=STEP/M
      
C
      DO 5 K=1,2
      DO 6 I=IN,IK
      


      



      II=MNU2+I
      IR=I/KIT+NIT
      I1=(IR-1)*LAS2+MNULA2
      LALASR=LALAS*(IR-1)
      IIC=MNU2+IR
      LALASI=LALAS*(I-1)
      L1=(I-1)*LAS2+MNULA2
      L1AP=(I-1)*LAS2
      IF(K.EQ.2.AND.I.EQ.IK) GO TO 14
      R=STEP1*(I-KL)
      GO TO 15
   14 R=0.D0
 
C     NON-DIAGONAL TERMS BY ISOSPIN

   15 CONTINUE
      
C      write(21,9999)vr,vd,vrdc,wc,wd,ww, 7.77777777
C 9999 format(11e12.5)
      IF(MEPOT.EQ.1) GO TO 17 
      PZ=0.D0
   21 NT0=L1+1
      NT1=L1+2
      NT2=L1+3
      NT3=L1+4
      NT4=L1+5
      X=(R-RR)/AR
      IF(X.GT.23.) GO TO 30
      EX=EXP(X)
      EX1=1.D0+EX
      EXD=1.D0/EX1
      EXDM=1.D0-EXD
      IF(JO(NU1).EQ.JO(NU2)) PV(NT0)=-VR*EXD*WW
      IF(LAS.EQ.0) GO TO 31
      PV(NT1)=(-VR*EXD*WW-PZ)*EXDM*RAR
      IF(LAS.EQ.1) GO TO 31
      Y1=1.D0-2.D0*EXD
      PV(NT2)=PV(NT1)*Y1*RAR/2.D0
      IF(LAS.EQ.2) GO TO 31
      Y2=1.D0-6.D0*EXD*EXDM
      PV(NT3)=PV(NT2)/Y1*Y2*RAR/3.D0
      IF(LAS.EQ.3) GO TO 31
      Y3=1.D0-EXD*(14.D0-EXD*(36.D0-24.D0*EXD))
      PV(NT4)=PV(NT3)/Y2*Y3*RAR/4.D0
      GO TO 31
   30 EX1=EXP(-X)
      IF(JO(NU1).EQ.JO(NU2)) PV(NT0)=-VR*EX1*WW
      IF(LAS.EQ.0) GO TO 31
      PV(NT1)=(-VR*EX1*WW-PZ)*RAR
      IF(LAS.EQ.1) GO TO 31
      PV(NT2)=PV(NT1)*RAR/2.D0
      IF(LAS.EQ.2) GO TO 31
      PV(NT3)=PV(NT2)*RAR/3.D0
      IF(LAS.EQ.3) GO TO 31
      PV(NT4)=PV(NT3)*RAR/4.D0
   31 X=(R-RC)/AC
      Y=(R-RW)/AW
      XX=Y*Y
      IF(XX.GT.180.) EXX=0.
      IF(XX.LE.180.) EXX=EXP(-XX)
      IF(X.GT.23.) GO TO 32
      EX=EXP(X)
      EX1=1.D0+EX
      EXD=1.D0/EX1
      EXDM=1.D0-EXD
      WCC=-WC*ALF*EXD*WW
      WWW=-WC*(1.-ALF)*EXX*WW
      IF(MEREL.EQ.3) WCC=WCC/RELPOT
      IF(MEREL.EQ.3) WWW=WWW/RELPOT
      IF(JO(NU1).EQ.JO(NU2)) PW(NT0)=WCC+WWW
      IF(JO(NU1).EQ.JO(NU2).AND.WC.NE.0.D0) PV(NT0)=PV(NT0)+WCC*VRDC/WC
      IF(LAS.EQ.0) GO TO 33
      WCC=WCC*EXDM*RAC
      WWW=WWW*2.D0*RAW*Y
      PW(NT1)=WCC+WWW
      IF(WC.NE.0.D0) PV(NT1)=PV(NT1)+WCC*VRDC/WC
      IF(LAS.EQ.1) GO TO 33
      YC1=1.D0-2.D0*EXD
      WCC=WCC*YC1*RAC/2.D0
      Y1=2.D0*Y*Y-1.D0
      WWW=WWW/Y*Y1*RAW/2.D0
      PW(NT2)=WCC+WWW
      IF(WC.NE.0.D0) PV(NT2)=PV(NT2)+WCC*VRDC/WC
      IF(LAS.EQ.2) GO TO 33
      YC2=1.D0-6.D0*EXD*EXDM
      WCC=WCC/YC1*YC2*RAC/3.D0
      Y2=Y**3*2.D0-Y*3.D0
      WWW=WWW/Y1*Y2*RAW*2.D0/3.D0
      PW(NT3)=WCC+WWW
      IF(WC.NE.0.D0) PV(NT3)=PV(NT3)+WCC*VRDC/WC
      IF(LAS.EQ.3) GO TO 33
      YC3=1.D0-EXD*(14.D0-EXD*(36.D0-24.D0*EXD))
      WCC=WCC/YC2*YC3*RAC/4.
      WWW=WWW/Y2*(Y**4*4.-Y*Y*12.D0+3.D0)*RAW/4.D0
      PW(NT4)=WCC+WWW
      IF(WC.NE.0.D0) PV(NT4)=PV(NT4)+WCC*VRDC/WC
   32 EX1=EXP(-X)
      WCC=-WC*ALF*EX1*WW
      WWW=-WC*(1.D0-ALF)*EXX*WW
      IF(MEREL.EQ.3) WCC=WCC/RELPOT
      IF(MEREL.EQ.3) WWW=WWW/RELPOT
      IF(JO(NU1).EQ.JO(NU2)) PW(NT0)=WCC+WWW
      IF(JO(NU1).EQ.JO(NU2).AND.WC.NE.0.D0) PV(NT0)=PV(NT0)+WCC*VRDC/WC
      IF(LAS.EQ.0) GO TO 33
      WCC=WCC*RAC
      WWW=WWW*2.D0*RAW*Y
      PW(NT1)=WCC+WWW
      IF(WC.NE.0.D0) PV(NT1)=PV(NT1)+WCC*VRDC/WC
      IF(LAS.EQ.1) GO TO 33
      WCC=WCC*RAC/2.D0
      Y1=2.D0*Y*Y-1.D0
      WWW=WWW/Y*Y1*RAW/2.D0
      PW(NT2)=WCC+WWW
      IF(WC.NE.0.D0) PV(NT2)=PV(NT2)+WCC*VRDC/WC
      IF(LAS.EQ.2) GO TO 33
      WCC=WCC*RAC/3.
      Y2=Y**3*2.D0-Y*3.D0
      WWW=WWW/Y1*Y2*RAW*2.D0/3.D0
      PW(NT3)=WCC+WWW
      IF(WC.NE.0.D0) PV(NT3)=PV(NT3)+WCC*VRDC/WC
      IF(LAS.EQ.3) GO TO 33
      WCC=WCC*RAC/4.D0
      WWW=WWW/Y2*(Y**4*4.D0-Y*Y*12.D0+3.D0)*RAW/4.D0
      PW(NT4)=WCC+WWW
      IF(WC.NE.0.D0) PV(NT4)=PV(NT4)+WCC*VRDC/WC
   33 X=(R-RD)/AD
      IF(X.GT.23.) GO TO 34
      EX=EXP(X)
      EX1=1.D0+EX
      EXD=1.D0/EX1
      EXDM=1.D0-EXD
      WDD=-4.D0*WD*EXDM*EXD*WW
      IF(MEREL.EQ.3) WDD=WDD/RELPOT
      IF(JO(NU1).EQ.JO(NU2)) PW(NT0)=PW(NT0)+WDD
      IF(JO(NU1).EQ.JO(NU2).AND.WD.NE.0.D0) PV(NT0)=PV(NT0)+WDD/WD*VD
      IF(LAS.EQ.0) GO TO 35
      Y1=1.D0-2.D0*EXD
      WDD=WDD*Y1*RAD
      PW(NT1)=PW(NT1)+WDD
      IF(WD.NE.0.D0) PV(NT1)=PV(NT1)+WDD/WD*VD
      IF(LAS.EQ.1) GO TO 35
      Y2=1.D0-6.D0*EXD*EXDM
      WDD=WDD/Y1*Y2*RAD/2.D0
      PW(NT2)=PW(NT2)+WDD
      IF(WD.NE.0.D0) PV(NT2)=PV(NT2)+WDD/WD*VD
      IF(LAS.EQ.2) GO TO 35
      Y3=1.D0-EXD*(14.D0-EXD*(36.D0-24.D0*EXD))
      WDD=WDD/Y2*Y3*RAD/3.D0
      PW(NT3)=PW(NT3)+WDD
      IF(WD.NE.0.D0) PV(NT3)=PV(NT3)+WDD/WD*VD
      IF(LAS.EQ.3) GO TO 35
      Y4=1.-EXD*(30.D0-EXD*(150.D0-EXD*(240.D0-120.D0*EXD)))
      WDD=WDD/Y3*Y4*RAD/4.D0
      PW(NT4)=PW(NT4)+WDD
      IF(WD.NE.0.D0) PV(NT4)=PV(NT4)+WDD/WD*VD
      GO TO 35
   34 EX1=EXP(-X)
      WDD=-4.D0*WD*EX1*WW
      IF(MEREL.EQ.3) WDD=WDD/RELPOT
      IF(JO(NU1).EQ.JO(NU2)) PW(NT0)=PW(NT0)+WDD
      IF(JO(NU1).EQ.JO(NU2).AND.WD.NE.0.D0) PV(NT0)=PV(NT0)+WDD/WD*VD
      IF(LAS.EQ.0) GO TO 35
      WDD=WDD*RAD
      PW(NT1)=PW(NT1)+WDD
      IF(WD.NE.0.D0) PV(NT1)=PV(NT1)+WDD/WD*VD
      IF(LAS.EQ.1) GO TO 35
      WDD=WDD*RAD/2.D0
      PW(NT2)=PW(NT2)+WDD
      IF(WD.NE.0.D0) PV(NT2)=PV(NT2)+WDD/WD*VD
      IF(LAS.EQ.2) GO TO 35
      WDD=WDD*RAD/3.D0
      PW(NT3)=PW(NT3)+WDD
      IF(WD.NE.0.D0) PV(NT2)=PV(NT2)+WDD/WD*VD
      IF(LAS.EQ.3) GO TO 35
      WDD=WDD*RAD/4.D0
      PW(NT4)=PW(NT4)+WDD
      IF(WD.NE.0.D0) PV(NT4)=PV(NT4)+WDD/WD*VD
   35 IF(I/KIT*KIT.NE.I) GO TO 6
      PR(I1+1)=PV(NT0)
      PI(I1+1)=PW(NT0)

C      print 876, i1,nt0,Nu1,Nu2, JO(NU1),JO(NU2),PR(I1+1),PI(I1+1)
C  876 FORMAT (6i7,6e12.3)
C      pause 22

      IF(LAS.EQ.0) GO TO 6
      PR(I1+2)=PV(NT1)
      PI(I1+2)=PW(NT1)
      IF(LAS.EQ.1) GO TO 6
      PR(I1+3)=PV(NT2)
      PI(I1+3)=PW(NT2)
      IF(LAS.EQ.2) GO TO 6
      PR(I1+4)=PV(NT3)
      PI(I1+4)=PW(NT3)
      IF(LAS.EQ.3) GO TO 6
      PR(I1+5)=PV(NT4)
      PI(I1+5)=PW(NT4)
      GO TO 6



      
   17 DO 99 L=1,LAS2


      
      
      PDC=0.D0
      PZ=0.D0
      LL2=L1+L
      LL2AP=L1AP+L
      
      DE=DEF(1)

      
      CALL POTET
      
      VV=VP*POL(L,1)
      WV=WP*POL(L,1)
      N=NAN2
      DE=DEF(N)
      CALL POTET
      VV=VV+VP*POL(L,N)
      WV=WV+WP*POL(L,N)
      V4=0.
      W4=0.
      DO 10 N=2,NAN2,2
      DE=DEF(N)
      CALL POTET
      V4=V4+VP*POL(L,N)
   10 W4=W4+WP*POL(L,N)
      V2=0.
      W2=0.
      DO 11 N=3,NAN1,2
      DE=DEF(N)
      CALL POTET
      V2=V2+VP*POL(L,N)
   11 W2=W2+WP*POL(L,N)

      
      IF(JO(NU1).EQ.JO(NU2).AND.L.EQ.1) GO TO 780
      IF(L.EQ.1) GO TO 99
      
        
      
C      GO TO 99                                           !!!!!!!CHECK!!!!!!!


  780 PV(LL2)=(VV+4.D0*V4+2.D0*V2)*ST*WW*4.1887902047864D0
      PW(LL2)=(WV+4.D0*W4+2.D0*W2)*ST*WW*4.1887903047864D0
      IF(MEREL.EQ.3) PW(LL2)=PW(LL2)/RELPOT

C      write(21,1234) i,l,r
C 1234 format(2i5,e12.5)
C     write(21,9999)vr,vd,vrdc,wc,wd,ww, 7.77777777
C      write(21,9999)v2,v4,vv, 7.77777777,w2,w4,wv, pv(LL2),pWW(LL2), 5.5

      IF(K.EQ.2.AND.I.EQ.IK) GO TO 114
      R=STEP1*(I-KL)
      GO TO 115
  114 R=0.D0
      IR=1

      GO TO 116

  115 IF(I/KIT*KIT.NE.I) GO TO 99
      IR=I/KIT+NIT
  116 I1=(IR-1)*LAS2+MNULA2
      II2=I1+L
      PR(II2)=PV(LL2)
      PI(II2)=PW(LL2)
      
            

   99 CONTINUE

    6 CONTINUE
      KL=M4-4
      IN=M4
      KIT=1
      NIT=5-M4
      IK=NH+KL+2
      STEP1=STEP
    5 CONTINUE
      NH1=NH+KL
      NHI=NH+2
   38 CONTINUE
      EN=EINT
      RR=RRR
      RZ=RZIN
      CCOUL=CCCOUL
      CONZ=CONZZ


C      write(21,1799) vr,vd,vrdc,wc,wd, enef,WDBWNP
C 1799 format(7e12.5)
C     pause 2

C     WRITE(21,9997) RK, STEP,CONZ ,2.2222222, NH1
C9997  FORMAT(4E12.5, I3)
      RETURN
      END
C     ***********************************************************
      SUBROUTINE CMATC
C     ***********************************************************
      IMPLICIT DOUBLE PRECISION(A-H,O-Z) 
      COMMON/JNN/CSS,INCC,NCLL,NSS,NJ,INCR
     */ENA/EN,EL(40),BET(10),NUR,NMAX,NPD,LAS
     */ECI/NECI,NHI
      COMMON/MENU/MEJOB,MEPOT,MEHAM,MECHA,MEPRI,MESOL,MESHA,MESHO,MEHAO
     *,MEAPP,MEVOL,MEREL,MECUL,MERZZ,MERRR,MEDIS,MERIP
      MSOLIN=MESOL
C      IF(NSS.LT.5) MESOL=2
      IF(MESOL.LE.3) GO TO 5
      NCLLT=NCLL
      INCRT=INCR
      IF(MESOL.LT.INCR) INCR=MESOL
      IF(MESOL.LT.NCLL) NCLL=MESOL
      CALL KNDIT
      CALL SOSIT
      CALL MASCT
      NCLL=NCLLT
      INCR=INCRT  
      IF(MESOL.GE.NCLLT .AND. MEPRI.NE.99) PRINT 22,NSS,NCLL
      IF(MESOL.GE.NCLLT) GO TO 4
      CALL KNDIT
      GO TO 3
    5 CALL KNDIT
      IF(MESOL-2) 1,2,3
    1 IF(NSS.LT.5) GO TO 2
      IEC=INCC*(BET(2)*40.+3)
      IF(NCLL.GT.IEC) GO TO 3
    2 CALL SOSIT
      MESOL=2
      CALL MASCT
      IF(MEPRI.NE.99) PRINT 22,NSS,NCLL
  22  FORMAT (2I5,E20.7)
      GO TO 4
    3 CALL ECISS
      IF(NECI.EQ.0) GO TO 2
    4 MESOL=MSOLIN
      RETURN
      END
C*******************************************************************************
      SUBROUTINE RCWF(RHO,ETA,MINL,MAXL,FC,FCP,GC,GCP,ACCUR,STEP,NUMBR) 
      IMPLICIT DOUBLE PRECISION(A-H,O-Z) 
      DOUBLE PRECISION K,K1,K2,K3,K4,M1,M2,M3,M4
      DIMENSION FC(1),FCP(1),GC(1),GCP(1)                               
C *** COULOMB WAVEFUNCTIONS CALCULATED AT R = RHO BY THE                
C *** CONTINUED-FRACTION METHOD OF STEED   MINL,MAXL ARE ACTUAL L-VALUES
C *** SEE BARNETT FENG STEED AND GOLDFARB COMPUTER PHYSICS COMMUN 1974  
C *** RCWF            - A MODIFICATION OF THE REAL COULOMB              
C *** WAVEFUNCTION PROGRAM RCWFN.  A.R. BARNETT.                        
C *** REF. IN COMP. PHYS. COMMUN. 11 (1976) 141                         
C  INSERT AFTER CARD 48 THE NEXT SIX CARDS                              
C *** CPC 8 (1974) 377-395                                              
C *** IF NUMBR = 1 FC(MAXL) TO FC(MINL) ARE RETURNED  ARB MARCH 1975    
C *** IF NUMBR = 2 FC AND GC ARRAYS ARE RETURNED                        
C *** IF NUMBR = 3 FC,GC,FCP,GCP ARRAYS ARE RETURNED                    
      NUM = 1                                                           
      IF(NUMBR.GE.1.AND.NUMBR.LE.3) NUM = NUMBR                         
      PACE = STEP                                                       
      ACC  = ACCUR                                                      
      IF(PACE.LT.100.0) PACE = 100.0                                    
      IF(ACC.LT.1.0E-15.OR.ACC.GT.1.0E-6) ACC = 1.0E-6                  
      R    = RHO                                                        
      KTR  = 1                                                          
      LMAX = MAXL                                                       
      LMIN1= MINL + 1                                                   
      XLL1 = FLOAT(MINL*LMIN1)                                          
      ETA2 = ETA*ETA                                                    
      TURN = ETA + SQRT(ETA2 + XLL1)                                    
      IF(R.LT.TURN.AND.ABS(ETA).GE.1.0E-6) KTR = -1                     
      KTRP = KTR                                                        
      GO TO 2                                                           
1     R    = TURN                                                       
      TF   = F                                                          
      TFP  = FP                                                         
      LMAX = MINL                                                       
      KTRP = 1                                                          
2     ETAR = ETA*R                                                      
      RHO2 =   R*R                                                      
      PL   = FLOAT(LMAX + 1)                                            
      PMX  = PL + 0.5                                                   
C *** CONTINUED FRACTION FOR FP(MAXL)/F(MAXL)  XL IS F  XLPRIME IS FP **
      FP  = ETA/PL + PL/R                                               
      DK  = ETAR*2.0                                                    
      DEL = 0.0                                                         
      D   = 0.0                                                         
      F   = 1.0                                                         
      K   = (PL*PL - PL + ETAR)*(2.0*PL - 1.0)                          
      IF(PL*PL+PL+ETAR.NE.0.0) GO TO 3                                  
      R   = R + 1.0E-6                                                  
      GO TO 2                                                           
3     H   = (PL*PL + ETA2)*(1.0 - PL*PL)*RHO2                           
      K   = K + DK + PL*PL*6.0                                          
      D   =  1.0/(D*H + K)                                              
      DEL =  DEL*(D*K - 1.0)                                            
      IF(PL.LT.PMX) DEL = -R*(PL*PL + ETA2)*(PL + 1.0)*D/PL             
      PL  = PL + 1.0                                                    
      FP  = FP + DEL                                                    
      IF(D.LT.0.0) F = -F                                               
      IF(PL.GT.20000.) GO TO 11                                         
CC      IF(ABS(DEL/FP).GE.ACC) GO TO 3                                 
C  REPLACE CARD 91 BY THE NEXT CARD                                     
      IF(ABS(DEL).GE.ABS(FP)*ACC) GO TO 3                               
      FP  = F*FP                                                        
      IF( LMAX.EQ.MINL) GO TO 5                                        
      FC (LMAX+1) = F                                                   
CC      FCP(LMAX+1) = FP                                              
C  REPLACE CARD 95 BY NEXT TWO CARDS                                   
      FPL          = FP                                                 
      IF(NUM.EQ.3) FCP(LMAX+1) = FP                                     
C *** DOWNWARD RECURSION TO MINL FOR F AND FP, ARRAYS GC,GCP ARE STORAGE
      L  = LMAX                                                         
C  INSERT NEXT CARD AFTER 97                                            
      RI = 1.0/R                                                        
      DO 4 LP  = LMIN1,LMAX                                             
      PL = FLOAT(L)                                                     
CC      GC (L+1) = ETA/PL + PL/R                                       
CC      GCP(L+1) = SQRT(ETA2 + PL*PL)/PL                               
CC      FC (L)   = (GC(L+1)*FC(L+1) + FCP(L+1))/GCP(L+1)              
CC      FCP(L)   =  GC(L+1)*FC(L)   - GCP(L+1)*FC(L+1)                 
C  REPLACE CARDS 100 - 103 BY NEXT 10 CARDS                            
      SL = ETA/PL + PL*RI                                              
      RL = SQRT(1.D0 + ETA2/(PL*PL))                                   
      FC(L) = (SL*FC(L+1) + FPL)/RL                                  
      FPL1  =  SL*FC(L) - RL*FC(L+1)                                 
      FPL   = FPL1                                                  
      IF(NUM.EQ.1) GO TO 4                                          
      GC (L+1) = RL                                                 
      IF(NUM.LE.2) GO TO 4                                           
      GCP(L+1) = SL                                                  
      FCP(L)   = FPL1                                               
4     L  = L - 1                                                    
      F  = FC (LMIN1)                                                
CC      FP = FCP(LMIN1)                                             
C  REPLACE CARD 106 BY THE NEXT CARD                                 
      FP = FPL1                                                       
5     IF(KTRP.EQ.-1) GO TO 1                                         
C *** REPEAT FOR R = TURN IF RHO LT TURN                              
C *** NOW OBTAIN P + I.Q FOR MINL FROM CONTINUED FRACTION (32)         
C *** REAL ARITHMETIC TO FACILITATE CONVERSION TO IBM USING DOUBLE PRECISION
      P  = 0.d0                                                          
      Q  = R - ETA                                                      
      PL = 0.d0                                                          
      AR = -(ETA2 + XLL1)                                               
      AI =   ETA                                                        
      BR = 2.d0*Q                                                       
      BI = 2.d0                                                          
      WI = 2.d0*ETA                                                      
      DR =   BR/(BR*BR + BI*BI)                                         
      DI =  -BI/(BR*BR + BI*BI)                                        
      DP = -(AR*DI + AI*DR)                                            
      DQ =  (AR*DR - AI*DI)                                            
6     P  =  P + DP                                                     
      Q  =  Q + DQ                                                     
      PL = PL + 2.d0                                                     
      AR = AR + PL                                                     
      AI = AI + WI                                                     
      BI = BI + 2.d0                                                    
      D  = AR*DR - AI*DI + BR                                           
      DI = AI*DR + AR*DI + BI                                           
      T  = 1.d0/(D*D + DI*DI)                                           
      DR =  T*D                                                        
      DI = -T*DI                                                        
      H  = BR*DR - BI*DI - 1.d0                                          
      K  = BI*DR + BR*DI                                               
      T  = DP*H  - DQ*K                                                 
      DQ = DP*K  + DQ*H                                                
      DP = T                                                           
      IF(PL.GT.46000.d0) GO TO 11                                         
      IF(ABS(DP)+ABS(DQ).GE.(ABS(P)+ABS(Q))*ACC) GO TO 6                
      P  = P/R                                                          
      Q  = Q/R                                                          
C *** SOLVE FOR FP,G,GP AND NORMALISE F  AT L=MINL                      
      G  = (FP - P*F)/Q                                                
      GP = P*G - Q*F                                                   
      W  = 1.D0/SQRT(FP*G - F*GP)                                       
      G  = W*G                                                          
      GP = W*GP                                                        
      IF(KTR.EQ.1) GO TO 8                                             
      F  = TF                                                          
      FP = TFP                                                          
      LMAX = MAXL                                                       
C *** RUNGE-KUTTA INTEGRATION OF G(MINL) AND GP(MINL) INWARDS FROM TURN 
C ***             SEE FOX AND MAYERS 1968 PG 202                        
      IF(RHO.LT.0.2*TURN) PACE = 999.d0                                  
      R3 = 1.d0/3.0D0                                                    
      H  = (RHO - TURN)/(PACE + 1.d0)                                    
CC      H2 = 0.5*H                                                      
C  REPLACE CARD 158 BY THE NEXT CARD                                   
      H2 = 0.5D0*H                                                      
C      I2 = IFIX(PACE + 0.001)                                          
      I2 = PACE + 0.001d0                                                 
      ETAH = ETA*H                                                      
      H2LL = H2*XLL1                                                    
      S  = (ETAH + H2LL/R  )/R   - H2                                   
7     RH2= R + H2                                                       
      T  = (ETAH + H2LL/RH2)/RH2 - H2                                   
      K1 = H2*GP                                                        
      M1 =  S*G                                                         
      K2 = H2*(GP + M1)                                                 
      M2 =  T*(G  + K1)                                                 
      K3 =  H*(GP + M2)                                                 
      M3 =  T*(G  + K2)                                                 
      M3 =     M3 + M3                                                  
      K4 = H2*(GP + M3)                                                 
      RH = R + H                                                        
      S  = (ETAH + H2LL/RH )/RH  - H2                                   
      M4 =  S*(G + K3)                                                  
      G  = G  + (K1 + K2 + K2 + K3 + K4)*R3                             
      GP = GP + (M1 + M2 + M2 + M3 + M4)*R3                             
      R  = RH                                                           
      I2 = I2 - 1                                                       
      IF(ABS(GP).GT.1.0D300) GO TO 11                                   
      IF(I2.GE.0) GO TO 7                                               
      W  = 1.d0/(FP*G - F*GP)                                            
C *** UPWARD RECURSION FROM GC(MINL) AND GCP(MINL),STORED VALUES ARE R,S
C *** RENORMALISE FC,FCP FOR EACH L-VALUE                               
CC8     GC (LMIN1) = G                                                
CC      GCP(LMIN1) = GP                                                
CC      IF(LMAX.EQ.MINL) GO TO 10                                       
C  REPLACE CARDS 185 - 187 BY THE NEXT 6 CARDS                         
CC      IF(NUM.GE.2) GC (LMIN1) = G                                    
C  REPLACE CARD A056 BY                                                
8     IF(NUM.GE.2) GC (LMIN1) = G                                      
      IF(NUM.EQ.3) GCP(LMIN1) = GP                                    
      IF(NUM.EQ.3) FCP(LMIN1) = FP*W                                  
      FC(LMIN1) = F*W                                                  
      IF(LMAX.EQ.MINL) RETURN                                          
      GPL = GP                                                        
      DO  9  L = LMIN1,LMAX                                            
CC      T        = GC(L+1)                                             
CC      GC (L+1) = (GC(L)*GC (L+1) - GCP(L))/GCP(L+1)                  
CC      GCP(L+1) =  GC(L)*GCP(L+1) - GC(L+1)*T                        
CC      FC (L+1) = W*FC (L+1)                                          
CC9     FCP(L+1) = W*FCP(L+1)                                         
C  REPLACE CARDS 189 - 193 BY THE NEXT 11 CARDS                        
      IF(NUM.EQ.1) GO TO 9                                             
      IF(NUM.EQ.2) SL = ETA/FLOAT(L) + FLOAT(L)*RI                     
      IF(NUM.EQ.3) SL = GCP(L+1)                                      
      RL = GC(L+1)                                                  
      GC(L+1) = (SL*GC(L) - GPL)/RL                                  
      GPL1    =  RL*GC(L) - SL*GC(L+1)                                
      GPL     =  GPL1                                                 
      IF(NUM.LT.3) GO TO 9                                           
      GCP(L+1) = GPL1                                                 
      FCP(L+1) = FCP(L+1)*W                                           
9     FC (L+1) = FC(L+1)*W                                            
CC      FC (LMIN1) = FC (LMIN1)*W                                       
CC      FCP(LMIN1) = FCP(LMIN1)*W                                      
CC      RETURN                                                        
CC10    FC (LMIN1) = W*F                                             
CC      FCP(LMIN1) = W*FP                                              
      RETURN                                                           
11    W  = 0.d0                                                         
      G  = 0.d0                                                         
      GP = 0.d0                                                         
      GO TO 8                                                          
      END                                                             
C     ******************************************************************
      SUBROUTINE RCWFN(RHS,ETS,MINL,MAXL,FC,FCP,GC,GCP,ACCUR,STEPC)
C     ******************************************************************
      IMPLICIT DOUBLE PRECISION(A-H,O-Z) 
      DOUBLE PRECISION K,K1,K2,K3,K4,M1,M2,M3,M4
      DIMENSION FC(1),FCP(1),GC(1),GCP(1)
      DATA GPMAX,ONE,HUNDR,C1,C2,HALF,TWO,ZER,SIX,TWOUS,FSOUS,
     *OTWO,THRN,OONE/1.0D+60,1.0D+00,0.1D+02,1.0D-12,1.0D-06,
     *0.5D+00,0.2D+01,0.0D+00,0.6D+01,0.2D+05,0.46D+05,0.2D+00,
     *0.999D+03,0.1D-02/
      PAGE=STEPC
      ACC=ACCUR
      RHO=RHS
      ETA=ETS
      IF(PAGE.LT.HUNDR) PAGE=HUNDR
      IF(ACC.LT.C1.OR.ACC.GT.C2) ACC=C2
      R=RHO
      KTR=1
      LMAX=MAXL
      LMIN1=MINL+1
      XLL1=DFLOAT(MINL*LMIN1)
      ETA2=ETA*ETA
      TURN=ETA+SQRT(ETA2+XLL1)
      IF(R.LT.TURN.AND.ABS(ETA).GE.C2) KTR=-1
      KTRP=KTR
      GO TO 2
    1 R=TURN
      TF=F
      TFP=FP
      LMAX=MINL
      KTRP=1
    2 ETAR=ETA*R
      RHO2=R*R
      PL=DFLOAT(LMAX+1)
      PMX=PL+HALF
      FP=ETA/PL+PL/R
      DK=ETAR*TWO
      DEL=ZER
      D=ZER
      F=ONE
      K=(PL*PL-PL+ETAR)*(TWO*PL-ONE)
      IF((PL*PL+PL+ETAR).NE.ZER) GO TO 3
      R=R+C2
      GO TO 2
    3 H=(PL*PL+ETA2)*(ONE-PL*PL)*RHO2
      K=K+DK+PL*PL*SIX
      D=ONE/(D*H+K)
      DEL=DEL*(D*K-ONE)
      IF(PL.LT.PMX) DEL=-R*(PL*PL+ETA2)*(PL+ONE)*D/PL
      PL=PL+ONE
      FP=FP+DEL
      IF(D.LT.ZER) F=-F
      IF(PL.GT.TWOUS) GO TO 11
      IF(ABS(DEL/FP).GE.ACC) GO TO 3
      FP=F*FP
      IF(LMAX.EQ.MINL) GO TO 5
      FC(LMAX+1)=F
      FCP(LMAX+1)=FP
      L=LMAX
      DO 4 LP=LMIN1,LMAX
      PL=DFLOAT(L)
      GC(L+1)=ETA/PL+PL/R
      GCP(L+1)=SQRT(ETA2+PL*PL)/PL
      FC(L)=(GC(L+1)*FC(L+1)+FCP(L+1))/GCP(L+1)
      FCP(L)=GC(L+1)*FC(L)-GCP(L+1)*FC(L+1)
    4 L=L-1
      F=FC(LMIN1)
      FP=FCP(LMIN1)
    5 IF(KTRP.EQ.-1) GO TO 1
      P=ZER
      Q=R-ETA
      PL=ZER
      AR=-(ETA2+XLL1)
      AI=ETA
      BR=TWO*Q
      BI=TWO
      WI=TWO*ETA
      DR=BR/(BR*BR+BI*BI)
      DI=-BI/(BR*BR+BI*BI)
      DP=-(AR*DI+AI*DR)
      DQ=AR*DR-AI*DI
    6 P=P+DP
      Q=Q+DQ
      PL=PL+TWO
      AR=AR+PL
      AI=AI+WI
      BI=BI+TWO
      D=AR*DR-AI*DI+BR
      DI=AI*DR+AR*DI+BI
      T=ONE/(D*D+DI*DI)
      DR=D*T
      DI=-T*DI
      H=BR*DR-BI*DI-ONE
      K=BI*DR+BR*DI
      T=DP*H-DQ*K
      DQ=DP*K+DQ*H
      DP=T
      IF(PL.GT.FSOUS) GO TO 11
      CNT=(ABS(DP)+ABS(DQ))-((ABS(P)+ABS(Q))*ACC)
      IF(CNT) 66,66,6
   66 P=P/R
      Q=Q/R
      G=(FP-P*F)/Q
      GP=P*G-Q*F
      W=ONE/SQRT(FP*G-F*GP)
      G=W*G
      GP=W*GP
      IF(KTR.EQ.1) GO TO 8
      F=TF
      FP=TFP
      LMAX=MAXL
      IF(RHO.LT.(OTWO*TURN)) PAGE=THRN
      R3=0.333333333333333333333D+00
      H=(RHO-TURN)/(PAGE+ONE)
      H2=HALF*H
      I2=PAGE+OONE
      ETAH=ETA*H
      H2LL=H2*XLL1
      S=(ETAH+H2LL/R)/R-H2
    7 RH2=R+H2
      T=(ETAH+H2LL/RH2)/RH2-H2
      K1=H2*GP
      M1=S*G
      K2=H2*(GP+M1)
      M2=T*(G+K1)
      K3=H*(GP+M2)
      M3=T*(G+K2)
      M3=M3+M3
      K4=H2*(GP+M3)
      RH=R+H
      S=(ETAH+H2LL/RH)/RH-H2
      M4=S*(G+K3)
      G=G+(K1+K2+K2+K3+K4)*R3
      GP=GP+(M1+M2+M2+M3+M4)*R3
      R=RH
      I2=I2-1
      GPG=GP
      IF(ABS(GPG).GT.GPMAX) GO TO 11
      IF(I2.GE.0) GO TO 7
      W=ONE/(FP*G-F*GP)
    8 GC(LMIN1)=G
      GCP(LMIN1)=GP
      IF(LMAX.EQ.MINL) GO TO 10
      DO 9 L=LMIN1,LMAX
      T=GC(L+1)
      GC(L+1)=(GC(L)*GC(L+1)-GCP(L))/GCP(L+1)
      GCP(L+1)=GC(L)*GCP(L+1)-GC(L+1)*T
      FC(L+1)=W*FC(L+1)
    9 FCP(L+1)=W*FCP(L+1)
      FCP(LMIN1)=FCP(LMIN1)*W
      FC(LMIN1)=FC(LMIN1)*W
      GO  TO 12
   10 FC(LMIN1)=W*F
      FCP(LMIN1)=W*FP
      GO TO 12
   11 W=ZER
      G=ZER
      GP=ZER
      GO TO 8
   12 RETURN
      END
C     ***********************************************************
      SUBROUTINE LOGMO
C     ***********************************************************
      COMMON/LNP/JSS,NPIS,NPIO,JSO,NN1,LNO(180),NS1(180),JNO(180),NSPI
      N=0
      KJ1=IABS(JSO-JSS)+2
      KJ2=JSO+JSS+2
      DO 1 JO2=KJ1,KJ2,2
      JO=JO2-2
      KL1=IABS(JO-NSPI)+2
      KL2=JO+NSPI+2
      DO 2 LO2=KL1,KL2,2
      LO=LO2-2
      IF(NPIO*NPIS*(-1)**(LO/2).LE.0) GO TO 2
      N=N+1
      LNO(N)=LO/2
      JNO(N)=JO
      NS1(N)=JO-LO
    2 CONTINUE
    1 CONTINUE
      NN1=N
      N1M=NN1-1
      IF(NN1.LE.1) GO TO 5
      DO 3 IM=1,N1M
      LM=LNO(IM)
      IN=IM+1
      DO 4 IL=IN,NN1
      LA=LNO(IL)
      IF(LM.LE.LA) GO TO 4
      JA=JNO(IL)
      NS1A=NS1(IL)
      JNO(IL)=JNO(IM)
      LNO(IL)=LM
      NS1(IL)=NS1(IM)
      JNO(IM)=JA
      LNO(IM)=LA
      NS1(IM)=NS1A
      LM=LA
    4 CONTINUE
    3 CONTINUE
    5 CONTINUE
      RETURN
      END
C     *****************************************************************
      SUBROUTINE KLEGO1
C     *****************************************************************
      IMPLICIT DOUBLE PRECISION(A-H,O-Z) 
      COMMON/LOFAC/A(800)
      COMMON/KG/J1,J2,M1,M2,J,M,AKG
      IF(M1+M2-M)1,2,1
    1 AKG=0.
      GO TO 14
    2 IF(IABS(J1-J2)-J)3,3,1
    3 IF(J1+J2-J)1,4,4
    4 NF=J1+J2-J
      IF(NF/2*2.NE.NF) GO TO 1
      K=J1+J2+J
      IF(M1.EQ.0.AND.M.EQ.0.AND.K/4*4.NE.K) GO TO 1
      FLN=A(NF+2)
      CL=FLN
      NF=J1-J2+J
      FLN=A(NF+2)
      CL=CL+FLN
      NF=J+J2-J1
      FLN=A(NF+2)
      CL=CL+FLN
      NF=J1+J2+J+2
      FLN=A(NF+2)
      CL=CL-FLN
      NF=J1+M1
      FLN=A(NF+2)
      CL=CL+FLN
      NF=J1-M1
      FLN=A(NF+2)
      CL=CL+FLN
      NF=J2+M2
      FLN=A(NF+2)
      CL=CL+FLN
      NF=J2-M2
      FLN=A(NF+2)
      CL=CL+FLN
      NF=J+M
      FLN=A(NF+2)
      CL=CL+FLN
      NF=J-M
      FLN=A(NF+2)
      CL=0.5*(CL+FLN)
      NF1=J1+J2-J
      NF2=J1-M1
      NF3=J2+M2
      NF4=J-J2+M1
      NF5=J-J1-M2
      NB=NF1
      NM=-NF4
      IF(NF2-NB)5,6,6
    5 NB=NF2
    6 IF(NF3-NB)7,8,8
    7 NB=NF3
    8 IF(-NF5.GT.NM) NM=-NF5
      IF(NM.LT.0) NM=0
      NM=NM+2
      NB=NB+2
      AKG=0.
      IF(NB.LT.NM) GO TO 14
      DO 13 I1=NM,NB,2
      C1=1.
      N=I1-2
      NF=N
      FLN=A(NF+2)
      CL1=CL-FLN
      NF=NF1-N
      FLN=A(NF+2)
      CL1=CL1-FLN
      NF=NF2-N
      FLN=A(NF+2)
      CL1=CL1-FLN
      NF=NF3-N
      FLN=A(NF+2)
      CL1=CL1-FLN
      NF=NF4+N
      FLN=A(NF+2)
      CL1=CL1-FLN
      NF=NF5+N
      FLN=A(NF+2)
      CL1=CL1-FLN
      IF(N/4*4.NE.N) C1=-1.D0
   13 AKG=AKG+C1*EXP(CL1)
      AKG=AKG*SQRT(J+1.D0)
   14 RETURN
      END
C     *******************************************************
      SUBROUTINE COPHA
C     *******************************************************
      IMPLICIT DOUBLE PRECISION(A-H,O-Z) 
      COMMON/COUL/CONZ,ETA,COPH(40,90)
      DO 1 L1=1,1000
      ARG=ETA/L1
      ALF=ATAN(ARG)
      BET=SQRT(ETA**2+L1**2)
      CO=ALF*(L1-0.5)+ETA*(LOG(BET)-1.D0)-SIN(ALF)/12.D0/BET
      SUM7=SIN(9.D0*ALF)/1188.D0/BET**9
      ERROR=ABS(SUM7/CO)
      IF(ERROR.LT.10D-10) GO TO 2
    1 CONTINUE
      PRINT 24,ERROR
      WRITE(21,24)ERROR
   24 FORMAT(10X,'WARNING! ERROR IN COULOMB PHASE>',D12.5)
    2 LL1=L1
      COL=CO+SIN(3.D0*ALF)/360.D0/BET**3-SIN(5.D0*ALF)/1260.D0/BET**5
     *+SIN(7.D0*ALF)/1680.D0/BET**7-SUM7
      LL=LL1
      IF(LL.EQ.1) GO TO 5
    4 LL=LL-1
      COL=COL-ATAN(ETA/LL)
      IF(LL.GT.1) GO TO 4
    5 COPH(40,1)=COL
      RETURN
      END
C     *******************************************************
      SUBROUTINE SPHEPOT
C     ***********************************************************
      IMPLICIT DOUBLE PRECISION(A-H,O-Z) 
      COMMON/POTEB/R,DE,VP,WP
      COMMON/RAD/RR,RC,RD,RW,RS,AR,AC,AW,AD,AS,ALF,AT,ANEU,RZ,ZNUC,ASP
     *,AZ
      DE=RZ+20.*AZ
      RGR=DE
      IF(R.GE.RGR) DE=1.D0/R
      IF(R.GE.RGR) GO TO 22
      CALL SPHER
      ANO=DE
C     write(22,33) r,pp,de
C  33 FORMAT(6E12.4)
      DE=RZ+20.*AZ
      AN=DE/AZ+2.
      NAN1=AN*10
      NAN1=2*NAN1+2
      IF(NAN1.GT.96) NAN1=96
      NAN2=NAN1+1
      ST=DE/(NAN2-1)
      VV=0.D+00
      PDZ=1./DE**2
      CALL SPHER
      PDZ=PDZ*DE
    5 VV=VV+PDZ
      V4=0.
      DO 10 N=2,NAN2,2
      DE=ST*(N-1)
      PDZ=1./DE**2
      CALL SPHER
      PDZ=PDZ*DE
   10 V4=V4+PDZ
      V2=0.
      DO 11 N=3,NAN1,2
      DE=ST*(N-1)
      PDZ=1./DE**2
      CALL SPHER
      PDZ=PDZ*DE
   11 V2=V2+PDZ
      DE=(VV+4.*V4+2.*V2)*ST/3.
      PO0=DE/ANO+1./RGR
      DE=R
      IF(R.EQ.0.) PO1=0.
      IF(R.EQ.0.) GO TO 18
      AN=DE/AZ+2.
      NAN1=AN*5
      NAN1=2*NAN1+2
      IF(NAN1.GT.48) NAN1=48
      NAN2=NAN1+1
      ST=DE/(NAN2-1)
      VV=0.D+00
      PDZ=1./DE**2
      CALL SPHER
      PDZ=PDZ*DE
      VV=VV+PDZ
      V4=0.
      DO 12 N=2,NAN2,2
      DE=ST*(N-1)
      PDZ=1./DE**2
      CALL SPHER
      PDZ=PDZ*DE
   12 V4=V4+PDZ
      V2=0.
      DO 13 N=3,NAN1,2
      DE=ST*(N-1)
      PDZ=1./DE**2
      CALL SPHER
      PDZ=PDZ*DE
   13 V2=V2+PDZ
      PO1=(VV+4.*V4+2.*V2)*ST/3.
   18 DE=PO0-PO1/ANO
   22 CONTINUE
C     PRINT 33,R,PP, DE,PO0,PO1,ANO
      RETURN
      END
C     ***********************************************************
      SUBROUTINE SPHER
C     ***********************************************************
      IMPLICIT DOUBLE PRECISION(A-H,O-Z) 
      COMMON/POTEB/R,DE,VP,WP
      COMMON/RAD/RR,RC,RD,RW,RS,AR,AC,AW,AD,AS,ALF,AT,ANEU,RZ,ZNUC,ASP
     *,AZ
      IF(DE.EQ.0.) GO TO 15
      AN=DE/AZ+2.
      NAN1=AN*10
      NAN1=2*NAN1+2
      IF(NAN1.GT.96)NAN1=96
      NAN2=NAN1+1
      ST=DE/(NAN2-1)
C     PRINT 88,ST,NAN2
C     STOP
      VV=0.D+00
      X=(DE-RZ)/AZ
      IF(X.GT.23.) GO TO 4
      EX=EXP(X)
      EX1=1.D0+EX
      EXD=1.D0/EX1
      PDZ=EXD*DE**2
      GO TO 5
    4 PDZ=EXP(-X)*DE**2
    5 VV=VV+PDZ
      V4=0.
      DO 10 N=2,NAN2,2
      Y=ST*(N-1)
      X=(Y-RZ)/AZ
      IF(X.GT.23.) GO TO 6
      EX=EXP(X)
      EX1=1.D0+EX
      EXD=1.D0/EX1
      PDZ=EXD*Y**2
   88 FORMAT(4E12.3)
      GO TO 10
    6 PDZ=EXP(-X)*Y**2
   10 V4=V4+PDZ
      V2=0.
      DO 11 N=3,NAN1,2
      Y=ST*(N-1)
      X=(Y-RZ)/AZ
      IF(X.GT.23.) GO TO 7
      EX=EXP(X)
      EX1=1.D0+EX
      EXD=1.D0/EX1
      PDZ=EXD*Y**2
      GO TO 11
    7 PDZ=EXP(-X)*Y**2
   11 V2=V2+PDZ
      DE=(VV+4.D0*V4+2.D0*V2)*ST/3.D0
   15 RETURN
      END
C     ***********************************************************
      SUBROUTINE POTVOL
C     ***********************************************************
      IMPLICIT DOUBLE PRECISION(A-H,O-Z) 
      COMMON/RAD/RR,RC,RD,RW,RS,AR,AC,AW,AD,AS,ALF,AT,ANEU,RZ,ZNUC,ASP
     *,AZ
      COMMON/MENU/MEJOB,MEPOT,MEHAM,MECHA,MEPRI,MESOL,MESHA,MESHO,MEHAO
     *,MEAPP,MEVOL,MEREL,MECUL,MERZZ,MERRR,MEDIS,MERIP
     */POT1/VR3,VR2,VR1,VR0,WC1,WC0,WD1,WD0,VS,AC0,AR0,AW0,AD0,AS0
     */POT2/AR1,AC1,AW1,AD1,AS1
     */POT3/BNDC,WDA1,WCA1,CCOUL,CISO,WCISO,WS0,WS1
     */POTD/ALAVR,VRLA,WCBW,WCWID,WDBW,WDWID,ALAWD,EFERMN,EFERMP,ALASO
     *,PDIS,WSBW,WSWID,RRBWC,RRWID,RZBWC,RZWID
     */ENA/EN,EL(40),BET(10),NUR,NMAX,NPD,LAS
     */CVOL/CBET0
C     CALCULATES INTEGRALS OF FORM FACTOR, IT'S FIRST DERIVATIVE
C
C     MULTIPLIED BY (-1)*(Ri/Ai) AND IT'S SECOND DERIVATIVE
C     MULTIPLIED BY (1/2)*(Ri/Ai)**2
C     INTEGRALS BY RADIUS FROM ZERO TO INFINITY. TO GET VOLUME
C     INTEGRALS MULTIPLY BY 4*PI
      CBET0=1.
      IS=2
      RRR=RR
       IF(MERRR.EQ.1)   RR=RR*(1.D00-RRBWC*EN**PDIS/(EN**2+RRWID**PDIS))
      AR=AR0+AR1*EN
      AW=AW0+AW1*EN
      AC=AC0+AC1*EN
      AD=AD0+AD1*EN
      IF (EN.GT.BNDC) AD=AD0+AD1*BNDC
      RM=RR
      RAR=RR/AR
      RAD=RD/AD
      RAW=RW/AW
      RAC=RC/AC
      IF(RW.GT.RM) RM=RW
      IF(RC.GT.RM) RM=RC
      IF(RD.GT.RM) RM=RD
      AM=AR
      IF(AW.LT.AM) AM=AW
      IF(AC.LT.AM) AM=AC
      IF(AD.LT.AM) AM=AD
      AL=AR
      IF(AW.GT.AL) AL=AW
      IF(AC.GT.AL) AL=AC
      IF(AD.GT.AL) AL=AD
      AN=RM/AM+23.*AL/AM
      NAN1=AN*15
      NAN1=2*NAN1+2
      IF(NAN1.GT.148)NAN1=148
      NAN2=NAN1+1
      ST=(RM+23.*AL)/(NAN2-1)
      A42=4.D0*ST/3.D0
C
      VIR0=0.
      VIR1=0.
      VIR2=0.
      VIR3=0.
      WIC0=0.
      WIC1=0.
      WIC2=0.
      WIC3=0.
      WID0=0.
      WID1=0.
      WID2=0.
      WID3=0.
      WIW0=0.
      WIW1=0.
      WIW2=0.
      WIW3=0.
   16 DO 10 N=IS,NAN2,2
      R=ST*(N-1)
      X=(R-RR)/AR
      IF(X.GT.23.) GO TO 30
      EX=EXP(X)
      EX1=1.+EX
      EXD=1./EX1
      EXDM=1.-EXD
      PV0=EXD
      IF(LAS.EQ.0) GO TO 31
      PV1=PV0*EXDM*RAR
      IF(LAS.EQ.1) GO TO 31
      Y1=1.-2.*EXD
      PV2=PV1*Y1*RAR/2.
      IF(LAS.EQ.2) GO TO 31
      Y2=1.-6.*EXD*EXDM
      PV3=PV2/Y1*Y2*RAR/3.
      IF(LAS.EQ.3) GO TO 31
      Y3=1.-EXD*(14.-EXD*(36.-24.*EXD))
      PV4=PV3/Y2*Y3*RAR/4.
      GO TO 31
   30 EX1=EXP(-X)
      PV0=EX1
      IF(LAS.EQ.0) GO TO 31
      PV1=PV0*RAR
      IF(LAS.EQ.1) GO TO 31
      PV2=PV1*RAR/2.
      IF(LAS.EQ.2) GO TO 31
      PV3=PV2*RAR/3.
      IF(LAS.EQ.3) GO TO 31
      PV4=PV3*RAR/4.
   31 CONTINUE
      X=(R-RC)/AC
      Y=(R-RW)/AW
      XX=Y*Y
      IF(XX.GT.180.) EXX=0.
      IF(XX.LE.180.) EXX=EXP(-XX)
      IF(X.GT.23.) GO TO 32
      EX=EXP(X)
      EX1=1.+EX
      EXD=1./EX1
      EXDM=1.-EXD
      WCC=ALF*EXD
      WWW=(1.-ALF)*EXX
      PWC0=WCC
      PWW0=WWW
      IF(LAS.EQ.0) GO TO 33
      WCC=WCC*EXDM*RAC
      WWW=WWW*2.*RAW*Y
      PWC1=WCC
      PWW1=WWW
      IF(LAS.EQ.1) GO TO 33
      YC1=1.-2.*EXD
      WCC=WCC*YC1*RAC/2.
      Y1=2.*Y*Y-1.
      WWW=WWW/Y*Y1*RAW/2.
      PWC2=WCC
      PWW2=WWW
      IF(LAS.EQ.2) GO TO 33
      YC2=1.-6.*EXD*EXDM
      WCC=WCC/YC1*YC2*RAC/3.
      Y2=Y**3*2.-Y*3.
      WWW=WWW/Y1*Y2*RAW*2./3.
      PWC3=WCC
      PWW3=WWW
      IF(LAS.EQ.3) GO TO 33
      YC3=1.-EXD*(14.-EXD*(36.-24.*EXD))
      WCC=WCC/YC2*YC3*RAC/4.
      WWW=WWW/Y2*(Y**4*4.-Y*Y*12.+3.)*RAW/4.
      PWC4=WCC
      PWW4=WWW
      GO TO 33
   32 EX1=EXP(-X)
      WCC=ALF*EX1
      WWW=(1.-ALF)*EXX
      PWC0=WCC
      PWW0=WWW
      IF(LAS.EQ.0) GO TO 33
      WCC=WCC*RAC
      WWW=WWW*2.*RAW*Y
      PWC1=WCC
      PWW1=WWW
      IF(LAS.EQ.1) GO TO 33
      WCC=WCC*RAC/2.
      Y1=2.*Y*Y-1.
      WWW=WWW/Y*Y1*RAW/2.
      PWC2=WCC
      PWW2=WWW
      IF(LAS.EQ.2) GO TO 33
      WCC=WCC*RAC/3.
      Y2=Y**3*2.-Y*3.
      WWW=WWW/Y1*Y2*RAW*2./3.
      PWC3=WCC
      PWW3=WWW
      IF(LAS.EQ.3) GO TO 33
      WCC=WCC*RAC/4.
      WWW=WWW/Y2*(Y**4*4.-Y*Y*12.+3.)*RAW/4.
      PWC4=WCC
      PWW4=WWW
   33 X=(R-RD)/AD
      IF(X.GT.23.) GO TO 34
      EX=EXP(X)
      EX1=1.+EX
      EXD=1./EX1
      EXDM=1.-EXD
      WDD=4.*EXDM*EXD
      PWD0=WDD
      IF(LAS.EQ.0) GO TO 35
      Y1=1.-2.*EXD
      WDD=WDD*Y1*RAD
      PWD1=WDD
      IF(LAS.EQ.1) GO TO 35
      Y2=1.-6.*EXD*EXDM
      WDD=WDD/Y1*Y2*RAD/2.
      PWD2=WDD
      IF(LAS.EQ.2) GO TO 35
      Y3=1.-EXD*(14.-EXD*(36.-24.*EXD))
      WDD=WDD/Y2*Y3*RAD/3.
      PWD3=WDD
      IF(LAS.EQ.3) GO TO 35
      Y4=1.-EXD*(30.-EXD*(150.-EXD*(240.-120.*EXD)))
      WDD=WDD/Y3*Y4*RAD/4.
      PWD4=WDD
      GO TO 35
   34 EX1=EXP(-X)
      WDD=4.*EX1
      PWD0=WDD
      IF(LAS.EQ.0) GO TO 35
      WDD=WDD*RAD
      PWD1=WDD
      IF(LAS.EQ.1) GO TO 35
      WDD=WDD*RAD/2.
      PWD2=WDD
      IF(LAS.EQ.2) GO TO 35
      WDD=WDD*RAD/3.
      PWD3=WDD
      IF(LAS.EQ.3) GO TO 35
      WDD=WDD*RAD/4.
      PWD4=WDD
   35 CONTINUE
      VIR0=VIR0+R**2*PV0*A42
      VIR1=VIR1+R**2*PV1*A42
      VIR2=VIR2+R**2*PV2*A42
      VIR3=VIR3+R**2*PV3*A42
      WIC0=WIC0+R**2*PWC0*A42
      WIC1=WIC1+R**2*PWC1*A42
      WIC2=WIC2+R**2*PWC2*A42
      WIC3=WIC3+R**2*PWC3*A42
      WID0=WID0+R**2*PWD0*A42
      WID1=WID1+R**2*PWD1*A42
      WID2=WID2+R**2*PWD2*A42
      WID3=WID3+R**2*PWD3*A42
      WIW0=WIW0+R**2*PWW0*A42
      WIW1=WIW1+R**2*PWW1*A42
      WIW2=WIW2+R**2*PWW2*A42
   10 WIW3=WIW3+R**2*PWW3*A42
      IS=IS+1
      NAN2=NAN1
      A42=2.*ST/3.
      IF(IS.LE.3) GO TO 16
      CBETC=0.d0
      CBETD=0.d0
      CBETW=0.d0
      IF(WIC1.NE.0.)CBETC=WIC2/WIC1
      IF(WID1.NE.0.)CBETD=WID2/WID1
      IF(WIW1.NE.0.)CBETW=WIW2/WIW1
      IF(MEVOL.LE.1) GO TO 3
      CBET0=VIR2/VIR1
    3 WRITE(21,14)VIR0,VIR1,VIR2,VIR3,CBET0
   14 FORMAT(/2X,'SPHERICAL VOLUME INTEGRALS OF REAL POTENTIAL F-FACTORS
     * AND DERIVATIVES:'/2X,'VIR0=',F8.3,2X,'VIR1=',F8.3,2X,
     *'VIR2=',F8.3,2X,'VIR3=',F8.3,2X,'CBET0=',F8.3)
      WRITE(21,15)WIC0,WIC1,WIC2,WIC3,CBETC
   15 FORMAT(2X,'SPHERICAL VOLUME INTEGRALS OF IMAGINARY (WC) F-FACTORS
     * AND DERIVATIVES:'/2X,'WIC0=',F8.3,2X,'WIC1=',F8.3,2X,
     *'WIC2=',F8.3,2X,'WIC3=',F8.3,2X,'CBETC=',F8.3)
      WRITE(21,12)WID0,WID1,WID2,WID3,CBETD
   12 FORMAT(2X,'SPHERICAL VOLUME INTEGRALS OF IMAGINARY (WD) F-FACTORS
     * AND DERIVATIVES:'/2X,'WID0=',F8.3,2X,'WID1=',F8.3,2X,
     *'WID2=',F8.3,2X,'WID3=',F8.3,2X,'CBETD=',F8.3)
      WRITE(21,11)WIW0,WIW1,WIW2,WIW3,CBETW
   11 FORMAT(2X,'SPHERICAL VOLUME INTEGRALS OF IMAGINARY (WW) F-FACTORS
     * AND DERIVATIVES:'/2X,'WIW0=',F8.3,2X,'WIW1=',F8.3,2X,
     *'WIW2=',F8.3,2X,'WIW3=',F8.3,2X,'CBETW=',F8.3)
      RR=RRR
      RETURN
      END
C     ***********************************************************
      SUBROUTINE VHFROM
C     ***********************************************************
      IMPLICIT DOUBLE PRECISION(A-H,O-Z) 
      COMMON/VHFS/WW4,VHF,ENCON,VRLANP,EFERM,AMI
       COMMON/RAD/RR,RC,RD,RW,RS,AR,AC,AW,AD,AS,ALF,AT,ANEU,RZ,ZNUC,ASP
     *,AZ
     */POTD/ALAVR,VRLA,WCBW,WCWID,WDBW,WDWID,ALAWD,EFERMN,EFERMP,ALASO
     *,PDIS,WSBW,WSWID,RRBWC,RRWID,RZBWC,RZWID
C     WW4 - ENERGY CONVERSION FACTOR DEVIDED BY 4.
C     ALAVR - POTENTIAL NON-LOCALITY IN FM.
C     ENCON - INCIDENT ENERGY
C     EMATCH - MATCHING ENERGY
C     VAL - VALUE OF VHF REAL POTENTIAL AT MATCHING POINT,
C      USING EXPONENTIAL DECAY FORMULAE
C     DIR - DERIVATIVE OF VHT POTENTIAL AT MATCHING POINT
C     USING EXPONENTIAL DECAY FORMULAE

      APAN=ANEU/1.00866491600D0 
      IFPOT=0
      EMATCH=200.00D0

       VRLNP=VRLANP
       ALAV=ALAVR
       go to 6

       IF(IFPOT.EQ.0) GO TO 7
      IF (EMATCH.LE.ENCON) GO TO 6
      REL=(EMATCH+AMI)/AMI
        IF(REL.LT.1.D00) REL=1.D00
      EIRELM=SQRT(REL**2*AT**2+2.D+0*REL*ANEU*AT+ANEU**2)
      EIRELT=AT*SQRT(REL**2*ANEU**2+2.D+0*REL*ANEU*AT+AT**2)
      DEREL=SQRT(2.D+0*REL*ANEU*AT+AT**2+ANEU**2)
C        ENCON=(REL**2-1.D0)*AMI*AT*(ANEU+AT)/DEREL**2/2.D+0
      WW4=1.2064496D-2*EIRELM*EIRELT/(ANEU*EIRELM+EIRELT)/DEREL*APAN

       VAL=VRLANP*EXP(-ALAVR**2*(EMATCH-EFERM)*WW4)
       DIR=-VAL*ALAVR**2*WW4*(1.D+00+(EMATCH-EFERM)/(EMATCH+AMI))


C     ALAVR - MARCHED NON-LOCALITY RANGE FOR PEREY BUCK FORMULA
C     VRLANP - MATCHED VHFO FOR PEREY BUCK FORMULA

       ALAVR=SQRT(-DIR/VAL/(1.D+00+DIR+(EMATCH+VAL)/(EMATCH+AMI))/WW4)
       VRLANP=VAL*EXP(ALAVR**2*(EMATCH+VAL)*WW4)

C     MORILLIOM ROMAIM FORMULAE
C     VHF AT EN BELOW EMATCH, CALCULATED BY PEREY BUCK FORMULA,
C      ACCOUNTING RELATIVISTIC EFFECTS
C     WITH PARAMETERS MATCHED WITH EXPONENTIAL DECAY DEPENDENCE

    7 REL=(ENCON+AMI)/AMI
        IF(REL.LT.1.D00) REL=1.D00
      EIRELM=SQRT(REL**2*AT**2+2.D+0*REL*ANEU*AT+ANEU**2)
      EIRELT=AT*SQRT(REL**2*ANEU**2+2.D+0*REL*ANEU*AT+AT**2)
      DEREL=SQRT(2.D+0*REL*ANEU*AT+AT**2+ANEU**2)
C        ENCON=(REL**2-1.D0)*AMI*AT*(ANEU+AT)/DEREL**2/2.D+0
      WW4=1.2064496D-2*EIRELM*EIRELT/(ANEU*EIRELM+EIRELT)/DEREL*APAN
      CONST=VRLANP*EXP(-WW4*ALAVR**2*ENCON)
       RANGE=0.000D+00
       WW4216=WW4**2*16.D+00
      VHF=0.D+00
       STEP=45.D+00
    3  VHF=VHF+STEP
      IF(VHF.GT.5.45D+02) GO TO 4
       VALUE=CONST*EXP(-WW4*ALAVR**2*VHF+WW4216*
     *(RANGE**4*(ENCON+VHF))**2)
       IF(VALUE-VHF) 1,2,3
    1 VHF=VHF-STEP
      STEP=STEP/3.0
      IF(STEP.LT.1.D-04) GO TO 2
      GO TO 3
    4 PRINT 5,ALAVR,ENCON,VALUE,WW4
      WRITE(21,5)ALAVR,ENCON,VALUE,WW4
    5 FORMAT(10X,'ERROR! CHECK ALAVR OR VHF SIGN:NO SOLUTION FOR VHF'/
     *5X,'ALAVR=',D11.5,
     *', ENCON=',D11.5,',VALUE=',D11.5,',WW4=',D11.5)
      STOP
    2 VRLANP=VRLNP
      ALAVR=ALAV
      RETURN

C     VHF AT EN AS EXPONENTIAL DECAY, WITH ALAVR AS NON-LOCALITY RANGE,
C     WITH REDUCED MASS ACCOUNTING RELATIVISTIC EFFECTS

    6 CONTINUE

C     CALCULATIONS OF RELATIVISTIC REDUCED MASS OF SYSTEM DEVIDED BY 2*h**2

      REL=(ENCON+AMI)/AMI
        IF(REL.LT.1.D00) REL=1.D00
      EIRELM=SQRT(REL**2*AT**2+2.D+0*REL*ANEU*AT+ANEU**2)
      EIRELT=AT*SQRT(REL**2*ANEU**2+2.D+0*REL*ANEU*AT+AT**2)
      DEREL=SQRT(2.D+0*REL*ANEU*AT+AT**2+ANEU**2)
C      ENCON=(REL**2-1.D0)*AMI*AT*(ANEU+AT)/DEREL**2/2.D+0
      WW4=1.2064496D-2*EIRELM*EIRELT/(ANEU*EIRELM+EIRELT)/DEREL*APAN

C     THIS IS NON RELATIVISTIC REDUCED MASS OF SYSTEM DEVIDED BY 2*h**2
      WW4=1.2064496D-2*AT/(ANEU+AT)*APAN

C     The line below is in the spirit of Morillon and Romain formulation
C     VHF=VRLANP*EXP(-ALAVR**2*(ENCON-EFERM)*WW4)
C
C     To follow our published papers on PRC
C
      VHF=VRLANP*EXP(-ALAVR*(ENCON-EFERM))
      RETURN
      END
C     *******************************************************
      SUBROUTINE THORA(IOUT)
C     *******************************************************
C     AUTHOR: R. Capote, March 2005
C     
C     * $Date: 2007/09/03 14:20:35 $
C     * $Id: thora.f,v 1.6 2007/09/03 14:20:35 Capote Exp $
C
C     GIVES THE TIME ELAPSED SINCE THE FIRST CALL
C     Note: Elapsed time must be less than one month
C
C     (g77, g95, LAHEY, and MS FORTRAN compatible)
C
      DOUBLE PRECISION BEGTIM,ENDTIM,BEGDAY,ENDDAY,DIFTIM,DIFTI1
      INTEGER IOUT
      CHARACTER*8 DATE
      CHARACTER*10 TIME
      CHARACTER*5 ZONE
      INTEGER DT
      DIMENSION DT(8)
      LOGICAL NEVER_CALLED
      DATA NEVER_CALLED/.TRUE./
      SAVE BEGTIM,BEGDAY,NEVER_CALLED

      IF (NEVER_CALLED) then
        NEVER_CALLED = .FALSE.
        CALL DATE_AND_TIME (DATE,TIME,ZONE,DT)
        BEGDAY=DT(3)
        BEGTIM=DT(5)*3600.d0+DT(6)*60.d0+DT(7)+DT(8)/1000.d0
        WRITE(IOUT,1001) time(1:2),time(3:4),time(5:6),
     >                   DATE(7:8),DATE(5:6),DATE(1:4)
      ELSE
        CALL DATE_AND_TIME (DATE,TIME,ZONE,DT)
        ENDTIM=DT(5)*3600.d0+DT(6)*60.d0+DT(7)+DT(8)/1000.d0
        ENDDAY=DT(3)
        ENDTIM = ENDTIM +  (ENDDAY-BEGDAY)*86400.d0
        DIFTIM=(ENDTIM-BEGTIM)/60.d0
        DIFTI1=(DIFTIM-INT(DIFTIM))*60.d0
        WRITE(IOUT,1002) INT(DIFTIM),NINT(DIFTI1)
      ENDIF

      RETURN
 1001 FORMAT
     >(/10X,'Start time: ',A2,':',A2,'.',A2,' (',A2,'-',A2,'-',A4,')'/)
 1002 FORMAT(//10X,' Calculation time: ',I3,' min ',I2,' s'//)
C====================================================================
      END
C     *******************************************************
C     SUBROUTINE WRITTEN BY Rui LI AND Weili SUN FROM IAPCM CHINA
      SUBROUTINE ANPOW !analyzing power
C     *******************************************************
      IMPLICIT REAL*8(A-H,O-Z)
      !----------analyzing power---------------
      REAL*8 FC_R(150),FC_I(150),X_R(4,40,50,50,150),
     *       X_I(4,40,50,50,150),COEF_X(4),CSRX,CSIX,
     *       CS_ANG_MIF(40,50,50,150),CS_ANG(40,150)             
       INTEGER MII_INDEX,MII,MII_MAX,MIF_INDEX,MIF,MIF_MAX
       COMMON Px(40,150),Pz(40,150),Ay(40,150) !anther two vector components 


      !----analyzing power---------
      COMMON/DISK/TET(150),MTET
      COMMON/DISCAN/DISC(40,150),PL(180,150),COEF(40,180),LKK
      COMMON/KG/J1,J2,M1,M2,J,M,AKG
     */ENA/EN,EL(40),BET(10),NUR,NMAX,NPD,LAS
     */NU/NUI,NUF
      COMMON/QNB/JO(40),NPO(40),KO(40),NCA(40)
      COMMON/POTB/WNK(40),WN(40),VR,WC,WD
      COMMON/CMAT/CR(200,200),CI(200,200)
     */CCMAT/CRD(180,10,200),CID(180,10,200)
      COMMON/JNN/CSS,INCC,NCLL,NSS,NJ,INCR
     */QNSB/INC(180),INR(180),JS(180)
     */QNSBD/LNJ(180,250),JNJ(180,250),NNJ(180,250)
      COMMON/QNS1/LNJ1(250),JNJ1(250),NNJ1(250),KPJ1(250)
     */NCLMA/LLMA,NCMA,NSMA,KODMA
     */COUL/CONZ,ETA,COPH(40,90)
     */COULCO/COEFR(90),COEFI(90)
      COMMON/RAD/RR,RC,RD,RW,RS,AR,AC,AW,AD,AS,ALF,AT,ANEU,RZ,ZNUC,ASP
     *,AZ
     */AP/KEYAP
            !----------initialization----------
      X_R=0.D0
      X_I=0.D0
      FC_R=0.D0
      FC_I=0.D0
      DISC=0.D0
C     Px=0.
C     Pz=0.
      Ay=0.D0
C     open(unit=904,file='PxPz.txt')

        CS_ANG=0.D0
      !*************initialization***********
      CALL PLM_CAL !calculating harmonic function
      LKK=1
      ETA=CONZ/WNK(1)
      IC=JO(1)
      !-------------calculate f_c(Coulomb amplitude)-------------
      DO 627 M=1,MTET 
      TETA=TET(M)*3.1415927/180
      ALST2=DLOG(DSIN(TETA/2.D0))
      ARGC=2.D0*(COPH(1,1)-ETA*ALST2)
      temp=-ETA/(20.D0*WNK(1)*DSIN(TETA/2.D0)**2)*DCOS(ARGC)
      FC_R(M)=temp 
      temp=-ETA/(20.D0*WNK(1)*DSIN(TETA/2.D0)**2)*DSIN(ARGC)
      FC_I(M)=temp       
  627 continue          
      !------------------------------------------------------------     
      DO 1 K1=1,NJ
      JS1=JS(K1)
      N1I=INC(K1)
      N1F=INR(K1)
      DO 1 N1C=1,N1I 
      L1I=LNJ(K1,N1C)
      J1I=JNJ(K1,N1C)
      DO 8 N1R=1,N1F
      L1F=LNJ(K1,N1R)              !L1F is not multiplied by 2
      J1F=JNJ(K1,N1R)
      NU1=NNJ(K1,N1R)

      MII_MAX=IC+1 !number of MII:2*I+1
      DO 614 MII_INDEX=1,MII_MAX
      MII=2*MII_INDEX-IC-2 !2*MI
      MIF_MAX=JO(NU1)+1 !number of MIF: 2*I'+1
      DO 615 MIF_INDEX=1,MIF_MAX
      MIF=2*MIF_INDEX-JO(NU1)-2 
      COEF_X=2.D0*DSQRT(3.1415927D0)/(10.D0*WNK(1))
     **DSQRT(1.D0*(2.D0*L1I+1))
      J1=2*L1I
      J2=1
      M1=0
      M2=1
      J=J1I
      M=1
      call KLEGO
      COEF_X(1)=COEF_X(1)*AKG
      COEF_X(2)=COEF_X(2)*AKG
      J1=J1I
      J2=IC
      M1=1
      M2=MII
      J=JS1
      M=MII+1
      CALL KLEGO
      COEF_X(1)=COEF_X(1)*AKG
      COEF_X(2)=COEF_X(2)*AKG
      J1=2*L1F
      J2=1
      M1=MII-MIF
      M2=1
      J=J1F
      M=1+MII-MIF
      CALL KLEGO
      COEF_X(1)=COEF_X(1)*AKG
      J1=2*L1F
      J2=1
      M1=2+MII-MIF
      M2=-1
      J=J1F
      M=1+MII-MIF
      CALL KLEGO
      COEF_X(2)=COEF_X(2)*AKG
      J1=J1F
      J2=JO(NU1) 
      M1=1+MII-MIF
      M2=MIF
      J=JS1
      M=MII+1
      CALL KLEGO
      COEF_X(1)=COEF_X(1)*AKG
      COEF_X(2)=COEF_X(2)*AKG   

      J1=2*L1I
      J2=1
      M1=0
      M2=-1
      J=J1I
      M=-1
      CALL KLEGO
      COEF_X(3)=COEF_X(3)*AKG
      COEF_X(4)=COEF_X(4)*AKG
      J1=J1I
      J2=IC
      M1=-1
      M2=MII
      J=JS1
      M=MII-1
      CALL KLEGO
      COEF_X(3)=COEF_X(3)*AKG
      COEF_X(4)=COEF_X(4)*AKG
      J1=2*L1F
      J2=1
      M1=-2+MII-MIF
      M2=1
      J=J1F
      M=-1+MII-MIF
      CALL KLEGO
      COEF_X(3)=COEF_X(3)*AKG
      J1=2*L1F
      J2=1
      M1=MII-MIF
      M2=-1
      J=J1F
      M=-1+MII-MIF
      CALL KLEGO
      COEF_X(4)=COEF_X(4)*AKG
      J1=J1F
      J2=JO(NU1) 
      M1=-1+MII-MIF
      M2=MIF
      J=JS1
      M=MII-1
      CALL KLEGO
      COEF_X(3)=COEF_X(3)*AKG
      COEF_X(4)=COEF_X(4)*AKG 

               CORR=DCOS(COPH(NU1,L1F+1)+COPH(1,L1I+1)) 
               COII=DSIN(COPH(NU1,L1F+1)+COPH(1,L1I+1)) 
               CSRX=CORR*CRD(K1,N1C,N1R)-COII*CID(K1,N1C,N1R)
               CSIX=CORR*CID(K1,N1C,N1R)+COII*CRD(K1,N1C,N1R)
      DO 616 M=1,MTET
      IF(NU1.EQ.1 .AND. L1I.EQ.0 .AND. J1I.EQ.1 .AND. JS1.EQ.(IC+1) 
     * .AND. L1F.EQ.0   .AND. J1F.EQ.1 .AND. MII.EQ.MIF)
     *THEN !add the Coulomb amplitude(only added once for different J1I and JS1)
      X_R(1,NU1,MII_INDEX,MIF_INDEX,M)=X_R(1,NU1,MII_INDEX,MIF_INDEX,M)+
     *FC_R(M)
      X_I(1,NU1,MII_INDEX,MIF_INDEX,M)=X_I(1,NU1,MII_INDEX,MIF_INDEX,M)+
     *FC_I(M)
      X_R(4,NU1,MII_INDEX,MIF_INDEX,M)=X_R(4,NU1,MII_INDEX,MIF_INDEX,M)+
     *FC_R(M)
      X_I(4,NU1,MII_INDEX,MIF_INDEX,M)=X_I(4,NU1,MII_INDEX,MIF_INDEX,M)+
     *FC_I(M)
      COUNT_FC=COUNT_FC+1
      ENDIF
      X_R(1,NU1,MII_INDEX,MIF_INDEX,M)=X_R(1,NU1,MII_INDEX,MIF_INDEX,M)+
     *CSRX*YLM_VAL(L1F,(MII-MIF)/2,M)*COEF_X(1)
      X_I(1,NU1,MII_INDEX,MIF_INDEX,M)=X_I(1,NU1,MII_INDEX,MIF_INDEX,M)+
     *CSIX*YLM_VAL(L1F,(MII-MIF)/2,M)*COEF_X(1)
      X_R(2,NU1,MII_INDEX,MIF_INDEX,M)=X_R(2,NU1,MII_INDEX,MIF_INDEX,M)+
     *CSRX*YLM_VAL(L1F,1+(MII-MIF)/2,M)*COEF_X(2)
      X_I(2,NU1,MII_INDEX,MIF_INDEX,M)=X_I(2,NU1,MII_INDEX,MIF_INDEX,M)+
     *CSIX*YLM_VAL(L1F,1+(MII-MIF)/2,M)*COEF_X(2)
      X_R(3,NU1,MII_INDEX,MIF_INDEX,M)=X_R(3,NU1,MII_INDEX,MIF_INDEX,M)+
     *CSRX*YLM_VAL(L1F,-1+(MII-MIF)/2,M)*COEF_X(3)
      X_I(3,NU1,MII_INDEX,MIF_INDEX,M)=X_I(3,NU1,MII_INDEX,MIF_INDEX,M)+
     *CSIX*YLM_VAL(L1F,-1+(MII-MIF)/2,M)*COEF_X(3)
      X_R(4,NU1,MII_INDEX,MIF_INDEX,M)=X_R(4,NU1,MII_INDEX,MIF_INDEX,M)+
     *CSRX*YLM_VAL(L1F,(MII-MIF)/2,M)*COEF_X(4)
      X_I(4,NU1,MII_INDEX,MIF_INDEX,M)=X_I(4,NU1,MII_INDEX,MIF_INDEX,M)+
     *CSIX*YLM_VAL(L1F,(MII-MIF)/2,M)*COEF_X(4)
  616 CONTINUE
  615 CONTINUE  
  614 CONTINUE
    8 CONTINUE
    1 CONTINUE
    
      DO 619 N=NUI,NUF
      MII_MAX=IC+1
      DO 612 MII_INDEX=1,MII_MAX
      MIF_MAX=JO(N)+1
      DO 618 MIF_INDEX=1,MIF_MAX
      DO 617 M=1,MTET
       CS_ANG_MIF(N,MII_INDEX,MIF_INDEX,M)=
     *X_R(1,N,MII_INDEX,MIF_INDEX,M)**2+ 
     *X_I(1,N,MII_INDEX,MIF_INDEX,M)**2+
     *X_R(2,N,MII_INDEX,MIF_INDEX,M)**2+
     *X_I(2,N,MII_INDEX,MIF_INDEX,M)**2+
     *X_R(3,N,MII_INDEX,MIF_INDEX,M)**2+ 
     *X_I(3,N,MII_INDEX,MIF_INDEX,M)**2+
     *X_R(4,N,MII_INDEX,MIF_INDEX,M)**2+
     *X_I(4,N,MII_INDEX,MIF_INDEX,M)**2
      CS_ANG(N,M)=CS_ANG(N,M)+CS_ANG_MIF(N,MII_INDEX,MIF_INDEX,M)
     */MII_MAX/2.D0 !calculate the cross-section

      IF(KEYAP.EQ.1) GO TO 625
      
      DISC(N,M)=DISC(N,M)+    !calculate Py
     *2.D0*(X_R(1,N,MII_INDEX,MIF_INDEX,M)*X_I(2,N,MII_INDEX,MIF_INDEX,
     *   M)-X_I(1,N,MII_INDEX,MIF_INDEX,M)*X_R(2,N,MII_INDEX,MIF_INDEX,
     *   M))/MII_MAX/2.D0 +
     *2.D0*(X_R(3,N,MII_INDEX,MIF_INDEX,M)*X_I(4,N,MII_INDEX,MIF_INDEX,
     *   M)-X_I(3,N,MII_INDEX,MIF_INDEX,M)*X_R(4,N,MII_INDEX,MIF_INDEX,
     *   M))/MII_MAX/2.D0
     
      GO TO 617
      
  625 DISC(N,M)=DISC(N,M)+  !calculate Ay
     *2.D0*(X_R(1,N,MII_INDEX,MIF_INDEX,M)*X_I(3,N,MII_INDEX,MIF_INDEX,
     *   M)-X_I(1,N,MII_INDEX,MIF_INDEX,M)*X_R(3,N,MII_INDEX,MIF_INDEX,
     *   M))/MII_MAX/2.D0 +
     *2.D0*(X_R(2,N,MII_INDEX,MIF_INDEX,M)*X_I(4,N,MII_INDEX,MIF_INDEX,
     *   M)-X_I(2,N,MII_INDEX,MIF_INDEX,M)*X_R(4,N,MII_INDEX,MIF_INDEX,
     *   M))/MII_MAX/2.D0
  617 CONTINUE
  618 CONTINUE
  612 CONTINUE
      DO 620 M=1,MTET 
 
      DISC(N,M)=DISC(N,M)/CS_ANG(N,M)
      IF(KEYAP.EQ.1) DISC(N,M)=-DISC(N,M)
  620 CONTINUE    
  619 CONTINUE 


      RETURN
      END SUBROUTINE ANPOW
C     *******************************************************
      SUBROUTINE PLM_CAL
C     *******************************************************
      IMPLICIT REAL*8(A-H,O-Z)
      COMMON/PLM/PLM_DATA(181,181,150)
      COMMON/DISK/TET(150),MTET
      PLM_DATA(1,1,:)=1./DSQRT(2.D0)
      DO 636 L=1,180
      C=1.D0
      DO 638 INDEX_L=1,L
      C=C*DSQRT(1.D0-1.D0/(2.D0*INDEX_L))
  638 CONTINUE

      DO 637 M=1,MTET
      TETA=TET(M)*3.1415927D0/180.D0
      COS_TETA=DCOS(TETA)
      SIN_TETA=DSIN(TETA)
      COT_TETA=COS_TETA/SIN_TETA
C     COT_TETA=DCOTAN(TETA)
      PLM_DATA(L+1,L+1,M)=C*(-1.D0*SIN_TETA)**L !P L L
      PLM_DATA(L+1,L,M)=PLM_DATA(L+1,L+1,M)*(-2.D0*COT_TETA)*L/
     *                  DSQRT(2.D0*L) !P L L-1
      DO 639 LM=L-2,0,-1 !recursion from l-2 to 0 order 
      PLM_DATA(L+1,LM+1,M)=(-2.D0*COT_TETA*(LM+1)*PLM_DATA(L+1,LM+2,M)-
     *DSQRT(1.D0*(L+LM+2))*DSQRT(1.D0*(L-LM-1))*PLM_DATA(L+1,LM+3,M))/
     *(DSQRT(1.D0*(L+LM+1))*DSQRT(1.D0*(L-LM)))
  639 CONTINUE !LM LOOP

  637 CONTINUE !ANGULAR LOOP
      PLM_DATA(L+1,1:L+1,:)=PLM_DATA(L+1,1:L+1,:)*DSQRT(L+0.5D0)
      IFLAG=1
      DO 635 LM=0,L
      PLM_DATA(L+1,LM+1,:)=PLM_DATA(L+1,LM+1,:)*IFLAG
      IFLAG=-IFLAG
  635 CONTINUE !LM2 LOOP
  636 CONTINUE !L LOOP
      END SUBROUTINE PLM_CAL
C     *******************************************************     
      FUNCTION YLM_VAL(Y_L,Y_M,Y_ANGU) !find the value according to l and m
C     *******************************************************
      IMPLICIT REAL*8(A-H,O-Z)
      COMMON/PLM/PLM_DATA(181,181,150)
      REAL*8 YLM_VAL      
      INTEGER Y_ANGU,Y_L,Y_M,YM_TEMP !Y_ANGU is the index of angular 
      
      IF(Y_M.ge.0) THEN
      YLM_VAL=(-1)**Y_M*PLM_DATA(Y_L+1,Y_M+1,Y_ANGU)
     * /DSQRT(2.D0*3.1415927D0) ! for m>=0 case
      ENDIF
      
      IF(Y_M.lt.0) THEN
       YM_TEMP=-1*Y_M
       YLM_VAL=PLM_DATA(Y_L+1,YM_TEMP+1,Y_ANGU)
     * /DSQRT(2.D0*3.1415927D0) ! for m<0 case
      ENDIF
      
      RETURN
      END FUNCTION YLM_VAL
C     ******************************************************* 

C     *******************************************************
C     SUBROUTINE WRITTEN BY Rui LI AND Weili SUN FROM IAPCM CHINA
      SUBROUTINE DEUTR !analyzing power
C     *******************************************************
      IMPLICIT REAL*8(A-H,O-Z)
      !----deutron angular distribution1---------
      REAL*8 FC_R(150),FC_I(150),X_R(9,10,20,20,150),
     *       X_I(9,10,20,20,150),COEF_X(9),CSRX,CSIX,
     *       CS_ANG_MIF(9,20,20,150),CS_ANG(9,150)               
       INTEGER MII_INDEX,MII,MII_MAX,MIF_INDEX,MIF,MIF_MAX


      !----deutron angular distribution2---------
      COMMON/DISK/TET(150),MTET
      COMMON/DISCAN/DISC(40,150),PL(180,150),COEF(40,180),LKK
      COMMON/KG/J1,J2,M1,M2,J,M,AKG
     */ENA/EN,EL(40),BET(10),NUR,NMAX,NPD,LAS
     */NU/NUI,NUF
      COMMON/QNB/JO(40),NPO(40),KO(40),NCA(40)
      COMMON/POTB/WNK(40),WN(40),VR,WC,WD
      COMMON/CMAT/CR(200,200),CI(200,200)
     */CCMAT/CRD(180,10,200),CID(180,10,200)
      COMMON/JNN/CSS,INCC,NCLL,NSS,NJ,INCR
     */QNSB/INC(180),INR(180),JS(180)
     */QNSBD/LNJ(180,250),JNJ(180,250),NNJ(180,250)
      COMMON/QNS1/LNJ1(250),JNJ1(250),NNJ1(250),KPJ1(250)
     */NCLMA/LLMA,NCMA,NSMA,KODMA
     */COUL/CONZ,ETA,COPH(40,90)
     */COULCO/COEFR(90),COEFI(90)
      COMMON/RAD/RR,RC,RD,RW,RS,AR,AC,AW,AD,AS,ALF,AT,ANEU,RZ,ZNUC,ASP
     *,AZ
     */AP/KEYAP
            !----------initialization----------
      X_R=0.D0
      X_I=0.D0
      FC_R=0.D0
      FC_I=0.D0
      DISC=0.D0


      CS_ANG=0.D0
      !*************initialization***********
      CALL PLM_CAL !calculating harmonic function
      LKK=1
      ETA=CONZ/WNK(1)
      IC=JO(1)
      !-------------calculate f_c(Coulomb amplitude)-------------
      DO 627 M=1,MTET 
      TETA=TET(M)*3.1415927D0/180.d0
      ALST2=DLOG(DSIN(TETA/2.D0))
      ARGC=2.D0*(COPH(1,1)-ETA*ALST2)
      temp=-ETA/(20.D0*WNK(1)*DSIN(TETA/2.D0)**2)*DCOS(ARGC)
      FC_R(M)=temp 
      temp=-ETA/(20.D0*WNK(1)*DSIN(TETA/2.D0)**2)*DSIN(ARGC)
      FC_I(M)=temp       
  627 continue          
      !------------------------------------------------------------     
      DO 1 K1=1,NJ
      JS1=JS(K1)
      N1I=INC(K1)
      N1F=INR(K1)
      DO 1 N1C=1,N1I 
      L1I=LNJ(K1,N1C)
      J1I=JNJ(K1,N1C)
      DO 8 N1R=1,N1F
      L1F=LNJ(K1,N1R)              !L1F is not multiplied by 2
      J1F=JNJ(K1,N1R)
      NU1=NNJ(K1,N1R)

      MII_MAX=IC+1 !number of MII:2*I+1
      DO 614 MII_INDEX=1,MII_MAX
      MII=2*MII_INDEX-IC-2 !2*MI
      MIF_MAX=JO(NU1)+1 !number of MIF: 2*I'+1
      DO 615 MIF_INDEX=1,MIF_MAX
      MIF=2*MIF_INDEX-JO(NU1)-2 
      COEF_X=2.D0*DSQRT(3.1415927D0)/(10.D0*WNK(1))
     **DSQRT(1.D0*(2.D0*L1I+1))
      J1=2*L1I
      J2=2
      M1=0
      M2=2
      J=J1I
      M=2
      call KLEGO
      COEF_X(1)=COEF_X(1)*AKG
      COEF_X(2)=COEF_X(2)*AKG
      COEF_X(3)=COEF_X(3)*AKG
      J1=J1I
      J2=IC
      M1=2
      M2=MII
      J=JS1
      M=MII+2
      CALL KLEGO
      COEF_X(1)=COEF_X(1)*AKG
      COEF_X(2)=COEF_X(2)*AKG
      COEF_X(3)=COEF_X(3)*AKG
      J1=2*L1F
      J2=2
      M1=MII-MIF
      M2=2
      J=J1F
      M=2+MII-MIF
      CALL KLEGO
      COEF_X(1)=COEF_X(1)*AKG
      J1=2*LIF
      J2=2
      M1=2+MII-MIF
      M2=0
      J=J1F
      M=2+MII-MIF
      CALL KLEGO
      COEF_X(2)=COEF_X(2)*AKG
      J1=2*L1F
      J2=1
      M1=4+MII-MIF
      M2=-2
      J=J1F
      M=2+MII-MIF
      CALL KLEGO
      COEF_X(3)=COEF_X(3)*AKG
      J1=J1F
      J2=JO(NU1) 
      M1=2+MII-MIF
      M2=MIF
      J=JS1
      M=MII+2
      CALL KLEGO
      COEF_X(1)=COEF_X(1)*AKG
      COEF_X(2)=COEF_X(2)*AKG 
      COEF_X(3)=COEF_X(3)*AKG  
c----------------------------
      J1=2*L1I
      J2=2
      M1=0
      M2=0
      J=J1I
      M=0
      CALL KLEGO
      COEF_X(4)=COEF_X(4)*AKG
      COEF_X(5)=COEF_X(5)*AKG
      COEF_X(6)=COEF_X(6)*AKG
      J1=J1I
      J2=IC
      M1=0
      M2=MII
      J=JS1
      M=MII
      CALL KLEGO
      COEF_X(4)=COEF_X(4)*AKG
      COEF_X(5)=COEF_X(5)*AKG
      COEF_X(6)=COEF_X(6)*AKG
      J1=2*L1F
      J2=2
      M1=-2+MII-MIF
      M2=2
      J=J1F
      M=MII-MIF
      CALL KLEGO
      COEF_X(4)=COEF_X(4)*AKG
      J1=2*L1F
      J2=2
      M1=MII-MIF
      M2=0
      J=J1F
      M=MII-MIF
      CALL KLEGO
      COEF_X(5)=COEF_X(5)*AKG
      J1=2*L1F
      J2=2
      M1=2+MII-MIF
      M2=-2
      J=J1F
      M=MII-MIF
      CALL KLEGO
      COEF_X(6)=COEF_X(6)*AKG
      J1=J1F
      J2=JO(NU1) 
      M1=MII-MIF
      M2=MIF
      J=JS1
      M=MII
      CALL KLEGO
      COEF_X(4)=COEF_X(4)*AKG
      COEF_X(5)=COEF_X(5)*AKG 
      COEF_X(6)=COEF_X(6)*AKG
c----------------------------
      J1=2*L1I
      J2=2
      M1=0
      M2=-2
      J=J1I
      M=-2
      CALL KLEGO
      COEF_X(7)=COEF_X(7)*AKG
      COEF_X(8)=COEF_X(8)*AKG
      COEF_X(9)=COEF_X(9)*AKG
      J1=J1I
      J2=IC
      M1=-2
      M2=MII
      J=JS1
      M=MII-2
      CALL KLEGO
      COEF_X(7)=COEF_X(7)*AKG
      COEF_X(8)=COEF_X(8)*AKG
      COEF_X(9)=COEF_X(9)*AKG
      J1=2*L1F
      J2=2
      M1=-4+MII-MIF
      M2=2
      J=J1F
      M=-2+MII-MIF
      CALL KLEGO
      COEF_X(7)=COEF_X(7)*AKG
      J1=2*L1F
      J2=2
      M1=-2+MII-MIF
      M2=0
      J=J1F
      M=-2+MII-MIF
      CALL KLEGO
      COEF_X(8)=COEF_X(8)*AKG
      J1=2*L1F
      J2=2
      M1=MII-MIF
      M2=-2
      J=J1F
      M=-2+MII-MIF
      CALL KLEGO
      COEF_X(9)=COEF_X(9)*AKG
      J1=J1F
      J2=JO(NU1) 
      M1=-2+MII-MIF
      M2=MIF
      J=JS1
      M=-2+MII
      CALL KLEGO
      COEF_X(7)=COEF_X(7)*AKG
      COEF_X(8)=COEF_X(8)*AKG 
      COEF_X(9)=COEF_X(9)*AKG 

               CORR=DCOS(COPH(NU1,L1F+1)+COPH(1,L1I+1)) 
               COII=DSIN(COPH(NU1,L1F+1)+COPH(1,L1I+1)) 
               CSRX=CORR*CRD(K1,N1C,N1R)-COII*CID(K1,N1C,N1R)
               CSIX=CORR*CID(K1,N1C,N1R)+COII*CRD(K1,N1C,N1R)
      DO 616 M=1,MTET
      IF(NU1.EQ.1 .AND. L1I.EQ.0 .AND. J1I.EQ.2   ! .AND. JS1.EQ.(IC+2) 
     * .AND. L1F.EQ.0   .AND. J1F.EQ.2 .AND. MII.EQ.MIF)
     *THEN !add the Coulomb amplitude(only added once for different J1I and JS1)
      X_R(1,NU1,MII_INDEX,MIF_INDEX,M)=X_R(1,NU1,MII_INDEX,MIF_INDEX,M)+
     *FC_R(M)
      X_I(1,NU1,MII_INDEX,MIF_INDEX,M)=X_I(1,NU1,MII_INDEX,MIF_INDEX,M)+
     *FC_I(M)
      X_R(5,NU1,MII_INDEX,MIF_INDEX,M)=X_R(5,NU1,MII_INDEX,MIF_INDEX,M)+
     *FC_R(M)
      X_I(5,NU1,MII_INDEX,MIF_INDEX,M)=X_I(5,NU1,MII_INDEX,MIF_INDEX,M)+
     *FC_I(M)
      X_R(9,NU1,MII_INDEX,MIF_INDEX,M)=X_R(9,NU1,MII_INDEX,MIF_INDEX,M)+
     *FC_R(M)
      X_I(9,NU1,MII_INDEX,MIF_INDEX,M)=X_I(9,NU1,MII_INDEX,MIF_INDEX,M)+
     *FC_I(M)
      COUNT_FC=COUNT_FC+1
      ENDIF
      X_R(1,NU1,MII_INDEX,MIF_INDEX,M)=X_R(1,NU1,MII_INDEX,MIF_INDEX,M)+
     *CSRX*YLM_VAL(L1F,(MII-MIF)/2,M)*COEF_X(1)
      X_I(1,NU1,MII_INDEX,MIF_INDEX,M)=X_I(1,NU1,MII_INDEX,MIF_INDEX,M)+
     *CSIX*YLM_VAL(L1F,(MII-MIF)/2,M)*COEF_X(1)
      X_R(2,NU1,MII_INDEX,MIF_INDEX,M)=X_R(2,NU1,MII_INDEX,MIF_INDEX,M)+
     *CSRX*YLM_VAL(L1F,1+(MII-MIF)/2,M)*COEF_X(2)
      X_I(2,NU1,MII_INDEX,MIF_INDEX,M)=X_I(2,NU1,MII_INDEX,MIF_INDEX,M)+
     *CSIX*YLM_VAL(L1F,1+(MII-MIF)/2,M)*COEF_X(2)
      X_R(3,NU1,MII_INDEX,MIF_INDEX,M)=X_R(3,NU1,MII_INDEX,MIF_INDEX,M)+
     *CSRX*YLM_VAL(L1F,2+(MII-MIF)/2,M)*COEF_X(3)
      X_I(3,NU1,MII_INDEX,MIF_INDEX,M)=X_I(3,NU1,MII_INDEX,MIF_INDEX,M)+
     *CSIX*YLM_VAL(L1F,2+(MII-MIF)/2,M)*COEF_X(3)
     
      X_R(4,NU1,MII_INDEX,MIF_INDEX,M)=X_R(4,NU1,MII_INDEX,MIF_INDEX,M)+
     *CSRX*YLM_VAL(L1F,-1+(MII-MIF)/2,M)*COEF_X(4)
      X_I(4,NU1,MII_INDEX,MIF_INDEX,M)=X_I(4,NU1,MII_INDEX,MIF_INDEX,M)+
     *CSIX*YLM_VAL(L1F,-1+(MII-MIF)/2,M)*COEF_X(4)
      X_R(5,NU1,MII_INDEX,MIF_INDEX,M)=X_R(5,NU1,MII_INDEX,MIF_INDEX,M)+
     *CSRX*YLM_VAL(L1F,(MII-MIF)/2,M)*COEF_X(5)
      X_I(5,NU1,MII_INDEX,MIF_INDEX,M)=X_I(5,NU1,MII_INDEX,MIF_INDEX,M)+
     *CSIX*YLM_VAL(L1F,(MII-MIF)/2,M)*COEF_X(5)   
      X_R(6,NU1,MII_INDEX,MIF_INDEX,M)=X_R(6,NU1,MII_INDEX,MIF_INDEX,M)+
     *CSRX*YLM_VAL(L1F,1+(MII-MIF)/2,M)*COEF_X(6)
      X_I(6,NU1,MII_INDEX,MIF_INDEX,M)=X_I(6,NU1,MII_INDEX,MIF_INDEX,M)+
     *CSIX*YLM_VAL(L1F,1+(MII-MIF)/2,M)*COEF_X(6)      

      X_R(7,NU1,MII_INDEX,MIF_INDEX,M)=X_R(7,NU1,MII_INDEX,MIF_INDEX,M)+
     *CSRX*YLM_VAL(L1F,-2+(MII-MIF)/2,M)*COEF_X(7)
      X_I(7,NU1,MII_INDEX,MIF_INDEX,M)=X_I(7,NU1,MII_INDEX,MIF_INDEX,M)+
     *CSIX*YLM_VAL(L1F,-2+(MII-MIF)/2,M)*COEF_X(7)
      X_R(8,NU1,MII_INDEX,MIF_INDEX,M)=X_R(8,NU1,MII_INDEX,MIF_INDEX,M)+
     *CSRX*YLM_VAL(L1F,-1+(MII-MIF)/2,M)*COEF_X(8)
      X_I(8,NU1,MII_INDEX,MIF_INDEX,M)=X_I(8,NU1,MII_INDEX,MIF_INDEX,M)+
     *CSIX*YLM_VAL(L1F,-1+(MII-MIF)/2,M)*COEF_X(8)   
      X_R(9,NU1,MII_INDEX,MIF_INDEX,M)=X_R(9,NU1,MII_INDEX,MIF_INDEX,M)+
     *CSRX*YLM_VAL(L1F,(MII-MIF)/2,M)*COEF_X(9)
      X_I(9,NU1,MII_INDEX,MIF_INDEX,M)=X_I(9,NU1,MII_INDEX,MIF_INDEX,M)+
     *CSIX*YLM_VAL(L1F,(MII-MIF)/2,M)*COEF_X(9)      
  616 CONTINUE
  615 CONTINUE  
  614 CONTINUE
    8 CONTINUE
    1 CONTINUE
    
      DO 619 N=NUI,NUF
      MII_MAX=IC+1
      DO 612 MII_INDEX=1,MII_MAX
      MIF_MAX=JO(N)+1
      DO 618 MIF_INDEX=1,MIF_MAX
      DO 617 M=1,MTET
       CS_ANG_MIF(N,MII_INDEX,MIF_INDEX,M)=
     *X_R(1,N,MII_INDEX,MIF_INDEX,M)**2+ 
     *X_I(1,N,MII_INDEX,MIF_INDEX,M)**2+
     *X_R(2,N,MII_INDEX,MIF_INDEX,M)**2+
     *X_I(2,N,MII_INDEX,MIF_INDEX,M)**2+
     *X_R(3,N,MII_INDEX,MIF_INDEX,M)**2+ 
     *X_I(3,N,MII_INDEX,MIF_INDEX,M)**2+
     *X_R(4,N,MII_INDEX,MIF_INDEX,M)**2+
     *X_I(4,N,MII_INDEX,MIF_INDEX,M)**2+
     *X_R(5,N,MII_INDEX,MIF_INDEX,M)**2+
     *X_I(5,N,MII_INDEX,MIF_INDEX,M)**2+
     *X_R(6,N,MII_INDEX,MIF_INDEX,M)**2+
     *X_I(6,N,MII_INDEX,MIF_INDEX,M)**2+     
     *X_R(7,N,MII_INDEX,MIF_INDEX,M)**2+
     *X_I(7,N,MII_INDEX,MIF_INDEX,M)**2+ 
     *X_R(8,N,MII_INDEX,MIF_INDEX,M)**2+
     *X_I(8,N,MII_INDEX,MIF_INDEX,M)**2+
     *X_R(9,N,MII_INDEX,MIF_INDEX,M)**2+
     *X_I(9,N,MII_INDEX,MIF_INDEX,M)**2     
      CS_ANG(N,M)=CS_ANG(N,M)+CS_ANG_MIF(N,MII_INDEX,MIF_INDEX,M)
     */MII_MAX/3.D0 !calculate the cross-section
  617 CONTINUE
  618 CONTINUE
  612 CONTINUE
      DO 620 M=1,MTET 
      DISC(N,M)=CS_ANG(N,M)
  620 CONTINUE    
  619 CONTINUE 


      RETURN
      END SUBROUTINE DEUTR
C     *******************************************************



C     END of ccrd
C     *******************************************************
C     *******************************************************
C     START of knditd
C     *******************************************************
C     ****************************************************************
      SUBROUTINE KNCOE
C     *******************************************************
      IMPLICIT DOUBLE PRECISION(A-H,O-Z) 
      COMMON /KG/J1,J2,M1,M2,J,M,AKG
     */ENA/EN,EL(40),BET(10),NUR,NMAX,NPD,LAS
      COMMON/MENU/MEJOB,MEPOT,MEHAM,MECHA,MEPRI,MESOL,MESHA,MESHO,MEHAO
     *,MEAPP,MEVOL,MEREL,MECUL,MERZZ,MERRR,MEDIS,MERIP
     */SHEM1/HW,AMG0,AMB0,GAM0,BET0,BET4,GAMDE,GSHAPE
     */SHAMO/BET3,ETO,AMUO,HWO,BB32,DPAR
     */ALL/ALK11(15),ALK21(15),ALK22(15),ALK31(15),ALK32(15)
     *,ALK41(15),ALK42(15),ALK43(15),B0LK1(15),
     *B0LK2(15),B1LK21(15),B1LK22(15),B2LK31(15),B2LK32(15),B2LK33(15)
     */OLL/B0O1(15),B1O21(15),B1O22(15),B2O31(15),B2O32(15),B2O33(15),
     *BO2(15),B0O2(15)
     */TRIGF/TRIG(40,40,14)
     */INRM/AMO,BMO,CMO,BB42,GAMG,DELG
      DIMENSION NLA(5)
      DATA NLA/0,1,3,6,10/
      SQ2=SQRT(2.D0)
      C1=SQ2      
C     SQ32=2.828427D0
      SQ32=SQ2**3
      C95=1.8D0
      CQ95=1.34164D0
      C75=7.D0/5.D0
      CSQ75=SQRT(C75)
      C6325=63.D0/25.D0
      C6325=SQRT(C6325)
      DO 11 I=1,15
      ALK11(I)=0.D0
      ALK21(I)=0.D0
      ALK22(I)=0.D0
      ALK31(I)=0.D0
      ALK32(I)=0.D0
      ALK41(I)=0.D0
      ALK42(I)=0.D0
      ALK43(I)=0.D0
      B0LK1(I)=0.D0
      B0LK2(I)=0.D0
      B1LK21(I)=0.D0
      B1LK22(I)=0.D0
      B2LK31(I)=0.D0
      B2LK32(I)=0.D0
      B2LK33(I)=0.D0
      B0O1(I)=0.D0
      B1O21(I)=0.D0
      B1O22(I)=0.D0
      BO2(I)=0.D0
      B0O2(I)=0.D0
      B2O31(I)=0.D0
      B2O32(I)=0.D0
      B2O33(I)=0.D0
   11 CONTINUE
      IF(MESHA.EQ.1)GO TO 5
      BET42=BET4*BET4
      IF(MESHA.GT.2) GO TO 82
      A40=1.D0
      A42=0.D0
      A44=0.D0
      GO TO 5
   82 C7=7.D0/12.D0
      C5=5.D0/12.D0
      CQ5=SQRT(C5)
      CQ7=SQRT(C7)
      IF(MESHA.EQ.4) CSDG=COS(DELG)
      IF(MESHA.EQ.4) GO TO 7
      GAMG=GAM0
      CSDG=CQ7*COS(3.D0*GAM0)
    7 SSDG=SQRT(1.D0-CSDG*CSDG)
      CSGG=COS(GAMG)
      SSGG=SIN(GAMG)
      A40=CQ7*CSDG+CQ5*SSDG*CSGG
      A42=-SSDG*SSGG/SQ2
      A44=(CQ5*CSDG-CQ7*SSDG*CSGG)/SQ2
    5 ALK11(2)=1.D0
      ALK11(3)=1.D0/SQ2
      IF(LAS.EQ.1) GO TO 21
      DO 15 JJ2=1,3
      JI2=4*(JJ2-1)
      J1=4
      J2=4
      J=JI2
      M1=0
      M2=0
      M=0
      CALL KLEGO
      AK1=AKG
      DO 16 KK2=1,JJ2
      LK=NLA(JJ2)+KK2
      GO TO (17,18,19),KK2
   17 M1=4
      M2=-4
      CALL KLEGO
      AK2=AKG
      ALK21(LK)=AK1*AK1
      ALK22(LK)=AK1*AK2
      GO TO 16
   18 M1=0
      M2=4
      M=4
      CALL KLEGO
      AK4=AKG
      ALK21(LK)=AK1*AK4*SQ2
      GO TO 16
   19 M1=4
      M=8
      CALL KLEGO
      AK3=AKG
      ALK21(LK)=AK1*AK3/2.D0
   16 CONTINUE
      IF(LAS.EQ.2) GO TO 15
      J3M=IABS(JJ2-2)+1
      J3P=JJ2+1
      DO 46 JJ3=J3M,J3P
      JI3=4*(JJ3-1)
      J2=JI2
      M1=0
      M2=0
      J=JI3
      M=0
      CALL KLEGO
      BK1=AKG
      ABK=AK1*BK1
      DO 45 KK3=1,JJ3
      LK=NLA(JJ3)+KK3
      GO TO (36,37,38,39), KK3
   36 M1=-4
      M2=4
      CALL KLEGO
      BK2=AKG
      ALK31(LK)=ALK31(LK)+ABK*AK1*BK1
      ALK32(LK)=ALK32(LK)+ABK*(BK1*AK2+AK4*BK2)
      GO TO 45
   37 M1=0
      M=4
      CALL KLEGO
      BK6=AKG
      M1=4
      M2=0
      CALL KLEGO
      BK7=AKG
      M1=-4
      M2=8
      CALL KLEGO
      BK9=AKG
      ALK31(LK)=ALK31(LK)+ABK*(SQ2*AK4*BK6+AK1*BK7/SQ2)
      ALK32(LK)=ALK32(LK)+ABK*(AK3*BK9/SQ32+AK2*BK7/SQ2)
      GO TO 45
   38 M1=0
      M=8
      CALL KLEGO
      BK5=AKG
      M1=4
      M2=4
      CALL KLEGO
      BK3=AKG
      ALK31(LK)=ALK31(LK)+ABK*(AK3*BK5/2.D0+AK4*BK3)
      GO TO 45
   39 M2=8
      M=12
      CALL KLEGO
      BK8=AKG
      ALK31(LK)=ALK31(LK)+ABK*AK3*BK8
   45 CONTINUE
      IF(LAS.EQ.3) GO TO 46
      J4M=IABS(JJ3-1)+1
      J4P=JJ3+1
      DO 40 JJ4=J4M,J4P
      JI4=4*(JJ4-1)
      J2=JI3
      M1=0
      M2=0
      J=JI4
      M=0
      CALL KLEGO
      CK1=AKG
      ABCK=ABK*CK1
      DO 49 KK4=1,JJ4
      LK=NLA(JJ4)+KK4
      GO TO (50,51,52,53,54),KK4
   50 M1=-4
      M2=4
      CALL KLEGO
      CK2=AKG
      ALK41(LK)=ALK41(LK)+ABCK*CK1*BK1*AK1
      ALK42(LK)=ALK42(LK)+ABCK*(CK1*(BK1*AK2+AK4*BK2)+CK2*
     *(AK4*BK6+BK7*AK1/2.D0))
      ALK43(LK)=ALK43(LK)+ABCK*CK2*(AK3*BK9/4.D0+BK7*AK2/2.D0)
      GO TO 49
   51 M1=0
      M=4
      CALL KLEGO
      CK3=AKG
      M1=4
      M2=0
      CALL KLEGO
      CK4=AKG
      M1=-4
      M2=8
      CALL KLEGO
      CK5=AKG
      ALK41(LK)=ALK41(LK)+ABCK*(CK3*(AK4*BK6*SQ2+
     *BK7*AK1/SQ2)+CK4*BK1*AK1/SQ2)
      ALK42(LK)=ALK42(LK)+ABCK*(CK3*(AK3*BK9/SQ32+BK7*AK2/SQ2)+
     *CK5*(AK3*BK5/SQ32+AK4*BK3/SQ2)+CK4*(BK1*AK2/SQ2+AK4*BK2/SQ2))
      GO TO 49
   52 M1=0
      M=8
      CALL KLEGO
      CK6=AKG
      M1=4
      M2=4
      CALL KLEGO
      CK7=AKG
      M1=-4
      M2=12
      CALL KLEGO
      CK8=AKG
      ALK41(LK)=ALK41(LK)+ABCK*(CK6*(AK3*BK5/2.D0+AK4*BK3)+
     *CK7*(AK4*BK6+BK7*AK1/2.D0))
      ALK42(LK)=ALK42(LK)+ABCK*(CK7*(AK3*BK9/4.D0+BK7*AK2/2.D0)+
     *CK8*AK3*BK8/4.)
      GO TO 49
   53 M1=0
      M=12
      CALL KLEGO
      CK11=AKG
      M1=4
      M2=8
      CALL KLEGO
      CK10=AKG
      ALK41(LK)=ALK41(LK)+ABCK*(CK11*AK3*BK8/SQ32+
     *CK10*(AK3*BK5/SQ32+AK4*BK3/SQ2))
      GO TO 49
   54 M2=12
      M=16
      CALL KLEGO
      CK9=AKG
      ALK41(LK)=ALK41(LK)+ABCK*CK9*AK3*BK8/4.D0
   49 CONTINUE
   40 CONTINUE
   46 CONTINUE
   15 CONTINUE
   21 IF(MESHA.EQ.1) GO TO 60
      B0LK1(4)=A40*BET4
      B0LK1(5)=A42*BET4
      B0LK1(6)=A44*BET4
      IF(LAS.EQ.1) GO TO 60
      DO 61 JJ2=1,5
      JI2=4*(JJ2-1)
      J1=8
      J2=8
      J=JI2
      M1=0
      M2=0
      M=0
      CALL KLEGO
      CK1=AKG
      DO 62 KK2=1,JJ2
      LK=NLA(JJ2)+KK2
      GO TO (63,64,65,66,67),KK2
   63 M1=4
      M2=-4
      CALL KLEGO
      CK2=AKG
      M1=8
      M2=-8
      CALL KLEGO
      CK3=AKG
      B0LK2(LK)=CK1*(A40*A40*CK1+2.D0*A42*A42*CK2+
     *2.D0*A44*A44*CK3)*C95*BET42
      GO TO 62
   64 M1=0
      M2=4
      M=4
      CALL KLEGO
      CK4=AKG
      M1=-4
      M2=8
      CALL KLEGO
      CK5=AKG
      B0LK2(LK)=CK1*2.D0*(A40*A42*CK4+A42*A44*CK5)*C95*BET42
      GO TO 62
   65 M1=4
      M2=4
      M=8
      CALL KLEGO
      CK6=AKG
      M1=0
      M2=8
      CALL KLEGO
      CK7=AKG
      B0LK2(LK)=CK1*(A42*A42*CK6+2.D0*A40*A44*CK7)*C95*BET42
      GO TO 62
   66 M1=4
      M=12
      CALL KLEGO
      CK8=AKG
      B0LK2(LK)=CK1*2.D0*A42*A44*CK8*C95*BET42
      GO TO 62
   67 M1=8
      M=16
      CALL KLEGO
      CK9=AKG
      B0LK2(LK)=CK1*A44*A44*CK9*C95*BET42
   62 CONTINUE
      IF(JJ2.LT.2.OR.JJ2.GT.4) GO TO 61
      J1=4
      J2=8
      M1=0
      M2=0
      M=0
      CALL KLEGO
      BK1=AKG
      DO 68 KK2=1,JJ2
      LK=NLA(JJ2)+KK2
      GO TO (69,70,71,72),KK2
   69 M1=4
      M2=-4
      CALL KLEGO
      BK2=AKG
      B1LK21(LK)=2.D0*BK1*A40*BK1*CQ95*BET4
      B1LK22(LK)=2.D0*BK1*A42*BK2*CQ95*BET4*SQ2
      GO TO 68
   70 M1=0
      M2=4
      M=4
      CALL KLEGO
      BK3=AKG
      M1=4
      M2=0
      CALL KLEGO
      BK4=AKG
      M1=-4
      M2=8
      CALL KLEGO
      BK5=AKG
      B1LK21(LK)=2.D0*BK1*A42*BK3*CQ95*BET4
      B1LK22(LK)=2.D0*BK1*(A40*BK4+A44*BK5)*CQ95*BET4/SQ2
      GO TO 68
   71 M1=0
      M2=8
      M=8
      CALL KLEGO
      BK6=AKG
      M1=4
      M2=4
      CALL KLEGO
      BK7=AKG
      B1LK21(LK)=2.D0*BK1*A44*BK6*CQ95*BET4
      B1LK22(LK)=2.D0*BK1*A42*BK7*CQ95*BET4/SQ2
      GO TO 68
   72 M2=8
      M=12
      CALL KLEGO
      BK8=AKG
      B1LK22(LK)=2.D0*BK1*A44*BK8*CQ95*BET4/SQ2
   68 CONTINUE
   61 CONTINUE
      IF(LAS.EQ.2) GO TO 60
      DO 73 JJ2=1,3
      JMI=IABS(JJ2-3)+1
      JMA=JJ2+2
      JI2=4*(JJ2-1)
      J1=4
      J2=4
      J=JI2
      M1=0
      M2=0
      M=0
      CALL KLEGO
      AK1=AKG
      M1=4
      M2=-4
      CALL KLEGO
      AK2=AKG
      M1=0
      M2=4
      M=4
      CALL KLEGO
      AK3=AKG
      M1=4
      M=8
      CALL KLEGO
      AK4=AKG
      DO 74 JJ3=JMI,JMA
      JI3=4*(JJ3-1)
      J1=JI2
      J2=8
      J=JI3
      M1=0
      M2=0
      M=0
      CALL KLEGO
      DK1=AKG
      DO 75 KK3=1,JJ3
      LK=NLA(JJ3)+KK3
      GO TO (76,77,78,79,80),KK3
   76 M1=4
      M2=-4
      CALL KLEGO
      DK2=AKG
      M1=8
      M2=-8
      CALL KLEGO
      DK3=AKG
      B2LK31(LK)=B2LK31(LK)+3.D0*AK1*DK1*AK1*A40*DK1*CQ95*BET4
      B2LK32(LK)=B2LK32(LK)+3.D0*AK1*DK1*(AK2*A40*DK1+
     *AK4*A44*DK3)*CQ95*BET4
      B2LK33(LK)=B2LK33(LK)+3.D0*AK1*DK1*AK3*A42*DK2*CQ95*BET4*SQ2*2.D0
      GO TO 75
   77 M1=4
      M2=0
      M=4
      CALL KLEGO
      DK4=AKG
      M1=0
      M2=4
      CALL KLEGO
      DK5=AKG
      M1=8
      M2=-4
      CALL KLEGO
      DK6=AKG
      M1=-4
      M2=8
      CALL KLEGO
      DK7=AKG
      B2LK31(LK)=B2LK31(LK)+3.D0*AK1*DK1*AK1*A42*DK5*CQ95*BET4
      B2LK32(LK)=B2LK32(LK)+3.D0*AK1*DK1*(AK2*A42*DK5+
     *AK4*A42*DK6/2.)*CQ95*BET4
      B2LK33(LK)=B2LK33(LK)+3.D0*AK1*DK1*(AK3*A40*DK4+
     *AK3*A44*DK7)*CQ95*BET4*SQ2
      GO TO 75
   78 M1=8
      M2=0
      M=8
      CALL KLEGO
      DK8=AKG
      M1=4
      M2=4
      CALL KLEGO
      DK9=AKG
      M1=0
      M2=8
      CALL KLEGO
      DK10=AKG
      B2LK31(LK)=B2LK31(LK)+3.D0*AK1*DK1*AK1*A44*DK10*CQ95*BET4
      B2LK32(LK)=B2LK32(LK)+3.D0*AK1*DK1*(AK3*A44*DK10+
     *AK4*A40*DK8/2.D0)*CQ95*BET4
      B2LK33(LK)=B2LK33(LK)+3.D0*AK1*DK1*AK3*A42*DK9*CQ95*BET4*SQ2
      GO TO 75
   79 M1=8
      M2=4
      M=12
      CALL KLEGO
      DK11=AKG
      M1=4
      M2=8
      CALL KLEGO
      DK12=AKG
      B2LK32(LK)=B2LK32(LK)+3.D0*AK1*DK1*AK4*A42*DK11*CQ95*BET4/2.D0
      B2LK33(LK)=B2LK33(LK)+3.D0*AK1*DK1*AK3*A44*DK12*CQ95*BET4*SQ2
      GO TO 75
   80 M1=8
      M=16
      CALL KLEGO
      DK13=AKG
      B2LK32(LK)=B2LK32(LK)+3.D0*AK1*DK1*AK4*A44*DK13*CQ95*BET4/2.D0
   75 CONTINUE
   74 CONTINUE
   73 CONTINUE
   60 IF(MESHO.EQ.0) GO TO 2
      GO TO (1,6),MESHO
    1 A30=1.D0
      A32=0.D0
      GO TO 8
    6 A30=COS(ETO)
      A32=SIN(ETO)/C1
    8 B0O1(2)=A30
      B0O1(3)=A32
      IF(LAS.EQ.1) GO TO 2
      DO 9 J32=1,4
      IF(J32.EQ.4) GO TO 10
      JI32=4*(J32-1)+2
      J1=4
      J2=6
      J=JI32
      M1=0
      M2=0
      M=0
      CALL KLEGO
      OK1=AKG
      DO 20 K32=1,J32
      LK=NLA(J32)+K32
      GO TO (22,23,24),K32
   22 M1=4
      M2=-4
      CALL KLEGO
      OK2=AKG
      B1O21(LK)=2.D0*OK1**2*A30*CSQ75
      B1O22(LK)=OK1*OK2*C1*A32*CSQ75*2.D0
      GO TO 20
   23 M2=0
      M=4
      CALL KLEGO
      OK3=AKG
      M1=0
      M2=4
      CALL KLEGO
      OK4=AKG
      B1O21(LK)=2.D0*OK1*OK4*A32*CSQ75
      B1O22(LK)=OK1*OK3*C1*A30*CSQ75
      GO TO 20
   24 M1=4
      M=8
      CALL KLEGO
      OK5=AKG
      B1O22(LK)=OK1*OK5*C1*A32*CSQ75
   20 CONTINUE
   10 IF(MESHA.EQ.1) GO TO 55
      J1=6
      J1=8
      J=JI32
      M1=0
      M2=0
      M=0
      CALL KLEGO
      OL1=AKG
      DO 26 K34=1,J32
      LK=NLA(J32)+K34
      GO TO (27,28,29,30),K34
   27 M1=4
      M2=-4
      CALL KLEGO
      OL2=AKG
      B0O2(LK)=2.D0*OL1*(OL1*A30*A40+2.D0*OL2*A32*A42)*C6325*BET4
      GO TO 26
   28 M2=0
      M=4
      CALL KLEGO
      OL3=AKG
      M1=0
      M2=4
      CALL KLEGO
      OL5=AKG
      M1=-4
      M2=8
      CALL KLEGO
      OL4=AKG
      B0O2(LK)=2.D0*OL1*(OL3*A32*A40+OL5*A30*A42+OL4*A32*A44)*C6325
     **BET4
      GO TO 26
   29 M1=4
      M2=4
      M=8
      CALL KLEGO
      OL6=AKG
      M1=0
      M2=8
      CALL KLEGO
      OL7=AKG
      B0O2(LK)=2.D0*OL1*(OL6*A32*A42+OL7*A30*A44)*C6325*BET4
      GO TO 26
   30 M2=4
      M=12
      CALL KLEGO
      OL8=AKG
      B0O2(LK)=2.D0*OL1*(OL8*A32*A44)*C6325*BET4
   26 CONTINUE
   55 JI33=4*(J32-1)
      J1=6
      J2=6
      J=JI33
      M1=0
      M2=0
      M=0
      CALL KLEGO
      OM1=AKG
      DO 31 K33=1,J32
      LK=NLA(J32)+K33
      GO TO (32,33,34,31),K33
   32 M1=4
      M2=-4
      CALL KLEGO
      OM2=AKG
      BO2(LK)=OM1*(A30**2*OM1+2.D0*OM2*A32**2)*C75
      GO TO 31
   33 M1=0
      M2=4
      M=4
      CALL KLEGO
      OM3=AKG
      BO2(LK)=OM1*A30*A32*2.D0*OM3*C75
      GO TO 31
   34 M1=4
      M=8
      CALL KLEGO
      OM4=AKG
      BO2(LK)=OM1*A32**2*OM4*C75
   31 CONTINUE
      IF(LAS.EQ.2) GO TO 9
      IF(J32.EQ.4) GO TO 2
      J1=4
      J2=4
      J=4*(J32-1)
      M1=0
      M2=0
      M=0
      CALL KLEGO
      ON1=AKG
      M1=4
      M2=-4
      CALL KLEGO
      ON2=AKG
      M1=0
      M2=4
      M=4
      CALL KLEGO
      ON3=AKG
      M1=4
      M=8
      CALL KLEGO
      ON4=AKG
      JMI=1
      JMA=J32+1
      IF(J32.EQ.1) JMI=2
      DO 35 J223=JMI,JMA
      JI223=4*(J223-1)+2
      J1=4*(J32-1)
      J2=6
      J=JI223
      M1=0
      M2=0
      M=0
      CALL KLEGO
      OM1=AKG
      DO 41 K223=1,J223
      LK=NLA(J223)+K223
      GO TO (42,43,44,48),K223
   42 M1=4
      M2=-4
      CALL KLEGO
      OM2=AKG
      B2O31(LK)=B2O31(LK)+ON1*OM1*ON1*OM1*A30*3.D0*CSQ75
      B2O32(LK)=B2O32(LK)+ON1*OM1*A30*ON2*OM1*3.D0*CSQ75
      B2O33(LK)=B2O33(LK)+ON1*OM1*A32*ON3*OM2*C1*3.D0*CSQ75
      GO TO 41
   43 M1=4
      M2=0
      M=4
      CALL KLEGO
      OM3=AKG
      M1=0
      M2=4
      CALL KLEGO
      OM4=AKG
      M1=8
      M2=-4
      CALL KLEGO
      OM5=AKG
      B2O31(LK)=B2O31(LK)+ON1*OM1*ON1*OM4*A32*3.D0*CSQ75
      B2O32(LK)=B2O32(LK)+ON1*OM1*(ON2*OM4+ON4*OM5/2.D0)*A32*3.D0*CSQ75
      B2O33(LK)=B2O33(LK)+ON1*OM1*A30*ON3*OM3/C1*3.D0*CSQ75
      GO TO 41
   44 M2=0
      M=8
      CALL KLEGO
      OM6=AKG
      M1=4
      M2=4
      CALL KLEGO
      OM7=AKG
      B2O31(LK)=B2O31(LK)+ON1*OM1*ON1*OM7*A32*3.D0*CSQ75
      B2O32(LK)=B2O32(LK)+ON1*OM1*(ON2*OM7*A32+ON4*OM6*A30/2.D0)*3.D0*
     *CSQ75
      GO TO 41
   48 M1=8
      M=12
      CALL KLEGO
      OM8=AKG
      B2O32(LK)=B2O32(LK)+ON1*OM1*ON4*OM8*A32*1.5D0*CSQ75
   41 CONTINUE
   35 CONTINUE
    9 CONTINUE
    2 CONTINUE
      RETURN
      END
C     *******************************************************
      SUBROUTINE KNDIT
C     *******************************************************
      IMPLICIT DOUBLE PRECISION(A-H,O-Z) 
C     BELOW CARD IS NO NECESSARY IF MEMORY IS MORE THAN 32Mb
      DOUBLE PRECISION PVC,PRC
      COMMON/LNP/JSS,NPIS,NPIO,JSO,NN1,LNO(180),NS1(180),JNO(180),NSPI
      COMMON/QNB/JO(40),NPO(40),KO(40),NCA(40)
     */QNBBAND/NUMB(40),BETB(40)                                  
      COMMON/RACB/JA,JB,JC,JD,JE,JF,W
      COMMON /KG/J1,J2,M1,M2,J,M,AKG
     */ENA/EN,EL(40),BET(10),NUR,NMAX,NPD,LAS
      COMMON/CV/CVNR(160000)
      COMMON/CVPN/CVNRPN(40000)
      COMMON/JNN/CSS,INCC,NCLL,NSS,NJ,INCR
      COMMON/QNS1/LNJ1(250),JNJ1(250),NNJ1(250),KPJ1(250)
      COMMON/MENU/MEJOB,MEPOT,MEHAM,MECHA,MEPRI,MESOL,MESHA,MESHO,MEHAO
     *,MEAPP,MEVOL,MEREL,MECUL,MERZZ,MERRR,MEDIS,MERIP
     */SHEM1/HW,AMG0,AMB0,GAM0,BET0,BET4,GAMDE,GSHAPE
     */SHEMM/ES(40),JJ(40),NTU(40),NNB(40),NNG(40),NNO(40),
     *NPI(40)
     */SHEM2/GIT(40),EP0(40),EP1(40),EP2(40),EP12(40),ANG(40,2),EPB(40),
     *EPG(40),EGB(40),AIT(31,16),PT(40),FOG(2,8,8),XI(40),ANB(40),
     *CD(40,2)
     */OVKN/FOV(40,40,4),CG1,SG1,CG2,SG2,CSG11,CG4,SG4,CSG22,CSG31,
     *CSG13,CG3,SG3,CSG21,CSG12
     */ALL/ALK11(15),ALK21(15),ALK22(15),ALK31(15),ALK32(15)
     *,ALK41(15),ALK42(15),ALK43(15),B0LK1(15),
     *B0LK2(15),B1LK21(15),B1LK22(15),B2LK31(15),B2LK32(15),B2LK33(15)
     */OLL/B0O1(15),B1O21(15),B1O22(15),B2O31(15),B2O32(15),B2O33(15),
     *BO2(15),B0O2(15)
     */TRIGF/TRIG(40,40,14)
     */INRM/AMO,BMO,CMO,BB42,GAMG,DELG
     */SHAMO/BET3,ETO,AMUO,HWO,BB32,DPAR
     */FOK12/FOK(6,6,2)
     */ABCOUL/PVC(5400),PRC(5400),CVNC(720000)
      COMMON/RAD/RR,RC,RD,RW,RS,AR,AC,AW,AD,AS,ALF,AT,ANEU,RZ,ZNUC,ASP
     *,AZ
     */ALFA/AIGS(40),AGSIC(40)

      DIMENSION NLA(5)
      DATA NLA/0,1,3,6,10/
      NSPIP=IDINT(ASP*2.D0+0.01)
      SQ2=SQRT(2.D0)
C     SQ32=2.828427D0
      SQ32=SQ2**3
      C95=1.8D0
      CQ95=1.34164D0
      IILL=NCLL*NCLL*2*9
      NCLLX2=NCLL*NCLL
      NCLL4=NCLLX2*4
      DO 4 I=1,IILL
      IF(I.LE.NCLLX2) CVNRPN(I)=0.D0
      IF(I.LE.NCLL4) CVNR(I)=0.D0
    4 CVNC(I)=0.D0
      LAS2=LAS
      ICLL=NCLL*9*LAS
      LAS8=LAS*9
          LASC=1
          LAS8=9
          IF(LAS2.GE.2) LASC=2
          IF(LAS2.GE.2) LAS8=18
          IF(MEHAM.EQ.1) GO TO 10
      ICLL=NCLL*LAS8
      NCLA=NCLL*LAS2
      DO 20 K=1,NCLL
      K1=(K-1)*NCLA
      KI1=(K-1)*LAS2
      K1PN=(K-1)*NCLL
      KC1=ICLL*(K-1)
      KIC1=LAS8*(K-1)
      NU1=NNJ1(K)
      JO1=JO(NU1)
      LN1=LNJ1(K)*2
      JN1=JNJ1(K)
      NPI1=(-1)**NNO(NU1)
      JO12=JO1/2
      KMI1=1-(-1)**JO12*NPI1
      KMA1=2*(JO12/2)
      NK1=(KMA1-KMI1+2)/2
      DO 20 KK=K,NCLL
      K2=K1+(KK-1)*LAS2
      K2PN=K1PN+KK
      K2NP=(KK-1)*NCLL+K
      KI2=KI1+(KK-1)*NCLA
      KC2=KC1+(KK-1)*LAS8
      KIC2=KIC1+(KK-1)*ICLL
      NU2=NNJ1(KK)
      JO2=JO(NU2)
      LN2=LNJ1(KK)*2
      JN2=JNJ1(KK)
      JO22=JO2/2
      NPI2=(-1)**NNO(NU2)
      KMI2=1-(-1)**JO22*NPI2
      KMA2=2*(JO22/2)
      NK2=(KMA2-KMI2+2)/2
      AA=(-1)**((JSS-JO2-NSPIP+LN1+LN2)/2+(LN2-LN1)/4)*SQRT((LN1+1.D0)*
     *(LN2+1.D0)*(JN1+1.D0)*(JN2+1.D0)*(JO1+1.D0)/12.56663708D0)
      NPCV=1
      IF(LN2-LN1.LT.0) NPCV=-1
      FOK1=FOK(NNO(NU1)+1,NNO(NU2)+1,1)
      FOK2=FOK(NNO(NU1)+1,NNO(NU2)+1,2)
      IF(BET3.NE.0.) FOK1=FOK1*BET3
      IF(BET3.NE.0.) FOK2=FOK2*BET3**2
      IF(BET3.EQ.0.) FOK1=FOK1*AMUO
      IF(BET3.EQ.0.) FOK2=FOK2*AMUO**2
      CG1=TRIG(NU1,NU2,1)
      SG1=TRIG(NU1,NU2,2)
      CG2=TRIG(NU1,NU2,3)
      SG2=TRIG(NU1,NU2,4)
      CSG11=TRIG(NU1,NU2,5)
      CG3=TRIG(NU1,NU2,6)
      SG3=TRIG(NU1,NU2,7)
      CSG21=TRIG(NU1,NU2,8)
      CSG12=TRIG(NU1,NU2,9)
      CG4=TRIG(NU1,NU2,10)
      SG4=TRIG(NU1,NU2,11)
      CSG22=TRIG(NU1,NU2,12)
      CSG31=TRIG(NU1,NU2,13)
      CSG13=TRIG(NU1,NU2,14)
      FOIJ1=FOV(NU1,NU2,1)*BET0
      IF(LAS2.GE.2) FOIJ2=FOV(NU1,NU2,2)*BET0**2
      IF(LAS2.GE.3) FOIJ3=FOV(NU1,NU2,3)*BET0**3
      DO 22 L=1,LAS2
      AAA=AA*FOV(NU1,NU2,L)*BET0**L
      LL2=K2+L
      LI2=KI2+L
      LLC2=KC2+L
      LIC2=KIC2+L
      SU1=0.
      DO 23 LA=1,5
C     NEXT TWO CARDS JUST TO OVERCOME THE ERROR OR  MICROSOFT FPS COMPILER
C     v.1.00, YOU CAN DELETE THEM USING OTHER COMPILERS
      IF(LA.GT.5 .AND. MEPRI.LT.99) PRINT 9911, LA
 9911 FORMAT (I3)
      IF(L.EQ.1.AND.LA.NE.2) GO TO 83
      IF(L.EQ.2.AND.LA.GE.4) GO TO 83
      IF(L.EQ.3.AND.LA.GE.5) GO TO 83
      GO TO 84
   83 IF(MESHA.LT.1) GO TO 3
      IF(L.EQ.1.AND.LA.NE.3) GO TO 3
      GO TO 84
    3 IF(MESHO.EQ.0) GO TO 23
      IF(L.EQ.1.AND.LA.NE.2) GO TO 23
      IF(LA.GT.4) GO TO 23
   84 LAM=4*(LA-1)
      SU2=0.
      DO 25 N=1,LA
      ALK=0.
      LK=NLA(LA)+N
      IF(NPI1.NE.NPI2) GO TO 11
      GO TO (27,28,29,30),L
   27 GO TO (31,32),N
   31 ALK=ALK11(LK)*CG1
      IF(MESHA.LE.1) GO TO 33
      ALK=ALK+B0LK1(LK)/FOIJ1
      GO TO 33
   32 ALK=ALK11(LK)*SG1
      IF(MESHA.LE.1) GO TO 33
      ALK=ALK+B0LK1(LK)/FOIJ1
      GO TO 33
   28 GO TO (34,35,48),N
   34 ALK=ALK21(LK)*CG2+ALK22(LK)*SG2
      IF(MESHA.LE.1) GO TO 33
      ALK=ALK+B0LK2(LK)/FOIJ2+(B1LK21(LK)*CG1+B1LK22(LK)*SG1)*
     *FOIJ1/FOIJ2
      IF(MESHO.EQ.0) GO TO 33
      IF(MEHAO.EQ.2) GO TO 61
      ALK=ALK+BO2(LK)*FOK2/FOIJ2
      GO TO 33
   61 ALK=ALK+BO2(LK)*FOK2
      GO TO 33
   35 ALK=ALK21(LK)*CSG11
      IF(MESHA.LE.1) GO TO 33
      ALK=ALK+B0LK2(LK)/FOIJ2+(B1LK21(LK)*CG1+B1LK22(LK)*SG1)*
     *FOIJ1/FOIJ2
      IF(MESHO.EQ.0) GO TO 33
      IF(MEHAO.EQ.2) GO TO 62
      ALK=ALK+BO2(LK)*FOK2/FOIJ2
      GO TO 33
   62 ALK=ALK+BO2(LK)*FOK2
      GO TO 33
   48 ALK=ALK21(LK)*SG2
      IF(MESHA.LE.1) GO TO 33
      ALK=ALK+B0LK2(LK)/FOIJ2+(B1LK21(LK)*CG1+B1LK22(LK)*SG1)*
     *FOIJ1/FOIJ2
      IF(MESHO.EQ.0) GO TO 33
      IF(MEHAO.EQ.2) GO TO 63
      ALK=ALK+BO2(LK)*FOK2/FOIJ2
      GO TO 33
   63 ALK=ALK+BO2(LK)*FOK2
      GO TO 33
   29 GO TO (41,42,43,44),N
   41 ALK=ALK31(LK)*CG3+ALK32(LK)*CSG12
      IF(MESHA.LE.1) GO TO 33
      ALK=ALK+(B2LK31(LK)*CG2+B2LK32(LK)*SG2+B2LK33(LK)*CSG11)*
     *FOIJ2/FOIJ3
      GO TO 33
   42 ALK=ALK31(LK)*CSG21+ALK32(LK)*SG3
      IF(MESHA.LE.1) GO TO 33
      ALK=ALK+(B2LK31(LK)*CG2+B2LK32(LK)*SG2+B2LK33(LK)*CSG11)*
     *FOIJ2/FOIJ3
      GO TO 33
   43 ALK=ALK31(LK)*CSG12
      IF(MESHA.LE.1) GO TO 33
      ALK=ALK+(B2LK31(LK)*CG2+B2LK32(LK)*SG2+B2LK33(LK)*CSG11)*
     *FOIJ2/FOIJ3
      GO TO 33
   44 ALK=ALK31(LK)*SG3
      IF(MESHA.LE.1) GO TO 33
      ALK=ALK+(B2LK31(LK)*CG2+B2LK32(LK)*SG2+B2LK33(LK)*CSG11)*
     *FOIJ2/FOIJ3
   30 GO TO (55,56,57,58,59),N
   55 ALK=ALK41(LK)*CG4+ALK42(LK)*CSG22+ALK43(LK)*SG4
      GO TO 33
   56 ALK=ALK41(LK)*CSG31+ALK42(LK)*CSG13
      GO TO 33
   57 ALK=ALK41(LK)*CSG22+ALK42(LK)*SG4
      GO TO 33
   58 ALK=ALK41(LK)*CSG13
      GO TO 33
   59 ALK=ALK41(LK)*SG4
      GO TO 33
   11 LAM=4*(LA-1)+2
      IF(MESHO.EQ.0.OR.MEHAO.EQ.0) GO TO 33
      GO TO (16,17,18,23),L
   16 GO TO (5,7),N
    5 IF(MEHAO.EQ.2) GO TO 64
      ALK=B0O1(LK)*FOK1/FOIJ1
      GO TO 33
   64 ALK=B0O1(LK)*FOK1
      GO TO 33
    7 IF(MEHAO.EQ.2) GO TO 65
      ALK=B0O1(LK)*FOK1/FOIJ1
      GO TO 33
   65 ALK=B0O1(LK)*FOK1
      GO TO 33
   17 GO TO (36,37,38,39),N
   36 IF(MEHAO.EQ.2) GO TO 66
      ALK=((B1O21(LK)*CG1+B1O22(LK)*SG1)*FOIJ1+
     *B0O2(LK))/FOIJ2*FOK1
      GO TO 33
   66 ALK=((B1O21(LK)*CG1+B1O22(LK)*SG1)*FOIJ2+
     *B0O2(LK)*FOIJ1)/FOIJ2*FOK1                                        
      GO TO 33
   37 IF(MEHAO.EQ.2) GO TO 67
      ALK=((B1O21(LK)*CG1+B1O22(LK)*SG1)*FOIJ1+
     *B0O2(LK))/FOIJ2*FOK1                                              
      GO TO 33
   67 ALK=((B1O21(LK)*CG1+B1O22(LK)*SG1)*FOIJ2+
     *B0O2(LK)*FOIJ1)/FOIJ2*FOK1                                        
      GO TO 33
   38 IF(MEHAO.EQ.2) GO TO 68
      ALK=((B1O21(LK)*CG1+B1O22(LK)*SG1)*FOIJ1+
     *B0O2(LK))/FOIJ2*FOK1                                              
      GO TO 33
   68 ALK=((B1O21(LK)*CG1+B1O22(LK)*SG1)*FOIJ2+
     *B0O2(LK)*FOIJ1)/FOIJ2*FOK1                                        
      GO TO 33
   39 IF(MEHAO.EQ.2) GO TO 69
      ALK=((B1O21(LK)*CG1+B1O22(LK)*SG1)*FOIJ1+
     *B0O2(LK))/FOIJ2*FOK1                                              
      GO TO 33
   69 ALK=((B1O21(LK)*CG1+B1O22(LK)*SG1)*FOIJ2+
     *B0O2(LK)*FOIJ1)/FOIJ2*FOK1                                        
      GO TO 33
   18 GO TO (50,51,52,53),N
   50 IF(MEHAO.EQ.2) GO TO 70
      ALK=(B2O31(LK)*CG2+B2O32(LK)*SG2+B2O33(LK)*CSG11*2.D0)*
     *FOK1*FOIJ2/FOIJ3
      GO TO 33
   70 ALK=(B2O31(LK)*CG2+B2O32(LK)*SG2+B2O33(LK)*CSG11*2.D0)*
     *FOK1
      GO TO 33
   51 IF(MEHAO.EQ.2) GO TO 71
      ALK=(B2O31(LK)*CG2+B2O32(LK)*SG2+B2O33(LK)*CSG11*2.D0)*
     *FOK1*FOIJ2/FOIJ3
      GO TO 33
   71 ALK=(B2O31(LK)*CG2+B2O32(LK)*SG2+B2O33(LK)*CSG11*2.D0)*
     *FOK1
   52 IF(MEHAO.EQ.2) GO TO 72
      ALK=(B2O31(LK)*CG2+B2O32(LK)*SG2+B2O33(LK)*CSG11*2.D0)*
     *FOK1*FOIJ2/FOIJ3
      GO TO 33
   72 ALK=(B2O31(LK)*CG2+B2O32(LK)*SG2+B2O33(LK)*CSG11*2.D0)*
     *FOK1
      GO TO 33
   53 IF(MEHAO.EQ.2) GO TO 73
      ALK=(B2O31(LK)*CG2+B2O32(LK)*SG2+B2O33(LK)*CSG11*2.D0)*
     *FOK1*FOIJ2/FOIJ3
      GO TO 33
   73 ALK=(B2O31(LK)*CG2+B2O32(LK)*SG2+B2O33(LK)*CSG11*2.D0)*
     *FOK1
   33 IF(ALK.EQ.0.D0) GO TO 25
      SU=0.
      J1=LN1
      J2=LN2
      J=LAM
      M1=0
      M2=0
      M=0
      CALL KLEGO
      AB=AKG
      JA=JN1
      JB=JO1
      JE=JN2
      JD=JO2
      JC=JSS
      JF=LAM
      CALL RACAH
      AB=AB*W
      JA=LN1
      JB=JN1
      JE=LN2
      JD=JN2
      JC=NSPIP
      JF=LAM
      CALL RACAH
      AB=AB*W
      DO 24 LK1=1,NK1
      KS1=2*(KMI1+2*(LK1-1))
      AIT1=AIT(NU1,LK1)
      IF(KS1.EQ.0) AIT1=AIT1/SQ2
      DO 24 LK2=1,NK2
      KS2=2*(KMI2+2*(LK2-1))
      AIT2=AIT(NU2,LK2)
      IF(KS2.EQ.0) AIT2=AIT2/SQ2
      KA=4*(N-1)
      J1=JO1
      J2=LAM
      J=JO2
      M1=KS1
      M2=KA
      M=KS2
      CALL KLEGO
      CK1=AKG
      M1=-KS1
      CALL KLEGO
      CLK=CK1+(-1)**JO12*AKG*NPI1
      IF(KA.EQ.0) GO TO 26
      M1=KS1
      M2=-KA
      CALL KLEGO
      CLK=CLK+AKG
   26 SU=SU+CLK*AIT1*AIT2
   24 CONTINUE
      SU2=SU2+SU*ALK
   25 CONTINUE
      IF(L.EQ.2) SU2=SU2*1.4104730D0/SQRT(LAM+1.D0)
      IF(L.EQ.3) SU2=SU2*0.8897014D0/SQRT(LAM+1.D0)
      IF(L.EQ.4) SU2=SU2*0.5612105D0/SQRT(LAM+1.D0)
      SU1=SU1+SU2*AB
      IF(L.GT.2) GO TO 23
      LLLC2=LLC2+(LAM/2)*LASC
      LLIC2=LIC2+(LAM/2)*LASC
      CVNC(LLLC2)=SU2*AB*AAA
C     CVNC(LLLC2)=0.
      IF(KK.NE.K) CVNC(LLIC2)=CVNC(LLLC2)
      IF(NPI1.NE.NPI2) CVNC(LLLC2)=CVNC(LLLC2)*NPCV
      IF(NPI1.NE.NPI2)CVNC(LLIC2)=-CVNC(LLLC2)
C      if(lam/2.eq.0) write (21,9999)jo1,jo2,l,kc2,k,kk,llc2,cvnc(lllc2)
C9999 format(7i6,e12.4)
   23 CONTINUE
      CVNR(LL2)=SU1*AAA
      IF(KK.NE.K) CVNR(LI2)=CVNR(LL2)
      IF(NPI1.NE.NPI2) CVNR(LL2)=CVNR(LL2)*NPCV
      IF(NPI1.NE.NPI2)CVNR(LI2)=-CVNR(LL2)
      
      
 
   22 CONTINUE
   20 CONTINUE


      DO 115 K=1,NCLL
      K1PN=(K-1)*NCLL
      NU=NNJ1(K)
      JO1=JO(NU)
      LN1=LNJ1(K)*2
      NPI1=(-1)**NNO(NU)
      DO 115 KK=1,NCLL
      K2PN=K1PN+KK
      NU1=NNJ1(KK)
      JO2=JO(NU1)
      LN2=LNJ1(KK)*2
      NPI2=(-1)**NNO(NU1)
      CVNRPN(K2PN)=0.D0

      IF(JO1.EQ.JO2.AND.NCA(NU).NE.NCA(NU1).AND.NTU(NU).EQ.NTU(NU1)
     *.AND.NNB(NU).EQ.NNB(NU1).AND.NNG(NU).EQ.NNG(NU1)
     *.AND.NNO(NU).EQ.NNO(NU1))GO TO 121
      GO TO 115
  121 CVNRPN(K2PN)=1.D0
      NPCV=1
C     IF(LN2-LN1.LT.0) NPCV=-1
C     IF(NPI1.NE.NPI2) CVNRPN(K2PN)=CVNRPN(K2PN)*NPCV
C     IF(NPI1.NE.NPI2)CVNRPN(K2PN)=-CVNRPN(K2PN)
  
  
  115 CONTINUE




      GO TO 47
   10 JC=JSS
      LAS2=LAS/2
      NCLA=NCLL*LAS2
      BTGS=BET(2)
      DO 1 K=1,NCLL
      K1=(K-1)*NCLA
      K1PN=(K-1)*NCLL
      NU=NNJ1(K)
      KO1=KO(NU)
      JO1=JO(NU)
      LN1=LNJ1(K)
      J1=JNJ1(K)
      BT1=BETB(NU)
      NUMB1=NUMB(NU)
      IF(NUMB1.EQ.0 )BT1=BET(2)
      ALFA1=1.D0
      IF(JO(1)/2*2.NE.JO(1)) ALFA1=AIGS(NU)
      JN1=J1
      JA=J1
      JB=JO1
      DO 2 KK=1,NCLL
      K2=K1+(KK-1)*LAS2
      K2PN=K1PN+KK
      IF(NPD.EQ.0) GO TO 2
      LN2=LNJ1(KK)
      J2=JNJ1(KK)
      JN2=J2
      JE=J2
      CC=SQRT((J1+1)*(J2+1)/12.566371D0)
      NU1=NNJ1(KK)
      BT2=BETB(NU1)
      NUMB2=NUMB(NU1)
      IF(NUMB2.EQ.0 )BT2=BET(2)
      ALFA2=1.D0
      IF(JO(1)/2*2.NE.JO(1)) ALFA2=AIGS(NU1)
      B12BGS=SQRT(BT1*BT2/BTGS**2)
      B12BGS=BT1*BT2/BTGS**2
      IF(NUMB1.NE.NUMB2) B12BGS=B12BGS*ALFA1*ALFA2
      KO2=KO(NU1)                         
                                
      JO2=JO(NU1)
      JD=JO2
      JPS=JSS-1-JO2+J1+J2+LN2-LN1
      JPS=JPS-JPS/4*4
      IF(JPS.NE.0) GO TO 6
      CCC=CC
      GO TO 8
    6 CCC=-CC
    8 DO 9 L=1,LAS2
    
      LL2=K2+L 

      M1=-1
      M2=1
      LAM=2*L
      IF(NPO(NU).NE.NPO(NU1))LAM=LAM+1

      
      J=LAM*2
      M=0
      CALL KLEGO
      BKG=AKG
      

 
      IF(NUMB1.EQ.NUMB2) GO TO 125 
  
      IF(L.GT.1) GO TO 140
      IF(KO1.EQ.KO2) GO TO 125
   
      IF(JO(1)/2*2.NE.JO(1)) GO TO 200

C     COUPLING FOR EVEN-EVEN NUCLIDES
C     NON-AXIAL BAND LEVELS
      
C     JOSES'S FORMULAS
      IF(KO1.EQ.4.AND.KO2.EQ.0) GO TO 131
      IF(KO1.EQ.0.AND.KO2.EQ.4) GO TO 130
  131 J1=JO1
      J2=2*LAM
      J=JO2
      M1=4
      M2=-4
      M=0
      CALL KLEGO
      AKG=AKG*SQRT(J1+1.D0)*(-1.D0)**(J1/2)       
 
      GO TO 129
      
         
      
  130 J1=JO2
      J2=2*LAM
      J=JO1
      M1=4
      M2=-4
      M=0
      CALL KLEGO
      AKG=AKG*SQRT(J1+1.D0)
         
      GO TO 129   
C     END JOSES'S FORMULAS FOR EVEN-EVEN CASE

  200 CONTINUE
C     COUPLING FOR ODD NUCLIDES, NON GS STATE LEVELS WITH GS ONES 

C      IF(KO1.EQ.KO(1).OR.KO2.EQ.KO(1))  GO TO 201
      IF(NUMB1.EQ.0.OR. NUMB2.EQ.0) GO TO 201
      
      GO TO 140
  201 CONTINUE
      MUBAND=IABS(KO(1)-KO1)
      IF(KO(1)-KO1.EQ.0) MUBAND=IABS(KO(1)-KO2)
C     IF(MUBAND.EQ.2.OR.MUBAND.GT.6) GO TO 140
      IF(MUBAND.GT.6) GO TO 140     
      IF(MUBAND.EQ.0) PAUSE 11
      J1=JO2
      J2=2*LAM
      J=JO1
      M1=KO2
      M2=MUBAND
      M=KO1
      CALL KLEGO
      AKGS=AKG
      M1=-KO2
      CALL KLEGO
      AKGS=AKGS+(-1)**((J02-1)/2)*AKG
      M=-KO1
      CALL KLEGO
      AKGS=AKGS+(-1)**((JO1-JO2)/2)*AKG 
      M=KO2
      CALL KLEGO
      AKGS=AKGS+(-1)**((JO1-1)/2)*AKG
      
      AKG=AKGS*SQRT(J1+1.D0)/2.D0
      GO TO 129
 
      
C     END OF JOSE'S FORMULA ODD CASE
 
      
 

C    AXIAL ROTATOR COUPLING, WITH NORMALIZATION ACCOUNTING 
C    EFFECTIVE DEFORMATIONS OF OTHER BANDS 
      
      
 125  J1=JO2
      J2=2*LAM
      M1=KO(NU1)
      M2=0
      M=KO1
      J=JO1
      CALL KLEGO
      AKG=AKG*SQRT(J1+1.D0)
 129  JF=LAM*2
      CALL RACAH
      CVNR(LL2)=CCC*BKG*AKG*W
C     IF(NPO(NU).EQ.-1. AND.NPO(NU1).EQ.-1) GO TO 144
      SCALE=B12BGS**L
      IF(NUMB1.EQ.NUMB2) SCALE=1.D0
      CVNR(LL2)=CVNR(LL2)*SCALE
      

 
      
C     NECESSARY TO DIVIDE BY SIN(GAMMA)/SQRT(2.)!!! for actinides
            
C 144  IF(KO1.EQ.4.AND.KO2.EQ.4) CVNR(LL2)=CVNR(LL2)/0.1D0                 !!!!!!

      GO TO 141
  140 CVNR(LL2)=0.D0
  141 J1=JN1
      J2=JN2
    9 CONTINUE
    
 
  124 IF(JO1.EQ.JO2.AND.NCA(NU).NE.NCA(NU1)) GO TO 135
      GO TO 2
  135 IF(NUMB1.EQ.0. AND. NUMB2.EQ.0) GO TO 101
      GO TO 2

  101 CC=SQRT((J1+1)*(J2+1)/12.566371D0)
      NU1=NNJ1(KK)
      JO2=JO(NU1)
      JD=JO2
      JPS=JSS-1-JO2+J1+J2+LN2-LN1
      JPS=JPS-JPS/4*4
      IF(JPS.NE.0) GO TO 102
      CCC=CC
      GO TO 21
  102 CCC=-CC       
   21 M1=-1
      M2=1
      LAM=0
      J=0
      M=0
      CALL KLEGO
      BKG=AKG
      J1=JO2
      J2=2*LAM
      M1=KO(NU1)
      M2=0
      M=KO1
      J=JO1
      CALL KLEGO
      AKG=AKG*SQRT(J1+1.D0)
      JF=0
      CALL RACAH
      CVNRPN(K2PN)=CCC*BKG*AKG*W
      J1=JN1
      J2=JN2
C     WRITE(21,'(1X,8I8,1X,6(f8.4,1X))') NCLL,K,KK,KL, JO1,JO2,NCA(NU)
C     *,NCA(NU1)
C     *,CVNRPN(K2PN)
            
    2 CONTINUE
    1 CONTINUE
   47 CONTINUE
      RETURN
      END
C     *****************************************************************
      SUBROUTINE PREQU
C     *****************************************************************
      IMPLICIT DOUBLE PRECISION(A-H,O-Z) 
      COMMON/ENA/EN,EL(40),BET(10),NUR,NMAX,NPD,LAS
      COMMON/SHEMM/ES(40),JU(40),NTU(40),NNB(40),NNG(40),NNO(40),
     *NPI(40)
      COMMON/OVLG/ANG1,ANG2,FOLAG,FOLAC,FOLAS,CD1,CD2,DG1,DG2,NNTG
     */MENU/MEJOB,MEPOT,MEHAM,MECHA,MEPRI,MESOL,MESHA,MESHO,MEHAO
     *,MEAPP,MEVOL,MEREL,MECUL,MERZZ,MERRR,MEDIS,MERIP
      COMMON/SHEM1/HW,AMG0,AMB0,GAM0,BET0,BET4,GAMDE,GSHAPE
     */SHEM2/GIT(40),EP0(40),EP1(40),EP2(40),EP12(40),ANG(40,2),EPB(40),
     *EPG(40),EGB(40),AIT(31,16),PT(40),FOG(2,8,8),XI(40),ANB(40),
     *CD(40,2)
     */OVLB/X1,P1,ANU1,X2,P2,ANU2,FOLAR,NNT
     */OVKN/FOV(40,40,4),CG1,SG1,CG2,SG2,CSG11,CG4,SG4,CSG22,CSG31,
     *CSG13,CG3,SG3,CSG21,CSG12
     */TRIGF/TRIG(40,40,14)
     */FOK12/FOK(6,6,2)
      COMMON/QNB/JO(40),NPO(40),KO(40),NCA(40)
     */SHAMO/BET3,ETO,AMUO,HWO,BB32,DPAR
     */AA/ANO(40)
      SI=SIN(GAM0+GSHAPE)
      CO=COS(GAM0+GSHAPE)
      CG1=CO
      SG1=SI
      SG2=SI*SI
      CG2=CO*CO
      CSG11=SI*CO
      CG3=CG1*CG2
      SG3=SG1*SG2
      CSG21=CG2*SG1
      CSG12=SG2*CG1
      CG4=CG2*CG2
      SG4=SG2*SG2
      CSG22=CG2*SG2
      CSG31=CG3*SG1
      CSG13=SG3*CG1
      FOLAR=1.
      DO 1 I=1,NUR
    1 JU(I)=JO(I)/2
      CALL SHEM
      IF(MESHO.EQ.0) GO TO 4
      NOM=0
      DO 5 I=1,NUR
      IF(NOM.LT.NNO(I)) NOM=NNO(I)
    5 CONTINUE
      NOM1=NOM+1
      LA=1
      IF(LAS.GE.2) LA=2
      DO 6 IO1=1,NOM1
      ANU1=ANO(IO1)
      DO 6 IO2=IO1,NOM1
      ANU2=ANO(IO2)
      DO 6 NNT=1,LA
      FOLAR=BET3**NNT
      IF(MEHAO.EQ.0) GO TO 7
      IF(MEHAO.LT.2) GO TO 9
      FOLAR=0.
      EBM=EXP(-(BET3/AMUO)**2)
      IF(NNT.EQ.1.AND.IO1.NE.IO2) FOLAR=BET3/SQRT(1.D0-EBM**2)
      IF(NNT.EQ.2.AND.IO1.EQ.IO2) FOLAR=AMUO**2/2.D0+BET3**2/
     *(1.-(-1)**IO1*EBM)
      IF(MEHAO.GE.2) GO TO 7
    9 X1=-BET3/AMUO
      X2=X1
      P1=BET3
      P2=BET3
      IF(BET3.EQ.0.) CALL OVLAO
      IF(BET3.NE.0.) CALL OVLAB
    7 IF(BET3.NE.0.) FOLAR=FOLAR/BET3**NNT
      IF(BET3.EQ.0.) FOLAR=FOLAR/AMUO**NNT
      IF(NNT.EQ.1.AND.IO1.EQ.IO2) FOLAR=0.D0
      IF(NNT.EQ.2.AND.IO1.NE.IO2) FOLAR=0.D0
      FOK(IO1,IO2,NNT)=FOLAR
      IF(IO1.NE.IO2) FOK(IO2,IO1,NNT)=FOK(IO1,IO2,NNT)
      IF(MEPRI.LT.99) THEN
        PRINT 8,IO1,IO2,NNT,FOLAR,ANU1,ANU2
        WRITE(21,88)IO1,IO2,NNT,FOLAR,ANU1,ANU2
      ENDIF
    8 FORMAT (3I4,3E20.7)
   88 FORMAT (1X,'IO1=',I3,1X,'IO2=',I3,1X,'NNT=',I3,2X,'FOLAR=',D15.7,
     *1X,'ANU1=',D15.7,2X,'ANU2=',D15.7)
    6 CONTINUE
    4 DO 2 JU1=1,NUR
      KG1=(3-(-1)**NNO(JU1))/2
      DG1=0.
      IF(KG1.EQ.2) DG1=GAMDE
      ANG1=ANG(NNG(JU1)+1,KG1)
      CD1=CD(NNG(JU1)+1,KG1)
      X1=XI(JU1)
      P1=PT(JU1)
      ANU1=ANB(JU1)
      DO 2 JU2=JU1,NUR
      KG2=(3-(-1)**NNO(JU2))/2
      DG2=0.
      IF(KG2.EQ.2) DG2=GAMDE
      ANG2=ANG(NNG(JU2)+1,KG2)
      CD2=CD(NNG(JU2)+1,KG2)
      X2=XI(JU2)
      P2=PT(JU2)
      ANU2=ANB(JU2)
      IF(MEHAM.GT.4) CALL OVLAGE
      TRIG(JU1,JU2,1)=CG1
      IF(JU1.NE.JU2) TRIG(JU2,JU1,1)=CG1
      TRIG(JU1,JU2,2)=SG1
      IF(JU1.NE.JU2) TRIG(JU2,JU1,2)=SG1
      TRIG(JU1,JU2,3)=CG2
      IF(JU1.NE.JU2) TRIG(JU2,JU1,3)=CG2
      TRIG(JU1,JU2,4)=SG2
      IF(JU1.NE.JU2) TRIG(JU2,JU1,4)=SG2
      TRIG(JU1,JU2,5)=CSG11
      IF(JU1.NE.JU2) TRIG(JU2,JU1,5)=CSG11
      TRIG(JU1,JU2,6)=CG3
      IF(JU1.NE.JU2) TRIG(JU2,JU1,6)=CG3
      TRIG(JU1,JU2,7)=SG3
      IF(JU1.NE.JU2) TRIG(JU2,JU1,7)=SG3
      TRIG(JU1,JU2,8)=CSG21
      IF(JU1.NE.JU2) TRIG(JU2,JU1,8)=CSG21
      TRIG(JU1,JU2,9)=CSG12
      IF(JU1.NE.JU2) TRIG(JU2,JU1,9)=CSG12
      TRIG(JU1,JU2,10)=CG4
      IF(JU1.NE.JU2) TRIG(JU2,JU1,10)=CG4
      TRIG(JU1,JU2,11)=SG4
      IF(JU1.NE.JU2) TRIG(JU2,JU1,11)=SG4
      TRIG(JU1,JU2,12)=CSG22
      IF(JU1.NE.JU2) TRIG(JU2,JU1,12)=CSG22
      TRIG(JU1,JU2,13)=CSG31
      IF(JU1.NE.JU2) TRIG(JU2,JU1,13)=CSG31
      TRIG(JU1,JU2,14)=CSG13
      IF(JU1.NE.JU2) TRIG(JU2,JU1,14)=CSG13
      FOLAR=1.
      DO 3 NNT=1,LAS
      IF(MEHAM.NE.4) CALL OVLAB
      FOV(JU1,JU2,NNT)=FOLAR
      IF(JU1.NE.JU2) FOV(JU2,JU1,NNT)=FOV(JU1,JU2,NNT)
      IF(MEPRI.LT.99) THEN
        PRINT 10,JU1,JU2,NNT,FOV(JU1,JU2,NNT),ANU1,ANU2
        WRITE(21,89)JU1,JU2,NNT,FOV(JU1,JU2,NNT),ANU1,ANU2
      ENDIF
   10 FORMAT(3I5,3E17.7)
   89 FORMAT(1X,'JU1=',I2,1X,'JU2=',I2,1X,'NNT=',I2,1X,'FOV(JU1,JU2,
     *NNT)=',D15.7,2X,'ANU1=',D15.7,2X,'ANU2=',D15.7)
    3 CONTINUE
    2 CONTINUE
      RETURN
      END
C     *******************************************************
C     END of knditd
C     *******************************************************
C     *******************************************************
C     START of dispersive  
C     *******************************************************
C==========================================================================
C     AUTHOR: Dr. Roberto Capote Noy
c
C     e-mail: r.capotenoy@iaea.org ; rcapotenoy@yahoo.com; 
C
C     DISPERSIVE OPTICAL MODEL POTENTIAL PACKAGE
C
c     Analytical dispersive integrals are included
c     see Quesada JM, Capote R et al,
C             Computer Physics Communications 153(2003) 97
C             Phys. Rev. C67(2003) 067601
C
C     Dispersive integral's derivatives calculated by Dr.J.M.Quesada
C
      DOUBLE PRECISION FUNCTION DOM_INT_Wv
     >    (Ef,Ep,Av,Bv,n,Einc,DerivIntWv)
C
C     Analytical dispersive integral and its derivative for
C     Wv(E)=Av*(E-Ep)**n/( (E-Ep)**n + Bv**n )  for E>Ep
C     Wv(E)=Wv(2*Ef-E)                          for E<2Ef-Ep
C     Wv(E)=0                                     OTHERWISE
C
      IMPLICIT NONE
      DOUBLE PRECISION Ef,Ep,Av,Bv,E,pi,Einc
      DOUBLE PRECISION E0,Ex,Eplus,Emin,Rs,ResEmin,ResEplus
      DOUBLE PRECISION DerEmin, DerEplus, Rds, DerivIntWv
      DOUBLE COMPLEX Pj,I,Zj,Ztmp
      DOUBLE COMPLEX Fs,Ds

      INTEGER N,j,IS

      DATA I/(0.d0,1.d0)/

      pi=4.d0*atan(1.d0)

      IS = 1
      E = Einc
      IF(Einc.LE.Ef) THEN
        E=2.d0*Ef-Einc
C       Odd function
        IS = -1
      ENDIF

      E0 = Ep - Ef
      Ex = E  - Ef
      Eplus = Ex + E0
      Emin  = Ex - E0
      DOM_INT_Wv = 0.d0
      DerivIntWv = 0.d0

      ResEmin  =  Emin**n / (Emin**n + Bv**n)

      DerEmin  =  Emin**(n-1) *
     >            ( Emin**n + Bv**n*(1.d0 + n*log(ABS(Emin)) ) )
     >            / (Emin**n + Bv**n)**2

      ResEplus = -Eplus**n / (Eplus**n + Bv**n)

      DerEplus = -Eplus**(n-1) *
     >            ( Eplus**n + Bv**n*(1.d0+n*log(Eplus)) )
     >            / (Eplus**n + Bv**n)**2

C----------------------------------
C     Complex arithmetic follows
C
      Fs = (0.d0,0.d0)
      Ds = (0.d0,0.d0)
      do j=1,n
       Ztmp = I*(2*j-1)/dble(n)*pi
       Pj = Bv*exp(Ztmp)
       Zj = Pj * (2*Pj +Eplus -Emin) * Ex
       Zj = Zj / ( (Pj+E0) * (Pj+Eplus) * (Pj-Emin) )
       Fs = Fs + Zj*log(-Pj)
       Ds = Ds + 2*Pj*(Ex*Ex + (Pj+E0)**2)*log(-Pj)
     >           /( (Pj+Eplus)**2 * (Pj-Emin)**2 )
      enddo

      IF(ABS(IMAG(Fs)).gt.1.d-4) STOP 'Too big imag part in Wv'
      IF(ABS(IMAG(Ds)).gt.1.d-4) STOP 'Too big imag deriv in Wv'
      Rs  = REAL(Fs)
      Rds = REAL(Ds)
C----------------------------------

      DOM_INT_Wv = -Av/pi*IS*
     &  (Rs/n  + ResEplus*log(Eplus) + ResEmin*log(ABS(Emin)))

C     Sign of derivative changed
C     DerivIntWv = -Av/pi*IS*( Rds/n + DerEplus + DerEmin)
      DerivIntWv =  Av/pi*IS*( Rds/n + DerEplus + DerEmin)

      RETURN
      END

      DOUBLE PRECISION FUNCTION DOM_INT_Ws
     >     (Ef,Ep,As,Bs,Cs,m,Einc,DerivIntWs)
C
C     Analytical dispersive integral and its derivative for
C     Ws(E)=As*(E-Ep)**m/( (E-Ep)**m + Bs**m ) * exp(-Cs*(E-Ep)) for E>Ep
C     Ws(E)=Ws(2*Ef-E)                                           for E<2Ef-Ep
C     Ws(E)=0                                                    OTHERWISE
C
      IMPLICIT NONE
      DOUBLE PRECISION Ef,Ep,As,Bs,Cs,E,EIn,Einc
      DOUBLE COMPLEX I,Pj,Zj,Ztmp,zfi
      DOUBLE PRECISION E0,Ex,Eplus,Emin,pi
      DOUBLE PRECISION Rs,ResEmin,ResEplus
      DOUBLE PRECISION DerivIntWs,DerEmin,DerEplus,Rds
      INTEGER m,j,IS
      DOUBLE COMPLEX Fs,Ds

      DATA I/(0.d0,1.d0)/

      pi=4.d0*atan(1.d0)

      IS = 1
      E = Einc
      IF(Einc.LE.Ef) THEN
        E=2.d0*Ef-Einc
C       Odd function
        IS = -1
      ENDIF

      E0 = Ep - Ef
      Ex = E  - Ef
      Eplus = Ex + E0
      Emin  = Ex - E0
      DOM_INT_Ws = 0.d0
      DerivIntWs = 0.d0

      ResEmin  =  Emin**m / (Emin**m + Bs**m)

      DerEmin  = -Emin**(m-1) *
     >           ( Emin**m + Bs**m + ( -Cs*Emin**(m+1) +
     >            Bs**m *(-Cs*Emin+m) ) * exp(-Cs*Emin)*EIn(Cs*Emin) )
     >            / (Emin**m + Bs**m)**2

      ResEplus = -Eplus**m / (Eplus**m + Bs**m)

      DerEplus =  Eplus**(m-1) *
     >           ( Eplus**m + Bs**m + ( Cs*Eplus**(m+1) +
     >            Bs**m *(Cs*Eplus+m) ) * exp(Cs*Eplus)*EIn(-Cs*Eplus) )
     >            / (Eplus**m + Bs**m)**2

C----------------------------------
C     Complex arithmetic follows
C
      Fs = (0.d0,0.d0)
      Ds = (0.d0,0.d0)
      do j=1,m
       Ztmp = I*(2*j-1)/dble(m)*pi
       Pj = Bs*exp(Ztmp)
       Zj = Pj * (2*Pj +Eplus -Emin) * Ex
       Zj = Zj / (Pj+E0) / (Pj+Eplus) / (Pj-Emin)
       Fs = Fs + Zj* zfi(-Pj*Cs)
       Ds = Ds + 2*Pj*(Ex*Ex + (Pj+E0)**2)*zfi(-Pj*Cs)
     >           /( (Pj+Eplus)**2 * (Pj-Emin)**2 )
      enddo

      IF(ABS(IMAG(Fs)).gt.1.d-4) STOP 'Too big imag part in Ws'
      IF(ABS(IMAG(Ds)).gt.1.d-4) STOP 'Too big imag deriv in Ws'
      Rs = REAL(Fs)
      Rds = REAL(Ds)
C----------------------------------

      DOM_INT_Ws = As/pi*IS*(Rs/m
     &                  - ResEplus*exp(Cs*Eplus)*EIn(-Cs*Eplus)
     &                  - ResEmin*exp(-Cs*Emin)*EIn(Cs*Emin) )
C     Sign of derivative changed
C     DerivIntWs =  As/pi*IS*( Rds/m + DerEplus + DerEmin)
      DerivIntWs = -As/pi*IS*( Rds/m + DerEplus + DerEmin)

      RETURN
      END

      DOUBLE PRECISION  function WV(A,B,Ep,Ef,E,n)
      IMPLICIT NONE
      DOUBLE PRECISION  A,B,Ep,Ef,E,ee
      integer n

      WV=0.d0
      if(E.LE.Ef) E=2.d0*Ef-E
      if(E.LT.Ep) return

      ee=(E-Ep)**n
      WV=A*ee/(ee+B**n)

      return
      end

      DOUBLE PRECISION function WDD(A,B,C,Ep,Ef,E,m)
      IMPLICIT NONE
      DOUBLE PRECISION A,B,C,Ep,Ef,E,ee,arg
      integer m

      WDD=0.d0
      if(E.LE.Ef) E=2.d0*Ef-E
      if(E.LT.Ep) return

      arg=C*(E-Ep)
      IF(arg.GT.15) return
      ee=(E-Ep)**m
      WDD=A*ee/(ee+B**m)*EXP(-arg)
      return
      end


      DOUBLE PRECISION FUNCTION DOM_int_T1(Ef,Ea,E)
C
C     Integral over E' corresponding to nonlocal additions T1(E'<<0)
C
      IMPLICIT NONE

      DOUBLE PRECISION E,Ea,Ef,Ex,Ea2,Eax,Pi,T11,T12,T13
      Pi=4.d0*ATAN(1.d0)

      Ex=E-Ef
      Ea2=Ea**2
      Eax=Ex+Ea

      T11 = 0.5d0*log(Ea)/Ex
      T12 =  ( (2*Ea+Ex)*log(Ea)+0.5d0*pi*Ex )
     >      /(2.*(Eax**2 + Ea2))
      T13 = -Eax**2*log(Eax)/(Ex*(Eax**2+Ea2))

      DOM_int_T1 = Ex/Pi*(T11+T12+T13)
C
      RETURN
      END
C
      DOUBLE PRECISION FUNCTION DOM_int_T2(Ef,Ea,E)
C
C     Integral over E' corresponding to nonlocal additions T2(E'>>0)
C
      IMPLICIT NONE
      DOUBLE PRECISION E,Ea,Ef,EL,Pi

      Pi=4.d0*ATAN(1.d0)
      EL=Ef+Ea
      DOM_int_T2= 1.d0 / Pi * (
     >      sqrt(abs(Ef)) * atan( (2*sqrt(EL*abs(Ef)))/(EL-abs(Ef)) )
     > +    EL**1.5d0/(2*Ef)*log(Ea/EL) )

      IF(E.GT.EL) THEN

      DOM_int_T2 = DOM_int_T2 + 1.d0/Pi* (
     >  sqrt(E) * log( (sqrt(E)+sqrt(EL)) / (sqrt(E)-sqrt(EL)) ) +
     >  1.5d0*sqrt(EL)*log((E-EL)/Ea) + EL**1.5d0/(2*E)*log(EL/(E-EL)) )

      ELSEIF(E.EQ.EL) THEN

      DOM_int_T2 = DOM_int_T2 + 1.d0/Pi*1.5d0*sqrt(EL)
     > *log((2**(4.d0/3.d0)*EL)/Ea)

      ELSEIF(E.GT.0.d0 .AND. E.LE.EL) THEN

      DOM_int_T2 = DOM_int_T2 + 1.d0/Pi * (
     > sqrt(e) * log( (sqrt(E)+sqrt(EL)) / (sqrt(EL)-sqrt(E)) ) +
     > 1.5d0*sqrt(EL)*log((EL-E)/Ea)+EL**1.5d0/(2.d0*E)*log(EL/(EL-E)) )

      ELSEIF(E.EQ.0.d0) THEN

      DOM_int_T2 = DOM_int_T2 + 1.d0/Pi*( 0.5*EL**(1./3.)
     > + log(EL/Ea) + 0.5d0*sqrt(EL) )

      ELSE

      DOM_int_T2 = DOM_int_T2 + 1.d0/Pi * (
     > -sqrt(abs(E))*atan( 2*(sqrt(EL-abs(E))) / (EL-abs(E)) ) +
     > 1.5d0*sqrt(EL)*log((EL-E)/Ea)+EL**1.5d0/(2.d0*E)*log(EL/(EL-E)) )

      ENDIF
      RETURN
      END
C
C-----FUNCTION TO EVALUATE exp(Z)*E1(Z)
C
      DOUBLE COMPLEX function zfi(za)     
C
C Complex exponential integral function multiplied by exponential
C
C AUTHOR: J. Raynal
C
      IMPLICIT NONE
      DOUBLE PRECISION aj
      DOUBLE COMPLEX za,y 
      integer m,i
      zfi=0.d0
      if (za.eq.0.d0) return
c     if (ABS(dreal(za)+18.5d0).ge.25.d0) go to 3
c     if (SQRT(625.d0-(dreal(za)+18.5d0)**2)/1.665d0.lt.ABS(dimag(za))
      if (abs(real(za)+18.5d0).ge.25.d0) go to 3
      if (sqrt(625.d0-(real(za)+18.5d0)**2)/1.665d0.lt.abs(imag(za))
     1) go to 3
C     zfi=-.57721566490153d0-cdlog(za)
      zfi=-.57721566490153d0-log(za)
      y=1.d0
      do 1 m=1,2000
      aj=m
      y=-y*za/aj
c     if (cABS(y).lt.1.d-15*cABS(zfi)) go to 2
      if (abs(y).lt.1.d-15*abs(zfi)) go to 2
    1 zfi=zfi-y/aj
C   2 zfi=cEXP(za)*zfi
    2 zfi=EXP(za)*zfi
      return
    3 do 4 i=1,20
      aj=21-i
      zfi=aj/(za+zfi)
    4 zfi=aj/(1.d0+zfi)
      zfi=1.d0/(zfi+za)
      return
      end

C
C-----FUNCTION TO EVALUATE Ei(X)
C
      DOUBLE PRECISION FUNCTION EIn(X)
      IMPLICIT NONE
      DOUBLE PRECISION FAC, H, X
      INTEGER N
      EIn = 0.57721566490153d0+LOG(ABS(X))
      FAC = 1.0
      DO N = 1,100
      H = FLOAT(N)
      FAC = FAC*H
      EIn = EIn + X**N/(H*FAC)
      ENDDO
      RETURN
      END
C     *******************************************************
C     END of dispersive
C     *******************************************************

C     *******************************************************
C     START of shemsofd
C     *******************************************************
C     *****************************************************************
      SUBROUTINE FUDNU
C     *****************************************************************
      IMPLICIT DOUBLE PRECISION(A-H,O-Z) 
      COMMON/FUDN/Y,DN,ANU,DNV,CDV
      SQ2=SQRT(2.d0)
      YY=Y
      Z2=Y**2
      AN2=ANU/2.D0
      EX4=EXP(-Z2/4.D0)
      GNP1=GAMMA(ANU+1.D0)
      M=1
       PI=4.d0*atan(1.d0)
       PIN=PI*ANU
       PIN2=PI*AN2
      SQPIN=0.564189583548D0
      IF(ABS(YY).GT.5.AND.ABS(YY).GT.ANU) GO TO 1
      CPN2=COS(PIN2)
      SPN2=SIN(PIN2)
      GN2P12=GAMMA(AN2+0.5D0)
      GN2P1=GAMMA(AN2+1.D0)
      C2N2=(2.D0)**AN2
      FM=1.D0
      VM=1.D0
      DF=FM
      DV=VM
    2 Z2M=Z2/M
      FM=FM*Z2M/(2*M-1)*(M-1-AN2)
      VM=VM*Z2M/(2*M+1)*(M-0.5D0-AN2)
      DF=DF+FM
      DV=DV+VM
      IF((ABS(FM)+ABS(VM))/(ABS(DF)+ABS(DV)).LT.1.D-6)GO TO 5
      M=M+1
      GO TO 2
    5 DN=C2N2*SQPIN*EX4*(CPN2*GN2P12*DF+SQ2*Y*SPN2*GN2P1*DV)
      DNV=EX4/C2N2*(-SPN2/GN2P1*DF+SQ2*Y*CPN2/GN2P12*DV)
      GO TO 6
    1 YN=Z2**AN2
      YN1=Z2**(AN2+0.5D0)
      CPN=COS(PIN)
      SPN=SIN(PIN)
      SQP2=SQ2*SQPIN
      FM=1.D0
      DF=FM
    4 Z2M=Z2*(2*M)
      VM=(2*M+ANU)*(2*M-1+ANU)/Z2M
      IF(ABS(VM).GT.1.D0)GO TO 3
      FM=FM*VM
      DF=DF+FM
      IF(ABS(FM).LT.1.D-6)GO TO 3
      M=M+1
      GO TO 4
    3 M=1
      FM=1
      DV=FM
    9 Z2M=Z2*(2*M)
      VM=-(ANU-2*M+1)*(ANU-2*M+2)/Z2M
      IF(ABS(VM).GT.1.D0)GO TO 8
      FM=FM*VM
      DV=DV+FM
      IF(ABS(FM).LT.1.D-6)GO TO 8
      M=M+1
      GO TO 9
    8 IF(Y) 10,10,11
   11 DN=EX4*YN*DV
      DNV=SQP2/EX4/YN1*DF
      GO TO 6
   10 DN=-SQP2*SPN*GNP1/EX4/YN1*DF+CPN*EX4*YN*DV
      DNV=-SPN*EX4/GNP1*YN*DV-CPN*SQP2/EX4/YN1*DF
    6 RETURN
      END
C     *****************************************************************
      SUBROUTINE MATAM
C     *****************************************************************
      IMPLICIT DOUBLE PRECISION(A-H,O-Z) 
      COMMON/VEC/TM(16,16),AM(16,16),EIN(31,16),NK,KMI,ITAU
      COMMON/INRM/AMO,BMO,CMO,BB42,GAMG,DELG
      COMMON/MAT/GAM,IS,NO
      KMI=1-(-1)**(IS+NO)
      KMA=2*(IS/2)
      NK=(KMA-KMI+2)/2
      CALL INERMO
      A=AMO
      B=BMO
      C=CMO
      C4=C/4.D0
      AB8=(A+B)/8.
      AB16=(A-B)/16.
      DO 1 J=1,NK
      DO 1 I=1,J
      KS=KMI+2*(I-1)
      IF(I.EQ.J) GO TO 2
      IF(I+1.EQ.J) GO TO 3
    5 AM(I,J)=0.D0
      GO TO 1
    2 AM(I,J)=AB8*(IS*(IS+1.D0)-KS*KS)+C4*KS*KS
      GO TO 1
    3 AB16IK=AB16*SQRT((IS+KS+2.D0)*(IS-KS-1.D0)*(IS+KS+1.D0)*(IS-KS))
      IF(KS.EQ.0) GO TO 4
      AM(I,J)=AB16IK
      GO TO 1
    4 AM(I,J)=AB16IK*(1+(-1)**(IS+NO))/SQRT(2.d0)
    1 CONTINUE
      CALL VECNO
      RETURN
      END
C     *****************************************************************
      SUBROUTINE VECNO
C     *****************************************************************
      IMPLICIT DOUBLE PRECISION(A-H,O-Z) 
      COMMON/VEC/TM(16,16),AM(16,16),EIN(31,16),NK,KMI,ITAU
     */VEC1/TM1(16,16),AM1(16,16)
      COMMON/MAT/GAM,IS,NO
      DO 1 I=1,NK
      DO 1 J=1,NK
      IF(I.EQ.J) GO TO 2
      TM(I,J)=0.D0
      TM1(I,J)=0.D0
      GO TO 18
    2 TM(I,J)=1.D0
      TM1(I,J)=1.D0
   18 IF(I.GT.J) GO TO 1
      AM1(I,J)=AM(I,J)
    1 CONTINUE
      IF(NK.EQ.1) GO TO 17
    3 IM=1
      JM=2
      A=ABS(AM(1,2))
      IF(NK.EQ.2) GO TO 20
      DO 4 J=3,NK
      J1=J-1
      DO 4 I=1,J1
      B=ABS(AM(I,J))
      IF(A.GE.B) GO TO 4
      A=B
      IM=I
      JM=J
    4 CONTINUE
   20 CONTINUE
      AMII=AM(IM,IM)
      AMJJ=AM(JM,JM)
      AMIJ=AM(IM,JM)
      AL=ATAN(2.D0*AMIJ/(AMII-AMJJ))/2.D0
      ACO=COS(AL)
      ASI=SIN(AL)
      DO 5 L=1,NK
      DO 5 K=1,L
      IF(K.EQ.IM) GO TO 6
      IF(K.EQ.JM) GO TO 7
      IF(L.EQ.IM) GO TO 8
      IF(L.EQ.JM) GO TO 9
      GO TO 5
    6 IF(L.EQ.IM) GO TO 10
      IF(L.EQ.JM) GO TO 11
      AM1(K,L)=ACO*AM(IM,L)+ASI*AM(JM,L)
      GO TO 5
    7 IF(L.EQ.JM) GO TO 12
      AM1(K,L)=-ASI*AM(IM,L)+ACO*AM(JM,L)
      GO TO 5
    8 AM1(K,L)=ACO*AM(K,IM)+ASI*AM(K,JM)
      GO TO 5
    9 AM1(K,L)=-ASI*AM(K,IM)+ACO*AM(K,JM)
      GO TO 5
   10 AM1(K,L)=ACO*(ACO*AMII+ASI*AMIJ)+ASI*(ACO*AMIJ+ASI*AMJJ)
      GO TO 5
   11 AM1(K,L)=0.D0
      GO TO 5
   12 AM1(K,L)=-ASI*(-ASI*AMII+ACO*AMIJ)+ACO*(-ASI*AMIJ+ACO*AMJJ)
    5 CONTINUE
      DO 13 K=1,NK
      DO 13 L=1,NK
      IF(K.EQ.IM) GO TO 14
      IF(K.EQ.JM) GO TO 15
      GO TO 13
   14 TM1(K,L)=ACO*TM(IM,L)-ASI*TM(JM,L)
      GO TO 13
   15 TM1(K,L)=ASI*TM(IM,L)+ACO*TM(JM,L)
   13 CONTINUE
      E2A=0.D0
      DO 16 K=1,NK
      DO 16 L=1,NK
      TM(K,L)=TM1(K,L)
      IF(K.GT.L) GO TO 16
      IF(K.LT.L) E2A=E2A+AM1(K,L)**2
      AM(K,L)=AM1(K,L)
   16 CONTINUE
      IF(E2A.LT.1.D-8) GO TO 17
      GO TO 3
   17 CONTINUE
      IF(IS.EQ.0) GO TO 25
      DO 24 I=1,NK
      I1=I+1
      EM=AM(I,I)
      IM=I
      IF(I1.GT.NK) GO TO 23
      DO 21 J=I1,NK
      E=AM(J,J)
      IF(EM.LE.E) GO TO 21
      EM=E
      IM=J
   21 CONTINUE
   23 DO 22 J=1,NK
      T1=TM(J,IM)
      TM(J,IM)=TM(J,I)
      TM(J,I)=T1
   22 CONTINUE
      EIN(IS+1,I)=EM
      AM(IM,IM)=AM(I,I)
      AM(I,I)=EM
   24 CONTINUE
   25 CONTINUE
      IF(IS.EQ.0) EIN(1,1)=0.d0
      IF(IS.EQ.0) TM(1,1)=1.d0
      RETURN
      END

C     *****************************************************************
      BLOCK DATA
C     *****************************************************************
      IMPLICIT DOUBLE PRECISION(A-H,O-Z) 
      COMMON/SENU/ B0(6),B1(6),C0(5),C1(5),AN(4)
     *,B2(5),C2(5),B3(5),C3(5)
      DATA B0/-22.11505d0,6.64844d0,.769968d0,-.622406d0,.107521d0,
     * -.007364d0/, B1/-17.87240d0,4.66396d0,2.94376d0,-2.03488d0,
     * .51202d0,-.051336d0/, C0/-2.32285d0,2.65365d0,-1.077629d0,
     * .173691d0,-.007545d0/, C1/-.630875d0,1.198105d0,-.727493d0,
     * .164266d0,-.0091289d0/, AN/-2.d0,-2.2d0,-2.4d0,-2.6d0/
      DATA B2/-15.007288d0,5.405502d0,1.117805d0,-.831844d0,.110628d0/,
     *B3/-12.206497d0,4.48481d0,1.679626d0,-1.226475d0,.190465d0/,
     *C2/.82866d0,-.587769d0,-.005974d0,.0581198d0,-.00321186d0/,
     *C3/1.191310d0,-1.284630d0,.368034d0,-.00188923d0,.000426352d0/
      END
C     ***************************************************************
      SUBROUTINE EIT12
C     *****************************************************************
      IMPLICIT DOUBLE PRECISION(A-H,O-Z) 
      COMMON/EIT/EPIT1,EPIT2,EPIT12
      COMMON/VEC/TM(16,16),AM(16,16),EIN(31,16),NK,KMI,ITAU
      COMMON/XI12/XIT,XIT1,DET,NROOT
      COMMON/MAT/GAM,IS,NO
      COMMON/INRM/AMO,BMO,CMO,BB42,GAMG,DELG
      COMMON/OVLG/ANG1,ANG2,FOLAG,FOLAC,FOLAS,CD1,CD2,DG1,DG2,NNTG
      COMMON/SHEM1/HW,AMG0,AMB0,GAM0,BET0,BET4,GAMDE,GSHAPE
     */MENU/MEJOB,MEPOT,MEHAM,MECHA,MEPRI,MESOL,MESHA,MESHO,MEHAO
     *,MEAPP,MEVOL,MEREL,MECUL,MERZZ,MERRR,MEDIS,MERIP
     */SHEM2/GIT(40),EP0(40),EP1(40),EP2(40),EP12(40),ANG(40,2),EPB(40),
     *EPG(40),EGB(40),AIT(31,16),PT(40),FOG(2,8,8),XI(40),ANB(40),
     *CD(40,2)
      COMMON/FUDN/Y,DN,ANU,DNV,CDV
      C1=SQRT(2.d0)
C     PI3=1.0471975512D0
      PI3=4.d0/3.d0*atan(1.d0)      
      IF (MEHAM.EQ.5.OR.MEHAM.EQ.7) GO TO 2
      AMOO=AMO
      BMOO=BMO
      CMOO=CMO
      DEGAM=0.001D0*GAM
      GAM=GAM0+DEGAM
      CALL INERMO
      AMOP=AMO
      BMOP=BMO
      CMOP=CMO
      GAM=GAM0-DEGAM
      CALL INERMO
      AMOM=AMO
      BMOM=BMO
      CMOM=CMO
      GAM=GAM0
      A1D=(AMOP-AMOM)/DEGAM/2.D0
      B1D=(BMOP-BMOM)/DEGAM/2.D0
      C1D=(CMOP-CMOM)/DEGAM/2.D0
      A2D=(AMOP+AMOM-2.D0*AMOO)/DEGAM/DEGAM
      B2D=(BMOP+BMOM-2.D0*BMOO)/DEGAM/DEGAM
      C2D=(CMOP+CMOM-2.D0*CMOO)/DEGAM/DEGAM
      AB1DM=A1D-B1D
      AB1DP=A1D+B1D
      AB2DM=A2D-B2D
      AB2DP=A2D+B2D
      NGD=2
      SUM3=0.D0
      NGU=NROOT
      NR1=NROOT+1
      INGP=NGU+NGD+1
      IF(INGP.GT.4) INGP=4
      INGM=NGU-NGD+1
      IF(INGM.LT.1) INGM=1
      ANG1=ANG(NROOT+1,NO)
      CD1=CD(NROOT+1,NO)
      DO 10 INGA=INGM,INGP
      NGA=INGA-1
      ANG2=ANG(INGA,NO)
      CD2=CD(INGA,NO)
      DO 10 ITA=1,NK
      SUM1=0.D0
      SUM2=0.D0
      DO 1 I=1,NK
      AK=TM(I,ITAU)
      AKA=TM(I,ITA)
      KA=KMI+(I-1)*2
      SUM1=SUM1+AK*AKA*KA*KA
      D=1.D0
      DD=0.D0
      IF(KA.EQ.0) DD=1.d0
      IF(KA.EQ.0) D=SQRT(2.d0)
      IF(I.EQ.NK) GO TO 1
      AK1=TM(I+1,ITAU)
      AKA1=TM(I+1,ITA)
      SUM2=SUM2+(AK1*AKA+AK*AKA1)*(1.D0+(-1)**IS*DD)/D*
     *SQRT((IS+KA+2.D0)*(IS+KA+1.D0)*(IS-KA-1.D0)*(IS-KA))
    1 CONTINUE
      IF(ITA.EQ.ITAU.AND.NGU.EQ.NGA)GO TO 5
      IF(ITA.NE.ITAU.AND.NGU.NE.NGA)GO TO 4
      GO TO 10
    5 EPIT1=AB1DP/8.D0*IS*(IS+1)+(0.25D0*C1D-AB1DP/8.D0)*SUM1+
     *AB1DM/16.D0*SUM2
      EPIT1=EPIT1*FOG(1,NR1,INGA)
      EPIT2=AB2DP/8.D0*IS*(IS+1)+(0.25D0*C2D-AB2DP/8.D0)*SUM1+
     *AB2DM/16.D0*SUM2
      EPIT2=EPIT2*FOG(2,NR1,INGA)/2.
      GO TO 10
    4 SU1M=(0.25D0*C1D-AB1DP/8.D0)*SUM1+
     *AB1DM/16.D0*SUM2
      IF(ITAU.EQ.ITA) SU1M=SU1M+AB1DP/8.D0*IS*(IS+1.D0)
      SU1M=SU1M*FOG(1,NR1,INGA)
      SU2M=(0.25D0*C2D-AB2DP/8.D0)*SUM1+
     *AB2DM/16.D0*SUM2
      IF(ITAU.EQ.ITA) SU2M=SU2M+AB2DP/8.*IS*(IS+1.)
      SU2M=SU2M*FOG(2,NR1,INGA)/2.D0
      DELE=2.D0/AMG0**2*(ANG1-ANG2)+EIN(IS+1,ITAU)-EIN(IS+1,ITA)
      SUM3=SUM3+(SU1M+SU2M)**2/DELE
   10 CONTINUE
      EPIT12=SUM3
      GO TO 3
    2 EPIT1=0.D0
      EPIT2=0.D0
      EPIT12=0.D0
    3 RETURN
      END
C     *****************************************************************
      SUBROUTINE ANUDF
C     *****************************************************************
      IMPLICIT DOUBLE PRECISION(A-H,O-Z) 
      COMMON/SENU/B0(6),B1(6),C0(5),C1(5),AN(4),
     *B2(5),C2(5),B3(5),C3(5)
      COMMON/XI12/XIT,XIT1,DET,NROOT/FUDN/Y,DN,ANU,DNV,CDV
      ANU=NROOT
      NRP1=NROOT+1
      IF(XIT+5) 7,7,8
    7 IF(XIT.LT.-10.D0) ANU=ANU
      IF(XIT.LT.-10.D0)GO TO 9
      Z2=2.D0*XIT*XIT
      ANU=ANU+EXP(-Z2/2.D0)*Z2**(ANU+0.5D0)*0.39894228D0
     **(1.D0-(ANU*ANU+ANU+1.D0)/Z2)/GAMMA(1.D0+ANU)
      GO TO 9
    8 Y=5.D0+XIT
      Y2=Y*Y
      Y3=Y*Y2
      Y4=Y*Y3
      IF(XIT-AN(NRP1)) 10,10,11
   10 Y5=Y*Y4
      GO TO (2,12,22,32,35,35,35),NRP1
    2 ANU=ANU+Y*EXP(B0(1)+B0(2)*Y+B0(3)*Y2+B0(4)*Y3+
     *B0(5)*Y4+B0(6)*Y5)
      GO TO 9
   12 ANU=ANU+ Y*EXP(B1(1)+B1(2)*Y+B1(3)*Y2+B1(4)*Y3+
     *B1(5)*Y4+B1(6)*Y5)
      GO TO 9
   22 ANU=ANU+Y*EXP(B2(1)+B2(2)*Y+B2(3)*Y2+B2(4)*Y3+
     *B2(5)*Y4)
      GO TO 9
   32 ANU=ANU+Y*EXP(B3(1)+B3(2)*Y+B3(3)*Y2+B3(4)*Y3+
     *B3(5)*Y4)
      GO TO 9
   11 GO TO (3,13,23,33,35,35,35),NRP1
    3 ANU=ANU+C0(1)+C0(2)*Y+C0(3)*Y2+C0(4)*Y3+C0(5)*Y4
      GO TO 9
   13 ANU=ANU+C1(1)+C1(2)*Y+C1(3)*Y2+C1(4)*Y3+C1(5)*Y4
      GO TO 9
   23 ANU=ANU+C2(1)+C2(2)*Y+C2(3)*Y2+C2(4)*Y3+C2(5)*Y4
      GO TO 9
   33 ANU=ANU+C3(1)+C3(2)*Y+C3(3)*Y2+C3(4)*Y3+C3(5)*Y4
      GO TO 9
   35 PRINT 36
      WRITE(21,36)
   36 FORMAT(10X,'THIS CASE IS NOT REALIZED NROOT>=4')
    9 RETURN
      END
C     *****************************************************************
      SUBROUTINE DETX12
C     *****************************************************************
      IMPLICIT DOUBLE PRECISION(A-H,O-Z) 
      COMMON/XI12/XIT,XIT1,DET,NROOT/FUDN/Y,DN,ANU,DNV,CDV
      Y=XIT
      CALL FUDNU
      UX1=DN
      VX1=DNV
      Y=XIT1
      CALL FUDNU
      UX2=DN
      VX2=DNV
      DET=UX1*VX2-UX2*VX1
      RETURN
      END
C     *****************************************************************
      SUBROUTINE ANDET0
C     *****************************************************************
      IMPLICIT DOUBLE PRECISION(A-H,O-Z) 
      COMMON/XI12/XIT,XIT1,DET,NROOT/FUDN/Y,DN,ANU,DNV,CDV
      ANU=0.D0
      NR=0
    5 STEP=0.1D0
      CALL DETX12
      A=DET
C     IF(A.EQ.0.) GO TO 2
      IF(ABS(A).LE.1.d-10) GO TO 2
    3 ANU=ANU+STEP
      CALL DETX12
      IF(A*DET)1,2,3
    1 ANU=ANU-STEP
      STEP=STEP/5.D0
      IF(STEP.LT.1.D-6) GO TO 2
      GO TO 3
    2 NR=NR+1
      IF(NR.GT.NROOT) GO TO 4
      ANU=ANU+0.01D0
      GO TO 5
    4 CDV=-DN/DNV
      RETURN
      END
C     *****************************************************************
      SUBROUTINE OVLAG
C     *****************************************************************
      IMPLICIT DOUBLE PRECISION(A-H,O-Z) 
      COMMON/OVLG/ANG1,ANG2,FOLAG,FOLAC,FOLAS,CD1,CD2,DG1,DG2,NNTG
      COMMON/SHEM1/HW,AMG0,AMB0,GAM0,BET0,BET4,GAMDE,GSHAPE
     */FUDN/Y,DN,ANU,DNV,CDV
      C1=SQRT(2.d0)
      G3=3.D0*GAM0
      IF(NNTG.EQ.11) CBG2=-27.D0/4.D0*AMG0**3*COS(G3)/SIN(G3)**3*C1
      YGH=-GSHAPE/AMG0*C1
C     PI3=1.0471975512D0
      PI3=4.d0/3.d0*atan(1.d0)      
      NSTEP=4.D0*(ANG1+ANG2+4.D0)
      STEP=PI3/AMG0/NSTEP*C1
      SUM2=0.D0
      S1M2=0.D0
      S2M2=0.D0
      SUM1=0.D0
      S1M1=0.D0
      S2M1=0.D0
      Y0=-GAM0/AMG0*C1
      NS2=NSTEP-1
      DO 1 I=1,NS2
      ANU=ANG1
      Y=Y0+STEP*I
      X=(Y+YGH)**NNTG
      IF(NNTG.EQ.11)X=(Y+YGH+CBG2)**2
      CALL FUDNU
      DN13=DN+CD1*DNV
      ANU=ANG2
      CALL FUDNU
      DN23=DN+CD2*DNV
      IF(I/2*2.EQ.I) GO TO 3
      SUM1=SUM1+X*DN13*DN23
      S1M1=S1M1+DN13**2
      S2M1=S2M1+DN23**2
      GO TO 1
    3 SUM2=SUM2+X*DN13*DN23
      S1M2=S1M2+DN13**2
      S2M2=S2M2+DN23**2
    1 CONTINUE
      FNR1=S1M2*2.D0+S1M1*4.D0
      FNR2=S2M2*2.D0+S2M1*4.D0
      FOLAG=SUM2*2.D0+SUM1*4.D0
      FOLAG=FOLAG/SQRT(FNR1*FNR2)*(AMG0/C1)**NNTG
      IF(NNTG.EQ.11) FOLAG=FOLAG/(AMG0/C1)**NNTG
      RETURN
      END
C     *****************************************************************
      SUBROUTINE KLEGO
C     *****************************************************************
      IMPLICIT DOUBLE PRECISION(A-H,O-Z) 
      COMMON/LOFAC/A(800)
      COMMON/KG/J1,J2,M1,M2,J,M,AKG
      IF(J1.LT.IABS(M1)) GO TO 1
      IF(J2.LT.IABS(M2)) GO TO 1
      IF(J.LT.IABS(M)) GO TO 1
      IF(M1+M2-M)1,2,1
    1 AKG=0.D0
      GO TO 14
    2 IF(IABS(J1-J2)-J)3,3,1
    3 IF(J1+J2-J)1,4,4
    4 NF=J1+J2-J
      IF(NF/2*2.NE.NF) GO TO 1
      K=J1+J2+J
      IF(M1.EQ.0.AND.M.EQ.0.AND.K/4*4.NE.K) GO TO 1
      FLN=A(NF+2)
      CL=FLN
      NF=J1-J2+J
      FLN=A(NF+2)
      CL=CL+FLN
      NF=J+J2-J1
      FLN=A(NF+2)
      CL=CL+FLN
      NF=J1+J2+J+2
      FLN=A(NF+2)
      CL=CL-FLN
      NF=J1+M1
      FLN=A(NF+2)
      CL=CL+FLN
      NF=J1-M1
      FLN=A(NF+2)
      CL=CL+FLN
      NF=J2+M2
      FLN=A(NF+2)
      CL=CL+FLN
      NF=J2-M2
      FLN=A(NF+2)
      CL=CL+FLN
      NF=J+M
      FLN=A(NF+2)
      CL=CL+FLN
      NF=J-M
      FLN=A(NF+2)
      CL=0.5*(CL+FLN)
      NF1=J1+J2-J
      NF2=J1-M1
      NF3=J2+M2
      NF4=J-J2+M1
      NF5=J-J1-M2
      NB=NF1
      NM=-NF4
      IF(NF2-NB)5,6,6
    5 NB=NF2
    6 IF(NF3-NB)7,8,8
    7 NB=NF3
    8 IF(-NF5.GT.NM) NM=-NF5
      IF(NM.LT.0) NM=0
      NM=NM+2
      NB=NB+2
      AKG=0.D0
      IF(NB.LT.NM) GO TO 14
      DO 13 I1=NM,NB,2
      C1=1.D0
      N=I1-2
      NF=N
      FLN=A(NF+2)
      CL1=CL-FLN
      NF=NF1-N
      FLN=A(NF+2)
      CL1=CL1-FLN
      NF=NF2-N
      FLN=A(NF+2)
      CL1=CL1-FLN
      NF=NF3-N
      FLN=A(NF+2)
      CL1=CL1-FLN
      NF=NF4+N
      FLN=A(NF+2)
      CL1=CL1-FLN
      NF=NF5+N
      FLN=A(NF+2)
      CL1=CL1-FLN
      IF(N/4*4.NE.N) C1=-1.D0
   13 AKG=AKG+C1*EXP(CL1)
      AKG=AKG*SQRT(J+1.D0)
   14 RETURN
      END
C     *****************************************************************
      SUBROUTINE TRLAG
C     *****************************************************************
      IMPLICIT DOUBLE PRECISION(A-H,O-Z) 
      COMMON/OVLG/ANG1,ANG2,FOLAG,FOLAC,FOLAS,CD1,CD2,DG1,DG2,NNTG
      COMMON/SHEM1/HW,AMG0,AMB0,GAM0,BET0,BET4,GAMDE,GSHAPE
     */FUDN/Y,DN,ANU,DNV,CDV
     */OVLG1/FOLC2,FOLS2,FOLC3
     */SHAMO/BET3,ETO,AMUO,HWO,BB32,DPAR
C     PI3=1.0471975512D0
      PI3=4.d0/3.d0*atan(1.d0)      
      C1=SQRT(2.d0)
      NSTEP=4.D0*(ANG1+ANG2+4.D0)
      STEP=PI3/AMG0/NSTEP*C1
      SCM2=0.D0
      SSM2=0.D0
      S1M2=0.D0
      S2M2=0.D0
      SCM1=0.D0
      SSM1=0.D0
      S1M1=0.D0
      S2M1=0.D0
      SCM22=0.D0
      SSM22=0.D0
      SCM12=0.D0
      SSM12=0.D0
      SCM13=0.D0
      SCM23=0.D0
      Y0=-C1*GAM0/AMG0
      DY1=-C1*DG1/AMG0
      DY2=-C1*DG2/AMG0
      NS2=NSTEP-1
      DO 1 I=1,NS2
      ANU=ANG1
      Y=Y0+STEP*I
      SI=SIN(GAM0+AMG0*Y/C1)
      CO=COS(GAM0+AMG0*Y/C1)
      SI2=SIN(2*(GAM0+AMG0*Y/C1))
      CO2=COS(2*(GAM0+AMG0*Y/C1))
      CO3=COS(3*(GAM0+AMG0*Y/C1))
      Y=Y0+STEP*I+DY1
      CALL FUDNU
      DN13=DN+CD1*DNV
      ANU=ANG2
      Y=Y0+STEP*I+DY2
      CALL FUDNU
      DN23=DN+CD2*DNV
      IF(I/2*2.EQ.I) GO TO 3
      SSM1=SSM1+SI*DN13*DN23
      SCM1=SCM1+CO*DN13*DN23
      SSM12=SSM12+SI2*DN13*DN23
      SCM13=SCM13+CO3*DN13*DN23
      SCM12=SCM12+CO2*DN13*DN23
      S1M1=S1M1+DN13**2
      S2M1=S2M1+DN23**2
      GO TO 1
    3 SSM2=SSM2+SI*DN13*DN23
      SCM2=SCM2+CO*DN13*DN23
      SSM22=SSM22+SI2*DN13*DN23
      SCM22=SCM22+CO2*DN13*DN23
      SCM23=SCM23+CO3*DN13*DN23
      S1M2=S1M2+DN13**2
      S2M2=S2M2+DN23**2
    1 CONTINUE
      FNR1=S1M2*2.D0+S1M1*4.D0
      FNR2=S2M2*2.D0+S2M1*4.D0
      FOLAS=SSM2*2.D0+SSM1*4.D0
      FOLAC=SCM2*2.D0+SCM1*4.D0
      FOLS2=SSM22*2.D0+SSM12*4.D0
      FOLC2=SCM22*2.D0+SCM12*4.D0
      FOLC3=SCM23*2.D0+SCM13*4.D0
      DF=SQRT(FNR1*FNR2)
      FOLAS=FOLAS/DF
      FOLAC=FOLAC/DF
      FOLS2=FOLS2/DF
      FOLC2=FOLC2/DF
      FOLC3=FOLC3/DF
      RETURN
      END
C     *****************************************************************
      SUBROUTINE OVLAGE
C     *****************************************************************
      IMPLICIT DOUBLE PRECISION(A-H,O-Z) 
      COMMON/OVLG/ANG1,ANG2,FOLAG,FOLAC,FOLAS,CD1,CD2,DG1,DG2,NNTG
      COMMON/SHEM1/HW,AMG0,AMB0,GAM0,BET0,BET4,GAMDE,GSHAPE
     */FUDN/Y,DN,ANU,DNV,CDV
     */OVKN/FOV(40,40,4),CG1,SG1,CG2,SG2,CSG11,CG4,SG4,CSG22,CSG31,
     *CSG13,CG3,SG3,CSG21,CSG12
     */ENA/EN,EL(40),BET(10),NUR,NMAX,NPD,LAS
     */SHAMO/BET3,ETO,AMUO,HWO,BB32,DPAR
C     PI3=1.0471975512D0
      PI3=4.d0/3.d0*atan(1.d0)      
      C1=SQRT(2.d0)
      NSTEP=4.*(ANG1+ANG2+4.)
      STEP=PI3/AMG0/NSTEP*C1
      SUF11=0.D0
      SUF12=0.D0
      SUF21=0.D0
      SUF22=0.D0
      C11=0.D0
      S11=0.D0
      C12=0.D0
      S12=0.D0
      C21=0.D0
      C22=0.D0
      S21=0.D0
      S22=0.D0
      CS111=0.D0
      CS112=0.D0
      C31=0.D0
      C32=0.D0
      S31=0.D0
      S32=0.D0
      CS121=0.D0
      CS122=0.D0
      CS211=0.D0
      CS212=0.D0
      C41=0.D0
      C42=0.D0
      S41=0.D0
      S42=0.D0
      CS221=0.D0
      CS222=0.D0
      CS311=0.D0
      CS312=0.D0
      CS131=0.D0
      CS132=0.D0
      Y0=-C1*GAM0/AMG0
      DY1=-C1*DG1/AMG0
      DY2=-C1*DG2/AMG0
      NS2=NSTEP-1
      DO 1 I=1,NS2
      Y=Y0+STEP*I
      SI=SIN(GAM0+GSHAPE+AMG0*Y/C1)
      CO=COS(GAM0+GSHAPE+AMG0*Y/C1)
      ANU=ANG1
      Y=Y0+STEP*I+DY1
      CALL FUDNU
      DN1 =DN+CD1*DNV
      ANU=ANG2
      Y=Y0+STEP*I+DY2
      CALL FUDNU
      DN2 =DN+CD2*DNV
      DN12=DN1*DN2
      IF(I/2*2.EQ.I) GO TO 3
      SUF11=SUF11+DN1*DN1
      SUF21=SUF21+DN2*DN2
      CODN=CO*DN12
      SIDN=SI*DN12
      C11=C11+CODN
      S11=S11+SIDN
      IF(LAS.EQ.1) GO TO 1
      CO2DN=CO*CODN
      SI2DN=SI*SIDN
      C21=C21+CO2DN
      S21=S21+SI2DN
      CS111=CS111+CO*SIDN
      IF(LAS.EQ.2) GO TO 1
      CO3DN=CO*CO2DN
      SI3DN=SI*SI2DN
      C31=C31+CO3DN
      S31=S31+SI3DN
      CS121=CS121+CO*SI2DN
      CS211=CS211+SI*CO2DN
      IF(LAS.EQ.3) GO TO 1
      C41=C41+CO*CO3DN
      S41=S41+SI*SI3DN
      CS221=CS221+CO2DN*SI2DN/DN12
      CS311=CS311+SI*CO3DN
      CS131=CS131+CO*SI3DN
      GO TO 1
    3 SUF12=SUF12+DN1*DN1
      SUF22=SUF22+DN2*DN2
      CODN=CO*DN12
      SIDN=SI*DN12
      C12=C12+CODN
      S12=S12+SIDN
      IF(LAS.EQ.1) GO TO 1
      CO2DN=CO*CODN
      SI2DN=SI*SIDN
      C22=C22+CO2DN
      S22=S22+SI2DN
      CS112=CS112+CO*SIDN
      IF(LAS.EQ.2) GO TO 1
      CO3DN=CO*CO2DN
      SI3DN=SI*SI2DN
      C32=C32+CO3DN
      S32=S32+SI3DN
      CS122=CS122+CO*SI2DN
      CS212=CS212+SI*CO2DN
      IF(LAS.EQ.3) GO TO 1
      C42=C42+CO*CO3DN
      S42=S42+SI*SI3DN
      CS222=CS222+CO2DN*SI2DN/DN12
      CS312=CS312+SI*CO3DN
      CS132=CS132+CO*SI3DN
    1 CONTINUE
      SUF1=SUF11*4.D0+SUF12*2.D0
      SUF2=SUF21*4.D0+SUF22*2.D0
      ANOR=SQRT(SUF1*SUF2)
      CG1=(C11*4.D0+C12*2.D0)/ANOR
      SG1=(S11*4.D0+S12*2.D0)/ANOR
      IF(LAS.EQ.1) GO TO 4
      CG2=(C21*4.D0+C22*2.D0)/ANOR
      SG2=(S21*4.D0+S22*2.D0)/ANOR
      CSG11=(CS111*4.D0+CS112*2.D0)/ANOR
      IF(LAS.EQ.2) GO TO 4
      CG3=(C31*4.D0+C32*2.D0)/ANOR
      SG3=(S31*4.D0+S32*2.D0)/ANOR
      CSG21=(CS211*4.D0+CS212*2.D0)/ANOR
      CSG12=(CS121*4.D0+CS122*2.D0)/ANOR
      IF(LAS.EQ.3) GO TO 4
      CG4=(C41*4.D0+C42*2.D0)/ANOR
      SG4=(S41*4.D0+S42*2.D0)/ANOR
      CSG22=(CS221*4.D0+CS222*2.D0)/ANOR
      CSG31=(CS311*4.D0+CS312*2.D0)/ANOR
      CSG13=(CS131*4.D0+CS132*2.D0)/ANOR
    4 CONTINUE
      RETURN
      END
C     *****************************************************************
      SUBROUTINE OVLAB
C     *****************************************************************
      IMPLICIT DOUBLE PRECISION(A-H,O-Z) 
      COMMON/FUDN/Y,DN,ANU,DNV,CDV
     */OVLB/X1,P1,ANU1,X2,P2,ANU2,FOLAR,NNT
      AH=0.2D0
      C1=SQRT(2.d0)
      C=X2*P1/P2/X1
      IF(C.GT.1.) GO TO 1
      STEP1=AH
      STEP2=AH*C
      GO TO 2
    1 STEP2=AH
      STEP1=AH/C
    2 STEP=-STEP1*P1/X1/C1
      N=1
      SQX2=C1*X2
      SQX1=C1*X1
      ANU=ANU1
      Y=SQX1+STEP1
      CALL FUDNU
      DN12=DN
      ANU=ANU2
      Y=SQX2+STEP2
      CALL FUDNU
      DN22=DN
      SUM2=0.D0
      S1M2=0.D0
      S2M2=0.D0
      FUYN=STEP**NNT
      IF(NNT.EQ.10) FUYN=(STEP-1.D0)/STEP/STEP
      IF(NNT.EQ.11) FUYN=(STEP**3-1.D0)/STEP/STEP
      IF(NNT.EQ.12) FUYN=STEP-1.D0
      IF(NNT.EQ.13) FUYN=1.D0/STEP/STEP
      SUM1=FUYN*DN12*DN22
      S1M1=DN12*DN12
      S2M1=DN22*DN22
    6 N=N+1
      YY=STEP*N
      FUYN=YY**NNT
      IF(NNT.EQ.10) FUYN=(YY-1.D0)/YY/YY
      IF(NNT.EQ.11) FUYN=(YY**3-1.D0)/YY/YY
      IF(NNT.EQ.12) FUYN=YY-1.
      IF(NNT.EQ.13) FUYN=1./YY/YY
      ANU=ANU1
      Y=SQX1+N*STEP1
      CALL FUDNU
      DN13=DN
      ANU=ANU2
      Y=SQX2+N*STEP2
      CALL FUDNU
      DN23=DN
      IF (N/2*2.EQ.N) GO TO 3
      SUM1=SUM1+FUYN*DN13*DN23
      S1M1=S1M1+DN13*DN13
      S2M1=S2M1+DN23*DN23
      GO TO 4
    3 SUM2=SUM2+FUYN*DN13*DN23
      S1M2=S1M2+DN13*DN13
      S2M2=S2M2+DN23*DN23
    4 IF(ABS(DN13).LT.1.D-4*SQRT(S1M1).AND.ABS(DN23).
     *LT.1.D-4*SQRT(S2M1)) GO TO 5
      GO TO 6
    5 FNR1=S1M2*2.D0+S1M1*4.D0
      FNR2=S2M2*2.D0+S2M1*4.D0
      FOLAR=SUM2*2.D0+SUM1*4.D0
      FOLAR=FOLAR/SQRT(FNR1*FNR2)
      RETURN
      END
C     *****************************************************************
      SUBROUTINE OVLAO
C     *****************************************************************
      IMPLICIT DOUBLE PRECISION(A-H,O-Z) 
      COMMON/FUDN/Y,DN,ANU,DNV,CDV
     */OVLB/X1,P1,ANU1,X2,P2,ANU2,FOLAR,NNT
     */SHAMO/BET3,ETO,AMUO,HWO,BB32,DPAR
      AH=0.2D0
      C1=SQRT(2.d0)
      STEPP=AH
      STEP=STEPP*AMUO/C1
      N=1
      ANU=ANU1
      Y=STEPP
      CALL FUDNU
      DN12=DN
      ANU=ANU2
      CALL FUDNU
      DN22=DN
      SUM2=0.D0
      S1M2=0.D0
      S2M2=0.D0
      FUYN=STEP**NNT
      SUM1=FUYN*DN12*DN22
      S1M1=DN12*DN12
      S2M1=DN22*DN22
    6 N=N+1
      YY=STEP*N
      FUYN=YY**NNT
      ANU=ANU1
      Y=N*STEPP
      CALL FUDNU
      DN13=DN
      ANU=ANU2
      CALL FUDNU
      DN23=DN
      IF (N/2*2.EQ.N) GO TO 3
      SUM1=SUM1+FUYN*DN13*DN23
      S1M1=S1M1+DN13*DN13
      S2M1=S2M1+DN23*DN23
      GO TO 4
    3 SUM2=SUM2+FUYN*DN13*DN23
      S1M2=S1M2+DN13*DN13
      S2M2=S2M2+DN23*DN23
    4 IF(ABS(DN13).LT.1.D-4*SQRT(S1M1).AND.ABS(DN23).
     *LT.1.D-4*SQRT(S2M1)) GO TO 5
      GO TO 6
    5 FNR1=S1M2*2.D0+S1M1*4.D0
      FNR2=S2M2*2.D0+S2M1*4.D0
      FOLAR=SUM2*2.D0+SUM1*4.D0
      FOLAR=FOLAR/SQRT(FNR1*FNR2)
      RETURN
      END
C     *****************************************************************
      SUBROUTINE INERMO
C     *****************************************************************
      IMPLICIT DOUBLE PRECISION(A-H,O-Z) 
      COMMON/MAT/GAM,IS,NO
      COMMON/INRM/AMO,BMO,CMO,BB42,GAMG,DELG
     */SHAMO/BET3,ETO,AMUO,HWO,BB32,DPAR
     */MENU/MEJOB,MEPOT,MEHAM,MECHA,MEPRI,MESOL,MESHA,MESHO,MEHAO
     *,MEAPP,MEVOL,MEREL,MECUL,MERZZ,MERRR,MEDIS,MERIP
      COMMON/SHEM1/HW,AMG0,AMB0,GAM0,BET0,BET4,GAMDE,GSHAPE
     */PB/PBET2,PBET3
      PI23=2.0943951024D0
      C1=SQRT(2.d0)
      C2=3.16227766017D0
      C3=2.64575131106D0
      ARG1=GAM-PI23
      ARG2=GAM-2.D0*PI23
      ARG3=GAM
      ARG1=GAM-PI23+GSHAPE
      ARG2=GAM-2.*PI23+GSHAPE
      ARG3=GAM+GSHAPE
      AMO=SIN(ARG1)**2
      BMO=SIN(ARG2)**2
      CMO=SIN(ARG3)**2
      PBE22=1./PBET2**2
      IF(MESHA-2) 1,2,3
    2 AMO=AMO+BB42*2.5D0*PBE22
      BMO=BMO+BB42*2.5D0*PBE22
      GO TO 1
    3 C7=7.D0/12.D0
      C5=5.D0/12.D0
      CQ7=SQRT(C7)
      CQ5=SQRT(C5)
      IF(MESHA.EQ.4) CSDG=COS(DELG)
      IF(MESHA.EQ.4) GO TO 4
      GAMG=GAM
      CSDG=CQ7*COS(3.D0*GAM)
    4 SSDG=SQRT(1.D0-CSDG*CSDG)
      CSGG=COS(GAMG)
      SSGG=SIN(GAMG)
      A40=CQ7*CSDG+CQ5*SSDG*CSGG
      A42=-SSDG*SSGG/C1
      A44=(CQ5*CSDG-CQ7*SSDG*CSGG)/C1
      AB1=2.5D0*A40*A40+4.*A42*A42+A44*A44
      AB2=1.5D0*C2*A40*A42+C3*A42*A44
      AMO=AMO+BB42*(AB1+AB2)*PBE22
      BMO=BMO+BB42*(AB1-AB2)*PBE22
      CMO=CMO+2.D0*BB42*(A42*A42+4.D0*A44*A44)*PBE22
    1 PBET32=PBET3**2*PBE22
      IF(MESHO-1) 5,6,7
    6 AMO=AMO+BB32*1.5D0*PBET32
      BMO=BMO+BB32*1.5D0*PBET32
      GO TO 5
    7 C30=SQRT(30.D0)
      A30=COS(ETO)
      A32=SIN(ETO)/C1
      AB1=1.5D0*A30*A30+2.D0*A32*A32
      AB2=0.5D0*C30*A30*A32
      AMO=AMO+BB32*(AB1+AB2)*PBET32
      BMO=BMO+BB32*(AB1-AB2)*PBET32
      CMO=CMO+2.D0*BB32*A32*A32*PBET32
    5 AMO=1.D0/AMO
      BMO=1.D0/BMO
      CMO=1.D0/CMO
      RETURN
      END
C     *************************************************************
      DOUBLE PRECISION FUNCTION GAMMA(XX)
C     *************************************************************
      DOUBLE PRECISION COF(6),STP,HALF,ONE,FPF,X,TMP,SER,XX
      DATA COF,STP/76.18009173D0,-86.50532033D0,24.01409822D0,
     *    -1.231739516D0,.120858003D-2,-.536382D-5,2.50662827465D0/
      DATA HALF,ONE,FPF/0.5D0,1.0D0,5.5D0/
      X=XX-ONE
      TMP=X+FPF
      TMP=TMP**(X+HALF)*EXP(-TMP)
      SER=ONE
      DO 11 J=1,6
        X=X+ONE
        SER=SER+COF(J)/X
11    CONTINUE
      GAMMA=TMP*STP*SER
      RETURN
      END
C     *****************************************************************
      SUBROUTINE TRANS
C     *****************************************************************
      IMPLICIT DOUBLE PRECISION(A-H,O-Z) 
      COMMON/MAT/GAM,IS,NO
      COMMON/INRM/AMO,BMO,CMO,BB42,GAMG,DELG
      COMMON/SHEMM/ES(40),JU(40),NTU(40),NNB(40),NNG(40),NNO(40),
     *NPI(40)
      COMMON/ENA/EN,EL(40),BET(10),NUR,NMAX,NPD,LAS
      COMMON/SHEM1/HW,AMG0,AMB0,GAM0,BET0,BET4,GAMDE,GSHAPE
     */SHEM2/GIT(40),EP0(40),EP1(40),EP2(40),EP12(40),ANG(40,2),EPB(40),
     *EPG(40),EGB(40),AIT(31,16),PT(40),FOG(2,8,8),XI(40),ANB(40),
     *CD(40,2)
      COMMON/OVLG/ANG1,ANG2,FOLAG,FOLAC,FOLAS,CD1,CD2,DG1,DG2,NNTG
     */MENU/MEJOB,MEPOT,MEHAM,MECHA,MEPRI,MESOL,MESHA,MESHO,MEHAO
     *,MEAPP,MEVOL,MEREL,MECUL,MERZZ,MERRR,MEDIS,MERIP
     */FUDN/Y,DN,ANU,DNV,CDV
     */OVLB/X1,P1,ANU1,X2,P2,ANU2,FOLAR,NNT
      COMMON/KG/J1,J2,M1,M2,J,M,AKG
     */TR/B2(41,41),B4(41,41),B3(41,41)/NP/NPJ(25)
     */OVLG1/FOLC2,FOLS2,FOLC3/TR1/B0(41,41)
     */SHAMO/BET3,ETO,AMUO,HWO,BB32,DPAR
     */AA/ANO(40)
          IF(MEPRI.LT.99) PRINT 10
          WRITE(21,10)
          IF(MEHAM.EQ.5 .AND. MEPRI.LT.99)  PRINT 13
          IF(MEHAM.EQ.5)  WRITE(21,13)
          IF(MEHAM.EQ.3 .AND. MEPRI.LT.99)  PRINT 14
          IF(MEHAM.EQ.3)  WRITE(21,14)
          IF(MEPRI.LT.99) PRINT 15
          WRITE(21,15)
          IF(MEPRI.LT.99) PRINT 11
          WRITE(21,11)
   10     FORMAT(30X,'E2-TRANSITION PROBABILITIES B(E2:I->F)')
   11     FORMAT(1X,120(1H-))
   13     FORMAT(20X,'( CALCULATIONS INVOLVE  5-PARAM. MODEL')

   14     FORMAT(20X,'( CALCULATIONS INVOLVE DAVYDOV-CHABAN MODEL')
   15     FORMAT(20X,'TAKING INTO ACCOUNT SQUARE A(LAM,MU) TERMS )')
   16     FORMAT (30X,'E3-TRANSITION PROBABILITIES B(E3;I->F)')
  110     FORMAT(30X,' E4-TRANSITION PROBABILITIES B(E4:I->F)')
      C1=SQRT(2.d0)
      PI=4.d0*atan(1.d0)
      SQPI=SQRT(PI)
      SQ5=SQRT(5.D0)
      CQ57=SQRT(5.D0/7.D0)
      CQ59=SQRT(5.D0)/3.d0
      CQ79=SQRT(7.D0)/3.d0
      C00=5.d0*SQ5/21.d0/SQPI
      IF(MESHA-2) 21,22,23
C     LAMBDA4 AXIAL DEFORMATION
   22 A40=1.D0
      A42=0.D0
      A44=0.D0
      GO TO 21
C     LAMBDA4 NON-AXIAL DEFORMATION
   23 C712=7.D0/12.D0
      C512=5.D0/12.D0
      CQ712=SQRT(C712)
      CQ512=SQRT(C512)
      IF(MESHA.EQ.4) CSDG=COS(DELG)
      IF(MESHA.EQ.4) GO TO 24
      GAMG=GAM
      CSDG=CQ712*COS(3.D0*GAM)
   24 SSDG=SQRT(1.D0-CSDG*CSDG)
      CSGG=COS(GAMG)
      SSGG=SIN(GAMG)
      A40=CQ712*CSDG+CQ512*SSDG*CSGG
      A42=-SSDG*SSGG/C1
      A44=(CQ512*CSDG-CQ712*SSDG*CSGG)/C1
   21 IF(MESHO-1) 25,26,27
C     LAMBDA3 AXIAL DEFORMATION
   26 A30=1.D0
      A32=0.D0
      GO TO 25
C     LAMBDA3 NON-AXIAL DEFORMATION
   27 A30=COS(ETO)
      A32=SIN(ETO)/C1
   25 SIGO=SIN(GAM0+GSHAPE)
      COGO=COS(GAM0+GSHAPE)
      FOLAC=COS(GAM0+GSHAPE)
      FOLAS=SIN(GAM0+GSHAPE)
      FOLC2=COS(2.D0*(GAM0+GSHAPE))
      FOLS2=SIN(2.D0*(GAM0+GSHAPE))
      FLC2=COS(GAM0+GSHAPE)**2
      FLS2=SIN(GAM0+GSHAPE)**2
      FOLC3=COS(3.D0*(GAM0+GSHAPE))
      C3G0=COS(3.D0*(GAM0+GSHAPE))
      FOKS=BET3
      FOKS1=BET3*BET3
C
C     VECTOR COEFFICIENTS FOR E2-,E3- � E4-TRANSITIONS
C
      QQQ00=-0.5345224838D0
      QQQ22=-QQQ00
      GQQ00=0.7171371656D0
      GQQ20=0.1195228609D0
      GQQ22=0.4629100500D0
      GQQ24=1.D0
      GQG00=-0.5096471915D0
      GQG20=0.5921565255D0
      GQG22=-0.2038588766D0
      GQG02=GQG20
      GQG42=0.3302891295D0
      GQG44=0.7135060680D0
      GQG24=GQG42
      QGG00=CQ59*GQG00
      QGG20=CQ59*GQG22
      QGG02=CQ59*GQG02
      QGG42=CQ59*GQG24
      QGG40=CQ59*GQG44
      QQG00=CQ59*GQQ00
      QQG20=CQ59*GQQ22
      QQG22=QQG20
      QQG02=CQ59*GQQ20
      QQG42=CQ59*GQQ24
      GOO00=-0.4834937784D0
      GOO20=0.5640760748D0
      GOO02=0.1395726315D0
      GOO24=-0.6741998625D0
      GOO22=GOO02
      GGG00=0.4022911408D0
      GGG20=-0.245844586D0
      GGG40=0.3128931094D0
      GGG22=GGG20
      GGG42=0.560968194D0
      GGG44=GGG40
      GGG24=GGG42
      OQO00=-0.5163977795D0
      OQO20=0.5773502692D0
      OQO02=OQO20
      OQO22=0.D0
      OOG00=-CQ79*GOO00
      OOG20=-CQ79*GOO20
      OOG22=-CQ79*GOO22
      OOG02=-CQ79*GOO20
      OOG42=-CQ79*GOO24
      QOO00=-CQ57*OQO00
      QOO02=-CQ57*OQO02
      QOO20=-CQ57*OQO22
C
C     CONSTANTS FOR E2-TRANSITIONS
C     Q20=3.*Z*e*RR*2*BETTA20/SQRT(5*PI)
C     CONST=5*Q20**2/16/PI   RESULTS TO BE MULTIPLIED BY
C
      QCG1A=6.D0*BET4/SQPI
      QSG1A=QCG1A*C1
      QC2GA=SQ5/SQPI*BET0
      QG0A=3.D0*BET4**2/SQ5/SQPI/BET0
      QO0A=7.D0/SQPI/SQ5/BET0
C     QO0A=7.D0/SQPI/SQ5/BET0*BET3**2
      QSG1B=QCG1A/C1
      QCG1B=QCG1A
      QS2GB=QC2GA/C1
      QG0B=QG0A*2.D0
      QO0B=QO0A*2.D0
      AC2=1.D0+QCG1A*QQG00**2*A40
      AS2=QSG1A*QQG00*QQG20*A42
      AC22=QC2GA*QQQ00**2
      AQG2=QG0A*QGG00*(2.D0*QGG40*A44**2+2.D0*QGG20*A42**2+
     1QGG00*A40**2)
      AQO2=QO0A*QOO00*(QOO00*A30**2+2.D0*QOO20*A32**2)
      BS2=1.D0/C1+QSG1B*QQG00*(QQG02*A40+QQG42*A44)
      BC2=QCG1B*QQG00*QQG22*A42
      BS22=QS2GB*QQQ00*QQQ22
      BQG2=QG0B*QGG00*(QGG02*A40*A42+QGG42*A42*A44)
      BQO2=QO0B*QOO00*QOO02*A30*A32
C
C     CONSTANTS FOR E3-TRANSITIONS
C     Q30=3.*Z*e*RR*3*BETTA30/SQRT(7*PI)
C     CONST=7*Q30**2/16/PI RESULTS TO BE MULTIPLIED BY
C
      OC1=2.5D0*SQ5/SQPI*BET0
      OC2=7.5D0*BET4/SQPI
      AC3=OC1*OQO00**2*COS(ETO)
      AS3=OC1*OQO00*OQO20*SIN(ETO)*0.5D0
      AC00=COS(ETO)
      AOG3=OC2*OOG00*(OOG00*COS(ETO)*A40+C1*OOG20*SIN(ETO)*A42)
      BC3=OC1*OQO00*OQO22*SIN(ETO)/C1
      BS3=OC1*OQO00*OQO02*COS(ETO)/C1
      BS00=SIN(ETO)/C1
      BOG3=OC2*OOG00*(OOG22*COS(ETO)*A42+SIN(ETO)/C1*
     1(OOG02*A40+OOG42*A44))
C
C     CONSTANTS FOR E4-TRANSITIONS
C     Q40=3.*Z*e*RR*4*BETTA40/SQRT(9*PI)
C     CONST=9*Q40**2/16/PI  RESULTS TO BE MULTIPLIED BY
C
      A1=2.5D0/SQPI*BET0**2/BET4
      A2=3.D0*SQ5/SQPI*BET0
      A3=4.5D0/SQPI*BET4
      A4=3.5D0/SQPI/BET4
C     A4=3.5D0/SQPI/BET4*BET3**2
      BB1=A1/C1
      BB2=A2
      BB3=A3*2.D0
      BB4=A4*2.D0
      G1=A1/2.D0
      G2=A2
      G3=A3
      G4=A4
      AC4=A2*GQG00**2*A40
      AS4=A2*GQG00*GQG20*C1*A42
      AC24=A1*GQQ00**2
      AS24=A1*GQQ00*GQQ20
      AGG4=A3*GGG00*(GGG00*A40**2+2.D0*GGG20*A44**2+
     12.D0*GGG40*A44**2)
      AGO4=A4*GOO00*(GOO00*A30**2+2.D0*GOO20*A32**2)
      BC4=BB2*GQG00*GQG22*A42
      BS4=BB2*GQG00*(GQG02*A40+GQG42*A44)/C1
      BS24=BB1*GQQ00*GQQ22
      BGG4=BB3*GGG00*(GGG22*A40*A42+GGG42*A44*A42)
      BGO4=BB4*GOO00*GOO22*A30*A32
      CC4=G2*GQG00**2*A44
      CS4=G2*GQG00*GQG24*A42/C1
      CS24=G1*GQG00*GQG24
      CGG4=G3*GGG00*(2.D0*GGG44*A40*A44+GGG24*A42**2)
      CGO4=G4*GOO00*GOO24*A32**2
C     CALCULATIONS OF E2-TRANSITION PROBABILITIES
      J2=4
      DO 1 I=1,NUR
      JI=JU(I)
      PO1=(-1)**NNO(I)
      DG1=0.
      IF(PO1.NE.1) DG1=GAMDE
      KG1=(3-PO1)/2
      NG1I=NNG(I)+1
      ANG1=ANG(NG1I,KG1)
      NO1I=NNO(I)+1
      AONU1=ANO(NO1I)
      CD1=CD(NG1I,KG1)
      J1=2*JI
      KMI=1-(-1)**(JI+NNO(I))
      NKI=(2*(JI/2)-KMI+2)/2
      DO 1 JJ=1,NUR
      JF=JU(JJ)
      PO2=(-1)**NNO(JJ)
      DG2=0.
      IF(PO2.NE.1) DG2=GAMDE
      KG2=(3-PO2)/2
      NG1F=NNG(JJ)+1
      ANG2=ANG(NG1F,KG2)
      NO1F=NNO(JJ)+1
      AONU2=ANO(NO1F)
      CD2=CD(NG1F,KG2)
      J=2*JF
      JIF=IABS(JI-JF)
      IF(JIF.GT.2) GO TO 111
      IF(JI.EQ.0.AND.JF.EQ.0) GO TO 111
      IF(PO1.NE.PO2) GO TO 111
      IF(MEHAO.EQ.0) GO TO 45
      IF(MEHAO.LT.2) GO TO 51
      EBM=EXP(-(BET3/AMUO)**2)
      FOKS1=AMUO**2/2.D0+BET3**2/(1.D0-(-1)**NO1I*EBM)
      IF(MEHAO.GE.2) GO TO 45
   51 NNT=2
      X1=-BET3/AMUO
      X2=X1
      P1=BET3
      P2=BET3
      ANU1=AONU1
      ANU2=AONU2
      IF(BET3.EQ.0.D0) CALL OVLAO
      IF(BET3.NE.0.D0) CALL OVLAB
      FOKS1=FOLAR
   45 IF(MEHAM.GE.5)  CALL TRLAG
      X1= XI(I)
      X2= XI(JJ)
      P1=PT(I)
      P2=PT(JJ)
      ANU1=ANB(I)
      ANU2=ANB(JJ)
      FOLAR2=1.D0
      IF(MEHAM.EQ.4) GO TO 12
      NNT=3
      CALL OVLAB
      FOLAR2=FOLAR
   12 NNT=2
      IF(MEHAM.NE.4) CALL OVLAB
      IF(MEHAM.EQ.4) FOLAR=1.
      FOLAR1=FOLAR
      IF(MEHAO.EQ.2) FOKS1=FOKS1*FOLAR1*BET0**2
      NNT=1
      IF(MEHAM.NE.4) CALL OVLAB
      IF(MEHAM.EQ.4) FOLAR=1.
      NNTG=1
      IF(MEHAM.GT.4)CALL OVLAG
      KMF=1-(-1)**(JF+NNO(JJ))
      NKF=(2*(JF/2)-KMF+2)/2
      SUM=0.D0
      DO 2 JKI=1,NKI
      KI=KMI+2*(JKI-1)
      DI=1.D0
      IF(KI.EQ.0) DI=C1
      DO 2 JKF=1,NKF
      KF=KMF+2*(JKF-1)
      SUM1=0.D0
      DF=1.D0
      IF(KF.EQ.0) DF=C1
      M=2*KF
      IF(KF.NE.KI) GO TO 3
      M1=2*KI
      M2=0
      CALL KLEGO
      T1=AKG
      IF(KF.NE.0) GO TO 4
      M1=-2*KI
      CALL KLEGO
      T1=T1+AKG*(-1)**(JI+NNO(I))
    4 SUM1=SUM1 +T1*((AC2*FOLAC+AS2*FOLAS)*FOLAR+
     1AC22*FOLC2*FOLAR1+AQG2+AQO2*FOKS1)
    3 T1=0.
      IF((KI+2).NE.KF) GO TO 5
      M1=2*KI
      M2=4
      CALL KLEGO
      T1=AKG
    5 IF((KI-2).NE.KF) GO TO 6
      M1=2*KI
      M2=-4
      CALL KLEGO
      T1=T1+AKG
    6 IF((2-KI).NE.KF) GO TO 7
      M1=-2*KI
      M2=4
      CALL KLEGO
      T1=T1+AKG*(-1)**(JI+NNO(I))
    7 SUM1=SUM1 +T1*((BC2*FOLAC+BS2*FOLAS)*FOLAR+BS22*FOLS2*FOLAR1+
     1BQG2+BQO2*FOKS1)
      SUM=SUM+SUM1*AIT(I,JKI)*AIT(JJ,JKF)/DI/DF
    2 CONTINUE
      IF(I.NE.JJ) GO TO 20
      Q1= SQRT(JI*(2.D0*JI-1.D0)/((JI+1.D0)*(2.D0*JI+1.)*(2.D0*JI+3.)))
      Q1=Q1*SUM*SQRT(2.D0*JI+1.D0)
      IF(MEPRI.LT.99) 
     *  PRINT 17,ES(I),JI,NTU(I),NNB(I),NNG(I),NNO(I),Q1
      WRITE(21,17)ES(I),JI,NTU(I),NNB(I),NNG(I),NNO(I),Q1
   17 FORMAT(10X,'AVERAGE VALUE OF LAMBDA2 MOMENT FOR STATE  E=',
     *D11.4,'JI=',I2,'TAU=',I1,'NB=',I1,'NG=',I1,'NO=',I1,3X,
     *'DEVIDED BY Q20=',D11.4)
      GO TO 1
   20 B2(I,JJ)=SUM*SUM
      IF(MEPRI.LT.99) PRINT 11
      WRITE(21,11)
      IF(MEPRI.LT.99) 
     *  PRINT 8, ES(I),JI,NTU(I),NNB(I),NNG(I),NNO(I),
     *         ES(JJ),JF,NTU(JJ),NNB(JJ),NNG(JJ),NNO(JJ),B2(I,JJ)
      WRITE(21,8) ES(I),JI,NTU(I),NNB(I),NNG(I),NNO(I),
     *         ES(JJ),JF,NTU(JJ),NNB(JJ),NNG(JJ),NNO(JJ),B2(I,JJ)
      IF(MEPRI.LT.99) PRINT 11
      WRITE(21,11)
      IF(MEPRI.LT.99) 
     *  PRINT 9,FOLAC,FOLAS,FOLAR,COGO,SIGO,FOKS1,FOLAG
      WRITE(21,9)FOLAC,FOLAS,FOLAR,COGO,SIGO,FOKS1,FOLAG
    9 FORMAT(5X,'I(COS)=',D11.4,'  I(SIN)=',D11.4,
     *'  FOLAR=',D11.4,'  COS(G0)=',D11.4,'  SIN(G0)=',
     *D11.4,'  FOKS1=', D11.4,' FOLAG=',D11.4//)
    8 FORMAT(5X,'B(E2:(',D11.4,')JI=',I2,'TAU=',I1,'NB=',
     *I1,'NG=',I1,'NO=',I1,'--->(',D11.4,')JF=',I2,'TAU=',I1,'NB=',
     *I1,'NG=',I1,'NO=',I1,')=',D11.4)
C     LAMBDA0 TRANSITION PROBABILITIES
  111 IF(JIF.NE.0) GO TO 1
      IF(I.EQ.JJ) GO TO 1
      IF(MEHAM.EQ.4) GO TO 1
      B0(I,JJ)=(BET0*BET0*FOLAR1+C00*BET0**3*FOLAR2*FOLC3)**2
      IF(MEPRI.LT.99) PRINT 11
      WRITE(21,11)
      IF(MEPRI.LT.99) 
     *  PRINT 81,ES(I),JI,NTU(I),NNB(I),NNG(I),NNO(I),
     *         ES(JJ),JF,NTU(JJ),NNB(JJ),NNG(JJ),NNO(JJ),B0(I,JJ)
      WRITE(21,81)ES(I),JI,NTU(I),NNB(I),NNG(I),NNO(I),
     *         ES(JJ),JF,NTU(JJ),NNB(JJ),NNG(JJ),NNO(JJ),B0(I,JJ)
   81 FORMAT(5X,'B(E0:(',D11.4,')JI=',I2,'TAU=',I1,'NB=',
     *I1,'NG=',I1,'NO=',I1,'--->(',D11.4,')JF=',I2,'TAU=',I1,'NB=',
     *I1,'NG=',I1,'NO=',I1,')=',D11.4)
      IF(MEPRI.LT.99) PRINT 91,FOLC3,FOLAR2,FOLAR1,C3G0
      WRITE(21,91)FOLC3,FOLAR2,FOLAR1,C3G0
   91 FORMAT(5X,'I(COS(3*GAM0))=',D11.4,' FOLAR2 =',D11.4,
     *' FOLAR1=',D11.4,'  COS(3*G0)=',D11.4/)
    1 CONTINUE
      IF(MESHA.LT.2) GO TO 100
          IF(MEPRI.LT.99) PRINT 110
          WRITE(21,110)
          IF(MEPRI.LT.99) PRINT 11
          WRITE(21,11)
C     CALCULATIONS OF E4- TRANSITION PROBABILITIES
      J2=8
      DO 19 I=1,NUR
      JI=JU(I)
      PO1=(-1)**NNO(I)
      DG1=0.
      IF(PO1.NE.1) DG1=GAMDE
      KG1=(3-PO1)/2
      NG1I=NNG(I)+1
      ANG1=ANG(NG1I,KG1)
      NO1I=NNO(I)+1
      AONU1=ANO(NO1I)
      CD1=CD(NG1I,KG1)
      J1=2*JI
      KMI=1-(-1)**(JI+NNO(I))
      NKI=(2*(JI/2)-KMI+2)/2
      DO 19 JJ=1,NUR
      JF=JU(JJ)
      PO2=(-1)**NNO(JJ)
      DG2=0.
      IF(PO2.NE.1) DG2=GAMDE
      KG2=(3-PO2)/2
      NG1F=NNG(JJ)+1
      ANG2=ANG(NG1F,KG2)
      NO1F=NNO(JJ)+1
      AONU2=ANO(NO1F)
      CD2=CD(NG1F,KG2)
      J=2*JF
      JIF=IABS(JI-JF)
      IF(JIF.GT.4) GO TO 19
      IF(JI.EQ.0.AND.JF.EQ.0) GO TO 19
      IF(PO1.NE.PO2) GO TO 19
      IF(MEHAO.EQ.0) GO TO 46
      IF(MEHAO.LT.2) GO TO 52
      EBM=EXP(-(BET3/AMUO)**2)
      FOKS1=AMUO**2/2.D0+BET3**2/(1.D0-(-1)**NO1I*EBM)
      IF(MEHAO.GE.2) GO TO 46
   52 NNT=2
      X1=-BET3/AMUO
      X2=X1
      P1=BET3
      P2=BET3
      ANU1=AONU1
      ANU2=AONU2
      IF(BET3.EQ.0.D0) CALL OVLAO
      IF(BET3.NE.0.D0) CALL OVLAB
      FOKS1=FOLAR
   46 IF(MEHAM.GE.5)  CALL TRLAG
      FLC2=(1.D0+FOLC2)/2.D0
      FLS2=(1.D0-FOLC2)/2.D0
      X1= XI(I)
      X2= XI(JJ)
      P1=PT(I)
      P2=PT(JJ)
      ANU1=ANB(I)
      ANU2=ANB(JJ)
      NNT=2
      IF(MEHAM.NE.4) CALL OVLAB
      IF(MEHAM.EQ.4) FOLAR=1.D0
      FOLAR1=FOLAR
      IF(MEHAO.EQ.2) FOKS1=FOKS1*FOLAR1*BET0**2
      NNT=1
      IF(MEHAM.NE.4) CALL OVLAB
      IF(MEHAM.EQ.4) FOLAR=1.
      KMF=1-(-1)**(JF+NNO(JJ))
      NKF=(2*(JF/2)-KMF+2)/2
      SUM=0.D0
      DO 120 JKI=1,NKI
      KI=KMI+2*(JKI-1)
      DI=1.D0
      IF(KI.EQ.0) DI=C1
      DO 120 JKF=1,NKF
      KF=KMF+2*(JKF-1)
      SUM1=0.D0
      DF=1.D0
      IF(KF.EQ.0) DF=C1
      M=2*KF
      IF(KF.NE.KI) GO TO 30
      M1=2*KI
      M2=0
      CALL KLEGO
      T1=AKG
      IF(KF.NE.0) GO TO 40
      M1=-2*KI
      CALL KLEGO
      T1=T1+AKG*(-1)**(JI+NNO(I))
   40 SUM1=SUM1 +T1*(A40+(AC4*FOLAC+AS4*FOLAS)*FOLAR+
     1(AC24*FLC2+AS24*FLS2)*FOLAR1+AGG4+AGO4*FOKS1)
   30 T1=0.
      IF((KI+2).NE.KF) GO TO 50
      M1=2*KI
      M2=4
      CALL KLEGO
      T1=AKG
   50 IF((KI-2).NE.KF) GO TO 60
      M1=2*KI
      M2=-4
      CALL KLEGO
      T1=T1+AKG
   60 IF((2-KI).NE.KF) GO TO 70
      M1=-2*KI
      M2=4
      CALL KLEGO
      T1=T1+AKG*(-1)**(JI+NNO(I))
   70 SUM1=SUM1 +T1*(A42+(BC4*FOLAC+BS4*FOLAS)*FOLAR+
     1BS24*FOLS2*FOLAR1+BGG4+BGO4*FOKS1)
      T1=0.
      IF((KI+4).NE.KF) GO TO 80
      M1=2*KI
      M2=8
      CALL KLEGO
      T1=T1+AKG
   80 IF((KI-4).NE.KF) GO TO 90
      M1=2*KI
      M2=-8
      CALL KLEGO
      T1=T1+AKG
   90 IF((4-KI).NE.KF) GO TO 95
      M1=-2*KI
      M2=8
      CALL KLEGO
      T1=T1+(-1)**JI*AKG
   95 SUM1=SUM1+T1*(A44+(CC4*FOLAC+CS4*FOLAS)*FOLAR+CS24*FLS2*FOLAR1+
     1CGG4+CGO4*FOKS1)
      SUM=SUM+SUM1*AIT(I,JKI)*AIT(JJ,JKF)/DI/DF
  120 CONTINUE
      IF(I.NE.JJ) GO TO 130
      Q4=SUM
      IF(MEPRI.LT.99) 
     * PRINT 117,ES(I),JI,NTU(I),NNB(I),NNG(I),NNO(I),Q4
      WRITE(21,117)ES(I),JI,NTU(I),NNB(I),NNG(I),NNO(I),Q4
  117 FORMAT(10X,'AVERAGE VALUE OF LAMBDA4 MOMENT FOT STATE  E=',
     *D11.4,'JI=',I2,'TAU=',I1,'NB=',I1,'NG=',I1,'NO=',I1,3X,
     *'DEVIDED BY Q40=',D11.4)
      GO TO 19
  130 B4(I,JJ)=SUM*SUM
      IF(MEPRI.LT.99) PRINT 11
      WRITE (21,11)
      IF(MEPRI.LT.99) PRINT 88,ES(I),JI,NTU(I),NNB(I),NNG(I),NNO(I),
     *         ES(JJ),JF,NTU(JJ),NNB(JJ),NNG(JJ),NNO(JJ),B4(I,JJ)
      WRITE(21,88)ES(I),JI,NTU(I),NNB(I),NNG(I),NNO(I),
     *         ES(JJ),JF,NTU(JJ),NNB(JJ),NNG(JJ),NNO(JJ),B4(I,JJ)
      IF(MEPRI.LT.99) PRINT 11
      WRITE(21,11)
      IF(MEPRI.LT.99) PRINT 99,FLC2,FLS2,FOLS2,FOLAR1,FOKS1
      WRITE(21,99)FLC2,FLS2,FOLS2,FOLAR1,FOKS1
   99 FORMAT(5X,'I(CS**2)',D11.4,'I(SS**2)=',D11.4,
     *'I(SS(2G0))',D11.4,' FOLAR1=',D11.4,' FOKS1=',D11.4//)
   88 FORMAT(5X,'B(E4:(',D11.4,')JI=',I2,'TAU=',I1,'NB=',
     *I1,'NG=',I1,'NO=',I1,'--->(',D11.4,')JF=',I2,'TAU=',I1,'NB=',
     *I1,'NG=',I1,'NO=',I1,')=',D11.4)
   19 CONTINUE
      IF(MEHAO.EQ.0) GO TO 100
      IF(MEPRI.LT.99) PRINT 16
      WRITE(21,16)
C     CALCULATIONS OF E3- TRANSITION PROBABILITIES
      J2=6
      DO 42 I=1,NUR
      PO1=(-1)**NNO(I)
      DG1=0.D0
      IF(PO1.NE.1) DG1=GAMDE
      KG1=(3-PO1)/2
      JI=JU(I)
      NG1I=NNG(I)+1
      ANG1=ANG(NG1I,KG1)
      NO1I=NNO(I)+1
      AONU1=ANO(NO1I)
      CD1=CD(NG1I,KG1)
      J1=2*JI
      KMI=1-(-1)**(JI+NNO(I))
      NKI=(2*(JI/2)-KMI+2)/2
      DO 42 JJ=1,NUR
      JF=JU(JJ)
      PO2=(-1)**NNO(JJ)
      DG2=0.D0
      IF(PO2.NE.1) DG2=GAMDE
      KG2=(3-PO2)/2
      NG1F=NNG(JJ)+1
      ANG2=ANG(NG1F,KG2)
      NO1F=NNO(JJ)+1
      AONU2=ANO(NO1F)
      CD2=CD(NG1F,KG2)
      J=2*JF
      JIF=IABS(JI-JF)
      IF(JIF.GT.3) GO TO 42
      IF(PO1.EQ.PO2) GO TO 42
      IF(JI.EQ.0.AND.JF.EQ.0) GO TO 42
      IF(MEHAO.EQ.0) GO TO 47
      IF(MEHAO.LT.2) GO TO 53
      EBM=EXP(-(BET3/AMUO)**2)
      FOKS=BET3/SQRT(1.D0-EBM**2)
      IF(MEHAO.GE.2) GO TO 47
   53 NNT=1
      X1=-BET3/AMUO
      X2=X1
      P1=BET3
      P2=BET3
      ANU1=AONU1
      ANU2=AONU2
      IF(BET3.EQ.0.D0) CALL OVLAO
      IF(BET3.NE.0.D0) CALL OVLAB
      FOKS=FOLAR
   47 FOKS=FOKS/BET3
      IF(MEHAO.EQ.2) FOKS=FOKS/BET0
      IF(MEHAM.GE.5)  CALL TRLAG
      NNT=2
      X1= XI(I)
      X2= XI(JJ)
      P1=PT(I)
      P2=PT(JJ)
      ANU1=ANB(I)
      ANU2=ANB(JJ)
      IF(MEHAM.NE.4) CALL OVLAB
      IF(MEHAM.EQ.4) FOLAR=1.
      FOLAR1=FOLAR
      NNT=1
      IF(MEHAM.NE.4) CALL OVLAB
      IF(MEHAM.EQ.4) FOLAR=1.
      KMF=1-(-1)**(JF+NNO(JJ))
      NKF=(2*(JF/2)-KMF+2)/2
      SUM=0.
      DO 31 JKI=1,NKI
      KI=KMI+2*(JKI-1)
      DI=1.
      IF(KI.EQ.0) DI=C1
      DO 31 JKF=1,NKF
      KF=KMF+2*(JKF-1)
      SUM1=0.D0
      DF=1.D0
      IF(KF.EQ.0) DF=C1
      M=2*KF
      IF(KF.NE.KI) GO TO 32
      M1=2*KI
      M2=0
      CALL KLEGO
      T1=AKG
      IF(KF.NE.0) GO TO 33
      M1=-2*KI
      CALL KLEGO
      T1=T1+AKG*(-1)**(JI+NNO(I))
   33 IF(MEHAO.EQ.2) SUM1=SUM1+T1*((AC3*FOLAC+AS3*FOLAS)
     1*FOLAR1+(AC00+AOG3)*FOLAR)*FOKS*BET0
      IF(MEHAO.EQ.2) GO TO 32
      SUM1=SUM1 +T1*(AC00+(AC3*FOLAC+AS3*FOLAS)*FOLAR+
     1AOG3)*FOKS
   32 T1=0.
      IF((KI+2).NE.KF) GO TO 34
      M1=2*KI
      M2=4
      CALL KLEGO
      T1=AKG
      T1=T1+AKG*(-1)**(JI+NNO(I))
   34 IF((KI-2).NE.KF) GO TO 35
      M1=2*KI
      M2=-4
      CALL KLEGO
      T1=T1+AKG
   35 IF((2-KI).NE.KF) GO TO 36
      M1=-2*KI
      M2=4
      CALL KLEGO
      T1=T1+AKG*(-1)**(JI+NNO(I))
   36 IF(MEHAO.EQ.2) SUM1=SUM1+T1*((BC3*FOLAC+BS3*FOLAS)
     1*FOLAR1+(BS00+AOG3)*FOLAR)*FOKS*BET0
      IF(MEHAO.EQ.2) GO TO 43
      SUM1=SUM1 +T1*(BS00+(BC3*FOLAC+BS3*FOLAS)*FOLAR+
     1BOG3)*FOKS
   43 SUM=SUM+SUM1*AIT(I,JKI)*AIT(JJ,JKF)/DI/DF
   31 CONTINUE
      IF(I.NE.JJ) GO TO 37
      Q3= SUM
      IF(MEPRI.LT.99) PRINT 38,ES(I),JI,NTU(I),NNB(I),NNG(I),NNO(I),Q3
      WRITE(21,38)ES(I),JI,NTU(I),NNB(I),NNG(I),NNO(I),Q3
   38 FORMAT(10X,'AVERAGE VALUE OF LAMBDA3 MOMENT FOT STATE  E=',
     *D11.4,'JI=',I2,'TAU=',I1,'NB=',I1,'NG=',I1,'NO=',I1,3X,
     *'DEVIDED BY Q30=',D11.4)
   37 B3(I,JJ)=SUM*SUM
      IF(MEPRI.LT.99) PRINT 11
      WRITE(21,11)
      IF(MEPRI.LT.99) PRINT 39, ES(I),JI,NTU(I),NNB(I),NNG(I),NNO(I),
     *         ES(JJ),JF,NTU(JJ),NNB(JJ),NNG(JJ),NNO(JJ),B3(I,JJ)
      WRITE(21,39) ES(I),JI,NTU(I),NNB(I),NNG(I),NNO(I),
     *         ES(JJ),JF,NTU(JJ),NNB(JJ),NNG(JJ),NNO(JJ),B3(I,JJ)
   39 FORMAT(5X,'B(E3:(',D11.4,')JI=',I2,'TAU=',I1,'NB=',
     *I1,'NG=',I1,'NO=',I1,'--->(',D11.4,')JF=',I2,'TAU=',I1,'NB=',
     *I1,'NG=',I1,'NO=',I1,')=',D11.4)
      IF(MEPRI.LT.99) PRINT 83,FOKS
      WRITE(21,83) FOKS
   83 FORMAT(5X,'FOKS=',D11.4/)
   42 CONTINUE
  100 STOP
      END
C     *****************************************************************
      SUBROUTINE SHEMTEK
C     *****************************************************************
      IMPLICIT DOUBLE PRECISION(A-H,O-Z) 
      COMMON/SHEMM/ES(40),JU(40),NTU(40),NNB(40),NNG(40),NNO(40),
     *NPI(40)
      COMMON/ENA/EN,EL(40),BET(10),NUR,NMAX,NPD,LAS
      COMMON/SHEM1/HW,AMG0,AMB0,GAM0,BET0,BET4,GAMDE,GSHAPE
     */SHAMO/BET3,ETO,AMUO,HWO,BB32,DPAR
      COMMON/MAT/GAM,IS,NO
      COMMON/AA/ANO(40)
      COMMON/EIT/EPIT1,EPIT2,EPIT12
     */MENU/MEJOB,MEPOT,MEHAM,MECHA,MEPRI,MESOL,MESHA,MESHO,MEHAO
     *,MEAPP,MEVOL,MEREL,MECUL,MERZZ,MERRR,MEDIS,MERIP
     */SHEM2/GIT(40),EP0(40),EP1(40),EP2(40),EP12(40),ANG(40,2),EPB(40),
     *EPG(40),EGB(40),AIT(31,16),PT(40),FOG(2,8,8),XI(40),ANB(40),
     *CD(40,2)
      COMMON/VEC/ TM(16,16),AM(16,16),EIN(31,16),NK,KMI,ITAU
      COMMON/XI12/XIT,XIT1,DET,NROOT/FUDN/Y,DN,ANU,DNV,CDV
      COMMON/OVLG/ANG1,ANG2,FOLAG,FOLAC,FOLAS,CD1,CD2,DG1,DG2,NNTG
     */NP/NPJ(25)
      COMMON/INRM/AMO,BMO,CMO,BB42,GAMG,DELG
     */OVLB/X1,P1,ANU1,X2,P2,ANU2,FOLAR,NNT
     */SGB/FOLGB(10),FOLB(10),FOLKS(10),FOLGA(10)
     */PB/PBET2,PBET3
      AMB4=AMB0**4
      AMG4=AMG0**4
      PBET2=1.D0
      PBET3=1.D0
C     PI3=1.0471975512D0
      PI3=4.d0/3.d0*atan(1.d0)      
      C1=SQRT(2.d0)
      AMO2=AMUO**2/2.
      IF(MEHAO.EQ.0) GO TO 31
      NOM=0
      DO 30 I=1,NUR
      IF(NOM.LT.NNO(I)) NOM=NNO(I)
   30 CONTINUE
      NOM1=NOM+1
      XIT=-BET3/AMUO
      X1=XIT
      X2=XIT
      P1=BET3
      P2=BET3
      DO 32 I=1,NOM1
      NROOT=I-1
      IF(BET3.EQ.0.) ANO(I)=2*NROOT+1.D0
      IF(BET3.EQ.0.) GO TO 2
      CALL ANUDF
      ANO(I)=ANU
    2 IF(MEHAO.LT.1) GO TO 32
      IF(MEHAO.EQ.1) GO TO 45
      FOLB(I)=AMO2*(1.D0+(-1)**NO*EXP(-(BET3/AMUO)**2))+BET3**2
      GO TO 32
   45 ANU1=ANU
      ANU2=ANU
      IF(BET3.EQ.0.) GO TO 40
      NNT=1
      CALL OVLAB
      FOLB(I)=(FOLAR-BET3)/BET3
      GO TO 32
   40 FOLB(I)=(2.D0*ANO(I)+1.D0)*AMO2
   32 CONTINUE
   31 NGD=2
      IF(MEHAM.EQ.5.OR.MEHAM.EQ.7) NGD=0
      NGM=0
      IF(MEHAM.LE.4) GO TO 26
      DO 23 I=1,NUR
      IF(NGM.LT.NNG(I)) NGM=NNG(I)
   23 CONTINUE
      GAMM=GAM0
      NGMP=NGM+NGD+1
      DO 46 INO=1,2
      DO 24 I=1,NGMP
      NROOT=I-1
      XIT=-C1*GAM0/AMG0
      XIT1=C1*(PI3-GAM0)/AMG0
      IF(XIT1.GT.7.D0) GO TO 21
      CALL ANDET0
      ANG(I,INO)=ANU
      CD(I,INO)=CDV
      GO TO 24
   21 XIT=-GAM0/AMG0
      CALL ANUDF
      ANG(I,INO)=ANU
      CD(I,INO)=0.D0
   24 CONTINUE
      GAM0=GAM0+GAMDE
   46 CONTINUE
      GAM0=GAMM
      NGM1=NGM+1
      DO 25 I=1,NGM1
      JP1=I+NGD
      ANG1=ANG(I,1)
      CD1=CD(I,1)
      DO 25 J=I,JP1
      ANG2=ANG(J,1)
      CD2=CD(J,1)
      IF(MEHAM.EQ.5.OR.MEHAM.EQ.7) GO TO 22
      NNTG=1
      CALL OVLAG
      FOG(1,I,J)=FOLAG
      FOG(1,J,I)=FOG(1,I,J)
      NNTG=2
      CALL OVLAG
      FOG(2,I,J)=FOLAG
      FOG(2,J,I)=FOG(2,I,J)
   22 CONTINUE
      IF(I.NE.J) GO TO 25
      IF(MEHAM.NE.7) GO TO 25
      NNTG=11
      CALL OVLAG
      FOLGB(I)=FOLAG
   25 CONTINUE
      GAM=GAM0
      IF(BET3.EQ.0.D0.OR.MEHAO.EQ.2) PBET3=0.
      IF(MESHA.EQ.1) GO TO 37
      BB420=BB42
      DEBET=0.001D0*BB420
      BB42=BB420+DEBET
      CALL INERMO
      AMOP=AMO
      BMOP=BMO
      CMOP=CMO
      BB42=BB420-DEBET
      CALL INERMO
      AMOM=AMO
      BMOM=BMO
      CMOM=CMO
      BB42=BB420
      A1D=(AMOP-AMOM)/DEBET/2.D0
      B1D=(BMOP-BMOM)/DEBET/2.D0
      C1D=(CMOP-CMOM)/DEBET/2.D0
      AB1DM=A1D-B1D
      AB1DP=A1D+B1D
   37 IF(MESHO.EQ.0) GO TO 26
      BB320=BB32
      IF(BET3.EQ.0.OR.MEHAO.EQ.2) PBET3=0.001D0/BB32*GAM**2
      IF(BET3.EQ.0.OR.MEHAO.EQ.2) GO TO 41
      DEBET=0.001D0*BB320
      BB32=BB320+DEBET
   41 CALL INERMO
      AMOP=AMO
      BMOP=BMO
      CMOP=CMO
      IF(BET3.EQ.0.D0.OR.MEHAO.EQ.2) PBET3=0.D0
      IF(BET3.EQ.0.D0.OR.MEHAO.EQ.2) GO TO 42
      BB32=BB320-DEBET
   42 CALL INERMO
      AMOM=AMO
      BMOM=BMO
      CMOM=CMO
      BB32=BB320
      IF(BET3.EQ.0.D0.OR.MEHAO.EQ.2) DEBET=0.0005D0*GAM**2
      AOD=(AMOP-AMOM)/DEBET/2.D0
      BOD=(BMOP-BMOM)/DEBET/2.D0
      COD=(CMOP-CMOM)/DEBET/2.D0
      ABODM=AOD-BOD
      ABODP=AOD+BOD
      IF(BET3.EQ.0.D0.OR.MEHAO.EQ.2) PBET3=0.D0
      GAMM=GAM0
   26 DO 1 I=1,NUR
      IS=JU(I)
      ITAU=NTU(I)
      NG=NNG(I)
      NB=NNB(I)
      NO=NNO(I)
      NROOT=NG
      GAM=GAM0
      IF((-1)**NO.NE.1) GAM=GAM+GAMDE
      CALL MATAM
      EP0(I)=EIN(IS+1,ITAU)
      IF(MEHAM.LE.4) GO TO 27
      CALL EIT12
      GIT(I)=GAM
      EP1(I)=EPIT1
      EP2(I)=EPIT2
      EP12(I)=EPIT12
   27 DO 20 J=1,NK
      AIT(I,J)=TM(J,ITAU)
   20 CONTINUE
      IF(MEHAM.EQ.4) EGB(I)=EP0(I)
      IF(MEHAM.EQ.4) GO TO 1
      IF(MEHAM.EQ.3) GO TO 29
      ANU=ANG(NG+1,(3-(-1)**NO)/2)
      EPG(I)=(ANU+0.5D0)*2.D0/AMG0**2+EPIT1+EPIT2+EPIT12
      EE=EP0(I)+EPG(I)-EPG(1)
   29 IF(MEHAM.EQ.3) EE=EP0(I)
      IF(MEHAO.EQ.0) GO TO 43
      EDO=DPAR*(-1)**(NO+1)
      IF(I.EQ.1) EDO1=EDO
      IF(MEHAO.EQ.2) EE=EE+EDO-EDO1
   43 AA=AMB4*EE
      PIT0=1.D0
      DPI=100.D0
      IF(AA.EQ.0.D0) GO TO 5
    7 PIT=PIT0+DPI
      IF((PIT-1.)*PIT**3-AA) 8,6,9
    8 PIT0=PIT
      GO TO 7
    9 DPI=DPI/5.D0
      IF(DPI.LE.1.D-8) GO TO 6
      GO TO 7
    5 PIT=1.D0
    6 CONTINUE
      PT(I)=PIT
      XIT=-PIT/AMB0*(4.D0-3.D0/PIT)**0.25D0
      XI(I)=XIT
      NROOT=NB
      AP=AMB0/PIT
      AP2=AP*AP
      AP6=AP2*AP2*AP2
      CALL ANUDF
      ANB(I)=ANU
      EPIBO1=0.D0
      P1=PIT
      P2=PIT
      X1=XIT
      X2=XIT
      ANU1=ANU
      ANU2=ANU
      IF(MESHA.EQ.1.AND.MESHO.EQ.0) GO TO 33
      SUM1=0.D0
      SUM2=0.D0
      DO 11 J=1,NK
      AK=TM(J,ITAU)
      AKA=TM(J,ITAU)
      KA=KMI+(J-1)*2
      SUM1=SUM1+AK*AKA*KA*KA
      D=1.D0
      DD=0.D0
      IF(KA.EQ.0) DD=1.d0
      IF(KA.EQ.0) D=SQRT(2.d0)
      IF(J.EQ.NK) GO TO 11
      AK1=TM(J+1,ITAU)
      AKA1=TM(J+1,ITAU)
      SUM2=SUM2+(AK1*AKA+AK*AKA1)*(1.D0+(-1)**(IS+NO)*DD)/D*
     *SQRT((IS+KA+2.D0)*(IS+KA+1.D0)*(IS-KA-1.D0)*(IS-KA))
   11 CONTINUE
      IF(MESHO.EQ.0) GO TO 34
      EPIOO=ABODP/8.D0*IS*(IS+1)+(0.25D0*COD-ABODP/8.D0)*SUM1+
     *ABODM/16.D0*SUM2
      IF(MEHAO.LT.1) GO TO 35
      NNT=13
      CALL OVLAB
      EPIBO1=HW*BB32*AMB0**2*FOLAR*EPIOO*FOLB(NO+1)
   35 IF(BET3.EQ.0.D0) GO TO 33
      NNT=10
      CALL OVLAB
      IF(MEHAO.EQ.2) GO TO 33
      EPIBO1=-HW*BB32*FOLAR*AMB0**2*EPIOO+EPIBO1
   33 IF(MESHA.EQ.1) EPIB1=0.D0
      IF(MESHA.EQ.1) GO TO 28
      IF(MESHO.GT.0) GO TO 36
   34 NNT=10
      CALL OVLAB
   36 EPIB1=AB1DP/8.D0*IS*(IS+1)+(0.25D0*C1D-AB1DP/8.D0)*SUM1+
     *AB1DM/16.D0*SUM2
      EPIB1=EPIB1*FOLAR*AMB0**2*BB42*HW
   28 EPB(I)=EPIB1
      EGB(I)=HW*((ANU+0.5D0)*SQRT(4.D0-3.D0/PIT)
     *+0.5D0*AP2*EE+0.5D0*AP6*EE*EE)-EPIB1+EPIBO1
      IF(MEHAO.EQ.1) EGB(I)=EGB(I)+HWO*(ANO(NO+1)+0.5D0)
      IF(MEHAM.NE.7) GO TO 1
      G3=3.D0*GAM0
      CBG1=1.D0/AMG0**4+81.D0/4.D0*(1.D0/SIN(G3)**2+3.D0*COS(G3)**2/
     *SIN(G3)**4)
      NNT=11
      X1=XIT
      P1=PIT
      X2=XIT
      P2=PIT
      ANU1=ANU
      ANU2=ANU
      CALL OVLAB
      EBG1=HW*AMB0**2/4.D0*BET0**2*CBG1*FOLAR*FOLGB(NG+1)*AMG0**2
      EGB(I)=EGB(I)+EBG1
    1 CONTINUE
      EGB1=EGB(1)
      DO 10 I=1,NUR
   10 EGB(I)=EGB(I)-EGB1
      RETURN
      END
C     *****************************************************************
      SUBROUTINE SHEM
C     *****************************************************************
      IMPLICIT DOUBLE PRECISION(A-H,O-Z) 
      COMMON/SHEMM/ES(40),JU(40),NTU(40),NNB(40),NNG(40),NNO(40),
     *NPI(40)
      COMMON/ENA/EN,EL(40),BET(10),NUR,NMAX,NPD,LAS
      COMMON/SHEM1/HW,AMG0,AMB0,GAM0,BET0,BET4,GAMDE,GSHAPE
     */SHAMO/BET3,ETO,AMUO,HWO,BB32,DPAR
      COMMON/MAT/GAM,IS,NO
      COMMON/AA/ANO(40)
      COMMON/EIT/EPIT1,EPIT2,EPIT12
     */MENU/MEJOB,MEPOT,MEHAM,MECHA,MEPRI,MESOL,MESHA,MESHO,MEHAO
     *,MEAPP,MEVOL,MEREL,MECUL,MERZZ,MERRR,MEDIS,MERIP
     */SHEM2/GIT(40),EP0(40),EP1(40),EP2(40),EP12(40),ANG(40,2),EPB(40),
     *EPG(40),EGB(40),AIT(31,16),PT(40),FOG(2,8,8),XI(40),ANB(40),
     *CD(40,2)
      COMMON/VEC/ TM(16,16),AM(16,16),EIN(31,16),NK,KMI,ITAU
      COMMON/XI12/XIT,XIT1,DET,NROOT/FUDN/Y,DN,ANU,DNV,CDV
      COMMON/OVLG/ANG1,ANG2,FOLAG,FOLAC,FOLAS,CD1,CD2,DG1,DG2,NNTG
     */NP/NPJ(25)
      COMMON/INRM/AMO,BMO,CMO,BB42,GAMG,DELG
     */OVLB/X1,P1,ANU1,X2,P2,ANU2,FOLAR,NNT
     */SGB/FOLGB(10),FOLB(10),FOLKS(10),FOLGA(10)
     */PB/PBET2,PBET3
      PI=4.d0*atan(1.d0)
      SQPI=SQRT(PI)
      AMB4=AMB0**4
      AMG4=AMG0**4
      PBET2=1.D0
      PBET3=1.D0
C     PI3=1.0471975512D0
      PI3=PI/3.d0      
      C1=SQRT(2.d0)
      AMO2=AMUO**2/2.D0
      IF(MEHAO.EQ.0) GO TO 31
      NOM=0
      DO 30 I=1,NUR
      IF(NOM.LT.NNO(I)) NOM=NNO(I)
   30 CONTINUE
      NOM1=NOM+1
      XIT=-BET3/AMUO
      X1=XIT
      X2=XIT
      P1=BET3
      P2=BET3
      DO 32 I=1,NOM1
      NROOT=I-1
      IF(BET3.EQ.0.D0) ANO(I)=2*NROOT+1.
      IF(BET3.EQ.0.D0) GO TO 2
      CALL ANUDF
      ANO(I)=ANU
    2 IF(MEHAO.EQ.1) GO TO 45
      EBM=EXP(-XIT**2)
      FOL=EBM*(-1.D0/XIT/SQPI*(1-(-1)**I)+(-1)**I)-ERFC2(-XIT)
      FOLB(I)=FOL/(1-(-1)**I*EBM)
      GO TO 32
   45 ANU1=ANU
      ANU2=ANU
      IF(BET3.EQ.0.D0) GO TO 40
      NNT=1
      CALL OVLAB
      FOLB(I)=(FOLAR-BET3)/BET3
      GO TO 32
   40 FOLB(I)=(2.D0*ANO(I)+1.D0)*AMO2
   32 CONTINUE
   31 NGD=2
      IF(MEHAM.EQ.5.OR.MEHAM.EQ.7) NGD=0
      NGM=0
      IF(MEHAM.LE.4) GO TO 26
      DO 23 I=1,NUR
      IF(NGM.LT.NNG(I)) NGM=NNG(I)
   23 CONTINUE
      GAMM=GAM0
      NGMP=NGM+NGD+1
      DO 46 INO=1,2
      DO 24 I=1,NGMP
      NROOT=I-1
      XIT=-C1*GAM0/AMG0
      XIT1=C1*(PI3-GAM0)/AMG0
      IF(XIT1.GT.7.D0) GO TO 21
      CALL ANDET0
      ANG(I,INO)=ANU
      CD(I,INO)=CDV
      GO TO 24
   21 XIT=-GAM0/AMG0
      CALL ANUDF
      ANG(I,INO)=ANU
      CD(I,INO)=0.D0
   24 CONTINUE
      GAM0=GAM0+GAMDE
   46 CONTINUE
      GAM0=GAMM
      NGM1=NGM+1
      DO 25 I=1,NGM1
      JP1=I+NGD
      ANG1=ANG(I,1)
      CD1=CD(I,1)
      DO 25 J=I,JP1
      ANG2=ANG(J,1)
      CD2=CD(J,1)
      IF(MEHAM.EQ.5.OR.MEHAM.EQ.7) GO TO 22
      NNTG=1
      CALL OVLAG
      FOG(1,I,J)=FOLAG
      FOG(1,J,I)=FOG(1,I,J)
      NNTG=2
      CALL OVLAG
      FOG(2,I,J)=FOLAG
      FOG(2,J,I)=FOG(2,I,J)
   22 CONTINUE
      IF(I.NE.J) GO TO 25
      IF(MEHAM.NE.7) GO TO 25
      NNTG=11
      CALL OVLAG
      FOLGB(I)=FOLAG
   25 CONTINUE
      GAM=GAM0
      IF(BET3.EQ.0.) PBET3=0.D0
      IF(MESHA.EQ.1) GO TO 37
      BB420=BB42
      DEBET=0.001D0*BB420
      BB42=BB420+DEBET
      CALL INERMO
      AMOP=AMO
      BMOP=BMO
      CMOP=CMO
      BB42=BB420-DEBET
      CALL INERMO
      AMOM=AMO
      BMOM=BMO
      CMOM=CMO
      BB42=BB420
      A1D=(AMOP-AMOM)/DEBET/2.D0
      B1D=(BMOP-BMOM)/DEBET/2.D0
      C1D=(CMOP-CMOM)/DEBET/2.D0
      AB1DM=A1D-B1D
      AB1DP=A1D+B1D
   37 IF(MESHO.EQ.0) GO TO 26
      BB320=BB32
      IF(BET3.EQ.0.D0) PBET3=0.001D0/BB32*GAM**2
      IF(BET3.EQ.0.D0) GO TO 41
      DEBET=0.001*BB320
      BB32=BB320+DEBET
   41 CALL INERMO
      AMOP=AMO
      BMOP=BMO
      CMOP=CMO
      IF(BET3.EQ.0.D0) PBET3=0.D0
      IF(BET3.EQ.0.D0) GO TO 42
      BB32=BB320-DEBET
   42 CALL INERMO
      AMOM=AMO
      BMOM=BMO
      CMOM=CMO
      BB32=BB320
      IF(BET3.EQ.0.D0) DEBET=0.0005D0*GAM**2
      AOD=(AMOP-AMOM)/DEBET/2.D0
      BOD=(BMOP-BMOM)/DEBET/2.D0
      COD=(CMOP-CMOM)/DEBET/2.D0
      ABODM=AOD-BOD
      ABODP=AOD+BOD
      IF(BET3.EQ.0.) PBET3=0.D0
      GAMM=GAM0
   26 DO 1 I=1,NUR
      IS=JU(I)
      ITAU=NTU(I)
      NG=NNG(I)
      NB=NNB(I)
      NO=NNO(I)
      NROOT=NG
      GAM=GAM0
      IF((-1)**NO.NE.1) GAM=GAM+GAMDE
      CALL MATAM
      EP0(I)=EIN(IS+1,ITAU)
      IF(MEHAM.LE.4) GO TO 27
      CALL EIT12
      GIT(I)=GAM
      EP1(I)=EPIT1
      EP2(I)=EPIT2
      EP12(I)=EPIT12
   27 DO 20 J=1,NK
      AIT(I,J)=TM(J,ITAU)
   20 CONTINUE
      IF(MEHAM.EQ.4) EGB(I)=EP0(I)
      IF(MEHAM.EQ.4) GO TO 1
      IF(MEHAM.EQ.3) GO TO 29
      ANU=ANG(NG+1,(3-(-1)**NO)/2)
      EPG(I)=(ANU+0.5D0)*2./AMG0**2+EPIT1+EPIT2+EPIT12
      EE=EP0(I)+EPG(I)-EPG(1)
   29 IF(MEHAM.EQ.3) EE=EP0(I)
      IF(MEHAO.EQ.0) GO TO 43
      EDO=DPAR*(-1)**(NO+1)
      IF(I.EQ.1) EDO1=EDO
      IF(MEHAO.EQ.2) EE=EE+EDO-EDO1
   43 AA=AMB4*EE
      PIT0=1.D0
      DPI=100.D0
      IF(AA.EQ.0.D0) GO TO 5
    7 PIT=PIT0+DPI
      IF((PIT-1.D0)*PIT**3-AA) 8,6,9
    8 PIT0=PIT
      GO TO 7
    9 DPI=DPI/5.
      IF(DPI.LE.1.D-8) GO TO 6
      GO TO 7
    5 PIT=1.
    6 CONTINUE
      PT(I)=PIT
      XIT=-PIT/AMB0*(4.D0-3.D0/PIT)**0.25D0
      XI(I)=XIT
      NROOT=NB
      AP=AMB0/PIT
      AP2=AP*AP
      AP6=AP2*AP2*AP2
      CALL ANUDF
      ANB(I)=ANU
      EPIBO1=0.D0
      P1=PIT
      P2=PIT
      X1=XIT
      X2=XIT
      ANU1=ANU
      ANU2=ANU
      IF(MESHA.EQ.1.AND.MESHO.EQ.0) GO TO 33
      SUM1=0.D0
      SUM2=0.D0
      DO 11 J=1,NK
      AK=TM(J,ITAU)
      AKA=TM(J,ITAU)
      KA=KMI+(J-1)*2
      SUM1=SUM1+AK*AKA*KA*KA
      D=1.D0
      DD=0.D0
      IF(KA.EQ.0) DD=1.D0
      IF(KA.EQ.0) D=SQRT(2.d0)
      IF(J.EQ.NK) GO TO 11
      AK1=TM(J+1,ITAU)
      AKA1=TM(J+1,ITAU)
      SUM2=SUM2+(AK1*AKA+AK*AKA1)*(1.D0+(-1)**(IS+NO)*DD)/D*
     *SQRT((IS+KA+2.D0)*(IS+KA+1.D0)*(IS-KA-1.D0)*(IS-KA))
   11 CONTINUE
      IF(MESHO.EQ.0) GO TO 34
      EPIOO=ABODP/8.D0*IS*(IS+1)+(0.25D0*COD-ABODP/8.D0)*SUM1+
     *ABODM/16.D0*SUM2
      IF(MEHAO.LT.1) GO TO 35
      NNT=13
      CALL OVLAB
      EPIBO1=HW*BB32*AMB0**2*FOLAR*EPIOO*FOLB(NO+1)
   35 NNT=10
      CALL OVLAB
      IF(BET3.EQ.0.D0) GO TO 33
      EPIBO1=-HW*BB32*FOLAR*AMB0**2*EPIOO+EPIBO1
   33 IF(MESHA.EQ.1) EPIB1=0.D0
      IF(MESHA.EQ.1) GO TO 28
      IF(MESHO.GT.0) GO TO 36
   34 NNT=10
      CALL OVLAB
   36 EPIB1=AB1DP/8.D0*IS*(IS+1)+(0.25D0*C1D-AB1DP/8.D0)*SUM1+
     *AB1DM/16.D0*SUM2
      EPIB1=EPIB1*FOLAR*AMB0**2*BB42*HW
   28 EPB(I)=EPIB1
      EGB(I)=HW*((ANU+0.5D0)*SQRT(4.-3./PIT)+0.5*AP2*EE+0.5*AP6*EE*EE)
     *-EPIB1+EPIBO1
      IF(MEHAO.EQ.1) EGB(I)=EGB(I)+HWO*(ANO(NO+1)+0.5D0)
      IF(MEHAO.EQ.3) EGB(I)=EGB(I)+EDO
      IF(MEHAM.NE.7) GO TO 1
      G3=3.D0*GAM0
      CBG1=1.D0/AMG0**4+81.D0/4.D0*(1.D0/SIN(G3)**2+3.D0*COS(G3)**2/
     *SIN(G3)**4)
      NNT=11
      X1=XIT
      P1=PIT
      X2=XIT
      P2=PIT
      ANU1=ANU
      ANU2=ANU
      CALL OVLAB
      EBG1=HW*AMB0**2/4.0*BET0**2*CBG1*FOLAR*FOLGB(NG+1)*AMG0**2
      EGB(I)=EGB(I)+EBG1
    1 CONTINUE
      EGB1=EGB(1)
      DO 10 I=1,NUR
   10 EGB(I)=EGB(I)-EGB1
      RETURN
      END                        
C **********************************************************************
      DOUBLE PRECISION FUNCTION ERFC2(XX)
C **********************************************************************
C     Abramowitz & Stegun, p.299, Eq. 7.1.26 (Error < 1.5E-7)
C
      DOUBLE PRECISION P,A1,A2,A3,A4,A5,P1,R1,R2,R3,R4,R5,XX
      DATA P,A1,A2,A3,A4,A5/0.3275911d0,0.254829592d0,-0.284496736d0,
     *1.421413741d0,-1.453152027d0,1.061405429d0/
      P1=EXP(-XX*XX)
      R1=1.d0/(1.+P*XX)
      R2=R1*R1
      R3=R2*R1
      R4=R3*R1
      R5=R4*R1
      ERFC2=(A5*R5+A4*R4+A3*R3+A2*R2+A1*R1)*P1
      RETURN
      END
C     *******************************************************
C     END of shemsofd
C     *******************************************************
