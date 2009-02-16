C=======================================================================
C
C     RECENT COMMON
C
C=======================================================================
      PARAMETER (MAXRHO = 5000)   ! Max. point in tabulated rho
C-----12/20/06 - INCREASED FROM 120,000 TO 360,000
C-----01/04/07 - DECREASED FROM 360,000 TO 120,000
      PARAMETER (MAXRES = 120000) ! Max. # of resonances
C-----01/04/07 - ADDED MAXPT FOR CROSS SECTION POINT VALUES.
C-----01/04/07 - INCREASED FROM 360,000 TO 600,000
      PARAMETER (MAXPT  = 600000) ! Max. # energy points per page
      PARAMETER (MAXSEC = 1000)   ! Max. # of sections
C-----12/20/06 - INCREASED MAXSAVE FROM 200 TO 2,000
      PARAMETER (MAXSAVE = 2000)  ! Max. # of saved iteration points
C-----01/04/07 - MAX. # OF INTERPOLATION RANGES.
      PARAMETER (MAXINT = 1200)
C-----TABULATED ENERGY DEPENDENT RHO
      COMMON/TABRHO/ERHOTB(MAXRHO),RHOTAB(MAXRHO),APTAB(MAXRHO),
     1 NRHO(MAXSEC),NBTRHO(MAXSEC),INTRHO(MAXSEC),INXRHO(4,MAXSEC),
     2 NUMRHO
C-----RESONANCE PARAMETER SECTION PARAMETERS
      COMMON/SECTON/BETA(MAXSEC),RHOX2(MAXSEC),RHOC2(MAXSEC),
     1 RHOP1(MAXSEC),EL(MAXSEC),EH(MAXSEC),GJTAB(MAXSEC),QVALUE(MAXSEC),
     2 ZETA(MAXSEC),EXCITE(4,MAXSEC),
     3 NLOW(MAXSEC),NHIGH(MAXSEC),LVALUE(MAXSEC),LVALUEC(MAXSEC),
     3 LRXTAB(MAXSEC),NAPTAB(MAXSEC),MODE(MAXSEC),ISECT,NSECT
C-----CROSS SECTION ARRAYS
      COMMON/SAVECOM/ESAVE(MAXSAVE),SIGSAVE(4,MAXSAVE),NSAVE
C-----INTERPOLATION RANGES
      COMMON/TERPCOM/INTF(MAXINT),NBTF(MAXINT)
C
C     RESONANCE PARAMETER ARRAYS
C
      COMMON ENRES(MAXRES),SHIFT2(MAXRES),ENODE(MAXRES),WIDNOD(MAXRES),
     1 RESTAB(6,MAXRES),RESJTAB(MAXRES)
C
C     CROSS SECTION ARRAYS
C     ETAB2   = FILE 2 RESONANCE  ENERGY
C     SIG2    = FILE 2 RESONANCE  CROSS SECTION
C     ETAB3   = FILE 3 BACKGROUND ENERGY
C     SIG3    = FILE 3 BACKGROUND CROSS SECTION
C     ETAB23  = COMBINED FILE 2 + 3 ENERGY
C     SIG23   = COMBINED FILE 2 + 3 CROSS SECTION
C     SIGMID  = CROSS SECTION AT MIDPOINT OF ITERATION INTERVAL
C
      COMMON ETAB2 (MAXPT+1),SIG2 (4,MAXPT+1),
     1       ETAB3 (MAXPT),  SIG3 (MAXPT),
     1       ETAB23(MAXPT),  SIG23(MAXPT),SIGMID(4)
C
C     NOTE THAT ETAB2 AND SIG2 ARE DIMENSIONED MAXPT+1, WHILE ETAB2X
C     AND SIG2X ARE DIMENSIONED MAXPT - TO ALLOW PAGES TO BE WRITTEN
C     OR READ WHILE STILL KEEPING 1 ENERGY POINT IN MEMORY.
 
      DIMENSION ETAB2X(MAXPT),SIG2X(4,MAXPT)
      EQUIVALENCE (ETAB2(1) ,ETAB2X(1)),(SIG2(1,1),SIG2X(1,1))
