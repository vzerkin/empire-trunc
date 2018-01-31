C=======================================================================
C
C     LEGEND COMMON
C
C=======================================================================
C
C     PARAMETERS
C
C-----------------------------------------------------------------------
C-----TABULATED ANGULAR DISTRIBUTIONS
c-----2017/3/7 - Increased to 3,000,000 from 240,000.
      PARAMETER (MAXPOINT = 3000000)
C-----REQUESTS
      PARAMETER (MAXMAT = 100)
C-----LEGENDRE COEFFICIENT STORAGE
c-----2017/6/17- Changed from 50 to 500
c-----WARNING  - The recursion relationship for Legendre polynomials
c-----           becomes numerical unstable in higher order polynomials.
c-----           I have mechanically changed this from 50 to 500 - but
c-----           50 is about the limit for numerical stability, even
c-----           using DOUBLE PRECISION.
      PARAMETER (MAXLEGS = 500)
C-----------------------------------------------------------------------
C
C     STORAGE
C
C-----------------------------------------------------------------------
C-----TABULATED ANGULAR DISTRIBUTIONS
      COMMON XMUIN (MAXPOINT),PTIN (MAXPOINT),
     1       XMUOUT(MAXPOINT),PTOUT(MAXPOINT)
C-----REQUESTS
      COMMON/TABLET/ETAB1(MAXMAT),ETAB2(MAXMAT),SIGTAB(MAXMAT),
     1 DFLTAB(MAXMAT),
     2 MATAB1(MAXMAT),MFTAB1(MAXMAT),MTAB1(MAXMAT),
     3 MATAB2(MAXMAT),MFTAB2(MAXMAT),MTAB2(MAXMAT),ITAB
C-----LEGENDRE COEFFICIENT STORAGE
      COMMON/LEGCONF/FLMULT(MAXLEGS),PCON1(MAXLEGS),PCON2(MAXLEGS),
     1 VALUES(MAXLEGS),DFL(MAXLEGS),SUMN(MAXLEGS),PN(MAXLEGS),
     2 FL(MAXLEGS),FLOLD(MAXLEGS),FLKEEP(MAXLEGS,3),XMUBASE(5*MAXLEGS),
     3 EKEEP(3),ELAST4,SUM,DFLCHN,DFLMAX
      COMMON/LEGCONI/LVALUE(MAXLEGS),NEWFL(MAXLEGS),LKEEP(3),LEGIN,
     1 LEGUSE,LOOPL,LOOPPC,KPASS
