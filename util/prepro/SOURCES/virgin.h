C=======================================================================
C
C     VIRGIN COMMON
C
C=======================================================================
C
C     PARAMETERS
C
C-----------------------------------------------------------------------
c-----2017/3/7 - INCREASED PAGE SIZE FROM 600,000 TO 3,000,000
      PARAMETER (MAXPOINT = 3000000)
c-----2017/3/7 - Increased max. groups fom 6,000 to 30,000
      PARAMETER (MAXGROUP =   30000)
C-----------------------------------------------------------------------
C
C     STORAGE
C
C-----------------------------------------------------------------------
      COMMON XTAB(MAXPOINT,3),YTAB(MAXPOINT,3),ETRAN(MAXPOINT),
     1 STRAN(MAXPOINT),FLUX(MAXGROUP),REACT(MAXGROUP),
     1 WTTP(MAXGROUP),WTTI(MAXGROUP),WTRP(MAXGROUP),WRTP(MAXGROUP),
     2 WRTI(MAXGROUP),WRRP(MAXGROUP),DENTAB(MAXGROUP),THICKI(MAXGROUP),
     3 FLUXINT(MAXGROUP),REACTINT(MAXGROUP),AVXCINT(MAXGROUP),
     4 EGROUP(MAXGROUP+1)
      DIMENSION XTAB1(MAXPOINT),XTAB2(MAXPOINT),XTAB3(MAXPOINT),
     1          YTAB1(MAXPOINT),YTAB2(MAXPOINT),YTAB3(MAXPOINT)
      EQUIVALENCE (XTAB(1,1),XTAB1(1)),(XTAB(1,2),XTAB2(1)),
     1            (XTAB(1,3),XTAB3(1)),(YTAB(1,1),YTAB1(1)),
     2            (YTAB(1,2),YTAB2(1)),(YTAB(1,3),YTAB3(1))
