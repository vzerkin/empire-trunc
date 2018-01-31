C=======================================================================
C
C     LINEAR COMMON
C
C=======================================================================
C
C     PARAMETERS
C
C-----------------------------------------------------------------------
C-----2017/3/7 - INCREASED PAGE SIZE to 3,000,000 from 600,000
      PARAMETER (MAXPAGE =  3000000)
C-----------------------------------------------------------------------
C
C     STORAGE
C
C-----------------------------------------------------------------------
C-----INPUT IS 1 PAGE, OUTPUT IS 2 PAGES
      COMMON XIN(MAXPAGE),YIN(MAXPAGE),XOUT(2*MAXPAGE),YOUT(2*MAXPAGE)
      DIMENSION XPAGE1(MAXPAGE),YPAGE1(MAXPAGE),XPAGE2(MAXPAGE),
     1 YPAGE2(MAXPAGE)
      EQUIVALENCE (XOUT(1),XPAGE1(1)),(YOUT(1),YPAGE1(1)),
     1 (XOUT(MAXPAGE+1),XPAGE2(1)),(YOUT(MAXPAGE+1),YPAGE2(1))
