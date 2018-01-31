      SUBROUTINE TIMEIT(SECONDS)
C=======================================================================
C
C     LINUX - TIME SINCE START OF PROBLEM (BASE TIME) IN SECONDS
C
C=======================================================================
      IMPLICIT REAL*4 (A-H,O-Z) ! note REAL*4 not 8
      SAVE
      DIMENSION TARRAY(2)
      SECONDS=ETIME(TARRAY)
      SECONDS=(TARRAY(1)+TARRAY(2))
      RETURN
      END
