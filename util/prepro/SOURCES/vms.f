      SUBROUTINE TIMEIT(SECONDS)
C=======================================================================
C
C     VMS - TIME SINCE START OF PROBLEM (BASE TIME) IN SECONDS
C
c=======================================================================
      IMPLICIT REAL*4 (A-H,O-Z) ! note REAL*4 not 8
      SAVE
      SECONDS=SECNDS(0.0)
      RETURN
      END
