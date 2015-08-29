*
* $Id: mnseti.for 4468 2015-08-29 15:06:00Z rcapote $
*
* $Log: mnseti.F,v $
* Revision 1.1.1.1  1996/03/07 14:31:31  mclareni
* Minuit
*
*
      SUBROUTINE MNSETI(TIT)
      IMPLICIT DOUBLE PRECISION (A-H,O-Z)
      include 'd506.inc'
CC       Called by user to set or change title of current task.
CC
      CHARACTER*(*) TIT
      CTITL = TIT
      RETURN
      END
