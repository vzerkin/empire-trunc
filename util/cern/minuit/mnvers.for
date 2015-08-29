*
* $Id: mnvers.for 4468 2015-08-29 15:06:00Z rcapote $
*
* $Log: mnvers.F,v $
* Revision 1.1.1.1  1996/03/07 14:31:32  mclareni
* Minuit
*
*
      SUBROUTINE MNVERS(CV)
      IMPLICIT DOUBLE PRECISION (A-H,O-Z)
      include 'd506.inc'
CC         Returns the Minuit version in CV, char*6
CC
      CHARACTER*(*) CV
      CV = CVRSN
      RETURN
      END
