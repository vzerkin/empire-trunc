*
* $Id: mnexin.for 4468 2015-08-29 15:06:00Z rcapote $
*
* $Log: mnexin.F,v $
* Revision 1.1.1.1  1996/03/07 14:31:29  mclareni
* Minuit
*
*
      SUBROUTINE MNEXIN(PINT)
      IMPLICIT DOUBLE PRECISION (A-H,O-Z)
CC        Transforms the external parameter values U to internal
CC        values in the dense array PINT. Subroutine MNPINT is used.
CC
      include 'd506.inc'
      DIMENSION PINT(*)
      LIMSET = .FALSE.
      DO 100  IINT= 1, NPAR
      IEXT = NEXOFI(IINT)
      CALL MNPINT(U(IEXT),IEXT,PINTI)
      PINT(IINT) = PINTI
  100 CONTINUE
      RETURN
      END
