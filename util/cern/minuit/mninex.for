*
* $Id: mninex.F,v 1.1.1.1 1996/03/07 14:31:30 mclareni Exp $
*
* $Log: mninex.F,v $
* Revision 1.1.1.1  1996/03/07 14:31:30  mclareni
* Minuit
*
*
      SUBROUTINE MNINEX(PINT)
      IMPLICIT DOUBLE PRECISION (A-H,O-Z)
CC        Transforms from internal coordinates (PINT) to external
CC        parameters (U).   The minimizing routines which work in
CC        internal coordinates call this routine before calling FCN.
      include 'd506.inc'
      DIMENSION PINT(*)
      DO 100 J= 1, NPAR
      I = NEXOFI(J)
      IF (NVARL(I) .EQ. 1) THEN
         U(I) = PINT(J)
      ELSE
         U(I) = ALIM(I) + 0.5*(SIN(PINT(J)) +1.0) * (BLIM(I)-ALIM(I))
      ENDIF
  100 CONTINUE
      RETURN
      END
