*
* $Id: mndxdi.F,v 1.1.1.1 1996/03/07 14:31:29 mclareni Exp $
*
* $Log: mndxdi.F,v $
* Revision 1.1.1.1  1996/03/07 14:31:29  mclareni
* Minuit
*
*
      SUBROUTINE MNDXDI(PINT,IPAR,DXDI)
      IMPLICIT DOUBLE PRECISION (A-H,O-Z)
CC        calculates the transformation factor between external and
CC        internal parameter values.     this factor is one for
CC        parameters which are not limited.     called from MNEMAT.
      include 'd506.inc'
      I = NEXOFI(IPAR)
      DXDI = 1.0
      IF (NVARL(I) .GT. 1)
     +      DXDI = 0.5 *ABS((BLIM(I)-ALIM(I)) * COS(PINT))
      RETURN
      END
