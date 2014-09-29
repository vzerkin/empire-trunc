*
* $Id: mnrset.F,v 1.1.1.1 1996/03/07 14:31:31 mclareni Exp $
*
* $Log: mnrset.F,v $
* Revision 1.1.1.1  1996/03/07 14:31:31  mclareni
* Minuit
*
*
      SUBROUTINE MNRSET(IOPT)

      IMPLICIT DOUBLE PRECISION (A-H,O-Z)
      include 'd506.inc'

CC        Called from MNCLER and whenever problem changes, for example
CC        after SET LIMITS, SET PARAM, CALL FCN 6
CC    If IOPT=1,
CC        Resets function value and errors to UNDEFINED
CC    If IOPT=0, sets only MINOS errors to undefined
      CSTATU = 'RESET     '
      IF (IOPT .GE. 1)  THEN
        AMIN = UNDEFI
        FVAL3 = 2.0*ABS(AMIN) + 1.
        EDM = BIGEDM
        ISW(4) = 0
        ISW(2) = 0
        DCOVAR = 1.D0
        ISW(1) = 0
      ENDIF
      LNOLIM = .TRUE.
      DO 10 I= 1, NPAR
      IEXT = max(NEXOFI(I),1)                     ! fix SDH, June 2012 - add max(nex,1) 
      IF (NVARL(IEXT) .GE. 4) LNOLIM=.FALSE.
      ERP(I) = ZERO
      ERN(I) = ZERO
      GLOBCC(I) = ZERO
   10 CONTINUE
      IF (ISW(2) .GE. 1)  THEN
         ISW(2) = 1
         DCOVAR = MAX(DCOVAR,HALF)
      ENDIF
      RETURN
      END
