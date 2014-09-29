*
* $Id: mncler.F,v 1.1.1.1 1996/03/07 14:31:28 mclareni Exp $
*
* $Log: mncler.F,v $
* Revision 1.1.1.1  1996/03/07 14:31:28  mclareni
* Minuit
*
*
      SUBROUTINE MNCLER
      IMPLICIT DOUBLE PRECISION (A-H,O-Z)
CC        Called from MINUIT and by option from MNEXCM
CC        Resets the parameter list to UNDEFINED
      include 'd506.inc'
      NPFIX = 0
      NU = 0
      NPAR = 0
      NFCN = 0
      NWRMES(1) = 0
      NWRMES(2) = 0
      DO 10 I= 1, MAXEXT
      U(I) = 0.0
      CPNAM(I) = CUNDEF
      NVARL(I) = -1
   10 NIOFEX(I) = 0
      CALL MNRSET(1)
      CFROM = 'CLEAR   '
      NFCNFR = NFCN
      CSTATU ='UNDEFINED '
      LNOLIM = .TRUE.
      LPHEAD = .TRUE.
      RETURN
      END
