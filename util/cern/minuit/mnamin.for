*
* $Id: mnamin.for 4468 2015-08-29 15:06:00Z rcapote $
*
* $Log: mnamin.F,v $
* Revision 1.1.1.1  1996/03/07 14:31:28  mclareni
* Minuit
*
*
      SUBROUTINE MNAMIN(FCN,FUTIL)
      IMPLICIT DOUBLE PRECISION (A-H,O-Z)
CC        Called  from many places.  Initializes the value of AMIN by
CC        calling the user function. Prints out the function value and
CC        parameter values if Print Flag value is high enough.
CC
      include 'd506.inc'
      EXTERNAL FCN,FUTIL
      NPARX = NPAR
      IF (ISW(5) .GE. 1) WRITE (ISYSWR,'(/A,A)') ' FIRST CALL TO ',
     + 'USER FUNCTION AT NEW START POINT, WITH IFLAG=4.'
      CALL MNEXIN(X)
      CALL FCN(NPARX,GIN,FNEW,U,4,FUTIL)
      NFCN = NFCN + 1
      AMIN = FNEW
      EDM = BIGEDM
      RETURN
      END
