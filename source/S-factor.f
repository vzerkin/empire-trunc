ccc   $Rev: 3212 $
Cc    $Author: rcapote $
Ccc   $Date: 2012-11-15 11:52:39 +0100 (Thu, 15 Nov 2012) $
C
Cc    $Original: 3210 $
CC    $Author: apalumbo $ 
CCc   $Date: 2012-11-14 $
 
      REAL*8 FUNCTION SFACTOR(Csection)

CC  This program calculates the S-factor for a non-resonant reaction (see manual for further details).
CC  Input: Cross section Csection is in mb (but calculation must be done in b)
CC  The CMS energy EIN is taken from EMPIRE (in MeV)
CC  S-factor is output in MeV barns (MeV*b). 
CC
CC  Treatment follows Cauldrons in the Cosmos - Rolfs and Rodney (University of Chicago Press - 1988) 
CC  Outlined in Empire.pdf
CC
CC  Another treatment of the S-factor formulation follows from Nuclear Physics of Stars - Christian Iliadis - (Wiley-Vch - 2007) 
CC
CC  The calculation of eta is consistent in both Rolfs/Rodney and Iliadis.  
CC  Since the Incident energy is in MeV, the formulation of eta by C. Iliadis is preferred.
CC
CC  RC: Iliadis constant changed to use EMPIRE constants traceable to NIST (CODATA 2010 set)
CC	Iliadis 0.989534 -> CETa * 2 * pi = 0.9895106848
CC

      INCLUDE 'dimension.h'
      INCLUDE 'global.h'

      REAL*8 Csection

      REAL*8 mu, stmp, eta

C     stmp in barn
      stmp = Csection/1000
         
      mu = (A(0)*AEJc(0))/(A(0)+AEJc(0))
C
C     CETa = ELE2/HHBarc*SQRT(AMUmev/2) 
C     CETa = e^2 / (hbar C) SQRT( MASS*C^2 /2) = e^2 / hbar SQRT( AMUmev /2)
C     The Sommerfeld factor eta can be calculated as CETa*SQRT(mu/EIN)*Zt*Ze
C     eta = e^2*Zt*Ze / hbar SQRT( AMUmev*mu/ (2EIN)) where mu is adimensional, and EIN is the CMS energy in MeV
C     eta = CETa*Z(0)*ZEJc(0)*SQRT(mu/EIN)
	  eta = 2*pi*CETa*Z(0)*ZEJc(0)*SQRT(mu/EIN) 
C
C     Using the latest value of CODATA 2010 constants I obtained:
C     CETa * 2 * pi = 0.9895106848
C     eta = 0.989534*Z(0)*ZEJc(0)*SQRT(mu/EIN)

      SFACTOR = EIN*stmp*exp(eta)

      WRITE(781,50) EIN, stmp, SFACTOR
   50 FORMAT(E10.4,4X,E12.6,4X,E12.6)
      RETURN
      END
