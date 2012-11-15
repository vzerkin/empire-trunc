Ccc   * $Rev: 3167 $
Ccc   * $Author: apalumbo $
Ccc   * $Date: 2012-10-25 03:42:51 +0200 (Thu, 25 Oct 2012) $

      REAL*8 FUNCTION SFACTOR(Csection)

CC  This program calculates the S-factor for a non-resonant reaction (x,n).
CC  Input: Cross section Csection is in mb
CC  The CMS energy EIN is taken from EMPIRE (in MeV)
CC  S-factor is output in MeV barns (MeV*b). 
CC  Treatment follows A.G.W.Cameron, Technical Report CRL-41, AECL-454 (Chalk River, Ontario, June 1957), p.30,31
CC  Available online at http://www.fas.org/sgp/eprint/CRL-41.pdf 
CC 
CC  Treatment follows Cauldrons in the Cosmos - Rolfs and Rodney (University of Chicago Press - 1988) 
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
      eta = CETa*Z(0)*ZEJc(0)*SQRT(mu/EIN)
C     
      SFACTOR = EIN*stmp*exp(2*pi*eta)
C
C     etmp in keV
C     etmp = EIN*1000
C     eta = 31.29*Z(0)*ZEJc(0)*SQRT(mu/etmp)
C     SFACTOR = EIN*stmp*exp(eta)

      WRITE(781,50) EIN, stmp, SFACTOR
   50 FORMAT(E10.4,4X,E12.6,4X,E12.6)
      RETURN
      END
