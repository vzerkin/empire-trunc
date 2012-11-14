Ccc     $Author: apalumbo $
Ccc     $Date: 2012-05-04

       SUBROUTINE SFACTORp(S_factorp)
       IMPLICIT REAL*8 (A-H,O-Z)

CC  This program calculates the S-factor for a non-resonant reaction.
CC  The energies are converted to keV and the cross section to barns and the 
CC  S-factor is output in keV barns (MeV*b).  Furthermore, the incident energy (lab
CC  frame from EMPIRE) is converted to a center-of-mass energy and the
CC  subsequent S-factor calculated accordingly - treatment follows Cauldrons
CC  in the Cosmos - Rolfs and Rodney (University of Chicago Press - 1988) 
CC  

       INCLUDE 'dimension.h'
       INCLUDE 'global.h'
       REAL alpha, mu

        open(unit = 781, file = "sfoutp.txt")
CC        WRITE(781,*) 'Ecm, Cross Section (b), S_factor (MeV b)'
CC        WRITE(781,*) 
         
         alpha = A(0)/(A(0)+AEJc(0))
         EIN = (EIN*1000)*alpha
         mu = (A(0)*AEJc(0))/(A(0)+AEJc(0))
         eta = 31.29*Z(0)*ZEJc(0)*((mu/EIN)**0.5)
         S_factor = (((CSPrd(3)/1000)*EIN)/exp(-eta))
         WRITE(781,50) EIN/1000, CSPrd(3)/1000, S_factor
   50    FORMAT(E10.4,4X,E12.6,4X,E12.6)
C       RETURN
       END
