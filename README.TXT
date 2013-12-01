                 E M P I R E  3.2 (Malta)
                       November 2013

EMPIRE is a modular system of nuclear reaction codes for advanced modeling
of nuclear reactions using various theoretical models. It consists of a
number of FORTRAN codes, input parameter libraries, and experimental data
library (EXFOR/CSISRS) -  operated through the Graphic User Interface (GUI).

EMPIRE 3.2 has been tested successfully on Linux (Ubuntu, Redhat) and Mac OS X
(10.5 and higher).  It can also be run on Windows with either the MinGW and
Cygwin environments.  EMPIRE is provided as source code that must be compiled to 
function.  See the file INSTALL.TXT for installation instructions.

EMPIRE is intended to be a general, flexible, and easy to use tool 
for basic research and evaluation of nuclear data. It offers a  possibility
of combining several theoretical approaches, choosing among alternative 
input parameters and calculating extended set of observables in a single run.
Nuclear data evaluation is facilitated by the ENDF-6 formatting, resonance
module, ENDF-6 file verification and graphical comparison with experimental data.

Major new features in 3.2
-------------------------
*  PFNS implemented, Los Alamos and Kornilov (1st chance fission) 
*  Angular distributions for compound elastic and inelastics
*  Plotting of PFNS, mu-bars, and nu-bars
*  Covariances for PFNS, mu-bars, and nu-bars
*  Simulation of the Engelbrecht-Weidenmuller transformation. Change in inelastic compensated by changing the compound elastic
*  Including gamma and fission transmission coefficients calculated by EMPIRE into ECIS compound calculation
*  Kalbach parameterizations for breakup and transfer reactions of complex projectiles
*  Nobre's deformation systematics
*  Astrophysical S-factor (A. Palumbo)
*  All physics constants updated to CODATA 2010 set
*  Fixing fluctuations due to the gap between the last level and the continuum
*  Improving x-sec and energy balance
*  Minor problems in DWBA calculation with closed channels corrected
*  New zvview2-1.020
*  Upgrade to plot cross sections at fixed angles for neutrons
*  DDHMS multiple emission spectra and DDX's in CM
*  Inclusive DDX's implemented for ENDF=0
*  First ports to Fortran90 (kalend.f90, kalman.f90, genkal.f90, newinp.f90)
*  Improved gfortran compatibility
*  Improved makefiles
*  Improved support for assimilation procedure
*  Improved qsubEmpire.py for running on cluster
*  Add tab in Xrun.tcl that applies Kalman results back to Empire input file
*  New IO subroutines for ENDF-6
*  Making line numbers in ENDF files optional


Installation
------------
See the INSTALL.TXT file included in the EMPIRE distribution.

To run EMPIRE
-------------
To run empire you may create a working subdirectory of your empire root 
[e.g. /home/empire-3.2-malta/Fe56new], change to it and launch the EMPIRE GUI (Xrun.tcl):

    $ mkdir Fe56new
    $ cd Fe56new
    $ empire3 & 


Good calculations !
The EMPIRE team
_______________________________________________________________________
Report problems to: r.capotenoy@iaea.org (IAEA); mwherman@bnl.gov (BNL)