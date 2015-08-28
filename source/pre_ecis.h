Ccc   * $Rev: 4456 $
Ccc   * $Author: rcapote $ 
Ccc   * $Date: 2015-08-28 16:58:23 +0200 (Fr, 28 Aug 2015) $
            
c      ---------------------------------------------------------
c      | File with common blocks and declarations for PRE_ECIS  |
c      ---------------------------------------------------------
c
c *** Common blocks for ECIS95 input subroutine ***
c
      common /ecischar/ becis1,becis2,ecis1,ecis2
      character*50 becis1,becis2,ecis1,ecis2      
      common /ecisreal/ astep
      real*8 astep
      common /optcalc/flgrel,flgdom  
      character*1 flgdom,flgrel
      common /znome/nuc,parname,outfile
      character*2 nuc(103)
      character*8 parname(9)
      character*8 outfile  
