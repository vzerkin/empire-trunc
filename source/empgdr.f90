!cc   * $Rev: 5365 $
!cc   * $Author: mwherman $
!cc   * $Date: 2022-06-06 03:43:09 +0200 (Mo, 06 Jun 2022) $
!
module empgdr
   implicit none

   integer , parameter :: MAXGDR = 5986, MEXPPAR = 270, MEXP = 9000, MDEF = 700

   real*8, allocatable, dimension(:) :: DHCs1, DHCs2, DHE1, DHE2, DHGw1, DHGw2, ETAt, HALpha2, HBEtagfl  
   real*8, allocatable, dimension(:) :: HCS1, HCS2, HE1, HE1t, HE2, HE2t, HENergygfl, HGW1, HGW1t, HGW2, HGW2t
   integer, allocatable, dimension(:) :: NANa, NANz, NARam, NNA, NNAt, NNG, NNGt, NNZ, NNZt, NZRam
   

   
   contains
   
   subroutine EMPAgdr()
   
      implicit none
      integer myalloc, my
      myalloc = 0
        
   
      if(allocated(HCS1))deallocate(HCS1)
      allocate(HCS1(MEXPPAR) , STAT = my)
      myalloc = myalloc + my
      HCS1 = 0.0D0
 
      if(allocated(DHCs1))deallocate(DHCs1)
      allocate(DHCs1(MEXPPAR) , STAT = my)
      myalloc = myalloc + my
      DHCs1 = 0.0D0
 
      if(allocated(HCS2))deallocate(HCS2)
      allocate(HCS2(MEXPPAR) , STAT = my)
      myalloc = myalloc + my
      HCS2 = 0.0D0
 
      if(allocated(DHCs2))deallocate(DHCs2)
      allocate(DHCs2(MEXPPAR) , STAT = my)
      myalloc = myalloc + my
      DHCs2 = 0.0D0
 
      if(allocated(HE1))deallocate(HE1)
      allocate(HE1(MEXPPAR) , STAT = my)
      myalloc = myalloc + my
      HE1 = 0.0D0
 
      if(allocated(DHE1))deallocate(DHE1)
      allocate(DHE1(MEXPPAR) , STAT = my)
      myalloc = myalloc + my
      DHE1 = 0.0D0
 
      if(allocated(HE2))deallocate(HE2)
      allocate(HE2(MEXPPAR) , STAT = my)
      myalloc = myalloc + my
      HE2 = 0.0D0
 
      if(allocated(DHE2))deallocate(DHE2)
      allocate(DHE2(MEXPPAR) , STAT = my)
      myalloc = myalloc + my
      DHE2 = 0.0D0
 
      if(allocated(HGW1))deallocate(HGW1)
      allocate(HGW1(MEXPPAR) , STAT = my)
      myalloc = myalloc + my
      HGW1 = 0.0D0
 
      if(allocated(DHGw1))deallocate(DHGw1)
      allocate(DHGw1(MEXPPAR) , STAT = my)
      myalloc = myalloc + my
      DHGw1 = 0.0D0
 
      if(allocated(HGW2))deallocate(HGW2)
      allocate(HGW2(MEXPPAR) , STAT = my)
      myalloc = myalloc + my
      HGW2 = 0.0D0
 
      if(allocated(DHGw2))deallocate(DHGw2)
      allocate(DHGw2(MEXPPAR) , STAT = my)
      myalloc = myalloc + my
      DHGw2 = 0.0D0
 
      if(allocated(NNA))deallocate(NNA)
      allocate(NNA(MEXPPAR) , STAT = my)
      myalloc = myalloc + my
      NNA = 0
 
      if(allocated(NNG))deallocate(NNG)
      allocate(NNG(MEXPPAR) , STAT = my)
      myalloc = myalloc + my
      NNG = 0
 
      if(allocated(NNZ))deallocate(NNZ)
      allocate(NNZ(MEXPPAR) , STAT = my)
      myalloc = myalloc + my
      NNZ = 0
 
      if(allocated(HBEtagfl))deallocate(HBEtagfl)
      allocate(HBEtagfl(MDEF) , STAT = my)
      myalloc = myalloc + my
      HBEtagfl = 0.D0
 
      if(allocated(HENergygfl))deallocate(HENergygfl)
      allocate(HENergygfl(MDEF) , STAT = my)
      myalloc = myalloc + my
      HENergygfl = 0.D0
 
      if(allocated(NZRam))deallocate(NZRam)
      allocate(NZRam(MDEF) , STAT = my)
      myalloc = myalloc + my
      NZRam = 0
 
      if(allocated(NARam))deallocate(NARam)
      allocate(NARam(MDEF) , STAT = my)
      myalloc = myalloc + my
      NARam = 0
 
      if(allocated(NANa))deallocate(NANa)
      allocate(NANa(MEXP) , STAT = my)
      myalloc = myalloc + my
      NANa = 0
 
      if(allocated(NANz))deallocate(NANz)
      allocate(NANz(MEXP) , STAT = my)
      myalloc = myalloc + my
      NANz = 0
 
      if(allocated(HALpha2))deallocate(HALpha2)
      allocate(HALpha2(MEXP) , STAT = my)
      myalloc = myalloc + my
      HALpha2 = 0.D0
 
      if(allocated(ETAt))deallocate(ETAt)
      allocate(ETAt(MAXGDR) , STAT = my)
      myalloc = myalloc + my
      ETAt = 0.D0
 
      if(allocated(HE1t))deallocate(HE1t)
      allocate(HE1t(MAXGDR) , STAT = my)
      myalloc = myalloc + my
      HE1t = 0.D0
 
      if(allocated(HE2t))deallocate(HE2t)
      allocate(HE2t(MAXGDR) , STAT = my)
      myalloc = myalloc + my
      HE2t = 0.D0
 
      if(allocated(HGW1t))deallocate(HGW1t)
      allocate(HGW1t(MAXGDR) , STAT = my)
      myalloc = myalloc + my
      HGW1t = 0.D0
 
      if(allocated(HGW2t))deallocate(HGW2t)
      allocate(HGW2t(MAXGDR) , STAT = my)
      myalloc = myalloc + my
      HGW2t = 0.D0
 
      if(allocated(NNAt))deallocate(NNAt)
      allocate(NNAt(MAXGDR) , STAT = my)
      myalloc = myalloc + my
      NNAt = 0
 
      if(allocated(NNGt))deallocate(NNGt)
      allocate(NNGt(MAXGDR) , STAT = my)
      myalloc = myalloc + my
      NNGt = 0
 
      if(allocated(NNZt))deallocate(NNZt)
      allocate(NNZt(MAXGDR) , STAT = my)
      myalloc = myalloc + my
      NNZt = 0
 
      if(myalloc/=0)then
         write(8 , *)'ERROR: INSUFFICIENT MEMORY FOR EMPGDR'
         write(12 , *)'ERROR: INSUFFICIENT MEMORY FOR EMPGDR'
         stop 'ERROR: INSUFFICIENT MEMORY FOR EMPGDR'
      endif

   end subroutine EMPAGDR

   subroutine EMPDgdr()
      implicit none
 
      if(allocated(HCS1))deallocate(HCS1)
      if(allocated(HCS2))deallocate(HCS2)
      if(allocated(HE1))deallocate(HE1)
      if(allocated(HE2))deallocate(HE2)
      if(allocated(HGW1))deallocate(HGW1)
      if(allocated(HGW2))deallocate(HGW2)
 
      if(allocated(DHCs1))deallocate(DHCs1)
      if(allocated(DHCs2))deallocate(DHCs2)
      if(allocated(DHE1))deallocate(DHE1)
      if(allocated(DHE2))deallocate(DHE2)
      if(allocated(DHGw1))deallocate(DHGw1)
      if(allocated(DHGw2))deallocate(DHGw2)
 
      if(allocated(NNA))deallocate(NNA)
      if(allocated(NNG))deallocate(NNG)
      if(allocated(NNZ))deallocate(NNZ)
 
      if(allocated(HBEtagfl))deallocate(HBEtagfl)
      if(allocated(HENergygfl))deallocate(HENergygfl)
      if(allocated(NZRam))deallocate(NZRam)
      if(allocated(NARam))deallocate(NARam)
 
      if(allocated(NANa))deallocate(NANa)
      if(allocated(NANz))deallocate(NANz)
      if(allocated(HALpha2))deallocate(HALpha2)
 
      if(allocated(ETAt))deallocate(ETAt)
      if(allocated(HE1t))deallocate(HE1t)
      if(allocated(HE2t))deallocate(HE2t)
      if(allocated(HGW1t))deallocate(HGW1t)
      if(allocated(HGW2t))deallocate(HGW2t)
 
      if(allocated(NNAt))deallocate(NNAt)
      if(allocated(NNGt))deallocate(NNGt)
      if(allocated(NNZt))deallocate(NNZt)

   end subroutine EMPDGDR
end module EMPGDR

