!cc   * $Rev: 4504 $
!cc   * $Author: mherman $
!cc   * $Date: 2015-11-20 23:29:16 +0100 (Fr, 20 Nov 2015) $

      MODULE empgdr

      INTEGER MAXGDR,MEXPPAR,MEXP,MDEF
      PARAMETER (MAXGDR=5986,MEXPPAR=270,MEXP=9000,MDEF=700)

C     HCS1(270),HCS2(270),HE1(270),HE2(270),HGW1(270),HGW2(270)
C     dHCS1(270),dHCS2(270),dHE1(270),dHE2(270),dHGW1(270),dHGW2(270)
      DOUBLE PRECISION, ALLOCATABLE ::  HCS1(:), HCS2(:), HE1(:), HE2(:)
      DOUBLE PRECISION, ALLOCATABLE :: dHCS1(:),dHCS2(:),dHE1(:),dHE2(:)
      DOUBLE PRECISION, ALLOCATABLE ::  HGW1(:), HGW2(:)
      DOUBLE PRECISION, ALLOCATABLE :: dHGW1(:),dHGW2(:)
C     NNA(270),NNG(270),NNZ(270)
      INTEGER, ALLOCATABLE :: NNA(:),NNG(:),NNZ(:)

C     HBEtagfl(700),HENergygfl(700),NARam(700),NZRam(700)
      DOUBLE PRECISION, ALLOCATABLE :: HBEtagfl(:),HENergygfl(:)
      INTEGER, ALLOCATABLE :: NARam(:),NZRam(:)

C     NANa(9000),NANz(9000),HALpha2(9000)
      DOUBLE PRECISION, ALLOCATABLE :: halpha2(:)
      INTEGER, ALLOCATABLE :: nanz(:),nana(:)
C     etat(MAXGDR),he1t(MAXGDR),he2t(MAXGDR),hgw1t(MAXGDR),hgw2t(MAXGDR)
      DOUBLE PRECISION, ALLOCATABLE :: etat(:)
      DOUBLE PRECISION, ALLOCATABLE :: he1t(:),he2t(:),hgw1t(:),hgw2t(:)
C     nnat(MAXGDR), nngt(MAXGDR), nnzt(MAXGDR)
      INTEGER, ALLOCATABLE :: nnzt(:),nnat(:),nngt(:)

      CONTAINS

      SUBROUTINE EMPAgdr()

      implicit none

      INTEGER :: myalloc,my

      myalloc = 0

C     HCS1(MEXPPAR),HCS2(MEXPPAR),HE1(MEXPPAR),HE2(MEXPPAR)
C     HGW1(MEXPPAR),HGW2(MEXPPAR)
      if(allocated(HCS1)) deallocate(HCS1)
      ALLOCATE(HCS1(MEXPPAR),STAT=my)
      myalloc=myalloc+my
      HCS1 = 0.0d0

      if(allocated(dHCS1)) deallocate(dHCS1)
      ALLOCATE(dHCS1(MEXPPAR),STAT=my)
      myalloc=myalloc+my
      dHCS1 = 0.0d0

      if(allocated(HCS2)) deallocate(HCS2)
      ALLOCATE(HCS2(MEXPPAR),STAT=my)
      myalloc=myalloc+my
      HCS2 = 0.0d0

      if(allocated(dHCS2)) deallocate(dHCS2)
      ALLOCATE(dHCS2(MEXPPAR),STAT=my)
      myalloc=myalloc+my
      dHCS2 = 0.0d0

      if(allocated(HE1)) deallocate(HE1)
      ALLOCATE(HE1(MEXPPAR),STAT=my)
      myalloc=myalloc+my
      HE1 = 0.0d0

      if(allocated(dHE1)) deallocate(dHE1)
      ALLOCATE(dHE1(MEXPPAR),STAT=my)
      myalloc=myalloc+my
      dHE1 = 0.0d0

      if(allocated(HE2)) deallocate(HE2)
      ALLOCATE(HE2(MEXPPAR),STAT=my)
      myalloc=myalloc+my
      HE2 = 0.0d0

      if(allocated(dHE2)) deallocate(dHE2)
      ALLOCATE(dHE2(MEXPPAR),STAT=my) 
      myalloc=myalloc+my
      dHE2 = 0.0d0

      if(allocated(HGW1)) deallocate(HGW1)
      ALLOCATE(HGW1(MEXPPAR),STAT=my)
      myalloc=myalloc+my
      HGW1 = 0.0d0

      if(allocated(dHGW1)) deallocate(dHGW1)
      ALLOCATE(dHGW1(MEXPPAR),STAT=my)
      myalloc=myalloc+my
      dHGW1 = 0.0d0

      if(allocated(HGW2)) deallocate(HGW2)
      ALLOCATE(HGW2(MEXPPAR),STAT=my)
      myalloc=myalloc+my
      HGW2 = 0.0d0

      if(allocated(dHGW2)) deallocate(dHGW2)
      ALLOCATE(dHGW2(MEXPPAR),STAT=my)
      myalloc=myalloc+my
      dHGW2 = 0.0d0

C     NNA(MEXPPAR),NNG(MEXPPAR),NNZ(MEXPPAR)
      if(allocated(NNA)) deallocate(NNA)
      ALLOCATE(NNA(MEXPPAR),STAT=my)
      myalloc=myalloc+my
      NNA = 0

      if(allocated(NNG)) deallocate(NNG)
      ALLOCATE(NNG(MEXPPAR),STAT=my)
      myalloc=myalloc+my
      NNG = 0

      if(allocated(NNZ)) deallocate(NNZ)
      ALLOCATE(NNZ(MEXPPAR),STAT=my)
      myalloc=myalloc+my
      NNZ = 0

C     HBEtagfl(MDEF),HENergygfl(MDEF),NARam(MDEF),NZRam(MDEF)
      if(allocated(HBEtagfl)) deallocate(HBEtagfl)
      ALLOCATE(HBEtagfl(MDEF),STAT=my)
      myalloc=myalloc+my
      HBEtagfl = 0.d0

      if(allocated(HENergygfl)) deallocate(HENergygfl)
      ALLOCATE(HENergygfl(MDEF),STAT=my)
      myalloc=myalloc+my
      HENergygfl = 0.d0

      if(allocated(NZRam)) deallocate(NZRam)
      ALLOCATE(NZRam(MDEF),STAT=my)
      myalloc=myalloc+my
      NZRam = 0

      if(allocated(NARam)) deallocate(NARam)
      ALLOCATE(NARam(MDEF),STAT=my)
      myalloc=myalloc+my
      NARam = 0

C     NANa(MEXP),NANz(MEXP),HALpha2(MEXP)
      if(allocated(NANa)) deallocate(NANa)
      ALLOCATE(NANa(MEXP),STAT=my)
      myalloc=myalloc+my
      NANa = 0

      if(allocated(NANz)) deallocate(NANz)
      ALLOCATE(NANz(MEXP),STAT=my)
      myalloc=myalloc+my
      NANz = 0

      if(allocated(halpha2)) deallocate(halpha2)
      ALLOCATE(halpha2(MEXP),STAT=my)
      myalloc=myalloc+my
      halpha2 = 0.d0

C     etat(MAXGDR),he1t(MAXGDR),he2t(MAXGDR),hgw1t(MAXGDR),hgw2t(MAXGDR)
      if(allocated(etat)) deallocate(etat)
      ALLOCATE(etat(MAXGDR),STAT=my)
      myalloc=myalloc+my
      etat = 0.d0

      if(allocated(he1t)) deallocate(he1t)
      ALLOCATE(he1t(MAXGDR),STAT=my)
      myalloc=myalloc+my
      he1t = 0.d0

      if(allocated(he2t)) deallocate(he2t)
      ALLOCATE(he2t(MAXGDR),STAT=my)
      myalloc=myalloc+my
      he2t = 0.d0

      if(allocated(hgw1t)) deallocate(hgw1t)
      ALLOCATE(hgw1t(MAXGDR),STAT=my)
      myalloc=myalloc+my
      hgw1t = 0.d0

      if(allocated(hgw2t)) deallocate(hgw2t)
      ALLOCATE(hgw2t(MAXGDR),STAT=my)
      myalloc=myalloc+my
      hgw2t = 0.d0

C     nnat(MAXGDR), nngt(MAXGDR), nnzt(MAXGDR)
      if(allocated(nnat)) deallocate(nnat)
      ALLOCATE(nnat(MAXGDR),STAT=my)
      myalloc=myalloc+my
      nnat = 0

      if(allocated(nngt)) deallocate(nngt)
      ALLOCATE(nngt(MAXGDR),STAT=my)
      myalloc=myalloc+my
      nngt = 0

      if(allocated(nnzt)) deallocate(nnzt)
      ALLOCATE(nnzt(MAXGDR),STAT=my)
      myalloc=myalloc+my
      nnzt = 0

      IF(myalloc.NE.0) THEN
        WRITE(8,*)  
     &    'ERROR: Insufficient memory for EMPgdr'
        WRITE(12,*) 
     &    'ERROR: Insufficient memory for EMPgdr'
        STOP 
     &    'ERROR: Insufficient memory for EMPgdr'
      ENDIF     

      END SUBROUTINE EMPAgdr

      SUBROUTINE EMPDgdr()

C     HCS1(MEXPPAR),HCS2(MEXPPAR),HE1(MEXPPAR),HE2(MEXPPAR)
C     HGW1(MEXPPAR),HGW2(MEXPPAR)
      if(allocated(HCS1)) deallocate(HCS1)
      if(allocated(HCS2)) deallocate(HCS2)
      if(allocated(HE1)) deallocate(HE1)
      if(allocated(HE2)) deallocate(HE2)
      if(allocated(HGW1)) deallocate(HGW1)
      if(allocated(HGW2)) deallocate(HGW2)

      if(allocated(dHCS1)) deallocate(dHCS1)
      if(allocated(dHCS2)) deallocate(dHCS2)
      if(allocated(dHE1)) deallocate(dHE1)
      if(allocated(dHE2)) deallocate(dHE2)
      if(allocated(dHGW1)) deallocate(dHGW1)
      if(allocated(dHGW2)) deallocate(dHGW2)

C     NNA(MEXPPAR),NNG(MEXPPAR),NNZ(MEXPPAR)
      if(allocated(NNA)) deallocate(NNA)
      if(allocated(NNG)) deallocate(NNG)
      if(allocated(NNZ)) deallocate(NNZ)

C     HBEtagfl(MDEF),HENergygfl(MDEF),NARam(MDEF),NZRam(MDEF)
      if(allocated(HBEtagfl)) deallocate(HBEtagfl)
      if(allocated(HENergygfl)) deallocate(HENergygfl)
      if(allocated(NZRam)) deallocate(NZRam)
      if(allocated(NARam)) deallocate(NARam)

C     NANa(MEXP),NANz(MEXP),HALpha2(MEXP)
      if(allocated(NANa)) deallocate(NANa)
      if(allocated(NANz)) deallocate(NANz)
      if(allocated(halpha2)) deallocate(halpha2)

C     etat(MAXGDR),he1t(MAXGDR),he2t(MAXGDR),hgw1t(MAXGDR),hgw2t(MAXGDR)
      if(allocated(etat)) deallocate(etat)
      if(allocated(he1t)) deallocate(he1t)
      if(allocated(he2t)) deallocate(he2t)
      if(allocated(hgw1t)) deallocate(hgw1t)
      if(allocated(hgw2t)) deallocate(hgw2t)

C     nnat(MAXGDR), nngt(MAXGDR), nnzt(MAXGDR)
      if(allocated(nnat)) deallocate(nnat)
      if(allocated(nngt)) deallocate(nngt)
      if(allocated(nnzt)) deallocate(nnzt)

      END SUBROUTINE EMPDgdr

      END MODULE empgdr
