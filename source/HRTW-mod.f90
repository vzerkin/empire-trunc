!   $Rev: 3952 $
!   $Author: mherman $
!   $Date: 2014-04-30 08:47:48 +0200 (Wed, 30 Apr 2014) $
!
module HRTW_mod

   implicit none

   type channel
      integer*4 l         !ejectile l
      real*8 j            !ejectile j
      real*8 t            !ejectile Tlj
      real*8 ti1          !temporary Tlj used for iteration
      real*8 ti2          !temporary Tlj used for iteration
      real*8 rho          !final level density for this channel
      real*8 eef          !elastic enhancement factor
      integer*4 nejc      !ejectile index (nejc)
      integer*4 kres      !populated energy bin (negative for discrete levels, g.s. -1,...)
      real*8 xjrs         !spin of the populated state
      integer*4 jres      !spin index of the populated state
      integer*4 pres      !parity index of the populated state
    end type channel

    type numchnl
       integer*4 neut     !number of neutron channels, i.e., number of neutron entries in the 'channel' type
       integer*4 part     !number of particle channels, i.e., number of particle entries in the 'channel' type
       integer*4 elal     !position of the first (low) elastic channel
       integer*4 elah     !position of the last elastic channel; elastics are embedded in particle channels elal<=elah<=part
       integer*4 fiss     !effective number of fission channels
       integer*4 gamm     !effective number of gamma channels
    end type numchnl

    type fusion
      integer*4 nout      !position of the corresponding outgoing channel in outchnl
      integer*4 l         !projectile l
      real*8 j            !projectile j
      real*8 t            !projectile Tlj
      real*8 sig          !absorption x-section for this channel
    end type fusion

   integer*4, parameter :: ndhrtw1 = 4000
   integer*4, parameter :: ndhrtw2 = 30


   real*8 :: H_Sumtl      !Sum of strong Tlj
   real*8 :: H_Sumtls     !Sum of strong Tlj**2
   real*8 :: H_Sweak      !Sum of weak Tlj
   real*8 :: H_Sweaks     !Sum of weak Tlj**2
   real*8 :: H_Tav        !Avarage strong Tlj
   real*8 :: H_Tthr       !Thershold for Tlj to be considered strong
   real*8 :: TFIs         !Sum of fission transmission coefficients
   real*8 :: TGam         !Sum of gamma transmission coefficients
   integer*4 :: NCH       !Number of strong channels (Tlj's)
   integer*4 :: NSCh      !Number of strong  Tlj processed by VT routine, i.e. poistion in H_Tl matrix
   integer*4, allocatable :: MEMel(:,:)
   real*8, allocatable :: H_Tl(:,:)              !strong transmission coefficients LIKELY TO GET RID OFF!!!
   real*8, allocatable :: H_Abs(:,:)
   type(channel), allocatable :: outchnl(:)      !outgoing channels
   type(fusion), allocatable :: inchnl(:)        !fusion channels
   type(numchnl) :: num                          !number of particular channels

contains

   subroutine AllocHRTW(nd1,nd2)
      implicit none
      integer :: myalloc,my
      integer*4, intent(in), optional :: nd1, nd2
      integer*4 :: ndch, ndfus
      myalloc = 0
      ndch = ndhrtw1
      ndfus = ndhrtw2

      if(present(nd1)) ndch = nd1
      if(present(nd2)) ndfus = nd2

      if(allocated(MEMel)) deallocate(MEMel)
      allocate(MEMel(ndfus,3),stat=my)
      myalloc=myalloc+my
      MEMel = 0.0d0

      if(allocated(H_Tl)) deallocate(H_Tl)
      allocate(H_Tl(ndch,2),stat=my)
      myalloc=myalloc+my
      H_Tl = 0.0d0

      if(allocated(H_Abs)) deallocate(H_Abs)
      allocate(H_Abs(ndfus,3),stat=my)
      myalloc=myalloc+my
      H_Abs = 0.0d0

      if(allocated(outchnl)) deallocate(outchnl)
      allocate(outchnl(ndch),stat=my)
      myalloc=myalloc+my
      outchnl%l = 0
      outchnl%j = 0.d0
      outchnl%t = 0.d0
      outchnl%ti1 = 0.d0
      outchnl%ti2 = 0.d0
      outchnl%rho = 0.d0
      outchnl%eef = 1.d0
      outchnl%nejc = 0
      outchnl%kres = 0
      outchnl%xjrs = 0.d0
      outchnl%jres = 0
      outchnl%pres = 0

      if(allocated(inchnl)) deallocate(inchnl)
      allocate(inchnl(ndfus),stat=my)
      myalloc=myalloc+my
      inchnl%nout = 0
      inchnl%l = 0
      inchnl%j = 0.d0
      inchnl%t = 0.d0
      inchnl%sig = 0.d0


      if(myalloc/=0) then
         write(8,*)  'ERROR: Insufficient memory for HRTW'
         write(12,*) 'ERROR: Insufficient memory for HRTW'
         stop 'ERROR: Insufficient memory for HRTW'
      endif
   end subroutine AllocHRTW

   subroutine DelHRTW()
      implicit none
      if(allocated(MEMel)) deallocate(MEMel)
      if(allocated(H_Tl)) deallocate(H_Tl)
      if(allocated(H_Abs)) deallocate(H_Abs)
      if(allocated(outchnl)) deallocate(outchnl)
      if(allocated(inchnl)) deallocate(inchnl)
   end subroutine DelHRTW

   function ZCoefficient( l1, j1, l2, j2, s, ll ) result (retval)
      !
      ! Blatt & Biedenharn's definition in Rev. Mod. Phys. 24, 258 (1952)
      !
      real*8, intent(in) :: l1, j1, l2, j2, s, ll
      real*8 :: retval
      real*8, external :: CLEBG, RACAH
      retval = sqrt( ( 2.0d0 * l1 + 1.0d0 ) * &
        ( 2.0d0 * l2 + 1.0d0 ) * &
        ( 2.0d0 * j1 + 1.0d0 ) * &
        ( 2.0d0 * j2 + 1.0d0 ) ) * &
        CLEBG( l1, l2, ll, 0d0, 0d0, 0d0 ) * &
        RACAH( l1, j1, l2, j2, s, ll )
      if ( mod( ll - l1 + l2, 4d0 ) > 0.1d0 ) retval = -retval
   end function ZCoefficient

   function ZBarCoefficient( l1, j1, l2, j2, s, ll ) result (retval)
      !
      ! Lane & Thomas's Zbar-coefficient coefficient
      !   = Zbar(l1  j1  l2  j2 | S L )
      !   = (-i)^( -l1 + l2 + ll ) * Z(l1  j1  l2  j2 | S L )
      !
      ! Lane & Thomas Rev. Mod. Phys. 30, 257-353 (1958).
      ! Note, Lane & Thomas define this because they did not like the different phase convention in Blatt & Biedenharn's Z coefficient.  They changed it to get better time-reversal behavior.
      ! Froehner uses Lane & Thomas convention as does T. Kawano.
      !
      real*8, intent(in) :: l1, j1, l2, j2, s, ll
      real*8 :: retval
      real*8, external :: CLEBG, RACAH
      real*8 :: rc, cc
      retval = 0.d0

      cc = CLEBG( l1, l2, ll, 0d0, 0d0, 0d0 )
      if(cc==0.d0) return
      rc = RACAH( l1, j1, l2, j2, s, ll )
      if(rc==0.d0) return

      retval = sqrt( ( 2.0d0 * l1 + 1.0d0 ) * &
        ( 2.0d0 * l2 + 1.0d0 ) * &
        ( 2.0d0 * j1 + 1.0d0 ) * &
        ( 2.0d0 * j2 + 1.0d0 ) ) * cc * rc
        
   end function ZBarCoefficient

   real*8 function Blatt(J,Ia,la,ja,sa,Ib,lb,jb,sb,L)
      !
      ! Blatt-Biedenharn formula for Compound Nuclei (as written by Froebrich & Lipperheide "Theory of Nuclear Reactions", Oxford Science (1996) p. 328),
      ! but substantially revised because of the different angular momentum coupling scheme in EMPIRE
      !
      implicit none
      real*8 :: zb1, zb2, rc1, rc2
      real*8, intent(in) :: J           !CN spin
      real*8, intent(in) :: Ia          !target spin
      real*8, intent(in) :: la          !projectile l
      real*8, intent(in) :: ja          !total projectile angular momentum l+s
      real*8, intent(in) :: sa          !projectile spin
      real*8, intent(in) :: Ib          !residual nucleus spin
      real*8, intent(in) :: lb          !ejectile l
      real*8, intent(in) :: jb          !total ejectile angular momentum l+s
      real*8, intent(in) :: sb          !ejectile spin
      real*8, intent(in) :: L           !Legendre polynomial order (P_L)
      real*8, parameter :: pi4=12.5663706144d0   !4*pi
      real*8, external :: RACAH
      Blatt = 0.d0

      rc1   = RACAH( ja, J, ja, J, Ia, L )
      if (rc1==0.d0) return

      rc2   = RACAH( jb, J, jb, J, Ib, L )
      if (rc2==0.d0) return

      zb1   = ZBarCoefficient( la, ja, la, ja, sa, L )
      if (zb1==0.d0) return

      zb2   = ZBarCoefficient( lb, jb, lb, jb, sb, L )
      if (zb2==0.d0) return

      Blatt =  (2.0d0 * J + 1.d0 )* zb1*zb2*rc1*rc2/pi4 * NINT((-1.d0)**NINT(-Ia - sa + Ib + sb + 2.d0*( ja + jb ) ))

!     Blatt =  (2.0d0 * J + 1.d0 ) &
!               * ZBarCoefficient( la, ja, la, ja, sa, L ) &
!               * ZBarCoefficient( lb, jb, lb, jb, sb, L ) &
!               * RACAH( ja, J, ja, J, Ia, L ) &
!               * RACAH( jb, J, jb, J, Ib, L ) / pi4

!     Blatt = Blatt* (-1.d0)**INT(-Ia - sa + Ib + sb + 2.d0*( ja + jb ) ) 
      return
    end function Blatt
    
end module HRTW_mod
