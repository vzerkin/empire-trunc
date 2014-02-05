!cc   * $Rev: 3705 $
!cc   * $Author: rcapote $
!cc   * $Date: 2014-01-04 22:01:02 +0100 (Sat, 04 Jan 2014) $

      subroutine defcal(z,a,beta2,beta3)

!     Subroutine that calculates the quadrupole and octupole deformation
!     parameters base on the systematics of the deformation lengths and 
!     radii of charge, as described in Phys. Rev. C 70, 014604 (2004) (L.C. 
!     Chamon, G.P.A. Nobre, D. Pereira et al.) and Phys. Rev. C 76, 024605 
!     (2007) (G.P.A. Nobre, C.P. Silva, L.C. Chamon, B.V. Carlson)


      implicit none

      integer :: z, a, n
      real, parameter :: alpha=3.2d0 !fm
      real(kind=8) :: r0c, beta2, beta3, delta2, delta3,d2z,d2n

      real, dimension(2:156) :: D2

      D2(2)   = 2.61; D2(4)   = 2.30; D2(6)   = 0.98; D2(8)   = 0.37
      D2(10)  = 1.36; D2(12)  = 1.18; D2(14)  = 0.57; D2(16)  = 0.65
      D2(18)  = 0.54; D2(20)  = 0.27; D2(22)  = 0.64; D2(24)  = 0.78
      D2(26)  = 0.64; D2(28)  = 0.30; D2(30)  = 0.60; D2(32)  = 0.60
      D2(34)  = 0.81; D2(36)  = 0.70; D2(38)  = 0.67; D2(40)  = 0.70
      D2(42)  = 0.75; D2(44)  = 0.67; D2(46)  = 0.43; D2(48)  = 0.30
      D2(50)  = 0.00; D2(52)  = 0.28; D2(54)  = 0.62; D2(56)  = 0.77
      D2(58)  = 0.83; D2(60)  = 0.95; D2(62)  = 0.93; D2(64)  = 0.99
      D2(66)  = 1.10; D2(68)  = 0.99; D2(70)  = 0.91; D2(72)  = 0.80
      D2(74)  = 0.63; D2(76)  = 0.43; D2(78)  = 0.26; D2(80)  = 0.00
      D2(82)  = 0.00; D2(84)  = 0.02; D2(86)  = 0.11; D2(88)  = 0.39
      D2(90)  = 0.73; D2(92)  = 1.06; D2(94)  = 1.21; D2(96)  = 1.27
      D2(98)  = 1.34; D2(100) = 1.13; D2(102) = 1.18; D2(104) = 1.18
      D2(106) = 1.21; D2(108) = 1.09; D2(110) = 1.01; D2(112) = 0.82
      D2(114) = 0.84; D2(116) = 0.64; D2(118) = 0.63; D2(120) = 0.59
      D2(122) = 0.54; D2(124) = 0.51; D2(126) = 0.08; D2(128) = 0.23
      D2(130) = 0.59; D2(132) = 0.68; D2(134) = 0.87; D2(136) = 0.99
      D2(138) = 1.05; D2(140) = 1.17; D2(142) = 1.31; D2(144) = 1.06
      D2(146) = 1.07; D2(148) = 1.10; D2(150) = 1.10; D2(152) = 1.05
      D2(154) = 1.06; D2(156) = 0.98
      
      n=a-z

      beta2=0.d0
      beta3=0.d0

      if(z<2.or.z>156.or.n<2.or.n>156) return

      if(mod(z,2)==0) then
        d2z=D2(z)
      else
        d2z=(D2(z+1)+D2(z-1))/2.d0
      endif

      if(mod(n,2)==0) then
        d2n=D2(n)
      else
        d2n=(D2(n+1)+D2(n-1))/2.d0
      endif

      delta2=d2z+d2n
      delta3=alpha*(z**(-0.5d0)+n**(-0.5d0))

      r0c=1.76d0*z**(1.d0/3.d0)-0.96 !fm

      beta2=delta2/r0c
      beta3=delta3/r0c

      return
      end