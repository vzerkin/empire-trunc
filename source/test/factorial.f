      subroutine FCT()
c***********************************************************************
c  Calculate factorial logarithms from 0! ( =1.) up to (idim-1)!       *
c                  k! = fact(k+1)                                      *
c***********************************************************************
c
      implicit none
      include 'dimension.h' 
      integer          k
      double precision zero
      data             zero /0.0d+00/
	  double precision FFFact(6*NDLW)
      common /factorial/FFFact
c=======================================================================
      FFFact(1) = zero
      FFFact(2) = zero
      do k=3,6*NDLW
        FFFact(k) = FFFact(k-1) + DLOG(DBLE(k-1))
      end do
      return
      end
