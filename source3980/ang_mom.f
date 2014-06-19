Ccc   * $Rev: 3812 $
Ccc   * $Author: rcapote $
Ccc   * $Date: 2014-02-06 21:08:38 +0100 (Thu, 06 Feb 2014) $
C
C
      double precision function RACAH(a,b,c,d,e,f)
c***********************************************************************
c  Calculate Racah coefficients      w(a,b,c,d;e,f)                    *
c                                                                      *
c  Attention:   w(a,b,c,d;e,f) = (-1)**(a+b+c+d)*6-J(a,b,e;d,c,f)      *
c                                                ---                   *
c                                                                      *
c  from John.G.Wills     ORNL-TM-1949 (August 1967)                    *
c                    and Comp.Phys.Comm. 2(1971)381                    *
c                                                                      *
c  O.Bersillon     August 1977                                         *
c                                                                      *
c***********************************************************************
c
      implicit none
      include 'dimension.h' 

	   double precision g(6*NDLW)
      common /factorial/g
c
      double precision a,b,c,d,e,f
c
      double precision h,o,p,q,r,s,t,v,w,x,y,z
      integer          i
      integer          i1,i2,i3,i4,i5,i6,i7,i8,i9,i10,i11,i12,i13
     *                ,i14,i15,i16,il
      integer          ja,jb,jc,jd,je,jf
      integer          j1,j2,j3,j4,j5,j6,j7
      integer          j,k,n
c
      dimension        i(16)
      equivalence
     *(i( 1), i1),(i( 2), i2),(i( 3), i3),(i( 4), i4),(i( 5), i5),
     *(i( 6), i6),(i( 7), i7),(i( 8), i8),(i( 9), i9),(i(10),i10),
     *(i(11),i11),(i(12),i12),(i(13),i13),(i(14),i14),(i(15),i15),
     *(i(16),i16)
c
      double precision zero,eps,one,two
      data             zero /0.0d+00/
      data             eps  /1.0d-10/
      data             one  /1.0d+00/
      data             two  /2.0d+00/
c=======================================================================
c
      racah = zero
c
c  Convert arguments to integer and make useful combinations
c
      ja  = IDINT(two*a + eps)
      jb  = IDINT(two*b + eps)
      jc  = IDINT(two*c + eps)
      jd  = IDINT(two*d + eps)
      je  = IDINT(two*e + eps)
      jf  = IDINT(two*f + eps)
      i1  = ja + jb - je
      i2  = jb + je - ja
      i3  = je + ja - jb
      i4  = jc + jd - je
      i5  = jd + je - jc
      i6  = je + jc - jd
      i7  = ja + jc - jf
      i8  = jc + jf - ja
      i9  = jf + ja - jc
      i10 = jb + jd - jf
      i11 = jd + jf - jb
      i12 = jf + jb - jd
      i13 = ja + jb + je
      i14 = jc + jd + je
      i15 = ja + jc + jf
      i16 = jb + jd + jf
c
c  Check triangular inequalities, find no. of terms in sum,
c    divide I's by 2
c
      n = i16
      do j=1,12
        k = i(j)/2
        if(i(j) .ne. 2*k) return
        if(k .lt. 0) return
        if(k .lt. n) then
          n = k
        endif
        i(j) = k + 1
      end do
c
c  Find minimum value of summation index
c
      il = 0
      do j=13,16
        i(j) = i(j)/2
        if(il .lt. i(j)) then
          il = i(j)
        endif
      end do
      j1 = il  - i13 + 1
      j2 = il  - i14 + 1
      j3 = il  - i15 + 1
      j4 = il  - i16 + 1
      j5 = i13 + i4  - il
      j6 = i15 + i5  - il
      j7 = i16 + i6  - il
      h  = -      DEXP((g(i1)+g(i2)+g(i3)-g(i13+2)+g(i4)+g(i5)+g(i6)-
     *g(i14+2)+g(i7)+g(i8)+g(i9)-g(i15+2)+g(i10)+g(i11)+g(i12)-g(i16+2))
     */two+g(il+2)-g(j1)-g(j2)-g(j3)-g(j4)-g(j5)-g(j6)-g(j7))
      if((j5 - 2*(j5/2)) .ne. 0) h = -h
      if(n .lt. 0) return
      if(n .eq. 0) then
        racah = h
        return
      else
c
        s = one
        q = n  - 1
        p = il + 2
        r = j1
        o = j2
        v = j3
        w = j4
        x = j5 - 1
        y = j6 - 1
        z = j7 - 1
        do j=1,n
          t = (p+q)/(r+q)*(x-q)/(o+q)*(y-q)/(v+q)*(z-q)/(w+q)
          s = one - s*t
          q = q   - one
        end do
        racah = h*s
      endif
c
      return
      end

      double precision function CLEBG(aj1,aj2,aj3,am1,am2,am3)
c***********************************************************************
c  Calculate Clebsch-Gordan coefficients                               *
c                                                                      *
c  Attention:  cg(j1,j2,j3,;m1,m2,m3) = (-1)**(j1+j2-m3)*              *
c                                       3-J(j1,j2,j3;m1,m2,-m3)        *
c                                       ---                            *
c                                                                      *
c  from    John.G. Wills     ORNL-TM-1949 (August 1967)                *
c                         et Comp.Phys.Comm. 2(1971)381                *
c                                                                      *
c  O.Bersillon     August 1977                                         *
c                                                                      *
c***********************************************************************
c
      implicit none

      include 'dimension.h' 
	   double precision g(6*NDLW)
      common /factorial/g
c
      double precision aj1,aj2,aj3,am1,am2,am3
c
      double precision a,b,c,d,e,f,h,q,s,t,x
      integer          i
      integer          i1,i2,i3,i4,i5,i6,i7,i8,i9,i10,i11,il
      integer          j,j1,j2,j3,k,l,la,lb,m,m1,m2,m3,n
c
      dimension        i(11)
      equivalence
     *(i(1),i1),(i(2),i2),(i(3),i3),(i( 4), i4),(i( 5), i5),(i(6),i6),
     *(i(7),i7),(i(8),i8),(i(9),i9),(i(10),i10),(i(11),i11)
c
      double precision zero,eps,one,two
      data             zero /0.0d+00/
      data             eps  /1.0d-10/
      data             one  /1.0d+00/
      data             two  /2.0d+00/
c=======================================================================
c
      clebg = zero
c
c  Convert the arguments to integer
c
      j1 = IDINT(two*aj1 + eps)
      j2 = IDINT(two*aj2 + eps)
      j3 = IDINT(two*aj3 + eps)
      m1 = IDINT(two*am1 + SIGN(eps,am1))
      m2 = IDINT(two*am2 + SIGN(eps,am2))
      m3 = IDINT(two*am3 + SIGN(eps,am3))
c
c  Test m1 + m2 = m3
c
      if(m1 + m2 - m3 .ne. 0) return
c
c  Test table size
c
      i(10) = (j1 + j2 + j3)/2 + 2
      n     = i(10)
      i(11) = j3 + 2
      if(i(10) .gt. 6*NDLW) then
        WRITE(8,9010) i(10),6*NDLW,aj1,aj2,aj3,am1,am2,am3
 9010   format(' ERROR: MEMORY OVERFLOW IN CLEBG ',2i5,6f5.1,
     *         ' INCREASE NDLW in dimension.h' )
	   STOP ' ERROR: MEMORY OVERFLOW IN CLEBG, INCREASE NDLW'
      endif
c
      i(1) = j1 + j2 - j3
      i(2) = j2 + j3 - j1
      i(3) = j3 + j1 - j2
      i(4) = j1 - m1
      i(5) = j1 + m1
      i(6) = j2 - m2
      i(7) = j2 + m2
      i(8) = j3 - m3
      i(9) = j3 + m3
c
c  Check i(j) = even, triangular inequality, m less than j,
c    find number of terms
c
      do j=1,9
        k = i(j)/2
        if(i(j) .ne. 2*k) return
        if(k .lt. 0) return
        if(k .lt. n) then
          n = k
        endif
        i(j) = k + 1
      end do
c
      if(m3 .ne. 0 .or. m1 .ne. 0 .or. m1 .ne. 1) then
        il = 0
        la = i1 - i5
        lb = i1 - i6
        if(il .lt. la) then
          il = la
        endif
        if(il .lt. lb) then
          il = lb
        endif
c
c  Form coefficients of sum
c
        c  = (g(i11) - g(i11-1) + g(i1) + g(i2) + g(i3) - g(i10) +
     *        g(i4) + g(i5) + g(i6) + g(i7) + g(i8) + g(i9))/two
        j1 = i1 - il
        j2 = i4 - il
        j3 = i7 - il
        m1 = il + 1
        m2 = il - la + 1
        m3 = il - lb + 1
        c  = c - g(j1) - g(j2) - g(j3) - g(m1) - g(m2) - g(m3)
        c  = DEXP(c)
        if((il - 2*(il/2)) .ne. 0) c = -c
        if(n .lt. 0) return
        if(n .eq. 0) then
          clebg = c
          return
        else
c
c  Form sum
c
          a = j1 - 1
          b = j2 - 1
          h = j3 - 1
          d = m1
          e = m2
          f = m3
          s = one
          q = n - 1
          do j=1,n
            t = (a-q)/(d+q)*(b-q)/(e+q)*(h-q)/(f+q)
            s = one - s*t
            q = q - one
          end do
          clebg = c*s
          return
        endif
      else
c
c  Special formula for m3 = 0 and m1 = 0 or 1/2
c
        k = i10/2
        if(i10 .eq. 2*k) then
          k = 0
        else
          k = 1
        endif
c
        if(m1 .eq. 0) then
          l = 0
          if(k .ne. 0) return
        else if(m1 .eq. 1) then
          l = 1
        endif
c
        x  = l
        m  = i3 + (i1 + k + 1)/2 - l
        m1 = i10/2 + k
        m2 = i4 + i5
        m3 = i6 + i7
        j1 = (i1 + 1 - k    )/2
        j2 = (i2 + 1 + k - l)/2
        j3 = (i3 + 1 + k - l)/2
        clebg = DEXP((g(i11)-g(i11-1)+g(i1)+g(i2)+g(i3)-g(i10))/two +
     *          g(m1)-g(j1)-g(j2)-g(j3)+
     *          x*(g(3)-(g(m2)-g(m2-1)+g(m3)-g(m3-1))/two))
        if((m - 2*(m/2)) .ne. 0) clebg = -clebg
        return
      endif
      return
      end
