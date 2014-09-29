    module angular_momentum

    private

    integer*4, parameter :: mfac = 500     ! # of stored factorials

    real*8, parameter :: zero = 0.D0
    real*8, parameter :: one  = 1.D0
    real*8, parameter :: two  = 2.D0
    real*8, parameter :: pi = 3.14159265358979d0
    real*8, parameter :: pi4 = 4.D0*pi

    real*8 fact(mfac)

    public init_factorial,racah,clebg,ZCoefficient,ZBarCoefficient,Blatt,parity

    contains

    !---------------------------------------------------------------------------------

    real*8 function RACAH(a,b,c,d,e,f)

    !***********************************************************************
    !  Calculate Racah coefficients      w(a,b,c,d;e,f)                    *
    !                                                                      *
    !  Attention:   w(a,b,c,d;e,f) = (-1)**(a+b+c+d)*6-J(a,b,e;d,c,f)      *
    !                                                ---                   *
    !                                                                      *
    !  from John.G.Wills     ORNL-TM-1949 (August 1967)                    *
    !                    and Comp.Phys.Comm. 2(1971)381                    *
    !                                                                      *
    !  O.Bersillon     August 1977                                         *
    !                                                                      *
    !***********************************************************************

    implicit none

    real*8, intent(in) :: a,b,c,d,e,f

    integer*4 il,j,k,n,i(16)
    integer*4 ja,jb,jc,jd,je,jf,j1,j2,j3,j4,j5,j6,j7
    real*8 h,o,p,q,r,s,t,v,w,x,y,z

    racah = zero

    ! Convert arguments to integer and make useful combinations

    ja  = nint(two*a)
    jb  = nint(two*b)
    jc  = nint(two*c)
    jd  = nint(two*d)
    je  = nint(two*e)
    jf  = nint(two*f)

    i(1)  = ja + jb - je
    i(2)  = jb + je - ja
    i(3)  = je + ja - jb
    i(4)  = jc + jd - je
    i(5)  = jd + je - jc
    i(6)  = je + jc - jd
    i(7)  = ja + jc - jf
    i(8)  = jc + jf - ja
    i(9)  = jf + ja - jc
    i(10) = jb + jd - jf
    i(11) = jd + jf - jb
    i(12) = jf + jb - jd
    i(13) = ja + jb + je
    i(14) = jc + jd + je
    i(15) = ja + jc + jf
    i(16) = jb + jd + jf

    ! Check triangular inequalities, find no. of terms in sum, divide I's by 2

    n = i(16)
    do j = 1,12
        k = i(j)/2
        if(i(j) /= 2*k) return
        if(k < 0)       return
        n = min(n,k)
        i(j) = k + 1
    end do

    ! Find minimum value of summation index

    il = 0
    do j = 13,16
        i(j) = i(j)/2
        il = max(il,i(j))
    end do

    j1 = il - i(13) + 1
    j2 = il - i(14) + 1
    j3 = il - i(15) + 1
    j4 = il - i(16) + 1
    j5 = i(13) + i(4) - il
    j6 = i(15) + i(5) - il
    j7 = i(16) + i(6) - il

    h = fct(i(1)) + fct(i(2)) + fct(i(3)) - fct(i(13)+2) + fct(i(4)) + fct(i(5)) + fct(i(6)) - fct(i(14)+2)
    h = h + fct(i(7)) + fct(i(8)) + fct(i(9)) - fct(i(15)+2) + fct(i(10)) + fct(i(11)) + fct(i(12)) - fct(i(16)+2)
    h = h/two + fct(il+2) - fct(j1) - fct(j2) - fct(j3) - fct(j4) - fct(j5) - fct(j6) - fct(j7)
    h = -dexp(h)

    if(mod(j5,2) /= 0) h = -h
    if(n < 0) return

    if(n == 0) then
        racah = h
    else
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
        do j = 1,n
          t = (p+q)/(r+q)*(x-q)/(o+q)*(y-q)/(v+q)*(z-q)/(w+q)
          s = one - s*t
          q = q - one
        end do
        racah = h*s
    endif

    return
    end function RACAH

    !---------------------------------------------------------------------------------

    real*8 function CLEBG(aj1,aj2,aj3,am1,am2,am3)

    !***********************************************************************
    !  Calculate Clebsch-Gordan coefficients                               *
    !                                                                      *
    !  Attention:  cg(j1,j2,j3;m1,m2,m3) = (-1)**(j1+j2-m3)*               *
    !                                       3-J(j1,j2,j3;m1,m2,-m3)        *
    !                                       ---                            *
    !                                                                      *
    !  from    John.G. Wills     ORNL-TM-1949 (August 1967)                *
    !                         et Comp.Phys.Comm. 2(1971)381                *
    !                                                                      *
    !  O.Bersillon     August 1977                                         *
    !                                                                      *
    !***********************************************************************

    implicit none


    real*8, intent(in) :: aj1,aj2,aj3,am1,am2,am3

    integer*4 i(11),il,j,j1,j2,j3,k,l,la,lb,m,m1,m2,m3,n
    real*8 a,b,c,d,e,f,h,q,s,t,x

    clebg = zero

    ! Convert the arguments to integer

    j1 = nint(two*aj1)
    j2 = nint(two*aj2)
    j3 = nint(two*aj3)
    m1 = nint(two*am1)
    m2 = nint(two*am2)
    m3 = nint(two*am3)

    ! Test m1 + m2 = m3

    if(m1 + m2 - m3 /= 0) return

    ! Test table size

    i(1) = j1 + j2 - j3
    i(2) = j2 + j3 - j1
    i(3) = j3 + j1 - j2
    i(4) = j1 - m1
    i(5) = j1 + m1
    i(6) = j2 - m2
    i(7) = j2 + m2
    i(8) = j3 - m3
    i(9) = j3 + m3
    i(10) = (j1 + j2 + j3)/2 + 2
    i(11) = j3 + 2

    ! Check i(j) = even, triangular inequality, m less than j, find number of terms

    n = i(10)
    do j=1,9
        k = i(j)/2
        if(i(j) /= 2*k) return
        if(k < 0) return
        n = min(n,k)
        i(j) = k + 1
    end do

    if((m3 /= 0) .or. (m1 /= 0) .or. (m1 /= 1)) then

        la = i(1) - i(5)
        lb = i(1) - i(6)
        il = max(0,la,lb)

        ! Form coefficients of sum

        j1 = i(1) - il
        j2 = i(4) - il
        j3 = i(7) - il
        m1 = il + 1
        m2 = il - la + 1
        m3 = il - lb + 1

        c  = fct(i(11)) - fct(i(11)-1) + fct(i(1)) + fct(i(2)) + fct(i(3)) - fct(i(10))
        c = c + fct(i(4)) + fct(i(5)) + fct(i(6)) + fct(i(7)) + fct(i(8)) + fct(i(9))
        c = c/two
        c  = c - fct(j1) - fct(j2) - fct(j3) - fct(m1) - fct(m2) - fct(m3)
        c  = dexp(c)
        if((il - 2*(il/2)) /= 0) c = -c

        if(n < 0) return

        if(n == 0) then
            clebg = c
        else

            ! Form sum

            a = j1 - 1
            b = j2 - 1
            h = j3 - 1
            d = m1
            e = m2
            f = m3
            s = one
            q = n - 1
            do j = 1,n
                t = (a-q)/(d+q)*(b-q)/(e+q)*(h-q)/(f+q)
                s = one - s*t
                q = q - one
            end do
            clebg = c*s
        endif

    else

        ! Special formula for m3 = 0 and m1 = 0 or 1/2

        k = mod(i(10),2)

        if(m1 == 0) then
            l = 0
            if(k /= 0) return
        else if(m1 == 1) then
            l = 1
        endif

        x  = l
        m  = i(3) + (i(1) + k + 1)/2 - l
        m1 = i(10)/2 + k
        m2 = i(4) + i(5)
        m3 = i(6) + i(7)
        j1 = (i(1) + 1 - k    )/2
        j2 = (i(2) + 1 + k - l)/2
        j3 = (i(3) + 1 + k - l)/2
        c = fct(i(11)) - fct(i(11)-1) + fct(i(1)) + fct(i(2)) + fct(i(3)) - fct(i(10))
        c = c/two
        c = c + fct(m1) - fct(j1) - fct(j2) - fct(j3) + x*(fct(3) - (fct(m2) - fct(m2-1) + fct(m3) - fct(m3-1))/two)
        c = dexp(c)
        if((mod(m,2)) /= 0) c = -c
        clebg = c

    endif

    return
    end function CLEBG

    !---------------------------------------------------------------------------------

    subroutine init_factorial

    !  Calculate factorial logarithms from 0! ( =1.) up to (mfac)!
    !                 log(k!) = fact(k+1)

    implicit none

    integer*4 k

    fact(1) = zero
    fact(2) = zero

    do k = 3,mfac
        fact(k) = fact(k-1) + dlog(dble(k-1))
    end do

    return
    end subroutine init_factorial

    !---------------------------------------------------------------------------------

    real*8 function fct(i)

    implicit none

    integer*4, intent(in) :: i

    ! retreive log factorial function after checking bound

    if(i <= mfac) then
        fct = fact(i)
        return
    endif

    write(8,'(a,i0)') ' ERROR: LOG-FACTORIAL function FCT overflow for value = ',i
    write(8,'(a)') ' Increase parameter MFAC in ANGULAR_DISTRIBUTIONS module.'
    stop ' ERROR: MEMORY OVERFLOW IN FCT, INCREASE MFAC in ANGULAR_DISTRIBUTIONS'

    end function fct

    !---------------------------------------------------------------------------------

    real*8 function ZCoefficient( l1, j1, l2, j2, s, ll )

    ! Blatt & Biedenharn's definition in Rev. Mod. Phys. 24, 258 (1952)

    implicit none

    real*8, intent(in) :: l1, j1, l2, j2, s, ll

    real*8 z

    z = ZBarCoefficient( l1, j1, l2, j2, s, ll )
    if(mod(nint(ll-l1+l2), 4) /= 0) z = -z

    ZCoefficient = z

    return
    end function ZCoefficient

    !---------------------------------------------------------------------------------

    real*8 function ZBarCoefficient( l1, j1, l2, j2, s, ll )

    ! Lane & Thomas's Zbar-coefficient coefficient
    !   = Zbar(l1  j1  l2  j2 | S L )
    !   = (-i)^( -l1 + l2 + ll ) * Z(l1  j1  l2  j2 | S L )
    !
    ! Lane & Thomas Rev. Mod. Phys. 30, 257-353 (1958).
    ! Note, Lane & Thomas define this because they did not like the different phase convention in
    ! Blatt & Biedenharn's Z coefficient.  They changed it to get better time-reversal behavior.
    ! Froehner uses Lane & Thomas convention as does T. Kawano.

    implicit none

    real*8, intent(in) :: l1, j1, l2, j2, s, ll

    real*8 :: rc, cc

    ZBarCoefficient = zero

    cc = CLEBG( l1, l2, ll, zero, zero, zero )
    if(cc==zero) return
    rc = RACAH( l1, j1, l2, j2, s, ll )
    if(rc==zero) return

    ZBarCoefficient = dsqrt((two*l1 + one) * (two*l2 + one) * (two*j1 + one) * (two*j2 + one))*cc*rc

    return
    end function ZBarCoefficient

    !---------------------------------------------------------------------------------

    real*8 function Blatt(J,Ia,la,ja,sa,Ib,lb,jb,sb,L)

    ! Blatt-Biedenharn formula for Compound Nuclei (as written by Froebrich & Lipperheide
    ! "Theory of Nuclear Reactions", Oxford Science (1996) p. 328), but substantially
    ! revised because of the different angular momentum coupling scheme in EMPIRE

    implicit none

    real*8, intent(in) :: J           ! CN spin
    real*8, intent(in) :: Ia          ! target spin
    real*8, intent(in) :: la          ! projectile l
    real*8, intent(in) :: ja          ! total projectile angular momentum l+s
    real*8, intent(in) :: sa          ! projectile spin
    real*8, intent(in) :: Ib          ! residual nucleus spin
    real*8, intent(in) :: lb          ! ejectile l
    real*8, intent(in) :: jb          ! total ejectile angular momentum l+s
    real*8, intent(in) :: sb          ! ejectile spin
    real*8, intent(in) :: L           ! Legendre polynomial order (P_L)

    integer*4 m
    real*8 zb1, zb2, rc1, rc2, x

    Blatt = zero

    rc1 = RACAH( ja, J, ja, J, Ia, L )
    if(rc1==zero) return

    rc2 = RACAH( jb, J, jb, J, Ib, L )
    if(rc2==zero) return

    zb1 = ZBarCoefficient( la, ja, la, ja, sa, L )
    if(zb1==zero) return

    zb2 = ZBarCoefficient( lb, jb, lb, jb, sb, L )
    if(zb2==zero) return

    m = nint(Ib -Ia + sb - sa + two*(ja + jb))
    x = dble(parity(m))

    Blatt = x*(two*j + one)*zb1*zb2*rc1*rc2/pi4

    return
    end function Blatt

    !---------------------------------------------------------------------------------

    integer*4 function parity(l)

    implicit none

    ! return the parity (+1 or -1) of ang mom l

    integer*4, intent(in) :: l     ! ang mom

    parity = 1 - 2*abs(mod(l,2))

    return
    end function parity

    end module angular_momentum
