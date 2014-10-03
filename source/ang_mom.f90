    module angular_momentum

    implicit none

    private

    integer*4, parameter :: mfac = 400     ! # of stored log-factorials
    integer*4, parameter :: nfac = 160     ! # of stored factorials

    real*8, parameter :: zero = 0.D0
    real*8, parameter :: one  = 1.D0
    real*8, parameter :: two  = 2.D0
    real*8, parameter :: pi = 3.14159265358979d0
    real*8, parameter :: pi4 = 4.D0*pi

    real*8 log_fact(-2:mfac), lin_fact(-2:nfac)

    public init_factorial,racah,clebg,ZCoefficient,ZBarCoefficient,Blatt
    public parity,paritx,lfct,fact

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
        i(j) = k
    end do

    if(n < 0) return

    ! Find minimum value of summation index

    il = 0
    do j = 13,16
        i(j) = i(j)/2
        il = max(il,i(j))
    end do

    j1 = il - i(13)
    j2 = il - i(14)
    j3 = il - i(15)
    j4 = il - i(16)
    j5 = i(13) + i(4) - il
    j6 = i(15) + i(5) - il
    j7 = i(16) + i(6) - il

    h = lfct(i(1)) + lfct(i(2)) + lfct(i(3)) - lfct(i(13)+1) + lfct(i(4)) + lfct(i(5)) + lfct(i(6)) - lfct(i(14)+1)
    h = h + lfct(i(7)) + lfct(i(8)) + lfct(i(9)) - lfct(i(15)+1) + lfct(i(10)) + lfct(i(11)) + lfct(i(12)) - lfct(i(16)+1)
    h = h/two + lfct(il+1) - lfct(j1) - lfct(j2) - lfct(j3) - lfct(j4) - lfct(j5) - lfct(j6) - lfct(j7)
    h = paritx(j5)*dexp(h)

    if(n == 0) then
        racah = h
    else
        s = one
        q = dble(n  - 1)
        p = dble(il + 2)
        r = dble(j1 + 1)
        o = dble(j2 + 1)
        v = dble(j3 + 1)
        w = dble(j4 + 1)
        x = dble(j5)
        y = dble(j6)
        z = dble(j7)
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
    i(11) = j3 + 1

    ! Check i(j) = even, triangular inequality, m less than j, find number of terms

    n = i(10)
    do j=1,9
        k = i(j)/2
        if(i(j) /= 2*k) return
        if(k < 0)       return
        n = min(n,k)
        i(j) = k
    end do

    if((m3 /= 0) .or. ((m1 /= 0) .and. (m1 /= 1))) then

        la = i(1) - i(5)
        lb = i(1) - i(6)
        il = max(0,la,lb)

        ! Form coefficients of sum

        j1 = i(1) - il
        j2 = i(4) - il
        j3 = i(7) - il
        m1 = il
        m2 = il - la
        m3 = il - lb

        c  = lfct(i(11)) - lfct(i(11)-1) + lfct(i(1)) + lfct(i(2)) + lfct(i(3)) - lfct(i(10)-1)
        c = c + lfct(i(4)) + lfct(i(5)) + lfct(i(6)) + lfct(i(7)) + lfct(i(8)) + lfct(i(9))
        c = c/two - lfct(j1) - lfct(j2) - lfct(j3) - lfct(m1) - lfct(m2) - lfct(m3)
        c  = paritx(il)*dexp(c)

        if(n < 0) return

        if(n == 0) then
            clebg = c
        else
            ! Form sum
            a = dble(j1)
            b = dble(j2)
            h = dble(j3)
            d = dble(m1 + 1)
            e = dble(m2 + 1)
            f = dble(m3 + 1)
            s = one
            q = dble(n - 1)
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
        else
            l = 1
        endif

        x  = dble(l)
        m  = i(3) + (i(1) + k)/2 - l
        m1 = i(10)/2 + k - 1
        m2 = i(4) + i(5) + 1
        m3 = i(6) + i(7) + 1
        j1 = (i(1) - k    )/2
        j2 = (i(2) + k - l)/2
        j3 = (i(3) + k - l)/2
        c = lfct(i(11)) - lfct(i(11)-1) + lfct(i(1)) + lfct(i(2)) + lfct(i(3)) - lfct(i(10)-1)
        c = c/two + lfct(m1) - lfct(j1) - lfct(j2) - lfct(j3) + x*(lfct(2) - (lfct(m2) - lfct(m2-1) + lfct(m3) - lfct(m3-1))/two)
        clebg = paritx(m)*dexp(c)

    endif

    return
    end function CLEBG

    !---------------------------------------------------------------------------------

    subroutine init_factorial

    implicit none

    integer*4 k

    ! allow for slightly negative values (down to -2)
    ! for compatibility with pcross.f

    !  Calculate factorial logarithms from 0! ( =1.) up to (mfac)!
    !  log_fact(k) = ln(k!)

    log_fact(-2) = zero
    log_fact(-1) = zero
    log_fact(0)  = zero
    log_fact(1)  = zero
    do k = 2,mfac
        log_fact(k) = log_fact(k-1) + dlog(dble(k))
    end do

    !  Calculate factorial from 0! ( =1.) up to (nfac)!
    !  lin_fact(k) = k!

    lin_fact(-2) = one
    lin_fact(-1) = one
    lin_fact(0)  = one
    lin_fact(1)  = one
    do k = 2,nfac
        lin_fact(k) = dble(k)*lin_fact(k-1)
    end do

    return
    end subroutine init_factorial

    !---------------------------------------------------------------------------------

    real*8 function lfct(i)

    implicit none

    integer*4, intent(in) :: i

    logical*4, save :: warn = .true.
    real*8 x

    ! return log factorial of i = ln(i!)

    if(i < -2) then
        write(8,'(a,i4)') ' ERROR: LOG-FACTORIAL function LFCT for negative value = ',i
        STOP ' ERROR: LOG-FACTORIAL OF NEGATIVE VALUE'
    else if(i <= mfac) then
        ! use saved value
        lfct = log_fact(i)
    else
        ! use Stirling's formula
        x = dble(i)
        lfct = x*log(x) - x + log(2.D0*pi*x)/two + one/(12.D0*x) - one/(360.D0*x*x*x)
        if(warn) then
            write(8,'(a,i4)') ' WARNING: LOG-FACTORIAL function LFCT overflowed stored value = ',i
            write(8,'(a)') ' WARNING: Increase parameter MFAC in ANGULAR_DISTRIBUTIONS module.'
            warn = .false.
        endif
    endif

    return
    end function lfct

    !---------------------------------------------------------------------------------

    real*8 function fact(i)

    implicit none

    ! return the factorial of i = i!

    integer*4, intent(in) :: i

    if(i < -2) then
        write(8,'(a,i4)') ' ERROR: FACTORIAL function FACT for negative value = ',i
        STOP ' ERROR: FACTORIAL OF NEGATIVE VALUE'
    else if(i <= nfac) then
        fact = lin_fact(i)
        return
    else
        write(8,'(a,i4)') ' ERROR: FACTORIAL function FACT overflow for  = ',i
        STOP ' ERROR: FACTORIAL OVERFLOW'
    endif

    end function fact

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
    real*8 zb1, zb2, rc1, rc2

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

    Blatt = paritx(m)*(two*j + one)*zb1*zb2*rc1*rc2/pi4

    return
    end function Blatt

    !---------------------------------------------------------------------------------

    integer*4 function parity(l)

    implicit none

    ! return the int*4 parity (+1 or -1) of ang mom l

    integer*4, intent(in) :: l     ! ang mom

    if(btest(l,0)) then
        parity = -1
    else
        parity = 1
    endif

    return
    end function parity

    !---------------------------------------------------------------------------------

    real*8 function paritx(l)

    implicit none

    ! return the real*8 parity (+1 or -1) of ang mom l

    integer*4, intent(in) :: l     ! ang mom

    if(btest(l,0)) then
        paritx = -1.D0
    else
        paritx = 1.D0
    endif

    return
    end function paritx

    end module angular_momentum
