module ph_lvl_dens

    use angular_momentum

    implicit none

    real*8 cnex      ! excitation energy of compound nucleus [MeV]
    real*8 sp_den    ! single particle state density [1/MeV]
    real*8 bind_e    ! neutron binding energy for conditional state densities [MeV]
    real*8 signx     ! width [MeV]

    private w1, w2, w3

contains

    real*8 function WT(In, Ip, Ih, X)

        implicit none

        ! calculates conditional p-h state densities according
        ! to Nucl. Phys. A430(1984)69 (including all necessary factors)

        integer*4, intent(in) :: In   !
        integer*4, intent(in) :: IP   ! particle number
        integer*4, intent(in) :: IH   ! hole number
        real*8, intent(in) :: X    ! excitation energy

        WT = W(Ip, Ih, X)/fact(Ip)/fact(Ih)*sp_den**In

        return
    end function WT

    real*8 function W(Ip, Ih, X)

        implicit none

        ! calculates conditional p-h state densities according
        ! to Nucl. Phys. A430(1984)69 without g**n/p!/h!

        integer*4, intent(in) :: IP   ! particle number
        integer*4, intent(in) :: IH   ! hole number
        real*8, intent(in) :: X    ! excitation energy

        integer*4 ix
        real*8 pix

        W = 0.D0
        IF (X <= 0.0D0) RETURN
        IF ((Ip == 0) .AND. (Ih == 0)) RETURN

        ix = INT(X/bind_e)
        pix = ABS(dble(ix)*bind_e - X)
        IF (pix < 1.D-8) ix = ix - 1

        if (Ip == 0) then
            W = W2(0, Ih, 0, X)
        else if (Ih == 0) then
            if (ix < Ip) W = W2(Ip, 0, ix, X)
        else if (ix < Ip) then
            ! W2 is eq.7a of reference above without g**(p+h)/p!/h! factor
            W = W2(Ip, Ih, ix, X)
        else if (Ip >= Ih) THEN
            ! W1 is eq.7b of reference above without g**(p+h)/p!/h! factor
            W = W1(Ip, Ih, Ih - 1, X)
        else
            W = W2(Ip, Ih, Ip - 1, X) + W3(Ip, Ih, Ip - 1, X)
        end if

        return
    end function W


    real*8 function W1(J, L, K, X)

        implicit none

        integer*4, intent(in) :: j, k, l
        real*8, intent(in) :: x

        integer*4 i, m
        real*8 a1, a2, s, sum0, sum1, x1, z1

        ! seems to be 7b

        IF (K >= L) THEN
            WRITE (50, 10) J, L, K, X
            10 format(1X, 'ERROR:', 5X, 'W1(', I1, ',', I1, ',', I1, ',', E12.5, ')')
            STOP 'ERROR: W1'
        end if

        x1 = X - dble(J)*bind_e
        a1 = x1**(L - 1)
        s = -1.D0
        sum0 = 0.D0
        DO i = 0, J - 1
            s = -s
            sum1 = 0.D0
            a2 = dble(J - i)*bind_e
            z1 = x1/a2
            DO m = 0, k
                z1 = z1*a2/x1
                sum1 = sum1 + z1/fact(J + m)/fact(L - m - 1)
            END DO
            a2 = a2**J
            sum0 = sum0 + a2/fact(i)/fact(j - i)*sum1*s
        END DO

        W1 = sum0*a1*fact(j)

        return
    end function W1


    real*8 function W2(J, L, K, X)

        implicit none

        integer*4, intent(in) :: j, k, l
        real*8, intent(in) :: x

        integer*4 i, jl
        real*8 s, sum0, xx

        ! looks like 7a

        W2 = 0.D0

        IF ((K == 0) .AND. (X == 0.D0)) return
        IF ((J < 0) .OR. (K > 15)) return

        jl = J + L - 1
        s = -1.D0

        sum0 = 0.D0
        DO i = 0, k
            s = -s
            xx = (X - dble(i)*bind_e)**jl
            sum0 = sum0 + s/fact(i)/fact(j - i)*xx
        END DO

        W2 = sum0/fact(jl)*fact(j)

        return
    end function W2


    real*8 function W3(J, L, K, X)

        implicit none

        integer*4, intent(in) :: j, k, l
        real*8, intent(in) :: x

        integer*4 i, j1, m
        real*8 a1, a2, s, sum0, sum1, x1, z1

        ! seems to be second part of 7c

        j1 = J - 1
        IF (K > j1) THEN
            WRITE (50, 10) J, L, K, X
            10 format(1X, 'ERROR:', 5X, 'W3(', I1, ',', I1, ',', I1, ',', E12.5, ')')
            STOP 'ERROR: W3'
        end if

        x1 = X - dble(J)*bind_e
        a1 = x1**L
        s = 1.D0
        sum0 = 0.D0
        DO i = 0, j1
            s = -s
            sum1 = 0.D0
            a2 = dble(J - i)*bind_e
            z1 = a2/x1
            DO m = 0, k
                z1 = z1*x1/a2
                sum1 = sum1 + z1/fact(L + m)/fact(J1 - m)
            END DO
            a2 = a2**j1
            sum0 = sum0 + a2/fact(i)/fact(J - i)*s*sum1
        END DO

        W3 = sum0*a1*fact(j)

        return
    end function W3


    real*8 function GDOWN(Ip, Ih, X)

        implicit none

        ! calculates gamma down for multistep compound according to
        ! Nucl. Phys. A435(1985)67
        ! the actual version accounts for the factor 1/2 as pointed
        ! out by Oblozinsky (Nucl. Phys. A453(1986)127)

        integer*4, intent(in) :: IP   ! particle number
        integer*4, intent(in) :: IH   ! hole number
        real*8, intent(in) :: X    ! excitation energy

        integer*4 ip1
        real*8 e1b, ro1, roph, xh2, xp

        e1b = X - bind_e
        xh2 = dble(Ih)
        xp = dble(Ip)
        ip1 = Ip - 1
        roph = W(Ip, Ih + 2, X)*xh2 + W(ip1, Ih + 3, X)*xp

        IF (e1b > 0.D0) THEN
            roph = roph - W(Ip, Ih + 2, e1b)*xh2
            ro1 = W(ip1, Ih + 3, e1b) + bind_e*(W(ip1, Ih + 1, e1b)*bind_e/2.D0 + W(ip1, Ih + 2, e1b))
            roph = roph - ro1*xp
        END IF

        gdown = roph/2.D0

        return
    end function GDOWN


    real*8 function ZERO(Ip, Ih, X)

        implicit none

        integer*4, intent(in) :: Ip   ! particle number
        integer*4, intent(in) :: Ih   ! hole number
        real*8, intent(in) :: X    ! excitation energy

        real*8 a, a1, e2b

        e2b = cnex - 2.D0*bind_e
        a = W(Ip - 1, Ih, X)*bind_e*dble(Ih)

        a1 = 0.D0
        IF (e2b < X) THEN
            a1 = a1 - W(Ip - 2, Ih + 2, X)
            a1 = a1 + W(Ip - 2, Ih + 1, X)*(X - e2b)
            a1 = a1 + W(Ip - 2, Ih + 2, e2b)
        END IF
        a1 = a1*dble(Ip - 1)/2.D0

        zero = (a + a1)*dble(Ip)

        return
    end function ZERO


    real*8 function MINUS(Ip, Ih, X)

        implicit none

        integer*4, intent(in) :: Ip   ! particle number
        integer*4, intent(in) :: Ih   ! hole number
        real*8, intent(in) :: X    ! excitation energy

        minus = W(2, 1, cnex - X)*W(Ip - 2, Ih - 1, X)*dble(Ip*(Ip - 1)*Ih)/2.D0

        return
    end function MINUS


    real*8 function WOBL(Ip, Ih, U, Ni)

        implicit none

        ! calculates conditional state densities according to Oblozinsky
        ! Nucl. Phys. A453(1986)127 formula 13; without factor
        ! g**(p+h)/p!h!(n-1)! and neglecting well depth

        integer*4, intent(in) :: Ip   ! particle number
        integer*4, intent(in) :: Ih   ! hole number
        integer*4, intent(in) :: Ni   !
        real*8, intent(in) :: U    !

        integer*4 i, ipm, n
        real*8 s, w

        WOBL = 0.D0
        IF (U <= 0.0D0) RETURN

        n = Ip + Ih + Ni
        IF ((Ip < 0) .or. (Ih < 0) .or. (n == 0)) THEN
            WRITE (8, 10) Ip, Ih, n
            10 format(1X, /1X, ' ERROR IN WOBL CALL P=', I2, '  H=', I2, '  N=', I2, '    WOBL=0.0 RETURNED',/)
            return
        end if

        ipm = AINT(U/bind_e)
        ipm = MIN(Ip, ipm)

        s = -1.D0
        DO i = 0, ipm
            s = -s
            w = (U - dble(i)*bind_e)**n
            w = w*s/fact(i)/fact(Ip - i)
            WOBL = WOBL + w
        END DO

        WOBL = WOBL*fact(Ip)

        return
    end function WOBL


    real*8 function BACK(Yb, Ip, Ih)

        implicit none

        ! calculates density of accessible states (conditional) for internal
        ! backward transitions  (without g/w(p,h,e,-1) factor)
        ! using Oblozinsky's formula for cond. st. den.

        integer*4, intent(in) :: Ip   ! particle number
        integer*4, intent(in) :: Ih   ! hole number

        integer*4 n1, n2
        real*8 ebe, yb

        yb = 0.D0
        n1 = Ip + Ih - 1
        n2 = n1 - 1
        IF (n2 <= 1) RETURN
        IF (cnex <= 0.0D0) RETURN

        yb = WOBL(Ip - 2, Ih - 1, cnex, 2) + WOBL(Ip - 1, Ih - 2, cnex, 2)*dble(Ih - 1)/dble(Ip - 1)
        ebe = cnex - bind_e
        IF (ebe > 0.0D0) yb = yb - WOBL(Ip - 2, Ih - 1, ebe, 2) &
                              - bind_e*dble(n1)*WOBL(Ip - 2, Ih - 1, ebe, 1) &
                              - bind_e*bind_e*dble(n1*n2)*WOBL(Ip - 2, Ih - 1, ebe, 0)/2.D0

        back = dble(Ip*(Ip - 1)*Ih)*yb/2.D0

        return
    end function BACK


    real*8 FUNCTION VQ(Ip, Ih, U)

        implicit none

        ! calculates avrage of imaginary part of o.m. pot. given as W=C*E**2
        ! exciton distribution function OM(P-1,H,E-EP)/OM(P,H,E) is used as
        ! weighting funtion in the case of particles (analogous for holes)

        integer*4, intent(in) :: Ip   ! particle number
        integer*4, intent(in) :: Ih   ! hole number
        real*8, intent(in) :: U    !

        real*8, parameter :: c = 3.0D-3

        integer*4 ih1, ip1, n
        real*8 ub, x, w1, w2, w3, w4, w5, w6, w7, w8

        VQ = 0.D0

        IF ((Ip < 0) .or. (Ih < 0) .or. ((Ip == 0) .and. (Ih == 0))) THEN
            WRITE (8, 10) Ip, Ih, U
            10 format(1X, 'ERROR IN VQ INPUT: P=', I2, ' H=', I2, ' E=', F8.4, '  VQ=0 RETURNED')
            return
        end if

        IF (U <= 0.0D0) RETURN

        n = Ip + Ih
        x = dble(n)
        ip1 = Ip - 1
        ih1 = Ih - 1
        ub = U - bind_e
        w1 = WOBL(ip1, Ih, U, 0)
        w2 = WOBL(ip1, Ih, ub, 0)
        w3 = WOBL(ip1, Ih, U, 2)
        w4 = WOBL(ip1, Ih, ub, 2)
        w5 = WOBL(ip1, Ih, ub, 1)
        w6 = WOBL(ip1, Ih, ub, 0)
        w7 = WOBL(Ip, ih1, U, 2)
        w8 = WOBL(Ip, ih1, U, 0)

        ! particle part

        VQ = 2.D0/x/(x + 1.D0)*(w3 - w4) - 2.D0/x*w5*bind_e - w6*bind_e*bind_e
        VQ = VQ*dble(Ip)*c/x/(w1 - w2)

        ! hole part

        VQ = VQ + 2.D0*c*dble(Ih)*w7/(x*x*(x + 1.D0)*w8)

        return
    end FUNCTION VQ


    subroutine TRATES

        WRITE (8, *) 'ERROR: TRATES NOT IMPLEMENTED'

        return
    end subroutine TRATES

     

    real*8 function ROPHM(N, I, E, G)

        implicit none

        integer*4, intent(in) :: i, n
        real*8, intent(in) :: e, g

        WRITE (8, *) 'ERROR: MICROSCOPIC PARTIAL LEVEL DENSITIES NOT IMPLEMENTED'

        ROPHM = N + I + E + G    ! dummy line to avoid compiler warning
        ROPHM = 0.0

        return
    end function ROPHM


    real*8 function WILLI(N, X)

        implicit none

        ! calculates p-h state densities according to williams formula
        ! (without g**n/p!h! factor which is contained in omj)

        integer*4, intent(in) :: n
        real*8, intent(in) :: x

        WILLI = 0.D0
        IF ((X <= 0.D0) .OR. (N <= 0)) RETURN
        WILLI = X**(N - 1)/fact(N - 1)

        return
    end function WILLI


    real*8 function OMJ(N, Ip, Ih, J, S, Ngs)

        implicit none

        ! calculates spin dependent factor in state density including
        ! 1/2 for parity and g**n/p!h! missing in w function
        ! the latter factor is set to 1 when microscopic densities are
        ! used (ngs=1)

        real*8, parameter :: pi = 3.1415926535897932D0 ! until we have base class

        integer*4, intent(in) :: N    !
        integer*4, intent(in) :: Ip   ! particle number
        integer*4, intent(in) :: Ih   ! hole number
        integer*4, intent(in) :: J    !
        integer*4, intent(in) :: Ngs  !
        real*8, intent(in) :: S    !

        real*8 sig, w, xj

        OMJ = 0.D0
        IF ((N <= 0) .OR. (Ip < 0) .OR. (Ih < 0)) RETURN

        sig = SIGnx*dble(N)
        xj = dble(J) + S
        w = (xj + 1.D0)*xj/2.D0/sig
        IF (w > 50.D0) RETURN
        OMJ = 1.D0
        IF (Ngs == 0) OMJ = sp_den**N/fact(Ip)/fact(Ih)
        OMJ = OMJ*(2*xj + 1.D0)*EXP(-w)/4.D0/dsqrt(2.D0*pi)/sig**1.5D0

        return
    end function OMJ


    real*8 function WOB1(X, Np, Nh, F)

        implicit none

        !**************************************************************************
        !*                                                              class:PPU *
        !*                            W O B 1                                     *
        !*                                                                        *
        !* Calculates the partial nuclear state density of a given excited        *
        !* particle-hole configuration by means of the Betak-Dobes formula        *
        !* [1] accounting for nuclear potential finite-depth, for one-            *
        !* component Fermi-gas. If the nucleon binding energy is specified        *
        !* the bound-state densities according to Oblozinsky [2] are calculated.  *
        !*                                                                        *
        !* [1] C  2. E. Betak and J. Dobes, Z. Phys. A279, 319 (1976)             *
        !* [2] P.Oblozinsky, Nucl.Phys. A453,127(1986), Eqs.(7,9)                 *
        !*                                                                        *
        !* Coded by M. AVRIGEANU, INPE-BUCHAREST, SEPTEMBER 1997.                 *
        !* Extracted from RIPL-1.                                                 *
        !*                                                                        *
        !* Output: WOB1 - p-h level density at X                                  *
        !*                                                                        *
        !**************************************************************************

        integer*4, intent(in) :: np    ! number of particles
        integer*4, intent(in) :: nh    ! number of holes
        real*8, intent(in) :: x     ! excitation energy [MeV]
        real*8, intent(in) :: f     ! nuclear potential depth

        integer*4 i, j, nn
        real*8 alpha, aph, ch, cp, ecor, ecor1, h, p, sum0, t1, t2

        WOB1 = 0.D0
        nn = Np + Nh - 1
        p = dble(Np)
        h = dble(Nh)
        t1 = (sp_den**(p + h))/fact(nn)
        t2 = fact(Np)*fact(Nh)
        alpha = (p*p + h*h)/(2.D0*sp_den)
        aph = (p*(p - 1.D0) + h*(h - 1.D0))/(4.D0*sp_den)
        IF ((X + alpha - p*bind_e - h*F) > 0.D0) RETURN

        sum0 = 0.D0
        DO i = 0, Np
            DO j = 0, Nh
                ecor = X - aph - dble(i)*bind_e - dble(j)*F
                ecor1 = X - alpha - dble(i)*bind_e - dble(j)*F
                IF (ecor1 > 0.D0) THEN
                    cp = fact(Np)/(fact(Np - i)*fact(i))
                    ch = fact(Nh)/(fact(Nh - j)*fact(j))
                    sum0 = sum0 + paritx(i + j)*cp*ch*ecor**nn/t2
                END IF
            END DO
        END DO

        WOB1 = t1*sum0

        return
    end function WOB1

end module ph_lvl_dens
