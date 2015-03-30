module empend_test

    use fruit
    use fruit_util

    implicit none
    
contains

    !-----------------------------------------------------------------------------
    !
    ! Helper routines
    !
    !-----------------------------------------------------------------------------

    !
    ! Test that two values are close
    !
    subroutine assert_close( a, b, reltol, abstol, testname )
        real, intent (in):: a, b, reltol, abstol
        character(500):: message, msg_prefix
        character(*), intent (in), optional:: testname
        msg_prefix=''
        if ( testname .ne. '' ) then
            msg_prefix = trim(testname) //':'
        end if
        message = trim( trim(msg_prefix)//' '//trim(to_s(a)) // ' not close to ' // trim(to_s(b)) )
        call assert_true( (abs(a - b) < abstol) .or. ( abs(a-b)/abs(a+b) < reltol ), message )
    end subroutine assert_close

    !
    ! Test that the values in two vectors are close
    !
    subroutine assert_all_close( a, b, n, reltol, abstol, testname )
        real, intent (in):: reltol, abstol
        real, dimension (*), intent (in)::a, b
        integer, intent (in):: n
        character(*), intent (in), optional:: testname
        integer:: i
        i=1
        do while (i.le.n)
            call assert_close( a(i), b(i), reltol, abstol, testname )
            i=i+1
        end do
    end subroutine assert_all_close



    !-----------------------------------------------------------------------------
    !
    ! unit tests
    !
    !-----------------------------------------------------------------------------
    
    !
    ! LSQLGV is a wrapper around LSQLEG to adaptively increases the order of the Legendre 
    ! fit until the best fit is achieved.
    !
    subroutine test_LSQLGV
        integer:: NP
        parameter(NP=91)
        real, dimension (NP):: XP
        real, dimension (NP):: AP; data AP/&
               0.0000,         2.0000,         4.0000,         6.0000,&
               8.0000,         10.000,         12.000,         14.000,&
               16.000,         18.000,         20.000,         22.000,&
               24.000,         26.000,         28.000,         30.000,&
               32.000,         34.000,         36.000,         38.000,&
               40.000,         42.000,         44.000,         46.000,&
               48.000,         50.000,         52.000,         54.000,&
               56.000,         58.000,         60.000,         62.000,&
               64.000,         66.000,         68.000,         70.000,&
               72.000,         74.000,         76.000,         78.000,&
               80.000,         82.000,         84.000,         86.000,&
               88.000,         90.000,         92.000,         94.000,&
               96.000,         98.000,         100.00,         102.00,&
               104.00,         106.00,         108.00,         110.00,&
               112.00,         114.00,         116.00,         118.00,&
               120.00,         122.00,         124.00,         126.00,&
               128.00,         130.00,         132.00,         134.00,&
               136.00,         138.00,         140.00,         142.00,&
               144.00,         146.00,         148.00,         150.00,&
               152.00,         154.00,         156.00,         158.00,&
               160.00,         162.00,         164.00,         166.00,&
               168.00,         170.00,         172.00,         174.00,&
               176.00,         178.00,         180.00/
        real, dimension (NP):: YP; data YP/&
             0.15091E+02,    0.15066E+02,    0.15063E+02,    0.18674E+02,&   
             0.18659E+02,    0.18613E+02,    0.18539E+02,    0.18438E+02,&
             0.18311E+02,    0.18164E+02,    0.17999E+02,    0.17819E+02,&   
             0.17630E+02,    0.17435E+02,    0.17239E+02,    0.17046E+02,&
             0.16859E+02,    0.16681E+02,    0.16516E+02,    0.16363E+02,&   
             0.16222E+02,    0.16089E+02,    0.15963E+02,    0.15845E+02,&
             0.15736E+02,    0.15641E+02,    0.15561E+02,    0.15497E+02,&   
             0.15447E+02,    0.15410E+02,    0.15384E+02,    0.15365E+02,&
             0.15352E+02,    0.15344E+02,    0.15341E+02,    0.15340E+02,&   
             0.15343E+02,    0.15347E+02,    0.15354E+02,    0.15364E+02,&
             0.15376E+02,    0.15397E+02,    0.15447E+02,    0.15437E+02,&   
             0.15434E+02,    0.15439E+02,    0.15447E+02,    0.15457E+02,&
             0.15468E+02,    0.15480E+02,    0.15493E+02,    0.15506E+02,&   
             0.15517E+02,    0.15528E+02,    0.15537E+02,    0.15544E+02,&
             0.15548E+02,    0.15548E+02,    0.15545E+02,    0.15539E+02,&   
             0.15529E+02,    0.15515E+02,    0.15498E+02,    0.15477E+02,&
             0.15452E+02,    0.15425E+02,    0.15406E+02,    0.15382E+02,&   
             0.15354E+02,    0.15313E+02,    0.15312E+02,    0.15304E+02,&
             0.15300E+02,    0.15303E+02,    0.15239E+02,    0.15184E+02,&   
             0.15152E+02,    0.15130E+02,    0.15115E+02,    0.15104E+02,&
             0.15098E+02,    0.15093E+02,    0.15091E+02,    0.15090E+02,&
             0.15091E+02,    0.15092E+02,    0.15094E+02,    0.15096E+02,&
             0.15098E+02,    0.15100E+02,    0.15102E+02/
        integer:: i=0
        do while (i.lt.NP)
            XP(i+1)=cos( AP(i+1)*3.14159265359/180.0 )
            i = i + 1
        end do
!        LSQLGV(XP,YP,NP,QQ,LMI,LMX,EMM,ERR,RWO,MXR)
    end subroutine test_LSQLGV
    
    
    !
    ! LSQLEG gets the best fit Legendre polynomial expansion at order NL to a set of 
    ! (angle, value) pairs.  The angles are given as mu=cos(theta).
    !
    ! Test against pairs of x, x^3+x^2.  The best fit Legendre series for NL = 5 better 
    ! be c_L=(1/3, 3/5, 2/3, 2/5, 0, 0), because that's what it is analytically.
    !
    subroutine test_LSQLEG
    
        ! stuff that gets passed into LSQLEG
        integer:: NP
        parameter(NP=11)
        real, dimension (NP):: XP
        real, dimension (NP):: YP
        integer:: N1
        parameter(N1=6)
        real, dimension (N1):: QQ
        real, dimension (N1,N1):: AA
        integer:: IER=0
        
        ! local variables
        integer:: i
        real, dimension (N1):: answer; data answer/0.3333333333, 0.6, 0.666666666667, 0.4, 0.0, 0.0/
        
        ! initialize data arrays
        do i=1,NP
            XP(i) = -1.0 + (i-1)*2.0/(NP-1)
            YP(i) = XP(i)*XP(i)*(XP(i)+1.0)
!            write(*,*) i,NP, XP(i), YP(i)
        end do

        ! invert
!        call LSQLEG(XP,YP,NP,QQ,N1,AA,IER)

        ! check results
        call assert_all_close( answer, QQ, N1, 1e-6, 1e-6, 'Fit to Legendre polynomial of order 5' )
    end subroutine test_LSQLEG
    
    
    !
    ! MTXGUP inverts a matrix A to solve for X in the equation A*X=F 
    !
    ! Test against: 
    !    A = [ 1.5   2.5   3.5   4.5 ]
    !        [ 2.5   4.5   6.5   5.5 ]
    !        [ 3.5   6.5   9.5  12.5 ]
    !        [ 4.5   5.5  12.5  16.5 ]
    !    answer = [ 1.5   3.5   2.3  15.8 ]
    !    routine below sets up test with data F = A*answer
    !
    subroutine test_MTXGUP
        integer:: N
        parameter(N=4)
        real, dimension(N):: answer, F, X
        real, dimension(N,N):: A 
        data answer/1.5,3.5,2.3,15.8/
        integer:: i,j, LDIG
        real:: DET

        ! Set up test input data 
        do i=1,N
            F(i)=0.0
            do j=1,N
                A(i,j) = real(i*j) + 0.5
                if (i*j.eq.8) A(i,j)=5.5 ! make sure don't get zero eigenvalues
                F(i) = F(i) + A(i,j)*answer(j)
            end do
        end do

        ! invert
        call MTXGUP(A,F,X,N,LDIG,DET)

        ! check results
        call assert_all_close( answer, X, N, 1e-4, 1e-4, 'Solve matrix equation Ax=F' )
    end subroutine test_MTXGUP
    
    
    !
    ! PLNLEG computes the Legendre polynomials up to order NL-1 at fix value of UU
    !
    ! Test against: 
    !
    !   x^3 = (2/5) * P_3(x) + (3/5) * P_1(x)
    !
    subroutine test_POLLG1
        integer:: NL
        parameter(NL=4)
        real, dimension (NL):: QL; data QL/ 0.0, 0.6, 0.0, 0.4 /
        real:: UU, POLLG1, answer
        integer:: i=0, Ni=10
        do i=0,Ni
            UU = -1.0 + i*2.0/Ni 
            answer = UU**3  
            call assert_close( answer, POLLG1(UU,QL,NL), 1e-6, 1e-6, 'Legendre series representation of UU**3'   )
        end do
    end subroutine test_POLLG1


    !
    ! PLNLEG computes the Legendre polynomials up to order NL-1 at fix value of UU
    !
    ! Test against known values of P_L(x)
    !
    subroutine test_PLNLEG
        integer::NL
        parameter(NL=4)
        real, dimension (NL):: PL, answer

        call PLNLEG(-1.0e0, PL, NL)
        answer = (/ 1.0, -1.0, 1.0, -1.0 /)
        call assert_all_close( answer, PL, NL, 1e-6, 1e-6, 'Legendre polynomial values for mu = -1'   ) 

        call PLNLEG(-0.5e0, PL, NL)
        answer = (/ 1.0, -0.5, -0.125, 55.0/16.0-3.0 /)
        call assert_all_close( answer, PL, NL, 1e-6, 1e-6, 'Legendre polynomial values for mu = -1/2' ) 

        call PLNLEG( 0.0e0, PL, NL)
        answer = (/ 1.0, 0.0, -0.5, 0.0 /)
        call assert_all_close( answer, PL, NL, 1e-6, 1e-6, 'Legendre polynomial values for mu =  0'   ) 

        call PLNLEG( 0.5e0, PL, NL)
        answer = (/ 1.0, 0.5, -0.125, -55.0/16.0+3.0 /)
        call assert_all_close( answer, PL, NL, 1e-6, 1e-6, 'Legendre polynomial values for mu = +1/2' ) 

        call PLNLEG( 1.0e0, PL, NL)
        answer = (/ 1.0, 1.0, 1.0, 1.0 /)
        call assert_all_close( answer, PL, NL, 1e-6, 1e-6, 'Legendre polynomial values for mu = +1'   ) 
    end subroutine test_PLNLEG

end module empend_test

