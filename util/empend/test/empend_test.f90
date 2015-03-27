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
!        write(*,*) testname
        i=1
        do while (i.le.n)
!            write(*,*) a(i), b(i)
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
    ! PLNLEG computes the Legendre polynomials up to order NL-1 at fix value of UU
    !
    ! Test against: 
    !
    !   x^3 = (2/5) * P_3(x) + (3/5) * P_1(x)
    !
    subroutine test_POLLG1
        integer:: NL
        parameter(NL=4)
        real, dimension (NL):: QL=(/ 0.0, 0.6, 0.0, 0.4 /)
        real:: UU, POLLG1, answer
        integer:: i=0, Ni=10
        do while (i.le.Ni)
            UU = -1.0 + i*2.0/Ni 
            answer = UU**3  
            call assert_close( answer, POLLG1(UU,QL,NL), 1e-6, 1e-6, 'Legendre series representation of UU**3'   )
            i = i + 1
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

