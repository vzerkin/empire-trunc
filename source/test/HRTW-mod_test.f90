module HRTW_mod_test
  use fruit
  use fruit_util
  use angular_momentum
  implicit none

contains 

    subroutine assert_close( a, b, reltol, abstol, testname )
        real*8, intent (in):: a, b, reltol, abstol
        character(500):: message, msg_prefix
        character(*), intent (in), optional:: testname
        msg_prefix=''
        if ( testname .ne. '' ) then
            msg_prefix = trim(testname) //':'
        end if
        message = trim( trim(msg_prefix)//' '//trim(to_s(a)) // ' not close to ' // trim(to_s(b)) )
        call assert_true( (dabs(a - b) < abstol) .or. ( dabs(a-b)/dabs(a+b) < reltol ), message )
    end subroutine assert_close

    ! ---------------------------------------------------------------------------------------------------
    !  Testing Blatt & Biedenharn's Z coefficient.  This is not coded directly in ang_mom.f, but is 
    !  an alternative test of both RACAH and CLEBG.
    !
    !  Values checked against L.C. Biedenharn, "Revised Z Tables of the Racah Coefficients", ORNL-1501, May 28, 1953.
    !
    ! ---------------------------------------------------------------------------------------------------
    
    subroutine test_ZCoefficient_Swave
        !
        ! Values checked against L.C. Biedenharn, "Revised Z Tables of the Racah Coefficients", ORNL-1501, May 28, 1953.
        !
        call assert_close( sqrt(2.0d0), ZCoefficient( 0d0, 0.5d0, 0d0, 0.5d0, 0.5d0, 0d0 ), 1d-6, 1d-6, 'Z l1=l2=0, S=1/2, L=0' )
        call assert_close( sqrt(2.0d0), ZCoefficient( 1d0, 0.5d0, 1d0, 0.5d0, 0.5d0, 0d0 ), 1d-6, 1d-6, 'Z l1=l2=1, S=1/2, L=0' )
        call assert_close( sqrt(10d0),  ZCoefficient( 4d0, 4.5d0, 4d0, 4.5d0, 0.5d0, 0d0 ), 1d-6, 1d-6, 'Z l1=l2=4, S=1/2, L=0' )
        call assert_close( sqrt(10d0),  ZCoefficient( 5d0, 4.5d0, 5d0, 4.5d0, 0.5d0, 0d0 ), 1d-6, 1d-6, 'Z l1=l2=5, S=1/2, L=0' )
    end subroutine test_ZCoefficient_Swave

    subroutine test_ZCoefficient_Pwave
        !
        ! Values checked against L.C. Biedenharn, "Revised Z Tables of the Racah Coefficients", ORNL-1501, May 28, 1953.
        !
        call assert_close( -sqrt(40d0/3d0),  ZCoefficient( 4d0, 4.5d0, 3d0, 3.5d0, 0.5d0, 1d0 ), 1d-6, 1d-6, 'Z l1=4, l2=3, S=1/2, L=1' )
        call assert_close( -sqrt(40d0/3d0),  ZCoefficient( 5d0, 4.5d0, 4d0, 3.5d0, 0.5d0, 1d0 ), 1d-6, 1d-6, 'Z l1=5, l2=4, S=1/2, L=1' )
    end subroutine test_ZCoefficient_Pwave
    
    subroutine test_ZCoefficient_Dwave
        !
        ! Values checked against L.C. Biedenharn, "Revised Z Tables of the Racah Coefficients", ORNL-1501, May 28, 1953.
        !
        call assert_close( -sqrt(8d0/7d0),  ZCoefficient( 3d0, 3.5d0, 3d0, 2.5d0, 0.5d0, 2d0 ), 1d-6, 1d-6, 'Z l1=3, l2=3, S=1/2, L=2' )
        call assert_close(  sqrt(8d0/7d0),  ZCoefficient( 4d0, 3.5d0, 2d0, 2.5d0, 0.5d0, 2d0 ), 1d-6, 1d-6, 'Z l1=4, l2=2, S=1/2, L=2' )
    end subroutine test_ZCoefficient_Dwave

    subroutine test_ZBarCoefficient_Swave
        !
        ! Values checked against L.C. Biedenharn, "Revised Z Tables of the Racah Coefficients", ORNL-1501, May 28, 1953.
        !
        call assert_close( sqrt(2.0d0), ZBarCoefficient( 0d0, 0.5d0, 0d0, 0.5d0, 0.5d0, 0d0 ), 1d-6, 1d-6, 'Z l1=l2=0, S=1/2, L=0' )
        call assert_close( sqrt(2.0d0), ZBarCoefficient( 1d0, 0.5d0, 1d0, 0.5d0, 0.5d0, 0d0 ), 1d-6, 1d-6, 'Z l1=l2=1, S=1/2, L=0' )
        call assert_close( sqrt(10d0),  ZBarCoefficient( 4d0, 4.5d0, 4d0, 4.5d0, 0.5d0, 0d0 ), 1d-6, 1d-6, 'Z l1=l2=4, S=1/2, L=0' )
        call assert_close( sqrt(10d0),  ZBarCoefficient( 5d0, 4.5d0, 5d0, 4.5d0, 0.5d0, 0d0 ), 1d-6, 1d-6, 'Z l1=l2=5, S=1/2, L=0' )
    end subroutine test_ZBarCoefficient_Swave

    subroutine test_ZBarCoefficient_Pwave
        !
        ! Values checked against L.C. Biedenharn, "Revised Z Tables of the Racah Coefficients", ORNL-1501, May 28, 1953.
        !
        call assert_close( -sqrt(40d0/3d0),  ZBarCoefficient( 4d0, 4.5d0, 3d0, 3.5d0, 0.5d0, 1d0 ), 1d-6, 1d-6, 'Z l1=4, l2=3, S=1/2, L=1' )
        call assert_close( -sqrt(40d0/3d0),  ZBarCoefficient( 5d0, 4.5d0, 4d0, 3.5d0, 0.5d0, 1d0 ), 1d-6, 1d-6, 'Z l1=5, l2=4, S=1/2, L=1' )
    end subroutine test_ZBarCoefficient_Pwave
    
    subroutine test_ZBarCoefficient_Dwave
        !
        ! Values checked against L.C. Biedenharn, "Revised Z Tables of the Racah Coefficients", ORNL-1501, May 28, 1953.
        !
        call assert_close(  sqrt(8d0/7d0),  ZBarCoefficient( 3d0, 3.5d0, 3d0, 2.5d0, 0.5d0, 2d0 ), 1d-6, 1d-6, 'Z l1=3, l2=3, S=1/2, L=2' )
        call assert_close(  sqrt(8d0/7d0),  ZBarCoefficient( 4d0, 3.5d0, 2d0, 2.5d0, 0.5d0, 2d0 ), 1d-6, 1d-6, 'Z l1=4, l2=2, S=1/2, L=2' )
    end subroutine test_ZBarCoefficient_Dwave
    
    subroutine test_Blatt_Biedenharn_BCoeff
 
        call assert_equals( 0.d0, Blatt( 1.d0, 2.d0, 3.d0, 4.d0, 5.d0, 6.d0, 7.d0, 8.d0, 9.d0, 10.d0 ) )
    
    end subroutine test_Blatt_Biedenharn_BCoeff
end module HRTW_mod_test

