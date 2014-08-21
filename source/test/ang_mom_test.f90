module ang_mom_test
    use fruit
    use fruit_util
    implicit none
    real*8,external:: CLEBG, RACAH
    
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
    !
    !  Testing CLEBG( j1, j2, j3, m1, m2, m3 ), all arguments are real*8
    !
    !  and uses the following conventions
    !
    !       Clebsh-Gordan coefficient 
    !           = <j1,j2,m1,m2|j3,m3>
    !           = (-)^(j1-j2+m3) * sqrt(2*j3+1) * / j1 j2  j3 \
    !                                             \ m1 m2 -m3 /
    !
    !  The Clebsch-Gordan tests are adapted from the testing routines in Fudge, 
    !  which were taken from T. Kawano's CoH-3.2.0 (Umbrial) code after correcting some errors.  
    !  The function prototype in Fudge is 
    !
    !       double nf_amc_clebsh_gordan( int j1, int j2, int m1, int m2, int j3 )
    !
    !  and uses the following conventions
    !
    !       Clebsh-Gordan coefficient 
    !           = <j1,j2,m1,m2|j3,m1+m2>
    !           = (-)^(j1-j2+m1+m2) * sqrt(2*j3+1) * / j1 j2   j3   \
    !                                                \ m1 m2 -m1-m2 /
    !
    !  Note: Last value m3 is preset to m1+m2.  Any other value will evaluate to 0.0.
    !
    !  The Fudge unit tests are:
    !
    !    class Test_ClebschGordanCoefficient(unittest.TestCase):
    !
    !        def test_all_zeroes(self): 
    !            self.assertAlmostEqual( nf_amc.clebsh_gordan( 0, 0, 0, 0, 0 ), 1.0 )
    !    
    !        def test_odd_sum_of_js(self):
    !            '''J = j1 + j2 + j3 == odd, then is zero'''
    !            self.assertAlmostEqual( nf_amc.clebsh_gordan( 0, 2, 0, 0, 0 ), 0.0 )
    !            self.assertAlmostEqual( nf_amc.clebsh_gordan( 6, 0, 0, 0, 0 ), 0.0 )
    !
    !        def test_wikipedia_values(self):
    !            '''Tougher test value, from Wikipedia: http://en.wikipedia.org/wiki/Table_of_Clebsch%E2%80%93Gordan_coefficients'''
    !            self.assertAlmostEqual( nf_amc.clebsh_gordan( 4, 1, 4, -1, 3 ), 0.8944271909999159 )
    !            self.assertAlmostEqual( nf_amc.clebsh_gordan( 4, 1, 0, 1, 5 ), 0.7745966692414834 )
    !            self.assertAlmostEqual( nf_amc.clebsh_gordan( 4, 4, 0, 0, 0 ), 0.4472135954999579 )
    !            self.assertAlmostEqual( nf_amc.clebsh_gordan( 4, 0, 0, 0, 4 ), 1.0 )
    !            self.assertAlmostEqual( nf_amc.clebsh_gordan( 8, 8, 0, 0, 0 ), 0.33333333333333333 )
    !            self.assertAlmostEqual( nf_amc.clebsh_gordan( 8, 0, 0, 0, 8 ), 1.0 )
    !            self.assertAlmostEqual( nf_amc.clebsh_gordan( 8, 8, 4, -4, 0 ), 0.33333333333333333 )
    !            self.assertAlmostEqual( nf_amc.clebsh_gordan( 8, 8, 6, -6, 0 ), -0.33333333333333333 )
    !            self.assertAlmostEqual( nf_amc.clebsh_gordan( 3, 2, 3, 0, 5 ), 0.6324555320336759 )
    !            self.assertAlmostEqual( nf_amc.clebsh_gordan( 3, 2, 3, 0, 3 ), 0.7745966692414834 )
    !            self.assertAlmostEqual( nf_amc.clebsh_gordan( 3, 2, 1, 2, 5 ), 0.7745966692414834 )
    !            self.assertAlmostEqual( nf_amc.clebsh_gordan( 3, 2, 1, 2, 3 ), -0.6324555320336759 )
    !            self.assertAlmostEqual( nf_amc.clebsh_gordan( 4, 6, 0, 0, 6 ), -0.516398, 6 )
    !            self.assertAlmostEqual( nf_amc.clebsh_gordan( 6, 4, 0, 0, 6 ), -0.516398, 6 )
    !
    ! ---------------------------------------------------------------------------------------------------

    subroutine test_clebsch_gordan_all_zeroes
        call assert_equals( 1.0d0, CLEBG( 0.D0, 0.D0, 0.D0, 0.D0, 0.D0, 0.D0 ), 'CG all zeroes' ) ! passed
    end subroutine test_clebsch_gordan_all_zeroes

    subroutine test_clebsch_gordan_odd_sum_of_js
        !
        ! J = j1 + j2 + j3 == odd, then is zero
        !
        call assert_equals( 0.0d0, CLEBG( 0d0, 1d0, 0d0, 0d0, 0d0, 0d0 ), 'CG odd sum of js' ) ! passed
        call assert_equals( 0.0d0, CLEBG( 3d0, 0d0, 0d0, 0d0, 0d0, 0d0 ), 'CG odd sum of js' ) ! passed
    end subroutine test_clebsch_gordan_odd_sum_of_js

    subroutine test_clebsch_gordan_m3_sumrule
        !
        ! m3 = m1+m2 test... In Fudge version, m3 is calculated.  In CLEBG, m3 is user inputed so better evaluate to zero for illegal values
        !
        call assert_equals( 0d0,                    CLEBG( 2d0, 0.5d0, 1.5d0,   2d0, -0.5d0,  0.5d0 ), 'CG m3 != m1+m2' )
        call assert_equals( 0d0,                    CLEBG( 2d0, 0.5d0, 1.5d0,   2d0,  0.5d0,  1.5d0 ), 'CG m3 != m1+m2' )
        call assert_equals( 0d0,                    CLEBG( 2d0, 0.5d0, 1.5d0,   1d0, -0.5d0,  1.5d0 ), 'CG m3 != m1+m2' )
        call assert_close(  0.8944271909999159d0,   CLEBG( 2d0, 0.5d0, 1.5d0,   2d0, -0.5d0,  1.5d0 ), 1d-6, 1d-6, 'CG m3 == m1+m2' )
    
    end subroutine test_clebsch_gordan_m3_sumrule

    subroutine test_clebsch_gordan_wikipedia_values
        !
        ! Tougher test value, from Wikipedia: 
        !   http://en.wikipedia.org/wiki/Table_of_Clebsch%E2%80%93Gordan_coefficients
        !
        !                                                    j1    j2    j3       m1    m2      m3
        call assert_close(   0.8944271909999159d0,  CLEBG(   2d0, 0.5d0, 1.5d0,   2d0, -0.5d0,  1.5d0 ), 1d-6, 1d-6, 'CG 1/2 spin' ) 
        call assert_close(   0.7745966692414834d0,  CLEBG(   2d0, 0.5d0, 2.5d0,   0d0,  0.5d0,  0.5d0 ), 1d-6, 1d-6, 'CG 1/2 spin' ) 
        call assert_close(   0.4472135954999579d0,  CLEBG(   2d0,   2d0,   0d0,   0d0,    0d0,    0d0 ), 1d-6, 1d-6, 'CG all m=0'  )
        call assert_equals(  1.0d0,                 CLEBG(   2d0,   0d0,   2d0,   0d0,    0d0,    0d0 ),             'CG all m=0'  ) 
        call assert_close(   0.33333333333333333d0, CLEBG(   4d0,   4d0,   0d0,   0d0,    0d0,    0d0 ), 1d-6, 1d-6, 'CG all m=0'  )
        call assert_equals(  1.0d0,                 CLEBG(   4d0,   0d0,   4d0,   0d0,    0d0,    0d0 ),             'CG all m=0'  ) 
        call assert_close(   0.33333333333333333d0, CLEBG(   4d0,   4d0,   0d0,   2d0,   -2d0,    0d0 ), 1d-6, 1d-6, 'CG'          )
        call assert_close(  -0.33333333333333333d0, CLEBG(   4d0,   4d0,   0d0,   3d0,   -3d0,    0d0 ), 1d-6, 1d-6, 'CG'          )
        call assert_close(   0.6324555320336759d0,  CLEBG( 1.5d0,   1d0, 2.5d0, 1.5d0,    0d0,  1.5d0 ), 1d-6, 1d-6, 'CG 1/2 spin' ) 
        call assert_close(   0.7745966692414834d0,  CLEBG( 1.5d0,   1d0, 1.5d0, 1.5d0,    0d0,  1.5d0 ), 1d-6, 1d-6, 'CG 1/2 spin' ) 
        call assert_close(   0.7745966692414834d0,  CLEBG( 1.5d0,   1d0, 2.5d0, 0.5d0,    1d0,  1.5d0 ), 1d-6, 1d-6, 'CG 1/2 spin' ) 
        call assert_close(  -0.6324555320336759d0,  CLEBG( 1.5d0,   1d0, 1.5d0, 0.5d0,    1d0,  1.5d0 ), 1d-6, 1d-6, 'CG 1/2 spin' ) 
        call assert_close(  -0.516398d0,            CLEBG(   2d0,   3d0,   3d0,   0d0,    0d0,    0d0 ), 1d-6, 1d-6, 'CG all m=0'  )
        call assert_close(  -0.516398d0,            CLEBG(   3d0,   2d0,   3d0,   0d0,    0d0,    0d0 ), 1d-6, 1d-6, 'CG all m=0'  )
    end subroutine test_clebsch_gordan_wikipedia_values


    ! ---------------------------------------------------------------------------------------------------
    !  Testing RACAH( a, b, c, d, e, f ), all arguments are real*8
    !
    !  Values checked against A.Simon, J.H. Vander Sluis, L.C. Biedenharn "Tables of the Racah Coefficients", ORNL-1679, March 26, 1954.
    !  Analytic values in comments computed from "Anthony Stone's Wigner coefficient calculator":
    !       http://www-stone.ch.cam.ac.uk/wigner.shtml
    !  They match the test values to the precision stated in the ORNL report.
    !
    ! ---------------------------------------------------------------------------------------------------

    subroutine test_racah_all_zeroes
        call assert_equals( 1.0d0, RACAH( 0.D0, 0.D0, 0.D0, 0.D0, 0.D0, 0.D0 ) ) ! passed
    end subroutine test_racah_all_zeroes

    subroutine test_racah_nontrivial_values_from_6j_tests
        call assert_close(  0.11167656571008167d0,  RACAH( 4.D0, 2.D0,  2.D0, 2.D0,  2.D0,  4.D0 ),  1d-6, 1d-6, 'RACAH' ) !  (1/21)*sqrt(11/2)
        call assert_close(  0.22360679774997896d0,  RACAH( 2.D0, 1.d0,  1.d0, 1.d0,  1.d0,  2.d0 ),  1d-6, 1d-6, 'RACAH' ) ! –(1/2) *sqrt(1/5)
        call assert_close(  0.07453559924999298d0,  RACAH( 2.D0, 2.d0,  1.d0, 1.d0,  1.d0,  2.d0 ),  1d-6, 1d-6, 'RACAH' ) !  (1/6) *sqrt(1/5)
    end subroutine test_racah_nontrivial_values_from_6j_tests
        
    subroutine test_racah_nontrivial_values_sequence
        call assert_close(  0.0161413111d0,  RACAH( 2.5d0, 4.d0, 5.5d0, 4.d0, 2.5d0, 3.d0 ),  1d-6, 1d-6, 'RACAH sequence' ) ! (1/18) * sqrt(13/154)
        call assert_close(  0.0435382494d0,  RACAH( 3.5d0, 4.d0, 5.5d0, 4.d0, 2.5d0, 3.d0 ),  1d-6, 1d-6, 'RACAH sequence' ) ! (17/66)* sqrt(1/35)
        call assert_close(  0.0623609564d0,  RACAH( 4.5d0, 4.d0, 5.5d0, 4.d0, 2.5d0, 3.d0 ),  1d-6, 1d-6, 'RACAH sequence' ) ! (1/30) * sqrt(7/2)
        call assert_close(  0.0341078118d0,  RACAH( 5.5d0, 4.d0, 5.5d0, 4.d0, 2.5d0, 3.d0 ),  1d-6, 1d-6, 'RACAH sequence' ) ! (7/18) * sqrt(1/130)
        call assert_close( -0.0544676990d0,  RACAH( 6.5d0, 4.d0, 5.5d0, 4.d0, 2.5d0, 3.d0 ),  1d-6, 1d-6, 'RACAH sequence' ) ! (1/11) * sqrt(14/39)
    end subroutine test_racah_nontrivial_values_sequence

end module ang_mom_test

