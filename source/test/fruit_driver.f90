program fruit_driver
  use fruit
  use ang_mom_test
  use hrtw_mod_test

  call init_factorial ! initialize the factorial COMMON block used in ang_mom.f 
    
  call init_fruit                 
  call test_clebsch_gordan_all_zeroes
  call test_clebsch_gordan_odd_sum_of_js
  call test_clebsch_gordan_m3_sumrule
  call test_clebsch_gordan_wikipedia_values
  call test_racah_all_zeroes
  call test_racah_nontrivial_values_from_6j_tests
  call test_racah_nontrivial_values_sequence
  call test_ZCoefficient_Swave
  call test_ZCoefficient_Pwave
  call test_ZCoefficient_Dwave
  call test_ZBarCoefficient_Swave
  call test_ZBarCoefficient_Pwave
  call test_ZBarCoefficient_Dwave
  call test_Blatt_Biedenharn_BCoeff
  call fruit_summary
  call fruit_finalize
end program fruit_driver
