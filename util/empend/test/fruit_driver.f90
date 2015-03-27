program fruit_driver
  use fruit
  use empend_test
    
  call init_fruit                 
  call test_PLNLEG
  call test_POLLG1
  call fruit_summary
  call fruit_finalize
end program fruit_driver
