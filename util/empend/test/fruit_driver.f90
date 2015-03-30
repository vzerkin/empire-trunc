program fruit_driver
  use fruit
  use empend_test
    
  call init_fruit                 
  call test_PLNLEG
  call test_POLLG1
  call test_MTXGUP
  call test_LSQLEG
  call test_LSQLGV
  call fruit_summary
  call fruit_finalize
end program fruit_driver
