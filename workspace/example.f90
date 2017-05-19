program test
  use simulator_without_split 
  call init 

  ! test results, define 'Q_hes'
  include "10sp2_result.inc"
  
  print *, "6*4 test data:", tac() 


end program test
