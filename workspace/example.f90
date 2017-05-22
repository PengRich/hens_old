program test
  use simulator_without_split 
  call init 

  ! test results, define 'Q_hes'
  include "9sp1_result.inc"

  print *, "6*4 test data:", tac(Q_hes) 
  ! print *, "6*4 test data:", tac() 
  print *, results
  do i =1 ,n_hs+n_cs
    print *, utilities(i)%T_in

  enddo


end program test
