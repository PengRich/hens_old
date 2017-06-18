program test
  use simulator_without_split 
  call init_case

  ! test results, define 'Q_hes'
  ! include "9sp1_result.inc"
  include "10sp2_result.inc"
  print *, tac()
  print *, simulated_result
  stop
      do i=1, n_hs
        if(sizes(i).eq.0) cycle
        do id=1, sizes(i)
          print *, hesp(loc(i, id))%ex_he, hesp(loc(i, id))%ex_hu, hesp(loc(i, id))%ex_cu
          print *, hesp(loc(i, id))%A_he, hesp(loc(i, id))%A_hu, hesp(loc(i, id))%A_cu
        enddo
      enddo
      do i=1, n_hs+n_cs
        print *, utip(i)%Q

      enddo


  ! print *, y
  ! print *, results
  ! print *, sizes
  ! 
  ! y=0.d0
  ! do id=1, n_he
  !    if(hesp(id)%ex .eq. 0) cycle
  !   print *, hesp(id)%A_he, hesp(id)%Q, hesp(id)%K_he
  !   y = y+ 70.d0*hesp(id)%A_he+2000.d0

  ! enddo

  ! do i=1, n_hs+n_cs
  !   print *, utilities(i)%A, utilities(i)%Q

  !   y = y+ 70.d0*utip(i)%A+2000.d0
  ! enddo
  ! print *,y+Q_hu(1)*60.d0 + Q_cu(1)*6.d0


end program test
