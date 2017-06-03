program sa_example
  use simulator_without_split
  use hens_bilevel_sa 
  implicit none
  type :: x
    real(kind=8), pointer :: Q
  end type x
  call init

  include "9sp1_result.inc"
  call init_tac()
  print *, tac_with_topo()
  ! Q_hes = 0.d0

  allocate(real_xs(sum(sizes)))

  k = 1
  do i=1, n_hs+n_cs
    do j=1, sizes(i)
    real_xs(k)%x => Q_hes(loc(i, j))
    real_xs(k)%low_bound = 0.d0
    real_xs(k)%up_bound  = min(hesp(loc(i,j))%hs%HCpF*(hesp(loc(i,j))%hs%T_in-hesp(loc(i,j))%hs%T_out),&
        hesp(loc(i,j))%cs%HCpF*(hesp(loc(i,j))%cs%T_out-hesp(loc(i,j))%cs%T_in))
    print *, real_xs(k)%up_bound
    k = k + 1
    enddo
  enddo

  call low_level(tac_with_topo) 

  deallocate(real_xs)
end program sa_example
