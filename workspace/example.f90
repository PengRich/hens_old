program test
  use simulator_without_split 
  real(kind=8) :: y
  call init 
  include "10sp2_result.inc"
  ! print*, Q_hes
  y = tac()
  print *, y
  print *, results
  print *, "*****************"
        do k=1, n_st
        do i=1, n_hs
          do j=1, n_cs
      ! do k=n_st, 1, -1
      !   do i=n_hs, 1, -1
      !     do j=n_cs, 1, -1


            id = (k-1)*n_hs*n_cs + (i-1)*n_cs + j
            if(i .eq. 4) then
              print *, hes(i,j,k)%T_hin, hes(i,j,k)%T_hout, hes(i,j,k)%Q,hes(i,j,k)%cs%HCpF
            endif
            if(hes(i,j,k)%ex .eq. 0) cycle
            ! print *, hes(i,j,k)%hs%h, hes(i,j,k)%cs%h, hes(i,j,k)%K_he
            !print *, id, hes(i,j,k)%A_he, hes(i,j,k)%Q
            ! print *, hes(i,j,k)%T_hin-hes(i,j,k)%T_cout, hes(i,j,k)%T_hout-hes(i,j,k)%T_cin

          enddo
        enddo
      enddo

!   do k=1, n_hs+n_cs
!     print *, utilities(k)%T_in,utilities(k)%T_out
!   enddo
!       do i=1, n_hs
!         print *, hes(i, 5, n_st)%T_hout
!       enddo
!       do j=1, n_cs
!         print *, hes(1, j, 1)%T_cout
!       enddo




end program test
