module simulator_without_split
  use flexible_utility_base
  implicit none

  integer(kind=4) :: sizes(n_hs+n_cs)
  integer(kind=4) :: loc(n_hs+n_cs, max(n_cs, n_hs)*n_st)

  private init_value
  private cal_unit_temp
  private cal_internal_area_utility
  private cal_terminal_area_utility
  private cal_tac
  contains
    !**********************************************
    ! initialize
    !**********************************************
    subroutine init_value()
      implicit none

      global_pen = 0.d0
      Q_hu       = 0.d0
      Q_cu       = 0.d0
    end subroutine init_value

    subroutine init_tac()
      implicit none

      sizes = 0
      do k=1, n_st
        do i=1, n_hs
          do j=1, n_cs
            id = (k-1)*n_hs*n_cs + (i-1)*n_cs + j
            if(abs(Q_hes(i,j,k)) .gt. 1.d-3) then 
              hesp(id)%ex_he   = 1
              ! hesp(id)%Q    => Q_hes(i,j,k)
              ! hesp(id)%A_he = 0.d0 
              ! hesp(id)%A_hu = 0.d0 
              ! hesp(id)%A_cu = 0.d0 

              sizes(i)      = sizes(i)      + 1
              sizes(n_hs+j) = sizes(n_hs+j) + 1

              loc(i, sizes(i))           = id 
              loc(n_hs+j, sizes(n_hs+j)) = id 
            else
               hesp(id)%ex_he   = 0
            endif
          enddo
        enddo
      enddo

      return
    end subroutine init_tac

    !***************************************************************
    ! calculate inlet and outlet temperature of each heat exchanger
    !***************************************************************
    subroutine cal_unit_temp()
      implicit none

      do i=1, n_hs

        if(sizes(i).eq.0) cycle

        hesp(loc(i, 1))%T_hin  = streams(i)%T_in
        hesp(loc(i, 1))%T_hout = hesp(loc(i, 1))%T_hin - &
            hesp(loc(i, 1))%Q/hesp(loc(i, 1))%hs%HCpF

        if(sizes(i).eq.1) cycle

        do k=2, sizes(i) 
          hesp(loc(i, k))%T_hin  = hesp(loc(i, k-1))%T_hout
          hesp(loc(i, k))%T_hout = hesp(loc(i, k))%T_hin - &
              hesp(loc(i, k))%Q/hesp(loc(i, k))%hs%HCpF
        enddo
      enddo

      do j=1, n_cs

        i = n_hs+j
        if(sizes(i).eq.0) cycle

        hesp(loc(i, sizes(i)))%T_cin  = streams(i)%T_in
        hesp(loc(i, sizes(i)))%T_cout = hesp(loc(i, sizes(i)))%T_cin + &
            hesp(loc(i, sizes(i)))%Q/hesp(loc(i, sizes(i)))%cs%HCpF

        if(sizes(i).eq.1) cycle

        do k=sizes(i)-1, 1, -1
          hesp(loc(i, k))%T_cin  = hesp(loc(i, k+1))%T_cout
          hesp(loc(i, k))%T_cout = hesp(loc(i, k))%T_cin + &
              hesp(loc(i, k))%Q/hesp(loc(i, k))%cs%HCpF
        enddo
        
      enddo

      return 
    end subroutine cal_unit_temp

    !****************************************************************************
    ! calculate area of each internal heat exchanger, including internal utility
    !****************************************************************************
    subroutine cal_internal_area_utility()
      implicit none

      do i=1, n_hs
        if(sizes(i).eq.0) cycle
        do id=1, sizes(i)
          call cal_internal_he(hesp(loc(i, id)))
        enddo
      enddo

      return
    end subroutine cal_internal_area_utility

    !**************************************************************************
    ! calculate area of terminal utility
    !**************************************************************************
    subroutine cal_terminal_area_utility()
      implicit none

      real(kind=8) :: Q

      do i=1, n_hs
        if(sizes(i).eq.0) then
          utip(i)%T_in = streams(i)%T_in
        else
          utip(i)%T_in = hesp(loc(i, sizes(i)))%T_hout
        endif
        Q = (utip(i)%T_in-utip(i)%T_out)*utip(i)%s%HCpF

        if(abs(Q) .gt. 1.d-3) then 
          utip(i)%ex = 1
        else
          utip(i)%ex = 0
        endif
        utip(i)%Q  = Q
        ! utip(i)%A  = 0.d0 
      enddo

      do j=1, n_cs
        i = n_hs + j
        if(sizes(i).eq.0) then
          utip(i)%T_in = streams(i)%T_in
        else
          utip(i)%T_in = hesp(loc(i, 1))%T_cout
        endif
        Q = (utip(i)%T_in-utip(i)%T_out)*utip(i)%s%HCpF

        if(abs(Q) .gt. 1.d-3) then 
          utip(i)%ex = 1
        else
          utip(i)%ex = 0
        endif
        utip(i)%Q  = Q
        ! utip(i)%A  = 0.d0 
      enddo

      do k=1, n_hs+n_cs
        if(utip(k)%ex .eq. 0) cycle
        call cal_teriminal_he(utip(k))
      enddo

      return
    end subroutine cal_terminal_area_utility

    !*******************************************************
    ! cal tac 
    !*******************************************************
    subroutine cal_tac()
      implicit none

      simulated_result = cost(0.d0, 0.d0, 0.d0, 0.d0, global_pen)

      do i=1, n_hs

        if(sizes(i).eq.0) cycle

        do id=1, sizes(i)
          if(hesp(loc(i,id))%ex_he .eq. 1) then
            simulated_result%ac = simulated_result%ac + expen%fix_A + &
                expen%fac_A*hesp(loc(i,id))%A_he**expen%ex_A
          else
            if(hesp(loc(i,id))%ex_hu .eq. 1) then
              Q_hu(1) = Q_hu(1) + abs(hesp(loc(i,id))%Q)
              simulated_result%ac = simulated_result%ac + expen%fix_A + &
                  expen%fac_A*hesp(loc(i,id))%A_hu**expen%ex_A
            endif
            if(hesp(loc(i,id))%ex_cu .eq. 1) then
                Q_cu(1) = Q_cu(1) + abs(hesp(loc(i,id))%Q)
                simulated_result%ac = simulated_result%ac + expen%fix_A + &
                    expen%fac_A*hesp(loc(i,id))%A_cu**expen%ex_A
            endif
          endif
        enddo

      enddo

      do k=1, n_hs+n_cs
        if(utip(k)%ex .eq. 0) cycle
        simulated_result%ac = simulated_result%ac + expen%fix_A + expen%fac_A*utip(k)%A**expen%ex_A
        Q_cu(1) = Q_cu(1) + max(0.d0, utip(k)%Q)
        Q_hu(1) = Q_hu(1) + abs(min(0.d0, utip(k)%Q))
      enddo
      
      simulated_result%cuc = Q_cu(1) * expen%fac_cu
      simulated_result%huc = Q_hu(1) * expen%fac_hu

      ! global_pen = 0.d0
      ! Q_hu = 0.d0
      ! Q_cu = 0.d0
      call init_value()

    end subroutine cal_tac

    !*******************************
    ! external call tac
    !*******************************
    real(kind=8) function tac()
      implicit none
      
      call init_tac()

      tac = tac_with_topo() 
 
    end function tac

    real(kind=8) function tac_with_topo()
      implicit none
      
      call cal_unit_temp()
      call cal_internal_area_utility()
      call cal_terminal_area_utility()
      call cal_tac()

      tac_with_topo = simulated_result%ac + simulated_result%huc + simulated_result%cuc + simulated_result%pen
      simulated_result%tac = tac_with_topo
 
    end function tac_with_topo

end module simulator_without_split
