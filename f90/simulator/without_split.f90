module simulator_without_split
  use hens_def
  use flexible_utility
  implicit none

  integer(kind=4) :: sizes(n_hs+n_cs)=0
  integer(kind=4) :: loc(n_hs+n_cs, max(n_cs, n_hs)*n_st)
  
  private cal_unit_temp
  private cal_internal_area_utility
  private cal_terminal_area_utility
  private cal_tac

  contains
    !**********************************************
    ! initialize
    !**********************************************
    subroutine init_tac(Qs)
      implicit none
      real(kind=8) :: Qs(n_he)

      results = cost(0.d0, 0.d0, 0.d0, 0.d0, global_pen)
        
      do id=1, n_he
        if(abs(Qs(id)) .gt. 1.d-3) then 
          hesp(id)%ex   = 1
          hesp(id)%Q    = Qs(id)
          hesp(id)%A_he = 0.d0 
          hesp(id)%A_hu = 0.d0 
          hesp(id)%A_cu = 0.d0 

          sizes(hesp(id)%h_id)      = sizes(hesp(id)%h_id) + 1
          sizes(n_hs+hesp(id)%c_id) = sizes(n_hs+hesp(id)%c_id) + 1

          loc(hesp(id)%h_id, sizes(hesp(id)%h_id))           = id 
          loc(n_hs+hesp(id)%c_id, sizes(n_hs+hesp(id)%c_id)) = id 
        else
           hesp(id)%ex   = 0
        endif
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
        utip(i)%A  = 0.d0 
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
        utip(i)%A  = 0.d0 
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

      do i=1, n_hs

        if(sizes(i).eq.0) cycle

        do id=1, sizes(i)
          if(hesp(loc(i,id))%A_he .gt. 1.d-3) then
            results%ac = results%ac + expen%fix_A + &
                expen%fac_A*hesp(loc(i,id))%A_he**expen%ex_A
          else
            if(hesp(loc(i,id))%A_hu .gt. 1.d-3) then
              Q_hu(1) = Q_hu(1) + abs(hesp(loc(i,id))%Q)
              results%ac = results%ac + expen%fix_A + &
                  expen%fac_A*hesp(loc(i,id))%A_hu**expen%ex_A
            endif
            if(hesp(loc(i,id))%A_cu .gt. 1.d-3) then
                Q_cu(1) = Q_cu(1) + abs(hesp(loc(i,id))%Q)
                results%ac = results%ac + expen%fix_A + &
                    expen%fac_A*hesp(loc(i,id))%A_cu**expen%ex_A
            endif
          endif
        enddo

      enddo

      do k=1, n_hs+n_cs
        if(utip(k)%ex .eq. 0) cycle
        results%ac = results%ac + expen%fix_A + expen%fac_A*utip(k)%A**expen%ex_A
        Q_cu(1) = Q_cu(1) + max(0.d0, utip(k)%Q)
        Q_hu(1) = Q_hu(1) + abs(min(0.d0, utip(k)%Q))
      enddo
      
      results%cuc = Q_cu(1) * expen%fac_cu
      results%huc = Q_hu(1) * expen%fac_hu

      global_pen = 0.d0

    end subroutine cal_tac

    !*******************************
    ! external call tac
    !*******************************
    real(kind=8) function tac(Qs)
      implicit none
      real(kind=8) :: Qs(n_he)
      
      call init_tac(Qs)

      tac = tac_with_topo(Qs) 
 
    end function tac

    real(kind=8) function tac_with_topo(Qs)
      implicit none
      real(kind=8) :: Qs(n_he)
      
      call cal_unit_temp()
      call cal_internal_area_utility()
      call cal_terminal_area_utility()
      call cal_tac()

      tac_with_topo = results%ac + results%huc + results%cuc + results%pen
      results%tac = tac_with_topo
 
    end function tac_with_topo

end module simulator_without_split
