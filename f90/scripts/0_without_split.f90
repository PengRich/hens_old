module simulator_without_split
  use hens_def
  use flexible_utility
  implicit none
  contains
    !***************************************************************
    ! calculate inlet and outlet temperature of each heat exchanger
    !***************************************************************
    subroutine cal_unit_temp
      implicit none

      do k=1, n_st
        do i=1, n_hs
          do j=1, n_cs

            id = (k-1)*n_hs*n_cs + (i-1)*n_cs + j

            if(j .eq. 1) then
              if(k .ne. 1) then
                hes(i,j,k)%T_hin = hes(i,n_cs,k-1)%T_hout
              else
                hes(i,j,k)%T_hin = streams(i)%T_in 
              endif
            else
              hes(i,j,k)%T_hin = hes(i,j-1,k)%T_hout
            endif
            hes(i,j,k)%T_hout = hes(i,j,k)%T_hin - Q_hes(id)/hes(i,j,k)%hs%HCpF


            if(abs(Q_hes(id)) .gt. 1.d-3) then
              hes(i,j,k)%ex = 1
              hes(i,j,k)%Q  = Q_hes(id)
            else
              hes(i,j,k)%ex = 0
              hes(i,j,k)%Q  = 0.d0
            endif
            hes(i,j,k)%A_he  = 0.d0 
            hes(i,j,k)%A_hu  = 0.d0 
            hes(i,j,k)%A_cu  = 0.d0 

          enddo
        enddo
      enddo

      do k=n_st, 1, -1
        do i=n_hs, 1, -1
          do j=n_cs, 1, -1

            id = (k-1)*n_hs*n_cs + (i-1)*n_cs + j

            if(i .eq. n_hs) then
              if(k .ne. n_st) then
                hes(i,j,k)%T_cin = hes(1,j,k+1)%T_cout
              else
                hes(i,j,k)%T_cin = streams(n_hs+j)%T_in
              endif
            else
                hes(i,j,k)%T_cin = hes(i+1,j,k)%T_cout
            endif
            hes(i,j,k)%T_cout = hes(i,j,k)%T_cin + Q_hes(id)/hes(i,j,k)%cs%HCpF
                
          enddo
        enddo
      enddo

      return 
    end subroutine cal_unit_temp

    !****************************************************************************
    ! calculate area of each internal heat exchanger, including internal utility
    !****************************************************************************
    subroutine cal_internal_area_utility
      implicit none

      call cal_unit_temp

      do k=1, n_st
        do i=1, n_hs
          do j=1, n_cs

            if(hes(i,j,k)%ex .eq. 0) cycle

            hes(i,j,k) = flexible_he(hes(i,j,k))

          enddo
        enddo
      enddo

    end subroutine cal_internal_area_utility

    !*************************************
    ! calculate area of terminal utility
    !*************************************
    subroutine cal_terminal_area_utility
      implicit none

      real(kind=8) :: Q

      call cal_internal_area_utility

      do i=1, n_hs
        utilities(i)%T_in = hes(i, n_cs, n_st)%T_hout
        Q = (utilities(i)%T_in-utilities(i)%T_out)*utilities(i)%s%HCpF
        if(abs(Q) .gt. 1.d-3) then 
          utilities(i)%ex = 1
        else
          utilities(i)%ex = 0
        endif
        utilities(i)%Q  = Q
        utilities(i)%A  = 0.d0 
      enddo

      do j=1, n_cs
        k = n_hs+j
        utilities(k)%T_in = hes(1, j, 1)%T_cout
        Q = (utilities(k)%T_in-utilities(k)%T_out)*utilities(k)%s%HCpF
        if(abs(Q) .gt. 1.d-3) then 
          utilities(k)%ex = 1
        else
          utilities(k)%ex = 0
        endif
        utilities(k)%Q  = Q
        utilities(k)%A  = 0.d0 
      enddo

      do k=1, n_hs+n_cs
        if(utilities(k)%ex .eq. 0) cycle

        if(utilities(k)%Q .gt. 0.d0) then

          dtl = utilities(k)%T_in - utilities(k)%cu%T_out
          dtr = utilities(k)%T_out - utilities(k)%cu%T_in
          if(dtl.gt.0.d0 .and. dtr.gt.0.d0) then
            utilities(k)%A = cal_he_area(utilities(k)%Q, utilities(k)%K_cu)
          else
            call add_penalty(dtl)
            call add_penalty(dtr)
          endif

        else

          dtl = utilities(k)%hu%T_in - utilities(k)%T_out
          dtr = utilities(k)%hu%T_out - utilities(k)%T_in
          if(dtl.gt.0.d0 .and. dtr.gt.0.d0) then
            utilities(k)%A = cal_he_area(abs(utilities(k)%Q), utilities(k)%K_hu)
          else
            call add_penalty(dtl)
            call add_penalty(dtr)
          endif
        endif

      enddo

      return

    end subroutine cal_terminal_area_utility

    !*************************************
    ! two verisons of tac 
    !*************************************
    subroutine cal_tac
      print *, "cal tac"
    end subroutine cal_tac

    real(kind=8) function tac()
      implicit none

      results%ac  = 0.d0
      results%huc = 0.d0
      results%cuc = 0.d0
      results%tac = 0.d0
      results%pen = global_pen 

      call cal_terminal_area_utility

      do k=1, n_st
        do i=1, n_hs
          do j=1, n_cs

            if(hes(i,j,k)%ex .eq. 0) cycle
            
            if(hes(i,j,k)%A_he .gt. 1.d-3) then
              results%ac = results%ac + expen%fix_A + expen%fac_A*hes(i,j,k)%A_he**expen%ex_A
            else
              if(hes(i,j,k)%A_hu .gt. 1.d-3) then
                Q_hu(1) = Q_hu(1) + abs(hes(i,j,k)%Q)
                results%ac = results%ac + expen%fix_A + expen%fac_A*hes(i,j,k)%A_hu**expen%ex_A
              endif
              if(hes(i,j,k)%A_cu .gt. 1.d-3) then
                Q_cu(1) = Q_cu(1) + abs(hes(i,j,k)%Q)
                results%ac = results%ac + expen%fix_A + expen%fac_A*hes(i,j,k)%A_cu**expen%ex_A
              endif
            endif

          enddo
        enddo
      enddo

      do k=1, n_hs+n_cs
        if(utilities(k)%ex .eq. 0) cycle
        if(utilities(k)%A .gt. 1.d-3) then
          results%ac = results%ac + expen%fix_A + expen%fac_A*utilities(k)%A**expen%ex_A
          Q_cu(1) = Q_cu(1) + max(0.d0, utilities(k)%Q)
          Q_hu(1) = Q_hu(1) + abs(min(0.d0, utilities(k)%Q))
        endif
      enddo
      
      results%cuc = Q_cu(1) * expen%fac_cu
      results%huc = Q_hu(1) * expen%fac_hu

      tac = results%ac + results%huc + results%cuc + results%pen
      results%tac = tac
      global_pen = 0.d0

    end function tac

end module simulator_without_split
