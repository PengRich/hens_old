module simulator_without_split
  use hens_def
  use flexible_utility
  implicit none
  ! integer(kind=4), target, private :: he_size(n_hs+n_cs)=0
  ! integer(kind=4), pointer, private :: h, c 
  integer(kind=4), private :: he_size(n_hs+n_cs)=0
  type(exchanger), private :: involved_hes(n_hs+n_cs, n_he)
  type(exchanger), allocatable, private :: internal_hes(:)

  contains
    !***************************************************************
    ! calculate inlet and outlet temperature of each heat exchanger
    !***************************************************************
    subroutine cal_unit_temp
      implicit none
      integer(kind=4) :: ite=1

      allocate(internal_hes(sum(he_size)/2))

      do i=1, n_hs

        if(he_size(i).eq.0) cycle

        involved_hes(i, 1)%T_hin  = streams(i)%T_in
        involved_hes(i, 1)%T_hout = involved_hes(i, 1)%T_hin - &
            involved_hes(i, 1)%Q/involved_hes(i, 1)%hs%HCpF
        internal_hes(ite) = involved_hes(i, 1)
        ite = ite+1

        if(he_size(i).eq.1) cycle

        do k=2, he_size(i) 
          involved_hes(i, k)%T_hin  = involved_hes(i, k-1)%T_hout
          involved_hes(i, k)%T_hout = involved_hes(i, k)%T_hin - &
              involved_hes(i, k)%Q/involved_hes(i, k)%hs%HCpF
          internal_hes(ite) = involved_hes(i, k)
          ite = ite+1
        enddo
      enddo

      do j=1, n_cs

        i = n_hs+j
        if(he_size(i).eq.0) cycle

        involved_hes(i, he_size(i))%T_cin  = streams(i)%T_in
        involved_hes(i, he_size(i))%T_cout = involved_hes(i, he_size(i))%T_cin + &
            involved_hes(i, he_size(i))%Q/involved_hes(i, he_size(i))%cs%HCpF

        if(he_size(i).eq.1) cycle

        do k=he_size(i)-1, 1, -1
          involved_hes(i, k)%T_cin  = involved_hes(i, k+1)%T_cout
          involved_hes(i, k)%T_cout = involved_hes(i, k)%T_cin + &
              involved_hes(i, k)%Q/involved_hes(i, k)%cs%HCpF
        enddo
      enddo

      do j=1, n_cs
        i = n_hs+j
        if(he_size(i).eq.0) cycle
        do k=1, he_size(i)
          do id=1, sum(he_size)/2
            if(internal_hes(id)%tag .eq. involved_hes(i, k)%tag) then
              internal_hes(id)%T_cin = involved_hes(i, k)%T_cin
              internal_hes(id)%T_cout = involved_hes(i, k)%T_cout
            endif
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

      do k=1, sum(he_size)/2 
        internal_hes(k) = flexible_he(internal_hes(k))
      enddo

      return
    end subroutine cal_internal_area_utility

    !*************************************
    ! calculate area of terminal utility
    !*************************************
    subroutine cal_terminal_area_utility
      implicit none

      real(kind=8) :: Q

      call cal_internal_area_utility

      do i=1, n_hs
        if(he_size(i).eq.0) then
          utilities(i)%T_in = streams(i)%T_in
        else
          utilities(i)%T_in = involved_hes(i, he_size(i))%T_hout
        endif
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
        i = n_hs + j
        if(he_size(i).eq.0) then
          utilities(i)%T_in = streams(i)%T_in
        else
          utilities(i)%T_in = involved_hes(i, 1)%T_cout
        endif
        write(24,*) i, utilities(i)%T_in
        Q = (utilities(i)%T_in-utilities(i)%T_out)*utilities(i)%s%HCpF

        if(abs(Q) .gt. 1.d-3) then 
          utilities(i)%ex = 1
        else
          utilities(i)%ex = 0
        endif
        utilities(i)%Q  = Q
        utilities(i)%A  = 0.d0 
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

    subroutine init_tac(Qs)
      implicit none
      real(kind=8) :: Qs(n_he)
        
      results%ac  = 0.d0
      results%huc = 0.d0
      results%cuc = 0.d0
      results%tac = 0.d0
      results%pen = global_pen 

      do k=1, n_st
        do i=1, n_hs
          do j=1, n_cs

            id = (k-1)*n_hs*n_cs + (i-1)*n_cs + j

            if(abs(Qs(id)) .lt. 1.d-3) cycle 
              hes(i,j,k)%ex = 1
              hes(i,j,k)%Q  = Qs(id)
              hes(i,j,k)%A_he  = 0.d0 
              hes(i,j,k)%A_hu  = 0.d0 
              hes(i,j,k)%A_cu  = 0.d0 


              he_size(i)      = he_size(i) + 1
              he_size(n_hs+j) = he_size(n_hs+j) + 1
              ! h => he_size(i)
              ! c => he_size(n_hs+j)

              involved_hes(i, he_size(i))       = hes(i,j,k)
              involved_hes(i, he_size(i))%tag = id 
              involved_hes(n_hs+j, he_size(n_hs+j))     = hes(i,j,k)
              involved_hes(n_hs+j, he_size(n_hs+j))%tag = id 

          enddo
        enddo
      enddo

      return

    end subroutine init_tac

    real(kind=8) function tac(Qs)
      implicit none
      real(kind=8) :: Qs(n_he)
      
      call init_tac(Qs)
      call cal_terminal_area_utility

      do k=1, sum(he_size)/2 

        if(internal_hes(k)%A_he .gt. 1.d-3) then
          results%ac = results%ac + expen%fix_A + &
              expen%fac_A*internal_hes(k)%A_he**expen%ex_A
        else
          if(internal_hes(k)%A_hu .gt. 1.d-3) then
            Q_hu(1) = Q_hu(1) + abs(internal_hes(k)%Q)
            results%ac = results%ac + expen%fix_A + &
                expen%fac_A*internal_hes(k)%A_hu**expen%ex_A
          endif
          if(internal_hes(k)%A_cu .gt. 1.d-3) then
            Q_cu(1) = Q_cu(1) + abs(internal_hes(k)%Q)
            results%ac = results%ac + expen%fix_A + &
                expen%fac_A*internal_hes(k)%A_cu**expen%ex_A
          endif
        endif

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
