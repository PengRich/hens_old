module sa_base
  use optimizer_def
  use random_numbers
  implicit none
  real(kind=8), private, parameter :: TF=1.d0
 
contains
    subroutine basic_sa(opt, dT, ite, accept_rate, T0, update_x, obj, update_optimium)
      implicit none

      integer(kind=4) :: ite, opt
      real(kind=8)    :: T0, accept_rate, dT

      integer(kind=4) :: accept
      integer(kind=4) :: k0=1
      real(kind=8)    :: p, rate, rn0, conv 
      real(kind=8)    :: rate0=0.d0

      external :: update_x, update_optimium
      real(kind=8), external :: obj 

      rate0 = 0.d0
      y0    = 1.d20
      conv  = real(int(accept_rate*ite))/real(ite)
      rn0   = rand_hit()

      do while(T0 .gt. TF)
        accept = 0
        do k=1, ite

          call update_x(0)
          y = obj()

          call update_optimium()
            
          dy = y - y0 
          if(dy .lt. 0.d0) then
            accept = accept + 1
            call update_x(1)
            y0 = y 
          else
            if(dy/T0 .gt. 50.d0)             cycle 
            if(dexp(-dy/T0) .lt. rand_hit()) cycle 
            accept = accept + 1
            call update_x(1)
          endif
        enddo

        rate = real(accept)/real(ite)
        if(opt .eq. 0) then
          if(abs(rate-accept_rate).le.1.e-6) then
            opt = 1
          else
            if(rate .lt. accept_rate) then
              if(rate0 .lt. accept_rate) then
                T0 = T0 + dT
              else
                T0 = T0 + dT*0.5d0
                dT = dT * 0.5d0 
              endif
            else
              T0 = T0- dT*0.5d0
            endif
            rate0 = rate
            rn = rn0
          endif
        else
          T0 = T0 * 0.9d0
        endif
      enddo

    end subroutine basic_sa 

end module sa_base


module hens_bilevel_sa 
  use optimizer_hens_def
  use simulator_without_split
  use sa_base
  use hen_generator 
  implicit none
  !==============================================================
  ! parameter setting
  !==============================================================
  integer(kind=4), private, parameter :: ite_up=n_he, ite_low=500
  real(kind=4),    private, parameter :: TF=1.d0

  real(kind=8), private :: T0_low=1.d4, T0_up=3.d4
  real(kind=8), private :: accept_low=0.95d0, accept_up=0.95d0
  real(kind=8), private :: p_switch=0.3d0
  contains
    !==============================================================
    ! bilevel sa
    !==============================================================
    subroutine run_bilevel_sa()
      call init_bilevel_sa()
      call random_topo((5, 5, n_s+1)
      call up_level()
    end subroutine run_bilevel_sa
    !==============================================================
    ! low level sa to optimzie fixed topo
    !==============================================================
    subroutine run_low_level_sa()

    end subroutine run_low_level_sa
    !==============================================================
    ! initialize bilevel sa 
    !==============================================================
    subroutine init_bilevel_sa()
      Q_hes = 0.d0

    end subroutine init_bilevel_sa
    !==============================================================
    ! up level sa 
    !==============================================================
    subroutine up_level()
             
    end subroutine up_level
    !==============================================================
    ! low level sa 
    !==============================================================
    subroutine low_level()
             
    end subroutine low_level
    !==============================================================
    ! update Q 
    !==============================================================
    subroutine update_Q()

    end subroutine update_Q
    !==============================================================
    ! update global tac 
    !==============================================================
    subroutine update_optimal_tac()

    end subroutine update_optimal_tac

end module hens_bilevel_sa 



module test_bi_level
  implicit none
end module test_bi_level
