!********************************
! initilization functions 
!********************************
! module hens_init
!   use hens_def
!   use logger
!   implicit none
!   contains
!     subroutine init_case()
!       implicit none
! 
!       select case(case_name)
! 
!         case("case_9sp1") 
!           include "case_9sp1.inc"
! 
!         case("case_10sp2")
!           include "case_10sp2.inc"
! 
!         case default
!           call log_char("Warning: Input wrong case parameters!")
! 
!       end select
! 
!       return
!     end subroutine init_case
!     
!   subroutine init_value()
!     implicit none
! 
!     global_pen = 0.d0
!     Q_hu = 0.d0
!     Q_cu = 0.d0
!   end subroutine init_value
! 
! end module hens_init

!********************************
! thermal calculation of single heat exchanger
!********************************
module heat_exchanger_calculation
  implicit none
  real(kind=8) :: dtl, dtr
  contains

    function cal_he_area(Q, K_he) result(A)
      implicit none

      real(kind=8), intent(in) :: Q, K_he
      real(kind=8) :: dtm, A

      if(abs(dtl-dtr) .gt. 1.d-5) then
        dtm = (dtl-dtr)/log(dtl/dtr)
      else
        dtm = (dtl+dtr)/2.d0
      endif
      A = Q/dtm/K_he
 
    end function cal_he_area

end module heat_exchanger_calculation
 
!********************************
! thermal calculation of flexible utility 
!********************************
module flexible_utility_base
  use heat_exchanger_calculation
  use hen
  use penalty
  implicit none

  contains

    subroutine cal_teriminal_he(he)
      implicit none
      type(utility), target :: he

      if(he%Q .gt. 0.d0) then

        dtl = he%T_in - he%cu%T_out
        dtr = he%T_out - he%cu%T_in
        if(dtl.gt.0.d0 .and. dtr.gt.0.d0) then
          he%A = cal_he_area(he%Q, he%K_cu)
        else
          call add_penalty(dtl)
          call add_penalty(dtr)
        endif

      else
        dtl = he%hu%T_in - he%T_out
        dtr = he%hu%T_out - he%T_in
        if(dtl.gt.0.d0 .and. dtr.gt.0.d0) then
          he%A = cal_he_area(abs(he%Q), he%K_hu)
        else
          call add_penalty(dtl)
          call add_penalty(dtr)
        endif
      endif

      return

    end subroutine cal_teriminal_he 

    subroutine cal_internal_he(he)
      implicit none
      type(exchanger), target :: he

      he%ex_he = 0
      he%ex_hu = 0
      he%ex_cu = 0

      if(he%Q .gt. 0.d0) then
        dtl = he%T_hin - he%T_cout
        dtr = he%T_hout - he%T_cin
        if(dtl.gt.0.d0 .and. dtr.gt.0.d0) then
          he%A_he  = cal_he_area(he%Q, he%K_he)
          he%ex_he = 1
        else
          dtl = he%T_hin - he%cu%T_out 
          dtr = he%T_hout - he%cu%T_in 
          if(dtl.gt.0.d0 .and. dtr.gt.0.d0) then
            he%A_cu  = cal_he_area(he%Q, he%K_hs_cu)
            he%ex_cu = 1
          else
            call add_penalty(dtl)
            call add_penalty(dtr)
          endif

          dtl = he%hu%T_in - he%T_cout 
          dtr = he%hu%T_out - he%T_cin
          if(dtl.gt.0.d0 .and. dtr.gt.0.d0) then
            he%A_hu  = cal_he_area(he%Q, he%K_cs_hu)
            he%ex_hu = 1
          else
            call add_penalty(dtl)
            call add_penalty(dtr)
          endif
        endif

      else

        dtl = he%T_cin - he%T_hout
        dtr = he%T_cout - he%T_hin
        if(dtl.gt.0.d0 .and. dtr.gt.0.d0) then
          he%A_he  = cal_he_area(abs(he%Q), he%K_he)
          he%ex_he = 1
        else
          dtl = he%T_cin - he%cu%T_out 
          dtr = he%T_cout - he%cu%T_in 
          if(dtl.gt.0.d0 .and. dtr.gt.0.d0) then
            he%A_cu  = cal_he_area(abs(he%Q), he%K_cs_cu)
            he%ex_cu = 1
          else
            call add_penalty(dtl)
            call add_penalty(dtr)
          endif

          dtl = he%hu%T_in - he%T_hout 
          dtr = he%hu%T_out - he%T_hin
          if(dtl.gt.0.d0 .and. dtr.gt.0.d0) then
            he%A_hu  = cal_he_area(abs(he%Q), he%K_hs_hu)
            he%ex_hu = 1
          else
            call add_penalty(dtl)
            call add_penalty(dtr)
          endif
        endif

      endif

      return

      end subroutine cal_internal_he 

end module flexible_utility_base
