module hen 
  implicit none

  type, public :: stream
    real(kind=8)    :: T_in, T_out, HCpF, h
    integer(kind=4) :: style ! 0: normal stream, 1: hot utility, 2: cold utitlity  
  end type stream

  type, public :: exchanger
    type(stream)    :: hs, cs, hu, cu
    real(kind=8)    :: T_hin, T_hout
    real(kind=8)    :: T_cin, T_cout
    real(kind=8)    :: K_he, K_hs_hu, K_hs_cu, K_cs_hu, K_cs_cu
    real(kind=8)    :: Q, A_he, A_hu, A_cu
    integer(kind=4) :: ex, tag
  end type exchanger

  type, public :: utility
    type(stream)    :: s, hu, cu
    real(kind=8)    :: T_in, T_out
    real(kind=8)    :: K_hu, K_cu
    real(kind=8)    :: Q, A
    integer(kind=4) :: ex 

  end type utility

  type, public :: expense
    real(kind=8) :: fac_cu, fac_hu
    real(kind=8) :: fix_A, fac_A, ex_A 
  end type expense

  type, public :: cost 
    real(kind=8) :: ac, huc, cuc, tac, pen
  end type cost 

end module hen 


module flexible_utility
  use hen
  use penalty
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

    function flexible_he(he) result(new)
      implicit none
      type(exchanger) :: he
      type(exchanger) :: new

      if(he%Q .gt. 0.d0) then
        dtl = he%T_hin - he%T_cout
        dtr = he%T_hout - he%T_cin
        if(dtl.gt.0.d0 .and. dtr.gt.0.d0) then
          he%A_he = cal_he_area(he%Q, he%K_he)
        else
          dtl = he%T_hin - he%cu%T_out 
          dtr = he%T_hout - he%cu%T_in 
          if(dtl.gt.0.d0 .and. dtr.gt.0.d0) then
            he%A_cu = cal_he_area(he%Q, he%K_hs_cu)
          else
            call add_penalty(dtl)
            call add_penalty(dtr)
          endif

          dtl = he%hu%T_in - he%T_cout 
          dtr = he%hu%T_out - he%T_cin
          if(dtl.gt.0.d0 .and. dtr.gt.0.d0) then
            he%A_hu = cal_he_area(he%Q, he%K_cs_hu)
          else
            call add_penalty(dtl)
            call add_penalty(dtr)
          endif
        endif

      else

        dtl = he%T_cin - he%T_hout
        dtr = he%T_cout - he%T_hin
        if(dtl.gt.0.d0 .and. dtr.gt.0.d0) then
          he%A_he = cal_he_area(abs(he%Q), he%K_he)
        else
          dtl = he%T_cin - he%cu%T_out 
          dtr = he%T_cout - he%cu%T_in 
          if(dtl.gt.0.d0 .and. dtr.gt.0.d0) then
            he%A_cu = cal_he_area(abs(he%Q), he%K_cs_cu)
          else
            call add_penalty(dtl)
            call add_penalty(dtr)
          endif

          dtl = he%hu%T_in - he%T_hout 
          dtr = he%hu%T_out - he%T_hin
          if(dtl.gt.0.d0 .and. dtr.gt.0.d0) then
            he%A_hu = cal_he_area(abs(he%Q), he%K_hs_hu)
          else
            call add_penalty(dtl)
            call add_penalty(dtr)
          endif
        endif

      endif

      new = he
      return

      end function flexible_he 

end module flexible_utility
