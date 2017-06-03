module hen 
  implicit none

  type, public :: stream
    real(kind=8)    :: T_in, T_out, HCpF, h
    integer(kind=4) :: tag ! 0: normal stream, 1: hot utility, 2: cold utitlity  
  end type stream

  type, public :: exchanger
    type(stream) :: hs, cs, hu, cu
    real(kind=8) :: T_hin, T_hout
    real(kind=8) :: T_cin, T_cout
    real(kind=8) :: K_he, K_hs_hu, K_hs_cu, K_cs_hu, K_cs_cu
    real(kind=8) :: A_he, A_hu, A_cu

    integer(kind=4) :: ex, tag, h_id, c_id

    real(kind=8), pointer :: Q
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
