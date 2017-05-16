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
    real(kind=8)    :: K_ex 
    real(kind=8)    :: Q, A
    integer(kind=4) :: parent, child
    integer(kind=4) :: ex
  end type exchanger

  type, public :: expense 
    real(kind=8) :: fac_cu, fac_hu
    real(kind=8) :: fix_A, fac_A, ex_A 
  end type expense 

end module hen 
