module hen 
  implicit none

  integer(kind=4), public :: n_hs, n_cs
  integer(kind=4), public :: n_hu, n_cu

  type, public :: stream
    real(kind=8) :: T_in, T_out, HCpF, h
  end type stream

  type, public :: utility
    real(kind=8)    :: T_in, T_out, h
    integer(kind=4) :: typ ! typ=0 is hot utility, type=1 is cold utility
  end type utility

  type, public :: exchanger
    real(kind=8) :: T_hin, T_hout
    real(kind=8) :: T_cin, T_cout
    real(kind=8) :: k 
    real(kind=8) :: Q, A
  end type exchanger

  type, public :: expense 
    real(kind=8) :: fac_cu, fac_hu
    real(kind=8) :: fix_A, fac_A, ex_A 
  end type expense 

end module hen 
