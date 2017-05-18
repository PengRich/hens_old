module base_def
  implicit none
  integer(kind=4) i, j, k, id
end module base_def


module hens_def
  use util
  use base_def
  use hen
  implicit none
  character(len=100), public, parameter :: case_name = "case_10sp2"
  integer(kind=4),    public, parameter :: n_hs=6, n_cs=4, n_st=4, n_hu=1, n_cu=1
  integer(kind=4),    public, parameter :: n_he=n_hs*n_cs*n_st

  type(stream),       public :: streams(n_hs+n_cs+n_hu+n_cu)
  type(expense),      public :: expen
  type(cost),         public :: results 
  type(exchanger),    public :: hes(n_hs, n_cs, n_st)
  type(utility),      public :: utilities(n_hs+n_cs)

  real(kind=8),       public :: Q_hes(n_he)=0.d0
  real(kind=8),       public :: Q_hu(n_hu)=0.d0, Q_cu(n_cu)=0.d0
 
  contains
    subroutine init 
      implicit none

      select case(case_name)
        case("case_9sp1") 
          include "case_9sp1.inc"
        case("case_10sp2")
          include "case_10sp2.inc"
        case default
          call logger("Warning: Input wrong case parameters!")
      end select

      return
    end subroutine init 

end module hens_def