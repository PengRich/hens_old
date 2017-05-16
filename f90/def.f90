module base_def
  implicit none
  integer(kind=4) i, j, k, id
end module base_def


module hens_def
  use util
  use base_def
  use hen
  implicit none
  character(len=100), public, parameter :: case_name = "case_9sp1"
  integer(kind=4),    public, parameter :: n_hs=4, n_cs=5, n_st=3, n_hu=1, n_cu=1
  integer(kind=4),    public, parameter :: n_he=n_hs*n_cs*n_st

  type(stream),       public :: streams(n_hs+n_cs+n_hu+n_cu)
  type(expense),      public :: cost
  type(exchanger),    public :: he(n_he)
 
  contains
    subroutine init 
      implicit none

      select case(case_name)
        case("case_9sp1") 
          include "case_9sp1.inc"
        case default
          call logger("Warning: Input wrong case parameters!")
      end select

    end subroutine init 

end module hens_def
