module hens_def
  use base_def
  use logger
  use hens_config
  use hen
  implicit none

  type(stream),    public :: streams(n_hs+n_cs+n_hu+n_cu)
  type(expense),   public :: expen
  type(cost),      public :: simulated_result 
  real(kind=8),    public :: Q_hu(n_hu)=0.d0, Q_cu(n_cu)=0.d0

  integer(kind=4), target,  public :: ex_hes(n_hs, n_cs, n_st)=0
  real(kind=8),    target,  public :: Q_hes(n_hs, n_cs, n_st)=0.d0
  type(exchanger), target,  public :: hes(n_cs, n_hs, n_st)
  type(utility),   target,  public :: utilities(n_hs+n_cs)
  type(exchanger), pointer, public :: hesp(:)
  type(utility),   pointer, public :: utip(:)
  contains
    subroutine init_case()
      implicit none

      select case(case_name)

        case("case_9sp1") 
          include "case_9sp1.inc"

        case("case_10sp2")
          include "case_10sp2.inc"

        case default
          call log_char("Warning: Input wrong case parameters!")

      end select

      return
    end subroutine init_case

end module hens_def
