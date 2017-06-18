module optimizer_def
  use base_def
  implicit none
  real(kind=8), public ::  y, y0, dy

end module optimizer_def


module optimizer_hens_def
  use optimizer_variable
  use hens_def
  implicit none

  type :: optimium
    real(kind=8)    :: tac
    integer(kind=4) :: sizes(n_s)
    integer(kind=4) :: loc(n_s, max(n_cs, n_hs)*n_st)
  end type optimium 

  type(real_x), pointer, public :: Qs(:)
  type(optimium),        public :: optimal_result

end module optimizer_hens_def
