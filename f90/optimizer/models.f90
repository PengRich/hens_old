module optimizer_variable
  implicit none

  type, public :: real_x
    real(kind=8), pointer :: x
    real(kind=8) :: up_bound, low_bound
  end type real_x

  type, public :: int_x
    integer(kind=4), pointer :: x
    integer(kind=4) :: up_bound, low_bound
  end type int_x

end module optimizer_variable

module optimizer_def
  use optimizer_variable
  implicit none

  type(real_x), pointer :: real_xs(:)

  type(int_x), pointer :: int_xs(:)

end module optimizer_def
