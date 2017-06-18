module optimizer_variable
  implicit none
  real(kind=8) :: real_y, int_y

  type, public :: real_x
    real(kind=8), pointer :: x 
    real(kind=8)          :: up, low
  end type real_x

  type, public :: int_x
    integer(kind=4), pointer :: x 
    integer(kind=4)          :: up, low
  end type int_x

end module optimizer_variable
