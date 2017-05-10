module case_9sp1
  ! define
  use hens_def 
  ! models
  use hen
  implicit none
  integer(kind=4), public, parameter :: n_he=20*n_st
  type(stream),  public :: streams(9)
  type(utility), public :: utilities(2)
  type(expense), public :: cost
  type(exchanger), public :: he(20, n_st)
  contains
    subroutine case_data

      n_hs=4
      n_cs=5
      n_hu=1
      n_cu=1

      streams(1) = stream(327, 40,  100, 0.5)
      streams(2) = stream(220, 160, 160, 0.4)
      streams(3) = stream(220, 60,  60,  0.14)
      streams(4) = stream(160, 45,  400, 0.3)
      streams(5) = stream(100, 300, 100, 0.35)
      streams(6) = stream(35,  164, 70,  0.7)
      streams(7) = stream(85,  138, 350, 0.5)
      streams(8) = stream(60,  170, 60,  0.14)
      streams(9) = stream(140, 300, 200, 0.6)

      utilities(1) = utility(330, 250, 0.5, 0)
      utilities(2) = utility(15,  30,  0.5, 1)

      cost = expense(6, 60, 2000, 70, 1)

    end subroutine case_data



end module case_9sp1
