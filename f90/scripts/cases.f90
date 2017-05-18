module case_9sp1
  ! define
  use base_def
  use hens_def 
  ! models
  use hen
  implicit none
  type(stream),  public :: streams(11)
  type(expense), public :: cost
  type(exchanger), public :: he(n_he)
  contains
    subroutine case_data

      n_hs=4
      n_cs=5
      n_hu=1
      n_cu=1

      streams(1) = stream(327, 40,  100, 0.5,  0)
      streams(2) = stream(220, 160, 160, 0.4,  0)
      streams(3) = stream(220, 60,  60,  0.14, 0)
      streams(4) = stream(160, 45,  400, 0.3,  0)
      streams(5) = stream(100, 300, 100, 0.35, 0)
      streams(6) = stream(35,  164, 70,  0.7,  0)
      streams(7) = stream(85,  138, 350, 0.5,  0)
      streams(8) = stream(60,  170, 60,  0.14, 0)
      streams(9) = stream(140, 300, 200, 0.6,  0)

      ! utility
      streams(10) = stream(330, 250, 0, 0.5, 1)
      streams(11) = stream(15,  30,  0, 0.5, 2)

      cost = expense(6, 60, 2000, 70, 1)

      do id=1, n_he
        k = id - (id/21)*20
        i = k/5 + min(1, mod(k, 5))
        j = k - (i-1)*5
        ! he(id)%K_ex = streams(i)%h * streams(n_hs+j)%h / (streams(i)%h + streams(n_hs+j)%h)
        he(id)%hs = streams(i)
        he(id)%cs = streams(j)
        he(id)%hu = streams(10)
        he(id)%cu = streams(11)
        he(id)%Q = 0.d0
        he(id)%A = 0.d0
      enddo

    end subroutine case_data

end module case_9sp1
