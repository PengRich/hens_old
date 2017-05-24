module util
  implicit none
  interface logger
    module procedure log_char
    module procedure log_real
  end interface logger
  contains

    subroutine log_char(message)
      implicit none
      character(len=*) :: message
      
      write(*, *) message
      
    end subroutine log_char

    subroutine log_real(message)
      real(kind=8) :: message
      
      write(*, *) message
      
    end subroutine log_real

end module util
        
module penalty
  implicit none
  real(kind=8), parameter :: pen_factor=1.d7
  real(kind=8), public    :: global_pen = 0.d0
  contains

    subroutine add_penalty(excess)

      real(kind=8), intent(in) :: excess

      global_pen = global_pen + 0.5d0*pen_factor*(max(0.d0, -excess+1.d0)**2.d0)

    end subroutine add_penalty

end module penalty
