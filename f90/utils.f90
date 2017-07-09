!********************************
! make formatted logs 
!********************************
module logger 
  implicit none
  ! interface logger
  !   module procedure log_char
  !   module procedure log_real
  ! end interface logger
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

end module logger 

!********************************
! simplified penalty function 
!********************************
module penalty
    implicit none
    real(kind=8) :: global_pen = 0.d0
    contains

        subroutine add_penalty(excess)
            real(kind=8), intent(in) :: excess
            real(kind=8) :: factor

            factor     = 1.d7
            global_pen = global_pen + 0.5d0*factor*(max(0.d0, -excess+1.d0)**2.d0)
            return 
        end subroutine add_penalty

        real(kind=8) function mpen(excess)
            real(kind=8), intent(in) :: excess
            real(kind=8) :: factor

            factor = 1.d7
            mpen   = 0.5d0*factor*(max(0.d0, -excess+1.d0)**2.d0)
            return 
        end function mpen 
end module penalty


!********************************
! kinds of random numbers
!********************************
module random_numbers
  implicit none
  real(kind=8)  :: rn=0.1d0
  contains
    real(kind=8) function rand_hit()
      implicit none
	  real(kind=8) :: ax, am, ac

      if(rn .ne. 0.0d0) then
        ac = dble(16807)
        rn = ac * rn
        rn = rn - dble(idint(rn))
      else
        ax = dble(8388607)
        am = dble(2147483647)
        rn = ax / am
      endif
      rand_hit = rn
      return
    end function rand_hit
end module random_numbers
