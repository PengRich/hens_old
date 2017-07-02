program test_sa
      use simulated_annealing
      implicit none
      real(kind=8), external :: bukin, ackley, drop_wave
      real(kind=8) :: limit(2, 2)
      real(kind=8) :: x(2) = 0.d0, y1
      x(1) = 0 
      x(2) = 0 
      limit(1,1) = -5.12d0
      limit(1,2) = 5.12d0
      limit(2,1) = -5.12d0
      limit(2,2) = 5.12d0
      print *, drop_wave(x)
      do i=1, 2
      print *, limit(i, 1), limit(i, 2)
      enddo
      call test(2, limit, drop_wave)
 
      ! do i =1, 5
      ! limit(i, 1) = -32.768d0
      ! limit(i, 2) = 32.768d0
      ! enddo


end program test_sa

real(kind=8) function bukin(x)
      implicit none
      real(kind=8) :: x(2)

      bukin  = 100.d0*sqrt(abs(x(2)-0.01d0*x(1)**2.d0))+0.01d0*abs((x(1)+10.d0))
end function bukin

real(kind=8) function ackley(x)
      implicit none
      integer :: i
      real(kind=8) :: x(5), y1, y2
      ackley = 20.d0 + exp(1.d0)
      y1 = 0.d0
      y2 = 0.d0
      do i=1, 5
        y1 = y1+ x(i)**2.d0
        y2 = y2 + cos(2*3.141592653d0*x(i))
      enddo
      ackley = ackley -20.d0 * exp(-0.2d0*sqrt(1.d0/5.d0*y1)) - exp(1.d0/5.d0*y2)
      end function ackley

real(kind=8) function drop_wave(x)
      implicit none
      integer :: i
      real(kind=8) :: x(2), y1, y2

      y1 = x(1)**2.d0+x(2)**2.d0
      drop_wave = -((1+cos(12.d0*sqrt(y1))) / (0.5d0*y1+2.d0))

end function drop_wave
