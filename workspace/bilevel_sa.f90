program test_sa
      use simulated_annealing
      implicit none
      integer(kind=4), parameter :: n_x=2
      real(kind=8) :: limit(n_x, 2), x(n_x)
      real(kind=8), external :: ackley 
      real(kind=8), external :: bukin_n6
      real(kind=8), external :: drop_wave 
      real(kind=8), external :: eggholder 
      real(kind=8), external :: griewank 
      real(kind=8), external :: rastrigin 
      real(kind=8), external :: schaffer_n2
      real(kind=8), external :: schaffer_n4 
      real(kind=8), external :: schwefel 

      x(1) = 0.d0 
      x(2) = 0.d0 
      print *,"ackley",  ackley(x), 0
      print *, "result", x, 0
      limit(1,1) = -32.768d0
      limit(1,2) = 32.768d0
      limit(2,1) = -32.768d0
      limit(2,2) = 32.768d0
      call test(2, limit, ackley, 0.d0)
      print *, "ackley"

      x(1) = -10.d0
      x(2) = 1.d0
      print *, "bukin_n6", bukin_n6(x), 0
      print *, "result", x, 0
      limit(1,1) = -15.d0
      limit(1,2) = -5.d0
      limit(2,1) = -3.d0
      limit(2,2) = 3.d0
      call test(2, limit, bukin_n6, 0.d0)
      print *, "bukin_n6"
 
      x(1) = 0.d0 
      x(2) = 0.d0 
      print *,"drop_wave",  drop_wave(x), -1 
      print *, "result", x, -1 
      limit(1,1) = -5.120
      limit(1,2) = 5.12d0
      limit(2,1) = -5.12d0
      limit(2,2) = 5.12d0
      call test(2, limit, drop_wave, -1.d0)
      print *, "drop_wave"

      x(1) = 512.d0 
      x(2) = 404.2319d0 
      print *,"eggholder",  eggholder(x), -959.6407
      print *, "result", x, -959.6407
      limit(1,1) = -512.d0
      limit(1,2) = 512.d0
      limit(2,1) = -512.d0
      limit(2,2) = 512.d0
      call test(2, limit, eggholder, -959.6407d0)
      print *, "eggholder"
 
      x(1) = 0.d0 
      x(2) = 0.d0 
      print *,"griewank",  griewank(x), 0 
      print *, "result", x, 0
      limit(1,1) = -600.d0
      limit(1,2) = 600.d0
      limit(2,1) = -600.d0
      limit(2,2) = 600.d0
      call test(2, limit, griewank, 0.d0)
      print *, "griewank"

      x(1) = 0.d0 
      x(2) = 0.d0 
      print *,"rastrigin",  rastrigin(x), 0 
      print *, "result", x, 0
      limit(1,1) = -5.120
      limit(1,2) = 5.12d0
      limit(2,1) = -5.12d0
      limit(2,2) = 5.12d0
      call test(2, limit, rastrigin, 0.d0)
      print *, "rastrigin"

      x(1) = 0.d0 
      x(2) = 0.d0 
      print *,"schaffer_n2",  schaffer_n2(x), 0 
      print *, "result", x, 0
      limit(1,1) = -100.d0
      limit(1,2) = 100.d0 
      limit(2,1) = -100.d0
      limit(2,2) = 100.d0
      call test(2, limit, schaffer_n2, 0.d0)
      print *, "schaffer_n2"

      x(1) = 0.d0 
      x(2) = 1.25313d0 
      print *,"schaffer_n4",  schaffer_n4(x), 0.292579
      print *, "result", x, 0.292579
      limit(1,1) = -100.d0
      limit(1,2) = 100.d0 
      limit(2,1) = -100.d0
      limit(2,2) = 100.d0
      call test(2, limit, schaffer_n4, 0.292579d0)
      print *, "schaffer_n4"

      x(1) = 420.9687d0
      x(2) = 420.9687d0
      print *, schwefel(x), 0
      print *, "result", x, 0
      limit(1,1) = -500.d0
      limit(1,2) = 500.d0
      limit(2,1) = -500.d0
      limit(2,2) = 500.d0
      call test(2, limit, schwefel, 0.d0)
      print *, "schwefel"
       
end program test_sa


real(kind=8) function ackley(x)
    ! x_i = [-32.768,32.768]
    ! y = 0, x=(0,0)
    implicit none
    integer :: i
    real(kind=8) :: x(2), y1, y2

    ackley = 20.d0 + exp(1.d0)
    y1 = 0.d0
    y2 = 0.d0
    do i=1, 2
        y1 = y1+ x(i)**2.d0
        y2 = y2 + cos(2*3.141592653d0*x(i))
    enddo
    ackley = ackley -20.d0 * exp(-0.2d0*sqrt(1.d0/2.d0*y1)) - exp(1.d0/2.d0*y2)
end function ackley


real(kind=8) function bukin_n6(x)
    ! x1 = [-15,-5]
    ! x2 = [-3,3]
    ! y=0, x=(-10,1)
    implicit none
    real(kind=8) :: x(2)

    bukin_n6 = 100.d0*sqrt(abs(x(2)-0.01d0*x(1)**2.d0))+0.01d0*abs((x(1)+10.d0))
end function bukin_n6


real(kind=8) function drop_wave(x)
    ! x_i = [-5.12, 5.12]
    ! y=-1, x=(0,0)
    implicit none
    integer :: i
    real(kind=8) :: x(2), y1, y2

    y1 = x(1)**2.d0+x(2)**2.d0
    drop_wave = -((1+cos(12.d0*sqrt(y1))) / (0.5d0*y1+2.d0))
end function drop_wave


real(kind=8) function eggholder(x)
    ! xi ∈ [−512, 512]
    ! f(x∗) = −959.6407, x∗ = (512,404.2319)
    implicit none
    real(kind=8) x(2), y1, y2, y3
    y1 = -(x(2)+47.d0)
    y2 = sin(sqrt(abs(x(2)+x(1)/2.d0+47.d0))) 
    y3 = -x(1)*sin(sqrt(abs(x(1)-x(2)-47.d0)))

    eggholder = y1*y2 + y3

end function eggholder


real(kind=8) function griewank(x)
    ! xi ∈ [−600, 600]
    ! f(x∗) = 0, x∗ = (0,...,0)
    implicit none
    integer(kind=4) i
    real(kind=8) x(2), y1, y2, y3

    y1 = 0
    y2 = 1
    do i=1, 2
        y1 = y1 + x(i)**2.d0/4000.d0
        y2 = y2*cos(x(i)/sqrt(real(i)))
    enddo

    griewank = y1-y2+1.d0

end function griewank


real(kind=8) function rastrigin(x)
    ! xi ∈ [−5.12, 5.12]
    ! f(x∗) = 0, x∗ = (0,...,0)
    implicit none
    integer(kind=4) i
    real(kind=8) x(2), y1, y2, y3
    
    y1 = 0.d0
    do i=1, 2
        y1 = y1 + x(i)**2.d0 - 10.d0*cos(2.d0*3.141592653d0*x(i))
    enddo

    rastrigin = 10.d0*2.d0 + y1

end function rastrigin


real(kind=8) function schaffer_n2(x)
    !  xi ∈ [−100, 100]
    ! f(x∗) = 0, x∗ = (0,0)
    implicit none
    integer(kind=4) i
    real(kind=8) x(2), y1, y2, y3

    y2 = sin(x(1)**2.d0-x(2)**2.d0)**2.d0
    y3 = (1.d0+0.001d0*(x(1)**2.d0+x(2)**2.d0))**2.d0

    schaffer_n2 = 0.5d0 + (y2-0.5d0)/y3
    
end function schaffer_n2


real(kind=8) function schaffer_n4(x)
    !  xi ∈ [−100, 100]
    ! f(x∗) = 0.292579, x∗ = (0,1.25313)
    implicit none
    integer(kind=4) i
    real(kind=8) x(2), y1, y2, y3
    y1 = sin(abs(x(1)**2.d0-x(2)**2.d0))
    y2 = cos(y1)**2.d0
    y3 = (1.d0+0.001d0*(x(1)**2.d0+x(2)**2.d0))**2.d0

    schaffer_n4 = 0.5d0 + (y2-0.5d0)/y3
    
end function schaffer_n4


real(kind=8) function schwefel(x)
    ! xi ∈ [−500, 500]
    ! f(x∗)=0, x∗ =(420.9687,...,420.9687)
    implicit none
    integer(kind=4) i
    real(kind=8) x(2), y1, y2, y3

    y1 = 0.d0
    do i=1, 2
        y1 = y1 + x(i) * sin(sqrt(abs(x(i))))
    enddo

    schwefel = 418.9829d0*2.d0 - y1
end function schwefel
