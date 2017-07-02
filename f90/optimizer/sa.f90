module simulated_annealing
    use random_numbers
    implicit none
    integer(kind=4) :: i, j, k

    contains
        subroutine test(n_x, limit, y)
            integer(kind=4) :: n_x
            real(kind=8) :: limit(n_x, 2), x_min(n_x), x(n_x)
            real(kind=8), external :: y
            
            integer(kind=4), parameter :: L_low=10000000
            real(kind=8), parameter :: al=0.9d0, ep_low=1.d0
            real(kind=8) :: T_0 = 10000.d0
            real(kind=8) :: fmin=1.d20, f, dx1, dx2

            do i=1, n_x
                x(i) = limit(i, 1) + rand_hit()*(limit(i, 2)-limit(i, 1))
            enddo
            do while(T_0 > ep_low)
                do k=1, L_low
                    f = y(x)
                    if(f < fmin) then
                        fmin=f
                        x_min = x
                        print *, x_min, fmin
                    endif
                    do i=1, n_x
                        x(i) = limit(i, 1) + rand_hit()*(limit(i, 2)-limit(i, 1))
                        ! print *, i, x(i), x(i)-limit(i,1), limit(i,2)-x(i), min(x(i)-limit(i,1), limit(i,2)-x(i), 1.d0)

                        ! dx1 = x(i)-limit(i,1)
                        ! dx2 = limit(i,2)-x(i)
                        ! if(dx1<1.d-5) then
                        !     x(i) = x(i)+ rand_hit()*(limit(i,2)-limit(i,1))*0.1d0
                        ! else if(dx2<1.d-5) then
                        !     x(i) = x(i)- rand_hit()*(limit(i,2)-limit(i,1))*0.1d0
                        ! else
                        !     x(i) = x(i) + (1.d0-2.d0*rand_hit())*min(x(i)-limit(i,1), limit(i,2)-x(i), 0.1d0)
                        ! endif
 
                    enddo
                enddo
                T_0 = T_0*al
                print *, T_0 ,fmin
            enddo
        end subroutine test

end module simulated_annealing
