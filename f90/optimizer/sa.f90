module simulated_annealing
    use random_numbers
    implicit none
    integer(kind=4) :: i, j, k

    contains
        subroutine test(n_x, limit, y, y0)
            integer(kind=4) :: n_x, ite, n_x0
            real(kind=8) :: limit(n_x, 2), x_min(n_x), x(n_x)
            real(kind=8), external :: y
            
            integer(kind=4), parameter :: L_low=1000000
            real(kind=8), parameter :: al=0.9d0, ep_low=1.d0, beta=0
            real(kind=8) :: T0 = 10000.d0
            real(kind=8) :: fmin, f, dx1, dx2, T_0, r, y0
           
            do ite=1, 5 
            T_0 = T0
            fmin=1.d20
            do i=1, n_x
                x(i) = limit(i, 1) + rand_hit()*(limit(i, 2)-limit(i, 1))
                ! call random_number(r)
                ! x(i) = limit(i, 1) + r*(limit(i, 2)-limit(i, 1))
            enddo
            do while(T_0 > ep_low)
                do k=1, L_low
                    f = y(x)
                    if(f < fmin) then
                        fmin=f
                        x_min = x
            ! print *, ite, x_min, fmin
                    endif
                    do i=1, n_x
                        ! call random_number(r)
                        if(rand_hit() > beta) then
                        ! if(r>beta) then
                            ! call random_number(r)
                            x(i) = limit(i, 1) + rand_hit()*(limit(i, 2)-limit(i, 1))
                            ! x(i) = limit(i, 1) + r*(limit(i, 2)-limit(i, 1))
                        else
                            ! call random_number(r)
                            if(rand_hit() > 0.5) then
                            ! if(r>0.5) then
                                x(i) = limit(i, 1)
                            else
                                x(i) = limit(i, 2)
                            endif
                        endif
 
                    enddo
                enddo
                T_0 = T_0*al
                ! print *, T_0 ,fmin
            enddo
            print *, ite, x_min, fmin-y0
            enddo
            
            print *, "***************************"
            do ite=1, 5 
            T_0 = T0
            fmin=1.d20
            do i=1, n_x
                x(i) = limit(i, 1) + rand_hit()*(limit(i, 2)-limit(i, 1))
                ! call random_number(r)
                ! x(i) = limit(i, 1) + r*(limit(i, 2)-limit(i, 1))
            enddo
            do while(T_0 > ep_low)
                do k=1, L_low
                    f = y(x)
                    if(f < fmin) then
                        fmin=f
                        x_min = x
                        ! print *, x_min, fmin
                    endif
                    do i=1, n_x
                        ! call random_number(r)
                        ! if(r > beta) then
                        if(rand_hit() > beta) then
                        dx1 = x(i)-limit(i,1)
                        dx2 = limit(i,2)-x(i)
                        if(dx1<1.d-5) then
                            ! call random_number(r)
                            ! x(i) = x(i)+ r*(limit(i,2)-limit(i,1))*0.1d0
                            x(i) = x(i)+ rand_hit()*(limit(i,2)-limit(i,1))*0.1d0
                        else if(dx2<1.d-5) then
                            ! call random_number(r)
                            ! x(i) = x(i)- r*(limit(i,2)-limit(i,1))*0.1d0
                            x(i) = x(i)- rand_hit()*(limit(i,2)-limit(i,1))*0.1d0
                        else
                            ! call random_number(r)
                            ! x(i) = x(i) + (1.d0-2.d0*r)*min(x(i)-limit(i,1), limit(i,2)-x(i), 0.1d0)
                            x(i) = x(i) + (1.d0-2.d0*rand_hit())*min(x(i)-limit(i,1), limit(i,2)-x(i), 0.1d0)

                        endif
                        else
                            ! call random_number(r)
                            ! if(r > 0.5) then
                            if(rand_hit() > 0.5) then
                                x(i) = limit(i, 1)
                            else
                                x(i) = limit(i, 2)
                            endif
                        endif


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
                ! print *, T_0 ,fmin
            enddo
 
            print *, ite, x_min, fmin-y0
            enddo


        end subroutine test

end module simulated_annealing
