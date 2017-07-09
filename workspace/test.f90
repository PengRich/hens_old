program test
    use random_numbers
    integer :: i
    real(kind=8) :: r
    
    do i=1, 1d8
    r = rand_hit()

    enddo
    print *, r




      end program test
