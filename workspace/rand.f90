program randomnumber
      use random_numbers
      integer :: i,j
      real(kind=8) :: r
      do i=1, 1d6
        r = rand_hit()
        ! print *, r

      enddo
      print *, r

      


end program randomnumber
