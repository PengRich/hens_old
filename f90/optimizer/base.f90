module hen_generator
  use hens_def
  use random_numbers
  contains
    subroutine random_topo(max_hs, max_cs, expected_hes)
      integer(kind=4), intent(in) :: max_hs, max_cs
      ! integer, optional   :: expected_hes
      integer(kind=4), intent(in) :: expected_hes
      
      integer(kind=4) :: n_he0, limit_cs(n_cs), limit_hs(n_hs), ex_hes(n_he)
      real(kind=8) :: px

      px     = real(expected_hes)/real(n_he)
      ex_hes = 0
      n_he0  = 0
      do id=1, n_he
        if(rand_hit() .lt. px) n_he0 = n_he0 + 1
      enddo

      limit_cs = 0
      limit_hs = 0
      do k=1, n_he0
100     i = int(rand_hit()*n_hs)+1 
        if(limit_hs(i).gt.max_hs) goto 100
        limit_hs(i) = limit_hs(i) + 1

200     j = int(rand_hit()*n_cs)+1 
        if(limit_cs(i).gt.max_cs) goto 200
        limit_cs(i) = limit_cs(i) + 1

        id = int(rand_hit()*n_st)*n_hs*n_cs+(i-1)*n_cs+j
        if(ex_hes(id).eq.1) then
          limit_hs(i) = limit_hs(i) - 1
          limit_cs(i) = limit_cs(i) - 1
          goto 100
        endif
        ex_hes(id) = 1
      enddo 

      sizes = 0
      do k=1, n_st
        do i=1, n_hs
          do j=1, n_cs
            id = (k-1)*n_hs*n_cs + (i-1)*n_cs + j
            if(ex_hes(id) .eq. 1) then 
              hesp(id)%ex   = 1
              sizes(i)      = sizes(i)      + 1
              sizes(n_hs+j) = sizes(n_hs+j) + 1

              loc(i, sizes(i))           = id 
              loc(n_hs+j, sizes(n_hs+j)) = id 
            else
               hesp(id)%ex   = 0
            endif
          enddo
        enddo
      enddo
 
    end subroutine random_topo

    subroutine rand_heat_duty()
      
    end subroutine rand_heat_duty

end module topo_generator
