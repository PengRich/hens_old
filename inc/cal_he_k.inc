do k=1, n_st
  do i=1, n_hs
    do j=1, n_cs
      hes(i,j,k)%hs = streams(i)
      hes(i,j,k)%cs = streams(n_hs+j)
      hes(i,j,k)%hu = streams(n_hs+n_cs+1)
      hes(i,j,k)%cu = streams(n_hs+n_cs+2)
      hes(i,j,k)%Q = 0.d0
      hes(i,j,k)%A_he = 0.d0
      hes(i,j,k)%A_hu = 0.d0
      hes(i,j,k)%A_cu = 0.d0
      hes(i,j,k)%ex = 0 

      hes(i,j,k)%K_he = streams(i)%h*streams(n_hs+j)%h / (streams(i)%h+streams(n_hs+j)%h) 
      hes(i,j,k)%K_hs_cu = streams(i)%h*streams(n_hs+n_cs+2)%h / (streams(i)%h+streams(n_hs+n_cs+2)%h) 
      hes(i,j,k)%K_hs_hu = streams(i)%h*streams(n_hs+n_cs+1)%h / (streams(i)%h+streams(n_hs+n_cs+1)%h)
      hes(i,j,k)%K_cs_hu = streams(n_hs+j)%h*streams(n_hs+n_cs+1)%h / (streams(n_hs+j)%h+streams(n_hs+n_cs+1)%h)
      hes(i,j,k)%K_cs_cu = streams(n_hs+j)%h*streams(n_hs+n_cs+2)%h / (streams(n_hs+j)%h+streams(n_hs+n_cs+2)%h)
    enddo
  enddo
enddo

do i=1, n_hs
  utilities(i)%s  = streams(i)
  utilities(i)%T_out = streams(i)%T_out
  utilities(i)%hu = streams(n_hs+n_cs+1)
  utilities(i)%cu = streams(n_hs+n_cs+2)
  utilities(i)%Q  = 0.d0
  utilities(i)%A  = 0.d0
  utilities(i)%ex = 0 
  utilities(i)%K_cu = streams(i)%h*streams(n_hs+n_cs+2)%h / (streams(i)%h+streams(n_hs+n_cs+2)%h)
  utilities(i)%K_hu = streams(i)%h*streams(n_hs+n_cs+1)%h / (streams(i)%h+streams(n_hs+n_cs+1)%h)
enddo

do j=1, n_cs
  utilities(n_hs+j)%s  = streams(n_hs+j)
  utilities(n_hs+j)%T_out = streams(n_hs+j)%T_out
  utilities(n_hs+j)%hu = streams(n_hs+n_cs+1)
  utilities(n_hs+j)%cu = streams(n_hs+n_cs+2)
  utilities(n_hs+j)%Q  = 0.d0
  utilities(n_hs+j)%A  = 0.d0
  utilities(n_hs+j)%ex = 0 
  utilities(n_hs+j)%K_cu = streams(n_hs+j)%h*streams(n_hs+n_cs+2)%h / (streams(n_hs+j)%h+streams(n_hs+n_cs+2)%h)
  utilities(n_hs+j)%K_hu = streams(n_hs+j)%h*streams(n_hs+n_cs+1)%h / (streams(n_hs+j)%h+streams(n_hs+n_cs+1)%h)
enddo
