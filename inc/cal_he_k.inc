do k=1, n_st
  do i=1, n_hs
    do j=1, n_cs
      hes(j,i,k)%hs      = streams(i)
      hes(j,i,k)%cs      = streams(n_hs+j)
      hes(j,i,k)%hu      = streams(n_s+1)
      hes(j,i,k)%cu      = streams(n_s+2)
      hes(j,i,k)%A_he    = 0.d0
      hes(j,i,k)%A_hu    = 0.d0
      hes(j,i,k)%A_cu    = 0.d0
      hes(j,i,k)%h_id    = i 
      hes(j,i,k)%c_id    = j 
      hes(j,i,k)%K_he    =      streams(i)%h * streams(n_hs+j)%h /      (streams(i)%h + streams(n_hs+j)%h) 
      hes(j,i,k)%K_hs_cu =      streams(i)%h * streams(n_s+2)%h  /      (streams(i)%h + streams(n_s+2)%h) 
      hes(j,i,k)%K_hs_hu =      streams(i)%h * streams(n_s+1)%h  /      (streams(i)%h + streams(n_s+1)%h)
      hes(j,i,k)%K_cs_hu = streams(n_hs+j)%h * streams(n_s+1)%h  / (streams(n_hs+j)%h + streams(n_s+1)%h)
      hes(j,i,k)%K_cs_cu = streams(n_hs+j)%h * streams(n_s+2)%h  / (streams(n_hs+j)%h + streams(n_s+2)%h)
      hes(j,i,k)%Q       => Q_hes(i,j,k)
      hes(j,i,k)%ex_he   => ex_hes(i,j,k)
    enddo
  enddo
enddo

hesp(1:size(hes)) => hes(:,:,:)

do i=1, n_hs
  utilities(i)%s     = streams(i)
  utilities(i)%T_out = streams(i)%T_out
  utilities(i)%hu    = streams(n_s+1)
  utilities(i)%cu    = streams(n_s+2)
  utilities(i)%Q     = 0.d0
  utilities(i)%A     = 0.d0
  utilities(i)%ex    = 0 
  utilities(i)%K_cu  = streams(i)%h*streams(n_s+2)%h / (streams(i)%h+streams(n_s+2)%h)
  utilities(i)%K_hu  = streams(i)%h*streams(n_s+1)%h / (streams(i)%h+streams(n_s+1)%h)
  if(streams(i)%T_out .le. streams(n_s+2)%T_out) then
    streams(i)%potential = streams(i)%HCpF * (streams(i)%T_in-(streams(n_s+2)%T_out+1.d0))
  else
    streams(i)%potential = streams(i)%HCpF * (streams(i)%T_in-streams(i)%T_out)
  endif
enddo

do j=1, n_cs
  i = n_hs+j
  utilities(i)%s     = streams(i)
  utilities(i)%T_out = streams(i)%T_out
  utilities(i)%hu    = streams(n_s+1)
  utilities(i)%cu    = streams(n_s+2)
  utilities(i)%Q     = 0.d0
  utilities(i)%A     = 0.d0
  utilities(i)%ex    = 0 
  utilities(i)%K_cu  = streams(i)%h*streams(n_s+2)%h / (streams(i)%h+streams(n_s+2)%h)
  utilities(i)%K_hu  = streams(i)%h*streams(n_s+1)%h / (streams(i)%h+streams(n_s+1)%h)
  if(streams(i)%T_out .ge. streams(n_s+1)%T_out) then
    streams(i)%potential = streams(i)%HCpF * ((streams(n_s+1)%T_out-1.d0)-streams(i)%T_in)
  else
    streams(i)%potential = streams(i)%HCpF * (streams(i)%T_out-streams(i)%T_in)
  endif
enddo

utip(1:size(utilities)) => utilities(:)
