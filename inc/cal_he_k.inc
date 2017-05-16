do id=1, n_he
  k = id - (id/21)*20
  i = k/5 + min(1, mod(k, 5))
  j = k - (i-1)*5
  ! he(id)%K_ex = streams(i)%h * streams(n_hs+j)%h / (streams(i)%h + streams(n_hs+j)%h)
  he(id)%hs = streams(i)
  he(id)%cs = streams(j)
  he(id)%hu = streams(10)
  he(id)%cu = streams(11)
  he(id)%Q = 0.d0
  he(id)%A = 0.d0
enddo
