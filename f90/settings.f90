!********************************
! basic config 
!********************************
module settings 
  implicit none
  character(len=100), public, parameter :: case_name = "case_10sp2"
  integer(kind=4),    public, parameter :: n_hs=6, n_cs=4, n_st=4, n_hu=1, n_cu=1
  integer(kind=4),    public, parameter :: n_he=n_hs*n_cs*n_st, n_s=n_hs+n_cs
end module settings
