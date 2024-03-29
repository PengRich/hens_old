!********************************
! heat exchanger network 
!********************************
module hen 
    implicit none

    type, public :: stream
        real(kind=8)    :: T_in, T_out, HCpF, h
        integer(kind=4) :: tag ! 0: normal stream, 1: hot utility, 2: cold utitlity  
        real(kind=8)    :: potential
    end type stream

    type, public :: exchanger
        type(stream) :: hs, cs, hu, cu
        real(kind=8) :: T_hin, T_hout
        real(kind=8) :: T_cin, T_cout
        real(kind=8) :: K_he, K_hs_hu, K_hs_cu, K_cs_hu, K_cs_cu
        real(kind=8) :: A_he, A_hu, A_cu

        integer(kind=4) :: ex_hu, ex_cu
        integer(kind=4) :: tag, h_id, c_id

        integer(kind=4), pointer :: ex_he
        real(kind=8), pointer    :: Q !=>null()
    end type exchanger

    type, public :: utility
        type(stream)    :: s, hu, cu
        real(kind=8)    :: T_in, T_out
        real(kind=8)    :: K_hu, K_cu
        real(kind=8)    :: Q, A
        integer(kind=4) :: ex 
    end type utility

    type, public :: expense
        real(kind=8) :: fac_cu, fac_hu
        real(kind=8) :: fix_A, fac_A, ex_A 
    end type expense

    type, public :: cost 
        real(kind=8) :: ac, huc, cuc, tac, pen
    end type cost 

end module hen 


module hens_def
    use base_def
    use settings 
    use hen
    implicit none

    type(stream),    public :: streams(n_hs+n_cs+n_hu+n_cu)
    type(expense),   public :: expen
    type(cost),      public :: simulated_result 
    real(kind=8),    public :: Q_hu(n_hu)=0.d0, Q_cu(n_cu)=0.d0

    integer(kind=4), target,  public :: ex_hes(n_hs, n_cs, n_st)=0
    real(kind=8),    target,  public :: Q_hes(n_hs, n_cs, n_st)=0.d0
    type(exchanger), target,  public :: hes(n_cs, n_hs, n_st)
    type(utility),   target,  public :: utilities(n_hs+n_cs)
    type(exchanger), pointer, public :: hesp(:)
    type(utility),   pointer, public :: utip(:)
    contains
        subroutine init_case()
            implicit none

            select case(case_name)

                case("case_9sp1") 
                    include "case_9sp1.inc"

                case("case_10sp2")
                    include "case_10sp2.inc"

                case default
                    call log_char("Warning: Input wrong case parameters!")

            end select
            return
        end subroutine init_case

end module hens_def
