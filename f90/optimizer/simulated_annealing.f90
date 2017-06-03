module hens_bilevel_sa 
  use optimizer_def
  use simulator_without_split
  implicit none
  contains

    subroutine init_bilevel_sa()
      Q_hes = 0.d0
      call init_tac

    end subroutine init_bilevel_sa

    subroutine optimze_heat_exchange()

    end subroutine optimze_heat_exchange

    subroutine optimize_topo()

    end optimize_topo

    subroutine up_level()
      implicit none

    end subroutine up_level 

    subroutine low_level(y)
      implicit none
      real(kind=8), external :: y
      print *, y()

    end subroutine low_level 

end module hens_bilevel_sa 
