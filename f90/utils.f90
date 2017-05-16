module util
  implicit none
  contains
    subroutine logger(message)
      character(len=*) :: message
      
      write(*, *) message
      
    end subroutine logger
end module util
        
