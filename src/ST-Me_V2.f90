module ST_Me_V2

  implicit none
  private

  public :: say_hello
  
contains

  subroutine say_hello
    print *, "Hello, ST-Me_V2!"
  end subroutine say_hello
  
end module ST_Me_V2
