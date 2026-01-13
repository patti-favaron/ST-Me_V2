module ST_Me_V2

    use descriptor

    implicit none
    private

    public :: process
  
contains

    function process(tDescr, sFileName) result(iRetCode)
    
        ! Routine arguments
        type(descriptor_type), intent(in)   :: tDescr
        character(len=*), intent(in)        :: sFileName
        integer                             :: iRetCode
        
        ! Locals
        
        ! Assume success (will falsify on failure)
        iRetCode = 0
        
    end function process

end module ST_Me_V2
