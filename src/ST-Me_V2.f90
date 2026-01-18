! Main module: here is where things really happen in the ST-Me :)
!
! =============================================================================
!
! MIT License
!
! Copyright (c) 2026 Patrizia Favaron
!
! Permission is hereby granted, free of charge, to any person obtaining a copy
! of this software and associated documentation files (the "Software"), to deal
! in the Software without restriction, including without limitation the rights
! to use, copy, modify, merge, publish, distribute, sublicense, and/or sell
! copies of the Software, and to permit persons to whom the Software is
! furnished to do so, subject to the following conditions:
!
! The above copyright notice and this permission notice shall be included in all
! copies or substantial portions of the Software.
!
! THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
! IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,
! FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE
! AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER
! LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM,
! OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE
! SOFTWARE.
!
module ST_Me_V2

    use descriptor
    use data_set

    implicit none
    private

    public :: process
  
contains

    function process(tDescr, sFileName, iTimeStep) result(iRetCode)
    
        ! Routine arguments
        type(descriptor_type), intent(in)   :: tDescr
        character(len=*), intent(in)        :: sFileName
        integer, intent(in)                 :: iTimeStep
        integer                             :: iRetCode
        
        ! Locals
        character(len=256)  :: sOutName
        character(len=256)  :: sReportName
        integer             :: iErrCode
        type(data_set_type) :: tSet
        
        ! Assume success (will falsify on failure)
        iRetCode = 0
        
        ! Try reading the data set
        iErrCode = tSet % load(sFileName, tDescr)
        if(iErrCode /= 0) then
            iRetCode = 1
            return
        end if
        ! Data set found
        
        ! Print summary
        iErrCode = tSet % summarize(tDescr)
        if(iErrCode /= 0) then
            iRetCode = 2
            return
        end if
        
        ! Check time stamp regularity
        iErrCode = tSet % check_time()
        select case(iErrCode)
        case(1)
            print *, "ST-Me:: error: Data set does not begin on an exact day boundary"
            iRetCode = 3
            return
        case(2)
            print *, "ST-Me:: error: Data set does not end on an exact day boundary"
            iRetCode = 4
            return
        case(3)
            print *, "ST-Me:: error: Data set has not equally spaced time stamps"
            iRetCode = 5
            return
        end select
        
        ! Fill mandatory data (wind components, temperature and relative humidity)
        iErrCode = tSet % fill_mandatory()
        if(iErrCode /= 0) then
            print *, "ST-Me:: error: Fill procedure failed with return code ", iErrCode
            iRetCode = 6
            return
        end if
        
        ! Fill any other data
        iErrCode = tSet % fill_radiation_turbulence(tDescr)
        if(iErrCode /= 0) then
            print *, "ST-Me:: error: Estimation procedure failed with return code ", iErrCode
            iRetCode = 7
            return
        end if
        
        ! Write to file
        sOutName = trim(sFileName) // ".cpf"
        sReportName = trim(sFileName) // ".rpt"
        iErrCode = tSet % write_to_calpuff(tDescr, sOutName, sReportName)
        if(iErrCode /= 0) then
            print *, "ST-Me:: error: Write to Calpuff failed with return code ", iErrCode
            iRetCode = 8
            return
        end if
        
    end function process

end module ST_Me_V2
