! ST-Me version 2
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
program main

    use descriptor
    use ST_Me_V2

    implicit none
    
    ! Locals
    character(len=256)      :: sFileDescr
    character(len=256)      :: sFileName
    type(descriptor_type)   :: tDescr
    integer                 :: iRetCode
    integer                 :: i
    
    ! Get command line args and validate them
    if(command_argument_count() < 1) then
        print *, "ST-Me - Version 2 - Ultrasonic anemometer aware met processor"
        print *
        print *, "Usage:"
        print *
        print *, "  ./ST-Me <Site_Descriptor> [<Data_File_1> <Data_File_2> <...> <Data_File_N>]"
        print *
        print *, "If no <Data_file> is specified, the program just reads the site descriptor"
        print *, "file and prints it to screen."
        print *
        print *, "Copyright 2026 by Patrizia Favaron"
        print *, "This program is covered by the MIT license"
        print *
        stop
    end if
    call get_command_argument(1, sFileDescr)
    
    ! Get descriptor
    iRetCode = tDescr % load(sFileDescr)
    if(iRetCode /= 0) then
        print "('Invalid descriptor - Return code = ', i2)", iRetCode
        stop
    end if
    
    ! If this is a diagnostic run, dump the descriptor file
    if(command_argument_count() == 1) then
        iRetCode = tDescr % dump()
        stop
    end if
    
    ! Process all files in turn
    do i = 2, command_argument_count()
    
        ! Get file name and try processing it
        call get_command_argument(i, sFileName)
        iRetCode = process(tDescr, sFileName, tDescr % iTimeStep)
        if(iRetCode /= 0) then
            print "('File ',a,' was not processed. Return code = ',i3)", trim(sFileName), iRetCode
        else
            print "('File ',a,' processed.')", trim(sFileName)
        end if
        
    end do

end program main
