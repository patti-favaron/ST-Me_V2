! data_set module.
!
! =============================================================================
!
! MIT License
!
! Copyright (c) 2023 Patrizia Favaron
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
module data_set

    use types
    use DateTime
    use descriptor
    use estimate

    implicit none
    
    private
    
    ! Public interface
    public  :: data_set_type
    
    ! Data types
    
    type data_set_type
        character(len=256)  :: sFileName
        character(len=20)   :: sFirstDateTime
        character(len=20)   :: sLastDateTime
        integer             :: iDeltaTime
        integer, dimension(:), allocatable  :: ivTimeStamp
        real(dp), dimension(:), allocatable :: rvVel
        real(dp), dimension(:), allocatable :: rvDir
        real(dp), dimension(:), allocatable :: rvU
        real(dp), dimension(:), allocatable :: rvV
        real(dp), dimension(:), allocatable :: rvTemp
        real(dp), dimension(:), allocatable :: rvRelH
        real(dp), dimension(:), allocatable :: rvPrec
        real(dp), dimension(:), allocatable :: rvN
        real(dp), dimension(:), allocatable :: rvRg
        real(dp), dimension(:), allocatable :: rvRn
        real(dp), dimension(:), allocatable :: rvUstar
        real(dp), dimension(:), allocatable :: rvH0
        real(dp), dimension(:), allocatable :: rvZLm1
        real(dp), dimension(:), allocatable :: rvG0
        real(dp), dimension(:), allocatable :: rvHe
        real(dp), dimension(:), allocatable :: rvLalt1
        real(dp), dimension(:), allocatable :: rvTstar
        real(dp), dimension(:), allocatable :: rvZi
        integer, dimension(:), allocatable  :: ivLstab
        logical, dimension(:), allocatable  :: lvVel
        logical, dimension(:), allocatable  :: lvDir
        logical, dimension(:), allocatable  :: lvU
        logical, dimension(:), allocatable  :: lvV
        logical, dimension(:), allocatable  :: lvTemp
        logical, dimension(:), allocatable  :: lvRelH
        logical, dimension(:), allocatable  :: lvPrec
        logical, dimension(:), allocatable  :: lvRg
        logical, dimension(:), allocatable  :: lvRn
        logical, dimension(:), allocatable  :: lvUstar
        logical, dimension(:), allocatable  :: lvH0
    contains
        procedure   :: clean
        procedure   :: load
        procedure   :: summarize
        procedure   :: check_time
        procedure   :: fill_mandatory   ! U, V, Vel, Dir, Temp, RelH
        procedure   :: fill_radiation_turbulence
        procedure   :: write_to_calpuff
    end type data_set_type
    
contains

    function clean(me) result(iRetCode)
    
        ! Routine arguments
        class(data_set_type), intent(inout) :: me
        integer                             :: iRetCode
        
        ! Locals
        ! -- None --
        
        ! Assume success (will falsify on failure)
        iRetCode = 0
        
        ! Clean out data set
        me % sFileName      = ''
        me % sFirstDateTime = ''
        me % sLastDateTime  = ''
        me % iDeltaTime     = 0
        if(allocated(me % ivTimeStamp)) deallocate(me % ivTimeStamp)
        if(allocated(me % rvVel))   deallocate(me % rvVel)
        if(allocated(me % rvDir))   deallocate(me % rvDir)
        if(allocated(me % rvU))     deallocate(me % rvU)
        if(allocated(me % rvV))     deallocate(me % rvV)
        if(allocated(me % rvTemp))  deallocate(me % rvTemp)
        if(allocated(me % rvRelH))  deallocate(me % rvRelH)
        if(allocated(me % rvPrec))  deallocate(me % rvPrec)
        if(allocated(me % rvRg))    deallocate(me % rvRg)
        if(allocated(me % rvRn))    deallocate(me % rvRn)
        if(allocated(me % rvUstar)) deallocate(me % rvUstar)
        if(allocated(me % rvH0))    deallocate(me % rvH0)
        if(allocated(me % rvHe))    deallocate(me % rvHe)
        if(allocated(me % rvG0))    deallocate(me % rvG0)
        if(allocated(me % rvLalt1)) deallocate(me % rvLalt1)
        if(allocated(me % ivLstab)) deallocate(me % ivLstab)
        if(allocated(me % rvTstar)) deallocate(me % rvTstar)
        if(allocated(me % lvVel))   deallocate(me % lvVel)
        if(allocated(me % lvDir))   deallocate(me % lvDir)
        if(allocated(me % lvU))     deallocate(me % lvU)
        if(allocated(me % lvV))     deallocate(me % lvV)
        if(allocated(me % lvTemp))  deallocate(me % lvTemp)
        if(allocated(me % lvRelH))  deallocate(me % lvRelH)
        if(allocated(me % lvPrec))  deallocate(me % lvPrec)
        if(allocated(me % lvRg))    deallocate(me % lvRg)
        if(allocated(me % lvRn))    deallocate(me % lvRn)
        if(allocated(me % lvUstar)) deallocate(me % lvUstar)
        if(allocated(me % lvH0))    deallocate(me % lvH0)
        if(allocated(me % rvZi))    deallocate(me % rvZi)
    end function clean
    
    
    function load(me, sFileName, tDescr) result(iRetCode)
    
        ! Routine arguments
        class(data_set_type), intent(inout) :: me
        character(len=*), intent(in)        :: sFileName
        type(descriptor_type), intent(in)   :: tDescr
        integer                             :: iRetCode
        
        ! Locals
        integer             :: iLUN
        integer             :: iErrCode
        character(len=256)  :: sBuffer
        integer             :: iTimeStamp
        integer             :: iFromTime
        integer             :: iToTime
        integer             :: i
        integer             :: j
        integer             :: iNumData
        
        ! Constants (please do not change... :) )
        real(dp), parameter :: pi = 4.d0 * atan(1.d0)
        
        ! Assume success (will falsify on failure)
        iRetCode = 0
        
        ! First read pass: get time limits
        open(newunit=iLUN, file=sFileName, action='read', status='old', iostat=iErrCode)
        if(iErrCode /= 0) then
            iRetCode = 1
            return
        end if
        read(iLUN, "(a)", iostat=iErrCode) sBuffer  ! Skip header line
        if(iErrCode /= 0) then
            iRetCode = 2
            return
        end if
        me % iDeltaTime = tDescr % iTimeStep
        me % sFirstDateTime = "9999-99-99 99:99:99" ! Lexicographic datetime max
        me % sLastDateTime  = "0000-00-00 00:00:00" ! Lexicographic datetime min
        do
            read(iLUN, "(a)", iostat=iErrCode) sBuffer
            if(iErrCode /= 0) exit
            if(sBuffer(1:19) < me % sFirstDateTime) me % sFirstDateTime = sBuffer(1:19)
            if(sBuffer(1:19) > me % sLastDateTime)  me % sLastDateTime = sBuffer(1:19)
        end do
        iFromTime = toEpoch(fromString(me % sFirstDateTime))
        iToTime   = toEpoch(fromString(me % sLastDateTime))
        iNumData  = (iToTime - iFromTime) / me % iDeltaTime + 1
        if(iNumData <= 0) then
            iRetCode = 3
            close(iLUN)
            return
        end if
        
        ! Reserve data space
        allocate(me % ivTimeStamp(iNumData))
        allocate(me % rvVel(iNumData))
        allocate(me % rvDir(iNumData))
        allocate(me % rvU(iNumData))
        allocate(me % rvV(iNumData))
        allocate(me % rvTemp(iNumData))
        allocate(me % rvRelH(iNumData))
        allocate(me % rvPrec(iNumData))
        allocate(me % rvN(iNumData))
        allocate(me % rvRg(iNumData))
        allocate(me % rvRn(iNumData))
        allocate(me % rvUstar(iNumData))
        allocate(me % rvH0(iNumData))
        allocate(me % rvHe(iNumData))
        allocate(me % rvG0(iNumData))
        allocate(me % rvLalt1(iNumData))
        allocate(me % ivLstab(iNumData))
        allocate(me % rvZLm1(iNumData))
        allocate(me % rvTstar(iNumData))
        allocate(me % rvZi(iNumData))
        allocate(me % lvVel(iNumData))
        allocate(me % lvDir(iNumData))
        allocate(me % lvU(iNumData))
        allocate(me % lvV(iNumData))
        allocate(me % lvTemp(iNumData))
        allocate(me % lvRelH(iNumData))
        allocate(me % lvPrec(iNumData))
        allocate(me % lvRg(iNumData))
        allocate(me % lvRn(iNumData))
        allocate(me % lvUstar(iNumData))
        allocate(me % lvH0(iNumData))
        
        ! Initialize vectors
        do i = 1, iNumData
            me % ivTimeStamp(i) = iFromTime + me % iDeltaTime * (i-1)
        end do
        ! -1- Data from file
        me % rvVel   = -9999.9d0
        me % rvDir   = -9999.9d0
        me % rvTemp  = -9999.9d0
        me % rvRelH  = -9999.9d0
        me % rvPrec  = -9999.9d0
        me % rvRg    = -9999.9d0
        me % rvRn    = -9999.9d0
        me % rvUstar = -9999.9d0
        me % rvH0    = -9999.9d0
        me % lvVel   = .false.
        me % lvDir   = .false.
        me % lvTemp  = .false.
        me % lvRelH  = .false.
        me % lvPrec  = .false.
        me % lvRg    = .false.
        me % lvRn    = .false.
        me % lvUstar = .false.
        me % lvH0    = .false.
        ! -1- Computed data
        me % rvU     = -9999.9d0
        me % rvV     = -9999.9d0
        me % lvU     = .false.
        me % lvV     = .false.
        ! -1- Estimated data
        me % rvHe    = -9999.9d0
        me % rvG0    = -9999.9d0
        me % rvLalt1 = -9999.9d0
        me % ivLstab = -9999
        me % rvTstar = -9999.9d0
        me % rvZi    = -9999.9d0
        
        ! Second read pass: get actual data and place them in data space
        rewind(iLUN)
        read(iLUN, "(a)") sBuffer
        do
        
            ! Get a data line, and infer its positional index from time stamp
            read(iLUN, "(a)", iostat=iErrCode) sBuffer
            if(iErrCode /= 0) exit
            iTimeStamp = toEpoch(fromString(sBuffer(1:19)))
            i = (iTimeStamp - iFromTime) / me % iDeltaTime + 1
            if(i < 1 .or. i > iNumData) cycle
            ! Post: index in range
            
            ! Gather data line
            do j = 21, len_trim(sBuffer)
                if(sBuffer(j:j) == ',') sBuffer(j:j) = ' '
            end do
            read(sBuffer(21:),*,iostat=iErrCode) &
                me % rvVel(i), me % rvDir(i), me % rvTemp(i), &
                me % rvRelH(i), &
                me % rvPrec(i), &
                me % rvRg(i), me % rvRn(i), &
                me % rvUstar(i), me % rvH0(i)
            if(iErrCode == 0) then
                ! Validate values
                if(0.0d0 <= me % rvVel(i) .and. me % rvVel(i) <=  60.0d0) me % lvVel(i) = .true.
                if(0.0d0 <= me % rvDir(i) .and. me % rvDir(i) <  360.0d0) me % lvDir(i) = .true.
                if(-40.0d0 <= me % rvTemp(i) .and. me % rvTemp(i) <= 60.0d0) me % lvTemp(i) = .true.
                if(0.0d0 <= me % rvRelH(i) .and. me % rvRelH(i) <= 110.0d0) then
                    me % lvRelH(i) = .true.
                    me % rvRelH(i) = min(100.d0, me % rvRelH(i))
                end if
                if(0.0d0 <= me % rvPrec(i)) me % lvPrec(i) = .true.
                if(-10.0d0 <= me % rvRg(i) .and. me % rvRg(i) <= 1500.0d0) me % lvRg(i) = .true.
                if(-10.0d0 <= me % rvRg(i) .and. me % rvRg(i) <= 0.0d0) me % rvRg(i) = 0.d0
                if(-600.0d0 <= me % rvRn(i) .and. me % rvRn(i) <= 1500.0d0) me % lvRn(i) = .true.
                if(-600.0d0 <= me % rvH0(i) .and. me % rvH0(i) <= 1500.0d0) me % lvH0(i) = .true.
                if(0.0d0 <= me % rvUstar(i)) me % lvUstar(i) = .true.
            end if
            
            ! Compute / validate U and V components
            do i = 1, iNumData
                if(me % lvVel(i) .and. me % lvDir(i)) then
                    me % rvU(i) = me % rvVel(i) * sin(pi*me % rvDir(i)/180.d0)
                    me % rvV(i) = me % rvVel(i) * cos(pi*me % rvDir(i)/180.d0)
                    me % lvU(i) = .true.
                    me % lvV(i) = .true.
                end if
            end do
            
        end do
        close(iLUN)
        
        ! Check whether the quantities expected in data really exist: in case any does not
        ! the execution terminates in error
        ! -1- Wind speed and direction (alternatively U and V) must always exist
        if(all(.not. me % lvU) .or. all(.not. me % lvV)) then
            iRetCode = 4
            return
        end if
        ! -1- Temperature must always exist
        if(all(.not. me % lvTemp)) then
            iRetCode = 5
            return
        end if
        ! -1- Relative humidity must always exist
        if(all(.not. me % lvRelH)) then
            iRetCode = 6
            return
        end if
        ! -1- All other quantities must exist only if they have been declared
        !     present in the descriptor file
        if(tDescr % lIsPrec) then
            if(all(.not. me % lvPrec)) then
                iRetCode = 7
                return
            end if
        end if
        if(tDescr % lIsRg) then
            if(all(.not. me % lvRg)) then
                iRetCode = 8
                return
            end if
        end if
        if(tDescr % lIsRn) then
            if(all(.not. me % lvRn)) then
                iRetCode = 9
                return
            end if
        end if
        if(tDescr % lIsH0) then
            if(all(.not. me % lvH0)) then
                iRetCode = 10
                return
            end if
        end if
        if(tDescr % lIsUstar) then
            if(all(.not. me % lvUstar)) then
                iRetCode = 11
                return
            end if
        end if
        
        ! Data which are positively asked for "missing" are cleaned out,
        ! possibly overwriting data which were read as "valid".
        if(.not. tDescr % lIsPrec) then
            me % rvPrec = -9999.9
            me % lvPrec = .false.
        end if
        if(.not. tDescr % lIsRg) then
            me % rvRg = -9999.9
            me % lvRg = .false.
        end if
        if(.not. tDescr % lIsRn) then
            me % rvRn = -9999.9
            me % lvRn = .false.
        end if
        if(.not. tDescr % lIsH0) then
            me % rvH0 = -9999.9
            me % lvH0 = .false.
        end if
        if(.not. tDescr % lIsUstar) then
            me % rvUstar = -9999.9
            me % lvUstar = .false.
        end if
        
    end function load
    
    
    function summarize(me, tDescr) result(iRetCode)
    
        ! Routine arguments
        class(data_set_type), intent(in)    :: me
        type(descriptor_type), intent(in)   :: tDescr
        integer                             :: iRetCode
        
        ! Locals
        character(len=19)   :: sMinTime
        character(len=19)   :: sMaxTime
        real(dp)    :: rAvailU
        real(dp)    :: rMeanU
        real(dp)    :: rStdU
        real(dp)    :: rAvailV
        real(dp)    :: rMeanV
        real(dp)    :: rStdV
        real(dp)    :: rAvailTemp
        real(dp)    :: rMeanTemp
        real(dp)    :: rStdTemp
        real(dp)    :: rAvailRelH
        real(dp)    :: rMeanRelH
        real(dp)    :: rStdRelH
        real(dp)    :: rAvailPrec
        real(dp)    :: rMeanPrec
        real(dp)    :: rStdPrec
        real(dp)    :: rAvailRg
        real(dp)    :: rMeanRg
        real(dp)    :: rStdRg
        real(dp)    :: rAvailRn
        real(dp)    :: rMeanRn
        real(dp)    :: rStdRn
        real(dp)    :: rAvailH0
        real(dp)    :: rMeanH0
        real(dp)    :: rStdH0
        real(dp)    :: rAvailUstar
        real(dp)    :: rMeanUstar
        real(dp)    :: rStdUstar
        integer     :: iNumValid
        
        ! Assume success (will falsify on failure)
        iRetCode = 0
        
        ! Check something can be made
        if(.not. allocated(me % ivTimeStamp)) then
            iRetCode = 1
            return
        end if
        
        ! Compute basic statistics
        sMinTime = toString(fromEpoch(minval(me % ivTimeStamp)))
        sMaxTime = toString(fromEpoch(maxval(me % ivTimeStamp)))
        iNumValid = summary_stats(me % rvU, me % lvU, rAvailU, rMeanU, rStdU)
        iNumValid = summary_stats(me % rvV, me % lvV, rAvailV, rMeanV, rStdV)
        iNumValid = summary_stats(me % rvTemp, me % lvTemp, rAvailTemp, rMeanTemp, rStdTemp)
        iNumValid = summary_stats(me % rvRelH, me % lvRelH, rAvailRelH, rMeanRelH, rStdRelH)
        iNumValid = summary_stats(me % rvPrec, me % lvPrec, rAvailPrec, rMeanPrec, rStdPrec)
        iNumValid = summary_stats(me % rvRg, me % lvRg, rAvailRg, rMeanRg, rStdRg)
        iNumValid = summary_stats(me % rvRn, me % lvRn, rAvailRn, rMeanRn, rStdRn)
        iNumValid = summary_stats(me % rvH0, me % lvH0, rAvailH0, rMeanH0, rStdH0)
        iNumValid = summary_stats(me % rvUstar, me % lvUstar, rAvailUstar, rMeanUstar, rStdUstar)
        
        ! Print statistics
        print *
        print "('Input data summary')"
        print "('==================')"
        print *
        print "('Data from ',a,' to ',a)", sMinTime, sMaxTime
        print *
        print "('Mandatory data:')"
        print "('U  availability: ',f6.2,' %     mean: ',f6.2,' m/s   std: ',f7.3,' m/s')", rAvailU, rMeanU, rStdU
        print "('V  availability: ',f6.2,' %     mean: ',f6.2,' m/s   std: ',f7.3,' m/s')", rAvailV, rMeanV, rStdV
        print "('T  availability: ',f6.2,' %     mean: ',f6.2,' °C    std: ',f7.3,' °C')", rAvailTemp, rMeanTemp, rStdTemp
        print "('RH availability: ',f6.2,' %     mean: ',f6.2,' %     std: ',f7.3,' %')", rAvailRelH, rMeanRelH, rStdRelH
        print *
        print "('Optional data:')"
        if(tDescr % lIsPrec) then
        print "('Pr availability: ',f6.2,' %     mean: ',f6.2,' mm    std: ',f7.3,' mm')", rAvailPrec, rMeanPrec, rStdPrec
        else
        print "('Pr declared missing')"
        end if
        if(tDescr % lIsRg) then
        print "('Rg availability: ',f6.2,' W/m2  mean: ',f6.2,' W/m2  std: ',f7.3,' W/m2')", rAvailRg, rMeanRg, rStdRg
        else
        print "('Rg declared missing')"
        end if
        if(tDescr % lIsRn) then
        print "('Rn availability: ',f6.2,' W/m2  mean: ',f6.2,' W/m2  std: ',f7.3,' W/m2')", rAvailRn, rMeanRn, rStdRn
        else
        print "('Rn declared missing')"
        end if
        if(tDescr % lIsH0) then
        print "('H0 availability: ',f6.2,' W/m2  mean: ',f6.2,' W/m2  std: ',f7.3,' W/m2')", rAvailH0, rMeanH0, rStdH0
        else
        print "('H0 declared missing')"
        end if
        if(tDescr % lIsUstar) then
        print "('u* availability: ',f6.2,' m/s   mean: ',f6.2,' m/s   std: ',f7.3,' m/s')", rAvailUstar, rMeanUstar, rStdUstar
        else
        print "('u* declared missing')"
        end if
        
    end function summarize
    
    
    ! This function restricts the time period so that it begins and ends on
    ! exact time boundaries, conditioned to a number of initial
    function check_time(me) result(iRetCode)
    
        ! Routine arguments
        class(data_set_type), intent(inout) :: me
        integer                             :: iRetCode
        
        ! Locals
        integer     :: iTimeMin
        integer     :: iTimeMax
        integer     :: i
        
        ! Constants (please do not change... :) )
        integer, parameter  :: ONE_HOUR =  3600
        integer, parameter  :: ONE_DAY  =  24*ONE_HOUR
        
        ! Assume success (will falsify on failure)
        iRetCode = 0
        
        ! Check time begins on an exact day boundary
        iTimeMin = minval(me % ivTimeStamp)
        if(mod(iTimeMin, ONE_DAY) /= 0) then
            iRetCode = 1
            return
        end if
        
        ! Check time ends on an exact day boundary minus one time step
        iTimeMax = maxval(me % ivTimeStamp)
        if(mod(iTimeMax + me % iDeltaTime, ONE_DAY) /= 0) then
            iRetCode = 2
            return
        end if
        
        ! Check time stamps form a strictly increasing sequence, with
        ! a spacing equal to the one specified in the descriptor
        do i = 1, size(me % ivTimeStamp)-1
            if(me % ivTimeStamp(i+1) - me % ivTimeStamp(i) /= me % iDeltaTime) then
                iRetCode = 3
                return
            end if
        end do
        
    end function check_time
    
    
    function fill_mandatory(me) result(iRetCode)
    
        ! Routine arguments
        class(data_set_type), intent(inout) :: me
        integer                             :: iRetCode
        
        ! Locals
        integer :: iErrCode
        integer :: i
        
        ! Constants (please do not change... :) )
        real(dp), parameter :: PI = 4.0_dp * atan(1.0_dp)
        
        ! Assume success (will falsify on failure)
        iRetCode = 0
        
        ! Gap-fill wind components
        iErrCode = mean_day_gap_filler(me % ivTimeStamp, me % rvU, me % lvU, me % iDeltaTime, .true.)
        if(iErrCode /= 0) then
            print *, "ST-Me:: error: Gap fill of U component failed with return code = ", iErrCode
            iRetCode = 1
            return
        end if
        iErrCode = mean_day_gap_filler(me % ivTimeStamp, me % rvV, me % lvV, me % iDeltaTime, .true.)
        if(iErrCode /= 0) then
            print *, "ST-Me:: error: Gap fill of V component failed with return code = ", iErrCode
            iRetCode = 2
            return
        end if
        
        ! Generate missing Vel, Dir couples from U, V
        do i = 1, size(me % ivTimeStamp)
            if(.not.me % lvVel(i) .or. .not.me % lvDir(i)) then
            
                ! Wind speed
                me % rvVel(i) = sqrt(me % rvU(i)**2 + me % rvV(i)**2)
                me % lvVel(i) = .true.
                
                ! Wind direction
                me % rvDir(i) = PI / 180.0_dp * atan2(me % rvU(i), me % rvV(i))
                if(me % rvDir(i) < 0.0_dp) me % rvDir(i) = 360.0_dp * me % rvDir(i)
                me % lvDir(i) = .true.
                
            end if
        end do
        
        ! Gap-fill temperature
        iErrCode = mean_day_gap_filler(me % ivTimeStamp, me % rvTemp, me % lvTemp, me % iDeltaTime, .false.)
        if(iErrCode /= 0) then
            print *, "ST-Me:: error: Gap fill of temperature failed with return code = ", iErrCode
            iRetCode = 3
            return
        end if
        where(me % rvTemp < -40.0_dp)
            me % rvTemp = -40.0_dp
        end where
        where(me % rvTemp > 60.0_dp)
            me % rvTemp = 60.0_dp
        end where
        
        ! Gap-fill relative humidity
        iErrCode = mean_day_gap_filler(me % ivTimeStamp, me % rvRelH, me % lvRelH, me % iDeltaTime, .false.)
        if(iErrCode /= 0) then
            print *, "ST-Me:: error: Gap fill of relative humidity failed with return code = ", iErrCode
            iRetCode = 4
            return
        end if
        where(me % rvRelH < 10.0_dp)
            me % rvRelH = 10.0_dp
        end where
        where(me % rvRelH > 100.0_dp)
            me % rvRelH = 100.0_dp
        end where
        
        ! Fill precipitation gaps with zero (this has the same practical effect
        ! of using a mean day gap filler)
        do i = 1, size(me % ivTimeStamp)
            if(.not.me % lvPrec(i)) then
                me % rvPrec(i) = 0.0_dp
                me % lvPrec(i) = .true.
            end if
        end do
    
    end function fill_mandatory
    
    
    ! The 'fill_radiation' function is the core of the "classical" estimation chain
    ! within ST-Me. Notice this part is executed even if a sonic is available.
    function fill_radiation_turbulence(me, tDescr) result(iRetCode)
    
        ! Routine arguments
        class(data_set_type), intent(inout) :: me
        type(descriptor_type), intent(in)   :: tDescr
        integer                             :: iRetCode
        
        ! Locals
        real(dp), dimension(:), allocatable :: rvRe     ! Extraterrestrial radiation (W/m2)
        real(dp), dimension(:), allocatable :: rvRc     ! Clear-sky radiation (W/m2)
        real(dp), dimension(:), allocatable :: rvSe     ! Solar elevation angle (°)
        real(dp), dimension(:), allocatable :: rvFcd    ! Cloudiness factor (dimensionless)
        real(dp), dimension(:), allocatable :: rvTa     ! Air temperature (K)
        real(dp), dimension(:), allocatable :: rvPa     ! Air pressure at site (hPa)
        real(dp), dimension(:), allocatable :: rvEa     ! Partial pressure of water vapor (hPa)
        real(dp), dimension(:), allocatable :: rvEs     ! Saturation pressure of water vapor (hPa)
        real(dp), dimension(:), allocatable :: rvL      ! Obukhov length (m)
        integer                             :: i
        integer                             :: n
        real(dp)                            :: rTempUs
        real(dp)                            :: rTempTs
        real(dp)                            :: rTempH0
        real(dp)                            :: rTempZLm1
        real(dp)                            :: rH0
        real(dp)                            :: rUstar
        real(dp)                            :: rHLM
        real(dp)                            :: rHour
        type(time)                          :: tCurrentTime
        real(dp)                            :: rRc
        real(dp)                            :: rZiMec
        real(dp)                            :: rZiConv
        real(dp)                            :: rSunrise
        real(dp)                            :: rSunset
        real(dp), dimension(2)              :: rvSunRiseSet
        
        ! Assume success (will falsify on failure)
        iRetCode = 0
        
        ! Initialize
        n = size(me % ivTimeStamp)
        allocate(rvRe(n))
        allocate(rvRc(n))
        allocate(rvSe(n))
        allocate(rvFcd(n))
        allocate(rvTa(n))
        allocate(rvPa(n))
        allocate(rvEa(n))
        allocate(rvEs(n))
        allocate(rvL(n))
        
        ! Radiation loop
        do i = 1, n
        
            ! Estimate extraterrestrial and clear sky radiation
            rvRe(i) = ExtraterrestrialRadiation( &
                me % ivTimeStamp(i), &
                tDescr % iTimeStep, &
                tDescr % rLat, &
                tDescr % rLon, &
                tDescr % iTimeZone, &
                rvSe(i) &
            )
            rvRc(i) = ClearSkyRadiation(rvRe(i), tDescr % rHeight)
            
            ! Adjust the measured radiation (it can not exceed the clear-sky radiation)
            if(me % lvRg(i) .and. me % rvRg(i) > rvRc(i)) then
                me % rvRg(i) = rvRc(i)
            end if
            
            ! Estimate the cloudiness
            if(me % lvRg(i) .and. rvSe(i) > 15.0_dp) then
                rvFcd(i) = MeasuredCloudiness(me % rvRg(i), rvRc(i))
            else
                rvFcd(i) = EstimatedCloudiness(me % rvRelH(i))
            end if
            
            ! If the global radiation is missing, fill it by estimation
            if(.not. me % lvRg(i)) then
                me % rvRg(i) = GlobalRadiation(rvRc(i), rvFcd(i))
                me % lvRg(i) = .true.
            end if
            
            ! Estimate site pressure assuming mean value at mean sea level
            rvTa(i) = me % rvTemp(i) + 273.15_dp
            rvPa(i) = GuessPressure(tDescr % rHeight, rvTa(i))
            
            ! Estimate saturation and actual water vapor pressures at site
            rvEs(i) = EstimateSaturationPressure(rvTa(i))
            rvEa(i) = rvEs(i) * me % rvRelH(i) / 100.0_dp
            
            ! Estimate net radiation, if missing
            if(.not. me % lvRn(i)) then
                
                ! Estimate net radiation
                me % rvRn(i) = NetRadiation(me % rvRg(i), tDescr % rAlbedo, rvFcd(i), rvEa(i), rvTa(i))
                me % lvRn(i) = .true.
                
            end if
            
        end do
                
        ! Turbulence loop
        do i = 1, n
        
            ! Reconstruct Surface Layer parameters
            call PBL_33( &
                tDescr % iLandUse, tDescr % rZ0, tDescr % rD, tDescr % rZr, &
                me % rvVel(i), rvTa(i), me % rvRn(i), rvFcd(i), &
                rTempUs, rTempTs, rTempH0, rTempZLm1 &
            )
            tCurrentTime = fromEpoch(me % ivTimeStamp(i))
            rHour = real(tCurrentTime % iHour, kind=dp)
            me % rvG0(i) = SOIL_HEAT_FLUX(rHour, me % rvRn(i))
            me % rvZLm1(i) = rTempZLm1 / tDescr % rZr
            call SURFACE_PT( &
                tDescr % rZ0, rvTa(i), rvPa(i), &
                me % rvVel(i), me % rvRn(i), me % rvG0(i), &
                rH0, me % rvHe(i), rUstar, &
                me % rvTstar(i), rHLM &
            )
            me % rvZLm1(i) = tDescr % rZr * me % rvZLm1(i) ! Now it really is zr/L
            if(.not. me % lvUstar(i)) then
                me % rvUstar(i) = rUstar
                me % lvUstar(i) = .true.
            end if
            if(.not. me % lvH0(i)) then
                me % rvH0(i)    = rH0
                me % lvH0(i)    = .true.
            end if
            me % rvZLm1(i)  = rHLM
            
            ! Convert stability parameter to Obukhov length
            if(abs(me % rvZLm1(i)) > 1.d-3) then
                rvL(i) = tDescr % rZr / me % rvZLm1(i)
            else
                if(rvL(i) > 0.) then
                    rvL(i) =  9000.0
                else
                    rvL(i) = -9000.0
                end if
            end if
            
            ! Estimate stability category using surface roughness and Obukhov length
            me % ivLstab(i) = LSTAB(rvL(i), tDescr % rZ0)
            
            ! Estimate backwards Obukhov length from the "turbulent" stability category just computed
            call STAB2L(me % ivLstab(i), tDescr % rZ0, me % rvLalt1(i))
            me % rvLalt1(i) = me % rvLalt1(i) * tDescr % rZr
            if(abs(me % rvLalt1(i)) < 1.d-4) then
                if(me % rvLalt1(i) > 0._dp) then
                    me % rvLalt1(i) =  1.d-4
                else
                    me % rvLalt1(i) = -1.d-4
                end if
            end if
            
        end do
        
        ! Estimate mixing height
        do i = 1, n
        
            ! Convert time to its constituting parts, and get sunrise/sunset times
            tCurrentTime = fromEpoch(me % ivTimeStamp(i))
            rHour = real(tCurrentTime % iHour, kind=dp)
            rvSunRiseSet = SunRiseSunSet(me % ivTimeStamp(i), tDescr % rLat, tDescr % rLon, tDescr % iTimeZone)
            rSunRise = rvSunRiseSet(1)
            rSunSet  = rvSunRiseSet(2)
            
            ! Estimate mixing height
            rRc    = 1305._dp * 273.15_dp/rvTa(i)
            rZiMec = 1330._dp * me % rvUstar(i)
            if(rHour > rSunRise .and. rHour < rSunSet) then
                rZiConv = max(rZiConv, 0._dp)
                rZiConv = HMIX_NEW(real(tDescr % iTimeStep, kind=dp), me % rvH0(i), me % rvUstar(i), rvTa(i), rRc, rZiConv)
                me % rvZi(i) = max(rZiMec, rZiConv)
            else
                rZiConv = 0._dp
                me % rvZi(i) = StableZi( &
                    tDescr % rLat, &
                    me % rvTemp(i), &
                    me % rvH0(i), &
                    me % rvUstar(i), &
                    rvL(i), &
                    rvFcd(i) &
                )
            end if
        
        end do
        
        ! Leave
        deallocate(rvL)
        deallocate(rvEs)
        deallocate(rvEa)
        deallocate(rvPa)
        deallocate(rvTa)
        deallocate(rvFcd)
        deallocate(rvSe)
        deallocate(rvRc)
        deallocate(rvRe)
        
    end function fill_radiation_turbulence
    
    
    function write_to_calpuff(me, tDescr, sFileName, sReportName) result(iRetCode)
    
        ! Routine arguments
        class(data_set_type), intent(in)    :: me
        type(descriptor_type), intent(in)   :: tDescr
        character(len=*), intent(in)        :: sFileName
        character(len=*), intent(in)        :: sReportName
        integer                             :: iRetCode
        
        ! Locals
        integer     :: iLUN
        integer     :: i
        integer     :: n
        real(dp)    :: rDir
        real(dp)    :: rHLM, rL
        real(dp)    :: rZi
        real(dp)    :: rTa
        type(time)  :: tFrom
        type(time)  :: tTo
        type(time)  :: tCurrent
        integer     :: iYearFrom, iMonthFrom, iDayFrom, iHourFrom, iMinuteFrom, iSecondFrom
        integer     :: iYearTo, iMonthTo, iDayTo, iHourTo, iMinuteTo, iSecondTo
        integer     :: iYear, iMonth, iDay, iHour, iMinute, iSecond
        integer     :: iPrecCode
        real(dp)    :: rStability
        
        ! Assume success (will falsify on failure)
        iRetCode = 0
        
        ! Write the CALPUFF ISC-format meteo file
        open(newunit=iLUN, file=sFileName, status='unknown', action='write')
        n = size(me % ivTimeStamp)
        tFrom = fromEpoch(me % ivTimeStamp(1))
        iYearFrom   = tFrom % iYear
        iMonthFrom  = tFrom % iMonth
        iDayFrom    = tFrom % iDay
        iHourFrom   = tFrom % iHour
        iMinuteFrom = tFrom % iMinute
        iSecondFrom = tFrom % iSecond
        tTo   = fromEpoch(me % ivTimeStamp(n))
        iYearTo   = tTo % iYear
        iMonthTo  = tTo % iMonth
        iDayTo    = tTo % iDay
        iHourTo   = tTo % iHour
        iMinuteTo = tTo % iMinute
        iSecondTo = tTo % iSecond
        write(iLUN,"('ISCMET.DAT      2.1             Processed data')")
        write(iLUN,"('   1')")
        write(iLUN,"('Prepared by ST-Me')")
        write(iLUN,"('NONE')")
        if(tDescr % iTimeZone > 0) then
            write(iLUN, "('UTC+',i2.2,'00')") tDescr % iTimeZone
        else
            write(iLUN, "('UTC',i3.3,'00')") tDescr % iTimeZone
        end if
        write(iLUN,"(i4,1x,i2,1x,i2,1x,i2,1x,i4,1x,i4,1x,i2,1x,i2,1x,i2,1x,i4)") &
            iYearFrom, iMonthFrom, iDayFrom, iHourFrom+1, 60*iMinuteFrom+iSecondFrom, &
            iYearTo, iMonthTo, iDayTo, iHourTo+1, 60*iMinuteTo+iSecondTo + tDescr % iTimeStep
        write(iLUN,"('00000 ',i4,' 00000 ',i4)") iYearFrom, iYearTo
        
        do i = 1, n
        
            ! Prepare date and time, and change hour to reflect CALPUFF/ISC convention
            tCurrent = fromEpoch(me % ivTimeStamp(i))
            iYear   = tCurrent % iYear
            iMonth  = tCurrent % iMonth
            iDay    = tCurrent % iDay
            iHour   = tCurrent % iHour
            iMinute = tCurrent % iMinute
            iSecond = tCurrent % iSecond
            iHour = iHour + 1
            
            ! Change wind direction from anemometer to flow convention
            rDir = me % rvDir(i) + 180._dp
            if(rDir > 360._dp) rDir = rDir - 360._dp
            
            ! Compute Obukhov length
            rHLM = me % rvZLm1(i)
            if(abs(rHLM) > 1.e-3) then
                rL = tDescr % rZr / me % rvZLm1(i)
            else
                rL = tDescr % rZr / 1.d-3
            end if
            if(rL < -100000.0_dp) rL = -100000.0_dp
            if(rL >  100000.0_dp) rL =  100000.0_dp
            
            ! Adjust Zi
            rZi = MAX(me % rvZi(i), 100._dp)
            
            ! Convert temperature to K
            rTa = me % rvTemp(i) + 273.15_dp
            
            ! Generate precipitation code, assuming all precipitation is liquid,
            ! with code 1
            if(me % rvPrec(i) > 0.0_dp) then
                iPrecCode = 1
            else
                iPrecCode = 0
            end if
        
            ! Print data the CALPUFF/ISC way
            write(iLUN,"(2(4i2,i4),2f9.4,f6.1,i2,2f7.1,f9.4,f10.1,f8.4,i4,f7.2,a10,a5,1x,f8.1,i3)") &
                mod(iYear, 100), iMonth, iDay, iHour, iMinute * 60 + iSecond, &
                mod(iYear, 100), iMonth, iDay, iHour, iMinute * 60 + iSecond + tdescr % iTimeStep, &
                rDir, me % rvVel(i), rTa, me % ivLstab(i), rZi, rZi, &
                me % rvUstar(i), rL, tDescr % rZ0, iPrecCode, me % rvPrec(i), ' ', '     ', &
                me % rvRg(i), floor(min(me % rvRelH(i),100.0_dp))
            
        end do
        close(iLUN)
        
        ! Write the same information, this time using a regular CSV form
        open(newunit=iLUN, file=sReportName, action='write', status='unknown')
        write(iLUN, "('Time.Stamp, Vel, Dir, Temp, RelH, Prec, Rg, Rn, H0, u.star, Stability, Stability.Category, Zi')")
        do i = 1, n
        
            ! Prepare date and time, and change hour to reflect CALPUFF/ISC convention
            tCurrent = fromEpoch(me % ivTimeStamp(i))
            iYear   = tCurrent % iYear
            iMonth  = tCurrent % iMonth
            iDay    = tCurrent % iDay
            iHour   = tCurrent % iHour
            iMinute = tCurrent % iMinute
            iSecond = tCurrent % iSecond
            
            ! Clip stability values to practically interesting limits
            ! (values exceeding these limits re anyway classifiable as
            ! extremely stable/unstable)
            rStability = max(min(me % rvZLm1(i), 99._dp), -99._dp)
            
            ! Write data in plain form
            write(iLUN,"(i4.4,2('-',i2.2),1x,i2.2,2(':',i2.2),',',f5.2,4(',',f6.2),3(',',f8.2),2(',',f7.3),',',i2,',',f7.1)") &
                iYear, iMonth, iDay, iHour, iMinute, iSecond, &
                me % rvVel(i), me % rvDir(i), me % rvTemp(i), me % rvRelH(i), &
                me % rvPrec(i), &
                me % rvRg(i), me % rvRn(i), me % rvH0(i), &
                me % rvUstar(i), rStability, me % ivLstab(i), &
                me % rvZi(i)
        end do
        close(iLUN)

    end function write_to_calpuff
    
    ! *********************
    ! * Internal routines *
    ! *********************
    
    function summary_stats(rvVal, lvValid, rAvail, rMean, rStd) result(iNumValid)
    
        ! Routine arguments
        real(dp), dimension(:), intent(in)  :: rvVal
        logical, dimension(:), intent(in)   :: lvValid
        real(dp), intent(out)               :: rAvail       ! Availability (%)
        real(dp), intent(out)               :: rMean
        real(dp), intent(out)               :: rStd
        integer                             :: iNumValid
        
        ! Locals
        integer     :: iNumData

        ! Get the information desired
        iNumData = size(rvVal)
        iNumValid = count(lvValid)
        if(iNumValid > 0) then
            rAvail = 100.d0 * iNumValid / real(iNumData, kind=dp)
            rMean  = sum(rvVal, mask=lvValid) / iNumValid
            rStd   = sqrt(sum((rvVal - rMean)**2, mask=lvValid) / iNumValid)
        else
            rAvail = -9999.9d0
            rMean  = -9999.9d0
            rStd   = -9999.9d0
        end if
        
    end function summary_stats
    
end module data_set
