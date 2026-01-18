! Module "descriptor", dealing with measurements location and type
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
module descriptor

    use types

    implicit none
    
    private
    
    ! Public interface
    public  :: descriptor_type
    
    ! Constants
    integer, parameter  :: NUM_DATA = 16
    
    ! Data types
    type descriptor_type
        logical     :: lFull            ! True is assigned, false elsewhere
        real(dp)    :: rLat             ! Latitude, positive Northwards (given, °)
        real(dp)    :: rLon             ! Longitude, positive Eastwards (given, °)
        integer     :: iTimeZone        ! Integerized time zone (given)
        real(dp)    :: rHeight          ! Height above mean sea height (given, m)
        real(dp)    :: rZ0              ! Aerodynamic roughness length (given, m)
        real(dp)    :: rAlbedo          ! Albedo (given, dimensionless)
        real(dp)    :: rD               ! Displacement height (given, m)
        real(dp)    :: rZr              ! Anemometer height above ground zero level (given, m)
        integer     :: iLandUse         ! Land use code (given, dimensionless, see Tech Report)
        integer     :: iDateMeaning     ! 0:Anticipated time stamp; 1:Posticipated time stamp (given)
        integer     :: iTimeStep        ! Time step expected in data (given, s)
        logical     :: lIsPrec          ! Precipitation presence state (given)
        logical     :: lIsRg            ! Global radiation presence state (given)
        logical     :: lIsRn            ! Net radiation presence state (given)
        logical     :: lIsUstar         ! Friction velocity presence state (given)
        logical     :: lIsH0            ! Turbulent sensible heat flux presence state (given)
    contains
        procedure   :: load
        procedure   :: dump
    end type descriptor_type
    
contains

    function load(me, sFileName) result(iRetCode)
    
        ! Routine arguments
        class(descriptor_type), intent(out) :: me
        character(len=256), intent(in)      :: sFileName
        integer                             :: iRetCode
        
        ! Locals
        integer                                 :: iErrCode
        integer                                 :: iLUN
        character(len=80)                       :: sBuffer
        integer, parameter                      :: MAX_LINES = 40
        character(len=80), dimension(MAX_LINES) :: svName
        character(len=80), dimension(MAX_LINES) :: svValue
        integer, dimension(MAX_LINES)           :: ivIndex
        integer                                 :: iNumLines
        integer                                 :: iLine
        integer                                 :: iField
        integer                                 :: iPos
        integer, dimension(NUM_DATA)            :: ivPosition
        
        ! Constants (please do not change :) )
        character(len=18), dimension(NUM_DATA), parameter   :: FIELDS = [ &
            'LAT               ', &
            'LON               ', &
            'TIMEZONE          ', &
            'ALTITUDE          ', &
            'DISPL_HEIGHT      ', &
            'ANEMO_HEIGHT      ', &
            'LAND_USE          ', &
            'Z0                ', &
            'ALBEDO            ', &
            'TIME_STAMP_MEANING', &
            'TIME_STEP         ', &
            'DATA_PREC         ', &
            'DATA_RG           ', &
            'DATA_RN           ', &
            'DATA_USTAR        ', &
            'DATA_H0           '  &
        ]
        integer, parameter  :: LAT                =  1
        integer, parameter  :: LON                =  2
        integer, parameter  :: TIMEZONE           =  3
        integer, parameter  :: ALTITUDE           =  4
        integer, parameter  :: DISPL_HEIGHT       =  5
        integer, parameter  :: ANEMO_HEIGHT       =  6
        integer, parameter  :: LAND_USE           =  7
        integer, parameter  :: Z0                 =  8
        integer, parameter  :: ALBEDO             =  9
        integer, parameter  :: TIME_STAMP_MEANING = 10
        integer, parameter  :: TIME_STEP          = 11
        integer, parameter  :: DATA_PREC          = 12
        integer, parameter  :: DATA_RG            = 13
        integer, parameter  :: DATA_RN            = 14
        integer, parameter  :: DATA_USTAR         = 15
        integer, parameter  :: DATA_H0            = 16
        
        ! Assume success (will falsify on failure)
        iRetCode = 0
        me % lFull = .false.
        
        ! Import data
        open(newunit=iLUN, file=sFileName, status='old', action='read', iostat=iErrCode)
        if(iErrCode /= 0) then
            iRetCode = 1
            return
        end if
        iNumLines = 0
        do while(iNumLines < MAX_LINES)
            read(iLUN, "(a)", iostat=iErrCode) sBuffer
            iPos = index(sBuffer, "=")
            if(iPos > 0) then
                iNumLines = iNumLines + 1
                svName(iNumLines)  = toUpper(sBuffer(:(iPos-1)))
                svValue(iNumLines) = toUpper(sBuffer((iPos+1):))
            end if
        end do
        close(iLUN)
        
        ! Associate data with their indices
        ivIndex = 0
        ivPosition = 0
        do iField = 1, size(FIELDS)
            do iLine = 1, iNumLines
                if(ivIndex(iLine) == 0) then
                    if(FIELDS(iField) == svName(iLine)) then
                        ivIndex(iLine) = iField
                        ivPosition(iField) = iLine
                        exit
                    end if
                end if
            end do
        end do
        
        ! Check all fields have been found in descriptor
        if(any(ivPosition <= 0)) then
            iRetCode = 2
            return
        end if
        
        ! All found: decode individually, and apply validation rules whenever necessary
        read(svValue(ivPosition(LAT)), *, iostat=iErrCode) me % rLat
        if(iErrCode /= 0) then
            iRetCode = 3
            return
        end if
        if(me % rLat < -90.0d0 .or. me % rLat > 90.0d0) then
            iRetCode = 4
            return
        end if
        read(svValue(ivPosition(LON)), *, iostat=iErrCode) me % rLon
        if(iErrCode /= 0) then
            iRetCode = 5
            return
        end if
        if(me % rLon < -180.0d0 .or. me % rLon > 180.0d0) then
            iRetCode = 6
            return
        end if
        read(svValue(ivPosition(TIMEZONE)), *, iostat=iErrCode) me % iTimeZone
        if(iErrCode /= 0) then
            iRetCode = 7
            return
        end if
        if(me % iTimeZone < -11 .or. me % iTimeZone > 12) then
            iRetCode = 8
            return
        end if
        read(svValue(ivPosition(ALTITUDE)), *, iostat=iErrCode) me % rHeight
        if(iErrCode /= 0) then
            iRetCode = 9
            return
        end if
        if(me % rHeight < -450.0d0 .or. me % rHeight > 8848.0d0) then
            iRetCode = 10
            return
        end if
        read(svValue(ivPosition(DISPL_HEIGHT)), *, iostat=iErrCode) me % rD
        if(iErrCode /= 0) then
            iRetCode = 11
            return
        end if
        if(me % rD < 0.0d0) then
            iRetCode = 12
            return
        end if
        read(svValue(ivPosition(ANEMO_HEIGHT)), *, iostat=iErrCode) me % rZr
        if(iErrCode /= 0) then
            iRetCode = 13
            return
        end if
        if(me % rZr < 0.0d0 .or. me % rZr > 150.0d0) then
            iRetCode = 14
            return
        end if
        read(svValue(ivPosition(LAND_USE)), *, iostat=iErrCode) me % iLandUse
        if(iErrCode /= 0) then
            iRetCode = 15
            return
        end if
        if(me % iLandUse < 1 .or. me % iLandUse > 6) then
            iRetCode = 16
            return
        end if
        read(svValue(ivPosition(Z0)), *, iostat=iErrCode) me % rZ0
        if(iErrCode /= 0) then
            iRetCode = 17
            return
        end if
        if(me % rZ0 < 0.0d0 .or. me % rZ0 > 10.0d0) then
            iRetCode = 18
            return
        end if
        read(svValue(ivPosition(ALBEDO)), *, iostat=iErrCode) me % rAlbedo
        if(iErrCode /= 0) then
            iRetCode = 19
            return
        end if
        if(me % rAlbedo < 0.0d0 .or. me % rAlbedo > 1.0d0) then
            print *, me % rAlbedo
            iRetCode = 20
            return
        end if
        read(svValue(ivPosition(TIME_STAMP_MEANING)), *, iostat=iErrCode) me % iDateMeaning
        if(iErrCode /= 0) then
            iRetCode = 21
            return
        end if
        if(me % iDateMeaning < 0 .or. me % iDateMeaning > 1) then
            iRetCode = 22
            return
        end if
        read(svValue(ivPosition(TIME_STEP)), *, iostat=iErrCode) me % iTimeStep
        if(iErrCode /= 0) then
            iRetCode = 23
            return
        end if
        if(me % iTimeStep <= 0 .or. mod(3600, me % iTimeStep) /= 0) then
            iRetCode = 24
            return
        end if
        sBuffer = adjustl(svValue(ivPosition(DATA_PREC)))
        if(sBuffer /= "PRESENT" .and. sBuffer /= "MISSING") then
            iRetCode = 25
            return
        end if
        me % lIsPrec = (sBuffer == "PRESENT")
        sBuffer = adjustl(svValue(ivPosition(DATA_RG)))
        if(sBuffer /= "PRESENT" .and. sBuffer /= "MISSING") then
            iRetCode = 26
            return
        end if
        me % lIsRg = (sBuffer == "PRESENT")
        sBuffer = adjustl(svValue(ivPosition(DATA_RN)))
        if(sBuffer /= "PRESENT" .and. sBuffer /= "MISSING") then
            iRetCode = 27
            return
        end if
        me % lIsRn = (sBuffer == "PRESENT")
        sBuffer = adjustl(svValue(ivPosition(DATA_USTAR)))
        if(sBuffer /= "PRESENT" .and. sBuffer /= "MISSING") then
            iRetCode = 28
            return
        end if
        me % lIsUstar = (sBuffer == "PRESENT")
        sBuffer = adjustl(svValue(ivPosition(DATA_H0)))
        if(sBuffer /= "PRESENT" .and. sBuffer /= "MISSING") then
            iRetCode = 29
            return
        end if
        me % lIsH0 = (sBuffer == "PRESENT")
        
        ! Declare completion
        me % lFull = .true.
        
    end function load
    
    function dump(me) result(iRetCode)
    
        ! Routine arguments
        class(descriptor_type), intent(in)  :: me
        integer                             :: iRetCode
        
        ! Locals
        ! --none--
        
        ! Assume success (will falsify on failure)
        iRetCode = 0
        
        ! Check parameters
        if(.not. me % lFull) then
            print "('Descriptor is invalid: terminating immediately')"
            iRetCode = 1
            return
        end if
        
        ! Print report to screen
        print "('Descriptor')"
        print "('==========')"
        print *
        print "('Lat: ', f11.6, '  Lon: ', f11.6, '  TZ: ',i3,'  H: ',f6.1)", me % rLat, me % rLon, me % iTimeZone, me % rHeight
        print "('Anemometer height above ground: ',f7.2)", me % rZr
        print "('z0: ', f9.6, '  D: ', f9.6, '  Albedo: ', f8.6)", me % rZ0, me % rD, me % rAlbedo
        print "('Land use code: ', i1, '  Date type code: ', i1, '  Time step: ', i4)", me % iLandUse, me % iDateMeaning, me % iTimeStep
        print "('List of optional data present:')"
        if(me % lIsPrec) then
            print "('  Precipitation:       Expected')"
        else
            print "('  Precipitation:       Missing')"
        end if
        if(me % lIsRg) then
            print "('  Global radiation:    Expected')"
        else
            print "('  Global radiation:    Missing')"
        end if
        if(me % lIsRn) then
            print "('  Net radiation:       Expected')"
        else
            print "('  Net radiation:       Missing')"
        end if
        if(me % lIsUstar) then
            print "('  Friction velocity:   Expected')"
        else
            print "('  Friction velocity:   Missing')"
        end if
        if(me % lIsH0) then
            print "('  Turbulent heat flux: Expected')"
        else
            print "('  Turbulent heat flux: Missing')"
        end if
        
    end function dump
    
    ! *********************
    ! * Internal routines *
    ! *********************
    
    function toUpper(sInStr) result(sOutStr)
    
        ! Routine arguments
        character(len=*), intent(in)    :: sInStr
        character(len=len(sInStr))      :: sOutStr
        
        ! Locals
        integer     :: i
        character   :: c
        
        ! Convert string
        do i = 1, len(sInStr)
            c = sInStr(i:i)
            if(c >= 'a' .and. c <= 'z') then
                sOutStr(i:i) = char(ichar(c) - ichar('a') + ichar('A'))
            else
                sOutStr(i:i) = c
            end if
        end do
        
    end function toUpper

end module descriptor
