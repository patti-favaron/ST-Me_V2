module descriptor

    implicit none
    
    private
    
    ! Public interface
    integer, parameter, public  :: dp = selected_real_kind(15)
    public  :: descriptor_type
    
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
        logical     :: lIsRelH          ! Relative humidity presence state (given)
        logical     :: lIsPrec          ! Precipitation presence state (given)
        logical     :: lIsCloudCover    ! Cloud cover presence state (given)
        logical     :: lIsRg            ! Global radiation presence state (given)
        logical     :: lIsRn            ! Net radiation presence state (given)
        logical     :: lIsUstar         ! Friction velocity presence state (given)
        logical     :: lIsH0            ! Turbulent sensible heat flux presence state (given)
        logical     :: lIsStability     ! Stability parameter (zr/L) presence state (given)
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
        character(len=80)                       :: sLeft
        character(len=80)                       :: sRight
        character(len=80), dimension(MAX_LINES) :: svName
        character(len=80), dimension(MAX_LINES) :: svValue
        integer, dimension(MAX_LINES)           :: ivIndex
        integer                                 :: iNumLines
        integer                                 :: iLine
        integer                                 :: iField
        integer                                 :: iPos
        integer, dimension(size(FIELDS))        :: ivPosition
        
        ! Constants (please do not change :) )
        character(len=*), dimension(*), parameter   :: FIELDS = [ &
            'LAT', &
            'LON', &
            'TIMEZONE', &
            'ALTITUDE', &
            'DISPL_HEIGHT', &
            'ANEMO_HEIGHT', &
            'LAND_USE', &
            'Z0', &
            'ALBEDO', &
            'TIME_STAMP_MEANING', &
            'TIME_STEP', &
            'DATA_RELH', &
            'DATA_PREC', &
            'DATA_CLOUD_COVER', &
            'DATA_RG', &
            'DATA_RN', &
            'DATA_USTAR', &
            'DATA_H0', &
            'DATA_STABILITY' &
        ]
        
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
        read(svValue(ivPosition(1)), *, iostat=iErrCode) me % rLat
        if(iErrCode /= 0) then
            iRetCode = 3
            return
        end if
        if(me % rLat < -90.0d0 .or. me % rLat > 90.0d0) then
            iRetCode = 4
            return
        end if
        read(svValue(ivPosition(2)), *, iostat=iErrCode) me % rLon
        if(iErrCode /= 0) then
            iRetCode = 5
            return
        end if
        if(me % rLon < -180.0d0 .or. me % rLon > 180.0d0) then
            iRetCode = 6
            return
        end if
        read(svValue(ivPosition(3)), *, iostat=iErrCode) me % iTimeZone
        if(iErrCode /= 0) then
            iRetCode = 7
            return
        end if
        if(me % iTimeZone < -11 .or. me % iTimeZone > 12) then
            iRetCode = 8
            return
        end if
        read(svValue(ivPosition(4)), *, iostat=iErrCode) me % rHeight
        if(iErrCode /= 0) then
            iRetCode = 9
            return
        end if
        if(me % rHeight < -450.0d0 .or. me % rHeight > 8848.0d0) then
            iRetCode = 10
            return
        end if
        read(svValue(ivPosition(5)), *, iostat=iErrCode) me % rZ0
        if(iErrCode /= 0) then
            iRetCode = 11
            return
        end if
        if(me % rZ0 < 0.0d0 .or. me % rZ0 > 10.0d0) then
            iRetCode = 12
            return
        end if
        read(svValue(ivPosition(6)), *, iostat=iErrCode) me % rAlbedo
        if(iErrCode /= 0) then
            iRetCode = 13
            return
        end if
        if(me % rAlbedo < 0.0d0 .or. me % rAlbedo > 1.0d0) then
            iRetCode = 14
            return
        end if
        read(svValue(ivPosition(7)), *, iostat=iErrCode) me % rD
        if(iErrCode /= 0) then
            iRetCode = 15
            return
        end if
        if(me % rD < 0.0d0 .or. me % rD > 10.0d0) then
            iRetCode = 16
            return
        end if
        read(svValue(ivPosition(8)), *, iostat=iErrCode) me % rZr
        if(iErrCode /= 0) then
            iRetCode = 17
            return
        end if
        if(me % rZr < 0.0d0 .or. me % rZr > 150.0d0) then
            iRetCode = 18
            return
        end if
        read(svValue(ivPosition(9)), *, iostat=iErrCode) me % iLandUse
        if(iErrCode /= 0) then
            iRetCode = 19
            return
        end if
        if(me % iLandUse < 1 .or. me % iLandUse > 6) then
            iRetCode = 20
            return
        end if
        read(svValue(ivPosition(10)), *, iostat=iErrCode) me % iDateMeaning
        if(iErrCode /= 0) then
            iRetCode = 21
            return
        end if
        if(me % iDateMeaning < 0 .or. me % iDateMeaning > 1) then
            iRetCode = 22
            return
        end if
        read(svValue(ivPosition(11)), *, iostat=iErrCode) me % iTimeStep
        if(iErrCode /= 0) then
            iRetCode = 23
            return
        end if
        if(me % iTimeStep <= 0 .or. mod(3600, me % iTimeStep) /= 0) then
            iRetCode = 24
            return
        end if
        read(svValue(ivPosition(12)), "(a)", iostat=iErrCode) sBuffer
        if(me % sBuffer /= "PRESENT" .and. me % sBuffer /= "MISSING") then
            iRetCode = 25
            return
        end if
        me % lIsRelH = (sBuffer == "PRESENT")
        read(svValue(ivPosition(13)), "(a)", iostat=iErrCode) sBuffer
        if(me % sBuffer /= "PRESENT" .and. me % sBuffer /= "MISSING") then
            iRetCode = 26
            return
        end if
        me % lIsPrec = (sBuffer == "PRESENT")
        read(svValue(ivPosition(14)), "(a)", iostat=iErrCode) sBuffer
        if(me % sBuffer /= "PRESENT" .and. me % sBuffer /= "MISSING") then
            iRetCode = 27
            return
        end if
        me % lIsCloudCover = (sBuffer == "PRESENT")
        read(svValue(ivPosition(15)), "(a)", iostat=iErrCode) sBuffer
        if(me % sBuffer /= "PRESENT" .and. me % sBuffer /= "MISSING") then
            iRetCode = 28
            return
        end if
        me % lIsRg = (sBuffer == "PRESENT")
        read(svValue(ivPosition(16)), "(a)", iostat=iErrCode) sBuffer
        if(me % sBuffer /= "PRESENT" .and. me % sBuffer /= "MISSING") then
            iRetCode = 29
            return
        end if
        me % lIsRn = (sBuffer == "PRESENT")
        read(svValue(ivPosition(17)), "(a)", iostat=iErrCode) sBuffer
        if(me % sBuffer /= "PRESENT" .and. me % sBuffer /= "MISSING") then
            iRetCode = 30
            return
        end if
        me % lIsUstar = (sBuffer == "PRESENT")
        read(svValue(ivPosition(18)), "(a)", iostat=iErrCode) sBuffer
        if(me % sBuffer /= "PRESENT" .and. me % sBuffer /= "MISSING") then
            iRetCode = 31
            return
        end if
        me % lIsH0 = (sBuffer == "PRESENT")
        read(svValue(ivPosition(19)), "(a)", iostat=iErrCode) sBuffer
        if(me % sBuffer /= "PRESENT" .and. me % sBuffer /= "MISSING") then
            iRetCode = 32
            return
        end if
        me % lIsStability = (sBuffer == "PRESENT")
        
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
        if(me % lIsRelH) then
            print "('  Relative humidity: Expected')"
        else
            print "('  Relative humidity: Missing')"
        end if
        if(me % lIsPrec) then
            print "('  Precipitation: Expected')"
        else
            print "('  Precipitation: Missing')"
        end if
        if(me % lIsCloudCover) then
            print "('  Cloud cover: Expected')"
        else
            print "('  Cloud cover: Missing')"
        end if
        if(me % lIsRg) then
            print "('  Global radiation: Expected')"
        else
            print "('  Global radiation: Missing')"
        end if
        if(me % lIsRn) then
            print "('  Net radiation: Expected')"
        else
            print "('  Net radiation: Missing')"
        end if
        if(me % lIsUstar) then
            print "('  Friction velocity: Expected')"
        else
            print "('  Friction velocity: Missing')"
        end if
        if(me % lIsH0) then
            print "('  Turbulent heat flux: Expected')"
        else
            print "('  Turbulent heat flux: Missing')"
        end if
        if(me % lIsStability) then
            print "('  Stability parameter: Expected')"
        else
            print "('  Stability parameter: Missing')"
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
        character   :: iAscii
        
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
