!------------------------------------------------------------------
! estimate
!
! Module, containing all the estimation routines used in ST-Me
!
! Written by: Patrizia Favaron
! e-mail:     patti.favaron@gmail.com
!
!------------------------------------------------------------------
! Statement of Licensing Conditions
!------------------------------------------------------------------
!
! Copyright 2026 Patrizia Favaron
!
! Permission is hereby granted, free of charge, to any person
! obtaining a copy of this software and associated documentation
! files (the "Software"), to deal in the Software without
! restriction, including without limitation the rights to use,
! copy, modify, merge, publish, distribute, sublicense, and/or
! sell copies of the Software, and to permit persons to whom the
! Software is furnished to do so, subject to the following conditions:
!
! The above copyright notice and this permission notice shall be
! included in all copies or substantial portions of the Software.
!
! THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND,
! EXPRESS OR IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES
! OF MERCHANTABILITY, FITNESS FOR A PARTICULAR PURPOSE AND
! NONINFRINGEMENT. IN NO EVENT SHALL THE AUTHORS OR COPYRIGHT
! HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER LIABILITY,
! WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING
! FROM, OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR
! OTHER DEALINGS IN THE SOFTWARE.
!
!------------------------------------------------------------------
!
module estimate

    use types
    use datetime

    implicit none
    
    private
    
    ! Public interface
    public  :: adjust_time_stamps
    public  :: mean_day_gap_filler
    public  :: ExtraterrestrialRadiation
    public  :: GlobalRadiation
    public  :: NetRadiation
    public  :: ClearSkyRadiation
    public  :: EstimatedCloudiness
    public  :: MeasuredCloudiness
    public  :: GuessPressure
    public  :: EstimateSaturationPressure
    public  :: PBL_33
    public  :: SURFACE_PT
    public  :: SOIL_HEAT_FLUX
    public  :: LSTAB
    public  :: STAB2L
    public  :: HMIX_NEW
    public  :: StableZi
    public  :: SunRiseSunSet
    
    ! Operational parameters
    integer, parameter  :: DAYS_RADIUS = 15 ! Number of days before and after the current for mean days
    
contains

    ! Change time stamp so that it refers to begin of time averaging intervals.
    ! Note this has a side effect: if the original time stamp is of the "retarded"
    ! type, that is, at end-of-averaging-interval, and the original data set
    ! covered one exact day, then after the adjustment the new stamped dates will
    ! conduce to a new initial "day" containing one row only, and a final day without
    ! the last row. Measures should be taken at caller level to prevent any adverse
    ! effets.
    function adjust_time_stamps(ivTimeStamp, iDateMeaning, iTimeStep) result(iRetCode)
    
        ! Routine arguments
        integer, dimension(:), intent(inout)    :: ivTimeStamp
        integer, intent(in)                     :: iDateMeaning
        integer, intent(in)                     :: iTimeStep
        integer                                 :: iRetCode
        
        ! Locals
        ! --none--
        
        ! Assume success (will falsify on failure)
        iRetCode = 0
        
        ! Adjust the time stamp so that it is always expressed from the beginning
        ! of averaging time
        if(iDateMeaning == 1) then  ! "Retarded", or "end-period", time stamps
            ivTimeStamp = ivTimeStamp - iTimeStep
        end if
        
    end function adjust_time_stamps
    
    
    function mean_day_gap_filler(ivTimeStamp, rvValue, lvValid, iTimeStep, lRandomize) result(iRetCode)
    
        ! Routine arguments
        integer, dimension(:), intent(in)                   :: ivTimeStamp
        real(dp), dimension(:), intent(inout)               :: rvValue
        logical, dimension(:), intent(inout)                :: lvValid
        integer, intent(in)                                 :: iTimeStep
        logical, intent(in)                                 :: lRandomize
        integer                                             :: iRetCode
        
        ! Locals
        integer     :: iMinTimeStamp
        integer     :: iMaxTimeStamp
        integer     :: iNumDays
        integer     :: iNumItemsPerDay
        integer     :: iIdxFrom
        integer     :: iIdxTo
        integer     :: iDay
        integer     :: iDayIdx
        integer     :: i
        real(dp)    :: rRand
        integer, dimension(:), allocatable  :: ivBlockData
        real(dp), dimension(:), allocatable :: rvBlockValue
        real(dp), dimension(:), allocatable :: rvBlockStdDev
        
        ! Constants (please do not change... :) )
        integer, parameter  :: ONE_HOUR = 3600
        integer, parameter  :: ONE_DAY  = 24*ONE_HOUR
        
        ! Assume success (will falsify on failure)
        iRetCode = 0
        
        ! Check how many days and items per day are expected
        iMinTimeStamp   = minval(ivTimeStamp)
        iMaxTimeStamp   = maxval(ivTimeStamp)
        iNumDays        = (iMaxTimeStamp - iMinTimeStamp + iTimeStep) / ONE_DAY
        iNumItemsPerDay = ONE_DAY / iTimeStep
        if(iNumItemsPerDay <= 0) then
            iRetCode = 1
            return
        end if
        if(iNumDays < 1) then
            iRetCode = 2
            return
        end if
        allocate(ivBlockData(iNumItemsPerDay))
        allocate(rvBlockValue(iNumItemsPerDay))
        allocate(rvBlockStdDev(iNumItemsPerDay))
        
        ! Process days in sequence
        do iDay = 1, iNumDays
        
            ! Locate day in data vector
            iIdxFrom = iNumItemsPerDay * (iDay - 1) + 1
            iIdxTo   = iIdxFrom + iNumItemsPerDay - 1
            
            ! Is there something invalid? (If not, continue scanning days;
            ! otherwise, fill gaps)
            if(.not.all(lvValid(iIdxFrom:iIdxTo))) then
            
                ! Extend index ranges to include the mean day radius
                iIdxFrom = max(iIdxFrom - DAYS_RADIUS * iNumItemsPerDay, 1)
                iIdxTo   = min(iIdxTo + DAYS_RADIUS * iNumItemsPerDay, size(rvValue))
                
                ! Compute mean day over desired block
                ivBlockData = 0
                rvBlockValue = 0.d0
                do i = iIdxFrom, iIdxTo
                    iDayIdx = mod(ivTimeStamp(i), ONE_DAY) / iTimeStep + 1
                    iDayIdx = min(iDayIdx, iNumItemsPerDay)
                    iDayIdx = max(iDayIdx, 1)
                    if(lvValid(i)) then
                        ivBlockData(iDayIdx)  = ivBlockData(iDayIdx) + 1
                        rvBlockValue(iDayIdx) = rvBlockValue(iDayIdx) + rvValue(i)
                    end if
                end do
                where(ivBlockData > 0)
                    rvBlockValue = rvBlockValue / real(ivBlockData, kind=dp)
                elsewhere
                    rvBlockValue = -9999.9d0
                end where
                
                ! Compute standard deviations
                rvBlockStdDev = 0.d0
                do i = iIdxFrom, iIdxTo
                    iDayIdx = mod(ivTimeStamp(i), ONE_DAY) / iTimeStep + 1
                    iDayIdx = min(iDayIdx, iNumItemsPerDay)
                    iDayIdx = max(iDayIdx, 1)
                    if(lvValid(i)) then
                        rvBlockStdDev(iDayIdx) = rvBlockStdDev(iDayIdx) + (rvValue(i) - rvBlockValue(iDayIdx))**2
                    end if
                end do
                where(ivBlockData > 0)
                    rvBlockStdDev = sqrt(rvBlockStdDev / real(ivBlockData, kind=dp))
                elsewhere
                    rvBlockStdDev = -9999.9d0
                end where
                
                ! Replace missing values
                do i = iIdxFrom, iIdxTo
                    iDayIdx = mod(ivTimeStamp(i), ONE_DAY) / iTimeStep + 1
                    iDayIdx = min(iDayIdx, iNumItemsPerDay)
                    iDayIdx = max(iDayIdx, 1)
                    if(.not. lvValid(i)) then
                        rvValue(i) = rvBlockValue(iDayIdx)
                        if(lRandomize) then
                            call random_number(rRand)
                            rvValue(i) = rvValue(i) + sqrt(3.0_dp*rvBlockStdDev(iDayIdx))*2.0_dp*(rRand - 0.5_dp)
                        end if
                        lvValid(i) = .true.
                    end if
                end do
                
            end if
        end do
        
        ! If some value remains invalid after the substitution occurred, then
        ! a warning is given; this means some items in a hour are systematically missing,
        ! which indicates an extremely poor data quality - users are then free to
        ! consider this "warning" as an error
        if(.not.all(lvValid)) iRetCode = -1
        
        ! Leave
        deallocate(rvBlockStdDev)
        deallocate(rvBlockValue)
        deallocate(ivBlockData)
        
    end function mean_day_gap_filler
    
    ! ******************************
    ! * Physically-based estimates *
    ! ******************************
    
    ! Estimate of extraterrestrial solar radiation, using equations from then
    ! ASCE "Reference Evapotranspiration Equation"
    function ExtraterrestrialRadiation(iTimeStamp, iAveragingPeriod, lat, lon, iZone, rSolarElevationAngle) result(rRe)

        ! Routine arguments
        integer, intent(in)     :: iTimeStamp
        integer, intent(in)     :: iAveragingPeriod
        real(dp), intent(in)    :: lat
        real(dp), intent(in)    :: lon
        integer, intent(in)     :: iZone                ! Time zone, positive eastwards
        real(dp), intent(out)   :: rSolarElevationAngle ! Ditto, in degrees
        real(dp)                :: rRe                  ! Extraterrestrial radiation (W/m2)

        ! Locals
        real(dp)    :: rDayInYear
        real(dp)    :: rInvSqDistCorrection
        real(dp)    :: omega, omega1, omega2, omegaS
        real(dp)    :: rTime
        real(dp)    :: rZone
        real(dp)    :: t, Sc, b, t1
        real(dp)    :: rSolarDeclination
        real(dp)    :: centralMeridianLongitude
        real(dp)    :: localLongitude
        type(time)  :: tCurrentTime
        type(time)  :: tBaseTime

        ! Constants (please do not change... :) )
        real, parameter	:: SOLAR_CONSTANT = 1360.8_dp   ! W/m2 (official value for now, not ASCE's)
        real, parameter	:: PI             = 4.0_dp * atan(1.0_dp)

        ! Split date and time in parts, and build the year-based time
        tCurrentTime = fromEpoch(iTimeStamp)
        tBaseTime = time(tCurrentTime % iYear, 1_1, 1_1, 0_1, 0_1, 0_1)
        rDayInYear = real(julianDay(tCurrentTime) - julianDay(tBaseTime) + 1, kind=dp)

        ! Compute solar declination
        rSolarDeclination = 0.409_dp * sin(2.0_dp * PI / 365.0_dp * rDayInYear - 1.39_dp)

        ! Compute current time expressed in hours and fractions
        rZone = real(iZone, kind=dp)
        rTime = real(tCurrentTime % iHour, kind=dp) + &
            real(tCurrentTime % iMinute, kind=dp)/60.0_dp + &
            real(tCurrentTime % iSecond, kind=dp)/3600.0_dp - rZone

        ! Calculate geographical positioning parameters (with a "-" sign for
        ! longitudes, according to the US-centered ASCE conventions)
        centralMeridianLongitude = -rZone * 15.0_dp
        if(centralMeridianLongitude < 0.0_dp) then
            centralMeridianLongitude = centralMeridianLongitude + 360.0_dp
        end if
        localLongitude = -lon
        if(localLongitude < 0.0_dp) then
            localLongitude = localLongitude + 360.0_dp
        end if

        ! Compute hour at mid of averaging time
        t1 = real(iAveragingPeriod, kind=dp) / 3600.0_dp
        t = rTime + rZone + t1 / 2.0_dp

        ! Calculate seasonal correction for solar time
        b  = 2.0_dp * PI * (rDayInYear - 81.0_dp) / 364.0_dp
        Sc = 0.1645_dp * sin(2.0_dp * b) - 0.1255_dp * cos(b) - 0.025_dp * sin(b)

        ! Solar time angle at midpoint of averaging time
        omega = (PI/12.0_dp) * ((t + 0.06667_dp * (centralMeridianLongitude - localLongitude) + Sc) - 12.0_dp)

        ! Solar time angle at beginning and end of averaging period
        omega1 = omega - PI * t1 / 24.0_dp
        omega2 = omega + PI * t1 / 24.0_dp

        ! Adjust angular end points to exclude nighttime hours
        omegaS = acos(-tan(lat * PI / 180.0_dp) * tan(rSolarDeclination))    ! Sunset angle
        if(omega1 < -omegaS) then
            omega1 = -omegaS
        end if
        if(omega2 < -omegaS) then
            omega2 = -omegaS
        end if
        if(omega1 > omegaS) then
            omega1 = omegaS
        end if
        if(omega2 > omegaS) then
            omega2 = omegaS
        end if
        if(omega1 > omega2) then
            omega1 = omega2
        end if
        
        ! Compute solar elevation angle
        rSolarElevationAngle = asin( &
            sin(lat*PI/180.0_dp)*sin(rSolarDeclination) + &
            cos(lat*PI/180.0_dp)*cos(rSolarDeclination)*cos(omega) &
        ) * 180.0_dp / PI

        ! Compute reciprocal squared relative distance factor for Sun-Earth
        rInvSqDistCorrection = 1.0_dp + 0.033_dp * cos(2.0_dp * PI * rDayInYear / 365.0_dp)

        ! Estimate extraterrestrial radiation
        rRe = 12.0_dp / PI * SOLAR_CONSTANT * rInvSqDistCorrection * ( &
            (omega2 - omega1)*sin(lat * PI / 180.0_dp)*sin(rSolarDeclination) + &
            cos(lat * PI / 180.0_dp) * cos(rSolarDeclination) * (sin(omega2) - sin(omega1)) &
        )

    end function ExtraterrestrialRadiation


    function NetRadiation(Rg, albedo, fcd, Ea, Ta) result(Rn)

        ! Routine arguments
        real(dp), intent(in)    :: Rg       ! Global (possibly clear-sky) radiation (W/m2)
        real(dp), intent(in)    :: albedo   ! Albedo coefficient
        real(dp), intent(in)    :: fcd      ! Cloudiness coefficient (0 to 1)
        real(dp), intent(in)    :: Ea       ! Water vapor pressure (hPa)
        real(dp), intent(in)    :: Ta       ! Air temperature (K)
        real(dp)                :: Rn       ! Net radiation (W/m2)

        ! Locals
        real(dp)    :: Rns, Rnl     ! Short- and long-wave components of net radiation

        ! Short-wave component of net radiation is the part which is not reflected
        Rns = Rg*(1.0 - albedo)

        ! Long-wave component depends on various things
        Rnl = 5.6722d-8 * fcd * (0.34 - 0.14*SQRT(Ea/10.0)) * Ta**4		! 5.6722e-8 = sigma[MJ / m2 h] * = 2.042e-10 * 1000000 / 3600

        ! Finally, the Net Radiation:
        Rn = Rns - Rnl
    
    end function NetRadiation


    function ClearSkyRadiation(rRe, rZ) result(rRg)

        ! Routine arguments
        real(dp), intent(in)	:: rRe  ! Extraterrestrial radiation (W/m2)
        real(dp), intent(in)	:: rZ   ! Altitude above mean sea level (m)
        real(dp)                :: rRg  ! Clear-sky radiation (W/m2)

        ! Locals
        ! -none-

        ! Compute the information item desired
        rRg = rRe * (0.75_dp + 2.0d-5 * rZ)

    end function ClearSkyRadiation
    
    
    ! Cloudiness estimation from ASCE "Reference evapotranspiration equation"
    function MeasuredCloudiness(rRg, rRgClearSky) result(rFcd)
    
        ! Routine arguments
        real(dp), intent(in)    :: rRg          ! Measured global radiation (W/m2)
        real(dp), intent(in)    :: rRgClearSky  ! Clear-sky radiation estimate (W/m2)
        real(dp)                :: rFcd
        
        ! Locals
        ! --none--
        
        ! Compute the information desired
        rFcd = max(min(1.35_dp * rRg / rRgClearSky - 0.35_dp, 1.0_dp), 0.0_dp)
        
    end function MeasuredCloudiness
    
    
    ! Cloudiness estimation by ECMWF rule
    function EstimatedCloudiness(rRelH) result(rFcd)
    
        ! Routine arguments
        real(dp), intent(in)    :: rRelH    ! Relative humidity (%)
        real(dp)                :: rFcd     ! Cloudiness factor (dimensionless)
        
        ! Locals
        ! --none--
        
        ! Constants (please do not change :) )
        real(dp), parameter     :: RHc = 100.0_dp * 2.0_dp / 3.0_dp
        
        ! Get the information desired
        rFcd = max(0.0_dp, (rRelH - RHc)/(100.0_dp - RHc))**2
        
    end function EstimatedCloudiness
    
    
    function GlobalRadiation(rRgClearSky, rFcd) result(rRg)
        
        ! Routine arguments
        real(dp), intent(in)    :: rRgClearSky
        real(dp), intent(in)    :: rFcd
        real(dp)                :: rRg
        
        ! Locals
        ! --none--
        
        ! Get the information desired
        rRg = rRgClearSky * (rFcd + 0.35_dp) / 1.35_dp
        
    end function GlobalRadiation


    ! Zero-lapse estimation of pressure with respect to height above mean sea level
    function GuessPressure(rHeight, rTa) result(rPa)

        ! Routine arguments
        real(dp), intent(in)    :: rHeight  ! Altitude above mean sea level (m)
        real(dp), intent(in)    :: rTa      ! Air temperature (K)
        real(dp)                :: rPa      ! Site pressure (hPa)

        ! Locals
        real(dp), parameter :: g = 9.80665_dp
        real(dp), parameter :: R = 8.31432_dp
        real(dp), parameter :: M = 0.0289644_dp
        real(dp), parameter :: P0 = 1013.0_dp

        ! Calculate the desired quantity
        rPa = P0 * exp(-g * M * rHeight / (R * rTa))
    
    end function GuessPressure
    
    
    ! Saturation water vapor pressure given air temperature, using
    ! NASA GISS model E formula.
    function EstimateSaturationPressure(rTa) result(rEsat)

        ! Routine arguments
        real(dp), intent(in)    :: rTa      ! Air temperature (K)
        real(dp)                :: rEsat    ! Saturation water pressure (hPa)

        ! Locals
        real(dp)    :: rLatentHeat
        
        ! Constants (please do not change... :) )
        real(dp), parameter     :: LATENT_HEAT_WATER = 2.500d6
        real(dp), parameter     :: LATENT_HEAT_ICE   = 2.834d6

        ! Compute the data item required
        if(rTa > 273.15_dp) then
            ! Water formula
            rLatentHeat = LATENT_HEAT_WATER
        else
            ! Ice formula
            rLatentHeat = LATENT_HEAT_ICE
        end if
        rEsat = 6.1078_dp * exp(rLatentHeat * (7.93252d-6 - 2.166847d-3/rTa))

    end function EstimateSaturationPressure
    
    
    function StableZi(Lat, Temp, H0, Ustar, L, N) result(Zi)

        ! Routine arguments
        real(dp), intent(in)    :: Lat      ! Latitude (degrees)
        real(dp), intent(in)    :: Temp     ! Air temperature (¡C)
        real(dp), intent(in)    :: H0       ! Turbulent sensible heat flux (W/m2)
        real(dp), intent(in)    :: Ustar    ! Friction velocity (m/s)
        real(dp), intent(in)    :: L        ! Obukhov length (m)
        real(dp), intent(in)    :: N        ! Brunt-Vaisala frequency (Hz)
        real(dp)                :: Zi

        ! Locals
        real(dp)    :: rLat
        real(dp)    :: f
        real(dp)    :: Ta
        real(dp)    :: a
        real(dp)    :: b1
        real(dp)    :: b2
        real(dp)    :: b3
        real(dp)    :: b
        real(dp)    :: wt
        real(dp)    :: rc

        ! Constants
        real(dp), parameter :: g = 9.807d0

        ! Check something is to be done
        if(L < 1.d-5 .or. Ustar < 1.d-5 .or. Temp < -40.0_dp) then
            Zi = 1330.0_dp * Ustar  ! Degrade to purely mechanical rough estimate
            return
        end if
        ! From now on, stability is guaranteed

        ! Compute Coriolis parameter
        rLat = Lat * 3.14159265358979d0 / 180.d0
        f    = 2.d0*7.29d-5*SIN(rLat)

        ! Compute temperature in K
        Ta = Temp + 273.15d0

        ! Compute w't'
        rc = 1305.d0 * 273.16d0/Ta
        wt = H0 / rc

        ! Compute Zilitinkevich non-advective function parts
        a  = (f/(0.5d0*Ustar))**2
        b1 = 0.1d0 / L
        b2 = N / (26.d0*Ustar)
        b3 = SQRT(ABS(g*f*wt/Ta)) / (1.7d0*Ustar)
        b  = b1 + b2 + b3

        ! Compute stable estimate of mixing height
        Zi = (SQRT(4.d0*a + b**2) - b)/(2.d0*a)

        ! Accept only if >= than purely mechanical approx
        if(Zi > 2.0*1330.0*Ustar .or. Zi < 0.5*1330.0*Ustar) then
            Zi = 1330.0*Ustar
        else
            Zi = MAX(Zi, 1330.0*Ustar)
        end if

    end function StableZi

    ! **************************************************
    ! * Old Roberto's routines - from original PBL_MET *
    ! * and some CALPUFF routines. Minor editing made. *
    ! * I intentionally refrained from refactoring.    *
    ! **************************************************

        
    Subroutine PBL_33(iland,z0,d,zr,vel,T,Rn,cloud,us,Ts,H0,hlm)
        
        integer     :: iland
        real(dp)    :: z0,d,zr,vel,T,Rn,cloud,us,Ts,H0,hlm
        REAL(dp)    :: alpha(6)
        real(dp)    :: hlmin, ustarmin, rc, rground, alu, tt, S, alp, beta, hk, gg
        real(dp)    :: usn, zz0, aln, d1, d2, d3, ts1, ts2, us_min, uss, uuu
        
        DATA alpha/0.1_dp,0.3_dp,0.5_dp,0.7_dp,1._dp,1.4_dp/
        DATA beta/20._dp/, hk/0.4_dp/, gg/9.807_dp/
        !     -----------------------------------------------------------------
        IF(z0 .LT.0.) z0 = 0.1_dp
        if(iland.LT.1  .OR. iland.GT.6) iland = 4
        
        hlmin    = 1._dp/5._dp
        ustarmin = 0.05_dp
        rc       = 1305._dp*273.15_dp/T
        rground  = 0.8_dp
        
        alu    = LOG((zr-d)/z0)
        tt     = T-273.15_dp
        S      = 1.05_dp*EXP( (6.42_dp-tt)/17.78_dp )
        alp    = alpha(iland)
        
        H0 = ((1._dp-alp)+S)/(1._dp+S) * rground*Rn - beta
        
        IF(H0.GT.0._dp) Then
            usn = hk*vel/alu
            zz0 = z0/zr
            aln = LOG(z0/zr)
            IF(zz0 .LE.0.01_dp) Then
                d1 = 0.128_dp+0.005_dp*aln
            Else
                d1 = 0.107_dp
            Endif
            d2 = 1.95_dp+32.6_dp*(zz0)**0.45_dp
            IF(h0 .LE. 0._dp) Then
                d3 = 0._dp
            Else
                d3 = H0/rc * (hk*gg*zr)/(T*usn**3)
            Endif
            us  = usn * (1._dp + d1*LOG(1._dp + d2*d3))
            IF(us .LT. ustarmin) us = ustarmin
            Ts  = -H0/rc/us
            hlm = hk*gg/T * Ts/us**2
        Else
            Ts1    = 0.09_dp*(1._dp - 0.5_dp*cloud**2)
            Ts2    = hk*T*vel**2/(18.8_dp*zr*gg*alu)
            Ts     = MIN(ts1,ts2)
            us_min = hk/alu*vel
            uss    = 0.5_dp*hk*vel/alu
            uuu    = 1._dp- 4._dp* 4.7_dp*gg*zr*Ts*alu/(hk*T*vel**2)
            IF(uuu.LE.0._dp) Then
                hlm = hlmin
                us  = hk*vel/(alu + 4.7_dp*zr*hlm)
                IF(us .LT.us_min) us = us_min
                H0  = -rc*us*ts
            Else
                uuu = SQRT(uuu)
                us  = uss * (1._dp+uuu)
                IF(us .LT. us_min) us = us_min
                h0  = -rc*us*ts
                hlm = hk*gg/T * Ts/us**2
            Endif
        Endif
    
    END Subroutine PBL_33


    Function Hmix_new(dtime,H0,us,Tm,rc,hold) result(rHmix)
    
        real(dp)    :: dtime,H0,us,Tm,rc,hold
        real(dp)    :: rHmix
        
        integer     :: n_step
        real(dp)    :: dt, hmm, hk1, hk2, hk3, hk4, ggmm
        integer     :: i

        DATA n_step/60/
        
        rHmix = -9999._dp
        dt       = dtime/n_step

        if(rc < -9990._dp) rc = 1200._dp
        hmm  = hold
        DO i=1, n_step
            ggmm = gg(hmm)
            hk1  = dt * F(rc,Tm,ggmm,us,H0,hmm)
            ggmm = gg(hmm+hk1/2._dp)
            hk2  = dt * F(rc,Tm,ggmm,us,H0,hmm+hk1/2._dp)
            ggmm = gg(hmm+hk2/2._dp)
            hk3  = dt * F(rc,Tm,ggmm,us,H0,hmm+hk2/2._dp)
            ggmm = gg(hmm+hk3/2._dp)
            hk4  = dt * F(rc,Tm,ggmm,us,H0,hmm+hk3/2._dp)
            hmm  = hmm + (hk1+2._dp*(hk2+hk3)+hk4)/6._dp
        EndDO
        
        rHmix = hmm
    
    END FUNCTION Hmix_New


    function F(rc,Ta,Gm,Ustar,h0,zi)
    
        ! Routine arguments
        real(dp), intent(in)    :: rc       ! RhoCp
        real(dp), intent(in)    :: Ta       ! Air temperature (K)
        real(dp), intent(in)    :: Gm       ! Value of "Gamma" (temperature lapse rate at old mixing height)
        real(dp), intent(in)    :: Ustar    ! Friction velocity (m/s)
        real(dp), intent(in)    :: H0       ! Turbulent sensible heat flux (W/m2)
        real(dp), intent(in)    :: zi       ! Old convective mixing height
        real(dp)                :: F
    
        ! Locals
        real(dp)    :: L    ! Obukhov length (m)
        real(dp)    :: H0c  ! Revised turbulent sensible heat flux
    
        ! Constants
        real(dp), parameter :: K = 0.4d0    ! von Karman constant
        real(dp), parameter :: G = 9.807d0  ! Gravity acceleration constant
        real(dp), parameter :: A = 0.2d0
        real(dp), parameter :: C = 8.0d0
    
        ! Compute Obukhov length
        H0c = SIGN(MAX(0.01_dp, ABS(H0)), H0)
        L = -rc*Ta*Ustar**3/(K*G*H0c)
    
        ! Compute Gryning-Batchvarova function
        F = H0/(rc*Gm) * &
            1.d0/( &
                zi**2/((1.d0+2.d0*A)*zi-2.d0*L) + &
                C*Ustar**2.d0*Ta/(Gm*G*((1.0d0+A)*zi-L)) &
            )
    
    end function F
    
    
    FUNCTION gg(z)
    
        real(dp)    :: z
        real(dp)    :: gg
        
        gg = 3._dp / (z + 1._dp) - 1.98d-3 + 2.27d-6 * z
        
    END FUNCTION gg


    ! Compute the derivative of the saturation vapor pressure multiplied
    ! by P/0.622; the input temperature is in K.
    FUNCTION D_E_SAT(T) RESULT(DEsat)

        ! Routine arguments
        REAL(dp), INTENT(IN)    :: T
        REAL(dp)                :: DEsat

        ! Locals
        REAL(dp), PARAMETER :: E0 =   0.6112_dp
        REAL(dp), PARAMETER :: a  =  17.67_dp
        REAL(dp), PARAMETER :: T0 = 273.15_dp
        REAL(dp), PARAMETER :: Tb =  29.66_dp

        ! Compute the saturation vapor tension
        DEsat = E0*a*(1._dp/(T-Tb) + (T-T0)/(T-Tb)**2)*EXP(a*(T-T0)/(T-Tb))
!
    END FUNCTION D_E_SAT

    
    ! ==================================================================
    ! Funzione Universale di Similarità
    ! per la velocità del vento nel SL.
    ! ------------------------------------------------------------------
    Function PSIM(zL) result(rPSIM)
    
        real(dp), intent(in)    :: zL
        real(dp)                :: rPSIM
        real(dp)                :: x, y
        
        IF(zL.LT.0.) Then
            ! Situazione convettiva
            x    = (1._dp-16._dp*zL)**0.25_dp
            y    = (1._dp+x)/2._dp
            rPSIM = LOG((1._dp+x*x)/2._dp*y*y) - 2._dp*ATAN(x) + 1.570796_dp
        Else
            ! Situazione stabile
            rPSIM = -17._dp*(1._dp-EXP(-0.29_dp*zL))
        Endif

    END FUNCTION PSIM

    ! ==================================================================
    ! Funzione Universale di Similarità
    ! per la temperatura dell'aria nel SL.
    ! ------------------------------------------------------------------
    Function PSIH(zL) result(rPSIH)
    
        real(dp), intent(in)    :: zL
        real(dp)                :: rPSIH
        real(dp)                :: y
    
        IF(zL.LT.0.) Then
            ! Situazione convettiva
            y    = SQRT(1._dp-16._dp*zl)
            rPSIH = 2._dp*LOG((1._dp+y)/2._dp)
        Else
            ! Situazione stabile
            rPSIH = -17._dp*(1._dp-EXP(-0.29_dp*zL))
        Endif
    
    END FUNCTION PSIH


    !   ==================================================================
    !   Stima del flusso turbolento di calore sensibile e latente, della
    !   velocity supposto noto il flusso di calore nel suolo.
    !
    !   Metodo di Priestley-Taylor modificato
    !   ------------------------------------------------------------------
    Subroutine SURFACE_PT(z0,Tk,Pres,Vel,Rn,G0,H0,HE,us,Ts,hL)
    
        real(dp)    :: z0,Tk,Pres,Vel,Rn,G0,H0,HE,us,Ts,hL
        real(dp)    :: hk, g, ga, zru, alfa, beta
       
        real(dp)    :: Rho_Cp, Scc, zLu, zLu0, alm
        real(dp)    :: AA, psm, psm0
        DATA    hk/0.4_dp/, g/9.81_dp/, ga/4.08d-04/
        DATA    zru/10._dp/,alfa/0.8_dp/,beta/20._dp/
        
        Rho_Cp = 350.14_dp * Pres/Tk
        Scc    = 0.622_dp/Pres * D_E_SAT(Tk)
    
        zLu  = zru*hL
        zLu0 = z0*hL
    
        alm = LOG(zru/z0)
        psm  = PSIM(zLu)
        psm0 = PSIM(zLu0)
    
        AA = ga/Scc
        H0 = ((1._dp-alfa)+AA)/(1._dp+AA)*(Rn-G0)-beta
        HE = alfa/(1._dp+AA)*(Rn-G0)+beta
    
        us   = MAX(hk*Vel/(alm - psm + psm0),0.05_dp)
        Ts   = -H0/Rho_Cp/us
        hL   = hk*g/Tk * Ts/us**2
    
    END SUBROUTINE SURFACE_PT


    ! Semplificazione del metodo proposto da Santanello e Friedl in
    ! Journal of Applied Meteorology, 2003, 42, 851-862
    Function SOIL_HEAT_FLUX(hour,rRn) result(rG0)
    
        real(dp)    :: hour
        real(dp)    :: rRn
        real(dp)    :: rG0
        
        real(dp)    :: A, B, C, pg2, Rap, time
        
        DATA A/0.33_dp/, B/85000._dp/, C/10800._dp/
        DATA pg2/6.2831853_dp/

        time = 3600._dp * (hour - 12._dp)

        Rap  = A * COS(pg2*(time+C)/B)
    
        rG0 = Rap * rRn
    
    END FUNCTION SOIL_HEAT_FLUX


    function SunRiseSunSet(iTimeStamp, lat, lon, zone) result(sunRiseSet)
    
        implicit none
    
        ! Routine arguments
        integer, intent(in)     :: iTimeStamp
        real(dp), intent(in)    :: lat, lon
        integer, intent(in)     :: zone
        real(dp), dimension(2)  :: sunRiseSet
    
        ! Locals
        real(dp)    :: doy
        real(dp)    :: solarDeclination
        real(dp)    :: t, b, Sc
        real(dp)    :: centralMeridianLongitude
        real(dp)    :: localLongitude
        real(dp)    :: omegaZeroElev, tZeroElev1, tZeroElev2
        type(time)  :: tCurrentTime
        type(time)  :: tBaseTime
    
        ! Parameters
        real(dp), parameter :: PI = 4._dp * atan(1._dp)
    
        ! Compute solar declination
        tCurrentTime = fromEpoch(iTimeStamp)
        tBaseTime = time(tCurrentTime % iYear, 1_1, 1_1, 0_1, 0_1, 0_1)
        doy = real(julianDay(tCurrentTime) - julianDay(tBaseTime) + 1, kind=dp)
        solarDeclination = 0.409_dp * sin(2._dp * PI / 365._dp * doy - 1.39_dp)
    
        ! Calculate geographical positioning parameters (with a "-" sign for longitudes, according to ASCE conventions)
        centralMeridianLongitude = -zone*15.0_dp
        if(centralMeridianLongitude < 0.0_dp) then
            centralMeridianLongitude = centralMeridianLongitude + 360.0_dp
        end if
        localLongitude = -lon
        if(localLongitude < 0.0_dp) then
            localLongitude = localLongitude + 360.0_dp
        end if
    
        ! Calculate seasonal correction for solar time
        b  = 2._dp * PI * (doy - 81._dp) / 364.0_dp
        Sc = 0.1645_dp * sin(2.0_dp * b) - 0.1255_dp * cos(b) - 0.025_dp * sin(b)
    
        ! Sunrise and sunset angles
        omegaZeroElev = acos(-tan(lat * PI / 180.0_dp) * tan(solarDeclination))
        tZeroElev1 =  omegaZeroElev * 12.0_dp / PI + 12.0_dp - Sc - 0.06667_dp*(centralMeridianLongitude - localLongitude)
        if(tZeroElev1 < 0._dp) tZeroElev1 = tZeroElev1 + 12.0_dp
        tZeroElev2 = -omegaZeroElev * 12.0_dp / PI + 12.0_dp - Sc - 0.06667_dp*(centralMeridianLongitude - localLongitude)
        if(tZeroElev2 < 0._dp) tZeroElev2 = tZeroElev2 + 12.0_dp
        sunRiseSet(1) = MIN(tZeroElev1, tZeroElev2)
        sunRiseSet(2) = MAX(tZeroElev1, tZeroElev2)
    
    end function SunRiseSunSet
    

    !----------------------------------------------------------------------
    !
    ! --- CALPUFF    Version: 6.42     Level: 960521                  LSTAB
    !
    ! --- PURPOSE:  Calculate a PG class given the Monin-Obukhov length
    !               and the surface roughness from Golder's 1972 curves
    !
    ! --- Taken from CTDMplus subroutine of the same name
    !
    ! ASSUMPTIONS: THE DIVIDING LINES BETWEEN CATEGORIES ARE ASSUMED TO BE
    !               LINEAR.
    !
    ! LIMITATIONS: THIS FUNCTION IS ONLY VALID FOR 0.01 <= Z0 <= 0.5(M).
    !              HOWEVER, RESULTS ARE EXTENDED TO OTHER VALUES OF Z0 BY
    !              USING Z0 = 0.01 IF Z0 < 0.01 M, AND BY USING Z0 = 0.5
    !              IF Z0 > 0.5 M.
    !
    ! --- INPUTS:
    !       EL      REAL    MONIN-OBUKHOV LENGHT (M)
    !       ZR0     REAL    SURFACE ROUGHNESS LENGTH (M)
    !
    ! --- OUTPUT:
    !       LSTAB   INT     P-G STABILITY CATEGORY 1=A, 2=B, ETC.
    !
    ! CALLING ROUTINES: SEQMOD
    !
    ! EXTERNAL ROUTINES: NONE
    !
    ! INTERNAL FUNCTIONS:
    !       XL - EQUATION OF DIVIDING LINE BETWEEN P-G STABILITY CLASSES
    !
    ! INTRINSIC FUNCTIONS: ALOG
    !
    ! REFERENCES:
    !       GOLDER, D. (1972): RELATIONS AMONG STABILITY PARAMETERS IN THE
    !                       SURFACE LAYER, BOUNDARY-LAYER METEOROLOGY, 3:56.
    !
    !-----------------------------------------------------------------------
    !
    integer function lstab(el,zr0)
    
        REAL(dp)    :: EL, XEL, XL, Z0, ZR0
        real(dp)    :: y, xm, b

        XL(Y,XM,B)=XM/(LOG(Y)-B)

        Z0 = ZR0
        IF(Z0 .GT. 0.5_dp) Z0 = 0.5_dp
        IF(Z0 .LT. 0.01_dp) Z0 = 0.01_dp
        IF(EL .LT. 0.0_dp) THEN
            XEL = -EL
            IF(XEL .LE. XL(Z0,-70.0_dp,4.35_dp)) THEN
!               STABILITY A
                LSTAB=1
            ELSE IF(XEL .LE. XL(Z0,-85.2_dp,0.502_dp)) THEN
!               STABILITY B
                LSTAB=2
            ELSE IF(XEL .LE. XL(Z0,-245._dp,0.050_dp)) THEN
!               STABILITY C
                LSTAB=3
            ELSE
!               STABILITY D
                LSTAB=4
            ENDIF
        ELSE
            IF(EL .GE. XL(Z0,-327._dp,0.627_dp)) THEN
!               STABILITY D
                LSTAB=4
            ELSE IF(EL .GE. XL(Z0,-70.0_dp,0.295_dp)) THEN
!               STABILITY E
                LSTAB=5
            ELSE
!               STABILITY F
                LSTAB=6
            ENDIF
        ENDIF

    END FUNCTION lstab


    !----------------------------------------------------------------------
    !
    ! --- CALPUFF    Version: 6.42     Level: 960521                 STAB2L
    ! ---            D. Strimaitis
    !
    ! --- PURPOSE:  Calculate a Monin-Obukhov length given the PG class
    !               and the surface roughness from Golder's 1972 curves
    !               as presented in Seinfeld, 1986 (Atm. Chem. & Phys. of
    !               Air Poll.)
    !
    ! LIMITATIONS: THIS FUNCTION IS VALID FOR 0.01 <= Z0 <= 0.5(M).
    !              HOWEVER, RESULTS ARE EXTENDED TO OTHER VALUES OF Z0 BY
    !              USING Z0 = 0.01 IF Z0 < 0.01 M, AND BY USING Z0 = 0.5
    !              IF Z0 > 0.5 M.
    !
    ! --- INPUTS:
    !       ISTAB - integer    - P-G stability category 1=A, 2=B, ETC.
    !         ZR0 - real       - Surface roughness length (m)
    !
    ! --- OUTPUT:
    !         ELI - real       - 1/Monin-Obukhov length (1/m)
    !
    ! --- STAB2L called by:  RDPLM
    ! --- STAB2L calls:      none
    !----------------------------------------------------------------------
    subroutine stab2l(istab,zr0,eli)
    
        integer     :: istab
        real(dp)    :: zr0, eli, z0

        real(dp) :: a(6),b(6)
        
        data a/-.096_dp,-.037_dp,-.002_dp,0.0_dp,.004_dp,.035_dp/
        data b/.029_dp,.029_dp,.018_dp,0.0_dp,-.018_dp,-.036_dp/
        
        z0 = zr0
        if(z0.GT.0.5_dp) z0 = 0.5_dp
        if(z0.LT.0.01_dp) z0 = 0.01_dp
        eli=a(istab)+b(istab)*LOG10(z0)
    
    end subroutine stab2l

end module estimate

