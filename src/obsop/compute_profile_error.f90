MODULE compute_profile_error

IMPLICIT NONE

! The cmpTz subroutine in this module uses the 3DVar-GODAS method to
! estimate the vertical error distribution for temperature (or salinity)
! profiles. The errors are based on the gradient of the profile at each depth
! using a centered-difference estimate.
!
! NOTE: The code ASSUMES that the profiles are only defined on the model grid
!       levels. The behavoir when that is not the case (e.g. if the raw observation
!       profiles were used) is unknown.
!
! Authors:
!       Steve Penny
!       David Behrginger (Original author of C code on which this module is based)
!

! the vertical temperature gradient is rescaled to vary between 0.0 and 1.0
! so to set the std errors ultimately between 1.0 and 2.5 use
! SE0 = 1.0 and SEF = 1.5
! then EVR = 1/(se*se)
! if std error = 0.5, then EVR = 4.0
!                1.0             1.0
!                1.5             0.4444
!
REAL,    PARAMETER :: maxerr = 999.0 !STEVE: large number assigned as max error
REAL,    PARAMETER :: teps = 0.00005 !epsilon(1.0)  !STEVE: a small number
INTEGER, PARAMETER :: kav = 5        !STEVE: indicates how many gridpoints to use in the vetical averaging
!LOGICAL :: DO_SCALEDZ=.false.
LOGICAL :: DO_SQRT=.false.
LOGICAL :: dodebug=.false.

PUBLIC :: cmpTz

CONTAINS

! Compute the gradient of t at the model levels. Also perform some other computations
! tz is the scaling to apply to the se error profiles
! a contains 1 / se**2

SUBROUTINE cmpTz(se,se0,seF,t,z,kd,spv)
!  USE common, ONLY: r_size
  INTEGER,PARAMETER :: r_size=kind(0.0e0)
  INTEGER ::  k, kk, km, kp, kv2, cnt
  REAL :: tzmn, tzmx
  REAL(r_size),DIMENSION(kd), INTENT(OUT) :: se  ! standard deviations (errors)
  REAL(r_size), INTENT(IN) :: se0
  REAL(r_size), INTENT(IN) :: seF
  REAL(r_size),DIMENSION(kd), INTENT(IN)  :: t   ! temperature values
  REAL(r_size),DIMENSION(kd), INTENT(IN)  :: z   ! depths
  INTEGER, INTENT(IN) :: kd              ! total number (depth) of levels
  REAL(r_size), INTENT(IN) :: spv ! Spurious/missing value
  REAL,DIMENSION(kd) :: tz, tw

  if (dodebug) then
    print *, "============================================"
    print *, "Inputs ::"
    print *, "t   = ", t
    print *, "z   = ", z
    print *, "kd  = ", kd
    print *, "spv = ", spv
    print *, "============================================"
  endif

  ! Check input quality:
  if (MAXVAL(z) < TINY(1.0d0)) then
    print *, "cmpTz :: input depths are zero, check input arguments. EXITING..."
    STOP(13)
  endif

  ! Estimate profile gradients
  ! i.e. every level is linearly fit from below and above
  do k = 2,kd-1
    if (t(k-1) /= spv .AND. t(k+1) /= spv) then
      ! Estimate profile gradient with centered-difference scheme
      tz(k) = (t(k-1) - t(k+1)) / (z(k+1) - z(k-1))
      ! ensure non-negative gradient
      if (tz(k) < 0.0) then
        tz(k) = 0.0
      endif
    else
      tz(k) = spv
    endif 
  enddo
  tz(1) = spv
  tz(kd) = spv

  ! For all levels with obs data,
  ! Fix levels in which gradients end up as spurious values (most likely due to missing values in obs data)
  ! Use 'nearest neighbor' method to assign the gradient value
  do k = 1,kd

    ! if one of the values was computed with a missing value, then
    ! we have to go back and find the nearest non-missing value to do the computation of the gradient
    if (tz(k) == spv .AND. t(k) /= spv) then

      ! k minus
      km = -1;
      ! Go back to the first non-missing value
      do kk = k,-1,1
        if (tz(kk) /= spv) then
          km = kk
          exit
        endif
      enddo 

      ! k plus
      kp = kd;
      ! Go forward to the first non-missing value
      do kk = k+1,kd-1
        if (tz(kk) /= spv) then
          kp = kk
          exit
        endif
      enddo

      ! if k-minus and k-plus are in the range of observed levels,
      if (km >= 1 .AND. kp <= kd) then

        ! assign the value to level k of the nearest neighbor (km or kp)
        if (k-km <= kp-k) then
          tz(k) = tz(km)
        else
          tz(k) = tz(kp)
        endif

      ! otherwise, handle the boundary cases
      elseif (km >= 1) then
        tz(k) = tz(km)
      elseif (kp <= kd) then
        tz(k) = tz(kp)
      else 
        tz(k) = spv
      endif

    endif
  enddo

  do k = 1,kd
    if (tz(k) == spv .AND. t(k) /= spv) then
      tz(k) = 0.0
    endif
  enddo

  if (DO_SQRT) then
    ! take square root of tz
    WHERE(tz /= spv .AND. tz > 0.0) tz = SQRT(tz)
  endif

  !STEVE: This averages consecutive tz's to smooth out the observation errors
  ! kav identifies how many gridpoints to use in the averaging window.
  if (kav > 1) then
    kv2 = kav / 2
    do k = 1, kd
      if (tz(k) /= spv) then
        if (k-kv2 >= 1) then
          km = k-kv2
        else
          km = 1
        endif
        cnt = 1;
        tw(k) = tz(k)
        do kk = k-1,-1,km
          if (tz(kk) /= spv) then
            tw(k) = tw(k) + tz(kk)
            cnt=cnt+1
          else
            exit
          endif
        enddo 

        if (k+kv2 <= kd) then
          kp = k+kv2
        else
          kp = kd
        endif
        do kk = k+1,kp
          if (tz(kk) /= spv) then
            tw(k) = tw(k) + tz(kk)
            cnt=cnt+1
          else
            exit
          endif
        enddo
        if (cnt>1) then
          tw(k) = tw(k)/REAL(cnt);
        endif
      else 
        tw(k) = spv
      endif
    enddo
    do k = 1,kd
      tz(k) = tw(k)
    enddo
  endif

  ! Find the minimum and maximum tz, and rescale so tz is between
  ! 0 and 1, before applying to the errors
  if (dodebug) print *, "pre-rescaled tz = ", tz
  tzmn = abs(spv);
  tzmx = -abs(spv);
  do k = 1,kd
    if (tz(k) /= spv) then
      if (tzmn >= tz(k)) then
        tzmn = tz(k)
      endif
      if (tzmx <= tz(k)) then
        tzmx = tz(k)
      endif
    endif
  enddo
  tzmx = tzmx-tzmn
  if (dodebug) then
    print *, "tzmn = ", tzmn
    print *, "tzmx = ", tzmx
  endif
  if (tzmx < teps) then
    WHERE (tz /= spv) tz = 0.0
    if (dodebug) print *, "WARNING :: zeroed out tz"
  else
    do k = 1,kd
      if (tz(k) /= spv) then
        tz(k) = tz(k)-tzmn
        tz(k) = tz(k)/tzmx
      endif
    enddo
  endif
  if (dodebug) print *, "post-rescaled tz = ", tz

  ! tz is used only for scaling the standard deviation (error) by depth, so it's not needed
  ! if the observational errors are read in from the netcdf obs file

  if (dodebug) print *, "t  = ", t
  if (dodebug) print *, "tz = ", tz
  do k = 1,kd
    if (t(k) /= spv) then
      ! specify the standard deviation (obs error)
      ! The result is scaled in the range 
      ! from se0 to se0+seF.
      se(k) = se0 + seF * tz(k)
    else 
      se(k) = maxerr
    endif
    
    !STEVE: check for NaN's, if we have one, assign average of se0 and seF as error
    if(se(k) /= se(k)) then !NaN check
      se(k) = (se0+se0+seF)/2.0 !STEVE: temporary fix (ISSUE)
    endif

  enddo
  if (dodebug) print *, "se = ", se

END SUBROUTINE cmpTz

END MODULE compute_profile_error
