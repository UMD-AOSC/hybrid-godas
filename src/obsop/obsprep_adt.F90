program obsprep_adt
  use obscom_obsio
  use obscom_grid
  use netcdf
  use datetime_module
  use running_stats_mod

  implicit none


  integer :: obid = 2100

  ! variables read in from command line
  character(len=1024) :: obsfile
  character(len=1024) :: outfile

  ! other variables
  type(obsio_nc) :: obsio
  type(observation), allocatable :: obsout(:)

  integer :: i, x, y
  real :: r

  character(len=*),parameter :: nml_file="obsprep.nml"
  integer :: ncid, d_t, vid
  integer, allocatable :: tmp_i(:)
  real(8), allocatable :: tmp_d(:)
  character(len=1024) :: tmp_str
  real :: scale
  integer :: fill

  integer :: dy, ss

  integer :: nobs
  real, allocatable :: obs_val(:)
  real, allocatable :: obs_lat(:), obs_lon(:)
  real, allocatable :: obs_time(:)
  integer, allocatable :: obs_qc(:)
  real :: err

  type(datetime) :: basedate, basedate2, date2
  type(timedelta) :: time

  type bin
     type(running_stats) :: val
     type(running_stats) :: time
     type(running_stats) :: lon
     type(running_stats) :: lat
  end type bin
  type(bin), allocatable :: bins(:,:)

  type(running_stats) :: stats_cnt
  type(running_stats) :: stats_val
  type(running_stats) :: stats_sprd

  integer :: rm_land

  ! use this to get the repository verstion at compile tim
#ifndef CVERSION
#define CVERSION "Unknown"
#endif

  print *, "------------------------------------------------------------"
  print *, "  ADT observation preparation"
  print *, ""
  print *, " version:  ", CVERSION
  print *, "------------------------------------------------------------"


  ! read the command line arguments
  i = command_argument_count()
  if (i /= 2) then
     print *, 'ERROR: call with "obsprep_adt <inputfile> <outputfile>" '
     stop 1
  end if
  call get_command_argument(1, value=obsfile)
  call get_command_argument(2, value=outfile)
  print *, "In:  ", trim(obsfile)
  print *, "Out: ", trim(outfile)


  ! read the grid
  call grid_init(nml_file)
  print *, ""


  ! read the observations
  ! -------------------------------------------------------
  call check(nf90_open(obsfile, nf90_nowrite, ncid))
  call check(nf90_inq_dimid(ncid, "time", d_t))
  call check(nf90_inquire_dimension(ncid, d_t, len=nobs))
  print *, nobs, "observations"
  print *, ""
  allocate(obs_lat(nobs), obs_lon(nobs))
  allocate(obs_time(nobs))
  allocate(obs_val(nobs))
  allocate(obs_qc(nobs))
  allocate(tmp_i(nobs))
  allocate(tmp_d(nobs))
  obs_qc = 0


  ! get the base date 
  call check(nf90_inq_varid(ncid, 'time_mjd', vid))
  call check(nf90_get_att(ncid, vid, "units", tmp_str))
  if (trim(tmp_str) /= "days since 1858-11-17 00:00:00 UTC") then
     print *, "ERROR, unexpected datetime format"
     stop 1
  end if
  basedate=datetime(1858,11,17,0,0,0,0)

  ! get time
  call check(nf90_get_var(ncid, vid, tmp_d))
  do i=1,nobs
     dy= int(tmp_d(i))
     ss= mod(tmp_d(i), 1.0)*24*60*60.0
     date2 = basedate + timedelta(dy,0,0,ss)
     if(i==1) then
        basedate2 = datetime(date2%getYear(), date2%getMonth(), date2%getDay())
        print *, "base datetime: ", basedate2%isoformat()
        print *, ""
  
     end if
     time = date2-basedate2
     obs_time(i) = time%total_seconds()/3600.0
  end do
  
  
  ! lat
  call check(nf90_inq_varid(ncid, 'lat', vid))
  call check(nf90_get_att(ncid, vid, 'scale_factor', scale))
  call check(nf90_get_att(ncid, vid, '_FillValue', fill))
  call check(nf90_get_var(ncid, vid, tmp_i))
  obs_lat = tmp_i * scale
  where (tmp_i == fill)
     obs_qc = 1
  end where

  ! lon
  call check(nf90_inq_varid(ncid, 'lon', vid))
  call check(nf90_get_att(ncid, vid, 'scale_factor', scale))
  call check(nf90_get_att(ncid, vid, '_FillValue', fill))
  call check(nf90_get_var(ncid, vid, tmp_i))
  obs_lon = tmp_i * scale
  where (tmp_i == fill)
     obs_qc = 1
  end where

  ! value
  call check(nf90_inq_varid(ncid, 'adt_xgm2016', vid))
  call check(nf90_get_att(ncid, vid, 'scale_factor', scale))
  call check(nf90_get_att(ncid, vid, '_FillValue', fill))
  call check(nf90_get_var(ncid, vid, tmp_i))
  obs_val = tmp_i * scale
  where (tmp_i == fill)
     obs_val = 0.0
     obs_qc = 1
  end where


  ! TODO remove invalid obs
  nobs = nobs - sum(obs_qc)


  ! print some stats
  print *, "Invalid observations: ", sum(obs_qc)
  print *, "Valid observations:   ", nobs


  ! perform superobbing
  !------------------------------------------------------------
  allocate(bins(grid_nx, grid_ny))
  do i=1, nobs
     ! skip if bad
     if(obs_qc(i) > 0) cycle

     ! get the closest grid point to the ob
     call grid_ll2xy(obs_lat(i), obs_lon(i), x, y)

     ! in the case the long has wrapped around
     if (bins(x,y)%val%count > 0 .and. &
          abs(obs_lon(i) - bins(x,y)%lon%mean()) > 180) then
        if (obs_lon(i) < bins(x,y)%lon%mean()) then
           obs_lon(i) = obs_lon(i) + 360.0
        else if(obs_lon(i) > bins(x,y)%lon%mean()) then
           obs_lon = obs_lon(i) - 360
        end if
     end if

     ! add to superob bins
     call bins(x,y)%time%add(obs_time(i))
     call bins(x,y)%lat%add(obs_lat(i))
     call bins(x,y)%lon%add(obs_lon(i))
     call bins(x,y)%val%add(obs_val(i))
  end do


  ! output the data
  !------------------------------------------------------------
  allocate(obsout(nobs))
  rm_land = 0
  i = 0
  do x=1,grid_nx
     do y=1,grid_ny
        if(bins(x,y)%val%count==0) cycle ! no obs at this grid point
        if(grid_mask(x,y) == 0) then ! grid point on land, skip
           rm_land = rm_land + 1
           cycle
        end if
        i = i + 1

        obsout(i)%id   = 2100 ! TODO dont hardcode this
        obsout(i)%plat = 1000 ! TODO dont hardcode this, vary with platform
        obsout(i)%lat  = bins(x,y)%lat%mean()
        obsout(i)%lon  = bins(x,y)%lon%mean()
        obsout(i)%lat  = bins(x,y)%lat%mean()
        obsout(i)%dpth = 0.0
        obsout(i)%hr   = bins(x,y)%time%mean()
        obsout(i)%val  = bins(x,y)%val%mean()

        ! TODO, dont hardcode error offsets
        r = 1.0/sqrt(bins(x,y)%val%count * 1.0)
        err = max(r*0.5, sqrt(bins(x,y)%val%variance()))
!        err = r*0.2 + (1-r)*sqrt(bins(x,y)%val%variance())
        obsout(i)%err  = err
        
        call stats_cnt%add(bins(x,y)%val%count * 1.0)
        call stats_val%add(bins(x,y)%val%mean())
        call stats_sprd%add(err)
     end do
  end do

  print *, rm_land, "removed for being on land"
  print *, i, "observations after superobbing"
  print *, ""
  print *, "count  min/mean/max", stats_cnt%min(), stats_cnt%mean(), stats_cnt%max()
  print *, "value  min/mean/max", stats_val%min(), stats_val%mean(), stats_val%max()
  print *, "spread min/mean/max", stats_sprd%min(), stats_sprd%mean(), stats_sprd%max()


  if(i > 0) then
     call obsio%write(outfile, obsout(1:i), basedate2)
  end if


contains



  subroutine check(status)
    integer, intent(in) :: status
    if (status /= nf90_noerr) then
       print *, status
       print *, trim(nf90_strerror(status))
       stop 1
    end if
  end subroutine check

end program


