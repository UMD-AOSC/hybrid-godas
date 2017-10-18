program obsprep_sst
  use obscom_obsio
  use obscom_grid
  use obsprep_sst_gds2
  use datetime_module

#ifdef __INTEL_COMPILER
  use ifport
#endif

  implicit none

  ! variables read in from namelist
  integer :: min_err_lvl = 4
  real :: err_base = 0.5
  real :: err_sses = 0.0
  real :: err_superob = 0.0

!  real :: time_offset = 0.0
  integer :: obid = 2210
  integer :: platid = 1000
  real :: bias_adj = -1

  real :: thinning(2)  = (/0.0,0.0/)  
  real :: thinning_eq  = 10.0

  ! variables read in from command line
  character(len=1024) :: obsfile
  character(len=1024) :: outfile

  ! other  variables
  type(datetime) :: basedate
  character(len=1024) :: nml_file
  integer :: unit
  integer :: i, x, y
  real :: r, val_min, val_max, val_avg, err_min, err_max, err_avg, sprd_ave, sprd_max, err
  integer :: sprd_cnt
  type(sst_data), allocatable :: obsin(:)
  type(observation), allocatable :: obsout(:)
  integer :: rm_thin, rm_land, rm_superob
  type(obsio_nc) :: obsio
  
  integer, allocatable :: bin_cnt(:,:)
  real,    allocatable :: bin_val(:,:)
  real,    allocatable :: bin_err(:,:)
  real,    allocatable :: bin_m2(:,:)
  real,    allocatable :: bin_lat(:,:)
  real,    allocatable :: bin_lon(:,:)
  real,    allocatable :: bin_time(:,:)
  
  namelist /obsprep_sst_nml/ obid, platid, min_err_lvl, err_base, err_sses,&
       err_superob, bias_adj, thinning, thinning_eq

  print *, "------------------------------------------------------------"
  print *, "  SST (GDS2.0 GHRSST format) observation preparation"
  print *, "------------------------------------------------------------"
  nml_file = "obsprep.nml"

  ! read in command line arguments
  i = command_argument_count()
  if (i /= 2) then
     print *, 'ERROR: call with "obsprep_sst <inputfile> <outputfile>" '
     stop 1
  end if
  call get_command_argument(1, value=obsfile)
  call get_command_argument(2, value=outfile)
  print *, "In:  ", trim(obsfile)
  print *, "Out: ", trim(outfile)
  print *, ""

  !read the namelist
  open(newunit=unit, file=nml_file)
  read(unit, obsprep_sst_nml)
  close(unit)
  print obsprep_sst_nml
  print *, ""

  ! read in the grid
  call grid_init(nml_file)

  
  ! read the observations
  print *, "------------------------------------------------------------"
  call read_sst_gds2_nc(obsfile, basedate, obsin)

  if (size(obsin) == 0) then
     print *, "WARNING: no valid observations were read in"
     stop 0
  end if

  ! place each ob into bins, keeping track of running mean for lat/lon/err/val and variance for val
  print *, ""
  print *, "Superobbing..."
  allocate(bin_cnt(grid_nx, grid_ny))

  allocate(bin_val(grid_nx, grid_ny))
  allocate(bin_err(grid_nx, grid_ny))
  allocate(bin_lat(grid_nx, grid_ny))
  allocate(bin_lon(grid_nx, grid_ny))
  allocate(bin_time(grid_nx, grid_ny))

  allocate(bin_m2( grid_nx, grid_ny))
  bin_cnt = 0
  bin_val = 0.0
  bin_err = 0.0
  bin_m2  = 0.0
  bin_lat = 0.0
  bin_lon = 0.0
  bin_time= 0.0
  do i=1, size(obsin)
     ! get the closest grid point to the ob
     call grid_ll2xy(obsin(i)%lat, obsin(i)%lon, x, y)

     ! in case the lon has wrapped around
     if (bin_cnt(x,y) .gt. 0 .and. abs(obsin(i)%lon - bin_lon(x,y)) > 180) then
        if (obsin(i)%lon < bin_lon(x,y)) then
           obsin(i)%lon = obsin(i)%lon + 360.0
        else if (obsin(i)%lon > bin_lon(x,y)) then
           obsin(i)%lon = obsin(i)%lon - 360.0
        end if
     end if

!     if (bin_cnt(x,y) .gt. 0 .and. abs(obsin(i)%lon - bin_lon(x,y)) > 180) then
!        print *, obsin(i)%lon, bin_lon(x,y)
!        stop 1
!     end if

     bin_cnt(x,y) = bin_cnt(x,y) + 1
     r = obsin(i)%val - bin_val(x,y)
     bin_val(x,y) = bin_val(x,y) + r/bin_cnt(x,y)
     bin_m2(x,y)  = bin_m2(x,y)  + r*(obsin(i)%val - bin_val(x,y))
     bin_err(x,y) = bin_err(x,y) + (obsin(i)%err - bin_err(x,y))/bin_cnt(x,y)
     bin_time(x,y)= bin_time(x,y)+ (obsin(i)%time- bin_time(x,y))/bin_cnt(x,y)
     bin_lon(x,y) = bin_lon(x,y) + (obsin(i)%lon - bin_lon(x,y))/bin_cnt(x,y)
     bin_lat(x,y) = bin_lat(x,y) + (obsin(i)%lat - bin_lat(x,y))/bin_cnt(x,y)

  end do

 
  ! output the data
  allocate(obsout(count(bin_cnt > 0)))
  rm_superob = size(obsin) - size(obsout)
  rm_land = 0
  rm_thin = 0
  val_min = 1e10
  val_max = -1e10
  val_avg = 0.0
  err_min = 1e10  
  err_max = -1e10
  err_avg = 0.0
  sprd_ave = 0.0
  sprd_max = 0.0
  sprd_cnt = 0
  i = 0
  do x =1,grid_nx
     do y = 1, grid_ny
        if(bin_cnt(x,y) == 0) cycle  ! no obs at this grid point
        if(grid_mask(x,y) == 0) then ! grid point on land, skip
           rm_land = rm_land+1
           cycle
        end if
        
        if (sum(thinning) > 0.0) then
           if (thin(grid_lats(x,y))) then
              rm_thin = rm_thin + 1
              cycle
           end if
        end if

        i = i + 1
        obsout(i)%id   = obid
        obsout(i)%plat = platid
        obsout(i)%lat  = bin_lat(x,y)
        obsout(i)%lon  = bin_lon(x,y)
        ! obsout(i)%lat  = grid_lats(x,y) !if we just want the grid center
        ! obsout(i)%lon  = grid_lons(x,y)
        obsout(i)%dpth = 0.0    
        obsout(i)%hr   = bin_time(x,y)
        obsout(i)%val  = bin_val(x,y) - 273.15 + bias_adj

        err = err_base + bin_err(x,y)*err_sses
        if(bin_cnt(x,y) > 1 ) then
           r = sqrt(bin_m2(x,y)/(bin_cnt(x,y)-1))
           if (err_superob > 0) err = err + r*err_superob

           ! statistics on the value spread to print later
           if(r > sprd_max) sprd_max = r
           sprd_ave = sprd_ave + (r-sprd_ave)/i
           sprd_cnt = sprd_cnt + 1
        end if
        obsout(i)%err = err

        ! calculate some statistics to print later
        val_max = max(val_max, obsout(i)%val)
        val_min = min(val_min, obsout(i)%val)
        val_avg = val_avg + (obsout(i)%val - val_avg) / i
        err_max = max(err_max, obsout(i)%err)
        err_min = min(err_min, obsout(i)%err)
        err_avg = err_avg + (obsout(i)%err - err_avg) / i
     end do
  end do

  if(i > 0) then
     call obsio%write(outfile, obsout(1:i), basedate)
  end if

  r = i*100.0/size(obsin)
  print *, " maxsprd:", sprd_max
  print *, " avesprd:", sprd_ave
  print *, ""

  print *, "Final observations: "
  print *, " count:  ", i,"  (",nint(r),"%)"
  print *, " val min/avg/max: ", val_min, val_avg, val_max
  print *, " err min/avg/max: ", err_min, err_avg, err_max

  print *, ""
  print *, "removed for superobbing: ", rm_superob
  print *, "removed for ON LAND:     ", rm_land
  print *, "removed for thinning:    ", rm_thin


contains


  function thin(lat) result(v)
    real, intent(in) :: lat
    logical :: v
    real :: cutoff
    cutoff = (thinning(2)-thinning(1))*exp(-0.5 * lat**2 / (thinning_eq**2)) + thinning(1)
    v = rand() <= cutoff
  end function thin

end program obsprep_sst
