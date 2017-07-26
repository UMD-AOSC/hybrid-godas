program obsprep_sst
  use obscom_obsio
  use obscom_grid
  use obsprep_sst_pathfinder
  
  implicit none

  ! variables read in from namelist
  character(len=:), allocatable :: obsfile
  character(len=:), allocatable :: outfile
  integer :: min_err_lvl = 4
  real :: err_base = 0.5
  real :: err_superob = 0.5
  real :: time_offset = 0.0
  integer :: obid = 2210
  integer :: platid = 1000
  real :: min_wind = -1
  real :: bias_adj = 0.0

  character(len=1024) :: nml_file
  integer :: unit
  integer :: i, x, y
  real :: r, val_min, val_max, sprd_ave, sprd_max
  integer :: sprd_cnt
  type(avhrr_data), allocatable :: obsin(:)
  type(observation), allocatable :: obsout(:)
  integer :: rm_thin, rm_land
  type(obsio_nc) :: obsio
  
  integer, allocatable :: bin_cnt(:,:)
  real,    allocatable :: bin_val(:,:)
  real,    allocatable :: bin_m2(:,:)
  real,    allocatable :: bin_lat(:,:)
  real,    allocatable :: bin_lon(:,:)

  
  namelist /obsprep_sst_nml/ obsfile, outfile, obid, platid, min_err_lvl, err_base,&
       err_superob, time_offset, min_wind, bias_adj

  print *, "------------------------------------------------------------"
  print *, "  SST (AVHRR Pathfinder) observation preparation"
  print *, "------------------------------------------------------------"

  nml_file = "obsprep_sst.nml"
  call grid_init(nml_file)

  ! read the namelist
  allocate(character(len=1024) :: obsfile)
  allocate(character(len=1024) :: outfile)
  open(newunit=unit, file=nml_file)
  read(unit, obsprep_sst_nml)
  close(unit)
  obsfile=trim(obsfile)
  outfile=trim(outfile)
  print *, ""
  print obsprep_sst_nml
  print *, ""

  ! read the observations
  call read_avhrr_pathfinder_nc(obsfile, min_err_lvl, obsin)

  ! place each ob into bins, keeping track of running mean and variance
  print *, "Superobbing..."
  allocate(bin_cnt(grid_nx, grid_ny))
  allocate(bin_val(grid_nx, grid_ny))
  allocate(bin_m2( grid_nx, grid_ny))
  allocate(bin_lat(grid_nx, grid_ny))
  allocate(bin_lon(grid_nx, grid_ny))
  bin_cnt = 0
  bin_val = 0
  bin_m2  = 0.0
  bin_lat = 0.0
  bin_lon = 0.0
  val_min=1e10
  val_max=-1e10
  do i=1, size(obsin)
     ! ignore ob if not windy enough
     if( (min_wind > 0) .and. (obsin(i)%wnd < min_wind)) cycle

     ! get the closest grid point to the ob
     call grid_ll2xy(obsin(i)%lat, obsin(i)%lon, x, y)
     bin_cnt(x,y) = bin_cnt(x,y) + 1
     r = obsin(i)%val - bin_val(x,y)
     bin_val(x,y) = bin_val(x,y) + r/bin_cnt(x,y)
     bin_m2(x,y)  = bin_m2(x,y)  + r*(obsin(i)%val - bin_val(x,y))
     if(obsin(i)%val > val_max) val_max = obsin(i)%val
     if(obsin(i)%val < val_min) val_min = obsin(i)%val
  end do

  print *, ""
  print *, "Original observations:"
  print *, "   count:  ", size(obsin)
  print *, "   minval: ", val_min
  print *, "   maxval: ", val_max
  
  ! output the data
  allocate(obsout(count(bin_cnt > 0)))
  rm_thin = size(obsin) - size(obsout)
  rm_land = 0
  val_min = 1e10
  val_max = -1e10
  sprd_ave = 0.0
  sprd_max = 0.0
  sprd_cnt = 0
  i = 0
  do x =1,grid_nx
     do y = 1, grid_ny
        if(bin_cnt(x,y) == 0) cycle
        if(grid_mask(x,y) == 0) then
           rm_land = rm_land+1
           cycle
        end if
        if(bin_val(x,y) > val_max) val_max = bin_val(x,y)
        if(bin_val(x,y) < val_min) val_min = bin_val(x,y)
        i = i + 1
        obsout(i)%id   = obid
        obsout(i)%plat = platid
        obsout(i)%lat  = grid_lats(x,y)
        obsout(i)%lon  = grid_lons(x,y)
        obsout(i)%dpth = 0.0
        obsout(i)%hr   = time_offset
        obsout(i)%val  = bin_val(x,y) - 273.15 + bias_adj
        obsout(i)%err  = err_base
        if(bin_cnt(x,y) > 1 .and. err_superob > 0) then
           r = sqrt(bin_m2(x,y)/(bin_cnt(x,y)-1))
           if(r > sprd_max) sprd_max = r
           sprd_ave = sprd_ave + r
           sprd_cnt = sprd_cnt + 1
           obsout(i)%err = obsout(i)%err + r*err_superob
        end if
     end do
  end do

  call obsio%write(outfile, obsout(1:i))
  r = size(obsout)*100.0/size(obsin)
  print *, ""
  print *, " Final observations: "
  print *, "   count:  ", size(obsout),"  (",nint(r),"%)"
  print *, "   minval: ", val_min
  print *, "   maxval: ", val_max
  print *, ""
  print *, "   maxsprd:", sprd_max
  print *, "   avesprd:", sprd_ave / sprd_cnt
  print *, ""
  print *, "   removed for thinning: ", rm_thin
  print *, "   removed for ON LAND:  ", rm_land
end program obsprep_sst
