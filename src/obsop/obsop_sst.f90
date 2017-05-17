program obsop_sst
  use netcdf
  use kdtree
  use obsio_nc_mod
  use read_avhrr_pathfinder
  implicit none

  integer :: nx, ny

  ! kd-tree holding all grid lat/lon points
  type(kd_root) :: ll_kdtree
  integer, allocatable :: ll_kdtree_x(:)
  integer, allocatable :: ll_kdtree_y(:)
  real, allocatable :: lons(:,:), lats(:,:)

  real, allocatable :: tree_lons(:), tree_lats(:)

  type(avhrr_data), allocatable :: obs_data(:)

  integer, allocatable :: sob_cnt (:,:)
  real,    allocatable :: sob_val(:,:)
  real,    allocatable :: sob_m2(:,:)

  integer :: ncid, vid
  integer :: x, y
  integer :: r_points(1), r_num
  real :: r_dist(1)
  integer :: ob_cnt, i
  real :: time_offset = 0.0

  type(observation), allocatable :: obs_out(:)
  type(obsio_nc) :: obsio
  real, allocatable :: bkg_temp(:,:,:)
  real :: delta, inc
  real :: min_inc, max_inc, err, err_superob = -1

  integer :: tmp
  integer :: unit

  character(len=:), allocatable :: grid_file
  character(len=:), allocatable :: grid_lon_dim
  character(len=:), allocatable :: grid_lat_dim
  character(len=:), allocatable :: grid_lon_var
  character(len=:), allocatable :: grid_lat_var
  character(len=:), allocatable :: bkg_file
  character(len=:), allocatable :: obs_file
  character(len=:), allocatable :: obsout_file

  namelist /obsop_nml/  grid_file, grid_lon_dim, grid_lat_dim, &
       grid_lon_var, grid_lat_var, bkg_file, obs_file, obsout_file, &
       err, err_superob, time_offset

  
  ! read in namelist
  allocate(character(len=1024) :: grid_file)
  allocate(character(len=1024) :: grid_lon_dim)
  allocate(character(len=1024) :: grid_lat_dim)
  allocate(character(len=1024) :: grid_lon_var)
  allocate(character(len=1024) :: grid_lat_var)
  allocate(character(len=1024) :: bkg_file)
  allocate(character(len=1024) :: obs_file)
  allocate(character(len=1024) :: obsout_file)    
  open(newunit=unit, file="obsop.nml")
  read(unit, obsop_nml)
  close(unit)
  grid_file = trim(grid_file)
  grid_lon_dim = trim(grid_lon_dim)
  grid_lat_dim = trim(grid_lat_dim)
  grid_lon_var = trim(grid_lon_var)
  grid_lat_var = trim(grid_lat_var)  
  bkg_file = trim(bkg_file)
  obs_file = trim(obs_file)
  obsout_file = trim(obsout_file)  
  
  print obsop_nml
  
  ! load model grid
  !------------------------------------------------------------
  print *,""
  print *, "Loading model grid and placing in kd-tree..."

  ! get grid sizes
  print *, '  opening ',grid_file
  call check(nf90_open(grid_file, nf90_nowrite, ncid))
  call check(nf90_inq_dimid(ncid, grid_lon_dim, vid))
  call check(nf90_inquire_dimension(ncid, vid, len=nx))
  call check(nf90_inq_dimid(ncid, grid_lat_dim, vid))
  call check(nf90_inquire_dimension(ncid, vid, len=ny))

  ! get longitudes, place in 1d array
  allocate(lons(nx,ny))
  call check(nf90_inq_varid(ncid, grid_lon_var, vid))
  call check(nf90_get_var(ncid,vid, lons))

  ! get latitudes, place in 1d array
  allocate(lats(nx,ny))
  call check(nf90_inq_varid(ncid, grid_lat_var, vid))
  call check(nf90_get_var(ncid,vid, lats))
  
  ! put grid points into kd-tree
  allocate(ll_kdtree_x(nx*ny))
  allocate(ll_kdtree_y(nx*ny))
  allocate(tree_lats(nx*ny))
  allocate(tree_lons(nx*ny))
  do x = 1, nx
     do y = 1, ny
        tree_lats((y-1)*nx + x) = lats(x,y)
        tree_lons((y-1)*nx + x) = lons(x,y)
        ll_kdtree_x((y-1)*nx + x) = x
        ll_kdtree_y((y-1)*nx + x) = y
     end do
  end do
  call kd_init(ll_kdtree, tree_lons, tree_lats)
  deallocate(tree_lons)
  deallocate(tree_lats)

  ! load the background
  !------------------------------------------------------------
  print *, ""
  print *, "Loading background file..."
  print *, "  ",bkg_file
  allocate(bkg_temp(nx,ny,75))
  call check(nf90_open(bkg_file, nf90_nowrite, ncid))
  call check(nf90_inq_varid(ncid, "temp", vid))
  call check(nf90_get_var(ncid, vid, bkg_temp))
  call check(nf90_close(ncid))


  ! Load observations
  !------------------------------------------------------------
  print *, ""
  print *, "Loading observation file..."
  print *, "  ", obs_file
  call read_avhrr_pathfinder_nc(obs_file, 4, obs_data)
  print *, size(obs_data), "observations loaded"
  print *, ""


  ! for each obs, place into bins
  !------------------------------------------------------------
  allocate(sob_cnt(nx,ny))
  allocate(sob_val(nx,ny))
  allocate(sob_m2(nx,ny))
  sob_cnt = 0
  sob_val = 0
  sob_m2 = 0.0

  do i=1,size(obs_data)
     call kd_search_nnearest(ll_kdtree, obs_data(i)%lon, obs_data(i)%lat, 1, r_points, r_dist, r_num, .false.)
     x = ll_kdtree_x(r_points(1))
     y = ll_kdtree_y(r_points(1))
     ! running mean/variance update algorithm
     sob_cnt(x,y) = sob_cnt(x,y)+1
     inc =  obs_data(i)%val-273.15 - bkg_temp(x,y,1)
     delta = inc - sob_val(x,y)     
     sob_val(x,y) = sob_val(x,y)+delta/sob_cnt(x,y)
     sob_m2(x,y) = sob_m2(x,y) + delta*(inc - sob_val(x,y))
  end do
  print *, count(sob_cnt > 0)
  



  ! output obs
  !------------------------------------------------------------
  min_inc= 1e10
  max_inc = -1e10


  allocate(obs_out(count(sob_cnt>0)))
  ob_cnt = 0
  do x=1,nx
     do y=1,ny
        if(sob_cnt(x,y) == 0) cycle
        ob_cnt = ob_cnt + 1 
        obs_out(ob_cnt)%id   = 2210
        obs_out(ob_cnt)%lat  = lats(x,y)
        obs_out(ob_cnt)%lon  = lons(x,y)
        obs_out(ob_cnt)%dpth = 0.0
        obs_out(ob_cnt)%hr   = time_offset
        obs_out(ob_cnt)%inc  = sob_val(x,y)
        obs_out(ob_cnt)%err  = err
        if(sob_cnt(x,y) > 1 .and. err_superob > 0) &
             obs_out(ob_cnt)%err = obs_out(ob_cnt)%err +&
              sqrt(sob_m2(x,y)/(sob_cnt(x,y)-1))*err_superob
        if(obs_out(ob_cnt)%inc > max_inc) max_inc = obs_out(ob_cnt)%inc
        if(obs_out(ob_cnt)%inc < min_inc) min_inc = obs_out(ob_cnt)%inc

     end do
  end do
  print *, "writing output file..."
  print *, "  ", obsout_file
  call obsio%write(obsout_file,obs_out)

  print *, "min/max increment:",min_inc, max_inc
contains 



subroutine check(status)
  integer, intent(in) :: status
  if(status/= nf90_noerr) then
     print *, trim(nf90_strerror(status))
     stop 1
  end if
end subroutine check

end program obsop_sst
