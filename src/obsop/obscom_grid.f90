module obscom_grid
  use netcdf
  use kdtree
  implicit none
  private

  public :: grid_init
  public :: grid_ll2xy
  public :: grid_d2z
  
  integer, public :: grid_nx
  integer, public :: grid_ny
  integer, public :: grid_nz
  
  real, allocatable, public :: grid_lons(:,:)
  real, allocatable, public :: grid_lats(:,:)
  real, allocatable, public :: grid_mask(:,:)
  real, allocatable, public :: grid_depths(:)
  
  type(kd_root) :: ll_kdtree
  integer, allocatable :: ll_kdtree_x(:)
  integer, allocatable :: ll_kdtree_y(:)

  
contains


  
  pure subroutine grid_d2z(depth, z)
    ! find the closest vertical level given a
    ! depth, performs a binary search
    real, intent(in) :: depth
    integer, intent(out) :: z

    integer :: i1, i2, i3

    i1=1
    i2=(1+grid_nz)/2
    i3=grid_nz

    do while(i2 /= i1) 
       if(depth <= grid_depths(i2)) then
          i3 = i2
       else
          i1 = i2
       end if
       i2 = (i1+i3)/2
    end do
    if(abs(grid_depths(i1)-depth) <= abs(grid_depths(i3)-depth)) then
       z = i1
    else
       z = i3
    end if

  end subroutine grid_d2z


  
  pure subroutine grid_ll2xy(lat, lon, x, y)
    ! get the closest x/y grid point for a given lat/lon
    
    real,    intent(in)  :: lat, lon
    integer, intent(out) :: x, y

    integer :: r_points(1), r_num
    real    :: r_dist(1)
    
    call kd_search_nnearest(ll_kdtree, lon, lat, 1, r_points, r_dist, r_num, .false.)

    x = ll_kdtree_x(r_points(1))
    y = ll_kdtree_y(r_points(1))
  end subroutine grid_ll2xy

  
  subroutine grid_init(nml_file)
    character(len=*), intent(in) :: nml_file

    integer :: unit, ncid, vid
    character(len=:), allocatable :: grid_file
    character(len=:), allocatable :: grid_lon_dim
    character(len=:), allocatable :: grid_lat_dim
    character(len=:), allocatable :: grid_lon_var
    character(len=:), allocatable :: grid_lat_var
    character(len=:), allocatable :: grid_msk_var
    character(len=:), allocatable :: grid_dpth_file
    character(len=:), allocatable :: grid_dpth_dim
    character(len=:), allocatable :: grid_dpth_var
    
    integer :: x, y, i

    real, allocatable :: tree_lons(:), tree_lats(:)
    
    namelist /grid_nml/ grid_file, grid_lon_dim, grid_lat_dim, &
         grid_lon_var, grid_lat_var, grid_msk_var, &
         grid_dpth_file, grid_dpth_dim, grid_dpth_var
    

    ! read namelist file
!    print *, trim(nml_file)
    allocate(character(len=1024) :: grid_file)
    allocate(character(len=1024) :: grid_lon_dim)
    allocate(character(len=1024) :: grid_lat_dim)
    allocate(character(len=1024) :: grid_lon_var)
    allocate(character(len=1024) :: grid_lat_var)
    allocate(character(len=1024) :: grid_msk_var)
    allocate(character(len=1024) :: grid_dpth_file)
    allocate(character(len=1024) :: grid_dpth_dim)
    allocate(character(len=1024) :: grid_dpth_var)
    open(newunit=unit, file=nml_file)
    read(unit, grid_nml)
    close(unit)
    grid_file    = trim(grid_file)
    grid_lon_dim = trim(grid_lon_dim)
    grid_lat_dim = trim(grid_lat_dim)
    grid_lon_var = trim(grid_lon_var)
    grid_lat_var = trim(grid_lat_var)
    grid_msk_var = trim(grid_msk_var)
    grid_dpth_file = trim(grid_dpth_file)
    grid_dpth_dim  = trim(grid_dpth_dim)
    grid_dpth_var  = trim(grid_dpth_var)
    print *, ""
    print grid_nml
    print *, ""

    
    ! load the model grid
    !------------------------------------------------------------
    print *, "Loading model grid ..."

    ! get the grid sizes
    call check(nf90_open(grid_file, nf90_nowrite, ncid))
    call check(nf90_inq_dimid(ncid, grid_lon_dim, vid))
    call check(nf90_inquire_dimension(ncid, vid, len=grid_nx))
    call check(nf90_inq_dimid(ncid, grid_lat_dim, vid))
    call check(nf90_inquire_dimension(ncid, vid, len=grid_ny))
    print *, "  grid size: ",grid_nx,"X",grid_ny

    ! get longitudes, place in array
    allocate(grid_lons(grid_nx,grid_ny))
    call check(nf90_inq_varid(ncid, grid_lon_var, vid))
    call check(nf90_get_var(ncid, vid, grid_lons))

    ! get latitudes, place in array
    allocate(grid_lats(grid_nx,grid_ny))
    call check(nf90_inq_varid(ncid, grid_lat_var, vid))
    call check(nf90_get_var(ncid, vid, grid_lats))

    print *, "  lat range:",minval(grid_lats),maxval(grid_lats)
    print *, "  lon range:",minval(grid_lons),maxval(grid_lons)

    allocate(grid_mask(grid_nx,grid_ny))
    call check(nf90_inq_varid(ncid, grid_msk_var, vid))
    call check(nf90_get_var(ncid, vid, grid_mask))

    ! put the grid into kd-tree
    allocate(ll_kdtree_x(grid_nx*grid_ny))
    allocate(ll_kdtree_y(grid_nx*grid_ny))
    allocate(tree_lats(grid_nx*grid_ny))
    allocate(tree_lons(grid_nx*grid_ny))
    do x = 1, grid_nx
       do y = 1, grid_ny
          i = (y-1)*grid_nx + x
          tree_lats(i) = grid_lats(x,y)
          tree_lons(i) = grid_lons(x,y)
          ll_kdtree_x(i) = x
          ll_kdtree_y(i) = y
       end do
    end do
    call kd_init(ll_kdtree, tree_lons, tree_lats)
    deallocate(tree_lons)
    deallocate(tree_lats)

    ! get the vertical grid
    call check(nf90_open(grid_dpth_file, nf90_nowrite,  ncid))
    call check(nf90_inq_dimid(ncid, grid_dpth_dim, vid))
    call check(nf90_inquire_dimension(ncid, vid, len=grid_nz))

    allocate(grid_depths(grid_nz))
    call check(nf90_inq_varid(ncid, grid_dpth_var, vid))
    call check(nf90_get_var(ncid, vid, grid_depths))
    call check(nf90_close(ncid))

    print *, "Vertical grid:"
    print *, "  size: ",grid_nz
    print *, "  range:",minval(grid_depths),maxval(grid_depths)    
    
  end subroutine grid_init

  
  
  subroutine check(status)
    integer, intent(in) :: status
    if(status /= nf90_noerr) then
       print *, trim(nf90_strerror(status))
       stop 1
    end if
  end subroutine check

  
end module obscom_grid
