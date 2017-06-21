program bgvar
  use kdtree
  use datatable
  use netcdf
  implicit none


  character(len=:), allocatable :: bgvar_file_in
  character(len=:), allocatable :: bgvar_file_out

  ! 3dvar grid
  integer :: grid_nx
  integer :: grid_ny
  integer :: grid_nz  
  real, allocatable :: grid_lons(:,:)
  real, allocatable :: grid_lats(:,:)
  real, allocatable :: grid_depths(:)
  real, allocatable :: grid_mask(:,:)

  ! climatology grid
  integer :: cgrid_nx
  integer :: cgrid_ny
  integer :: cgrid_nz
  real, allocatable :: cgrid_lons(:, :)
  real, allocatable :: cgrid_lats(:, :)
  real, allocatable :: cgrid_depths(:)
  real, allocatable :: cgrid_mask(:,:)

  real, allocatable :: cgrid_val(:,:,:,:)
  type(kd_root) :: cgrid_kdtree
  integer, allocatable :: cgrid_kdtree_x(:)
  integer, allocatable :: cgrid_kdtree_y(:)

  ! other temporary variables
  real, allocatable :: tree_lons(:), tree_lats(:)
  real, allocatable :: tmp3d(:,:,:)
  integer :: x,y,i,j,k,z
  integer :: unit
  integer :: ncid, vid, dim_x, dim_y, dim_z
  integer :: ncid_c, vid_c
  real :: r_dist(1)
  integer :: r_points(1), r_num, x1, y1, z1,z2
  real :: r, r2
  
  namelist /bgvar_nml/ bgvar_file_in, bgvar_file_out, &
       grid_nx, grid_ny, grid_nz, cgrid_nx, cgrid_ny, cgrid_nz
  
  print *, "------------------------------------------------------------"
  print *, "GODAS-3DVar background error variance update"
  print *, "------------------------------------------------------------"

  ! read in namelist and initialize datatable
  allocate(character(len=1024) :: bgvar_file_in)
  allocate(character(len=1024) :: bgvar_file_out)
  open(newunit=unit, file="namelist.bgvar")
  read(unit, nml=bgvar_nml)
  bgvar_file_in = trim(bgvar_file_in)
  bgvar_file_out = trim(bgvar_file_out)
  print bgvar_nml
  close(unit)  
  call datatable_init(.true., 'datatable.bgvar')
  
  ! Read in the input/output grid
  print *, ""
  print *, "Reading 3dvar grid..."
  allocate(grid_lons(grid_nx,grid_ny))
  allocate(grid_lats(grid_nx,grid_ny))
  allocate(grid_mask(grid_nx,grid_ny))
  allocate(grid_depths(grid_nz))
  call datatable_get("grid_x", grid_lons)
  call datatable_get("grid_y", grid_lats)
  call datatable_get("grid_z", grid_depths)
  call datatable_get("grid_mask", grid_mask)

  ! read in the climatology grid
  print *, ""
  print *, "reading climatology..."
  allocate(cgrid_lons(cgrid_nx, cgrid_ny))
  allocate(cgrid_lats(cgrid_nx, cgrid_ny))
  allocate(cgrid_mask(cgrid_nx, cgrid_ny))
  allocate(cgrid_depths(cgrid_nz))
  call datatable_get("cgrid_x", cgrid_lons)
  call datatable_get("cgrid_y", cgrid_lats)
  call datatable_get("cgrid_mask", cgrid_mask)
  call datatable_get("cgrid_z", cgrid_depths)

  ! pu the climatology grid into kd-tree
  allocate(cgrid_kdtree_x(cgrid_nx*cgrid_ny))
  allocate(cgrid_kdtree_y(cgrid_nx*cgrid_ny))
  allocate(tree_lats(cgrid_nx*cgrid_ny))
  allocate(tree_lons(cgrid_nx*cgrid_ny))
  j = 0
  do x=1, cgrid_nx
     do y = 1, cgrid_ny
        if(cgrid_mask(x,y) < 1.0) cycle
        j = j + 1
        tree_lats(j) = cgrid_lats(x,y)
        tree_lons(j) = cgrid_lons(x,y)
        cgrid_kdtree_x(j) = x
        cgrid_kdtree_y(j) = y
     end do
  end do
  call kd_init(cgrid_kdtree, tree_lons(:j), tree_lats(:j))  
  deallocate(tree_lats)
  deallocate(tree_lons)

  allocate(tmp3d(grid_nx, grid_ny, grid_nz))
  allocate(cgrid_val(cgrid_nx, cgrid_ny, cgrid_nz, 1))
  

  ! create output data
  print *, "creating output..."
  call check(nf90_create(bgvar_file_out, nf90_clobber, ncid))
  call check(nf90_def_dim(ncid, "lon", grid_nx, dim_x))
  call check(nf90_def_var(ncid, "lon", nf90_real, dim_x, vid))
  call check(nf90_put_att(ncid, vid, "units", "degrees_E"))

  call check(nf90_def_dim(ncid, "lat", grid_ny, dim_y))
  call check(nf90_def_var(ncid, "lat", nf90_real, dim_y, vid))
  call check(nf90_put_att(ncid, vid, "units", "degrees_N"))

  call check(nf90_def_dim(ncid, "depth", grid_nz, dim_z))
  call check(nf90_def_var(ncid, "depth", nf90_real, dim_z, vid))
  call check(nf90_put_att(ncid, vid, "units", "meters"))

  call check(nf90_def_var(ncid, "temp", nf90_real, (/dim_x,dim_y,dim_z/), vid))
  call check(nf90_def_var(ncid, "salt", nf90_real, (/dim_x,dim_y,dim_z/), vid))  
  call check(nf90_enddef(ncid))

  call check(nf90_inq_varid(ncid, "depth", vid))
  call check(nf90_put_var(ncid, vid, grid_depths))
  
  call check(nf90_open("clim_var.nc", nf90_nowrite, ncid_c))
  
  
  !============================================================
  !============================================================
  print *, "Temperature..."
  call check(nf90_inq_varid(ncid_c, "temp", vid_c))
  call check(nf90_get_var(ncid_c, vid_c, cgrid_val))
  call interpClim(cgrid_val, tmp3d, 0.01, 4.0) 
  call check(nf90_inq_varid(ncid, "temp", vid))
  call check(nf90_put_var(ncid, vid, tmp3d))
  
  
  print *, "Salinity..."
  call check(nf90_inq_varid(ncid_c, "salt", vid_c))
  call check(nf90_get_var(ncid_c, vid_c, cgrid_val))  
  call interpClim(cgrid_val, tmp3d, 0.001, 2.0)
  call check(nf90_inq_varid(ncid, "salt", vid))
  call check(nf90_put_var(ncid, vid, tmp3d))

  call check(nf90_close(ncid_c))
  call check(nf90_close(ncid))


  
contains

  
  
  subroutine check(status)
    integer, intent(in) :: status
    if(status /= nf90_noerr) then
       print *, trim(nf90_strerror(status))
       stop 1
    end if
  end subroutine check
  
  
  
  subroutine interpClim(clim, val, minv, maxv)
    real, intent(in) :: clim(:,:,:,:)
    real, intent(inout) :: val(:,:,:)
    real, intent(in) :: minv, maxv

    integer :: x, y, x1, y1, z, z1, z2, i
    real :: r,r2
   
    val = minv
    do x=1,grid_nx
      do y=1,grid_ny
        if(grid_mask(x,y) < 1.0) cycle
        call kd_search_nnearest(cgrid_kdtree, grid_lons(x,y), grid_lats(x,y), 1, r_points, r_dist, r_num)
        x1 = cgrid_kdtree_x(r_points(1))
        y1 = cgrid_kdtree_y(r_points(1))
        do z=1,grid_nz
           r = 1e10
           do i = 1, cgrid_nz
              r2 = abs(cgrid_depths(i)-grid_depths(z))
              if (r2 < r) then
                 r = r2
              else
                 exit
              end if              
           end do
           i = i -1
           if (cgrid_depths(i) > grid_depths(z)) then
              z1 = max(1,i-1)
              z2 = i
              if (z2 > 1e5) z2 = z1
           else
              z1 = i
              z2 = min(i+1, cgrid_nz)
              if (z2 > 1e5) z2 = z1              
           end if
           r2 =  cgrid_depths(z2) - cgrid_depths(z1)
           r = 0.0
           if (r2 > 0.0) then
              r = (grid_depths(z) - cgrid_depths(z1)) / r2
           end if
           if(cgrid_val(x1,y1,z2,1) > 1e5) cycle
           r = (1.0-r)*cgrid_val(x1,y1,z1,1) + r*cgrid_val(x1,y1,z2,1)
           val(x,y,z) = min(max(r,minv),maxv)           
        end do
     end do
    end do
  end subroutine interpClim

  
end program bgvar
