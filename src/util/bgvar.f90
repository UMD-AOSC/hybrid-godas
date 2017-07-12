program bgvar
  use kdtree
  use datatable
  use netcdf
  implicit none

  character(len=:), allocatable :: bgvar_file_out
  real :: day_update
  real :: day_relax_obs  = 10
  real :: day_relax_clim = 30
  logical :: init = .false.

  ! main grid
  integer :: grid_nx
  integer :: grid_ny
  integer :: grid_nz
  real, allocatable :: grid_lons(:,:)
  real, allocatable :: grid_lats(:,:)
  real, allocatable :: grid_mask(:,:)
  real, allocatable :: grid_depths(:)

  real, allocatable :: val(:,:,:)
  real, allocatable :: ana_inc(:,:,:)
  real, allocatable :: ana_inc_mc(:,:,:)
  real, allocatable :: tmp_grid(:,:,:)

  ! climatology grid
  integer :: cgrid_nx
  integer :: cgrid_ny
  integer :: cgrid_nz
  real, allocatable :: cgrid_lons(:,:)
  real, allocatable :: cgrid_lats(:,:)
  real, allocatable :: cgrid_mask(:,:)
  real, allocatable :: cgrid_depths(:)
  type(kd_root) :: cgrid_kdtree
  integer, allocatable :: cgrid_kdtree_x(:)
  integer, allocatable :: cgrid_kdtree_y(:)
  real, allocatable :: tmp_cgrid(:,:,:, :)

  ! other temp variables
  integer :: unit
  real, allocatable :: tree_lons(:), tree_lats(:)
  integer :: x,y,i,j,k,z
  integer :: ncid, ncid_c, vid, dim_x, dim_y, dim_z
  real :: r_ob, r_clim

  namelist /bgvar_nml/ init, bgvar_file_out, &
       grid_nx, grid_ny, grid_nz, &
       cgrid_nx, cgrid_ny, cgrid_nz, &
       day_update, day_relax_obs, day_relax_clim



  !============================================================
  !============================================================

  print *, "------------------------------------------------------------"
  print *, " GODAS-3DVar background error variance update"
  print *, "------------------------------------------------------------"  

  ! read in the namelist and initialize datatable
  allocate(character(len=1024) :: bgvar_file_out)
  open(newunit=unit, file="namelist.bgvar")
  read(unit, nml=bgvar_nml)
  bgvar_file_out = trim(bgvar_file_out)
  close(unit)
  print bgvar_nml
  call datatable_init(.true., 'datatable.bgvar')


  ! read in the main grid
  print *, ""
  print *, "Reading 3dvar grid..."
  allocate(grid_lons(grid_nx, grid_ny))
  allocate(grid_lats(grid_nx, grid_ny))
  allocate(grid_mask(grid_nx, grid_ny))
  allocate(grid_depths(grid_nz))
  call datatable_get("grid_x", grid_lons)
  call datatable_get("grid_y", grid_lats)
  call datatable_get("grid_z", grid_depths)
  call datatable_get("grid_mask", grid_mask)

  allocate(val(grid_nx, grid_ny, grid_nz))
  allocate(ana_inc(grid_nx, grid_ny, grid_nz))
  allocate(ana_inc_mc(grid_nx, grid_ny, grid_nz))
  allocate(tmp_grid(grid_nx, grid_ny, grid_nz))


  ! read in the climatology grid
  print *, ""
  print *, "Reading climatology grid..."
  allocate(cgrid_lons(cgrid_nx, cgrid_ny))
  allocate(cgrid_lats(cgrid_nx, cgrid_ny))
  allocate(cgrid_mask(cgrid_nx, cgrid_ny))
  allocate(cgrid_depths(cgrid_nz))
  call datatable_get("cgrid_x", cgrid_lons)
  call datatable_get("cgrid_y", cgrid_lats)
  call datatable_get("cgrid_z", cgrid_depths)
  call datatable_get("cgrid_mask", cgrid_mask)
  
  allocate(tmp_cgrid(cgrid_nx, cgrid_ny, cgrid_nz, 1))

  ! put climatology into kdtree
  ! TODO, replace all this with ESMF based interpolation
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

  ! open clim file
  call check(nf90_open("INPUT/clim_var.nc", nf90_nowrite, ncid_c))

  ! create the output file
  print *, ""
  print *, "initializing output file..."
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

  ! calculate weighting
  print *,""
  r_ob = 1.0 - exp(-(day_update)**2 / (2*day_relax_obs**2))
  r_clim = 1.0 - exp(-(day_update)**2 / (2*day_relax_clim**2))
  if(init) r_clim = 1.0
  print *, "r_ob:   ", r_ob
  print *, "r_clim: ", r_clim
  

  ! ============================================================
  ! Temperature
  !============================================================

  print *, ""
  if (init) then
     val = 0.0
  else
     ! read in previous value
     call datatable_get("bgvar_prev_t", val) 
  end if

  ! relax to climatology
  call check(nf90_inq_varid(ncid_c, "temp", vid))
  call check(nf90_get_var(ncid_c, vid, tmp_cgrid))
  call interpClim(tmp_cgrid, tmp_grid, 0.01, 4.0)
  val = r_clim*tmp_grid + (1.0-r_clim)*val

  ! relax to analysis increment
  call datatable_get("ana_inc_t", ana_inc)
  call datatable_get("ana_mc_t",  ana_inc_mc)
  val =  r_ob*(val*(1-ana_inc_mc)+abs(ana_inc)*ana_inc_mc) + (1.0-r_ob)*val

  ! write it out
  call check(nf90_inq_varid(ncid, "temp", vid))
  call check(nf90_put_var(ncid, vid, val))


  ! ============================================================
  ! Temperature
  !============================================================

  print *, ""
  if (init) then
     val = 0.0
  else
     ! read in previous value
     call datatable_get("bgvar_prev_s", val) 
  end if

  ! relax to climatology
  call check(nf90_inq_varid(ncid_c, "salt", vid))
  call check(nf90_get_var(ncid_c, vid, tmp_cgrid))
  call interpClim(tmp_cgrid, tmp_grid, 0.001, 2.0)
  val = r_clim*tmp_grid + (1.0-r_clim)*val

  ! relax to analysis increment
  call datatable_get("ana_inc_s", ana_inc)
  call datatable_get("ana_mc_s",  ana_inc_mc)
  val =  r_ob*(val*(1-ana_inc_mc)+abs(ana_inc)*ana_inc_mc) + (1.0-r_ob)*val

  ! write it out
  call check(nf90_inq_varid(ncid, "salt", vid))
  call check(nf90_put_var(ncid, vid, val))


  !============================================================
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

    integer :: r_points(1), r_num
    real :: r_dist(1)
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
           if(clim(x1,y1,z2,1) > 1e5) cycle
           r = (1.0-r)*clim(x1,y1,z1,1) + r*clim(x1,y1,z2,1)
           val(x,y,z) = min(max(r,minv),maxv)           
        end do
     end do
    end do
  end subroutine interpClim


end program bgvar


