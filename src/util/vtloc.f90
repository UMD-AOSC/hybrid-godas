program vtloc
   use hzsmooth
   use netcdf
   use datatable
   implicit none

   ! variables read in from namelist
   integer :: grid_nx
   integer :: grid_ny
   integer :: grid_nz
   real    :: rho_delta = 0.125
   real    :: vtloc_min = 5.0
   real    :: vtloc_max = 250.0
   real    :: hz_loc(2) = (/250e3, 50e3/)
   real    :: hz_loc_scale = 2.5
   integer :: gauss_iter = 2
   logical :: mld_only = .true.

   ! data that is read in
   real,    allocatable :: grid_lat(:,:)
   real,    allocatable :: grid_lon(:,:)
   real,    allocatable :: grid_mask(:,:)
   real,    allocatable :: grid_dpth(:)
   real,    allocatable :: grid_D(:,:)
   real,    allocatable :: dens(:,:,:) 

   ! calculated data
   integer, allocatable :: grid_btm(:,:)
   real,    allocatable :: grid_ld(:,:)
   real,    allocatable :: vtloc_dist(:,:,:)

   ! constants
   real, parameter :: pi = 4*atan(1.0)
   real, parameter :: re = 6371d3
   real, parameter :: omega = 7.29e-5

   ! other misc variables
   integer :: x, y, z, btm
   integer :: unit, ierr
   integer :: nc, nc_x, nc_y, nc_z, nc_v

   ! OpenMP functions
   INTEGER ::  OMP_GET_NUM_THREADS,  OMP_GET_THREAD_NUM


   namelist /g3dv_grid/ grid_nx, grid_ny, grid_nz
   namelist /g3dv_hzloc/ hz_loc, hz_loc_scale
   namelist /vtloc_nml/ rho_delta, vtloc_min, vtloc_max, gauss_iter, mld_only


   ! read in namelist and datatable files
   print *, "------------------------------------------------------------"
   print *, " GODAS-3DVar vertical localization distance calculator"
   print *, "------------------------------------------------------------"

!$OMP PARALLEL
   if(OMP_GET_THREAD_NUM() == 0) then
      print *, "OpenMP threads: ",OMP_GET_NUM_THREADS()
   end if
!$OMP END PARALLEL
   
   open(newunit=unit, file="namelist.3dvar", status='old')

   read(unit, nml=g3dv_grid)
   print g3dv_grid

   rewind(unit)
   read(unit, nml=g3dv_hzloc)
   print g3dv_hzloc

   rewind(unit)
   read(unit, nml=vtloc_nml)
   print vtloc_nml

   close(unit)

   call datatable_init(.true., "datatable.3dvar")

    
   ! read in the data files
   print *, ""
   print *, "------------------------------------------------------------"
   print *, "reading in data fields..."
   print *, "------------------------------------------------------------"     

   allocate(dens(grid_nx, grid_ny, grid_nz))
   call datatable_get('bg_dens', dens)
   
   allocate(grid_mask(grid_nx,grid_ny))
   call datatable_get('grid_mask', grid_mask)
   
   allocate(grid_D(grid_nx, grid_ny))
   call datatable_get('grid_D', grid_D)
   
   allocate(grid_dpth(grid_nz))
   call datatable_get('grid_z', grid_dpth)

   allocate(grid_lon(grid_nx,grid_ny))
   call datatable_get('grid_x', grid_lon)

   allocate(grid_lat(grid_nx,grid_ny))
   call datatable_get('grid_y', grid_lat)

   allocate(grid_ld(grid_nx,grid_ny))
   do y=1,grid_ny
      do x=1,grid_nx
         grid_ld(x,y) = sqrt(bgcov_hzdist(grid_lat(x,y))**2/gauss_iter)
      end do
   end do

   
   
   ! determine bottom levels
   print *, ""
   print *, "------------------------------------------------------------"
   print *, "finding bottom levels..."
   allocate(grid_btm(grid_nx,grid_ny))
   grid_btm = 0
   do y=1,grid_ny
      do x =1,grid_nx
         if(grid_mask(x,y) <= 0.0) cycle
         do z =1, grid_nz
            if(grid_dpth(z) > grid_D(x,y)) then
               grid_btm(x,y) = z-1
               exit
            end if
            grid_btm(x,y) = grid_nz
         end do
      end do
   end do

  
   ! calculate initial vt loc distances
   allocate(vtloc_dist(grid_nx, grid_ny, grid_nz))
   vtloc_dist =0
   print*, "Calculating vertical distances..."
!$OMP PARALLEL DO PRIVATE(btm,x) SCHEDULE(static,1)
   do y=1,grid_ny
      do x=1,grid_nx
         btm = grid_btm(x,y)
         if(btm == 0) cycle
         if (mld_only) then
            vtloc_dist(x,y,:) = col_vtloc_mld( &
                 dens(x,y,:), grid_dpth, btm, rho_delta)
         else
            vtloc_dist(x,y,1:btm) = col_vtloc( &
                 dens(x,y,1:btm),grid_dpth(1:btm), &
                 rho_delta, vtloc_min, vtloc_max, 1.0)
         end if
      end do
   end do
!$OMP END PARALLEL DO


   !------------------------------------------------------------  
   !perform gaussian smoothing
   !------------------------------------------------------------
   print *, "Gaussian smoother..."
   call hzsmooth_gauss(vtloc_dist, grid_lat, grid_lon, grid_btm, grid_ld, gauss_iter)


   !------------------------------------------------------------
   ! write the output
   !------------------------------------------------------------
   print *, ""
   print *, "------------------------------------------------------------"
   print *, "------------------------------------------------------------"
   print *, " Saving output..."
   call check(nf90_create("vtloc.nc", nf90_write, nc))
   call check(nf90_def_dim(nc, "x", grid_nx, nc_x))
   call check(nf90_def_var(nc, "x", nf90_real, (/nc_x/), nc_v))
   call check(nf90_put_att(nc, nc_v, "units", "degrees_east"))
   
   call check(nf90_def_dim(nc, "y", grid_ny, nc_y))
   call check(nf90_def_var(nc, "y", nf90_real, (/nc_y/), nc_v))
   call check(nf90_put_att(nc, nc_v, "units", "degrees_north"))
     
   call check(nf90_def_dim(nc, "z", grid_nz, nc_z))
   call check(nf90_def_var(nc, "z", nf90_real, (/nc_z/), nc_v))
   call check(nf90_put_att(nc, nc_v, "units", "meters"))
   
   call check(nf90_def_var(nc, "vtloc", nf90_real, (/nc_x, nc_y, nc_z/), nc_v))          
   call check(nf90_enddef(nc))
   
   ! ---
   
   call check(nf90_inq_varid(nc, "vtloc", nc_v))
   call check(nf90_put_var(nc, nc_v, vtloc_dist))
   
   call check(nf90_inq_varid(nc, "x", nc_v))
   call check(nf90_put_var(nc, nc_v, grid_lon(:,1)))
   
   call check(nf90_inq_varid(nc, "y", nc_v))
   call check(nf90_put_var(nc, nc_v, maxval(grid_lat, 1, grid_lat < 100)))
   
   call check(nf90_inq_varid(nc, "z", nc_v))
   call check(nf90_put_var(nc, nc_v, grid_dpth))
   
   call check(nf90_close(nc))

  

  
 contains



   pure function col_vtloc_mld(dens, dpth,  bottom, rho_delta) result(vtloc)
    real,    intent(in) :: dens(:)
    real,    intent(in) :: dpth(:)
    integer, intent(in) :: bottom
    real,    intent(in) :: rho_delta

    real :: vtloc(size(dens))

    real    :: mld_max 
    real    :: mld
    integer :: mld_z
    integer :: z2, z
    real    :: r

    vtloc=0.0
    mld_max = vtloc_max

    ! calculate vt loc distances from layer thickensses
    vtloc(1) = (dpth(2)-dpth(1))
    do z=2,size(dpth)-1
       vtloc(z) = (dpth(z+1)-dpth(z-1))/2.0
    end do
    vtloc(size(dpth)) = vtloc(size(dpth)-1)
    vtloc(bottom+1:size(dpth)) = 0.0


    ! calculate the mixed layer depth
    mld = -1
    mld_z = bottom
    do z2 = 2, bottom
       if(dens(z2)-dens(1) >= rho_delta) then                    
          r = (dens(1)+rho_delta-dens(z2-1)) / (dens(z2)-dens(z2-1))
          mld = dpth(z2-1) + ( dpth(z2)-dpth(z2-1))*r
          mld_z = z2-1
          exit
       end if       
       if(mld_max > 0 .and. dpth(z2) > mld_max) then
          mld_z = z2 -1
          mld = mld_max
          exit
       end if
    end do
    if(mld < 0) mld = dpth(bottom)
    if(mld_max > 0)  mld = min(mld_max, mld)

    ! linear interpolation of MLD from top to bottom of MLD
    do z = 1, mld_z
       r = 1.0 - (dpth(z) / mld)
       vtloc(z) = mld*r + vtloc(mld_z)*(1.0-r)
    end do    
   end function col_vtloc_mld


  
   pure function col_vtloc(dens, dpth, rho_delta, vtloc_min, vtloc_max, vtloc_pow) result(vtloc)
    real, intent(in) :: dens(:)
    real, intent(in) :: dpth(:)
    real, intent(in) :: rho_delta
    real, intent(in) :: vtloc_max, vtloc_min, vtloc_pow
    real :: vtloc(size(dens))

    real :: r
    real :: loc_u(size(dens))
    real :: loc_d(size(dens))
    
    integer :: z, z1, z2

    vtloc = 0.0

    !TODO, currently uses linear interpolation, should switch to cubic spline

    
    ! initial pass through the column
    ! to determine localization distances in the up/down directions
    !------------------------------
    loc_u = -1
    loc_d = -1
    do z = 1, size(dens)
       ! calculate UPWARD localization distance
       do z2 = z-1, 1 ,-1          
          if(dens(z)-dens(z2) >= rho_delta) then
             loc_u(z) = dpth(z) - dpth(z2+1) - &
                  (dens(z) - rho_delta-dens(z2+1))&
                  * (dpth(z2) - dpth(z2+1)) &
                  / (dens(z2) - dens(z2+1))
             r = (dpth(z)-dpth(z-1))
             if(loc_u(z) <  r) loc_u(z) = r
             exit
          end if
       end do
       
       ! calculate DOWNWARD localization distance
       do z2 = z+1, size(dens)
          if(dens(z2)-dens(z) >= rho_delta) then
             loc_d(z) = -dpth(z) + dpth(z2-1) + &
                  (dens(z) + rho_delta-dens(z2-1))&
                  * (dpth(z2) - dpth(z2-1)) &
                  / (dens(z2) - dens(z2-1))
             r = (dpth(z+1)-dpth(z))
             if(loc_d(z) <  r) loc_d(z) = r
             exit
          end if
       end do       
    end do

    
    ! set the lengths at the top/bottom boundary
    loc_u(1) = loc_d(1)
    loc_d(size(dens)) = loc_u(size(dens))

    
    ! for shallow/stable layers that have no localization lengths set at all:
    if(loc_d(1) < 0 .or. loc_u(size(dens)) < 0) then
       loc_d(1) = dpth(size(dens))*sqrt(0.3)/2.0
       loc_u(1) = loc_d(1)
       loc_u(size(dens)) = dpth(size(dens))*sqrt(0.3)/2.0
       loc_d(size(dens)) = loc_u(size(dens))
!       loc_d = dpth(size(dens))*sqrt(0.3)/2.0
!       loc_u = dpth(size(dens))*sqrt(0.3)/2.0
    end if

    
    ! clip to some min/max value
    if(vtloc_max > 0) then
       loc_u = min(loc_u, vtloc_max)
       loc_d = min(loc_d, vtloc_max)
    end if
    if(vtloc_min > 0) then
       where(loc_u > 0) loc_u = max(loc_u, vtloc_min)
       where(loc_d > 0) loc_d = max(loc_d, vtloc_min)
    end if
    

    
    ! fill in gaps
    !------------------------------

    ! upward lengths
    z1 = -1
    do z2 = 2, size(dens)
       if(loc_u(z2) > 0) then
          z1 = z2
          exit
       end if
    end do
    if(z1 > 2) then
       do z=2,z1-1
          loc_u(z) = loc_u(1) + &
               (dpth(z) - dpth(1)) &
               * (loc_u(z1) - loc_u(1))&
               / (dpth(z1) - dpth(1))
       end do
    end if

    
    ! downward lengths
    z1 = size(dens)+1
    do z2 = size(dens)-1, 1, -1
       if(loc_d(z2) > 0) then
          z1 = z2
          exit
       end if
    end do
    if(z1 < size(dens) - 1) then
       do z = z1+1, size(dens)-1
          loc_d(z) = loc_d(size(dens)) + &
               (dpth(z) - dpth(size(dens))) &
               * (loc_d(z1) - loc_d(size(dens))) &
               / (dpth(z1) - dpth(size(dens)))
       end do
    end if

    ! any small gaps remaining are due to density decreasing slightly with depth
    ! just fill these in with the previous level's value
    do z = 2, size(dens)       
       if(loc_u(z) <= 0) loc_u(z) = loc_u(z-1)
       if(loc_d(z) <= 0) loc_d(z) = loc_d(z-1)
    end do
    
    ! calculate the final correlation lengths
    vtloc = (loc_u * loc_d * min(loc_u,loc_d)**vtloc_pow)**(1/(2.0+vtloc_pow))
    
  end function col_vtloc


  !================================================================================
  !================================================================================

  pure function bgcov_hzdist(lat) result(cor)
    real, intent(in) :: lat
    real :: cor
    ! more accurate rossby radius calculation
    if ( abs(lat) < 0.1) then
       cor = hz_loc(1)
    else
       cor = max(hz_loc(2), min(hz_loc(1), &
            hz_loc_scale*2.6/(2*omega*abs(sin(lat*pi/180.0))) ))
    end if
  end function bgcov_hzdist



  !================================================================================
  !================================================================================


  subroutine check(status)
    !! helper function to wrap calls to netcdf
    integer, intent(in) :: status
    if(status /= nf90_noerr) then
       print *, trim(nf90_strerror(status))
       stop 1
    end if
  end subroutine check


  !================================================================================
  !================================================================================


end program vtloc
