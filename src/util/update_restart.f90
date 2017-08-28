program update_restart
  use netcdf
  use datatable
  use gsw_mod_toolbox
  implicit none

  ! TODO, move these to namelist
  integer :: grid_nx = 1440
  integer :: grid_ny = 1080
  integer :: grid_nz = 75
  real :: salt_min = 0.01
  real :: salt_max = 50.0
  real :: temp_min = -3.0
  real :: temp_max = 50.0


  integer :: nc_ai, nc_rst
  integer :: v1, v2
  integer :: x,y,z

  real, allocatable :: grid_D(:,:)
  real, allocatable :: grid_dpth(:)
  real, allocatable :: grid_btm(:,:)
  real, allocatable :: d1(:,:,:)
  real, allocatable :: d2(:,:,:)

  ! initialize / read in grid
  call gsw_saar_init(.true.)

  call datatable_init(.true., "datatable.3dvar")
  allocate(grid_D(grid_nx, grid_ny))
  call datatable_get('grid_D', grid_D)
  allocate(grid_dpth(grid_nz))
  call datatable_get('grid_z', grid_dpth)


  ! calculate bottom levels
  allocate(grid_btm(grid_nx, grid_ny))
  do y=1,grid_ny
     do x=1,grid_nx
        do z=1,grid_nz
           if(grid_dpth(z) > grid_D(x,y)) then
              grid_btm(x,y) = z-1
              exit
           end if
           grid_btm(x,y) = grid_nz
        end do
     end do
  end do


  ! open restart and analysis increment files
  call check(nf90_open('ana_inc.nc', NF90_NOWRITE, nc_ai))
  call check(nf90_open('RESTART/MOM.res.nc', NF90_WRITE, nc_rst))
  allocate(d1(grid_nx, grid_ny, grid_nz))
  allocate(d2(grid_nx, grid_ny, grid_nz))


  !------------------------------
  ! temperature
  !------------------------------
  call check(nf90_inq_varid(nc_ai, 'ai_temp', v1))
  call check(nf90_inq_varid(nc_rst, 'Temp', v2))
  call check(nf90_get_var(nc_ai, v1, d1))
  call check(nf90_get_var(nc_rst, v2, d2))
  d2 = d2 + d1
  do y=1,grid_ny
     do x=1,grid_nx
        do z=1,grid_btm(x,y)
           if(d2(x,y,z) < temp_min) d2(x,y,z) = temp_min
           if(d2(x,y,z) > temp_max) d2(x,y,z) = temp_max
        end do
     end do
  end do
  call check(nf90_put_var(nc_rst, v2, d2))


  !----------------------------------------
  ! salinity
  !----------------------------------------
  call check(nf90_inq_varid(nc_ai, 'ai_salt', v1))
  call check(nf90_inq_varid(nc_rst, 'Salt', v2))
  call check(nf90_get_var(nc_ai, v1, d1))
  call check(nf90_get_var(nc_rst, v2, d2))
  d2 = d2 + d1
  do y=1,grid_ny
     do x=1,grid_nx
        do z= 1,grid_btm(x,y)
           if(d2(x,y,z) < salt_min) d2(x,y,z) = salt_min
           if(d2(x,y,z) > salt_max) d2(x,y,z) = salt_max
        end do
     end do
  end do
  call check(nf90_put_var(nc_rst, v2, d2))


  ! all done
  !------------------------------------------------------------
  call check(nf90_close(nc_rst))
  call check(nf90_close(nc_ai))



contains

  

  subroutine check(status)
    integer, intent(in) :: status
    if(status /= nf90_noerr) then
       print *, trim(nf90_strerror(status))
       stop 1
    end if
  end subroutine check



end program update_restart
