program obsop
  ! TODO, horizontal interpolation
  ! TODO, make sure the datetimes given are in range of the model state given
  use obscom_obsio
  use obscom_grid
  use netcdf
  use gsw_mod_toolbox
  use cubic_spline
  use datetime_module

  implicit none

  ! read in from namelist
  character(len=:), allocatable :: statefile
  integer :: obid_t  = 2210
  integer :: obid_pt = 2211
  integer :: obid_s  = 2220
  real :: lat_bounds(2) = (/-90,90/)

  ! read in from command line
  character(len=1024) :: obsfile
  character(len=1024) :: outfile
  
  character(len=1024) :: nml_file

  type(datetime) :: basedate
  type(observation), allocatable :: obs(:)
  real, allocatable :: obs_inc(:)
  type(obsio_nc) :: obsio

  integer :: i
  integer :: x, y, z
  integer :: prev_x, prev_y
  real :: s, pt, v
  integer :: unit

  type(cspline) :: spline_t, spline_s
  real, allocatable :: state_t(:,:,:), state_s(:,:,:), state_sst(:,:)
  integer :: ncid, vid


  namelist /obsop_nml/ statefile, obid_t, obid_pt, obid_s, lat_bounds

  
  print *, "------------------------------------------------------------"
  print *, " Ocean observation operaetor, for standard variables"
  print *, " (T,Pt,S,U,V)"
  print *, "------------------------------------------------------------"
  
  
  nml_file = "obsprep.nml"
  
  ! read command line arguments
  i = command_argument_count()
  if (i/=2) then
     print *, 'ERROR: command line arguments'
     stop 1
  end if
  call get_command_argument(1, value=obsfile)
  call get_command_argument(2, value=outfile)
  print *, "In:  ", trim(obsfile)
  print *, "Out: ", trim(outfile)

  ! read the namelist
  allocate(character(len=1024) :: statefile)
  open(newunit=unit, file=nml_file)
  read(unit, obsop_nml)
  close(unit)
  statefile=trim(statefile)
  print *, ""
  print obsop_nml
  print *, ""

  ! read in the grid
  call grid_init(nml_file)

  ! read in the model state
  call check(nf90_open(statefile, nf90_nowrite, ncid))

  allocate(state_t(grid_nx, grid_ny, grid_nz))
  print *, "Reading state TEMP..."
  call check(nf90_inq_varid(ncid, "temp", vid))
  call check(nf90_get_var(ncid, vid, state_t))

  allocate(state_s(grid_nx, grid_ny, grid_nz))
  print *, "Reading state SALT..."
  call check(nf90_inq_varid(ncid, "salt", vid))
  call check(nf90_get_var(ncid, vid, state_s))

  allocate(state_sst(grid_nx, grid_ny))
  print *, "Reading state SST..."
  call check(nf90_inq_varid(ncid, "SST_min", vid))
  call check(nf90_get_var(ncid, vid, state_sst))

  call check(nf90_close(ncid))

  call gsw_saar_init(.true.)

  ! read in the observations
  call obsio%read(obsfile, obs, basedate)
  print *, size(obs),"observations read in"
  allocate(obs_inc(size(obs)))


  ! process each observation
  prev_x = -1
  prev_y = -1
  do i=1,size(obs)

     ! set as a bad observation unless we make
     ! it to the end of this loop
     obs(i)%qc = 1

     ! check lat bounds
     if( obs(i)%lat < lat_bounds(1) .or. obs(i)%lat > lat_bounds(2) ) cycle

     ! get the closest ocean grid point
     call grid_ll2xy(obs(i)%lat, obs(i)%lon, x, y)

     ! ignore points that happen to be on land
     if (grid_mask(x,y) < 1) cycle
     
     ! get the model PT, S values at the observation location
     if(obs(i)%plat .ge. 1000 .and. obs(i)%id == obid_t) then
        pt = state_sst(x,y)
     else if(obs(i)%dpth <= grid_depths(1)) then
        ! if at the surface...
        pt = state_t(x,y,1)
        s  = state_s(x,y,1) 
     else
        ! else if at depth
        if(prev_x /= x .or. prev_y /= y) then
           ! generate a new cubic spline of
           ! the t/s profile at the given location
           prev_x = x
           prev_y = y
           spline_t = cspline(grid_depths, state_t(x,y,:))
           spline_s = cspline(grid_depths, state_s(x,y,:))
        end if
        pt  = spline_t%interp(obs(i)%dpth, check=.true.)
        s   = spline_s%interp(obs(i)%dpth, check=.true.)

        ! double check to make sure the interpolation worked
        if (pt == cspline_error .or. s ==cspline_error)  then
           print *, "ERROR interpolating from spline."
           print *, "bg profiles (depth, T, S)"
           do z=1,size(grid_depths)
              print *, grid_depths(z), state_t(x,y,z), state_s(x,y,z)
           end do
           print *, ""
           print *,'interpolation (depth, pt, s)'
           print *, obs(i)%dpth, pt, s
           stop 1
        end if
        
     end if


     !  calculate observation increment
     if(obs(i)%id == obid_s) then
        ! salinity
        v = s
     else if(obs(i)%id == obid_t) then
        ! insitu temperature
        !  note: need to convert model output from
        !   potential temp to insitu here
        if (obs(i)%dpth .gt. 0) then
           v = pt2t(pt,s,obs(i)%dpth, obs(i)%lon, obs(i)%lat)
        else
           v = pt
        end if

     else if(obs(i)%id == obid_pt) then
        ! potential temperature
        v = pt
     else
        print *, "unkown observation id: ",obs(i)%id
        stop 1
     end if   

     obs_inc(i) = obs(i)%val - v

     ! observation has passed this QC
     obs(i)%qc = 0
  end do

  !done, write out observation
  call obsio%write(outfile, obs, basedate, obs_inc)


  
contains


  
  pure function pt2t(pt, sp, z, lon, lat) result(t)
    !! Convert from potential temperature to in-situ temperature
    real, intent(in) :: pt, sp, z, lon, lat
    real :: t
    real(kind=8) :: ct, p, sa

    !NOTE: gsw takes HEIGHT not DEPTH (so negative depth)... 
    ! a feature I didn't notice for months, which means all my temperatures were wrong... ugh
    p = gsw_p_from_z(-z*1d0, lat*1d0)
    sa = gsw_sa_from_sp(sp*1d0, p, lon*1d0, lat*1d0) 
    ct = gsw_ct_from_pt(sa, pt*1d0)
    t = real(gsw_t_from_ct(sa, ct, p))
  end function pt2t

  
  
  pure function t2pt(t, sp, z, lat, lon) result(pt)
    !! convert from insitu temperature to potential temperature
    real, intent(in) :: t, sp, z, lat, lon
    real :: pt
    real(kind=8) :: sa, p

    !NOTE: gsw takes HEIGHT not DEPTH (so negative depth)... 
    p = gsw_p_from_z(-z*1d0, lat*1d0)
    sa = gsw_sa_from_sp(sp*1d0, p, lon*1d0, lat*1d0)
    pt = real(gsw_pt0_from_t(sa, t*1d0, p))
  end function t2pt


  subroutine check(status)
    integer, intent(in) :: status
    if(status /= nf90_noerr) then
       print *, trim(nf90_strerror(status))
       stop 1
    end if
  end subroutine check
end program obsop
