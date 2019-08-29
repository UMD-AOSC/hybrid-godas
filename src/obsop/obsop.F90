program obsop
  ! TODO, horizontal interpolation
  ! TODO, make sure the datetimes given are in range of the model state given
  use obscom_obsio
  use obscom_grid
  use netcdf
  use gsw_mod_toolbox
  use cubic_spline
  use datetime_module
  use ieee_arithmetic

  implicit none

  ! read in from namelist
  integer :: obid_adt = 2100
  integer :: obid_t   = 2210
  integer :: obid_pt  = 2211
  integer :: obid_s   = 2220
  logical :: rm_adt_mean_inc = .true.
  real :: lat_bounds(2) = (/-90,90/)

  ! read in from command line
  character(len=1024) :: obsfile
  character(len=1024) :: statefile
  character(len=1024) :: outfile
  
  character(len=1024) :: nml_file
  character(len=1024) :: str1, str2


  type(datetime) :: basedate
  type(observation), allocatable :: obs(:)
  real, allocatable :: obs_inc(:)
  type(obsio_nc) :: obsio

  integer :: decomp_x(4), decomp_y(4), dims(4)
  integer :: i, bad_val, bad_inc, bad_err, bad_land, num, n, args, j
  integer :: x, y, z, ierr
  integer :: btm
  integer :: prev_x, prev_y
  real :: s, pt, v
  integer :: unit

  type(cspline) :: spline_t, spline_s
  real, allocatable :: state_t(:,:,:), state_s(:,:,:), state_sst(:,:), state_ssh(:,:), state_valid(:,:)
  real, allocatable :: tmp3d(:,:,:), tmp2d(:,:)
  integer :: ncid, vid

  logical :: has_ssh

! use this to get the repository version at compile time
#ifndef CVERSION
#define CVERSION "Unknown"
#endif
#ifndef CTIME
#define CTIME "Unknown"
#endif

  namelist /obsop_nml/ obid_t, obid_pt, obid_s, obid_adt, lat_bounds, rm_adt_mean_inc

  
  print *, "------------------------------------------------------------"
  print *, " Ocean observation operator, for standard variables"
  print *, " (T,Pt,S,U,V)"
  print *, ""
  print *, " version:  ", CVERSION
  print *, "------------------------------------------------------------"
  
  
  nml_file = "obsprep.nml"
  
  ! read command line arguments
  args = command_argument_count()
  if (args < 3) then
     print *, 'ERROR, usage: obsop <input_obs> <input_state(s)> <output_obs>'
     stop 1
  end if
  call get_command_argument(1, value=obsfile)
  call get_command_argument(args, value=outfile)

  print *, "In:  ", trim(obsfile)
  print *, "Out: ", trim(outfile)

  ! read the namelist
  open(newunit=unit, file=nml_file)
  read(unit, obsop_nml)
  close(unit)
  print *, ""
  print obsop_nml
  print *, ""

  ! read in the grid
  call grid_init(nml_file)



  ! read in the model state(s)
  !------------------------------------------------------------
  allocate(state_t(grid_nx, grid_ny, grid_nz))
  allocate(state_s(grid_nx, grid_ny, grid_nz))
  allocate(state_sst(grid_nx, grid_ny))
  allocate(state_ssh(grid_nx, grid_ny))
  allocate(state_valid(grid_nx, grid_ny))

  state_valid=0.0

  do i =2,args-1
     call get_command_argument(i, value=statefile)
     call check(nf90_open(statefile, nf90_nowrite, ncid))

     ! find the x/y dimensions
     call check(nf90_inq_varid(ncid, "Temp", vid))
     call check(nf90_inquire_variable(ncid, vid, dimids=dims))
     call check(nf90_inquire_dimension(ncid, dims(1), name=str1))
     call check(nf90_inquire_dimension(ncid, dims(2), name=str2))
     if (i==2) THEN
        print *, ""
        print *, "X dimension: ", TRIM(str1)
        print *, "Y dimension: ", TRIM(str2)
        print *, ""
     end if


     ! is this a decomposed grid?
     decomp_x=(/1,grid_nx,1,grid_nx/)
     decomp_y=(/1,grid_ny,1,grid_ny/)
     call check(nf90_inq_varid(ncid, str1, vid))
     ierr=nf90_inquire_attribute(ncid, vid, "domain_decomposition", attnum=j)
     if(j > 0) then
        call check(nf90_get_att(ncid, vid, "domain_decomposition", decomp_x))
     end if
     call check(nf90_inq_varid(ncid, str2, vid))
     ierr=nf90_inquire_attribute(ncid, vid, "domain_decomposition", attnum=j)
     if(j > 0) then
        call check(nf90_get_att(ncid, vid, "domain_decomposition", decomp_y))
     end if

     allocate(tmp3d(decomp_x(4)-decomp_x(3)+1, decomp_y(4)-decomp_y(3)+1, grid_nz))
     allocate(tmp2d(decomp_x(4)-decomp_x(3)+1, decomp_y(4)-decomp_y(3)+1))
     state_valid(decomp_x(3):decomp_x(4), decomp_y(3):decomp_y(4)) = 1.0
     

     ! read state variables
     call check(nf90_inq_varid(ncid, "Temp", vid))
     call check(nf90_get_var(ncid, vid, tmp3d))
     state_t(decomp_x(3):decomp_x(4), decomp_y(3):decomp_y(4), :) = tmp3d

     call check(nf90_inq_varid(ncid, "Salt", vid))
     call check(nf90_get_var(ncid, vid, tmp3d))
     state_s(decomp_x(3):decomp_x(4), decomp_y(3):decomp_y(4), :) = tmp3d

     j=nf90_inq_varid(ncid, "SST_min", vid)
     if (j /= NF90_NOERR) then
        if(i==2) then
           print *, " WARNING: no SST  variable found, using top level of Temp"
        end if
        state_sst(decomp_x(3):decomp_x(4), decomp_y(3):decomp_y(4))=&
             state_t(decomp_x(3):decomp_x(4), decomp_y(3):decomp_y(4),1)
     else
        call check(nf90_get_var(ncid, vid, tmp2d))
        state_sst(decomp_x(3):decomp_x(4), decomp_y(3):decomp_y(4)) = tmp2d
     end if

     j = nf90_inq_varid(ncid, "SSH", vid)
     has_ssh = j == NF90_NOERR
     if (.not. has_ssh) then
        if(i==2) then
           print *, "WARNING: no SSH variable found, marking all SSH obs as bad"
        end if
     else
        call check(nf90_get_var(ncid, vid, tmp2d))
        state_ssh(decomp_x(3):decomp_x(4), decomp_y(3):decomp_y(4)) = tmp2d
     end if


     ! cleanup
     call check(nf90_close(ncid))
     deallocate(tmp3d)
     deallocate(tmp2d)
  end do

  if(minval(state_valid) == 0.0) then
     print *, ""
     print *, "WARNING: there are gridpoints that were not read in. If reading tiled files, make sure all filenames were given."
  end if



  !------------------------------------------------------------

  ! read in the observations
  call obsio%read(obsfile, obs, basedate)
  print *, size(obs),"observations read in"
  allocate(obs_inc(size(obs)))


  ! process each observation
  obs_inc=0
  bad_land=0
  bad_val=0
  bad_err=0
  bad_inc=0
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
     if (grid_mask(x,y) < 1) then
        bad_land = bad_land + 1
        cycle
     end if
     
     ! get the model T/S values at the observation location
     if(obs(i)%id == obid_adt) then
        continue ! ssh doesn't need T/S
     else if(obs(i)%plat .ge. 1000 .and. obs(i)%id == obid_t) then
        ! satellite SST, use the SST state
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

           ! TODO: replace with gsw_rr68_interp()

           ! determine the bottom level 
           ! TODO: faster to replace this with a binary search
           ! TODO: not needed once switching to hybrid, need to use actual 
           !  interface depths though
           do btm = grid_nz,1,-1
              if (grid_D(x,y) >= grid_depths(btm) .and. state_t(x,y,btm) < 1e5) exit
           end do

           ! fit a spline to the T/S
           prev_x = x
           prev_y = y
           spline_t = cspline(grid_depths(:btm), state_t(x,y,:btm))
           spline_s = cspline(grid_depths(:btm), state_s(x,y,:btm))
        end if

        if (obs(i)%dpth > grid_depths(btm)) then
           bad_land = bad_land+1
           obs(i)%err = 0
           obs(i)%qc = 1
           cycle
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
           print *, "bottom (lvl_idx, lvl_meters, depth)"
           print *, btm, grid_depths(btm), grid_D(x,y)
           print *,""
           print *,'interpolation (depth, pt, s)'
           print *, obs(i)%dpth, pt, s
           stop 1
        end if
        
     end if


     !  calculate observation increment
     if(obs(i)%id == obid_adt) then
        if(.not. has_ssh) cycle
        v = state_ssh(x,y)
     else if(obs(i)%id == obid_s) then
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

     ! extra checks on the numbers for obvious bad values
     if ( .not. ieee_is_finite(obs(i)%val)) then
        bad_val = bad_val + 1
        obs(i)%val = 0
     else if( .not. ieee_is_finite(obs(i)%err)) then
        bad_err = bad_err + 1
        obs(i)%err = 0
     else if ( .not. ieee_is_finite(obs_inc(i))) then
        bad_inc = bad_inc + 1
        obs_inc(i) = 0
     else
        ! observation has passed this QC
        obs(i)%qc = 0
     end if
  end do


  ! remove the mean increment from the adt observations
  if (rm_adt_mean_inc) then
     num=0
     v=0
     ! calculate the running mean
     do i=1,size(obs)
        if (obs(i)%qc /= 0) cycle
        if (obs(i)%id /= obid_adt) cycle
        num = num + 1
        v = v + (obs_inc(i) - v)/num
     end do

     if (num > 0) then
        print *, ""
        print *, "Removing mean ADT incrment of ",v
     end if

     ! go through and remove the mean 
     do i=1, size(obs)
        if(obs(i)%id /= obid_adt) cycle
        obs_inc(i) = obs_inc(i) - v
        obs(i)%val = obs(i)%val - v
     end do
  end if

  ! count the number of bad obs
  num=0
  do i=1,size(obs)
     if(obs(i)%qc == 0) num = num +1
  end do
  print *, ""
  print *, "Good obs:  ", num
  print *, " bad ob:   ", bad_val
  print *, " bad h(x): ", bad_inc
  print *, " bad err:  ", bad_err
  print *, " bad land: ", bad_land

  
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
