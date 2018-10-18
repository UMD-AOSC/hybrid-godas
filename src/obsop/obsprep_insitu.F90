program obsprep_insitu
  use obscom_obsio
  use obscom_grid
  use cubic_spline
  use netcdf
  use datetime_module

  implicit none


  ! parameters read in from namelist
  integer :: obid_t=2210
  integer :: obid_s=2220
  real    :: err_cscale_mul=6.0
  real    :: err_cscale_dist=800e3
  real    :: err_t_surf=0.78
  real    :: err_t_max=1.0
  real    :: err_t_do=0.07
  real    :: err_t_d(4)=(/75.0,300.0,450.0,1000.0/)
  real    :: err_s_surf=0.18
  real    :: err_s_do=0.02
  real    :: err_s_d=750.0
  logical :: vrt_interp = .FALSE.
  real    :: vrt_interp_lvls(1000) = -1
  

  ! variables read in from the command line
  character(len=1024) :: obsfile
  character(len=1024) :: outfile

   
  ! variables read in from obs file
  integer ::prf_num
  character(len=:), allocatable :: prf_type
  real,      allocatable :: prf_lat(:)
  real,      allocatable :: prf_lon(:)
  real,      allocatable :: prf_hr(:)
  integer,   allocatable :: prf_obsidx(:)
  integer,   allocatable :: prf_obslen(:)
  integer :: obs_num
  real,      allocatable :: obs_depth(:)
  real,      allocatable :: obs_val(:)


  ! output data that is generated here
  ! TODO, remove the hardcoded max value (replace with vector)
  integer, parameter :: obsout_max = 10000000
  type(observation)  :: obsout(obsout_max)
  integer            :: obsout_num

  real, allocatable :: obs_depth2(:)
  real, allocatable :: obs_val2(:)
  
  ! other misc variables
  type(datetime) :: basedate
  integer :: unit, i, p, v, idx1, idx2, cnt, prflen
  type(obsio_nc) :: obsio
  character(len=1) :: var
  integer :: ncid, nc_v
  integer :: obid
  real :: err(1000)
  character(len=1024) :: tmp_str
  integer :: yr,mn,dy
  real, parameter :: pi = 4*atan(1.0)
  integer :: vrt_interp_lvls_cnt

! use this to get the repository version at compile time
#ifndef CVERSION
#define CVERSION "Unknown"
#endif
#ifndef CTIME
#define CTIME "Unknown"
#endif
  
  namelist /obsprep_insitu_nml/ obid_t, obid_s, &
       err_cscale_mul, err_cscale_dist, &
       err_t_surf, err_t_max, err_t_do, err_t_d, &
       err_s_surf, err_s_do, err_s_d, &
       vrt_interp, vrt_interp_lvls

  print *, "------------------------------------------------------------"
  print *, " insitu observation preparation "
  print *, ""
  print *, " version:  ", CVERSION
  print *, " compiled: ", CTIME
  print *, "------------------------------------------------------------"

  call grid_init("obsprep.nml")

  ! read in commane line arguments
  i = command_argument_count()
  if (i /= 2) then
     print *, 'ERROR: call with "obsprep_insitu <inputfile> <outputfile>" '
     stop 1
  end if
  call get_command_argument(1, value=obsfile)
  call get_command_argument(2, value=outfile)
  print *, "In:  ", trim(obsfile)
  print *, "Out: ", trim(outfile)
  print *, ""
  
  !read the namelist
  open(newunit=unit, file="obsprep.nml", status='OLD')
  read(unit, obsprep_insitu_nml)
  close(unit)
  print *, ""
  print obsprep_insitu_nml
  print *, ""

  ! are we going to interpolate profiles to model levels?
  ! count the number of levels
  if(vrt_interp) then
     vrt_interp_lvls_cnt = 0
     do i=1,size(vrt_interp_lvls)
        if(vrt_interp_lvls(i) == -1) exit
        vrt_interp_lvls_cnt = vrt_interp_lvls_cnt + 1 
     end do
  end if
    
  
  obsout_num = 0

  ! for each of TMP and SALT obs types
  do v=1,2
     cnt = 0
     if (v == 1) then
        var = 'T'
        obid=obid_t
     else
        var = 'S'
        obid=obid_s
     end if
     
     ! read in observation
     print *, ""
     print *,"READING ",trim(obsfile)//"."//var//".nc"
     call check(nf90_open(trim(obsfile)//"."//var//".nc", nf90_nowrite, ncid))

     call check(nf90_get_att(ncid, nf90_global, "date", tmp_str))
     read (tmp_str(1:4), *) yr
     read (tmp_str(5:6), *) mn
     read (tmp_str(7:8), *) dy
     basedate=datetime(yr,mn,dy,0)
     
     call check(nf90_inq_dimid(ncid, 'obs', nc_v))
     call check(nf90_inquire_dimension(ncid, nc_v, len=obs_num))     
     call check(nf90_inq_dimid(ncid, 'prfs', nc_v))
     call check(nf90_inquire_dimension(ncid, nc_v, len=prf_num))
     allocate(obs_depth(obs_num))
     allocate(obs_val(obs_num))
     allocate(character(len=prf_num) :: prf_type)
     allocate(prf_lat(prf_num))
     allocate(prf_lon(prf_num))
     allocate(prf_hr(prf_num))
     allocate(prf_obsidx(prf_num))
     allocate(prf_obslen(prf_num))

     call check(nf90_inq_varid(ncid, "obs_depth", nc_v))
     call check(nf90_get_var(ncid, nc_v, obs_depth))
     call check(nf90_inq_varid(ncid, "obs_val", nc_v))
     call check(nf90_get_var(ncid, nc_v, obs_val))
     call check(nf90_inq_varid(ncid, "prf_type", nc_v))
     call check(nf90_get_var(ncid, nc_v, prf_type))
     call check(nf90_inq_varid(ncid, "prf_lat", nc_v))
     call check(nf90_get_var(ncid, nc_v, prf_lat))
     call check(nf90_inq_varid(ncid, "prf_lon", nc_v))
     call check(nf90_get_var(ncid, nc_v, prf_lon))
     call check(nf90_inq_varid(ncid, "prf_hr", nc_v))
     call check(nf90_get_var(ncid, nc_v, prf_hr))
     call check(nf90_inq_varid(ncid, "prf_obsidx", nc_v))
     call check(nf90_get_var(ncid, nc_v, prf_obsidx))
     call check(nf90_inq_varid(ncid, "prf_obslen", nc_v))
     call check(nf90_get_var(ncid, nc_v, prf_obslen))

     print*,"loaded",obs_num,"obs from",prf_num,"profiles"
     call check(nf90_close(ncid))

     ! for each profile, do optional interpolation
     ! place interpolated values onto obs_*_interp arrays
     if(vrt_interp) then
        ! move values over to temporary array
        allocate(obs_depth2(obs_num))
        allocate(obs_val2(obs_num))
        obs_depth2 = obs_depth
        obs_val2 = obs_val
        deallocate(obs_val, obs_depth)        
        allocate(obs_depth(prf_num * vrt_interp_lvls_cnt))
        allocate(obs_val(prf_num * vrt_interp_lvls_cnt))
        obs_num=0

        ! interpolate each profile, updating the obs_val and obs_depth arrays
        do p=1,prf_num
           idx1 = prf_obsidx(p)
           idx2 = prf_obsidx(p) + prf_obslen(p) - 1

           call interp(obs_depth2(idx1:idx2), obs_val2(idx1:idx2), &
                obs_depth(obs_num+1:size(obs_depth)), &
                obs_val(obs_num+1:size(obs_val)), &                
                i)
           
           prf_obsidx(p) = obs_num + 1
           prf_obslen(p) = i          
           obs_num = obs_num + i           
           
        end do

        ! cleanup
        deallocate(obs_depth2)
        deallocate(obs_val2)
     end if

     ! for each profile, determine the error profile
     ! create output observations
     do p=1,prf_num
        idx1 = prf_obsidx(p)
        idx2 = prf_obsidx(p) + prf_obslen(p) - 1
        prflen = prf_obslen(p)

        ! calculate error profile
        if (v == 1) then
           call calcErr_t(prf_lon(p), prf_lat(p), obs_depth(idx1:idx2), err(1:prf_obslen(p)))
        else
           call calcErr_s(prf_lon(p), prf_lat(p), obs_depth(idx1:idx2), err(1:prf_obslen(p)))
        end if

        ! generate final observation object that will be saved
        do i=idx1,idx1+prflen-1
!           print *, obs_depth(i), obs_val(i)
           cnt = cnt + 1
           obsout_num = obsout_num + 1
           obsout(obsout_num)%id   = obid
           obsout(obsout_num)%plat = 1
           obsout(obsout_num)%lat  = prf_lat(p)
           obsout(obsout_num)%lon  = prf_lon(p)
           obsout(obsout_num)%dpth = obs_depth(i)
           obsout(obsout_num)%hr   = prf_hr(p)
           obsout(obsout_num)%val  = obs_val(i)
           obsout(obsout_num)%err  = err(i-idx1+1)
           obsout(obsout_num)%qc   = 0
        end do
     end do

     print *, cnt,"observations kept"
     deallocate(obs_depth)
     deallocate(obs_val)
     deallocate(prf_type)
     deallocate(prf_lat)
     deallocate(prf_lon)
     deallocate(prf_hr)
     deallocate(prf_obsidx)
     deallocate(prf_obslen)
     
  end do

  print *, ""
  print *, "Writing to output file...",trim(outfile)
  print *, obsout_num, " total observations"
  call obsio%write(outfile,obsout(1:obsout_num), basedate)


contains


  subroutine interp(depth_in, val_in, depth_out, val_out, cnt_out)
    real, intent(in) :: depth_in(:), val_in(:)
    real, intent(inout) :: depth_out(:), val_out(:)
    integer, intent(out) :: cnt_out

    type(cspline) :: spline
    integer :: i
    integer :: dst_lvl_start, dst_lvl_end
    
    ! if there are less than 2 levels, dont interpolate
    if(size(val_in) < 2) then
       cnt_out = size(val_in)
       depth_out(1:cnt_out) = depth_in
       val_out(1:cnt_out) = val_in
       return
    end if
    
    ! determien the relevant levels of the reference grid
    dst_lvl_start = 0
    cnt_out =0
    dst_lvl_end =0
    do i=1, vrt_interp_lvls_cnt
       if(vrt_interp_lvls(i) > depth_in(size(depth_in))) exit
       if(vrt_interp_lvls(i) >= depth_in(1)) then
          cnt_out = cnt_out + 1
          dst_lvl_end = i
          if(dst_lvl_start ==0) dst_lvl_start = i
       end if
    end do

    ! interpolate to the desired levels
    depth_out(1:cnt_out) = vrt_interp_lvls(dst_lvl_start:dst_lvl_end)    
    spline = cspline(depth_in, val_in)
    val_out = spline%interp(depth_out(1:cnt_out), .TRUE.)    
  end subroutine interp
  


  subroutine calcErr_t(lon, lat, depth, err)
    real, intent(in) :: lon, lat
    real, intent(in) :: depth(:)
    real, intent(out):: err(:)

    integer :: x,y
    real :: coast_dist
    real :: cscale

    ! determine the profile distance to coast    
    call grid_ll2xy(lat, lon, x, y)
    coast_dist=grid_coast(x,y)   

    ! calculate vertical error profile
    do i=1,size(depth)
       if ( depth(i) <= err_t_d(1) ) then
          err(i) = (err_t_surf - err_t_max)*(-depth(i) / err_t_d(1)) + err_t_surf
       elseif ( depth(i) <= err_t_d(2) ) then
          err(i) = (err_t_max * exp( (-depth(i)+err_t_d(1)) / err_t_d(3)))
       else
          err(i) = max(err_t_do, err_t_max*exp( (-err_t_d(2) + err_t_d(1))/err_t_d(3))*&
               exp( (-depth(i)+err_t_d(2))/err_t_d(4)))
       end if
    end do  

    ! Calculate scaling based on distance from coast
    cscale = 1.0
    if (err_cscale_dist > coast_dist) then
       cscale = 0.5*(1+cos(pi * coast_dist /err_cscale_dist))
       cscale = min(1.0, max(0.0, cscale))
       cscale = cscale * (err_cscale_mul - 1.0) + 1.0
    end if
    err = err * cscale

  end subroutine calcErr_t



  subroutine calcErr_s(lon, lat, depth, err)
    real, intent(in) :: lon, lat
    real, intent(in) :: depth(:)
    real, intent(out):: err(:)

    integer :: x, y, i 
    real :: coast_dist
    real :: cscale

    ! determine the profile distance to coast    
    call grid_ll2xy(lat, lon, x, y)
    coast_dist=grid_coast(x,y)   

    ! calculate vertical error profile
    do i=1,size(depth)
       err(i) = max(err_s_do, err_s_surf*exp(-depth(i)/err_s_d))
    end do

    ! Calculate scaling based on distance from coast
    cscale = 1.0
    if (err_cscale_dist > coast_dist) then
       cscale = 0.5*(1+cos(pi * coast_dist /err_cscale_dist))
       cscale = min(1.0, max(0.0, cscale))
       cscale = cscale * (err_cscale_mul - 1.0) + 1.0
    end if
    err = err * cscale

  end subroutine calcErr_s



  pure subroutine calcErr_db(depth, val, se_min, se_max, err)
    !!observation error calculation, dave behringer method
    real, intent(in) :: depth(:)
    real, intent(in) :: val(:)    
    real, intent(in) :: se_min
    real, intent(in) :: se_max
    real, intent(inout) :: err(:)


    integer,parameter :: smooth_size = 2
    real :: err2(size(err))
    integer :: plen, z, z2, cnt

    real :: errmin, errmax

    plen = size(depth)
    err = 0.0

    ! calculate gradient
    if ( plen > 1) then
       err(1) = (val(1)-val(2)) / (depth(2)-depth(11))
       do z = 2, plen-1
          err(z) = (val(z-1)-val(z+1)) / (depth(z+1)-depth(z-1))
       end do
       err(plen) = (val(plen-1)-val(plen)) / (depth(plen)-depth(plen-1))
    end if
    err = abs(err)

    ! smooth the gradient
    err2 =0.0
    do z=1,plen
       cnt = 0
       do z2 = z-smooth_size, z+smooth_size
          if (z < 1 .or. z > plen) cycle
          err2(z) = err2(z) + err(z)
          cnt = cnt + 1
       end do
       err2(z) = err2(z) / cnt
    end do

    ! rescale between 0 and 1
    errmin = minval(err)
    errmax = maxval(err)
    if ((errmax-errmin)/errmax > 0.1) then
       err = (err-errmin)/(errmax-errmin)
    else
       err =0
    end if
       
       
    ! calculate final err stddev
    err = (se_max-se_min)*err + se_min

  end subroutine calcErr_db




  subroutine check(status)
    integer, intent(in) :: status
    if(status /= nf90_noerr) then
       print *, trim(nf90_strerror(status))
       stop 1
    end if
  end subroutine check

end program obsprep_insitu
