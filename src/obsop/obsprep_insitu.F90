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
  integer, parameter :: obsout_max = 1000000
  type(observation)  :: obsout(obsout_max)
  integer            :: obsout_num


  ! other misc variables
  type(datetime) :: basedate
  integer :: unit, i, j, p, v, z, idx1, idx2, cnt, prflen
  type(obsio_nc) :: obsio
  character(len=1) :: var
  integer :: ncid, nc_v
  integer :: obid
  type(cspline) :: spline
  real :: err(1000)
  real :: se(2)
  character(len=1024) :: tmp_str
  integer :: yr,mn,dy
  integer :: x,y
  real, parameter :: pi = 4*atan(1.0)

! use this to get the repository version at compile time
#ifndef CVERSION
#define CVERSION "Unknown"
#endif
#ifndef CTIME
#define CTIME "Unknown"
#endif
  
!  namelist /obsprep_insitu_nml/ obid_t, obid_s, se_t, se_s
  namelist /obsprep_insitu_nml/ obid_t, obid_s, &
       err_cscale_mul, err_cscale_dist, &
       err_t_surf, err_t_max, err_t_do, err_t_d, &
       err_s_surf, err_s_do, err_s_d

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


  obsout_num = 0

  ! for each of TMP and SALT obs types
  do v=1,2
     cnt = 0
     if (v == 1) then
        var = 'T'
!        se = se_t
        obid=obid_t
     else
        var = 'S'
!        se = se_s
        obid=obid_s
     end if
     
     ! read in observation
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


     ! for each profile, determine the error profile
     ! create output observations
     do p=1,prf_num
!        print *,obid 
!        print *, "" 
!        print*, "Profile",p 
        idx1 = prf_obsidx(p)
        idx2 = prf_obsidx(p) + prf_obslen(p) - 1
        prflen = prf_obslen(p)

        ! calculate error profile
        if (v == 1) then
           call calcErr_t(prf_lon(p), prf_lat(p), obs_depth(idx1:idx2), obs_val(idx1:idx2), err(1:prf_obslen(p)))
        else
           call calcErr_s(prf_lon(p), prf_lat(p), obs_depth(idx1:idx2), obs_val(idx1:idx2), err(1:prf_obslen(p)))
        end if

        ! calculate error profile
!        call calcErr_db(obs_depth(idx1:idx2), obs_val(idx1:idx2), se(1), se(2), err(1:prf_obslen(p)))
        

        ! add interpolated obs where there is stratified ocean
        ! TODO

        ! thin the profile vertically
!        call prfThin(obs_depth(idx1:idx2), obs_val(idx1:idx2), prflen)

        ! generate final observation object that will be saved
        do i=idx1,idx1+prflen-1
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
!          print*, obs_depth(i), obs_val(i), err(i-idx1+1)
        end do                
     end do
 !    print *, obsout_num
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

  print *, "Writing to output file...",trim(outfile)
  call obsio%write(outfile,obsout(1:obsout_num), basedate)


contains


  subroutine prfThin(depth, val, count)
    real, intent(inout) :: depth(:)
    real, intent(inout) :: val(:)
    integer, intent(inout) :: count

    integer :: lvl(size(depth))
    real    :: lvl_dist(size(depth))
    real    :: d
    integer :: z, zprev, z2

    lvl = -1
    lvl_dist = 1e9

    ! determine the closest level, and distance from center of level, for each ob    
    do z=1,size(depth)
       do z2=1,size(grid_depths)
          d = abs(grid_depths(z2)-depth(z))
          if(d <  lvl_dist(z)) then
             lvl_dist(z) = d
             lvl(z) = z2
          end if
       end do
!       print *, depth(z), grid_depths(lvl(z)), lvl(z), lvl_dist(z), val(z)
    end do


    ! do the thinning
    ! TODO
  end subroutine prfThin


  subroutine calcErr_t(lon, lat, depth, val, err)
    real, intent(in) :: lon, lat
    real, intent(in) :: depth(:)
    real, intent(in) :: val(:)
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



  subroutine calcErr_s(lon, lat, depth, val, err)
    real, intent(in) :: lon, lat
    real, intent(in) :: depth(:)
    real, intent(in) :: val(:)
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
