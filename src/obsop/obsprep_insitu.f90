program obs_prep_insitu
  !! prepares insitu T/S ocean observations by performing 
  !! hz/vt binning to do superobbing. Each co-located profile 
  !! has a cubic spline fit to it, and the common vertical levels are
  !! sampled from that. Observation error is estimated from the spread of the
  !! profiles. Background density is used to fill determine where gaps in the profile
  !! need to be interpolated in where the model is highly stratified.

  use obscom_obsio
  use obscom_grid
  use linked_list_type
  use cubic_spline
  use netcdf
  implicit none

  ! variables read in from namelist
  character(len=:), allocatable :: obsfile
  character(len=:), allocatable :: outfile
  character(len=:), allocatable :: densityfile
  character(len=:), allocatable :: densityvar
  ! if < 0, error is determined from profile spread
  ! otherwise specified here
  real :: err_t = -1.0  
  real :: err_grad_scale_t = 10.0
  real :: err_grad_min_t   = 0.1
  real :: err_grad_max_t   = 2
  real :: err_s = -1.0
  real :: err_grad_scale_s = 10.0
  real :: err_grad_min_s   = 0.01
  real :: err_grad_max_s   = 0.2
  real :: density_sigma = 0.125
  integer :: obid_t  = 2210
  integer :: obid_pt = 2211
  integer :: obid_s  = 2220
  integer :: platid = 1

  
  character(len=1024) :: nml_file
  integer :: unit

  integer :: ncid, vid
  integer :: nobs
  integer, allocatable :: ob_id(:)
  real,    allocatable :: ob_lat(:)
  real,    allocatable :: ob_lon(:)
  real,    allocatable :: ob_val(:)
  real,    allocatable :: ob_dpth(:)
  integer, allocatable :: ob_prf(:)
  
  integer :: i, x, y, bin_len, z
  real :: err, lat, lon
  integer :: prev_id
  integer :: prf_num 
  integer, allocatable :: prf_start(:), prf_end(:)
  integer :: cnt_tmp, cnt_tmp_thinned, cnt_sal, cnt_sal_thinned

  real, allocatable :: bg_density(:,:,:)

  ! horizontal profile binning
  type(linked_list), allocatable :: hzbin_temps(:,:)
  type(linked_list), allocatable :: hzbin_salts(:,:)

  ! final profiles
  real, allocatable :: prf_dpth(:), prf_val(:), prf_sprd(:)
  integer, allocatable :: prf_cnt(:)
  integer :: prf_lvls

  type(observation), allocatable :: obsout(:)
  integer :: obsout_cnt
  type(obsio_nc) :: obsio

  
  namelist /obsprep_insitu_nml/ obsfile, outfile, densityfile, densityvar, density_sigma,&
       obid_t, obid_s, obid_pt, platid, &
       err_t, err_grad_scale_t, err_grad_min_t, err_grad_max_t,&
       err_s, err_grad_scale_s, err_grad_min_s, err_grad_max_s
  

  print *, "------------------------------------------------------------"
  print *, "  insitu observation preparation"
  print *, "------------------------------------------------------------"

  nml_file = "obsprep_insitu.nml"
  call grid_init(nml_file)

  allocate(prf_dpth(grid_nz))
  allocate(prf_val(grid_nz))
  allocate(prf_sprd(grid_nz))
  allocate(prf_cnt(grid_nz))
  
  
  ! read the namelist
  allocate(character(len=1024) :: obsfile)
  allocate(Character(len=1024) :: outfile)
  allocate(character(len=1024) :: densityfile)
  allocate(character(len=1024) :: densityvar)
  open(newunit=unit, file=nml_file)
  read(unit, obsprep_insitu_nml)
  close(unit)
  obsfile=trim(obsfile)
  outfile=trim(outfile)
  densityfile=trim(densityfile)
  densityvar=trim(densityvar)
  print *, ""
  print obsprep_insitu_nml
  print *, ""
  

  ! read the background density
  !------------------------------------------------------------
  if(density_sigma > 0) then
     print *, ""
     print *, "Reading background density..."
     print *, ""
     allocate(bg_density(grid_nx, grid_ny, grid_nz))
     call check(nf90_open(densityfile, nf90_nowrite, ncid))
     call check(nf90_inq_varid(ncid, densityvar, vid))
     call check(nf90_get_var(ncid, vid, bg_density))
     call check(nf90_close(ncid))
  end if


  ! read the observations
  ! ------------------------------------------------------------
  ! the ob_* arrays will be filled after this
  print *, "Reading observations..."
  call check(nf90_open(obsfile, nf90_nowrite, ncid))
  call check(nf90_inq_dimid(ncid, 'obs', vid))
  call check(nf90_inquire_dimension(ncid, vid, len=nobs))
  print *, "  ",nobs,"incoming observations"

  allocate(ob_id(nobs))
  call check(nf90_inq_varid(ncid, 'obid', vid))
  call check(nf90_get_var(ncid, vid, ob_id))

  allocate(ob_lat(nobs))
  call check(nf90_inq_varid(ncid, 'lat', vid))
  call check(nf90_get_var(ncid, vid, ob_lat))

  allocate(ob_lon(nobs))
  call check(nf90_inq_varid(ncid, 'lon', vid))
  call check(nf90_get_var(ncid, vid, ob_lon))

  allocate(ob_val(nobs))
  call check(nf90_inq_varid(ncid, 'val', vid))
  call check(nf90_get_var(ncid, vid, ob_val))

  allocate(ob_dpth(nobs))
  call check(nf90_inq_varid(ncid, 'depth', vid))
  call check(nf90_get_var(ncid, vid, ob_dpth))

  allocate(ob_prf(nobs))
  call check(nf90_inq_varid(ncid, 'prf', vid))
  call check(nf90_get_var(ncid, vid, ob_prf))
  
  call check(nf90_close(ncid))

  allocate(obsout(nobs))
  obsout_cnt = 0


  ! break up into individual profiles
  ! ------------------------------------------------------------
  ! afterward the following will be set:
  ! prf_num = total number of individual profiles
  !           and size of prf_start and prf_end
  ! prf_start = index within the ob_* arrays of the start
  !             of a given profile
  ! prf_end   = index within the ob_* arrays of the end
  !             of a given profile
  print *, "dividing into individual profiles..."
  prf_num = 0
  prev_id = -1
  allocate(prf_start(size(ob_id)))
  allocate(prf_end(size(ob_id)))
  do i=1,size(ob_id)
     if(prev_id /= ob_prf(i)) then
        if (prf_num > 0) prf_end(prf_num) = i-1
        prev_id = ob_prf(i)
        prf_num = prf_num + 1
        prf_start(prf_num) = i
     end if
  end do
  prf_end(prf_num) = size(ob_id)

  
  ! put the profiles in horizontal gridbox bins
  ! ------------------------------------------------------------
  print *, "placing profiles into horizontal bins..."
  allocate(hzbin_temps(grid_nx, grid_ny))
  allocate(hzbin_salts(grid_nx, grid_ny))
  do i = 1, prf_num     
     call grid_ll2xy(ob_lat(prf_start(i)), ob_lon(prf_start(i)), x, y)
     if (ob_id(prf_start(i)) == obid_t) then
        call hzbin_temps(x,y)%append(i)
     else if(ob_id(prf_start(i)) == obid_s) then
        call hzbin_salts(x,y)%append(i)
     end if
  end do

  
  ! for each bin, create a single profile to save
  !------------------------------------------------------------
  ! TODO, does plat and hr need to be filled in?
  print *, "creating single profiles..."
  cnt_tmp = 0
  cnt_tmp_thinned = 0
  cnt_sal = 0
  cnt_sal_thinned = 0
  do x=1,grid_nx
    do y=1,grid_ny
       
        ! ignore points that end up on land
        if(grid_mask(x,y) < 1.0) cycle
        

        ! temperature profiles
        !------------------------------------------------------------
        bin_len = hzbin_temps(x,y)%len()
        cnt_tmp = cnt_tmp + bin_len
        if ( bin_len > 0) then
           call avg_profiles(hzbin_temps(x,y), lat, lon, bg_density(x,y,:), &
                prf_dpth, prf_val, prf_sprd, prf_cnt, prf_lvls, &
                err_grad_scale_t, err_grad_min_t, err_grad_max_t)
           do z=1,prf_lvls
              obsout_cnt = obsout_cnt + 1
              obsout(obsout_cnt)%id   = obid_t
              obsout(obsout_cnt)%plat = platid
              obsout(obsout_cnt)%lat  = lat
              obsout(obsout_cnt)%lon  = lon
              obsout(obsout_cnt)%dpth = prf_dpth(z)
              obsout(obsout_cnt)%hr   = 0
              obsout(obsout_cnt)%val  = prf_val(z)
              obsout(obsout_cnt)%err  = merge(prf_sprd(z), err_t, err_t < 0)
              obsout(obsout_cnt)%qc   = 0
           end do
           cnt_tmp_thinned = cnt_tmp_thinned + 1
        end if

     
        ! salinity profiles
        !------------------------------------------------------------
        bin_len = hzbin_salts(x,y)%len()
        cnt_sal = cnt_sal + bin_len
        if ( bin_len > 0 ) then
           call avg_profiles(hzbin_salts(x,y), lat, lon, bg_density(x,y,:), &
                prf_dpth, prf_val, prf_sprd,  prf_cnt, prf_lvls, &
                err_grad_scale_s, err_grad_min_s, err_grad_max_s)
           do z=1,prf_lvls
              obsout_cnt = obsout_cnt + 1
              obsout(obsout_cnt)%id   = obid_s
              obsout(obsout_cnt)%plat = platid
              obsout(obsout_cnt)%lat  = lat
              obsout(obsout_cnt)%lon  = lon
              obsout(obsout_cnt)%dpth = prf_dpth(z)
              obsout(obsout_cnt)%hr   = 0
              obsout(obsout_cnt)%val  = prf_val(z)
              obsout(obsout_cnt)%err  = merge(prf_sprd(z), err_s, err_s < 0)
              obsout(obsout_cnt)%qc   = 0
           end do           
           cnt_sal_thinned = cnt_sal_thinned + 1
        end if
        
     end do
  end do


  ! write out the observations
  ! ------------------------------------------------------------
  print *, "Writing observations out..."
  call obsio%write(outfile, obsout(1:obsout_cnt))
  
  
  ! print out statistics
  !------------------------------------------------------------
  print *, "Temperture profiles:"
  print *, "  count before thinning: ", cnt_tmp
  print *, "  count after  thinning: ", cnt_tmp_thinned
  print *, ""
  print *, "Salinity profiles:"
  print *, "  count before thinning: ", cnt_sal
  print *, "  count after  thinning: ", cnt_sal_thinned
  print *, ""
  print *, "Total observations:"
  print *, "  ", obsout_cnt
  


contains


  
  subroutine runningMeanVar(val, cnt, mean, m2)
    real,    intent(in)    :: val
    integer, intent(inout) :: cnt
    real,    intent(inout) :: mean, m2
    real :: r
    cnt = cnt + 1
    r = val - mean
    mean = mean + r/cnt
    m2 = m2 + r*(val-mean)
  end subroutine runningMeanVar



  subroutine avg_profiles(hzbin, lat, lon, density, prf_depth, prf_val, prf_sprd, prf_cnt, prf_lvls, &
       grad_scale, grad_min, grad_max)
    ! average together co-located profiles by
    ! 1) fitting each profile to a cubic spline
    ! 2) determining the vertical levels that the profiles have in common, thinned
    !   out to no more than 1 per model level
    ! 3) additional levels are added if the spacing between levels is too large
    !   and the vertical stratification is strong
    ! 4) each profile is sampled at the common levels
    ! 5) mean/variance of profiles is saved as observation value / spread

    type(linked_list), intent(in) :: hzbin
    real, intent(out) :: lat, lon
    real, intent(in) :: density(:)
    real, intent(out) :: prf_depth(grid_nz),  prf_val(grid_nz),  prf_sprd(grid_nz)
    real, intent(in) :: grad_scale, grad_min, grad_max
    integer, intent(out) :: prf_cnt(grid_nz)
    integer, intent(out) :: prf_lvls

    
    integer :: bin_len
    integer :: vtbin_cnt(grid_nz)
    real    :: vtbin_val(grid_nz)
    real    :: vtbin_m2(grid_nz)
    real    :: vtbin_dpth(grid_nz)
    real    :: vtbin_com_dpth(grid_nz)

    integer :: i, j, idx, l, z, z2, z3, m
    real ::  r
    real :: rz1(grid_nz), rz2(grid_nz)

    type(cspline) :: spline    

    integer :: stack(2, grid_nz)
    integer :: stack_cnt

    integer :: prev_l

    ! return if there are no profiles in this hz bin
    ! ... why was this function even called in the first place ??
    bin_len = hzbin%len()
    prf_lvls = 0
    if ( bin_len == 0 ) return


    ! get a list of the common provided depths, binned to model levels
    vtbin_cnt  = 0
    vtbin_com_dpth = 0
    do i = 1, bin_len
       call list_get_item(hzbin, i, idx)
       do l=prf_start(idx), prf_end(idx)
          call grid_d2z(ob_dpth(l), z)
          vtbin_cnt(z) = vtbin_cnt(z) + 1
          vtbin_com_dpth(z) = vtbin_com_dpth(z) + (ob_dpth(l) - vtbin_com_dpth(z))/vtbin_cnt(z)
       end do
    end do
    where(vtbin_cnt == 0) vtbin_com_dpth = -1


    ! get the segments with gaps in the levels and push them on the stack to be processed
    prev_l = -1
    stack_cnt = 0
    do i =1, grid_nz
       if( vtbin_com_dpth(i) > 0 ) then
          if (prev_l > 0 .and. i - prev_l > 1) then
             stack_cnt = stack_cnt + 1
             stack(1, stack_cnt) = prev_l
             stack(2, stack_cnt) = i
          end if
          prev_l = i
       end if
    end do


    ! add levels in the middle of gaps that are too large given the density gradient
    ! TODO, do this smarter so that the ob is added closer to the actual level where 
    ! density is in the middle?
    if(density_sigma > 0) then
       do while(stack_cnt > 0)
          i = stack(1,stack_cnt)
          j = stack(2,stack_cnt)
          m = (i+j)/2
          stack_cnt = stack_cnt - 1
          if(j-i <= 1) cycle
          if(density(j)-density(i) > density_sigma) then
             vtbin_com_dpth(m) = grid_depths(m)
             stack(1,stack_cnt+1) = i
             stack(2,stack_cnt+1) = m
             stack(1,stack_cnt+2) = m
             stack(2,stack_cnt+2) = j
             stack_cnt = stack_cnt + 2
          end if
       end do
    end if


    ! for each profile in the hz bin, add it to the vertical
    ! bins for running mean / variance calculations
    vtbin_cnt = 0
    vtbin_val = 0
    vtbin_m2 = 0
    vtbin_dpth = 0
    lat = 0
    lon = 0
    do i = 1, bin_len
       call list_get_item(hzbin, i, idx)
       lat = lat + (ob_lat(prf_start(idx))-lat)/i
       lon = lon + (ob_lon(prf_start(idx))-lon)/i

       ! for each vertical level in the given profile
       ! calculate the running mean/variance of the values
       ! at the various depths. Also keep track of the
       ! average depth of obs in each vertical bin
       ! do l=prf_start(idx), prf_end(idx)
       !    call grid_d2z(ob_dpth(l), z)
       !    vtbin_cnt(z) = vtbin_cnt(z) + 1
       !    r = ob_val(l) - vtbin_val(z)
       !    vtbin_val(z) = vtbin_val(z) + r/vtbin_cnt(z)
       !    vtbin_m2(z) = vtbin_m2(z) + r*(ob_val(l)-vtbin_val(z))
       !    r = ob_dpth(l) - vtbin_dpth(z)
       !    vtbin_dpth(z) = vtbin_dpth(z) + r/vtbin_cnt(z)
       ! end do

       ! if observation is only at a single level:
       if(prf_start(idx) == prf_end(idx)) then
          call grid_d2z(ob_dpth(prf_start(idx)), z)
          call runningMeanVar(ob_val(prf_start(idx)), vtbin_cnt(z), vtbin_val(z), vtbin_m2(z))
          vtbin_dpth(z) = vtbin_dpth(z) + (ob_dpth(prf_start(idx))-vtbin_dpth(z))/vtbin_cnt(z)
       else
      ! otherwise produce obs at the common levels from a spline
          spline = cspline(ob_dpth(prf_start(idx):prf_end(idx)), ob_val(prf_start(idx):prf_end(idx)))
          do l=1,grid_nz          
             if(vtbin_com_dpth(l) <= 0) cycle
             call grid_d2z(vtbin_com_dpth(l), z)

             ! get from the spline
             if(spline%x(1) <= vtbin_com_dpth(l) .and. spline%x(size(spline%x)) >= vtbin_com_dpth(l)) then
                call runningMeanVar(spline%interp(vtbin_com_dpth(l)),vtbin_cnt(z), vtbin_val(z), vtbin_m2(z))
                vtbin_dpth(z) = vtbin_dpth(z) + (vtbin_com_dpth(l) - vtbin_dpth(z))/vtbin_cnt(z)
             else 
                ! or, see if we get from the end of the spline
                call grid_d2z(spline%x(1), z2)
                call grid_d2z(spline%x(size(spline%x)), z3)
                if(z == z2) then
                   call runningMeanVar(spline%y(1), vtbin_cnt(z), vtbin_val(z), vtbin_m2(z))
                   vtbin_dpth(z) = vtbin_dpth(z) + (spline%x(1) - vtbin_dpth(z))/vtbin_cnt(z)
                else if(z == z3) then
                   call runningMeanVar(spline%y(size(spline%x)), vtbin_cnt(z), vtbin_val(z), vtbin_m2(z))
                   vtbin_dpth(z) = vtbin_dpth(z) + (spline%x(size(spline%x)) - vtbin_dpth(z))/vtbin_cnt(z)
                end if
             end if
          end do
       end if
    end do
      

    ! generate a spline from the final profile
    ! ------------------------------------------------------------
    j=0
    do z=1,grid_nz
       if(vtbin_cnt(z) > 0) then
          j = j +1
          rz1(j) = vtbin_dpth(z)
          rz2(j) = vtbin_val(z)          
       end if
    end do
    if(size(rz1(1:j)) == 0) then
       print *, "ERROR: final profile spline has no points!"
       stop 1
    end if

    if (j == 1) then
    ! There is only a single point, so don't bother creating a cubic spline
       prf_lvls = 1
       prf_depth(1) = rz1(1)
       prf_val(1)   = rz2(1)
       prf_sprd(1)  = (grad_min+grad_max)/2.0
       prf_cnt(1)   = 1
    else
    ! create final profile from a cubic spline
       spline = cspline(rz1(1:j), rz2(1:j))
       do z=1,grid_nz
          if(vtbin_cnt(z) > 0) then
             prf_lvls = prf_lvls + 1
             err = max(grad_min,min(grad_max,abs(spline%deriv(vtbin_dpth(z)))*grad_scale))
             if(vtbin_cnt(z) > 1 .and. vtbin_m2(z) /=0) then
                ! get from co-located spread
                err = 0.5*err + 0.5*sqrt(vtbin_m2(z)/(vtbin_cnt(z)-1))
             end if
             prf_depth(prf_lvls) = vtbin_dpth(z)
             prf_val(prf_lvls)   = vtbin_val(z)
             prf_sprd(prf_lvls)  = err
             prf_cnt(prf_lvls)   = vtbin_cnt(z)
          end if
       end do
    end if
  end subroutine avg_profiles

  
  
  subroutine check(status)
    integer, intent(in) :: status
    if(status /= nf90_noerr) then
       print *, trim(nf90_strerror(status))
       stop 1
    end if
  end subroutine check
    
end program obs_prep_insitu
