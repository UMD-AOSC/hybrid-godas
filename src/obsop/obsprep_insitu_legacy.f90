program obsprep_insitu_legacy
  use obscom_obsio
  use obscom_grid
  use cubic_spline
  use compute_profile_error, only : cmpTz

  use netcdf
  implicit none

  integer, parameter :: inlvls = 40
  real, allocatable :: bg_density(:,:,:)

  type(obsio_nc) :: obsio
  integer :: unit
  integer :: ncid, vid
  integer :: i, j, p1, p2, k
  character(len=3) :: var

  real :: se(2)
  real :: ob_err(inlvls)
  type(cspline) :: spline_ob
  type(cspline) :: spline_err

  type(observation)  :: ob
  integer, parameter :: max_obs_out = 500000
  type(observation)  :: obs(max_obs_out)
  integer            :: num_obs = 0
  
  ! read in profiles
  integer           :: nprof
  real              :: ob_z(inlvls)
  real, allocatable :: ob_val(:,:)
  real, allocatable :: ob_lon(:)
  real, allocatable :: ob_lat(:)
  real              :: ob_missing_value
  
  ! parameters read in from namelist
  character(len=:), allocatable :: obsfile
  character(len=:), allocatable :: outfile
  character(len=:), allocatable :: densityfile
  character(len=:), allocatable :: densityvar
  real :: se_t(2) = (/1.0, 1.5/)
  real :: se_s(2) = (/0.05, 0.15/)
  integer :: obid_t = 2210
  integer :: obid_s = 2220
  real ::  density_sigma = -1

  namelist /obsprep_insitu_legacy_nml/ obsfile, outfile, densityfile, densityvar, &
       density_sigma, se_t, se_s, obid_t, obid_s


  print *, "------------------------------------------------------------"
  print *, "  insitu observation preparation - legacy GODAS profiles"
  print *, "------------------------------------------------------------"

  call grid_init("obsprep.nml")

  ! read the namelist
  allocate(character(len=1024) :: obsfile)
  allocate(character(len=1024) :: outfile)
  allocate(character(len=1024) :: densityfile)
  allocate(character(len=1024) :: densityvar)
  open(newunit=unit, file="obsprep.nml")
  read(unit, obsprep_insitu_legacy_nml)
  obsfile=trim(obsfile)
  outfile=trim(outfile)
  densityfile=trim(densityfile)
  densityvar=trim(densityvar)
  print *, ""
  print obsprep_insitu_legacy_nml
  print *, ""
  
  ! read the background density
  if (density_sigma > 0) then
     print *, ""
     print *, "Reading background density..."
     print *, ""
     allocate(bg_density(grid_nx, grid_ny, grid_nz))
     call check(nf90_open(densityfile, nf90_nowrite, ncid))     
     call check(nf90_inq_varid(ncid, densityvar, vid))
     call check(nf90_get_var(ncid, vid, bg_density))
     call check(nf90_close(ncid))
  end if

  !  For each of TMP and SALT obs types...
  !------------------------------------------------------------
  do i=1,2
     var = merge("tmp","sal",i==1)

     ! read in observations
     print *, ""
     print *, "Reading ",var," observations..."
     
     call check(nf90_open(trim(obsfile)//var//".nc", nf90_nowrite, ncid))
     call check(nf90_inq_dimid(ncid, 'count', vid))
     call check(nf90_inquire_dimension(ncid, vid, len=nprof))
     print *, nprof, ' profiles'

     call check(nf90_inq_varid(ncid, 'grid_z', vid))
     call check(nf90_get_var(ncid, vid, ob_z))

     allocate(ob_lon(nprof))
     call check(nf90_inq_varid(ncid, 'xlon', vid))
     call check(nf90_get_var(ncid, vid, ob_lon))

     allocate(ob_lat(nprof))
     call check(nf90_inq_varid(ncid, 'ylat', vid))
     call check(nf90_get_var(ncid, vid, ob_lat))

     allocate(ob_val(inlvls, nprof))
     call check(nf90_inq_varid(ncid, merge("temp","salt",i==1), vid))
     call check(nf90_get_var(ncid, vid, ob_val))
     call check(nf90_get_att(ncid, vid, "missing_value", ob_missing_value))
     call check(nf90_close(ncid))

     ! for each profile...
     do j = 1, nprof
        ! get the hi/low of the desired error values
        se = merge(se_t, se_s, i == 1) 

        ! calculate the error profile
        call cmpTz(ob_err, se(1), se(2), ob_val, ob_z, inlvls, ob_missing_value)

        ! find the top and bottom of the profile
        ! (this assumes there are no gaps in the profile otherwise)
        p1 = 1
        do while (ob_val(p1,j) == ob_missing_value .and. p1 <= inlvls)
           p1 = p1 + 1
        end do
        p2 = inlvls
        do while (ob_val(p2,j) == ob_missing_value .and. p2 >= 1)
           p2 = p2 - 1
        end do
        if (p1 > inlvls .or. p2 < 1) cycle

        ! TODO, add interpolated observations in areas of high background vertical stability
!        ! build a cubic spline from this profile       
!        spline_ob  = cspline(ob_z(p1:p2), ob_val(p1:p2,j))
!        spline_err = cspline(ob_z(p1:p2), ob_err(p1:p2))

        ! add observation to final list of observations
        do k=p1,p2
           if(ob_err(k) == 999) cycle ! problem with bottom of some profiles
           ob%id   = merge(obid_t, obid_s, i == 1)
           ob%plat = 1
           ob%lat  = ob_lat(j)
           ob%lon  = ob_lon(j)
           ob%dpth = ob_z(k)
           ob%hr   = 0
           ob%val  = ob_val(k,j)
           ob%err  = ob_err(k)
           ob%qc   = 0
           num_obs = num_obs + 1
           obs(num_obs) = ob
        end do
     end do

     ! done, cleanup
     deallocate(ob_lon)
     deallocate(ob_lat)
     deallocate(ob_val)
  end do

  ! all done, write out final observations  
  print *, "writing observatiosn to ",outfile
  call obsio%write(outfile, obs(1:num_obs))
  print *, num_obs, "total observations written"




contains



  subroutine check(status)
    integer, intent(in) :: status
    if(status /= nf90_noerr) then
       print *, trim(nf90_strerror(status))
       stop 1
    end if
  end subroutine check


end program obsprep_insitu_legacy
