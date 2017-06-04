module obsprep_sst_pathfinder
  use netcdf
  implicit none
  private

  public :: read_avhrr_pathfinder_nc

  type, public :: avhrr_data
     real    :: lon
     real    :: lat
     real    :: val
     real    :: oerr
     real    :: hour
     integer :: qkey
     integer :: typ
     logical :: kept
  end type avhrr_data



contains



  subroutine read_avhrr_pathfinder_nc(infile, min_qual, obs)
    character(len=*), intent(in) :: infile
    integer,          intent(in) :: min_qual
    !! 5 = native pathfinder quality 7   = best_quality
    !! 4 = native pathfinder quality 4-6 = acceptable_quality
    !! 3 = native pathfinder quality 2-3 = low_quality
    !! 2 = native pathfinder quality 1   = worst_quality
    !! 1 = native pathfinder quality 0   = bad_data
    !! 0 = native pathfinder quality -1  = missing_data
    type(avhrr_data), intent(out), allocatable :: obs(:)

    integer :: x , y

    integer :: ncid, d_lon, d_lat, vid
    integer :: nx, ny

    ! variables read in from file
    real, allocatable :: lons(:), lats(:)
    integer :: time
    integer :: ob_cnt
    real,    allocatable :: ob_val(:,:)
    integer, allocatable :: ob_qual(:,:)
    integer, allocatable :: ob_dtime(:,:)

    real :: t_offset, t_scale

!    print *, ""
!    print *, "Pathfinder AVHRR SST"

    ! read lat / lon grid
    call check(nf90_open(infile, nf90_nowrite, ncid))
    call check(nf90_inq_dimid(ncid, 'lon', d_lon))
    call check(nf90_inquire_dimension(ncid, d_lon, len=nx))
    call check(nf90_inq_dimid(ncid, 'lat', d_lat))
    call check(nf90_inquire_dimension(ncid, d_lat, len=ny))
    allocate(lons(nx))
    call check(nf90_inq_varid(ncid, 'lon', vid))
    call check(nf90_get_var(ncid, vid, lons))
    allocate(lats(ny))
    call check(nf90_inq_varid(ncid, 'lat', vid))
    call check(nf90_get_var(ncid, vid, lats))

    ! read base time and offset
    call check(nf90_inq_varid(ncid, 'time', vid))
    call check(nf90_get_var(ncid, vid, time))
    allocate(ob_dtime(nx,ny))
    call check(nf90_inq_varid(ncid, 'sst_dtime', vid))
    call check(nf90_get_var(ncid, vid, ob_dtime))

    ! read quality flags
    allocate(ob_qual(nx,ny))
    call check(nf90_inq_varid(ncid, 'quality_level', vid))
    call check(nf90_get_var(ncid, vid, ob_qual))
    ob_cnt = count(ob_qual >= min_qual)

    ! read observed SST
    call check(nf90_inq_varid(ncid, 'sea_surface_temperature', vid))
    call check(nf90_get_att(ncid, vid, "add_offset", t_offset))
    call check(nf90_get_att(ncid, vid, "scale_factor", t_scale))
!    print *,'observed temp = "sea_surface_temperature" * ',t_scale,"+",t_offset
    allocate(ob_val(nx,ny))
    call check(nf90_get_var(ncid, vid, ob_val))
    ob_val = ob_val*t_scale + t_offset

    ! all done, put into list
    allocate(obs(ob_cnt))
    ob_cnt = 0
    do x = 1, nx
       do y = 1, ny
          if(ob_qual(x,y) < min_qual) cycle
          ob_cnt = ob_cnt + 1
          obs(ob_cnt)%lon = lons(x)
          obs(ob_cnt)%lat = lats(y)
          obs(ob_cnt)%val = ob_val(x,y)
          
!                real    :: lon
!      real    :: lat
!      real    :: value
!      real    :: oerr
!      real    :: hour
!      integer :: qkey
!      integer :: typ
!      logical :: kept

       end do
    end do

    ! cleanup
    
  end subroutine read_avhrr_pathfinder_nc


  subroutine check(status)
    integer, intent(in) :: status
    if(status /= nf90_noerr) then
       print *, trim(nf90_strerror(status))
       stop 1
    end if
  end subroutine check

end module obsprep_sst_pathfinder
