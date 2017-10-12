program obsprep_comine
  use obscom_obsio
  use obscom_grid
  use datetime_module

#ifdef __INTEL_COMPILER
  use ifport
#endif
  
  implicit none

  !variables red in from namelist
  integer :: collate = 0     ! 0=none, 1=thin, 2=avg
  real :: thinning(2) = (/0.0,0.0/)
  real :: thinning_eq = 10.0
  integer :: basedate(6) = 0

  ! variables read in from command line
  character(len=1024) :: outfile

  ! other variables
  integer :: unit
  integer ::  i, j, k, x,y
  character(len=1024) :: nml_file
  character(len=1024) :: filename
  character(len=1024) :: tmp_str, tmp_str2
  logical :: ex
  type(obsio_nc) :: obsio

  type(timedelta) :: td
  type(datetime) :: dt
  type(datetime) :: basedate_t
  integer, parameter :: maxfiles = 100
  integer :: numfiles
  type obsarr_ptr
     type(observation),dimension(:), allocatable :: p
  end type obsarr_ptr
  type(obsarr_ptr) :: obs_in(maxfiles)
  type incarr_ptr
     real, dimension(:), allocatable :: p
  end type incarr_ptr
  type(incarr_ptr) :: inc_in(maxfiles)
  type(observation), allocatable :: obs(:)
  real, allocatable :: inc(:)
  integer :: obs_cnt


  integer, allocatable :: bin_file(:,:)
  integer, allocatable :: bin_idx(:,:)
  

  namelist /obsprep_combine_nml/ collate, thinning, thinning_eq, basedate
  print *, "------------------------------------------------------------"
  print *, " obsprep_combine (Combine multiple obs files, optionally thinning"
  print *, "  coincident obs to only keep those closest to analysis time.)"
  print *, "------------------------------------------------------------"

  nml_file = "obsprep.nml"

  ! read in the namelist
  open(newunit=unit, file=nml_file)
  read(unit, obsprep_combine_nml)
  close(unit)
  print *,""
  print  obsprep_combine_nml
  if (collate /=0 .and. collate /= 1) then
     print *, "ERROR: collate values of 0,1 are only supported now (no averaging)"
     stop 1
  end if


  ! read in the command line arguments
  ! read in optional arguments
  i = command_argument_count()
  j = 1
  do while (j < i)
    call get_command_argument(j, value=tmp_str)
    if(tmp_str(1:1) == "-") then
       if ( j >= i) then
          print *, "ERROR with command line argument"
          stop 1
       end if
       call get_command_argument(j+1, value=tmp_str2)
       if (trim(tmp_str) == '-basedate') then
          read (tmp_str2, *) basedate
       else if(trim(tmp_str) == '-collate') then
          read (tmp_str2, *) collate
       else
          print *, "ERROR: illegal argument: ", trim(tmp_str)
          stop 1
       end if
       j = j + 2
    else
       exit
    end if
  end do
  numfiles = i-j

  basedate_t=datetime(basedate(1),basedate(2),basedate(3),basedate(4),basedate(5),basedate(6))
  print *, "basedate: ",basedate_t%isoformat()

  ! read in the grid
  if (collate > 0)  call grid_init(nml_file)

  ! read in required arguments
  call get_command_argument(i, value=outfile)
  print *, "Out file: ", trim(outfile)
  inquire(file=outfile, exist=ex)
  if (ex) then
     print *, "ERROR: output file already exists"
     stop 1
  end if
  if (numfiles <= 0) then
     print *, 'ERROR: cll with "obsprep_combine <outputfile> <inputfile1> <inputfile2>.... <inputfileN>"'
     stop 1
  end if


  ! read in the observations from each file
  print *, ""
  print *, "Reading in observation files...", numfiles
  do i=1,numfiles
     call get_command_argument(i+j-1, value=filename)
     print *, "reading: ",trim(filename)
     call obsio%read(filename, obs_in(i)%p, dt, inc_in(i)%p)
     td = dt - basedate_t     
     do k=1,size(obs_in(i)%p)
        obs_in(i)%p(k)%hr = obs_in(i)%p(k)%hr + td%total_seconds()/3600.0
     end do
  end do
  obs_cnt = 0
  do i=1,numfiles
     obs_cnt = obs_cnt + size(obs_in(i)%p)
  end do
  allocate(obs(obs_cnt))
  if(allocated(inc_in(1)%p)) then
     allocate(inc(obs_cnt))
  end if
  print *, "Total observations read: ", obs_cnt
  print *,""
  
  !combine obs
  if (collate == 0) then
     print *, "Doing simple file mergin..."
     ! just merging all the files together
     obs_cnt = 0
     do i=1,numfiles        
        k = size(obs_in(i)%p)
        if (k == 0) cycle
        obs(obs_cnt+1:obs_cnt+k) = obs_in(i)%p(:)
        if(allocated(inc)) then
           inc(obs_cnt+1:obs_cnt+k) = inc_in(i)%p(:)
        end if
        obs_cnt = obs_cnt + k        
     end do

  else if(collate == 1) then
     print *, "Collating (non-averaging)..."
     ! for obs on the same gridpoint only keep time closest to the analysis time
     if (allocated(inc)) then
        print *, "ERROR: inc not supported for collate=1"
        stop 1
     end if

     allocate(bin_file(grid_nx, grid_ny))
     allocate(bin_idx(grid_nx, grid_ny))
     bin_file=0
     bin_idx=0
     do i=1,numfiles
        do j=1,size(obs_in(i)%p)
           ! TODO, do actual time comparisions to keep the ob closest to the basedate
           call grid_ll2xy(obs_in(i)%p(j)%lat, obs_in(i)%p(j)%lon, x, y)
           bin_file(x,y) = i
           bin_idx(x,y) = j
        end do
     end do
     obs_cnt=0
     do y=1,grid_ny
        do x=1,grid_nx
           if (bin_file(x,y) > 0) then
              obs_cnt = obs_cnt + 1
              obs(obs_cnt) = obs_in(bin_file(x,y))%p(bin_idx(x,y))
           end if
        end do
     end do     
     print *, "obs after collating: ",obs_cnt
  else
     print *, "ERROR: collate value of",collate,"not handled"
     stop 1
  end if


  ! additional thinning
  if(sum(thinning) > 0.0) then
     print *, ""
     print *, "Latitude based thinning..."
     j=obs_cnt
     obs_cnt=0
     do i=1,j
        if(thin(obs(i)%lat)) cycle
        obs_cnt = obs_cnt + 1
        obs(obs_cnt) = obs(i)
        if(allocated(inc)) then
           inc(obs_cnt) = inc(i)
        end if
       
     end do
  end if

  print *, "Final observations: ",obs_cnt

  if (allocated(inc)) then
     call obsio%write(outfile, obs(1:obs_cnt), basedate_t, inc(1:obs_cnt))
  else
     call obsio%write(outfile, obs(1:obs_cnt), basedate_t)
  end if

contains


  function thin(lat) result(v)
    real, intent(in) :: lat
    logical :: v
    real :: cutoff
    cutoff = (thinning(2)-thinning(1))*exp(-0.5 * lat**2 / (thinning_eq**2)) + thinning(1)
    v = rand() <= cutoff
  end function thin


end program obsprep_comine
