program obsprep_nc2dat
  use obscom_obsio
  use datetime_module
  
  implicit none

  integer :: i, j
  logical :: ex
  character(len=1024) :: file_in, file_out
  character(len=1024) :: tmp_str
  real :: r


  type(obsio_nc) :: obsio
  type(observation), dimension(:), allocatable :: obs_in
  real,              dimension(:), allocatable :: inc_in
  type(datetime) :: dt

  real(4) :: wk(9)
  integer :: obscnv1(100), obscnv2(100)
  integer :: obscnv_cnt = 0
  

  ! read in the command line arguments
  i = command_argument_count()
  if (i < 2 .or. i > 3) then
     print *, "ERROR, command line arguments"
  end if
  call get_command_argument(1, value=file_in)
  call get_command_argument(2, value=file_out)
  if (i == 3) then
     call get_command_argument(3, value=tmp_str)
     do while(len(trim(tmp_str)) > 0)
        i = scan(tmp_str, ",")
        if (i == 0) i=len(trim(tmp_str))+1
        j = scan(tmp_str(1:i-1), ":")
        obscnv_cnt = obscnv_cnt+1
        read(tmp_str(1:j-1),   *) obscnv1(obscnv_cnt)
        read(tmp_str(j+1:i-1), *) obscnv2(obscnv_cnt)
        tmp_str=tmp_str(i+1:len(trim(tmp_str)))
     end do
  end if

  inquire(file=file_out, exist=ex)
  if (ex) then
     print *, "ERROR: output file already exists"
     stop 1
  end if

  print *, "reading: ",  trim(file_in)
  call obsio%read(file_in, obs_in, dt, inc_in)

  ! convert the obsids 
  do i=1,size(obs_in)
     r=-1
     do j=1,obscnv_cnt
        if (obs_in(i)%id == obscnv1(j)) then
           obs_in(i)%id = obscnv2(j)
           r = 1
           exit
        end if
     end do
  end do
  print *, "obs in: ", size(obs_in)

  ! write out
  open(91, file=file_out, form='unformatted', access='sequential')
  do i=1, size(obs_in)
     wk(1)=obs_in(i)%id
     wk(2)=obs_in(i)%lon
     wk(3)=obs_in(i)%lat
     wk(4)=obs_in(i)%dpth
     wk(5)=obs_in(i)%val
     wk(6)=obs_in(i)%err
     wk(7)=inc_in(i)+obs_in(i)%val
     wk(8)=merge(1,0,obs_in(i)%qc==0)
     wk(9)=obs_in(i)%hr
     write(91) wk
  end do
  close(91)
!  r = 0
!  select (
!  wk(1) = 
end program obsprep_nc2dat
