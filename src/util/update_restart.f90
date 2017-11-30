program update_restart
  use netcdf
  use mpi

  implicit none

  ! variables (to be eventually) read in from namelist
  integer :: rst_layout(2) = (/6,6/)
  integer :: ai_layout(2)  = (/1,1/)
  character(len=1024) :: rst_filename      != "RESTART/MOM.res.nc"
  character(len=1024) :: ai_filename       != "ana_inc.nc"
  real :: salt_bounds(2) = (/0.0, 50.0/)
  real :: temp_bounds(2) = (/-3.0, 50.0/)


  ! other vars
  integer :: nxyz(3)
  real, allocatable :: ai_s(:,:,:)
  real, allocatable :: ai_t(:,:,:)

  integer :: nxyz_xfer(3)
  real, allocatable :: xfer(:,:,:)
  real, allocatable :: rst(:,:,:)

  integer, allocatable :: domains(:,:)
  integer, allocatable :: requests(:)
  
  integer :: domain(4)
  integer :: i4(4)
  integer :: status(MPI_STATUS_SIZE)
  logical :: ex
  character(len=1024) :: str
  integer :: ncid,dimid, vid
  integer :: ierr, i, j
  integer :: rst_files, ai_files
  integer :: mp_comm, mp_rank, mp_size
  logical :: isroot
  integer, allocatable :: pa_rst(:) ! processor assignment for restart file I/O

  ! ------------------------------------------------------------

  call get_command_argument(1, value=ai_filename)
  call get_command_argument(2, value=rst_filename)

  ! initialize MPI
  call mpi_init(ierr)
  mp_comm = mpi_comm_world
  call mpi_comm_size(mp_comm, mp_size, ierr)
  call mpi_comm_rank(mp_comm, mp_rank, ierr)
  isroot = mp_rank == 0
  if (isroot) print *, "MPI initialized with size ",mp_size
  
  ! read in the namelist
  ! TODO
  rst_files = rst_layout(1) * rst_layout(2)
  ai_files = ai_layout(1) * ai_layout(2)


  ! determine the list of which restart files each proc is responsible for
  ! FOr now, root proc reads in AI, while other procs edit rst files
  allocate(pa_rst(rst_files))
  do i=1,rst_files
     pa_rst(i)=mod(i-1 , mp_size-1)+1
  end do


  
  ! determine the list of which input file patches each proc is responsible for
  ! TODO: implement this


  ! load in the analysis increments
  !------------------------------------------------------------
  ! TODO: use patches for AI file
  if (isroot) then     
     call check(nf90_open(ai_filename, nf90_nowrite, ncid))
     call check(nf90_inq_dimid(ncid, "grid_x", dimid))
     call check(nf90_inquire_dimension(ncid, dimid, len=nxyz(1)))
     call check(nf90_inq_dimid(ncid, "grid_y", dimid))
     call check(nf90_inquire_dimension(ncid, dimid, len=nxyz(2)))
     call check(nf90_inq_dimid(ncid, "grid_z", dimid))
     call check(nf90_inquire_dimension(ncid, dimid, len=nxyz(3)))

     print *, "Reading analysis increments...."
     print *, "Grid size", nxyz

     allocate(ai_t(nxyz(1), nxyz(2),nxyz(3)))
     allocate(ai_s(nxyz(1), nxyz(2),nxyz(3)))
     call check(nf90_inq_varid(ncid, 'ai_temp', vid))
     call check(nf90_get_var(ncid, vid, ai_t))
     call check(nf90_inq_varid(ncid, 'ai_salt', vid))
     call check(nf90_get_var(ncid, vid, ai_s))
     call check(nf90_close(ncid))
  end if
  
  ! determine the size of the grid needed to transfer the AI patches to the worker procs
  if (isroot) then
     if( mod(nxyz(1), rst_layout(1)) > 0) then
        print *, "ERROR: grid not evenly divisible in x direction"        
        stop 1
     end if
     if( mod(nxyz(2), rst_layout(2)) > 0) then
        print *, "ERROR: grid not evenly divisible in y direction"        
        stop 1
     end if
     
     nxyz_xfer(1) = nxyz(1) / rst_layout(1)
     nxyz_xfer(2) = nxyz(2) / rst_layout(2)
     nxyz_xfer(3) = nxyz(3)
     print *, "MPI transfer grid size", nxyz_xfer
  end if
  call mpi_bcast(nxyz_xfer, 3, mpi_int, 0, mp_comm, ierr)
  allocate(xfer(nxyz_xfer(1), nxyz_xfer(2), nxyz_xfer(3)))


  ! worker procs
  !------------------------------------------------------------
  if(.not. isroot) then
     allocate(rst(nxyz_xfer(1), nxyz_xfer(2), nxyz_xfer(3)))

     do i=1, rst_files
        if (mp_rank /= pa_rst(i)) cycle

        ! see if the file exists, if not, cycle
        write(str,'(A,A,I0.4)') trim(rst_filename), ".", i-1
        inquire(file=str, exist=ex)
        if(.not. ex) cycle

        ! open the file, get the grid decomposition parameters,
        ! tell the root proc which patch of the AI we need
        call check(nf90_open(str, nf90_write, ncid))
        !TODO, need to use lonq eventually for U/V
        call check(nf90_inq_varid(ncid, 'lonh', vid))
        call check(nf90_get_att(ncid, vid, "domain_decomposition", i4))
        domain(1) = i4(3)
        domain(2) = i4(4)
        call check(nf90_inq_varid(ncid, 'lath', vid))
        call check(nf90_get_att(ncid, vid, "domain_decomposition", i4))
        domain(3) = i4(3)
        domain(4) = i4(4)                   
        call mpi_send(domain, 4, mpi_int, 0, 0, mp_comm, ierr)

        ! apply temperature AI
        call check(nf90_inq_varid(ncid, "Temp", vid))
        call check(nf90_get_var(ncid, vid, rst))
        call mpi_recv(xfer, size(xfer), mpi_real, 0, 1, mp_comm, status, ierr)
        rst = rst + xfer
        where (rst < temp_bounds(1)) rst = temp_bounds(1)
        where (rst > temp_bounds(2)) rst = temp_bounds(2)
        call check(nf90_put_var(ncid, vid, rst))

        ! apply salinity AI
        call check(nf90_inq_varid(ncid, "Salt", vid))
        call check(nf90_get_var(ncid, vid, rst))                
        call mpi_recv(xfer, size(xfer), mpi_real, 0, 2, mp_comm, status, ierr)
        rst = rst + xfer
        where (rst < salt_bounds(1)) rst = salt_bounds(1)
        where (rst > salt_bounds(2)) rst = salt_bounds(2)
        call check(nf90_put_var(ncid, vid, rst))

        ! all done with this file
        call check(nf90_close(ncid))
     end do

     ! tell the root proc that this worker proc is done
     domain(1) = -1
     call mpi_send(domain, 4, mpi_int, 0, 0, mp_comm, ierr)
  end if


  
  ! root proc
  !------------------------------------------------------------
  ! TODO: when using waitany is there a chance of starving some workers?
  if(isroot) then
     allocate(domains(4,mp_size-1))
     allocate(requests(mp_size-1))

     ! start listening from each worker proc
     do i=1,mp_size-1
        call mpi_irecv(domains(:,i), 4, mpi_int, i, 0, mp_comm, requests(i), ierr)        
     end do

     do while(any(requests /= mpi_request_null))
        ! wait for a request        
        call mpi_waitany(size(requests), requests, i, status, ierr)
        if(domains(1,i) == -1) then
           ! this proc has finished, take note
           requests(i) = mpi_request_null
           cycle
        end if

        ! send the proc the AI it needs
        domain = domains(:,i)
!        print *, i, "is requesting", domains(:,i)
        
        call mpi_send(ai_t(domain(1):domain(2),domain(3):domain(4),:), size(xfer), mpi_real, i, 1, mp_comm, ierr)
        call mpi_send(ai_s(domain(1):domain(2),domain(3):domain(4),:), size(xfer), mpi_real, i, 2, mp_comm, ierr)

        ! start listening again for a message from this proc
        call mpi_irecv(domains(:,i), 4, mpi_int, i, 0, mp_comm, requests(i), ierr)        
     end do

!     print *, "Root is done listening"
     print *, "DONE"
  end if

   
  call mpi_finalize(mp_comm)



  
contains

  

  subroutine check(status)
    integer, intent(in) :: status
    if(status /= nf90_noerr) then
       print *, trim(nf90_strerror(status))
       stop 1
    end if
  end subroutine check



end program update_restart
