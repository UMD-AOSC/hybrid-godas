!! author: Travis Sluka
!! category: godas-3dvar
!!

module datatable
  !!

  use netcdf

  implicit none
  private

  ! public module methods
  !------------------------------------------------------------
  public :: datatable_init
  public :: datatable_get

  interface datatable_get
     procedure datatable_get1d
     procedure datatable_get2d
     procedure datatable_get3d
  end interface datatable_get


  ! custom types
  !------------------------------------------------------------
  type datatable_entry
     character(len=10)   :: key
     character(len=20)   :: file_var
     character(len=1024) :: file_name
  end type datatable_entry


  ! private module variables
  !------------------------------------------------------------
  integer, parameter    :: max_vars = 100
  integer               :: num_vars
  type(datatable_entry) :: datatable_entries(max_vars)
  logical               :: initialized = .false.
  logical :: isroot




contains



  !================================================================================
  !================================================================================



  subroutine datatable_get1d(key, val)
    character(len=*), intent(in) :: key
    real, intent(inout) :: val(:)
    type(datatable_entry) :: v
    integer :: ncid, vid

    call datatable_getRec(key, v)

    if (isroot)   print *," ",trim(v%key),': reading "',trim(v%file_var),'" from "', trim(v%file_name)

    call check(nf90_open(v%file_name, NF90_NOWRITE, ncid))
    call check(nf90_inq_varid(ncid, trim(v%file_var), vid))
    call check(nf90_get_var(ncid, vid, val))
    call check(nf90_close(ncid))

    if (isroot)   print *,"  range:", minval(val), maxval(val)
  end subroutine datatable_get1d



  !================================================================================
  !================================================================================



  subroutine datatable_get2d(key, val)
    character(len=*), intent(in) :: key
    real, intent(inout) :: val(:,:)
    type(datatable_entry) :: v
    integer :: ncid, vid

    call datatable_getRec(key, v)

    if (isroot)   print *," ",trim(v%key),': reading "',trim(v%file_var),'" from "', trim(v%file_name)

    call check(nf90_open(v%file_name, NF90_NOWRITE, ncid))
    call check(nf90_inq_varid(ncid, trim(v%file_var), vid))
    call check(nf90_get_var(ncid, vid, val))
    call check(nf90_close(ncid))

    if (isroot)   print *,"  range:", minval(val), maxval(val)    
  end subroutine datatable_get2d



  !================================================================================
  !================================================================================



  subroutine datatable_get3d(key, val)
    character(len=*), intent(in) :: key
    real, intent(inout) :: val(:,:,:)
    type(datatable_entry) :: v
    integer :: ncid, vid

    call datatable_getRec(key, v)

    if (isroot)   print *," ",trim(v%key),': reading "',trim(v%file_var),'" from "', trim(v%file_name)

    call check(nf90_open(v%file_name, NF90_NOWRITE, ncid))
    call check(nf90_inq_varid(ncid, trim(v%file_var), vid))
    call check(nf90_get_var(ncid, vid, val))
    call check(nf90_close(ncid))

    if (isroot)   print *,"  range:", minval(val), maxval(val)    
  end subroutine datatable_get3d



  !================================================================================
  !================================================================================



  subroutine datatable_getRec(key, val)
    character(len=*), intent(in) :: key
    type(datatable_entry), intent(out) :: val
    integer :: i, j

    if (.not. initialized) then
       print *, "ERROR: datatable_get called before datatable_init"
       stop 1
    end if
    j = 0
    do i=1,num_vars
       if (trim(key) == trim(datatable_entries(i)%key)) then
          j = i
          exit
       end if
    end do

    if (j == 0) then
       print *, 'ERROR: "',trim(key),'" not found in data_table'
       stop 1
    end if

    val = datatable_entries(j)
  end subroutine datatable_getRec



  !================================================================================
  !================================================================================



  subroutine datatable_init(root, filename)
    logical, intent(in) :: root
    character(len=*), intent(in) :: filename
    type(datatable_entry) :: newEntry
    integer :: unit, iostat, i
    logical :: ex
    character(len=1024) :: line

    isroot = root

    num_vars = 0

    if(isroot) then
       print *,new_line('a'),&
            new_line('a'), "------------------------------------------------------------",&
            new_line('a'), "datatable_init() : ", &
            new_line('a'), "------------------------------------------------------------"
       print *, 'Reading datatable file "',trim(filename),'"'
    end if

    ! ensure that the data file exists
    inquire(file=filename, exist=ex)
    if (.not. ex) then
       print *, "ERROR: Unable to open data table file"
       stop 1
    end if

    ! start reading in lines
    open(newunit=unit, file=filename, action='read')
    do while(.true.)
       ! read a new line
       read(unit, '(A)', iostat=iostat) line
       if (iostat < 0) exit
       if (iostat > 0) then
          print *,"ERROR: problem reading the data table file",&
               ' error code: ',iostat
          stop 1
       end if

       ! convert tabs to spaces
       do i = 1, len(line)
          if (line(i:i) == char(9)) line(i:i) = ' '
       end do

       ! ignore comments
       line = adjustl(line)
       if (line(1:1) == '#') cycle

       ! ignore empty lines
       if (len(trim(adjustl(line))) == 0) cycle

       ! read the line
       read(line, *, iostat=iostat)  newEntry
       if (iostat > 0) then
          print *,"ERROR: problem reading the data table file",&
               ' line: ',line
          stop 1
       end if

       num_vars = num_vars +1
       datatable_entries(num_vars) = newEntry
    end do

    if(isroot) print *, num_vars, "fields found"

    close(unit)
    initialized = .true.
  end subroutine datatable_init



  !================================================================================
  !================================================================================



  subroutine check(status)
    !! helper function to wrap calls to netcdf
    integer, intent(in) :: status
    if(status /= nf90_noerr) then
       print *, trim(nf90_strerror(status))
       stop 1
    end if
  end subroutine check



  !================================================================================

end module datatable
