program rst_update
  use netcdf
  implicit none 

  CHARACTER(:), ALLOCATABLE :: update_vars

  CHARACTER(len=1024) :: da_mode, str, str2, varname
  CHARACTER(len=1024) :: filename_var, filename_ekf, filename_output, filename_input
  INTEGER :: i,j, l, vid, ierr, unit
  real :: r

  INTEGER :: ncid_i, ncid_o, ncid_e, ncid_v
  INTEGER :: nDim, nVar, nAtt
  INTEGER :: varid, dimid, vType
  INTEGER :: dimids(6)
  INTEGER :: dimLen(6)
  
  INTEGER :: decomp_x(4), decomp_y(4)
  REAL(8), ALLOCATABLE :: data(:,:,:,:)
  REAL(8), ALLOCATABLE :: data2(:,:,:,:)

  REAL(8), allocatable :: data1d(:)
  REAL(8), ALLOCATABLE :: alpha_val(:,:,:,:)


  ! parameters read in from namelist
  real :: alpha=1.0
  real :: alpha_eq=1.0
  real :: lat1=5.0
  real :: lat2=10.0

  namelist /rst_update_nml/ alpha, alpha_eq, lat1, lat2

  update_vars="/Temp/Salt/u/v/"

  ! read in namelist
  print *, "Reading rst_update.nml..."
  open(newunit=unit, file="rst_update.nml")
  read(unit, rst_update_nml)
  close(unit)
  print *, ""
  print rst_update_nml
  print *, ""
  
  ! get names of files from the command line
  !------------------------------------------------------------
  CALL get_command_argument(1, da_mode)
  SELECT CASE(da_mode)
  CASE('var','ekf','hyb')
     continue
  CASE DEFAULT
     PRINT *, 'ERROR: usage: "rst_update [hyb,ekf,var] <rst_input> <enkf_ana> <var_ai> <rst_output>"'
     STOP 1
  END SELECT
  
  CALL get_command_argument(2, filename_input)

  i=3
  IF (da_mode /= 'var') THEN
     CALL get_command_argument(i, filename_ekf)
     i = i + 1
  ELSE
     filename_ekf=""
  END IF

  IF (da_mode /= 'ekf') THEN
     CALL get_command_argument(i, filename_var)
     i = i + 1
  ELSE
     filename_var=""
  END IF

  CALL get_command_argument(i, filename_output)

  PRINT *, "input  restart: ", TRIM(filename_input)
  PRINT *, "output restart: ", TRIM(filename_output)
  PRINT *, "LETKF analysis: ", TRIM(filename_ekf)
  PRINT *, "Var  increment: ", TRIM(filename_var)
  PRINT *, ""
  !------------------------------------------------------------


  ! open input/output files
  CALL check( nf90_open(filename_input, nf90_nowrite, ncid_i))
  IF(da_mode /= 'var') CALL check( nf90_open(filename_ekf, nf90_nowrite, ncid_e))
  IF(da_mode /= 'ekf') CALL check( nf90_open(filename_var, nf90_nowrite, ncid_v))
  CALL check( nf90_create( filename_output, nf90_clobber, ncid_o))


  ! create identical dimensions / variables in output file
  !------------------------------------------------------------
  CALL check( NF90_INQUIRE(ncid_i, nDim, nVar, nAtt))

  ! dimensions
  DO i=1, nDim
     CALL check( nf90_inquire_dimension(ncid_i, i, str, l))
     CALL check( nf90_def_dim(ncid_o, str, l, dimid) )
     IF (dimid /= i) THEN
        PRINT *, "error creating dimensions."
        STOP 1
     END IF
  END DO

  ! global attributes
  DO i=1, nAtt
     CALL check( nf90_inq_attname(ncid_i, nf90_global, i, str))
     CALL check( nf90_copy_att(ncid_i, nf90_global, str, ncid_o, nf90_global))
  END DO

  ! variables
  DO i=1, nVar
     CALL check( nf90_inquire_variable(ncid_i, i, varname, vType, nDim, dimids, nAtt))
     CALL check( nf90_def_var(ncid_o, varname, vType, dimids(1:nDim), varid))
     IF ( varid /= i) THEN
        PRINT *, "error creating variable."
        STOP 1
     END IF

     DO j=1, nAtt
        CALL check( nf90_inq_attname(ncid_i, i, j, str))
        IF (str == "checksum") CYCLE
        CALL check( nf90_copy_att(ncid_i, i, str, ncid_o, i))
     END DO
  END DO

  CALL check( nf90_enddef(ncid_o))
  !------------------------------------------------------------


  ! copy variables from restart file
  DO i=1,nVar
     CALL check( nf90_inquire_variable(ncid_o, i,varname, vType, nDim, dimids, nAtt))

     ! determine the data array size
     dimLen=1
     DO j=1,nDim
       CALL check(nf90_inquire_dimension(ncid_o, dimids(j), len=dimLen(j)))
     END DO     
     ALLOCATE(data(dimLen(1),dimLen(2),dimLen(3),dimLen(4)))

     ! get the data
     IF(INDEX(update_vars, TRIM(varname)) > 0) THEN

        ! does the data come from restart file, or enkf analysis?
        IF( da_mode == 'var') THEN
           PRINT *, "Getting ",TRIM(varname)," from restart file"
           CALL check(nf90_get_var(ncid_i, i, data))
        ELSE
           PRINT *, "Getting ",TRIM(varname)," from EnKF analysis"
           CALL check(nf90_inq_varid(ncid_e,varname, j))
           CALL check(nf90_get_var(ncid_e, j, data))
        END IF

        ! does the var analysis increment need to be applied also
        IF( da_mode =="var" .OR. da_mode =="hyb") THEN

           ierr=nf90_inq_varid(ncid_v, varname, j)
           if(ierr == NF90_NOERR) then         
              j = 0
              ! determine if restart the source is from a decomposed grid
              decomp_x=(/1,dimLen(1),1,dimLen(1)/)
              decomp_y=(/1,dimLen(2),1,dimLen(2)/)           
              CALL check(nf90_inquire_variable(ncid_i, i, dimids=dimids))
              call check(nf90_inquire_dimension(ncid_i, dimids(1), name=str))
              call check(nf90_inq_varid(ncid_i, str, vid))
              ierr=nf90_inquire_attribute(ncid_i, vid, "domain_decomposition", attnum=j)
              if(j>0) &
                   call check(nf90_get_att(ncid_i, vid, "domain_decomposition", decomp_x))
              call check(nf90_inquire_dimension(ncid_i, dimids(2), name=str))
              call check(nf90_inq_varid(ncid_i, str, vid))
              ierr=nf90_inquire_attribute(ncid_i, vid, "domain_decomposition", attnum=j)
              if(j>0) &
                   call check(nf90_get_att(ncid_i, vid, "domain_decomposition", decomp_y))

              ! calculate alpha values, if they haven't already
              if(.not. allocated(alpha_val)) then
                 allocate(alpha_val(dimLen(1), dimLen(2),dimLen(3), dimLen(4)))
                 alpha_val=1.0
                 if(da_mode == "hyb") then
                    allocate(data1d(dimLen(2)))
                    call check(nf90_get_var(ncid_i,vid,data1d))
                    do j=1,dimLen(2)
                       r=(abs(data1d(j))-lat2)/(lat2-lat1)
                       if(r>1.0) r=1.0
                       if(r<0.0) r=0.0
                       alpha_val(:,j,:,:) = r*(alpha-alpha_eq) + alpha_eq
                    end do
                    print *, "  generating alpha matrix"
                 end if
              end if

              print *, "  adding 3dvar increment"
              
              ! read in var increment from file (assumed to NOT be on decomposed grid)
              allocate(data2(decomp_x(2), decomp_y(2), dimLen(3),1))
              call check(nf90_inq_varid(ncid_v, varname, j))
              call check(nf90_get_var(ncid_v, j, data2))
              data = data + alpha_val*data2(decomp_x(3):decomp_x(4), decomp_y(3):decomp_y(4), :,:)
              deallocate(data2)
           end if
        END IF
     ELSE
        ! copy the data from existing restart file
        CALL check(nf90_get_var(ncid_i, i, data))
     END IF

     ! write the data
     CALL check(nf90_put_var(ncid_o, i, data))


     DEALLOCATE(data)
  END DO



  ! close 
  CALL check( nf90_close(ncid_i))
  CALL check( nf90_close(ncid_o))



CONTAINS



    subroutine check(status)
    !! helper function to wrap calls to netcdf
    integer, intent(in) :: status
    if(status /= nf90_noerr) then
       print *, trim(nf90_strerror(status))
       stop 1
    end if
  end subroutine check

end program
