PROGRAM pressure_correction
  USE datatable
  USE gsw_mod_toolbox
  USE netcdf

  IMPLICIT NONE

  CHARACTER(len=*), PARAMETER :: nml_file = "pressure_correction.nml"
  CHARACTER(len=:), ALLOCATABLE :: data_file
  CHARACTER(len=:), ALLOCATABLE :: input_bias_file
  CHARACTER(len=:), ALLOCATABLE :: output_bias_file
  CHARACTER(len=:), ALLOCATABLE :: output_pres_file

  INTEGER, PARAMETER :: MAX_TIMESCALES = 9

  ! parameters read in from namelist
  INTEGER :: grid_nx = 0
  INTEGER :: grid_ny = 0
  INTEGER :: grid_nz = 0
  INTEGER :: timescales = 0
  REAL :: bias_decay(MAX_TIMESCALES) = 0.0
  REAL :: bias_gain(MAX_TIMESCALES) = 0.9
  REAL :: lat_dist1(MAX_TIMESCALES) = 5.0
  REAL :: lat_dist2(MAX_TIMESCALES) = 1.0

  ! model grid
  REAL, ALLOCATABLE :: hgrid_lat(:,:)
  REAL, ALLOCATABLE :: hgrid_lon(:,:)
  REAL, ALLOCATABLE :: hgrid_depth(:,:)
  REAL, ALLOCATABLE :: vgrid_interface(:)
  REAL, ALLOCATABLE :: vgrid_layer(:)
  REAL, ALLOCATABLE :: vgrid_dz(:)

  ! state read in
  REAL, ALLOCATABLE :: temp_ana(:,:,:)
  REAL, ALLOCATABLE :: temp_bkg(:,:,:)
  REAL, ALLOCATABLE :: salt_ana(:,:,:)
  REAL, ALLOCATABLE :: salt_bkg(:,:,:)

  ! state read in / written out
  REAL, ALLOCATABLE :: temp_bias(:,:,:,:)
  REAL, ALLOCATABLE :: salt_bias(:,:,:,:)

  ! state written out
  REAL, ALLOCATABLE :: pres(:,:, :)

  ! misc variables
  INTEGER :: unit
  INTEGER :: x, y, z, i
  INTEGER :: ncid, vid, d_x, d_y, d_z, d_t
  INTEGER, ALLOCATABLE :: vids(:,:)
  CHARACTER(len=2) :: str
  LOGICAL :: ex
  INTEGER :: depth_lvl
  REAL, ALLOCATABLE :: lat_weight(:,:,:)
  REAL :: r
  REAL, ALLOCATABLE :: rho_ana(:), rho_bias(:)

  NAMELIST /pressure_correction_nml/ &
       grid_nx, grid_ny, grid_nz, &
       timescales, bias_decay, bias_gain, &
       lat_dist1, lat_dist2, &
       data_file, input_bias_file, output_bias_file, output_pres_file


  ! read in namelist
  data_file = "pressure_correction.data_table"//REPEAT(" ", 1000)
  input_bias_file = "bias.nc"//REPEAT(" ", 1000)
  output_bias_file = "output.bias.nc"//REPEAT(" ", 1000)
  output_pres_file = "pres.nc"//REPEAT(" ", 1000)
  OPEN(newunit=unit, file=nml_file)
  READ(unit, pressure_correction_nml)
  CLOSE(unit)
  data_file = trim(data_file)
  input_bias_file = trim(input_bias_file)
  output_bias_file = trim(output_bias_file)
  output_pres_file = trim(output_pres_file)
  PRINT pressure_correction_nml

  IF(grid_nx <= 0 .OR. grid_ny <= 0 .OR. grid_nz <=0) THEN
     PRINT *, "ERROR: grid_nx, grid_ny, and grid_nz must be set correctly."
     STOP 1
  END IF
  IF(timescales <=0 .OR. timescales > MAX_TIMESCALES) THEN
     PRINT *, 'ERROR: "timescales" must be >= 1 and <= ', MAX_TIMESCALES
     STOP 1
  END IF

  CALL datatable_init(.TRUE., data_file)


  ! read in the grid
  ALLOCATE(hgrid_lat(grid_nx, grid_ny))
  ALLOCATE(hgrid_lon(grid_nx, grid_ny))
  ALLOCATE(hgrid_depth(grid_nx, grid_ny))
  ALLOCATE(vgrid_interface(grid_nz + 1))
  ALLOCATE(vgrid_layer(grid_nz))
  ALLOCATE(vgrid_dz(grid_nz))
  CALL datatable_get("lat", hgrid_lat)
  CALL datatable_get("lon", hgrid_lon)
  CALL datatable_get("max_depth", hgrid_depth)
  CALL datatable_get("vert_interface", vgrid_interface)


  ! calculate derived vertical grid paramters
  DO z=1,grid_nz
     vgrid_dz(z) = vgrid_interface(z+1) - vgrid_interface(z)
     vgrid_layer(z) = (vgrid_interface(z+1) + vgrid_interface(z)) /2.0
  END DO


  ! calculate lat based fading
  ALLOCATE(lat_weight(grid_nx, grid_ny, timescales))
  DO i = 1, timescales
     DO y=1,grid_ny
        DO x=1,grid_nx
           r = ABS(hgrid_lat(x,y))
           IF ( r < lat_dist1(i)) THEN
              lat_weight(x,y,i) = 1.0
           ELSE IF ( r > lat_dist2(i)) THEN
              lat_weight(x,y,i) = 0.0
           ELSE
              lat_weight(x,y,i) = EXP(-0.5 &
                   * (r-lat_dist1(i))*(r-lat_dist1(i)) &
                   /(r-lat_dist2(i)+lat_dist1(i)) )
           END IF
        END DO
     END DO
  END DO


  ! read in temperature/salinity analysis/forecast
  ALLOCATE(temp_bkg(grid_nx, grid_ny, grid_nz))
  ALLOCATE(temp_ana(grid_nx, grid_ny, grid_nz))
  ALLOCATE(salt_bkg(grid_nx, grid_ny, grid_nz))
  ALLOCATE(salt_ana(grid_nx, grid_ny, grid_nz))
  CALL datatable_get("temp_bkg", temp_bkg)
  CALL datatable_get("temp_ana", temp_ana)
  CALL datatable_get("salt_bkg", salt_bkg)
  CALL datatable_get("salt_ana", salt_ana)


  ! read in existing T/S bias fields
  ! if file does not already exist, initialize with zeros
  ALLOCATE(temp_bias(grid_nx, grid_ny, grid_nz, timescales))
  ALLOCATE(salt_bias(grid_nx, grid_ny, grid_nz, timescales))
  INQUIRE(file=input_bias_file, exist=ex)
  IF(.NOT. ex) THEN
     PRINT *, ""
     PRINT *, "WARNING: input bias file does not exist, initializing to zeros"
     temp_bias = 0.0
     salt_bias = 0.0
  ELSE
     DO i = 1,timescales
        WRITE(str,'(A,I1)') '_',i
        CALL read3d(input_bias_file, "temp"//str, temp_bias(:,:,:,i))
        CALL read3d(input_bias_file, "salt"//str, salt_bias(:,:,:,i))
     END DO
  END IF


  ! calculate analysis incement
  !(put it in _bkg since we don't need the background again)
  PRINT *, ""
  PRINT *, "calculating analysis increment..."
  temp_bkg = temp_ana - temp_bkg
  salt_bkg = salt_ana - salt_bkg


  ! calculate updated bias fields
  DO i=1,timescales
     ! bias analysis->forecast (simply decrease existing bias estimate)
     temp_bias(:,:,:,i) = (1.0-bias_decay(i)) * temp_bias(:,:,:,i)
     salt_bias(:,:,:,i) = (1.0-bias_decay(i)) * salt_bias(:,:,:,i)

     ! add new analysis increment to the bias
     DO z=1,grid_nz
        temp_bias(:,:,z,i) = temp_bias(:,:,z,i) &
             -bias_gain(i) * lat_weight(:,:,i) * temp_bkg(:,:,z)
        salt_bias(:,:,z,i) = salt_bias(:,:,z,i) &
             -bias_gain(i) * lat_weight(:,:,i) * salt_bkg(:,:,z)
     END DO
  END DO


  ! do the calculations
  PRINT *, ""
  PRINT *, "Calculating pressure correction..."
  ALLOCATE(pres(grid_nx, grid_ny, timescales))
  ALLOCATE(rho_bias(grid_nz))
  ALLOCATE(rho_ana(grid_nz))
  pres = 0.0
  DO y=1, grid_ny
     DO x=1, grid_nx

        ! skip if on land
        IF(hgrid_depth(x,y) <= 0) CYCLE

        ! find the bottom level index
        depth_lvl = 1
        DO z=1,grid_nz
           IF(vgrid_interface(z+1) > hgrid_depth(x,y)) EXIT
           depth_lvl = z
        END DO


        rho_ana = 0.0
        ! for each of the given timescales
        DO i = 1, timescales

           ! if out side the lat range of this timescale, skip gridpoint
           IF(lat_weight(x,y,i) == 0.0) CYCLE

           ! calculate the analysis density, if not already done so
           IF(rho_ana(1) == 0.0) THEN
              CALL calcDensity( &
                   temp_ana(x,y,1:depth_lvl), &
                   salt_ana(x,y,1:depth_lvl), &
                   hgrid_lat(x,y), hgrid_lon(x,y), &
                   rho_ana(1:depth_lvl))
           END IF

           ! calculate biased density
           rho_bias = 0.0
           CALL calcDensity( &
                temp_ana(x,y,1:depth_lvl) + temp_bias(x,y,1:depth_lvl,i), &
                salt_ana(x,y,1:depth_lvl) + salt_bias(x,y,1:depth_lvl,i), &
                hgrid_lat(x,y), hgrid_lon(x,y), &
                rho_bias(1:depth_lvl))
           rho_bias = rho_bias - rho_ana

           ! integrate to get pressure correction
           pres(x,y,i) = 9.8 * SUM(rho_bias * vgrid_dz)

        END DO
     END DO
  END DO



  ! write the bias output file
  PRINT *, ""
  PRINT *, "Saving output files..."
  ALLOCATE(vids(3, timescales))
  CALL check(nf90_create(output_bias_file, NF90_CLOBBER, ncid))
  CALL check(nf90_def_dim(ncid, "lon",   grid_nx, d_x))
  CALL check(nf90_def_dim(ncid, "lat",   grid_ny, d_y))
  CALL check(nf90_def_dim(ncid, "depth", grid_nz, d_z))
  CALL check(nf90_def_dim(ncid, "time", 0, d_t))
  DO i = 1, timescales
     WRITE(str,'(A,I1)') '_',i
     CALL check(nf90_def_var(ncid, "temp"//str, nf90_real, (/d_x, d_y, d_z, d_t/), vids(1,i)))
     CALL check(nf90_def_var(ncid, "salt"//str, nf90_real, (/d_x, d_y, d_z, d_t/), vids(2,i)))
     CALL check(nf90_def_var(ncid, "pres"//str, nf90_real, (/d_x, d_y, d_t/),      vids(3,i)))
  END DO
  CALL check(nf90_enddef(ncid))
  DO i = 1, timescales
     CALL check(nf90_put_var(ncid, vids(1,i), temp_bias(:,:,:,i)))
     CALL check(nf90_put_var(ncid, vids(2,i), salt_bias(:,:,:,i)))
     CALL check(nf90_put_var(ncid, vids(3,i), pres(:,:,i)))
  END DO
  CALL check(nf90_close(ncid))


  ! write the pressure correction output file
  DO i = 2, timescales
     pres(:,:,1) = pres(:,:,1) + pres(:,:,i)
  END DO
  CALL check(nf90_create(output_pres_file, NF90_CLOBBER, ncid))
  CALL check(nf90_def_dim(ncid, "lon",   grid_nx, d_x))
  CALL check(nf90_def_dim(ncid, "lat",   grid_ny, d_y))
  CALL check(nf90_def_dim(ncid, "depth", grid_nz, d_z))
  CALL check(nf90_def_dim(ncid, "time", 0, d_t))
  CALL check(nf90_def_var(ncid, "pres",  nf90_real, (/d_x, d_y, d_t/), vid))
  CALL check(nf90_enddef(ncid))
  CALL check(nf90_put_var(ncid, vid, pres(:,:,1)))
  CALL check(nf90_close(ncid))



CONTAINS



  SUBROUTINE calcDensity(temp, salt, lat, lon, rho)
    REAL, INTENT(in) :: temp(:)
    REAL, INTENT(in) :: salt(:)
    REAL, INTENT(in) :: lat, lon
    REAL, INTENT(out) :: rho(:)

    REAL(8) :: p(SIZE(rho))
    REAL(8) :: sa(SIZE(rho))
    REAL(8) :: ct(SIZE(rho))

    INTEGER :: nz

    nz = SIZE(rho)

    p = gsw_p_from_z(-vgrid_layer(1:nz)*1.0d0, lat*1.0d0)
    sa = gsw_sa_from_sp(salt*1.0d0, p, lon*1.0d0, lat*1.0d0)
    ct = gsw_ct_from_pt(sa, temp*1.0d0)
    rho = gsw_rho(sa, ct, 0.0d0)

  END SUBROUTINE calcDensity



  SUBROUTINE read3d(filename, varname, val)
    CHARACTER(len=*), INTENT(in) :: filename
    CHARACTER(len=*), INTENT(in) :: varname
    REAL, INTENT(inout) :: val(:,:,:)

    INTEGER :: ncid, vid
    CALL check(nf90_open(filename, NF90_NOWRITE, ncid))
    CALL check(nf90_inq_varid(ncid, varname, vid))
    CALL check(nf90_get_var(ncid, vid, val))
    CALL check(nf90_close(ncid))

  END SUBROUTINE read3d


  SUBROUTINE check(status)
    INTEGER, INTENT(in) :: status
    IF(status /= nf90_noerr) THEN
       PRINT *, status
       PRINT *, TRIM(nf90_strerror(status))
       STOP 1
    END IF
  END SUBROUTINE check

END PROGRAM pressure_correction
