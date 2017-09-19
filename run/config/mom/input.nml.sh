#!/bin/bash
# Required variables:
#  $fcst_start   start date in YYYY-MM-DD format
#  $restart      either 'r' or 'n'
#  $fcst_len     integration length, in days

cat <<EOF

 &MOM_input_nml
         output_directory = 'OUTPUT',
         input_filename = '$restart'
         restart_input_dir = 'INPUT',
         restart_output_dir = 'RESTART',
         parameter_filename = 'MOM_input',
                              'MOM_layout',
                              'MOM_saltrestore',
                              'MOM_override'
/

 &SIS_input_nml
        output_directory = 'OUTPUT',
        input_filename = '$restart'
        restart_input_dir = 'INPUT/',
        restart_output_dir = 'RESTART/',
        parameter_filename = 'SIS_input',
                             'SIS_layout',
                             'SIS_override' /

 &atmos_model_nml
           layout = 0, 0
/

 &coupler_nml
            months = 0,
            days   = $fcst_len,
            hours  = 0,
            current_date = $(date "+%Y,%m,%d" -d "$fcst_start"),0,0,0,
            calendar = 'julian',
            dt_cpld = 1800,
            dt_atmos = 1800,
            do_atmos = .false.,
            do_land = .false.,
            do_ice = .true.,
            do_ocean = .true.,
            do_flux = .true.,
            atmos_npes = 0, 
            concurrent = .false.     
            use_lag_fluxes=.false.    
            check_stocks = 0
/

 &diag_manager_nml
            max_axes = 100,
            max_files = 63,
            max_num_axis_sets = 100,
            max_input_fields = 699
            max_output_fields = 699
            mix_snapshot_average_fields=.false.
/

 &flux_exchange_nml
            debug_stocks = .FALSE.
            divert_stocks_report = .TRUE.            
            do_area_weighted_flux = .FALSE.
/

 &fms_io_nml
            fms_netcdf_restart=.true.
            threading_read='multi'
            max_files_r = 200
            max_files_w = 200
            checksum_required = .false.
/

 &fms_nml
            clock_grain='ROUTINE'
            clock_flags='NONE'
            domains_stack_size = 5000000
            stack_size =0
/

 &ice_albedo_nml
            t_range = 10.
/

 &ice_model_nml
/

 &icebergs_nml
    verbose=.false.,
    verbose_hrs=24,
    traj_sample_hrs=24,
    debug=.false.,
    really_debug=.false.,
    use_slow_find=.true.,
    add_weight_to_ocean=.true.,
    passive_mode=.false.,
    generate_test_icebergs=.false.,
    speed_limit=0.,
    use_roundoff_fix=.true.,
    make_calving_reproduce=.true.,
 /

 &monin_obukhov_nml
            neutral = .true.
/

 &ocean_albedo_nml
            ocean_albedo_option = 2
/

 &ocean_rough_nml
            rough_scheme = 'beljaars'
/

 &sat_vapor_pres_nml
            construct_table_wrt_liq = .true.
            construct_table_wrt_liq_and_ice = .true.
/

 &surface_flux_nml
            ncar_ocean_flux = .true.
	    raoult_sat_vap = .true.
            fixed_z_atm_tq = 2.0
            fixed_z_atm_uv = 10.0
/

 &topography_nml
            topog_file = 'INPUT/navy_topography.data.nc'
/

 &xgrid_nml
            make_exchange_reproduce = .false.
            interp_method = 'second_order'
/

EOF
