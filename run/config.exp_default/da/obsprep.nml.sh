#!/bin/bash
cat <<EOF

&grid_nml
  grid_file="INPUT/grid.nc"
  grid_lon_dim="lonq"
  grid_lat_dim="latq"
  grid_lon_var="geolon"
  grid_lat_var="geolat"
  grid_msk_var="wet"
  grid_D_var="D"
  grid_dpth_file="INPUT/vgrid.nc"
  grid_dpth_dim="Layer"
  grid_dpth_var="Layer"
  grid_coast_file="INPUT/coast_dist.nc"
  grid_coast_var="coast_dist"
/

&obsprep_insitu_nml
  obid_t=2210
  obid_s=2220

  err_cscale_mul=3.0
  err_cscale_dist=400e3

  err_t_d=75,300,450,1000
  err_t_surf=0.78
  err_t_max=1.0
  err_t_do=0.07

  err_s_d=750
  err_s_surf=0.18
  err_s_do=0.02

  vrt_interp=${PROF_INTERP}
  vrt_interp_lvls=1, 3, 5, 7, 9.005, 11.015, 13.03, 15.055, 17.095, 19.16, 21.255, 23.385, 25.56, 27.795, 30.1, 32.49, 34.985, 37.605, 40.375, 43.32, 46.475, 49.88, 53.575, 57.61, 62.05, 66.97, 72.455, 78.61, 85.555, 93.425, 102.385, 112.63, 124.385, 137.91, 153.51, 171.535, 192.38, 216.495, 244.385, 276.605, 313.765, 356.52, 405.565, 461.63, 525.455, 597.77, 679.285, 770.665, 872.5, 985.275, 1109.355, 1244.97, 1392.185, 1550.895, 1720.835, 1901.575, 2092.53, 2292.985, 2502.125, 2719.06, 2942.855, 3172.565, 3407.26, 3646.055, 3888.13, 4132.75, 4379.275, 4627.165, 4875.98, 5125.38, 5375.12, 5625.03, 5875.005, 6125, 6375 

/

&obsprep_combine_nml
 collate=0
 thinning=0
 thinning_eq=10
/

&obsprep_sst_nml
  obid=2210
  platid=1000
  bias_adj=0.0
  err_base=0.2
  err_sses=1.0
  err_superob=1.0
/

&obsop_nml
  statefile="obsop_bkg.nc"
  lat_bounds= -90, 90
/

&obsprep_insitu_legacy_nml
  obsfile="$obsfile_in"
  outfile="$obsfile_out"
  densityfile="obsop_bkg.nc"
  densityvar="rhopot0"
  density_sigma=0.125
/

EOF
