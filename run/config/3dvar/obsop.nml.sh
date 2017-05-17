#!/bin/bash

cat << EOF
&obsop_nml
 grid_file="INPUT/grid.nc"
 grid_lon_dim="lonq"
 grid_lat_dim="latq"
 grid_lon_var="geolon"
 grid_lat_var="geolat"
 bkg_file="obsop_bkg.nc"
 obs_file="obsin.nc"
 obsout_file="obsout.nc"
 err=0.5
 err_superob=0.2
 time_offset=${obsop_hr}
/

EOF
