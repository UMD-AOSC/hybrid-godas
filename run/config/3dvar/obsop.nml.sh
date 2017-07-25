#!/bin/bash

cat << EOF
&grid_nml
  grid_file="INPUT/grid.nc"
  grid_lon_dim="lonq"
  grid_lat_dim="latq"
  grid_lon_var="geolon"
  grid_lat_var="geolat"
  grid_msk_var="wet"
  grid_dpth_file="INPUT/vgrid.nc"
  grid_dpth_dim="Layer"
  grid_dpth_var="Layer"
/

&obsprep_insitu_nml
  obsfile="$obsfile_in"
  outfile="$obsfile_out"
  densityfile="obsop_bkg.nc"
  densityvar="rhopot0"
  density_sigma=0.125
/

&obsprep_sst_nml
  obsfile="$obsfile_in"
  outfile="$obsfile_out"
/

&obsop_nml
  obsfile="$obsfile_in"
  outfile="$obsfile_out"
  statefile="obsop_bkg.nc"
  time_offset=$obsop_hr
/

&obsprep_insitu_legacy_nml
  obsfile="$obsfile_in"
  outfile="$obsfile_out"
  densityfile="obsop_bkg.nc"
  densityvar="rhopot0"
  density_sigma=0.125
/

EOF
