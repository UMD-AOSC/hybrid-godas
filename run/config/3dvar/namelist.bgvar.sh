#!/bin/bash
cat <<EOF
 
&bgvar_nml
 init=${da_bgvar_init:-F}
 day_update = ${da_interval}
 day_relax_obs=30
 day_relax_clim=90

 bgvar_file_out="bgvar.nc"

 grid_nx = 1440
 grid_ny = 1080
 grid_nz = 75

 cgrid_nx = 720
 cgrid_ny = 330
 cgrid_nz = 50
/

EOF