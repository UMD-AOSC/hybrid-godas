#!/bin/bash
cat << EOF

# Main experiment configuration script for the MOM6-GODAS
#  for MOAB systems (Gaea)


# directory locations
#--------------------------------------------------------------------------------
# NOTE: root_dir was set automatically but should be checked to make sure it is
#  a path accessible on the compute nodes (i.e. usually the /lustre partition and not
#  the home directory)
# NOTE: the work_dir is a temporary space, and should be moved to a swept lustre location

root_dir="$root_dir"
work_dir="$root_dir/WRK/g3dv_$(echo $exp_dir | md5sum | cut -c 1-6)"
flux_cfsr_dir="$root_dir/DATA/fluxes/cfsr"


# Job submission system properties
#--------------------------------------------------------------------------------
# NOTE: if then umber of nodes is changed, the MOM configuration file will need
#  to be changed as well
# NOTE: Currently the scripts are hardcoded to use the c4 partition of Gaea, (need to change this)

moab_acct=cpo_hyb_go
moab_nodes=20
da_nproc=144
da_threads=35
moab_walltime="00:20:00"
moab_queue="batch"


# experiment properties
# ------------------------------------------------------------

# experiment start/end dates
date_start=2003-01-01
date_end=2003-12-31

# observations
da_sst_use=1
da_sst_dir=$root_dir/DATA/obs/sst_acspo_avhrr
da_prof_use=1
da_prof_legacy=0
da_prof_dir=$root_dir/DATA/obs/profile_wod

# skips 3dvar code if =1, instead doing just the obsop
da_skip=0        

# da cycle, in days
da_interval=5 

# if 1 a "pentad" is 6 days if there is a leap day
fcst_leapadj=1   

# dont bother masking land for now, it just 
# slows down the DA cycle (need to convert the program from 
# python to fortran), land can be masked out in postprocessing.
fcst_maskland=0

EOF
