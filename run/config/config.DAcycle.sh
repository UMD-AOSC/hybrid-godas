#!/bin/bash
cat << EOF

# Main experiment configuration script for the MOM6-GODAS
#  for MOAB systems (Gaea)


# directory locations
# NOTE: root_dir was set automatically but should be checked to make sure it is
#  a path accessible on the compute nodes (i.e. usually the /lustre partition and not
#  the home directory)
# NOTE: the work_dir is a temporary space, and should be moved to a swept lustre location
root_dir="$root_dir"
work_dir="$root_dir/WRK/g3dv_$(echo $exp_dir | md5sum | cut -c 1-6)"
flux_cfsr_dir="$root_dir/DATA/fluxes/cfsr_corr"

# Job submission system properties
# NOTE: if then umber of nodes is changed, the MOM configuration file will need
#  to be changed as well
moab_acct=cpo_hyb_go
moab_nodes=15
da_nproc=64
da_threads=16
moab_walltime="00:40:00"
moab_queue="batch"

# experiment start/end dates
date_start=2003-01-01
date_end=2003-02-01


# experiment properties
# ------------------------------------------------------------

da_sst_use=1
da_prof_use=1
da_prof_legacy=1
da_prof_dir=$root_dir/DATA/obs/profile_legacy

# skips 3dvar code if =1, instead doing just the obsop
da_skip=0        

# da cycle, in days
da_interval=5    

# if 1 a "pentad" is 6 days if there is a leap day
fcst_leapadj=1   

# leave as 1
fcst_otherfiles=1

EOF