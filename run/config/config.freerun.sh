#!/bin/bash


cat << EOF

# Main experiment configuration script for the MOM6-GODAS
#   for MOAB systems (Gaea)

# directory locations
# NOTE: root_dir was set automatically but should be checked to make sure it is 
#  a path accessible on the compute nodes (i.e. usually the /lustre partition and not
#  the home directory)
# NOTE: the work_dir is a temporary space, and should be moved to a swept lustre location
root_dir="$root_dir"
work_dir="$root_dir/WRK/g3dv_$(echo $exp_dir | md5sum | cut -c 1-6)"
flux_cfsr_dir="/lustre/f1/unswept/ncep/Yan.Xue/MOM6_ensrun/fluxes_CFSR"

# Job submission system properties
# NOTE: if number of nodes is changed, the MOM configuration file will need
#  to be changed as well
moab_acct=cpo_hyb_go
moab_nodes=15
moab_walltime="00:30:00"
moab_queue="batch"

# experiment start/end dates
# NOTE: date_start only has an impact for initializing a new experiment. Otherwise
#  subsequent runs will simply pick up from where the previous run stopped.
date_start=2003-01-01
date_end=2004-01-01

# length of each job (days), if the end date is not reached at the end of a given job
#  run, another job is submitted to continue the run
# NOTE: if this is changed then the moab_walltime should be changed above as well
#  (e.g. 30 days takes about 2 hours, 5 days about 20 minutes)
fcst_len=5
fcst_leapadj=1

# normally the forecasting scripts save daily mean files at specific intervals,
# this just tells the scripts to save any other files specified on the mom/diag_table file
# (E.g. pentad output)
fcst_dailymean=0
fcst_otherfiles=1


EOF