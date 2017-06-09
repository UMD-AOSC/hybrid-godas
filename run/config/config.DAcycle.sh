#!/bin/bash
cat << EOF

# directory locations
root_dir="$root_dir"
work_dir="$root_dir/WRK/g3dv_$(echo $exp_dir | md5sum | cut -c 1-6)"
flux_cfsr_dir="/lustre/f1/unswept/ncep/Yan.Xue/MOM6_ensrun/fluxes_CFSR"

# Job submission system properties
moab_acct=cpo_hyb_go
moab_nodes=15
da_nproc=32
moab_walltime="00:30:00"
moab_queue="batch"

# experiment start/end dates
date_start=2003-01-01
date_end=2003-02-01

# experiment properties
da_interval=5  
fcst_leapadj=1
fcst_otherfiles=0

EOF