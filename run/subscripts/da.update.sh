#!/bin/bash

cat << \#\#

#================================================================================
#================================================================================
# NCEP Hybrid-GODAS  -  da.update.sh
#   Post EnKF/3DVar step (restart file updates, etc)
#================================================================================
##
# Travis.Sluka
#
# Prerequisites:
#
# Results:
#
# Required MANUALLY defined environment variables:
#  * The following need to be defined by the caller of this script:
envar+=("ROOT_DIR")
envar+=("EXP_DIR")
envar+=("NPROC")

envar+=("CYCLE_NEXT")
envar+=("FCST_RST_TIME")
#
# Required AUTOMATICALLY defined environment variables:
#  * The following are required but should already be defined by all.common.sh
#================================================================================


# run common script setup
set -e
scriptsdir="$( cd "$( dirname "${BASH_SOURCE[0]}" )" && pwd )"
source ${scriptsdir}/all.common.sh
envar_check "${envar[@]}"
set -u


#--------------------------------------------------------------------------------
#--------------------------------------------------------------------------------


# Check to make sure the restart directory has not already had an analysis increment applied
# TODO : allow rst_dir to be configurable
rst_dir=$EXP_DIR/fcst_rst/mem_0001/%Y%m%d%H
rst_dir=$(date "+$rst_dir" -d "$FCST_RST_TIME")
status_file=$rst_dir/da_update
echo "Updating restart in directory:"
echo " $rst_dir"
if [[ -f "$status_file" ]]; then
    error "It appears the analysis increment has already been applied to the given restart directory"
    # TODO : test to see if AI was SUCCESSFULLY applied (check value in the status_file)
    exit 1
fi


# TODO : 3dvar directory configurable
var_dir=$WORK_DIR/3dvar_$CYCLE

# move output files
# - ana_inc, ana_diag, bgvar, vtloc, obs.varqc.nc

# create status file to indicate we are starting the AI update
echo "0" > $status_file

# apply restart update
aprun -n $NPROC $ROOT_DIR/build/update_restart $var_dir/ana_inc.nc $rst_dir/MOM.res.nc
echo "1" > $status_file

echo "$CYCLE_NEXT_NO_Z" > $EXP_DIR/cycle_status
