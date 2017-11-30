#!/bin/bash

cat << \#\#

#================================================================================
#================================================================================
# NCEP Hybrid-GODAS  -  fcst.run.sh
#   MOM6 ocean forecast ensemble member model run
#================================================================================
##
#
# Travis.Sluka@noaa.gov / tsluka.umd.edu
#
# Prerequisites:
#  * The $FCST_DIR location must already be setup by the fcst.prep.sh step with
#    the MOM6 configuration and required files in place, including surface forcing
#    files, and restart files (if doing a restart).
#
# Results:
#  * MOM6 forecasts will run in the $FCST_DIR location.
#  * The restart files will be moved to $RST_DIR
#  * other output files will NOT be moved out of the working directory by this
#    script, other jobsteps handle that task
#
# Required MANUALLY defined environment variables:
#  * The following need to be specified by the caller of this script
envar+=("ROOT_DIR")    # The path to the hybrid-godas root code/source directory
envar+=("EXP_DIR")     # The path to the experiment directory
envar+=("FCST_DIR")    # The directory that the forecast will be run in
envar+=("PPN")         # Number of cores per node (procs per node)
envar+=("NODES")       # Number of nodes to use for MPI enabled forecast
envar+=("RST_DIR")     # name of directory that the RESTART output should be moved to
#
# Required AUTOMATICALLY defined environment variables:
#  * The following are required but should already be defined by all.common.sh
#  <none>
#================================================================================
#================================================================================


# run common script initialization, and make sure required env vars exist
set -e
scriptsdir="$( cd "$( dirname "${BASH_SOURCE[0]}" )" && pwd )"
source ${scriptsdir}/all.common.sh
envar_check "${envar[@]}"
set -u


#--------------------------------------------------------------------------------
#--------------------------------------------------------------------------------


# calculate number of cores needed
NPROC=$(($PPN * $NODES))
echo "Running with $NPROC cores"


# run the forecast
cd $FCST_DIR
aprun -n $NPROC ./MOM6


# move the restart files to their desired final location
rst_dir=$(date "+$RST_DIR" -d "$FCST_RST_TIME")
echo "moving restart files to:"
echo " $rst_dir"
if [[ -e "$rst_dir" ]]; then
    echo "WARNING, restart directory already exists, removing old one"
    rm -r $rst_dir
fi
mkdir -p $rst_dir
mv $FCST_DIR/RESTART/* $rst_dir/
