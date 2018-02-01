#!/bin/bash
set -e
cat << \#\#

#================================================================================
#================================================================================
# NCEP Hybrid-GODAS  -  da.3dvar.run.sh
#   Observation space 3dvar
#================================================================================
##
#
# Prerequisites:
#
# Results:
#
# Required environment variables:
 envar=()
 envar+=("ROOT_GODAS_DIR")
 envar+=("TMP_DIR")
 envar+=("PPN")
 envar+=("NODES")
#================================================================================
#================================================================================


# make sure required env vars exist
for v in ${envar[@]}; do
    if [[ -z "${!v}" ]]; then
	echo "ERROR: env var $v is not set."; exit 1
    fi
    echo " $v = ${!v}"
done
set -u
echo ""


# calculate number of cores to run on
NPROC=$(($PPN * $NODES))
echo "Running with $NPROC cores"


# change to working directory
work_dir=$TMP_DIR
cd $work_dir


# run the bgvar/vtloc programs
OMP_NUM_THREADS=$PPN
aprun -cc depth -n 1 -d $PPN $ROOT_GODAS_DIR/build/vtloc
aprun -cc depth -n 1 -d $PPN $ROOT_GODAS_DIR/build/bgvar


# run the 3dvar
aprun -n $NPROC ./3dvar

