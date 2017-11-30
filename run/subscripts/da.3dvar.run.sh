#!/bin/bash

cat << \#\#

#================================================================================
#================================================================================
# NCEP Hybrid-GODAS  -  da.3dvar.run.sh
#   Observation space 3dvar
#================================================================================
##
# Travis.Sluka@noaa.gov / tsluka@umd.edu
#
# Prerequisites:
#
# Results:
#
# Required MANUALLY defined environment variables:
#  * The following need to be defined by the caller of this script:
envar+=("ROOT_DIR")
envar+=("CYCLE")
envar+=("WORK_DIR")
envar+=("PPN")
envar+=("NODES")
envar+=("BKG_FILE")
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


# calculate number of cores to run on
NPROC=$(($PPN * $NODES))
echo "Running with $NPROC cores"


# determine the location of the background file at the analysis time
bkg_file=$(date "+$BKG_FILE" -d "$CYCLE")
echo "using background file "
echo " $bkg_file"


# setup working directory
# TODO : should this be in the da.3dvar.prep.sh  section?
# TODO : move work_dir definition externally
work_dir=$WORK_DIR/3dvar_$CYCLE
#mkdir -p $work_dir
cd $work_dir
ln -sf $bkg_file INPUT/bkg.nc


# run the bgvar/vtloc programs
#------------------------------------------------------------
# TODO : should this be done in the da.3dvar.prep step?
OMP_NUM_THREADS=$PPN
aprun -cc depth -n 1 -d $PPN $ROOT_DIR/build/vtloc
aprun -cc depth -n 1 -d $PPN $ROOT_DIR/build/bgvar


# run the 3dvar
#------------------------------------------------------------
aprun -n $NPROC ./3dvar


# TODO : move the resulting files to their proper destinations?

