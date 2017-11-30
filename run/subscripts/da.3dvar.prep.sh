#!/bin/bash

cat << \#\#

#================================================================================
#================================================================================
# NCEP Hybrid-GODAS  -  da.3dvar.prep.sh
#   3DVar preparation
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
envar+=("EXP_DIR")
envar+=("CYCLE")
envar+=("WORK_DIR")
envar+=("DA_WNDW_SLOTS")
envar+=("OBSOP_FILE")
envar+=("OBSOP_CMB_FILE")
# 
# Required AUTOMATICALLY defined environment variables:
#  *  The following are required but should already be defined by all.common.sh
#================================================================================
#================================================================================


# run common script setup
set -e
scriptsdir="$( cd "$( dirname "${BASH_SOURCE[0]}" )" && pwd )"
source ${scriptsdir}/all.common.sh
envar_check "${envar[@]}"
set -u


#--------------------------------------------------------------------------------
#--------------------------------------------------------------------------------


# make sure file searches below return null if nothing exists
shopt -s nullglob


# setup working directory
#------------------------------------------------------------
# TODO, move work_dir to external definition
work_dir=$WORK_DIR/3dvar_$CYCLE
echo ""
echo "Using working directory:"
echo " $work_dir"
if [[ -e $work_dir ]]; then rm -r $work_dir; fi
mkdir -p $work_dir
cd $work_dir

ln -s $ROOT_DIR/build/gsw_data_v3_0.nc . # is this needed?
ln -s $ROOT_DIR/build/3dvar .

ln -s $EXP_DIR/config/da/* .  

mkdir -p INPUT
ln -s $ROOT_DIR/DATA/grid/ocean_geometry.nc INPUT/grid.nc
ln -s $ROOT_DIR/DATA/grid/Vertical_coordinate.nc INPUT/vgrid.nc
ln -s $ROOT_DIR/DATA/grid/coast_dist.nc INPUT/

# combine the observation increment files
#------------------------------------------------------------
# TODO : instead of ens 0001, should use "mean"
basedate="$(date "+%Y,%m,%d,%H,0,0" -d "$CYCLE")"
obsfiles=()
for s in $DA_WNDW_SLOTS; do
    f="$(date "+$OBSOP_FILE" -d "$CYCLE $s days")"
    if [[ -f "$f" ]]; then obsfiles+=("$f"); fi
done
echo "Using observations from ${#obsfiles[@]} timeslots"
for f in "${obsfiles[@]}"; do
    echo "  $f"
done

obsop_cmb_file=$(date "+$OBSOP_CMB_FILE" -d "$CYCLE")
obsop_cmb_dir=$(dirname $obsop_cmb_file)
mkdir -p $obsop_cmb_dir

$ROOT_DIR/build/obsprep_combine -basedate $basedate ${obsfiles[@]} $obsop_cmb_file
ln -s $obsop_cmb_file INPUT/obs.nc


# background variance
#============================================================
# TODO : should these be moved here?

# vertical localization distance
#============================================================
# TODO : should these be moved here?

