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
 envar+=("EXP_DIR")
 envar+=("OBS_FILE")
 envar+=("BKG_FILE")
 envar+=("AI_FILE")
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
# TODO, directories as input
work_dir=$TMP_DIR
if [[ -e $TMP_DIR ]]; then  rm -r $TMP_DIR; fi
mkdir -p $TMP_DIR
cd $work_dir

ln -s $ROOT_GODAS_DIR/build/{gsw_data_v3_0.nc,3dvar} .
ln -s $EXP_DIR/config/da/* .

mkdir -p INPUT
cd INPUT
ln -s $ROOT_GODAS_DIR/DATA/grid/ocean_geometry.nc grid.nc
ln -s $ROOT_GODAS_DIR/DATA/grid/Vertical_coordinate.nc vgrid.nc
ln -s $ROOT_GODAS_DIR/DATA/grid/coast_dist.nc .
ln -s $OBS_FILE obs.nc
ln -s $BKG_FILE bkg.nc
#ln -s ../../da.3dvar.prep/omf/obs.nc .
#ln -s ../../da.letkf/OUTPUT/ana_mean.nc bkg.nc
# TODO, remove this (only needed for density, ssh?, include these in the letkf state vector)
ln -s ../../da.prep/bkg/mem_0001/${CYCLE:0:8}.nc bkg2.nc
cd ..

mkdir -p $(dirname $AI_FILE)
ln -s $AI_FILE ana_inc.nc


# run the bgvar/vtloc programs
OMP_NUM_THREADS=$PPN
aprun -cc depth -n 1 -d $PPN $ROOT_GODAS_DIR/build/vtloc
aprun -cc depth -n 1 -d $PPN $ROOT_GODAS_DIR/build/bgvar


# run the 3dvar
aprun -n $NPROC ./3dvar

