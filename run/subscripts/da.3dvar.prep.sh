#!/bin/bash
set -e
cat << \#\#

#================================================================================
#================================================================================
# NCEP Hybrid-GODAS  -  da.3dvar.prep.sh
#   3DVar preparation
#================================================================================
##
#
# Prerequisites:
#  
# Results:
#
# Required environment variables:
 envar+=("ROOT_GODAS_DIR")
 envar+=("EXP_DIR")
 envar+=("CYCLE")
 envar+=("TMP_DIR")
 envar+=("DA_WNDW_SLOTS")
 envar+=("OMF_SLOT_FILE")
 envar+=("OMF_FILE")
 envar+=("BKG_FILE")
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


# make sure file searches below return null if nothing exists
shopt -s nullglob


# setup working directory
#------------------------------------------------------------
work_dir=$TMP_DIR
echo ""
echo "Using working directory:"
echo " $work_dir"
if [[ -e $work_dir ]]; then rm -r $work_dir; fi
mkdir -p $work_dir
cd $work_dir

ln -s $ROOT_GODAS_DIR/build/gsw_data_v3_0.nc . # is this needed?
ln -s $ROOT_GODAS_DIR/build/3dvar .

ln -s $EXP_DIR/config/da/* .  

mkdir -p INPUT
ln -s $ROOT_GODAS_DIR/DATA/grid/ocean_geometry.nc INPUT/grid.nc
ln -s $ROOT_GODAS_DIR/DATA/grid/Vertical_coordinate.nc INPUT/vgrid.nc
ln -s $ROOT_GODAS_DIR/DATA/grid/coast_dist.nc INPUT/
ln -s $BKG_FILE INPUT/bkg.nc



# combine the observation increment files
#------------------------------------------------------------
dtz(){ echo ${1:0:8}Z${1:8:10}; }
basedate="$(date "+%Y,%m,%d,%H,0,0" -d "$(dtz $CYCLE)")"
obsfiles=()
for s in $DA_WNDW_SLOTS; do
    f="$(date "+$OMF_SLOT_FILE" -d "$(dtz $CYCLE) $s days")" # subsitute date/time, if that's whats needed
    f=${f//#slot#/$s}                                        # subsitute slot, if that's whats needed
    echo $f
    if [[ -f "$f" ]]; then obsfiles+=("$f"); fi
done
echo "Using observations from ${#obsfiles[@]} timeslots"
for f in "${obsfiles[@]}"; do
    echo "  $f"
done

obsop_cmb_file=$(date "+$OMF_FILE" -d "$(dtz $CYCLE)")
obsop_cmb_dir=$(dirname $obsop_cmb_file)
mkdir -p $obsop_cmb_dir

rm -f $obsop_cmb_file
$ROOT_GODAS_DIR/build/obsprep_combine -basedate $basedate ${obsfiles[@]} $obsop_cmb_file
ln -s $obsop_cmb_file INPUT/obs.nc


# background variance
#============================================================
# TODO : should these be moved here?

# vertical localization distance
#============================================================
# TODO : should these be moved here?

