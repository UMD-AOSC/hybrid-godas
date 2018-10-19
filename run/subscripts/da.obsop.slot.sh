#!/bin/bash
set -e
cat << \#\#

#================================================================================
#================================================================================
# NCEP Hybrid-GODAS  -  da.obsop.sh
#   Observation operator for a single ensemble member / timeslot
#================================================================================
##
#
# Prerequisites:
#  * The daily background file needs to already be combined and placed in $BKG_FILE
#  * The observations specified by $OBS_SST and $OBS_PROF_T/S need to already exist
#
# Results:
#  * Observation innovations for the given timeslot / ensemble member will be placed
#    in the location specified by $OBSOP_FILE
#
# Required environment variables:
#  * NOTE: Variables below that say they use "datetime placeholders" should have 
#    the filenames specified with  %Y %m %d %H so that the correct file can be chosen
#    based on the actual date needed based on the "$CYCLE + $DA_SLOT" calculation.
 envar=()
 envar+=("ROOT_GODAS_DIR")  # The path to the hybrid-godas root code/source dir
 envar+=("JOB_WORK_DIR")
 envar+=("CYCLE")           # The datetime of the current cycle and ana time (YYYYMMDDHH)
 envar+=("DA_SLOT")         # The offset (in hours) from the analysis time (e.g. "-5")
 envar+=("BKG_FILE")        # The path to the background file (using datetime placeholders)
 envar+=("OBSOP_FILE")      # Path for output observation operator data (using datetime placeholders)
 envar+=("OBS_USE_SST")     # ==1 if SST obs are to be used
 envar+=("OBS_USE_PROF")    # ==1 if T/S profiles are to be used
 envar+=("OBS_SST")         # The path to the SST observation data (using datetime placeholders)
 envar+=("OBS_PROF_T")      # The path to the T profile data (using datetime placeholders)
 envar+=("OBS_PROF_S")      # The path to the S profile data (using datetime placeholders)
 envar+=("OBS_ERR_ON_MISS") # if == 1, an error is thrown if an obs file is missing
 envar+=("PROF_INTERP")
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


# determine the filename of the model background 
# TODO: pull this calculation out of the script into the rocoto xml
dtz(){ echo ${1:0:8}Z${1:8:10}; }
fdate=$(date "+%Y%m%d" -d "$(dtz $CYCLE) + $DA_SLOT hours")
bkg_file=$(date "+$BKG_FILE" -d "$fdate")
echo ""
echo "Using background file:"
echo " $bkg_file"


# make sure the background file exists
if [[ ! -f "$bkg_file" ]]; then
    echo "ERROR: background file does not exist. Aborting."
    exit 1
fi


# setup the working directory
#------------------------------------------------------------
# TODO, move work_dir to external definition
# TODO move config file and grid file locations to external definition
work_dir=$JOB_WORK_DIR
echo ""
echo "Using working directory:"
echo " $work_dir"
if [[ -e $work_dir ]]; then rm -r $work_dir; fi
mkdir -p $work_dir

cd $work_dir
ln -s $bkg_file obsop_bkg.nc
source $EXP_DIR/config/da/obsprep.nml.sh  > obsprep.nml
mkdir -p INPUT
ln -s $ROOT_GODAS_DIR/DATA/grid/ocean_geometry.nc INPUT/grid.nc
ln -s $ROOT_GODAS_DIR/DATA/grid/Vertical_coordinate.nc INPUT/vgrid.nc
ln -s $ROOT_GODAS_DIR/DATA/grid/coast_dist.nc INPUT/coast_dist.nc


# SST observations
#------------------------------------------------------------
if [[ "$OBS_USE_SST" == 1 ]]; then
    obs_sst="$(date "+$OBS_SST" -d "$fdate")"
    echo ""
    echo "SST observations, using file:"
    echo " $obs_sst"

    # make sure file exists
    if [[ ! -f $obs_sst ]]; then
	echo " ERROR: Cannot find file"
	if [[ "$OBS_ERR_ON_MISS" == 1 ]]; then exit 1; fi
    else
	# TODO : the obsprep routine has already been run for these files
	#  Add a configurable to manually allow bypass
	echo "file has already been processed, linking..."
	ln -s $obs_sst obsprep.sst.nc
    fi
fi


# insitu T/S observations
#------------------------------------------------------------
if [[ "$OBS_USE_PROF" == 1 ]]; then
    #TODO : right now the obsprep_insitu program only takes 1 filename as input
    #  let it take 2, or only specify 1 here

    obs_t="$(date "+$OBS_PROF_T" -d "$fdate")"
    obs_s="$(date "+$OBS_PROF_S" -d "$fdate")"
    obsprep_exec="$ROOT_GODAS_DIR/build/obsprep_insitu"

    echo ""
    echo "T/S Profile observations, using files:"
    echo " $obs_t"
    echo " $obs_s"
    
    # make sure files exist
    if [[ ! -f $obs_t ]]; then
	echo " ERROR: Cannot find file for T profiles."
	if [[ "$OBS_ERR_ON_MISS" == 1 ]]; then exit 1; fi
    elif [[ ! -f $obs_s ]]; then
	echo " ERROR: Cannot find file for S profiles."
	if [[ "$OBS_ERR_ON_MISS" == 1 ]]; then exit 1; fi
    else
	$obsprep_exec ${obs_t:0: -5} obsprep.insitu.nc
    fi
fi


# Combine into single file
#------------------------------------------------------------
obsprepfiles=(obsprep.*.nc)
if [[ "${#obsprepfiles[@]}" == "0" ]]; then
    echo "No observations files to perform obsop on. Quitting."
    exit 1
fi
basedate="$(date "+%Y,%m,%d,%H,0,0" -d "$(dtz $CYCLE) $DA_SLOT hours")"
$ROOT_GODAS_DIR/build/obsprep_combine -basedate $basedate ${obsprepfiles[@]} obsprep.nc


# observation operator
#------------------------------------------------------------
obsop_file=$(date "+$OBSOP_FILE" -d "$fdate")
obsop_dir=$(dirname "$obsop_file")
mkdir -p $obsop_dir
if [[ -e "$obsop_file" ]]; then rm $obsop_file; fi
$ROOT_GODAS_DIR/build/obsop obsprep.nc $obsop_file


# # convert output from nc to dat
# # TODO: only do this if needed by LETKF
# # TODO: remove hardcoding of observation IDs in LETKF/3dvar/obsops
# echo ""
# echo "Converting .nc obs files to .dat ..."
# dat_file=${obsop_file:0: -3}.dat
#  if [[ -e "$dat_file" ]]; then rm $dat_file; fi
# $ROOT_GODAS_DIR/build/obsprep_nc2dat $obsop_file ${obsop_file:0: -3}.dat 2210:3073,2220:5521
# #,sst:5525
