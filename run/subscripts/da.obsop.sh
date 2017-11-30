#!/bin/bash

cat << \#\#

#================================================================================
#================================================================================
# NCEP Hybrid-GODAS  -  da.obsop.sh
#   Observation operator for a single ensemble member / timeslot
#================================================================================
##
# Travis.Sluka@noaa.gov / tsluka@umd.edu
#
# Prerequisites:
#  * The daily background file needs to already be combined and placed in $BKG_FILE
#  * The observations specified by $OBS_SST and $OBS_PROF_T/S need to already exist
#
# Results:
#  * Observation innovations for the given timeslot / ensemble member will be placed
#    in the location specified by $OBSOP_FILE
#
# Required MANUALLY defined environment variables:
#  * The following need to be defined by the caller of this script:
#  * NOTE: Variables below that say they use "datetime placeholders" should have 
#    the filenames specified with  %Y %m %d %H so that the correct file can be chosen
#    based on the actual date needed based on the "$CYCLE + $DA_SLOT" calculation.
envar+=("ROOT_DIR")        # The path to the hybrid-godas root code/source dir
envar+=("CYCLE")           # The datetime of the current cycle and ana time (YYYYMMDDZHH)
envar+=("DA_SLOT")         # The offset (in days) from the analysis time (e.g. "-5")
envar+=("ENS_MEM")         # 0 padded ensemble member number (e.g. "0001")
envar+=("OBS_USE_SST")     # ==1 if SST obs are to be used
envar+=("OBS_USE_PROF")    # ==1 if T/S profiles are to be used
envar+=("OBS_SST")         # The path to the SST observation data (using datetime placeholders)
envar+=("OBS_PROF_T")      # The path to the T profile data (using datetime placeholders)
envar+=("OBS_PROF_S")      # The path to the S profile data (using datetime placeholders)
envar+=("OBS_ERR_ON_MISS") # if == 1, an error is thrown if an obs file is missing
envar+=("BKG_FILE")        # The path to the background file (using datetime placeholders)
envar+=("OBSOP_FILE")      # Path for the output observation operator data (using datetime placeholders)
#
# Required AUTOMATICALLY defined environment variables:
#  * The following are required but should already be defined by all.common.sh
envar+=("FCST_START_TIME")
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


# determine the filename of the model background 
fdate=$(date "+%Y%m%d" -d "$CYCLE + $DA_SLOT days")
bkg_file=$(date "+$BKG_FILE" -d "$fdate")
echo ""
echo "Using background file:"
echo " $bkg_file"

# make sure the background file exists
if [[ ! -f "$bkg_file" ]]; then
    error "ERROR: background file does not exist. Aborting."
    exit 1
fi


# setup the working directory
#------------------------------------------------------------
# TODO, move work_dir to external definition
# TODO move config file and grid file locations to external definition
work_dir=$WORK_DIR/obsop_$fdate/$ENS_MEM
echo ""
echo "Using working directory:"
echo " $work_dir"
if [[ -e $work_dir ]]; then rm -r $work_dir; fi
mkdir -p $work_dir
cd $work_dir
ln -s $bkg_file obsop_bkg.nc
ln -s $ROOT_DIR/build/gsw_data_v3_0.nc .
ln -s $EXP_DIR/config/da/obsprep.nml .
mkdir -p INPUT
ln -s $ROOT_DIR/DATA/grid/ocean_geometry.nc INPUT/grid.nc
ln -s $ROOT_DIR/DATA/grid/Vertical_coordinate.nc INPUT/vgrid.nc


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
	if [[ "$OBS_ERR_ON_MISSING" == 1 ]]; then exit 1; fi
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
    obsprep_exec="$ROOT_DIR/build/obsprep_insitu"

    echo ""
    echo "T/S Profile observations, using files:"
    echo " $obs_t"
    echo " $obs_s"
    
    # make sure files exist
    if [[ ! -f $obs_t ]]; then
	echo " ERROR: Cannot find file for T profiles."
	if [[ "$OBS_ERR_ON_MISSING" == 1 ]]; then exit 1; fi
    elif [[ ! -f $obs_s ]]; then
	echo " ERROR: Cannot find file for S profiles."
	if [[ "$OBS_ERR_ON_MISSING" == 1 ]]; then exit 1; fi
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
basedate="$(date "+%Y,%m,%d,%H,0,0" -d "$CYCLE $DA_SLOT days")"
$ROOT_DIR/build/obsprep_combine -basedate $basedate ${obsprepfiles[@]} obsprep.nc


# observation operator
#------------------------------------------------------------
obsop_file=$(date "+$OBSOP_FILE" -d "$fdate")
obsop_dir=$(dirname "$obsop_file")
mkdir -p $obsop_dir
$ROOT_DIR/build/obsop obsprep.nc $obsop_file
