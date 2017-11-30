#!/bin/bash

cat << \#\#

#================================================================================
#================================================================================
# NCEP Hybrid-GODAS  -  fcst.prep.sh 
#   MOM6 ocean ensemble member forecast preparation
#================================================================================ 
##
#
# Travis.Sluka@noaa.gov / tsluka@umd.edu
#
# Prerequisites:
#  * Daily forcing files for the appropriate date range must exist in the
#    $FORCING_DIR directory
#
# Results:
#  * The $FCST_DIR location will be prepared to run a MOM6 forecast
#
# TODO:
#  * link to the restart file
#  * add support for ensemble perturbations
#
# Required MANUALLY defined environment variables:
#   * The following need to be speficied by the caller of this script
envar+=("ROOT_DIR")     # The path to the hybrid-godas root code/source directory
envar+=("EXP_DIR")      # The path to the experiment directory
envar+=("FORCING_DIR")  # path to the daily flux files
envar+=("FORCING_HR")   # The hour at which forcing is specified (usually 12Z)
envar+=("FCST_DIR")     # The directory that the forecast will be setup in
envar+=("RST_DIR")
#
# Required AUTOMATICALLY defined environment variables:
#  * The following are required but should already be defined by all.common.sh 
envar+=("FCST_START_TIME")  # datetime for start of the forecast (YYYYMMDDZHH)
envar+=("FCST_END_TIME")    # datetime for start of the forecast (YYYYMMDDZHH)
envar+=("FCST_LEN")         # Total length of the forecast (hours)
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


# setup working directory for forecast
#------------------------------------------------------------
echo "Preparing a forecast run from $FCST_START_TIME to $FCST_END_TIME"

if [[ -e "$FCST_DIR" ]]; then
    error "$FCST_DIR already exists, removing."
    rm -rf $FCST_DIR
fi
mkdir -p $FCST_DIR
cd $FCST_DIR

mkdir -p OUTPUT
mkdir -p RESTART
ln -s $ROOT_DIR/build/MOM6 .

# Are we doing a restart? Assume no if the restart path does not exist
# TODO: need better logic here, in case we WANT to restart but the restart dir is
#   accidentally missing
FCST_RESTART=1
if [[ ! -f $RST_DIR ]]; then
    FCST_RESTART=0
fi

# namelist files
cp $EXP_DIR/config/mom/* .
source diag_table.sh > diag_table
source input.nml.sh > input.nml

# static input files
mkdir -p INPUT
ln -s $ROOT_DIR/run/config/mom_input/* INPUT/

# link restart files
rst_dir=$(date "+$RST_DIR" -d "$FCST_START_TIME")
ln -s $rst_dir ./RESTART_IN


# determine the actual start/end dates of the forcing files we need.
#  We are likely to need 1 day before, and 1 day after because the daily forcings
#  are usually specified at 12Z. So a forecast starting at Jan02,0Z will need the
#  Jan01 daily file as well.
#------------------------------------------------------------

# forcing start day (forc_start)
fcst_start_hr=$(date "+%H" -d "$FCST_START_TIME")
forc_start=$FCST_START_TIME
if [[ $fcst_start_hr -lt $FORCING_HR ]]; then
    forc_start=$(date "+$DATE_FMT" -d "$forc_start - 1 day")
fi
forc_start=$(date "+%Y%m%d" -d "$forc_start")

# forcing end day (forc_end)
fcst_end_hr=$(date "+%H" -d "$FCST_END_TIME")
forc_end=$FCST_END_TIME
if [[ $fcst_end_hr -gt $FORCING_HR ]]; then
    forc_end=$(date "+$DATE_FMT" -d "$forc_end + 1 day")
fi
forc_end=$(date "+%Y%m%d" -d "$forc_end")


# prepare surface forcing files
#------------------------------------------------------------
forc_dir=$FCST_DIR/FORC
forcings="PRATE SLP TMP.2m SPFH.2m UGRD.10m VGRD.10m DLWRF.sfc DSWRF.sfc"

echo "preparing surface forcing from $forc_start to $forc_end"
mkdir -p $forc_dir
for f in $forcings; do
    echo "  $f"
    files=""
    date_cur=$forc_start
    #TODO : make sure this can handle start/end times that aren't 00Z
    while [[ $(date -d "$date_cur" +%s) -le $(date -d "$forc_end" +%s) ]];do
	date_next=$(date "+%F" -d "$date_cur + 1 day")
	file=$FORCING_DIR/$(date "+%Y/%Y%m%d/*.%Y%m%d" -d "$date_cur").${f}.nc
	files="$files $file"
	date_cur=$date_next
    done
    ncrcat $files $forc_dir/$f.nc
    ncatted -O -a axis,time,c,c,T $forc_dir/$f.nc
    ncatted -O -a calendar,,m,c,gregorian $forc_dir/$f.nc
done
