#!/bin/bash
set -e
cat << \#\#

#================================================================================
#================================================================================
# NCEP Hybrid-GODAS  -  fcst.prep.sh 
#   MOM6 ocean ensemble member forecast preparation
#================================================================================ 
##
#
# Prerequisites:
#  * Daily forcing files for the appropriate date range must exist in the
#    $FORC_MEAN_FILE (and $FORC_ENS_FILE locations if doing ensemble DA)
#  * the restart files do NOT need to be ready yet for this script to finish
#
# Results:
#  * ensemble surface forcing files are placed in $FORC_DIR
#
# TODO:
#  * handle when FORC_PERTURB=0 (no ctrl file is needed for recentering)
#
# Required environment variables:
 envar=()
 envar+=("JOB_WORK_DIR")
 envar+=("FORC_DIR")         # Directory for the output generated surface forcings

 envar+=("FCST_START_TIME")  # Datetime for start of the forecast (YYYYMMDDHH)
 envar+=("FCST_LEN")         # Length of the forecast (hours)

 envar+=("FORC_HR")          # The hour at which forcing is specified (usually 12Z)
 envar+=("FORC_MEAN_FILE")   # path to the input daily flux files
 envar+=("FORC_ENS_FILE")    # path to the input daily flux file ensemble perturbations
 envar+=("FORC_PERTURB")

 envar+=("ENS_LIST")
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


# variables in the forcing files
forcings="PRATE SLP TMP.2m SPFH.2m UGRD.10m VGRD.10m DLWRF.sfc DSWRF.sfc"

# variables that have ensemble perturbations
forcings_ens="PRATE TMP.2m SPFH.2m UGRD.10m VGRD.10m DLWRF.sfc DSWRF.sfc"

# variables that do NOT yet have ensemble perturbations 
# TODO: get SLP working with ensembles
forcings_mean="SLP"

# variables that should be kept positive 
#  (in case ensemble perturbation makes it negative)
clamp_positive="PRATE SPFH.2m DLWRF.sfc DSWRF.sfc"

# surface forcing interpolation method ("bil" or "bic")
interp="bic"



# calculate some other parameters
#------------------------------------------------------------
dtz() {
   echo "${1:0:8}Z${1:8:10}"
}

FCST_END_TIME=$(date "+%Y%m%d%H" -d "$(dtz $FCST_START_TIME) + $FCST_LEN hours")
echo " FCST_END_TIME=$FCST_END_TIME"
echo ""


# setup forecast working directory
#------------------------------------------------------------
if [[ -e "$FORC_DIR" ]]; then
    echo "WARNING: FORC_DIR already exists, removing:"
    echo " $FORC_DIR"
    rm -rf "$FORC_DIR"
fi
mkdir -p "$FORC_DIR"

if [[ -e "$JOB_WORK_DIR" ]]; then
    echo "WARNING: JOB_WORK_DIR already exists, removing:"
    echo " $JOB_WORK_DIR"
    rm -rf "$JOB_WORK_DIR"
fi
mkdir -p "$JOB_WORK_DIR"
cd $JOB_WORK_DIR



# determine the actual start/end dates of the forcing files we need.
#  We are likely to need 1 day before, and 1 day after because the daily forcings
#  are usually specified at 12Z. So a forecast starting at Jan02,0Z will need the
#  Jan01 daily file as well.
#------------------------------------------------------------

forc_start_dy=${FCST_START_TIME:0:8}
forc_start_hr=${FCST_START_TIME:8:12}
if [[ $forc_start_hr -lt $FORC_HR ]]; then
    forc_start_dy=$(date "+%Y%m%d" -d "$forc_start_dy - 1 day")
fi

forc_end_dy=${FCST_END_TIME:0:8}
forc_end_hr=${FCST_END_TIME:8:12}
if [[ $forc_end_hr -gt $FORC_HR ]]; then
    forc_end_dy=$(date "+%Y%m%d" -d "$forc_end_dy + 1 day")
fi

echo "preparing surface forcing from $forc_start_dy to $forc_end_dy"
echo " forcing variables are: $forcings"
echo ""


# Create the combined mean forcing file
#------------------------------------------------------------
mkdir -p mean
echo "Generating control forcing file..."
for f in $forcings; do
    files=()
    date_cur=$forc_start_dy
    # TODO : make sure this can handle start/end times that aren't 00Z
    while [[ $(date -d "$date_cur" +%s) -le $(date -d "$forc_end_dy" +%s) ]];do
	date_next=$(date "+%F" -d "$date_cur + 1 day")	
	file=${FORC_MEAN_FILE//#var#/$f}
	file=$(date "+$file" -d "$date_cur")
	files+=("$file")
	date_cur=$date_next
    done
    ncrcat ${files[@]} mean/$f.nc
    ncatted -O -a axis,time,c,c,T mean/$f.nc
    ncatted -O -a calendar,,m,c,gregorian mean/$f.nc
done
echo ""


# If we are doing an ensemble run, process the ensemble perturbations
# ------------------------------------------------------------
ens_list=($ENS_LIST)
ens_size=${#ens_list[@]}
if [[ "$ens_size" -gt 1 ]]; then
    # Create the combined forcing file for each member
    echo "Generating ensemble member forcing files..."
    for m in ${ens_list[@]}; do
	d=ens/mem_$m
	mkdir -p $d
	echo " member: $m"

 	for f in $forcings_ens; do
 	    files=()
 	    date_cur=$forc_start_dy
 	    #TODO : create a function that combines this with the previous ctrl script lines
 	    while [[ $(date -d "$date_cur" +%s) -le $(date -d "$forc_end_dy" +%s) ]];do
 		date_next=$(date "+%F" -d "$date_cur + 1 day")
 		file=${FORC_ENS_FILE//#var#/$f}
 		file=${file//#mem2#/${m: -2}}
 		file=$(date "+$file" -d "$date_cur")
 		files+=("$file")
 		date_cur=$date_next
 	    done
 	    ncrcat ${files[@]} $d/$f.nc
 	done
     done
     echo ""

     # Calculate the mean of the ens files,
     echo "Generating ensemble forcing mean..."
     mkdir -p ens_mean
     for f in $forcings_ens; do
 	cdo ensmean "ens/*/$f.nc" ens_mean/$f.nc
     done
     echo "" 

     # generate remap weights
     echo 'Generating "ens->mean" interpolation weights...'
     mkdir -p weights
     for f in $forcings_ens; do
 	cdo gen${interp},mean/$f.nc ens_mean/$f.nc weights/$f.nc
     done
     echo ""

     # remap the ens means and calculate mean-ens_mean
     echo 'Calculating "mean - ens_mean"...'
     mkdir -p offset
     for f in $forcings_ens; do
 	cdo sub mean/$f.nc -remap,mean/$f.nc,weights/$f.nc ens_mean/$f.nc offset/$f.nc
     done
     echo ""

     # generate the final individual ensemble forcing files
     echo "Generating final ensemble members forcings..."
     for m in ${ens_list[@]}; do
 	d=$FORC_DIR/mem_$m
 	mkdir -p $d
 	for f in $forcings_ens; do
 	    cdo add offset/$f.nc -remap,mean/$f.nc,weights/$f.nc ens/mem_$m/$f.nc $d/$f.nc
	    
 	    # Certain fields need to be kept positive
 	    clamp=0
 	    for f2 in $clamp_positive; do
 		if [[ "$f2" == "$f" ]]; then clamp=1; fi
 	    done
 	    if [[ "$clamp" -eq 1 ]]; then
 		cdo setrtoc,-1e20,0,0 $d/$f.nc $d/$f.nc.2
 		mv $d/$f.nc.2 $d/$f.nc
 	    fi

 	    ncatted -O -a axis,time,c,c,T $d/$f.nc
 	    ncatted -O -a calendar,,m,c,gregorian $d/$f.nc	    
 	done
 	for f in $forcings_mean; do
 	    cp mean/$f.nc $d/$f.nc
 	done
     done
     
else
     ln -s ../mean $FORC_DIR/mem_0001
fi
