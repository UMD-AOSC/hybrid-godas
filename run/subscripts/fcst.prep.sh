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
#    $FORCING_CTRL_FILE (and $FORCING_ENS_FILE locations if doing ensemble DA)
#  * the restart files do NOT need to be ready yet for this script to finish
#
# Results:
#  * The $FCST_DIR location(s) will be prepared to run a MOM6 forecast
#
# TODO:
#  * handle when FORCING_PERTURB=0 (no ctrl file is needed for recentering)
#  * do the ensemble perturbations need to be clamped positive for
#    radiation / precip / q ??
#
# Required MANUALLY defined environment variables:
#  * The following need to be speficied by the caller of this script

envar+=("ROOT_DIR")     # The path to the hybrid-godas root code/source directory
envar+=("EXP_DIR")      # The path to the experiment directory
envar+=("FORC_DIR")

envar+=("ENSEMBLE")     # =0 if only doing single control run, =1 if doing ensemble DA
envar+=("ENS_SIZE")
#envar+=("ENS_CTRL")

envar+=("FORCING_HR")        # The hour at which forcing is specified (usually 12Z)
envar+=("FORCING_PERTURB")
envar+=("FORCING_CTRL_FILE")  # path to the daily flux files
envar+=("FORCING_ENS_FILE")   #

# these have the "#mem#" placeholder
envar+=("FCST_DIR")     # The directory that the forecast(s) will be setup in
envar+=("RST_DIR")      

envar+=("FCST_RESTART") # =1 is yes, =0 is no, =-1 is figure out based on CYCLE_START
                        # and whether the cycle_status file exists
#envar+=("CYCLE")
envar+=("CYCLE_NO_Z")
envar+=("CYCLE_START")

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



# TODO: need to get SLP/PRES issue fixed
# TODO: move generation of ensemble member list to the common.sh file

forcings="PRATE SLP TMP.2m SPFH.2m UGRD.10m VGRD.10m DLWRF.sfc DSWRF.sfc"
forcings_ens="PRATE TMP.2m SPFH.2m UGRD.10m VGRD.10m DLWRF.sfc DSWRF.sfc"
forcings_ctrl="SLP"
clamp_positive="PRATE SPFH.2m DLWRF.sfc DSWRF.sfc"
mem_list="$(seq -s ' ' -f "%04g" 1 $ENS_SIZE)"
interp="bil" # or bic or bil


# setup temporary working directory
#------------------------------------------------------------
if [[ -e "$FORC_DIR" ]]; then
    error "$FORC_DIR already exists, removing."
    rm -rf $FORC_DIR
fi
mkdir -p $FORC_DIR
cd $FORC_DIR



# determine the actual start/end dates of the forcing files we need.
#  We are likely to need 1 day before, and 1 day after because the daily forcings
#  are usually specified at 12Z. So a forecast starting at Jan02,0Z will need the
#  Jan01 daily file as well.
#------------------------------------------------------------

# forcing start day (forc_start)
#------------------------------
fcst_start_hr=$(date "+%H" -d "$FCST_START_TIME")
forc_start=$FCST_START_TIME
if [[ $fcst_start_hr -lt $FORCING_HR ]]; then
    forc_start=$(date "+$DATE_FMT" -d "$forc_start - 1 day")
fi
forc_start=$(date "+%Y%m%d" -d "$forc_start")

# forcing end day (forc_end)
#------------------------------
fcst_end_hr=$(date "+%H" -d "$FCST_END_TIME")
forc_end=$FCST_END_TIME
if [[ $fcst_end_hr -gt $FORCING_HR ]]; then
    forc_end=$(date "+$DATE_FMT" -d "$forc_end + 1 day")
fi
forc_end=$(date "+%Y%m%d" -d "$forc_end")



echo "preparing surface forcing from $forc_start to $forc_end"
echo " forcing variables are: $forcings"
echo ""



# Create the combined control forcing file
#------------------------------------------------------------
mkdir -p ctrl
echo "Generating control forcing file..."
for f in $forcings; do
    files=()
    date_cur=$forc_start
    #TODO : make sure this can handle start/end times that aren't 00Z
    while [[ $(date -d "$date_cur" +%s) -le $(date -d "$forc_end" +%s) ]];do
	date_next=$(date "+%F" -d "$date_cur + 1 day")	
	file=${FORCING_CTRL_FILE//#var#/$f}
	file=$(date "+$file" -d "$date_cur")
	files+=("$file")
	date_cur=$date_next
    done
    ncrcat ${files[@]} ctrl/$f.nc
    ncatted -O -a axis,time,c,c,T ctrl/$f.nc
    ncatted -O -a calendar,,m,c,gregorian ctrl/$f.nc
done
echo ""



# If we are doing an ensemble run, process the ensemble perturbations
# ------------------------------------------------------------
if [[ "$ENSEMBLE" -eq 1 ]]; then

    # Create the combined forcing file for each member
    echo "Generating ensemble member forcing files..."
    for m in $mem_list; do
	d=ens/mem_$m
	mkdir -p $d
	echo " member: $m"

	for f in $forcings_ens; do
	    files=()
	    date_cur=$forc_start
	    #TODO : create a function that combines this with the previous ctrl script lines
	    while [[ $(date -d "$date_cur" +%s) -le $(date -d "$forc_end" +%s) ]];do
		date_next=$(date "+%F" -d "$date_cur + 1 day")
		file=${FORCING_ENS_FILE//#var#/$f}
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
    echo 'Generating "ens->ctrl" interpolation weights...'
    mkdir -p weights
    for f in $forcings_ens; do
	cdo gen$interp,ctrl/$f.nc ens_mean/$f.nc weights/$f.nc
    done
    echo ""

    # remap the ens means and calculate ctrl-ens_mean
    echo 'Calculating "ctrl - ens_mean"...'
    mkdir -p offset
    for f in $forcings_ens; do
	cdo sub ctrl/$f.nc -remap,ctrl/$f.nc,weights/$f.nc ens_mean/$f.nc offset/$f.nc
    done
    echo ""

    # generate the final individual ensemble forcing files
    echo "Generating final ensemble members forcings..."
    mkdir -p final
    for m in $mem_list; do
	d=final/mem_$m
	mkdir -p $d
	for f in $forcings_ens; do
	    cdo add offset/$f.nc -remap,ctrl/$f.nc,weights/$f.nc ens/mem_$m/$f.nc final/mem_$m/$f.nc
	    
	    # Certain fields need to be kept positive
	    clamp=0
	    for f2 in $clamp_positive; do
		if [[ "$f2" == "$f" ]]; then clamp=1; fi
	    done
	    if [[ "$clamp" -eq 1 ]]; then
		cdo setrtoc,-1e20,0,0 final/mem_$m/$f.nc final/mem_$m/$f.nc.2
		mv final/mem_$m/$f.nc.2 final/mem_$m/$f.nc
	    fi

	    ncatted -O -a axis,time,c,c,T final/mem_$m/$f.nc
	    ncatted -O -a calendar,,m,c,gregorian final/mem_$m/$f.nc	    
	done
	for f in $forcings_ctrl; do
	    cp ctrl/$f.nc final/mem_$m/$f.nc
	done
    done

else
    mkdir -p final
    ln -s ../ctrl final/mem_0001
fi
echo ""



# setup working directory for forecasts
#------------------------------------------------------------
echo "Setting up forecast working directories in:"
echo " $FCST_DIR"
for m in $mem_list; do
    fcst_dir=${FCST_DIR//#mem#/$m}
    rst_dir=${RST_DIR//#mem#/$m}
    if [[ -e "$fcst_dir" ]]; then
	error "$fcst_dir already exists, removing."
	rm -rf $fcst_dir
    fi
    mkdir -p $fcst_dir
    cd $fcst_dir

    mkdir -p OUTPUT
    mkdir -p RESTART
    ln -s $FORC_DIR/final/mem_$m FORC
    ln -s $ROOT_DIR/build/MOM6 .

    # Are we doing a restart? Or initializing from climatology
    # TODO: i don't like this, clean it up
    if [[ "$FCST_RESTART" -eq "-1" ]]; then    
	# start from climatology (FCST_RESTART=0) if this cycle is the first cycle AND a restart directory
	# does not already exist
	[[ "$CYCLE_START" -eq "$CYCLE_NO_Z" &&  (! -f "$rst_dir") ]] && FCST_RESTART=0 || FCST_RESTART=1
    fi

    # namelist files
    cp $EXP_DIR/config/mom/* .
    source diag_table.sh > diag_table
    source input.nml.sh > input.nml

    # static input files
    mkdir -p INPUT
    ln -s $ROOT_DIR/run/config/mom_input/* INPUT/

    # link restart files
    rst_dir=$(date "+$rst_dir" -d "$FCST_START_TIME")
    ln -s $rst_dir ./RESTART_IN
done
