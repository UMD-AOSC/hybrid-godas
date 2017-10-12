#!/bin/bash
set -e

# Hybrid-GODAS forecast run script

timing_start=$(date +%s)

#================================================================================
#================================================================================
# Set the required environment variables, along with any default values
# Any variable that doesn't have a default value will be checked 
# by this script to make sure it is defined.
v=""

v="$v PBS_NP"      # number of processors
v="$v PBS_NUM_PPN" # number ofr cores per node

# directory paths
#------------------------------
v="$v root_dir"   # Path to the top level directory for the hybrid-godas code.
v="$v work_dir"   # A temporary working directory to use when running the forecast, the location
                  # needs to be accessible from all computational nodes.
v="$v exp_dir"    # Top level directory of the experiment  
v="$v flux_cfsr_dir" # Path to the daily cfsr fluxes

# forecast time start /stop
#------------------------------
v="$v fcst_start" # Start date of forecast (YYYY-MM-DD), hour is assumed to be 00Z.
v="$v fcst_len"   # Length of forecast in days.

# saving of daily mean values (for data assimilation)
#------------------------------
v="$v fcst_diag_combine" #
v="$v fcst_diag_dir"
v="$v fcst_diag_daily"
v="$v fcst_diag_daily_int"
v="$v fcst_diag_pentad"
#v="$v fcst_dailymean_int" # interval of daily mean files to save. E.g. if 1, saves every day
                          # if 5, saves every 5th day counting back from and including the end date
#v="$v fcst_dailymean_dir" # directory to save daily mean files to

fcst_diag_combine="${fcst_diag_combine:-0}"
fcst_diag_daily="${fcst_diag_daily:-0}"
fcst_diag_daily_int="${fcst_diag_daily_int:-1}"
fcst_diag_pentad="${fcst_diag_pentad:-1}"
#if [[ "$fcst_diag_daily" -eq 0 ]]; then fcst_dailymean_dir=" "; fi

# saving of other model output (usually pentad data)
#------------------------------
v="$v fcst_save_rstyr"      # If 1, save restart files on Jan 1 of every year
#v="$v fcst_otherfiles"      # If save other output files from the model.
#v="$v fcst_otherfiles_dir"  # Directory to save other files to
v="$v fcst_maskland"        # If 1, output undergoes an extra step to have a land mask applied

fcst_save_rstyr=${fcst_save_rstyr:-1}
fcst_maskland=${fcst_maskland:-1}
#if [[ "$fcst_otherfiles" -eq 0 ]]; then fcst_otherfiles_dir=" "; fi


envvars="$v"



# ================================================================================
# ================================================================================


echo ""
echo "============================================================"
echo "   Running MOM6 forecast..."
echo "============================================================"
echo "Running on $(hostname)"

ts=$(date +%s)

PBS_NUM_PPN_M1=$(($PBS_NUM_PPN-1))

# check required environment variables, if any that dont have a default
# are not defined, exit with an error
for v in ${envvars}; do
    if [[ -z "${!v}" ]]; then echo "ERROR: env var $v not set."; exit 1; fi
    echo "  $v = ${!v}"
done
echo ""


# setup the environment
. $root_dir/config/env


# are we running a restart?
# if not the model will initialize with climatological T/S at rest
restart="r"
if [[ ! -d "$exp_dir/RESTART" ]]; then
    echo "Initializing NEW experiment without restart on $fcst_start"
    restart="n"
fi


# what are the dates we are running
fcst_end=$(date "+%Y-%m-%d" -d "$fcst_start + $fcst_len day")
echo "Running forecast from $fcst_start 00Z to $fcst_end 00Z"


# Setup the working directory
#------------------------------------------------------------
work_dir=$work_dir/fcst
rm -rf $work_dir
mkdir -p $work_dir
cd $work_dir
mkdir -p OUTPUT
mkdir -p RESTART
ln -s $root_dir/build/MOM6 .
ln -s $root_dir/build/mppnccombine .

# namelist files
cp $exp_dir/config/mom/* .
. ./diag_table.sh > diag_table
. ./input.nml.sh > input.nml

# static input files
mkdir -p INPUT
ln -s $root_dir/run/config/mom_input/* INPUT/

# restart files
if [[ "$restart" = 'r' ]]; then
    ln -s $exp_dir/RESTART/* INPUT/
fi

timing_setup=$(( $(date +%s) - $ts ))


# Prepare the forcing files
#------------------------------------------------------------
# forcing start needs to be 1 day earlier because forcing is
# centered at 12Z, and fcst_start/fcst_end are at 0Z
ts=$(date +%s)

mkdir -p FORC
cd FORC
forc_start=$(date "+%Y-%m-%d" -d "$fcst_start - 1 day")
forc_end=$fcst_end
(. $root_dir/tools/prep_forcing.sh $forc_start $forc_end)
cd ..

timing_obprep=$(( $(date +%s) - $ts ))


# run the forecast
#------------------------------------------------------------
ts=$(date +%s)

aprun -n $PBS_NP ./MOM6
echo "exit code $?"
if [[ $? -gt 0 ]]; then
    echo "ERROR running forecast."
    exit 1
fi

timing_fcst=$(( $(date +%s) - $ts ))


# Combine the output files
# TODO: really shouldn't be done on the computational nodes
# TODO: should also get the DA code to take in bkg files in patches
#------------------------------------------------------------
ts=$(date +%s)

pfx=$(date "+%Y%m%d" -d "$fcst_start")
if [[ "$fcst_diag_combine" -gt 0 ]]; then
    echo "combining MOM output patches"
    
    f=$(ls $pfx.*.nc.???? | rev | cut -d. -f 2- | uniq | rev)
    for f2 in $f
    do
	echo $f2
#	aprun -n 1 -cc depth -d $PBS_NUM_PPN_M1 ./mppnccombine -m $f2 $f2.???? &  
	aprun -n 1 ./mppnccombine -m $f2 $f2.???? &  
    done
    wait
fi

timing_combine=$(( $(date +%s) - $ts ))


# Move the output files
#------------------------------------------------------------
ts=$(date +%s)

out_dir=$(date -d $fcst_start "+$fcst_diag_dir")
mkdir -p $out_dir
if [[ "$fcst_diag_combine" -gt 0 ]]; then
    mv $work_dir/$pfx.*.nc $out_dir/
else
    mv $work_dir/$pfx.*.nc* $out_dir/
fi


# move the restart files
#------------------------------------------------------------
rm -rf $exp_dir/RESTART_old
if [[ -d $exp_dir/RESTART && "$fcst_save_rstyr" -gt 0 ]]; then 
    # save a permanent backup of restart files once a year
    # check to see if the year of the previous run would have been
    # different than the year of this run
    y1=$(date "+%Y" -d "$fcst_start")
    y2=$(date "+%Y" -d "$fcst_start -$fcst_len day")
    if [[ "$y1" -ne "$y2" ]]; then
	d=$(date "+%Y%m%d" -d "$fcst_start")
	echo "Saving backup of restart files to ./RESTART_SAVE/$d ..."
	mkdir -p $exp_dir/RESTART_SAVE
	mv $exp_dir/RESTART $exp_dir/RESTART_SAVE/$d
    fi
fi
if [[ -d $exp_dir/RESTART ]]; then
    # otherwise, move the old restart folder as a temporary backup
    mv $exp_dir/RESTART $exp_dir/RESTART_old; 
fi
# move over the new restart
mv $work_dir/RESTART $exp_dir/RESTART

timing_mv=$(( $(date +%s) - $ts ))


#------------------------------------------------------------


# update the "last_date" file
echo $fcst_end > $exp_dir/last_date_fcst


# clean up working directory
rm -rf $work_dir

timing_total=$(( $(date +%s) - $timing_start ))

echo ""
echo "============================================================"
echo " Forecast timing (seconds)"
echo "============================================================"
echo " setup           : $timing_setup"
echo " sbc prep        : $timing_obprep"
echo " forecast        : $timing_fcst"
echo " output combine  : $timing_combine"
echo " output move     : $timing_mv"
echo "           total : $timing_total"
