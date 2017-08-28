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

v="$v PBS_NP"   # number of processors

# directory paths
#------------------------------
v="$v root_dir"   # Path to the top level directory for the hybrid-godas code.
v="$v work_dir"   # A temporary working directory to use when running the forecast, the location
                  # needs to be accessible from all computational nodes.
v="$v exp_dir"    # Top level directory of the experiment  

# forecast time start /stop
#------------------------------
v="$v fcst_start" # Start date of forecast (YYYY-MM-DD), hour is assumed to be 00Z.
v="$v fcst_len"   # Length of forecast in days.

# saving of daily mean values (for data assimilation)
#------------------------------
v="$v fcst_dailymean"     # If 1, save the daily mean files required by the DA step, (Default: 0)
v="$v fcst_dailymean_int" # interval of daily mean files to save. E.g. if 1, saves every day
                          # if 5, saves every 5th day counting back from and including the end date
v="$v fcst_dailymean_dir" # directory to save daily mean files to

fcst_dailymean="${fcst_dailymean:-0}"
fcst_dailymean_int=${fcst_dailymean_int:-1}
if [[ "$fcst_dailymean" -eq 0 ]]; then fcst_dailymean_dir=""; fi

# saving of other model output (usually pentad data)
#------------------------------
v="$v fcst_save_rstyr"      # If 1, save restart files on Jan 1 of every year
v="$v fcst_otherfiles"      # If save other output files from the model.
v="$v fcst_otherfiles_dir"  # Directory to save other files to
v="$v fcst_maskland"        # If 1, output undergoes an extra step to have a land mask applied

fcst_save_rstyr=${fcst_save_rstyr:-1}
fcst_maskland=${fcst_maskland:-1}
if [[ "$fcst_otherfiles" -eq 0 ]]; then fcst_otherfiles_dir=""; fi


envvars="$v"



# ================================================================================
# ================================================================================


echo ""
echo "============================================================"
echo "   Running MOM6 forecast..."
echo "============================================================"

ts=$(date +%s)

# check required environment variables
for v in ${envvars}; do
    if [ -z "${!v}" ]; then echo "ERROR: env var $v not set."; exit 1; fi
    echo "  $v = ${!v}"
done
echo ""


# setup the environment
. $root_dir/config/env


# are we running a restart?
restart="r"
if [ ! -d "$exp_dir/RESTART" ]; then
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

# namelist files
cp $exp_dir/config/mom/* .
. ./diag_table.sh > diag_table
. ./input.nml.sh > input.nml

# static input files
mkdir -p INPUT
ln -s $root_dir/run/config/mom_input/* INPUT/

# restart files
if [ "$restart" = 'r' ]; then
    ln -s $exp_dir/RESTART/* INPUT/
    # save a backup of restart files once a year
    # check to see if the year of the previous run would have been
    # different than the year of this run
    if [ "$fcst_save_rstyr" -gt 0 ]; then
	y1=$(date "+%Y" -d "$fcst_start")
	y2=$(date "+%Y" -d "$fcst_start -$fcst_len day")
	if [ "$y1" -ne "$y2" ]; then
	    d=$(date "+%Y%m%d" -d "$fcst_start")
	    echo "Saving backup of restart files to ./RESTART_SAVE/$d ..."
	    mkdir -p $exp_dir/RESTART_SAVE
	    cp -r $exp_dir/RESTART $exp_dir/RESTART_SAVE/$d
	fi
    fi
fi
timing_setup=$(( $(date +%s) - $ts ))

# Prepare the forcing files
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
if [ $? -gt 0 ]; then
    echo "ERROR running forecast."
    exit 1
fi
timing_fcst=$(( $(date +%s) - $ts ))


ts=$(date +%s)
# Move the output files needed for DA
#------------------------------------------------------------
if [ "$fcst_dailymean" -gt 0 ]; then
    echo "Moving daily mean files..."
    fdate=$(date "+%Y%m%d" -d "$fcst_end - 1 day")
    pfx=$work_dir/$(date "+%Y%m%d" -d "$fcst_start")
    while [ $(date -d $fdate +%s) -ge $(date -d $fcst_start +%s) ]
    do
	out_dir=$(date -d $fdate "+$fcst_dailymean_dir")
	mkdir -p $out_dir
	
	src_file=$pfx.ocean_daily_$(date "+%Y_%m_%d" -d "$fdate").nc
	dst_file=$out_dir/$(date "+%Y%m%d" -d "$fdate").nc
	mv $src_file $dst_file
	
	fdate=$(date "+%Y%m%d" -d "$fdate - $fcst_dailymean_int day")
    done
fi


# move any other user defined files that might be there
if [ "$fcst_otherfiles" -gt 0 ]; then
    pfx=$(date "+%Y%m%d" -d "$fcst_start")

    # mask the land on the files first
   if [ "$fcst_maskland" = 1 ]; then
	echo "Masking land of output files..."
	$root_dir/tools/mask_output.py $pfx.ocean_*.nc
   fi

    # move the files
    echo "Moving other output files..."
    cd $work_dir
    fdate=$(date "+%Y%m%d" -d "$fcst_end - 1 day")
    for f in $pfx.*.nc
    do
	echo "Moving $f"
	ofdate=$(echo "${f: -13:10}" | tr _ -)
	ofname="${f: 9:$((${#f}-23))}"

 	out_dir=$(date -d $ofdate "+$fcst_otherfiles_dir")
 	mkdir -p $out_dir

 	dst_file=$out_dir/$ofname.$(date "+%Y%m%d" -d "$ofdate").nc
 	mv $f $dst_file
    done
fi

# move the restart files
rm -rf $exp_dir/RESTART_old
if [ -d $exp_dir/RESTART ]; then mv $exp_dir/RESTART $exp_dir/RESTART_old; fi
mv $work_dir/RESTART $exp_dir/RESTART


# update the "last_date" file
echo $fcst_end > $exp_dir/last_date_fcst
timing_mv=$(( $(date +%s) - $ts ))


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
echo " output move     : $timing_mv"
echo "           total : $timing_total"