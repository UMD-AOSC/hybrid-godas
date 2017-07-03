#!/bin/bash
# TODO, list the env vars it is expecting

# required environment variables
#------------------------------------------------------------
# root_dir            =
# work_dir            =
# exp_dir             =
#
# fcst_start          = start date of forecast (YYYY-MM-DD), hour is assumed to be 00Z
# fcst_len            = length of forecast in days

# fcst_dailymean      = if 1, save the daily mean files
# fcst_dailymean_da   = if 1, the optional DA required daily mean fields are saved as well
# fcst_dailymean_int  = interval of daily mean files to save. E.g. if 1, saves every day
#                     = if 5, saves every 5th day counting back from and including the end date
# fcst_dailymean_dir  = directory to save daily mean files to

# fcst_otherfiles     = if 1, indicates other "ocean_*.nc" files in the output that are to be saved
# fcst_otherfiles_dir = directory to save other files to


echo ""
echo "============================================================"
echo "   Running MOM6 forecast..."
echo "============================================================"

# check required environment variables
fcst_maskland=${fcst_maskland:-1}
envvars="root_dir work_dir exp_dir fcst_start fcst_len"
envvars="$envvars fcst_dailymean fcst_dailymean_da fcst_dailymean_int fcst_dailymean_dir"
envvars="$envvars fcst_otherfiles fcst_otherfiles_dir"
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
fi

# Prepare the forcing files
# forcing start needs to be 1 day earlier because forcing is
# centered at 12Z, and fcst_start/fcst_end are at 0Z
mkdir -p FORC
cd FORC
forc_start=$(date "+%Y-%m-%d" -d "$fcst_start - 1 day")
forc_end=$fcst_end
(. $root_dir/tools/prep_forcing.sh $forc_start $forc_end)
cd ..


# run the forecast
#------------------------------------------------------------
aprun -n $PBS_NP MOM6
echo "exit code $?"
if [ $? -gt 0 ]; then
    echo "ERROR running forecast."
    exit 1
fi


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

# clean up working directory
rm -rf $work_dir