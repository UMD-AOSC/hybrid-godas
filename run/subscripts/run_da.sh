#!/bin/bash
set -e

# Hybrid-GODAS data assimilation step scripts

#================================================================================
#================================================================================
# Set the required environment variables, along with any default values.
# Any variable that doesn't have a default value will be checked by this script
# to make sure it has been defined
v=""

v="$V PBS_NP"    # number of processors given by job scheduler
v="$v da_nproc"  # number of processors we actually want to use
v="$v da_skip"   # If = 1, skip the 3DVar code (but still do obsop for O-F stats)

# directory paths
#------------------------------
v="$v root_dir"    # Path to the top level directory for the hybrid-godas code.
v="$v work_dir"    # A temporary working directory to use when running the forecast
                   # the location needs to be accessible from all computational nodes.
v="$v exp_dir"     # Top level directory of the experiment


# Dates (all dates in YYYYMMDD format)
#------------------------------
v="$v da_date_ana"       # The date on which the analysis is centered.
v="$v da_date_ob_start"  # The start date of observation window
v="$v da_date_ob_end"    # The end date of observation window


# Observation types
#------------------------------
# da_sst_use:     If = 1, use SST observations
da_sst_use=${da_sst_use:-1}
v="$v da_sst_use"   

# da_sst_dir:     Directory to AVHRR SST observation
da_sst_dir=${da_sst_dir:-$root_dir/DATA/obs/sst_pathfinder}
v="$v da_sst_dir"   

# da_prof_use:    If = 1, use profile observations
da_prof_use=${da_prof_use:-1}
v="$v da_prof_use"  

# da_prof_dir:    Directory to T/S profile observations
da_prof_dir=${da_prof_dir:-$root_dir/DATA/obs/profile}
v="$v da_prof_dir"  

# da_prof_legacy: If = 1 , use "dave's obs" from legacy GODAS
da_prof_legacy=${da_prof_legacy:-0}
v="$v da_prof_legacy"


envvars="$v"


#================================================================================
#================================================================================

echo ""
echo "============================================================"
echo "   Running Data assimilation"
echo "============================================================"


# check the required environment variables
for v in ${envvars}; do
    if [ -z "${!v}" ]; then echo "ERROR: env var $v not set."; exit 1; fi
    echo "  $v = ${!v}"
done
echo ""

# setup the environment
. $root_dir/config/env

# setup the working directory
work_dir=$work_dir/3dvar
rm -rf $work_dir
mkdir -p $work_dir
cd $work_dir
ln -s $root_dir/build/3dvar .
ln -s $root_dir/build/obsop .
ln -s $root_dir/build/obsprep* .
ln -s $root_dir/build/bgvar .
cp $exp_dir/config/3dvar/* .

mkdir INPUT
cd INPUT
ln -s $root_dir/DATA/grid/ocean_geometry.nc grid.nc
ln -s $root_dir/DATA/grid/Vertical_coordinate.nc vgrid.nc
ln -s $root_dir/DATA/grid/coast_dist.nc .
ln -s $root_dir/DATA/grid/clim_var.nc .
ln -s $exp_dir/bkg_errvar/bgvar.nc .
ln -s $exp_dir/bkg/${da_date_ana}.nc bkg.nc
cd ..

#------------------------------------------------------------
# Observation prep
#------------------------------------------------------------
echo ""
echo "============================================================"
echo "Observation processing"
echo ""
echo "Preparing Observations (SST/insitu)"


#run the obs prep programs
#------------------------------------------------------------
# TODO: move everythin to work on /dev/shm for speed improvement ?
toffset=0
fdate=$da_date_ob_end
while [ $(date -d $fdate +%s) -ge $(date -d $da_date_ob_start +%s) ]
do
    # make directory
    d=$work_dir/obsop_$fdate
    mkdir -p $d
    cd $d
    export obsop_hr=$toffset
    export fdate

    # link required files    
    ln -s ../INPUT .
    ln -sf $exp_dir/bkg/$fdate.nc obsop_bkg.nc
    ln -s $root_dir/build/gsw_data_v3_0.nc .

    # conventional obs
    if [[ "$da_prof_legacy" -eq 1 ]]; then
	# are use using the legacy GODAS profiles, or 
	# new ones (not yet implemented
	obsprep_exec=obsprep_insitu_legacy
	obfile=$da_prof_dir/$(date "+%Y/%Y%m%d" -d $fdate)    
    else
	obsprep_exec=obsprep_insitu
	obfile=$da_prof_dir/$(date "+%Y/%Y%m/%Y%m%d" -d $fdate).nc
    fi

    if [[ ("$da_prof_use" -eq 1) ]]; then #&& (-f  $obfile) ]]; then
	echo "  obsprep_insitu $fdate"
    	export obsfile_in=$obfile
    	export obsfile_out=obprep.insitu.nc
    	source ../obsop.nml.sh > obsprep.nml
#    	source ../obsop.nml.sh > obsprep_insitu.nml	
	aprun -n 1 ../$obsprep_exec > obsprep_insitu.log &
    fi    

    # SST obs
    obfile=$da_sst_dir/$(date "+%Y/%Y%m/%Y%m%d" -d $fdate).nc    
    if [[ ("$da_sst_use" -eq 1) &&  (-f $obfile) ]]; then
	echo "  obsprep_sst    $fdate"
    	export obsfile_in=$obfile
    	export obsfile_out=obprep.sst.nc
    	source ../obsop.nml.sh > obsprep_sst.nml
    	aprun -n 1 ../obsprep_sst > obsprep_sst.log &
    fi

    # obsop file (used in later step)
    export obsfile_in=obprep.nc
    export obsfile_out=obsop.nc
    source ../obsop.nml.sh > obsop.nml

    fdate=$(date "+%Y%m%d" -d "$fdate - 1 day")    
    toffset=$(($toffset -24))
done
echo "Waiting for completetion..."
wait
cd $work_dir
cat obsop_????????/obsprep*.log


# Obseration operators
#------------------------------------------------------------
echo ""
echo "============================================================"
echo "Daily observation operator"
fdate=$da_date_ob_end
while [ $(date -d $fdate +%s) -ge $(date -d $da_date_ob_start +%s) ]
do
    d=$work_dir/obsop_$fdate
    cd $d
    echo "  concatenating $fdate..."
    ncrcat --no_tmp_fl obprep.*.nc obprep.nc &
    fdate=$(date "+%Y%m%d" -d "$fdate - 1 day")    
done
wait
fdate=$da_date_ob_end
while [ $(date -d $fdate +%s) -ge $(date -d $da_date_ob_start +%s) ]
do
    d=$work_dir/obsop_$fdate
    cd $d
    echo "  obsop for $fdate..."
    aprun ../obsop > obsop.log &
    fdate=$(date "+%Y%m%d" -d "$fdate - 1 day")    
done
echo "waiting for completion..."
wait
cd $work_dir
cat obsop_????????/obsop.log


# combine all obs
cd $work_dir
echo ""
echo "============================================================"
echo "Combining all obs into single file..."
ncrcat --no_tmp_fl obsop_????????/obsop.nc INPUT/obs.nc



#------------------------------------------------------------
# 3dvar
#------------------------------------------------------------
if [ $da_skip -eq 0 ]; then
    echo ""
    echo "============================================================"
    echo "Running 3DVar..."
    echo "============================================================"
    aprun -n $da_nproc 3dvar

    # calculate updated bgvar for next time
    source namelist.bgvar.sh > namelist.bgvar
    aprun -n 1 bgvar
    mv $exp_dir/bkg_errvar/bgvar.nc $exp_dir/bkg_errvar/${da_date_ana}.nc
    mv bgvar.nc $exp_dir/bkg_errvar/bgvar.nc

    # update the restart
    echo ""
    echo "Updating the restart files..."
    $root_dir/tools/update_restart.py output.nc $exp_dir/RESTART

    # move da output
    echo "Moving AI file..."
    d=$exp_dir/diag/ana_inc/$date_dir/${da_date_ana:0:4}
    mkdir -p $d
    mv output.nc $d/${da_date_ana}.nc

    # delete background files
    echo "Deleting background..."
    rm $exp_dir/bkg/* 

fi



#------------------------------------------------------------
# post processing
#------------------------------------------------------------

# O-B
echo ""
echo "Creating observation space statistics..."
date_dir=${da_date_ana:0:4}
d=$exp_dir/diag/OmF/$date_dir
mkdir -p $d
mv $work_dir/INPUT/obs.nc  $d/${da_date_ana}.nc

# clean up
rm -rf $work_dir