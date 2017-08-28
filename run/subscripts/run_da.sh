#!/bin/bash
set -e


# Hybrid-GODAS data assimilation step scripts

timing_start=$(date +%s)


#================================================================================
#================================================================================
# Set the required environment variables, along with any default values.
# Any variable that doesn't have a default value will be checked by this script
# to make sure it has been defined
v=""

v="$V PBS_NP"     # number of processors given by job scheduler
v="$v da_nproc"   # number of processors we actually want to use
v="$v da_threads" # number of thread to use for the non-mpi openmp jobs
v="$v da_skip"    # If = 1, skip the 3DVar code (but still do obsop for O-F stats)

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
ln -s $root_dir/build/* .
cp $exp_dir/config/3dvar/* .

mkdir INPUT
cd INPUT
ln -s $root_dir/DATA/grid/ocean_geometry.nc grid.nc
ln -s $root_dir/DATA/grid/Vertical_coordinate.nc vgrid.nc
ln -s $root_dir/DATA/grid/coast_dist.nc .
ln -s $exp_dir/bkg/${da_date_ana}.nc bkg.nc
cd ..

# make sure that unless otherwise specified, all programs are single threaded
export OMP_NUM_THREADS=1

#------------------------------------------------------------
#------------------------------------------------------------
echo ""
echo "============================================================"
echo "Vertical Localization distance"
echo ""
ts=$(date +%s)
OMP_NUM_THREADS=$da_threads aprun -cc depth -n 1 -d $da_threads time  ./vtloc
timing_vtloc=$(( $(date +%s) - $ts ))



#------------------------------------------------------------
#------------------------------------------------------------
echo ""
echo "============================================================"
echo "background variance"
echo ""
ts=$(date +%s)
OMP_NUM_THREADS=$da_threads aprun -cc depth -n 1 -d $da_threads time ./bgvar
timing_bgvar=$(( $(date +%s) - $ts ))



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
ts=$(date +%s)
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
    obfile=$da_prof_dir/$(date "+%Y/%Y%m%d" -d $fdate)    
    if [[ "$da_prof_legacy" -eq 1 ]]; then
	# are use using the legacy GODAS profiles, or 
	# new ones (not yet implemented
	obsprep_exec=obsprep_insitu_legacy
    else
	obsprep_exec=obsprep_insitu
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
timing_obsprep=$(( $(date +%s) - $ts ))


# Obseration operators
#------------------------------------------------------------
ts=$(date +%s)
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
timing_obsop=$(( $(date +%s) - $ts ))


# combine all obs
ts=$(date +%s)
cd $work_dir
echo ""
echo "============================================================"
echo "Combining all obs into single file..."
ncrcat --no_tmp_fl obsop_????????/obsop.nc INPUT/obs.nc
timing_obscmb=$(( $(date +%s) - $ts ))


#------------------------------------------------------------
# 3dvar
#------------------------------------------------------------
if [ $da_skip -eq 0 ]; then
    ts=$(date +%s)
    echo ""
    echo "============================================================"
    echo "Running 3DVar..."
    echo "============================================================"
    aprun -n $da_nproc ./3dvar
    timing_3dvar=$(( $(date +%s) - $ts ))

    # update the restart
    ts=$(date +%s)
    echo ""
    echo "Updating the restart files..."
#    time $root_dir/tools/update_restart.py output.nc $exp_dir/RESTART
    ln -s $exp_dir/RESTART .
    aprun -n 1 ./update_restart
    timing_restart=$(( $(date +%s) - $ts ))

    ts=$(date +%s)
    # move da output
    echo "Moving AI file..."
    d=$exp_dir/output/ana_inc/$date_dir/${da_date_ana:0:4}
    mkdir -p $d
    mv ana_inc.nc $d/${da_date_ana}.nc

    d=$exp_dir/diag/misc/$date_dir/${da_date_ana:0:4}
    mkdir -p $d
    mv ana_diag.nc $d/${da_date_ana}.nc
    
    # vtloc file
    d=$exp_dir/diag/vtloc/$date_dir/${da_date_ana:0:4}
    mkdir -p $d
    mv vtloc.nc $d/${da_date_ana}.nc

    #bgvar file
    d=$exp_dir/diag/bgvar/$date_dir/${da_date_ana:0:4}
    mkdir -p $d
    mv bgvar.nc $d/${da_date_ana}.nc

    # background files
    d=$exp_dir/output/bkg_inst/$date_dir/${da_date_ana:0:4}
    mkdir -p $d
    mv $exp_dir/bkg/${da_date_ana}.nc $d/${da_date_ana}.nc

    # delete background files    
    echo "Deleting background..."
    rm $exp_dir/bkg/* 

    timing_mvfiles=$(( $(date +%s) - $ts ))

fi



#------------------------------------------------------------
# post processing
#------------------------------------------------------------

# O-B
echo ""
echo "Creating observation space statistics..."
date_dir=${da_date_ana:0:4}
d=$exp_dir/output/bkg_omf/$date_dir
mkdir -p $d
mv $work_dir/INPUT/obs.nc  $d/${da_date_ana}.nc
mv $work_dir/obs.varqc.nc  $d/${da_date_ana}.varqc.nc


# clean up
rm -rf $work_dir

timing_final=$(( $(date +%s) - $timing_start ))



#Print out timing statistics summary
echo ""
echo "============================================================"
echo "Data assimilation Timing (seconds)"
echo "============================================================"
echo " vertical localization distance (vtloc): $timing_vtloc"
echo " Background error variance (bgvar)     : $timing_bgvar"
echo " observation preparation (obsprep)     : $timing_obsprep"
echo " observation operator (obsop)          : $timing_obsop"
echo " observation file combination          : $timing_obscmb"
echo " 3dvar solver (3dvar)                  : $timing_3dvar"
echo " update restart file                   : $timing_restart"
echo " move output files                     : $timing_mvfiles"
echo "                               Total   : $timing_final"