#!/bin/bash
set -e

obs_sst_dir=$root_dir/DATA/obs/sst_pathfinder
obs_prf_dir=$root_dir/DATA/obs/profile

echo ""
echo "============================================================"
echo "   Running Data assimilation"
echo "============================================================"

# check the required environment variables
envvars="root_dir work_dir exp_dir date_ana date_obs_end date_obs_start obs_sst_dir da_nproc da_skip"
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
cp $exp_dir/config/3dvar/* .

mkdir INPUT
cd INPUT
ln -s $root_dir/DATA/grid/ocean_geometry.nc grid.nc
ln -s $root_dir/DATA/grid/Vertical_coordinate.nc vgrid.nc
ln -s $root_dir/DATA/grid/coast_dist.nc .
ln -s $root_dir/DATA/grid/bgvar.nc .
ln -s $exp_dir/bkg/${date_ana}.nc bkg.nc
ln -s $exp_dir/bkg/${date_ana}_da.nc bkg_da.nc

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
fdate=$date_obs_end
while [ $(date -d $fdate +%s) -ge $(date -d $date_obs_start +%s) ]
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
    obfile=$obs_prf_dir/$(date "+%Y/%Y%m/%Y%m%d" -d $fdate).nc    
    if [ -f  $obfile ]; then
	echo "  obsprep_insitu $fdate"
    	export obsfile_in=$obfile
    	export obsfile_out=obprep.insitu.nc
    	source ../obsop.nml.sh > obsprep_insitu.nml
	aprun -n 1 ../obsprep_insitu > obsprep_insitu.log &
    fi    

    # SST obs
    obfile=$obs_sst_dir/$(date "+%Y/%Y%m/%Y%m%d" -d $fdate).nc    
    if [ -f $obfile ]; then
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
fdate=$date_obs_end
while [ $(date -d $fdate +%s) -ge $(date -d $date_obs_start +%s) ]
do
    d=$work_dir/obsop_$fdate
    cd $d
    echo "  concatenating $fdate..."
    ncrcat --no_tmp_fl obprep.*.nc obprep.nc &
    fdate=$(date "+%Y%m%d" -d "$fdate - 1 day")    
done
wait
fdate=$date_obs_end
while [ $(date -d $fdate +%s) -ge $(date -d $date_obs_start +%s) ]
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

    # update the restart
    echo ""
    echo "Updating the restart files..."
    $root_dir/tools/update_restart.py output.nc $exp_dir/RESTART

    # move da output
    echo "Moving AI file..."
    d=$exp_dir/diag/ana_inc/$date_dir/${date_ana:0:4}
    mkdir -p $d
    mv output.nc $d/${date_ana}.nc

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
date_dir=${date_ana:0:4}
d=$exp_dir/diag/OmF/$date_dir
mkdir -p $d
mv $work_dir/INPUT/obs.nc  $d/${date_ana}.nc

# clean up
rm -rf $work_dir