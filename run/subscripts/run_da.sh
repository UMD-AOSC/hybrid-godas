#!/bin/bash

obs_sst_dir=$root_dir/DATA/obs/sst_pathfinder


echo ""
echo "============================================================"
echo "   Running Data assimilation"
echo "============================================================"

# check the required environment variables
envvars="root_dir work_dir exp_dir date_ana date_obs_end date_obs_start obs_sst_dir da_nproc"
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
ln -s $root_dir/build/obsop_sst .
cp $root_dir/run/config/3dvar/* .

mkdir INPUT
cd INPUT
ln -s $root_dir/DATA/grid/ocean_geometry.nc grid.nc
ln -s $root_dir/DATA/grid/Vertical_coordinate.nc vgrid.nc
ln -s $root_dir/DATA/grid/coast_dist.nc .
ln -s $exp_dir/bkg/${date_ana}.nc bkg.nc
ln -s $exp_dir/bkg/${date_ana}_da.nc bkg_da.nc
cd ..

# run observation operators
toffset=0
fdate=$date_obs_end
while [ $(date -d $fdate +%s) -ge $(date -d $date_obs_start +%s) ]
do
    echo ""
    echo "============================================================"
    echo " date=$fdate, time offset=$toffset "

    export obsop_hr=$toffset
    source obsop.nml.sh > obsop.nml

    ln -sf $exp_dir/bkg/$fdate.nc obsop_bkg.nc

    obfile=$obs_sst_dir/$(date "+%Y/%Y%m/%Y%m%d" -d $fdate).nc
    echo $obfile
    if [ -f $obfile ]; then
	ln -sf $obfile obsin.nc
        aprun ./obsop_sst
	ncks --mk_rec_dmn obs obsout.nc obs.$toffset.nc
    fi

    fdate=$(date "+%Y%m%d" -d "$fdate - 1 day")
    toffset=$(($toffset -24))
done

rm obsin.nc
rm obsout.nc
ncrcat obs.*.nc INPUT/obs.nc

# 3dvar
aprun -n $da_nproc 3dvar

# update the restart
$root_dir/tools/update_restart.py output.nc $exp_dir/RESTART

# create the analysis file
cp $exp_dir/bkg/${date_ana}.nc $exp_dir/ana/
$root_dir/tools/update_bkg.py output.nc $exp_dir/ana/${date_ana}.nc