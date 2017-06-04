#!/bin/bash

obs_sst_dir=$root_dir/DATA/obs/sst_pathfinder
obs_prf_dir=$root_dir/DATA/obs/profile
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
ln -s $root_dir/build/obsop .
ln -s $root_dir/build/obsprep* .
cp $exp_dir/config/3dvar/* .

mkdir INPUT
cd INPUT
ln -s $root_dir/DATA/grid/ocean_geometry.nc grid.nc
ln -s $root_dir/DATA/grid/Vertical_coordinate.nc vgrid.nc
ln -s $root_dir/DATA/grid/coast_dist.nc .
ln -s $exp_dir/bkg/${date_ana}.nc bkg.nc
ln -s $exp_dir/bkg/${date_ana}_da.nc bkg_da.nc
cd ..

# run observation prep
toffset=0
fdate=$date_obs_end
while [ $(date -d $fdate +%s) -ge $(date -d $date_obs_start +%s) ]
do
    echo ""
    echo "============================================================"
    echo " date=$fdate, time offset=$toffset "
    
    export obsop_hr=$toffset

    ln -sf $exp_dir/bkg/$fdate.nc obsop_bkg.$fdate.nc
    ln -sf $exp_dir/bkg/${fdate}_da.nc obsop_bkg_da.$fdate.nc
    export fdate
    
    # conventional obs
    obfile=$obs_prf_dir/$(date "+%Y/%Y%m/%Y%m%d" -d $fdate).nc    
    if [ -f $obfile ]; then
    	export obsfile_in=$obfile
    	export obsfile_out=obprep.$fdate.insitu.nc
    	source obsop.nml.sh > obsprep_insitu.nml
    	aprun obsprep_insitu
    fi

    # SST obs
    obfile=$obs_sst_dir/$(date "+%Y/%Y%m/%Y%m%d" -d $fdate).nc    
    if [ -f $obfile ]; then
    	export obsfile_in=$obfile
    	export obsfile_out=obprep.$fdate.sst.nc
    	source obsop.nml.sh > obsprep_sst.nml
    	aprun obsprep_sst
    fi

    # combine
    echo ""
    echo "------------------------------------------------------------"
    echo " Combining all obs in single day..."
    echo "------------------------------------------------------------"
    ncrcat obprep.$fdate.*.nc obprep.$fdate.nc

    # observation operator
    export obsfile_in=obprep.$fdate.nc
    export obsfile_out=obsop.$fdate.nc
    source obsop.nml.sh > obsop.nml
    aprun obsop

    fdate=$(date "+%Y%m%d" -d "$fdate - 1 day")
    toffset=$(($toffset -24))
done

ncrcat obsop.*.nc INPUT/obs.nc

# 3dvar
aprun -n $da_nproc 3dvar

# update the restart
$root_dir/tools/update_restart.py output.nc $exp_dir/RESTART

# create the analysis file
cp $exp_dir/bkg/${date_ana}.nc $exp_dir/ana/
$root_dir/tools/update_bkg.py output.nc $exp_dir/ana/${date_ana}.nc