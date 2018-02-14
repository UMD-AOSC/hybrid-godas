#!/bin/bash
set -e
cat << \#\#
#================================================================================
#================================================================================
# NCEP Hybrid-GODAS  -  cycle.post.sh
#   Post processing of data assimilation / forecasting results
#================================================================================
##
# Required environment variables:
 envar=()
 envar+=("ROOT_GODAS_DIR")
 envar+=("ROOT_EXP_DIR")
 envar+=("JOB_WORK_DIR")
 envar+=("ENS_LIST")
 envar+=("FCST_RST_TIME")
 envar+=("SAVE_ENS_DAILY")
   SAVE_ENS_DAILY=${SAVE_ENS_DAILY:-0}
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


tools_dir=$ROOT_GODAS_DIR/tools/postproc
truncations=SST_min/2,rhopot0/4,ssh/4,temp/2,salt/2
truncations_hi=Temp/3,Salt/3
clvl=8

# ensemble files
#------------------------------------------------------------
echo "Saving ensemble data..."
for ens in ${ENS_LIST[@]}; do
    echo ""
    echo " ensemble member: $ens"
    
    #daily background
    if [[ $SAVE_ENS_DAILY == 1 ]]; then
	src_dir=$JOB_WORK_DIR/da.prep/bkg/mem_$ens
	for f in $src_dir/*.nc; do
	    f2=${f##*/}
	    echo "  daily background: $f2"
	    out_dir=$ROOT_EXP_DIR/output/bkg_daily/$ens/${f2:0:4}/
	    mkdir -p $out_dir
	    $tools_dir/compress.py -compression 6\
                -lsd truncations -remove rhopot0 $f $out_dir/$f2    
	done	
    fi

    # omf
    src_dir=$JOB_WORK_DIR/da.prep/omf/mem_$ens
    for f in $src_dir/*.nc; do
	f2=${f##*/}
	echo "  daily OmF: $f2"
	out_dir=$ROOT_EXP_DIR/output/bkg_OmF/$ens/${FCST_RST_TIME:0:4}/${FCST_RST_TIME}
	mkdir -p $out_dir
	$tools_dir/compress.py -compression 6 $f $out_dir/$f2    
    done	
done



# DA mean/spread
# ------------------------------------------------------------
for f in ana_mean bkg_mean; do
    src=$JOB_WORK_DIR/da.letkf/OUTPUT/$f.nc
    out_dir=$ROOT_EXP_DIR/output/${f//_//}/${FCST_RST_TIME:0:4}/
    dst=$out_dir/${FCST_RST_TIME}.nc
    mkdir -p $out_dir
    $tools_dir/mask.py $src $dst.2 -geomFile $ROOT_GODAS_DIR/DATA/grid/ocean_geometry.nc -vertFile $ROOT_GODAS_DIR/DATA/grid/Vertical_coordinate.nc
#    $tools_dir/compress.py -compression 6 -lsd $truncations_hi $dst.2 $dst
    $tools_dir/compress.py -compression $clvl -pack Temp/0/0.01,Salt/0/0.01 $dst.2 $dst
    rm $dst.2
done

for f in ana_sprd bkg_sprd; do
    src=$JOB_WORK_DIR/da.letkf/OUTPUT/$f.nc
    out_dir=$ROOT_EXP_DIR/output/${f//_//}/${FCST_RST_TIME:0:4}/
    dst=$out_dir/${FCST_RST_TIME}.nc
    mkdir -p $out_dir
    $tools_dir/mask.py $src $dst.2 -geomFile $ROOT_GODAS_DIR/DATA/grid/ocean_geometry.nc -vertFile $ROOT_GODAS_DIR/DATA/grid/Vertical_coordinate.nc
#    $tools_dir/compress.py -compression 6 -lsd $truncations_hi $dst.2 $dst
    $tools_dir/compress.py -compression $clvl -pack Temp/0/0.001,Salt/0/0.001 $dst.2 $dst
    rm $dst.2
done

# mask/regrid/pack/compress the following:
#--------------------------------------------------------------------------------

# analysis/background mean/spread

# each ensemble member



# move the following to their final locations
#--------------------------------------------------------------------------------
