#!/bin/bash
set -e
cat << \#\#

#================================================================================
#================================================================================
# NCEP Hybrid-GODAS  -  da.update.sh
#   Update restart file with the analysis from LETKF and/or 3DVAR
#================================================================================
##
#
# Prerequisites:
#
# Results:
#
# TODO: 
#  * replace EXP_DIR to allow explicit definition of where each file goes
#  * allow rst_dir to be configurable
#  * make this work for hybrid
#  * allow this to be run in parallel
#  * move the full restart instead of linking (in case of running fcst on a swept partition)
#
# Required environment variables:
  envar=()
  envar+=("ROOT_GODAS_DIR")
  envar+=("ROOT_EXP_DIR")
  envar+=("MEM")
  envar+=("JOB_WORK_DIR")
  envar+=("FCST_RST_TIME")
  envar+=("DA_TS_ONLY")
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

# puts the "Z" in datetime strings to make "date" tool happy
dtz(){ echo ${1:0:8}Z${1:8:10}; }

var=$ROOT_EXP_DIR/cycle/$CYCLE/da.3dvar/ai.nc
m=$MEM

out_dir=$JOB_WORK_DIR
rst0=$ROOT_EXP_DIR/cycle/$CYCLE/rst/mem_$m
rstc=$ROOT_EXP_DIR/cycle/$CYCLE/rst_comb/mem_$m
if [[ $m == "0000" ]]; then
    # this is a control forecast being run from the analysis mean
    ekf=$ROOT_EXP_DIR/cycle/$CYCLE/da.letkf/ana_mean.nc
else
    ekf=$ROOT_EXP_DIR/cycle/$CYCLE/da.letkf/$m.nc
fi

if [[ -e "$out_dir" ]]; then
    echo "WARNING: restart directory already exists"
    rm -rf $out_dir
fi
mkdir -p $out_dir

# link the restart files.
for f in $rst0/*; do
    ln -s $f $out_dir/
done

# remove the inidiviual restart file patches for those restart files that we have
# combined files for (and will be doing a DA update with)
rm $out_dir/MOM.res.nc.*
if [[ $DA_TS_ONLY -eq 0 ]]; then
    rm $out_dir/MOM.res_1.nc.*
fi

# update restart files that need to be modified
args=""
if [[ $DA_MODE == "var" ]]; then
    args="-var $var"
elif [[ $DA_MODE == "ekf" ]]; then
    args="-ekf $ekf"
elif [[ $DA_MODE == "hyb" ]]; then
    args="-ekf $ekf -var $var -alpha $DA_HYB_ALPHA"
fi

$ROOT_GODAS_DIR/tools/rst_update.py $args $rstc/MOM.res.nc $out_dir/MOM.res.nc -vars Temp,Salt,u
if [[ $DA_TS_ONLY -eq 0 ]]; then
    $ROOT_GODAS_DIR/tools/rst_update.py $args $rstc/MOM.res_1.nc $out_dir/MOM.res_1.nc -vars v
fi



