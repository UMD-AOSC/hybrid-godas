#!/bin/bash
set -e
cat << \#\#

#================================================================================
#================================================================================
# NCEP Hybrid-GODAS  -  da.update.sh
#   Post EnKF/3DVar step (restart file updates, etc)
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
  envar+=("ENS_LIST")
  envar+=("JOB_WORK_DIR")
#  envar+=("BKG_RST_DIR")
#  envar+=("NPROC")
  envar+=("FCST_RST_TIME")
#  envar+=("DA_DIR")
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


# # directory definitions
# # TODO: handle hybrid
# var_dir=$DA_DIR/var

# puts the "Z" in datetime strings to make "date" tool happy
dtz(){ echo ${1:0:8}Z${1:8:10}; }


# todo, use properly defined working directories
rst_dir=$ROOT_EXP_DIR/fcst_rst/mem_#mem#/%Y%m%d%H
ana_files=$JOB_WORK_DIR/da.letkf/OUTPUT/#mem#.nc    #Temp,Salt
bkg_files=$JOB_WORK_DIR/da.prep/bkg_rst/mem_#mem#/MOM.res.nc
prev_files=$JOB_WORK_DIR/fcst.run/mem_#mem#/RESTART/

rst_dir=$(date "+$rst_dir" -d "$(dtz $FCST_RST_TIME)")


for m in $ENS_LIST; do
    if [[ -d "$rst_dir" ]]; then
	echo "ERROR: restart directory already exists: $rst_dir"
	exit 1
    fi
    echo ""
    echo "creating new restart files for $m"

    r=${rst_dir//\#mem\#/$m}
    p=${prev_files//\#mem\#/$m}
    b=${bkg_files//\#mem\#/$m}
    a=${ana_files//\#mem\#/$m}
    h=$JOB_WORK_DIR/da.3dvar/ana_inc.nc

    if [[ -d $r ]]; then
	echo "Removing old restart directory."
	rm $r -r
    fi
    mkdir -p $r

    # link files that we aren't going to modify
    # TODO, need to copy, not link
    for f in $p/*; do
	ln -s $f $r/${f##*/}
    done

    # update restart files that need to be modified
    # TODO, define the variables to be updated (T/S/U/V)
    rm $r/MOM.res.nc.*
    $ROOT_GODAS_DIR/tools/rst_update.py -hyb_inc $h $b $a $r/MOM.res.nc -vars Temp,Salt
done 


# All done, make sure everything is ready to start the next cycle
# TODO
