#!/bin/bash
set -e

dtz(){ echo ${1:0:8}Z${1:8:10}; }

export OMP_NUM_THREADS=1

basedate=$(date "+%Y,%m,%d,%H,0,0" -d "$(dtz $CYCLE)")

for mem in $*; do
    cd $WORK_DIR/mem_$mem/obsop

    files=*/obs.nc
    if [[ "$files" == "" ]]; then
	echo "WARNING: no observations found for $mem"
	exit 0
    fi
    pwd
    mkdir INPUT
    ln -s $GRID_DIR/{hgrid,vgrid,coast_dist}.nc INPUT/
    ln -s $DA_CFG_DIR/obsprep.nml .

    $BIN_DIR/obsprep_combine -basedate $basedate */obs.nc obs.nc #> combine.log &
done
wait

