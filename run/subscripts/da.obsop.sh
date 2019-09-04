#!/bin/bash
set -e

#for each slot/mem that was passed in on the command line
# perform observation operator
for x in $*; do
    mem=${x##*/}
    slot=${x%%/*}

    cd $WORK_DIR/mem_$mem/obsop/$slot
    mkdir INPUT
    ln -s $GRID_DIR/{hgrid,vgrid,coast_dist}.nc INPUT/
    ln -s $DA_CFG_DIR/obsprep.nml .

    bkg_dir=$WORK_DIR/../da.prep/bkg/mem_$mem/fcst.diag
    f=$(date "+%Y_%m_%d" -d "$slot")
    diag_files=$bkg_dir/*.ocean_da_${f}.nc

    $BIN_DIR/obsop $WORK_DIR/../da.prep/obs/obs.$slot.nc $diag_files obs.nc > obsop.log
done
wait
