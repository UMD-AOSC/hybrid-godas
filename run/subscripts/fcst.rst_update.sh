#!/bin/bash
set -e
export OMP_NUM_THREADS=1


if [[ $TILED_IO == 0 ]]; then
    cd $WORK_DIR
    args=""    
    if [[ $DA_MODE != "var" ]]; then
	args="$args $LETKF_ANA_DIR/ana.nc"
    fi
    if [[ $DA_MODE != "ekf" ]]; then
	args="$args $VAR_ANA_DIR/ana_inc.nc"
    fi

    $BIN_DIR/rst_update $DA_MODE $RST_DIR_IN/MOM.res.nc $args RESTART_IN/MOM.res.nc #> rst_update.log/mom.res.$t.l    
fi
# wait

#TODO do tiles correctly

# # for each tile to be updated
# # TODO, allow specification of WHICH res files to update
# for t in $*; do
#     cd $WORK_DIR

#     args=""    
#     if [[ $DA_MODE != "var" ]]; then
# 	args="$args $LETKF_ANA_DIR/ana.nc.$t"
#     fi
#     if [[ $DA_MODE != "ekf" ]]; then
# 	args="$args $VAR_ANA_DIR/ana_inc.nc"
#     fi

#     $BIN_DIR/rst_update $DA_MODE $RST_DIR_IN/MOM.res.nc.$t $args RESTART_IN/MOM.res.nc.$t > rst_update.log/mom.res.$t.log &
# done
# wait
