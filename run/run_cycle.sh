#!/bin/bash
set -eu

# Read in command line argument (location of experiment directory)
if [[ $# != 1 || ! -d $1 ]]; then
    echo "USAGE: run_cycle.sh <dir_to_exp>"
    exit 1
fi
export EXP_DIR=$(readlink -f $1)

# read in the machine config file
source $EXP_DIR/config/config.machine

# read in experiment config file
source $EXP_DIR/config/config.exp


#================================================================================
#================================================================================


# location of experiment cycle restart files
export CYCLE_DIR=$EXP_DIR/cycle
export CYCLE_STATUS_FILE=$CYCLE_DIR/cycle_status

# Determine which cycle date we are currently on.
# If the $CYCLE_STATUS_FILE doesn't exist, then the previous
# cycle didn't finish.. we can then assume this is the first cycle
export FIRST_CYCLE=0
[[ ! -e $CYCLE_STATUS_FILE ]] && FIRST_CYCLE=1
if [[ $FIRST_CYCLE == 1 ]]; then
    export CYCLE=$CYCLE_START   
else
    export CYCLE=$(cat $CYCLE_STATUS_FILE)    
fi
if [[ $CYCLE -gt $CYCLE_END ]]; then
    echo "Done with cycles, CYCLE_END=$CYCLE_END"
    exit 0
fi
echo "Running cycle: $CYCLE"

# derived directories and file locations
export BIN_DIR=$HGODAS_BIN_DIR
export SCRIPT_DIR=$HGODAS_ROOT_DIR/run/subscripts
export LOG_DIR_BASE=$EXP_DIR/logs/$CYCLE
export SCRATCH_DIR_CYCLE=$HGODAS_SCRATCH_DIR/cycle_$CYCLE
export OUTPUT_DIR=$EXP_DIR/output
export DA_CFG_DIR=$EXP_DIR/config/da

# # machine/build specific params (move elsewhere)
# export PPN=8
# export NODES=1
    
# Create a list of all ensemble member IDs.
# Note, this is used even if not doing ensemble DA
export ENS_LIST="0000"
if [[ $DA_MODE == 'hyb' || $DA_MODE == 'ekf' ]]; then
   for e in $(seq -f %04g $ENS_SIZE); do
       ENS_LIST="$ENS_LIST $e"
   done
fi

# load the common per/cycle parameters that are handled by the job wrapper script
set --
. $SCRIPT_DIR/jobwrapper.sh


# -------------------------------------------------------------------
# DA preparation
# -------------------------------------------------------------------
if [[ $FIRST_CYCLE == 0 ]]; then
(
    export GRID_DIR=$EXP_DIR/config/da/grid
    export WORK_DIR=$SCRATCH_DIR_CYCLE/da.prep
    export LOG_DIR=$LOG_DIR_BASE/da.prep
    $SCRIPT_DIR/da.prep.sh
)
fi


# -------------------------------------------------------------------
# DA run
# -------------------------------------------------------------------
if [[ $FIRST_CYCLE == 0 ]]; then
(
    export GRID_DIR=$EXP_DIR/config/da/grid    
    export WORK_DIR=$SCRATCH_DIR_CYCLE/da.run
    export LOG_DIR=$LOG_DIR_BASE/da.run
    $SCRIPT_DIR/da.run.sh
)
fi


# -------------------------------------------------------------------
# DA Post processing
# -------------------------------------------------------------------
if [[ $FIRST_CYCLE == 0 ]]; then
(    
    export SAVE_OMF=1
    export WORK_DIR=$SCRATCH_DIR_CYCLE/da.post
    $SCRIPT_DIR/da.post.sh
)
fi  


# -------------------------------------------------------------------
# forecast preparation
# -------------------------------------------------------------------
(
    export IC_GEN=$FIRST_CYCLE
    export IC_DIR=$HGODAS_DATA_DIR/ic/soda_20160101
    export WORK_DIR=$SCRATCH_DIR_CYCLE/fcst.prep
    $SCRIPT_DIR/fcst.prep.sh
)

# -------------------------------------------------------------------
# Forecast run for each ens member
# -------------------------------------------------------------------
for MEM in $ENS_LIST; do
(
    log=$LOG_DIR_BASE/fcst.run/fcst.$MEM.log
    mkdir -p $(dirname $log)
    echo "running ensemble member $MEM ..."
    echo "  log file: $log"

    export FORC_DIR=$SCRATCH_DIR_CYCLE/fcst.prep/forc/mem_$MEM
    export IC_FILE=$SCRATCH_DIR_CYCLE/fcst.prep/ic/mem_$MEM/ic.nc
    export FCST_DONE=$EXP_DIR/cycle/$CYCLE_NEXT/mem_$MEM/fcst_done
    export MOM_CFG_DIR=$EXP_DIR/config/mom
#     export FCST_RESTART=0
    export MOM6_EXE=$BIN_DIR/mom6
    export RST_DIR_IN=$EXP_DIR/cycle/$CYCLE/mem_$MEM/fcst.rst
    export RST_DIR_OUT=$EXP_DIR/cycle/$CYCLE_NEXT/mem_$MEM/fcst.rst
    export DIAG_DIR_OUT=$EXP_DIR/cycle/$CYCLE_NEXT/mem_$MEM/fcst.diag
    export SET_MOM_PE=0
    export ALLOW_COLDSTART=1
    export FCST_DIAG_DA=1
    if [[ $MEM == "0000" ]]; then
	export FCST_DIAG_OTHER=1
    else
	export FCST_DIAG_OTHER=0
    fi
    export LETKF_ANA_DIR=$SCRATCH_DIR/da.run/mem_$MEM/letkf
    export VAR_ANA_DIR=$SCRATCH_DIR_CYCLE/da.run/var
    export WORK_DIR=$SCRATCH_DIR_CYCLE/fcst.run/mem_$MEM

    $SCRIPT_DIR/fcst.run.sh &> $log  || echo "ERROR: unable to run fcst for member $MEM. Check the individual log file."
)   
done


# -------------------------------------------------------------------
# Forecast Post processing
# -------------------------------------------------------------------
(
    export COMBINE_EXE=$HGODAS_BIN_DIR/mppnccombine
    export FCST_DIR=$SCRATCH_DIR_CYCLE/fcst.run/mem_0000
    export DIAG_FILES="*_diag*"
    export WORK_DIR=$SCRATCH_DIR_CYCLE/fcst.post
    $SCRIPT_DIR/fcst.post.sh
)

# -------------------------------------------------------------------
# cleanup
# -------------------------------------------------------------------
(
    export SCRATCH_DIR=$HGODAS_SCRATCH_DIR
    export SAVE_DIR=$CYCLE_DIR
    export KEEP_CYCLES=0
    export KEEP_CYCLES_REGEX="none"
    $SCRIPT_DIR/cycle.scrub.sh
)


# -------------------------------------------------------------------
# Done! 
# -------------------------------------------------------------------
echo $CYCLE_NEXT > $CYCLE_STATUS_FILE
echo "Done with cycle $CYCLE"
