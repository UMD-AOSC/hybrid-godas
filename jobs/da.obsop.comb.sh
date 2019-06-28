#!/bin/bash
set -e
cat << \#\#

#================================================================================
#================================================================================
# NCEP Hybrid-GODAS  -  da.obsop.comb.sh
#   Combine the output from multiple observation operators / slots
#   into a single file
#================================================================================
##
#
# Prerequisites:
#
# Results:
#
# Required environment variables:
 envar=()
 envar+=("ROOT_GODAS_DIR")  # The path to the hybrid-godas root code/source dir
 envar+=("JOB_WORK_DIR")
 envar+=("CYCLE")           # The datetime of the current cycle and ana time (YYYYMMDDHH)
 envar+=("OMF_FILES")
 envar+=("OUT_FILE")
 envar+=("EXP_DIR")
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



# determine the filename of the model background 
dtz(){ echo ${1:0:8}Z${1:8:10}; }


# setup the working directory
#------------------------------------------------------------
echo ""
echo "Using working directory:"
echo " $JOB_WORK_DIR"
if [[ -e $JOB_WORK_DIR ]]; then rm -r $JOB_WORK_DIR; fi
mkdir -p $JOB_WORK_DIR 
cd $JOB_WORK_DIR

basedate=$(date "+%Y,%m,%d,%H,0,0" -d "$(dtz $CYCLE)")
ln -s $EXP_DIR/config/da/obsprep.nml .

rm -f $OUT_FILE
$ROOT_GODAS_DIR/build/obsprep_combine -basedate $basedate $OMF_FILES $OUT_FILE

