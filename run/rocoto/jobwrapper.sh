#!/bin/bash
set +x
set -e

cat << \#\#
#================================================================================
#================================================================================
# NCEP Hybrid-GODAS  -  jobwrapper.sh"
#================================================================================
##
# Required environment variables:
#  * The following need to be specified by the caller of this script
 envar=()
 envar+=("ROOT_GODAS_DIR")
 envar+=("ROOT_EXP_DIR")
 envar+=("CYCLE")
#================================================================================
#================================================================================


# make sure the required env vars exist
#------------------------------------------------------------
for v in ${envar[@]}; do
    if [[ -z "${!v}" ]]; then 
        echo "ERROR: env var $v is not set."; exit 1
    fi
    echo " $v = ${!v}"
done
echo ""
set -u



# Load in configurables from the system end experiment configuration scripts
# TODO: check for env vars existing
#------------------------------------------------------------
files=()
files+=("${ROOT_GODAS_DIR}/config/env")
files+=("${ROOT_EXP_DIR}/config/hybridgodas.config")
for f in ${files[@]}; do
    if [[ ! -f "$f" ]]; then
	echo "ERROR: cannot find environment file: $f"
	exit 1
    fi
    echo "loading config file: $f"
    source $f
    echo ""
done

# env vars that should already be set 
for v in EXP_NAME ENS_SIZE DA_MODE DA_TIMESLOTS DA_WNDW_OFST FCST_RST_OFST FCST_IO_PROC FCST_IO_MISS; do
    echo " $v = ${!v}"
done
echo ""

# Calculate derived parameters based on the cycle date/time
#------------------------------------------------------------
export TZ=UTC
dtz() {
    echo "${1:0:8}Z${1:8:10}"
}

# TODO: adjust for leap day
export CYCLE_NEXT=$(date "+%Y%m%d%H" -d "$(dtz $CYCLE) + $CYCLE_LEN hours")

# data assimilation window
export DA_WNDW_LEN=$CYCLE_LEN
export DA_WNDW_CNTR_TIME=$(date  "+%Y%m%d%H" -d "$(dtz $CYCLE) + $DA_WNDW_OFST hours")
export DA_WNDW_START_TIME=$(date "+%Y%m%d%H" -d "$(dtz $DA_WNDW_CNTR_TIME) - $(($DA_WNDW_LEN / 2)) hours")
export DA_WNDW_END_TIME=$(date   "+%Y%m%d%H" -d "$(dtz $DA_WNDW_START_TIME) + $DA_WNDW_LEN hours")
s1=$(( $DA_WNDW_OFST / 24 - ($DA_TIMESLOTS-1)/2))
export DA_WNDW_SLOTS=$(seq -s ' ' -f "%+02g" $s1 $(($s1 + $DA_TIMESLOTS -1)) )

for v in DA_WNDW_LEN DA_WNDW_CNTR_TIME DA_WNDW_START_TIME DA_WNDW_END_TIME DA_WNDW_SLOTS; do
    echo " $v = ${!v}"
done


# forecast length /restart times
export FCST_RST_TIME="$(date "+%Y%m%d%H" -d "$(dtz $CYCLE) + $FCST_RST_OFST hours")"
export FCST_LEN="$(($DA_WNDW_OFST - $FCST_RST_OFST + 3*$DA_WNDW_LEN/2))"
export FCST_START_TIME="$(date "+%Y%m%d%H" -d "$(dtz $FCST_RST_TIME) - $CYCLE_LEN hours")"
export FCST_END_TIME="$(date "+%Y%m%d%H" -d "$(dtz $FCST_START_TIME) + $FCST_LEN hours")"

for v in FCST_LEN FCST_START_TIME FCST_END_TIME FCST_RST_TIME; do
    echo " $v = ${!v}"
done



# Launch a subscript if told to do so
#------------------------------------------------------------
if [[ "$#" -gt 0 ]]; then
    echo "Launching $*"
    echo ""
    $*
fi
echo ""
