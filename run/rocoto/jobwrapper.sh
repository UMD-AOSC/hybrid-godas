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
# TODO: is this necessary? they should already be loaded into the environment
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
done
echo ""

# env vars that should already be set 
#for v in EXP_NAME ENS_SIZE DA_MODE DA_TIMESLOTS DA_WNDW_OFST FCST_RST_OFST FCST_IO_PROC FCST_IO_MISS; do
for v in EXP_NAME ENS_SIZE; do
    echo " $v = ${!v}"
done
echo ""



#================================================================================
# Calculate derived parameters based on the cycle date/time
#================================================================================
export TZ=UTC
dtz() { echo "${1:0:8}Z${1:8:10}"; }


#cycle length, next, previous.... adjustments are done for leap days
#------------------------------------------------------------
CYCLE_LEN_NEXT=$CYCLE_LEN
CYCLE_LEN_PREV=$CYCLE_LEN
if [[ $CYCLE_LEAPADJ -ne 0 ]]; then
    # determine if leap day could affect this cycle
    if [[ $(date "+%m%d" -d "${CYCLE:0:4}0228 + 1 day") == "0229" ]]; then
	# yes, this is a leap year...
	echo "Checking leap day..."
	ldate=${CYCLE:0:4}0229  #< date of leap date for this year
	cdate=${CYCLE:0:8}
	pdate=$(date "+%Y%m%d" -d "$cdate - $CYCLE_LEN hours") #< date for prev cycle
	ndate=$(date "+%Y%m%d" -d "$cdate + $CYCLE_LEN hours") #< date for next cycle
	cycle_len_leap=$(( $CYCLE_LEN + 24 ))

	# if current cycle includes a leap day...
	if [[ $cdate -ge $ldate && $pdate -le $ldate ]]; then
	    echo "Current cycle has a leap day!"
# TODO: allow increasing CYCLE_LEN, which will allow for an extra DA slot during leap day
#  this will only work if rocoto script is NOT splitting jobs by slot numbers. add a 
# flag to the config file to allow for this.

#	    export CYCLE_LEN=$cycle_len_leap
	    CYCLE_LEN_PREV=$cycle_len_leap
	fi

	# if next cycle includes a leap day...
	if [[ $cdate -le $ldate && $ndate -ge $ldate ]]; then
	    echo "next cycle has a leap day!"
	    CYCLE_LEN_NEXT=$cycle_len_leap
	fi
    fi
fi
export CYCLE_NEXT=$(date "+%Y%m%d%H" -d "$(dtz $CYCLE) + $CYCLE_LEN_NEXT hours")
export CYCLE_PREV=$(date "+%Y%m%d%H" -d "$(dtz $CYCLE) - $CYCLE_LEN_PREV hours")

for v in CYCLE_LEN CYCLE_PREV CYCLE CYCLE_NEXT; do
    echo " $v = ${!v}"
done
echo ""


# data assimilation window
#------------------------------------------------------------
export DA_WNDW_LEN=$CYCLE_LEN
export DA_WNDW_END_TIME=$(date "+%Y%m%d%H" -d "$(dtz $CYCLE) + $DA_IAU_LEN hours")
export DA_WNDW_START_TIME=$(date "+%Y%m%d%H" -d "$(dtz $DA_WNDW_END_TIME) - $CYCLE_LEN hours")
export DA_SLOT_NUM=$(($CYCLE_LEN / $DA_SLOT_LEN))
ss=$(( $DA_IAU_LEN - $DA_WNDW_LEN  + $DA_SLOT_LEN/2))
se=$(( $ss + $DA_SLOT_LEN*($DA_SLOT_NUM-1) ))
export DA_SLOTS=$(seq -s ' ' -f "%+02g" $ss $DA_SLOT_LEN $se )
#export DA_WNDW_CNTR_TIME=

for v in DA_IAU_LEN  DA_WNDW_LEN  DA_WNDW_START_TIME  DA_WNDW_END_TIME\
    DA_SLOT_LEN DA_SLOT_NUM DA_SLOTS; do
    echo " $v = ${!v}"
done
echo ""


# Sanity check on resulting values
#------------------------------------------------------------
#TODO: check:
# * input dates are valid
# * slots are equal sizes
# * don't do leap adj if da length is <= 24 hours

if [[ $DA_IAU_LEN -gt 0 ]]; then
    echo "ERROR: IAU is not yet supported. Set DA_IAU_LEN to 0 and try again..."
    #exit 1
fi
if [[ $DA_SLOT_LEN -ne 24 ]]; then
    echo "ERROR: DA slots other than 24 hours is not supported (yet). set DA_SLOT_LEN to 24 and try again."
    #exit 1
fi


# forecast length /restart times
#------------------------------------------------------------
export FCST_LEN=$(($CYCLE_LEN_PREV + $DA_IAU_LEN))
export FCST_RST_TIME=$CYCLE
export FCST_START_TIME=$CYCLE_PREV
export FCST_END_TIME=$DA_WNDW_END_TIME

for v in FCST_LEN FCST_START_TIME FCST_END_TIME FCST_RST_TIME; do
    echo " $v = ${!v}"
done


#================================================================================
# Launch a subscript if told to do so
#================================================================================
if [[ "$#" -gt 0 ]]; then
    echo "Launching $*"
    echo ""
    $*
fi
echo ""
