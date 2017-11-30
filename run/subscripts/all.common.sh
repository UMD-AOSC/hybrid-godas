#!/bin/bash
set -e
# Common bash code that is called at the beginning of all Hybrid-GODAS job subscripts


function envar_check() {
    local ar=("$@")
    for v in "${ar[@]}"; do
	if [[ -z "${!v}" ]]; then
	    error "env var $v is not set.";
	    exit 1
	fi
	if [[ "$envar_check_print" == 1 ]]; then
	    echo " $v = ${!v}"
	fi
    done
    if [[ "$envar_check_print" == 1 ]]; then
	echo ""
    fi
}


function error() {
    # Prints additional info along with a message sent to STDERR
#    echo "[$(date +'%Y-%m-%dT%H:%M:%S%z')]: $@" >&2
    echo "$@" >&2
}



function all_common_init() {


# the following are the environment variables REQUIRED before this script runs
#--------------------------------------------------------------------------------
local envar=()
envar+=("ROOT_DIR")         # path to the hybrid-godas root code/source directory
envar+=("EXP_DIR")          # path to the experiment directory


# the following are REQUIRED to exists after the CONFIGURATION FILES are loaded
#--------------------------------------------------------------------------------
local envar2=()
envar2+=("CYCLE")           # The datetime of the current cycle and analysis time (YYYYMMDDZHH)
envar2+=("CYCLE_LEN")       # The cycle period (hours)
envar2+=("DA_WNDW_OFST")    # offset between center of DA/cycle window and analysis time (hours)
envar2+=("FCST_RST_OFST")   # offset between center of DA/cycle window and forecast restart output time (hours)
envar2+=("FCST_LEAP_ADJ")  


# the following are the environment variables that are CREATE UPON EXIT of this script
#--------------------------------------------------------------------------------
local envar3=()
envar3+=("CYCLE_NEXT")
envar3+=("DA_WNDW_LEN")        # length of data assimilation window (hours)
envar3+=("DA_WNDW_CNTR_TIME")  # datetime at center of data assimilation window (YYYYMMDDZHH)
envar3+=("DA_WNDW_START_TIME") # datetime at start of data assimilation window (YYYYMMDDZHH)
envar3+=("DA_WNDW_END_TIME")   # datetime at end of data assimilation window (YYYYMMDDZZHH)
envar3+=("FCST_LEN")           # Total required length of a forecast (hours)
envar3+=("FCST_RST_TIME")      # datetime that forecast is to save its restart file (YYYYMMDDZHH)
envar3+=("FCST_START_TIME")    # datetime for the start of the forecast (YYYYMMDDZHH)
envar3+=("FCST_END_TIME")      # datetime for the end of the forecast (YYYYMMDDZHH)

#echo ""
#echo "Initializing common run script library (common.sh)..."

# make sure the required environment variables were passed in
envar_check_print=0
envar_check "${envar[@]}"


# setup the global hybrid-godas environment
#------------------------------------------------------------
#if [[ -z "${ROOT_DIR}" ]]; then
#    error "WARNING: ROOT_DIR environment variable SHOULD be set."
#    ROOT_DIR="$( cd "$( dirname "${BASH_SOURCE[0]}" )/../../" && pwd )"
#    echo "guessing that ROOT_DIR= $ROOT_DIR"
#    exit 1
#fi

env_config_file="${ROOT_DIR}/config/env"
if [[ ! -f "${env_config_file}" ]]; then
    error "FATAL: cannot find environment file: $env_config_file"
    exit 1
fi
source $env_config_file


# setup the experiment specific environment
#------------------------------------------------------------
# if [[ -z "${EXP_DIR}" ]]; then
#     error "FATAL: EXP_DIR environment variable needs to be set."
#     exit 1
# fi
exp_config_file="${EXP_DIR}/config/hybridgodas.config"
if [[ ! -f "${exp_config_file}" ]]; then
    error "FATAL: cannot find the experiment configuration file: ${exp_config_file}"
    exit 1
fi
source $exp_config_file

# make sure all required env vars are now specified
envar_check "${envar2[@]}"



# determine timing parameters
#------------------------------------------------------------
export TZ=UTC
DATE_FMT="%Y%m%dZ%H"

# datetime of next cycle
# TODO : adjust for leap day 
CYCLE_NEXT=$(date "+$DATE_FMT" -d "$CYCLE + $CYCLE_LEN hours")

# data assimilation window
DA_WNDW_LEN=$CYCLE_LEN
DA_WNDW_CNTR_TIME=$(date "+$DATE_FMT" -d "$CYCLE + $DA_WNDW_OFST hours")
DA_WNDW_START_TIME=$(date "+$DATE_FMT" -d "$DA_WNDW_CNTR_TIME - $(($DA_WNDW_LEN / 2)) hours")
DA_WNDW_END_TIME=$(date "+$DATE_FMT" -d "$DA_WNDW_START_TIME + $DA_WNDW_LEN hours")

# forecast length / restart time
FCST_RST_TIME=$(date "+$DATE_FMT" -d "$CYCLE + $FCST_RST_OFST hours")
FCST_LEN="$(($DA_WNDW_OFST - $FCST_RST_OFST + 3*$DA_WNDW_LEN/2))"
FCST_START_TIME=$(date "+$DATE_FMT" -d "$FCST_RST_TIME - $CYCLE_LEN hours")
FCST_END_TIME=$(date "+$DATE_FMT" -d "$FCST_START_TIME + $FCST_LEN hours")
FCST_PDAW_LEN="$(($FCST_LEN - $DA_WNDW_LEN))"


# safety check to make sure we have defined all the env vars we said we would
envar_check "${envar3[@]}"
envar_check_print=1


# all done
#echo  "done with common script initialization (common.sh)"
#echo ""
}
all_common_init
