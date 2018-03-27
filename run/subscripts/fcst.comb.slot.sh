#!/bin/bash
set -e
cat << \#\#

#================================================================================
#================================================================================
# NCEP Hybrid-GODAS  -  fcst.comb.slot.sh
#   MOM6 output file processing
#================================================================================
##
#
# Prequisites:
#  * MOM6 must be running in $FCST_DIR , the output files to combine do not 
#    necessarily need to be ready. If they are not, the script simply returns
#
# Results:
#  * The output file for the given timeslot of the given ensemble member forecast
#    will be combined and placed at $BKG_FILE
#
# Required environment variables:
 envar=()
 envar+=("ROOT_GODAS_DIR")    # Path to the hybrid-godas root code/source directory
 envar+=("CYCLE")             # The datetime of the current cycle and analysis time (YYYYMMDDHH)
 envar+=("DA_SLOT")           # The offset (in hours) from the analysis time
                              # (e.g.  "-5" or "+0")
 envar+=("FCST_DIR")          # The directory that the forecast is runnin in
 envar+=("BKG_FILE")          # filename the the combined output file should be moved to 
 envar+=("FCST_IO_PROC")      # The number of processors used for the MOM6 output and 
                              # therefore the max number of files that need to be combined
 envar+=("FCST_IO_MISS")      # id's of the output files that will be missing (due to
                              # blocks being entirely on land) (e.g. ".0024")
 envar+=("FCST_START_TIME")   # datetime for the start of the forecast (YYYYMMDDHH)
#================================================================================
#================================================================================


# make sure the required env vars exist
for v in ${envar[@]}; do
    if [[ -z "${!v}" ]]; then
	echo "ERROR: env var $v is not set. ${!v}"; exit 1
    fi
    echo " $v = ${!v}"
done
set -u
echo ""


dtz(){ echo ${1:0:8}Z${1:8:10};  }


# determine which file(s) we should be looking at
#  the result will be placed in the "files" array
fdate=$(date "+%Y%m%d" -d "$(dtz $CYCLE) + $DA_SLOT hours")
file_base="$(date "+%Y%m%d" -d "$(dtz $FCST_START_TIME)").ocean_daily_$(date "+%Y_%m_%d" -d "$fdate").nc"
files=()
if [[ $FCST_IO_PROC -gt 1 ]]; then
    for f in $(seq -f "%04g" 0 $(($FCST_IO_PROC-1)) ); do
	valid=true
	for g in $FCST_IO_MISS; do
	    if [[ "$f" == "$g" ]]; then valid=false; break; fi
	done
	if [[ $valid == false ]]; then continue; fi
	files+=("${file_base}.$f")
    done
else
    files+=("$file_base")
fi


# where should the output file be placed?
outputfile=$(date "+$BKG_FILE" -d "$fdate")
outputdir=$(dirname "${outputfile}")
echo "Output will be combined and placed at:"
echo " $outputfile"


# make the output directory
mkdir -p $outputdir


# do the combine
cd $FCST_DIR
if [[ "$FCST_IO_PROC" -gt 1 ]]; then

    # remove the output file if it already exists
    if [[ -f $outputfile ]]; then
	echo "WARNING: Removing already existing output file $outputfile"
	rm $outputfile;
    fi    

    echo "Performing mppncombine"
    $ROOT_GODAS_DIR/build/mppnccombine -m -64 $outputfile ${files[@]}
fi
