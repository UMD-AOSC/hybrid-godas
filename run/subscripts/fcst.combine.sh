#!/bin/bash

cat << \#\#

#================================================================================
#================================================================================
# NCEP Hybrid-GODAS  -  fcst.combine.sh
#   MOM6 output file processing
#================================================================================
##
# Travis.Sluka@noaa.gov / tsluka.umd.edu
#
# Command line arguments:
#  * a "-check" flag can be passed, in which case the scripts determines if the 
#    required files are ready to be combined, but does not actually do so.
#
# Prequisites:
#  * MOM6 must be running in $FCST_DIR , the output files to combine do not 
#    necessarily need to be ready. If they are not, the script simply returns
#
# Results:
#  * The output file for the given timeslot of the given ensemble member forecast
#    will be combined and placed at $BKG_FILE
#
# Required MANUALLY defined environment variables:
#  * The following need to be specified by the caller of this script
envar+=("ROOT_DIR")     # The path to the hybrid-godas root code/source directory
envar+=("DA_SLOT")      # The offset (in days) from the analysis time
                        # (e.g.  "-5" or "+0")
envar+=("FCST_DIR")     # The directory that the forecast is runnin in
envar+=("CYCLE")        # The datetime of the current cycle and analysis time (YYYYMMDDZHH)
envar+=("FCST_IO_PROC") # The number of processors used for the MOM6 output and 
                        # therefore the max number of files that need to be combined
envar+=("FCST_IO_MISS") # id's of the output files that will be missing (due to
                        # blocks being entirely on land) (e.g. ".0024")
envar+=("BKG_FILE")     # filename the the combined output file should be moved to
envar+=("FCST_FILE_MINAGE")  # Minimum age, in seconds, a file should be before
                             # trying to use it.
envar+=("FCST_FILE_MINSIZE") # Minimum size, in bytes, a file should be before
                             # trying to use it.
#
# Required AUTOMATICALLY defined environment variables:
#  * The following are required but should already be defined by all.common.sh
envar+=("FCST_START_TIME") # datetime for the start of the forecast (YYYYMMDDZHH)
#================================================================================
#================================================================================


# run common script setup
set -e
scriptsdir="$( cd "$( dirname "${BASH_SOURCE[0]}" )" && pwd )"
source ${scriptsdir}/all.common.sh
envar_check "${envar[@]}"
set -u


#--------------------------------------------------------------------------------
#--------------------------------------------------------------------------------


# determine which file(s) we should be looking at
#  the result will be placed in the "files" array
fdate=$(date "+%Y%m%d" -d "$CYCLE + $DA_SLOT days")
file_base="$(date "+%Y%m%d" -d "$FCST_START_TIME").ocean_daily_$(date "+%Y_%m_%d" -d "$fdate").nc"
files=()
if [[ $FCST_IO_PROC -gt 1 ]]; then
    for f in $(seq -f "%04g" 0 $(($FCST_IO_PROC-1)) ); do
	valid=true
	for g in ${FCST_IO_MISS[@]}; do
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


# Check to make sure all the files are ready.
# The "isready" variable will be set to 1 at the end of the for loop
# if all required files are ready to go.
now=$(date "+%s")
for f in "${files[@]}"; do
    isready=0

    # test if file exists
    if [[ ! -f "$FCST_DIR/$f" ]]; then break; fi

    # test if file is the right size
    fs=($(stat --printf="%s %Y " $FCST_DIR/$f))
    if [[ "${fs[0]}" -lt "$FCST_FILE_MINSIZE" ]]; then break; fi

    # test if file was recently modified
    a="$(( $now - ${fs[1]} ))"
    if [[ "$a" -lt "$FCST_FILE_MINAGE" ]]; then break; fi

    isready=1
done
if [[ "$isready" == 1 ]]; then
    # test if there is at least 1 time record for the last file
    # MOM6 seems to keep a file open 1 day past when it should, keeping the file
    # header from being updated until well after the file data is
    v=$(ncdump -h $FCST_DIR/${files[-1]} | grep "time = UNLIMITED ; // (1 currently)" -c)
    if [[ "$v" -ne "1" ]]; then isready=0; fi
fi




# if "-check" was passed in on the command line, stop here, and return whether the files are ready
# Return with exit code of 1 if not ready, or 0 if ready.
if [[ "$#" -gt 0 ]]; then
    if [[ "$1" == "-check" ]]; then
	exit $(( 1 - $isready ))
    else
	error "unknown argument passed \"$1\""
	exit 1
    fi
fi


# If files aren't ready, exit
if [[ "$isready" == "0" ]]; then exit 1; fi


# make the output directory
mkdir -p $outputdir


# do the combine
cd $FCST_DIR
if [[ "$FCST_IO_PROC" -gt 1 ]]; then

    # remove the output file if it already exists
    if [[ -f $file_base ]]; then
	error "Removing already existing output file $file_base"
	rm $file_base;
    fi    

    echo "Performing mppncombine"
    $ROOT_DIR/build/mppnccombine -m -64 $outputfile ${files[@]}
fi
