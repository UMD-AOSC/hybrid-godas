#!/bin/bash
set -e

cat <<EOF



#================================================================================
#================================================================================
# Hybrid-GODAS  -  da.prep.sh
#   observation and background file preparation
#================================================================================
#================================================================================

EOF

envar=()
envar+=("ENS_LIST")
envar+=("CYCLE_DIR")
envar+=("WORK_DIR")
envar+=("GRID_DIR")
envar+=("BIN_DIR")
envar+=("LOG_DIR")
envar+=("CYCLE")
envar+=("DA_SLOT_LEN")
envar+=("DA_SLOTS")
envar+=("DA_CFG_DIR")
envar+=("OBS_SST")
if [[ "$OBS_SST" -gt 0 ]]; then
    envar+=("OBS_SST_PATH")
fi

envar+=("OBS_ADT")
if [[ "$OBS_ADT" -gt 0 ]]; then
    envar+=("OBS_ADT_PATH")
fi

envar+=("OBS_PROF")
if [[ "$OBS_PROF" -gt 0 ]]; then
    envar+=("OBS_PROF_PATH")
fi


# make sure required env vars exist
for v in ${envar[@]}; do
    if [[ -z "${!v}" ]]; then
	echo "ERROR: env var $v is not set."; exit 1
    fi
    echo " $v = ${!v}"
done
set -u


# helper function to have "date" command be happy with  datetime strings
dtz(){ echo ${1:0:8}Z${1:8:10}; }


# setup working directory
if [[ -e $WORK_DIR ]]; then rm -r $WORK_DIR; fi
mkdir -p $WORK_DIR
cd $WORK_DIR



# link to the background files needed by the DA
mkdir -p $WORK_DIR/bkg/
for e in $ENS_LIST; do
    ln -s $CYCLE_DIR/$CYCLE/mem_$e $WORK_DIR/bkg/mem_$e
done


# TODO, handle slot sizes that are NOT 24 hours
if [[ $DA_SLOT_LEN -ne 24 ]]; then
    echo "ERROR: only DA_SLOT_LEN of 24 is currently supported."
    exit 1
fi



# get working directory ready
mkdir -p $WORK_DIR/obs/work
cd $WORK_DIR/obs/work
ln -s $DA_CFG_DIR/obsprep.nml .
mkdir INPUT
ln -s $GRID_DIR/{hgrid,vgrid,coast_dist}.nc INPUT/


# for each DA slot
# TODO handle slot sizes that are not 24
#------------------------------------------------------------
shopt -s nullglob
for slot_offset in $DA_SLOTS; do
    slot=$(date "+%Y%m%d" -d "$(dtz $CYCLE) + $slot_offset hours")

    echo ""
    echo ""
    echo "================================================================================"
    echo " Processing observations for $slot"
    echo "================================================================================"

    # SST observations
    #------------------------------------------------------------
    if [[ "$OBS_SST" == 1 ]]; then
	mkdir -p $WORK_DIR/obs/work/$slot.obs_sst
	cd $WORK_DIR/obs/work/$slot.obs_sst

	# TODO do the actual processing, instead of linking to preprocessed files
	echo ""
	echo "SST observation files:"
	file="$(date "+$OBS_SST_PATH" -d "$slot")"
	if [[ -f $file ]]; then
	    echo "  $file"
	    ln -s $file obsprep.sst.nc
	fi
    fi


    # ADT observations
    #------------------------------------------------------------
    if [[ "$OBS_ADT" == 1 ]]; then
	# setup working directory
	mkdir -p $WORK_DIR/obs/work/$slot.obs_adt
	cd $WORK_DIR/obs/work/$slot.obs_adt
	ln -s $WORK_DIR/obs/work/obsprep.nml .
	ln -s $WORK_DIR/obs/work/INPUT .

	# find all the files for this slot
	obs_dir="$(date "+$OBS_ADT_PATH" -d "$slot")"
	obs_files=$obs_dir/*.nc
	echo ""
	echo "Preparing ADT observation files: "
	for f in $obs_files; do echo "  $f"; done

	# setup log file
	log=$LOG_DIR/obsprep.adt.$slot.log
	mkdir -p $(dirname $log)
	echo "  NOTE: additional output placed in $(basename $log)"

	# run obsprep
	for f in $obs_files; do
	    f2=${f##*/}
	    $BIN_DIR/obsprep_adt $f obsprep.$f2 &> $log
	done
    fi

    # insitu profiles
    #------------------------------------------------------------
    if [[ "$OBS_PROF" == 1 ]]; then
	# setup working directory
	mkdir -p $WORK_DIR/obs/work/$slot.obs_prof
	cd $WORK_DIR/obs/work/$slot.obs_prof
	ln -s $WORK_DIR/obs/work/obsprep.nml .
	ln -s $WORK_DIR/obs/work/INPUT .

	# find all the files for this slot
	file="$(date +"$OBS_PROF_PATH" -d "$slot")"
	echo ""
	echo "Preparing Insitu observation files: "
	echo "  $file"

	# setup log file
	log=$LOG_DIR/obsprep.insitu.$slot.log
	mkdir -p $(dirname $log)
	echo "  NOTE: additional output placed in $(basename $log)"
	
	if [[ -f "$file" ]]; then
	    f2=${file##*/}
	    $BIN_DIR/obsprep_insitu $file obsprep.$f2 &> $log
	fi
    fi


    # combine into single file
    #------------------------------------------------------------
    cd $WORK_DIR/obs/work
    files=($slot.*/obsprep.*.nc)
    if [[ "${files[@]}" == 0 ]]; then
	echo -e "\nWARNING:  there are no observation files to combine for this slot."
    else
	echo -e "\nCombining files..."
	log=$LOG_DIR/obscomb.$slot.log
	mkdir -p $(dirname $log)
	echo "  NOTE: additional output placed in $(basename $log)"

	basedate="$(date "+%Y,%m,%d,%H,0,0" -d "$(dtz $slot)")"
	$BIN_DIR/obsprep_combine -basedate $basedate ${files[@]} ../obs.$slot.nc &> $log
    fi

done
