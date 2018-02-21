#!/bin/bash
set -e
cat << \#\#

#================================================================================
#================================================================================
# NCEP Hybrid-GODAS  -  da.letkf.prep.sh
#   LETKF Data assimilation preparation
#================================================================================
##
#
# Prerequisites:
#
# Results:
#
# Required environment variables:
 envar=()
 envar+=("ROOT_GODAS_DIR")
 envar+=("BKG_RST_DIR")
 envar+=("ANA_RST_DIR")
 envar+=("FCST_DIR")
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

# setup output directories
if [[ -e "$BKG_RST_DIR" ]]; then
    echo "WARNING: BKG_RST_DIR already exists, removing."
    rm -r $BKG_RST_DIR;
fi
mkdir -p $BKG_RST_DIR

#if [[ -e "$ANA_RST_DIR" ]]; then
#    echo "WARNING: ANA_RST_DIR already exists, removing."
#    rm -r $ANA_RST_DIR;
#fi
#mkdir -p $ANA_RST_DIR

# do mppnccombine on restart files
# TODO, modify LETKF to not need this
echo ""
echo "Performing mppnccombine on restart files..."
for f in "" ; do
    fi=$FCST_DIR/RESTART/MOM.res${f}.nc
    fo=$BKG_RST_DIR/MOM.res${f}.nc

    echo $fi
    echo "  mppnccombine..."
    $ROOT_GODAS_DIR/build/mppnccombine -m -64 $fo $fi.*
done

# combine the observation increment files
# TODO use correct dir
dtz(){ echo ${1:0:8}Z${1:8:10}; }
basedate="$(date "+%Y,%m,%d,%H,0,0" -d "$(dtz $CYCLE)")"
cd $(dirname $OBSOP_FILE)
ln -s $ROOT_EXP_DIR/config/da/obsprep.nml .
$ROOT_GODAS_DIR/build/obsprep_combine -basedate $basedate t*.nc obs.nc
# obsfiles=()
# for s in $DA_WNDW_SLOTS
