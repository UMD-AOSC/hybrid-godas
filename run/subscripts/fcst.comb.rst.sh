#!/bin/bash
set -e
cat << \#\#

#================================================================================
#================================================================================
# NCEP Hybrid-GODAS  -  fcst.comb.rst.sh
#   Combine the restart files
#   TODO: get rid of this and have all DA scripts hand ocean output in
#    its native patches
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
 envar+=("FCST_DIR")
 envar+=("BKG_RST_DIR")
 envar+=("RST_COMB_DIR")
 envar+=("DA_TS_ONLY")
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

if [[ -e "$BKG_RST_DIR" ]]; then
    echo "WARNING: BKG_RST_DIR already exists, removing."
    rm -r $BKG_RST_DIR;
fi
mkdir -p $BKG_RST_DIR

mkdir -p $RST_COMB_DIR


# do mppncombine on restart file(s)
# we only need "MOM.res.nc" if we are only updating T/S
# we also need "MOM.res_1.nc" if we are also updating U/V
echo ""
echo "Performing mppnccombine on restart files(s)... "
rst_pfx=("")
if [[ $DA_TS_ONLY -eq 0 ]]; then rst_pfx+=(_1); fi
for f in "${rst_pfx[@]}"; do
    fi=$FCST_DIR/RESTART/MOM.res${f}.nc
    fo=$BKG_RST_DIR/MOM.res${f}.nc


    echo $fi
    echo "  mppnccombine..."
    $ROOT_GODAS_DIR/build/mppnccombine -m -64 $fo $fi.*

    mv $fo $RST_COMB_DIR
done
ln -s $RST_COMB_DIR/MOM.res* $BKG_RST_DIR/
