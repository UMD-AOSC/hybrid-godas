#!/bin/bash
set -e

cat << \#\#

#================================================================================
#================================================================================
# NCEP Hybrid-GODAS  -  da.letkf.run.sh
#  Local Ensemble Transform Kalman Filter run step
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
 envar+=("TMP_DIR")
 envar+=("PPN")
 envar+=("NODES")
 envar+=("DA_WNDW_SLOTS")
 envar+=("ENS_LIST")
#================================================================================
#================================================================================

# make sure the required env vars exist
for v in ${envar[@]}; do
    if [[ -z "${!v}" ]]; then
	echo "ERROR: env var $v is not set."; exit 1
    fi
    echo " $v = ${!v}"
done
set -u
echo ""


# calculate the number cores to run on
nproc=$(($PPN * $NODES))
echo "Running with $nproc cores"

# create working directory
if [[ -e "$TMP_DIR" ]]; then 
    echo "WARNING: TMP_DIR already exists, removing."
    rm -r $TMP_DIR
fi
mkdir -p $TMP_DIR
cd $TMP_DIR
ln -s $ROOT_GODAS_DIR/build/letkf .

# configuration files
source $ROOT_EXP_DIR/config/da/namelist.letkf.sh >> namelist.letkf
ln -s $ROOT_EXP_DIR/config/da/{obsdef,platdef,statedef}.cfg .

# grid definition files
ln -s $ROOT_GODAS_DIR/DATA/grid/ocean_geometry.nc .
ln -s $ROOT_GODAS_DIR/DATA/grid/Vertical_coordinate.nc ocean_vgrid.nc

# gues files
mkdir -p INPUT/gues
m=0
for mem in $ENS_LIST; do
    m=$(printf "%04g" $((10#$m+1)))
    ln -s ../../../da.prep/bkg_rst/mem_$mem/MOM.res.nc INPUT/gues/$m.nc
#    ln -s ../../../da.prep/bkg_rst/mem_$mem/MOM.res_1.nc INPUT/gues/$m.2.nc
done

# observations
# TODO use the combined obs output
mkdir -p INPUT/obsop
m=0
for mem in $ENS_LIST; do
    m=$(printf "%04g" $((10#$m+1)) )
    ln -s ../../../da.prep/omf/mem_$mem/obs.nc INPUT/obsop/$m.dat
done

## link observations
# s=0
# for slot in $DA_WNDW_SLOTS; do
#     s=$(printf "%02g" $((10#$s+1)) )
#     m=0
#     for mem in $ENS_LIST; do
# 	m=$(printf "%03g" $((10#$m+1)))
# 	ln -s ../da.prep/omf/mem_$mem/t$slot.dat obs$s$m.dat
#     done
# done



# run the LETKF
aprun -n $nproc ./letkf
