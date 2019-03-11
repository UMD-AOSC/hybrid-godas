#!/bin/bash
set -e

cat <<EOF
#================================================================================
#================================================================================
# NCEP Hybrid-GODAS  -  fcst.bias_corr.sh
#  MOM6 equatorial pressure bias correction
#================================================================================
EOF
# Required environment variables:
 envar=()
 envar+=("JOB_WORK_DIR")
 envar+=("ROOT_EXP_DIR")
 envar+=("CYCLE")
 envar+=("FCST_DIR")
 envar+=("FORC_DIR")
#================================================================================
#================================================================================

# make sure required env vars exist
for v in ${envar[@]}; do
    if [[ -z "${!v}" ]]; then
	echo "ERROR: env var $v is not set."; exit 1
    fi
    echo " $v = ${!v}"
done




# TODO test FORC_PRES_CORR==1
# TODO make this work with non-var cycle

if [[ -e "$JOB_WORK_DIR" ]]; then
    rm -rf $JOB_WORK_DIR
fi
mkdir -p $JOB_WORK_DIR
cd $JOB_WORK_DIR

mkdir -p FORC

mkdir -p $BIAS_FILES_OUT
ln -s $BIAS_FILES_OUT OUTPUT
ln -s $BIAS_FILES_IN INPUT


# run the pressure correction calculation
ln -s $ROOT_EXP_DIR/config/da/pressure_correction.* .
ln -s $RST_DIR_IN/MOM.res.nc ana.nc
ln -s $ROOT_GODAS_DIR/DATA/grid/ocean_geometry.nc hgrid.nc
ln -s $ROOT_GODAS_DIR/DATA/grid/Vertical_coordinate.nc vgrid.nc
cdo sub -select,name=Temp,Salt ana.nc $ROOT_EXP_DIR/cycle/$CYCLE/da.3dvar/ai.nc bkg.nc

$ROOT_GODAS_DIR/build/pressure_correction

# regrid to a regular lat/lon
infile=pres.corr.nc \
 outfile=pres.corr.remap1.nc \
 vars=pres \
 weights=$ROOT_GODAS_DIR/DATA/grid/remap.grid_t.025deg.bil.nc \
 ncl $ROOT_GODAS_DIR/tools/postproc/remap.ncl

cdo setmisstoc,0 -remapbil,../../fcst.prep/mem_$MEM/SLP.nc pres.corr.remap1.nc pres.corr.remap2.nc
cdo add ../../fcst.prep/mem_$MEM/SLP.nc pres.corr.remap2.nc SLP.nc
ncatted -O -a calendar,,m,c,gregorian SLP.nc
ncatted -O -a axis,time,c,c,T SLP.nc


cd FORC
ln -s ../../../fcst.prep/mem_$MEM/*.nc .
ln -sf ../SLP.nc .
