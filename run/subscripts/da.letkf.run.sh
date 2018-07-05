#!/bin/bash
set -e

cat << EOF

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
#================================================================================
EOF
# Required environment variables:
 envar=()
 envar+=("ROOT_GODAS_DIR")
 envar+=("TMP_DIR")
 envar+=("PPN")
 envar+=("NODES")
 envar+=("ENS_SIZE")
 envar+=("ENS_LIST")
 envar+=("ANA_FILES")
 envar+=("DA_TS_ONLY")
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
#------------------------------------------------------------
ln -s $ROOT_EXP_DIR/config/da/letkf*.json .

# state  config file depens on whether we are doing T/S only update
# or all variables
f=letkf_state_all.json
if [[ $DA_TS_ONLY -ne 0 ]]; then 
    echo "WARNING: configured to only update T/S state variables"
    f=letkf_state_ts.json
fi
ln -s $f letkf_state.json

#ensemble size / node specific variables
cat  > letkf_mpi.json <<EOF
{
 "ens_size" : ${ENS_SIZE},
 "ppn" : ${PPN},
}
EOF
#------------------------------------------------------------


# grid definition files
ln -s $ROOT_GODAS_DIR/DATA/grid/ocean_geometry.nc .
ln -s $ROOT_GODAS_DIR/DATA/grid/Vertical_coordinate.nc ocean_vgrid.nc

# gues files
mkdir -p INPUT/gues
m=0
for mem in $ENS_LIST; do
    m=$(printf "%04g" $((10#$m+1)))
    ln -s ../../../da.prep/bkg_rst/mem_$mem/MOM.res.nc INPUT/gues/$m.nc

    if [[ $DA_TS_ONLY -eq 0 ]]; then
	ln -s ../../../da.prep/bkg_rst/mem_$mem/MOM.res_1.nc INPUT/gues/$m.1.nc
    fi
#    ln -s ../../../da.prep/bkg_rst/mem_$mem/MOM.res_1.nc INPUT/gues/$m.2.nc
done

# observations
mkdir -p INPUT/obsop
m=0
for mem in $ENS_LIST; do
    m=$(printf "%04g" $((10#$m+1)) )
    ln -s ../../../da.prep/omf/mem_$mem/obs.nc INPUT/obsop/$m.dat
done


#ana ens files
mkdir -p $ANA_FILES
ln -s $ANA_FILES OUTPUT
#mkdir -p OUTPUT
#for mem in $ENS_LIST; do
#    m=$(printf "%04g" $((10#$m+1)) )
#    ln -s $ANA_FILES/$m.nc OUTPUT/$m.nc
#done

# run the LETKF
aprun -n $nproc ./letkf

# move output files to OUPUT directory
# (this is temporary, there is a bug in the LETKF code)
mv {ana,bkg,letkf}.*.nc OUTPUT/
cd OUTPUT/
for f in *; do
    mv $f ${f//.*}_${f#*.}
done
for f in ana_0*; do
    mv $f ${f#*_}
done
