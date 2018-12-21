#!/bin/bash
set -e
cat << EOF

#================================================================================
#================================================================================
# NCEP Hybrid-GODAS  -  fcst.run.sh
#   MOM6 ocean forecast ensemble member model run
#================================================================================
EOF
#
# Prerequisites:
#  * The required atmospheric forcing files have been created in $FORC_DIR
#
# Results:
#  * MOM6 forecasts will run in the $WORK_DIR location.
#  * The restart files will be moved to $RST_DIR
#  * other output files will NOT be moved out of the working directory by this
#    script, other jobsteps handle that task
#
# Required environment variables:
 envar=()
 envar+=("WORK_DIR")         # The directory that the forecast will be run in
 envar+=("FORC_DIR")         # directory for the generated surface forcings
 envar+=("MOM_CFG_DIR")
 envar+=("MOM_INPUT_DIR")
 envar+=("RST_DIR_IN")
 envar+=("RST_DIR_OUT")
 envar+=("PPN")              # Number of cores per node (procs per node)
 envar+=("NODES")            # Number of nodes to use for MPI enabled forecast
 envar+=("FCST_RST_TIME")    # datetime for forecast restart outptut (YYYYMMDDHH)
 envar+=("FCST_START_TIME")  # datetime for start of forecast (YYYYMMDDHH)
 envar+=("FCST_LEN")         # length of forecast (hours)
 envar+=("FCST_RESTART")     # 1 if yes, 0 if no
 envar+=("FCST_DIAG_DA")     # 1 if the diagnostic output required for DA is to be saved
 envar+=("FCST_DIAG_OTHER")  # 1 if other diag output (not needed for DA) is to be saved
 envar+=("MOM6_EXE")
 envar+=("NIPROC")
 envar+=("NJPROC")
 envar+=("MASKTABLE")
#================================================================================
#================================================================================


# make sure required env vars exist
for v in ${envar[@]}; do
    if [[ -z "${!v}" ]]; then
	echo "ERROR: env var $v is not set."; exit 1
    fi
    echo " $v = ${!v}"
done
echo ""
set -u


# calculate number of cores needed
NPROC=$(($PPN * $NODES))
echo "Running with $NPROC cores"


# setup the foreacst directory
#------------------------------------------------------------
echo "Setting up forecast working directory in:"
echo " $WORK_DIR"
if [[ -e "$WORK_DIR" ]]; then
    echo "WARNING: $WORK_DIR already exists, removing."
    rm -rf $WORK_DIR
fi
mkdir -p $WORK_DIR
cd $WORK_DIR

mkdir -p OUTPUT
ln -s $FORC_DIR FORC
ln -s $MOM6_EXE .

# namelist files
ln -s $MOM_CFG_DIR/* .
source diag_table.sh > diag_table
source input.nml.sh > input.nml

# static input files
mkdir -p INPUT
ln -s $MOM_INPUT_DIR/* INPUT/

# link restart files
ln -s $RST_DIR_IN ./RESTART_IN

# output directory for restart files
mkdir -p $RST_DIR_OUT
ln -s $RST_DIR_OUT RESTART

# processor layouts
cat > MOM_layout << EOF 
  IO_LAYOUT=${NIPROC},${NJPROC}
  LAYOUT=${NIPROC},${NJPROC}
  MASKTABLE="${MASKTABLE}"
EOF
cp MOM_layout SIS_layout


# run the forecast
#------------------------------------------------------------
echo "running MOM..."
aprun -n $NPROC ./mom6


# the intermediate restart files are being a little weird currently.
# Do this the messy way until I have time to look into the MOM/SIS code.
# Rename the intermediate restart files after the model finishes...
cd RESTART
for f in ????????.????00.*.res*; do 
    mv $f ${f:16}
done

# all done with forecast, leave a signal to other cycles watching
touch $WORK_DIR/fcst_done
