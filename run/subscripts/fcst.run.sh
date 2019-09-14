#!/bin/bash
set -e
cat << EOF

#================================================================================
#================================================================================
# Hybrid-GODAS  -  fcst.run.sh
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
# TODO throw an error if expecting a restart file and there is not one
# Required environment variables:
 envar=()
 envar+=("WORK_DIR")         # The directory that the forecast will be run in
 envar+=("FORC_DIR")         # directory for the generated surface forcings
 envar+=("FCST_DONE")
 envar+=("BIN_DIR")
 envar+=("MOM_CFG_DIR")
 envar+=("MOM_INPUT_DIR")
 envar+=("RST_DIR_IN")
 envar+=("RST_DIR_OUT")
 # envar+=("DIAG_DIR_OUT")
 # envar+=("LETKF_ANA_DIR")
 # envar+=("SCRIPT_DIR")
 envar+=("DA_MODE")
 if [[ $DA_MODE != "none" ]]; then
     envar+=("VAR_ANA_DIR")
 fi
     
 # envar+=("DA_CFG_DIR")
 # envar+=("PPN")              # Number of cores per node (procs per node)
 # envar+=("NODES")            # Number of nodes to use for MPI enabled forecast
 # envar+=("FCST_RST_TIME")    # datetime for forecast restart outptut (YYYYMMDDHH)
 # envar+=("FCST_START_TIME")  # datetime for start of forecast (YYYYMMDDHH)
 # envar+=("FCST_DIAG_START_TIME")
 # envar+=("FCST_LEN")         # length of forecast (hours)
 # envar+=("FCST_DIAG_DA")     # 1 if the diagnostic output required for DA is to be saved
 # envar+=("FCST_DIAG_OTHER")  # 1 if other diag output (not needed for DA) is to be saved
 # envar+=("MOM6_EXE")
 # envar+=("IO_LAYOUT")
 # envar+=("NIPROC")
 # envar+=("NJPROC")
 # envar+=("MASKTABLE")
 # envar+=("ALLOW_COLDSTART")
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


## calculate number of cores needed
#NPROC=$(($PPN * $NODES))
#echo "Running with $NPROC cores"


# setup the foreacst directory
#------------------------------------------------------------
echo "Setting up forecast working directory in:"
echo " $WORK_DIR"
if [[ -e "$WORK_DIR" ]]; then
    echo -e "\nWARNING: $WORK_DIR already exists, removing.\n"
    rm -rf $WORK_DIR
fi
mkdir -p $WORK_DIR
cd $WORK_DIR

mkdir -p OUTPUT
ln -s $FORC_DIR FORC
ln -s $MOM6_EXE .

# namelist files
ln -s $MOM_CFG_DIR/* .

# static input files
mkdir -p INPUT
ln -s $MOM_INPUT_DIR/* INPUT/

# output directory for restart files
d=${RST_DIR_OUT/\#CYCLE_NEXT\#/$CYCLE_NEXT}
mkdir -p $d
ln -s $d RESTART

# output directory for diag files
d=${DIAG_DIR_OUT/\#CYCLE_NEXT\#/$CYCLE_NEXT}
mkdir -p $d
ln -s $d DIAG

# processor layouts
if [[ "$SET_MOM_PE" == 1 ]]; then
  cat > MOM_layout << EOF
    IO_LAYOUT=${IO_LAYOUT}
    LAYOUT=${NIPROC},${NJPROC}
    MASKTABLE="${MASKTABLE}"
EOF
else
  touch MOM_layout
fi
cp MOM_layout SIS_layout


# determine source of restart files
# (if restart files are not present, do a coldstart from climatology)
# (IF DA increment present, apply to restart files)
#------------------------------------------------------------
shopt -s nullglob
FCST_RESTART=1
if [[ ! -e $RST_DIR_IN ]]; then
    # case of no restart file found
    if [[ $ALLOW_COLDSTART != 0 ]]; then
      FCST_RESTART=0
      echo -e "\nWARNING: no restart files found, starting from T/S initial conditions\n"
      ln -s $IC_FILE INPUT/ic.nc
    else
      echo -e "\nERROR: no restart files found. Link restart files, or set ALLOW_COLDSTART to start from clim.\n"
      exit 1
    fi

elif [[ "$DA_MODE" == "none" ]]; then
    # restart files found, but not doing a DA update
    echo "Linking restart files from $RST_DIR_IN"
    ln -s $RST_DIR_IN ./RESTART_IN

else
    # restart files found, and DA increment to be applied
    echo "Linking restart files from $RST_DIR_IN and applying DA increment"

    rst_letkf=0
    rst_var=0

    mkdir RESTART_IN
    mkdir rst_update.log

    # if doing letkf find the letkf analysis files
    letkf_files=()
    if [[ "$DA_MODE" != "var" ]]; then
      letkf_files=($LETKF_ANA_DIR/ana.*)
      if [[ ${#letkf_files[@]} == 0 ]]; then
        echo -e "\n\nWARNING: no LETKF analysis files found."
      else
        rst_letkf=1
      fi
    fi

    # if doing var find the var files
    var_files=()
    if [[ "$DA_MODE" != "ekf" ]]; then
      var_files=($VAR_ANA_DIR/*.nc)
      if [[ ${#var_files[@]} == 0 ]]; then
        echo -e "\nWARNING: no 3DVAR analysis increment files found."
      else
        rst_var=1
      fi
    fi

    # run the rst_update scripts
    # TODO, fix tiling
    # if [[ $rst_var != 0 || $rst_letkf != 0 ]]; then
    #   echo "Updating restart files with DA analysis..."
    #   tiles=()
    #   for f in ${letkf_files[@]}; do
    #     tiles+=(${f##*.})
    #   done
    #   ln -s $DA_CFG_DIR/rst_update.nml .
    #   idx=0
    #   while [[ $idx -lt ${#tiles[@]} ]]; do
    # 	  # TODO, run on MOM.res_1, MOM.res_2 as well
    # 	  t=${tiles[@] :$idx:$PPN}
    # 	  aprun -n 1 -d $PPN $SCRIPT_DIR/fcst.rst_update.sh $t &
    # 	  idx=$(( $idx + $PPN ))
    #   done
    #   wait
    # fi
    if [[ $rst_var != 0 || $rst_letkf != 0 ]]; then
	export TILED_IO=0
	ln -s $DA_CFG_DIR/rst_update.nml .
	$SCRIPT_DIR/fcst.rst_update.sh
    fi
    
    # link any other file that was not updated
    echo "Linking other restart files no to be updated..."
    for f in ${RST_DIR_IN}/*; do
	f2=${f##*/}
	if [[ ! -e RESTART_IN/$f2 ]]; then
	    ln -s $f RESTART_IN/$f2
	fi
    done

fi

# write out the config files, with tokens subsituted for actual values
export FCST_RESTART
source diag_table.sh > diag_table
source input.nml.sh > input.nml


# run the forecast
#------------------------------------------------------------
echo "running MOM..."
OMP_NUM_THREADS=1
${MPIEXEC} ./mom6 

# move the diag files
mv *.ocean_da*.nc DIAG/

# the intermediate restart files are being a little weird currently.
# Do this the messy way until I have time to look into the MOM/SIS code.
# Rename the intermediate restart files after the model finishes...
cd RESTART
shopt -s nullglob
for f in ????????.????00.*.res*; do
    mv $f ${f:16}
done

# all done with forecast, leave a signal to other cycles watching
d=${FCST_DONE/\#CYCLE_NEXT\#/$CYCLE_NEXT}
touch $d
