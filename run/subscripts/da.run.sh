#!/bin/bash
set -e

cat <<EOF



#================================================================================
#================================================================================
# Hybrid-GODAS   -  da.run.sh
#  Run the data assimilation step (with options of 3dvar/LETKF/hybrid-gain)
#================================================================================
#================================================================================

EOF

envar=()
#envar+=("PPN")
envar+=("CYCLE")
envar+=("DA_SLOTS")
envar+=('DA_MODE')
envar+=("WORK_DIR")
envar+=("ENS_SIZE")
envar+=("ENS_LIST")
envar+=("BIN_DIR")
envar+=("DA_CFG_DIR")
envar+=("GRID_DIR")
envar+=("SCRIPT_DIR")
envar+=("LOG_DIR")

# make sure required env vars exist
for v in ${envar[@]}; do
    if [[ -z "${!v}" ]]; then
	echo "ERROR: env var $v is not set."; exit 1
    fi
    echo " $v = ${!v}"
done
set -u
echo -e "\n================================================================================\n"


# setup working directory
if [[ -e $WORK_DIR ]]; then rm -r $WORK_DIR; fi
mkdir -p $WORK_DIR
cd $WORK_DIR


# determine if the files needed for da are present
do_da=1
for mem in $ENS_LIST; do
    if [[ ! -d $WORK_DIR/../da.prep/bkg/mem_$mem/fcst.diag ]]; then
	do_da=0
    fi
done
if [[ $do_da == 0 ]]; then
    echo "WARNING: no DA will be performed because the forecast diag files were not found. (this is normal for the first cycle)"
    exit 0
fi


#ln -s $DA_CFG_DIR/obsprep.nml .
mkdir INPUT
ln -s $GRID_DIR/da_{hgrid,vgrid,coast_dist}.nc INPUT/


# finish setting up working directory
for mem in $ENS_LIST; do
    mkdir -p mem_$mem
    ln -s $WORK_DIR/../da.prep/bkg/mem_$mem/fcst.rst mem_$mem/fcst.rst
    mkdir $WORK_DIR/mem_$mem/letkf
done


dtz(){ echo ${1:0:8}Z${1:8:10}; }



#------------------------------------------------------------
# observation operator
#------------------------------------------------------------

# generate the list of slot/mem combinations
# and setup working directories
sm=()
for slot_offset in $DA_SLOTS; do
    slot=$(date "+%Y%m%d" -d "$(dtz $CYCLE) + $slot_offset hours")
    for mem in $ENS_LIST; do
	# setup working directory
	d=$WORK_DIR/mem_$mem/obsop/$slot
	mkdir -p $d

	sm+=("$slot/$mem")
    done
done

# submit in batches, one script per node
# TODO, enable multiple runs per script, again
echo "running observation operators..."
echo "  NOTE: additional output placed in \$LOG_DIR/obsop/#slot#/#mem#.log"
idx=0
while [[ $idx -lt ${#sm[@]} ]]; do
    s=${sm[@]:$idx:1}
    $SCRIPT_DIR/da.obsop.sh $s
    ((idx=idx+1))
done

#
# idx=0
# while [[ $idx -lt ${#sm[@]} ]]; do
#     s=${sm[@] :$idx:$PPN}
#     aprun -n 1 -d $PPN $SCRIPT_DIR/da.obsop.sh $s &
#     idx=$(( $idx + $PPN ))
# done
# wait


# combine all the slots for a given member
# TODO, enable multiple runs per script, again
echo ""
echo "combining slots for each member..."
echo "  NOTE: additional output placed in \$LOG_DIR/obscomb/#mem#.log"
ens_list=($ENS_LIST)
idx=0
while [[ $idx -lt ${#ens_list[@]} ]]; do
    s=${ens_list[@]:$idx:1}
    $SCRIPT_DIR/da.obsop.comb.sh $s
    ((idx=idx+1))
#    idx=$(( $idx + $PPN ))
done


# idx=0
# while [[ $idx -lt ${#ens_list[@]} ]]; do
#     s=${ens_list[@] :$idx:$PPN}
#     aprun -n 1 -d $PPN $SCRIPT_DIR/da.obsop.comb.sh $s &
#     idx=$(( $idx + $PPN ))
# done
# wait


# if [[ $do_DA == 0 ]]; then
#     echo "WARNING: skipping DA because there are no observations and/or background files."
#     exit 0
# fi



#--------------------------------------------------------------------------------
#--------------------------------------------------------------------------------


# determine the ids of the restart tiles
do_tiles=0
if [[ -e $WORK_DIR/mem_0000/fcst.rst/MOM.res.nc.0000 ]]; then
    do_tiles=1
    tiles=()
    for f in $WORK_DIR/mem_0000/fcst.rst/MOM.res.nc.*; do
	tiles+=(${f##*.})
	echo "res ${f##*.}"
    done
    echo -e "\nRestart files have ${#tiles[@]} tiles. Running multiple instances of LETKF"
else
    echo -e "\nrestart file is NOT tiled, only running one instance of LETKF"
fi


#------------------------------------------------------------
# LETKF
#------------------------------------------------------------
if [[ "$DA_MODE" == "hyb" || "$DA_MODE" == "ekf" ]]; then
    cd $WORK_DIR
    mkdir -p mem_{mean,sprd}/letkf
    mkdir -p letkf.log
    mkdir -p letkf.diag
    mkdir -p letkf.cfg

    # # IF TILED
    # for t in ${tiles[@]}; do
    # 	# get the tile position in the grid
    # 	tile_x=$(ncks -m -v lonh $WORK_DIR/mem_0000/fcst.rst/MOM.res.nc.$t | grep domain_decomposition | cut -d ' ' -f 13- )
    # 	tile_y=$(ncks -m -v lath $WORK_DIR/mem_0000/fcst.rst/MOM.res.nc.$t | grep domain_decomposition | cut -d ' ' -f 13- )

    # 	# generate the letkf yaml file
    # 	sed -e "s/#ENS_SIZE#/$ENS_SIZE/" -e "s/#TILE_NUM#/$t/" -e "s/#TILE_X#/$tile_x/" -e "s/#TILE_Y#/$tile_y/" $DA_CFG_DIR/letkf.yaml > $WORK_DIR/letkf.cfg/$t.yaml
    # done
    sed -e "s/#ENS_SIZE#/$ENS_SIZE/" -e "s/#TILE_NUM#//" -e "s/#TILE_X#//" -e "s/#TILE_Y#//" $DA_CFG_DIR/letkf.yaml > $WORK_DIR/letkf.cfg/letkf.yaml


    
    # # submit in batches, one script per node
    # TODO
    echo -e "\nRunning LETKF..."

    # setup log file
    log=$LOG_DIR/letkf.log
    mkdir -p $(dirname $log)
    echo "  NOTE: additional output placed in \$LOG_DIR/$(basename $log)"

    # idx=0
    # export OMP_NUM_THREADS=1
    # for t in ${tiles[@]}; do
    # 	aprun -n $PPN $BIN_DIR/letkfdriver letkf.cfg/$t.yaml &> letkf.log/$t.log &
    # done
    # wait
    ${MPIEXEC} $BIN_DIR/letkfdriver letkf.cfg/letkf.yaml &> $log
    
    # ensemble member 0000 actually uses the ensemble mean, so link those files
    cd $WORK_DIR/mem_0000/letkf
    for f in $WORK_DIR/mem_mean/letkf/ana.*; do
    	f2=${f##*/}
    	ln -s $f $f2
    done

    # # add required netcdf domain_decomposition attributes to letkf files
    # cd ../../
    # for t in ${tiles[@]}; do
    # 	# get the tile position in the grid
    # 	decomp_x=$(ncks -m -v lonh $WORK_DIR/mem_0000/fcst.rst/MOM.res.nc.$t | grep domain_decomposition | cut -d ' ' -f 11- )
    # 	decomp_y=$(ncks -m -v lath $WORK_DIR/mem_0000/fcst.rst/MOM.res.nc.$t | grep domain_decomposition | cut -d ' ' -f 11- )

    # 	for ms in mean sprd; do
    # 	    for ba in ana bkg; do
    # 		ncatted -a domain_decomposition,lon,o,i,"$decomp_x" -a domain_decomposition,lat,o,i,"$decomp_y" -O $WORK_DIR/mem_$ms/letkf/$ba.nc.$t
    # 	    done
    # 	done
    # done

fi


#--------------------------------------------------------------------------------
# perform observation operator on the analysis ensemble mean
#--------------------------------------------------------------------------------
if [[ "$DA_MODE" == "hyb" ]]; then
    echo ""
    echo "Rerunning the observation operator on LETKF analysis..."


    mkdir $WORK_DIR/obsop.var
    cd $WORK_DIR/obsop.var
    ln -s $WORK_DIR/INPUT .
    ln -s $DA_CFG_DIR/obsprep.nml .

    bkg_files=../mem_mean/letkf/ana.*
    obs_file=$WORK_DIR/mem_0000/obsop/obs.nc

    # setup log file
    log=$LOG_DIR/obsop.var.log
    mkdir -p $(dirname $log)
    echo "  NOTE: additional output placed in \$LOG_DIR/$(basename $log)"

    # run observation operator
    # TODO, parallel   
    #aprun -n 1 $BIN_DIR/obsop $obs_file $bkg_files obs.nc > obsop.log  &
    $BIN_DIR/obsop $obs_file $bkg_files obs.nc &> $log
    
elif [[ "$DA_MODE" == "var" ]]; then
    # just link the directory
    mkdir $WORK_DIR/obsop.var
    cd $WORK_DIR/obsop.var
    ln -s ../mem_0000/obsop/obs.nc .
    # TODO, use s better path
    bkg_files=../../da.prep/bkg/mem_0000/fcst.diag/*.ocean_da_*.nc
fi


#--------------------------------------------------------------------------------
# run 3DVar
#--------------------------------------------------------------------------------
if [[ "$DA_MODE" == "var" || "$DA_MODE" == "hyb" ]]; then

    mkdir $WORK_DIR/var
    cd $WORK_DIR/var

    mkdir INPUT
    ln -s $GRID_DIR/da_{hgrid,vgrid,coast_dist}.nc INPUT/
    ln -s $DA_CFG_DIR/*.3dvar .
    ln -s ../obsop.var/obs.nc .

    # combine the background files, if needed
    if [[ $do_tiles == 1 ]]; then
	echo "combining background tiles for 3dvar..."
	aprun -n 1 $BIN_DIR/mppnccombine -64 bkg.nc $bkg_files &
	wait
    else
	ln -s $bkg_files bkg.nc
    fi

    echo -e "\nRunning 3dvar..."

    # setup log file
    log=$LOG_DIR/var.log
    mkdir -p $(dirname $log)
    echo "  NOTE: additional output placed in \$LOG_DIR/$(basename $log)"
    
    $MPIEXEC $BIN_DIR/3dvardriver &> $log
fi
