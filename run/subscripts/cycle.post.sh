#!/bin/bash
set -e
cat << \#\#
#================================================================================
#================================================================================
# NCEP Hybrid-GODAS  -  cycle.post.sh
#   Post processing of data assimilation / forecasting results
#================================================================================
##
# Required environment variables:
 envar=()
 envar+=("ROOT_GODAS_DIR")
 envar+=("ROOT_EXP_DIR")
 envar+=("JOB_WORK_DIR")
 envar+=("ENS_LIST")
 envar+=("DA_MODE")
 envar+=("DA_HYB_ALPHA")
 envar+=("FCST_RST_TIME")

 envar+=("SAVE_FORMAT_GRID")
 envar+=("SAVE_COMP_LEVEL")

 envar+=("SAVE_BKG_ENS")
 envar+=("SAVE_BKG_SLOT")
 envar+=("SAVE_BKG_MEAN")
 envar+=("SAVE_BKG_SPRD")

 envar+=("SAVE_ANA_ENS")
 envar+=("SAVE_ANA_MEAN")
 envar+=("SAVE_ANA_MEAN_LETKF")
 envar+=("SAVE_ANA_SPRD")

 envar+=("SAVE_OMF")
 envar+=("SAVE_OMA")

 envar+=("SAVE_RST_ANNUAL")
 envar+=("SAVE_RST_CYCLES")

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

comp2=Temp/3,Salt/3,SSH/4
comp3=Temp/2,Salt/2,SSH/3
tools_dir=$ROOT_GODAS_DIR/tools/postproc


#--------------------------------------------------------------------------------
# function that does the actual masking / regridding / compression of the 3d output
function proc(){
    echo ""
    echo "processing:"
    echo "  $1"
    echo "  $2"
    echo "  $3 $4"

    # make the output directory
    dout=$(dirname $2)
    mkdir -p $dout

    # file masking
    if [[ "$3" -gt 0 ]]; then
	$tools_dir/mask.py $1 $2.tmp \
           -geomFile $ROOT_GODAS_DIR/DATA/grid/ocean_geometry.nc \
	   -vertFile $ROOT_GODAS_DIR/DATA/grid/Vertical_coordinate.nc
    else
	ln -f $1 $2.tmp
    fi
    
    # file regridding
    ln -f $2.tmp $2.tmp2

    # file compression
    truncations=""
    compression=""
    remove=""
    if [[ "$4" -gt 0 ]]; then
	compression=" -compression $SAVE_COMP_LEVEL "
    fi

    if [[ "$4" -eq 2 ]]; then
	truncations=" -lsd $comp2 "
    elif [[ "$4" -eq 3 ]]; then
	truncations=" -lsd $comp3 "
    fi

    
    if [[ "$?" -gt 4 ]]; then
	if [[ "$5" ]]; then
	    remove=" -remove $5"
	fi
    fi

    if [[ $truncations || $compression ]]; then
	$tools_dir/compress.py $compression $truncations $remove $2.tmp2 $2
    else
	mv $2.tmp2 $s
    fi

    rm -f $2.tmp $2.tmp2
}



# Things that are done only for ensemble DA
#--------------------------------------------------------------------------------
if [[ ( "$DA_MODE" == "hyb" ) || ( "$DA_MODE" == "ekf" ) ]]; then

    # ensemble background mean
    if [[ "$SAVE_BKG_MEAN" -gt 0 ]]; then
	ifile=$JOB_WORK_DIR/da.letkf/OUTPUT/bkg_mean.nc
	ofile=$ROOT_EXP_DIR/bkg/mean/${FCST_RST_TIME:0:4}/${FCST_RST_TIME}.nc
	proc $ifile $ofile 1 3
    fi

    # ensemble background spread
    if [[ "$SAVE_BKG_SPRD" -gt 0 ]]; then
	ifile=$JOB_WORK_DIR/da.letkf/OUTPUT/bkg_sprd.nc
	ofile=$ROOT_EXP_DIR/bkg/sprd/${FCST_RST_TIME:0:4}/${FCST_RST_TIME}.nc
	proc $ifile $ofile 1 2
    fi

    # ensemble analysis mean
    if [[ "$SAVE_ANA_MEAN" -gt 0 ]]; then
    	ofile=$ROOT_EXP_DIR/ana/mean/${FCST_RST_TIME:0:4}/${FCST_RST_TIME}.nc	
    	ifile=$JOB_WORK_DIR/da.letkf/OUTPUT/ana_mean.nc
    	mkdir -p $(dirname $ofile)
    	if [[ "$DA_MODE" == "hyb" ]]; then
    	    ifile2=$JOB_WORK_DIR/da.3dvar/ana_inc.nc	    
    	    cdo add -mulc,$DA_HYB_ALPHA $ifile2 $ifile $ofile.tmp0
    	    proc $ofile.tmp0 $ofile 1 3
    	    rm $ofile.tmp0
    	else
    	    proc $file $ofile 1 3
    	fi
    fi

    # ensemble analysis mean (from the intermediate LETKF part of they hybrid)
    if [[ ( "$SAVE_ANA_MEAN_LETKF" -gt 0 ) && ( "$DA_MODE" == "hyb" ) ]]; then
	ifile=$JOB_WORK_DIR/da.letkf/OUTPUT/ana_mean.nc
	ofile=$ROOT_EXP_DIR/ana/mean_letkf/${FCST_RST_TIME:0:4}/${FCST_RST_TIME}.nc
	proc $ifile $ofile 1 3
    fi

    # ensemble analysis spread
    if [[ "$SAVE_ANA_SPRD" -gt 0 ]]; then
	ifile=$JOB_WORK_DIR/da.letkf/OUTPUT/ana_sprd.nc
	ofile=$ROOT_EXP_DIR/ana/sprd/${FCST_RST_TIME:0:4}/${FCST_RST_TIME}.nc
	proc $ifile $ofile 1 2
    fi
fi



# things thare are done for any DA type
# note that for 3dvar, there is "one" ensemble member
#--------------------------------------------------------------------------------
echo "Saving ensemble data..."
for ens in ${ENS_LIST}; do
    echo ""
    echo " ensemble member: $ens"

    # ensemble member background at analysis time
    if [[ "$SAVE_BKG_ENS" -gt 0 ]]; then
	ifile=$JOB_WORK_DIR/da.prep/bkg/mem_$ens/${DA_WNDW_CNTR_TIME:0:8}.nc
	ofile=$ROOT_EXP_DIR/bkg/ens/$ens/${DA_WNDW_CNTR_TIME}.nc 
#	proc $ifile $ofile 0 3 rhopot0
	echo "ERROR: background at analysis time not being save"
	exit 1
    fi

    #
    if [[ "$SAVE_BKG_SLOT" -gt 0 ]]; then
	out_dir=$ROOT_EXP_DIR/bkg/ens_slots/$ens
	echo "ERROR: background ensemble slots not being saved"
	exit 1
    fi

    #
    if [[ "$SAVE_ANA_ENS" -gt 0 ]]; then
	out_dir=$ROOT_EXP_DIR/ana/ens/$ens
	echo "ERROR: analysis ensemble slots not being saved."
	exit 1
    fi
done


# Save observation minus background stats
if [[ "$SAVE_OMF" -gt 0 ]]; then
    echo "Saving OmF..."
    ofile=$ROOT_EXP_DIR/omf/${CYCLE:0:4}/${CYCLE}.nc 
    mkdir -p $(dirname $ofile)

    #TODO: fix this to use combined obs (all slots in one file)
    e=( $ENS_LIST )
    e=${e[0]}
    ifiles=$JOB_WORK_DIR/da.prep/omf/mem_*/obs.nc
    ifile0=$JOB_WORK_DIR/da.prep/omf/mem_$e/obs.nc

    # calculate the ensemble members / mean /spread
    # for the increments   
    ncecat -h -O $ifiles -v inc $ofile.ens.tmp
    ncwa -h -O -a record $ofile.ens.tmp $ofile.mean.tmp
    ncbo -h -O $ofile.ens.tmp $ofile.mean.tmp $ofile.sprd.tmp
    ncra -h -O -y rmssdn $ofile.sprd.tmp $ofile.sprd.tmp
    ncwa -h -O -a record $ofile.sprd.tmp $ofile.sprd.tmp

    # combine things
    #... i have no idea why its being weird
    ncrename -h -O -v inc,inc_mean $ofile.mean.tmp $ofile.mean.tmp
    ncrename -h -O -v inc,inc_sprd $ofile.sprd.tmp $ofile.sprd.tmp
    ncrename -h -O -d record,mem $ofile.ens.tmp $ofile.ens.tmp
    ncks -h -O --fix_rec_dmn all $ofile.ens.tmp $ofile.ens.tmp
    mv $ofile.ens.tmp $ofile.tmp
    ncks -h -A $ofile.mean.tmp $ofile.tmp
    ncks -h -A $ofile.sprd.tmp $ofile.tmp    
    vars="time obid plat lat lon depth hr val err qc"
    for v in $vars; do
	ncks -h --fix_rec_dmn all -v $v $ifile0 $ofile.$v.tmp
	ncks -h -A $ofile.$v.tmp $ofile.tmp
    done
    nccopy -d 8 -s $ofile.tmp $ofile

    rm $ofile.tmp
    rm $ofile.*.tmp
fi
