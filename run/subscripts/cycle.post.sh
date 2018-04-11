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
 envar+=("CYCLE")

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

#TODO: there is inconsistency in variables names (e.g. Temp vs temp) fix this
comp2=Temp/3,Salt/3,SSH/4,u/3,v/3
comp3=Temp/2,Salt/2,SSH/3,u/2,v/2,\
temp/2,salt/2,ssh/3,\
mld_003/1,mld_0125/1,taux/2,tauy/2,speed/2,ssh/3,lh/1,lw/1,sh/1,net_heat_coupler/1,\
cn/2,siu/3,siv/3,sisnconc/2,siconc/2,hs/2,hi/2

tools_dir=$ROOT_GODAS_DIR/tools/postproc

# create temporary working directory
work_dir=$JOB_WORK_DIR/cycle.post
if [[ -d "$work_dir" ]]; then
    echo "Deleting old working directory"
    rm -r $work_dir
fi
mkdir -p $work_dir



#================================================================================
# procGrid(): function that does the actual masking / regridding / compression
#   of the 3d output files
# TODO: enable the actual regridding code
#================================================================================
function procGrid(){
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
    # if [[ true ]]; then
    # 	$ROOT_GODAS_DIR/tools/postproc/rotUV.py -u u -v v $2.tmp $2.uv
    # 	infile=$2.tmp \
    # 	    outfile=$2.tmp.t.nc \
    # 	    vars=speed,ssh \
    # 	    weights=$ROOT_GODAS_DIR/tools/postproc/weights/remap.grid_t.05deg.con.nc \
    # 	    ncl $ROOT_GODAS_DIR/tools/postproc/remap.ncl
    # 	infile=$2.uv \
    # 	    outfile=$2.tmp.uv.nc \
    # 	    vars=u,v \
    # 	    weights=$ROOT_GODAS_DIR/tools/postproc/weights/remap.grid_t.05deg.bil.nc \
    # 	    ncl $ROOT_GODAS_DIR/tools/postproc/remap.ncl
    # fi
    ln -f $2.tmp $2.tmp2

    # file compression
    truncations=""
    compression=""
    remove=""
    if [[ "$4" -gt 0 ]]; then
	compression=" -compression $SAVE_COMP_LEVEL "
    fi
    case "$4" in
	2)  truncations=" -lsd $comp2 " ;;
	3)  truncations=" -lsd $comp3 " ;;
    esac

    if [[ "$#" -gt 4 ]]; then
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
#================================================================================




#--------------------------------------------------------------------------------
# Save ensemble background/analysis mean/spread
#--------------------------------------------------------------------------------
if [[ ( "$DA_MODE" == "hyb" ) || ( "$DA_MODE" == "ekf" ) ]]; then

    # ensemble background mean (as reported by the LETKF)
    if [[ "$SAVE_BKG_MEAN" -gt 0 ]]; then
	ifile=$JOB_WORK_DIR/da.letkf/OUTPUT/bkg_mean.nc
	ofile=$ROOT_EXP_DIR/output/bkg/mean/${FCST_RST_TIME:0:4}/${FCST_RST_TIME}.nc
	procGrid $ifile $ofile 1 3
    fi

    # ensemble background spread (as reported by the LETKF)
    if [[ "$SAVE_BKG_SPRD" -gt 0 ]]; then
	ifile=$JOB_WORK_DIR/da.letkf/OUTPUT/bkg_sprd.nc
	ofile=$ROOT_EXP_DIR/output/bkg/sprd/${FCST_RST_TIME:0:4}/${FCST_RST_TIME}.nc
	procGrid $ifile $ofile 1 2
    fi

    # ensemble analysis mean (as reported by the LETKF, plus the increment from the VAR)
    if [[ "$SAVE_ANA_MEAN" -gt 0 ]]; then
    	ofile=$ROOT_EXP_DIR/output/ana/mean/${FCST_RST_TIME:0:4}/${FCST_RST_TIME}.nc	
    	ifile=$JOB_WORK_DIR/da.letkf/OUTPUT/ana_mean.nc
    	mkdir -p $(dirname $ofile)
    	if [[ "$DA_MODE" == "hyb" ]]; then
    	    ifile2=$JOB_WORK_DIR/da.3dvar/ana_inc.nc	    
	    $ROOT_GODAS_DIR/tools/rst_update.py -vars Temp,Salt $ifile $ofile.tmp0 -alpha $DA_HYB_ALPHA -var $ifile2
    	    procGrid $ofile.tmp0 $ofile 1 3
    	    rm $ofile.tmp0
    	else
    	    procGrid $ifile $ofile 1 3
    	fi
    fi

    # ensemble analysis mean (from the intermediate LETKF part of they hybrid)
    if [[ ( "$SAVE_ANA_MEAN_LETKF" -gt 0 ) && ( "$DA_MODE" == "hyb" ) ]]; then
	ifile=$JOB_WORK_DIR/da.letkf/OUTPUT/ana_mean.nc
	ofile=$ROOT_EXP_DIR/output/ana/mean_letkf/${FCST_RST_TIME:0:4}/${FCST_RST_TIME}.nc
	procGrid $ifile $ofile 1 3
    fi

    # ensemble analysis spread (as reported by the LETKF)
    if [[ "$SAVE_ANA_SPRD" -gt 0 ]]; then
	ifile=$JOB_WORK_DIR/da.letkf/OUTPUT/ana_sprd.nc
	ofile=$ROOT_EXP_DIR/output/ana/sprd/${FCST_RST_TIME:0:4}/${FCST_RST_TIME}.nc
	procGrid $ifile $ofile 1 2
    fi
fi



#--------------------------------------------------------------------------------
# Save 3dvar background/analysis
#--------------------------------------------------------------------------------
if [[ "$DA_MODE" == "var" ]]; then

    ofileb=$ROOT_EXP_DIR/output/bkg/mean/${FCST_RST_TIME:0:4}/${FCST_RST_TIME}.nc	

    if [[ "$SAVE_BKG_MEAN" -gt 0 || "$SAVE_ANA_MEAN" -gt 0 ]]; then
	ifileb=$JOB_WORK_DIR/da.prep/bkg_rst/mem_0000/MOM.res.nc
    	mkdir -p $(dirname $ofileb)
	cdo select,name=Temp,Salt $ifileb $ofileb.tmp0
	procGrid $ofileb.tmp0 $ofileb 1 2
    fi

    if [[ "$SAVE_ANA_MEAN" -gt 0 ]]; then
	ifile=$JOB_WORK_DIR/da.3dvar/ana_inc.nc
	ofile=$ROOT_EXP_DIR/output/ana/mean/${FCST_RST_TIME:0:4}/${FCST_RST_TIME}.nc	
    	mkdir -p $(dirname $ofile)
	cdo add $ofileb.tmp0 $ifile $ofile.tmp0
	procGrid $ofile.tmp0 $ofile 1 2
	rm $ofile.tmp0
    fi    
    rm -f $ofileb.tmp0
fi


#--------------------------------------------------------------------------------
# save the extra forecast diagnostics. (ensemble member 0000)
#  this is the forecast from the "mean" state if doing ensemble da
# TODO: right now this is hardcoded for "pentad" files, but this should 
#  be generalized to any extra diagnostic files
#--------------------------------------------------------------------------------
echo ""
echo "Saving mean forecast (member 0000)"
$ROOT_GODAS_DIR/build/mppnccombine -m -64 $work_dir/pentad.ice.nc \
    $JOB_WORK_DIR/fcst.run/mem_0000/*.ice_pentad_*
procGrid $work_dir/pentad.ice.nc $ROOT_EXP_DIR/output/bkg_diag/${FCST_RST_TIME:0:4}/${FCST_RST_TIME}.ice.nc 0 3
$ROOT_GODAS_DIR/build/mppnccombine -m -64 $work_dir/pentad.ocean.nc \
    $JOB_WORK_DIR/fcst.run/mem_0000/*.ocean_pentad_*
procGrid $work_dir/pentad.ocean.nc $ROOT_EXP_DIR/output/bkg_diag/${FCST_RST_TIME:0:4}/${FCST_RST_TIME}.ocean.nc 0 3



#--------------------------------------------------------------------------------
# save per-ensemble state
#  note that for 3dvar, there is one "ensemble member"
#--------------------------------------------------------------------------------
echo "Saving ensemble data..."
for ens in ${ENS_LIST}; do
    echo ""
    echo " ensemble member: $ens"

    # ensemble member background at analysis time
    # TODO: use the restart file instead?
    # TODO: DA_WNDW_CNTR_TIME is no longer defined
#    if [[ "$SAVE_BKG_ENS" -gt 0 ]]; then
#	ifile=$JOB_WORK_DIR/da.prep/bkg/mem_$ens/${DA_WNDW_CNTR_TIME:0:8}.nc
#	ofile=$ROOT_EXP_DIR/output/bkg/ens/$ens/${DA_WNDW_CNTR_TIME}.nc 
#	procGrid $ifile $ofile 0 3 rhopot0,SST_min
#    fi

    #
    if [[ "$SAVE_BKG_SLOT" -gt 0 ]]; then
	out_dir=$ROOT_EXP_DIR/output/bkg/ens_slots/$ens
	echo "ERROR: background ensemble slots not being saved"
	exit 1
    fi

    #
    if [[ "$SAVE_ANA_ENS" -gt 0 ]]; then
	out_dir=$ROOT_EXP_DIR/output/ana/ens/$ens
	echo "ERROR: analysis ensemble slots not being saved."
	exit 1
    fi
done



#--------------------------------------------------------------------------------
# Save observation minus background stats
#--------------------------------------------------------------------------------
if [[ "$SAVE_OMF" -gt 0 ]]; then
    echo "Saving OmF..."
    ofile=$ROOT_EXP_DIR/output/omf/${CYCLE:0:4}/${CYCLE}.nc 
    mkdir -p $(dirname $ofile)

    e=( $ENS_LIST )
    e=${e[0]}
    ifiles=$JOB_WORK_DIR/da.prep/omf/mem_*/obs.nc
    ifile0=$JOB_WORK_DIR/da.prep/omf/mem_$e/obs.nc

    # calculate the ensemble members / mean /spread
    # for the increments   
    ncecat -h -O $ifiles -v inc $ofile.ens.tmp
    if [[ ${#e[@]} -gt 1 ]]; then
	ncwa -h -O -a record $ofile.ens.tmp $ofile.mean.tmp
	ncbo -h -O $ofile.ens.tmp $ofile.mean.tmp $ofile.sprd.tmp
	ncra -h -O -y rmssdn $ofile.sprd.tmp $ofile.sprd.tmp
	ncwa -h -O -a record $ofile.sprd.tmp $ofile.sprd.tmp
    fi

    # combine things
    #... i have no idea why its being weird
    ncrename -h -O -d record,mem $ofile.ens.tmp $ofile.ens.tmp
    ncks -h -O --fix_rec_dmn all $ofile.ens.tmp $ofile.ens.tmp
    mv $ofile.ens.tmp $ofile.tmp
    if [[ ${#e[@]} -gt 1 ]]; then
	ncrename -h -O -v inc,inc_mean $ofile.mean.tmp $ofile.mean.tmp
	ncrename -h -O -v inc,inc_sprd $ofile.sprd.tmp $ofile.sprd.tmp
	ncks -h -A $ofile.mean.tmp $ofile.tmp
	ncks -h -A $ofile.sprd.tmp $ofile.tmp    
    fi


    vars="time obid plat lat lon depth hr val err qc"
    for v in $vars; do
	ncks -h --fix_rec_dmn all -v $v $ifile0 $ofile.$v.tmp
	ncks -h -A $ofile.$v.tmp $ofile.tmp
    done
    nccopy -d 8 -s $ofile.tmp $ofile

    rm $ofile.tmp
    rm $ofile.*.tmp
fi


#--------------------------------------------------------------------------------
# delete any old working directories that are no longer needed
#--------------------------------------------------------------------------------
# cycle directory
if [[ "$SAVE_RST_CYCLES" -gt "0" ]]; then
    echo "Checking for old cycles to delete..."

    # how far back do we need to go?
    dtz(){ echo ${1:0:8}Z${1:8:10}; }
    keep_hrs=$(($FCST_LEN + $SAVE_RST_CYCLES))
    keep_date=$(date "+%Y%m%d%H" -d "$(dtz $CYCLE) - $keep_hrs hours")
    echo "Deleting cycles before $keep_date"

    for f in $ROOT_EXP_DIR/cycle/*; do
	d=${f##*/}
	keep=0

	# keep if new enough
	if [[ "$d" -ge "$CYCLE" ]]; then keep=1; fi

	#save annual restart file?
	if [[ "$SAVE_RST_ANNUAL" -gt 0 ]]; then
	    y1=${d:0:4}
	    y2=$(date "+%Y" -d "$(dtz $d) - $FCST_LEN hours")
	    if [[ "$y1" -ne "$y2" ]]; then keep=1; fi
	fi

	# delete
	if [[ "$keep" -eq 0 ]]; then
	    echo "* Deleting $d"
	    rm -r $f
	fi
    done
fi


# working directory
echo "* Deleting cycle temporary working directory: "
echo "  $JOB_WORK_DIR"
rm -r $JOB_WORK_DIR
