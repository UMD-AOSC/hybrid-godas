#!/bin/bash
set -e

cat <<EOF


#================================================================================
#================================================================================
# Hybrid-GODAS  -  da.post.sh
#================================================================================

EOF

envar=()
envar+=("WORK_DIR")
envar+=("DA_MODE")
envar+=("BIN_DIR")
envar+=("ENS_LIST")
envar+=("SAVE_OMF")
envar+=("WORK_DIR")
envar+=("OUTPUT_DIR")


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


comp2=Temp/3,Salt/3,SSH/4,u/3,v/3
comp3=Temp/2,Salt/2,SSH/3,u/2,v/2,\
temp/2,salt/2,ssh/3,\
mld_003/1,mld_0125/1,taux/2,tauy/2,speed/2,ssh/3,lh/1,lw/1,sh/1,net_heat_coupler/1,\
cn/2,siu/3,siv/3,sisnconc/2,siconc/2,hs/2,hi/2

#------------------------------------------------------------
# helper function to combine tiled files
#------------------------------------------------------------
function combtiles(){
   shopt -s nullglob
   found=0
   for f in ${1}*; do found=1; done
   if [[ $found == 0 ]]; then
       echo "WARNING: not combining $2, source files not found"
       return
   fi
   
   # create output directory
   d=$(dirname $2)
   mkdir -p $d

   # remove output file if it already exists
   if [[ -e $2 ]]; then
       rm $2
   fi

   # combine tiles
   $BIN_DIR/mppnccombine -64 $2 ${1}*
}



#------------------------------------------------------------
# Save O-F stats
#------------------------------------------------------------
if [[ "$SAVE_OMF" -gt 0 ]]; then
    echo "Saving O-F..."

    e=( $ENS_LIST )
    e=${e[0]}

    # make sure all the obs.nc files are present
    missing=0
    for e in $ENS_LIST; do
	if [[ ! -f $WORK_DIR/../da.run/mem_$e/obsop/obs.nc ]]; then
	    missing=1
	fi
    done
    if [[ $missing == 1 ]]; then
	echo "WARNING: O-F stats will not be calculated."
	echo " Per-ensemble observation files not found."
	#TODO, make an if statement instead of exiting here
	exit 1
    fi


    ifiles=$WORK_DIR/../da.run/mem_????/obsop/obs.nc
    ifile0=$WORK_DIR/../da.run/mem_$e/obsop/obs.nc
    ofile=$OUTPUT_DIR/omf/${CYCLE:0:4}/${CYCLE}.nc

    # calculate the ensemble members / mean / spread
    ncecat -h -O $ifiles -v inc omf.ens.tmp
    if [[ ${#e[@]} -gt 1 ]]; then
	# if there were ensembles, process those
	ncwa -h -O -a record omf.ens.tmp omf.mean.tmp
	ncbo -h -O omf.ens.tmp omf.mean.tmp omf.sprd.tmp
	ncra -h -O -y rmssdn omf.sprd.tmp omf.sprd.tmp
	ncwa -h -O -a record omf.sprd.tmp omf.sprd.tmp
    fi

    # combine things
    ncrename -h -O -d record,mem omf.ens.tmp omf.ens.tmp
    ncks -h -O --fix_rec_dmn all omf.ens.tmp omf.ens.tmp
    mv omf.ens.tmp omf.tmp
    if [[ ${#e[@]} -gt 1 ]]; then
	# if there were ensembles, process those
	ncrename -h -O -v inc,inc_mean omf.mean.tmp omf.mean.tmp
	ncrename -h -O -v inc,inc_sprd omf.sprd.tmp omf.sprd.tmp
	ncks -h -A omf.mean.tmp omf.tmp
	ncks -h -A omf.sprd.tmp omf.tmp
    fi

    # fix up and compress the resulting file
    vars="time obid plat lat lon depth hr val err qc"
    for v in $vars; do
	ncks -h --fix_rec_dmn all -v $v $ifile0 omf.$v.tmp
	ncks -h -A omf.$v.tmp omf.tmp
    done
    nccopy -d 8 -s omf.tmp omf.nc

    # move to final location
    d=$(dirname $ofile)
    mkdir -p $d
    mv omf.nc $ofile
fi



#------------------------------------------------------------
# Save 3dvar background/analysis
#------------------------------------------------------------
if [[ "$DA_MODE" == "var" ]]; then
    ofile=$OUTPUT_DIR/ana/mean/${CYCLE:0:4}/${CYCLE}.nc
    echo "TODO var output"
    exit 1
    # bkg.nc
    # ana
fi


#------------------------------------------------------------
# Save ensemble background/analysis mean/spread
#------------------------------------------------------------
# TODO: renable masking and tile combination
if [[ ( "$DA_MODE" == "hyb" ) || ( "$DA_MODE" == "ekf" ) ]]; then
    echo -e "\nSaving ensemble mean/spread..."
    maskopt="-geomFile $GRID_DIR/hgrid.nc -vertFile $GRID_DIR/vgrid.nc -d2 SSH -d3 Temp,Salt,u,v"
    compopt="-compression 5 -lsd"

    # # background mean
    # combtiles $WORK_DIR/../da.run/mem_mean/letkf/bkg. tmp.nc
    # $POSTPROC_SCRIPTS/mask.py tmp.nc tmp2.nc $maskopt
    #$POSTPROC_SCRIPTS/compress.py  $compopt $comp2 tmp2.nc $OUTPUT_DIR/bkg/mean/${CYCLE:0:4}/${CYCLE}.nc
    f=$OUTPUT_DIR/bkg/mean/${CYCLE:0:4}/${CYCLE}.nc
    mkdir -p $(dirname $f)
    ncks -7 -L 4 -O --ppc default=4  $WORK_DIR/../da.run/mem_mean/letkf/bkg.nc $f

    # # background spread
    # combtiles $WORK_DIR/../da.run/mem_sprd/letkf/bkg. tmp.nc
    # $POSTPROC_SCRIPTS/mask.py tmp.nc tmp2.nc $maskopt
    # $POSTPROC_SCRIPTS/compress.py  $compopt $comp2 tmp2.nc $OUTPUT_DIR/bkg/sprd/${CYCLE:0:4}/${CYCLE}.nc
    # rm tmp*.nc
    f=$OUTPUT_DIR/bkg/sprd/${CYCLE:0:4}/${CYCLE}.nc
    mkdir -p $(dirname $f)
    ncks -7 -L 4 -O --ppc default=3  $WORK_DIR/../da.run/mem_sprd/letkf/bkg.nc $f

    
    # # analysis spread
    # combtiles $WORK_DIR/../da.run/mem_sprd/letkf/ana. tmp.nc
    # $POSTPROC_SCRIPTS/mask.py tmp.nc tmp2.nc $maskopt
    # $POSTPROC_SCRIPTS/compress.py  $compopt $comp2 tmp2.nc $OUTPUT_DIR/ana/sprd/${CYCLE:0:4}/${CYCLE}.nc
    # rm tmp*.nc
    f=$OUTPUT_DIR/ana/sprd/${CYCLE:0:4}/${CYCLE}.nc
    mkdir -p $(dirname $f)
    ncks -7 -L 4 -O --ppc default=3  $WORK_DIR/../da.run/mem_sprd/letkf/ana.nc $f

    # analysis mean (letkf)
    ofile=$OUTPUT_DIR/ana/mean/${CYCLE:0:4}/${CYCLE}.nc
    if [[ "$DA_MODE" == "hyb" ]]; then
	ofile=$OUTPUT_DIR/ana/mean_letkf/${CYCLE:0:4}/${CYCLE}.nc
    fi
    # combtiles $WORK_DIR/../da.run/mem_mean/letkf/ana. tmp.nc
    # $POSTPROC_SCRIPTS/mask.py tmp.nc tmp2.nc $maskopt
    # $POSTPROC_SCRIPTS/compress.py  $compopt $comp2 tmp2.nc $ofile
    # rm tmp*.nc
    mkdir -p $(dirname $ofile)
    ncks -7 -L 4 -O --ppc default=4  $WORK_DIR/../da.run/mem_mean/letkf/ana.nc $ofile

    # analysis mean (hybrid)
    if [[ "$DA_MODE" == "hyb" ]]; then
	pwd 
	ekf_file=$OUTPUT_DIR/ana/mean_letkf/${CYCLE:0:4}/${CYCLE}.nc
	var_file=$WORK_DIR/../da.run/var/ana_inc.nc
	ofile=$OUTPUT_DIR/ana/mean/${CYCLE:0:4}/${CYCLE}.nc
	d=$(dirname $ofile)
	mkdir -p  $d
	cdo select,name=Temp,Salt $ekf_file  tmp.nc
	cdo --sort add tmp.nc $var_file tmp2.nc
	cdo replace $ekf_file tmp2.nc tmp.nc

	mkdir -p $(dirname $ofile)
	ncks -7 -L 4 -O --ppc default=4 tmp.nc $ofile
#	$POSTPROC_SCRIPTS/compress.py  $compopt $comp2 tmp2.nc $ofile
	rm tmp*.nc
    fi
fi


#------------------------------------------------------------
# Save per-ensemble background/analysis
#------------------------------------------------------------
# TODO
