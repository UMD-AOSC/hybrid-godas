#!/bin/bash
set -e
cat <<EOF


#================================================================================
#================================================================================
# NCEP Hybrid-GODAS  -  fcst.post.sh
#   post processing of MOM6 background forecast output
#================================================================================
EOF

envar=() 
envar+=("WORK_DIR")
envar+=("COMBINE_EXE")
envar+=("FCST_DIR")
envar+=("DIAG_FILES")
envar+=("OUTPUT_DIR")
envar+=("CYCLE")

# make sure the required env vars exist
for v in ${envar[@]}; do
    if [[ -z "${!v}" ]]; then
	echo "ERROR: env var $v is not set."; exit 1
    fi
    echo " $v = ${!v}"
done
echo ""
set -u


# setup temporary working directory
echo "Setting up working directory in:"
echo "  $WORK_DIR"
if [[ -e "$WORK_DIR" ]]; then
    echo "WARNING: WORK_DIR already exists, removing."
    rm -rf $WORK_DIR
fi
mkdir -p $WORK_DIR
cd $WORK_DIR



# get a list of the diagnostic files that need to be combined (by removing the .xxxx suffixes)
shopt -s nullglob
cd $FCST_DIR
files=($( for f in $DIAG_FILES; do echo ${f%.*}; done | sort | uniq | xargs))
cd $WORK_DIR

# combine the files
for f in ${files[@]}; do 
    f2=${f::-14}; f2=${f2#*.}
    echo "combining $f2"
    echo "    $COMBINE_EXE -m -64 $f2 $FCST_DIR/*$f2.*"
    $COMBINE_EXE -m -64 $f2.nc.2 $FCST_DIR/*$f2*
done


# compress the files
comp2=Temp/3,Salt/3,SSH/4,u/3,v/3
comp3=Temp/2,Salt/2,SSH/3,u/2,v/2,\
temp/2,salt/2,ssh/3,\
mld_003/1,mld_0125/1,taux/2,tauy/2,speed/2,ssh/3,lh/1,lw/1,sh/1,net_heat_coupler/1,\
cn/2,siu/3,siv/3,sisnconc/2,siconc/2,hs/2,hi/2
for f in *.nc.2; do
    f2=${f%.*}
    $POSTPROC_SCRIPTS/compress.py -compression 5 -lsd $comp3 $f $f2
done 


# TODO processing of derrived parameters


# move the files to their correct locations
# TODO change CYCLE to the day at end of pentad?
outdir=$OUTPUT_DIR/bkg_diag/${CYCLE:0:4}/
mkdir -p $outdir
for f in *.nc; do
    mv $f $outdir/${CYCLE:0:8}.$f
done
