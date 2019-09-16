#!/bin/bash
set -e
cat <<EOF


#================================================================================
#================================================================================
# Hybrid-GODAS  -  fcst.post.sh
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
set +u
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
  flen=${#f}
  if [[ ${f##*.} != 'nc' ]]; then
    echo "No need to combine diag file for $f.nc"
    ((i0=flen-11))
    f2=${f:0:$i0}; f2=${f2#*.}
    ln -s $FCST_DIR/$f.nc $f2.nc.2
  else
    ((i0=flen-14))
    f2=${f:0:$i0}; f2=${f2#*.}
    echo "combining $f2"
    echo "    $COMBINE_EXE -m -64 $f2 $FCST_DIR/*$f2.*"
    $COMBINE_EXE -m -64 $f2.nc.2 $FCST_DIR/*$f2*
  fi
done


# compress the files
ncks -4 -L 4 -O --ppc default=3 \
  ocean_diag.nc.2 ocean_diag.nc
ncks -4 -L 4 -O --ppc default=3 \
  ice_diag.nc.2 ice_diag.nc


# TODO processing of derrived parameters


# move the files to their correct locations
# TODO change CYCLE to the day at end of pentad?
outdir=$OUTPUT_DIR/bkg_diag/${CYCLE:0:4}/
mkdir -p $outdir
for f in *.nc; do
    mv $f $outdir/${CYCLE:0:8}.$f
done
