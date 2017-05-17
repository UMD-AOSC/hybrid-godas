#!/bin/bash
set -e

# check required environment variables
envvars="flux_cfsr_dir "
for v in ${envvars}; do
    if [ -z "${!v}" ]; then echo "ERROR: env var $v not set."; exit 1; fi
done

# command line arguments
date_start=$1  #in YYYYMMDD format
date_end=$2    #in YYYYMMDD format



echo "------------------------------------------------------------"
echo " prep_forcing.sh"
echo "------------------------------------------------------------"
echo "Preparing forcing files from $date_start to $date_end"



file_types="PRATE PRES.sfc TMP.2m SPFH.2m UGRD.10m VGRD.10m DLWRF.sfc DSWRF.sfc"
for arg in $file_types
do
    echo ""
    echo $arg
    files=""
    date_cur=$date_start
    while [ $(date -d "$date_cur" +%s) -le $(date -d "$date_end" +%s) ]
    do
	date_next=$(date "+%Y%m%d" -d "$date_cur + 1 day")
	file=$flux_cfsr_dir/$(date "+%Y/%Y%m%d/cfsr.%Y%m%d" -d "$date_cur").${arg}.nc
	files="$files $file"
	date_cur=$date_next
    done

    rm -f $arg.nc
    cdo cat $files $arg.nc.tmp
    cdo settaxis,$date_start,00:00:00,1day $arg.nc.tmp $arg.nc
    rm -f $arg.nc.tmp
    ncatted -O -a axis,time,c,c,T $arg.nc
    ncatted -O -a calendar,,m,c,gregorian $arg.nc    
done


echo "------------------------------------------------------------"