#!/bin/bash
set -e

# TODO move this logic into the cmake files
if [[ "$MOM_RES" == "05" ]]; then
    MOM6_NIGLOBAL=720
    MOM6_NJGLOBAL=576
elif [[ "$MOM_RES" == "025" ]]; then
    MOM6_NIGLOBAL=1440
    MOM6_NJGLOBAL=1080
else
    echo "ERROR: MOM_RES must be set to one of 025 or 05"
    exit 1
fi


#TODO, set the correct environment
source config/gaea.env

# TODO, pull out the fortran/c compiler flags
mkdir -p build.release
cd build.release
cmake ../ \
  -DCMAKE_BUILD_TYPE=Release \
  -DCMAKE_PREFIX_PATH=${CMAKE_PREFIX_PATH} \
  -DMOM6_NIGLOBAL=${MOM6_NIGLOBAL} \
  -DMOM6_NJGLOBAL=${MOM6_NJGLOBAL} \
  -DMOM6_NIPROC=${HYGODAS_FCST_NIPROC} \
  -DMOM6_NJPROC=${HYGODAS_FCST_NJPROC} \
  -DCMAKE_Fortran_FLAGS="-target-cpu=sandybridge" \
  -DCMAKE_C_FLAGS="-target-cpu=sandybridge" 

#make 
