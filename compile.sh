#!/bin/bash
set -e

source config/gaea.env

mkdir -p build.release
cd build.release
cmake ../ -DCMAKE_BUILD_TYPE=Release -DCMAKE_PREFIX_PATH=${CMAKE_PREFIX_PATH} \
  -DMOM6_NIPROC=${HYGODAS_FCST_NIPROC} -DMOM6_NJPROC=${HYGODAS_FCST_NJPROC}

make -j 1
