#!/bin/bash

cd MOM6

git submodule update --recursive --init

rm -rf build/intel/*

mkdir -p build/intel
cat << EOFA > build/intel/env
module unload PrgEnv-pgi
module unload PrgEnv-pathscale
module unload PrgEnv-intel
module unload PrgEnv-gnu
module unload PrgEnv-cray
module load PrgEnv-intel
module unload netcdf
module load cray-netcdf
EOFA

mkdir -p build/intel/shared/repro/
(cd build/intel/shared/repro/; rm -f path_names; \
../../../../src/mkmf/bin/list_paths ../../../../src/FMS; \
../../../../src/mkmf/bin/mkmf -t ../../../../src/mkmf/templates/ncrc-intel.mk -p libfms.a -c "-Duse_libMPI -Duse_netCDF -DSPMD" path_names)

(cd build/intel/shared/repro/; source ../../env; make NETCDF=3 REPRO=1 libfms.a -j)

mkdir -p build/intel/ice_ocean_SIS2/repro/
(cd build/intel/ice_ocean_SIS2/repro/; rm -f path_names; \
../../../../src/mkmf/bin/list_paths ./ ../../../../src/MOM6/config_src/{dynamic,coupled_driver} ../../../../src/MOM6/src/{*,*/*}/ ../../../../src/{atmos_null,coupler,land_null,ice_ocean_extras,icebergs,SIS2,FMS/coupler,FMS/include}/)
(cd build/intel/ice_ocean_SIS2/repro/; \
    ../../../../src/mkmf/bin/mkmf -t ../../../../src/mkmf/templates/ncrc-intel.mk -o '-I../../shared/repro' -p MOM6 -l '-L../../shared/repro -lfms' -c '-Duse_libMPI -Duse_netCDF -DSPMD -DUSE_LOG_DIAG_FIELD_INFO -Duse_AM3_physics' path_names )

(cd build/intel/ice_ocean_SIS2/repro/; source ../../env; make NETCDF=3 REPRO=1 MOM6 -j)
    
