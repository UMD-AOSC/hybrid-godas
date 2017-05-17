.PHONY: all fms mom obsop


all: 
	@echo "select individual component to build. (fms, mom, obsop, 3dvar)"

fms:
	rm -rf src/MOM6/build/intel/*
	mkdir -p src/MOM6/build/intel/shared/repro
	(cd src/MOM6/build/intel/shared/repro/; rm -f path_names; ../../../../src/mkmf/bin/list_paths ../../../../src/FMS; ../../../../src/mkmf/bin/mkmf -t ../../../../src/mkmf/templates/ncrc-intel.mk -p libfms.a -c "-Duse_libMPI -Duse_netCDF -DSPMD" path_names)
	(source config/env; cd src/MOM6/build/intel/shared/repro/;  make NETCDF=3 REPRO=1 libfms.a -j)


mom:
	mkdir -p src/MOM6/build/intel/ice_ocean_SIS2/repro/
	(cd src/MOM6/build/intel/ice_ocean_SIS2/repro/; rm -f path_names; ../../../../src/mkmf/bin/list_paths ./ ../../../../src/MOM6/config_src/{dynamic,coupled_driver} ../../../../src/MOM6/src/{*,*/*}/ ../../../../src/{atmos_null,coupler,land_null,ice_ocean_extras,icebergs,SIS2,FMS/coupler,FMS/include}/)
	(cd src/MOM6/build/intel/ice_ocean_SIS2/repro/; ../../../../src/mkmf/bin/mkmf -t ../../../../src/mkmf/templates/ncrc-intel.mk -o '-I../../shared/repro' -p MOM6 -l '-L../../shared/repro -lfms' -c '-Duse_libMPI -Duse_netCDF -DSPMD -DUSE_LOG_DIAG_FIELD_INFO -Duse_AM3_physics' path_names )
	(source config/env; cd src/MOM6/build/intel/ice_ocean_SIS2/repro/; make NETCDF=3 REPRO=1 MOM6 -j)

obsop:
	cd src/obsop; make

3dvar:
	ln -sf ../../../config/env src/3dvar/config/env
	cd src/3dvar; make --no-print-directory
