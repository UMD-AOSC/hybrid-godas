.PHONY: all   clean clean-model clean-da   model fms mom   da gsw datetime obsop util 3dvar letkf

all: 
	@echo "select individual component to build. (fms, mom, gsw, datetime, obsop, util, 3dvar, letkf)"
	@echo "or the entire mom or da system (model, da)"

clean: clean-model clean-da


# MOM6 model components
#--------------------------------------------------------------------------------
model: fms mom

fms:
	mkdir -p src/MOM6/build/intel/shared/repro
	(cd src/MOM6/build/intel/shared/repro/; rm -f path_names; ../../../../src/mkmf/bin/list_paths ../../../../src/FMS; ../../../../src/mkmf/bin/mkmf -t ../../../../src/mkmf/templates/ncrc-intel.mk -p libfms.a -c "-Duse_libMPI -Duse_netCDF -DSPMD" path_names)
	(source config/env; cd src/MOM6/build/intel/shared/repro/;  make NETCDF=3 REPRO=1 libfms.a -j)

mom:
	mkdir -p src/MOM6/build/intel/ice_ocean_SIS2/repro/
	(cd src/MOM6/build/intel/ice_ocean_SIS2/repro/; rm -f path_names; ../../../../src/mkmf/bin/list_paths ./ ../../../../../MOM6-changes/{coupler,memory,MOM6,SIS2} ../../../../src/MOM6/config_src/{dynamic,coupled_driver} ../../../../src/MOM6/src/{*,*/*}/ ../../../../src/{atmos_null,coupler,land_null,ice_ocean_extras,icebergs,SIS2,FMS/coupler,FMS/include}/ )
	(cd src/MOM6/build/intel/ice_ocean_SIS2/repro/; ../../../../src/mkmf/bin/mkmf -t ../../../../src/mkmf/templates/ncrc-intel.mk -o '-I../../shared/repro' -p MOM6 -l '-L../../shared/repro -lfms' -c '-Duse_libMPI -Duse_netCDF -DSPMD -Duse_AM3_physics -D_USE_LEGACY_LAND_ ' path_names )
	(source config/env; cd src/MOM6/build/intel/ice_ocean_SIS2/repro/; make NETCDF=3 REPRO=1 MOM6 -j)	

clean-model:
	rm -rf src/MOM6/build/intel/*


# DA system components
#--------------------------------------------------------------------------------
da: gsw datetime obsop util 3dvar letkf

gsw:
	cd src/gsw; make

datetime:
	cd src/datetime; make

obsop: gsw datetime
	cd src/obsop; make

util: 
	cd src/util; make

3dvar:
	ln -sf ../../../config/env src/3dvar/config/env
	cd src/3dvar; make --no-print-directory

letkf:
	ln -sf ../../../config/env src/letkf/config/env
	cd src/letkf; make --no-print-directory

clean-da:
	cd src/3dvar; make clean
	cd src/util; make clean
	cd src/obsop; make clean
	cd src/datetime; make clean
	cd src/gsw; make clean
	cd src/letkf; make clean
