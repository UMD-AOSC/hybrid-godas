# hybrid-godas
This repository provides the code and configuration for the hybrid global ocean data assimilation system (hybrid-GODAS) being developed by NCEP/UMD.

For more information, use the [hybrid-godas wiki](https://github.com/UMD-AOSC/hybrid-godas/wiki)

## Current Status / Limitations
* Code is currently able to run a free running MOM6 model run, issues with CFSR fluxes are still in progress
* 3DVar data assimilation cycles works, currently being tested. Uses insitu T/S and AVHRR SST, but with univariate covariance
* **No LETKF has been integrated yet**


## Directory Structure
| File/directory   | description |
| ---------------- | --------|
| ```config/```    |  system dependant configuration files for running/compiling |
| ```run/```       |  scripts to initialize/run experiments|
| ```src/```       |  all source code|
| ```tools/```     | scripts to perform secondary tasks (download observations, prepare forcing, etc.)|


Several parts of the code are maintained as separate git repositories. These are downloaded by git when checking out this main hybrid-godas repository. 

| submodule | description |
|---|---|
| ```src/3dvar/``` | 3DVar source code ([github link](https://github.com/UMD-AOSC/godas-3dvar)) |
| ```src/MOM6/```  | MOM6 ocean model ([github link](https://github.com/NOAA-GFDL/MOM6-examples)) |
| ```src/obsop/gsw/GSW-Fortran/``` | GSW Oceanographic toobox, for converting between Temp and Potential Temp, etc. ([github link](https://github.com/TEOS-10/GSW-Fortran))|

To ensure that the submodules are up to date, run:
```git submodule update --init --recursive```