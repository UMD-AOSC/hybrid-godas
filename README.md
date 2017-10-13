# hybrid-godas
This repository provides the code and configuration for the hybrid global ocean data assimilation system (hybrid-GODAS) being developed by NCEP/UMD.

For more information, use the [hybrid-godas wiki](https://github.com/UMD-AOSC/hybrid-godas/wiki)

## Current Status
* 3DVar data assimilation cycle works, performance is currently being tested.
* observations consist of insitu T/S from World Ocean Database and NOAA ACSPO AVHRR nighttime SST
* **No LETKF has been integrated yet**

## Upcoming major upgrades
* Integration with LETKF for hybrid DA (obviously)
* IAU capability for standalone 3dvar
* multivariate bg err covariance for standalone 3dvar

## Directory Structure
| File/directory   | description |
| ---------------- | --------|
| ```build/```     | links to all the executables (built from ```src/``` directory) needed by the model and data assimilation |
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
| ```src/datetime/datetime-fortran``` | Date and time manipulation classes for fortran ([github link](https://github.com/wavebitscientific/datetime-fortran)) |

To ensure that the submodules are up to date, run:
```git submodule update --init --recursive```