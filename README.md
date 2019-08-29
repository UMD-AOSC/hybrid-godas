[![docs](https://readthedocs.org/projects/hybrid-godas/badge/?version=latest)](http://hybrid-godas.readthedocs.io)

For complete set of documentation, visit [hybrid-godas.readthedocs.io](http://hybrid-godas.readthedocs.io/).

# hybrid-godas
This repository provides the code and configuration for the hybrid global ocean data assimilation system (hybrid-GODAS) being developed by NCEP/UMD.

For more information, use the [hybrid-godas wiki](https://github.com/UMD-AOSC/hybrid-godas/wiki)

## Current Status
* 3DVar / Hybrid data assimilation cycles works, performance is currently being tested.
* observations consist of insitu T/S from World Ocean Database, and NOAA ACSPO AVHRR nighttime SST

## Directory Structure
| File/directory   | description |
| ---------------- | ----------- |
| `build/`     | links to all the executables (built from `src/` directory) needed by the model and data assimilation |
| `config/`    | system dependent configuration files for running/compiling |
| `run/`       | scripts to initialize/run experiments|
| `src/`       | all source code|
| `tools/`     | scripts to perform secondary tasks (download observations, prepare forcing, etc.)|


Several parts of the code are maintained as separate git repositories. These are downloaded by git when checking out this main hybrid-godas repository. 

| submodule | description |
| --------- | ----------- |
| `src/3dvar/`     | 3DVar source code ([github link](https://github.com/UMD-AOSC/godas-3dvar)) |
| `src/datetime/`  | Date and time manipulation classes for fortran ([github link](https://github.com/wavebitscientific/datetime-fortran)) |
| `src/geoKdTree/` | geospatial k-d tree for fast lat/lon point lookups ([github link](https://github.com/travissluka/geoKdTree)) |
| `src/gsw/`       | GSW Oceanographic toobox, for converting between Temp and Potential Temp, etc. ([github link](https://github.com/TEOS-10/GSW-Fortran))|
| `src/MOM6/`      | MOM6 ocean model ([github link](https://github.com/NOAA-GFDL/MOM6-examples)) |
| `src/letkf/`     | model agnostic LETKF ([github link](https://github.com/travissluka/UMD-LETKF)) |

To ensure that the submodules are up to date, run:
`git submodule update --init --recursive`