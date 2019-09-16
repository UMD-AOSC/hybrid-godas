[![docs](https://readthedocs.org/projects/hybrid-godas/badge/?version=latest)](http://hybrid-godas.readthedocs.io)

For complete set of documentation, visit [hybrid-godas.readthedocs.io](http://hybrid-godas.readthedocs.io/).

# hybrid-godas
This repository provides the code and configuration for the hybrid global ocean data assimilation system (Hybrid-GODAS) being developed by NCEP/UMD.

## Directory Structure
| File/directory   | description |
| ---------------- | ----------- |
| `config/`    | all configuration files (machine specific, MOM grid specific, and da config files) |
| `docs/`   | documentation compiled and available on readthedocs website |
| `pkg/`   | The dependent external git repositories |
| `run/`       | scripts to initialize/run experiments|
| `src/`       | hybrid-godas specific source code |
| `tools/`     | scripts to perform secondary tasks (download observations, prepare forcing, etc.)|


Several parts of the code are maintained as separate git repositories. These are downloaded by git when checking out this main hybrid-godas repository. 

| submodule | description |
| --------- | ----------- |
| `pkg/godas-3dvar/`     | 3DVar source code ([github link](https://github.com/UMD-AOSC/godas-3dvar)) |
| `pkg/datetime-fortran/`  | Date and time manipulation classes for fortran ([github link](https://github.com/wavebitscientific/datetime-fortran)) |
| `pkg/geoKdTree/` | geospatial k-d tree for fast lat/lon point lookups ([github link](https://github.com/travissluka/geoKdTree)) |
| `pkg/GSW-Fortran/`       | GSW Oceanographic toobox, for converting between Temp and Potential Temp, etc. ([github link](https://github.com/TEOS-10/GSW-Fortran))|
| `pkg/model/MOM6/`      | MOM6 ocean model ([github link](https://github.com/NOAA-GFDL/MOM6-examples)) |
| `pkg/UMD-LETKF/`     | model agnostic LETKF ([github link](https://github.com/travissluka/UMD-LETKF)) |

To ensure that the submodules are up to date before building the source code, run:
`git submodule update --init --recursive`