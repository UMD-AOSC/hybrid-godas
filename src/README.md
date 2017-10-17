all source code required by hybrid-GODAS. This includes git submodules that pulls code from other git repositories.

| directory   | description |
| ----------- | -------- |
| ```3dvar``` | The 3dvar standalone executable and modules |
| ```datetime``` | A datetime library used by the executables in the ```obsop``` directory |
| ```gsw```   | Gibbs SeaWater Oceanographic Toolbox for TEOS-10, used by ```obsop``` executables |
| ```MOM6```  | MOM6 ocean model from GFDL |
| ```MOM6-changes``` | Overrides to the default MOM6 code from GFDL, currently just a change to the bulk formula code |
| ```obsop``` | Observation operator and observation preparation executables |
| ```util```  | other utilites needed for DA cycles (background error variance, restart update, and vertical localization executables) |

all code can be compiled using the makefile in the parent directory