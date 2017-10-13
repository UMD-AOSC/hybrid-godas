all source code required by hybrid-GODAS. This includes git submodules that pulls code from other git repositories.

| directory   | description |
| ----------- | -------- |
| ```3dvar``` | The 3dvar standalone executable and modules |
| ```datetime``` | A datetime library used by the executables in the ```obsop``` directory |
| ```gsw```   | Gibbs SeaWater Oceanographic Toolbox for TEOS-10, used by ```obsop``` executables |
| ```MOM6```  | MOM6 ocean model from GFDL |
| ```obsop``` | Observation operator and observation preparation executables |
| ```util```  | other utilites needed for DA cycles (background error variance, restart update, and vertical localization executables) |

The ```MOM6.coupler.patch``` file contains changes to the MOM6 bulk flux code and is applied by running ```make patch``` from the root directory. (eventually this needs to be cleaned up into something that is NOT a patch file)