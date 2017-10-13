scripts to initialize and run experiments

| directory | description |
| ------------ | ---------------------|
| ```config``` | the default MOM/DA configuration files that are copied to experiment directories when those directories are initialized |
| ```moab``` | these are the system dependant run scripts for running the model and DA cycle |
| ```subscripts``` | the scripts that do the actual work (forecast, data assimilation). The aim is to have them be as system independant as possible. They are called by the appropriate ```moab/``` files |

