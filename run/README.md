All the scripts to needed to initialize and run experiments.

| directory | description |
| ------------ | ---------------------|
| `config.exp_default` | the default MOM/DA configuration files that are copied to experiment directories when those directories are initialized |
| `rocoto` | the configuration files used by the [rocoto](https://github.com/christopherwharrop/rocoto) workflow|
| `subscripts` | the scripts that do the actual work (forecast, data assimilation). The aim is to have them be as system independent as possible. They are called by the appropriate steps in the `rocoto` workflow |

