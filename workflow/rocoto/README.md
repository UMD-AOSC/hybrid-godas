Jian Kuang 20190627
These scripts will be decommissioned after CROW

These are the files that generate the XML control file used by `rocoto` to manage the hybrid-godas job steps.

`hybridgodas.run` and `hybridgodas.status` should only be called by the user via the links that are created within an individual experiment directory.


| file | description |
| ---  | ----------- |
| `hybridgodas.run` | dynamically generates the xml file for rocoto (if it needs to be updated), and launches the rocoto steps (either a single step, or in a cycle if `--cycle` flag is given |
| `hybridgodas.status` | shows statistics of the experiment's rocoto workflow |
| `hybridgodas.template.xml` | the template `rocoto` configuration file that is filled in by `hybridgodas.run` to produce a `cycle/rocoto/hybridgodas.rocoto.xml` file in the experiment's directory |
| `jobwrapper.sh` | Called within each HPC job step to provide some commonly used environment variables (loads in the experiment's `config/hybridgodas.config` file, calculates cycle step dependent variables, ...) |

