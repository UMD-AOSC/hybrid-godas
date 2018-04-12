this directory contains scripts to perform secondary tasks such as downloading observations and preparing surface forcing files.

### plots
`./plots/` contains basic python scripts for generating plots for evaluating O-F (observation minud forecast) statistics.

### postproc
`./postproc` scripts that are called by the `/run/subscript/cycle.post.sh` subscript to perform masking, remapping, and compressing of output files.

### Observation preparation

| file | description |
| --------------------   | ----------------- |
| `get_sst_acspo.py` | Downloads NOAA ACSPO AVHRR SST data |
| `get_sst_pathfinder.py` | Downloads Pathfinder AVHRR SST data (use ACSPO, it's better!) |

### Surface forcing preparation

| file | description |
| --------------------   | ----------------- |
| `get_cfsr_fluxes.py` | Downloads and converts CFSR daily fluxes |

### Misc 

| file | description |
| --------------------   | ----------------- |
| `calc_mask_table.py` | calculates new processor masks for MOM6 for any given number of processors / nodes |
| `rst_update.py` | applies the update from the LETKF/3Dvar to a single ensemble member's restart file. Obviously python is not the prefered way of doing this (too slow). This will be replaced in the future. |