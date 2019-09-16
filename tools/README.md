This directory contains scripts to perform secondary tasks such as downloading observations and preparing surface forcing files. Most of these scripts are python, and a detailed description of each tool can be viewed by running the tool with the `-h` option.

### plots
`./plots/` contains basic python scripts for generating plots for evaluating O-F (observation minud forecast) statistics.

### postproc
`./postproc` scripts that are called by the `/run/subscript/cycle.post.sh` subscript to perform masking, remapping, and compressing of output files. (Note: this is being phased out since the python code can be slow)

### Observation preparation

| file | description |
| --------------------   | ----------------- |
| `get_sst_acspo.py` | Downloads NOAA ACSPO AVHRR SST data |
| `get_sst_pathfinder.py` | Downloads Pathfinder AVHRR SST data (use ACSPO, it's better!) |

### Surface forcing preparation

| file | description |
| --------------------   | ----------------- |
| `get_fluxes_cfsr.py` | Downloads and converts CFSR 6 hourly fluxes |
| `get_fluxes_20CRv2.py` | Download the 20crv2 ensemble members used for flux purturbations. |

### Misc 

| file | description |
| --------------------   | ----------------- |
| `calc_mask_table.py` | calculates new processor masks for MOM6 for any given number of processors / nodes |