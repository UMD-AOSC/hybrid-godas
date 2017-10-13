this directory contains scripts to perform secondary tasks such as downloading observations and preparing surface forcing files.

## Observation preparation

| file | description |
| --------------------   | ----------------- |
| ```get_sst_acspo.py``` | Downloads NOAA ACSPO AVHRR SST data |
| ```get_sst_acspo.py``` | Downloads Pathfinder AVHRR SST data |

## Surface forcing preparation

| file | description |
| --------------------   | ----------------- |
| ```get_cfsr_fluxes.py``` | Downloads and converts CFSR daily fluxes |

## Misc 

| file | description |
| --------------------   | ----------------- |
| ```calc_mask_table.py``` | calculates the processor masks for MOM6 for any given number of processors / nodes |
| ```mask_output.py```     | does land masking of MOM6 output (too slow, needs to convert to Fortan) |
| ```omf_stats.py```       | creates plots of O-B stats from DA experiments |
| ```prep_forcing.sh```    | creates the surface forcing files required by a MOM6 run by combining the CFSR daily forcing files |