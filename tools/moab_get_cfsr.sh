#!/bin/bash
#PBS -E
#PBS -l walltime=16:00:00
#PBS -j oe
#PBS -A cpo_hyb_go
#PBS -q rdtn

yr=${MOAB_JOBNAME::4}
dt=${MOAB_JOBNAME::8}

echo "activating pygrib"
source activate pygrib

echo "downloading fluxes"
./get_cfsr_fluxes.py $dt ${yr}1231
