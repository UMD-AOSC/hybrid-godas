#!/bin/bash
#PBS -E
#PBS -l walltime=5:00:00
#PBS -j oe
#PBS -A cpo_hyb_go
#PBS -q ldtn

# moab job name will be in format YYYYMMDDYYYYMMDD {startdate}{enddate}
module load nco
source activate pygrib

date_start=${MOAB_JOBNAME::8}
date_end=${MOAB_JOBNAME:8:8}

len=2

date_start_next=$(date +%Y%m%d -d "$date_start + $len day")
date_end_job=$(date +%Y%m%d -d "$date_start_next - 1 day")
if [ $date_end_job -gt $date_end ]; then 
    date_end_job=$date_end;
 fi

echo "start_date:$date_start end_date:$date_end"
echo "this job will do: $date_start to $date_end_job"

./get_cfsr_fluxes.py $date_start $date_end_job

if [ $date_end_job -lt $date_end ]; then
    echo "submitting next job $date_start_next"
    msub -N $date_start_next$date_end moab_get_cfsr.sh
fi