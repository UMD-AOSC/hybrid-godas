#!/bin/bash
#PBS -A cpo_hyb_go
#PBS -N MOM6_fcst
#PBS -l nodes=15
#PBS -l partition=c3
#PBS -q batch
#PBS -j oe
#PBS -q debug
#PBS -o MOM6_fcst.log

#PBS -l walltime=00:30:00
##  ~5 min for 1 day
## ~16 min for 5 day


interval=5 # interval in days
date_start="1979-01-01"
date_end="1979-01-10"

root_dir="/lustre/f1/unswept/ncep/Travis.Sluka/godas-3dvar-mom6"
exp_dir="$root_dir/exp/exp1"
work_dir="/lustre/f1/ncep/Travis.Sluka/g3dv_$(echo $exp_dir | md5sum | cut -c 1-10)"


#------------------------------------------------------------



# import linux environment
. $root_dir/MOM6/build/intel/env


# check to see if the output directory has been created
restart="r"
if [ ! -d "$exp_dir" ]; then
    echo "Initializing new experiment starting on $date_start"
    restart="n"
    mkdir -p $exp_dir
    mkdir -p $exp_dir/RESTART
    echo "$date_start" > $exp_dir/last_date    
fi


# figure out where we've left off with previous runs
date_cur=$(cat $exp_dir/last_date)
date_next=$(date "+%Y-%m-%d" -d "$date_cur + $interval day")
echo "Running forecast from $date_cur to $date_next   (stopping: $date_end)"


# setup working directory
#------------------------------------------------------------
rm -rf $work_dir
mkdir -p $work_dir
mkdir -p $work_dir/RESTART
cd $work_dir
cp $root_dir/run/config_static/* . 
. $root_dir/run/config_script/input.nml > input.nml
mkdir INPUT
ln -s $root_dir/run/INPUT/* INPUT/
cp $root_dir/MOM6/build/intel/ice_ocean_SIS2/repro/MOM6 .

# copy the restart files if needed
if [ "$restart" = 'r' ]; then
    ln -s $exp_dir/RESTART/* INPUT/
fi

# prepare the forcing
mkdir -p $work_dir/FORC
cd $work_dir/FORC
$root_dir/run/prep_forcing.sh $date_cur $date_next


# run the forecast
#------------------------------------------------------------
cd $work_dir
if aprun -n $PBS_NP MOM6
then
    echo "MOM6 run complete"
else
    echo "MOM6 run failure"
    exit 1
fi


# move the resart files, update the "last_date"
mv $exp_dir/RESTART $exp_dir/RESTART_old
mv $work_dir/RESTART $exp_dir/RESTART
echo "$date_next" > $exp_dir/last_date
rm $exp_dir/RESTART_old -rf


# submit next cycle 
if [ $(date -d "$date_next" +%s) -lt $(date -d "$date_end" +%s) ]; 
then
     cd $root_dir/run
     msub ./run_fcst.sh
fi
