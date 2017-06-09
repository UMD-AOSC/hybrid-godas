#!/bin/bash
set -e

# get command line arguments
if [ "$#" -ne 1 ]; then
    echo "usage: init_DAcycle.py <exp_dir>"
    exit 1
fi
exp_dir=$1


# determin the root directory 
root_dir="$( cd "$( dirname "${BASH_SOURCE[0]}" )"/../ && pwd )"

# create experiment directory
if [ -d "$exp_dir" ]; then
    echo "ERROR: Directory already exists"
fi
mkdir -p $exp_dir
cd $exp_dir

mkdir -p logs
mkdir -p ana
mkdir -p bkg

mkdir -p config
cp -r $root_dir/run/config/3dvar config/  
cp -r $root_dir/run/config/mom config/
(. $root_dir/run/config/config.DAcycle config/config.DAcycle)

ln -s $root_dir/run/moab/run_DAcycle.sh .


echo "Experiment directory has been setup."
echo "Ensure configuration in $exp_dir/config is correct before running"