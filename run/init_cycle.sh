#!/bin/bash
set -e

# get command line arguments
if [[ "$#" -ne 1 ]]; then
    echo "usage: init_cycle.sh <exp_dir>"
    exit 1
fi
exp_dir=$1


#determine the root directory
# (should be one directory above the location of this file)
root_dir="$( cd "$( dirname "${BASH_SOURCE[0]}" )"/../ && pwd )"
root_dir="$(readlink -f $root_dir)"


# create experiment directory
if [[ -d "$exp_dir" ]]; then
    echo "ERROR: Directory already exists"
fi
mkdir -p $exp_dir
exp_dir="$(readlink -f $exp_dir)"
cd $exp_dir


# setup default configurations
mkdir -p config
mkdir -p config/da
mkdir -p config/mom
cp $root_dir/run/config/da/* config/da
cp $root_dir/run/config/mom/* config/mom
ln -s $root_dir/run/rocoto/hybridgodas.runstep .
ln -s $root_dir/run/rocoto/hybridgodas.status .

# setup default values to fill the default configuration script with
export EXP_NAME="${exp_dir##*/}"
export ROOT_DIR=$root_dir
export EXP_DIR=$exp_dir
export D='$'
cat $root_dir/run/config/hybridgodas.template.config | envsubst '$EXP_NAME $ROOT_DIR $EXP_DIR $D' > config/hybridgodas.config


# Save code version information
touch version
echo "Source code versions used at time project was initialized: " >> version
echo "Main repository:" >> version
echo " $(git rev-parse HEAD)" >> version
echo "Submodules:" >> version
echo "$(git submodule)" >> version

# All done
echo "Experiment directory has been setup."
echo "Ensure configuration in $exp_dir/config is correct before running"
