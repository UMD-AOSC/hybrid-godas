#!/bin/bash
set -e

export HGODAS_ROOT_DIR=$(readlink -f $(dirname ${BASH_SOURCE[0]}))

# get the machine config file and source it
if [[ $# != 1 ]]; then
    echo "USAGE: compile.sh <path_to_machine_config>"
    exit 1
fi
MACH_CFG_FILE=$1
source $MACH_CFG_FILE

# make sure submodules are up to date
echo "Updating git submodules..."
git submodule update --init --recursive

# run cmake
echo "Preparing build in $HGODAS_BUILD_DIR"
mkdir -p $HGODAS_BUILD_DIR
cd $HGODAS_BUILD_DIR
cmake $HGODAS_ROOT_DIR $CMAKE_OPT

cd $HGODAS_BUILD_DIR
make -j 4
