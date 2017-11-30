#!/bin/bash

cat << \#\#

#================================================================================
#================================================================================
# NCEP Hybrid-GODAS  -  da.postproc.sh
#   Post processing after everything else in the cycle is complete
#================================================================================
##
# Travis.Sluka@noaa.gov / tsluka@umd.edu
#
# Prerequisites:
#
# Results:
#
# Required MANUALLY defined environment variables:
#  * The following need to be defined by the caller of this script:
envar+=("ROOT_DIR")
envar+=("CYCLE")
#
# Required AUTOMATICALLY defined environment variables:
#  * The following are required but should already be defined by all.common.sh
#================================================================================
#================================================================================


# run common script setup
set -e
scriptsdir="$( cd "$( dirname "${BASH_SOURCE[0]}" )" && pwd )"
source ${scriptsdir}/all.common.sh
envar_check "${envar[@]}"
set -u


#--------------------------------------------------------------------------------
#--------------------------------------------------------------------------------

# move files from working directory that we want to keep
# TODO

# post processing of the MOM output
# TODO

# compress files that are being kept
# TODO

# remove old files from previous timesteps that we no longer need
# TODO

exit 1
