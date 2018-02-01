#!/bin/bash
set -e
set -u

cat <<EOF

&params_model_nml
/

&params_obs_nml
/

&params_letkf_nml
  nbv = 3,
  nslots = 5,
  nbslot = 5,
  gross_error=3.0
/

EOF
