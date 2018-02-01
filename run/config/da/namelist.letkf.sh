#!/bin/bash
set -e
set -u

cat <<EOF

&letkf_settings
  !--------------------------------------------------------------------------------
  ! Overall variables needed by the LETKF program
  !--------------------------------------------------------------------------------
/



&letkf_mpi
  !--------------------------------------------------------------------------------
  ! mem         = number of ensemble members 
  ! interleave  = if true, distributes gridpoints to the mpi processes in a
  !               round-robin manner. Slows down grid scatter and gather a little,
  !               but helps prevent mpi work imbalance for the letkf core solver
  !               TODO: need to do this correctly with MPI strides
  !               (default: T)
  !--------------------------------------------------------------------------------
  mem = ${ENS_SIZE}
  ppn=36
/



&letkf_obs
  !--------------------------------------------------------------------------------
  ! Observation input and quality control
  !
  ! obs_test     = if true, reads from the "obstest.cfg" file to generate test
  !                observations based on a given increment from the background mean
  !                (default: F)
  ! ioclass      = the observation I/O class to use, built-ins include "LETKF_DAT" and
  !                "LETKF_NC",
  ! obsqc_maxstd = 
  !--------------------------------------------------------------------------------
  obs_test = F
  ioclass = 'LETKF_NC'
  obsqc_maxstd = 3.0
/



&letkf_state
  !--------------------------------------------------------------------------------
  !--------------------------------------------------------------------------------	
  ioclass = 'LETKF_NC'
  grid_nx = 1440
  grid_ny = 1080
  grid_nz = 75
  grid_ns = 150
/



&letkf_inflation
  !--------------------------------------------------------------------------------
  ! infl_mul  : fixed constant multiplicative inflation
  !             ( >= 1.0)  1.0 is considered OFF
  ! infl_rtps : relaxation to prior spread
  !             ( >= 0.0, <= 1.0), 0.0 is considered OFF
  ! infl_rtpp : relaxation to prior perturbations
  !             ( >= 0.0, <= 1.0), 0.0 is considered OFF
  !--------------------------------------------------------------------------------  
  infl_mul  = 1.0
  infl_rtps = 1.0
  infl_rtpp = 0.0
/



&letkf_loc
  !--------------------------------------------------------------------------------
  !--------------------------------------------------------------------------------
!  loc_hz = 720.0d3, 200.0d3
!  loc_prune = 1e-1
   locclass = 'loc_novt'
/



EOF
