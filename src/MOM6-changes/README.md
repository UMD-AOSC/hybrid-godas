this directory contains modifications we wish to apply to the default MOM6 code from GFDL currently this consists of:

coupler - the default coupler was modified so that the bulk formula is capable of using 2m T/q and 10 U/V, inline with what the CFSR files provide. Before the bulk formula code was expecting all fields at the same level (10m)


MOM6 - bug fixes were made to the fresh water balance logic, these have been approved by GFDL, but some fixe have not been added to their repository. This MOM6 folder here should be removed once they do update their repository