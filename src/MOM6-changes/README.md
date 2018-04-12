this directory contains modifications we wish to apply to the default MOM6 code from GFDL currently this consists of:

* *coupler* - the default coupler was modified so that the bulk formula is capable of using 2m T/q and 10 U/V, inline with what the CFSR files provide. Before the bulk formula code was expecting all fields at the same level (10m). Also, flag for using the COARE4 bulk formula was added.

* *memory* - static memory parameters, runs a little bit faster that way

