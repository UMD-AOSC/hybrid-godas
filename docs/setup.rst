Initial Setup
===========================

Following examples setup all the data needed for an experiment beginning on 2010-01-01. Modify instructions as neeed for the date you want. Note that most of the scripts discussed can be run with the `-h` option for a complete description of what the tools do and the options available. All required files for a 5 day experiment cycle.


Initial Conditions
--------------------------

Using soda for initial conditions. Dates available are usually from 1980 - 2015, depending on which version of SODA is used. You are free to use any source of initial conditions that you want, but scripts have been added in the `tools/` directory to easily download and process SODA files.

First, we'll download the initial conditions needed for a 10 member ensemble starting on 2010-01-01.

.. code-block:: bash

  ./tools/get_ic_soda.py 20100101 -mem 10



Atmospheric Forcing
-----------------------------

The atmospheric forcing consists of three separate parts

1) High resolution files from the CFSR
2) Low resolution ensemble perturbations that are added onto the CFSR (from the 20CRv3)
3) A monthly climatology bias correction derived from the difference between CFSR and DFS5.2

scripts are included in the `tools/` directory to download and process these separate parts, but you are able to use your own source of forcing so long as they adhere to the formats expected by Hybrid-GODAS.


CFSR Mean forcing
+++++++++++++++++++++++

* since by default daily mean fields are used for radiation and precipitation, and this is considered to be at 12Z, a day extra before and after the desired experiment period is required.

* script works by downloading forecast hours 1-6 for the 4 analysis periods of CFSR (0Z, 6Z, 12Z, 18Z), averaging over the entire 24 hour period for radiation and precipitation, and saving the instantatneous values for the other fields every 6 hours (3Z, 9Z, 15Z, 21Z). Providing similar forcing to the CORE2.0 used by GFDL for their MOM6 experiments.

.. code-block:: bash

  ./tools/get_fluxes_cfsr.py 20091231 20100106


20CRv3 ensemble perturbations
+++++++++++++++++++++++++++++++

bias correction files
++++++++++++++++++++++++++
