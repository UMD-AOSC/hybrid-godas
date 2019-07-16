Jian Kuang 20190627
We don't need this in the future

These are the default MOM/DA configuration files that are copied to an experiment's directory when it is first initialized.

| directory | description |
| ----------| ------------|
| `da` | configuration files for the 3dvar, letkf, observation operators, and other utilities required by the DA step|
| `mom`  | ocean model configuration files. Some of the files are simply links for instances where the file is unchanged from the default `MOM6-examples/ice_ocean_SIS2/OM4_025/` files |
| `mom_input` | links to the `MOM6-examples/ice_ocean_SIS2/OM4_025/` files required for forecast runs |
