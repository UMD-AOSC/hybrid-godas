# stand alone observation-space 3dvar for MOM6 ocean model

## Description##
TODO

##Building##
ln -sf /lustre/f1/pdata/gfdl_O/datasets src/MOM6/.datasets
git submodule update --init --recursive
make fms
make mom