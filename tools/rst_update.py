#!/usr/bin/env python3
import netCDF4 as nc
import argparse


# TODO, allow recentering on 3dvar analysis
# TODO add some global attributes to the output
#  indicating it was updated with DA analysis

# read command line arguments
parser=argparse.ArgumentParser()
parser.add_argument('bkg_rst')
parser.add_argument('ana_rst')
parser.add_argument('out_rst')
parser.add_argument('-vars')
args=parser.parse_args()
print(args)


# open / create files
ncd_bkg=nc.Dataset(args.bkg_rst,'r')
ncd_ana=nc.Dataset(args.ana_rst,'r')
ncd_out=nc.Dataset(args.out_rst,'w')
for d in ncd_bkg.dimensions:
    ncd_out.createDimension(d, ncd_bkg.dimensions[d].size)


# determine which variables to copy from the restart update file
if args.vars is None:
    args.vars = [v for v in ncd_ana.variables if v not in ncd_ana.dimensions]
print("Updating: ",args.vars)


for a in ncd_bkg.ncattrs():
    ncd_out.setncattr(a, ncd_bkg.getncattr(a))

# copy the variables
for var in ncd_bkg.variables:
    print (var)
    var_bkg=ncd_bkg.variables[var]
    var_out=ncd_out.createVariable(var, var_bkg.dtype, var_bkg.dimensions)

    for a in var_bkg.ncattrs():
        var_out.setncattr(a, var_bkg.getncattr(a))

    # update with new values
    if var in args.vars:
        var_out[:] = ncd_ana[var][:]
    else:
        var_out[:] = ncd_bkg[var][:]

ncd_out.close()
ncd_bkg.close()
ncd_ana.close()
        


