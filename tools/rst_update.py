#!/usr/bin/env python3
import netCDF4 as nc
import argparse


vrange={}
vrange['Temp']=(-2.1, 45.0)
vrange['Salt']=(0, 45.0)

# read command line arguments
parser=argparse.ArgumentParser()
parser.add_argument('bkg_rst')
#parser.add_argument('ana_rst')
parser.add_argument('out_rst')
parser.add_argument('-vars')
parser.add_argument('-var')
parser.add_argument('-ekf')
parser.add_argument('-alpha',type=float, default=0.5)
args=parser.parse_args()
print(args)


# open / create files
ncd_bkg=nc.Dataset(args.bkg_rst,'r')
ncd_out=nc.Dataset(args.out_rst,'w')
if args.ekf is not None:
    ncd_ekf=nc.Dataset(args.ekf,'r')
if args.var is not None:
    ncd_var=nc.Dataset(args.var, 'r')


for d in ncd_bkg.dimensions:
    ncd_out.createDimension(d, ncd_bkg.dimensions[d].size)

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
        if args.ekf is not None:
            val=ncd_ekf[var][:]
        else:
            val=ncd_bkg[var][:]
        if args.var is not None:
            val += args.alpha * ncd_var[var][:]
    else:
        val=ncd_bkg[var][:]

    # make sure variable is in range
    if var in vrange:
        val[val < vrange[var][0]] = vrange[var][0]
        val[val > vrange[var][1]] = vrange[var][1]

    var_out[:] = val

ncd_out.close()
ncd_bkg.close()


#ncd_ekf.close()
#ncd_var.close()
        


