#!/usr/bin/env python3
import argparse
import netCDF4 as nc
import numpy as np

parser=argparse.ArgumentParser()
parser.add_argument("ai_file")
parser.add_argument("restart_dir")

args = parser.parse_args()

# update Temp, Salt
ncd_ai = nc.Dataset(args.ai_file, 'r')
ncd_r = nc.Dataset(args.restart_dir+'/MOM.res.nc', 'r+')

print("temp background: ",np.min(ncd_r.variables['Temp'][:]),np.max(ncd_r.variables['Temp'][:]))
t2 = ncd_r.variables['Temp'][:] + ncd_ai.variables['ai_temp'][:]
m = t2 < 0
t2[m] = ncd_r.variables['Temp'][:][m]
ncd_r.variables['Temp'][:] = t2
print("temp analysis: ",np.min(ncd_r.variables['Temp'][:]),np.max(ncd_r.variables['Temp'][:]))

print("salt background: ",np.min(ncd_r.variables['Salt'][:]),np.max(ncd_r.variables['Salt'][:]))
ncd_r.variables['Salt'][:] += ncd_ai.variables['ai_salt'][:]
print("salt analysis: ",np.min(ncd_r.variables['Salt'][:]),np.max(ncd_r.variables['Salt'][:]))

ncd_r.close()
ncd_ai.close()
