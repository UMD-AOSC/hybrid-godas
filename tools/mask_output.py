#!/usr/bin/env python3

# TODO, this is really slow, find a way to do it with nco?
import argparse
import netCDF4 as nc
import os
import numpy as np
from glob import glob
import datetime as dt


parser = argparse.ArgumentParser(description=(
  "Masks out the areas on land for the 3d fields of MOM6 output."))
parser.add_argument('file', nargs='+', help=(
  "one or more files to modify in place"))
args = parser.parse_args()


rootdir = os.path.abspath(os.path.dirname(os.path.realpath(__file__))+'/../')


# read in the grid
ncd = nc.Dataset(rootdir+'/DATA/grid/ocean_geometry.nc', 'r')
grid_D = ncd.variables['D'][:]
ncd.close()

ncd = nc.Dataset(rootdir+'/DATA/grid/Vertical_coordinate.nc', 'r')
grid_lvls = ncd.variables['Layer'][:]
ncd.close()


# for each file in the directory that needs to be processed...
print("Processing files...")
for f in args.file:
    print(f)
    ncd = nc.Dataset(f, 'r+')

    for v in ncd.variables:
        if(len(ncd.variables[v].shape) == 4 and ncd.variables[v].shape[1] == len(grid_lvls)):
            print("  ",v)
            # process all 3d variables (shape is of length 4)            
            dat = ncd.variables[v][0]
            for z in range(len(grid_lvls)):
                msk = grid_D < grid_lvls[z]
                dat[z].mask = msk
            ncd.variables[v][0] = dat
