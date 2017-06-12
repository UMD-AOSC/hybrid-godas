#!/usr/bin/env python
import argparse
import netCDF4 as nc
import os
import numpy as np
from glob import glob
import datetime as dt


parser = argparse.ArgumentParser(description=(
  "Masks out the areas on land for the 3d fields of MOM6 output."))
parser.add_argument('path', help=(
  "path to the output directory"))
parser.add_argument('start_date', nargs='?', help=(
  "start date, in YYYYMMDD format, if not provided, ALL dates in the output"
  " directory are used"))
parser.add_argument('end_date',   nargs='?', help=(
  "end date, in YYYYMMDD format, if not provided, only the start_date is used"))
args = parser.parse_args()
if args.start_date is not None:
    if args.end_date is None:
        args.end_date = args.start_date
    args.start_date = dt.datetime.strptime(args.start_date, "%Y%m%d")
    args.end_date = dt.datetime.strptime(args.end_date, "%Y%m%d")


rootdir = os.path.abspath(os.path.dirname(os.path.realpath(__file__))+'/../')

# read in the grid
ncd = nc.Dataset(rootdir+'/DATA/grid/ocean_geometry.nc', 'r')
grid_D = ncd.variables['D'][:]
ncd.close()

ncd = nc.Dataset(rootdir+'/DATA/grid/Vertical_coordinate.nc', 'r')
grid_lvls = ncd.variables['Layer'][:]
ncd.close()

# for each file in the directory that needs to be processed
print("Processing files...")
files = glob(args.path+'/????/*.nc')
for f in files:
    if args.start_date is not None:
        fdate = dt.datetime.strptime(f.split('.')[-2], "%Y%m%d")
        if fdate >args.end_date or fdate < args.start_date:
            continue

    print(f)
    ncd = nc.Dataset(f, 'r+')

    # get all 3d variables (shape is of length 4)
    for v in ncd.variables:
        if(len(ncd.variables[v].shape) == 4):
            dat = ncd.variables[v][0]
            for z in range(len(grid_lvls)):
                msk = grid_D < grid_lvls[z]
                dat[z].mask = msk
            ncd.variables[v][0] = dat
