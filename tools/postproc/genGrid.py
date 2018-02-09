#!/usr/bin/env python3
# generate the input/output grid specifications for ESMF to later use

import netCDF4 as nc
import numpy as np
import argparse
import sys,os
import math

parser=argparse.ArgumentParser()
parser.add_argument("--grid", default='ocean_hgrid.nc')
parser.add_argument("--mask", default="ocean_mask.nc")
args=parser.parse_args()
print(args)



# Create output grid in various resolutions
#--------------------------------------------------------------------------------
#bounds=(-75,90)
bounds=(-90,90)
if (not os.path.exists('grid')):
    os.makedirs('grid')

for res in ('1','05','025'):
    print("Creating output lat/lon grid for resolution: ",res)

    r={'1':1, '05':2, '025': 4}[res]
    lons_edge=np.linspace(0,360, 360*r+1)
    lats_edge=np.linspace(*bounds, (bounds[1]-bounds[0 ])*r +1)
    lons_cntr= (lons_edge[:-1] + lons_edge[1:]) / 2.0
    lats_cntr= (lats_edge[:-1] + lats_edge[1:]) / 2.0
    xt,yt=np.meshgrid(lons_cntr,lats_cntr)
    xq,yq=np.meshgrid(lons_edge,lats_edge)

    nco=nc.Dataset('grid/grid_latlon_'+res+'.nc','w')
    osize=np.prod(xt.shape)
    nco.createDimension('grid_size', osize)
    nco.createDimension('grid_corners', 4)
    nco.createDimension('grid_rank', 2)

    v=nco.createVariable('grid_dims','i4',( 'grid_rank', ))
    v[:] = [y for y in reversed(xt.shape)]

    v=nco.createVariable('grid_center_lon', 'f8', ('grid_size',))
    v.units="degrees"
    v[:]=xt.flatten()

    v=nco.createVariable('grid_center_lat', 'f8', ('grid_size',))
    v.units="degrees"
    v[:]=yt.flatten()

    v=nco.createVariable('grid_imask','i4', ('grid_size',))
    v[:]=np.ones(xt.shape).flatten()

    c=np.zeros( (osize,4))
    v=nco.createVariable('grid_corner_lon', 'f8', ('grid_size','grid_corners'))
    v.units='degrees'
    c[:,0]=xq[:-1,:-1].flatten()
    c[:,1]=xq[:-1, 1:].flatten()
    c[:,2]=xq[1: , 1:].flatten()
    c[:,3]=xq[1: ,:-1].flatten()
    v[:] = c

    v=nco.createVariable('grid_corner_lat', 'f8', ('grid_size','grid_corners'))
    v.units='degrees'
    c[:,0]=yq[:-1,:-1].flatten()
    c[:,1]=yq[:-1, 1:].flatten()
    c[:,2]=yq[1: , 1:].flatten()
    c[:,3]=yq[1: ,:-1].flatten()
    v[:] = c



# Create the input grid, each for U/V/T
#--------------------------------------------------------------------------------
# load input grid
ncd=nc.Dataset(args.grid,'r')
x=ncd.variables['x'][:]
grid_x=ncd.variables['x'][:]
grid_y=ncd.variables['y'][:]
ncd.close()

grid_xt=grid_x[1::2,1::2]
grid_yt=grid_y[1::2,1::2]
#grid_xq=grid_x[::2,::2]
#grid_yq=grid_y[::2,::2]
grid_size=np.prod(grid_xt.shape)
grid_xb=np.roll(grid_x,-1,0)
grid_xb=np.roll(grid_x,-1,1)
grid_yb=np.roll(grid_y,-1,0)
grid_yb=np.roll(grid_y,-1,1)

# load input mask
grid_mask=nc.Dataset(args.mask, 'r').variables['mask'][:]

# create angle output
nco=nc.Dataset('grid/grid_angle.nc','w')
nco.createDimension('nx', grid_xt.shape[1])
nco.createDimension('ny', grid_xt.shape[0])
v=nco.createVariable('angle','f4', ('ny','nx'))
x1=np.radians(grid_x[1::2,1::2])
x2=np.radians(grid_xb[1::2,1::2])
y1=np.radians(grid_y[1::2,1::2])
y2=np.radians(grid_yb[1::2,1::2])
dx=x2-x1
v[:]=np.arctan2(np.sin(dx)*np.cos(y2), 
                np.cos(y1)*np.sin(y2) - np.sin(y1)*np.cos(y2)*np.cos(dx)) - np.radians(90.0)
nco.close()


# create input grid file definitions for ESMF regrid
print("creating input MOM6 grid")

nco=nc.Dataset('grid/grid_t.nc','w')
d_size=nco.createDimension('grid_size', grid_size)
d_corn=nco.createDimension('grid_corners', 4)
d_rank=nco.createDimension('grid_rank', 2)

v=nco.createVariable('grid_dims','i4',( 'grid_rank', ))
v[:] = [y for y in reversed(grid_xt.shape)]

v=nco.createVariable('grid_center_lon', 'f8', ('grid_size',))
v.units="degrees"

v=nco.createVariable('grid_center_lat', 'f8', ('grid_size',))
v.units="degrees"

v=nco.createVariable('grid_imask','i4', ('grid_size',))
v[:]=grid_mask.flatten()

v=nco.createVariable('grid_corner_lon', 'f8', ('grid_size','grid_corners'))
v.units='degrees'

v=nco.createVariable('grid_corner_lat', 'f8', ('grid_size','grid_corners'))
v.units='degrees'

c=np.zeros( (grid_size,4))
nco.variables['grid_center_lon'][:] = grid_x[1::2,1::2].flatten()
nco.variables['grid_center_lat'][:] = grid_y[1::2,1::2].flatten()
c[:,0]=grid_x[0:-1:2,0:-1:2].flatten()
c[:,1]=grid_x[0:-1:2,2:  :2].flatten()
c[:,2]=grid_x[2:  :2,2:  :2].flatten()
c[:,3]=grid_x[2:  :2,0:-1:2].flatten()
nco.variables['grid_corner_lon'][:] = c
c[:,0]=grid_y[0:-1:2,0:-1:2].flatten()
c[:,1]=grid_y[0:-1:2,2:  :2].flatten()
c[:,2]=grid_y[2:  :2,2:  :2].flatten()
c[:,3]=grid_y[2:  :2,0:-1:2].flatten()
nco.variables['grid_corner_lat'][:] = c
nco.close()
