#!/usr/bin/env python3
import netCDF4 as nc
import numpy as np
from scipy.spatial import KDTree
import argparse
from math import *

parser=argparse.ArgumentParser()
parser.add_argument('hgrid')
parser.add_argument('output_file')
args=parser.parse_args()


# load the input horizontal grid
nc_i = nc.Dataset(args.hgrid,'r')
g_mask=nc_i.variables['wet'][:]
g_lat=nc_i.variables['geolat'][:]
g_lon=nc_i.variables['geolon'][:]
g_latn=nc_i.variables['lath'][:]
g_lonn=nc_i.variables['lonh'][:]
ny,nx=g_mask.shape
nc_i.close()
f_mask=g_mask.flatten()
f_lat=g_lat.flatten()
f_lon=g_lon.flatten()



def to_Cartesian(lat,lon):
    R = 6371 
    x = R * cos(lat*np.pi/180.0) * cos(lon*np.pi/180.0)
    y = R * cos(lat*np.pi/180.0) * sin(lon*np.pi/180.0)
    z = R * sin(lat*np.pi/180.0)
    return x, y, z


def getDist(x):
    R = 6371 # earth radius
    gamma = 2*np.arcsin(x/(2*R)) # compute the angle of the isosceles triangle
    dist=gamma*R # compute the length of the chord
    dist *= 1000
    return(dist)    


# form a kd-tree with all the LAND points (converted to x,y,z)
print("Creating KDTree of land points...")
tree=KDTree(list(map(to_Cartesian, f_lat[f_mask==0], f_lon[f_mask==0])))

# for each gridpoint, find the closest land point, and get the distance to it
print("Searching kdtree for closest land points...")
points=tree.query(list(map(to_Cartesian, f_lat, f_lon)))

# convert the cartesian distances to spherical distances
print("converting cartesian to spherical distance...")
dist=np.array(list(map(getDist,points[0]))).reshape(ny,nx)

# save to the file
print("done, saving to file...")
nc_o = nc.Dataset(args.output_file, 'w')
nc_o.createDimension('lon', nx)
nc_o.createDimension('lat',ny)
nc_o.createVariable('lon','f4',('lon',))
nc_o.createVariable('lat','f4',('lat',))
nc_o.createVariable('coast_dist','f4',('lat','lon',))
nc_o.variables['lat'].units="degrees_N"
nc_o.variables['lon'].units="degrees_E"
nc_o.variables['coast_dist'].long_name="distance to the coast"
nc_o.variables['coast_dist'].units="meters"

nc_o.variables['coast_dist'][:]=dist
nc_o.variables['lat'][:]=g_latn
nc_o.variables['lon'][:]=g_lonn

nc_o.close()


