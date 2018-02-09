#!/usr/bin/env python3
#--------------------------------------------------------------------------------
# Destagers U/V fields onto the T grid, and rotates from i/j to lat/lon
#------------------------------------------------------------

import os
import numpy as np
import netCDF4 as nc
import mask

_rootdir=os.path.dirname(os.path.abspath(__file__+'/../../'))
_geomFile=_rootdir+'/DATA/grid/ocean_geometry.nc'
_anglFile=_rootdir+'/tools/postproc/grid/grid_angle.nc'
_angles=None
_lat=None
_lon=None



def main():
    import argparse
    global _geomFile
    global _anglFile
    
    parser=argparse.ArgumentParser(description=(
        "UV destaggering and rotation for MOM6 output"))
    parser.add_argument('infile')
    parser.add_argument('outfile')
    parser.add_argument('-u', required=True)
    parser.add_argument('-v', required=True)
    parser.add_argument('-geomFile', default=_geomFile)
    parser.add_argument('-anglFile', default=_anglFile)
    parser.add_argument('--mask', action='store_true')
    parser.add_argument('--nodestag', action='store_true')
    parser.add_argument('--norotate', action='store_true')
    
    args=parser.parse_args()
    
    _geomFile = args.geomFile
    _anglFile = args.anglFile

    args.u=args.u.split(',')
    args.v=args.v.split(',')
    print(args)


    # read input file, create output file
    ncd_in = nc.Dataset(args.infile,  'r')
    ncd_out= nc.Dataset(args.outfile, 'w')

    dims_out=set([])

    # For each u/v pair we need to process
    #------------------------------------------------------------
    for i in range(len(args.u)):

        u_var=args.u[i]
        v_var=args.v[i]        
        u_in = ncd_in.variables[u_var]
        v_in = ncd_in.variables[v_var]        
        u = u_in[:]
        v = v_in[:]

        # create output dimensions if not already created
        dims=[]
        hasXYZ=set([])        
        for d in u_in.dimensions:
            d0=d
            dim_var=None
            if d in ncd_in.variables:
                dim_var=ncd_in.variables[d]
                ax=None
                for a in 'cartesian_axis','axis':
                    if a in dim_var.ncattrs():
                        ax=a
                if ax is not None:
                    ct=dim_var.getncattr(ax)
                    if ct == 'X':
                        d = 'lon'                        
                    elif ct == 'Y':
                        d = 'lat'
                    elif ct == 'Z':
                        d = 'depth'
                    hasXYZ.add(ct)
            if d not in dims_out:
                dims_out.add(d)
                ncd_out.createDimension(d, ncd_in.dimensions[d0].size)
                if dim_var is not None:
                    var=ncd_out.createVariable(d, dim_var.dtype, d)
                    for a in dim_var.ncattrs():
                        var.setncattr(a,dim_var.getncattr(a))
                    if d == "lat":
                        var[:], _ = getLatLon()
                    elif d == "lon":
                        _, var[:] = getLatLon()
                    else:
                        var[:] = dim_var[:]
            dims.append(d)
        hasXYZ = len(hasXYZ & set(('X','Y','Z'))) == 3


        # mask the variables
        if args.mask and hasXYZ:
            u = mask.mask(u)
            v = mask.mask(v)

        # destagger the variables
        if not args.nodestag:
            u = destagger(u, 'U')
            v = destagger(v, 'V')

        # rotate the variables
        if not args.norotate:
            u,v = uvRot(u,v)

        # create variable
        u_out=ncd_out.createVariable(u_var, u_in.dtype, dims)
        v_out=ncd_out.createVariable(v_var, v_in.dtype, dims)
        for var in ( (u_out,u_in), (v_out,v_in)):
            for a in var[1].ncattrs():
                var[0].setncattr(a,var[1].getncattr(a))
        u_out[:] = u
        v_out[:] = v

    ncd_in.close()
    ncd_out.close()



def getLatLon():
    global _lat
    global _lon
    
    if _lat is None:
        ncd=nc.Dataset(_geomFile,'r')
        _lat = ncd.variables['lath'][:]
        _lon = ncd.variables['lonh'][:]
        ncd.close()
    return _lat, _lon



def getAngles():
    global _angles

    if _angles is None:
        _angles=nc.Dataset(_anglFile,'r').variables['angle'][:]
    return _angles



def destagger(field, uv):
    if uv not in ('U','V'):
        raise Exception("'uv' in call to destagger()  must be either 'U' or 'V'")

    if uv == 'U':
        axis=-1
    if uv == 'V':
        axis=-2

    field = (field + np.roll(field,1, len(field.shape)+axis))/2    
    return field



def uvRot(u,v):
    # title the angles array to be the right size
    dims=[x for x in u.shape]
    dims[-1]=1
    dims[-2]=1
    angle2=np.tile(getAngles(), dims)
    
    u2=np.sin(angle2)*v + np.cos(angle2)*u
    v2=np.cos(angle2)*v - np.sin(angle2)*u
    return u2,v2



if __name__=="__main__":
    main()
