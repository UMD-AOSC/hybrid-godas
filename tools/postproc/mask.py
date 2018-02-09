#!/usr/bin/env python3
#--------------------------------------------------------------------------------
#  mask out land values of MOM6 output files
#--------------------------------------------------------------------------------
import os
import netCDF4 as nc
import numpy as np

_rootdir=os.path.dirname(os.path.abspath(__file__+'/../../'))
_geomFile=_rootdir+'/DATA/grid/ocean_geometry.nc'
_vertFile=_rootdir+'/DATA/grid/Vertical_coordinate.nc'

_mask=None
_mask3d=None



def main():
    import argparse
    global _geomFile
    global _vertFile
    
    parser=argparse.ArgumentParser(description=(
        "Apply land masking to a MOM6 output file"))
    parser.add_argument('infile')
    parser.add_argument('outfile')
    parser.add_argument('-geomFile', default=_geomFile)
    parser.add_argument('-vertFile', default=_vertFile)
    parser.add_argument('-ignore')
    parser.add_argument('-only')

    args=parser.parse_args()
    if args.ignore is not None and args.only is not None:
        print("ERROR, only one of '-ignore' or '-only' can be specified")
        sys.exit(1)
    print(args)

    _geomFile = args.geomFile
    _vertFile = args.vertFile

    # read input file, create output file
    ncd_in = nc.Dataset(args.infile,  'r')
    ncd_out= nc.Dataset(args.outfile, 'w')

    # for each var we are processing
    varnames = ncd_in.variables.keys()
    if args.ignore is not None:
        varnames = [v for v in varnames if v not in args.ignore]
    elif args.only is not None:
        varnames = [v for v in varnames if v in args.only]

    for var in varnames:
        v_in = ncd_in.variables[var]

        if var in ncd_in.dimensions:
            continue

        # create output dimensions needed for this variable if not already created
        dims=[]
        hasXYZ=set([])
        for d in v_in.dimensions:
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
            if d not in ncd_out.dimensions:
                ncd_out.createDimension(d, ncd_in.dimensions[d0].size)
                if dim_var is not None:
                    v=ncd_out.createVariable(d, dim_var.dtype, d)
                    for a in dim_var.ncattrs():
                        v.setncattr(a, dim_var.getncattr(a))
                    # TODO set lat/lon for outside file instead?
                    v[:] = dim_var[:]
            dims.append(d)
        hasXYZ = len(hasXYZ & set(('X','Y','Z'))) == 3

        v = v_in[:]
        if hasXYZ:
            print("masking ",var)
            v=mask(v)

        # write out the variable
        v_out=ncd_out.createVariable(var, v_in.dtype, dims)
        for a in v_in.ncattrs():
            v_out.setncattr(a, v_in.getncattr(a))
        v_out[:] = v

    ncd_in.close()
    ncd_out.close()



def getMask3D():
    global _mask
    global _mask3d

    if _mask is None:
        ncd=nc.Dataset(_geomFile,'r')
        _mask=ncd.variables['wet'][:]
        grid_depth=ncd.variables['D'][:]
        ncd=nc.Dataset(_vertFile,'r')
#        grid_lvls=ncd.variables['Interface'][0:-1]
        grid_lvls=ncd.variables['Layer'][:]
        ncd.close()
    
        nx=_mask.shape[1]
        ny=_mask.shape[0]
        nz=len(grid_lvls)
#        _mask3d=np.tile(grid_lvls.reshape(1,nz,1,1), (1,1,ny,nx)) >= np.tile(grid_depth, (1,nz,1,1))
        _mask3d=np.tile(grid_lvls.reshape(nz,1,1), (1,ny,nx)) >= np.tile(grid_depth, (nz,1,1))
    return _mask3d



def mask(field):

    m=getMask3D()
    if len(field.shape) > 3:
        m=m.reshape( (1,) + m.shape )

    if np.ma.is_masked(field):
        field.mask=np.logical_or(m, field.mask)
    else:
        field = np.ma.masked_where(m,field )
    return field
    


if __name__=="__main__":
    main()
