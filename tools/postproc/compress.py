#!/usr/bin/env python3
#--------------------------------------------------------------------------------
# Compresses a netcdf file by packing floats into integers rand enabling zlib
#--------------------------------------------------------------------------------
import netCDF4 as nc



def main():
    import argparse
    import numpy as np
    
    parser=argparse.ArgumentParser(description=(
        "Pack and compress MOM6 output file"))
    parser.add_argument('infile')
    parser.add_argument('outfile')
    parser.add_argument('-compression', default=4, type=int)
    parser.add_argument('-pack')
    parser.add_argument('-remove')
    args=parser.parse_args()

    packing={}
    if args.pack is not None:
        p=args.pack.split(',')
        for p2 in p:
            p3=p2.split('/')
            if len(p3) == 1:
                packing[p3[0]] = None
            else:
                packing[p3[0]] = (float(p3[1]), float(p3[2]))
    print(packing)

    if args.remove is not None:
        args.remove=args.remove.split(',')
    else:
        args.remove=[]


    # open files
    ncd_in=nc.Dataset(args.infile, 'r')
    ncd_out=nc.Dataset(args.outfile, 'w')


    # for each variable we are processing
    varnames = sorted(ncd_in.variables.keys())
    varnames = [v for v in varnames if v not in args.remove]
    #TODO, filter some out?
    for var in varnames:
        v_in = ncd_in.variables[var]
        if var in ncd_in.dimensions:
            continue

        # create needed dimensions
        for d in v_in.dimensions:
            if d not in ncd_out.dimensions:
                ncd_out.createDimension(d, ncd_in.dimensions[d].size)
                if d in ncd_in.variables:
                    dim_var = ncd_in.variables[d]
                    v=ncd_out.createVariable(d, dim_var.dtype, d, zlib=args.compression>0, complevel=args.compression)
                    for a in dim_var.ncattrs():
                        v.setncattr(a, dim_var.getncattr(a))
                        v[:] = dim_var[:]


        # determine paramaters for packing vs non-packing
        pack = var in packing        
        print(var,pack)

        var_fill=None
        var_chunk=None
        val = v_in[:]
        if not pack:
            var_type=v_in.dtype
            if "_FillValue" in v_in.ncattrs():
                var_fill=v_in._FillValue
        else:
            var_type='i2'
            var_fill=int(-32768)
            if packing[var] == None: #auto packing
                val_offset=np.min(val)
                val_scale=(np.max(val)-val_offset)/2**15
            else:
                val_offset,val_scale = packing[var]
                

        # create the variable
        v_out=ncd_out.createVariable(var, var_type, v_in.dimensions, fill_value=var_fill, 
                                     zlib=args.compression > 0, complevel=args.compression, chunksizes=var_chunk)
        for a in v_in.ncattrs():
            if a[0] != "_" and a not in ('missing_value'):
                v_out.setncattr(a,v_in.getncattr(a))
        if pack:
            v_out.add_offset=val_offset
            v_out.scale_factor=val_scale

        v_out[:] = val

    ncd_in.close()
    ncd_out.close()



if __name__=="__main__":
    main()
