#!/usr/bin/env python3
#--------------------------------------------------------------------------------
# Compresses a netcdf file by:
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
    parser.add_argument('-lsd',help=("least significant digits"))
    parser.add_argument('-remove')
    parser.add_argument('--verbose')
    args=parser.parse_args()

    def splitList(l):
        out={}
        if l is not None:
            p=l.split(',')
            for p2 in p:
                p3=p2.split('/')
                if len(p3) == 1:
                    out[p3[0]] = None
                elif len(p3) == 2:
                    out[p3[0]] = float(p3[1])
                else:
                    out[p3[0]] = (float(p3[1]), float(p3[2]))
        return out
    args.pack=splitList(args.pack)
    args.lsd=splitList(args.lsd)

    if args.remove is not None:
        args.remove=args.remove.split(',')
    else:
        args.remove=[]
    if args.verbose:
        print(args)

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
        pack = var in args.pack
        doround = var in args.lsd
        if pack and doround:
            print("ERROR, "+var+" was specified for both int packing and float truncation.")
            sys.exit(1)

        if args.verbose:
            print(var, "pack" if pack else ("round" if doround else "none"))
        var_fill=None
        var_chunk=None
        val = v_in[:]
        lsd=None
        if not pack:
            var_type=v_in.dtype
            if "_FillValue" in v_in.ncattrs():
                var_fill=v_in._FillValue
            if doround:
                lsd=args.lsd[var]
        else:
            var_type='i2'
            var_fill=int(-32768)
            if args.pack[var] == None: #auto packing
                val_offset=np.min(val)
                val_scale=(np.max(val)-val_offset)/2**15
            else:
                val_offset,val_scale = args.pack[var]

        # create the variable
        v_out=ncd_out.createVariable(var, var_type, v_in.dimensions, fill_value=var_fill, 
                                     zlib=args.compression > 0, complevel=args.compression, chunksizes=var_chunk,
                                     least_significant_digit=lsd)
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
