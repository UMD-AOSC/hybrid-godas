#!/usr/bin/env python3
import netCDF4 as nc
import numpy as np
import argparse
from scipy import interpolate

# get command line arguments
parser=argparse.ArgumentParser(description=( ""))
parser.add_argument('outfile')
parser.add_argument('-nx', type=int, required=True)
parser.add_argument('-ny', type=int, required=True)
parser.add_argument('-scale', default='100/0.2,30/0.6,3/0.1')
parser.add_argument('-month', type=int, required=True)
parser.add_argument('-year',  type=int, required=True)
parser.add_argument('-seed', type=int, required=True)

args=parser.parse_args()
args.scale=[tuple([float(f) for f in a.split("/")]) for a in args.scale.split(",")]
print(args)

grid=(args.ny, args.nx)
gridx=np.arange(args.nx)/args.nx
gridy=np.arange(args.ny)/args.ny

noise=np.zeros( (12, args.ny, args.nx))


for mn in range(12):
    seed=args.seed*100000 + args.year*12 + mn
    if args.month == 12 and mn == 0:
        seed += 12
    if args.month == 1 and mn == 11:
        seed -= 12

#    print(seed)
    np.random.seed(seed)
    for scale in args.scale:
        ngrid=[int(g/scale[0]) for g in grid]
        ngridx=np.arange(ngrid[1])*1.0/(ngrid[1]-1)
        ngridy=np.arange(ngrid[0])*1.0/(ngrid[0]-1)
        nval=np.random.rand(ngrid[0],ngrid[1])*2.0-1.0
        nval[:,-1]=nval[:,0]
        nval[-1,-int(ngrid[1]/2):] = np.flip(nval[-1,0:int(ngrid[1]/2)],0)
        f=interpolate.interp2d(ngridx,ngridy,nval, kind='cubic')
        noise[mn,:,:]+=f(gridx,gridy)*scale[1]

noise[noise > 1.0] = 1.0
noise[noise < -1.0] = -1.0

# write out the file
nco=nc.Dataset(args.outfile, 'w')
nco.createDimension('time', None)
nco.createDimension('j', args.ny)
nco.createDimension('i', args.nx)
nco.createVariable('noise', 'f4', ('time','j','i'))
nco.variables['noise'][:] = noise
nco.close()
