#!/usr/bin/env python3
import pandas as pd
import netCDF4 as nc
import numpy as np
import pickle
from glob import glob
import os, sys, shutil
import argparse

parser=argparse.ArgumentParser(description=(""))
parser.add_argument('path', help=(
   "Path to experiment directory"))
args = parser.parse_args()



fields = ['obid','plat','lat','lon','depth','hr','val','err','qc']
regions=(
    ('GL',90,-90),
    ('NP',90,66),
    ('NM',66,23),
    ('TP',23,-23),
    ('SM',-23,-66),
    ('SP',-66,-90))
depths=(
    ('ALL',0,7000),
    ('0',  0,  25),
    ('1', 25,  50),
    ('2', 50, 100),
    ('3',100, 250),
    ('4',250, 500),
    ('5',500,7000),
)

#============================================================
#============================================================
def procFile(filename):
    stats = {}
    
    # load in the file, afterward "dat" is the pandas dataframe
    print(filename)
    ncd=nc.Dataset(filename,'r')
    dat={}
    for f in fields:
        dat[f] = ncd.variables[f][:]
    dat = pd.DataFrame(dat).dropna()
    dat = dat[dat.qc == 0]
    ncd.close()

    # divide T and S obs
    #TODO, remove the 10, 1
    dat_t = dat[ (dat.obid == 2210) & (np.abs(dat.val) < 10)]
    dat_s = dat[ (dat.obid == 2220) & (np.abs(dat.val) < 1)]
    stats['count_t'] = dat_t.val.count()
    stats['count_s'] = dat_s.val.count()
    
    # divide into SST
    dat_sst = dat_t[dat_t.plat >= 1000]
    stats['count_sst'] = dat_sst.val.count()

    # divide SST into regions
    for r in regions:
        df = dat_sst[ (dat_sst.lat <= r[1]) & (dat_sst.lat >= r[2])]
        stats['count_sst_r'+r[0]] = df.val.count()
        stats[ 'bias_sst_r'+r[0]] = df.val.mean()
        stats[ 'rmsd_sst_r'+r[0]] = np.sqrt((df.val**2).mean())
        stats[ 'stdv_sst_r'+r[0]] = np.std(df.val)

    # divide into profiles
    dat_prof = { 
        't': dat_t[dat_t.plat == 1],
        's': dat_s[dat_s.plat == 1],}
    for v in ('t','s'):
      for r in regions:
         for d in depths:
            df = dat_prof[v][ (dat_prof[v].lat <= r[1]) & (dat_prof[v].lat >= r[2]) & 
                              (dat_prof[v].depth >= d[1]) & (dat_prof[v].depth <= d[2])]
            stats['count_prof_{}_r{}_d{}'.format(v,r[0],d[0])] = df.val.count()
            stats[ 'bias_prof_{}_r{}_d{}'.format(v,r[0],d[0])] = df.val.mean()
            stats[ 'rmsd_prof_{}_r{}_d{}'.format(v,r[0],d[0])] = np.sqrt((df.val**2).mean())
            stats[ 'stdv_prof_{}_r{}_d{}'.format(v,r[0],d[0])] = np.std(df.val)        
    return stats


#============================================================
#============================================================

indir=args.path+'/diag/OmF/*/*.nc'
outdir1=args.path+'/diag/OmF_stats/'
    
for f in sorted(glob(indir)):
    yr = f.split('/')[-1][:4]
    outdir = outdir1+yr
    if not os.path.exists(outdir):
        os.makedirs(outdir)
    stats = procFile(f)
    fn = f.split('/')[-1].split('.')[0]+'.p'
    fh = open(outdir+'/'+fn,'wb')
    pickle.dump(stats, fh)
