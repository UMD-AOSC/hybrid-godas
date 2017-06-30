#!/usr/bin/env python3
import pickle
import numpy as np
import os, sys, shutil
from glob import glob
import pandas as pd
import datetime as dt
import argparse

import matplotlib
matplotlib.use('agg')
import matplotlib.pyplot as plt
import matplotlib.dates as mdates

#regions=('GL','NP','NM','TP','SM','SP')
regions=('GL','NM','TP','SM')

# get the command line arguments
parser = argparse.ArgumentParser(description=(
  "Plotting tools for O-F statistics"))
parser.add_argument('exp', nargs="+", help=(
  "the path to one or more experiments to plot."))
parser.add_argument('--outdir', default="omf_plots", help=(
  "output directory. Default: %(default)s"))
args = parser.parse_args()

# make output directory if it doesn't exist yet
if not os.path.exists(args.outdir):
    os.makedirs(args.outdir)


# load in the data
print("Reading Data...")
files=[]
for e in args.exp:
    files.append(sorted(glob(e+"/diag/OmF_stats/????/*.p")))
dats=[]
for f2 in files:
    dat={}
    for f in f2:
        date = dt.datetime.strptime(f.split('/')[-1][:8],"%Y%m%d").date()
        with open(f,'rb') as fh:
            dat[date]=pickle.load(fh)
    dat = pd.DataFrame(dat).transpose()
    dats.append(dat)

# start plotting it
#============================================================
years = mdates.YearLocator()
months = mdates.MonthLocator()
yearsFmt = mdates.DateFormatter('%Y')

fs=(10,2)

def setAx(ax):
  ax[0,0].set_title('Bias')
  ax[0,1].set_title('RMSD')
  for i in range(ax.shape[0]):
    ax[i,1].yaxis.tick_right()
    for j in range(ax.shape[1]):
        ax[i,j].xaxis.set_ticklabels([])
        ax[i,j].axhline(y=0.0, color='green')
        ax[i,j].xaxis.set_major_locator(years)
        ax[i,j].xaxis.set_major_formatter(yearsFmt)
        ax[i,j].xaxis.set_minor_locator(months)
        ax[i,j].grid(True)


# global values
print("SST globally...")
f, ax = plt.subplots(3, 2, figsize=(fs[0],fs[1]*3))
f.suptitle('Global values')
for d in dats:
    ax[0,0].plot(d.index, d.bias_sst_rGL)
    ax[0,1].plot(d.index, d.rmsd_sst_rGL)
    ax[1,0].plot(d.index, d.bias_prof_t_rGL_dALL)
    ax[1,1].plot(d.index, d.rmsd_prof_t_rGL_dALL)
    ax[2,0].plot(d.index, d.bias_prof_s_rGL_dALL)
    ax[2,1].plot(d.index, d.rmsd_prof_s_rGL_dALL)
setAx(ax)
ax[0,0].set_ylabel('SST')
ax[1,0].set_ylabel('T')
ax[2,0].set_ylabel('S')
plt.savefig(args.outdir+'/global.png')
plt.close('all')


# SST values by region
print("SST by region...")
f, ax = plt.subplots(5, 2, figsize=(fs[0],fs[1]*5))
f.suptitle('SST by region')
for d in dats:
    ax[0,0].plot(d.index, d.bias_sst_rNP)
    ax[0,1].plot(d.index, d.rmsd_sst_rNP)
    ax[1,0].plot(d.index, d.bias_sst_rNM)
    ax[1,1].plot(d.index, d.rmsd_sst_rNM)
    ax[2,0].plot(d.index, d.bias_sst_rTP)
    ax[2,1].plot(d.index, d.rmsd_sst_rTP)
    ax[3,0].plot(d.index, d.bias_sst_rSM)
    ax[3,1].plot(d.index, d.rmsd_sst_rSM)
    ax[4,0].plot(d.index, d.bias_sst_rSP)
    ax[4,1].plot(d.index, d.rmsd_sst_rSP)
setAx(ax)
ax[0,0].set_ylabel('NP 90N-66N')
ax[1,0].set_ylabel('NM 66N-23N')
ax[2,0].set_ylabel('TP 23S-23N')
ax[3,0].set_ylabel('SM 23S-66S')
ax[4,0].set_ylabel('SP 66S-90S')
plt.savefig(args.outdir+'/sst_region.png')
plt.close('all')


#Profile values by region/depth
for v in ('t','s'):
  n = 6
  for r in regions:
    print('profile {} for {} region'.format(v,r))
    f, ax = plt.subplots(n,2, figsize=(fs[0], fs[1]*n))
    f.suptitle('{} profiles in {} region'.format(v,r))
    for d in dats:
      for i in range(n):
        ax[i,0].plot(d.index, d['bias_prof_{}_r{}_d{}'.format(v,r,i)])
        ax[i,1].plot(d.index, d['rmsd_prof_{}_r{}_d{}'.format(v,r,i)])
    setAx(ax)
    ax[0,0].set_ylabel('0m - 25m')
    ax[1,0].set_ylabel('25m - 50m')
    ax[2,0].set_ylabel('50m - 100m')
    ax[3,0].set_ylabel('100m - 250m')
    ax[4,0].set_ylabel('250m - 500m')
    ax[5,0].set_ylabel('+500m')
    
    plt.savefig(args.outdir+'/prof_{}_r{}.png'.format(v,r))
    plt.close('all')

#     for r in regions:
