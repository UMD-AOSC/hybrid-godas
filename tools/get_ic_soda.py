#!/usr/bin/env python3
import argparse
import os
import urllib.request
from datetime import datetime, timedelta
import math
import subprocess as sp

this_path = os.path.dirname(os.path.realpath(__file__))
default_outdir = os.path.abspath(this_path+'/../DATA/ic/soda_%Y%m%d')

# setup command line arguments
parser=argparse.ArgumentParser(
    description="Download ocean initial conditions from SODA for a date given. If that exact date is not found, the closest available date is used. SODA data is generally available from 1980-2015. If a year beyond 2015 is given, 2015 will be used instead.")
parser.add_argument(
    'date',
    help="a date in YYYYMMDD format")
parser.add_argument(
    '-mem', type=int, default=0,
    help="Number of extra dates to download for ensemble members")
parser.add_argument(
    '-version', default="3.3.1",
    help="The version of SODA to download, list of versions can be found at atmos.umd.edu/~ocean/index_files/soda3_readme.htm")
parser.add_argument(
    '-outdir', default=default_outdir,
    help="directory to place the output files in (default: %(default)s)")
args = parser.parse_args()
args.date = datetime.strptime(args.date, '%Y%m%d')
args.outdir=args.date.strftime(args.outdir)

# get the text file listing all the dates available
dateurl='http://dsrs.atmos.umd.edu/DATA/soda{0}/REGRIDED/soda{0}_5dy_ocean.txt'.format(args.version)
print("reading file "+dateurl)
with urllib.request.urlopen(dateurl) as response:
    dates = response.read()
dates=dates.decode('utf-8').splitlines()
dates = [ datetime.strptime(d[-13:-3],"%Y_%m_%d") for d in dates ]

# so, which date do we need to download? find the closes available before 2015
if args.date.year > 2015:
    args.date = args.date.replace(year=2015)
center_date = None
center_date_diff = timedelta(days=9999)
center_idx = -1
for i,d in enumerate(dates):
    dt = abs(d - args.date)
    if dt < center_date_diff:
        center_date_diff = dt
        center_date = d
        center_idx = i
print("Closest date found: ", center_date)

# prep output directory
if not os.path.exists(args.outdir):
    os.makedirs(args.outdir)

with open(".lonlat", "w+") as f:
    f.write("""
    gridtype = lonlat
    xsize    = 720
    ysize    = 346
    xfirst   = -179.75
    xinc     = 0.5
    yfirst   = -82.75
    yinc     = 0.5  """)

# determine the order of adjacent dates that make up the ensemble
# create two lists, of odd and even member numbers, and then interleave them.
# The final result is a list of members that as the ensemble number increases
# goes away from the central date
mo = dates[center_idx + 1: center_idx + math.ceil(args.mem/2)+1]
me = dates[center_idx - math.floor(args.mem/2): center_idx ]
me.reverse()
m=[i for j in zip(mo, me) for i in j]
m=[dates[center_idx],] + m
# zip doesn't handle lists of different sizes, and the the extra member
# if mo is longer than 
if(len(mo) != len(me)):
    m += [mo[-1],]

# get the files
for i, d in enumerate(m):
    print("")
    print("")
    print("downloading ", d)
    url="http://dsrs.atmos.umd.edu/DATA/soda{0}/REGRIDED/ocean/soda{0}_5dy_ocean_reg_{1}.nc".format(args.version, dates[i].strftime("%Y_%m_%d"))    
    outfile="mem_{:04d}".format(i)

    # download the file
    urllib.request.urlretrieve(url, args.outdir+'/'+outfile+'.nc.tmp')

    # convert files
    sp.run("cdo -f nc4c -z zip6 remapnn,.lonlat -fillmiss -select,name=salt,temp {0}.nc.tmp {0}.nc && rm {0}.nc.tmp".format(args.outdir+'/'+outfile), shell=True, check=True)

# done
os.remove('.lonlat')
