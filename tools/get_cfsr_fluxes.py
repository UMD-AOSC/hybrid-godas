#!/usr/bin/env python3
import argparse
import pygrib
import numpy as np
import netCDF4 as nc
import subprocess as sp
import os, shutil, sys
import datetime as dt
from glob import glob


fields = [ 
 # List of all fields that should be extracted
 # [0] = name of field for output file
 # [1] = index in the grb file
 # [2] = forecast hour to get data from (3 or 6)
 ('DSWRF.sfc', 16, "f06"),
 ('DLWRF.sfc', 11, "f06"),
 ('PRATE',     31, "f06"),
 ('PRES.sfc',  40, "f*"),
 ('TMP.2m',    38, "f*"),
 ('SPFH.2m',   39, "f*"),
 ('UGRD.10m',  36, "f*"),
 ('VGRD.10m',  37, "f*"),
 ]

server = "https://nomads.ncdc.noaa.gov/modeldata/cmd_flxf"


# Get command line arguments 
#------------------------------------------------------------
parser=argparse.ArgumentParser(description=(
    "Download and process CFSR fluxes into daily means" ))
parser.add_argument('start_date', help=(
    "start date of download, in YYYYMMDD format"))
parser.add_argument('end_date', nargs='?', help=(
    "end date of download, in YYYYMMDD format. If not given "
    " only the start date is downloaded"))
parser.add_argument('--outdir', default = 
    os.path.abspath(os.path.dirname(os.path.realpath(__file__))+'/../DATA/fluxes/cfsr'), help=(
    "Directory to save final fluxes to. Default: %(default)s"))
                    
args = parser.parse_args()
if args.end_date == None:
    args.end_date = args.start_date

args.tmpdir = 'tmp_'+args.start_date
args.start_date = dt.datetime.strptime(args.start_date, "%Y%m%d").date()
args.end_date   = dt.datetime.strptime(args.end_date,   "%Y%m%d").date()


min_date = dt.date(1979,1,1)
max_date = dt.date(2011,3,31)

if( args.start_date < min_date or args.start_date > max_date):
    print("start date out of range")
    sys.exit(1)
if( args.end_date < min_date or args.end_date > max_date):
    print("end date out of range")
    sys.exit(1)

# removte temporary directory if it already exists
if os.path.exists(args.tmpdir):
    shutil.rmtree(args.tmpdir)


# For each date to process
#------------------------------------------------------------
cdate = args.start_date
while(cdate <= args.end_date):
    date = cdate.strftime("%Y%m%d")
    outdir = args.outdir+'/{0:.4}/{0}/'.format(date)

    # create temporary directory
    if not os.path.exists(outdir):
        os.makedirs(outdir)

    # get all the files needed
    #------------------------------------------------------------
    os.makedirs(args.tmpdir)
    f = open(args.tmpdir+'/wgetfiles','w')
    for i in range(1,7): # forecast hour (F01 - F06)
        for hr in (0,6,12,18): # initial time (every 6 hours)
            f.write("{0}/{1:.4}/{1:.6}/{1}/flxf{2:02d}.gdas.{1}{3:02d}.grb2\n".format(server,date,i,hr))
    f.close()
    sp.check_call('wget -nv -i wgetfiles',shell=True, cwd=args.tmpdir)
            

    #process the fields
    #------------------------------------------------------------
    for field in fields:
        # create new netcdf file
        ncd = nc.Dataset(outdir+'cfsr.'+date+'.'+field[0]+'.nc','w', format="NETCDF3_CLASSIC")
    
        # open the F03 or F06 files and average the values
        avg = None
        cnt = 0
        for flxfile in glob(args.tmpdir+'/flx{}*.grb2'.format(field[2])):
            grbs=pygrib.open(flxfile)
            grb = grbs.message(field[1])
            cnt += 1
            if avg is None:
                avg = grb.values    
            else:
                avg += grb.values
        print(field[0], cnt)
        avg = np.flipud(avg) / cnt

        lats, lons = grb.latlons()
        lats = np.flipud(lats)
        
        # write out to netCDF file
        ncd.createDimension('lon',lons.shape[1])
        ncd.createDimension('lat',lons.shape[0])
        ncd.createDimension('time',None)
        
        v = ncd.createVariable('lon', 'f4', ('lon',))
        v.standard_name = "longitude"
        v.long_name     = "longitude"
        v.units         = "degrees_east"
        v.axis          = "X"
        v[:] = lons[0,:]
        
        v = ncd.createVariable('lat', 'f4', ('lat',))
        v.standard_name = "latitude"
        v.long_name     = "latitude"
        v.units         = "degrees_north"
        v.axis          = "Y"
        v[:] = lats[:,0]
        
        basedate = dt.datetime(1970,1,1)
        v = ncd.createVariable('time', 'f8', ('time',))
        v.standard_name = "time"
        v.long_name     = "time"
        v.units         = "seconds since "+str(basedate)        
        v.calendar      = "standard"
        v[0] = (dt.datetime.combine(cdate, dt.time(12))-basedate).total_seconds()

        v = ncd.createVariable(field[0], 'f4', ('time','lat','lon'))
        v.long_name=grb['name']
        v.missing_value = grb['missingValue']
        v.units=grb['units']
        v.grid_type="gaussian"
        v.short_name=grb['shortName']
        v[0] = avg

        ncd.close()

    # continue on to the next date
    cdate += dt.timedelta(days=1)
    shutil.rmtree(args.tmpdir)

