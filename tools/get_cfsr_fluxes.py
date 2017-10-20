#!/usr/bin/env python3
import argparse
import pygrib
import numpy as np
import netCDF4 as nc
import subprocess as sp
import os, shutil, sys
import datetime as dt
from glob import glob

# TODO, need to add the code for converting from surface pressure to SLP


fields = [ 
 # List of all fields that should be extracted
 # [0] = name of field for output file
 # [1] = index in the grb file
 # [2] = forecast hour to get data from (3 or 6)
 ('DSWRF.sfc', 16, "f06"),
 ('DLWRF.sfc', 11, "f06"),
 ('PRATE',     31, "f06"),
 ('PRES.sfc',  40, "f0[1-6]"),
 ('TMP.2m',    38, "f0[1-6]"),
 ('SPFH.2m',   39, "f0[1-6]"),
 ('UGRD.10m',  36, "f0[1-6]"),
 ('VGRD.10m',  37, "f0[1-6]"),
 ]


nomads_server     = "https://nomads.ncdc.noaa.gov/modeldata/"
nomads_path_cfsr  = nomads_server+"/cmd_flxf/{0:.4}/{0:.6}/{0}/flxf{1:02d}.gdas.{0}{2:02d}.grb2\n"
nomads_path_cfsv2 = nomads_server+"/cfsv2_analysis_flxf/{0:.4}/{0:.6}/{0}/cdas1.t{2:02d}z.sfluxgrbf{1:02d}.grib2\n"

cfsr_end   = dt.date(2011,3,31)
hires_date = dt.date(2011,1,1)

toolsdir=os.path.dirname(os.path.realpath(__file__))


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
parser.add_argument('--source', choices={'nomads','ucar'}, default='nomads', help=(
     "source to download data from, default: %(default)s"))

                    
args = parser.parse_args()
if args.end_date == None:
    args.end_date = args.start_date

args.outdir=os.path.realpath(args.outdir)
args.tmpdir = 'tmp_'+args.start_date
args.start_date = dt.datetime.strptime(args.start_date, "%Y%m%d").date()
args.end_date   = dt.datetime.strptime(args.end_date,   "%Y%m%d").date()


# removte temporary directory if it already exists
if os.path.exists(args.tmpdir):
    shutil.rmtree(args.tmpdir)


# For each date to process
#------------------------------------------------------------
cdate = args.start_date
while(cdate <= args.end_date):
    date = cdate.strftime("%Y%m%d")

    print("************************************************************")
    print("Processing date "+date)
    print("************************************************************")

    hires = cdate >= hires_date

    # create temporary/output directory
    outdir = args.outdir+'/{0:.4}/{0}/'.format(date)
    if not os.path.exists(outdir):
        os.makedirs(outdir)
    os.makedirs(args.tmpdir)

    # get all the files needed
    #------------------------------------------------------------    
    if(args.source == 'nomads'):
        path = nomads_path_cfsr if cdate <= cfsr_end else nomads_path_cfsv2
        f = open(args.tmpdir+'/wgetfiles','w')
        for i in range(1,7): # forecast hour (F01 - F06)
            for hr in (0,6,12,18): # initial time (every 6 hours)
                f.write(path.format(date,i,hr))
        f.close()
        sp.check_call('wget -nv -i wgetfiles',shell=True, cwd=args.tmpdir)
    else:
        filename='{}/cdas1.{}.sfluxgrbf.tar'.format(date[:4],date)
        sp.check_call(toolsdir+'/get_cfsr_fluxes.ucar '+filename, shell=True, cwd=args.tmpdir)
        sp.check_call('tar -xaf *.tar', shell=True, cwd=args.tmpdir)
        
    #process the fields
    #------------------------------------------------------------
    for field in fields:
        # create new netcdf file
        ncd_file = outdir+'cfsr.'+date+'.'+field[0]+'.nc'
        ncd = nc.Dataset(ncd_file,'w', format="NETCDF3_CLASSIC")
    
        # open the F0X files and average the values
        avg = None
        cnt = 0
        match_cfsv2 = "/cdas1.t??z.sfluxgrb{}.grib2"
        match_cfsr  = "/flx{}*.grb2"
        match = match_cfsr if cdate <= cfsr_end else match_cfsv2

        for flxfile in glob(args.tmpdir+match.format(field[2])):
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

        # convert to lower resolution, if needed
        if hires:
            sp.check_call('ncks --map="{}" {} tmp.nc'.format(
                    toolsdir+"/get_cfsr_fluxes.data/esmf_CFSRhi2low_patch_weights.nc",
                    ncd_file), shell=True, cwd=args.tmpdir)
            sp.check_call('ncks -O -x -v gw,lat_bnds,lon_bnds,area tmp.nc '+ncd_file,
                    shell=True, cwd=args.tmpdir)
            sp.check_call('ncks -O --mk_rec_dmn time {0} {0}'.format(ncd_file),
                    shell=True, cwd=args.tmpdir)
            os.remove(args.tmpdir+'/tmp.nc')

    # continue on to the next date
    cdate += dt.timedelta(days=1)
    shutil.rmtree(args.tmpdir)

