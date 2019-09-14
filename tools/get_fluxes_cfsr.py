#!/usr/bin/env python3
#================================================================================
# The SLP files have some noise in them due to errors in the converstion from
# pressure to sea level pressure near the orographic boundaries. A correction
# is calculated to remove this noise. The correction files are generated with
# the following procedure:
# 1) 1 year of SLP data is created (without correction applied)
#    get_fluxes_cfsr.py 20120101 20121231 --noslpcorr
# 2) data is averaged
#    cdo timmean -select,name=SLP "*SLP.nc" mean.nc
# 3) remove obviously bad values
#    cdo setrtomiss,102500,120000 -setrtomiss,80000,97500 mean.nc mean2.nc
# 4) smoothing to fill in the removed points
#    cdo smooth,nsmooth=30,radius=0.5,maxpoints=25,weight0=1.0 mean2.nc mean3.nc
# 5) subract the difference
#    cdo sub mean3.nc mean.nc slp_corr.nc
#================================================================================

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
 # [2] = forecast hour to get data from, negative means to an average
 # [3] = precision to use in the compression    
    ('DLWRF.sfc', 11, -6, ".0"),
    ('DSWRF.sfc', 16, -6, ".0"),
    ('PRATE',     31, -6, ".6"),
    ('SLP',       40,  3,  "3"),
    ('SPFH.2m',   39,  3, ".4"),
    ('TMP.2m',    38,  3, ".1"),
    ('UGRD.10m',  36,  3, ".1"),
    ('VGRD.10m',  37,  3, ".1"),
 ]
orog_id = 75
temp_id = 38

nomads_server     = "https://nomads.ncdc.noaa.gov/modeldata/"
nomads_path_cfsr  = nomads_server+"/cmd_flxf/{0:.4}/{0:.6}/{0}/flxf{1:02d}.gdas.{0}{2:02d}.grb2\n"
nomads_path_cfsv2 = nomads_server+"/cfsv2_analysis_flxf/{0:.4}/{0:.6}/{0}/cdas1.t{2:02d}z.sfluxgrbf{1:02d}.grib2\n"

cfsr_end   = dt.date(2011,3,31)
hires_date = dt.date(2011,1,1)

toolsdir=os.path.dirname(os.path.realpath(__file__))


# Get command line arguments 
#------------------------------------------------------------
parser=argparse.ArgumentParser(description=(
    "Download and process CFSR fluxes into the format expected by the Hybrid-GODAS workflow scripts" ))
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
parser.add_argument('--noslpcorr', action="store_true", default=False, help=(
    "do not apply the SLP correction if argument is given"))
args = parser.parse_args()
if args.end_date == None:
    args.end_date = args.start_date

args.outdir=os.path.realpath(args.outdir)
args.tmpdir = args.outdir+'/tmp_'+args.start_date
args.start_date = dt.datetime.strptime(args.start_date, "%Y%m%d").date()
args.end_date   = dt.datetime.strptime(args.end_date,   "%Y%m%d").date()


# remove temporary directory if it already exists
if os.path.exists(args.tmpdir):
   shutil.rmtree(args.tmpdir)


# For each date to process
#------------------------------------------------------------
cdate = args.start_date
while(cdate <= args.end_date):
    date = cdate.strftime("%Y%m%d")

    print("")
    print("************************************************************")
    print("Processing date "+date)
    print("************************************************************")

    hires = cdate >= hires_date

    # create temporary/output directory
    outdir = args.outdir+'/{0:.4}/'.format(date)
    if not os.path.exists(outdir):
        os.makedirs(outdir)
    if not os.path.exists(args.tmpdir):
        os.makedirs(args.tmpdir)


    # get all the files needed
    #------------------------------------------------------------    
    fcst_hrs_needed=list(set([abs(f[2]) for f in fields]))
    if(args.source == 'nomads'):
        path = nomads_path_cfsr if cdate <= cfsr_end else nomads_path_cfsv2
        f = open(args.tmpdir+'/wgetfiles','w')
        for i in fcst_hrs_needed: # forecast hour (F01 - F06)
            for hr in (0,6,12,18): # initial time (every 6 hours)
                f.write(path.format(date,i,hr))
        f.close()
        sp.check_call('wget -nv -i wgetfiles',shell=True, cwd=args.tmpdir)
    else:
        print("ERROR: downloading from UCAR servers needs to be reimplemented")
        sys.exit(1)


    #process the fields
    #------------------------------------------------------------
    for field in fields:
        print(field[0])
        do_avg = field[2] < 0
        
        # determine which input files we need
        match_cfsv2 = "/cdas1.t??z.sfluxgrbf{:02g}.grib2"
        match_cfsr  = "/flxf{:02g}*.grb2"
        match = match_cfsr if cdate <= cfsr_end else match_cfsv2
        infiles = []
        fcst_hrs = field[2] if field[2] is list else [field[2],]
        fcst_hrs = [abs(h) for h in fcst_hrs]
        for f in fcst_hrs:
            infiles += sorted(glob(args.tmpdir+match.format(f)))
               
        # create new netcdf file
        ncd_file = args.tmpdir+'/'+field[0]+'.nc'
        ncd = nc.Dataset(ncd_file,'w', format="NETCDF4_CLASSIC")
        basedate = dt.datetime(1970,1,1)

        # for each input file:
        for i, f in enumerate(infiles):
            grbs = pygrib.open(f)
            grb = grbs.message(field[1])

            # if SLP, we need to get the orography and temperature as well
            if field[0] == 'SLP':
                orog = np.flipud(grbs.message(orog_id).values)
                temp = np.flipud(grbs.message(temp_id).values)

            # add definitions if this is the first iteration
            if i == 0:
                lats, lons = grb.latlons()
                lats = np.flipud(lats)
                
                ncd.createDimension('lon',lons.shape[1])
                ncd.createDimension('lat',lons.shape[0])
                ncd.createDimension('time', None)       

                v = ncd.createVariable('lon', 'f4', ('lon',))
                v.standard_name = "longitude"
                v.long_name     = "longitude"
                v.units         = "degrees_east"
                v.axis          = "X"
                
                v = ncd.createVariable('lat', 'f4', ('lat',))
                v.standard_name = "latitude"
                v.long_name     = "latitude"
                v.units         = "degrees_north"
                v.axis          = "Y"

                v = ncd.createVariable('time', 'f8', ('time',))
                v.standard_name = "time"
                v.long_name     = "time"
                v.units         = "seconds since "+str(basedate)        
                v.calendar      = "standard"
                
                v = ncd.createVariable(field[0], 'f4', ('time','lat','lon'))
                v.long_name=grb['name']
                v.missing_value = grb['missingValue']
                v.units=grb['units']
                v.grid_type="gaussian"
                v.short_name=grb['shortName']

                ncd.variables['lat'][:] = lats[:,0]
                ncd.variables['lon'][:] = lons[0,:]
                
            # get the value, 
            val = np.flipud(grb.values)
            
            # if SLP, convert from pressure to SLP
            if field[0] == 'SLP':
                rgas=287.04
                grav=9.81
                lapserate=6.5e-3
                temp+=orog*lapserate/2.0
                exp=(grav*orog)/(rgas*temp)
                val=val*np.exp(exp)

            # either average it, or use the instant value
            if do_avg:
                if i == 0:
                    val_avg = val
                else:
                    val_avg += val
                if i == len(infiles)-1:
                    ncd.variables[field[0]][0] = val_avg/len(infiles)
                    ncd.variables['time'][0] = \
                        (dt.datetime.combine(cdate, dt.time(12))-basedate).total_seconds()
            else:
                ncd.variables[field[0]][i] = val
                #TODO, the following is wrong if instantaneous values are more frequent
                # than ev 6 hours
                ncd.variables['time'][i] = \
                    (dt.datetime.combine(cdate, dt.time(field[2]+i*6))-basedate).total_seconds()
                
        ncd.close()

        # convert to lower resolution, if needed
        if hires:
            sp.check_call('cdo -s remapbil,{0}/get_fluxes_cfsr.data/remapgrid.nc {1}.nc {1}.nc.tmp && mv {1}.nc.tmp {1}.nc'.format(toolsdir,field[0]),
                          shell=True, cwd=args.tmpdir)

        # add the SLP correction, if this is an SLP field
        if not args.noslpcorr:
            corrfile = toolsdir+'/get_fluxes_cfsr.data/slp_corr.{}.nc'.format(
                'hires' if hires else 'lowres')
            if field[0] == 'SLP':
                sp.check_call('cdo -s add SLP.nc {} SLP.nc.tmp; mv SLP.nc.tmp SLP.nc'.format(
                    corrfile), shell=True, cwd=args.tmpdir)

        # compress
        cmd = 'ncks -O -7 -L 6 --ppc default={1} {0}.nc {0}.nc.tmp && mv {0}.nc.tmp {0}.nc '.format(
            field[0], field[3])
        sp.check_call(cmd, shell=True, cwd=args.tmpdir)

    # tar / move to final location
    sp.check_call('tar -caf {1}/cfsr.{0}.tgz *.nc'.format(date,outdir),
                  shell=True, cwd=args.tmpdir)
                  

    # continue on to the next date
    cdate += dt.timedelta(days=1)
    shutil.rmtree(args.tmpdir)        
