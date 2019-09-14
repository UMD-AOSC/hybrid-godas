#!/usr/bin/env python3

import argparse
import os, shutil
import subprocess as sp
import datetime as dt
import numpy as np
import netCDF4 as nc

fields = [
    ('DLWRF.sfc', -1),    
    ('DSWRF.sfc', -1),
    ('PRATE',     -1),
    ('UGRD.10m',   6),
    ('TMP.2m',     6),
    ('SPFH.2m',    6),
    ('VGRD.10m',   6),
    ('SLP',        6),
]

max_members=56
server_path="http://portal.nersc.gov/archive/home/projects/incite11/www/20C_Reanalysis_version_2c/everymember_grib_indi_fg_variables"

parser=argparse.ArgumentParser(description=(
    "" ))
parser.add_argument('start_date', help=(
    "start year of download, in YYYYMMD format"))
parser.add_argument('end_date', nargs='?', help=(
    "end year of download, in YYYYMMD format. If not given only"
    " the start year is downloaded"))
parser.add_argument('--mem', type=int, default=max_members, help=(
    "number of ensemble members to keep (default and max is %(default)s)"))
parser.add_argument('--outdir', default=
    os.path.abspath(os.path.dirname(os.path.realpath(__file__))+'/../DATA/fluxes/20CRv2'),
    help=("directory to save final fluxes to. Default: %(default)s"))
args = parser.parse_args()

if args.end_date == None:
    args.end_date = args.start_date
args.outdir=os.path.realpath(args.outdir)
args.start_date = dt.datetime.strptime(args.start_date, "%Y%m%d").date()
args.end_date = dt.datetime.strptime(args.end_date, "%Y%m%d").date()
args.tmpdir = args.outdir+'/tmp_'+str(args.start_date.year)

# remove temporary directory if it already exists
if os.path.exists(args.tmpdir):
    shutil.rmtree(args.tmpdir)

years=list(range(args.start_date.year, args.end_date.year+1))

# for each year to process
for yr in years:
    print("************************************************************")
    print("Processing year {}".format(yr))
    print("************************************************************")
    
    cdate = dt.date(yr,1,1)
    cdate = cdate if args.start_date < cdate else args.start_date
    edate = dt.date(yr,12,31)
    edate = edate if args.end_date > edate else args.end_date
    outdir = args.outdir+'/'+str(yr)

    # create temporary/output directory
    if not os.path.exists(args.tmpdir):
        os.makedirs(args.tmpdir)
    if not os.path.exists(outdir):
        os.makedirs(outdir)
        
    # download the file needed
    for f in fields:
        f_file = 'PRES.sfc' if f[0] == 'SLP' else f[0]
        grbfile='{0}.{1}.ens.grb'.format(f_file, yr)
        tarfile=grbfile+'.tar'
        if os.path.exists(args.tmpdir+'/'+grbfile):
            continue
        print ('downloading {} ...'.format(f))
        sp.check_call('wget {0}/{1}/{2}'.format(
            server_path, f_file.split('.')[0], tarfile), shell=True, cwd=args.tmpdir)
        print('decompressing {} ...'.format(f))
        sp.check_call('tar -xaf {0} && rm {0}'.format(tarfile), shell=True, cwd=args.tmpdir)

    # for each date in this year
    while(cdate <= edate):
        print(str(cdate))

        # prepare ens output directories
        for e in range(1,args.mem+1):
            d = args.tmpdir+'/20CRv2.{}/{:02d}'.format(cdate.strftime("%Y%m%d"), e)
            if not os.path.exists(d):
                os.makedirs(d)
        
        # for each field
        # NOTE: we're doing this a little weird because there seems to be a bug
        # in the latest version of CDO (1.9.x)
        for f in fields:            
            f_file = 'PRES.sfc' if f[0] == 'SLP' else f[0]            
            grbfile='{0}.{1}.ens.grb'.format(f_file, yr)

            # get all the files in that day
            sp.check_call('cdo -s -f nc4c  -splitrec -seldate,{0} {1} {2}.'.format(
                str(cdate), grbfile, f[0]),
                             shell=True, cwd=args.tmpdir)

            do_avg=f[1]<0

            # figure out what the variable name is
            # For some reason is changes depending on cdo version, annoying
            ncd=nc.Dataset(args.tmpdir+'/{}.000001.nc'.format(f[0]),'r')
            varname=list(ncd.variables.keys())[-1]
                
            # for each ensemble member
            for m in range(1,args.mem+1):
                # determine which files numbers to use
                idx = np.arange(m,449,56) if do_avg else np.arange(56+m,449,112)

                # combine files
                files=[ '{0}.{1:06d}.nc'.format(f[0],i) for i in idx]
                files = ' '.join(files)
                if do_avg:
                    cmd = '-settime,12:00:00 -ensmean "{0}"'
                else:
                    cmd =  '-mergetime "{0}"'
                cmd = 'cdo -s chname,{1},{2} '+cmd+' tmp.nc'

                sp.check_call(cmd.format(files,varname,f[0]),
                    shell=True, cwd=args.tmpdir)

                sp.check_call('ncks -O -7 -L 6 --ppc default=5 tmp.nc 20CRv2.{0}/{1:02d}/20CRv2.{0}.{2}.nc && rm tmp.nc'.format(
                    cdate.strftime("%Y%m%d"), m, f[0]), shell=True, cwd=args.tmpdir)

        # compress and move
        sp.check_call('tar -caf {1}/20CRv2.{0}.tgz 20CRv2.{0}'.format(
            cdate.strftime("%Y%m%d"), outdir),
                      shell=True, cwd=args.tmpdir)

        # cleanup
        sp.check_call('rm -r *.nc 20CRv2.{}'.format(cdate.strftime("%Y%m%d")), shell=True, cwd=args.tmpdir)
            
        cdate += dt.timedelta(days=1)

 
    # cleanup the grb files
    shutil.rmtree(args.tmpdir)
