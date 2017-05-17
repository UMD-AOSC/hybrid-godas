#!/usr/bin/env python3
import argparse
import datetime as dt
import ftplib
import os


parser=argparse.ArgumentParser()
parser.add_argument('start_date')
parser.add_argument('end_date', nargs='?')
args = parser.parse_args()

if args.end_date == None:
    args.end_date = args.start_date

args.start_date = dt.datetime.strptime(args.start_date, "%Y%m%d").date()
args.end_date   = dt.datetime.strptime(args.end_date,   "%Y%m%d").date()
args.ftpsite = "ftp.nodc.noaa.gov"
args.ftpdir = "/pub/data.nodc/pathfinder/Version5.2"
args.obsdir = os.path.abspath(os.path.dirname(os.path.realpath(__file__))+'/../DATA/obs/sst_pathfinder')
print(args)    

#------------------------------------------------------------

ftp = ftplib.FTP(args.ftpsite)
ftp.login()

cyear = args.start_date.year
while cyear <= args.end_date.year:    
    # get an new directory listing from the server for the given year
    ftp.cwd(args.ftpdir+'/{}'.format(cyear))
    files = ftp.nlst()
    prevdate = None
    for f in files:
        fdate = dt.datetime.strptime(f[:14],"%Y%m%d%H%M%S")
        daynight=f.split('_')[-1].split('-')[0]
        if daynight != 'night':
            continue
        if fdate.date() > args.end_date or fdate.date() < args.start_date:
            continue
        outdir = args.obsdir+fdate.strftime("/%Y/%Y%m")
        if not os.path.exists(outdir):
            os.makedirs(outdir)
        print (fdate.strftime("%Y-%m-%d"))
        #TODO - handle multiple files on same date
        file = open(outdir+fdate.strftime('/%Y%m%d.nc'), 'wb')
        ftp.retrbinary('RETR ' + f, file.write)
        file.close()
    cyear += 1


