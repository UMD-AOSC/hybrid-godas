#!/usr/bin/env python3
import argparse
import datetime as dt
import ftplib
import os, shutil

rootdir=os.path.abspath(os.path.dirname(os.path.realpath(__file__))+'/../')

parser=argparse.ArgumentParser()
parser.add_argument('start_date')
parser.add_argument('end_date', nargs='?')
parser.add_argument('-nrt', action="store_true")
parser.add_argument('-sat', default="metopa,noaa16,noaa17,noaa18,noaa19")
parser.add_argument('-lvl', choices=['l2p'], default='l2p')
args=parser.parse_args()
if args.end_date == None:
    args.end_date = args.start_date
args.sat = args.sat.split(',')

args.start_date = dt.datetime.strptime(args.start_date, "%Y%m%d").date()
args.end_date = dt.datetime.strptime(args.end_date, "%Y%m%d").date()
args.ftpsite="ftp.star.nesdis.noaa.gov"
args.ftpdir="/pub/socd2/coastwatch/sst/{}/avhrr_gac".format("nrt" if args.nrt else "ran")
args.obsdir=rootdir+'/DATA/obs/sst_acspo_avhrr/raw'
print(args)

#------------------------------------------------------------
ftp=ftplib.FTP(args.ftpsite)
ftp.login()

# for each satellite get a directory listing of the days available
satDates={}
for s in args.sat:
    satDates[s]=[]
    print("Reading available dates for "+s)
    ftp.cwd(args.ftpdir+'/{}/{}'.format(s,args.lvl))
    years = ftp.nlst()
    years = [y for y in years if  args.start_date.year <= int(y) <= args.end_date.year]

    for y in years:        
        ftp.cwd(args.ftpdir+'/{}/{}/{}'.format(s,args.lvl,y))
        days = ftp.nlst()
        satDates[s]+=[y+d for d in days]

# for each date we want to download
cdate = args.start_date
while cdate <= args.end_date:
    print("")
    print("downloading "+str(cdate))
    daynum=(cdate-dt.date(cdate.year, 1,1)).days + 1
    outdir = args.obsdir+cdate.strftime("/%Y/%Y%m%d")
    if not os.path.exists(outdir):
        os.makedirs(outdir)
    yrday="{}{:03d}".format(cdate.year, daynum)
    for s in args.sat:
        if yrday not in satDates[s]:
            continue
        filedir=args.ftpdir+'/{}/{}/{}/{:03d}'.format(s,args.lvl,cdate.year,daynum)
        ftp.cwd(filedir)
        files=ftp.nlst()
        for f in sorted(files):
            fname=f[:10]+'.'+s+'.nc'
            print(fname)
            try:
                with open(outdir+'/'+fname, 'wb') as outfile:
                    ftp.retrbinary('RETR '+f,outfile.write)
                    outfile.close()
            except:
                print('*** ERROR: unable to download ***************************')
                if os.path.exists(outdir+'/'+fname):
                    os.remove(outdir+'/'+fname)

    cdate += dt.timedelta(days=1)
