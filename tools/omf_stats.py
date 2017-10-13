#!/usr/bin/env python3
import netCDF4 as nc
import numpy as np
import pandas as pd
import datetime
import sys, os
import math

import matplotlib
matplotlib.use('agg')
import matplotlib.pyplot as plt
import matplotlib.dates as mdates
from multiprocessing import Pool

#--------------------------------------------------------------------------------
#--------------------------------------------------------------------------------
# a series of rectangles to define indian/atlantic/pacific boundaries
# format is (lon1, lon2, lat1, lat2)
basin_bounds={
    'GL' : ( ( 180, -180, -80, 90),),
    'PA' : ( ( -70,  -80, -80,  9),
             ( -80,  -84, -80,  9),
             ( -84,  -90, -80, 14),
             ( -90, -100, -80, 18),
             (-100, -180, -80, 66),
             ( 180,  145, -80, 66),             
             ( 145,  100,   0, 66)),
    'IN' : ( ( 100,   20, -80, 31),
             ( 145,  100, -80,  0)),
    'AT' : ( ( 100,   20,  31, 90),
             (  20,  -70, -80, 90),
             ( -70,  -84,   9, 90),
             ( -84,  -90,  14, 90),
             ( -90, -100,  18, 90),
             (-100, -180,  66, 90),
             ( 180,  100,  66, 90)) }
def basin_check(basin, d):
    mask = None
    for box in basin_bounds[basin]:
        m2 = (d.lon <= box[0]) & (d.lon > box[1]) & \
             (d.lat >= box[2]) & (d.lat < box[3])
        mask = m2 if mask is None else (mask | m2)
    return mask


masks = {
    # qc
    'q_valid'  : lambda d: (d.qc == 0) | (d.qc == 30),
    'q_good'   : lambda d: (d.qc == 0),

    # obs types
    'o_sst'  : lambda d: (d.obid == 2210) & (d.plat == 1000),
    'o_temp' : lambda d: ((d.obid == 2211) | (d.obid == 2210)) & (d.plat == 1),
    'o_sal'  : lambda d: (d.obid == 2220)  & (d.plat == 1),

    # by basin
    'b_pa' :  lambda d: basin_check('PA', d),
    'b_in' :  lambda d: basin_check('IN', d),
    'b_at' :  lambda d: basin_check('AT', d),

    # by other regions
    'r_nino3'  : lambda d: (d.lon <= -90)  & (d.lon >= -150)  & (d.lat >= -5)  & (d.lat <= 5),
    'r_nino34' : lambda d: (d.lon <= -120) & (d.lon >= -170)  & (d.lat >= -5)  & (d.lat <= 5),
    'r_nino4'  : lambda d: ((d.lon <= -150) | (d.lon >= 160)) & (d.lat >= -5)  & (d.lat <= 5),
    'r_nino12' : lambda d: (d.lon <= -80)  & (d.lon >= -90)   & (d.lat >= -10) & (d.lat <= 0),
    'r_gl'     : lambda d: (d.lat <= 60) & (d.lat >= -60),
    'r_nh'     : lambda d: (d.lat <= 60) & (d.lat >= 20),
    'r_sh'     : lambda d: (d.lat >= -60) & (d.lat <= -20),
    'r_tp'     : lambda d: (d.lat >= -20) & (d.lat <= 20),

    # depths
    # 'd_surf' : lambda d: (d.depth <= 1),
    'd_d1'   : lambda d: (d.depth <= 5),
    'd_d2'   : lambda d: (d.depth > 5) & (d.depth <= 50),
    'd_d3'   : lambda d: (d.depth > 50) & (d.depth <= 300),
}


#--------------------------------------------------------------------------------
#--------------------------------------------------------------------------------


region_list = (('global',  '60S-60N',     ('r_gl',) ),
               ('sh',      '60S-20S',     ('r_sh',) ),
               ('tp',      '20S-20N',     ('r_tp',) ),
               ('nh',      '20N-60N',     ('r_nh',) ),

#               ('atl_sh',  'Atlatinc 60S-20S', ('r_sh','b_at') ),
#               ('pac_sh',  'Pacific 60S-20S',  ('r_sh','b_pa') ),
#               ('ind_sh',  'Indian 60S-20S',   ('r_sh','b_in') ),
               ('atl_tp',  'Atlatic 20S-20N',  ('r_tp','b_at') ),
               ('pac_tp',  'Pacific 20S-20N',  ('r_tp','b_pa') ),
               ('ind_tp',  'Indian 20S-20N',   ('r_tp','b_in') ),
               ('atl_nh',  'Atlantic 20N-60N', ('r_nh','b_at') ),
               ('pac_nh',  'Pacific 20N-60N',  ('r_nh','b_pa') ),
               ('nino4',   'Niño4',    ('r_nino4',)  ),
               ('nino34',  'Niño3.4',  ('r_nino34',) ),
               ('nino3',   'Niño3',    ('r_nino3',)  ),
               ('nino12',  'Niño1+2',  ('r_nino12',) ))

depth_list    = (
                 # ('surf',    '0m-1m',   ('d_surf',) ),
                 ('0-5m',    '0m-5m',   ('d_d1',)   ),
                 ('5-50m',   '5m-50m',  ('d_d2',)   ),
                 ('50-300m', '50m-300m',('d_d3',)   ))


#------------------------------------------------------------
def configPlots():
    plots = []
    
    # SST
    #------------------------------------------------------------
    for r in region_list:
        plots.append( {
            'pfx'   : 'sst_'+r[0],
            'title' : 'SST ('+r[1]+')',
            'fn'    : ('o_sst',) + r[2]})
        
    # Temperature and salinity
    #------------------------------------------------------------
    for v in ( ('insitu-temp', 'insitu T', 'o_temp'), 
               ('insitu-sal',  'insitu S', 'o_sal') ):
        for r in region_list:
            for d in depth_list:
                plots.append( {
                    'pfx'   : v[0]+'_'+r[0]+'_'+d[0],
                    'title' : v[1] +' ('+r[1]+', '+d[1]+')',
                    'fn'    : (v[2],) + r[2] + d[2]})   
    return plots


#--------------------------------------------------------------------------------
#--------------------------------------------------------------------------------
def procFile(filename):
    plots = [p['fn'] for p in configPlots()]

    # load the file, afterward "df" is the pandas dataframe    
    print(filename)
    df={}
    ncd = nc.Dataset(filename, 'r')
    fvars=('obid','plat','lat','lon','depth','inc')
    for f in fvars: #ncd.variables:
        df[f] = ncd.variables[f][:]
    ncd.close()
    ncd = nc.Dataset(filename[:-3]+'.varqc.nc', 'r')
    fvars = ('qc',)
    for f in fvars:
        df[f] = ncd.variables[f][:]
    ncd.close()
    df['lon'][df['lon'] < -180] += 360
    df['lon'][df['lon'] >  180] -= 360
    df = pd.DataFrame(df)

    # calculate all the masks
    dmasks={}
    for m in masks:
        dmasks[m] = masks[m](df)

    data=[]
    for p in plots:
        m_valid = dmasks['q_valid']
        m_good  = dmasks['q_good']

        mask = None
        for m in p:
            mask = dmasks[m] if mask is None else (mask & dmasks[m])
            
        count_good = (m_good & mask).sum()
        count_bad  = mask.sum()- count_good
        obs  = df[ m_valid & mask]
        bias = obs.inc.mean()
        rmsd = math.sqrt((obs.inc**2).mean())
#        val  = obs.val.mean()
        data.append( (count_good, count_bad, bias, rmsd ) )#, val) )

    return (filename.split('/')[-1][:-3], data)

def smooth(d):
    if len(d) > 0:
        d = np.concatenate( (np.repeat(d[0], box_adj), d, np.repeat(d[-1], box_adj) ) )
        d = np.convolve(d, box, mode='valid')
    return d    



#--------------------------------------------------------------------------------
#--------------------------------------------------------------------------------
if __name__ == "__main__":
    import argparse
    from glob import glob

    parser = argparse.ArgumentParser(description=(
        "process 3dvar individual observation O-F stats into "
        "regional / dpeth  based stats"))
    parser.add_argument('path', nargs="+", help=(
        "path to one or more experiment directories"))
    parser.add_argument('--start', help=(
        "start date in YYYYMMDD format"))
    parser.add_argument('--end', help=(
        "end date in YYYYMMDD format"))
    parser.add_argument('--smooth', type=int, default=3)
    parser.add_argument('--threads', type=int, default=4)
    parser.add_argument('--out', default='./omf', help=(
        "Output directory"))
    args = parser.parse_args()
    if args.start is None:
        args.start = "00000000"
    if args.end is None:        
        args.end = "99999999"

    if not os.path.exists(args.out):
        os.makedirs(args.out)

    # get list of files for each experiment
    expfiles={}
    num=0
    for exp in args.path:
        num += 1
        files = glob(exp + '/output/bkg_omf/????/????????.nc')
        files = sorted([f for f in files if args.start <= f.split('/')[-1][:8] <= args.end])
        expfiles['exp{}'.format(num)] = files


    # get ist of the parameters that need to be calculated from configPlots
    plots = configPlots()


    # process each file
    pool = Pool(args.threads)
    data={} #[exp name] [ (date, [plot1, plot2...]), ....]
    for exp in expfiles:
        data[exp] = pool.map(procFile, expfiles[exp])
    pool.close()
    pool.join()

    # make the plots
    major = mdates.YearLocator()
    majorFmt = mdates.DateFormatter('%Y')
    minor = mdates.MonthLocator()
    box = np.ones(args.smooth)/args.smooth
    box_adj = (args.smooth-1)/2

    cnt = -1
    print('plotting...')
    for p in plots:
        cnt += 1
        expd = {}
        for exp in expfiles:
            e2={
                'date' : [],
                'bias' : [],
                'rmsd' : [],
                'val'  : [],
                'count': []    }
            d = data[exp]            
            for d2 in d:
                e2['date'].append( datetime.datetime.strptime(d2[0],"%Y%m%d").date() )
                e2['bias'].append(d2[1][cnt][2])
                e2['rmsd'].append(d2[1][cnt][3])
                e2['count'].append(d2[1][cnt][0])
#                e2['val'].append(d2[1][cnt][4])
            expd[exp]= e2

#         for e in expd:
#             d = expd[e]['val']
# #            d = np.concatenate( (np.repeat(d[0], box_adj), d, np.repeat(d[-1], box_adj) ) )
# #            d = np.convolve(d, box, mode='valid')
#             plt.plot(expd[e]['date'], d)
#         plt.title(p['title']+' value')
# #        plt.gca().axhline(y=0.0, color='black')
#         plt.gca().xaxis.set_major_locator(major)
#         plt.gca().xaxis.set_major_formatter(majorFmt)
#         plt.gca().xaxis.set_minor_locator(minor)
#         plt.savefig('img{:03d}_v.png'.format(cnt))
#         plt.close('all')

        plt.figure(figsize=(9,4.5))
        for e in expd:
            plt.plot(expd[e]['date'], smooth(expd[e]['bias']))
        plt.title('bias '+p['title'])
        plt.gca().axhline(y=0.0, color='black')
        plt.gca().xaxis.set_major_locator(major)
        plt.gca().xaxis.set_major_formatter(majorFmt)
        plt.gca().xaxis.set_minor_locator(minor)
        plt.gca().grid(True)
        plt.savefig(args.out+'/bias_{}.png'.format(p['pfx']))
        plt.close('all')

        plt.figure(figsize=(9,4.5))
        for e in expd:
            plt.plot(expd[e]['date'], smooth(expd[e]['rmsd']))
        plt.title('rmsd '+p['title'])
        plt.gca().axhline(y=0.0, color='black')
        plt.gca().xaxis.set_major_locator(major)
        plt.gca().xaxis.set_major_formatter(majorFmt)
        plt.gca().xaxis.set_minor_locator(minor)
        plt.gca().grid(True)
        plt.savefig(args.out+'/rmsd_{}.png'.format(p['pfx']))
        plt.close('all')

        plt.figure(figsize=(9,4.5))
        for e in expd:
            plt.plot(expd[e]['date'], smooth(expd[e]['count']))
#            plt.plot(expd[e]['date'], expd[e]['count'])
        plt.title('count '+p['title'])
        plt.gca().xaxis.set_major_locator(major)
        plt.gca().xaxis.set_major_formatter(majorFmt)
        plt.gca().xaxis.set_minor_locator(minor)
        plt.gca().grid(True)
        plt.savefig(args.out+'/count_{}.png'.format(p['pfx']))
        plt.close('all')

