#!/usr/bin/env python3
import omf
import os
from multiprocessing import Pool
from glob import glob
import math
import numpy as np

def processExp(f):
    """process a single date of an experiment"""
    omfs, mem =omf.read(f)

    # a correction to the RMSE of standard deviations based on ensemble size
    c=math.sqrt((mem+1.0)/mem)

    # mask the omfs by region / qc / obs type
    masks={}
    for m in omf.masks:
        masks[m] = omf.masks[m](omfs)

    # mask the omfs by depth
    # TODO

    # calculate the statistics we want to save for this date
    m_valid = masks['q_valid']
    allData=[]
    for p in plotTypes:
        data={}

        # calculate the overall mask for this plot type
        mask = None
        for m in p['masks']:
            mask = masks[m] if mask is None else (mask & masks[m])            
        obs = omfs[ m_valid & mask ]

        data['date'] = f.split('/')[-1][:-3]
        data['cnt_good'] = (m_valid & mask).sum()
        data['cnt_bad']  = mask.sum() - data['cnt_good']
        data['bias'] = obs.inc_mean.mean()
        data['rmsd'] = math.sqrt((obs.inc_mean**2).mean())
#        data['err']  = np.sqrt(np.mean(c*obs.err.values**2))
        data['err']  = obs.err.mean()
        data['sprd'] = np.sqrt(np.mean(c*obs.inc_sprd.values**2))
        
        allData.append(data)
    return allData



if __name__ == "__main__":
    import argparse
    import matplotlib
    matplotlib.use('agg')
    import matplotlib.pyplot as plt
    import matplotlib.dates as mdates
    import datetime
    
    # read in command line arguments
    parser = argparse.ArgumentParser(description=(
        "Process ensemble Observation minus Forecast (OmF) statistics "
        "as regionally binned timeseries"))
    parser.add_argument('path', nargs="+", help=(
        "path to one or more experiment directories"))
    parser.add_argument('-start', default="00000000", help=(
        "start date in YYYYMMDD format"))
    parser.add_argument('-end', default="99999999", help=(
        "end date in YYYYMMDD format"))
    parser.add_argument('-threads', type=int, default=4, help=(
        "number of threads to use when reading input files. (Default: %(default)s)"))
    parser.add_argument('-out', default="./omf_timeseries", help=(
        "output directory for generated plots (Default: %(default)s)"))
    args = parser.parse_args()

    if not os.path.exists(args.out):
        os.makedirs(args.out)

    # configure the plot types
    plotTypes=[]
    for r in omf.region_list:
        plotTypes.append( {
            'title' : 'SST ('+r[1]+')',
            'masks' : ('o_sst',) + r[2],
            'fn'    : 'sst/#p#/#p#_sst_'+r[0]})

        for v in ( ('insitu-temp', 'insitu T', 'o_temp'),
                   ('insitu-salt', 'insitu S', 'o_salt')):
            plotTypes.append( {
                'title' : v[1] + ' ('+r[1]+')',
                'masks' : (v[2],) + r[2],
                'fn'    : '{0}/#p#/#p#_{0}_{1}'.format(v[0],r[0]) })


    # for each experiment:
    allData={}
    for exp in args.path:
        # get list of files, keeping only those within the date range
        files = glob(exp + '/output/omf/????/*.nc')
        files = sorted([f for f in files if args.start <= f.split('/')[-1][:8] <= args.end])

        # do a parallel processing of the dates
        pool = Pool(args.threads)
        data = pool.map(processExp, files)
        pool.close()
        pool.join()
        allData[exp] = data


    # plot the plots
    major = mdates.YearLocator()
    majorFmt = mdates.DateFormatter('%Y')
    minor = mdates.MonthLocator()
    cnt=-1
    for p in plotTypes:
        cnt+=1
        for p2 in ('bias', 'rmsd'):
            filename=args.out+'/'+p['fn'].replace('#p#',p2)+'.png'
            dirname=os.path.dirname(filename)
            if not os.path.exists(dirname):
                os.makedirs(dirname)
            print( filename)

            plt.figure(figsize=(9, 4.5))
            plt.title('{} {}'.format(p2, p['title']))
            plt.gca().xaxis.set_major_locator(major)
            plt.gca().xaxis.set_major_formatter(majorFmt)
            plt.gca().xaxis.set_minor_locator(minor)
            plt.gca().grid(True)

            enum=-1
            for e in args.path:
                elbl=e.split('/')[-1]
                enum+=1
                xval = [ datetime.datetime.strptime(x[cnt]['date'],'%Y%m%d%H').date() for x in allData[e] ]
                yval = [ x[cnt][p2]     for x in allData[e] ]
                y2val= [ x[cnt]['sprd']  for x in allData[e] ]
                y3val= [ x[cnt]['err']  for x in allData[e] ]
                plt.plot(xval, yval,  'C{}'.format(enum), label=elbl)
                plt.plot(xval, y2val, 'C{}'.format(enum), ls='--')
                if enum == 0:
                    plt.plot(xval, y3val, 'gray', ls=':')
            plt.axhline(y=0.0, color='black')

            plt.legend()
            plt.savefig(filename)
            plt.close('all')
        
        

        

