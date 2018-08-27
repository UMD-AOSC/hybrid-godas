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

    # mask the omfs by region / qc / obs type / depth/ etc..
    masks={}
    for m in omf.masks:
        masks[m] = omf.masks[m](omfs)

    # calculate the statistics we want to save for this date
    m_valid = masks['q_valid']
    allData=[]
    for p in plotTypes:
        data={}

        # calculate the overall mask for this plot type
        mask = None
        for m in p['masks']:
            mask = masks[m] if mask is None else (mask & masks[m])            
        m_hr = np.logical_and(omfs.hr >= args.hr[0], omfs.hr <= args.hr[1])
        obs = omfs[ m_valid & mask  & m_hr]

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


# TODO, this is inefficient, fix up
def smoothData(xs,ys,cs):
    ys0 = ys
    for i, x in enumerate(ys):
        i1 = max(0, i-args.smooth)
        i2 = min(len(xs), i+args.smooth+1)
        ys[i] = sum(np.multiply(ys0[i1:i2] ,cs[i1:i2])) / sum(cs[i1:i2])
    return ys


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
    parser.add_argument('-hr', nargs=2, type=int, default=(-9e10, 9e10))
    parser.add_argument('-label', help=(
        "A comma separated list of labels to use for the plot lines"))
    parser.add_argument('-smooth', type=int, default=0, help=(
        "number of steps in each direction to do a weighted smooth over (Default:%(default)s)"))
    args = parser.parse_args()        
    args.path = [os.path.abspath(p) for p in args.path]
    if args.label is not None:
        args.label = args.label.split(',')
    else:
        args.label = [p.split('/')[-1] for p in args.path]
    print(args)


    # make sure the output directory exists
    if not os.path.exists(args.out):
        os.makedirs(args.out)

    # configure the plot types
    plotTypes=[]
    for r in omf.region_list:
        # SST, by region
        plotTypes.append( {
            'title' : 'SST ('+r[1]+')',
            'masks' : ('o_sst',) + r[2],
            'fn'    : 'sst/#p#/#p#_sst_'+r[0]})

        # T/S by region, and by depth
        for v in ( ('insitu-temp',  'insitu T', 'o_temp'),
                   ('insitu-salt',  'insitu S', 'o_salt')):
            for d in ( ('1m_50m',   '1-50m',    'd_d1'),
                       ('50m_200m', '50-200m',  'd_d2'),
                       ('200m_750m','200-750m', 'd_d3')):
                plotTypes.append( {
                    'title' : v[1] + ' ('+r[1]+') '+ d[1],
                    'masks' : (v[2],d[2]) + r[2],
                    'fn'    : '{0}/#p#/#p#_{0}_{1}_{2}'.format(v[0],r[0],d[0]) })


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
    for cnt, p in enumerate(plotTypes):
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

            for i, e in enumerate(args.path):
                xval = [ datetime.datetime.strptime(x[cnt]['date'],'%Y%m%d%H').date() for
                         x in allData[e] ]
                vcount=[ x[cnt]['cnt_good'] for x in allData[e] ]
                yval = [ x[cnt][p2]     for x in allData[e] ]
                yval= smoothData(xval,yval, vcount)


                # plot the experiment rmsd or bias
                plt.plot(xval, yval,  'C{}'.format(i), label=args.label[i])

                if p2 == 'rmsd':
                    # plot the ensemble spread
                    y2val= [ x[cnt]['sprd']  for x in allData[e] ]
                    y2val = smoothData(xval,y2val, vcount)
                    plt.plot(xval, y2val, 'C{}'.format(i), ls='--')

                    # plot the avg obs err
#                    if i == 0:
#                        y3val= [ x[cnt]['err']  for x in allData[e] ]
#                        plt.plot(xval, y3val, 'gray', ls=':')
            plt.axhline(y=0.0, color='black')

            plt.legend()
            plt.savefig(filename)
            plt.close('all')

