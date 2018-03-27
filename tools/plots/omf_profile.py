#!/usr/bin/env python3
import omf
from multiprocessing import Pool
import os
import numpy as np
import bisect
import math

#  TODO, fit a curve to the raw data, don't do binning
lvls=np.concatenate( (np.arange(0.0,100.0,5),np.arange(100.0,500.0,25),np.arange(500.0,2001.0,50)) )


def processExp(e):
    data=[]
    for d in plotTypes:
        d2={
            "count"     : np.zeros(lvls.shape),
            "inc_mean"  : np.zeros(lvls.shape),
            "inc_mean2" : np.zeros(lvls.shape),
            "inc_sprd"  : np.zeros(lvls.shape),
            "val"       : np.zeros(lvls.shape)}
        data.append(d2)


    for f in e:
        omfs,mem = omf.read(f)
        
        # a correction to the RMSE for standard deviation
        c=(mem+1.0)/mem

        # mask the omfs by region / qc / obs type
        masks={}
        for m in omf.masks:
            masks[m] = omf.masks[m](omfs)

        m_valid = masks['q_valid']
        m_depth = omfs.depth <= args.depth
        cnt=-1
        for p in plotTypes:
            cnt+=1

            mask=None
            for m in p['masks']:
                mask = masks[m] if mask is None else (mask & masks[m])
            obs = omfs[ m_valid & mask & m_depth]

            for d,v,e,i_m,i_s in zip(
                    obs.depth.values, obs.val.values,
                    obs.err.values, obs.inc_mean.values, obs.inc_sprd.values):
                idx=bisect.bisect_left(lvls, d)

                count = data[cnt]['count'][idx] +1
                data[cnt]['count'][idx] = count
                data[cnt]['inc_mean'][idx]  += (i_m    - data[cnt]['inc_mean'][idx])/count
                data[cnt]['inc_mean2'][idx] += (i_m**2 - data[cnt]['inc_mean2'][idx])/count
                data[cnt]['inc_sprd'][idx]  += (c*i_s**2 - data[cnt]['inc_sprd'][idx])/count

                data[cnt]['val'][idx] += (v - data[cnt]['val'][idx])/count

    return data
 


if __name__=="__main__":
    import argparse
    from glob import glob
    import matplotlib
    matplotlib.use('agg')
    import matplotlib.pyplot as plt

    # read in command line arguments
    parser = argparse.ArgumentParser(description=(
        "Process ensemble Observation minus Forecast (OmF) statistics "
        "as regionally binned profiles"))
    parser.add_argument('path', nargs="+", help=(
        "path to one or more experiment directories"))
    parser.add_argument('-depth', type=float, default='300', help=(
        "maximum plot depth (Default %(default)s)"))
    parser.add_argument('-start', default="00000000", help=(
        "start date in YYYYMMDD format"))
    parser.add_argument('-end', default="99999999", help=(
        "end date in YYYYMMDD format"))
    parser.add_argument('-out', default="./omf_profile", help=(
        "output directory for generated plots (Default: %(default)s)"))
    parser.add_argument('-threads', type=int, default=4, help=(
        "number of threads to use when reading input files. (Default: %(default)s)"))
    args = parser.parse_args()

    # configure the plot types
    plotTypes=[]
    for r in omf.region_list:
        for v in ( ('insitu-temp', 'insitu T', 'o_temp'),
                   ('insitu-salt', 'insitu S', 'o_salt')):
            plotTypes.append( {
                'title' : v[1] + ' ('+r[1]+')',
                'masks' : (v[2],) + r[2],
                'fn'    : '{0}/#p#/#p#_{0}_{1}'.format(v[0],r[0])})

    # for each experiment:
    efiles=[]
    for exp in args.path:
        # get list of files, keeping only those within the date range
        files = glob(exp + '/output/omf/????/*.nc')
        files = sorted([f for f in files if args.start <= f.split('/')[-1][:8] <= args.end])
        efiles.append(files)
                    
    pool = Pool(args.threads)
    allData=pool.map(processExp, efiles)
    pool.close()
    pool.join()


    # plot the plots
    cnt=-1
    for p in plotTypes:
        cnt+=1
        for p2 in ('bias','rmsd'):
            filename=args.out+'/'+p['fn'].replace('#p#',p2)+'.png'
            dirname=os.path.dirname(filename)
            if not os.path.exists(dirname):
                os.makedirs(dirname)
            print(filename)

            plt.figure(figsize=(4.5,9))
            plt.title('{} {}'.format(p2, p['title']))
            plt.gca().grid(True)
            plt.ylim(args.depth, 0)

            enum=-1
            for e in args.path:
                enum+=1
                data=allData[enum][cnt]
                if p2 == 'rmsd':
                    plt.plot(np.sqrt(data['inc_mean2']), lvls, 'C{}'.format(enum))
                    plt.plot(np.sqrt(data['inc_sprd']), lvls, 'C{}'.format(enum), ls='--')
                    plt.axvline(x=0.0, color='black')
                elif p2 == 'bias':
                    plt.plot(data['inc_mean'], lvls, 'C{}'.format(enum))
                    plt.axvline(x=0.0, color='black')
#                elif p2 =='val':
#                    plt.plot(data['val'], lvls, 'C{}'.format(enum))


            plt.annotate('profiles: {}'.format(int(np.max(allData[0][cnt]['count']))),
                         xy=(6,30), xycoords='figure points')

            plt.savefig(filename)
            plt.close('all')
