import numpy as np
import pandas as pd


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
    """Returns a mask indicating which observation points (d) are within the given basin"""
    mask = None
    for box in basin_bounds[basin]:
        m2 = (d.lon <= box[0]) & (d.lon > box[1]) & \
             (d.lat >= box[2]) & (d.lat < box[3])
        mask = m2 if mask is None else (mask | m2)
    return mask



masks = {
    # qc
#    'q_valid'  : lambda d: (d.qc == 0),
    'q_valid'  : lambda d: (d.qc == 0) & (d.err<1e10) & (d.inc_mean.abs() < 1e10),

    # obs types
    'o_sst'  : lambda d: (d.obid == 2210) & (d.plat == 1000),
    'o_temp' : lambda d: ((d.obid == 2211) | (d.obid == 2210)) & (d.plat == 1),
    'o_salt' : lambda d: (d.obid == 2220)  & (d.plat == 1),

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
    'd_d1'   : lambda d: (d.depth >= 1) & (d.depth <= 50),
    'd_d2'   : lambda d: (d.depth > 50) & (d.depth <= 200),
    'd_d3'   : lambda d: (d.depth > 200) & (d.depth <= 750),
}


region_list = (('global',  'GL 60S-60N',     ('r_gl',) ),
               ('sh',      'SH 60S-20S',     ('r_sh',) ),
               ('tp',      'TP 20S-20N',     ('r_tp',) ),
               ('nh',      'NH 20N-60N',     ('r_nh',) ),
#               ('atl_sh',  'Atlatinc 60S-20S', ('r_sh','b_at') ),
#               ('pac_sh',  'Pacific 60S-20S',  ('r_sh','b_pa') ),
#               ('ind_sh',  'Indian 60S-20S',   ('r_sh','b_in') ),                                      
               ('atl_tp',  'Atlatic TP 20S-20N',  ('r_tp','b_at') ),
               ('pac_tp',  'Pacific TP 20S-20N',  ('r_tp','b_pa') ),
               ('ind_tp',  'Indian TP 20S-20N',   ('r_tp','b_in') ),
               ('atl_nh',  'Atlantic NH 20N-60N', ('r_nh','b_at') ),
               ('pac_nh',  'Pacific NH 20N-60N',  ('r_nh','b_pa') ),
               ('nino4',   'Niño4',    ('r_nino4',)  ),
               ('nino34',  'Niño3.4',  ('r_nino34',) ),
               ('nino3',   'Niño3',    ('r_nino3',)  ),
               ('nino12',  'Niño1+2',  ('r_nino12',) ))


def read(filename):
    import netCDF4 as nc

    # load the file, afterward "df" is the pandas dataframe
    print (filename)
    ncd=nc.Dataset(filename,'r')
    df={}
    fvars=('obid','plat','lat','lon','depth','qc', 'val','err')
    for f in fvars:
        df[f] = ncd.variables[f][:]

    mem =ncd.dimensions['mem'].size

    if "inc_mean" in ncd.variables.keys():
        df['inc_mean'] = ncd.variables['inc_mean'][:]
    else:
        df['inc_mean'] = ncd.variables['inc'][0]

    if "inc_sprd" in ncd.variables.keys():
        df['inc_sprd'] = ncd.variables['inc_sprd'][:]
    else:
        df['inc_sprd'] = np.zeros(df['inc_mean'].shape)    
    ncd.close()

    #todo, remove this
    df['qc'][df['inc_mean'] != df['inc_mean']] = 1

    df['lon'][df['lon'] < -180] += 360
    df['lon'][df['lon'] >  180] -= 360
    df = pd.DataFrame(df)
    return df, mem


if __name__=="__main__":
    raise(Exception("ERROR: this python file is not to be run directly."))
