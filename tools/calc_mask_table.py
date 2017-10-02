#!/usr/bin/env python3
import netCDF4 as nc
import numpy as np


print("nodes: ", end='')
nodes = int(input())

print('core/node: ',end='')
cpn   = int(input())  # cores per node


maskfile='../src/MOM6/ice_ocean_SIS2/OM4_025/INPUT/ocean_mask.nc'
ncd = nc.Dataset(maskfile,'r')
mask = ncd.variables['mask'][:].transpose()
ncd.close()

cores = nodes*cpn
nx,ny = mask.shape

# all the ways we could evenly split up the grid
gx = [ x for x in range(1,nx+1) if nx % x == 0]
gy = [ y for y in range(1,ny+1) if ny % y == 0]


layouts=[]

for x in gx:
    for y in gy:
        if x*y < cores or x*y > cores*2:
            continue
        good=0
        bad=0
        mask2 = np.split(mask, x, axis=0)
        for m2 in mask2:
            mask3 = np.split(m2, y, axis=1)
            for m3 in mask3:
                if m3.any():
                    good += 1
                else:
                    bad += 1
        if good > cores:
            continue

        saved=(bad-cores+good)*x*y
        layouts.append( (saved, good, bad, x, y) )
        
layouts = sorted(layouts, key=lambda x: -x[0])
layouts = [l for l in layouts if l[0] == layouts[0][0]]
cnt = 0
for l in layouts:
    cnt += 1
    masked=l[2]-cores+l[1]
    print("{}) layout: {}x{}   patches masked: {}  gridpoints masked: {: 7d}".format(cnt, l[3], l[4], masked,l[0]))

print("select which mask table to generate: ",end='')
choice = int(input())

layout=layouts[choice-1]
masked=layout[2]-cores+layout[1]

fout=open('mask_table.{}.{}x{}'.format(masked, layout[3], layout[4]),'w')
cnt = 0
mask2 = np.split(mask, layout[4], axis=1)
print(str(masked), file=fout)
print('{},{}'.format(layout[3],layout[4]), file=fout)
for y in range(len(mask2)):
    mask3 = np.split(mask2[y], layout[3], axis=0)
    for x in range(len(mask3)):
        if not mask3[x].any():
            cnt += 1
            if cnt <= masked:
                print('{},{}'.format(x+1,y+1), file=fout)

