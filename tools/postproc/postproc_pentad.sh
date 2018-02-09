#!/bin/bash
set -eu

module load cdo
module load ncl

infile=../../../test_esmf_regrid/old/input/ocn.nc
ice_infile=../../../test_esmf_regrid/old/input/ice.nc

TMPDIR=./WRK
res=025

uvars=u,taux
vvars=v,tauy
tvars3d=temp,salt
ocn_remove=net_massin,net_massout,Th_tendency_2d,Sh_tendency_2d,hfrainds,\
latent_fprec_diag,net_heat_surface,heat_content_surfwater,lrunoff,\
umo_2d,T_adx_2d,S_adx_2d,\
vmo_2d,T_ady_2d,S_ady_2d,\
sw,lw,lh,sh,evap,precip,lprec
ice_remove=saltf,runoff

ice_uvars=siu
ice_vvars=siv

packing=\
temp/0/0.01,\
salt/0/0.01,\
u/0/0.001,\
v/0/0.001,\
\
taux/0/0.01,\
tauy/0/0.01,\
speed/0/0.01,\
ssh/0/0.001,\
\
lh/0/0.1,\
lw/0/0.1,\
sh/0/0.1,\
sw/0/0.1,\
net_heat_coupler/0/0.1,\
mld_003/1000/0.1,\
mld_0125/3000/0.1,\
\
siu/0/0.001,\
siv/0/0.001,\
sisnconc/0/0.01,\
siconc/0/0.01,\
hs/0/0.01,\
hi/0/0.01


mkdir -p $TMPDIR
export TMPDIR


# Ice U/V vars
#------------------------------------------------------------
echo ""
echo "Processing ICE U/V variables..."
./rotUV.py $ice_infile $TMPDIR/ice.uv.rot_mask.nc -u $ice_uvars -v $ice_vvars

infile=$TMPDIR/ice.uv.rot_mask.nc \
 outfile=$TMPDIR/ice.uv.remapped.nc \
 vars=$ice_uvars,$ice_vvars \
 weights=./weights/remap.grid_t.${res}deg.bil.nc \
 ncl -Q  remap.ncl
./compress.py $TMPDIR/ice.uv.remapped.nc $TMPDIR/ice.uv.packed.nc -compression 1 -pack $packing



# ICE other vars
#------------------------------------------------------------
echo ""
echo "Processing other ICE variables..."
./mask.py $ice_infile $TMPDIR/ice.t.masked.nc -ignore $ice_uvars,$ice_vvars,$ice_remove

vars=$(ncdump -h $TMPDIR/ice.t.masked.nc | grep "lat, lon")
IFS=';' readarray -t vars <<< "$vars"
t_vars=""
for v in "${vars[@]}"; do
    v=${v%%(*}
    v=${v##* }
    t_vars="$t_vars,$v"
done
t_vars=${t_vars:1}

infile=$TMPDIR/ice.t.masked.nc \
 outfile=$TMPDIR/ice.t.remapped.nc \
 vars=$t_vars \
 weights=./weights/remap.grid_t.${res}deg.con.nc \
 ncl -Q  remap.ncl
./compress.py $TMPDIR/ice.t.remapped.nc $TMPDIR/ice.t.packed.nc -compression 1 -pack $packing



# generate 3d land area mask (needed for correcting interpolation
# due to land masking for 3d variables using conservative interp)
#------------------------------------------------------------
echo ""
echo "Creating 3d land adjustment mask..."
cdo setrtoc,-500,500,1 -select,name=temp $infile $TMPDIR/ocn.ones.nc
./mask.py $TMPDIR/ocn.ones.nc $TMPDIR/ocn.mask.nc
infile=$TMPDIR/ocn.mask.nc \
 outfile=$TMPDIR/ocn.mask.remapped.nc \
 vars=temp\
 weights=./weights/remap.grid_t.${res}deg.con.nc \
 ncl -Q  remap.ncl



# process the 3d T vars
# (mask/remap/adjust_landmask)
#------------------------------------------------------------
echo ""
echo "Processing 3d T variables..."
./mask.py $infile $TMPDIR/ocn.t3d.masked.nc -only $tvars3d

infile=$TMPDIR/ocn.t3d.masked.nc \
 outfile=$TMPDIR/ocn.t3d.remapped.raw.nc \
 vars=$tvars3d \
 weights=./weights/remap.grid_t.${res}deg.con.nc \
 ncl -Q  remap.ncl

cdo div \
 $TMPDIR/ocn.t3d.remapped.raw.nc \
 $TMPDIR/ocn.mask.remapped.nc \
 $TMPDIR/ocn.t3d.remapped.nc

./compress.py $TMPDIR/ocn.t3d.remapped.nc $TMPDIR/ocn.t3d.packed.nc -compression 1 -pack $packing



# process the U/V vars
# (mask/destag/rotate/remap)
#------------------------------------------------------------
echo ""
echo "Processing U/V variables..."

./rotUV.py $infile $TMPDIR/ocn.uv.rot_mask.nc -u $uvars -v $vvars --mask 

infile=$TMPDIR/ocn.uv.rot_mask.nc \
 outfile=$TMPDIR/ocn.uv.remapped.nc \
 vars=$uvars,$vvars \
 weights=./weights/remap.grid_t.${res}deg.bil.nc \
 ncl -Q  remap.ncl

./compress.py $TMPDIR/ocn.uv.remapped.nc $TMPDIR/ocn.uv.packed.nc -compression 1 -pack $packing



# process other 2d/3d vars 
#(mask/rotate/remap)
#------------------------------------------------------------
echo ""
echo "Processing other 2d/3d T variables..."

./mask.py $infile $TMPDIR/ocn.t.masked.nc -ignore $uvars,$vvars,$tvars3d,$ocn_remove

vars=$(ncdump -h $TMPDIR/ocn.t.masked.nc | grep "lat, lon")
IFS=';' readarray -t vars <<< "$vars"
t_vars=""
for v in "${vars[@]}"; do
    v=${v%%(*}
    v=${v##* }
    t_vars="$t_vars,$v"
done
t_vars=${t_vars:1}

infile=$TMPDIR/ocn.t.masked.nc \
 outfile=$TMPDIR/ocn.t.remapped.nc \
 vars=$t_vars \
 weights=./weights/remap.grid_t.${res}deg.con.nc \
 ncl -Q  remap.ncl

./compress.py $TMPDIR/ocn.t.remapped.nc $TMPDIR/ocn.t.packed.nc -compression 1 -pack $packing



# Combine into one file
#------------------------------------------------------------
echo ""
echo "Combining..."

cdo -z zip6 merge $TMPDIR/*.packed.nc $TMPDIR/final.nc
