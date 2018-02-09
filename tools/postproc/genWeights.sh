#!/bin/bash
set -e

export OMP_NUM_THREADS=1
run="aprun -n 12 ESMF_RegridWeightGen -i --norm_type fracarea"
d=weights

mkdir -p $d
for res in 025 05 1; do
    $run -s grid/grid_t.nc -d grid/grid_latlon_$res.nc -m conserve -w $d/remap.grid_t.${res}deg.con.nc
    $run -s grid/grid_t.nc -d grid/grid_latlon_$res.nc -m bilinear -w $d/remap.grid_t.${res}deg.bil.nc -p teeth
    $run -s grid/grid_t.nc -d grid/grid_latlon_$res.nc -m patch    -w $d/remap.grid_t.${res}deg.pat.nc -p teeth
#3    $run -s grid/grid_u.nc -d grid/grid_latlon_$res.nc -m bilinear -w weights/remap.grid_u.${res}deg.bil.nc
#    $run -s grid/grid_v.nc -d grid/grid_latlon_$res.nc -m bilinear -w weights/remap.grid_v.${res}deg.bil.nc

done

rm *.Log
