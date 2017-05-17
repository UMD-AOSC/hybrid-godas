cmake_minimum_required(VERSION 2.8 FATAL_ERROR)

project(obsop Fortran)
include_directories($ENV{NETCDF_INCS})


set( EXE obsop_sst)
project( ${EXE} Fortran)
set(SRCS obsop_sst.f90 kdtree.f90 read_avhrr_pathfinder.f90 obsio_nc.f90)
add_executable( ${EXE} ${SRCS})
target_link_libraries( ${EXE} $ENV{NETCDF_LIBS})
