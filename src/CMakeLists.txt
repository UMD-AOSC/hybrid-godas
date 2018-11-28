cmake_minimum_required(VERSION 3.0 FATAL_ERROR)
project(hybridgodas Fortran)


add_subdirectory(obsop)
add_subdirectory(letkf)
add_subdirectory(util)
add_subdirectory(3dvar/src)
