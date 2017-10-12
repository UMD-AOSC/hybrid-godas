cmake_minimum_required(VERSION 2.8 FATAL_ERROR)

project(obsop Fortran)

# global compiler options
#------------------------------------------------------------
if(NOT CMAKE_BUILD_TYPE)
  set(CMAKE_BUILD_TYPE Debug CACHE STRING
    "Choose the type of build, options are Debug Release" FORCE)
endif()
if(CMAKE_BUILD_TYPE MATCHES "Debug")
  add_definitions(-Wall)
  add_definitions(-fbacktrace)
  add_definitions(-fcheck=all)
  add_definitions(-g)
else()
  add_definitions(-O3)
endif()

include_directories($ENV{NETCDF_INCS})
include_directories("../gsw/build")
include_directories("../datetime/build/include")
find_library(GSWLIB NAMES gsw_toolbox PATHS ../gsw/build)
find_library(DATETIMELIB NAMES datetime PATHS ../datetime/build/lib)

# common objects
set(SRCS obscom_obsio.f90 obscom_grid.f90 kdtree.f90 cubic_spline.f90)
# linked_list.f90
add_library( obsop_common STATIC ${SRCS} )

# obs prep - insitu
file(GLOB SRCS "obsprep_insitu.[f,F]90")
set(EXE  obsprep_insitu)
add_executable(${EXE} ${SRCS})
target_link_libraries(${EXE} obsop_common $ENV{NETCDF_LIBS} ${DATETIMELIB})

# # obs prep - legacy insitu
# file(GLOB SRCS "obsprep_insitu_legacy.[f,F]90")
# set(SRCS ${SRCS} compute_profile_error.f90 linked_list.f90)
# set(EXE  obsprep_insitu_legacy)
# add_executable(${EXE} ${SRCS})
# target_link_libraries(${EXE} obsop_common $ENV{NETCDF_LIBS} ${DATETIMELIB})

# obs prep - sst
file(GLOB SRCS "obsprep_sst*.[f,F]90")
set(EXE  obsprep_sst)
add_executable(${EXE} ${SRCS})
target_link_libraries(${EXE} obsop_common $ENV{NETCDF_LIBS} ${DATETIMELIB})

# obsprep - combine
set(SRCS obsprep_combine.F90)
set(EXE  obsprep_combine)
add_executable(${EXE} ${SRCS})
target_link_libraries(${EXE} obsop_common $ENV{NETCDF_LIBS} ${DATETIMELIB})

# obsop
set(SRCS obsop.f90)
set(EXE  obsop)
add_executable(${EXE} ${SRCS})
target_link_libraries(${EXE} obsop_common $ENV{NETCDF_LIBS} ${GSWLIB} ${DATETIMELIB})

