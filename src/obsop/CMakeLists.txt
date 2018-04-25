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

# determine the repository version and compile time
execute_process(COMMAND git describe --dirty --tags --always OUTPUT_VARIABLE GIT_VERSION RESULT_VARIABLE RES OUTPUT_STRIP_TRAILING_WHITESPACE)
if(NOT ${RES} EQUAL "0")
  set(GIT_VERSION "Unknown")
endif()
add_definitions(-DCVERSION=\"${GIT_VERSION}\")
string(TIMESTAMP TS %Y-%m-%d )
add_definitions(-DCTIME=\"${TS}\")


# find required datetime library for fortran
include_directories($ENV{NETCDF_INCS})
include_directories("../../build/datetime/include")
find_library(DATETIMELIB NAMES datetime PATHS ../../build/datetime/lib)


# obsop common objects (including GSW toolbox)
file(GLOB GSW_TB_SRC "../gsw/toolbox/*.[f,F]90")
file(GLOB GSW_MD_SRC "../gsw/modules/*.[f,F]90")
file(GLOB KDTREE_SRC "../geoKdTree/*.[f,F]90")
set(SRCS obscom_obsio.f90 obscom_grid.f90 cubic_spline.f90 ${GSW_TB_SRC} ${GSW_MD_SRC} ${KDTREE_SRC})
add_library( obsop_common STATIC ${SRCS} )
set_target_properties(obsop_common PROPERTIES
    COMPILE_FLAGS "$ENV{OBSOP_COMPILE_FLAGS}")		      


# obs prep - insitu
file(GLOB SRCS "obsprep_insitu.[f,F]90")
set(EXE  obsprep_insitu)
add_executable(${EXE} ${SRCS})
target_link_libraries(${EXE} obsop_common $ENV{NETCDF_LIBS} ${DATETIMELIB})
set_target_properties(${EXE} PROPERTIES
    COMPILE_FLAGS "$ENV{OBSOP_COMPILE_FLAGS}")		      


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
set_target_properties(${EXE} PROPERTIES
    COMPILE_FLAGS "$ENV{OBSOP_COMPILE_FLAGS}")		      


# obsprep - combine
set(SRCS obsprep_combine.F90)
set(EXE  obsprep_combine)
add_executable(${EXE} ${SRCS})
target_link_libraries(${EXE} obsop_common $ENV{NETCDF_LIBS} ${DATETIMELIB})
set_target_properties(${EXE} PROPERTIES
    COMPILE_FLAGS "$ENV{OBSOP_COMPILE_FLAGS}")		      


# obsop
set(SRCS obsop.F90)
set(EXE  obsop)
add_executable(${EXE} ${SRCS})
target_link_libraries(${EXE} obsop_common $ENV{NETCDF_LIBS} ${DATETIMELIB})
set_target_properties(${EXE} PROPERTIES
    COMPILE_FLAGS "$ENV{OBSOP_COMPILE_FLAGS}")		      


# obs2dat
set(SRCS obsprep_nc2dat.F90)
set(EXE obsprep_nc2dat)
add_executable(${EXE} ${SRCS})
target_link_libraries(${EXE} obsop_common $ENV{NETCDF_LIBS} ${DATETIMELIB})
set_target_properties(${EXE} PROPERTIES
    COMPILE_FLAGS "$ENV{OBSOP_COMPILE_FLAGS}")
