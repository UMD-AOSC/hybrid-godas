cmake_minimum_required(VERSION 2.8 FATAL_ERROR)

project(utilities Fortran C)

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
find_library(GSWLIB NAMES gsw_toolbox PATHS ../gsw/build)
FIND_PACKAGE( OpenMP REQUIRED)


# common objects
set(SRCS ../geoKdTree/kdtree.f90 datatable.f90 hzsmooth.f90)
add_library( util_common STATIC ${SRCS} )
set_target_properties(util_common PROPERTIES 
    COMPILE_FLAGS "${OpenMP_C_FLAGS} $ENV{UTIL_COMPILE_FLAGS}")


# bgerr var
file(GLOB SRCS "bgvar*.[f,F]90")
set(EXE  bgvar)
add_executable(${EXE} ${SRCS})
target_link_libraries(${EXE} util_common $ENV{NETCDF_LIBS})
set_target_properties(${EXE} PROPERTIES 
    COMPILE_FLAGS "${OpenMP_C_FLAGS} $ENV{UTIL_COMPILE_FLAGS}"
    LINK_FLAGS "${OpenMP_C_FLAGS}")


# vt loc
file(GLOB SRCS "vtloc*.[f,F]90")
set(EXE vtloc)
add_executable(${EXE} ${SRCS})
target_link_libraries(${EXE} util_common)
target_link_libraries(${EXE} $ENV{NETCDF_LIBS})
set_target_properties(${EXE} PROPERTIES 
    COMPILE_FLAGS "${OpenMP_C_FLAGS} $ENV{UTIL_COMPILE_FLAGS}"
    LINK_FLAGS "${OpenMP_C_FLAGS}")


# update_restart
set(EXE update_restart)
add_executable(${EXE} update_restart.f90)
target_link_libraries(${EXE} util_common $ENV{NETCDF_LIBS})
set_target_properties(${EXE} PROPERTIES
    COMPILE_FLAGS "$ENV{UTIL_COMPILE_FLAGS}")			    


set(EXE mppnccombine)
add_executable(${EXE} mppnccombine.c)
target_link_libraries(${EXE} $ENV{NETCDF_LIBS})
set_target_properties(${EXE} PROPERTIES
    COMPILE_FLAGS "$ENV{UTIL_COMPILE_FLAGS}")			    
