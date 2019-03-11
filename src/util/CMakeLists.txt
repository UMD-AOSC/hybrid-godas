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


# determine the repository version and time
execute_process(COMMAND git describe --dirty --tags --always OUTPUT_VARIABLE GIT_VERSION RESULT_VARIABLE RES OUTPUT_STRIP_TRAILING_WHITESPACE)
if(NOT ${RES} EQUAL "0")
  set(GIT_VERSION "Unknown")
endif()
add_definitions(-DCVERSION=\"${GIT_VERSION}\")
string(TIMESTAMP TS %Y-%m-%d )
add_definitions(-DCTIME=\"${TS}\")


# find GSW, 
# TODO, do this correctly
file(GLOB GSW_TB_SRC "../gsw/toolbox/*.[f,F]90")
file(GLOB GSW_MD_SRC "../gsw/modules/*.[f,F]90")

# common objects
set(SRCS ../geoKdTree/kdtree.f90 datatable.f90 hzsmooth.f90)
add_library( util_common STATIC ${SRCS} )
set_target_properties(util_common PROPERTIES 
    COMPILE_FLAGS "${OpenMP_C_FLAGS} $ENV{UTIL_COMPILE_FLAGS}")


# pressure bias correction
set(SRCS pressure_correction.F90 ${GSW_TB_SRC} ${GSW_MD_SRC})
set(EXE pressure_correction)
add_executable(${EXE} ${SRCS})
target_link_libraries(${EXE} util_common $ENV{NETCDF_LIBS})
set_target_properties(${EXE} PROPERTIES
    COMPILE_FLAGS "$ENV{UTIL_COMPILE_FLAGS}" )


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


# mppnccombine
set(EXE mppnccombine)
add_executable(${EXE} mppnccombine.c)
target_link_libraries(${EXE} $ENV{NETCDF_LIBS})
set_target_properties(${EXE} PROPERTIES
    COMPILE_FLAGS "$ENV{UTIL_COMPILE_FLAGS}")			    
