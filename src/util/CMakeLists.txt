cmake_minimum_required(VERSION 2.8 FATAL_ERROR)

project(utilities Fortran)

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


# common objects
set(SRCS kdtree.f90 datatable.f90)
add_library( util_common STATIC ${SRCS} )

# bgerr var
file(GLOB SRCS "bgvar*.[f,F]90")
set(EXE  bgvar)
add_executable(${EXE} ${SRCS})
target_link_libraries(${EXE} util_common $ENV{NETCDF_LIBS})
