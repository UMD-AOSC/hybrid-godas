cmake_minimum_required(VERSION 2.8 FATAL_ERROR)


add_definitions(-DCVERSION=\"${GIT_VERSION}\")


# external libraries
FIND_PACKAGE( OpenMP REQUIRED)
include_directories(${NETCDF_INCLUDE_DIRS})


# ------------------------------
# common objects
set(EXE
  util_common)
set(SRC
  datatable.f90
  hzsmooth.f90)
add_library(${EXE} STATIC ${SRC})


# ------------------------------
# background error variance calculator
set(EXE
  bgvar)
set(SRC
  bgvar.F90)
add_executable(${EXE} ${SRC})
target_link_libraries(${EXE}
  util_common
  ${NETCDF_LIBRARIES})
set_target_properties(${EXE} PROPERTIES
  LINK_FLAGS "${OpenMP_Fortran_FLAGS}" )


# ------------------------------
# vertical localization calculator
set(EXE
  vtloc)
set(SRC
  vtloc.F90)
add_executable(${EXE} ${SRC})
target_link_libraries(${EXE}
  util_common
  ${NETCDF_LIBRARIES})
set_target_properties(${EXE} PROPERTIES 
  LINK_FLAGS "${OpenMP_Fortran_FLAGS}")


# ------------------------------
# netcdf tile combiner
set(EXE
  mppnccombine)
add_executable(${EXE}
  mppnccombine.c)
target_link_libraries(${EXE}
  m
  ${NETCDF_LIBRARIES})
