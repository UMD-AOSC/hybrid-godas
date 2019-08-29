cmake_minimum_required(VERSION 2.8 FATAL_ERROR)


add_definitions(-DCVERSION=\"${GIT_VERSION}\")


# external libraries
FIND_PACKAGE( OpenMP REQUIRED)
include_directories(${NETCDF_INCLUDE_DIRS})

#-------------------------------
# restart update
set(EXE
  rst_update)
set(SRC
  rst_update.F90)
add_executable(${EXE} ${SRC})
target_link_libraries(${EXE}
  ${NETCDF_LIBRARIES})


# ------------------------------
# netcdf tile combiner
set(EXE
  mppnccombine)
add_executable(${EXE}
  mppnccombine.c)
target_link_libraries(${EXE}
  m
  ${NETCDF_LIBRARIES})
