cmake_minimum_required(VERSION 2.8 FATAL_ERROR)


add_definitions(-DCVERSION=\"${GIT_VERSION}\")


# external libraries
include_directories(${geokdtree_INCLUDE_DIRS})
include_directories(${datetime_INCLUDE_DIRS})
include_directories(${gsw_INCLUDE_DIRS})
include_directories(${NETCDF_INCLUDE_DIRS})


# ------------------------------
# common objects used by all of the following programs
set(EXE
  obsop_common)
set(SRC
  cubic_spline.f90
  running_stats.F90
  obscom_grid.f90
  obscom_obsio.f90)
add_library( ${EXE} STATIC ${SRC})
target_link_libraries(${EXE}
  ${datetime_LIBRARIES}
  ${NETCDF_LIBRARIES}
  geokdtree)


# ------------------------------
# insitu observation preparation
set(EXE
  obsprep_insitu)
set(SRC
  obsprep_insitu.F90)
add_executable(${EXE} ${SRC})
target_link_libraries(${EXE}
  obsop_common)


# ------------------------------
# satellite SST observation preparation
set(EXE
  obsprep_sst)
set(SRC
  obsprep_sst.F90
  obsprep_sst_gds2.f90)
add_executable(${EXE} ${SRC})
target_link_libraries(${EXE}
  obsop_common)


# ------------------------------
# satellite ADT observation preparation
set(EXE
  obsprep_adt)
set(SRC
  obsprep_adt.F90)
add_executable(${EXE} ${SRC})
target_link_libraries(${EXE}
  obsop_common)


# ------------------------------
# observation netcdf file combine
set(EXE
  obsprep_combine)
set(SRC
  obsprep_combine.F90)
add_executable(${EXE} ${SRC})
target_link_libraries(${EXE}
  obsop_common)

# ------------------------------
# observation operator
set(EXE
  obsop)
set(SRC
  obsop.F90)
add_executable(${EXE} ${SRC})
target_link_libraries(${EXE}
  gsw obsop_common)


# ------------------------------
#set(SRCS obsprep_nc2dat.F90)
#set(EXE obsprep_nc2dat)
#add_executable(${EXE} ${SRCS})
#target_link_libraries(${EXE} datetime obsop_common ${NETCDF_LIBRARIES})
