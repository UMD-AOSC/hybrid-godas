find_package(MPI REQUIRED)

#TODO add optimization flags
if(CMAKE_Fortran_COMPILER_ID MATCHES Intel)
  set(CMAKE_Fortran_FLAGS "${CMAKE_Fortran_FLAGS} -fno-alias -auto -safe-cray-ptr -ftz -assume byterecl -i4 -r8 -nowarn -sox -traceback")
elseif(CMAKE_Fortran_COMPILER_ID MATCHES GNU)
  set(CMAKE_Fortran_FLAGS "${CMAKE_Fortran_FLAGS} -fcray-pointer -fdefault-double-8 -fdefault-real-8 -Waliasing -ffree-line-length-none -fno-range-check")
endif()

include_directories(
  ${NETCDF_INCLUDE_DIRS}
  ${MPI_Fortran_INCLUDE_DIRS})


macro( addfiles path)
  file(GLOB g
    ${path}/*.F90
    ${path}/*.f90
    ${path}/*.h
    ${path}/*.c
    ${path}/*.inc)
  list(APPEND SRC ${g})
endmacro()


# FMS
set(FMS_DIR ${CMAKE_CURRENT_SOURCE_DIR}/MOM6/src/FMS)
set(SRC)
addfiles(${FMS_DIR}/*)
add_library(fms STATIC ${SRC})

target_compile_definitions(fms PUBLIC
  -Duse_libMPI
  -Duse_netCDF
  -DSPMD)
target_include_directories(fms PUBLIC
  ${FMS_DIR}/fms
  ${FMS_DIR}/include
  ${FMS_DIR}/mpp/include)
set_property(SOURCE MOM6/src/FMS/fft/fft.F90 APPEND PROPERTY OBJECT_DEPENDS
  ${CMAKE_CURRENT_BINARY_DIR}/CMakeFiles/fms.dir/MOM6/src/FMS/fft/fft99.F90.o)


# MOM6
set(MOM_DIR ${CMAKE_CURRENT_SOURCE_DIR}/MOM6/src)
set(SRC)
addfiles(${MOM_DIR}/MOM6/config_src/dynamic)
addfiles(${MOM_DIR}/MOM6/config_src/coupled_driver)
addfiles(${MOM_DIR}/MOM6/src/*)
addfiles(${MOM_DIR}/MOM6/src/*/*)
addfiles(${MOM_DIR}/atmos_null)
addfiles(${MOM_DIR}/coupler)
addfiles(${MOM_DIR}/land_null)
addfiles(${MOM_DIR}/ice_ocean_extras/*)
addfiles(${MOM_DIR}/icebergs)
addfiles(${MOM_DIR}/SIS2/config_src/dynamic)
addfiles(${MOM_DIR}/SIS2/src)
addfiles(${MOM_DIR}/FMS/coupler)
addfiles(${MOM_DIR}/FMS/include)
add_executable(mom6 ${SRC})

target_include_directories(mom6 PUBLIC
  ${MOM_DIR}/FMS/include
  ${MOM_DIR}/MOM6/src/framework
  ${MOM_DIR}/MOM6/config_src/dynamic
  ${MOM_DIR}/SIS2/config_src/dynamic
  ${MOM_DIR}/SIS2/src/)
target_link_libraries(mom6 PUBLIC
  fms
  ${NETCDF_LIBRARIES}
  ${MPI_Fortran_LIBRARIES})
target_compile_definitions(mom6 PRIVATE
  -Duse_libMPI
  -Duse_netCDF
  -DSPMD
  -Duse_AM3_physics
  -D_USE_LEGACY_LAND_)


