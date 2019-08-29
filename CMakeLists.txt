cmake_minimum_required(VERSION 3.6)

project(HybridGODAS C Fortran)

# update the git submodules
find_package(Git)
option(GIT_SUBMODULE "Check submodules during build" OFF)
if(GIT_SUBMODULE)
  message(STATUS "updating git submodules...")	
  execute_process(COMMAND ${GIT_EXECUTABLE} submodule update --init --recursive
    WORKING_DIRECTORY ${CMAKE_CURRENT_SOURCE_DIR}
    RESULT_VARIABLE GIT_SUBMOD_RESULT)
  if(NOT GIT_SUBMOD_RESULT EQUAL "0")
    message(FATAL_ERROR "git submodule update --init failed with ${GIT_SUBMOD_RESULT}, please checkout submodules")
  endif()
endif()

# determine the git repository version
execute_process(COMMAND get describe --dirty --tags --always
  OUTPUT_VARIABLE GIT_VERSION
  RESULT_VARIABLE RES
  OUTPUT_STRIP_TRAILING_WHITESPACE
  WORKING_DIRECTORY ${CMAKE_CURRENT_SOURCE_DIR})
IF(NOT ${RES} EQUAL "0")
  SET(GIT_VERSION "Unknown")
ENDIF()


# external libraries
#------------------------------------------------------------
list(APPEND CMAKE_MODULE_PATH ${CMAKE_SOURCE_DIR}/cmake)

# MPI
find_package(MPI REQUIRED)

# NetCDF
set(NETCDF_F90 "YES")
find_package(NetCDF REQUIRED)



add_subdirectory(pkg)
add_subdirectory(src)
