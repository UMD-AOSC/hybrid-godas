cmake_minimum_required(VERSION 2.8 FATAL_ERROR)

project(bias_correction Fortran)

include_directories($ENV{NETCDF_INCS})


# TODO, compile GSW separately
file(GLOB GSW_TB_SRC  "../gsw/toolbox/*.[f,F]90")
file(GLOB GSW_MD_SRC "../gsw/modules/*.[f,F]90")
set(SRCS
  pressure_correction.F90
  datatable.F90
${GSW_TB_SRC}
${GSW_MD_SRC})

add_executable(pressure_correction ${SRCS})
target_link_libraries(pressure_correction $ENV{NETCDF_LIBS})
