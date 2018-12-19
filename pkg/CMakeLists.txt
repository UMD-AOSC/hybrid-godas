
add_subdirectory(UMD-LETKF)
add_subdirectory(godas-3dvar)
add_subdirectory(model)

# datetime module for fortran
add_subdirectory(datetime-fortran)
set(datetime_INCLUDE_DIRS ${CMAKE_CURRENT_BINARY_DIR}/datetime-fortran/include
  CACHE INTERNAL "datetime-fortran include directories" FORCE)
set(datetime_LIBRARIES datetime
  CACHE INTERNAL "datetime-fortran library" FORCE)


# Gibbs Sewater toolbox for fortran
file(GLOB GSW_TB_SRC "GSW-Fortran/toolbox/*.[f,F]90")
file(GLOB GSW_MD_SRC "GSW-Fortran/modules/*.[f,F]90")
add_library(gsw OBJECT ${GSW_TB_SRC} ${GSW_MD_SRC})
set_target_properties(gsw PROPERTIES
  Fortran_MODULE_DIRECTORY "${CMAKE_CURRENT_BINARY_DIR}/gsw")
set(gsw_INCLUDE_DIRS ${CMAKE_CURRENT_BINARY_DIR}/gsw
  CACHE INTERNAL "gsw-fortran include directories for modules" FORCE)


# geoKDTree
add_library(geokdtree OBJECT geoKdTree/kdtree.f90)
set_target_properties(geokdtree PROPERTIES
  Fortran_MODULE_DIRECTORY "${CMAKE_CURRENT_BINARY_DIR}/geokdtree")
set(geokdtree_INCLUDE_DIRS ${CMAKE_CURRENT_BINARY_DIR}/geokdtree
  CACHE INTERNAL "geokdtree include directories for modules" FORCE)
set(geokdtree_LIBRARIES geokdtree
  CACHE INTERNAL "geoKdTree library" FORCE)
