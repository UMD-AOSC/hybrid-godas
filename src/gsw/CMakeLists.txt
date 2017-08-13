cmake_minimum_required(VERSION 2.8 FATAL_ERROR)
project(gsw_toolbox Fortran)


file(GLOB TB_SRCS "GSW-Fortran/toolbox/*.[f,F]90")
file(GLOB MD_SRCS "GSW-Fortran/modules/*.[f,F]90")

# only compile the files from the toolbox that we actually need
#set(TB_SRCS gsw_saar_init gsw_p_from_z gsw_sa_from_sp gsw_ct_from_pt gsw_t_from_ct gsw_specvol_sso_0 gsw_enthalpy_sso_0 gsw_sa_from_sp_baltic gsw_saar gsw_pt_from_ct gsw_pt_from_t gsw_util_xinterp1 gsw_util_indx gsw_add_barrier gsw_add_mean gsw_gibbs_pt0_pt0 gsw_entropy_part gsw_gibbs)
#string(REGEX REPLACE "([^;]+)" "GSW-Fortran/toolbox/\\1.f90" TB_SRCS "${TB_SRCS}")


#set(MD_SRCS gsw_mod_netcdf gsw_mod_toolbox gsw_mod_kinds gsw_mod_teos10_constants gsw_mod_error_functions gsw_mod_specvol_coefficients gsw_mod_baltic_data gsw_mod_saar_data)
#string(REGEX REPLACE "([^;]+)" "GSW-Fortran/modules/\\1.f90" MD_SRCS "${MD_SRCS}")

set(SRCS ${TB_SRCS} ${MD_SRCS})
add_library(gsw_toolbox STATIC ${SRCS})
