#!/bin/bash
# This file, when sourced, generates the diagnostic ouput configuration file for MOM6.
#
# Required variables:
#  $fcst_len          forecast length (days)
#  $fcst_diag_daily   if 1, daily mean ouput files are saved (needed for data assimilation)
#  $fcst_diag_pentad  if 1, pentad (or 6 day pentad if leap day) files are saved



# file header
#------------------------------------------------------------
cat <<EOF
OM4_SIS2_cgrid_025
1950 1 1 0 0 0

EOF




#------------------------------------------------------------
# Files needed for data assimilation
#------------------------------------------------------------
if [ "$fcst_diag_daily" = 1 ]; then
cat <<EOF

"ocean_daily%4yr%2mo%2dy",     1,"days",1,"days","time",1,"days"

'ocean_model', 'SST',      'SST',      'ocean_daily%4yr%2mo%2dy',    'all', 'min', 'none',2
'ocean_model', 'temp',     'temp',     'ocean_daily%4yr%2mo%2dy',    'all', 'mean','none',2
'ocean_model', 'salt',     'salt',     'ocean_daily%4yr%2mo%2dy',    'all', 'mean','none',2
'ocean_model', 'ssh',      'ssh',      'ocean_daily%4yr%2mo%2dy',    'all', 'mean','none',2

## TODO, U,V aren't needed by the DA right now, but they will be eventually
#'ocean_model', 'u',        'u',        'ocean_daily%4yr%2mo%2dy',   'all', 'mean','none',2
#'ocean_model', 'v',        'v',        'ocean_daily%4yr%2mo%2dy',   'all', 'mean','none',2

## TODO, just calculate rho from T/S in the DA code?
'ocean_model', 'rhopot0',  'rhopot0',  'ocean_daily%4yr%2mo%2dy', 'all', 'mean','none',2

EOF
fi




#------------------------------------------------------------
# other diagnostic files
#------------------------------------------------------------
if [ "$fcst_diag_pentad" = 1 ]; then
cat <<EOF

"ocean_pentad%4yr%2mo%2dy",    ${fcst_len}, "days",   1, "days",   "time", ${fcst_len}, "days"
"ice_pentad%4yr%2mo%2dy",      ${fcst_len}, "days",   1, "days",   "time", ${fcst_len}, "days"
#"ocean_static",                -1,          "months", 1, "days",   "time"

# For some weird reason it seems MOM is often putting zeros in for the entire field of
# the first time averaged variable defined in this file (for averaging periods > ~2 days).
#  As a work around I'm just defining a 2D field here that I don't care about, until the bug is fixed.
"ocean_model", "tosga", "ignore_this",  "ocean_pentad%4yr%2mo%2dy",  "all", "mean", "none", 2
"ocean_model", "tosga", "ignore_this",  "ice_pentad%4yr%2mo%2dy",    "all", "mean", "none", 2



#------------------------------------------------------------
## grid defintion, only need to generate this once
#------------------------------------------------------------
# "ocean_model", "areacello",  "areacello",   "ocean_static", "all", "none", "none", 2
# "ocean_model", "deptho",     "deptho",      "ocean_static", "all", "none", "none", 2
# "ocean_model", "geolon",     "geolon",      "ocean_static", "all", "none", "none", 2
# "ocean_model", "geolat",     "geolat",      "ocean_static", "all", "none", "none", 2
# "ocean_model", "geolon_u",   "geolon_u",    "ocean_static", "all", "none", "none", 2
# "ocean_model", "geolat_u",   "geolat_u",    "ocean_static", "all", "none", "none", 2
# "ocean_model", "geolon_v",   "geolon_v",    "ocean_static", "all", "none", "none", 2
# "ocean_model", "geolat_v",   "geolat_v",    "ocean_static", "all", "none", "none", 2
# "ocean_model", "wet",        "wet",         "ocean_static", "all", "none", "none", 2
# "ocean_model", "wet_u",      "wet_u",       "ocean_static", "all", "none", "none", 2
# "ocean_model", "wet_v",      "wet_v",       "ocean_static", "all", "none", "none", 2
# "ocean_model", "dxt",        "dxt",         "ocean_static", "all", "none", "none", 2
# "ocean_model", "dyt",        "dyt",         "ocean_static", "all", "none", "none", 2
# "ocean_model", "dxCu",       "dxCu",        "ocean_static", "all", "none", "none", 2
# "ocean_model", "dyCu",       "dyCu",        "ocean_static", "all", "none", "none", 2
# "ocean_model", "dxCv",       "dxCv",        "ocean_static", "all", "none", "none", 2
# "ocean_model", "dyCv",       "dyCv",        "ocean_static", "all", "none", "none", 2
# "ocean_model", "thkcello",   "thkcello",    "ocean_static", "all", "none", "none", 2 # needed for hybrid vertical coord?



## U/V and derived quantities
#------------------------------------------------------------
"ocean_model", "u",          "u",           "ocean_pentad%4yr%2mo%2dy", "all", "mean", "none", 2
"ocean_model", "v",          "v",           "ocean_pentad%4yr%2mo%2dy", "all", "mean", "none", 2
"ocean_model", "speed",      "speed",       "ocean_pentad%4yr%2mo%2dy", "all", "mean", "none", 2
#"ocean_model", "SSU",        "SSU",         "ocean_pentad%4yr%2mo%2dy", "all", "mean", "none", 2
#"ocean_model", "SSV",        "SSV",         "ocean_pentad%4yr%2mo%2dy", "all", "mean", "none", 2



## mass / mass transport
#------------------------------------------------------------
"ocean_model", "umo_2d",      "umo_2d",      "ocean_pentad%4yr%2mo%2dy", "all", "mean", "none", 2   # vertical sum of X mass tranport 
"ocean_model", "vmo_2d",      "vmo_2d",      "ocean_pentad%4yr%2mo%2dy", "all", "mean", "none", 2   # vertical sum of Y mass tranport 
"ocean_model", "masso",       "masso",       "ocean_pentad%4yr%2mo%2dy", "all", "mean", "none", 2   # total mass of ocean
"ocean_model", "volo",        "volo",        "ocean_pentad%4yr%2mo%2dy", "all", "mean", "none", 2   # total volume of ocean
"ocean_model", "net_massin",  "net_massin",  "ocean_pentad%4yr%2mo%2dy", "all", "mean", "none", 2   # mass into ocean from precip, runoff, icemelt
"ocean_model", "net_massout", "net_massout", "ocean_pentad%4yr%2mo%2dy", "all", "mean", "none", 2



## surface height
#------------------------------------------------------------
"ocean_model", "ssh",         "ssh",         "ocean_pentad%4yr%2mo%2dy", "all", "mean", "none", 2
"ocean_model", "ssh_ga",      "ssh_ga",      "ocean_pentad%4yr%2mo%2dy", "all", "mean", "none", 2



## Temperature (and extra derived quantities)
#------------------------------------------------------------
"ocean_model", "temp",              "temp",              "ocean_pentad%4yr%2mo%2dy", "all", "mean", "none", 2
"ocean_model", "sst_global",        "sst_global",        "ocean_pentad%4yr%2mo%2dy", "all", "mean", "none", 2  # or tosga?
"ocean_model", "thetaoga",          "thetaoga",          "ocean_pentad%4yr%2mo%2dy", "all", "mean", "none", 2  # global mean temp
"ocean_model", "Th_tendency_2d",    "Th_tendency_2d",    "ocean_pentad%4yr%2mo%2dy", "all", "mean", "none", 2  # not sure of difference with prev
"ocean_model", "Th_tendency_xyave", "Th_tendency_xyave", "ocean_pentad%4yr%2mo%2dy", "all", "mean", "none", 2
"ocean_model", "temp_layer_ave",    "temp_layer_ave",    "ocean_pentad%4yr%2mo%2dy", "all", "mean", "none", 2
"ocean_model", "T_adx_2d",          "T_adx_2d",          "ocean_pentad%4yr%2mo%2dy", "all", "mean", "none", 2
"ocean_model", "T_ady_2d",          "T_ady_2d",          "ocean_pentad%4yr%2mo%2dy", "all", "mean", "none", 2
#ocean_model", "SST",               "SST",               "ocean_pentad%4yr%2mo%2dy", "all", "mean", "none", 2



## salinity and extra derived quantities 
#------------------------------------------------------------
"ocean_model", "salt",              "salt",              "ocean_pentad%4yr%2mo%2dy", "all", "mean", "none", 2
"ocean_model", "sss_global",        "sss_global",        "ocean_pentad%4yr%2mo%2dy", "all", "mean", "none", 2
"ocean_model", "soga",              "soga",              "ocean_pentad%4yr%2mo%2dy", "all", "mean", "none", 2
"ocean_model", "Sh_tendency_2d",    "Sh_tendency_2d",    "ocean_pentad%4yr%2mo%2dy", "all", "mean", "none", 2
"ocean_model", "Sh_tendency_xyave", "Sh_tendency_xyave", "ocean_pentad%4yr%2mo%2dy", "all", "mean", "none", 2
"ocean_model", "salt_layer_ave",    "salt_layer_ave",    "ocean_pentad%4yr%2mo%2dy", "all", "mean", "none", 2
"ocean_model", "S_adx_2d",          "S_adx_2d",          "ocean_pentad%4yr%2mo%2dy", "all", "mean", "none", 2
"ocean_model", "S_ady_2d",          "S_ady_2d",          "ocean_pentad%4yr%2mo%2dy", "all", "mean", "none", 2
#"ocean_model", "SSS",               "SSS",               "ocean_pentad%4yr%2mo%2dy", "all", "mean", "none", 2



## wind stress
#------------------------------------------------------------
"ocean_model", "taux",     "taux",     "ocean_pentad%4yr%2mo%2dy", "all", "mean", "none", 2
"ocean_model", "tauy",     "tauy",     "ocean_pentad%4yr%2mo%2dy", "all", "mean", "none", 2



## heat flux
#------------------------------------------------------------
"ocean_model", "hfrainds",               "hfrainds",               "ocean_pentad%4yr%2mo%2dy", "all", "mean", "none", 2  # realtive to 0deg, from liquid+frozen precip into ocean
#"ocean_model", "hfrunoffds",             "hfrunoffds",             "ocean_pentad%4yr%2mo%2dy", "all", "mean", "none", 2  # relative to 0deg, from liquid+solid runoff into ocean
#"ocean_model", "heat_content_lprec",     "heat_content_lprec",     "ocean_pentad%4yr%2mo%2dy", "all", "mean", "none", 2
#"ocean_model", "heat_content_fprec",     "heat_content_fprec",     "ocean_pentad%4yr%2mo%2dy", "all", "mean", "none", 2
#"ocean_model", "heat_content_vprec",     "heat_content_vprec",     "ocean_pentad%4yr%2mo%2dy", "all", "mean", "none", 2
"ocean_model", "heat_content_surfwater", "heat_content_surfwater", "ocean_pentad%4yr%2mo%2dy", "all", "mean", "none", 2  # dont need? just add {l,f,v}prec ?
#"ocean_model", "heat_content_lrunoff",   "heat_content_lrunoff",   "ocean_pentad%4yr%2mo%2dy", "all", "mean", "none", 2
#"ocean_model", "heat_content_massout",   "heat_content_massout",   "ocean_pentad%4yr%2mo%2dy", "all", "mean", "none", 2  # from evaporation & ice forming, hfevapds
#"ocean_model", "heat_content_massin",    "heat_content_massin",    "ocean_pentad%4yr%2mo%2dy", "all", "mean", "none", 2
#"ocean_model", "heat_content_cond",      "heat_content_cond",      "ocean_pentad%4yr%2mo%2dy", "all", "mean", "none", 2
"ocean_model", "latent_fprec_diag",      "latent_fprec_diag",      "ocean_pentad%4yr%2mo%2dy", "all", "mean", "none", 2  # from melting of fprec (hfsnthermds)
#"ocean_model", "Heat_PmE",               "Heat_PmE",               "ocean_pentad%4yr%2mo%2dy", "all", "mean", "none", 2
"ocean_model", "net_heat_surface",       "net_heat_surface",       "ocean_pentad%4yr%2mo%2dy", "all", "mean", "none", 2
"ocean_model", "net_heat_coupler",       "net_heat_coupler",       "ocean_pentad%4yr%2mo%2dy", "all", "mean", "none", 2
"ocean_model", "sw",                     "sw",                     "ocean_pentad%4yr%2mo%2dy", "all", "mean", "none", 2
"ocean_model", "lw",                     "lw",                     "ocean_pentad%4yr%2mo%2dy", "all", "mean", "none", 2
"ocean_model", "latent",                 "lh",                     "ocean_pentad%4yr%2mo%2dy", "all", "mean", "none", 2
"ocean_model", "sensible",               "sh",                     "ocean_pentad%4yr%2mo%2dy", "all", "mean", "none", 2
#"ocean_model", "ave_hfsso",              "ave_hfsso",              "ocean_pentad%4yr%2mo%2dy", "all", "mean", "none", 2  # global mean SH
#"ocean_model", "ave_rsntds",             "ave_rsntds",             "ocean_pentad%4yr%2mo%2dy", "all", "mean", "none", 2  # global mean SW
#"ocean_model", "ave_rlntds",             "ave_rlntds",             "ocean_pentad%4yr%2mo%2dy", "all", "mean", "none", 2  # global mean LW
#"ocean_model", "ave_hflso",              "ave_hflso",              "ocean_pentad%4yr%2mo%2dy", "all", "mean", "none", 2  # global mean latent    
#"ocean_model", "ave_hfds",               "ave_hfds",               "ocean_pentad%4yr%2mo%2dy", "all", "mean", "none", 2  # global mean net heat surface
"ocean_model", "net_heat_coupler_ga",    "net_heat_coupler_ga",    "ocean_pentad%4yr%2mo%2dy", "all", "mean", "none", 2  # global mean net heat coupler

"ice_model",   "sh",                     "sh",                     "ice_pentad%4yr%2mo%2dy",   "all", "mean", "none", 2
"ice_model",   "lh",                     "lh",                     "ice_pentad%4yr%2mo%2dy",   "all", "mean", "none", 2
"ice_model",   "sw",                     "sw",                     "ice_pentad%4yr%2mo%2dy",   "all", "mean", "none", 2
"ice_model",   "lw",                     "lw",                     "ice_pentad%4yr%2mo%2dy",   "all", "mean", "none", 2
"ice_model",   "swdn",                   "swdn",                   "ice_pentad%4yr%2mo%2dy",   "all", "mean", "none", 2



## salt flux
#------------------------------------------------------------
"ice_model",   "saltf",                  "saltf",                 "ice_pentad%4yr%2mo%2dy",   "all", "mean", "none", 2
"ocean_model", "salt_flux_global_restoring_adjustment",      "salt_flux_global_restoring_adjustment",     "ocean_pentad%4yr%2mo%2dy", "all", "mean", "none", 2
"ocean_model", "salt_flux",              "salt_flux",             "ocean_pentad%4yr%2mo%2dy", "all", "mean", "none", 2
"ocean_model", "salt_flux_added",        "salt_flux_added",       "ocean_pentad%4yr%2mo%2dy", "all", "mean", "none", 2
"ocean_model", "total_salt_flux",        "total_salt_flux",       "ocean_pentad%4yr%2mo%2dy", "all", "mean", "none", 2



## Freshwater flux
#------------------------------------------------------------
# Note, the following are at the OCEAN/ICE surface
"ocean_model", "lprec",     "lprec",     "ocean_pentad%4yr%2mo%2dy", "all", "mean", "none", 2
"ocean_model", "fprec",     "fprec",     "ocean_pentad%4yr%2mo%2dy", "all", "mean", "none", 2
"ocean_model", "vprec",     "vprec",     "ocean_pentad%4yr%2mo%2dy", "all", "mean", "none", 2
"ocean_model", "evap",      "evap",      "ocean_pentad%4yr%2mo%2dy", "all", "mean", "none", 2
"ocean_model", "lrunoff",   "lrunoff",   "ocean_pentad%4yr%2mo%2dy", "all", "mean", "none", 2
#"ocean_model", "frunoff",   "frunoff",   "ocean_pentad%4yr%2mo%2dy", "all", "mean", "none", 2  # currently 0, there is no calving
"ocean_model", "PRCmE",     "PRCmE",     "ocean_pentad%4yr%2mo%2dy", "all", "mean", "none", 2
"ocean_model", "precip",    "precip",    "ocean_pentad%4yr%2mo%2dy", "all", "mean", "none", 2  # Don't need this, as its just a sum of {l,f,v}prec ?

"ocean_model", "net_fresh_water_global_adjustment", "net_fresh_water_global_adjustment", "ocean_pentad%4yr%2mo%2dy", "all", "mean", "none", 2
"ocean_model", "ave_wfo",   "ave_wfo",   "ocean_pentad%4yr%2mo%2dy", "all", "mean", "none", 2  # global mean prcme
"ocean_model", "ave_evs",   "ave_evs",   "ocean_pentad%4yr%2mo%2dy", "all", "mean", "none", 2  # global mean evap
"ocean_model", "precip_ga", "precip_ga", "ocean_pentad%4yr%2mo%2dy", "all", "mean", "none", 2  # global mean precip
"ocean_model", "total_vprec","total_vprec", "ocean_pentad%4yr%2mo%2dy", "all", "mean", "none", 2

"ice_model",   "snowfl",    "snowfl",    "ice_pentad%4yr%2mo%2dy",   "all", "mean", "none", 2  #redundant with ocn_model:fprec ?
"ice_model",   "rain",      "rain",      "ice_pentad%4yr%2mo%2dy",   "all", "mean", "none", 2
"ice_model",   "runoff",    "runoff",    "ice_pentad%4yr%2mo%2dy",   "all", "mean", "none", 2
#"ice_model",   "calving",   "calving",   "ice_pentad%4yr%2mo%2dy",   "all", "mean", "none", 2 # currently 0, there is no calving
"ice_model",   "evap",      "evap",      "ice_pentad%4yr%2mo%2dy",   "all", "mean", "none", 2



## mixed layer depth
#------------------------------------------------------------
"ocean_model", "mld_003",   "mld_003",    "ocean_pentad%4yr%2mo%2dy", "all", "mean", "none", 2
"ocean_model", "mld_0125",  "mld_0125",   "ocean_pentad%4yr%2mo%2dy", "all", "mean", "none", 2



## Ice / snow
#------------------------------------------------------------
"ice_model",   "cn",         "cn",         "ice_pentad%4yr%2mo%2dy",   "all", "mean", "none", 2
"ice_model",   "siu",        "siu",        "ice_pentad%4yr%2mo%2dy",   "all", "mean", "none", 2
"ice_model",   "siv",        "siv",        "ice_pentad%4yr%2mo%2dy",   "all", "mean", "none", 2
"ice_model",   "siconc",     "siconc",     "ice_pentad%4yr%2mo%2dy",   "all", "mean", "none", 2
"ice_model",   "sisnconc",   "sisnconc",   "ice_pentad%4yr%2mo%2dy",   "all", "mean", "none", 2
#"ice_model",   "simass",     "simass",     "ice_pentad%4yr%2mo%2dy",   "all", "mean", "none", 2  # mass ice
#"ice_model",   "sivol",      "sivol",      "ice_pentad%4yr%2mo%2dy",   "all", "mean", "none", 2
"ice_model",   "hi",         "hi",         "ice_pentad%4yr%2mo%2dy",   "all", "mean", "none", 2  # height ice
"ice_model",   "hs",         "hs",         "ice_pentad%4yr%2mo%2dy",   "all", "mean", "none", 2  # height snow
"ice_model",   "mi",         "mi",         "ice_pentad%4yr%2mo%2dy",   "all", "mean", "none", 2  # Mass ice + snow
#"ice_model",   "sispeed",    "sispeed",    "ice_pentad%4yr%2mo%2dy",   "all", "mean", "none", 2
#"ice_model",   "sitimefrac", "sitimefrac", "ice_pentad%4yr%2mo%2dy",   "all", "mean", "none", 2
#"ice_model",   "mib",        "mib",        "ice_pentad%4yr%2mo%2dy",   "all", "mean", "none", 2  # not needed if no icebergs?
#"ice_model",   "sst",        "sst",        "ice_pentad%4yr%2mo%2dy",   "all", "mean", "none", 2  # why is this slightly different from ocean SST?
#"ice_model",   "ext",        "ext",        "ice_pentad%4yr%2mo%2dy",   "all", "mean", "none", 2  # extent (0 or 1)



## misc
#------------------------------------------------------------
#"ocean_model", "p_surf",  "p_surf",  "ocean_pentad%4yr%2mo%2dy", "all", "mean", "none", 2  # surface pressure over water / under ice
#"ice_model",   "SLP",     "SLP",     "ice_pentad%4yr%2mo%2dy",   "all", "mean", "none", 2



EOF
fi
