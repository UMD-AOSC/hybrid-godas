#!/usr/bin/env python
import os
import sys

#------------------------------------------------------------
# user configurable variables
# TODO, move these outside
#------------------------------------------------------------
root_dir='/lustre/f2/dev/ncep/Travis.Sluka/hybridgodas-work/cleanup'
machine_config=root_dir+'/config/gaea.env'
exp_name='cleanup'
exp_dir=root_dir+'/TEST'
ens_size=20
da_mode="hyb" # none, var, ekf, hyb



entity={
 'EXP_NAME':exp_name,
 'EXP_DIR':exp_dir,
 'CYCLE_LEN':120,
 'CYCLE_LEAPADJ':1,
 'ENS_SIZE':ens_size,
 'DA_MODE':da_mode,
 'DA_WNDW_OFST':48,
 'FCST_RES':os.environ['MOM_RES'],
    
 'PPN':os.environ['HYGODAS_PPN'],
 'SCHED':os.environ['HYGODAS_SCHED'],
 'SCHED_ACCT':os.environ['HYGODAS_ACCT'],
 'SCHED_S_QUEUE':os.environ['HYGODAS_S_QUEUE'],
 'SCHED_S_ARGS':os.environ['HYGODAS_S_ARGS'],
 'SCHED_P_QUEUE':os.environ['HYGODAS_P_QUEUE'],
 'SCHED_P_ARGS':os.environ['HYGODAS_P_ARGS'],

 'SCHED_FCST_ARGS':'&SCHED_P_ARGS; -l size=&FCST_NODES;',
 'FCST_NODES':os.environ['HYGODAS_FCST_NODES'],
 'FCST_WALLTIME':os.environ['HYGODAS_FCST_WALLTIME'],
 'FCST_NIPROC':os.environ['HYGODAS_FCST_NIPROC'],
 'FCST_NJPROC':os.environ['HYGODAS_FCST_NJPROC'],
 'FCST_MASKTABLE':os.environ['HYGODAS_FCST_MASKTABLE'],
 'FCST_IO_LAYOUT':'4,6',

 'MAXCYCLES':2,
 'MAXTRIES':2,

# directories 
 'HYGODAS_SRC_DIR':'/lustre/f2/dev/ncep/Travis.Sluka/hybridgodas-work/cleanup',
 'BIN_DIR':'/lustre/f2/dev/ncep/Travis.Sluka/hybridgodas-work/cleanup/build/bin',
 'SCRATCH_DIR':'/lustre/f2/scratch/ncep/Travis.Sluka/hgodas/&EXP_NAME;/cycle_&CYCLE;',
 'CYCLE_DIR':'&EXP_DIR;/cycle',
 'OBS_DIR':'/lustre/f2/dev/ncep/Travis.Sluka/hybridgodas-DATA/obs',
 'GRID_DIR':'/lustre/f2/dev/ncep/Travis.Sluka/hybridgodas-DATA/grid/grid_&FCST_RES;',
 'MACHINE_CONFIG':machine_config,

# leave these alone!
 'CYCLE':'<cyclestr>@Y@m@d@H</cyclestr>',
 'LOG_DIR':'&EXP_DIR;/log/<cyclestr>@Y/@Y@m@d@H</cyclestr>',
 'SCRIPT_WRAPPER':"source &SCRIPT_DIR;/jobwrapper.sh;",
 'SCRIPT_DIR':'&HYGODAS_SRC_DIR;/run/subscripts',
}


# ------------------------------------------------------------
# shouldn't need to edit below here
# ------------------------------------------------------------

# check for valid data, and calculate derived parameters
valid_da_modes=("none",'var','ekf','hyb')
if da_mode not in valid_da_modes:
    print("ERROR: da_mode must be one of", valid_da_modes)
    sys.exit(1)
if da_mode in ("none","var") and ens_size > 1:
    print("ERROR: ens_size must be 1 when not doing ensemble DA")
    sys.exit(1)

fcst_size=ens_size+1 if ens_size > 1 else 1
entity['ENS_LIST_FCST']=' '.join(["{:04d}".format(x) for x in range(fcst_size)])

#---------------------------------------


outfile="rocoto.xml"

of=open(outfile, 'w')

env_com={
 'CYCLE':'&CYCLE;',
 'CYCLE_LEN':'&CYCLE_LEN;',
 'CYCLE_LEAPADJ':'&CYCLE_LEAPADJ;',
 'DA_WNDW_OFST':'&DA_WNDW_OFST;',
 'DA_SLOT_LEN':24, 
 'MACHINE_CONFIG':'&MACHINE_CONFIG;',
}

# start the file header
of.write('<?xml version="1.0"?>')

# entities
of.write("""
<!DOCTYPE workflow
[
""")
for e in entity:
    of.write("""<!ENTITY {} "{}">
""".format(e.ljust(15),str(entity[e])))
of.write("""
]>
""")


# start of workflow
# TODO determine the correct dates
of.write("""
<workflow realtime="F" scheduler="&SCHED;" cyclethrottle="&MAXCYCLES;">
  <cycledef>200312270000 200406050000 &CYCLE_LEN;:00:00</cycledef>
  <log verbosity="10">&LOG_DIR;/workflow.log</log>
""")


#------------------------------------------------------------
def writeJob(specs):
    of.write("""
    <task name="{name}" maxtries="&MAXTRIES;">
      <jobname>&EXP_NAME;.{name}</jobname>
      <join>&LOG_DIR;/{name}.log</join>
      <walltime>{walltime}</walltime>
      <account>&SCHED_ACCT;</account>
      <queue>{sched_queue}</queue>
      <native>{sched_args}</native>\n""".format(**specs))
    if 'cores' in specs:
            of.write("""      <cores>{cores}</cores>\n""".format(**specs))
    if 'nodes' in specs:
            of.write("""      <nodes>{nodes}:ppn=&PPN;</nodes>\n""".format(**specs))

    of.write("""
      <command>
        &SCRIPT_WRAPPER;
        {command}
      </command>\n""".format(**specs))

    if 'depends' in specs:
        of.write("""\n      <dependency><and>""")
        for d in specs['depends']:
            if d[0] == 'data':
                of.write("""\n        <datadep>{}</datadep>""".format(d[1]))
            else:
                of.write("""\n        <{0}dep {0}="{1}"/>""".format(*d))
        of.write("""\n      </and></dependency>\n""")

    if 'env' in specs:
        for e in specs['env']:
            of.write("""\n      <envar><name>{}</name><value>{}</value></envar>""".format(
                e, str(specs['env'][e])))            

    of.write("""
    </task>""")
#------------------------------------------------------------

    


#------------------------------------------------------------
# DA preparation
#------------------------------------------------------------
of.write("\n\n\n<!--Observation preparation-->")
job={
 'name':'da.prep',
 'walltime':'5:00',
 'sched_queue':'&SCHED_S_QUEUE;',
 'sched_args':'&SCHED_S_ARGS;',
 'cores':1,
 'command':'&SCRIPT_DIR;/da.prep.sh',
 'env':{**env_com,
   'WORK_DIR':"&SCRATCH_DIR;/da.prep",
   'OBS_SST':1,
   'OBS_ADT':1,
   'OBS_PROF':1,
   'OBS_SST_PATH':'&OBS_DIR;/sst_acspo_avhrr/%Y/%Y%m/%Y%m%d.nc',
   'OBS_ADT_PATH':'&OBS_DIR;/rads_adt/%Y/%Y%m%d',
   'OBS_PROF_PATH':'&OBS_DIR;/profile_wod18/%Y/%Y%m%d.nc',
   'DA_CFG_DIR':'&EXP_DIR;/config/da_cfg',
   'GRID_DIR':'&GRID_DIR;',
   'BIN_DIR':'&BIN_DIR;',
   'CYCLE_DIR':'&EXP_DIR;/cycle',
   'ENS_LIST':'&ENS_LIST_FCST;',
   }
}
writeJob(job)



#------------------------------------------------------------
# forecst preparation
#------------------------------------------------------------
of.write("\n\n\n<!--Forecast preparation-->")
job={
 'name':'fcst.prep',
 'walltime':'10:00',
 'sched_queue':'&SCHED_S_QUEUE;',
 'sched_args':'&SCHED_S_ARGS;',
 'cores':1,
 'command':'&SCRIPT_DIR;/fcst.prep.sh',
 'env':{**env_com,
        'WORK_DIR':"&SCRATCH_DIR;/fcst.prep",
        'ENS_SIZE':'&ENS_SIZE;',
        'FORC_MEAN_FILE':'/lustre/f2/dev/ncep/Travis.Sluka/hybridgodas-work/work1/DATA/fluxes/cfsr.6hr/%Y/%Y%m%d/cfsr.%Y%m%d.#var#.nc',
        'FORC_VAR':'PRATE,SLP,TMP.2m,SPFH.2m,UGRD.10m,VGRD.10m,DLWRF.sfc,DSWRF.sfc',
        'FORC_VAR_POS':'PRATE,SPFH.2m,DLWRF.sfc,DSWRF.sfc',
        'FORC_ENS_FILE':'/lustre/f2/dev/ncep/Travis.Sluka/hybridgodas-work/work1/DATA/fluxes/20CRv2c.6hr/%Y/%Y%m%d/#mem2#/20cr.%Y%m%d.#var#.nc',
        'FORC_VAR_ENS':'PRATE,TMP.2m,SPFH.2m,UGRD.10m,VGRD.10m,DLWRF.sfc,DSWRF.sfc',
        'FORC_CORR':1,
        'FORC_CORR_DIR':'/lustre/f2/dev/ncep/Travis.Sluka/hybridgodas-work/work1/DATA/fluxes/bias.cfsr_dfs52',
        'FORC_CORR_ADD':'UGRD.10m,VGRD.10m',
        'FORC_CORR_MUL':'PRATE,DLWRF.sfc,DSWRF.sfc',
        'FORC_RUNOFF_PERTURB':0,
        'FORC_RUNOFF_CLIM':'/lustre/f2/dev/ncep/Travis.Sluka/hybridgodas-work/work1/DATA/fluxes/runoff/runoff.daitren.v20180328.clim.nc',
        'FORC_RUNOFF_VAR':'/lustre/f2/dev/ncep/Travis.Sluka/hybridgodas-work/work1/DATA/fluxes/runoff/runoff.daitren.v20180328.var.nc',
    }
}
writeJob(job)



#------------------------------------------------------------
# DA run
#------------------------------------------------------------
of.write("\n\n\n<!--Data assimilation run-->")
job={
 'name':'da.run',
 'walltime':'10:00',
 'sched_queue':'&SCHED_P_QUEUE;',
# 'sched_args':'&SCHED_FCST_ARGS;', #fix this with correct vars
 'sched_args':'&SCHED_P_ARGS; -l size=3',
 'nodes':'3',
# 'sched_queue':'&SCHED_S_QUEUE;',
# 'sched_args':'&SCHED_S_ARGS;',
# 'cores':1,
 'command':'&SCRIPT_DIR;/da.sh',
 'depends':[ ('task','da.prep'), 
             *[ ('data','&SCRATCH_DIR;/da.prep/bkg/mem_{:04d}/fcst.done'.format(x)) 
                for x in range(fcst_size)]],
 'env':{**env_com,
    'PPN':'&PPN;',
    'ENS_SIZE':'&ENS_SIZE;',
    'WORK_DIR':'&SCRATCH_DIR;/da.run',
    'ENS_LIST':'&ENS_LIST_FCST;',
    'BIN_DIR':'&BIN_DIR;',
    'DA_CFG_DIR':'&EXP_DIR;/config/da_cfg',
    'GRID_DIR':'&GRID_DIR;',
    'SCRIPT_DIR':'&SCRIPT_DIR;',
    'DA_MODE':'&DA_MODE;',
    }
}
writeJob(job)



#------------------------------------------------------------
# forecast run
#------------------------------------------------------------
# TODO set FCST_RESTART correctly 
of.write("\n\n\n<!-- Model forecast run-->")
env=env_com
dep=[ ('task','fcst.prep'), 
      ('data','&SCRATCH_DIR;/da.prep/bkg/mem_#mem#/fcst.done') ]
if da_mode != 'none':
    dep.append(( 'task','da.run') )

job={
 'name':'fcst.run.#mem#',
 'walltime':'&FCST_WALLTIME;',
 'sched_queue':'&SCHED_P_QUEUE;',
 'sched_args':'&SCHED_FCST_ARGS;',
 'nodes':'&FCST_NODES;',
 'command':'&SCRIPT_DIR;/fcst.run.sh',
 'depends':dep,
 'env':{**env_com,
        'BIN_DIR':'&BIN_DIR;',
        'DA_MODE':'&DA_MODE;',
        'WORK_DIR':"&SCRATCH_DIR;/fcst.run/mem_#mem#",
        'SCRIPT_DIR':'&SCRIPT_DIR;',
        'FORC_DIR':"&SCRATCH_DIR;/fcst.prep/mem_#mem#",
        'MOM_CFG_DIR':'&EXP_DIR;/config/mom_cfg',
        'MOM_INPUT_DIR':'&EXP_DIR;/config/mom_input',
        'LETKF_ANA_DIR':'&SCRATCH_DIR;/da.run/mem_#mem#/letkf',
        'VAR_ANA_DIR' : '&SCRATCH_DIR;/da.run/var/',
        'DA_CFG_DIR':'&EXP_DIR;/config/da_cfg',
        'NODES':'&FCST_NODES;',
        'PPN':'&PPN;',
        'RST_DIR_IN':"&SCRATCH_DIR;/da.prep/bkg/mem_#mem#/fcst.rst",
        'RST_DIR_OUT':"&EXP_DIR;/cycle/#CYCLE_NEXT#/mem_#mem#/fcst.rst",
        'DIAG_DIR_OUT':"&EXP_DIR;/cycle/#CYCLE_NEXT#/mem_#mem#/fcst.diag",
        'FCST_DONE':"&EXP_DIR;/cycle/#CYCLE_NEXT#/mem_#mem#/fcst.done",
        'MEM':"#mem#",
        'FCST_DIAG_DA':1,
        'FCST_DIAG_OTHER':1,
        'MOM6_EXE':"&BIN_DIR;/mom6",
        'IO_LAYOUT':'&FCST_IO_LAYOUT;',
        'NIPROC':'&FCST_NIPROC;',
        'NJPROC':'&FCST_NJPROC;',
        'MASKTABLE':'&FCST_MASKTABLE;',
        'ALLOW_COLDSTART':1,
    }
}
of.write("""
  <metatask name="fcst.run.all">
    <var name="mem">&ENS_LIST_FCST;</var>
""")
writeJob(job)
of.write("""
  </metatask>
""")





#------------------------------------------------------------
# post processing
#------------------------------------------------------------
of.write("\n\n\n<!--control forecast post processing-->")
job={
 'name':'fcst.post',
 'walltime':'1:00',
 'sched_queue':'&SCHED_S_QUEUE;',
 'sched_args':'&SCHED_S_ARGS;',
 'cores':1,
 'command':'&SCRIPT_DIR;/fcst.post.sh',
 'depends':[ ('task','fcst.run.0000') ],
 'env':{**env_com,
        'POSTPROC_SCRIPTS':'&HYGODAS_SRC_DIR;/tools/postproc',
        'WORK_DIR':"&SCRATCH_DIR;/fcst.post",
        'COMBINE_EXE':"&BIN_DIR;/mppnccombine",
        'FCST_DIR':'&SCRATCH_DIR;/fcst.run/mem_0000',
        'DIAG_FILES':'*_pentad*',
        'OUTPUT_DIR':'&EXP_DIR;/output',
    }
}
writeJob(job)

of.write("\n\n\n<!-- data assimilation post processing-->")
job={
 'name':'da.post',
 'walltime':'5:00',
 'sched_queue':'&SCHED_S_QUEUE;',
 'sched_args':'&SCHED_S_ARGS;',
 'cores':1,
 'command':'&SCRIPT_DIR;/da.post.sh',
 'depends':[ ('task','da.run')],
 'env':{**env_com,    
    'GRID_DIR':'&GRID_DIR;',
    'DA_MODE':'&DA_MODE;',
    'POSTPROC_SCRIPTS':'&HYGODAS_SRC_DIR;/tools/postproc',
    'SAVE_OMF':1,
    'BIN_DIR':'&BIN_DIR;',
    'WORK_DIR':"&SCRATCH_DIR;/da.post",
    'ENS_LIST':'&ENS_LIST_FCST;',
    'OUTPUT_DIR':'&EXP_DIR;/output',    
    }
}
writeJob(job)


of.write("\n\n\n<!-- cleanup of unneeded files-->")
job={
 'name':'scrub',
 'walltime':'1:00',
 'sched_queue':'&SCHED_S_QUEUE;',
 'sched_args':'&SCHED_S_ARGS;',
 'cores':1,
 'command':'&SCRIPT_DIR;/cycle.scrub.sh',
 'depends':[ ('task', 'da.post'), 
             ('task', 'fcst.post'),
             ('metatask','fcst.run.all'),
         ],
 'env':{**env_com,
   'SCRATCH_DIR':'/lustre/f2/scratch/ncep/Travis.Sluka/hgodas/&EXP_NAME;',
   'SAVE_DIR':'&CYCLE_DIR;',
   'KEEP_CYCLES':1,
   'KEEP_CYCLES_REGEX':"^....(0101|0401|0705|1003)00",
    }
}
writeJob(job)
  
#------------------------------------------------------------
#------------------------------------------------------------


# end of workflow
of.write("""
</workflow>
""")

# end of 
of.close()
