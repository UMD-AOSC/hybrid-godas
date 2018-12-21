#!/usr/bin/env python
import os


#------------------------------------------------------------
# user configurable variables
#------------------------------------------------------------
root_dir='/lustre/f2/dev/ncep/Travis.Sluka/hybridgodas-work/cleanup'
machine_config=root_dir+'/config/gaea.env'
exp_name='cleanup'
exp_dir=root_dir+'/TEST'

# DA / forecast type
ens_size=2
da_mode="hyb"
da_wndw_ofst=48
cycle_len=120
cycle_leapadj=1

# surface forcing 
#TODO add

entity={
 'EXP_NAME':exp_name,
 'EXP_DIR':exp_dir,
 'ENS_LIST_FCST':' '.join(["{:04d}".format(x) for x in range(ens_size+1)]),

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

 'MAXCYCLES':2,
 'MAXTRIES':100,

# directories
 'HYGODAS_SRC_DIR':'/lustre/f2/dev/ncep/Travis.Sluka/hybridgodas-work/cleanup',
 'BIN_DIR':'/lustre/f2/dev/ncep/Travis.Sluka/hybridgodas-work/cleanup/build/bin',
 'SCRATCH_DIR':'/lustre/f2/scratch/ncep/Travis.Sluka/hygodas/&EXP_NAME;/cycle_&CYCLE;',
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

outfile="rocoto.xml"

of=open(outfile, 'w')

env_com={
 'CYCLE':'&CYCLE;',
 'CYCLE_LEN':cycle_len,
 'CYCLE_LEAPADJ':cycle_leapadj,
 'DA_WNDW_OFST':da_wndw_ofst,
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
of.write("""
<workflow realtime="F" scheduler="&SCHED;" cyclethrottle="&MAXCYCLES;">
  <cycledef>200404010000 200406050000 120:00:00</cycledef>
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
# observation preparation
#------------------------------------------------------------
of.write("\n\n\n<!--Observation preparation-->")
job={
 'name':'da.prep',
 'walltime':'1:00',
 'sched_queue':'&SCHED_S_QUEUE;',
 'sched_args':'&SCHED_S_ARGS;',
 'cores':1,
 'command':'&SCRIPT_DIR;/da.prep.sh',
 'env':{**env_com,
   'WORK_DIR':"&SCRATCH_DIR;/da.prep"
   }
}
#writeJob(job)
#      <account>&SCHED_ACCT;</account>
#      <queue>&SCHED_QUEUE_S;</queue>
#      <cores>1</cores>
#      <native>&SCHED_ARGS_S;</native>




#------------------------------------------------------------
# forecst preparation
#------------------------------------------------------------
of.write("\n\n\n<!--Forecast preparation-->")
job={
 'name':'fcst.prep',
 'walltime':'1:00',
 'sched_queue':'&SCHED_S_QUEUE;',
 'sched_args':'&SCHED_S_ARGS;',
 'cores':1,
 'command':'&SCRIPT_DIR;/fcst.prep.sh',
 'env':{**env_com,
        'WORK_DIR':"&SCRATCH_DIR;/fcst.prep",
        'ENS_SIZE':ens_size,
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
 'walltime':'1:00',
 'sched_queue':'&SCHED_S_QUEUE;',
 'sched_args':'&SCHED_S_ARGS;',
 'cores':1,
 'command':'&SCRIPT_DIR;/da.run.sh',
 'depends':[ ('task','da.prep'), 
             *[ ('data','&SCRATCH_DIR;/fcst.bkg/mem_{:04d}/fcst_done'.format(x)) 
                for x in range(ens_size+1)]],
 'env':{**env_com}
}
writeJob(job)



#------------------------------------------------------------
# forecast run
#------------------------------------------------------------
of.write("\n\n\n<!-- Model forecast run-->")
env=env_com
job={
 'name':'fcst.run.#mem#',
 'walltime':'&FCST_WALLTIME;',
 'sched_queue':'&SCHED_P_QUEUE;',
 'sched_args':'&SCHED_FCST_ARGS;',
 'nodes':'&FCST_NODES;',
 'command':'&SCRIPT_DIR;/fcst.run.sh',
 'depends':[ ('task','da.run'),
             ('task','fcst.prep')],
 'env':{**env_com,
        'WORK_DIR':"&SCRATCH_DIR;/fcst.run/mem_#mem#",
        'FORC_DIR':"&SCRATCH_DIR;/fcst.prep/mem_#mem#",
        'MOM_CFG_DIR':'&EXP_DIR;/config/mom_cfg',
        'MOM_INPUT_DIR':'&EXP_DIR;/config/mom_input',
        'NODES':'&FCST_NODES;',
        'PPN':'&PPN;',
        'RST_DIR_IN':"&EXP_DIR;/cycle/&CYCLE;/rst/mem_#mem#",
        'RST_DIR_OUT':"&EXP_DIR;/cycle/&CYCLE;/rst.next/mem_#mem#",
        'MEM':"#mem#",
        'FCST_RESTART':0,
        'FCST_DIAG_DA':1,
        'FCST_DIAG_OTHER':1,
        'MOM6_EXE':"&BIN_DIR;/mom6",
        'NIPROC':'&FCST_NIPROC;',
        'NJPROC':'&FCST_NJPROC;',
        'MASKTABLE':'&FCST_MASKTABLE;'
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
of.write("\n\n\n<!--Post processing tasks-->")
job={
 'name':'fcst.post',
 'walltime':'1:00',
 'sched_queue':'&SCHED_S_QUEUE;',
 'sched_args':'&SCHED_S_ARGS;',
 'cores':1,
 'command':'&SCRIPT_DIR;/fcst.post.sh',
 'depends':[ ('metatask','fcst.run.all') ], #TODO, can this be done after member 0000 is done?
 'env':{**env_com}

}
writeJob(job)

job={
 'name':'da.post',
 'walltime':'1:00',
 'sched_queue':'&SCHED_S_QUEUE;',
 'sched_args':'&SCHED_S_ARGS;',
 'cores':1,
 'command':'&SCRIPT_DIR;/da.post.sh',
 'depends':[ ('task','da.run') ],
 'env':{**env_com}
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
