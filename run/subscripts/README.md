These are the scripts that are combined by ROCOTO to form the multiple job steps needed to run the Hybrid-GODAS. Further documentation on each step is available within the script files.

| script                  | descrtiption |
| ------                  | -------------|
| ```fcst.prep.sh```      | Generates the ensemble surface forcing files. |
| ```fcst.run.sh```       | Runs the MOM6 ocean model |
| ```fcst.comb.slot.sh``` | Currently, MOM6 saves all output files in tiles, but the DA code can only handle a whole ocean grid. This combines those tiles into a single file. TODO: get rid of this and have all DA code be able to natively do I/O on the uncombined tiles. |
| ```fcst.comb.rst.sh```  | Same as ```fcst.comb.slot.sh``` except for the restart files that are needed for DA. TODO: get rid of this as well |
| ```da.obsop.slot.sh```  | Performs the observation preparation and observation operator |
| ```da.obsop.comb.sh```  | combines the observation operator outputs files from multiple slots so that there is a single file for a given ensemble member |
| ```da.letkf.run.sh```   | runs the LETKF |
| ```da.3dvar.run.sh```   | runs the 3DVar |
| ```da.update.sh````     | applies the LETKF/3DVAR analysis/analysis_increment to the ensemble member restart file |
| ```cycle.post.sh```     | performs postprocessing at the end of the cycle. Mainly cleans up and compresses the final files we want to save. |