Jian Kuang 20190627
These will be separated into J-Jobs and scripts

These are the scripts that are combined by rocoto to form the multiple job steps needed to run the Hybrid-GODAS. Further documentation on each step is available within the script files.

| script                  | descrtiption |
| ------                  | -------------|
| `cycle.post.sh`     | performs postprocessing at the end of the cycle. Mainly cleans up and compresses the final files we want to save. |
| `da.3dvar.run.sh`   | runs the 3DVar |
| `da.letkf.run.sh`   | runs the LETKF |
| `da.obsop.comb.sh`  | combines the observation operator outputs files from multiple slots so that there is a single file for a given ensemble member |
| `da.obsop.slot.sh`  | Performs the observation preparation and observation operator for a single time slot of a single ensemble member |
| `da.update.sh`      | applies the LETKF/3DVAR analysis/analysis_increment to a single ensemble member restart file |
| `fcst.comb.rst.sh`  | Same as `fcst.comb.slot.sh` except for the restart files that are needed for DA. TODO: get rid of this |
| `fcst.comb.slot.sh` | MOM6 saves all output files in tiles, but the DA code can currently only handle a whole ocean grid. This combines those tiles into a single file. TODO: get rid of this and have all DA code be able to natively do I/O on the uncombined tiles. |
| `fcst.prep.sh`      | Generates the ensemble surface forcing files. |
| `fcst.run.sh`       | Runs the MOM6 ocean model for a single ensemble member |
