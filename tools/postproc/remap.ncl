load "$NCARG_ROOT/lib/ncarg/nclscripts/esmf/ESMF_regridding.ncl"
begin
;    WRITE_RESULTS = True 

; configurables
  infile = getenv("infile")
  outfile = getenv("outfile")
  varsin=getenv("vars")
  vars=str_split(varsin, ",")
  weights=getenv("weights")

; source file(s)
  sf = addfile(infile,"r")

; get output file ready
  system("/bin/rm -f "+outfile)
  df=addfile(outfile,"c")
  filedimdef(df,"time",-1,True)

; process variables at T points
  do i=0,dimsizes(vars)-1
    df->$vars(i)$ = ESMF_regrid_with_weights(sf->$vars(i)$,  weights, False)
  end do			

end
