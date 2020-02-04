
run_tcl <- function(region, indir, hemisphere, outdir, verbose = TRUE){
  
  labfile <- paste0(indir, "/", hemisphere, "_", region, ".label")
  
  if(!dir.exists(outdir)) dir.create(outdir, recursive = TRUE)
  
  xpts <- paste0("export INLABFILE=", labfile,
                 "; export REGNAME=", region,
                 "; export TD=", outdir, "; ")
  
  
  tcl_script <- system.file("bash_scripts", "snapshot.tcl", package = "ggsegExtra")
  
  fs_cmd <- paste0(freesurfer::get_fs(),
                   "tksurfer")
  
  cmd <- paste(
    xpts,
    fs_cmd,
    "fsaverage ",
    hemisphere, "inflated",
    "-tcl", tcl_script,
    "-title", region
  )
  
  k <- system(cmd, intern=!verbose)
  
  invisible(k)
}

run_smooth_labels <- function(lab_file, 
                              surf_file, 
                              output, 
                              verbose=TRUE){
  
  smooth_script <- system.file("bash_scripts", "smooth_labels.sh", package = "ggsegExtra")
  
  cmd <- paste(
    smooth_script,
    lab_file,
    surf_file,
    output
  )
  
  k <- system(cmd, intern=!verbose)
  
  invisible(k)
}
