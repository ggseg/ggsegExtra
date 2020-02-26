
run_tcl <- function(region, indir, hemisphere, outdir, verbose = TRUE){
  
  labfile <- paste0(indir, "/", hemisphere, "_", region, ".label")
  
  if(!dir.exists(outdir)) dir.create(outdir, recursive = TRUE)
  
  xpts <- paste0("export INLABFILE=", labfile,
                 "; export REGNAME=", region,
                 "; export TD=", outdir, "; ")
  
  
  tcl_script <- system.file("sh", "snapshot.tcl", package = "ggsegExtra")
  
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
  
  smooth_script <- system.file("sh", "smooth_labels.sh", package = "ggsegExtra")
  
  wb <- paste0("export PATH=", get_wb_command(), ";")
  
  cmd <- paste(
    wb,
    smooth_script,
    lab_file,
    surf_file,
    output
  )
  
  k <- system(cmd, intern=!verbose)
  
  invisible(k)
}


get_wb_command <- function(){
  PATH <- strsplit(Sys.getenv("PATH"), ":")[[1]]
  path <- PATH[grepl("workbench", PATH)]
  
  if(length(path) == 0){
    if(grepl("darwin", sessionInfo()$platform)){
      dirs <- list.dirs("/Applications/workbench", recursive = FALSE)
      dirs <- dirs[grepl("bin", dirs)]
      PATH <- c(PATH, dirs)
    }
  }
  
  PATH <- paste0(PATH, collapse = ":")
  
  return(PATH)
}
