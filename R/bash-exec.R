
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

# 
# run_smooth_labels <- function(lab_file, 
#                               surf_file, 
#                               output, 
#                               verbose = TRUE){
#   
#   smooth_script <- system.file("sh", "smooth_labels.sh", package = "ggsegExtra")
#   
#   wb <- paste0("export PATH=", get_wb_command(), ";")
#   
#   cmd <- paste(
#     wb,
#     smooth_script,
#     lab_file,
#     surf_file,
#     output
#   )
#   
#   k <- system(cmd, intern=!verbose)
#   
#   invisible(k)
# }


# #' HPC workbench command
# #'
# #' Some options for optimization of region
# #' extraction for ggseg atlases can be run using the
# #' HPC workbench program. This can be installed on the system
# #' in various way. Here, the function will either assume
# #' workbench is in standard macOS location, or in the locatino
# #' you provide to the function call. This path will be added
# #' to the environment $PATH. Currently does not work on windows.
# #'
# #' @param wbdir optional path to workbench program
# #'
# #' @return PATH
# #' @export
# #'
# #' @examples
# #' wb_command()
# #'
# #' wb_command("some/path/workbench")
# wb_command <- function(wbdir = NULL){
# 
# 
#   PATH <- strsplit(Sys.getenv("PATH"), ":")[[1]]
# 
#   if(is.null(wbdir)){
#     path <- PATH[grepl("workbench", PATH)]
# 
#     if(length(path) == 0){
#       if(grepl("darwin", utils::sessionInfo()$platform)){
#         dirs <- list.dirs("/Applications/workbench", recursive = FALSE)
#         dirs <- dirs[grepl("bin", dirs)]
#         PATH <- c(PATH, dirs)
#       }
#     }
# 
#   }else{
#     PATH <- c(PATH, wbdir)
#   }
# 
#   PATH <- paste0(PATH, collapse = ":")
# 
#   return(PATH)
# }
