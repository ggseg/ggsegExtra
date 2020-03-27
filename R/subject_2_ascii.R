
#' Convert subject surface files to ascii
#'
#' This function goes through all specified
#' subject,  per hermisphere, surface,
#' curvature and no fix curvature specified
#' and turns them into ascii files.
#'
#' @template subject
#' @template hemisphere 
#' @param surfaces string vector of surfaces
#' @param curvatures string vector of curvatures
#' @param nofix_curv string vector of nofix curvatures
#' @template subjects_dir 
#' @template output_dir 
#' @template verbose 
#'
#' @export
subject_2_ascii <- function(subject = "fsaverage5",
                            hemisphere = c("rh", "lh"),
                            surfaces = fs_surfaces(),
                            curvatures = fs_curvatures(),
                            nofix_curv = fs_nofixcurv(),
                            subjects_dir = freesurfer::fs_subj_dir(),
                            output_dir = subjects_dir,
                            verbose = TRUE){
  
  if(!dir.exists(output_dir)) dir.create(output_dir, recursive = TRUE)
  
  # loop through subjects
  for(sub in subject){
    
    # Simplify a bit with a shorter variable
    dir <- file.path(output_dir, sub)
    sdir <- file.path(subjects_dir, sub)
    
    # Where to store the outputs
    if(!dir.exists(file.path(dir, "ascii"))) dir.create(file.path(dir, "ascii"), recursive = TRUE)
    
    if(file.access(dir, 2) != 0){
      stop(paste("No write permissions to \n",
                 output_dir, "\nMake sure you have the correct permissions."),
           call. = FALSE
      )
    }
    
    # run surfaces ----
    surfs <- surf_list(hemisphere, surfaces)
    
    surfs <- sapply(surfs, function(x)
      surf2asc(input_file = file.path(sdir, "surf", x),
               output_file = file.path(dir, "ascii", paste(x, "srf", sep=".")),
               verbose = verbose)
    )
    
    # run curvatures ----
    curvs <- surf_list(hemisphere, curvatures)
    curvs <- sapply(curvs, function(x)
      curv2asc(input_file = file.path(sdir, "surf", x),
               output_file = file.path(dir, "ascii", paste(x, "dpv", sep=".")),
               white = file.path(sdir, "surf", paste(strsplit(x, "\\.")[[1]][1], "white", sep = ".")),
               verbose = verbose)
    )
    
    # run no fix curvatures ----
    curvsnf <- surf_list(hemisphere, nofix_curv)
    curvsnf <- sapply(curvsnf, function(x)
      curvnf2asc(input_file = file.path(sdir, "surf", x),
                 output_file = file.path(dir, "ascii", paste(x, "dpv", sep=".")),
                 nofix = file.path(sdir, "surf", paste(strsplit(x, "\\.")[[1]][1], "orig.nofix", sep = ".")),
                 verbose = verbose)
    )
    
  } # end sub
}

