# Cortical ----
#' Converts annotations fo atlas
#' 
#' This function will create an atlas
#' ready data.frame for ggseg3d to
#' plot with plotly. 
#' 
#' 
#' Based on A. Winkler scripts 
#' 
#' @template subject
#' @template hemisphere
#' @param surface surface from subjects surf-folder
#' @param annot base-name of annot file
#' @template subjects_dir
#' @template annot_dir
#' @template output_dir
#' @template cleanup
#' @template verbose
#' @export
#' @return data frame with one row per label
#' 
#' @examples 
#' \dontrun{
#' dt <- aparc_2_mesh()
#' dt <- aparc_2_mesh(surface = "white")
#' dt <- aparc_2_mesh(hemisphere = "lh")
#' dt <- aparc_2_mesh(annot = "aparc.a2009s")
#' }
#' 
aparc_2_mesh <- function(subject = "fsaverage5",
                          hemisphere = "rh",
                          surface = "inflated",
                          annot = "aparc",
                          subjects_dir = freesurfer::fs_subj_dir(),
                          annot_dir = file.path(subjects_dir, subject, "label"),
                          output_dir = tempdir(),
                          cleanup = TRUE,
                          verbose = TRUE
){
  
  fs <- check_fs()
  if(!fs) stop(call. = FALSE)
  
  # Setup some directories for output data  
  dirs <- sapply(c(paste("atlas", annot, c("ascii", "ply"), sep="/"), "srf/ascii", "srf/ply"), 
                 function(x) file.path(output_dir, subject, x))
  j <- lapply(dirs[!sapply(dirs, dir.exists)], dir.create, recursive = TRUE)
  
  # check hemisphere information
  hemisphere <- match.arg(hemisphere, c("rh", "lh"))
  hemi <- switch(hemisphere, "rh" = "right", "lh" = "left")
  
  # find path to annotation file
  annot_file <- list.files(annot_dir, paste(hemisphere, annot, "annot", sep="."), full.names = T)
  
  # Read in annotation file, verbose false since annot2dpv() also spits out this information
  ant <- read_annotation(annot_file, verbose = FALSE)
  colortable <- dplyr::mutate(ant$colortable,
                              hex = grDevices::rgb(R, G, B, 
                                                   maxColorValue = 255)
  )
  
  # convert annotation to dpv files
  dpx <- annot2dpv(annot_file,
                   file.path(dirs[1], paste(hemisphere, annot, "dpv", sep=".")),
                   verbose = verbose)
  
  # Locate surface file
  if(surface == "LCBC" &  subject == "fsaverage5"){
    surf_file <- system.file("surfaces", paste0(hemisphere, ".LCBC"), package = "ggsegExtra")
    if(verbose) cat("Using surface file:", surf_file, "\n")
  }else{
    surf_file <- file.path(subjects_dir, subject, "surf", paste(hemisphere, surface, sep="."))
  }
  
  if(!file.exists(surf_file)){
    cat("cannot find specified surface file for this subject.\n",
        "Please check file path for surface:\n",
        surf_file, "\n")
    stop(call. = FALSE)
  }
  
  # convert surface file to ascii
  surf <- surf2asc(surf_file,
                   file.path(dirs[3], paste(hemisphere, surface, "dpv", sep=".")), 
                   verbose = verbose)
  
  # make entire brain ply
  ply <- asc2ply(file.path(dirs[3], paste(hemisphere, surface, "dpv", sep=".")),
                 file.path(dirs[4], paste(hemisphere, surface, "ply", sep=".")))
  
  # split into labels
  plys <- surfsplit(
    srf_ply = file.path(dirs[4], paste(hemisphere, surface, "ply", sep=".")), 
    label_path = file.path(dirs[1], paste(hemisphere, annot, "dpv", sep=".")), 
    prefix = annot, 
    output_dir = file.path(output_dir, subject, "atlas", annot), 
    verbose = verbose)
  
  if(length(plys) != nrow(colortable)){
    # Find any color label _not_ in label vector
    lab_not_present <- sapply(colortable$code, function(x) any(x %in% ant$label))
    colortable <- colortable[lab_not_present,]
  }
  
  dt <- dplyr::tibble(
    atlas = annot,
    surf = surface,
    hemi = hemi, 
    region = colortable$label,
    colour = grDevices::rgb(colortable$R, colortable$G, colortable$B, maxColorValue = 255),
    label = paste(hemisphere, colortable$label, sep="_"),
    roi = sprintf("%04d", 1:nrow(colortable)),
    annot = colortable$label
  )
  
  dt$mesh <- lapply(dt$roi, function(x) plys[[which(names(plys) == x)]])
  
  # Make medial wall and unknown colour NA
  dt$colour[grepl("wall|unknown", dt$label, ignore.case = TRUE)] <- NA
  
  if(cleanup){
    if(verbose) cat(paste0("\nCleaning up files in:", output_dir, "\n"))
    sapply(dirs, unlink, recursive = TRUE)
  }
  
  return(dt)
}


#' Create cortical ggseg3d-atlas from annot-file
#' 
#' Function loops through hemispheres and surfaces
#' to create a data frame that is ready to use with 
#' ggseg3d. 
#' 
#' It is recommended that you change the region names
#' for the atlas, and the atlas name to something shorter.
#' See the dk_3d atlas for examples.
#'
#' @param annot annotation file, with name [hemi].[annot].annot abd be in annot_dir
#' @template subject 
#' @template hemisphere 
#' @param surface Freesurfer surface
#' @template subjects_dir 
#' @template annot_dir 
#' @template output_dir 
#' @template cleanup 
#' @template verbose 
#'
#' @return nested data frame as ggseg3d-atlas
#' @export
#'
#' @examples
#' \dontrun{
#' dt <- aparc_2_3datlas()
#' dt <- aparc_2_3datlas(annot = "aparc.a2009s")
#' dt <- aparc_2_3datlas(surface = "sphere")
#' }
make_aparc_2_3datlas <- function(annot = "aparc",
                             subject = "fsaverage5",
                             hemisphere = c("rh", "lh"),
                             surface = c("inflated", "LCBC", "white"),
                             subjects_dir = freesurfer::fs_subj_dir(),
                             annot_dir = file.path(subjects_dir, subject, "label"),
                             output_dir = tempdir(),
                             cleanup = TRUE,
                             verbose = TRUE
){
  
  dt <- list()
  k <- 0
  j <- length(surface) * length(hemisphere)
  for(s in surface){
    for(h in hemisphere){
      k <- k + 1
      
      if(verbose) cat(crayon::cyan("\n\n", paste0(k, "/", j), 
                                   "Extracting information from", 
                                   h, s, "for", annot, "\n"))
      
      dt[[k]] <- aparc_2_mesh(subject = subject,
                               hemisphere = h,
                               surface = s,
                               annot = annot,
                               subjects_dir = subjects_dir,
                               annot_dir = annot_dir,
                               output_dir = output_dir,
                               cleanup = cleanup,
                               verbose = verbose
      )
      
    } #for h
  } # for s
  
  dt <- dplyr::bind_rows(dt)
  dt <- dplyr::group_by(dt, atlas, surf, hemi)
  dt <- tidyr::nest(dt)
  dt <- dplyr::rename(dt, ggseg_3d = data)
  dt <- dplyr::ungroup(dt)
  dt$atlas <- paste0(dt$atlas, "_3d")
  return(dt)
}

# Subcortical ----
aseg_2_mesh <- function(subject = "fsaverage5",
                           subjects_dir = freesurfer::fs_subj_dir(),
                           label = 0,
                           template = file.path(subjects_dir, subject, "mri/aseg.mgz"),
                           steps = 1:5,
                           output_dir = tempdir(),
                           verbose = TRUE
){
  
  fs <- check_fs()
  if(!fs) stop(call. = FALSE)
  
  if(!any(steps %in% 1:5)) stop("steps must be numbers between 1 and 5.\n", call. = FALSE)
  
  annot <- gsub("\\.mgz", "", basename(template))
  
  # create output dirs
  dirs <- file.path(output_dir, subject, "atlas", annot, c("ascii", "ply", "surf"))
  k <- sapply(dirs, dir.exists)
  j <- sapply(dirs[!k], dir.create, recursive = TRUE, showWarnings = FALSE)
  
  if(any(!k) & length(j) == 0) 
    stop(paste("Unable to create output directories. Check if output directory parent is writeable.\n",
               "output_dir is set to:", output_dir), call. = FALSE)
  
  lab_string <- sprintf("%04d", label)
  
  if(1 %in% steps){
    # pretesselate
    mri_pretess(template,
                label,
                file.path(dirs[3], paste(lab_string, "filled.mgz", sep="_")),
                verbose = verbose)
  } #end 1
  
  
  if(2 %in% steps){
    # tesselate
    mri_tessellate(file.path(dirs[3], paste0(lab_string, "_filled.mgz")),
                   label,
                   file.path(dirs[3], paste0(lab_string, "_notsmooth")),
                   verbose = verbose)
  } #end 2
  
  if(3 %in% steps){
    # smoothing
    mri_smooth(file.path(dirs[3], paste0(lab_string, "_notsmooth")), 
               label,
               file.path(dirs[3], lab_string),
               verbose = verbose)
    k <- file.rename(file.path(dirs[3], paste0("rh.", label)),
                     file.path(dirs[3], paste(lab_string, "smooth", sep="_")))
  } #end 3
  
  if(4 %in% steps){
    # to ascii
    k <- surf2asc(file.path(dirs[3], paste(lab_string, "smooth", sep="_")),
                  file.path(dirs[1], paste0(lab_string, ".dpv")),
                  verbose = verbose)
  } # end 4
  
  if(5 %in% steps){
    # to ply
    k <- asc2ply(file.path(dirs[1], paste0(lab_string, ".dpv")),
                 file.path(dirs[2], paste0(lab_string, ".ply"))
    )
  } # end 5
  
  if(verbose) cat("Reading in mesh information\n")
  ply <- get_mesh(file.path(dirs[2], paste0(lab_string, ".ply")), 
                  ShowSpecimen = FALSE)
  return(ply)
}

#' Subcortical segmentation to atlas
#'
#' @template subject 
#' @template subjects_dir 
#' @param template tempalte volume.mgz file path
#' @param color_lut Freesurfer colour look-up-table. Either
#' as a path or a data.frame that \code{\link{is_ctab}}
#' @param steps if cleanup is disabled, all files are
#'  saved, and steps can be re-run individually
#' @template output_dir 
#' @template verbose 
#' @template cleanup 
#'
#' @return returns a ggseg3d-atlas ready object
#' @export
make_aseg_2_3datlas <- function(subject = "fsaverage5",
                              subjects_dir = freesurfer::fs_subj_dir(),
                              template = file.path(subjects_dir, subject, "mri/aseg.mgz"),
                              color_lut = file.path(freesurfer::fs_dir(), "ASegStatsLUT.txt"),
                              steps = 1:5,
                              output_dir = tempdir(),
                              verbose = TRUE,
                              cleanup = TRUE)
{
  
  # Make basename for atlas
  annot <- gsub("\\.mgz", "", basename(template))
  
  # Fins unique labels from template
  labels <- unique(c(freesurfer::readmgz(template)))
  
  # Specify dirs
  dirs <- file.path(output_dir, subject, "atlas", annot, c("ascii", "ply", "surf"))
  
  # If there is a LUT supplied,
  # get it, reduce labels to only those in the LUT
  if(!is.null(color_lut)){
    colortable <- get_ctab(color_lut)
    colortable$roi <- sprintf("%04d", colortable$idx)
    
    # Because LUTS _may_ contain rois with no vertices
    # Reduce to label overlap within the template file
    colortable <- colortable[colortable$idx %in% labels, ]
    
    labels <- colortable$idx
  }else{
    # Make a color LUT based on already existing FS LUT
    colortable <- get_ctab(file.path(freesurfer::fs_dir(), "FreeSurferColorLUT.txt"))
    colortable <- colortable[1:length(labels), ]
    colortable$label <- labels
  }
  
  n <- length(labels)
  plys <- list()
  for(j in 1:n){
    
    lab_string <- sprintf("%04d", labels[j])
    
    if(verbose) cat(crayon::cyan("\n\n", paste0(j, "/", n), 
                                 "Extracting information from", 
                                 lab_string, "for", annot, "\n"))
    
    plys[[j]] <- aseg_2_mesh(subject = subject,
                   subjects_dir = subjects_dir,
                   label = labels[j],
                   template = template,
                   steps = steps,
                   output_dir = output_dir,
                   verbose = verbose
    )
    
  } # end j
  
  
  names(plys) <-  sprintf("%04d", labels)
  
  dt <- dplyr::tibble(
    atlas = annot,
    surf = "LCBC",
    hemi = "subcort", 
    region = colortable$label,
    colour = grDevices::rgb(colortable$R, colortable$G, colortable$B, maxColorValue = 255),
    label =  colortable$label,
    roi =  sprintf("%04d", labels)
  )
  
  dt$mesh <- lapply(dt$roi, function(x) plys[[which(names(plys) == x)]])
  
  if(cleanup){
    if(verbose) cat(paste0("Cleaning up files in:", output_dir, "\n"))
    j <- sapply(dirs, unlink, recursive = TRUE)
  }
  
  dt <- dplyr::bind_rows(dt)
  dt <- dplyr::group_by(dt, atlas, surf, hemi)
  dt <- tidyr::nest(dt)
  dt <- dplyr::rename(dt, ggseg_3d = data)
  dt <- dplyr::ungroup(dt)
  dt$atlas <- paste0(dt$atlas, "_3d")
  return(dt)
}


## quiets concerns of R CMD check
if(getRversion() >= "2.15.1"){
  utils::globalVariables(c("atlas", "surf", "data",
                           "hemi", "ggseg_3d",
                           "R", "G", "B", "A"))
}
