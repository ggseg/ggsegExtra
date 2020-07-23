# Image conversions and manipulations ----

#' Convert volume to surface
#'
#' @param input_file input volume
#' @template output_file
#' @template hemisphere
#' @param projfrac argument to projfrac
#' @template verbose
#'
#' @export
mri_vol2surf <- function(input_file , 
                         output_file,
                         hemisphere,
                         projfrac = .5,
                         verbose = TRUE){
  
  fs <- check_fs()
  if(!fs) stop(call. = FALSE)
  
  fs_cmd <- paste0(freesurfer::get_fs(),
                   "mri_vol2surf")
  
  cmd <- paste(fs_cmd,
               "--mov", input_file,
               "--o", output_file,
               "--mni152reg",
               "--hemi", hemisphere,
               "--projfrac", projfrac)
  
  k <- system(cmd, intern=!verbose)
  invisible(k)
}

#' Convert volume to label
#'
#' Converts values in a volume or surface overlay to a label. The program
#' searches the input for values equal to labelid. The xyz values for
#' each point are then computed based on the tkregister voxel-to-RAS
#' matrix (volume) or from the xyz of the specified surface.  The xyz
#' values are then stored in labelfile in the label file format. The
#' statistic value is set to 0.  While this program can be used with any
#' mri volume, it was designed to convert parcellation volumes, which
#' happen to be stored in mri format. 
#' 
#' 
#' @param input_file input volume
#' @param label_id label to run
#' @template hemisphere
#' @template subject
#' @param surface ouput surface
#' @template subjects_dir
#' @template output_dir
#' @template verbose
#'
#' @export
mri_vol2label <- function(input_file, 
                          label_id,
                          hemisphere, 
                          surface = NULL,
                          subject = "fsaverage5",
                          subjects_dir = freesurfer::fs_subj_dir(),
                          output_dir, 
                          verbose = TRUE){
  fs <- check_fs()
  if(!fs) stop(call. = FALSE)
  
  hemisphere <- match.arg(hemisphere, c("rh", "lh"))
  
  if(!dir.exists(output_dir)) dir.create(output_dir, recursive = TRUE)
  
  output_file <- paste0(output_dir, "/", hemisphere, "_", 
                        stringr::str_pad(label_id, 4, side="left", pad="0"), 
                        ".label")
  
  fs_cmd <- paste0(freesurfer::get_fs(),
                   "mri_vol2label")

  cmd <-  paste(fs_cmd,
                "--c", input_file,
                "--id", label_id,
                "--sd", subjects_dir, 
                "--l", output_file)
  
  if(!is.null(surface)){
    cmd <- paste(cmd, "--surf", subject, hemisphere,)
  }
  
  k <- system(cmd, intern=!verbose)
  
  invisible(k)
}


#' Run pre-tesselation on file
#'
#' @param template template mgz
#' @param label label to run
#' @template output_file
#' @template verbose
mri_pretess <- function(template, label, output_file, verbose = TRUE){
  fs <- check_fs()
  if(!fs) stop(call. = FALSE)
  
  fscmd <- paste0(freesurfer::get_fs(), "mri_pretess")
  
  cmd <- paste(fscmd,
               template, label,
               template,
               output_file)
  
  k <- system(cmd, intern = !verbose)
}

#' Tesselate data
#'
#' @param label label to run
#' @template verbose
#' @template output_file
#' @param input_file input file
mri_tessellate <- function(input_file, label, output_file, verbose){
  fs <- check_fs()
  if(!fs) stop(call. = FALSE)
  
  fscmd <- paste0(freesurfer::get_fs(), "mri_tessellate")
  
  cmd <- paste(fscmd,
               input_file, 
               label,
               output_file
  )
  
  k <- system(cmd, intern = !verbose)
}

#' Smooth data
#'
#' @param input_file input file to smooth
#' @param label label to run
#' @template output_file
#' @template verbose
mri_smooth <- function(input_file, label, output_file, verbose){
  fs <- check_fs()
  if(!fs) stop(call. = FALSE)
  
  fscmd <- paste0(freesurfer::get_fs(), "mris_smooth")
  
  cmd <- paste(fscmd, "-nw",
               input_file, 
               label,
               output_file
  )
  
  k <- system(cmd, intern = !verbose)
}


#' Convert Label to Annotation
#'
#' @param labels label vector
#' @template hemisphere
#' @param ctab colourtable file
#' @template subject
#' @template subjects_dir
#' @template annot_dir
#' @template output_dir
#' @template verbose
#'
#' @export
mris_label2annot <- function(labels, 
                             hemisphere = "rh", 
                             ctab, 
                             subject = "fsaverage5",
                             subjects_dir = freesurfer::fs_subj_dir(),
                             annot_dir = file.path(subjects_dir, subject, "label"),
                             output_dir = subjects_dir,
                             verbose = TRUE){
  fs <- check_fs()
  if(!fs) stop(call. = FALSE)
  
  hemisphere <- match.arg(hemisphere, c("rh", "lh"))
  
  if(!dir.exists(output_dir)) dir.create(output_dir, recursive = TRUE)
  # out_file <- file.path(output_dir, hemisphere)
  
  labs <- paste("--l", labels, collapse=" ")
  fs_cmd <- paste0(freesurfer::get_fs(),
                   "mris_label2annot")
  
  cmd <-  paste(fs_cmd,
                "--sd" , subjects_dir,
                "--s", subject,
                "--ctab", ctab,
                labs,
                "--h", hemisphere, 
                "--annot-path", output_dir
                # "--a tmp"
  )
  
  k <- system(cmd, intern=!verbose)
  
  invisible(k)
}

#' Convert annotation to label
#' 
#' @param annot_file annotation file path
#' @template subject
#' @template hemisphere
#' @template output_dir
#' @template verbose
mris_annot2label <- function(annot_file, 
                             subject = "fsaverage5",
                             hemisphere = "lh", 
                             output_dir = freesurfer::fs_subj_dir(), 
                             verbose = TRUE){
  fs <- check_fs()
  if(!fs) stop(call. = FALSE)
  
  hemisphere <- match.arg(hemisphere, c("rh", "lh"))
  
  outdir <- file.path(output_dir, subject, "label", 
                      gsub("rh\\.|lh\\.|annot|\\.", "", basename(annot_file)))
  if(!dir.exists(outdir)) dir.create(outdir, recursive = TRUE)
  out_file <- file.path(outdir, hemisphere)
  
  fs_cmd <- paste0(freesurfer::get_fs(),
                   "freeview")
  
  cmd <- paste(
    fs_cmd,
    "--subject", subject,
    "--hemi", hemisphere,
    "--labelbase", file.path(outdir, hemisphere),
    "--annotation", annot_file
  )
  
  k <- system(cmd, intern=!verbose)
  
}


#' MRI ca label
#' 
#' For a single subject, produces an annotation file, in which each 
#' cortical surface vertex is assigned a neuroanatomical label.
#' This automatic procedure employs data from a previously-prepared 
#' atlas file. An atlas file is created from a training set, capturing 
#' region data manually drawn by neuroanatomists combined with 
#' statistics on variability correlated to geometric information 
#' derived from the cortical model (sulcus and curvature). Besides the 
#' atlases provided with FreeSurfer, new ones can be prepared using 
#' mris_ca_train).
#'
#' @template  subject 
#' @template hemisphere 
#' @param canonsurf canonical surface file. Ie: the name of the 
#' spherical surface file which describes the registration of a 
#' subject's vertices to the reference "average" surface. Example: sphere.reg
#' @param classifier specify classifier array input file (atlas file)
#' @template output_file 
#' @param opts other options to freesurfer function mris_ca_label
#' @template subjects_dir
#'
#' @export
#'
#' @examples
#' if(freesurfer::have_fs()){
#' # for freesurfer help see:
#' freesurfer::fs_help("mris_ca_label")
#' }
mris_ca_label <- function(subject = "fsaverage5",
                         hemisphere = "lh", 
                         canonsurf  ="sphere.reg",
                         classifier = file.path(freesurfer::fs_dir(), "average/lh.DKTatlas40.gcs"),
                         output_file, 
                         subjects_dir = freesurfer::fs_subj_dir(),
                         opts = NULL){
  
  fs <- check_fs()
  if(!fs) stop(call. = FALSE)
  
  if(!is.null(opts)){
    opts <- paste("-sdir", subjects_dir, opts)
  }else{
    opts <- paste("-sdir", subjects_dir)
  }
  
  fs_cmd <- paste0(freesurfer::get_fs(), "mris_ca_label")
  
  necs <- paste(subject, hemisphere, canonsurf, classifier, output_file)
  
  cmd <- paste(fs_cmd, opts, necs)
  
  jj <- system(cmd, intern = TRUE)
  invisible(jj)
  
}

# 2 asc ----

#' Convert Freesurfer surface file to ascii
#' 
#' Surface files from Freesurfer need to be
#' conserted to be handled in R. This first
#' step turns the surface file in to a text
#' ascii file.
#'
#' @param input_file path to input surface file to convert
#' @template output_file
#' @template verbose
#'
#' @return ascii data
#' @export
surf2asc <- function(input_file, output_file, verbose = TRUE){
  fs <- check_fs()
  if(!fs) stop(call. = FALSE)
  
  k <- strsplit(output_file, "\\.")[[1]]
  if(k[length(k)] != "dpv"){
    cat("output_file must end with '.dpv'")
    stop(call.=FALSE)
  }
  
  fscmd <- paste0(freesurfer::get_fs(), "mris_convert")
  
  if(!file.exists(input_file)){
    if(verbose) cat(paste0("Inputfile does not exist. Check file path:\n",
                           input_file))
    return()
  }
  
  cmd <-  paste(fscmd,
                input_file,
                gsub("\\.dpv", "\\.asc", output_file)
  )
  j <- system(cmd, intern = FALSE)
  
  if(verbose) cat(paste("Saving", output_file, "\n"))
  
  j <- file.rename(gsub("\\.dpv", "\\.asc", output_file),
                   output_file)
  
  read_dpv(output_file)
}

#' Convert Freesurfer curvature file to ascii
#' 
#' Curvature files from Freesurfer need to be
#' conserted to be handled in R. This first
#' step turns the curvature file in to a text
#' ascii file.
#' 
#' @param input_file path to curvature file
#' @param white path to subjects hemi.white file
#' @template output_file
#' @template verbose
#'
#' @return ascii data
#' @export
curv2asc <- function(input_file, white, output_file, verbose = TRUE){
  fs <- check_fs()
  if(!fs) stop(call. = FALSE)
  
  k <- strsplit(output_file, "\\.")[[1]]
  if(k[length(k)] != "dpv"){
    cat("output_file must end with '.dpv'")
    stop(call.=FALSE)
  }
  
  if(!file.exists(input_file)){
    if(verbose) cat(paste0("Inputfile does not exist. Check file path:\n",
                           input_file))
    return()
  }
  
  fscmd <- paste0(freesurfer::get_fs(), "mris_convert -c")
  
  cmd <-  paste(fscmd,
                input_file,
                white,
                gsub("\\.dpv", "\\.asc", output_file)
  )
  j <- system(cmd, intern=!verbose)
  
  if(verbose) cat(paste("Saving", output_file, "\n"))
  j <- file.rename(gsub("\\.dpv", "\\.asc", output_file), output_file)
  
  read_dpv(output_file)
}

#' Convert Freesurfer no fix curvature file to ascii
#' 
#' No fix curvature files from Freesurfer need to be
#' conserted to be handled in R. This first
#' step turns the no fix curvature file in to a text
#' ascii file.
#' 
#' @param input_file path to nofix curvature file
#' @param nofix path to subjects hemi.orig.nofix file
#' @template output_file
#' @template verbose
#' 
#' @return ascii data
#' @export
curvnf2asc <- function(input_file, nofix, output_file, verbose = TRUE){
  fs <- check_fs()
  if(!fs) stop(call. = FALSE)
  
  k <- strsplit(output_file, "\\.")[[1]]
  if(k[length(k)] != "dpv"){
    cat("output_file must end with '.dpv'")
    stop(call.=FALSE)
  }
  
  fscmd <- paste0(freesurfer::get_fs(), "mris_convert -c")
  
  if(!file.exists(input_file)){
    if(verbose) cat(paste0("Inputfile does not exist. Check file path:\n",
                           input_file))
    return()
  }
  
  cmd <-  paste(fscmd,
                input_file,
                nofix, 
                gsub("\\.dpv", "\\.asc", output_file)
  )
  j <- system(cmd, intern=!verbose)
  
  if(verbose) cat(paste("Saving", output_file, "\n"))
  j <- file.rename(gsub("\\.dpv", "\\.asc", output_file), output_file)
  
  read_dpv(output_file)
}


#' Convert annotation file to dpv
#' 
#' Based on matlab scripts from
#' Anderson M. Winkler
#' Yale University / Institute of Living
#' Oct/2011
#' http://brainder.org
#'
#' @param input_file annotation file path
#' @template output_file
#' @param coordinates contains the vertex coordinates or face indices
#' @param indeces vertex or face sequential index
#' @template verbose
#'
#' @return data frame
#' @export
annot2dpv <- function(input_file, 
                      output_file,
                      coordinates = NULL,
                      indeces = NULL,
                      verbose = TRUE){
  
  annot <- read_annotation(input_file, verbose = verbose)
  
  # For each structure, replace its coded colour by its index
  labs <- match(annot$label, annot$colortable$code)
  
  if(all(unlist(lapply(labs, is.integer)))){
    fstr = '%d';
  } else{
    fstr = '%0.10f'
  }
  
  if(!all(is.null(coordinates) & is.null(indeces))){
    # Organise the coords
    if(dim(coordinates)[1] < dim(coordinates)[2]) {
      coordinates = t(coordinates)
    }
    
    # Prepare to save
    dpx = cbind(indeces,  coordinates, labs)
    
  } else {
    
    dpx = data.frame(
      idx = 0:(length(labs)-1),
      V1 = 0,
      V2 = 0, 
      V3 = 0,
      labs = labs
    )
  }
  
  dpx <- within(dpx,  l <- sprintf(paste('%d %g %g %g', fstr), idx, V1, V2, V3, labs))
  
  # Save
  con <- file(output_file)
  on.exit(close(con))
  
  writeLines(unlist(dpx$l), con = con)
  
  utils::read.table(output_file)
}

## 2 ply ----
#' Convert ascii to .ply
#' 
#' ggseg3d bases its functions on data
#' from .ply files. To turn Freesurfer 
#' surface or curvature based ascii files
#' into ply, this is the function you need.
#'
#' @param input_file input surface or curvature file made with 
#' \code{\link{surf2asc}}, \code{\link{curv2asc}}, or \code{\link{curvnf2asc}}
#' @template output_file
#'
#' @return ply text file
#' @export
asc2ply <- function(input_file, 
                    output_file = gsub("\\.dpv", ".ply", input_file)){
  srf_file <- readLines(input_file)
  
  nfo <- as.numeric(strsplit(srf_file[2], " ")[[1]])
  names(nfo) <- c("vertex", "face")
  
  srf_file <- srf_file[c(-1, -2)]
  srf_data <- utils::read.table(text = srf_file)
  
  vert <- srf_data[1:nfo["vertex"],1:3]
  vert <- unname(apply(vert, 1, paste, collapse = " "))
  
  face <- cbind(3, srf_data[(nfo["vertex"]+1):nrow(srf_data),1:3])
  face <- unname(apply(face, 1, paste, collapse = " "))
  
  ply_head <- c(
    "ply",
    "format ascii 1.0",
    paste("element vertex", nfo["vertex"]),
    "property float x",
    "property float y",
    "property float z",
    paste("element face", nfo["face"]),
    "property list uchar int vertex_index",
    "end_header"
  )
  
  
  ply <- c(ply_head, vert, face)
  
  con <- file(output_file)
  on.exit(close(con))
  writeLines(ply, con)
  
  return(ply)
}


#' Convert Freesurfer surface file into ply
#' 
#' Function to convert Freesurfer surface
#' file into .ply
#'
#' @param input_file path to Freesurfer surface file
#' @template output_file
#' @template verbose
#'
#' @return ply text
#' @export
surf2ply <- function(input_file,
                     output_file = paste(input_file, ".ply"),
                     verbose = TRUE){
  
  basefile <- gsub("\\.ply", "", output_file)
  
  srf <- surf2asc(input_file, paste0(basefile, ".asc"), FALSE)
  ply <- asc2ply(paste0(basefile, ".asc"), output_file)
  
  return(ply)
}

#' Convert Freesurfer curvature file to ply
#' 
#' Function to convert Freesurfer curvature
#' file into .ply
#'
#' @param input_file path to Freesurfer curvature file
#' @template verbose
#' @template output_file
#' 
#' @return .ply text
#' @export
curv2ply <- function(input_file,
                     output_file = paste(input_file, ".ply"),
                     verbose = TRUE){
  
  basefile <- gsub("\\.ply", "", output_file)
  
  srf <- curv2asc(input_file, paste0(basefile, ".asc"), FALSE)
  ply <- asc2ply(paste0(basefile, ".asc"), output_file)
  
  return(ply)
}

# other ----
#' Turn smooth file to ascii
#'
#' @param input_file input file path
#' @template output_file
#' @template verbose
smooth2srf <- function(input_file, output_file, verbose){
  
  fs <- check_fs()
  if(!fs) stop(call. = FALSE)
  
  k <- strsplit(output_file, "\\.")[[1]]
  if(k[length(k)] != "srf"){
    cat("output_file must end with '.srf'")
    stop(call.=FALSE)
  }
  
  fscmd <- paste0(freesurfer::get_fs(), "mris_convert")
  
  cmd <- paste(fscmd, 
               input_file, 
               gsub("\\.srf", "\\.asc", output_file)
  )
  
  k <- system(cmd, intern = !verbose)
  
  k <- file.rename(gsub("\\.srf", "\\.asc", output_file),
                   output_file)
}

#' Convert LCBC surface file to other subjects
#'
#' @param input_volume path to input volume
#' @param source_subject source subject
#' @param target_subject target subject
#' @template hemisphere 
#' @template subjects_dir 
#' @template output_dir 
#' @param cortex toggle "--cortex" (TRUE) or "--no-cortex" (FALSE)
#' @template verbose 
lcbc_surf2surf <- function(
  input_volume,
  source_subject = "fsaverage", 
  target_subject = "fsaverage5", 
  hemisphere = "rh",
  subjects_dir = freesurfer::fs_subj_dir(),
  output_dir = file.path(subjects_dir, target_subject, "surf"),
  cortex = TRUE,
  verbose = TRUE
){
  fs <- check_fs()
  if(!fs) stop(call. = FALSE)
  
  j <- freesurfer::mri_surf2surf(
    sval = input_volume,
    subject = source_subject,
    target_subject = target_subject,
    hemi = hemisphere,
    outfile = paste0(output_dir, hemisphere, ".lcbc"), 
    subj_dir = subjects_dir, 
    # opts = ifelse(cortex, "--cortex", "--no-cortex"),
    verbose = verbose
  )
}


#' Check if FS can be run
#' @param msg message to print on error
#' @return logical
check_fs <- function(msg = NULL){
  
  if(is.null(msg)) 
    msg <- paste0("System does not have Freesurfer or Freesurfer has not been setup correctly.\n",
                  "Aborting.\n")
  if(!freesurfer::have_fs()){
    cat(crayon::red(msg))
  }
  
  freesurfer::have_fs()
}

fs_ss_slice <- function(lab, x, y, z, view, subjects_dir, subject, output_dir) {
  coords <- sprintf(c(x, y, z), fmt = "%03d")
  vv <- paste0(strsplit(view, "")[[1]][1:5], collapse="")
  
  filenm <- paste0(paste(c(coords, vv), collapse="_"), "_", basename(lab), ".png")
  
  fs_cmd <- paste0(freesurfer::get_fs(), "freeview")
  
  cmd <- paste(fs_cmd,
               "--volume", paste0(file.path(subjects_dir, subject, "mri/T1.mgz"), ":opacity=0"),
               "--slice", paste0(c(x, y, z), collapse=' '),
               "--viewport", view,
               paste0("--label ", lab, ":color=red"),
               "-ss", file.path(output_dir, filenm),
               "") 
  
  jj <- system(cmd, intern = TRUE)
  invisible(jj)
}



## quiets concerns of R CMD check
if(getRversion() >= "2.15.1"){
  utils::globalVariables(c("idx", 
                           "key", "view", "val", "vars",
                           paste0("V", 1:3)))
}


