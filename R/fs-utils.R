# Image conversions and manipulations ----

#' Convert volume to surface
#'
#' @param input_file input volume
#' @template output_file
#' @template hemisphere
#' @param projfrac argument to projfrac
#' @template verbose
#' @template opts
#' @importFrom freesurfer get_fs
#' @noRd
mri_vol2surf <- function(input_file , 
                         output_file,
                         hemisphere,
                         projfrac = .5,
                         opts = NULL,
                         verbose = TRUE){
  
  if(!check_fs()) stop(call. = FALSE)
  
  fs_cmd <- paste0(get_fs(),
                   "mri_vol2surf")
  
  if(!is.null(opts)) fs_cmd <- paste0(fs_cmd, opts)
  
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
#' Calls FreeSurfer's \code{mri_vol2label}.
#' 
#' 
#' @param input_file input volume
#' @param label_id label to run
#' @template hemisphere
#' @template subject
#' @param surface output surface
#' @template subjects_dir
#' @template output_dir
#' @template verbose
#' @template opts
#' @return returns nothing. Writes a label file.
#' @importFrom freesurfer get_fs fs_subj_dir
#' @importFrom stringr str_pad
#' @export
#' @examples 
#' if(freesurfer::have_fs()){
#' # for freesurfer help see:
#' freesurfer::fs_help("mri_vol2label")
#' 
#' out_dir <- tempdir()
#' vol <- file.path(freesurfer::fs_subj_dir(),
#'                  "fsaverage5/mri/aseg.mgz")
#' 
#' mri_vol2label(vol, 
#'      label_id = 2, 
#'      hemisphere = "rh", 
#'      output_dir = out_dir)
#'  
#'  # delete output dir when not needed
#'  unlink(out_dir)
#' }
mri_vol2label <- function(input_file, 
                          label_id,
                          hemisphere, 
                          output_dir, 
                          surface = NULL,
                          subject = "fsaverage5",
                          subjects_dir = fs_subj_dir(),
                          opts = NULL,
                          verbose = TRUE){
  if(!check_fs()) stop(call. = FALSE)
  
  hemisphere <- match.arg(hemisphere, c("rh", "lh"))
  
  if(!dir.exists(output_dir)) dir.create(output_dir, recursive = TRUE)
  
  output_file <- paste0(output_dir, "/", hemisphere, "_", 
                        str_pad(label_id, 4, side="left", pad="0"), 
                        ".label")
  
  fs_cmd <- paste0(get_fs(),
                   "mri_vol2label")
  
  if(!is.null(opts)) fs_cmd <- paste0(fs_cmd, opts)
  
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
#' @template opts
#' @importFrom freesurfer get_fs
#' @noRd
mri_pretess <- function(template, label, output_file, verbose = TRUE, opts = NULL){
  if(!check_fs()) stop(call. = FALSE)
  
  fscmd <- paste0(get_fs(), "mri_pretess")
  
  if(!is.null(opts)) fs_cmd <- paste0(fs_cmd, opts)
  
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
#' @template opts
#' @importFrom freesurfer get_fs
#' @noRd
mri_tessellate <- function(input_file, label, output_file, verbose, opts = NULL){
  if(!check_fs()) stop(call. = FALSE)
  
  fscmd <- paste0(get_fs(), "mri_tessellate")
  if(!is.null(opts)) fs_cmd <- paste0(fs_cmd, opts)
  
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
#' @template opts
#' @importFrom freesurfer get_fs
#' @noRd
mri_smooth <- function(input_file, label, output_file, verbose, opts = NULL){
  if(!check_fs()) stop(call. = FALSE)
  
  fscmd <- paste0(get_fs(), "mris_smooth")
  if(!is.null(opts)) fs_cmd <- paste0(fs_cmd, opts)
  
  cmd <- paste(fscmd, "-nw",
               input_file, 
               label,
               output_file
  )
  
  k <- system(cmd, intern = !verbose)
}


#' Convert Label to Annotation
#' 
#' If you have labels rather than a full annotation
#' file, these can be combined with FreeSurfer's
#' \code{mris_label2annot}. 
#'
#' @param labels label file path vector
#' @template hemisphere
#' @param ctab colourtable file
#' @template subject
#' @template subjects_dir
#' @template annot_dir
#' @template output_dir
#' @template verbose
#' @template opts
#' @importFrom freesurfer get_fs fs_subj_dir
#' @export
#' @examples 
#' if(freesurfer::have_fs()){
#' # for freesurfer help see:
#' freesurfer::fs_help("mris_label2annot")
#' 
#' subj_dir <- freesurfer::fs_subj_dir()
#' # Split up aparc annot into labels
#' mri_annotation2label(annot_name = "aparc")
#' 
#' # get annot for colour labels
#' annot <- freesurfer::read_annotation(
#'               file.path(subj_dir, 
#'                         "fsaverage5/label/rh.aparc.annot"))
#' 
#' labels <- list.files(
#'    file.path(subj_dir, "fsaverage5/label/aparc"), 
#'     full.names = TRUE)
#' 
#' # Combine them again into annot.
#' mris_label2annot(labels, annot$colortable)
#' 
#' }
mris_label2annot <- function(labels, 
                             ctab, 
                             hemisphere = "rh", 
                             subject = "fsaverage5",
                             subjects_dir = fs_subj_dir(),
                             annot_dir = file.path(subjects_dir, subject, "label"),
                             output_dir = subjects_dir,
                             opts = NULL,
                             verbose = TRUE){
  if(!check_fs()) stop(call. = FALSE)
  
  hemisphere <- match.arg(hemisphere, c("rh", "lh"))
  
  if(!dir.exists(output_dir)) dir.create(output_dir, recursive = TRUE)
  # out_file <- file.path(output_dir, hemisphere)
  
  labs <- paste("--l", labels, collapse=" ")
  fs_cmd <- paste0(get_fs(),
                   "mris_label2annot")
  if(!is.null(opts)) fs_cmd <- paste0(fs_cmd, opts)
  
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
#' Calls FreeSurfer's \code{mri_annotation2label}
#' to split an annotation file into several labels.
#' 
#' @param annot_name annotation name. File should exist in subjects label directory
#' @template subject
#' @template hemisphere
#' @template output_dir
#' @template verbose
#' @template opts
#' @return nothing. Runs command line to write label files
#' @export
#' @importFrom freesurfer get_fs
#' @examples 
#' if(freesurfer::have_fs()){
#' # for freesurfer help see:
#' freesurfer::fs_help("mri_annotation2label")
#' mri_annotation2label(annot_name = "aparc")
#' 
#' mri_annotation2label(annot_name = "aparc.a2009s")
#' 
#' mri_annotation2label(subject = "fsaverage", annot_name = "aparc.a2009s")
#' }
mri_annotation2label <- function(annot_name, 
                             subject = "fsaverage5",
                             hemisphere = "lh", 
                             output_dir = fs_subj_dir(), 
                             verbose = TRUE,
                             opts = NULL){
  if(!check_fs()) stop(call. = FALSE)
  
  hemisphere <- match.arg(hemisphere, c("rh", "lh"))
  
  outdir <- file.path(output_dir, subject, "label", 
                      gsub("rh\\.|lh\\.|annot|\\.", "", basename(annot_name)))
  if(!dir.exists(outdir)) dir.create(outdir, recursive = TRUE)
  out_file <- file.path(outdir, hemisphere)
  
  fs_cmd <- paste0(get_fs(),
                   "mri_annotation2label")
  if(!is.null(opts)) fs_cmd <- paste0(fs_cmd, opts)
  
  
  cmd <- paste(
    fs_cmd,
    "--subject", subject,
    "--hemi", hemisphere,
    "--labelbase", file.path(outdir, hemisphere),
    "--annotation", annot_name
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
#' @template opts
#' @template subjects_dir
#'
#' @export
#' @importFrom freesurfer get_fs fs_subj_dir
#' @examples
#' if(freesurfer::have_fs()){
#' # for freesurfer help see:
#' freesurfer::fs_help("mris_ca_label")
#' mris_ca_label(output_file = "test.lh.annot")
#' 
#' mris_ca_label(hemisphere = "rh", output_file = "test.rh.annot")
#' }
mris_ca_label <- function(subject = "fsaverage5",
                          hemisphere = "lh", 
                          canonsurf  ="sphere.reg",
                          classifier = file.path(fs_dir(), "average/lh.DKTatlas40.gcs"),
                          output_file, 
                          subjects_dir = fs_subj_dir(),
                          opts = NULL){
  if(!check_fs()) stop(call. = FALSE)
  
  options <- paste("-sdir", subjects_dir)
  
  if(!is.null(opts)){
    options <- paste("-sdir", subjects_dir, opts)
  }
  
  fs_cmd <- paste0(get_fs(), "mris_ca_label")
  
  necs <- paste(subject, hemisphere, canonsurf, classifier, output_file)
  
  cmd <- paste(fs_cmd, options, necs)
  
  jj <- system(cmd, intern = TRUE)
  invisible(jj)
  
}

#' Re-register an annotation file
#' 
#' Annotation files are subject specific.
#' Most are registered for fsaverage, but
#' we recommend using fsaverage5 for the mesh
#' plots in ggseg3d, as these contain a decent
#' balance in number of vertices for detailed
#' rendering and speed.
#'
#' @param subject subject the original annotation file is registered to
#' @param annot annotation file name (as found in subjects_dir)
#' @param hemi hemisphere (one of "lh" or "rh")
#' @param target_subject subject to re-register the annotation (default fsaverage5)
#' @template output_dir 
#' @template verbose 
#' @importFrom freesurfer get_fs
#' @return nothing
#' @export
#' @examples
#' if(freesurfer::have_fs()){
#' 
#' # For help see:
#' freesurfer::fs_help("mri_surf2surf")
#' 
#' mri_surf2surf_rereg(subject = "bert", 
#'                     annot = "aparc.DKTatlas",
#'                     target_subject = "fsaverage5")
#' }
#' 
mri_surf2surf_rereg <- function(subject,
                                annot,
                                hemi = c("lh", "rh"),
                                target_subject = "fsaverage5",
                                output_dir = file.path(fs_subj_dir(),subject, "label"),
                                verbose = TRUE
){
  if(!check_fs()) stop(call. = FALSE)
  
  hemi <- match.arg(hemi, c("lh", "rh"))
  
  if(!dir.exists(output_dir)) dir.create(output_dir, recursive = TRUE)
  
  fscmd <- paste0(get_fs(), "mri_surf2surf")
  
  cmd <- paste(fscmd, 
               "--srcsubject", subject,
               "--sval-annot",  annot,
               "--trgsubject fsaverage5",
               "--tval", file.path(output_dir, paste(hemi, annot, sep=".")),
               "--hemi", hemi
  )
  
  k <- system(cmd, intern = !verbose)
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
#' @importFrom freesurfer get_fs
#' @return ascii data
#' @noRd
surf2asc <- function(input_file, output_file, verbose = TRUE){
  if(!check_fs()) stop(call. = FALSE)
  
  k <- strsplit(output_file, "\\.")[[1]]
  if(k[length(k)] != "dpv"){
    cat("output_file must end with '.dpv'")
    stop(call.=FALSE)
  }
  
  fscmd <- paste0(get_fs(), "mris_convert")
  
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
#' @importFrom freesurfer get_fs
#' @return ascii data
#' @noRd
curv2asc <- function(input_file, white, output_file, verbose = TRUE){
  if(!check_fs()) stop(call. = FALSE)
  
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
  
  fscmd <- paste0(get_fs(), "mris_convert -c")
  
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
#' @importFrom freesurfer get_fs
#' @return ascii data
#' @noRd
curvnf2asc <- function(input_file, nofix, output_file, verbose = TRUE){
  if(!check_fs()) stop(call. = FALSE)
  
  k <- strsplit(output_file, "\\.")[[1]]
  if(k[length(k)] != "dpv"){
    cat("output_file must end with '.dpv'")
    stop(call.=FALSE)
  }
  
  fscmd <- paste0(get_fs(), "mris_convert -c")
  
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
#' @param indices vertex or face sequential index
#' @template verbose
#' @importFrom freesurfer read_annotation 
#' @return data frame
#' @noRd
annot2dpv <- function(input_file, 
                      output_file,
                      coordinates = NULL,
                      indices = NULL,
                      verbose = TRUE){
  
  annot <- read_annotation(input_file, verbose = verbose)
  
  # For each structure, replace its coded colour by its index
  labs <- match(annot$label, annot$colortable$code)
  
  if(all(unlist(lapply(labs, is.integer)))){
    fstr = '%d';
  } else{
    fstr = '%0.10f'
  }
  
  if(!all(is.null(coordinates) & is.null(indices))){
    # Organise the coords
    if(dim(coordinates)[1] < dim(coordinates)[2]) {
      coordinates = t(coordinates)
    }
    
    # Prepare to save
    dpx = cbind(indices,  coordinates, labs)
    
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
  
  read.table(output_file)
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
#' @noRd
asc2ply <- function(input_file, 
                    output_file = gsub("\\.dpv", ".ply", input_file)){
  srf_file <- readLines(input_file)
  
  nfo <- as.numeric(strsplit(srf_file[2], " ")[[1]])
  names(nfo) <- c("vertex", "face")
  
  srf_file <- srf_file[c(-1, -2)]
  srf_data <- read.table(text = srf_file)
  
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
#' @importFrom freesurfer get_fs
smooth2srf <- function(input_file, output_file, verbose){
  
  if(!check_fs()) stop(call. = FALSE)
  
  k <- strsplit(output_file, "\\.")[[1]]
  if(k[length(k)] != "srf"){
    cat("output_file must end with '.srf'")
    stop(call.=FALSE)
  }
  
  fscmd <- paste0(get_fs(), "mris_convert")
  
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
#' @importFrom freesurfer fs_subj_dir mri_surf2surf
lcbc_surf2surf <- function(
  input_volume,
  source_subject = "fsaverage", 
  target_subject = "fsaverage5", 
  hemisphere = "rh",
  subjects_dir = fs_subj_dir(),
  output_dir = file.path(subjects_dir, target_subject, "surf"),
  cortex = TRUE,
  verbose = TRUE
){
  if(!check_fs()) stop(call. = FALSE)
  
  j <- mri_surf2surf(
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
    cat(msg)
  }
  
  freesurfer::have_fs()
}

#' @importFrom freesurfer get_fs
fs_ss_slice <- function(lab, x, y, z, view, subjects_dir, subject, output_dir,
                        skip_existing = TRUE) {
  coords <- sprintf(c(x, y, z), fmt = "%03d")
  vv <- paste0(strsplit(view, "")[[1]][1:5], collapse="")
  
  filenm <- paste0(paste(c(coords, vv), collapse="_"), "_", basename(lab), ".png")
  
  if(file.exists(file.path(output_dir, filenm)) & skip_existing){
    
  }else{
    fs_cmd <- paste0(get_fs(), "freeview")
    
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
}



## quiets concerns of R CMD check
if(getRversion() >= "2.15.1"){
  globalVariables(c("idx", 
                           "key", "view", "val", "vars",
                           paste0("V", 1:3)))
}


