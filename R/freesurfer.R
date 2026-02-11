# FreeSurfer constants ----

#' Available Freesurfer surfaces
#'
#' @return character
#' @keywords internal
fs_surfaces <- function() {
  c(
    "inflated",
    "inflated.nofix",
    "orig",
    "orig.nofix",
    "pial",
    "qsphere.nofix",
    "smoothwm",
    "smoothwm.nofix",
    "sphere",
    "sphere.reg",
    "white",
    "LCBC"
  )
}

#' Available Freesurfer curvatures
#'
#' @return character
#' @keywords internal
fs_curvatures <- function() {
  c(
    "area",
    "area.mid",
    "area.pial",
    "avg_curv",
    "curv",
    "curv.pial",
    "inflated.H",
    "inflated.K",
    "jacobian_white",
    "smoothwm.BE.crv",
    "smoothwm.C.crv",
    "smoothwm.FI.crv",
    "smoothwm.H.crv",
    "smoothwm.K1.crv",
    "smoothwm.K2.crv",
    "smoothwm.K.crv",
    "smoothwm.S.crv",
    "sulc",
    "thickness",
    "volume",
    "w-g.pct.mgh"
  )
}

#' Available Freesurfer no fix curvatures
#'
#' @return character vector of no-fix curvature file names
#' @keywords internal
fs_nofixcurvatures <- function() {
  c("defect_borders", "defect_chull", "defect_labels")
}


# FreeSurfer check ----

#' Check if FS can be run
#' @param msg message to print on error
#' @param abort logical. If function should error
#'     if Freesurfer is not installed. Defaults to FALSE.
#' @return logical
#' @keywords internal
check_fs <- function(msg = NULL, abort = FALSE) {
  x <- freesurfer::have_fs()

  if (!x) {
    msg <- paste0(
      "System does not have Freesurfer or ",
      "Freesurfer has not been setup correctly.\n",
      "Aborting.\n"
    )
    if (abort) {
      cli::cli_abort(msg)
    }
    cli::cli_alert_danger(msg)
  }
  invisible(x)
}


# FreeSurfer command wrappers ----

#' Convert volume to surface
#'
#' @param input_file input volume
#' @template output_file
#' @template hemisphere
#' @param projfrac single cortical depth fraction (0-1). Ignored if
#'   `projfrac_range` is provided.
#' @param projfrac_range numeric vector `c(min, max, delta)` for multi-depth
#'   projection via `--projfrac-max`. Takes the maximum value across depths,
#'   giving much better coverage for volumetric parcellations.
#' @template verbose
#' @template opts
#' @importFrom freesurfer get_fs
#' @noRd
mri_vol2surf <- function(
  input_file,
  output_file,
  hemisphere,
  projfrac = .5,
  projfrac_range = NULL,
  mni152reg = TRUE,
  opts = NULL,
  verbose = get_verbose() # nolint: object_usage_linter
) {
  check_fs(abort = TRUE)

  fs_cmd <- "mri_vol2surf"

  if (!is.null(opts)) {
    fs_cmd <- paste(fs_cmd, opts)
  }

  cmd <- paste(
    fs_cmd,
    "--mov",
    input_file,
    "--o",
    output_file
  )

  if (mni152reg) {
    cmd <- paste(cmd, "--mni152reg")
  }

  cmd <- paste(cmd, "--hemi", hemisphere)

  if (!is.null(projfrac_range)) {
    cmd <- paste(
      cmd, "--projfrac-max",
      projfrac_range[1], projfrac_range[2], projfrac_range[3]
    )
  } else {
    cmd <- paste(cmd, "--projfrac", projfrac)
  }

  suppressWarnings(
    k <- run_cmd(cmd, verbose = verbose)
  )

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
#' Calls FreeSurfer's `mri_vol2label`.
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
#' @keywords internal
mri_vol2label <- function(
  input_file,
  label_id,
  hemisphere = "rh",
  output_dir,
  surface = NULL,
  subject = "fsaverage5",
  subjects_dir = fs_subj_dir(),
  opts = NULL,
  verbose = get_verbose() # nolint: object_usage_linter
) {
  check_fs(abort = TRUE)

  hemisphere <- match.arg(hemisphere, c("lh", "rh"))

  mkdir(output_dir)

  output_file <- paste0(
    output_dir,
    "/",
    hemisphere,
    "_",
    formatC(label_id, width = 4, flag = "0"),
    ".label"
  )

  fs_cmd <- "mri_vol2label"

  if (!is.null(opts)) {
    fs_cmd <- paste0(fs_cmd, opts)
  }

  cmd <- paste(
    fs_cmd,
    "--c",
    input_file,
    "--id",
    label_id,
    "--sd",
    subjects_dir,
    "--l",
    output_file
  )

  if (!is.null(surface)) {
    cmd <- paste(cmd, "--surf", subject, hemisphere)
  }

  k <- run_cmd(cmd, verbose = verbose)

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
mri_pretess <- function(
  template,
  label,
  output_file,
  verbose = get_verbose(), # nolint: object_usage_linter
  opts = NULL
) {
  check_fs(abort = TRUE)

  fscmd <- "mri_pretess"

  if (!is.null(opts)) {
    fscmd <- paste(fscmd, opts)
  }

  cmd <- paste(fscmd, template, label, template, output_file)

  k <- run_cmd(cmd, verbose = verbose) # nolint: object_usage_linter
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
mri_tessellate <- function(
  input_file,
  label,
  output_file,
  verbose,
  opts = NULL
) {
  check_fs(abort = TRUE)

  fscmd <- "mri_tessellate"

  if (!is.null(opts)) {
    fscmd <- paste(fscmd, opts)
  }

  cmd <- paste(fscmd, input_file, label, output_file)

  k <- run_cmd(cmd, verbose = verbose) # nolint: object_usage_linter
}


#' Smooth data
#'
#' @param input_file input file to smooth
#' @template output_file
#' @template verbose
#' @template opts
#' @importFrom freesurfer get_fs
#' @noRd
mri_smooth <- function(input_file, output_file, verbose, opts = NULL) {
  check_fs(abort = TRUE)

  fscmd <- "mris_smooth"
  if (!is.null(opts)) {
    fscmd <- paste(fscmd, opts)
  }

  cmd <- paste(
    fscmd,
    "-nw",
    shQuote(normalizePath(input_file, mustWork = FALSE)),
    shQuote(normalizePath(output_file, mustWork = FALSE))
  )

  k <- run_cmd(cmd, verbose = verbose)
  invisible(k)
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
#' @param target_subject subject to re-register the annotation
#'   (default fsaverage5)
#' @template output_dir
#' @template verbose
#' @importFrom freesurfer get_fs
#' @return nothing
#' @export
#' @examples
#' \dontrun{
#' # For help see:
#' freesurfer::fs_help("mri_surf2surf")
#'
#' mri_surf2surf_rereg(
#'   subject = "bert",
#'   annot = "aparc.DKTatlas",
#'   target_subject = "fsaverage5"
#' )
#' }
mri_surf2surf_rereg <- function(
  subject,
  annot,
  hemi = c("lh", "rh"),
  target_subject = "fsaverage5",
  output_dir = file.path(fs_subj_dir(), subject, "label"),
  verbose = get_verbose() # nolint: object_usage_linter
) {
  check_fs(abort = TRUE)

  hemi <- match.arg(hemi, c("lh", "rh"))

  mkdir(output_dir)

  fscmd <- "mri_surf2surf"

  cmd <- paste(
    fscmd,
    "--srcsubject",
    subject,
    "--sval-annot",
    annot,
    "--trgsubject fsaverage5",
    "--tval",
    file.path(output_dir, paste(hemi, annot, sep = ".")),
    "--hemi",
    hemi
  )

  k <- run_cmd(cmd, verbose = verbose) # nolint: object_usage_linter
}


# Surface/curvature to ASCII ----

#' Convert Freesurfer surface file to ascii
#'
#' @param input_file path to input surface file to convert
#' @template output_file
#' @template verbose
#' @importFrom freesurfer get_fs
#' @return ascii data
#' @noRd
surf2asc <- function(input_file, output_file, verbose = get_verbose()) {
  # nolint: object_usage_linter
  check_fs(abort = TRUE)

  k <- strsplit(output_file, "\\.")[[1]]
  if (k[length(k)] != "dpv") {
    cat("output_file must end with '.dpv'")
    cli::cli_abort(call. = FALSE)
  }

  if (!file.exists(input_file)) {
    if (verbose) {
      cli::cli_warn(
        "Inputfile does not exist. Check file path: {.file {input_file}}"
      )
    }
    return()
  }

  if (verbose) {
    cli::cli_alert("Saving {.file {output_file}}")
  }

  freesurfer::mris_convert(
    infile = input_file,
    outfile = gsub("\\.dpv", "\\.asc", output_file),
    verbose = verbose
  )

  j <- file.rename(gsub("\\.dpv", "\\.asc", output_file), output_file) # nolint: object_usage_linter

  read_dpv(output_file)
}


#' Convert Freesurfer curvature file to ascii
#'
#' @param input_file path to curvature file
#' @param white path to subjects hemi.white file
#' @template output_file
#' @template verbose
#' @importFrom freesurfer get_fs
#' @return ascii data
#' @noRd
curv2asc <- function(input_file, white, output_file, verbose = get_verbose()) {
  # nolint: object_usage_linter
  check_fs(abort = TRUE)

  k <- strsplit(output_file, "\\.")[[1]]
  if (k[length(k)] != "dpv") {
    cat("output_file must end with '.dpv'")
    cli::cli_abort(call. = FALSE)
  }

  if (!file.exists(input_file)) {
    if (verbose) {
      cat(paste0("Inputfile does not exist. Check file path:\n", input_file))
    }
    return()
  }

  fscmd <- "mris_convert -c"

  cmd <- paste(fscmd, input_file, white, gsub("\\.dpv", "\\.asc", output_file))
  j <- run_cmd(cmd, verbose = verbose) # nolint: object_usage_linter

  if (verbose) {
    cat(paste("Saving", output_file, "\n"))
  }
  j <- file.rename(gsub("\\.dpv", "\\.asc", output_file), output_file) # nolint: object_usage_linter

  read_dpv(output_file)
}


# ASCII to PLY ----

#' Convert ascii to .ply
#'
#' @param input_file input surface or curvature file
#' @template output_file
#'
#' @return mesh object with vertices and faces
#' @noRd
asc2ply <- function(
  input_file,
  output_file = gsub("\\.dpv", ".ply", input_file)
) {
  srf_file <- readLines(input_file)

  nfo <- as.numeric(strsplit(srf_file[2], " ")[[1]])
  names(nfo) <- c("vertex", "face")

  srf_file <- srf_file[c(-1, -2)]
  srf_data <- read.table(text = srf_file)

  vert <- srf_data[1:nfo["vertex"], 1:3]
  vert <- unname(apply(vert, 1, paste, collapse = " "))

  face <- cbind(3, srf_data[(nfo["vertex"] + 1):nrow(srf_data), 1:3])
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

  invisible(read_ply_mesh(output_file))
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
#' @return mesh object with vertices and faces
#' @keywords internal
surf2ply <- function(
  input_file,
  output_file = paste(input_file, ".ply"),
  verbose = get_verbose() # nolint: object_usage_linter
) {
  basefile <- gsub("\\.ply", "", output_file)

  srf <- surf2asc(input_file, paste0(basefile, ".asc"), FALSE) # nolint: object_usage_linter
  asc2ply(paste0(basefile, ".asc"), output_file)
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
#' @return mesh object with vertices and faces
#' @keywords internal
curv2ply <- function(
  input_file,
  output_file = paste(input_file, ".ply"),
  verbose = get_verbose() # nolint: object_usage_linter
) {
  basefile <- gsub("\\.ply", "", output_file)

  srf <- curv2asc(input_file, paste0(basefile, ".asc"), FALSE) # nolint: object_usage_linter
  asc2ply(paste0(basefile, ".asc"), output_file)
}


# Other FreeSurfer utilities ----

#' Turn smooth file to ascii
#'
#' @param input_file input file path
#' @template output_file
#' @template verbose
#' @importFrom freesurfer get_fs
#' @keywords internal
smooth2srf <- function(input_file, output_file, verbose) {
  check_fs(abort = TRUE)

  k <- strsplit(output_file, "\\.")[[1]]
  if (k[length(k)] != "srf") {
    cat("output_file must end with '.srf'")
    cli::cli_abort(call. = FALSE)
  }

  fscmd <- "mris_convert"

  cmd <- paste(fscmd, input_file, gsub("\\.srf", "\\.asc", output_file))

  k <- run_cmd(cmd, verbose = verbose)

  k <- file.rename(gsub("\\.srf", "\\.asc", output_file), output_file)
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
#' @keywords internal
lcbc_surf2surf <- function(
  input_volume,
  source_subject = "fsaverage",
  target_subject = "fsaverage5",
  hemisphere = "rh",
  subjects_dir = fs_subj_dir(),
  output_dir = file.path(subjects_dir, target_subject, "surf"),
  cortex = TRUE,
  verbose = get_verbose()
) {
  check_fs(abort = TRUE)
  invisible(
    mri_surf2surf(
      sval = input_volume,
      subject = source_subject,
      target_subject = target_subject,
      hemi = hemisphere,
      outfile = paste0(output_dir, hemisphere, ".lcbc"),
      subj_dir = subjects_dir,
      verbose = verbose
    )
  )
}
