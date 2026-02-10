# File I/O functions ----

#' Check if file is a supported volume format
#'
#' @param file Path to file
#' @return Logical
#' @keywords internal
is_volume_file <- function(file) {
  grepl("\\.(mgz|nii|nii\\.gz)$", file, ignore.case = TRUE)
}

#' Read neuroimaging volume file
#'
#' Reads volume data from common neuroimaging formats including
#' FreeSurfer MGZ and NIfTI. By default, reorients to RAS+ so that
#' dim1 = Left-to-Right, dim2 = Posterior-to-Anterior,
#' dim3 = Inferior-to-Superior.
#'
#' When `reorient = FALSE`, returns an RNifti niftiImage preserving the
#' file's native orientation and header metadata.
#'
#' @param file Path to volume file (.mgz, .nii, .nii.gz)
#' @param reorient If TRUE (default), reorient the volume to RAS+ and
#'   return a plain array. If FALSE, return an RNifti niftiImage in the
#'   file's native orientation (preserves header for downstream use).
#' @return 3D array (reorient=TRUE) or niftiImage (reorient=FALSE)
#' @keywords internal
read_volume <- function(file, reorient = TRUE) {
  if (!file.exists(file)) {
    cli::cli_abort("Volume file not found: {.path {file}}")
  }

  ext <- tolower(tools::file_ext(file))
  if (ext == "gz") {
    ext <- tools::file_ext(sub("\\.gz$", "", file))
  }

  vol <- switch(
    ext,
    "mgz" = freesurfer::read_mgz(file),
    "nii" = RNifti::readNifti(file),
    cli::cli_abort(c(
      "Unsupported volume format: {.file {basename(file)}}",
      "i" = "Supported formats: .mgz, .nii, .nii.gz"
    ))
  )

  nii <- RNifti::asNifti(vol)

  if (reorient) {
    if (RNifti::orientation(nii) != "RAS") {
      RNifti::orientation(nii) <- "RAS"
    }
    return(as.array(nii))
  }

  nii
}

#' Read mesh data from PLY file
#'
#' Reads an ASCII PLY file and extracts vertices and faces into
#' a list format suitable for ggseg3d.
#'
#' @param ply path to ply-file
#' @param ... ignored, kept for backward compatibility
#'
#' @return list with vertices (data.frame with x, y, z) and
#'   faces (data.frame with i, j, k)
#' @keywords internal
read_ply_mesh <- function(ply, ...) {
  if (!is.character(ply)) {
    cli::cli_abort("{.arg ply} must be a file path")
  }

  lines <- readLines(ply)

  if (lines[1] != "ply") {
    cli::cli_abort("Not a valid PLY file: {.path {ply}}")
  }

  n_vertices <- 0L
  n_faces <- 0L
  header_end <- 0L

  for (i in seq_along(lines)) {
    line <- trimws(lines[i])
    if (grepl("^element vertex", line)) {
      n_vertices <- as.integer(sub("element vertex ", "", line))
    } else if (grepl("^element face", line)) {
      n_faces <- as.integer(sub("element face ", "", line))
    } else if (line == "end_header") {
      header_end <- i
      break
    }
  }

  vert_lines <- lines[(header_end + 1):(header_end + n_vertices)]
  vert_data <- do.call(
    rbind,
    lapply(
      strsplit(vert_lines, "\\s+"),
      function(x) as.numeric(x[1:3])
    )
  )

  vertices <- data.frame(
    x = vert_data[, 1],
    y = vert_data[, 2],
    z = vert_data[, 3]
  )

  face_lines <- lines[
    (header_end + n_vertices + 1):(header_end + n_vertices + n_faces)
  ]
  face_data <- do.call(
    rbind,
    lapply(
      strsplit(face_lines, "\\s+"),
      function(x) as.integer(x[2:4])
    )
  )

  faces <- data.frame(
    i = face_data[, 1],
    j = face_data[, 2],
    k = face_data[, 3]
  )

  list(vertices = vertices, faces = faces)
}

# FreeSurfer annotation reading ----

#' Read annotation data from files
#'
#' Reads FreeSurfer annotation files and extracts region information
#' including vertices, colours, and labels for both hemispheres.
#'
#' @param annot_files Character vector of paths to annotation files.
#'   Files should follow FreeSurfer naming convention with `lh.` or `rh.`
#'   prefix (e.g., `c("lh.aparc.annot", "rh.aparc.annot")`).
#'
#' @return A tibble with columns: hemi, region, label, colour, vertices
#' @export
#' @importFrom dplyr tibble bind_rows
#' @importFrom freesurfer read_annotation
#' @importFrom grDevices rgb
#'
#' @examples
#' \dontrun{
#' # Read from file paths
#' atlas_data <- read_annotation_data(c(
#'   "path/to/lh.aparc.annot",
#'   "path/to/rh.aparc.annot"
#' ))
#' }
read_annotation_data <- function(annot_files) {
  if (!all(file.exists(annot_files))) {
    missing <- annot_files[!file.exists(annot_files)] # nolint: object_usage_linter
    cli::cli_abort("Annotation file{?s} not found: {.path {missing}}")
  }

  all_data <- list()

  for (annot_file in annot_files) {
    filename <- basename(annot_file)
    hemi_short <- if (grepl("^lh\\.", filename)) {
      "lh"
    } else if (grepl("^rh\\.", filename)) {
      "rh"
    } else {
      cli::cli_warn(
        "Cannot detect hemisphere from filename: {.file {filename}}"
      )
      next
    }
    hemi <- if (hemi_short == "lh") "left" else "right"

    ant <- freesurfer::read_annotation(annot_file, verbose = get_verbose()) # nolint: object_usage_linter
    colortable <- ant$colortable[!is.na(ant$colortable$R), ]
    colortable_codes <- colortable$code # nolint: object_usage_linter

    labeled_vertices <- integer(0)

    for (i in seq_len(nrow(colortable))) {
      region_name <- colortable$label[i]
      region_code <- colortable$code[i]

      region_vertices <- which(ant$label == region_code) - 1L
      if (length(region_vertices) == 0) {
        next
      }

      labeled_vertices <- c(labeled_vertices, region_vertices)

      all_data[[length(all_data) + 1]] <- tibble(
        hemi = hemi,
        region = region_name,
        label = paste(hemi_short, region_name, sep = "_"),
        colour = rgb(
          colortable$R[i],
          colortable$G[i],
          colortable$B[i],
          maxColorValue = 255
        ),
        vertices = list(region_vertices)
      )
    }

    all_vertex_indices <- seq_along(ant$label) - 1L
    unlabeled_vertices <- setdiff(all_vertex_indices, labeled_vertices)

    if (length(unlabeled_vertices) > 0) {
      all_data[[length(all_data) + 1]] <- tibble(
        hemi = hemi,
        region = "unknown",
        label = paste(hemi_short, "unknown", sep = "_"),
        colour = "#BEBEBE",
        vertices = list(unlabeled_vertices)
      )
    }
  }

  bind_rows(all_data)
}


#' Read vertex indices from a FreeSurfer label file
#'
#' @param label_file Path to .label file
#' @return Integer vector of vertex indices (0-indexed)
#' @keywords internal
read_label_vertices <- function(label_file) {
  lines <- readLines(label_file)

  if (length(lines) < 2) {
    cli::cli_warn("Empty or malformed label file: {label_file}")
    return(integer(0))
  }

  first_data_line <- if (grepl("^#", lines[1])) 3 else 2
  n_vertices <- as.integer(lines[first_data_line - 1])

  if (is.na(n_vertices) || n_vertices == 0) {
    return(integer(0))
  }

  data_lines <- lines[first_data_line:(first_data_line + n_vertices - 1)]
  vertices <- vapply(
    strsplit(data_lines, "\\s+"),
    function(x) as.integer(x[1]),
    integer(1)
  )

  vertices
}


# DPV file format ----

#' Write DPV file
#'
#' @param path path to file
#' @param vertices object with vertices
#' @param faces object with faces
#' @noRd
write_dpv <- function(path, vertices, faces) {
  if (min(faces) == 1) {
    faces <- faces - 1
  }

  vertices <- cbind(vertices, r = rep(0, nrow(vertices)))
  faces <- cbind(faces, r = rep(0, nrow(faces)))

  vertices <- within(vertices, l <- sprintf(paste("%f %f %f %g"), x, y, z, r)) # nolint: object_usage_linter
  faces <- within(faces, l <- sprintf(paste("%g %g %g %g"), i, j, k, r))

  file_content <- c(
    "#!ascii",
    sprintf("%g %g", nrow(vertices), nrow(faces)),
    vertices$l,
    faces$l
  )

  con <- file(path)
  on.exit(close(con))
  writeLines(file_content, con)
}


#' Read DPV file
#'
#' @param path path to dpv file
#' @noRd
#' @return list of vertices and faces
#' @importFrom utils read.table
read_dpv <- function(path) {
  header <- readLines(path, n = 2)
  counts <- as.integer(strsplit(trimws(header[2]), "\\s+")[[1]])
  n_vertices <- counts[1]
  n_faces <- counts[2]

  data <- read.table(path, skip = 2)

  vertices <- data[seq_len(n_vertices), 1:3, drop = FALSE]
  names(vertices) <- c("x", "y", "z")
  row.names(vertices) <- NULL

  faces <- data[(n_vertices + 1):(n_vertices + n_faces), 1:3, drop = FALSE]
  names(faces) <- c("i", "j", "k")
  row.names(faces) <- NULL

  list(vertices = vertices, faces = faces)
}


# FreeSurfer color table functions ----

#' Read FreeSurfer color table
#'
#' Read a FreeSurfer color lookup table file (e.g., `FreeSurferColorLUT.txt`
#' or `ASegStatsLUT.txt`). These files map label indices to region names
#' and RGBA colours.
#'
#' @param path Path to the color table file.
#' @return A data.frame with columns: idx, label, R, G, B, A.
#' @seealso [get_ctab()] to read and add hex colours, [write_ctab()] to write
#' @export
#' @importFrom utils read.table
read_ctab <- function(path) {
  x <- read.table(path)
  names(x) <- c("idx", "label", "R", "G", "B", "A")
  x
}


#' Write FreeSurfer color table
#'
#' Write a color table to file in FreeSurfer format.
#'
#' @param x A data.frame with columns: idx, label, R, G, B, A.
#' @param path Path to write to.
#' @return Invisibly returns the lines written.
#' @seealso [read_ctab()], [is_ctab()]
#' @export
write_ctab <- function(x, path) {
  lls <- apply(x, 1, function(c) ctab_line(c[1], c[2], c[3], c[4], c[5], c[6]))
  lls[length(lls) + 1] <- ""
  writeLines(lls, path)
  invisible(lls)
}


#' Check if object is a color table
#'
#' @param x Object to check.
#' @return TRUE if x is a data.frame with the required color table columns.
#' @export
is_ctab <- function(x) {
  if (!is.data.frame(x)) {
    return(FALSE)
  }
  required <- c("idx", "label", "R", "G", "B", "A") #nolint
  all(required %in% names(x))
}


#' Read color table and add hex colours
#'
#' Reads a FreeSurfer color lookup table and adds hex colour codes for
#' use in plotting.
#'
#' @param color_lut Path to a color table file, or a data.frame that
#'   passes [is_ctab()].
#' @return A data.frame with the original columns plus `roi` (zero-padded
#'   index) and `color` (hex colour code).
#' @seealso [read_ctab()], [is_ctab()]
#' @export
#' @importFrom grDevices rgb
get_ctab <- function(color_lut) {
  colourtable <- if (is.character(color_lut)) {
    read_ctab(color_lut)
  } else {
    color_lut
  }

  if (!is_ctab(colourtable)) {
    cli::cli_abort(c(
      "color_lut does not have the correct format.",
      "i" = "Required columns: idx, label, R, G, B, A"
    ))
  }

  colourtable$roi <- sprintf("%04d", colourtable$idx)
  colourtable$color <- rgb(
    colourtable$R,
    colourtable$G,
    colourtable$B,
    maxColorValue = 255
  )

  colourtable
}

# nolint start
#' @noRd
ctab_line <- function(idx, name, R, G, B, A) {
  if (nchar(name) > 29) {
    name <- substr(name, 1, 29)
  }
  sprintf(
    "% 3s  % -30s  % 3s % 3s % 3s % 3s",
    idx,
    name,
    R,
    G,
    B,
    A
  )
}
# nolint end
