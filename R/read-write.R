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

# Annotation reading ----

#' @noRd
annot_to_atlas_data <- function(annot, hemi, hemi_short) {
  ct <- annot$colortable_df
  ct <- ct[!is.na(ct$r), ]

  all_data <- list()
  labeled_vertices <- integer(0)

  for (i in seq_len(nrow(ct))) {
    region_name <- ct$struct_name[i]
    region_code <- ct$code[i]

    region_vertices <- which(annot$label_codes == region_code) - 1L
    if (length(region_vertices) == 0) next

    labeled_vertices <- c(labeled_vertices, region_vertices)

    all_data[[length(all_data) + 1]] <- tibble(
      hemi = hemi,
      region = region_name,
      label = paste(hemi_short, region_name, sep = "_"),
      colour = ct$hex_color_string_rgb[i],
      vertices = list(region_vertices)
    )
  }

  all_vertex_indices <- seq_along(annot$label_codes) - 1L
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

  all_data
}


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
#'
#' @examples
#' \dontrun{
#' atlas_data <- read_annotation_data(c(
#'   "path/to/lh.aparc.annot",
#'   "path/to/rh.aparc.annot"
#' ))
#' }
read_annotation_data <- function(annot_files) {
  rlang::check_installed(
    "freesurferformats",
    reason = "to read annotation files"
  )

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

    annot <- freesurferformats::read.fs.annot(annot_file)
    all_data <- c(all_data, annot_to_atlas_data(annot, hemi, hemi_short))
  }

  bind_rows(all_data)
}


#' Read vertex indices from a FreeSurfer label file
#'
#' @param label_file Path to .label file
#' @return Integer vector of vertex indices (0-indexed)
#' @keywords internal
read_label_vertices <- function(label_file) {
  rlang::check_installed(
    "freesurferformats",
    reason = "to read label files"
  )
  tryCatch(
    freesurferformats::read.fs.label.native(
      label_file,
      return_one_based_indices = FALSE
    ),
    error = function(e) {
      cli::cli_warn("Could not parse label file: {.path {label_file}}")
      integer(0)
    }
  )
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

# GIFTI annotation reading ----

#' Detect hemisphere from GIFTI filename
#'
#' @param filename Basename of the GIFTI file
#' @return "lh" or "rh", or NA if undetectable
#' @keywords internal
detect_hemi_from_gifti_filename <- function(filename) {
  if (grepl("^lh\\.|[._]lh[._]|\\.L\\.", filename)) {
    return("lh")
  }
  if (grepl("^rh\\.|[._]rh[._]|\\.R\\.", filename)) {
    return("rh")
  }
  NA_character_
}


#' @noRd
detect_hemi_from_neuromaps_filename <- function(filename) {
  if (grepl("hemi-L", filename, fixed = TRUE)) return("lh")
  if (grepl("hemi-R", filename, fixed = TRUE)) return("rh")
  detect_hemi_from_gifti_filename(filename)
}


#' Read GIFTI annotation files
#'
#' Reads GIFTI annotation (`.label.gii`) files and extracts region
#' information including vertices, colours, and labels. Returns data in
#' the same format as [read_annotation_data()] for use with the cortical
#' atlas pipeline.
#'
#' Hemisphere is detected from filename patterns: `lh.`, `rh.`, `.L.`, `.R.`
#'
#' @param gifti_files Character vector of paths to `.label.gii` files.
#'
#' @return A tibble with columns: hemi, region, label, colour, vertices
#' @export
#' @importFrom dplyr tibble bind_rows
#' @importFrom grDevices rgb
#'
#' @examples
#' \dontrun{
#' atlas_data <- read_gifti_annotation(c(
#'   "lh.aparc.label.gii",
#'   "rh.aparc.label.gii"
#' ))
#' }
read_gifti_annotation <- function(gifti_files) {
  rlang::check_installed(
    "freesurferformats",
    reason = "to read GIFTI annotation files"
  )

  if (!all(file.exists(gifti_files))) {
    missing <- gifti_files[!file.exists(gifti_files)]
    cli::cli_abort("GIFTI file{?s} not found: {.path {missing}}")
  }

  all_data <- list()

  for (gifti_file in gifti_files) {
    filename <- basename(gifti_file)
    hemi_short <- detect_hemi_from_gifti_filename(filename)

    if (is.na(hemi_short)) {
      cli::cli_warn(
        "Cannot detect hemisphere from filename: {.file {filename}}"
      )
      next
    }
    hemi <- if (hemi_short == "lh") "left" else "right"

    annot <- freesurferformats::read.fs.annot.gii(gifti_file)
    all_data <- c(all_data, annot_to_atlas_data(annot, hemi, hemi_short))
  }

  bind_rows(all_data)
}


# CIFTI annotation reading ----

#' Read CIFTI annotation file
#'
#' Reads a CIFTI dense label file (`.dlabel.nii`) and extracts region
#' information for both hemispheres. Returns data in the same format as
#' [read_annotation_data()] for use with the cortical atlas pipeline.
#'
#' The CIFTI file must be in fsaverage5 space (10,242 vertices per
#' hemisphere). If your file uses a different resolution, resample it first
#' with Connectome Workbench:
#' ```
#' wb_command -cifti-resample input.dlabel.nii ...
#' ```
#'
#' @param cifti_file Path to a `.dlabel.nii` CIFTI file.
#'
#' @return A tibble with columns: hemi, region, label, colour, vertices
#' @export
#' @importFrom dplyr tibble bind_rows
#' @importFrom grDevices rgb
#'
#' @examples
#' \dontrun{
#' atlas_data <- read_cifti_annotation("parcellation.dlabel.nii")
#' }
read_cifti_annotation <- function(cifti_file) {
  rlang::check_installed("ciftiTools", reason = "to read CIFTI files")

  if (!file.exists(cifti_file)) {
    cli::cli_abort("CIFTI file not found: {.path {cifti_file}}")
  }

  cii <- ciftiTools::read_cifti(cifti_file)

  fsaverage5_nverts <- 10242L
  all_data <- list()

  hemi_info <- list(
    list(
      data = cii$data$cortex_left,
      hemi = "left",
      hemi_short = "lh",
      expected_n = fsaverage5_nverts
    ),
    list(
      data = cii$data$cortex_right,
      hemi = "right",
      hemi_short = "rh",
      expected_n = fsaverage5_nverts
    )
  )

  label_table <- cii$meta$cifti$labels[[1]]

  for (hi in hemi_info) {
    if (is.null(hi$data)) next

    vertex_labels <- as.integer(hi$data[, 1])
    n_verts <- length(vertex_labels)

    if (n_verts != hi$expected_n) {
      cli::cli_abort(c(
        paste(
          "CIFTI {hi$hemi} hemisphere has {n_verts} vertices,",
          "expected {hi$expected_n} (fsaverage5)"
        ),
        "i" = paste(
          "Resample to fsaverage5 first using",
          "{.code wb_command -cifti-resample}"
        )
      ))
    }

    labeled_vertices <- integer(0)

    for (i in seq_len(nrow(label_table))) {
      region_key <- label_table$Key[i]
      region_name <- label_table$Label[i]

      region_vertices <- which(vertex_labels == region_key) - 1L
      if (length(region_vertices) == 0) next

      labeled_vertices <- c(labeled_vertices, region_vertices)

      all_data[[length(all_data) + 1]] <- tibble(
        hemi = hi$hemi,
        region = region_name,
        label = paste(hi$hemi_short, region_name, sep = "_"),
        colour = rgb(
          label_table$Red[i],
          label_table$Green[i],
          label_table$Blue[i],
          maxColorValue = 1
        ),
        vertices = list(region_vertices)
      )
    }

    all_vertex_indices <- seq_len(n_verts) - 1L
    unlabeled_vertices <- setdiff(all_vertex_indices, labeled_vertices)

    if (length(unlabeled_vertices) > 0) {
      all_data[[length(all_data) + 1]] <- tibble(
        hemi = hi$hemi,
        region = "unknown",
        label = paste(hi$hemi_short, "unknown", sep = "_"),
        colour = "#BEBEBE",
        vertices = list(unlabeled_vertices)
      )
    }
  }

  bind_rows(all_data)
}


# Neuromaps annotation reading ----

#' Read neuromaps annotation files
#'
#' Reads neuromaps GIFTI metric files (`.func.gii`) containing integer
#' parcel IDs and converts them to the standard annotation format used by
#' the cortical atlas pipeline. Vertex value 0 is treated as medial
#' wall / background.
#'
#' Files must be in fsaverage5 space (10,242 vertices per hemisphere).
#' Use `space = "fsaverage"` with `density = "10k"` when fetching from
#' neuromaps.
#'
#' @param gifti_files Character vector of paths to `.func.gii` files.
#'   Hemisphere is detected from BIDS filename patterns (`hemi-L`, `hemi-R`).
#' @param label_table Optional data.frame mapping integer parcel IDs to
#'   region names. Must have columns `id` (integer) and `region` (character).
#'   Optionally include `colour` (hex string). When `NULL`, regions are
#'   named `parcel_1`, `parcel_2`, etc.
#'
#' @return A tibble with columns: hemi, region, label, colour, vertices
#' @export
#' @importFrom dplyr tibble bind_rows
#' @importFrom grDevices hcl.colors
#'
#' @examples
#' \dontrun{
#' files <- ggseg.hub::fetch_neuromaps_annotation(
#'   "schaefer", "400parcels", "fsaverage", density = "10k"
#' )
#' atlas_data <- read_neuromaps_annotation(files)
#' }
read_neuromaps_annotation <- function(gifti_files, label_table = NULL) {
  rlang::check_installed("gifti", reason = "to read GIFTI metric files")

  if (!all(file.exists(gifti_files))) {
    missing <- gifti_files[!file.exists(gifti_files)]
    cli::cli_abort("GIFTI file{?s} not found: {.path {missing}}")
  }

  volume_files <- grepl("\\.(nii|nii\\.gz)$", gifti_files, ignore.case = TRUE)
  if (any(volume_files)) {
    cli::cli_abort(c(
      "Volume files are not supported for cortical atlas creation.",
      "i" = "Found volume file{?s}: {.path {gifti_files[volume_files]}}",
      "i" = "Use only surface (.func.gii) files."
    ))
  }

  if (!is.null(label_table)) {
    if (!all(c("id", "region") %in% names(label_table))) {
      cli::cli_abort(c(
        "{.arg label_table} must have columns {.field id} and {.field region}",
        "i" = "Optionally include a {.field colour} column with hex colour codes."
      ))
    }
  }

  fsaverage5_nverts <- 10242L
  all_data <- list()

  for (gifti_file in gifti_files) {
    filename <- basename(gifti_file)
    hemi_short <- detect_hemi_from_neuromaps_filename(filename)

    if (is.na(hemi_short)) {
      cli::cli_warn(
        "Cannot detect hemisphere from filename: {.file {filename}}"
      )
      next
    }
    hemi <- if (hemi_short == "lh") "left" else "right"

    gii <- gifti::read_gifti(gifti_file)
    values <- as.numeric(gii$data[[1]])
    n_verts <- length(values)

    if (n_verts != fsaverage5_nverts) {
      cli::cli_abort(c(
        paste(
          "{hemi} hemisphere has {n_verts} vertices,",
          "expected {fsaverage5_nverts} (fsaverage5)"
        ),
        "i" = paste(
          "Use space='fsaverage' with density='10k'",
          "for fsaverage5 compatibility."
        )
      ))
    }

    parcel_ids <- round(values)
    unique_ids <- sort(unique(parcel_ids))
    labeled_vertices <- integer(0)

    for (pid in unique_ids) {
      if (pid == 0) next

      region_vertices <- which(parcel_ids == pid) - 1L
      if (length(region_vertices) == 0) next

      labeled_vertices <- c(labeled_vertices, region_vertices)

      if (!is.null(label_table) && pid %in% label_table$id) {
        row <- label_table[label_table$id == pid, ]
        region_name <- row$region[1]
        colour <- if ("colour" %in% names(row)) row$colour[1] else NA_character_
      } else {
        region_name <- paste0("parcel_", pid)
        colour <- NA_character_
      }

      all_data[[length(all_data) + 1]] <- tibble(
        hemi = hemi,
        region = region_name,
        label = paste(hemi_short, region_name, sep = "_"),
        colour = colour,
        vertices = list(region_vertices)
      )
    }

    unlabeled_vertices <- which(parcel_ids == 0) - 1L
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

  result <- bind_rows(all_data)

  if (nrow(result) == 0) return(result)

  needs_colour <- is.na(result$colour) & result$region != "unknown"
  if (any(needs_colour)) {
    n_missing <- sum(needs_colour)
    generated <- hcl.colors(n_missing, palette = "Set2")
    result$colour[needs_colour] <- generated
  }

  result
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
