# Hemisphere utilities ----

#' Detect hemisphere from label name
#'
#' Detects whether a label belongs to left or right hemisphere based on
#' common naming conventions. Handles multiple patterns:
#' - Prefix: "Left-", "left_", "lh.", "lh_", "L_"
#' - Suffix: "_left", "_lh", "_L", "_l"
#' - Contains: "left", "right" (case insensitive)
#'
#' @param label_name Character string containing label/region name
#' @param strict If TRUE, only match prefix patterns. If FALSE (default),
#'   also check if label contains "left"/"right" anywhere.
#' @param default Value to return when no hemisphere detected. Default is
#'   NA_character_. Use "midline" for tract atlases.
#' @return "left", "right", or the default value
#' @noRd
detect_hemi <- function(label_name, strict = FALSE, default = NA_character_) {
  if (is.na(label_name) || label_name == "") {
    return(default)
  }

  left_prefix <- grepl("^(Left|left|lh|L)[- _.]+", label_name)
  left_suffix <- grepl("[- _.]+(left|lh|L|l)$", label_name)
  right_prefix <- grepl("^(Right|right|rh|R)[- _.]+", label_name)
  right_suffix <- grepl("[- _.]+(right|rh|R|r)$", label_name)

  if (left_prefix || left_suffix) {
    return("left")
  }
  if (right_prefix || right_suffix) {
    return("right")
  }

  if (!strict) {
    if (grepl("left|lh", label_name, ignore.case = TRUE)) {
      return("left")
    }
    if (grepl("right|rh", label_name, ignore.case = TRUE)) {
      return("right")
    }
  }

  default
}

#' Vectorized hemisphere detection
#' @noRd
detect_hemi_vec <- function(
  label_names,
  strict = FALSE,
  default = NA_character_
) {
  vapply(
    label_names,
    detect_hemi,
    character(1),
    strict = strict,
    default = default,
    USE.NAMES = FALSE
  )
}

#' Map short hemisphere code to long form
#' @noRd
hemi_to_long <- function(hemi_short) {
  if (hemi_short == "lh") {
    "left"
  } else if (hemi_short == "rh") {
    "right"
  } else {
    hemi_short
  }
}

#' Map long hemisphere to short code
#' @noRd
hemi_to_short <- function(hemi_long) {
  if (hemi_long == "left") {
    "lh"
  } else if (hemi_long == "right") {
    "rh"
  } else {
    hemi_long
  }
}


# Region name utilities ----

#' Clean region name from label
#'
#' Removes hemisphere prefix and normalizes the region name by converting
#' dashes and underscores to spaces and lowercasing.
#'
#' @param label_name Label name to clean
#' @param remove_hemi Remove hemisphere prefix (default TRUE)
#' @param normalize Convert to lowercase with spaces (default TRUE)
#' @return Cleaned region name
#' @noRd
clean_region_name <- function(
  label_name,
  remove_hemi = TRUE,
  normalize = TRUE
) {
  region <- label_name

  if (remove_hemi) {
    region <- gsub("^(Left|Right|left|right|lh|rh|L|R)[- _.]?", "", region)
  }

  if (normalize) {
    region <- gsub("-|_", " ", region)
    region <- tolower(region)
  }

  region
}

#' Vectorized region name cleaning
#' @noRd
clean_region_names <- function(
  label_names,
  remove_hemi = TRUE,
  normalize = TRUE
) {
  vapply(
    label_names,
    clean_region_name,
    character(1),
    remove_hemi = remove_hemi,
    normalize = normalize,
    USE.NAMES = FALSE
  )
}


# Directory setup ----

#' Setup standard atlas directory structure
#' @param output_dir Base output directory
#' @param atlas_name Name of the atlas
#' @param type Type of atlas: "cortical", "subcortical", or "tract"
#' @return Named list of directory paths
#' @noRd
setup_atlas_dirs <- function(output_dir, atlas_name = NULL, type = "cortical") {
  base <- if (is.null(atlas_name)) {
    output_dir
  } else {
    file.path(output_dir, atlas_name)
  }

  dirs <- list(
    base = base,
    snapshots = file.path(base, "snapshots"),
    processed = file.path(base, "processed"),
    masks = file.path(base, "masks"),
    snaps = file.path(base, "snapshots")
  )

  if (type == "subcortical") {
    dirs$meshes <- file.path(base, "meshes")
  }

  if (type == "tract") {
    dirs$volumes <- file.path(base, "volumes")
  }

  invisible(
    lapply(dirs, mkdir)
  )

  dirs
}


# Atlas data construction ----

#' Build core, palette, and vertices/meshes from atlas data
#'
#' Consolidates the repeated pattern of building atlas components from a
#' data frame containing hemi, region, label, colour, and vertices/mesh columns.
#'
#' Labels with empty vertices are filtered out (these are context-only regions
#' like the medial wall that will only appear in sf geometry).
#'
#' @param atlas_data Data frame with hemi, region, label, colour columns
#'   and either vertices (list column) or mesh (list column)
#' @return Named list with core, palette, and either vertices_df or meshes_df
#' @noRd
#' @importFrom dplyr distinct bind_rows
build_atlas_components <- function(atlas_data) {
  if ("vertices" %in% names(atlas_data)) {
    vertex_lengths <- vapply(atlas_data$vertices, length, integer(1))
    atlas_data <- atlas_data[vertex_lengths > 0, , drop = FALSE]
  }

  core <- distinct(atlas_data, hemi, region, label)

  raw_colours <- stats::setNames(atlas_data$colour, atlas_data$label)
  raw_colours <- raw_colours[!duplicated(names(raw_colours))]
  palette <- if (all(is.na(raw_colours))) NULL else raw_colours

  result <- list(core = core, palette = palette)

  if ("vertices" %in% names(atlas_data)) {
    vertices_df <- data.frame(
      label = atlas_data$label,
      stringsAsFactors = FALSE
    )
    vertices_df$vertices <- atlas_data$vertices
    result$vertices_df <- vertices_df
  }

  if ("mesh" %in% names(atlas_data)) {
    meshes_df <- data.frame(
      label = atlas_data$label,
      stringsAsFactors = FALSE
    )
    meshes_df$mesh <- atlas_data$mesh
    result$meshes_df <- meshes_df
  }

  result
}
