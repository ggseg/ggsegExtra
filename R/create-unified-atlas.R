# Unified atlas creation ----

#' Create brain atlas from FreeSurfer annotation
#'
#' Creates a brain_atlas object from FreeSurfer annotation files. The atlas
#' contains vertex indices for 3D rendering and optionally 2D geometry for
#' ggseg plots.
#'
#' For 3D-only atlases, this function simply reads the annotation files and
#' extracts vertex indices and colours - no mesh creation needed. For 2D
#' geometry, vertex-based rendering is used to take screenshots and extract
#' contour polygons.
#'
#' @param annot Annotation name (e.g., "aparc", "aparc.DKTatlas")
#' @param subject FreeSurfer subject (default "fsaverage5")
#' @param subjects_dir FreeSurfer subjects directory
#' @param include_geometry Whether to create 2D geometry for ggseg plots
#' @param output_dir Directory for temporary files (only used if include_geometry = TRUE)
#' @param hemisphere Which hemispheres to include
#' @param surface Surface for 2D geometry extraction
#' @param view Which views to include for 2D rendering
#' @param tolerance Tolerance for vertex reduction in 2D polygons
#' @param smoothness Smoothing factor for 2D contours
#' @param cleanup Remove temporary files after creation
#' @param verbose Print progress messages
#'
#' @return A brain_atlas object with vertices, colour, and optionally geometry columns
#' @export
#' @importFrom dplyr filter select mutate left_join group_by ungroup tibble bind_rows
#' @importFrom freesurfer fs_subj_dir read_annotation
#' @importFrom furrr future_pmap furrr_options
#' @importFrom ggseg.formats brain_atlas
#' @importFrom grDevices rgb
#' @importFrom progressr progressor
#' @importFrom sf st_as_sf st_combine
#' @importFrom tidyr expand_grid
#'
#' @examples
#' \dontrun{
#' # Create 3D-only atlas (fast, no mesh creation)
#' atlas <- make_brain_atlas(annot = "aparc.DKTatlas", include_geometry = FALSE)
#' ggseg3d(atlas = atlas)
#'
#' # Create full atlas with 2D geometry
#' atlas <- make_brain_atlas(annot = "aparc.DKTatlas")
#' ggseg(atlas = atlas)
#' ggseg3d(atlas = atlas)
#' }
make_brain_atlas <- function(
  annot,
  subject = "fsaverage5",
  subjects_dir = fs_subj_dir(),
  include_geometry = TRUE,
  output_dir = tempdir(),
  hemisphere = c("rh", "lh"),
  surface = "inflated",
  view = c("lateral", "medial"),
  tolerance = 0,
  smoothness = 5,
  cleanup = TRUE,
  verbose = TRUE
) {
  check_fs(abort = TRUE)
  check_magick()

  if (verbose) {
    cli::cli_alert_info("Creating brain atlas from {.val {annot}}...")
  }

  # Read annotation files - this gives us everything needed for 3D
  if (verbose) {
    cli::cli_alert_info("Reading annotation files...")
  }

  atlas_data <- read_annotation_data(
    annot = annot,
    subject = subject,
    subjects_dir = subjects_dir
  )

  if (nrow(atlas_data) == 0) {
    cli::cli_abort("No regions found in annotation files")
  }

  # Create 3D-only atlas
  atlas_3d <- brain_atlas(
    atlas = gsub("\\.", "_", annot),
    type = "cortical",
    data = atlas_data
  )

  if (!include_geometry) {
    if (verbose) {
      cli::cli_alert_success("3D atlas created with {nrow(atlas_data)} regions")
    }
    return(atlas_3d)
  }

  # Create 2D geometry using vertex-based rendering
  atlas_name <- gsub("\\.", "_", annot)
  dirs <- list(
    base = file.path(output_dir, atlas_name),
    snapshots = file.path(output_dir, atlas_name, "snapshots"),
    interim = file.path(output_dir, atlas_name, "interim"),
    masks = file.path(output_dir, atlas_name, "masks")
  )
  j <- sapply(dirs, mkdir)

  # Step 1: Full brain snapshots
  if (verbose) {
    cli::cli_alert_info("1/5 Taking full brain snapshots...")
  }

  snapshot_grid <- expand_grid(
    hemisphere = hemisphere,
    view = view
  )

  p <- progressor(steps = nrow(snapshot_grid))
  for (i in seq_len(nrow(snapshot_grid))) {
    snapshot_brain_unified(
      atlas = atlas_3d,
      hemisphere = snapshot_grid$hemisphere[i],
      view = snapshot_grid$view[i],
      surface = surface,
      output_dir = dirs$base
    )
    p()
  }

  # Step 2: Region snapshots
  if (verbose) {
    cli::cli_alert_info("2/5 Taking region snapshots...")
  }

  region_labels <- unique(atlas_data$label[!is.na(atlas_data$region)])

  region_grid <- expand.grid(
    region_label = region_labels,
    hemisphere = hemisphere,
    view = view,
    stringsAsFactors = FALSE
  )

  # Filter to matching hemisphere
  region_grid <- region_grid[
    (grepl("^lh_", region_grid$region_label) & region_grid$hemisphere == "lh") |
      (grepl("^rh_", region_grid$region_label) &
        region_grid$hemisphere == "rh"),
  ]

  p <- progressor(steps = nrow(region_grid))
  for (i in seq_len(nrow(region_grid))) {
    snapshot_region_unified(
      atlas = atlas_3d,
      region_label = region_grid$region_label[i],
      hemisphere = region_grid$hemisphere[i],
      view = region_grid$view[i],
      surface = surface,
      output_dir = dirs$snapshots
    )
    p()
  }

  # Step 3: Isolate regions
  if (verbose) {
    cli::cli_alert_info("3/5 Isolating regions...")
  }

  ffs <- list.files(dirs$snapshots, full.names = TRUE)
  file_grid <- data.frame(
    input_file = ffs,
    output_file = file.path(dirs$masks, basename(ffs)),
    interim_file = file.path(dirs$interim, basename(ffs))
  )

  p <- progressor(steps = nrow(file_grid))
  j <- future_pmap(
    file_grid,
    function(input_file, output_file, interim_file) {
      isolate_region(
        input_file = input_file,
        output_file = output_file,
        interim_file = interim_file
      )
      p()
    }
  )

  # Step 4: Extract and smooth contours
  if (verbose) {
    cli::cli_alert_info("4/5 Extracting and smoothing contours...")
  }

  conts <- extract_contours(
    dirs$masks,
    dirs$base,
    step = "4/5",
    verbose = FALSE
  )
  conts <- smooth_contours(dirs$base, smoothness, step = "4/5")
  conts <- reduce_vertex(dirs$base, tolerance, step = "4/5")

  # Step 5: Create atlas data frame
  if (verbose) {
    cli::cli_alert_info("5/5 Building atlas data...")
  }

  load(file.path(dirs$base, "contours_reduced.rda"))
  contours <- terra::vect(contours) |>
    terra::flip() |>
    sf::st_as_sf()

  # Parse filenames to get region info
  # Format is: {label}_{hemi}_{side}.png where label can contain underscores
  # Extract from right: side is last part, hemi_short is second-to-last
  fn_parts <- strsplit(contours$filenm, "_")
  contours$side <- sapply(fn_parts, function(x) x[length(x)])
  contours$hemi_short <- sapply(fn_parts, function(x) x[length(x) - 1])
  contours$label <- sapply(fn_parts, function(x) {
    paste(x[1:(length(x) - 2)], collapse = "_")
  })
  contours$hemi <- ifelse(contours$hemi_short == "lh", "left", "right")

  # Merge with atlas data
  contours <- left_join(
    contours,
    select(atlas_data, label, region, colour, vertices),
    by = "label"
  )

  # Adjust coordinates and combine geometries
  contours <- adjust_coords_sf(contours)
  contours <- group_by(contours, hemi, side, region, label)
  contours <- mutate(contours, geometry = st_combine(geometry))
  contours <- ungroup(contours)
  contours <- select(
    contours,
    hemi,
    side,
    region,
    label,
    colour,
    vertices,
    geometry
  )

  # Set wall/unknown to NA
  contours$region <- ifelse(
    grepl("wall|unknown", contours$region, ignore.case = TRUE),
    NA,
    contours$region
  )

  if (cleanup) {
    unlink(dirs$base, recursive = TRUE)
    if (verbose) cli::cli_alert_success("Temporary files removed")
  }

  atlas <- brain_atlas(
    atlas = atlas_name,
    type = "cortical",
    data = contours
  )

  if (verbose) {
    cli::cli_alert_success("Brain atlas created with {nrow(contours)} regions")
  }

  atlas
}


#' Read atlas data from FreeSurfer annotation
#'
#' Reads FreeSurfer annotation files and creates atlas data with
#' region labels, colours, and vertex indices for 3D rendering.
#' This is the core function for the vertex-based atlas system.
#'
#' @param annot Annotation name (e.g., "aparc", "aparc.DKTatlas")
#' @param subject FreeSurfer subject (default "fsaverage5")
#' @param subjects_dir FreeSurfer subjects directory
#'
#' @return A tibble with columns: hemi, region, label, colour, vertices
#' @export
#' @importFrom dplyr tibble bind_rows
#' @importFrom freesurfer fs_subj_dir read_annotation
#' @importFrom grDevices rgb
#'
#' @examples
#' \dontrun{
#' atlas_data <- read_annotation_data("aparc")
#' atlas_data <- read_annotation_data("aparc.DKTatlas")
#' }
read_annotation_data <- function(
  annot,
  subject = "fsaverage5",
  subjects_dir = fs_subj_dir()
) {
  check_fs(abort = TRUE)
  annot_dir <- file.path(subjects_dir, subject, "label")

  all_data <- list()

  for (hemi_short in c("lh", "rh")) {
    hemi <- if (hemi_short == "lh") "left" else "right"

    annot_file <- file.path(
      annot_dir,
      paste(hemi_short, annot, "annot", sep = ".")
    )

    if (!file.exists(annot_file)) {
      cli::cli_warn("Annotation file not found: {annot_file}")
      next
    }

    ant <- freesurfer::read_annotation(annot_file, verbose = FALSE)
    colortable <- ant$colortable[!is.na(ant$colortable$R), ]

    for (i in seq_len(nrow(colortable))) {
      region_name <- colortable$label[i]
      region_code <- colortable$code[i]

      region_vertices <- which(ant$label == region_code) - 1L
      if (length(region_vertices) == 0) {
        next
      }

      colour <- rgb(
        colortable$R[i],
        colortable$G[i],
        colortable$B[i],
        maxColorValue = 255
      )

      # Set medial wall/unknown to NA colour
      if (grepl("wall|unknown", region_name, ignore.case = TRUE)) {
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
  }

  bind_rows(all_data)
}


#' Create brain atlas from label files
#'
#' Creates a brain_atlas object from FreeSurfer .label files. Each label file
#' represents a single region containing vertex indices. This is useful for
#' displaying analysis results where specific regions are highlighted.
#'
#' Label files can contain either cortical surface vertices or subcortical
#' structure vertices. Set `type` accordingly.
#'
#' @param label_files Character vector of paths to .label files
#' @param atlas_name Name for the atlas (default: derived from first label file)
#' @param type Atlas type: "cortical" or "subcortical"
#' @param region_names Optional character vector of region names (default: derived from filenames)
#' @param colours Optional character vector of colours for each region
#' @param subject FreeSurfer subject (default "fsaverage5")
#' @param subjects_dir FreeSurfer subjects directory
#' @param include_geometry Whether to create 2D geometry for ggseg plots
#' @param output_dir Directory for temporary files (only used if include_geometry = TRUE)
#' @param surface Surface for 2D geometry extraction (cortical only)
#' @param view Which views to include for 2D rendering
#' @param tolerance Tolerance for vertex reduction in 2D polygons
#' @param smoothness Smoothing factor for 2D contours
#' @param cleanup Remove temporary files after creation
#' @param verbose Print progress messages
#'
#' @return A brain_atlas object with vertices, colour, and optionally geometry columns
#' @export
#' @importFrom dplyr tibble bind_rows
#' @importFrom freesurfer fs_subj_dir
#' @importFrom ggseg.formats brain_atlas
#' @importFrom grDevices rainbow
#' @importFrom tools file_path_sans_ext
#' @importFrom utils read.table
#'
#' @examples
#' \dontrun{
#' # Create cortical atlas from label files
#' labels <- c("lh.region1.label", "lh.region2.label", "rh.region1.label")
#' atlas <- make_atlas_from_labels(labels)
#' ggseg3d(atlas = atlas)
#'
#' # Create subcortical atlas
#' labels <- c("left_thalamus.label", "right_thalamus.label")
#' atlas <- make_atlas_from_labels(labels, type = "subcortical")
#'
#' # With custom names and colours
#' atlas <- make_atlas_from_labels(
#'   labels,
#'   region_names = c("Motor", "Visual", "Motor"),
#'   colours = c("#FF0000", "#00FF00", "#0000FF")
#' )
#' }
make_atlas_from_labels <- function(
  label_files,
  atlas_name = NULL,
  type = c("cortical", "subcortical"),
  region_names = NULL,
  colours = NULL,
  subject = "fsaverage5",
  subjects_dir = fs_subj_dir(),
  include_geometry = FALSE,
  output_dir = tempdir(),
  surface = "inflated",
  view = c("lateral", "medial"),
  tolerance = 0,
  smoothness = 5,
  cleanup = TRUE,
  verbose = TRUE
) {
  check_fs(abort = TRUE)
  type <- match.arg(type)

  if (!all(file.exists(label_files))) {
    missing <- label_files[!file.exists(label_files)]
    cli::cli_abort("Label files not found: {missing}")
  }

  if (is.null(atlas_name)) {
    atlas_name <- file_path_sans_ext(basename(label_files[1]))
    atlas_name <- gsub("^[lr]h\\.", "", atlas_name)
    atlas_name <- gsub("\\.", "_", atlas_name)
  }

  if (verbose) {
    cli::cli_alert_info(
      "Creating {type} brain atlas from {length(label_files)} label files..."
    )
  }

  all_data <- list()

  for (i in seq_along(label_files)) {
    label_file <- label_files[i]
    filename <- basename(label_file)

    hemi_short <- if (grepl("^lh\\.", filename)) {
      "lh"
    } else if (grepl("^rh\\.", filename)) {
      "rh"
    } else {
      NA
    }
    hemi <- if (identical(hemi_short, "lh")) {
      "left"
    } else if (identical(hemi_short, "rh")) {
      "right"
    } else {
      NA
    }

    if (is.null(region_names)) {
      region <- file_path_sans_ext(filename)
      region <- gsub("^[lr]h\\.", "", region)
    } else {
      region <- region_names[i]
    }

    label <- if (!is.na(hemi_short)) {
      paste(hemi_short, region, sep = "_")
    } else {
      region
    }

    if (is.null(colours)) {
      colour <- rainbow(length(label_files))[i]
    } else {
      colour <- colours[i]
    }

    vertices <- read_label_vertices(label_file)

    all_data[[i]] <- tibble(
      hemi = hemi,
      region = region,
      label = label,
      colour = colour,
      vertices = list(vertices)
    )
  }

  atlas_data <- bind_rows(all_data)

  if (nrow(atlas_data) == 0) {
    cli::cli_abort("No regions found in label files")
  }

  atlas_3d <- brain_atlas(
    atlas = atlas_name,
    type = type,
    data = atlas_data
  )

  if (!include_geometry) {
    if (verbose) {
      cli::cli_alert_success(
        "3D {type} atlas created with {nrow(atlas_data)} regions"
      )
    }
    return(atlas_3d)
  }

  if (type == "subcortical") {
    cli::cli_abort(
      "2D geometry for subcortical label atlases is not yet supported. Use include_geometry = FALSE."
    )
  }

  if (verbose) {
    cli::cli_alert_info("Creating 2D geometry (this may take a while)...")
  }

  dirs <- list(
    base = file.path(output_dir, atlas_name),
    snapshots = file.path(output_dir, atlas_name, "snapshots"),
    interim = file.path(output_dir, atlas_name, "interim"),
    masks = file.path(output_dir, atlas_name, "masks")
  )
  j <- sapply(dirs, mkdir)

  hemisphere <- unique(atlas_data$hemi[!is.na(atlas_data$hemi)])
  if (length(hemisphere) == 0) {
    hemisphere <- c("lh", "rh")
  }

  snapshot_grid <- expand_grid(
    hemisphere = hemisphere,
    view = view
  )

  p <- progressor(steps = nrow(snapshot_grid))
  for (i in seq_len(nrow(snapshot_grid))) {
    snapshot_brain_unified(
      atlas = atlas_3d,
      hemisphere = snapshot_grid$hemisphere[i],
      view = snapshot_grid$view[i],
      surface = surface,
      output_dir = dirs$base
    )
    p()
  }

  region_labels <- unique(atlas_data$label[!is.na(atlas_data$region)])
  region_grid <- expand.grid(
    region_label = region_labels,
    hemisphere = hemisphere,
    view = view,
    stringsAsFactors = FALSE
  )

  region_grid <- region_grid[
    (grepl("^lh_", region_grid$region_label) & region_grid$hemisphere == "lh") |
      (grepl("^rh_", region_grid$region_label) &
        region_grid$hemisphere == "rh") |
      (!grepl("^[lr]h_", region_grid$region_label)),
  ]

  p <- progressor(steps = nrow(region_grid))
  for (i in seq_len(nrow(region_grid))) {
    snapshot_region_unified(
      atlas = atlas_3d,
      region_label = region_grid$region_label[i],
      hemisphere = region_grid$hemisphere[i],
      view = region_grid$view[i],
      surface = surface,
      output_dir = dirs$snapshots
    )
    p()
  }

  ffs <- list.files(dirs$snapshots, full.names = TRUE)
  file_grid <- data.frame(
    input_file = ffs,
    output_file = file.path(dirs$masks, basename(ffs)),
    interim_file = file.path(dirs$interim, basename(ffs))
  )

  p <- progressor(steps = nrow(file_grid))
  j <- future_pmap(
    file_grid,
    function(input_file, output_file, interim_file) {
      isolate_region(
        input_file = input_file,
        output_file = output_file,
        interim_file = interim_file
      )
      p()
    }
  )

  conts <- extract_contours(dirs$masks, dirs$base, step = "", verbose = FALSE)
  conts <- smooth_contours(dirs$base, smoothness, step = "")
  conts <- reduce_vertex(dirs$base, tolerance, step = "")

  load(file.path(dirs$base, "contours_reduced.rda"))
  contours <- terra::vect(contours) |>
    terra::flip() |>
    sf::st_as_sf()

  fn_parts <- strsplit(contours$filenm, "_")
  contours$side <- sapply(fn_parts, function(x) x[length(x)])
  contours$hemi_short <- sapply(fn_parts, function(x) x[length(x) - 1])
  contours$label <- sapply(fn_parts, function(x) {
    paste(x[1:(length(x) - 2)], collapse = "_")
  })
  contours$hemi <- ifelse(contours$hemi_short == "lh", "left", "right")

  contours <- left_join(
    contours,
    select(atlas_data, label, region, colour, vertices),
    by = "label"
  )

  contours <- adjust_coords_sf(contours)
  contours <- group_by(contours, hemi, side, region, label)
  contours <- mutate(contours, geometry = st_combine(geometry))
  contours <- ungroup(contours)
  contours <- select(
    contours,
    hemi,
    side,
    region,
    label,
    colour,
    vertices,
    geometry
  )

  if (cleanup) {
    unlink(dirs$base, recursive = TRUE)
    if (verbose) cli::cli_alert_success("Temporary files removed")
  }

  atlas <- brain_atlas(
    atlas = atlas_name,
    type = type,
    data = contours
  )

  if (verbose) {
    cli::cli_alert_success("{type} atlas created with {nrow(contours)} regions")
  }

  atlas
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
  vertices <- sapply(strsplit(data_lines, "\\s+"), function(x) as.integer(x[1]))

  vertices
}


## quiets concerns of R CMD check
if (getRversion() >= "2.15.1") {
  utils::globalVariables(c(
    "vertices",
    "filenm"
  ))
}
