# General utilities ----

mkdir <- function(path, ...) {
  dir.create(path, recursive = TRUE, showWarnings = FALSE, ...)
}


# Interactive preview ----

#' Preview atlas plots interactively
#'
#' Shows ggseg (2D) and ggseg3d (3D) plots of the atlas one at a time,
#' waiting for user input between each. Only runs in interactive sessions.
#'
#' @param atlas A brain_atlas object
#' @return Invisible atlas
#' @noRd
preview_atlas <- function(atlas) {
  if (!interactive()) {
    return(invisible(atlas))
  }

  has_sf <- !is.null(atlas$data$sf)
  has_3d <- !is.null(atlas$data$vertices) ||
    !is.null(atlas$data$meshes)

  if (!has_sf && !has_3d) {
    cli::cli_alert_danger(
      "Atlas malformed and doesn't contain compatible data."
    )
    return(invisible(atlas))
  }

  if (has_3d) {
    tryCatch(
      {
        if (atlas$type == "cortical") {
          for (hemi in c("left", "right")) {
            p3d <- ggseg3d::ggseg3d(atlas = atlas, hemisphere = hemi) |>
              ggseg3d::pan_camera(paste(hemi, "lateral")) |>
              ggseg3d::set_legend(show = FALSE)
            print(p3d)
            readline(sprintf("3D %s hemisphere. Press Enter for next", hemi))
          }
        } else {
          p3d <- ggseg3d::ggseg3d(atlas = atlas) |>
            ggseg3d::set_legend(show = FALSE)
          print(p3d)
          readline("Press Enter to continue")
        }
      },
      error = function(e) NULL
    )
  }

  if (has_sf) {
    tryCatch(
      {
        gp <- ggplot2::ggplot() +
          ggseg::geom_brain(
            atlas = atlas,
            position = ggseg::position_brain(ncol = 4),
            show.legend = FALSE,
            alpha = .7,
            ggplot2::aes(fill = label)
          )
        if (!is.null(atlas$palette)) {
          gp <- gp +
            ggplot2::scale_fill_manual(values = atlas$palette)
        }
      },
      error = function(e) {
        gp <- plot(atlas$data$sf)
      }
    )
    print(gp)
  }
  invisible(atlas)
}


# Verbosity control ----

#' Get verbosity level
#'
#' Returns the current verbosity level from options or environment variable.
#' Checks in order: function argument, option `ggsegExtra.verbosity`,
#' environment variable `GGSEGEXTRA_VERBOSITY`.
#'
#' Verbosity levels:
#' - 0: Silent (no output)
#' - 1: Normal (progress messages, FreeSurfer verbose = FALSE)
#' - 2: High (detailed output, FreeSurfer verbose = TRUE)
#'
#' @param verbose Optional explicit value. If NULL, reads from options/env.
#'   Can be logical (TRUE = 1, FALSE = 0) or numeric (0, 1, 2).
#' @return Integer verbosity level (0, 1, or 2)
#' @export
#' @examples
#' get_verbosity()
#' get_verbosity(TRUE)
#' get_verbosity(2)
get_verbosity <- function(verbose = NULL) {
  if (!is.null(verbose)) {
    if (is.logical(verbose)) {
      return(as.integer(verbose))
    }
    return(as.integer(verbose))
  }

  opt <- getOption("ggsegExtra.verbosity")
  if (!is.null(opt)) {
    if (is.logical(opt)) {
      return(as.integer(opt))
    }
    return(as.integer(opt))
  }

  env <- Sys.getenv("GGSEGEXTRA_VERBOSITY", unset = NA)
  if (!is.na(env)) {
    val <- suppressWarnings(as.integer(env))
    if (!is.na(val)) {
      return(val)
    }
  }

  1L
}

#' Check if verbose output is enabled
#'
#' @param verbose Optional explicit value. If NULL, reads from options/env.
#' @return Logical TRUE if verbosity >= 1
#' @export
#' @examples
#' is_verbose()
#' is_verbose(FALSE)
is_verbose <- function(verbose = NULL) {
  get_verbosity(verbose) >= 1L
}

#' Check if high verbosity is enabled
#' @param verbose Optional explicit value
#' @return Logical
#' @noRd
is_very_verbose <- function(verbose = NULL) {
  get_verbosity(verbose) >= 2L
}


# Step data handling ----

#' Load or run a pipeline step
#'
#' Handles the logic for loading cached data or running a step:
#' - If skip_existing and files exist, load and return data
#' - If step is in steps list, return NULL to signal step should run
#' - If step not in steps and files don't exist, throw error
#'
#' @param step_num Integer step number
#' @param steps Integer vector of steps to run
#' @param files Character vector of file paths that must exist
#' @param skip_existing Logical, try to load existing files first
#' @param step_name Human-readable step name for error messages
#'
#' @return List with loaded data if files exist and should be skipped,
#'   NULL if step should run, or throws error if files missing
#' @noRd
load_or_run_step <- function(
  step_num,
  steps,
  files,
  skip_existing,
  step_name = paste("Step", step_num)
) {
  files_exist <- all(file.exists(files))
  step_requested <- step_num %in% steps

  if (files_exist && skip_existing) {
    data <- lapply(files, readRDS)
    names(data) <- basename(files)
    return(list(run = FALSE, data = data))
  }

  if (step_requested) {
    return(list(run = TRUE, data = NULL))
  }

  if (!files_exist) {
    missing <- files[!file.exists(files)]
    cli::cli_abort(c(
      "{step_name} was not run but required files are missing",
      "i" = "Missing: {.path {missing}}",
      "i" = "Include step {step_num} in the steps argument to generate these files"
    ))
  }

  data <- lapply(files, readRDS)
  names(data) <- basename(files)
  list(run = FALSE, data = data)
}

#' Get FreeSurfer verbose setting based on verbosity level
#' @param verbose Optional explicit value
#' @return Logical for FreeSurfer verbose parameter
#' @noRd
fs_verbose <- function(verbose = NULL) {
  is_very_verbose(verbose)
}

#' Set verbosity level
#'
#' @param level Verbosity level: 0 (silent), 1 (normal), 2 (high)
#' @return Previous verbosity level (invisibly)
#' @export
#' @examples
#' # Set to silent
#' set_verbosity(0)
#'
#' # Set to normal
#' set_verbosity(1)
#'
#' # Set to high (includes FreeSurfer verbose output)
#' set_verbosity(2)
set_verbosity <- function(level) {
  if (!level %in% c(0L, 1L, 2L)) {
    cli::cli_abort("{.arg level} must be 0, 1, or 2")
  }
  old <- getOption("ggsegExtra.verbosity", default = 1L)
  options(ggsegExtra.verbosity = as.integer(level))
  invisible(old)
}


# Pipeline parameter defaults ----

#' Get cleanup setting
#'
#' Returns the cleanup setting from options or environment variable.
#' Controls whether intermediate files are removed after pipeline completion.
#'
#' @param cleanup Optional explicit value. If NULL, reads from options/env.
#' @return Logical TRUE to remove intermediate files
#' @noRd
get_cleanup <- function(cleanup = NULL) {
  get_bool_option(cleanup, "ggsegExtra.cleanup", "GGSEGEXTRA_CLEANUP", TRUE)
}

#' Get skip_existing setting
#'
#' Returns the skip_existing setting from options or environment variable.
#' Controls whether to reuse existing intermediate files.
#'
#' @param skip_existing Optional explicit value. If NULL, reads from options/env.
#' @return Logical TRUE to skip existing files
#' @noRd
get_skip_existing <- function(skip_existing = NULL) {
  get_bool_option(
    skip_existing,
    "ggsegExtra.skip_existing",
    "GGSEGEXTRA_SKIP_EXISTING",
    TRUE
  )
}

#' Get tolerance setting
#'
#' Returns the tolerance setting from options or environment variable.
#' Controls vertex reduction during contour simplification.
#'
#' @param tolerance Optional explicit value. If NULL, reads from options/env.
#' @return Numeric tolerance value (0 = no simplification)
#' @noRd
get_tolerance <- function(tolerance = NULL) {
  get_numeric_option(
    tolerance,
    "ggsegExtra.tolerance",
    "GGSEGEXTRA_TOLERANCE",
    0
  )
}

#' Get smoothness setting
#'
#' Returns the smoothness setting from options or environment variable.
#' Controls contour smoothing during geometry extraction.
#'
#' @param smoothness Optional explicit value. If NULL, reads from options/env.
#' @return Numeric smoothness value
#' @noRd
get_smoothness <- function(smoothness = NULL) {
  get_numeric_option(
    smoothness,
    "ggsegExtra.smoothness",
    "GGSEGEXTRA_SMOOTHNESS",
    5
  )
}

#' Helper to get boolean option with fallback
#' @noRd
get_bool_option <- function(explicit, option_name, env_name, default) {
  if (!is.null(explicit)) {
    return(as.logical(explicit))
  }

  opt <- getOption(option_name)
  if (!is.null(opt)) {
    return(as.logical(opt))
  }

  env <- Sys.getenv(env_name, unset = NA)
  if (!is.na(env)) {
    return(tolower(env) %in% c("true", "1", "yes"))
  }

  default
}

#' Helper to get numeric option with fallback
#' @noRd
get_numeric_option <- function(explicit, option_name, env_name, default) {
  if (!is.null(explicit)) {
    return(as.numeric(explicit))
  }

  opt <- getOption(option_name)
  if (!is.null(opt)) {
    return(as.numeric(opt))
  }

  env <- Sys.getenv(env_name, unset = NA)
  if (!is.na(env)) {
    val <- suppressWarnings(as.numeric(env))
    if (!is.na(val)) {
      return(val)
    }
  }

  default
}

#' Helper to get string option with fallback
#' @noRd
get_string_option <- function(explicit, option_name, env_name, default) {
  if (!is.null(explicit)) {
    return(as.character(explicit))
  }

  opt <- getOption(option_name)
  if (!is.null(opt)) {
    return(as.character(opt))
  }

  env <- Sys.getenv(env_name, unset = NA)
  if (!is.na(env) && nzchar(env)) {
    return(env)
  }

  default
}

#' Get output_dir setting
#'
#' Returns the output directory from options or environment variable.
#' Used as default output directory for atlas creation pipelines.
#'
#' @param output_dir Optional explicit value. If NULL, reads from options/env.
#' @return Character path to output directory
#' @noRd
get_output_dir <- function(output_dir = NULL) {
  get_string_option(
    output_dir,
    "ggsegExtra.output_dir",
    "GGSEGEXTRA_OUTPUT_DIR",
    tempdir()
  )
}


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
  base <- if (is.null(atlas_name)) output_dir else file.path(output_dir, atlas_name)

  dirs <- list(
    base = base,
    snapshots = file.path(base, "snapshots"),
    interim = file.path(base, "interim"),
    masks = file.path(base, "masks"),
    snaps = file.path(base, "snapshots"),
    inter = file.path(base, "interim")
  )

  if (type == "subcortical") {
    dirs$meshes <- file.path(base, "meshes")
  }

  if (type == "tract") {
    dirs$volumes <- file.path(base, "volumes")
  }

  for (d in dirs) {
    mkdir(d)
  }
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

  palette <- stats::setNames(atlas_data$colour, atlas_data$label)
  palette <- palette[!duplicated(names(palette))]

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


# Contour processing pipeline ----

#' Run full contour processing pipeline
#'
#' Consolidates the extract -> smooth -> reduce pattern used across
#' cortical, subcortical, and tract atlas creation.
#'
#' @param mask_dir Directory containing mask images
# Image processing ----
#' Process snapshot image for contour extraction
#'
#' Applies transparency and optional dilation to prepare image for
#' contour extraction.
#'
#' @param input_file Path to input image
#' @param output_file Path for output image
#' @param dilate Optional dilation iterations
#' @param transparent_color Color to make transparent (default "black")
#' @param fuzz Fuzz factor for transparency (default 10)
#' @param skip_existing If TRUE, skip if output file already exists
#' @noRd
#' @importFrom magick image_read image_convert image_transparent image_morphology image_write
process_snapshot_image <- function(
  input_file,
  output_file,
  dilate = NULL,
  transparent_color = "black",
  fuzz = 10,
  skip_existing = TRUE
) {
  if (skip_existing && file.exists(output_file)) {
    return(invisible(output_file))
  }

  img <- image_read(input_file) |>
    image_convert() |>
    image_transparent(color = transparent_color, fuzz = fuzz)

  if (!is.null(dilate) && dilate > 0) {
    img <- image_morphology(
      img,
      method = "DilateI",
      kernel = "diamond",
      iterations = dilate
    )
  }

  image_write(image = img, path = output_file)
  invisible(output_file)
}


#' Extract alpha channel from image using ImageMagick
#' @noRd
extract_alpha_mask <- function(input_file, output_file, skip_existing = TRUE) {
  if (skip_existing && file.exists(output_file)) {
    return(invisible(output_file))
  }

  cmd <- paste(
    "magick",
    shQuote(input_file),
    "-alpha extract",
    shQuote(output_file)
  )
  system(cmd, intern = FALSE, ignore.stdout = TRUE, ignore.stderr = TRUE)
  invisible(output_file)
}


# Contour loading ----

#' Load and parse reduced contours file
#'
#' Loads the contours_reduced.rda file, flips coordinates, and parses
#' filenames into view/hemi/label components.
#'
#' @param base_dir Directory containing contours_reduced.rda
#' @return sf object with view, hemi_short, hemi, label columns added
#' @noRd
load_reduced_contours <- function(base_dir) {
  load(file.path(base_dir, "contours_reduced.rda"))
  contours <- terra::vect(contours) |>
    terra::flip() |>
    sf::st_as_sf()

  parsed <- parse_contour_filenames(contours$filenm)
  contours$view <- parsed$view
  contours$hemi_short <- parsed$hemi_short
  contours$hemi <- parsed$hemi
  contours$label <- parsed$label

  contours
}


# Filename parsing ----

#' Parse contour filename into components
#'
#' Extracts view, hemisphere, and label from standardized filename format.
#' Supports formats like: "label_hemi_view.png" or "coords_view_label.png"
#'
#' @param filenames Character vector of filenames (without path)
#' @return Data frame with label, hemi_short, hemi, view columns
#' @noRd
parse_contour_filenames <- function(filenames) {
  filenames_no_ext <- tools::file_path_sans_ext(filenames)
  fn_parts <- strsplit(filenames_no_ext, "_")

  result <- data.frame(
    filenm = filenames,
    view = vapply(fn_parts, function(x) x[length(x)], character(1)),
    hemi_short = vapply(fn_parts, function(x) x[length(x) - 1], character(1)),
    stringsAsFactors = FALSE
  )

  result$hemi <- ifelse(
    result$hemi_short == "lh",
    "left",
    ifelse(result$hemi_short == "rh", "right", result$hemi_short)
  )

  result$label <- vapply(
    fn_parts,
    function(x) {
      paste(x[1:(length(x) - 2)], collapse = "_")
    },
    character(1)
  )

  result
}


# ImageMagick utilities ----

#' @noRd
has_magick <- function() {
  k <- magick_version()
  any(grepl("Version: ImageMagick", k))
}

check_magick <- function() {
  k <- has_magick()
  if (!k) {
    cli::cli_abort(c(
      "ImageMagick not installed, cannot run pipeline.",
      "i" = "See {.url https://imagemagick.org/script/download.php}"
    ))
  }
}

#' @noRd
magick_version <- function() {
  system2("magick", "--version", stdout = TRUE)[1]
}


# Command execution ----

#' @noRd
run_cmd <- function(cmd, verbose = FALSE, no_ui = FALSE) {
  if (no_ui) {
    if (Sys.info()["sysname"] == "Darwin") {
      fv_args <- sub("^freeview[[:space:]]*", "", cmd)
      cmd <- paste(
        "open -g -j -n -W $FREESURFER_HOME/Freeview.app --args",
        fv_args
      )
    } else {
      cmd <- paste("fsxvfb", cmd)
    }
  }
  system(
    paste0(get_fs(), cmd),
    intern = TRUE,
    ignore.stdout = !verbose,
    ignore.stderr = !verbose
  )
}


# Contour extraction ----

#' @noRd
#' @importFrom terra global rast as.polygons
#' @importFrom sf st_as_sf st_is_empty st_geometry
get_contours <- function(
  raster_object,
  max_val = 255,
  vertex_size_limits = c(3 * 10^6, 3 * 10^7),
  verbose = TRUE
) {
  mx <- global(raster_object, fun = "max", na.rm = TRUE)[1, 1]

  if (mx < max_val) {
    return(NULL)
  }

  tmp.rst <- raster_object
  tmp.rst[tmp.rst == 0] <- NA

  contours_raw <- as.polygons(tmp.rst, values = TRUE, na.rm = TRUE)

  coords <- st_as_sf(contours_raw)

  if (all(nrow(coords) > 0 & !st_is_empty(coords))) {
    coords <- to_coords(coords, 1)
    coords <- coords2sf(coords, vertex_size_limits)

    return(coords)
  }
  return(NULL)
}


#' Isolate region to alpha channel
#'
#' @param input_file image file path
#' @param output_file output file path
#' @param interim_file interim image path
#' @param skip_existing If TRUE, skip if output file already exists
#' @noRd
#' @importFrom magick image_read image_convert image_transparent image_write
isolate_region <- function(
  input_file,
  output_file,
  interim_file = tempfile(),
  skip_existing = TRUE
) {
  if (skip_existing && file.exists(output_file)) {
    return(invisible(output_file))
  }

  tmp <- image_read(input_file)
  tmp <- image_convert(tmp, "png")

  tmp <- image_transparent(tmp, "white", fuzz = 30)
  k <- image_write(tmp, interim_file)

  if (has_magick()) {
    cmd <- paste("magick", interim_file, "-alpha extract", output_file)

    k <- run_cmd(cmd)
    invisible(k)
  } else {
    cli::cli_abort(
      "Cannot complete last extraction step, missing imagemagick. Please install"
    )
  }
}


# View generation utilities ----

#' Create chunked view ranges for projections
#'
#' Generates a data.frame of view specifications by dividing a range into
#' chunks. Used by both subcortical and tract atlas pipelines.
#'
#' @param lo Start of range
#' @param hi End of range
#' @param chunk_size Size of each chunk
#' @param type View type: "axial", "coronal", or "sagittal"
#'
#' @return data.frame with columns: name, type, start, end
#' @noRd
make_view_chunks <- function(lo, hi, chunk_size, type) {
  starts <- seq(lo, hi, by = chunk_size)
  ends <- pmin(starts + chunk_size - 1, hi)
  n <- length(starts)
  data.frame(
    name = paste0(type, "_", seq_len(n)),
    type = type,
    start = starts,
    end = ends,
    stringsAsFactors = FALSE
  )
}


#' Create cortex reference slices from views
#'
#' Generates cortex slice positions that match the view specifications.
#' For sagittal views, uses hemisphere-appropriate x positions.
#' For axial/coronal views, uses the midpoint of the projection range.
#'
#' @param views data.frame with columns: name, type, start, end
#' @param dims Volume dimensions (3-element vector)
#' @param cortex_x X coordinate for non-hemisphere-specific sagittal slices
#'
#' @return data.frame with columns: x, y, z, view, name
#' @noRd
create_cortex_slices <- function(views, dims, cortex_x = NULL) {
  if (is.null(cortex_x)) {
    scale <- dims[1] / 256
    cortex_x <- round(119 * scale)
  }

  slices <- lapply(seq_len(nrow(views)), function(i) {
    v <- views[i, ]

    if (v$type == "sagittal") {
      if (grepl("left", v$name, ignore.case = TRUE)) {
        x_pos <- round(dims[1] * 0.65)
      } else if (grepl("right", v$name, ignore.case = TRUE)) {
        x_pos <- round(dims[1] * 0.35)
      } else {
        x_pos <- cortex_x
      }
      data.frame(
        x = x_pos,
        y = NA,
        z = NA,
        view = v$type,
        name = v$name,
        stringsAsFactors = FALSE
      )
    } else {
      mid_pos <- round((v$start + v$end) / 2)
      data.frame(
        x = NA,
        y = if (v$type == "coronal") mid_pos else NA,
        z = if (v$type == "axial") mid_pos else NA,
        view = v$type,
        name = v$name,
        stringsAsFactors = FALSE
      )
    }
  })
  do.call(rbind, slices)
}


#' Detect cortex labels from segmentation volume
#'
#' Auto-detects cortical voxel labels from a segmentation volume.
#' Handles both aparc+aseg (labels 1000-2999) and plain aseg (labels 3, 42).
#'
#' @param vol 3D array of segmentation labels
#'
#' @return Named list with "left" and "right" vectors of label values
#' @noRd
detect_cortex_labels <- function(vol) {
  vol_labels <- unique(as.vector(vol))
  has_aparc <- any(vol_labels >= 1000 & vol_labels < 3000)

  if (has_aparc) {
    list(
      left = intersect(1000:1999, vol_labels),
      right = intersect(2000:2999, vol_labels)
    )
  } else {
    list(left = 3, right = 42)
  }
}


#' Extract hemisphere from sagittal view name
#'
#' Determines hemisphere from view name for sagittal projections.
#' Returns NULL for non-sagittal views.
#'
#' @param view_type View type: "axial", "coronal", or "sagittal"
#' @param view_name View name (e.g., "sagittal_left", "axial_3")
#'
#' @return "left", "right", or NULL
#' @noRd
extract_hemi_from_view <- function(view_type, view_name) {
  if (view_type != "sagittal") {
    return(NULL)
  }
  if (grepl("left", view_name, ignore.case = TRUE)) {
    "left"
  } else if (grepl("right", view_name, ignore.case = TRUE)) {
    "right"
  } else {
    NULL
  }
}


# Atlas validation ----

#' @noRd
warn_if_large_atlas <- function(atlas, max_vertices = 10000) {
  if (is.null(atlas$data$sf)) {
    return(invisible(NULL))
  }

  n_vertices <- sum(count_vertices(atlas$data$sf))

  if (n_vertices > max_vertices) {
    cli::cli_warn(c(
      "Atlas has {.val {n_vertices}} vertices (threshold: {.val {max_vertices}})",
      "i" = "Large atlases may be slow to plot and increase package size",
      "i" = "Re-run with higher {.arg tolerance} to reduce vertices"
    ))
  }

  invisible(NULL)
}
