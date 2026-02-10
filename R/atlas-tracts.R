# White matter tract atlas creation ----

#' Create brain atlas from white matter tracts
#'
#' @description
#' `r lifecycle::badge("experimental")`
#'
#' Turn tractography streamlines into a brain atlas where each tract is
#' rendered as a 3D tube. The function computes a centerline from the
#' streamlines and generates a tube mesh around it.
#'
#' You can provide tract data in several formats: TRK files from TrackVis,
#' TCK files from MRtrix, or coordinate matrices directly in R. The function
#' reads the streamlines, extracts a representative centerline (by averaging
#' or selecting the medoid), and builds a tube mesh for 3D rendering.
#'
#' For tracts with many streamlines, set `tube_radius = "density"` to make
#' the tube thicker where more streamlines pass through.
#'
#' @param input_tracts Paths to tractography files (`.trk` or `.tck`), or a
#'   named list of coordinate matrices where each matrix has N rows and 3
#'   columns (x, y, z).
#' @param input_aseg Path to a segmentation volume (`.mgz`, `.nii`) used to
#'   draw cortex outlines in 2D views. Required for steps 2+.
#' @template atlas_name
#' @param input_lut Path to a color lookup table (LUT) file, or a data.frame
#'   with columns `region` and colour columns (R, G, B or hex).
#'   Use this to provide tract names and colours. If NULL, names are derived
#'   from filenames or list names, and the atlas will have no palette.
#' @param tube_radius Controls the tube thickness. Either a single numeric
#'   value for uniform radius, or `"density"` to scale radius by how many
#'   streamlines pass through each point.
#' @param tube_segments Number of segments around the tube circumference.
#'   Higher values make smoother tubes but larger meshes. Default 8 is a
#'   good balance.
#' @param n_points Number of points to resample the centerline to. All tracts
#'   are resampled to this length for consistent tube generation.
#' @param centerline_method How to extract the centerline from multiple
#'   streamlines: `"mean"` averages coordinates point-by-point, `"medoid"`
#'   selects the single most representative streamline.
#' @param views A data.frame specifying projection views. If NULL, uses
#'   [default_tract_views()].
#' @template output_dir
#' @template verbose
#' @template smoothness
#' @template tolerance
#' @template cleanup
#' @template skip_existing
#' @template dilate
#' @template vertex_size_limits
#' @param steps Which pipeline steps to run. Default NULL runs all steps.
#'   Steps are:
#'   \itemize{
#'     \item 1: Read tractography and create tube meshes
#'     \item 2: Create projection snapshots
#'     \item 3: Process images
#'     \item 4: Extract contours
#'     \item 5: Smooth contours
#'     \item 6: Reduce vertices
#'     \item 7: Build atlas
#'   }
#'   Use `steps = 1` for 3D-only atlas. Use `steps = 5:7` to iterate on
#'   smoothing and vertex reduction.
#'
#' @return A `ggseg_atlas` object with type `"tract"`, containing region
#'   metadata, tube meshes for 3D rendering, colours, and optionally sf
#'   geometry for 2D projection plots.
#' @export
#' @importFrom dplyr tibble bind_rows distinct
#' @importFrom furrr future_map2 furrr_options
#' @importFrom grDevices rgb
#' @importFrom progressr progressor
#' @importFrom tools file_path_sans_ext file_ext
#'
#' @examples
#' \dontrun{
#' # From TRK files (names derived from filenames)
#' atlas <- create_tract_atlas(
#'   input_tracts = c("cst_left.trk", "cst_right.trk")
#' )
#'
#' # With custom names and colours via LUT
#' atlas <- create_tract_atlas(
#'   input_tracts = c("cst_left.trk", "cst_right.trk"),
#'   input_lut = "tract_colors.txt"
#' )
#'
#' # View with ggseg3d
#' ggseg3d(atlas = atlas)
#' }
create_tract_atlas <- function(
  input_tracts,
  input_aseg = NULL,
  atlas_name = NULL,
  input_lut = NULL,
  tube_radius = 5,
  tube_segments = 8,
  n_points = 50,
  centerline_method = c("mean", "medoid"),
  views = NULL,
  output_dir = NULL,
  verbose = get_verbose(), # nolint: object_usage_linter
  tolerance = NULL,
  smoothness = NULL,
  cleanup = NULL,
  skip_existing = NULL,
  dilate = NULL,
  vertex_size_limits = NULL,
  steps = NULL
) {
  start_time <- Sys.time()

  config <- validate_tract_config(
    output_dir,
    verbose,
    cleanup,
    skip_existing,
    tolerance,
    smoothness,
    steps,
    centerline_method,
    tube_radius,
    tube_segments,
    n_points
  )

  if (is.null(atlas_name)) {
    atlas_name <- basename(config$output_dir)
  }

  dirs <- setup_atlas_dirs(config$output_dir, atlas_name, type = "tract")
  lut_result <- parse_lut_colours(input_lut)
  tract_log_header(config, input_tracts, input_aseg)

  step1 <- tract_resolve_step1(
    config,
    dirs,
    input_tracts,
    lut_result$region_names,
    lut_result$colours
  )

  if (max(config$steps) == 1L) {
    atlas <- tract_assemble_3d(step1)
    return(tract_finalize(atlas, config, dirs, start_time))
  }

  tract_check_aseg(input_aseg, config$steps)

  snaps <- tract_resolve_snapshots(
    config,
    dirs,
    step1,
    input_aseg,
    views
  )

  tract_run_image_steps(config, dirs, dilate, vertex_size_limits)

  if (7L %in% config$steps) {
    atlas <- tract_assemble_full(step1, dirs, snaps$views, snaps$cortex_slices)
    return(tract_finalize(atlas, config, dirs, start_time))
  }

  tract_finalize(NULL, config, dirs, start_time)
}


# Tract pipeline helpers ----

#' @noRd
validate_tract_config <- function(
  output_dir,
  verbose,
  cleanup,
  skip_existing,
  tolerance,
  smoothness,
  steps,
  centerline_method,
  tube_radius,
  tube_segments,
  n_points
) {
  verbose <- is_verbose(verbose)
  cleanup <- get_cleanup(cleanup)
  skip_existing <- get_skip_existing(skip_existing)
  tolerance <- get_tolerance(tolerance)
  smoothness <- get_smoothness(smoothness)
  output_dir <- get_output_dir(output_dir)
  output_dir <- normalizePath(output_dir, mustWork = FALSE)

  centerline_method <- match.arg(centerline_method, c("mean", "medoid"))

  if (is.null(steps)) {
    steps <- 1L:7L
  }
  steps <- as.integer(steps)

  list(
    output_dir = output_dir,
    verbose = verbose,
    cleanup = cleanup,
    skip_existing = skip_existing,
    tolerance = tolerance,
    smoothness = smoothness,
    steps = steps,
    centerline_method = centerline_method,
    tube_radius = tube_radius,
    tube_segments = tube_segments,
    n_points = n_points,
    density_radius_range = c(0.2, 1.0),
    tract_radius = 3
  )
}


#' @noRd
tract_log_header <- function(config, input_tracts, input_aseg) {
  if (!config$verbose) {
    return(invisible(NULL))
  }
  cli::cli_h1("Creating tractography atlas")
  cli::cli_alert_info("Tract files: {.path {input_tracts}}")
  if (!is.null(input_aseg)) {
    cli::cli_alert_info("Anatomical reference: {.path {input_aseg}}")
  }
}


#' @noRd
tract_resolve_step1 <- function(
  config,
  dirs,
  input_tracts,
  tract_names,
  colours
) {
  files <- file.path(dirs$base, "step1_data.rds")
  cached <- load_or_run_step(
    1L,
    config$steps,
    files,
    config$skip_existing,
    "Step 1 (Read tracts & create tube meshes)"
  )

  if (!cached$run) {
    if (config$verbose) {
      cli::cli_alert_success("1/7 Loaded existing tract data")
    }
    return(cached$data[["step1_data.rds"]])
  }

  input_result <- tract_read_input(input_tracts, tract_names)
  streamlines_data <- input_result$streamlines_data
  tract_names <- input_result$tract_names

  coords_are_voxels <- detect_tract_coord_space(
    streamlines_data,
    config$verbose
  )

  if (is.null(colours)) {
    colours <- rep(NA_character_, length(streamlines_data))
  }

  if (config$verbose) {
    cli::cli_progress_step(
      "1/7 Creating tube meshes for {length(streamlines_data)} tracts"
    )
  }

  meshes_list <- tract_create_meshes(
    streamlines_data,
    tract_names,
    config$centerline_method,
    config$n_points,
    config$tube_radius,
    config$tube_segments,
    config$density_radius_range
  )

  if (config$verbose) {
    cli::cli_progress_done()
  }

  built <- tract_build_core(meshes_list, colours, tract_names)

  step1_data <- list(
    streamlines_data = streamlines_data,
    centerlines_df = built$centerlines_df,
    core = built$core,
    palette = built$palette,
    atlas_name = built$atlas_name,
    tube_radius = config$tube_radius,
    tube_segments = config$tube_segments,
    coords_are_voxels = coords_are_voxels
  )

  saveRDS(step1_data, file.path(dirs$base, "step1_data.rds"))
  step1_data
}


#' @noRd
detect_tract_coord_space <- function(streamlines_data, verbose) {
  all_streamlines <- unlist(streamlines_data, recursive = FALSE)
  coords_are_voxels <- detect_coords_are_voxels(all_streamlines)
  if (verbose) {
    space <- if (coords_are_voxels) "voxel" else "RAS" # nolint: object_usage_linter
    cli::cli_alert_info("Auto-detected coordinate space: {.val {space}}")
  }
  coords_are_voxels
}


#' @noRd
tract_check_aseg <- function(input_aseg, steps) {
  if (any(2L:7L %in% steps) && is.null(input_aseg)) {
    cli::cli_abort(c(
      "{.arg input_aseg} is required for steps 2-7",
      "i" = "Provide a segmentation volume (e.g., aparc+aseg.nii.gz)"
    ))
  }
}


#' @noRd
tract_resolve_snapshots <- function(config, dirs, step1, input_aseg, views) {
  files <- c(
    file.path(dirs$base, "views.rds"),
    file.path(dirs$base, "cortex_slices.rds")
  )
  cached <- load_or_run_step(
    2L,
    config$steps,
    files,
    config$skip_existing,
    "Step 2 (Create projection snapshots)"
  )

  if (!cached$run) {
    if (any(config$steps > 2L)) {
      if (config$verbose) {
        cli::cli_alert_success("2/7 Loaded existing snapshots")
      }
      return(list(
        views = cached$data[["views.rds"]],
        cortex_slices = cached$data[["cortex_slices.rds"]]
      ))
    }
    return(list(views = NULL, cortex_slices = NULL))
  }

  if (config$verbose) {
    cli::cli_progress_step("2/7 Creating projection snapshots")
  }

  coords_are_voxels <- step1$coords_are_voxels
  if (is.null(coords_are_voxels)) {
    coords_are_voxels <- detect_tract_coord_space(
      step1$streamlines_data,
      config$verbose
    )
  }

  result <- tract_create_snapshots(
    step1$streamlines_data,
    step1$centerlines_df,
    input_aseg,
    views,
    dirs,
    coords_are_voxels,
    config$skip_existing,
    config$tract_radius,
    config$verbose
  )

  saveRDS(result$views, file.path(dirs$base, "views.rds"))
  saveRDS(result$cortex_slices, file.path(dirs$base, "cortex_slices.rds"))
  if (config$verbose) {
    cli::cli_progress_done()
  }
  result
}


#' @noRd
tract_run_image_steps <- function(config, dirs, dilate, vertex_size_limits) {
  if (3L %in% config$steps) {
    if (config$verbose) {
      cli::cli_progress_step("3/7 Processing images")
    }
    process_and_mask_images(
      # nolint: object_usage_linter.
      dirs$snaps,
      dirs$processed,
      dirs$masks,
      dilate = dilate,
      skip_existing = config$skip_existing
    )
    if (config$verbose) cli::cli_progress_done()
  }

  if (4L %in% config$steps) {
    extract_contours(
      dirs$masks,
      dirs$base,
      step = "4/7",
      verbose = config$verbose,
      vertex_size_limits = vertex_size_limits
    )
  }

  if (5L %in% config$steps) {
    smooth_contours(
      dirs$base,
      config$smoothness,
      step = "5/7",
      verbose = config$verbose
    )
  }

  if (6L %in% config$steps) {
    reduce_vertex(
      dirs$base,
      config$tolerance,
      step = "6/7",
      verbose = config$verbose
    )
  }
}


#' @noRd
tract_assemble_3d <- function(step1) {
  ggseg_atlas(
    atlas = step1$atlas_name,
    type = "tract",
    palette = step1$palette,
    core = step1$core,
    data = ggseg_data_tract(
      centerlines = step1$centerlines_df,
      tube_radius = if (is.numeric(step1$tube_radius)) {
        step1$tube_radius
      } else {
        5
      },
      tube_segments = step1$tube_segments
    )
  )
}


#' @noRd
tract_assemble_full <- function(step1, dirs, views, cortex_slices) {
  contours_file <- file.path(dirs$base, "contours_reduced.rda")
  if (!file.exists(contours_file)) {
    cli::cli_abort(c(
      "Step 7 requires contours_reduced.rda which doesn't exist",
      "i" = "Run steps 3-6 first to generate contour data"
    ))
  }

  sf_data <- build_contour_sf(
    contours_file,
    views,
    cortex_slices
  )

  atlas <- ggseg_atlas(
    atlas = step1$atlas_name,
    type = "tract",
    palette = step1$palette,
    core = step1$core,
    data = ggseg_data_tract(
      sf = sf_data,
      centerlines = step1$centerlines_df,
      tube_radius = if (is.numeric(step1$tube_radius)) {
        step1$tube_radius
      } else {
        5
      },
      tube_segments = step1$tube_segments
    )
  )

  warn_if_large_atlas(atlas)
  preview_atlas(atlas)
  atlas
}


#' @noRd
tract_finalize <- function(atlas, config, dirs, start_time) {
  if (config$cleanup) {
    unlink(dirs$base, recursive = TRUE)
    if (config$verbose) cli::cli_alert_success("Temporary files removed")
  }

  if (config$verbose) {
    if (!is.null(atlas)) {
      # fmt: skip
      type <- if (max(config$steps) == 1L) { # nolint
        "3D"
      } else {
        "Tract"
      }
      cli::cli_alert_success(
        "{type} atlas created with {nrow(atlas$core)} tracts"
      )
    } else {
      cli::cli_alert_success("Completed steps {.val {config$steps}}")
    }
    log_elapsed(start_time) # nolint: object_usage_linter.
  }

  if (is.null(atlas)) invisible(NULL) else atlas
}
