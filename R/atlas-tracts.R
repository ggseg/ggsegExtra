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

  verbose <- is_verbose(verbose)
  cleanup <- get_cleanup(cleanup)
  skip_existing <- get_skip_existing(skip_existing)
  tolerance <- get_tolerance(tolerance)
  smoothness <- get_smoothness(smoothness)
  output_dir <- get_output_dir(output_dir)

  max_step <- 7L

  if (is.null(steps)) {
    steps <- 1L:max_step
  }
  steps <- as.integer(steps)

  centerline_method <- match.arg(centerline_method)
  density_radius_range <- c(0.2, 1.0)
  tract_radius <- 3
  coords_are_voxels <- NULL
  cortex_slices <- NULL

  output_dir <- normalizePath(output_dir, mustWork = FALSE)
  if (is.null(atlas_name)) {
    atlas_name <- basename(output_dir)
  }

  dirs <- setup_atlas_dirs(output_dir, atlas_name, type = "tract")
  cache_dir <- dirs$base

  if (verbose) {
    cli::cli_h1("Creating tractography atlas {.val {atlas_name}}")
    cli::cli_alert_info("Tract files: {.path {input_tracts}}")
    cli::cli_alert_info("Anatomical reference: {.path {input_aseg}}")
  }

  tract_names <- NULL
  colours <- NULL

  if (!is.null(input_lut)) {
    lut <- if (is.character(input_lut)) read_ctab(input_lut) else input_lut
    tract_names <- lut$region
    colours <- if ("hex" %in% names(lut)) {
      lut$hex
    } else if (all(c("R", "G", "B") %in% names(lut))) {
      grDevices::rgb(lut$R, lut$G, lut$B, maxColorValue = 255)
    } else {
      NULL
    }
  }

  streamlines_data <- NULL
  core <- NULL
  palette <- NULL
  centerlines_df <- NULL

  # Step 1: Read tracts & create tube meshes ----
  step1_files <- file.path(cache_dir, "step1_data.rds")
  step1 <- load_or_run_step(
    1L,
    steps,
    step1_files,
    skip_existing,
    "Step 1 (Read tracts & create tube meshes)"
  )

  if (step1$run) {
    input_result <- tract_read_input(input_tracts, tract_names)
    streamlines_data <- input_result$streamlines_data
    tract_names <- input_result$tract_names

    if (is.null(coords_are_voxels)) {
      all_streamlines <- unlist(streamlines_data, recursive = FALSE)
      coords_are_voxels <- detect_coords_are_voxels(all_streamlines)
      if (verbose) {
        space <- # nolint: object_usage_linter
          if (coords_are_voxels) "voxel" else "RAS"
        cli::cli_alert_info("Auto-detected coordinate space: {.val {space}}")
      }
    }

    if (is.null(colours)) {
      colours <- rep(NA_character_, length(streamlines_data))
    }

    if (verbose) {
      cli::cli_progress_step(
        "1/7 Creating tube meshes for {length(streamlines_data)} tracts"
      )
    }

    meshes_list <- tract_create_meshes(
      streamlines_data,
      tract_names,
      centerline_method,
      n_points,
      tube_radius,
      tube_segments,
      density_radius_range
    )

    if (verbose) {
      cli::cli_progress_done()
    }

    built <- tract_build_core(meshes_list, colours, tract_names)
    core <- built$core
    palette <- built$palette
    centerlines_df <- built$centerlines_df
    atlas_name <- built$atlas_name

    saveRDS(
      list(
        streamlines_data = streamlines_data,
        centerlines_df = centerlines_df,
        core = core,
        palette = palette,
        atlas_name = atlas_name,
        tube_radius = tube_radius,
        tube_segments = tube_segments
      ),
      file.path(cache_dir, "step1_data.rds")
    )

    if (max(steps) == 1L) {
      atlas <- ggseg_atlas(
        atlas = atlas_name,
        type = "tract",
        palette = palette,
        core = core,
        data = ggseg_data_tract(
          centerlines = centerlines_df,
          tube_radius = if (is.numeric(tube_radius)) tube_radius else 5,
          tube_segments = tube_segments
        )
      )

      if (cleanup) {
        unlink(cache_dir, recursive = TRUE)
      }

      if (verbose) {
        cli::cli_alert_success(
          "Tract atlas created with {nrow(core)} tracts (3D only)"
        )
        log_elapsed(start_time) # nolint: object_usage_linter.
      }

      preview_atlas(atlas)
      return(atlas)
    }
  } else {
    cached <- step1$data[["step1_data.rds"]]
    streamlines_data <- cached$streamlines_data
    centerlines_df <- cached$centerlines_df
    core <- cached$core
    palette <- cached$palette
    atlas_name <- cached$atlas_name
    tube_radius <- cached$tube_radius %||% 5
    tube_segments <- cached$tube_segments %||% 8
    if (verbose) cli::cli_alert_success("1/7 Loaded existing tract data")
  }

  if (any(2L:7L %in% steps) && is.null(input_aseg)) {
    cli::cli_abort(c(
      "{.arg input_aseg} is required for steps 2-7",
      "i" = "Provide a segmentation volume (e.g., aparc+aseg.nii.gz)"
    ))
  }

  step2_files <- c(
    file.path(cache_dir, "views.rds"),
    file.path(cache_dir, "cortex_slices.rds")
  )
  step2 <- load_or_run_step(
    2L,
    steps,
    step2_files,
    skip_existing,
    "Step 2 (Create projection snapshots)"
  )

  if (step2$run) {
    if (verbose) {
      cli::cli_progress_step("2/7 Creating projection snapshots")
    }

    if (is.null(coords_are_voxels)) {
      all_streamlines <- unlist(streamlines_data, recursive = FALSE)
      coords_are_voxels <- detect_coords_are_voxels(all_streamlines)
      if (verbose) {
        space <- if (coords_are_voxels) "voxel" else "RAS"
        cli::cli_alert_info("Auto-detected coordinate space: {.val {space}}")
      }
    }

    snapshot_result <- tract_create_snapshots(
      streamlines_data,
      centerlines_df,
      input_aseg,
      views,
      dirs,
      coords_are_voxels,
      skip_existing,
      tract_radius,
      verbose
    )
    views <- snapshot_result$views
    cortex_slices <- snapshot_result$cortex_slices

    saveRDS(views, file.path(cache_dir, "views.rds"))
    saveRDS(cortex_slices, file.path(cache_dir, "cortex_slices.rds"))

    if (verbose) cli::cli_progress_done()
  } else if (any(steps > 2L)) {
    views <- step2$data[["views.rds"]]
    cortex_slices <- step2$data[["cortex_slices.rds"]]
    if (verbose) cli::cli_alert_success("2/7 Loaded existing snapshots")
  }

  if (3L %in% steps) {
    if (verbose) {
      cli::cli_progress_step("3/7 Processing images")
    }
    process_and_mask_images(
      # nolint: object_usage_linter.
      dirs$snaps,
      dirs$processed,
      dirs$masks,
      dilate = dilate,
      skip_existing = skip_existing
    )
    if (verbose) cli::cli_progress_done()
  }

  if (4L %in% steps) {
    extract_contours(
      dirs$masks,
      dirs$base,
      step = "4/7",
      verbose = verbose,
      vertex_size_limits = vertex_size_limits
    )
  }

  if (5L %in% steps) {
    smooth_contours(dirs$base, smoothness, step = "5/7", verbose = verbose)
  }

  if (6L %in% steps) {
    reduce_vertex(dirs$base, tolerance, step = "6/7", verbose = verbose)
  }

  if (7L %in% steps) {
    contours_file <- file.path(dirs$base, "contours_reduced.rda")
    if (!file.exists(contours_file)) {
      cli::cli_abort(c(
        "Step 7 requires contours_reduced.rda which doesn't exist",
        "i" = "Run steps 3-6 first to generate contour data"
      ))
    }

    if (verbose) {
      cli::cli_progress_step("7/7 Building atlas")
    }

    sf_data <- build_contour_sf(
      # nolint: object_usage_linter.
      contours_file,
      views,
      cortex_slices
    )

    atlas <- ggseg_atlas(
      atlas = atlas_name,
      type = "tract",
      palette = palette,
      core = core,
      data = ggseg_data_tract(
        sf = sf_data,
        centerlines = centerlines_df,
        tube_radius = if (is.numeric(tube_radius)) tube_radius else 5,
        tube_segments = tube_segments
      )
    )
    if (verbose) {
      cli::cli_progress_done()
    }

    if (cleanup) {
      unlink(cache_dir, recursive = TRUE)
    }

    if (verbose) {
      cli::cli_alert_success(
        "Tract atlas created with {nrow(core)} tracts (3D + 2D)"
      )
      log_elapsed(start_time) # nolint: object_usage_linter.
    }

    warn_if_large_atlas(atlas)
    preview_atlas(atlas)
    return(atlas)
  }

  if (verbose) {
    cli::cli_alert_success("Completed steps {.val {steps}}")
    log_elapsed(start_time) # nolint: object_usage_linter.
  }

  invisible(NULL)
}


# Tract step functions ----

#' @noRd
tract_read_input <- function(input_tracts, tract_names) {
  if (is.list(input_tracts) && !is.character(input_tracts)) {
    streamlines_data <- input_tracts
    if (is.null(tract_names)) {
      tract_names <- names(input_tracts)
      if (is.null(tract_names)) {
        tract_names <- paste0("tract_", seq_along(input_tracts))
      }
    }
  } else {
    if (!all(file.exists(input_tracts))) {
      missing <- # nolint: object_usage_linter
        input_tracts[!file.exists(input_tracts)]
      cli::cli_abort("Tract files not found: {.path {missing}}")
    }

    if (is.null(tract_names)) {
      tract_names <- file_path_sans_ext(basename(input_tracts))
    }

    streamlines_data <- furrr::future_map(input_tracts, read_tractography)
    names(streamlines_data) <- tract_names
  }

  if (length(streamlines_data) == 0) {
    cli::cli_abort("No tract data provided")
  }

  list(streamlines_data = streamlines_data, tract_names = tract_names)
}


#' @noRd
tract_create_meshes <- function(
  streamlines_data,
  tract_names,
  centerline_method,
  n_points,
  tube_radius,
  tube_segments,
  density_radius_range
) {
  p <- progressor(steps = length(streamlines_data))

  meshes_list <- future_map2(
    streamlines_data,
    tract_names,
    function(streamlines, tract_name) {
      centerline <- extract_centerline(
        streamlines,
        method = centerline_method,
        n_points = n_points
      )

      if (is.null(centerline) || nrow(centerline) < 2) {
        p()
        return(NULL)
      }

      radius <- resolve_tube_radius(
        tube_radius,
        streamlines,
        centerline,
        density_radius_range
      )

      mesh <- generate_tube_mesh(
        centerline = centerline,
        radius = radius,
        segments = tube_segments
      )

      p()
      mesh
    },
    .options = furrr_options(
      packages = "ggsegExtra",
      globals = c(
        "centerline_method",
        "n_points",
        "tube_radius",
        "density_radius_range",
        "tube_segments",
        "p"
      )
    )
  )
  names(meshes_list) <- tract_names
  meshes_list <- Filter(Negate(is.null), meshes_list)

  if (length(meshes_list) == 0) {
    cli::cli_abort("No meshes were successfully created")
  }

  center_meshes(meshes_list)
}


#' @noRd
tract_build_core <- function(meshes_list, colours, tract_names) {
  core_rows <- lapply(seq_along(meshes_list), function(i) {
    tract_name <- names(meshes_list)[i]
    data.frame(
      hemi = detect_hemi(tract_name, default = "midline"),
      region = clean_region_name(tract_name),
      label = tract_name,
      stringsAsFactors = FALSE
    )
  })

  core <- do.call(rbind, core_rows)

  raw_colours <- colours[seq_along(meshes_list)]
  palette <- if (all(is.na(raw_colours))) {
    NULL
  } else {
    stats::setNames(raw_colours, names(meshes_list))
  }

  centerlines_df <- data.frame(
    label = names(meshes_list),
    stringsAsFactors = FALSE
  )
  centerlines_df$points <- lapply(meshes_list, function(m) {
    m$metadata$centerline
  })
  centerlines_df$tangents <- lapply(meshes_list, function(m) {
    m$metadata$tangents
  })

  atlas_name <- if (length(tract_names) == 1) tract_names[1] else "tracts"

  list(
    core = core,
    palette = palette,
    centerlines_df = centerlines_df,
    atlas_name = atlas_name
  )
}


#' @noRd
tract_create_snapshots <- function(
  streamlines_data,
  centerlines_df,
  input_aseg,
  views,
  dirs,
  coords_are_voxels,
  skip_existing,
  tract_radius,
  verbose
) {
  aseg_vol <- read_volume(input_aseg)
  dims <- dim(aseg_vol)

  if (is.null(views)) {
    views <- default_tract_views(dims)
  }
  cortex_slices <- create_cortex_slices(views, dims)

  tract_labels <- centerlines_df$label

  p <- progressor(steps = length(tract_labels))

  tract_volumes <- future_pmap(
    list(label = tract_labels, i = seq_along(tract_labels)),
    function(label, i) {
      centerline <- streamlines_data[[label]]

      if (is.list(centerline) && !is.matrix(centerline)) {
        centerline <- extract_centerline(centerline, n_points = 50)
      }

      vol <- streamlines_to_volume(
        centerline = centerline,
        template_file = input_aseg,
        label_value = i,
        radius = tract_radius,
        coords_are_voxels = coords_are_voxels
      )

      p()
      vol
    },
    .options = furrr_options(
      packages = "ggsegExtra",
      globals = c(
        "streamlines_data",
        "input_aseg",
        "tract_radius",
        "coords_are_voxels",
        "p"
      )
    )
  )
  names(tract_volumes) <- tract_labels

  cortex_labels <- detect_cortex_labels(aseg_vol)

  cortex_vol <- array(0L, dim = dims)
  invisible(lapply(
    c(cortex_labels$left, cortex_labels$right),
    function(lbl) {
      cortex_vol[aseg_vol == lbl] <<- 1L
    }
  ))

  snapshot_grid <- expand.grid(
    view_idx = seq_len(nrow(views)),
    label = tract_labels,
    stringsAsFactors = FALSE
  )

  p <- progressor(steps = nrow(snapshot_grid))

  invisible(future_pmap(
    list(
      view_type = views$type[snapshot_grid$view_idx],
      view_start = views$start[snapshot_grid$view_idx],
      view_end = views$end[snapshot_grid$view_idx],
      view_name = views$name[snapshot_grid$view_idx],
      label = snapshot_grid$label
    ),
    function(view_type, view_start, view_end, view_name, label) {
      tract_vol <- tract_volumes[[label]]
      hemi <- extract_hemi_from_view(view_type, view_name)
      snapshot_partial_projection(
        vol = tract_vol,
        view = view_type,
        start = view_start,
        end = view_end,
        view_name = view_name,
        label = label,
        output_dir = dirs$snaps,
        colour = "red",
        hemi = hemi,
        skip_existing = skip_existing
      )
      p()
      NULL
    },
    .options = furrr_options(
      packages = "ggsegExtra",
      globals = c("tract_volumes", "dirs", "skip_existing", "p")
    )
  ))

  if (verbose) {
    cli::cli_alert_info("Creating cortex reference slices")
  }

  p2 <- progressor(steps = nrow(cortex_slices))

  invisible(future_pmap(
    list(
      x = cortex_slices$x,
      y = cortex_slices$y,
      z = cortex_slices$z,
      slice_view = cortex_slices$view,
      view_name = cortex_slices$name
    ),
    function(x, y, z, slice_view, view_name) {
      hemi <- extract_hemi_from_view(slice_view, view_name)
      snapshot_cortex_slice(
        vol = cortex_vol,
        x = x,
        y = y,
        z = z,
        slice_view = slice_view,
        view_name = view_name,
        hemi = hemi,
        output_dir = dirs$snaps,
        skip_existing = skip_existing
      )
      p2()
      NULL
    },
    .options = furrr_options(
      packages = "ggsegExtra",
      globals = c("cortex_vol", "dirs", "skip_existing", "p2")
    )
  ))

  list(views = views, cortex_slices = cortex_slices)
}


#' Default tract atlas view configuration
#'
#' Creates projection views optimized for white matter tract visualization.
#' Tracts typically span large portions of the brain, so projections cover
#' wider ranges than subcortical views.
#'
#' @param dims Volume dimensions (3-element vector)
#'
#' @return data.frame with columns: name, type, start, end
#' @keywords internal
default_tract_views <- function(dims) {
  scale <- dims[1] / 256
  chunk_size <- round(30 * scale)
  half_chunk <- chunk_size %/% 2

  z_lo <- round(60 * scale)
  z_hi <- round(180 * scale)
  y_lo <- round(50 * scale)
  y_hi <- round(200 * scale)

  axial_views <- make_view_chunks(z_lo, z_hi, chunk_size, "axial")
  coronal_views <- make_view_chunks(y_lo, y_hi, chunk_size, "coronal")

  mid_x <- dims[1] %/% 2
  gap <- round(10 * scale)
  lateral_peak <- round(40 * scale)

  sagittal_views <- data.frame(
    name = c("sagittal_midline", "sagittal_left", "sagittal_right"),
    type = "sagittal",
    start = c(
      mid_x - half_chunk,
      mid_x + gap + lateral_peak - half_chunk,
      mid_x - gap - lateral_peak - half_chunk
    ),
    end = c(
      mid_x + half_chunk,
      mid_x + gap + lateral_peak + half_chunk,
      mid_x - gap - lateral_peak + half_chunk
    ),
    stringsAsFactors = FALSE
  )

  rbind(axial_views, coronal_views, sagittal_views)
}


#' Resolve tube radius specification
#' @keywords internal
resolve_tube_radius <- function(
  tube_radius,
  streamlines,
  centerline,
  density_range
) {
  n_points <- nrow(centerline)

  if (is.numeric(tube_radius)) {
    if (length(tube_radius) == 1) {
      return(rep(tube_radius, n_points))
    }
    if (length(tube_radius) == n_points) {
      return(tube_radius)
    }
    cli::cli_abort("Numeric tube_radius must be length 1 or {n_points}")
  }

  if (is.character(tube_radius) && tube_radius == "density") {
    density <- compute_streamline_density(
      streamlines,
      centerline,
      search_radius = 2
    )
    if (max(density) == 0) {
      return(rep(mean(density_range), n_points))
    }
    normalized <- density / max(density)
    return(
      density_range[1] + normalized * (density_range[2] - density_range[1])
    )
  }

  rep(0.5, n_points)
}
