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
#'   with columns `label`, `region`, and colour columns (R, G, B or hex).
#'   Use this to provide tract names and colours. If NULL, names are derived
#'   from filenames or list names, and colours are auto-generated.
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
#' @return A `brain_atlas` object with type `"tract"`, containing region
#'   metadata, tube meshes for 3D rendering, colours, and optionally sf
#'   geometry for 2D projection plots.
#' @export
#' @importFrom dplyr tibble bind_rows distinct
#' @importFrom furrr future_map2 furrr_options
#' @importFrom ggseg.formats brain_atlas tract_data
#' @importFrom grDevices rainbow
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
  verbose = get_verbose(),
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
    streamlines_data <- list()

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
        missing <- input_tracts[!file.exists(input_tracts)]
        cli::cli_abort("Tract files not found: {.path {missing}}")
      }

      if (is.null(tract_names)) {
        tract_names <- file_path_sans_ext(basename(input_tracts))
      }

      streamlines_data <- furrr::future_map(
        input_tracts,
        read_tractography
      )
      names(streamlines_data) <- tract_names
    }

    if (length(streamlines_data) == 0) {
      cli::cli_abort("No tract data provided")
    }

    if (is.null(coords_are_voxels)) {
      all_streamlines <- unlist(streamlines_data, recursive = FALSE)
      coords_are_voxels <- detect_coords_are_voxels(all_streamlines)
      if (verbose) {
        space <- if (coords_are_voxels) "voxel" else "RAS"
        cli::cli_alert_info("Auto-detected coordinate space: {.val {space}}")
      }
    }

    if (is.null(colours)) {
      colours <- rainbow(length(streamlines_data))
    }

    if (verbose) {
      cli::cli_progress_step(
        "1/7 Creating tube meshes for {length(streamlines_data)} tracts"
      )
    }

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

    if (verbose) {
      cli::cli_progress_done()
    }

    if (length(meshes_list) == 0) {
      cli::cli_abort("No meshes were successfully created")
    }

    meshes_list <- center_meshes(meshes_list)

    core_rows <- list()
    for (i in seq_along(meshes_list)) {
      tract_name <- names(meshes_list)[i]

      hemi <- detect_hemi(tract_name, default = "midline")
      region <- clean_region_name(tract_name)

      core_rows[[i]] <- data.frame(
        hemi = hemi,
        region = region,
        label = tract_name,
        stringsAsFactors = FALSE
      )
    }

    core <- do.call(rbind, core_rows)

    palette <- stats::setNames(
      colours[seq_along(meshes_list)],
      names(meshes_list)
    )

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

    atlas_name <- if (length(tract_names) == 1) {
      tract_names[1]
    } else {
      "tracts"
    }

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
      atlas <- brain_atlas(
        atlas = atlas_name,
        type = "tract",
        palette = palette,
        core = core,
        data = tract_data(
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
        elapsed <- round(difftime(Sys.time(), start_time, units = "mins"), 1)
        cli::cli_alert_info("Pipeline completed in {elapsed} minutes")
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

  # Step 2: Create projection snapshots ----
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

    aseg_vol <- read_volume(input_aseg)
    dims <- dim(aseg_vol)

    if (is.null(views)) {
      views <- default_tract_views(dims)
    }
    cortex_slices <- create_cortex_slices(views, dims)

    tract_labels <- centerlines_df$label

    p <- progressor(steps = length(tract_labels))

    tract_volumes <- future_pmap(
      list(
        label = tract_labels,
        i = seq_along(tract_labels)
      ),
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
    for (lbl in c(cortex_labels$left, cortex_labels$right)) {
      cortex_vol[aseg_vol == lbl] <- 1L
    }

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

    saveRDS(views, file.path(cache_dir, "views.rds"))
    saveRDS(cortex_slices, file.path(cache_dir, "cortex_slices.rds"))

    if (verbose) {
      cli::cli_progress_done()
    }
  } else if (any(steps > 2L)) {
    views <- step2$data[["views.rds"]]
    cortex_slices <- step2$data[["cortex_slices.rds"]]
    if (verbose) cli::cli_alert_success("2/7 Loaded existing snapshots")
  }

  # Step 3: Process images ----
  if (3L %in% steps) {
    if (verbose) {
      cli::cli_progress_step("3/7 Processing images")
    }

    files <- list.files(dirs$snaps, full.names = TRUE, pattern = "\\.png$")

    for (f in files) {
      process_snapshot_image(
        input_file = f,
        output_file = file.path(dirs$processed, basename(f)),
        dilate = dilate,
        skip_existing = skip_existing
      )
    }

    for (f in list.files(dirs$processed, full.names = TRUE)) {
      extract_alpha_mask(
        f,
        file.path(dirs$masks, basename(f)),
        skip_existing = skip_existing
      )
    }

    if (verbose) {
      cli::cli_progress_done()
    }
  }

  # Step 4: Extract contours ----
  if (4L %in% steps) {
    extract_contours(
      dirs$masks,
      dirs$base,
      step = "4/7",
      verbose = verbose,
      vertex_size_limits = vertex_size_limits
    )
  }

  # Step 5: Smooth contours ----
  if (5L %in% steps) {
    smooth_contours(dirs$base, smoothness, step = "5/7", verbose = verbose)
  }

  # Step 6: Reduce vertices ----
  if (6L %in% steps) {
    reduce_vertex(dirs$base, tolerance, step = "6/7", verbose = verbose)
  }

  # Step 7: Build atlas ----
  if (7L %in% steps) {
    contours_file <- file.path(dirs$base, "contours_reduced.rda")
    if (!file.exists(contours_file)) {
      cli::cli_abort(c(
        "Step 7 requires contours_reduced.rda which doesn't exist",
        "i" = "Run steps 3-6 first to generate contour data"
      ))
    }

    if (verbose) {
      cli::cli_alert_info("7/7 Building atlas")
    }

    conts <- make_multipolygon(contours_file)

    filenm_base <- sub("\\.png$", "", conts$filenm)

    all_view_names <- if (!is.null(cortex_slices)) {
      c(views$name, cortex_slices$name)
    } else {
      views$name
    }

    conts$view <- vapply(
      filenm_base,
      function(fn) {
        for (vn in all_view_names) {
          if (startsWith(fn, paste0(vn, "_"))) {
            return(vn)
          }
        }
        return(NA_character_)
      },
      character(1)
    )

    conts$geometry <- conts$geometry * matrix(c(1, 0, 0, -1), 2, 2)

    conts <- adjust_coords_sf2(conts)

    conts$label <- vapply(
      seq_along(filenm_base),
      function(i) {
        fn <- filenm_base[i]
        vn <- conts$view[i]
        if (is.na(vn)) {
          return(fn)
        }
        sub(paste0("^", vn, "_"), "", fn)
      },
      character(1)
    )

    sf_data <- dplyr::select(conts, label, view, geometry)
    sf_data <- sf::st_as_sf(sf_data)
    sf_data <- dplyr::arrange(
      sf_data,
      dplyr::desc(grepl("cortex", label, ignore.case = TRUE))
    )

    atlas <- brain_atlas(
      atlas = atlas_name,
      type = "tract",
      palette = palette,
      core = core,
      data = tract_data(
        sf = sf_data,
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
        "Tract atlas created with {nrow(core)} tracts (3D + 2D)"
      )
      elapsed <- round(difftime(Sys.time(), start_time, units = "mins"), 1)
      cli::cli_alert_info("Pipeline completed in {elapsed} minutes")
    }

    warn_if_large_atlas(atlas)
    preview_atlas(atlas)
    return(atlas)
  }

  if (verbose) {
    cli::cli_alert_success("Completed steps {.val {steps}}")
    elapsed <- round(difftime(Sys.time(), start_time, units = "mins"), 1)
    cli::cli_alert_info("Pipeline completed in {elapsed} minutes")
  }

  invisible(NULL)
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


# Tractography file reading ----

#' Read tractography file
#'
#' Load streamlines from a tractography file. Supports TrackVis (`.trk`) and
#' MRtrix (`.tck`) formats. The file format is detected from the extension.
#'
#' @param file Path to a `.trk` or `.tck` file.
#' @return A list of matrices, one per streamline. Each matrix has N rows
#'   (points along the streamline) and 3 columns (x, y, z coordinates).
#' @seealso [read_trk()], [read_tck()] for format-specific readers
#' @export
read_tractography <- function(file) {
  ext <- tolower(file_ext(file))

  if (ext == "trk") {
    return(read_trk(file))
  }

  if (ext == "tck") {
    return(read_tck(file))
  }

  cli::cli_abort(c(
    "Unsupported tractography format: {.file {basename(file)}}",
    "i" = "Supported formats: .trk (TrackVis), .tck (MRtrix)"
  ))
}


#' Read TrackVis TRK file
#'
#' Parse a TrackVis `.trk` file and extract all streamlines.
#'
#' @param file Path to a `.trk` file.
#' @return A list of matrices, one per streamline. Each matrix has columns
#'   x, y, z.
#' @seealso [read_tractography()] for format auto-detection
#' @keywords internal
read_trk <- function(file) {
  con <- file(file, "rb")
  on.exit(close(con))

  header <- readBin(con, "raw", 1000)
  id_string <- rawToChar(header[1:6])

  if (!grepl("TRACK", id_string)) {
    cli::cli_abort("Invalid TRK file: {file}")
  }

  n_scalars <- readBin(header[37:38], "integer", 1, size = 2)
  n_properties <- readBin(header[239:240], "integer", 1, size = 2)
  n_count <- readBin(header[989:992], "integer", 1, size = 4)

  seek(con, 1000)

  streamlines <- list()

  for (i in seq_len(n_count)) {
    n_pts <- readBin(con, "integer", 1, size = 4)
    if (is.na(n_pts) || n_pts <= 0) {
      break
    }

    points <- matrix(
      readBin(con, "double", n_pts * (3 + n_scalars), size = 4),
      ncol = 3 + n_scalars,
      byrow = TRUE
    )

    streamlines[[i]] <- points[, 1:3, drop = FALSE]
    colnames(streamlines[[i]]) <- c("x", "y", "z")

    if (n_properties > 0) {
      readBin(con, "double", n_properties, size = 4)
    }
  }

  streamlines
}


#' Read MRtrix TCK file
#'
#' Parse an MRtrix `.tck` file and extract all streamlines.
#'
#' @param file Path to a `.tck` file.
#' @return A list of matrices, one per streamline. Each matrix has columns
#'   x, y, z.
#' @seealso [read_tractography()] for format auto-detection
#' @keywords internal
read_tck <- function(file) {
  con <- file(file, "rb")
  on.exit(close(con))

  header_lines <- character()
  while (TRUE) {
    line <- readLines(con, 1)
    if (length(line) == 0 || line == "END") {
      break
    }
    header_lines <- c(header_lines, line)
  }

  datatype <- "float32"
  for (line in header_lines) {
    if (grepl("^datatype:", line)) {
      datatype <- trimws(sub("datatype:", "", line))
    }
  }

  byte_size <- switch(
    datatype,
    "Float32LE" = 4,
    "Float32BE" = 4,
    "Float64LE" = 8,
    "Float64BE" = 8,
    4
  )

  endian <- if (grepl("BE$", datatype)) "big" else "little"

  streamlines <- list()
  current_streamline <- matrix(ncol = 3, nrow = 0)
  colnames(current_streamline) <- c("x", "y", "z")

  while (TRUE) {
    coords <- readBin(con, "double", 3, size = byte_size, endian = endian)
    if (length(coords) < 3) {
      break
    }

    if (all(is.infinite(coords))) {
      break
    }

    if (all(is.nan(coords))) {
      if (nrow(current_streamline) > 0) {
        streamlines[[length(streamlines) + 1]] <- current_streamline
        current_streamline <- matrix(ncol = 3, nrow = 0)
        colnames(current_streamline) <- c("x", "y", "z")
      }
      next
    }

    current_streamline <- rbind(current_streamline, coords)
  }

  if (nrow(current_streamline) > 0) {
    streamlines[[length(streamlines) + 1]] <- current_streamline
  }

  streamlines
}


# Centerline extraction ----

#' Extract centerline from streamlines
#'
#' Compute a single representative path from a bundle of streamlines.
#' Useful for creating a tube mesh that summarises a tract.
#'
#' The `"mean"` method resamples all streamlines to the same number of points
#' and averages coordinates. The `"medoid"` method picks the single streamline
#' that's most similar to all others (minimises total distance).
#'
#' @param streamlines A list of Nx3 matrices (one per streamline), or a
#'   single matrix if you have just one streamline.
#' @param method How to compute the centerline: `"mean"` averages point-wise,
#'   `"medoid"` selects the most representative streamline.
#' @param n_points Number of points to resample the centerline to.
#'
#' @return A matrix with `n_points` rows and 3 columns (x, y, z).
#' @keywords internal
extract_centerline <- function(
  streamlines,
  method = c("mean", "medoid"),
  n_points = 50
) {
  method <- match.arg(method)

  if (is.matrix(streamlines)) {
    return(resample_streamline(streamlines, n_points))
  }

  if (!is.list(streamlines) || length(streamlines) == 0) {
    return(NULL)
  }

  if (length(streamlines) == 1) {
    return(resample_streamline(streamlines[[1]], n_points))
  }

  resampled <- lapply(streamlines, resample_streamline, n_points = n_points)

  valid <- vapply(
    resampled,
    function(x) !is.null(x) && nrow(x) == n_points,
    logical(1)
  )
  resampled <- resampled[valid]

  if (length(resampled) == 0) {
    return(NULL)
  }

  if (method == "mean") {
    centerline <- Reduce(`+`, resampled) / length(resampled)
    colnames(centerline) <- c("x", "y", "z")
    return(centerline)
  }

  if (method == "medoid") {
    distances <- vapply(
      resampled,
      function(sl) {
        mean(vapply(
          resampled,
          function(other) sqrt(sum((sl - other)^2)),
          numeric(1)
        ))
      },
      numeric(1)
    )
    return(resampled[[which.min(distances)]])
  }

  NULL
}


#' Resample streamline to fixed number of points
#' @keywords internal
resample_streamline <- function(streamline, n_points) {
  if (!is.matrix(streamline) || nrow(streamline) < 2) {
    return(NULL)
  }

  diffs <- apply(streamline, 2, diff)
  if (is.matrix(diffs)) {
    segment_lengths <- sqrt(rowSums(diffs^2))
  } else {
    segment_lengths <- sqrt(sum(diffs^2))
  }
  cumulative_length <- c(0, cumsum(segment_lengths))
  total_length <- cumulative_length[length(cumulative_length)]

  if (total_length == 0) {
    return(NULL)
  }

  target_positions <- seq(0, total_length, length.out = n_points)
  resampled <- matrix(0, nrow = n_points, ncol = 3)

  for (i in seq_len(n_points)) {
    target_pos <- target_positions[i]
    segment_idx <- findInterval(target_pos, cumulative_length)
    segment_idx <- max(1, min(segment_idx, nrow(streamline) - 1))

    segment_start <- cumulative_length[segment_idx]
    segment_end <- cumulative_length[segment_idx + 1]

    t <- if (segment_end == segment_start) {
      0
    } else {
      (target_pos - segment_start) / (segment_end - segment_start)
    }
    resampled[i, ] <- (1 - t) *
      streamline[segment_idx, ] +
      t * streamline[segment_idx + 1, ]
  }

  colnames(resampled) <- c("x", "y", "z")
  resampled
}


# Tube mesh generation ----

#' Generate tube mesh from centerline
#'
#' Build a 3D tube mesh around a path. The tube follows the centerline with
#' consistent orientation (no twisting) using parallel transport frames.
#' Useful for visualising tracts as smooth tubes rather than raw streamlines.
#'
#' @param centerline A matrix with N rows and columns x, y, z defining the
#'   path the tube follows.
#' @param radius Tube radius. Either a single value for uniform thickness,
#'   or a vector of length N to vary the radius along the path.
#' @param segments Number of segments around the tube circumference. Higher
#'   values make smoother tubes but larger meshes.
#'
#' @return A list with:
#'   \itemize{
#'     \item `vertices`: data.frame with x, y, z columns
#'     \item `faces`: data.frame with i, j, k columns (1-indexed triangle vertices)
#'     \item metadata: list with n_centerline_points, centerline, tangents
#'   }
#' @keywords internal
generate_tube_mesh <- function(centerline, radius = 0.5, segments = 8) {
  if (!is.matrix(centerline) || nrow(centerline) < 2) {
    cli::cli_abort("centerline must be a matrix with at least 2 rows")
  }

  n_points <- nrow(centerline)
  frames <- compute_parallel_transport_frames(centerline)

  if (length(radius) == 1) {
    radius <- rep(radius, n_points)
  } else if (length(radius) != n_points) {
    cli::cli_abort(
      "radius must be length 1 or {n_points}, got {length(radius)}"
    )
  }

  n_vertices <- n_points * segments
  n_faces <- (n_points - 1) * segments * 2

  vertices <- matrix(0, nrow = n_vertices, ncol = 3)
  faces <- matrix(0L, nrow = n_faces, ncol = 3)

  angles <- seq(0, 2 * pi, length.out = segments + 1)[1:segments]

  for (i in seq_len(n_points)) {
    center <- centerline[i, ]
    normal <- frames$normals[i, ]
    binormal <- frames$binormals[i, ]
    r <- radius[i]

    for (j in seq_len(segments)) {
      angle <- angles[j]
      offset <- r * (cos(angle) * normal + sin(angle) * binormal)
      vertex_idx <- (i - 1) * segments + j
      vertices[vertex_idx, ] <- center + offset
    }
  }

  face_idx <- 1
  for (i in seq_len(n_points - 1)) {
    for (j in seq_len(segments)) {
      j_next <- if (j == segments) 1L else j + 1L

      v1 <- (i - 1L) * segments + j
      v2 <- (i - 1L) * segments + j_next
      v3 <- i * segments + j
      v4 <- i * segments + j_next

      faces[face_idx, ] <- c(v1, v2, v3)
      faces[face_idx + 1L, ] <- c(v2, v4, v3)
      face_idx <- face_idx + 2L
    }
  }

  list(
    vertices = data.frame(
      x = vertices[, 1],
      y = vertices[, 2],
      z = vertices[, 3]
    ),
    faces = data.frame(i = faces[, 1], j = faces[, 2], k = faces[, 3]),
    metadata = list(
      n_centerline_points = n_points,
      centerline = centerline,
      tangents = frames$tangents
    )
  )
}


#' Compute parallel transport frames along curve
#'
#' Uses the parallel transport method to compute stable perpendicular frames
#' along a 3D curve, avoiding the twisting artifacts of Frenet-Serret frames.
#'
#' @param curve Matrix with N rows and 3 columns
#' @return List with tangents, normals, and binormals matrices
#' @keywords internal
compute_parallel_transport_frames <- function(curve) {
  n <- nrow(curve)

  tangents <- matrix(0, nrow = n, ncol = 3)
  for (i in seq_len(n - 1)) {
    tangents[i, ] <- curve[i + 1, ] - curve[i, ]
    len <- sqrt(sum(tangents[i, ]^2))
    if (len > 0) tangents[i, ] <- tangents[i, ] / len
  }
  tangents[n, ] <- tangents[n - 1, ]

  t0 <- tangents[1, ]
  arbitrary <- if (abs(t0[1]) < 0.9) c(1, 0, 0) else c(0, 1, 0)
  n0 <- cross_product(t0, arbitrary)
  n0 <- n0 / sqrt(sum(n0^2))

  normals <- matrix(0, nrow = n, ncol = 3)
  binormals <- matrix(0, nrow = n, ncol = 3)

  normals[1, ] <- n0
  binormals[1, ] <- cross_product(t0, n0)

  for (i in seq_len(n - 1)) {
    t_curr <- tangents[i, ]
    t_next <- tangents[i + 1, ]

    cross_t <- cross_product(t_curr, t_next)
    cross_norm <- sqrt(sum(cross_t^2))

    if (cross_norm < 1e-10) {
      normals[i + 1, ] <- normals[i, ]
      binormals[i + 1, ] <- binormals[i, ]
    } else {
      axis <- cross_t / cross_norm
      angle <- acos(max(-1, min(1, sum(t_curr * t_next))))

      normals[i + 1, ] <- rotate_vector(normals[i, ], axis, angle)
      normals[i + 1, ] <- normals[i + 1, ] / sqrt(sum(normals[i + 1, ]^2))
      binormals[i + 1, ] <- cross_product(t_next, normals[i + 1, ])
    }
  }

  list(tangents = tangents, normals = normals, binormals = binormals)
}


#' Cross product of two 3D vectors
#' @keywords internal
cross_product <- function(a, b) {
  c(
    a[2] * b[3] - a[3] * b[2],
    a[3] * b[1] - a[1] * b[3],
    a[1] * b[2] - a[2] * b[1]
  )
}


#' Rotate vector around axis by angle (Rodrigues' formula)
#' @keywords internal
rotate_vector <- function(v, axis, angle) {
  cos_a <- cos(angle)
  sin_a <- sin(angle)
  v *
    cos_a +
    cross_product(axis, v) * sin_a +
    axis * sum(axis * v) * (1 - cos_a)
}


#' Compute streamline density for tract bundles
#'
#' Calculates how many streamlines pass through each point along the centerline.
#'
#' @param streamlines List of streamline matrices
#' @param centerline Centerline matrix (Nx3)
#' @param search_radius Radius around centerline points to count streamlines
#'
#' @return Numeric vector of density values (one per centerline point)
#' @keywords internal
compute_streamline_density <- function(
  streamlines,
  centerline,
  search_radius = 2
) {
  n_points <- nrow(centerline)
  density <- numeric(n_points)

  for (i in seq_len(n_points)) {
    center <- centerline[i, ]
    count <- 0
    for (sl in streamlines) {
      if (!is.matrix(sl) || nrow(sl) == 0) {
        next
      }
      dists <- sqrt(rowSums(sweep(sl, 2, center)^2))
      if (any(dists <= search_radius)) count <- count + 1
    }
    density[i] <- count
  }

  density
}


# 2D geometry creation for tracts (volumetric approach) ----

#' Convert streamlines/centerline to volumetric representation
#'
#' Creates a 3D volume where voxels containing tract coordinates are labeled.
#' Uses a template volume to define the output space dimensions and voxel size.
#'
#' The vox2ras matrix maps to the file's **native** voxel layout, so the
#' tract volume is built in native orientation, then reoriented to RAS+ at
#' the end for consistent downstream use.
#'
#' @param centerline Matrix with x, y, z columns
#' @param template_file Path to template volume (.mgz, .nii) that defines the output space
#' @param label_value Integer label value to assign to tract voxels (default 1)
#' @param radius Dilation radius in voxels to thicken the tract (default 2)
#' @param coords_are_voxels Logical. If TRUE, coordinates are already in voxel space
#'   (1-indexed). If FALSE (default), coordinates are in RAS space and will be
#'   transformed using the volume's vox2ras matrix.
#'
#' @return 3D array in RAS+ orientation, tract voxels set to label_value
#' @keywords internal
streamlines_to_volume <- function(
  centerline,
  template_file,
  label_value = 1L,
  radius = 2,
  coords_are_voxels = FALSE
) {
  if (!file.exists(template_file)) {
    cli::cli_abort("Template file not found: {.path {template_file}}")
  }

  template_nii <- read_volume(template_file, reorient = FALSE)
  dims <- dim(template_nii)
  vox2ras <- load_vox2ras_matrix(template_file, coords_are_voxels)
  vol <- array(0L, dim = dims)

  for (i in seq_len(nrow(centerline))) {
    vox_idx <- coord_to_voxel(
      centerline[i, ],
      dims,
      vox2ras,
      coords_are_voxels
    )
    vol <- set_sphere_voxels(vol, vox_idx, radius, label_value, dims)
  }

  nii <- RNifti::asNifti(vol, reference = template_nii)
  if (RNifti::orientation(nii) != "RAS") {
    RNifti::orientation(nii) <- "RAS"
  }
  as.array(nii)
}


#' Load vox2ras transformation matrix from volume file
#' @noRd
load_vox2ras_matrix <- function(template_file, coords_are_voxels) {
  if (coords_are_voxels) {
    return(NULL)
  }

  ext <- tolower(tools::file_ext(template_file))
  if (ext == "gz") {
    ext <- tools::file_ext(sub("\\.gz$", "", template_file))
  }

  if (ext == "mgz") {
    if (!requireNamespace("freesurferformats", quietly = TRUE)) {
      return(NULL)
    }
    tryCatch(
      freesurferformats::read.fs.mgh(template_file, with_header = TRUE)$vox2ras,
      error = function(e) NULL
    )
  } else if (ext == "nii") {
    if (!requireNamespace("RNifti", quietly = TRUE)) {
      return(NULL)
    }
    tryCatch(
      {
        hdr <- RNifti::niftiHeader(template_file)
        RNifti::xform(hdr)
      },
      error = function(e) NULL
    )
  } else {
    NULL
  }
}


#' Convert world coordinate to voxel index
#'
#' Maps 3D coordinates to 1-based voxel array indices.
#' When coords_are_voxels is TRUE, assumes 0-based voxel indices
#' (TrackVis convention) and adds 1 for R indexing.
#' @noRd
coord_to_voxel <- function(coord, dims, vox2ras, coords_are_voxels) {
  if (coords_are_voxels) {
    return(round(coord) + 1L)
  }
  if (!is.null(vox2ras)) {
    ras2vox <- solve(vox2ras)
    vox_coord <- ras2vox %*% c(coord, 1)
    return(round(vox_coord[1:3]) + 1)
  }
  c(
    round(dims[1] / 2 - coord[1]) + 1,
    round(dims[2] / 2 + coord[2]) + 1,
    round(dims[3] / 2 + coord[3]) + 1
  )
}


#' Set voxels within a sphere around center point
#' @noRd
set_sphere_voxels <- function(vol, center, radius, label_value, dims) {
  for (dx in seq(-radius, radius)) {
    for (dy in seq(-radius, radius)) {
      for (dz in seq(-radius, radius)) {
        if (dx^2 + dy^2 + dz^2 > radius^2) {
          next
        }

        vx <- center[1] + dx
        vy <- center[2] + dy
        vz <- center[3] + dz

        if (voxel_in_bounds(vx, vy, vz, dims)) {
          vol[vx, vy, vz] <- label_value
        }
      }
    }
  }
  vol
}


#' Check if voxel coordinates are within volume bounds
#' @noRd
voxel_in_bounds <- function(x, y, z, dims) {
  x >= 1 && x <= dims[1] && y >= 1 && y <= dims[2] && z >= 1 && z <= dims[3]
}


#' Detect if streamline coordinates are in voxel space
#'
#' Uses heuristics to guess whether coordinates are in voxel space (0 to dims)
#' or RAS/world space (centered around 0).
#'
#' @param streamlines List of streamline matrices (each with x, y, z columns)
#' @param dims Optional volume dimensions for validation
#' @return TRUE if likely voxel space, FALSE if likely RAS
#' @noRd
detect_coords_are_voxels <- function(streamlines, dims = NULL) {
  all_coords <- do.call(
    rbind,
    lapply(streamlines, function(s) {
      if (is.matrix(s) && nrow(s) > 0) {
        s[, 1:3, drop = FALSE]
      } else {
        NULL
      }
    })
  )

  if (is.null(all_coords) || nrow(all_coords) == 0) {
    return(FALSE)
  }

  min_coord <- min(all_coords, na.rm = TRUE)
  max_coord <- max(all_coords, na.rm = TRUE)

  if (min_coord < -10) {
    return(FALSE)
  }

  if (min_coord >= 0 && max_coord <= 300) {
    if (!is.null(dims) && max(dims) > 0) {
      max_dim <- max(dims)
      if (max_coord <= max_dim * 1.1) {
        return(TRUE)
      }
    } else {
      return(TRUE)
    }
  }

  FALSE
}


#' Create 2D geometry for tract atlas
#'
#' Generate polygon outlines for tract visualisation in 2D. The function
#' projects tract centerlines onto slice views (coronal, axial) and extracts
#' contours. Also creates cortex outlines for anatomical context.
#'
#' This is typically called automatically by [create_tract_atlas()] when
#' `include_geometry = TRUE`, but you can call it separately if you want
#' custom views or need to regenerate geometry.
#'
#' @param atlas A `brain_atlas` of type `"tract"` (from [create_tract_atlas()]).
#' @param aseg_file Path to a segmentation volume (`.mgz`, `.nii`) used to
#'   draw cortex outlines for anatomical context.
#' @param streamlines Named list of streamline matrices (Nx3 with x, y, z).
#'   Names must match tract labels in the atlas. Required because atlas
#'   centerlines are centred for 3D rendering and don't match volumetric space.
#' @param views A data.frame defining which projection views to create. Columns:
#'   `name` (view label), `type` (`"coronal"` or `"axial"`), `start` (first
#'   slice), `end` (last slice). Default creates upper/lower coronal and
#'   anterior/posterior axial views.
#' @param cortex_slices A data.frame specifying cortex slice positions for
#'   reference outlines. Columns: `x`, `y`, `z`, `view`, `name`. Default uses
#'   central slices for each view.
#' @template output_dir
#' @param tract_radius Dilation radius when rasterising tract coordinates.
#' @param coords_are_voxels If TRUE, streamline coordinates are in voxel
#'   space (0-indexed). If FALSE, coordinates are in RAS space. If NULL
#'   (default), auto-detects by checking coordinate ranges.
#' @template vertex_size_limits
#' @template dilate
#' @template tolerance
#' @template smoothness
#' @template verbose
#' @template cleanup
#' @template skip_existing
#'
#' @return An sf data.frame with columns `label`, `side` (view name), and
#'   `geometry`.
#' @keywords internal
#' @importFrom dplyr bind_rows left_join select
#' @importFrom furrr future_pmap furrr_options
#' @importFrom progressr progressor
#' @importFrom tidyr separate
create_tract_geometry_volumetric <- function(
  atlas,
  aseg_file,
  streamlines,
  views = NULL,
  cortex_slices = NULL,
  output_dir = NULL,
  tract_radius = 3,
  coords_are_voxels = NULL,
  vertex_size_limits = NULL,
  dilate = NULL,
  tolerance = NULL,
  smoothness = NULL,
  verbose = get_verbose(),
  cleanup = NULL,
  skip_existing = NULL
) {
  verbose <- is_verbose(verbose)
  cleanup <- get_cleanup(cleanup)
  skip_existing <- get_skip_existing(skip_existing)
  tolerance <- get_tolerance(tolerance)
  smoothness <- get_smoothness(smoothness)
  output_dir <- get_output_dir(output_dir)

  if (atlas$type != "tract") {
    cli::cli_abort("Atlas must be of type 'tract'")
  }

  if (!file.exists(aseg_file)) {
    cli::cli_abort("Aseg file not found: {.path {aseg_file}}")
  }

  if (missing(streamlines) || is.null(streamlines)) {
    cli::cli_abort(c(
      "{.arg streamlines} is required for volumetric geometry",
      "i" = "Atlas centerlines are centered for 3D rendering and don't match volumetric space",
      "i" = "Pass the original streamlines/centerlines in RAS coordinates"
    ))
  }

  if (is.null(coords_are_voxels)) {
    all_streamlines <- unlist(streamlines, recursive = FALSE)
    coords_are_voxels <- detect_coords_are_voxels(all_streamlines)
    if (verbose) {
      space <- if (coords_are_voxels) "voxel" else "RAS"
      cli::cli_alert_info("Auto-detected coordinate space: {.val {space}}")
    }
  }

  output_dir <- path.expand(output_dir)
  output_dir <- normalizePath(output_dir, mustWork = FALSE)
  if (verbose) {
    cli::cli_alert_info("Setting output directory to {.path {output_dir}}")
  }

  dirs <- setup_atlas_dirs(output_dir, type = "tract")
  dirs$vols <- dirs$volumes

  centerlines <- atlas$data$centerlines
  if (is.null(centerlines)) {
    cli::cli_abort("Atlas must have centerlines data for volumetric geometry")
  }
  tract_labels <- centerlines$label

  missing_labels <- setdiff(tract_labels, names(streamlines))
  if (length(missing_labels) > 0) {
    cli::cli_abort(c(
      "Streamlines missing for tracts: {.val {missing_labels}}",
      "i" = "streamlines must be a named list with names matching tract labels"
    ))
  }

  contours_file <- file.path(dirs$base, "contours_reduced.rda")
  views_file <- file.path(dirs$base, "views.rds")
  if (skip_existing && file.exists(contours_file)) {
    if (file.exists(views_file)) {
      views <- readRDS(views_file)
    } else {
      dims <- dim(read_volume(aseg_file))
      views <- default_tract_views(dims)
    }
    if (verbose) {
      cli::cli_alert_success("Loaded existing 2D geometry from cache")
    }
  } else {
    aseg_vol <- read_volume(aseg_file)
    dims <- dim(aseg_vol)

    if (is.null(views)) {
      views <- default_tract_views(dims)
    }
    saveRDS(views, views_file)

    if (is.null(cortex_slices)) {
      cortex_slices <- create_cortex_slices(views, dims)
    }

    n_snapshots <- nrow(views) * length(tract_labels) + nrow(cortex_slices)
    existing_snaps <- length(list.files(dirs$snaps, pattern = "\\.png$"))

    if (skip_existing && existing_snaps >= n_snapshots) {
      if (verbose) {
        cli::cli_alert_success(
          "Using existing snapshots ({existing_snaps} files)"
        )
      }
    } else {
      if (verbose) {
        cli::cli_alert_info("Converting tracts to volumes")
      }

      p <- progressor(steps = length(tract_labels))

      tract_volumes <- future_pmap(
        list(
          label = tract_labels,
          i = seq_along(tract_labels)
        ),
        function(label, i) {
          centerline <- streamlines[[label]]

          if (is.list(centerline) && !is.matrix(centerline)) {
            centerline <- extract_centerline(centerline, n_points = 50)
          }

          vol <- streamlines_to_volume(
            centerline = centerline,
            template_file = aseg_file,
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
            "streamlines",
            "aseg_file",
            "tract_radius",
            "coords_are_voxels",
            "p"
          )
        )
      )
      names(tract_volumes) <- tract_labels

      if (verbose) {
        cli::cli_alert_info("Creating projections")
      }

      cortex_labels <- detect_cortex_labels(aseg_vol)

      cortex_vol <- array(0L, dim = dims)
      for (lbl in c(cortex_labels$left, cortex_labels$right)) {
        cortex_vol[aseg_vol == lbl] <- 1L
      }

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
    }

    if (verbose) {
      cli::cli_alert_info("Processing images")
    }

    files <- list.files(dirs$snaps, full.names = TRUE, pattern = "\\.png$")

    for (f in files) {
      process_snapshot_image(
        input_file = f,
        output_file = file.path(dirs$processed, basename(f)),
        dilate = dilate,
        skip_existing = skip_existing
      )
    }

    for (f in list.files(dirs$processed, full.names = TRUE)) {
      extract_alpha_mask(
        f,
        file.path(dirs$masks, basename(f)),
        skip_existing = skip_existing
      )
    }

    extract_contours(
      dirs$masks,
      dirs$base,
      step = NULL,
      verbose = verbose,
      vertex_size_limits = vertex_size_limits
    )
    smooth_contours(dirs$base, smoothness, step = NULL, verbose = verbose)
    reduce_vertex(dirs$base, tolerance, step = NULL, verbose = verbose)
  }

  if (verbose) {
    cli::cli_alert_info("Building sf geometry")
  }

  conts <- make_multipolygon(file.path(dirs$base, "contours_reduced.rda"))

  filenm_base <- sub("\\.png$", "", conts$filenm)

  all_view_names <- if (!is.null(cortex_slices)) {
    c(views$name, cortex_slices$name)
  } else {
    views$name
  }

  conts$view <- vapply(
    filenm_base,
    function(fn) {
      for (vn in all_view_names) {
        if (startsWith(fn, paste0(vn, "_"))) {
          return(vn)
        }
      }
      return(NA_character_)
    },
    character(1)
  )

  conts$geometry <- conts$geometry * matrix(c(1, 0, 0, -1), 2, 2)

  conts <- adjust_coords_sf2(conts)

  filenm_base <- sub("\\.png$", "", conts$filenm)
  conts$label <- vapply(
    seq_along(filenm_base),
    function(i) {
      fn <- filenm_base[i]
      vn <- conts$view[i]
      if (is.na(vn)) {
        return(fn)
      }
      sub(paste0("^", vn, "_"), "", fn)
    },
    character(1)
  )

  sf_data <- dplyr::select(conts, label, view, geometry)
  sf_data <- sf::st_as_sf(sf_data)
  sf_data <- dplyr::arrange(
    sf_data,
    dplyr::desc(grepl("cortex", label, ignore.case = TRUE))
  )

  if (cleanup) {
    unlink(dirs$base, recursive = TRUE)
  }

  sf_data
}


#' Center meshes around origin
#'
#' Computes the global centroid of all mesh vertices and translates
#' all meshes to center around the origin.
#'
#' @param meshes_list Named list of meshes
#' @return Centered meshes list
#' @keywords internal
center_meshes <- function(meshes_list) {
  all_vertices <- do.call(rbind, lapply(meshes_list, function(m) m$vertices))

  centroid <- c(
    mean(all_vertices$x),
    mean(all_vertices$y),
    mean(all_vertices$z)
  )

  for (name in names(meshes_list)) {
    meshes_list[[name]]$vertices$x <- meshes_list[[name]]$vertices$x -
      centroid[1]
    meshes_list[[name]]$vertices$y <- meshes_list[[name]]$vertices$y -
      centroid[2]
    meshes_list[[name]]$vertices$z <- meshes_list[[name]]$vertices$z -
      centroid[3]

    if (!is.null(meshes_list[[name]]$metadata$centerline)) {
      meshes_list[[name]]$metadata$centerline[, 1] <- meshes_list[[
        name
      ]]$metadata$centerline[, 1] -
        centroid[1]
      meshes_list[[name]]$metadata$centerline[, 2] <- meshes_list[[
        name
      ]]$metadata$centerline[, 2] -
        centroid[2]
      meshes_list[[name]]$metadata$centerline[, 3] <- meshes_list[[
        name
      ]]$metadata$centerline[, 3] -
        centroid[3]
    }
  }

  meshes_list
}
