# Subcortical atlas creation ----

#' Create brain atlas from subcortical segmentation
#'
#' @description
#' `r lifecycle::badge("experimental")`
#'
#' Turn a subcortical segmentation volume (like `aseg.mgz`) into a brain
#' atlas with 3D meshes for each structure. The function extracts each labelled
#' region from the volume, creates a surface mesh, and smooths it.
#'
#' For 2D plotting, the function can also generate slice views by taking
#' snapshots at specified coordinates and extracting contours.
#'
#' Requires FreeSurfer for mesh generation.
#'
#' @param input_volume Path to the segmentation volume. Supports `.mgz`, `.nii`,
#'   and `.nii.gz` formats. Typically this is `aseg.mgz` or a custom
#'   segmentation in the same space.
#' @param input_lut Path to a FreeSurfer-style colour lookup table that maps
#'   label IDs to region names and colours (e.g., `FreeSurferColorLUT.txt`
#'   or `ASegStatsLUT.txt`), or a data.frame with columns `region` and colour
#'   columns (R, G, B or hex). If NULL, region names will be generic
#'   (e.g., "region_0010") and the atlas will have no palette.
#' @template atlas_name
#' @param views A data.frame specifying projection views with columns `name`,
#'   `type` ("axial", "coronal", "sagittal"), `start` (first slice), `end`
#'   (last slice). Default projects entire volume from each direction.
#'   Unlike slices, projections show ALL structures in their spatial
#'   relationships - like an X-ray view.
#' @template output_dir
#' @template vertex_size_limits
#' @template dilate
#' @template tolerance
#' @template smoothness
#' @template verbose
#' @template cleanup
#' @template skip_existing
#' @template decimate
#' @param steps Which pipeline steps to run. Default NULL runs all steps.
#'   Steps are:
#'   \itemize{
#'     \item 1: Extract labels from volume and get colour table
#'     \item 2: Create meshes for each structure
#'     \item 3: Build atlas data (3D only if stopping here)
#'     \item 4: Create projection snapshots
#'     \item 5: Process images
#'     \item 6: Extract contours
#'     \item 7: Smooth contours
#'     \item 8: Reduce vertices
#'     \item 9: Build final atlas with 2D geometry
#'   }
#'   Use `steps = 1:3` for 3D-only atlas. Use `steps = 7:8` to iterate on
#'   smoothing and reduction parameters.
#'
#' @return A `brain_atlas` object with region metadata (core), 3D meshes,
#'   a colour palette, and optionally sf geometry for 2D slice plots.
#' @export
#' @importFrom dplyr tibble bind_rows left_join filter distinct
#' @importFrom freesurfer have_fs fs_dir fs_subj_dir
#' @importFrom furrr future_pmap furrr_options
#' @importFrom progressr progressor
#' @importFrom tools file_path_sans_ext
#'
#' @examples
#' \dontrun{
#' # Create 3D-only subcortical atlas from aseg
#' atlas <- create_subcortical_atlas(
#'   input_volume = "path/to/aseg.mgz",
#'   input_lut = "path/to/FreeSurferColorLUT.txt",
#'   steps = 1:3
#' )
#'
#' # View with ggseg3d
#' ggseg3d(atlas = atlas, hemisphere = "subcort")
#'
#' # Full atlas with 2D slices
#' atlas <- create_subcortical_atlas(
#'   input_volume = "path/to/aseg.mgz",
#'   input_lut = "path/to/ASegStatsLUT.txt"
#' )
#'
#' # Post-process to remove/modify regions
#' atlas <- atlas |>
#'   atlas_remove_regions("White-Matter") |>
#'   atlas_set_context("Cortex")
#' }
create_subcortical_atlas <- function(
  input_volume,
  input_lut = NULL,
  atlas_name = NULL,
  views = NULL,
  output_dir = NULL,
  vertex_size_limits = NULL,
  dilate = NULL,
  tolerance = NULL,
  smoothness = NULL,
  verbose = get_verbose(), # nolint: object_usage_linter
  cleanup = NULL,
  skip_existing = NULL,
  decimate = 0.5,
  steps = NULL
) {
  start_time <- Sys.time()

  verbose <- is_verbose(verbose)
  cleanup <- get_cleanup(cleanup)
  skip_existing <- get_skip_existing(skip_existing)
  tolerance <- get_tolerance(tolerance)
  smoothness <- get_smoothness(smoothness)
  output_dir <- get_output_dir(output_dir)

  if (!is.null(decimate)) {
    if (!is.numeric(decimate) || length(decimate) != 1 ||
        decimate <= 0 || decimate >= 1) {
      cli::cli_abort(c(
        "{.arg decimate} must be a single number between 0 and 1 (exclusive)",
        "x" = "Got {.val {decimate}}",
        "i" = "Use {.code NULL} to skip mesh decimation"
      ))
    }
  }

  max_step <- 9L
  if (is.null(steps)) {
    steps <- 1L:max_step
  }
  steps <- as.integer(steps)

  check_fs(abort = TRUE)

  if (!file.exists(input_volume)) {
    cli::cli_abort("Volume file not found: {.path {input_volume}}")
  }

  if (!is.null(input_lut) && !file.exists(input_lut)) {
    cli::cli_abort("Color lookup table not found: {.path {input_lut}}")
  }

  output_dir <- normalizePath(output_dir, mustWork = FALSE)

  if (is.null(atlas_name)) {
    atlas_name <- file_path_sans_ext(basename(input_volume))
  }

  dirs <- setup_atlas_dirs(output_dir, atlas_name, type = "subcortical")

  if (verbose) {
    cli::cli_h1("Creating subcortical atlas {.val {atlas_name}}")
    cli::cli_alert_info("Volume: {.path {input_volume}}")
    if (!is.null(input_lut)) {
      cli::cli_alert_info("Color LUT: {.path {input_lut}}")
    }
    cli::cli_alert_info("Setting output directory to {.path {output_dir}}")
  }

  colortable <- NULL
  vol_labels <- NULL
  meshes_list <- NULL
  components <- NULL

  step1_files <- c(
    file.path(dirs$base, "colortable.rds"),
    file.path(dirs$base, "vol_labels.rds")
  )
  step1 <- load_or_run_step(
    1L,
    steps,
    step1_files,
    skip_existing,
    "Step 1 (Extract labels)"
  )

  if (step1$run) {
    if (is.null(input_lut)) {
      cli::cli_warn(c(
        "No color lookup table provided",
        "i" = "Region names will be generic (e.g., 'region_0010')",
        "i" = "Atlas will have no palette"
      ))
      colortable <- generate_colortable_from_volume(input_volume)
    } else {
      colortable <- get_ctab(input_lut)
    }

    if (verbose) cli::cli_progress_step("1/9 Extracting labels from volume")

    vol <- read_volume(input_volume)
    vol_labels <- unique(c(vol))
    vol_labels <- vol_labels[vol_labels != 0]

    colortable <- colortable[colortable$idx %in% vol_labels, ]

    if (nrow(colortable) == 0) {
      cli::cli_abort("No matching labels found in volume and color table")
    }

    if (verbose) {
      cli::cli_alert_success("Found {nrow(colortable)} subcortical structures")
    }

    saveRDS(colortable, file.path(dirs$base, "colortable.rds"))
    saveRDS(vol_labels, file.path(dirs$base, "vol_labels.rds"))
    if (verbose) cli::cli_progress_done()
  } else {
    colortable <- step1$data[["colortable.rds"]]
    vol_labels <- step1$data[["vol_labels.rds"]]
    if (verbose) cli::cli_alert_success("1/9 Loaded existing labels")
  }

  step2_files <- file.path(dirs$base, "meshes_list.rds")
  step2 <- load_or_run_step(
    2L,
    steps,
    step2_files,
    skip_existing,
    "Step 2 (Create meshes)"
  )

  if (step2$run) {
    if (verbose) {
      cli::cli_progress_step("2/9 Creating meshes for each structure")
    }
    meshes_list <- subcort_create_meshes(
      input_volume, colortable, dirs, skip_existing, verbose,
      decimate = decimate
    )
    if (verbose) cli::cli_progress_done()
    saveRDS(meshes_list, file.path(dirs$base, "meshes_list.rds"))
  } else if (any(steps > 2L)) {
    meshes_list <- step2$data[["meshes_list.rds"]]
    if (verbose) cli::cli_alert_success("2/9 Loaded existing meshes")
  }

  step3_files <- file.path(dirs$base, "components.rds")
  step3 <- load_or_run_step(
    3L, steps, step3_files, skip_existing, "Step 3 (Build atlas data)"
  )

  if (step3$run) {
    if (verbose) cli::cli_progress_step("3/9 Building atlas data")
    components <- subcort_build_components(colortable, meshes_list)
    saveRDS(components, file.path(dirs$base, "components.rds"))

    if (max(steps) == 3L) {
      atlas <- brain_atlas(
        atlas = atlas_name,
        type = "subcortical",
        palette = components$palette,
        core = components$core,
        data = brain_data_subcortical(sf = NULL, meshes = components$meshes_df)
      )

      cli::cli_progress_done()
      if (cleanup) {
        unlink(dirs$base, recursive = TRUE)
        if (verbose) cli::cli_alert_success("Temporary files removed")
      }

      if (verbose) {
        cli::cli_alert_success(
          "3D atlas created with {nrow(components$core)} structures"
        )
        log_elapsed(start_time) # nolint: object_usage_linter.
      }
      return(atlas)
    }
  } else if (any(steps > 3L)) {
    components <- step3$data[["components.rds"]]
    if (verbose) cli::cli_alert_success("3/9 Loaded existing components")
  }

  cli::cli_progress_done()

  step4_files <- c(
    file.path(dirs$base, "views.rds"),
    file.path(dirs$base, "cortex_slices.rds")
  )
  step4 <- load_or_run_step(
    4L, steps, step4_files, skip_existing, "Step 4 (Create snapshots)"
  )

  cortex_slices <- NULL

  if (step4$run) {
    if (verbose) cli::cli_progress_step("4/9 Creating projection snapshots")
    snapshot_result <- subcort_create_snapshots(
      input_volume, colortable, views, dirs, skip_existing
    )
    views <- snapshot_result$views
    cortex_slices <- snapshot_result$cortex_slices
    saveRDS(views, file.path(dirs$base, "views.rds"))
    saveRDS(cortex_slices, file.path(dirs$base, "cortex_slices.rds"))
    if (verbose) cli::cli_progress_done()
  } else if (any(steps > 4L)) {
    views <- step4$data[["views.rds"]]
    cortex_slices <- step4$data[["cortex_slices.rds"]]
    if (verbose) cli::cli_alert_success("4/9 Loaded existing views")
  }

  cli::cli_progress_done()

  if (5L %in% steps) {
    if (verbose) cli::cli_progress_step("5/9 Processing images")
    process_and_mask_images( # nolint: object_usage_linter.
      dirs$snaps, dirs$processed, dirs$masks,
      dilate = dilate, skip_existing = skip_existing
    )
    if (verbose) cli::cli_progress_done()
  }

  if (6L %in% steps) {
    extract_contours(
      dirs$masks, dirs$base, step = "6/9",
      verbose = verbose, vertex_size_limits = vertex_size_limits
    )
  }

  if (7L %in% steps) {
    smooth_contours(dirs$base, smoothness, step = "7/9", verbose = verbose)
  }

  if (8L %in% steps) {
    reduce_vertex(dirs$base, tolerance, step = "8/9", verbose = verbose)
  }

  if (9L %in% steps) {
    contours_file <- file.path(dirs$base, "contours_reduced.rda")
    if (!file.exists(contours_file)) {
      cli::cli_abort(c(
        "Step 9 requires contours_reduced.rda which doesn't exist",
        "i" = "Run steps 5-8 first to generate contour data"
      ))
    }

    if (verbose) cli::cli_progress_step("9/9 Building final atlas")

    sf_data <- build_contour_sf( # nolint: object_usage_linter.
      contours_file, views, cortex_slices
    )

    atlas <- brain_atlas(
      atlas = atlas_name,
      type = "subcortical",
      palette = components$palette,
      core = components$core,
      data = brain_data_subcortical(sf = sf_data, meshes = components$meshes_df)
    )
    if (verbose) cli::cli_progress_done()

    if (cleanup) {
      unlink(dirs$base, recursive = TRUE)
      if (verbose) cli::cli_alert_success("Temporary files removed")
    }

    if (verbose) {
      cli::cli_alert_success(
        "Subcortical atlas created with {nrow(components$core)} structures"
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


# Subcortical step functions ----

#' @noRd
subcort_create_meshes <- function(
  input_volume, colortable, dirs, skip_existing, verbose,
  decimate = 0.5
) {
  p <- progressor(steps = nrow(colortable))

  meshes_list <- future_map2(
    colortable$idx,
    colortable$label,
    function(label_id, label_name) {
      mesh <- tryCatch(
        tessellate_label(
          volume_file = input_volume,
          label_id = label_id,
          output_dir = dirs$meshes,
          verbose = get_verbose(), # nolint: object_usage_linter
          skip_existing = skip_existing
        ),
        error = function(e) {
          if (verbose) {
            cli::cli_warn(
              "Failed to create mesh for {label_name}: {e$message}"
            )
          }
          NULL
        }
      )
      p()
      mesh
    },
    .options = furrr_options(
      packages = "ggsegExtra",
      globals = c("input_volume", "dirs", "verbose", "skip_existing", "p")
    )
  )
  names(meshes_list) <- colortable$label
  meshes_list <- Filter(Negate(is.null), meshes_list)

  if (length(meshes_list) == 0) {
    cli::cli_abort("No meshes were successfully created")
  }

  meshes_list <- center_meshes(meshes_list)

  if (!is.null(decimate) && decimate < 1) {
    if (verbose) {
      orig_faces <- sum(vapply(
        meshes_list, function(m) nrow(m$faces), integer(1)
      ))
      cli::cli_alert_info(
        "Decimating meshes to {decimate * 100}% of original faces"
      )
    }
    meshes_list <- lapply(meshes_list, decimate_mesh, percent = decimate)
    if (verbose) {
      new_faces <- sum(vapply(
        meshes_list, function(m) nrow(m$faces), integer(1)
      ))
      cli::cli_alert_success(
        "Reduced from {orig_faces} to {new_faces} faces ({round(new_faces/orig_faces * 100)}%)"
      )
    }
  }

  if (verbose) {
    cli::cli_alert_success("Created {length(meshes_list)} meshes")
  }

  meshes_list
}


#' @noRd
subcort_build_components <- function(colortable, meshes_list) {
  all_data <- lapply(names(meshes_list), function(label_name) {
    ct_row <- colortable[colortable$label == label_name, ]
    tibble(
      hemi = detect_hemi(label_name),
      region = clean_region_name(label_name),
      label = label_name,
      colour = ct_row$color[1],
      mesh = list(meshes_list[[label_name]])
    )
  })

  atlas_data <- bind_rows(all_data)
  build_atlas_components(atlas_data)
}


#' @noRd
subcort_create_snapshots <- function(
  input_volume, colortable, views, dirs, skip_existing
) {
  vol <- read_volume(input_volume)
  dims <- dim(vol)

  if (is.null(views)) {
    views <- default_subcortical_views(dims)
  }

  cortex_slices <- create_cortex_slices(views, dims)
  cortex_labels <- detect_cortex_labels(vol)

  snapshot_grid <- expand.grid(
    struct_idx = seq_len(nrow(colortable)),
    view_idx = seq_len(nrow(views)),
    stringsAsFactors = FALSE
  )

  p <- progressor(steps = nrow(snapshot_grid))

  invisible(future_pmap(
    list(
      label_id = colortable$idx[snapshot_grid$struct_idx],
      label_name = colortable$label[snapshot_grid$struct_idx],
      view_type = views$type[snapshot_grid$view_idx],
      view_start = views$start[snapshot_grid$view_idx],
      view_end = views$end[snapshot_grid$view_idx],
      view_name = views$name[snapshot_grid$view_idx]
    ),
    function(
      label_id, label_name, view_type, view_start, view_end, view_name
    ) {
      structure_vol <- array(0L, dim = dims)
      structure_vol[vol == label_id] <- 1L

      if (sum(structure_vol) > 0) {
        hemi <- extract_hemi_from_view(view_type, view_name)
        snapshot_partial_projection(
          vol = structure_vol,
          view = view_type,
          start = view_start,
          end = view_end,
          view_name = view_name,
          label = label_name,
          output_dir = dirs$snaps,
          colour = "red",
          hemi = hemi,
          skip_existing = skip_existing
        )
      }
      p()
      NULL
    },
    .options = furrr_options(
      packages = "ggsegExtra",
      globals = c("dims", "vol", "dirs", "skip_existing", "p")
    )
  ))

  cortex_vol <- array(0L, dim = dims)
  invisible(lapply(c(cortex_labels$left, cortex_labels$right), function(lbl) {
    cortex_vol[vol == lbl] <<- 1L
  }))

  invisible(lapply(seq_len(nrow(cortex_slices)), function(i) {
    cs <- cortex_slices[i, ]
    hemi <- extract_hemi_from_view(cs$view, cs$name)
    snapshot_cortex_slice(
      vol = cortex_vol,
      x = cs$x, y = cs$y, z = cs$z,
      slice_view = cs$view,
      view_name = cs$name,
      hemi = hemi,
      output_dir = dirs$snaps,
      skip_existing = skip_existing
    )
  }))

  list(views = views, cortex_slices = cortex_slices)
}


#' Default subcortical atlas view configuration
#'
#' Creates projection views calibrated for subcortical structures.
#' Uses anatomically-calibrated ranges based on typical aseg label positions.
#'
#' @param dims Volume dimensions (3-element vector)
#'
#' @return data.frame with columns: name, type, start, end
#' @keywords internal
default_subcortical_views <- function(dims) {
  mid_x <- dims[1] %/% 2
  chunk_size <- 10
  scale <- dims[1] / 256

  z_lo <- round(85 * scale)
  z_hi <- round(152 * scale)
  y_lo <- round(110 * scale)
  y_hi <- round(154 * scale)

  axial_views <- make_view_chunks(z_lo, z_hi, chunk_size, "axial")
  coronal_views <- make_view_chunks(y_lo, y_hi, chunk_size, "coronal")

  sagittal_views <- data.frame(
    name = "sagittal",
    type = "sagittal",
    start = mid_x,
    end = mid_x,
    stringsAsFactors = FALSE
  )

  rbind(axial_views, coronal_views, sagittal_views)
}


