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
#'   or `ASegStatsLUT.txt`). If NULL, generates a colour table from the
#'   volume labels using rainbow colours. Region names will be generic
#'   (e.g., "region_0010").
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
#' @importFrom ggseg.formats brain_atlas subcortical_data
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
  verbose = NULL,
  cleanup = NULL,
  skip_existing = NULL,
  steps = NULL
) {
  start_time <- Sys.time()

  verbose <- is_verbose(verbose)
  cleanup <- get_cleanup(cleanup)
  skip_existing <- get_skip_existing(skip_existing)
  tolerance <- get_tolerance(tolerance)
  smoothness <- get_smoothness(smoothness)
  output_dir <- get_output_dir(output_dir)

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
        "i" = "Generating colours from volume labels",
        "i" = "Region names will be generic (e.g., 'region_0010')"
      ))
      colortable <- generate_colortable_from_volume(input_volume)
    } else {
      colortable <- get_ctab(input_lut)
    }

    if (verbose) {
      cli::cli_alert_info("1/9 Extracting labels from volume")
    }

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
            verbose = FALSE,
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

    if (verbose) {
      cli::cli_progress_done()
    }

    if (length(meshes_list) == 0) {
      cli::cli_abort("No meshes were successfully created")
    }

    meshes_list <- center_meshes(meshes_list)

    if (verbose) {
      cli::cli_alert_success("Created {length(meshes_list)} meshes")
    }

    saveRDS(meshes_list, file.path(dirs$base, "meshes_list.rds"))
  } else if (any(steps > 2L)) {
    meshes_list <- step2$data[["meshes_list.rds"]]
    if (verbose) cli::cli_alert_success("2/9 Loaded existing meshes")
  }

  step3_files <- file.path(dirs$base, "components.rds")
  step3 <- load_or_run_step(
    3L,
    steps,
    step3_files,
    skip_existing,
    "Step 3 (Build atlas data)"
  )

  if (step3$run) {
    if (verbose) {
      cli::cli_progress_step("3/9 Building atlas data")
    }

    all_data <- list()
    for (label_name in names(meshes_list)) {
      ct_row <- colortable[colortable$label == label_name, ]

      hemi <- detect_hemi(label_name)
      region <- clean_region_name(label_name)

      all_data[[length(all_data) + 1]] <- tibble(
        hemi = hemi,
        region = region,
        label = label_name,
        colour = ct_row$color[1],
        mesh = list(meshes_list[[label_name]])
      )
    }

    atlas_data <- bind_rows(all_data)
    components <- build_atlas_components(atlas_data)

    saveRDS(components, file.path(dirs$base, "components.rds"))

    if (max(steps) == 3L) {
      atlas <- brain_atlas(
        atlas = atlas_name,
        type = "subcortical",
        palette = components$palette,
        core = components$core,
        data = subcortical_data(sf = NULL, meshes = components$meshes_df)
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
        elapsed <- round(difftime(Sys.time(), start_time, units = "mins"), 1)
        cli::cli_alert_info("Pipeline completed in {elapsed} minutes")
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
    4L,
    steps,
    step4_files,
    skip_existing,
    "Step 4 (Create snapshots)"
  )

  if (step4$run) {
    if (verbose) {
      cli::cli_progress_step("4/9 Creating projection snapshots")
    }

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
        label_id,
        label_name,
        view_type,
        view_start,
        view_end,
        view_name
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
    for (lbl in c(cortex_labels$left, cortex_labels$right)) {
      cortex_vol[vol == lbl] <- 1L
    }

    for (i in seq_len(nrow(cortex_slices))) {
      cs <- cortex_slices[i, ]
      hemi <- extract_hemi_from_view(cs$view, cs$name)

      snapshot_cortex_slice(
        vol = cortex_vol,
        x = cs$x,
        y = cs$y,
        z = cs$z,
        slice_view = cs$view,
        view_name = cs$name,
        hemi = hemi,
        output_dir = dirs$snaps,
        skip_existing = skip_existing
      )
    }

    saveRDS(views, file.path(dirs$base, "views.rds"))
    saveRDS(cortex_slices, file.path(dirs$base, "cortex_slices.rds"))

    if (verbose) {
      cli::cli_progress_done()
    }
  } else if (any(steps > 4L)) {
    views <- step4$data[["views.rds"]]
    cortex_slices <- step4$data[["cortex_slices.rds"]]
    if (verbose) cli::cli_alert_success("4/9 Loaded existing views")
  }

  cli::cli_progress_done()

  if (5L %in% steps) {
    if (verbose) {
      cli::cli_progress_step("5/9 Processing images")
    }

    files <- list.files(dirs$snaps, full.names = TRUE, pattern = "\\.png$")

    p <- progressor(steps = length(files))
    for (f in files) {
      process_snapshot_image(
        input_file = f,
        output_file = file.path(dirs$inter, basename(f)),
        dilate = dilate,
        skip_existing = skip_existing
      )
      p()
    }

    for (f in list.files(dirs$inter, full.names = TRUE)) {
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

  if (6L %in% steps) {
    extract_contours(
      dirs$masks,
      dirs$base,
      step = "6/9",
      verbose = verbose,
      vertex_size_limits = vertex_size_limits
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

    if (verbose) {
      cli::cli_alert_info("9/9 Building final atlas")
    }

    conts <- make_multipolygon(contours_file)

    filenm_base <- sub("\\.png$", "", conts$filenm)

    all_view_names <- c(views$name, cortex_slices$name)
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
    sf_data <- dplyr::arrange(sf_data, dplyr::desc(grepl("cortex", label, ignore.case = TRUE)))

    atlas <- brain_atlas(
      atlas = atlas_name,
      type = "subcortical",
      palette = components$palette,
      core = components$core,
      data = subcortical_data(sf = sf_data, meshes = components$meshes_df)
    )

    if (cleanup) {
      unlink(dirs$base, recursive = TRUE)
      if (verbose) cli::cli_alert_success("Temporary files removed")
    }

    if (verbose) {
      cli::cli_alert_success(
        "Subcortical atlas created with {nrow(components$core)} structures"
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


#' Tessellate a single label from a volume
#'
#' Creates a mesh from a single label in a segmentation volume using
#' FreeSurfer's tessellation pipeline.
#'
#' @param volume_file Path to segmentation volume
#' @param label_id Numeric label ID
#' @param output_dir Output directory for intermediate files
#' @param verbose Print progress
#' @param skip_existing If TRUE, skip files that already exist
#'
#' @return list with vertices (data.frame) and faces (data.frame)
#' @keywords internal
tessellate_label <- function(
  volume_file,
  label_id,
  output_dir,
  verbose = FALSE,
  skip_existing = TRUE
) {
  check_fs(abort = TRUE)

  label_str <- sprintf("%04d", label_id)
  base_name <- file.path(output_dir, label_str)

  pretess_file <- paste0(base_name, "_pretess.mgz")
  tess_file <- paste0(base_name, "_tess")
  smooth_file <- paste0(base_name, "_smooth")

  if (skip_existing && file.exists(smooth_file)) {
    return(read_fs_surface(smooth_file))
  }

  if (!skip_existing || !file.exists(pretess_file)) {
    mri_pretess(
      template = volume_file,
      label = label_id,
      output_file = pretess_file,
      verbose = verbose
    )
  }

  if (!file.exists(pretess_file)) {
    cli::cli_abort("Pre-tessellation failed for label {label_id}")
  }

  if (!skip_existing || !file.exists(tess_file)) {
    mri_tessellate(
      input_file = pretess_file,
      label = label_id,
      output_file = tess_file,
      verbose = verbose
    )
  }

  if (!file.exists(tess_file)) {
    cli::cli_abort("Tessellation failed for label {label_id}")
  }

  if (!skip_existing || !file.exists(smooth_file)) {
    mri_smooth(
      input_file = tess_file,
      output_file = smooth_file,
      verbose = verbose
    )
  }

  if (!file.exists(smooth_file)) {
    cli::cli_abort("Smoothing failed for label {label_id}")
  }

  read_fs_surface(smooth_file)
}


#' Read FreeSurfer surface file
#'
#' Reads FreeSurfer surface files including QUAD format from mri_tessellate.
#' Uses FreeSurfer's mris_convert for robust handling of all surface formats.
#'
#' @param file Path to FreeSurfer surface file
#' @return list with vertices and faces data.frames (faces are 1-indexed)
#' @keywords internal
read_fs_surface <- function(file) {
  dpv_file <- paste0(file, ".dpv")

  result <- tryCatch(
    {
      surf2asc(file, dpv_file, verbose = FALSE)
      mesh <- read_dpv(dpv_file)

      vertices <- mesh$vertices
      faces <- data.frame(
        i = mesh$faces$i + 1L,
        j = mesh$faces$j + 1L,
        k = mesh$faces$k + 1L
      )

      list(vertices = vertices, faces = faces)
    },
    error = function(e) NULL
  )

  if (!is.null(result)) {
    return(result)
  }

  if (!requireNamespace("freesurferformats", quietly = TRUE)) {
    cli::cli_abort(c(
      "Failed to read surface file: {.path {file}}",
      "i" = "FreeSurfer conversion failed and freesurferformats not available"
    ))
  }

  surf <- freesurferformats::read.fs.surface(file)

  vertices <- data.frame(
    x = surf$vertices[, 1],
    y = surf$vertices[, 2],
    z = surf$vertices[, 3]
  )
  faces <- data.frame(
    i = surf$faces[, 1],
    j = surf$faces[, 2],
    k = surf$faces[, 3]
  )

  list(vertices = vertices, faces = faces)
}


#' Generate colour table from volume labels
#'
#' Creates a colour lookup table from unique labels in a volume file.
#' Uses rainbow colours and generic region names.
#'
#' @param volume_file Path to volume file
#' @return data.frame with columns: idx, label, R, G, B, A, roi, color
#' @keywords internal
#' @importFrom grDevices rainbow col2rgb
generate_colortable_from_volume <- function(volume_file) {
  vol <- read_volume(volume_file)
  vol_labels <- sort(unique(c(vol)))
  vol_labels <- vol_labels[vol_labels != 0]

  n_labels <- length(vol_labels)
  colours <- rainbow(n_labels)
  rgb_vals <- col2rgb(colours)

  data.frame(
    idx = vol_labels,
    label = sprintf("region_%04d", vol_labels),
    R = rgb_vals["red", ],
    G = rgb_vals["green", ],
    B = rgb_vals["blue", ],
    A = 0L,
    roi = sprintf("%04d", vol_labels),
    color = colours,
    stringsAsFactors = FALSE
  )
}


## quiets concerns of R CMD check
if (getRversion() >= "2.15.1") {
  utils::globalVariables(c(
    "lab",
    "data",
    "file",
    "roi",
    "mni_x",
    "mni_y",
    "mni_z",
    "view_type"
  ))
}


#' Create 2D geometry for subcortical atlas using projections
#'
#' Generate polygon outlines for subcortical structures using volumetric
#' projections instead of slices. This shows all structures in their spatial
#' relationships - like an X-ray view.
#'
#' Subcortical structures are projected (all voxels visible), while cortex
#' reference outlines use single slices (to show outline, not filled blob).
#'
#' @param input_volume Path to segmentation volume
#' @param colortable Color lookup table data.frame
#' @param views A data.frame defining projection views. Columns:
#'   `name` (view label), `type` ("axial", "coronal", "sagittal"),
#'   `start` (first slice), `end` (last slice).
#' @param cortex_slices A data.frame specifying cortex slice positions.
#'   Columns: `x`, `y`, `z`, `view`, `name`. Cortex uses single slices
#'   (not projections) to show outlines rather than filled blobs.
#' @param output_dir Output directory for intermediate files
#' @param vertex_size_limits Size limits for contour filtering
#' @param dilate Dilation amount for image processing
#' @param tolerance Vertex reduction tolerance
#' @param smoothness Contour smoothing amount
#' @param verbose Print progress
#' @param cleanup Remove intermediate files
#' @param skip_existing Skip existing files
#'
#' @return sf data.frame with label, view, and geometry columns
#' @keywords internal
create_subcortical_geometry_projection <- function(
  input_volume,
  colortable,
  views = NULL,
  cortex_slices = NULL,
  output_dir = NULL,
  vertex_size_limits = NULL,
  dilate = NULL,
  tolerance = NULL,
  smoothness = NULL,
  verbose = NULL,
  cleanup = NULL,
  skip_existing = NULL
) {
  verbose <- is_verbose(verbose)
  cleanup <- get_cleanup(cleanup)
  skip_existing <- get_skip_existing(skip_existing)
  tolerance <- get_tolerance(tolerance)
  smoothness <- get_smoothness(smoothness)
  output_dir <- get_output_dir(output_dir)

  output_dir <- normalizePath(output_dir, mustWork = FALSE)

  dirs <- list(
    base = file.path(output_dir, "subcort_proj_geom"),
    snaps = file.path(output_dir, "subcort_proj_geom", "snapshots"),
    inter = file.path(output_dir, "subcort_proj_geom", "interim"),
    masks = file.path(output_dir, "subcort_proj_geom", "masks")
  )
  for (d in dirs) {
    mkdir(d)
  }

  if (verbose) {
    cli::cli_alert_info("Reading volume")
  }

  vol <- read_volume(input_volume)
  dims <- dim(vol)

  if (is.null(views)) {
    mid_x <- dims[1] %/% 2
    views <- data.frame(
      name = c("axial", "coronal", "sagittal_left", "sagittal_right"),
      type = c("axial", "coronal", "sagittal", "sagittal"),
      start = c(1, 1, 1, mid_x + 1),
      end = c(dims[3], dims[2], mid_x, dims[1]),
      stringsAsFactors = FALSE
    )
  }

  if (is.null(cortex_slices)) {
    cortex_slices <- data.frame(
      x = c(NA, NA, dims[1] / 4, dims[1] * 3 / 4),
      y = c(dims[2] / 2, NA, NA, NA),
      z = c(NA, dims[3] / 2, NA, NA),
      view = c("coronal", "axial", "sagittal_left", "sagittal_right"),
      name = c("coronal", "axial", "sagittal_left", "sagittal_right"),
      stringsAsFactors = FALSE
    )
  }

  cortex_labels <- detect_cortex_labels(vol)

  if (verbose) {
    cli::cli_alert_info(
      "Creating projections for {nrow(colortable)} structures"
    )
  }

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
    function(label_id, label_name, view_type, view_start, view_end, view_name) {
      structure_vol <- array(0L, dim = dims)
      structure_vol[vol == label_id] <- 1L

      if (sum(structure_vol) > 0) {
        snapshot_partial_projection(
          vol = structure_vol,
          view = view_type,
          start = view_start,
          end = view_end,
          view_name = view_name,
          label = label_name,
          output_dir = dirs$snaps,
          colour = "red",
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

  if (verbose) {
    cli::cli_alert_info("Creating cortex reference slices")
  }

  cortex_vol <- array(0L, dim = dims)
  for (lbl in c(cortex_labels$left, cortex_labels$right)) {
    cortex_vol[vol == lbl] <- 1L
  }

  for (i in seq_len(nrow(cortex_slices))) {
    cs <- cortex_slices[i, ]

    snapshot_cortex_slice(
      vol = cortex_vol,
      x = cs$x,
      y = cs$y,
      z = cs$z,
      slice_view = cs$view,
      view_name = cs$name,
      hemi = "cortex",
      output_dir = dirs$snaps,
      skip_existing = skip_existing
    )
  }

  if (verbose) {
    cli::cli_alert_info("Processing images")
  }

  files <- list.files(dirs$snaps, full.names = TRUE, pattern = "\\.png$")

  for (f in files) {
    process_snapshot_image(
      input_file = f,
      output_file = file.path(dirs$inter, basename(f)),
      dilate = dilate,
      skip_existing = skip_existing
    )
  }

  for (f in list.files(dirs$inter, full.names = TRUE)) {
    extract_alpha_mask(
      f,
      file.path(dirs$masks, basename(f)),
      skip_existing = skip_existing
    )
  }

  extract_contours(
    dirs$masks,
    dirs$base,
    step = "",
    verbose = verbose,
    vertex_size_limits = vertex_size_limits
  )
  smooth_contours(dirs$base, smoothness, step = "", verbose = verbose)
  reduce_vertex(dirs$base, tolerance, step = "", verbose = verbose)

  if (verbose) {
    cli::cli_alert_info("Building sf geometry")
  }

  conts <- make_multipolygon(file.path(dirs$base, "contours_reduced.rda"))

  filenm_base <- sub("\\.png$", "", conts$filenm)

  conts$view <- vapply(
    filenm_base,
    function(fn) {
      for (vn in c(views$name, cortex_slices$name)) {
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

  if (cleanup) {
    unlink(dirs$base, recursive = TRUE)
  }

  sf_data
}
