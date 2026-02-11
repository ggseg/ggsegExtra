# Whole-brain atlas creation ----

#' Create atlas from whole-brain volumetric parcellation
#'
#' @description
#' `r lifecycle::badge("experimental")`
#'
#' Build a brain atlas from a volumetric parcellation (NIfTI/MGZ) that
#' contains both cortical and subcortical regions. Cortical regions are
#' projected onto the fsaverage5 surface via FreeSurfer's `mri_vol2surf`
#' and rendered as surface views, while subcortical regions go through
#' the mesh-based subcortical pipeline.
#'
#' The function automatically classifies regions as cortical or subcortical
#' based on vertex coverage on the surface projection, or you can manually
#' specify the split via `cortical_labels` and `subcortical_labels`.
#'
#' Requires FreeSurfer.
#'
#' @param input_volume Path to volumetric parcellation in MNI152 space
#'   (.mgz, .nii, .nii.gz).
#' @param input_lut Path to FreeSurfer-style colour lookup table, or a
#'   data.frame with columns `idx`, `label`, `R`, `G`, `B`, `A`.
#'   If NULL, generic names and no palette.
#' @template atlas_name
#' @template output_dir
#' @param projfrac Cortical depth fraction for projection (0 = white surface,
#'   1 = pial surface). Only used when `projfrac_range` is NULL. Default 0.5.
#' @param projfrac_range Numeric vector `c(min, max, delta)` for multi-depth
#'   projection via `mri_vol2surf --projfrac-max`. Samples at multiple cortical
#'   depths and takes the maximum label value at each vertex, giving much better
#'   surface coverage than single-depth projection. Default `c(0, 1, 0.1)`.
#'   Set to NULL to use single-depth `projfrac` instead.
#' @param subject Target surface subject. Default "fsaverage5".
#' @param regheader If TRUE (default), assumes volume RAS coordinates match
#'   the subject space and uses `--regheader`. Works well for standard
#'   MNI152-space volumes. If FALSE, uses FreeSurfer's `--mni152reg`
#'   registration (may produce noisy results due to surf2surf resampling).
#' @param min_vertices Minimum total vertex count across hemispheres for a
#'   label to be classified as cortical. Default 50.
#' @param cortical_labels Character vector of label names to force as cortical.
#' @param subcortical_labels Character vector of label names to force as
#'   subcortical.
#' @param cortical_views Views for cortical sub-pipeline.
#'   Default `c("lateral", "medial")`.
#' @param subcortical_views Views for subcortical sub-pipeline. Default NULL
#'   (auto-detected).
#' @param decimate Mesh decimation ratio for subcortical meshes (0-1).
#'   Default 0.5.
#' @template tolerance
#' @template smoothness
#' @template cleanup
#' @template verbose
#' @template skip_existing
#' @param steps Which pipeline steps to run. Default NULL runs all steps.
#'   Steps are:
#'   \itemize{
#'     \item 1: Project volume onto surface
#'     \item 2: Split labels into cortical/subcortical
#'     \item 3: Run cortical pipeline
#'     \item 4: Run subcortical pipeline
#'   }
#'   Use `steps = 1:2` to run projection and split only.
#'
#' @return A named list with elements `cortical` and `subcortical`, each a
#'   `ggseg_atlas` object (or NULL if no regions of that type exist).
#' @export
#' @importFrom dplyr tibble bind_rows filter
#' @importFrom freesurfer have_fs fs_subj_dir
#' @importFrom grDevices rgb
#' @importFrom tools file_path_sans_ext
#'
#' @examples
#' \dontrun{
#' # Create from MNI152-space NIfTI with LUT
#' result <- create_wholebrain_atlas(
#'   input_volume = "shen_268.nii.gz",
#'   input_lut = "shen_268_LUT.txt",
#'   atlas_name = "shen268"
#' )
#' result$cortical   # surface-based cortical atlas
#' result$subcortical # mesh-based subcortical atlas
#'
#' # Projection + split only (inspect classification)
#' result <- create_wholebrain_atlas(
#'   input_volume = "atlas.nii.gz",
#'   input_lut = "atlas_LUT.txt",
#'   steps = 1:2
#' )
#' }
create_wholebrain_atlas <- function(
  input_volume,
  input_lut = NULL,
  atlas_name = NULL,
  output_dir = NULL,
  projfrac = 0.5,
  projfrac_range = c(0, 1, 0.1),
  subject = "fsaverage5",
  regheader = TRUE,
  min_vertices = 50L,
  cortical_labels = NULL,
  subcortical_labels = NULL,
  cortical_views = c("lateral", "medial"),
  subcortical_views = NULL,
  decimate = 0.5,
  tolerance = NULL,
  smoothness = NULL,
  cleanup = NULL,
  verbose = get_verbose(), # nolint: object_usage_linter
  skip_existing = NULL,
  steps = NULL
) {
  start_time <- Sys.time()

  config <- validate_wholebrain_config(
    input_volume = input_volume,
    input_lut = input_lut,
    atlas_name = atlas_name,
    output_dir = output_dir,
    projfrac = projfrac,
    projfrac_range = projfrac_range,
    subject = subject,
    regheader = regheader,
    min_vertices = min_vertices,
    verbose = verbose,
    cleanup = cleanup,
    skip_existing = skip_existing,
    tolerance = tolerance,
    smoothness = smoothness,
    steps = steps
  )

  dirs <- setup_atlas_dirs(config$output_dir, config$atlas_name, type = "cortical")

  if (config$verbose) {
    cli::cli_h1("Creating whole-brain atlas {.val {config$atlas_name}}")
    cli::cli_alert_info("Volume: {.path {config$input_volume}}")
    if (!is.null(config$input_lut)) {
      cli::cli_alert_info("Color LUT: {.path {config$input_lut}}")
    }
    cli::cli_alert_info(
      "Setting output directory to {.path {config$output_dir}}"
    )
  }

  projection <- wholebrain_resolve_projection(config, dirs)

  split <- wholebrain_resolve_split(
    config, dirs, projection,
    cortical_labels = cortical_labels,
    subcortical_labels = subcortical_labels
  )

  if (max(config$steps) <= 2L) {
    if (config$verbose) {
      log_elapsed(start_time)
    }
    return(invisible(split))
  }

  cortical_atlas <- NULL
  subcortical_atlas <- NULL

  if (3L %in% config$steps && length(split$cortical_labels) > 0) {
    cortical_atlas <- wholebrain_run_cortical(
      config, dirs, projection, split,
      views = cortical_views
    )
  }

  if (4L %in% config$steps && length(split$subcortical_labels) > 0) {
    subcortical_atlas <- wholebrain_run_subcortical(
      config, dirs, split,
      colortable = projection$colortable,
      views = subcortical_views,
      decimate = decimate
    )
  }

  result <- list(
    cortical = cortical_atlas,
    subcortical = subcortical_atlas
  )

  if (config$cleanup) {
    unlink(dirs$base, recursive = TRUE)
    if (config$verbose) cli::cli_alert_success("Temporary files removed")
  }

  if (config$verbose) {
    n_cort <- if (!is.null(cortical_atlas)) nrow(cortical_atlas$core) else 0L
    n_sub <- if (!is.null(subcortical_atlas)) {
      nrow(subcortical_atlas$core)
    } else {
      0L
    }
    cli::cli_alert_success(
      "Whole-brain atlas created: {n_cort} cortical, {n_sub} subcortical"
    )
    log_elapsed(start_time)
  }

  result
}


# Validation ----

#' @noRd
validate_wholebrain_config <- function(
  input_volume, input_lut, atlas_name, output_dir,
  projfrac, projfrac_range, subject, regheader, min_vertices,
  verbose, cleanup, skip_existing,
  tolerance, smoothness, steps
) {
  verbose <- is_verbose(verbose)
  cleanup <- get_cleanup(cleanup)
  skip_existing <- get_skip_existing(skip_existing)
  tolerance <- get_tolerance(tolerance)
  smoothness <- get_smoothness(smoothness)
  output_dir <- get_output_dir(output_dir)

  if (is.null(steps)) steps <- 1L:4L
  steps <- as.integer(steps)

  check_fs(abort = TRUE)

  if (!file.exists(input_volume)) {
    cli::cli_abort("Volume file not found: {.path {input_volume}}")
  }
  if (!is.null(input_lut) && is.character(input_lut) &&
      !file.exists(input_lut)) {
    cli::cli_abort("Color lookup table not found: {.path {input_lut}}")
  }

  output_dir <- normalizePath(output_dir, mustWork = FALSE)

  if (is.null(atlas_name)) {
    atlas_name <- basename(input_volume)
    atlas_name <- sub("\\.(nii\\.gz|nii|mgz)$", "", atlas_name,
                       ignore.case = TRUE)
  }

  list(
    input_volume = input_volume,
    input_lut = input_lut,
    atlas_name = atlas_name,
    output_dir = output_dir,
    projfrac = projfrac,
    projfrac_range = projfrac_range,
    subject = subject,
    regheader = regheader,
    min_vertices = as.integer(min_vertices),
    verbose = verbose,
    cleanup = cleanup,
    skip_existing = skip_existing,
    tolerance = tolerance,
    smoothness = smoothness,
    steps = steps
  )
}


# Step 1: Project volume onto surface ----

#' @noRd
wholebrain_resolve_projection <- function(config, dirs) {
  files <- c(
    file.path(dirs$base, "atlas_data.rds"),
    file.path(dirs$base, "colortable.rds")
  )
  cached <- load_or_run_step(
    1L, config$steps, files, config$skip_existing,
    "Step 1 (Project to surface)"
  )

  if (!cached$run) {
    if (config$verbose) {
      cli::cli_alert_success("1/4 Loaded existing surface projection")
    }
    return(list(
      atlas_data = cached$data[["atlas_data.rds"]],
      colortable = cached$data[["colortable.rds"]]
    ))
  }

  if (config$verbose) {
    cli::cli_progress_step("1/4 Projecting volume onto surface")
  }

  colortable <- if (is.null(config$input_lut)) {
    cli::cli_warn(c(
      "No color lookup table provided",
      "i" = "Region names will be generic (e.g., 'region_0010')",
      "i" = "Atlas will have no palette"
    ))
    generate_colortable_from_volume(config$input_volume)
  } else {
    get_ctab(config$input_lut)
  }

  atlas_data <- wholebrain_project_to_surface(
    input_volume = config$input_volume,
    colortable = colortable,
    subject = config$subject,
    projfrac = config$projfrac,
    projfrac_range = config$projfrac_range,
    regheader = config$regheader,
    output_dir = dirs$base,
    verbose = config$verbose
  )

  saveRDS(atlas_data, file.path(dirs$base, "atlas_data.rds"))
  saveRDS(colortable, file.path(dirs$base, "colortable.rds"))
  if (config$verbose) cli::cli_progress_done()

  list(atlas_data = atlas_data, colortable = colortable)
}


#' @noRd
wholebrain_project_to_surface <- function(
  input_volume, colortable, subject, projfrac,
  projfrac_range, regheader, output_dir, verbose
) {
  surf_dir <- file.path(output_dir, "surface_overlays")
  mkdir(surf_dir)

  all_data <- list()

  for (hemi_short in c("lh", "rh")) {
    hemi <- hemi_to_long(hemi_short)
    output_mgz <- file.path(surf_dir, paste0(hemi_short, "_overlay.nii.gz"))

    reg_opts <- paste0(
      "--interp nearest --trgsubject ", subject
    )
    if (regheader) {
      reg_opts <- paste(reg_opts, "--regheader", subject)
    }

    mri_vol2surf(
      input_file = input_volume,
      output_file = output_mgz,
      hemisphere = hemi_short,
      projfrac = projfrac,
      projfrac_range = projfrac_range,
      mni152reg = !regheader,
      opts = reg_opts,
      verbose = verbose
    )

    if (!file.exists(output_mgz)) {
      cli::cli_abort(c(
        "mri_vol2surf failed to produce output for {hemi_short}",
        "i" = "Expected: {.path {output_mgz}}",
        "i" = "Check that the volume is in the correct space (MNI152 or native)"
      ))
    }

    overlay <- as.integer(c(RNifti::readNifti(output_mgz)))

    n_before <- sum(overlay != 0L)
    overlay <- fill_surface_labels(overlay, hemi_short, subject)
    if (verbose) {
      n_after <- sum(overlay != 0L)
      cli::cli_alert(
        "{hemi_short}: {n_before} -> {n_after} labeled vertices ({sprintf('%.0f%%', 100 * n_after / length(overlay))})"
      )
    }

    unique_labels <- sort(unique(overlay[overlay != 0L]))

    for (label_val in unique_labels) {
      vertex_indices <- which(overlay == label_val) - 1L

      ct_row <- colortable[colortable$idx == label_val, ]
      if (nrow(ct_row) == 0) next

      label_name <- ct_row$label[1]
      safe_name <- gsub("\\(", "_", label_name)
      safe_name <- gsub(")", "", safe_name)
      safe_name <- gsub("/", "-", safe_name)
      colour <- if ("color" %in% names(ct_row)) {
        ct_row$color[1]
      } else if (all(c("R", "G", "B") %in% names(ct_row))) {
        rgb(ct_row$R[1], ct_row$G[1], ct_row$B[1], maxColorValue = 255)
      } else {
        NA_character_
      }

      all_data[[length(all_data) + 1]] <- tibble(
        hemi = hemi,
        region = clean_region_name(label_name),
        label = paste(hemi_short, safe_name, sep = "_"),
        colour = colour,
        vertices = list(vertex_indices),
        source_label = label_name,
        source_idx = label_val
      )
    }
  }

  bind_rows(all_data)
}


# Step 2: Split labels ----

#' @noRd
wholebrain_resolve_split <- function(
  config, dirs, projection,
  cortical_labels = NULL,
  subcortical_labels = NULL
) {
  files <- file.path(dirs$base, "label_split.rds")
  cached <- load_or_run_step(
    2L, config$steps, files, config$skip_existing,
    "Step 2 (Split labels)"
  )

  if (!cached$run) {
    if (config$verbose) {
      cli::cli_alert_success("2/4 Loaded existing label classification")
    }
    return(cached$data[["label_split.rds"]])
  }

  if (config$verbose) {
    cli::cli_progress_step("2/4 Classifying cortical vs subcortical labels")
  }

  split <- wholebrain_classify_labels(
    atlas_data = projection$atlas_data,
    min_vertices = config$min_vertices,
    cortical_labels = cortical_labels,
    subcortical_labels = subcortical_labels,
    verbose = config$verbose
  )

  saveRDS(split, file.path(dirs$base, "label_split.rds"))
  if (config$verbose) cli::cli_progress_done()

  split
}


#' Classify volume labels as cortical or subcortical
#'
#' Uses vertex coverage on the surface projection to decide which labels
#' should be treated as cortical (surface views) vs subcortical (mesh views).
#'
#' @param atlas_data Tibble from `wholebrain_project_to_surface()` with
#'   `source_label` and `vertices` columns.
#' @param min_vertices Minimum total vertex count for cortical classification.
#' @param cortical_labels Manual override: force these labels as cortical.
#' @param subcortical_labels Manual override: force these labels as subcortical.
#' @param verbose Print classification summary.
#'
#' @return Named list with `cortical_labels` and `subcortical_labels`
#'   (character vectors of source label names).
#' @noRd
wholebrain_classify_labels <- function(
  atlas_data,
  min_vertices = 50L,
  cortical_labels = NULL,
  subcortical_labels = NULL,
  verbose = FALSE
) {
  vertex_counts <- tapply(
    vapply(atlas_data$vertices, length, integer(1)),
    atlas_data$source_label,
    sum
  )

  all_labels <- names(vertex_counts)
  classified_cortical <- character(0)
  classified_subcortical <- character(0)

  if (!is.null(cortical_labels)) {
    classified_cortical <- intersect(cortical_labels, all_labels)
  }
  if (!is.null(subcortical_labels)) {
    classified_subcortical <- intersect(subcortical_labels, all_labels)
  }

  remaining <- setdiff(
    all_labels,
    c(classified_cortical, classified_subcortical)
  )

  auto_cortical <- remaining[vertex_counts[remaining] >= min_vertices]
  auto_subcortical <- remaining[vertex_counts[remaining] < min_vertices]

  classified_cortical <- c(classified_cortical, auto_cortical)
  classified_subcortical <- c(classified_subcortical, auto_subcortical)

  if (verbose) {
    cli::cli_alert_info(
      "{length(classified_cortical)} cortical, {length(classified_subcortical)} subcortical labels"
    )
    if (length(classified_subcortical) > 0) {
      sub_info <- paste(
        classified_subcortical,
        paste0("(", vertex_counts[classified_subcortical], "v)"),
        collapse = ", "
      )
      cli::cli_alert("Subcortical: {sub_info}")
    }
  }

  list(
    cortical_labels = classified_cortical,
    subcortical_labels = classified_subcortical,
    vertex_counts = vertex_counts
  )
}


# Step 3: Run cortical pipeline ----

#' @noRd
wholebrain_run_cortical <- function(
  config, dirs, projection, split, views
) {
  if (config$verbose) {
    cli::cli_progress_step(
      "3/4 Running cortical pipeline ({length(split$cortical_labels)} regions)"
    )
  }

  cortical_data <- projection$atlas_data[
    projection$atlas_data$source_label %in% split$cortical_labels,
  ]
  cortical_data <- cortical_data[
    , c("hemi", "region", "label", "colour", "vertices")
  ]

  cortical_name <- paste0(config$atlas_name, "_cortical")
  cortical_dirs <- setup_atlas_dirs(
    config$output_dir, cortical_name, type = "cortical"
  )

  cortical_config <- validate_cortical_config(
    output_dir = config$output_dir,
    verbose = config$verbose,
    cleanup = FALSE,
    skip_existing = config$skip_existing,
    tolerance = config$tolerance,
    smoothness = config$smoothness,
    snapshot_dim = NULL,
    steps = NULL
  )

  step1 <- cortical_resolve_step1(
    cortical_config, cortical_dirs, cortical_name,
    read_fn = function() cortical_data,
    step_label = "Reading projected cortical data",
    cache_label = "Cortical step 1"
  )

  if (max(cortical_config$steps) == 1L) {
    if (config$verbose) cli::cli_progress_done()
    return(step1$atlas_3d)
  }

  hemisphere <- unique(cortical_data$hemi)
  hemi_short <- vapply(hemisphere, hemi_to_short, character(1),
                        USE.NAMES = FALSE)

  atlas <- cortical_pipeline(
    atlas_3d = step1$atlas_3d,
    components = step1$components,
    atlas_name = cortical_name,
    hemisphere = hemi_short,
    views = views,
    region_snapshot_fn = cortical_region_snapshots,
    config = cortical_config,
    dirs = cortical_dirs,
    start_time = Sys.time()
  )

  if (config$verbose) cli::cli_progress_done()
  atlas
}


# Step 4: Run subcortical pipeline ----

#' @noRd
wholebrain_run_subcortical <- function(
  config, dirs, split, colortable, views, decimate
) {
  if (config$verbose) {
    cli::cli_progress_step(
      "4/4 Running subcortical pipeline ({length(split$subcortical_labels)} regions)"
    )
  }

  subcort_ct <- colortable[
    colortable$label %in% split$subcortical_labels,
  ]

  subcort_lut <- file.path(dirs$base, "subcort_lut.txt")
  write_ctab(subcort_ct[, c("idx", "label", "R", "G", "B", "A")], subcort_lut)

  filtered_vol <- file.path(dirs$base, "subcort_volume.nii.gz")
  wholebrain_filter_volume(
    input_volume = config$input_volume,
    keep_labels = subcort_ct$idx,
    output_file = filtered_vol
  )

  subcort_name <- paste0(config$atlas_name, "_subcortical")

  atlas <- create_subcortical_atlas(
    input_volume = filtered_vol,
    input_lut = subcort_lut,
    atlas_name = subcort_name,
    views = views,
    output_dir = config$output_dir,
    decimate = decimate,
    tolerance = config$tolerance,
    smoothness = config$smoothness,
    verbose = config$verbose,
    cleanup = FALSE,
    skip_existing = config$skip_existing
  )

  if (config$verbose) cli::cli_progress_done()
  atlas
}


#' @noRd
wholebrain_filter_volume <- function(input_volume, keep_labels, output_file) {
  vol <- read_volume(input_volume, reorient = FALSE)
  arr <- as.array(vol)
  arr[!arr %in% keep_labels] <- 0L
  filtered <- RNifti::asNifti(arr, reference = vol)
  RNifti::writeNifti(filtered, output_file)
  invisible(output_file)
}


# Surface label dilation ----

#' Fill unlabeled surface vertices via mesh-neighbor dilation
#'
#' After vol2surf projection, many surface vertices remain unlabeled (value 0)
#' due to the sparse sampling. This function iteratively assigns each unlabeled
#' vertex the most common label among its mesh neighbors, using the surface
#' topology (face adjacency) to propagate labels outward until all reachable
#' vertices are filled.
#'
#' @param overlay Integer vector of label values (0 = unlabeled), one per vertex.
#' @param hemi Hemisphere code ("lh" or "rh").
#' @param subject FreeSurfer subject for surface mesh. Default "fsaverage5".
#' @return Integer vector of same length with gaps filled.
#' @noRd
fill_surface_labels <- function(overlay, hemi, subject = "fsaverage5") {
  surf_file <- file.path(
    freesurfer::fs_subj_dir(), subject, "surf",
    paste0(hemi, ".white")
  )
  if (!file.exists(surf_file)) {
    cli::cli_warn("Surface file not found: {.path {surf_file}}, skipping dilation")
    return(overlay)
  }

  surf <- freesurferformats::read.fs.surface(surf_file)
  adj <- build_adjacency(surf$faces, nrow(surf$vertices))

  result <- overlay
  unlabeled <- which(result == 0L)

  while (length(unlabeled) > 0L) {
    newly_labeled <- integer(0)

    for (idx in unlabeled) {
      neighbor_labels <- result[adj[[idx]]]
      neighbor_labels <- neighbor_labels[neighbor_labels != 0L]
      if (length(neighbor_labels) > 0L) {
        tbl <- tabulate(neighbor_labels, nbins = max(neighbor_labels))
        result[idx] <- which.max(tbl)
        newly_labeled <- c(newly_labeled, idx)
      }
    }

    if (length(newly_labeled) == 0L) break
    unlabeled <- setdiff(unlabeled, newly_labeled)
  }

  result
}


#' Build vertex adjacency list from face matrix
#' @param faces n x 3 integer matrix (1-indexed vertex indices)
#' @param n_vertices total number of vertices
#' @return list of integer vectors, one per vertex
#' @noRd
build_adjacency <- function(faces, n_vertices) {
  adj <- vector("list", n_vertices)

  for (i in seq_len(nrow(faces))) {
    v <- faces[i, ]
    adj[[v[1]]] <- c(adj[[v[1]]], v[2], v[3])
    adj[[v[2]]] <- c(adj[[v[2]]], v[1], v[3])
    adj[[v[3]]] <- c(adj[[v[3]]], v[1], v[2])
  }

  lapply(adj, function(x) unique.default(x))
}
