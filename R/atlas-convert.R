# Legacy atlas conversion ----

#' Convert legacy ggseg atlases to brain_atlas format
#'
#' @description
#' `r lifecycle::badge("superseded")`
#'
#' Convert old-style ggseg (2D) and ggseg3d (3D) atlases into the new
#' `brain_atlas` format. This is a bridge function for working with existing
#' atlases during the transition period.
#'
#' For new atlases, use [create_cortical_atlas()] or
#' [create_subcortical_atlas()]
#' instead - they produce better results with proper vertex indices.
#'
#' The function handles three scenarios:
#' - **Both 2D and 3D**: Merges geometry with vertex data

#' - **3D only**: Extracts vertices, optionally computes indices from mesh
#' - **2D only**: Keeps geometry, 3D rendering unavailable
#'
#' If the 3D atlas already contains vertex indices (newer ggseg3d atlases),
#' those are preserved. Otherwise, if `compute_vertices = TRUE` and FreeSurfer
#' is available, vertex indices are computed by matching mesh coordinates to
#' the brain surface.
#'
#' @param atlas_2d A legacy `brain_atlas` or `ggseg_atlas` with 2D geometry,
#'   or NULL.
#' @param atlas_3d A `ggseg3d_atlas` with mesh data, or NULL.
#' @param atlas_name Name for the output atlas. If NULL, derived from input.
#' @param type Atlas type: `"cortical"` or `"subcortical"`. If NULL, inferred
#'   from the input atlases.
#' @param compute_vertices If TRUE and the 3D atlas lacks vertex indices,
#'   attempt to compute them by matching mesh coordinates to a FreeSurfer
#'   surface. Requires FreeSurfer. Default FALSE for speed.
#' @param surface Which surface to match against when computing vertices
#'   (e.g., `"inflated"`). Must match the 3D atlas surface exactly.
#' @param subject FreeSurfer subject for surface coordinates (default
#'   `"fsaverage5"`).
#' @param tolerance Coordinate matching tolerance when computing vertices.
#'
#' @return A `brain_atlas` object.
#' @export
#' @importFrom dplyr case_when distinct
#' @importFrom rlang %||%
#' @importFrom tidyr unnest
#'
#' @examples
#' \dontrun{
#' # Convert a 3D atlas (quick, uses existing data)
#' new_atlas <- unify_legacy_atlases(atlas_3d = dk_3d)
#'
#' # Merge 2D and 3D atlases
#' new_atlas <- unify_legacy_atlases(atlas_2d = dk, atlas_3d = dk_3d)
#'
#' # Compute vertex indices from mesh (slower, needs FreeSurfer)
#' new_atlas <- unify_legacy_atlases(
#'   atlas_3d = dk_3d,
#'   compute_vertices = TRUE
#' )
#' }
unify_legacy_atlases <- function(
    atlas_2d = NULL,
    atlas_3d = NULL,
    atlas_name = NULL,
    type = NULL,
    compute_vertices = FALSE,
    surface = "inflated",
    subject = "fsaverage5",
    tolerance = 1e-4) {
  lifecycle::signal_stage("superseded", "unify_legacy_atlases()")

  has_2d <- !is.null(atlas_2d)
  has_3d <- !is.null(atlas_3d)

  if (!has_2d && !has_3d) {
    cli::cli_abort(
      "At least one of {.arg atlas_2d} or {.arg atlas_3d} must be provided."
    )
  }

  if (has_2d && !inherits(atlas_2d, "brain_atlas")) {
    cli::cli_abort("{.arg atlas_2d} must be a {.cls brain_atlas} object.")
  }

  if (has_3d && !("ggseg_3d" %in% names(atlas_3d))) {
    cli::cli_abort("{.arg atlas_3d} must have a {.field ggseg_3d} column.")
  }

  original_palette <- if (has_2d) atlas_2d$palette else NULL

  if (has_2d && is.null(atlas_2d$core)) {
    atlas_2d <- ggseg.formats::convert_legacy_brain_atlas(atlas_2d)
  }

  atlas_name <- atlas_name %||%
    (if (has_2d) atlas_2d$atlas else gsub("_3d$", "", atlas_3d$atlas[1]))
  type <- type %||% infer_atlas_type(has_2d, atlas_2d, atlas_3d)

  sf_data <- if (has_2d) {
    if (!is.null(atlas_2d$data$sf)) atlas_2d$data$sf else atlas_2d$sf
  } else {
    NULL
  }
  core <- if (has_2d) atlas_2d$core else NULL
  palette <- if (has_2d) atlas_2d$palette else NULL

  vertices_df <- NULL
  meshes_df <- NULL

  if (has_3d) {
    dt <- tidyr::unnest(atlas_3d, ggseg_3d)

    if (is.null(core)) {
      core <- dplyr::distinct(dt, hemi, region, label)
    }

    if (is.null(palette) && "colour" %in% names(dt)) {
      palette <- stats::setNames(dt$colour, dt$label)
      palette <- palette[!duplicated(names(palette))]
    }

    if (type %in% c("subcortical", "tract") && "mesh" %in% names(dt)) {
      meshes_df <- data.frame(label = dt$label, stringsAsFactors = FALSE)
      meshes_df$mesh <- lapply(seq_len(nrow(dt)), function(i) {
        m <- dt$mesh[[i]]
        if (is.null(m)) {
          return(NULL)
        }
        list(
          vertices = data.frame(
            x = if ("vb" %in% names(m)) m$vb[1, ] else m$vertices$x,
            y = if ("vb" %in% names(m)) m$vb[2, ] else m$vertices$y,
            z = if ("vb" %in% names(m)) m$vb[3, ] else m$vertices$z
          ),
          faces = data.frame(
            i = if ("it" %in% names(m)) m$it[1, ] else m$faces$i,
            j = if ("it" %in% names(m)) m$it[2, ] else m$faces$j,
            k = if ("it" %in% names(m)) m$it[3, ] else m$faces$k
          )
        )
      })
      cli::cli_inform(c("i" = "Extracted meshes from 3D atlas."))
    } else if (has_vertex_data(dt)) {
      vertices_df <- data.frame(label = dt$label, stringsAsFactors = FALSE)
      vertices_df$vertices <- dt$vertices
      cli::cli_inform(c("i" = "Using existing vertex indices from 3D atlas."))
    } else if (compute_vertices && type == "cortical") {
      atlas_3d_flat <- flatten_ggseg3d_atlas(atlas_3d, surface = surface)
      vertices_list <- compute_vertex_indices(
        atlas_3d_flat,
        surface = surface, subject = subject, tolerance = tolerance
      )
      vertices_df <- data.frame(label = core$label, stringsAsFactors = FALSE)
      vertices_df$vertices <- lapply(vertices_df$label, function(lbl) {
        if (!is.na(lbl) && lbl %in% names(vertices_list)) {
          vertices_list[[lbl]]
        } else {
          integer(0)
        }
      })
    } else if (type == "cortical") {
      vertices_list <- infer_vertices_from_meshes(atlas_3d, surface = surface)
      if (!is.null(vertices_list)) {
        vertices_df <- data.frame(
          label = names(vertices_list), stringsAsFactors = FALSE
        )
        vertices_df$vertices <- unname(vertices_list)
        cli::cli_inform(c(
          "i" = "Inferred vertex indices from mesh coordinates."
        ))
      } else {
        cli::cli_warn(c(
          "Could not infer vertex indices from mesh data.",
          "i" = "Set {.code compute_vertices = TRUE} to compute from FreeSurfer.",
          "i" = "For best results, recreate with {.fn create_cortical_atlas}."
        ))
        if (is.null(sf_data)) {
          cli::cli_abort(c(
            "Cannot create cortical atlas without vertex indices or 2D geometry.",
            "i" = "Provide a 2D atlas or set {.code compute_vertices = TRUE}."
          ))
        }
      }
    }
  } else if (has_2d && !is.null(atlas_2d$data$vertices) &&
               "vertices" %in% names(atlas_2d$data$vertices)) {
    vertices_df <- atlas_2d$data$vertices
    cli::cli_inform(c("i" = "Using existing vertex data from 2D atlas."))
  } else {
    vertices_df <- NULL
    cli::cli_inform(c(
      "i" = "Created atlas from 2D only.",
      "i" = "3D rendering will not be available without vertex data."
    ))
  }

  if (is.null(palette) || !any(names(palette) %in% core$label)) {
    palette <- remap_palette_to_labels(original_palette, core)
  }

  data <- switch(
    type,
    "cortical" = ggseg.formats::cortical_data(
      sf = sf_data, vertices = vertices_df
    ),
    "subcortical" = ggseg.formats::subcortical_data(
      sf = sf_data, meshes = meshes_df
    ),
    "tract" = ggseg.formats::tract_data(
      sf = sf_data, meshes = meshes_df
    ),
    ggseg.formats::cortical_data(sf = sf_data, vertices = vertices_df)
  )

  ggseg.formats::brain_atlas(
    atlas = atlas_name,
    type = type,
    palette = palette,
    core = core,
    data = data
  )
}


#' Infer atlas type from input atlases
#' @noRd
infer_atlas_type <- function(has_2d, atlas_2d, atlas_3d) {
  if (has_2d) {
    return(atlas_2d$type)
  }
  if (any(atlas_3d$hemi == "subcort")) {
    return("subcortical")
  }
  "cortical"
}


#' Check if data frame has non-empty vertex data
#' @noRd
has_vertex_data <- function(dt) {
  if (!("vertices" %in% names(dt))) {
    return(FALSE)
  }
  !all(vapply(dt$vertices, function(v) length(v) == 0, logical(1)))
}


#' Remap region-keyed palette to label-keyed palette
#' @noRd
remap_palette_to_labels <- function(palette, core) {
  if (is.null(palette)) return(NULL)

  new_palette <- character(0)
  for (region_name in names(palette)) {
    labels <- core$label[!is.na(core$region) & core$region == region_name]
    for (lbl in labels) {
      new_palette[lbl] <- unname(palette[region_name])
    }
  }
  if (length(new_palette) == 0) NULL else new_palette
}


#' Flatten ggseg3d_atlas to data frame
#' @keywords internal
flatten_ggseg3d_atlas <- function(atlas_3d, surface = "inflated") {
  atlas_filtered <- atlas_3d[atlas_3d$surf == surface, ]

  if (nrow(atlas_filtered) == 0) {
    available <- unique(atlas_3d$surf) # nolint: object_usage_linter
    cli::cli_abort(c(
      "No data found for surface {.val {surface}}.",
      "i" = "Available surfaces: {.val {available}}"
    ))
  }

  result <- do.call(rbind, lapply(seq_len(nrow(atlas_filtered)), function(i) {
    row <- atlas_filtered[i, ]
    ggseg_3d <- row$ggseg_3d[[1]]
    ggseg_3d$hemi <- row$hemi
    ggseg_3d$surf <- row$surf
    ggseg_3d
  }))

  result
}


#' Compute vertex indices by matching to brain mesh
#' @keywords internal
compute_vertex_indices <- function(
    atlas_3d_flat,
    surface = "inflated",
    subject = "fsaverage5",
    tolerance = 1e-4) {
  hemi_map <- c("left" = "lh", "right" = "rh")
  vertices_list <- list()
  total_matched <- 0
  total_vertices <- 0

  for (hemi in c("left", "right")) {
    hemi_short <- hemi_map[hemi]

    brain_mesh <- tryCatch(
      get_brain_mesh(
        hemisphere = hemi_short,
        surface = surface,
        subject = subject
      ),
      error = function(e) NULL
    )

    if (is.null(brain_mesh)) {
      cli::cli_warn(
        "Could not load brain mesh for {.val {hemi_short}} {.val {surface}}."
      )
      next
    }

    brain_coords <- as.matrix(brain_mesh$vertices)
    hemi_data <- atlas_3d_flat[atlas_3d_flat$hemi == hemi, ]

    for (i in seq_len(nrow(hemi_data))) {
      label <- hemi_data$label[i]
      region_mesh <- hemi_data$mesh[[i]]

      if (is.null(region_mesh) || is.null(region_mesh$vertices)) {
        vertices_list[[label]] <- integer(0)
        next
      }

      region_coords <- as.matrix(region_mesh$vertices)
      total_vertices <- total_vertices + nrow(region_coords)
      matched_indices <- match_vertices(region_coords, brain_coords, tolerance)
      total_matched <- total_matched + length(matched_indices)
      vertices_list[[label]] <- matched_indices
    }
  }

  if (total_vertices > 0 && total_matched == 0) {
    atlas_surf <- # nolint: object_usage_linter
      unique(atlas_3d_flat$surf)[1]
    cli::cli_warn(c(
      "No vertices matched between atlas and brain mesh.",
      "i" = "Atlas surface: {.val {atlas_surf}}, target: {.val {surface}}.",
      "i" = "Ensure the 3D atlas surface matches the FreeSurfer surface.",
      "i" = "Try increasing {.arg tolerance} if coordinates are similar."
    ))
  } else if (total_matched < total_vertices * 0.5) {
    pct <- # nolint: object_usage_linter
      round(total_matched / total_vertices * 100)
    cli::cli_warn("Only {pct}% of vertices matched.")
  }

  vertices_list
}


#' Match region vertices to brain mesh vertices
#' @keywords internal
match_vertices <- function(region_coords, brain_coords, tolerance = 1e-4) {
  matched <- integer(nrow(region_coords))
  n_matched <- 0L

  for (i in seq_len(nrow(region_coords))) {
    dists <- sqrt(
      (brain_coords[, 1] - region_coords[i, 1])^2 +
        (brain_coords[, 2] - region_coords[i, 2])^2 +
        (brain_coords[, 3] - region_coords[i, 3])^2
    )

    min_idx <- which.min(dists)
    if (dists[min_idx] <= tolerance) {
      n_matched <- n_matched + 1L
      matched[n_matched] <- min_idx - 1L
    }
  }

  unique(matched[seq_len(n_matched)])
}


#' Infer vertex indices by matching mesh coordinates to brain surface
#'
#' @description
#' Hash-based O(n+m) matching that rounds coordinates to 4 decimal places
#' and uses named vector lookup. Interpolated triangulation vertices that
#' don't match exactly are silently skipped.
#'
#' @param atlas_3d A `ggseg3d_atlas` with mesh data.
#' @param surface Which surface to match against (default `"inflated"`).
#' @param brain_meshes Brain mesh data to match against. If NULL, attempts
#'   to load from ggseg3d package.
#' @return Named list of integer vertex indices (0-based) keyed by label,
#'   or NULL if brain_meshes unavailable.
#' @keywords internal
infer_vertices_from_meshes <- function(atlas_3d, surface = "inflated",
                                       brain_meshes = NULL) {
  if (is.null(brain_meshes)) {
    brain_meshes <- tryCatch(
      get("brain_meshes", envir = asNamespace("ggseg3d")),
      error = function(e) NULL
    )
  }
  if (is.null(brain_meshes)) return(NULL)

  hemi_map <- c("left" = "lh", "right" = "rh")
  vertices_list <- list()

  for (hemi in c("left", "right")) {
    hemi_short <- hemi_map[hemi]
    mesh_name <- paste0(hemi_short, "_", surface)
    brain_mesh <- brain_meshes[[mesh_name]]
    if (is.null(brain_mesh)) next

    brain_coords <- as.matrix(brain_mesh$vertices)
    brain_keys <- paste(
      round(brain_coords[, 1], 4),
      round(brain_coords[, 2], 4),
      round(brain_coords[, 3], 4)
    )
    brain_index <- stats::setNames(seq_len(nrow(brain_coords)) - 1L, brain_keys)

    row_idx <- which(atlas_3d$hemi == hemi & atlas_3d$surf == surface)
    if (length(row_idx) == 0) next

    ggseg <- atlas_3d$ggseg_3d[[row_idx]]
    if (!"mesh" %in% names(ggseg)) next

    for (i in seq_len(nrow(ggseg))) {
      m <- ggseg$mesh[[i]]
      if (is.null(m)) next

      if ("vb" %in% names(m)) {
        region_coords <- cbind(m$vb[1, ], m$vb[2, ], m$vb[3, ])
      } else if ("vertices" %in% names(m) && !is.null(m$vertices)) {
        region_coords <- as.matrix(m$vertices)
      } else {
        next
      }

      region_keys <- paste(
        round(region_coords[, 1], 4),
        round(region_coords[, 2], 4),
        round(region_coords[, 3], 4)
      )
      matched <- brain_index[region_keys]
      matched <- unique(unname(matched[!is.na(matched)]))
      if (length(matched) > 0) {
        vertices_list[[ggseg$label[i]]] <- as.integer(matched)
      }
    }
  }

  if (length(vertices_list) == 0) NULL else vertices_list
}
