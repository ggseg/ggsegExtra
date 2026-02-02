#' Create unified brain atlas
#'
#' Converts existing 2D and/or 3D atlases into a unified brain_atlas format
#' that supports both ggseg (2D) and ggseg3d (3D) rendering.
#'
#' The function handles three scenarios:
#' \itemize{
#'   \item Both atlases provided: Combines geometry from 2D and vertex indices from 3D
#'   \item Only 2D atlas: Keeps geometry, vertices will be empty (2D rendering only)
#'   \item Only 3D atlas: Computes vertex indices, geometry will be NULL (3D rendering only)
#' }
#'
#' @section Surface matching:
#' When a 3D atlas is provided, vertex indices are computed by matching the
#' atlas mesh coordinates to a FreeSurfer brain mesh. This requires:
#' \itemize{
#'   \item The `surface` parameter must match a surface in the 3D atlas
#'   \item The 3D atlas must use standard FreeSurfer coordinates (not transformed)
#'   \item FreeSurfer must be installed and configured
#' }
#'
#' @param atlas_2d A brain_atlas object with geometry column (from ggseg/ggseg.formats),
#'   or NULL if only 3D atlas is available
#' @param atlas_3d A ggseg3d_atlas object with mesh data, or NULL if only 2D atlas
#'   is available
#' @param atlas_name Name for the unified atlas. If NULL, derived from input atlas.
#' @param type Atlas type: "cortical" or "subcortical". If NULL, derived from input.
#' @param surface Surface type in the 3D atlas to use (must match exactly, e.g. "inflated")
#' @param subject FreeSurfer subject (default "fsaverage5")
#' @param tolerance Coordinate matching tolerance (default 1e-4). Increase if
#'   coordinates have minor floating-point differences.
#'
#' @return A unified brain_atlas with geometry and/or vertices columns
#' @export
#' @importFrom dplyr case_when
#' @importFrom rlang %||%
#'
#' @examples
#' \dontrun{
#' # From both 2D and 3D atlases
#' unified <- unify_atlas(ggseg::dk, ggseg3d::dk_3d)
#'
#' # From 3D atlas only
#' unified <- unify_atlas(atlas_3d = dk_3d)
#'
#' # From 2D atlas only (no 3D rendering)
#' unified <- unify_atlas(atlas_2d = ggseg::dk)
#' }
unify_atlas <- function(
    atlas_2d = NULL,
    atlas_3d = NULL,
    atlas_name = NULL,
    type = NULL,
    surface = "inflated",
    subject = "fsaverage5",
    tolerance = 1e-4
) {
  has_2d <- !is.null(atlas_2d)
  has_3d <- !is.null(atlas_3d)

 if (!has_2d && !has_3d) {
    cli::cli_abort("At least one of {.arg atlas_2d} or {.arg atlas_3d} must be provided.")
  }

  if (has_2d && !inherits(atlas_2d, "brain_atlas")) {
    cli::cli_abort("{.arg atlas_2d} must be a {.cls brain_atlas} object.")
  }

  if (has_3d && !inherits(atlas_3d, "ggseg3d_atlas")) {
    cli::cli_abort("{.arg atlas_3d} must be a {.cls ggseg3d_atlas} object.")
  }

  mode <- dplyr::case_when(
    has_2d && has_3d ~ "both",
    has_2d ~ "2d_only",
    has_3d ~ "3d_only"
  )

  switch(mode,
    "both" = unify_from_both(
      atlas_2d, atlas_3d, atlas_name, type, surface, subject, tolerance
    ),
    "2d_only" = unify_from_2d(atlas_2d, atlas_name, type),
    "3d_only" = unify_from_3d(
      atlas_3d, atlas_name, type, surface, subject, tolerance
    )
  )
}


#' @keywords internal
unify_from_both <- function(
    atlas_2d,
    atlas_3d,
    atlas_name,
    type,
    surface,
    subject,
    tolerance
) {
  if ("vertices" %in% names(atlas_2d$data)) {
    cli::cli_warn("{.arg atlas_2d} already has a vertices column. It will be replaced.")
  }

  atlas_name <- atlas_name %||% atlas_2d$atlas
  type <- type %||% atlas_2d$type

  atlas_3d_flat <- flatten_ggseg3d_atlas(atlas_3d, surface = surface)

  vertices_list <- compute_vertex_indices(
    atlas_3d_flat,
    surface = surface,
    subject = subject,
    tolerance = tolerance
  )

  atlas_data <- atlas_2d$data
  atlas_data$vertices <- vector("list", nrow(atlas_data))

  for (i in seq_len(nrow(atlas_data))) {
    label <- atlas_data$label[i]
    if (!is.na(label) && label %in% names(vertices_list)) {
      atlas_data$vertices[[i]] <- vertices_list[[label]]
    } else {
      atlas_data$vertices[[i]] <- integer(0)
    }
  }

  if (!"colour" %in% names(atlas_data) && "colour" %in% names(atlas_3d_flat)) {
    colour_map <- stats::setNames(atlas_3d_flat$colour, atlas_3d_flat$label)
    atlas_data$colour <- unname(colour_map[atlas_data$label])
  }

  ggseg.formats::brain_atlas(atlas_name, type, atlas_data)
}


#' @keywords internal
unify_from_2d <- function(atlas_2d, atlas_name, type) {
  atlas_name <- atlas_name %||% atlas_2d$atlas
  type <- type %||% atlas_2d$type

  atlas_data <- atlas_2d$data

  if (!"vertices" %in% names(atlas_data)) {
    atlas_data$vertices <- vector("list", nrow(atlas_data))
    for (i in seq_len(nrow(atlas_data))) {
      atlas_data$vertices[[i]] <- integer(0)
    }
  }

  cli::cli_inform(c(
    "i" = "Created unified atlas from 2D only.",
    "i" = "3D rendering will not be available without vertex data."
  ))

  ggseg.formats::brain_atlas(atlas_name, type, atlas_data)
}


#' @keywords internal
unify_from_3d <- function(
    atlas_3d,
    atlas_name,
    type,
    surface,
    subject,
    tolerance
) {
  atlas_name <- atlas_name %||% gsub("_3d$", "", atlas_3d$atlas[1])
  type <- type %||% "cortical"

  atlas_3d_flat <- flatten_ggseg3d_atlas(atlas_3d, surface = surface)

  vertices_list <- compute_vertex_indices(
    atlas_3d_flat,
    surface = surface,
    subject = subject,
    tolerance = tolerance
  )

  unique_regions <- unique(atlas_3d_flat[, c("hemi", "region", "label", "colour")])

  atlas_data <- data.frame(
    hemi = unique_regions$hemi,
    region = unique_regions$region,
    label = unique_regions$label,
    colour = unique_regions$colour,
    stringsAsFactors = FALSE
  )

  atlas_data$vertices <- vector("list", nrow(atlas_data))

  for (i in seq_len(nrow(atlas_data))) {
    label <- atlas_data$label[i]
    if (!is.na(label) && label %in% names(vertices_list)) {
      atlas_data$vertices[[i]] <- vertices_list[[label]]
    } else {
      atlas_data$vertices[[i]] <- integer(0)
    }
  }

  cli::cli_inform(c(
    "i" = "Created unified atlas from 3D only.",
    "i" = "2D rendering will not be available without geometry data."
  ))

  ggseg.formats::brain_atlas(atlas_name, type, atlas_data)
}


#' Flatten ggseg3d_atlas to data frame
#' @keywords internal
flatten_ggseg3d_atlas <- function(atlas_3d, surface = "inflated") {
  atlas_filtered <- atlas_3d[atlas_3d$surf == surface, ]

  if (nrow(atlas_filtered) == 0) {
    available <- unique(atlas_3d$surf)
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
    tolerance = 1e-4
) {
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
        "Could not load brain mesh for {.val {hemi_short}} {.val {surface}}. Skipping."
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
    atlas_surf <- unique(atlas_3d_flat$surf)[1]
    cli::cli_warn(c(
      "No vertices matched between atlas and brain mesh.",
      "i" = "Atlas uses surface {.val {atlas_surf}}, matching against {.val {surface}}.",
      "i" = "Ensure the 3D atlas surface matches the FreeSurfer surface.",
      "i" = "Try increasing {.arg tolerance} if coordinates are similar but not exact."
    ))
  } else if (total_matched < total_vertices * 0.5) {
    pct <- round(total_matched / total_vertices * 100)
    cli::cli_warn("Only {pct}% of vertices matched. Check surface compatibility.")
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
