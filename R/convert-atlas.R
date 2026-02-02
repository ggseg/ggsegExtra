#' Convert legacy ggseg3d_atlas to brain_atlas
#'
#' Converts an existing ggseg3d_atlas object to the new unified brain_atlas
#' format. This allows using legacy atlases with the new vertex-based system.
#'
#' Note: Legacy atlases don't contain vertex indices, so 3D rendering will
#' fall back to the legacy mesh-based mode. For full vertex-based rendering,
#' recreate the atlas using [make_brain_atlas()].
#'
#' @param ggseg3d_atlas A ggseg3d_atlas object
#' @param ggseg_atlas Optional ggseg atlas (2D) to merge geometry from
#'
#' @return A brain_atlas object
#' @export
#' @importFrom dplyr select left_join
#' @importFrom ggseg.formats brain_atlas
#' @importFrom ggseg3d is_ggseg3d_atlas
#' @importFrom tidyr unnest
#'
#' @examples
#' \dontrun{
#' # Convert a legacy 3D atlas
#' new_atlas <- convert_ggseg3d_atlas(dk_3d)
#'
#' # Convert and merge with 2D geometry
#' new_atlas <- convert_ggseg3d_atlas(dk_3d, dk)
#' }
convert_ggseg3d_atlas <- function(ggseg3d_atlas, ggseg_atlas = NULL) {
  if (!ggseg3d::is_ggseg3d_atlas(ggseg3d_atlas)) {
    cli::cli_abort("Input must be a ggseg3d_atlas object")
  }

  atlas_name <- gsub("_3d$", "", unique(ggseg3d_atlas$atlas))

  dt <- unnest(ggseg3d_atlas, ggseg_3d)

  keep_cols <- c("hemi", "region", "label", "colour")
  if ("vertices" %in% names(dt)) {
    keep_cols <- c(keep_cols, "vertices")
  }

  atlas_data <- dt[, intersect(names(dt), keep_cols), drop = FALSE]

  if (!"vertices" %in% names(atlas_data)) {
    cli::cli_warn(c(

      "Legacy atlas does not contain vertex indices.",
      "i" = "3D rendering will use legacy mesh-based mode.",
      "i" = "For vertex-based rendering, recreate with {.fn make_brain_atlas}."
    ))
    atlas_data$vertices <- lapply(seq_len(nrow(atlas_data)), function(x) integer(0))
  }

  atlas_data <- unique(atlas_data)

  if (!is.null(ggseg_atlas)) {
    if (!inherits(ggseg_atlas, "brain_atlas")) {
      cli::cli_abort("{.arg ggseg_atlas} must be a brain_atlas object")
    }

    geom_data <- ggseg_atlas$data
    if ("geometry" %in% names(geom_data)) {
      geom_cols <- c("label", "side", "geometry")
      geom_data <- geom_data[, intersect(names(geom_data), geom_cols), drop = FALSE]
      atlas_data <- left_join(atlas_data, geom_data, by = "label")
    }
  }

  atlas_type <- if (any(ggseg3d_atlas$hemi == "subcort")) "subcortical" else "cortical"

  brain_atlas(
    atlas = atlas_name,
    type = atlas_type,
    data = atlas_data
  )
}
