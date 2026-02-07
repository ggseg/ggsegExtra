#' {GGSEG} Brain Atlas
#'
#' A brain atlas for the {GGSEG} parcellation scheme.
#'
#' This atlas contains 2D polygon geometry for [ggseg::ggseg()]
#' and vertex indices for 3D rendering with [ggseg3d::ggseg3d()].
#'
#' @format A [ggseg.formats::brain_atlas] object with columns:
#' \describe{
#'   \item{hemi}{Hemisphere: "left" or "right"}
#'   \item{region}{Region name}
#'   \item{label}{Unique region label (includes hemisphere prefix)}
#'   \item{colour}{Default colour for the region}
#'   \item{vertices}{Integer vector of vertex indices for 3D rendering}
#'   \item{geometry}{sf polygon geometry for 2D plotting}
#' }
#'
#' @section Atlas Citation:
#' If you use this atlas, please cite:
#'
#' TODO: Add the original publication reference here
#'
#' @seealso
#' * [ggseg::ggseg()] for 2D brain plots
#' * [ggseg3d::ggseg3d()] for 3D brain visualisation
#' * [ggseg.formats::brain_atlas] for atlas structure details
#'
#' @source
#' TODO: Add source URL or description
#'
#' @examples
#' # Load the atlas
#' data({GGSEG})
#'
#' # 2D plot
#' ggseg::ggseg(atlas = {GGSEG})
#'
#' # 3D plot (interactive)
#' if (interactive()) {
#'   ggseg3d::ggseg3d(atlas = {GGSEG})
#' }
#'
#' @name {GGSEG}
#' @docType data
#' @keywords datasets
NULL
