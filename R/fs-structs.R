# List of surfaces & curvatures ----
#' Available Freesurfer surfaces
#'
#' @return character
#' @export
#'
#' @examples
#' fs_surfaces()
fs_surfaces <- function() {
  c(
    "inflated",
    "inflated.nofix",
    "orig",
    "orig.nofix",
    "pial",
    "qsphere.nofix",
    "smoothwm",
    "smoothwm.nofix",
    "sphere",
    "sphere.reg",
    "white",
    "LCBC"
  )
}

#' Available Freesurfer curvatures
#'
#' @return character
#' @export
#'
#' @examples
#' fs_curvatures()
fs_curvatures <- function() {
  c(
    "area",
    "area.mid",
    "area.pial",
    "avg_curv",
    "curv",
    "curv.pial",
    "inflated.H",
    "inflated.K",
    "jacobian_white",
    "smoothwm.BE.crv",
    "smoothwm.C.crv",
    "smoothwm.FI.crv",
    "smoothwm.H.crv",
    "smoothwm.K1.crv",
    "smoothwm.K2.crv",
    "smoothwm.K.crv",
    "smoothwm.S.crv",
    "sulc",
    "thickness",
    "volume",
    "w-g.pct.mgh"
  )
}

#' Available Freesurfer no fix curvatures
#'
#' @return character vector of no-fix curvature file names
#' @export
#'
#' @examples
#' fs_nofixcurvatures()
fs_nofixcurvatures <- function() {
  c("defect_borders", "defect_chull", "defect_labels")
}


#' @describeIn fs_nofixcurvatures Deprecated. Use [fs_nofixcurvatures()] instead.
#' @export
fs_nofixcurv <- function() {
  lifecycle::deprecate_warn("2.0.0", "fs_nofixcurv()", "fs_nofixcurvatures()")
  fs_nofixcurvatures()
}
