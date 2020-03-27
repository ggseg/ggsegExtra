# List of surfaces & curvatures ----
#' Available Freesurfer surfaces
#'
#' @return character
#' @export
#'
#' @examples
#' fs_surfaces()
fs_surfaces <- function(){
  c("inflated", "inflated.nofix", "orig", "orig.nofix", "pial", 
    "qsphere.nofix", "smoothwm", "smoothwm.nofix", 
    "sphere", "sphere.reg", "white", "LCBC")
}

#' Available Freesurfer curvatures
#'
#' @return character
#' @export
#'
#' @examples
#' fs_curvatures()
fs_curvatures <- function(){
  c("area", "area.mid", "area.pial", "avg_curv", "curv", "curv.pial",
    "inflated.H", "inflated.K", "jacobian_white", "smoothwm.BE.crv",
    "smoothwm.C.crv", "smoothwm.FI.crv", "smoothwm.H.crv", "smoothwm.K1.crv",
    "smoothwm.K2.crv", "smoothwm.K.crv", "smoothwm.S.crv", "sulc", "thickness", 
    "volume", "w-g.pct.mgh")
}

#' Available Freesurfer no fix curvatures
#'
#' @return character
#' @export
#'
#' @examples
#' fs_nofixcurv()
fs_nofixcurv <- function(){
  c("defect_borders", "defect_chull", "defect_labels")
}

surf_list <- function(hemispheres, surfs){
  k <- expand.grid(hemispheres, surfs)
  tidyr::unite(k, x, c("Var1", "Var2"), sep=".")$x
}
