## 2d polygons ----

#' JHU parcellation
#'
#' @docType data
#' @name jhu
#' @keywords datasets
#' @family ggseg_atlases
#' @references Hua et al. (2008) NeuroImage, 39(1):336-347 
#' (\href{https://www.ncbi.nlm.nih.gov/pmc/articles/PMC2724595/}{PubMed})
#'
#' @format A data.frame with 11341 observations and 11 variables
#' \describe{
#'   \item{long}{coordinates for the x-axis}
#'   \item{lat}{coordinates for the y-axis}
#'   \item{area}{name of region}
#'   \item{hemi}{dummy name of the hemisphere}
#'   \item{side}{which side to view (sagittal)}
#'   \item{label}{label name from Freesurfer}
#'   \item{atlas}{name of the atlas}
#' }
#'
#' @examples
#' data(jhu)
"jhu"

### 3d meshes ----
#' Parcellation from JHU
#'
#' @docType data
#' @name jhu_3d
#' @keywords datasets
#' @family ggseg3d_atlases
#' @references Hua et al., Tract probability maps in stereotaxic spaces:
#' analysis of white matter anatomy and tract-specific quantification. NeuroImage, 39(1):336-347 (2008)
#' (\href{https://www.ncbi.nlm.nih.gov/pmc/articles/PMC2724595/}{PubMed})
#'
#'
#' @examples
#' data(jhu_3d)
"jhu_3d"
