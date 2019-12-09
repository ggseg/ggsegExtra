## 2d polygons ----

#' HPC - Multi-modal parcellation of human cerebral cortex
#'
#' @docType data
#' @name glasser
#' @keywords datasets
#' @family ggseg_atlases
#' @references Glasser et al. (2016) Nature, volume 536, pages 171–178
#' (\href{https://www.nature.com/articles/nature18933}{PubMed})
#'
#' @format A data.frame with 2483 observations and 8 variables
#' \describe{
#'   \item{long}{coordinates for the x-axis}
#'   \item{lat}{coordinates for the y-axis}
#'   \item{area}{name of region}
#'   \item{hemi}{dummy name of the hemisphere}
#'   \item{side}{which side to view (medial, lateral)}
#'   \item{label}{label name from Freesurfer}
#'   \item{atlas}{name of the atlas}
#' }
#'
#' @examples
#' data(glasser)
"glasser"


### 3d meshes ----

#' Parcellation from the Human Connectome Project
#'
#' @docType data
#' @name glasser_3d
#' @keywords datasets
#' @family ggseg3d_atlases
#' @references Glasser et al. (2016) Nature, volume 536, pages 171–178
#' (\href{https://www.nature.com/articles/nature18933}{PubMed})
#'
#' @format A data.frame with 2483 observations and 8 variables
#' \describe{
#'   \item{long}{coordinates for the x-axis}
#'   \item{lat}{coordinates for the y-axis}
#'   \item{area}{name of region}
#'   \item{hemi}{dummy name of the hemisphere}
#'   \item{side}{which side to view (medial, lateral)}
#'   \item{label}{label name from Freesurfer}
#'   \item{atlas}{name of the atlas}
#' }
#'
#' @examples
#' data(glasser_3d)
"glasser_3d"
