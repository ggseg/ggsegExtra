
## 2d polygons ----

#' Harvard-Oxford Cortical atlas
#'
#' Coordinate data for the parcellations in the
#' Harvard-Oxford cortical atlas.
#'
#' @docType data
#' @name hoCort
#' @usage data(hoCort)
#' @family ggseg_atlases
#'
#' @keywords datasets
#'
#' @references Makris,et al. (2006) Schizophrenia research 83(2-3):155-151
#' (\href{https://doi.org/10.1016/j.schres.2005.11.020}{PubMed})
#'
#' @format A data.frame with 8203 observations and 10 variables
#' \describe{
#'   \item{long}{coordinates for the x-axis}
#'   \item{lat}{coordinates for the y-axis}
#'   \item{area}{name of network}
#'   \item{hemi}{name of the hemisphere (left, right)}
#'   \item{side}{which side to view (medial, lateral)}
#'   \item{network}{network number (1:7)}
#'   \item{label}{unique name to each node}
#'   \item{atlas}{name of the atlas}
#' }
#'
#' @examples
#' data(hoCort)
"hoCort"


### 3d meshes ----
