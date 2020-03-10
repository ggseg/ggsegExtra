#' Desikan-Killiany Cortical Atlas
#'
#' Coordinate data for the Desikan-Killany Cortical atlas,
#' with 40 regions in on the cortical surface of the brain.
#'
#' @docType data
#' @name dkextra
#' @usage data(dkextra)
#' @keywords datasets
#' @family ggseg_atlases
#'
#' @references Fischl et al. (2004) Cerebral Cortex 14:11-22
#' (\href{https://academic.oup.com/cercor/article/14/1/11/433466}{PubMed})
#'
#' @format A data.frame with 10913 observations and 11 variables
#' \describe{
#'   \item{long}{coordinates for the x-axis}
#'   \item{lat}{coordinates for the y-axis}
#'   \item{region}{name of region}
#'   \item{hemi}{name of the hemisphere (left, right)}
#'   \item{side}{which side to view (medial, lateral)}
#'   \item{acronym}{abbreviated name of region}
#'   \item{lobe}{lobe location}
#'   \item{label}{label from recon-all segmentation}
#'   \item{atlas}{name of the atlas}
#' }
#' @examples
#' data(dkextra)
"dkextra"
