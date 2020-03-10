### 3d meshes ----

#' Freesurfer ASEG with posterior-anterior hippocampus
#'
#' Coordinate data for the subcortical parcellations implemented
#' in Freesurfer, with a division of the hippocampus in posterior
#' and anterior segments.
#'
#' @docType data
#' @name hcpa_3d
#' @usage data(hcpa_3d)
#' @family ggseg_atlases
#'
#' @keywords datasets
#'
#' @references Fischl et al., (2002). Neuron, 33:341-355
#' (\href{https://www.ncbi.nlm.nih.gov/pubmed/11832223}{PubMed})
#'
#' @format A data.frame with 2702 observations and 9 variables
#' \describe{
#'   \item{lat}{coordinates for the x-axis}
#'   \item{long}{coordinates for the y-axis}
#'   \item{area}{acronym of network}
#'   \item{name}{full name of network}
#'   \item{hemi}{name of the hemisphere (left, right)}
#'   \item{side}{which side to view (medial, lateral,axial)}
#'   \item{atlas}{name of the atlas}
#' }
#'
#' @examples
#' data(hcpa_3d)
"hcpa_3d"



