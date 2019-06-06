## 2d polygons ----

#' White matter tract parcellations
#'
#' @docType data
#' @name tracula
#' @keywords datasets
#'
#' @references Yendiki et al. (2011) Automated probabilistic reconstruction of 
#' white-matter pathways in health and disease using an atlas of the underlying anatomy.  
#' Front. Neuroinform. 5:23. doi: 10.3389/fninf.2011.00023
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
#' data(tracula)
"tracula"


### 3d meshes ----
#' White matter tract parcellations
#'
#' @docType data
#' @name tracula_3d
#' @keywords datasets
#'
#' @references Yendiki et al. (2011) Automated probabilistic reconstruction of 
#' white-matter pathways in health and disease using an atlas of the underlying anatomy.  
#' Front. Neuroinform. 5:23. doi: 10.3389/fninf.2011.00023
#' (\href{https://www.ncbi.nlm.nih.gov/pmc/articles/PMC2724595/}{PubMed})
#'
#'
#' @examples
#' data(tracula_3d)
"tracula_3d"
