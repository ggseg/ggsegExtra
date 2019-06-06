## 2d polygons ----

#' Genetic topography of brain thickness morphology
#'
#' @docType data
#' @name chenTh
#' @keywords datasets
#'
#' @references Chen et al. (2013) PNAS, 110 (42) 17089-17094; 
#' (\href{https://doi.org/10.1073/pnas.1308091110 }{PubMed})
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
#' data(chenTh)
"chenTh"

#' Genetic topography of brain area morphology
#'
#' @docType data
#' @name chenAr
#' @keywords datasets
#'
#' @references Chen et al. (2013) PNAS, 110 (42) 17089-17094; 
#' (\href{https://doi.org/10.1073/pnas.1308091110 }{PubMed})
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
#' data(chenAr)
"chenAr"

#' ### 3d meshes ----
#' #' Genetic topography of brain thickness morphology
#' #'
#' #' @docType data
#' #' @name chenTh_3d
#' #' @keywords datasets
#' #'
#' #' @references Chen et al. (2013) PNAS, 110 (42) 17089-17094;
#' #' (\href{https://doi.org/10.1073/pnas.1308091110 }{PubMed})
#' #'
#' #'
#' #' @examples
#' #' data(chenTh_3d)
#' "chenTh_3d"
#' 
#' #' Genetic topography of brain area morphology
#' #'
#' #' @docType data
#' #' @name chenAr_3d
#' #' @keywords datasets
#' #'
#' #' @references Chen et al. (2013) PNAS, 110 (42) 17089-17094;
#' #' (\href{https://doi.org/10.1073/pnas.1308091110 }{PubMed})
#' #'
#' #'
#' #' @examples
#' #' data(chenAr_3d)
#' "chenAr_3d"
