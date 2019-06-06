
## 2d polygons ----

#' Yeo 7 Resting-state Cortical Parcellations
#'
#' Coordinate data for the resting-state networks of
#' the Yeo 2011 7 networks.
#'
#' @docType data
#' @name yeo7
#' @usage data(yeo7)
#' @family ggseg_atlases
#'
#' @keywords datasets
#'
#' @references Yeo et al. (2011) J. Neurophysiology 16(3):1125-1165
#' (\href{https://www.ncbi.nlm.nih.gov/pubmed/21653723}{PubMed})
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
#' data(yeo7)
"yeo7"



#' Yeo 17 Resting-state Cortical Parcellations
#'
#' Coordinate data for the resting-state networks of
#' the Yeo 2011 17 networks.
#'
#' @docType data
#' @name yeo17
#' @usage data(yeo17)
#' @family ggseg_atlases
#'
#' @keywords datasets
#'
#' @references Yeo et al. (2011) J. Neurophysiology 16(3):1125-1165
#' (\href{https://www.ncbi.nlm.nih.gov/pubmed/21653723}{PubMed})
#'
#' @format A data.frame with 8203 observations and 10 variables
#' \describe{
#'   \item{long}{coordinates for the x-axis}
#'   \item{lat}{coordinates for the y-axis}
#'   \item{area}{name of network}
#'   \item{hemi}{name of the hemisphere (left, right)}
#'   \item{side}{which side to view (medial, lateral)}
#'   \item{network}{network number (1:17)}
#'   \item{label}{unique name to each node}
#'   \item{atlas}{name of the atlas}
#' }
#'
#' @examples
#' data(yeo17)
"yeo17"



### 3d meshes ----
#' Yeo 7 Resting-state Cortical Parcellations
#'
#' Mesh data for the resting-state networks of
#' the Yeo 2011 7 networks.
#'
#' @docType data
#' @name yeo7_3d
#' @usage data(yeo7_3d)
#'
#' @keywords datasets
#' @family ggseg3d_atlases
#'
#' @references Yeo et al. (2011) J. Neurophysiology 16(3):1125-1165
#' (\href{https://www.ncbi.nlm.nih.gov/pubmed/21653723}{PubMed})
#'
#' @format A tibble with 4 observations and a nested data.frame
#' \describe{
#'   \item{surf}{type of surface (`inflated` or `white`)}
#'   \item{hemi}{hemisphere (`left`` or `right`)}
#'   \item{data}{data.frame of necessary variables for plotting
#'   }
#'
#'   \item{atlas}{String. atlas name}
#'   \item{roi}{numbered region from surface}
#'   \item{annot}{concatenated region name}
#'   \item{label}{label `hemi_annot` of the region}
#'   \item{mesh}{list of meshes in two lists: vb and it}
#'   \item{area}{name of area in full}
#'   \item{colour}{HEX colour of region}
#' }
#' @examples
#' data(yeo7_3d)
"yeo7_3d"


#' Yeo 17 Resting-state Cortical Parcellations
#'
#' Mesh data for the resting-state networks of
#' the Yeo 2011 17 networks.
#'
#' @docType data
#' @name yeo17_3d
#' @usage data(yeo17_3d)
#' @family ggseg3d_atlases
#' @keywords datasets
#'
#' @references Yeo et al. (2011) J. Neurophysiology 16(3):1125-1165
#' (\href{https://www.ncbi.nlm.nih.gov/pubmed/21653723}{PubMed})
#'
#' @format A tibble with 4 observations and a nested data.frame
#' \describe{
#'   \item{surf}{type of surface (`inflated` or `white`)}
#'   \item{hemi}{hemisphere (`left`` or `right`)}
#'   \item{data}{data.frame of necessary variables for plotting
#'   }
#'
#'   \item{atlas}{String. atlas name}
#'   \item{roi}{numbered region from surface}
#'   \item{annot}{concatenated region name}
#'   \item{label}{label `hemi_annot` of the region}
#'   \item{mesh}{list of meshes in two lists: vb and it}
#'   \item{area}{name of area in full}
#'   \item{colour}{HEX colour of region}
#' }
#' @examples
#' data(yeo7_3d)
"yeo17_3d"
