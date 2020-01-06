

### 3d meshes ----

#' Local-Global 7 Parcellation of the Human Cerebral Cortex
#'
#' Mesh data for the 7 resting-state networks of
#' the Schaefer et al. (2018) networks.
#'
#' @docType data
#' @name schaefer7_3d
#' @usage data(schaefer7_3d)
#' @family ggseg3d_atlases
#' @keywords datasets
#'
#'
#' @references Schaefer et al. (2018) Cereb Cortex. 2018 Sep 1;28(9):3095-3114. doi: 10.1093/cercor/bhx179
#' (\href{https://www.ncbi.nlm.nih.gov/pubmed/28981612}{PubMed})
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
#' data(schaefer7_3d)
"schaefer7_3d"



#' Local-Global 17 Parcellation of the Human Cerebral Cortex
#'
#' Mesh data for the 17 resting-state networks of
#' the Schaefer et al. (2018) networks.
#'
#' @docType data
#' @name schaefer17_3d
#' @usage data(schaefer17_3d)
#' @family ggseg3d_atlases
#' @keywords datasets
#'
#' @references Schaefer et al. (2018) Cereb Cortex. 2018 Sep 1;28(9):3095-3114. doi: 10.1093/cercor/bhx179
#' (\href{https://www.ncbi.nlm.nih.gov/pubmed/28981612}{PubMed})
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
#' data(schaefer17_3d)
"schaefer17_3d"
