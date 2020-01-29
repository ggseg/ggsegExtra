
#' Extract colours for FSL atlas
#'
#' @param atlas string name of atlas
#'
#' @return data.frame
#' @export
#' @importFrom dplyr tibble mutate filter slice bind_cols
#' @importFrom freesurfer fs_dir
#' @importFrom janitor make_clean_names
#' @importFrom purrr map_chr
#' @importFrom readr read_table2 cols
#' @importFrom xml2 read_xml as_list
get_fsl_colour <- function(atlas = "HarvardOxford-Cortical",
                           path = NULL){
  
  atlas_xml <- list.files(paste0(fslr:::fsldir(), "/data/atlases/"), 
                          paste0(atlas, ".xml"), full.names = TRUE)
  atlas_xml <- xml2::read_xml(atlas_xml)
  
  atlas_list <- xml2::as_list(atlas_xml)
  
  labels <- purrr::map_chr(atlas_list$atlas$data, 1)
  labels <- unname(labels)
  labels <- c("unknown", labels)
  
  atlas_df <- dplyr::tibble(idx = 0:(length(labels)-1), 
                            labels = labels)
  
  FS <- readr::read_table2(
    file.path(freesurfer::fs_dir(), 
              "FreeSurferColorLUT.txt"), 
    skip = 4, 
    col_names = c("idx", "label", "R", "G", "B", "A"), 
    col_types = readr::cols())
  FS <- dplyr::mutate(FS, 
                      R = as.numeric(R),
                      A = 255)
  FS <- dplyr::filter(FS, !is.na(R))
  FS <- dplyr::mutate(FS, hex = rgb(R, G, B, A, maxColorValue = 255))
  
  ## choose 48 rows from here for colour
  start <- which.max(FS$label=="ctx-lh-Unknown")
  
  colours <- dplyr::slice(FS, start:(start+nrow(atlas_df)-1))
  
  atlas_df <- dplyr::bind_cols(atlas_df, select(colours, hex))
  
  atlas_df <- dplyr::mutate(atlas_df, 
                            labels = janitor::make_clean_names(labels))
  
  if(!is.null(path)) gdata::write.fwf(atlas_df,
                                        path, 
                                        col_names = FALSE)
  
  ## Want the colours to be distinct - do any editing here.
  ## HLS transform could be an option.
  atlas_df
}







