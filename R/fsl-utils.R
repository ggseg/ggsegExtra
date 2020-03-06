
#' Extract colours for FSL atlas
#'
#' @param atlas string name of atlas
#' @param path path to save annotation file
#'
#' @return data.frame
#' @export
get_fsl_colour <- function(atlas = "HarvardOxford-Cortical",
                           path = NULL){
  
  atlas_xml <- list.files(paste0(fsl_dir(), "/data/atlases/"), 
                          paste0(atlas, ".xml"), full.names = TRUE)
  atlas_xml <- xml2::read_xml(atlas_xml)
  
  atlas_list <- xml2::as_list(atlas_xml)
  
  labels <- purrr::map_chr(atlas_list$atlas$data, 1)
  labels <- unname(labels)
  labels <- c("unknown", labels)
  
  atlas_df <- dplyr::tibble(
    idx = 0:(length(labels)-1), 
    labels = labels
  )
  
  suppressWarnings(
    FS <- readr::read_table2(
      file.path(freesurfer::fs_dir(), 
                "FreeSurferColorLUT.txt"), 
      skip = 4, 
      col_names = c("idx", "label", "R", "G", "B", "A"), 
      col_types = readr::cols())
  )
  
  suppressWarnings(
    FS <- dplyr::mutate_at(FS, dplyr::vars(R,G,B,A), as.numeric)
  )
  
  FS <- dplyr::filter_at(FS, dplyr::vars(R,G,B,A), not_na)
  FS <- dplyr::mutate(FS, 
                      hex = grDevices::rgb(R, G, B, A, maxColorValue = 255))
  
  ## choose 48 rows from here for colour
  start <- which.max(grepl("Unknown", FS$label))
  
  colours <- dplyr::slice(FS, start:(start+nrow(atlas_df)-1))
  
  atlas_df <- dplyr::bind_cols(atlas_df, dplyr::select(colours, -1:-2))
  
  atlas_df <- dplyr::mutate(atlas_df, 
                            labels = janitor::make_clean_names(labels))
  
  if(!is.null(path)) write_ctab(dplyr::select(atlas_df, -hex),
                                path)
  
  ## Want the colours to be distinct - do any editing here.
  ## HLS transform could be an option.
  atlas_df
}

fsl_dir <- function (){
  fsldir = Sys.getenv("FSLDIR")
  if (fsldir == "") {
    x = fslr::get.fsl
    fsldir = getOption("fsl.path")
  }
  return(fsldir)
}

not_na <- function(x){!is.na(x)}

if(getRversion() >= "2.15.1"){
  utils::globalVariables(c("R","G","B","A",
                           "hex", "rgb"))
}
