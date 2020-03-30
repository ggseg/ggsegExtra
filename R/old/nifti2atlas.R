#' Convert nifti file with labels to atlas
#'
#' @param nifti nifti file
#' @param outdir output directory (will be created)
#' @param annot_lab annotation dataframe
#' @param atlas_name name to give atlas
#' @param dilation dilation factor
#' @param eroding erosion factor
#' @param smoothing smoothing factor
#' @param steps which steps to run 1:5
#' @param verbose verbose or not
#'
#' @details The steps in this function are:
#' \itemize{
#'  \item{step 1 - }{\code{atlas_vol2surf} -  converts volume to surface}
#'  \item{step 2 - }{\code{atlas_vol2label} - reads labels from volume}
#'  \item{step 3 - }{\code{atlas_lab2ctab} - creates ctab from labels}
#'  \item{step 4 - }{\code{atlas_tcl} - runs tkSurfer tcl script}
#'  \item{step 5 - }{\code{atlas_isolate} - isolates coloured regions from images}
#' }
#' 
#' @return ggseg-atlas
#' @export
nifti_2_atlas <- function(nifti, outdir, annot_lab, atlas_name, 
                          dilation = 2, eroding = 2, smoothing = 2,
                          steps = 1:5, 
                          verbose = TRUE){

  if(!dir.exists(outdir)) dir.create(outdir)
  
  if(1 %in% steps){
    if(verbose) cat("Runnin step 1\n")
    atlas_vol2surf(infile, outdir, projfrac, verbose)
  }
  
  
  # labels ----
  if(2 %in% steps){
    if(verbose) cat("Runnin step 2\n")
    k <- atlas_vol2label(annot_lab, outdir, verbose)
  }
  
  # ctab ----
  if(3 %in% steps){
    if(verbose) cat("Runnin step 3\n")
    atlas_lab2ctab(outdir, verbose)
  }
  
  # tcl ----
  if(4 %in% steps){
    if(verbose) cat("Runnin step 4\n")
    atlas_tcl(annot_lab, outdir, verbose)
  }
  
  # isolate colour ----
  if(5 %in% steps){
    if(verbose) cat("Runnin step 5\n")
    atlas_isolate(outdir, dilation, eroding, smoothing, verbose)
  }
  
  # raster ----
 

  # combine to make df
  atlas_df <- atlas_raster2sf(rasterobjs)
  
  atlas_df_gg <- atlas_sf2gg(atlas_df, atlas_name)
  
  save_atlas(atlas_df_gg, atlas_name, outdir, verbose)
  
  invisible(atlas_df_gg)
}
