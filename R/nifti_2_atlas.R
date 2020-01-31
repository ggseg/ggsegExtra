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
#' @return ggseg-atlas
#' @export
nifti_2_atlas <- function(nifti, outdir, annot_lab, atlas_name, 
                          dilation = 2, eroding = 2, smoothing = 2,
                          steps = 1:5, 
                          verbose = TRUE){

  if(!dir.exists(outdir)) dir.create(outdir)
  
  if(1 %in% steps){
    atlas_vol2surf(infile, outdir, projfrac, verbose)
  }
  
  
  # labels ----
  if(2 %in% steps){
    k <- atlas_vol2label(annot_lab, outdir, verbose)
  }
  
  # ctab ----
  if(3 %in% steps){
    atlas_lab2ctab(outdir, verbose)
  }
  
  # tcl ----
  if(4 %in% steps){
    atlas_tcl(annot_lab, outdir, verbose)
  }
  
  # isolate colour ----
  if(5 %in% steps){
    atlas_isolate(outdir, dilation, eroding, smoothing, verbose)
  }
  
  # raster ----
  rasterobjs <- atlas_raster(outdir)
  
  # combine to make df
  atlas_df <- atlas_raster2sf(rasterobjs)
  
  atlas_df_gg <- atlas_sf2gg(atlas_df, atlas_name)
  
  save_atlas(atlas_df_gg, atlas_name, outdir, verbose)
  
  invisible(atlas_df_gg)
}
