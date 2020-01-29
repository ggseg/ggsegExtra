nifti_2_atlas <- function(nifti, outdir, annot_lab, atlas_name, 
                          dilation = 2, eroding = 2, smoothing = 2,
                          verbose = TRUE){

  if(!dir.exists(outdir)) dir.create(outdir)
  
  atlas_vol2surf(infile, outdir, projfrac, verbose)
  
  # labels ----
  k <- atlas_vol2label(annot_lab, outdir, verbose)
  
  # ctab ----
  atlas_lab2ctab(outdir, verbose)
  
  # tcl ----
  atlas_tcl(annot_lab, outdir, verbose)
  
  # isolate colour ----
  atlas_isolate(outdir, dilation, eroding, smoothing, verbose)
  
  # raster ----
  rasterobjs <- atlas_raster(outdir)
  
  # combine to make df
  atlas_df <- atlas_raster2sf(rasterobjs)
  
  atlas_df_gg <- atlas_sf2gg(atlas_df, atlas_name)
  
  save_atlas(atlas_df_gg, atlas_name, outdir, verbose)
  
  invisible(atlas_df_gg)
}
