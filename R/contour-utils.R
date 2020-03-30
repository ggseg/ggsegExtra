get_contours <- function(raster_object, max_val = 255, verbose = TRUE){
  
  mx <- raster::cellStats(raster_object, stat=max)
  
  # Filter out the blank images
  if (mx < max_val) {
    return(NULL)
  }
  
  tmp.rst <- raster_object
  tmp.rst[tmp.rst == 0] <- NA
  
  ## levels = 50 is to remove the occasional edge point that has
  ## non zero hue.
  #cntr <- raster::rasterToPolygons(rstobj, fun = function(X)X>100, dissolve=TRUE)
  
  if(verbose) cat(paste("extracting contours for", names(raster_object), "\n"))
  g <- sf::st_as_sf(stars::st_as_stars(tmp.rst), merge=TRUE, connect8=TRUE)
  g <- sf::st_sf(g)
  
  if(nrow(g)>0){
    names(g)[[1]] <- "region"
    g$region <- names(raster_object)
    return(g)
  }else{
    return(NULL)
  }
}


isolate_colour <- function(file, outdir, 
                           dilation = NULL, 
                           eroding = NULL, 
                           smoothing = NULL, 
                           verbose){
  
  infile <- basename(file)
  
  alpha_dir <- paste0(outdir, "alpha/")
  mask_dir <- paste0(outdir, "mask/")
  if(!dir.exists(alpha_dir)) dir.create(alpha_dir, recursive = TRUE)
  if(!dir.exists(mask_dir)) dir.create(mask_dir, recursive = TRUE)
  if(verbose) cat(paste("Isolating label from", infile, "\n"))
  
  tmp <- magick::image_read(file)
  tmp <- magick::image_convert(tmp, "png")
  
  if(!is.null(dilation)) 
    tmp <- magick::image_morphology(tmp, "Open", "Disk:2", dilation)
  
  if(!is.null(eroding))  
    tmp <- magick::image_morphology(tmp, "Erode", "Disk:1.5", eroding)
  
  if(!is.null(smoothing))
    tmp <- magick::image_morphology(tmp, "Smooth", "Disk:2", smoothing)
  
  tmp <- magick::image_transparent(tmp, "red", fuzz=45)
  tmp <- magick::image_write(tmp, paste0(alpha_dir, infile))
  
  if(has_magick()){
    cmd <- paste("convert", paste0(alpha_dir, infile),
                 "-alpha extract", paste0(mask_dir, infile))
    k <- system(cmd, intern = !verbose)
    invisible(k)
  }else{
    cat(crayon::red("Cannot complete last extraction step, missing imagemagick. Please install"))
    stop(call. = FALSE)
  }

}


has_magick <- function(){
  k <- system("which convert", intern = TRUE)
  ifelse(k == "", FALSE, TRUE)

}
