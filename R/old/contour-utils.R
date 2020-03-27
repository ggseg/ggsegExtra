get_contours <- function(raster_object, max_val = 255, verbose = TRUE){
  mx <- raster::cellStats(raster_object, stat=max)
  # Filter out the blank images
  if (mx < max_val) {
    return(NULL)
  }
  # browser()
  tmp.rst <- raster_object
  tmp.rst[tmp.rst != 0] <- NA
  
  ## levels = 50 is to remove the occasional edge point that has
  ## non zero hue.
  #cntr <- raster::rasterToPolygons(rstobj, fun = function(X)X>100, dissolve=TRUE)
  cat(paste("extracting contours for", names(raster_object), "\n"))
  g <- sf::st_as_sf(stars::st_as_stars(tmp.rst), merge=TRUE, connect8=TRUE)
  ## Is it a multipolygon? Keep the biggest bit
  ## Small parts are usually corner connected single voxels
  # if (nrow(g)>1) {
  #   gpa <- sf::st_area(g)
  #   biggest <- which.max(gpa)
  #   g <- g[biggest,]
  # }
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
                           dilation = 2, eroding = 2, smoothing = 2, 
                           verbose){
  
  infile <- basename(file)

  alpha_dir <- paste0(outdir, "alpha/")
  mask_dir <- paste0(outdir, "mask/")
  if(!dir.exists(alpha_dir)) dir.create(alpha_dir, recursive = TRUE)
  if(!dir.exists(mask_dir)) dir.create(mask_dir, recursive = TRUE)
  if(verbose) cat(paste("Isolating label from", infile, "\n"))
  
  magick::image_read(file) %>%
    magick::image_convert("png") %>%
    magick::image_morphology("Open", "Disk:2", dilation) %>% 
    magick::image_morphology("Erode", "Disk:1.5", eroding) %>% 
    magick::image_morphology("Smooth", "Disk:2", smoothing) %>%
    magick::image_transparent("red", fuzz=45) %>% 
    magick::image_write(paste0(alpha_dir, infile))
  
  cmd <- paste("convert", paste0(alpha_dir, infile),
               "-alpha extract", paste0(mask_dir, infile))
  k <- system(cmd, intern = !verbose)
  invisible(k)
}
