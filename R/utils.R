#' Find the mode of a vector
#'
#' @param x vector
#' @noRd
getmode <- function(x) {
  tmp <- tabulate(x)
  if(length(unique(tmp)) == 1){
    return(NA)
  }else{
    which.max(tmp)
  }
}

#' @noRd
#' @importFrom raster cellStats
#' @importFrom sf st_crs st_as_sf st_is_empty st_geometry
#' @importFrom stars st_as_stars
get_contours <- function(raster_object, max_val = 255, 
                         vertex_size_limits = c(3*10^6,3*10^7),
                         verbose = TRUE){
  mx <- cellStats(raster_object, stat=max)
  
  # Filter out the blank images
  if (mx < max_val) {
    return(NULL)
  }
  
  tmp.rst <- raster_object
  tmp.rst[tmp.rst == 0] <- NA
  
  if(verbose) cat(paste("extracting contours for", names(raster_object), "\n"))

  g <- st_as_stars(tmp.rst)
  sf::st_crs(g) <- 4326 
  g <- st_as_sf(g, connect8=TRUE, 
                    as_points = FALSE, 
                    merge = TRUE)
  
  coords <- to_coords(g, 1)

  coords <- coords2sf(coords, vertex_size_limits)
  
  if(all(nrow(coords)>0 & !st_is_empty(coords))){
    names(coords)[1] <- "geometry"
    sf::st_geometry(coords) <- "geometry"
    coords$filenm <- gsub("^X", "", names(raster_object))
    
    return(coords)
  }else{
    return(NULL)
  }
}

#' @noRd
#' @importFrom magick image_read image_convert image_morphology image_transparent image_write
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
  
  tmp <- image_read(file)
  tmp <- image_convert(tmp, "png")
  
  if(!is.null(dilation)) 
    tmp <- image_morphology(tmp, "Open", "Disk:2", dilation)
  
  if(!is.null(eroding))  
    tmp <- image_morphology(tmp, "Erode", "Disk:1.5", eroding)
  
  if(!is.null(smoothing))
    tmp <- image_morphology(tmp, "Smooth", "Disk:2", smoothing)
  
  tmp <- image_transparent(tmp, "red", fuzz=45)
  tmp <- image_write(tmp, paste0(alpha_dir, infile))
  
  if(has_magick()){
    cmd <- paste("convert", paste0(alpha_dir, infile),
                 "-alpha extract", paste0(mask_dir, infile))
    k <- system(cmd, intern = !verbose)
    invisible(k)
  }else{
    cat("Cannot complete last extraction step, missing imagemagick. Please install")
    stop(call. = FALSE)
  }
  
}

#' @noRd
has_magick <- function(){
  k <- magick_version()
  ifelse(length(k) > 1, TRUE, FALSE)
}

#' @noRd
magick_version <- function()(
  system("identify --version", intern = TRUE)
)

#' @noRd
check_atlas_vertices <- function(atlas_df_sf, max = 10000) {
  
  jj <- sum(count_vertices(atlas_df_sf))
  
  if(jj > max){
    cat("Atlas is complete with", jj,
                           "vertices, try re-running steps 6:7 with a higher 'tolerance' number.")
  }else{
    cat("Atlas complete with", jj, "vertices")
  }
  
}

#' @noRd
gdal_min <- function() "2.4.0"

#' @noRd
#' @importFrom rgdal getGDALVersionInfo
has_gdal <- function(min_version = gdal_min(), verbose = TRUE){
  x <- getGDALVersionInfo()
  
  if(x == ""){
    if(verbose)
      cat("Cannot find gdal installed.\n See install instructions at: https://github.com/domlysz/BlenderGIS/wiki/How-to-install-GDAL")
    return(FALSE)
  }
  
  .ver2num <- function(x){
    x <- strsplit(x, "\\.")[[1]]
    x <- paste0(x, collapse="")
    as.numeric(x)
  }
  
  min_ver <- .ver2num(min_version)
  version <- .ver2num(gsub(",", "", strsplit(x, " ")[[1]][2]))
  
  if(version >= min_ver){
    return(TRUE)
  }else{
    return(FALSE)
  }
  
}

#' @noRd
has_orca <- function(){
  k <- Sys.getenv("orca")
  if(length(k)>0) return(TRUE)
  
  k <- system("which orca", intern = TRUE)
  ifelse(k == "", FALSE, TRUE)
}


orca_version <- function(){
  if(has_orca()){
    cat("Cannot find orca installed.\n See install instructions at: https://github.com/plotly/orca")
    return(NA_character_)
  }
  
  system2("orca", "--version", stdout = TRUE, stderr = TRUE)
}


## quiets concerns of R CMD check
if(getRversion() >= "2.15.1"){
  globalVariables(c("atlas", "surf", "data",
                           "hemi", "i", "j", "k",
                           "x", "y", "z", "r"))
}
