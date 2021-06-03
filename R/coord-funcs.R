

move_hemi_side <- function(data, by, predicate){
  tmp <- dplyr::filter(data, {{predicate}}) 
  tmp <- dplyr::mutate(tmp, 
                       `.long` = `.long` + by )
  return(tmp)
}

correct_coords_sf <- function(data, by){
  
  ymin <- min(sf::st_coordinates(data)[,"Y"])
  
  tmp <- dplyr::mutate(data, 
                       geometry = geometry + c(by, 0),
                       geometry = geometry - c(0, ymin))
  return(tmp)
}

resize_coords_sf <- function(data, by){
  # resize
  tmp <- dplyr::mutate(data, 
                       geometry = geometry*by)
  
  # get back to middle  
  bbx <- sf::st_bbox(tmp)
  tmp <- dplyr::mutate(tmp, 
                       geometry = geometry - bbx[c("xmin", "ymin")])
  return(tmp)
}

#' Isolate region to alpha channel
#'
#' @param input_file image file path
#' @param output_file output file path
#' @param interrim_file interrim image path
#' @noRd
isolate_region <- function(input_file, 
                           output_file, 
                           interrim_file = tempfile()){
  tmp <- magick::image_read(input_file)
  tmp <- magick::image_convert(tmp, "png")
  
  tmp <- magick::image_transparent(tmp, "white", fuzz=30)
  k <- magick::image_write(tmp, interrim_file)
  
  if(has_magick()){
    cmd <- paste("convert", interrim_file,
                 "-alpha extract", output_file)
    
    # cmd <- paste("convert", input_file,"-channel rgba -fuzz 20% -fill none +opaque red", output_file)
    k <- system(cmd, intern = FALSE)
    invisible(k)
  }else{
    cat(crayon::red("Cannot complete last extraction step, missing imagemagick. Please install"))
    stop(call. = FALSE)
  }
}

adjust_coords <- function(atlas_df, by = 1.35){
  
  atlas_df <- dplyr::group_by(atlas_df, hemi, side)
  atlas_df <- dplyr::mutate(atlas_df, 
                            `.lat`  = `.lat`-min(`.lat`),
                            `.long` = `.long`-min(`.long`))
  atlas_df <- dplyr::ungroup(atlas_df)
  
  atlas_df_list <- list(
    lh.lat <- dplyr::filter(atlas_df,
                            (hemi=="left" & side=="lateral")),
    lh.med = move_hemi_side(atlas_df, 430,
                            (hemi=="left" & side=="medial")),
    rh.med <- move_hemi_side(atlas_df, 730,
                             (hemi=="right" & side=="medial")),
    rh.lat <- move_hemi_side(atlas_df, 1300,
                             (hemi=="right" & side=="lateral"))
  )
  
  # rescale the small ones
  atlas_df_list[[1]]$`.lat` <- atlas_df_list[[1]]$`.lat`*by
  atlas_df_list[[1]]$`.long` <- atlas_df_list[[1]]$`.long`*by
  atlas_df_list[[3]]$`.lat` <- atlas_df_list[[3]]$`.lat`*(by*.9)
  atlas_df_list[[3]]$`.long` <- atlas_df_list[[3]]$`.long`*(by*.9)
  
  do.call(rbind, atlas_df_list)
}

adjust_coords_sf <- function(atlas_df){
  
  atlas <- dplyr::group_by(atlas_df, hemi, side)
  atlas <- dplyr::group_split(atlas)
  
  atlas <- list(atlas[[1]], # left lat
                atlas[[2]], # left med
                atlas[[4]], # right med
                atlas[[3]]) # right lat
  
  # rescale the small ones
  sz <- c(.98, .74, .94, .78)
  atlas <- lapply(1:4, function(x) resize_coords_sf(atlas[[x]], sz[x]))

  # correct coordinates so they ar ealigned and moved next to eachoter
  sz <-  c(0, 350, 750, 1100)
  atlas <- lapply(1:4, function(x) correct_coords_sf(atlas[[x]], sz[x]))

  atlas_df_r <- do.call(rbind, atlas)
  
  return(dplyr::ungroup(atlas_df_r))
}

adjust_coords_sf2 <- function(atlas_df){
  atlas <- dplyr::group_by(atlas_df, view)
  atlas <- dplyr::group_split(atlas)
  
  atlas <- lapply(atlas, gather_geometry)
  
  atlas2 <- restack(atlas)
  
  return(atlas2$df)
}


count_vertices <- function(x){
  sapply(x$geometry, 
         function(i) nrow(sf::st_coordinates(i)))
}

to_coords <- function(x, n){
  if(nrow(x) != 0){
      
    k <- sf::st_combine(x)
    k <- sf::st_coordinates(k)
    k <- dplyr::as_tibble(k)
    k$L2 <- n * 10000 + k$L2
  
    k <- dplyr::group_by(k, L2)
    k <- dplyr::mutate(k, .order = dplyr::row_number())
    k <- dplyr::ungroup(k)
    
  }else{
    k <- data.frame(matrix(nrow = 0, ncol = 6))
  }
  names(k) <- c(".long", ".lat",  ".subid", ".id", ".poly", ".order")
  
  k
}

coords2sf <- function(x, vertex_size_limits = NULL) {

  dt <- dplyr::group_by(x, .subid, .id)
  dt <- dplyr::group_split(dt)
  
  if(!is.null(vertex_size_limits)){
    if(!is.na(vertex_size_limits[1]))
      dt <- dt[sapply(dt, function(x) nrow(x) > vertex_size_limits[1])]

    if(!is.na(vertex_size_limits[2]))
      dt <- dt[sapply(dt, function(x) nrow(x) < vertex_size_limits[2])]
  }
  
  dt <- lapply(dt, as.matrix)
  dt <- lapply(dt, function(x) matrix(as.numeric(x[, 1:4]), ncol = 4))
  
  dt <- sf::st_polygon(dt)
  dt <- sf::st_sfc(dt)
  dt <- sf::st_sf(dt)
  dt <- sf::st_zm(dt)
  dt <- sf::st_cast(dt, "MULTIPOLYGON")
  dt
}

sf2coords <- function(x){
  dt <- dplyr::mutate(
    x,
    ggseg = list(to_coords(geometry, 1:nrow(x)))
  )
  dt$geometry <- NULL
  
  dt
}


gather_geometry <- function(df){
  bbx <- sf::st_bbox(df$geometry)
  
  # cent <- bbx[c("xmin", "ymin")]
  cent <- center_coord(df)
  
  df$geometry <- df$geometry - cent[1:2]
  
  df
}


center_coord <- function(x){
  cent <- range_coord(x)
  cent <- apply(cent, 2, mean)
  cent
}

range_coord <- function(x){
  cent <- sf::st_coordinates(x)  
  cent <- apply(cent, 2, range)
  cent[,1:2]
}

restack <- function(df){
  
  rr <- lapply(df, range_coord)
  rr2 <- lapply(rr, function(k) apply(k, 2, function(x) x[2]*2))
  rr2 <- do.call(rbind, rr2)
  rr_sd <- apply(rr2, 2, stats::sd)*2
  
  x_rr <- abs(rr[[1]][,1])[1]
  y_rr <- apply(abs(do.call(rbind, rr)), 2, max)[2]
  
  .cent <- function(x){
    x$geometry <- x$geometry + c(x_rr, y_rr)
    x
  }
  
  df2 <- lapply(df, .cent)
  
  bx <- list()
  for(k in 1:length(df)){
    if(k != 1){
      mes <- c(bx[[k-1]][3] + rr_sd[1], 0)
      df2[[k]]$geometry <- df2[[k]]$geometry + mes
    }
    bx[[k]] <- sf::st_bbox(df2[[k]]$geometry )
  }
  
  bx <- do.call(rbind, bx)
  bx <- apply(bx, 2, min)
  
  return(
    list(
      df = do.call(rbind, df2),
      box = bx
    )
  )
}

## quiets concerns of R CMD check
if(getRversion() >= "2.15.1"){
  utils::globalVariables(c("L2"))
}
