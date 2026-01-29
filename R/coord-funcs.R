
#' @importFrom dplyr filter mutate
move_hemi_side <- function(data, by, predicate){
  tmp <- filter(data, {{predicate}}) 
  tmp <- mutate(tmp, 
                       `.long` = `.long` + by )
  return(tmp)
}

#' @importFrom dplyr mutate
#' @importFrom sf st_coordinates
correct_coords_sf <- function(data, by){
  
  ymin <- min(st_coordinates(data)[,"Y"])
  
  tmp <- mutate(data, 
                       geometry = geometry + c(by, 0),
                       geometry = geometry - c(0, ymin))
  return(tmp)
}

#' @importFrom dplyr mutate
#' @importFrom sf st_bbox
resize_coords_sf <- function(data, by){
  # resize
  tmp <- mutate(data, 
                       geometry = geometry*by)
  
  # get back to middle  
  bbx <- st_bbox(tmp)
  tmp <- mutate(tmp, 
                       geometry = geometry - bbx[c("xmin", "ymin")])
  return(tmp)
}

#' Isolate region to alpha channel
#'
#' @param input_file image file path
#' @param output_file output file path
#' @param interim_file interim image path
#' @noRd
#' @importFrom magick image_read image_convert image_transparent image_write
isolate_region <- function(input_file, 
                           output_file, 
                           interim_file = tempfile()){
  tmp <- image_read(input_file)
  tmp <- image_convert(tmp, "png")
  
  tmp <- image_transparent(tmp, "white", fuzz=30)
  k <- image_write(tmp, interim_file)
  
  if(has_magick()){
    magick_full_version <- unlist(strsplit(magick_version()[1], " "))[3]
    magick_major_version <- as.numeric(unlist(strsplit(magick_full_version, ""))[1])
    if (magick_major_version >= 7) {
      # per magick warnings, above v7 the command has been renamed
      cmd <- paste("magick", interim_file,
                   "-alpha extract", output_file)
    } else {
      cmd <- paste("convert", interim_file,
                   "-alpha extract", output_file)
    }
    
    # cmd <- paste("convert", input_file,"-channel rgba -fuzz 20% -fill none +opaque red", output_file)
    k <- system(cmd, intern = FALSE)
    invisible(k)
  }else{
    cat("Cannot complete last extraction step, missing imagemagick. Please install")
    stop(call. = FALSE)
  }
}

#' @importFrom dplyr group_by mutate ungroup filter
adjust_coords <- function(atlas_df, by = 1.35){
  
  atlas_df <- group_by(atlas_df, hemi, side)
  atlas_df <- mutate(atlas_df, 
                            `.lat`  = `.lat`-min(`.lat`),
                            `.long` = `.long`-min(`.long`))
  atlas_df <- ungroup(atlas_df)
  
  atlas_df_list <- list(
    lh.lat <- filter(atlas_df,
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

#' @importFrom dplyr group_by group_split ungroup
adjust_coords_sf <- function(atlas_df){
  
  atlas_split <- group_by(atlas_df, hemi, side)
  atlas_split <- group_split(atlas_split)
  views <- lapply(1:length(atlas_split), 
                  function (x) paste(unique(atlas_split[[x]]$hemi), 
                                     unique(atlas_split[[x]]$side), 
                                     sep = "_")
                  )
  names(atlas_split) <- views
  
  atlas <- list(atlas_split[["left_lateral"]],
                atlas_split[["left_medial"]],
                atlas_split[["right_medial"]],
                atlas_split[["right_lateral"]]
                )
  
  rescales <- c(.98, .74, .94, .78)
  coords <-  c(0, 350, 750, 1100)
  
  if (any(grepl("dorsal", views))) {
    atlas <- c(atlas,
               list(
                  atlas_split[["left_dorsal"]],
                  atlas_split[["right_dorsal"]],
                  atlas_split[["right_ventral"]],
                  atlas_split[["left_ventral"]]
               )
               )
    
    rescales <- c(rescales, .75, .75, .65, .65)
    coords <- c(coords, 1500, 1600, 1800, 1900)
  }
  
  # rescale the small ones
  atlas <- lapply(1:length(atlas), function(x) resize_coords_sf(atlas[[x]], rescales[x]))

  # correct coordinates so they are aligned and moved next to each other
  atlas <- lapply(1:length(atlas), function(x) correct_coords_sf(atlas[[x]], coords[x]))

  atlas_df_r <- do.call(rbind, atlas)
  
  return(ungroup(atlas_df_r))
}

#' @importFrom dplyr group_by group_split
adjust_coords_sf2 <- function(atlas_df){
  atlas <- group_by(atlas_df, view)
  atlas <- group_split(atlas)
  
  atlas <- lapply(atlas, gather_geometry)
  
  atlas2 <- restack(atlas)
  
  return(atlas2$df)
}

#' @importFrom sf st_coordinates
count_vertices <- function(x){
  sapply(x$geometry, 
         function(i) nrow(st_coordinates(i)))
}

#' @importFrom dplyr as_tibble group_by mutate row_number ungroup
#' @importFrom sf st_combine st_coordinates
to_coords <- function(x, n){
  if(nrow(x) != 0){
      
    k <- st_combine(x)
    k <- st_coordinates(k)
    k <- as_tibble(k)
    k$L2 <- n * 10000 + k$L2
  
    k <- group_by(k, L2)
    k <- mutate(k, .order = row_number())
    k <- ungroup(k)
    
  }else{
    k <- data.frame(matrix(nrow = 0, ncol = 6))
  }
  names(k) <- c(".long", ".lat",  ".subid", ".id", ".poly", ".order")
  
  k
}

#' @importFrom dplyr group_by group_split
#' @importFrom sf st_polygon st_sfc st_sf st_zm st_cast
coords2sf <- function(x, vertex_size_limits = NULL) {

  dt <- group_by(x, .subid, .id)
  dt <- group_split(dt)
  
  if(!is.null(vertex_size_limits)){
    if(!is.na(vertex_size_limits[1]))
      dt <- dt[sapply(dt, function(x) nrow(x) > vertex_size_limits[1])]

    if(!is.na(vertex_size_limits[2]))
      dt <- dt[sapply(dt, function(x) nrow(x) < vertex_size_limits[2])]
  }
  
  dt <- lapply(dt, as.matrix)
  dt <- lapply(dt, function(x) matrix(as.numeric(x[, 1:4]), ncol = 4))
  
  dt <- st_polygon(dt)
  dt <- st_sfc(dt)
  dt <- st_sf(dt)
  dt <- st_zm(dt)
  dt <- st_cast(dt, "MULTIPOLYGON")
  dt
}

#' @importFrom dplyr mutate
sf2coords <- function(x){
  dt <- mutate(
    x,
    ggseg = list(to_coords(geometry, 1:nrow(x)))
  )
  dt$geometry <- NULL
  
  dt
}

#' @importFrom sf st_bbox
gather_geometry <- function(df){
  bbx <- st_bbox(df$geometry)
  
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

#' @importFrom sf st_coordinates
range_coord <- function(x){
  cent <- st_coordinates(x)  
  cent <- apply(cent, 2, range)
  cent[,1:2]
}

#' @importFrom sf st_bbox
#' @importFrom stats sd
restack <- function(df){
  
  rr <- lapply(df, range_coord)
  rr2 <- lapply(rr, function(k) apply(k, 2, function(x) x[2]*2))
  rr2 <- do.call(rbind, rr2)
  rr_sd <- apply(rr2, 2, sd)*2
  
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
    bx[[k]] <- st_bbox(df2[[k]]$geometry )
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
  globalVariables(c("L2"))
}
