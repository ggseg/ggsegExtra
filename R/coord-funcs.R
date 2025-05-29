#' @importFrom dplyr filter mutate
move_hemi_side <- function(data, by, predicate) {
  tmp <- filter(data, {{ predicate }})
  tmp <- mutate(tmp, `.long` = `.long` + by)
  return(tmp)
}

#' @importFrom dplyr mutate
#' @importFrom sf st_geometry st_bbox st_transform
correct_coords_sf <- function(data, by) {
  geom <- st_geometry(data)

  # Calculate the bounding box of the *entire* geometry collection
  bbox <- st_bbox(geom)
  ymin <- bbox["ymin"]

  # Apply the transformations to the geometry column
  transformed_geom <- geom + c(by, 0) - c(0, ymin)

  # Update the geometry column of the input data
  tmp <- mutate(
    data,
    geometry = transformed_geom
  )
  return(tmp)
}

#' @importFrom dplyr mutate
#' @importFrom sf st_bbox
resize_coords_sf <- function(data, by) {
  # resize
  tmp <- mutate(data, geometry = geometry * by)

  # get back to middle
  bbx <- st_bbox(tmp)
  tmp <- mutate(tmp, geometry = geometry - bbx[c("xmin", "ymin")])
  return(tmp)
}

#' Isolate region to alpha channel
#'
#' @param input_file image file path
#' @param output_file output file path
#' @param interim_file interim image path
#' @noRd
#' @importFrom magick image_read image_convert image_transparent image_write
isolate_region <- function(
  input_file,
  output_file,
  interim_file = tempfile()
) {
  if (file.exists(output_file)) return()

  tmp <- image_read(input_file)
  tmp <- image_convert(tmp, "png")

  tmp <- image_transparent(tmp, "white", fuzz = 30)
  k <- image_write(tmp, interim_file)

  if (has_magick()) {
    cmd <- paste("magick", interim_file, "-alpha extract", output_file)

    # cmd <- paste("magick", input_file,"-channel rgba -fuzz 20% -fill none +opaque red", output_file)
    k <- run_cmd(cmd)
    invisible(k)
  } else {
    cli::cli_abort(
      "Cannot complete last extraction step, missing imagemagick. Please install"
    )
  }
}

#' @importFrom dplyr group_by mutate ungroup filter
adjust_coords <- function(atlas_df, by = 1.35) {
  atlas_df <- group_by(atlas_df, hemi, side)
  atlas_df <- mutate(
    atlas_df,
    `.lat` = `.lat` - min(`.lat`),
    `.long` = `.long` - min(`.long`)
  )
  atlas_df <- ungroup(atlas_df)

  atlas_df_list <- list(
    lh.lat <- filter(atlas_df, (hemi == "left" & side == "lateral")),
    lh.med = move_hemi_side(atlas_df, 430, (hemi == "left" & side == "medial")),
    rh.med <- move_hemi_side(
      atlas_df,
      730,
      (hemi == "right" & side == "medial")
    ),
    rh.lat <- move_hemi_side(
      atlas_df,
      1300,
      (hemi == "right" & side == "lateral")
    )
  )

  # rescale the small ones
  atlas_df_list[[1]]$`.lat` <- atlas_df_list[[1]]$`.lat` * by
  atlas_df_list[[1]]$`.long` <- atlas_df_list[[1]]$`.long` * by
  atlas_df_list[[3]]$`.lat` <- atlas_df_list[[3]]$`.lat` * (by * .9)
  atlas_df_list[[3]]$`.long` <- atlas_df_list[[3]]$`.long` * (by * .9)

  do.call(rbind, atlas_df_list)
}

#' @importFrom dplyr group_by group_split ungroup
adjust_coords_sf <- function(atlas_df) {
  atlas <- group_by(atlas_df, hemi, side)
  atlas <- group_split(atlas)

  atlas <- list(
    atlas[[1]], # left lat
    atlas[[2]], # left med
    atlas[[4]], # right med
    atlas[[3]]
  ) # right lat

  # rescale the small ones
  sz <- c(.98, .74, .94, .78)
  atlas <- lapply(1:4, function(x) resize_coords_sf(atlas[[x]], sz[x]))

  # correct coordinates so they ar ealigned and moved next to eachoter
  sz <- c(0, 350, 750, 1100)
  atlas <- lapply(1:4, function(x) correct_coords_sf(atlas[[x]], sz[x]))

  atlas_df_r <- do.call(rbind, atlas)

  return(ungroup(atlas_df_r))
}

#' @importFrom dplyr group_by group_split
adjust_coords_sf2 <- function(atlas_df) {
  atlas <- group_by(atlas_df, view)
  atlas <- group_split(atlas)

  atlas <- lapply(atlas, gather_geometry)

  atlas2 <- restack(atlas)

  return(atlas2$df)
}

#' @importFrom sf st_coordinates
count_vertices <- function(x) {
  sapply(x$geometry, function(i) nrow(st_coordinates(i)))
}

#' @importFrom dplyr as_tibble group_by mutate row_number ungroup
#' @importFrom sf st_combine st_coordinates
to_coords <- function(x, n) {
  if (nrow(x) != 0) {
    k <- st_combine(st_geometry(x)) # Operate on the geometry column
    k <- st_coordinates(k)
    k <- as_tibble(k)
    k$L2 <- n * 10000 + k$L2

    k <- group_by(k, L2)
    k <- mutate(k, .order = row_number())
    k <- ungroup(k)
  } else {
    k <- data.frame(matrix(nrow = 0, ncol = 6))
  }
  names(k) <- c(".long", ".lat", ".subid", ".id", ".poly", ".order")
  k
}

#' @importFrom dplyr group_by group_split
#' @importFrom sf st_polygon st_sfc st_sf st_zm st_cast
coords2sf <- function(x, vertex_size_limits = NULL) {
  dt <- group_by(x, .subid, .id)
  dt_split <- group_split(dt)

  if (!is.null(vertex_size_limits)) {
    if (!is.na(vertex_size_limits[1]))
      dt_split <- dt_split[sapply(
        dt_split,
        function(d) nrow(d) > vertex_size_limits[1]
      )]

    if (!is.na(vertex_size_limits[2]))
      dt_split <- dt_split[sapply(
        dt_split,
        function(d) nrow(d) < vertex_size_limits[2]
      )]
  }

  polygon_list <- lapply(dt_split, function(d) {
    coords_matrix <- as.matrix(d[, c(".long", ".lat")])
    # Ensure the polygon is closed by repeating the first vertex
    if (!identical(coords_matrix[1, ], tail(coords_matrix, 1)[1, ])) {
      coords_matrix <- rbind(coords_matrix, coords_matrix[1, ])
    }
    st_polygon(list(coords_matrix))
  })

  if (length(polygon_list) > 0) {
    dt_sf <- st_sfc(polygon_list)
    dt_sf <- st_sf(geometry = dt_sf)
    dt_sf <- st_zm(dt_sf) # Keep if Z/M dimensions are important
    dt_sf <- st_cast(dt_sf, "MULTIPOLYGON")
    return(dt_sf)
  }
  return(
    st_sf(geometry = st_sfc(crs = st_crs(NA)))
  )
}

#' @importFrom dplyr mutate
sf2coords <- function(x) {
  dt <- mutate(
    x,
    ggseg = list(to_coords(geometry, 1:nrow(x)))
  )
  dt$geometry <- NULL

  dt
}


#' @importFrom sf st_bbox
gather_geometry <- function(df) {
  bbx <- st_bbox(df$geometry)
  cent <- center_coord(df)
  df$geometry <- df$geometry - cent[1:2]
  df
}


center_coord <- function(x) {
  cent <- range_coord(x)
  cent <- apply(cent, 2, mean)
  cent
}

#' @importFrom sf st_coordinates
range_coord <- function(x) {
  cent <- st_coordinates(x)
  cent <- apply(cent, 2, range)
  cent[, 1:2]
}

#' @importFrom sf st_bbox
#' @importFrom stats sd
restack <- function(df) {
  rr <- lapply(df, range_coord)
  rr2 <- lapply(rr, function(k) apply(k, 2, function(x) x[2] * 2))
  rr2 <- do.call(rbind, rr2)
  rr_sd <- apply(rr2, 2, sd) * 2

  x_rr <- abs(rr[[1]][, 1])[1]
  y_rr <- apply(abs(do.call(rbind, rr)), 2, max)[2]

  .cent <- function(x) {
    x$geometry <- x$geometry + c(x_rr, y_rr)
    x
  }

  df2 <- lapply(df, .cent)

  bx <- list()
  for (k in 1:length(df)) {
    if (k != 1) {
      mes <- c(bx[[k - 1]][3] + rr_sd[1], 0)
      df2[[k]]$geometry <- df2[[k]]$geometry + mes
    }
    bx[[k]] <- st_bbox(df2[[k]]$geometry)
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
if (getRversion() >= "2.15.1") {
  globalVariables(c("L2"))
}
