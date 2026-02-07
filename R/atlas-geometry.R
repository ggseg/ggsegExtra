# Geometry processing functions for atlas creation ----
# These functions are shared across volumetric and cortical atlas pipelines

#' @noRd
#' @importFrom dplyr bind_rows group_by summarise
#' @importFrom furrr future_map furrr_options
#' @importFrom progressr progressor
#' @importFrom terra rast global
#' @importFrom sf st_is_empty st_combine st_as_sf st_make_valid
#' @importFrom tools file_path_sans_ext
extract_contours <- function(
  input_dir,
  output_dir,
  verbose = is_verbose(),
  step = "",
  vertex_size_limits = NULL
) {
  if (verbose) {
    cli::cli_progress_step("{step} Extracting contours")
  }

  regions <- list.files(input_dir, full.names = TRUE)
  region_names <- file_path_sans_ext(basename(regions))

  maks <- 0
  for (f in regions[1:min(10, length(regions))]) {
    r <- rast(f)
    m <- global(r, fun = "max", na.rm = TRUE)[1, 1]
    if (m > maks) {
      maks <- m
    }
    if (maks > 0) break
  }
  if (maks == 0) {
    maks <- 1
  }

  p <- progressor(
    steps = length(regions),
    label = paste(step, "Extracting contours")
  )
  contourobjs <- furrr::future_map(
    regions,
    function(region_file) {
      r <- rast(region_file)
      result <- get_contours(
        r,
        max_val = maks,
        vertex_size_limits = vertex_size_limits,
        verbose = FALSE
      )
      p()
      result
    },
    .options = furrr::furrr_options(
      packages = c("terra", "ggsegExtra"),
      globals = c("maks", "vertex_size_limits", "p")
    )
  )
  names(contourobjs) <- region_names

  kp <- !vapply(contourobjs, is.null, logical(1))
  contourobjs2 <- contourobjs[kp]

  contours <- bind_rows(contourobjs2, .id = "filenm")
  contours <- group_by(contours, filenm)
  contours <- summarise(contours, geometry = st_combine(geometry))
  contours <- st_as_sf(contours)
  contours <- st_make_valid(contours)

  save(contours, file = file.path(output_dir, "contours.rda"))

  if (verbose) {
    cli::cli_progress_done()
  }

  invisible(contours)
}


#' @noRd
#' @importFrom smoothr smooth
smooth_contours <- function(dir, smoothness, step, verbose = is_verbose()) {
  load(file.path(dir, "contours.rda"))

  if (verbose) {
    cli::cli_progress_step(
      "{step} Smoothing contours (smoothness = {.val {smoothness}})"
    )
  }
  contours <- filter_valid_geometries(contours)
  if (nrow(contours) == 0) {
    cli::cli_warn("No valid contours found after extraction")
    save(contours, file = file.path(dir, "contours_smoothed.rda"))
    return(invisible(contours))
  }

  contours <- smooth(contours, method = "ksmooth", smoothness = smoothness)
  contours <- filter_valid_geometries(contours)

  save(contours, file = file.path(dir, "contours_smoothed.rda"))
  if (verbose) {
    cli::cli_progress_done()
  }
  invisible(contours)
}


#' @noRd
#' @importFrom sf st_simplify
reduce_vertex <- function(dir, tolerance, step = "", verbose = is_verbose()) {
  if (verbose) {
    cli::cli_progress_step(
      "{step} Reducing vertices (tolerance = {.val {tolerance}})"
    )
  }
  load(file.path(dir, "contours_smoothed.rda"))

  contours <- filter_valid_geometries(contours)
  if (nrow(contours) == 0) {
    cli::cli_warn("No valid contours to simplify")
    save(contours, file = file.path(dir, "contours_reduced.rda"))
    return(invisible(contours))
  }

  contours <- st_simplify(
    contours,
    preserveTopology = TRUE,
    dTolerance = tolerance
  )
  contours <- filter_valid_geometries(contours)
  save(contours, file = file.path(dir, "contours_reduced.rda"))
  if (verbose) {
    cli::cli_progress_done()
  }
  invisible(contours)
}


#' Filter out geometries with non-finite bounds or coordinates
#' @noRd
#' @importFrom sf st_bbox st_is_empty st_coordinates st_make_valid
filter_valid_geometries <- function(sf_obj) {
  if (nrow(sf_obj) == 0) {
    return(sf_obj)
  }

  sf_obj <- st_make_valid(sf_obj)

  valid_idx <- vapply(
    seq_len(nrow(sf_obj)),
    function(i) {
      geom <- sf_obj$geometry[i]

      if (st_is_empty(geom)) {
        return(FALSE)
      }

      coords <- tryCatch(
        st_coordinates(geom),
        error = function(e) NULL
      )
      if (is.null(coords) || nrow(coords) == 0) {
        return(FALSE)
      }
      if (!all(is.finite(coords[, 1:2]))) {
        return(FALSE)
      }

      bbox <- tryCatch(
        st_bbox(geom),
        error = function(e) NULL
      )
      if (is.null(bbox)) {
        return(FALSE)
      }
      if (!all(is.finite(bbox))) {
        return(FALSE)
      }

      TRUE
    },
    logical(1)
  )

  sf_obj[valid_idx, , drop = FALSE]
}


#' @noRd
#' @importFrom dplyr group_by summarise ungroup as_tibble
#' @importFrom sf st_combine st_coordinates st_geometry
#' @importFrom tidyr gather
make_multipolygon <- function(contourfile) {
  load(contourfile)

  contours <- group_by(contours, filenm)
  contours <- summarise(contours, geometry = st_combine(geometry))
  contours <- ungroup(contours)

  # recalc bbox
  bbx1 <- data.frame(
    filenm = contours$filenm,
    xmin = NA,
    ymin = NA,
    xmax = NA,
    ymax = NA
  )

  for (i in seq_len(nrow(contours))) {
    j <- as_tibble(st_coordinates(contours[i, ]))
    j <- gather(j, key, val, X, Y)
    j <- group_by(j, key)
    j <- summarise(j, Min = min(val), Max = max(val))

    bbx1[i, 2:5] <- c(j$Min[1], j$Min[2], j$Max[1], j$Max[2])
  }

  new_bb <- c(min(bbx1$xmin), min(bbx1$ymin), max(bbx1$xmax), max(bbx1$ymax))
  names(new_bb) <- c("xmin", "ymin", "xmax", "ymax")
  attr(new_bb, "class") <- "bbox"

  attr(sf::st_geometry(contours), "bbox") <- new_bb

  return(contours)
}
