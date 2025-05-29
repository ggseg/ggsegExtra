#' Find the mode of a vector
#'
#' @param x vector
#' @noRd
getmode <- function(x) {
  tmp <- tabulate(x)
  if (length(unique(tmp)) == 1) {
    return(NA)
  } else {
    which.max(tmp)
  }
}

#' @noRd
#' @importFrom terra global rast as.polygons
#' @importFrom sf st_as_sf st_is_empty st_geometry
get_contours <- function(
  raster_object,
  max_val = 255,
  vertex_size_limits = c(3 * 10^6, 3 * 10^7),
  verbose = TRUE
) {
  mx <- global(raster_object, fun = "max", na.rm = TRUE)[1, 1]

  # Filter out the blank images
  if (mx < max_val) {
    return(NULL)
  }

  tmp.rst <- raster_object
  tmp.rst[tmp.rst == 0] <- NA

  # terra provides a direct way to get polygons from raster contours
  contours_raw <- as.polygons(tmp.rst, values = TRUE, na.rm = TRUE)

  # Convert to sf object
  coords <- st_as_sf(contours_raw)

  if (all(nrow(coords) > 0 & !st_is_empty(coords))) {
    coords <- to_coords(coords, 1)
    coords <- coords2sf(coords, vertex_size_limits)

    return(coords)
  }
  return(NULL)
}

#' @noRd
#' @importFrom magick image_read image_convert image_morphology image_transparent image_write
isolate_colour <- function(
  file,
  outdir,
  dilation = NULL,
  eroding = NULL,
  smoothing = NULL,
  verbose = FALSE
) {
  infile <- basename(file)

  alpha_dir <- file.path(outdir, "alpha/")
  mask_dir <- file.path(outdir, "mask/")
  mkdir(alpha_dir)
  mkdir(mask_dir)

  if (verbose) cat(paste("Isolating label from", infile, "\n"))

  tmp <- image_read(file)
  tmp <- image_convert(tmp, "png")

  if (!is.null(dilation))
    tmp <- image_morphology(tmp, "Open", "Disk:2", dilation)

  if (!is.null(eroding))
    tmp <- image_morphology(tmp, "Erode", "Disk:1.5", eroding)

  if (!is.null(smoothing))
    tmp <- image_morphology(tmp, "Smooth", "Disk:2", smoothing)

  tmp <- image_transparent(tmp, "red", fuzz = 45)
  tmp <- image_write(tmp, paste0(alpha_dir, infile))

  check_magick()

  cmd <- paste(
    "magick",
    paste0(alpha_dir, infile),
    "-alpha extract",
    paste0(mask_dir, infile)
  )
  k <- run_cmd(cmd, verbose = verbose)
  invisible(k)
}

#' @noRd
has_magick <- function() {
  k <- magick_version()
  ifelse(length(k) > 1, TRUE, FALSE)
}

check_magick <- function() {
  k <- has_magick()
  if (!k)
    cli::cli_abort(c(
      "ImageMagick not installed, cannot run pipeline.",
      "i" = "See {.url https://imagemagick.org/script/download.php}"
    ))
}

#' @noRd
magick_version <- function() {
  run_cmd("magick --version", verbose = TRUE)
}

#' @noRd
run_cmd <- function(cmd, verbose = FALSE, no_ui = FALSE) {
  if (no_ui) {
    cmd <- paste("fsxvfb", cmd)
  }
  system(
    paste0(get_fs(), cmd),
    intern = TRUE,
    ignore.stdout = !verbose,
    ignore.stderr = !verbose
  )
}

#' @noRd
check_atlas_vertices <- function(atlas_df_sf, max = 10000) {
  jj <- sum(count_vertices(atlas_df_sf))

  if (jj > max) {
    cli::cli_alert_info(
      "Atlas is complete with {jj} vertices, try re-running steps 6:7 with a higher `tolerance` number."
    )
  } else {
    cli::cli_alert_success("Atlas complete with {jj} vertices")
  }
}

init_progress <- function(handler = "cli") {
  progressr::handlers(handler)
  progressr::handlers(global = TRUE)
}

init_kaleido <- function() {
  if (reticulate::condaenv_exists()) return()

  if (!dir.exists(reticulate::miniconda_path())) reticulate::install_miniconda()

  cli::cli_alert_info("Setting up Mini-Cona env for kaleido")
  reticulate::conda_install(
    'ggseg-reticulate',
    'python-kaleido'
  )
  reticulate::conda_install(
    'ggseg-reticulate',
    'plotly',
    channel = 'plotly'
  )
  reticulate::use_miniconda('ggseg-reticulate')
}

mkdir <- function(path, ...) {
  dir.create(path, recursive = TRUE, showWarnings = FALSE, ...)
}

## quiets concerns of R CMD check
if (getRversion() >= "2.15.1") {
  globalVariables(c(
    "atlas",
    "surf",
    "data",
    "hemi",
    "i",
    "j",
    "k",
    "x",
    "y",
    "z",
    "r"
  ))
}
