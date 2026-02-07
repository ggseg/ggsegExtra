#' @param smoothness Smoothing factor for 2D contours. Higher values produce
#'   smoother region boundaries. Passed to [smoothr::smooth()]. If not
#'   specified, uses `options("ggsegExtra.smoothness")` or the
#'   `GGSEGEXTRA_SMOOTHNESS` environment variable. Default is 5.
