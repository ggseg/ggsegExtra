#' @param tolerance Simplification tolerance for 2D polygons. Higher values
#'   produce simpler shapes with fewer vertices. Passed to [sf::st_simplify()].
#'   If not specified, uses `options("ggseg.extra.tolerance")` or the
#'   `GGSEG_EXTRA_TOLERANCE` environment variable. Default is 0 (no simplification).
