#' @param tolerance Simplification tolerance for 2D polygons. Higher values
#'   produce simpler shapes with fewer vertices. Passed to [sf::st_simplify()].
#'   If not specified, uses `options("ggsegExtra.tolerance")` or the
#'   `GGSEGEXTRA_TOLERANCE` environment variable. Default is 0 (no simplification).
