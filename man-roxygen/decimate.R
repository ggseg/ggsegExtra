#' @param decimate Mesh decimation factor between 0 and 1. Reduces the number
#'   of faces in 3D meshes using quadric edge decimation (via
#'   [Rvcg::vcgQEdecim()]). A value of 0.5 reduces faces by 50%. Set to NULL
#'   to skip decimation. Requires the Rvcg package. Default is 0.5.
