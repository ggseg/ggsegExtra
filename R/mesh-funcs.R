# ply/mesh functions ----

#' Read mesh data from PLY file
#'
#' PLY files contain mesh information including vertices and faces.
#' This function reads a PLY file and extracts the vertices and faces
#' into a list format suitable for ggseg3d.
#'
#' @param ply path to ply-file
#' @param ... arguments to [geomorph::read.ply()]
#'
#' @return list with vertices (data.frame with x, y, z) and faces (data.frame with i, j, k)
#' @export
#'
#' @examples
#' \dontrun{
#' read_ply_mesh("path/to/surface.ply")
#'
#' # Turn off showing the ply when reading
#' read_ply_mesh("path/to/surface.ply", ShowSpecimen = FALSE)
#' }
#' @importFrom geomorph read.ply
read_ply_mesh <- function(ply, ...) {
  if (is.character(ply)) {
    ply <- read.ply(ply, ...)
  }

  vertices <- data.frame(
    x = ply$vb[1, ],
    y = ply$vb[2, ],
    z = ply$vb[3, ]
  )

  faces <- data.frame(
    i = ply$it[1, ],
    j = ply$it[2, ],
    k = ply$it[3, ]
  )

  list(
    vertices = vertices,
    faces = faces
  )
}
