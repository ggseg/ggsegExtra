# ply/mesh functions ----

#' Extract mesh data from ply
#'
#' .ply files contain a lot of information.
#' for ggseg3d, we only need information
#' on the vertices and faces of the mesh.
#' Thes function opens a ply file, and
#' organises the meshes and faces into
#' a single list.
#'
#' @param ply path to ply-file
#' @param ... arguments to [geomorph::read.ply()]
#'
#' @return list of meshes and faces
#' @export
#'
#' @examples
#' \dontrun{
#' get_mesh("path/to/surface.ply")
#'
#' # Turn off showing the ply when reading
#' get_mesh("path/to/surface.ply", ShowSpecimen = FALSE)
#' }
#' @importFrom geomorph read.ply
get_mesh <- function(ply, ...) {
  if (is.character(ply)) ply <- read.ply(ply, ...)

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

  return(list(vertices = vertices, faces = faces))
}

#' Change old atlas setup to new
#'
#' @param atlas_data ggseg3d-atlas object
#' @importFrom dplyr group_by rename ungroup
#' @importFrom tidyr unnest nest
restruct_old_3datlas <- function(atlas_data) {
  x <- unnest(atlas_data, ggseg_3d)
  x$mesh <- lapply(x$mesh, change_meshes)

  #as_ggseg3d_atlas(atlas)
  x <- group_by(x, atlas, surf, hemi)
  x <- nest(x)
  x <- rename(x, ggseg_3d = data)
  ungroup(x)
}


#' Change meshes to new system
#'
#' @param mesh mesh object
change_meshes <- function(mesh) {
  vertices <- t(mesh$vb)
  vertices <- as.data.frame(vertices)
  names(vertices) <- c("x", "y", "z", "r")

  faces <- t(mesh$it)
  faces <- as.data.frame(faces)
  names(faces) <- c("i", "j", "k")

  return(list(vertices = vertices[, c("x", "y", "z")], faces = faces))
}
