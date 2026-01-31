# dpv-functions ----

#' Write DPV file
#'
#' write a data-per-vertex file,
#' and ascii-format text-file
#' based on vertices and faces.
#'
#' @param path path to file
#' @param vertices object with vertices
#' @param faces object with faces
#' @noRd
# #' @export
write_dpv <- function(path, vertices, faces) {
  # face index start at 0
  if (min(faces) == 1) {
    faces <- faces - 1
  }

  # Add 0-col to both
  vertices <- cbind(vertices, r = rep(0, nrow(vertices)))
  faces <- cbind(faces, r = rep(0, nrow(faces)))

  # Make every row a single string
  vertices <- within(vertices, l <- sprintf(paste('%f %f %f %g'), x, y, z, r))
  faces <- within(faces, l <- sprintf(paste('%g %g %g %g'), i, j, k, r))

  # Write to the disk
  file_content <- c(
    "#!ascii",
    sprintf('%g %g', nrow(vertices), nrow(faces)),
    vertices$l,
    faces$l
  )

  con <- file(path)
  on.exit(close(con))
  writeLines(file_content, con)
}

#' Read dpv file
#'
#' @param path path to dpv file
#' @noRd
#' @return list of vertices and faces
#' @importFrom utils read.table
read_dpv <- function(path) {
  k <- readLines(path)

  ns <- strsplit(k[2], " ")[[1]]
  names(ns) <- c("nvertices", "nfaces")

  k <- read.table(path, skip = 2)

  vertices <- k[1:ns["nvertices"], ]
  row.names(vertices) = NULL

  faces <- k[ns["nvertices"]:length(k), ]
  row.names(faces) = NULL

  return(list(vertices = vertices, faces = faces))
}

#' Check if dpv is facewise
#'
#' @param dpx vertex or face data
#' @param surface surface vertex and face list
#' @template verbose
#' @noRd
is_facewise <- function(dpx, surface, verbose = TRUE) {
  nX = nrow(dpx)
  nV = nrow(surface$vertices)
  nF = nrow(surface$faces)

  # Verify if this is facewise or vertexwise data
  if (nX == nV) {
    if (verbose) {
      cli::cli_alert_info('Working with vertexwise data.\n')
    }
    return(FALSE)
  } else if (nX == nF) {
    if (verbose) {
      cli::cli_alert_info('Working with facewise data.\n')
    }
    return(TRUE)
  } else {
    cli::cli_abort('The annot-data does not match the surface.\n')
  }
}
