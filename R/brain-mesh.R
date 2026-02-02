# Brain surface mesh functions ----

#' Extract brain surface mesh
#'
#' Extracts the full brain surface mesh for a given subject and surface type.
#' This mesh can be used for 3D rendering where vertex colors are applied
#' based on atlas vertex indices.
#'
#' @param subject FreeSurfer subject (default "fsaverage5")
#' @param hemisphere "lh" or "rh"
#' @param surface Surface type: "inflated", "white", "pial"
#' @param subjects_dir FreeSurfer subjects directory
#'
#' @return list with vertices (data.frame with x, y, z) and faces (data.frame with i, j, k)
#' @export
#' @importFrom freesurfer fs_subj_dir
#'
#' @examples
#' \dontrun{
#' mesh <- get_brain_mesh(hemisphere = "lh", surface = "inflated")
#' str(mesh)
#' }
get_brain_mesh <- function(
  subject = "fsaverage5",
  hemisphere = c("lh", "rh"),
  surface = c("inflated", "white", "pial"),
  subjects_dir = fs_subj_dir()
) {
  check_fs(abort = TRUE)

  hemisphere <- match.arg(hemisphere)
  surface <- match.arg(surface)

  surf_file <- file.path(
    subjects_dir,
    subject,
    "surf",
    paste(hemisphere, surface, sep = ".")
  )

  if (!file.exists(surf_file)) {
    cli::cli_abort(c(
      "Surface file not found:",
      surf_file
    ))
  }

  # Use temporary file for conversion
  tmp_dir <- tempdir()
  tmp_asc <- file.path(tmp_dir, paste0(hemisphere, ".", surface, ".asc"))

  # Convert to ascii using FreeSurfer
  freesurfer::mris_convert(
    infile = surf_file,
    outfile = tmp_asc,
    verbose = FALSE
  )

  # Read ascii file
  asc_lines <- readLines(tmp_asc)

  # First line is comment, second has vertex/face counts
  counts <- as.integer(strsplit(trimws(asc_lines[2]), "\\s+")[[1]])
  n_vertices <- counts[1]
  n_faces <- counts[2]

  # Parse vertices (lines 3 to 2+n_vertices)
  vert_lines <- asc_lines[3:(2 + n_vertices)]
  vertices <- do.call(rbind, lapply(vert_lines, function(line) {
    as.numeric(strsplit(trimws(line), "\\s+")[[1]][1:3])
  }))
  vertices <- as.data.frame(vertices)
  names(vertices) <- c("x", "y", "z")


  # Parse faces (lines after vertices)
  face_lines <- asc_lines[(3 + n_vertices):(2 + n_vertices + n_faces)]
  faces <- do.call(rbind, lapply(face_lines, function(line) {
    as.integer(strsplit(trimws(line), "\\s+")[[1]][1:3])
  }))
  faces <- as.data.frame(faces)
  names(faces) <- c("i", "j", "k")

  # Clean up

  unlink(tmp_asc)

  list(
    vertices = vertices,
    faces = faces,
    hemisphere = hemisphere,
    surface = surface,
    subject = subject
  )
}


#' Create brain surface meshes for ggseg3d
#'
#' Creates brain surface mesh data for all combinations of hemispheres
#' and surface types. This data should be stored in ggseg3d for shared
#' use across all atlases.
#'
#' @param subject FreeSurfer subject (default "fsaverage5")
#' @param surfaces Surface types to extract
#' @param subjects_dir FreeSurfer subjects directory
#'
#' @return Named list of meshes, with names like "lh_inflated", "rh_white", etc.
#' @export
#'
#' @examples
#' \dontrun{
#' brain_meshes <- make_brain_meshes()
#' names(brain_meshes)
#' }
make_brain_meshes <- function(
  subject = "fsaverage5",
  surfaces = c("inflated", "white", "pial"),
  subjects_dir = fs_subj_dir()
) {
  check_fs(abort = TRUE)

  hemispheres <- c("lh", "rh")

  meshes <- list()

  for (hemi in hemispheres) {
    for (surf in surfaces) {
      name <- paste(hemi, surf, sep = "_")
      cli::cli_alert_info("Extracting {name}...")

      meshes[[name]] <- get_brain_mesh(
        subject = subject,
        hemisphere = hemi,
        surface = surf,
        subjects_dir = subjects_dir
      )
    }
  }

  structure(
    meshes,
    class = c("brain_meshes", "list"),
    subject = subject
  )
}


#' Map atlas vertices to mesh colors
#'
#' Given a brain_atlas and a brain mesh, creates a color vector
#' for each vertex based on which region it belongs to.
#'
#' @param atlas A brain_atlas object with vertices column
#' @param mesh A brain mesh (from get_brain_mesh)
#' @param hemisphere Filter atlas to this hemisphere ("left" or "right")
#' @param na_colour Color for vertices not in any region
#'
#' @return Character vector of colors, one per mesh vertex
#' @export
#'
#' @examples
#' \dontrun{
#' atlas <- ggseg.formats::dk
#' mesh <- get_brain_mesh(hemisphere = "lh")
#' colors <- atlas_to_vertex_colors(atlas, mesh, hemisphere = "left")
#' }
atlas_to_vertex_colors <- function(
  atlas,
  mesh,
  hemisphere = c("left", "right"),
  na_colour = "#CCCCCC"
) {
  hemisphere <- match.arg(hemisphere)

  if (!inherits(atlas, "brain_atlas")) {
    cli::cli_abort("atlas must be a brain_atlas object")
  }

  if (!"vertices" %in% names(atlas$data)) {
    cli::cli_abort("atlas must have a 'vertices' column")
  }

  if (!"colour" %in% names(atlas$data)) {
    cli::cli_abort("atlas must have a 'colour' column")
  }

  # Filter to hemisphere
  atlas_data <- atlas$data[atlas$data$hemi == hemisphere, ]

  # Initialize all vertices to NA color
  n_vertices <- nrow(mesh$vertices)
  vertex_colors <- rep(na_colour, n_vertices)

  # For each region, set vertex colors
  for (i in seq_len(nrow(atlas_data))) {
    region_vertices <- atlas_data$vertices[[i]]
    region_colour <- atlas_data$colour[i]

    if (length(region_vertices) > 0 && !is.na(region_colour)) {
      # Vertex indices are 0-indexed, R is 1-indexed
      idx <- region_vertices + 1
      idx <- idx[idx >= 1 & idx <= n_vertices]
      vertex_colors[idx] <- region_colour
    }
  }

  vertex_colors
}
