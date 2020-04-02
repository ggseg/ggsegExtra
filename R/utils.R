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
#'
#' @export
write_dpv <- function(path, vertices, faces){
  # face index start at 0
  if(min(faces) == 1) faces <- faces - 1
  
  # Add 0-col to both
  vertices <- cbind(vertices, r = rep(0, nrow(vertices)))
  faces <- cbind(faces, r = rep(0, nrow(faces)))
  
  # Make every row a single string
  vertices <- within(vertices,  l <- sprintf(paste('%f %f %f %g'), x, y, z, r))
  faces <- within(faces,  l <- sprintf(paste('%g %g %g %g'), i, j, k, r))
  
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
#' Read in data-per-vertex file, which is
#' usually a file made by for instance \code{\link{surf2asc}},
#' \code{\link{curv2asc}}, \code{\link{curvnf2asc}}
#'
#' @param path path to dpv file
#'
#' @return list of vertices and faces
#' @export
read_dpv <- function(path){
  
  k <- readLines(path)
  
  ns <- strsplit(k[2], " ")[[1]]
  names(ns) <- c("nvertices", "nfaces")  
  
  k <- utils::read.table(path, skip = 2)
  
  vertices <- k[1:ns["nvertices"],]
  row.names(vertices) = NULL
  
  faces <- k[ns["nvertices"]:length(k),]
  row.names(faces) = NULL
  
  return(list(vertices = vertices,
              faces = faces))
}

#' Check if dpv is facewise
#'
#' @param dpx vertex or face data
#' @param surface surface vertex and face list
#' @template verbose 
is_facewise <- function(dpx, surface, verbose = TRUE){
  nX = nrow(dpx)
  nV = nrow(surface$vertices)
  nF = nrow(surface$faces)
  
  # Verify if this is facewise or vertexwise data
  if(nX == nV){
    if(verbose) cat('Working with vertexwise data.\n')
    facewise = FALSE
  }else if(nX == nF){
    if(verbose) cat('Working with facewise data.\n');
    facewise = TRUE
  } else{
    if(verbose) cat('The data does not match the surface.\n')
    facewise = NA
  }
  
  facewise
}

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
#' @param ... arguments to \code{\link[geomorph]{read.ply}} 
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
get_mesh <- function(ply, ...){
  
  if(is.character(ply)) 
    ply <- geomorph::read.ply(ply, ...)
  
  vertices <- data.frame(
    x = ply$vb[1,],
    y = ply$vb[2,],
    z = ply$vb[3,]
  )
  
  faces <- data.frame(
    i = ply$it[1,],
    j = ply$it[2,],
    k = ply$it[3,]
  )
  
  return(list(vertices = vertices, 
              faces = faces))
}

#' Change old atlas setup to new
#'
#' @param atlas_data ggseg3d-atlas object
restruct_old_3datlas <- function(atlas_data){
  x <- tidyr::unnest(atlas_data, ggseg_3d)
  x$mesh <- lapply(x$mesh, change_meshes)
  
  #as_ggseg3d_atlas(atlas)
  x <- dplyr::group_by(x, atlas, surf, hemi)
  x <- tidyr::nest(x)
  x <- dplyr::rename(x, ggseg_3d = data)
  dplyr::ungroup(x)
}


#' Change meshes to new system
#'
#' @param mesh mesh object
change_meshes <- function(mesh){
  vertices <- t(mesh$vb)
  vertices <- as.data.frame(vertices)
  names(vertices) <- c("x","y","z","r")
  
  faces <- t(mesh$it)
  faces <- as.data.frame(faces)
  names(faces) <- c("i","j","k")
  
  return(list(vertices = vertices[, c("x", "y", "z")],
              faces = faces))
}


# Other ----
#' Find the mode of a vector
#'
#' @param x vector
getmode <- function(x) {
  tmp <- tabulate(x)
  if(length(unique(tmp)) == 1){
    return(NA)
  }else{
    which.max(tmp)
  }
}

get_contours <- function(raster_object, max_val = 255, verbose = TRUE){
  
  mx <- raster::cellStats(raster_object, stat=max)
  
  # Filter out the blank images
  if (mx < max_val) {
    return(NULL)
  }
  
  tmp.rst <- raster_object
  tmp.rst[tmp.rst == 0] <- NA
  
  ## levels = 50 is to remove the occasional edge point that has
  ## non zero hue.
  #cntr <- raster::rasterToPolygons(rstobj, fun = function(X)X>100, dissolve=TRUE)
  
  if(verbose) cat(paste("extracting contours for", names(raster_object), "\n"))
  g <- sf::st_as_sf(stars::st_as_stars(tmp.rst), merge=TRUE, connect8=TRUE)
  g <- sf::st_sf(g)
  
  if(nrow(g)>0){
    names(g)[[1]] <- "region"
    g$region <- names(raster_object)
    return(g)
  }else{
    return(NULL)
  }
}


isolate_colour <- function(file, outdir, 
                           dilation = NULL, 
                           eroding = NULL, 
                           smoothing = NULL, 
                           verbose){
  
  infile <- basename(file)
  
  alpha_dir <- paste0(outdir, "alpha/")
  mask_dir <- paste0(outdir, "mask/")
  if(!dir.exists(alpha_dir)) dir.create(alpha_dir, recursive = TRUE)
  if(!dir.exists(mask_dir)) dir.create(mask_dir, recursive = TRUE)
  if(verbose) cat(paste("Isolating label from", infile, "\n"))
  
  tmp <- magick::image_read(file)
  tmp <- magick::image_convert(tmp, "png")
  
  if(!is.null(dilation)) 
    tmp <- magick::image_morphology(tmp, "Open", "Disk:2", dilation)
  
  if(!is.null(eroding))  
    tmp <- magick::image_morphology(tmp, "Erode", "Disk:1.5", eroding)
  
  if(!is.null(smoothing))
    tmp <- magick::image_morphology(tmp, "Smooth", "Disk:2", smoothing)
  
  tmp <- magick::image_transparent(tmp, "red", fuzz=45)
  tmp <- magick::image_write(tmp, paste0(alpha_dir, infile))
  
  if(has_magick()){
    cmd <- paste("convert", paste0(alpha_dir, infile),
                 "-alpha extract", paste0(mask_dir, infile))
    k <- system(cmd, intern = !verbose)
    invisible(k)
  }else{
    cat(crayon::red("Cannot complete last extraction step, missing imagemagick. Please install"))
    stop(call. = FALSE)
  }
  
}


has_magick <- function(){
  k <- system("which convert", intern = TRUE)
  ifelse(k == "", FALSE, TRUE)
  
}


move_hemi_side <- function(data, by, predicate){
  tmp <- dplyr::filter(data, {{predicate}}) 
  tmp <- dplyr::mutate(tmp, 
                       `.long` = `.long` + by )
  return(tmp)
}


#' Isolate region to alpha channel
#'
#' @param input_file image file path
#' @param output_file output file path
#' @param interrim_file interrim image path
#'
isolate_region <- function(input_file, 
                           output_file, 
                           interrim_file = tempfile()){
  tmp <- magick::image_read(input_file)
  tmp <- magick::image_convert(tmp, "png")
  
  tmp <- magick::image_transparent(tmp, "white", fuzz=30)
  k <- magick::image_write(tmp, interrim_file)
  
  if(has_magick()){
    cmd <- paste("convert", interrim_file,
                 "-alpha extract", output_file)
    
    # cmd <- paste("convert", input_file,"-channel rgba -fuzz 20% -fill none +opaque red", output_file)
    k <- system(cmd, intern = FALSE)
    invisible(k)
  }else{
    cat(crayon::red("Cannot complete last extraction step, missing imagemagick. Please install"))
    stop(call. = FALSE)
  }
}

adjust_coords <- function(atlas_df, by = 1.35){
  atlas_df <- dplyr::group_by(atlas_df, hemi, side)
  atlas_df <- dplyr::mutate(atlas_df, 
                            `.lat`  = `.lat`-min(`.lat`),
                            `.long` = `.long`-min(`.long`))
  atlas_df <- dplyr::ungroup(atlas_df)
  
  atlas_df_list <- list(
    lh.lat <- dplyr::filter(atlas_df,
                            (hemi=="left" & side=="lateral")),
    lh.med = move_hemi_side(atlas_df, 430,
                            (hemi=="left" & side=="medial")),
    rh.med <- move_hemi_side(atlas_df, 730,
                             (hemi=="right" & side=="medial")),
    rh.lat <- move_hemi_side(atlas_df, 1300,
                             (hemi=="right" & side=="lateral"))
  )
  
  # rescale the small ones
  atlas_df_list[[1]]$`.lat` <- atlas_df_list[[1]]$`.lat`*by
  atlas_df_list[[1]]$`.long` <- atlas_df_list[[1]]$`.long`*by
  atlas_df_list[[3]]$`.lat` <- atlas_df_list[[3]]$`.lat`*(by*.9)
  atlas_df_list[[3]]$`.long` <- atlas_df_list[[3]]$`.long`*(by*.9)
  
  do.call(rbind, atlas_df_list)
}

to_coords <- function(x, n){
  k <- sf::st_coordinates(x)
  k <- dplyr::as_tibble(k)
  k <- k[,c(1:3)]
  names(k) <- c(".long", ".lat",  ".subid")
  
  k$`.order` <- 1:nrow(k)
  k$`.id` <- n
  k
}
# Is this one needed??
# #' Get annotation files
# #'
# #' @template subject 
# #' @param label annotation file
# #' @tempalte hemisphere 
# #' @tempalte subjects_dir 
# #' @tempalte output_dir 
# #' @tempalte verbose 
# get_annots <- function(subject = "fsaverage5",
#                        label = "aparc.annot$",
#                        hemisphere = c("rh", "lh"),
#                        subjects_dir = freesurfer::fs_subj_dir(),
#                        output_dir = subjects_dir,
#                        verbose = TRUE){
#   
#   for(sub in subject){
#     
#     # Simplify a bit with a shorter variable
#     dir <- file.path(output_dir, sub)
#     sdir <- file.path(subjects_dir, sub)
#     
#     if(!dir.exists(file.path(dir, "label")))
#       dir.create(file.path(dir, "label"),
#                  recursive = TRUE)
#     
#     labs <- list.files(file.path(sdir, "label"),
#                        pattern = label)
#     labs <- labs[grepl(paste0("^", hemisphere, collapse="|"), labs)]
#     
#     # If subject dir and output dir are not the same
#     # copy over files
#     if(sdir != dir){
#       for(l in labs){
#         j <- file.copy(file.path(sdir, "label", l),
#                        file.path(dir, "label", l))
#       }
#       
#     }
#   }
#   
# }
# 

## quiets concerns of R CMD check
if(getRversion() >= "2.15.1"){
  utils::globalVariables(c("atlas", "surf", "data",
                           "hemi", "i", "j", "k",
                           "x", "y", "z", "r"))
}
