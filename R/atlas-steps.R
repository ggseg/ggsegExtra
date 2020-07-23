
#' Nifti volume to surface
#'
#' @param input_file nifti volume
#' @template output_dir
#' @param projfrac value to mri_vol2surf -projfrac
#' @template verbose
#'
#' @export
atlas_vol2surf <- function(input_file, output_dir, 
                           projfrac = .5, verbose = TRUE){
  if(verbose) cat("Transforming volume to surface files/n")
  
  for(hemi in c("rh", "lh")){
    mri_vol2surf(input_file, 
                 output_file = paste0(output_dir, "template_", hemi, ".mgh"),
                 hemisphere =  hemi,
                 projfrac = projfrac,
                 verbose = verbose)
  }
}

#' Volume to label
#'
#' @param annot_lab annotation label
#' @template ncores
#' @inheritParams atlas_vol2surf 
#' @export
atlas_vol2label <- function(annot_lab, output_dir, verbose, ncores = parallel::detectCores()-2){
  if(verbose) cat("... extracting labels\n")
  
  for(hemi in c("rh", "lh")){
    k <- parallel::mclapply(1:nrow(annot_lab)-1, function(x) 
      mri_vol2label(input_file = paste0(output_dir, "template_", hemi, ".mgh"), 
                    label_id = x, 
                    hemisphere = hemi, 
                    output_dir = paste0(output_dir, "labels"), 
                    verbose = verbose)
    )
  }
  invisible(k)
}

#' Label to ctab
#'
#' @inheritParams atlas_vol2surf 
#'
#' @export
atlas_lab2ctab <- function(output_dir, verbose){
  if(verbose) cat("... making ctab\n")
  
  for(hemi in c("rh", "lh")){
    ll <- list.files(paste0(output_dir, "labels"), 
                     pattern=paste0(hemi,".*label"), 
                     full.names = TRUE)
    
    mris_label2annot(ll,
                     hemisphere = hemi, 
                     ctab = paste0(output_dir, "annots/annots.ctab"), 
                     output_dir = paste0(output_dir, "annots/"), 
                     verbose=verbose)
  }
}


save_atlas <- function(atlas_df_gg, atlas_name, output_dir, verbose){
  if(verbose) cat("\n Saving dataset")
  save(atlas_df_gg,  file=paste0(output_dir, atlas_name, ".rda"))
  
  if(verbose) cat("\n Saving svg")
  p <- ggseg::ggseg(atlas=atlas_df_gg,
                    mapping = ggplot2::aes(fill=area),
                    colour="black",
                    show.legend = FALSE) +
    ggplot2::theme_void()
  
  ggplot2::ggsave(plot = p, device = "svg", 
                  width=14, height = 8, units = "in",
                  filename = paste0(output_dir, atlas_name, ".svg"))
  
  p
}


# make ggseg atlas steps ----

#' Make snapshots through orca and plotly
#'
#' @param ggseg3d_atlas object of class ggseg3d-atlas
#' @template hemisphere 
#' @param surface  Freesurfer surface
#' @param view view
#' @param pb progressbar
#' @template output_dir 

snapshot_brain <- function(ggseg3d_atlas, hemisphere, view, surface, 
                           output_dir, pb = NULL) {
  if(!is.null(pb)) pb$tick()$print()
  p <- ggseg3d::ggseg3d(atlas = ggseg3d_atlas, 
                        hemisphere = hemisphere, 
                        surface = surface)
  p <- ggseg3d::pan_camera(p, paste(hemisphere, view))
  p <- ggseg3d::remove_axes(p)
  
  if(surface == "subcort") p <- ggseg3d::add_glassbrain(p)
  
  withr::with_dir(output_dir,
                  plotly::orca(p,
                               paste0(paste("full", hemisphere, view, sep="_"),
                                      ".png")))
}

snapshot_region <- function(.data,  region, ggseg3d_atlas, hemisphere, 
                            view, surface, output_dir, pb = NULL) {
  
  if(!is.null(pb)) pb$tick()$print()
  
  .data <- dplyr::filter(.data, roi == region)
  .data$p <- 1
  
  p <- ggseg3d::ggseg3d(.data = .data,
                        atlas = ggseg3d_atlas,
                        colour = "p",
                        palette = c("red" = 1),
                        show.legend = FALSE,
                        hemisphere = hemisphere,
                        na.colour = "white",
                        surface = surface)
  
  p <- ggseg3d::pan_camera(p, paste(hemisphere, view))
  p <- ggseg3d::remove_axes(p)
  
  if(surface == "subcort") p <- ggseg3d::add_glassbrain(p)
  
  withr::with_dir(output_dir,
                  plotly::orca(p,
                               paste0(paste(region, hemisphere, view, sep="_"),
                                      ".png")))
}

extract_contours <- function(input_dir, output_dir, step, 
                             verbose = TRUE, 
                             ncores = parallel::detectCores()-2 ) {
  regions <- list.files(input_dir, full.names = TRUE)
  rasterobjs <- lapply(regions, raster::raster)
  
  
  usethis::ui_todo("{step} Extracting contours from regions")
  maks <- raster::cellStats(rasterobjs[[1]], stat = max)
  mins <- raster::cellStats(rasterobjs[[1]], stat = min)
  if(verbose) pb <- utils::txtProgressBar(min = 1,
                                          max = length(rasterobjs),
                                          style = 3)

  contourobjs <- parallel::mclapply(rasterobjs, 
                                    get_contours,
                                    max_val = maks, 
                                    mc.cores = ncores, 
                                    mc.preschedule = FALSE)
  
  kp <- !purrr::map_lgl(contourobjs, is.null)
  
  contourobjsDF <- do.call(rbind, contourobjs[kp])

  # remove very small polygons
  contourobjsDF <- contourobjsDF[sapply(contourobjsDF$geometry, 
                                        function(x) nrow(sf::st_coordinates(x)) > 10),]
  
  save(contourobjsDF,
       file = file.path(output_dir, "contours.rda"))
  usethis::ui_done("contours complete")
  
  invisible(contourobjsDF)
}

smooth_contours <- function(dir, smoothness, step, ncores = parallel::detectCores()-2) {
  usethis::ui_todo("{step} Smoothing contours")
  load(file.path(dir, "contours.rda"))

  for(i in 1:nrow(contourobjsDF)){
    contourobjsDF$geometry[[i]] <- smoothr::smooth(contourobjsDF$geometry[[i]],
                                                   method = "ksmooth",
                                                   smoothness = smoothness)
  }
  
  save(contourobjsDF,
       file = file.path(dir, "contours_smoothed.rda"))
  usethis::ui_done("Smoothing complete")
  
  invisible(contourobjsDF)
}


reduce_vertex <- function(dir, tolerance, step) {
  usethis::ui_todo("{step} Reducing vertexes")
  load(file.path(dir, "contours_smoothed.rda"))
  
  contourobjsDF <- sf::st_simplify(contourobjsDF,
                                   preserveTopology = TRUE,
                                   dTolerance = tolerance)
  save(contourobjsDF,
       file = file.path(dir, "contours_reduced.rda"))
  usethis::ui_done("Vertexes reduced")
  
  invisible(contourobjsDF)
}


make_multipolygon <- function(contourfile) {
  # make contour polygons to multipolygons
  load(contourfile)

  contourobjsDF <- dplyr::group_by(contourobjsDF, filenm)
  contourobjsDF <- dplyr::summarise(contourobjsDF,
                                    geometry = sf::st_combine(geometry))
  contourobjsDF <- dplyr::ungroup(contourobjsDF)
  
  # recalc bbox
  bbx1 <- data.frame(filenm = contourobjsDF$filenm,
                     xmin = NA, ymin = NA, xmax = NA, ymax = NA)
  
  for(i in 1:nrow(contourobjsDF)){
    j <- dplyr::as_tibble(sf::st_coordinates(contourobjsDF[i, ])) 
    j <- tidyr::gather(j, key, val, X, Y) 
    j <- dplyr::group_by(j, key)
    j <- dplyr::summarise_at(j, vars(val), list(Min = min, Max = max))
    
    bbx1[i, 2:5] <-c(j$Min[1], j$Min[2], j$Max[1], j$Max[2])
  }
  
  new_bb <- c(min(bbx1$xmin), min(bbx1$ymin),
              max(bbx1$xmax), max(bbx1$ymax))
  names(new_bb) = c("xmin", "ymin", "xmax", "ymax")
  attr(new_bb, "class") = "bbox"
  
  attr(sf::st_geometry(contourobjsDF), "bbox") = new_bb
  
  return(contourobjsDF)
}

prep_labels <- function(label_file, color_lut, subject, subjects_dir, 
                        output_dir, step="", verbose, ncores = parallel::detectCores()-2 ) {
  usethis::ui_todo("Preparing labels")
  
  # If there is a LUT supplied,
  # get it, reduce labels to only those in the LUT
  if(!is.null(color_lut)){
    colortable <- get_ctab(color_lut)
    labs <- colortable$idx
  }else{
    # Make a color LUT based on already existing FS LUT
    colortable <- get_ctab(file.path(freesurfer::fs_dir(), "FreeSurferColorLUT.txt"))
    colortable <- colortable[1:length(labels), ]
    colortable$label <- "undefined region"
  }


  if(dir.exists(label_file)){
    tmp <- list.files(label_file, pattern="label$", full.names = TRUE)
    labs <- tmp[]
    
  }else if(grepl("\\.mgz$", label_file)){
    usethis::ui_todo("Splitting atlas file into labels")
    
    # Find unique labels from template
    tmp <- unique(c(freesurfer::readmgz(label_file)))
    
    all_combs <- expand.grid(
      label = labs,
      hemi = c("rh", "lh"),
      stringsAsFactors = FALSE
    )
    
    k <- parallel::mcmapply(mri_vol2label,
                            label_id = all_combs$label,
                            hemisphere = all_combs$hemi, 
                            MoreArgs = list(input_file = label_file,
                                            subject = subject,
                                            subjects_dir = subjects_dir,
                                            output_dir = output_dir, 
                                            verbose = verbose),
                            mc.cores = ncores)
    
    all_combs <- all_combs[!as.logical(k),]
    all_combs$label <- sprintf("%04d", all_combs$label)
    
    labs <- list.files(output_dir, pattern=paste0(all_combs$label, collapse="|"), full.names = TRUE)
    
  }else{
    labs <- labels
  }

  # Because LUTS _may_ contain rois with no vertices
  # Reduce to label overlap within the template file
  ll <- readr::parse_number(basename(labs))
  colortable <- colortable[colortable$roi %in% sprintf("%04d", ll), ]

  writeLines(labs, file.path(output_dir, "labels_list.txt"))
  utils::write.table(colortable,  file.path(output_dir, "colortable.tsv"), 
              sep="\t", row.names = FALSE)
  
  usethis::ui_done("labels ready")
  list(labels = labs, colortable = colortable)
}


## quiets concerns of R CMD check
if(getRversion() >= "2.15.1"){
  utils::globalVariables(c("verbose","output_dir", "geometry",
                           "side", "hemi","region", "label",
                           "coords", "X", "Y", "area",
                           "R","G","B","A", "input_file",
                           "projfrac",
                           "."))
}
