
#' Nifti volume to surface
#' 
#' Transform a Nifti volume to a surface
#' file for FreeSurfer. Calls
#' FreeSurfer's \code{mri_vol2surf} for the transformation.
#'
#' @param input_file nifti volume
#' @template output_dir
#' @param projfrac value to mri_vol2surf -projfrac
#' @template verbose
#' @returns nothing, creates surface files
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
#' Turn volumetric files into labels for
#' use in annotations. Calls FreeSurfer's
#' \code{mri_vol2label}.
#'
#' @param annot_lab annotation label
#' @template ncores
#' @inheritParams atlas_vol2surf 
#' @returns invisibly returns the list of labels. 
#' @export
#' @importFrom parallel mclapply
atlas_vol2label <- function(annot_lab, output_dir, verbose, ncores = 2){
  if(verbose) cat("... extracting labels\n")
  
  for(hemi in c("rh", "lh")){
    k <- mclapply(1:nrow(annot_lab)-1, function(x) 
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
#' Create a FreeSurfer colortab based
#' on labels. Calls FreeSurfer's \code{mris_lab2ctab}
#'
#' @inheritParams atlas_vol2surf 
#' @return returns nothing, creates files on the file system
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

#' @noRd
#' @importFrom ggplot2 aes theme_void ggsave
#' @importFrom ggseg ggseg
save_atlas <- function(atlas_df_gg, atlas_name, output_dir, verbose){
  if(verbose) cat("\n Saving dataset")
  save(atlas_df_gg,  file=paste0(output_dir, atlas_name, ".rda"))
  
  if(verbose) cat("\n Saving svg")
  p <- ggseg(atlas=atlas_df_gg,
             mapping = aes(fill=area),
             colour="black",
             show.legend = FALSE) +
    theme_void()
  
  ggsave(plot = p, device = "svg", 
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
#' @noRd
#' @importFrom ggseg3d ggseg3d pan_camera remove_axes add_glassbrain
#' @importFrom plotly orca
#' @importFrom withr with_dir
snapshot_brain <- function(ggseg3d_atlas, hemisphere, view, surface, 
                           output_dir, pb = NULL) {
  if(!has_orca()) stop("Orca (for plotly) not installed, cannot run command", call. = FALSE)
  if(!is.null(pb)) pb$tick()$print()
  p <- ggseg3d(atlas = ggseg3d_atlas, 
               hemisphere = hemisphere, 
               surface = surface)
  p <- pan_camera(p, paste(hemisphere, view))
  p <- remove_axes(p)
  
  if(surface == "subcort") p <- add_glassbrain(p)
  
  with_dir(output_dir,
           orca(p,
                paste0(paste("full", hemisphere, view, sep="_"),
                       ".png")))
}

#' @noRd
#' @importFrom dplyr filter
#' @importFrom ggseg3d ggseg3d pan_camera remove_axes add_glassbrain
#' @importFrom plotly orca
#' @importFrom withr with_dir
snapshot_region <- function(.data,  region, ggseg3d_atlas, hemisphere, 
                            view, surface, output_dir, pb = NULL) {
  
  if(!is.null(pb)) pb$tick()$print()
  
  .data <- filter(.data, roi == region)
  .data$p <- 1
  
  p <- ggseg3d(.data = .data,
               atlas = ggseg3d_atlas,
               colour = "p",
               palette = c("red" = 1),
               show.legend = FALSE,
               hemisphere = hemisphere,
               na.colour = "white",
               surface = surface)
  
  p <- pan_camera(p, paste(hemisphere, view))
  p <- remove_axes(p)
  
  if(surface == "subcort") p <- add_glassbrain(p)
  
  with_dir(output_dir,
           orca(p,
                paste0(paste(region, hemisphere, view, sep="_"),
                       ".png")))
}

#' @noRd
#' @importFrom dplyr bind_rows group_by summarise
#' @importFrom pbmcapply pbmclapply
#' @importFrom raster raster cellStats
#' @importFrom sf st_is_empty st_combine st_as_sf st_crs
extract_contours <- function(input_dir, output_dir, step, 
                             verbose = TRUE, 
                             ncores = 2,
                             skip_existing = TRUE,
                             vertex_size_limits = NULL) {
  
  cat("%%{step} Extracting contours from regions\n")
  
  regions <- list.files(input_dir, full.names = TRUE)
  rasterobjs <- lapply(regions, raster)
  
  maks <- cellStats(rasterobjs[[1]], stat = max)
  get_contours(rasterobjs[[1]], 
               max_val = maks,
               vertex_size_limits = vertex_size_limits, verbose = FALSE)
  
  contourobjs <- pbmclapply(rasterobjs, 
                            get_contours,
                            max_val = maks,
                            mc.cores = ncores,
                            vertex_size_limits = vertex_size_limits,
                            verbose = FALSE,
                            mc.preschedule = FALSE)
  
  kp <- !sapply(contourobjs, is.null)
  contourobjs2 <- contourobjs[kp]
  
  kp <- !sapply(contourobjs2, st_is_empty)
  contourobjs2 <- contourobjs2[kp]
  
  contours <- do.call(bind_rows, 
                      contourobjs2)
  contours <- group_by(contours, filenm)
  contours <- summarise(contours, 
                        geometry = st_combine(geometry))
  contours <- st_as_sf(contours)
  sf::st_crs(contours) <- NA
  
  save(contours,
       file = file.path(output_dir, "contours.rda"))
  cat("... v ... contours complete\n")
  
  invisible(contours)
}

#' @noRd
#' @importFrom smoothr smooth
smooth_contours <- function(dir, smoothness, step, ncores = 2) {
  cat("%%{step} Smoothing contours\n")
  load(file.path(dir, "contours.rda"))
  
  contours <- smooth(contours, 
                     method = "ksmooth", 
                     smoothness = smoothness)
  
  save(contours,
       file = file.path(dir, "contours_smoothed.rda"))
  cat("... v ... smoothing complete\n")
  
  invisible(contours)
}

#' @noRd
#' @importFrom sf st_simplify
reduce_vertex <- function(dir, tolerance, step) {
  cat("%%{step} Reducing vertexes")
  load(file.path(dir, "contours_smoothed.rda"))
  
  contours <- st_simplify(contours,
                          preserveTopology = TRUE,
                          dTolerance = tolerance)
  save(contours,
       file = file.path(dir, "contours_reduced.rda"))
  cat("Vertexes reduced")
  
  invisible(contours)
}

#' @noRd
#' @importFrom dplyr group_by summarise ungroup as_tibble summarise_at
#' @importFrom sf st_combine st_coordinates st_geometry
#' @importFrom tidyr gather
make_multipolygon <- function(contourfile) {
  # make contour polygons to multipolygons
  load(contourfile)
  
  contours <- group_by(contours, filenm)
  contours <- summarise(contours,
                        geometry = st_combine(geometry))
  contours <- ungroup(contours)
  
  # recalc bbox
  bbx1 <- data.frame(filenm = contours$filenm,
                     xmin = NA, ymin = NA, xmax = NA, ymax = NA)
  
  for(i in 1:nrow(contours)){
    j <- as_tibble(st_coordinates(contours[i, ])) 
    j <- gather(j, key, val, X, Y) 
    j <- group_by(j, key)
    j <- summarise_at(j, vars(val), list(Min = min, Max = max))
    
    bbx1[i, 2:5] <- c(j$Min[1], j$Min[2], j$Max[1], j$Max[2])
  }
  
  new_bb <- c(min(bbx1$xmin), min(bbx1$ymin),
              max(bbx1$xmax), max(bbx1$ymax))
  names(new_bb) = c("xmin", "ymin", "xmax", "ymax")
  attr(new_bb, "class") = "bbox"
  
  attr(sf::st_geometry(contours), "bbox") = new_bb
  
  return(contours)
}

#' @noRd
#' @importFrom freesurfer fs_dir readmgz
#' @importFrom pbmcapply pbmcmapply
#' @importFrom readr parse_number
#' @importFrom utils write.table
prep_labels <- function(label_file, color_lut, subject, subjects_dir, 
                        output_dir, step="", verbose, ncores = 2,
                        skip_existing = TRUE) {
  cat("%%Preparing labels")
  
  # If there is a LUT supplied,
  # get it, reduce labels to only those in the LUT
  if(!is.null(color_lut)){
    colortable <- get_ctab(color_lut)
    labs <- colortable$idx
  }else{
    # Make a color LUT based on already existing FS LUT
    colortable <- get_ctab(file.path(fs_dir(), "FreeSurferColorLUT.txt"))
    colortable <- colortable[1:length(labels), ]
    colortable$label <- "undefined region"
  }
  
  # if(dir.exists(label_file)){
  if(file.exists(file.path(output_dir, "labels_list.txt")) & skip_existing ){
    labs <- readLines(file.path(output_dir, "labels_list.txt"))    
    # tmp <- list.files(label_file, pattern="label$", full.names = TRUE)
    # labs <- tmp[]
    # 
  }else if(grepl("\\.mgz$", label_file)){
    cat("%%Splitting atlas file into labels")
    
    # Find unique labels from template
    tmp <- unique(c(readmgz(label_file)))
    
    all_combs <- expand.grid(
      label = labs,
      hemi = c("rh", "lh"),
      stringsAsFactors = FALSE
    )
    
    k <- pbmcmapply(mri_vol2label,
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
  ll <- parse_number(basename(labs))
  colortable <- colortable[colortable$roi %in% sprintf("%04d", ll), ]
  
  writeLines(labs, file.path(output_dir, "labels_list.txt"))
  write.table(colortable,  file.path(output_dir, "colortable.tsv"), 
              sep="\t", row.names = FALSE)
  
  cat("labels ready")
  list(labels = labs, colortable = colortable)
}


## quiets concerns of R CMD check
if(getRversion() >= "2.15.1"){
  globalVariables(c("verbose","output_dir", "geometry",
                    "side", "hemi","region", "label",
                    "coords", "X", "Y", "area",
                    "R","G","B","A", "input_file",
                    "projfrac",
                    "."))
}
