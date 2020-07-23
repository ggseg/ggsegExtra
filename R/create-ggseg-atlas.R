#' Turn ggseg3d-atlas to ggseg
#'
#' Function will create a dataframe
#' based on a ggseg3d atlas, based on
#' the contours of each segment.
#'
#' @param ggseg3d_atlas object of class ggseg3d-atlas
#' @param steps number of 1:6 of which steps to run through
#' @template output_dir
#' @template ncores
#' @param smoothness smoothing factor, argument to \code{\link[smoothr]{smooth}}
#' @param tolerance tolerance during vertex reduction \code{\link[sf]{st_simplify}}
#' @template cleanup
# #' @template verbose
#'
#' @return data.frame ready for manual cleaning
#' @export
make_ggseg3d_2_ggseg <- function(ggseg3d_atlas = ggseg3d::dk_3d,
                                 steps = 1:6,
                                 output_dir = tempdir(),
                                 tolerance = 0,
                                 smoothness = 10,
                                 cleanup = FALSE ,
                                 ncores = parallel::detectCores()-2#,
                                 #verbose = TRUE
){
  
  if(!ggseg3d::is_ggseg3d_atlas(ggseg3d_atlas)){
    cat(crayon::red("Atlas must be a valid ggseg3d-atlas\n"),
        "Check atlas with", usethis::ui_code('is_ggseg3d_atlas()')
    )
    stop(call. = FALSE)
  }
  
  hemi <- unique(ggseg3d_atlas$hemi)
  surface <- unique(ifelse(hemi == "subcort", "LCBC", "inflated"))
  atlas <- gsub("_3d", "", unique(ggseg3d_atlas$atlas))
  
  if(! surface %in% ggseg3d_atlas$surf){
    cat(crayon::red("Atlas must have surface"), crayon::italic(surface), "\n")
    stop(call. = FALSE)
  }
  
  # create output dirs
  dirs <- file.path(output_dir, atlas)
  dirs <- c(dirs, file.path(dirs, c("img", "regions", "masks")))
  k <- sapply(dirs, dir.exists)
  j <- sapply(dirs[!k], dir.create, recursive = TRUE, showWarnings = FALSE)
  
  if(any(!k) & length(j) == 0){
    cat(crayon::red("Unable to create output directories. Check if output directory parent is writeable.\n",
                    "output_dir is set to:", output_dir, "\n"))
    stop(call. = FALSE)
  }
  
  # brain snapshot ----
  if(1 %in% steps){
    usethis::ui_todo("1/7 Snapshotting views of entire atlas to {dirs[1]}")
    
    all <- tidyr::expand_grid(hemi = hemi,
                              view = c("lateral", "medial"))
    
    pb <- dplyr::progress_estimated(nrow(all))
    purrr::walk2(all$hemi, all$view,
                 ~ snapshot_brain(ggseg3d_atlas, .x, .y,
                                  surface, dirs[1], pb))
    
    usethis::ui_done("Snapshots complete")
  }

  # region snapshots ----
  if(2 %in% steps){
    usethis::ui_todo("2/7 Snapshotting individual regions to {dirs[2]}")
    
    tmp_atlas <- dplyr::filter(ggseg3d_atlas, surf == surface)
    tmp_atlas <- tidyr::unnest(tmp_atlas, ggseg_3d)
    tmp_atlas <- dplyr::select(tmp_atlas, roi)
    tmp_atlas <- unique(tmp_atlas)
    
    pb <- utils::txtProgressBar(min = 1,
                                max = length(hemi)*2*length(tmp_atlas$roi),
                                style = 3)
    
    full_list <- expand.grid(roi = tmp_atlas$roi, 
                             hemi = hemi, 
                             view = c("lateral", "medial"))

    j <- parallel::mcmapply(snapshot_region,
           region = full_list$roi,
           hemisphere = full_list$hemi,
           view = full_list$view,
           MoreArgs = list(
             surface = surface, 
             ggseg3d_atlas = ggseg3d_atlas,
             .data = tmp_atlas,
             output_dir = dirs[2]
           ),
           
           mc.cores = ncores, 
           mc.preschedule = TRUE, 
           SIMPLIFY = TRUE
    )

    usethis::ui_done("Region snapshots complete")
  }
  
  # isolate region snapshots ----
  if(3 %in% steps){
    usethis::ui_todo("3/7 Isolating regions to {dirs[4]}")
    usethis::ui_todo("& writing masks to {dirs[3]}")
    ffs <- list.files(dirs[2], full.names = TRUE)
    ffso <- file.path(dirs[4], basename(ffs))
    ffsi <- file.path(dirs[3], basename(ffs))
    regions <- mapply(isolate_region,
                      input_file = ffs,
                      output_file = ffso,
                      interrim_file = ffsi,
                      SIMPLIFY = FALSE)
    usethis::ui_done("isolation complete")
  }
  
  # contour extraction ----
  if(4 %in% steps){
    conts <- extract_contours(dirs[4], dirs[1], step = "4/7")
  }
  
  # smoothing ----
  if(5 %in% steps){
    conts <- smooth_contours(dirs[1], smoothness, step = "5/7")
  }
  
  # vertex reduction ----
  if(6 %in% steps){
    conts <- reduce_vertex(dirs[1], tolerance, step = "6/7")
  }
  
  # create df ----
  if(7 %in% steps){
    usethis::ui_todo("7/7 Making data frame")
    
    tmp_atlas <- dplyr::filter(ggseg3d_atlas, surf == surface)
    tmp_atlas <- tidyr::unnest(tmp_atlas, ggseg_3d)
    tmp_atlas <- dplyr::select(tmp_atlas, roi, region, label)
    tmp_atlas <- dplyr::mutate(tmp_atlas,
                               hemi = dplyr::case_when(
                                 is.na(region) ~ NA_character_,
                                 grepl("^lh_", label) ~ "left",
                                 grepl("^rh_", label) ~ "right"
                               ))
    
    atlas_df <- dplyr::tibble(
      filenm = gsub("\\.png", "", list.files(dirs[3]))
    )
    atlas_df <- tidyr::separate(atlas_df,
                                filenm, c("roi", "hemi", "side"),
                                remove = FALSE
    )
    
    atlas_df <- dplyr::left_join(atlas_df,
                                 tmp_atlas,
                                 by = c("roi", "hemi"))
    
    contourobjsDF <- make_multipolygon(file.path(dirs[1], "contours_reduced.rda"))
    
    idx <- match(contourobjsDF$filenm, atlas_df$filenm)
    atlas_df_sf <- atlas_df[idx, ]
    
    contourobjsDF$roi <- atlas_df_sf$roi
    contourobjsDF$hemi <- atlas_df_sf$hemi
    contourobjsDF$side <- atlas_df_sf$side
    contourobjsDF$region <- atlas_df_sf$region
    contourobjsDF$label <- atlas_df_sf$label
    contourobjsDF$atlas <- gsub("_3d$", "", unique(ggseg3d_atlas$atlas))
    
    atlas_df_sf <- dplyr::select(contourobjsDF,
                                 atlas, hemi, side, region, label, roi, geometry)
    
    
    atlas_df_sf <- adjust_coords_sf(atlas_df_sf)
    
    
    atlas_df <- dplyr::mutate(
      atlas_df_sf,
      ggseg = purrr::map(geometry, ~(sf::st_coordinates(.x))),
      ggseg = purrr::map(ggseg, dplyr::as_tibble),
      ggseg = purrr::map2(ggseg, 1:nrow(atlas_df_sf),
                          ~ mutate(.x,
                                   .order = 1:nrow(.x),
                                   .id = .y)),
      ggseg = purrr::map(ggseg, ~ rename(.x,
                                         .long = X,
                                         .lat = Y,
                                         .subid = L2) )
    )
    
    if(cleanup){
      unlink(dirs[1], recursive = TRUE)
      usethis::ui_done("Output directory removed")
    }
    
    jj <- nrow(tidyr::unnest(atlas_df, ggseg))
    
    if(jj > 20000){
      usethis::ui_todo(paste("Atlas is complete with", jj,
                             "vertices, try re-running steps 6:7 with a higher 'tolerance' number."))
    }else{
      usethis::ui_done(paste("Atlas complete with", jj, "vertices"))
    }
    
    # for now, remove geometry
    # add it when you have sf functionality improved
    atlas_df$geometry <- NULL
    #atlas_df <- as_tibble(atlas_df)
    
    ggseg::as_ggseg_atlas(atlas_df)
    return(atlas_df)
    
  }else{
    usethis::ui_info("Step 7 skipped, no atlas to return")
    return()
  }
}



#' Create ggseg palette from ggseg3d-atlas
#'
#' atlases in ggseg have palettes based on
#' colours from the paper originally
#' introducing the atlas. These
#' colours are hard-coded into ggseg3d-atlases.
#' This function extracts those and makes a object
#' ready for incporporation to a ggseg-atlas repository
#'
#' @inheritParams make_ggseg3d_2_ggseg
#'
#' @return list
#' @export
#'
#' @examples
#' make_palette_ggseg(ggseg3d::dk_3d)
make_palette_ggseg <- function(ggseg3d_atlas){
  
  j <- dplyr::filter(ggseg3d_atlas, surf == "LCBC")
  j <- tidyr::unnest(j, ggseg_3d)
  j <- dplyr::select(j, region, colour)
  j <- dplyr::distinct(j)
  j <- stats::na.omit(j)
  
  k <- list(stats::setNames(
    j$colour,
    j$region
  ))
  
  names(k) <- gsub("_3d$", "",
                   unique(ggseg3d_atlas$atlas))
  k
}


make_subcort_ggseg <- function(subject = "fsaverage5",
                               subjects_dir = freesurfer::fs_subj_dir(), 
                               label_file = file.path(subjects_dir, subject, "mri/aseg.mgz"),
                               output_dir = "~/Desktop/test/2d",
                               color_lut = file.path(freesurfer::fs_dir(), "ASegStatsLUT.txt"),
                               steps = 1:8,
                               slices = data.frame(x=c(150, 122, 122), 
                                                   y=c(150, 127, 137), 
                                                   z=c(150, 70, 106),
                                                   view=c("axial", "sagittal", "coronal"),
                                                   stringsAsFactors = FALSE),
                               dilate = NULL,
                               tolerance = 0,
                               ncores = parallel::detectCores()-2,
                               smoothness = 5,
                               verbose = TRUE
){
  
  viewport <- match.arg(slices$view,
                        c('sagittal','coronal', 'axial'), 
                        several.ok = TRUE)
  
  slices <- slices[, c("x", "y", "z", "view")]
  
  dirs <- list(
    snaps = file.path(output_dir, "snapshots"),
    inter = file.path(output_dir, "interrim"),
    masks = file.path(output_dir, "masks"),
    labs = file.path(output_dir, "labels")
  )
  
  for(k in which(!unlist(lapply(dirs, dir.exists)))){
    dir.create(dirs[[k]], recursive = TRUE)
  }
  
  if(1 %in% steps){
    lls <- prep_labels(label_file, color_lut, 
                       subject, subjects_dir, 
                       dirs$labs,
                       step="1/8",
                       verbose, ncores
    ) 
    llabs <- lls$labels
    colortable <- lls$colortable
  }
  
  if(2 %in% steps){
    usethis::ui_todo("2/8 Snapshotting views of regions to {dirs$snaps}, ")
    if(!exists("llabs")) llabs <- readLines(file.path(dirs$labs, "labels_list.txt"))
    labs_df <- data.frame(labs = llabs, stringsAsFactors = FALSE)
    labs_df$data <- lapply(1:length(llabs), function(x) slices)
    labs_df <- tidyr::unnest(labs_df, data)
    
    j <- parallel::mcmapply(fs_ss_slice,
                            lab = labs_df$labs,
                            x = labs_df$x,
                            y = labs_df$y,
                            z = labs_df$z,
                            view = labs_df$view, 
                            MoreArgs = list(
                              subjects_dir = subjects_dir, 
                              subject = subject,
                              output_dir = dirs$snaps),
                            mc.cores = ncores, 
                            mc.preschedule = FALSE)
    
    usethis::ui_done("snapshots complete")
  }
  
  if(3 %in% steps){
    usethis::ui_todo("3/8 isolating regions to {dirs$inter}")
    
    files <- list.files(dirs$snaps, full.names = TRUE)
    tmp <- lapply(files, magick::image_read)
    tmp <- lapply(tmp, magick::image_convert)
    
    tmp <- lapply(tmp, magick::image_transparent,
                  color =  "black", fuzz=10)
    
    if(!is.null(dilate)) tmp <- parallel::mclapply(tmp, magick::image_morphology, 
                                                   method = 'DilateI', 
                                                   kernel = 'diamond', 
                                                   iter = dilate,
                                                   mc.cores = ncores, 
                                                   mc.preschedule = FALSE)
    
    k <- parallel::mcmapply(magick::image_write,
                            image = tmp, 
                            path = file.path(dirs$inter, basename(files)),
                            mc.cores = ncores, 
                            mc.preschedule = FALSE)
    
    usethis::ui_done("isolation complete")
  }
  
  if(4 %in% steps){
    usethis::ui_todo("4/8 Writing masks to {dirs$masks}")
    
    files <- list.files(dirs$inter, full.names = TRUE)
    
    cmd <- paste("convert", files,
                 "-alpha extract", file.path(dirs$mask, basename(files))
    )
    
    k <- parallel::mclapply(cmd, system, intern = FALSE,
                            mc.cores = ncores,
                            mc.preschedule = FALSE) 
    usethis::ui_done("masks complete")
  }
  
  # contour extraction ----
  if(5 %in% steps){
    conts <- extract_contours(dirs$mask, output_dir, step = "5/8", verbose = TRUE, ncores = ncores)
  }
  
  # smoothing ----
  if(6 %in% steps){
    conts <- smooth_contours(output_dir, smoothness, step = "6/8", ncores = ncores)
  }
  
  # vertex reduction ----
  if(7 %in% steps){
    conts <- reduce_vertex(output_dir, tolerance, step = "7/8")
  }
  
  # create df ----
  if(8 %in% steps){
    usethis::ui_todo("8/8 Cleaning up data")
    
    lut <- utils::read.table(file.path(dirs$labs, "colortable.tsv"), sep="\t", 
                      header = TRUE, colClasses = "character")
    lut <- lut[c("roi", "label", "color")]

    slices2 <- cbind(sapply(slices[,c("x", "y","z")], sprintf, fmt="%03d"),
                     sapply(slices$view, function(x) paste0(strsplit(x, "")[[1]][1:5], collapse=""))
    )
    slices2 <- apply(slices2, 1, paste, collapse ="_")
    
    conts <- make_multipolygon(file.path(output_dir, "contours_reduced.rda"))
    conts <- tidyr::separate(conts, filenm, c("view", "file"), 17)
    conts <- dplyr::filter(conts, view %in% slices2)

    conts <- adjust_coords_sf2(conts)
    conts <- tidyr::separate(conts, file, c(NA, "hemi", "roi", NA))
    
    conts <- dplyr::left_join(conts, lut, by="roi")

    
    browser()
    usethis::ui_done("cleanup complete")
    
    return(conts)
  }
  
  stop("Step 8 is required to return atlas data")
  
}


## quiets concerns of R CMD check
if(getRversion() >= "2.15.1"){
  utils::globalVariables(c("roi", ".subid",
                           ".lat", ".long", ".id",
                           "filenm", "colour", "tmp_dt", "ggseg"))
}
