#' Turn ggseg3d-atlas to ggseg
#'
#' Function will create a dataframe
#' based on a ggseg3d atlas, based on
#' the contours of each segment.
#'
#' @param ggseg3d_atlas object of class ggseg3d-atlas
#' @param steps number of 1:6 of which steps to run through
#' @template output_dir
#' @param smoothness smoothing factor, argument to \code{\link[smoothr]{smooth}}
#' @template cleanup
#' @template verbose
#'
#' @return data.frame ready for manual cleaning
#' @export
make_ggseg3d_2_ggseg <- function(ggseg3d_atlas = ggseg3d::dk_3d,
                                 steps = 1:6,
                                 output_dir = tempdir(),
                                 smoothness = 10,
                                 cleanup = FALSE,
                                 verbose = TRUE){
  
  if(!ggseg3d::is_ggseg3d_atlas(ggseg3d_atlas)){
    cat(crayon::red("Atlas must be a valid ggseg3d-atlas\n"),
        "Check atlas with", usethis::ui_code('is_ggseg3d_atlas()')
    )
    stop()
  }
  
  hemi <- unique(ggseg3d_atlas$hemi)
  surface <- unique(ifelse(hemi == "subcort", "LCBC", "inflated"))
  atlas <- gsub("_3d", "", unique(ggseg3d_atlas$atlas))
  
  if(! surface %in% ggseg3d_atlas$surf){
    cat(crayon::red("Atlas must have surface"), crayon::italic(surface), "\n")
    stop()
  }
  
  # create output dirs
  dirs <- file.path(output_dir, atlas)
  dirs <- c(dirs, file.path(dirs, c("img", "regions", "masks")))
  k <- sapply(dirs, dir.exists)
  j <- sapply(dirs[!k], dir.create, recursive = TRUE, showWarnings = FALSE)
  
  if(any(!k) & length(j) == 0)
    stop(paste("Unable to create output directories. Check if output directory parent is writeable.\n",
               "output_dir is set to:", output_dir), call. = FALSE)
  
  # atlas snapshot ----
  if(1 %in% steps){
    usethis::ui_todo("Snapshotting views of entire atlas to {dirs[1]}")
    for(h in hemi){
      for(view in c("lateral", "medial")){
        
        p <- ggseg3d::ggseg3d(atlas = ggseg3d_atlas, hemisphere = h, surface = surface)
        p <- ggseg3d::pan_camera(p, paste(h, view))
        p <- ggseg3d::remove_axes(p)
        
        if(surface == "subcort") p <- ggseg3d::add_glassbrain(p)
        
        withr::with_dir(dirs[1],
                        plotly::orca(p,
                                     paste0(paste("full", h, view, sep="_"),
                                            ".png")))
        
      }
    }
    usethis::ui_done("snapshots complete")
  }
  
  if(1 %in% steps){
    tmp_atlas <- dplyr::filter(ggseg3d_atlas, surf == surface)
    tmp_atlas <- dplyr::slice(tmp_atlas, 1)
    tmp_atlas <- tidyr::unnest(tmp_atlas, ggseg_3d)
    tmp_atlas <- dplyr::select(tmp_atlas, roi)
    
    # region snapshots ----
    usethis::ui_todo("Snapshotting individual regions to {dirs[2]}")
    for(r in tmp_atlas$roi){
      for(h in hemi){
        for(view in c("lateral", "medial")){
          
          tmp_dt <- dplyr::filter(tmp_atlas, roi == r)
          tmp_dt$p <- 1
          
          p <- ggseg3d::ggseg3d(.data = tmp_dt,
                                atlas = ggseg3d_atlas,
                                colour = "p",
                                palette = c("red" = 1),
                                show.legend = FALSE,
                                hemisphere = h,
                                na.colour = "white",
                                surface = surface)
          
          p <- ggseg3d::pan_camera(p, paste(h, view))
          p <- ggseg3d::remove_axes(p)
          
          if(surface == "subcort") p <- ggseg3d::add_glassbrain(p)
          
          withr::with_dir(dirs[2],
                          plotly::orca(p,
                                       paste0(paste(r, h, view, sep="_"),
                                              ".png")))
          
        } # for view
      } # for h
    } # for r
    usethis::ui_done("snapshots complete")
  }
  
  # isolate region snapshots ----
  if(3 %in% steps){
    usethis::ui_todo("Isolating regions to {dirs[4]}")
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
  
  if(4 %in% steps){
    regions <- list.files(dirs[4], full.names = TRUE)
    rasterobjs <- lapply(regions, raster::raster)
    
    # contour extraction ----
    usethis::ui_todo("Extracting contours from regions")
    maks <- raster::cellStats(rasterobjs[[1]], stat = max)
    mins <- raster::cellStats(rasterobjs[[1]], stat = min)
    if(verbose) pb <- utils::txtProgressBar(min = 1,
                                            max = length(rasterobjs),
                                            style = 3)
    contourobjs <- list()
    for(i in 1:length(rasterobjs)){
      if(verbose) utils::setTxtProgressBar(pb, i)
      contourobjs[[i]] <- get_contours(rasterobjs[[i]] ,
                                       max_val = maks,
                                       verbose = FALSE)
      
    }
    
    kp <- !purrr::map_lgl(contourobjs, is.null)
    
    contourobjsDF <- do.call(rbind, contourobjs[kp])
    names(contourobjsDF)[1] <- "filenm"
    contourobjsDF$filenm <- gsub("^X", "", contourobjsDF$filenm )
    contourobjsDF$subid <- 1:nrow(contourobjsDF)
    
    save(contourobjsDF,
         file = file.path(dirs[1], "contours.rda"))
    cat("\n")
    usethis::ui_done("contours complete")
  }
  
  if(5 %in% steps){
    usethis::ui_todo("Smoothing contours")
    load(file.path(dirs[1], "contours.rda"))
    
    for(i in 1:nrow(contourobjsDF)){
      contourobjsDF$geometry[[i]] <- smoothr::smooth(contourobjsDF$geometry[[i]],
                                                     method = "ksmooth",
                                                     smoothness = smoothness)
    }
    save(contourobjsDF,
         file = file.path(dirs[1], "contours_smoothed.rda"))
    usethis::ui_done("smoothing complete")
  }
  
  if(6 %in% steps){
    load(file.path(dirs[1], "contours_smoothed.rda"))
    atlas_df <- dplyr::tibble(
      filenm = gsub("\\.png", "", list.files(dirs[3]))
    )
    atlas_df <- tidyr::separate(atlas_df,
                                filenm, c("roi", "hemi", "side"),
                                remove = FALSE
    )
    # browser()
    tmp_atlas <- dplyr::filter(ggseg3d_atlas, surf == surface)
    tmp_atlas <- dplyr::slice(tmp_atlas, 1)
    tmp_atlas <- tidyr::unnest(tmp_atlas, ggseg_3d)
    tmp_atlas <- dplyr::select(tmp_atlas, roi, region, label)
    
    atlas_df <- dplyr::left_join(atlas_df,
                                 tmp_atlas,
                                 by = c("roi"))
    
    atlas_df <- dplyr::left_join(atlas_df,
                                 contourobjsDF,
                                 by = "filenm")
    
    atlas_df <- dplyr::filter(atlas_df, !is.na(subid))
    
    atlas_df_gg <- dplyr::mutate(
      atlas_df,
      id = 1:nrow(atlas_df),
      coords = purrr::map(geometry, ~(sf::st_coordinates(.x)[, c("X", "Y")])),
      coords = purrr::map(coords, dplyr::as_tibble),
      coords = purrr::map(coords, ~dplyr::mutate(.x, order=1:nrow(.x)))
    )
    
    atlas_df_gg$geometry <- NULL
    atlas_df_gg <- tidyr::unnest(atlas_df_gg,
                                 cols = c(coords))
    
    atlas_df_gg <- adjust_coords(atlas_df_gg)
    
    atlas_df_gg <- dplyr::rename(atlas_df_gg,
                                 long=X, lat=Y)
    
    usethis::ui_done("atlas complete")
    
    if(cleanup){
      unlink(dirs[1], recursive = TRUE)
      usethis::ui_done("Outout directory removed")
    }
    
    atlas_df_gg
  }
}


## quiets concerns of R CMD check
if(getRversion() >= "2.15.1"){
  utils::globalVariables(c("roi", "subid",
                           "filenm"))
}
