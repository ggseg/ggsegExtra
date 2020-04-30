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
                                 cleanup = FALSE #,
                                 #ncores = 1,
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

  # prep paralell processing
  # cl <- parallel::makeCluster(ncores)
  # doParallel::registerDoParallel(cl)
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
  # doParallel::stopImplicitCluster()

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
    i <- 0
    for(r in tmp_atlas$roi){
      for(h in hemi){
        for(view in c("lateral", "medial")){
          i <- i+1
          utils::setTxtProgressBar(pb, i)

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
    regions <- list.files(dirs[4], full.names = TRUE)
    rasterobjs <- lapply(regions, raster::raster)


    usethis::ui_todo("4/7 Extracting contours from regions")
    maks <- raster::cellStats(rasterobjs[[1]], stat = max)
    mins <- raster::cellStats(rasterobjs[[1]], stat = min)
    pb <- utils::txtProgressBar(min = 1,
                                max = length(rasterobjs),
                                style = 3)
    contourobjs <- list()
    for(i in 1:length(rasterobjs)){
      utils::setTxtProgressBar(pb, i)
      contourobjs[[i]] <- get_contours(rasterobjs[[i]] ,
                                       max_val = maks,
                                       verbose = FALSE)

    }

    kp <- !purrr::map_lgl(contourobjs, is.null)

    contourobjsDF <- do.call(rbind, contourobjs[kp])

    #names(contourobjsDF)[1] <- "filenm"
    #contourobjsDF$filenm <- gsub("^X", "", contourobjsDF$filenm )

    save(contourobjsDF,
         file = file.path(dirs[1], "contours.rda"))
    cat("\n")
    usethis::ui_done("Contours complete")
  }

  # smoothing ----
  if(5 %in% steps){
    usethis::ui_todo("5/7 Smoothing contours")
    load(file.path(dirs[1], "contours.rda"))

    for(i in 1:nrow(contourobjsDF)){
      contourobjsDF$geometry[[i]] <- smoothr::smooth(contourobjsDF$geometry[[i]],
                                                     method = "ksmooth",
                                                     smoothness = smoothness)
    }

    save(contourobjsDF,
         file = file.path(dirs[1], "contours_smoothed.rda"))
    usethis::ui_done("Smoothing complete")
  }

  # vertex reduction ----
  if(6 %in% steps){
    usethis::ui_todo("6/7 Reducing vertexes")
    load(file.path(dirs[1], "contours_smoothed.rda"))

    contourobjsDF <- sf::st_simplify(contourobjsDF,
                                     preserveTopology = TRUE,
                                     dTolerance = tolerance)
    save(contourobjsDF,
         file = file.path(dirs[1], "contours_reduced.rda"))
    usethis::ui_done("Vertexes reduced")
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

    # make contour polygons to multipolygons
    load(file.path(dirs[1], "contours_reduced.rda"))

    contourobjsDF <- dplyr::group_by(contourobjsDF, filenm)
    contourobjsDF <- dplyr::summarise(contourobjsDF,
                                      geometry = sf::st_combine(geometry))
    contourobjsDF <- dplyr::ungroup(contourobjsDF)


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
    #atlas_df$geometry <- NULL
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

## quiets concerns of R CMD check
if(getRversion() >= "2.15.1"){
  utils::globalVariables(c("roi", ".subid",
                           ".lat", ".long", ".id",
                           "filenm", "colour", "tmp_dt", "ggseg"))
}
