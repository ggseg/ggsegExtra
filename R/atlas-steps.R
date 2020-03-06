
atlas_vol2surf <- function(infile, outdir, projfrac = .5, verbose){
  if(verbose) cat("Transforming volume to surface files/n")
  
  # templates ----
  for(hemi in c("rh", "lh")){
    mri_vol2surf(infile, 
                 outfile = paste0(outdir, "template_", hemi, ".mgh"),
                 hemisphere =  hemi,
                 projfrac = projfrac,
                 verbose = verbose)
  }
}

atlas_vol2label <- function(annot_lab, outdir, verbose){
  if(verbose) cat("... extracting labels\n")
  
  for(hemi in c("rh", "lh")){
    k <- lapply(1:nrow(annot_lab)-1, function(x) 
      mri_vol2label(infile = paste0(outdir, "template_", hemi, ".mgh"), 
                    label_id = x, 
                    hemisphere = hemi, 
                    outdir = paste0(outdir, "labels"), 
                    verbose = verbose)
    )
  }
  invisible(k)
}

atlas_lab2ctab <- function(outdir, verbose){
  if(verbose) cat("... making ctab\n")
  
  for(hemi in c("rh", "lh")){
    list.files(paste0(outdir, "labels"), 
               pattern=paste0(hemi,".*label"), 
               full.names = TRUE) %>% 
      mris_label2annot(hemisphere = hemi, 
                       ctab = paste0(outdir, "annots/annots.ctab"), 
                       outdir = paste0(outdir, "annots/"), 
                       verbose=verbose)
  }
}

atlas_labelgii <- function(outdir, annotdir){
  for(hemi in c("rh", "lh")){
    freesurfer::mris_convert_annot(
      infile = paste0(freesurfer::fs_subj_dir(),"/fsaverage/surf/",hemi, ".inflated"),
      outfile = paste0(outdir, "/fsaverage_", hemi, ".gii"),
      annot = paste0(annotdir, hemi, ".annot")
    )
  }
}

atlas_tcl <- function(annot_lab, outdir, verbose){
  if(verbose) cat("... rendering labels\n")
  
  for(hemi in c("rh", "lh")){
    jj <- stringr::str_pad(annot_lab$idx, width = 3, "left", "0")
    sapply(jj, run_tcl,
           indir = paste0(outdir, "labels"), 
           hemisphere = hemi, 
           outdir = paste0(outdir, "pics/raw/"), 
           verbose = verbose)
  }
}

atlas_isolate <- function(outdir, dilation = 2, eroding = 2, smoothing = 4, verbose = TRUE){
  if(verbose) cat("... isolating labels\n")
  
  pics <- list.files(pattern="^.h_.+\\.tif", 
                     path=paste0(outdir, "pics/raw"), 
                     full.names = TRUE)
  k <- sapply(pics, 
              isolate_colour, 
              outdir = paste0(outdir, "pics/"), 
              dilation = dilation,
              eroding = eroding, 
              smoothing = smoothing,
              verbose = verbose)
  invisible(k)
}

atlas_raster <- function(indir, verbose = TRUE){
  if(verbose) cat("... rasterizing images\n")
  
  pics <- list.files(pattern="tif", 
                     path=indir, 
                     full.names = TRUE)
  
  lapply(pics, raster::raster)
}

#' @importFrom dplyr '%>%'
atlas_raster2sf <- function(rasterobjs, verbose = TRUE){
  if(verbose) cat("... extracting contours\n")
  
  maks <- raster::cellStats(rasterobjs[[1]], stat = max)
  contourobjs <- lapply(rasterobjs, get_contours, max_val = maks)
  kp <- !purrr::map_lgl(contourobjs, is.null)
  
  contourobjsDF <- do.call(rbind, contourobjs)
  
  pics <- list.files(pattern="tif$", 
                     path=paste0(outdir, "pics/raw"), 
                     full.names = TRUE)
  
  atlas_df <- dplyr::tibble(
    region = stringr::str_remove(basename(pics), "\\.tif")
  ) %>% 
    tidyr::separate(region, c("hemi", "area", "side"), 
                    remove = FALSE) %>% 
    dplyr::filter(kp) %>% 
    dplyr::left_join(dplyr::tibble(region=contourobjsDF$region)) %>% 
    dplyr::group_by(region) %>% 
    dplyr::mutate(subid = dplyr::row_number()) %>% 
    dplyr::ungroup()
  
  atlas_df <- dplyr::bind_cols(dplyr::select(contourobjsDF, -region),
                               atlas_df)
  
  ## Now we need to place them into their own panes
  ## Bounding box for all
  bball <- sf::st_bbox(atlas_df)
  atlas_df <- dplyr::mutate(atlas_df, 
                            geometry = geometry - bball[c("xmin", "ymin")])
  
  atlas_df <- adjust_coords(atlas_df)
  
  graphics::plot(atlas_df)
  
  return(atlas_df)
}


adjust_coords <- function(atlas_df){
  atlas_df_list <- list(
    lh.med = move_hemi_side(atlas_df, 1, 
                            (hemi=="lh" & side=="med")),
    
    rh.med <- move_hemi_side(atlas_df, 2, 
                             (hemi=="rh" & side=="med")),
    
    rh.lat <- move_hemi_side(atlas_df, 3,
                             (hemi=="rh" & side=="lat")),
    
    lh.lat <- dplyr::filter(atlas_df,
                            (hemi=="lh" & side=="lat"))
  )
  
  do.call(rbind, atlas_df_list)
  
}

atlas_sf2gg <- function(atlas_df, atlas_name, verbose = TRUE){
  if(verbose) cat("... turning geometry to data.frame\n")
  
  atlas_df_gg <- dplyr::mutate(
    atlas_df, 
    id = 1:nrow(.),
    coords = purrr::map(geometry, ~(sf::st_coordinates(.x)[, c("X", "Y")])),
    coords = purrr::map(coords, dplyr::as_tibble),
    coords = purrr::map(coords, ~dplyr::mutate(.x, order=1:nrow(.x)))
  )
  
  atlas_df_gg$geometry <- NULL
  atlas_df_gg <- tidyr::unnest(atlas_df_gg, cols = c(coords), .drop=TRUE)
  atlas_df_gg <- dplyr::rename(atlas_df_gg, long=X, lat=Y, label=region)
  
  atlas_df_gg <-  dplyr::mutate(
    atlas_df_gg, 
    atlas = atlas_name, 
    label = ifelse(area == "000", NA, label),
    area = ifelse(area == "000", NA, area),
    hemi = ifelse(hemi == "rh", "right", "left"),
    side = ifelse(side == "lat", "lateral", "medial")
  )
  
  atlas_df_gg <- dplyr::rename(
    atlas_df_gg,
    .id = id,
    .order = order,
    .lat = lat,
    .long = long,
    .order = order
  )
  
  ggseg::as_ggseg_atlas(atlas_df_gg)
}

save_atlas <- function(atlas_df_gg, atlas_name, outdir, verbose){
  if(verbose) cat("\n Saving dataset")
  save(atlas_df_gg,  file=paste0(outdir, atlas_name, ".rda"))
  
  if(verbose) cat("\n Saving svg")
  p <- ggseg::ggseg(atlas=atlas_df_gg,
                    mapping = ggplot2::aes(fill=area),
                    colour="black",
                    show.legend = FALSE) +
    ggplot2::theme_void()
  
  ggplot2::ggsave(plot = p, device = "svg", 
                  width=14, height = 8, units = "in",
                  filename = paste0(outdir, atlas_name, ".svg"))
  
  p
}


move_hemi_side <- function(data, n, predicate){
  tmp <- dplyr::filter(data, {{predicate}}) 
  tmp <- dplyr::mutate(tmp, 
                       geometry = geometry + 
                         c( n * 600, 0))
  return(tmp)
}

## quiets concerns of R CMD check
if(getRversion() >= "2.15.1"){
  utils::globalVariables(c("verbose","outdir", "geometry",
                           "side", "hemi","region", "label",
                           "coords", "X", "Y", "area",
                           "R","G","B","A",
                           "long", "lat","infile",
                           "projfrac",
                           ".", "id"))
}
