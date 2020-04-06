
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
#' @inheritParams atlas_vol2surf 
#' @export
atlas_vol2label <- function(annot_lab, output_dir, verbose){
  if(verbose) cat("... extracting labels\n")
  
  for(hemi in c("rh", "lh")){
    k <- lapply(1:nrow(annot_lab)-1, function(x) 
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
  
  p <- ggseg3d::ggseg3d(.data = tmp_dt,
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

## quiets concerns of R CMD check
if(getRversion() >= "2.15.1"){
  utils::globalVariables(c("verbose","output_dir", "geometry",
                           "side", "hemi","region", "label",
                           "coords", "X", "Y", "area",
                           "R","G","B","A", "input_file",
                           "projfrac",
                           "."))
}
