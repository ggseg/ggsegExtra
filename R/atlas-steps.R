
#' Nifti volume to surface
#'
#' @param input_file nifti volume
#' @template output_dir
#' @param projfrac value to mri_vol2surf -projfrac
#' @template verbose
#'
# #' @export
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
# #' @export
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
# #' @return
# #' @export
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


move_hemi_side <- function(data, by, predicate){
  tmp <- dplyr::filter(data, {{predicate}}) 
  tmp <- dplyr::mutate(tmp, 
                       X = X + by )
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


## quiets concerns of R CMD check
if(getRversion() >= "2.15.1"){
  utils::globalVariables(c("verbose","output_dir", "geometry",
                           "side", "hemi","region", "label",
                           "coords", "X", "Y", "area",
                           "R","G","B","A",
                           "long", "lat","input_file",
                           "projfrac",
                           ".", "id"))
}