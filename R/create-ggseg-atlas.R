#' Turn ggseg3d-atlas to ggseg
#'
#' Function will create a dataframe
#' based on a ggseg3d atlas, based on
#' the contours of each segment.
#'
#' @param ggseg3d_atlas object of class ggseg3d-atlas
#' @template steps
#' @template output_dir
#' @template ncores
#' @template smoothness
#' @template tolerance
#' @template cleanup
#'
#' @return data.frame ready for manual cleaning
#' @export
make_ggseg3d_2_ggseg <- function(ggseg3d_atlas = ggseg3d::dk_3d,
                                 steps = 1:7,
                                 output_dir = tempdir(),
                                 tolerance = 0,
                                 smoothness = 5,
                                 cleanup = FALSE ,
                                 ncores = parallel::detectCores()-2
){
  
  if(!has_orca()) stop("Orca (for plotly) not installed, cannot run pipeline. See https://github.com/plotly/orca#installation", call. = FALSE)
  if(!has_magick()) stop("ImageMagick not installed, cannot run pipeline. See https://imagemagick.org/script/download.php", call. = FALSE)
  
  if(!ggseg3d::is_ggseg3d_atlas(ggseg3d_atlas)){
    cat(crayon::red("Atlas must be a valid ggseg3d-atlas\n"),
        "Check atlas with", usethis::ui_code('is_ggseg3d_atlas()')
    )
    stop(call. = FALSE)
  }
  
  hemi <- unique(ggseg3d_atlas$hemi)
  surface <- unique(ifelse(hemi == "subcort", "LCBC", "inflated"))
  atlas <- gsub("_3d", "", unique(ggseg3d_atlas$atlas))
  
  dt <- tidyr::unnest(ggseg3d_atlas, ggseg_3d)
  
  pal <- make_palette_ggseg(ggseg3d_atlas)[[1]]
  
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
    
    if(!has_orca())
      stop(call. = FALSE)
    
    usethis::ui_todo("1/7 Snapshotting views of entire atlas to {dirs[1]}")
    
    all <- tidyr::expand_grid(hemi = hemi,
                              view = c("lateral", "medial"))
    j <- parallel::mcmapply(snapshot_brain,
                            hemisphere = all$hemi,
                            view = all$view,
                            MoreArgs = list(
                              ggseg3d_atlas = ggseg3d_atlas, 
                              surface = surface,
                              output_dir = dirs[1]
                            ),
                            
                            mc.cores = ncores, 
                            mc.preschedule = TRUE, 
                            SIMPLIFY = TRUE
    )
    
    
    usethis::ui_done("Snapshots complete")
  }
  
  # region snapshots ----
  if(2 %in% steps){
    if(!has_orca()) stop(call. = FALSE)
    
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
    
    j <- pbmcapply::pbmcmapply(
      snapshot_region,
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
    
    # contours <- make_multipolygon(file.path(dirs[1], "contours_reduced.rda"))
    load(file.path(dirs[1], "contours_reduced.rda"))
    
    idx <- match(contours$filenm, atlas_df$filenm)
    atlas_df_sf <- atlas_df[idx, ]
    
    contours$roi <- atlas_df_sf$roi
    contours$hemi <- atlas_df_sf$hemi
    contours$side <- atlas_df_sf$side
    contours$region <- atlas_df_sf$region
    contours$label <- atlas_df_sf$label
    
    atlas_df_sf <- dplyr::select(contours,
                                 hemi, side, 
                                 region, label, roi, geometry)
    
    atlas_df_sf <- adjust_coords_sf(atlas_df_sf)
    atlas_df_sf <- dplyr::group_by(atlas_df_sf, 
                                   hemi, side, region, label, roi)
    atlas_df_sf <- dplyr::mutate(atlas_df_sf, 
                                 geometry = sf::st_combine(geometry))
    atlas_df_sf <- dplyr::ungroup(atlas_df_sf)
    
    # browser()
    
    if(cleanup){
      unlink(dirs[1], recursive = TRUE)
      usethis::ui_done("Output directory removed")
    }
    
    check_atlas_vertices(atlas_df_sf)
    
    atlas_df_sf$region <- ifelse(grepl("wall|unknown", atlas_df_sf$region, ignore.case = TRUE),
                                 NA, atlas_df_sf$region)
    
    atlas <- ggseg::brain_atlas(atlas = gsub("_3d$", "", unique(ggseg3d_atlas$atlas)),
                                type = "cortical",
                                data = atlas_df_sf)
    
    # Because it is automatic, adding directly above can result in error
    # when the medial wall have not been made NA
    atlas$palette <- pal
    return(atlas)
    
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
#' ready for incorporation to a ggseg-atlas repository
#'
#' @param ggseg3d_atlas ggseg3d-atlas
#' @return list
#' @export
#' @importFrom tidyr unnest
#' @importFrom dplyr select distinct
#' @importFrom stats na.omit setNames
#' @examples
#' make_palette_ggseg(ggseg3d::dk_3d)
make_palette_ggseg <- function(ggseg3d_atlas){
  
  j <- unnest(ggseg3d_atlas, ggseg_3d)
  j <- select(j, region, colour)
  j <- distinct(j)
  j <- na.omit(j)
  
  k <- list(setNames(
    j$colour,
    j$region
  ))
  
  names(k) <- gsub("_3d$", "",
                   unique(ggseg3d_atlas$atlas))
  k
}


#' Make ggseg atlas from volumetric template
#' 
#' 
#'
#' @template subject 
#' @template subjects_dir 
#' @param label_file a volumetric image containing the labels
#' @template output_dir 
#' @param color_lut a file containing color information for the labels
#' @template steps 
#' @param skip_existing logical. If slice snapsnots already exist, should these be skipped.
#' @param slices a data.frame with columns x, y, z, and view specifying coordinates and view of slice snapshots.
#' @template vertex_size_limits 
#' @param dilate numeric. Dilation factor for polygons. Default NULL applies no dilation.
#' @template tolerance 
#' @template ncores 
#' @template smoothness 
#' @template verbose 
#' @template cleanup 
#'
#' @return brain-atlas class
#' @export
#'
#' @examples
#' \dontrun{
#' 
#'    label_file <- file.path(freesurfer::fs_subj_dir(), subject, "mri/aseg.mgz")
#'    slices = data.frame(x=130, y=130, z=130, view="axial", stringsAsFactors = FALSE)
#'    
#'    aseg2 <- make_volumetric_ggseg(
#'       label_file =  label_file,
#'       slices = slices
#'    )
#' 
#'    # Have a look at the atlas
#'    plot(aseg2)
#' }
make_volumetric_ggseg <- function(label_file,
                                  subject = "fsaverage5",
                                  subjects_dir = freesurfer::fs_subj_dir(), 
                                  output_dir = tempdir(),
                                  color_lut = NULL,
                                  steps = 1:8,
                                  skip_existing = TRUE,
                                  slices = data.frame(x=c(130, 122, 122), 
                                                      y=c(130, 235, 112), 
                                                      z=c(130, 100, 106),
                                                      view=c("axial", "sagittal", "coronal"),
                                                      stringsAsFactors = FALSE),
                                  vertex_size_limits = NULL,
                                  dilate = NULL,
                                  tolerance = 0,
                                  ncores = parallel::detectCores()-2,
                                  smoothness = 5,
                                  verbose = TRUE,
                                  cleanup = FALSE
){
  
  if(!freesurfer::have_fs())
    stop("FreeSurfer not installed. Cannot run pipeline. See http://surfer.nmr.mgh.harvard.edu/fswiki/DownloadAndInstall", call. = FALSE)
  if(!has_magick()) 
    stop("ImageMagick not installed, cannot run pipeline. See https://imagemagick.org/script/download.php", call. = FALSE)
  
  
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
  
  if(is.null(color_lut)){
    color_lut = file.path(freesurfer::fs_dir(), "ASegStatsLUT.txt")
  }
  
  for(k in which(!unlist(lapply(dirs, dir.exists)))){
    dir.create(dirs[[k]], recursive = TRUE)
  }
  
  if(1 %in% steps){
    
    lls <- prep_labels(label_file, 
                       color_lut, 
                       subject, subjects_dir, 
                       dirs$labs,
                       step="1/8",
                       verbose, 
                       ncores,
                       skip_existing
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
    
    # fs_ss_slice(labs_df$labs[1], x = labs_df$x[1], y = labs_df$y[1], z = labs_df$z[1], view =  labs_df$view[1], 
    #             subject = subject, subjects_dir = subjects_dir, output_dir = dirs$snaps, skip_existing = skip_existing)
    
    j <- pbmcapply::pbmcmapply(fs_ss_slice,
                               lab = labs_df$labs,
                               x = labs_df$x,
                               y = labs_df$y,
                               z = labs_df$z,
                               view = labs_df$view, 
                               MoreArgs = list(
                                 subjects_dir = subjects_dir, 
                                 subject = subject,
                                 output_dir = dirs$snaps,
                                 skip_existing = skip_existing),
                               mc.cores = ncores, 
                               mc.preschedule = FALSE)
    
    usethis::ui_done("snapshots complete")
  }
  
  if(3 %in% steps){
    usethis::ui_todo("3/8 isolating regions to {dirs$inter}")
    
    files <- list.files(dirs$snaps, full.names = TRUE)
    
    if(skip_existing){
      files_exist <- list.files(dirs$inter, full.names = TRUE)
      
      files <- files[!gsub(dirs$snaps, dirs$inter, files) %in% files_exist]
    }
    
    if(length(files) > 0){
      tmp <- lapply(files, magick::image_read)
      tmp <- lapply(tmp, magick::image_convert)
      
      tmp <- lapply(tmp, magick::image_transparent,
                    color =  "black", fuzz=10)
      
      if(!is.null(dilate)) 
        tmp <- pbmcapply::pbmclapply(tmp, magick::image_morphology, 
                                     method = 'DilateI', 
                                     kernel = 'diamond', 
                                     iter = dilate,
                                     mc.cores = ncores, 
                                     mc.preschedule = FALSE)
      
      k <- pbmcapply::pbmcmapply(magick::image_write,
                                 image = tmp, 
                                 path = file.path(dirs$inter, basename(files)),
                                 mc.cores = ncores, 
                                 mc.preschedule = FALSE)
    }
    
    usethis::ui_done("isolation complete")
  }
  
  if(4 %in% steps){
    usethis::ui_todo("4/8 Writing masks to {dirs$masks}")
    
    files <- list.files(dirs$inter, full.names = TRUE)
    
    if(skip_existing){
      files_exist <- list.files(dirs$mask, full.names = TRUE)
      
      files <- files[!gsub(dirs$inter, dirs$mask, files) %in% files_exist]
    }
    
    if(length(files) > 0){
      cmd <- paste("convert", files,
                   "-alpha extract", file.path(dirs$mask, basename(files))
      )
      
      k <- pbmcapply::pbmclapply(cmd, system, intern = FALSE,
                                 mc.cores = ncores,
                                 mc.preschedule = FALSE) 
    }
    
    usethis::ui_done("masks complete")
  }
  
  # contour extraction ----
  if(5 %in% steps){
    
    if(!has_gdal())
      stop(paste("You do not have GDAL version above", gdal_min(), "installed. \n Cannot extract contrours"))
    
    conts <- extract_contours(dirs$mask, output_dir,
                              step = "5/8", 
                              verbose = TRUE, 
                              ncores = ncores,
                              vertex_size_limits = vertex_size_limits)
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
    
    conts$hemi <- dplyr::case_when(
      grepl("right|rh", conts$label, ignore.case = TRUE) ~ "right",
      grepl("left|lh", conts$label, ignore.case = TRUE) ~ "left"
    )
    
    conts <- tidyr::separate(conts, view, c("mni_x", "mni_y", "mni_z", "side"), sep="_")
    conts$side <- dplyr::case_when(
      conts$side == "coron" ~ "coronal",
      conts$side == "axial" ~ "axial",
      conts$side == "sagit" ~ "sagittal"
    )
    conts$region <- tolower(gsub("-|_", " ", conts$label))
    conts$region <- gsub("unknown", NA, conts$region)
    conts$region <- gsub("left|right", "", conts$region)
    conts$region <- gsub("^ ", "", conts$region)
    
    atlas_df <- conts
    
    dt <- atlas_df[!duplicated(atlas_df$region) & !is.na(atlas_df$region),c("color", "region")]
    pal <- stats::setNames(dt$color, dt$region)
    
    atlas_df <- dplyr::select(atlas_df, -dplyr::starts_with("mni_"), -color)
    
    if(cleanup){
      unlink(dirs[1], recursive = TRUE)
      usethis::ui_done("Output directory removed")
    }
    
    jj <- sum(count_vertices(atlas_df))
    
    if(jj > 20000){
      usethis::ui_todo(paste("Atlas is complete with", jj,
                             "vertices, try re-running steps 7:8 with a higher 'tolerance' number."))
    }else{
      usethis::ui_done(paste("Atlas complete with", jj, "vertices"))
    }
    
    atlas <- ggseg::brain_atlas(atlas = substr(basename(label_file), 1, nchar(basename(label_file))-4),
                                type = "subcortical",
                                data = atlas_df,
                                palette = pal)
    
    usethis::ui_done("cleanup complete")
    
    return(atlas)
  }
  
  stop("Step 8 is required to return atlas data",
       call. = FALSE)
  
}


## quiets concerns of R CMD check
if(getRversion() >= "2.15.1"){
  utils::globalVariables(c("roi", ".subid", "color",
                           ".lat", ".long", ".id", "mesh", 
                           "filenm", "colour", "tmp_dt", "ggseg"))
}
