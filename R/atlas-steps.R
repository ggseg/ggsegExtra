#' Nifti volume to surface
#'
#' Transform a Nifti volume to a surface
#' file for FreeSurfer. Calls
#' FreeSurfer's `mri_vol2surf` for the transformation.
#'
#' @param input_file nifti volume
#' @template output_dir
#' @param projfrac value to \code{freesurfer mri_vol2surf} -projfrac
#' @returns nothing, creates surface files
#' @export
atlas_vol2surf <- function(
  input_file,
  output_dir,
  projfrac = .5
) {
  hemi <- c("rh", "lh")
  lapply(hemi, function(x) {
    mri_vol2surf(
      input_file,
      output_file = paste0(output_dir, "template_", hemi[x], ".mgh"),
      hemisphere = hemi[x],
      projfrac = projfrac,
      verbose = FALSE
    )
  })
}

#' Volume to label
#'
#' Turn volumetric files into labels for
#' use in annotations. Calls FreeSurfer's
#' `mri_vol2label`.
#'
#' @param annot_lab annotation label
#' @inheritParams atlas_vol2surf
#' @template verbose
#' @returns invisibly returns the list of labels.
#' @export
atlas_vol2label <- function(annot_lab, output_dir, verbose) {
  if (verbose) {
    cat("... extracting labels\n")
  }

  for (hemi in c("rh", "lh")) {
    k <- lapply(
      seq_along(1:nrow(annot_lab) - 1),
      function(x) {
        mri_vol2label(
          input_file = paste0(output_dir, "template_", hemi, ".mgh"),
          label_id = x,
          hemisphere = hemi,
          output_dir = paste0(output_dir, "labels"),
          verbose = verbose
        )
      }
    )
  }
  invisible(k)
}

#' Label to ctab
#'
#' Create a FreeSurfer colortab based
#' on labels. Calls FreeSurfer's `mris_lab2ctab`
#'
#' @inheritParams atlas_vol2surf
#' @template verbose
#' @return returns nothing, creates files on the file system
#' @export
atlas_lab2ctab <- function(output_dir, verbose) {
  if (verbose) {
    cat("... making ctab\n")
  }

  for (hemi in c("rh", "lh")) {
    ll <- list.files(
      paste0(output_dir, "labels"),
      pattern = paste0(hemi, ".*label"),
      full.names = TRUE
    )

    mris_label2annot(
      ll,
      hemisphere = hemi,
      ctab = paste0(output_dir, "annots/annots.ctab"),
      output_dir = paste0(output_dir, "annots/"),
      verbose = verbose
    )
  }
}

#' @noRd
#' @importFrom ggplot2 aes theme_void ggsave
#' @importFrom ggseg ggseg
save_atlas <- function(atlas_df_gg, atlas_name, output_dir, verbose) {
  if (verbose) {
    cat("\n Saving dataset")
  }
  save(atlas_df_gg, file = paste0(output_dir, atlas_name, ".rda"))

  if (verbose) {
    cat("\n Saving svg")
  }
  p <- ggseg(
    atlas = atlas_df_gg,
    mapping = aes(fill = area),
    colour = "black",
    show.legend = FALSE
  ) +
    theme_void()

  ggsave(
    plot = p,
    device = "svg",
    width = 14,
    height = 8,
    units = "in",
    filename = paste0(output_dir, atlas_name, ".svg")
  )

  p
}


# make ggseg atlas steps ----

#' Snapshot full brain with vertex coloring
#'
#' Takes a screenshot of a brain_atlas using vertex-based rendering.
#' Uses ggseg3d's built-in snapshot function.
#'
#' @param atlas brain_atlas object with vertices and colour columns
#' @param hemisphere "lh" or "rh"
#' @param view "lateral" or "medial"
#' @param surface Surface type (e.g., "inflated")
#' @param output_dir Directory to save screenshots
#' @noRd
#' @importFrom ggseg3d ggseg3d pan_camera snapshot_brain
snapshot_brain_unified <- function(
    atlas,
    hemisphere,
    view,
    surface,
    output_dir
) {
  hemi_long <- if (hemisphere == "lh") "left" else "right"

  p <- ggseg3d(
    atlas = atlas,
    hemisphere = hemi_long,
    surface = surface,
    show.legend = FALSE,
    na.colour = "#CCCCCC"
  )
  p <- pan_camera(p, paste(hemisphere, view))

  outfile <- file.path(
    output_dir,
    paste0(paste("full", hemisphere, view, sep = "_"), ".png")
  )

  ggseg3d::snapshot_brain(p, outfile)
}

#' Snapshot single region with vertex coloring
#'
#' Takes a screenshot of a single region highlighted in red,
#' using vertex-based rendering on the unified brain_atlas.
#' Uses ggseg3d's built-in snapshot function.
#'
#' @param atlas brain_atlas object with vertices column
#' @param region_label Label of region to highlight
#' @param hemisphere "lh" or "rh"
#' @param view "lateral" or "medial"
#' @param surface Surface type
#' @param output_dir Directory to save screenshots
#' @noRd
#' @importFrom ggseg3d ggseg3d pan_camera snapshot_brain
snapshot_region_unified <- function(
    atlas,
    region_label,
    hemisphere,
    view,
    surface,
    output_dir
) {
  outfile <- file.path(
    output_dir,
    sprintf("%s_%s_%s.png", region_label, hemisphere, view)
  )

  if (file.exists(outfile)) {
    return()
  }

  hemi_long <- if (hemisphere == "lh") "left" else "right"

  atlas_data <- atlas$data
  atlas_data$colour <- ifelse(
    atlas_data$label == region_label,
    "#FF0000",
    "#FFFFFF"
  )

  highlight_atlas <- ggseg.formats::brain_atlas(
    atlas = atlas$atlas,
    type = atlas$type,
    data = atlas_data
  )

  p <- ggseg3d(
    atlas = highlight_atlas,
    hemisphere = hemi_long,
    surface = surface,
    show.legend = FALSE,
    na.colour = "#FFFFFF"
  )
  p <- pan_camera(p, paste(hemisphere, view))

  ggseg3d::snapshot_brain(p, outfile)
}

#' Make snapshots using webshot2 (legacy)
#'
#' @param ggseg3d_atlas object of class ggseg3d-atlas
#' @template hemisphere
#' @param surface  Freesurfer surface
#' @param view view
#' @param pb progressbar
#' @template output_dir
#' @noRd
#' @importFrom ggseg3d ggseg3d pan_camera add_glassbrain
snapshot_brain <- function(
  ggseg3d_atlas,
  hemisphere,
  view,
  surface,
  output_dir
) {
  p <- ggseg3d(
    atlas = ggseg3d_atlas,
    hemisphere = hemisphere,
    surface = surface
  )
  p <- pan_camera(p, paste(hemisphere, view))

  if (surface == "subcort") {
    p <- add_glassbrain(p)
  }

  outfile <- file.path(
    output_dir,
    paste0(paste("full", hemisphere, view, sep = "_"), ".png")
  )
  save_plotly_png(p, outfile)
}

#' @noRd
#' @importFrom dplyr filter
#' @importFrom ggseg3d ggseg3d pan_camera add_glassbrain
snapshot_region <- function(
  .data,
  region,
  ggseg3d_atlas,
  hemisphere,
  view,
  surface,
  output_dir
) {
  outfile <- file.path(
    output_dir,
    sprintf("%s_%s_%s.png", region, hemisphere, view)
  )
  cli::cli_alert_info("{.file {outfile}}")
  if (file.exists(outfile)) {
    return()
  }

  .data <- filter(.data, roi == region)
  .data$p <- 1

  p <- ggseg3d(
    .data = .data,
    atlas = ggseg3d_atlas,
    colour = "p",
    palette = c("red" = 1),
    show.legend = FALSE,
    hemisphere = hemisphere,
    na.colour = "white",
    surface = surface
  )

  p <- pan_camera(p, paste(hemisphere, view))

  if (surface == "subcort") {
    p <- add_glassbrain(p)
  }

  save_plotly_png(p, outfile)
}

#' @noRd
#' @importFrom dplyr bind_rows group_by summarise
#' @importFrom furrr future_map furrr_options
#' @importFrom progressr progressor
#' @importFrom terra rast global
#' @importFrom sf st_is_empty st_combine st_as_sf st_make_valid
#' @importFrom tools file_path_sans_ext
extract_contours <- function(
  input_dir,
  output_dir,
  verbose = TRUE,
  step = "",
  skip_existing = TRUE,
  vertex_size_limits = NULL
) {
  regions <- list.files(input_dir, full.names = TRUE)
  region_names <- file_path_sans_ext(basename(regions))

  # Binary masks have max value of 1 for non-blank images
  # Find the actual max by checking a few files until we find a non-blank one
  maks <- 0
  for (f in regions[1:min(10, length(regions))]) {
    r <- rast(f)
    m <- global(r, fun = "max", na.rm = TRUE)[1, 1]
    if (m > maks) maks <- m
    if (maks > 0) break
  }
  if (maks == 0) maks <- 1

  p <- progressor(steps = length(regions), label = paste(step, "Extracting contours"))
  contourobjs <- furrr::future_map(
    regions,
    function(region_file) {
      r <- rast(region_file)
      result <- get_contours(
        r,
        max_val = maks,
        vertex_size_limits = vertex_size_limits,
        verbose = FALSE
      )
      p()
      result
    },
    .options = furrr::furrr_options(
      packages = c("terra", "ggsegExtra"),
      globals = c("maks", "vertex_size_limits")
    )
  )
  names(contourobjs) <- region_names

  kp <- !sapply(contourobjs, is.null)
  contourobjs2 <- contourobjs[kp]
  #kp <- !sapply(contourobjs2, st_is_empty)
  #contourobjs2 <- contourobjs2[kp]

  contours <- bind_rows(contourobjs2, .id = "filenm")
  contours <- group_by(contours, filenm)
  contours <- summarise(contours, geometry = st_combine(geometry))
  contours <- st_as_sf(contours)
  contours <- st_make_valid(contours)

  save(contours, file = file.path(output_dir, "contours.rda"))

  invisible(contours)
}

#' @noRd
#' @importFrom smoothr smooth
smooth_contours <- function(dir, smoothness, step) {
  load(file.path(dir, "contours.rda"))
  contours <- smooth(contours, method = "ksmooth", smoothness = smoothness)

  save(contours, file = file.path(dir, "contours_smoothed.rda"))

  invisible(contours)
}

#' @noRd
#' @importFrom sf st_simplify
reduce_vertex <- function(dir, tolerance, step = "") {
  cli::cli_progress_step("{step} Reducing vertices")
  load(file.path(dir, "contours_smoothed.rda"))
  contours <- st_simplify(
    contours,
    preserveTopology = TRUE,
    dTolerance = tolerance
  )
  save(contours, file = file.path(dir, "contours_reduced.rda"))

  invisible(contours)
}

#' @noRd
#' @importFrom dplyr group_by summarise ungroup as_tibble
#' @importFrom sf st_combine st_coordinates st_geometry
#' @importFrom tidyr gather
make_multipolygon <- function(contourfile) {
  # make contour polygons to multipolygons
  load(contourfile)

  contours <- group_by(contours, filenm)
  contours <- summarise(contours, geometry = st_combine(geometry))
  contours <- ungroup(contours)

  # recalc bbox
  bbx1 <- data.frame(
    filenm = contours$filenm,
    xmin = NA,
    ymin = NA,
    xmax = NA,
    ymax = NA
  )

  for (i in 1:nrow(contours)) {
    j <- as_tibble(st_coordinates(contours[i, ]))
    j <- gather(j, key, val, X, Y)
    j <- group_by(j, key)
    j <- summarise(j, Min = min(val), Max = max(val))

    bbx1[i, 2:5] <- c(j$Min[1], j$Min[2], j$Max[1], j$Max[2])
  }

  new_bb <- c(min(bbx1$xmin), min(bbx1$ymin), max(bbx1$xmax), max(bbx1$ymax))
  names(new_bb) = c("xmin", "ymin", "xmax", "ymax")
  attr(new_bb, "class") = "bbox"

  attr(sf::st_geometry(contours), "bbox") = new_bb

  return(contours)
}

#' @noRd
#' @importFrom freesurfer fs_dir read_mgz
#' @importFrom furrr future_pmap
#' @importFrom progressr progressor
#' @importFrom readr parse_number
#' @importFrom utils write.table
prep_labels <- function(
  label_file,
  color_lut,
  subject,
  subjects_dir,
  output_dir,
  step = "",
  verbose,
  skip_existing = FALSE,
  ...
) {
  # If there is a LUT supplied,
  # get it, reduce labels to only those in the LUT
  if (!is.null(color_lut)) {
    colortable <- get_ctab(color_lut)
    labs <- colortable$idx
  } else {
    # Make a color LUT based on already existing FS LUT
    colortable <- get_ctab(file.path(fs_dir(), "FreeSurferColorLUT.txt"))
    colortable <- colortable[1:length(labels), ]
    colortable$label <- "undefined region"
  }

  labels_list <- file.path(output_dir, "labels_list.txt")

  if (file.exists(labels_list) && skip_existing) {
    labs <- readLines(labels_list)
  } else if (grepl("\\.mgz$", label_file)) {
    # Find unique labels from template
    labs <- unique(c(read_mgz(label_file)))

    all_combs <- expand.grid(
      label_id = labs,
      hemisphere = c("rh", "lh"),
      stringsAsFactors = FALSE
    )

    p <- progressor(steps = nrow(all_combs), label = "1/8 Extracting labels")
    k <- future_pmap(
      all_combs,
      function(label_id, hemisphere) {
        result <- purrr::possibly(mri_vol2label, otherwise = NULL)(
          label_id = label_id,
          hemisphere = hemisphere,
          input_file = label_file,
          subject = subject,
          subjects_dir = subjects_dir,
          output_dir = output_dir,
          verbose = verbose
        )
        p()
        result
      }
    )

    not_empty <- !sapply(k, is.null)
    all_combs <- all_combs[not_empty, ]
    all_combs$label_id <- sprintf("%04d", all_combs$label_id)

    labs <- list.files(
      output_dir,
      pattern = paste0(all_combs$label_id, collapse = "|"),
      full.names = TRUE
    )
  }

  # Because LUTS _may_ contain rois with no vertices
  # Reduce to label overlap within the template file
  ll <- parse_number(basename(as.character(labs)))
  colortable <- colortable[colortable$roi %in% sprintf("%04d", ll), ]

  writeLines(
    as.character(labs),
    file.path(output_dir, "labels_list.txt")
  )

  write.table(
    colortable,
    file.path(output_dir, "colortable.tsv"),
    sep = "\t",
    row.names = FALSE
  )

  list(
    labels = labs,
    colortable = colortable
  )
}


## quiets concerns of R CMD check
if (getRversion() >= "2.15.1") {
  globalVariables(c(
    "verbose",
    "output_dir",
    "geometry",
    "side",
    "hemi",
    "region",
    "label",
    "coords",
    "X",
    "Y",
    "area",
    "R",
    "G",
    "B",
    "A",
    "input_file",
    "projfrac",
    "."
  ))
}
