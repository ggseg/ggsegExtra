#' Turn ggseg3d-atlas to ggseg
#'
#' Function will create a data.frame
#' based on a ggseg3d atlas, based on
#' the contours of each segment.
#'
#' @param ggseg3d_atlas object of class ggseg3d-atlas
#' @template steps
#' @template output_dir
#' @template smoothness
#' @template tolerance
#' @template cleanup
#' @param view which sides of the brain to be snapshotted
#'
#' @return data.frame ready for manual cleaning before turning into a proper ggseg3d-atlas
#' @export
#' @importFrom dplyr filter select mutate case_when tibble left_join group_by ungroup one_of
#' @importFrom furrr future_pmap
#' @importFrom ggseg.formats brain_atlas
#' @importFrom ggseg3d is_ggseg3d_atlas
#' @importFrom progressr progressor
#' @importFrom sf st_combine
#' @importFrom tidyr unnest expand_grid separate
#' @importFrom terra flip vect
#' @examples
#' \dontrun{
#'
#' # Create the DKT atlas as found in the FreeSurfer Subjects directory
#' # And output the temporary files to the Desktop.
#' dkt_3d <- make_aparc_2_3datlas(annot = "aparc.DKTatlas",
#'     output_dir = "~/Desktop/")
#' }
make_ggseg3d_2_ggseg <- function(
  ggseg3d_atlas,
  steps = 1:7,
  output_dir = tempdir(),
  view = c("medial", "lateral"),
  tolerance = 0,
  smoothness = 5,
  cleanup = FALSE
) {
  check_magick()

  if (!is_ggseg3d_atlas(ggseg3d_atlas)) {
    cli::cli_abort(c(
      "{.code ggseg3d_atlas} must be a valid ggseg3d-atlas",
      "Check atlas with {.code is_ggseg3d_atlas()}"
    ))
  }

  hemi <- unique(ggseg3d_atlas$hemi)
  surface <- unique(ifelse(hemi == "subcort", "LCBC", "inflated"))
  atlas <- gsub("_3d", "", unique(ggseg3d_atlas$atlas))

  dt <- unnest(ggseg3d_atlas, ggseg_3d)

  pal <- make_palette_ggseg(ggseg3d_atlas)[[1]]

  if (!surface %in% ggseg3d_atlas$surf) {
    cli::cli_abort("Atlas must have surface {.var {surface}}")
  }

  # create output dirs
  dirs <- file.path(output_dir, atlas)
  dirs <- c(dirs, file.path(dirs, c("img", "regions", "masks")))
  j <- sapply(dirs, mkdir)

  if (any(!j) & length(j) == 0) {
    cli::cli_abort(c(
      "Unable to create output directories.",
      "i" = "Check if output directory parent is writeable.",
      "*" = "{.code output_dir} is set to: {.file {output_dir}}"
    ))
  }

  # brain snapshot ----
  if (1 %in% steps) {
    snapshot_grid <- expand_grid(
      hemisphere = hemi,
      view = view
    )
    cli::cli_progress_step("1/7 Full atlas snapshots")

    p <- progressor(
      steps = nrow(snapshot_grid)
    )
    j <- future_pmap(
      snapshot_grid,
      function(hemisphere, view) {
        snapshot_brain(
          hemisphere = hemisphere,
          view = view,
          ggseg3d_atlas = ggseg3d_atlas,
          surface = surface,
          output_dir = dirs[1]
        )
        p()
      }
    )
  }

  # region snapshots ----
  if (2 %in% steps) {
    cli::cli_progress_step("2/7 Region snapshots")
    tmp_atlas <- filter(ggseg3d_atlas, surf == surface)
    tmp_atlas <- unnest(tmp_atlas, ggseg_3d)
    tmp_atlas <- select(tmp_atlas, roi)
    tmp_atlas <- unique(tmp_atlas)

    region_grid <- expand.grid(
      region = tmp_atlas$roi,
      hemisphere = hemi,
      view = view,
      stringsAsFactors = FALSE
    )

    p <- progressor(steps = nrow(region_grid))
    j <- future_pmap(
      region_grid,
      function(region, hemisphere, view) {
        snapshot_region(
          region = region,
          hemisphere = hemisphere,
          view = view,
          surface = surface,
          ggseg3d_atlas = ggseg3d_atlas,
          .data = tmp_atlas,
          output_dir = dirs[2]
        )
        p()
      }
    )
  }

  # isolate region snapshots ----
  if (3 %in% steps) {
    cli::cli_progress_step("3/7 Isolating regions")

    ffs <- list.files(dirs[2], full.names = TRUE)
    file_grid <- data.frame(
      input_file = ffs,
      output_file = file.path(dirs[4], basename(ffs)),
      interim_file = file.path(dirs[3], basename(ffs))
    )

    p <- progressor(steps = nrow(file_grid))
    j <- future_pmap(
      file_grid,
      function(input_file, output_file, interim_file) {
        isolate_region(
          input_file = input_file,
          output_file = output_file,
          interim_file = interim_file
        )
        p()
      }
    )
  }

  # contour extraction ----
  if (4 %in% steps) {
    cli::cli_progress_step("4/7 Extracting contours")
    conts <- extract_contours(
      dirs[4],
      dirs[1],
      step = "4/7"
    )
  }

  # smoothing ----
  if (5 %in% steps) {
    conts <- smooth_contours(
      dirs[1],
      smoothness,
      step = "5/7"
    )
  }

  # vertex reduction ----
  if (6 %in% steps) {
    conts <- reduce_vertex(
      dirs[1],
      tolerance,
      step = "6/7"
    )
  }

  # create df ----
  if (!7 %in% steps) {
    cli::cli_alert_danger("Step 7 skipped, no atlas to return")
    return()
  }

  tmp_atlas <- filter(ggseg3d_atlas, surf == surface)
  tmp_atlas <- unnest(tmp_atlas, ggseg_3d)
  tmp_atlas <- select(tmp_atlas, -mesh, -colour)
  tmp_atlas <- mutate(
    tmp_atlas,
    hemi = case_when(
      is.na(region) ~ NA_character_,
      grepl("^lh_", label) ~ "left",
      grepl("^rh_", label) ~ "right"
    )
  )

  atlas_df <- tibble(
    filenm = gsub("\\.png", "", list.files(dirs[3]))
  )
  atlas_df <- separate(
    atlas_df,
    filenm,
    c("roi", "hemi", "side"),
    remove = FALSE
  )
  atlas_df <- mutate(
    atlas_df,
    hemi = case_when(
      hemi == "lh" ~ "left",
      hemi == "rh" ~ "right",
      TRUE ~ hemi
    )
  )

  atlas_df <- left_join(atlas_df, tmp_atlas, by = c("roi", "hemi"))

  # contours <- make_multipolygon(file.path(dirs[1], "contours_reduced.rda"))
  load(file.path(dirs[1], "contours_reduced.rda"))
  contours <- vect(contours) |>
    flip() |>
    st_as_sf()

  idx <- match(contours$filenm, atlas_df$filenm)
  atlas_df_sf <- atlas_df[idx, ]

  contours$roi <- atlas_df_sf$roi
  contours$hemi <- atlas_df_sf$hemi
  contours$side <- atlas_df_sf$side
  contours$region <- atlas_df_sf$region
  contours$label <- atlas_df_sf$label

  atlas_df_sf <- select(contours, hemi, side, region, label, roi, geometry)

  atlas_df_sf <- adjust_coords_sf(atlas_df_sf)
  atlas_df_sf <- group_by(atlas_df_sf, hemi, side, region, label, roi)
  atlas_df_sf <- mutate(atlas_df_sf, geometry = st_combine(geometry))
  atlas_df_sf <- ungroup(atlas_df_sf)

  dt <- select(dt, -one_of(c("atlas", "surf", "colour", "mesh", "geometry")))
  dt <- unique(dt)

  atlas_df_sf <- suppressMessages(left_join(atlas_df_sf, dt))

  if (cleanup) {
    unlink(dirs[1], recursive = TRUE)
    cli::cli_alert_success("... Output directory removed")
  }

  check_atlas_vertices(atlas_df_sf)

  atlas_df_sf$region <- ifelse(
    grepl("wall|unknown", atlas_df_sf$region, ignore.case = TRUE),
    NA,
    atlas_df_sf$region
  )

  # Add vertices column (empty for now, vertex mapping is complex)
  atlas_df_sf$vertices <- lapply(seq_len(nrow(atlas_df_sf)), function(x) numeric(0))

  atlas <- brain_atlas(
    atlas = gsub("_3d$", "", unique(ggseg3d_atlas$atlas)),
    type = "cortical",
    data = atlas_df_sf
  )

  # Because it is automatic, adding directly above can result in error
  # when the medial wall have not been made NA
  atlas$palette <- pal
  return(atlas)
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
#' @return list with a colour palette
#' @export
#' @importFrom tidyr unnest
#' @importFrom dplyr select distinct
#' @importFrom stats na.omit setNames
#' @examples
#' make_palette_ggseg(dk_3d)
make_palette_ggseg <- function(ggseg3d_atlas) {
  j <- unnest(ggseg3d_atlas, ggseg_3d)
  j <- select(j, region, colour)
  j <- distinct(j)
  j <- na.omit(j)

  k <- list(setNames(
    j$colour,
    j$region
  ))

  names(k) <- gsub("_3d$", "", unique(ggseg3d_atlas$atlas))
  k
}


#' Make ggseg atlas from volumetric template
#'
#' If making an atlas from a non-cortical atlas,
#' volumetric atlases are the best options.
#' Instead of snapshotting images of inflated
#' brain, will snapshot brain slices given x, y, z
#' coordinates for the slices trhoush the `slices`
#' argument.
#'
#' @template subject
#' @template subjects_dir
#' @param label_file a volumetric image containing the labels
#' @template output_dir
#' @param color_lut a file containing color information for the labels
#' @template steps
#' @param skip_existing logical. If slice snapshots already exist, should these be skipped.
#' @param slices a data.frame with columns x, y, z, and view specifying coordinates and view of slice snapshots.
#' @template vertex_size_limits
#' @param dilate numeric. Dilation factor for polygons. Default NULL applies no dilation.
#' @template tolerance
#' @template smoothness
#' @template verbose
#' @template cleanup
#'
#' @return brain-atlas class
#' @export
#' @importFrom dplyr filter left_join case_when select starts_with
#' @importFrom freesurfer have_fs fs_dir
#' @importFrom furrr future_pmap
#' @importFrom ggseg.formats brain_atlas
#' @importFrom magick image_read image_convert image_transparent image_morphology image_write
#' @importFrom progressr progressor
#' @importFrom stats setNames
#' @importFrom tidyr unnest separate
#' @importFrom utils read.table
#' @examples
#' \dontrun{
#'
#'    label_file <- file.path(fs_subj_dir(), subject, "mri/aseg.mgz")
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
make_volumetric_ggseg <- function(
  label_file,
  subject = "fsaverage5",
  subjects_dir = fs_subj_dir(),
  output_dir = tempdir(),
  color_lut = NULL,
  steps = 1:8,
  skip_existing = TRUE,
  slices = data.frame(
    x = c(130, 122, 122),
    y = c(130, 235, 112),
    z = c(130, 100, 106),
    view = c("axial", "sagittal", "coronal"),
    stringsAsFactors = FALSE
  ),
  vertex_size_limits = NULL,
  dilate = NULL,
  tolerance = 0,
  smoothness = 5,
  verbose = TRUE,
  cleanup = FALSE
) {
  check_fs()
  check_magick()

  output_dir <- normalizePath(output_dir, mustWork = FALSE)

  viewport <- match.arg(
    slices$view,
    c('sagittal', 'coronal', 'axial'),
    several.ok = TRUE
  )

  slices <- slices[, c("x", "y", "z", "view")]

  dirs <- list(
    snaps = file.path(output_dir, "snapshots"),
    inter = file.path(output_dir, "interim"),
    masks = file.path(output_dir, "masks"),
    labs = file.path(output_dir, "labels")
  )

  if (is.null(color_lut)) {
    color_lut = file.path(fs_dir(), "ASegStatsLUT.txt")
  }

  for (k in which(!unlist(lapply(dirs, dir.exists)))) {
    mkdir(dirs[[k]])
  }

  if (1 %in% steps) {
    lls <- prep_labels(
      label_file,
      color_lut,
      subject,
      subjects_dir,
      dirs$labs,
      step = "1/8",
      verbose = verbose,
      skip_existing = skip_existing
    )
    llabs <- lls$labels
    colortable <- lls$colortable
  }

  if (2 %in% steps) {
    if (!exists("llabs")) {
      llabs <- readLines(file.path(dirs$labs, "labels_list.txt"))
    }

    labs_df <- data.frame(lab = llabs, stringsAsFactors = FALSE)
    labs_df$data <- lapply(seq_along(llabs), function(x) slices)
    labs_df <- unnest(labs_df, data)

    p <- progressor(steps = nrow(labs_df), label = "2/8 Slice snapshots")
    invisible(future_pmap(
      labs_df,
      function(lab, x, y, z, view) {
        fs_ss_slice(
          lab = lab,
          x = x,
          y = y,
          z = z,
          view = view,
          subjects_dir = subjects_dir,
          subject = subject,
          output_dir = dirs$snaps,
          skip_existing = skip_existing
        )
        p()
      },
      .options = furrr::furrr_options(packages = "ggsegExtra")
    ))
  }

  if (3 %in% steps) {
    files <- list.files(dirs$snaps, full.names = TRUE)

    if (skip_existing) {
      files_exist <- list.files(dirs$inter, full.names = TRUE)

      files <- files[!gsub(dirs$snaps, dirs$inter, files) %in% files_exist]
    }

    if (length(files) > 0) {
      p <- progressor(steps = length(files), label = "3/8 Isolating regions")
      furrr::future_walk(
        files,
        function(f) {
          img <- image_read(f)
          img <- image_convert(img)
          img <- image_transparent(img, color = "black", fuzz = 10)

          if (!is.null(dilate)) {
            img <- image_morphology(
              img,
              method = 'DilateI',
              kernel = 'diamond',
              iterations = dilate
            )
          }

          image_write(
            image = img,
            path = file.path(dirs$inter, basename(f))
          )
          p()
        },
        .options = furrr::furrr_options(
          packages = c("magick"),
          globals = c("dirs", "dilate")
        )
      )
    }
  }

  if (4 %in% steps) {
    cat("4/8 Writing masks to ", dirs$masks, "\n")

    files <- list.files(dirs$inter, full.names = TRUE)

    if (skip_existing) {
      files_exist <- list.files(dirs$masks, full.names = TRUE)

      files <- files[!gsub(dirs$inter, dirs$masks, files) %in% files_exist]
    }

    if (length(files) > 0) {
      p <- progressor(steps = length(files), label = "4/8 Writing masks")
      furrr::future_walk(
        files,
        function(f) {
          cmd <- paste(
            "magick",
            shQuote(f),
            "-alpha extract",
            shQuote(file.path(dirs$masks, basename(f)))
          )
          system(cmd, intern = FALSE)
          p()
        },
        .options = furrr::furrr_options(globals = "dirs")
      )
    }

    cli::cli_alert_success("... masks complete")
  }

  # contour extraction ----
  if (5 %in% steps) {
    conts <- extract_contours(
      dirs$masks,
      output_dir,
      step = "5/8",
      verbose = TRUE,
      vertex_size_limits = vertex_size_limits
    )
  }

  # smoothing ----
  if (6 %in% steps) {
    conts <- smooth_contours(
      output_dir,
      smoothness,
      step = "6/8"
    )
  }

  # vertex reduction ----
  if (7 %in% steps) {
    conts <- reduce_vertex(output_dir, tolerance, step = "7/8")
  }

  # create df ----
  if (8 %in% steps) {
    cat("8/8 Cleaning up data")

    lut <- read.table(
      file.path(dirs$labs, "colortable.tsv"),
      sep = "\t",
      header = TRUE,
      colClasses = "character"
    )
    lut <- lut[c("roi", "label", "color")]

    slices2 <- cbind(
      sapply(slices[, c("x", "y", "z")], sprintf, fmt = "%03d"),
      sapply(
        slices$view,
        function(x) paste0(strsplit(x, "")[[1]][1:5], collapse = "")
      )
    )
    slices2 <- apply(slices2, 1, paste, collapse = "_")

    conts <- make_multipolygon(file.path(output_dir, "contours_reduced.rda"))
    conts$geometry <- conts$geometry * matrix(c(1, 0, 0, -1), 2, 2)
    conts <- separate(conts, filenm, c("view", "file"), 17)
    conts <- filter(conts, view %in% slices2)

    conts <- adjust_coords_sf2(conts)
    conts <- separate(conts, file, c(NA, "hemi", "roi", NA))

    conts <- left_join(conts, lut, by = "roi")

    conts$hemi <- case_when(
      grepl("right|rh", conts$label, ignore.case = TRUE) ~ "right",
      grepl("left|lh", conts$label, ignore.case = TRUE) ~ "left"
    )

    conts <- separate(
      conts,
      view,
      c("mni_x", "mni_y", "mni_z", "side"),
      sep = "_"
    )
    conts$side <- case_when(
      conts$side == "coron" ~ "coronal",
      conts$side == "axial" ~ "axial",
      conts$side == "sagit" ~ "sagittal"
    )
    conts$region <- tolower(gsub("-|_", " ", conts$label))
    conts$region <- gsub("unknown", NA, conts$region)
    conts$region <- gsub("left|right", "", conts$region)
    conts$region <- gsub("^ ", "", conts$region)

    atlas_df <- conts

    dt <- atlas_df[
      !duplicated(atlas_df$region) & !is.na(atlas_df$region),
      c("color", "region")
    ]
    pal <- setNames(dt$color, dt$region)

    atlas_df <- select(atlas_df, -starts_with("mni_"), -color)

    if (cleanup) {
      unlink(dirs[1], recursive = TRUE)
      cat("Output directory removed\n")
    }

    jj <- sum(count_vertices(atlas_df))

    if (jj > 20000) {
      cat(
        "Atlas is complete with",
        jj,
        "vertices, try re-running steps 7:8 with a higher 'tolerance' number.\n"
      )
    } else {
      cat("Atlas complete with", jj, "vertices\n")
    }

    # Add vertices column (empty for now, vertex mapping is complex)
    atlas_df$vertices <- lapply(seq_len(nrow(atlas_df)), function(x) numeric(0))

    atlas <- brain_atlas(
      atlas = substr(basename(label_file), 1, nchar(basename(label_file)) - 4),
      type = "subcortical",
      data = atlas_df
    )
    atlas$palette <- pal

    cat("... cleanup complete\n")

    return(atlas)
  }

  cli::cli_abort("Step 8 is required to return atlas data")
}


## quiets concerns of R CMD check
if (getRversion() >= "2.15.1") {
  globalVariables(c(
    "roi",
    ".subid",
    "color",
    ".lat",
    ".long",
    ".id",
    "mesh",
    "filenm",
    "colour",
    "tmp_dt",
    "ggseg"
  ))
}
