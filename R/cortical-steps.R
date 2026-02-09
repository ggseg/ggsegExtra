# Cortical step functions ----

#' @noRd
cortical_brain_snapshots <- function(
  atlas_3d,
  hemisphere,
  views,
  dirs,
  skip_existing
) {
  snapshot_grid <- expand.grid(
    hemisphere = hemisphere,
    view = views,
    stringsAsFactors = FALSE
  )

  p <- progressor(steps = nrow(snapshot_grid))
  invisible(future_pmap(
    snapshot_grid,
    function(hemisphere, view) {
      snapshot_brain(
        atlas = atlas_3d,
        hemisphere = hemisphere,
        view = view,
        surface = "inflated",
        output_dir = dirs$base,
        skip_existing = skip_existing
      )
      p()
    },
    .options = furrr_options(
      packages = "ggsegExtra",
      globals = c("atlas_3d", "dirs", "skip_existing", "p")
    )
  ))
}


#' @noRd
cortical_region_snapshots <- function(
  atlas_3d,
  components,
  hemisphere,
  views,
  dirs,
  skip_existing
) {
  region_labels <- unique(components$core$label[
    !is.na(components$core$label)
  ])

  region_grid <- expand.grid(
    region_label = region_labels,
    hemisphere = hemisphere,
    view = views,
    stringsAsFactors = FALSE
  )

  region_grid <- region_grid[
    (grepl("^lh_", region_grid$region_label) &
       region_grid$hemisphere == "lh") |
      (grepl("^rh_", region_grid$region_label) &
         region_grid$hemisphere == "rh"),
  ]

  p <- progressor(steps = nrow(region_grid))
  invisible(future_pmap(
    region_grid,
    function(region_label, hemisphere, view) {
      snapshot_region(
        atlas = atlas_3d,
        region_label = region_label,
        hemisphere = hemisphere,
        view = view,
        surface = "inflated",
        output_dir = dirs$snapshots,
        skip_existing = skip_existing
      )
      p()
    },
    .options = furrr_options(
      packages = "ggsegExtra",
      globals = c("atlas_3d", "dirs", "skip_existing", "p")
    )
  ))
}


#' @noRd
cortical_isolate_regions <- function(dirs, skip_existing) {
  ffs <- list.files(dirs$snapshots, full.names = TRUE)
  file_grid <- data.frame(
    input_file = ffs,
    output_file = file.path(dirs$masks, basename(ffs)),
    interim_file = file.path(dirs$processed, basename(ffs))
  )

  p <- progressor(steps = nrow(file_grid))
  invisible(future_pmap(
    file_grid,
    function(input_file, output_file, interim_file) {
      isolate_region(
        input_file = input_file,
        output_file = output_file,
        interim_file = interim_file,
        skip_existing = skip_existing
      )
      p()
    }
  ))
}


#' @noRd
cortical_build_sf <- function(dirs) {
  load_reduced_contours(dirs$base) |>
    layout_cortical_views() |> # nolint: object_usage_linter.
    group_by(view, label) |>
    mutate(geometry = st_combine(geometry)) |>
    ungroup() |>
    select(label, view, geometry) |>
    sf::st_as_sf()
}


# Label atlas step functions ----

#' @noRd
labels_read_files <- function(
  label_files,
  region_names,
  colours,
  default_colours
) {
  p <- progressor(steps = length(label_files))

  all_data <- future_pmap(
    list(
      label_file = label_files,
      i = seq_along(label_files)
    ),
    function(label_file, i) {
      filename <- basename(label_file)

      hemi_short <- if (grepl("^lh\\.", filename)) {
        "lh"
      } else if (grepl("^rh\\.", filename)) {
        "rh"
      } else {
        NA
      }
      hemi <- if (!is.na(hemi_short)) hemi_to_long(hemi_short) else NA

      region <- if (is.null(region_names)) {
        gsub("^[lr]h\\.", "", file_path_sans_ext(filename))
      } else {
        region_names[i]
      }

      label <- if (!is.na(hemi_short)) {
        paste(hemi_short, region, sep = "_")
      } else {
        region
      }
      colour <- if (is.null(colours)) default_colours[i] else colours[i]

      p()
      tibble(
        hemi = hemi,
        region = region,
        label = label,
        colour = colour,
        vertices = list(read_label_vertices(label_file))
      )
    },
    .options = furrr_options(
      packages = "ggsegExtra",
      globals = c("region_names", "colours", "default_colours", "p")
    )
  )

  bind_rows(all_data)
}


#' @noRd
labels_region_snapshots <- function(
  atlas_3d,
  components,
  hemi_short,
  views,
  dirs,
  skip_existing
) {
  region_labels <- unique(
    components$core$label[!is.na(components$core$region)]
  )
  region_grid <- expand.grid(
    region_label = region_labels,
    hemisphere = hemi_short,
    view = views,
    stringsAsFactors = FALSE
  )

  region_grid <- region_grid[
    (grepl("^lh_", region_grid$region_label) &
       region_grid$hemisphere == "lh") |
      (grepl("^rh_", region_grid$region_label) &
         region_grid$hemisphere == "rh") |
      (!grepl("^[lr]h_", region_grid$region_label)),
  ]

  p <- progressor(steps = nrow(region_grid))
  invisible(future_pmap(
    region_grid,
    function(region_label, hemisphere, view) {
      snapshot_region(
        atlas = atlas_3d,
        region_label = region_label,
        hemisphere = hemisphere,
        view = view,
        surface = "inflated",
        output_dir = dirs$snapshots,
        skip_existing = skip_existing
      )
      p()
    },
    .options = furrr_options(
      packages = "ggsegExtra",
      globals = c("atlas_3d", "dirs", "skip_existing", "p")
    )
  ))

  na_grid <- expand.grid(
    hemisphere = hemi_short,
    view = views,
    stringsAsFactors = FALSE
  )
  invisible(future_pmap(
    na_grid,
    function(hemisphere, view) {
      snapshot_na_regions(
        atlas = atlas_3d,
        hemisphere = hemisphere,
        view = view,
        surface = "inflated",
        output_dir = dirs$snapshots,
        skip_existing = skip_existing
      )
    },
    .options = furrr_options(
      packages = "ggsegExtra",
      globals = c("atlas_3d", "dirs", "skip_existing")
    )
  ))
}
