describe("mkdir", {
  it("creates directory", {
    tmp <- withr::local_tempdir()
    new_dir <- file.path(tmp, "test_subdir")

    expect_false(dir.exists(new_dir))
    mkdir(new_dir)
    expect_true(dir.exists(new_dir))
  })

  it("creates nested directories", {
    tmp <- withr::local_tempdir()
    nested <- file.path(tmp, "a", "b", "c")

    mkdir(nested)
    expect_true(dir.exists(nested))
  })

  it("does not error if directory exists", {
    tmp <- withr::local_tempdir()
    expect_no_error(mkdir(tmp))
  })
})


describe("has_magick", {
  it("returns logical", {
    result <- has_magick()
    expect_type(result, "logical")
    expect_length(result, 1)
  })
})


describe("check_magick", {
  it("does not error when ImageMagick is installed", {
    skip_if_no_imagemagick()
    expect_no_error(check_magick())
  })
})


describe("detect_hemi", {
  it("detects left from prefix", {
    expect_equal(detect_hemi("Left-Thalamus"), "left")
    expect_equal(detect_hemi("left_amygdala"), "left")
    expect_equal(detect_hemi("lh.aparc"), "left")
    expect_equal(detect_hemi("lh_region"), "left")
    expect_equal(detect_hemi("L_motor"), "left")
  })

  it("detects right from prefix", {
    expect_equal(detect_hemi("Right-Thalamus"), "right")
    expect_equal(detect_hemi("right_amygdala"), "right")
    expect_equal(detect_hemi("rh.aparc"), "right")
    expect_equal(detect_hemi("rh_region"), "right")
    expect_equal(detect_hemi("R_motor"), "right")
  })

  it("detects from suffix", {
    expect_equal(detect_hemi("cst_left"), "left")
    expect_equal(detect_hemi("cst_right"), "right")
    expect_equal(detect_hemi("tract_lh"), "left")
    expect_equal(detect_hemi("tract_rh"), "right")
  })

  it("detects from anywhere when not strict", {
    expect_equal(detect_hemi("motor_left_area"), "left")
    expect_equal(detect_hemi("rightHemisphere"), "right")
  })

  it("returns NA for ambiguous labels", {
    expect_true(is.na(detect_hemi("brainstem")))
    expect_true(is.na(detect_hemi("corpus_callosum")))
  })

  it("handles NA and empty input", {
    expect_true(is.na(detect_hemi(NA)))
    expect_true(is.na(detect_hemi("")))
  })
})


describe("detect_hemi_vec", {
  it("works on vectors", {
    labels <- c("Left-Thalamus", "Right-Amygdala", "brainstem")
    result <- detect_hemi_vec(labels)
    expect_equal(result, c("left", "right", NA_character_))
  })
})


describe("clean_region_name", {
  it("removes hemisphere prefix and normalizes", {
    expect_equal(clean_region_name("Left-Thalamus"), "thalamus")
    expect_equal(clean_region_name("right_Amygdala"), "amygdala")
    expect_equal(clean_region_name("lh.superior_frontal"), "superior frontal")
  })

  it("converts underscores and dashes to spaces", {
    expect_equal(clean_region_name("superior_frontal"), "superior frontal")
    expect_equal(clean_region_name("pre-central"), "pre central")
  })

  it("can skip hemisphere removal", {
    expect_equal(
      clean_region_name("Left-Thalamus", remove_hemi = FALSE),
      "left thalamus"
    )
  })

  it("can skip normalization", {
    expect_equal(
      clean_region_name("Left-Thalamus", normalize = FALSE),
      "Thalamus"
    )
  })
})


describe("hemi_to_long", {
  it("converts short to long form", {
    expect_equal(hemi_to_long("lh"), "left")
    expect_equal(hemi_to_long("rh"), "right")
  })

  it("returns unchanged for non-short forms", {
    expect_equal(hemi_to_long("left"), "left")
    expect_equal(hemi_to_long("subcort"), "subcort")
  })
})


describe("hemi_to_short", {
  it("converts long to short form", {
    expect_equal(hemi_to_short("left"), "lh")
    expect_equal(hemi_to_short("right"), "rh")
  })

  it("returns unchanged for non-long forms", {
    expect_equal(hemi_to_short("lh"), "lh")
    expect_equal(hemi_to_short("subcort"), "subcort")
  })
})


describe("clean_region_names", {
  it("works on vectors", {
    labels <- c("Left-Thalamus", "right_Amygdala", "lh.superior_frontal")
    result <- clean_region_names(labels)
    expect_equal(result, c("thalamus", "amygdala", "superior frontal"))
  })
})


describe("setup_atlas_dirs", {
  it("creates standard directory structure", {
    tmp <- withr::local_tempdir()
    dirs <- setup_atlas_dirs(tmp, "test_atlas", type = "cortical")

    expect_true(dir.exists(dirs$base))
    expect_true(dir.exists(dirs$snapshots))
    expect_true(dir.exists(dirs$interim))
    expect_true(dir.exists(dirs$masks))
  })

  it("creates additional dirs for subcortical type", {
    tmp <- withr::local_tempdir()
    dirs <- setup_atlas_dirs(tmp, "test_subcort", type = "subcortical")

    expect_true(dir.exists(dirs$labels))
    expect_true(dir.exists(dirs$meshes))
    expect_equal(dirs$snaps, dirs$snapshots)
    expect_equal(dirs$inter, dirs$interim)
  })

  it("handles existing directories without error", {
    tmp <- withr::local_tempdir()
    dirs1 <- setup_atlas_dirs(tmp, "test_atlas")

    expect_no_error({
      dirs2 <- setup_atlas_dirs(tmp, "test_atlas")
    })
    expect_equal(dirs1$base, dirs2$base)
  })
})


describe("build_atlas_components", {
  it("builds core, palette and vertices from atlas data", {
    atlas_data <- data.frame(
      hemi = c("left", "left", "right"),
      region = c("motor", "visual", "motor"),
      label = c("lh_motor", "lh_visual", "rh_motor"),
      colour = c("#FF0000", "#00FF00", "#0000FF"),
      stringsAsFactors = FALSE
    )
    atlas_data$vertices <- list(c(1L, 2L, 3L), c(4L, 5L), c(6L, 7L, 8L))

    result <- build_atlas_components(atlas_data)

    expect_true("core" %in% names(result))
    expect_true("palette" %in% names(result))
    expect_true("vertices_df" %in% names(result))

    expect_equal(nrow(result$core), 3)
    expect_equal(length(result$palette), 3)
    expect_equal(nrow(result$vertices_df), 3)
  })

  it("builds meshes_df when mesh column present", {
    atlas_data <- data.frame(
      hemi = c("left", "right"),
      region = c("thalamus", "thalamus"),
      label = c("Left-Thalamus", "Right-Thalamus"),
      colour = c("#FF0000", "#0000FF"),
      stringsAsFactors = FALSE
    )
    mock_mesh <- list(
      vertices = data.frame(x = 1:3, y = 1:3, z = 1:3),
      faces = data.frame(i = 1, j = 2, k = 3)
    )
    atlas_data$mesh <- list(mock_mesh, mock_mesh)

    result <- build_atlas_components(atlas_data)

    expect_true("meshes_df" %in% names(result))
    expect_equal(nrow(result$meshes_df), 2)
  })

  it("handles duplicate labels in palette", {
    atlas_data <- data.frame(
      hemi = c("left", "left"),
      region = c("motor", "motor"),
      label = c("lh_motor", "lh_motor"),
      colour = c("#FF0000", "#FF0000"),
      stringsAsFactors = FALSE
    )
    atlas_data$vertices <- list(c(1L, 2L), c(3L, 4L))

    result <- build_atlas_components(atlas_data)

    expect_equal(length(result$palette), 1)
    expect_equal(names(result$palette), "lh_motor")
  })
})


describe("parse_contour_filenames", {
  it("parses standard filename format", {
    filenames <- c("region1_lh_lateral", "region2_rh_medial")
    result <- parse_contour_filenames(filenames)

    expect_equal(result$view, c("lateral", "medial"))
    expect_equal(result$hemi_short, c("lh", "rh"))
    expect_equal(result$hemi, c("left", "right"))
    expect_equal(result$label, c("region1", "region2"))
  })

  it("handles underscores in region names", {
    filenames <- c("left_superior_frontal_lh_lateral")
    result <- parse_contour_filenames(filenames)

    expect_equal(result$label, "left_superior_frontal")
    expect_equal(result$hemi_short, "lh")
    expect_equal(result$view, "lateral")
  })

  it("strips file extension before parsing", {
    filenames <- c("region1_lh_lateral.png", "region2_rh_medial.png")
    result <- parse_contour_filenames(filenames)

    expect_equal(result$view, c("lateral", "medial"))
    expect_equal(result$hemi_short, c("lh", "rh"))
    expect_equal(result$hemi, c("left", "right"))
    expect_equal(result$label, c("region1", "region2"))
  })
})


describe("filter_valid_geometries", {
  it("removes empty geometries", {
    sf_obj <- sf::st_sf(
      id = c("a", "b"),
      geometry = sf::st_sfc(
        sf::st_polygon(list(matrix(
          c(0, 0, 1, 0, 1, 1, 0, 0),
          ncol = 2,
          byrow = TRUE
        ))),
        sf::st_polygon()
      )
    )

    result <- filter_valid_geometries(sf_obj)

    expect_equal(nrow(result), 1)
    expect_equal(result$id, "a")
  })

  it("returns empty sf for all-invalid input", {
    sf_obj <- sf::st_sf(
      id = "a",
      geometry = sf::st_sfc(sf::st_polygon())
    )

    result <- filter_valid_geometries(sf_obj)

    expect_equal(nrow(result), 0)
  })

  it("handles already empty sf object", {
    sf_obj <- sf::st_sf(
      id = character(0),
      geometry = sf::st_sfc()
    )

    result <- filter_valid_geometries(sf_obj)

    expect_equal(nrow(result), 0)
  })
})


describe("count_vertices", {
  it("counts vertices in sf geometry", {
    sf_obj <- sf::st_sf(
      id = c("a", "b"),
      geometry = sf::st_sfc(
        sf::st_polygon(list(matrix(
          c(0, 0, 1, 0, 1, 1, 0, 1, 0, 0),
          ncol = 2,
          byrow = TRUE
        ))),
        sf::st_polygon(list(matrix(
          c(0, 0, 2, 0, 2, 2, 0, 2, 0, 0),
          ncol = 2,
          byrow = TRUE
        )))
      )
    )

    result <- count_vertices(sf_obj)

    expect_equal(length(result), 2)
    expect_equal(result[1], 5)
    expect_equal(result[2], 5)
  })
})




describe("get_verbosity", {
  it("returns explicit value when provided", {
    expect_equal(get_verbosity(0), 0L)
    expect_equal(get_verbosity(1), 1L)
    expect_equal(get_verbosity(2), 2L)
  })

  it("converts logical to integer", {
    expect_equal(get_verbosity(TRUE), 1L)
    expect_equal(get_verbosity(FALSE), 0L)
  })

  it("reads from option when explicit value is NULL", {
    withr::local_options(ggsegExtra.verbosity = 2)
    expect_equal(get_verbosity(), 2L)
  })

  it("reads from environment variable when option is NULL", {
    withr::local_options(ggsegExtra.verbosity = NULL)
    withr::local_envvar(GGSEGEXTRA_VERBOSITY = "0")
    expect_equal(get_verbosity(), 0L)
  })

  it("returns default of 1 when nothing is set", {
    withr::local_options(ggsegExtra.verbosity = NULL)
    withr::local_envvar(GGSEGEXTRA_VERBOSITY = NA)
    expect_equal(get_verbosity(), 1L)
  })
})


describe("is_verbose", {
  it("returns TRUE for verbosity >= 1", {
    expect_true(is_verbose(1))
    expect_true(is_verbose(2))
    expect_true(is_verbose(TRUE))
  })

  it("returns FALSE for verbosity 0", {
    expect_false(is_verbose(0))
    expect_false(is_verbose(FALSE))
  })
})


describe("set_verbosity", {
  it("sets verbosity option and returns old value", {
    withr::local_options(ggsegExtra.verbosity = 1)

    old <- set_verbosity(2)
    expect_equal(old, 1L)
    expect_equal(getOption("ggsegExtra.verbosity"), 2L)
  })

  it("errors on invalid level", {
    expect_error(set_verbosity(5), "must be 0, 1, or 2")
    expect_error(set_verbosity(-1), "must be 0, 1, or 2")
  })
})


describe("get_cleanup", {
  it("returns explicit value when provided", {
    expect_true(get_cleanup(TRUE))
    expect_false(get_cleanup(FALSE))
  })

  it("reads from option when explicit value is NULL", {
    withr::local_options(ggsegExtra.cleanup = FALSE)
    expect_false(get_cleanup())
  })

  it("reads from environment variable when option is NULL", {
    withr::local_options(ggsegExtra.cleanup = NULL)
    withr::local_envvar(GGSEGEXTRA_CLEANUP = "false")
    expect_false(get_cleanup())

    withr::local_envvar(GGSEGEXTRA_CLEANUP = "true")
    expect_true(get_cleanup())

    withr::local_envvar(GGSEGEXTRA_CLEANUP = "1")
    expect_true(get_cleanup())

    withr::local_envvar(GGSEGEXTRA_CLEANUP = "0")
    expect_false(get_cleanup())
  })

  it("returns default of TRUE when nothing is set", {
    withr::local_options(ggsegExtra.cleanup = NULL)
    withr::local_envvar(GGSEGEXTRA_CLEANUP = NA)
    expect_true(get_cleanup())
  })
})


describe("get_skip_existing", {
  it("returns explicit value when provided", {
    expect_true(get_skip_existing(TRUE))
    expect_false(get_skip_existing(FALSE))
  })

  it("reads from option when explicit value is NULL", {
    withr::local_options(ggsegExtra.skip_existing = FALSE)
    expect_false(get_skip_existing())
  })

  it("reads from environment variable when option is NULL", {
    withr::local_options(ggsegExtra.skip_existing = NULL)
    withr::local_envvar(GGSEGEXTRA_SKIP_EXISTING = "false")
    expect_false(get_skip_existing())
  })

  it("returns default of TRUE when nothing is set", {
    withr::local_options(ggsegExtra.skip_existing = NULL)
    withr::local_envvar(GGSEGEXTRA_SKIP_EXISTING = NA)
    expect_true(get_skip_existing())
  })
})


describe("get_tolerance", {
  it("returns explicit value when provided", {
    expect_equal(get_tolerance(0.5), 0.5)
    expect_equal(get_tolerance(1), 1)
  })

  it("reads from option when explicit value is NULL", {
    withr::local_options(ggsegExtra.tolerance = 0.75)
    expect_equal(get_tolerance(), 0.75)
  })

  it("reads from environment variable when option is NULL", {
    withr::local_options(ggsegExtra.tolerance = NULL)
    withr::local_envvar(GGSEGEXTRA_TOLERANCE = "0.25")
    expect_equal(get_tolerance(), 0.25)
  })

  it("returns default of 0 when nothing is set", {
    withr::local_options(ggsegExtra.tolerance = NULL)
    withr::local_envvar(GGSEGEXTRA_TOLERANCE = NA)
    expect_equal(get_tolerance(), 0)
  })
})


describe("get_smoothness", {
  it("returns explicit value when provided", {
    expect_equal(get_smoothness(10), 10)
    expect_equal(get_smoothness(2.5), 2.5)
  })

  it("reads from option when explicit value is NULL", {
    withr::local_options(ggsegExtra.smoothness = 15)
    expect_equal(get_smoothness(), 15)
  })

  it("reads from environment variable when option is NULL", {
    withr::local_options(ggsegExtra.smoothness = NULL)
    withr::local_envvar(GGSEGEXTRA_SMOOTHNESS = "20")
    expect_equal(get_smoothness(), 20)
  })

  it("returns default of 5 when nothing is set", {
    withr::local_options(ggsegExtra.smoothness = NULL)
    withr::local_envvar(GGSEGEXTRA_SMOOTHNESS = NA)
    expect_equal(get_smoothness(), 5)
  })
})


describe("make_view_chunks", {
  it("creates correct number of chunks", {
    result <- make_view_chunks(85, 152, 10, "axial")
    expect_s3_class(result, "data.frame")
    expect_true(all(c("name", "type", "start", "end") %in% names(result)))
    expect_equal(nrow(result), 7)
  })

  it("names chunks with type prefix", {
    result <- make_view_chunks(100, 150, 20, "coronal")
    expect_true(all(grepl("^coronal_", result$name)))
    expect_equal(unique(result$type), "coronal")
  })

  it("handles exact division", {
    result <- make_view_chunks(0, 30, 10, "axial")
    expect_equal(result$start, c(0, 10, 20, 30))
    expect_equal(result$end, c(9, 19, 29, 30))
  })

  it("clamps end to hi boundary", {
    result <- make_view_chunks(0, 25, 10, "axial")
    expect_equal(result$end[nrow(result)], 25)
  })
})


describe("create_cortex_slices", {
  it("creates slices matching views", {
    views <- data.frame(
      name = c("axial_1", "coronal_1", "sagittal"),
      type = c("axial", "coronal", "sagittal"),
      start = c(85, 110, 128),
      end = c(95, 120, 128),
      stringsAsFactors = FALSE
    )
    dims <- c(256, 256, 256)

    result <- create_cortex_slices(views, dims)

    expect_s3_class(result, "data.frame")
    expect_equal(nrow(result), 3)
    expect_true(all(c("x", "y", "z", "view", "name") %in% names(result)))
  })

  it("sets z for axial views", {
    views <- data.frame(
      name = "axial_1",
      type = "axial",
      start = 85,
      end = 95,
      stringsAsFactors = FALSE
    )
    dims <- c(256, 256, 256)

    result <- create_cortex_slices(views, dims)

    expect_true(is.na(result$x))
    expect_true(is.na(result$y))
    expect_equal(result$z, 90)
  })

  it("sets y for coronal views", {
    views <- data.frame(
      name = "coronal_1",
      type = "coronal",
      start = 110,
      end = 120,
      stringsAsFactors = FALSE
    )
    dims <- c(256, 256, 256)

    result <- create_cortex_slices(views, dims)

    expect_true(is.na(result$x))
    expect_equal(result$y, 115)
    expect_true(is.na(result$z))
  })

  it("sets hemisphere-specific x for sagittal views", {
    views <- data.frame(
      name = c("sagittal_left", "sagittal_right"),
      type = c("sagittal", "sagittal"),
      start = c(128, 1),
      end = c(256, 128),
      stringsAsFactors = FALSE
    )
    dims <- c(256, 256, 256)

    result <- create_cortex_slices(views, dims)

    expect_equal(result$x[1], round(256 * 0.65))
    expect_equal(result$x[2], round(256 * 0.35))
  })
})


describe("detect_cortex_labels", {
  it("detects aparc labels when present", {
    vol <- array(0L, dim = c(10, 10, 10))
    vol[1:5, , ] <- 1001L
    vol[6:10, , ] <- 2005L

    result <- detect_cortex_labels(vol)

    expect_type(result, "list")
    expect_named(result, c("left", "right"))
    expect_equal(result$left, 1001L)
    expect_equal(result$right, 2005L)
  })

  it("falls back to aseg labels when no aparc", {
    vol <- array(0L, dim = c(10, 10, 10))
    vol[1:5, , ] <- 3L
    vol[6:10, , ] <- 42L

    result <- detect_cortex_labels(vol)

    expect_equal(result$left, 3)
    expect_equal(result$right, 42)
  })
})


describe("extract_hemi_from_view", {
  it("returns NULL for non-sagittal views", {
    expect_null(extract_hemi_from_view("axial", "axial_3"))
    expect_null(extract_hemi_from_view("coronal", "coronal_1"))
  })

  it("returns left for sagittal left views", {
    expect_equal(
      extract_hemi_from_view("sagittal", "sagittal_left"),
      "left"
    )
    expect_equal(
      extract_hemi_from_view("sagittal", "left_sagittal"),
      "left"
    )
  })

  it("returns right for sagittal right views", {
    expect_equal(
      extract_hemi_from_view("sagittal", "sagittal_right"),
      "right"
    )
    expect_equal(
      extract_hemi_from_view("sagittal", "right_sagittal"),
      "right"
    )
  })

  it("returns NULL for sagittal without hemisphere", {
    expect_null(extract_hemi_from_view("sagittal", "sagittal"))
  })
})
