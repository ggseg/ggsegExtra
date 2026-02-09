describe("create_tract_atlas", {
  it("creates atlas from coordinate matrices", {
    tracts <- list(
      cst_left = matrix(c(1:20, rep(0, 40)), ncol = 3),
      cst_right = matrix(c(1:20, rep(1, 40)), ncol = 3)
    )

    atlas <- create_tract_atlas(
      input_tracts = tracts,
      steps = 1,
      verbose = FALSE
    )

    expect_s3_class(atlas, "brain_atlas")
    expect_equal(atlas$type, "tract")
    expect_false(is.null(atlas$data$centerlines))
    expect_equal(nrow(atlas$data$centerlines), 2)
  })

  it("assigns correct labels", {
    tracts <- list(
      cst_left = matrix(c(1:20, rep(0, 40)), ncol = 3),
      cst_right = matrix(c(1:20, rep(1, 40)), ncol = 3)
    )

    atlas <- create_tract_atlas(
      input_tracts = tracts,
      steps = 1,
      verbose = FALSE
    )

    expect_true("cst_left" %in% atlas$core$label)
    expect_true("cst_right" %in% atlas$core$label)
  })

  it("accepts custom names and colours via input_lut", {
    tracts <- list(
      matrix(c(1:20, rep(0, 40)), ncol = 3),
      matrix(c(1:20, rep(1, 40)), ncol = 3)
    )

    custom_lut <- data.frame(
      region = c("Tract A", "Tract B"),
      hex = c("#FF0000", "#00FF00")
    )

    atlas <- create_tract_atlas(
      input_tracts = tracts,
      input_lut = custom_lut,
      steps = 1,
      verbose = FALSE
    )

    expect_true("Tract A" %in% atlas$core$label)
    expect_true("Tract B" %in% atlas$core$label)
    expect_true("#FF0000" %in% atlas$palette)
    expect_true("#00FF00" %in% atlas$palette)
  })

  it("creates valid centerline structure for each tract", {
    tracts <- list(
      tract1 = matrix(c(1:20, rep(0, 40)), ncol = 3)
    )

    atlas <- create_tract_atlas(
      input_tracts = tracts,
      steps = 1,
      verbose = FALSE
    )

    expect_true(all(c("label", "points") %in% names(atlas$data$centerlines)))
    points <- atlas$data$centerlines$points[[1]]
    expect_true(all(c("x", "y", "z") %in% colnames(points)))
  })

  it("stores tube parameters", {
    tract <- list(
      tract1 = matrix(c(1:10, rep(0, 20)), ncol = 3)
    )

    atlas <- create_tract_atlas(
      tract,
      tube_radius = 3.5,
      tube_segments = 12,
      steps = 1,
      verbose = FALSE
    )

    expect_equal(atlas$data$tube_radius, 3.5)
    expect_equal(atlas$data$tube_segments, 12L)
  })

  it("can render with ggseg3d", {
    tracts <- list(
      cst_left = matrix(c(1:20, rep(0, 40)), ncol = 3),
      cst_right = matrix(c(1:20, rep(1, 40)), ncol = 3)
    )

    atlas <- create_tract_atlas(
      input_tracts = tracts,
      steps = 1,
      verbose = FALSE
    )

    expect_no_error({
      p <- ggseg3d::ggseg3d(atlas = atlas, hemisphere = "subcort")
    })
  })
})


describe("tract_read_input", {
  it("reads from named list", {
    tracts <- list(
      cst = matrix(1:9, ncol = 3),
      af = matrix(10:18, ncol = 3)
    )

    result <- tract_read_input(tracts, NULL)

    expect_equal(result$tract_names, c("cst", "af"))
    expect_equal(length(result$streamlines_data), 2)
  })

  it("generates default names for unnamed list", {
    tracts <- list(
      matrix(1:9, ncol = 3),
      matrix(10:18, ncol = 3)
    )

    result <- tract_read_input(tracts, NULL)

    expect_equal(result$tract_names, c("tract_1", "tract_2"))
  })

  it("errors when files not found", {
    expect_error(
      tract_read_input(c("nonexistent.trk"), NULL),
      "not found"
    )
  })

  it("errors when no data provided", {
    expect_error(
      tract_read_input(list(), NULL),
      "No tract data"
    )
  })
})


describe("tract_build_core", {
  it("builds core, palette, and centerlines", {
    meshes_list <- list(
      cst_left = list(
        metadata = list(
          centerline = matrix(
            1:9, ncol = 3,
            dimnames = list(NULL, c("x", "y", "z"))
          ),
          tangents = matrix(1:9, ncol = 3)
        )
      ),
      cst_right = list(
        metadata = list(
          centerline = matrix(
            10:18, ncol = 3,
            dimnames = list(NULL, c("x", "y", "z"))
          ),
          tangents = matrix(10:18, ncol = 3)
        )
      )
    )
    colours <- c("#FF0000", "#00FF00")

    result <- tract_build_core(
      meshes_list, colours, c("cst_left", "cst_right")
    )

    expect_equal(nrow(result$core), 2)
    expect_equal(length(result$palette), 2)
    expect_equal(nrow(result$centerlines_df), 2)
    expect_equal(result$atlas_name, "tracts")
  })

  it("uses single tract name as atlas_name", {
    meshes_list <- list(
      cst = list(
        metadata = list(
          centerline = matrix(
            1:9, ncol = 3,
            dimnames = list(NULL, c("x", "y", "z"))
          ),
          tangents = matrix(1:9, ncol = 3)
        )
      )
    )

    result <- tract_build_core(meshes_list, "#FF0000", "cst")

    expect_equal(result$atlas_name, "cst")
  })
})


describe("create_tract_atlas pipeline flow", {
  it("step 1 calls tract_read_input and tract_create_meshes", {
    read_called <- FALSE
    mesh_called <- FALSE
    test_dir <- withr::local_tempdir()
    local_mocked_bindings(
      tract_read_input = function(input_tracts, tract_names) {
        read_called <<- TRUE
        list(
          streamlines_data = list(t1 = matrix(1:30, ncol = 3)),
          tract_names = "t1"
        )
      },
      detect_coords_are_voxels = function(...) TRUE,
      tract_create_meshes = function(...) {
        mesh_called <<- TRUE
        list(t1 = list(
          metadata = list(
            centerline = matrix(
              1:9, ncol = 3,
              dimnames = list(NULL, c("x", "y", "z"))
            ),
            tangents = matrix(1:9, ncol = 3)
          )
        ))
      },
      tract_build_core = function(...) {
        list(
          core = data.frame(
            hemi = "midline", region = "t1", label = "t1",
            stringsAsFactors = FALSE
          ),
          palette = c(t1 = "#FF0000"),
          centerlines_df = data.frame(label = "t1"),
          atlas_name = "t1"
        )
      },
      brain_atlas = function(...) structure(list(...), class = "brain_atlas"),
      tract_data = function(...) list(...),
      preview_atlas = function(...) invisible(NULL),
      setup_atlas_dirs = function(...) {
        list(
          base = test_dir, snaps = test_dir,
          processed = test_dir, masks = test_dir
        )
      },
      load_or_run_step = function(step, steps, ...) {
        list(run = step %in% steps, data = list())
      }
    )

    withr::local_options(ggsegExtra.output_dir = withr::local_tempdir())

    atlas <- create_tract_atlas(
      input_tracts = list(t1 = matrix(1:30, ncol = 3)),
      steps = 1,
      verbose = FALSE
    )

    expect_true(read_called)
    expect_true(mesh_called)
  })

  it("errors when input_aseg is NULL for steps beyond 1", {
    test_dir <- withr::local_tempdir()
    cached <- list(
      streamlines_data = list(t1 = matrix(1:30, ncol = 3)),
      centerlines_df = data.frame(label = "t1"),
      core = data.frame(
        hemi = "midline", region = "t1", label = "t1",
        stringsAsFactors = FALSE
      ),
      palette = c(t1 = "#FF0000"),
      atlas_name = "t1",
      tube_radius = 5,
      tube_segments = 8
    )

    local_mocked_bindings(
      setup_atlas_dirs = function(...) {
        list(
          base = test_dir, snaps = test_dir,
          processed = test_dir, masks = test_dir
        )
      },
      load_or_run_step = function(step, steps, ...) {
        if (step == 1L) {
          list(run = FALSE, data = list("step1_data.rds" = cached))
        } else {
          list(run = step %in% steps, data = list())
        }
      }
    )

    withr::local_options(ggsegExtra.output_dir = test_dir)

    expect_error(
      create_tract_atlas(
        input_tracts = list(t1 = matrix(1:30, ncol = 3)),
        input_aseg = NULL,
        steps = 2,
        verbose = FALSE
      ),
      "input_aseg.*required"
    )
  })

  it("reads LUT colours from RGB columns", {
    tracts <- list(
      matrix(c(1:20, rep(0, 40)), ncol = 3)
    )

    custom_lut <- data.frame(
      region = "Tract A",
      R = 255, G = 0, B = 128
    )

    atlas <- create_tract_atlas(
      input_tracts = tracts,
      input_lut = custom_lut,
      steps = 1,
      verbose = FALSE
    )

    expect_true(!is.na(atlas$palette[1]))
  })

  it("handles LUT without colour columns", {
    tracts <- list(
      matrix(c(1:20, rep(0, 40)), ncol = 3)
    )

    custom_lut <- data.frame(
      region = "Tract A"
    )

    atlas <- create_tract_atlas(
      input_tracts = tracts,
      input_lut = custom_lut,
      steps = 1,
      verbose = FALSE
    )

    expect_s3_class(atlas, "brain_atlas")
    expect_true("Tract A" %in% atlas$core$label)
  })

  it("loads cached step1 data when step 1 not in steps", {
    test_dir <- withr::local_tempdir()
    cached <- list(
      streamlines_data = list(t1 = matrix(1:30, ncol = 3)),
      centerlines_df = data.frame(label = "t1"),
      core = data.frame(
        hemi = "midline", region = "t1", label = "t1",
        stringsAsFactors = FALSE
      ),
      palette = c(t1 = "#FF0000"),
      atlas_name = "t1",
      tube_radius = 5,
      tube_segments = 8
    )

    local_mocked_bindings(
      setup_atlas_dirs = function(...) {
        list(
          base = test_dir, snaps = test_dir,
          processed = test_dir, masks = test_dir
        )
      },
      load_or_run_step = function(step, steps, ...) {
        if (step == 1L && !(step %in% steps)) {
          list(run = FALSE, data = list("step1_data.rds" = cached))
        } else {
          list(run = step %in% steps, data = list())
        }
      },
      detect_coords_are_voxels = function(...) TRUE,
      tract_create_snapshots = function(...) {
        list(
          views = data.frame(name = "ax_1", type = "axial",
                             start = 1, end = 10),
          cortex_slices = NULL
        )
      },
      process_and_mask_images = function(...) invisible(NULL),
      extract_contours = function(...) invisible(NULL),
      smooth_contours = function(...) invisible(NULL),
      reduce_vertex = function(...) invisible(NULL)
    )

    withr::local_options(ggsegExtra.output_dir = test_dir)
    aseg_file <- withr::local_tempfile(fileext = ".mgz")
    file.create(aseg_file)

    expect_no_error(
      create_tract_atlas(
        input_tracts = list(t1 = matrix(1:30, ncol = 3)),
        input_aseg = aseg_file,
        steps = 2:6,
        verbose = FALSE
      )
    )
  })

  it("step 1 verbose messages and auto-detect coordinate space", {
    test_dir <- withr::local_tempdir()
    tract_file <- withr::local_tempfile(fileext = ".trk")
    file.create(tract_file)

    local_mocked_bindings(
      tract_read_input = function(input_tracts, tract_names) {
        list(
          streamlines_data = list(t1 = matrix(1:30, ncol = 3)),
          tract_names = "t1"
        )
      },
      detect_coords_are_voxels = function(...) TRUE,
      tract_create_meshes = function(...) {
        list(t1 = list(
          metadata = list(
            centerline = matrix(1:9, ncol = 3,
              dimnames = list(NULL, c("x", "y", "z"))),
            tangents = matrix(1:9, ncol = 3)
          )
        ))
      },
      tract_build_core = function(...) {
        list(
          core = data.frame(
            hemi = "midline", region = "t1", label = "t1",
            stringsAsFactors = FALSE
          ),
          palette = c(t1 = "#FF0000"),
          centerlines_df = data.frame(label = "t1"),
          atlas_name = "t1"
        )
      },
      brain_atlas = function(...) structure(list(...), class = "brain_atlas"),
      tract_data = function(...) list(...),
      preview_atlas = function(...) invisible(NULL),
      setup_atlas_dirs = function(...) {
        list(
          base = test_dir, snaps = test_dir,
          processed = test_dir, masks = test_dir
        )
      },
      load_or_run_step = function(step, steps, ...) {
        list(run = step %in% steps, data = list())
      }
    )

    withr::local_options(ggsegExtra.output_dir = withr::local_tempdir())

    atlas <- create_tract_atlas(
      input_tracts = tract_file,
      steps = 1,
      verbose = TRUE
    )

    expect_s3_class(atlas, "brain_atlas")
  })

  it("step 1 3D-only atlas with cleanup", {
    test_dir <- withr::local_tempdir()
    tract_file <- withr::local_tempfile(fileext = ".trk")
    file.create(tract_file)

    local_mocked_bindings(
      tract_read_input = function(input_tracts, tract_names) {
        list(
          streamlines_data = list(t1 = matrix(1:30, ncol = 3)),
          tract_names = "t1"
        )
      },
      detect_coords_are_voxels = function(...) TRUE,
      tract_create_meshes = function(...) {
        list(t1 = list(
          metadata = list(
            centerline = matrix(1:9, ncol = 3,
              dimnames = list(NULL, c("x", "y", "z"))),
            tangents = matrix(1:9, ncol = 3)
          )
        ))
      },
      tract_build_core = function(...) {
        list(
          core = data.frame(
            hemi = "midline", region = "t1", label = "t1",
            stringsAsFactors = FALSE
          ),
          palette = c(t1 = "#FF0000"),
          centerlines_df = data.frame(label = "t1"),
          atlas_name = "t1"
        )
      },
      brain_atlas = function(...) structure(list(...), class = "brain_atlas"),
      tract_data = function(...) list(...),
      preview_atlas = function(...) invisible(NULL),
      setup_atlas_dirs = function(...) {
        list(
          base = test_dir, snaps = test_dir,
          processed = test_dir, masks = test_dir
        )
      },
      load_or_run_step = function(step, steps, ...) {
        list(run = step %in% steps, data = list())
      }
    )

    withr::local_options(ggsegExtra.output_dir = withr::local_tempdir())

    atlas <- create_tract_atlas(
      input_tracts = tract_file,
      steps = 1,
      verbose = TRUE,
      cleanup = TRUE
    )

    expect_s3_class(atlas, "brain_atlas")
  })

  it("loads cached step1 data with verbose", {
    test_dir <- withr::local_tempdir()
    tract_file <- withr::local_tempfile(fileext = ".trk")
    file.create(tract_file)

    cached <- list(
      streamlines_data = list(t1 = matrix(1:30, ncol = 3)),
      centerlines_df = data.frame(label = "t1"),
      core = data.frame(
        hemi = "midline", region = "t1", label = "t1",
        stringsAsFactors = FALSE
      ),
      palette = c(t1 = "#FF0000"),
      atlas_name = "t1",
      tube_radius = 5,
      tube_segments = 8
    )

    local_mocked_bindings(
      setup_atlas_dirs = function(...) {
        list(
          base = test_dir, snaps = test_dir,
          processed = test_dir, masks = test_dir
        )
      },
      load_or_run_step = function(step, steps, ...) {
        if (step == 1L && !(step %in% steps)) {
          list(run = FALSE, data = list("step1_data.rds" = cached))
        } else {
          list(run = step %in% steps, data = list())
        }
      },
      detect_coords_are_voxels = function(...) TRUE,
      tract_create_snapshots = function(...) {
        list(
          views = data.frame(name = "ax_1", type = "axial",
                             start = 1, end = 10),
          cortex_slices = NULL
        )
      },
      process_and_mask_images = function(...) invisible(NULL),
      extract_contours = function(...) invisible(NULL),
      smooth_contours = function(...) invisible(NULL),
      reduce_vertex = function(...) invisible(NULL)
    )

    withr::local_options(ggsegExtra.output_dir = test_dir)
    aseg_file <- withr::local_tempfile(fileext = ".mgz")
    file.create(aseg_file)

    result <- create_tract_atlas(
      input_tracts = tract_file,
      input_aseg = aseg_file,
      steps = 2:6,
      verbose = TRUE
    )

    expect_null(result)
  })

  it("loads cached step2 data with verbose", {
    test_dir <- withr::local_tempdir()
    tract_file <- withr::local_tempfile(fileext = ".trk")
    file.create(tract_file)

    cached <- list(
      streamlines_data = list(t1 = matrix(1:30, ncol = 3)),
      centerlines_df = data.frame(label = "t1"),
      core = data.frame(
        hemi = "midline", region = "t1", label = "t1",
        stringsAsFactors = FALSE
      ),
      palette = c(t1 = "#FF0000"),
      atlas_name = "t1",
      tube_radius = 5,
      tube_segments = 8
    )

    local_mocked_bindings(
      setup_atlas_dirs = function(...) {
        list(
          base = test_dir, snaps = test_dir,
          processed = test_dir, masks = test_dir
        )
      },
      load_or_run_step = function(step, steps, ...) {
        if (step == 1L) {
          list(run = FALSE, data = list("step1_data.rds" = cached))
        } else if (step == 2L) {
          list(run = FALSE, data = list(
            "views.rds" = data.frame(
              name = "ax_1", type = "axial", start = 1, end = 10
            ),
            "cortex_slices.rds" = NULL
          ))
        } else {
          list(run = step %in% steps, data = list())
        }
      },
      process_and_mask_images = function(...) invisible(NULL),
      extract_contours = function(...) invisible(NULL),
      smooth_contours = function(...) invisible(NULL),
      reduce_vertex = function(...) invisible(NULL)
    )

    withr::local_options(ggsegExtra.output_dir = test_dir)
    aseg_file <- withr::local_tempfile(fileext = ".mgz")
    file.create(aseg_file)

    result <- create_tract_atlas(
      input_tracts = tract_file,
      input_aseg = aseg_file,
      steps = 3:6,
      verbose = TRUE
    )

    expect_null(result)
  })

  it("step 7 builds final atlas when contours exist", {
    test_dir <- withr::local_tempdir()
    tract_file <- withr::local_tempfile(fileext = ".trk")
    file.create(tract_file)

    cached <- list(
      streamlines_data = list(t1 = matrix(1:30, ncol = 3)),
      centerlines_df = data.frame(label = "t1"),
      core = data.frame(
        hemi = "midline", region = "t1", label = "t1",
        stringsAsFactors = FALSE
      ),
      palette = c(t1 = "#FF0000"),
      atlas_name = "t1",
      tube_radius = 5,
      tube_segments = 8
    )

    contours_file <- file.path(test_dir, "contours_reduced.rda")
    file.create(contours_file)

    local_mocked_bindings(
      setup_atlas_dirs = function(...) {
        list(
          base = test_dir, snaps = test_dir,
          processed = test_dir, masks = test_dir
        )
      },
      load_or_run_step = function(step, steps, ...) {
        if (step == 1L) {
          list(run = FALSE, data = list("step1_data.rds" = cached))
        } else if (step == 2L) {
          list(run = FALSE, data = list(
            "views.rds" = data.frame(
              name = "ax_1", type = "axial", start = 1, end = 10
            ),
            "cortex_slices.rds" = NULL
          ))
        } else {
          list(run = step %in% steps, data = list())
        }
      },
      build_contour_sf = function(...) "mock_sf_data",
      brain_atlas = function(...) {
        args <- list(...)
        structure(
          list(
            core = args$core,
            palette = args$palette,
            type = args$type,
            data = args$data
          ),
          class = "brain_atlas"
        )
      },
      tract_data = function(...) list(...),
      warn_if_large_atlas = function(...) invisible(NULL),
      preview_atlas = function(...) invisible(NULL)
    )

    withr::local_options(ggsegExtra.output_dir = test_dir)
    aseg_file <- withr::local_tempfile(fileext = ".mgz")
    file.create(aseg_file)

    atlas <- create_tract_atlas(
      input_tracts = tract_file,
      input_aseg = aseg_file,
      steps = 7,
      verbose = TRUE
    )

    expect_s3_class(atlas, "brain_atlas")
  })

  it("step 7 with cleanup removes temp files", {
    test_dir <- withr::local_tempdir()
    tract_file <- withr::local_tempfile(fileext = ".trk")
    file.create(tract_file)

    cached <- list(
      streamlines_data = list(t1 = matrix(1:30, ncol = 3)),
      centerlines_df = data.frame(label = "t1"),
      core = data.frame(
        hemi = "midline", region = "t1", label = "t1",
        stringsAsFactors = FALSE
      ),
      palette = c(t1 = "#FF0000"),
      atlas_name = "t1",
      tube_radius = 5,
      tube_segments = 8
    )

    contours_file <- file.path(test_dir, "contours_reduced.rda")
    file.create(contours_file)

    local_mocked_bindings(
      setup_atlas_dirs = function(...) {
        list(
          base = test_dir, snaps = test_dir,
          processed = test_dir, masks = test_dir
        )
      },
      load_or_run_step = function(step, steps, ...) {
        if (step == 1L) {
          list(run = FALSE, data = list("step1_data.rds" = cached))
        } else if (step == 2L) {
          list(run = FALSE, data = list(
            "views.rds" = data.frame(
              name = "ax_1", type = "axial", start = 1, end = 10
            ),
            "cortex_slices.rds" = NULL
          ))
        } else {
          list(run = step %in% steps, data = list())
        }
      },
      build_contour_sf = function(...) "mock_sf",
      brain_atlas = function(...) {
        args <- list(...)
        structure(
          list(core = args$core, palette = args$palette),
          class = "brain_atlas"
        )
      },
      tract_data = function(...) list(...),
      warn_if_large_atlas = function(...) invisible(NULL),
      preview_atlas = function(...) invisible(NULL)
    )

    withr::local_options(ggsegExtra.output_dir = test_dir)
    aseg_file <- withr::local_tempfile(fileext = ".mgz")
    file.create(aseg_file)

    atlas <- create_tract_atlas(
      input_tracts = tract_file,
      input_aseg = aseg_file,
      steps = 7,
      verbose = TRUE,
      cleanup = TRUE
    )

    expect_s3_class(atlas, "brain_atlas")
  })

  it("step 7 errors when contours_reduced.rda missing", {
    test_dir <- withr::local_tempdir()
    cached <- list(
      streamlines_data = list(t1 = matrix(1:30, ncol = 3)),
      centerlines_df = data.frame(label = "t1"),
      core = data.frame(
        hemi = "midline", region = "t1", label = "t1",
        stringsAsFactors = FALSE
      ),
      palette = c(t1 = "#FF0000"),
      atlas_name = "t1",
      tube_radius = 5,
      tube_segments = 8
    )

    local_mocked_bindings(
      setup_atlas_dirs = function(...) {
        list(
          base = test_dir, snaps = test_dir,
          processed = test_dir, masks = test_dir
        )
      },
      load_or_run_step = function(step, steps, ...) {
        if (step == 1L) {
          list(run = FALSE, data = list("step1_data.rds" = cached))
        } else if (step == 2L) {
          list(run = FALSE, data = list(
            "views.rds" = data.frame(
              name = "ax_1", type = "axial", start = 1, end = 10
            ),
            "cortex_slices.rds" = NULL
          ))
        } else {
          list(run = step %in% steps, data = list())
        }
      }
    )

    withr::local_options(ggsegExtra.output_dir = test_dir)
    aseg_file <- withr::local_tempfile(fileext = ".mgz")
    file.create(aseg_file)

    expect_error(
      create_tract_atlas(
        input_tracts = list(t1 = matrix(1:30, ncol = 3)),
        input_aseg = aseg_file,
        steps = 7,
        verbose = FALSE
      ),
      "contours_reduced"
    )
  })

  it("returns invisible NULL for partial steps with verbose", {
    test_dir <- withr::local_tempdir()
    tract_file <- withr::local_tempfile(fileext = ".trk")
    file.create(tract_file)

    cached <- list(
      streamlines_data = list(t1 = matrix(1:30, ncol = 3)),
      centerlines_df = data.frame(label = "t1"),
      core = data.frame(
        hemi = "midline", region = "t1", label = "t1",
        stringsAsFactors = FALSE
      ),
      palette = c(t1 = "#FF0000"),
      atlas_name = "t1",
      tube_radius = 5,
      tube_segments = 8
    )

    local_mocked_bindings(
      setup_atlas_dirs = function(...) {
        list(
          base = test_dir, snaps = test_dir,
          processed = test_dir, masks = test_dir
        )
      },
      load_or_run_step = function(step, steps, ...) {
        if (step == 1L) {
          list(run = FALSE, data = list("step1_data.rds" = cached))
        } else if (step == 2L) {
          list(run = FALSE, data = list(
            "views.rds" = data.frame(
              name = "ax_1", type = "axial", start = 1, end = 10
            ),
            "cortex_slices.rds" = NULL
          ))
        } else {
          list(run = step %in% steps, data = list())
        }
      },
      process_and_mask_images = function(...) invisible(NULL),
      extract_contours = function(...) invisible(NULL),
      smooth_contours = function(...) invisible(NULL),
      reduce_vertex = function(...) invisible(NULL)
    )

    withr::local_options(ggsegExtra.output_dir = test_dir)
    aseg_file <- withr::local_tempfile(fileext = ".mgz")
    file.create(aseg_file)

    result <- create_tract_atlas(
      input_tracts = tract_file,
      input_aseg = aseg_file,
      steps = 3:6,
      verbose = TRUE
    )

    expect_null(result)
  })
})


describe("tract_read_input file path branch", {
  it("reads from file paths and derives names from filenames", {
    local_mocked_bindings(
      read_tractography = function(f) list(matrix(1:9, ncol = 3)),
      future_map = function(.x, .f, ...) lapply(.x, .f)
    )

    tract_file <- withr::local_tempfile(fileext = ".trk")
    file.create(tract_file)

    result <- tract_read_input(tract_file, NULL)

    expect_equal(length(result$streamlines_data), 1)
    expect_equal(
      result$tract_names,
      tools::file_path_sans_ext(basename(tract_file))
    )
  })

  it("uses provided tract_names for files", {
    local_mocked_bindings(
      read_tractography = function(f) list(matrix(1:9, ncol = 3)),
      future_map = function(.x, .f, ...) lapply(.x, .f)
    )

    tract_file <- withr::local_tempfile(fileext = ".trk")
    file.create(tract_file)

    result <- tract_read_input(tract_file, "custom_name")

    expect_equal(result$tract_names, "custom_name")
  })
})


describe("tract_create_meshes", {
  it("creates meshes and filters out NULL results", {
    call_count <- 0L
    local_mocked_bindings(
      progressor = function(...) function(...) NULL,
      future_map2 = function(.x, .y, .f, ...) {
        mapply(.f, .x, .y, SIMPLIFY = FALSE)
      },
      furrr_options = function(...) list(),
      extract_centerline = function(streamlines, ...) {
        matrix(1:9, ncol = 3, dimnames = list(NULL, c("x", "y", "z")))
      },
      resolve_tube_radius = function(...) rep(1, 3),
      generate_tube_mesh = function(...) {
        call_count <<- call_count + 1L
        if (call_count == 1L) {
          list(
            vertices = list(x = 1:3, y = 1:3, z = 1:3),
            faces = list(i = 1, j = 2, k = 3)
          )
        } else {
          NULL
        }
      },
      center_meshes = function(x) x
    )

    streamlines_data <- list(
      t1 = list(matrix(1:9, ncol = 3)),
      t2 = list(matrix(10:18, ncol = 3))
    )

    result <- tract_create_meshes(
      streamlines_data, c("t1", "t2"), "mean",
      50, 5, 8, c(0.2, 1.0)
    )

    expect_equal(length(result), 1)
    expect_equal(names(result), "t1")
  })

  it("skips tracts with NULL or too-short centerlines", {
    local_mocked_bindings(
      progressor = function(...) function(...) NULL,
      future_map2 = function(.x, .y, .f, ...) {
        mapply(.f, .x, .y, SIMPLIFY = FALSE)
      },
      furrr_options = function(...) list(),
      extract_centerline = function(streamlines, ...) NULL,
      center_meshes = function(x) x
    )

    streamlines_data <- list(t1 = list(matrix(1:9, ncol = 3)))

    expect_error(
      tract_create_meshes(
        streamlines_data, "t1", "mean", 50, 5, 8, c(0.2, 1.0)
      ),
      "No meshes"
    )
  })

  it("errors when all meshes fail", {
    local_mocked_bindings(
      progressor = function(...) function(...) NULL,
      future_map2 = function(.x, .y, .f, ...) {
        mapply(.f, .x, .y, SIMPLIFY = FALSE)
      },
      furrr_options = function(...) list(),
      extract_centerline = function(streamlines, ...) {
        matrix(1:6, ncol = 3, dimnames = list(NULL, c("x", "y", "z")))
      },
      resolve_tube_radius = function(...) rep(1, 2),
      generate_tube_mesh = function(...) NULL,
      center_meshes = function(x) x
    )

    streamlines_data <- list(t1 = list(matrix(1:9, ncol = 3)))

    expect_error(
      tract_create_meshes(
        streamlines_data, "t1", "mean", 50, 5, 8, c(0.2, 1.0)
      ),
      "No meshes"
    )
  })
})


describe("tract_create_snapshots", {
  it("creates snapshots for tracts and cortex slices", {
    snapshot_calls <- 0L
    cortex_calls <- 0L

    local_mocked_bindings(
      read_volume = function(f) {
        vol <- array(0L, dim = c(10, 10, 10))
        vol[3, 3, 3] <- 3L
        vol
      },
      default_tract_views = function(dims) {
        data.frame(
          name = "ax_1", type = "axial", start = 1, end = 10,
          stringsAsFactors = FALSE
        )
      },
      create_cortex_slices = function(views, dims) {
        data.frame(
          x = NA, y = NA, z = 5, view = "axial", name = "ax_1",
          stringsAsFactors = FALSE
        )
      },
      detect_cortex_labels = function(vol) list(left = 3L, right = integer(0)),
      extract_hemi_from_view = function(...) "left",
      extract_centerline = function(streamlines, ...) {
        matrix(1:9, ncol = 3, dimnames = list(NULL, c("x", "y", "z")))
      },
      streamlines_to_volume = function(...) array(1L, dim = c(10, 10, 10)),
      progressor = function(...) function(...) NULL,
      future_pmap = function(.l, .f, ...) purrr::pmap(.l, .f),
      furrr_options = function(...) list(),
      snapshot_partial_projection = function(...) {
        snapshot_calls <<- snapshot_calls + 1L
        invisible(NULL)
      },
      snapshot_cortex_slice = function(...) {
        cortex_calls <<- cortex_calls + 1L
        invisible(NULL)
      }
    )

    centerlines_df <- data.frame(label = "t1", stringsAsFactors = FALSE)
    streamlines_data <- list(t1 = list(matrix(1:9, ncol = 3)))
    dirs <- list(snaps = withr::local_tempdir())

    result <- tract_create_snapshots(
      streamlines_data, centerlines_df, "fake_aseg.mgz",
      NULL, dirs, TRUE, FALSE, 3, TRUE
    )

    expect_true(is.list(result))
    expect_true("views" %in% names(result))
    expect_true("cortex_slices" %in% names(result))
    expect_true(snapshot_calls > 0)
    expect_true(cortex_calls > 0)
  })

  it("uses provided views instead of defaults", {
    custom_views <- data.frame(
      name = "cor_1", type = "coronal", start = 50, end = 60,
      stringsAsFactors = FALSE
    )

    local_mocked_bindings(
      read_volume = function(f) array(0L, dim = c(10, 10, 10)),
      create_cortex_slices = function(views, dims) {
        data.frame(
          x = NA, y = 5, z = NA, view = "coronal", name = "cor_1",
          stringsAsFactors = FALSE
        )
      },
      detect_cortex_labels = function(vol) list(left = integer(0), right = integer(0)),
      extract_hemi_from_view = function(...) "left",
      extract_centerline = function(streamlines, ...) {
        matrix(1:9, ncol = 3, dimnames = list(NULL, c("x", "y", "z")))
      },
      streamlines_to_volume = function(...) array(0L, dim = c(10, 10, 10)),
      progressor = function(...) function(...) NULL,
      future_pmap = function(.l, .f, ...) purrr::pmap(.l, .f),
      furrr_options = function(...) list(),
      snapshot_partial_projection = function(...) invisible(NULL),
      snapshot_cortex_slice = function(...) invisible(NULL)
    )

    centerlines_df <- data.frame(label = "t1", stringsAsFactors = FALSE)
    streamlines_data <- list(t1 = list(matrix(1:9, ncol = 3)))
    dirs <- list(snaps = withr::local_tempdir())

    result <- tract_create_snapshots(
      streamlines_data, centerlines_df, "fake_aseg.mgz",
      custom_views, dirs, TRUE, FALSE, 3, FALSE
    )

    expect_equal(result$views$name, "cor_1")
    expect_equal(result$views$type, "coronal")
  })

  it("handles streamlines as list of lists", {
    local_mocked_bindings(
      read_volume = function(f) array(0L, dim = c(10, 10, 10)),
      default_tract_views = function(dims) {
        data.frame(
          name = "ax_1", type = "axial", start = 1, end = 10,
          stringsAsFactors = FALSE
        )
      },
      create_cortex_slices = function(views, dims) {
        data.frame(
          x = NA, y = NA, z = 5, view = "axial", name = "ax_1",
          stringsAsFactors = FALSE
        )
      },
      detect_cortex_labels = function(vol) list(left = integer(0), right = integer(0)),
      extract_hemi_from_view = function(...) "left",
      extract_centerline = function(streamlines, ...) {
        matrix(1:9, ncol = 3, dimnames = list(NULL, c("x", "y", "z")))
      },
      streamlines_to_volume = function(...) array(0L, dim = c(10, 10, 10)),
      progressor = function(...) function(...) NULL,
      future_pmap = function(.l, .f, ...) purrr::pmap(.l, .f),
      furrr_options = function(...) list(),
      snapshot_partial_projection = function(...) invisible(NULL),
      snapshot_cortex_slice = function(...) invisible(NULL)
    )

    centerlines_df <- data.frame(label = "t1", stringsAsFactors = FALSE)
    streamlines_data <- list(
      t1 = list(matrix(1:9, ncol = 3), matrix(10:18, ncol = 3))
    )
    dirs <- list(snaps = withr::local_tempdir())

    result <- tract_create_snapshots(
      streamlines_data, centerlines_df, "fake_aseg.mgz",
      NULL, dirs, TRUE, FALSE, 3, FALSE
    )

    expect_true(is.list(result))
  })
})


describe("resolve_tube_radius", {
  it("returns uniform radius when given scalar", {
    centerline <- matrix(c(1:10, rep(0, 20)), ncol = 3)
    streamlines <- list(centerline)

    result <- resolve_tube_radius(0.5, streamlines, centerline, c(0.2, 1.0))

    expect_length(result, 10)
    expect_true(all(result == 0.5))
  })

  it("returns input when given vector of correct length", {
    centerline <- matrix(c(1:5, rep(0, 10)), ncol = 3)
    streamlines <- list(centerline)
    radii <- c(0.1, 0.2, 0.3, 0.4, 0.5)

    result <- resolve_tube_radius(radii, streamlines, centerline, c(0.2, 1.0))

    expect_equal(result, radii)
  })

  it("errors when vector has wrong length", {
    centerline <- matrix(c(1:10, rep(0, 20)), ncol = 3)
    streamlines <- list(centerline)

    expect_error(
      resolve_tube_radius(c(0.1, 0.2), streamlines, centerline, c(0.2, 1.0)),
      "length 1 or 10"
    )
  })

  it("computes density-based radius", {
    centerline <- matrix(c(1:5, rep(0, 10)), ncol = 3)
    streamlines <- list(
      matrix(c(1:5, rep(0, 10)), ncol = 3),
      matrix(c(1:5, rep(0.5, 10)), ncol = 3)
    )

    result <- resolve_tube_radius(
      "density", streamlines, centerline, c(0.2, 1.0)
    )

    expect_length(result, 5)
    expect_true(all(result >= 0.2))
    expect_true(all(result <= 1.0))
  })

  it("returns mean density_range when all density is zero", {
    centerline <- matrix(c(1:5, rep(0, 10)), ncol = 3)
    streamlines <- list(
      matrix(c(100:104, rep(100, 10)), ncol = 3)
    )

    result <- resolve_tube_radius(
      "density", streamlines, centerline, c(0.2, 1.0)
    )

    expect_length(result, 5)
    expect_true(all(result == 0.6))
  })

  it("returns default radius for unknown string", {
    centerline <- matrix(c(1:5, rep(0, 10)), ncol = 3)
    streamlines <- list(centerline)

    result <- resolve_tube_radius(
      "unknown", streamlines, centerline, c(0.2, 1.0)
    )

    expect_length(result, 5)
    expect_true(all(result == 0.5))
  })
})


describe("default_tract_views", {
  it("creates views for standard 256 brain", {
    dims <- c(256, 256, 256)
    result <- default_tract_views(dims)

    expect_s3_class(result, "data.frame")
    expect_true(all(c("name", "type", "start", "end") %in% names(result)))
    expect_true("axial" %in% result$type)
    expect_true("coronal" %in% result$type)
    expect_true("sagittal" %in% result$type)
  })

  it("creates wider projections than subcortical", {
    dims <- c(256, 256, 256)
    tract_views <- default_tract_views(dims)
    subcort_views <- default_subcortical_views(dims)

    tract_axial <- tract_views[tract_views$type == "axial", ]
    subcort_axial <- subcort_views[subcort_views$type == "axial", ]

    tract_range <- max(tract_axial$end) - min(tract_axial$start)
    subcort_range <- max(subcort_axial$end) - min(subcort_axial$start)

    expect_gt(tract_range, subcort_range)
  })

  it("creates midline, left, and right sagittal projections", {
    dims <- c(256, 256, 256)
    result <- default_tract_views(dims)

    sagittal <- result[result$type == "sagittal", ]
    expect_equal(nrow(sagittal), 3)
    expect_true(any(grepl("midline", sagittal$name)))
    expect_true(any(grepl("left", sagittal$name)))
    expect_true(any(grepl("right", sagittal$name)))
  })

  it("sagittal projections are lateralised around midline", {
    dims <- c(256, 256, 256)
    result <- default_tract_views(dims)

    sagittal <- result[result$type == "sagittal", ]
    left <- sagittal[grepl("left", sagittal$name), ]
    right <- sagittal[grepl("right", sagittal$name), ]
    midline <- sagittal[grepl("midline", sagittal$name), ]

    expect_gt(left$start, midline$end)
    expect_lt(right$end, midline$start)
  })

  it("scales for different volume sizes", {
    dims_256 <- c(256, 256, 256)
    dims_128 <- c(128, 128, 128)

    result_256 <- default_tract_views(dims_256)
    result_128 <- default_tract_views(dims_128)

    axial_256 <- result_256[result_256$type == "axial", ]
    axial_128 <- result_128[result_128$type == "axial", ]

    expect_equal(axial_128$start[1] / axial_256$start[1], 0.5)
  })
})
