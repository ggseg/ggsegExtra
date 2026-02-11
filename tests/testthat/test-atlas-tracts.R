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

    expect_s3_class(atlas, "ggseg_atlas")
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

  it("tube params are not stored on data object", {
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

    expect_null(atlas$data$tube_radius)
    expect_null(atlas$data$tube_segments)
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
        list(
          t1 = list(
            metadata = list(
              centerline = matrix(
                1:9,
                ncol = 3,
                dimnames = list(NULL, c("x", "y", "z"))
              ),
              tangents = matrix(1:9, ncol = 3)
            )
          )
        )
      },
      tract_build_core = function(...) {
        list(
          core = data.frame(
            hemi = "midline",
            region = "t1",
            label = "t1",
            stringsAsFactors = FALSE
          ),
          palette = c(t1 = "#FF0000"),
          centerlines_df = data.frame(label = "t1"),
          atlas_name = "t1"
        )
      },
      ggseg_atlas = function(...) structure(list(...), class = "ggseg_atlas"),
      ggseg_data_tract = function(...) list(...),
      preview_atlas = function(...) invisible(NULL),
      setup_atlas_dirs = function(...) {
        list(
          base = test_dir,
          snapshots = test_dir,
          processed = test_dir,
          masks = test_dir
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
        hemi = "midline",
        region = "t1",
        label = "t1",
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
          base = test_dir,
          snapshots = test_dir,
          processed = test_dir,
          masks = test_dir
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
      R = 255,
      G = 0,
      B = 128
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

    expect_s3_class(atlas, "ggseg_atlas")
    expect_true("Tract A" %in% atlas$core$label)
  })

  it("loads cached step1 data when step 1 not in steps", {
    test_dir <- withr::local_tempdir()
    cached <- list(
      streamlines_data = list(t1 = matrix(1:30, ncol = 3)),
      centerlines_df = data.frame(label = "t1"),
      core = data.frame(
        hemi = "midline",
        region = "t1",
        label = "t1",
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
          base = test_dir,
          snapshots = test_dir,
          processed = test_dir,
          masks = test_dir
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
          views = data.frame(
            name = "ax_1",
            type = "axial",
            start = 1,
            end = 10
          ),
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
        list(
          t1 = list(
            metadata = list(
              centerline = matrix(
                1:9,
                ncol = 3,
                dimnames = list(NULL, c("x", "y", "z"))
              ),
              tangents = matrix(1:9, ncol = 3)
            )
          )
        )
      },
      tract_build_core = function(...) {
        list(
          core = data.frame(
            hemi = "midline",
            region = "t1",
            label = "t1",
            stringsAsFactors = FALSE
          ),
          palette = c(t1 = "#FF0000"),
          centerlines_df = data.frame(label = "t1"),
          atlas_name = "t1"
        )
      },
      ggseg_atlas = function(...) structure(list(...), class = "ggseg_atlas"),
      ggseg_data_tract = function(...) list(...),
      preview_atlas = function(...) invisible(NULL),
      setup_atlas_dirs = function(...) {
        list(
          base = test_dir,
          snapshots = test_dir,
          processed = test_dir,
          masks = test_dir
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

    expect_s3_class(atlas, "ggseg_atlas")
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
        list(
          t1 = list(
            metadata = list(
              centerline = matrix(
                1:9,
                ncol = 3,
                dimnames = list(NULL, c("x", "y", "z"))
              ),
              tangents = matrix(1:9, ncol = 3)
            )
          )
        )
      },
      tract_build_core = function(...) {
        list(
          core = data.frame(
            hemi = "midline",
            region = "t1",
            label = "t1",
            stringsAsFactors = FALSE
          ),
          palette = c(t1 = "#FF0000"),
          centerlines_df = data.frame(label = "t1"),
          atlas_name = "t1"
        )
      },
      ggseg_atlas = function(...) structure(list(...), class = "ggseg_atlas"),
      ggseg_data_tract = function(...) list(...),
      preview_atlas = function(...) invisible(NULL),
      setup_atlas_dirs = function(...) {
        list(
          base = test_dir,
          snapshots = test_dir,
          processed = test_dir,
          masks = test_dir
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

    expect_s3_class(atlas, "ggseg_atlas")
  })

  it("loads cached step1 data with verbose", {
    test_dir <- withr::local_tempdir()
    tract_file <- withr::local_tempfile(fileext = ".trk")
    file.create(tract_file)

    cached <- list(
      streamlines_data = list(t1 = matrix(1:30, ncol = 3)),
      centerlines_df = data.frame(label = "t1"),
      core = data.frame(
        hemi = "midline",
        region = "t1",
        label = "t1",
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
          base = test_dir,
          snapshots = test_dir,
          processed = test_dir,
          masks = test_dir
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
          views = data.frame(
            name = "ax_1",
            type = "axial",
            start = 1,
            end = 10
          ),
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
        hemi = "midline",
        region = "t1",
        label = "t1",
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
          base = test_dir,
          snapshots = test_dir,
          processed = test_dir,
          masks = test_dir
        )
      },
      load_or_run_step = function(step, steps, ...) {
        if (step == 1L) {
          list(run = FALSE, data = list("step1_data.rds" = cached))
        } else if (step == 2L) {
          list(
            run = FALSE,
            data = list(
              "views.rds" = data.frame(
                name = "ax_1",
                type = "axial",
                start = 1,
                end = 10
              ),
              "cortex_slices.rds" = NULL
            )
          )
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
        hemi = "midline",
        region = "t1",
        label = "t1",
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
          base = test_dir,
          snapshots = test_dir,
          processed = test_dir,
          masks = test_dir
        )
      },
      load_or_run_step = function(step, steps, ...) {
        if (step == 1L) {
          list(run = FALSE, data = list("step1_data.rds" = cached))
        } else if (step == 2L) {
          list(
            run = FALSE,
            data = list(
              "views.rds" = data.frame(
                name = "ax_1",
                type = "axial",
                start = 1,
                end = 10
              ),
              "cortex_slices.rds" = NULL
            )
          )
        } else {
          list(run = step %in% steps, data = list())
        }
      },
      build_contour_sf = function(...) "mock_sf_data",
      ggseg_atlas = function(...) {
        args <- list(...)
        structure(
          list(
            core = args$core,
            palette = args$palette,
            type = args$type,
            data = args$data
          ),
          class = "ggseg_atlas"
        )
      },
      ggseg_data_tract = function(...) list(...),
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

    expect_s3_class(atlas, "ggseg_atlas")
  })

  it("step 7 with cleanup removes temp files", {
    test_dir <- withr::local_tempdir()
    tract_file <- withr::local_tempfile(fileext = ".trk")
    file.create(tract_file)

    cached <- list(
      streamlines_data = list(t1 = matrix(1:30, ncol = 3)),
      centerlines_df = data.frame(label = "t1"),
      core = data.frame(
        hemi = "midline",
        region = "t1",
        label = "t1",
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
          base = test_dir,
          snapshots = test_dir,
          processed = test_dir,
          masks = test_dir
        )
      },
      load_or_run_step = function(step, steps, ...) {
        if (step == 1L) {
          list(run = FALSE, data = list("step1_data.rds" = cached))
        } else if (step == 2L) {
          list(
            run = FALSE,
            data = list(
              "views.rds" = data.frame(
                name = "ax_1",
                type = "axial",
                start = 1,
                end = 10
              ),
              "cortex_slices.rds" = NULL
            )
          )
        } else {
          list(run = step %in% steps, data = list())
        }
      },
      build_contour_sf = function(...) "mock_sf",
      ggseg_atlas = function(...) {
        args <- list(...)
        structure(
          list(core = args$core, palette = args$palette),
          class = "ggseg_atlas"
        )
      },
      ggseg_data_tract = function(...) list(...),
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

    expect_s3_class(atlas, "ggseg_atlas")
  })

  it("step 7 errors when contours_reduced.rda missing", {
    test_dir <- withr::local_tempdir()
    cached <- list(
      streamlines_data = list(t1 = matrix(1:30, ncol = 3)),
      centerlines_df = data.frame(label = "t1"),
      core = data.frame(
        hemi = "midline",
        region = "t1",
        label = "t1",
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
          base = test_dir,
          snapshots = test_dir,
          processed = test_dir,
          masks = test_dir
        )
      },
      load_or_run_step = function(step, steps, ...) {
        if (step == 1L) {
          list(run = FALSE, data = list("step1_data.rds" = cached))
        } else if (step == 2L) {
          list(
            run = FALSE,
            data = list(
              "views.rds" = data.frame(
                name = "ax_1",
                type = "axial",
                start = 1,
                end = 10
              ),
              "cortex_slices.rds" = NULL
            )
          )
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
        hemi = "midline",
        region = "t1",
        label = "t1",
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
          base = test_dir,
          snapshots = test_dir,
          processed = test_dir,
          masks = test_dir
        )
      },
      load_or_run_step = function(step, steps, ...) {
        if (step == 1L) {
          list(run = FALSE, data = list("step1_data.rds" = cached))
        } else if (step == 2L) {
          list(
            run = FALSE,
            data = list(
              "views.rds" = data.frame(
                name = "ax_1",
                type = "axial",
                start = 1,
                end = 10
              ),
              "cortex_slices.rds" = NULL
            )
          )
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
