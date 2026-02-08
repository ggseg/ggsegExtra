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
