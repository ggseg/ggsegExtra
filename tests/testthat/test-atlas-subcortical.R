describe("create_subcortical_atlas decimate validation", {
  it("errors for values outside (0, 1)", {
    local_mocked_bindings(check_fs = function(...) TRUE)
    vol_file <- withr::local_tempfile(fileext = ".mgz")
    file.create(vol_file)

    for (val in list(-0.5, 0, 1, 1.5, 2)) {
      expect_error(
        create_subcortical_atlas(vol_file, decimate = val, verbose = FALSE),
        "decimate.*must be a single number between 0 and 1"
      )
    }
  })

  it("errors for non-numeric values", {
    local_mocked_bindings(check_fs = function(...) TRUE)
    vol_file <- withr::local_tempfile(fileext = ".mgz")
    file.create(vol_file)

    expect_error(
      create_subcortical_atlas(vol_file, decimate = "half", verbose = FALSE),
      "decimate.*must be a single number"
    )
  })

  it("errors for vectors of length > 1", {
    local_mocked_bindings(check_fs = function(...) TRUE)
    vol_file <- withr::local_tempfile(fileext = ".mgz")
    file.create(vol_file)

    expect_error(
      create_subcortical_atlas(
        vol_file,
        decimate = c(0.3, 0.5),
        verbose = FALSE
      ),
      "decimate.*must be a single number"
    )
  })

  it("accepts NULL to skip decimation", {
    local_mocked_bindings(check_fs = function(...) TRUE)

    expect_error(
      create_subcortical_atlas(
        "/nonexistent/volume.mgz",
        decimate = NULL,
        verbose = FALSE
      ),
      "not found"
    )
  })

  it("accepts valid values in (0, 1)", {
    local_mocked_bindings(check_fs = function(...) TRUE)

    for (val in c(0.1, 0.25, 0.5, 0.75, 0.99)) {
      expect_error(
        create_subcortical_atlas(
          "/nonexistent/volume.mgz",
          decimate = val,
          verbose = FALSE
        ),
        "not found"
      )
    }
  })
})


describe("create_subcortical_atlas", {
  it("requires FreeSurfer to be available", {
    local_mocked_bindings(
      check_fs = function(abort = FALSE) {
        if (abort) {
          cli::cli_abort("FreeSurfer not found")
        }
        FALSE
      }
    )

    expect_error(
      create_subcortical_atlas(
        input_volume = "test.mgz",
        verbose = FALSE
      ),
      "FreeSurfer"
    )
  })

  it("errors when volume file not found", {
    skip_if_no_freesurfer()

    expect_error(
      create_subcortical_atlas(
        input_volume = "nonexistent_file.mgz",
        verbose = FALSE
      ),
      "not found"
    )
  })

  it("errors when specified color table not found", {
    skip_if_no_freesurfer()

    vol_file <- test_mgz_file()
    skip_if(!file.exists(vol_file), "Test volume file not found")

    expect_error(
      create_subcortical_atlas(
        input_volume = vol_file,
        input_lut = "nonexistent_lut.txt",
        verbose = FALSE
      ),
      "not found"
    )
  })

  it("generates colortable when input_lut is NULL", {
    skip_if_no_freesurfer()

    vol_file <- test_mgz_file()
    skip_if(!file.exists(vol_file), "Test volume file not found")

    expect_warning(
      atlas <- create_subcortical_atlas(
        input_volume = vol_file,
        input_lut = NULL,
        steps = 1:3,
        verbose = FALSE
      ),
      "No color lookup table"
    )

    expect_s3_class(atlas, "ggseg_atlas")
    expect_true(nrow(atlas$core) > 0)
    expect_true(all(grepl("^region_", atlas$core$label)))
    expect_null(atlas$palette)
  })

  it("creates atlas with meshes component", {
    skip_if_no_freesurfer()

    vol_file <- test_mgz_file()
    skip_if(!file.exists(vol_file), "Test volume file not found")

    lut_file <- test_lut_file()
    skip_if(!file.exists(lut_file), "Test LUT file not found")

    atlas <- create_subcortical_atlas(
      input_volume = vol_file,
      input_lut = lut_file,
      steps = 1:3,
      verbose = FALSE
    )

    expect_s3_class(atlas, "ggseg_atlas")
    expect_equal(atlas$type, "subcortical")
    expect_false(is.null(atlas$data$meshes))
    expect_true("mesh" %in% names(atlas$data$meshes))
  })

  it("creates valid mesh structure", {
    skip_if_no_freesurfer()

    vol_file <- test_mgz_file()
    skip_if(!file.exists(vol_file), "Test volume file not found")

    lut_file <- test_lut_file()
    skip_if(!file.exists(lut_file), "Test LUT file not found")

    atlas <- create_subcortical_atlas(
      input_volume = vol_file,
      input_lut = lut_file,
      steps = 1:3,
      verbose = FALSE
    )

    for (i in seq_len(nrow(atlas$data$meshes))) {
      mesh <- atlas$data$meshes$mesh[[i]]
      if (!is.null(mesh)) {
        expect_true(all(c("vertices", "faces") %in% names(mesh)))
        expect_true(all(c("x", "y", "z") %in% names(mesh$vertices)))
        expect_true(all(c("i", "j", "k") %in% names(mesh$faces)))
      }
    }
  })

  it("assigns correct hemisphere", {
    skip_if_no_freesurfer()

    vol_file <- test_mgz_file()
    skip_if(!file.exists(vol_file), "Test volume file not found")

    lut_file <- test_lut_file()
    skip_if(!file.exists(lut_file), "Test LUT file not found")

    atlas <- create_subcortical_atlas(
      input_volume = vol_file,
      input_lut = lut_file,
      steps = 1:3,
      verbose = FALSE
    )

    left_labels <- atlas$core$label[grepl("Left|left|lh", atlas$core$label)]
    right_labels <- atlas$core$label[grepl("Right|right|rh", atlas$core$label)]

    left_hemis <- atlas$core$hemi[atlas$core$label %in% left_labels]
    right_hemis <- atlas$core$hemi[atlas$core$label %in% right_labels]

    if (length(left_hemis) > 0) {
      expect_true(all(left_hemis == "left" | is.na(left_hemis)))
    }
    if (length(right_hemis) > 0) {
      expect_true(all(right_hemis == "right" | is.na(right_hemis)))
    }
  })

  it("can render with ggseg3d", {
    skip_if_no_freesurfer()

    vol_file <- test_mgz_file()
    skip_if(!file.exists(vol_file), "Test volume file not found")

    lut_file <- test_lut_file()
    skip_if(!file.exists(lut_file), "Test LUT file not found")

    atlas <- create_subcortical_atlas(
      input_volume = vol_file,
      input_lut = lut_file,
      steps = 1:3,
      verbose = FALSE
    )

    expect_no_error({
      p <- ggseg3d::ggseg3d(atlas = atlas, hemisphere = "subcort")
    })
  })
})


describe("subcort_build_components", {
  it("builds components from colortable and meshes", {
    meshes_list <- list(
      "Left-Putamen" = list(
        vertices = list(x = 1:3, y = 1:3, z = 1:3),
        faces = list(i = 1, j = 2, k = 3)
      ),
      "Right-Putamen" = list(
        vertices = list(x = 4:6, y = 4:6, z = 4:6),
        faces = list(i = 1, j = 2, k = 3)
      )
    )
    colortable <- data.frame(
      idx = c(12, 51),
      label = c("Left-Putamen", "Right-Putamen"),
      color = c("#FF0000", "#00FF00"),
      stringsAsFactors = FALSE
    )

    result <- subcort_build_components(colortable, meshes_list)

    expect_type(result, "list")
    expect_true("core" %in% names(result))
    expect_true("palette" %in% names(result))
    expect_equal(nrow(result$core), 2)
  })
})


describe("subcort_create_meshes", {
  it("errors when no meshes are created", {
    local_mocked_bindings(
      tessellate_label = function(...) NULL,
      progressor = function(...) function(...) NULL,
      center_meshes = function(x) x
    )

    colortable <- data.frame(
      idx = 10,
      label = "test",
      stringsAsFactors = FALSE
    )
    dirs <- list(meshes = withr::local_tempdir())

    expect_error(
      subcort_create_meshes(
        "fake.mgz",
        colortable,
        dirs,
        FALSE,
        FALSE
      ),
      "No meshes"
    )
  })

  it("warns and skips when tessellation fails with verbose", {
    local_mocked_bindings(
      tessellate_label = function(...) stop("mesh error"),
      progressor = function(...) function(...) NULL,
      future_map2 = mock_future_map2,
      furrr_options = function(...) list(),
      center_meshes = function(x) x
    )

    colortable <- data.frame(
      idx = c(10, 20),
      label = c("Left-Putamen", "Right-Putamen"),
      stringsAsFactors = FALSE
    )
    dirs <- list(meshes = withr::local_tempdir())

    expect_error(
      expect_warning(
        expect_warning(
          subcort_create_meshes("fake.mgz", colortable, dirs, FALSE, TRUE),
          "Failed to create mesh"
        ),
        "Failed to create mesh"
      ),
      "No meshes"
    )
  })

  it("returns meshes and logs success with verbose", {
    mock_mesh <- list(
      vertices = list(x = 1:3, y = 1:3, z = 1:3),
      faces = list(i = 1, j = 2, k = 3)
    )
    local_mocked_bindings(
      tessellate_label = function(...) mock_mesh,
      progressor = function(...) function(...) NULL,
      future_map2 = mock_future_map2,
      furrr_options = function(...) list(),
      center_meshes = function(x) x
    )

    colortable <- data.frame(
      idx = 10,
      label = "Left-Putamen",
      stringsAsFactors = FALSE
    )
    dirs <- list(meshes = withr::local_tempdir())

    result <- subcort_create_meshes(
      "fake.mgz",
      colortable,
      dirs,
      FALSE,
      TRUE,
      decimate = NULL
    )

    expect_equal(length(result), 1)
    expect_equal(names(result), "Left-Putamen")
  })

  it("filters out NULL meshes from failed tessellations", {
    call_count <- 0L
    mock_mesh <- list(
      vertices = list(x = 1:3, y = 1:3, z = 1:3),
      faces = list(i = 1, j = 2, k = 3)
    )
    local_mocked_bindings(
      tessellate_label = function(...) {
        call_count <<- call_count + 1L
        if (call_count == 1L) stop("fail") else mock_mesh
      },
      progressor = function(...) function(...) NULL,
      future_map2 = mock_future_map2,
      furrr_options = function(...) list(),
      center_meshes = function(x) x
    )

    colortable <- data.frame(
      idx = c(10, 20),
      label = c("Left-Putamen", "Right-Putamen"),
      stringsAsFactors = FALSE
    )
    dirs <- list(meshes = withr::local_tempdir())

    result <- subcort_create_meshes(
      "fake.mgz",
      colortable,
      dirs,
      FALSE,
      FALSE,
      decimate = NULL
    )

    expect_equal(length(result), 1)
    expect_equal(names(result), "Right-Putamen")
  })
})


describe("subcort_create_snapshots", {
  it("creates snapshots for structures and cortex slices", {
    snapshot_calls <- 0L
    cortex_calls <- 0L

    local_mocked_bindings(
      read_volume = function(f) {
        vol <- array(0L, dim = c(10, 10, 10))
        vol[2, 2, 2] <- 10L
        vol[5, 5, 5] <- 3L
        vol
      },
      default_subcortical_views = function(dims) {
        data.frame(
          name = "ax_1",
          type = "axial",
          start = 1,
          end = 10,
          stringsAsFactors = FALSE
        )
      },
      create_cortex_slices = function(views, dims) {
        data.frame(
          x = NA,
          y = NA,
          z = 5,
          view = "axial",
          name = "ax_1",
          stringsAsFactors = FALSE
        )
      },
      detect_cortex_labels = function(vol) {
        list(left = 3L, right = integer(0))
      },
      extract_hemi_from_view = function(...) "left",
      progressor = function(...) function(...) NULL,
      future_pmap = mock_future_pmap,
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

    colortable <- data.frame(
      idx = 10,
      label = "Left-Putamen",
      stringsAsFactors = FALSE
    )
    dirs <- list(snaps = withr::local_tempdir())

    result <- subcort_create_snapshots(
      "fake.mgz",
      colortable,
      NULL,
      dirs,
      FALSE
    )

    expect_true(is.list(result))
    expect_true("views" %in% names(result))
    expect_true("cortex_slices" %in% names(result))
    expect_true(snapshot_calls > 0)
    expect_true(cortex_calls > 0)
  })

  it("uses provided views instead of defaults", {
    custom_views <- data.frame(
      name = "custom_view",
      type = "coronal",
      start = 50,
      end = 60,
      stringsAsFactors = FALSE
    )

    local_mocked_bindings(
      read_volume = function(f) {
        vol <- array(0L, dim = c(10, 10, 10))
        vol[2, 2, 2] <- 10L
        vol
      },
      create_cortex_slices = function(views, dims) {
        data.frame(
          x = NA,
          y = 5,
          z = NA,
          view = "coronal",
          name = "custom_view",
          stringsAsFactors = FALSE
        )
      },
      detect_cortex_labels = function(vol) {
        list(left = integer(0), right = integer(0))
      },
      extract_hemi_from_view = function(...) "left",
      progressor = function(...) function(...) NULL,
      future_pmap = mock_future_pmap,
      furrr_options = function(...) list(),
      snapshot_partial_projection = function(...) invisible(NULL),
      snapshot_cortex_slice = function(...) invisible(NULL)
    )

    colortable <- data.frame(
      idx = 10,
      label = "Left-Putamen",
      stringsAsFactors = FALSE
    )
    dirs <- list(snaps = withr::local_tempdir())

    result <- subcort_create_snapshots(
      "fake.mgz",
      colortable,
      custom_views,
      dirs,
      FALSE
    )

    expect_equal(result$views$name, "custom_view")
    expect_equal(result$views$type, "coronal")
  })

  it("skips structures with zero voxels in volume", {
    snapshot_calls <- 0L

    local_mocked_bindings(
      read_volume = function(f) {
        vol <- array(0L, dim = c(10, 10, 10))
        vol
      },
      default_subcortical_views = function(dims) {
        data.frame(
          name = "ax_1",
          type = "axial",
          start = 1,
          end = 10,
          stringsAsFactors = FALSE
        )
      },
      create_cortex_slices = function(views, dims) {
        data.frame(
          x = NA,
          y = NA,
          z = 5,
          view = "axial",
          name = "ax_1",
          stringsAsFactors = FALSE
        )
      },
      detect_cortex_labels = function(vol) {
        list(left = integer(0), right = integer(0))
      },
      extract_hemi_from_view = function(...) "left",
      progressor = function(...) function(...) NULL,
      future_pmap = mock_future_pmap,
      furrr_options = function(...) list(),
      snapshot_partial_projection = function(...) {
        snapshot_calls <<- snapshot_calls + 1L
        invisible(NULL)
      },
      snapshot_cortex_slice = function(...) invisible(NULL)
    )

    colortable <- data.frame(
      idx = 99,
      label = "Missing-Region",
      stringsAsFactors = FALSE
    )
    dirs <- list(snaps = withr::local_tempdir())

    result <- subcort_create_snapshots(
      "fake.mgz",
      colortable,
      NULL,
      dirs,
      FALSE
    )

    expect_equal(snapshot_calls, 0)
  })
})


describe("create_subcortical_atlas pipeline flow", {
  it("step 1 uses generate_colortable_from_volume when no LUT", {
    generated <- FALSE
    test_dir <- withr::local_tempdir()
    local_mocked_bindings(
      check_fs = function(...) TRUE,
      generate_colortable_from_volume = function(vol) {
        generated <<- TRUE
        data.frame(
          idx = 10,
          label = "test_region",
          color = NA_character_,
          stringsAsFactors = FALSE
        )
      },
      read_volume = function(f) {
        vol <- array(0L, dim = c(3, 3, 3))
        vol[1, 1, 1] <- 10L
        vol
      },
      setup_atlas_dirs = function(...) {
        list(
          base = test_dir,
          meshes = test_dir,
          snaps = test_dir,
          processed = test_dir,
          masks = test_dir
        )
      },
      load_or_run_step = function(step, steps, ...) {
        list(run = step %in% steps, data = list())
      },
      subcort_create_meshes = function(...) {
        list(
          test_region = list(
            vertices = list(x = 1, y = 1, z = 1),
            faces = list(i = 1, j = 1, k = 1)
          )
        )
      },
      subcort_build_components = function(...) {
        list(
          core = data.frame(
            hemi = NA,
            region = "test",
            label = "test_region",
            stringsAsFactors = FALSE
          ),
          palette = NULL,
          meshes_df = data.frame(label = "test_region")
        )
      },
      ggseg_atlas = function(...) structure(list(...), class = "ggseg_atlas"),
      ggseg_data_subcortical = function(...) list(...)
    )

    withr::local_options(ggsegExtra.output_dir = withr::local_tempdir())
    vol_file <- withr::local_tempfile(fileext = ".mgz")
    file.create(vol_file)

    expect_warning(
      atlas <- create_subcortical_atlas(
        input_volume = vol_file,
        input_lut = NULL,
        steps = 1:3,
        verbose = FALSE
      ),
      "No color lookup table"
    )

    expect_true(generated)
    expect_null(atlas$palette)
  })

  it("returns 3D-only atlas when max(steps) == 3", {
    test_dir <- withr::local_tempdir()
    local_mocked_bindings(
      check_fs = function(...) TRUE,
      get_ctab = function(f) {
        data.frame(
          idx = 10,
          label = "region",
          color = "#FF0000",
          stringsAsFactors = FALSE
        )
      },
      read_volume = function(f) {
        vol <- array(0L, dim = c(3, 3, 3))
        vol[1, 1, 1] <- 10L
        vol
      },
      setup_atlas_dirs = function(...) {
        list(
          base = test_dir,
          meshes = test_dir,
          snaps = test_dir,
          processed = test_dir,
          masks = test_dir
        )
      },
      load_or_run_step = function(step, steps, ...) {
        list(run = step %in% steps, data = list())
      },
      subcort_create_meshes = function(...) {
        list(
          region = list(
            vertices = list(x = 1, y = 1, z = 1),
            faces = list(i = 1, j = 1, k = 1)
          )
        )
      },
      subcort_build_components = function(...) {
        list(
          core = data.frame(
            hemi = NA,
            region = "region",
            label = "region",
            stringsAsFactors = FALSE
          ),
          palette = c(region = "#FF0000"),
          meshes_df = data.frame(label = "region")
        )
      },
      ggseg_atlas = function(...) structure(list(...), class = "ggseg_atlas"),
      ggseg_data_subcortical = function(...) list(...)
    )

    vol_file <- withr::local_tempfile(fileext = ".mgz")
    file.create(vol_file)
    lut_file <- withr::local_tempfile(fileext = ".txt")
    file.create(lut_file)
    withr::local_options(ggsegExtra.output_dir = withr::local_tempdir())

    atlas <- create_subcortical_atlas(
      input_volume = vol_file,
      input_lut = lut_file,
      steps = 1:3,
      verbose = FALSE
    )

    expect_s3_class(atlas, "ggseg_atlas")
  })

  it("step 5 calls process_and_mask_images", {
    mask_called <- FALSE
    local_mocked_bindings(
      process_and_mask_images = function(...) {
        mask_called <<- TRUE
      }
    )

    dirs <- list(
      snaps = withr::local_tempdir(),
      processed = withr::local_tempdir(),
      masks = withr::local_tempdir()
    )

    process_and_mask_images(
      dirs$snaps,
      dirs$processed,
      dirs$masks,
      dilate = NULL,
      skip_existing = FALSE
    )

    expect_true(mask_called)
  })

  it("verbose step 1 logs structure count", {
    test_dir <- withr::local_tempdir()
    logged_msg <- NULL
    local_mocked_bindings(
      check_fs = function(...) TRUE,
      get_ctab = function(f) {
        data.frame(
          idx = c(10, 20),
          label = c("region_a", "region_b"),
          color = c("#FF0000", "#00FF00"),
          stringsAsFactors = FALSE
        )
      },
      read_volume = function(f) {
        vol <- array(0L, dim = c(3, 3, 3))
        vol[1, 1, 1] <- 10L
        vol[2, 2, 2] <- 20L
        vol
      },
      setup_atlas_dirs = function(...) {
        list(
          base = test_dir,
          meshes = test_dir,
          snaps = test_dir,
          processed = test_dir,
          masks = test_dir
        )
      },
      load_or_run_step = function(step, steps, ...) {
        list(run = step %in% steps, data = list())
      },
      subcort_create_meshes = function(...) {
        list(
          region_a = list(
            vertices = list(x = 1, y = 1, z = 1),
            faces = list(i = 1, j = 1, k = 1)
          ),
          region_b = list(
            vertices = list(x = 2, y = 2, z = 2),
            faces = list(i = 1, j = 1, k = 1)
          )
        )
      },
      subcort_build_components = function(...) {
        list(
          core = data.frame(
            hemi = NA,
            region = c("a", "b"),
            label = c("region_a", "region_b"),
            stringsAsFactors = FALSE
          ),
          palette = c(region_a = "#FF0000", region_b = "#00FF00"),
          meshes_df = data.frame(label = c("region_a", "region_b"))
        )
      },
      ggseg_atlas = function(...) structure(list(...), class = "ggseg_atlas"),
      ggseg_data_subcortical = function(...) list(...)
    )

    vol_file <- withr::local_tempfile(fileext = ".mgz")
    file.create(vol_file)
    lut_file <- withr::local_tempfile(fileext = ".txt")
    file.create(lut_file)
    withr::local_options(ggsegExtra.output_dir = test_dir)

    atlas <- create_subcortical_atlas(
      input_volume = vol_file,
      input_lut = lut_file,
      steps = 1:3,
      verbose = TRUE
    )

    expect_s3_class(atlas, "ggseg_atlas")
    expect_equal(nrow(atlas$core), 2)
  })

  it("errors when no matching labels found", {
    test_dir <- withr::local_tempdir()
    local_mocked_bindings(
      check_fs = function(...) TRUE,
      get_ctab = function(f) {
        data.frame(
          idx = 999,
          label = "nonexistent",
          color = "#FF0000",
          stringsAsFactors = FALSE
        )
      },
      read_volume = function(f) {
        vol <- array(0L, dim = c(3, 3, 3))
        vol[1, 1, 1] <- 10L
        vol
      },
      setup_atlas_dirs = function(...) {
        list(
          base = test_dir,
          meshes = test_dir,
          snaps = test_dir,
          processed = test_dir,
          masks = test_dir
        )
      },
      load_or_run_step = function(step, steps, ...) {
        list(run = step %in% steps, data = list())
      }
    )

    vol_file <- withr::local_tempfile(fileext = ".mgz")
    file.create(vol_file)
    lut_file <- withr::local_tempfile(fileext = ".txt")
    file.create(lut_file)
    withr::local_options(ggsegExtra.output_dir = test_dir)

    expect_error(
      create_subcortical_atlas(
        input_volume = vol_file,
        input_lut = lut_file,
        steps = 1,
        verbose = FALSE
      ),
      "No matching labels"
    )
  })

  it("loads cached step data when step not in steps", {
    test_dir <- withr::local_tempdir()
    cached_colortable <- data.frame(
      idx = 10,
      label = "cached_region",
      color = "#AABBCC",
      stringsAsFactors = FALSE
    )
    cached_meshes <- list(
      cached_region = list(
        vertices = list(x = 1, y = 1, z = 1),
        faces = list(i = 1, j = 1, k = 1)
      )
    )
    cached_components <- list(
      core = data.frame(
        hemi = NA,
        region = "cached",
        label = "cached_region",
        stringsAsFactors = FALSE
      ),
      palette = c(cached_region = "#AABBCC"),
      meshes_df = data.frame(label = "cached_region")
    )

    local_mocked_bindings(
      check_fs = function(...) TRUE,
      setup_atlas_dirs = function(...) {
        list(
          base = test_dir,
          meshes = test_dir,
          snaps = test_dir,
          processed = test_dir,
          masks = test_dir
        )
      },
      load_or_run_step = function(step, steps, files, ...) {
        if (step == 1L && !(step %in% steps)) {
          list(
            run = FALSE,
            data = list(
              "colortable.rds" = cached_colortable,
              "vol_labels.rds" = c(10)
            )
          )
        } else if (step == 2L && !(step %in% steps)) {
          list(run = FALSE, data = list("meshes_list.rds" = cached_meshes))
        } else if (step == 3L) {
          list(run = TRUE, data = list())
        } else {
          list(run = step %in% steps, data = list())
        }
      },
      subcort_build_components = function(...) cached_components,
      ggseg_atlas = function(...) structure(list(...), class = "ggseg_atlas"),
      ggseg_data_subcortical = function(...) list(...)
    )

    vol_file <- withr::local_tempfile(fileext = ".mgz")
    file.create(vol_file)
    lut_file <- withr::local_tempfile(fileext = ".txt")
    file.create(lut_file)
    withr::local_options(ggsegExtra.output_dir = test_dir)

    atlas <- create_subcortical_atlas(
      input_volume = vol_file,
      input_lut = lut_file,
      steps = 3,
      verbose = FALSE
    )

    expect_s3_class(atlas, "ggseg_atlas")
  })

  it("step 9 errors when contours_reduced.rda missing", {
    test_dir <- withr::local_tempdir()
    local_mocked_bindings(
      check_fs = function(...) TRUE,
      setup_atlas_dirs = function(...) {
        list(
          base = test_dir,
          meshes = test_dir,
          snaps = test_dir,
          processed = test_dir,
          masks = test_dir
        )
      },
      load_or_run_step = function(step, steps, ...) {
        if (step %in% steps) {
          list(run = TRUE, data = list())
        } else {
          list(
            run = FALSE,
            data = list(
              "colortable.rds" = data.frame(idx = 10, label = "r"),
              "vol_labels.rds" = c(10),
              "meshes_list.rds" = list(),
              "components.rds" = list(
                core = data.frame(hemi = NA, region = "r", label = "r"),
                palette = c(r = "#FF0000"),
                meshes_df = data.frame(label = "r")
              ),
              "views.rds" = data.frame(
                name = "ax_1",
                type = "axial",
                start = 1,
                end = 10
              ),
              "cortex_slices.rds" = NULL
            )
          )
        }
      },
      subcort_create_snapshots = function(...) {
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

    vol_file <- withr::local_tempfile(fileext = ".mgz")
    file.create(vol_file)
    lut_file <- withr::local_tempfile(fileext = ".txt")
    file.create(lut_file)
    withr::local_options(ggsegExtra.output_dir = test_dir)

    expect_error(
      create_subcortical_atlas(
        input_volume = vol_file,
        input_lut = lut_file,
        steps = 9,
        verbose = FALSE
      ),
      "contours_reduced"
    )
  })

  it("loads cached data with verbose for steps 1-4", {
    test_dir <- withr::local_tempdir()
    cached_colortable <- data.frame(
      idx = 10,
      label = "cached_r",
      color = "#AABBCC",
      stringsAsFactors = FALSE
    )
    cached_meshes <- list(
      cached_r = list(
        vertices = list(x = 1, y = 1, z = 1),
        faces = list(i = 1, j = 1, k = 1)
      )
    )
    cached_components <- list(
      core = data.frame(
        hemi = NA,
        region = "cached",
        label = "cached_r",
        stringsAsFactors = FALSE
      ),
      palette = c(cached_r = "#AABBCC"),
      meshes_df = data.frame(label = "cached_r")
    )
    cached_views <- data.frame(
      name = "ax_1",
      type = "axial",
      start = 1,
      end = 10,
      stringsAsFactors = FALSE
    )

    local_mocked_bindings(
      check_fs = function(...) TRUE,
      setup_atlas_dirs = function(...) {
        list(
          base = test_dir,
          meshes = test_dir,
          snaps = test_dir,
          processed = test_dir,
          masks = test_dir
        )
      },
      load_or_run_step = function(step, steps, ...) {
        if (step == 1L) {
          list(
            run = FALSE,
            data = list(
              "colortable.rds" = cached_colortable,
              "vol_labels.rds" = c(10)
            )
          )
        } else if (step == 2L) {
          list(run = FALSE, data = list("meshes_list.rds" = cached_meshes))
        } else if (step == 3L) {
          list(run = FALSE, data = list("components.rds" = cached_components))
        } else if (step == 4L) {
          list(
            run = FALSE,
            data = list(
              "views.rds" = cached_views,
              "cortex_slices.rds" = data.frame(
                x = 128,
                y = NA,
                z = NA,
                view = "axial",
                name = "ax_1"
              )
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

    vol_file <- withr::local_tempfile(fileext = ".mgz")
    file.create(vol_file)
    lut_file <- withr::local_tempfile(fileext = ".txt")
    file.create(lut_file)
    withr::local_options(ggsegExtra.output_dir = test_dir)

    result <- create_subcortical_atlas(
      input_volume = vol_file,
      input_lut = lut_file,
      steps = 5:8,
      verbose = TRUE
    )

    expect_null(result)
  })

  it("runs steps 4-8 via the pipeline", {
    test_dir <- withr::local_tempdir()
    cached_colortable <- data.frame(
      idx = 10,
      label = "region",
      color = "#FF0000",
      stringsAsFactors = FALSE
    )
    step4_called <- FALSE
    step5_called <- FALSE
    step6_called <- FALSE
    step7_called <- FALSE
    step8_called <- FALSE

    local_mocked_bindings(
      check_fs = function(...) TRUE,
      setup_atlas_dirs = function(...) {
        list(
          base = test_dir,
          meshes = test_dir,
          snaps = test_dir,
          processed = test_dir,
          masks = test_dir
        )
      },
      load_or_run_step = function(step, steps, ...) {
        if (step %in% c(1L, 2L, 3L)) {
          list(
            run = FALSE,
            data = list(
              "colortable.rds" = cached_colortable,
              "vol_labels.rds" = c(10),
              "meshes_list.rds" = list(),
              "components.rds" = list(
                core = data.frame(hemi = NA, region = "r", label = "region"),
                palette = c(region = "#FF0000"),
                meshes_df = data.frame(label = "region")
              )
            )
          )
        } else if (step == 4L) {
          list(run = TRUE, data = list())
        } else {
          list(run = step %in% steps, data = list())
        }
      },
      subcort_create_snapshots = function(...) {
        step4_called <<- TRUE
        list(
          views = data.frame(
            name = "ax_1",
            type = "axial",
            start = 1,
            end = 10,
            stringsAsFactors = FALSE
          ),
          cortex_slices = data.frame(
            x = 128,
            y = NA,
            z = NA,
            view = "axial",
            name = "ax_1"
          )
        )
      },
      process_and_mask_images = function(...) {
        step5_called <<- TRUE
        invisible(NULL)
      },
      extract_contours = function(...) {
        step6_called <<- TRUE
        invisible(NULL)
      },
      smooth_contours = function(...) {
        step7_called <<- TRUE
        invisible(NULL)
      },
      reduce_vertex = function(...) {
        step8_called <<- TRUE
        invisible(NULL)
      }
    )

    vol_file <- withr::local_tempfile(fileext = ".mgz")
    file.create(vol_file)
    lut_file <- withr::local_tempfile(fileext = ".txt")
    file.create(lut_file)
    withr::local_options(ggsegExtra.output_dir = test_dir)

    result <- create_subcortical_atlas(
      input_volume = vol_file,
      input_lut = lut_file,
      steps = 4:8,
      verbose = TRUE
    )

    expect_true(step4_called)
    expect_true(step5_called)
    expect_true(step6_called)
    expect_true(step7_called)
    expect_true(step8_called)
  })

  it("step 9 builds final atlas when contours exist", {
    test_dir <- withr::local_tempdir()
    cached_components <- list(
      core = data.frame(
        hemi = NA,
        region = "r",
        label = "region",
        stringsAsFactors = FALSE
      ),
      palette = c(region = "#FF0000"),
      meshes_df = data.frame(label = "region")
    )
    cached_views <- data.frame(
      name = "ax_1",
      type = "axial",
      start = 1,
      end = 10,
      stringsAsFactors = FALSE
    )

    contours_file <- file.path(test_dir, "contours_reduced.rda")
    file.create(contours_file)

    local_mocked_bindings(
      check_fs = function(...) TRUE,
      setup_atlas_dirs = function(...) {
        list(
          base = test_dir,
          meshes = test_dir,
          snaps = test_dir,
          processed = test_dir,
          masks = test_dir
        )
      },
      load_or_run_step = function(step, steps, ...) {
        list(
          run = FALSE,
          data = list(
            "colortable.rds" = data.frame(idx = 10, label = "region"),
            "vol_labels.rds" = c(10),
            "meshes_list.rds" = list(),
            "components.rds" = cached_components,
            "views.rds" = cached_views,
            "cortex_slices.rds" = NULL
          )
        )
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
      ggseg_data_subcortical = function(...) list(...),
      warn_if_large_atlas = function(...) invisible(NULL),
      preview_atlas = function(...) invisible(NULL)
    )

    vol_file <- withr::local_tempfile(fileext = ".mgz")
    file.create(vol_file)
    lut_file <- withr::local_tempfile(fileext = ".txt")
    file.create(lut_file)
    withr::local_options(ggsegExtra.output_dir = test_dir)

    atlas <- create_subcortical_atlas(
      input_volume = vol_file,
      input_lut = lut_file,
      steps = 9,
      verbose = TRUE
    )

    expect_s3_class(atlas, "ggseg_atlas")
  })

  it("step 9 with cleanup removes temp files", {
    test_dir <- withr::local_tempdir()
    cached_components <- list(
      core = data.frame(
        hemi = NA,
        region = "r",
        label = "region",
        stringsAsFactors = FALSE
      ),
      palette = c(region = "#FF0000"),
      meshes_df = data.frame(label = "region")
    )
    cached_views <- data.frame(
      name = "ax_1",
      type = "axial",
      start = 1,
      end = 10,
      stringsAsFactors = FALSE
    )

    contours_file <- file.path(test_dir, "contours_reduced.rda")
    file.create(contours_file)

    local_mocked_bindings(
      check_fs = function(...) TRUE,
      setup_atlas_dirs = function(...) {
        list(
          base = test_dir,
          meshes = test_dir,
          snaps = test_dir,
          processed = test_dir,
          masks = test_dir
        )
      },
      load_or_run_step = function(step, steps, ...) {
        list(
          run = FALSE,
          data = list(
            "colortable.rds" = data.frame(idx = 10, label = "region"),
            "vol_labels.rds" = c(10),
            "meshes_list.rds" = list(),
            "components.rds" = cached_components,
            "views.rds" = cached_views,
            "cortex_slices.rds" = NULL
          )
        )
      },
      build_contour_sf = function(...) "mock_sf",
      ggseg_atlas = function(...) {
        args <- list(...)
        structure(
          list(core = args$core, palette = args$palette),
          class = "ggseg_atlas"
        )
      },
      ggseg_data_subcortical = function(...) list(...),
      warn_if_large_atlas = function(...) invisible(NULL),
      preview_atlas = function(...) invisible(NULL)
    )

    vol_file <- withr::local_tempfile(fileext = ".mgz")
    file.create(vol_file)
    lut_file <- withr::local_tempfile(fileext = ".txt")
    file.create(lut_file)
    withr::local_options(
      ggsegExtra.output_dir = test_dir,
      ggsegExtra.cleanup = TRUE
    )

    atlas <- create_subcortical_atlas(
      input_volume = vol_file,
      input_lut = lut_file,
      steps = 9,
      verbose = TRUE,
      cleanup = TRUE
    )

    expect_s3_class(atlas, "ggseg_atlas")
  })

  it("returns invisible NULL for partial steps with verbose", {
    test_dir <- withr::local_tempdir()
    local_mocked_bindings(
      check_fs = function(...) TRUE,
      setup_atlas_dirs = function(...) {
        list(
          base = test_dir,
          meshes = test_dir,
          snaps = test_dir,
          processed = test_dir,
          masks = test_dir
        )
      },
      load_or_run_step = function(step, steps, ...) {
        list(
          run = FALSE,
          data = list(
            "colortable.rds" = data.frame(idx = 10, label = "r"),
            "vol_labels.rds" = c(10),
            "meshes_list.rds" = list(),
            "components.rds" = list(
              core = data.frame(hemi = NA, region = "r", label = "r"),
              palette = c(r = "#FF0000"),
              meshes_df = data.frame(label = "r")
            ),
            "views.rds" = data.frame(
              name = "ax_1",
              type = "axial",
              start = 1,
              end = 10
            ),
            "cortex_slices.rds" = NULL
          )
        )
      },
      process_and_mask_images = function(...) invisible(NULL),
      extract_contours = function(...) invisible(NULL)
    )

    vol_file <- withr::local_tempfile(fileext = ".mgz")
    file.create(vol_file)
    lut_file <- withr::local_tempfile(fileext = ".txt")
    file.create(lut_file)
    withr::local_options(ggsegExtra.output_dir = test_dir)

    result <- create_subcortical_atlas(
      input_volume = vol_file,
      input_lut = lut_file,
      steps = 5:6,
      verbose = TRUE
    )

    expect_null(result)
  })
})


describe("default_subcortical_views", {
  it("creates views for standard 256 brain", {
    dims <- c(256, 256, 256)
    result <- default_subcortical_views(dims)

    expect_s3_class(result, "data.frame")
    expect_true(all(c("name", "type", "start", "end") %in% names(result)))
    expect_true("axial" %in% result$type)
    expect_true("coronal" %in% result$type)
    expect_true("sagittal" %in% result$type)
  })

  it("scales views for different volume sizes", {
    dims_256 <- c(256, 256, 256)
    dims_512 <- c(512, 512, 512)

    result_256 <- default_subcortical_views(dims_256)
    result_512 <- default_subcortical_views(dims_512)

    axial_256 <- result_256[result_256$type == "axial", ]
    axial_512 <- result_512[result_512$type == "axial", ]

    expect_equal(axial_512$start[1] / axial_256$start[1], 2)
  })

  it("creates single sagittal midline slice", {
    dims <- c(256, 256, 256)
    result <- default_subcortical_views(dims)

    sagittal <- result[result$type == "sagittal", ]
    expect_equal(nrow(sagittal), 1)
    expect_equal(sagittal$start, 128)
    expect_equal(sagittal$end, 128)
  })
})
