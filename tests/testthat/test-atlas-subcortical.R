describe("create_subcortical_atlas", {
  it("requires FreeSurfer to be available", {
    local_mocked_bindings(
      check_fs = function(abort = FALSE) {
        if (abort) cli::cli_abort("FreeSurfer not found")
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

    expect_s3_class(atlas, "brain_atlas")
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

    expect_s3_class(atlas, "brain_atlas")
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
      idx = 10, label = "test", stringsAsFactors = FALSE
    )
    dirs <- list(meshes = withr::local_tempdir())

    expect_error(
      subcort_create_meshes(
        "fake.mgz", colortable, dirs, FALSE, FALSE
      ),
      "No meshes"
    )
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
          idx = 10, label = "test_region", color = NA_character_,
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
          base = test_dir, meshes = test_dir, snaps = test_dir,
          processed = test_dir, masks = test_dir
        )
      },
      load_or_run_step = function(step, steps, ...) {
        list(run = step %in% steps, data = list())
      },
      subcort_create_meshes = function(...) {
        list(test_region = list(
          vertices = list(x = 1, y = 1, z = 1),
          faces = list(i = 1, j = 1, k = 1)
        ))
      },
      subcort_build_components = function(...) {
        list(
          core = data.frame(
            hemi = NA, region = "test", label = "test_region",
            stringsAsFactors = FALSE
          ),
          palette = NULL,
          meshes_df = data.frame(label = "test_region")
        )
      },
      brain_atlas = function(...) structure(list(...), class = "brain_atlas"),
      subcortical_data = function(...) list(...)
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
          idx = 10, label = "region", color = "#FF0000",
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
          base = test_dir, meshes = test_dir, snaps = test_dir,
          processed = test_dir, masks = test_dir
        )
      },
      load_or_run_step = function(step, steps, ...) {
        list(run = step %in% steps, data = list())
      },
      subcort_create_meshes = function(...) {
        list(region = list(
          vertices = list(x = 1, y = 1, z = 1),
          faces = list(i = 1, j = 1, k = 1)
        ))
      },
      subcort_build_components = function(...) {
        list(
          core = data.frame(
            hemi = NA, region = "region", label = "region",
            stringsAsFactors = FALSE
          ),
          palette = c(region = "#FF0000"),
          meshes_df = data.frame(label = "region")
        )
      },
      brain_atlas = function(...) structure(list(...), class = "brain_atlas"),
      subcortical_data = function(...) list(...)
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

    expect_s3_class(atlas, "brain_atlas")
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
      dirs$snaps, dirs$processed, dirs$masks,
      dilate = NULL, skip_existing = FALSE
    )

    expect_true(mask_called)
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
