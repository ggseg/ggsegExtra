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


describe("tessellate_label", {
  it("creates mesh from volume label", {
    skip_if_no_freesurfer()

    vol_file <- test_mgz_file()
    skip_if(!file.exists(vol_file), "Test volume file not found")

    vol <- read_volume(vol_file)
    labels <- unique(c(vol))
    labels <- labels[labels != 0]
    skip_if(length(labels) == 0, "No labels in test volume")

    output_dir <- tempdir()
    mesh <- tessellate_label(
      volume_file = vol_file,
      label_id = labels[1],
      output_dir = output_dir,
      verbose = FALSE
    )

    expect_true(is.list(mesh))
    expect_true(all(c("vertices", "faces") %in% names(mesh)))
    expect_true(nrow(mesh$vertices) > 0)
    expect_true(nrow(mesh$faces) > 0)
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
