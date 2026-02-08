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


describe("process_and_mask_images", {
  it("calls process_snapshot_image for each png then extract_alpha_mask", {
    snap_dir <- withr::local_tempdir("snap_")
    processed_dir <- withr::local_tempdir("proc_")
    mask_dir <- withr::local_tempdir("mask_")

    file.create(file.path(snap_dir, "img1.png"))
    file.create(file.path(snap_dir, "img2.png"))

    process_called <- character(0)
    mask_called <- character(0)

    local_mocked_bindings(
      process_snapshot_image = function(input_file, output_file, ...) {
        process_called <<- c(process_called, basename(input_file))
        file.create(output_file)
      },
      extract_alpha_mask = function(input_file, output_file, ...) {
        mask_called <<- c(mask_called, basename(input_file))
      },
      progressor = function(...) function(...) NULL
    )

    process_and_mask_images(snap_dir, processed_dir, mask_dir)

    expect_equal(sort(process_called), c("img1.png", "img2.png"))
    expect_equal(sort(mask_called), c("img1.png", "img2.png"))
  })

  it("passes dilate parameter through", {
    snap_dir <- withr::local_tempdir("snap_")
    processed_dir <- withr::local_tempdir("proc_")
    mask_dir <- withr::local_tempdir("mask_")

    file.create(file.path(snap_dir, "img1.png"))

    captured_dilate <- NULL
    local_mocked_bindings(
      process_snapshot_image = function(input_file, output_file, dilate, ...) {
        captured_dilate <<- dilate
        file.create(output_file)
      },
      extract_alpha_mask = function(...) NULL,
      progressor = function(...) function(...) NULL
    )

    process_and_mask_images(snap_dir, processed_dir, mask_dir, dilate = 3L)

    expect_equal(captured_dilate, 3L)
  })

  it("handles empty directory", {
    snap_dir <- withr::local_tempdir("snap_")
    processed_dir <- withr::local_tempdir("proc_")
    mask_dir <- withr::local_tempdir("mask_")

    local_mocked_bindings(
      progressor = function(...) function(...) NULL
    )

    expect_no_error(
      process_and_mask_images(snap_dir, processed_dir, mask_dir)
    )
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

    expect_equal(result$x[1], round(256 * 0.55))
    expect_equal(result$x[2], round(256 * 0.45))
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
