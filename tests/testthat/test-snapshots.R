describe("extract_slice_2d", {
  it("extracts axial slice", {
    vol <- array(0, dim = c(10, 10, 10))
    vol[5, 5, 5] <- 1

    slice <- extract_slice_2d(vol, "axial", 5)

    expect_true(is.matrix(slice))
    expect_equal(dim(slice), c(10, 10))
    expect_equal(max(slice), 1)
  })

  it("extracts coronal slice", {
    vol <- array(0, dim = c(10, 10, 10))
    vol[5, 5, 5] <- 1

    slice <- extract_slice_2d(vol, "coronal", 5)

    expect_true(is.matrix(slice))
    expect_equal(dim(slice), c(10, 10))
    expect_equal(max(slice), 1)
  })

  it("extracts sagittal slice", {
    vol <- array(0, dim = c(10, 10, 10))
    vol[5, 5, 5] <- 1

    slice <- extract_slice_2d(vol, "sagittal", 5)

    expect_true(is.matrix(slice))
    expect_equal(dim(slice), c(10, 10))
    expect_equal(max(slice), 1)
  })
})


describe("orient_slice_2d", {
  it("flips left sagittal horizontally", {
    slice <- matrix(c(1, 2, 3, 4), nrow = 2)

    flipped <- orient_slice_2d(slice, "sagittal", hemi = "left")

    expect_equal(flipped[1, ], slice[2, ])
    expect_equal(flipped[2, ], slice[1, ])
  })

  it("does not flip right sagittal", {
    slice <- matrix(c(1, 2, 3, 4), nrow = 2)

    result <- orient_slice_2d(slice, "sagittal", hemi = "right")

    expect_equal(result, slice)
  })

  it("does not flip axial or coronal", {
    slice <- matrix(c(1, 2, 3, 4), nrow = 2)

    expect_equal(orient_slice_2d(slice, "axial"), slice)
    expect_equal(orient_slice_2d(slice, "coronal"), slice)
  })
})


describe("snapshot_partial_projection", {
  it("creates PNG for synthetic volume", {
    vol <- array(0L, dim = c(10, 10, 10))
    vol[4:6, 4:6, 4:6] <- 1L

    outdir <- withr::local_tempdir("partial_proj_")

    snapshot_partial_projection(
      vol = vol,
      view = "axial",
      start = 1,
      end = 10,
      view_name = "axial_1",
      label = "test_region",
      output_dir = outdir,
      colour = "red",
      skip_existing = FALSE
    )

    files <- list.files(outdir, pattern = "\\.png$")
    expect_true(length(files) > 0)
  })
})


describe("snapshot_slice", {
  it("creates PNG when slice has data", {
    skip_if_no_freesurfer()

    mgz_file <- test_mgz_file()
    skip_if(!file.exists(mgz_file), "Test MGZ file not found")

    outdir <- withr::local_tempdir("snapshot_test_")

    snapshot_slice(
      lab = mgz_file,
      x = 16,
      y = 16,
      z = 16,
      view = "axial",
      output_dir = outdir
    )

    expected_file <- file.path(outdir, "016_016_016_axial_aseg.png")
    expect_true(file.exists(expected_file))
  })

  it("skips snapshot when slice has no data", {
    skip_if_no_freesurfer()

    mgz_file <- test_mgz_file()
    skip_if(!file.exists(mgz_file), "Test MGZ file not found")

    outdir <- withr::local_tempdir("snapshot_test_")

    expect_no_error(
      snapshot_slice(
        lab = mgz_file,
        x = 1,
        y = 30,
        z = 20,
        view = "sagittal",
        output_dir = outdir
      )
    )

    files_created <- list.files(outdir, pattern = "\\.png$")
    expect_length(files_created, 0)
  })

  it("runs without error for all view types", {
    skip_if_no_freesurfer()

    mgz_file <- test_mgz_file()
    skip_if(!file.exists(mgz_file), "Test MGZ file not found")

    outdir <- withr::local_tempdir("snapshot_test_")

    for (view in c("axial", "sagittal", "coronal")) {
      expect_no_error(
        snapshot_slice(
          lab = mgz_file,
          x = 16,
          y = 16,
          z = 16,
          view = view,
          output_dir = outdir
        )
      )
    }
  })

  it("skips existing files when skip_existing = TRUE", {
    skip_if_no_freesurfer()

    mgz_file <- test_mgz_file()
    skip_if(!file.exists(mgz_file), "Test MGZ file not found")

    outdir <- withr::local_tempdir("snapshot_test_")
    outfile <- file.path(outdir, "016_016_016_axial_aseg.png")

    snapshot_slice(
      lab = mgz_file,
      x = 16,
      y = 16,
      z = 16,
      view = "axial",
      output_dir = outdir
    )

    mtime1 <- file.mtime(outfile)
    Sys.sleep(0.1)

    snapshot_slice(
      lab = mgz_file,
      x = 16,
      y = 16,
      z = 16,
      view = "axial",
      output_dir = outdir,
      skip_existing = TRUE
    )

    mtime2 <- file.mtime(outfile)
    expect_equal(mtime1, mtime2)
  })
})


describe("snapshot_volume_slice", {
  it("creates PNG for valid volume slice", {
    vol <- array(0, dim = c(32, 32, 32))
    vol[10:20, 10:20, 16] <- 1

    outdir <- withr::local_tempdir("snapshot_vol_")

    result <- snapshot_volume_slice(
      vol = vol,
      x = 16,
      y = 16,
      z = 16,
      view = "axial",
      label = "test",
      output_dir = outdir
    )

    expect_true(file.exists(result))
    expect_match(result, "axial.*test\\.png$")
  })

  it("returns NULL when slice is empty", {
    vol <- array(0, dim = c(32, 32, 32))

    outdir <- withr::local_tempdir("snapshot_vol_")

    result <- snapshot_volume_slice(
      vol = vol,
      x = 16,
      y = 16,
      z = 16,
      view = "axial",
      label = "empty",
      output_dir = outdir
    )

    expect_null(result)
    expect_length(list.files(outdir, pattern = "\\.png$"), 0)
  })
})


describe("volume_projection", {
  it("creates axial projection", {
    vol <- array(0, dim = c(10, 10, 10))
    vol[5, 5, 1:10] <- 1:10

    proj <- volume_projection(vol, "axial")

    expect_equal(dim(proj), c(10, 10))
    expect_equal(max(proj), 10)
  })

  it("creates sagittal projection", {
    vol <- array(0, dim = c(10, 10, 10))
    vol[1:10, 5, 5] <- 1:10

    proj <- volume_projection(vol, "sagittal")

    expect_equal(dim(proj), c(10, 10))
  })

  it("creates coronal projection", {
    vol <- array(0, dim = c(10, 10, 10))
    vol[5, 1:10, 5] <- 1:10

    proj <- volume_projection(vol, "coronal")

    expect_equal(dim(proj), c(10, 10))
  })
})


describe("volume_projection with start/end", {
  it("creates partial axial projection", {
    vol <- array(0, dim = c(10, 10, 10))
    vol[5, 5, 3:7] <- 1

    proj <- volume_projection(vol, "axial", start = 3, end = 7)

    expect_equal(dim(proj), c(10, 10))
    expect_equal(proj[5, 5], 1)
  })

  it("respects slice boundaries", {
    vol <- array(0, dim = c(10, 10, 10))
    vol[5, 5, 1:2] <- 1
    vol[5, 5, 8:10] <- 2

    proj <- volume_projection(vol, "axial", start = 3, end = 7)

    expect_equal(max(proj), 0)
  })
})
