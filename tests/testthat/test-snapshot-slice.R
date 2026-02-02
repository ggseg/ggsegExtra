describe("snapshot_slice", {
  it("creates PNG when slice has data", {
    skip_if_no_freesurfer()

    mgz_file <- test_mgz_file()
    skip_if(!file.exists(mgz_file), "Test MGZ file not found")

    outdir <- withr::local_tempdir("snapshot_test_")

    ggsegExtra:::snapshot_slice(
      lab = mgz_file,
      x = 16, y = 16, z = 16,
      view = "axial",
      output_dir = outdir
    )

    expected_file <- file.path(outdir, "016_016_016_axial_test_aseg.png")
    expect_true(file.exists(expected_file))
  })

  it("skips snapshot when slice has no data", {
    skip_if_no_freesurfer()

    mgz_file <- test_mgz_file()
    skip_if(!file.exists(mgz_file), "Test MGZ file not found")

    outdir <- withr::local_tempdir("snapshot_test_")

    expect_no_error(
      ggsegExtra:::snapshot_slice(
        lab = mgz_file,
        x = 16, y = 16, z = 16,
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
        ggsegExtra:::snapshot_slice(
          lab = mgz_file,
          x = 16, y = 16, z = 16,
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
    outfile <- file.path(outdir, "016_016_016_axial_test_aseg.png")

    ggsegExtra:::snapshot_slice(
      lab = mgz_file,
      x = 16, y = 16, z = 16,
      view = "axial",
      output_dir = outdir
    )

    mtime1 <- file.mtime(outfile)
    Sys.sleep(0.1)

    ggsegExtra:::snapshot_slice(
      lab = mgz_file,
      x = 16, y = 16, z = 16,
      view = "axial",
      output_dir = outdir,
      skip_existing = TRUE
    )

    mtime2 <- file.mtime(outfile)
    expect_equal(mtime1, mtime2)
  })

  it("handles label files with label_file parameter", {
    skip_if_no_freesurfer()
    skip("Label file test requires matching label IDs - tested via integration")
  })
})
