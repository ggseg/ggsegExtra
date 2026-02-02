describe("integration tests", {
  it("creates atlas from labels and renders with ggseg3d", {
    skip_if_no_freesurfer()

    labels <- unlist(test_label_files())
    atlas <- make_atlas_from_labels(
      labels,
      atlas_name = "integration_test",
      include_geometry = FALSE,
      verbose = FALSE
    )

    expect_s3_class(atlas, "brain_atlas")
    expect_equal(nrow(atlas$data), 3)

    expect_no_error({
      p <- ggseg3d::ggseg3d(atlas = atlas, hemisphere = "left")
    })
  })

  it("creates atlas from annotation and renders with ggseg3d", {
    skip_if_no_freesurfer()

    atlas <- make_brain_atlas(
      annot = "aparc",
      include_geometry = FALSE,
      verbose = FALSE
    )

    expect_s3_class(atlas, "brain_atlas")
    expect_true(nrow(atlas$data) > 30)

    expect_no_error({
      p <- ggseg3d::ggseg3d(atlas = atlas, hemisphere = "left")
    })
  })

  it("reads colortable files", {
    lut_file <- test_lut_file()
    skip_if(!file.exists(lut_file), "Test LUT file not found")

    ctab <- ggseg.formats::read_ctab(lut_file)

    expect_s3_class(ctab, "data.frame")
    expect_true(all(c("idx", "label", "R", "G", "B", "A") %in% names(ctab)))
    expect_equal(nrow(ctab), 5)
  })

  it("test data files exist", {
    labels <- test_label_files()
    expect_true(all(file.exists(unlist(labels))))

    mgz <- test_mgz_file()
    expect_true(file.exists(mgz))

    lut <- test_lut_file()
    expect_true(file.exists(lut))
  })
})
