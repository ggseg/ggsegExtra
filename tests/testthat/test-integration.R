describe("integration tests", {
  it("creates atlas from labels and renders with ggseg3d", {
    skip_if_no_freesurfer()

    labels <- unlist(test_label_files())
    atlas <- create_atlas_from_labels(
      labels,
      atlas_name = "integration_test",
      steps = 1,
      verbose = FALSE
    )

    expect_s3_class(atlas, "ggseg_atlas")
    expect_equal(nrow(atlas$core), 3)

    expect_no_error({
      p <- ggseg3d::ggseg3d(atlas = atlas, hemisphere = "left")
    })
  })

  it("creates atlas from annotation and renders with ggseg3d", {
    skip_if_no_freesurfer()

    annot_dir <- file.path(freesurfer::fs_subj_dir(), "fsaverage5", "label")
    annot_files <- file.path(annot_dir, c("lh.aparc.annot", "rh.aparc.annot"))
    skip_if(
      !all(file.exists(annot_files)),
      "fsaverage5 aparc annotation not found"
    )

    atlas <- expect_warnings(
      create_cortical_atlas(
        input_annot = annot_files,
        steps = 1,
        verbose = FALSE
      ),
      "version"
    )

    expect_s3_class(atlas, "ggseg_atlas")
    expect_true(nrow(atlas$core) > 30)

    expect_no_error({
      p <- ggseg3d::ggseg3d(atlas = atlas, hemisphere = "left")
    })
  })

  it("reads colortable files", {
    lut_file <- test_lut_file()
    skip_if(!file.exists(lut_file), "Test LUT file not found")

    ctab <- read_ctab(lut_file)

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
