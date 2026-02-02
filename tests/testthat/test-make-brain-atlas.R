describe("make_brain_atlas", {
  it("creates 3D-only atlas from annotation", {
    skip_if_no_freesurfer()

    atlas <- make_brain_atlas(
      annot = "aparc",
      subject = "fsaverage5",
      include_geometry = FALSE,
      verbose = FALSE
    )

    expect_s3_class(atlas, "brain_atlas")
    expect_equal(atlas$type, "cortical")
    expect_true(nrow(atlas$data) > 0)
  })

  it("includes vertices for 3D rendering", {
    skip_if_no_freesurfer()

    atlas <- make_brain_atlas(
      annot = "aparc",
      subject = "fsaverage5",
      include_geometry = FALSE,
      verbose = FALSE
    )

    expect_true("vertices" %in% names(atlas$data))
    expect_type(atlas$data$vertices, "list")
  })

  it("works with different annotations", {
    skip_if_no_freesurfer()

    atlas_aparc <- make_brain_atlas(
      annot = "aparc",
      include_geometry = FALSE,
      verbose = FALSE
    )

    expect_true(nrow(atlas_aparc$data) > 0)

    annot_dir <- file.path(freesurfer::fs_subj_dir(), "fsaverage5", "label")
    has_a2009s <- file.exists(file.path(annot_dir, "lh.aparc.a2009s.annot"))

    if (has_a2009s) {
      atlas_a2009s <- make_brain_atlas(
        annot = "aparc.a2009s",
        include_geometry = FALSE,
        verbose = FALSE
      )
      expect_true(nrow(atlas_a2009s$data) > 0)
      expect_false(identical(atlas_aparc$data$region, atlas_a2009s$data$region))
    }
  })

  it("can render with ggseg3d", {
    skip_if_no_freesurfer()

    atlas <- make_brain_atlas(
      annot = "aparc",
      include_geometry = FALSE,
      verbose = FALSE
    )

    expect_no_error({
      p <- ggseg3d::ggseg3d(atlas = atlas, hemisphere = "left")
    })
  })
})
