describe("make_ggseg_atlas_volumetric", {
  it("creates atlas from MGZ file", {
    skip_if_no_freesurfer()
    skip("Full volumetric test is slow - run manually")

    mgz_file <- test_mgz_file()
    lut_file <- test_lut_file()
    skip_if(!file.exists(mgz_file), "Test MGZ file not found")

    outdir <- withr::local_tempdir("atlas_test_")

    atlas <- make_ggseg_atlas_volumetric(
      label_file = mgz_file,
      color_lut = lut_file,
      output_dir = outdir,
      cleanup = TRUE,
      verbose = FALSE
    )

    expect_s3_class(atlas, "brain_atlas")
    expect_equal(atlas$type, "subcortical")
  })
})

describe("extract_contours", {
  it("extracts contours from mask directory", {
    skip_if_no_freesurfer()
    skip("Requires mask images - tested via integration tests")
  })
})

describe("smooth_contours", {
  it("smooths contour geometry", {
    skip_if_no_freesurfer()
    skip("Requires contour data - tested via integration tests")
  })
})

describe("reduce_vertex", {
  it("simplifies contour geometry", {
    skip_if_no_freesurfer()
    skip("Requires contour data - tested via integration tests")
  })
})
