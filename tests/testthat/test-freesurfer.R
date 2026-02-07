describe("fs_surfaces", {
  it("returns character vector of surface names", {
    result <- fs_surfaces()

    expect_type(result, "character")
    expect_true(length(result) > 0)
  })

  it("includes common surfaces", {
    result <- fs_surfaces()

    expect_true("inflated" %in% result)
    expect_true("pial" %in% result)
    expect_true("white" %in% result)
    expect_true("sphere" %in% result)
  })
})


describe("fs_curvatures", {
  it("returns character vector of curvature names", {
    result <- fs_curvatures()

    expect_type(result, "character")
    expect_true(length(result) > 0)
  })

  it("includes common curvatures", {
    result <- fs_curvatures()

    expect_true("thickness" %in% result)
    expect_true("curv" %in% result)
    expect_true("sulc" %in% result)
    expect_true("area" %in% result)
  })
})


describe("fs_nofixcurvatures", {
  it("returns character vector", {
    result <- fs_nofixcurvatures()

    expect_type(result, "character")
    expect_true(length(result) > 0)
  })

  it("includes defect files", {
    result <- fs_nofixcurvatures()

    expect_true("defect_borders" %in% result)
    expect_true("defect_labels" %in% result)
  })
})


describe("check_fs", {
  it("returns logical", {
    result <- check_fs()
    expect_type(result, "logical")
  })

  it("aborts when abort = TRUE and FS not installed", {
    skip_if(freesurfer::have_fs(), "FreeSurfer is installed")

    expect_error(check_fs(abort = TRUE), "Freesurfer")
  })

  it("does not abort when abort = FALSE", {
    result <- check_fs(abort = FALSE)
    expect_type(result, "logical")
  })
})
