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


describe("mri_vol2surf", {
  it("constructs correct command", {
    captured_cmd <- NULL
    local_mocked_bindings(
      check_fs = function(abort = FALSE) invisible(TRUE),
      run_cmd = function(cmd, verbose = FALSE) {
        captured_cmd <<- cmd
        invisible(NULL)
      }
    )

    mri_vol2surf(
      input_file = "input.mgz",
      output_file = "output.mgz",
      hemisphere = "lh",
      verbose = FALSE
    )

    expect_match(captured_cmd, "mri_vol2surf")
    expect_match(captured_cmd, "--mov input.mgz")
    expect_match(captured_cmd, "--o output.mgz")
    expect_match(captured_cmd, "--hemi lh")
    expect_match(captured_cmd, "--projfrac 0.5")
  })
})


describe("mri_pretess", {
  it("constructs correct command", {
    captured_cmd <- NULL
    local_mocked_bindings(
      check_fs = function(abort = FALSE) invisible(TRUE),
      run_cmd = function(cmd, verbose = FALSE) {
        captured_cmd <<- cmd
        invisible(NULL)
      }
    )

    mri_pretess(
      template = "vol.mgz",
      label = 10,
      output_file = "pretess.mgz",
      verbose = FALSE
    )

    expect_match(captured_cmd, "mri_pretess")
    expect_match(captured_cmd, "vol.mgz")
    expect_match(captured_cmd, "10")
    expect_match(captured_cmd, "pretess.mgz")
  })
})


describe("mri_tessellate", {
  it("constructs correct command", {
    captured_cmd <- NULL
    local_mocked_bindings(
      check_fs = function(abort = FALSE) invisible(TRUE),
      run_cmd = function(cmd, verbose = FALSE) {
        captured_cmd <<- cmd
        invisible(NULL)
      }
    )

    mri_tessellate(
      input_file = "pretess.mgz",
      label = 10,
      output_file = "tess",
      verbose = FALSE
    )

    expect_match(captured_cmd, "mri_tessellate")
    expect_match(captured_cmd, "pretess.mgz")
    expect_match(captured_cmd, "10")
    expect_match(captured_cmd, "tess")
  })
})


describe("mri_smooth", {
  it("constructs correct command", {
    captured_cmd <- NULL
    local_mocked_bindings(
      check_fs = function(abort = FALSE) invisible(TRUE),
      run_cmd = function(cmd, verbose = FALSE) {
        captured_cmd <<- cmd
        invisible(NULL)
      }
    )

    mri_smooth(
      input_file = "tess",
      output_file = "smooth",
      verbose = FALSE
    )

    expect_match(captured_cmd, "mris_smooth")
    expect_match(captured_cmd, "-nw")
  })
})


describe("mri_surf2surf_rereg", {
  it("constructs correct command", {
    captured_cmd <- NULL
    local_mocked_bindings(
      check_fs = function(abort = FALSE) invisible(TRUE),
      run_cmd = function(cmd, verbose = FALSE) {
        captured_cmd <<- cmd
        invisible(NULL)
      }
    )

    tmp <- withr::local_tempdir()

    mri_surf2surf_rereg(
      subject = "bert",
      annot = "aparc.DKTatlas",
      hemi = "lh",
      output_dir = tmp,
      verbose = FALSE
    )

    expect_match(captured_cmd, "mri_surf2surf")
    expect_match(captured_cmd, "--srcsubject bert")
    expect_match(captured_cmd, "--sval-annot aparc.DKTatlas")
    expect_match(captured_cmd, "--hemi lh")
  })
})
