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
    local_mocked_bindings(
      have_fs = function() FALSE,
      .package = "freesurfer"
    )

    expect_error(check_fs(abort = TRUE), "Freesurfer")
  })

  it("shows danger message when abort = FALSE and FS not installed", {
    local_mocked_bindings(
      have_fs = function() FALSE,
      .package = "freesurfer"
    )

    expect_message(
      result <- check_fs(abort = FALSE),
      "Freesurfer"
    )
    expect_false(result)
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

  it("appends opts to command", {
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
      opts = "--keep",
      verbose = FALSE
    )

    expect_match(captured_cmd, "--keep")
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

  it("appends opts to command", {
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
      opts = "--extra-flag",
      verbose = FALSE
    )

    expect_match(captured_cmd, "--extra-flag")
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

  it("appends opts to command", {
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
      opts = "--seed 42",
      verbose = FALSE
    )

    expect_match(captured_cmd, "--seed 42")
  })
})


describe("mri_vol2label", {
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

    mri_vol2label(
      input_file = "input.mgz",
      label_id = 10,
      hemisphere = "lh",
      output_dir = tmp,
      verbose = FALSE
    )

    expect_match(captured_cmd, "mri_vol2label")
    expect_match(captured_cmd, "--c input.mgz")
    expect_match(captured_cmd, "--id 10")
    expect_match(captured_cmd, "--l")
  })

  it("includes surface option when specified", {
    captured_cmd <- NULL
    local_mocked_bindings(
      check_fs = function(abort = FALSE) invisible(TRUE),
      run_cmd = function(cmd, verbose = FALSE) {
        captured_cmd <<- cmd
        invisible(NULL)
      }
    )

    tmp <- withr::local_tempdir()

    mri_vol2label(
      input_file = "input.mgz",
      label_id = 10,
      hemisphere = "rh",
      output_dir = tmp,
      surface = "white",
      verbose = FALSE
    )

    expect_match(captured_cmd, "--surf fsaverage5 rh")
  })

  it("appends opts to command", {
    captured_cmd <- NULL
    local_mocked_bindings(
      check_fs = function(abort = FALSE) invisible(TRUE),
      run_cmd = function(cmd, verbose = FALSE) {
        captured_cmd <<- cmd
        invisible(NULL)
      }
    )

    tmp <- withr::local_tempdir()

    mri_vol2label(
      input_file = "input.mgz",
      label_id = 10,
      hemisphere = "lh",
      output_dir = tmp,
      opts = " --extra-opt",
      verbose = FALSE
    )

    expect_match(captured_cmd, "--extra-opt")
  })
})


describe("mri_vol2surf with opts", {
  it("appends opts to command", {
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
      opts = "--interp trilinear",
      verbose = FALSE
    )

    expect_match(captured_cmd, "--interp trilinear")
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


describe("surf2asc", {
  it("errors when output_file doesn't end with dpv", {
    local_mocked_bindings(
      check_fs = function(abort = FALSE) invisible(TRUE)
    )
    expect_output(
      expect_error(
        surf2asc("input", "output.txt", verbose = FALSE)
      ),
      "dpv"
    )
  })

  it("returns NULL when input_file doesn't exist", {
    local_mocked_bindings(
      check_fs = function(abort = FALSE) invisible(TRUE)
    )
    result <- surf2asc("/nonexistent/file", "output.dpv", verbose = FALSE)
    expect_null(result)
  })

  it("warns when input_file doesn't exist and verbose is TRUE", {
    local_mocked_bindings(
      check_fs = function(abort = FALSE) invisible(TRUE)
    )
    expect_warning(
      result <- surf2asc("/nonexistent/file", "output.dpv", verbose = TRUE),
      "Inputfile does not exist"
    )
    expect_null(result)
  })

  it("shows alert when verbose is TRUE and file exists", {
    tmp <- withr::local_tempdir()
    input <- file.path(tmp, "lh.white")
    writeLines("fake surface", input)
    output <- file.path(tmp, "lh.white.dpv")

    local_mocked_bindings(
      check_fs = function(abort = FALSE) invisible(TRUE),
      read_dpv = function(path) data.frame(x = 1)
    )
    local_mocked_bindings(
      mris_convert = function(infile, outfile, verbose = FALSE) {
        writeLines(
          c(
            "#!ascii",
            "2 1",
            "0.0 0.0 0.0 0",
            "1.0 1.0 1.0 0",
            "0 1 0 0"
          ),
          outfile
        )
      },
      .package = "freesurfer"
    )

    expect_message(
      result <- surf2asc(input, output, verbose = TRUE),
      "Saving"
    )
    expect_s3_class(result, "data.frame")
  })

  it("calls mris_convert with correct args when file exists", {
    tmp <- withr::local_tempdir()
    input <- file.path(tmp, "lh.white")
    writeLines("fake surface", input)
    output <- file.path(tmp, "lh.white.dpv")

    local_mocked_bindings(
      check_fs = function(abort = FALSE) invisible(TRUE),
      read_dpv = function(path) data.frame(x = 1)
    )
    local_mocked_bindings(
      mris_convert = function(infile, outfile, verbose = FALSE) {
        writeLines(
          c(
            "#!ascii",
            "2 1",
            "0.0 0.0 0.0 0",
            "1.0 1.0 1.0 0",
            "0 1 0 0"
          ),
          outfile
        )
      },
      .package = "freesurfer"
    )

    result <- surf2asc(input, output, verbose = FALSE)
    expect_s3_class(result, "data.frame")
  })
})


describe("curv2asc", {
  it("errors when output_file doesn't end with dpv", {
    local_mocked_bindings(
      check_fs = function(abort = FALSE) invisible(TRUE)
    )
    expect_output(
      expect_error(
        curv2asc("input", "white", "output.txt", verbose = FALSE)
      ),
      "dpv"
    )
  })

  it("returns NULL when input_file doesn't exist", {
    local_mocked_bindings(
      check_fs = function(abort = FALSE) invisible(TRUE)
    )
    result <- curv2asc(
      "/nonexistent/file",
      "white",
      "output.dpv",
      verbose = FALSE
    )
    expect_null(result)
  })

  it("prints message when input_file doesn't exist and verbose is TRUE", {
    local_mocked_bindings(
      check_fs = function(abort = FALSE) invisible(TRUE)
    )
    expect_output(
      result <- curv2asc(
        "/nonexistent/file",
        "white",
        "output.dpv",
        verbose = TRUE
      ),
      "Inputfile does not exist"
    )
    expect_null(result)
  })

  it("runs mris_convert -c and returns data when input file exists", {
    tmp <- withr::local_tempdir()
    input <- file.path(tmp, "lh.thickness")
    white <- file.path(tmp, "lh.white")
    writeLines("fake curvature", input)
    writeLines("fake white", white)
    output <- file.path(tmp, "lh.thickness.dpv")
    asc_output <- file.path(tmp, "lh.thickness.asc")

    captured_cmd <- NULL
    local_mocked_bindings(
      check_fs = function(abort = FALSE) invisible(TRUE),
      run_cmd = function(cmd, verbose = FALSE) {
        captured_cmd <<- cmd
        file.create(asc_output)
        invisible(NULL)
      },
      read_dpv = function(path) data.frame(x = 1)
    )

    result <- curv2asc(input, white, output, verbose = FALSE)

    expect_match(captured_cmd, "mris_convert -c")
    expect_match(captured_cmd, input)
    expect_match(captured_cmd, white)
    expect_s3_class(result, "data.frame")
  })

  it("prints saving message when verbose is TRUE and input exists", {
    tmp <- withr::local_tempdir()
    input <- file.path(tmp, "lh.thickness")
    white <- file.path(tmp, "lh.white")
    writeLines("fake curvature", input)
    writeLines("fake white", white)
    output <- file.path(tmp, "lh.thickness.dpv")
    asc_output <- file.path(tmp, "lh.thickness.asc")

    local_mocked_bindings(
      check_fs = function(abort = FALSE) invisible(TRUE),
      run_cmd = function(cmd, verbose = FALSE) {
        file.create(asc_output)
        invisible(NULL)
      },
      read_dpv = function(path) data.frame(x = 1)
    )

    expect_output(
      result <- curv2asc(input, white, output, verbose = TRUE),
      "Saving"
    )
    expect_s3_class(result, "data.frame")
  })
})


describe("asc2ply", {
  it("converts ASCII surface to PLY format", {
    tmp_asc <- withr::local_tempfile(fileext = ".dpv")
    writeLines(
      c(
        "#!ascii",
        "4 2",
        "0.0 0.0 0.0 0",
        "1.0 0.0 0.0 0",
        "1.0 1.0 0.0 0",
        "0.0 1.0 0.0 0",
        "0 1 2 0",
        "1 2 3 0"
      ),
      tmp_asc
    )

    local_mocked_bindings(
      read_ply_mesh = function(f, ...) {
        list(
          vertices = data.frame(x = 1:4, y = 1:4, z = 1:4),
          faces = data.frame(i = c(0, 1), j = c(1, 2), k = c(2, 3))
        )
      }
    )

    result <- asc2ply(tmp_asc)
    ply_file <- gsub("\\.dpv", ".ply", tmp_asc)
    expect_true(file.exists(ply_file))

    ply_content <- readLines(ply_file)
    expect_equal(ply_content[1], "ply")
    expect_match(ply_content[3], "element vertex 4")
    expect_match(ply_content[7], "element face 2")
  })
})


describe("smooth2srf", {
  it("errors when output_file doesn't end with srf", {
    local_mocked_bindings(
      check_fs = function(abort = FALSE) invisible(TRUE)
    )
    expect_output(
      expect_error(
        smooth2srf("input", "output.txt", verbose = FALSE)
      ),
      "srf"
    )
  })

  it("calls mris_convert with correct command", {
    captured_cmd <- NULL
    local_mocked_bindings(
      check_fs = function(abort = FALSE) invisible(TRUE),
      run_cmd = function(cmd, verbose = FALSE) {
        captured_cmd <<- cmd
        invisible(NULL)
      }
    )

    tmp <- withr::local_tempdir()
    input <- file.path(tmp, "smooth")
    output <- file.path(tmp, "output.srf")
    asc_file <- file.path(tmp, "output.asc")
    file.create(asc_file)

    smooth2srf(input, output, verbose = FALSE)
    expect_match(captured_cmd, "mris_convert")
    expect_match(captured_cmd, input)
  })
})


describe("surf2ply", {
  it("converts surface file to ply via surf2asc and asc2ply", {
    skip_on_covr()

    local_mocked_bindings(
      surf2asc = function(input_file, output_file, verbose) invisible(NULL),
      asc2ply = function(input_file, output_file) {
        list(
          vertices = data.frame(x = 1:3, y = 1:3, z = 1:3),
          faces = data.frame(i = 0, j = 1, k = 2)
        )
      }
    )

    result <- surf2ply("lh.white", "lh.white.ply", verbose = FALSE)

    expect_true(!is.null(result))
    expect_true("vertices" %in% names(result))
  })

  it("executes body lines with mocked intermediate functions", {
    pkg_ns <- asNamespace("ggseg.extra")
    orig_surf2asc <- pkg_ns$surf2asc
    orig_asc2ply <- pkg_ns$asc2ply

    unlockBinding("surf2asc", pkg_ns)
    unlockBinding("asc2ply", pkg_ns)
    assign("surf2asc", function(...) invisible(NULL), envir = pkg_ns)
    assign(
      "asc2ply",
      function(...) {
        list(
          vertices = data.frame(x = 1:3, y = 1:3, z = 1:3),
          faces = data.frame(i = 0, j = 1, k = 2)
        )
      },
      envir = pkg_ns
    )
    withr::defer({
      assign("surf2asc", orig_surf2asc, envir = pkg_ns)
      assign("asc2ply", orig_asc2ply, envir = pkg_ns)
      lockBinding("surf2asc", pkg_ns)
      lockBinding("asc2ply", pkg_ns)
    })

    result <- surf2ply("lh.white", "lh.white.ply", verbose = FALSE)

    expect_true(!is.null(result))
    expect_true("vertices" %in% names(result))
    expect_true("faces" %in% names(result))
  })
})


describe("curv2ply", {
  it("converts curvature file to ply via curv2asc and asc2ply", {
    skip_on_covr()

    local_mocked_bindings(
      curv2asc = function(input_file, white, output_file, verbose) {
        invisible(NULL)
      },
      asc2ply = function(input_file, output_file) {
        list(
          vertices = data.frame(x = 1:3, y = 1:3, z = 1:3),
          faces = data.frame(i = 0, j = 1, k = 2)
        )
      }
    )

    result <- curv2ply("lh.thickness", "lh.thickness.ply", verbose = FALSE)

    expect_true(!is.null(result))
    expect_true("vertices" %in% names(result))
  })

  it("executes body lines with mocked intermediate functions", {
    pkg_ns <- asNamespace("ggseg.extra")
    orig_curv2asc <- pkg_ns$curv2asc
    orig_asc2ply <- pkg_ns$asc2ply

    unlockBinding("curv2asc", pkg_ns)
    unlockBinding("asc2ply", pkg_ns)
    assign("curv2asc", function(...) invisible(NULL), envir = pkg_ns)
    assign(
      "asc2ply",
      function(...) {
        list(
          vertices = data.frame(x = 1:3, y = 1:3, z = 1:3),
          faces = data.frame(i = 0, j = 1, k = 2)
        )
      },
      envir = pkg_ns
    )
    withr::defer({
      assign("curv2asc", orig_curv2asc, envir = pkg_ns)
      assign("asc2ply", orig_asc2ply, envir = pkg_ns)
      lockBinding("curv2asc", pkg_ns)
      lockBinding("asc2ply", pkg_ns)
    })

    result <- curv2ply("lh.thickness", "lh.thickness.ply", verbose = FALSE)

    expect_true(!is.null(result))
    expect_true("vertices" %in% names(result))
    expect_true("faces" %in% names(result))
  })
})


describe("lcbc_surf2surf", {
  it("passes correct args to mri_surf2surf", {
    captured_args <- list()
    local_mocked_bindings(
      check_fs = function(abort = FALSE) invisible(TRUE),
      mri_surf2surf = function(...) {
        captured_args <<- list(...)
        invisible(NULL)
      }
    )

    tmp <- withr::local_tempdir()

    lcbc_surf2surf(
      input_volume = "input.mgz",
      source_subject = "fsaverage",
      target_subject = "fsaverage5",
      hemisphere = "lh",
      subjects_dir = tmp,
      output_dir = file.path(tmp, "surf/"),
      verbose = FALSE
    )

    expect_equal(captured_args$sval, "input.mgz")
    expect_equal(captured_args$subject, "fsaverage")
    expect_equal(captured_args$target_subject, "fsaverage5")
    expect_equal(captured_args$hemi, "lh")
  })
})
