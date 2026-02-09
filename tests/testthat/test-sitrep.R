describe("setup_sitrep", {
  it("returns list of results invisibly", {
    result <- setup_sitrep("simple")

    expect_type(result, "list")
    expect_true("freesurfer" %in% names(result))
    expect_true("system" %in% names(result))
    expect_true("fsaverage" %in% names(result))
  })

  it("accepts detail parameter", {
    expect_no_error(setup_sitrep("simple"))
    expect_no_error(setup_sitrep("full"))
  })

  it("validates detail argument", {
    expect_error(setup_sitrep("invalid"), "arg")
  })
})


describe("check_freesurfer", {
  it("returns list with available field", {
    result <- check_freesurfer("simple")

    expect_type(result, "list")
    expect_true("available" %in% names(result))
    expect_type(result$available, "logical")
  })
})


describe("check_other_system_deps", {
  it("returns list with imagemagick and chrome fields", {
    result <- check_other_system_deps("simple")

    expect_type(result, "list")
    expect_true("imagemagick" %in% names(result))
    expect_true("chrome" %in% names(result))
    expect_type(result$imagemagick, "logical")
    expect_type(result$chrome, "logical")
  })
})


describe("check_fsaverage", {
  it("returns list with fsaverage5 field", {
    result <- check_fsaverage("simple")

    expect_type(result, "list")
    expect_true("fsaverage5" %in% names(result))
    expect_type(result$fsaverage5, "logical")
  })
})


describe("check_freesurfer", {
  it("alerts danger when FreeSurfer not configured in simple mode", {
    local_mocked_bindings(
      have_fs = function() FALSE,
      .package = "freesurfer"
    )
    msgs <- capture.output(check_freesurfer("simple"), type = "message")
    expect_true(any(grepl("not configured", msgs)))
  })
})


describe("check_other_system_deps", {
  it("shows install URL when ImageMagick missing in full detail", {
    local_mocked_bindings(
      has_magick = function() FALSE
    )
    local_mocked_bindings(
      find_chrome = function() "/usr/bin/chromium",
      .package = "chromote"
    )
    msgs <- capture.output(check_other_system_deps("full"), type = "message")
    expect_true(any(grepl("imagemagick.org", msgs)))
  })

  it("shows help text when Chrome missing in full detail", {
    local_mocked_bindings(
      has_magick = function() TRUE
    )
    local_mocked_bindings(
      find_chrome = function() NULL,
      .package = "chromote"
    )
    msgs <- capture.output(check_other_system_deps("full"), type = "message")
    expect_true(any(grepl("Install Chrome", msgs)))
  })
})


describe("check_fsaverage", {
  it("alerts when fsaverage5 not found", {
    local_mocked_bindings(
      fs_subj_dir = function() "/nonexistent/path",
      .package = "freesurfer"
    )
    msgs <- capture.output(check_fsaverage("simple"), type = "message")
    expect_true(any(grepl("not found", msgs)))
  })
})


describe("summarize_sitrep", {
  it("handles all-true results", {
    results <- list(
      freesurfer = list(available = TRUE),
      system = list(imagemagick = TRUE, chrome = TRUE),
      fsaverage = list(fsaverage5 = TRUE)
    )

    expect_message(summarize_sitrep(results, "simple"), "Ready")
  })

  it("handles missing requirements", {
    results <- list(
      freesurfer = list(available = FALSE),
      system = list(imagemagick = TRUE, chrome = TRUE),
      fsaverage = list(fsaverage5 = TRUE)
    )

    expect_message(summarize_sitrep(results, "simple"), "Missing")
  })
})
