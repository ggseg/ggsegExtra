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
