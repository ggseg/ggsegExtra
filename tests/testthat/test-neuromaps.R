describe("detect_hemi_from_neuromaps_filename", {
  it("detects left from hemi-L BIDS pattern", {
    expect_equal(
      detect_hemi_from_neuromaps_filename(
        "source-abagen_desc-genepc1_space-fsaverage_den-10k_hemi-L_feature.func.gii"
      ),
      "lh"
    )
  })

  it("detects right from hemi-R BIDS pattern", {
    expect_equal(
      detect_hemi_from_neuromaps_filename(
        "source-abagen_desc-genepc1_space-fsaverage_den-10k_hemi-R_feature.func.gii"
      ),
      "rh"
    )
  })

  it("falls back to gifti detection for non-BIDS names", {
    expect_equal(
      detect_hemi_from_neuromaps_filename("lh.aparc.func.gii"),
      "lh"
    )
  })

  it("returns NA for undetectable hemisphere", {
    expect_true(is.na(
      detect_hemi_from_neuromaps_filename("unknown_file.func.gii")
    ))
  })
})


describe("read_neuromaps_annotation", {
  it("errors when file does not exist", {
    expect_error(
      read_neuromaps_annotation("nonexistent.func.gii"),
      "not found"
    )
  })

  it("errors for volume files", {
    tmp <- withr::local_tempfile(fileext = ".nii.gz")
    writeLines("mock", tmp)

    expect_error(
      read_neuromaps_annotation(tmp),
      "Volume files are not supported"
    )
  })

  it("validates label_table columns", {
    skip_if_not_installed("gifti")

    bad_lt <- data.frame(name = "x", stringsAsFactors = FALSE)

    tmp <- withr::local_tempfile(
      pattern = "source-test_hemi-L_feature",
      fileext = ".func.gii"
    )
    writeLines("mock", tmp)

    expect_error(
      read_neuromaps_annotation(tmp, label_table = bad_lt),
      "label_table.*must have columns"
    )
  })

  it("returns tibble with correct structure", {
    skip_if_not_installed("gifti")

    n <- 10242L
    mock_gii <- list(data = list(
      c(rep(0, 2000), rep(1, 4000), rep(2, 4242))
    ))

    local_mocked_bindings(
      read_gifti = function(...) mock_gii,
      .package = "gifti"
    )

    tmp <- withr::local_tempfile(
      pattern = "source-test_desc-test_space-fsaverage_den-10k_hemi-L_feature",
      fileext = ".func.gii"
    )
    writeLines("mock", tmp)

    result <- read_neuromaps_annotation(tmp)

    expect_s3_class(result, "tbl_df")
    expect_named(result, c("hemi", "region", "label", "colour", "vertices"))
    expect_true(all(result$hemi == "left"))
    expect_true("lh_parcel_1" %in% result$label)
    expect_true("lh_parcel_2" %in% result$label)
    expect_true("lh_unknown" %in% result$label)
  })

  it("uses label_table for custom region names and colours", {
    skip_if_not_installed("gifti")

    n <- 10242L
    mock_gii <- list(data = list(
      c(rep(0, 2000), rep(1, 4000), rep(2, 4242))
    ))

    local_mocked_bindings(
      read_gifti = function(...) mock_gii,
      .package = "gifti"
    )

    tmp <- withr::local_tempfile(
      pattern = "source-test_hemi-L_feature",
      fileext = ".func.gii"
    )
    writeLines("mock", tmp)

    lt <- data.frame(
      id = c(1L, 2L),
      region = c("motor_cortex", "visual_cortex"),
      colour = c("#FF0000", "#00FF00"),
      stringsAsFactors = FALSE
    )

    result <- read_neuromaps_annotation(tmp, label_table = lt)

    expect_true("lh_motor_cortex" %in% result$label)
    expect_true("lh_visual_cortex" %in% result$label)
    expect_equal(
      result$colour[result$region == "motor_cortex"],
      "#FF0000"
    )
  })

  it("extracts 0-indexed vertex indices", {
    skip_if_not_installed("gifti")

    n <- 10242L
    values <- rep(0, n)
    values[1:3] <- 1
    mock_gii <- list(data = list(values))

    local_mocked_bindings(
      read_gifti = function(...) mock_gii,
      .package = "gifti"
    )

    tmp <- withr::local_tempfile(
      pattern = "source-test_hemi-R_feature",
      fileext = ".func.gii"
    )
    writeLines("mock", tmp)

    result <- read_neuromaps_annotation(tmp)
    parcel_1 <- result[result$region == "parcel_1", ]

    expect_equal(parcel_1$vertices[[1]], c(0L, 1L, 2L))
  })

  it("errors when vertex count does not match fsaverage5", {
    skip_if_not_installed("gifti")

    mock_gii <- list(data = list(rep(1, 5000)))

    local_mocked_bindings(
      read_gifti = function(...) mock_gii,
      .package = "gifti"
    )

    tmp <- withr::local_tempfile(
      pattern = "source-test_hemi-L_feature",
      fileext = ".func.gii"
    )
    writeLines("mock", tmp)

    expect_error(
      read_neuromaps_annotation(tmp),
      "expected 10242"
    )
  })

  it("warns and skips files with undetectable hemisphere", {
    skip_if_not_installed("gifti")

    tmp <- withr::local_tempfile(
      pattern = "unknown",
      fileext = ".func.gii"
    )
    writeLines("mock", tmp)

    expect_warning(
      result <- read_neuromaps_annotation(tmp),
      "Cannot detect hemisphere"
    )
    expect_equal(nrow(result), 0)
  })
})


describe("create_atlas_from_neuromaps", {
  it("creates ggseg_atlas with steps = 1", {
    skip_if_not_installed("ggseg.hub")
    skip_if_not(
      exists("fetch_neuromaps_annotation", envir = asNamespace("ggseg.hub")),
      "ggseg.hub without neuromaps support"
    )
    skip_if_not_installed("gifti")

    n <- 10242L
    mock_gii <- list(data = list(
      c(rep(0, 2000), rep(1, 4000), rep(2, 4242))
    ))

    lh <- withr::local_tempfile(
      pattern = "source-test_hemi-L_feature",
      fileext = ".func.gii"
    )
    rh <- withr::local_tempfile(
      pattern = "source-test_hemi-R_feature",
      fileext = ".func.gii"
    )
    writeLines("mock", lh)
    writeLines("mock", rh)

    local_mocked_bindings(
      fetch_neuromaps_annotation = function(...) c(lh, rh),
      .package = "ggseg.hub"
    )
    local_mocked_bindings(
      read_gifti = function(...) mock_gii,
      .package = "gifti"
    )
    local_mocked_bindings(
      is_interactive = function() FALSE,
      preview_atlas = function(atlas) invisible(atlas)
    )

    result <- create_atlas_from_neuromaps(
      source = "test",
      desc = "testdesc",
      atlas_name = "test_neuromaps",
      steps = 1,
      verbose = FALSE,
      cleanup = FALSE
    )

    expect_s3_class(result, "ggseg_atlas")
    expect_true(nrow(result$core) > 0)
    expect_true("left" %in% result$core$hemi)
    expect_true("right" %in% result$core$hemi)
  })

  it("derives atlas_name from source and desc", {
    skip_if_not_installed("ggseg.hub")
    skip_if_not(
      exists("fetch_neuromaps_annotation", envir = asNamespace("ggseg.hub")),
      "ggseg.hub without neuromaps support"
    )
    skip_if_not_installed("gifti")

    n <- 10242L
    mock_gii <- list(data = list(
      c(rep(1, 5000), rep(2, 5242))
    ))

    tmp <- withr::local_tempfile(
      pattern = "source-abagen_hemi-L_feature",
      fileext = ".func.gii"
    )
    writeLines("mock", tmp)

    local_mocked_bindings(
      fetch_neuromaps_annotation = function(...) tmp,
      .package = "ggseg.hub"
    )
    local_mocked_bindings(
      read_gifti = function(...) mock_gii,
      .package = "gifti"
    )
    local_mocked_bindings(
      is_interactive = function() FALSE,
      preview_atlas = function(atlas) invisible(atlas)
    )

    result <- create_atlas_from_neuromaps(
      source = "abagen",
      desc = "genepc1",
      steps = 1,
      verbose = FALSE,
      cleanup = FALSE
    )

    expect_equal(result$atlas, "abagen_genepc1")
  })

  it("errors for volume annotations", {
    skip_if_not_installed("ggseg.hub")
    skip_if_not(
      exists("fetch_neuromaps_annotation", envir = asNamespace("ggseg.hub")),
      "ggseg.hub without neuromaps support"
    )

    local_mocked_bindings(
      fetch_neuromaps_annotation = function(...) "brain_map.nii.gz",
      .package = "ggseg.hub"
    )
    local_mocked_bindings(
      is_interactive = function() FALSE,
      preview_atlas = function(atlas) invisible(atlas)
    )

    expect_error(
      suppressWarnings(create_atlas_from_neuromaps(
        source = "test",
        desc = "vol",
        space = "MNI152",
        density = "2mm",
        steps = 1,
        verbose = FALSE
      )),
      "volume format"
    )
  })

  it("warns for non-default space/density", {
    skip_if_not_installed("ggseg.hub")
    skip_if_not(
      exists("fetch_neuromaps_annotation", envir = asNamespace("ggseg.hub")),
      "ggseg.hub without neuromaps support"
    )
    skip_if_not_installed("gifti")

    n <- 10242L
    mock_gii <- list(data = list(
      c(rep(1, 5000), rep(2, 5242))
    ))

    tmp <- withr::local_tempfile(
      pattern = "source-test_hemi-L_feature",
      fileext = ".func.gii"
    )
    writeLines("mock", tmp)

    local_mocked_bindings(
      fetch_neuromaps_annotation = function(...) tmp,
      .package = "ggseg.hub"
    )
    local_mocked_bindings(
      read_gifti = function(...) mock_gii,
      .package = "gifti"
    )
    local_mocked_bindings(
      is_interactive = function() FALSE,
      preview_atlas = function(atlas) invisible(atlas)
    )

    expect_warning(
      create_atlas_from_neuromaps(
        source = "test",
        desc = "test",
        space = "fsLR",
        density = "32k",
        steps = 1,
        verbose = FALSE,
        cleanup = FALSE
      ),
      "Non-default space/density"
    )
  })
})
