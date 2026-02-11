describe("detect_hemi_from_gifti_filename", {
  it("detects lh from lh. prefix", {
    expect_equal(detect_hemi_from_gifti_filename("lh.aparc.label.gii"), "lh")
  })

  it("detects rh from rh. prefix", {
    expect_equal(detect_hemi_from_gifti_filename("rh.aparc.label.gii"), "rh")
  })

  it("detects left from .L. pattern", {
    expect_equal(
      detect_hemi_from_gifti_filename("aparc.L.label.gii"),
      "lh"
    )
  })

  it("detects right from .R. pattern", {
    expect_equal(
      detect_hemi_from_gifti_filename("aparc.R.label.gii"),
      "rh"
    )
  })

  it("detects from _lh_ pattern", {
    expect_equal(
      detect_hemi_from_gifti_filename("atlas_lh_aparc.label.gii"),
      "lh"
    )
  })

  it("returns NA for undetectable hemisphere", {
    expect_true(is.na(detect_hemi_from_gifti_filename("aparc.label.gii")))
  })
})


describe("read_gifti_annotation", {
  it("errors when file does not exist", {
    expect_error(
      read_gifti_annotation("nonexistent.label.gii"),
      "not found"
    )
  })

  it("returns tibble with correct structure", {
    skip_if_not_installed("freesurferformats")

    mock_annot <- list(
      label_codes = c(1L, 1L, 2L, 2L, 0L),
      colortable_df = data.frame(
        struct_name = c("region_a", "region_b"),
        r = c(255L, 0L),
        g = c(0L, 255L),
        b = c(0L, 0L),
        a = c(0L, 0L),
        code = c(1L, 2L),
        hex_color_string_rgb = c("#FF0000", "#00FF00"),
        hex_color_string_rgba = c("#FF000000", "#00FF0000"),
        struct_index = c(0L, 1L),
        stringsAsFactors = FALSE
      )
    )

    local_mocked_bindings(
      read.fs.annot.gii = function(...) mock_annot,
      .package = "freesurferformats"
    )

    tmp <- withr::local_tempfile(
      pattern = "lh.test",
      fileext = ".label.gii"
    )
    writeLines("mock", tmp)

    result <- read_gifti_annotation(tmp)

    expect_s3_class(result, "tbl_df")
    expect_named(result, c("hemi", "region", "label", "colour", "vertices"))
    expect_true(all(result$hemi == "left"))
    expect_true("lh_region_a" %in% result$label)
    expect_true("lh_region_b" %in% result$label)
  })

  it("extracts 0-indexed vertex indices", {
    skip_if_not_installed("freesurferformats")

    mock_annot <- list(
      label_codes = c(1L, 1L, 2L),
      colortable_df = data.frame(
        struct_name = c("a", "b"),
        r = c(100L, 200L),
        g = c(100L, 200L),
        b = c(100L, 200L),
        a = c(0L, 0L),
        code = c(1L, 2L),
        hex_color_string_rgb = c("#646464", "#C8C8C8"),
        hex_color_string_rgba = c("#64646400", "#C8C8C800"),
        struct_index = c(0L, 1L),
        stringsAsFactors = FALSE
      )
    )

    local_mocked_bindings(
      read.fs.annot.gii = function(...) mock_annot,
      .package = "freesurferformats"
    )

    tmp <- withr::local_tempfile(
      pattern = "rh.test",
      fileext = ".label.gii"
    )
    writeLines("mock", tmp)

    result <- read_gifti_annotation(tmp)
    region_a <- result[result$region == "a", ]

    expect_equal(region_a$vertices[[1]], c(0L, 1L))
  })

  it("warns and skips files with undetectable hemisphere", {
    skip_if_not_installed("freesurferformats")

    tmp <- withr::local_tempfile(
      pattern = "unknown",
      fileext = ".label.gii"
    )
    writeLines("mock", tmp)

    expect_warning(
      result <- read_gifti_annotation(tmp),
      "Cannot detect hemisphere"
    )
    expect_equal(nrow(result), 0)
  })
})


describe("read_cifti_annotation", {
  it("errors when file does not exist", {
    skip_if_not_installed("ciftiTools")

    expect_error(
      read_cifti_annotation("nonexistent.dlabel.nii"),
      "not found"
    )
  })

  it("returns tibble with correct structure", {
    skip_if_not_installed("ciftiTools")

    n <- 10242L
    mock_cii <- list(
      data = list(
        cortex_left = matrix(c(rep(1L, 5000), rep(2L, 5242)), ncol = 1),
        cortex_right = matrix(c(rep(1L, 4000), rep(2L, 6242)), ncol = 1)
      ),
      meta = list(
        cifti = list(
          labels = list(
            data.frame(
              Key = c(1L, 2L),
              Label = c("region_a", "region_b"),
              Red = c(1, 0),
              Green = c(0, 1),
              Blue = c(0, 0),
              stringsAsFactors = FALSE
            )
          )
        )
      )
    )

    local_mocked_bindings(
      read_cifti = function(...) mock_cii,
      .package = "ciftiTools"
    )

    tmp <- withr::local_tempfile(fileext = ".dlabel.nii")
    writeLines("mock", tmp)

    result <- read_cifti_annotation(tmp)

    expect_s3_class(result, "tbl_df")
    expect_named(result, c("hemi", "region", "label", "colour", "vertices"))
    expect_true("left" %in% result$hemi)
    expect_true("right" %in% result$hemi)
    expect_true("lh_region_a" %in% result$label)
    expect_true("rh_region_b" %in% result$label)
  })

  it("uses maxColorValue = 1 for CIFTI colours", {
    skip_if_not_installed("ciftiTools")

    n <- 10242L
    mock_cii <- list(
      data = list(
        cortex_left = matrix(rep(1L, n), ncol = 1),
        cortex_right = NULL
      ),
      meta = list(
        cifti = list(
          labels = list(
            data.frame(
              Key = 1L,
              Label = "test_region",
              Red = 1.0,
              Green = 0.0,
              Blue = 0.0,
              stringsAsFactors = FALSE
            )
          )
        )
      )
    )

    local_mocked_bindings(
      read_cifti = function(...) mock_cii,
      .package = "ciftiTools"
    )

    tmp <- withr::local_tempfile(fileext = ".dlabel.nii")
    writeLines("mock", tmp)

    result <- read_cifti_annotation(tmp)
    expect_equal(result$colour[result$region == "test_region"], "#FF0000")
  })

  it("errors when vertex count does not match fsaverage5", {
    skip_if_not_installed("ciftiTools")

    mock_cii <- list(
      data = list(
        cortex_left = matrix(rep(1L, 5000), ncol = 1),
        cortex_right = NULL
      ),
      meta = list(
        cifti = list(
          labels = list(
            data.frame(
              Key = 1L,
              Label = "test",
              Red = 1.0,
              Green = 0.0,
              Blue = 0.0,
              stringsAsFactors = FALSE
            )
          )
        )
      )
    )

    local_mocked_bindings(
      read_cifti = function(...) mock_cii,
      .package = "ciftiTools"
    )

    tmp <- withr::local_tempfile(fileext = ".dlabel.nii")
    writeLines("mock", tmp)

    expect_error(
      read_cifti_annotation(tmp),
      "expected 10242"
    )
  })
})


describe("create_atlas_from_gifti", {
  it("creates ggseg_atlas with steps = 1", {
    skip_if_not_installed("freesurferformats")

    mock_annot <- list(
      label_codes = c(1L, 1L, 2L, 2L, 0L),
      colortable_df = data.frame(
        struct_name = c("region_a", "region_b"),
        r = c(255L, 0L),
        g = c(0L, 255L),
        b = c(0L, 0L),
        a = c(0L, 0L),
        code = c(1L, 2L),
        hex_color_string_rgb = c("#FF0000", "#00FF00"),
        hex_color_string_rgba = c("#FF000000", "#00FF0000"),
        struct_index = c(0L, 1L),
        stringsAsFactors = FALSE
      )
    )

    local_mocked_bindings(
      read.fs.annot.gii = function(...) mock_annot,
      .package = "freesurferformats"
    )
    local_mocked_bindings(
      is_interactive = function() FALSE,
      preview_atlas = function(atlas) invisible(atlas)
    )

    lh <- withr::local_tempfile(pattern = "lh.test", fileext = ".label.gii")
    rh <- withr::local_tempfile(pattern = "rh.test", fileext = ".label.gii")
    writeLines("mock", lh)
    writeLines("mock", rh)

    result <- create_atlas_from_gifti(
      gifti_files = c(lh, rh),
      atlas_name = "test_gifti",
      steps = 1,
      verbose = FALSE,
      cleanup = FALSE
    )

    expect_s3_class(result, "ggseg_atlas")
    expect_true(nrow(result$core) > 0)
  })
})


describe("create_atlas_from_cifti", {
  it("creates ggseg_atlas with steps = 1", {
    skip_if_not_installed("ciftiTools")

    n <- 10242L
    mock_cii <- list(
      data = list(
        cortex_left = matrix(c(rep(1L, 5000), rep(2L, 5242)), ncol = 1),
        cortex_right = matrix(c(rep(1L, 4000), rep(2L, 6242)), ncol = 1)
      ),
      meta = list(
        cifti = list(
          labels = list(
            data.frame(
              Key = c(1L, 2L),
              Label = c("region_a", "region_b"),
              Red = c(1, 0),
              Green = c(0, 1),
              Blue = c(0, 0),
              stringsAsFactors = FALSE
            )
          )
        )
      )
    )

    local_mocked_bindings(
      read_cifti = function(...) mock_cii,
      .package = "ciftiTools"
    )
    local_mocked_bindings(
      is_interactive = function() FALSE,
      preview_atlas = function(atlas) invisible(atlas)
    )

    tmp <- withr::local_tempfile(fileext = ".dlabel.nii")
    writeLines("mock", tmp)

    result <- create_atlas_from_cifti(
      cifti_file = tmp,
      atlas_name = "test_cifti",
      steps = 1,
      verbose = FALSE,
      cleanup = FALSE
    )

    expect_s3_class(result, "ggseg_atlas")
    expect_true(nrow(result$core) > 0)
    expect_true("left" %in% result$core$hemi)
    expect_true("right" %in% result$core$hemi)
  })
})
