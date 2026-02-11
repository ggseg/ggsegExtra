describe("wholebrain_classify_labels", {
  make_atlas_data <- function(labels, vertex_counts) {
    rows <- mapply(function(lbl, n) {
      tibble(
        hemi = "left",
        region = lbl,
        label = paste0("lh_", lbl),
        colour = "#FF0000",
        vertices = list(seq_len(n) - 1L),
        source_label = lbl,
        source_idx = match(lbl, labels)
      )
    }, labels, vertex_counts, SIMPLIFY = FALSE)
    bind_rows(rows)
  }

  it("classifies labels above threshold as cortical", {
    ad <- make_atlas_data(
      c("region_a", "region_b"),
      c(100, 200)
    )
    result <- wholebrain_classify_labels(ad, min_vertices = 50L)
    expect_true("region_a" %in% result$cortical_labels)
    expect_true("region_b" %in% result$cortical_labels)
    expect_length(result$subcortical_labels, 0)
  })

  it("classifies labels below threshold as subcortical", {
    ad <- make_atlas_data(
      c("region_a", "region_b"),
      c(10, 20)
    )
    result <- wholebrain_classify_labels(ad, min_vertices = 50L)
    expect_length(result$cortical_labels, 0)
    expect_true("region_a" %in% result$subcortical_labels)
    expect_true("region_b" %in% result$subcortical_labels)
  })

  it("splits labels correctly at threshold boundary", {
    ad <- make_atlas_data(
      c("big", "small", "exact"),
      c(100, 10, 50)
    )
    result <- wholebrain_classify_labels(ad, min_vertices = 50L)
    expect_true("big" %in% result$cortical_labels)
    expect_true("exact" %in% result$cortical_labels)
    expect_true("small" %in% result$subcortical_labels)
  })

  it("respects manual cortical_labels override", {
    ad <- make_atlas_data(
      c("region_a", "region_b"),
      c(10, 200)
    )
    result <- wholebrain_classify_labels(
      ad,
      min_vertices = 50L,
      cortical_labels = "region_a"
    )
    expect_true("region_a" %in% result$cortical_labels)
    expect_true("region_b" %in% result$cortical_labels)
    expect_length(result$subcortical_labels, 0)
  })

  it("respects manual subcortical_labels override", {
    ad <- make_atlas_data(
      c("region_a", "region_b"),
      c(100, 200)
    )
    result <- wholebrain_classify_labels(
      ad,
      min_vertices = 50L,
      subcortical_labels = "region_a"
    )
    expect_true("region_a" %in% result$subcortical_labels)
    expect_true("region_b" %in% result$cortical_labels)
  })

  it("manual overrides take precedence over auto-classification", {
    ad <- make_atlas_data(
      c("region_a", "region_b", "region_c"),
      c(100, 200, 5)
    )
    result <- wholebrain_classify_labels(
      ad,
      min_vertices = 50L,
      cortical_labels = "region_c",
      subcortical_labels = "region_a"
    )
    expect_setequal(result$cortical_labels, c("region_b", "region_c"))
    expect_equal(result$subcortical_labels, "region_a")
  })

  it("sums vertex counts across hemispheres", {
    ad <- bind_rows(
      tibble(
        hemi = "left", region = "r", label = "lh_r", colour = "#FF0000",
        vertices = list(seq_len(30) - 1L), source_label = "r", source_idx = 1L
      ),
      tibble(
        hemi = "right", region = "r", label = "rh_r", colour = "#FF0000",
        vertices = list(seq_len(30) - 1L), source_label = "r", source_idx = 1L
      )
    )
    result <- wholebrain_classify_labels(ad, min_vertices = 50L)
    expect_true("r" %in% result$cortical_labels)
  })

  it("returns vertex_counts in result", {
    ad <- make_atlas_data(c("a", "b"), c(100, 10))
    result <- wholebrain_classify_labels(ad, min_vertices = 50L)
    expect_true("vertex_counts" %in% names(result))
    expect_equal(as.integer(result$vertex_counts["a"]), 100L)
    expect_equal(as.integer(result$vertex_counts["b"]), 10L)
  })

  it("handles empty atlas data", {
    ad <- tibble(
      hemi = character(),
      region = character(),
      label = character(),
      colour = character(),
      vertices = list(),
      source_label = character(),
      source_idx = integer()
    )
    result <- wholebrain_classify_labels(ad, min_vertices = 50L)
    expect_length(result$cortical_labels, 0)
    expect_length(result$subcortical_labels, 0)
  })
})


describe("create_wholebrain_atlas validation", {
  it("requires FreeSurfer to be available", {
    local_mocked_bindings(
      check_fs = function(abort = FALSE) {
        if (abort) cli::cli_abort("FreeSurfer not found")
        FALSE
      }
    )
    expect_error(
      create_wholebrain_atlas("test.nii.gz", verbose = FALSE),
      "FreeSurfer"
    )
  })

  it("errors when volume file not found", {
    local_mocked_bindings(check_fs = function(...) TRUE)
    expect_error(
      create_wholebrain_atlas(
        input_volume = "nonexistent.nii.gz",
        verbose = FALSE
      ),
      "not found"
    )
  })

  it("errors when LUT file not found", {
    local_mocked_bindings(check_fs = function(...) TRUE)
    vol_file <- withr::local_tempfile(fileext = ".nii.gz")
    file.create(vol_file)
    expect_error(
      create_wholebrain_atlas(
        input_volume = vol_file,
        input_lut = "nonexistent_lut.txt",
        verbose = FALSE
      ),
      "not found"
    )
  })

  it("derives atlas_name from volume filename", {
    captured_name <- NULL
    test_dir <- withr::local_tempdir()
    local_mocked_bindings(
      check_fs = function(...) TRUE,
      setup_atlas_dirs = function(output_dir, atlas_name, ...) {
        captured_name <<- atlas_name
        list(
          base = test_dir,
          snapshots = test_dir,
          processed = test_dir,
          masks = test_dir,
          snapshots = test_dir
        )
      },
      load_or_run_step = function(step, steps, ...) {
        list(run = TRUE, data = list())
      },
      generate_colortable_from_volume = function(...) {
        data.frame(
          idx = 1L, label = "a", R = 255L, G = 0L, B = 0L, A = 0L,
          roi = "0001", color = "#FF0000", stringsAsFactors = FALSE
        )
      },
      wholebrain_project_to_surface = function(...) {
        tibble(
          hemi = character(), region = character(),
          label = character(), colour = character(),
          vertices = list(), source_label = character(),
          source_idx = integer()
        )
      },
      wholebrain_classify_labels = function(...) {
        list(
          cortical_labels = character(),
          subcortical_labels = character(),
          vertex_counts = integer()
        )
      }
    )

    vol_file <- file.path(withr::local_tempdir(), "test_vol.nii.gz")
    file.create(vol_file)

    expect_warning(
      result <- create_wholebrain_atlas(
        input_volume = vol_file,
        steps = 1:2,
        verbose = FALSE
      ),
      "No color lookup table"
    )
    expect_equal(captured_name, "test_vol")
  })
})


describe("create_wholebrain_atlas pipeline flow", {
  it("returns split data for steps 1:2", {
    test_dir <- withr::local_tempdir()
    local_mocked_bindings(
      check_fs = function(...) TRUE,
      setup_atlas_dirs = function(...) {
        list(
          base = test_dir,
          snapshots = test_dir,
          processed = test_dir,
          masks = test_dir,
          snapshots = test_dir
        )
      },
      load_or_run_step = function(step, steps, ...) {
        list(run = step %in% steps, data = list())
      },
      generate_colortable_from_volume = function(...) {
        data.frame(
          idx = c(1, 2), label = c("a", "b"),
          R = c(255, 0), G = c(0, 255), B = c(0, 0), A = c(0, 0),
          roi = c("0001", "0002"), color = c("#FF0000", "#00FF00"),
          stringsAsFactors = FALSE
        )
      },
      wholebrain_project_to_surface = function(...) {
        bind_rows(
          tibble(
            hemi = "left", region = "a", label = "lh_a", colour = "#FF0000",
            vertices = list(seq_len(100) - 1L),
            source_label = "a", source_idx = 1L
          ),
          tibble(
            hemi = "left", region = "b", label = "lh_b", colour = "#00FF00",
            vertices = list(seq_len(10) - 1L),
            source_label = "b", source_idx = 2L
          )
        )
      }
    )

    vol_file <- withr::local_tempfile(fileext = ".nii.gz")
    file.create(vol_file)

    expect_warning(
      result <- create_wholebrain_atlas(
        input_volume = vol_file,
        steps = 1:2,
        verbose = FALSE
      ),
      "No color lookup table"
    )

    expect_true("cortical_labels" %in% names(result))
    expect_true("subcortical_labels" %in% names(result))
    expect_true("a" %in% result$cortical_labels)
    expect_true("b" %in% result$subcortical_labels)
  })

  it("runs cortical pipeline for step 3", {
    test_dir <- withr::local_tempdir()
    cortical_called <- FALSE

    local_mocked_bindings(
      check_fs = function(...) TRUE,
      check_magick = function(...) TRUE,
      setup_atlas_dirs = function(...) {
        list(
          base = test_dir,
          snapshots = test_dir,
          processed = test_dir,
          masks = test_dir,
          snapshots = test_dir
        )
      },
      load_or_run_step = function(step, steps, ...) {
        if (step %in% steps) {
          list(run = TRUE, data = list())
        } else {
          list(run = FALSE, data = list(
            "atlas_data.rds" = tibble(
              hemi = "left", region = "a", label = "lh_a",
              colour = "#FF0000",
              vertices = list(seq_len(100) - 1L),
              source_label = "a", source_idx = 1L
            ),
            "colortable.rds" = data.frame(
              idx = 1, label = "a",
              color = "#FF0000", stringsAsFactors = FALSE
            ),
            "label_split.rds" = list(
              cortical_labels = "a",
              subcortical_labels = character(),
              vertex_counts = c(a = 100L)
            )
          ))
        }
      },
      validate_cortical_config = function(...) {
        list(
          output_dir = test_dir, verbose = FALSE, cleanup = FALSE,
          skip_existing = FALSE, tolerance = 1, smoothness = 5,
          snapshot_dim = 800, steps = 1L:8L
        )
      },
      cortical_resolve_step1 = function(...) {
        cortical_called <<- TRUE
        list(
          atlas_3d = structure(
            list(
              core = data.frame(
                hemi = "left", region = "a", label = "lh_a"
              ),
              type = "cortical"
            ),
            class = "ggseg_atlas"
          ),
          components = list(
            core = data.frame(
              hemi = "left", region = "a", label = "lh_a"
            ),
            palette = c(lh_a = "#FF0000"),
            vertices_df = data.frame(label = "lh_a")
          )
        )
      },
      cortical_pipeline = function(atlas_3d, ...) atlas_3d
    )

    vol_file <- withr::local_tempfile(fileext = ".nii.gz")
    file.create(vol_file)

    result <- create_wholebrain_atlas(
      input_volume = vol_file,
      steps = 3,
      verbose = FALSE,
      cleanup = FALSE
    )

    expect_true(cortical_called)
    expect_false(is.null(result$cortical))
    expect_null(result$subcortical)
  })

  it("runs subcortical pipeline for step 4", {
    test_dir <- withr::local_tempdir()
    subcort_called <- FALSE

    local_mocked_bindings(
      check_fs = function(...) TRUE,
      setup_atlas_dirs = function(...) {
        list(
          base = test_dir,
          snapshots = test_dir,
          processed = test_dir,
          masks = test_dir,
          snapshots = test_dir
        )
      },
      load_or_run_step = function(step, steps, ...) {
        if (step %in% steps) {
          list(run = TRUE, data = list())
        } else {
          list(run = FALSE, data = list(
            "atlas_data.rds" = tibble(
              hemi = "left", region = "b", label = "lh_b",
              colour = "#00FF00",
              vertices = list(seq_len(10) - 1L),
              source_label = "b", source_idx = 2L
            ),
            "colortable.rds" = data.frame(
              idx = 2, label = "b", R = 0, G = 255, B = 0, A = 0,
              roi = "0002", color = "#00FF00",
              stringsAsFactors = FALSE
            ),
            "label_split.rds" = list(
              cortical_labels = character(),
              subcortical_labels = "b",
              vertex_counts = c(b = 10L)
            )
          ))
        }
      },
      write_ctab = function(...) invisible(NULL),
      wholebrain_filter_volume = function(...) invisible("filtered.nii.gz"),
      create_subcortical_atlas = function(...) {
        subcort_called <<- TRUE
        structure(
          list(
            core = data.frame(
              hemi = NA, region = "b", label = "b"
            ),
            type = "subcortical"
          ),
          class = "ggseg_atlas"
        )
      }
    )

    vol_file <- withr::local_tempfile(fileext = ".nii.gz")
    file.create(vol_file)

    result <- create_wholebrain_atlas(
      input_volume = vol_file,
      steps = 4,
      verbose = FALSE,
      cleanup = FALSE
    )

    expect_true(subcort_called)
    expect_null(result$cortical)
    expect_false(is.null(result$subcortical))
  })

  it("skips cortical when no cortical labels", {
    test_dir <- withr::local_tempdir()

    local_mocked_bindings(
      check_fs = function(...) TRUE,
      setup_atlas_dirs = function(...) {
        list(
          base = test_dir,
          snapshots = test_dir,
          processed = test_dir,
          masks = test_dir,
          snapshots = test_dir
        )
      },
      load_or_run_step = function(step, steps, ...) {
        list(run = FALSE, data = list(
          "atlas_data.rds" = tibble(
            hemi = "left", region = "b", label = "lh_b",
            colour = "#00FF00",
            vertices = list(seq_len(10) - 1L),
            source_label = "b", source_idx = 2L
          ),
          "colortable.rds" = data.frame(
            idx = 2, label = "b", R = 0, G = 255, B = 0, A = 0,
            roi = "0002", color = "#00FF00",
            stringsAsFactors = FALSE
          ),
          "label_split.rds" = list(
            cortical_labels = character(),
            subcortical_labels = "b",
            vertex_counts = c(b = 10L)
          )
        ))
      },
      write_ctab = function(...) invisible(NULL),
      wholebrain_filter_volume = function(...) invisible("filtered.nii.gz"),
      create_subcortical_atlas = function(...) {
        structure(
          list(core = data.frame(hemi = NA, region = "b", label = "b")),
          class = "ggseg_atlas"
        )
      }
    )

    vol_file <- withr::local_tempfile(fileext = ".nii.gz")
    file.create(vol_file)

    result <- create_wholebrain_atlas(
      input_volume = vol_file,
      steps = 3:4,
      verbose = FALSE,
      cleanup = FALSE
    )

    expect_null(result$cortical)
    expect_false(is.null(result$subcortical))
  })
})


describe("wholebrain_filter_volume", {
  it("zeros out labels not in keep_labels", {
    skip_if_not_installed("RNifti")

    tmp_in <- withr::local_tempfile(fileext = ".nii.gz")
    tmp_out <- withr::local_tempfile(fileext = ".nii.gz")

    arr <- array(0L, dim = c(3, 3, 3))
    arr[1, 1, 1] <- 10L
    arr[2, 2, 2] <- 20L
    arr[3, 3, 3] <- 30L
    nii <- RNifti::asNifti(arr)
    RNifti::writeNifti(nii, tmp_in)

    wholebrain_filter_volume(tmp_in, keep_labels = c(10L, 30L), tmp_out)
    result <- as.array(RNifti::readNifti(tmp_out))

    expect_equal(result[1, 1, 1], 10)
    expect_equal(result[2, 2, 2], 0)
    expect_equal(result[3, 3, 3], 30)
  })
})


describe("create_wholebrain_atlas integration", {
  it("projects native-space volume and classifies labels", {
    skip_if_no_freesurfer()

    vol_file <- test_mgz_file()
    skip_if(!file.exists(vol_file), "Test volume file not found")
    lut_file <- test_lut_file()
    skip_if(!file.exists(lut_file), "Test LUT file not found")

    result <- create_wholebrain_atlas(
      input_volume = vol_file,
      input_lut = lut_file,
      regheader = TRUE,
      steps = 1:2,
      verbose = FALSE
    )

    expect_true("cortical_labels" %in% names(result))
    expect_true("subcortical_labels" %in% names(result))
    expect_true(
      length(result$cortical_labels) + length(result$subcortical_labels) > 0
    )
  })
})
