describe("create_cortical_atlas", {
  it("checks for FreeSurfer when geometry is requested", {
    local_mocked_bindings(
      check_fs = function(abort = FALSE) {
        if (abort) cli::cli_abort("FreeSurfer not found")
        FALSE
      }
    )

    expect_error(
      create_cortical_atlas(
        input_annot = c("lh.test.annot", "rh.test.annot"),
        steps = NULL,
        verbose = FALSE
      ),
      "FreeSurfer"
    )
  })

  it("validates annotation files exist", {
    expect_error(
      create_cortical_atlas(
        input_annot = c("nonexistent.annot"),
        steps = 1,
        verbose = FALSE
      ),
      "not found"
    )
  })

  it("creates 3D-only atlas from annotation", {
    skip_if_no_freesurfer()

    annot_dir <- file.path(
      freesurfer::fs_subj_dir(), "fsaverage5", "label"
    )
    annot_files <- c(
      file.path(annot_dir, "lh.aparc.annot"),
      file.path(annot_dir, "rh.aparc.annot")
    )

    atlas <- expect_warnings(
      create_cortical_atlas(
        input_annot = annot_files,
        steps = 1,
        verbose = FALSE
      ),
      "version"
    )

    expect_s3_class(atlas, "brain_atlas")
    expect_equal(atlas$type, "cortical")
    expect_true(nrow(atlas$core) > 0)
  })

  it("includes vertices for 3D rendering", {
    skip_if_no_freesurfer()

    annot_dir <- file.path(
      freesurfer::fs_subj_dir(), "fsaverage5", "label"
    )
    annot_files <- c(
      file.path(annot_dir, "lh.aparc.annot"),
      file.path(annot_dir, "rh.aparc.annot")
    )

    atlas <- expect_warnings(
      create_cortical_atlas(
        input_annot = annot_files,
        steps = 1,
        verbose = FALSE
      ),
      "version"
    )

    vertices <- ggseg.formats::atlas_vertices(atlas)
    expect_true(nrow(vertices) > 0)
    expect_true("vertices" %in% names(vertices))
  })

  it("works with different annotations", {
    skip_if_no_freesurfer()

    annot_dir <- file.path(
      freesurfer::fs_subj_dir(), "fsaverage5", "label"
    )
    aparc_files <- c(
      file.path(annot_dir, "lh.aparc.annot"),
      file.path(annot_dir, "rh.aparc.annot")
    )

    atlas_aparc <- expect_warnings(
      create_cortical_atlas(
        input_annot = aparc_files,
        steps = 1,
        verbose = FALSE
      ),
      "version"
    )

    expect_true(nrow(atlas_aparc$core) > 0)

    a2009s_files <- c(
      file.path(annot_dir, "lh.aparc.a2009s.annot"),
      file.path(annot_dir, "rh.aparc.a2009s.annot")
    )
    has_a2009s <- all(file.exists(a2009s_files))

    if (has_a2009s) {
      atlas_a2009s <- expect_warnings(
        create_cortical_atlas(
          input_annot = a2009s_files,
          steps = 1,
          verbose = FALSE
        ),
        "version"
      )
      expect_true(nrow(atlas_a2009s$core) > 0)
      expect_false(
        identical(atlas_aparc$core$region, atlas_a2009s$core$region)
      )
    }
  })

  it("can render with ggseg3d", {
    skip_if_no_freesurfer()

    annot_dir <- file.path(
      freesurfer::fs_subj_dir(), "fsaverage5", "label"
    )
    annot_files <- c(
      file.path(annot_dir, "lh.aparc.annot"),
      file.path(annot_dir, "rh.aparc.annot")
    )

    atlas <- expect_warnings(
      create_cortical_atlas(
        input_annot = annot_files,
        steps = 1,
        verbose = FALSE
      ),
      "version"
    )

    expect_no_error({
      p <- ggseg3d::ggseg3d(atlas = atlas, hemisphere = "left")
    })
  })
})


describe("cortical_brain_snapshots", {
  it("dispatches snapshot_brain for each hemi x view combination", {
    captured <- list()
    local_mocked_bindings(
      snapshot_brain = function(atlas, hemisphere, view, ...) {
        captured[[length(captured) + 1]] <<- list(
          hemisphere = hemisphere, view = view
        )
      },
      progressor = function(...) function(...) NULL
    )

    atlas_3d <- structure(list(), class = "brain_atlas")
    dirs <- list(base = tempdir())

    cortical_brain_snapshots(
      atlas_3d,
      hemisphere = c("lh", "rh"),
      views = c("lateral", "medial"),
      dirs = dirs,
      skip_existing = FALSE
    )

    expect_equal(length(captured), 4)
    hemis <- vapply(captured, `[[`, character(1), "hemisphere")
    expect_true(all(c("lh", "rh") %in% hemis))
  })
})


describe("cortical_region_snapshots", {
  it("filters grid to matching hemi-label pairs", {
    captured <- list()
    local_mocked_bindings(
      snapshot_region = function(atlas, region_label, hemisphere, view, ...) {
        captured[[length(captured) + 1]] <<- list(
          region_label = region_label, hemisphere = hemisphere
        )
      },
      progressor = function(...) function(...) NULL
    )

    components <- list(
      core = data.frame(
        label = c("lh_frontal", "rh_frontal"),
        stringsAsFactors = FALSE
      )
    )
    atlas_3d <- structure(list(), class = "brain_atlas")
    dirs <- list(snapshots = tempdir())

    cortical_region_snapshots(
      atlas_3d, components,
      hemisphere = c("lh", "rh"),
      views = c("lateral"),
      dirs = dirs,
      skip_existing = FALSE
    )

    labels <- vapply(captured, `[[`, character(1), "region_label")
    hemis <- vapply(captured, `[[`, character(1), "hemisphere")
    expect_true(all(hemis[labels == "lh_frontal"] == "lh"))
    expect_true(all(hemis[labels == "rh_frontal"] == "rh"))
  })
})


describe("cortical_isolate_regions", {
  it("calls isolate_region for each file in snapshots dir", {
    snap_dir <- withr::local_tempdir("snap_")
    file.create(file.path(snap_dir, "region1.png"))
    file.create(file.path(snap_dir, "region2.png"))

    captured <- list()
    local_mocked_bindings(
      isolate_region = function(input_file, output_file, ...) {
        captured[[length(captured) + 1]] <<- basename(input_file)
      },
      progressor = function(...) function(...) NULL
    )

    dirs <- list(
      snapshots = snap_dir,
      masks = withr::local_tempdir("masks_"),
      processed = withr::local_tempdir("proc_")
    )

    cortical_isolate_regions(dirs, skip_existing = FALSE)

    expect_equal(sort(unlist(captured)), c("region1.png", "region2.png"))
  })
})


describe("cortical_build_sf", {
  it("produces sf with label and view columns", {
    local_mocked_bindings(
      load_reduced_contours = function(base_dir) {
        sf::st_sf(
          hemi = c("left", "right"),
          view = c("lateral", "lateral"),
          label = c("lh_frontal", "rh_frontal"),
          geometry = sf::st_sfc(
            sf::st_polygon(list(matrix(
              c(0, 0, 1, 0, 1, 1, 0, 0), ncol = 2, byrow = TRUE
            ))),
            sf::st_polygon(list(matrix(
              c(2, 0, 3, 0, 3, 1, 2, 0), ncol = 2, byrow = TRUE
            )))
          )
        )
      },
      layout_cortical_views = function(df) df
    )

    dirs <- list(base = tempdir())
    result <- cortical_build_sf(dirs)

    expect_s3_class(result, "sf")
    expect_true(all(c("label", "view") %in% names(result)))
  })
})


describe("labels_read_files", {
  it("reads label files and builds atlas data tibble", {
    labels <- unlist(test_label_files())
    default_colours <- rep(NA_character_, length(labels))

    result <- labels_read_files(labels, NULL, NULL, default_colours)

    expect_s3_class(result, "tbl_df")
    expect_equal(nrow(result), 3)
    expect_true(all(
      c("hemi", "region", "label", "colour", "vertices") %in%
        names(result)
    ))
    expect_true("left" %in% result$hemi)
    expect_true("right" %in% result$hemi)
  })

  it("uses custom region_names when provided", {
    labels <- unlist(test_label_files())
    default_colours <- rep(NA_character_, length(labels))
    custom_names <- c("Motor", "Visual", "Motor")

    result <- labels_read_files(labels, custom_names, NULL, default_colours)

    expect_equal(result$region, custom_names)
  })
})


describe("labels_region_snapshots", {
  it("also takes NA region snapshots", {
    region_captured <- list()
    na_captured <- list()
    local_mocked_bindings(
      snapshot_region = function(...) {
        region_captured[[length(region_captured) + 1]] <<- TRUE
      },
      snapshot_na_regions = function(...) {
        na_captured[[length(na_captured) + 1]] <<- TRUE
      },
      progressor = function(...) function(...) NULL
    )

    components <- list(
      core = data.frame(
        label = c("lh_motor", "rh_motor"),
        region = c("motor", "motor"),
        stringsAsFactors = FALSE
      )
    )
    atlas_3d <- structure(list(), class = "brain_atlas")
    dirs <- list(snapshots = tempdir())

    labels_region_snapshots(
      atlas_3d, components,
      hemi_short = c("lh", "rh"),
      views = c("lateral"),
      dirs = dirs,
      skip_existing = FALSE
    )

    expect_true(length(region_captured) > 0)
    expect_true(length(na_captured) > 0)
  })
})


describe("cortical_pipeline", {
  it("dispatches region_snapshot_fn for step 3", {
    snapshot_fn_called <- FALSE
    local_mocked_bindings(
      cortical_brain_snapshots = function(...) NULL,
      cortical_isolate_regions = function(...) NULL,
      extract_contours = function(...) NULL,
      smooth_contours = function(...) NULL,
      reduce_vertex = function(...) NULL,
      cortical_build_sf = function(...) {
        sf::st_sf(
          label = "test", view = "lateral",
          geometry = sf::st_sfc(sf::st_polygon(list(matrix(
            c(0, 0, 1, 0, 1, 1, 0, 0), ncol = 2, byrow = TRUE
          ))))
        )
      },
      brain_atlas = function(...) structure(list(...), class = "brain_atlas"),
      brain_data_cortical = function(...) list(...),
      warn_if_large_atlas = function(...) NULL,
      preview_atlas = function(...) NULL
    )

    custom_fn <- function(...) {
      snapshot_fn_called <<- TRUE
    }

    components <- list(
      core = data.frame(
        hemi = "left", region = "frontal", label = "lh_frontal",
        stringsAsFactors = FALSE
      ),
      palette = c(lh_frontal = "#FF0000"),
      vertices_df = data.frame(
        label = "lh_frontal", vertices = I(list(1:10))
      )
    )

    cortical_pipeline(
      atlas_3d = structure(list(), class = "brain_atlas"),
      components = components,
      atlas_name = "test",
      hemisphere = c("lh", "rh"),
      views = c("lateral"),
      region_snapshot_fn = custom_fn,
      dirs = list(
        base = withr::local_tempdir(),
        snapshots = withr::local_tempdir(),
        processed = withr::local_tempdir(),
        masks = withr::local_tempdir()
      ),
      steps = 2:8,
      skip_existing = FALSE,
      tolerance = 1,
      smoothness = 5,
      cleanup = FALSE,
      verbose = FALSE,
      start_time = Sys.time()
    )

    expect_true(snapshot_fn_called)
  })

  it("skips steps not in steps vector", {
    step2_called <- FALSE
    step4_called <- FALSE
    local_mocked_bindings(
      cortical_brain_snapshots = function(...) {
        step2_called <<- TRUE
      },
      cortical_isolate_regions = function(...) {
        step4_called <<- TRUE
      },
      extract_contours = function(...) NULL,
      smooth_contours = function(...) NULL,
      reduce_vertex = function(...) NULL,
      cortical_build_sf = function(...) {
        sf::st_sf(
          label = "test", view = "lateral",
          geometry = sf::st_sfc(sf::st_polygon(list(matrix(
            c(0, 0, 1, 0, 1, 1, 0, 0), ncol = 2, byrow = TRUE
          ))))
        )
      },
      brain_atlas = function(...) structure(list(...), class = "brain_atlas"),
      brain_data_cortical = function(...) list(...),
      warn_if_large_atlas = function(...) NULL,
      preview_atlas = function(...) NULL
    )

    components <- list(
      core = data.frame(
        hemi = "left", region = "r", label = "lh_r",
        stringsAsFactors = FALSE
      ),
      palette = c(lh_r = "#FF0000"),
      vertices_df = data.frame(label = "lh_r", vertices = I(list(1:5)))
    )

    cortical_pipeline(
      atlas_3d = structure(list(), class = "brain_atlas"),
      components = components,
      atlas_name = "test",
      hemisphere = "lh",
      views = "lateral",
      region_snapshot_fn = function(...) NULL,
      dirs = list(
        base = withr::local_tempdir(),
        snapshots = withr::local_tempdir(),
        processed = withr::local_tempdir(),
        masks = withr::local_tempdir()
      ),
      steps = 5:8,
      skip_existing = FALSE,
      tolerance = 1,
      smoothness = 5,
      cleanup = FALSE,
      verbose = FALSE,
      start_time = Sys.time()
    )

    expect_false(step2_called)
    expect_false(step4_called)
  })
})


describe("create_cortical_atlas pipeline flow", {
  it("step 1 passes input_annot to read_annotation_data", {
    captured <- list()
    local_mocked_bindings(
      read_annotation_data = function(annot_files) {
        captured$annot <<- annot_files
        dplyr::tibble(
          hemi = "left", region = "frontal", label = "lh_frontal",
          colour = "#FF0000", vertices = list(1:10)
        )
      },
      build_atlas_components = function(data) {
        list(
          core = data.frame(
            hemi = "left", region = "frontal", label = "lh_frontal",
            stringsAsFactors = FALSE
          ),
          palette = c(lh_frontal = "#FF0000"),
          vertices_df = data.frame(
            label = "lh_frontal", vertices = I(list(1:10))
          )
        )
      },
      brain_atlas = function(...) {
        structure(list(...), class = "brain_atlas")
      },
      brain_data_cortical = function(...) list(...)
    )

    withr::local_options(ggsegExtra.output_dir = withr::local_tempdir())

    result <- create_cortical_atlas(
      input_annot = c("lh.test.annot"),
      steps = 1,
      verbose = FALSE
    )

    expect_equal(captured$annot, c("lh.test.annot"))
    expect_s3_class(result, "brain_atlas")
  })

  it("returns early for steps = 1 without calling step 2", {
    step2_called <- FALSE
    local_mocked_bindings(
      read_annotation_data = function(annot_files) {
        dplyr::tibble(
          hemi = "left", region = "frontal", label = "lh_frontal",
          colour = "#FF0000", vertices = list(1:10)
        )
      },
      build_atlas_components = function(data) {
        list(
          core = data.frame(
            hemi = "left", region = "frontal", label = "lh_frontal",
            stringsAsFactors = FALSE
          ),
          palette = c(lh_frontal = "#FF0000"),
          vertices_df = data.frame(
            label = "lh_frontal", vertices = I(list(1:10))
          )
        )
      },
      brain_atlas = function(...) {
        structure(list(...), class = "brain_atlas")
      },
      brain_data_cortical = function(...) list(...),
      cortical_brain_snapshots = function(...) {
        step2_called <<- TRUE
      }
    )

    withr::local_options(ggsegExtra.output_dir = withr::local_tempdir())

    create_cortical_atlas(
      input_annot = c("lh.test.annot"),
      steps = 1,
      verbose = FALSE
    )

    expect_false(step2_called)
  })
})


describe("read_annotation_data", {
  it("reads annotation data from files", {
    skip_if_no_freesurfer()

    annot_dir <- file.path(
      freesurfer::fs_subj_dir(), "fsaverage5", "label"
    )
    annot_files <- c(
      file.path(annot_dir, "lh.aparc.annot"),
      file.path(annot_dir, "rh.aparc.annot")
    )

    atlas_data <- expect_warnings(
      read_annotation_data(annot_files),
      "version"
    )

    expect_s3_class(atlas_data, "tbl_df")
    expect_true(all(
      c("hemi", "region", "label", "colour", "vertices") %in%
        names(atlas_data)
    ))
    expect_true(nrow(atlas_data) > 0)
  })

  it("reads annotation from custom files", {
    annot_files <- test_annot_files()
    skip_if(
      !file.exists(annot_files$lh),
      "Test annotation files not found"
    )

    atlas_data <- expect_warnings(
      read_annotation_data(
        c(annot_files$lh, annot_files$rh)
      ),
      "version"
    )

    expect_s3_class(atlas_data, "tbl_df")
    expect_true(nrow(atlas_data) > 0)
  })

  it("returns data for both hemispheres", {
    skip_if_no_freesurfer()

    annot_dir <- file.path(
      freesurfer::fs_subj_dir(), "fsaverage5", "label"
    )
    annot_files <- c(
      file.path(annot_dir, "lh.aparc.annot"),
      file.path(annot_dir, "rh.aparc.annot")
    )

    atlas_data <- expect_warnings(
      read_annotation_data(annot_files),
      "version"
    )

    expect_true("left" %in% atlas_data$hemi)
    expect_true("right" %in% atlas_data$hemi)
  })

  it("creates proper labels with hemisphere prefix", {
    skip_if_no_freesurfer()

    annot_dir <- file.path(
      freesurfer::fs_subj_dir(), "fsaverage5", "label"
    )
    annot_files <- c(
      file.path(annot_dir, "lh.aparc.annot"),
      file.path(annot_dir, "rh.aparc.annot")
    )

    atlas_data <- expect_warnings(
      read_annotation_data(annot_files),
      "version"
    )

    lh_labels <- atlas_data$label[atlas_data$hemi == "left"]
    rh_labels <- atlas_data$label[atlas_data$hemi == "right"]

    expect_true(all(grepl("^lh_", lh_labels)))
    expect_true(all(grepl("^rh_", rh_labels)))
  })

  it("includes vertex indices as list column", {
    skip_if_no_freesurfer()

    annot_dir <- file.path(
      freesurfer::fs_subj_dir(), "fsaverage5", "label"
    )
    annot_files <- c(
      file.path(annot_dir, "lh.aparc.annot"),
      file.path(annot_dir, "rh.aparc.annot")
    )

    atlas_data <- expect_warnings(
      read_annotation_data(annot_files),
      "version"
    )

    expect_type(atlas_data$vertices, "list")
    expect_true(all(
      vapply(atlas_data$vertices, is.integer, logical(1))
    ))
    expect_true(all(
      vapply(
        atlas_data$vertices,
        function(x) length(x) > 0,
        logical(1)
      )
    ))
  })

  it("sets NA colour for medial wall/unknown regions", {
    skip_if_no_freesurfer()

    annot_dir <- file.path(
      freesurfer::fs_subj_dir(), "fsaverage5", "label"
    )
    annot_files <- c(
      file.path(annot_dir, "lh.aparc.annot"),
      file.path(annot_dir, "rh.aparc.annot")
    )

    atlas_data <- expect_warnings(
      read_annotation_data(annot_files),
      "version"
    )

    wall_rows <- grepl(
      "wall|unknown", atlas_data$region, ignore.case = TRUE
    )
    if (any(wall_rows)) {
      expect_true(all(!is.na(atlas_data$colour[wall_rows])))
    }
  })

  it("errors when files not found", {
    expect_error(
      read_annotation_data(c("nonexistent.annot")),
      "not found"
    )
  })
})


describe("create_atlas_from_labels", {
  it("creates atlas from label files", {
    skip_if_no_freesurfer()

    labels <- unlist(test_label_files())
    atlas <- create_atlas_from_labels(
      labels,
      atlas_name = "test_atlas",
      steps = 1,
      verbose = FALSE
    )

    expect_s3_class(atlas, "brain_atlas")
    expect_equal(atlas$atlas, "test_atlas")
    expect_equal(atlas$type, "cortical")
    expect_equal(nrow(atlas$core), 3)
  })

  it("correctly parses hemisphere from filename", {
    skip_if_no_freesurfer()

    labels <- unlist(test_label_files())
    atlas <- create_atlas_from_labels(
      labels,
      steps = 1,
      verbose = FALSE
    )

    expect_true("left" %in% atlas$core$hemi)
    expect_true("right" %in% atlas$core$hemi)
    expect_equal(sum(atlas$core$hemi == "left"), 2)
    expect_equal(sum(atlas$core$hemi == "right"), 1)
  })

  it("stores vertices correctly", {
    skip_if_no_freesurfer()

    labels <- unlist(test_label_files())
    atlas <- create_atlas_from_labels(
      labels,
      steps = 1,
      verbose = FALSE
    )

    vertices <- ggseg.formats::atlas_vertices(atlas)
    expect_true(nrow(vertices) > 0)
    expect_equal(length(vertices$vertices[[1]]), 5)
  })

  it("accepts custom names and colours via input_lut", {
    skip_if_no_freesurfer()

    labels <- unlist(test_label_files())
    custom_lut <- data.frame(
      region = c("Motor", "Visual", "Motor"),
      hex = c("#FF0000", "#00FF00", "#0000FF")
    )

    atlas <- create_atlas_from_labels(
      labels,
      input_lut = custom_lut,
      steps = 1,
      verbose = FALSE
    )

    expect_equal(atlas$core$region, custom_lut$region)
    expect_true(all(custom_lut$hex %in% atlas$palette))
  })

  it("errors when label files not found", {
    skip_if_no_freesurfer()

    expect_error(
      create_atlas_from_labels(
        c("nonexistent.label"),
        verbose = FALSE
      ),
      "Label files not found"
    )
  })

  it("requires FreeSurfer to be available", {
    local_mocked_bindings(
      check_fs = function(abort = FALSE) {
        if (abort) cli::cli_abort("FreeSurfer not found")
        FALSE
      }
    )

    expect_error(
      create_atlas_from_labels(
        c("lh.test.label"),
        verbose = FALSE
      ),
      "FreeSurfer"
    )
  })
})


describe("read_label_vertices", {
  it("reads vertex indices from label file", {
    label_file <- test_label_files()$lh_region1
    vertices <- read_label_vertices(label_file)

    expect_type(vertices, "integer")
    expect_equal(length(vertices), 5)
    expect_equal(vertices, c(100L, 101L, 102L, 150L, 151L))
  })

  it("handles different label files", {
    label_file <- test_label_files()$lh_region2
    vertices <- read_label_vertices(label_file)

    expect_equal(length(vertices), 3)
    expect_equal(vertices, c(200L, 201L, 202L))
  })

  it("handles right hemisphere labels", {
    label_file <- test_label_files()$rh_region1
    vertices <- read_label_vertices(label_file)

    expect_equal(length(vertices), 4)
  })
})


describe("cortical_step1", {
  it("loads cached data when files exist and skip_existing is TRUE", {
    tmp_dir <- withr::local_tempdir()
    mock_atlas <- structure(list(atlas = "test"), class = "brain_atlas")
    mock_components <- list(
      core = data.frame(hemi = "left", region = "r", label = "lh_r"),
      palette = c(lh_r = "#FF0000"),
      vertices_df = data.frame(label = "lh_r", vertices = I(list(1:5)))
    )
    saveRDS(mock_atlas, file.path(tmp_dir, "atlas_3d.rds"))
    saveRDS(mock_components, file.path(tmp_dir, "components.rds"))

    result <- cortical_step1(
      dirs = list(base = tmp_dir),
      atlas_name = "test",
      steps = 1L,
      skip_existing = TRUE,
      verbose = FALSE,
      read_fn = function() stop("should not be called"),
      step_label = "test",
      cache_label = "test"
    )

    expect_s3_class(result$atlas_3d, "brain_atlas")
    expect_equal(result$components$palette, mock_components$palette)
  })
})


describe("cortical_early_return", {
  it("returns atlas_3d and logs when verbose", {
    local_mocked_bindings(
      log_elapsed = function(...) NULL
    )
    mock_atlas <- structure(list(atlas = "test"), class = "brain_atlas")
    components <- list(
      core = data.frame(hemi = "left", region = "r", label = "lh_r")
    )
    dirs <- list(base = withr::local_tempdir())

    msgs <- capture.output(
      result <- cortical_early_return(
        mock_atlas, components, dirs,
        cleanup = FALSE, verbose = TRUE, start_time = Sys.time()
      ),
      type = "message"
    )

    expect_s3_class(result, "brain_atlas")
    expect_true(any(grepl("1 regions", msgs)))
  })

  it("cleans up when cleanup is TRUE", {
    local_mocked_bindings(log_elapsed = function(...) NULL)
    mock_atlas <- structure(list(atlas = "test"), class = "brain_atlas")
    components <- list(
      core = data.frame(hemi = "left", region = "r", label = "lh_r")
    )
    base_dir <- withr::local_tempdir()
    dir.create(file.path(base_dir, "subdir"))

    cortical_early_return(
      mock_atlas, components,
      dirs = list(base = file.path(base_dir, "subdir")),
      cleanup = TRUE, verbose = FALSE, start_time = Sys.time()
    )

    expect_false(dir.exists(file.path(base_dir, "subdir")))
  })
})


describe("cortical_pipeline verbose and cleanup paths", {
  it("logs verbose messages for each step", {
    local_mocked_bindings(
      cortical_brain_snapshots = function(...) NULL,
      cortical_isolate_regions = function(...) NULL,
      extract_contours = function(...) NULL,
      smooth_contours = function(...) NULL,
      reduce_vertex = function(...) NULL,
      cortical_build_sf = function(...) {
        sf::st_sf(
          label = "test", view = "lateral",
          geometry = sf::st_sfc(sf::st_polygon(list(matrix(
            c(0, 0, 1, 0, 1, 1, 0, 0), ncol = 2, byrow = TRUE
          ))))
        )
      },
      brain_atlas = function(...) structure(list(...), class = "brain_atlas"),
      brain_data_cortical = function(...) list(...),
      warn_if_large_atlas = function(...) NULL,
      preview_atlas = function(...) NULL,
      log_elapsed = function(...) NULL
    )

    components <- list(
      core = data.frame(
        hemi = "left", region = "r", label = "lh_r",
        stringsAsFactors = FALSE
      ),
      palette = c(lh_r = "#FF0000"),
      vertices_df = data.frame(label = "lh_r", vertices = I(list(1:5)))
    )

    msgs <- capture.output(
      cortical_pipeline(
        atlas_3d = structure(list(), class = "brain_atlas"),
        components = components,
        atlas_name = "test",
        hemisphere = "lh",
        views = "lateral",
        region_snapshot_fn = function(...) NULL,
        dirs = list(
          base = withr::local_tempdir(),
          snapshots = withr::local_tempdir(),
          processed = withr::local_tempdir(),
          masks = withr::local_tempdir()
        ),
        steps = 2:8,
        skip_existing = FALSE,
        tolerance = 1,
        smoothness = 5,
        cleanup = FALSE,
        verbose = TRUE,
        start_time = Sys.time()
      ),
      type = "message"
    )

    expect_true(any(grepl("2/8", msgs)))
    expect_true(any(grepl("3/8", msgs)))
    expect_true(any(grepl("4/8", msgs)))
    expect_true(any(grepl("8/8", msgs)))
  })

  it("cleans up base directory when cleanup is TRUE in step 8", {
    local_mocked_bindings(
      cortical_brain_snapshots = function(...) NULL,
      cortical_isolate_regions = function(...) NULL,
      extract_contours = function(...) NULL,
      smooth_contours = function(...) NULL,
      reduce_vertex = function(...) NULL,
      cortical_build_sf = function(...) {
        sf::st_sf(
          label = "test", view = "lateral",
          geometry = sf::st_sfc(sf::st_polygon(list(matrix(
            c(0, 0, 1, 0, 1, 1, 0, 0), ncol = 2, byrow = TRUE
          ))))
        )
      },
      brain_atlas = function(...) structure(list(...), class = "brain_atlas"),
      brain_data_cortical = function(...) list(...),
      warn_if_large_atlas = function(...) NULL,
      preview_atlas = function(...) NULL,
      log_elapsed = function(...) NULL
    )

    base_dir <- withr::local_tempdir()
    actual_base <- file.path(base_dir, "atlas_work")
    dir.create(actual_base)

    components <- list(
      core = data.frame(
        hemi = "left", region = "r", label = "lh_r",
        stringsAsFactors = FALSE
      ),
      palette = c(lh_r = "#FF0000"),
      vertices_df = data.frame(label = "lh_r", vertices = I(list(1:5)))
    )

    cortical_pipeline(
      atlas_3d = structure(list(), class = "brain_atlas"),
      components = components,
      atlas_name = "test",
      hemisphere = "lh",
      views = "lateral",
      region_snapshot_fn = function(...) NULL,
      dirs = list(
        base = actual_base, snapshots = tempdir(),
        processed = tempdir(), masks = tempdir()
      ),
      steps = 8L,
      skip_existing = FALSE,
      tolerance = 1,
      smoothness = 5,
      cleanup = TRUE,
      verbose = FALSE,
      start_time = Sys.time()
    )

    expect_false(dir.exists(actual_base))
  })

  it("returns atlas_3d when step 8 not in steps", {
    local_mocked_bindings(
      extract_contours = function(...) NULL,
      smooth_contours = function(...) NULL,
      reduce_vertex = function(...) NULL,
      preview_atlas = function(...) NULL,
      log_elapsed = function(...) NULL
    )

    components <- list(
      core = data.frame(
        hemi = "left", region = "r", label = "lh_r",
        stringsAsFactors = FALSE
      ),
      palette = c(lh_r = "#FF0000"),
      vertices_df = data.frame(label = "lh_r", vertices = I(list(1:5)))
    )

    mock_atlas <- structure(list(atlas = "test_3d"), class = "brain_atlas")

    result <- cortical_pipeline(
      atlas_3d = mock_atlas,
      components = components,
      atlas_name = "test",
      hemisphere = "lh",
      views = "lateral",
      region_snapshot_fn = function(...) NULL,
      dirs = list(
        base = withr::local_tempdir(), snapshots = tempdir(),
        processed = tempdir(), masks = tempdir()
      ),
      steps = 5:7,
      skip_existing = FALSE,
      tolerance = 1,
      smoothness = 5,
      cleanup = FALSE,
      verbose = FALSE,
      start_time = Sys.time()
    )

    expect_s3_class(result, "brain_atlas")
  })
})


describe("create_cortical_atlas magick check", {
  it("checks for ImageMagick when steps > 1", {
    local_mocked_bindings(
      check_fs = function(abort = FALSE) invisible(TRUE),
      check_magick = function() cli::cli_abort("ImageMagick not found")
    )

    expect_error(
      create_cortical_atlas(
        input_annot = c("lh.test.annot"),
        steps = 2:8,
        verbose = FALSE
      ),
      "ImageMagick"
    )
  })
})


describe("create_cortical_atlas verbose output", {
  it("prints atlas name and paths when verbose is TRUE", {
    local_mocked_bindings(
      read_annotation_data = function(annot_files) {
        dplyr::tibble(
          hemi = "left", region = "frontal", label = "lh_frontal",
          colour = "#FF0000", vertices = list(1:10)
        )
      },
      build_atlas_components = function(data) {
        list(
          core = data.frame(
            hemi = "left", region = "frontal", label = "lh_frontal",
            stringsAsFactors = FALSE
          ),
          palette = c(lh_frontal = "#FF0000"),
          vertices_df = data.frame(
            label = "lh_frontal", vertices = I(list(1:10))
          )
        )
      },
      brain_atlas = function(...) structure(list(...), class = "brain_atlas"),
      brain_data_cortical = function(...) list(...),
      log_elapsed = function(...) NULL
    )

    withr::local_options(ggsegExtra.output_dir = withr::local_tempdir())

    msgs <- capture.output(
      create_cortical_atlas(
        input_annot = c("lh.test.annot"),
        steps = 1,
        verbose = TRUE
      ),
      type = "message"
    )

    expect_true(any(grepl("Creating brain atlas", msgs)))
    expect_true(any(grepl("lh.test.annot", msgs)))
    expect_true(any(grepl("output directory", msgs)))
  })
})


describe("create_cortical_atlas full pipeline path", {
  it("calls cortical_pipeline when steps > 1", {
    pipeline_called <- FALSE
    local_mocked_bindings(
      check_fs = function(abort = FALSE) invisible(TRUE),
      check_magick = function() invisible(TRUE),
      read_annotation_data = function(annot_files) {
        dplyr::tibble(
          hemi = "left", region = "frontal", label = "lh_frontal",
          colour = "#FF0000", vertices = list(1:10)
        )
      },
      build_atlas_components = function(data) {
        list(
          core = data.frame(
            hemi = "left", region = "frontal", label = "lh_frontal",
            stringsAsFactors = FALSE
          ),
          palette = c(lh_frontal = "#FF0000"),
          vertices_df = data.frame(
            label = "lh_frontal", vertices = I(list(1:10))
          )
        )
      },
      brain_atlas = function(...) structure(list(...), class = "brain_atlas"),
      brain_data_cortical = function(...) list(...),
      cortical_pipeline = function(...) {
        pipeline_called <<- TRUE
        structure(list(), class = "brain_atlas")
      }
    )

    withr::local_options(ggsegExtra.output_dir = withr::local_tempdir())

    create_cortical_atlas(
      input_annot = c("lh.test.annot"),
      steps = 1:8,
      verbose = FALSE
    )

    expect_true(pipeline_called)
  })
})


describe("cortical_step1 verbose paths", {
  it("prints progress step when verbose is TRUE and step runs", {
    local_mocked_bindings(
      brain_atlas = function(...) structure(list(...), class = "brain_atlas"),
      brain_data_cortical = function(...) list(...)
    )

    tmp_dir <- withr::local_tempdir()
    read_fn <- function() {
      dplyr::tibble(
        hemi = "left", region = "frontal", label = "lh_frontal",
        colour = "#FF0000", vertices = list(1:10)
      )
    }

    msgs <- capture.output(
      cortical_step1(
        dirs = list(base = tmp_dir),
        atlas_name = "test",
        steps = 1L,
        skip_existing = FALSE,
        verbose = TRUE,
        read_fn = read_fn,
        step_label = "1/8 Reading annotation files",
        cache_label = "Step 1 (Read annotations)"
      ),
      type = "message"
    )

    expect_true(any(grepl("1/8 Reading", msgs)))
  })

  it("aborts when read_fn returns zero rows", {
    tmp_dir <- withr::local_tempdir()
    read_fn <- function() {
      dplyr::tibble(
        hemi = character(), region = character(), label = character(),
        colour = character(), vertices = list()
      )
    }

    expect_error(
      cortical_step1(
        dirs = list(base = tmp_dir),
        atlas_name = "test",
        steps = 1L,
        skip_existing = FALSE,
        verbose = FALSE,
        read_fn = read_fn,
        step_label = "1/8 test",
        cache_label = "test"
      ),
      "No regions found"
    )
  })

  it("prints loaded existing data when verbose and skip_existing", {
    tmp_dir <- withr::local_tempdir()
    mock_atlas <- structure(list(atlas = "test"), class = "brain_atlas")
    mock_components <- list(
      core = data.frame(hemi = "left", region = "r", label = "lh_r"),
      palette = c(lh_r = "#FF0000"),
      vertices_df = data.frame(label = "lh_r", vertices = I(list(1:5)))
    )
    saveRDS(mock_atlas, file.path(tmp_dir, "atlas_3d.rds"))
    saveRDS(mock_components, file.path(tmp_dir, "components.rds"))

    msgs <- capture.output(
      cortical_step1(
        dirs = list(base = tmp_dir),
        atlas_name = "test",
        steps = 1L,
        skip_existing = TRUE,
        verbose = TRUE,
        read_fn = function() stop("should not be called"),
        step_label = "test",
        cache_label = "test"
      ),
      type = "message"
    )

    expect_true(any(grepl("Loaded existing atlas data", msgs)))
  })
})


describe("cortical_pipeline cleanup verbose in step 8", {
  it("prints cleanup message when cleanup and verbose", {
    local_mocked_bindings(
      cortical_brain_snapshots = function(...) NULL,
      cortical_isolate_regions = function(...) NULL,
      extract_contours = function(...) NULL,
      smooth_contours = function(...) NULL,
      reduce_vertex = function(...) NULL,
      cortical_build_sf = function(...) {
        sf::st_sf(
          label = "test", view = "lateral",
          geometry = sf::st_sfc(sf::st_polygon(list(matrix(
            c(0, 0, 1, 0, 1, 1, 0, 0), ncol = 2, byrow = TRUE
          ))))
        )
      },
      brain_atlas = function(...) structure(list(...), class = "brain_atlas"),
      brain_data_cortical = function(...) list(...),
      warn_if_large_atlas = function(...) NULL,
      preview_atlas = function(...) NULL,
      log_elapsed = function(...) NULL
    )

    base_dir <- withr::local_tempdir()
    actual_base <- file.path(base_dir, "atlas_work")
    dir.create(actual_base)

    components <- list(
      core = data.frame(
        hemi = "left", region = "r", label = "lh_r",
        stringsAsFactors = FALSE
      ),
      palette = c(lh_r = "#FF0000"),
      vertices_df = data.frame(label = "lh_r", vertices = I(list(1:5)))
    )

    msgs <- capture.output(
      cortical_pipeline(
        atlas_3d = structure(list(), class = "brain_atlas"),
        components = components,
        atlas_name = "test",
        hemisphere = "lh",
        views = "lateral",
        region_snapshot_fn = function(...) NULL,
        dirs = list(
          base = actual_base, snapshots = tempdir(),
          processed = tempdir(), masks = tempdir()
        ),
        steps = 8L,
        skip_existing = FALSE,
        tolerance = 1,
        smoothness = 5,
        cleanup = TRUE,
        verbose = TRUE,
        start_time = Sys.time()
      ),
      type = "message"
    )

    expect_true(any(grepl("Temporary files removed", msgs)))
  })
})


describe("cortical_pipeline verbose for non-step-8 completion", {
  it("prints completed steps message when step 8 not included", {
    local_mocked_bindings(
      extract_contours = function(...) NULL,
      smooth_contours = function(...) NULL,
      reduce_vertex = function(...) NULL,
      preview_atlas = function(...) NULL,
      log_elapsed = function(...) NULL
    )

    components <- list(
      core = data.frame(
        hemi = "left", region = "r", label = "lh_r",
        stringsAsFactors = FALSE
      ),
      palette = c(lh_r = "#FF0000"),
      vertices_df = data.frame(label = "lh_r", vertices = I(list(1:5)))
    )

    mock_atlas <- structure(list(atlas = "test_3d"), class = "brain_atlas")

    msgs <- capture.output(
      cortical_pipeline(
        atlas_3d = mock_atlas,
        components = components,
        atlas_name = "test",
        hemisphere = "lh",
        views = "lateral",
        region_snapshot_fn = function(...) NULL,
        dirs = list(
          base = withr::local_tempdir(), snapshots = tempdir(),
          processed = tempdir(), masks = tempdir()
        ),
        steps = 5:7,
        skip_existing = FALSE,
        tolerance = 1,
        smoothness = 5,
        cleanup = FALSE,
        verbose = TRUE,
        start_time = Sys.time()
      ),
      type = "message"
    )

    expect_true(any(grepl("Completed steps", msgs)))
  })
})


describe("create_atlas_from_labels verbose and LUT paths", {
  it("prints verbose output when verbose is TRUE", {
    local_mocked_bindings(
      brain_atlas = function(...) structure(list(...), class = "brain_atlas"),
      brain_data_cortical = function(...) list(...),
      log_elapsed = function(...) NULL
    )

    labels <- unlist(test_label_files())

    msgs <- capture.output(
      create_atlas_from_labels(
        labels,
        atlas_name = "test_atlas",
        steps = 1,
        verbose = TRUE
      ),
      type = "message"
    )

    expect_true(any(grepl("Creating brain atlas", msgs)))
    expect_true(any(grepl("Input files", msgs)))
    expect_true(any(grepl("output directory", msgs)))
  })

  it("extracts colours from RGB columns in LUT", {
    labels <- unlist(test_label_files())
    rgb_lut <- data.frame(
      region = c("Motor", "Visual", "Motor"),
      R = c(255, 0, 0),
      G = c(0, 255, 0),
      B = c(0, 0, 255)
    )

    local_mocked_bindings(
      brain_atlas = function(...) {
        args <- list(...)
        structure(
          list(
            atlas = args$atlas, type = args$type,
            palette = args$palette, core = args$core,
            data = args$data
          ),
          class = "brain_atlas"
        )
      },
      brain_data_cortical = function(...) list(...),
      log_elapsed = function(...) NULL
    )

    atlas <- create_atlas_from_labels(
      labels,
      input_lut = rgb_lut,
      atlas_name = "test_atlas",
      steps = 1,
      verbose = FALSE
    )

    expect_true(!is.null(atlas$palette))
    expect_true(any(grepl("^#", atlas$palette)))
  })

  it("sets NULL colour when LUT lacks hex and RGB columns", {
    labels <- unlist(test_label_files())
    bad_lut <- data.frame(
      region = c("Motor", "Visual", "Motor"),
      score = c(1, 2, 3)
    )

    local_mocked_bindings(
      brain_atlas = function(...) {
        args <- list(...)
        structure(
          list(
            atlas = args$atlas, type = args$type,
            palette = args$palette, core = args$core,
            data = args$data
          ),
          class = "brain_atlas"
        )
      },
      brain_data_cortical = function(...) list(...),
      log_elapsed = function(...) NULL
    )

    atlas <- create_atlas_from_labels(
      labels,
      input_lut = bad_lut,
      atlas_name = "test_atlas",
      steps = 1,
      verbose = FALSE
    )

    expect_null(atlas$palette)
  })

  it("calls cortical_pipeline for steps > 1", {
    pipeline_called <- FALSE
    local_mocked_bindings(
      check_fs = function(abort = FALSE) invisible(TRUE),
      check_magick = function() invisible(TRUE),
      brain_atlas = function(...) structure(list(...), class = "brain_atlas"),
      brain_data_cortical = function(...) list(...),
      cortical_pipeline = function(...) {
        pipeline_called <<- TRUE
        structure(list(), class = "brain_atlas")
      }
    )

    labels <- unlist(test_label_files())
    withr::local_options(ggsegExtra.output_dir = withr::local_tempdir())

    create_atlas_from_labels(
      labels,
      atlas_name = "test_atlas",
      steps = 1:8,
      verbose = FALSE
    )

    expect_true(pipeline_called)
  })
})


describe("labels_read_files hemisphere-less filenames", {
  it("assigns region without hemi prefix for unknown hemisphere", {
    tmp <- withr::local_tempdir()
    nohemi_file <- file.path(tmp, "some_region.label")
    writeLines(c(
      "#!ascii label",
      "3",
      "100  0.0  0.0  0.0  0.0",
      "101  1.0  1.0  1.0  0.0",
      "102  2.0  2.0  2.0  0.0"
    ), nohemi_file)

    default_colours <- rep(NA_character_, 1)

    result <- labels_read_files(
      c(nohemi_file), NULL, NULL, default_colours
    )

    expect_equal(result$label[1], "some_region")
    expect_true(is.na(result$hemi[1]))
  })
})


describe("create_atlas_from_labels hemi fallback", {
  it("defaults to both hemispheres when all hemi values are NA", {
    captured_hemisphere <- NULL
    local_mocked_bindings(
      check_fs = function(abort = FALSE) invisible(TRUE),
      check_magick = function() invisible(TRUE),
      brain_atlas = function(...) structure(list(...), class = "brain_atlas"),
      brain_data_cortical = function(...) list(...),
      cortical_pipeline = function(...) {
        args <- list(...)
        captured_hemisphere <<- args$hemisphere
        structure(list(), class = "brain_atlas")
      }
    )

    tmp <- withr::local_tempdir()
    nohemi_file <- file.path(tmp, "some_region.label")
    writeLines(c(
      "#!ascii label",
      "3",
      "100  0.0  0.0  0.0  0.0",
      "101  1.0  1.0  1.0  0.0",
      "102  2.0  2.0  2.0  0.0"
    ), nohemi_file)

    withr::local_options(ggsegExtra.output_dir = withr::local_tempdir())

    create_atlas_from_labels(
      c(nohemi_file),
      atlas_name = "test_nohemi",
      steps = 1:8,
      verbose = FALSE
    )

    expect_equal(captured_hemisphere, c("lh", "rh"))
  })
})
