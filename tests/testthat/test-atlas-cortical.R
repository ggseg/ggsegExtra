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

    annot_dir <- file.path(freesurfer::fs_subj_dir(), "fsaverage5", "label")
    annot_files <- c(
      file.path(annot_dir, "lh.aparc.annot"),
      file.path(annot_dir, "rh.aparc.annot")
    )

    atlas <- create_cortical_atlas(
      input_annot = annot_files,
      steps = 1,
      verbose = FALSE
    )

    expect_s3_class(atlas, "brain_atlas")
    expect_equal(atlas$type, "cortical")
    expect_true(nrow(atlas$core) > 0)
  })

  it("includes vertices for 3D rendering", {
    skip_if_no_freesurfer()

    annot_dir <- file.path(freesurfer::fs_subj_dir(), "fsaverage5", "label")
    annot_files <- c(
      file.path(annot_dir, "lh.aparc.annot"),
      file.path(annot_dir, "rh.aparc.annot")
    )

    atlas <- create_cortical_atlas(
      input_annot = annot_files,
      steps = 1,
      verbose = FALSE
    )

    vertices <- ggseg.formats::atlas_vertices(atlas)
    expect_true(nrow(vertices) > 0)
    expect_true("vertices" %in% names(vertices))
  })

  it("works with different annotations", {
    skip_if_no_freesurfer()

    annot_dir <- file.path(freesurfer::fs_subj_dir(), "fsaverage5", "label")
    aparc_files <- c(
      file.path(annot_dir, "lh.aparc.annot"),
      file.path(annot_dir, "rh.aparc.annot")
    )

    atlas_aparc <- create_cortical_atlas(
      input_annot = aparc_files,
      steps = 1,
      verbose = FALSE
    )

    expect_true(nrow(atlas_aparc$core) > 0)

    a2009s_files <- c(
      file.path(annot_dir, "lh.aparc.a2009s.annot"),
      file.path(annot_dir, "rh.aparc.a2009s.annot")
    )
    has_a2009s <- all(file.exists(a2009s_files))

    if (has_a2009s) {
      atlas_a2009s <- create_cortical_atlas(
        input_annot = a2009s_files,
        steps = 1,
        verbose = FALSE
      )
      expect_true(nrow(atlas_a2009s$core) > 0)
      expect_false(identical(atlas_aparc$core$region, atlas_a2009s$core$region))
    }
  })

  it("can render with ggseg3d", {
    skip_if_no_freesurfer()

    annot_dir <- file.path(freesurfer::fs_subj_dir(), "fsaverage5", "label")
    annot_files <- c(
      file.path(annot_dir, "lh.aparc.annot"),
      file.path(annot_dir, "rh.aparc.annot")
    )

    atlas <- create_cortical_atlas(
      input_annot = annot_files,
      steps = 1,
      verbose = FALSE
    )

    expect_no_error({
      p <- ggseg3d::ggseg3d(atlas = atlas, hemisphere = "left")
    })
  })
})


describe("read_annotation_data", {
  it("reads annotation data from files", {
    skip_if_no_freesurfer()

    annot_dir <- file.path(freesurfer::fs_subj_dir(), "fsaverage5", "label")
    annot_files <- c(
      file.path(annot_dir, "lh.aparc.annot"),
      file.path(annot_dir, "rh.aparc.annot")
    )

    atlas_data <- read_annotation_data(annot_files)

    expect_s3_class(atlas_data, "tbl_df")
    expect_true(all(
      c("hemi", "region", "label", "colour", "vertices") %in% names(atlas_data)
    ))
    expect_true(nrow(atlas_data) > 0)
  })

  it("reads annotation from custom files", {
    annot_files <- test_annot_files()
    skip_if(!file.exists(annot_files$lh), "Test annotation files not found")

    atlas_data <- read_annotation_data(c(annot_files$lh, annot_files$rh))

    expect_s3_class(atlas_data, "tbl_df")
    expect_true(nrow(atlas_data) > 0)
  })

  it("returns data for both hemispheres", {
    skip_if_no_freesurfer()

    annot_dir <- file.path(freesurfer::fs_subj_dir(), "fsaverage5", "label")
    annot_files <- c(
      file.path(annot_dir, "lh.aparc.annot"),
      file.path(annot_dir, "rh.aparc.annot")
    )

    atlas_data <- read_annotation_data(annot_files)

    expect_true("left" %in% atlas_data$hemi)
    expect_true("right" %in% atlas_data$hemi)
  })

  it("creates proper labels with hemisphere prefix", {
    skip_if_no_freesurfer()

    annot_dir <- file.path(freesurfer::fs_subj_dir(), "fsaverage5", "label")
    annot_files <- c(
      file.path(annot_dir, "lh.aparc.annot"),
      file.path(annot_dir, "rh.aparc.annot")
    )

    atlas_data <- read_annotation_data(annot_files)

    lh_labels <- atlas_data$label[atlas_data$hemi == "left"]
    rh_labels <- atlas_data$label[atlas_data$hemi == "right"]

    expect_true(all(grepl("^lh_", lh_labels)))
    expect_true(all(grepl("^rh_", rh_labels)))
  })

  it("includes vertex indices as list column", {
    skip_if_no_freesurfer()

    annot_dir <- file.path(freesurfer::fs_subj_dir(), "fsaverage5", "label")
    annot_files <- c(
      file.path(annot_dir, "lh.aparc.annot"),
      file.path(annot_dir, "rh.aparc.annot")
    )

    atlas_data <- read_annotation_data(annot_files)

    expect_type(atlas_data$vertices, "list")
    expect_true(all(vapply(atlas_data$vertices, is.integer, logical(1))))
    expect_true(all(vapply(atlas_data$vertices, function(x) length(x) > 0, logical(1))))
  })

  it("sets NA colour for medial wall/unknown regions", {
    skip_if_no_freesurfer()

    annot_dir <- file.path(freesurfer::fs_subj_dir(), "fsaverage5", "label")
    annot_files <- c(
      file.path(annot_dir, "lh.aparc.annot"),
      file.path(annot_dir, "rh.aparc.annot")
    )

    atlas_data <- read_annotation_data(annot_files)

    wall_rows <- grepl("wall|unknown", atlas_data$region, ignore.case = TRUE)
    if (any(wall_rows)) {
      expect_true(all(is.na(atlas_data$colour[wall_rows])))
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

  it("accepts custom region names", {
    skip_if_no_freesurfer()

    labels <- unlist(test_label_files())
    custom_names <- c("Motor", "Visual", "Motor")

    atlas <- create_atlas_from_labels(
      labels,
      region_names = custom_names,
      steps = 1,
      verbose = FALSE
    )

    expect_equal(atlas$core$region, custom_names)
  })

  it("accepts custom colours", {
    skip_if_no_freesurfer()

    labels <- unlist(test_label_files())
    custom_colours <- c("#FF0000", "#00FF00", "#0000FF")

    atlas <- create_atlas_from_labels(
      labels,
      colours = custom_colours,
      steps = 1,
      verbose = FALSE
    )

    expect_true(all(custom_colours %in% atlas$palette))
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
