describe("make_atlas_from_labels", {
  it("creates atlas from label files", {
    skip_if_no_freesurfer()

    labels <- unlist(test_label_files())
    atlas <- make_atlas_from_labels(
      labels,
      atlas_name = "test_atlas",
      include_geometry = FALSE,
      verbose = FALSE
    )

    expect_s3_class(atlas, "brain_atlas")
    expect_equal(atlas$atlas, "test_atlas")
    expect_equal(atlas$type, "cortical")
    expect_equal(nrow(atlas$data), 3)
  })

  it("correctly parses hemisphere from filename", {
    skip_if_no_freesurfer()

    labels <- unlist(test_label_files())
    atlas <- make_atlas_from_labels(
      labels,
      include_geometry = FALSE,
      verbose = FALSE
    )

    expect_true("left" %in% atlas$data$hemi)
    expect_true("right" %in% atlas$data$hemi)
    expect_equal(sum(atlas$data$hemi == "left"), 2)
    expect_equal(sum(atlas$data$hemi == "right"), 1)
  })

  it("stores vertices correctly", {
    skip_if_no_freesurfer()

    labels <- unlist(test_label_files())
    atlas <- make_atlas_from_labels(
      labels,
      include_geometry = FALSE,
      verbose = FALSE
    )

    expect_true("vertices" %in% names(atlas$data))
    expect_type(atlas$data$vertices, "list")
    expect_equal(length(atlas$data$vertices[[1]]), 5)
  })

  it("accepts custom region names", {
    skip_if_no_freesurfer()

    labels <- unlist(test_label_files())
    custom_names <- c("Motor", "Visual", "Motor")

    atlas <- make_atlas_from_labels(
      labels,
      region_names = custom_names,
      include_geometry = FALSE,
      verbose = FALSE
    )

    expect_equal(atlas$data$region, custom_names)
  })

  it("accepts custom colours", {
    skip_if_no_freesurfer()

    labels <- unlist(test_label_files())
    custom_colours <- c("#FF0000", "#00FF00", "#0000FF")

    atlas <- make_atlas_from_labels(
      labels,
      colours = custom_colours,
      include_geometry = FALSE,
      verbose = FALSE
    )

    expect_equal(atlas$data$colour, custom_colours)
  })

  it("supports subcortical type", {
    skip_if_no_freesurfer()

    labels <- unlist(test_label_files())
    atlas <- make_atlas_from_labels(
      labels,
      type = "subcortical",
      include_geometry = FALSE,
      verbose = FALSE
    )

    expect_equal(atlas$type, "subcortical")
  })

  it("errors when label files not found", {
    skip_if_no_freesurfer()

    expect_error(
      make_atlas_from_labels(
        c("nonexistent.label"),
        verbose = FALSE
      ),
      "Label files not found"
    )
  })

  it("errors for subcortical with geometry", {
    skip_if_no_freesurfer()

    labels <- unlist(test_label_files())
    expect_error(
      make_atlas_from_labels(
        labels,
        type = "subcortical",
        include_geometry = TRUE,
        verbose = FALSE
      ),
      "2D geometry for subcortical"
    )
  })
})
