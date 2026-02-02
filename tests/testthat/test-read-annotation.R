describe("read_annotation_data", {
  it("reads annotation data from FreeSurfer subject", {
    skip_if_no_freesurfer()

    atlas_data <- read_annotation_data(
      annot = "aparc",
      subject = "fsaverage5"
    )

    expect_s3_class(atlas_data, "tbl_df")
    expect_true(all(c("hemi", "region", "label", "colour", "vertices") %in% names(atlas_data)))
    expect_true(nrow(atlas_data) > 0)
  })

  it("returns data for both hemispheres", {
    skip_if_no_freesurfer()

    atlas_data <- read_annotation_data(
      annot = "aparc",
      subject = "fsaverage5"
    )

    expect_true("left" %in% atlas_data$hemi)
    expect_true("right" %in% atlas_data$hemi)
  })

  it("creates proper labels with hemisphere prefix", {
    skip_if_no_freesurfer()

    atlas_data <- read_annotation_data(
      annot = "aparc",
      subject = "fsaverage5"
    )

    lh_labels <- atlas_data$label[atlas_data$hemi == "left"]
    rh_labels <- atlas_data$label[atlas_data$hemi == "right"]

    expect_true(all(grepl("^lh_", lh_labels)))
    expect_true(all(grepl("^rh_", rh_labels)))
  })

  it("includes vertex indices as list column", {
    skip_if_no_freesurfer()

    atlas_data <- read_annotation_data(
      annot = "aparc",
      subject = "fsaverage5"
    )

    expect_type(atlas_data$vertices, "list")
    expect_true(all(sapply(atlas_data$vertices, is.integer)))
    expect_true(all(sapply(atlas_data$vertices, function(x) length(x) > 0)))
  })

  it("sets NA colour for medial wall/unknown regions", {
    skip_if_no_freesurfer()

    atlas_data <- read_annotation_data(
      annot = "aparc",
      subject = "fsaverage5"
    )

    wall_rows <- grepl("wall|unknown", atlas_data$region, ignore.case = TRUE)
    if (any(wall_rows)) {
      expect_true(all(is.na(atlas_data$colour[wall_rows])))
    }
  })
})
