describe("tessellate_label", {
  it("creates mesh from volume label", {
    skip_if_no_freesurfer()

    vol_file <- test_mgz_file()
    skip_if(!file.exists(vol_file), "Test volume file not found")

    vol <- read_volume(vol_file)
    labels <- unique(c(vol))
    labels <- labels[labels != 0]
    skip_if(length(labels) == 0, "No labels in test volume")

    output_dir <- tempdir()
    mesh <- tessellate_label(
      volume_file = vol_file,
      label_id = labels[1],
      output_dir = output_dir,
      verbose = FALSE
    )

    expect_true(is.list(mesh))
    expect_true(all(c("vertices", "faces") %in% names(mesh)))
    expect_true(nrow(mesh$vertices) > 0)
    expect_true(nrow(mesh$faces) > 0)
  })
})


describe("generate_colortable_from_volume", {
  it("creates color table with correct structure", {
    skip_if_no_freesurfer()

    vol_file <- test_mgz_file()
    skip_if(!file.exists(vol_file), "Test volume file not found")

    result <- generate_colortable_from_volume(vol_file)

    expect_s3_class(result, "data.frame")
    expect_true(all(
      c("idx", "label", "R", "G", "B", "A", "roi", "color") %in%
        names(result)
    ))
    expect_true(nrow(result) > 0)
    expect_true(all(result$idx > 0))
    expect_true(all(grepl("^region_", result$label)))
  })
})
