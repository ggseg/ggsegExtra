# Test suite for {GGSEG} atlas
# Tests atlas compliance with ggseg ecosystem standards

describe("{GGSEG} atlas structure", {
  it("is a valid brain_atlas object", {
    expect_s3_class({GGSEG}, "brain_atlas")
  })

  it("has required columns", {
    required_cols <- c("hemi", "region", "label", "colour")
    expect_true(all(required_cols %in% names({GGSEG}$data)))
  })

  it("has valid hemisphere values", {
    valid_hemis <- c("left", "right")
    hemis <- unique({GGSEG}$data$hemi)
    hemis <- hemis[!is.na(hemis)]
    expect_true(all(hemis %in% valid_hemis))
  })

  it("has atlas name and type", {
    expect_true(nchar({GGSEG}$atlas) > 0)
    expect_true({GGSEG}$type %in% c("cortical", "subcortical"))
  })

  it("has non-empty data", {
    expect_true(nrow({GGSEG}$data) > 0)
  })
})


describe("{GGSEG} 3D rendering support", {
  it("has vertices column for 3D rendering", {
    expect_true("vertices" %in% names({GGSEG}$data))
  })

  it("vertices are integer vectors", {
    vertices <- {GGSEG}$data$vertices
    expect_type(vertices, "list")

    non_na_vertices <- vertices[!vapply(vertices, is.null, logical(1))]
    if (length(non_na_vertices) > 0) {
      expect_true(all(vapply(non_na_vertices, is.integer, logical(1)) |
                      vapply(non_na_vertices, is.numeric, logical(1))))
    }
  })
})


describe("{GGSEG} 2D plotting", {
  it("can be plotted with ggseg", {
    skip_if_not_installed("ggseg")

    p <- ggseg::ggseg(atlas = {GGSEG})
    expect_s3_class(p, c("gg", "ggplot"))
  })

  it("can be plotted with region fill", {
    skip_if_not_installed("ggseg")
    skip_if_not_installed("ggplot2")

    p <- ggseg::ggseg(
      atlas = {GGSEG},
      mapping = ggplot2::aes(fill = region)
    )
    expect_s3_class(p, c("gg", "ggplot"))
  })
})


describe("{GGSEG} 3D plotting", {
  it("can be rendered with ggseg3d", {
    skip_if_not_installed("ggseg3d")
    skip_on_ci()  # 3D rendering may not work in CI

    p <- ggseg3d::ggseg3d(atlas = {GGSEG})
    expect_s3_class(p, c("plotly", "htmlwidget"))
  })
})


describe("{GGSEG} data quality", {
  it("has unique labels per hemisphere", {
    labels_by_hemi <- split({GGSEG}$data$label, {GGSEG}$data$hemi)

    for (hemi in names(labels_by_hemi)) {
      labels <- labels_by_hemi[[hemi]]
      labels <- labels[!is.na(labels)]
      expect_equal(
        length(labels),
        length(unique(labels)),
        info = paste("Duplicate labels in", hemi, "hemisphere")
      )
    }
  })

  it("has no empty region names (except NA for wall/unknown)", {
    regions <- {GGSEG}$data$region
    non_na_regions <- regions[!is.na(regions)]
    expect_true(all(nchar(non_na_regions) > 0))
  })

  it("has valid hex colours", {
    colours <- {GGSEG}$data$colour
    non_na_colours <- colours[!is.na(colours)]

    if (length(non_na_colours) > 0) {
      # Check hex format or valid R colour names
      is_valid <- vapply(non_na_colours, function(col) {
        grepl("^#[0-9A-Fa-f]{6}$|^#[0-9A-Fa-f]{8}$", col) ||
        col %in% grDevices::colours()
      }, logical(1))
      expect_true(all(is_valid))
    }
  })
})
