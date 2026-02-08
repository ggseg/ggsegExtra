describe("unify_legacy_atlases", {
  it("errors when both atlases are NULL", {
    expect_error(
      unify_legacy_atlases(atlas_2d = NULL, atlas_3d = NULL),
      "At least one"
    )
  })

  it("errors when atlas_2d is wrong class", {
    expect_error(
      unify_legacy_atlases(atlas_2d = "not_an_atlas"),
      "brain_atlas"
    )
  })

  it("errors when atlas_3d lacks ggseg_3d column", {
    bad_3d <- data.frame(hemi = "left", surf = "inflated")

    expect_error(
      unify_legacy_atlases(atlas_3d = bad_3d),
      "ggseg_3d"
    )
  })

  it("runs successfully with valid 2D atlas", {
    mock_2d <- structure(
      list(
        atlas = "test",
        type = "cortical",
        core = data.frame(
          hemi = "left",
          region = "test",
          label = "lh_test",
          stringsAsFactors = FALSE
        ),
        palette = c(lh_test = "#FF0000"),
        data = list(
          sf = NULL,
          vertices = data.frame(
            label = "lh_test",
            stringsAsFactors = FALSE
          )
        )
      ),
      class = "brain_atlas"
    )
    mock_2d$data$vertices$vertices <- list(1:5)

    local_mocked_bindings(
      signal_stage = function(...) invisible(NULL),
      .package = "lifecycle"
    )

    result <- unify_legacy_atlases(atlas_2d = mock_2d)

    expect_s3_class(result, "brain_atlas")
    expect_equal(result$atlas, "test")
  })

  it("runs successfully with valid 3D atlas containing vertices", {
    mock_3d <- data.frame(
      atlas = "test_3d",
      hemi = c("left", "right"),
      surf = c("inflated", "inflated"),
      stringsAsFactors = FALSE
    )
    mock_3d$ggseg_3d <- list(
      data.frame(
        region = "motor",
        label = "lh_motor",
        colour = "#FF0000",
        stringsAsFactors = FALSE
      ),
      data.frame(
        region = "motor",
        label = "rh_motor",
        colour = "#0000FF",
        stringsAsFactors = FALSE
      )
    )
    mock_3d$ggseg_3d[[1]]$vertices <- list(1:10)
    mock_3d$ggseg_3d[[2]]$vertices <- list(11:20)

    local_mocked_bindings(
      signal_stage = function(...) invisible(NULL),
      .package = "lifecycle"
    )

    result <- unify_legacy_atlases(atlas_3d = mock_3d)

    expect_s3_class(result, "brain_atlas")
    expect_equal(result$atlas, "test")
    expect_true("vertices" %in% names(result$data$vertices))
  })

  it("uses custom atlas_name when provided", {
    mock_2d <- structure(
      list(
        atlas = "original_name",
        type = "cortical",
        core = data.frame(
          hemi = "left",
          region = "test",
          label = "lh_test",
          stringsAsFactors = FALSE
        ),
        palette = c(lh_test = "#FF0000"),
        data = list(sf = NULL, vertices = data.frame(label = "lh_test"))
      ),
      class = "brain_atlas"
    )
    mock_2d$data$vertices$vertices <- list(1:5)

    local_mocked_bindings(
      signal_stage = function(...) invisible(NULL),
      .package = "lifecycle"
    )

    result <- unify_legacy_atlases(
      atlas_2d = mock_2d, atlas_name = "custom_name"
    )

    expect_equal(result$atlas, "custom_name")
  })

  it("handles subcortical type with mesh data", {
    mock_3d <- data.frame(
      atlas = "test_3d",
      hemi = c("subcort", "subcort"),
      surf = c("LCBC", "LCBC"),
      stringsAsFactors = FALSE
    )
    mock_mesh <- list(
      vertices = data.frame(x = 1:3, y = 1:3, z = 1:3),
      faces = data.frame(i = 1, j = 2, k = 3)
    )
    mock_3d$ggseg_3d <- list(
      data.frame(
        region = "thalamus",
        label = "Left-Thalamus",
        colour = "#FF0000",
        stringsAsFactors = FALSE
      ),
      data.frame(
        region = "thalamus",
        label = "Right-Thalamus",
        colour = "#0000FF",
        stringsAsFactors = FALSE
      )
    )
    mock_3d$ggseg_3d[[1]]$mesh <- list(mock_mesh)
    mock_3d$ggseg_3d[[2]]$mesh <- list(mock_mesh)

    local_mocked_bindings(
      signal_stage = function(...) invisible(NULL),
      .package = "lifecycle"
    )

    result <- unify_legacy_atlases(atlas_3d = mock_3d, type = "subcortical")

    expect_s3_class(result, "brain_atlas")
    expect_equal(result$type, "subcortical")
  })
})


describe("flatten_ggseg3d_atlas", {
  it("errors when surface not found", {
    mock_atlas <- data.frame(
      atlas = "test_3d",
      hemi = c("left", "right"),
      surf = c("inflated", "inflated"),
      stringsAsFactors = FALSE
    )
    mock_atlas$ggseg_3d <- list(
      data.frame(region = "r1", label = "l1"),
      data.frame(region = "r2", label = "l2")
    )

    expect_error(
      flatten_ggseg3d_atlas(mock_atlas, surface = "pial"),
      "No data found"
    )
  })

  it("flattens nested ggseg_3d structure", {
    mock_atlas <- data.frame(
      atlas = "test_3d",
      hemi = c("left", "right"),
      surf = c("inflated", "inflated"),
      stringsAsFactors = FALSE
    )
    mock_atlas$ggseg_3d <- list(
      data.frame(region = "r1", label = "lh_r1", stringsAsFactors = FALSE),
      data.frame(region = "r2", label = "rh_r2", stringsAsFactors = FALSE)
    )

    result <- flatten_ggseg3d_atlas(mock_atlas, surface = "inflated")

    expect_s3_class(result, "data.frame")
    expect_true("hemi" %in% names(result))
    expect_true("surf" %in% names(result))
    expect_equal(nrow(result), 2)
  })
})


describe("match_vertices", {
  it("matches exact coordinates", {
    region_coords <- matrix(
      c(0, 0, 0, 1, 1, 1, 2, 2, 2), ncol = 3, byrow = TRUE
    )
    brain_coords <- matrix(
      c(0, 0, 0, 1, 1, 1, 2, 2, 2, 3, 3, 3),
      ncol = 3, byrow = TRUE
    )

    result <- match_vertices(region_coords, brain_coords, tolerance = 1e-4)

    expect_type(result, "integer")
    expect_equal(length(result), 3)
    expect_equal(sort(result), c(0, 1, 2))
  })

  it("respects tolerance", {
    region_coords <- matrix(c(0, 0, 0), ncol = 3, byrow = TRUE)
    brain_coords <- matrix(c(0.1, 0.1, 0.1), ncol = 3, byrow = TRUE)

    result_strict <- match_vertices(
      region_coords, brain_coords, tolerance = 0.01
    )
    result_loose <- match_vertices(
      region_coords, brain_coords, tolerance = 0.5
    )

    expect_length(result_strict, 0)
    expect_length(result_loose, 1)
  })

  it("returns unique indices", {
    region_coords <- matrix(c(0, 0, 0, 0, 0, 0), ncol = 3, byrow = TRUE)
    brain_coords <- matrix(c(0, 0, 0), ncol = 3, byrow = TRUE)

    result <- match_vertices(region_coords, brain_coords, tolerance = 1e-4)

    expect_length(unique(result), length(result))
  })
})
