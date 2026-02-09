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

  it("converts legacy 2D atlas without core", {
    mock_2d <- structure(
      list(
        atlas = "old_atlas",
        type = "cortical",
        core = NULL,
        palette = c(lh_test = "#FF0000"),
        data = list(sf = NULL, vertices = NULL),
        sf = NULL
      ),
      class = "brain_atlas"
    )

    local_mocked_bindings(
      signal_stage = function(...) invisible(NULL),
      .package = "lifecycle"
    )
    local_mocked_bindings(
      convert_legacy_brain_atlas = function(atlas) {
        atlas$core <- data.frame(
          hemi = "left", region = "test", label = "lh_test",
          stringsAsFactors = FALSE
        )
        vdf <- data.frame(label = "lh_test", stringsAsFactors = FALSE)
        vdf$vertices <- list(1:10)
        atlas$data$vertices <- vdf
        atlas
      },
      .package = "ggseg.formats"
    )

    result <- unify_legacy_atlases(atlas_2d = mock_2d)

    expect_s3_class(result, "brain_atlas")
    expect_equal(result$atlas, "old_atlas")
  })

  it("extracts sf from data$sf when available", {
    mock_sf <- sf::st_sf(
      label = "lh_test",
      view = "lateral",
      geometry = sf::st_sfc(
        sf::st_polygon(list(matrix(c(0, 0, 1, 0, 1, 1, 0, 0), ncol = 2,
                                   byrow = TRUE)))
      )
    )
    vdf <- data.frame(label = "lh_test", stringsAsFactors = FALSE)
    vdf$vertices <- list(1:10)
    mock_2d <- structure(
      list(
        atlas = "test",
        type = "cortical",
        core = data.frame(
          hemi = "left", region = "test", label = "lh_test",
          stringsAsFactors = FALSE
        ),
        palette = c(lh_test = "#FF0000"),
        data = list(sf = mock_sf, vertices = vdf)
      ),
      class = "brain_atlas"
    )

    local_mocked_bindings(
      signal_stage = function(...) invisible(NULL),
      .package = "lifecycle"
    )

    result <- unify_legacy_atlases(atlas_2d = mock_2d)

    expect_s3_class(result, "brain_atlas")
    expect_false(is.null(result$data$sf))
  })

  it("warns when vertex inference fails for cortical 3D atlas", {
    mock_3d <- data.frame(
      atlas = "test_3d",
      hemi = c("left", "right"),
      surf = c("inflated", "inflated"),
      stringsAsFactors = FALSE
    )
    mock_3d$ggseg_3d <- list(
      data.frame(
        region = "motor", label = "lh_motor", colour = "#FF0000",
        stringsAsFactors = FALSE
      ),
      data.frame(
        region = "motor", label = "rh_motor", colour = "#0000FF",
        stringsAsFactors = FALSE
      )
    )
    mock_3d$ggseg_3d[[1]]$vertices <- list(integer(0))
    mock_3d$ggseg_3d[[2]]$vertices <- list(integer(0))

    local_mocked_bindings(
      signal_stage = function(...) invisible(NULL),
      .package = "lifecycle"
    )
    local_mocked_bindings(
      infer_vertices_from_meshes = function(...) NULL
    )

    warned <- FALSE
    tryCatch(
      withCallingHandlers(
        unify_legacy_atlases(atlas_3d = mock_3d),
        warning = function(w) {
          if (grepl("Could not infer", conditionMessage(w))) warned <<- TRUE
          invokeRestart("muffleWarning")
        }
      ),
      error = function(e) NULL
    )
    expect_true(warned)
  })

  it("extracts rgl-style meshes with vb/it columns", {
    mock_3d <- data.frame(
      atlas = "test_3d",
      hemi = c("subcort"),
      surf = c("LCBC"),
      stringsAsFactors = FALSE
    )
    rgl_mesh <- list(
      vb = matrix(c(1, 2, 3, 4, 5, 6, 7, 8, 9), nrow = 3),
      it = matrix(c(1, 2, 3), nrow = 3)
    )
    mock_3d$ggseg_3d <- list(
      data.frame(
        region = "thalamus", label = "Left-Thalamus", colour = "#FF0000",
        stringsAsFactors = FALSE
      )
    )
    mock_3d$ggseg_3d[[1]]$mesh <- list(rgl_mesh)

    local_mocked_bindings(
      signal_stage = function(...) invisible(NULL),
      .package = "lifecycle"
    )

    result <- unify_legacy_atlases(atlas_3d = mock_3d, type = "subcortical")

    expect_s3_class(result, "brain_atlas")
    expect_false(is.null(result$data$meshes))
    mesh <- result$data$meshes$mesh[[1]]
    expect_equal(mesh$vertices$x, c(1, 4, 7))
  })
})


describe("compute_vertex_indices", {
  it("warns when brain mesh loading fails", {
    mock_flat <- data.frame(
      hemi = c("left"),
      label = c("lh_motor"),
      stringsAsFactors = FALSE
    )
    mock_flat$mesh <- list(
      list(vertices = data.frame(x = 1:3, y = 1:3, z = 1:3))
    )

    local_mocked_bindings(
      get_brain_mesh = function(...) stop("no mesh")
    )

    expect_warning(
      expect_warning(
        result <- compute_vertex_indices(mock_flat),
        "Could not load brain mesh"
      ),
      "Could not load brain mesh"
    )
  })

  it("warns when no vertices match", {
    mock_flat <- data.frame(
      hemi = c("left"),
      label = c("lh_motor"),
      stringsAsFactors = FALSE
    )
    mock_flat$mesh <- list(
      list(vertices = data.frame(x = 100:102, y = 100:102, z = 100:102))
    )

    local_mocked_bindings(
      get_brain_mesh = function(...) {
        list(vertices = data.frame(x = 1:3, y = 1:3, z = 1:3))
      }
    )

    expect_warning(
      result <- compute_vertex_indices(mock_flat, tolerance = 1e-6),
      "No vertices matched"
    )
  })

  it("matches vertices when coordinates align", {
    mock_flat <- data.frame(
      hemi = c("left"),
      label = c("lh_motor"),
      surf = c("inflated"),
      stringsAsFactors = FALSE
    )
    mock_flat$mesh <- list(
      list(vertices = data.frame(x = c(1, 2), y = c(1, 2), z = c(1, 2)))
    )

    local_mocked_bindings(
      get_brain_mesh = function(...) {
        list(vertices = data.frame(x = 1:5, y = 1:5, z = 1:5))
      }
    )

    result <- compute_vertex_indices(mock_flat, tolerance = 0.1)

    expect_true(length(result[["lh_motor"]]) > 0)
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


describe("infer_atlas_type", {
  it("returns atlas_2d type when has_2d is TRUE", {
    mock_2d <- list(type = "cortical")
    result <- infer_atlas_type(TRUE, mock_2d, NULL)
    expect_equal(result, "cortical")
  })

  it("returns subcortical when atlas_3d has subcort hemi", {
    mock_3d <- data.frame(hemi = c("subcort", "subcort"))
    result <- infer_atlas_type(FALSE, NULL, mock_3d)
    expect_equal(result, "subcortical")
  })

  it("defaults to cortical when no subcort hemi", {
    mock_3d <- data.frame(hemi = c("left", "right"))
    result <- infer_atlas_type(FALSE, NULL, mock_3d)
    expect_equal(result, "cortical")
  })
})


describe("has_vertex_data", {
  it("returns FALSE when no vertices column", {
    dt <- data.frame(label = "test")
    expect_false(has_vertex_data(dt))
  })

  it("returns FALSE when all vertices are empty", {
    dt <- data.frame(label = c("a", "b"))
    dt$vertices <- list(integer(0), integer(0))
    expect_false(has_vertex_data(dt))
  })

  it("returns TRUE when some vertices have data", {
    dt <- data.frame(label = c("a", "b"))
    dt$vertices <- list(1:5, integer(0))
    expect_true(has_vertex_data(dt))
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


describe("unify_legacy_atlases subcortical NULL mesh", {
  it("handles NULL mesh entries in subcortical 3D atlas", {
    mock_3d <- data.frame(
      atlas = "test_3d",
      hemi = c("subcort"),
      surf = c("LCBC"),
      stringsAsFactors = FALSE
    )
    mock_3d$ggseg_3d <- list(
      data.frame(
        region = "thalamus",
        label = "Left-Thalamus",
        colour = "#FF0000",
        stringsAsFactors = FALSE
      )
    )
    mock_3d$ggseg_3d[[1]]$mesh <- list(NULL)

    local_mocked_bindings(
      signal_stage = function(...) invisible(NULL),
      .package = "lifecycle"
    )
    local_mocked_bindings(
      subcortical_data = function(sf = NULL, meshes = NULL) {
        list(sf = sf, meshes = meshes)
      },
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
      .package = "ggseg.formats"
    )

    result <- unify_legacy_atlases(atlas_3d = mock_3d, type = "subcortical")

    expect_s3_class(result, "brain_atlas")
    expect_null(result$data$meshes$mesh[[1]])
  })
})


describe("unify_legacy_atlases compute_vertices path", {
  it("computes vertex indices when compute_vertices is TRUE", {
    mock_3d <- data.frame(
      atlas = "test_3d",
      hemi = c("left", "right"),
      surf = c("inflated", "inflated"),
      stringsAsFactors = FALSE
    )
    mock_3d$ggseg_3d <- list(
      data.frame(
        region = "motor", label = "lh_motor", colour = "#FF0000",
        stringsAsFactors = FALSE
      ),
      data.frame(
        region = "motor", label = "rh_motor", colour = "#0000FF",
        stringsAsFactors = FALSE
      )
    )
    mock_3d$ggseg_3d[[1]]$mesh <- list(
      list(vertices = data.frame(x = c(1, 2), y = c(1, 2), z = c(1, 2)))
    )
    mock_3d$ggseg_3d[[2]]$mesh <- list(
      list(vertices = data.frame(x = c(3, 4), y = c(3, 4), z = c(3, 4)))
    )

    local_mocked_bindings(
      signal_stage = function(...) invisible(NULL),
      .package = "lifecycle"
    )
    local_mocked_bindings(
      flatten_ggseg3d_atlas = function(atlas_3d, surface) {
        df <- data.frame(
          hemi = c("left", "right"),
          label = c("lh_motor", "rh_motor"),
          surf = c("inflated", "inflated"),
          stringsAsFactors = FALSE
        )
        df$mesh <- list(
          list(vertices = data.frame(x = c(1, 2), y = c(1, 2), z = c(1, 2))),
          list(vertices = data.frame(x = c(3, 4), y = c(3, 4), z = c(3, 4)))
        )
        df
      },
      compute_vertex_indices = function(atlas_3d_flat, ...) {
        list(lh_motor = c(0L, 1L), rh_motor = c(2L, 3L))
      }
    )

    result <- unify_legacy_atlases(
      atlas_3d = mock_3d,
      compute_vertices = TRUE,
      type = "cortical"
    )

    expect_s3_class(result, "brain_atlas")
    vertices <- result$data$vertices
    expect_true(!is.null(vertices))
    expect_true(all(c("lh_motor", "rh_motor") %in% vertices$label))
    expect_equal(vertices$vertices[[1]], c(0L, 1L))
  })

  it("assigns empty vertices for labels not in computed list", {
    mock_3d <- data.frame(
      atlas = "test_3d",
      hemi = c("left"),
      surf = c("inflated"),
      stringsAsFactors = FALSE
    )
    mock_3d$ggseg_3d <- list(
      data.frame(
        region = "motor", label = "lh_motor", colour = "#FF0000",
        stringsAsFactors = FALSE
      )
    )
    mock_3d$ggseg_3d[[1]]$mesh <- list(
      list(vertices = data.frame(x = 1, y = 1, z = 1))
    )

    local_mocked_bindings(
      signal_stage = function(...) invisible(NULL),
      .package = "lifecycle"
    )
    local_mocked_bindings(
      flatten_ggseg3d_atlas = function(atlas_3d, surface) {
        df <- data.frame(
          hemi = "left", label = "lh_motor", surf = "inflated",
          stringsAsFactors = FALSE
        )
        df$mesh <- list(
          list(vertices = data.frame(x = 1, y = 1, z = 1))
        )
        df
      },
      compute_vertex_indices = function(...) {
        list(lh_other = c(5L))
      }
    )
    local_mocked_bindings(
      cortical_data = function(sf = NULL, vertices = NULL) {
        list(sf = sf, vertices = vertices)
      },
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
      .package = "ggseg.formats"
    )

    result <- unify_legacy_atlases(
      atlas_3d = mock_3d,
      compute_vertices = TRUE,
      type = "cortical"
    )

    vertices <- result$data$vertices
    expect_equal(vertices$vertices[[1]], integer(0))
  })
})


describe("unify_legacy_atlases 2D-only without vertex data", {
  it("passes NULL vertices and informs when 2D only with no vertices", {
    mock_2d <- structure(
      list(
        atlas = "test",
        type = "cortical",
        core = data.frame(
          hemi = "left", region = "test", label = "lh_test",
          stringsAsFactors = FALSE
        ),
        palette = c(lh_test = "#FF0000"),
        data = list(sf = NULL, vertices = NULL)
      ),
      class = "brain_atlas"
    )

    local_mocked_bindings(
      signal_stage = function(...) invisible(NULL),
      .package = "lifecycle"
    )
    local_mocked_bindings(
      cortical_data = function(sf = NULL, vertices = NULL) {
        list(sf = sf, vertices = vertices)
      },
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
      .package = "ggseg.formats"
    )

    msgs <- capture.output(
      result <- unify_legacy_atlases(atlas_2d = mock_2d),
      type = "message"
    )

    expect_s3_class(result, "brain_atlas")
    expect_true(any(grepl("2D only", msgs)))
    expect_true(any(grepl("3D rendering will not be available", msgs)))
  })
})


describe("compute_vertex_indices NULL region mesh", {
  it("returns empty vertices for region with NULL mesh", {
    mock_flat <- data.frame(
      hemi = c("left", "left"),
      label = c("lh_motor", "lh_visual"),
      surf = c("inflated", "inflated"),
      stringsAsFactors = FALSE
    )
    mock_flat$mesh <- list(
      list(vertices = data.frame(x = c(1, 2), y = c(1, 2), z = c(1, 2))),
      NULL
    )

    local_mocked_bindings(
      get_brain_mesh = function(...) {
        list(vertices = data.frame(x = 1:5, y = 1:5, z = 1:5))
      }
    )

    result <- compute_vertex_indices(mock_flat, tolerance = 0.1)

    expect_equal(result[["lh_visual"]], integer(0))
    expect_true(length(result[["lh_motor"]]) > 0)
  })

  it("returns empty vertices for region with NULL vertices inside mesh", {
    mock_flat <- data.frame(
      hemi = c("left"),
      label = c("lh_motor"),
      surf = c("inflated"),
      stringsAsFactors = FALSE
    )
    mock_flat$mesh <- list(
      list(vertices = NULL)
    )

    local_mocked_bindings(
      get_brain_mesh = function(...) {
        list(vertices = data.frame(x = 1:5, y = 1:5, z = 1:5))
      }
    )

    result <- compute_vertex_indices(mock_flat, tolerance = 0.1)

    expect_equal(result[["lh_motor"]], integer(0))
  })
})


describe("compute_vertex_indices low match percentage", {
  it("warns when less than 50 percent of vertices matched", {
    mock_flat <- data.frame(
      hemi = c("left"),
      label = c("lh_motor"),
      surf = c("inflated"),
      stringsAsFactors = FALSE
    )
    mock_flat$mesh <- list(
      list(vertices = data.frame(
        x = c(1, 2, 100, 200, 300),
        y = c(1, 2, 100, 200, 300),
        z = c(1, 2, 100, 200, 300)
      ))
    )

    local_mocked_bindings(
      get_brain_mesh = function(...) {
        list(vertices = data.frame(x = 1:5, y = 1:5, z = 1:5))
      }
    )

    expect_warning(
      result <- compute_vertex_indices(mock_flat, tolerance = 0.1),
      "Only.*% of vertices matched"
    )

    expect_true(length(result[["lh_motor"]]) > 0)
    expect_true(length(result[["lh_motor"]]) < 5)
  })
})


describe("remap_palette_to_labels", {
  it("remaps region-keyed palette to label-keyed", {
    palette <- c("motor" = "#FF0000", "visual" = "#0000FF")
    core <- data.frame(
      region = c("motor", "motor", "visual"),
      label = c("lh_motor", "rh_motor", "lh_visual"),
      stringsAsFactors = FALSE
    )

    result <- remap_palette_to_labels(palette, core)

    expect_equal(result[["lh_motor"]], "#FF0000")
    expect_equal(result[["rh_motor"]], "#FF0000")
    expect_equal(result[["lh_visual"]], "#0000FF")
  })

  it("returns NULL for NULL palette", {
    core <- data.frame(region = "test", label = "lh_test")
    expect_null(remap_palette_to_labels(NULL, core))
  })

  it("returns NULL when no regions match", {
    palette <- c("unknown" = "#FF0000")
    core <- data.frame(region = "motor", label = "lh_motor")
    expect_null(remap_palette_to_labels(palette, core))
  })

  it("skips NA regions in core", {
    palette <- c("motor" = "#FF0000")
    core <- data.frame(
      region = c("motor", NA),
      label = c("lh_motor", "lh_medialwall"),
      stringsAsFactors = FALSE
    )

    result <- remap_palette_to_labels(palette, core)

    expect_equal(names(result), "lh_motor")
  })
})


describe("unify_legacy_atlases with ggsegDKT", {
  it("converts 2D-only DKT atlas", {
    skip_if_not_installed("ggsegDKT")

    local_mocked_bindings(
      signal_stage = function(...) invisible(NULL),
      .package = "lifecycle"
    )

    dkt <- ggsegDKT::dkt

    result <- suppressWarnings(suppressMessages(
      unify_legacy_atlases(atlas_2d = dkt)
    ))

    expect_s3_class(result, "brain_atlas")
    expect_equal(result$atlas, "dkt")
    expect_equal(result$type, "cortical")
    expect_false(is.null(result$core))
    expect_false(is.null(result$data$sf))
    expect_false(is.null(result$palette))
    expect_true(any(names(result$palette) %in% result$core$label))
  })

  it("converts 2D + 3D DKT atlas with inferred vertices", {
    skip_if_not_installed("ggsegDKT")

    local_mocked_bindings(
      signal_stage = function(...) invisible(NULL),
      .package = "lifecycle"
    )

    dkt <- ggsegDKT::dkt
    dkt_3d <- ggsegDKT::dkt_3d

    result <- suppressWarnings(suppressMessages(
      unify_legacy_atlases(atlas_2d = dkt, atlas_3d = dkt_3d)
    ))

    expect_s3_class(result, "brain_atlas")
    expect_equal(result$atlas, "dkt")
    expect_equal(result$type, "cortical")
    expect_false(is.null(result$core))
    expect_false(is.null(result$data$sf))
    expect_false(is.null(result$palette))
    expect_true(any(names(result$palette) %in% result$core$label))
    expect_false(is.null(result$data$vertices))
  })

  it("converts 3D-only DKT atlas with inferred vertices", {
    skip_if_not_installed("ggsegDKT")

    local_mocked_bindings(
      signal_stage = function(...) invisible(NULL),
      .package = "lifecycle"
    )

    dkt_3d <- ggsegDKT::dkt_3d

    result <- suppressWarnings(suppressMessages(
      unify_legacy_atlases(atlas_3d = dkt_3d)
    ))

    expect_s3_class(result, "brain_atlas")
    expect_equal(result$type, "cortical")
    expect_false(is.null(result$data$vertices))
    expect_true("label" %in% names(result$data$vertices))
    expect_true("vertices" %in% names(result$data$vertices))
    expect_true(nrow(result$data$vertices) > 0)
  })
})


describe("infer_vertices_from_meshes", {
  it("returns correct 0-based indices with matching coordinates", {
    brain_verts <- data.frame(
      x = c(1.0, 2.0, 3.0, 4.0, 5.0),
      y = c(10.0, 20.0, 30.0, 40.0, 50.0),
      z = c(100.0, 200.0, 300.0, 400.0, 500.0)
    )
    mock_brain_meshes <- list(
      lh_inflated = list(vertices = brain_verts)
    )

    mock_3d <- data.frame(
      atlas = "test",
      hemi = "left",
      surf = "inflated",
      stringsAsFactors = FALSE
    )
    region_mesh <- list(
      vertices = data.frame(x = c(1.0, 3.0, 5.0),
                            y = c(10.0, 30.0, 50.0),
                            z = c(100.0, 300.0, 500.0))
    )
    mock_3d$ggseg_3d <- list(
      data.frame(label = "lh_motor", region = "motor",
                 stringsAsFactors = FALSE)
    )
    mock_3d$ggseg_3d[[1]]$mesh <- list(region_mesh)

    result <- infer_vertices_from_meshes(
      mock_3d, surface = "inflated", brain_meshes = mock_brain_meshes
    )

    expect_type(result, "list")
    expect_true("lh_motor" %in% names(result))
    expect_equal(sort(result[["lh_motor"]]), c(0L, 2L, 4L))
  })

  it("returns NULL when brain_meshes is NULL", {
    mock_3d <- data.frame(
      atlas = "test", hemi = "left", surf = "inflated",
      stringsAsFactors = FALSE
    )
    mock_3d$ggseg_3d <- list(
      data.frame(label = "lh_motor", region = "motor",
                 stringsAsFactors = FALSE)
    )

    result <- infer_vertices_from_meshes(
      mock_3d, surface = "inflated", brain_meshes = NULL
    )

    expect_null(result)
  })

  it("skips regions with no mesh column", {
    mock_brain_meshes <- list(
      lh_inflated = list(
        vertices = data.frame(x = 1:3, y = 1:3, z = 1:3)
      )
    )
    mock_3d <- data.frame(
      atlas = "test", hemi = "left", surf = "inflated",
      stringsAsFactors = FALSE
    )
    mock_3d$ggseg_3d <- list(
      data.frame(label = "lh_motor", region = "motor",
                 stringsAsFactors = FALSE)
    )

    result <- infer_vertices_from_meshes(
      mock_3d, surface = "inflated", brain_meshes = mock_brain_meshes
    )

    expect_null(result)
  })

  it("handles rgl-style vb mesh format", {
    brain_verts <- data.frame(
      x = c(1.0, 2.0, 3.0),
      y = c(10.0, 20.0, 30.0),
      z = c(100.0, 200.0, 300.0)
    )
    mock_brain_meshes <- list(
      lh_inflated = list(vertices = brain_verts)
    )

    vb_mesh <- list(
      vb = matrix(c(1.0, 10.0, 100.0, 1,
                     3.0, 30.0, 300.0, 1),
                   nrow = 4, ncol = 2)
    )

    mock_3d <- data.frame(
      atlas = "test", hemi = "left", surf = "inflated",
      stringsAsFactors = FALSE
    )
    mock_3d$ggseg_3d <- list(
      data.frame(label = "lh_motor", region = "motor",
                 stringsAsFactors = FALSE)
    )
    mock_3d$ggseg_3d[[1]]$mesh <- list(vb_mesh)

    result <- infer_vertices_from_meshes(
      mock_3d, surface = "inflated", brain_meshes = mock_brain_meshes
    )

    expect_type(result, "list")
    expect_true("lh_motor" %in% names(result))
    expect_equal(sort(result[["lh_motor"]]), c(0L, 2L))
  })

  it("skips NULL mesh entries", {
    brain_verts <- data.frame(
      x = c(1.0, 2.0, 3.0),
      y = c(10.0, 20.0, 30.0),
      z = c(100.0, 200.0, 300.0)
    )
    mock_brain_meshes <- list(
      lh_inflated = list(vertices = brain_verts)
    )

    mock_3d <- data.frame(
      atlas = "test", hemi = "left", surf = "inflated",
      stringsAsFactors = FALSE
    )
    mock_3d$ggseg_3d <- list(
      data.frame(label = c("lh_motor", "lh_visual"), region = c("motor", "visual"),
                 stringsAsFactors = FALSE)
    )
    region_mesh <- list(
      vertices = data.frame(x = 1.0, y = 10.0, z = 100.0)
    )
    mock_3d$ggseg_3d[[1]]$mesh <- list(region_mesh, NULL)

    result <- infer_vertices_from_meshes(
      mock_3d, surface = "inflated", brain_meshes = mock_brain_meshes
    )

    expect_type(result, "list")
    expect_true("lh_motor" %in% names(result))
    expect_false("lh_visual" %in% names(result))
  })

  it("skips mesh entries with neither vb nor vertices", {
    brain_verts <- data.frame(
      x = c(1.0, 2.0),
      y = c(10.0, 20.0),
      z = c(100.0, 200.0)
    )
    mock_brain_meshes <- list(
      lh_inflated = list(vertices = brain_verts)
    )

    mock_3d <- data.frame(
      atlas = "test", hemi = "left", surf = "inflated",
      stringsAsFactors = FALSE
    )
    mock_3d$ggseg_3d <- list(
      data.frame(label = "lh_motor", region = "motor",
                 stringsAsFactors = FALSE)
    )
    mock_3d$ggseg_3d[[1]]$mesh <- list(list(something = "else"))

    result <- infer_vertices_from_meshes(
      mock_3d, surface = "inflated", brain_meshes = mock_brain_meshes
    )

    expect_null(result)
  })

  it("omits labels with no matching vertices", {
    brain_verts <- data.frame(
      x = c(1.0, 2.0, 3.0),
      y = c(10.0, 20.0, 30.0),
      z = c(100.0, 200.0, 300.0)
    )
    mock_brain_meshes <- list(
      lh_inflated = list(vertices = brain_verts)
    )

    mock_3d <- data.frame(
      atlas = "test", hemi = "left", surf = "inflated",
      stringsAsFactors = FALSE
    )
    region_mesh <- list(
      vertices = data.frame(x = 999, y = 999, z = 999)
    )
    mock_3d$ggseg_3d <- list(
      data.frame(label = "lh_nowhere", region = "nowhere",
                 stringsAsFactors = FALSE)
    )
    mock_3d$ggseg_3d[[1]]$mesh <- list(region_mesh)

    result <- infer_vertices_from_meshes(
      mock_3d, surface = "inflated", brain_meshes = mock_brain_meshes
    )

    expect_null(result)
  })
})
