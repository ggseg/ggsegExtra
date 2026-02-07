describe("generate_tube_mesh", {
  it("creates valid mesh from centerline", {
    centerline <- matrix(
      c(
        0,
        0,
        0,
        1,
        0,
        0,
        2,
        0.5,
        0,
        3,
        1,
        0,
        4,
        1,
        0.5
      ),
      ncol = 3,
      byrow = TRUE
    )
    colnames(centerline) <- c("x", "y", "z")

    mesh <- generate_tube_mesh(centerline, radius = 0.5, segments = 8)

    expect_true(is.list(mesh))
    expect_true(all(c("vertices", "faces") %in% names(mesh)))
    expect_true(nrow(mesh$vertices) > 0)
    expect_true(nrow(mesh$faces) > 0)
  })

  it("creates correct number of vertices", {
    n_points <- 10
    segments <- 8
    centerline <- matrix(
      c(seq_len(n_points), rep(0, n_points * 2)),
      ncol = 3
    )
    colnames(centerline) <- c("x", "y", "z")

    mesh <- generate_tube_mesh(centerline, radius = 1, segments = segments)

    expected_vertices <- n_points * segments
    expect_equal(nrow(mesh$vertices), expected_vertices)
  })

  it("creates correct number of faces", {
    n_points <- 10
    segments <- 8
    centerline <- matrix(
      c(seq_len(n_points), rep(0, n_points * 2)),
      ncol = 3
    )
    colnames(centerline) <- c("x", "y", "z")

    mesh <- generate_tube_mesh(centerline, radius = 1, segments = segments)

    expected_faces <- (n_points - 1) * segments * 2
    expect_equal(nrow(mesh$faces), expected_faces)
  })

  it("respects radius parameter", {
    centerline <- matrix(c(0, 0, 0, 10, 0, 0), ncol = 3, byrow = TRUE)
    colnames(centerline) <- c("x", "y", "z")

    mesh_small <- generate_tube_mesh(centerline, radius = 0.1, segments = 8)
    mesh_large <- generate_tube_mesh(centerline, radius = 1.0, segments = 8)

    small_range <- max(mesh_small$vertices$y) - min(mesh_small$vertices$y)
    large_range <- max(mesh_large$vertices$y) - min(mesh_large$vertices$y)

    expect_true(large_range > small_range * 5)
  })

  it("errors with too few points", {
    centerline <- matrix(c(0, 0, 0), ncol = 3)
    colnames(centerline) <- c("x", "y", "z")

    expect_error(generate_tube_mesh(centerline), "at least 2 rows")
  })
})


describe("extract_centerline", {
  it("returns input for single streamline", {
    streamline <- matrix(c(1:10, rep(0, 20)), ncol = 3)
    colnames(streamline) <- c("x", "y", "z")

    centerline <- extract_centerline(streamline, n_points = 5)

    expect_equal(nrow(centerline), 5)
    expect_true(all(c("x", "y", "z") %in% colnames(centerline)))
  })

  it("computes mean centerline from multiple streamlines", {
    streamlines <- list(
      matrix(c(1:5, rep(0, 10)), ncol = 3),
      matrix(c(1:5, rep(1, 10)), ncol = 3),
      matrix(c(1:5, rep(2, 10)), ncol = 3)
    )

    centerline <- extract_centerline(streamlines, method = "mean", n_points = 5)

    expect_equal(nrow(centerline), 5)
    expect_equal(mean(centerline[, 2]), 1, tolerance = 0.5)
  })

  it("resamples to specified number of points", {
    streamline <- matrix(c(1:100, rep(0, 200)), ncol = 3)
    colnames(streamline) <- c("x", "y", "z")

    centerline <- extract_centerline(streamline, n_points = 20)

    expect_equal(nrow(centerline), 20)
  })

  it("returns NULL for empty input", {
    result <- extract_centerline(list())
    expect_null(result)

    result <- extract_centerline(NULL)
    expect_null(result)
  })
})


describe("compute_parallel_transport_frames", {
  it("returns orthonormal frames", {
    curve <- matrix(
      c(
        0,
        0,
        0,
        1,
        0,
        0,
        2,
        0.5,
        0,
        3,
        1,
        0
      ),
      ncol = 3,
      byrow = TRUE
    )

    frames <- compute_parallel_transport_frames(curve)

    expect_true(all(c("tangents", "normals", "binormals") %in% names(frames)))
    expect_equal(nrow(frames$tangents), nrow(curve))
    expect_equal(nrow(frames$normals), nrow(curve))
    expect_equal(nrow(frames$binormals), nrow(curve))

    for (i in seq_len(nrow(curve))) {
      t <- frames$tangents[i, ]
      n <- frames$normals[i, ]
      b <- frames$binormals[i, ]

      expect_equal(sqrt(sum(n^2)), 1, tolerance = 1e-6)
      expect_equal(sqrt(sum(b^2)), 1, tolerance = 1e-6)
      expect_equal(sum(n * b), 0, tolerance = 1e-6)
    }
  })
})


describe("create_tract_atlas", {
  it("creates atlas from coordinate matrices", {
    tracts <- list(
      cst_left = matrix(c(1:20, rep(0, 40)), ncol = 3),
      cst_right = matrix(c(1:20, rep(1, 40)), ncol = 3)
    )

    atlas <- create_tract_atlas(
      input_tracts = tracts,
      steps = 1,
      verbose = FALSE
    )

    expect_s3_class(atlas, "brain_atlas")
    expect_equal(atlas$type, "tract")
    expect_false(is.null(atlas$data$meshes))
    expect_equal(nrow(atlas$data$meshes), 2)
  })

  it("assigns correct labels", {
    tracts <- list(
      cst_left = matrix(c(1:20, rep(0, 40)), ncol = 3),
      cst_right = matrix(c(1:20, rep(1, 40)), ncol = 3)
    )

    atlas <- create_tract_atlas(
      input_tracts = tracts,
      steps = 1,
      verbose = FALSE
    )

    expect_true("cst_left" %in% atlas$core$label)
    expect_true("cst_right" %in% atlas$core$label)
  })

  it("accepts custom tract names", {
    tracts <- list(
      matrix(c(1:20, rep(0, 40)), ncol = 3),
      matrix(c(1:20, rep(1, 40)), ncol = 3)
    )

    atlas <- create_tract_atlas(
      input_tracts = tracts,
      tract_names = c("Tract A", "Tract B"),
      steps = 1,
      verbose = FALSE
    )

    expect_true("Tract A" %in% atlas$core$label)
    expect_true("Tract B" %in% atlas$core$label)
  })

  it("accepts custom colours", {
    tracts <- list(
      cst_left = matrix(c(1:20, rep(0, 40)), ncol = 3),
      cst_right = matrix(c(1:20, rep(1, 40)), ncol = 3)
    )

    atlas <- create_tract_atlas(
      input_tracts = tracts,
      colours = c("#FF0000", "#00FF00"),
      steps = 1,
      verbose = FALSE
    )

    expect_true("#FF0000" %in% atlas$palette)
    expect_true("#00FF00" %in% atlas$palette)
  })

  it("creates valid mesh structure for each tract", {
    tracts <- list(
      tract1 = matrix(c(1:20, rep(0, 40)), ncol = 3)
    )

    atlas <- create_tract_atlas(
      input_tracts = tracts,
      steps = 1,
      verbose = FALSE
    )

    mesh <- atlas$data$meshes$mesh[[1]]
    expect_true(all(c("vertices", "faces") %in% names(mesh)))
    expect_true(all(c("x", "y", "z") %in% names(mesh$vertices)))
    expect_true(all(c("i", "j", "k") %in% names(mesh$faces)))
  })

  it("respects tube parameters", {
    tract <- list(
      tract1 = matrix(c(1:10, rep(0, 20)), ncol = 3)
    )

    atlas_thin <- create_tract_atlas(
      tract,
      tube_radius = 0.1,
      steps = 1,
      verbose = FALSE
    )
    atlas_thick <- create_tract_atlas(
      tract,
      tube_radius = 1.0,
      steps = 1,
      verbose = FALSE
    )

    thin_range <- max(atlas_thin$data$meshes$mesh[[1]]$vertices$y) -
      min(atlas_thin$data$meshes$mesh[[1]]$vertices$y)
    thick_range <- max(atlas_thick$data$meshes$mesh[[1]]$vertices$y) -
      min(atlas_thick$data$meshes$mesh[[1]]$vertices$y)

    expect_true(thick_range > thin_range * 5)
  })

  it("can render with ggseg3d", {
    tracts <- list(
      cst_left = matrix(c(1:20, rep(0, 40)), ncol = 3),
      cst_right = matrix(c(1:20, rep(1, 40)), ncol = 3)
    )

    atlas <- create_tract_atlas(
      input_tracts = tracts,
      steps = 1,
      verbose = FALSE
    )

    expect_no_error({
      p <- ggseg3d::ggseg3d(atlas = atlas, hemisphere = "subcort")
    })
  })
})


describe("resample_streamline", {
  it("resamples to exact number of points", {
    streamline <- matrix(c(1:100, rep(0, 200)), ncol = 3)
    colnames(streamline) <- c("x", "y", "z")

    resampled <- resample_streamline(streamline, 20)

    expect_equal(nrow(resampled), 20)
  })

  it("preserves endpoints", {
    streamline <- matrix(c(0, 0, 0, 10, 0, 0), ncol = 3, byrow = TRUE)
    colnames(streamline) <- c("x", "y", "z")

    resampled <- resample_streamline(streamline, 5)

    expect_equal(as.numeric(resampled[1, 1]), 0, tolerance = 1e-6)
    expect_equal(as.numeric(resampled[5, 1]), 10, tolerance = 1e-6)
  })

  it("returns NULL for invalid input", {
    expect_null(resample_streamline(matrix(ncol = 3, nrow = 1), 10))
    expect_null(resample_streamline(NULL, 10))
  })
})


describe("cross_product", {
  it("computes correct cross product", {
    a <- c(1, 0, 0)
    b <- c(0, 1, 0)

    result <- cross_product(a, b)

    expect_equal(result, c(0, 0, 1))
  })

  it("returns zero for parallel vectors", {
    a <- c(1, 0, 0)
    b <- c(2, 0, 0)

    result <- cross_product(a, b)

    expect_equal(result, c(0, 0, 0))
  })

  it("is anti-commutative", {
    a <- c(1, 2, 3)
    b <- c(4, 5, 6)

    result1 <- cross_product(a, b)
    result2 <- cross_product(b, a)

    expect_equal(result1, -result2)
  })
})


describe("rotate_vector", {
  it("rotates vector 90 degrees around z-axis", {
    v <- c(1, 0, 0)
    axis <- c(0, 0, 1)
    angle <- pi / 2

    result <- rotate_vector(v, axis, angle)

    expect_equal(result[1], 0, tolerance = 1e-6)
    expect_equal(result[2], 1, tolerance = 1e-6)
    expect_equal(result[3], 0, tolerance = 1e-6)
  })

  it("does not change vector when angle is 0", {
    v <- c(1, 2, 3)
    axis <- c(0, 0, 1)

    result <- rotate_vector(v, axis, 0)

    expect_equal(result, v, tolerance = 1e-6)
  })

  it("returns original vector when rotating around itself", {
    v <- c(0, 0, 1)
    axis <- c(0, 0, 1)

    result <- rotate_vector(v, axis, pi / 4)

    expect_equal(result, v, tolerance = 1e-6)
  })
})


describe("resolve_tube_radius", {
  it("returns uniform radius when given scalar", {
    centerline <- matrix(c(1:10, rep(0, 20)), ncol = 3)
    streamlines <- list(centerline)

    result <- resolve_tube_radius(0.5, streamlines, centerline, c(0.2, 1.0))

    expect_length(result, 10)
    expect_true(all(result == 0.5))
  })

  it("returns input when given vector of correct length", {
    centerline <- matrix(c(1:5, rep(0, 10)), ncol = 3)
    streamlines <- list(centerline)
    radii <- c(0.1, 0.2, 0.3, 0.4, 0.5)

    result <- resolve_tube_radius(radii, streamlines, centerline, c(0.2, 1.0))

    expect_equal(result, radii)
  })

  it("errors when vector has wrong length", {
    centerline <- matrix(c(1:10, rep(0, 20)), ncol = 3)
    streamlines <- list(centerline)

    expect_error(
      resolve_tube_radius(c(0.1, 0.2), streamlines, centerline, c(0.2, 1.0)),
      "length 1 or 10"
    )
  })
})


describe("compute_streamline_density", {
  it("computes density at each centerline point", {
    streamlines <- list(
      matrix(c(1:5, rep(0, 10)), ncol = 3),
      matrix(c(1:5, rep(0.5, 10)), ncol = 3),
      matrix(c(1:5, rep(1, 10)), ncol = 3)
    )
    centerline <- matrix(c(1:5, rep(0.5, 10)), ncol = 3)

    result <- compute_streamline_density(streamlines, centerline, search_radius = 1)

    expect_length(result, 5)
    expect_true(all(result >= 0))
  })

  it("returns zeros when no streamlines nearby", {
    streamlines <- list(
      matrix(c(100:110, rep(100, 22)), ncol = 3)
    )
    centerline <- matrix(c(1:5, rep(0, 10)), ncol = 3)

    result <- compute_streamline_density(streamlines, centerline, search_radius = 1)

    expect_true(all(result == 0))
  })
})


describe("volume_projection", {
  it("creates axial projection", {
    vol <- array(0, dim = c(10, 10, 10))
    vol[5, 5, 5] <- 1

    result <- volume_projection(vol, "axial")

    expect_equal(dim(result), c(10, 10))
    expect_equal(max(result), 1)
  })

  it("creates sagittal projection", {
    vol <- array(0, dim = c(10, 10, 10))
    vol[5, 5, 5] <- 1

    result <- volume_projection(vol, "sagittal")

    expect_equal(dim(result), c(10, 10))
  })

  it("creates coronal projection", {
    vol <- array(0, dim = c(10, 10, 10))
    vol[5, 5, 5] <- 1

    result <- volume_projection(vol, "coronal")

    expect_equal(dim(result), c(10, 10))
  })
})


describe("volume_partial_projection", {
  it("projects subset of slices for axial view", {
    vol <- array(0, dim = c(10, 10, 10))
    vol[5, 5, 3:7] <- 1

    result <- volume_partial_projection(vol, "axial", start = 3, end = 7)

    expect_equal(dim(result), c(10, 10))
    expect_equal(max(result), 1)
  })

  it("respects slice range", {
    vol <- array(0, dim = c(10, 10, 10))
    vol[5, 5, 8] <- 1

    result <- volume_partial_projection(vol, "axial", start = 1, end = 5)

    expect_equal(max(result), 0)
  })
})


describe("center_meshes", {
  it("centers multiple meshes around origin", {
    meshes <- list(
      mesh1 = list(
        vertices = data.frame(x = 10:12, y = 10:12, z = 10:12),
        faces = data.frame(i = 1, j = 2, k = 3)
      ),
      mesh2 = list(
        vertices = data.frame(x = 20:22, y = 20:22, z = 20:22),
        faces = data.frame(i = 1, j = 2, k = 3)
      )
    )

    result <- center_meshes(meshes)

    all_verts <- do.call(rbind, lapply(result, function(m) m$vertices))
    centroid <- c(mean(all_verts$x), mean(all_verts$y), mean(all_verts$z))

    expect_equal(centroid[1], 0, tolerance = 1e-6)
    expect_equal(centroid[2], 0, tolerance = 1e-6)
    expect_equal(centroid[3], 0, tolerance = 1e-6)
  })

  it("preserves mesh structure", {
    meshes <- list(
      mesh1 = list(
        vertices = data.frame(x = 1:3, y = 1:3, z = 1:3),
        faces = data.frame(i = 1, j = 2, k = 3)
      )
    )

    result <- center_meshes(meshes)

    expect_true(all(c("vertices", "faces") %in% names(result$mesh1)))
    expect_equal(nrow(result$mesh1$vertices), 3)
    expect_equal(nrow(result$mesh1$faces), 1)
  })

  it("also centers metadata centerline if present", {
    meshes <- list(
      mesh1 = list(
        vertices = data.frame(x = 10:12, y = 10:12, z = 10:12),
        faces = data.frame(i = 1, j = 2, k = 3),
        metadata = list(
          centerline = matrix(c(10, 11, 12, 10, 11, 12, 10, 11, 12), ncol = 3)
        )
      )
    )

    result <- center_meshes(meshes)

    cl <- result$mesh1$metadata$centerline
    expect_true(mean(cl[, 1]) < 5)
  })
})


describe("read_tractography", {
  it("errors on unsupported format", {
    tmp <- withr::local_tempfile(fileext = ".xyz")
    writeLines("dummy", tmp)

    expect_error(read_tractography(tmp), "Unsupported tractography format")
  })
})


describe("read_trk", {
  it("errors on invalid TRK file", {
    tmp <- withr::local_tempfile(fileext = ".trk")
    writeBin(charToRaw("INVALID HEADER DATA"), tmp)

    expect_error(read_trk(tmp), "Invalid TRK")
  })
})


describe("read_tck", {
  it("reads valid TCK format with header", {
    tmp <- withr::local_tempfile(fileext = ".tck")
    con <- file(tmp, "wb")
    writeLines(c(
      "mrtrix tracks",
      "datatype: Float32LE",
      "END"
    ), con)
    writeBin(as.numeric(c(1, 2, 3)), con, size = 4)
    writeBin(as.numeric(c(NaN, NaN, NaN)), con, size = 4)
    writeBin(as.numeric(c(Inf, Inf, Inf)), con, size = 4)
    close(con)

    result <- read_tck(tmp)

    expect_type(result, "list")
  })
})


describe("default_tract_views", {
  it("creates views for standard 256 brain", {
    dims <- c(256, 256, 256)
    result <- default_tract_views(dims)

    expect_s3_class(result, "data.frame")
    expect_true(all(c("name", "type", "start", "end") %in% names(result)))
    expect_true("axial" %in% result$type)
    expect_true("coronal" %in% result$type)
    expect_true("sagittal" %in% result$type)
  })

  it("creates wider projections than subcortical", {
    dims <- c(256, 256, 256)
    tract_views <- default_tract_views(dims)
    subcort_views <- default_subcortical_views(dims)

    tract_axial <- tract_views[tract_views$type == "axial", ]
    subcort_axial <- subcort_views[subcort_views$type == "axial", ]

    tract_range <- max(tract_axial$end) - min(tract_axial$start)
    subcort_range <- max(subcort_axial$end) - min(subcort_axial$start)

    expect_gt(tract_range, subcort_range)
  })

  it("creates left and right sagittal projections", {
    dims <- c(256, 256, 256)
    result <- default_tract_views(dims)

    sagittal <- result[result$type == "sagittal", ]
    expect_equal(nrow(sagittal), 2)
    expect_true(any(grepl("left", sagittal$name)))
    expect_true(any(grepl("right", sagittal$name)))
  })

  it("sagittal projections cover full hemisphere", {
    dims <- c(256, 256, 256)
    result <- default_tract_views(dims)

    sagittal <- result[result$type == "sagittal", ]
    left <- sagittal[grepl("left", sagittal$name), ]
    right <- sagittal[grepl("right", sagittal$name), ]

    expect_equal(left$start, 128)
    expect_equal(left$end, 256)
    expect_equal(right$start, 1)
    expect_equal(right$end, 128)
  })

  it("scales for different volume sizes", {
    dims_256 <- c(256, 256, 256)
    dims_128 <- c(128, 128, 128)

    result_256 <- default_tract_views(dims_256)
    result_128 <- default_tract_views(dims_128)

    axial_256 <- result_256[result_256$type == "axial", ]
    axial_128 <- result_128[result_128$type == "axial", ]

    expect_equal(axial_128$start[1] / axial_256$start[1], 0.5)
  })
})
