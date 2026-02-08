describe("generate_tube_mesh", {
  it("creates valid mesh from centerline", {
    centerline <- matrix(
      c(
        0, 0, 0,
        1, 0, 0,
        2, 0.5, 0,
        3, 1, 0,
        4, 1, 0.5
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
        0, 0, 0,
        1, 0, 0,
        2, 0.5, 0,
        3, 1, 0
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


describe("compute_streamline_density", {
  it("computes density at each centerline point", {
    streamlines <- list(
      matrix(c(1:5, rep(0, 10)), ncol = 3),
      matrix(c(1:5, rep(0.5, 10)), ncol = 3),
      matrix(c(1:5, rep(1, 10)), ncol = 3)
    )
    centerline <- matrix(c(1:5, rep(0.5, 10)), ncol = 3)

    result <- compute_streamline_density(
      streamlines, centerline, search_radius = 1
    )

    expect_length(result, 5)
    expect_true(all(result >= 0))
  })

  it("returns zeros when no streamlines nearby", {
    streamlines <- list(
      matrix(c(100:110, rep(100, 22)), ncol = 3)
    )
    centerline <- matrix(c(1:5, rep(0, 10)), ncol = 3)

    result <- compute_streamline_density(
      streamlines, centerline, search_radius = 1
    )

    expect_true(all(result == 0))
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


describe("detect_coords_are_voxels", {
  it("detects positive-only coordinates as voxel space", {
    streamlines <- list(
      matrix(c(50:60, rep(70, 11), rep(80, 11)), ncol = 3)
    )

    expect_true(detect_coords_are_voxels(streamlines))
  })

  it("detects negative coordinates as world space", {
    streamlines <- list(
      matrix(c(-10:0, rep(-5, 11), rep(3, 11)), ncol = 3)
    )

    expect_false(detect_coords_are_voxels(streamlines))
  })
})
