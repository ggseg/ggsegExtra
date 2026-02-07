describe("extract_contours", {
  it("extracts contours from mask directory", {
    skip_if_no_imagemagick()
    skip("Requires mask images - tested via integration tests")
  })
})


describe("filter_valid_geometries", {
  it("removes empty geometries", {
    sf_obj <- sf::st_sf(
      id = c("a", "b"),
      geometry = sf::st_sfc(
        sf::st_polygon(list(matrix(c(0, 0, 1, 0, 1, 1, 0, 0), ncol = 2, byrow = TRUE))),
        sf::st_polygon()
      )
    )

    result <- filter_valid_geometries(sf_obj)

    expect_equal(nrow(result), 1)
    expect_equal(result$id, "a")
  })

  it("returns empty sf for all-invalid input", {
    sf_obj <- sf::st_sf(
      id = "a",
      geometry = sf::st_sfc(sf::st_polygon())
    )

    result <- filter_valid_geometries(sf_obj)

    expect_equal(nrow(result), 0)
  })

  it("handles already empty sf object", {
    sf_obj <- sf::st_sf(
      id = character(0),
      geometry = sf::st_sfc()
    )

    result <- filter_valid_geometries(sf_obj)

    expect_equal(nrow(result), 0)
  })

  it("preserves valid geometries", {
    sf_obj <- sf::st_sf(
      id = c("a", "b"),
      geometry = sf::st_sfc(
        sf::st_polygon(list(matrix(c(0, 0, 1, 0, 1, 1, 0, 0), ncol = 2, byrow = TRUE))),
        sf::st_polygon(list(matrix(c(2, 0, 3, 0, 3, 1, 2, 0), ncol = 2, byrow = TRUE)))
      )
    )

    result <- filter_valid_geometries(sf_obj)

    expect_equal(nrow(result), 2)
    expect_equal(result$id, c("a", "b"))
  })
})


describe("smooth_contours", {
  it("smooths contour geometry", {
    outdir <- withr::local_tempdir("smooth_test_")

    contours <- sf::st_sf(
      region = c("test1", "test2"),
      geometry = sf::st_sfc(
        sf::st_polygon(list(matrix(
          c(0, 0, 1, 0, 1, 1, 0, 1, 0, 0),
          ncol = 2,
          byrow = TRUE
        ))),
        sf::st_polygon(list(matrix(
          c(2, 0, 3, 0, 3, 1, 2, 1, 2, 0),
          ncol = 2,
          byrow = TRUE
        )))
      )
    )
    save(contours, file = file.path(outdir, "contours.rda"))

    result <- smooth_contours(outdir, smoothness = 5, step = "")

    expect_s3_class(result, "sf")
    expect_true(file.exists(file.path(outdir, "contours_smoothed.rda")))
  })
})


describe("reduce_vertex", {
  it("simplifies contour geometry", {
    outdir <- withr::local_tempdir("reduce_test_")

    contours <- sf::st_sf(
      region = "test",
      geometry = sf::st_sfc(
        sf::st_polygon(list(matrix(
          c(
            0,
            0,
            0.1,
            0.01,
            0.2,
            0,
            1,
            0,
            1,
            1,
            0,
            1,
            0,
            0
          ),
          ncol = 2,
          byrow = TRUE
        )))
      )
    )
    save(contours, file = file.path(outdir, "contours_smoothed.rda"))

    result <- reduce_vertex(outdir, tolerance = 0.5, step = "")

    expect_s3_class(result, "sf")
    expect_true(file.exists(file.path(outdir, "contours_reduced.rda")))
  })
})


describe("make_multipolygon", {
  it("combines contours into multipolygons", {
    outdir <- withr::local_tempdir("multipoly_test_")

    contours <- sf::st_sf(
      filenm = c("region1", "region1", "region2"),
      geometry = sf::st_sfc(
        sf::st_polygon(list(matrix(
          c(0, 0, 1, 0, 1, 1, 0, 1, 0, 0),
          ncol = 2,
          byrow = TRUE
        ))),
        sf::st_polygon(list(matrix(
          c(2, 0, 3, 0, 3, 1, 2, 1, 2, 0),
          ncol = 2,
          byrow = TRUE
        ))),
        sf::st_polygon(list(matrix(
          c(4, 0, 5, 0, 5, 1, 4, 1, 4, 0),
          ncol = 2,
          byrow = TRUE
        )))
      )
    )
    contourfile <- file.path(outdir, "contours_reduced.rda")
    save(contours, file = contourfile)

    result <- make_multipolygon(contourfile)

    expect_s3_class(result, "sf")
    expect_equal(nrow(result), 2)
    expect_equal(result$filenm, c("region1", "region2"))
  })
})
