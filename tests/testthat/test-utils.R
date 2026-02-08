describe("mkdir", {
  it("creates directory", {
    tmp <- withr::local_tempdir()
    new_dir <- file.path(tmp, "test_subdir")

    expect_false(dir.exists(new_dir))
    mkdir(new_dir)
    expect_true(dir.exists(new_dir))
  })

  it("creates nested directories", {
    tmp <- withr::local_tempdir()
    nested <- file.path(tmp, "a", "b", "c")

    mkdir(nested)
    expect_true(dir.exists(nested))
  })

  it("does not error if directory exists", {
    tmp <- withr::local_tempdir()
    expect_no_error(mkdir(tmp))
  })
})


describe("get_verbose", {
  it("returns TRUE by default", {
    withr::local_options(ggsegExtra.verbose = NULL)
    withr::local_envvar(GGSEGEXTRA_VERBOSE = NA)
    expect_true(get_verbose())
  })

  it("reads from option", {
    withr::local_options(ggsegExtra.verbose = FALSE)
    expect_false(get_verbose())
  })

  it("reads from environment variable when option is NULL", {
    withr::local_options(ggsegExtra.verbose = NULL)
    withr::local_envvar(GGSEGEXTRA_VERBOSE = "false")
    expect_false(get_verbose())
  })

  it("option takes precedence over envvar", {
    withr::local_options(ggsegExtra.verbose = TRUE)
    withr::local_envvar(GGSEGEXTRA_VERBOSE = "false")
    expect_true(get_verbose())
  })
})


describe("is_verbose", {
  it("returns TRUE for truthy values", {
    expect_true(is_verbose(1))
    expect_true(is_verbose(TRUE))
  })

  it("returns FALSE for falsy values", {
    expect_false(is_verbose(0))
    expect_false(is_verbose(FALSE))
  })

  it("delegates to get_verbose when NULL", {
    withr::local_options(ggsegExtra.verbose = FALSE)
    expect_false(is_verbose())

    withr::local_options(ggsegExtra.verbose = TRUE)
    expect_true(is_verbose())
  })
})


describe("get_cleanup", {
  it("returns explicit value when provided", {
    expect_true(get_cleanup(TRUE))
    expect_false(get_cleanup(FALSE))
  })

  it("reads from option when explicit value is NULL", {
    withr::local_options(ggsegExtra.cleanup = FALSE)
    expect_false(get_cleanup())
  })

  it("reads from environment variable when option is NULL", {
    withr::local_options(ggsegExtra.cleanup = NULL)
    withr::local_envvar(GGSEGEXTRA_CLEANUP = "false")
    expect_false(get_cleanup())

    withr::local_envvar(GGSEGEXTRA_CLEANUP = "true")
    expect_true(get_cleanup())

    withr::local_envvar(GGSEGEXTRA_CLEANUP = "1")
    expect_true(get_cleanup())

    withr::local_envvar(GGSEGEXTRA_CLEANUP = "0")
    expect_false(get_cleanup())
  })

  it("returns default of TRUE when nothing is set", {
    withr::local_options(ggsegExtra.cleanup = NULL)
    withr::local_envvar(GGSEGEXTRA_CLEANUP = NA)
    expect_true(get_cleanup())
  })
})


describe("get_skip_existing", {
  it("returns explicit value when provided", {
    expect_true(get_skip_existing(TRUE))
    expect_false(get_skip_existing(FALSE))
  })

  it("reads from option when explicit value is NULL", {
    withr::local_options(ggsegExtra.skip_existing = FALSE)
    expect_false(get_skip_existing())
  })

  it("reads from environment variable when option is NULL", {
    withr::local_options(ggsegExtra.skip_existing = NULL)
    withr::local_envvar(GGSEGEXTRA_SKIP_EXISTING = "false")
    expect_false(get_skip_existing())
  })

  it("returns default of TRUE when nothing is set", {
    withr::local_options(ggsegExtra.skip_existing = NULL)
    withr::local_envvar(GGSEGEXTRA_SKIP_EXISTING = NA)
    expect_true(get_skip_existing())
  })
})


describe("get_tolerance", {
  it("returns explicit value when provided", {
    expect_equal(get_tolerance(0.5), 0.5)
    expect_equal(get_tolerance(1), 1)
  })

  it("reads from option when explicit value is NULL", {
    withr::local_options(ggsegExtra.tolerance = 0.75)
    expect_equal(get_tolerance(), 0.75)
  })

  it("reads from environment variable when option is NULL", {
    withr::local_options(ggsegExtra.tolerance = NULL)
    withr::local_envvar(GGSEGEXTRA_TOLERANCE = "0.25")
    expect_equal(get_tolerance(), 0.25)
  })

  it("returns default of 1 when nothing is set", {
    withr::local_options(ggsegExtra.tolerance = NULL)
    withr::local_envvar(GGSEGEXTRA_TOLERANCE = NA)
    expect_equal(get_tolerance(), 1)
  })
})


describe("get_smoothness", {
  it("returns explicit value when provided", {
    expect_equal(get_smoothness(10), 10)
    expect_equal(get_smoothness(2.5), 2.5)
  })

  it("reads from option when explicit value is NULL", {
    withr::local_options(ggsegExtra.smoothness = 15)
    expect_equal(get_smoothness(), 15)
  })

  it("reads from environment variable when option is NULL", {
    withr::local_options(ggsegExtra.smoothness = NULL)
    withr::local_envvar(GGSEGEXTRA_SMOOTHNESS = "20")
    expect_equal(get_smoothness(), 20)
  })

  it("returns default of 5 when nothing is set", {
    withr::local_options(ggsegExtra.smoothness = NULL)
    withr::local_envvar(GGSEGEXTRA_SMOOTHNESS = NA)
    expect_equal(get_smoothness(), 5)
  })
})


describe("load_or_run_step", {
  it("returns run=TRUE when step is requested and files don't exist", {
    result <- load_or_run_step(
      1L, 1L:3L,
      files = "/nonexistent/file.rds",
      skip_existing = FALSE,
      step_name = "Test step"
    )

    expect_true(result$run)
    expect_null(result$data)
  })

  it("loads data when files exist and skip_existing=TRUE", {
    tmp <- withr::local_tempfile(fileext = ".rds")
    saveRDS(list(a = 1), tmp)

    result <- load_or_run_step(
      1L, 1L:3L,
      files = tmp,
      skip_existing = TRUE,
      step_name = "Test step"
    )

    expect_false(result$run)
    expect_type(result$data, "list")
  })

  it("errors when step not requested and files missing", {
    expect_error(
      load_or_run_step(
        1L, 2L:3L,
        files = "/nonexistent/file.rds",
        skip_existing = FALSE,
        step_name = "Test step"
      ),
      "missing"
    )
  })
})


describe("warn_if_large_atlas", {
  it("warns when atlas has many vertices", {
    coords <- matrix(runif(200), ncol = 2)
    coords <- rbind(coords, coords[1, ])
    sf_obj <- sf::st_sf(
      label = "test",
      geometry = sf::st_sfc(sf::st_polygon(list(coords)))
    )
    atlas <- list(data = list(sf = sf_obj))

    expect_warning(
      warn_if_large_atlas(atlas, max_vertices = 5),
      "vertices"
    )
  })

  it("does not warn when atlas is small", {
    sf_obj <- sf::st_sf(
      label = "test",
      geometry = sf::st_sfc(
        sf::st_polygon(list(matrix(
          c(0, 0, 1, 0, 1, 1, 0, 0),
          ncol = 2,
          byrow = TRUE
        )))
      )
    )
    atlas <- list(data = list(sf = sf_obj))

    expect_no_warning(warn_if_large_atlas(atlas, max_vertices = 10000))
  })

  it("does nothing when atlas has no sf data", {
    atlas <- list(data = list(sf = NULL))
    expect_no_warning(warn_if_large_atlas(atlas))
  })
})
