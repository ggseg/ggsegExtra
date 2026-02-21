describe("ggseg_atlas_repos", {
  it("delegates to ggseg.hub", {
    skip_if_not_installed("ggseg.hub")

    hub_called <- FALSE
    local_mocked_bindings(
      ggseg_atlas_repos = function(...) {
        hub_called <<- TRUE
        dplyr::as_tibble(data.frame(
          Package = "test",
          stringsAsFactors = FALSE
        ))
      },
      .package = "ggseg.hub"
    )

    suppressWarnings(ggseg_atlas_repos())
    expect_true(hub_called)
  })
})


describe("install_ggseg_atlas", {
  it("delegates to ggseg.hub", {
    skip_if_not_installed("ggseg.hub")

    hub_called <- FALSE
    local_mocked_bindings(
      install_ggseg_atlas = function(...) {
        hub_called <<- TRUE
        invisible(NULL)
      },
      .package = "ggseg.hub"
    )

    suppressWarnings(install_ggseg_atlas("ggsegTest"))
    expect_true(hub_called)
  })
})


describe("install_ggseg_atlas_all", {
  it("delegates to ggseg.hub", {
    skip_if_not_installed("ggseg.hub")

    hub_called <- FALSE
    local_mocked_bindings(
      install_ggseg_atlas_all = function(...) {
        hub_called <<- TRUE
        invisible(NULL)
      },
      .package = "ggseg.hub"
    )

    suppressWarnings(install_ggseg_atlas_all())
    expect_true(hub_called)
  })
})


describe("ggseg_atlas_repos", {
  it("emits deprecation warning", {
    local_mocked_bindings(
      ggseg_atlas_repos = function(...) data.frame(),
      .package = "ggseg.hub"
    )
    lifecycle::expect_deprecated(ggseg.extra::ggseg_atlas_repos())
  })
})


describe("install_ggseg_atlas", {
  it("emits deprecation warning", {
    local_mocked_bindings(
      install_ggseg_atlas = function(...) invisible(NULL),
      .package = "ggseg.hub"
    )
    lifecycle::expect_deprecated(ggseg.extra::install_ggseg_atlas("ggsegTest"))
  })
})


describe("install_ggseg_atlas_all", {
  it("emits deprecation warning", {
    local_mocked_bindings(
      install_ggseg_atlas_all = function(...) invisible(NULL),
      .package = "ggseg.hub"
    )
    lifecycle::expect_deprecated(ggseg.extra::install_ggseg_atlas_all())
  })
})
