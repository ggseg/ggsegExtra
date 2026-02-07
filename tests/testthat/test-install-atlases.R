describe("ggseg_atlas_repos", {
  it("returns tibble from r-universe", {
    skip_if_offline()

    result <- ggseg_atlas_repos()

    expect_s3_class(result, "tbl_df")
    expect_true(nrow(result) > 0)
    expect_true("Package" %in% names(result))
  })

  it("filters by pattern", {
    skip_if_offline()

    result <- ggseg_atlas_repos(pattern = "ggseg$")

    expect_s3_class(result, "tbl_df")
    if (nrow(result) > 0) {
      expect_true(all(grepl("ggseg", result$Package)))
    }
  })
})


describe("install_ggseg_atlas", {
  it("calls install.packages with correct repos", {
    install_calls <- NULL

    local_mocked_bindings(
      install.packages = function(pkgs, repos, ...) {
        install_calls <<- list(pkgs = pkgs, repos = repos)
        invisible(NULL)
      }
    )

    install_ggseg_atlas("ggsegTest")

    expect_equal(install_calls$pkgs, "ggsegTest")
    expect_true("ggseg" %in% names(install_calls$repos))
    expect_match(install_calls$repos["ggseg"], "r-universe")
  })

  it("passes additional arguments", {
    install_calls <- NULL

    local_mocked_bindings(
      install.packages = function(pkgs, repos, ...) {
        args <- list(...)
        install_calls <<- list(pkgs = pkgs, repos = repos, args = args)
        invisible(NULL)
      }
    )

    install_ggseg_atlas("ggsegTest", dependencies = TRUE)

    expect_true("dependencies" %in% names(install_calls$args))
    expect_true(install_calls$args$dependencies)
  })
})


describe("install_ggseg_atlas_all", {
  it("installs all packages from r-universe", {
    install_calls <- NULL

    local_mocked_bindings(
      ggseg_atlas_repos = function(...) {
        data.frame(Package = c("pkg1", "pkg2", "pkg3"), stringsAsFactors = FALSE)
      },
      install.packages = function(pkgs, repos, ...) {
        install_calls <<- list(pkgs = pkgs, repos = repos)
        invisible(NULL)
      }
    )

    install_ggseg_atlas_all()

    expect_equal(install_calls$pkgs, c("pkg1", "pkg2", "pkg3"))
    expect_true("ggseg" %in% names(install_calls$repos))
  })
})
