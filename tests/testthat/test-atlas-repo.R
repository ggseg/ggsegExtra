describe("create_atlas_repo", {
  it("creates package with explicit atlas name", {
    tmp <- withr::local_tempdir("atlas_test_")

    result <- create_atlas_repo(tmp, "dkt", open = FALSE)

    expect_true(dir.exists(result))
    expect_true(dir.exists(tmp))
    expect_true(file.exists(file.path(tmp, "DESCRIPTION")))
  })

  it("derives atlas name from ggsegXxx path format", {
    parent <- withr::local_tempdir()
    tmp <- file.path(parent, paste0("ggsegSchaefer", Sys.getpid()))

    create_atlas_repo(tmp, open = FALSE)

    desc <- readLines(file.path(tmp, "DESCRIPTION"))
    pkg_line <- desc[grep("^Package:", desc)]

    expect_match(pkg_line, "ggsegSchaefer")
  })

  it("derives atlas name from plain directory name", {
    parent <- withr::local_tempdir()
    tmp <- file.path(parent, paste0("myatlas", Sys.getpid()))

    create_atlas_repo(tmp, open = FALSE)

    desc <- readLines(file.path(tmp, "DESCRIPTION"))
    pkg_line <- desc[grep("^Package:", desc)]

    expect_match(pkg_line, "ggsegMyatlas")
  })

  it("cleans atlas name (lowercase, alphanumeric only)", {
    tmp <- withr::local_tempdir("atlas_test_")

    create_atlas_repo(tmp, "My-Atlas_123!", open = FALSE)

    desc <- readLines(file.path(tmp, "DESCRIPTION"))
    pkg_line <- desc[grep("^Package:", desc)]

    expect_match(pkg_line, "ggsegMyatlas123")
  })

  it("errors on empty atlas name", {
    tmp <- withr::local_tempdir("atlas_test_")

    expect_error(
      create_atlas_repo(file.path(tmp, "newpkg"), "---", open = FALSE),
      "must contain at least one letter"
    )
  })

  it("errors on non-empty directory", {
    tmp <- withr::local_tempdir("atlas_test_")
    writeLines("test", file.path(tmp, "existing.txt"))

    expect_error(
      create_atlas_repo(tmp, "test", open = FALSE),
      "not empty"
    )
  })

  it("creates .Rproj file when rstudio = TRUE", {
    tmp <- withr::local_tempdir("atlas_test_")

    create_atlas_repo(tmp, "test", open = FALSE, rstudio = TRUE)

    rproj_files <- list.files(tmp, pattern = "\\.Rproj$")
    expect_length(rproj_files, 1)
    expect_equal(rproj_files, "ggsegTest.Rproj")
  })

  it("skips .Rproj file when rstudio = FALSE", {
    tmp <- withr::local_tempdir("atlas_test_")

    create_atlas_repo(tmp, "test", open = FALSE, rstudio = FALSE)

    rproj_files <- list.files(tmp, pattern = "\\.Rproj$")
    expect_length(rproj_files, 0)
  })
})


describe("create_atlas_repo template files", {
  tmp <- withr::local_tempdir("atlas_template_test_")
  create_atlas_repo(tmp, "testatlas", open = FALSE)

  it("creates all required directories", {
    expect_true(dir.exists(file.path(tmp, "R")))
    expect_true(dir.exists(file.path(tmp, "data-raw")))
    expect_true(dir.exists(file.path(tmp, "tests", "testthat")))
    expect_true(dir.exists(file.path(tmp, ".github", "workflows")))
  })

  it("creates all required files", {
    expected_files <- c(
      "DESCRIPTION",
      "NAMESPACE",
      "LICENSE",
      "LICENSE.md",
      "README.Rmd",
      "_pkgdown.yml",
      ".gitignore",
      ".Rbuildignore",
      "R/data.R",
      "data-raw/create-atlas.R",
      "data-raw/create-hex-logo.R",
      "tests/testthat.R",
      "tests/testthat/test-atlas.R",
      ".github/workflows/R-CMD-check.yaml",
      ".github/workflows/pkgdown.yaml"
    )

    for (f in expected_files) {
      expect_true(
        file.exists(file.path(tmp, f)),
        info = paste("Missing file:", f)
      )
    }
  })

  it("replaces {GGSEG} placeholder with atlas name", {
    data_r <- readLines(file.path(tmp, "R/data.R"))

    expect_false(any(grepl("\\{GGSEG\\}", data_r)))
    expect_true(any(grepl("testatlas", data_r)))
  })

  it("replaces {REPO} placeholder with package name", {
    desc <- readLines(file.path(tmp, "DESCRIPTION"))

    expect_false(any(grepl("\\{REPO\\}", desc)))
    expect_true(any(grepl("ggsegTestatlas", desc)))
  })

  it("creates valid DESCRIPTION file", {
    desc <- readLines(file.path(tmp, "DESCRIPTION"))

    expect_true(any(grepl("^Package: ggsegTestatlas", desc)))
    expect_true(any(grepl("^License: MIT", desc)))
    expect_true(any(grepl("ggseg", desc)))
    expect_true(any(grepl("ggseg3d", desc)))
  })

  it("creates valid test file", {
    test_file <- readLines(file.path(tmp, "tests/testthat/test-atlas.R"))

    expect_true(any(grepl("describe.*testatlas", test_file)))
    expect_true(any(grepl("brain_atlas", test_file)))
    expect_true(any(grepl("vertices", test_file)))
  })

  it("creates GitHub Actions workflows", {
    r_cmd_check <- readLines(file.path(
      tmp,
      ".github/workflows/R-CMD-check.yaml"
    ))
    pkgdown <- readLines(file.path(tmp, ".github/workflows/pkgdown.yaml"))

    expect_true(any(grepl("R-CMD-check", r_cmd_check)))
    expect_true(any(grepl("rcmdcheck", r_cmd_check)))
    expect_true(any(grepl("pkgdown", pkgdown)))
  })

  it("creates data-raw scripts with correct atlas name", {
    create_script <- readLines(file.path(tmp, "data-raw/create-atlas.R"))
    hex_script <- readLines(file.path(tmp, "data-raw/create-hex-logo.R"))

    expect_true(any(grepl("testatlas", create_script)))
    expect_true(any(grepl("testatlas", hex_script)))
    expect_false(any(grepl("\\{GGSEG\\}", create_script)))
  })

  it("creates README with correct package references", {
    readme <- readLines(file.path(tmp, "README.Rmd"))

    expect_true(any(grepl("ggsegTestatlas", readme)))
    expect_true(any(grepl("testatlas", readme)))
    expect_true(any(grepl("Citation", readme)))
    expect_false(any(grepl("\\{REPO\\}", readme)))
  })
})


describe("create_atlas_repo .Rproj file", {
  it("contains correct package build settings", {
    tmp <- withr::local_tempdir("atlas_rproj_test_")

    create_atlas_repo(tmp, "test", open = FALSE)

    rproj <- readLines(file.path(tmp, "ggsegTest.Rproj"))

    expect_true(any(grepl("BuildType: Package", rproj)))
    expect_true(any(grepl("PackageUseDevtools: Yes", rproj)))
    expect_true(any(grepl("PackageRoxygenize:", rproj)))
  })
})


describe("create_atlas_repo lowercase ggseg prefix", {
  it("derives atlas name from lowercase ggseg prefix path", {
    parent <- withr::local_tempdir()
    tmp <- file.path(parent, paste0("ggsegfoo", Sys.getpid()))

    create_atlas_repo(tmp, open = FALSE)

    desc <- readLines(file.path(tmp, "DESCRIPTION"))
    pkg_line <- desc[grep("^Package:", desc)]

    expect_match(pkg_line, "ggsegFoo")
  })

  it("calls open_rstudio_project when open and rstudio are TRUE", {
    tmp <- withr::local_tempdir("atlas_open_test_")
    opened <- FALSE

    local_mocked_bindings(
      open_rstudio_project = function(path) {
        opened <<- TRUE
        invisible(TRUE)
      }
    )

    create_atlas_repo(tmp, "test", open = TRUE, rstudio = TRUE)

    expect_true(opened)
  })
})


describe("create_atlas_from_template", {
  it("errors when template directory does not exist", {
    local_mocked_bindings(
      dir.exists = function(...) FALSE,
      .package = "base"
    )

    expect_error(
      create_atlas_from_template("/tmp/nonexistent", "test"),
      "Template not found"
    )
  })
})


describe("open_rstudio_project", {
  it("returns FALSE when rstudioapi not available", {
    local_mocked_bindings(
      rstudioapi_available = function() FALSE
    )

    result <- open_rstudio_project("/tmp/some_path")

    expect_false(result)
  })

  it("returns FALSE when no .Rproj files found", {
    tmp <- withr::local_tempdir("rproj_test_")

    local_mocked_bindings(
      rstudioapi_available = function() TRUE
    )

    result <- open_rstudio_project(tmp)

    expect_false(result)
  })

  it("opens project when rstudioapi available and Rproj exists", {
    tmp <- withr::local_tempdir("rproj_test_")
    writeLines("Version: 1.0", file.path(tmp, "test.Rproj"))
    opened_path <- NULL

    local_mocked_bindings(
      rstudioapi_available = function() TRUE
    )
    local_mocked_bindings(
      isAvailable = function() TRUE,
      openProject = function(path, ...) {
        opened_path <<- path
        invisible(NULL)
      },
      .package = "rstudioapi"
    )

    result <- open_rstudio_project(tmp)

    expect_true(result)
    expect_equal(opened_path, file.path(tmp, "test.Rproj"))
  })
})


describe("rstudioapi_available", {
  it("returns FALSE when rstudioapi is not installed", {
    local_mocked_bindings(
      requireNamespace = function(pkg, ...) FALSE,
      .package = "base"
    )

    expect_false(rstudioapi_available())
  })
})


describe("template_replace error handling", {
  it("returns NULL for unreadable files", {
    result <- suppressWarnings(
      template_replace("/nonexistent/path/file.txt", "test")
    )

    expect_null(result)
  })
})


describe("new_project_create_atlas_repo", {
  it("delegates to create_atlas_repo with correct parameters", {
    tmp <- withr::local_tempdir("wizard_test_")
    called_args <- NULL

    local_mocked_bindings(
      create_atlas_repo = function(path, atlas_name, open, rstudio) {
        called_args <<- list(
          path = path,
          atlas_name = atlas_name,
          open = open,
          rstudio = rstudio
        )
        invisible(path)
      }
    )

    new_project_create_atlas_repo(tmp, atlas_name = "myatlas")

    expect_equal(called_args$path, tmp)
    expect_equal(called_args$atlas_name, "myatlas")
    expect_false(called_args$open)
    expect_true(called_args$rstudio)
  })

  it("passes NULL atlas_name when not provided", {
    tmp <- withr::local_tempdir("wizard_null_test_")
    called_args <- NULL

    local_mocked_bindings(
      create_atlas_repo = function(path, atlas_name, open, rstudio) {
        called_args <<- list(atlas_name = atlas_name)
        invisible(path)
      }
    )

    new_project_create_atlas_repo(tmp)

    expect_null(called_args$atlas_name)
  })
})


describe("template_replace", {
  it("replaces both GGSEG and REPO placeholders", {
    tmp <- withr::local_tempfile(fileext = ".txt")
    writeLines(
      c(
        "Package: {REPO}",
        "Atlas: {GGSEG}",
        "URL: https://github.com/ggseg/{REPO}"
      ),
      tmp
    )

    template_replace(tmp, "myatlas")

    result <- readLines(tmp)
    expect_equal(result[1], "Package: ggsegMyatlas")
    expect_equal(result[2], "Atlas: myatlas")
    expect_equal(result[3], "URL: https://github.com/ggseg/ggsegMyatlas")
  })

  it("handles files without placeholders", {
    tmp <- withr::local_tempfile(fileext = ".txt")
    writeLines("No placeholders here", tmp)

    expect_no_error(template_replace(tmp, "test"))

    result <- readLines(tmp)
    expect_equal(result, "No placeholders here")
  })
})
