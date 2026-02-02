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
  tmp <- NULL

  setup({
    tmp <<- withr::local_tempdir("atlas_template_test_", .local_envir = parent.frame())
    create_atlas_repo(tmp, "testatlas", open = FALSE)
  })

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
    r_cmd_check <- readLines(file.path(tmp, ".github/workflows/R-CMD-check.yaml"))
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


describe("template_replace", {
  it("replaces both GGSEG and REPO placeholders", {
    tmp <- withr::local_tempfile(fileext = ".txt")
    writeLines(c(
      "Package: {REPO}",
      "Atlas: {GGSEG}",
      "URL: https://github.com/ggseg/{REPO}"
    ), tmp)

    ggsegExtra:::template_replace(tmp, "myatlas")

    result <- readLines(tmp)
    expect_equal(result[1], "Package: ggsegMyatlas")
    expect_equal(result[2], "Atlas: myatlas")
    expect_equal(result[3], "URL: https://github.com/ggseg/ggsegMyatlas")
  })

  it("handles files without placeholders", {
    tmp <- withr::local_tempfile(fileext = ".txt")
    writeLines("No placeholders here", tmp)

    expect_no_error(ggsegExtra:::template_replace(tmp, "test"))

    result <- readLines(tmp)
    expect_equal(result, "No placeholders here")
  })
})
