library(dplyr, quietly = TRUE, warn.conflicts = FALSE)
library(tidyr, quietly = TRUE, warn.conflicts = FALSE)
library(ggseg, quietly = TRUE, warn.conflicts = FALSE)
library(ggseg3d, quietly = TRUE, warn.conflicts = FALSE)

# Helper to get test data directory
testdata_dir <- function() {
  testthat::test_path("testdata")
}

# Helper to skip tests if package not installed
skip_if_not_installed <- function(pkg) {
  if (!requireNamespace(pkg, quietly = TRUE)) {
    testthat::skip(paste0("Package '", pkg, "' not installed"))
  }
}

# Helper to skip tests requiring FreeSurfer
skip_if_no_freesurfer <- function() {
  if (!freesurfer::have_fs()) {
    testthat::skip("FreeSurfer not available")
  }
}

# Helper to skip tests requiring ImageMagick
skip_if_no_imagemagick <- function() {
  if (Sys.which("convert") == "") {
    testthat::skip("ImageMagick not available")
  }
}

# Helper to get test label files
test_label_files <- function() {
  list(
    lh_region1 = file.path(testdata_dir(), "cortical", "lh.region1.label"),
    lh_region2 = file.path(testdata_dir(), "cortical", "lh.region2.label"),
    rh_region1 = file.path(testdata_dir(), "cortical", "rh.region1.label")
  )
}

# Helper to get test MGZ file
test_mgz_file <- function() {
  file.path(testdata_dir(), "volumetric", "aseg.mgz")
}

# Helper to get test LUT file
test_lut_file <- function() {
  file.path(testdata_dir(), "volumetric", "lut.txt")
}

# Helper to get test annotation files (Yeo7 networks)
test_annot_files <- function() {
  list(
    lh = file.path(testdata_dir(), "cortical", "lh.yeo7.annot"),
    rh = file.path(testdata_dir(), "cortical", "rh.yeo7.annot")
  )
}

# Helper to get test annotation name
test_annot_name <- function() {
  "yeo7"
}

mock_future_pmap <- function(.l, .f, ...) {
  do.call(Map, c(list(f = .f), .l))
}

mock_future_map2 <- function(.x, .y, .f, ...) {
  mapply(.f, .x, .y, SIMPLIFY = FALSE)
}

expect_warnings <- function(expr, regexp) {
  warnings_caught <- character()
  result <- withCallingHandlers(
    expr,
    warning = function(w) {
      if (grepl(regexp, conditionMessage(w))) {
        warnings_caught[[length(warnings_caught) + 1L]] <<-
          conditionMessage(w)
        invokeRestart("muffleWarning")
      }
    }
  )
  testthat::expect_true(
    length(warnings_caught) > 0,
    label = paste0(
      "Expected at least one warning matching '", regexp, "'"
    )
  )
  invisible(result)
}

# Helper to skip tests requiring internet
skip_if_offline <- function() {
  tryCatch(
    {
      con <- url("https://ggseg.r-universe.dev/api/packages", open = "r")
      close(con)
    },
    error = function(e) {
      testthat::skip("No internet connection available")
    }
  )
}
