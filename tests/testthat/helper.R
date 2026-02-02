library(dplyr, quietly = TRUE, warn.conflicts = FALSE)
library(tidyr, quietly = TRUE, warn.conflicts = FALSE)
library(ggseg, quietly = TRUE, warn.conflicts = FALSE)
library(ggseg3d, quietly = TRUE, warn.conflicts = FALSE)

# Helper to get test data directory
testdata_dir <- function() {
  testthat::test_path("testdata")
}

# Helper to check if FreeSurfer is available
has_freesurfer <- function() {
  tryCatch(
    {
      fs_dir <- freesurfer::fs_dir()
      !is.null(fs_dir) && dir.exists(fs_dir)
    },
    error = function(e) FALSE
  )
}

# Helper to skip tests requiring FreeSurfer
skip_if_no_freesurfer <- function() {
  if (!has_freesurfer()) {
    testthat::skip("FreeSurfer not available")
  }
}

# Helper to get test label files
test_label_files <- function() {
  list(
    lh_region1 = file.path(testdata_dir(), "lh.test_region1.label"),
    lh_region2 = file.path(testdata_dir(), "lh.test_region2.label"),
    rh_region1 = file.path(testdata_dir(), "rh.test_region1.label")
  )
}

# Helper to get test MGZ file
test_mgz_file <- function() {
  file.path(testdata_dir(), "test_aseg.mgz")
}

# Helper to get test LUT file
test_lut_file <- function() {
  file.path(testdata_dir(), "test_aseg_lut.txt")
}
