#' @import ggseg.formats
#' @keywords internal
"_PACKAGE"

## usethis namespace: start
#' @importFrom lifecycle badge
## usethis namespace: end
NULL

# nocov start
#' Utility to easily knit tutorial vignettes
#' @noRd
knit_tutorials <- function() {
  tutorials <- list.files("vignettes", "orig$", full.names = TRUE)

  build_tutorials <- function(file) {
    cli::cli_h1("Building {basename(file)}")

    knitr::knit(
      file,
      sub("\\.orig$", "", file)
    )
  }

  lapply(tutorials, build_tutorials)
}

#' Utility to set tutorial build options
#' @noRd
set_tutorial_options <- function() {
  knitr::opts_chunk$set(
    collapse = TRUE,
    comment = "#>",
    error = FALSE
  )
  options(
    freesurfer.verbose = FALSE,
    progressr.enabled = TRUE
  )

  with(
    future::plan(
      future::multisession(workers = future::availableCores() / 2)
    ),
    local = TRUE
  )

  check_fs(abort = FALSE) && has_magick()
}
# nocov end

## quiets concerns of R CMD check
if (getRversion() >= "2.15.1") {
  utils::globalVariables(c(
    ".id",
    ".subid",
    "filenm",
    "geometry",
    "ggseg_3d",
    "hemi",
    "key",
    "L2",
    "label",
    "region",
    "val",
    "view",
    "X",
    "Y"
  ))
}
