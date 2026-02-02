#' Create a new ggseg atlas package
#'
#' Creates a new R package structure for a ggseg-compatible brain atlas.
#' Works from both the R console and the RStudio New Project wizard.
#'
#' The package includes:
#' - Template files for atlas creation (`data-raw/`)
#' - Documentation templates (`R/data.R`, `README.Rmd`)
#' - Test suite for atlas validation (`tests/`)
#' - GitHub Actions for R CMD check
#' - Hex logo creation script
#'
#' @param path Path where the package should be created. If the path already
#'   exists, it must be an empty directory. If `path` is missing and you're
#'   in RStudio, will use the current project directory.
#' @param atlas_name Name of the atlas (lowercase, no spaces). The package
#'   will be named `ggseg{AtlasName}` (e.g., "ggsegDkt" for `atlas_name = "dkt"`).
#'   If not provided, will be derived from the directory name.
#' @param open If `TRUE`, opens the new project in RStudio (if available).
#'   If `NA` (default), opens in RStudio if called interactively.
#' @param rstudio If `TRUE`, creates an `.Rproj` file for RStudio users.
#'
#' @return Invisibly returns the path to the created package.
#' @export
#'
#' @examples
#' \dontrun{
#' # Create atlas package in a new directory
#' create_atlas_repo("ggsegDkt", "dkt")
#'
#' # Create in current directory, derive name from path
#' create_atlas_repo("ggsegMyatlas")
#'
#' # Specify full path
#' create_atlas_repo("~/projects/ggsegSchaefer", "schaefer")
#'
#' # Without opening in RStudio
#' create_atlas_repo("ggsegHarvard", "harvard", open = FALSE)
#' }
create_atlas_repo <- function(
    path,
    atlas_name = NULL,
    open = rlang::is_interactive(),
    rstudio = TRUE
) {
 path <- normalizePath(path, mustWork = FALSE)

  # Derive atlas_name from path if not provided
  if (is.null(atlas_name)) {
    dir_name <- basename(path)
    # Extract atlas name from ggsegXxx format (case sensitive for ggseg prefix)
    if (grepl("^ggseg[A-Z]", dir_name)) {
      atlas_name <- sub("^ggseg", "", dir_name)
    } else if (grepl("^ggseg", dir_name, ignore.case = TRUE)) {
      # Handle lowercase ggseg prefix
      atlas_name <- sub("^ggseg", "", dir_name, ignore.case = TRUE)
    } else {
      atlas_name <- dir_name
    }
  }

  # Clean atlas name: lowercase first, then remove non-alphanumeric
  atlas_name <- tolower(atlas_name)
  atlas_name <- gsub("[^a-z0-9]", "", atlas_name)
  repo_name <- paste0("ggseg", tools::toTitleCase(atlas_name))

  if (nchar(atlas_name) == 0) {
    cli::cli_abort(c(
      "Invalid atlas name",
      "x" = "atlas_name must contain at least one letter or number",
      "i" = "Example: {.code create_atlas_repo('ggsegDkt', 'dkt')}"
    ))
  }

  # Check if path exists and is empty
 if (dir.exists(path)) {
    files <- list.files(path, all.files = TRUE, no.. = TRUE)
    if (length(files) > 0) {
      cli::cli_abort(c(
        "Directory is not empty",
        "x" = "{.path {path}} already contains files",
        "i" = "Use an empty directory or a new path"
      ))
    }
  }

  # Create the package structure
  create_atlas_from_template(path, atlas_name)

  # Create .Rproj file if requested
  if (rstudio) {
    create_rproj_file(path, repo_name)
  }

  cli::cli_alert_success("Created atlas package {.pkg {repo_name}}")
  cli::cli_alert_info("Location: {.path {path}}")

  cli::cli_h3("Next steps")
  cli::cli_bullets(c(
    "1" = "Edit {.file data-raw/create-atlas.R} to create your atlas",
    "2" = "Update {.file R/data.R} with documentation and citation",
    "3" = "Add atlas citation to {.file README.Rmd}",
    "4" = "Run {.code devtools::document()} to generate documentation",
    "5" = "Run {.code devtools::check()} to verify the package"
  ))

  # Open in RStudio if requested
  if (open && rstudio) {
    open_rstudio_project(path)
  }

  invisible(path)
}


#' @keywords internal
create_atlas_from_template <- function(path, atlas_name) {
  template_dir <- system.file(
    file.path("rstudio", "templates", "project", "create-ggseg-atlas"),
    package = "ggsegExtra"
  )

  if (!dir.exists(template_dir)) {
    cli::cli_abort(c(
      "Template not found",
      "x" = "Could not find atlas template directory",
      "i" = "Is ggsegExtra installed correctly?"
    ))
  }

  # Create base directory
  mkdir(path)

  # Create all subdirectories (including hidden ones like .github)
  dirs <- list.dirs(template_dir, full.names = FALSE, recursive = TRUE)
  for (d in dirs) {
    if (nchar(d) > 0) {
      mkdir(file.path(path, d))
    }
  }

  # Copy all files (including hidden ones)
  files <- list.files(template_dir, recursive = TRUE, all.files = TRUE)
  files <- files[!grepl("^\\.\\.?$", basename(files))]

  for (f in files) {
    src <- file.path(template_dir, f)
    dst <- file.path(path, f)
    mkdir(dirname(dst))
    file.copy(src, dst, overwrite = TRUE)
  }

  # Replace template placeholders
  all_files <- list.files(path, full.names = TRUE, recursive = TRUE, all.files = TRUE)
  # Skip binary files
  all_files <- all_files[!grepl("\\.(png|jpg|jpeg|gif|ico|rda|RData|rds)$", all_files, ignore.case = TRUE)]

  for (f in all_files) {
    if (file.exists(f) && !dir.exists(f)) {
      template_replace(f, atlas_name)
    }
  }

  invisible(path)
}


#' @keywords internal
create_rproj_file <- function(path, repo_name) {
  rproj_content <- c(
    "Version: 1.0",
    "",
    "RestoreWorkspace: No",
    "SaveWorkspace: No",
    "AlwaysSaveHistory: Default",
    "",
    "EnableCodeIndexing: Yes",
    "UseSpacesForTab: Yes",
    "NumSpacesForTab: 2",
    "Encoding: UTF-8",
    "",
    "RnwWeave: Sweave",
    "LaTeX: pdfLaTeX",
    "",
    "AutoAppendNewline: Yes",
    "StripTrailingWhitespace: Yes",
    "LineEndingConversion: Posix",
    "",
    "BuildType: Package",
    "PackageUseDevtools: Yes",
    "PackageInstallArgs: --no-multiarch --with-keep.source",
    "PackageRoxygenize: rd,collate,namespace"
  )

  rproj_file <- file.path(path, paste0(repo_name, ".Rproj"))
  writeLines(rproj_content, rproj_file)

  invisible(rproj_file)
}


#' @keywords internal
open_rstudio_project <- function(path) {
  if (!rstudioapi_available()) {
    return(invisible(FALSE))
  }

  rproj_files <- list.files(path, pattern = "\\.Rproj$", full.names = TRUE)
  if (length(rproj_files) == 0) {
    return(invisible(FALSE))
  }

  if (rstudioapi::isAvailable()) {
    rstudioapi::openProject(rproj_files[1], newSession = TRUE)
  }

  invisible(TRUE)
}


#' @keywords internal
rstudioapi_available <- function() {
  requireNamespace("rstudioapi", quietly = TRUE) &&
    rstudioapi::isAvailable()
}


#' @keywords internal
template_replace <- function(file, atlas_name) {
  repo_name <- paste0("ggseg", tools::toTitleCase(atlas_name))

  tryCatch({
    input <- readLines(file, warn = FALSE)
    output <- gsub("\\{GGSEG\\}", atlas_name, input)
    output <- gsub("\\{REPO\\}", repo_name, output)
    writeLines(output, file)
  }, error = function(e) {
    # Skip files that can't be read/written
    NULL
  })
}


# RStudio project wizard binding ----

#' @keywords internal
new_project_create_atlas_repo <- function(dir, ...) {
  params <- list(...)
  atlas_name <- params$atlas_name

  # Don't open new session when called from project wizard
  # (RStudio handles this)
  create_atlas_repo(
    path = dir,
    atlas_name = atlas_name,
    open = FALSE,
    rstudio = TRUE
  )
}
