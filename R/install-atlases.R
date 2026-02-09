#' List available ggseg atlas packages
#'
#' Query the ggseg r-universe to see which atlas packages are available
#' for installation. Returns package metadata including name, version,
#' and description.
#'
#' @param pattern Optional regex to filter packages by name (e.g., `"yeo"`
#'   to find Yeo atlas packages).
#' @param ... Additional arguments passed to [base::grep()].
#'
#' @return A tibble of available atlas packages.
#' @seealso [install_ggseg_atlas()] to install a specific atlas
#' @export
#' @importFrom dplyr as_tibble
#' @examples
#' \dontrun{
#' # See all available atlases
#' ggseg_atlas_repos()
#'
#' # Find Yeo parcellation atlases
#' ggseg_atlas_repos("yeo")
#' }
ggseg_atlas_repos <- function(pattern = NULL, ...) {
  rlang::check_installed("jsonlite", reason = "to query atlas repositories")
  repos <- jsonlite::fromJSON("https://ggseg.r-universe.dev/api/packages")

  if (!is.null(pattern)) {
    idx <- grep(pattern, repos$Package, ...)
    repos <- repos[idx, ]
  }

  as_tibble(repos)
}


#' Install a ggseg atlas package
#'
#' Install an atlas package from the ggseg r-universe. This is the
#' easiest way to get pre-built atlases like DKT, Yeo, Schaefer, etc.
#'
#' @param package Package name (e.g., `"ggsegYeo2011"`, `"ggsegDKT"`).
#'   Use [ggseg_atlas_repos()] to see available packages.
#' @param repos Repositories to install from. Defaults to the ggseg
#'   r-universe plus your current CRAN mirror.
#' @param ... Additional arguments passed to [utils::install.packages()].
#'
#' @seealso [ggseg_atlas_repos()] to list available atlases
#' @export
#' @importFrom utils install.packages
#' @examples
#' \dontrun{
#' # Find available Yeo atlases
#' ggseg_atlas_repos("yeo")
#'
#' # Install one
#' install_ggseg_atlas("ggsegYeo2011")
#' }
install_ggseg_atlas <- function(
  package,
  repos = c(ggseg = "https://ggseg.r-universe.dev", getOption("repos")),
  ...
) {
  install.packages(package, repos = repos, ...)
}


#' Install all available ggseg atlas packages
#'
#' Downloads and installs every atlas package from the ggseg r-universe.
#' This will take a while and use substantial disk space. For most users,
#' installing individual atlases with [install_ggseg_atlas()] is more
#' practical.
#'
#' @param repos Repositories to install from. Defaults to the ggseg
#'   r-universe plus your current CRAN mirror.
#' @param ... Additional arguments passed to [utils::install.packages()].
#'
#' @seealso [install_ggseg_atlas()] to install specific atlases
#' @export
#' @importFrom utils install.packages
#' @examples
#' \dontrun{
#' # Install everything (slow, large download)
#' install_ggseg_atlas_all()
#' }
install_ggseg_atlas_all <- function(
  repos = c(ggseg = "https://ggseg.r-universe.dev", getOption("repos")),
  ...
) {
  pkgs <- ggseg_atlas_repos()$Package

  install.packages(pkgs, repos = repos, ...)
}
