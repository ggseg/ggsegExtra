#' List all online repositories with ggseg-atlases
#' 
#' Function to easily find all online repositories that
#' contain ggseg atlases
#'
#' @param pattern string pattern to search repos
#' @param ... additional arguments to \code{\link[base]{grep}}
#'
#' @return data frame of online repositories with ggseg-atlases
#' @export
#'
#' @examples
#' ggseg_atlas_repos()
#' 
#' ggseg_atlas_repos("yeo")
ggseg_atlas_repos <- function(pattern = NULL, ...){
  repos <- dplyr::tribble(
    ~ repo,                       ~ggseg, ~ggseg3d, ~source,  ~comment,
    "ggseg/ggsegYeo2011",      TRUE,    TRUE,    "github", "both 17 and 7 Network data",
    "ggseg/ggsegDesterieux",   FALSE,   TRUE,    "github", "the 2009 atlas",
    "ggseg/ggsegChen",         FALSE,   TRUE,    "github", "both thickness and area maps",
    "ggseg/ggsegSchaefer",     FALSE,   TRUE,    "github", "both 17 and 7 networks",
    "ggseg/ggsegGlasser",      TRUE,    TRUE,    "github", "full atlas",
    "ggseg/ggsegJHU",          TRUE,    TRUE,    "github", "white tract atlas",
    "ggseg/ggsegTracula",      TRUE,    TRUE,    "github", "white tract atlas",
    "ggseg/ggsegICBM",         FALSE,   TRUE,    "github", "white tract atlas",
    "ggseg/ggsegHO",           TRUE,    FALSE,   "github", "Harvard-Oxford cortical (FSL)",
    "ggseg/ggsegDefaultExtra", TRUE,    FALSE,   "github", "extra 2d view for dk, p/a division of aseg hippocampus",
    "ggseg/ggsegDKT",          TRUE,    TRUE,    "github", "Desikan-Killiany-Tourville cortical atlas"
  )
  repos$package = basename(repos$repo)
  
  if(!is.null(pattern)){
    idx <- grep(pattern, repos$repo, ...)
    repos <- repos[idx, ]
  }
  
  repos
}



#' Install ggseg-atlas from repo
#' 
#' installs ggseg-atlas library from
#' the ggseg r-universe <https://ggseg.r-universe.dev/ui#builds> .
#' 
#' To install, will temporarily alter your install
#' repo settings to also use the ggseg r-universe
#' build as source for packages. These settings will
#' be restored when the function exits.
#'
#' @param package package name
#' @param ... additional arguments to \code{install.packages}.
#' @param repos vector of repositories to install from. Defaults to
#'   ggseg r-universe and CRAN.
#' @export
#'
#' @examples
#' \dontrun{
#' yeo2011_repo <- ggseg_atlas_repos("yeo2011")
#' install_ggseg_atlas(yeo2011_repo$repo, yeo2011_repo$source)
#' }
install_ggseg_atlas <- function(package, 
                                repos = c(
                                  ggseg = 'https://ggseg.r-universe.dev',
                                  CRAN = 'https://cloud.r-project.org'), 
                                ...){
  utils::install.packages(package, 
                   repos = repos,
                   ...)
  
}


#' Install all ggseg-atlases registeres
#' 
#' Calls \code{\link{ggseg_atlas_repos}}, 
#' and installs all libraries listed.
#' Be careful calling this function, this
#' will likely take quite some time to
#' complete, and also will contain a lot of
#' heavy data. We recommend only installing 
#' atlases you are likely to use and on demand.
#'
#' @param repos repositories to install from. Defaults to ggseg 
#'    r-universe and a CRAN mirror.
#' @param ... additional arguments to \code{install.packages}.
#' @export
#'
#' @examples
#' \dontrun{
#' install_ggseg_atlas_all()
#' }
install_ggseg_atlas_all <- function(
  repos = c(
    ggseg = 'https://ggseg.r-universe.dev',
    CRAN = 'https://cloud.r-project.org'), 
  ...){
  
  pkgs <- ggseg_atlas_repos()$package
  
  utils::install.packages(pkgs, 
                   repos = repos,
                   ...)
}


