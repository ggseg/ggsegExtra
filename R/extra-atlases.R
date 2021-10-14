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
#' @importFrom jsonlite stream_in
#' @importFrom dplyr as_tibble
#' @examples
#' ggseg_atlas_repos()
#' 
#' ggseg_atlas_repos("yeo")
ggseg_atlas_repos <- function(pattern = NULL, ...){
  con <- url("https://ggseg.r-universe.dev/stats/descriptions")
  repos <- stream_in(con)
  
  if(!is.null(pattern)){
    idx <- grep(pattern, repos$Package, ...)
    repos <- repos[idx, ]
  }
  
  as_tibble(repos)
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
                                repos = c(ggseg = 'https://ggseg.r-universe.dev', getOption("repos")), 
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
  repos = c(ggseg = 'https://ggseg.r-universe.dev', getOption("repos")), 
  ...){
  
  pkgs <- ggseg_atlas_repos()$package
  
  utils::install.packages(pkgs, 
                   repos = repos,
                   ...)
}


