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
    "LCBC-UiO/ggsegYeo2011",      TRUE,    TRUE,    "github", "both 17 and 7 Network data",
    "LCBC-UiO/ggsegDesterieux",   FALSE,   TRUE,    "github", "the 2009 atlas",
    "LCBC-UiO/ggsegChen",         FALSE,   TRUE,    "github", "both thickness and area maps",
    "LCBC-UiO/ggsegSchaefer",     FALSE,   TRUE,    "github", "both 17 and 7 networks",
    "LCBC-UiO/ggsegGlasser",      TRUE,    TRUE,    "github", "full atlas",
    "LCBC-UiO/ggsegJHU",          TRUE,    TRUE,    "github", "white tract atlas",
    "LCBC-UiO/ggsegTracula",      TRUE,    TRUE,    "github", "white tract atlas",
    "LCBC-UiO/ggsegICBM",         FALSE,   TRUE,    "github", "white tract atlas",
    "LCBC-UiO/ggsegHO",           TRUE,    FALSE,   "github", "Harvard-Oxford cortical (FSL)",
    "LCBC-UiO/ggsegDefaultExtra", TRUE,    FALSE,   "github", "extra 2d view for dk, p/a division of aseg hippocampus",
  )
  repos$package = basename(repos$repo)
  
  if(!is.null(pattern)){
    idx <- grep(pattern, repos$repo, ...)
    repos <- repos[idx, ]
  }
  
  return(repos)
}



#' Install ggseg-atlas from repo
#' 
#' installs ggseg-atlas library from
#' online repository. 
#'
#' @param repo repository string
#' @param source source of repository as string
#' @param ... additional arguments to \code{remotes::install_[source]}
#' function called depending on the \code{source}.
#'
#' @export
#'
#' @examples
#' \dontrun{
#' yeo2011_repo <- ggseg_atlas_repos("yeo2011")
#' install_ggseg_atlas(yeo2011_repo$repo, yeo2011_repo$source)
#' }
install_ggseg_atlas <- function(repo, source, ...){
  src <- paste0("remotes::install_", source)
  install_func <- eval(parse(text = src))
  
  install_func(repo, ...)
  
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
#' @param ... additional arguments to \code{remotes::install_*}
#' function called depending on the \code{source}.
#' @export
#'
#' @examples
#' \dontrun{
#' install_ggseg_atlas_all()
#' }
install_ggseg_atlas_all <- function(...){
  
  repos <- ggseg_atlas_repos()
  
  mapply(install_ggseg_atlas, repos$repo, repos$source, ...)
}


