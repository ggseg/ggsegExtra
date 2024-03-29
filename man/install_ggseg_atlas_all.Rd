% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/extra-atlases.R
\name{install_ggseg_atlas_all}
\alias{install_ggseg_atlas_all}
\title{Install all ggseg-atlases registeres}
\usage{
install_ggseg_atlas_all(
  repos = c(ggseg = "https://ggseg.r-universe.dev", getOption("repos")),
  ...
)
}
\arguments{
\item{repos}{repositories to install from. Defaults to ggseg 
r-universe and a CRAN mirror.}

\item{...}{additional arguments to \code{install.packages}.}
}
\description{
Calls \code{\link{ggseg_atlas_repos}}, 
and installs all libraries listed.
Be careful calling this function, this
will likely take quite some time to
complete, and also will contain a lot of
heavy data. We recommend only installing 
atlases you are likely to use and on demand.
}
\examples{
\dontrun{
install_ggseg_atlas_all()
}
}
