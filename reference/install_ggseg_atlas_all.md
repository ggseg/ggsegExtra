# Install all registered ggseg-atlases

Calls
[`ggseg_atlas_repos()`](https://ggseg.github.io/ggsegExtra/reference/ggseg_atlas_repos.md),
and installs all libraries listed. Be careful calling this function,
this will likely take quite some time to complete, and also will contain
a lot of heavy data. We recommend only installing atlases you are likely
to use and on demand.

## Usage

``` r
install_ggseg_atlas_all(
  repos = c(ggseg = "https://ggseg.r-universe.dev", getOption("repos")),
  ...
)
```

## Arguments

- repos:

  repositories to install from. Defaults to ggseg r-universe and a CRAN
  mirror.

- ...:

  additional arguments to `install.packages`.

## Examples

``` r
if (FALSE) { # \dontrun{
install_ggseg_atlas_all()
} # }
```
