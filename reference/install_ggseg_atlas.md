# Install ggseg-atlas from repo

installs ggseg-atlas library from the ggseg r-universe
<https://ggseg.r-universe.dev/#builds> .

## Usage

``` r
install_ggseg_atlas(
  package,
  repos = c(ggseg = "https://ggseg.r-universe.dev", getOption("repos")),
  ...
)
```

## Arguments

- package:

  package name

- repos:

  vector of repositories to install from. Defaults to ggseg r-universe
  and CRAN.

- ...:

  additional arguments to `install.packages`.

## Details

To install, will temporarily alter your install repo settings to also
use the ggseg r-universe build as source for packages. These settings
will be restored when the function exits.

## Examples

``` r
if (FALSE) { # \dontrun{
ggseg_atlas_repos("yeo")
install_ggseg_atlas("ggsegYeo2011")
} # }
```
