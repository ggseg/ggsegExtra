
<!-- README.md is generated from README.Rmd. Please edit that file -->

<!-- badges: start -->

[![Travis build
Status](https://travis-ci.com/LCBC-UiO/ggsegExtra.svg?branch=master)](https://travis-ci.com/LCBC-UiO/ggsegExtra)
[![AppVeyor build
status](https://ci.appveyor.com/api/projects/status/github/LCBC-UiO/ggsegExtra?branch=master&svg=true)](https://ci.appveyor.com/project/LCBC-UiO/ggsegExtra)
[![Coverage
Status](https://codecov.io/gh/LCBC-UiO/ggsegExtra/branch/master/graph/badge.svg)](https://codecov.io/gh/LCBC-UiO/ggsegExtra)
[![CRAN
status](https://www.r-pkg.org/badges/version/ggsegExtra)](https://CRAN.R-project.org/package=ggsegExtra)
[![Lifecycle:
stable](https://img.shields.io/badge/lifecycle-stable-brightgreen.svg)](https://www.tidyverse.org/lifecycle/#stable)
[![R build
status](https://github.com/LCBC-UiO/ggsegExtra/workflows/R-CMD-check/badge.svg)](https://github.com/LCBC-UiO/ggsegExtra/actions)
<!-- badges: end -->

# ggsegExtra <img src="man/img/logo.png" align="right" alt="" width="120" />

This package contains information on verified atlases compatible for use
in [ggseg](https://lcbc-uio.github.io/ggseg) and
[ggseg3d](https://lcbc-uio.github.io/ggseg3d) plotting packages in R. It
also contains functions and pipelines to create custom atlases.

## Atlases

There are currently 28 available atlases across 11 packages. The
packages, their repository name and location can be found with:

``` r
library(ggsegExtra)

# List all verified compatible atlases
ggseg_atlas_repos()
#> # A tibble: 11 x 6
#>    repo            ggseg ggseg3d source comment                      package    
#>    <chr>           <lgl> <lgl>   <chr>  <chr>                        <chr>      
#>  1 LCBC-UiO/ggseg… TRUE  TRUE    github both 17 and 7 Network data   ggsegYeo20…
#>  2 LCBC-UiO/ggseg… FALSE TRUE    github the 2009 atlas               ggsegDeste…
#>  3 LCBC-UiO/ggseg… FALSE TRUE    github both thickness and area maps ggsegChen  
#>  4 LCBC-UiO/ggseg… FALSE TRUE    github both 17 and 7 networks       ggsegSchae…
#>  5 LCBC-UiO/ggseg… TRUE  TRUE    github full atlas                   ggsegGlass…
#>  6 LCBC-UiO/ggseg… TRUE  TRUE    github white tract atlas            ggsegJHU   
#>  7 LCBC-UiO/ggseg… TRUE  TRUE    github white tract atlas            ggsegTracu…
#>  8 LCBC-UiO/ggseg… FALSE TRUE    github white tract atlas            ggsegICBM  
#>  9 LCBC-UiO/ggseg… TRUE  FALSE   github Harvard-Oxford cortical (FS… ggsegHO    
#> 10 LCBC-UiO/ggseg… TRUE  FALSE   github extra 2d view for dk, p/a d… ggsegDefau…
#> 11 LCBC-UiO/ggseg… TRUE  TRUE    github Desikan-Killiany-Tourville … ggsegDKT
```

We have tutorials on how to make atlases available in the `Articles` of
the [package documentation
page](https://lcbc-uio.github.io/ggsegExtra/). Currently, the pipeline
for making 3d atlases needs Freesufer to be installed on your system and
a ready `.annot` file to use for the converions. Improvements to the
pipeline in creating cortical atlases both for ggseg and ggseg3d is
still being developed. Pipelines for creating subcortical atlases of any
kind are still in their infancy, and we do not yet have good solutions
for their creation. Suggestions for improvement are welcome through GH
issues or direct Pull requests.

## Code of Conduct

Please note that the ggsegExtra project is released with a [Contributor
Code of
Conduct](https://www.contributor-covenant.org/version/1/0/0/code-of-conduct.html).
By contributing to this project, you agree to abide by its terms.

### Report bugs or requests

Don’t hesitate to ask for support using [github
issues](https://github.com/LCBC-UiO/ggsegExtra/issues), or requesting
new atlases. While we would love getting help in creating new atlases,
you may also request atlases through the issues, and we will try to get
to it.

# Funding

This work is funded by **EU Horizon 2020 Grant** *‘Healthy minds 0-100
years: Optimizing the use of European brain imaging cohorts
(Lifebrain)’*, with grant agreement `732592`. The project has also
received funding from the **European Research Council**’s *Starting
grant* (grant agreements `283634`, to Anders Martin Fjell and `313440`
to Kristine Beate Walhovd) and *Consolidator Grant* (grant agreement
`771355` to Kristine Beate Walhovd and `725025` to Anders Martin Fjell).
The project has received funding through multiple grants from the
Norwegian Research Council.
