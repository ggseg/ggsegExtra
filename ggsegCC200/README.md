
<!-- README.md is generated from README.Rmd. Please edit that file -->

# ggsegCC200

<!-- badges: start -->

[![Travis build
status](https://travis-ci.com/LCBC-UiO/ggsegCC200.svg?branch=master)](https://travis-ci.com/LCBC-UiO/ggsegCC200)
[![AppVeyor build
status](https://ci.appveyor.com/api/projects/status/github/LCBC-UiO/ggsegCC200?branch=master&svg=true)](https://ci.appveyor.com/project/LCBC-UiO/ggsegCC200)
[![Codecov test
coverage](https://codecov.io/gh/LCBC-UiO/ggsegCC200/branch/master/graph/badge.svg)](https://codecov.io/gh/LCBC-UiO/ggsegCC200?branch=master)
<!-- badges: end -->

This package contains dataset for plotting the Tracula white matter
tracts with ggseg and ggseg3d.

Yendiki et al. (2011) *Automated probabilistic reconstruction of
white-matter pathways in health and disease using an atlas of the
underlying anatomy*. Front. Neuroinform. 5:23.
[doi: 10.3389/fninf.2011.00023](https://www.ncbi.nlm.nih.gov/pubmed/22016733)

## Installation

You can install the released version of ggsegCC200 from
[GitHub](https://github.com/) with:

``` r
# install.packages("remotes")
remotes::install_github("LCBC-UiO/ggsegCC200")
```

``` r
library(ggseg)
library(ggseg3d)
library(ggsegCC200)

ggseg(atlas = cc200, mapping = aes(fill = region)) +
  scale_fill_brain("cc200", package = "ggsegCC200") +
  theme(legend.position = "bottom", 
        legend.text = element_text(size = 9)) +
  guides(fill = guide_legend(ncol = 3))
```

<img src="man/figures/README-unnamed-chunk-2-1.png" width="100%" />

``` r
ggseg3d(atlas = cc200_3d) %>% 
  add_glassbrain() %>% 
  pan_camera("right lateral")
```

<!-- ```{r "orca", include=FALSE} -->

<!-- p <- ggseg3d(atlas = cc200_3d) %>%  -->

<!--   add_glassbrain(hemisphere = "left")%>% -->

<!--   pan_camera("right lateral") %>%  -->

<!--   plotly::add_annotations( text="Screen capture", -->

<!--                   legendtitle=TRUE, showarrow=FALSE, -->

<!--                   font = list(color = "#000000b4", -->

<!--                               family = 'sans serif', -->

<!--                               size = 50)) -->

<!-- plotly::orca(p, "man/figures/README-3d-plot.png") -->

<!-- ``` -->

<img src="man/figures/README-3d-plot.png" width="100%" />

Please note that the ‘ggsegCC200’ project is released with a
[Contributor Code of Conduct](CODE_OF_CONDUCT.md). By contributing to
this project, you agree to abide by its terms.
