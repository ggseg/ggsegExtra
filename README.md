
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
<!-- badges: end -->

# ggsegExtra <img src="man/img/logo.png" align="right" alt="" width="120" />

This package contains extra atlases for the use in the
[ggseg](https://github.com/LCBC-UiO/ggseg) and
[ggseg3d](https://github.com/LCBC-UiO/ggseg3d) plotting packages in R.

## Atlases

There are currently 12 atlases available in the package:

| Title | Dataset    | Mesh | Polygon | citation                |
| :---- | :--------- | :--- | :------ | :---------------------- |
|       | chenAr     | No   | Yes     | Chen et al. (2013)      |
|       | chenTh     | No   | Yes     | Chen et al. (2013)      |
|       | desterieux | Yes  | No      | Destrieux et al. (2010) |
|       | glasser    | Yes  | Yes     | Glasser et al. (2016)   |
|       | hoCort     | No   | Yes     | Makris et al. (2006)    |
|       | icbm       | Yes  | No      | Mori et al. (2005)      |
|       | jhu        | Yes  | Yes     | Hua et al. (2008)       |
|       | schaefer17 | Yes  | No      | Schaefer et al. (2017)  |
|       | schaefer7  | Yes  | No      | Schaefer et al. (2017)  |
|       | tracula    | Yes  | Yes     | Yendiki et al. (2011)   |
|       | yeo17      | Yes  | Yes     | Yeo et al. (2011)       |
|       | yeo7       | Yes  | Yes     | Yeo et al. (2011)       |

Table of currently available atlases in the ggseg, ggseg3d, and the
ggsegExtra R-packages. Polygon and mesh refer to 2D and 3D brain atlas
representations, respectively

We are working on creating a detailed description in the wiki on how to
create and contribute atlases to the package. The `ggseg` function
already allows you to provide it with a data.frame of a custom atlas if
you have it, but it must correspond to certain specifications to work.

Please see the
[wiki](https://github.com/LCBC-UiO/ggseg/wiki/Creating-and-contributing-atlases)
for information on adding atlases, or inspect the included datasets for
requirements. If anything is unclear in the wiki, give us a shout out in
the issues\!

## Use

You will need to use the `ggseg` or `ggseg3d` package to use these
atlases, or wrangle them your self.

``` r
remotes::install_github("LCBC-UiO/ggseg")
remotes::install_github("LCBC-UiO/ggseg3d")
```

Make sure to always load the ggsegExtra package *after* the ggseg
packages.

``` r
library(ggseg)
library(ggseg3d)
library(ggsegExtra)
```

### Report bugs or requests

Don’t hesitate to ask for support using [github
issues](https://github.com/LCBC-UiO/ggsegExtra/issues), or requesting
new atlases. While we would love getting help in creating new atlases,
you may also request atlases through the issues, and we will try to get
to it.

# Funding

This tool is partly funded by:

**EU Horizon 2020 Grant:** Healthy minds 0-100 years: Optimising the use
of European brain imaging cohorts (Lifebrain).

**Grant agreement number:** 732592.

**Call:** Societal challenges: Health, demographic change and well-being

# References (atlas source articles)

<div id="refs" class="references">

<div id="ref-chen">

Chen, Chi-Hua, Mark Fiecas, E. D. Gutiérrez, Matthew S. Panizzon, Lisa
T. Eyler, Eero Vuoksimaa, Wesley K. Thompson, et al. 2013. “Genetic
Topography of Brain Morphology.” *Proceedings of the National Academy of
Sciences* 110 (42): 17089–94. <https://doi.org/10.1073/pnas.1308091110>.

</div>

<div id="ref-desterieux">

Destrieux, Christophe, Bruce Fischl, Anders Dale, and Eric Halgren.
2010. “Automatic Parcellation of Human Cortical Gyri and Sulci Using
Standard Anatomical Nomenclature.” *NeuroImage* 53 (1): 1–15.
<https://doi.org/https://doi.org/10.1016/j.neuroimage.2010.06.010>.

</div>

<div id="ref-glasser">

Glasser, Matthew F., Timothy S. Coalson, Emma C. Robinson, Carl D.
Hacker, John Harwell, Essa Yacoub, Kamil Ugurbil, et al. 2016. “A
Multi-Modal Parcellation of Human Cerebral Cortex.” *Nature* 536 (July):
171 EP. <https://doi.org/10.1038/nature18933>.

</div>

<div id="ref-jhu">

Hua, Kegang, Jiangyang Zhang, Setsu Wakana, Hangyi Jiang, Xin Li, Daniel
S. Reich, Peter A. Calabresi, James J. Pekar, Peter C. M. van Zijl, and
Susumu Mori. 2008. “Tract Probability Maps in Stereotaxic Spaces:
Analyses of White Matter Anatomy and Tract-Specific Quantification.”
*NeuroImage* 39 (1): 336–47.
<https://doi.org/https://doi.org/10.1016/j.neuroimage.2007.07.053>.

</div>

<div id="ref-ho">

Makris, Nikos, Jill M. Goldstein, David Kennedy, Steven M. Hodge, Verne
S. Caviness, Stephen V. Faraone, Ming T. Tsuang, and Larry J. Seidman.
2006. “Decreased Volume of Left and Total Anterior Insular Lobule in
Schizophrenia.” *Schizophrenia Research* 83 (2): 155–71.
<https://doi.org/https://doi.org/10.1016/j.schres.2005.11.020>.

</div>

<div id="ref-icbm">

Mori, Susumu, S. Wakana, Peter C M van Zijl, and L. M. Nagae-Poetscher.
2005. *MRI Atlas of Human White Matter*. Elsevier Science.
<https://www.elsevier.com/books/mri-atlas-of-human-white-matter/mori/978-0-444-51741-8>.

</div>

<div id="ref-schaefer">

Schaefer, Alexander, Ru Kong, Evan M Gordon, Timothy O Laumann, Xi-Nian
Zuo, Avram J Holmes, Simon B Eickhoff, and B T Thomas Yeo. 2017.
“Local-Global Parcellation of the Human Cerebral Cortex from Intrinsic
Functional Connectivity MRI.” *Cerebral Cortex* 28 (9): 3095–3114.
<https://doi.org/10.1093/cercor/bhx179>.

</div>

<div id="ref-tracula">

Yendiki, Anastasia, Patricia Panneck, Priti Srinivasan, Allison Stevens,
Lilla Zöllei, Jean Augustinack, Ruopeng Wang, et al. 2011. “Automated
Probabilistic Reconstruction of White-Matter Pathways in Health and
Disease Using an Atlas of the Underlying Anatomy.” *Frontiers in
Neuroinformatics* 5: 23. <https://doi.org/10.3389/fninf.2011.00023>.

</div>

<div id="ref-yeo2011">

Yeo, Thomas, B. T., Fenna M. Krienen, Jorge Sepulcre, Mert R. Sabuncu,
Danial Lashkari, Marisa Hollinshead, et al. 2011. “The Organization of
the Human Cerebral Cortex Estimated by Intrinsic Functional
Connectivity.” *Journal of Neurophysiology* 106 (3): 1125–65.
<https://doi.org/10.1152/jn.00338.2011>.

</div>

</div>
