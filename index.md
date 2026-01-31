# ggsegExtra

This package contains information on verified atlases compatible for use
in [ggseg](https://ggseg.github.io/ggseg) and
[ggseg3d](https://ggseg.github.io/ggseg3d) plotting packages in R. It
also contains functions and pipelines to create custom atlases.

## Installing

We recommend installing this package from its
[r-universe](https://ggseg.r-universe.dev/#builds) build:

``` r
# Enable this universe
options(repos = c(
  ggseg = 'https://ggseg.r-universe.dev',
  CRAN = 'https://cloud.r-project.org'))

# Install some packages
install.packages('ggsegExtra')
```

Alternatively, install the package with the remotes package from github.

``` r
# install.packages("remotes")
remotes::install_github("ggseg/ggsegExtra")
```

### Development version of freesurfer

The atlas creation functions require a development version of the
freesurfer R package that is not yet on CRAN. Until the [PR is
merged](https://github.com/muschellij2/freesurfer/pull/28), install
from:

``` r
pak::pak("drmowinckels/freesurfer@refactor") 
```

## Create custom atlases

We have tutorials on how to make atlases available in the `Articles` of
the [package documentation page](https://ggseg.github.io/ggsegExtra/).
Currently, the pipeline for making 3d atlases needs FreeSufer to be
installed on your system and a ready `.annot` file to use for the
conversions. Improvements to the pipeline in creating cortical atlases
both for ggseg and ggseg3d is still being developed. Pipelines for
creating sub-cortical atlases of any kind are still in their infancy,
and we do not yet have good solutions for their creation. Suggestions
for improvement are welcome through GH issues or direct Pull requests.

## Available atlases

There are currently 23 available atlases across 23 packages. The
packages, their repository name and location can be found with:

### [ggsegEconomo](https://github.com/ggseg/ggsegEconomo)

![ggsegEconomo
logo](https://github.com/ggseg/ggsegEconomo/raw/HEAD/man/figures/logo.png)

[![R-CMD-check](https://github.com/ggseg/ggsegEconomo/workflows/R-CMD-check/badge.svg)](https://github.com/ggseg/ggsegEconomo/actions)
[![DOI](https://zenodo.org/badge/424723872.svg)](https://zenodo.org/badge/latestdoi/424723872)

economo datasets for the ggseg-plotting tool This is a support package
for the ggseg, and ggseg3d packages. It contains the economo atlases to
plot using functions from those two packages.

### [ggsegFlechsig](https://github.com/ggseg/ggsegFlechsig)

![ggsegFlechsig
logo](https://github.com/ggseg/ggsegFlechsig/raw/HEAD/man/figures/logo.png)

[![R-CMD-check](https://github.com/ggseg/ggsegFlechsig/workflows/R-CMD-check/badge.svg)](https://github.com/ggseg/ggsegFlechsig/actions)
[![DOI](https://zenodo.org/badge/425033264.svg)](https://zenodo.org/badge/latestdoi/425033264)

Flechsig datasets for the ggseg-plotting tool This is a support package
for the ggseg, and ggseg3d packages. It contains the Flechsig atlases to
plot using functions from those two packages.

### [ggsegKleist](https://github.com/ggseg/ggsegKleist)

![ggsegKleist
logo](https://github.com/ggseg/ggsegKleist/raw/HEAD/man/figures/logo.png)

[![R-CMD-check](https://github.com/ggseg/ggsegKleist/workflows/R-CMD-check/badge.svg)](https://github.com/ggseg/ggsegKleist/actions)
[![DOI](https://zenodo.org/badge/425006974.svg)](https://zenodo.org/badge/latestdoi/425006974)

Kleist datasets for the ggseg-plotting tool This is a support package
for the ggseg, and ggseg3d packages. It contains the historical Kleist
atlases to plot using functions from those two packages.

### [ggsegBrodmann](https://github.com/ggseg/ggsegBrodmann)

![ggsegBrodmann
logo](https://github.com/ggseg/ggsegBrodmann/raw/HEAD/man/figures/logo.png)

[![R-CMD-check](https://github.com/ggseg/ggsegBrodmann/workflows/R-CMD-check/badge.svg)](https://github.com/ggseg/ggsegBrodmann/actions)
[![DOI](https://zenodo.org/badge/424723872.svg)](https://zenodo.org/badge/latestdoi/424723872)

brodmann datasets for the ggseg-plotting tool This is a support package
for the ggseg, and ggseg3d packages. It contains the brodmann area
atlases to plot using functions from those two packages.

### [ggsegCampbell](https://github.com/ggseg/ggsegCampbell)

![ggsegCampbell
logo](https://github.com/ggseg/ggsegCampbell/raw/HEAD/man/figures/logo.png)

[![R-CMD-check](https://github.com/ggseg/ggsegCampbell/workflows/R-CMD-check/badge.svg)](https://github.com/ggseg/ggsegCampbell/actions)
[![DOI](https://zenodo.org/badge/424732644.svg)](https://zenodo.org/badge/latestdoi/424732644)

Campbell(1905) datasets for the ggseg-plotting tool This is a support
package for the ggseg, and ggseg3d packages. It contains the Campbell
(1905) atlases to plot using functions from those two packages.

### [ggsegIca](https://github.com/ggseg/ggsegIca)

![ggsegIca
logo](https://github.com/ggseg/ggsegIca/raw/HEAD/man/figures/logo.png)

[![R-CMD-check](https://github.com/ggseg/ggsegIca/workflows/R-CMD-check/badge.svg)](https://github.com/ggseg/ggsegIca/actions)
[![DOI](https://zenodo.org/badge/417492385.svg)](https://zenodo.org/badge/latestdoi/417492385)

ica datasets for the ggseg-plotting tool This is a support package for
the ggseg, and ggseg3d packages. It contains the ica atlases to plot
using functions from those two packages.

### [ggsegArslan](https://github.com/ggseg/ggsegArslan)

![ggsegArslan
logo](https://github.com/ggseg/ggsegArslan/raw/HEAD/man/figures/logo.png)

[![R-CMD-check](https://github.com/ggseg/ggsegArslan/workflows/R-CMD-check/badge.svg)](https://github.com/ggseg/ggsegArslan/actions)
[![DOI](https://zenodo.org/badge/417483567.svg)](https://zenodo.org/badge/latestdoi/417483567)

arslan datasets for the ggseg-plotting tool This is a support package
for the ggseg, and ggseg3d packages. It contains the arslan atlases to
plot using functions from those two packages.

### [ggsegGordon](https://github.com/ggseg/ggsegGordon)

![ggsegGordon
logo](https://github.com/ggseg/ggsegGordon/raw/HEAD/man/figures/logo.png)

[![R-CMD-check](https://github.com/ggseg/ggsegGordon/workflows/R-CMD-check/badge.svg)](https://github.com/ggseg/ggsegGordon/actions)
[![DOI](https://zenodo.org/badge/417531585.svg)](https://zenodo.org/badge/latestdoi/417531585)

gordon datasets for the ggseg-plotting tool This is a support package
for the ggseg, and ggseg3d packages. It contains the gordon atlases to
plot using functions from those two packages.

### [ggsegAicha](https://github.com/ggseg/ggsegAicha)

![ggsegAicha
logo](https://github.com/ggseg/ggsegAicha/raw/HEAD/man/figures/logo.png)

[![R-CMD-check](https://github.com/ggseg/ggsegAicha/workflows/R-CMD-check/badge.svg)](https://github.com/ggseg/ggsegAicha/actions)
[![DOI](https://zenodo.org/badge/417476540.svg)](https://zenodo.org/badge/latestdoi/417476540)

aicha datasets for the ggseg-plotting tool This is a support package for
the ggseg, and ggseg3d packages. It contains the aicha atlases to plot
using functions from those two packages.

### [ggsegAal](https://github.com/ggseg/ggsegAal)

![ggsegAal
logo](https://github.com/ggseg/ggsegAal/raw/HEAD/man/figures/logo.png)

[![R-CMD-check](https://github.com/ggseg/ggsegAal/workflows/R-CMD-check/badge.svg)](https://github.com/ggseg/ggsegAal/actions)
[![DOI](https://zenodo.org/badge/417464916.svg)](https://zenodo.org/badge/latestdoi/417464916)

aal datasets for the ggseg-plotting tool This is a support package for
the ggseg, and ggseg3d packages. It contains the aal atlases to plot
using functions from those two packages.

### [ggsegPower](https://github.com/ggseg/ggsegPower)

![ggsegPower
logo](https://github.com/ggseg/ggsegPower/raw/HEAD/man/figures/logo.png)

[![R-CMD-check](https://github.com/ggseg/ggsegPower/workflows/R-CMD-check/badge.svg)](https://github.com/ggseg/ggsegPower/actions)
[![DOI](https://zenodo.org/badge/417497619.svg)](https://zenodo.org/badge/latestdoi/417497619)

power datasets for the ggseg-plotting tool This is a support package for
the ggseg, and ggseg3d packages. It contains the power atlases to plot
using functions from those two packages.

### [ggsegBrainnetome](https://github.com/ggseg/ggsegBrainnetome)

![ggsegBrainnetome
logo](https://github.com/ggseg/ggsegBrainnetome/raw/HEAD/man/figures/logo.png)

[![R-CMD-check](https://github.com/ggseg/ggsegBrainnetome/workflows/R-CMD-check/badge.svg)](https://github.com/ggseg/ggsegBrainnetome/actions)
[![DOI](https://zenodo.org/badge/416644902.svg)](https://zenodo.org/badge/latestdoi/416644902)

Brainnetome datasets for the ggseg-plotting tool This is a support
package for the ggseg, and ggseg3d packages. It contains the Brainnetome
atlases to plot using functions from those two packages.

### [ggsegJHU](https://github.com/ggseg/ggsegJHU)

![ggsegJHU
logo](https://github.com/ggseg/ggsegJHU/raw/HEAD/man/figures/logo.png)

[![R build
status](https://github.com/LCBC-UiO/ggsegJHU/workflows/R-CMD-check/badge.svg)](https://github.com/LCBC-UiO/ggsegJHU/actions)
[![DOI](https://zenodo.org/badge/250280074.svg)](https://zenodo.org/badge/latestdoi/250280074)

JHU datasets for the ggseg-plotting tool This is a support package for
the ggseg, and ggseg3d packages. It contains the JHU atlases to plot
using functions from those two packages.

### [ggsegHO](https://github.com/ggseg/ggsegHO)

![ggsegHO
logo](https://github.com/ggseg/ggsegHO/raw/HEAD/man/figures/logo.png)

[![R-CMD-check](https://github.com/ggseg/ggsegHO/workflows/R-CMD-check/badge.svg)](https://github.com/ggseg/ggsegHO/actions)
[![DOI](https://zenodo.org/badge/250284032.svg)](https://zenodo.org/badge/latestdoi/250284032)

Harvard-Oxford datasets for the ggseg-plotting tool This is a support
package for the ggseg, and ggseg3d packages. It contains the
Harvard-Oxford cortical atlases to plot using functions from those two
packages.

### [ggsegICBM](https://github.com/ggseg/ggsegICBM)

![ggsegICBM
logo](https://github.com/ggseg/ggsegICBM/raw/HEAD/man/figures/logo.png)

[![R-CMD-check](https://github.com/ggseg/ggsegICBM/workflows/R-CMD-check/badge.svg)](https://github.com/ggseg/ggsegICBM/actions)
[![DOI](https://zenodo.org/badge/250281676.svg)](https://zenodo.org/badge/latestdoi/250281676)

ICBM datasets for the ggseg-plotting tool This is a support package for
the ggseg, and ggseg3d packages. It contains the ICBM atlases to plot
using functions from those two packages.

### [ggsegDefaultExtra](https://github.com/ggseg/ggsegDefaultExtra)

![ggsegDefaultExtra
logo](https://github.com/ggseg/ggsegDefaultExtra/raw/HEAD/man/figures/logo.png)

[![R build
status](https://github.com/LCBC-UiO/ggsegDefaultExtra/workflows/R-CMD-check/badge.svg)](https://github.com/LCBC-UiO/ggsegDefaultExtra/actions)
[![DOI](https://zenodo.org/badge/250292657.svg)](https://zenodo.org/badge/latestdoi/250292657)

Extra dk and aseg datasets for the ggseg-plotting tool This is a support
package for the ggseg, and ggseg3d packages. It contains the extra dk
and aseg, with some minor differences from the original atlases to plot
using functions from those two packages.

### [ggsegDKT](https://github.com/ggseg/ggsegDKT)

![ggsegDKT
logo](https://github.com/ggseg/ggsegDKT/raw/HEAD/man/figures/logo.png)

[![R build
status](https://github.com/LCBC-UiO/ggsegDKT/workflows/R-CMD-check/badge.svg)](https://github.com/LCBC-UiO/ggsegDKT/actions)
[![DOI](https://zenodo.org/badge/314486110.svg)](https://zenodo.org/badge/latestdoi/314486110)

Desikan-Killiany-Tourville datasets for the ggseg-plotting tool This is
a support package for the ggseg, and ggseg3d packages. It contains the
DKT atlases to plot using functions from those two packages.

### [ggsegTracula](https://github.com/ggseg/ggsegTracula)

![ggsegTracula
logo](https://github.com/ggseg/ggsegTracula/raw/HEAD/man/figures/logo.png)

[![R build
status](https://github.com/LCBC-UiO/ggsegTracula/workflows/R-CMD-check/badge.svg)](https://github.com/LCBC-UiO/ggsegTracula/actions)
[![DOI](https://zenodo.org/badge/250281064.svg)](https://zenodo.org/badge/latestdoi/250281064)

Tracula datasets for the ggseg-plotting tool This is a support package
for the ggseg, and ggseg3d packages. It contains the Tracula atlases to
plot using functions from those two packages.

### [ggsegChen](https://github.com/ggseg/ggsegChen)

![ggsegChen
logo](https://github.com/ggseg/ggsegChen/raw/HEAD/man/figures/logo.png)

[![R build
status](https://github.com/LCBC-UiO/ggsegChen/workflows/R-CMD-check/badge.svg)](https://github.com/LCBC-UiO/ggsegChen/actions)
[![DOI](https://zenodo.org/badge/250277410.svg)](https://zenodo.org/badge/latestdoi/250277410)

Chen datasets for the ggseg-plotting tool This is a support package for
the ggseg, and ggseg3d packages. It contains the Chen atlases to plot
using functions from those two packages.

### [ggsegDesterieux](https://github.com/ggseg/ggsegDesterieux)

![ggsegDesterieux
logo](https://github.com/ggseg/ggsegDesterieux/raw/HEAD/man/figures/logo.png)

[![R build
status](https://github.com/LCBC-UiO/ggsegDesterieux/workflows/R-CMD-check/badge.svg)](https://github.com/LCBC-UiO/ggsegDesterieux/actions)
[![DOI](https://zenodo.org/badge/250272332.svg)](https://zenodo.org/badge/latestdoi/250272332)

Desterieux datasets for the ggseg-plotting tool This is a support
package for the ggseg, and ggseg3d packages. It contains the Desterieux
atlases to plot using functions from those two packages.

### [ggsegSchaefer](https://github.com/ggseg/ggsegSchaefer)

![ggsegSchaefer
logo](https://github.com/ggseg/ggsegSchaefer/raw/HEAD/man/figures/logo.png)

[![R-CMD-check](https://github.com/ggseg/ggsegSchaefer/workflows/R-CMD-check/badge.svg)](https://github.com/ggseg/ggsegSchaefer/actions)
[![DOI](https://zenodo.org/badge/250276444.svg)](https://zenodo.org/badge/latestdoi/250276444)

Schaefer datasets for the ggseg-plotting tool This is a support package
for the ggseg, and ggseg3d packages. It contains the Schaefer atlases to
plot using functions from those two packages.

### [ggsegGlasser](https://github.com/ggseg/ggsegGlasser)

![ggsegGlasser
logo](https://github.com/ggseg/ggsegGlasser/raw/HEAD/man/figures/logo.png)

[![R build
status](https://github.com/LCBC-UiO/ggsegGlasser/workflows/R-CMD-check/badge.svg)](https://github.com/LCBC-UiO/ggsegGlasser/actions)
[![DOI](https://zenodo.org/badge/250278991.svg)](https://zenodo.org/badge/latestdoi/250278991)

Glasser datasets for the ggseg-plotting tool This is a support package
for the ggseg, and ggseg3d packages. It contains the Glasser atlases to
plot using functions from those two packages.

### [ggsegYeo2011](https://github.com/ggseg/ggsegYeo2011)

![ggsegYeo2011
logo](https://github.com/ggseg/ggsegYeo2011/raw/HEAD/man/figures/logo.png)

[![R build
status](https://github.com/LCBC-UiO/ggsegYeo2011/workflows/R-CMD-check/badge.svg)](https://github.com/LCBC-UiO/ggsegYeo2011/actions)
[![DOI](https://zenodo.org/badge/250192046.svg)](https://zenodo.org/badge/latestdoi/250192046)

Yeo 2011 Datasets for the ‘ggseg’-Package This is a support package for
the ggseg, and ggseg3d packages. It contains the Yeo 2011 atlases to
plot using functions from those two packages.

## Code of Conduct

Please note that the ggsegExtra project is released with a [Contributor
Code of
Conduct](https://www.contributor-covenant.org/version/1/0/0/code-of-conduct.html).
By contributing to this project, you agree to abide by its terms.

### Report bugs or requests

Don’t hesitate to ask for support using [github
issues](https://github.com/ggseg/ggsegExtra/issues), or requesting new
atlases. While we would love getting help in creating new atlases, you
may also request atlases through the issues, and we will try to get to
it.

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
