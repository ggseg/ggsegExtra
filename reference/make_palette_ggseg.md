# Create ggseg palette from ggseg3d-atlas

atlases in ggseg have palettes based on colours from the paper
originally introducing the atlas. These colours are hard-coded into
ggseg3d-atlases. This function extracts those and makes a object ready
for incorporation to a ggseg-atlas repository

## Usage

``` r
make_palette_ggseg(ggseg3d_atlas)
```

## Arguments

- ggseg3d_atlas:

  ggseg3d-atlas

## Value

list with a colour palette

## Examples

``` r
make_palette_ggseg(dk_3d)
#> $dk
#>                   bankssts  caudal anterior cingulate 
#>                  "#196428"                  "#7D64A0" 
#>      caudal middle frontal            corpus callosum 
#>                  "#641900"                  "#784632" 
#>                     cuneus                 entorhinal 
#>                  "#DC1464"                  "#DC140A" 
#>                   fusiform          inferior parietal 
#>                  "#B4DC8C"                  "#DC3CDC" 
#>          inferior temporal          isthmus cingulate 
#>                  "#B42878"                  "#8C148C" 
#>          lateral occipital      lateral orbitofrontal 
#>                  "#141E8C"                  "#234B32" 
#>                    lingual       medial orbitofrontal 
#>                  "#E18C8C"                  "#C8234B" 
#>            middle temporal            parahippocampal 
#>                  "#A06432"                  "#14DC3C" 
#>                paracentral           pars opercularis 
#>                  "#3CDC3C"                  "#DCB48C" 
#>             pars orbitalis          pars triangularis 
#>                  "#146432"                  "#DC3C14" 
#>              pericalcarine                postcentral 
#>                  "#78643C"                  "#DC1414" 
#>        posterior cingulate                 precentral 
#>                  "#DCB4DC"                  "#3C14DC" 
#>                  precuneus rostral anterior cingulate 
#>                  "#A08CB4"                  "#50148C" 
#>     rostral middle frontal           superior frontal 
#>                  "#4B327D"                  "#14DCA0" 
#>          superior parietal          superior temporal 
#>                  "#14B48C"                  "#8CDCDC" 
#>              supramarginal               frontal pole 
#>                  "#50A014"                  "#640064" 
#>              temporal pole        transverse temporal 
#>                  "#464646"                  "#9696C8" 
#>                     insula 
#>                  "#FFC020" 
#> 
```
