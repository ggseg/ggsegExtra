# Turn ggseg3d-atlas to ggseg

Function will create a data.frame based on a ggseg3d atlas, based on the
contours of each segment.

## Usage

``` r
make_ggseg3d_2_ggseg(
  ggseg3d_atlas,
  steps = 1:7,
  output_dir = tempdir(),
  view = c("medial", "lateral"),
  tolerance = 0,
  smoothness = 5,
  cleanup = FALSE
)
```

## Arguments

- ggseg3d_atlas:

  object of class ggseg3d-atlas

- steps:

  numeric vector of steps to run

- output_dir:

  output directory path

- view:

  which sides of the brain to be snapshotted

- tolerance:

  tolerance during vertex reduction
  [`st_simplify`](https://r-spatial.github.io/sf/reference/geos_unary.html)

- smoothness:

  smoothing factor, argument to
  [`smooth`](https://strimas.com/smoothr/reference/smooth.html)

- cleanup:

  logical to toggle removal of all intermediary files

## Value

data.frame ready for manual cleaning before turning into a proper
ggseg3d-atlas

## Examples

``` r
if (FALSE) { # \dontrun{

# Create the DKT atlas as found in the FreeSurfer Subjects directory
# And output the temporary files to the Desktop.
dkt_3d <- make_aparc_2_3datlas(annot = "aparc.DKTatlas",
    output_dir = "~/Desktop/")
} # }
```
