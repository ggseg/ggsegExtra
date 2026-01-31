# Converts annotations to atlas

This function will create an atlas ready data.frame for ggseg3d to plot
with plotly.

## Usage

``` r
aparc_2_mesh(
  subject = "fsaverage5",
  hemisphere = "rh",
  surface = "inflated",
  annot = "aparc",
  subjects_dir = freesurfer::fs_subj_dir(),
  annot_dir = file.path(subjects_dir, subject, "label"),
  output_dir = tempdir(),
  cleanup = TRUE,
  verbose = TRUE
)
```

## Arguments

- subject:

  Freesurfer subject, must exist in whatever subject directory specified
  or set in the environment with \$SUBJECTS_DIR

- hemisphere:

  hemisphere, either "rh" or "lh"

- surface:

  surface from subjects surf-folder

- annot:

  base-name of annot file

- subjects_dir:

  Freesurfer subject directory

- annot_dir:

  path to directory with annot-files

- output_dir:

  output directory path

- cleanup:

  logical to toggle removal of all intermediary files

- verbose:

  logical indicating to be verbose or not

## Value

data frame with one row per label

## Details

Based on A. Winkler scripts

## Examples

``` r
if (FALSE) { # \dontrun{
dt <- aparc_2_mesh()
dt <- aparc_2_mesh(surface = "white")
dt <- aparc_2_mesh(hemisphere = "lh")
dt <- aparc_2_mesh(annot = "aparc.a2009s")
} # }
```
