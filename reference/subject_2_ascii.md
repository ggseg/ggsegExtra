# Convert subject surface files to ascii

This function goes through all specified subject, per hemisphere,
surface, curvature and no fix curvature specified and turns them into
ascii files.

## Usage

``` r
subject_2_ascii(
  subject = "fsaverage5",
  hemisphere = c("rh", "lh"),
  surfaces = fs_surfaces(),
  curvatures = fs_curvatures(),
  nofix_curv = fs_nofixcurv(),
  subjects_dir = freesurfer::fs_subj_dir(),
  output_dir = subjects_dir,
  verbose = TRUE
)
```

## Arguments

- subject:

  Freesurfer subject, must exist in whatever subject directory specified
  or set in the environment with \$SUBJECTS_DIR

- hemisphere:

  hemisphere, either "rh" or "lh"

- surfaces:

  string vector of surfaces

- curvatures:

  string vector of curvatures

- nofix_curv:

  string vector of nofix curvatures

- subjects_dir:

  Freesurfer subject directory

- output_dir:

  output directory path

- verbose:

  logical indicating to be verbose or not

## Value

no return. Writes files.

## Examples

``` r
if (FALSE) { # \dontrun{
subject_2_ascii()
subject_2_ascii("fsaverage")
subject_2_ascii("bert")
} # }
```
