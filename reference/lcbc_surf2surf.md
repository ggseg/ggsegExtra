# Convert LCBC surface file to other subjects

Convert LCBC surface file to other subjects

## Usage

``` r
lcbc_surf2surf(
  input_volume,
  source_subject = "fsaverage",
  target_subject = "fsaverage5",
  hemisphere = "rh",
  subjects_dir = fs_subj_dir(),
  output_dir = file.path(subjects_dir, target_subject, "surf"),
  cortex = TRUE,
  verbose = TRUE
)
```

## Arguments

- input_volume:

  path to input volume

- source_subject:

  source subject

- target_subject:

  target subject

- hemisphere:

  hemisphere, either "rh" or "lh"

- subjects_dir:

  Freesurfer subject directory

- output_dir:

  output directory path

- cortex:

  toggle "–cortex" (TRUE) or "–no-cortex" (FALSE)

- verbose:

  logical indicating to be verbose or not
