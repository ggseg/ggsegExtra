# Convert annotation to label

Calls FreeSurfer's `mri_annotation2label` to split an annotation file
into several labels.

## Usage

``` r
mri_annotation2label(
  annot_name,
  subject = "fsaverage5",
  hemisphere = "rh",
  output_dir = fs_subj_dir(),
  verbose = TRUE,
  opts = NULL
)
```

## Arguments

- annot_name:

  annotation name. File should exist in subjects label directory

- subject:

  Freesurfer subject, must exist in whatever subject directory specified
  or set in the environment with \$SUBJECTS_DIR

- hemisphere:

  hemisphere, either "rh" or "lh"

- output_dir:

  output directory path

- verbose:

  logical indicating to be verbose or not

- opts:

  other arguments to freesurfer command

## Value

nothing. Runs command line to write label files

## Examples

``` r
if (FALSE) { # \dontrun{
# for freesurfer help see:
freesurfer::fs_help("mri_annotation2label")
mri_annotation2label(annot_name = "aparc")

mri_annotation2label(annot_name = "aparc.a2009s")

mri_annotation2label(subject = "fsaverage", annot_name = "aparc.a2009s")
} # }
```
