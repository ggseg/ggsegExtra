# Create cortical ggseg3d-atlas from annot-file

Function loops through hemispheres and surfaces to create a data frame
that is ready to use with ggseg3d.

## Usage

``` r
make_aparc_2_3datlas(
  annot,
  subject = "fsaverage5",
  hemisphere = c("rh", "lh"),
  surface = c("inflated", "LCBC"),
  subjects_dir = freesurfer::fs_subj_dir(),
  annot_dir = file.path(subjects_dir, subject, "label"),
  output_dir = tempdir(),
  cleanup = TRUE,
  verbose = TRUE
)
```

## Arguments

- annot:

  annotation file, with name without the hemisphere information or the
  .annot extension, and be in annot_dir

- subject:

  Freesurfer subject, must exist in whatever subject directory specified
  or set in the environment with \$SUBJECTS_DIR

- hemisphere:

  hemisphere, either "rh" or "lh"

- surface:

  Freesurfer surface

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

nested data frame as ggseg3d-atlas

## Details

It is recommended that you change the region names for the atlas, and
the atlas name to something shorter. See the dk_3d atlas for examples.

## Examples

``` r
if (FALSE) { # \dontrun{
dt <- aparc_2_3datlas(annot = "aparc.a2009s")
dt <- aparc_2_3datlas(annot = "aparc.a2009s",
                      surface = "sphere")
} # }
```
