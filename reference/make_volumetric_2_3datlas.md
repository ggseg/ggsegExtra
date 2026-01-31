# Volumetric segmentation to 3d-atlas

Function to create a ggseg3d-atlas from a volumetric parcellation.
Currently only tested using `.mgz` extension images. Will call
command-line tools to complete the process.

## Usage

``` r
make_volumetric_2_3datlas(
  template,
  color_lut = NULL,
  subject = "fsaverage5",
  subjects_dir = freesurfer::fs_subj_dir(),
  steps = 1:5,
  output_dir = tempdir(),
  verbose = TRUE,
  cleanup = TRUE
)
```

## Arguments

- template:

  template `volume.mgz` file path

- color_lut:

  Freesurfer colour look-up-table. Either as a path or a data.frame that
  [`is_ctab()`](https://ggseg.github.io/ggsegExtra/reference/is_ctab.md)

- subject:

  Freesurfer subject, must exist in whatever subject directory specified
  or set in the environment with \$SUBJECTS_DIR

- subjects_dir:

  Freesurfer subject directory

- steps:

  if cleanup is disabled, all files are saved, and steps can be re-run
  individually

- output_dir:

  output directory path

- verbose:

  logical indicating to be verbose or not

- cleanup:

  logical to toggle removal of all intermediary files

## Value

returns a ggseg3d-atlas ready object. Might need manual cleaning to
become a good atlas.

## Examples

``` r
if (FALSE) { # \dontrun{

fs_subject_dir <- freesurfer::fs_dir()
aseg_temp <- file.path(fs_subject_dir, "fsaverage5/mri/aseg.mgz")
colorlut <- file.path(fs_subject_dir, "ASegStatsLUT.txt")

make_volumetric_2_3datlas(aseg_temp, colorlut)
} # }
```
