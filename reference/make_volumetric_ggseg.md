# Make ggseg atlas from volumetric template

If making an atlas from a non-cortical atlas, volumetric atlases are the
best options. Instead of snapshotting images of inflated brain, will
snapshot brain slices given x, y, z coordinates for the slices trhoush
the `slices` argument.

## Usage

``` r
make_volumetric_ggseg(
  label_file,
  subject = "fsaverage5",
  subjects_dir = fs_subj_dir(),
  output_dir = tempdir(),
  color_lut = NULL,
  steps = 1:8,
  skip_existing = TRUE,
  slices = data.frame(x = c(130, 122, 122), y = c(130, 235, 112), z = c(130, 100, 106),
    view = c("axial", "sagittal", "coronal"), stringsAsFactors = FALSE),
  vertex_size_limits = NULL,
  dilate = NULL,
  tolerance = 0,
  smoothness = 5,
  verbose = TRUE,
  cleanup = FALSE
)
```

## Arguments

- label_file:

  a volumetric image containing the labels

- subject:

  Freesurfer subject, must exist in whatever subject directory specified
  or set in the environment with \$SUBJECTS_DIR

- subjects_dir:

  Freesurfer subject directory

- output_dir:

  output directory path

- color_lut:

  a file containing color information for the labels

- steps:

  numeric vector of steps to run

- skip_existing:

  logical. If slice snapshots already exist, should these be skipped.

- slices:

  a data.frame with columns x, y, z, and view specifying coordinates and
  view of slice snapshots.

- vertex_size_limits:

  numeric vector of two, setting the minimum and maximum vector size of
  polygons. Defaults to NULL, which sets no limits.

- dilate:

  numeric. Dilation factor for polygons. Default NULL applies no
  dilation.

- tolerance:

  tolerance during vertex reduction
  [`st_simplify`](https://r-spatial.github.io/sf/reference/geos_unary.html)

- smoothness:

  smoothing factor, argument to
  [`smooth`](https://strimas.com/smoothr/reference/smooth.html)

- verbose:

  logical indicating to be verbose or not

- cleanup:

  logical to toggle removal of all intermediary files

## Value

brain-atlas class

## Examples

``` r
if (FALSE) { # \dontrun{

   label_file <- file.path(fs_subj_dir(), subject, "mri/aseg.mgz")
   slices = data.frame(x=130, y=130, z=130, view="axial", stringsAsFactors = FALSE)

   aseg2 <- make_volumetric_ggseg(
      label_file =  label_file,
      slices = slices
   )

   # Have a look at the atlas
   plot(aseg2)
} # }
```
