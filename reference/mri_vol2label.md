# Convert volume to label

Converts values in a volume or surface overlay to a label. The program
searches the input for values equal to labelid. The xyz values for each
point are then computed based on the tkregister voxel-to-RAS matrix
(volume) or from the xyz of the specified surface. The xyz values are
then stored in labelfile in the label file format. The statistic value
is set to 0. While this program can be used with any mri volume, it was
designed to convert parcellation volumes, which happen to be stored in
mri format. Calls FreeSurfer's `mri_vol2label`.

## Usage

``` r
mri_vol2label(
  input_file,
  label_id,
  hemisphere = "rh",
  output_dir,
  surface = NULL,
  subject = "fsaverage5",
  subjects_dir = fs_subj_dir(),
  opts = NULL,
  verbose = TRUE
)
```

## Arguments

- input_file:

  input volume

- label_id:

  label to run

- hemisphere:

  hemisphere, either "rh" or "lh"

- output_dir:

  output directory path

- surface:

  output surface

- subject:

  Freesurfer subject, must exist in whatever subject directory specified
  or set in the environment with \$SUBJECTS_DIR

- subjects_dir:

  Freesurfer subject directory

- opts:

  other arguments to freesurfer command

- verbose:

  logical indicating to be verbose or not

## Value

returns nothing. Writes a label file.

## Examples

``` r
if (FALSE) { # \dontrun{
# for freesurfer help see:
freesurfer::fs_help("mri_vol2label")

out_dir <- tempdir()
vol <- file.path(freesurfer::fs_subj_dir(),
                 "fsaverage5/mri/aseg.mgz")

mri_vol2label(vol,
     label_id = 2,
     hemisphere = "rh",
     output_dir = out_dir)

 # delete output dir when not needed
 unlink(out_dir)
} # }
```
