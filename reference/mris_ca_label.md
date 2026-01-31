# MRI ca label

For a single subject, produces an annotation file, in which each
cortical surface vertex is assigned a neuroanatomical label. This
automatic procedure employs data from a previously-prepared atlas file.
An atlas file is created from a training set, capturing region data
manually drawn by neuroanatomists combined with statistics on
variability correlated to geometric information derived from the
cortical model (sulcus and curvature). Besides the atlases provided with
FreeSurfer, new ones can be prepared using mris_ca_train).

## Usage

``` r
mris_ca_label(
  subject = "fsaverage5",
  hemisphere = "lh",
  canonsurf = "sphere.reg",
  classifier = file.path(fs_dir(), "average/lh.DKTatlas40.gcs"),
  output_file,
  subjects_dir = fs_subj_dir(),
  opts = NULL
)
```

## Arguments

- subject:

  Freesurfer subject, must exist in whatever subject directory specified
  or set in the environment with \$SUBJECTS_DIR

- hemisphere:

  hemisphere, either "rh" or "lh"

- canonsurf:

  canonical surface file. Ie: the name of the spherical surface file
  which describes the registration of a subject's vertices to the
  reference "average" surface. Example: sphere.reg

- classifier:

  specify classifier array input file (atlas file)

- output_file:

  path to output file

- subjects_dir:

  Freesurfer subject directory

- opts:

  other arguments to freesurfer command

## Examples

``` r
if (FALSE) { # \dontrun{
# for freesurfer help see:
freesurfer::fs_help("mris_ca_label")
mris_ca_label(output_file = "test.lh.annot")

mris_ca_label(hemisphere = "rh", output_file = "test.rh.annot")
} # }
```
