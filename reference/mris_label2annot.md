# Convert Label to Annotation

If you have labels rather than a full annotation file, these can be
combined with FreeSurfer's `mris_label2annot`.

## Usage

``` r
mris_label2annot(
  labels,
  hemisphere = "rh",
  ctab,
  subject = "fsaverage5",
  subjects_dir = fs_subj_dir(),
  output_dir = subjects_dir,
  opts = NULL,
  verbose = TRUE
)
```

## Arguments

- labels:

  label file path vector

- hemisphere:

  hemisphere, either "rh" or "lh"

- ctab:

  colourtable file

- subject:

  Freesurfer subject, must exist in whatever subject directory specified
  or set in the environment with \$SUBJECTS_DIR

- subjects_dir:

  Freesurfer subject directory

- output_dir:

  output directory path

- opts:

  other arguments to freesurfer command

- verbose:

  logical indicating to be verbose or not

## Examples

``` r
if (FALSE) { # \dontrun{
# for freesurfer help see:
freesurfer::fs_help("mris_label2annot")

subj_dir <- freesurfer::fs_subj_dir()
# Split up aparc annot into labels
mri_annotation2label(annot_name = "aparc")

# get annot for colour labels
annot <- freesurfer::read_annotation(
              file.path(subj_dir,
                        "fsaverage5/label/rh.aparc.annot"))

labels <- list.files(
   file.path(subj_dir, "fsaverage5/label/aparc"),
    full.names = TRUE)

# Write colortable to file
ctab_file <- tempfile(fileext = ".ctab")
write_ctab(annot$colortable, ctab_file)

# Combine labels back into annotation
mris_label2annot(labels, hemisphere = "rh", ctab = ctab_file)
} # }
```
