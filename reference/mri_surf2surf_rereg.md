# Re-register an annotation file

Annotation files are subject specific. Most are registered for
fsaverage, but we recommend using fsaverage5 for the mesh plots in
ggseg3d, as these contain a decent balance in number of vertices for
detailed rendering and speed.

## Usage

``` r
mri_surf2surf_rereg(
  subject,
  annot,
  hemi = c("lh", "rh"),
  target_subject = "fsaverage5",
  output_dir = file.path(fs_subj_dir(), subject, "label"),
  verbose = TRUE
)
```

## Arguments

- subject:

  subject the original annotation file is registered to

- annot:

  annotation file name (as found in subjects_dir)

- hemi:

  hemisphere (one of "lh" or "rh")

- target_subject:

  subject to re-register the annotation (default fsaverage5)

- output_dir:

  output directory path

- verbose:

  logical indicating to be verbose or not

## Value

nothing

## Examples

``` r
if (FALSE) { # \dontrun{
# For help see:
freesurfer::fs_help("mri_surf2surf")

mri_surf2surf_rereg(subject = "bert",
                    annot = "aparc.DKTatlas",
                    target_subject = "fsaverage5")
} # }
```
