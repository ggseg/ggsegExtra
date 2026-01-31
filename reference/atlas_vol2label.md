# Volume to label

Turn volumetric files into labels for use in annotations. Calls
FreeSurfer's `mri_vol2label`.

## Usage

``` r
atlas_vol2label(annot_lab, output_dir, verbose)
```

## Arguments

- annot_lab:

  annotation label

- output_dir:

  output directory path

- verbose:

  logical indicating to be verbose or not

## Value

invisibly returns the list of labels.
