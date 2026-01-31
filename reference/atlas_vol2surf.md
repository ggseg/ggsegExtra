# Nifti volume to surface

Transform a Nifti volume to a surface file for FreeSurfer. Calls
FreeSurfer's `mri_vol2surf` for the transformation.

## Usage

``` r
atlas_vol2surf(input_file, output_dir, projfrac = 0.5)
```

## Arguments

- input_file:

  nifti volume

- output_dir:

  output directory path

- projfrac:

  value to `freesurfer mri_vol2surf` -projfrac

## Value

nothing, creates surface files
