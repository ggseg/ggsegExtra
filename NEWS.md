# ggsegExtra 1.6

## 1.6.0

- Removed rgdal dependency, replaced with sf/terra (#49, #59)
- Fixed r-universe API calls (JSON array format change)
- Fixed vignette build issues with conditional evaluation for suggested packages
- Replaced reticulate/kaleido with webshot2 for plotly screenshots
- Updated system setup vignette with new requirements
- Added documentation for parallel processing and progress bars
- Added note about freesurfer dev version requirement
- Updated CITATION to use bibentry()
- Fixed mris_label2annot example documentation

# ggsegExtra 1.5

## 1.5.33.003

- small bug fix that prevented calls to FreeSurfer
- Possibility to initiate new atlas project from the RStudio Project GUI

## ggsegExtra 1.5.33

- removes purrr dependency
- used ggseg [r-universe](https://ggseg.r-universe.dev/#builds) as install repo for install functions

## ggsegExtra 1.5.32

- non-standard columns in 3d atlas are retained in 2d atlas
- Freesurfer annotation file custom S3 class implemented
- progressbar for region snapshots

## ggsegExtra 1.5.3

- Added pipeline functions for:
  - creating ggseg3d-atlas from annotation files
  - creating ggseg3d-atlas from volumetric files
  - creating ggseg-atlas from cortical ggseg3d-atlas
  - creating ggseg-atlas from volumetric files
- Added a `NEWS.md` file to track changes to the package.
