# Split surface file into separate plys

Function to split up a surface ply into several ply's based on a
freesurfer label file.

## Usage

``` r
surfsplit(
  srf_ply,
  label_path,
  prefix = "test",
  output_dir = dirname(label_path),
  verbose = TRUE
)
```

## Arguments

- srf_ply:

  Surface ply path

- label_path:

  label dpv file

- prefix:

  output prefix

- output_dir:

  output directory path

- verbose:

  logical indicating to be verbose or not

## Value

list of ply meshes. Will also write ply meshes to files.

## Details

Script adapted to R and ggseg based on scripts from Anderson M. Winkler:
http://brainder.org
