# Extract mesh data from ply

.ply files contain a lot of information. for ggseg3d, we only need
information on the vertices and faces of the mesh. Thes function opens a
ply file, and organises the meshes and faces into a single list.

## Usage

``` r
get_mesh(ply, ...)
```

## Arguments

- ply:

  path to ply-file

- ...:

  arguments to
  [`geomorph::read.ply()`](https://rdrr.io/pkg/geomorph/man/read.ply.html)

## Value

list of meshes and faces

## Examples

``` r
if (FALSE) { # \dontrun{
get_mesh("path/to/surface.ply")

# Turn off showing the ply when reading
get_mesh("path/to/surface.ply", ShowSpecimen = FALSE)
} # }
```
