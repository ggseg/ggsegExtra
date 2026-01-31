# List all online repositories with ggseg-atlases

Function to easily find all online repositories that contain ggseg
atlases

## Usage

``` r
ggseg_atlas_repos(pattern = NULL, ...)
```

## Arguments

- pattern:

  string pattern to search repos

- ...:

  additional arguments to
  [`base::grep()`](https://rdrr.io/r/base/grep.html)

## Value

data frame of online repositories with ggseg-atlases

## Examples

``` r
if (FALSE) { # \dontrun{
ggseg_atlas_repos()

ggseg_atlas_repos("yeo")
} # }
```
