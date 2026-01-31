# System Setup

``` r
library(ggsegExtra)
#> Loading required package: ggseg
#> Loading required package: ggseg3d
#> Warning in rgl.init(initValue, onlyNULL): RGL: unable to open X11 display
#> Warning: 'rgl.init' failed, will use the null device.
#> See '?rgl.useNULL' for ways to avoid this warning.
```

## FreeSurfer

Creating custom atlases requires
[FreeSurfer](https://surfer.nmr.mgh.harvard.edu/fswiki/DownloadAndInstall)
to be installed and configured.

On macOS, you also need:

- [XQuartz](https://www.xquartz.org/)
- [Xcode](https://developer.apple.com/xcode/) command line tools

## Plotly screenshots

For taking screenshots of 3D plotly brain visualizations, ggsegExtra
uses the webshot2 package, which requires Google Chrome or Chromium.

``` r
install.packages("webshot2")
```

Chrome/Chromium is typically already installed. If not, webshot2 will
attempt to download a suitable version automatically.

## Parallel processing

Atlas creation can be slow, especially for cortical atlases with many
regions. ggsegExtra uses the [furrr](https://furrr.futureverse.org/)
package for parallel processing. By default, processing runs
sequentially. To enable parallel processing, set up a `future` plan
before running atlas creation functions:

``` r
library(future)

# Use all available cores
plan(multisession)

# Or specify number of workers
plan(multisession, workers = 4)
```

To return to sequential processing:

``` r
plan(sequential)
```

## Progress bars

ggsegExtra uses the [progressr](https://progressr.futureverse.org/)
package to report progress during long-running operations. Progress
reporting is disabled by default. To enable progress bars, wrap your
code in
[`with_progress()`](https://progressr.futureverse.org/reference/with_progress.html)
or set a global handler:

``` r
library(progressr)

# Option 1: Wrap specific code
with_progress({
  atlas <- make_ggseg3d_2_ggseg(...)
})

# Option 2: Enable globally for the session
handlers(global = TRUE)
```

You can customize the progress bar style:

``` r
# Use a simple text progress bar
handlers("txtprogressbar")

# Use cli-style progress (recommended)
handlers("cli")
```
