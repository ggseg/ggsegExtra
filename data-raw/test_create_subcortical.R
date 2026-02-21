devtools::load_all("../ggseg.formats")
devtools::load_all("../ggseg")
devtools::load_all("../ggseg3d")
devtools::load_all(".")
library(dplyr)
library(ggplot2)
options(freesurfer.verbose = FALSE)

# Use sequential for small test data (faster startup, no worker overhead)
future::plan(future::multicore(workers = 4))
progressr::handlers("cli")
progressr::handlers(global = TRUE)

output_dir <- "data-raw/subcortical_test"
unlink(output_dir, recursive = TRUE)
dir.create(output_dir, showWarnings = FALSE, recursive = TRUE)

folder <- "tests/testthat/testdata/volumetric"

# Test volume is 74x56x43 (cropped amygdala + thalamus from fsaverage5)
# Thalamus: X 12-65, Y 4-28, Z 4-40
# Amygdala: X 4-71, Y 32-53, Z 26-40
test_slices <- data.frame(
  x = c(37, 37, 50, 50),
  y = c(16, 42, 16, 42),
  z = c(20, 33, 20, 33),
  view = c("axial", "axial", "coronal", "coronal"),
  stringsAsFactors = FALSE
)

# Test 1: Subcortical 3D-only atlas ----
cli::cli_h1("Testing create_subcortical_from_volume (3D only)")

aseg_3d <- create_subcortical_from_volume(
  volume_file = file.path(folder, "aseg.mgz"),
  color_lut = file.path(folder, "lut.txt"),
  output_dir = file.path(output_dir, "subcortical_3d"),
  target_vertices = 200,
  include_geometry = FALSE,
  cleanup = FALSE,
  verbose = TRUE
)

print(aseg_3d)

p3d <- ggseg3d(atlas = aseg_3d, hemisphere = "subcort")
cli::cli_alert_success("3D rendering successful")

# Test 2: Subcortical full atlas (3D + 2D) ----
cli::cli_h1("Testing create_subcortical_from_volume (3D + 2D)")

aseg_full <- create_subcortical_from_volume(
  volume_file = file.path(folder, "aseg.mgz"),
  color_lut = file.path(folder, "lut.txt"),
  output_dir = file.path(output_dir, "subcortical_full"),
  target_vertices = 200,
  include_geometry = TRUE,
  slices = test_slices,
  cleanup = FALSE,
  verbose = TRUE
)

print(aseg_full)

p_full <- plot(aseg_full, alpha = 0.8)
ggsave(
  file.path(output_dir, "subcortical_full_2d.png"),
  p_full,
  width = 8,
  height = 6,
  bg = "white"
)
cli::cli_alert_success("Full subcortical 2D plot saved")

p3d_full <- ggseg3d(atlas = aseg_full, hemisphere = "subcort")
cli::cli_alert_success("Full subcortical 3D rendering successful")

cli::cli_h1("All subcortical tests passed")
