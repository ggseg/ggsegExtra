library(ggseg.extra)
library(dplyr)
library(ggplot2)

# Use sequential for faster startup on test data
future::plan(future::multicore(workers = 4))
progressr::handlers("cli")
progressr::handlers(global = TRUE)

# Cortical atlas creation using new vertex-based pipeline ----
# Uses Yeo7 annotation files from testdata (copied from fsaverage5)
atlas <- "yeo7"
input_dir <- "tests/testthat/testdata/cortical"


fsaverage5 <- file.path(
  freesurfer::fs_subj_dir(),
  "fsaverage"
)

annots <- list.files(
  file.path(
    fsaverage5,
    "label"
  ),
  "Yeo2011_7Networks_N1000",
  full.names = TRUE
)
atlas <- create_cortical_from_annotation(
  annots,
  atlas_name = atlas,
  output_dir = "data-raw",
  tolerance = 1,
  cleanup = FALSE
)

# Check atlas structure
print(atlas)

# Test 2D rendering
p <- plot(atlas, show.legend = FALSE)
ggsave("data-raw/yeo7_test.png", p, width = 10, height = 8, bg = "white")
cli::cli_alert_success("2D plot saved to data-raw/yeo7_test.png")

# Test 3D rendering
p3d <- ggseg3d::ggseg3d(atlas = atlas, hemisphere = "left")
cli::cli_alert_success("3D rendering successful")

# Check the data structure
cat("\nCore:\n")
print(atlas$core)
cat("\nSF data:\n")
print(head(atlas$data$sf))
