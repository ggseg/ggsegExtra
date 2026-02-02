devtools::load_all(".")
library(dplyr)
progressr::handlers(global = TRUE)

future::plan(
  future::multisession,
  workers = future::availableCores() - 2
)

# Cortical atlas creation using new vertex-based pipeline ----
atlas <- "Yeo2011_7Networks_N1000"

# Option 1: Create 3D-only atlas (fast, no screenshots needed)
atlas_3d_only <- make_brain_atlas(
  annot = atlas,
  subject = "fsaverage5",
  include_geometry = FALSE
)

# Test 3D rendering
ggseg3d::ggseg3d(atlas = atlas_3d_only)

# Option 2: Create full atlas with 2D geometry
atlas_full <- make_brain_atlas(
  annot = atlas,
  subject = "fsaverage5",
  include_geometry = TRUE,
  output_dir = "~/Desktop/dkt_test",
  smoothness = 2,
  tolerance = 0.8,
  view = c("medial", "lateral"),
  cleanup = FALSE
)

# Test 2D rendering
plot(atlas_full, show.legend = FALSE)

# Test 3D rendering
ggseg3d::ggseg3d(atlas = atlas_full)

# Check the data structure
str(atlas_full$data)
head(atlas_full$data)
