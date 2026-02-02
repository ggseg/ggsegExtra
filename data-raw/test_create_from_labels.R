devtools::load_all(".")
library(dplyr)

# Test creating atlas from label files ----

label_dir <- file.path(freesurfer::fs_subj_dir(), "fsaverage5/label")

# Get Brodmann area labels
labels <- list.files(label_dir, pattern = "BA[0-9].*\\.label$", full.names = TRUE)
cat("Found", length(labels), "BA label files\n")

# Option 1: Create 3D-only atlas (fast)
ba_atlas <- make_atlas_from_labels(
  labels,
  atlas_name = "brodmann_areas",
  include_geometry = FALSE
)

print(ba_atlas)

# Check vertices are loaded
cat("\nVertices per region:\n")
for (i in 1:min(5, nrow(ba_atlas$data))) {
  cat(ba_atlas$data$label[i], ":", length(ba_atlas$data$vertices[[i]]), "vertices\n")
}

# Test 3D rendering
ggseg3d::ggseg3d(atlas = ba_atlas, hemisphere = "left")

# Option 2: With custom colours
ba_atlas_custom <- make_atlas_from_labels(
  labels[1:4],
  atlas_name = "ba_custom",
  colours = c("#E74C3C", "#3498DB", "#2ECC71", "#F39C12"),
  include_geometry = FALSE
)

print(ba_atlas_custom)
ggseg3d::ggseg3d(atlas = ba_atlas_custom)
