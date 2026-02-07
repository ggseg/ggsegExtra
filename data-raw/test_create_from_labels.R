devtools::load_all("../ggseg.formats")
devtools::load_all("../ggseg")
devtools::load_all("../ggseg3d")
devtools::load_all(".")
library(dplyr)
library(ggplot2)

future::plan(future::multisession(workers = 4))
progressr::handlers("cli")
progressr::handlers(global = TRUE)

# Test creating atlas from label files ----

label_dir <- file.path(freesurfer::fs_subj_dir(), "fsaverage5/label")

# Get Brodmann area labels
labels <- list.files(
  label_dir,
  pattern = "BA[0-9].*\\.label$",
  full.names = TRUE
)
cat("Found", length(labels), "BA label files\n")

# Option 1: Create 3D-only atlas (fast)
ba_atlas <- create_atlas_from_labels(
  labels,
  atlas_name = "brodmann_areas",
  include_geometry = FALSE
)

print(ba_atlas)

# Check vertices are loaded
cat("\nVertices per region:\n")
vertices_df <- ba_atlas$data$vertices
for (i in seq_len(min(5, nrow(vertices_df)))) {
  n_verts <- length(vertices_df$vertices[[i]])
  cat(vertices_df$label[i], ":", n_verts, "vertices\n")
}

# Test 3D rendering
ggseg3d::ggseg3d(atlas = ba_atlas, hemisphere = "left")

# Option 2: With custom colours
ba_atlas_custom <- create_atlas_from_labels(
  labels[1:4],
  atlas_name = "ba_custom",
  colours = c("#E74C3C", "#3498DB", "#2ECC71", "#F39C12"),
  include_geometry = FALSE
)

print(ba_atlas_custom)
ggseg3d::ggseg3d(atlas = ba_atlas_custom)
