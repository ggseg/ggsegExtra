devtools::load_all(".")
library(dplyr)
options(freesurfer.verbose = FALSE)
progressr::handlers(global = TRUE)

future::plan(
  future::multisession,
  workers = future::availableCores() - 2
)

# Subcortical/volumetric atlas creation ----
# Uses slice snapshots from volumetric data (no mesh creation)

aseg <- make_ggseg_atlas_volumetric(
  label_file = file.path(
    freesurfer::fs_subj_dir(),
    "fsaverage5",
    "mri/aseg.mgz"
  ),
  subject = "fsaverage5",
  subjects_dir = freesurfer::fs_subj_dir(),
  output_dir = "~/Desktop/aseg_test",
  steps = 6:8,
  skip_existing = FALSE,
  tolerance = 0.6,
  vertex_size_limits = c(10, NA),
  cleanup = FALSE
)

# Filter out unwanted regions
aseg$data <- aseg$data |>
  filter(
    !grepl("Unknown", label, ignore.case = TRUE),
    !grepl("White-matter", label, ignore.case = TRUE)
  ) |>
  mutate(region = ifelse(grepl("cortex", region), NA, region))

# Test 2D rendering
plot(aseg, alpha = 0.8)

# Check the data structure
str(aseg$data)
head(aseg$data)
