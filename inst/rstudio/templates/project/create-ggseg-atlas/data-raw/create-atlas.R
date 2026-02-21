# Create {GGSEG} atlas for the ggseg ecosystem

library(ggseg.extra)
library(dplyr)

future::plan(future::sequential)
progressr::handlers("cli")
progressr::handlers(global = TRUE)

# =============================================================================
# STEP 1: Define your atlas source
# =============================================================================
# Choose ONE of the following methods depending on your atlas source.

# METHOD A: From FreeSurfer annotation files (most common)
# --------------------------------------------------------
# Provide paths to lh and rh annotation files.
# Standard fsaverage5 annotations live under $FREESURFER_HOME/subjects/fsaverage5/label/

annot_files <- c(
  file.path(freesurfer::fs_subj_dir(), "fsaverage5", "label", "lh.{GGSEG}.annot"),
  file.path(freesurfer::fs_subj_dir(), "fsaverage5", "label", "rh.{GGSEG}.annot")
)

# If your annotation is on fsaverage (not fsaverage5), convert first:
# lapply(c("lh", "rh"), function(hemi) {
#   mri_surf2surf_rereg(
#     subject = "fsaverage",
#     annot = "{GGSEG}",
#     hemi = hemi,
#     output_dir = here::here("data-raw")
#   )
# })
# annot_files <- list.files(here::here("data-raw"), "\\.annot$", full.names = TRUE)


# METHOD B: From label files
# --------------------------
# label_files <- list.files(
#   here::here("data-raw"), pattern = "\\.label$", full.names = TRUE
# )


# =============================================================================
# STEP 2: Create the atlas
# =============================================================================
# steps = 1  -> 3D only (fast, no screenshots needed)
# steps = NULL -> full pipeline including 2D geometry (needs FreeSurfer + ImageMagick)

# METHOD A: From annotation
{GGSEG} <- create_cortical_from_annotation(
  input_annot = annot_files,
  atlas_name = "{GGSEG}",
  output_dir = here::here("data-raw"),
  steps = NULL,
  skip_existing = TRUE,
  cleanup = FALSE,
  verbose = TRUE
)

# METHOD B: From label files
# {GGSEG} <- create_cortical_from_labels(
#   label_files = label_files,
#   atlas_name = "{GGSEG}",
#   output_dir = here::here("data-raw"),
#   steps = NULL,
#   skip_existing = TRUE,
#   cleanup = FALSE,
#   verbose = TRUE
# )

# =============================================================================
# STEP 3: Clean up region names (if needed)
# =============================================================================

{GGSEG}$core <- {GGSEG}$core |>
  mutate(
    region = gsub("_L$|_R$|_lh$|_rh$", "", region),
    region = if_else(
      grepl("unknown|wall|\\?|corpus.callosum", region, ignore.case = TRUE),
      NA_character_,
      region
    )
  )


# =============================================================================
# STEP 4: Verify the atlas
# =============================================================================

print({GGSEG})

plot({GGSEG})

if (interactive()) {
  ggseg3d::ggseg3d(atlas = {GGSEG}, hemisphere = "left")
}


# =============================================================================
# STEP 5: Save the atlas as internal data
# =============================================================================

.{GGSEG} <- {GGSEG}
usethis::use_data(.{GGSEG}, internal = TRUE, overwrite = TRUE, compress = "xz")
