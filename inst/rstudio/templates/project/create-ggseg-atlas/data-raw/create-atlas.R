# Create {GGSEG} atlas for the ggseg ecosystem
# Creates a ggseg_atlas for ggseg and ggseg3d

library(ggsegExtra)
library(dplyr)

# =============================================================================
# STEP 1: Define your atlas source
# =============================================================================
# Choose ONE of the following methods depending on your atlas source:

# METHOD A: From FreeSurfer annotation file (most common)
# --------------------------------------------------------
# Place annotation files in data-raw/ named: lh.{GGSEG}.annot, rh.{GGSEG}.annot
# Or use standard FreeSurfer annotations: "aparc", "aparc.DKTatlas", etc.

annot_name <- "{GGSEG}"

# If your annotation is in a custom location:
# annot_dir <- here::here("data-raw")

# Convert from fsaverage to fsaverage5 (smaller file size):
# lapply(c("lh", "rh"), function(hemi) {
#   ggsegExtra::mri_surf2surf_rereg(
#     subject = "fsaverage",
#     annot = annot_name,
#     hemi = hemi,
#     output_dir = here::here("data-raw/fsaverage5/")
#   )
# })

# METHOD B: From label files
# --------------------------
# Place your .label files in data-raw/
# label_files <- list.files(
#   here::here("data-raw"), pattern = "\\.label$", full.names = TRUE
# )


# =============================================================================
# STEP 2: Create the atlas
# =============================================================================

# METHOD A: From annotation (includes 2D geometry)
{GGSEG} <- ggsegExtra::create_cortical_atlas(
  annot = annot_name,
  subject = "fsaverage5",
  include_geometry = TRUE,
  verbose = TRUE
)

# METHOD B: From label files (3D only, faster)
# {GGSEG} <- ggsegExtra::create_atlas_from_labels(
#   label_files = label_files,
#   atlas_name = "{GGSEG}",
#   type = "cortical",
#   include_geometry = FALSE,
#   verbose = TRUE
# )

# =============================================================================
# STEP 3: Clean up region names (if needed)
# =============================================================================
# Modify region names in the core metadata

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

# Check structure
print({GGSEG})

# Preview 2D plot
ggseg::ggseg(atlas = {GGSEG}, show.legend = FALSE)

# Preview 3D plot (if interactive)
if (interactive()) {
  ggseg3d::ggseg3d(atlas = {GGSEG}, hemisphere = "left")
}


# =============================================================================
# STEP 5: Save the atlas
# =============================================================================

usethis::use_data({GGSEG}, overwrite = TRUE, compress = "xz")

message("Atlas saved to data/{GGSEG}.rda")
message("Don't forget to:")
message("1. Update R/data.R with proper documentation")
message("2. Update the README with atlas citation")
message("3. Run the hex logo script: source('data-raw/create-hex-logo.R')")
