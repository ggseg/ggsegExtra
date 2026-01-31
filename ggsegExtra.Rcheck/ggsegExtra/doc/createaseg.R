## -----------------------------------------------------------------------------
knitr::opts_chunk$set(
  collapse = TRUE,
  comment = "#>",
  eval = FALSE,
  fig.retina = 3
)

## -----------------------------------------------------------------------------
# library(ggsegExtra)
# library(ggseg)
# library(ggseg3d)
# library(tidyverse) # for cleaning the atlas data efficiently

## -----------------------------------------------------------------------------
# # Make  3d ----
# aseg2_3d <- make_volumetric_2_3datlas(
#   subject = "fsaverage5",
#   subjects_dir = freesurfer::fs_subj_dir(),
#   template = file.path(freesurfer::fs_subj_dir(), "fsaverage5/mri/aseg.mgz"),
#   output_dir = "~/Desktop/aseg"
# )

## -----------------------------------------------------------------------------
# ggseg3d(atlas = aseg2_3d)

## -----------------------------------------------------------------------------
knitr::include_graphics("figures/aseg_raw.png")

## -----------------------------------------------------------------------------
# aseg2_3d

## -----------------------------------------------------------------------------
# unnest(aseg2_3d, ggseg_3d)

## -----------------------------------------------------------------------------
# unnest(aseg2_3d, ggseg_3d) |>
#   select(region) |>
#   unique() |>
#   unlist()

## -----------------------------------------------------------------------------
# unnest(aseg2_3d, ggseg_3d) |>
#     mutate(region = gsub("-|_", " ", region),
#            region = tolower(region),
#            region = gsub("cc ", "corpus callosum ", region)
#            ) |>
#     select(region) |>
#     unique() |>
#     unlist()

## -----------------------------------------------------------------------------
# unnest(aseg2_3d, ggseg_3d) |>
#     mutate(region = gsub("-|_", " ", region),
#            region = tolower(region),
#            region = gsub("cc ", "corpus callosum ", region),
# 
#            atlas = "aseg2_3d"
#            )

## -----------------------------------------------------------------------------
# aseg2_3d <- unnest(aseg2_3d, ggseg_3d) |>
#     mutate(region = gsub("-|_", " ", region),
#            region = tolower(region),
#            region = gsub("cc ", "corpus callosum ", region),
# 
#            atlas = "aseg2_3d"
#     ) |>
#     filter(!grepl("Cortex|White-Matter", label))

## -----------------------------------------------------------------------------
# ggseg3d(atlas = aseg2_3d)

## -----------------------------------------------------------------------------
knitr::include_graphics("figures/aseg.png")

## -----------------------------------------------------------------------------
# aseg2_3d <- aseg2_3d |>
#   nest_by(atlas, surf, hemi, .key = "ggseg_3d") |>
#   as_ggseg3d_atlas()
# 
# aseg2_3d

## -----------------------------------------------------------------------------
# ggseg3d(atlas = aseg2_3d) |>
#   add_glassbrain()

## -----------------------------------------------------------------------------
knitr::include_graphics("figures/aseg_glass.png")

## -----------------------------------------------------------------------------
# slices <- data.frame(x = c(130, 122, 122),
#            y = c(130, 235, 112),
#            z = c(130, 100, 106),
#            view = c("axial", "sagittal", "coronal"),
#            stringsAsFactors = FALSE)
# slices

## -----------------------------------------------------------------------------
# # make atlas ----
# aseg2 <- make_volumetric_ggseg(
#   label_file = file.path(freesurfer::fs_subj_dir(), "fsaverage5", "mri/aseg.mgz"),
#   slices = slices,
#   vertex_size_limits = c(10, NA)
# )

## -----------------------------------------------------------------------------
# aseg2

## -----------------------------------------------------------------------------
# plot(aseg2)

## -----------------------------------------------------------------------------
knitr::include_graphics("figures/aseg2_first.png")

## -----------------------------------------------------------------------------
# plot(aseg2, alpha = .5)

## -----------------------------------------------------------------------------
knitr::include_graphics("figures/aseg2_second.png")

## -----------------------------------------------------------------------------
# aseg2$data

## -----------------------------------------------------------------------------
# aseg2$data <- filter(aseg2$data,
#                      !grepl("Unknown", label, ignore.case = TRUE),
#                      !grepl("White-matter", label, ignore.case = TRUE)
# ) |>
#   mutate(region = ifelse(grepl("cortex", region), NA, region))
# 
# aseg2

## -----------------------------------------------------------------------------
# plot(aseg2)

## -----------------------------------------------------------------------------
knitr::include_graphics("figures/aseg2_complete.png")

## -----------------------------------------------------------------------------
# aseg2$palette <- NULL
# aseg2

## -----------------------------------------------------------------------------
# plot(aseg2)

## -----------------------------------------------------------------------------
knitr::include_graphics("figures/aseg2_nocol.png")

## -----------------------------------------------------------------------------
# library(ggsegExtra)
# library(ggseg)
# library(tidyverse)
# 
# # Make  3d ----
# aseg2_3d <- make_aseg_2_3datlas(
#   subject = "fsaverage5",
#   subjects_dir = freesurfer::fs_subj_dir(),
#   template = file.path(freesurfer::fs_subj_dir(), "fsaverage5", "mri/aseg.mgz"),
#   output_dir = "~/Desktop/aseg"
# )
# 
# ggseg3d(atlas = aseg2_3d)
# 
# # make atlas ----
# aseg2 <- make_volumetric_ggseg(subject = "fsaverage5",
#                                steps = 1:8,
#                                output_dir = "~/Desktop/aseg",
#                                vertex_size_limits = c(10, NA),
#                                label_file = file.path(freesurfer::fs_subj_dir(), "fsaverage5", "mri/aseg.mgz"))
# 
# aseg2$data <- filter(aseg2$data,
#                      !grepl("Unknown", label, ignore.case = TRUE),
#                      !grepl("White-matter", label, ignore.case = TRUE)
# ) |>
#   mutate(region = ifelse(grepl("cortex", region), NA, region))
# plot(aseg2, alpha = .8)

## -----------------------------------------------------------------------------
knitr::include_graphics("figures/aseg2_complete.png")

