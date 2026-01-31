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
# # convert DKT to fsaverage5
# mri_surf2surf_rereg(subject = "bert",
#                     annot = "aparc.DKTatlas",
#                     hemi = "lh")
# 
# mri_surf2surf_rereg(subject = "bert",
#                     annot = "aparc.DKTatlas",
#                     hemi = "rh")

## -----------------------------------------------------------------------------
# # Make  3d ----
# dkt_3d <- make_aparc_2_3datlas(annot = "aparc.DKTatlas",
#                                output_dir = "~/Desktop/")

## -----------------------------------------------------------------------------
# dkt_3d

## -----------------------------------------------------------------------------
# ggseg3d(atlas = dkt_3d)

## -----------------------------------------------------------------------------
knitr::include_graphics("figures/dkt3d.png")

## -----------------------------------------------------------------------------
# unnest(dkt_3d, ggseg_3d)

## -----------------------------------------------------------------------------
# unnest(dkt_3d, ggseg_3d) |>
#   select(region) |>
#   unique() |>
#   unlist()

## -----------------------------------------------------------------------------
# unnest(dkt_3d, ggseg_3d) |>
#     mutate(region = gsub("temporal", " temporal", region),
#            region = gsub("caudal", "caudal ", region),
#            region = gsub("rostral", "rostral ", region),
#            region = gsub("middle", "middle ", region),
#            region = gsub("lateral", "lateral ", region),
#            region = gsub("medial", "medial ", region),
#            region = gsub("rior", "rior ", region),
#            region = gsub("cingulate", " cingulate", region),
#            # we've introduced some double whitespace, we don't want that
#            region = gsub("  ", " ", region)
#            ) |>
#     select(region) |>
#     unique() |>
#     unlist()

## -----------------------------------------------------------------------------
# unnest(dkt_3d, ggseg_3d) |>
#     mutate(region = gsub("temporal", " temporal", region),
#            region = gsub("caudal", "caudal ", region),
#            region = gsub("rostral", "rostral ", region),
#            region = gsub("middle", "middle ", region),
#            region = gsub("lateral", "lateral ", region),
#            region = gsub("medial", "medial ", region),
#            region = gsub("rior", "rior ", region),
#            region = gsub("cingulate", " cingulate", region),
#            # we've introduced some double whitespace, we don't want that
#            region = gsub("  ", " ", region),
# 
#            atlas = "dkt_3d"
#            )

## -----------------------------------------------------------------------------
# dkt_3d <- unnest(dkt_3d, ggseg_3d) |>
#     mutate(region = gsub("temporal", " temporal", region),
#            region = gsub("caudal", "caudal ", region),
#            region = gsub("rostral", "rostral ", region),
#            region = gsub("middle", "middle ", region),
#            region = gsub("lateral", "lateral ", region),
#            region = gsub("medial", "medial ", region),
#            region = gsub("rior", "rior ", region),
#            region = gsub("cingulate", " cingulate", region),
#            # we've introduced some double whitespace, we don't want that
#            region = gsub("  ", " ", region),
# 
#            atlas = "dkt_3d"
#            ) |>
#   nest_by(atlas, surf, hemi, .key = "ggseg_3d") |>
#   as_ggseg3d_atlas()
# 
# dkt_3d

## -----------------------------------------------------------------------------
# ggseg3d(atlas = dkt_3d, hemisphere = "left", surface = "inflated")
# ggseg3d(atlas = dkt_3d, hemisphere = "right", surface = "inflated")
# 
# ggseg3d(atlas = dkt_3d, hemisphere = "left", surface = "LCBC")
# ggseg3d(atlas = dkt_3d, hemisphere = "right", surface = "LCBC")

## -----------------------------------------------------------------------------
# # make atlas ----
# dkt <- make_ggseg3d_2_ggseg(dkt_3d,
#                             steps = 1:7,
#                             smoothness = 2,
#                             tolerance = .5,
#                             output_dir = "~/Desktop/")

## -----------------------------------------------------------------------------
# dkt

## -----------------------------------------------------------------------------
# plot(dkt)

## -----------------------------------------------------------------------------
knitr::include_graphics("figures/dkt.png")

## -----------------------------------------------------------------------------
# dkt$palette <- NULL
# dkt

## -----------------------------------------------------------------------------
# plot(dkt)

## -----------------------------------------------------------------------------
knitr::include_graphics("figures/dkt_nocol.png")

## -----------------------------------------------------------------------------
# library(ggsegExtra)
# library(ggseg)
# library(tidyverse)
# 
# # convert DKT to fsaverage5
# mri_surf2surf_rereg(subject = "bert",
#               annot = "aparc.DKTatlas",
#               hemi = "lh",
#               output_dir = "data-raw/")
# 
# mri_surf2surf_rereg(subject = "bert",
#               annot = "aparc.DKTatlas",
#               hemi = "rh",
#               output_dir = "data-raw/")
# 
# 
# # Make  3d ----
# dkt_3d <- make_aparc_2_3datlas(annot = "aparc.DKTatlas",
#                                annot_dir = "data-raw/",
#                                output_dir = "~/Desktop") |>
#     mutate(region = gsub("temporal", " temporal", region),
#            region = gsub("caudal", "caudal ", region),
#            region = gsub("rostral", "rostral ", region),
#            region = gsub("middle", "middle ", region),
#            region = gsub("lateral", "lateral ", region),
#            region = gsub("medial", "medial ", region),
#            region = gsub("rior", "rior ", region),
#            region = gsub("cingulate", " cingulate", region),
#            # we've introduced some double whitespace, we don't want that
#            region = gsub("  ", " ", region),
# 
#            atlas = "dkt_3d"
#            ) |>
#   nest_by(atlas, surf, hemi, .key = "ggseg_3d") |>
#   as_ggseg3d_atlas()
# 
# # make 2d ----
# dkt <- make_ggseg3d_2_ggseg(dkt_3d,
#                             steps = 1:7,
#                             smoothness = 2,
#                             tolerance = .5,
#                             output_dir = "~/Desktop/")
# plot(dkt)

