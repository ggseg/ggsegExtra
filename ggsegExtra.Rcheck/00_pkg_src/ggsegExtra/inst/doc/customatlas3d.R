## -----------------------------------------------------------------------------
knitr::opts_chunk$set(
  collapse = TRUE,
  eval = FALSE,
  comment = "#>",
  fig.retina = 3
)

## -----------------------------------------------------------------------------
# # convert bert's DKT to fsaverage5
# mri_surf2surf_rereg(subject = "bert",
#               annot = "aparc.DKTatlas",
#               hemi = "lh",
#               output_dir = "/path/to/where/you/want/it")
# 
# mri_surf2surf_rereg(subject = "bert",
#               annot = "aparc.DKTatlas",
#               hemi = "rh",
#               output_dir = "/path/to/where/you/want/it")

## -----------------------------------------------------------------------------
# # Desikan-Killiany atlas (a.k.a aparc)
# dt <- make_aparc_2_3datlas()
# 
# # Desterieux atlas
# dt <- make_aparc_2_3datlas(annot = "aparc.a2009s")
# 
# # Yeo 7 networks atlas
# dt <- make_aparc_2_3datlas(annot = "Yeo2011_7Networks_N1000")

## -----------------------------------------------------------------------------
# # myResults atlas
# dt <- make_aparc_2_3datlas(annot = "myResults",
#                        annot_dir = "~/Desktop")

## -----------------------------------------------------------------------------
# # convert DKT to fsaverage5
# mri_surf2surf_rereg(subject = "bert",
#                     annot = "aparc.DKTatlas",
#                     hemi = "lh",
#                     output_dir = "~/Desktop/")
# 
# mri_surf2surf_rereg(subject = "bert",
#                     annot = "aparc.DKTatlas",
#                     hemi = "rh",
#                     output_dir = "~/Desktop/")

## -----------------------------------------------------------------------------
# library(dplyr)
# library(tidyr)
# 
# # Make  3d ----
# dkt_3d <- make_aparc_2_3datlas(annot = "aparc.DKTatlas",
#                                annot_dir = "~/Desktop/",
#                                subjects_dir = freesurfer::fs_subj_dir(),
#                                output_dir = "~/Desktop/") |>
#   mutate(atlas = "dkt_3d")|>
#   unnest(ggseg_3d) |>
#   select(-region) |>
#   left_join(select(ggseg::dk$data, hemi, region, label)) |>
#   nest_by(atlas, surf, hemi, .key = "ggseg_3d") |>
#   as_ggseg3d_atlas()

## -----------------------------------------------------------------------------
# # aseg atlas (a.k.a aseg)
# dt <- make_aseg_2_3datlas()

## -----------------------------------------------------------------------------
# # myResults template without a color LUT
# dt <- make_aparc_2_3datlas(template = "path/to/myResults.mgz",
#                        color_lut = NULL)
# 
# # myResults template with a color LUT
# dt <- make_aparc_2_3datlas(template = "path/to/myResults.mgz",
#                        color_lut = "path/to/myResults_LUT.txt")

