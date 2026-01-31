## -----------------------------------------------------------------------------
knitr::opts_chunk$set(
collapse = TRUE,
comment = "#>",
eval = FALSE
)
dt <- ggseg::dk

## -----------------------------------------------------------------------------
# dt <- make_ggseg3d_2_ggseg(
#   ggseg3d_atlas = dk_3d,
#   steps = 1:7,
#   output_dir = tempdir(),
#   smoothness = 5,
#   tolerance = 0,
#   cleanup = FALSE,
#   verbose = TRUE)

## -----------------------------------------------------------------------------
# ggplot() +
#   geom_brain(atlas = dt)

## -----------------------------------------------------------------------------
# # make atlas ----
# dkt <- make_ggseg3d_2_ggseg(dkt_3d,
#                             steps = 1:7,
#                             smoothness = 2,
#                             tolerance = .5,
#                             output_dir = "~/Desktop")
# 
# # check that it looks ok.
# plot(dkt)

