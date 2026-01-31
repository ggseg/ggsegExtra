## -----------------------------------------------------------------------------
knitr::opts_chunk$set(
  collapse = TRUE,
  out.width = "100%",
  fig.width = 10
)
has_dkt <- requireNamespace("ggsegDKT", quietly = TRUE)

## -----------------------------------------------------------------------------
library(ggsegExtra)

## -----------------------------------------------------------------------------
ggseg_atlas_repos()

# Search for repos with pattern
ggseg_atlas_repos("Yeo")

# Search is case-sensitive
ggseg_atlas_repos("yeo")

# Search is case-sensitive, but this can be fixed
ggseg_atlas_repos("yeo", ignore.case = TRUE)

## -----------------------------------------------------------------------------
# ggseg_atlas_repos("dkt", ignore.case = TRUE)
# install_ggseg_atlas("ggsegDKT")

## -----------------------------------------------------------------------------
library(ggplot2)
library(ggseg)

ggplot() +
  geom_brain(atlas = ggsegDKT::dkt)

## -----------------------------------------------------------------------------
library(ggplot2)
library(ggseg)
library(ggsegDKT)

ggplot() +
  geom_brain(atlas = dkt)

## -----------------------------------------------------------------------------
ggplot() +
  geom_brain(atlas = dkt) +
  scale_fill_brain("dkt", package = "ggsegDKT")

## -----------------------------------------------------------------------------
# library(ggseg3d)
# 
# ggseg3d(atlas = dkt_3d) |>
#   pan_camera("right lateral")

## -----------------------------------------------------------------------------
knitr::include_graphics("figures/vignette-dkt-3d-plot.png")

