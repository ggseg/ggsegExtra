## ----"setup", include = FALSE-------------------------------------------------
knitr::opts_chunk$set(
  collapse = TRUE,
  out.width = "100%",
  fig.width = 10
)
devtools::load_all(".")

## ---- eval=FALSE--------------------------------------------------------------
#  library(ggsegExtra)

## -----------------------------------------------------------------------------
ggseg_atlas_repos()

# Search for repos with pattern
ggseg_atlas_repos("Yeo")

# Search is case-sensitive
ggseg_atlas_repos("yeo")

# Search is case-sensitive, but this can be fixed
ggseg_atlas_repos("yeo", ignore.case = TRUE)

## ---- message = FALSE---------------------------------------------------------
yeo_repo <- ggseg_atlas_repos("yeo", ignore.case = TRUE)
install_ggseg_atlas(repo = yeo_repo$repo, source = yeo_repo$source)

## -----------------------------------------------------------------------------
# Load the atlas directly
ggseg(atlas = ggsegYeo2011::yeo7)

## ----"ggseg"------------------------------------------------------------------
# load on all atlases and palettes from the ggsegYeo2011 library
library(ggsegYeo2011)

ggseg(atlas = yeo7)
ggseg(atlas = yeo17)

## ----"ggseg-scale"------------------------------------------------------------
ggseg(atlas = yeo7, 
      mapping=aes(fill = region)) +
  scale_fill_brain("yeo7", package="ggsegYeo2011")

## ----"noneval1", eval=FALSE---------------------------------------------------
#  library(ggseg3d)
#  
#  ggseg3d(atlas = yeo7_3d) %>%
#    pan_camera("right lateral")

## ----"incl1", echo=FALSE------------------------------------------------------
knitr::include_graphics("figures/vignette-yeo7-3d-plot.png")

## ----"noneval2", eval=FALSE---------------------------------------------------
#  ggseg3d(atlas = yeo17_3d) %>%
#    pan_camera("right lateral")

## ----"incl2", echo=FALSE------------------------------------------------------
knitr::include_graphics("figures/vignette-yeo17-3d-plot.png")

