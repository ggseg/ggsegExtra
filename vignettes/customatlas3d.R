## ---- include = FALSE---------------------------------------------------------
knitr::opts_chunk$set(
  collapse = TRUE,
  eval = FALSE,
  comment = "#>"
)

## -----------------------------------------------------------------------------
#  # Desikan-Killany atlas (a.k.a aparc)
#  dt <- make_aparc_2_3datlas()
#  
#  # Desterieux atlas
#  dt <- make_aparc_2_3datlas(annot = "aparc.a2009s")
#  
#  # Yeo 7 networks atlas
#  dt <- make_aparc_2_3datlas(annot = "Yeo2011_7Networks_N1000")

## -----------------------------------------------------------------------------
#  # myResults atlas
#  dt <- make_aparc_2_3datlas(annot = "myResults",
#                         annot_dir = "~/Desktop")

## -----------------------------------------------------------------------------
#  # aseg atlas (a.k.a aseg)
#  dt <- make_aseg_2_3datlas()

## -----------------------------------------------------------------------------
#  
#  # myResults template without a color LUT
#  dt <- make_aparc_2_3datlas(template = "path/to/myResults.mgz",
#                         color_lut = NULL)
#  
#  # myResults template with a color LUT
#  dt <- make_aparc_2_3datlas(template = "path/to/myResults.mgz",
#                         color_lut = "path/to/myResults_LUT.txt")

