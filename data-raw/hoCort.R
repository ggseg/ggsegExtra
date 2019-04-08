load("data-raw/HarvardOxford/ho_atlases.Rda")
library(tidyverse)
library(ggseg)

hoCort <- ho.df.final %>% 
  mutate(hemi = case_when(hemi == "lh" ~ "left",
                          hemi == "rh" ~ "right"),
         side = case_when(side == "lat" ~ "lateral",
                          side == "med" ~ "medial"),
         area = ifelse(grepl("wall", area), NA, area),
         pos = NA,
         atlas = "hoCort") 

hoCort$pos[1] <- list(x = 1)
for(i in 1:nrow(hoCort)){
  hoCort$pos[[i]] = list(
    stacked = list(
      x = list(breaks = c(250, 900), 
               labels = c("left", "right")), 
      y = list(breaks = c(200,  600), 
               labels = c("lateral", "medial")), labs = list(
                 y = "side", x = "hemisphere")), 
    dispersed = list(
      x = list(
        breaks = c(519.272418189545, 1841.89700704225), 
        labels = c("left", "right")), 
      y = list(breaks = NULL, labels = ""), 
      labs = list(y = NULL, x = "hemisphere")))
}
hoCort <- as_ggseg_atlas(hoCort)
usethis::use_data(hoCort, internal = FALSE, overwrite = TRUE, compress = "xz")
