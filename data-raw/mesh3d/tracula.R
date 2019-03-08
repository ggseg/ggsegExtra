library(tidyverse)
library(ggseg)

nn <- here::here()
folder <- "/data-raw/mesh3d/JHU_tracts_vis.fsaverage5/"

ff = read_csv2(paste0(nn, folder, "/annot2filename.csv")) %>% 
  rename(files=`filename (*.ply)`,
         area2 = JHU) %>% 
  select(2:3, 8:11, annot) %>% 
  na.omit() %>% 
  mutate(colour = ggtern::rgb2hex(R_1,G_1,B_1),
         area = gsub(" L$| R$", "", area2)) %>% 
  select(-R_1, -G_1, -B_1) %>%   
  mutate(hemi = "subcort",
         surf = "LCBC",
         atlas = "tracula_3d"
         
  ) %>% 
  mutate(colour = ifelse(area == lead(area), lead(colour), colour),
         colour = ifelse(is.na(colour), lag(colour), colour),
         label = annot,
         annot = gsub("rh_|lh_", "", annot)) %>% 
  select(-area2, -Tracula) %>% 
  separate(files, sep="_", into=c(NA, "roi"), remove = F) 


mesh = paste0(nn, folder, ff$files, ".ply") %>% 
  lapply(geomorph::read.ply, ShowSpecimen = F)

ff$mesh = list(vb=1)
for(i in 1:length(mesh)){
  ff$mesh[[i]] = list(vb=mesh[[i]]$vb,
                      it=mesh[[i]]$it
  )
}
# for(i in 1:nrow(ff)){
#   tt <- ff
#   ff$mesh[[i]]$vb["xpts",] = tt$mesh[[i]]$vb["ypts",]
#   ff$mesh[[i]]$vb["ypts",] = tt$mesh[[i]]$vb["zpts",]
#   ff$mesh[[i]]$vb["zpts",] = tt$mesh[[i]]$vb["xpts",]
# }

tracula_3d <- ff %>% 
  select(-files) %>% 
  group_by(atlas, surf, hemi) %>%
  nest() %>% 
  as_ggseg3d_atlas()

ggseg3d(atlas=tracula_3d, glassbrain = .6, glassbrain_hemisphere = "left")

usethis::use_data(tracula_3d, overwrite = TRUE, internal = FALSE, compress = "xz")
