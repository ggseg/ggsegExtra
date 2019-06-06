library(tidyverse)
library(ggseg)

nn <- here::here()
folder <- "/data-raw/mesh3d/JHU_tracts_vis.fsaverage5/"

ff = read_csv2(paste0(nn, folder, "annot2filename.csv")) %>% 
  rename(files=`filename (*.ply)`,
         area2 = JHU) %>% 
  select(2:6, annot) %>% 
  mutate(colour = ggtern::rgb2hex(R,G,B),
         area = gsub(" L$| R$", "", area2)) %>% 
  select(-R, -G, -B) %>%   
  mutate(hemi = "subcort",
         surf = "LCBC",
         atlas = "jhu_3d"
         
  ) %>% 
  separate(files, into=c(NA, "roi"), sep="_") %>% 
  mutate(colour = ifelse(area == lead(area), lead(colour), colour),
         colour = ifelse(is.na(colour), lag(colour), colour),
         label = annot,
         annot = gsub("rh_|lh_", "", annot)) %>% 
  select(-area2)

mesh = lapply(list.files(paste0(nn, folder), pattern="ply", full.names = T), 
              geomorph::read.ply, ShowSpecimen = F)

ff$mesh = list(vb=1)
for(i in 1:length(mesh)){
  ff$mesh[[i]] = list(vb=mesh[[i]]$vb,
                        it=mesh[[i]]$it
  )
}

# for(i in 1:nrow(ff)){
#   tt <- ff
#   ff$mesh[[i]]$vb["ypts",] = tt$mesh[[i]]$vb["zpts",]-20
#   ff$mesh[[i]]$vb["zpts",] = tt$mesh[[i]]$vb["ypts",]+20
#  # ff$mesh[[i]]$vb["ypts",] = tt$mesh[[i]]$vb["xpts",]
# }

jhu_3d <- ff %>% 
  group_by(atlas, surf, hemi) %>%
  nest() %>% 
  as_ggseg3d_atlas()

ggseg3d(atlas=jhu_3d, glassbrain = .6, glassbrain_hemisphere = "left")

usethis::use_data(jhu_3d, overwrite = TRUE, internal = FALSE, compress = "xz")
