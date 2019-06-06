library(tidyverse)
library(ggseg)

folder = "data-raw/mesh3d/JHU_ICBM.fsaverage5/"
mesh = lapply(list.files(folder, pattern="ply", full.names = T), 
              geomorph::read.ply, ShowSpecimen = F)

annots = read_csv2(paste0(folder, "annot2filename.csv")) %>% 
  rename(files=filename,
         area2 = JHU_label) %>% 
  select(-1, -3) %>% 
  mutate(colour = ggtern::rgb2hex(R,G,B),
         area = gsub(" L$| R$", "", area2),
         label = annot,
         annot = gsub("rh_|lh_", "", annot)) %>% 
  select(-R, -G, -B) %>%   
  mutate(hemi = "subcort",
         surf = "LCBC"
         
  ) %>% 
  separate(files, into=c(NA, NA, "roi"), sep="_") %>% 
  mutate(colour = ifelse(area == lead(area), lead(colour), colour),
         colour = ifelse(is.na(colour), lag(colour), colour)) %>% 
  select(-area2)


ff <- tibble(files = list.files(folder, pattern="ply", full.names = F),
             atlas = "icbm_3d") %>% 
  separate(files, sep="_", into=c(NA, NA, "roi"), remove = F) %>% 
  separate(roi, sep="[.]", into=c("roi", NA), remove = F) %>% 
  left_join(annots) 


ff$mesh = list(vb=1)
for(i in 1:length(mesh)){
  ff$mesh[[i]] = list(vb=mesh[[i]]$vb,
                      it=mesh[[i]]$it
  )
}

icbm_3d <- ff %>% 
  group_by(atlas, surf, hemi) %>%
  nest() %>% 
  as_ggseg3d_atlas()

ggseg3d(atlas=icbm_3d, glassbrain = .6, glassbrain_hemisphere = "left")

usethis::use_data(icbm_3d, overwrite = TRUE, internal = FALSE, compress = "xz")
