library(tidyverse)
library(ggseg3d)

folder = "data-raw/mesh3d/hippo_antpost/"
mesh = lapply(list.files(folder, pattern="ply", full.names = T), 
              geomorph::read.ply, ShowSpecimen = F)

annots = read.table(paste0(folder, "annotation.txt"), header=FALSE, 
                    col.names = c("filename", "label", "R", "G", "B")) %>% 
  mutate(colour = c("#FF0000", "#00FF00", "#FF0000", "#00FF00"),
         area = gsub("-", " ", gsub("Left-|Right-", "", label)),
         hemi = "subcort",
         surf = "LCBC",
         roi = str_pad(1:4, 3, "left", "0")
  ) %>% 
  select(-R, -G, -B)


ff <- tibble(files = list.files(folder, pattern="ply", full.names = F),
             atlas = "hc_pa_3d") %>% 
  separate(files, sep="_", into=c(NA, NA,NA, "roi"), remove = F) %>% 
  separate(roi, sep="[.]", into=c("roi", NA), remove = F) %>% 
  left_join(annots) 


ff$mesh = list(vb=1)
for(i in 1:length(mesh)){
  ff$mesh[[i]] = list(vb=mesh[[i]]$vb,
                      it=mesh[[i]]$it
  )
}

hc_pa_3d <- ff %>% 
  group_by(atlas, surf, hemi) %>%
  as_ggseg3d_atlas()

ggseg3d(atlas=hc_pa_3d) %>% add_glassbrain()

usethis::use_data(hc_pa_3d, overwrite = TRUE, internal = FALSE, compress = "xz")
