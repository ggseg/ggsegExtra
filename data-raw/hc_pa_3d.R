library(tidyverse)
library(ggseg3d)

folder = "data-raw/mesh3d/hippo_antpost/"

## aseg ----
hc_pa_3d = list.files(folder, pattern="ply", full.names = T) %>%
  data.frame(files = ., stringsAsFactors = F) %>%
  separate(files, c("DEL","DEL1","DEL2","DEL3", "DEL4", "DEL5", "roi"), remove = F) %>%
  select(-contains("DEL")) %>%
  mutate(surf="inflated", hemi="subcort", atlas = "hc_pa_3d")

rgb2hex <- function(r,g,b) rgb(r, g, b, maxColorValue = 255)

hc_pa_3d = hc_pa_3d %>%
  left_join(
    read.table(file.path(folder, "annot2filename.csv"), sep="\t",header=T,stringsAsFactors = F) %>%
      mutate(colour = rgb2hex(R,G,B),
             no = str_pad(no, 3, side = "left", pad="0")) %>%
      rename(roi=no) %>%
      select(roi, label, colour)
  )


mesh = lapply(hc_pa_3d$files, geomorph::read.ply, ShowSpecimen = F)

for(i in 1:length(mesh)){
  hc_pa_3d$mesh[[i]] = list(vb=mesh[[i]]$vb,
                           it=mesh[[i]]$it
  )
}

hc_pa_3d = hc_pa_3d %>%
  mutate(area=label,
         surf="LCBC") %>%
  group_by(atlas, surf, hemi) %>%
  as_ggseg3d_atlas()


ggseg3d(atlas=hc_pa_3d) %>% add_glassbrain()

usethis::use_data(hc_pa_3d, overwrite = TRUE, internal = FALSE, compress = "xz")
