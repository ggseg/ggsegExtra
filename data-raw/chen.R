library(tidyverse)
library(ggseg)

load("data-raw/geobrain_Chencth.Rda")

chen <- geobrain_Chencth %>% 
  mutate(hemi = case_when(hemi == "lh" ~ "left",
                          hemi == "rh" ~ "right"))
