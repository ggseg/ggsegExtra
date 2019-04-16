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
         atlas = "hoCort",
         area = gsub(" division", "", area),
         area = gsub("anterior", "ant.", area),
         area = gsub("posterior", "post.", area),
         area = gsub(" formerly Supplementary Motor Cortex ", "", area),
         area = gsub("Inferior", "Inf.", area),
         area = gsub("inferior", "inf.", area),
         area = gsub("Superior", "Sup.", area),
         area = gsub("Lateral", "Lat.", area),
         area = gsub("Middle", "Mid.", area),
         area = gsub(" part", "", area),
         ) 

# hoCort$pos[1] <- list(x = 1)
# for(i in 1:nrow(hoCort)){
#   hoCort$pos[[i]] = list(
#     stacked = list(
#       x = list(breaks = c(250, 900), 
#                labels = c("lateral", "medial")), 
#       y = list(breaks = c(200,  600), 
#                labels = c("left", "right")), labs = list(
#                  y = "side", x = "hemisphere")), 
#     dispersed = list(
#       x = list(
#         breaks = c(580, 1800), 
#         labels = c("left", "right")), 
#       y = list(breaks = NULL, labels = ""), 
#       labs = list(y = NULL, x = "hemisphere")))
# }

hoCort <- hoCort %>% 
  unnest(ggseg) %>% 
  select(-.pos)
hoCort <- as_ggseg_atlas(hoCort)
usethis::use_data(hoCort, internal = FALSE, overwrite = TRUE, compress = "xz")
