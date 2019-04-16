library(tidyverse)

glasser = geobrain_hcp %>%
  separate(aparc, c("hemi","area", "DEL"), sep="_", remove = F) %>%
  select(-DEL, -piece, -meas) %>%
  mutate(hemi = ifelse(hemi == "L", "left", "right"),
         side="medial",
         lat = lat - min(lat),
         long = long - min(long)) %>%
  mutate(side = ifelse(id <= 150, "lateral", side)) %>%
  mutate(side = ifelse(id %in% c(2000:2500), "lateral", side),
         area = ifelse(grepl("\\?", area), NA, area)) %>%
  rename(label=aparc) %>%
  select(long, lat, id, hemi, area, side, label, everything())

glasser <- glasser %>%
  select(-group) %>%
  mutate(pos = NA)

# glasser <- unnest(glasser, ggseg)
# glasser$.pos[1] <- list(x = 1)
# for(i in 1:nrow(glasser)){
#   glasser$.pos[[i]] = list(
#     stacked = list(x = list(breaks = c(1.1, 4.5),
#                             labels = c("lateral","medial")),
#                    y = list(breaks = c(0.9, 3.4),
#                             labels = c("left","right")),
#                    labs = list(x = "side",
#                                y = "hemisphere")),
#     dispersed = list(x = list(breaks = c(3, 9.1),
#                               labels = c("left","right")),
#                      y = list(breaks = NULL,
#                               labels = NULL),
#                      labs = list(x = "hemisphere",
#                                  y = NULL))
#   )
# }

glasser <- glasser %>% 
  unnest(ggseg) %>% 
  select(-.pos)
glasser <- as_ggseg_atlas(glasser)
usethis::use_data(glasser, internal = FALSE, overwrite = TRUE, compress = "xz")
