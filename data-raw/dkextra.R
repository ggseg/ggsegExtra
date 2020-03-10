library(tidyverse)
library(ggseg)

load(here::here("data-raw/dkextra.Rda"))
dkextra <- geobrain_dk_extra %>% 
  rename(side = View,
         hemi = Hemi,
         label = aparc) %>% 
  as_tibble() %>% 
  mutate(
    label = ifelse(grepl("wall", label), NA, label),
    atlas = "dkextra",
    id = as.character(id)
    ) %>% 
  select(-group, -meas, -piece) %>% 
  left_join(select(unnest(dk, ggseg), label, region, `.acronym`, `.lobe`)) %>% 
  group_by(side, hemi) %>% 
  mutate(
    long = long * 1.8,
    lat = lat * 1.8,
    long = long - min(long),
    lat = lat - min(lat)
  ) %>% 
  ungroup() %>% 
  mutate(
    long = case_when(
      hemi == "right" ~ long + 16.1,
      TRUE ~ long + 3.4
    ),
    lat = case_when(
      side == "inferior" ~ lat - 3.9,
      TRUE ~ lat - 1.3
    )
  ) %>% 
  as_ggseg_atlas()

# ggseg(atlas = dkextra)

dkextra <- bind_rows(dk, dkextra) %>% 
  mutate(atlas = "dkextra")

ggseg(atlas = dkextra, mapping = aes(fill = region)) +
  scale_fill_brain("dk")

usethis::use_data(dkextra, internal = FALSE, overwrite = TRUE, compress = "xz")


