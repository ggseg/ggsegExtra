library(tidyverse)
library(ggseg)

load("data-raw/geobrain_Chencth.Rda")

chen <- geobrain_Chencth %>% 
  mutate(label = paste(hemi, gsub(" ", "_", aparc), sep = "_"),
         hemi = case_when(hemi == "lh" ~ "left",
                          hemi == "rh" ~ "right"),
         side = ifelse(id %in% c(1:17, 2000:2009), "lateral", "medial"),
         aparc = ifelse(grepl("wall", aparc), NA, aparc),
         atlas = "chen") %>% 
  rename(area = aparc) %>% 
  select(-group, -meas, -aparc2, -piece) %>% 
  mutate(
    long = long - min(long),
    lat = lat - min(lat)
  )


# swap medial and lateral of right hemisphere
minMed <- chen %>% filter(hemi=="right" & side == "medial") %>% select(long) %>% min
minLat <- chen %>% filter(hemi=="right" & side == "lateral") %>% select(long) %>% min
diff <- minLat - minMed
diff <- 4.25 # adjustment for nicer distances

chen <- chen %>% 
  mutate(long = ifelse(hemi=="right" & side == "lateral",
                       long + diff, long),
         long = ifelse(hemi=="right" & side == "medial",
                       long - diff, long))


chen$pos[1] <- list(x = 1)
for(i in 1:nrow(hoCort)){
  chen$pos[[i]] = list(
    stacked = list(
      x = list(breaks = c(1.7, 6.5), 
               labels = c("lateral", "medial")), 
      y = list(breaks = c(1,  4.5), 
               labels = c("left", "right")), labs = list(
                 y = "side", x = "hemisphere")), 
    dispersed = list(
      x = list(
        breaks = c(4, 13.5), 
        labels = c("left", "right")), 
      y = list(breaks = NULL, labels = ""), 
      labs = list(y = NULL, x = "hemisphere")))
}
chen <- as_ggseg_atlas(chen)
usethis::use_data(chen, internal = FALSE, overwrite = TRUE, compress = "xz")
