source("data-raw/get_surface.R")

## Yeo 7 ----
yeo7_3d = get_surface("data-raw/mesh3d/Yeo20117NetworksN1000/", atlasname = "yeo7_3d")

t = data.frame(ggseg::brain.pals$yeo7)
names(t)[1] = "colour"
t = t %>%
  rownames_to_column(var = "area") %>%
  mutate_all(as.character)

t = yeo7 %>%
  left_join(t) %>%
  select(label, area, colour) %>%
  distinct() %>%
  na.omit()

yeo7_3d = yeo7_3d %>%
  mutate(data = map(data, ~left_join(., t, by="label"))) 
save(yeo7_3d, file="data/yeo7_3d.RData", compress = "xz")


## Yeo 17 ----
yeo17_3d = get_surface("data-raw/mesh3d/Yeo201117NetworksN1000/", atlasname = "yeo17_3d")

t = data.frame(ggseg::brain.pals$yeo17)
names(t)[1] = "colour"
t = t %>%
  rownames_to_column(var = "area") %>%
  mutate_all(as.character)

t = yeo17 %>%
  left_join(t) %>%
  select(label, area, colour) %>%
  distinct() %>%
  na.omit()

yeo17_3d = yeo17_3d %>%
  mutate(data = map(data, ~left_join(., t, by="label")))
#save(yeo17_3d, file="data/yeo17_3d.RData", compress = "xz")

## Schaefer 7 ----
schaefer7_3d = get_surface("data-raw/mesh3d/Schaefer2018400Parcels7Networksorder/",
                           atlasname = "schaefer7_3d") %>%
  unnest() %>%
  separate(annot, c("DEL", "DEL2", "network", "no"), remove = F) %>%
  unite(area, c("network", "no"), remove = F) %>%
  select(-contains("DEL"), -no) %>%
  mutate_all(funs(ifelse(grepl("Defined", .), "medialwall", .))) %>%
  group_by(atlas, surf, hemi) %>%
  nest()

lut = rio::import_list("data-raw/mesh3d/Schaefer2018_400Parcels_ctabs.xlsx") %>%
  bind_rows() %>%
  select(annot, HEX) %>%
  rename(colour = HEX) %>%
  filter(!grepl("Wall$", annot))

schaefer7_3d = schaefer7_3d %>%
  mutate(data = map(data, ~left_join(., lut) %>%
                      select(1:4, colour, everything()) %>%
                      mutate(annot = ifelse(grepl("Wall$",annot), "medialwall", annot))
  )
  )
#save(schaefer7_3d, file="data/schaefer7_3d.RData", compress = "xz")

## Schaefer 17 ----
schaefer17_3d = get_surface("data-raw/mesh3d/Schaefer2018400Parcels17Networksorder/",
                            atlasname = "schaefer17_3d") %>%
  unnest() %>%
  separate(annot, c("DEL", "DEL2", "network", "no"), remove = F) %>%
  unite(area, c("network", "no"), remove = F) %>%
  select(-contains("DEL"), -no) %>%
  mutate_all(funs(ifelse(grepl("Defined", .), "medialwall", .))) %>%
  group_by(atlas, surf, hemi) %>%
  nest()

schaefer17_3d = schaefer17_3d %>%
  mutate(data = map(data, ~left_join(., lut) %>%
                      select(1:4, colour, everything()) %>%
                      mutate(annot = ifelse(grepl("Wall$",annot), "medialwall", annot))
  )
  )
#save(schaefer17_3d, file="data/schaefer17_3d.RData", compress = "xz")

## Glasser ----
glasser_3d = get_surface("data-raw/mesh3d/HCPMMP1/", atlasname = "glasser_3d") %>%
  unnest() %>%
  separate(annot, c("DEL","area", "DEL2", "DEL3"), remove = F) %>%
  mutate(area = ifelse(!is.na(DEL3), paste(area, DEL2, sep="-"), area),
         label = gsub("^L_|^R_|_ROI$", "", label)) %>%
  select(-contains("DEL")) %>%
  group_by(atlas, surf, hemi) %>%
  nest()

t = data.frame(ggseg::brain.pals$glasser)
names(t)[1] = "colour"
t = t %>%
  rownames_to_column(var = "area") %>%
  mutate_all(as.character)

# t = glasser %>%
#   left_join(t) %>%
#   select(area, colour) %>%
#   distinct() %>%
#   na.omit()

glasser_3d = glasser_3d %>%
  mutate(data = map(data, ~left_join(., t, ) %>%
                      select(1:5, colour, everything()) %>%
                      mutate(area = ifelse(area == "", "medialwall", area))
  )
  )
#save(glasser_3d, file="data/glasser_3d.RData", compress = "xz")

## Desterieux ----
desterieux_3d = get_surface("data-raw/mesh3d/Desterieux/",
                            atlasname = "desterieux_3d")

lut = lapply(list.files("data-raw/mesh3d/Desterieux/", pattern="*csv", full.names = T), read_csv) %>%
  bind_rows() %>%
  mutate(colour = rgb(R,G,B, maxColorValue = 255)) %>%
  select(annot, colour) %>%
  filter(!grepl("annot",annot)) %>%
  filter(!grepl("all$",annot))

desterieux_3d = desterieux_3d %>%
  mutate(data = map(data, ~left_join(., lut) %>%
                      select(1:4, colour, everything()) %>%
                      mutate(annot = ifelse(grepl("all$",annot), "medialwall", annot),                             area = annot)
  )
  )

usethis::use_data(desterieux_3d, glasser_3d, schaefer17_3d, schaefer7_3d,
                  yeo17_3d, yeo7_3d,
                  internal = FALSE, overwrite = TRUE, compress = "xz")





## jhu ----
icbm_3d = get_surface("data-raw/mesh3d/JHU-ICBM-vis-2mm.nii.fsaverage5//", atlasname = "icbm_3d")

t = data.frame(ggseg::brain.pals$yeo7)
names(t)[1] = "colour"
t = t %>%
  rownames_to_column(var = "area") %>%
  mutate_all(as.character)

t = yeo7 %>%
  left_join(t) %>%
  select(label, area, colour) %>%
  distinct() %>%
  na.omit()

yeo7_3d = yeo7_3d %>%
  mutate(data = map(data, ~left_join(., t, by="label")))
save(yeo7_3d, file="data/yeo7_3d.RData", compress = "xz")
