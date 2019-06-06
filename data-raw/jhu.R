# jhu = rio::import("data-raw/geobrain_JHU.Rda") %>% 
# mutate(aparc = gsub("ifl", "ilf", aparc),
#        aparc = gsub("sflt", "slft", aparc)) %>%
#   separate(aparc, c("hemi","acronym"), remove = FALSE) %>%
#   mutate(acronym = ifelse(!hemi %in% c("lh","rh"), hemi, acronym),
#          acronym = ifelse(acronym %in% "gm", NA, acronym),
#          acronym = ifelse(id %in% 100:101, "cst", acronym),
#          aparc = ifelse(aparc %in% "gm", NA, aparc),
#          hemi = ifelse(hemi %in% "lh", "left",
#                        ifelse(hemi %in% "rh", "right", "center")),
#          aparc = gsub("\\.", "_", aparc),
#          side = ifelse(id %in% c(200:240), "upper axial",
#                        ifelse(id %in% c(1:13), "lower axial","coronal")),
#          atlas = "jhu"
#   ) %>%
#   mutate(acronym = ifelse(id %in% c(5,8,217),
#                           "slft", acronym)) %>% 
#   rename(label = aparc)

tmp = data.frame(area = c("Anterior thalamic radiation", "Corticospinal tract","Cingulum (cingulate gyrus)",
                          "Cingulum (hippocampus)","Forceps major","Forceps minor",
                          "Inferior fronto-occipital fasciculus","Inferior longitudinal fasciculus","Superior longitudinal fasciculus",
                          "Uncinate fasciculus","Superior longitudinal fasciculus (temporal part)","Cerebral spinal fluid"),
                 acronym=c("atr","cst","ccg","cab","fmajor","fminor","ifof","ilf","slf","unc","slft", "csf"))

jhu = jhu %>%
#  left_join(tmp) %>%
  #rename(label=aparc) %>%
  #select(-meas, -piece) %>% 
  select(lat, long, area, hemi, side, acronym, atlas, everything())

jhu <- jhu %>%
  # select(-group) %>%
  mutate(pos = list(x = 1))

for(i in 1:nrow(jhu)){
  jhu$pos[[i]] = list(
    stacked = list(),
    dispersed = list(x = list(breaks = c(7.5, 10.75, 14),
                              labels = c("upper coronal","axial", "lower coronal")),
                     y = list(breaks = NULL,
                              labels = NULL),
                     labs = list(x = "side",
                                 y = NULL))
  )
}

jhu <- jhu %>%
  select(one_of(names(ggseg::dkt))) %>% 
  ggseg:::as_ggseg_atlas() %>% 
  mutate(side = case_when(
    side == "upper coronal" ~ "upper axial",
    side == "lower coronal" ~ "lower axial",
    side == "axial" ~ "coronal"
  )) 

usethis::use_data(jhu, internal = FALSE, overwrite = TRUE, compress = "xz")

