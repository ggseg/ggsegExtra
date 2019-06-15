tracula = jhu %>% 
  filter(!grepl("ifof", label)) %>% 
  mutate(area = ifelse(grepl("hippocampus", area), "Cingulum (angular bundle)", 
                       as.character(area)) )

usethis::use_data(tracula, internal = FALSE, overwrite = TRUE, compress="xz")
